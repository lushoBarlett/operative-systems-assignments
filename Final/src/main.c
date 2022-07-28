#define _GNU_SOURCE
#include <features.h>

#include <netinet/in.h>
#include <netinet/ip.h>
#include <pthread.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <sys/signalfd.h>
#include <unistd.h>
#include <signal.h>
#include <sys/epoll.h>

#include "text_parser.h"
#include "binary_parser.h"
#include "database.h"

#define MAX_MSG_SIZE 2048

/*
 * Constantes usadas por el protocolo
 * de comunicación del servidor de memcached
 */

typedef enum {
	Listen, Client, Signal,
} Sock_type;

typedef enum {
	Binary, Text,
} Cli_type;

typedef struct fdinfo {
	Cli_type cli_type;
	Sock_type sock_type;
	int fd;
	bin_state_machine_t bin_state_machine;
	txt_state_machine_t txt_state_machine;
	struct fdinfo* next;
	struct fdinfo* prev;
} fdinfo_list_t;

typedef struct thread_args {
	fdinfo_list_t** head;
	pthread_mutex_t lock;
	int epfd;
} thread_args_t;

database_t database;

void quit(const char* error_message) {
	perror(error_message);

	abort();
}

int require_signal_fd() {
	sigset_t mask;
	int sfd;

	sigemptyset(&mask);
	sigaddset(&mask, SIGINT);

	sigprocmask(SIG_BLOCK, &mask, NULL);

	sfd = signalfd(-1, &mask, 0);
	return sfd;
}

int require_accept(int listen_socket) {
	int connection_socket = accept(listen_socket, NULL, NULL);
	
	if (connection_socket < 0)
		quit("accept");

	return connection_socket;
}

int require_socket() {
	int listen_socket = socket(AF_INET, SOCK_STREAM, 0);

	if (listen_socket < 0)
		quit("socket");

	int yes = 1;

	if (setsockopt(listen_socket, SOL_SOCKET, SO_REUSEADDR, &yes, sizeof yes) == 1)
		quit("setsockopt");

	return listen_socket;
}

void require_bind(int listen_socket, int port) {
	struct sockaddr_in socket_address;

	memset(&socket_address, 0, sizeof socket_address);
	socket_address.sin_family = AF_INET;
	socket_address.sin_port = htons(port);
	socket_address.sin_addr.s_addr = htonl(INADDR_ANY);

	if (bind(listen_socket, (const struct sockaddr*)&socket_address, sizeof socket_address) < 0)
		quit("bind");
}

void require_listen(int listen_socket, int connections) { 
	if (listen(listen_socket, connections) < 0)
		quit("listen");
}

int configure_lsock(int port) {
	int listen_socket = require_socket();
	
	require_bind(listen_socket, port);
	
	require_listen(listen_socket, 10000);
	
	return listen_socket;
}

size_t core_count() {
	return sysconf(_SC_NPROCESSORS_ONLN);
}

int require_epoll() {
	int epfd = epoll_create(1);

	if (epfd < 0)
		quit("epoll");

	return epfd;
}

void poll_set_event(struct epoll_event* ev, Sock_type type) {
	if (type == Client)
		ev->events = EPOLLIN | EPOLLONESHOT | EPOLLHUP | EPOLLRDHUP;

	if (type == Signal)
		ev->events = EPOLLIN;

	if (type == Listen)
		ev->events = EPOLLIN | EPOLLONESHOT;
}

void client_state_machine_init(fdinfo_list_t* fdinfo, Cli_type cli_type) {
	if (cli_type == Binary)
		bin_state_machine_init(&fdinfo->bin_state_machine, fdinfo->fd, &database);

	if (cli_type == Text)
		txt_state_machine_init(&fdinfo->txt_state_machine, fdinfo->fd, &database);
}

fdinfo_list_t* poll_socket(int socket, int epfd, Sock_type sock_type, Cli_type cli_type) {

	struct epoll_event ev;
	fdinfo_list_t* fdinfo = malloc(sizeof (fdinfo_list_t));
	fdinfo->sock_type = sock_type;
	fdinfo->fd = socket;

	if (sock_type != Signal) {
		
		fdinfo->cli_type = cli_type;

		if (sock_type == Client)
			client_state_machine_init(fdinfo, cli_type);	
	}

	ev.data.ptr = fdinfo;

	poll_set_event(&ev, sock_type);

	int res = epoll_ctl(epfd, EPOLL_CTL_ADD, socket, &ev);

	if (res < 0) {
		free(fdinfo);
		return NULL;
	} else
		return fdinfo;
}

int repoll_socket(fdinfo_list_t* fdinfo, int epfd, Sock_type type) {
	struct epoll_event event;
	event.data.ptr = fdinfo;
	poll_set_event(&event, type);
	return epoll_ctl(epfd, EPOLL_CTL_MOD, fdinfo->fd, &event);
}

fdinfo_list_t* require_poll_socket(int socket, int epfd, Sock_type sock_type, Cli_type type) {
	fdinfo_list_t* fdinfo = poll_socket(socket, epfd, sock_type, type);

	return fdinfo;
}

void kill_client(int epfd, fdinfo_list_t* fdinfo){
	epoll_ctl(epfd, EPOLL_CTL_DEL, fdinfo->fd, NULL);

	close(fdinfo->fd);

	free(fdinfo);
}

void fdinfo_list_remove(fdinfo_list_t* node, fdinfo_list_t** head) {
	if (node->next)
		node->next->prev = node->prev;

	if (node->prev)
		node->prev->next = node->next;
	else
		*head = node->next;
}

void fdinfo_list_add(fdinfo_list_t* new_fdinfo, fdinfo_list_t** head) {
	new_fdinfo->next = *head;
	new_fdinfo->prev = NULL;

	if (*head)
		(*head)->prev = new_fdinfo;
	
	*head = new_fdinfo;
}

void fdinfo_list_destroy_remaining(int epfd, fdinfo_list_t** head_ptr) {
	fdinfo_list_t* head = *head_ptr;
	fdinfo_list_t* tmp;

	while (head) {
		tmp = head;
		head = head->next;
		kill_client(epfd, tmp);
	}

	free(head_ptr);
}

void handle_client(int epfd, struct epoll_event event, fdinfo_list_t* fdinfo, fdinfo_list_t** head, pthread_mutex_t lock) {
	int ret_value = 0;

	if (event.events & EPOLLIN) {

		if (fdinfo->cli_type == Text)
			ret_value = can_read_txt(&fdinfo->txt_state_machine);

		if (fdinfo->cli_type == Binary)
			ret_value = bin_state_machine_advance(&fdinfo->bin_state_machine);
	}

	if (ret_value == 0) {
		pthread_mutex_lock(&lock);
		
		fdinfo_list_remove(fdinfo, head);
		
		kill_client(epfd, fdinfo);
		
		pthread_mutex_unlock(&lock);
	} else
		repoll_socket(fdinfo, epfd, Client);
}

void handle_listener(int epfd, fdinfo_list_t* fdinfo, fdinfo_list_t** head, pthread_mutex_t lock) {
	int listen_socket = fdinfo->fd;

	int connection_socket = accept4(listen_socket, NULL, NULL, SOCK_NONBLOCK);

	fdinfo_list_t* new_fdinfo = poll_socket(connection_socket, epfd, Client, fdinfo->cli_type);

	pthread_mutex_lock(&lock);

	fdinfo_list_add(new_fdinfo, head);
	
	pthread_mutex_unlock(&lock);

	repoll_socket(fdinfo, epfd, Listen);
}

int handle_event(int epfd, struct epoll_event event, fdinfo_list_t** head, pthread_mutex_t lock) {
	fdinfo_list_t* fdinfo = event.data.ptr;
	int ret_value = 1;

	switch (fdinfo->sock_type) {
	case Listen:
		handle_listener(epfd, fdinfo, head, lock);		
		break;

	case Client:
		handle_client(epfd, event, fdinfo, head, lock);
		break;

	case Signal:
		ret_value = -1;
		break;
	}

	return ret_value;
}

void service(int epfd, pthread_mutex_t lock, fdinfo_list_t** head_ptr) {
	int stop = 0;	

	struct epoll_event event;	

	while (!stop) {
		int n = epoll_wait(epfd, &event, 1, -1);

		// TODO: error handling
		if (n < 0 && errno != EINTR)
			stop = 1;

		for (size_t i = 0; i < (size_t)n && !stop; i++)
			if (handle_event(epfd, event, head_ptr, lock) == -1)
				stop = 1;
	}
}

void* service_wrap(void* arg) {
	thread_args_t* thread_args = (thread_args_t*) arg;

	int epfd = thread_args->epfd;

	pthread_mutex_t lock = thread_args->lock;
	
	fdinfo_list_t** head = thread_args->head;

	service(epfd, lock, head);

	return NULL;
}

void spawn_service_threads(pthread_t* threads, size_t thread_amount, thread_args_t* thread_args) {
	for (size_t i = 0; i < thread_amount; i++) {
		pthread_create(&threads[i], NULL, service_wrap, (void*) thread_args);
	}
}

void join_service_threads(pthread_t* threads, size_t thread_amount) {
	for (size_t i = 0; i < thread_amount; i++)
		pthread_join(threads[i], NULL);
}

thread_args_t* thread_args_create(int epfd) {
	thread_args_t* thread_args = malloc(sizeof(thread_args_t));

	pthread_mutex_init(&thread_args->lock, NULL);
	
	thread_args->epfd = epfd;

	fdinfo_list_t** head_ptr = malloc(sizeof(fdinfo_list_t*));
	*head_ptr = NULL;
	thread_args->head = head_ptr;

	return thread_args;
}

void service_threads(pthread_t* threads, size_t thread_amount, int epfd) {
	thread_args_t* thread_args = thread_args_create(epfd);

	spawn_service_threads(threads, thread_amount, thread_args);
	join_service_threads(threads, thread_amount);

	fdinfo_list_destroy_remaining(epfd, thread_args->head);

	pthread_mutex_destroy(&thread_args->lock);
	
	free(thread_args);
}

int main() {
	// TODO: setrlimit
	database_init(&database);

	int epfd = require_epoll();

	// TODO: configure the correct ports
	int listen_txt_sock = configure_lsock(8000);
	int listen_bin_sock = configure_lsock(8001);

	fdinfo_list_t* fdinfo1 = require_poll_socket(listen_txt_sock, epfd, Listen, Text);
	fdinfo_list_t* fdinfo2 = require_poll_socket(listen_bin_sock, epfd, Listen, Binary);

	int signalfd = require_signal_fd();
	fdinfo_list_t* fdinfo3 = require_poll_socket(signalfd, epfd, Signal, Text);

	size_t thread_amount = core_count();
	
	pthread_t* threads = malloc(sizeof(*threads) * thread_amount);

	service_threads(threads, thread_amount, epfd);

	free(threads);
	free(fdinfo1);
	free(fdinfo2);
	free(fdinfo3);

	database_destroy(&database);
}