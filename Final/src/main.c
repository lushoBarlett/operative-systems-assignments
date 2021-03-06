#define _GNU_SOURCE
#include <features.h>

#include <netinet/in.h>
#include <netinet/ip.h>
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <unistd.h>
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
	LSOCK, CLIENT,
} Sock_type;

typedef enum {
	BINARY, TEXT,
} Cli_type;

typedef struct fdinfo {
	Cli_type cli_type;
	Sock_type sock_type;
	int fd;
	bin_state_machine_t bin_state_machine;
	txt_state_machine_t txt_state_machine;
} fdinfo ;

database_t database;

void quit(const char* error_message) {
	perror(error_message);

	abort();
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
	if (type == LSOCK)
		ev->events = EPOLLIN | EPOLLONESHOT;
	else
		ev->events = EPOLLIN | EPOLLONESHOT | EPOLLHUP | EPOLLRDHUP;
}

int poll_socket(int socket, int epfd, Sock_type sock_type, Cli_type cli_type) {

	struct fdinfo* fdinfo = malloc(sizeof (struct fdinfo));
	fdinfo->sock_type = sock_type;
	fdinfo->cli_type = cli_type;
	fdinfo->fd = socket;

	if (sock_type == CLIENT) {
		if (cli_type == BINARY)
			bin_state_machine_init(&fdinfo->bin_state_machine, fdinfo->fd, &database);
		if (cli_type == TEXT)
			txt_state_machine_init(&fdinfo->txt_state_machine, fdinfo->fd, &database);
	}

	struct epoll_event ev;

	ev.data.ptr = fdinfo;
	poll_set_event(&ev, sock_type);

	return epoll_ctl(epfd, EPOLL_CTL_ADD, socket, &ev);
}

int repoll_socket(struct fdinfo* fdinfo, int epfd, Sock_type type) {
	struct epoll_event event;
	event.data.ptr = fdinfo;
	poll_set_event(&event, type);
	return epoll_ctl(epfd, EPOLL_CTL_MOD, fdinfo->fd, &event);
}

void require_poll_socket(int socket, int epfd, Cli_type type) {
	if (poll_socket(socket, epfd, LSOCK, type) < 0)
		quit("poll socket");
}

void kill_cli(int epfd, struct fdinfo* fdinfo){
	epoll_ctl(epfd, EPOLL_CTL_DEL, fdinfo->fd, NULL);
	close(fdinfo->fd);
	free(fdinfo);
}

// TODO: according to the port, call text or binary protocols
void handle_event(int epfd, struct epoll_event event) {
	struct fdinfo *fdinfo = event.data.ptr;
	int listen_socket, connection_socket;

	switch (fdinfo->sock_type) {
	case LSOCK:
		listen_socket = fdinfo->fd;
		connection_socket = accept4(listen_socket, NULL, NULL, SOCK_NONBLOCK);
		poll_socket(connection_socket, epfd, CLIENT, fdinfo->cli_type);
		repoll_socket(fdinfo, epfd, LSOCK);
		break;

	case CLIENT:
		if (event.events & EPOLLHUP) {
			kill_cli(epfd, fdinfo);
			printf("hup\n");
			return;
		}

		if (event.events & EPOLLRDHUP) {
			kill_cli(epfd, fdinfo);
			printf("rdhup\n");
			return;
		}

		if (event.events & EPOLLIN) {
			if (fdinfo->cli_type == TEXT)
				can_read_txt(&fdinfo->txt_state_machine);

			if (fdinfo->cli_type == BINARY)
				bin_state_machine_advance(&fdinfo->bin_state_machine);
		}

		repoll_socket(fdinfo, epfd, CLIENT);
		break;
	}
}

void service(int epfd) {
	struct epoll_event event;

	while (1) {
		int n = epoll_wait(epfd, &event, 1, -1);

		// TODO: error handling
		if (n < 0)
			quit("epoll_wait");

		for (size_t i = 0; i < (size_t)n; i++)
			handle_event(epfd, event);
	}
}

void* service_wrap(void* arg) {
	int epfd = *((int*)arg);

	service(epfd);

	return NULL;
}

void spawn_service_threads(pthread_t* threads, size_t thread_amount, int epfd) {
	int args[] = {epfd};
	for (size_t i = 0; i < thread_amount; i++) {
		pthread_create(&threads[i], NULL, service_wrap, args);
	}
}

void join_service_threads(pthread_t* threads, size_t thread_amount) {
	for (size_t i = 0; i < thread_amount; i++)
		pthread_join(threads[i], NULL);
}

void service_threads(pthread_t* threads, size_t thread_amount, int epfd) {
	spawn_service_threads(threads, thread_amount, epfd);
	join_service_threads(threads, thread_amount);
}

int main() {
	database_init(&database);

	int epfd = require_epoll();

	// TODO: configure the correct ports
	int listen_txt_sock = configure_lsock(8000);
	int listen_bin_sock = configure_lsock(8001);

	require_poll_socket(listen_txt_sock, epfd, TEXT);
	require_poll_socket(listen_bin_sock, epfd, BINARY);

	size_t thread_amount = core_count();
	
	pthread_t* threads = malloc(sizeof(*threads) * thread_amount);

	service_threads(threads, thread_amount, epfd);

	free(threads);

	database_destroy(&database);
}
