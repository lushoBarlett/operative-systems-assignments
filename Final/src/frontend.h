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

/*
 * Constantes usadas por el protocolo
 * de comunicación del servidor de memcached
 */
enum Protocol {
	PUT = 11,
	DEL = 12,
	GET = 13,
	TAKE = 14,
	STATS = 21,
	OK = 101,
	EINVAL = 111,
	ENOTFOUND = 112,
	EBINARY = 113,
	EBIG = 114,
	EUNK = 115,
};

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

int require_bind(int listen_socket, int port) {
	struct sockaddr_in socket_address;

	memset(&socket_address, 0, sizeof socket_address);
	socket_address.sin_family = AF_INET;
	socket_address.sin_port = htons(port);
	socket_address.sin_addr.s_addr = htonl(INADDR_ANY);

	if (bind(listen_socket, (const struct sockaddr*)&socket_address, sizeof socket_address) < 0)
		quit("bind");
}

int require_listen(int listen_socket, int connections) { 
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

struct epoll_event poll_oneshot(int socket) {
	struct epoll_event event;
	event.data.fd = socket;
	event.events = EPOLLIN | EPOLLONESHOT;
	return event;
}

int poll_socket(int socket, int epfd) {
	struct epoll_event event = poll_oneshot(socket);
	return epoll_ctl(epfd, EPOLL_CTL_ADD, socket, &event);
}

int repoll_socket(int socket, int epfd) {
	struct epoll_event event = poll_oneshot(socket);
	return epoll_ctl(epfd, EPOLL_CTL_MOD, socket, &event);
}

void require_poll_socket(int socket, int epfd) {
	if (poll_socket(socket, epfd) < 0)
		quit("poll socket");
}

// TODO: according to the port, call text or binary protocols
void handle_event(int listen_socket, int epfd, struct epoll_event event) {
	if (event.data.fd == listen_socket) {
		// TODO: handle error (jump to repoll)
		int connection_socket = accept(listen_socket, NULL, NULL);

		// TODO: handle error (close connection)
		poll_socket(connection_socket, epfd);

		repoll_socket(listen_socket, epfd);
	} else {
		// TODO: process message (build read automaton)
	}
}

void service(int epfd, int listen_socket) {
	struct epoll_event event;

	while (1) {
		int n = epoll_wait(epfd, &event, 1, -1);

		// TODO: error handling
		if (n < 0)
			quit("epoll_wait");

		for (size_t i = 0; i < n; i++)
			handle_event(listen_socket, epfd, event);
	}
}

void* service_wrap(void* arg) {
	int epfd = *(int*)arg;
	int listen_socket = *(int*)(arg + 1);

	service(epfd, listen_socket);

	return NULL;
}

void spawn_service_threads(pthread_t* threads, size_t thread_amount, int epfd, int listen_socket) {
	int args[] = {epfd, listen_socket};
	for (size_t i = 0; i < thread_amount; i++)
		pthread_create(&threads[i], NULL, service_wrap, args);
}

void join_service_threads(pthread_t* threads, size_t thread_amount) {
	for (size_t i = 0; i < thread_amount; i++)
		pthread_join(threads[i], NULL);
}

void service_threads(pthread_t* threads, size_t thread_amount, int epfd, int listen_socket) {
	spawn_service_threads(threads, thread_amount, epfd, listen_socket);
	join_service_threads(threads, thread_amount);
}


int main() {
	int epfd = require_epoll();

	int listen_socket = configure_lsock(8000);

	require_poll_socket(listen_socket, epfd);

	size_t thread_amount = core_count();
	
	pthread_t* threads = malloc(sizeof(*threads) * thread_amount);

	service_threads(threads, thread_amount, epfd, listen_socket);

	free(threads);
}
