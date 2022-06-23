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

int require_accept(int lsock) {
	int csock = accept(lsock, NULL, NULL);
	
	if (csock < 0)
		quit("accept");

	return csock;
}

int require_socket() {
	int lsock = socket(AF_INET, SOCK_STREAM, 0);

	if (lsock < 0)
		quit("socket");

	int yes = 1;

	if (setsockopt(lsock, SOL_SOCKET, SO_REUSEADDR, &yes, sizeof yes) == 1)
		quit("setsockopt");

	return lsock;
}

int require_bind(int lsock, int port) {
	struct sockaddr_in sa;

	memset(&sa, 0, sizeof sa);
	sa.sin_family = AF_INET;
	sa.sin_port = htons(port);
	sa.sin_addr.s_addr = htonl(INADDR_ANY);

	if (bind(lsock, (const struct sockaddr*)&sa, sizeof sa) < 0)
		quit("bind");
}

int require_listen(int lsock, int connections) { 
	if (listen(lsock, connections) < 0)
		quit("listen");
}

int configure_lsock(int port) {
	int lsock = require_socket();
	
	require_bind(lsock, port);
	
	require_listen(lsock, 10000);
	
	return lsock;
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
	struct epoll_event ev;
	ev.data.fd = socket;
	ev.events = EPOLLIN | EPOLLONESHOT;
	return ev;
}

int poll_socket(int socket, int epfd) {
	struct epoll_event ev = poll_oneshot(socket);
	return epoll_ctl(epfd, EPOLL_CTL_ADD, socket, &ev);
}

int repoll_socket(int socket, int epfd) {
	struct epoll_event ev = poll_oneshot(socket);
	return epoll_ctl(epfd, EPOLL_CTL_MOD, socket, &ev);
}

void require_poll_socket(int socket, int epfd) {
	if (poll_socket(socket, epfd) < 0)
		quit("poll socket");
}

// TODO: according to the port, call text or binary protocols
void handle_event(int lsock, int epfd, struct epoll_event ev) {
	int fd = ev.data.fd;
	if (fd == lsock) {
		// TODO: handle error (jump to repoll)
		int csock = accept(lsock, NULL, NULL);

		// TODO: handle error (close connection)
		poll_socket(csock, epfd);

		repoll_socket(lsock, ev);
	} else {
		// TODO: process message (build read automaton)
	}
}

void service(int epfd, int lsock) {
	struct epoll_event event;

	for (;;) {
		int n = epoll_wait(epfd, &event, 1, -1);

		// TODO: error handling
		if (n < 0)
			quit("epoll_wait");

		for (size_t i = 0; i < n; i++)
			handle_event(lsock, epfd, event);
	}
}

void *service_wrap(void *arg) {
	int epfd = *(int*)arg;
	int lsock = *(int*)(arg+1);
	service(epfd, lsock);

	return 0;
}

void spawn_service_threads(pthread_t* threads, size_t thread_amount, int epfd, int lsock) {
	int args[2] = {epfd, lsock};
	for (size_t i = 0; i < thread_amount; i++)
		pthread_create(&threads[i], NULL, service_wrap, args);
}

void join_service_threads(pthread_t* threads, size_t thread_amount) {
	for (size_t i = 0; i < thread_amount; i++)
		pthread_join(threads[i], NULL);
}


int main() {
	int epfd = require_epoll();

	int lsock = configure_lsock(8000);

	require_poll_socket(lsock, epfd);

	size_t thread_amount = core_count();
	
	pthread_t* threads = malloc(sizeof(*threads) * thread_amount);

	spawn_service_threads(threads, thread_amount, epfd, lsock);

	join_service_threads(threads, thread_amount);

	free(threads);
}
