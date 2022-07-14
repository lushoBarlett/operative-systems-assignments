#pragma once
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

#define MAX_MSG_SIZE 2048

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
	state_machine_t state_machine;
	buf_t* buf;
} fdinfo ;

void quit(const char* error_message);

int require_accept(int listen_socket);

int require_socket();

int require_bind(int listen_socket, int port);

int require_listen(int listen_socket, int connections);

int configure_lsock(int port);

size_t core_count();

int require_epoll();

void poll_set_event(struct epoll_event* ev, Sock_type type);

int poll_socket(int socket, int epfd, Sock_type sock_type, Cli_type cli_type);

int repoll_socket(struct fdinfo*, int epfd, Sock_type type);

void require_poll_socket(int socket, int epfd, Cli_type type);

void handle_event(int epfd, struct epoll_event event);

void service(int epfd);

void* service_wrap(void* arg);

void spawn_service_threads(pthread_t* threads, size_t thread_amount, int epfd);

void join_service_threads(pthread_t* threads, size_t thread_amount);

void service_threads(pthread_t* threads, size_t thread_amount, int epfd);
