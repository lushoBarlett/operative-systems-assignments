#pragma once

#define _GNU_SOURCE
#include <sys/socket.h>
#include <pthread.h>
#include <sys/epoll.h>
#include <stdlib.h>
#include <unistd.h>

#include "binary_parser.h"
#include "text_parser.h"
#include "database.h"

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

fdinfo_list_t* poll_socket(int socket, int epfd, Sock_type sock_type, Cli_type cli_type, database_t* database);

void fdinfo_list_destroy_remaining(int epfd, fdinfo_list_t** head_ptr);

int handle_event(int epfd, struct epoll_event event, fdinfo_list_t** head, pthread_mutex_t* lock, database_t* database);