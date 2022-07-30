#pragma once

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
#include "epoll_utils.h"

typedef struct thread_args {
	fdinfo_list_t** head;
	pthread_mutex_t lock;
	int epfd;
	database_t* database;
} thread_args_t;

void server_run(database_t* database);