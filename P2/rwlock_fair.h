#pragma once

#include "queue.h"

#include <pthread.h>

struct read_write_lock_t {

	int readers;
	int writer;

	queue_t queue;

	pthread_mutex_t lock;
};

typedef struct read_write_lock_t read_write_lock_t;

void read_write_lock_init(read_write_lock_t* rw_lock);

void write_lock(read_write_lock_t* rw_lock);

void write_unlock(read_write_lock_t* rw_lock);

void read_lock(read_write_lock_t* rw_lock);

void read_unlock(read_write_lock_t* rw_lock);
