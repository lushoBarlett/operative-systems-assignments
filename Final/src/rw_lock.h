#pragma once

#include <pthread.h>
#include <semaphore.h>

typedef struct {
	pthread_mutex_t writer_lock;

	size_t readers;
	pthread_mutex_t readers_lock;

	size_t writers;
	pthread_mutex_t writers_lock;

	pthread_cond_t no_readers;
	pthread_cond_t no_writers;

} rw_lock_t;

void readers_writer_lock_init(rw_lock_t* rw_lock);

void write_lock(rw_lock_t* rw_lock);

void write_unlock(rw_lock_t* rw_lock);

void read_lock(rw_lock_t* rw_lock);

void read_unlock(rw_lock_t* rw_lock);
