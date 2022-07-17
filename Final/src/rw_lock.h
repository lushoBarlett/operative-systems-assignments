#pragma once

#include <pthread.h>

typedef struct {
	pthread_mutex_t writer_lock;

	size_t readers;

	pthread_mutex_t reader_lock;

	pthread_cond_t no_readers;

} readers_writer_lock_t;

void readers_writer_lock_init(readers_writer_lock_t* rw_lock);

void write_lock(readers_writer_lock_t* rw_lock);

void write_unlock(readers_writer_lock_t* rw_lock);

void read_lock(readers_writer_lock_t* rw_lock);

void read_unlock(readers_writer_lock_t* rw_lock);