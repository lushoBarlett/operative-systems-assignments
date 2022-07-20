#include "rw_lock.h"

#include <stdio.h>

void readers_writer_lock_init(rw_lock_t* rw_lock) {
	pthread_mutex_init(&rw_lock->writer_lock, NULL);

	rw_lock->readers = 0;
	pthread_mutex_init(&rw_lock->readers_lock, NULL);

	rw_lock->writers = 0;
	pthread_mutex_init(&rw_lock->writers_lock, NULL);

	pthread_cond_init(&rw_lock->no_readers, NULL);
	pthread_cond_init(&rw_lock->no_writers, NULL);
}

static void increment_readers(rw_lock_t* rw_lock) {
	pthread_mutex_lock(&rw_lock->readers_lock);

	rw_lock->readers++;

	pthread_mutex_unlock(&rw_lock->readers_lock);
}

static void increment_writers(rw_lock_t* rw_lock) {
	pthread_mutex_lock(&rw_lock->writers_lock);

	rw_lock->writers++;

	pthread_mutex_unlock(&rw_lock->writers_lock);
}

static void decrement_readers(rw_lock_t* rw_lock) {
	pthread_mutex_lock(&rw_lock->readers_lock);

	if (--rw_lock->readers == 0)
		pthread_cond_signal(&rw_lock->no_readers);

	pthread_mutex_unlock(&rw_lock->readers_lock);
}

static void decrement_writers(rw_lock_t* rw_lock) {
	pthread_mutex_lock(&rw_lock->writers_lock);

	if (--rw_lock->writers == 0)
		pthread_cond_broadcast(&rw_lock->no_writers);

	pthread_mutex_unlock(&rw_lock->writers_lock);
}

void write_lock(rw_lock_t* rw_lock) {
	increment_writers(rw_lock);

	pthread_mutex_lock(&rw_lock->readers_lock);

	while (rw_lock->readers > 0)
		pthread_cond_wait(&rw_lock->no_readers, &rw_lock->readers_lock);

	pthread_mutex_lock(&rw_lock->writer_lock);

	pthread_mutex_unlock(&rw_lock->readers_lock);
}

void write_unlock(rw_lock_t* rw_lock) {
	decrement_writers(rw_lock);

	pthread_mutex_unlock(&rw_lock->writer_lock);
}

void read_lock(rw_lock_t* rw_lock) {
	pthread_mutex_lock(&rw_lock->writers_lock);

	while (rw_lock->writers > 0)
		pthread_cond_wait(&rw_lock->no_writers, &rw_lock->writers_lock);

	increment_readers(rw_lock);

	pthread_mutex_lock(&rw_lock->writer_lock);

	pthread_mutex_unlock(&rw_lock->writers_lock);

	pthread_mutex_unlock(&rw_lock->writer_lock);
}

void read_unlock(rw_lock_t* rw_lock) {
	decrement_readers(rw_lock);
}
