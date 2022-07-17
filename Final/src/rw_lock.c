#include "rw_lock.h"

void readers_writer_lock_init(readers_writer_lock_t* rw_lock) {
	rw_lock->readers = 0;
	pthread_mutex_init(&rw_lock->writer_lock, NULL);
	pthread_mutex_init(&rw_lock->reader_lock, NULL);
	pthread_cond_init(&rw_lock->no_readers, NULL);
}

static size_t current_readers(readers_writer_lock_t* rw_lock) {
	pthread_mutex_lock(&rw_lock->reader_lock);

	size_t readers = rw_lock->readers;

	pthread_mutex_unlock(&rw_lock->reader_lock);

	return readers;
}

void write_lock(readers_writer_lock_t* rw_lock) {
	pthread_mutex_lock(&rw_lock->writer_lock);

	while (current_readers(rw_lock) > 0)
		pthread_cond_wait(&rw_lock->no_readers, &rw_lock->writer_lock);
}

void write_unlock(readers_writer_lock_t* rw_lock) {
	pthread_mutex_unlock(&rw_lock->writer_lock);
}

void read_lock(readers_writer_lock_t* rw_lock) {
	pthread_mutex_lock(&rw_lock->writer_lock);
	pthread_mutex_lock(&rw_lock->reader_lock);

	rw_lock->readers++;

	pthread_mutex_unlock(&rw_lock->reader_lock);
	pthread_mutex_unlock(&rw_lock->writer_lock);
}

void read_unlock(readers_writer_lock_t* rw_lock) {
	pthread_mutex_lock(&rw_lock->reader_lock);

	if (--rw_lock->readers == 0)
		pthread_cond_signal(&rw_lock->no_readers);

	pthread_mutex_unlock(&rw_lock->reader_lock);
}
