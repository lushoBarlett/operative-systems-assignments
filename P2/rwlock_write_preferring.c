#include "rwlock_write_preferring.h"

#include <pthread.h>

void read_write_lock_init(read_write_lock_t* rw_lock) {
	pthread_mutex_init(&rw_lock->reader_count_lk, NULL);
	pthread_mutex_init(&rw_lock->wanna_write_lk, NULL);
	pthread_mutex_init(&rw_lock->writer, NULL);
}

void increment_writer_queue(read_write_lock_t* rw_lock) {
	pthread_mutex_lock(&rw_lock->wanna_write_lk);

	rw_lock->wanna_write++;

	pthread_mutex_unlock(&rw_lock->wanna_write_lk);
}

void decrement_writer_queue(read_write_lock_t* rw_lock) {
	pthread_mutex_lock(&rw_lock->wanna_write_lk);

	rw_lock->wanna_write--;

	if (rw_lock->wanna_write == 0)
		pthread_cond_broadcast(&rw_lock->no_writers);

	pthread_mutex_unlock(&rw_lock->wanna_write_lk);
}

void increment_reader_queue(read_write_lock_t* rw_lock) {
	pthread_mutex_lock(&rw_lock->reader_count_lk);

	rw_lock->reader_count++;

	pthread_mutex_unlock(&rw_lock->reader_count_lk);
}

void decrement_reader_queue(read_write_lock_t* rw_lock) {
	pthread_mutex_lock(&rw_lock->reader_count_lk);

	rw_lock->reader_count--;

	if (rw_lock->reader_count == 0)
		pthread_cond_broadcast(&rw_lock->last_reader_leaves);

	pthread_mutex_unlock(&rw_lock->reader_count_lk);
}

void wait_for_readers_to_leave(read_write_lock_t* rw_lock) {
	pthread_mutex_lock(&rw_lock->reader_count_lk);

	while (rw_lock->reader_count > 0)
		pthread_cond_wait(&rw_lock->last_reader_leaves, &rw_lock->reader_count_lk);

	pthread_mutex_unlock(&rw_lock->reader_count_lk);
}

void wait_for_no_writers_and_enter(read_write_lock_t* rw_lock) {
	pthread_mutex_lock(&rw_lock->wanna_write_lk);

	while (rw_lock->wanna_write > 0)
		pthread_cond_wait(&rw_lock->no_writers, &rw_lock->wanna_write_lk);

	increment_reader_queue(rw_lock);

	pthread_mutex_unlock(&rw_lock->wanna_write_lk);
}

void write_lock(read_write_lock_t* rw_lock) {
	increment_writer_queue(rw_lock);

	wait_for_readers_to_leave(rw_lock);

	pthread_mutex_lock(&rw_lock->writer);
}

void write_unlock(read_write_lock_t* rw_lock) {
	pthread_mutex_unlock(&rw_lock->writer);

	decrement_writer_queue(rw_lock);
}

void read_lock(read_write_lock_t* rw_lock) {
	wait_for_no_writers_and_enter(rw_lock);
}

void read_unlock(read_write_lock_t* rw_lock) {
	decrement_reader_queue(rw_lock);
}