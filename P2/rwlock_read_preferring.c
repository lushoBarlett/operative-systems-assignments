#include "rwlock_read_preferring.h"

#include <pthread.h>

void read_write_init(read_write_lock_t* rw_lock) {
	pthread_mutex_init(&rw_lock->reader, NULL);
	pthread_mutex_init(&rw_lock->reader, NULL);
	rw_lock->reader_count = 0;
}

void write_lock(read_write_lock_t* rw_lock) {
	pthread_mutex_lock(&rw_lock->writer);
}

void write_unlock(read_write_lock_t* rw_lock) {
	pthread_mutex_unlock(&rw_lock->writer);
}

void read_lock(read_write_lock_t* rw_lock) {
	pthread_mutex_lock(&rw_lock->reader);
	
	if (rw_lock->reader_count == 0)
		pthread_mutex_lock(&rw_lock->writer);

	rw_lock->reader_count++;

	pthread_mutex_unlock(&rw_lock->reader);
}

void read_unlock(read_write_lock_t* rw_lock) {
	pthread_mutex_lock(&rw_lock->reader);

	rw_lock->reader_count--;
	
	if (rw_lock->reader_count == 0)
		pthread_mutex_unlock(&rw_lock->writer);
	
	pthread_mutex_unlock(&rw_lock->reader);
}