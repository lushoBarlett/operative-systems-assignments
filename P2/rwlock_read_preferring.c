#include "rwlock_read_preferring.h"

void read_write_lock_init(read_write_lock_t* rw_lock) {
	block_init(&rw_lock->readers, 0);
	pthread_mutex_init(&rw_lock->writer, NULL);
}

void write_lock(read_write_lock_t* rw_lock) {
	pthread_mutex_lock(&rw_lock->writer);

	while (block_counter(&rw_lock->readers) > 0)
		block_wait(&rw_lock->readers, &rw_lock->writer);
}

void write_unlock(read_write_lock_t* rw_lock) {
	pthread_mutex_unlock(&rw_lock->writer);
}

void read_lock(read_write_lock_t* rw_lock) {
	pthread_mutex_lock(&rw_lock->writer);

	block_enter(&rw_lock->readers);

	pthread_mutex_unlock(&rw_lock->writer);
}

void read_unlock(read_write_lock_t* rw_lock) {
	block_leave_signal(&rw_lock->readers);
}