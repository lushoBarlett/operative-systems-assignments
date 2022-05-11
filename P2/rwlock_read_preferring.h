#include <pthread.h>

struct read_write_lock_t {

	/*
	 * Locks the whole channel_write function
	 * to prevent further writers from entering
	 * before the current one leaves
	 */
	pthread_mutex_t reader;

	pthread_mutex_t writer;

	int reader_count;
};

typedef struct read_write_lock_t read_write_lock_t;

void read_write_init(read_write_lock_t* rw_lock);

void write_lock(read_write_lock_t* rw_lock);

void write_unlock(read_write_lock_t* rw_lock);

void read_lock(read_write_lock_t* rw_lock);

void read_unlock(read_write_lock_t* rw_lock);