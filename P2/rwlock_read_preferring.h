#include "block.h"

#include <pthread.h>

struct read_write_lock_t {

	block_t readers;

	pthread_mutex_t writer;
};

typedef struct read_write_lock_t read_write_lock_t;

void read_write_init(read_write_lock_t* rw_lock);

void write_lock(read_write_lock_t* rw_lock);

void write_unlock(read_write_lock_t* rw_lock);

void read_lock(read_write_lock_t* rw_lock);

void read_unlock(read_write_lock_t* rw_lock);