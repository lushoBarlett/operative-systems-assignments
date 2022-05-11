#include <pthread.h>

struct read_write_lock_t {
	pthread_mutex_t reader_count_lk;
	pthread_mutex_t wanna_write_lk;
	pthread_mutex_t writer;

	int wanna_write;
	int reader_count;

	pthread_cond_t last_reader_leaves;
	pthread_cond_t no_writers;
};

typedef struct read_write_lock_t read_write_lock_t;

void read_write_lock_init(read_write_lock_t* rw_lock);

void write_lock(read_write_lock_t* rw_lock);

void write_unlock(read_write_lock_t* rw_lock);

void read_lock(read_write_lock_t* rw_lock);

void read_unlock(read_write_lock_t* rw_lock);