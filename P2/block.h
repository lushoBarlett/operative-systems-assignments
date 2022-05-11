#pragma once

#include <pthread.h>

struct block_t {

	unsigned int counter;

	pthread_mutex_t counter_lock;

	pthread_cond_t depleted;
};

typedef struct block_t block_t;

void block_init(block_t* block, unsigned int counter);

void block_enter(block_t* block);

void block_leave_signal(block_t* block);

void block_leave_broadcast(block_t* block);

void block_wait(block_t* block, pthread_mutex_t* lock);

unsigned int block_counter(block_t* block);