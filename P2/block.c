#include "block.h"

void block_init(block_t* block, unsigned int counter) {
	block->counter = counter;
	pthread_mutex_init(&block->counter_lock, NULL);
	pthread_cond_init(&block->depleted, NULL);
}

void block_enter(block_t* block) {
	pthread_mutex_lock(&block->counter_lock);

	block->counter++;

	pthread_mutex_unlock(&block->counter_lock);

}

void block_leave(block_t* block) {
	pthread_mutex_lock(&block->counter_lock);

	block->counter--;

	if (block->counter == 0)
		pthread_cond_broadcast(&block->depleted);

	pthread_mutex_unlock(&block->counter_lock);
}

void block_wait(block_t* block, pthread_mutex_t* lock) {
	pthread_cond_wait(&block->depleted, lock);
}