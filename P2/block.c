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

void block_leave(block_t* block, int broadcast) {
	pthread_mutex_lock(&block->counter_lock);

	block->counter--;

	if (block->counter == 0) {
	 	if (broadcast)
			pthread_cond_broadcast(&block->depleted);
		else
			pthread_cond_signal(&block->depleted);
	}

	pthread_mutex_unlock(&block->counter_lock);
}

void block_leave_signal(block_t* block) {
	block_leave(block, 0);
}

void block_leave_broadcast(block_t* block) {
	block_leave(block, 1);
}

void block_wait(block_t* block, pthread_mutex_t* lock) {
	pthread_cond_wait(&block->depleted, lock);
}

unsigned int block_counter(block_t* block) {
	pthread_mutex_lock(&block->counter_lock);

	int counter = block->counter;

	pthread_mutex_unlock(&block->counter_lock);

	return counter;
}