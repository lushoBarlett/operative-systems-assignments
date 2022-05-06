#include "concurrent_queue.h"

void concurrent_queue_init(concurrent_queue_t* cqueue) {
	queue_init(&cqueue->queue);

	sem_init(&cqueue->size, 0, 0);

	pthread_mutex_init(&cqueue->queue_lock, NULL);
}

void cenqueue(concurrent_queue_t* cqueue, void* value) {
	pthread_mutex_lock(&cqueue->queue_lock);

	enqueue(&cqueue->queue, value);

	pthread_mutex_unlock(&cqueue->queue_lock);

	sem_post(&cqueue->size);
}

void* cdequeue(concurrent_queue_t* cqueue) {
	sem_wait(&cqueue->size);

	pthread_mutex_lock(&cqueue->queue_lock);

	void* value = dequeue(&cqueue->queue);

	pthread_mutex_unlock(&cqueue->queue_lock);

	return value;
}