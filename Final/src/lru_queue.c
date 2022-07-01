#include "lru_queue.h"

void lru_queue_init(lru_queue_t* lru_queue) {
	lru_queue->back = lru_queue->front = NULL;
	pthread_mutex_init(&lru_queue->lock, NULL);
}

static void enqueue_operation(lru_queue_t* lru_queue, bucket_t* bucket) {
	if (lru_queue->back)
		lru_queue->back->next_queue = bucket;

	bucket->prev_queue = lru_queue->back;

	lru_queue->back = bucket;
}

void lru_queue_enqueue(lru_queue_t* lru_queue, bucket_t* bucket) {
	pthread_mutex_lock(&lru_queue->lock);

	enqueue_operation(lru_queue, bucket);

	pthread_mutex_unlock(&lru_queue->lock);
}

bucket_t* lru_queue_dequeue(lru_queue_t* lru_queue) {
	pthread_mutex_lock(&lru_queue->lock);

	bucket_t* bucket = lru_queue->front;

	if (lru_queue->front)
		lru_queue->front = lru_queue->front->next_queue;

	pthread_mutex_unlock(&lru_queue->lock);

	bucket->next_queue = NULL;

	return bucket;
}

static void delete_operation(lru_queue_t* lru_queue, bucket_t* bucket) {
	bucket_t* next = bucket->next_queue;
	bucket_t* prev = bucket->prev_queue;

	if (next)
		next->prev_queue = prev;

	if (prev)
		prev->next_queue = next;

	bucket->next_queue = NULL;
}

void lru_queue_delete(lru_queue_t* lru_queue, bucket_t* bucket) {
	pthread_mutex_lock(&lru_queue->lock);

	delete_operation(lru_queue, bucket);

	pthread_mutex_unlock(&lru_queue->lock);
}

void lru_queue_reenqueue(lru_queue_t* lru_queue, bucket_t* bucket) {
	pthread_mutex_lock(&lru_queue->lock);

	delete_operation(lru_queue, bucket);

	enqueue_operation(lru_queue, bucket);

	pthread_mutex_unlock(&lru_queue->lock);
}
