#pragma once

#include "bucket.h"

#include <pthread.h>

/*
 * TODO: LRU
 */
typedef struct {
	bucket_t* back;
	bucket_t* front;
	pthread_mutex_t lock;
} lru_queue_t;

void lru_queue_init(lru_queue_t* lru_queue);

void lru_queue_lock(lru_queue_t* lru_queue);

void lru_queue_unlock(lru_queue_t* lru_queue);

void lru_queue_enqueue(lru_queue_t* lru_queue, bucket_t* bucket);

bucket_t* lru_queue_dequeue(lru_queue_t* lru_queue);

void lru_queue_delete(lru_queue_t* lru_queue, bucket_t* bucket);

void lru_queue_reenqueue(lru_queue_t* lru_queue, bucket_t* bucket);
