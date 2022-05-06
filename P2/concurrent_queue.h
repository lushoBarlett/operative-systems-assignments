#pragma once

#include "queue.h"

#include <pthread.h>
#include <semaphore.h>

struct concurrent_queue_t {
	sem_t size;
	queue_t queue;
	pthread_mutex_t queue_lock;
};

typedef struct concurrent_queue_t concurrent_queue_t;

void concurrent_queue_init(concurrent_queue_t* cqueue);

void cenqueue(concurrent_queue_t* cqueue, void* value);

void* cdequeue(concurrent_queue_t* cqueue);