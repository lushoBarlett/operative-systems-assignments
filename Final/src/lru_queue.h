#pragma once

#include "database.h"

void lru_queue_init(lru_queue_t* lru_queue);

void lru_queue_enqueue(lru_queue_t* lru_queue, bucket_t* bucket);

bucket_t* lru_queue_dequeue(lru_queue_t* lru_queue);

void lru_queue_delete(lru_queue_t* lru_queue, bucket_t* bucket);

void lru_queue_reenqueue(lru_queue_t* lru_queue, bucket_t* bucket);
