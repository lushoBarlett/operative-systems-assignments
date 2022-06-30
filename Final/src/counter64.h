#pragma once

#include <stdint.h>
#include <pthread.h>

typedef struct counter64_t {
	uint64_t value;	
	pthread_mutex_t lock;
} counter64_t;

void counter_init(counter64_t* counter, uint64_t value);

void counter_add(counter64_t* counter, uint64_t amount);

void counter_sub(counter64_t* counter, uint64_t amount);

void counter_increment(counter64_t* counter);

uint64_t counter_get(counter64_t* counter);
