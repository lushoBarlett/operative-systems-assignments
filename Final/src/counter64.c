#include "counter64.h"

#include <stdint.h>
#include <pthread.h>

void counter_init(counter64_t* counter, uint64_t value) {
	counter->value = value;
	pthread_mutex_init(&counter->lock, NULL);
}

void counter_add(counter64_t* counter, uint64_t amount) {
	pthread_mutex_lock(&counter->lock);
	counter->value += amount;
	pthread_mutex_unlock(&counter->lock);
}

void counter_sub(counter64_t* counter, uint64_t amount) {
	pthread_mutex_lock(&counter->lock);
	counter->value -= amount;
	pthread_mutex_unlock(&counter->lock);
}

void counter_increment(counter64_t* counter) {
	counter_add(counter, 1);
}

uint64_t counter_get(counter64_t* counter) {
	return counter->value;
}
