#include "counter64_tests.h"

#include <assert.h>

#include "../src/counter64.h"

static counter64_t init(uint64_t value) {
	counter64_t counter;

	counter_init(&counter, value);

	return counter;
}

static void create() {
	counter64_t counter = init(50);

	uint64_t value = counter_get(&counter);

	assert(value == 50);
}

#define REPETITIONS 10000

static void* increment(void* arg) {
	counter64_t* counter = arg;

	for (size_t i = 0; i < REPETITIONS; i++)
		counter_increment(counter);

	return NULL;
}

static void* add(void* arg) {
	counter64_t* counter = arg;

	for (size_t i = 0; i < REPETITIONS; i++)
		counter_add(counter, 5);

	return NULL;
}

static void* sub(void* arg) {
	counter64_t* counter = arg;

	for (size_t i = 0; i < REPETITIONS; i++)
		counter_add(counter, 2);

	return NULL;
}

static void operate() {
	counter64_t counter = init(50);

	counter_increment(&counter);
	counter_add(&counter, 5);
	counter_sub(&counter, 2);

	uint64_t value = counter_get(&counter);

	assert(value == 50 + 1 + 5 - 2);
}

static void concurrent_operate() {
	counter64_t counter = init(50);

	pthread_t threads[3];

	pthread_create(&threads[0], NULL, increment, &counter);
	pthread_create(&threads[1], NULL, add, &counter);
	pthread_create(&threads[2], NULL, sub, &counter);

	pthread_join(threads[0], NULL);
	pthread_join(threads[1], NULL);
	pthread_join(threads[2], NULL);

	uint64_t value = counter_get(&counter);

	assert(value == 50 + (1 + 5 + 2) * REPETITIONS);
}

void counter64_tests() {
	create();
	operate();
	concurrent_operate();
}
