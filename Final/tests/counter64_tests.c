#include "counter64_tests.h"

#include <assert.h>

#include "../src/counter64.h"
#include "test_utils.h"

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

static void increment(counter64_t* counter) {
	for (size_t i = 0; i < REPETITIONS; i++)
		counter_increment(counter);
}

static void add(counter64_t* counter) {
	for (size_t i = 0; i < REPETITIONS; i++)
		counter_add(counter, 5);
}

static void sub(counter64_t* counter) {
	for (size_t i = 0; i < REPETITIONS; i++)
		counter_add(counter, 2);
}

PTHREAD_API(increment, counter64_t*)
PTHREAD_API(add, counter64_t*)
PTHREAD_API(sub, counter64_t*)

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

	pthread_t* threads = create_threads(3);

	void* arg = &counter;

	spawn_thread(&threads[0], PTHREAD_CALLER(increment), &arg);
	spawn_thread(&threads[1], PTHREAD_CALLER(add), &arg);
	spawn_thread(&threads[2], PTHREAD_CALLER(sub), &arg);

	join_threads(threads, 3);

	uint64_t value = counter_get(&counter);

	assert(value == 50 + (1 + 5 + 2) * REPETITIONS);
}

void counter64_tests() {
	create();
	operate();
	concurrent_operate();
}
