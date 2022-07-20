#include "rw_lock_tests.h"

#include <assert.h>
#include <semaphore.h>

#include "../src/rw_lock.h"
#include "test_utils.h"

static rw_lock_t init() {
	rw_lock_t rw_lock;

	readers_writer_lock_init(&rw_lock);

	return rw_lock;
}

typedef struct {
	rw_lock_t* rw_lock;
	uint8_t* number;
	sem_t* entered;
} action_orden_t;

static void attempt_write_action(action_orden_t args) {
	sem_post(args.entered);
	write_lock(args.rw_lock);
	*args.number = 0;
	write_unlock(args.rw_lock);
}
PTHREAD_API(attempt_write_action, action_orden_t)

static void attempt_read_action(action_orden_t args) {
	sem_post(args.entered);
	write_lock(args.rw_lock);
	*args.number = 1;
	write_unlock(args.rw_lock);
}
PTHREAD_API(attempt_read_action, action_orden_t)

static void reader_blocks_writer() {
	rw_lock_t rw_lock = init();

	pthread_t* threads = create_threads(1);

	uint8_t number;

	sem_t entered;
	sem_init(&entered, 0, 0);

	action_orden_t args = {
		.rw_lock = &rw_lock,
		.number = &number,
		.entered = &entered,
	};

	read_lock(&rw_lock);

	spawn_thread(&threads[0], PTHREAD_CALLER(attempt_write_action), &args);

	sem_wait(&entered);

	number = 1;

	read_unlock(&rw_lock);

	join_threads(threads, 1);

	assert(number == 0);
}

static void writer_blocks_reader() {
	rw_lock_t rw_lock = init();

	pthread_t* threads = create_threads(1);

	uint8_t number;

	sem_t entered;
	sem_init(&entered, 0, 0);

	action_orden_t args = {
		.rw_lock = &rw_lock,
		.number = &number,
		.entered = &entered,
	};

	write_lock(&rw_lock);

	spawn_thread(&threads[0], PTHREAD_CALLER(attempt_read_action), &args);

	sem_wait(&entered);

	number = 0;

	write_unlock(&rw_lock);

	join_threads(threads, 1);

	assert(number == 1);
}

typedef struct {
	rw_lock_t* rw_lock;
	sem_t* entered;
	uint8_t i;
} multiple_readers_t;

static void multiples_readers_test(multiple_readers_t args) {
	read_lock(args.rw_lock);
	sem_post(&args.entered[args.i]);
	sem_wait(&args.entered[!args.i]);
	read_unlock(args.rw_lock);
}
PTHREAD_API(multiples_readers_test, multiple_readers_t)

static void multiple_readers_are_allowed() {
	rw_lock_t rw_lock = init();

	pthread_t* threads = create_threads(2);

	sem_t entered[2];

	sem_init(&entered[0], 0, 0);
	sem_init(&entered[1], 0, 0);

	multiple_readers_t args0 = {
		.rw_lock = &rw_lock,
		.entered = entered,
		.i = 0,
	};
	spawn_thread(&threads[0], PTHREAD_CALLER(multiples_readers_test), &args0);

	multiple_readers_t args1 = {
		.rw_lock = &rw_lock,
		.entered = entered,
		.i = 1,
	};
	spawn_thread(&threads[1], PTHREAD_CALLER(multiples_readers_test), &args1);

	join_threads(threads, 2);
}

typedef struct {
	rw_lock_t* rw_lock;
	uint32_t repetitions;
	uint32_t* number;
} multiple_writers_t;

static void multiples_writers_test(multiple_writers_t args) {
	for (size_t i = 0; i < args.repetitions; i++) {
		write_lock(args.rw_lock);
		(*args.number)++;
		write_unlock(args.rw_lock);
	}
}
PTHREAD_API(multiples_writers_test, multiple_writers_t)

static void multiple_writers_not_allowed() {
	rw_lock_t rw_lock = init();

	pthread_t* threads = create_threads(8);

	uint32_t number = 0;

	multiple_writers_t args = {
		.rw_lock = &rw_lock,
		.repetitions = 1000,
		.number = &number,
	};
	spawn_threads(threads, 8, PTHREAD_CALLER(multiples_writers_test), &args);

	join_threads(threads, 8);

	assert(number == 8 * 1000);
}

typedef struct {
	rw_lock_t* rw_lock;
	sem_t* finish;
} write_preferring_t;

static void write_preferring_test(write_preferring_t args) {
	while (sem_trywait(args.finish)) {
		read_lock(args.rw_lock);
		read_unlock(args.rw_lock);
	}
}
PTHREAD_API(write_preferring_test, write_preferring_t)

static void writer_kicks_out_readers() {
	rw_lock_t rw_lock = init();

	pthread_t* threads = create_threads(4);

	sem_t finish;

	sem_init(&finish, 0, 0);

	write_preferring_t args = {
		.rw_lock = &rw_lock,
		.finish = &finish,
	};

	spawn_threads(threads, 4, PTHREAD_CALLER(write_preferring_test), &args);

	for (size_t i = 0; i < 100; i++) {
		write_lock(&rw_lock);
		write_unlock(&rw_lock);
	}

	for (size_t i = 0; i < 4; i++)
		sem_post(&finish);

	join_threads(threads, 4);
}

void rw_lock_tests() {
	TEST_SUITE(rw_lock);

	TEST(reader_blocks_writer());
	TEST(writer_blocks_reader());
	TEST(multiple_readers_are_allowed());
	TEST(multiple_writers_not_allowed());
	TEST(writer_kicks_out_readers());
}
