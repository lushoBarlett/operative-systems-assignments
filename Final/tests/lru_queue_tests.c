#include "lru_queue_tests.h"

#include <stdlib.h>
#include <assert.h>

#include "../src/lru_queue.h"
#include "test_utils.h"

static lru_queue_t init() {
	lru_queue_t lru_queue;

	lru_queue_init(&lru_queue);

	return lru_queue;
}

static void empty_dequeue_is_null() {
	lru_queue_t lru_queue = init();

	assert(lru_queue_dequeue(&lru_queue) == NULL);
}

static void enqueue_dequeue_are_the_same() {
	lru_queue_t lru_queue = init();

	bucket_t* bucket = new_bucket();

	lru_queue_enqueue(&lru_queue, SHARE(bucket));

	bucket_t* front = lru_queue_dequeue(&lru_queue);

	assert(bucket == front);

	bucket_dereference(front);
	bucket_dereference(bucket);
}

static void dequeue_removes_bucket() {
	lru_queue_t lru_queue = init();

	bucket_t* bucket = new_bucket();

	lru_queue_enqueue(&lru_queue, SHARE(bucket));

	bucket_dereference(lru_queue_dequeue(&lru_queue));

	assert(bucket->next_queue == NULL);
	assert(bucket->prev_queue == NULL);

	bucket_dereference(bucket);
}

static void assert_equals_and_dereference(bucket_t* expected, bucket_t* got) {
	assert(expected == got);

	if (expected)
		bucket_dereference(expected);

	if (got)
		bucket_dereference(got);
}

static void reenqueue_moves_bucket_back() {
	lru_queue_t lru_queue = init();

	bucket_t* front = new_bucket();
	bucket_t* middle = new_bucket();
	bucket_t* back = new_bucket();

	lru_queue_enqueue(&lru_queue, SHARE(front));
	lru_queue_enqueue(&lru_queue, SHARE(middle));
	lru_queue_enqueue(&lru_queue, SHARE(back));

	lru_queue_reenqueue(&lru_queue, middle);

	assert_equals_and_dereference(front, lru_queue_dequeue(&lru_queue));
	assert_equals_and_dereference(back, lru_queue_dequeue(&lru_queue));
	assert_equals_and_dereference(middle, lru_queue_dequeue(&lru_queue));
}

static void delete_removes_bucket() {
	lru_queue_t lru_queue = init();

	bucket_t* front = new_bucket();
	bucket_t* middle = new_bucket();
	bucket_t* back = new_bucket();

	lru_queue_enqueue(&lru_queue, SHARE(front));
	lru_queue_enqueue(&lru_queue, SHARE(middle));
	lru_queue_enqueue(&lru_queue, SHARE(back));

	lru_queue_delete(&lru_queue, middle);

	assert_equals_and_dereference(front, lru_queue_dequeue(&lru_queue));
	assert_equals_and_dereference(back, lru_queue_dequeue(&lru_queue));
	assert_equals_and_dereference(NULL, lru_queue_dequeue(&lru_queue));

	assert(middle->next_queue == NULL);
	assert(middle->prev_queue == NULL);

	bucket_dereference(middle);
}

static void empty_queue_has_null() {
	lru_queue_t lru_queue = init();

	assert(lru_queue.front == NULL);
	assert(lru_queue.back == NULL);

	lru_queue_enqueue(&lru_queue, new_bucket());

	bucket_dereference(lru_queue_dequeue(&lru_queue));

	assert(lru_queue.front == NULL);
	assert(lru_queue.back == NULL);
	
	bucket_t* bucket = new_bucket();
	
	lru_queue_enqueue(&lru_queue, SHARE(bucket));

	lru_queue_delete(&lru_queue, bucket);

	bucket_dereference(bucket);

	assert(lru_queue.front == NULL);
	assert(lru_queue.back == NULL);
}

struct common_args {
	lru_queue_t* lru_queue;
	const char* key;
	const char* value;
	size_t repetitions;
};

static void atomic_enqueue(lru_queue_t* lru_queue, bucket_t* bucket) {
	lru_queue_lock(lru_queue);

	lru_queue_enqueue(lru_queue, bucket);

	lru_queue_unlock(lru_queue);
}

static void atomic_dequeue(lru_queue_t* lru_queue) {
	lru_queue_lock(lru_queue);

	bucket_try_dereference(lru_queue_dequeue(lru_queue));

	lru_queue_unlock(lru_queue);
}

static void atomic_delete(lru_queue_t* lru_queue, bucket_t* bucket) {
	lru_queue_lock(lru_queue);

	lru_queue_delete(lru_queue, bucket);

	lru_queue_unlock(lru_queue);
}

static void enqueuer(struct common_args args) {
	for (size_t i = 0; i < args.repetitions; i++)
		atomic_enqueue(args.lru_queue, bucket_from_strings(args.key, args.value));
}

PTHREAD_API(enqueuer, struct common_args)

static void dequeuer(struct common_args args) {
	for (size_t i = 0; i < args.repetitions; i++)
		atomic_dequeue(args.lru_queue);
}

PTHREAD_API(dequeuer, struct common_args)

static void enqueue_dequeue_concurrently() {
	lru_queue_t lru_queue = init();

	pthread_t* threads = create_threads(2);

	struct common_args args1 = (struct common_args){ &lru_queue, "key", "value", 100000 };
	spawn_thread(&threads[0], PTHREAD_CALLER(enqueuer), &args1);

	struct common_args args2 = (struct common_args){ &lru_queue, NULL, NULL, 100000 };
	spawn_thread(&threads[1], PTHREAD_CALLER(dequeuer), &args2);

	join_threads(threads, 2);

	bucket_t* bucket;

	while ((bucket = lru_queue_dequeue(&lru_queue)))
		bucket_dereference(bucket);
}

static void common(struct common_args args) {
	for (size_t i = 0; i < args.repetitions; i++) {
		bucket_t* bucket = bucket_from_strings(args.key, args.value);

		atomic_enqueue(args.lru_queue, SHARE(bucket));

		atomic_delete(args.lru_queue, bucket);

		bucket_dereference(bucket);
	}
}

PTHREAD_API(common, struct common_args)

static void enqueue_release_lock_delete() {
	lru_queue_t lru_queue = init();

	pthread_t* threads = create_threads(10);

	struct common_args args = (struct common_args){ &lru_queue, "key", "value", 100000 };
	spawn_threads(threads, 10, PTHREAD_CALLER(common), &args);

	join_threads(threads, 10);

	assert(NULL == lru_queue_dequeue(&lru_queue));
}

void lru_queue_tests() {
	TEST_SUITE(lru_queue);

	TEST(empty_dequeue_is_null());
	TEST(enqueue_dequeue_are_the_same());
	TEST(dequeue_removes_bucket());
	TEST(reenqueue_moves_bucket_back());
	TEST(delete_removes_bucket());
	TEST(empty_queue_has_null());
	TEST(enqueue_dequeue_concurrently());
	TEST(enqueue_release_lock_delete());
}
