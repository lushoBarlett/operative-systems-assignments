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

	lru_queue_enqueue(&lru_queue, bucket);

	assert(bucket == lru_queue_dequeue(&lru_queue));

	free(bucket);
}

static void dequeue_removes_bucket() {
	lru_queue_t lru_queue = init();

	bucket_t* bucket = new_bucket();

	lru_queue_enqueue(&lru_queue, bucket);

	lru_queue_dequeue(&lru_queue);

	assert(bucket->next_queue == NULL);
	assert(bucket->prev_queue == NULL);

	free(bucket);
}

static void reenqueue_moves_bucket_back() {
	lru_queue_t lru_queue = init();

	bucket_t* front = new_bucket();
	bucket_t* middle = new_bucket();
	bucket_t* back = new_bucket();

	lru_queue_enqueue(&lru_queue, front);
	lru_queue_enqueue(&lru_queue, middle);
	lru_queue_enqueue(&lru_queue, back);

	lru_queue_reenqueue(&lru_queue, middle);

	assert(front == lru_queue_dequeue(&lru_queue));
	assert(back == lru_queue_dequeue(&lru_queue));
	assert(middle == lru_queue_dequeue(&lru_queue));

	free(front);
	free(middle);
	free(back);
}

static void delete_removes_bucket() {
	lru_queue_t lru_queue = init();

	bucket_t* front = new_bucket();
	bucket_t* middle = new_bucket();
	bucket_t* back = new_bucket();

	lru_queue_enqueue(&lru_queue, front);
	lru_queue_enqueue(&lru_queue, middle);
	lru_queue_enqueue(&lru_queue, back);

	lru_queue_delete(&lru_queue, middle);

	assert(front == lru_queue_dequeue(&lru_queue));
	assert(back == lru_queue_dequeue(&lru_queue));
	assert(NULL == lru_queue_dequeue(&lru_queue));

	assert(middle->next_queue == NULL);
	assert(middle->prev_queue == NULL);

	free(front);
	free(middle);
	free(back);
}

static void empty_queue_has_null() {
	lru_queue_t lru_queue = init();

	assert(lru_queue.front == NULL);
	assert(lru_queue.back == NULL);

	lru_queue_enqueue(&lru_queue, new_bucket());

	free(lru_queue_dequeue(&lru_queue));

	assert(lru_queue.front == NULL);
	assert(lru_queue.back == NULL);
	
	bucket_t* bucket = new_bucket();
	
	lru_queue_enqueue(&lru_queue, bucket);

	lru_queue_delete(&lru_queue, bucket);

	free(bucket);

	assert(lru_queue.front == NULL);
	assert(lru_queue.back == NULL);
}

void lru_queue_tests() {
	empty_dequeue_is_null();
	enqueue_dequeue_are_the_same();
	dequeue_removes_bucket();
	reenqueue_moves_bucket_back();
	delete_removes_bucket();
	empty_queue_has_null();
}
