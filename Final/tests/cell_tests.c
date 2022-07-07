#include "cell_tests.h"

#include <assert.h>
#include <stdio.h>

#include "../src/cell.h"
#include "test_utils.h"

static cell_t init() {
	cell_t cell;

	cell_init(&cell);

	return cell;
}

static void empty_cell_is_null() {
	cell_t cell = init();

	blob_t key = blob_empty();

	assert(cell_find(&cell, key) == NULL);

	cell_free(&cell);
}

static void find_returns_inserted_bucket() {
	cell_t cell = init();

	blob_t key = blob_from_string("key");
	blob_t value = blob_from_string("value");

	bucket_t* bucket = bucket_create(key, value);

	cell_insert(&cell, bucket);

	const bucket_t* found = cell_find(&cell, key);

	assert(found == bucket);

	cell_free(&cell);
}

static void delete_removes_bucket() {
	cell_t cell = init();

	blob_t key = blob_from_string("key");
	blob_t value = blob_from_string("value");

	bucket_t* bucket = bucket_create(key, value);

	cell_insert(&cell, bucket);

	cell_delete(&cell, key);

	const bucket_t* found = cell_find(&cell, key);

	assert(found == NULL);

	cell_free(&cell);
}

static void find_among_many() {
	cell_t cell = init();

	blob_t key1 = blob_from_string("key1");
	blob_t key2 = blob_from_string("key2");
	blob_t key3 = blob_from_string("key3");

	blob_t value1 = blob_from_string("value1");
	blob_t value2 = blob_from_string("value2");
	blob_t value3 = blob_from_string("value3");

	bucket_t* bucket1 = bucket_create(key1, value1);
	bucket_t* bucket2 = bucket_create(key2, value2);
	bucket_t* bucket3 = bucket_create(key3, value3);

	cell_insert(&cell, bucket1);
	cell_insert(&cell, bucket2);
	cell_insert(&cell, bucket3);

	assert(bucket1 == cell_find(&cell, key1));
	assert(bucket2 == cell_find(&cell, key2));
	assert(bucket3 == cell_find(&cell, key3));

	cell_free(&cell);
}

static void reinsert_same_key() {
	cell_t cell = init();

	for (size_t reps = 0; reps < 10000; reps++) {
		blob_t key = blob_from_string("key");
		blob_t value = blob_from_string("value");

		bucket_t* bucket = bucket_create(key, value);
		
		cell_insert(&cell, bucket);
		
		assert(bucket == cell_find(&cell, key));
	}

	cell_free(&cell);
}

struct common_args {
	cell_t* cell;
	const char* key;
	const char* value;
	size_t repetitions;
};

static void common(struct common_args args) {
	for (size_t i = 0; i < args.repetitions; i++) {
		blob_t key = blob_from_string(args.key);

		bucket_t* bucket = bucket_create(
			blob_from_string(args.key), blob_from_string(args.value));

		cell_insert(args.cell, bucket);

		const bucket_t* found = cell_find(args.cell, key);
		
		assert(bucket == found);

		cell_delete(args.cell, key);

		assert(NULL == cell_find(args.cell, key));

		blob_free(key);
	}
}

PTHREAD_API(common, struct common_args)

static void reinsertion(struct common_args args) {
	for (size_t i = 0; i < args.repetitions; i++) {
		blob_t key = blob_from_string(args.key);

		bucket_t* bucket = bucket_create(
			blob_from_string(args.key), blob_from_string(args.value));

		cell_insert(args.cell, bucket);

		const bucket_t* found = cell_find(args.cell, key);

		assert(found);
		int result = blob_equals(key, found->key);
		result = blob_equals(key, found->key);
		assert(result);
		blob_free(key);
	}
}

PTHREAD_API(reinsertion, struct common_args)

static void concurrent_reinsertion() {
	cell_t cell = init();

	pthread_t* threads = create_threads(2);

	struct common_args args1 = (struct common_args){ &cell, "key", "value1", 100000 };
	spawn_thread(&threads[0], PTHREAD_CALLER(reinsertion), &args1);

	struct common_args args2 = (struct common_args){ &cell, "key", "value2", 100000 };
	spawn_thread(&threads[1], PTHREAD_CALLER(reinsertion), &args2);

	join_threads(threads, 2);

	cell_free(&cell);
}

static void concurrency_test() {
	cell_t cell = init();

	pthread_t* threads = create_threads(3);

	struct common_args args1 = (struct common_args){ &cell, "key1", "value1", 100000 };
	spawn_thread(&threads[0], PTHREAD_CALLER(common), &args1);

	struct common_args args2 = (struct common_args){ &cell, "key2", "value2", 100000 };
	spawn_thread(&threads[1], PTHREAD_CALLER(common), &args2);

	struct common_args args3 = (struct common_args){ &cell, "key3", "value3", 100000 };
	spawn_thread(&threads[2], PTHREAD_CALLER(common), &args3);

	join_threads(threads, 3);

	cell_free(&cell);
}

void cell_tests() {
	empty_cell_is_null();
	find_returns_inserted_bucket();
	delete_removes_bucket();
	find_among_many();
	reinsert_same_key();
	concurrency_test();
	concurrent_reinsertion();
}
