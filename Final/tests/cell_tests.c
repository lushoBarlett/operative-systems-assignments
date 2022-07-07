#include "cell_tests.h"

#include <assert.h>

#include "../src/cell.h"
#include "test_utils.h"

static cell_t init() {
	cell_t cell;

	cell_init(&cell);

	return cell;
}

static const bucket_t* find_from_string(cell_t* cell, const char* key) {
	blob_t bkey = blob_from_string(key);

	const bucket_t* found = cell_find(cell, bkey);

	blob_free(bkey);

	return found;
}

static void delete_from_string(cell_t* cell, const char* key) {
	blob_t bkey = blob_from_string(key);

	cell_delete(cell, bkey);

	blob_free(bkey);
}

static void empty_cell_is_null() {
	cell_t cell = init();

	blob_t key = blob_empty();

	assert(cell_find(&cell, key) == NULL);

	cell_free(&cell);
}

static void find_returns_inserted_bucket() {
	cell_t cell = init();

	bucket_t* bucket = bucket_from_strings("key", "value");

	cell_insert(&cell, bucket);

	const bucket_t* found = find_from_string(&cell, "key");

	assert(found == bucket);

	cell_free(&cell);
}

static void delete_removes_bucket() {
	cell_t cell = init();

	bucket_t* bucket = bucket_from_strings("key", "value");

	cell_insert(&cell, bucket);

	delete_from_string(&cell, "key");

	const bucket_t* found = find_from_string(&cell, "key");

	assert(found == NULL);

	cell_free(&cell);
}

static void find_among_many() {
	cell_t cell = init();

	bucket_t* bucket1 = bucket_from_strings("key1", "value1");
	bucket_t* bucket2 = bucket_from_strings("key2", "value2");
	bucket_t* bucket3 = bucket_from_strings("key3", "value3");

	cell_insert(&cell, bucket1);
	cell_insert(&cell, bucket2);
	cell_insert(&cell, bucket3);

	assert(bucket1 == find_from_string(&cell, "key1"));
	assert(bucket2 == find_from_string(&cell, "key2"));
	assert(bucket3 == find_from_string(&cell, "key3"));

	cell_free(&cell);
}

static void reinsert_same_key() {
	cell_t cell = init();

	for (size_t reps = 0; reps < 10000; reps++) {
		bucket_t* bucket = bucket_from_strings("key", "value");
		
		cell_insert(&cell, bucket);
		
		assert(bucket == find_from_string(&cell, "key"));
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
		bucket_t* bucket = bucket_from_strings(args.key, args.value);

		cell_insert(args.cell, bucket);

		const bucket_t* found = find_from_string(args.cell, args.key);
		
		assert(bucket == found);

		delete_from_string(args.cell, args.key);

		assert(NULL == find_from_string(args.cell, args.key));
	}
}

PTHREAD_API(common, struct common_args)

static void reinsertion(struct common_args args) {
	blob_t key = blob_from_string(args.key);

	for (size_t i = 0; i < args.repetitions; i++) {
		bucket_t* bucket = bucket_from_strings(args.key, args.value);

		cell_insert(args.cell, bucket);

		const bucket_t* found = find_from_string(args.cell, args.key);

		assert(found);

		assert(blob_equals(key, found->key));
	}

	blob_free(key);
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
	TEST_SUITE(cell);

	TEST(empty_cell_is_null());
	TEST(find_returns_inserted_bucket());
	TEST(delete_removes_bucket());
	TEST(find_among_many());
	TEST(reinsert_same_key());
	TEST(concurrency_test());
	TEST(concurrent_reinsertion());
}
