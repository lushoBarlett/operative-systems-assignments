#include "database_tests.h"

#include <assert.h>
#include <time.h>
#include <stdlib.h>
#include <sys/time.h>
#include <sys/resource.h>

#include "../src/database.h"
#include "test_utils.h"

static database_t init() {
	database_t database;

	database_init(&database);

	return database;
}

static bucket_t* database_get_from_string(database_t* database, const char* key) {
	blob_t bkey = blob_from_string(key);

	bucket_t* found = database_get(database, bkey);

	blob_free(bkey);

	return found;
}

static bucket_t* database_take_from_string(database_t* database, const char* key) {
	blob_t bkey = blob_from_string(key);

	bucket_t* found = database_take(database, bkey);

	blob_free(bkey);

	return found;
}

static void database_delete_from_string(database_t* database, const char* key) {
	blob_t bkey = blob_from_string(key);

	database_delete(database, bkey);

	blob_free(bkey);
}

static bucket_t* database_put_from_strings(database_t* database, const char* key, const char* value) {
	bucket_t* bucket = bucket_from_strings(key, value);

	database_put(database, bucket);

	return SHARE(bucket);
}

static void assert_equals_then_dereference(bucket_t* expected, bucket_t* got) {
	assert(expected == got);

	bucket_dereference(expected);
	bucket_dereference(got);
}

static void destroy_deletes_memory() {
	database_t database = init();

	bucket_dereference(database_put_from_strings(&database, "key", "value"));

	database_destroy(&database);
}

static void put_then_get_returns_the_same_bucket() {
	database_t database = init();

	bucket_t* bucket = database_put_from_strings(&database, "key", "value");

	bucket_t* found = database_get_from_string(&database, "key");

	assert_equals_then_dereference(bucket, found);

	database_destroy(&database);
}

static void put_then_take_returns_the_same_bucket() {
	database_t database = init();

	bucket_t* bucket = database_put_from_strings(&database, "key", "value");

	bucket_t* found = database_take_from_string(&database, "key");

	assert_equals_then_dereference(bucket, found);

	database_destroy(&database);
}

static void get_after_take_returns_null() {
	database_t database = init();

	bucket_dereference(database_put_from_strings(&database, "key", "value"));

	bucket_dereference(database_take_from_string(&database, "key"));

	assert(NULL == database_get_from_string(&database, "key"));

	database_destroy(&database);
}

static void get_after_delete_returns_null() {
	database_t database = init();

	bucket_dereference(database_put_from_strings(&database, "key", "value"));

	database_delete_from_string(&database, "key");

	assert(NULL == database_get_from_string(&database, "key"));

	database_destroy(&database);
}

static void get_after_reinsertion_returs_the_second_value() {
	database_t database = init();

	bucket_dereference(database_put_from_strings(&database, "key", "value1"));

	bucket_t* second = database_put_from_strings(&database, "key", "value2");

	bucket_t* found = database_get_from_string(&database, "key");

	assert_equals_then_dereference(second, found);

	database_destroy(&database);
}

static blob_t make_random_blob_from(uint64_t* memory) {
	*memory = rand();
	*memory = *memory << 32 | rand();

	return (blob_t) {
		.memory = memory,
		.bytes = 8,
	};
}

static blob_t random_blob() {
	return make_random_blob_from(malloc(sizeof(uint64_t)));
}

static void should_expand_along_with_insertions() {
	database_t database = init();

	for (size_t i = 0; i < 10000; i++) {
		database_put(&database, bucket_create(random_blob(), random_blob()));

		assert(counter_get(&database.size) < database.capacity);
	}

	database_destroy(&database);
}

static blob_t database_memsafe_random_blob(database_t* database) {
	return make_random_blob_from(database_memsafe_malloc(database, sizeof(uint64_t)));
}

static bucket_t* database_memsafe_bucket_create(database_t* database, blob_t key, blob_t value) {
	bucket_t* bucket = database_memsafe_malloc(database, sizeof(bucket_t));

	bucket_init(bucket, key, value);

	return bucket;
}

static void database_releases_memory_when_needed() {
	struct rlimit restore;
	getrlimit(RLIMIT_AS, &restore);

	size_t bytes =
		sizeof(cell_t) +
		sizeof(bucket_t) +
		2 * sizeof(uint64_t);

	struct rlimit newlimit = {
		.rlim_cur = bytes,
		.rlim_max = bytes,
	};
	setrlimit(RLIMIT_AS, &newlimit);

	database_t database = init();

	for (size_t i = 0; i < 10000; i++) {
		blob_t key = database_memsafe_random_blob(&database);
		blob_t value = database_memsafe_random_blob(&database);

		bucket_t* bucket = database_memsafe_bucket_create(&database, key, value);

		database_put(&database, bucket);
	}

	database_destroy(&database);

	setrlimit(RLIMIT_AS, &restore);
}

void database_tests() {
	srand(time(NULL));

	TEST_SUITE(database);

	TEST(destroy_deletes_memory());
	TEST(put_then_get_returns_the_same_bucket());
	TEST(put_then_take_returns_the_same_bucket());
	TEST(get_after_take_returns_null());
	TEST(get_after_delete_returns_null());
	TEST(get_after_reinsertion_returs_the_second_value());
	TEST(should_expand_along_with_insertions());
	TEST(database_releases_memory_when_needed());
}
