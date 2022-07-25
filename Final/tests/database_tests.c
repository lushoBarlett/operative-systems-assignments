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

	database_put(database, SHARE(bucket));

	return bucket;
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

static blob_t database_memsafe_random_blob(database_t* database) {
	return make_random_blob_from(
		database_memsafe_malloc(database, sizeof(uint64_t)));
}

static bucket_t* database_memsafe_bucket_create(database_t* database, blob_t key, blob_t value) {
	bucket_t* bucket = database_memsafe_malloc(database, sizeof(bucket_t));

	bucket_init(bucket, key, value);

	return bucket;
}

static void database_releases_memory_when_needed() {
	struct rlimit restore;
	getrlimit(RLIMIT_AS, &restore);

	size_t database_size = sizeof(cell_t) * CAPACITY;

	size_t blob_size = sizeof(blob_t) + 64;
	size_t bucket_size = sizeof(bucket_t) + 2 * blob_size;
	
	// asumimos que esto es para el stack y esas cosas
	size_t four_megabytes = 1 << 22;

	struct rlimit newlimit = {
		.rlim_cur = database_size + 100 * bucket_size + four_megabytes,
		.rlim_max = -1,
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

static void delete_the_same_element_concurrently() {
	database_t database = init();

	for (size_t i = 0; i < 10000; i++) {
		bucket_dereference(database_put_from_strings(&database, "key", "value"));

		#pragma omp parallel
		database_delete_from_string(&database, "key");
	}

	database_destroy(&database);
}

static void take_and_get_the_same_element_concurrently() {
	database_t database = init();

	for (size_t i = 0; i < 10000; i++) {
		bucket_t* bucket = database_put_from_strings(&database, "key", "value");

		bucket_t* got;
		bucket_t* took;

		#pragma omp parallel sections
		{
			#pragma omp section
			{
				took = database_take_from_string(&database, "key");
			}

			#pragma omp section
			{
				got = database_get_from_string(&database, "key");
			}
		}

		assert(got == bucket || got == NULL);
		assert(took == bucket);

		bucket_dereference(bucket);

		if (got)
			bucket_dereference(got);

		bucket_dereference(took);
	}

	database_destroy(&database);
}

static void insert_same_key_concurrently_and_get() {
	database_t database = init();

	for (size_t i = 0; i < 10000; i++) {

		bucket_t* first;
		bucket_t* second;
		bucket_t* got;

		#pragma omp parallel sections
		{
			#pragma omp section
			{
				first = database_put_from_strings(&database, "key", "value1");
			}

			#pragma omp section
			{
				second = database_put_from_strings(&database, "key", "value2");
			}
		}

		got = database_get_from_string(&database, "key");

		assert(got == first || got == second);

		bucket_dereference(first);
		bucket_dereference(second);

		assert(counter_get(&got->references) == 3);

		bucket_dereference(got);
	}

	database_destroy(&database);
}

static void free_memory_while_inserting() {
	database_t database = init();

	#pragma omp parallel for
	for (size_t i = 0; i < 1000000; i++) {
		blob_t key = database_memsafe_random_blob(&database);
		blob_t value = database_memsafe_random_blob(&database);
		bucket_t* bucket = database_memsafe_bucket_create(&database, key, value);

		database_put(&database, bucket);
	}

	database_destroy(&database);
}

void database_tests() {
	srand(time(NULL));

	TEST_SUITE(database);

	/*
	 * Corrido primero para probar los límites de memoria
	 */
	TEST(database_releases_memory_when_needed());

	TEST(destroy_deletes_memory());
	TEST(put_then_get_returns_the_same_bucket());
	TEST(put_then_take_returns_the_same_bucket());
	TEST(get_after_take_returns_null());
	TEST(get_after_delete_returns_null());
	TEST(get_after_reinsertion_returs_the_second_value());

	TEST(delete_the_same_element_concurrently());
	TEST(take_and_get_the_same_element_concurrently());
	TEST(insert_same_key_concurrently_and_get());
	TEST(free_memory_while_inserting());
}
