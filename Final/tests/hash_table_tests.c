#include "hash_table_tests.h"

#include <assert.h>
#include <stdlib.h>

#include "../src/hash_table.h"
#include "test_utils.h"

static hash_table_t init(size_t capacity) {
	hash_table_t hash_table;

	hash_table_init(&hash_table, hash_table_cells(capacity), capacity);

	return hash_table;
}

static void free_all(hash_table_t* hash_table) {
	for (size_t i = 0; i < hash_table->capacity; i++)
		while (hash_table->cells[i].bucket)
			cell_delete_bucket(&hash_table->cells[i], hash_table->cells[i].bucket);

	free(hash_table->cells);
}

static void empty_cell_is_null() {
	hash_table_t hash_table = init(INITIAL_CAPACITY);

	blob_t key = blob_empty();

	assert(hash_table_lookup(&hash_table, key) == NULL);

	free_all(&hash_table);
}

static void find_returns_inserted_value() {
	hash_table_t hash_table = init(INITIAL_CAPACITY);

	blob_t key = blob_from_string("key");
	blob_t value = blob_from_string("value");

	hash_table_insert(&hash_table, bucket_create(key, value));

	bucket_t* found = hash_table_lookup(&hash_table, key);

	assert(blob_equals(found->value, value));

	bucket_dereference(found);

	free_all(&hash_table);
}

static void delete_removes_value() {
	hash_table_t hash_table = init(INITIAL_CAPACITY);

	blob_t key = blob_from_string("key");
	blob_t value = blob_from_string("value");

	hash_table_insert(&hash_table, bucket_create(key, value));

	key = blob_from_string("key");

	bucket_dereference(hash_table_delete(&hash_table, key));

	assert(hash_table_lookup(&hash_table, key) == NULL);

	free_all(&hash_table);

	blob_free(key);
}

static void find_among_many() {
	hash_table_t hash_table = init(INITIAL_CAPACITY);

	blob_t key1 = blob_from_string("key1");
	blob_t key2 = blob_from_string("key2");
	blob_t key3 = blob_from_string("key3");

	blob_t value1 = blob_from_string("value1");
	blob_t value2 = blob_from_string("value2");
	blob_t value3 = blob_from_string("value3");

	hash_table_insert(&hash_table, bucket_create(key1, value1));
	hash_table_insert(&hash_table, bucket_create(key2, value2));
	hash_table_insert(&hash_table, bucket_create(key3, value3));

	bucket_t* found1 = hash_table_lookup(&hash_table, key1);
	bucket_t* found2 = hash_table_lookup(&hash_table, key2);
	bucket_t* found3 = hash_table_lookup(&hash_table, key3);

	assert(blob_equals(value1, found1->value));
	assert(blob_equals(value2, found2->value));
	assert(blob_equals(value3, found3->value));

	bucket_dereference(found1);
	bucket_dereference(found2);
	bucket_dereference(found3);

	free_all(&hash_table);
}

static void intensive_work() {
	hash_table_t hash_table = init(INITIAL_CAPACITY);

	char string[] = "\0";

	blob_t keys[256];
	blob_t values[256];

	for (size_t i = 0; i < 256; i++) {
		string[0] = (char)i;

		keys[i] = blob_from_string(string);
		values[i] = blob_from_string(string);

		hash_table_insert(&hash_table, bucket_create(keys[i], values[i]));
	}

	assert(counter_get(&hash_table.size) == 256);

	for (size_t i = 0; i < 256; i++) {
		bucket_t* found = hash_table_lookup(&hash_table, keys[i]);

		assert(blob_equals(values[i], found->value));

		bucket_dereference(found);
	}

	for (size_t i = 0; i < 256; i++)
		bucket_dereference(hash_table_delete(&hash_table, keys[i]));

	assert(counter_get(&hash_table.size) == 0);

	free_all(&hash_table);
}

// TODO: test moves

void hash_table_tests() {
	TEST_SUITE(hash_table);

	TEST(empty_cell_is_null());
	TEST(find_returns_inserted_value());
	TEST(delete_removes_value());
	TEST(find_among_many());
	TEST(intensive_work());
}
