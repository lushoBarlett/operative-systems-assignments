#include "hash_table_tests.h"

#include <assert.h>

#include "../src/hash_table.h"
#include "test_utils.h"

static hash_table_t init() {
	hash_table_t hash_table;

	hash_table_init(&hash_table);

	return hash_table;
}

static void empty_cell_is_null() {
	hash_table_t hash_table = init();

	blob_t key = blob_empty();

	assert(blob_equals(hash_table_lookup(&hash_table, key), blob_empty()));

	hash_table_free(&hash_table);
}

static void find_returns_inserted_value() {
	hash_table_t hash_table = init();

	blob_t key = blob_from_string("key");
	blob_t value = blob_from_string("value");

	hash_table_insert(&hash_table, key, value);

	blob_t found = hash_table_lookup(&hash_table, key);

	assert(blob_equals(found, value));

	hash_table_free(&hash_table);
}

static void delete_removes_value() {
	hash_table_t hash_table = init();

	blob_t key = blob_from_string("key");
	blob_t value = blob_from_string("value");

	hash_table_insert(&hash_table, key, value);

	key = blob_from_string("key");

	hash_table_delete(&hash_table, key);

	blob_t found = hash_table_lookup(&hash_table, key);

	assert(blob_equals(found, blob_empty()));

	hash_table_free(&hash_table);

	blob_free(key);
}

static void find_among_many() {
	hash_table_t hash_table = init();

	blob_t key1 = blob_from_string("key1");
	blob_t key2 = blob_from_string("key2");
	blob_t key3 = blob_from_string("key3");

	blob_t value1 = blob_from_string("value1");
	blob_t value2 = blob_from_string("value2");
	blob_t value3 = blob_from_string("value3");

	hash_table_insert(&hash_table, key1, value1);
	hash_table_insert(&hash_table, key2, value2);
	hash_table_insert(&hash_table, key3, value3);

	assert(blob_equals(value1, hash_table_lookup(&hash_table, key1)));
	assert(blob_equals(value2, hash_table_lookup(&hash_table, key2)));
	assert(blob_equals(value3, hash_table_lookup(&hash_table, key3)));

	hash_table_free(&hash_table);
}

static void intensive_work() {
	hash_table_t hash_table = init();

	char string[] = "\0";

	blob_t keys[256];
	blob_t values[256];

	for (size_t i = 0; i < 256; i++) {
		string[0] = (char)i;

		keys[i] = blob_from_string(string);
		values[i] = blob_from_string(string);

		hash_table_insert(&hash_table, keys[i], values[i]);
	}

	assert(hash_table.size == 256);

	for (size_t i = 0; i < 256; i++)
		assert(blob_equals(values[i], hash_table_lookup(&hash_table, keys[i])));

	for (size_t i = 0; i < 256; i++)
		hash_table_delete(&hash_table, keys[i]);

	assert(hash_table.size == 0);

	hash_table_free(&hash_table);
}

void hash_table_tests() {
	empty_cell_is_null();
	find_returns_inserted_value();
	delete_removes_value();
	find_among_many();
	intensive_work();
}
