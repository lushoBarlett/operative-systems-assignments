#include "cell_tests.h"

#include <assert.h>

#include "../src/cell.h"
#include "test_utils.h"

static cell_t init() {
	cell_t cell;

	cell_init(&cell);

	return cell;
}

static void empty_cell_is_null() {
	cell_t cell = init();

	blob_t key;

	assert(cell_find(&cell, &key) == NULL);

	cell_free(&cell);
}

static void find_returns_inserted_bucket() {
	cell_t cell = init();

	blob_t key = blob_from_string("key");
	blob_t value = blob_from_string("value");

	bucket_t* bucket = bucket_create(&key, &value);

	cell_insert(&cell, bucket);

	const bucket_t* found = cell_find(&cell, &key);

	assert(found == bucket);

	cell_free(&cell);
}

static void delete_removes_bucket() {
	cell_t cell = init();

	blob_t key = blob_from_string("key");
	blob_t value = blob_from_string("value");

	bucket_t* bucket = bucket_create(&key, &value);

	cell_insert(&cell, bucket);

	cell_delete(&cell, &key);

	const bucket_t* found = cell_find(&cell, &key);

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

	bucket_t* bucket1 = bucket_create(&key1, &value1);
	bucket_t* bucket2 = bucket_create(&key2, &value2);
	bucket_t* bucket3 = bucket_create(&key3, &value3);

	cell_insert(&cell, bucket1);
	cell_insert(&cell, bucket2);
	cell_insert(&cell, bucket3);

	assert(bucket1 == cell_find(&cell, &key1));
	assert(bucket2 == cell_find(&cell, &key2));
	assert(bucket3 == cell_find(&cell, &key3));

	cell_free(&cell);
}

void cell_tests() {
	empty_cell_is_null();
	find_returns_inserted_bucket();
	delete_removes_bucket();
	find_among_many();
}
