#include "blob_tests.h"

#include <assert.h>

#include "../src/blob.h"
#include "test_utils.h"

static int equals_from_strings(const char* first, const char* second) {
	blob_t blob_first = blob_from_string(first);
	blob_t blob_second = blob_from_string(second);

	int result = blob_equals(blob_first, blob_second);

	blob_free(blob_first);
	blob_free(blob_second);

	return result;
}

static size_t hash_from_string(const char* string) {
	blob_t blob = blob_from_string(string);

	size_t hash = blob_hash(blob);

	blob_free(blob);

	return hash;
}

static void empty_equals() {
	blob_t first = blob_empty();
	blob_t second = blob_empty();

	assert(blob_equals(first, second));
}

static void general_equals() {
	assert(equals_from_strings("", ""));
	assert(!equals_from_strings("hello", "world"));
	assert(equals_from_strings("hello", "hello"));
}

/*
 * Hashear blobs vacíos queda sin definir
 */

static void hash() {
	size_t first = hash_from_string("hello");
	size_t second = hash_from_string("hello!");
	size_t third = hash_from_string("hello");

	assert(first != second);
	assert(first == third);
}

void blob_tests() {
	TEST_SUITE(blob);

	TEST(empty_equals());
	TEST(general_equals());
	TEST(hash());
}
