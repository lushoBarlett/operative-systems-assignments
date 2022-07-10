#include "bucket_tests.h"

#include <assert.h>

#include "../src/bucket.h"
#include "test_utils.h"

static void creation_and_destruction() {
	bucket_t* bucket = bucket_create(blob_from_string("key"), blob_from_string("value"));

	bucket_reference(bucket);

	bucket_dereference(bucket);

	bucket_dereference(bucket);
}

void bucket_tests() {
	TEST_SUITE(bucket);

	TEST(creation_and_destruction());
}
