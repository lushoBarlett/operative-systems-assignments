#include "bucket.h"

#include <stdlib.h>
#include <string.h>

bucket_t* bucket_create(blob_t key, blob_t value) {
	bucket_t* bucket = malloc(sizeof(bucket_t));

	bucket_init(bucket, key, value);

	return bucket;
}

void bucket_init(bucket_t* bucket, blob_t key, blob_t value) {
	memset(bucket, 0, sizeof(bucket_t));

	bucket->key = key;
	bucket->value = value;

	counter_init(&bucket->references, 1);
}

void bucket_reference(bucket_t* bucket) {
	counter_increment(&bucket->references);
}

void bucket_dereference(bucket_t* bucket) {
	if (counter_decrement(&bucket->references) == 0) {
		blob_free(bucket->key);
		blob_free(bucket->value);
		free(bucket);
	}
}
