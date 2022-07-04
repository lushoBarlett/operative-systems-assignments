#include "bucket.h"

#include <stdlib.h>
#include <string.h>

bucket_t* bucket_create(blob_t key, blob_t value) {
	bucket_t* bucket = malloc(sizeof(bucket_t));

	memset(bucket, 0, sizeof(bucket_t));

	bucket->key = key;
	bucket->value = value;

	return bucket;
}

void bucket_free(bucket_t* bucket) {
	blob_free(bucket->key);
	blob_free(bucket->value);
}
