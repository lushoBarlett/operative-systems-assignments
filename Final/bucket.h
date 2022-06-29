#include "database.h"

bucket_t* bucket_create(blob_t* key, blob_t* value) {
	bucket_t* bucket = malloc(sizeof(bucket_t));

	bucket->key = *key;
	bucket->value = *value;

	bucket->cell_index = 0;

	bucket->next_value = bucket->prev_value = NULL;
	bucket->next_queue = bucket->prev_queue = NULL;

	return bucket;
}

void bucket_free(bucket_t* bucket) {
	blob_free(&bucket->key);
	blob_free(&bucket->value);
}
