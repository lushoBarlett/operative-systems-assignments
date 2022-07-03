#include "cell.h"

#include <stdlib.h>

void cell_init(cell_t* cell) {
	cell->bucket = NULL;
	pthread_mutex_init(&cell->lock, NULL);
}

void cell_free(cell_t* cell) {
	for (bucket_t* bucket = cell->bucket; bucket; ) {
		bucket_t* next = bucket->next_value;

		bucket_free(bucket);
		free(bucket);

		bucket = next;
	}
}

void cell_insert(cell_t* cell, bucket_t* bucket) {
	if (cell->bucket)
		cell->bucket->prev_value = bucket;

	bucket->next_value = cell->bucket;

	cell->bucket = bucket;
}

static bucket_t* fetch_bucket(cell_t* cell, const blob_t* key) {
	for (bucket_t* bucket = cell->bucket; bucket; bucket = bucket->next_value)
		if (blob_equals(key, &bucket->key))
			return bucket;

	return NULL;
}

const bucket_t* cell_find(cell_t* cell, const blob_t* key) {
	return fetch_bucket(cell, key);
}

void cell_delete(cell_t* cell, const blob_t* key) {
	bucket_t* bucket = fetch_bucket(cell, key);

	if (!bucket)
		return;

	bucket_t* next = bucket->next_value;
	bucket_t* prev = bucket->prev_value;

	if (next)
		next->prev_value = prev;

	if (prev)
		prev->next_value = next;

	if (cell->bucket == bucket)
		cell->bucket = NULL;

	bucket_free(bucket);
	free(bucket);
}
