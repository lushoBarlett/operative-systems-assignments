#include "cell.h"

#include <stdlib.h>

void cell_init(cell_t* cell) {
	cell->bucket = NULL;
	pthread_mutex_init(&cell->lock, NULL);
}

static bucket_t* fetch_bucket(cell_t* cell, blob_t key) {
	for (bucket_t* bucket = cell->bucket; bucket; bucket = bucket->next_value)
		if (blob_equals(key, bucket->key))
			return bucket;

	return NULL;
}

static void delete_bucket(cell_t* cell, bucket_t* bucket) {
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

void cell_insert(cell_t* cell, bucket_t* bucket) {
	cell_delete(cell, bucket->key);

	if (cell->bucket)
		cell->bucket->prev_value = bucket;

	bucket->next_value = cell->bucket;

	cell->bucket = bucket;
}

const bucket_t* cell_find(cell_t* cell, blob_t key) {
	return fetch_bucket(cell, key);
}

void cell_delete(cell_t* cell, blob_t key) {
	bucket_t* bucket = fetch_bucket(cell, key);

	if (bucket)
		delete_bucket(cell, bucket);
}

void cell_free(cell_t* cell) {
	for (bucket_t* bucket = cell->bucket; bucket; ) {
		bucket_t* next = bucket->next_value;

		bucket_free(bucket);
		free(bucket);

		bucket = next;
	}
}
