#include "cell.h"

void cell_init(cell_t* cell) {
	cell->bucket = NULL;
	pthread_mutex_init(&cell->lock, NULL);
}

void cell_lock(cell_t* cell) {
	pthread_mutex_lock(&cell->lock);
}

void cell_unlock(cell_t* cell) {
	pthread_mutex_unlock(&cell->lock);
}

bucket_t* cell_find(cell_t* cell, blob_t key) {
	for (bucket_t* bucket = cell->bucket; bucket; bucket = bucket->next_value)
		if (blob_equals(key, bucket->key))
			return bucket;

	return NULL;
}

void cell_delete_bucket(cell_t* cell, bucket_t* bucket) {
	bucket_t* next = bucket->next_value;
	bucket_t* prev = bucket->prev_value;

	if (next)
		next->prev_value = prev;

	if (prev)
		prev->next_value = next;

	bucket->next_value = bucket->prev_value = NULL;

	if (cell->bucket == bucket)
		cell->bucket = next;
}

bucket_t* cell_delete(cell_t* cell, blob_t key) {
	bucket_t* bucket = cell_find(cell, key);

	if (bucket)
		cell_delete_bucket(cell, bucket);

	return bucket;
}

bucket_t* cell_insert(cell_t* cell, bucket_t* bucket) {
	bucket_t* old = cell_delete(cell, bucket->key);

	if (cell->bucket)
		cell->bucket->prev_value = bucket;

	bucket->next_value = cell->bucket;

	cell->bucket = bucket;

	return old;
}
