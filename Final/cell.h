#include "bucket.h"

void cell_init(cell_t* cell) {
	cell->bucket = NULL;
	pthread_mutex_init(&cell->lock, NULL);
}

void cell_free(cell_t* cell) {
	bucket_t* p = cell->bucket;

	while (p) {
		bucket_t* next = p->next_value;
		bucket_free(p);
		p = next;
	}
}

bucket_t* cell_insert(cell_t* cell, bucket_t* bucket) {
	if (cell->bucket)
		cell->bucket->prev_value = bucket;
	bucket->next_value = cell->bucket;
	cell->bucket = bucket;
}

bucket_t* cell_find(cell_t* cell, blob_t* key) {
	for (bucket_t* bucket = cell->bucket; bucket; bucket = bucket->next_value)
		if (blob_equals(key, &bucket->key))
			return bucket;

	return NULL;
}

void cell_delete(cell_t* cell, blob_t* key) {
	bucket_t* bucket = cell_find(cell, key);

	bucket_t* next = bucket->next_value;
	bucket_t* prev = bucket->prev_value;

	if (next)
		next->prev_value = prev;

	if (prev)
		prev->prev_value = next;

	bucket_free(bucket);

	return NULL;
}