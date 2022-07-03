#include "hash_table.h"

#include <stdlib.h>

#define LOAD_FACTOR 0.75
#define INITIAL_CAPACITY 37

static cell_t* create_cells() {
	return malloc(sizeof(cell_t) * INITIAL_CAPACITY);
}

void hash_table_init(hash_table_t* hash_table) {
	hash_table->cells = create_cells();
	hash_table->size = 0;
	hash_table->capacity = INITIAL_CAPACITY;

	for (size_t i = 0; i < hash_table->capacity; i++)
		cell_init(&hash_table->cells[i]);
}

static bucket_t** collect_buckets(hash_table_t* hash_table) {
	bucket_t** buckets = malloc(sizeof(*buckets) * hash_table->size);
	size_t bucket_index = 0;

	for (size_t i = 0; i < hash_table->capacity; i++)
		for (bucket_t* bucket = hash_table->cells[i].bucket; bucket; bucket = bucket->next_value)
			buckets[bucket_index++] = bucket;

	return buckets;
}

static void hash_table_reinsert(hash_table_t* hash_table, bucket_t* bucket) {
	size_t cell_index = blob_hash(&bucket->key) % hash_table->capacity;

	cell_insert(&hash_table->cells[cell_index], bucket);

	bucket->cell_index = cell_index;
}

static void hash_table_expand(hash_table_t* hash_table) {
	bucket_t** buckets = collect_buckets(hash_table);

	hash_table->capacity *= 2;

	hash_table->cells = realloc(hash_table->cells, sizeof(cell_t) * hash_table->capacity);

	for (size_t i = 0; i < hash_table->capacity; i++)
		cell_init(&hash_table->cells[i]);

	for (size_t i = 0; i < hash_table->size; i++)
		hash_table_reinsert(hash_table, buckets[i]);

	free(buckets);
}

bucket_t* hash_table_insert(hash_table_t* hash_table, blob_t* key, blob_t* value) {
	size_t cell_index = blob_hash(key) % hash_table->capacity;

	bucket_t* new_bucket = bucket_create(key, value);

	cell_insert(&hash_table->cells[cell_index], new_bucket);

	new_bucket->cell_index = cell_index;

	hash_table->size++;

	if (hash_table->size / (double) hash_table->capacity > LOAD_FACTOR)
		hash_table_expand(hash_table);

	return new_bucket;
}

const blob_t* hash_table_lookup(hash_table_t* hash_table, const blob_t* key) {
	size_t cell_index = blob_hash(key) % hash_table->capacity;

	const bucket_t* bucket = cell_find(&hash_table->cells[cell_index], key);

	if (!bucket)
		return NULL;

	return &bucket->value;
}

void hash_table_delete(hash_table_t* hash_table, const blob_t* key) {
	size_t cell_index = blob_hash(key) % hash_table->capacity;

	// TODO: optimize
	if (!cell_find(&hash_table->cells[cell_index], key))
		return;

	hash_table->size--;

	cell_delete(&hash_table->cells[cell_index], key);
}

void hash_table_free(hash_table_t* hash_table) {
	for (size_t i = 0; i < hash_table->capacity; i++)
		cell_free(&hash_table->cells[i]);

	free(hash_table->cells);
}
