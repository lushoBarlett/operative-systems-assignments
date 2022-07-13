#include "hash_table.h"

#include <stdlib.h>

#define LOAD_FACTOR 0.75

cell_t* hash_table_cells(size_t capacity) {
	return malloc(sizeof(cell_t) * capacity);
}

void hash_table_init(hash_table_t* hash_table, cell_t* cells, size_t capacity) {
	hash_table->cells = cells;
	counter_init(&hash_table->size, 0);
	hash_table->capacity = capacity;

	for (size_t i = 0; i < hash_table->capacity; i++)
		cell_init(&hash_table->cells[i]);
}

void hash_table_lock(hash_table_t* hash_table) {
	for (size_t i = 0; i < hash_table->capacity; i++)
		cell_lock(&hash_table->cells[i]);
}

void hash_table_unlock(hash_table_t* hash_table) {
	for (size_t i = 0; i < hash_table->capacity; i++)
		cell_unlock(&hash_table->cells[i]);
}

static size_t hash(hash_table_t* hash_table, blob_t key) {
	return blob_hash(key) % hash_table->capacity;
}

static cell_t* hashed_cell(hash_table_t* hash_table, blob_t key) {
	size_t cell_index = blob_hash(key) % hash_table->capacity;

	return &hash_table->cells[cell_index];
}

int hash_table_should_expand(hash_table_t* hash_table) {
	return counter_get(&hash_table->size) / (double) hash_table->capacity > LOAD_FACTOR;
}

// TODO: write prefering rwlock? seems legit useful here

void hash_table_move(hash_table_t* hash_table, hash_table_t* new_hash_table) {
	for (size_t i = 0; i < hash_table->capacity; i++)
		for (bucket_t* bucket = hash_table->cells[i].bucket; bucket; bucket = bucket->next_value)
			hash_table_insert(new_hash_table, bucket);
}

void hash_table_insert(hash_table_t* hash_table, bucket_t* bucket) {
	size_t cell_index = hash(hash_table, bucket->key);

	bucket->cell_index = cell_index;

	cell_t* cell = &hash_table->cells[cell_index];

	cell_lock(cell);

	cell_insert(cell, bucket);

	cell_unlock(cell);

	counter_increment(&hash_table->size);
}

bucket_t* hash_table_lookup(hash_table_t* hash_table, blob_t key) {
	cell_t* cell = hashed_cell(hash_table, key);

	cell_lock(cell);

	bucket_t* bucket = cell_find(cell, key);

	cell_unlock(cell);

	return bucket;
}

void hash_table_delete_bucket(hash_table_t* hash_table, bucket_t* bucket) {
	counter_decrement(&hash_table->size);

	cell_t* cell = &hash_table->cells[bucket->cell_index];

	cell_lock(cell);

	cell_delete_bucket(cell, bucket);

	cell_unlock(cell);
}

bucket_t* hash_table_delete(hash_table_t* hash_table, blob_t key) {
	cell_t* cell = hashed_cell(hash_table, key);

	cell_lock(cell);

	bucket_t* bucket = cell_find(cell, key);

	if (bucket) {
		counter_decrement(&hash_table->size);

		cell_delete_bucket(cell, bucket);
	}

	cell_unlock(cell);

	return bucket;
}
