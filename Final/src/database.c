#include "database.h"

#include <stdlib.h>

#define INITIAL_CAPACITY 37

void database_init(database_t* database) {
	database->cells = malloc(sizeof(cell_t) * INITIAL_CAPACITY);

	counter_init(&database->size, 0);
	database->capacity = INITIAL_CAPACITY;

	for (size_t i = 0; i < INITIAL_CAPACITY; i++)
		cell_init(&database->cells[i]);

	lru_queue_init(&database->lru_queue);

	record_init(&database->record);
}

static size_t hash(database_t* database, blob_t key) {
	return blob_hash(key) % database->capacity;
}

static cell_t* hashed_cell(database_t* database, blob_t key) {
	size_t cell_index = blob_hash(key) % database->capacity;

	return &database->cells[cell_index];
}

void database_put(database_t* database, bucket_t* bucket) {
	size_t cell_index = hash(database, bucket->key);

	bucket->cell_index = cell_index;

	cell_t* cell = &database->cells[cell_index];

	cell_lock(cell);

	bucket_t* old = cell_insert(cell, bucket);

	cell_unlock(cell);

	if (old) {
		lru_queue_delete(&database->lru_queue, old);

		bucket_dereference(old);
	} else {
		counter_increment(&database->size);
	}

	lru_queue_enqueue(&database->lru_queue, SHARE(bucket));

	counter_increment(&database->record.puts);
}

bucket_t* database_get(database_t* database, blob_t key) {
	cell_t* cell = hashed_cell(database, key);

	cell_lock(cell);

	bucket_t* bucket = cell_find(cell, key);

	cell_unlock(cell);

	if (bucket)
		lru_queue_reenqueue(&database->lru_queue, bucket);

	counter_increment(&database->record.gets);

	return bucket;
}

bucket_t* database_take(database_t* database, blob_t key) {
	cell_t* cell = hashed_cell(database, key);

	cell_lock(cell);

	bucket_t* bucket = cell_delete(cell, key);

	cell_unlock(cell);

	if (bucket)
		lru_queue_delete(&database->lru_queue, bucket);

	counter_increment(&database->record.takes);

	return bucket;
}

void database_delete(database_t* database, blob_t key) {
	bucket_dereference(database_take(database, key));
}

record_t databases_stats(database_t* database) {
	return report(&database->record);
}

static void free_one_bucket(database_t* database) {
	bucket_t* bucket = lru_queue_dequeue(&database->lru_queue);

	cell_t* cell = hashed_cell(database, bucket->key);

	cell_lock(cell);

	cell_delete_bucket(cell, bucket);

	cell_unlock(cell);

	bucket_dereference(bucket);

	counter_decrement(&database->size);
}

void database_destroy(database_t* database) {
	while (database->lru_queue.front)
		free_one_bucket(database);

	free(database->cells);
}
