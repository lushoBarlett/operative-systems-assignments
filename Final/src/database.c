#include "database.h"

#include <stdlib.h>

#define INITIAL_CAPACITY 37
#define LOAD_FACTOR 0.75

static void cells_init(database_t* database) {
	for (size_t i = 0; i < database->capacity; i++)
		cell_init(&database->cells[i]);
}

void database_init(database_t* database) {
	database->cells = malloc(sizeof(cell_t) * INITIAL_CAPACITY);

	counter_init(&database->size, 0);

	database->capacity = INITIAL_CAPACITY;
	pthread_mutex_init(&database->capacity_lock, NULL);

	cells_init(database);
	readers_writer_lock_init(&database->rw_cells_lock);

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

static int should_expand(database_t* database, size_t new_size) {
	return new_size / (double) database->capacity > LOAD_FACTOR;
}

static bucket_t** buckets_dump(database_t* database, size_t size) {
	bucket_t** buckets = malloc(sizeof(*buckets) * size);
	size_t bucket_index = 0;

	for (size_t i = 0; i < database->capacity; i++) {
		cell_t* cell = &database->cells[i];

		while(cell->bucket) {
			buckets[bucket_index++] = SHARE(cell->bucket);
			cell_delete_bucket(cell, cell->bucket);
		}
	}

	return buckets;
}

static bucket_t* insert(database_t* database, bucket_t* bucket) {
	size_t cell_index = hash(database, bucket->key);

	bucket->cell_index = cell_index;

	cell_t* cell = &database->cells[cell_index];

	cell_lock(cell);

	bucket_t* old_bucket = cell_insert(cell, bucket);

	cell_unlock(cell);

	return old_bucket;
}

static void buckets_reinsert(database_t* database, bucket_t** buckets, size_t size) {
	for (size_t i = 0; i < size; i++)
		insert(database, buckets[i]);
}

static void expand(database_t* database) {	
	write_lock(&database->rw_cells_lock);

	size_t size = counter_get(&database->size);

	bucket_t** buckets = buckets_dump(database, size);

	database->capacity *= 2;

	database->cells = realloc(database->cells, sizeof(cell_t) * database->capacity);

	// TODO: what do with reinit of cell locks?
	cells_init(database);

	buckets_reinsert(database, buckets, size);

	free(buckets);

	write_unlock(&database->rw_cells_lock);
}

static void try_expand(database_t* database, size_t new_size) {
	// TODO: try not to get everyone stuck here until expansion is done
	pthread_mutex_lock(&database->capacity_lock);

	if (should_expand(database, new_size))
		expand(database);

	pthread_mutex_unlock(&database->capacity_lock);
}

static size_t put(database_t* database, bucket_t* bucket) {
	read_lock(&database->rw_cells_lock);

	bucket_t* old_bucket = insert(database, bucket);

	size_t new_size;

	if (old_bucket) {
		lru_queue_delete(&database->lru_queue, old_bucket);

		bucket_dereference(old_bucket);
		
		new_size = counter_get(&database->size);
	} else {
		new_size = counter_increment(&database->size);
	}

	lru_queue_enqueue(&database->lru_queue, SHARE(bucket));

	read_unlock(&database->rw_cells_lock);

	return new_size;
}

void database_put(database_t* database, bucket_t* bucket) {
	size_t new_size = put(database, bucket);

	try_expand(database, new_size);

	counter_increment(&database->record.puts);
}

static bucket_t* get(database_t* database, blob_t key) {
	read_lock(&database->rw_cells_lock);

	cell_t* cell = hashed_cell(database, key);

	cell_lock(cell);

	bucket_t* bucket = cell_find(cell, key);

	cell_unlock(cell);

	if (bucket)
		lru_queue_reenqueue(&database->lru_queue, bucket);

	read_unlock(&database->rw_cells_lock);

	return bucket;
}

bucket_t* database_get(database_t* database, blob_t key) {
	bucket_t* bucket = get(database, key);

	counter_increment(&database->record.gets);

	return bucket;
}

static bucket_t* take(database_t* database, blob_t key) {
	read_lock(&database->rw_cells_lock);

	cell_t* cell = hashed_cell(database, key);

	cell_lock(cell);

	bucket_t* bucket = cell_delete(cell, key);

	cell_unlock(cell);

	if (bucket)
		lru_queue_delete(&database->lru_queue, bucket);

	read_unlock(&database->rw_cells_lock);

	return bucket;
}

bucket_t* database_take(database_t* database, blob_t key) {
	bucket_t* bucket = take(database, key);

	counter_increment(&database->record.takes);

	return bucket;
}

void database_delete(database_t* database, blob_t key) {
	bucket_t* bucket = take(database, key);

	counter_increment(&database->record.dels);

	bucket_dereference(bucket);
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
	write_lock(&database->rw_cells_lock);

	while (database->lru_queue.front)
		free_one_bucket(database);

	free(database->cells);

	write_unlock(&database->rw_cells_lock);
}
