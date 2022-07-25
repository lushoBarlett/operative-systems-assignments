#include "database.h"

#include <stdlib.h>

static void free_one_bucket(database_t* database) {
	lru_queue_lock(&database->lru_queue);

	bucket_t* bucket = lru_queue_dequeue(&database->lru_queue);

	lru_queue_unlock(&database->lru_queue);

	// TODO: handle NULL?

	cell_t* cell = &database->cells[bucket->cell_index];

	cell_lock(cell);

	/*
	 * Revisamos que no nos hayan quitado de la tabla hash.
	 * Esto es importante ya que si bien cell delete
	 * no tiene efecto si ya nos borraron, sigue
	 * restando una referencia.
	 */
	if (cell_in_list(cell, bucket))
		cell_delete_bucket(cell, bucket);

	cell_unlock(cell);

	bucket_dereference(bucket);

	counter_decrement(&database->size);
}

static void cells_init(database_t* database) {
	for (size_t i = 0; i < CAPACITY; i++)
		cell_init(&database->cells[i]);
}

void database_init(database_t* database) {
	database->cells = malloc(sizeof(cell_t) * CAPACITY);

	counter_init(&database->size, 0);

	cells_init(database);

	lru_queue_init(&database->lru_queue);

	record_init(&database->record);
}

static size_t hash(blob_t key) {
	return blob_hash(key) % CAPACITY;
}

static cell_t* hashed_cell(database_t* database, blob_t key) {
	size_t cell_index = blob_hash(key) % CAPACITY;

	return &database->cells[cell_index];
}

static bucket_t* insert(database_t* database, bucket_t* bucket) {
	size_t cell_index = hash(bucket->key);

	bucket->cell_index = cell_index;

	cell_t* cell = &database->cells[cell_index];

	cell_lock(cell);

	bucket_t* old_bucket = cell_insert(cell, bucket);

	cell_unlock(cell);

	return old_bucket;
}

static void put(database_t* database, bucket_t* bucket) {
	/*
	 * Hacemos el enqueue antes del insert,
	 * por el caso en donde otro insert, de la misma clave,
	 * nos quite de la tabla hash antes de que
	 * nosotros podamos haber entrado en la LRU
	 */
	lru_queue_lock(&database->lru_queue);

	lru_queue_enqueue(&database->lru_queue, SHARE(bucket));

	lru_queue_unlock(&database->lru_queue);

	bucket_t* old_bucket = insert(database, bucket);

	if (old_bucket) {
		lru_queue_lock(&database->lru_queue);

		/*
		 * Revisamos que no nos hayan quitado de la LRU.
		 * Esto es importante ya que si bien queue delete
		 * no tiene efecto si ya nos borraron, sigue
		 * restando una referencia.
		 */
		if (lru_in_queue(&database->lru_queue, old_bucket))
			lru_queue_delete(&database->lru_queue, old_bucket);

		lru_queue_unlock(&database->lru_queue);

		bucket_dereference(old_bucket);
	} else {
		counter_increment(&database->size);
	}
}

void database_put(database_t* database, bucket_t* bucket) {
	put(database, bucket);

	counter_increment(&database->record.puts);
}

static bucket_t* get(database_t* database, blob_t key) {
	cell_t* cell = hashed_cell(database, key);

	cell_lock(cell);

	bucket_t* bucket = cell_find(cell, key);

	cell_unlock(cell);

	if (bucket) {
		lru_queue_lock(&database->lru_queue);

		/*
		 * Revisamos que no nos hayan quitado de la LRU.
		 * Esto es importante si después de que hicimos find
		 * alguien más nos hizo un delete o un take, o trataron
		 * de liberar memoria
		 */
		if (lru_in_queue(&database->lru_queue, bucket))
			lru_queue_reenqueue(&database->lru_queue, bucket);

		lru_queue_unlock(&database->lru_queue);
	}

	return bucket;
}

bucket_t* database_get(database_t* database, blob_t key) {
	bucket_t* bucket = get(database, key);

	counter_increment(&database->record.gets);

	return bucket;
}

static bucket_t* take(database_t* database, blob_t key) {
	cell_t* cell = hashed_cell(database, key);

	cell_lock(cell);

	bucket_t* bucket = cell_delete(cell, key);

	cell_unlock(cell);

	if (bucket) {
		lru_queue_lock(&database->lru_queue);

		/*
		 * Revisamos que no nos hayan quitado de la LRU.
		 * Esto es importante ya que si bien queue delete
		 * no tiene efecto si ya nos borraron, sigue
		 * restando una referencia.
		 */
		if (lru_in_queue(&database->lru_queue, bucket))
			lru_queue_delete(&database->lru_queue, bucket);

		lru_queue_unlock(&database->lru_queue);

		counter_decrement(&database->size);
	}

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

	if (bucket)
		bucket_dereference(bucket);
}

record_t database_stats(database_t* database) {
	return report(&database->record);
}

void* database_memsafe_malloc(database_t* database, size_t bytes) {
	void* memory;

	while ((memory = malloc(bytes)) == NULL)
		free_one_bucket(database);
	
	return memory;
}

void database_destroy(database_t* database) {
	while (database->lru_queue.front)
		free_one_bucket(database);

	free(database->cells);
}
