#pragma once

#include "cell.h"
#include "lru_queue.h"
#include "record.h"
#include "rw_lock.h"

/*
 * TODO: justificar LRU
 *
 * Como parte de la base de datos guardamos
 * los metadatos de sus operaciones y memoria usada.
 */
typedef struct {
	counter64_t size;

	size_t capacity;
	pthread_mutex_t capacity_lock;

	cell_t* cells;
	readers_writer_lock_t rw_cells_lock;

	lru_queue_t lru_queue;
	concurrent_record_t record;
} database_t;

void database_init(database_t* database);

void database_put(database_t* database, bucket_t* bucket);

bucket_t* database_get(database_t* database, blob_t key);

bucket_t* database_take(database_t* database, blob_t key);

void database_delete(database_t* database, blob_t key);

record_t database_stats(database_t* database);

void database_destroy(database_t* database);
