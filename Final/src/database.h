#pragma once

#include "cell.h"
#include "lru_queue.h"
#include "record.h"

#define CAPACITY 999983

/*
 * TODO: justificar LRU
 *
 * Como parte de la base de datos guardamos
 * los metadatos de sus operaciones y memoria usada.
 */
typedef struct {
	counter64_t size;

	cell_t* cells;

	lru_queue_t lru_queue;
	concurrent_record_t record;
} database_t;

void database_init(database_t* database);

void database_put(database_t* database, bucket_t* bucket);

bucket_t* database_get(database_t* database, blob_t key);

bucket_t* database_take(database_t* database, blob_t key);

void database_delete(database_t* database, blob_t key);

record_t database_stats(database_t* database);

void* database_memsafe_malloc(database_t* database, size_t bytes);

void database_destroy(database_t* database);
