#pragma once

#include "lru_queue.h"
#include "record.h"

/*
 * TODO: justificar LRU
 *
 * Como parte de la base de datos guardamos
 * los metadatos de sus operaciones y memoria usada.
 */
typedef struct {
	lru_queue_t lru_queue;
	concurrent_record_t record;
} database_t;

database_t db_create();

void db_put(database_t* database, blob_t key, blob_t value);

bucket_t* db_get(database_t* database, blob_t key);

bucket_t* db_take(database_t* database, blob_t key);

void db_delete(database_t* database, blob_t key);

record_t db_stats(database_t* database);

void db_destroy(database_t* database);
