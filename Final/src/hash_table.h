#pragma once

#include "cell.h"

/*
 * Un array de celdas es una tabla hash,
 * que en nuestro caso es el banco de datos en sí.
 * Se va redimensionando de acuerdo a
 * nuestras necesidades de almacenamiento.
 */
typedef struct {
	size_t size;
	size_t capacity;
	cell_t* cells;
} hash_table_t;

void hash_table_init(hash_table_t* hash_table);

bucket_t* hash_table_insert(hash_table_t* hash_table, blob_t* key, blob_t* value);

const blob_t* hash_table_lookup(hash_table_t* hash_table, const blob_t* key);

void hash_table_delete(hash_table_t* hash_table, const blob_t* key);

void hash_table_free(hash_table_t* hash_table);
