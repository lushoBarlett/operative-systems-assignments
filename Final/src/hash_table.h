#pragma once

#include "cell.h"

/*
 * Un array de celdas es una tabla hash,
 * que en nuestro caso es el banco de datos en sí.
 * Se va redimensionando de acuerdo a
 * nuestras necesidades de almacenamiento.
 */
typedef struct {
	counter64_t size;
	size_t capacity;
	cell_t* cells;
} hash_table_t;

#define INITIAL_CAPACITY 37

cell_t* hash_table_cells(size_t capacity);

void hash_table_init(hash_table_t* hash_table, cell_t* cells, size_t capacity);

void hash_table_lock(hash_table_t* hash_table);

void hash_table_unlock(hash_table_t* hash_table);

int hash_table_should_expand(hash_table_t* hash_table);

void hash_table_move(hash_table_t* hash_table, hash_table_t* new_hash_table);

void hash_table_insert(hash_table_t* hash_table, bucket_t* bucket);

bucket_t* hash_table_lookup(hash_table_t* hash_table, blob_t key);

void hash_table_delete_bucket(hash_table_t* hash_table, bucket_t* bucket);

bucket_t* hash_table_delete(hash_table_t* hash_table, blob_t key);
