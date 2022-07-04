#pragma once

#include <pthread.h>

#include "bucket.h"

/*
 * Una celda de la tabla hash representa
 * la cabeza de una lista enlazada
 * de datos. Si la celda está vacía
 * contendrá puntero a NULL.
 */
typedef struct {
	bucket_t* bucket;
	pthread_mutex_t lock;
} cell_t;

void cell_init(cell_t* cell);

void cell_insert(cell_t* cell, bucket_t* bucket);

const bucket_t* cell_find(cell_t* cell, blob_t key);

void cell_delete(cell_t* cell, blob_t key);

void cell_free(cell_t* cell);
