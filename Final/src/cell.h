#pragma once

#include "bucket.h"

void cell_init(cell_t* cell);

void cell_free(cell_t* cell);

void cell_insert(cell_t* cell, bucket_t* bucket);

const bucket_t* cell_find(cell_t* cell, const blob_t* key);

void cell_delete(cell_t* cell, const blob_t* key);
