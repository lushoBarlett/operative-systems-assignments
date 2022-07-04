#pragma once

#include <stddef.h>

typedef struct {
	void* memory;
	size_t bytes;
} blob_t;

blob_t blob_empty();

int blob_equals(blob_t a, blob_t b);

size_t blob_hash(blob_t blob);

void blob_free(blob_t blob);
