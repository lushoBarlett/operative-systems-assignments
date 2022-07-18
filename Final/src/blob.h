#pragma once

#include <stddef.h>

typedef struct {
	void* memory;
	size_t bytes;
} blob_t;

blob_t blob_empty();

blob_t blob_create(void* memory, size_t bytes);

int blob_equals(blob_t a, blob_t b);

size_t blob_hash(blob_t blob);

char* blob_to_printable(const blob_t* value);

void blob_free(blob_t blob);
