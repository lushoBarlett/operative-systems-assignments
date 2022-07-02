#pragma once

#include <stddef.h>

typedef struct blob_t {
	void* memory;
	size_t bytes;
} blob_t;

int blob_equals(blob_t* a, blob_t* b);

size_t blob_hash(blob_t* blob);

void blob_free(blob_t* blob);