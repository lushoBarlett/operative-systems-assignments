#pragma once

#include <stdint.h>

typedef struct blob_t {
	void* memory;
	uint64_t bytes;
} blob_t;

int blob_equals(blob_t* a, blob_t* b);

uint64_t blob_hash(blob_t* blob);

void blob_free(blob_t* blob);
