#pragma once

#include <stdint.h>
#include <stddef.h>

#define BLOB_HASH_PRIME 31

typedef struct blob_t {
	void* memory;
	uint64_t bytes;
} blob_t;

int blob_equals(blob_t* a, blob_t* b) {
	if (a->bytes != b->bytes)
		return 0;
	
	uint8_t* pa = a->memory;
	uint8_t* pb = b->memory;
	uint64_t byte = 0;

	while(byte++ < a->bytes && *pa++ == *pb++);

	return byte == a->bytes;
}

uint64_t blob_hash(blob_t* blob) {
	uint64_t hash = 0;

	uint8_t* p = blob->memory;
	
	for (uint64_t byte = 0; byte < blob->bytes; byte++)
		hash = hash * BLOB_HASH_PRIME + *p++;
	
	return hash;
}

void blob_free(blob_t* blob) {
	free(blob->memory);
}
