#include "blob.h"

#include <stdlib.h>

#define BLOB_HASH_PRIME 31

blob_t blob_empty() {
	return (blob_t){ NULL, 0 };
}

int blob_equals(blob_t a, blob_t b) {
	if (a.bytes != b.bytes)
		return 0;

	const char* pa = a.memory;
	const char* pb = b.memory;
	size_t byte = 0;

	while(byte < a.bytes && *pa++ == *pb++)
		byte++;

	return byte == a.bytes;
}

size_t blob_hash(blob_t blob) {
	size_t hash = 0;

	const char* p = blob.memory;
	
	for (size_t byte = 0; byte < blob.bytes; byte++)
		hash = hash * BLOB_HASH_PRIME + *p++;
	
	return hash;
}

void blob_free(blob_t blob) {
	free(blob.memory);
}
