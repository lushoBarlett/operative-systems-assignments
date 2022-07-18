#include "blob.h"
#include <ctype.h>
#include <stdlib.h>
#include <string.h>

#define BLOB_HASH_PRIME 31

blob_t blob_empty() {
	return (blob_t){ NULL, 0 };
}

blob_t blob_create(void* memory, size_t bytes) {
	blob_t blob;
	blob.memory = memory;
	blob.bytes = bytes;
	return blob;
}

int blob_equals(blob_t a, blob_t b) {
	if (a.bytes != b.bytes)
		return 0;

	return memcmp(a.memory, b.memory, a.bytes) == 0;
}

size_t blob_hash(blob_t blob) {
	size_t hash = 0;

	const char* p = blob.memory;
	
	for (size_t byte = 0; byte < blob.bytes; byte++)
		hash = hash * BLOB_HASH_PRIME + *p++;
	
	return hash;
}

char* blob_to_printable(const blob_t* value) {
	char* memory = (char*) value->memory;
	for (int i = 0; i < value->bytes; i++) {
		if (!isprint(memory[i]))
			memory[i] = '.';
	}
	return value->memory;
}

void blob_free(blob_t blob) {
	free(blob.memory);
}
