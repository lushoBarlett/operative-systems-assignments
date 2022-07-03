#include "test_utils.h"

#include <string.h>
#include <stdlib.h>

blob_t blob_from_string(const char* string) {
	size_t bytes = strlen(string) + 1;

	void* memory = malloc(sizeof(char) * bytes);
	strcpy(memory, string);

	return (blob_t){
		.memory = memory,
		.bytes = bytes,
	};
}

bucket_t* new_bucket() {
	bucket_t* bucket = malloc(sizeof(bucket_t));
	memset(bucket, 0, sizeof(bucket_t));
	return bucket;
}
