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

bucket_t* bucket_from_strings(const char* key, const char* value) {
	bucket_t* bucket = new_bucket();

	bucket->key = blob_from_string(key);
	bucket->value = blob_from_string(value);

	return bucket;
}

void bucket_try_free(bucket_t* bucket) {
	if (bucket)
		bucket_free(bucket);
}

pthread_t* create_threads(size_t amount) {
	return malloc(sizeof(pthread_t) * amount);
}

void spawn_thread(pthread_t* thread, void* procedure(void*), void* argument) {
	pthread_create(thread, NULL, procedure, argument);
}

void spawn_threads(pthread_t* threads, size_t amount, void* procedure(void*), void* argument) {
	for (size_t i = 0; i < amount; i++)
		pthread_create(&threads[i], NULL, procedure, argument);
}

void join_threads(pthread_t* threads, size_t amount) {
	for (size_t i = 0; i < amount; i++)
		pthread_join(threads[i], NULL);

	free(threads);
}