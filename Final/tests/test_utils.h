#pragma once

#include "../src/blob.h"
#include "../src/database.h"

#include <stdio.h>

#define TEST_SUITE(NAME) \
printf("=== Tests for module: " #NAME " ===\n")

#define TEST(EXPRESSION) \
printf("|- Testing > " #EXPRESSION "\n"); \
EXPRESSION

blob_t blob_from_string(const char* string);

bucket_t* new_bucket();

bucket_t* bucket_from_strings(const char* key, const char* value);

#define PTHREAD_CALLER(FUNCTION) \
__pthread_api_##FUNCTION

#define PTHREAD_API(FUNCTION, ARG_T) \
static void* PTHREAD_CALLER(FUNCTION)(void* arg) { \
	ARG_T* casted = (ARG_T*)arg; \
	FUNCTION(*casted); \
	return (void*)0; \
}

pthread_t* create_threads(size_t amount);

void spawn_thread(pthread_t* thread, void* procedure(void*), void* argument);

void spawn_threads(pthread_t* threads, size_t amount, void* procedure(void*), void* argument);

void join_threads(pthread_t* threads, size_t amount);
