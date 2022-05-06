#pragma once

#include <stdlib.h>

struct queue_t {
	size_t size;
	struct node_t* end;
	struct node_t* begin;
};

typedef struct queue_t queue_t;

void queue_init(queue_t* queue);

void enqueue(queue_t* queue, void* value);

void* dequeue(queue_t* queue);