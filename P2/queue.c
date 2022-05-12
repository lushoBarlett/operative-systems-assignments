#include "queue.h"

#include <assert.h>

struct node_t {
	void* value;
	struct node_t* prev;
	struct node_t* next;
};

static struct node_t* node(void* value, struct node_t* prev, struct node_t* next) {
	struct node_t* new_node = (struct node_t*)malloc(sizeof(*new_node));
	new_node->value = value;
	new_node->prev = prev;
	new_node->next = next;
	return new_node;
}

void queue_init(queue_t* queue) {
	queue->size = 0;
	queue->begin = queue->end = NULL;
}

void enqueue(queue_t* queue, void* value) {
	struct node_t* target = node(value, NULL, queue->begin);
	
	if (queue->begin)
		queue->begin->prev = target;

	queue->begin = target;

	queue->size++;

	if (queue->size == 1)
		queue->end = queue->begin;
}

void* dequeue(queue_t* queue) {
	struct node_t* target = queue->end;

	assert(queue->end);
	queue->end = queue->end->prev;
	queue->size--;

	if (queue->end)
		queue->end->next = NULL;
	else
		queue->begin = NULL;

	assert(target);
	void* value = target->value;
	free(target);

	return value;
}

void* front(queue_t* queue) {
	return queue->end;
}