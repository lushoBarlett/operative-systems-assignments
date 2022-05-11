#pragma once

#include "concurrent_queue.h"

#include <pthread.h>
#include <semaphore.h>
#include <unistd.h>

#define M 5
#define N 5
#define ARRLEN 10240

int arr[ARRLEN], reading = 0;
concurrent_queue_t cqueue;
sem_t writer_out;
pthread_mutex_t reading_lk;
pthread_cond_t last_reader_leaves;

enum action_type {
	action_read,
	action_write,
};

struct action_t {
	enum action_type type;
	sem_t* wake_up;
};

typedef struct action_t action_t;

void action_init(action_t* action, enum action_type type) {
	action->type = type;
	action->wake_up = malloc(sizeof(*action->wake_up));
	sem_init(action->wake_up, 0, 0);
}

action_t* new_write_action() {
	action_t* action = malloc(sizeof(*action));
	action_init(action, action_write);
	return action;
}

action_t* new_read_action() {
	action_t* action = malloc(sizeof(*action));
	action_init(action, action_read);
	return action;
}

void action_free(action_t* action) {
	free(action->wake_up);
	free(action);
}

void wait_for_readers_to_leave() {
	pthread_mutex_lock(&reading_lk);
	while (reading > 0)
		pthread_cond_wait(&last_reader_leaves, &reading_lk);
	pthread_mutex_unlock(&reading_lk);
}

void* queue_master(void* arg) {
	while(1) {
		action_t* next = cdequeue(&cqueue);

		if (next->type == action_write) {
			sem_wait(&writer_out);
			wait_for_readers_to_leave();
			sem_post(next->wake_up);
		} else {
			sem_wait(&writer_out);
			sem_post(next->wake_up);
		}
	}
}

action_t* write_lock() {
	action_t* write_action = new_write_action();
	cenqueue(&cqueue, write_action);
	sem_wait(write_action->wake_up);
	return write_action;
}

void write_unlock(action_t *write_action) {
	action_free(write_action);
	sem_post(&writer_out);
}

void add_reader() {
	pthread_mutex_lock(&reading_lk);
	reading++;
	pthread_mutex_unlock(&reading_lk);
}

void leave() {
	pthread_mutex_lock(&reading_lk);
	if (--reading == 0)
		pthread_cond_signal(&last_reader_leaves);
	pthread_mutex_unlock(&reading_lk);
}

action_t* read_lock() {
	action_t* read_action = new_read_action();
	cenqueue(&cqueue, read_action);
	sem_wait(read_action->wake_up);
	add_reader();
	sem_post(&writer_out);
	return read_action;
}

void read_unlock(action_t* read_action) {
	action_free(read_action);
	leave();
}

void read_write_lock_init() {
	pthread_t master;
	concurrent_queue_init(&cqueue);
	pthread_create(&master, NULL, queue_master, NULL);
	sem_init(&writer_out, 0, 1);
}