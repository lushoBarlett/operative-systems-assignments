#pragma once

#include <pthread.h>

struct sem_t {
	int counter;

	pthread_mutex_t counter_lock;

	pthread_cond_t counted;
};

typedef struct sem_t sem_t;

void sem_init(sem_t* sem, unsigned int counter);

void sem_wait(sem_t* sem);

void sem_post(sem_t* sem);