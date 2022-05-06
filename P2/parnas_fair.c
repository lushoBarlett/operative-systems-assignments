#include "concurrent_queue.h"

#include <pthread.h>
#include <semaphore.h>
#include <unistd.h>
#include <stdio.h>

#define M 5
#define N 5
#define ARRLEN 10240

int arr[ARRLEN];
concurrent_queue_t cqueue;
sem_t writer_out;

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

void* queue_master(void* arg) {
	while(1) {
		action_t* next = cdequeue(&cqueue);

		if (next->type == action_write) {
			sem_wait(&writer_out);
			// TODO: wait for all readers out
			sem_post(next->wake_up);
		} else {
			sem_wait(&writer_out);
			sem_post(next->wake_up);
			sem_post(&writer_out);
		}
	}
}

void* escritor(void* arg) {
	int num = arg - (void*)0;

	while (1) {
		sleep(random() % 3);

		action_t* write_action = new_write_action();
		cenqueue(&cqueue, write_action);
		sem_wait(write_action->wake_up);

		printf("Escritor %d escribiendo\n", num);

		for (int i = 0; i < ARRLEN; i++)
			arr[i] = num;

		action_free(write_action);
		sem_post(&writer_out);
	}

	return NULL;
}

void* lector(void *arg) {
	int num = arg - (void*)0;

	while (1) {
		sleep(random() % 3);

		action_t* read_action = new_read_action();
		cenqueue(&cqueue, read_action);
		sem_wait(read_action->wake_up);

		int v = arr[0];
		int i;
		for (i = 1; i < ARRLEN; i++)
			if (arr[i] != v)
				break;

		if (i < ARRLEN)
			printf("Lector %d, error de lectura\n", num);
		else
			printf("Lector %d, dato %d\n", num, v);

		action_free(read_action);
	}

	return NULL;
}

int main() {
	pthread_t master;
	pthread_t lectores[M], escritores[N];

	concurrent_queue_init(&cqueue);

	pthread_create(&master, NULL, queue_master, NULL);
	
	sem_init(&writer_out, 0, 1);

	for (int i = 0; i < M; i++)
		pthread_create(&lectores[i], NULL, lector, i + (void*)0);

	for (int i = 0; i < N; i++)
		pthread_create(&escritores[i], NULL, escritor, i + (void*)0);

	pthread_join(lectores[0], NULL);

	return 0;
}
