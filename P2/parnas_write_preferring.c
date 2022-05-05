#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <pthread.h>

#define M 5
#define N 5
#define ARRLEN 10240

int arr[ARRLEN];
pthread_mutex_t reading_lk, wanna_write_lk, writer;
int wanna_write, reading;
pthread_cond_t last_reader_leaves, no_writers;

void increment_writer_queue() {
	pthread_mutex_lock(&wanna_write_lk);
	wanna_write++;
	pthread_mutex_unlock(&wanna_write_lk);
}

void wait_for_readers_to_leave() {
	pthread_mutex_lock(&reading_lk);
	while (reading > 0)
		pthread_cond_wait(&last_reader_leaves, &reading_lk);
	pthread_mutex_unlock(&reading_lk);
}

void decrement_writer_queue() {
	pthread_mutex_lock(&wanna_write_lk);
	if (--wanna_write == 0)
		pthread_cond_broadcast(&no_writers);
	pthread_mutex_unlock(&wanna_write_lk);
}

void* escritor(void* arg) {
	int num = arg - (void*)0;

	while (1) {
		sleep(random() % 3);
		increment_writer_queue();
		wait_for_readers_to_leave();

		pthread_mutex_lock(&writer);
		printf("Escritor %d escribiendo\n", num);

		for (int i = 0; i < ARRLEN; i++)
			arr[i] = num;

		pthread_mutex_unlock(&writer);

		decrement_writer_queue();
	}

	return NULL;
}

void enter_when_no_writers() {
	pthread_mutex_lock(&wanna_write_lk);
	while (wanna_write > 0)
		pthread_cond_wait(&no_writers, &wanna_write_lk);

	pthread_mutex_lock(&reading_lk);
	reading++;
	pthread_mutex_unlock(&reading_lk);
	pthread_mutex_unlock(&wanna_write_lk);
}

void leave() {
	pthread_mutex_lock(&reading_lk);
	if (--reading == 0)
		pthread_cond_broadcast(&last_reader_leaves);
	pthread_mutex_unlock(&reading_lk);
}

void* lector(void *arg) {
	int num = arg - (void*)0;
	while (1) {
		sleep(random() % 3);
		enter_when_no_writers();

		int v = arr[0];
		int i;
		for (i = 1; i < ARRLEN; i++)
			if (arr[i] != v)
				break;

		if (i < ARRLEN)
			printf("Lector %d, error de lectura\n", num);
		else
			printf("Lector %d, dato %d\n", num, v);

		leave();
	}

	return NULL;
}

int main() {
	pthread_t lectores[M], escritores[N];

	pthread_mutex_init(&reading_lk, NULL);
	pthread_mutex_init(&wanna_write_lk, NULL);
	pthread_mutex_init(&writer, NULL);


	for (int i = 0; i < M; i++)
		pthread_create(&lectores[i], NULL, lector, i + (void*)0);

	for (int i = 0; i < N; i++)
		pthread_create(&escritores[i], NULL, escritor, i + (void*)0);

	pthread_join(lectores[0], NULL);

	return 0;
}
