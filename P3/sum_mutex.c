#include <stdio.h>
#include <pthread.h>
#include <stdbool.h>
#include <omp.h>

#include "timing.h"

#define ARRAY_SIZE (int)5e8

double array[ARRAY_SIZE];

pthread_mutex_t sum_lock;

void init_array() {
	for (size_t i = 0; i < ARRAY_SIZE; i++)
		array[i] = 1;
}

void sumar() {
	double sum = 0;

	#pragma omp parallel for
	for (size_t i = 0; i < ARRAY_SIZE; i++) {
		pthread_mutex_lock(&sum_lock);
		sum += array[i];
		pthread_mutex_unlock(&sum_lock);
	}

	printf("La suma total es %f\n", sum);
}

int main() {
	init_array();

	pthread_mutex_init(&sum_lock, NULL);

	TIME_void(sumar(),NULL);

	return 0;
}
