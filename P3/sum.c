#include <stdio.h>
#include "timing.h"
#include <stdbool.h>
#include <omp.h>

#define ARRAY_SIZE (int)5e8

double array[ARRAY_SIZE];

void init_array() {
	for (size_t i = 0; i < ARRAY_SIZE; i++)
		array[i] = 1;
}

void sumar() {
	double sum = 0;

	#pragma omp parallel for reduction(+: sum)
	for (size_t i = 0; i < ARRAY_SIZE; i++)
		sum += array[i];

	printf("La suma total es %f\n", sum);
}

int main() {
	init_array();

	TIME_void(sumar(),NULL);

	return 0;
}
