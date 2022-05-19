#include <stdio.h>
#include <stdbool.h>
#include <omp.h>

#define ARRAY_SIZE (int)5e8

double array[ARRAY_SIZE];

void init_array() {
	for (size_t i = 0; i < ARRAY_SIZE; i++)
		array[i] = 1;
}

int main() {
	init_array();

	double sum;

	#pragma omp parallel for reduction(+: sum)
	for (size_t i = 0; i < ARRAY_SIZE; i++)
		sum += array[i];

	printf("La suma total es %f\n", sum);

	return 0;
}
