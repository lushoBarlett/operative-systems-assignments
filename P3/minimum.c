#include <stdio.h>
#include <omp.h>
#include <limits.h>

#define ARRAY_SIZE (int)5e8

int array[ARRAY_SIZE];

void init_array() {
	for (size_t i = 0; i < ARRAY_SIZE; i++)
		array[i] = i;
}

int main() {
	init_array();

	int minimum = INT_MAX;

	#pragma omp parallel for reduction(min: minimum)
	for (size_t i = 0; i < ARRAY_SIZE; i++)
	 	if (minimum > array[i])
		 	minimum = array[i];

	printf("El mínimo es %d\n", minimum);

	return 0;
}
