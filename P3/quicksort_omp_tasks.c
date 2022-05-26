#include <pthread.h>
#include <stdlib.h>
#include <assert.h>
#include <omp.h>

#include "timing.h"

#define ARRAY_SIZE 50000000

int array[ARRAY_SIZE];

void swap(int* a, int* b) {
	int aux = *a;
	*a = *b;
	*b = aux;
}

int partition(int* array, int n) {
	int middle = 0;
	int pivot = array[0];

	swap(&array[0], &array[n - 1]);

	for (int i = 0; i < n - 1; i++)
		if (array[i] <= pivot)
			swap(&array[i], &array[middle++]);

	swap(&array[middle], &array[n - 1]);

	return middle;
}

void squicksort_algorithm(int* array, int n) {
	if (n < 2)
		return;

	int m = partition(array, n);

	squicksort_algorithm(&array[0], m);
	squicksort_algorithm(&array[m + 1], n - (m + 1));
}

void quicksort_algorithm(int* array, int n) {
	if (n < 10000) {
		squicksort_algorithm(array, n);
		return;
	}

	int m = partition(array, n);

	#pragma omp task
	quicksort_algorithm(&array[0], m);

	quicksort_algorithm(&array[m + 1], n - (m + 1));
}

void quicksort(int* array, int n) {
	#pragma omp parallel
	#pragma omp single
	quicksort_algorithm(array, n);
}

void init_array() {
	for (int i = 0; i < ARRAY_SIZE; i++)
		array[i] = random();
}

void assert_is_sorted() {
	for (int i = 0; i < ARRAY_SIZE - 1; i++)
		assert(array[i] <= array[i + 1]);
}

int main() {
	init_array();

	TIME_void(quicksort(array, ARRAY_SIZE),NULL);

	assert_is_sorted();

	return 0;
}
