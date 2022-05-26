#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include "timing.h"

#define ARRAY_SIZE 100000000

int array[ARRAY_SIZE];

/*
 * Hackazo para evitar mallocs.
 * A cada región de array le corresponde una región en aux
 * para copiar ese segmento temporalmente.
 */
int aux[ARRAY_SIZE];

void merge(int* array, int left, int mid, int right) {
	for (int i = left; i <= right; i++)
		aux[i] = array[i];

	int i = left;
	int j = mid + 1;
	int k = left;
	while (i <= mid && j <= right) {
		if (aux[i] <= aux[j])
			array[k++] = aux[i++];
		else
			array[k++] = aux[j++];
	}

	while (i <= mid)
		array[k++] = aux[i++];

	while (j <= right)
		array[k++] = aux[j++];
}

void mergesort_algorithm(int* array, int begin, int end) {
	if (begin >= end)
		return;

	int mid = (begin + end) / 2;

	#pragma omp task if (end - begin > 1000)
	mergesort_algorithm(array, begin, mid);

	mergesort_algorithm(array, mid + 1, end);

	#pragma omp taskwait

	merge(array, begin, mid, end);
}

void mergesort(int* array, int begin, int end) {
	#pragma omp parallel
	#pragma omp single
	mergesort_algorithm(array, begin, end);
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

	TIME_void(mergesort(array, 0, ARRAY_SIZE - 1),NULL);

	assert_is_sorted();

	return 0;
}
