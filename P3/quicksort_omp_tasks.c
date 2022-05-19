#include <pthread.h>
#include <stdlib.h>
#include <assert.h>

#define ARRAY_SIZE 1000000

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

void quicksort(int* array, int n) {
	if (n < 2)
		return;

	int m = partition(array, n);

	#pragma omp single nowait
	{
		#pragma omp task
		quicksort(&array[0], m);

		quicksort(&array[m + 1], n - (m + 1));
	}
	#pragma omp taskwait
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

	#pragma omp parallel
	#pragma omp single
	quicksort(array, ARRAY_SIZE);

	assert_is_sorted();

	return 0;
}