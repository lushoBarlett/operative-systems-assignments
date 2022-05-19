#include <stdio.h>
#include <pthread.h>

#define ARRAY_SIZE (int)5e8

#define THREADS 4

struct range_t {
	int start;
	int end;
};

double sum;

double array[ARRAY_SIZE];

pthread_mutex_t sum_lock;

void init_array() {
	for (size_t i = 0; i < ARRAY_SIZE; i++)
		array[i] = 1;
}

void init_ranges(struct range_t* ranges) {
	ranges[0].start = 0;

	for (size_t i = 0; i < THREADS - 1; i++)
		ranges[i].end = ranges[i + 1].start = ranges[i].start + ARRAY_SIZE / THREADS;

	ranges[THREADS - 1].end = ARRAY_SIZE;
}

void* range_sum(void* arg) {
	struct range_t* range = (struct range_t*)arg;

	for (size_t i = range->start; i < range->end; i++) {
		pthread_mutex_lock(&sum_lock);
		sum += array[i];
		pthread_mutex_unlock(&sum_lock);
	}
}

int main() {
	init_array();

	struct range_t ranges[THREADS];
	init_ranges(ranges);

	pthread_mutex_init(&sum_lock, NULL);

	pthread_t threads[THREADS];
	for (size_t i = 0; i < THREADS; i++)
		pthread_create(&threads[i], NULL, range_sum, (void*)&ranges[i]);

	for (size_t i = 0; i < THREADS; i++)
		pthread_join(threads[i], NULL);

	printf("La suma total es %f\n", sum);

	return 0;
}
