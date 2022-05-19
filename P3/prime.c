#include <stdio.h>
#include <omp.h>
#include <math.h>
#include <stdlib.h>

typedef struct range_t {
	int start;
	int end;
} range_t;

void init_ranges(struct range_t* ranges, size_t threads, size_t beg, size_t end) {
	size_t size = end - beg;

	ranges[0].start = beg;

	for (size_t i = 0; i < threads - 1; i++)
		ranges[i].end = ranges[i + 1].start = ranges[i].start + size / threads;

	ranges[threads - 1].end = end;
}

int is_prime(unsigned int n) {
	unsigned int prime = 1;
	unsigned int limit = sqrt(n);

	range_t* ranges = NULL;

	#pragma omp parallel
	{
		#pragma omp single
		{
			int threads = omp_get_num_threads();
			ranges = malloc(sizeof(*ranges) * threads);
			init_ranges(ranges, threads, 2, limit);
		}

		int id = omp_get_thread_num();

		for (unsigned int i = ranges[id].start; i <= ranges[id].end && prime; i++)
			if (n % i == 0)
				prime = 0;

		#pragma omp barrier
		#pragma omp single
		free(ranges);
	}

	return prime;
}

int main(int argc, const char** argv) {

	if (argc < 2)
		return 1;

	unsigned int input = strtoul(argv[1], NULL, 10);

	if (is_prime(input))
		printf("Es primo\n");
	else
		printf("No es primo\n");

	return 0;
}
