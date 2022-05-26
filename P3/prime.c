#include <stdio.h>
#include <omp.h>
#include <math.h>
#include <stdlib.h>

#include "ranges.h"

int is_prime(unsigned int n) {
	unsigned int prime = 1;
	unsigned int limit = sqrt(n);

	range_t* ranges = NULL;

	#pragma omp parallel
	{
		#pragma omp single
		{
			int threads = omp_get_num_threads();
			ranges = make_ranges(threads, 2, limit);
		}

		int id = omp_get_thread_num();

		for (unsigned int i = beg(ranges, id); i <= end(ranges, id) && prime; i++)
			if (n % i == 0)
				prime = 0;

		#pragma omp barrier
		#pragma omp single
		free_ranges(ranges);
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
