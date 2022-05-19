#include <stdio.h>
#include <omp.h>
#include <math.h>
#include <stdlib.h>

int is_prime(unsigned int n) {
	unsigned int divisors = 0;
	unsigned int limit = sqrt(n);

	#pragma omp parallel for reduction(+: divisors)
	for (unsigned int i = 2; i <= limit; i++)
		divisors += n % i == 0;

	return divisors == 0;
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

