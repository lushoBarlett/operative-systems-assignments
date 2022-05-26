#include <mpi.h>
#include <stdio.h>
#include <stdlib.h>

#include "ranges.h"

#define ARR_LEN 10001

int arr[ARR_LEN];

void init_arr(int *arr) {
	for (int i = 0; i < ARR_LEN; i++)
		arr[i] = i;
}

void distribute(range_t* ranges, int threads) {
	for (int i = 1; i < threads; i++) {            
		int len = size(ranges, i);

		MPI_Send(&len, 1, MPI_INT, i, 0, MPI_COMM_WORLD);

		int offset = beg(ranges, i);

		MPI_Send(arr + offset, len, MPI_INT, i, 0, MPI_COMM_WORLD);
	}
}

int sum_range(int* arr, int len) {
	int sum = 0;

	for (int i = 0; i < len; i++)
		sum += arr[i];

	return sum;
}

void main(int argc, char **argv) {

	MPI_Init(&argc, &argv);

	int id;
	MPI_Comm_rank(MPI_COMM_WORLD, &id);

	if (id == 0) {
		init_arr(arr);

		int threads;
		MPI_Comm_size(MPI_COMM_WORLD, &threads);

		range_t* ranges = make_ranges(threads, 0, ARR_LEN);

		distribute(ranges, threads);

		int partial = sum_range(arr, size(ranges, 0));

		int sum;
		MPI_Reduce(&partial, &sum, 1, MPI_INT, MPI_SUM, 0, MPI_COMM_WORLD);

		printf("sum is: %d\n", sum);

		free(ranges);
	} else {
		MPI_Status status;

		int len;
		MPI_Recv(&len, 1, MPI_INT, 0, 0, MPI_COMM_WORLD, &status);
		MPI_Recv(arr, len, MPI_INT, 0, 0, MPI_COMM_WORLD, &status);

		int partial = sum_range(arr, len);

		MPI_Reduce(&partial, NULL, 1, MPI_INT, MPI_SUM, 0, MPI_COMM_WORLD);
	}

	MPI_Finalize();
}
