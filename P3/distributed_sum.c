#include <mpi.h>
#include <stdio.h>
#include <stdlib.h>

#define ARR_LEN 10001

int arr[ARR_LEN];

void init_arr(int *arr) {
	for (int i = 0; i < ARR_LEN; i++)
		arr[i] = i;
}

typedef struct range_t {
	int start;
	int end;
} range_t;


void init_ranges(struct range_t* ranges, size_t threads, size_t beg, size_t end) {
	size_t size = end - beg;

	ranges[0].start = beg;

	for (size_t i = 0; i < threads-1; i++)
		ranges[i].end = ranges[i + 1].start = ranges[i].start + size / threads;

	ranges[threads - 1].end = end;
}


void main(int argc, char **argv) {
    int id, sum, partial_sum = 0, len, offset;
    
    MPI_Init(&argc, &argv);

    
    MPI_Comm_rank(MPI_COMM_WORLD, &id);


    if (id == 0) { // proceso main

        // Initialize array
        init_arr(arr);

        // Create partitions
        int n_threads;
        MPI_Comm_size(MPI_COMM_WORLD, &n_threads);
        range_t *ranges = malloc(sizeof(*ranges)*(1+n_threads));
        init_ranges(ranges, n_threads, 0, ARR_LEN);

        // Distribute them to other processes
        for (int i = 1; i < n_threads; i++) {            
            len = ranges[i].end - ranges[i].start;
            MPI_Send(&len, 1, MPI_INT, i, 0, MPI_COMM_WORLD);
            offset = ranges[i].start;
            MPI_Send(arr + offset, len, MPI_INT, i, 0, MPI_COMM_WORLD);
        }

        // Sum my part
        len = ranges[0].end;
        for (int i = 0; i < len; i++) {
            partial_sum += arr[i];
        }
	    
        // Sum all the results obtained
        sum = partial_sum;
        MPI_Reduce(&partial_sum, &sum, 1, MPI_INT, MPI_SUM, 0, MPI_COMM_WORLD);

        printf("sum is: %d\n", sum);

        free(ranges);

    } else {

	    MPI_Status status;

        // Receive my part of the array
        MPI_Recv(&len, 1, MPI_INT, 0, 0, MPI_COMM_WORLD, &status);
        MPI_Recv(arr, len, MPI_INT, 0, 0, MPI_COMM_WORLD, &status);
        
        // Sum my part
        for (int i = 0; i < len; i++) {
            partial_sum += arr[i];
        }

        // Send my results
        MPI_Reduce(&partial_sum, &sum, 1, MPI_INT, MPI_SUM, 0, MPI_COMM_WORLD);
        
    }
    
    MPI_Finalize();
}
