#include <mpi.h>
#include <stdio.h>
#include <math.h>

void main(int argc, char **argv) {
    MPI_Init(&argc, &argv);

    int n_processes, id, step, priv_value, rcv_value, iters, partial_sum, id_to_comm;

    MPI_Status status;
    MPI_Comm_rank(MPI_COMM_WORLD, &id);
    MPI_Comm_size(MPI_COMM_WORLD, &n_processes);

    iters = log(n_processes)/log(2);
    priv_value = id;
    partial_sum = priv_value;

    for (int i = 0; i < iters; i++) {
        step = pow(2,i);

        if ((id/step) % 2 == 0) {
	    id_to_comm = id + step;
        } else {
	    id_to_comm = id - step;
	}

	MPI_Send(&partial_sum, 1, MPI_INT, id_to_comm, 0, MPI_COMM_WORLD);
        MPI_Recv(&rcv_value, 1, MPI_INT, id_to_comm, 0, MPI_COMM_WORLD, &status);
	partial_sum += rcv_value;
    }

    printf("i am %d and got %d\n", id, partial_sum);

    MPI_Finalize();
}