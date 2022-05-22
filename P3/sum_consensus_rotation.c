#include <mpi.h>
#include <stdio.h>

void main(int argc, char **argv) {
    
    int id, n_processes, msg_value, partial_sum, source,
    priv_value, destination;
    MPI_Status status;

    MPI_Init(&argc, &argv);

    MPI_Comm_rank(MPI_COMM_WORLD, &id);    
    MPI_Comm_size(MPI_COMM_WORLD, &n_processes);

    // This is the private value
    priv_value = id;
    partial_sum = priv_value;

    // Calculation of source and destination of messages for each process
    if (id)
        source = id - 1;
    else
        source = n_processes - 1;
    destination = (id + 1) % (n_processes);

    // Starts sending the private value
    MPI_Send(&priv_value, 1, MPI_INT, destination, 0, MPI_COMM_WORLD);
    
    // Performs all the rotations
    for (int i = 0; i < n_processes-2; i++) {
        MPI_Recv(&msg_value, 1, MPI_INT, source, 0, MPI_COMM_WORLD, &status);
        partial_sum += msg_value;
        MPI_Send(&msg_value, 1, MPI_INT, destination, 0, MPI_COMM_WORLD);
    }
    
    // Last iteration doesn't send any message
    MPI_Recv(&msg_value, 1, MPI_INT, source, 0, MPI_COMM_WORLD, &status);
    partial_sum += msg_value;

    printf("i am %d and got %d\n", id, partial_sum);

    MPI_Finalize();
}