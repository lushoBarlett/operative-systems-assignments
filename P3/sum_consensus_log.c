#include <mpi.h>
#include <stdio.h>

/*
 * Dados 2^n procesos que se encargan de un valor cada uno.
 * 
 * Primero envían de a pares sus valores, sincronizando la suma
 * en grupos de tamaño 2, o sea, cada nodo sincroniza con algùn otro nodo,
 * intercambiando sus valores.
 *
 * En pasos siguientes, cada grupo de tamaño 2^k, sincroniza la suma
 * con otro grupo del mismo tamaño (ya sea el de su izquierda o el de su derecha),
 * para esto, cada nodo en un grupo sincroniza la suma con algùn nodo del otro grupo.
 *
 * Como los grupos tienen el mismo tamaño, todas las comunicaciones ocurren de forma
 * concurrente, independiente unas de otras.
 *
 * Por lo tanto la comunicación entre grupos es perfectamente concurrente,
 * ya que hay una cantidad par de grupos en cada paso.
 */

int belongs_to_even_group(int id, int group_size) {
	int group = id / group_size;
	return group % 2 == 0;
}

void main(int argc, char **argv) {
	MPI_Init(&argc, &argv);

	MPI_Status status;

	int id;
	MPI_Comm_rank(MPI_COMM_WORLD, &id);

	int processes;
	MPI_Comm_size(MPI_COMM_WORLD, &processes);

	/*
	 * Elegimos el id como el valor privado de cada proceso,
	 * como arrancan en 0, les sumamos 1
	 */
	int private_value = id + 1;

	int partial = private_value;

	for (int group_size = 1; group_size < processes; group_size *= 2) {

		int communicate_to =
			belongs_to_even_group(id, group_size)
			? id + group_size
			: id - group_size;

		MPI_Send(&partial, 1, MPI_INT, communicate_to, 0, MPI_COMM_WORLD);

		int receive;
		MPI_Recv(&receive, 1, MPI_INT, communicate_to, 0, MPI_COMM_WORLD, &status);

		partial += receive;
	}

	printf("i am %d and got %d\n", id, partial);

	MPI_Finalize();
}
