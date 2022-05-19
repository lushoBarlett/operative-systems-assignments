#include <stdio.h>
#include <stdbool.h>
#include <omp.h>

#define N_VISITANTES 1000000

int visitantes = 0;

void molinete() {
	for (int i = 0; i < N_VISITANTES; i++)
		#pragma omp atomic
		visitantes++;
}

int main() {

  #pragma omp parallel
	molinete();

	printf("Hoy hubo %d visitantes!\n", visitantes);
	return 0;
}
