#include <stdio.h>
#include <stdlib.h>

#define N 1000

int A[N][N], B[N][N], C[N][N];

void mult(int A[N][N], int B[N][N], int C[N][N]) {
	#pragma omp parallel for
	for (int i = 0; i < N; i++)
	for (int j = 0; j < N; j++)
	for (int k = 0; k < N; k++)
		C[k][i] += A[k][j] * B[j][i];
}

void init_matrices() {
	for (int i = 0; i < N; i++) {
		for (int j = 0; j < N; j++) {
			A[i][j] = random() % 1000;
			B[i][j] = random() % 1000;
		}
	}
}

int main() {
	init_matrices();

	mult(A, B, C);

	return 0;
}