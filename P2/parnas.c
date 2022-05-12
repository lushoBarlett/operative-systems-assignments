#ifdef READ_PREF
	#include "rwlock_read_preferring.h"
#elif WRITE_PREF
	#include "rwlock_write_preferring.h"
#else
	#include "rwlock_fair.h"
#endif

#include <pthread.h>
#include <stdio.h>

#define M 5
#define N 5
#define ARRLEN 10240

read_write_lock_t rw_lock;

int arr[ARRLEN];

void* escritor(void* arg) {
	int num = arg - (void*)0;

	while (1) {
		//sleep(random() % 3);

		write_lock(&rw_lock);

		printf("Escritor %d escribiendo\n", num);

		for (int i = 0; i < ARRLEN; i++)
			arr[i] = num;

		write_unlock(&rw_lock);
	}

	return NULL;
}


void* lector(void *arg) {
	int num = arg - (void*)0;

	while (1) {
		//sleep(random() % 3);

		read_lock(&rw_lock);

		int v = arr[0];
		int i;
		for (i = 1; i < ARRLEN; i++)
			if (arr[i] != v)
				break;

		if (i < ARRLEN)
			printf("Lector %d, error de lectura\n", num);
		else
			printf("Lector %d, dato %d\n", num, v);

		read_unlock(&rw_lock);
	}

	return NULL;
}

int main() {
	pthread_t lectores[M], escritores[N];

	read_write_lock_init(&rw_lock);

	for (int i = 0; i < M; i++)
		//pthread_create(&lectores[i], NULL, lector, i + (void*)0);

	for (int i = 0; i < N; i++)
		pthread_create(&escritores[i], NULL, escritor, i + (void*)0);

	pthread_join(lectores[0], NULL);

	return 0;
}
