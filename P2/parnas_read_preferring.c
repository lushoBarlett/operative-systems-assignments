#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <pthread.h>

#define M 5
#define N 5
#define ARRLEN 10240

int arr[ARRLEN];
pthread_mutex_t reader, writer;
int n_readers;

void* escritor(void* arg) {
	int num = arg - (void*)0;

	while (1) {
		sleep(random() % 3);

		pthread_mutex_lock(&writer);

		printf("Escritor %d escribiendo\n", num);

		for (int i = 0; i < ARRLEN; i++)
			arr[i] = num;

		pthread_mutex_unlock(&writer);
	}

	return NULL;
}

void* lector(void *arg) {


	int num = arg - (void*)0;

	while (1) {
		sleep(random() % 3);

		pthread_mutex_lock(&reader);
		if (!n_readers)
			pthread_mutex_lock(&writer);
		n_readers++;
		pthread_mutex_unlock(&reader);


		int v = arr[0];
		int i;
		for (i = 1; i < ARRLEN; i++)
			if (arr[i] != v)
				break;

		if (i < ARRLEN)
			printf("Lector %d, error de lectura\n", num);
		else
			printf("Lector %d, dato %d\n", num, v);

		pthread_mutex_lock(&reader);
		n_readers--;
		if (!n_readers)
			pthread_mutex_unlock(&writer);
		pthread_mutex_unlock(&reader);

	}

	return NULL;
}

int main() {
	pthread_t lectores[M], escritores[N];
	pthread_mutex_init(&reader, NULL);
	pthread_mutex_init(&writer, NULL);
	n_readers = 0;

	for (int i = 0; i < M; i++)
		pthread_create(&lectores[i], NULL, lector, i + (void*)0);

	for (int i = 0; i < N; i++)
		pthread_create(&escritores[i], NULL, escritor, i + (void*)0);

	pthread_join(lectores[0], NULL);

	return 0;
}
