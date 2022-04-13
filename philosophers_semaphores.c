#include <pthread.h>
#include <semaphore.h>

#include "philosopher_utils.h"

pthread_mutex_t tenedores[N_FILOSOFOS];
sem_t comedores_disponibles;

void tomar_tenedores(int i) {
	pthread_mutex_lock(&tenedores[i]);
	pthread_mutex_lock(&tenedores[izquierda(i)]);
}

void dejar_tenedores(int i) {
	pthread_mutex_unlock(&tenedores[i]);
	pthread_mutex_unlock(&tenedores[izquierda(i)]);
}

void* filosofo(void* arg) {
	int i = (intptr_t)arg;

	while(1) {
		sem_wait(&comedores_disponibles);
		tomar_tenedores(i);
		comer(i);
		dejar_tenedores(i);
		sem_post(&comedores_disponibles);
		pensar(i);
	}
}

int main() {
	int i;
	pthread_t filosofos[N_FILOSOFOS];

	sem_init(&comedores_disponibles, 0, N_FILOSOFOS - 1);

	for (i = 0; i < N_FILOSOFOS; i++)
		pthread_mutex_init(&tenedores[i], NULL);

	for (i = 0; i < N_FILOSOFOS; i++)
		pthread_create(&filosofos[i], NULL, filosofo, (void*)(intptr_t)i);

	pthread_join(filosofos[0], NULL);

	return 0;
}