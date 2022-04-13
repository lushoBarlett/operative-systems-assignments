#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#define N_FILOSOFOS 5
#define ESPERA 5000000
#define ZURDO 0

pthread_mutex_t tenedores[N_FILOSOFOS];

int izquierda(int i) {
    return (i + 1) % N_FILOSOFOS;
}

void esperar() {
	usleep(random() % ESPERA);
}

void pensar(int i) {
	printf("Filosofo %d pensando...\n", i);
	esperar();
}

void comer(int i) {
	printf("Filosofo %d comiendo...\n", i);
	esperar();
}

void tomar_tenedores(int i) {
	int tenedor1, tenedor2;

    if (i == ZURDO) {
		tenedor1 = izquierda(i);
		tenedor2 = i;
    } else {
		tenedor1 = i;
		tenedor2 = izquierda(i);
    }

	pthread_mutex_lock(&tenedores[tenedor1]);
	pthread_mutex_lock(&tenedores[tenedor2]);
}

void dejar_tenedores(int i) {
	pthread_mutex_unlock(&tenedores[i]);
	pthread_mutex_unlock(&tenedores[izquierda(i)]);
}

void* filosofo(void* arg) {
	int i = (intptr_t)arg;

	while(1) {
		tomar_tenedores(i);
		comer(i);
		dejar_tenedores(i);
		pensar(i);
	}
}

int main() {
	int i;
	pthread_t filosofos[N_FILOSOFOS];

	for (i = 0; i < N_FILOSOFOS; i++)
		pthread_mutex_init(&tenedores[i], NULL);

	for (i = 0; i < N_FILOSOFOS; i++)
		pthread_create(&filosofos[i], NULL, filosofo, (void*)(intptr_t)i);

	pthread_join(filosofos[0], NULL);

	return 0;
}
