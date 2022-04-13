#pragma once

#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>

#define ESPERA 5000000
#define N_FILOSOFOS 5

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
