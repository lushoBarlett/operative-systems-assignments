#pragma once

#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>

#define ESPERA 5000000
#define N_FILOSOFOS 5

int izquierda(int i) {
    return (i + 1) % N_FILOSOFOS;
}

void pensar(int i, useconds_t tiempo) {
	printf("Filosofo %d pensando...\n", i);
	usleep(tiempo);
}

void comer(int i, useconds_t tiempo) {
	printf("Filosofo %d comiendo...\n", i);
	usleep(tiempo);
}
