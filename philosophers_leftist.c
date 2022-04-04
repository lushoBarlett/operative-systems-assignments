#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <pthread.h>

#define N_FILOSOFOS 5
#define ESPERA 5000000
#define ZURDO 0

pthread_mutex_t tenedor[N_FILOSOFOS];

void pensar(int i) {
    printf("Filosofo %d pensando...\n",i);
    usleep(random() % ESPERA);
}


void comer(int i) {
    printf("Filosofo %d comiendo...\n",i);
    usleep(random() % ESPERA);
}


void tomar_tenedores(int i) {
    int tenedor1 = i, tenedor2 = (i+1)%N_FILOSOFOS;
    if (i == ZURDO) {
        tenedor1 = tenedor2; /* El tenedor a su izquierda */
        tenedor2 = i; /* El tenedor a su derecha */
    }
    
    pthread_mutex_lock(&tenedor[tenedor1]);
    pthread_mutex_lock(&tenedor[tenedor2]);
}


void dejar_tenedores(int i) {
    pthread_mutex_unlock(&tenedor[i]); /* Deja el tenedor de su derecha */
    pthread_mutex_unlock(&tenedor[(i+1)%N_FILOSOFOS]); /* Deja el tenedor de su izquierda */
}


void *filosofo(void *arg) {
    int i = (int) arg;
    for (;;) {
        tomar_tenedores(i);
        comer(i);
        dejar_tenedores(i);
        pensar(i);
    }
}


int main()
{
    int i;
    pthread_t filo[N_FILOSOFOS];
    for (i=0;i<N_FILOSOFOS;i++)
        pthread_mutex_init(&tenedor[i], NULL);
    for (i=0;i<N_FILOSOFOS;i++)
        pthread_create(&filo[i], NULL, filosofo, (void *)i);
    pthread_join(filo[0], NULL);
    return 0;
}