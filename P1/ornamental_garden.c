#include <stdio.h>
#include <pthread.h>
#define N_VISITANTES 1000000

int visitantes = 0;
pthread_mutex_t mutex;

void *molinete(void *arg) {
    int i;
    for (i=0;i<N_VISITANTES;i++) {
        pthread_mutex_lock(&mutex);
        visitantes++;
        pthread_mutex_unlock(&mutex);
    }
}

int main() {
    pthread_t m1, m2;
    pthread_mutex_init(&mutex, NULL);
    pthread_create(&m1, NULL, molinete, NULL);
    pthread_create(&m2, NULL, molinete, NULL);
    pthread_join(m1, NULL);
    pthread_join(m2, NULL);
    printf("Hoy hubo %d visitantes!\n", visitantes);
    return 0;
}