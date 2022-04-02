#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <pthread.h>
#include <semaphore.h>

sem_t tabaco, papel, fosforos, otra_vez;

void agente() {
    for (;;) {
        int caso = random() % 3;
        sem_wait(&otra_vez);
        switch (caso) {
            case 0:
                sem_post(&tabaco);
                sem_post(&papel);
                break;
            case 1:
                sem_post(&fosforos);
                sem_post(&tabaco);
                break;
            case 2:
                sem_post(&papel);
                sem_post(&fosforos);
                break;
        }
    }
}

void fumar(int fumador) {
    printf("Fumador %d: Puf! Puf! Puf!\n", fumador);
    sleep(1);
}

void *fumador1(void *arg) {
    for (;;) {
        sem_wait(&tabaco);
        if (sem_trywait(&papel) == 0) {
            fumar(1);
            sem_post(&otra_vez);
        } else {
            sem_post(&tabaco);
        }
    }
}


void *fumador2(void *arg) {
    for (;;) {
        sem_wait(&fosforos);
        if (sem_trywait(&tabaco) == 0) {
            fumar(2);
            sem_post(&otra_vez);
        } else {
            sem_post(&fosforos);
        }
    }
}

void *fumador3(void *arg) {
    for (;;) {
        sem_wait(&papel);
        if (sem_trywait(&fosforos) == 0) {
            fumar(3);
            sem_post(&otra_vez);
        } else {
            sem_post(&papel);
        }
    }
}


int main() {
    pthread_t s1, s2, s3;
    sem_init(&tabaco, 0, 0);
    sem_init(&papel, 0, 0);
    sem_init(&fosforos, 0, 0);
    sem_init(&otra_vez, 0, 1);
    pthread_create(&s1, NULL, fumador1, NULL);
    pthread_create(&s2, NULL, fumador2, NULL);
    pthread_create(&s3, NULL, fumador3, NULL);
    agente();
    return 0;
}