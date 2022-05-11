#include <stdio.h>
#include <pthread.h>
#include <stdbool.h>
#include <math.h>
#define _GNU_SOURCE

#define N_VISITANTES 1000000
#define N_PROCESOS 4

int numero[N_PROCESOS];
bool eligiendo[N_PROCESOS];

int visitantes = 0;
pthread_mutex_t mutex;

void init_arrays() {
  for (int i = 0; i < N_PROCESOS; i++) {
    numero[i] = 0;
    eligiendo[i] = false;
  }
}

int max_array (int *arr, int len){
  int max = *arr;
  for (int i = 1; i < len; i++) {
    if (max < arr[i])
      max = arr[i];
  }
  return max;
}

void lock(int i) {
  eligiendo[i] = true;
  //mfence
  asm volatile ("mfence");
  numero[i] = 1 + max_array(numero,N_PROCESOS);
  eligiendo[i] = false;
  //mfence
  asm volatile ("mfence");
  for (int j = 0; j < N_PROCESOS; j++){
    while (eligiendo[j]); // busy waiting

    while ((numero[j] != 0) &&
      ((numero[j]<numero[i]) || ((numero[j] == numero[i]) && (j<i)))); //busy waiting
  }
}

void unlock(int i){
  numero[i] = 0;
}



void *molinete(void *arg) {
    int num = arg - (void*)0;
    int i;
    for (i=0;i<N_VISITANTES;i++) {
        lock(num);
        visitantes++;
        unlock(num);
    }
}

int main() {
    init_arrays();
    pthread_t m[N_PROCESOS];
    pthread_mutex_init(&mutex, NULL);
    for (int i = 0; i < N_PROCESOS; i++)
      pthread_create(&m[i], NULL, molinete, i + (void *)0);
    for (int i = 0; i < N_PROCESOS; i++)
      pthread_join(m[i], NULL);
    printf("Hoy hubo %d visitantes!\n", visitantes);
    return 0;
}
