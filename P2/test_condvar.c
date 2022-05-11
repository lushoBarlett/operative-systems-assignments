#include "cond_var.h"
#include <pthread.h>
#include <stdio.h>

pthread_mutex_t lock;


void * espera1(void * arg) {
  while (1) {
    pthread_mutex_lock(&lock);
    cv_wait((struct cond_var *) arg,&lock);
    printf("Recibi la señal1\n");
    pthread_mutex_unlock(&lock);
  }
}

void * espera2(void * arg) {
  while (1) {
    pthread_mutex_lock(&lock);
    cv_wait((struct cond_var *) arg,&lock);
    printf("Recibi la señal2\n");
    pthread_mutex_unlock(&lock);
  }
}

void * espera3(void * arg) {
  while (1) {
    pthread_mutex_lock(&lock);
    cv_wait((struct cond_var *) arg,&lock);
    printf("Recibi la señal3\n");
    pthread_mutex_unlock(&lock);
  }
}

void * senala(void * arg) {
  int p = 1e9 + 7;

  while(1) {

    p *= 37;

    if (p < 0)
      cv_broadcast((struct cond_var *) arg);
    else
      cv_signal((struct cond_var *) arg);

  }
}

int main(){
  pthread_mutex_init(&lock, NULL);

  struct cond_var cv;
  cond_var_init(&cv);

  pthread_t t[4];

  pthread_create(&t[0], NULL, espera1, (void *) &cv);
  pthread_create(&t[1], NULL, espera2, (void *) &cv);
  pthread_create(&t[2], NULL, espera3, (void *) &cv);
  pthread_create(&t[3], NULL, senala, (void *) &cv);

  pthread_join(t[0], NULL);

  return 0;
}