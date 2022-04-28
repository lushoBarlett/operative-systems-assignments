#include <pthread.h>
#include <semaphore.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <assert.h>

struct cond_var {
  sem_t condition;
  sem_t leaving;
  int waiting;
  pthread_mutex_t lock;
};

void cv_wait(struct cond_var *cv, pthread_mutex_t *lk) {
  pthread_mutex_lock(&cv->lock);

  pthread_mutex_unlock(lk);
  cv->waiting++;

  pthread_mutex_unlock(&cv->lock);

  sem_wait(&cv->condition);
  sem_post(&cv->leaving);

  pthread_mutex_lock(lk);
}

void cv_signal(struct cond_var *cv) {
  pthread_mutex_lock(&cv->lock);

  if (cv->waiting > 0) {
    cv->waiting--;
    sem_post(&cv->condition);
    sem_wait(&cv->leaving);
  }
  
  int aux;
  sem_getvalue(&cv->condition, &aux);
  assert(aux == 0);
  sem_getvalue(&cv->leaving, &aux);
  assert(aux == 0);

  pthread_mutex_unlock(&cv->lock);
}

void cv_broadcast(struct cond_var *cv) {
  pthread_mutex_lock(&cv->lock);

  int n = cv->waiting;

  for (int i = 0; i < n; i++)
    sem_post(&cv->condition);

  for (int i = 0; i < n; i++)
    sem_wait(&cv->leaving);

  cv->waiting = 0;

  int aux;
  sem_getvalue(&cv->condition, &aux);
  assert(aux == 0);
  sem_getvalue(&cv->leaving, &aux);
  assert(aux == 0);

  pthread_mutex_unlock(&cv->lock);
}

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

void cond_var_init(struct cond_var* cv) {
  sem_init(&cv->condition, 0, 0);
  sem_init(&cv->leaving, 0, 0);
  cv->waiting = 0;
  pthread_mutex_init(&cv->lock, NULL);
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
