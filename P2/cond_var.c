#include <pthread.h>
#include <semaphore.h>
#include <stdio.h>
#include <stdlib.h>

struct cond_var {
  sem_t sem;
  int waiting;
  pthread_mutex_t lk;
};

void cv_wait(struct cond_var *cv, pthread_mutex_t *lk) {
  pthread_mutex_lock(&cv->lk);

  pthread_mutex_unlock(lk);
  cv->waiting++;

  pthread_mutex_unlock(&cv->lk);
  sem_wait(&cv->sem);

  pthread_mutex_lock(lk);
}

void cv_signal(struct cond_var *cv) {
  pthread_mutex_lock(&cv->lk);
  if (cv->waiting > 0) {
    cv->waiting--;
    sem_post(&cv->sem);
  }
  pthread_mutex_unlock(&cv->lk);
}

void cv_broadcast(struct cond_var *cv) {
  pthread_mutex_lock(&cv->lk);
  int n = cv->waiting;

  for (int i = 0; i < n; i++) {
    sem_post(&cv->sem);
  }
  cv->waiting = 0;

  pthread_mutex_unlock(&cv->lk);
}

pthread_mutex_t lock;


void * espera1(void * arg) {
  while (1) {
    cv_wait((struct cond_var *) arg,&lock);
    printf("Recibi la señal1\n");
  }
}

void * espera2(void * arg) {
  while (1) {
    cv_wait((struct cond_var *) arg,&lock);
    printf("Recibi la señal2\n");
  }
}

void * espera3(void * arg) {
  while (1) {
    cv_wait((struct cond_var *) arg,&lock);
    printf("Recibi la señal3\n");
  }
}

void * senala(void * arg) {
  while(1) {
    sleep(1);
    cv_broadcast((struct cond_var *) arg);
  }
}

int main(){
  sem_t sem;
  sem_init(&sem, 0, 0);

  pthread_mutex_t lk;
  pthread_mutex_init(&lk, NULL);

  pthread_mutex_init(&lock, NULL);

  struct cond_var cv;
  cv.sem = sem;
  cv.waiting = 0;
  cv.lk = lk;

  pthread_t t[4];

  pthread_create(&t[0], NULL, espera1, (void *) &cv);
  pthread_create(&t[1], NULL, espera2, (void *) &cv);
  pthread_create(&t[2], NULL, espera3, (void *) &cv);
  pthread_create(&t[3], NULL, senala, (void *) &cv);

  pthread_join(t[0],NULL);

  return 0;
}
