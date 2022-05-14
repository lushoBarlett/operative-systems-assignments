#include "cond_var.h"

void cond_var_init(struct cond_var* cv) {
  sem_init(&cv->condition, 0, 0);
  sem_init(&cv->leaving, 0, 0);
  cv->waiting = 0;
  pthread_mutex_init(&cv->lock, NULL);
}

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

  pthread_mutex_unlock(&cv->lock);
}