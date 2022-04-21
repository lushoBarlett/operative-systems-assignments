#include <pthread.h>

typedef struct barrier {
  int n, n_max;
  pthread_mutex_t lk;
  pthread_cond_t cv;
} barrier;

void wait(barrier *b) {
  pthread_mutex_lock(&b->lk);
  b->n++;
  if (b->n == b->n_max) {
    b->n = 0;
    pthread_cond_broadcast(&b->cv);
  } else {
    pthread_cond_wait(&b->cv,&b->lk);
  }

  pthread_mutex_unlock(&b->lk);
}


void barrier_init(barrier *b, int n) {
  b->n = 0;
  b->n_max = n;
  pthread_mutex_init(&b->lk, NULL);
}
