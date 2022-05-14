#pragma once
#include <pthread.h>
#include <semaphore.h>

struct cond_var {
  /*
   * Blocks until a post is done in function signal or broadcast
   */
  sem_t condition;

  /*
   * Aknowledges that a signal was received
   */
  sem_t leaving;

  /*
   * Number of threads blocked, waiting to receive a signal
   */
  int waiting;

  /*
   * To ensure the synchronization of all functions
   */
  pthread_mutex_t lock;
};

void cond_var_init(struct cond_var *);

void cv_wait(struct cond_var *, pthread_mutex_t *);

void cv_signal(struct cond_var *);

void cv_broadcast(struct cond_var *);