#include "semaphore.h"

void sem_init(sem_t* sem, unsigned int counter) {
	sem->counter = counter;
	pthread_mutex_init(&sem->counter_lock, NULL);
	pthread_cond_init(&sem->counted, NULL);
}

void sem_wait(sem_t* sem) {
	pthread_mutex_lock(&sem->counter_lock);

	while (sem->counter == 0)
		pthread_cond_wait(&sem->counted, &sem->counter_lock);

	pthread_mutex_unlock(&sem->counter_lock);
}

void sem_post(sem_t* sem) {
	pthread_mutex_lock(&sem->counter_lock);

	sem->counter++;

	pthread_cond_signal(&sem->counted);

	pthread_mutex_unlock(&sem->counter_lock);
}