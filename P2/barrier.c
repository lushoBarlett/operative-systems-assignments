#include <pthread.h>

typedef struct barrier_t {
	int counter;
	int counter_max;

	pthread_mutex_t lock;
	pthread_cond_t condition_variable;
} barrier_t;

void wait(barrier_t* barrier) {
	pthread_mutex_lock(&barrier->lock);

	barrier->counter++;
	
	if (barrier->counter == barrier->counter_max) {
		barrier->counter = 0;
		pthread_cond_broadcast(&barrier->condition_variable);
	} else {
		pthread_cond_wait(&barrier->condition_variable, &barrier->lock);
	}

	pthread_mutex_unlock(&barrier->lock);
}

void barrier_init(barrier_t* barrier, int counter_max) {
	barrier->counter = 0;
	barrier->counter_max = counter_max;

	pthread_mutex_init(&barrier->lock, NULL);
	pthread_cond_init(&barrier->condition_variable, NULL);
}
