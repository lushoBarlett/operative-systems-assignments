#include <pthread.h>
#include <semaphore.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

sem_t tabaco, papel, fosforos, otra_vez;

sem_t can_smoke1, can_smoke2, can_smoke3;

sem_t smoker1, smoker2, smoker3;

pthread_mutex_t mutex;

void agente() {
	while(1) {
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

void* fumador1(void* arg) {
	while(1) {
		sem_wait(&smoker1);

		fumar(1);
		
		sem_post(&otra_vez);
	}
}

void* fumador2(void* arg) {
	while(1) {
		sem_wait(&smoker2);

		fumar(2);

		sem_post(&otra_vez);
	}
}

void* fumador3(void* arg) {
	while(1) {
		sem_wait(&smoker3);

		fumar(3);

		sem_post(&otra_vez);
	}
}

void* inspect_tabaco(void* arg) {
	while (1) {
		sem_wait(&tabaco);

		pthread_mutex_lock(&mutex);

		sem_post(&can_smoke1);
		sem_post(&can_smoke2);

		pthread_mutex_unlock(&mutex);
	}
}

void* inspect_papel(void* arg) {
	while (1) {
		sem_wait(&papel);

		pthread_mutex_lock(&mutex);

		sem_post(&can_smoke3);
		sem_post(&can_smoke1);

		pthread_mutex_unlock(&mutex);
	}
}

void* inspect_fosforos(void* arg) {
	while (1) {
		sem_wait(&fosforos);

		pthread_mutex_lock(&mutex);

		sem_post(&can_smoke2);
		sem_post(&can_smoke3);

		pthread_mutex_unlock(&mutex);
	}
}

void* middle_man(void* arg) {
	while (1) {
		sem_wait(&can_smoke1);
		sem_wait(&can_smoke2);
		sem_wait(&can_smoke3);

		pthread_mutex_lock(&mutex);

		if (sem_trywait(&can_smoke1) == 0)
			sem_post(&smoker1);

		else if (sem_trywait(&can_smoke2) == 0)
			sem_post(&smoker2);

		else if (sem_trywait(&can_smoke3) == 0)
			sem_post(&smoker3);

		pthread_mutex_unlock(&mutex);
	}
}

int main() {
	pthread_t s1, s2, s3;

	pthread_t insp_tabaco, insp_papel, insp_fosforos, mm;

	pthread_mutex_init(&mutex, NULL);

	sem_init(&smoker1, 0, 0);
	sem_init(&smoker2, 0, 0);
	sem_init(&smoker3, 0, 0);

	sem_init(&can_smoke1, 0, 0);
	sem_init(&can_smoke2, 0, 0);
	sem_init(&can_smoke3, 0, 0);

	sem_init(&otra_vez, 0, 1);

	pthread_create(&s1, NULL, fumador1, NULL);
	pthread_create(&s2, NULL, fumador2, NULL);
	pthread_create(&s3, NULL, fumador3, NULL);

	pthread_create(&insp_tabaco, NULL, inspect_tabaco, NULL);
	pthread_create(&insp_papel, NULL, inspect_papel, NULL);
	pthread_create(&insp_fosforos, NULL, inspect_fosforos, NULL);

	pthread_create(&mm, NULL, middle_man, NULL);

	agente();

	return 0;
}
