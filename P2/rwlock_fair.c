#include "rwlock_fair.h"

#include <semaphore.h>

enum action_type {
	action_read,
	action_write,
};

struct action_t {
	enum action_type type;
	pthread_cond_t wake_up;
};

typedef struct action_t action_t;

action_t* new_action(enum action_type type) {
	action_t* action = malloc(sizeof(*action));
	action->type = type;
	pthread_cond_init(&action->wake_up, NULL);
	return action;
}

int reader_should_wait(read_write_lock_t* rw_lock) {
	return rw_lock->queue.size || rw_lock->writer;
}

int writer_should_wait(read_write_lock_t* rw_lock) {
	return reader_should_wait(rw_lock) || rw_lock->readers;
}

void read_write_lock_init(read_write_lock_t* rw_lock) {
	rw_lock->readers = 0;
	rw_lock->writer = 0;
	queue_init(&rw_lock->queue);
	pthread_mutex_init(&rw_lock->lock, NULL);
}

void write_lock(read_write_lock_t* rw_lock) {
	pthread_mutex_lock(&rw_lock->lock);

	if (writer_should_wait(rw_lock)) {
		action_t* action = new_action(action_write);

		enqueue(&rw_lock->queue, action);

		pthread_cond_wait(&action->wake_up, &rw_lock->lock);
	}

	rw_lock->writer++;

	pthread_mutex_unlock(&rw_lock->lock);
}

void read_lock(read_write_lock_t* rw_lock) {
	pthread_mutex_lock(&rw_lock->lock);

	if (reader_should_wait(rw_lock)) {
		action_t* action = new_action(action_read);

		enqueue(&rw_lock->queue, action);

		pthread_cond_wait(&action->wake_up, &rw_lock->lock);
	}

	rw_lock->readers++;

	pthread_mutex_unlock(&rw_lock->lock);
}

void dequeue_signal(read_write_lock_t* rw_lock) {

	int readers_woken_up = 0;

	while (&rw_lock->queue.size) {
		action_t* action = front(&rw_lock->queue);

		if (action->type == action_read) {
			dequeue(&rw_lock->queue);

			pthread_cond_signal(&action->wake_up);

			free(action);
			readers_woken_up = 1;
		}

		else if (action->type == action_write && !readers_woken_up) {
			dequeue(&rw_lock->queue);

			pthread_cond_signal(&action->wake_up);

			free(action);
			break;
		}
	}
}

void write_unlock(read_write_lock_t* rw_lock) {
	pthread_mutex_lock(&rw_lock->lock);

	rw_lock->writer--;

	dequeue_signal(rw_lock);

	pthread_mutex_unlock(&rw_lock->lock);
}

void read_unlock(read_write_lock_t* rw_lock) {
	pthread_mutex_lock(&rw_lock->lock);

	rw_lock->readers--;

	if (rw_lock->readers == 0)
		dequeue_signal(rw_lock);

	pthread_mutex_unlock(&rw_lock->lock);
}