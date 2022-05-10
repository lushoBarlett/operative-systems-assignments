#pragma once

#include <pthread>

struct channel {
	int value;

	/*
	 * Locks the whole channel_write function
	 * to prevent further writers from entering
	 * before the current one leaves
	 */
	pthread_mutex_t writer_lock;

	/*
	 * Indicates wether there's a value currently
	 * written to the channel
	 */
	sem_t written;

	/*
	 * Indicates wether the value written to the
	 * channel has been read by any reader
	 */
	sem_t read;
};

void channel_init(struct channel* chan);

void channel_write(struct channel* chan, int value);

int channel_read(struct channel* chan);
