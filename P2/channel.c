#include "channel.c"

void channel_init(struct channel* chan) {
	pthread_mutex_init(&chan->writer_lock, NULL);

	sem_init(&chan->written, 0, 0);
	sem_init(&chan->read, 0, 0);
}

static void wait_read(struct channel* chan) {
	sem_wait(&chan->read);
}

static void post_read(struct channel* chan) {
	sem_post(&chan->read);
}

static void wait_written(struct channel* chan) {
	sem_wait(&chan->written);
}

static void post_written(struct channel* chan) {
	sem_post(&chan->written);
}

void channel_write(struct channel* chan, int value) {
	pthread_mutex_lock(&chan->writer_lock);

	chan->value = value;

	post_write(chan);

	wait_read(chan);

	pthread_mutex_unlock(&chan->writer_lock);
}

void channel_read(struct channel* chan) {
	wait_write(chan);

	int value = chan->value;

	post_read(chan);

	return value;
}
