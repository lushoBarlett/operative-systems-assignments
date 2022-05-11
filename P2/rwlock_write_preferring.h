#include <pthread.h>

pthread_mutex_t reading_lk, wanna_write_lk, writer;
int wanna_write, reading;
pthread_cond_t last_reader_leaves, no_writers;


void increment_writer_queue() {
	pthread_mutex_lock(&wanna_write_lk);
	wanna_write++;
	pthread_mutex_unlock(&wanna_write_lk);
}


void wait_for_readers_to_leave() {
	pthread_mutex_lock(&reading_lk);
	while (reading > 0)
		pthread_cond_wait(&last_reader_leaves, &reading_lk);
	pthread_mutex_unlock(&reading_lk);
}


void decrement_writer_queue() {
	pthread_mutex_lock(&wanna_write_lk);
	if (--wanna_write == 0)
		pthread_cond_broadcast(&no_writers);
	pthread_mutex_unlock(&wanna_write_lk);
}


void write_lock() {
	increment_writer_queue();
	wait_for_readers_to_leave();
	pthread_mutex_lock(&writer);
}


void write_unlock() {
	pthread_mutex_unlock(&writer);
	decrement_writer_queue();
}


void read_lock() {
	pthread_mutex_lock(&wanna_write_lk);
	while (wanna_write > 0)
		pthread_cond_wait(&no_writers, &wanna_write_lk);

	pthread_mutex_lock(&reading_lk);
	reading++;
	pthread_mutex_unlock(&reading_lk);
	pthread_mutex_unlock(&wanna_write_lk);
}


void read_unlock() {
	pthread_mutex_lock(&reading_lk);
	if (--reading == 0)
		pthread_cond_broadcast(&last_reader_leaves);
	pthread_mutex_unlock(&reading_lk);
}


void read_write_lock_init() {
	pthread_mutex_init(&reading_lk, NULL);
	pthread_mutex_init(&wanna_write_lk, NULL);
	pthread_mutex_init(&writer, NULL);
}