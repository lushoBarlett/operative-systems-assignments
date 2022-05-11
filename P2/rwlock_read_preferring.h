#include <pthread.h>

pthread_mutex_t reader, writer;
int n_readers;


void write_lock() {
	pthread_mutex_lock(&writer);
}


void write_unlock() {
	pthread_mutex_unlock(&writer);
}


void read_lock() {
	pthread_mutex_lock(&reader);
	if (!n_readers)
		pthread_mutex_lock(&writer);
	n_readers++;
	pthread_mutex_unlock(&reader);
}


void read_unlock() {
	pthread_mutex_lock(&reader);
	n_readers--;
	if (!n_readers)
		pthread_mutex_unlock(&writer);
	pthread_mutex_unlock(&reader);
}


void read_write_lock_init() {
	pthread_mutex_init(&reader, NULL);
	pthread_mutex_init(&writer, NULL);
	n_readers = 0;
}
