typedef struct counter64_t {
	uint64_t value;	
	pthread_mutex_t lock;
}

counter_init(uint64_t value) {
	counter64_t counter;
	counter.value = value;
	pthread_mutex_init(&counter.lock);
	return counter;
}

counter_add(counter64_t* counter, uint64_t amount) {
	pthread_mutex_lock(&counter->lock);
	counter->value += amount;
	pthread_mutex_unlock(&counter->lock);
}

counter_sub(counter64_t* counter, uint64_t amount) {
	pthread_mutex_lock(&counter->lock);
	counter->value -= amount;
	pthread_mutex_unlock(&counter->lock);
}

counter_increment(counter64_t* counter) {
	counter_add(counter, 1);
}

counter_get(counter64_t* counter) {
	return counter->value;
}
