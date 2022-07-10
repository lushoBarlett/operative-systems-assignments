#include "database.h"

// TODO: make concurrent

database_t db_create() {
	database_t database;
	
	hash_table_init(&database.hash_table);

	lru_queue_init(&database.hash_table);

	record_init(&database.record);

	return database;
}

void db_put(database_t* database, blob_t key, blob_t value) {
	bucket_t* bucket = bucket_create(key, value);
	
	bucket_t* old = hash_table_insert(&database->hash_table, bucket);

	if (old) {
		lru_queue_delete(&database->lru_queue, old);
		
		bucket_free(old);
	}

	lru_queue_enqueue(&database->lru_queue, bucket);

	counter_increment(&database->record.puts);
}

// TODO: return blob pointers?

const bucket_t* db_get(database_t* database, blob_t key) {
	bucket_t* bucket = hash_table_lookup(&database->hash_table, key);

	if (bucket)
		lru_queue_reenqueue(&database->lru_queue, bucket);

	counter_increment(&database->record.gets);

	return bucket;
}

bucket_t* db_take(database_t* database, blob_t key) {
	bucket_t* bucket = hash_table_lookup(&database->hash_table, key);

	if (bucket) {
		hash_table_delete_bucket(&database->hash_table, bucket);
		
		lru_queue_delete(&database->lru_queue, bucket);
	}

	counter_increment(&database->record.dels); // takes?

	return bucket;
}

void db_delete(database_t* database, blob_t key) {
	bucket_t* bucket = db_take(database, key);

	if (bucket)
		bucket_free(bucket);

	counter_increment(&database->record.dels);
}

record_t db_stats(database_t* database) {
	return report(&database->record);
}

static void free_one_bucket(database_t* database) {
	bucket_t* bucket = lru_queue_dequeue(&database->lru_queue);

	hash_table_delete(&database->hash_table, bucket->key);

	bucket_free(bucket);
}

void* db_memsafe_malloc(database_t* database, size_t bytes) {
	void* mallocd = malloc(bytes);
	
	while (mallocd == NULL)
		free_one_bucket(database);

	return mallocd;
}

void db_destroy(database_t* database) {
	while (database->lru_queue.front)
		free_one_bucket(database);
	
	free(database->hash_table.cells);
}
