#include "database.h"

// TODO: make concurrent, implement memory usage

database_t db_create() {
	database_t database;
	
	hash_table_init(&database.hash_table);
	lru_queue_init(&database.hash_table);
	record_init(&database.record);

	return database;
}

void db_put(database_t* database, blob_t* key, blob_t* value) {
	bucket_t* bucket = hash_table_insert(&database->hash_table, key, value);

	lru_queue_enqueue(&database->lru_queue, bucket);

	counter_increment(&database->record.puts);
}

// TODO: return blob pointers?

const bucket_t* db_get(database_t* database, blob_t* key) {
	bucket_t* bucket = hash_table_lookup(&database->hash_table, key);

	if (bucket)
		lru_queue_reenqueue(&database->lru_queue, bucket);

	counter_increment(&database->record.gets);

	return bucket;
}

bucket_t* db_take(database_t* database, blob_t* key) {
	bucket_t* bucket = hash_table_lookup(&database->hash_table, key);

	if (bucket) {
		hash_table_delete(&database->hash_table, key);
		lru_queue_delete(&database->lru_queue, bucket);
	}

	counter_increment(&database->record.dels); // takes?

	return bucket;
}

void db_delete(database_t* database, blob_t* key) {
	bucket_t* bucket = db_take(database, key);

	if (bucket)
		bucket_free(bucket);

	counter_increment(&database->record.dels);
}

record_t db_stats(database_t* database) {
	return report(&database->record);
}

void db_destroy(database_t* database) {
	hash_table_free(&database->hash_table);
}
