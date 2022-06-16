int main() {
	database_t database = db_create();

	db_put(&database, key, value);

	const void* value = db_get(&database, key);

	const void* value = db_take(&database, key);

	db_delete(&database, key);

	record_t record = db_stats(&database);

	db_destroy(&database);
}
