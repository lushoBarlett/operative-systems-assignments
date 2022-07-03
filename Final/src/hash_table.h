#include "cell.h"

#define LOAD_FACTOR 0.75
#define INITIAL_CAPACITY 37

void hash_table_init(hash_table_t* hash_table);

bucket_t* hash_table_insert(hash_table_t* hash_table, blob_t* key, blob_t* value);

const blob_t* hash_table_lookup(hash_table_t* hash_table, const blob_t* key);

void hash_table_delete(hash_table_t* hash_table, const blob_t* key);

void hash_table_free(hash_table_t* hash_table);
