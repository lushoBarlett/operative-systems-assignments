#include <assert.h>
#include <stdlib.h>
#include <stdio.h>

#include "blob.h"
#include "record.h"

#define LOAD_FACTOR 0.75

/*
 * Un bucket contiene toda la información relevante
 * a los valores almacenados en la base de datos,
 * implementada como una tabla hash.
 *
 * Está incluida la lista enlazada usada para resolver
 * colisiones de la tabla hash.
 *
 * Está también incluida una cola, implementada como
 * una lista doblemente enlazada, para determinar
 * qué valores borrar de la caché cuando se supera
 * el límite de memoria establecido.
 */
typedef struct bucket_t {
	/*
	 * Puntero al valor almacenado,
	 * seguido de la cantidad de bytes
	 * que este valor ocupa en memoria.
	 */
	blob_t key;
	blob_t value;

	/*
	 * TODO: explicar
	 */
	uint64_t cell_index;

	/*
	 * TODO: exclusión mutua de "filas"
	 */

	/*
	 * Siguiente valor en la lista enlazada de datos
	 * que han colisionado por ser mapeados a la misma
	 * celda de la tabla hash.
	 *
	 * TODO: es una doble
	 */
	struct bucket_t* next_value;
	struct bucket_t* prev_value;

	/*
	 * Usamos una lista doblemente enlazada para
	 * representar una cola de valores ordenados
	 * por su última instancia de uso en la base de datos.
	 *
	 * De esa forma, está al frente el valor menos relevante
	 * y si necesitamos borrarlo lo podemos hacer.
	 *
	 * El siguiente valor de un nodo de la cola es aquel
	 * que se borraría después, y el anterior es aquel
	 * que se borraría antes.
	 */
	struct bucket_t* next_queue;
	struct bucket_t* prev_queue;
} bucket_t;

/*
 * Una celda de la tabla hash representa
 * la cabeza de una lista enlazada
 * de datos. Si la celda está vacía
 * contendrá puntero a NULL.
 */
typedef struct {
	bucket_t *bucket;
	pthread_mutex_t lock;
} cell_t;

/*
 * Un array de celdas es una tabla hash,
 * que en nuestro caso es el banco de datos en sí.
 * Se va redimensionando de acuerdo a
 * nuestras necesidades de almacenamiento.
 *
 * TODO: justificar LRU
 *
 * Como parte de la base de datos guardamos
 * los metadatos de sus operaciones y memoria usada.
 */
typedef struct {
	size_t size;
	size_t capacity;
	cell_t* hash_table;

	bucket_t* least_recently_used;

	concurrent_record_t record;

} database_t;

#define INITIAL_CAPACITY 37

static cell_t* hash_table_create() {
	return malloc(sizeof(cell_t) * INITIAL_CAPACITY);
}

void db_init(database_t* db) {
	db->hash_table = hash_table_create();
	db->size = 0;
	db->capacity = INITIAL_CAPACITY;
	db->least_recently_used = NULL;

	for (size_t i = 0; i < db->capacity; ++i)
		// TODO
		cell_init(&db->hash_table[i]);
}

void hash_table_destroy(database_t* db) {
	for (size_t i = 0; i < db->capacity; ++i)
		// TODO
		cell_destroy(&db->hash_table[i]);

	free(db->hash_table);
}

void cell_insert(cell_t* cell, blob_t* key, blob_t* value) {
	// TODO
}

void hash_table_insert(database_t* db, blob_t* key, blob_t* value) {
	uint64_t cell_index = horner(key) % db->capacity;

	cell_insert(&db->hash_table[cell_index], key, value);

	db->size++;

	if (db->size / (double) db->capacity > LOAD_FACTOR)
		hash_table_expand(db);
}

blob_t* hash_table_lookup(database_t* db, blob_t* key) {
	uint64_t cell_index = horner(key) % db->capacity;

	// TODO
	return cell_find(&db->hash_table[cell_index], key);
}

void hash_table_delete(database_t* db, blob_t* key) {
	uint64_t cell_index = horner(key) % db->capacity;

	db->size--;

	// TODO
	cell_delete(&db->hash_table[cell_index], key);
}

void hash_table_expand(database_t* db) {
	// TODO: recuperar los buckets
	bucket_t* buckets = collect_buckets(db->hash_table);
	uint64_t bucket_amount = db->size;

	db->size = 0;
	db->capacity *= 2;

	db->hash_table = realloc(db->hash_table, sizeof(cell_t) * db->capacity);
	for (size_t i = 0; i < db->capacity; ++i)
		// TODO
		cell_init(&db->hash_table[i]);

	// TODO: reinsertar los buckets
	for (size_t i = 0; i < bucket_amount; i++)
		hash_table_reinsert(db, buckets[i]);

	free(buckets);
}
