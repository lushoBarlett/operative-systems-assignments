#pragma once

#include "blob.h"
#include "record.h"

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
	bucket_t* bucket;
	pthread_mutex_t lock;
} cell_t;

/*
 * Un array de celdas es una tabla hash,
 * que en nuestro caso es el banco de datos en sí.
 * Se va redimensionando de acuerdo a
 * nuestras necesidades de almacenamiento.
 */
typedef struct {
	size_t size;
	size_t capacity;
	cell_t* cells;
} hash_table_t;

/*
 * TODO: LRU
 */
typedef struct {
	bucket_t* back;
	bucket_t* front;
	pthread_mutex_t lock;
} lru_queue_t;

/*
 * TODO: justificar LRU
 *
 * Como parte de la base de datos guardamos
 * los metadatos de sus operaciones y memoria usada.
 */
typedef struct {
	hash_table_t hash_table;
	lru_queue_t lru_queue;
	concurrent_record_t record;
} database_t;
