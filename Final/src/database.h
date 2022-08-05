#pragma once

#include "cell.h"
#include "lru_queue.h"
#include "record.h"

#define CAPACITY 999983

typedef struct {
	/*
	 * Cantidad de pares clave-valor que están almacenados.
	 */
	counter64_t size;

	/*
	 * Array fijo de celdas, de tamaño CAPACITY, que es la tabla hash
	 * de direccionamiento abierto donde estarán almacenados los pares clave-valor.
	 */
	cell_t* cells;

	/*
	 * LRU, una cola de datos de acuerdo a su frecuencia de uso, los valores más viejos
	 * serán borrados primero cuando la memoria no sea suficiente.
	 */
	lru_queue_t lru_queue;

	/*
	 * Set de contadores concurrentes que cuentan la cantidad de veces que cada
	 * una de las cuatro operaciones fueron pedidas a la base de datos.
	 */
	concurrent_record_t record;
} database_t;

/*
 * Inicializa el contador en 0, reserva memoria para la tabla hash
 * e inicializa todas sus celdas, la LRU y los contadores concurrentes.
 */
void database_init(database_t* database);

/*
 * Inserta un par clave-valor en la base de datos,
 * asumiendo que no fue insertado todavía.
 * 
 * Le suma dos referencias al bucket, una por la tabla hash
 * y otra por la LRU.
 */
void database_put(database_t* database, bucket_t* bucket);

/*
 * Dada una clave, busca en si hay un par clave-valor asociado, NULL en otro caso.
 * 
 * De ser encontrado un bucket con esa clave, se retorna el mismo sumándole una referencia.
 */
bucket_t* database_get(database_t* database, blob_t key);

/*
 * Dada una clave, busca si hay un par clave-valor asociado y se retorna
 * eliminándolo de la base de datos, NULL en otro caso.
 * 
 * De ser encontrado un bucket con esa clave, se le restan dos referencias,
 * una por la tabla hash, y otra por la LRU, pero se le suma una por ser
 * retornado por la función.
 */
bucket_t* database_take(database_t* database, blob_t key);

/*
 * Dada una clave, elimina el par clave-valor asociado, retorna 1 si lo encuentra y 0 si no.
 * 
 * De ser encontrado un bucket con esa clave, se le restan dos referencias,
 * una por la tabla hash, y otra por la LRU.
 */
int database_delete(database_t* database, blob_t key);

/*
 * Produce un record con las estadísticas de la base de datos
 */
record_t database_stats(database_t* database);

/*
 * Realiza un malloc de una determinada cantidad de bytes de forma
 * que de no poder realizarse por falta de memoria, libera pares
 * clave-valor de la base de datos en orden de menos utilizados
 * hasta poder reservar la memoria.
 * 
 * Si se vacía la base de datos y aún no se puede hacer el malloc,
 * la función retornará NULL.
 */
void* database_memsafe_malloc(database_t* database, size_t bytes);

/*
 * Borra todos los pares clave-valor de la base de datos
 * y libera el array de celdas de la tabla hash.
 */
void database_destroy(database_t* database);
