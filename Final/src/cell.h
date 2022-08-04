#pragma once

#include <pthread.h>

#include "bucket.h"

/*
 * Una celda de la tabla hash representa
 * la cabeza de una lista doblemente enlazada
 * de pares clave-valor. Si la celda está vacía
 * contendrá puntero a NULL.
 * 
 * Tiene un lock para mantener exclusividad en toda
 * la lista a la que la celda apunta.
 */
typedef struct {
	bucket_t* bucket;
	pthread_mutex_t lock;
} cell_t;

/*
 * Inicializa el lock y setea la lista en NULL.
 */
void cell_init(cell_t* cell);

/*
 * Bloquea la celda
 */
void cell_lock(cell_t* cell);

/*
 * Desbloquea la celda
 */
void cell_unlock(cell_t* cell);

/*
 * Predicado para saber si un bucket se encuentra en esta celda de la tabla hash
 *
 * La función en realidad solamente revisa si los punteros del bucket son distintos de
 * NULL, o si la cabeza de la celda apunta a ese bucket. No hay un chequeo que indique
 * si el bucket está en esa celda en particular o en otras, este chequeo debería hacerse
 * previamente. La ventaja de hacerlo así es que se evalúa en O(1).
 */
int cell_in_list(cell_t* cell, bucket_t* bucket);

/*
 * Dada una clave, busca en O(|cell|) si esa clave existe en la celda, NULL en otro caso.
 * 
 * De ser encontrado un bucket con esa clave, se retorna el mismo sumándole una referencia.
 */
bucket_t* cell_find(cell_t* cell, blob_t key);

/*
 * Borra en O(1) el bucket de la celda, y resta una referencia al mismo.
 *
 * Tener en cuenta que no se revisa que esa sea la celda correcta, por lo que
 * introducir la celda incorrecta producirá comportamiento extraño del programa.
 * 
 * Un comportamiento secundario de esta función es que si el bucket
 * no se encuentra en la celda porque ya fue borrado, o por
 * no haber sido introducido todavía, la función no tiene
 * ningún otro efecto más que restar una referencia al bucket.
 */
void cell_delete_bucket(cell_t* cell, bucket_t* bucket);

/*
 * Borra de la celda en O(|cell|) el bucket correspondiente a esa clave,
 * si lo hubiere. Sino devuelve NULL.
 * 
 * Retorna el bucket con la misma cantidad de referencias, ya que
 * borrar de la celda resta una, pero devolverla como resultado suma una.
 */
bucket_t* cell_delete(cell_t* cell, blob_t key);

/*
 * Inserta un bucket a la cabeza de la celda y retorna el bucket
 * con la misma clave que el que acabamos de insertar, si lo hubiere,
 * tal como si le hubiésemos llamado cell_delete a la clave, por lo que
 * la función se ejecuta en O(|cell|).
 */
bucket_t* cell_insert(cell_t* cell, bucket_t* bucket);
