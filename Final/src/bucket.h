#pragma once

#include "blob.h"

#include <stddef.h>

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
	size_t cell_index;

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

bucket_t* bucket_create(blob_t key, blob_t value);

void bucket_free(bucket_t* bucket);
