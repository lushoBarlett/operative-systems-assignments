#pragma once

#include "blob.h"
#include "counter64.h"

#include <stddef.h>

/*
 * Un bucket contiene toda la información relevante
 * a los valores almacenados en la base de datos,
 * implementada como una tabla hash.
 *
 * Está incluida la lista doblemente enlazada usada
 * para resolver colisiones de la tabla hash.
 *
 * Está también incluida una cola, implementada también
 * como una lista doblemente enlazada, para determinar
 * qué valores borrar de la caché cuando se supera
 * el límite de memoria establecido.
 */
typedef struct bucket_t {
	blob_t key;
	blob_t value;

	/*
	 * Contador de referencias para saber
	 * cuántas estructuras o procesos están
	 * usando el bucket a la vez, como un
	 * mecanismo para mantenerlo vivo
	 * hasta que el contador llegue a 0.
	 * 
	 * Funciona concurrentemente.
	 */
	counter64_t references;

	/*
	 * Índice en la tabla hash al que este bucket corresponde,
	 * para que cuando encontremos el bucket por otros medios,
	 * como ser la LRU, podamos encontrar la celda de la tabla
	 * hash en la que se encuentra.
	 */
	size_t cell_index;

	/*
	 * Siguiente y anterior valor en la lista doblemente
	 * enlazada de datos que han colisionado por
	 * ser mapeados a la misma celda de la tabla hash.
	 */
	struct bucket_t* next_value;
	struct bucket_t* prev_value;

	/*
	 * Usamos una lista doblemente enlazada para
	 * representar una cola de buckets ordenados
	 * por su última instancia de uso.
	 *
	 * De esa forma, está al frente el valor menos relevante
	 * y si necesitamos borrarlo lo podemos hacer.
	 *
	 * El siguiente bucket de la cola es aquel
	 * que se borraría después, y el anterior es aquel
	 * que se borraría antes.
	 */
	struct bucket_t* next_queue;
	struct bucket_t* prev_queue;
} bucket_t;

/*
 * Inicializa un bucket con una referenceia
 * y se le asigna los blobs de clave y valor.
 * 
 * Por cómo los blobs deben ser usados, esto significa que ahora
 * este bucket es dueño de los blobs, y por lo tanto esa memoria
 * ya no le pertenece a la entidad que llama a esta función, más
 * bien ahora tiene una referencia al bucket que las contiene.
 */
void bucket_init(bucket_t* bucket, blob_t key, blob_t value);

/*
 * Suma una referencia al bucket concurrentemente, el bucket
 * no puede morir por ser desreferenciado en otros lugares,
 * ya que le quedaría una referencia.
 */
void bucket_reference(bucket_t* bucket);

/*
 * Resta una referencia al bucket concurrentemente, el que lleve
 * la cantidad de referencias a 0 liberará la memoria de clave,
 * valor y el bucket en sí.
 * 
 * Debería ser un bucket que ya esté fuera de ambas estructuras
 * de datos, pero eso no es controlado en esta función.
 */
void bucket_dereference(bucket_t* bucket);

/*
 * Macro que evalúa al bucket, pero habiendo sumado una referencia.
 * 
 * Útil para usar en expresiones.
 */
#define SHARE(bucket) \
(bucket_reference(bucket), bucket)
