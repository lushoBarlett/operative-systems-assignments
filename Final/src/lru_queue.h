#pragma once

#include "bucket.h"

#include <pthread.h>

/*
 * La LRU es una cola implementadad como una lista
 * doblemente enlazada, con puntero al principio y al final.
 * 
 * Es usada para tener de forma tal que en el frente están
 * los valores que fueron usados más en el pasado, y atrás
 * los valores que fueron usados más en el presente.
 * 
 * Tiene un lock para mantener exclusividad en toda la LRU.
 */
typedef struct {
	bucket_t* back;
	bucket_t* front;
	pthread_mutex_t lock;
} lru_queue_t;

/*
 * Inicializa el lock y setea front y back en NULL.
 */
void lru_queue_init(lru_queue_t* lru_queue);

/*
 * Bloquea la LRU
 */
void lru_queue_lock(lru_queue_t* lru_queue);

/*
 * Desbloquea la LRU
 */
void lru_queue_unlock(lru_queue_t* lru_queue);

/*
 * Predicado para saber si un bucket se encuentra en la LRU
 *
 * Puede no estar si fue borrado previamente por otro hilo,
 * o si todavía no fue insertado en la LRU.
 * 
 * Todos los buckets de la base de datos deberían estar en la LRU a priori.
 * 
 * Corre en O(1).
 */
int lru_in_queue(lru_queue_t* lru_queue, bucket_t* bucket);

/*
 * Inserta un bucket que no estaba en la LRU, atrás, o sea al final de la cola.
 * 
 * No suma ninguna referencia al bucket,
 * esto se debe hacer antes de llamar a la función.
 * 
 * Corre en O(1).
 */
void lru_queue_enqueue(lru_queue_t* lru_queue, bucket_t* bucket);

/*
 * Quita un bucket de la LRU, por el frente, o sea del principio de la cola.
 * 
 * De no haber buckets en la LRU retorna NULL.
 * 
 * Retorna el bucket con la misma cantidad de referencias, ya que
 * borrar de la LRU resta una, pero devolverla como resultado suma una.
 * 
 * Corre en O(1).
 */
bucket_t* lru_queue_dequeue(lru_queue_t* lru_queue);

/*
 * Borra en O(1) el bucket de la LRU, y resta una referencia al mismo,
 * no importa en qué lugar de la cola se encuentre, incluso al principio o al final.
 *
 * Un comportamiento secundario de esta función es que si el bucket
 * no se encuentra en la LRU porque ya fue borrado, o por
 * no haber sido introducido todavía, la función no tiene
 * ningún otro efecto más que restar una referencia al bucket.
 */
void lru_queue_delete(lru_queue_t* lru_queue, bucket_t* bucket);

/*
 * Realiza un delete y un enqueue en ese orden, por lo que
 * corre en O(1), pero sin cambiar la cantidad de referencias que el bucket tiene.
 * 
 * Un comportamiento secundario de esta función es que si el bucket
 * no se encuentra en la LRU porque ya fue borrado, o por
 * no haber sido introducido todavía, la función hace un enqueue
 * sin sumar una referencia como es debido.
 */
void lru_queue_reenqueue(lru_queue_t* lru_queue, bucket_t* bucket);
