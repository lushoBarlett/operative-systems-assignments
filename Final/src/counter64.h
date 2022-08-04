#pragma once

#include <stdint.h>
#include <pthread.h>

/*
 * Contador de 64 bits concurrente
 * con un lock general para sumar y restar.
 */
typedef struct counter64_t {
	uint64_t value;	
	pthread_mutex_t lock;
} counter64_t;

/*
 * Inicializa el contador con el valor provisto, y el lock
 */
void counter_init(counter64_t* counter, uint64_t value);

/*
 * Añade la cantidad especificada al contador y retorna
 * el valor que resultó de la operación, que puede ser
 * distinto al valor más actual, por concurrencia.
 * 
 * No chequea overflow.
 */
uint64_t counter_add(counter64_t* counter, uint64_t amount);

/*
 * Resta la cantidad especificada al contador y retorna
 * el valor que resultó de la operación, que puede ser
 * distinto al valor más actual, por concurrencia.
 * 
 * No revisa underflow.
 */
uint64_t counter_sub(counter64_t* counter, uint64_t amount);

/*
 * Suma uno al contador y retorna
 * el valor que resultó de la operación, que puede ser
 * distinto al valor más actual, por concurrencia.
 * 
 * No chequea overflow.
 */
uint64_t counter_increment(counter64_t* counter);

/*
 * Resta uno al contador y retorna
 * el valor que resultó de la operación, que puede ser
 * distinto al valor más actual, por concurrencia.
 * 
 * No revisa underflow.
 */
uint64_t counter_decrement(counter64_t* counter);

/*
 * Lee el actual valor del contador, que inmediatamente
 * después de retorna puede ser invalidado, por concurrencia.
 */
uint64_t counter_get(counter64_t* counter);
