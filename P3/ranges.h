#pragma once

#include <stdlib.h>

/*
 * Inicio y final de un rango de enteros
 * sobre los cuales un único thread
 * tiene que realizar cómputo
 */
typedef struct range_t {
	int beg;
	int end;
} range_t;

/*
 * Particiona un rango en un array de rangos contiguos,
 * cuya memoria es reservada, de acuerdo a la cantidad de threads
 */
range_t* make_ranges(size_t threads, int beg, int end);

/*
 * Devuelve el inicio del i-ésimo rango
 */
int beg(range_t* ranges, int i);

/*
 * Devuelve el final del i-ésimo rango
 */
int end(range_t* ranges, int i);

/*
 * Devuelve el tamaño del i-ésimo rango
 */
int size(range_t* ranges, int i);

/*
 * Libera la memoria del array de rangosLibera la memoria del array de rangos
 */
void free_ranges(range_t* ranges);
