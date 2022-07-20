#pragma once

#include <stdint.h>

#include "counter64.h"

/*
 * Estadísticas sobre los accesos a la base
 * de datos, específicamente las cantidades de:
 * - inserciones
 * - borrados
 * - lecturas
 * - pares clave-valor
 * - bytes utilizados por los valores almacenados
 */
typedef struct record_t {
	uint64_t puts;
	uint64_t dels;
	uint64_t gets;
	uint64_t takes;
	uint64_t keys;
	uint64_t bytes;
} record_t;

typedef struct concurrent_record_t {
	counter64_t puts;
	counter64_t dels;
	counter64_t gets;
	counter64_t takes;
	counter64_t keys;
	counter64_t bytes;
} concurrent_record_t;

void record_init(concurrent_record_t* concurrent_record);

record_t report(concurrent_record_t* concurrent_record);
