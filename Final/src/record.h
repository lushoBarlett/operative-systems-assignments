#pragma once

#include <stdint.h>

#include "counter64.h"

/*
 * Estadísticas sobre los accesos a la base
 * de datos, específicamente las cantidades de:
 * - inserciones
 * - borrados
 * - lecturas
 * - tomados
 * - pares clave-valor
 */
typedef struct record_t {
	uint64_t puts;
	uint64_t dels;
	uint64_t gets;
	uint64_t takes;
	uint64_t keys;
} record_t;

/*
 * Estadísticas sobre los accesos a la base
 * de datos, guardados como contadores concurrentes
 * actualizables, específicamente las cantidades de:
 * - inserciones
 * - borrados
 * - lecturas
 * - tomados
 */
typedef struct concurrent_record_t {
	counter64_t puts;
	counter64_t dels;
	counter64_t gets;
	counter64_t takes;
} concurrent_record_t;

/*
 * Inicializa los contadores del record concurrente.
 */
void record_init(concurrent_record_t* concurrent_record);

/*
 * Copia los valores actuales de los contadores al record
 * no concurrente, que se usa para repotar estado.
 * 
 * Además se añade un valor de pares clave-valor
 * para completar la información.
 */
record_t report(concurrent_record_t* concurrent_record, uint64_t keys);
