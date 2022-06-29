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
	uint64_t keys;
	uint64_t bytes;
} record_t;

typedef struct concurrent_record_t {
	counter64_t puts;
	counter64_t dels;
	counter64_t gets;
	counter64_t keys;
	counter64_t bytes;
} concurrent_record_t;

concurrent_record_t record_init(concurrent_record_t* crecord) {
	counter_init(&crecord->puts, 0);
	counter_init(&crecord->dels, 0);
	counter_init(&crecord->gets, 0);
	counter_init(&crecord->keys, 0);
	counter_init(&crecord->bytes, 0);
}

record_t report(concurrent_record_t* crecord) {
	return (record_t){
		.puts  = counter_get(&crecord->puts),
		.dels  = counter_get(&crecord->dels),
		.gets  = counter_get(&crecord->gets),
		.keys  = counter_get(&crecord->keys),
		.bytes = counter_get(&crecord->bytes)
	};
}
