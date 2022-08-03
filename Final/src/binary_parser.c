#include "binary_parser.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>
#include <arpa/inet.h>

/*
 * Representa el resultado de un procesamiento de input.
 * El autómata puede producir un error, puede quedarse a la mitad de alguna de sus etapas,
 * o puede encontrarse al principio, entre etapas, o haber finalizado. en cuyo caso está Idle.
 */
typedef enum {
	Error = -1,
	Processing = 0,
	Idle = 1,
} Result;

/*
 * Esta macro captura en un scope el resultado de una expresión que arroja un resultado
 * y en caso de producirse un error o no estar finalizada, habrá un retorno de dicho resultado.
 */
#define FINISH_OR_RETURN(N) \
{ \
	Result __succ_or_ret_expression_result = N; \
	if (__succ_or_ret_expression_result != Idle) \
		return __succ_or_ret_expression_result; \
}

static int record_to_string(record_t record, char* dest, int max_length) {
	return snprintf(
		dest, max_length,
		"PUTS=%ld DELS=%ld GETS=%ld TAKES=%ld KEYS=%ld BYTES=%ld\n",
		record.puts,
		record.dels,
		record.gets,
		record.takes,
		record.keys,
		record.bytes
	);
}

static void reply_with_code(Code code, int file_descriptor) {
	write(file_descriptor, &code, 1);
}

static void reply_with_blob(blob_t blob, int file_descriptor) {
	char response[5] = {Ok};

	uint32_t network_order_blob_bytes = htonl(blob.bytes);

	memcpy(response + 1, &network_order_blob_bytes, 4);

	write(file_descriptor, response, 5);
	write(file_descriptor, blob.memory, blob.bytes);
}

static void reply_with_record(record_t record, int file_descriptor) {
	/*
	 * Cada campo del record tiene como máximo 20 caracteres, y son 6
	 * por lo cual necesitamos al menos 120 caracteres, sumando los del mismo
	 * string de respuesta, el código y el largo, es imposible que esto supere
	 * 256 caracteres.
	 */
	char response[256] = {Ok};

	uint32_t record_length = record_to_string(record, response + 5, 251);

	uint32_t network_order_record_length = htonl(record_length);

	memcpy(response + 1, &network_order_record_length, 4);

	write(file_descriptor, response, record_length + 5);
}

static void put(bin_state_machine_t* state_machine) {
	blob_t key_blob = {
		.memory = state_machine->key,
		.bytes = state_machine->key_length
	};

	blob_t value_blob = {
		.memory = state_machine->value,
		.bytes = state_machine->value_length
	};

	bucket_t* bucket = database_memsafe_malloc(state_machine->database, sizeof(bucket_t));

	bucket_init(bucket, key_blob, value_blob);

	database_put(state_machine->database, bucket);

	reply_with_code(Ok, state_machine->file_descriptor);
}

static void get(bin_state_machine_t* state_machine) {
	blob_t key_blob = {
		.memory = state_machine->key,
		.bytes = state_machine->key_length,
	};

	bucket_t* bucket = database_get(state_machine->database, key_blob);

	if (bucket) {
		reply_with_blob(bucket->value, state_machine->file_descriptor);

		bucket_dereference(bucket);
	} else {
		reply_with_code(Enotfound, state_machine->file_descriptor);
	}
}

static void take(bin_state_machine_t* state_machine) {
	blob_t key_blob = {
		.memory = state_machine->key,
		.bytes = state_machine->key_length
	};

	bucket_t* bucket = database_take(state_machine->database, key_blob);

	if (bucket) {
		reply_with_blob(bucket->value, state_machine->file_descriptor);

		bucket_dereference(bucket);
	} else {
		reply_with_code(Enotfound, state_machine->file_descriptor);
	}
}

static void del(bin_state_machine_t* state_machine) {
	blob_t key_blob = {
		.memory = state_machine->key,
		.bytes = state_machine->key_length
	};
	
	if (database_delete(state_machine->database, key_blob))
		reply_with_code(Ok, state_machine->file_descriptor);
	else
		reply_with_code(Enotfound, state_machine->file_descriptor);
}

static void stats(bin_state_machine_t* state_machine) {
	record_t record = database_stats(state_machine->database);

	reply_with_record(record, state_machine->file_descriptor);
}

/*
 * Lee n caracteres en un buffer de destino, teniendo en cuenta
 * la cantidad de caracteres que ya han sido leídos hasta el momento.
 * Actualiza la máquina de estado con la nueva lectura realizada
 */
static Result read_n(bin_state_machine_t* state_machine, uint8_t* dest, size_t n) {
	uint8_t* current_dest = dest + state_machine->read_characters;

	uint8_t bytes_left = n - state_machine->read_characters;

	int current_read = read(state_machine->file_descriptor, current_dest, bytes_left);

	// TODO: handle error when EINTR
	if (current_read < 0 && (errno == EAGAIN || errno == EWOULDBLOCK))
		return Processing;

	if (current_read < 0)
		return Error;

	state_machine->read_characters += current_read;

	if (state_machine->read_characters < n)
		return Processing;
	
	state_machine->read_characters = 0;

	return Idle;
}

/*
 * Si nuestro estado es la lectura de código, se intentará leer un byte. 
 * Al lograrlo se avanza a la lectura de tamaño de clave y se finaliza la etapa.
 * 
 * En cualquier otro estado se considera que la etapa está completa.
 */
static Result parse_code(bin_state_machine_t* state_machine) {
	if (state_machine->state == ReadingCode) {
		FINISH_OR_RETURN(read_n(state_machine, (uint8_t*)&state_machine->code, 1));

		state_machine->state = ReadingKeyLength;
	}

	return Idle;
}

/*
 * Si nuestro estado es lectura de tamaño de clave, se intentarán leer cuatro bytes. 
 * Al lograrlo se avanza a la lectura de clave, reservando memoria suficiente
 * para almacenarla, y se finaliza la etapa.
 * 
 * En cualquier otro estado se considera que la etapa está completa.
 */
static Result parse_key_length(bin_state_machine_t* state_machine) {
	if (state_machine->state == ReadingKeyLength) {
		FINISH_OR_RETURN(read_n(state_machine, (uint8_t*)&state_machine->key_length, 4))

		state_machine->key_length = ntohl(state_machine->key_length);

		state_machine->key = database_memsafe_malloc(state_machine->database, state_machine->key_length);
		
		state_machine->state = ReadingKey;
	}

	return Idle;
}

/*
 * Si nuestro estado es lectura de clave, se intentarán lee la cantidad de bytes
 * especificada en el largo de clave. Al lograrlo se avanza a la lectura de tamaño de valor,
 * y se finaliza la etapa.
 * 
 * En cualquier otro estado se considera que la etapa está completa.
 * 
 * Nota: en varias ocasiones no necesitaremos leer un valor luego de una clave, pero esto
 * queda a cargo de mecanismos de control de más alto nivel de abstracción
 */
static Result parse_key(bin_state_machine_t* state_machine) {
	if (state_machine->state == ReadingKey) {
		FINISH_OR_RETURN(read_n(state_machine, state_machine->key, state_machine->key_length));

		state_machine->state = ReadingValueLength;
	}

	return Idle;
}

/*
 * Si nuestro estado es tamaño de valor, se intentarán leer cuatro bytes. 
 * Al lograrlo se avanza a la lectura de valor, reservando memoria suficiente
 * para almacenarl0, y se finaliza la etapa.
 * 
 * En cualquier otro estado se considera que la etapa está completa.
 */
static Result parse_value_length(bin_state_machine_t* state_machine) {
	if (state_machine->state == ReadingValueLength) {
		FINISH_OR_RETURN(read_n(state_machine, (uint8_t*)&state_machine->value_length, 4))

		state_machine->value_length = ntohl(state_machine->value_length);

		state_machine->value = database_memsafe_malloc(state_machine->database, state_machine->value_length);
		
		state_machine->state = ReadingValue;
	}

	return Idle;
}

/*
 * Si nuestro estado es lectura de valor, se intentarán lee la cantidad de bytes
 * especificada en el largo de valor. Al lograrlo se avanza de nuevo a la lectura de código,
 * habiendo concluido la lectura de un mensaje completo, y se finaliza la etapa.
 * 
 * En cualquier otro estado se considera que la etapa está completa.
 */
static Result parse_value(bin_state_machine_t* state_machine) {
	if (state_machine->state == ReadingValue) {
		FINISH_OR_RETURN(read_n(state_machine, state_machine->value, state_machine->value_length));

		state_machine->state = ReadingCode;
	}

	return Idle;
}

/*
 * Reinicia todos los valores que la máquina de estado utiliza para recordar
 * el estado de su proceso, liberando primero la memoria de la clave y el
 * valor si la hubiere.
 */
static void bin_state_machine_reset(bin_state_machine_t* state_machine) {
	state_machine->code = Nothing;
	state_machine->state = ReadingCode;

	free(state_machine->key);
	free(state_machine->value);

	state_machine->key = state_machine->value = NULL;

	state_machine->read_characters = state_machine->key_length = state_machine->value_length = 0;
}

/*
 * Interpreta una acción completa, de acuerdo al código de acción
 * que figure en la máquina de estado.
 * 
 * El caso donde no hay código cargado es especial, primero se lee
 * un código y luego se vuelve a intentar.
 * 
 * En cualquier otro caso, se intenta avanzar por las etapas
 * correspondientes a la acción, leyendo cada uno de los campos
 * especificados por el estándar.
 * 
 * En caso de no finalizar o haber un error retorna prematuramente.
 */
static Result dispatch_action(bin_state_machine_t* state_machine) {
	switch (state_machine->code) {

	case Nothing:
		FINISH_OR_RETURN(parse_code(state_machine));
		return dispatch_action(state_machine);

	case Put:
		FINISH_OR_RETURN(parse_key_length(state_machine));
		FINISH_OR_RETURN(parse_key(state_machine));
		FINISH_OR_RETURN(parse_value_length(state_machine));
		FINISH_OR_RETURN(parse_value(state_machine));
		put(state_machine);
		bin_state_machine_reset(state_machine);
		break;

	case Get:
		FINISH_OR_RETURN(parse_key_length(state_machine));
		FINISH_OR_RETURN(parse_key(state_machine));
		get(state_machine);
		bin_state_machine_reset(state_machine);
		break;

	case Del:
		FINISH_OR_RETURN(parse_key_length(state_machine));
		FINISH_OR_RETURN(parse_key(state_machine));
		del(state_machine);
		bin_state_machine_reset(state_machine);
		break;

	case Take:
		FINISH_OR_RETURN(parse_key_length(state_machine));
		FINISH_OR_RETURN(parse_key(state_machine));
		take(state_machine);
		bin_state_machine_reset(state_machine);
		break;

	case Stats:
		stats(state_machine);
		bin_state_machine_reset(state_machine);
		break;

	default:
		return Error;

	}

	return Idle;
}

int bin_state_machine_advance(bin_state_machine_t* state_machine) {
	Result result = dispatch_action(state_machine);

	/*
	 * Si hay error debemos liberar memoria, ya que el programa
	 * no la va a usar, y no tuvo oportunidad de liberarla.
	 */
	if (result == Error)
		bin_state_machine_reset(state_machine);

	return result != Error;
}

void bin_state_machine_init(bin_state_machine_t* state_machine, int file_descriptor, database_t* database) {
	bin_state_machine_reset(state_machine);

	state_machine->file_descriptor = file_descriptor;
	
	state_machine->database = database;
}
