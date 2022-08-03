#pragma once

#include "code.h"
#include "database.h"

/*
 * Representa el estado interno de la máquina de estado binaria.
 * Tenemos 4 estados posibles, tanto para una clave como para un valor
 * hay un estado de lectura del tamaño del dato como la lectura del dato en sí.
 */
typedef enum {
	ReadingCode,
	ReadingKeyLength,
	ReadingKey,
	ReadingValueLength,
	ReadingValue,
} InternalState;

/*
 * Máquina de estado binaria.
 * Interpreta el mensaje de un cliente binario.
 * 
 * Los datos que puede interpretar son:
 * - el código del comando pedido a la base de datos
 * - el largo de clave
 * - la clave
 * - el largo de valor
 * - el valor
 * 
 * Esos datos pueden quedar por la mitad si no hay suficientes
 * datos para leer y almacenar
 */
typedef struct bin_state_machine_t {
	InternalState state;

	Code code;

	uint32_t key_length;
	uint8_t* key;

	uint32_t value_length;
	uint8_t* value;

 	/*
	 * Almacena la cantidad de caracteres leídos hasta el momento
	 * si nos encontramos en medio de una lectura sin terminar
	 */
	size_t read_characters;

	int file_descriptor;

	database_t* database;

} bin_state_machine_t;

/*
 * Inicializa una máquina de estado binaria para lectura desde un file descriptor
 * y con una referencia a un database como su backend.
 * 
 * Inicializa el código en Nothing, y el estado inicial en lectura de largo de clave
 */
void bin_state_machine_init(bin_state_machine_t* state_machine, int file_descriptor, database_t* database);

/*
 * Avanza el procesamiento de una máquina de estado binaria hasta que ocurra
 * un error, no pueda procesar más datos, o termine 
 */
int bin_state_machine_advance(bin_state_machine_t* state_machine); 
