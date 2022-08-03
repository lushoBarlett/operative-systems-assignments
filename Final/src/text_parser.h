#pragma once

#include "code.h"
#include "database.h"

#define MAX_BUFFER_SIZE 2048

/*
 * Máquina de estado de texto.
 * Interpreta el mensaje de un cliente de texto.
 * 
 * Los datos que puede interpretar son:
 * - el código del comando pedido a la base de datos
 * - la clave
 * - el valor
 * 
 * Esos datos pueden quedar por la mitad si no hay suficientes
 * datos para leer y almacenar, en el buffer con un máximo de
 * MAX_BUFFER_SIZE caracteres.
 * 
 * En el caso de terminarse una linea, y que hayamos leído
 * el comienzo de otras estos caracteres se moverán
 * al principio del buffer luego de procesar el mensaje anterior.
 */
typedef struct text_state_machine_t {
	char buffer[MAX_BUFFER_SIZE];

 	/*
	 * Almacena la cantidad de caracteres leídos
	 * que no fueron procesados todavía. O sea
	 * la cantidad de caracteres en el buffer.
	 */
	size_t read_characters;

	int file_descriptor;

	database_t* database;

} text_state_machine_t;

/*
 * Inicializa una máquina de estado de texto para lectura desde un file descriptor
 * y con una referencia a un database como su backend.
 */
void text_state_machine_init(text_state_machine_t* state_machine, int file_descriptor, database_t* database);

/*
 * Avanza el procesamiento de una máquina de estado de texto hasta que ocurra
 * un error, no pueda procesar más datos, o termine 
 */
int text_state_machine_advance(text_state_machine_t* state_machine);
