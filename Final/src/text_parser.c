#include "text_parser.h"

#include <string.h>
#include <stdlib.h>
#include <assert.h>
#include <stdio.h>
#include <errno.h>
#include <unistd.h>
#include <ctype.h>

#include "database.h"
#include "blob.h"

int custom_write(int file_descriptor, char* buf, size_t count) {
	int written_count;

writing:
	written_count = write(file_descriptor, buf, count);

	if (written_count < 0 && (errno == EINTR))
		goto writing;
	
	if (written_count < 0)
		return 0;

	return 1;
}

static int reply_with_string(char* string, int length, int file_descriptor) {
	return custom_write(file_descriptor, string, length);
}

static int reply_with_blob(blob_t blob, int file_descriptor) {
	char response[2048] = "OK ";

	size_t limit = blob.bytes <= 2044 ? blob.bytes : 2044;

	char* memory = blob.memory;

	size_t i = 3;	

	for (size_t b = 0; b < limit; b++)
		response[i++] = isprint(memory[b]) ? memory[b] : '.';

	response[i++] = '\n';

	return custom_write(file_descriptor, response, i);
}

static int record_to_string(record_t record, char* buffer, int max_length) {
	return snprintf(
		buffer, max_length,
		"OK PUTS=%ld DELS=%ld GETS=%ld TAKES=%ld KEYS=%ld\n",
		record.puts,
		record.dels,
		record.gets,
		record.takes,
		record.keys
	);
}

static int reply_with_record(record_t record, int file_descriptor) {
	char response[256];

	int written_count = record_to_string(record, response, 256);

	return custom_write(file_descriptor, response, written_count);
}

static int put(database_t* database, const char* key, const char* value, int file_descriptor) {
	size_t key_size = strlen(key);
	size_t value_size = strlen(value);

	/*
	 * Se crea una copia de los datos para guardarlos en la base de datos.
	 * Si no hay espacio matamos el cliente.
	 */
	char* key_copy = database_memsafe_malloc(database, sizeof(char) * (key_size + 1));

	if (!key_copy)
		return 0;

	strcpy(key_copy, key);

	char* value_copy = database_memsafe_malloc(database, sizeof(char) * (value_size + 1));

	if (!value_copy) {
		free(key_copy);
		return 0;
	}

	strcpy(value_copy, value);

	blob_t key_blob = {
		.memory = key_copy,
		.bytes = key_size
	};

	blob_t value_blob = {
		.memory = value_copy,
		.bytes = value_size
	};

	bucket_t* bucket = database_memsafe_malloc(database, sizeof(bucket_t));

	if (!bucket) {
		free(key_copy);
		free(value_copy);
		return 0;
	}

	bucket_init(bucket, key_blob, value_blob);

	database_put(database, bucket);

	return reply_with_string("OK\n", 3, file_descriptor);
}

static int get(database_t* database, char* key, int file_descriptor) {
	blob_t key_blob = {
		.memory = key,
		.bytes = strlen(key)
	};

	bucket_t* bucket = database_get(database, key_blob);

	if (bucket) {
		int reply_success = reply_with_blob(bucket->value, file_descriptor);

		bucket_dereference(bucket);

		return reply_success;
	} else {
		return reply_with_string("ENOTFOUND\n", 10, file_descriptor);
	}
}

static int take(database_t* database, char* key, int file_descriptor) {
	blob_t key_blob = {
		.memory = key,
		.bytes = strlen(key)
	};

	bucket_t* bucket = database_take(database, key_blob);

	if (bucket) {
		int reply_success = reply_with_blob(bucket->value, file_descriptor);

		bucket_dereference(bucket);

		return reply_success;
	} else {
		return reply_with_string("ENOTFOUND\n", 10, file_descriptor);
	}
}

static int del(database_t* database, char* key, int file_descriptor) {
	blob_t key_blob = {
		.memory = key,
		.bytes = strlen(key)
	};
	
	if (database_delete(database, key_blob))
		return reply_with_string("OK\n", 3, file_descriptor);
	else
		return reply_with_string("ENOTFOUND\n", 10, file_descriptor);
}

static int stats(database_t* database, int file_descriptor) {
	record_t record = database_stats(database);

	return reply_with_record(record, file_descriptor);
}

/*
 * Intenta interpretar alguno de los nombres de acciones.

 * Al conseguirlo avanza el puntero pasado como argumento y retorna
 * el código correspondiente, sino no hace nada y retorna el código Nothing
 */
static Code parse_code(char** input) {
	if (!strncmp(*input, "STATS", 5)) {
		*input += 5;
		return Stats;

	} else if (!strncmp(*input, "PUT ", 4)) {
		*input += 4;
		return Put;

	} else if (!strncmp(*input, "GET ", 4)) {
		*input += 4;
		return Get;

	} else if (!strncmp(*input, "DEL ", 4)) {
		*input += 4;
		return Del;

	} else if (!strncmp(*input, "TAKE ", 5)) {
		*input += 5;
		return Take;
	}

	return Nothing;
}

/*
 * Indica dónde está el comienzo de palabra,
 * solamente en el caso en que luego siga otra
 * y avanza el puntero pasado como argumento.

 * En otro caso retorna NULL sin hacer nada más.
 */
static char* parse_word_and_space(char** input) {
	char* start = *input;
	char* space = strchr(*input, ' ');

	if (!space)
		return NULL;

	*space = 0;

	*input = space + 1;

	return start;
}

/*
 * Indica dónde está el comienzo de la línea,
 * solamente en el caso en que no haya un espacio antes
 * y avanza el puntero pasado como argumento.

 * En otro caso retorna NULL sin hacer nada más.
 */
static char* parse_word_and_end(char** input) {
	char* start = *input;
	char* space = strchr(*input, ' ');

	if (space)
		NULL;

	char* end = strchr(*input, '\0');

	*input = end + 1;

	return start;
}

/*
 * Interpreta una acción completa, de acuerdo al código de acción
 * que figure en el buffer de la máquina de estado.
 * 
 * El caso donde no se identifique ningún código retorna como 0.
 * 
 * En cualquier otro caso, se intenta avanzar por las etapas
 * correspondientes a la acción, leyendo cada uno de los campos
 * especificados por el estándar.
 * 
 * En caso de un error de interpretación, retorna prematuramente con 0.
 */
static int parse_line(text_state_machine_t* state_machine) {
	
	char* input = state_machine->buffer;
	char* key;
	char* value;
	
	switch (parse_code(&input)) {

	case Nothing:
		return 0;

	case Put:
		if (!(key = parse_word_and_space(&input)))
			return 0;

		if (!(value = parse_word_and_end(&input)))
			return 0;

		if (!put(state_machine->database, key, value, state_machine->file_descriptor))
			return -1;
		break;

	case Get:
		if (!(key = parse_word_and_end(&input)))
			return 0;
		
		if (!get(state_machine->database, key, state_machine->file_descriptor))
			return -1;
		break;

	case Del:
		if (!(key = parse_word_and_end(&input)))
			return 0;

		if (!del(state_machine->database, key, state_machine->file_descriptor))
			return -1;
		break;

	case Take:
		if (!(key = parse_word_and_end(&input)))
			return 0;

		if (!take(state_machine->database, key, state_machine->file_descriptor))
			return -1;
		break;

	case Stats:
		if (!stats(state_machine->database, state_machine->file_descriptor))
			return -1;
		break;

	default:
		return 0;
	}

	return 1;
}

/*
 * Borra la línea al principio del buffer y mueve el resto de los caracteres
 * no leídos al principio
 */
static void remove_line(text_state_machine_t* state_machine, size_t line_length) {
	if (state_machine->read_characters > line_length)
		memmove(state_machine->buffer, state_machine->buffer + line_length, state_machine->read_characters - line_length);

	state_machine->read_characters -= line_length;
}

/*
 * Lee todos los caracteres que pueda en el buffer, teniendo en cuenta
 * la cantidad de caracteres que ya han sido leídos hasta el momento.
 * Actualiza la máquina de estado con la nueva lectura realizada
 * 
 * Si pudo leer algo, retorna 1.
 * Si no había nada por leer retorna 0.
 * En caso de error retorna -1.
 */
static int read_stream(text_state_machine_t* state_machine) {
	char* current_dest = state_machine->buffer + state_machine->read_characters;

	/*
	 * Podemos leer el maximo menos 1 porque debemos reservar espacio
	 * para \0 al final
	 * Si no lo hacemos, cuando buscamos el final de una línea (\n) y no
	 * haya salto de línea leerá basura.
	 */
	int chars_left = (MAX_BUFFER_SIZE - 1) - state_machine->read_characters;

	int current_read;

reading:
	current_read = read(state_machine->file_descriptor, current_dest, chars_left);

	/*
	 * Si el read falló por una señal antes de llegar a leer algo
	 * vuelve a realizar el read
	 */
	if (current_read < 0 && (errno == EINTR))
		goto reading;

	if (current_read < 0 && (errno == EAGAIN || errno == EWOULDBLOCK))
		return 0;

	if (current_read < 0)
		return -1;

	state_machine->read_characters += current_read;

	state_machine->buffer[state_machine->read_characters] = 0;

	return 1;
}

static int buffer_is_full(text_state_machine_t* state_machine) {
	return state_machine->read_characters == MAX_BUFFER_SIZE;
}

/*
 * Indica dónde está el final de la línea, y la marca
 * como final de string. Retorna el tamaño de la línea sumado al caracter final
 */
static int line_end(text_state_machine_t* state_machine) {
	char* end_of_line = strchr(state_machine->buffer, '\n');

	if (!end_of_line)
		return 0;

	*end_of_line = 0;

	return end_of_line - state_machine->buffer + 1;
}

int text_state_machine_advance(text_state_machine_t* state_machine) {
	int keep_reading = 1;

	while (keep_reading > 0) {

		/*
		 * Si al intentar leer hubo error es irrecuperable y retorno 0.
		 * Si no había nada que leer retorno 1.
		 * Si anduvo correctamente sigo ejecutando
		 */
		int read_result = read_stream(state_machine);

		if (read_result == -1)
			return 0;

		if (read_result == 0)
			return 1;

		/*
		 * Guardo si el buffer se llenó al leer, lo que indica que
		 * debería volver a leer más.
		 */
		int filled_buffer = buffer_is_full(state_machine);

		size_t line_length;

		/*
		 * Por cada línea que identifiquemos en nuestro buffer vamos a intentar
		 * interpretarla, y de no poder hacerlo responder con mensaje inválido.
		 *
		 * En cualquier caso removemos la línea del buffer para continuar con
		 * las que queden.
		 */
		while ((line_length = line_end(state_machine))) {
			int parse_line_result = parse_line(state_machine);
			
			/*
			 * Si ocurrió un error irrecuperable al responder al cliente
			 * retornamos 0, lo que matará la conexión.
			 */
			if (parse_line_result == -1)
				return 0;

			if (parse_line_result == 0)
				reply_with_string("EINVAL\n", 7, state_machine->file_descriptor);

			remove_line(state_machine, line_length);
		}

		/*
		 * Si el buffer está lleno después de haber intentado interpretar
		 * líneas, significa que el mensaje es muy grande y esto
		 * es un error irrecuperable
		 * Si no, si antes de parsear líneas se había llenado, seguiremos
		 * leyendo. Porque pudo haber un mensaje que haya quedado sin leer
		 * por no haber espacio en el buffer.
		 */
		if (buffer_is_full(state_machine))
			return 0;
		else
			keep_reading = filled_buffer;
	}
	return 1;
}

void text_state_machine_init(text_state_machine_t* state_machine, int file_descriptor, database_t* database) {
	state_machine->read_characters = 0;

	state_machine->file_descriptor = file_descriptor;

	state_machine->database = database;
}
