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

#define MAX_BUF_SIZE 2048

void txt_state_machine_init(txt_state_machine_t* state_machine, int fd, database_t* database) {
	state_machine->read_characters = 0;
	state_machine->fd = fd;
	state_machine->database = database;
}

static void reply_with_str(char* str, int str_len, int fd) {
	write(fd, str, str_len);
}

static void reply_with_blob(blob_t value, int fd) {
	char response[2048] = { 'O', 'K', ' ' };

	size_t limit = value.bytes <= 2044 ? value.bytes : 2044;

	char* memory = value.memory;

	size_t i = 3;	

	for (size_t b = 0; b < limit; b++)
		response[i++] = isprint(memory[b]) ? memory[b] : '.';

	response[i++] = '\n';

	write(fd, response, i);
}

static int record_to_string(record_t record, char* str, int size) {
	return snprintf(
		str, size,
		"OK PUTS=%ld DELS=%ld GETS=%ld KEYS=%ld BYTES=%ld\n",
		record.puts,
		record.dels,
		record.gets,
		record.keys,
		record.bytes
	);
}

static void reply_with_record(record_t record, int fd) {
	char response[2048];

	int written_count = record_to_string(record, response, 2048);

	if (written_count >= 2048)
		reply_with_str("EBIG\n", 4, fd);
	else
		write(fd, response, written_count);
}

static void require_put(database_t* database, char* key, char* value, int fd) {
	blob_t key_blob = {
		.memory = key,
		.bytes = strlen(key)
	};

	blob_t value_blob = {
		.memory = value,
		.bytes = strlen(value)
	};

	bucket_t* bucket = database_memsafe_malloc(database, sizeof(bucket_t));

	bucket_init(bucket, key_blob, value_blob);

	database_put(database, bucket);

	reply_with_str("OK\n", 3, fd);
}

static void require_get(database_t* database, char* key, int fd) {
	blob_t key_blob = {
		.memory = key,
		.bytes = strlen(key)
	};

	bucket_t* bucket = database_get(database, key_blob);

	if (bucket) {
		reply_with_blob(bucket->value, fd);

		bucket_dereference(bucket);
	} else {
		reply_with_str("ENOTFOUND\n", 10, fd);
	}
}

static void require_take(database_t* database, char* key, int fd) {
	blob_t key_blob = {
		.memory = key,
		.bytes = strlen(key)
	};

	bucket_t* bucket = database_take(database, key_blob);

	if (bucket) {
		reply_with_blob(bucket->value, fd);

		bucket_dereference(bucket);
	} else {
		reply_with_str("ENOTFOUND\n", 10, fd);
	}
}

static void require_del(database_t* database, char* key, int fd) {
	blob_t key_blob = {
		.memory = key,
		.bytes = strlen(key)
	};
	
	int found = database_delete(database, key_blob);

	if (found)
		reply_with_str("OK\n", 3, fd);
	else
		reply_with_str("ENOTFOUND\n", 10, fd);
}

static void require_stats(database_t* database, int fd) {
	record_t record = database_stats(database);

	reply_with_record(record, fd);
}

// Returns 0 when no command matches, otherwise returns command length
static int set_command(char* buf, Code* code) {
	int command_length = 0;

	if (!strcmp(buf, "STATS")) {
		*code = Stats;
		command_length = 5;

	} else if (!strncmp(buf, "PUT ", 4)) {
		*code = Put;
		command_length = 4;

	} else if (!strncmp(buf, "GET ", 4)) {
		*code = Get;
		command_length = 4;

	} else if (!strncmp(buf, "DEL ", 4)) {
		*code = Del;
		command_length = 4;

	} else if (!strncmp(buf, "TAKE ", 5)) {
		*code = Take;
		command_length = 5;
	}

	return command_length;
}

static int get_key_length(char* key) {
	char* next_space = strchr(key, ' ');

	if (!next_space)
		return 0;

	return next_space - key;
}

static char* create_string_from(database_t* database, char* string, int length) {
	char* result = database_memsafe_malloc(database, sizeof(char) * (length + 1));

	strncpy(result, string, length);

	return result;
}

static char* require_last_key(database_t* database, char* buffer, int read_chars, int length) {
	char* key_ref = buffer + read_chars;

	if (strchr(key_ref, ' '))
		return NULL;

	int key_length = length - read_chars;

	return create_string_from(database, key_ref, key_length);
}

static void string_move_to_beginning(char* string, int offset, int length) {
	memmove(string, string + offset, length - offset);
}

static void handle_put(txt_state_machine_t* state_machine, int read_chars, int buf_len) {
	if (read_chars >= buf_len)
		return;

	char* key_ref = state_machine->buf + read_chars;

	int key_length = get_key_length(key_ref);

	read_chars += key_length + 1;

	if (!key_length || read_chars >= buf_len)
		return;

	char* value = require_last_key(state_machine->database, state_machine->buf, read_chars, buf_len);

	if (!value)
		return;

	char* key = create_string_from(state_machine->database, key_ref, key_length);

	require_put(state_machine->database, key, value, state_machine->fd);
}

static void handle_one_argument_command(txt_state_machine_t* state_machine, int read_chars, int buf_len, command_handler f) {
	if (read_chars >= buf_len)
		return;

	char* key = require_last_key(state_machine->database, state_machine->buf, read_chars, buf_len);

	if (key)
		f(state_machine->database, key, state_machine->fd);
}

static void parse_line(txt_state_machine_t* state_machine, int buf_len) {
	
	Code code;
	
	int read_chars = set_command(state_machine->buf, &code);

	if (!read_chars)
		return;

	switch (code) {
	case Put:
		handle_put(state_machine, read_chars, buf_len);
		break;

	case Get:
		handle_one_argument_command(state_machine, read_chars, buf_len, require_get);
		break;

	case Del:
		handle_one_argument_command(state_machine, read_chars, buf_len, require_del);
		break;

	case Take:
		handle_one_argument_command(state_machine, read_chars, buf_len, require_take);
		break;

	case Stats:
		require_stats(state_machine->database, state_machine->fd);
		break;

	default:
	 	// TODO: handle error cases
		abort();
	}
}

static void delete_first_line(txt_state_machine_t* state_machine, int line_length) {
	if (state_machine->read_characters > line_length)
		string_move_to_beginning(state_machine->buf, line_length, state_machine->read_characters);

	state_machine->read_characters -= line_length;
}

static int parse_recv(txt_state_machine_t* state_machine) {
	char* new_line = strchr(state_machine->buf, '\n');

	if (!new_line)
		return (state_machine->read_characters < MAX_BUF_SIZE);

	*new_line = 0;

	int line_length = new_line - state_machine->buf + 1;

	parse_line(state_machine, line_length - 1);

	delete_first_line(state_machine, line_length);

	if (state_machine->read_characters)
		parse_recv(state_machine);

	return 1;
}

int can_read_txt(txt_state_machine_t* state_machine) {
	char* dest = state_machine->buf + state_machine->read_characters;

	int max_to_read = MAX_BUF_SIZE - state_machine->read_characters;

	int current_read = read(state_machine->fd, dest, max_to_read);

	// TODO: handle error when EINTR
	if (current_read < 0 && (errno == EAGAIN || errno == EWOULDBLOCK))
		return 1;

	if (current_read < 0)
		return 0;

	state_machine->read_characters += current_read;

	int buf_is_full = state_machine->read_characters == MAX_BUF_SIZE;

	int parse_ret = parse_recv(state_machine);

	if (!parse_ret)
		reply_with_str("EINVAL\n", 7, state_machine->fd);

	if (buf_is_full)
		can_read_txt(state_machine);

	return 1;
}