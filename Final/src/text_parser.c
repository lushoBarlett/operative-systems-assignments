#include <string.h>
#include <stdlib.h>
#include <assert.h>
#include <stdio.h>
#include <errno.h>
#include <unistd.h>
#include <ctype.h>
#include "database.h"
#include "text_parser.h"
#include "blob.h"
#define MAX_BUF_SIZE 2048

// TODO: dereference buckets returned by db interface
// TODO: reply einval when a line isn't valid

void txt_state_machine_init(txt_state_machine_t* state_machine, int fd, database_t* db) {
	state_machine->read_characters = 0;
	state_machine->fd = fd;
	state_machine->db = db;
}

static void reply_with_code(enum code code, int fd) {
	char response[2] = {code, '\n'};

	write(fd, response, 2);
}

static void reply_with_blob(const blob_t value, int fd) {
	char* printable_value = blob_to_printable(&value);

	char response[2048];

	snprintf(response, 2048, "OK %s\n", printable_value);

	write(fd, response, value.bytes + 4);
}

static int record_to_string(record_t record, char* str, int size) {
	uint64_t puts = record.puts;
	uint64_t dels = record.dels;
	uint64_t gets = record.gets;
	uint64_t keys = record.keys;
	uint64_t bytes = record.bytes;

	return snprintf(str, size, "OK PUTS=%ld DELS=%ld GETS=%ld KEYS=%ld BYTES=%ld\n", puts, dels, gets, keys, bytes);
}

static void reply_with_record(record_t record, int fd) {
	char response[2048];

	int written_count = record_to_string(record, response, 2048);

	if (written_count >= 2048)
		reply_with_code(EBIG, fd);
	else
		write(fd, response, written_count);
}

static void require_put(database_t* db, char* key, char* value, int fd) {
	blob_t key_blob = blob_create(key, sizeof(key));
	blob_t value_blob = blob_create(value, sizeof(value));

	db_put(db, &key_blob, &value_blob);
	reply_with_code(OK, fd);
}

static void require_get(database_t* db, char* key, int fd) {
	blob_t key_blob = blob_create(key, sizeof(key));
	const bucket_t* bucket = db_get(db, &key_blob);

	if (bucket)
		reply_with_blob(bucket->value, fd);
	else
		reply_with_code(ENOTFOUND, fd);
}

static void require_take(database_t* db, char* key, int fd) {
	blob_t key_blob = blob_create(key, sizeof(key));
	const bucket_t* bucket = db_take(db, &key_blob);

	if (bucket)
		reply_with_blob(bucket->value, fd);
	else
		reply_with_code(ENOTFOUND, fd);
}

static void require_del(database_t* db, char* key, int fd) {
	blob_t key_blob = blob_create(key, sizeof(key));
	
	int found = db_delete(db, &key_blob);

	if (found)
		reply_with_code(OK, fd);
	else
		reply_with_code(ENOTFOUND, fd);
}

static void require_stats(database_t* db, int fd) {
	record_t record = db_stats(db);
	reply_with_record(record, fd);
}

// Returns 0 when no command matches, otherwise returns command length
static int set_command(char* buf, enum Command* command) {
	int command_length = 0;

	if (!strcmp(buf,"STATS")) {
		*command = Stats;
		command_length = 5;

	} else {

		if (!strncmp(buf, "PUT ", 4)) {
			*command = Put;
			command_length = 4;

		} else if (!strncmp(buf, "GET ", 4)) {
			*command = Get;
			command_length = 4;

		} else if (!strncmp(buf, "DEL ", 4)) {
			*command = Del;
			command_length = 4;

		} else if (!strncmp(buf, "TAKE ", 5)) {
			*command = Take;	
			command_length = 5;
		}
	}

	return command_length;
}

static char* find_next_space(char* buf) {
	return strchr(buf, ' ');
}

static int distance_between_ptrs(char* start, char* ending) {
	return (int) (ending - start);
}

static int get_key_length(char* key) {
	char* next_space = find_next_space(key);

	if (!next_space)
		return 0;

	return distance_between_ptrs(key, next_space);
}

static char* create_str(char* str, int str_len) {
	char* result = malloc(sizeof(char)*(str_len+1));

	strncpy(result, str, str_len);

	return result;
}

static char* require_last_key(char* buf, int read_chars, int buf_len) {
	char* key_ref = buf + read_chars;

	if (find_next_space(key_ref))
		return NULL;

	int key_length = buf_len - read_chars;

	char* key = create_str(key_ref, key_length);

	return key;
}

static void str_move_to_beginning(char* buf, int src_idx, int buf_len) {
	memmove(buf, buf + src_idx, buf_len - src_idx);
}

static void handle_put(txt_state_machine_t* sm, int read_chars, int buf_len) {
	if (read_chars >= buf_len)
		return;

	char* key_ref = sm->buf + read_chars;

	int key_length = get_key_length(key_ref);

	read_chars += key_length+1;

	if (!key_length || read_chars >= buf_len)
		return;

	char* value = require_last_key(sm->buf, read_chars, buf_len);

	if (!value)
		return;

	char* key = create_str(key_ref, key_length);

	require_put(sm->db, key, value, sm->fd);
}

static void handle_one_argument_command(txt_state_machine_t* sm, int read_chars, int buf_len, command_handler f) {
	if (read_chars >= buf_len)
		return;

	char* key = require_last_key(sm->buf, read_chars, buf_len);

	if (key)
		f(sm->db, key, sm->fd);
}

static void parse_line(txt_state_machine_t* sm, int buf_len) {
	enum Command command;
	
	int read_chars = set_command(sm->buf, &command);

	if (!read_chars)
		return;

	switch (command) {
	case Put:
		handle_put(sm, read_chars, buf_len);
		break;

	case Get:
		handle_one_argument_command(sm, read_chars, buf_len, require_get);
		break;

	case Del:
		handle_one_argument_command(sm, read_chars, buf_len, require_del);
		break;

	case Take:
		handle_one_argument_command(sm, read_chars, buf_len, require_take);
		break;

	case Stats:
		require_stats(sm->db, sm->fd);
		break;
	}
}

static void delete_first_line(txt_state_machine_t* state_machine, int line_length) {
	if (state_machine->read_characters > line_length)
		str_move_to_beginning(state_machine->buf, line_length, state_machine->read_characters);

	state_machine->read_characters -= line_length;
}

static int parse_recv(txt_state_machine_t* state_machine) {
	char* new_line = strchr(state_machine->buf, '.');

	if (!new_line)
		return (state_machine->read_characters < MAX_BUF_SIZE);

	*new_line = 0;

	int line_length = distance_between_ptrs(state_machine->buf, new_line) + 1;

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

	if (current_read < 0) {
		return 0;
	}

	state_machine->read_characters += current_read;

	int buf_is_full = state_machine->read_characters == MAX_BUF_SIZE;

	int parse_ret = parse_recv(state_machine);

	if (!parse_ret)
		reply_with_code(EINVAL, state_machine->fd);

	if (buf_is_full)
		can_read_txt(state_machine);

	return 1;
}