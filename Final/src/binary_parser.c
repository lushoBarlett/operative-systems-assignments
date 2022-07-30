#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>
#include <arpa/inet.h>
#include "binary_parser.h"

static int record_to_string(record_t record, char* str, int size) {
	return snprintf(
		str, size,
		"PUTS=%ld DELS=%ld GETS=%ld TAKES=%ld KEYS=%ld BYTES=%ld\n",
		record.puts,
		record.dels,
		record.gets,
		record.takes,
		record.keys,
		record.bytes
	);
}

static void reply_with_code(Code code, int fd) {
	write(fd, &code, 1);
}

static void reply_with_blob(blob_t blob, int fd) {
	char response[2048] = {Ok};

	int blob_bytes = htonl(blob.bytes);
	
	memcpy(response+1, (char*) &blob_bytes, 4);
	
	memcpy(response+5, blob.memory, blob.bytes);
	
	write(fd, response, blob.bytes+5);
}

static void reply_with_record(record_t record, int fd) {
	char response[2048];

	response[0] = Ok;

	int written_count = record_to_string(record, response+5, 2047);
	
	int len = ntohl(written_count);
	
	memcpy(response+1, &len, 4);

	if (written_count >= 2043)
		reply_with_code(Ebig, fd);
	else
		write(fd, response, written_count+5);
}

static void require_put(bin_state_machine_t* state_machine) {
	blob_t key_blob = {
		.memory = state_machine->key,
		.bytes = state_machine->key_len
	};

	blob_t value_blob = {
		.memory = state_machine->value,
		.bytes = state_machine->value_len
	};

	bucket_t* bucket = database_memsafe_malloc(state_machine->database, sizeof(bucket_t));

	bucket_init(bucket, key_blob, value_blob);

	database_put(state_machine->database, bucket);

	reply_with_code(Ok, state_machine->fd);
}

static void require_get(bin_state_machine_t* state_machine) {
	blob_t key_blob = {
		.memory = state_machine->key,
		.bytes = state_machine->key_len,
	};

	bucket_t* bucket = database_get(state_machine->database, key_blob);

	if (bucket) {
		reply_with_blob(bucket->value, state_machine->fd);

		bucket_dereference(bucket);
	} else {
		reply_with_code(Enotfound, state_machine->fd);
	}
}

static void require_take(bin_state_machine_t* state_machine) {
	blob_t key_blob = {
		.memory = state_machine->key,
		.bytes = state_machine->key_len
	};

	bucket_t* bucket = database_take(state_machine->database, key_blob);

	if (bucket) {
		reply_with_blob(bucket->value, state_machine->fd);

		bucket_dereference(bucket);
	} else {
		reply_with_code(Enotfound, state_machine->fd);
	}
}

static void require_del(bin_state_machine_t* state_machine) {
	Code code = Ok;
	blob_t key_blob = {
		.memory = state_machine->key,
		.bytes = state_machine->key_len
	};
	
	int found = database_delete(state_machine->database, key_blob);

	if (!found)
		code = Enotfound;
	
	reply_with_code(code, state_machine->fd);
}

static void require_stats(bin_state_machine_t* state_machine) {
	record_t record = database_stats(state_machine->database);

	reply_with_record(record, state_machine->fd);
}

static int is_key_state(InternalState state) {
	return ((state == ReadingKeyLength) || (state == ReadingKey));
}

static int is_length_state(InternalState state) {
	return ((state == ReadingKeyLength) || (state == ReadingValueLength));
}

static InternalState next_state(InternalState state) {
	if (state == ReadingKeyLength)
		return ReadingKey;
	if (state == ReadingValueLength)
		return ReadingValue;
	
	return state;
}

static int check_finished(bin_state_machine_t* state_machine, int n) {
	int finished = state_machine->read_characters == n;

	if (finished)
		state_machine->read_characters = 0;
	
	return finished;
}

// Returns 1 if could read all, -1 on error, and 0 if read less
static int read_n(bin_state_machine_t* state_machine, uint8_t* dest, int n) {

	uint8_t* current_dest = dest + state_machine->read_characters;

	uint8_t bytes_left = n - state_machine->read_characters;

	int current_read = read(state_machine->fd, current_dest, bytes_left);

	if (current_read < 0 && (errno == EAGAIN || errno == EWOULDBLOCK))
		return 0;

	if (current_read < 0)
		return -1;

	state_machine->read_characters += current_read;

	int ret = check_finished(state_machine, n);
	return ret;
}

static int get_length(bin_state_machine_t* state_machine, uint32_t* buf_len) {
	int finished = read_n(state_machine, (uint8_t*) buf_len, 4);

	if (finished > 0)
		*buf_len = ntohl(*buf_len);
	
	return finished;
}

static int parse_command(bin_state_machine_t* state_machine, uint8_t** buf, uint32_t* buf_len) {
	int finished;

	if (is_length_state(state_machine->state)) {
		finished = get_length(state_machine, buf_len);

		if (finished != 1)
			return finished;

		*buf = database_memsafe_malloc(state_machine->database, *buf_len);
		
		state_machine->state = next_state(state_machine->state);
	}

	finished = read_n(state_machine, *buf, *buf_len);	

	return finished;
}

static void bin_state_machine_reset(bin_state_machine_t* state_machine, Code old_code){
	if (old_code != Nothing)
		state_machine->code = Nothing;
	
	state_machine->state = ReadingKeyLength;
}

static int handle_nothing(bin_state_machine_t* state_machine) {
	uint8_t code;

	// TODO the success and return pattern may be extracted to a macro
	int success = read_n(state_machine, &code, 1);
	
	if (success > 0)
		state_machine->code = code;

	return success;
}

static int handle_one_argument_command(bin_state_machine_t* state_machine, void (*f)(bin_state_machine_t*)) {
	int success = parse_command(state_machine, &state_machine->key, &state_machine->key_len);
	if (success > 0) {
		f(state_machine);
		free(state_machine->key);
	}
		
	return success;
}

static int handle_put(bin_state_machine_t* state_machine) {
	if (is_key_state(state_machine->state)) {
		int success = parse_command(state_machine, &state_machine->key, &state_machine->key_len);

		if (success != 1)
			return success;

		state_machine->state = ReadingValueLength;
	}

	int success = parse_command(state_machine, &state_machine->value, &state_machine->value_len);

	if (success > 0)
		require_put(state_machine);

	return success;
}

static int handle_get(bin_state_machine_t* state_machine) {
	return handle_one_argument_command(state_machine, require_get);
}

static int handle_del(bin_state_machine_t* state_machine) {
	return handle_one_argument_command(state_machine, require_del);
}

static int handle_take(bin_state_machine_t* state_machine) {
	return handle_one_argument_command(state_machine, require_take);
}

static int handle_code(bin_state_machine_t* state_machine) {
	int ret = 1;

	switch (state_machine->code) {
	case Put:
		ret = handle_put(state_machine);
		break;
	case Get:
		ret = handle_get(state_machine);
		break;
	case Del:
		ret = handle_del(state_machine);
		break;
	case Take:
		ret = handle_take(state_machine);
		break;
	case Stats:
		require_stats(state_machine);
		break;
	case Nothing:
		ret = handle_nothing(state_machine);
		break;
	default:
		ret = -1;
	}

	return ret;
}

int bin_state_machine_advance(bin_state_machine_t* state_machine) {
	int ret = 1;

	Code old_code = state_machine->code;

	while (ret > 0) {
		ret = handle_code(state_machine);

		if (ret > 0)
			bin_state_machine_reset(state_machine, old_code);

		old_code = state_machine->code;
	}	

	return ret >= 0;
}

void bin_state_machine_init(bin_state_machine_t* state_machine, int fd, database_t* database) {
	state_machine->code = Nothing;
	state_machine->state = ReadingKeyLength;
	state_machine->key = state_machine->value = NULL;
	state_machine->read_characters = state_machine->key_len = state_machine->value_len = 0;
	state_machine->fd = fd;
	state_machine->database = database;
}
