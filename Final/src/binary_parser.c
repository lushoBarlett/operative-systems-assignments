#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>
#include <arpa/inet.h>
#include "binary_parser.h"

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

	return check_finished(state_machine, n);
}

static int get_length(bin_state_machine_t* state_machine) {
	int finished = read_n(state_machine, (uint8_t*) &state_machine->arg_len, 4);

	if (finished > 0)
		state_machine->arg_len = ntohl(state_machine->arg_len);
	
	return finished;
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

static int parse_command(bin_state_machine_t* state_machine, uint8_t** buf) {
	int finished;

	if (is_length_state(state_machine->state)) {
		finished = get_length(state_machine);

		if (finished != 1)
			return finished;

		*buf = malloc(state_machine->arg_len);

		state_machine->state = next_state(state_machine->state);
	}

	finished = read_n(state_machine, *buf, state_machine->arg_len);	

	return finished;
}

void require_put() {
	printf("requiring put...\n");
}

void require_get() {
	printf("requiring get...\n");
}

void require_take() {
	printf("requiring take...\n");
}

void require_del() {
	printf("requiring del...\n");
}

void require_stats() {
	printf("requiring stats...\n");
}

int state_machine_advance(bin_state_machine_t* state_machine) {
	int ret = 1;

	switch (state_machine->code) {
	case PUT:
		ret = handle_put(state_machine);
		break;
	case GET:
		ret = handle_get(state_machine);
		break;
	case DEL:
		ret = handle_del(state_machine);
		break;
	case TAKE:
		ret = handle_take(state_machine);
		break;
	case STATS:
		handle_stats(state_machine);
		break;
	}

	if (ret > 0)
		ret = handle_nothing(state_machine);

	return ret;
}

int handle_nothing(bin_state_machine_t* state_machine) {
	uint8_t code;

	// TODO the success and return pattern may be extracted to a macro
	int success = read_n(state_machine, &code, 1);
	
	if (success != 1)
		return success;

	state_machine->code = code;

	return state_machine_advance(state_machine);
}

static int handle_one_argument_command(bin_state_machine_t* state_machine, void (*f)()) {
	int success = parse_command(state_machine, &state_machine->key);
	if (success != 1)
		return success;
	f();
	return success;
}

static int is_key_state(InternalState state) {
	return ((state == ReadingKeyLength) || (state == ReadingKey));
}

int handle_put(bin_state_machine_t* state_machine) {
	if (is_key_state(state_machine->state)) {
		int success = parse_command(state_machine, &state_machine->key);

		if (success != 1)
			return success;

		state_machine->state = ReadingValueLength;
	}

	int success = parse_command(state_machine, &state_machine->value);

	if (success > 0)
		require_put();

	return success;
}

int handle_get(bin_state_machine_t* state_machine) {
	return handle_one_argument_command(state_machine, require_get);
}

int handle_del(bin_state_machine_t* state_machine) {
	return handle_one_argument_command(state_machine, require_del);
}

int handle_take(bin_state_machine_t* state_machine) {
	return handle_one_argument_command(state_machine, require_take);
}

void handle_stats(bin_state_machine_t* state_machine) {
	require_stats();
}

void state_machine_init(bin_state_machine_t* state_machine, int fd) {
	state_machine->code = NOTHING;
	state_machine->state = ReadingKeyLength;
	state_machine->key = state_machine->value = NULL;
	state_machine->read_characters = state_machine->arg_len = 0;
	state_machine->fd = fd;
}
