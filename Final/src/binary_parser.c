#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>
#include <arpa/inet.h>
#include "binary_parser.h"

enum code {
	NOTHING = 0,
	PUT = 11,
	DEL = 12,
	GET = 13,
	TAKE = 14,

	STATS = 21,

	OK = 101,
	EINVALID = 111,
	ENOTFOUND = 112,
	EBINARY = 113,
	EBIG = 114,
	EUNK = 115,
};

static int check_finished(state_machine_t* state_machine, int n) {
	int finished = state_machine->read_characters == n;

	if (finished)
		state_machine->read_characters = 0;
	
	return finished;
}

static int read_n(state_machine_t* state_machine, uint8_t* dest, int n) {

	uint8_t current_dest = dest + state_machine->read_characters;

	uint8_t bytes_left = n - state_machine->read_characters;

	int current_read = read(state_machine->file_descriptor, current_dest, bytes_left);

	if (current_read < 0 && (errno == EAGAIN || errno == EWOULDBLOCK))
		return 0;

	if (current_read < 0)
		printf("error! %s\n", strerror(errno));

	state_machine->read_characters += current_read;

	return check_finished(state_machine, n);
}

static int get_len(state_machine_t* state_machine) {
	int finished = read_n(state_machine, &state_machine->arg_len, 4);

	if (finished)
		state_machine->arg_len = ntohl(state_machine->arg_len);
	
	return finished;
}

// TODO: further divide
static int parse_comm(state_machine_t* state_machine, uint8_t** buf) {
	int finished;

	if (state_machine->state == ReadingKeyLength || state_machine->state == ReadingValueLength) {
		finished = get_len(state_machine);

		if (!finished)
			return 0;

		*buf = malloc(state_machine->arg_len);
	}

	finished = read_n(state_machine, *buf, state_machine->arg_len);

	if (finished)
		state_machine->state; // TODO: = new_state

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

void state_machine_advance(state_machine_t* state_machine) {
	switch (state_machine->code) {
	case PUT:
		handle_put(state_machine);
		break;
	case GET:
		handle_get(state_machine);
		break;
	case DEL:
		handle_del(state_machine);
		break;
	case TAKE:
		handle_take(state_machine);
		break;
	case STATS:
		handle_stats(state_machine);
		break;
	case NOTHING:
		handle_nothing(state_machine);
		break;
	default:
		// TODO: handle error, maybe remove "nothing" and always handle nothing
		assert(0);
	}
}

void handle_nothing(state_machine_t* state_machine) {
	uint8_t code;

	// TODO the succes and return pattern may be extracted to a macro
	int success = read_n(state_machine, &code, 1);
	
	if (!success)
		return;

	state_machine->code = code;

	state_machine_advance(state_machine);
}

static void handle_comm_uniarg(state_machine_t* state_machine, void (*f)()) {
	int success = parse_comm(state_machine, &state_machine->key);
	if (!success)
		return;
	f();
	handle_nothing(state_machine);
}

void handle_put(state_machine_t* state_machine) {
	if (state_machine->state == ReadingKey) {
		int success = parse_comm(state_machine, &state_machine->key);

		if (!success)
			return;

		state_machine->state; // TODO: new_state
	}

	int success = parse_comm(state_machine, &state_machine->value);

	if (!success)
		return;

	require_put();
	handle_nothing(state_machine);
}

void handle_get(state_machine_t* state_machine) {
	handle_comm_uniarg(state_machine, require_get);
}

void handle_del(state_machine_t* state_machine) {
	handle_comm_uniarg(state_machine, require_del);
}

void handle_take(state_machine_t* state_machine) {
	handle_comm_uniarg(state_machine, require_take);
}

void handle_stats(state_machine_t* state_machine) {
	require_stats();
	handle_nothing(state_machine);
}

void state_machine_init(state_machine_t* state_machine) {
	state_machine->code = NOTHING;
	state_machine->state = ReadingKeyLength;
	state_machine->key = state_machine->value = NULL;
	state_machine->read_characters = state_machine->arg_len = 0;
}
