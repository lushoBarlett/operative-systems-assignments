#pragma once

enum code {
	PUT = 11,
	DEL = 12,
	GET = 13,
	TAKE = 14,
	STATS = 21,
	OK = 101,
	EINVAL = 111,
	ENOTFOUND = 112,
	EBINARY = 113,
	EBIG = 114,
	EUNK = 115,
};

typedef enum {
	ReadingKeyLength,
	ReadingKey,
	ReadingValueLength,
	ReadingValue,
} InternalState;

typedef struct bin_state_machine_t {
	enum code code;
	InternalState state;

	uint32_t arg_len;
	uint8_t* key;
	uint8_t* value;

	int read_characters;

	int fd;
} bin_state_machine_t;

void state_machine_init(bin_state_machine_t* state_machine, int fd);

int handle_nothing(bin_state_machine_t* state_machine);

int handle_put(bin_state_machine_t* state_machine);

int handle_get(bin_state_machine_t* state_machine);

int handle_del(bin_state_machine_t* state_machine);

int handle_take(bin_state_machine_t* state_machine);

void handle_stats(bin_state_machine_t* state_machine);

int state_machine_advance(bin_state_machine_t* state_machine); 
