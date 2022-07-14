#pragma once

typedef enum {
	ReadingKeyLength,
	ReadingKey,
	ReadingValueLength,
	ReadingValue,
} IntentalState;

typedef struct state_machine_t {
	enum code code;
	IntentalState state;

	uint32_t arg_len;
	uint8_t* key;
	uint8_t* value;

	int read_characters;

	int file_descriptor;
} state_machine_t;

void state_machine_init(state_machine_t* state_machine);

void handle_nothing(state_machine_t* state_machine);

void handle_put(state_machine_t* state_machine);

void handle_get(state_machine_t* state_machine);

void handle_del(state_machine_t* state_machine);

void handle_take(state_machine_t* state_machine);

void handle_stats(state_machine_t* state_machine);

void state_machine_advance(state_machine_t* state_machine); 
