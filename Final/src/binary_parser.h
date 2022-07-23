#pragma once

#include "code.h"
#include "database.h"

typedef enum {
	ReadingKeyLength,
	ReadingKey,
	ReadingValueLength,
	ReadingValue,
} InternalState;

typedef struct bin_state_machine_t {
	Code code;
	InternalState state;

	uint32_t arg_len;
	uint8_t* key;
	uint8_t* value;

	int read_characters;

	int fd;

	database_t* database;
} bin_state_machine_t;

void bin_state_machine_init(bin_state_machine_t* state_machine, int fd, database_t* database);

int handle_nothing(bin_state_machine_t* state_machine);

int handle_put(bin_state_machine_t* state_machine);

int handle_get(bin_state_machine_t* state_machine);

int handle_del(bin_state_machine_t* state_machine);

int handle_take(bin_state_machine_t* state_machine);

void handle_stats(bin_state_machine_t* state_machine);

int bin_state_machine_advance(bin_state_machine_t* state_machine); 
