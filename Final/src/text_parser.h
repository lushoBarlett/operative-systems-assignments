#pragma once

#include "code.h"
#include "database.h"

typedef struct txt_state_machine_t {
	char buf[2048];

	int read_characters;
	int fd;

	database_t* database;
} txt_state_machine_t;

typedef void (*command_handler)(database_t*, char*, int);

void txt_state_machine_init(txt_state_machine_t* state_machine, int fd, database_t* database);

int can_read_txt(txt_state_machine_t* state_machine);
