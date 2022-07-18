#pragma once

enum Command {
	Put,
	Del,
	Get,
	Take,
	Stats,
	Nothing,
};
typedef struct txt_state_machine_t {
	char buf[2048];
	int read_characters;
	int fd;
	database_t* db;
} txt_state_machine_t;

typedef void (*command_handler)(database_t*, char*, int);

void txt_state_machine_init(txt_state_machine_t* state_machine, int fd, database_t* db);

int can_read_txt(txt_state_machine_t* state_machine);