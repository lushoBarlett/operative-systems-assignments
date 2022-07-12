#pragma once

typedef enum {
	Key, Val, 
} Internal_state;

struct state_machine {
	void (*next)(struct state_machine* sm, int fd);
	Internal_state ist;
	unsigned char* buf;
	unsigned char* key;
	unsigned char* value;
	int rc;
	int arg_len;
};

void handle_nothing(struct state_machine* sm, int fd);

void handle_put(struct state_machine* sm, int fd);

void handle_get(struct state_machine* sm, int fd);

void handle_del(struct state_machine* sm, int fd);

void handle_take(struct state_machine* sm, int fd);

void handle_stats(struct state_machine* sm, int fd);

void sm_can_read(struct state_machine* sm, int fd); 

struct state_machine* sm_init();

void sm_destroy(struct state_machine* sm);
