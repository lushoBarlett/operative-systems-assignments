#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>
#include <arpa/inet.h>
#include "binary_parser.h"

enum code {
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

static int read_n(struct state_machine* sm, unsigned char* dest, int n, int fd) {
	int ret;
	int rc = read(fd, dest + sm->rc, n - sm->rc);
	if (rc < 0 && (errno == EAGAIN || errno == EWOULDBLOCK)) {
		return 0;
	}
	if (rc < 0) {
		printf("error! %s\n", strerror(errno));
	}
	sm->rc += rc;
	ret = sm->rc == n;

	if (ret)
		sm->rc = 0;

	return ret;
}

static int get_len(struct state_machine* sm, int* len, int fd) {
	int ret = read_n(sm, sm->buf, 4, fd);
	if (!ret)
		return 0;

	*len = ntohl(*(int*)(sm->buf));
	
	return 1;
}

static int get_key_val(struct state_machine* sm, unsigned char* dest, int dest_len, int fd) {
	return read_n(sm, dest, dest_len, fd);
}

static int parse_comm(struct state_machine* sm, unsigned char** buf, int fd) {
	int success;

	if (sm->arg_len == 0) {
		success = get_len(sm, &sm->arg_len, fd);

		if (!success)
			return 0;

		*buf = malloc(sm->arg_len);
	}

	success = get_key_val(sm, *buf, sm->arg_len, fd);

	if (success)
		sm->arg_len = 0;

	return success;
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

void sm_can_read(struct state_machine* sm, int fd) {
	sm->next(sm, fd);
}

void handle_nothing(struct state_machine* sm, int fd) {

	unsigned char code[1];
	int success = read_n(sm, code, 1, fd);
	if (!success)
		return;

	switch (*code) {
	case PUT:
		sm->next = handle_put;
		break;
	case GET:
		sm->next = handle_get;
		break;
	case DEL:
		sm->next = handle_del;
		break;
	case TAKE:
		sm->next = handle_take;
		break;
	case STATS:
		sm->next = handle_stats;
		break;
	default:
		sm->next = handle_nothing;
	}
	
	sm->next(sm, fd);
}

static void handle_comm_uniarg(struct state_machine* sm, int fd, void (*f)()) {
	int success = parse_comm(sm, &sm->key, fd);
	if (!success)
		return;
	f();
	handle_nothing(sm, fd);
}

void handle_put(struct state_machine* sm, int fd) {
	if (sm->ist == Key) {
		int success = parse_comm(sm, &sm->key, fd);
		if (!success)
			return;
		sm->ist = Val;
	}

	int success = parse_comm(sm, &sm->value, fd);
	if (!success)
		return;

	require_put();
	handle_nothing(sm, fd);
}

void handle_get(struct state_machine* sm, int fd) {
	handle_comm_uniarg(sm, fd, require_get);
}

void handle_del(struct state_machine* sm, int fd) {
	handle_comm_uniarg(sm, fd, require_del);
}

void handle_take(struct state_machine* sm, int fd) {
	handle_comm_uniarg(sm, fd, require_take);
}

void handle_stats(struct state_machine* sm, int fd) {
	require_stats();
	handle_nothing(sm, fd);
}

struct state_machine* sm_init() {
	struct state_machine* sm = malloc(sizeof(struct state_machine));
	sm->next = handle_nothing;
	sm->ist = Key;
	sm->buf = malloc(sizeof(char)*4);
	sm->key = sm->value = NULL;
	sm->rc = sm->arg_len = 0;
	return sm;
}

void sm_destroy(struct state_machine* sm) {
	free(sm->buf);
	free(sm);
}
