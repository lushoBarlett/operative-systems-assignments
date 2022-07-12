#pragma once

typedef struct {
	char* buf;
	size_t len;
} buf_t;

buf_t* buf_create(int size);

void buf_clear(buf_t* buf);

void buf_destroy(buf_t* buf);

void buf_can_read(buf_t* buf, int fd);

void require_stats();

void require_get();

void require_take();

void require_del();

void require_put();
