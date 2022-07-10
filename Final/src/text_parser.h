#pragma once

typedef struct {
	char* buf;
	size_t len;
} buf_t;

buf_t* buf_create(int size);

void buf_clear(buf_t* buf);

void buf_destroy(buf_t* buf);

void buf_ring_cat(buf_t* buf, char* str, int str_len);

void last_n_words(char* buf, int buf_len, char** args, int n);

int str_ends_with(char* str, int str_len, char* cmp, int cmp_len);

int try_uni_args(char** args);

int try_bi_args(char** args);

int try_tri_args(char** args);

void print_args(char** args);

void parse_args(char** args);

void parse_recv(buf_t* buf, char* recv, int len);

void require_stats();

void require_get();

void require_take();

void require_del();

void require_put();
