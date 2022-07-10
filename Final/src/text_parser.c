#include <string.h>
#include <stdlib.h>
#include <assert.h>
#include <stdio.h>
#include "text_parser.h"
#define MAX_ARR_SIZE 2048

buf_t* buf_create(int size) {
	buf_t* buf = malloc(sizeof(buf_t));
	buf->buf = malloc(sizeof(char)*(size+1));
	buf->len = 0;
	return buf;
}

void buf_clear(buf_t* buf) {
	buf->len = 0;
}

void buf_destroy(buf_t* buf) {
	free(buf->buf);
	free(buf);
}

void buf_ring_cat(buf_t* buf, char* str, int str_len) {
	//assert(len <= MAX_ARR_SIZE);
	// en este caso capaz mejor abortar
	if (str_len > MAX_ARR_SIZE) {
		str += str_len - MAX_ARR_SIZE;
		str_len = MAX_ARR_SIZE;
	}

	int space = MAX_ARR_SIZE - buf->len;
	int dif = str_len - space;

	if (dif > 0) {
		strcpy(buf->buf, buf->buf + dif);
		strcpy(buf->buf + buf->len - dif, str);
	} else
		strcpy(buf->buf + buf->len, str);

	if (MAX_ARR_SIZE < buf->len + str_len)
		buf->len = MAX_ARR_SIZE;
	else
		buf->len = buf->len + str_len;
}

void last_n_words(char* buf, int buf_len, char** args, int n) {
	for (int i = n; i--;) args[i] = NULL;

	int idx = buf_len;

	while (idx && (buf[idx-1] == ' ')) idx--;

	for (int i = n; i-- && idx;) {

		while (idx && (buf[idx-1] != ' ')) idx--;

		args[i] = buf+idx;

		while (idx && (buf[idx-1] == ' ')) idx--;

		if (idx) buf[idx] = 0;
	}
}

int str_ends_with(char* str, int str_len, char* cmp, int cmp_len) {
	if (str_len < cmp_len)
		return 0;
	return !(strcmp(str + str_len - cmp_len, cmp));
}

int try_uni_args(char** args) {
	if ((args[2] != NULL) && str_ends_with(args[2], strlen(args[2]), "STATS", 5)) {
			require_stats();
			return 1;
	}
	return 0;
}

int try_bi_args(char** args) {
	int ret = 1;
	if ((args[1] != NULL)) {
		if (str_ends_with(args[1], strlen(args[1]), "GET", 3))
			require_get();
		else if (str_ends_with(args[1], strlen(args[1]), "TAKE", 4))
			require_take();
		else if (str_ends_with(args[1], strlen(args[1]), "DEL", 3))
			require_del();
		else
			ret = 0;
	} else
		ret = 0;

	return ret;
}

int try_tri_args(char** args) {
	if ((args[0] != NULL) && str_ends_with(args[0], strlen(args[0]), "PUT", 3)) {
			require_put();
			return 1;
	} 
	return 0;
}

void print_args(char** args) {
	printf("0: %s, 1: %s, 2: %s\n", args[0], args[1], args[2]);
}

void parse_args(char** args) {
	print_args(args);
	if (try_tri_args(args));
	else if (try_bi_args(args));
	else if (try_uni_args(args));
	else printf("args dont match\n"); // reply with EINVAL
	return;
}

void parse_recv(buf_t* buf, char* recv, int len) {
	char* args[3];
	char* snd = strchr(recv, '\n');
	int snd_len = snd - recv;
	snd_len = len - snd_len;

	if (!snd) {
		printf("need more stuff to form a line\n");
		buf_ring_cat(buf, recv, len);
		return;
	}

	*snd = 0;

	buf_ring_cat(buf, recv, len);
	last_n_words(buf->buf, buf->len, args, 3);
	parse_args(args);

	buf_clear(buf);
	
	if (*(++snd))
		parse_recv(buf, snd, snd_len);
}

void require_stats() {
	printf("Requiring stats...\n");
}

void require_get() {
	printf("Requiring get...\n");
}


void require_take() {
	printf("Requiring take...\n");
}


void require_del() {
	printf("Requiring del...\n");
}


void require_put() {
	printf("Requiring put...\n");
}
