#include <netinet/in.h>
#include <netinet/ip.h>
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <unistd.h>

pthread_mutex_t mutex;
int UID = 0;

int atomic_increment() {
	pthread_mutex_lock(&mutex);
	
	int id = UID++;
	
	pthread_mutex_unlock(&mutex);
	
	return id;
}

void quit(const char* error_message) {
	perror(error_message);

	abort();
}

int fd_readline(char* buf, int fd) {
	int read_result;
	int i;

	for (i = 0; (read_result = read(fd, buf + i, 1)) > 0; i++)
		if (buf[i] == '\n')
			break;

	if (read_result < 0)
		return read_result;

	buf[i] = 0;
	return i;
}

int require_fd_readline(char* buf, int fd) {
	int read_result = fd_readline(buf, fd);

	if (read_result < 0)
		quit("read");

	return read_result;
}

void reply_with(int csock, int client_id) {
	char reply[20];
	
	sprintf(reply, "%d\n", client_id);
	
	write(csock, reply, strlen(reply));
}

int is_new_request(const char* buf) {
	return !strcmp(buf, "NUEVO");
}

int is_close_request(const char* buf) {
	return !strcmp(buf, "CHAU");
}

void* handle_conn(void* arg) {
	int csock = (intptr_t)arg;
	char buf[200];

	while (1) {
		int rc = require_fd_readline(buf, csock);

		if (rc == 0) {
			close(csock);
			return NULL;
		}

		if (is_close_request(buf)) {
			close(csock);
			return NULL;
		}

		if (is_new_request(buf)) {
			int client_id = atomic_increment();
			reply_with(csock, client_id);
		}
	}
}

int require_accept(int lsock) {
	int csock = accept(lsock, NULL, NULL);
	
	if (csock < 0)
		quit("accept");

	return csock;
}

int require_socket() {
	int lsock = socket(AF_INET, SOCK_STREAM, 0);

	if (lsock < 0)
		quit("socket");

	int yes = 1;

	if (setsockopt(lsock, SOL_SOCKET, SO_REUSEADDR, &yes, sizeof yes) == 1)
		quit("setsockopt");

	return lsock;
}

int require_bind(int lsock, int port) {
	struct sockaddr_in sa;

	sa.sin_family = AF_INET;
	sa.sin_port = htons(port);
	sa.sin_addr.s_addr = htonl(INADDR_ANY);

	if (bind(lsock, (const struct sockaddr*)&sa, sizeof sa) < 0)
		quit("bind");
}

int require_listen(int lsock, int connections) { 
	if (listen(lsock, connections) < 0)
		quit("listen");
}

int configure_lsock() {
	int lsock = require_socket();
	
	require_bind(lsock, 8000);
	
	require_listen(lsock, 10);
	
	return lsock;
}

void wait_for_clients(int lsock) {
	pthread_t s;

	int csock = require_accept(lsock);

	pthread_create(&s, NULL, handle_conn, (void*)(intptr_t)csock);

	wait_for_clients(lsock);
}

int main() {
	int lsock = configure_lsock();
	
	pthread_mutex_init(&mutex, NULL);
	
	wait_for_clients(lsock);
}
