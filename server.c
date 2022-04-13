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

/*
 * Para probar, usar netcat. Ej:
 *
 *      $ nc localhost 4040
 *      NUEVO
 *      0
 *      NUEVO
 *      1
 *      CHAU
 */

void quit(char* error_message) {
	perror(error_message);
	abort();
}

int require_accept(int lsock) {
	int csock = accept(lsock, NULL, NULL);
	if (csock < 0)
		quit("accept");
	return csock;
}

int U = 0;

int fd_readline(char* buf, int fd) {
	int rc;
	int i = 0;

	while ((rc = read(fd, buf + i, 1)) > 0) {
		if (buf[i] == '\n')
			break;
		i++;
	}

	if (rc < 0)
		return rc;

	buf[i] = 0;
	return i;
}

int require_fd_readline(char* buf, int fd) {
	int rc = fd_readline(buf, fd);
	if (rc < 0)
		quit("read... raro");
	return rc;
}

void* handle_conn(void* arg) {
	int csock = (intptr_t)arg;
	char buf[200];

	while (1) {
		int rc = require_fd_readline(buf, csock);

		if (rc == 0) {
			/* linea vacia, se cerró la conexión */
			close(csock);
			return NULL;
		}

		if (!strcmp(buf, "NUEVO")) {
			char reply[20];
			pthread_mutex_lock(&mutex);  // LOCK
			sprintf(reply, "%d\n", U);
			U++;
			pthread_mutex_unlock(&mutex);  // UNLOCK
			write(csock, reply, strlen(reply));
		} else if (!strcmp(buf, "CHAU")) {
			close(csock);
			return NULL;
		}
	}
}

void wait_for_clients(int lsock) {
	pthread_t s;
	int csock = require_accept(lsock);

	pthread_create(&s, NULL, handle_conn, (void*)(intptr_t)csock);

	wait_for_clients(lsock);
}

/* Crea un socket de escucha en puerto 4040 TCP */
int mk_lsock() {
	struct sockaddr_in sa;
	int lsock;
	int rc;
	int yes = 1;

	/* Crear socket */
	lsock = socket(AF_INET, SOCK_STREAM, 0);
	if (lsock < 0)
		quit("socket");

	/* Setear opción reuseaddr... normalmente no es necesario */
	if (setsockopt(lsock, SOL_SOCKET, SO_REUSEADDR, &yes, sizeof yes) == 1)
		quit("setsockopt");

	sa.sin_family = AF_INET;
	sa.sin_port = htons(4040);
	sa.sin_addr.s_addr = htonl(INADDR_ANY);

	/* Bindear al puerto 4040 TCP, en todas las direcciones disponibles */
	rc = bind(lsock, (struct sockaddr*)&sa, sizeof sa);
	if (rc < 0)
		quit("bind");

	/* Setear en modo escucha */
	rc = listen(lsock, 10);
	if (rc < 0)
		quit("listen");

	return lsock;
}

int main() {
	int lsock;
	lsock = mk_lsock();
	pthread_mutex_init(&mutex, NULL);
	wait_for_clients(lsock);
}
