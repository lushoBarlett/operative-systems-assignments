#include <stdio.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <arpa/inet.h>
#include <unistd.h>
#include <string.h>
#include <time.h>
#include <errno.h>

#include <assert.h>

#define BUFF_SIZE 1024
#define N 20000

int main() {
	int sock[N], cto, i;
	struct sockaddr_in servaddr;
	char buffer[BUFF_SIZE], buffer2[BUFF_SIZE];
	struct timespec tsp;

	memset(&servaddr, 0, sizeof(servaddr));
	servaddr.sin_family = AF_INET;
	servaddr.sin_port = htons(8000);
	inet_pton(AF_INET, "127.0.0.1", &servaddr.sin_addr);
	tsp.tv_sec = 0;
	tsp.tv_nsec = 1000000;

	for (i = 0; i < N; i++) {
		int rc;

		printf("Conectando cliente %d...\n", i);
		sock[i] = socket(AF_INET, SOCK_STREAM, 0);
		if (sock[i] < 0) {
			perror("CLNT: Error creando socket");
			return -1;
		}

		/*
		 * Esto sólo está para no agotar los puertos efímeros
		 * de la dirección 127.0.0.1, de los cuales hay aprox
		 * 30000. Hacemos un bind() explicito a puertos e IPs
		 * (de origen) distintas a modo de evitarlo. (Todas
		 * las IP 127.x.y.z representan al host local.)
		 */
		{
			struct sockaddr_in clientaddr;
			int portnum = i % 1001 + 10000;
			int ipaddrnum = i / 1000 + 10;
			char ipaddr[32];

			int yes = 1;

			if (setsockopt(sock[i], SOL_SOCKET, SO_REUSEADDR, &yes, sizeof yes) == 1)
				assert(0);

			memset(&clientaddr, 0, sizeof(clientaddr));
			clientaddr.sin_family = AF_INET;
			clientaddr.sin_port = htons(portnum);
			sprintf(ipaddr, "127.0.0.%d", ipaddrnum);
			inet_pton(AF_INET, ipaddr, &clientaddr.sin_addr);
			rc = bind(sock[i], (struct sockaddr *)&clientaddr, sizeof(clientaddr));
			if (rc < 0) {
				perror("ECHOCLNT: Error llamando a bind()");
				return -1;
			}
		}

		rc = connect(sock[i], (struct sockaddr *)&servaddr, sizeof(servaddr));
		if (rc < 0) {
			perror("CLNT: Error conectando");
			return -1;
		}

		nanosleep(&tsp, NULL);
	}

	for (i = 0; i < N; i++) {
		int rc;

		sprintf(buffer2, "NUEVO\n", i);
		cto = strlen(buffer2);

		rc = write(sock[i], buffer2, cto);
		if (rc < 0) {
			perror("CLNT: Error escribiendo");
			return -1;
		}

		cto = read(sock[i], buffer, sizeof(buffer) - 1);
		if (cto < 0) {
			perror("CLNT: Error leyendo");
			return -1;
		}

		buffer[cto] = 0;
		printf("A [%s] devuelve [%s]\n", buffer2, buffer);
	}

	for (i = 0; i < N; i++)
		close(sock[i]);

	return 0;
}
