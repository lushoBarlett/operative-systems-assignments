#include <netinet/in.h>
#include <netinet/ip.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <unistd.h>

#define RETURN_IF_NEGATIVE(N) ({if (N < 0) return N;})

static int require_socket() {
	int listen_socket = socket(AF_INET, SOCK_STREAM, 0);

	RETURN_IF_NEGATIVE(listen_socket);

	int yes = 1;

	int sock_opt_success = setsockopt(listen_socket, SOL_SOCKET, SO_REUSEADDR, &yes, sizeof yes);

	RETURN_IF_NEGATIVE(sock_opt_success);

	return listen_socket;
}

static int require_bind(int listen_socket, int port) {
	struct sockaddr_in socket_address;

	memset(&socket_address, 0, sizeof socket_address);
	socket_address.sin_family = AF_INET;
	socket_address.sin_port = htons(port);
	socket_address.sin_addr.s_addr = htonl(INADDR_ANY);

	return bind(listen_socket, (const struct sockaddr*)&socket_address, sizeof socket_address);
}

/*
 * Crea un socket de escucha bindeado al puerto dado
 */
static int configure_lsock(int port) {
	int listen_socket = require_socket();

	RETURN_IF_NEGATIVE(listen_socket);
	
	int bind_success = require_bind(listen_socket, port);
    
	RETURN_IF_NEGATIVE(bind_success);
	
	int listen_success = listen(listen_socket, 10000);

	RETURN_IF_NEGATIVE(listen_success);
	
	return listen_socket;
}

/*
 * Utilizamos setuid y setgid para cambiar id de
 * usuario y de grupo
 */
int change_user() {
    if (setgid(1000) != 0)
        return 0;
    
    if (setuid(1000) != 0)
        return 0;

    return 1;
}

/*
 * Setea los argumentos para pasarlos por un execv
 */
char** set_argv(char* argv[], int text_sock, int bin_sock) {
    char string_text_sock[4];
    char string_bin_sock[4];

    sprintf(string_text_sock, "%d", text_sock);
    sprintf(string_bin_sock, "%d", bin_sock);

    argv[0] = "./memcache";
    argv[1] = string_text_sock;
    argv[2] = string_bin_sock;
    argv[3] = NULL;

    return argv;
}

/*
 * Este programa se debe ejecutar como usuario root para
 * poder crear dos sockets de escucha bindeados a los puertos
 * 800 y 801
 * 
 * Luego de crearlos, cambia a un usuario no root y ejecuta el
 * programa que correra el servidor pasandoles los sockets creados
 */
int main() {
    int text_sock = configure_lsock(888);
    int bin_sock = configure_lsock(889);

    if (!change_user())
        return 0;

    char* argv[4];
    set_argv(argv, text_sock, bin_sock);

    execv(argv[0], argv);
}