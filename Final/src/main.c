#include "database.h"
#include "server_utils.h"

/*
 * Recibira los sockets de escucha binarios y de texto
 */
int main(int argc, char* argv[]) {
	// TODO: setrlimit
	if (argc != 3)
		return 0;
	
	int listen_text_socket = strtol(argv[1], NULL, 10);
	int listen_bin_socket = strtol(argv[2], NULL, 10);

	database_t database;

	database_init(&database);

	server_run(&database, listen_text_socket, listen_bin_socket);

	database_destroy(&database);
}