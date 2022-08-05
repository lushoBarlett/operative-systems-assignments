#include "database.h"
#include "server_utils.h"

/*
 * Se ejecuta en modo root, obtiene dos sockets
 * y luego cambia a un usuario no root para correr
 * el servidor.
 */
int main() {
	/*
	 * 512 MiB
	 */
	size_t half_gigabyte = 1 << 29;

	if (set_memory_limit(half_gigabyte) < 0)
		return 0;

	int text_sock = configure_lsock(888);

	if (text_sock < 0)
		return 0;

	int bin_sock = configure_lsock(889);

	if (bin_sock < 0)
		goto bin_sock_error;

	if (!change_user())
		goto change_user_error;

	database_t database;

	database_init(&database);

	server_run(&database, text_sock, bin_sock);

	database_destroy(&database);

change_user_error:
	close(bin_sock);
bin_sock_error:
	close(text_sock);
}