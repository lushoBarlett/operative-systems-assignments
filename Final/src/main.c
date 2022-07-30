#include "database.h"
#include "server_utils.h"

int main() {
	// TODO: setrlimit
	// TODO: configure the correct ports
	database_t database;

	database_init(&database);

	server_run(&database);

	database_destroy(&database);
}