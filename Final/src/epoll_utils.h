#pragma once

#define _GNU_SOURCE
#include <sys/socket.h>
#include <pthread.h>
#include <stdio.h>
#include <sys/epoll.h>
#include <stdlib.h>
#include <unistd.h>
#include <errno.h>
#include <string.h>

#include "binary_parser.h"
#include "text_parser.h"
#include "database.h"

/*
 * Representa el tipo de socket
 */
typedef enum {
	Listen, Client, Signal,
} Sock_type;

/*
 * Si un socket tiene tipo cliente,
 * representa si sera modo binario o texto
 */
typedef enum {
	Binary, Text,
} Cli_type;

/*
 * Informacion de cada cliente que guardamos en epoll
 * 
 * Tipos, socket para comunicarme, su maquina de estados
 * 
 * Ademas tiene incorporada una lista doblemente enlazada
 * para llevar cuenta de los clientes conectados. Si en 
 * algun momento quiero matar el servidor, sirve para
 * liberar todos los recursos usados.
 */
typedef struct fdinfo {
	Cli_type cli_type;
	Sock_type sock_type;
	int fd;
	bin_state_machine_t bin_state_machine;
	text_state_machine_t text_state_machine;
	struct fdinfo* next;
	struct fdinfo* prev;
} fdinfo_list_t;

/*
 * Libera los recursos asociados a los clientes conectados
 * al servidor. Ademas cierra los file descriptors
 * No utiliza ninguna medida de sincronizacion porque
 * se utiliza cuando el servidor se esta cerrando y la
 * ejecutara un solo thread
 */
void fdinfo_list_destroy_remaining(int epfd, fdinfo_list_t** head_ptr);

/*
 * Agrega un socket a la lista de seguimiento de epoll
 * 
 * Se encarga de crear un fdinfo que tendra la informacion
 * de la conexion y si todo sale correctamente lo retorna.
 * Ademas setea los eventos a seguir en dicho file descriptor
 * 
 * En caso de error retorna NULL
 * 
 * Si el tipo de socket es de escucha o signal no utiliza
 * database_memsafe_malloc porque significa que aun se
 * esta inicializando el servidor y usa un malloc simple
 */
fdinfo_list_t* poll_socket(int socket, int epfd, Sock_type sock_type, Cli_type cli_type, database_t* database);

/*
 * Manejador de eventos ocurridos en algun file descriptor
 * trackeado por epoll. Si corresponde al file descriptor
 * de las señales retornara -1
 */
int handle_event(int epfd, struct epoll_event event, fdinfo_list_t** head, pthread_mutex_t* lock, database_t* database);