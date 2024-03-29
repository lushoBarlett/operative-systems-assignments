#pragma once

#define _GNU_SOURCE
#include <features.h>

#include <netinet/in.h>
#include <netinet/ip.h>
#include <pthread.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <sys/signalfd.h>
#include <unistd.h>
#include <signal.h>
#include <sys/epoll.h>

#include "text_parser.h"
#include "binary_parser.h"
#include "database.h"
#include "epoll_utils.h"

/*
 * Estructura para guardar los argumentos que se pasaran
 * a los threads para que reciban eventos
 * 
 * La cabeza de la lista de fdinfos por si un cliente
 * cierra su conexion y lo tengo que eliminar de la misma
 * 
 * Un lock para asegurar la sincronizacion al sacar o agregar
 * elementos a la lista de fdinfo
 * 
 * El file descriptor del epoll
 * 
 * Un puntero a la base de datos
 */
typedef struct thread_args {
	fdinfo_list_t** head;
	pthread_mutex_t lock;
	int epfd;
	database_t* database;
} thread_args_t;

/*
 * Limita la memoria a la cantidad de bytes especificados,
 * retorna 0 si lo logra y -1 si hubo un error,
 * en cuyo caso setea errno según la documentación de setrlimit
 */
int set_memory_limit(size_t soft_limit);

/*
 * Cambia a un usuario no root, por defecto es 1000
 */
int change_user();

/*
 * Crea un socket de escucha bindeado al puerto dado
 */
int configure_lsock(int port);

/*
 * Ejecucion completa del servidor
 */
void server_run(database_t* database, int, int);