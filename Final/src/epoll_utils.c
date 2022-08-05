#include "epoll_utils.h"

static void kill_client(int epfd, fdinfo_list_t* fdinfo){
	epoll_ctl(epfd, EPOLL_CTL_DEL, fdinfo->fd, NULL);

	close(fdinfo->fd);

	free(fdinfo);
}

static void client_state_machine_init(fdinfo_list_t* fdinfo, Cli_type cli_type, database_t* database) {
	if (cli_type == Binary)
		bin_state_machine_init(&fdinfo->bin_state_machine, fdinfo->fd, database);

	if (cli_type == Text)
		text_state_machine_init(&fdinfo->text_state_machine, fdinfo->fd, database);
}

/*
 * Crea un fdinfo.

 * Utilizando un malloc safe que utiliza la lru
 * si el fdinfo es para un cliente.
 * Si no solo utiliza malloc ya que el servidor esta arrancando.
 */
static fdinfo_list_t* fdinfo_create(int socket, Sock_type sock_type, Cli_type cli_type, database_t* database) {
	fdinfo_list_t* fdinfo;

	if (sock_type == Client)
		fdinfo = database_memsafe_malloc(database, sizeof (fdinfo_list_t));
	else
		fdinfo = malloc(sizeof (fdinfo_list_t));

	if (!fdinfo)
		return NULL;
	
	fdinfo->fd = socket;
	fdinfo->sock_type = sock_type;
	fdinfo->cli_type = cli_type;

	if (sock_type == Client)
		client_state_machine_init(fdinfo, cli_type, database);

	return fdinfo;
}

static void fdinfo_list_remove(fdinfo_list_t* node, fdinfo_list_t** head) {
	if (node->next)
		node->next->prev = node->prev;

	if (node->prev)
		node->prev->next = node->next;
	else
		*head = node->next;
}

static void fdinfo_list_add(fdinfo_list_t* new_fdinfo, fdinfo_list_t** head) {
	new_fdinfo->next = *head;
	new_fdinfo->prev = NULL;

	if (*head)
		(*head)->prev = new_fdinfo;
	
	*head = new_fdinfo;
}

void fdinfo_list_destroy_remaining(int epfd, fdinfo_list_t** head_ptr) {
	fdinfo_list_t* head = *head_ptr;
	fdinfo_list_t* tmp;

	while (head) {
		tmp = head;
		head = head->next;
		kill_client(epfd, tmp);
	}

	free(head_ptr);
}

/*
 * Chequea si un evento recibido es de desconexion
 */
static int is_hup_event(uint32_t events) {
	return (events & EPOLLRDHUP || events &  EPOLLHUP);
}

/*
 * Setea los eventos de interes para un socket.

 * Si es cliente, nos interesa cuando hay nuevos
 * mensajes, cuando hay desconexion y ademas
 * que cada evento lo reciba un solo thread
 * 
 * Si es una señal queremos que la reciban todos
 * los threads
 * 
 * Si es un socket de escucha queremos que
 * lo reciba un solo thread
 */
static void poll_set_event(struct epoll_event* ev, Sock_type type) {
	if (type == Client)
		ev->events = EPOLLIN | EPOLLONESHOT | EPOLLHUP | EPOLLRDHUP;

	if (type == Signal)
		ev->events = EPOLLIN;

	if (type == Listen)
		ev->events = EPOLLIN | EPOLLONESHOT;
}

fdinfo_list_t* poll_socket(int socket, int epfd, Sock_type sock_type, Cli_type cli_type, database_t* database) {

	struct epoll_event ev;

	ev.data.ptr = fdinfo_create(socket, sock_type, cli_type, database);

	if (!(ev.data.ptr))
		return NULL;

	poll_set_event(&ev, sock_type);

	if (epoll_ctl(epfd, EPOLL_CTL_ADD, socket, &ev) < 0) {
		free(ev.data.ptr);
		return NULL;
	} else
		return ev.data.ptr;
}

static int repoll_socket(fdinfo_list_t* fdinfo, int epfd, Sock_type type) {
	struct epoll_event event;

	event.data.ptr = fdinfo;
	
	poll_set_event(&event, type);
	
	return epoll_ctl(epfd, EPOLL_CTL_MOD, fdinfo->fd, &event);
}

/*
 * Maneja un evento ocurrido en el file descriptor de un cliente
 * 
 * Si no es un evento por desconexion, hara avanzar su maquina
 * de estados.
 * 
 * Lo matara si hubo un error en el parseo o si el evento era una
 * desconexion. Utilizamos un lock para ello para evitar
 * race conditions en la lista enlazada de fdinfos
 */
static void handle_client(int epfd, struct epoll_event event, fdinfo_list_t* fdinfo, fdinfo_list_t** head, pthread_mutex_t* lock) {
	int ret_value = 0;

	if (event.events & EPOLLIN && !is_hup_event(event.events)) {

		if (fdinfo->cli_type == Text)
			ret_value = text_state_machine_advance(&fdinfo->text_state_machine);

		if (fdinfo->cli_type == Binary)
			ret_value = bin_state_machine_advance(&fdinfo->bin_state_machine);
	}

	if (ret_value == 0) {
		pthread_mutex_lock(lock);
		
		fdinfo_list_remove(fdinfo, head);
		
		kill_client(epfd, fdinfo);
		
		pthread_mutex_unlock(lock);
	} else
		repoll_socket(fdinfo, epfd, Client);
}

/*
 * Intenta aceptar una nueva conexion.
 * Si puede agrega la informacion del cliente a la lista
 * de fdinfos
 */
static void handle_listener(int epfd, fdinfo_list_t* fdinfo, fdinfo_list_t** head, pthread_mutex_t* lock, database_t* database) {
	int listen_socket = fdinfo->fd;

	int connection_socket = accept4(listen_socket, NULL, NULL, SOCK_NONBLOCK);

	if (connection_socket < 0)
		return;

	fdinfo_list_t* new_fdinfo = poll_socket(connection_socket, epfd, Client, fdinfo->cli_type, database);

	if (!new_fdinfo)
		return;

	pthread_mutex_lock(lock);

	fdinfo_list_add(new_fdinfo, head);
	
	pthread_mutex_unlock(lock);

	repoll_socket(fdinfo, epfd, Listen);
}

int handle_event(int epfd, struct epoll_event event, fdinfo_list_t** head, pthread_mutex_t* lock, database_t* database) {
	fdinfo_list_t* fdinfo = event.data.ptr;
	int ret_value = 1;

	switch (fdinfo->sock_type) {
	case Listen:
		handle_listener(epfd, fdinfo, head, lock, database);
		break;

	case Client:
		handle_client(epfd, event, fdinfo, head, lock);
		break;

	case Signal:
		ret_value = -1;
		break;
	}

	return ret_value;
}