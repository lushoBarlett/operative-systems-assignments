#include "server_utils.h"

#include <sys/resource.h>

#define RETURN_IF_NEGATIVE(N) ({if (N < 0) return N;})
#define MAX_EVENTS 10

/*
 * Retorna la cantidad de cores disponibles
 */
static size_t core_count() {
	return sysconf(_SC_NPROCESSORS_ONLN);
}

int set_memory_limit(size_t soft_limit) {
	struct rlimit newlimit = {
		.rlim_cur = soft_limit,
		.rlim_max = -1,
	};

	return setrlimit(RLIMIT_AS, &newlimit);
}

/*
 * Utiliza setuid y setgid para cambiar id de
 * usuario y de grupo
 */
int change_user() {
	if (setgid(1000) != 0)
		return 0;

	if (setuid(1000) != 0)
		return 0;

	return 1;
}

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
 * Devuelve un file descriptor para recibir la señal SIGINT alli
 */
static int require_signal_fd() {
	sigset_t mask;
	int sfd;

	sigemptyset(&mask);
	sigaddset(&mask, SIGINT);

	sigprocmask(SIG_BLOCK, &mask, NULL);

	sfd = signalfd(-1, &mask, 0);
	return sfd;
}

int configure_lsock(int port) {
	int listen_socket = require_socket();

	RETURN_IF_NEGATIVE(listen_socket);
	
	int bind_success = require_bind(listen_socket, port);
    
	RETURN_IF_NEGATIVE(bind_success);
	
	int listen_success = listen(listen_socket, 10000);

	RETURN_IF_NEGATIVE(listen_success);
	
	return listen_socket;
}

/*
 * Crea e inicia los argumentos para pasar a cada thread
 */
static thread_args_t* thread_args_create(int epfd, database_t* database) {
	thread_args_t* thread_args = malloc(sizeof(thread_args_t));

	if (!thread_args)
		return NULL;

	pthread_mutex_init(&thread_args->lock, NULL);
	
	thread_args->epfd = epfd;

	fdinfo_list_t** head_ptr = malloc(sizeof(fdinfo_list_t*));

	if (!head_ptr) {
		free(thread_args);
		pthread_mutex_destroy(&thread_args->lock);
		return NULL;
	}

	*head_ptr = NULL;
	thread_args->head = head_ptr;

	thread_args->database = database;

	return thread_args;
}

/*
 * Libera los recursos de los argumentos de los threads y
 * ademas los de los clientes que quedaron conectados
 */
static void thread_args_destroy(thread_args_t* thread_args, int epfd) {
	fdinfo_list_destroy_remaining(epfd, thread_args->head);

	pthread_mutex_destroy(&thread_args->lock);
	
	free(thread_args);
}

/*
 * Bucle principal
 * Recibe eventos en los file descriptors observados y los maneja
 */
static void service(int epfd, pthread_mutex_t* lock, fdinfo_list_t** head_ptr, database_t* database) {
	int stop = 0;

	struct epoll_event event[MAX_EVENTS];	

	while (!stop) {
		int n = epoll_wait(epfd, event, MAX_EVENTS, -1);

		if (n < 0 && errno != EINTR)
			stop = 1;

		/*
		 * Si un evento es recibido en el file descriptor donde aceptamos
		 * señales, cortamos el bucle. Todos los threads van a recibirla
		 * porque no esta marcado con EPOLLONESHOT
		 */
		for (size_t i = 0; i < (size_t)n && !stop; i++)
			if (handle_event(epfd, event[i], head_ptr, lock, database) == -1)
				stop = 1;
	}
}

/*
 * Saca los valores de la estructura de argumentos
 * y llama a la funcion service con ellos
 */
static void* service_wrap(void* arg) {
	thread_args_t* thread_args = (thread_args_t*) arg;

	int epfd = thread_args->epfd;

	pthread_mutex_t* lock = &thread_args->lock;
	
	fdinfo_list_t** head = thread_args->head;

	database_t* database = thread_args->database;

	service(epfd, lock, head, database);

	return NULL;
}

static void spawn_service_threads(pthread_t* threads, size_t thread_amount, thread_args_t* thread_args) {
	for (size_t i = 0; i < thread_amount; i++) {
		pthread_create(&threads[i], NULL, service_wrap, (void*) thread_args);
	}
}

static void join_service_threads(pthread_t* threads, size_t thread_amount) {
	for (size_t i = 0; i < thread_amount; i++)
		pthread_join(threads[i], NULL);
}

/*
 * Crea una estructura con los argumentos a pasar a cada thread,
 * luego los spawnea hasta que finalicen y libera estos argumentos
 */
static void service_threads(pthread_t* threads, size_t thread_amount, int epfd, database_t* database) {
	thread_args_t* thread_args = thread_args_create(epfd, database);

	if (!thread_args)
		return;

	spawn_service_threads(threads, thread_amount, thread_args);
	join_service_threads(threads, thread_amount);

	thread_args_destroy(thread_args, epfd);
}

/*
 * Ejecucion del servidor
 *
 * Recibe dos sockets para escuchar modo binario o texto
 * 
 * Crea el socket para recibir señales, mete todos en la lista de
 * epoll y luego inicializa los threads para empezar a recibir
 * eventos.
 * 
 * En cada paso chequea si la operacion tuvo exito, y si no
 * va hacia el final donde libera los recursos correspondientes
 * hasta donde se llego a ejecutar
 * 
 * En la funcion poll_socket pasamos NULL como puntero a la 
 * base de datos porque como no son sockets de tipo cliente
 * no utilizan la base de datos para alocar memoria
 */
void server_run(database_t* database, int listen_txt_sock, int listen_bin_sock) {
	int epfd = epoll_create(1);

	if (epfd < 0)
		return;

	int signalfd = require_signal_fd();

	if (signalfd < 0)
		goto signal_fd_error;

	fdinfo_list_t* fdinfo_listen_txt = poll_socket(listen_txt_sock, epfd, Listen, Text, NULL);

	if (!fdinfo_listen_txt)
		goto fdinfo_listen_txt_error;

	fdinfo_list_t* fdinfo_listen_bin = poll_socket(listen_bin_sock, epfd, Listen, Binary, NULL);

	if (!fdinfo_listen_bin)
		goto fdinfo_listen_bin_error;

	fdinfo_list_t* fdinfo_signal = poll_socket(signalfd, epfd, Signal, Text, NULL);

	if (!fdinfo_signal)
		goto fdinfo_signal_error;
	
	size_t thread_amount = core_count();
	pthread_t* threads = malloc(sizeof(*threads) * thread_amount);

	if (!threads)
		goto thread_error;

	service_threads(threads, thread_amount, epfd, database);

	free(threads);
thread_error:
	free(fdinfo_signal);
fdinfo_signal_error:
	free(fdinfo_listen_bin);
fdinfo_listen_bin_error:
	free(fdinfo_listen_txt);
fdinfo_listen_txt_error:
	close(signalfd);
signal_fd_error:
	close(epfd);
}