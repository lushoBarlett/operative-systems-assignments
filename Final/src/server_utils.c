#include "server_utils.h"

#define RETURN_IF_NEGATIVE(N) ({if (N < 0) return N;})

static int require_signal_fd() {
	sigset_t mask;
	int sfd;

	sigemptyset(&mask);
	sigaddset(&mask, SIGINT);

	sigprocmask(SIG_BLOCK, &mask, NULL);

	sfd = signalfd(-1, &mask, 0);
	return sfd;
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

static int configure_lsock(int port) {
	int listen_socket = require_socket();

	RETURN_IF_NEGATIVE(listen_socket);
	
	int bind_success = require_bind(listen_socket, port);

	RETURN_IF_NEGATIVE(bind_success);
	
	int listen_success = listen(listen_socket, 10000);

	RETURN_IF_NEGATIVE(listen_success);
	
	return listen_socket;
}

static size_t core_count() {
	return sysconf(_SC_NPROCESSORS_ONLN);
}

static void service(int epfd, pthread_mutex_t* lock, fdinfo_list_t** head_ptr, database_t* database) {
	int stop = 0;

	struct epoll_event event;	

	while (!stop) {
		// TODO: set argument to more than 1?
		int n = epoll_wait(epfd, &event, 1, -1);

		if (n < 0 && errno != EINTR)
			stop = 1;

		for (size_t i = 0; i < (size_t)n && !stop; i++)
			if (handle_event(epfd, event, head_ptr, lock, database) == -1)
				stop = 1;
	}
}

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

static thread_args_t* thread_args_create(int epfd, database_t* database) {
	thread_args_t* thread_args = database_memsafe_malloc(database, sizeof(thread_args_t));

	pthread_mutex_init(&thread_args->lock, NULL);
	
	thread_args->epfd = epfd;

	fdinfo_list_t** head_ptr = database_memsafe_malloc(database, sizeof(fdinfo_list_t*));
	*head_ptr = NULL;
	thread_args->head = head_ptr;

	thread_args->database = database;

	return thread_args;
}

static void service_threads(pthread_t* threads, size_t thread_amount, int epfd, database_t* database) {
	thread_args_t* thread_args = thread_args_create(epfd, database);

	spawn_service_threads(threads, thread_amount, thread_args);
	join_service_threads(threads, thread_amount);

	fdinfo_list_destroy_remaining(epfd, thread_args->head);

	pthread_mutex_destroy(&thread_args->lock);
	
	free(thread_args);
}

static int get_sockets(int* listen_txt_sock, int* listen_bin_sock, int* signalfd) {
	*listen_txt_sock = configure_lsock(8000);

	RETURN_IF_NEGATIVE(*listen_txt_sock);
	
	*listen_bin_sock = configure_lsock(8001);

	if (*listen_bin_sock < 0)
		goto bin_sock_error;

	*signalfd = require_signal_fd();

	if (*signalfd < 0)
		goto signal_fd_error;

	return 1;

signal_fd_error:
	close(*listen_bin_sock);
bin_sock_error:
	close(*listen_txt_sock);

	return -1;
}

void server_run(database_t* database) {
	int epfd = epoll_create(1);

	if (epfd < 0)
		return;

	int listen_txt_sock;
	int listen_bin_sock;
	int signalfd;

	if (get_sockets(&listen_txt_sock, &listen_bin_sock, &signalfd) < 0)
		goto socket_error;
	
	fdinfo_list_t* fdinfo_listen_txt = poll_socket(listen_txt_sock, epfd, Listen, Text, database);

	if (!fdinfo_listen_txt)
		goto fdinfo_listen_txt_error;

	fdinfo_list_t* fdinfo_listen_bin = poll_socket(listen_bin_sock, epfd, Listen, Binary, database);

	if (!fdinfo_listen_bin)
		goto fdinfo_listen_bin_error;

	fdinfo_list_t* fdinfo_signal = poll_socket(signalfd, epfd, Signal, Text, database);

	if (!fdinfo_signal)
		goto fdinfo_signal_error;
	
	size_t thread_amount = core_count();
	pthread_t* threads = database_memsafe_malloc(database, sizeof(*threads) * thread_amount);

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
	close(listen_bin_sock);
	close(listen_txt_sock);
socket_error:
	close(epfd);
}
