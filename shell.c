#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <sys/wait.h>
#include <stdbool.h>

#define MAX_LINE 100
#define MAX_ARGS 10

bool is_exit_command(const char* buf) {
	return strcmp(buf, "exit") == 0;
}

void prompt() {
	printf(">> ");
}

void read_command(char* buf, char** args) {
	fgets(buf, MAX_LINE, stdin);
	buf[strcspn(buf, "\r\n")] = 0;

	char* token = strtok(buf, " ");
	int i = 0;

	while(token != NULL) {
		args[i++] = token;
		token = strtok(NULL, " ");
	}

	args[i] = NULL;
}

bool fork_failed(pid_t process_id) {
	return process_id == -1;
}

bool is_parent_process(pid_t process_id) {
	return process_id > 0;
}

void execute_command(const char* buf, char * const* args) {
	pid_t process_id = fork();

	if (fork_failed(process_id)) {
		printf("Shell error.\n");
		return;
	}

	if (is_parent_process(process_id)) {
		wait(NULL);
		return;
	}

	execv(buf, args);
	printf("Error executing the command.\n");
	exit(0);
}

int main() {

	while(1) {
		char buf[MAX_LINE];
		char* args[MAX_ARGS];

		prompt();

		read_command(buf, args);
		
		if (is_exit_command(buf)) {
			printf("Exiting the console.\n");
			break;
		}

		execute_command(buf, args);
	}

	return 0;
}
