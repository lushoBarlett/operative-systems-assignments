#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <sys/wait.h>
#include <stdbool.h>

#define MAX_LINE 100
#define MAX_ARGS 10

typedef struct Command {
	char* args[MAX_ARGS];
} Command;

bool is_exit_command(Command* command) {
	return strcmp(command->args[0], "exit") == 0;
}

void prompt() {
	printf(">> ");
}

void readline(char* buf) {
	fgets(buf, MAX_LINE, stdin);
	buf[strcspn(buf, "\r\n")] = 0;
}

void parse_command(char* buf, Command* command) {
	char* token = strtok(buf, " ");
	int i = 0;

	while(token != NULL) {
		command->args[i++] = token;
		token = strtok(NULL, " ");
	}

	command->args[i] = NULL;
}

bool fork_failed(pid_t process_id) {
	return process_id == -1;
}

bool is_parent_process(pid_t process_id) {
	return process_id > 0;
}

void execute_command(Command* command) {
	pid_t process_id = fork();

	if (fork_failed(process_id)) {
		printf("Shell error.\n");
		return;
	}

	if (is_parent_process(process_id)) {
		wait(NULL);
		return;
	}

	execv(command->args[0], command->args);
	printf("Error executing the command.\n");
	exit(0);
}

int main() {

	while(1) {
		char buf[MAX_LINE];
		Command command;

		prompt();

		readline(buf);

		parse_command(buf, &command);
		
		if (is_exit_command(&command)) {
			printf("Exiting the console.\n");
			break;
		}

		execute_command(&command);
	}

	return 0;
}
