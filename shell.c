#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <sys/wait.h>
#include <stdbool.h>

#define MAX_LINE 1000
#define MAX_ARGS 10
#define MAX_COMMANDS 10

#define IN 0
#define OUT 1

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
	char* argument_string = strtok(buf, " ");
	int i = 0;

	while (argument_string) {
		command->args[i++] = argument_string;
		argument_string = strtok(NULL, " ");
	}

	command->args[i] = NULL;
}

int parse_commands(char* buf, Command* commands) {
	char* command_strings[MAX_COMMANDS];

	int i = 0;

	command_strings[i] = strtok(buf, "|");

	while (command_strings[++i] = strtok(NULL, "|"));

	for (int j = 0; j < i; j++)
		parse_command(command_strings[j], &commands[j]);

	return i;
}

bool fork_failed(pid_t process_id) {
	return process_id == -1;
}

bool is_parent_process(pid_t process_id) {
	return process_id > 0;
}

bool is_child_process(pid_t process_id) {
	return process_id == 0;
}

void pipe_command_chain(Command* command, int input_fd, int current, int amount) {

	if (current == amount - 1) {
		pid_t process_id = fork();

		if (fork_failed(process_id)) {
			printf("Shell error.\n");
			return;
		}

		if (is_child_process(process_id)) {
			dup2(input_fd, IN);

			execvp(command->args[0], command->args);
			printf("Error executing the command.\n");
			exit(0);
		}

		if (input_fd != IN)
			close(input_fd);

		return;
	}

	int pipefd[2];
	
	// TODO: handle pipe errors better
	if (pipe(pipefd))
		exit(-1);

	pid_t process_id = fork();

	if (fork_failed(process_id)) {
		printf("Shell error.\n");
		return;
	}

	if (is_child_process(process_id)) {
		dup2(input_fd, IN);
		dup2(pipefd[OUT], OUT);
		close(pipefd[IN]);

		execvp(command->args[0], command->args);
		printf("Error executing the command.\n");
		exit(0);
	}

	if (input_fd != IN)
		close(input_fd);

	close(pipefd[OUT]);

	pipe_command_chain(command + 1, pipefd[IN], current + 1, amount);
}

int main() {

	while(1) {
		char buf[MAX_LINE];
		Command commands[MAX_COMMANDS];

		prompt();

		readline(buf);

		int command_amount = parse_commands(buf, commands);

		if (is_exit_command(&commands[0])) {
			printf("Exiting the console.\n");
			break;
		}

		pipe_command_chain(commands, IN, 0, command_amount);

		for (int i = 0; i < command_amount; i++)
			wait(NULL);
	}

	return 0;
}
