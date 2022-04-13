#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <sys/wait.h>
#include <stdbool.h>

#include "shell_utils.h"

#define MAX_COMMANDS 10

int parse_commands(char* buf, Command* commands) {
	char* command_strings[MAX_COMMANDS];

	int i = 0;

	command_strings[i] = strtok(buf, "|");
	while (command_strings[++i] = strtok(NULL, "|"));

	for (int j = 0; j < i; j++)
		parse_command(command_strings[j], &commands[j]);

	return i;
}

void pipe_command_chain(Command* command, int input_fd, int current, int amount) {

	if (current == amount - 1) {
		pid_t process_id = fork();

		if (fork_failed(process_id)) {
			printf("Shell error.\n");
			return;
		}

		if (is_child_process(process_id)) {
			set_stdin(input_fd);
			execute_command(command);
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
		set_stdin(input_fd);
		set_stdout(pipefd[OUT]);
		close(pipefd[IN]);
		execute_command(command);
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

		prompt(buf);

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
