#pragma once

#define MAX_LINE 1000
#define MAX_ARGS 10

#define IN 0
#define OUT 1

void set_stdin(int fd) {
	dup2(fd, IN);
}

void set_stdout(int fd) {
	dup2(fd, OUT);
}

typedef struct Command {
	char* args[MAX_ARGS];
} Command;

typedef struct Pipe {
	int in;
	int out;
} Pipe;

bool is_exit_command(Command* command) {
	return strcmp(command->args[0], "exit") == 0;
}

void prompt(char* buf) {
	printf(">> ");
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

void execute_command(Command* command){
	execvp(command->args[0], command->args);
	printf("Error executing the command.\n");
	exit(1);
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

pid_t require_fork() {
	pid_t process_id = fork();

	if (fork_failed(process_id)) {
		printf("Creation of child process was not possible. Terminating...\n");
		exit(1);
	}

	return process_id;
}

Pipe require_pipe() {
	Pipe result;
	int pipefd[2];
	
	if (pipe(pipefd)) {
		printf("Creation of pipe was not possible. Terminating...\n");
		exit(1);
	}

	result.in = pipefd[IN];
	result.out = pipefd[OUT];

	return result;
}
