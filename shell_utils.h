#pragma once

#define MAX_LINE 1000
#define MAX_ARGS 10

#define IN 0
#define OUT 1

typedef struct Command {
	char* args[MAX_ARGS];
} Command;

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
