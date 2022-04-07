#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>

#include <sys/wait.h>
#include <stdbool.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>


#define MAX_LINE 1000
#define MAX_ARGS 10

#define OUT 1

typedef struct Command {
	char* args[MAX_ARGS];
} Command;

bool is_exit_command(Command* command) {
	return strcmp((command->args)[0], "exit") == 0;
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

// Returns 0 if there is no filename, otherwise returns positive
int parse_line(char *buf, Command *command, char **filename){
	char* command_string;

	command_string = strtok(buf, ">");

	*filename = strtok(NULL,">");

	parse_command(command_string, command);

	return (*filename == NULL);
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


int open_file(char *filename) {
	if (*filename == ' ')
		filename++;
	open(filename, O_CREAT | O_WRONLY, 0644);
}


void execute_command(Command* command, int fd){
	dup2(fd,OUT);
	execvp(command->args[0],command->args);
}

int main() {

	while(1) {
		char buf[MAX_LINE];
		Command command;
		char *filename;
		int fd_newfile;

		prompt();

		readline(buf);

		int without_file = parse_line(buf, &command, &filename);

		if (is_exit_command(&command)) {
			printf("Exiting the console.\n");
			break;
		}


		pid_t process_id = fork();

		if (fork_failed(process_id)) {
			printf("Shell error.\n");
			return 0;
		}

		if (is_child_process(process_id)) {
			if (!without_file) {
				fd_newfile = open_file(filename);
				execute_command(&command,fd_newfile);
			}
			execvp(command.args[0],command.args);
		}

		wait(NULL);
	}

	return 0;
}
