#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>

#include <sys/wait.h>
#include <stdbool.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

#include "shell_utils.h"

int parse_line(char *buf, Command *command, char **filename){
	char* command_string;

	command_string = strtok(buf, ">");

	*filename = strtok(NULL,">");

	parse_command(command_string, command);

	return (*filename == NULL);
}

int open_file(char *filename) {
	if (*filename == ' ')
		filename++;
	open(filename, O_CREAT | O_WRONLY, 0644);
}

int main() {

	while(1) {
		char buf[MAX_LINE];
		Command command;
		char *filename;
		int fd_newfile;

		prompt(buf);

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
				dup2(fd_newfile, OUT);
			}
			
			execute_command(&command);
		}

		wait(NULL);
	}

	return 0;
}
