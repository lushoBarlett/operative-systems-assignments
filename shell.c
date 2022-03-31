#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <sys/wait.h>

#define MAX_LINE 100
#define MAX_ARGS 10

int main() {

	while(1) {
		char buf[MAX_LINE] = "\0";
		char* args[MAX_ARGS];

		printf(">> ");

		fgets(buf, MAX_LINE, stdin);
		buf[strcspn(buf, "\r\n")] = 0;

		char* token = strtok(buf, " ");
		int i = 0;

		while(token != NULL) {
			args[i++] = token;
			token = strtok(NULL, " ");
		}

		args[i] = NULL;
		
		if (strcmp(buf, "exit")) {
			printf("Exiting the console.\n");
			break;
		}

		pid_t pid = fork();

		if (pid == 0){
			execv(buf, args);
			printf("Error executing the command.\n");
			exit(0);
		} else if (pid > 0) {
			wait(NULL);
		} else {
			printf("Shell error.\n");
		}
	}

	return 0;
}
