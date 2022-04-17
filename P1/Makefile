.PHONY: all
all: shell smokers_same_agent philosophers_semaphores philosophers_leftist io server

shell: shell.c shell_utils.h
	gcc shell.c -o shell -pthread

smokers_same_agent: smokers_same_agent.c
	gcc smokers_same_agent.c -o smokers_same_agent -pthread

philosophers_semaphores: philosophers_semaphores.c philosopher_utils.h
	gcc philosophers_semaphores.c -o philosophers_semaphores -pthread

philosophers_leftist: philosophers_leftist.c philosopher_utils.h
	gcc philosophers_leftist.c -o philosophers_leftist -pthread

io: io.c shell_utils.h
	gcc io.c -pthread -o io

server: server.c
	gcc server.c -pthread -o server