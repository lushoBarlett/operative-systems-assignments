.PHONY: all
all: shell smokers_same_agent philosophers_semaphores philosophers_leftist

shell: shell.c
	gcc shell.c -o shell -pthread

smokers_same_agent: smokers_same_agent.c
	gcc smokers_same_agent.c -o smokers_same_agent -pthread

philosophers_semaphores: philosophers_semaphores.c
	gcc philosophers_semaphores.c -o philosophers_semaphores -pthread

philosophers_leftist: philosophers_leftist.c
	gcc philosophers_leftist.c -o philosophers_leftist -pthread
