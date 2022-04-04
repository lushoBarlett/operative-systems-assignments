.PHONY: all
all: shell smokers philosophers_semaphores philosophers_leftist

shell: shell.c
	gcc shell.c -o shell -pthread

smokers: smokers.c
	gcc smokers.c -o smokers -pthread

philosophers_semaphores: philosophers_semaphores.c
	gcc philosophers_semaphores.c -o philosophers_semaphores -pthread

philosophers_leftist: philosophers_leftist.c
	gcc philosophers_leftist.c -o philosophers_leftist -pthread
