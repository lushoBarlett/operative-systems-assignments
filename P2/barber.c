#include <stdio.h>
#include <pthread.h>
#include <semaphore.h>
#include <unistd.h>

#define SILLAS 4
#define N 10
pthread_mutex_t lock;
int waiting = 0;
pthread_cond_t barber_wake_up, barber_is_free;
sem_t cut_done, about_to_pay, client_leaves, number_updated, barber_can_cut;

void cutting() {
    printf("cutting hair...\n");
}

void getting_cut() {
    printf("getting cut...\n");
}

void paying() {
    printf("paying...\n");
}

void getting_paid() {
    printf("getting paid...\n");
}

void wait_for_my_turn() {
    waiting++;
    if (waiting > 1) {
        int wait_number = waiting;
        while (wait_number > 1) {
            pthread_cond_wait(&barber_is_free, &lock);
            wait_number--;
            sem_post(&number_updated);
        }
    } else {
        pthread_cond_signal(&barber_wake_up);
    }

}

void client_cutting_procedure() {
    getting_cut();
    sem_wait(&cut_done);
    sem_post(&about_to_pay);
    paying();
    sem_post(&client_leaves);
}

int no_chairs_available() {
    return waiting == SILLAS;
}

void* client() {
    pthread_mutex_lock(&lock);

    if (no_chairs_available()) {
        pthread_mutex_unlock(&lock);
        return NULL;
    }

    wait_for_my_turn();
    pthread_mutex_unlock(&lock);

    client_cutting_procedure();
}


void barber_cutting_procedure() {
    cutting();
    sem_post(&cut_done);
    sem_wait(&about_to_pay);
    getting_paid();
    sem_wait(&client_leaves);
}

void make_clients_update_turns() {
    pthread_mutex_lock(&lock);
    waiting--;

    int signals_sent = waiting;
    
    if (waiting)
        pthread_cond_broadcast(&barber_is_free);

    printf("\nNext! %d waiting\n\n", waiting);
    pthread_mutex_unlock(&lock);

    // Waits for all the clients to update their numbers
    for(int i = 0; i < signals_sent; i++) {
        sem_wait(&number_updated);
    }
}

void sleep_if_no_clients() {
    pthread_mutex_lock(&lock);
    if (waiting == 0) {
        printf("Time for a nap!\n");
        pthread_cond_wait(&barber_wake_up, &lock);
    }

    pthread_mutex_unlock(&lock);
}

void* barber() {
    while (1) {
        sem_post(&barber_can_cut);

        sleep_if_no_clients();

        barber_cutting_procedure();

        make_clients_update_turns();
    }
}


int main() {
    pthread_mutex_init(&lock, NULL);
    sem_init(&cut_done, 0, 0);
    sem_init(&about_to_pay, 0, 0);
    sem_init(&client_leaves, 0, 0);
    sem_init(&number_updated, 0, 0);
    sem_init(&barber_can_cut, 0, 0);


    pthread_t clients[N*2];
    pthread_t barber_thread;


    for(int i = 0; i < N; i++) {
        pthread_create(&clients[i], NULL, client, NULL);
    }

    pthread_create(&barber_thread, NULL, barber, NULL);

    sleep(1);
    for(int i = N; i < 2*N; i++) {
        pthread_create(&clients[i], NULL, client, NULL);
    }

    pthread_join(barber_thread, NULL);

    return 0;
}