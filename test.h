#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>

int pthread_mutexattr_settype(pthread_mutexattr_t*, int);
int sleep(int);

typedef void(*FnPtr)();

void initThreads(int, FnPtr*);
void sendSignal(short);
