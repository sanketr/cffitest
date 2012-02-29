#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>

int pthread_mutexattr_settype(pthread_mutexattr_t*, int);
int sleep(int);

typedef void(*FnPtr)();

static int gThreads = 0; //number of threads
static FnPtr* gFn = NULL;
static pthread_mutex_t* gLock = NULL;

void initThreads(int nThreads, FnPtr* fns)
{

  gThreads = nThreads;
  //set function pointers
  gFn = fns;
  //create mutexes for each thread
  gLock = (pthread_mutex_t*) malloc (gThreads * sizeof(pthread_mutex_t));
  pthread_mutexattr_t mutexAttr;
  pthread_mutexattr_settype(&mutexAttr,NULL);
  int i;
  //initialize the mutexes
  for(i=0;i<gThreads;i++){
    pthread_mutex_init(&gLock[i],&mutexAttr);
    }

}

void sendSignal(short threadid){
  //callback function to report thread data
  (gFn[threadid])();
}
