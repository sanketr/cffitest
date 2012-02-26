#include "test.h"
#define MAXITERATIONS 1000000

void startTest(int);

int gThreads = 0; //number of threads
FnPtr* gFn = NULL;
static pthread_mutex_t* gLock = NULL;

void initThreads(int nThreads, FnPtr* fns)
{

  gThreads = nThreads;
  //set function pointers
  gFn = fns;
  //create mutexes for each thread
  gLock = (pthread_mutex_t*) malloc (gThreads * sizeof(pthread_mutex_t));
  pthread_mutexattr_t mutexAttr;
  pthread_mutexattr_settype(&mutexAttr,PTHREAD_MUTEX_NORMAL);
  int i;
  //initialize the mutexes
  for(i=0;i<gThreads;i++){
    pthread_mutex_init(&gLock[i],&mutexAttr);
    }

  startTest(gThreads);
  printf("C FFI done...exiting\n");
}

void* doSomething(void* threadid){
  int i = *(int*) threadid;
  printf("Started thread %i\n",i);
  int j;
  //start filling the buffers here
  for(j=0; j < MAXITERATIONS;j++){
    pthread_mutex_lock(&gLock[i]);
    pthread_mutex_unlock(&gLock[i]);
  }
  sleep(1000000);
  printf("Thread done\n");
  return NULL;
}

void sendSignal(short threadid){
  pthread_mutex_lock(&gLock[threadid]);
  //callback function to report thread data
  (gFn[threadid])();
  pthread_mutex_unlock(&gLock[threadid]);
}

void startTest(int nThreads){
 pthread_t* pth = (pthread_t*) malloc (nThreads*sizeof(pthread_t));
 int* tid = (int*) malloc (nThreads*sizeof(int));

 int i;
 for(i=0; i < nThreads; i++){
  tid[i] = i;
  pthread_create(&pth[i], NULL, doSomething, (void*) &tid[i]);
 }

 for(i=0; i < nThreads; i++)
   pthread_join(pth[i], NULL);
}

