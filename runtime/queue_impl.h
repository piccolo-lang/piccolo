#ifndef __QUEUE_IMPL_H
#define __QUEUE_IMPL_H

#include "queue.h"
#include "pithread.h"

typedef struct _PICC_QueueCell {
  PICC_PiThread *thread;
  struct _PICC_QueueCell *next;
} PICC_QueueCell;

struct _PICC_ReadyQueue {
  PICC_QueueCell *head;
  PICC_QueueCell *tail;
  int size;
  pthread_mutex_t *lock;
};

typedef struct _PICC_WaitQueue {
  PICC_QueueCell *head;
  PICC_QueueCell *mark;
  PICC_QueueCell *tail;
  int size_active;
  int size_old;
  pthread_mutex_t *lock;
} PICC_WaitQueue;

PICC_ReadyQueue *PICC_readyqueue_alloc();
void PICC_readyqueue_free(PICC_ReadyQueue *);
PICC_WaitQueue *PICC_waitqueue_alloc();
void PICC_waitqueue_free(PICC_WaitQueue *);

PICC_PiThread *PICC_readyqueue_pop_front(PICC_ReadyQueue *);

void PICC_waitqueue_push_front(PICC_WaitQueue *, PICC_PiThread *);
PICC_PiThread *PICC_waitqueue_remove(PICC_WaitQueue *, PICC_PiThread *);
int PICC_waitqueue_size(PICC_WaitQueue *);
int PICC_waitqueue_active_size(PICC_WaitQueue *);
void PICC_waitqueue_active_reset(PICC_WaitQueue *);

void PICC_readyqueue_print(PICC_ReadyQueue *);
void PICC_waitqueue_print(PICC_WaitQueue *);

#endif /* !__QUEUE_IMPL_H */
