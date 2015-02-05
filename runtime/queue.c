#include <assert.h>
#include <stdio.h>
#include <pthread.h>
#include <stdlib.h>
#include "queue_impl.h"
#include "pithread_impl.h"
#include "errors.h"

PICC_ReadyQueue *PICC_readyqueue_alloc()
{
  PICC_ReadyQueue *queue = malloc(sizeof(PICC_ReadyQueue));
  if (queue == NULL)
    PICC_CRASH(ERR_OUT_OF_MEMORY);
  queue->head = NULL;
  queue->tail = NULL;
  queue->size = 0;
  queue->lock = malloc(sizeof(pthread_mutex_t));
  if (queue->lock == NULL)
    PICC_CRASH(ERR_OUT_OF_MEMORY);
  pthread_mutex_init(queue->lock, NULL);
  return queue;
}

void PICC_readyqueue_free(PICC_ReadyQueue *rq)
{
  PICC_QueueCell *current = rq->head;
  PICC_QueueCell *next;
  while (current) {
    next = current->next;
    free(current);
    current = next;
  }
  pthread_mutex_destroy(rq->lock);
  free(rq->lock);
  free(rq);
}

PICC_WaitQueue *PICC_waitqueue_alloc()
{
  PICC_WaitQueue *queue = malloc(sizeof(PICC_WaitQueue));
  if (queue == NULL)
    PICC_CRASH(ERR_OUT_OF_MEMORY);
  queue->head = NULL;
  queue->mark = NULL;
  queue->tail = NULL;
  queue->size_active = 0;
  queue->size_old    = 0;
  queue->lock = malloc(sizeof(pthread_mutex_t));
  if (queue->lock == NULL)
    PICC_CRASH(ERR_OUT_OF_MEMORY);
  pthread_mutex_init(queue->lock, NULL);
  return queue;
}

void PICC_waitqueue_free(PICC_WaitQueue *wq)
{
  PICC_QueueCell *current = wq->head;
  PICC_QueueCell *next;
  while (current) {
    next = current->next;
    free(current);
    current = next;
  }
  pthread_mutex_destroy(wq->lock);
  free(wq->lock);
  free(wq);
}

void PICC_readyqueue_push_front(PICC_ReadyQueue *queue, PICC_PiThread *pt)
{
  PICC_QueueCell *cell = malloc(sizeof(PICC_QueueCell));
  pthread_mutex_lock(queue->lock);

  cell->thread = pt;
  cell->next   = queue->head;
  queue->head = cell;
  queue->size ++;
  if (queue->tail == NULL)
    queue->tail = cell;

  pthread_mutex_unlock(queue->lock);
}

PICC_PiThread *PICC_readyqueue_pop_front(PICC_ReadyQueue *queue)
{
  PICC_PiThread *pt = NULL;
  pthread_mutex_lock(queue->lock);

  if (queue->head != NULL) {
    pt = queue->head->thread;
    PICC_QueueCell *next = queue->head->next;
    free(queue->head);
    queue->head = next;
    queue->size--;
  }

  pthread_mutex_unlock(queue->lock);
  return pt;
}

void PICC_waitqueue_push_front(PICC_WaitQueue *queue, PICC_PiThread *pt)
{
  PICC_QueueCell *cell = malloc(sizeof(PICC_QueueCell));
  pthread_mutex_lock(queue->lock);

  cell->thread = pt;
  cell->next   = queue->head;
  queue->head  = cell;
  queue->size_active ++;
  if (queue->mark == NULL)
    queue->mark = cell;
  if (queue->tail == NULL)
    queue->tail = cell;

  pthread_mutex_unlock(queue->lock);
}

PICC_PiThread *PICC_waitqueue_remove(PICC_WaitQueue *queue, PICC_PiThread *pt)
{
  pthread_mutex_lock(queue->lock);

  PICC_QueueCell *current = queue->head;
  PICC_QueueCell *prev    = NULL;
  int active = 1;

  while (current) {
    if (current == queue->mark)
      active = 0;
    if (current->thread == pt) {
      if (queue->head == current) {
        assert(prev == NULL);
        queue->head = current->next;
        if (queue->mark == current) {
          queue->mark = current->next;
        }
      } else {
        prev->next = current->next;
      }
      if (queue->tail == current) {
        queue->tail = prev;
        if (queue->mark == current) {
          queue->mark = NULL;
        }
      }
      free(current);
      if (active)
        queue->size_active--;
      else
        queue->size_old--;
      pthread_mutex_unlock(queue->lock);
      return pt;
    }
    prev = current;
    current = current->next;
  }

  pthread_mutex_unlock(queue->lock);
  return NULL;
}

int PICC_waitqueue_size(PICC_WaitQueue *queue)
{
  pthread_mutex_lock(queue->lock);
  int r = queue->size_active + queue->size_old;
  pthread_mutex_unlock(queue->lock);
  return r;
}

int PICC_waitqueue_active_size(PICC_WaitQueue *queue)
{
  pthread_mutex_lock(queue->lock);
  int r = queue->size_active;
  pthread_mutex_unlock(queue->lock);
  return r;
}

void PICC_waitqueue_active_reset(PICC_WaitQueue *queue)
{
  pthread_mutex_lock(queue->lock);
  queue->mark = queue->head;
  queue->size_old += queue->size_active;
  pthread_mutex_unlock(queue->lock);
}

void PICC_readyqueue_print(PICC_ReadyQueue *queue)
{
  pthread_mutex_lock(queue->lock);
  printf("ReadyQueue (size = %d):\n", queue->size);
  PICC_QueueCell *current = queue->head;
  while (current) {
    printf("| %p ", current->thread);
    current = current->next;
  }
  printf("|\n");
  pthread_mutex_unlock(queue->lock);
}

void PICC_waitqueue_print(PICC_WaitQueue *queue)
{
  pthread_mutex_lock(queue->lock);
  printf("WaitQueue (size = %d):\n", queue->size_active + queue->size_old);
  PICC_QueueCell *current = queue->head;
  while (current) {
    printf("| %p ", current->thread);
    current = current->next;
  }
  printf("|\n");
  pthread_mutex_unlock(queue->lock);
}
