#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include "scheduler_impl.h"
#include "pithread_impl.h"
#include "queue_impl.h"
#include "gc2_impl.h"
#include "errors_impl.h"

PICC_SchedPool *PICC_schedpool_alloc()
{
  PICC_SchedPool *sp = malloc(sizeof(PICC_SchedPool));
  if (sp == NULL)
    PICC_CRASH(ERR_OUT_OF_MEMORY);
  sp->ready             = PICC_readyqueue_alloc();
  sp->wait              = PICC_waitqueue_alloc();
  sp->lock              = malloc(sizeof(pthread_mutex_t));
  if (sp->lock == NULL)
    PICC_CRASH(ERR_OUT_OF_MEMORY);
  pthread_mutex_init(sp->lock, NULL);
  sp->cond              = malloc(sizeof(pthread_cond_t));
  if (sp->cond == NULL)
    PICC_CRASH(ERR_OUT_OF_MEMORY);
  pthread_cond_init(sp->cond, NULL);
  sp->nb_slaves         = 0;
  sp->nb_waiting_slaves = 0;
  sp->running           = 0;
  return sp;
}

void PICC_schedpool_free(PICC_SchedPool *sp)
{
  PICC_readyqueue_free(sp->ready);
  PICC_waitqueue_free(sp->wait);
  pthread_mutex_destroy(sp->lock);
  free(sp->lock);
  pthread_cond_destroy(sp->cond);
  free(sp->cond);
  free(sp);
}

void PICC_schedpool_master(PICC_SchedPool *sp, int std_gc_fuel,
                           int quick_gc_fuel, int active_factor)
{
  PICC_PiThread *current;
  int gc_fuel = std_gc_fuel;

  while (sp->running) {
    while ((current = PICC_readyqueue_pop_front(sp->ready))) {
      do {
        current->proc(sp, current);
      } while (current->status == PICC_STATUS_CALL);

      if (current->status == PICC_STATUS_ENDED) {
        PICC_pithread_free(current);
      } else if (current->status == PICC_STATUS_BLOCKED) {
        if (current->safe_choice) {
          PICC_pithread_free(current);
          PICC_CRASH(ERR_DEADLOCK);
        } else {
          PICC_pithread_free(current);
        }
      }

      gc_fuel--;
      if (gc_fuel == 0) {
        int max_active = PICC_waitqueue_active_size(sp->wait);
        PICC_waitqueue_active_reset(sp->wait);
        if (PICC_waitqueue_size(sp->wait) > max_active * active_factor) {
          int gc_ok = PICC_gc2(sp);

          if (!gc_ok || PICC_waitqueue_size(sp->wait) > max_active * active_factor)
            gc_fuel = quick_gc_fuel;
          else
            gc_fuel = std_gc_fuel;
        } else {
          gc_fuel = std_gc_fuel;
        }
      }
    }

    pthread_mutex_lock(sp->lock);
    if (sp->nb_waiting_slaves == sp->nb_slaves) {
      sp->running = 0;
      pthread_cond_broadcast(sp->cond);
    }
    pthread_mutex_unlock(sp->lock);
  }
}

void *PICC_schedpool_slave(PICC_SchedPool *sp)
{
  PICC_PiThread *current;

  while (sp->running) {
    while ((current = PICC_readyqueue_pop_front(sp->ready))) {
      do {
        current->proc(sp, current);
      } while (current->status == PICC_STATUS_CALL);

      if (current->status == PICC_STATUS_ENDED) {
        PICC_pithread_free(current);
      } else if (current->status == PICC_STATUS_BLOCKED) {
        if (current->safe_choice) {
          PICC_pithread_free(current);
          PICC_CRASH(ERR_DEADLOCK);
        } else {
          PICC_pithread_free(current);
        }
      }
    }

    pthread_mutex_lock(sp->lock);
    sp->nb_waiting_slaves++;
    pthread_cond_wait(sp->cond, sp->lock);
    sp->nb_waiting_slaves--;
    pthread_mutex_unlock(sp->lock);
  }

  return NULL;
}

PICC_ReadyQueue *PICC_sched_get_readyqueue(PICC_SchedPool *sched)
{
  return sched->ready;
}
