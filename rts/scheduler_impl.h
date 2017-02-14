#ifndef __SCHEDULER_IMPL_H
#define __SCHEDULER_IMPL_H

#include <pthread.h>
#include "scheduler.h"
#include "queue_impl.h"

struct _PICC_SchedPool {
  PICC_ReadyQueue *ready;
  PICC_WaitQueue *wait;
  pthread_mutex_t *lock;
  pthread_cond_t *cond;
  int nb_slaves;
  int nb_waiting_slaves;
  int running;
};

PICC_SchedPool *PICC_schedpool_alloc();
void PICC_schedpool_free(PICC_SchedPool *);

void *PICC_schedpool_slave(PICC_SchedPool *);
void PICC_schedpool_master(PICC_SchedPool *, int std_gc_fuel,
                           int quick_gc_fuel, int active_factor);

#endif /* !__SCHEDULER_IMPL_H */
