#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <sched.h>
#include "runtime.h"
#include "pithread_impl.h"
#include "scheduler_impl.h"
#include "errors.h"

void PICC_main(int nb_core_threads, PICC_PiThreadProc *entrypoint,
               int std_gc_fuel, int quick_gc_fuel, int active_factor,
               int entry_env_length, int entry_enabled_length)
{
  PICC_SchedPool *sp = PICC_schedpool_alloc();

  int status;
  void *slave_fun = PICC_schedpool_slave;
  pthread_t *threads = malloc(nb_core_threads * sizeof(pthread_t));
  if (threads == NULL)
    PICC_CRASH(ERR_OUT_OF_MEMORY);

  sp->running = 1;

  for (int i = 0 ; i < nb_core_threads ; i++) {
    status = pthread_create(&threads[i], NULL, slave_fun, sp);
    if (status)
      PICC_CRASH(ERR_PTHREAD_CREATE);
    sp->nb_slaves++;
  }

  while (sp->nb_waiting_slaves != sp->nb_slaves) {
    sched_yield();
  }

  PICC_PiThread *init_thread = PICC_pithread_alloc(entry_env_length, entry_enabled_length);
  init_thread->proc = entrypoint;

  PICC_readyqueue_push_front(sp->ready, init_thread);
  PICC_schedpool_master(sp, std_gc_fuel, quick_gc_fuel, active_factor);

  for (int i = 0 ; i < nb_core_threads ; i++) {
    pthread_join(threads[i], NULL);
  }

  free(threads);
  PICC_schedpool_free(sp);
}
