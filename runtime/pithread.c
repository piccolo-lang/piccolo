#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "pithread_impl.h"
#include "scheduler_impl.h"
#include "errors.h"

PICC_Clock *PICC_clock_alloc()
{
  PICC_Clock *clock = malloc(sizeof(PICC_Clock));
  if (clock == NULL)
    PICC_CRASH(ERR_OUT_OF_MEMORY);
  clock->value   = PICC_atomicint_alloc(0);
  return clock;
}

void PICC_clock_free(PICC_Clock *clock)
{
  PICC_atomicint_free(clock->value);
  free(clock);
}

PICC_PiThread *PICC_pithread_alloc(int env_length, int enabled_length)
{
  PICC_PiThread *pt = malloc(sizeof(PICC_PiThread));
  if (pt == NULL)
    PICC_CRASH(ERR_OUT_OF_MEMORY);
  pt->status         = PICC_STATUS_RUN;
  pt->enabled        = malloc(sizeof(int) * enabled_length);
  if (pt->enabled == NULL)
    PICC_CRASH(ERR_OUT_OF_MEMORY);
  pt->enabled_length = enabled_length;
  pt->managed_values = PICC_managedvalset_alloc();
  pt->env            = malloc(sizeof(PICC_Value) * env_length);
  if (pt->env == NULL)
    PICC_CRASH(ERR_OUT_OF_MEMORY);
  pt->env_length     = env_length;
  pt->commit         = NULL;
  pt->commits        = PICC_commitlist_alloc();
  pt->proc           = NULL;
  pt->pc             = DEFAULT_PC_ENTRY;
  PICC_novalue_init(&pt->val);
  pt->clock          = PICC_clock_alloc();
  pt->safe_choice    = 0;
  pt->fuel           = PICC_FUEL_INIT;
  pt->lock           = malloc(sizeof(pthread_mutex_t));
  if (pt->lock == NULL)
    PICC_CRASH(ERR_OUT_OF_MEMORY);
  pthread_mutex_init(pt->lock, NULL);
  return pt;
}

void PICC_pithread_free(PICC_PiThread *pt)
{
  PICC_managedvalset_clean_all(pt->managed_values);
  PICC_managedvalset_free(pt->managed_values);
  PICC_commitlist_free(pt->commits);

  free(pt->enabled);
  free(pt->env);
  PICC_clock_free(pt->clock);

  pthread_mutex_destroy(pt->lock);
  free(pt->lock);

  free(pt);
}

PICC_Value *PICC_get_register_ptr(PICC_PiThread *pt)
{
  return &pt->val;
}

int PICC_get_pc(PICC_PiThread *pt)
{
  return pt->pc;
}

PICC_Value PICC_get_register(PICC_PiThread *pt)
{
  return pt->val;
}

PICC_Value *PICC_get_env(PICC_PiThread *pt, int i)
{
  return &pt->env[i];
}

int PICC_get_enabled(PICC_PiThread *pt, int i)
{
  return pt->enabled[i];
}

int PICC_get_fuel(PICC_PiThread *pt)
{
  return pt->fuel;
}

void PICC_set_status(PICC_PiThread *pt, PICC_Status status)
{
  pt->status = status;
}

void PICC_set_proc(PICC_PiThread *pt, PICC_PiThreadProc proc)
{
  pt->proc = proc;
}

void PICC_set_pc(PICC_PiThread *pt, PICC_Label label)
{
  pt->pc = label;
}

void PICC_set_register(PICC_PiThread *pt, PICC_Value *value)
{
  pt->val = *value;
}

void PICC_set_env(PICC_PiThread *pt, int i, PICC_Value value)
{
  pt->env[i] = value;
}

void PICC_set_enabled(PICC_PiThread *pt, int i, int v)
{
  pt->enabled[i] = v;
}

void PICC_set_safechoice(PICC_PiThread *pt, int b)
{
  pt->safe_choice = 1;
}

void PICC_decr_fuel(PICC_PiThread *pt)
{
  pt->fuel --;
}

void PICC_forget_all_values(PICC_PiThread *pt)
{
  PICC_managedvalset_forget_all(pt->managed_values);
}

void PICC_register_env_value(PICC_PiThread *pt, int i)
{
  PICC_managedvalset_register(pt->managed_values, &pt->env[i]);
}

void PICC_register_register_value(PICC_PiThread *pt)
{
  PICC_managedvalset_register(pt->managed_values, &pt->val);
}

void PICC_process_end(PICC_PiThread *pt, PICC_Status status)
{
  PICC_managedvalset_clean_all(pt->managed_values);
  pt->status = status;
}

void PICC_process_yield(PICC_PiThread *pt, PICC_SchedPool *sp)
{
  pt->fuel   = PICC_FUEL_INIT;
  pt->status = PICC_STATUS_RUN;

  PICC_managedvalset_clean(pt->managed_values);

  PICC_readyqueue_push_front(sp->ready, pt);
}

void PICC_process_wait(PICC_PiThread *pt, PICC_SchedPool *sp)
{
  pt->pc   = PICC_INVALID_PC;
  pt->fuel = PICC_FUEL_INIT;

  PICC_managedvalset_clean(pt->managed_values);
  
  pt->status = PICC_STATUS_WAIT;
  PICC_waitqueue_push_front(sp->wait, pt);
  pthread_mutex_unlock(pt->lock);
}

void PICC_process_awake(PICC_Commit *commit, PICC_SchedPool *sp)
{
  PICC_PiThread *pt = commit->thread;
  pthread_mutex_lock(pt->lock);
  assert(pt->commit == commit);

  PICC_PiThread *res = NULL;
  do {
    res = PICC_waitqueue_remove(sp->wait, pt);
  } while (res == NULL);

  pt->commit = NULL;
  pt->pc     = commit->cont_pc;
  pt->status = PICC_STATUS_RUN;

  int clock_val = PICC_atomicint_get(pt->clock->value);
  if (clock_val == PICC_CLOCK_MAX_INT) {
    PICC_clock_free(pt->clock);
    pt->clock = PICC_clock_alloc();
  } else {
    PICC_atomicint_compare_and_swap(pt->clock->value, clock_val, clock_val + 1);
  }

  pthread_mutex_unlock(pt->lock);
  PICC_readyqueue_push_front(sp->ready, pt);
}

void PICC_process_lock(PICC_PiThread *pt)
{
  pthread_mutex_lock(pt->lock);
}

int PICC_process_lock_channel(PICC_PiThread *pt, PICC_Channel *chan)
{
  while (pthread_mutex_trylock(chan->lock)) {
    pt->fuel--;
    if (pt->fuel == 0)
      return 0;
    sched_yield();
  }
  return 1;
}

int PICC_process_can_awake(PICC_Commit *commit)
{
  if (pthread_mutex_trylock(commit->thread->lock))
    return PICC_CANNOT_ACQUIRE;

  if (commit->clock != commit->thread->clock ||
      commit->clockval != PICC_atomicint_get(commit->clock->value)) {
    pthread_mutex_unlock(commit->thread->lock);
    return PICC_INVALID_COMMIT;
  }

  commit->thread->commit = commit;
  pthread_mutex_unlock(commit->thread->lock);
  return PICC_VALID_COMMIT;
}

