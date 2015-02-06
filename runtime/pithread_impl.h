#ifndef __PITHREAD_IMPL_H
#define __PITHREAD_IMPL_H

#include "pithread.h"
#include "atomic_impl.h"
#include "value_impl.h"
#include "refcount_impl.h"
#include "commit_impl.h"
#include "channel_impl.h"

// forward declarations
struct _PICC_SchedPool;

#define DEFAULT_PC_ENTRY   0
#define PICC_INVALID_PC    (-1)
#define PICC_FUEL_INIT     10
#define PICC_CLOCK_MAX_INT 100000

typedef struct _PICC_Clock {
  PICC_AtomicInt *value;
} PICC_Clock;

struct _PICC_PiThread {
  PICC_Status status;
  int *enabled;
  int enabled_length;
  PICC_ManagedValSet *managed_values;
  PICC_Value *env;
  int env_length;
  PICC_Commit *commit;
  PICC_CommitList *commits;
  PICC_PiThreadProc *proc;
  PICC_Label pc;
  PICC_Value val;
  PICC_Clock *clock;
  int safe_choice;
  int fuel;
  pthread_mutex_t *lock;
};

PICC_Clock *PICC_clock_alloc();
void PICC_clock_free(PICC_Clock *);

void PICC_pithread_free(PICC_PiThread *);

int PICC_process_can_awake(PICC_Commit *);

#endif /* !__PITHREAD_IMPL_H */
