#ifndef __PITHREAD_H
#define __PITHREAD_H

#include "scheduler.h"
#include "value.h"
#include "commit.h"

typedef struct _PICC_PiThread PICC_PiThread;

typedef int PICC_Label;

typedef enum _PICC_Status {
  PICC_STATUS_RUN,
  PICC_STATUS_CALL,
  PICC_STATUS_WAIT,
  PICC_STATUS_ENDED,
  PICC_STATUS_BLOCKED,
} PICC_Status;

typedef void (PICC_PiThreadProc)(PICC_SchedPool *, PICC_PiThread *);

PICC_PiThread *PICC_pithread_alloc(int env_length, int enabled_length);

PICC_Value *PICC_get_register_ptr(PICC_PiThread *);

int PICC_get_pc(PICC_PiThread *);
PICC_Value PICC_get_register(PICC_PiThread *);
PICC_Value *PICC_get_env(PICC_PiThread *, int);
int PICC_get_enabled(PICC_PiThread *, int);
int PICC_get_fuel(PICC_PiThread *);

void PICC_set_status(PICC_PiThread *, PICC_Status);
void PICC_set_proc(PICC_PiThread *, PICC_PiThreadProc);
void PICC_set_pc(PICC_PiThread *, PICC_Label);
void PICC_set_register(PICC_PiThread *, PICC_Value *);
void PICC_set_env(PICC_PiThread *, int, PICC_Value);
void PICC_set_enabled(PICC_PiThread *, int, int);
void PICC_set_safechoice(PICC_PiThread *, int);

void PICC_decr_fuel(PICC_PiThread *);

void PICC_forget_all_values(PICC_PiThread *);
void PICC_register_env_value(PICC_PiThread *, int);
void PICC_register_register_value(PICC_PiThread *);

void PICC_process_end(PICC_PiThread *, PICC_Status);
void PICC_process_yield(PICC_PiThread *, PICC_SchedPool *);
void PICC_process_wait(PICC_PiThread *, PICC_SchedPool *);
void PICC_process_awake(PICC_Commit *, PICC_SchedPool *);
void PICC_process_lock(PICC_PiThread *);

int PICC_process_lock_channel(PICC_PiThread *, PICC_Channel *);

#endif /* !__PITHREAD_H */
