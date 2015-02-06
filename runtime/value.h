#ifndef __VALUE_H
#define __VALUE_H

#include <pthread.h>
#include "channel.h"

typedef struct _PICC_ValueHandle PICC_ValueHandle;
typedef void (*PICC_Reclaimer)(PICC_ValueHandle *);

struct _PICC_ValueHandle {
  int global_rc;
  pthread_mutex_t *lock;
  PICC_Reclaimer reclaim;
  void *data;
};

typedef struct {
  unsigned int header;
  union {
    int as_int;
    int as_bool;
    PICC_ValueHandle *as_handle;
  } data;
} PICC_Value;


void PICC_novalue_init(PICC_Value *);
void PICC_boolvalue_init(PICC_Value *, int);
void PICC_intvalue_init(PICC_Value *, int);

int PICC_boolvalue_unbox(PICC_Value *);

typedef struct {
  int global_rc;
  pthread_mutex_t *lock;
  PICC_Reclaimer reclaim;
  char *data;
} PICC_StringHandle;

void PICC_stringvalue_init(PICC_Value *, char *);

void PICC_channelvalue_init(PICC_Value *);
PICC_Channel *PICC_channelvalue_unbox(PICC_Value *);

#endif /* !__VALUE_H */
