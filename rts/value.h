#ifndef __VALUE_H
#define __VALUE_H

#include <pthread.h>
#include "channel.h"

typedef struct {
  unsigned int header;
  void *data;
} PICC_Value;


void PICC_novalue_init(PICC_Value *);
void PICC_boolvalue_init(PICC_Value *, int);
void PICC_intvalue_init(PICC_Value *, int);
void PICC_floatvalue_init(PICC_Value *, double);

int PICC_boolvalue_unbox(PICC_Value *);

void PICC_stringvalue_init(PICC_Value *, char *);

void PICC_channelvalue_init(PICC_Value *);
PICC_Channel *PICC_channelvalue_unbox(PICC_Value *);

#endif /* !__VALUE_H */
