#ifndef __REFCOUNT_IMPL_H
#define __REFCOUNT_IMPL_H

#include <pthread.h>
#include "value_impl.h"

typedef enum {
  PICC_UNKNOWN,
  PICC_KNOWN,
  PICC_FORGET,
} PICC_ManagedValState;

typedef struct {
  PICC_ManagedValState state;
  PICC_ValueHandle *handle;
} PICC_ManagedValElement;

typedef struct _PICC_ManagedValSet {
  PICC_ManagedValElement *content;
  int size;
  int max_size;
} PICC_ManagedValSet;

#define DEFAULT_MAX_SIZE 10

PICC_ManagedValSet *PICC_managedvalset_alloc();
void PICC_managedvalset_free(PICC_ManagedValSet *);

void PICC_managedvalue_dec_refcount(PICC_ValueHandle *);
void PICC_managedvalue_inc_refcount(PICC_ValueHandle *);

void PICC_managedvalset_register(PICC_ManagedValSet *, PICC_Value *);
void PICC_managedvalset_forget_all(PICC_ManagedValSet *);
void PICC_managedvalset_clean(PICC_ManagedValSet *);
void PICC_managedvalset_clean_all(PICC_ManagedValSet *);

#endif /* !__REFCOUNT_IMPL_H */
