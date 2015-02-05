#ifndef __ATOMIC_IMPL_H
#define __ATOMIC_IMPL_H

#include <pthread.h>

typedef struct _PICC_AtomicInt {
  int value;
  pthread_mutex_t *lock;
} PICC_AtomicInt;

PICC_AtomicInt *PICC_atomicint_alloc(int);
void PICC_atomicint_free(PICC_AtomicInt *);

int PICC_atomicint_get(PICC_AtomicInt *);
int PICC_atomicint_compare_and_swap(PICC_AtomicInt *, int, int);

#endif /* !__ATOMIC_IMPL_H */
