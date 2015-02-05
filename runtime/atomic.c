#include <assert.h>
#include <stdlib.h>
#include <pthread.h>
#include "atomic_impl.h"
#include "errors_impl.h"

PICC_AtomicInt *PICC_atomicint_alloc(int i)
{
  PICC_AtomicInt *atom = malloc(sizeof(PICC_AtomicInt));
  if (atom == NULL)
    PICC_CRASH(ERR_OUT_OF_MEMORY);
  atom->value = i;
  atom->lock = malloc(sizeof(pthread_mutex_t));
  if (atom->lock == NULL)
    PICC_CRASH(ERR_OUT_OF_MEMORY);
  pthread_mutex_init(atom->lock, NULL);
  return atom;
}

void PICC_atomicint_free(PICC_AtomicInt *atom)
{
  pthread_mutex_destroy(atom->lock);
  free(atom->lock);
  free(atom);
}

int PICC_atomicint_get(PICC_AtomicInt *atom)
{
  pthread_mutex_lock(atom->lock);
  int i = atom->value;
  pthread_mutex_unlock(atom->lock);
  return i;
}

int PICC_atomicint_compare_and_swap(PICC_AtomicInt *atom, int expected, int new)
{
  pthread_mutex_lock(atom->lock);

  int old = atom->value;
  if (old == expected) {
    atom->value = new;
  }

  pthread_mutex_unlock(atom->lock);
  return old;
}
