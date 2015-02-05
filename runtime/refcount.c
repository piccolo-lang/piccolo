#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include "refcount_impl.h"
#include "errors_impl.h"

PICC_ManagedValSet *PICC_managedvalset_alloc()
{
  PICC_ManagedValSet *set = malloc(sizeof(PICC_ManagedValSet));
  if (set == NULL)
    PICC_CRASH(ERR_OUT_OF_MEMORY);
  set->content  = malloc(sizeof(PICC_ManagedValElement) * DEFAULT_MAX_SIZE);
  if (set->content == NULL)
    PICC_CRASH(ERR_OUT_OF_MEMORY);
  set->size     = 0;
  set->max_size = DEFAULT_MAX_SIZE;
  for (int i = 0 ; i < set->max_size ; i++)
    set->content[i].state = PICC_UNKNOWN;
  return set;
}

void PICC_managedvalset_free(PICC_ManagedValSet *set)
{
  PICC_ManagedValElement *elem = NULL;
  for (int i = 0 ; i < set->size ; i++) {
    elem = &(set->content[i]);
    assert(elem->state == PICC_UNKNOWN);
  }

  free(set->content);
  free(set);
}

void PICC_managedvalue_dec_refcount(PICC_ValueHandle *handle)
{
  pthread_mutex_lock(handle->lock);
  handle->global_rc--;
  assert(handle->global_rc >= 0);
  if (handle->global_rc == 0) {
    pthread_mutex_unlock(handle->lock);
    handle->reclaim(handle);
    return;
  }
  pthread_mutex_unlock(handle->lock);
}

void PICC_managedvalue_inc_refcount(PICC_ValueHandle *handle)
{
  pthread_mutex_lock(handle->lock);
  handle->global_rc++;
  pthread_mutex_unlock(handle->lock);
}

void PICC_managedvalset_register(PICC_ManagedValSet *set, PICC_Value *value)
{
  assert(IS_MANAGED(value));
  PICC_ManagedValElement *elem = NULL;
  PICC_ManagedValElement *new  = NULL;
  for (int i = 0 ; i < set->size ; i++) {
    elem = &(set->content[i]);

    if (elem->handle == value->data.as_handle) {
      assert(elem->state != PICC_UNKNOWN);
      if (elem->state == PICC_FORGET)
        elem->state = PICC_KNOWN;
      return;
    } else {
      if (elem->state == PICC_UNKNOWN)
        new = elem;
    }
  }

  if (new == NULL) {
    if (set->size == set->max_size) {
      set->content = realloc(set->content, sizeof(PICC_ManagedValElement) * set->max_size * 2);
      if (set->content == NULL)
        PICC_CRASH(ERR_OUT_OF_MEMORY);
      set->max_size *= 2;
    }
    new = &(set->content[set->size]);
    set->size += 1;
  }

  new->state  = PICC_KNOWN;
  new->handle = value->data.as_handle;
  PICC_managedvalue_inc_refcount(value->data.as_handle);
}

void PICC_managedvalset_forget_all(PICC_ManagedValSet *set)
{
  PICC_ManagedValElement *elem = NULL;
  for (int i = 0 ; i < set->size ; i++) {
    elem = &(set->content[i]);
    if (elem->state == PICC_KNOWN)
      elem->state = PICC_FORGET;
  }
}

void PICC_managedvalset_clean(PICC_ManagedValSet *set)
{
  PICC_ManagedValElement *elem = NULL;
  for (int i = 0 ; i < set->size ; i++) {
    elem = &(set->content[i]);
    if (elem->state == PICC_FORGET) {
      elem->state = PICC_UNKNOWN;
      PICC_managedvalue_dec_refcount(elem->handle);
      elem->handle = NULL;
    }
  }
}

void PICC_managedvalset_clean_all(PICC_ManagedValSet *set)
{
  PICC_ManagedValElement *elem = NULL;
  for (int i = 0 ; i < set->size ; i++) {
    elem = &(set->content[i]);
    if (elem->state == PICC_KNOWN || elem->state == PICC_FORGET) {
      elem->state = PICC_UNKNOWN;
      PICC_managedvalue_dec_refcount(elem->handle);
      elem->handle = NULL;
    }
  }
}

