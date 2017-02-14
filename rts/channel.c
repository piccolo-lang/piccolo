#include <assert.h>
#include <stdlib.h>
#include "channel_impl.h"
#include "value_impl.h"
#include "errors.h"

PICC_Channel *PICC_channel_alloc()
{
  PICC_Channel *chan = malloc(sizeof(PICC_Channel));
  if (chan == NULL)
    PICC_CRASH(ERR_OUT_OF_MEMORY);
  chan->global_rc = 0;
  chan->lock      = malloc(sizeof(pthread_mutex_t));
  if (chan->lock == NULL)
    PICC_CRASH(ERR_OUT_OF_MEMORY);
  pthread_mutex_init(chan->lock, NULL);
  chan->reclaim    = PICC_channel_free;
  chan->incommits  = PICC_commitlist_alloc();
  chan->outcommits = PICC_commitlist_alloc();

  return chan;
}

void PICC_channel_free(PICC_ValueHandle *handle)
{
  PICC_Channel *chan = (PICC_Channel *)handle;
  pthread_mutex_destroy(handle->lock);
  free(handle->lock);
  PICC_commitlist_free(chan->incommits);
  PICC_commitlist_free(chan->outcommits);
  free(chan);
}

void PICC_unlock_channel(PICC_Channel *chan)
{
  pthread_mutex_unlock(chan->lock);
}

