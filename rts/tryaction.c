#include <assert.h>
#include <stdlib.h>
#include "tryaction.h"
#include "commit_impl.h"
#include "channel_impl.h"
#include "errors_impl.h"
#include "pithread_impl.h"

PICC_Commit *PICC_try_output_action(PICC_Channel *chan, PICC_TryResult *res)
{
  if (chan->global_rc == 1) {
    *res = PICC_TRY_DISABLED;
    return NULL;
  }

  PICC_Commit *commit  = NULL;
  PICC_CommitStatus ok = PICC_CANNOT_ACQUIRE;

  do {
    // search for a valid commitment
    commit = PICC_fetch_input_commitment(chan);
    if (commit == NULL) {
      // if no valid or invalid commitment left, need to
      // make an output commitment
      *res = PICC_TRY_COMMIT;
      return NULL;
    }

    // here, we have a candidate input commitment

    // try to awake the commiting thread
    do {
      ok = PICC_process_can_awake(commit);

      if (ok == PICC_CANNOT_ACQUIRE) {
        sched_yield();
      }
    } while (ok == PICC_CANNOT_ACQUIRE);

    // here we have either a valid commitment and an awoken thread,
    // or an invalid commitment that we just removed (lazy deletion)

    if (ok == PICC_VALID_COMMIT) {
      // ok, everything's fine: a synchronization will occur
      *res = PICC_TRY_ENABLED;
      return commit;
    }
  } while (!(PICC_commitlist_is_empty(chan->incommits)));

  PICC_CRASH(ERR_DEAD_CODE_REACHED);
  return NULL;
}

PICC_Commit *PICC_try_input_action(PICC_Channel *chan, PICC_TryResult *res)
{
  if (chan->global_rc == 1) {
    *res = PICC_TRY_DISABLED;
    return NULL;
  }

  PICC_Commit *commit = NULL;
  PICC_CommitStatus ok = PICC_CANNOT_ACQUIRE;

  do {
    // search for a valid commitment
    commit = PICC_fetch_output_commitment(chan);
    if (commit == NULL) {
      // if no valid or invalid commitment left, need to
      // make an input commitment
      *res = PICC_TRY_COMMIT;
      return NULL;
    }

    // here, we have a candidate output commitment

    // try to awake the commiting thread
    do {
      ok = PICC_process_can_awake(commit);

      if (ok == PICC_CANNOT_ACQUIRE) {
        sched_yield();
      }
    } while (ok == PICC_CANNOT_ACQUIRE);

    // here we have either a valid commitment and an awoken thread,
    // or an invalid commitment that we just removed (lazy deletion)

    if (ok == PICC_VALID_COMMIT) {
      // ok, everything's fine: a synchronization will occur
      *res = PICC_TRY_ENABLED;
      return commit;
    }
  } while (!(PICC_commitlist_is_empty(chan->incommits)));

  PICC_CRASH(ERR_DEAD_CODE_REACHED);
  return NULL;
}

PICC_Channel **PICC_channelarray_alloc(int size)
{
  PICC_Channel **array = malloc(size * sizeof(PICC_Channel *));
  if (array == NULL)
    PICC_CRASH(ERR_OUT_OF_MEMORY);
  return array;
}

void PICC_channelarray_unlock(PICC_Channel **chans, int nb_chans)
{
  for (int i = 0 ; i < nb_chans ; i++) {
    pthread_mutex_unlock(chans[i]->lock);
  }
  free(chans);
}

int PICC_channelarray_lock_and_register(PICC_Channel **chans, int *nb_chans, PICC_PiThread *pt, PICC_Channel *chan)
{
  for (int i = 0 ; i < *nb_chans ; i++) {
    if (chans[i] == chan)
      return 1;
  }

  if (!(PICC_process_lock_channel(pt, chan))) {
    return 0;
  }

  chans[*nb_chans] = chan;
  *nb_chans += 1;
  return 1;
}

