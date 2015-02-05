#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include "commit_impl.h"
#include "errors_impl.h"
#include "channel_impl.h"
#include "pithread_impl.h"


/*******************************************************************
 * Commitment structure and operations                             *
 *******************************************************************/

PICC_Commit *PICC_commit_alloc()
{
  PICC_Commit *commit = malloc(sizeof(PICC_Commit));
  if (commit == NULL)
    PICC_CRASH(ERR_OUT_OF_MEMORY);
  return commit;
}

void PICC_commit_free(PICC_Commit *commit)
{
  switch (commit->type) {
    case PICC_IN_COMMIT:
      free(commit->content.as_in);
      break;
    case PICC_OUT_COMMIT:
      free(commit->content.as_out);
      break;
  }
  free(commit);
}

PICC_PiThread *PICC_commit_get_pithread(PICC_Commit *commit)
{
  return commit->thread;
}

int PICC_commit_get_refvar(PICC_Commit *commit)
{
  assert(commit->type == PICC_IN_COMMIT);
  return commit->content.as_in->refvar;
}

PICC_Value PICC_commit_call_evalfun(PICC_Commit *commit)
{
  assert(commit->type == PICC_OUT_COMMIT);
  return commit->content.as_out->eval(commit->thread);
}

int PICC_commit_is_valid(PICC_Commit *commit)
{
  int res = commit->clock == commit->thread->clock;
  return res && commit->clockval == PICC_atomicint_get(commit->thread->clock->value);
}

void PICC_register_output_commitment(PICC_PiThread *thread, PICC_Channel *chan,
                                     PICC_EvalFunction eval, int cont_pc)
{
  PICC_Commit *commit = PICC_commit_alloc();
  commit->type           = PICC_OUT_COMMIT;
  commit->thread         = thread;
  commit->clock          = thread->clock;
  commit->clockval       = PICC_atomicint_get(thread->clock->value);
  commit->cont_pc        = cont_pc;
  commit->channel        = chan;
  commit->content.as_out = malloc(sizeof(PICC_OutCommit));
  if (commit->content.as_out == NULL)
    PICC_CRASH(ERR_OUT_OF_MEMORY);
  commit->content.as_out->eval = eval;

  PICC_commitlist_push_front(chan->outcommits, commit);
  PICC_commitlist_push_front(thread->commits, commit);
}

void PICC_register_input_commitment(PICC_PiThread *thread, PICC_Channel *chan,
                                    int ref_var, int cont_pc)
{
  PICC_Commit *commit = PICC_commit_alloc();
  commit->type          = PICC_IN_COMMIT;
  commit->thread        = thread;
  commit->clock         = thread->clock;
  commit->clockval      = PICC_atomicint_get(thread->clock->value);
  commit->cont_pc       = cont_pc;
  commit->channel       = chan;
  commit->content.as_in = malloc(sizeof(PICC_InCommit));
  if (commit->content.as_in == NULL)
    PICC_CRASH(ERR_OUT_OF_MEMORY);
  commit->content.as_in->refvar = ref_var;

  PICC_commitlist_push_front(chan->incommits, commit);
  PICC_commitlist_push_front(thread->commits, commit);
}
 

/*******************************************************************
 * Commitments lists and operations                                *
 *******************************************************************/

PICC_CommitList *PICC_commitlist_alloc()
{
  PICC_CommitList *list = malloc(sizeof(PICC_CommitList));
  if (list == NULL)
    PICC_CRASH(ERR_OUT_OF_MEMORY);
  list->head = NULL;
  list->tail = NULL;
  list->size = 0;
  return list;
}

void PICC_commitlist_free(PICC_CommitList *cl)
{
  PICC_CommitListElement *elem = cl->head;
  PICC_CommitListElement *next = NULL;
  while (elem) {
    next = elem->next;
    PICC_commit_free(elem->commit);
    free(elem);
    elem = next;
  }
  free(cl);
}

void PICC_commitlist_push_front(PICC_CommitList *cl, PICC_Commit *commit)
{
  PICC_CommitListElement *elem = malloc(sizeof(PICC_CommitListElement));
  if (elem == NULL)
    PICC_CRASH(ERR_OUT_OF_MEMORY);
  elem->commit = commit;

  if (cl->head == NULL) {
    assert(cl->tail == NULL);
    elem->next = NULL;
    cl->head = cl->tail = elem;
  } else {
    assert(cl->tail != NULL);
    elem->next = cl->head;
    cl->head = elem;
  }
  cl->size++;
}

int PICC_commitlist_is_empty(PICC_CommitList *cl)
{
  return (cl->size == 0);
}

PICC_Commit *PICC_commitlist_fetch(PICC_CommitList *cl)
{
  PICC_Commit *commit = NULL;
  if (!PICC_commitlist_is_empty(cl)) {
    PICC_CommitListElement *elem = cl->head;
    commit = elem->commit;
    pthread_mutex_lock(commit->thread->lock);
    if (cl->size == 1) {
      cl->head = cl->tail = NULL;
    } else {
      cl->head = elem->next;
    }
    cl->size --;
    free(elem);
    pthread_mutex_unlock(commit->thread->lock);
  }
  return commit;
}

PICC_Commit *PICC_fetch_input_commitment(PICC_Channel *chan)
{
  PICC_Commit *commit = NULL;

  do {
    commit = PICC_commitlist_fetch(chan->incommits);
  } while (commit && !PICC_commit_is_valid(commit));

  return commit;
}

PICC_Commit *PICC_fetch_output_commitment(PICC_Channel *chan)
{
  PICC_Commit *commit = NULL;

  do {
    commit = PICC_commitlist_fetch(chan->outcommits);
  } while (commit && !PICC_commit_is_valid(commit));

  return commit;
}

