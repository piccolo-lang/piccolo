#ifndef __COMMIT_IMPL_H
#define __COMMIT_IMPL_H

#include "commit.h"

typedef int PICC_Label;
typedef struct _PICC_Clock PICC_Clock;


/*******************************************************************
 * Commitment structure and operations                             *
 *******************************************************************/

typedef enum {
  PICC_IN_COMMIT,
  PICC_OUT_COMMIT,
} PICC_CommitType;

typedef enum {
  PICC_CANNOT_ACQUIRE,
  PICC_VALID_COMMIT,
  PICC_INVALID_COMMIT,
} PICC_CommitStatus;

typedef struct {
  int refvar;
} PICC_InCommit;

typedef struct {
  PICC_EvalFunction eval;
} PICC_OutCommit;

struct _PICC_Commit {
  PICC_CommitType type;
  PICC_PiThread *thread;
  PICC_Clock *clock;
  int clockval;
  PICC_Label cont_pc;
  PICC_Channel *channel;
  union {
    PICC_InCommit *as_in;
    PICC_OutCommit *as_out;
  } content;
};

PICC_Commit *PICC_commit_alloc();
void PICC_commit_free(PICC_Commit *);


/*******************************************************************
 * Commitments lists and operations                                *
 *******************************************************************/

typedef struct _PICC_CommitListElement {
  PICC_Commit *commit;
  struct _PICC_CommitListElement *next;
} PICC_CommitListElement;

typedef struct _PICC_CommitList {
  PICC_CommitListElement *head;
  PICC_CommitListElement *tail;
  int size;
} PICC_CommitList;

PICC_CommitList *PICC_commitlist_alloc();
void PICC_commitlist_free(PICC_CommitList *);

void PICC_commitlist_push_front(PICC_CommitList *, PICC_Commit *);
int PICC_commitlist_is_empty(PICC_CommitList *);

PICC_Commit *PICC_fetch_input_commitment(PICC_Channel *);
PICC_Commit *PICC_fetch_output_commitment(PICC_Channel *);

#endif /* !__COMMIT_IMPL_H */
