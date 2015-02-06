#ifndef __COMMIT_H
#define __COMMIT_H

#include "value.h"

typedef struct _PICC_Commit PICC_Commit;
typedef struct _PICC_PiThread PICC_PiThread;

typedef PICC_Value (*PICC_EvalFunction)(PICC_PiThread *);

PICC_PiThread *PICC_commit_get_pithread(PICC_Commit *);
int PICC_commit_get_refvar(PICC_Commit *);
PICC_Value PICC_commit_call_evalfun(PICC_Commit *);

void PICC_register_output_commitment(PICC_PiThread *, PICC_Channel *,
                                     PICC_EvalFunction, int);
void PICC_register_input_commitment(PICC_PiThread *, PICC_Channel *, int, int);

#endif /* !__COMMIT_H */
