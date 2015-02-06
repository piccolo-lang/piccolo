#ifndef __TRYACTION_H
#define __TRYACTION_H

#include "commit.h"

typedef enum {
  PICC_TRY_DISABLED,
  PICC_TRY_ENABLED,
  PICC_TRY_COMMIT,
  PICC_TRY_ABORT,
} PICC_TryResult;

PICC_Commit *PICC_try_output_action(PICC_Channel *, PICC_TryResult *);
PICC_Commit *PICC_try_input_action(PICC_Channel *, PICC_TryResult *);

PICC_Channel **PICC_channelarray_alloc(int);
void PICC_channelarray_unlock(PICC_Channel **, int);
int PICC_channelarray_lock_and_register(PICC_Channel **, int *, PICC_PiThread*, PICC_Channel *);

#endif /* !__TRYACTION_H */
