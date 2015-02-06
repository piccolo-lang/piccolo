#ifndef __CHANNEL_IMPL_H
#define __CHANNEL_IMPL_H

#include "channel.h"
#include "value_impl.h"
#include "commit_impl.h"

typedef enum {
  PI_CHANNEL = 0,
} PICC_ChannelKind;

struct _PICC_Channel {
  int global_rc;
  pthread_mutex_t *lock;
  PICC_Reclaimer reclaim;
  PICC_CommitList *incommits;
  PICC_CommitList *outcommits;
};

PICC_Channel *PICC_channel_alloc();
void PICC_channel_free(PICC_ValueHandle *);

#endif /* !__CHANNEL_IMPL_H */
