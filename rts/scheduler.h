#ifndef __SCHEDULER_H
#define __SCHEDULER_H

#include "queue.h"

typedef struct _PICC_SchedPool PICC_SchedPool;

PICC_ReadyQueue *PICC_sched_get_readyqueue(PICC_SchedPool *);

#endif /* !__SCHEDULER_H */
