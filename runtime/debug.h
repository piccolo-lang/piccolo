#ifndef __DEBUG_H
#define __DEBUG_H

#include "pithread.h"
#include "scheduler.h"

void PICC_debug_event(int event_id, PICC_PiThread *pt, PICC_SchedPool *sched);

#endif /* !__DEBUG_H */
