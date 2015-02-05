#ifndef __RUNTIME_H
#define __RUNTIME_H

#include "pithread.h"
#include "scheduler.h"
#include "value.h"
#include "primitives.h"
#include "queue.h"
#include "tryaction.h"
#include "channel.h"

void PICC_main(int nb_core_threads, PICC_PiThreadProc *entrypoint,
               int std_gc_fuel, int quick_gc_fuel, int active_factor,
               int entry_env_length, int entry_enabled_length);

#endif /* !__RUNTIME_H */
