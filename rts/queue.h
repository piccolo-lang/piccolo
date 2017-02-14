#ifndef __QUEUE_H
#define __QUEUE_H

typedef struct _PICC_PiThread PICC_PiThread;

typedef struct _PICC_ReadyQueue PICC_ReadyQueue;

void PICC_readyqueue_push_front(PICC_ReadyQueue *, PICC_PiThread *);

#endif /* !__QUEUE_H */
