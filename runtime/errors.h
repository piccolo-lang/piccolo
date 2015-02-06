#ifndef __ERRORS_H
#define __ERRORS_H

#define ERR_OUT_OF_MEMORY     0
#define ERR_PTHREAD_CREATE    1
#define ERR_DEADLOCK          2
#define ERR_DEAD_CODE_REACHED 3

void PICC_crash(int errcode, char *file, int line);

#define PICC_CRASH(errcode)                                 \
  do {                                                      \
    PICC_crash(errcode, __FILE__, __LINE__);                \
  } while (0)

#endif /* !__ERRORS_H */
