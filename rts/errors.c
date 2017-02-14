#include <stdlib.h>
#include <stdio.h>
#include "errors.h"

char *PICC_get_err_message(int errcode)
{
  switch (errcode) {
    case ERR_OUT_OF_MEMORY:
      return "memory allocation";
    case ERR_PTHREAD_CREATE:
      return "pthread creation";
    case ERR_DEADLOCK:
      return "a deadlock occured";
    default:
      return "unknwon error";
  }
}

void PICC_crash(int errcode, char *file, int line)
{
  printf("error: %s (%s %d)\n", PICC_get_err_message(errcode), file, line);
  exit(2);
}
