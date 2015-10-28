#include <stdio.h>
#include "debug_impl.h"

static FILE *debug_log_fd;

void PICC_debug_open()
{
  debug_log_fd = fopen(DEBUGFILE, "w+");
}

void PICC_debug_close()
{
  fclose(debug_log_fd);
}
