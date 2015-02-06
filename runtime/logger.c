#include <stdio.h>
#include <pthread.h>
#include <stdarg.h>
#include <sys/time.h>
#include "logger_impl.h"

static FILE *log_fd;
static pthread_mutex_t log_lock;

void PICC_logger_open()
{
  pthread_mutex_init(&log_lock, NULL);
  log_fd = fopen(LOGFILE, "w+");
}

void PICC_logger_log(const char *fmt, ...)
{
  va_list arg;

  struct timeval tval;
  gettimeofday(&tval, NULL);

  pthread_mutex_lock(&log_lock);

  fprintf(log_fd, "%ld ", (long)tval.tv_usec);
  va_start(arg, fmt);
  vfprintf(log_fd, fmt, arg);
  va_end(arg);

  fflush(log_fd);

  pthread_mutex_unlock(&log_lock);
}

void PICC_logger_close()
{
  pthread_mutex_destroy(&log_lock);
  fclose(log_fd);
}
