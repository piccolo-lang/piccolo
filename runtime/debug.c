#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <unistd.h>
#include <netdb.h>
#include "debug_impl.h"
#include "pithread_impl.h"
#include "scheduler_impl.h"

#define BUFSIZE 8192

static char debugging_session = 0;
static int sock = 0;
static int csock = 0;

static unsigned int __PICC_debug_format_pithread(char *, unsigned int, PICC_PiThread *);
static unsigned int __PICC_debug_format_value(char *, unsigned int, PICC_Value *);

void PICC_debug_begin()
{
  char* picc_env = getenv("PICC_DEBUG");
  if (picc_env == NULL) return;
  if (!strncmp(picc_env, "true", 5))
    debugging_session = 1;
  if (!debugging_session)
    return;

  // server
  sock = socket(AF_INET, SOCK_STREAM, 0);
  struct sockaddr_in sin = { 0 };

  sin.sin_addr.s_addr = htonl(INADDR_ANY);
  sin.sin_family = AF_INET;
  sin.sin_port = htons(5049);
  
  int enable = 1;
  setsockopt(sock, SOL_SOCKET, SO_REUSEADDR, &enable, sizeof(int));

  bind(sock, (struct sockaddr*)&sin, sizeof(sin));
  listen(sock, 1);

  // client connection
  struct sockaddr_in csin = { 0 };
  socklen_t sinsize = sizeof(csin);

  csock = accept(sock, (struct sockaddr*)&csin, &sinsize);
}

void PICC_debug_event(int event_id, PICC_PiThread *pt, PICC_SchedPool *sched)
{
  if (!debugging_session)
    return;

  fflush(stdout);
  char buffer[BUFSIZE];
  unsigned int n;

  // sending the event to the debugger
  buffer[0] = (char)event_id;
  write(csock, buffer, 1);

  // waiting for the "continue" order
  while (1) {
    read(csock, buffer, 1);
    if (buffer[0] == 'c') break;

    if (buffer[0] == 'i') {
      n = 0;
      n += snprintf(buffer+n, BUFSIZE-n, "{ \"currentThread\":\n");
      n = __PICC_debug_format_pithread(buffer, n, pt);
      n += snprintf(buffer+n, BUFSIZE-n, ",\n");

      n += snprintf(buffer+n, BUFSIZE-n, "  \"readyQueue\": [\n");
      PICC_ReadyQueue *ready = sched->ready;
      PICC_QueueCell *current = ready->head;
      if (current) {
        n = __PICC_debug_format_pithread(buffer, n, current->thread);
        current = current->next;
      }
      while (current) {
        n += snprintf(buffer+n, BUFSIZE-n, ",\n");
        n = __PICC_debug_format_pithread(buffer, n, current->thread);
        current = current->next;
      }
      n += snprintf(buffer+n, BUFSIZE-n, "  ],\n");

      n += snprintf(buffer+n, BUFSIZE-n, "  \"waitQueue\": [\n");
      PICC_WaitQueue *wait = sched->wait;
      current = wait->head;
      if (current) {
        n = __PICC_debug_format_pithread(buffer, n, current->thread);
        current = current->next;
      }
      while (current) {
        n += snprintf(buffer+n, BUFSIZE-n, ",\n");
        n = __PICC_debug_format_pithread(buffer, n, current->thread);
        current = current->next;
      }
      n += snprintf(buffer+n, BUFSIZE-n, "  ]\n");

      n += snprintf(buffer+n, BUFSIZE-n, "}");

      char buflength[4];
      buflength[0] = n & 0xFF;
      buflength[1] = (n >> 8)  & 0xFF;
      buflength[2] = (n >> 16) & 0xFF;
      buflength[3] = (n >> 24) & 0xFF;
      write(csock, buflength, 4);

      write(csock, buffer, n);
    }
  }
}

void PICC_debug_end()
{
  if (!debugging_session)
    return;

  close(sock);
  close(csock);
}

static unsigned int __PICC_debug_format_pithread(char *buffer,
                                                 unsigned int n,
                                                 PICC_PiThread *thread)
{
  n += snprintf(buffer+n, BUFSIZE-n, "    {\"threadId\": \"%p\",\n", thread);
  n += snprintf(buffer+n, BUFSIZE-n, "     \"status\": \"");
  switch (thread->status) {
    case PICC_STATUS_RUN:
      n += snprintf(buffer+n, BUFSIZE-n, "RUN");
      break;
    case PICC_STATUS_CALL:
      n += snprintf(buffer+n, BUFSIZE-n, "CALL");
      break;
    case PICC_STATUS_WAIT:
      n += snprintf(buffer+n, BUFSIZE-n, "WAIT");
      break;
    case PICC_STATUS_ENDED:
      n += snprintf(buffer+n, BUFSIZE-n, "ENDED");
      break;
    case PICC_STATUS_BLOCKED:
      n += snprintf(buffer+n, BUFSIZE-n, "BLOCKED");
      break;
    default:
      n += snprintf(buffer+n, BUFSIZE-n, "<error>");
  }
  n += snprintf(buffer+n, BUFSIZE-n, "\",\n");
  n += snprintf(buffer+n, BUFSIZE-n, "     \"env\": [\n");
  for (int i = 0 ; i < thread->env_length ; i++) {
    n += snprintf(buffer+n, BUFSIZE-n, "        ");
    n = __PICC_debug_format_value(buffer, n, thread->env + i);
    if (i != thread->env_length - 1)
      n += snprintf(buffer+n, BUFSIZE-n, ",\n");
    else
      n += snprintf(buffer+n, BUFSIZE-n, "\n");
  }
  n += snprintf(buffer+n, BUFSIZE-n, "     ]");
  n += snprintf(buffer+n, BUFSIZE-n, "}");
  return n;
}

static unsigned int __PICC_debug_format_value(char *buffer,
                                              unsigned int n,
                                              PICC_Value *value)
{
  n += snprintf(buffer+n, BUFSIZE-n, "{");
  switch (GET_VALUE_TAG(value->header)) {
    case TAG_NOVALUE:
      n += snprintf(buffer+n, BUFSIZE-n, "\"valueType\": \"NOVALUE\"");
      break;
    case TAG_BOOLEAN:
      n += snprintf(buffer+n, BUFSIZE-n, "\"valueType\": \"BOOLEAN\", ");
      if (GET_VALUE_CTRL(value->header))
        n += snprintf(buffer+n, BUFSIZE-n, "\"valueVal\": true");
      else
        n += snprintf(buffer+n, BUFSIZE-n, "\"valueVal\": false");
      break;
    case TAG_INTEGER:
      n += snprintf(buffer+n, BUFSIZE-n, "\"valueType\": \"INTEGER\", ");
      n += snprintf(buffer+n, BUFSIZE-n, "\"valueVal\": %ld",
                    ((PICC_IntValue*)value)->data);
      break;
    case TAG_FLOAT:
      n += snprintf(buffer+n, BUFSIZE-n, "\"valueType\": \"FLOAT\", ");
      n += snprintf(buffer+n, BUFSIZE-n, "\"valueVal\": %f",
                    ((PICC_FloatValue*)value)->data);
      break;
    case TAG_TUPLE:
      n += snprintf(buffer+n, BUFSIZE-n, "\"valueType\": \"TUPLE\"");
      break;
    case TAG_STRING:
      n += snprintf(buffer+n, BUFSIZE-n, "\"valueType\": \"STRING\", ");
      n += snprintf(buffer+n, BUFSIZE-n, "\"valueVal\": \"%s\", ",
                    ((PICC_StringValue*)value)->data->data);
      n += snprintf(buffer+n, BUFSIZE-n, "\"valueRefCount\": %d",
                    ((PICC_StringValue*)value)->data->global_rc);
      break;
    case TAG_CHANNEL:
      n += snprintf(buffer+n, BUFSIZE-n, "\"valueType\": \"CHANNEL\", ");
      n += snprintf(buffer+n, BUFSIZE-n, "\"valueRefCount\": %d",
                    ((PICC_ChannelValue*)value)->data->global_rc);
      break;
    default:
      n += snprintf(buffer+n, BUFSIZE-n, "\"valueType\": \"OTHER\"");
  }
  n += snprintf(buffer+n, BUFSIZE-n, "}");
  return n;
}
