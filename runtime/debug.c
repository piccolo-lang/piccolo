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

static char debugging_session = 0;
static int sock = 0;
static int csock = 0;

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

void PICC_debug_event(int event_id)
{
  if (!debugging_session)
    return;

  fflush(stdout);
  char buffer[64];

  // sending the event to the debugger
  buffer[0] = (char)event_id;
  write(csock, buffer, 1);

  // waiting for the "continue" order
  while (1) {
    read(csock, buffer, 1);
    if (buffer[0] == 'c') break;
  }
}

void PICC_debug_end()
{
  if (!debugging_session)
    return;

  close(sock);
  close(csock);
}
