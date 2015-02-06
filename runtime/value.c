#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "value_impl.h"
#include "channel_impl.h"
#include "errors.h"


/*****************************************************************************
 * Immediate values                                                          *
 *****************************************************************************/

void PICC_novalue_init(PICC_Value *val)
{
  val->header      = MAKE_HEADER(TAG_NOVALUE, 0);
  val->data.as_int = 0;
}

void PICC_boolvalue_init(PICC_Value *val, int b)
{
  val->header       = MAKE_HEADER(TAG_BOOLEAN, 0);
  val->data.as_bool = b ? 1 : 0;
}

int PICC_boolvalue_unbox(PICC_Value *val)
{
  assert(IS_BOOLEAN(val));
  return val->data.as_bool;
}

void PICC_intvalue_init(PICC_Value *val, int i)
{
  val->header      = MAKE_HEADER(TAG_INTEGER, 0);
  val->data.as_int = i;
}


/*****************************************************************************
 * String values                                                             *
 *****************************************************************************/

void PICC_stringvalue_init(PICC_Value *val, char *str)
{
  val->header         = MAKE_HEADER(TAG_STRING, strlen(str)+1);
  val->data.as_handle = (PICC_ValueHandle *)(PICC_stringhandle_alloc(str));
}

PICC_StringHandle *PICC_stringhandle_alloc(char *s)
{
  PICC_StringHandle *str = malloc(sizeof(PICC_StringHandle));
  if (str == NULL)
    PICC_CRASH(ERR_OUT_OF_MEMORY);
  str->global_rc      = 0;
  str->lock           = malloc(sizeof(pthread_mutex_t));
  if (str->lock == NULL)
    PICC_CRASH(ERR_OUT_OF_MEMORY);
  pthread_mutex_init(str->lock, NULL);
  str->reclaim        = PICC_stringhandle_free;
  str->data           = malloc(sizeof(char) * (strlen(s) + 1));
  if (str->data == NULL)
    PICC_CRASH(ERR_OUT_OF_MEMORY);
  strncpy(str->data, s, strlen(s));
  str->data[strlen(s)] = '\0';
  return str;
}

void PICC_stringhandle_free(PICC_ValueHandle *handle)
{
  PICC_StringHandle *str = (PICC_StringHandle *)handle;
  pthread_mutex_destroy(str->lock);
  free(str->lock);
  free(str->data);
  free(handle);
}


/*****************************************************************************
 * Channel values                                                            *
 *****************************************************************************/

void PICC_channelvalue_init(PICC_Value *val)
{
  val->header         = MAKE_HEADER(TAG_CHANNEL, 0);
  val->data.as_handle = (PICC_ValueHandle *)(PICC_channel_alloc());
}

PICC_Channel *PICC_channelvalue_unbox(PICC_Value *val)
{
  return (PICC_Channel *)val->data.as_handle;
}

