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
  PICC_NoValue *v = (PICC_NoValue *)val;
  v->header = MAKE_HEADER(TAG_NOVALUE, 0);
  v->data   = NULL;
}

void PICC_boolvalue_init(PICC_Value *val, int b)
{
  PICC_BoolValue *v = (PICC_BoolValue *)val;
  if (b)
    v->header = MAKE_HEADER(TAG_BOOLEAN, 1);
  else
    v->header = MAKE_HEADER(TAG_BOOLEAN, 0);
  v->data = NULL;
}

int PICC_boolvalue_unbox(PICC_Value *val)
{
  assert(IS_BOOLEAN(val));
  return GET_VALUE_CTRL(((PICC_BoolValue *)val)->header);
}

void PICC_intvalue_init(PICC_Value *val, int i)
{
  PICC_IntValue *v = (PICC_IntValue *)val;
  v->header = MAKE_HEADER(TAG_INTEGER, 0);
  v->data   = i;
}


/*****************************************************************************
 * Float values                                                             *
 *****************************************************************************/

void PICC_floatvalue_init(PICC_Value *val, double fl)
{
  PICC_FloatValue *v = (PICC_FloatValue *)val;
  v->header = MAKE_HEADER(TAG_FLOAT, 0);
  v->data   = fl;
}


/*****************************************************************************
 * String values                                                             *
 *****************************************************************************/

void PICC_stringvalue_init(PICC_Value *val, char *str)
{
  PICC_StringValue *v = (PICC_StringValue *)val;
  v->header = MAKE_HEADER(TAG_STRING, strlen(str) + 1);
  v->data   = PICC_stringhandle_alloc(str);
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
  PICC_ChannelValue *v = (PICC_ChannelValue *)val;
  v->header = MAKE_HEADER(TAG_CHANNEL, 0);
  v->data   = PICC_channel_alloc();
}

PICC_Channel *PICC_channelvalue_unbox(PICC_Value *val)
{
  return ((PICC_ChannelValue *)val)->data;
}

