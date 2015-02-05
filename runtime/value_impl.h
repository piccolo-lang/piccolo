#ifndef __VALUE_IMPL_H
#define __VALUE_IMPL_H

#include "value.h"
#include "refcount_impl.h"
#include "channel.h"

typedef enum {
  TAG_RESERVED               = 0x00,
  TAG_NOVALUE                = 0x01,
  TAG_BOOLEAN                = 0x02,
  TAG_INTEGER                = 0x03,
  TAG_FLOAT                  = 0x04,
  TAG_TUPLE                  = 0x10,
  TAG_STRING                 = 0x11,
  TAG_CHANNEL                = 0xFD,
  TAG_USER_DEFINED_IMMEDIATE = 0xEE,
  TAG_USER_DEFINED_MANAGED   = 0xFF,
} PICC_TagValue;

#define WORD_SIZE 32
#define GET_VALUE_TAG(header) (((unsigned int)(header)) >> (WORD_SIZE - 8))

#define VALUE_CTRL_MASK (~(0xFF << (WORD_SIZE - 8)))
#define GET_VALUE_CTRL(header) ((unsigned int)(header) & VALUE_CTRL_MASK)

#define MAKE_HEADER(tag, ctrl) ((unsigned int)(((tag) << (WORD_SIZE - 8))) | ((ctrl) & VALUE_CTRL_MASK))

#define IS_MANAGED(value)                (GET_VALUE_TAG(value->header) & 0x10)

#define IS_NOVALUE(value)                (GET_VALUE_TAG(value->header) == TAG_NOVALUE)
#define IS_BOOLEAN(value)                (GET_VALUE_TAG(value->header) == TAG_BOOLEAN)
#define IS_INTEGER(value)                (GET_VALUE_TAG(value->header) == TAG_INTEGER)
#define IS_FLOAT(value)                  (GET_VALUE_TAG(value->header) == TAG_FLOAT)
#define IS_TUPLE(value)                  (GET_VALUE_TAG(value->header) == TAG_TUPLE)
#define IS_STRING(value)                 (GET_VALUE_TAG(value->header) == TAG_STRING)
#define IS_USER_DEFINED_IMMEDIATE(value) (GET_VALUE_TAG(value->header) == TAG_USER_DEFINED_IMMEDIATE)
#define IS_USER_DEFINED_MANAGED(value)   (GET_VALUE_TAG(value->header) == TAG_USER_DEFINED_MANAGED)


PICC_StringHandle *PICC_stringhandle_alloc(char *);
void PICC_stringhandle_free(PICC_ValueHandle *);

#endif /* !__VALUE_IMPL_H */
