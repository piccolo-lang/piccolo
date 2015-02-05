#include <assert.h>
#include <stdio.h>
#include "primitives.h"
#include "value_impl.h"

void corearith_add(PICC_Value *res, PICC_Value *a, PICC_Value *b)
{
  assert(IS_INTEGER(a));
  assert(IS_INTEGER(b));
  PICC_intvalue_init(res, a->data.as_int + b->data.as_int);
}

void corearith_sub(PICC_Value *res, PICC_Value *a, PICC_Value *b)
{
  assert(IS_INTEGER(a));
  assert(IS_INTEGER(b));
  PICC_intvalue_init(res, a->data.as_int - b->data.as_int);
}

void corearith_mul(PICC_Value *res, PICC_Value *a, PICC_Value *b)
{
  assert(IS_INTEGER(a));
  assert(IS_INTEGER(b));
  PICC_intvalue_init(res, a->data.as_int * b->data.as_int);
}

void corearith_div(PICC_Value *res, PICC_Value *a, PICC_Value *b)
{
  assert(IS_INTEGER(a));
  assert(IS_INTEGER(b));
  PICC_intvalue_init(res, a->data.as_int / b->data.as_int);
}

void corearith_mod(PICC_Value *res, PICC_Value *a, PICC_Value *b)
{
  assert(IS_INTEGER(a));
  assert(IS_INTEGER(b));
  PICC_intvalue_init(res, a->data.as_int % b->data.as_int);
}

void corearith_equals(PICC_Value *res, PICC_Value *a, PICC_Value *b)
{
  assert(IS_INTEGER(a));
  assert(IS_INTEGER(b));
  if (a->data.as_int == b->data.as_int)
    PICC_boolvalue_init(res, 1);
  else
    PICC_boolvalue_init(res, 0);
}

void corearith_less_than(PICC_Value *res, PICC_Value *a, PICC_Value *b)
{
  assert(IS_INTEGER(a));
  assert(IS_INTEGER(b));
  if (a->data.as_int < b->data.as_int)
    PICC_boolvalue_init(res, 1);
  else
    PICC_boolvalue_init(res, 0);
}

void coreio_print_str(PICC_Value *res, PICC_Value *s)
{
  assert(IS_STRING(s));
  PICC_StringHandle *str = (PICC_StringHandle *)s->data.as_handle;
  printf("%s", str->data);
  PICC_novalue_init(res);
}

void coreio_print_int(PICC_Value *res, PICC_Value *i)
{
  assert(IS_INTEGER(i));
  printf("%d", i->data.as_int);
  PICC_novalue_init(res);
}
