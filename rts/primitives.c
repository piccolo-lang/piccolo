#include <assert.h>
#include <stdio.h>
#include "primitives.h"
#include "value_impl.h"

void core_arith_add(PICC_Value *res, PICC_Value *a, PICC_Value *b)
{
  assert(IS_INTEGER(a));
  assert(IS_INTEGER(b));
  PICC_intvalue_init(res,
      ((PICC_IntValue *)a)->data + ((PICC_IntValue *)b)->data);
}

void core_arith_sub(PICC_Value *res, PICC_Value *a, PICC_Value *b)
{
  assert(IS_INTEGER(a));
  assert(IS_INTEGER(b));
  PICC_intvalue_init(res,
      ((PICC_IntValue *)a)->data - ((PICC_IntValue *)b)->data);
}

void core_arith_mul(PICC_Value *res, PICC_Value *a, PICC_Value *b)
{
  assert(IS_INTEGER(a));
  assert(IS_INTEGER(b));
  PICC_intvalue_init(res,
      ((PICC_IntValue *)a)->data * ((PICC_IntValue *)b)->data);
}

void core_arith_div(PICC_Value *res, PICC_Value *a, PICC_Value *b)
{
  assert(IS_INTEGER(a));
  assert(IS_INTEGER(b));
  PICC_intvalue_init(res,
      ((PICC_IntValue *)a)->data / ((PICC_IntValue *)b)->data);
}

void core_arith_mod(PICC_Value *res, PICC_Value *a, PICC_Value *b)
{
  assert(IS_INTEGER(a));
  assert(IS_INTEGER(b));
  PICC_intvalue_init(res,
      ((PICC_IntValue *)a)->data % ((PICC_IntValue *)b)->data);
}

void core_arith_equals(PICC_Value *res, PICC_Value *a, PICC_Value *b)
{
  assert(IS_INTEGER(a));
  assert(IS_INTEGER(b));
  if (((PICC_IntValue *)a)->data == ((PICC_IntValue *)b)->data)
    PICC_boolvalue_init(res, 1);
  else
    PICC_boolvalue_init(res, 0);
}

void core_arith_not_equals(PICC_Value *res, PICC_Value *a, PICC_Value *b)
{
  assert(IS_INTEGER(a));
  assert(IS_INTEGER(b));
  if (((PICC_IntValue *)a)->data != ((PICC_IntValue *)b)->data)
    PICC_boolvalue_init(res, 1);
  else
    PICC_boolvalue_init(res, 0);
}

void core_arith_less_than(PICC_Value *res, PICC_Value *a, PICC_Value *b)
{
  assert(IS_INTEGER(a));
  assert(IS_INTEGER(b));
  if (((PICC_IntValue *)a)->data < ((PICC_IntValue *)b)->data)
    PICC_boolvalue_init(res, 1);
  else
    PICC_boolvalue_init(res, 0);
}

void core_arith_less_or_eq_than(PICC_Value *res, PICC_Value *a, PICC_Value *b)
{
  assert(IS_INTEGER(a));
  assert(IS_INTEGER(b));
  if (((PICC_IntValue *)a)->data <= ((PICC_IntValue *)b)->data)
    PICC_boolvalue_init(res, 1);
  else
    PICC_boolvalue_init(res, 0);
}

void core_arith_greater_than(PICC_Value *res, PICC_Value *a, PICC_Value *b)
{
  assert(IS_INTEGER(a));
  assert(IS_INTEGER(b));
  if (((PICC_IntValue *)a)->data > ((PICC_IntValue *)b)->data)
    PICC_boolvalue_init(res, 1);
  else
    PICC_boolvalue_init(res, 0);
}

void core_arith_greater_or_eq_than(PICC_Value *res, PICC_Value *a, PICC_Value *b)
{
  assert(IS_INTEGER(a));
  assert(IS_INTEGER(b));
  if (((PICC_IntValue *)a)->data >= ((PICC_IntValue *)b)->data)
    PICC_boolvalue_init(res, 1);
  else
    PICC_boolvalue_init(res, 0);
}

void core_bits_shift_left(PICC_Value *res, PICC_Value *a, PICC_Value *b)
{
  assert(IS_INTEGER(a));
  assert(IS_INTEGER(b));
  PICC_intvalue_init(res,
      ((PICC_IntValue *)a)->data << ((PICC_IntValue *)b)->data);
}

void core_bits_shift_right(PICC_Value *res, PICC_Value *a, PICC_Value *b)
{
  assert(IS_INTEGER(a));
  assert(IS_INTEGER(b));
  PICC_intvalue_init(res,
      ((PICC_IntValue *)a)->data >> ((PICC_IntValue *)b)->data);
}

void core_bits_and(PICC_Value *res, PICC_Value *a, PICC_Value *b)
{
  assert(IS_INTEGER(a));
  assert(IS_INTEGER(b));
  PICC_intvalue_init(res,
      ((PICC_IntValue *)a)->data & ((PICC_IntValue *)b)->data);
}

void core_bits_xor(PICC_Value *res, PICC_Value *a, PICC_Value *b)
{
  assert(IS_INTEGER(a));
  assert(IS_INTEGER(b));
  PICC_intvalue_init(res,
      ((PICC_IntValue *)a)->data ^ ((PICC_IntValue *)b)->data);
}

void core_bits_or(PICC_Value *res, PICC_Value *a, PICC_Value *b)
{
  assert(IS_INTEGER(a));
  assert(IS_INTEGER(b));
  PICC_intvalue_init(res,
      ((PICC_IntValue *)a)->data | ((PICC_IntValue *)b)->data);
}

void core_io_print_str(PICC_Value *res, PICC_Value *s)
{
  assert(IS_STRING(s));
  PICC_StringHandle *str = ((PICC_StringValue *)s)->data;
  printf("%s", str->data);
  PICC_novalue_init(res);
}

void core_io_print_int(PICC_Value *res, PICC_Value *i)
{
  assert(IS_INTEGER(i));
  printf("%ld", ((PICC_IntValue *)i)->data);
  PICC_novalue_init(res);
}
