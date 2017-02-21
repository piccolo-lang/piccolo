#ifndef __PRIMITIVES_H
#define __PRIMITIVES_H

#include "value.h"

void core_arith_add(PICC_Value *res, PICC_Value *a, PICC_Value *b);
void core_arith_sub(PICC_Value *res, PICC_Value *a, PICC_Value *b);
void core_arith_mul(PICC_Value *res, PICC_Value *a, PICC_Value *b);
void core_arith_div(PICC_Value *res, PICC_Value *a, PICC_Value *b);
void core_arith_mod(PICC_Value *res, PICC_Value *a, PICC_Value *b);
void core_arith_equals(PICC_Value *res, PICC_Value *a, PICC_Value *b);
void core_arith_less_than(PICC_Value *res, PICC_Value *a, PICC_Value *b);

void core_io_print_str(PICC_Value *res, PICC_Value *s);
void core_io_print_int(PICC_Value *res, PICC_Value *i);

#endif /* !__PRIMITIVES_H */
