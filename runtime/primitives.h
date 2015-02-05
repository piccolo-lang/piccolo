#ifndef __PRIMITIVES_H
#define __PRIMITIVES_H

#include "value.h"

void corearith_add(PICC_Value *res, PICC_Value *a, PICC_Value *b);
void corearith_sub(PICC_Value *res, PICC_Value *a, PICC_Value *b);
void corearith_mul(PICC_Value *res, PICC_Value *a, PICC_Value *b);
void corearith_div(PICC_Value *res, PICC_Value *a, PICC_Value *b);
void corearith_mod(PICC_Value *res, PICC_Value *a, PICC_Value *b);
void corearith_equals(PICC_Value *res, PICC_Value *a, PICC_Value *b);
void corearith_less_than(PICC_Value *res, PICC_Value *a, PICC_Value *b);

void coreio_print_str(PICC_Value *res, PICC_Value *s);
void coreio_print_int(PICC_Value *res, PICC_Value *i);

#endif /* !__PRIMITIVES_H */
