
INC=../../libpirt/include
LIB=../../libpirt/src

PCC=../src/pcc #-v 3 -debug
CFLAGS= -g -Wall -std=c99

LIBFILES=$(wildcard $(LIB)/*.c)
OBJS=$(subst .c,.o, $(LIBFILES))

PTHFILES=$(wildcard *.pi)
TARGETS=$(subst .pi,.run, $(PTHFILES))

all: $(TARGETS)

.PHONY: clean

%.c:%.pi
	$(PCC) $< -o $@
%.o:%.c
	gcc $(CFLAGS) -I $(INC) -c $< -o $@

%.run:%.o $(OBJS)
	gcc $(CFLAGS) -I $(INC) $^ -o $@ -lpthread

clean:
	rm -f *.c *~ *.o *.run
