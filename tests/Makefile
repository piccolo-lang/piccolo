
INC     = ../../libpiccolort/include
LIB     = ../../libpiccolort/lib

PCC     = ../dist/build/piccolo/piccolo
CC      = gcc
CFLAGS  = -g -Wall -std=c99
LD      = gcc
LDFLAGS = -lpiccolort -lpthread

PIFILES = $(wildcard *.pi)
TARGETS = $(subst .pi,.run, $(PIFILES))

all: $(TARGETS)

%.c: %.pi
	$(PCC) $<
%.o: %.c
	$(CC) -c $< -o $@ -I$(INC) $(CFLAGS)

%.run: %.o
	$(LD) $^ -o $@ -L$(LIB) $(LDFLAGS)

clean:
	rm -f *.c *~ *.o *.run
