
INC=../../libpirt/include
LIB=../../libpirt/src

PCC=../src/pcc #-debug
CFLAGS= -g -Wall -std=c99

LIBFILES=$(wildcard $(LIB)/*.c)
OBJS=$(subst .c,.o, $(LIBFILES))

PTHFILES=$(wildcard *.pth)
TARGETS=$(subst .pth,.run, $(PTHFILES))

all: $(TARGETS)

.PHONY: clean

%.c:%.pth
	$(PCC) $< -o $@
%.o:%.c
	gcc $(CFLAGS) -I $(INC) -c $< -o $@

%.run:%.o $(OBJS)
	gcc $(CFLAGS) -I $(INC) $^ -o $@ -lpthread

clean:
	rm -f *.c *~ *.o *.run
