OBJS      = atomic.o channel.o commit.o errors.o gc2.o pithread.o         \
            primitives.o queue.o refcount.o runtime.o scheduler.o         \
            tryaction.o value.o
HDRS      = channel.h commit.h errors.h pithread.h primitives.h           \
            queue.h runtime.h scheduler.h tryaction.h value.h
CC        = gcc
CFLAGS    = -fPIC -std=c11 -O2 -Wall -Werror
CFLAGSDBG = $(CFLAGS) -DPICC_DEBUG_MODE

LIBTARGET = libpiccolort.a

build: $(LIBTARGET)

%.o: %.c %.h %_impl.h
	$(CC) -c $(CFLAGS) $< -o $@

libpiccolort.a: $(OBJS)
	ar r $@ $(OBJS)
	ranlib $@

install:
	mkdir -p $(TARGET)
	install $(LIBTARGET) $(HDRS) $(TARGET)

clean:
	rm -f $(OBJS) $(LIBTARGET)

doc: piccolort.doxygen
	doxygen piccolort.doxygen

linecount:
	wc -l *.c *.h

.PHONY: build install clean doc linecount
