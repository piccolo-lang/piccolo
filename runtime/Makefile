OBJS      = atomic.o channel.o commit.o errors.o gc2.o debug.o pithread.o \
            primitives.o queue.o refcount.o runtime.o scheduler.o         \
            tryaction.o value.o
OBJSDBG   = $(patsubst %.o, %_dbg.o, $(OBJS))
HDRS      = channel.h commit.h debug.h errors.h pithread.h primitives.h   \
            queue.h runtime.h scheduler.h tryaction.h value.h
CC        = gcc
CFLAGS    = -fPIC -std=c11 -O2 -Wall -Werror
CFLAGSDBG = $(CFLAGS) -DPICC_DEBUG_MODE

LIBTARGET = libpiccolort.a libpiccolortdbg.a

build: $(LIBTARGET)

%_dbg.o: %.c
	$(CC) -c $(CFLAGSDBG) $^ -o $@

%.o: %.c %.h %_impl.h
	$(CC) -c $(CFLAGS) $< -o $@

libpiccolort.a: $(OBJS)
	ar r $@ $(OBJS)
	ranlib $@

libpiccolortdbg.a: $(OBJSDBG)
	ar r $@ $(OBJSDBG)
	ranlib $@

install:
	mkdir -p $(TARGET)
	install $(LIBTARGET) $(HDRS) $(TARGET)

clean:
	rm -f $(OBJS) $(OBJSDBG) $(LIBTARGET)

doc: piccolort.doxygen doc
	doxygen piccolort.doxygen

linecount:
	wc -l *.c *.h

.PHONY: build install clean doc linecount
