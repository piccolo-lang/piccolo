OBJS       = atomic.o channel.o commit.o errors.o gc2.o pithread.o         \
             primitives.o queue.o refcount.o runtime.o scheduler.o         \
             tryaction.o value.o
OBJSPROF   = $(patsubst %.o,%_prof.o,$(OBJS))
HDRS       = channel.h commit.h errors.h pithread.h primitives.h           \
             queue.h runtime.h scheduler.h tryaction.h value.h
CC         = gcc
CFLAGS     = -fPIC -std=c11 -O2 -Wall -Werror
CFLAGSPROF = -DPICC_PROFILE_MODE

LIBTARGET = libpiccolort.a libpiccolortprof.a

build: $(LIBTARGET)

%.o: %.c %.h %_impl.h
	$(CC) -c $(CFLAGS) $< -o $@

%_prof.o: %.c
	$(CC) -c $(CFLAGS) $(CFLAGSPROF) $< -o $@

libpiccolort.a: $(OBJS)
	ar r $@ $^
	ranlib $@

libpiccolortprof.a: $(OBJSPROF)
	ar r $@ $^
	ranlib $@

install:
	mkdir -p $(TARGET)
	install $(LIBTARGET) $(HDRS) $(TARGET)

clean:
	rm -f $(OBJS) $(OBJSPROF) $(LIBTARGET)

doc: piccolort.doxygen
	doxygen piccolort.doxygen

linecount:
	wc -l *.c *.h

.PHONY: build install clean doc linecount
