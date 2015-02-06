include ../config.mk

OBJS = atomic.o channel.o commit.o errors.o gc2.o logger.o pithread.o \
       primitives.o queue.o refcount.o runtime.o scheduler.o \
       tryaction.o value.o
HDRS = channel.h commit.h errors.h pithread.h primitives.h   \
       queue.h runtime.h scheduler.h tryaction.h value.h
CFLAGS := -fPIC $(CFLAGS)

LIBTARGET = libpiccolort.a

build: $(LIBTARGET)

$(LIBTARGET): $(OBJS)
	ar r $(LIBTARGET) $(OBJS)
	ranlib $(LIBTARGET)

install:
	mkdir -p $(TARGET)
	install $(LIBTARGET) $(HDRS) $(TARGET)

clean:
	rm -f $(OBJS) $(LIBTARGET)

.PHONY: build install clean
