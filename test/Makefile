.PHONY: $(TESTS)

TESTS = $(sort $(patsubst %/,%.test,$(wildcard */)))

all: $(TESTS)

ifdef WITH_VALGRIND
  VALG=--with-valgrind
endif

%.test:
	@/usr/bin/env python3 ./runtest.py $(patsubst %.test,%,$@) $(VALG)

