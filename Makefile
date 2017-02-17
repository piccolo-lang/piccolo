.PHONY: build install doc rtdoc linecount rtlinecount test

STACK = stack
MAKE  = make

all: build

install:
	$(STACK) install

reinstall: clean install

build:
	$(STACK) build

ifdef DEV_DOC
  HADDOCK_OPTS=--title Piccolo --hide Paths_piccolo --ignore-all-exports
else
  HADDOCK_OPTS=--title Piccolo --hide Paths_piccolo
endif

doc:
	cabal haddock --executables --hyperlink-source --html --hoogle \
		--haddock-options="$(HADDOCK_OPTS)"

rtdoc:
	$(MAKE) -C rts doc

linecount:
	wc -l src/*.hs src/**/*.hs

rtlinecount:
	$(MAKE) -C rts linecount

clean:
	$(STACK) clean

test:
	$(MAKE) -C test
