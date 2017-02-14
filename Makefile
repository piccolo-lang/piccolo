.PHONY: build install doc rtdoc linecount rtlinecount test

CABAL = cabal
MAKE  = make

install:
	$(CABAL) install

reinstall: clean install

dist/setup-config:
	$(CABAL) configure

build: dist/setup-config
	$(CABAL) build

ifdef DEV_DOC
  HADDOCK_OPTS=--title Piccolo --hide Paths_piccolo --ignore-all-exports
else
  HADDOCK_OPTS=--title Piccolo --hide Paths_piccolo
endif

doc: dist/setup-config

	$(CABAL) haddock --executables --hyperlink-source --html --hoogle \
		--haddock-options="$(HADDOCK_OPTS)"

rtdoc:
	$(MAKE) -C rts doc

linecount:
	wc -l src/*.hs src/**/*.hs

rtlinecount:
	$(MAKE) -C rts linecount

clean:
	$(CABAL) clean

test:
	$(MAKE) -C test
