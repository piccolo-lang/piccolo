.PHONY: build install doc rtdoc linecount rtlinecount test

include config.mk

install:
	$(CABAL) install

build: dist/setup-config
	$(CABAL) build

doc: dist/setup-config
	$(CABAL) haddock --executables --hyperlink-source --html --hoogle --haddock-options="--title Piccolo --hide Paths_piccolo"

rtdoc:
	$(MAKE) -C runtime doc

linecount:
	wc -l src/*.hs src/Core/*.hs src/Backend/*.hs

rtlinecount:
	$(MAKE) -C runtime linecount

clean:
	$(CABAL) clean

test:
	$(MAKE) -C test
