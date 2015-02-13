.PHONY: build install doc linecount

include config.mk

install:
	$(CABAL) install

build: dist/setup-config
	$(CABAL) build

doc: dist/setup-config
	$(CABAL) haddock --executables --hyperlink-source --html --hoogle --haddock-options="--title Piccolo --hide Paths_piccolo"

rtdoc:
	make -C runtime doc

linecount:
	wc -l src/*.hs src/Core/*.hs src/Backend/*.hs

rtlinecount:
	make -C runtime linecount
