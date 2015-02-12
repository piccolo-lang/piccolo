.PHONY: build install doc

include config.mk

install:
	$(CABAL) install

build: dist/setup-config
	$(CABAL) build

doc: dist/setup-config
	$(CABAL) haddock --executables --hyperlink-source --html --hoogle --haddock-options="--title Piccolo --hide Paths_piccolo"
