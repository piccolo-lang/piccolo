.PHONY: build install

include config.mk

install:
	$(CABAL) install

build: dist/setup-config
	$(CABAL) build

