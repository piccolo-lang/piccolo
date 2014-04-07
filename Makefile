BIN=piccolo
SRC=$(wildcard src/*.hs src/*.x src/*.y)
all: $(BIN)

piccolo: $(SRC)
	cabal configure
	cabal build
	cp dist/build/piccolo/piccolo .

test: piccolo
	./runtests.sh

clean:
	rm -rf ./piccolo
	cabal clean

