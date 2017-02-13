Piccolo
=========================

Pre-requisites
--------------

- The GHC compiler
- cabal (>= 1.18)
- The gcc compiler

How to use
--------------

### Installation:

A Makefile is present and is more or less a wrapper over cabal and runtime
makefile. Then, to install Piccolo, just type:

```
make install
```

This will install the ``piccolo`` binary into your ``cabal/bin`` dir (which is
likely to appear in your path), and install runtime (some header files and a
static library) somewhere into your ``cabal/share`` dir.

### Using the piccolo compiler:

```
piccolo file.pi
```

This will hopefully generate the executable *a.out* in the current directory.
