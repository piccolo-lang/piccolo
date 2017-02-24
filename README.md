Piccolo
=======

Pre-requisites
--------------

- The 'stack' haskell tool

How to use
----------

### Installation:

A Makefile is present and is more or less a wrapper over stack (for the
compiler) and gcc (for the runtime).
Then, to install Piccolo, simply type:

```
make install
```

This will install the ``piccolo`` binary into your ``~/.local/bin`` dir (which is
likely to appear in your path if you use stack),
and install runtime (some header files and static libraries)
somewhere into the ``.stack-work`` dir.

### Using the piccolo compiler:

```
piccolo file.pi
```

This will generate the executable *a.out* in the current directory.

For more options:

```
piccolo --help
```
