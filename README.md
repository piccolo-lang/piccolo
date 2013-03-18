picc
the (parallel) pi-calculus compiler
====

How to use:

First, compile the sources in src/ with make. Compile also the libpirt project with make in the libpirt directory.

Then, you will find some tests to run in the tests/ directory. To run a test, for example Fibonacci.pth, type (once in the tests directory):

make Fibonacci.run -> it will produce Fibonacci.run executable, run it with ./Fibonacci.run

If you want to get the Fibonacci.c, type make Fibonacci.c instead.


The picc and libpirth directories must be located in the same directory. The arborescence must look like:

PICC_PROJECT/

 -- picc/

 ---- src/

 ---- tests/

 -- libpirth/

 ---- include/

 ---- src/

 ---- tests/   




Enjoy. 

The Pi-Thread team