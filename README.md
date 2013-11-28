piccolo parallel compiler
=========================

How to use
--------------

First, compile the sources in src/ with make. Compile also the libpiccort project with make in the libpiccort directory.

Then, you will find some tests to run in the tests/ directory. To run a test, for example Fibonacci.pth, type (once in the tests directory):

	make Fibonacci.run
 
It will produce Fibonacci.run executable, run it with ./Fibonacci.run

If you want to get the Fibonacci.c, type instead:

	make Fibonacci.c

The picc and libpirt directories must be located in the same parent directory. The arborescence must look like:

- parent_folder/
    - picc/
        - src/
        - tests/
    - libpirt/
        - include/
        -  src/
        - tests/   




Enjoy. 

The Piccolo team
