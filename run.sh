#!/bin/sh 

if test $# -eq 1
then
    cd src
    make
    ./pcc -o ../tests/$1.c -v ../tests/$1.pth
else
    echo "Usage:\n ./run.sh mytest\n\t with mystest.pth present in tests/"
fi