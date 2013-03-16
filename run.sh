#!/bin/sh 

PIRT_PATH=../../libpirt/
CFLAGS="-g -Wall -std=c99 -L$PIRT_PATH/lib -I$PIRT_PATH/include"
LFLAGS="-lpirt -lpthread"

if test $# -eq 1
then
    cd src
    make
    ./pcc -o ../tests/$1.c -v ../tests/$1.pth
    gcc $CFLAGS ../tests/$1.c -o $1 $LFLAGS
else
    echo "Usage:\n ./run.sh mytest\n\t with mystest.pth present in tests/"
fi