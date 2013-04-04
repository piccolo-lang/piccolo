#! /bin/sh

echo "*** Debugging example '$1' ***"

if [ ! -e "../src/pcc" ]
then
  echo "Error: pi compiler not available" ;
  exit 1
fi

if [ ! -e $1.pi ]
then
  echo "Error: no such file '$1.pi'" ;
  exit 1
fi

if [ -e "$1" ]
then
  echo "==> Cleaning directory '$1'" ;
  rm -rf "$1"
  mkdir "$1"
else
  echo "==> Creating directory '$1'" ;
  mkdir "$1"
fi

echo "==> Compiling '$1.pi'"
echo [1] ../src/pcc -o "$1/$1.c" "$1.pi" #-v 3 -debug
../src/pcc -o "$1/$1.c" "$1.pi" #-v 3 -debug

echo "==> Fetching runtime sources"
if [ ! -e ../../libpirt ]
then
  echo "Error: runtime not available" ;
  exit 1
fi

cp ../../libpirt/include/*.h "$1/"
cp ../../libpirt/src/*.c "$1/"

echo "==> Compiling example"
cd "$1/"
gcc -g -Wall -std=c99 -I. -lpthread *.c -o "$1"
