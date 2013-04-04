#! /bin/sh

echo "*** Cleaning example '$1' ***"

if [ -e "$1" ]
then
  echo "==> Cleaning directory '$1'" ;
  rm -rf "$1"
else
  echo "==> Example '$1' already clean" ;
fi
