#!/bin/sh

for file in `ls tests/*.pi` ; do
  #echo "[running  \"./piccolo $file\"]"
  ./piccolo $file ;
  if [ $? -eq 0 ]
  then
    echo "[OK] program $file"
  else
    echo "[FAILURE] program $file was not correctly compiled"
    exit 1
  fi
done
echo "[SUCCESS] all tests ok"

