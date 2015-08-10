#!/bin/bash

if [ $# -eq 0 ]
then
  echo "What's the name of the test?"
  exit 1
fi

mkdir $1
echo '#!/bin/bash' >  $1/run
echo "piccolo $1.pi -o $1" >> $1/run
echo 'if [ "$#" -eq 1 ] && [ "$1" = "--with-valgrind" ]' >> $1/run
echo 'then' >> $1/run
echo "  valgrind --leak-check=full --error-exitcode=2 --suppressions=../valgrind.supp ./$1 > /dev/null 2> /dev/null" >> $1/run
echo '  if [ "$?" -ne 0 ]' >> $1/run
echo '  then' >> $1/run
echo '    exit 2' >> $1/run
echo '  fi' >> $1/run
echo 'else' >> $1/run
echo "./$1" >> $1/run
echo 'fi' >> $1/run
echo "rm $1" >> $1/run

chmod +x $1/run

echo "$1/run created"
