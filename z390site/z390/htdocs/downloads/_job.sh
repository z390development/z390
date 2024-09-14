#!/bin/sh
# This script is to allow job execution without the need for GUI window. 
echo COBOL Compile Assemble   Link and exec  $1 $2 
if [ -z  $1 ]              # Tested variable is not quoted.
then
 echo "Please Enter Source Library"  # Need quotes to escape #
  read Source
  echo Source Library
  export Source
else
   export Source=$1
fi 

if [ -z  $2 ]              # Tested variable is not quoted.
then
 echo "Please Enter Member(Program to compile)"  # Need quotes to escape #
  read Member
  export Member
else
   export Member=$2
fi 

echo source library $Source
echo member $Member
#java -classpath  /usr/local/lib/z390/z390.jar -Xrs -Xms150000K -Xmx150000K   zc390 $Source/$Member "SYSCPY(+zcobol+zcobol/z390)"
# Please note the DD statement for files SYSMAC and SYSCPY on the following line. 
#java -classpath /usr/local/lib/z390/z390.jar -Xrs -Xms150000K -Xmx150000K  mz390 $Source/$Member "sysmac(/usr/local/lib/z390/mac+.) syscpy(/usr/local/lib/z390/mac+.)"
#java -classpath /usr/local/lib/z390/z390.jar -Xrs -Xms150000K -Xmx150000K  lz390 $Source/$Member 
# One may need to specify any files used in the program on the following line
java -classpath /usr/local/lib/z390/z390.jar -Xrs -Xms150000K -Xmx150000K  ez390 $Source/$Member
    if [[ $? != 0 ]]; then
      echo "Error ==>   Step failed Please check errors!"
      exit 1
    fi
