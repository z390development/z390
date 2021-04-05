#!/bin/bash

# bldjar: rebuild z390.jar from src using current installed JDK
#
# Note: must be run from z390/bash subdirectory

read -s -p "Press Enter to reuild z390.jar from src using current installed JDK";
echo "";

# Windows BLDJAR.BAT copied src *.java and Z390.MAN to jar; not done here

# cd to destination directory; empty it
cd ../jar
rm *
# compile z390 java sources to class vm object code
echo "Compiling ../src/*.java into jar"
javac -d . -g:none ../src/*.java
ec=$?
echo "The exit code of the javac command is $ec"
# pause to allow the user to terminate
read -s -p "Press Enter to continue; press Ctrl-C to exit"
echo ""
echo ""
# build z390.jar with the JDK jar utility
echo "Build z390.jar"
jar cmf ../src/Z390.MAN z390.jar *.class
ec=$?
echo "The exit code of the jar command is $ec"
# pause to allow the user to terminate
read -s -p "Press Enter to continue; press Ctrl-C to exit"
echo ""
echo ""
# list names of all the compiled class files
echo "List of compiled class files"
ls -l
# pause press ok to copy new jar back to test location
echo ""
echo "Build is complete."
echo ""
read -s -p "Press Enter to copy jar/z390.jar to z390/z390.jar; press Ctrl-C to exit"
echo ""
# switch to z390 top level directory
cd ..
# list existing z390.jar* files in z390 top level directory before copy done
ls -l z390.jar*
cp jar/z390.jar .
# list new z390.jar file
ls -l z390.jar
