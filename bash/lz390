#!/bin/bash
# lz390: execute lz390 runtime with java runtime options:
# -classpath  path to jar file
# -verbose:gc trace garbage collection to detect memory leaks
# -XmsnK      set initial memory allocation to nK
# -XmxnK      set max     memory allocation to nK
# -Xrs        allow control break interruption of java tasks

# debug flag; 0=no debug, 1=debug
debug=0

# get full directory path to z390.jar; assumes that this bash script is in the z390 subdirectory
jardir=$(dirname $0)
jardir=$(cd $jardir && pwd)
jardir=$(dirname $jardir)

if [ $debug -eq 1 ]; then
    echo "lz390 jardir=$jardir"
    echo "lz390 1=$1"
    echo "lz390 2=$2"
    echo "lz390 3=$3"
    echo "lz390 4=$4"
fi

java -classpath ${jardir}/z390.jar -Xrs $J2SEOPTIONS lz390 $1 $2 $3 $4 $5 $6 $7 $8 $9

rc=$?
if [ $debug -eq 1 ]; then echo "lz390 after java command; rc=$rc"; fi

exit $rc
