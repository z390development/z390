#!/bin/bash
# ez390: execute ez390 runtime with java runtime options:
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
    echo "ez390 jardir=$jardir"
    echo "ez390 1=$1"
    echo "ez390 2=$2"
    echo "ez390 3=$3"
    echo "ez390 4=$4"
fi

java -classpath ${jardir}/z390.jar -Xrs $J2SEOPTIONS ez390 $1 $2 $3 $4 $5 $6 $7 $8 $9

rc=$?
if [ $debug -eq 1 ]; then echo "ez390 after java command; rc=$rc"; fi

exit $rc
