#!/bin/bash

# runasmdemos: run HELLO, DEMOM8Q1, TESTDCB1
#
# Note: run from z390/bash directory

# extract longest substring that ends with "/"
dir=${0%/*}

# get the z390 directory
zdir=$(dirname $0)
zdir=$(cd $zdir && pwd)
zdir=$(dirname $zdir)

cd ..

${dir}/bash/asmlg.sh demo/HELLO $1 $2 $3 $4 $5 $6 $7 $8 $9
echo "Verify Hello World WTO"

${dir}/bash/asm.sh demo/DEMOM8Q1 $1 $2 $3 $4 $5 $6 $7 $8 $9
echo "Verify rc=0; see PRN file for 8 queens solutions"

export SYSUT1=demo/TESTDCB1.TF1
export SYSUT2=demo/TESTDCB1.TF2
export SYSOUT=demo/TESTDCB1.TF3

${dir}/bash/asmlg.sh demo/TESTDCB1 $1 $2 $3 $4 $5 $6 $7 $8 $9
cat demo/TESTDCB1.TF2
echo "Verify demo/TESTDCB1 copied demo/TESTDCB1.TF1 to demo/TESTDCB1.TF2"
echo "Verify demo/TESTDCB1 wrote to demo/TESTDCB1.TF3"