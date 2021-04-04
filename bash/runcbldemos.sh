#!/bin/bash
# runcbldemos: run z390 zcobol demos hello world, powers, copyfile, compute
#
# Note: run from z390/bash directory

# extract longest substring that ends with "/"
dir=${0%/*}

# get the z390 directory
zdir=$(dirname $0)
zdir=$(cd $zdir && pwd)
zdir=$(dirname $zdir)

cd ..

${dir}/bash/cblclg.sh zcobol/demo/HELLO $1 $2 $3 $4 $5 $6 $7 $8 $9
echo "Verify Hello World"

${dir}/bash/cblclg.sh zcobol/demo/DATETIME $1 $2 $3 $4 $5 $6 $7 $8 $9
echo "Verify date and time"

${dir}/bash/cblclg.sh zcobol/demo/POWERS $1 $2 $3 $4 $5 $6 $7 $8 $9
echo "Verify powers"

${dir}/bash/cblclg.sh zcobol/demo/COPYFILE $1 $2 $3 $4 $5 $6 $7 $8 $9
echo "Verify zcobol/demo/COPYFILE.IN was copied to zcobol/demo/COPYFILE.OUT"

${dir}/bash/cblc.sh zcobol/demo/COMPSUM $1 $2 $3 $4 $5 $6 $7 $8 $9
echo "Verify successful compile of COMPSUM"

sys390='sys390('$zdir'/zcobol/lib)'

${dir}/bash/asmlg.sh zcobol/demo/CALLCOMP $sys390 $1 $2 $3 $4 $5 $6 $7 $8 $9
echo "Verify CALLCOMP"
echo "Verify ZCOBOL demos"
