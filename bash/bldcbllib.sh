#!/bin/bash

# bldcbllib: rebuild zcobol/lib/ZC390LIB.390
#
# Note: must be run from bash subdirectory

# extract longest substring that ends with "/"
dir=${0%/*}

# get the z390 directory
zdir=$(dirname $0)
zdir=$(cd $zdir && pwd)
zdir=$(dirname $zdir)

cd ..

sysmac='sysmac('$zdir'/mac)'
syscpy='syscpy('$zdir'/mac+'$zdir'/zcobol/lib)'
sys390='sys390('$zdir'/zcobol/lib)'

${dir}/bash/asm.sh zcobol/lib/ABORT $sysmac $syscpy $1 $2 $3 $4 $5 $6 $7 $8 $9
${dir}/bash/asm.sh zcobol/lib/ACCEPT $sysmac $syscpy $1 $2 $3 $4 $5 $6 $7 $8 $9
${dir}/bash/asm.sh zcobol/lib/CVTTOHEX $sysmac $syscpy $1 $2 $3 $4 $5 $6 $7 $8 $9
${dir}/bash/asm.sh zcobol/lib/DISPLAY $sysmac $syscpy $1 $2 $3 $4 $5 $6 $7 $8 $9
${dir}/bash/asm.sh zcobol/lib/INSPECT $sysmac $syscpy $1 $2 $3 $4 $5 $6 $7 $8 $9
${dir}/bash/asm.sh zcobol/lib/ZC390NUC $sysmac $syscpy $1 $2 $3 $4 $5 $6 $7 $8 $9
${dir}/bash/asml.sh zcobol/lib/ZC390LIB $sysmac $syscpy $sys390 RMODE24 $1 $2 $3 $4 $5 $6 $7 $8 $9

echo "Verify ZC390LIB.390 build ok"