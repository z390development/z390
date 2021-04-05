#!/bin/bash

# zopcheck: regression test z390 instructions
#
# Note: run from z390/bash directory

# extract longest substring that ends with "/"
dir=${0%/*}

# get the z390 directory
zdir=$(dirname $0)
zdir=$(cd $zdir && pwd)
zdir=$(dirname $zdir)

cd ..

export SNAPOUT=zopcheck/SNAPOUT.TXT

sysmac='sysmac('$zdir'/mac)'
syscpy='syscpy('$zdir'/zopcheck+'$zdir'/mac)'
optable='optable(z390)'

${dir}/bash/asmlg.sh zopcheck/ZOPCHECK trace $sysmac $syscpy $optable

# done with tests
echo "Verify zopcheck ran without errors"
