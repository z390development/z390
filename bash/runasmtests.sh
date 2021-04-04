#!/bin/bash

# runasmtests: regression test z390 instructions
#
# Note: run from z390/bash directory

# extract longest substring that ends with "/"
dir=${0%/*}

# get the z390 directory
zdir=$(dirname $0)
zdir=$(cd $zdir && pwd)
zdir=$(dirname $zdir)

cd ..

sysmac='sysmac('$zdir'/mac)'
optable='optable(z390)'

${dir}/bash/asm.sh tests/TESTINS1 trace $sysmac
${dir}/bash/asmlg.sh tests/TESTINS2 trace $sysmac
${dir}/bash/asmlg.sh tests/TESTINS3 trace $sysmac
${dir}/bash/asmlg.sh tests/TESTINS4 trace $sysmac
${dir}/bash/asmlg.sh tests/TESTINS5 trace $sysmac $optable
${dir}/bash/asmlg.sh tests/TESTDFP1 trace $sysmac $optable
${dir}/bash/asmlg.sh tests/TESTDFP2 trace $sysmac $optable
echo "Verify tests ran without errors"
