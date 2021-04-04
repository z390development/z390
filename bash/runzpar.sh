#!/bin/bash

# runzpar: run ZPAR asm and cbl demos
#
# Note: run from z390/bash directory

# extract longest substring that ends with "/"
dir=${0%/*}

# get the z390 directory
zdir=$(dirname $0)
zdir=$(cd $zdir && pwd)
zdir=$(dirname $zdir)

cd ..

${dir}/bash/asmlg.sh demo/HELLO trace

sysparm="SYSPARM(demo/HELLO)"
${dir}/bash/mac.sh zpar/ZPARTRS $sysparm

${dir}/bash/cblclg.sh zcobol/demo/HELLO trace

sysparm="SYSPARM(zcobol/demo/HELLO)"
${dir}/bash/mac.sh zpar/ZPARTRS $sysparm

# done with tests
echo "See generated TRS files to verify ZPAR demos ok"
