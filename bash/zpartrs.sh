#!/bin/bash

# zpartrs: COBOL and assembler source trace for $1 dir/pgmname
# pgmname must have been compiled and executed to gen PRN, LST and TRE files

# extract longest substring that ends with "/"
dir=${0%/*}

# get the z390 directory
zdir=$(dirname $0)
zdir=$(cd $zdir && pwd)
zdir=$(dirname $zdir)

if [ -f "$1.TRS" ]; then rm $1.TRS; fi

sysparm="SYSPARM($1+$2+$3+$4+$5+$6+$7+$8+$9)"
${zdir}/bash/mac.sh zpar/ZPARTRS $sysparm @$zdir/zpar/ZPARTRS
rc=$?
echo "zpartrs: mac rc=$rc"
if [ "$rc" -ne "0" ]; then
    echo "Error running zpar/ZPARTRS"
fi

ls $1.*

# done with tests
echo "zpartrs: done"
