#!/bin/bash

# cblclg: translate CBL to MLC; assemble, link, and exec using z390 

# debug flag; 0=no debug, 1=debug
debug=0

ECHO=0
if [ "$1" = "tron" ] || [ "$1" = "TRON" ]; then
#    echo "setting echo to 1"
	ECHO=1
	shift
fi

if [ -f "$1.CBL" ]; then

    if [ -f "$1.MLC" ]; then rm $1.MLC; fi
    if [ -f "$1.BAL" ]; then rm $1.BAL; fi
    if [ -f "$1.ERR" ]; then rm $1.ERR; fi
    if [ -f "$1.LST" ]; then rm $1.LST; fi
    if [ -f "$1.PRN" ]; then rm $1.PRN; fi
    if [ -f "$1.OBJ" ]; then rm $1.OBJ; fi
    if [ -f "$1.STA" ]; then rm $1.STA; fi
    if [ -f "$1.390" ]; then rm $1.390; fi
    if [ -f "$1.cpp" ]; then rm $1.cpp; fi
    if [ -f "$1.java" ]; then rm $1.java; fi
    if [ -f "$1.class" ]; then rm $1.class; fi

    # extract longest substring that ends with "/"
    dir=${0%/*}

    # COBOL options directory is full path name of z390/bash directory; may change
    optdir=$(cd $dir && pwd)

    # get the z390 directory
    zdir=$(dirname $0)
    zdir=$(cd $zdir && pwd)
    zdir=$(dirname $zdir)

    ${dir}/zc390.sh $1 $2 $3 $4 $5 $6 $7 $8 $9
    rc=$?
    if [ $debug -eq 1 ]; then echo "cblclg: zc390 rc=$rc"; fi
    if [ "$rc" -eq "0" ]; then
        cblopt='@'$optdir'/CBLOPT'
        sysmac='sysmac('$zdir'/zcobol/mac+'$zdir'/mac)'
        syscpy='syscpy(+'$zdir'/zcobol/cpy)'
        ${dir}/mz390.sh $1 $cblopt $sysmac $syscpy $2 $3 $4 $5 $6 $7 $8 $9
        rc=$?
        if [ $debug -eq 1 ]; then echo "cblclg: mz390 rc=$rc"; fi
        if [ "$rc" -eq "0" ]; then
            ${dir}/lz390.sh $1 $2 $3 $4 $5 $6 $7 $8 $9
            rc=$?
            if [ $debug -eq 1 ]; then echo "cblclg: lz390 rc=$rc"; fi
            if [ "$rc" -eq "0" ]; then
                sys390='sys390(+'$zdir'/zcobol/lib)'
                ${dir}/ez390.sh $1 $sys390 $2 $3 $4 $5 $6 $7 $8 $9
                rc=$?
                if [ $debug -eq 1 ]; then echo "cblclg: ez390 rc=$rc"; fi
                if [ "$rc" -ne "0" ]; then
                    echo "cblclg: ez390 error; ez390 rc=$rc; see errors in ez390 generated $1.LOG file and on console"
                fi
            else
                echo "cblclg: lz390 error; lz390 rc=$rc; see errors in lz390 generated $1.LST file and on console"
                if [ -f "$1.390" ]; then rm $1.390; fi
            fi
        else
            echo "cblclg: mz390 error; mz390 rc=$rc; see errors on mz390 generated $1.BAL file and console"
        fi
    else
        echo "cblclg: zc390 error; zc390 rc=$rc;see console"
    fi
else
    echo "cblclg: error; no zcobol program found: $1.CBL"
fi
