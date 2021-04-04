#!/bin/bash

# asml: assemble and link to generate 390 load module from mlc source 

# debug flag; 0=no debug, 1=debug
debug=0

ECHO=0
if [ "$1" = "tron" ] || [ "$1" = "TRON" ]; then
#    echo "setting echo to 1"
	ECHO=1
	shift
fi

if [ -f "$1.BAL" ]; then rm $1.BAL; fi
if [ -f "$1.PRN" ]; then rm $1.PRN; fi
if [ -f "$1.OBJ" ]; then rm $1.OBJ; fi
if [ -f "$1.LST" ]; then rm $1.LST; fi
if [ -f "$1.390" ]; then rm $1.390; fi
if [ -f "$1.ERR" ]; then rm $1.ERR; fi
if [ -f "$1.STA" ]; then rm $1.STA; fi
#if [ -f $1.TR* ]; then rm $1.TR*; fi
shopt -s nullglob
for f in $1.TR*; do
#    echo "remove file: $f"
    rm $f
done
shopt -u nullglob

# extract longest substring that ends with "/"
dir=${0%/*}

# get the z390 directory
zdir=$(dirname $0)
zdir=$(cd $zdir && pwd)
zdir=$(dirname $zdir)

sysmac='sysmac(+'$zdir'/mac)'
syscpy='syscpy(+'$zdir'/mac)'

${dir}/mz390.sh $1 $sysmac $syscpy $2 $3 $4 $5 $6 $7 $8 $9
rc=$?
if [ $debug -eq 1 ]; then echo "asml: mz390 rc=$rc"; fi
if [ "$rc" -eq "0" ]; then
    ${dir}/lz390.sh $1 $2 $3 $4 $5 $6 $7 $8 $9
    rc=$?
    if [ $debug -eq 1 ]; then echo "asml: lz390 rc=$rc"; fi
    if [ "$rc" -ne "0" ]; then
        echo "asml: lz390 error; lz390 rc=$rc; see errors on lz390 generated $1.LST file and console"
        if [ -f "$1.390" ]; then rm $1.390; fi
    fi
else
    echo "asml: mz390 error; mz390 rc=$rc; see errors on mz390 generated $1.BAL file and console"
    if [ -f "$1.OBJ" ]; then rm $1.OBJ; fi
fi
