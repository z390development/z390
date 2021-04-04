#!/bin/bash

# link: generate 390 load module from one or more obj files 

# debug flag; 0=no debug, 1=debug
debug=0

ECHO=0
if [ "$1" = "tron" ] || [ "$1" = "TRON" ]; then
#    echo "setting echo to 1"
	ECHO=1
	shift
fi

if [ -f "$1.390" ]; then rm $1.390; fi
if [ -f "$1.LST" ]; then rm $1.LST; fi
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

${dir}/lz390.sh $1 $2 $3 $4 $5 $6 $7 $8 $9
rc=$?
if [ $debug -eq 1 ]; then echo "link: lz390 rc=$rc"; fi
if [ "$rc" -ne "0" ]; then
    echo "link: lz390 error; see errors on lz390 generated $1.LST file and console"
fi

