#!/bin/bash

# runsort: 
#
# Note: run from z390/bash directory

# extract longest substring that ends with "/"
dir=${0%/*}

# get the z390 directory
zdir=$(dirname $0)
zdir=$(cd $zdir && pwd)
zdir=$(dirname $zdir)

cd ..

# rebuild sort/SORT
${dir}/bash/asml.sh sort/SORT $1 $2 $3 $4 $5

# TESTSORT = sort ASCII 21 record simple sort
export SORTIN=sort/TESTSORT.IN[RECFM=FT,LRECL=80]
export SORTOUT=sort/TESTSORT.OUT[RECFM=FT]
export SYSIN=sort/TESTSORT.INI
if [ -f sort/TESTSORT.OUT ]; then rm sort/TESTSORT.OUT; fi
${dir}/bash/exec.sh sort/SORT STATS $1 $2 $3 $4 $5
rc=$?
if [ "$rc" -ne "0" ]; then
    echo "Error in SORT1 for TESTSORT.OUT"
    read -s -p "Press any key to continue ..."
    echo ""
fi

cp sort/SORT.STA sort/SORT1.STA

# TESTSRT1/2  Test all key types with 21 20 byte records
export SYSUT2=sort/TESTSRT1.IN
${dir}/bash/asmlg.sh sort/TESTSRT1 $1 $2 $3 $4 $5
rc=$?
if [ "$rc" -ne "0" ]; then
    echo "Error in TESTSRT1 21 record gen"
    read -s -p "Press any key to continue ..."
    echo ""
fi

export SORTIN=sort/TESTSRT1.IN[RECFM=F,LRECL=20]
export SORTOUT=sort/TESTSRT1.OUT
export SYSIN=sort/TESTSRT1.INI
if [ -f sort/TESTSRT1.OUT ]; then rm sort/TESTSRT1.OUT; fi
${dir}/bash/exec.sh sort/SORT STATS $1 $2 $3 $4 $5
rc=$?
if [ "$rc" -ne "0" ]; then
    echo "Error in SORT2 for TESTSRT1.OUT"
    read -s -p "Press any key to continue ..."
    echo ""
fi

cp sort/SORT.STA sort/SORT2.STA

export SYSUT1=sort/TESTSRT1.OUT
${dir}/bash/asmlg.sh sort/TESTSRT2 $1 $2 $3 $4 $5
rc=$?
if [ "$rc" -ne "0" ]; then
    echo "Error in TESTSRT2; verify TESTSRT1.OUT"
    read -s -p "Press any key to continue ..."
    echo ""
fi


export SYSUT2=sort/TESTSRT1.IN
${dir}/bash/asmlg.sh sort/TESTSRT1 $1 $2 $3 $4 $5
rc=$?
if [ "$rc" -ne "0" ]; then
    echo "Error in TESTSRT1 32 record gen"
    read -s -p "Press any key to continue ..."
    echo ""
fi

# TESTSRT3/4  Test sorting 100000 using 1000 rec table
export SYSUT2=sort/TESTSRT3.IN
parm="PARM(100000)"
${dir}/bash/asmlg.sh sort/TESTSRT3 $parm $1 $2 $3 $4 $5
rc=$?
if [ "$rc" -ne "0" ]; then
    echo "Error in TESTSRT3 100000 record gen"
    read -s -p "Press any key to continue ..."
    echo ""
fi

export SORTIN=sort/TESTSRT3.IN[RECFM=F,LRECL=4]
export SORTOUT=sort/TESTSRT3.OUT
export SYSIN=sort/TESTSRT3.INI
if [ -f sort/TESTSRT3.OUT ]; then rm sort/TESTSRT3.OUT; fi
sortparm="PARM(1000)"
${dir}/bash/exec.sh sort/SORT STATS $sortparm $1 $2 $3 $4 $5
rc=$?
if [ "$rc" -ne "0" ]; then
    echo "Error in SORT for TESTSRT3.OUT"
    read -s -p "Press any key to continue ..."
    echo ""
fi

cp sort/SORT.STA sort/SORT3.STA

export SYSUT1=sort/TESTSRT3.OUT
${dir}/bash/asmlg.sh sort/TESTSRT4 $parm $1 $2 $3 $4 $5
rc=$?
if [ "$rc" -ne "0" ]; then
    echo "Error in TESTSRT4; verify TESTSRT3.OUT"
    read -s -p "Press any key to continue ..."
    echo ""
fi

# done with tests
echo "Verify all sort tests ok"
