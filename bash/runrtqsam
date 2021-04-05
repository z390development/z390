#!/bin/bash

# runrtqsam: run RT QSAM for V,VB,VT,VL
# BLD? creates test file
# CHK? verifies test file
# See the trace files for snap dumps of records
#
# If one of the BLD* steps times out, run with NOTIME:
#     $ ./runrtqsam.sh notime
#
# Note: run from z390/bash directory

# extract longest substring that ends with "/"
dir=${0%/*}

# get the z390 directory
zdir=$(dirname $0)
zdir=$(cd $zdir && pwd)
zdir=$(dirname $zdir)

cd ..

#if [ -f qsam/BLD*.ERR ]; then rm qsam/BLD*.ERR; fi
shopt -s nullglob
for f in qsam/BLD*.ERR; do
#    echo "remove file: $f"
    rm $f
done
shopt -u nullglob

#if [ -f qsam/TEST*.TFV ]; then rm qsam/TEST*.TFV; fi
shopt -s nullglob
for f in qsam/TEST*.TFV; do
#    echo "remove file: $f"
    rm $f
done
shopt -u nullglob

export SNAPOUT=DUMMY

export SYSUT2=qsam/TESTV.TFV
${dir}/bash/asmlg.sh qsam/BLDV.MLC trace $1
export SYSUT1=qsam/TESTV.TFV
${dir}/bash/asmlg.sh qsam/CHKV.MLC trace

export SYSUT2=qsam/TESTVB.TFV
${dir}/bash/asmlg.sh qsam/BLDVB.MLC trace $1
export SYSUT1=qsam/TESTVB.TFV
${dir}/bash/asmlg.sh qsam/CHKVB.MLC trace

export SYSUT2=qsam/TESTVT.TFV
${dir}/bash/asmlg.sh qsam/BLDVT.MLC trace $1
export SYSUT1=qsam/TESTVT.TFV
${dir}/bash/asmlg.sh qsam/CHKVT.MLC trace

export SYSUT2=qsam/TESTVL.TFV
${dir}/bash/asmlg.sh qsam/BLDVL.MLC trace $1
export SYSUT1=qsam/TESTVL.TFV
${dir}/bash/asmlg.sh qsam/CHKVL.MLC trace

export SYSUT2=qsam/TESTVBL.TFV
${dir}/bash/asmlg.sh qsam/BLDVBL.MLC trace $1
export SYSUT1=qsam/TESTVBL.TFV
${dir}/bash/asmlg.sh qsam/CHKVBL.MLC trace

export SYSUT2=qsam/TESTVTL.TFV
${dir}/bash/asmlg.sh qsam/BLDVTL.MLC trace $1
export SYSUT1=qsam/TESTVTL.TFV
${dir}/bash/asmlg.sh qsam/CHKVTL.MLC trace

export SYSUT2=qsam/TESTF.TFV
${dir}/bash/asmlg.sh qsam/BLDF.MLC trace $1
export SYSUT1=qsam/TESTF.TFV
${dir}/bash/asmlg.sh qsam/CHKF.MLC trace $1

export SYSUT2=qsam/TESTFL.TFV
${dir}/bash/asmlg.sh qsam/BLDFL.MLC trace $1
export SYSUT1=qsam/TESTFL.TFV
${dir}/bash/asmlg.sh qsam/CHKFL.MLC trace

export SYSUT2=qsam/TESTFT.TFV
${dir}/bash/asmlg.sh qsam/BLDFT.MLC trace $1
export SYSUT1=qsam/TESTFT.TFV
${dir}/bash/asmlg.sh qsam/CHKFT.MLC trace

export SYSUT2=qsam/TESTFTL.TFV
${dir}/bash/asmlg.sh qsam/BLDFTL.MLC trace $1
export SYSUT1=qsam/TESTFTL.TFV
${dir}/bash/asmlg.sh qsam/CHKFTL.MLC trace

ls qsam/TEST*.TFV
echo "Verify runqsamrt"
