# zVSAM v1 zRepro User Guide

## Introduction

ZREPRO is used to LOAD, build alternate index (BUILDAIX) UNLOAD or
VERIFY a zVSAM dataset. The function desired is specified in the PARM
parameter.

INVSAM and OUTVSAM specify a qualifier catalog name.
This catalog must have a SET variable identifying its location.

For INQSAM and OUTQSAM, the default RECFM is F or V depending on the
format of the zVSAM dataset. This may be overridden by adding
[RECFM=FT] or [RECFM=VT] to the end of the path name. The override
must be compatible with the format of the zVSAM dataset.

A log is produced showing the statistics from the run.

Sample .BAT files are provided for you to modify.
ZREPRO.BAT assembles ZREPRO.MLC which invokes ZREPROGO.BAT which has
a sample of all types of zREPRO function.

An 'internal use only' program ZREPCOMP is provided to prove that a
loaded zVSAM file can be unloaded identically. It's not meant for
general use.

## LOAD

- SET INQSAM=drive:\path\file.txt[RECFM=FT]
- SET OUTVSAM=drive:\path\catalog.vsamfile
- SET catalog=drive:\path

The RECFM override is optional, if missing will default to F or
V depending on the catalog entry. Overrides can be FT or VT.

file.txt must be sorted into base key order if loading a KSDS or
RRN order if loading an RRDS.

The vsamfile must be defined in the named catalog.

The resulting zVSAM data, index and log datasets are derived from vsamfile \
eg. MYFILE02 will generate MYFILE02.DTA, MYFILE02.IDX and MYFILE02.LOG

## BUILDAIX (Build Alternate Index)

- SET INVSAM=drive:\path\catalog.vsamfile
- SET OUTVSAM=drive:\path\catalog.vsamfile
- SET catalog=drive:\path

The input vsamfile must be defined in the named catalog as a KSDS or ESDS.
The output vsamfile must be defined in the named catalog as an AIX.
Both datasets must be defined in the same catalog.

The resulting zVSAM data, index and log datasets are derived from vsamfile \
eg. MYAIX02 will generate MYAIX02.DTA, MYAIX02.IDX and MYAIX02.LOG

This function uses an internal sort, if storage problems occur
then increase the MEM(nn) value. Also datasets SORTWKnn will be
generated.

## UNLOAD

- SET INVSAM=drive:\path\catalog.vsamfile
- SET OUTQSAM=drive:\path\file.txt[RECFM=FT]
- SET catalog=drive:\path

The RECFM override is optional, if missing will default to F or
V depending on the catalog entry. Overrides can be FT or VT.

The vsamfile must be defined in the named catalog.

Care must be taken with file.txt to prevent the unloaded file
destroying the original file.

At present an AIX cannot be unloaded.
An AIX can be unloaded by defining it as a fixed or variable-spanned KSDS.

UNLOAD is useful when VSAM datasets need to be converted eg. ESDS to KSDS.

## VERIFY

- SET INVSAM=drive:\path\catalog.vsamfile
- SET OUTQSAM=drive:\path\file.txt[RECFM=FT]
- SET catalog=drive:\path

The vsamfile must be defined in the named catalog.

Both the Data and Index components are verified.
Once 50 errors have been detected, the process is terminated.

A log is produced with error and warning messages.
If there are any of these then RC=15 is set.

For the prefix block (Data and Index) a list of errors is logged
with offset, expected value and value found eg.

```
zREPRO VERIFY DATA PREFIX BLOCK
    00000000  FFFFFF  000000
```

For file structure errors a message is issued with additional data eg.
`BHDRSELF ERROR XLRA=0000000000000A00 BLOCK=0000000000000B00`

## LOG

This is a sample log showing the load, unload and comparison of a fixed ESDS.
Message lines will vary with the type of operation being performed.

```
EZ390I program = E:\Z390\ZVSAM\CODE\ZREPRO.390
EZ390I options = PARM(LOAD) NOTIME DUMP
zREPRO UTILITY V1
zREPRO QSAM TO VSAM (LOAD)
zREPRO INPUT FILE OPENED :E:\Z390\ZVSAM\DATA\MYFILE2F.TXT
zREPRO OUTPUT DTA FILE OPENED:E:\Z390\ZVSAM\DATA\MYFILE02.DTA
zREPRO QSAM (RECFM=FT LRECL= 50) TO VSAM (FIXED ESDS NONSPANNED)
zREPRO DATA BLKSIZE 4096
zREPRO MAXIMUM DATA RECORDS/BLOCK 74
zREPRO DATA FREESPACE % 0%
zREPRO DATA BLOCKS 2
zREPRO DATA SPACEMAP BLOCKS 1
zREPRO DATA FILE SIZE 16384
zREPRO TOTAL RECORDS 96
zREPRO OUTPUT IDX FILE OPENED:E:\Z390\ZVSAM\DATA\MYFILE02.IDX
zREPRO INDEX BLKSIZE 1024
zREPRO MAX INDEX RECORDS/BLOCK 48
zREPRO INDEX FREESPACE % 0%
zREPRO ADJUSTED INDEX BLKSIZE 1008
zREPRO LOADED INDEX RECORDS/BLOCK 48
zREPRO INDEX BLOCKS (L 0) 2
zREPRO INDEX BLOCKS (L 1) 1
zREPRO INDEX SPACEMAP BLOCKS 1
zREPRO INDEX FILE SIZE 8128
zREPRO TOTAL TIME = 0:00.24
zREPRO ENDED OK

EZ390I program = E:\Z390\ZVSAM\CODE\ZREPRO.390
EZ390I options = PARM(UNLOAD) NOTIME DUMP
zREPRO UTILITY V1
zREPRO VSAM TO QSAM (UNLOAD)
zREPRO INPUT DTA FILE OPENED :E:\Z390\ZVSAM\DATA\Z390CAT2.MYFILE02
zREPRO OUTPUT FILE OPENED :E:\Z390\ZVSAM\DATA\MYFILE2F.TXO
zREPRO VSAM (FIXED ESDS NONSPANNED) TO QSAM (RECFM=FT LRECL= 50)
zREPRO BASE CLUSTER RECORDS 96
zREPRO TOTAL TIME = 0:00.06
zREPRO ENDED OK

EZ390I program = E:\Z390\ZVSAM\CODE\ZREPCOMP.390
EZ390I options = NOTIME DUMP
zREPCOMP INPUT FILE OPENED :E:\Z390\ZVSAM\DATA\MYFILE2F.TXT
zREPCOMP OUTPUT FILE OPENED :E:\Z390\ZVSAM\DATA\MYFILE2F.TXO
zREPCOMP LOAD FILE LENGTH 1597
zREPCOMP UNLOAD FILE LENGTH 1597
zREPCOMP COMPARE SUCCESSFUL

EZ390I program = E:\Z390\ZVSAM\CODE\ZREPRO.390
EZ390I options = PARM(VERIFY) NOTIME DUMP
zREPRO UTILITY V1
zREPRO VSAM VERIFY
zREPRO INPUT DTA FILE OPENED :E:\Z390\ZVSAM\DATA\Z390CAT2.MYFILE02
zREPRO VSAM (FIXED ESDS NONSPANNED)
zREPRO INPUT IDX FILE OPENED :E:\Z390\ZVSAM\DATA\Z390CAT2.MYFILE02
zREPRO VSAM (FIXED ESDS NONSPANNED)
zREPRO TOTAL TIME = 0:00.06
zREPRO ENDED OK
```

## Error Messages

zREPRO PARM NOT LOAD, UNLOAD, BUILDAIX OR VERIFY
- Correct the PARM parameter.

zREPRO VARIABLE RECORD TOO SHORT FOR BASE/AIX KEY AT RECORD NO. nnnnnnnnnnn
- A variable record must be long enough to contain the base key (KSDS) or the AIX key.

zREPRO SEQUENCE ERROR \
---key 1--- \
---key 2---
- The input file has not been sorted into base key order for loading a KSDS.

zREPRO CLUSTER NOT FOUND IN CATALOG
- The named cluster not found in the specified catalog.

zREPRO AIX BUILD, INVSAM AND AIX RELATE MISMATCH
- The RELATE= parameter in the AIX definition does not match the cluster name specified in SET INVSAM=

zREPRO INPUT AND OUTPUT HAVE DIFFERENT RECFM
- The RECFM either default or by override conflicts with the RECFM of the cluster.

zREPRO NO INPUT FILE
zREPRO NO OUTPUT FILE
- SET variables not correct for function.

zREPRO INVALID QSAM OVERRIDE
zREPRO INVALID FIXED INPUT QSAM RECFM
zREPRO INVALID VARIABLE QSAM RECFM
- Only [RECFM=FT] or [RECFM=VT] may be specified.

zREPRO QSAM INPUT FILE OPEN FAILED
zREPRO OUTPUT DATA FILE OPEN FAILED
zREPRO INPUT DATA FILE OPEN FAILED
zREPRO INDEX FILE OPEN FAILED
- See the log for the reason.

zREPRO LRECL EXCEEDS FREESPACE
zREPRO INDEX RECORD EXCEEDS FREESPACE
- For non-spanned datasets the remaining space in an empty block is
  not large enough to fit the current record. Reduce the freespace%
  or increase the blocksize.

zREPRO RRN EXCEEDS 4GB
- The RRN at the beginning of the input files exceeds 4GB.

zREPRO RRN NOT IN ASCENDING SEQUENCE
- The input file must be in RRN order.

zREPRO INVALID VSAM DSN FORMAT CODE=x
- Check the format of the VSAM SET= variables
    - CODE=1 CANNOT FIND DOT SEPARATOR
    - CODE=2 CATALOG NAME LENGTH ERROR
    - CODE=3 INPUT CATALOG NAME NOT EQUAL TO OUTPUT
    - CODE=4 CATALOG PATH MISSING
    - CODE=5 CATALOG LOAD FAILED
    - CODE=6 CATALOG VERSION ERROR
    - CODE=7 CLUSTER EXTRACTION FAILED
    - CODE=8 CLUSTER NAME LENGTH ERROR
    - CODE=9 CLUSTER.SUFFIX LENGTH ERROR
    - CODE=A ERROR IN DPAT/XPAT EXTRACTION

zREPRO UNLOAD NOT VALID FOR AIX
- The UNLOAD function is not available for an AIX.
- An AIX can be unloaded by defining it as a fixed or variable-spanned KSDS.

zREPRO VERIFY WARNING:SPANNED AND NO SEGMENTS CREATED
- It's unusual for a spanned dataset not to have any records split into segments.

zREPRO VERIFY ERROR LIMIT OF 50 REACHED
- The program is terminated after VERIFY detects 50 errors.
- See above in the VERIFY section for a sample of these errors.
