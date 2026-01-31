# Contributing to zCICS

zCICS was written entirely in assembler code.
Transactions can be written either in assembler or in zCobol.

This document explains how to build zCICS and how to run a regression test

It consists of the following chapters:
1. Building zCICS
2. Regression testing zCICS
2.1. Create a trace of your transactions
2.2. Running the simulation
2.3. Running the comparator
2.4. Abends
3. Trademarks & Credits

## Building zCICS

The zCICS source code is pre-packaged with z390.
But zCICS is not pre-built with z390.
To use zCICS you have to build it from source.

The build procedure for zCICS consists of two steps:
1. DFHALL compiles/assembles all programs involved
2. DFHALLV creates all the zVSAM clusters to run zCICS

=== "Windows"

    `bat> cics\bat\DFHALL.BAT`
    `bat> cics\bat\DFHALLV.BAT`

=== "MacOS/Unix"
        
    `bash> ./cics/bash/dfhall`
    `bash> ./cics/bash/dfhallv`

Under Windows, the DFHALL.BAT may be invoked with argumen unref:
    `bat> cics\bat\DFHALL.BAT unref`
When the unref parameter is supplied the UNREF utility will be run
after each assembly to provide an overview of unreferenced fields.

**Note:** On Windows zCICS can also be built as part of the z390 build procedure:
    `bat> BUILD.BAT *All`

## Regression testing for zCICS

This chapter explains the use of zCICS Sequential Terminal Support.

### Create a trace of your transactions

You are well advised to keep a document relating the file number to the transaction and functions being tested.
The standard zCICS tests are documented in `cics\seq\SEQDOC.TXT`.

1. In the `Z390CICS.INI` file, set `SEQ_TERM=TRACE`.
   This will automatically adjust other INI parms:
1.1 `LOCAL_TERMINALS=1`
1.2 No `INITIAL_TRANSID=`
1.3 `TRACE_LOCALS=YES`
2. Start Z390\CICS
3. Run a planned test of your transaction
4. `CEMT P SHU` to end zCICS
5. Run `Z390SEQG`
   If you change the `INI` parm `TERMID_PREFIX` then change `Z390SEQG.BAT` as well.
   This reads the trace file ttt0.TRE and extracts the input and output streams to
   create the files SEQInnnn.TXT and SEQCnnnn.TXT.
   Enter the file number when requested as a 4-digit decimal number including
   leading zeros, e.g. 0009.
   nnnn is one more than the last one starting from 0001 or the file number if you
   are replacing a previous one.
   Note: The final CEMT P SHU command is not included in the SEQInnnn stream.
6. Repeat the above steps for each of your transactions

### Runnning the simulation

1. In the `Z390CICS.INI` file, set `SEQ_TERM=YES`.
   This will automatically adjust other INI parms:
1.1 `LOCAL_TERMINALS=0`
1.2 No `INITIAL_TRANSID=`
1.3 `TRACE_LOCALS=NO`
2. If you want the simulation to shut down after running, a sample stream is
   provided called `SEQISHUT.TXT`, copy this to a new file called SEQInnnn,
   where nnnn is the next input file sequence.
3. Start Z390\CICS
   It will run and shut down if item 2 above has been done.
   It will use the SEQInnnn files as input and creates a single `SEQO0001.TXT` file as output.
   The output streams are also sent to the Sequential terminal.

### Running the comparator

1. Run `Z390CMPG`
   This will compare all SEQCnnnn files with the new output stream `SEQO0001.TXT`.
   It produces `SEQCOMP.TXT` with those data streams that have differences.
   There is a WTO message at the end with the number of streams that have
   been written. The ideal state for a `Z390CMPG` run is zero which results in a
   null `SEQCOMP.TXT` file.
2. Exclusion file
   Many output data streams have fields which are known to vary.
   Date and time are the most frequently encountered.
   Study the existing exclusion file called `cics\seq\SEQEXCL.TXT`.
   You can create your own exclusion file, any name will do, just change the
   `SET SEQEXC=` line in `Z390CMPG` to your path/filename.
   If you change your exclusion file or create a new one, just rerun `Z390CMPG` to see the new results.

### Abends

`Z390COMP` may abend with code `U900`.

This error is caused by a structural mismatch between `SEQO0001.TXT` and the comparator files SEQCnnnn.
The output file `SEQCOMP.TXT` will contain an error message and a full dump of both files.

Before reporting these as errors, please check the following:
1. Are all the SEQI and SEQC files in sequence ?
2. Has a simulation run been done using the SEQI files (`SEQ_TERM=YES`) ?
3. Have any of the transactions changed since the last run of `Z390SEQ` for those transactions ?

## Trademarks

IBM, CICS and VSAM are registered trademarks of International Business Machines Corporation.

## Credits

Author: : Melvyn Maltz
