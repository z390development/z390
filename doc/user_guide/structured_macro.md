# Structured Macro Extensions

z390 has extended the HLASM macro syntax to add support for structured macro 
code. This is sometimes referred to as _Structured Programming Extensions_ or 
SPE.

!!! Warning
    z390 structured macro extensions are not supported by IBM HLASM.
    You need to translate the macros from structured to standard first.


## Instructions

### Alternate selection of code blocks

* AIF  (expression) - execute the following block of code if expression is true
* AELSEIF (expression) - end prior block and execute following block if 
  expression is true
* AELSE - end prior block and execute following block if prior AIF and AELSEIF 
  false
* AEND - end last block for AIF at current level of nesting

### Repeat execution of code block

* AWHILE (expression) - repeat following code block while expression is true 
  (test at beginning)
* AEXIT AWHILE - exit to end of inner most AWHILE (for exceptions within nested AIF)
* AEND - end block of code for AWHILE at current level of nesting
* AUNTIL (expression) - repeat following code block until expression is true (test at end)
* AEXIT AUNTIL - exit to end of inner most AUNTIL (for exceptions within nested AIF)
* AEND - end block of code for AUNTIL at current level of nesting

### Perform code block

* ACALL name - call the named code block and return to next instruction
* AENTRY name - define start of performed block of code (skip over if entered 
  sequentially)
* AEXIT AENTRY - exit from AENTRY block of code (for exceptions within nested AIF)
* AEND - end the current performed code block and exit to next statement after APM

### Perform selection of code blocks based on index from 0 to 255

* ACASE (expression) - execute selected block based on value of index expression
* AWHEN values - define end of previous block and start of code block for index values
    * values can be decimal (0-255), character C'?', or hex X'??'
    * one or more values may be specified separated by commas
    * a range of values may be specified as (value1,value2)
    * for example AWHEN (C'0',C'9') defines EBCDIC digits 240-249
* AEXIT ACASE - exit to end of current ACASE (for exceptions within nested AIF)
* AELSE - define optional code block if no AWHEN block defined for current index
* AEND - end code block for ACASE

* Additional extension to indent label field by preceding with colon (:)

## Translating to standard

The following items are included for general use:

* :material-file-question: zstrmac - script to convert macro to standard HLASM format
* bldzstrmacs - script to translate all the SPM's in z390\mac 
  directory to HLASM compatible format 
* rt\test\ZSTRMAC2.ZSM - structured translator which can be translated to standard 
  HLASM compatible code using itself.
* rt\test\ZSTRMAC1.MLC - bootstrap structured translator which is HLASM compatible.  
* :material-file-question: linklib\zstrmac.txt - the generated HLASM compatible translator.

## Regression tests

You can execute the regression tests by using the script `runzstrmactests`.

The following regression test programs are included:

* rt\test\ZSTRMAC1.MLC - bootstrap version of translator written in standard HLASM.
* rt\test\ZSTRMAC2.ZSM - structured version of the translator which uses all the structures.
* rt\test\TESTSPE1.ZSM - test program for ZSTRMAC1 with all the basic structures.
* rt\test\TESTSPE2.ZSM - test program for ZSTRMAC2 with all the basic structures in lower case
* rt\test\TESTSPE3.ZSM - test of all 256 ACASE values using all forms of AWHEN operands
* rt\test\TESTSPE4.ZSM - test error messages
* rt\test\TESTSPM1.MLC - test program for structured programming macros

## Demo

The following demo programs using the macro extensions are include:

* demo\DEMOM8Q1.MLC - solve 8 queens chess problem suing recursive structured macro

## Utilities

The following z390 utility programs using the macro extensions are included:

* linklib\RTGENDIR.MLC - read file system directory and create list of file names
* linklib\RTGENCMP.MLC - read merged list of files from 2 directories and 
  generate compare commands
* linklib\RTGENDIF.MLC - read difference files and generate erase commands for 
  identical files

The following system macros use the macro extensions:

* EQUREGS.MAC - generate EQU symbols if not already generated for GPR and FPR registers
* ZCLOSE.MAC - close files (called from CLOSE and other user macros in concatenated directories)
* ZOPEN.MAC - open files (called from OPEN and other user macros in concatenated directories)
