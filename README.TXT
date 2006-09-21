z390 v1.1.02a PTF 09/21/06
Copyright 2006 Automated Software Tools Corporation

The enclosed z390.jar contains following cumulative fixes for v1.1.02a:

1.  RPI 445 correct mz390 dynamic array expansion to correctly
copy existing values to new expanded array.

2.  RPI 446 correct mz390 D'symbol function to return setb vs seta
so the NOT operator will perform logical versus binary operation.

3.  RPI 447 correct mz390 N'&PARMS(&VAR) to return correct conut
when subscript has variable.

4.  RPI 448 correct az390 locking error when using EQU/DS/DC during
lookahead mode.

5.  RPI 449 correct mz390 to restrict ordinary symbol lookup to only
literal symbols with no quotes and variable setc symbols in AIF and
SET expressions.

6.  RPI 450 correct az390 to prevent truncation of symbol table 
listing when M type symbols created by mz390 found.

7.  RPI 451 correct az390 to prevent EQU symbols defined as relative 
from being updated to current location counter during label updates.

8.  RPI 452 correct TTIMER macro and svc to correctly return remaining
TU or MIC time as requested with CANCEL option.  Note v1102a requires
reassembly with updated macro as the svc interface has changed.

9.  RPI 453 speed up MVST move string instruction using byte access
versus memory buffer access.

10. RPI 454 add TRE, TROO, TROT, TRTO, and TROO translate instructions.
See RT\TEST\TESTINS1.MLC for updated RRE instruction formats and see
RT\TEST\TESTINS2.MLC for new instruction tests.

11. RPI 455 reduce console output by only writing optional statistics
to the BAL, PRN, LST, or LOG file.  Only the copyright, GNU license,
program, options, and return codes are displayed for every step.

12. RPI 456 correct mz390 to handle duplication operator inside complex
expressions.

13. RPI 457 correct az390 to allow literal cross references across 
multiple CSECT's.

14. RPI 458 correct az390 to support S type constant with explicit 
offset and base in addition to relocatable expression.

15. RPI 459 correct lz390 to prevent address exception for missing
external references or entries.  Also correct az390 to generate all
CSECT ESD entries before ENTRY, EXTRN, and WXTRN ESD entries.

To install PTF follow these steps.

1.  Copy the z390.jar file into install directory for z390
    replacing existing copy of z390.jar.

2.  Copy the updated macro directory mac to z390\mac directory.

TTIMER.MAC - update interface to correctly pass and return TU or MIC    
             remaining time for CANCEL option also.  Use of the 
             STIMER interface in v1102a requires reassembly.

3.  Copy the updated CICS directory to z390\cics directory.

none

4.  Copy the udpate LINKLIB directory to z390\linklib directory.

none

4.  If you need to change Java source for this PTF version, all
    the Java source is in src directory and can be copied to 
    z390\src directory.

5.  The following regression test sources
    are included:

TESTDC1.MLC  - test S(rel) and S(off(base)) constants
TESTINS1.MLC - test updated RRE format for TROO,TROT,TRTO, and TRTT 
TESTINS2.MLC - test TRE, TROO, TROT, TRTO, and TRTT instructions    
TESTMAC1.MLC - test N'&PARM(&VAR)
TESTSET1.MLC - test array expansion, NOT D'sym, duplication in exp.
TESTTIM2.MLC - test TTIMER CANCEL,TU

6.  Copy updated commands to the z390 root directory
    
none

RTLINK.BAT - assemble, link, and test all LINKLIB programs

Visit www.z390.org for more information on latest updates.
