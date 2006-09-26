z390 v1.1.02b PTF 09/26/06
Copyright 2006 Automated Software Tools Corporation

The enclosed z390.jar contains following cumulative fixes for v1.1.02b:

1.  RPI 439 add mz390 pseudo code support to speed up macro    
execution.  The new default option is PC.  Use NOPC to turn it off.
Option MAXPC(nnn) can be used to change default pseuod code opcode
cache from default of 100000 entries.  The cache is reused on least
recently used opcode basis if overflow occurs.  Currently on AGO is
supported.  AIF, SET, and others to follows.  The speed of an AGO loop
executed 1000000 times improves from 0.15 to 2.2 MIPS with PC option by
eliminating regular expression parsing and symbol table lookups.

2.  RPI 461 update RT1.MAC regression test macro to suppport CCGE and 
CCLE for use in RT\TEST\TESTTIM2 stimer tests to prevent errors.        

3.  RPI 462 correct mz390 to always assume shorter string is low in
string compares.                

4.  RPI 463 correct mz390 and az390 to correctly handle quoted strings
split across 2 or more continuation lines.

5.  RPI 464 correct az390 to only display MNOTE's on ERR file during
last pass to avoid duplicates.                                        

6.  RPI 465 correct az390 to allow register 0 as valid base for 
a DSECT such as PSA.                                   

7.  RPI 466 correct mz390 lookahead to handle unlabeled DS/DC and
multiple DSECT's.

To install PTF follow these steps.

1.  Copy the z390.jar file into install directory for z390
    replacing existing copy of z390.jar.

2.  Copy the updated macro directory mac to z390\mac directory.

RT1.MAC    - regression test macro support CCGE and CCLE
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
TESTSYM1.MLC - test unlabled DS/DC and mulitple DSECT lookahead 
TESTTIM2.MLC - test TTIMER CANCEL,TU

6.  Copy updated commands to the z390 root directory
    
RTLINK.BAT - assemble, link, and test all LINKLIB programs

Visit www.z390.org for more information on latest updates.
