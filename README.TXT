z390 v1.3.03a PTF 04/29/07
Copyright 2007 Automated Software Tools Corporation

The enclosed z390.jar contains following cumulative fixes for v1.3.03:

1.  RPI 598 - Correct XCTL svc to save R1 (bug in RPI 596) and correct
    memory corruption after multiple XCTL during same execution.  See
    updated TESTXCL1-4 regression test.

2.  RPI 599 - Add DCBD macro call from DCB to define fields.  DCBD will only
    generated DSECT once.

3.  RPI 600 - Correct mz390 to assume undefined variables are local unless they start with     &SYS.  Also issue error 211 for duplicate keyword macro call parameters.

4.  RPI 601 - Correct macros SAVE and RESTORE to support Rnn register references;
    change SUBENTRY to use 4096 base offsets versus 4095 using LAY instead of LA;
    and correct SUBEXIT label generation error.

5.  RPI 602 - Correct az390 to issue errors for negative DS/DC lengths.

6.  RPI 603 - Correct gz390 GUI interface to correctly support up and down
    arrow scrolling of prior commands.  (It was skipping previous command.)

7.  RPI 605 - Add additional error detection in az390 by adding location counter on the     TRACEA file; add error message for first cyclic DS/DC and EQU valaue changes; remove
    duplicate az390 error messages on the the TRACEA file; and change section error 
    messages to show hex address and lengths versus decimal.

To install this cumulative PTF for z390 v1.3.03 follow these steps.

1.  Extract the PTF zip file into the z390 install directory replacing existing copy of z390.jar executable java file. 

2.  Copy updated macro source files into z390\mac directory:

DCB.MAC - add call to DCBD to generate DSECT once
RESTORE - support Rnn register symbols
SAVE    - support Rnn register symbols
SUBENTRY - change base offsets to 4096 versus 4095
SUBEXIT  - correct label generation

3.  If you need to change Java source for this PTF version, all
    the Java source is in src directory and can be copied to 
    z390\src directory.

Visit www.z390.org for more information on latest updates.
