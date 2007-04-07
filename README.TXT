z390 v1.3.02h PTF 04/07/07
Copyright 2007 Automated Software Tools Corporation

The enclosed z390.jar contains following cumulative fixes for v1.3.02:

1.  RPI 564 - correct az390 error in RLD offsets occurring in LOCTR sections.
    See updated rt\test\TESTLCT1.MLC for updated test.

2.  RPI 565 - correct mz390 to issue error for invalid scalar reference
    to &SYSLIST other than N'&SYSLIST.  See updated rt\test\TESTERR1.MLC. 

3.  RPI 568 - correct docuementation errors in dos.pl for Linux dos command processor
    and mac\GETMAIN.MAC for documenting output R1=addr and r0=length.  Copy dos.pl to
    z390 install directory and reset rwx attributes.

4.  RPI 569 - correct ez390 option setting to always set CON if TEST.  Previously
    if TRACE was specified after TEST, CON was left off.

5.  RPI 558 - initial support for VSE including macros, demos, and regresion tests in new vse      directory.  See z390 VSE User Guide on www.z390.org for more details.

6.  RPI 570 - publish updated z390 Macros and svc services guide contributed by Melvyn
    Maltz.

7.  RPI 574 - correct az390 to list all BAL lines with errors regardless of the current
    PRINT settings to improve ease of use in finding and fixing macro errors.

8.  RPI 575 - correct GETMAIN macro to honor LOC=RES like LOC=BELOW per documentation.
    This is also required to support the GETVIS default LOC=RES.

9.  RPI 576 - include updates to ZCVTD and DCBD macros in z390 mac 
    directory which were updated as part of new VSE support

10. RPI 577 - corect az390 to support optional 3rd M field on
    TR?? extended translate instructions.  Operand was required previously.

11. RPI 578 - correct az390 to correct parsing error on multiple S type explicit
    base displacement operands.  See updated rt\test\TESTDC1.MLC.

12. RPI 579 - correct pz390 to stop SRST on address = end address
    versus >= end address.

13. RPI 580 - correct TR?? test code compares to use function bytes
    from translate table instead of data byte from R2 field.
    See updated regression tests in rt\test\TESTINS2.MLC

14. RPI 566 - add total statistics for TCPIO operations in ez390 LOG.

15. RPI 567 - add assembler support for CCW, CCW0, and CCW2.

16. RPI 572 - Correct the following TN3270 errors:
    a)  Attribute byte must be EBCDIC
    b)  Correct invalid SBA addresses
    c)  Add 4 more attribute byte equates with _MDT added
    See updated mac\TN3270.MAC macro and run interactive

17. RPI 583 - allow COND=-1 on TCPIO macro.

18. RPI 581 - correct az390 to list COPY and inline source macros on PRN, add + for
    generated source from inline macros.

19. RPI 582 - correct ez390 to set R1 to address of address of execution PARM(...)
    field with 2 byte length prefix for MVS and VSE compatibility.  This will
    require changing any z390 programs which previously assumed R1 pointed directly
    to the parm field length.  See updated rt\test\TESTPRM4.MLC for L vs LR.

20.  RPI 584 - fix ez390 trap in startup for program names less than 8 characters
     with option ASCII.  This bug was introduced in RPI v1302b.

21.  RPI 585 - add missing S2 target address field in PRN listing for BR?? and
     J?? branch type instructions.

To install this cumulative PTF for z390 v1.3.02 follow these steps.

1.  Extract the PTF zip file into the z390 install directory replacing existing copy of z390.jar executable java file. 

2.  Copy updated macro source files into z390\mac directory:

DCBD.MAC    - add global to only gen once (called by each DTF) 
GETMAIN.MAC - update documentation comments for output R1=ADDR, R0=LEN and support LOC=RES
ZCVTD.MAC   - add VSE COMRG fields and only gen once (called by COMRG), add Z390_PARMA

3.  If you need to change Java source for this PTF version, all
    the Java source is in src directory and can be copied to 
    z390\src directory.

Visit www.z390.org for more information on latest updates.
