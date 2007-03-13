z390 v1.3.02b PTF 03/13/07
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

5.  RPI 558 - initial support for VSE including macros, demos, and regresion tests in new vse     directory.  See z390 VSE User Guide on www.z390.org for more details.

6.  RPI 570 - publish updated z390 Macros and svc services guide contributed by Melvyn
Maltz.

7.  RPI 574 - correct az390 to list all BAL lines with errors regardless of the current
    PRINT settings to improve ease of use in finding and fixing macro errors.

8.  RPI 575 - correct GETMAIN macro to honor LOC=RES like LOC=BELOW per documentation.
    This is also required to support the GETVIS default LOC=RES.

To install this cumulative PTF for z390 v1.3.02 follow these steps.

1.  Extract the PTF zip file into the z390 install directory replacing existing copy of z390.jar executable java file. 

2.  Copy updated macro source files into z390\mac directory:
 
GETMAIN.MAC - update documentation comments for output R1=ADDR, R0=LEN and support LOC=RES

3.  If you need to change Java source for this PTF version, all
    the Java source is in src directory and can be copied to 
    z390\src directory.

Visit www.z390.org for more information on latest updates.
