z390_v1704_readme.txt updated 2021-01-08 by don@higgins.net

This z390_v1704.zip version has the following fixes and changes:

1.  RPI 2223: Based on IBM APAR PH30740 issued Nov. 3, 2020, HLASM supports relocatable addresses 
    in 28 RIL intructions with 4 byte immediate fields: 
    https://www.ibm.com/support/pages/apar/PH30740 
    These are identified in zopcheck by the I2 filed defined as I2RLD.
    These non relative long instructions include: 
    a.  LGFI loads 64 bit register with sign extented 4 byte value
    b.  IIHF inserts into high 32 bits of register
    c.  IILF inserts into low 32 bits of register
    The test program TESTINS5.MLC has been updated to test these instructions with immediate RLDs
    Note that IILF R1,EXTRN1 functions as a faster replacement for L R1,=A(EXTRN1) as it
    eliminates need to calculate D2(X2,B2) and eliminates second memory fetch.

2.  RPI 2220: The ACALL, AENTRY, and AEXIT support for called routines within macros has been expanded
    to support a parameter list following the ACALL name.  The number of parms passed can be
    tested via N'&name.  The individual parms can be extracted via &name(n).
    There is a new control macro APARM that is generated inline by each ACALL to reset the
    ACALL parms just before entering AENTRY code.  For example:
      ACALL SUB1(P!,P2)
      ...
      AENTRY SUB1
      &N  SETA N'&SUB1      will be set to 2
      &PN SETC '&SUB1(2)'   will be set to 'P2' 
    See new test program TESTINS1.MLC as part of bat\RUNASMTESTS.  Note ACALL is used heavily to
    structure code in zCOBOL macros and resulted in additional testing required to prevent 
    regression errors.

3.  Changes contributed by John Ganci
    a.  Five BAT command files have been updated to correct case for Linux and MacOS.
    b.  RPI 2012 Correction to bytes_to_hex SNAP support routine in sz390.java

4.  RPI 2225 The following updates have been made to zopcheck V1.2 and the PDF docuement has been updated here:
    http://www.zopcodes.info/zopcheck.pdf  
    a.  Add STCCTM STORE CPU COUNTER MULTIPLE, opcode EB-17, format RSY-b, for machine measurement
    b.  Correct BPP and BPRP to support relative offsets to target branch/exec instr.
    c.  Test and verify total of 167 relative address operands.
    d.  Correct vector instruction RXB field for following instructions:
        VCP, VLRLR, and VSTRLR.
    e.  Correct order and placement of M4 and M5 fields in following instructions:
        VFTCI, VFMA, VFMS, VFNMA, and VFNMS.

5.  RPI 2227 add 16 byte pad to memory to prevent PD fetch exceptions

6.  Add ZPARTRS utility to support debugging z390 programs
    See website doc here: http://www.z390.info/zpar/ZPARTRS_Source_Execution_Trace_Utility.htm     
    A new BAT\ZPARTRS command and BAT\RUNZPAR have been added to generate a source execuiion trace for assembler
    or zCOBOL program that has been assembled, linked, and executed with trace option.  This utility
    consists of a macro assembler program zpar\zpartrs.mlc that syncs the source MLC, the linker LST,
    and the execution TRS trace to produce report file with type TRS.  


This z390 zipped directory contains everything you need to install and run z390 Portable Mainframe
Assembler and zCOBOL.  It has been regression tested on Windows 10.  Additional volunteers are
needed to help test on Linux, Apple OSX, Windows, and IBM mainframe platforms.

This directory can be downloaded from http://www.z390.info

This package has been tested on both of the following free open source Java runtimes:
  1.  Oracle SE JDK 8 https://www.oracle.com/java/technologies/javase-jre8-downloads.html 
  2.  Open JDK 11.0.08 https://adoptopenjdk.net/

Unzip this directory and copy it to  any drive you like.

Double click on bat\bldjar.bat to build z390.jar using current installed Java SDK. 
This will not work if only a Java runtime is installed.  If the current Java runtime
is not compatible with the current Java 8+ z390.jar, install Java SDK and rebuild
z390.jar by double clicking on bat\BLDJAR.BAT.

Double click bat\zopcheck.bat to assemble, link, and execute
assembler program zopcheck.mlc which verifies all z390 assembler instruction opcodes.

Double click bat\runsort.bat to assemble, link, and execute z390 sort utility 
including demo sort merge of 100,000 records.  See doc\z390_Sort_Utility.pdf
for documention on sort and demos. 

Double click bat\runasmdemos.bat to assemble, link, and execute assembler demo programs: 
  1. DEMO\HELLO.MLC     - Display "Hello World" via WTO macro
  2. DEMO\DEMOM8Q1.MLC  - Solve 8 Queens Problem written as structured macro
  3. DEMO\TESTDCB1.MLC  - Copy sequential ASCII text file using QSAM.

Double click bat\runcbldemos.bat to compile, link,and execute 5 zcobol programs:
  1. ZCOBOL\DEMO\HELLO.CBL    - Display "Hello World"
  2. ZCOBOL\DEMO\DATETIME.CBL - Display current date, day of week, and time of day
  3. ZCOBOL\DEMO\POWERS.CBL   - Display powers of 2 up to 128
  4. ZCOBOL\DEMO\COPYFILE.CBL - Copy sequential ASCII file using QSAM
  5. ZCOBOL\DEMO\CALLCOMP.MLC - Call COMPSUM.CBL to COMPUTE sun of 15 different field types
     ZCOBOL\DEMO\COMPSUM.CBL  - Use COMPUTE to sum 15 different data type fields
     (See generated COMPSUM.BAL for 173 instructions generated for COMPUTE statement)

Double click bat\runasmtests.bat to run sample regression tests of z390 assembler and emulator:
  1. TESTSTESTINS2.MLC  - verify non supervisor instructions
  2. TESTS\TESTINS3.MLC - verify HFP, BFP, and DFP floating point instructions
  3. TESTS\TESTINS4.MLC - verify new z196 non-supervisor instructions
  4. TESTS\TESTINS5.MLC - verify new after z196 non-supervisor instructions

Double click bat\runcbltests.bat to compile, link, and execute sample zcobol regression tests.

You can also run the demos from the z390 GUI command line which you can start by
double clicking on z390.jar.  For example enter the command:
  bat\asmlg demo\HELLO.mlc

See more documentation on www.z390.info and history up to 2012 on www.z390.org
Here is link to last z390 SHARE presentation Melvyn and I did togehter:
   http://z390.org/SHARE/SHARE_8194_z390_February_2008.pdf

Join the z390 user group here: https://groups.google.com/g/z390

Join zoom share sessions via www.ZoomToSHARE.org

Future planned releases and pending priority fixes will be posted on www.z390.info

Don Higgins
don@higgins.net