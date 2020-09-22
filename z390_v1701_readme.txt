z390_v1701 readme.txt updated 2020-09-22 by don@higgins.net

This directory contains everything you need to install and run z390 Portable Mainframe Assembler and zCOBOL.  It has been regression tested on Windows 10.  Volunteers are needed to help test on Linux and Apple OSX platforms.

This directory can be downloaded from http://www.z390.info

The changes in this release include the following:
  1.  Add zcobol support including bat commands to compile and run demos and tests
  2.  Add Linux directory with Perl commands for use on Linux
  3.  Fix the following RPI's
      a.  RPI 2210 Fix SETA to correctly handle EQU variable name passed as parm
      b.  RPI 2212 Fix assembler to correctly handle vector registers 0-31 
      c.  RPI 2213 Add assembler X'83' DIAGNOSE/DIAG RS and X'B214' SIE S instructions

This package requires one of the following free open source Java runtimes:
  1.  Oracle SE 8 https://www.oracle.com/java/technologies/javase-jre8-downloads.html 
  2.  Open JDK 11.0.08 https://adoptopenjdk.net/

Unzip this directory and copy it to  any drive you like.

Double click on bat\bldjar80261.bat or bat\bldjar1108.bat to rebuild z390.jar from sources
using installed Oracle or Open JDK SDK. The current default z390.jar is for Oracle SE 8.0.261. 

Directories jar80261 and jar1108 contain previous successful build for each runtime version of 
z390.jar.

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
  5. TESTS\TESTINS6.MLC = verify 2218 assembler opcode and operand combinations

Double click bat\runcbltests.bat to compile, link, and execute sample zcobol regression tests.

You can also run the demos from the z390 GUI command line which you can start by
double clicking on z390.jar.  For examplen enter the command:
  bat\asmlg demo\HELLO.mlc

See more documentation on www.z390.info and history up to 2012 on www.z390.org

Join the z390 user group here: https://groups.yahoo.com/neo/groups/z390/info 

Join monthly zoom share sessions via www.ZoomToSHARE.org

Future planned releases and pending priority fixes will be posted on www.z390.info

Don Higgins
don@higgins.net