z390_v1705a_readme.txt updated 2021-02-09 by don@higgins.net

This zip version has the following fixes and changes:

1.  RPI 2204 Implement new problem state instructions  CDPT, CXPT, CZDT, CZXT.
    See updated regression tests in tests\TESTDFP1.MLC
2.  RPI 2226 Correct instructions STCCTM and VNOT.
    See updated ZOPCHECK regression test and additional machine measurement referrence.
3.  Fixes contributed by John Ganci
    1. RPI 2013 modifications to MP and DP emulation.
    2. RPI 2014 initialize new CDE storage to zeros before setting fields.
    3. RPI 2204 fix minor typos in source; change increase in memory to +32.

This z390 zipped directory contains everything you need to install and run z390 Portable Mainframe
Assembler and zCOBOL.  It has been regression tested on Windows 10.  Additional volunteers are
needed to help test on Linux, Apple OSX, Windows, and IBM mainframe platforms.

This directory can be downloaded from http://www.z390.info

This package has been tested on both of the following free open source Java runtimes:
  1.  Oracle SE JDK 8 https://www.oracle.com/java/technologies/javase-jre8-downloads.html 
  2.  Open JDK 11.0.08 https://adoptopenjdk.net/

Unzip this directory and copy it to  any drive you like.

Double click on bat\bldjar.bat to rebuild z390.jar using current installed Java SDK. 
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
  5. TESTS\TESTDFP1.MLC - verify new DFP instructions
  6. TESTS\TESTDFP2.MLC - verify DFP assembler data definitions

Double click bat\runcbltests.bat to compile, link, and execute sample zcobol regression tests.

Double click bat\runzpar.bat to create source execution trace TRS files showing source program
lines and execution instruction trace for both assembler and zCOBOL.

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