z390_v1702_readme.txt updated 2020-11-02 by don@higgins.net

This z390 version has the following fixes and changes:

1.  TEST\TESTINS6.MLC has been renamed and moved to ZOPCHECK\ZOPCHECK.MLC
    with its own BAT\ZOPCHECK command.  The total number of opcode
    and operand tests has grown from 2216 to 2336 by adding some missing 
    mnemonics in zopcodes.cpy.  The program has now been optimized so its BAL file
    can now be run on mainframe using IBM COBOL course system.   The only missing
    opcode errors were for DIAG, SIE, and PGIN.  Most of the errors generated
    are primarily due to mask fields not having specific bit settings for specific
    instruction options.  Additional errors were for specific register requirements
    for floating point and 128 bit result instructions. 

2.  A new option zvsam(0=none,1=zvsam1, or 2=zvsam2) has been added which sets
    GBLA &SYSZVSAM which is used by new vsam macros to determine which vsam version to use.

3.  A new structured programming set of macros  developed by Dan Snyder has been
    added in directory named "structuredmacros".  It is released as open source software.
    These macros support more complex logical expressions in IF and DO macros and
    have been tested on z390 and mainframe by Dan.  A future z390 project may be to 
    integrate this support with existing z390 structured macro support.
    The structuredmacros subdirectories included are:
    1.  Documentation
    2.  z390 - maclib to concatenate with z390 mac library
    3.  zOS  - iebupdte jcl to update user library

4.  John Ganci has contributed an updated perl directory which now contains a readme.txt
    file plus a zip file with 3 directories: bat, perl, and src.  The bat directory contains
    updated bat files for use on linux. The perl directory contains updated cmd.pl and dos.pl
    perl commands to process the bat files on linus.  The src directory contains updated
    z390.java with changes including new path to perl files.  The java update will require 
    rebuild of z390.jar on linus platform with java jda 8+ installed.

5.  RPI 2211 fix for macro assembler prevents erroneous optimization of part of pseudo code
    generated for a macro assembler line.  This occurred in a vsam macro Melvyn was working
    with.  Note I used latest version of Eclipse to debug this problem, and Eclipse no
    longer supports Java JDK 8 so I had to switch to JDK 11.

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

Double click zopcheck\zopcheck.bat to assemble, link, and execute
assembler program zopcheck.mlc which verifies all z390 instruction opcodes.

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

Join monthly zoom share sessions via www.ZoomToSHARE.org

Future planned releases and pending priority fixes will be posted on www.z390.info

Don Higgins
don@higgins.net