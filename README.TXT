Github z390 Branch V1707 has been created from the z390_v1707.zip distribution
described below with the following additiona changes:
1. z90.bat has been updated to use the start z390.jar command to release command line.
2. The following additional directories have been retained from the Github prior
   repository version v1706:
   a. mfacc - sample assembler programs for student exercises
   b. mvs - MVS 3.8 public domain macros
4. Note this V1707 repository does not have the generated file z390.jar
   in the root z390 directory so the command bat\BLDJAR.BAT will need to be
   executed to create it, and it will require Java 8.0 JDK.
   A future distribution version may create it and run regression tests. 

z390_v1707_readme.txt updated 2021-05-02 by don@higgins.net

This zip version has the following fixes and changes:

1.  Issue #239 correct assembler to issue error message 
    for undefined operand for IIHF instruction.
2.  Issue #2 move zcobol opt directory and file under zcobol and
    test updated ASM and CBL bat files on Windows.
3.  Issue #234 add the following directories which were included
    in the 1.5.06 version of z390: assist, cics, linklib, guam, soa, vsam, vse, and rt. 
    1.  assist - run bat\runassist.bat to run demo and test programs using ASSIST.
    2.  cics - added 6 new subdirectories - 390, bat, cbl, cpy, mac, mlc.  
               opened issue #245 to update bat commands to use new subdirectories.
    3.  guam - added graphical user access method with demo and test subdirectories
    4.  linklib - added utility library with 21 mlc programs.  See issue #246 for updates.
    5.  soa - added service oriented architecture (SOA) with demo, maclib, test, and readme.
        Issue #247 has been opened to add bat commands to build components.
    6.  vsam1 = See issue #253 for error trying to run vsam1\demo\demos.bat with zvsam(1)
    7.  vsam2 - added 2 subdirectories bat and mlc and successfully assembled and linked
        6 executable programs using the zvsam(2) option in the mlc directory.  Also the
        mac directory was copied to macold and the new hybrid vsam1 and vsam2 mac
        library replaced the old mac library.  
    8.  vse - run bat\runvsedenis,bat to see 2 demo programs taht currently fail
        due to changes in low storage mapping for zcvt and comreg.  See issue #244.
    9.  rt - added with readme and regression test bat subdirectory with program for 
        issue #238 from John G. Note bat commands use cd.. cd.. to get to root.
        There is much more work to do with rt to add automated rt to repository.
        Note old version zc390lib.390 had to be deleted from this directory
        to prevent failure of zcobol which now uses zcobol\lib\zc390lib.390 which
        is built by bat\BLDCBLLIB.BAT whenever version changes in tz390.java.
4.  Added z390.bat to bat directory to invoke z390 GUI by executing z390.jar.  
    This is alternative to double clicking on z390.jar under Windows.

This z390 zipped directory contains everything you need to install and run z390 Portable Mainframe Assembler and zCOBOL.
It has been regression tested on Windows 10.  

This directory can be downloaded from http://www.z390.info

This package has been tested on both of the following free open source Java runtimes:
  1.  Oracle SE JDK 8 https://www.oracle.com/java/technologies/javase-jre8-downloads.html 
  2.  Open JDK 11.0.08 https://adoptopenjdk.net/

Unzip this directory and copy it to  any drive you like.

Double click on bat\bldjar.bat to rebuild z390.jar using current installed Java SDK. 
This will not work if only a Java runtime is installed.  If the current Java runtime
is not compatible with the current Java 8+ z390.jar, install Java SDK and rebuild
z390.jar by double clicking on bat\BLDJAR.BAT.

Double click rt\bat\zopcheck.bat to assemble, link, and execute
assembler program zopcheck.mlc which verifies all z390 assembler instruction opcodes.

Double click rt\bat\runsort.bat to assemble, link, and execute z390 sort utility 
including demo sort merge of 100,000 records.  See doc\z390_Sort_Utility.pdf
for documention on sort and demos. 

Double click bat\runasmdemos.bat to assemble, link, and execute assembler demo programs: 
  1. DEMO\HELLO.MLC     - Display "Hello World" via WTO macro
  2. DEMO\DEMOM8Q1.MLC  - Solve 8 Queens Problem written as structured macro
  3. DEMO\TESTDCB1.MLC  - Copy sequential ASCII text file using QSAM.

Double click bat\bldcbllib.bat to compile and link zcobol runtime zc390lib.390.

Double click bat\runcbldemos.bat to compile, link,and execute 5 zcobol programs:
  1. ZCOBOL\DEMO\HELLO.CBL    - Display "Hello World"
  2. ZCOBOL\DEMO\DATETIME.CBL - Display current date, day of week, and time of day
  3. ZCOBOL\DEMO\POWERS.CBL   - Display powers of 2 up to 128
  4. ZCOBOL\DEMO\COPYFILE.CBL - Copy sequential ASCII file using QSAM
  5. ZCOBOL\DEMO\CALLCOMP.MLC - Call COMPSUM.CBL to COMPUTE sun of 15 different field types
     ZCOBOL\DEMO\COMPSUM.CBL  - Use COMPUTE to sum 15 different data type fields
     (See generated COMPSUM.BAL for 173 instructions generated for COMPUTE statement)

Double click rt\bat\runasmtests.bat to run sample regression tests of z390 assembler and emulator:
  1. TESTSTESTINS2.MLC  - verify non supervisor instructions
  2. TESTS\TESTINS3.MLC - verify HFP, BFP, and DFP floating point instructions
  3. TESTS\TESTINS4.MLC - verify new z196 non-supervisor instructions
  4. TESTS\TESTINS5.MLC - verify new after z196 non-supervisor instructions
  5. TESTS\TESTDFP1.MLC - verify new DFP instructions
  6. TESTS\TESTDFP2.MLC - verify DFP assembler data definitions

Double click rt\bat\runcbltests.bat to compile, link, and execute sample zcobol regression tests.

Double click rt\bat\runzpar.bat to create source execution trace TRS files showing source program
lines and execution instruction trace for both assembler and zCOBOL.

You can also run the demos from the z390 GUI command line which you can start by
double clicking on z390\z390.jar or by running bat\z390.bat.  
For example start the GUI and enter the command:
  bat\asmlg demo\HELLO.mlc

See more documentation on www.z390.info and history up to 2012 on www.z390.org
Here is link to last z390 SHARE presentation Melvyn and I did togehter:
   http://z390.org/SHARE/SHARE_8194_z390_February_2008.pdf

Join the z390 user group here: https://groups.google.com/g/z390

This may be the last release published in zip format on z390.info as the z390 project
is migrating github shared development server.  Announcements coming soon.

Don Higgins
don@higgins.net