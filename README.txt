z390 distribution README
========================

The z390 distribution zip contains everything you need to install 
and run z390 Portable Mainframe Assembler and zCOBOL.

This distribution can be downloaded from GitHub 
at https://github.com/z390development/z390/releases

This distribution has been regression tested on Windows Server and Ubuntu Linux 
using GitHub Actions.

Java runtime/SDK used for build: 
Eclipse Temurin 21 https://adoptium.net/temurin/releases/?variant=temurin21

This distribution includes a prebuilt z390.jar file and scripts.
All you need to install is a Java JRE (version 8 or above).

This distribution DOES NOT include the z390 Java sources.
If you wish to rebuild the jar from source, please refer to the 
GitHub page https://github.com/z390development/z390.

Installation
------------
Unzip this directory and copy it to any drive you like.
* For Windows users, you can add the bat folder to your Windows path
* For Linux/MacOS users, you can add the bash folder to your shell path

Verification
------------
Run rt\bat\RUNSORT.BAT on Windows | bash/runsort on Linux/MacOS 
   to assemble, link, and execute z390 sort utility including demo sort merge of 
   100,000 records.  See documentation on sort and demos. 

Run bat\RUNASMDEMOS.BAT on Windows | bash/runasmdemos on Linux/MacOS 
   to assemble, link, and execute assembler demo programs: 
    1. demo\HELLO.MLC     - Display "Hello World" via WTO macro
    2. demo\DEMOM8Q1.MLC  - Solve 8 Queens Problem written as structured macro
    3. demo\TESTDCB1.MLC  - Copy sequential ASCII text file using QSAM.

Run bat\RUNCBLDEMOS.BAT on Windows | bash/runcbldemos on Linux/MacOS
   to compile, link,and execute 5 zcobol programs:
    1. zcobol\demo\HELLO.CBL    - Display "Hello World"
    2. zcobol\demo\DATETIME.CBL - Display current date, day of week, and time of day
    3. zcobol\demo\POWERS.CBL   - Display powers of 2 up to 128
    4. zcobol\demo\COPYFILE.CBL - Copy sequential ASCII file using QSAM
    5. zcobol\demo\CALLCOMP.MLC - Call COMPSUM.CBL to COMPUTE sun of 15 different field types
        zcobol\demo\COMPSUM.CBL - Use COMPUTE to sum 15 different data type fields
        (See generated COMPSUM.BAL for 173 instructions generated for COMPUTE statement)

Run rt\bat\RUNASMTESTS.BAT on Windows | bash/runasmtests on Linux/MacOS
   to run sample regression tests of z390 assembler and emulator:
    1. tests\TESTINS2.MLC - verify non supervisor instructions
    2. tests\TESTINS3.MLC - verify HFP, BFP, and DFP floating point instructions
    3. tests\TESTINS4.MLC - verify new z196 non-supervisor instructions
    4. tests\TESTINS5.MLC - verify new after z196 non-supervisor instructions
    5. tests\TESTDFP1.MLC - verify new DFP instructions
    6. tests\TESTDFP2.MLC - verify DFP assembler data definitions
    7. rt\bat\ZOPCHECK.BAT - verifies all z390 assembler instruction opcodes.

Run rt\bat\RUNCBLTESTS.BAT on Windows | bash/runcbltests on Linux/MacOS
   to compile, link, and execute sample zcobol regression tests.

Run rt\bat\RUNZPAR.BAT on Windows | bash/runzpar on Linux/MacOS
   to create source execution trace TRS files showing source program
   lines and execution instruction trace for both assembler and zCOBOL.

You can also run the demos from the z390 GUI command line which you can start by
running `java -jar z390.jar` or 
running bat\Z390.BAT on Windows | bash/z390 on Linux/MacOS  

For example, start the GUI and enter the command:
(Windows)     bat\asmlg demo\hello.mlc
(Linux/MacOS) bash/asmlg demo/hello.mlc

More information
----------------

More documentation is available at http://www.z390.info 
Project history up to 2012 at http://www.z390.org

Here is link to last z390 SHARE presentation Melvyn and I did together:
   http://z390.org/SHARE/SHARE_8194_z390_February_2008.pdf

Join the z390 user group here: https://groups.google.com/g/z390
Join the z390 developer group here: https://groups.google.com/g/z390development

Don Higgins
don@higgins.net