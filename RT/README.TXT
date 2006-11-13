z390 Regression Test readme as z390 v1.2.00 11/13/06

The following regression test commands are included with z390 install
and can be run from z390 GUI or command prompt to verify z390 works:

RTLINK.BAT - run all the programs in LINKLIB directory and verify 
RTCICS.BAT - run all the programs in CICS directory and verify 
RTDEMO.BAT - run all the programs in DEMO directory and verify 
RTGUAM.BAT - run interactive TN3290 programs in GUAM directory
             and verify the BAL, PCH, PRN, and LST files match.
             The user must enter text and END or PF3 to end programs.

The following additional regression tests can be run after downloading
and installing the RT and MVS optional directories:

RTMVS.BAT  - run all the programs in MVS\DEMO and MVS\TEST and verify
             (This requires download and installation of MVS directory
             which contains entire MVS 3.8J public domain macro library.
             The MVS\DEMO\DEMO.MLC program can be assembled, linked, and
             executed using the MVS\MACLIB directory and WTO macro.)
RTTEST.BAT - run all the progams in RT\TEST and verify
             (This requires download and installation of RT directory)
RT.BAT     - run RTDEMO, RTCICS, RTLINK, RTMVS, and RTTEST

The z390 Portable Mainframe Assembler and Emulator
has extensive regression tests in RT\TEST to verify that
components work as expected.  The RT directory
contains a large and growing collection of test programs for batch
testing of mz390, az390, lz390, and ez390 using the test
programs in RT/TEST.  The expected generation test files
are in RT/TEST/SAVE and the test output comparison files are in 
RT/DIF.

Th RTTEST.BAT command will assemble, link, and execute all the 
programs in the RT/TEST directory, It will then use LINKLIB\RTGENDIR and 
LINKLIB\RTGENCMP programs to build RTGENCOM.BAT file which is then
executed to compare each file in RT/TEST with each file in RT/TEST/SAVE.
Any missing files will cause pause statment to be generated showning the
name of the missing file.  After all the comparison files are generated,
the comparison files are then sorted and listed in descending order by 
size to show any changes between the current test files and the last
saved files.  If any missing files were found, the RTGENCMP.BAT file is
also copied to the RT/DIF directory so it shows up first and can be 
viewed at any time to see which files are missing.  See RTCOMP.BAT which
shows how the above MLC open macro code programs work.  See RTLIST.BAT
which sorts and lists the compare files.

For more information visit www.z390.org.  Any and all feedback and
suggestions are welcome.

Don Higgins
President, Automated Software Tools Corporation
don@higgins.net


