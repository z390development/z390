z390 Regression Test readme as z390 v1.3.01 02/06/06

The following regression test commands are included with z390 install
and can be run from z390 GUI or command prompt to verify z390 works:

RTCICS.BAT - run all the programs in CICS directory and verify 
RTDEMO.BAT - run all the programs in DEMO directory and verify 
RTGUAM.BAT - run interactive TN3290 programs in GUAM directory
             and verify the BAL, PCH, PRN, and LST files match.
             The user must enter text and END or PF3 to end each programs.
RTLINK.BAT - run all the programs in LINKLIB directory and verify 
RTSOA.BAT  - run SOA client server demo application generation and execution.
             This will start server as separate process in foreground.  
             User must return to the orinial process window and press enter
             to proceed to execution client application once server is running. 

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

All of the above regression tests have been successfully run on both 
Windows XP and Ubuntu 6.06 LTS Linux.  z390 includes perl shell utility
programs to support execution of DOS BAT files and subset of DOS commands
required to run all the z390 BAT files.  Visit www.z390.org for more 
information about Linux including detail download and installion guide.

The z390 Portable Mainframe Assembler and Emulator
has extensive regression tests in RT\TEST to verify that
components work as expected.  The RT directory
contains a large and growing collection of test programs for batch
testing of mz390, az390, lz390, and ez390 using the test
programs in RT/TEST.  The expected generation test files
are in RT/TEST/SAVE and the test output comparison files are in 
RT/DIF.

Th RTTEST.BAT command will assemble, link, and execute all the 
programs in the RT/TEST directory, For Windows, it will then use LINKLIB\RTGENDIR and 
LINKLIB\RTGENCMP programs to build RTGENCOM.BAT file which is then
executed to compare each file in RT/TEST with each file in RT/TEST/SAVE.
Any missing files will cause pause statment to be generated showning the
name of the missing file.  After all the comparison files are generated,
the comparison files are then sorted and listed in descending order by 
size to show any changes between the current test files and the last
saved files.  If any missing files were found, the RTGENCMP.BAT file is
also copied to the RT/DIF directory so it shows up first and can be 
viewed at any time to see which files are missing.  For Linux the utility diff
is used to generate single alphabetical dif\DIFF.TXT file showing differences.
See RTCOMP.BAT which has separate paths for Windows and Linux and shows how 
the above MLC open macro code programs work.  See RTLIST.BAT which sorts and lists
the compare files.

For more information visit www.z390.org.  Any and all feedback and
suggestions are welcome.

Don Higgins
President, Automated Software Tools Corporation
don@higgins.net


