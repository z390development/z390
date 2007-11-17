z390 v1.3.08e PTF 11/17/07
Copyright 2007 Automated Software Tools Corporation

The enclosed z390.jar contains following cumulative fixes for v1.3.08:

1.  RPI 712 corrections to GUAM TN3270 support for CICS:
    a.  Do not return any data in buffer for TGET if using
        NOWAIT and no data ready (was returning cursur etc.).
    b.  Reset screen and set cursor to (1,1) for CTRL-C clear key.
    c.  Fix trap occurring in ez390 interval timer exit due to bad subscript.
2.  RPI 713 corrections to BAT and PL scripts for Linux support:
    a) Correct iz390 for uppercase of Z390.IVP files on Linux.
    b) Correct RTLINK1.BAT to use lower case path mac+linklib for UNREF utility.
    c) Correct RT.BAT to remove RTUTIL refs and add VSAM diff file for Linux.
    d) Correct cmd.pl perl script to support SET commands with = character in the 
       set value such as [RECFM=FT] for REPRO files.
    e) Add new command COMP1.BAT to handle both Windows and Linux file comparisons using 
       fc and diff utilities which create diff output file and set return code for files
       with more than just differences in white space.
    f) Correct file I/O handlers to handle relative paths on Linux where the logical z390
       current directory may be different from the J2SE/OS current directory.
    g) Update cmd.pl perl script to pause and exit if unknown BAT file executed.
3.  RPI 717 set R15=0 for all successful TIME macro operations.
4.  RPI 718 update OPEN and CLOSE macros to issue MNOTE error if register in the form
    NN or RNN is missing extra required (..).
5.  RPI 719 add new option LOG(filename) to override the default main program filename
    for the LOG, ERR, and TRE files.  This option should be used to insure unique files
    when more than one instance of the same z390 program is started via CMDPROC svc.
6.  RPI 720 eliminate recursive abort error message following missing MLC source file error 39.
7.  RPI 721 correct WTOR svc to purge any pending CMDPROC input from queue priot to waiting
    for next CMDPROC input msg following WTOR message.  The input may come from 3 sources:
    from main GUI input if main program, GUAM dialog if active, or z390 parent task CMDPROC
    write command for CMDPROC concurrent processes.  z390 programs that use WTOR cannot be 
    executed from within nested BAT commands which depending on following BAT commands.
8.  RPI 724 add new REPRO sequnntial file formats to support all VSAM file types
    including record lengths to 2 GB.  Change RRDS fixed length to use VX0 index like
    VRRDS does to save space in sparce files.  Add buffering and total record statistics.
    See new regression tests and online documentation. Correct RRDS relative record
    numbers to always start with 1.
9.  RPI 725 - remove -Xmx java runtime option no longer required for Vista or other
    systems with sufficient storage.  See updated EZ390.BAT with options documented.
10. RPI 726 - supress error 187 for first address change in assembly if there are other errors.
11. RPI 728 - changes to support assembly of ASMG sources from University of Waterloo:
    a.  RENAME MVS macro DLETE.MAC to DELETE.MAC
    b.  Support ICTL and ignore ISEQ during macro processing.  The generated BAL
        source is in standard format and includes ICTL and ISEQ statements are
        ignored by the assembler.
12. RPI 729 - add CICS DFHRESP code ILLOGIC with value F'21'.  See rt\test\TESTCIC1.
13. RPI 730 - correct LOG(filename) option to use program path if any.
14. RPI 731 - Change CMDPROC started task handling as follows to suppor long running
    CICS and SOA applications:
    a.  Optimize CMDPROC messaging to use line at a time versus character at a time.
    b.  Use option MAXLOG(mb) with default 1 mb to limit size of GUI scrolling log.
        When size limit is reached the log is truncated 50% so it is no longer possible
        to view entire log in GUI.  The log file still has all messages.
    c.  Use option MAXQUE(msgs) with default 1000 to limit the number of messages
        that a CMDPROC task can queue in memory before the parent task starts copying
        them all to log and removing them from queue. 
    d.  Add keyword CMDLOG=YES to CMDPROC macro to indicate default is to copy
        all standard output messages to parent task log.  If CMDPROC=NO is specified
        then the parent task must issue CMDPROC READ before queue reaches MAXQUE limit.
    e.  Add support for CMDPROC tasks to request parent to abort by sending
        standard output message "z390_abort_request". This message is processed
        immediately and is not put on queue.  
    See updated rt\test\TESTCMD1 and TESTCMD2 regression tests. 
15. RPI 734 - correct structured macro ELSEIF to fix code generation error for
    second condition connected by OR on ELSEIF. 
16. RPI 732 - add linker support for optional command file with .LNK suffix containing
    one or more sets of the following linker commands:
    a)  INCLUDE ddnam(name) - default is linkname.obj 
    b)  ENTRY   name        - default is offset 0 in load module 
    c)  ALIAS   name        - creates stub 390 file with ascii name of 390 file to run
    d)  NAME    name        - creates name.390 with included obj files and specified entry
    With this PTF a single LINK command file was used to link the 44 ASMG obj files into 24
    390 load modules with 3 aliases.  See new regression test TESTLNK2.LNK
    using 2 INCLUDE's, ENTRY, and ALIAS execution.  See RTTEST1 for commands used.
17. RPI 734 - consolidate all the TRACE options into TRACE(AEGLMPQTV)
    where letters represent the following:
    A - trace az390 assembler creating .TRA file
    E - trace ez390 instruction execution creating .TRE file
    G - trace all GETMAIN/FREEMAIN requests on .TRE
    L - trace lz390 linker creating .TRL file
    M - trace mz390 macro propcessor creating .TRM file
    P - trace mz390 macro pseudo code generation/execution on .TRM file
    Q - trace all QSAM DCB I/O data transfers on .TRE file
    T - trace all TCPIO messaging transfers on .TRE file
    V - trace all VSAM ACB I/O data transfers on .TRE file
   The existing TRACE option is the same as TRACE(E) and
   the TRACEALL option is the same as TRACE(AEGLMQTV)
18> RPI 735 - Correct new linker LNK support:
    1) Change LNK to LKD to avoid conflict with Windows shortcuts
    2) Ignore CTRL-Z (hex 1A end of file codes in LKD source code
    3) Prevent trap when INCLUDE DDNAME is not defined
    4) Issue message ignoring all unrecognized command such as SETOPT and PARM(....)
    5) Check for explicit .OBJ on lz390 file parm and ignore LKD
    6) Assign default starting address of load module to matching EXTRN
       if there is no matching CSECT/ENTRY in load module.
19. RPI 737 - Misc. corrections and enhancements:
    1) Add mz390 warning message if non-comment type statements found after
       END statement with option ASM.  
    2) Add option STATS(filename) to append all statistics output from mz390,
       az390, lz390, and ez390 to filename.STA.  Use of this option turns off
       statistics as comments at end of BAL, PRN, LST, and LOG files.
    3) Correct spelling of "version" in IVP message.
    4) Correct superzap spelling on command prompt
    5) Correct superzap.bat rem, spelling, and remove pause
    6) Correct superzap display of each command read once
    8) Correct superzap SYSIN file I/O error to display error message and exit
       rather than issue user abend.
    9) Correct z390 GUI interface to route all input to running CMD task until
       it terminates.  Enter exit or ctrl-break to force CMD end.
    10) Update superzap web page documentation
    11) Correct az390 assembler to handle Enn exponent modifier on F and H type constants.
    12) Prevent trap on ASCII character literal conversion to EBCDIC for hex codes 00-ff.          ASCII with high bit maps to same EBCDIC character as ASCII with no high bit.
        When converting EBCDIC source to ASCII for use with z390, translation use a tool
        that reports any hex literals with non-ASCII characters as errors.  These must be         converted from C'...' to X'....' format for portability to ascii systems.
20. RPI 738 - correct trace for LAY instruction to prevent address exception
    when target address is beyond memory.
21. RPI 739 - correct RRDS and KSDS VSAM access to use OPTCD=(SEQ) to determine
    sequential versus KEY.
22. RPI 740 - issue warning MNOTE if macro proto-type name does not match macro file.  Add     option CHKMAC to issue MNOTE warning if macro statments follow final MEND in macro file. 


To install this cumulative PTF for z390 follow these steps.

1.  Extract the PTF zip file into the z390 install directory
    creating ptf subdirectory.

2.  Copy z390.jar from ptf directory to the z390 root directory
    replacing previous version.

3.  Copy the PL scripts and BAT files to the z390 install directory.

4.  Copy the vsam directory to the z390 install directory.

5.  Copy the linklib directory to the z390 install directory.

6.  Copy the mac directory to the z390 install directory/

7.  If you need to change Java source for this PTF version, all
    the Java source is in src directory and can be copied to 
    z390\src directory.

8.  Run IVP.BAT command from the z390 directory and verify 
    the new z390 version running matches this PTF version.

Visit www.z390.org for more information on latest updates.
