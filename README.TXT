z390 v1.3.08b PTF 11/07/07
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
