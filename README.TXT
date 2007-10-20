z390 v1.3.08a PTF 10/20/07
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

To install this cumulative PTF for z390 follow these steps.

1.  Extract the PTF zip file into the z390 install directory
    creating ptf subdirectory.

2.  Copy z390.jar from ptf directory to the z390 root directory
    replacing previous version.

3.  Copy the PL scripts and BAT files to the z390 install directory.

4.  Copy the vsam directory to the z390 install directory.

5.  If you need to change Java source for this PTF version, all
    the Java source is in src directory and can be copied to 
    z390\src directory.

6.  Run IVP.BAT command from the z390 directory and verify 
    the new z390 version running matches this PTF version.

Visit www.z390.org for more information on latest updates.
