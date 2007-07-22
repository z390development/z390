z390 v1.3.06a PTF 07/22/07
Copyright 2007 Automated Software Tools Corporation

The enclosed z390.jar contains following cumulative fixes for v1.3.06:

1.  RPI 657 - work-around for Windows VIsta to allow using z390 installed in
    the default C:\Program Files protected directory by going to the
    control panel, user accounts, dialog and turning off User Account Control (UAC)
    This allows reading and writing files without administrator level authorizaiton.
    The other work-around is to install to another directory outside C:\Program Files.

2.  RPI 658 - correct regression test BAT files for including new vse and util 
    directory programs added in v1.3.06 and change DEMOVSE1.MLC and DEMOVSE2.MLC
    to remove dependency on demo\DEMOWTO1.MLC.  Also add STDDEVLB.MLC demo to the
    demo directory and regression tests (this floating point standard deviation
    calculation in 128 bit floating point was included in prior PTF). Remove erroneous
    file copies from util\unref\save directory to prevent RT file compare errors. 

3.  RPI 659 - Add 2 new errors for compatibility with HLASM.  Add error 196 for opcode 
    with invalid character insteAd or missing opcode.  Add error 218 for macro variable
    generated label symbol with invalid characters including * or . (previously allowed).
    Also cleanup trace and error messages to eliminate duplicate ids and messages.See new
    regression test TESTLAB1.MLC.

4.  RPI 660 - correct ESTAE exit processing to reset PSW to address in R0 when
    retry is requested via R15=4 at exit from ESATE exit.  See updated regression tests
    TESTSTA1.MLC - TESTSTA3.MLC.

5.  RPI 661 - correct ez390 runtime to synchronize the closing of log file, check
    if log available for writing, and log to console if not to prevent errors 
    during shutdown with multiple tasks running.

6.  RPI 662 - add 2 additional CICS compatible DFHRESP literals - ITEMERR=F'26'
    and QIDERR=F'44'.  Add new z390 option TS to include JDBC SQL compatible timestamp
    on each TRACE option record with 9 digit fractional nanoseconds:
         yyyy-mm-dd hh:mm:ss.nnnnnnnnn
    nano-second.  Note timings shown include the overhead of formating timestamp
    and writing records to buffered trace file.  See bmk\INSTTIME.MLC for more 
    accurate instruction timings.  The TIME macro and svc have been updated to
    support a new TS,addr option to return current JDBC timestamp (29 characters) 

7.  RPI 663 - this PTF z390.jar was compiled and built using J2SE JDK 1.5.0_12
    and was tested with J2SE runtimes 1.5.0_11, 1.5.0_12, and 1.6.0u2 on XP and Vista.


To install this cumulative PTF for z390 v1.3.04 follow these steps.

1.  Extract the PTF zip file into the z390 install directory
    creating ptf subdirectory.

2.  Copy z390.jar from ptf directory to the z390 root directory
    replacing previous version.

3.  Copy the updated TIME.MAC macro to the z390\mac directory for support
    of new TIME TS,addr option for current 29 character JDBC timestamp.


4.  If you need to change Java source for this PTF version, all
    the Java source is in src directory and can be copied to 
    z390\src directory.

5.  Run IVP.BAT command from the z390 directory and verify 
    the new z390 version running matches this PTF version.

Visit www.z390.org for more information on latest updates.
