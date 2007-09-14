z390 v1.3.07d PTF 09/14/07
Copyright 2007 Automated Software Tools Corporation

The enclosed z390.jar contains following cumulative fixes for v1.3.07:

1.  RPI 630 - correct GUAM GUI interface support as follows:
    a.  Support variable TN3270 screen font sizes adjusted to fill current screen and 
        correct font display to eliminate clipping of characters and residual cursor bits.
    b.  Correct delete key function to delete within current input field on screen.
    c.  Correct keyboard input handler to suppress Windows default PF10 key function
        which previously caused "File" menu to popup.
    d.  Correct recursive screen update loop causing memory leakage for long running
        TN3270 screen applications such as CICS.
2.  RPI 670 Correct TCPIO and all other macros to suppress generation of LR when source
    and destination registers match using new macro ZOPTLR.  Also correct macro trace 
    option to correctly show SETA and SETB values in AIF compare traces.
3.  RPI 671 Correct TGET with ASIS to return all modified characters when there are no
    fields defined on screen.  Also add TRACET support to dump TGET/TPUT data.
4.  RPI 673 macro assembler as follows:
    a.  Correct macro processor to allow symbolic substitution for inline macro
        proto-type name.  See rt\test\TESTMAC1.MLC updated regression test.
    b.  COrrect assembler to support symbolic expressions in DROP operations.
        See rt\test\TESTASM1.MLC updated regression test.
    c.  COrrect assembler symbolic expression complex RLD reduction to handle
        multiple CSECT symbols which reduce to absolute or relative value.
        These fixes now allow 24k line SPITBOL program to assemble without error. 
5.  RPI 674 update linklib\CVTTOHEX.MLC callable routine to use 512 byte table with
    the RTOT instruction to translate any number of bytes to hex in one instruction.
6.  RPI 677 Correct DCB PUT support to prevent 0C5 checking end of buffer address
    instead of end or record address.
7.  RPI 680 correct initial VSAM macro errors as follows:
    a.  RPL - correct OPTCD loop limit
    b.  Remove RPL IN and OUT options from demo programs
    c.  ACB - correct misspelling of mutually (10 times)
    d.  MODCB - correct misspelled RPLLREC DSECT symbol reference
8.  RPI 685 - ignore nulls in DCB FT and VT input files rather than treating
    as end of record marker.
9.  RPI 686 - correct both z390 main GUI window and GUAM dialog window to 
    allow full display of both command line and status line.
10. RPI 687 - add VSAM response codes to CICS DFHRESP support.  See updates to
    rt\test\TESTCIC1.MLC regression test.
11. RPI 688 - correct errors in initial VSAM support, add ESDS RBA and XRBA
    support, add RRDS support, add RPL=(reg) support, add RBA and XRBA field
    support to SHOWCB, TESTCB, and MODCB.  Add RRDS demo programs and add new
    regression tests in vsam\test directory for fixed and variable ESDS and RRDS.
    Add new z390 option TRACEV to trace all data for VSAM file I/O operations. 
    Correct and test REPRO support for both fixed and variable length record QSAM
    to/from VSAM load and unload.
12. RPI 689 - correct TRACET to support TS timestamp option.  Route all dump output to
    just TRE file unless TEST and TRACE are off.  All log messages appear on both LOG 
    and TRE file.
13. RPI 681 redefine VCDT as VSAM catalog with multiple base cluster, alternate index,
    and path entries.  At ACB open time the DDNAME points to catalog and user can
    specify which entry to open via suffix on catalog name or using ACB name as default.
    See vsam\demo\DEMOCAT.MLC and vsam\test\TESTCAT.MLC for examples and run RTVSAM.BAT
    regression tests.
14. RPI 690 correct DFHRESP codes NOTEND to be NOTFND.  Add MODCB RPL support
    for ARG= and OPTCD=.  Add MODCB ACB MACRF= support. See updated rt\test\TESTCIC1.
15. RPI 691 correct mz390 macro assembler to terminate parsing global and local
    macro variable definitionss with singe space prior to comments.
16. RPI 672 add ESDS support for fixed and variable length UPD, LRD, BWD for update
    and read backwards.  Eliminated need for index file on ESDS fixed and variable 
    and RRDS fixed length record files.  Correct logic to always use RPL options 
    versus ACB flags to determine current request type.  Correct numerious errors
    in macros and logic.  Add the following new VSAM regression tests:
    a)  ESF2TEST - test ESDS fixed length update and RBA/XRBA access
    b)  ESF3TEST - test ESDS fixed length sequential read backwards
    c)  ESV3TEST = test ESDS variable update and RBA/XRBA access
    d)  ESV4TEST - test ESDS variable sequential read backwards
17. RPI 694 - add new option ERRSUM for critical error summary in place of
    normal assembly.  This option shows any missing macros and copybooks which
    must be fixed by adding correct library paths to SYSMAC and SYSCPY parms.
    If all macro and copybooks have been resolved, ERRSUM then shows any undefined
    symbols which usually indicates need to add macro or copybook reference.
    This option is turned on automatically if there are missing macros or copybooks.
    To force assembly with these critical errors use ERR(0).  The symbol table now
    includes all undefined symbols with TYPE=UND.
18. RPI 695 correct macro processor to insert comma for single null parm on inner macro
    call to prevent following comments from being parsed as first parm in error.  See
    updated regression test rt\test\TESTMAC1.


To install this cumulative PTF for z390 follow these steps.

1.  Extract the PTF zip file into the z390 install directory
    creating ptf subdirectory.

2.  Copy z390.jar from ptf directory to the z390 root directory
    replacing previous version.

3.  Copy the entire mac directory containing updated macros to the
    z390\mac directory.

4.  Copy the entire vsam directory containing updated demo and 
    regression tests. 

5.  Copy the entire linklib direcotory containing updated REPRO utility  
    and CVTTOHEX utility.

6.  If you need to change Java source for this PTF version, all
    the Java source is in src directory and can be copied to 
    z390\src directory.

7.  Run IVP.BAT command from the z390 directory and verify 
    the new z390 version running matches this PTF version.

Visit www.z390.org for more information on latest updates.
