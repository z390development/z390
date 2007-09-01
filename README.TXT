z390 v1.3.07b PTF 09/01/07
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
