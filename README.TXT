z390 v1.3.04a PTF 06/10/07
Copyright 2007 Automated Software Tools Corporation

The enclosed z390.jar contains following cumulative fixes for v1.3.04:

1.  RPI 632 correct ORG and CSECT processing as follows:
    a) Add ORG old and new address on PRN listing
    b) Align CSECT after LTORG on double word
    c) Show module relative addresses for A,Y,V constants on 
       listing (the actual object code is CSECT relative)
    d) Reduce number of passes required to resolve addresses
       for programs which use lookahead scan options.
    See new regression test TESTORG1.MLC.

2.  RPI 633 correct macro progressor to support any operand in the
    label field of a macro call statement.
    See updated regression test TESTMAC1.MLC.

3.  RPI 635 correct assembler spurious errors replacing 
    DFHRESP(type) with literal reference.  See updated TESTCIC1.MLC.

4.  RPI 636 add support for ESTAE exit restart or percolate based
    on required return code in R15.  If 4 restart at ESTAPSW.
    If 0 percolate to next ESTAE exit and purge link stack to 
    match program issuing the next ESTAE exit. See new regression
    tests TESTSTA2.MLC and TESTSTA3.MLC.

5.  RPI 637 correct assembler to issue error on missing close
    parenthesis on explicit offset(length,base) type operands.
    See updated regression test TESTERR2.MLC.

6.  RPI 638 upgrade to J2SE 6.0.00 update 1 for primary source 
    debugging and regression testing.  Upgrade to Eclipse 3.2.2
    for source editing and debugging to prevent suprious traps
    running on Windows Vista.  Note for J2SE 5.0 compatibility
    the final z390.jar for each release is recompiled and built
    using J2SE 1.5.00 update 11.  z390 J2SE 5.0 compatibility 
    will continue to be maintained through the remainder of 2007.
    Starting in January 2008 new z390 releases will require J2SE 6.0
    and will start to include 6.0 features such as SQL support.

7.  RPI 639 update RETURN macro to correctly support RC=(reg)
    format for return code in register.


To install this cumulative PTF for z390 v1.3.04 follow these steps.

1.  Extract the PTF zip file into the z390 install directory
    creating ptf subdirectory.

3.  Copy z390.jar from ptf directory to the z390 root directory
    replacing previous version.
  

2.  Copy updated macro source files into z390\mac directory:

RETRUN - support RC=(reg) option

3.  If you need to change Java source for this PTF version, all
    the Java source is in src directory and can be copied to 
    z390\src directory.

4.  Run IVP.BAT command from the z390 directory and verify 
    the new z390 version running matches this PTF version.

Visit www.z390.org for more information on latest updates.
