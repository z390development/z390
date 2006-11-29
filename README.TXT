z390 v1.2.00b PTF 11/29/06
Copyright 2006 Automated Software Tools Corporation

The enclosed z390.jar contains following cumulative fixes for v1.2.00:

1.  RPI 417 implement full bit length DS/DC field support.

2.  RPI 498 Ignore blank lines in MLC and macro source code and do not generate
    blank BAL statements.

3.  RPI 499 merge changes for Linux support contributed by Martin Ward in v1.2.00a
    so the same z390.jar should run on both platforms.  Changes include support for
    case sensitive file system on Linux, separate drive vs directory change command 
    required for Windows, correction to relative path conversion, change to CDE
    compare to accept lower case or upper case member names in directory for BLDL,
    correct created variables to force uppercase variable names.

4.  RPI 500 set default programs for Windows and Linux as follows:
    a.  Acrobat  - start/acroread
    b.  Browser  - start/foxfire
    c.  Commands - cmd.exe/perl
    d.  Editor   - notepad/xemacs
    Also fix following:
    a.  Use system separator vs \r\n for new lines.
    b.  Support running z390 from any directory on Linux.

5,  RPI 501 - support literal - absolute value in operands
    See updated TESTASM1.

6.  RPI 502 - ignore undefined variables in model statements
    See updated TESTASM1.

7.  RPI 503 - Remove duplicate MNOTE's from SYSTERM log

8.  RPI 504 - Update documentation and INSTIME benchmarks
    contributed by Melvyn Maltz.

9.  RPI 505 - Correct TEST mode to prevent duplicate messages on 
    console

10. RPI 506 - Correct z390 GUI to remove duplicate commands from scroll.

11. RPI 507 - Support CFD OUT=reg and CTD IN=reg as well as (reg) and    
    rx format options. See updated TESTCFD1 and TESTCTD1.

    To install PTF follow these steps.

1.  Copy the z390.jar file into install directory for z390
    replacing existing copy of z390.jar.

2.  If you need to change Java source for this PTF version, all
    the Java source is in src directory and can be copied to 
    z390\src directory.

Visit www.z390.org for more information on latest updates.
