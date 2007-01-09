z390 v1.2.00g PTF 01/08/07
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

12. RPI 508 - correct z390 GUI error on /SC startup commands and
    invalid path on CD commands. (Regressions due to Linux changes)

13. RPI 509 Change default font for both Windows and Linux to "Monospaced"
    for improved graphics on Linus and no change on Windows.

14. RPI 510 update default utility programs for Windows and Linux and
    add environment variables to override defaults:
    Z390ACROBAT=start/acroread
    z390BROWSER=start/firefox
    z390COMMAND=cmd.exe/perl
    z399EDUTIR=notepad.exe/xemacs

15. RPI 511 - add option MCALL to show MCALL and MEXIT comments on PRN listing
    showing every macro call and exit with instance # and nesting level.

16. RPI 512 - update CFD macro and svc to allow floating values for INT128 input
    and update return codes as follows:
    RC=0 ok
    RC=8 invalid type or register
    RC=12 invalid value (out of range)

17. RPI 407 add initial support for Decimal Floating Point instructions and data
constants ED, DD, and LD.  All 54 instructions can now be assembled by az390 and the
following initial instructions are supported by ez390 runtime: ADTR, SDTR, MDTR, DDTR,
CDTR, EEDTR, and CSDTR.  See new regression test TESTDFP1.MLC plus updated TESTINS1.MLC,
TESTFPC1.MLC, TESTFPC2.MLC, and TESTDC1.MLC.  See new demo DEMODFP1.MLC which shows the
different results adding 1.1+2.2 using HFP DH versus DFP DD formats due to irrational 
binary fractions using binary versus exact result using decimal.

18. RPI 513 - correct CFD and CTD suc support to correctly set return code and update
    TESTCFD1 and TESTCTD2 regression tests to test return codes.

19. RPI 514 - add CFD and CTD support for Decimal Floating Point (DFP) conversions
    between 45 character decimal scientrfic notation and ED, DD, or LD formats.  See
    TESTFPC1, TESTFPC2, TESTCDF2, and new DEMODFP2 programs using these new conversions.

20. RPI 515 change interactive debug TEST mode to strip the E370I prefix off the
    console output in order to see more trace info on each line.

21. RPI 516 change mz390 PUNCH correct code for support for '' and && reduction
    in PUNCH output.

22. RPI 517 correct all DFP operations to use current rounding mode to round any
    irrational values to the current ED, DD, or LD precision of 7, 16, or 34 digits.

23. RPI 518 add support for AXTR, SXTR, MXTR, and DXTR operations, plus fix fetch
    specification errors on HFP, BFP, and DFP for extended operation register pairs,
    plus fix CTD to handle leading 0 for fractional values.

24. RPI 406 add support for remaining DFP instructions to pz390 for total of 57 new
    decimal floating point instructions.  See updated TESTDFP1.MLC.

25. RPI 519 correct mz390 to prevent erroneous error 132 when macro file name and 
    prototype name are different cases.

26. RPI 520 publish updated Macro and SVC Guide with CFD and CTD support for DFP
    type conversions.

27. RPI 521 correct mz390 to prevent trap with brank to undefined label on AIF is not
    taken during pseudo code generation and then is taken on later pseudo code execution. 

28. RPI 522 Add additional DFP regression tests to RT\TESTDFP1.MLC

29. RPI 523 Update to Sun Microsystems J2SE 1.5 Update 10 for regression tests.

30. RPI 524 Add new TCPIO macro for use in establishing TCP/IP connections and sending
    and receiving messages between clients and servers running locally or anywhere on
    TCP/IP network.  See new SOA directory with DEMO which can be run as statically
    linked main program calling to routines and then again as generated SOA application.
    See SOA\DEMO\DEMOSTD.BAT and SOA\DEMO\DEMOSOA.BAT.  Also see readme and timing
    statistics report in the SOA directory.

31. RPI 525 Change mz390 to log subscript errors and continue in attempt to process all
    source code in main program rather than aborting.

32. RPI 526 Change sz390 to trap any CFD conversion errors and simply exit with RC=12.

33. RPI 527 Add missing DFP instructions CPSDR, LCDFR, LDGR, LGDR, LNDFR, and LPDFR.
    Also fix rounding for FIDTR, FIXTR, CGDTR, and CGXTR.

34. RPI 528 Correct mz390 to strip trailing period after subscripted variable and remove
    previous incorrect logic in variable replacement which was causing inconsistent period
    handling after subscripted variables.

35. RPI 529 Change mz390 to issue error for undefined macro varaiable when running
    macro processor without assembler.

36. RPI 530 Add option FORMAT extension to PUNCH operation to request continuation
    formatting for MLC and BAL type output files.  The default for PUNCH is variable
    length ASCII text with no continuation formatting.

37. RPI 531 change pz390 to correct a number of DFP and other instruction trace formats
    including SRP.  Also rewrite the trace code using table driven logic to simplify
    future additions.

To install PTF follow these steps.

1.  Copy the z390.jar file into install directory for z390
    replacing existing copy of z390.jar.

2.  If you need to change Java source for this PTF version, all
    the Java source is in src directory and can be copied to 
    z390\src directory.

Visit www.z390.org for more information on latest updates.
