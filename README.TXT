z390 v1.2.00a PTF 11/17/06
Copyright 2006 Automated Software Tools Corporation

The enclosed z390.jar contains following cumulative fixes for v1.2.00:

1.  RPI 417 implement full bit length DS/DC field support starting with type A and B.
    (Remainder will be added in v1.2.00b as time ran out today).

2.  RPI 498 Ignore blank lines in MLC and macro source code and do not generate
    blank BAL statements.

3.  RPI 499 merge changes for Linux support contributed by Martin Ward in v1.2.00a
    so the same z390.jar should run on both platforms.  Changes include support for
    case sensitive file system on Linux, separate drive vs directory change command 
    required for Windows, correction to relative path conversion, change to CDE
    compare to accept lower case or upper case member names in directory for BLDL,
    correct created variables to force uppercase variable names.

To install PTF follow these steps.

1.  Copy the z390.jar file into install directory for z390
    replacing existing copy of z390.jar.

2.  If you need to change Java source for this PTF version, all
    the Java source is in src directory and can be copied to 
    z390\src directory.

Visit www.z390.org for more information on latest updates.
