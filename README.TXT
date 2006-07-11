z390 v1.1.01a PTF 07/11/06
Copyright 2006 Automated Software Tools Corporation


The enclosed z390.jar contains following cumulative fixes for v1.1.01:

1.  RPI 359 correct mz390 to correctly handle mixed case keyword
    parameters.  Previously they had to be caps to work correctly.
    See TESTMAC1 updated regression test.

2.  RPI 360 add macro CTD to support covert to display extended 
    output formatting functions using new svc 170 with type code,
    input address, and output address passed via register 1.  The
    convertions currently supported are as follows:
    1) Convert 128 bit integer to 40 byte display
    2) Convert EH short to 40 byte scientific notation
    3) Convert EB short to 40 byte scientific notation
    4) Convert DH long  to 40 byte scientific notation
    5) Convert DB  long to 40 byte scientific notation
    6) Convert LH extended to 40 byte scientific notation
    7) Convert LB extended to 40 byte scientific notation
    These output services parallel the print formatting services
    available in HLA, C++, COBOL, and Java.  See www.mfatc.org
    new SQRT2 demos for examples.  Also see new z390 regression
    test TESTCTD1.MLC.  

3.  RPI 361 add regression test RT7.BAT that was left out of
    the v1.1.01 build.

4.  RPI 362 change ASM, ASML, ASMLG, LINK, and EXEC commands to 
    allow returns of 0-4 without pausing to check errors.

5.  RPI 364 correct pz390 SQDB square root instruction to prevent
    loss of precision.  It was returning only 32 bit precision 
    instead of 64.  See TESTCDT1 regression test and www.mfatc.org
    SQRT2.MLC demo.

To install PTF follow these steps.

1.  Copy the z390.jar file into install directory for z390
    replacing existing copy of z390.jar.

2.  Copy the updated macro directory mac to z390\mac directory.

CDT.MAC - new convert to display extended formatting service         

3.  Copy the updated CICS directory to z390\cics directory.

none

4.  If you need to change Java source for this PTF version, all
    the Java source is in src directory and can be copied to 
    z390\src directory.

5.  The following regression test sources
    are included:

TESTMAC1.MLC - test mixed case keyword parm
TESTCDT1.MLC - test convert to display macro and svc 170

6.  Copy updated commands to the z390 root directory

ASM.BAT   - allow up to rc=4
ASML.BAT  - allow up to rc=4
ASMLG.BAT - allow up to rc=4
EXEC.BAT  - allow up to rc=4
LINK.BAT  - allow up to rc=4
RT7.BAT - assemble, link, and execute and compare with no
          return code limits.

Visit www.z390.org for more information on latest updates.
