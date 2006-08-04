z390 v1.1.01b PTF 08/04/06
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

6.  RPI 365 correct az390 support L'literal length in expressions.

7.  RPI 366 correct mz390 to handle mixed case macro keywords.

8.  RPI 367 add az390 support for E, D, and L (MIN) and (MAX) values.

9.  RPI 368 add az390 support for S scale factor and ignore ACONTROL
    and ALIAS directives.

10. RPI 369 correct mz390 to ignore EQU 4th and 5th operands.

11. RPI 370 add extension macro CFD to convert from decimal
    number field to floating point or 128 bit integer values.

12. RPI 371 correct az390 to allow spaces in hex literal values.

13. RPI 372 correct az390 to correctly handle new LOCTR defined
    after previous LOCTR which is not the last LOCTR defined.

14. RPI 373 correct az390 alignment of L type field to 8 versus 16.

15. RPI 374 add new LINKLIB directory with the following new programs:
    a.  FPCONVRT.MAC - FP conversion routine contributed by David Bond
    b.  FPCONVRT.MLC - macro call to assemble FPCONVRT.MAC
    c.  ARCHLVL.MAC  - macro to define ARCHLVL for FPCONVRT, FPCONMFC.
    d.  FPCONMFC.MLC - interface from CTD and CFD to FPCONVRT.
    e.  TESTFPC1.MLC - test full range of FP and 128 bit integers.
    f.  TESTFPC2.MLC - test FP (MIN) and (MAX) values for 6 types.
    g.  CVTTOHEX.MLC - callable subroutine to convert to hex
    
16. RPI 375 correct az390 to issue error if USING label misused.

17. RPI 376 correct pz390 to set CC1 when AL?? sets high bit but no
    carry occurs.

18. RPI 377 correct sz390 to prevent DCB recursive error for missing
    DDNAME.
    
19. RPI 378 correct az390 and lz390 to use the first SYSOBJ directory
    to access primary OBJ file.
    
20. RPI update DO structured macro to support compound WHILE and UNTIL.

21. RPI 380 add CVTTOHEX routine to LINKLIB used by TESTFPC1.

22. RPI 381 updated CALL macro to support register parm overrides.

23. RPI 382 correct az390 to correctly generate F and H integer 
    constants up to 16 bytes long.
    
24. RPI 383 fix pz390 MLG and MLGR to multiply R1+1 and S2/R2.

25. RPI 384 fix az390 and pz390 to generate all zeros for HFP EDL.

26. RPI 385 change lz390 to display ESD and RLD info on LST for
    use in debugging complex static links linke TESTFPC1.

27. RPI 386 fix pz390 to prevent trap on MVCL with 0 data length.

28. RPI 387 fix az390 and pz390 to support 20 bit signed displacement
    for LAY and other RXY, SIY,  and RSY type instructions.  See
    FPCONVRT running in ARCHLVL=5 called from TESTFPC1 for example.

29. RPI 388 fix az390 to correct generate duplicate E, D, and L fields.

30. RPI 389 correct pz390 to correctly handle full range of values for
    CVB, CVBY, CVBG, CVD, CVDY, nd CVDG.  See TESTCVB1 and TESTCVBG.

To install PTF follow these steps.

1.  Copy the z390.jar file into install directory for z390
    replacing existing copy of z390.jar.

2.  Copy the updated macro directory mac to z390\mac directory.

CTD.MAC - new convert to display extended formatting service         
CFD.MAC - new convert from display to floating point or 128 bit int.
CALL.MAC - support register parm overrides

3.  Copy the updated CICS directory to z390\cics directory.

none

4.  Copy the udpate LINKLIB directory to z390\linklib directory.

ARCHLVL.MAC  - set ARCHLVL for OS 32 bit or z/OS 64 bit linkage etc.
CVTTOHEX.MLC - callable routine to convert any field to hex
FPCONVRT.MAC - floating point conversion to/from decimal string
FPCONVRT.MLC - macro call to assembler FPCONVRT
FPCONMFC.MLC - interface from CTD and CFD macros for LINKAGE=MF
TESTFPC1.MLC - test full range of FP and 128 bit integers
TESTFPC2.MLC - test (MIN) and (MAX) values for all 6 FP types.

4.  If you need to change Java source for this PTF version, all
    the Java source is in src directory and can be copied to 
    z390\src directory.

5.  The following regression test sources
    are included:

TESTCTD1.MLC - test convert to display macro and svc 170
TESTCFD1.MLC - test convert to display macro and svc 170
TESTCVB1.MLC - test CVB, CVBY, CVD, CVDY
TESTCVBG.MLC - test CVBG and CVDG
TESTASM1.MLC - test L'lit expression
TESTDC1.MLC  - test duplicate E, D, L
TESTERR7.MLC - test DCB missing DDNAME
TESTRLD1.MLC - test new lz390 listing

6.  Copy updated commands to the z390 root directory

ASM.BAT   - allow up to rc=4
ASML.BAT  - allow up to rc=4
ASMLG.BAT - allow up to rc=4
EXEC.BAT  - allow up to rc=4
LINK.BAT  - allow up to rc=4
RT7.BAT - assemble, link, and execute and compare with no
          return code limits.
RTLINK.BAT - assemble, link, and test all LINKLIB programs

Visit www.z390.org for more information on latest updates.
