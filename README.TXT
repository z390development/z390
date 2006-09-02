z390 v1.1.01e PTF 09/02/06
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

31. RPI 390 and RPI 405 correct mz390 to support O'opcode operator
    which is used in updated LINKLIB\FPCONVRT.MAC contributed by
    David Bond which now has support for ED, DD, and LD decimal
    floating point constants. 

32. RPI 391 correct LINKLIB\FPCONMFC to remove trailing nulls in
    decimal string output from FPCONVRT.

33. RPI 393 issue SF05 abend if WAIT count exceeds list ECB count.

34. RPI 394 correct GETMAIN to ignore SP= per documentation.  All
    getmains are in same pool and therefore purge subpool is not 
    supported yet.

35. RPI 396 change TESTTIM2 to allow up to 15 seconds to get 2 seconds
    of real cpu time in STIMER test.  This allows test to run ok when
    Windows background tasks take up 50%+ of cpu time.

36. RPI 397 issue S0C5 on all memory access violations within pz390.
    Previously some where only trapped by ez390 causing synchronization
    problem with log.

37. RPI 398 fix D and DR instructions to handle case where R1 and R1+1 
    are not zero.  Also speed up D, DR, SLDL, SLDA, SRLD, and SRDA by 
    using long shift and or versus byte buffer to build 64 bit operation
    from disjoint register storage.

38. RPI 401 fix mz390 to expand arrays as required to satisfy multiple 
    store values on SET statements.  Prevously only declaraction or 
    first subscript could trigger expansion.

39. RPI 402 change mz390 to report MNOTE total warnings 1-4 level.

40. RPI 403 fix az390 to correctly restore original opcode usage 
    after OPSYN temporary cancel after saving original for restore.

41. RPI 404 add mz390 missing operator functions including conversions
    such ?2?, DCLEN, DCVAL, DCEQUOTE, ISBIN, ISDEC, ISHEX, ISSYM, SLA
    SLL, SRA, SRL, SIGNED, SYSATTRP, SYSATTRA.

42. RPI 408 expand gz390 TN3290 default screen size to include extra
    5 pixels in both directions to avoid cutting off edges.  Also
    fix abend occurring in DEMOGUI5 due to missing graphics support 
    pending.

43. RPI 409 improve overall performance of mz390 and az390 by 10%+
    by eliminating extra string concatenation for every find_key_index
    call which is the most commonly used function in z390.

44. RPI 410 correct mz390 to issue error if AREAD or PUNCH extension
    parms DSNAME=,DDNAME=,DSN=,ID= are invalid.  Return 0 length string
    at end of file and single space for blank lines. Support variables
    in the parm fields, and and AREAD and PUNCH stats.

45. RPI 411 improve speed of mz390, az390, lz390, and pz390 (MVC, MVCL)
    by replacing array initializtion and copy with Arrays.fill and 
    System.arraycopy native speed functions.  And reduce string garbage
    collection overhead by replacing string concatenation loops with
    fill function where possible.

46. RPI 412 correct mz390 to show all errors during macro loading.

47. RPI 414 correct mz390 to correctly support ERR(nnn) error limit 
    override with 0 indicating no limit.

48. RPI 415 merge mz390 and az390 to run together as parallel threads
    with provider/consumer locking to pass BAL without requiring BAL
    file.  New default ASM replaces MFC and new default NOBAL eliminates
    need for BAL file output.  Option BAL will create it.  az390 can 
    still be run using BAL file output to skip repeated macro expansions
    if desired.  Option NOASM turns off az390 and surpresses support of
    ordinary symbols in mz390.  The new ASM option fully supports 
    ordinary symbol references in AIF expressions and does lookahead 
    function automatically during mlc loading.  bal and prn file 
    statistics now include total errors from each other.  See ERR
    file for summary of mz390 and az390 errors in sequence.  Note
    mz390 tracem and tracea messages may not appear in sequence due
    to thread time slicing.  See TESTSYM1 for attribute and symbol
    reference tests.

49. RPI 416 correct az390 loop on invalide DS/DC length calculation.

50. RPI 419 correct mz390 T', L', and D' operator parsing of SETC
    type parm to extract ordinary symbol with labelled using prebix
    and expression operator following name.

51. RPI 420 add support for up to 10 concurrent AREAD files and 10 
    concurrent PUNCH output files identified by new ID=n parm where
    n can be 0-9 with 0 being the default.  See TESTPCH3 example of
    text file merging and coping in single mz390 execution with NOASM.

52. RPI 422 correct mz390 to correctly compare padded setc strings of
    equal length but unequal value.  The string compare was failing
    for EBCDIC compares when character < 128 was compared to character 
    >= 128 due to not ANDing 0xff with byte value for int compare.  
    Yuck!

53. RPI 423 correct mz390, az390, ez390, sz390, and gz390 to 
    issue error and abort when thread traps by adding separate
    exception handler for each thread.

54. RPI 424 correct az390 to correctly handle invalid D, E, F, H, and L
    type decimal constants.

55. RPI 425 add file and line cross reference to all az390 error 
    messages when running with option ASM.  Generate mz390 
    statistics before END statement only if BAL specified.  Force 
    listing of file cross reference at end of mz390 or az390 for all 
    files with errors and also list on ERR file for easy reference.
    Note the cross reference info is not available when az390 is run
    separately using just BAL input file.

56. RPI 426 speed up assembly by skipping multiple passes to try and 
    resolve forward refernces when there are hard mz390 phase errors.

57. RPI 427 issue error when ordinary symbol reference is made when
    running mz390 with option NOASM.

58. RPI 428 ignore high bit indicating address mode when searching for
    address break in TEST mode.

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
TESTOPR1.MLC - test new operator functions added such as A2D, SLA, etc.
TESTINS2.MLC - test D, DR and optimize SLDL etc.
TESTSYN2.MLC - test OPSYN restore
TESTSET2.MLC - test array expansion with multiple values
TESTMAC1.MLC - test string compare
TESTSYM1.MLC - test ordinary symbol attribute and length
TESTPCH3.MLC - test merge and copy of multiple text files in single pgm

6.  Copy updated commands to the z390 root directory

ASM.BAT   - allow up to rc=4, new ASM, NOBAL options to skip az390
ASML.BAT  - allow up to rc=4, new ASM, NOBAL options to skip az390
ASMLG.BAT - allow up to rc=4, new ASM, NOBAL options to skip az390
EXEC.BAT  - allow up to rc=4
LINK.BAT  - allow up to rc=4
RT7.BAT - assemble, link, and execute and compare with no
          return code limits.
RTLINK.BAT - assemble, link, and test all LINKLIB programs

Visit www.z390.org for more information on latest updates.
