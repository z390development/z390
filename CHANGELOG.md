Github z390 Branch V1707 has been created from the z390_v1707.zip distribution
described below with the following additiona changes:
1. z90.bat has been updated to use the start z390.jar command to release command line.
2. The following additional directories have been retained from the Github prior
   repository version v1706:
   a. mfacc - sample assembler programs for student exercises
   b. mvs - MVS 3.8 public domain macros
4. Note this V1707 repository does not have the generated file z390.jar
   in the root z390 directory so the command bat\BLDJAR.BAT will need to be
   executed to create it, and it will require Java 8.0 JDK.
   A future distribution version may create it and run regression tests. 

z390_v1707_readme.txt updated 2021-05-02 by don@higgins.net

This zip version has the following fixes and changes:

1.  Issue #239 correct assembler to issue error message 
    for undefined operand for IIHF instruction.
2.  Issue #2 move zcobol opt directory and file under zcobol and
    test updated ASM and CBL bat files on Windows.
3.  Issue #234 add the following directories which were included
    in the 1.5.06 version of z390: assist, cics, linklib, guam, soa, vsam, vse, and rt. 
    1.  assist - run bat\runassist.bat to run demo and test programs using ASSIST.
    2.  cics - added 6 new subdirectories - 390, bat, cbl, cpy, mac, mlc.  
               opened issue #245 to update bat commands to use new subdirectories.
    3.  guam - added graphical user access method with demo and test subdirectories
    4.  linklib - added utility library with 21 mlc programs.  See issue #246 for updates.
    5.  soa - added service oriented architecture (SOA) with demo, maclib, test, and readme.
        Issue #247 has been opened to add bat commands to build components.
    6.  vsam1 = See issue #253 for error trying to run vsam1\demo\demos.bat with zvsam(1)
    7.  vsam2 - added 2 subdirectories bat and mlc and successfully assembled and linked
        6 executable programs using the zvsam(2) option in the mlc directory.  Also the
        mac directory was copied to macold and the new hybrid vsam1 and vsam2 mac
        library replaced the old mac library.  
    8.  vse - run bat\runvsedenis,bat to see 2 demo programs taht currently fail
        due to changes in low storage mapping for zcvt and comreg.  See issue #244.
    9.  rt - added with readme and regression test bat subdirectory with program for 
        issue #238 from John G. Note bat commands use cd.. cd.. to get to root.
        There is much more work to do with rt to add automated rt to repository.
        Note old version zc390lib.390 had to be deleted from this directory
        to prevent failure of zcobol which now uses zcobol\lib\zc390lib.390 which
        is built by bat\BLDCBLLIB.BAT whenever version changes in tz390.java.
4.  Added z390.bat to bat directory to invoke z390 GUI by executing z390.jar.  
    This is alternative to double clicking on z390.jar under Windows.
