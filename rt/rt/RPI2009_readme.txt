================================================================================
Begin readme.txt
================================================================================
Initial:     2017-02-25
Last update: 2017-03-21

To:   Abe Kornelis
From: John Ganci

--------------------------------------------------------------------------------
Contents

The b11-pk01.zip file contains the following RPIs:


RPI 2009 Fix condition code 3 errors in pz390.java
           1. Add (AR, AGR, A, etc) instructions and subtract
              (SR, SGR, S, etc) instructions do not set CC=3 and do not set the
              destination value when (fixed-point) overflow occurs; also, if
              if overflow occurs and the program mask fixed-point-overflow bit
              is one, the CC in the PSW at abend is not set
           2. LCR, LCGR, LPR, and LPGR do not set the destination
              register when CC=3; also, none of these check for
              fixed-point-overflow exceptions
           3. SLA, SLAK, SLDA, and SLAG do not check for
              fixed-point-overflow exceptions
         Remove pz390.java invalid call to get_int_add_cc() in ALSIHN instruction emulation

******************************************************************************************
* Begin information taken from doc subdirectory
******************************************************************************************

The informtion here is a summary of documentation that is in the supplied doc subdirectory.
It is here for reference only. You may read it now, or skip ahead to item 1 below.

The instructions that required attention was determined by finding all instructions
that can generate a fixed-point-overflow exception. The following list was created
using Appendix B Figure B-1 in z/Architecture Principles of Operation (SA22-7832-10).
It lists all instructions that can generate fixed-point-overflow exceptions.

The additional columns document calls by the pz390 instruction emulation to certain
pz390 methods. They were added later, after examining the pz390 source. The examination
of the pz390 source found that ALSIHN also called get_int_add_cc(). That is why it was
added to RPI 2009.

  Column labels indicate which pz390 method is called to get CC:
  ia=get_int_add_cc(), is= get_int_sub_cc()
  la=get_long_add_cc(), ls=get_long_sub_cc()

  Mnemonic  Format   ia  is  la  ls
  --------  ------   --  --  --  --
  A         RX-a      x
  AR        RR        x
  ARK       RRF-a     x
  AY        RXY-a     x
  AG        RXY-a             x
  AGR       RRE               x
  AGRK      RRF-a             x
  AGF       RXY-a             x
  AGFR      RRE               x
  AH        RX-a      x
  AHY       RXY-a     x
  AHI       RI-a      x
  AGHI      RI-a              x
  AHHHR     RRF-a     x
  AHHLR     RRF-a     x
  AFI       RIL-a     x
  AHIK      RIE-d     x
  ASI       SIY       x
  AGHIK     RIE-d             x
  AGFI      RIL-a             x
  AGSI      SIY               x
  AIH       RIL-a     x
  LAA       RSY-a     x
  LAAG      RSY-a             x

  LCR       RR
  LCGR      RRE
  LPR       RR
  LPGR      RRE
 
  SLDA      RS-a
  SLA       RS-a
  SLAK      RSY-a
  SLAG      RSY-a

  S         RX-a          x
  SR        RR            x
  SRK       RRF-a         x
  SY        RXY-a         x
  SG        RXY-a                 x
  SGR       RRE                   x
  SGRK      RRF-a                 x
  SGF       RXY-a                 x
  SGFR      RRE                   x
  SH        RX-a          x
  SHY       RXY-a         x
  SHHHR     RRF-a         x
  SHHLR     RRF-a         x

  ALSIHN    RIL-a    x-error


If you look at the above table, you will see that all "Add" instructions are
accounted for by the "ia" and "la" columns, and all "Subtract" instructions
are accounted for by the "is" and "ls" columns. It is in these methods that
the CC is set and checks for overflow occur.

The remaining instructions in the table - the four "Load" instructions and the
four "Shift" instructions - each have their own separate tests.

As noted above, the ALSIHN instruction was added later so that the invalid call
to get_int_add_cc() could be removed.

******************************************************************************************
* End information taken from doc subdirectory
******************************************************************************************

--------------------------------------------------------------------------------
1. Installation instructions

1.1. Unzip the b11-pk01.zip file. The resulting structure is a directory named
     b11-pk01 that contains the readme.txt file you are now reading and four
     subdirectories:

         doc
         rt
         rtxtra
         src

     ***************************************************************************
     *  WARNING: All text files have unix endline format (lines end with new
     *           line).
     *
     *           Using them on a Windows machine might cause problems.
     *           If so, use an editor like Notepad++ to edit each one
     *           and save it in Windows format.
     ***************************************************************************

1.1.1. The doc subdirectory contains documentation explaining how the tests for
       the Add and Subtract instructions came about. It also lists all of the
       instructions that are tested by RPI 2009.

       1, instructions_that_set_fixed_point_overflow.ods

          This LibreOffice Calc file contains documentation on

          a. All instructions that can generate fixed-point-overflow exceptions
          b. How the "Add" instruction tests were created
          c. How the "Subtract" instruction tests were created.

       2. instructions_that_set_fixed_point_overflow.txt

          A text file that explains the contents of the ods file in item 1.

       3, add_subtract_possibilities.txt

          A text file that contains all the possible test cases for "Add"
          and "Subtract" instructions. Builds on items 1 and 2.

          The lists are shown in the file, both in spreadsheet order and in
          CC order.

          The CC lists were used to create the Add and Subtract tests in
          RPI2009A.MLC and RPI2009B.MLC.

1.1.2. The rt subdirectory contains the regression test files and the
       corresponding assembler source files.

       RPI 2009 files
       --------------

         RPI2009A.MLC   Add instructions test assembler source
         RPI2009A.RT    Add instructions regression test script
         RPI2009B.MLC   Subtract instructions test assembler source
         RPI2009B.RT    Subtract instructions regression test script
         RPI2009C.MLC   LCR and LCGR test assembler source
         RPI2009C.RT    LCR and LCGR regression test script
         RPI2009D.MLC   LPR and LPGR test assembler source
         RPI2009D.RT    LPR and LPGR regression test script
         RPI2009E.MLC   SLA, SLAK, SLDA, and SLAG test assembler source
         RPI2009E.RT    SLA, SLAK, SLDA, and SLAG regression test script
         RPI2009F.MLC   ALSIHN test assembler source
         RPI2009F.RT    ALSIHN regression test script

1.1.3. The rtxtra subdirectory contains regression test files, one file for
       each of the Add instructions tested by RPI2009A.MLC and one file for
       each of the Subtract instructions tested by RPI2009B.MLC.


       RPI 2009 files in the rtxtra subdirectory
       -----------------------------------------

         RPI2009A_A.RT
         RPI2009A_AR.RT
         RPI2009A_ARK.RT
         RPI2009A_AY.RT
         RPI2009A_AG.RT
         RPI2009A_AGR.RT
         RPI2009A_AGRK.RT
         RPI2009A_AGF.RT
         RPI2009A_AGFR.RT
         RPI2009A_AH.RT
         RPI2009A_AHY.RT
         RPI2009A_AHI.RT
         RPI2009A_AGHI.RT
         RPI2009A_AHHHR.RT
         RPI2009A_AHHLR.RT
         RPI2009A_AFI.RT
         RPI2009A_AHIK.RT
         RPI2009A_ASI.RT
         RPI2009A_AGHIK.RT
         RPI2009A_AGFI.RT
         RPI2009A_AGSI.RT
         RPI2009A_AIH.RT
         RPI2009A_LAA.RT
         RPI2009A_LAAG.RT

         RPI2009B_S.RT
         RPI2009B_SR.RT
         RPI2009B_SRK.RT
         RPI2009B_SY.RT
         RPI2009B_SG.RT
         RPI2009B_SGR.RT
         RPI2009B_SGRK.RT
         RPI2009B_SGF.RT
         RPI2009B_SGFR.RT
         RPI2009B_SH.RT
         RPI2009B_SHY.RT
         RPI2009B_SHHHR.RT
         RPI2009B_SHHLR.RT


       The naming convention for each file is

           RPI2009[A or B]_<mnemonic>.RT

       where <mnemonic> is the instruction tested by the corresponding script.

       Note that the RPI2009A.RT test script includes all of the RPI2009A_x.RT
       test scripts and the RPI200B.RT test script includes all of the
       RPI2009B_x.RT test scripts. You may run these scripts using the RPI2009A
       and RPI2009B programs. See items 3.1.1 and 3.1.2 below for details.

1.1.4. The src subdirectory contains the z390 java source that is updated
       by this RPI.

       The updates were applied to the original beta 11 version.

         pz390.java - updated by RPI 2009.


1.2. Back up your existing beta 11 src directory and z390.jar file.

1.3. Follow your process to incorporate the changes in the source file(s)
     into your source file(s).

1.4. Rebuild the z390.jar file.

1.5. Copy the new z390.jar file to wherever you have installed z390.

1.6. Copy the rt directory to wherever you normally keep your regression
     test scripts and source. The destination is called the "RT directory"
     in Section 2.

1.7. (Optional) Copy the rtxtra directory to wherever you keep your regression
     test scripts. See items 3.1.1 and 3.1.2 below for instructions on how to
     use them.
--------------------------------------------------------------------------------
2. Testing instructions.

Each test is done from a command prompt (terminal window). If testing is done
from the GUI, alter the following instructions accordingly.

2.1. Change directory to your RT directory. For what follows, the path to that
     directory is represented by <RT path>/ where / is the path separator for
     your operating system ("/" for Linux and MacOS, "\" for Windows).
2.2. Invoke z390.
2.3. Assemble and link each of the assembler test programs:
       asml <RT path>/RPI2009A
       asml <RT path>/RPI2009B
       asml <RT path>/RPI2009C
       asml <RT path>/RPI2009D
       asml <RT path>/RPI2009E
       asml <RT path>/RPI2009F
2.4. Exit to the command prompt.
2.5. Repeat the following steps for each RT file:
2.5.1. Set the environment variable that you use for the RT file name to
       "<RT path>/RPI<suffix>.RT" (no quotes). where <suffix> is the
       appropriate suffix for the test being performed. In the following,
       <RT env var> denotes this variable name.
2.5.2. Invoke z390.
2.5.3. exec <RT path>/RPI<suffix> noloadhigh nocodepage test(<RT env var>)
2.5.4. The test should run to completion, getting a 0 return code.
2.5.5. For some of the tests you will need to read the console output.
       See the next section. DOES NOT APPLY FOR RPI 2009.
2.5.6. Exit to the command prompt.
2.6. After running all the tests, examine the *.TRE output as needed. See
     the next section.
--------------------------------------------------------------------------------
3. Notes about testing.

Some of the tests may require that you validate an RPI by examining the console
output produced when you ran the test scripts. The console output is saved in
files named RPI<suffix>.TRE that are stored in the <RT path>/ directory, where
<RT path>/ and <suffix> are as described in section 2.

What you must examine for each RPI is described in the following subsections.

In all cases, "look at the console output" means looking at the actual console
output or looking at the TRE file that is generated by the test.

3.1. RPI 2009.

     There is no need to examine the console output produced by any of
     the regression tests.

3.1.1. RPI2009A.

       The test script for RPI2000A tests the 24 Add instructions listed above.
       There is a lot of output produced by this test. All you can do here is
       note that each test ends successfully with a return code of 0.

       One can run individual tests for each Add instruction by using the
       RPI2009A_x.RT scripts contained in the rtxtra subdirectory
       (see item 1.1.3 above) by running a command like the following one that
       tests the AHHHR instruction:

           exec <RT path>/RPI2009A noloadhigh nocodepage parm(AHHHR) test(RT)

       where RT is the "DD statement" for the RPI2009A_AHHHR.RT file.

       Item 1.1.1 above describes how the RPI2009A and RPI2009B tests were
       created.

3.1.1.1. Testing that the correct condition (CC) value is in the PSW at abend
         is done indirectly.

         Testing fixed-point-overflow exceptions with the program mask set
         to 1000 binary would normally result in abend S0C8. Rather than
         actually abend, the test program sets an ESPIE to trap
         fixed-point-overflow exceptions. The ESPIE exit saves the potential
         PSW at abend in a field accessible to the test program and then
         retries at an address specified by the test program.

         The verification of the CC in the PSW at abend is done by verifying
         that the PSW passed to the ESPIE exit contains the correct CC value.

3.1.2. RPI2009B.

       The test script for RPI2000B tests the 13 Subtract instructions listed
       above. There is a lot of output produced by this test. All you can do
       here is note that each test ends successfully with a return code of 0.

       One can run individual tests for each Subtract instruction by using the
       RPI2009B_x.RT scripts contained in the rtxtra subdirectory
       (see item 1.1.3 above) by running a command like the following one that
       tests the SHY instruction:

           exec <RT path>/RPI2009B noloadhigh nocodepage parm(SHY) test(RT)

       where RT is the "DD statement" for the RPI2009B_SHY.RT file.

       Item 1.1.1 above describes how the RPI2009A and RPI2009B tests were
       created.

3.1.2.1. Testing that the correct condition (CC) value is in the PSW at abend
         is done indirectly. See 3.1.1.1 for details.

3.1.3. RPI2009C.

       The test script for RPI2000C tests the LCR and LCGR instructions. All
       you can do here is note that each test ends successfully with a return
       code of 0.

3.1.4. RPI 2009D.

       The test script for RPI2000D tests the LPR and LPGR instructions. All
       you can do here is note that each test ends successfully with a return
       code of 0.

3.1.5. RPI2009E.

       The test script for RPI2000E tests the SLA, SLAK, SLDA, and SLAG
       instructions. All you can do here is note that each test ends
       successfully with a return code of 0.

3.1.6. RPI2009F.

       The test script for RPI2000F tests the ALSIHN instruction. (While not
       required, it also tests the ALSIH instruction.) All you can do here is
       note that each test ends successfully with a return code of 0.
--------------------------------------------------------------------------------
4. Comments about the RT files and the MLC files.

4.1. The RT files are modeled after samples provided by you, making some
     assumptions about your notation. The RT files here have lines that
     start with "* ###### ..." to block off portions of the script and to help
     find them when viewing the TRE output. The "(@999)" values are statement
     numbers in the corresponding PRN file.

4.2. The MLC files are also modeled after samples provided by you.
     Some particulars about them follow.

     General comments about all of them
     ----------------------------------

     When you look at the source you will see that their structure is based
     on your source. The entry and exit code is slightly different. Setting
     register 12, the base register, also sets the high half. In the entry
     code, the original high half of GG12 (the 64-bit R12) is saved in the
     work area, then the high half is set to zero; the high half of GG12 is
     restored in the exit code. This is done to avoid unexpected behavior if
     the amode is set to 64 (a problem I encountered when working at Texas
     Instruments). For example, a "B     label" would attempt to branch above
     the bar if the high half of GG12 were not zero. This problem won't occur
     in z390 right now, but it will when z390 properly computes addresses.
--------------------------------------------------------------------------------
5. Cleanup.

When testing is complete, restore your src directory and the original z390.jar
file. Depending on how you do things, you might also clean up your RT directory.
--------------------------------------------------------------------------------
================================================================================
End readme.txt
================================================================================
