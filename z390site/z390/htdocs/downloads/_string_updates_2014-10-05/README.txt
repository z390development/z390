Updates to Gilbert Saint-Flour's STRING Macro
=============================================

This archive provides updates to the STRING macros found in CBT file 183
(STRING version 518 requiring Assembler H or High Level Assembler) and CBT
file 749 (STRINGXF version 101, a backport of STRING version 514 for use with
Assembler F under MVS 3.8j):

STRING   518 - Circumvention of not PoP compliant MVZ instruction emulation
               in z390.

STRINGXF 101 - Backport of (%TIME,LENGTH) syntax support from STRING version 518
             - Date format DD.MM.YYYY support
             - Circumvention of not PoP compliant MVZ instruction emulation
               in z390.

Note: STRING as well as STRINGXF generally work without any changes on z390.
----  However, due to a non PoP compliant implementation of the MVZ instruction,
      all date conversions (i.e. STRING (...,P,YYYY-MM-DD) and the like) yield
      invalid results. The circumvention implemented through the above updates 
      is meant as a temporary solution until the z390 developers fix the MVZ
      instruction.

      There is no need to apply the MVZ update on systems with PoP compliant MVZ
      instructions, like IBM z/Architecture mainframes or Hercules. On the other
      hand the update doesn't hurt on those systems: It simply splits up an
      MVZ instruction in the date conversion section of the macro into two MVZ
      instructions, i.e. the resulting code has one additional MVZ instruction,
      which is a negligible overhead.


Installation and Use
====================

Use TSO/E RECEIVE or RECV370 to receive string.xmi or stringxf.xmi to a PDS of
your choice. The PDS will have the following members:

STR$CLN  - STRING macro without preceeding and trailing test program and
           documentation, i.e. just the clean macro.
STR$GSF  - STRING macro with preceeding and trailing test program and
           documentation as Gilbert Saint-Flour used to distribute it.
STRING   - Alias of STR$GSF
UPDTEnnn - IEBUPDTE control statements used to create STR$GSF from STRING
           version nnn

MVS style systems (MVS 3.8j to current z/OS):
---------------------------------------------

To use the updated macro for assembly put the PDS into your SYSLIB concatenation
before the library containing your current STRING version. Should your assembler
need a "clean" macro definition, change the STRING alias to point to STR$CLN
instead of STR$GSF.

z390:
-----

Copy STR$GSF or STR$CLN to any folder and rename it to string.mac. Add this
folder to your SYSMAC concatenation before any other folders containing a
STRING macro. Folder z390 of this archive is a ready to run setup of Gilbert's
STRING test program. It consists of the following folder:
\
  string                - STR$GSF as updated from STRING version 518 and
                          renamed to string.mac
  string_test_program   - ready to run test program for STRING
  stringxf              - STR$GSF as updated from STRINGXF version 101 and
                          renamed to string.mac
  stringxf_test_program - ready to run test program for STRINGXF

  The ..._test program folders each contain the following files:
  \
    output.txt          - Output of test program run under z390
    output_hercules.pdf - Output of equivalent test program run under Hercules
                          for comparison purposes
    strngtst.390        - test program module (ASMCL of strngtst.mlc)
    strngtst.bat        - command script to run strngtst.390 (edit this script
                          to match the path names of your Java and z390
                          installations as necessary)
    strngtst.LOG        - EZ390 execution log of test program run
    strngtst.mlc        - test program assembler source
    testipl1.390        - IPL program module (ASMCL of testipl.mlc)
    testipl1.mlc        - IPL program assembler source

To run the test program simply run strngtst.bat from a command window or click
it from a Windows Explorer window.

Note: The output of the test programs is neither "nice" nor relevant. Gilbert
----  Saint-Flour created those test programs for quick and easy functional
      verification only. His famous SYSDEBUG program has some 200 STRING macro
      calls which can serve as examples for sophisticated STRING usage. SYSDEBUG
      can also be found in CBT file 183, but, of course, using it in z390
      doesn't make sense.

________________________________________________________________________________
Juergen Winkelmann, ETH Zuerich, October 2014
winkelmann@id.ethz.ch

Credits: Gilbert Saint-Flour - author of STRING macros on CBT files 183 and 749
________________________________________________________________________________
