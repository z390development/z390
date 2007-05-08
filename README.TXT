z390 v1.3.03b PTF 05/08/07
Copyright 2007 Automated Software Tools Corporation

The enclosed z390.jar contains following cumulative fixes for v1.3.03:

1.  RPI 598 - Correct XCTL svc to save R1 (bug in RPI 596) and correct
    memory corruption after multiple XCTL during same execution.  See
    updated TESTXCL1-4 regression test.

2.  RPI 599 - Add DCBD macro call from DCB to define fields.  DCBD will only
    generated DSECT once.

3.  RPI 600 - Correct mz390 to assume undefined variables are local unless they start with     &SYS.  Also issue error 211 for duplicate keyword macro call parameters.

4.  RPI 601 - Correct macros SAVE and RESTORE to support Rnn register references;
    change SUBENTRY to use 4096 base offsets versus 4095 using LAY instead of LA;
    and correct SUBEXIT label generation error.

5.  RPI 602 - Correct az390 to issue errors for negative DS/DC lengths.

6.  RPI 603 - Correct gz390 GUI interface to correctly support up and down
    arrow scrolling of prior commands.  (It was skipping previous command.)

7.  RPI 605 - Add additional error detection in az390 by adding location counter on the     TRACEA file; add error message for first cyclic DS/DC and EQU valaue changes; remove
    duplicate az390 error messages on the the TRACEA file; and change section error 
    messages to show hex address and lengths versus decimal.
  S
8.  RPI 606 - add MVCOS opcode per SHARE HLASM project leader documentation.  This
    instruction is faster than MVC or MVCL for moving from 257 to 4096 bytes
    using 1 instruction and only 2 registers.  See new instruction tests in  TESTINS2.

9.  RPI 609 - improve compatiblity with HLASM with following changes:
    a.  Issue error if DC has non-zero dup count and no data.
    b.  Prevent using labeled USING to resolve unqualified label address.
    c.  Issue error if comment * appears after position 1.
    d.  Issue error it SETB expression requires string to numeric conversion.
    e.  Issue error if E type instruction comments are not preceeded by comma.

10. RPI 610 - correct error in  XCTL when multiple LINK,  XCTL, and EXIT svcs are 
    executed  at different levels of nesting (Required for new CICS testing).

11. RPI 611 - correct mz390 to prevent internal trap on computed AGO with bad index.

12. RPI 612 - correct az390 to assemble RX explicit offset(reg) to use index vs base
    register (no change logically except for possible EX of RX instruction).

13. RPI 613 Correct az390 to assemble offset(len) as low storage reference using base
    register 0 which requires option NOPROTECT to prevent 0C5 at execution.

14. RPI 614 create new download single zip file pdfdocs.zip with all z390 pdf documents.

15. RPI 615 - Correct mz390 to correctly handle operand 5 ATTRA  strings including
    FPR which was not being recognized due to coding error.

16. RPI 616 replace existing cics directory content with new CICS type client server
    transaction manager contributed by Melvyn Maltz.  See documentation in the cics\docs
    directory.  After deleting  old cics directory and installing  new cics  directory from
    this PTF zip, you can perform the following:
      1.  From install directory command prompt,  start cics server cics\z390cicg.bat
      2.  From another install  directory command prompt, start cics  client cics\z390kcp.bat
          a)  Enter demo hello world transaction MMM2
          b)  Enter CTRL-C to clear screen
          c)  Enter GUI6 demo tranaction with multiple fields  and F1 help 
          d)  Enter CEMT S TER OUT to shut down  client 

To install this cumulative PTF for z390 v1.3.03 follow these steps.

1.  Delete existing cics directory with local transaction processor support.

1.  Extract the PTF zip file into the z390 install directory replacing:
    a.  README.TXT
    b.  z390.jar
    c.  cics directory with new client server support including updated open source
    d.  src  directory with latest z390 open source
    e.  updated macros and test programs
    

2.  Copy updated macro source files into z390\mac directory:

DCB.MAC - add call to DCBD to generate DSECT once
RESTORE - support Rnn register symbols
SAVE    - support Rnn register symbols
SUBENTRY - change base offsets to 4096 versus 4095
SUBEXIT  - correct label generation

3.  If you need to change Java source for this PTF version, all
    the Java source is in src directory and can be copied to 
    z390\src directory.

Visit www.z390.org for more information on latest updates.
