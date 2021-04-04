readme.txt for Linux and Mac users of z390 as of 2021-04-01 Don Higgins don@higgins.net

The bash directory consists of bash scripts that Linux and Mac users of z390 can
use to perform the same z390 functions provided by the bat files used by Windows
users.

See below for assumptions, restrictions, and known issues.

 As is the bat directory, the bash directory must be a subdirectory of the
z390 directory.

The bash directory contains 30 files:

  asmlg.sh      bldjar.sh   ez390.sh    runasmdemos.sh  runzpar.sh
  asml.sh       cblclg.sh   link.sh     runasmtests.sh  test.sh
  asm.sh        cblcl.sh    lz390.sh    runcbldemos.sh  z390.sh
  assist.sh     cblc.sh     mac.sh      runcbltests.sh  zc390.sh
  az390.sh      CBLOPT.OPT  mz390.sh    runrtqsam.sh    zopcheck.sh
  bldcbllib.sh  exec.sh     readme.txt  runsort.sh      zpartrs.sh

Except for two files readme.txt and z390.sh, the remaining 28 files are
replacements for the 28 files in the bat subdirectory. For example, asm.sh
replaces ASM.BAT.

The readme.txt file is the file you are now reading.

The z390.sh bash script is used to start the z390 GUI.

Try running the test.sh script.

  $ cd <full path to z390>/bash
  $ ./test.sh

If you get "Permission denied", you probably need to make the bash scripts executable:

  $ cd <full path to z390>/bash
  $ chmod 755 *.sh

Running test.sh should now be successful.

Assumptions, restrictions, and known issues.
--------------------------------------------

1. As is the bat directory, the bash directory must be a subdirectory of
   the z390 directory.
2. The bash scripts properly extract the "tron" parameter, if present, but
   do not use it.
3. Scripts runzpar.sh and zpartrs.sh do not yet work. Research in progress.
4. The CBLOPT.OPT file should probably be moved elsewhere. It is here because
   it is in the bat directory, where it really doesn't belong either. If/when this
   is done, the COBOL scripts clbc.sh, cblcl.sh, and cblclg.sh will require change.
5. Depending on when you begin using these scripts, you may encounter a couple of
   errors when running the demo and test scripts for assembler and COBOL due to
   upper/lower case issues.
   1. Rename z390/tests/TESTINS5.mlc to z390/tests/TESTINS5.MLC.
   2. z390/zcobol/demo/COPYFILE.CBL has two instances of ZCOBOL/DEMO that must
      be changed to zcobol/demo. Changed lines shown in context:

       FILE-CONTROL.
           SELECT IN-FILE
               ASSIGN TO 'zcobol\demo\COPYFILE.IN'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUT-FILE
               ASSIGN TO 'zcobol\demo\COPYFILE.OUT'
               ORGANIZATION IS LINE SEQUENTIAL.

    These two problems have been fixed and should be in z390 V17_07 and later.

--------------------------------------------------------------------------------

For help with Linux and Mac use of these scripts with z390, either post a question
to the z390 group at https://groups.google.com/g/z390 or send an email to

  John Ganci  jyganci@gmail.com

================================================================================
End readme.txt
================================================================================
