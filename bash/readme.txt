readme.txt for Linux and Mac users of z390 as of 2021-04-05 Don Higgins don@higgins.net

The bash directory consists of bash scripts that Linux and Mac users of z390 can
use to perform the same z390 functions provided by the bat files used by Windows
users.

See below for assumptions, restrictions, and known issues.

 As is the bat directory, the bash directory must be a subdirectory of the
z390 directory.

The bash directory contains 29 files:

asm     az390      cblcl   link   readme.txt   runcbltests  test.sh   zpartrs
asml    bldcbllib  cblclg  lz390  runasmdemos  runrtqsam    z390
asmlg   bldjar     exec    mac    runasmtests  runsort      zc390
assist  cblc       ez390   mz390  runcbldemos  runzpar      zopcheck


Except for the two files readme.txt and z390, the remaining 27 files are
replacements for the 28 files in the bat subdirectory. For example, asm
replaces ASM.BAT.

The readme.txt file is the file you are now reading.

The z390 bash script is used to start the z390 GUI.

Note: the test.sh script is used to test bash functionality. It is not used by z390.

Try running the test.sh script.

  $ cd <full path to z390>/bash
  $ ./test.sh

If you get "Permission denied", you probably need to make the bash scripts executable:

  $ cd <full path to z390>/bash
  $ chmod 755 *
  $ chmod 644 readme.txt

The second "chmod" resets the readme.txt permissions back to what they were before the
first "chmod" was done.

Running test.sh should now be successful.

Assumptions, restrictions, and known issues.
--------------------------------------------

1. As is the bat directory, the bash directory must be a subdirectory of
   the z390 directory.
2. The bash scripts properly extract the "tron" parameter, if present, but
   do not use it.
3. Scripts runzpar and zpartrs do not yet work. Research in progress.
4. Depending on when you begin using these scripts, you may encounter a couple of
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
