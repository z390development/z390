readme-2.txt for Linux and Mac users of z390 as of 2022-09-29 Don Higgins don@higgins.net

readme-2.txt replaces readme.txt.

The bash directory consists of bash scripts that Linux and Mac users of z390 can
use to perform the same z390 functions provided by the bat files used by Windows
users.

See below for assumptions, restrictions, and known issues.

 As is the bat directory, the bash directory must be a subdirectory of the
z390 directory.

The bash directory contains 33 files:

asm     bldcbllib   cblc    ez390  mz390        runcbldemos     z390
asml    blddist     cblcl   help   readme.txt   runcmdproc      zc390
asmlg   bldjar      cblclg  link   relver       runvsedemos     zpartrs
assist  bldlib      debug   lz390  runasmdemos  runzstrmactest
az390   bldzstrmac  exec    mac    runassist    test.sh

Except for the two files readme.txt and blddist, the remaining 31 files are
equilvaent to the corresponding 31 files in the bat subdirectory. For
example, asm is equivalent to ASM.BAT.

The readme.txt file is the file you are now reading.

The blddist bash script is used to build the z390 distribution on github.

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

--------------------------------------------------------------------------------

For help with Linux and Mac use of these scripts with z390, either post a question
to the z390 group at https://groups.google.com/g/z390 or send an email to

  John Ganci  jyganci@gmail.com

================================================================================
End readme.txt
================================================================================
