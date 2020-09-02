z390_v1700 readme.txt updated 2020-09-01

This directory contains everything you need to install and run z390 Portable Mainframe Assembler and Emulator.

This directory can be downloaded from http://www.z390.info

The changes in this release include the following:
  1.  z390 now supports either Oracle SE 8 or Open JDK 11.08 free open source java.
  2.  az390 has been update to assemble all the opcodes in POP SA22-7832-12
  3.  Regression test TESTINS6.MLC tests assembly of 2118 opcode and operand combinations 
  4.  New problem state opcode emulation tests in TESTINS5.MLC include:
      a. NCRK, NCGRK, MVCRL, NNRK, NNGRK
      b. NXRK, NXGRK, NORK, NOGRK, OCRK, OCGRK
      c. SELR, SELGR, SELFHR, POPCNT mask bit 8 to return total one bits in r1
      d. AGH, SGH, MGH, LLZRGF, LZRF, BIC, LLGFSG, LGG, MSGC, MG, LAT, LGAT
      e. LLGTAT, LGHAT, LLGFAT, LFHAT
  5.  Option PCOPT has been turned off until fix has been tested.

This package requires one of the following free open source Java runtimes:
  1.  Oracle SE 8 https://www.oracle.com/java/technologies/javase-jre8-downloads.html 
  2.  Open JDK 11.0.08 https://adoptopenjdk.net/

Unzip this directory and copy it to  any drive you like.

Double click on bat\bldjar80261.bat or bat\bldjar1108.bat to rebuild z390.jar from sources
in src directory using installed Oracle or Open JDK SDK.  
The current default z390.jar is for Oracle SE 8.0.261. 

Directories jar80261 and jar1108 contain previous successful build for each runtime for reference

Double click bat\runtests.bat to regression test this z390 version with installed runtime

Double click bat\rundemos.bat to assemble, link, and execute Hello World demo
plus 8 queens macro demo, and TESTDCB1 to copy file using QSAM.

You can also run the demo from the z390 GUI command line which you can start by
double clicking on z390.jar and then entering the command:
  bat\asmlg demo\demo.mlc sysmac(mac)

See more documentation on www.z390.info and history up to 2012 on www.z390.org

Join monthly zoom share sessions via www.ZoomToSHARE.org

Future planned releases and pending priority fixes will be posted on www.z390.info

Don Higgins
don@higgins.net