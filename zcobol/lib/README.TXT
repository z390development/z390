.*********************************************************************
.* Copyright 2009 Automated Software Tools Corporation               *
.* This source code is part of z390 zCOBOL/assembler/emulator package*
.* The z390 package is distributed under GNU general public license  *
.* Author - Don Higgins                                              *
.* Date   - 02/20/09                                                 *
.*********************************************************************

zcobol\z390 contains macros and commands for zcobol generation of
IBM HLASM compatible assembler source which can then be assembled,
linked, and executed on z9/z10 mainframes or the generated source can
can be assembled, linked, and executed using z390 on any J2SE platform
including Windows and Linux. 

This support requires installation of the following free downloads:

1.  z390 1.5.00+
2.  J2SE 1,6,00+

The following command will generate, assemble, link, and exec the COBOL demo zcobol\demo\HELLO.CBL:

ZC586CLG zcobol\HELLO

The generation steps are as follows:

1.  zc390   reads zcobol\demo\hello.cbl and generates 
                  zcobol\demo\i586\hello.mlc z390 meta language
2.  mz390   reads zcobol\demo\i586\hello.mlc and generates
                  zcobol\demo\i586\hello.bal
3.  az390   reads zcobol\demo\i586\hello.bal and generates
                  zcobol\demo\i586\hello.obj and prn listing
4.  lz390   reads zcobol\demo\i586\hello.obj and generates
                  zcobol\demo\i586\hello.390 load module
5.  ez390   reads zcobol\demo\i586\hello.390 and generates
                  zcobol\demo\i586\hello.log and console WTO display

For more informtion visit:

www.zcobol.org
www.z390.org

Don Higgins
don@higgins.net
