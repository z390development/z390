.*********************************************************************
.* Copyright 2009 Automated Software Tools Corporation               *
.* This source code is part of z390 zCOBOL/assembler/emulator package*
.* The z390 package is distributed under GNU general public license  *
.* Author - Don Higgins                                              *
.* Date   - 02/20/09                                                 *
.*********************************************************************

zcobol\java contains macros and commands for zcobol generation of
Sun J2SE java source programs which can be compiled
and executed in class format on Windows Vista/XP and Linux.

This support requires installation of the following free downloads:

1.  z390
2.  J2SE

The following command will generate, assemble, link, and exec the COBOL demo zcobol\demo\HELLO.CBL:

ZCJAVCLG zcobol\hello

The generation steps are as follows:

1.  zc390   reads zcobol\demo\hello.cbl and generates
                  zcobol\demo\java\hello.mlc
2.  mz390   reads zcobol\demo\java\hello.mlc and generates
                  zcobol\demo\java\hello.java                     
3.  javac   reads zcobol\demo\java\hello.java and generates
                  zcobol\demo\java\hello.class                     
4.  java    exec  zcobol\demo\java\hello.class              

For more information visit:

www.zcobol.org
www.z390.org

Don Higgins
don@higgins.net
