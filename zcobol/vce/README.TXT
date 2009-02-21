.*********************************************************************
.* Copyright 2009 Automated Software Tools Corporation               *
.* This source code is part of z390 zCOBOL/assembler/emulator package*
.* The z390 package is distributed under GNU general public license  *
.* Author - Don Higgins                                              *
.* Date   - 02/20/09                                                 *
.*********************************************************************

zcobol\vce contains macros and commands for zcobol generation of
Microsoft Visual C++ 9.0 CPP source programs which can be compiled
and linked in EXE format for execution on Windows Vista/XP.

This support requires installation of the following free downloads:

1.  z390
2.  MS Visual Express 9.0
5.  Windows SDK with kernel32.lib

The following command will generate, assemble, link, and exec the COBOL demo zcobol\demo\HELLO.CBL:

ZCVCECLG zcobol\HELLO

The generation steps are as follows:

1.  zc390   reads zcobol\demo\hello.cbl and generates 
                  zcobol\demo\vce\hello.mlc macro meta code
2.  mz390   reads zcobol\demo\vce\hello.mlc and generates 
                  zcobol\demo\vce\hello.cpp MS Visual Express C++
3.  cl.exe  reads zcobol\demo\vce\hello.cpp and generates 
                  zcobol\demo\vce\hello.obj and hello.exe
4.  hello.exe exec

For more information visit:

www.zcobol.org
www.z390.org

Don Higgins
don@higgins.net
