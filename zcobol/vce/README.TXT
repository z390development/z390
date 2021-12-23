.*********************************************************************
.* z390 - Mainframe assembler emulator and run-time engine
.* Copyright (C) 2021 z390 Assembler LLC
.*
.* This file is part of z390.
.*
.* z390 is free software; you can redistribute it and/or modify
.* it under the terms of the GNU General Public License as published by
.* the Free Software Foundation; either version 2 of the License, or
.* (at your option) any later version.
.* z390 is distributed in the hope that it will be useful,
.* but WITHOUT ANY WARRANTY; without even the implied warranty of
.* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
.* GNU General Public License for more details.
.*
.* You should have received a copy of the GNU General Public License 
.* along with this program; if not, see https://www.gnu.org/licenses.
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
