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
