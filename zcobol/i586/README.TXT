.*********************************************************************
.* Copyright 2009 Automated Software Tools Corporation               *
.* This source code is part of z390 zCOBOL/assembler/emulator package*
.* The z390 package is distributed under GNU general public license  *
.* Author - Don Higgins                                              *
.* Date   - 02/20/09                                                 *
.*********************************************************************

zcobol\i586 contains macros and commands for zcobol generation of
Intel 586+ assembler which is then assembled and linked using 
High Level Assembler for Intel on Windows or Linux:

http://webster.cs.ucr.edu/AsmTools/HLA/dnld.html

This support requires installation of the following free downloads:

1.  z390
2.  J2SE
3.  HLA
4.  MS Visual Express 2005
5.  MS MASM (installs in Vistual Express 2005 directory

The following command will generate, assemble, link, and exec the COBOL demo zcobol\demo\HELLO.CBL:

ZC586CLG zcobol\HELLO

The generation steps are as follows:

1.  zc390   reads zcobol\demo\hello.cbl and generates 
                  zcobol\demo\i586\hello.mlc z390 meta language
2.  mz390   reads zcobol\demo\i586\hello.mlc and generates
                  zcobol\demo\i586\hello.hla
    using macros in zcobol and zcobol\i586
3.  hla.exe reads zcobol\demo\i586\hello.hla and generates 
                  zcobol\demo\i586\hello.asm
    and then calls masm ml.exe to assemble and link
                   zcobol\demo\i586\hello.asm into
                   zcobol\demo\i586\hello.obj and exe 
4,  hello.exe exec

For more informtion visit:

www.zcobol.org
www.z390.org

Don Higgins
don@higgins.net
