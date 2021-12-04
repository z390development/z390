# Contributing to z390

The following document will provide details on how to contribute to the z390
project. For how to contribute to the documentation, see 
[contributing to documentation](/contributing/contribute_docs)

## Technical details

### Compatibility macros

z390 includes a number of macros that are interface compatible with those 
provided by mainframe operating systems. 

The internal operation of the macros is different from a real mainframe.
For example, the SVC instructions used by z390 are not the same as SVC
calls used by z/OS.

This means, if you want to write programs that will work on a real mainframe
then you need to use the supplied macros to perform the OS based actions. 
You cannot write programs that use SVC commands as they are not the 
same.

### Structured macro extensions

z390 extends the standard HLASM syntax with additional keywords that allow you 
to write structured macro code.

See [Structured Macro Extensions](/user_guide/structured_macro) for more details.

This makes writing macros easier but structured macros are NOT compatible 
with HLASM.

Structured macro extensions are used extensively in the codebase which means
moving between the mainframe and z390 has some challenges.

The project is looking at how to make this transition easier as it understands
z390 users come here because they want to write and run HLASM programs and 
macros.

## Proposing new functionality

Enhancements are welcome, but be aware that you are stepping into 
an existing and well established project. 

Before you spend time on an enhancement, we __strongly__ suggest
that you first discuss your proposal with the core team and get their 
buy-in before progressing.

The best place to have these discussions is on the z390 developer Google group.

[z390development](https://groups.google.com/g/z390development)

Once you get approval from the group, you can work on your change via the 
standard git/GitHub pull request model.

## Submitting changes

Changes can be submitted to the project by creating a pull request on the 
[z390 project repository](https://github.com/z390development/z390).

### License

By contributing to the z390 project, you agree to assign all copyright
to Don Higgins, the original author. 

This allows the project to operate and change without the consultation 
of all copyright holders. This has not presented itself as an issue to
date but could be an issue in the future.

If you don't agree to this condition, you are free to fork the project
and create your own version under the GNU 2 license conditions.

The following preamble should be applied to all programs

    z390 - Mainframe assembler emulator and run-time engine
    Copyright (C) 2021 Don Higgins

    This file is part of z390.
    z390 is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    z390 is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License along
    with this program; if not, see <https://www.gnu.org/licenses/>.

#### ASM macro template

```hlasm
         MACRO
.**********************************************************************
.* z390 - Mainframe assembler emulator and run-time engine
.* Copyright (C) 2021 Don Higgins
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
.**********************************************************************
.* dd/mm/yy change details
.*          that go over a line
.* 04/19/08 RPI 833 add SETC quotes for HLASM compatibility
.**********************************************************************
.* Macro documentation goes here
.**********************************************************************

```