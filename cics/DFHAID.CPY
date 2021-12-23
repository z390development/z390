.**********************************************************************
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
.* Author - Don Higgins                                               *
.**********************************************************************
*
* AID EQUATES
*
DFHNULL  EQU   X'00'       NULL
DFHENTER EQU   X'7D'       ENTER KEY
DFHPF1   EQU   X'F1'       PROGRAM FUNCTION KEY 1
DFHPF2   EQU   X'F2'       PROGRAM FUNCTION KEY 2
DFHPF3   EQU   X'F3'       PROGRAM FUNCTION KEY 3
DFHPF4   EQU   X'F4'       PROGRAM FUNCTION KEY 4
DFHPF5   EQU   X'F5'       PROGRAM FUNCTION KEY 5
DFHPF6   EQU   X'F6'       PROGRAM FUNCTION KEY 6
DFHPF7   EQU   X'F7'       PROGRAM FUNCTION KEY 7
DFHPF8   EQU   X'F8'       PROGRAM FUNCTION KEY 8
DFHPF9   EQU   X'F9'       PROGRAM FUNCTION KEY 9
DFHPF10  EQU   X'7A'       PROGRAM FUNCTION KEY 10
DFHPF11  EQU   X'7B'       PROGRAM FUNCTION KEY 11
DFHPF12  EQU   X'7C'       PROGRAM FUNCTION KEY 12
DFHPF13  EQU   X'C1'       PROGRAM FUNCTION KEY 13 (CTRL+ALT+F1)
DFHPF14  EQU   X'C2'       PROGRAM FUNCTION KEY 14 (CTRL+ALT+F2)
DFHPF15  EQU   X'C3'       PROGRAM FUNCTION KEY 15 (CTRL+ALT+F3)
DFHPF16  EQU   X'C4'       PROGRAM FUNCTION KEY 16 (CTRL+ALT+F4)
DFHPF17  EQU   X'C5'       PROGRAM FUNCTION KEY 17 (CTRL+ALT+F5)
DFHPF18  EQU   X'C6'       PROGRAM FUNCTION KEY 18 (CTRL+ALT+F6)
DFHPF19  EQU   X'C7'       PROGRAM FUNCTION KEY 19 (CTRL+ALT+F7)
DFHPF20  EQU   X'C8'       PROGRAM FUNCTION KEY 20 (CTRL+ALT+F8)
DFHPF21  EQU   X'C9'       PROGRAM FUNCTION KEY 21 (CTRL+ALT+F9)
DFHPF22  EQU   X'4A'       PROGRAM FUNCTION KEY 22 (CTRL+ALT+F10)
DFHPF23  EQU   X'4B'       PROGRAM FUNCTION KEY 23 (CTRL+ALT+F11)
DFHPF24  EQU   X'4C'       PROGRAM FUNCTION KEY 24 (CTRL+ALT+F12)
DFHPA1   EQU   X'6C'       PROGRAM ATTENTION 1     (CTRL+F1)
DFHPA2   EQU   X'6E'       PROGRAM ATTENTION 2     (CTRL+F2)
DFHPA3   EQU   X'6B'       PROGRAM ATTENTION 3     (CTRL+F3)
DFHCLEAR EQU   X'6D'       CLEAR SCREEN            (CTRL+C)
