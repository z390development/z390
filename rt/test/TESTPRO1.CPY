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
.*********************************************************************
.* 04/09/06 RPI 276 MZ390 OPTION PROFILE(TESTPRO1.CPY) TEST               
.*********************************************************************
.* USE OPTION PROFILE(TESTPRO1) TO PERFORM THE FOLLOWING FUNCTIONS:
.*   1.  EXECUTE MACRO TESTPROS.MAC AT MZ390 STARTUP
.*   2.  EXECUTE MACRO TESTPROT.MAC AT MZ390 TERMINATION 
.*********************************************************************
         TESTPROS
         MACRO
         END
         TESTPROT
         MEND
