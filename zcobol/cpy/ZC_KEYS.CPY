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
.* This source code is part of z390 zCOBOL/assembler/emulator package*
.* Author - Don Higgins                                              *
.* Date   - 08/15/09                                                 *
.*********************************************************************
.* 08/15/09 RPI 106 global keywords to identifed as not vars or lits        
.*********************************************************************
         GBLB &ZCKW_EQUAL
         GBLB &ZCKW_GREATER
         GBLB &ZCKW_LESS
         GBLB &ZCKW_THAN
         GBLB &ZCKW_TO
&ZCKW_EQUAL   SETB 1
&ZCKW_GREATER SETB 1
&ZCKW_LESS    SETB 1
&ZCKW_THAN    SETB 1
&ZCKW_TO      SETB 1
