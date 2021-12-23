***********************************************************************
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
* Author - Don Higgins                                                *
***********************************************************************
*
* Standard BMS equates
*
* Attributes
DFHBMUNP EQU   X'40'              UNPROT  ALPHA
DFHBMFSE EQU   X'C1'              UNPROT  ALPHA            MDT
DFHBMBRY EQU   X'C8'              UNPROT  ALPHA  BRT
DFHUNIMD EQU   X'C9'              UNPROT  ALPHA  BRT       MDT
DFHBMDAR EQU   X'4C'              UNPROT  ALPHA       DRK
DFHUNNOD EQU   X'4D'              UNPROT  ALPHA       DRK  MDT
DFHBMUNN EQU   X'50'              UNPROT  NUM
DFHUNNUM EQU   X'D1'              UNPROT  NUM              MDT
DFHUNNUB EQU   X'D8'              UNPROT  NUM    BRT
DFHUNINT EQU   X'D9'              UNPROT  NUM    BRT       MDT
*        EQU   X'5C'              UNPROT  NUM         DRK
DFHUNNON EQU   X'5D'              UNPROT  NUM         DRK  MDT
DFHBMPRO EQU   X'60'              PROT    ALPHA
DFHBMPRF EQU   X'61'              PROT    ALPHA            MDT
DFHPROTI EQU   X'E8'              PROT    ALPHA  BRT
*        EQU   X'E9'              PROT    ALPHA  BRT       MDT
DFHPROTN EQU   X'6C'              PROT    ALPHA       DRK
*        EQU   X'6D'              PROT    ALPHA       DRK  MDT
DFHBMASK EQU   X'F0'              PROT    NUM
DFHBMASF EQU   X'F1'              PROT    NUM              MDT
DFHBMASB EQU   X'F8'              PROT    NUM    BRT
*        EQU   X'F9'              PROT    NUM    BRT       MDT
* ADDED LABEL
DFHBMASD EQU   X'7C'              PROT    NUM         DRK
*        EQU   X'7D'              PROT    NUM         DRK  MDT
*
* Mapping
DFHBMEOF EQU   X'80'              FIELD ERASED
DFHBMCUR EQU   X'02'              FIELD CONTAINING CURSOR
DFHBMEF  EQU   X'82'              FIELD ERASED AND CONTAINING CURSOR
* Extended Attributes
DFHDFCOL EQU   X'00'              DEFAULT COLOUR
DFHBLUE  EQU   X'F1'              BLUE
DFHRED   EQU   X'F2'              RED
DFHPINK  EQU   X'F3'              PINK
DFHGREEN EQU   X'F4'              GREEN
DFHTURQ  EQU   X'F5'              TURQUOISE
DFHYELLO EQU   X'F6'              YELLOW
DFHNEUTR EQU   X'F7'              NEUTRAL (WHITE)
