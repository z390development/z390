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
.*********************************************************************
.* Author - Zane Hambly                                              *
.* Date   - 2025/12                                                  *
.*********************************************************************
.* Sort-Merge global variables for COBOL SORT implementation.
.* Per FIPS PUB 21-2 (ANSI X3.23-1985), Section XI.
.*
.* This file is separate from ZC_WS.CPY to avoid modifying the
.* original z390 infrastructure.
.*********************************************************************
.*
.* SORT FILE TABLE DEFINED FROM SD STATEMENTS
.*
         GBLA  &SD_TOT               TOTAL SORT FILES DEFINED
         GBLA  &SD_IX                SORT FILE INDEX (0 IF NOT FOUND)
         GBLC  &SD_NAME(100)         SORT FILE NAME FROM SD
         GBLC  &SD_RECORD(100)       SORT FILE RECORD NAME
         GBLA  &SD_LRECL(100)        SORT FILE RECORD LENGTH
         GBLA  &SORT_LAB             UNIQUE SORT LABEL COUNTER
.*
.* END OF ZC_SD.CPY
.*

