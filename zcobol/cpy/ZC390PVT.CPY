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
.* Date   - 06/18/09                                                 *
.*********************************************************************
.* 06/18/09 RPI 1042 use ZC390PVT passed to ZC390NUC to verify CICS
.* 05/25/10 RPI 1118 add PVTMAXDL max display line from ZC_MAXDISPLAY
.*********************************************************************
PVTPGM   DS    CL8  ZCOBOL PGM NAME                    
PVTDATE  DS    CL8  ZCOBOL COMPILE DATE              
PVTTIME  DS    CL8  ZCOBOL COMPILE TIME              
PVTVER   DS    CL8  ZCOBOL COMPILER VERSION       
PVTCICS  DS    C    ZCOBOL EXEC CICS PGM
PVTEXTND DS    C    ZCOBOL EXTENDED PRECISION
PVTTRUNC DS    C    ZCOBOL TRUNCATE TO PICTURE
PVTR64   DS    C    ZCOBOL USE 64 BIT REGISTERS
PVTMAXDL DS    H    ZCOBOL MAX DISPLAY LINE LENGTH
         DS    XL10 RESERVED
.*********************************************************************
.* end of ZC390PVT                                             
.*********************************************************************
