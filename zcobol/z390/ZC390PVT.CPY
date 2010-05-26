.*********************************************************************
.* Copyright 2009 Automated Software Tools Corporation               *
.* This source code is part of z390 zCOBOL/assembler/emulator package*
.* The z390 package is distributed under GNU general public license  *
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
