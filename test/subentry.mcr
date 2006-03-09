         MACRO
.*********************************************************************
.* Copyright 2005 Automated Software Tools Corporation               *
.* This source code is part of z390 assembler/emulator package       *
.* The z390 package is distributed under GNU general public license  *
.* Author - Don Higgins                                              *
.* Date   - 09/30/05                                                 *
.*********************************************************************
.* 10/20/05 RPI27 - issue error for any undefined operands     
.* 10/27/05 RPI59 - add DROP 15 to avoid overlapping using     
.*********************************************************************
&N       SUBENTRY &NOCSECT
         AIF   (N'&SYSLIST LE 1).NPOK
         MNOTE 12,'UNSUPPORTED OPERANDS - &SYSLIST(2)'
.NPOK    ANOP
         AIF   (&NOCSECT EQ 'NOCSECT').NOCSECT 
&N       CSECT
         AGO   .USING
.NOCSECT ANOP
&N       DS    0D
.USING   ANOP
         USING *,15
         STM   14,12,12(13)
         BAL   15,*+4+18*4
         DROP  15
         USING *,13
         DS    18F
         ST    15,8(13)
         ST    13,4(15)
         LR    13,15
         MEND
