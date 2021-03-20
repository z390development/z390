         MACRO
.*********************************************************************
.* Copyright 2005 Automated Software Tools Corporation               *
.* This source code is part of z390 assembler/emulator package       *
.* The z390 package is distributed under GNU general public license  *
.* Author - Don Higgins                                              *
.* Date   - 09/30/05                                                 *
.*********************************************************************
.* 10/20/05 RPI27 - issue error for any undefined operands  
.* 10/21/05 RPI41  - add MF=L support   
.* 11/05/05 RPI80  - add missing .ERR1 label   
.* 12/02/05 RPI114 - fix to use more unique labels to avoid dup labels
.* 12/20/05 RPI143 - allow no pos parms for MF=                        
.*********************************************************************
&N       WTO   &MSG,&MF=
         AIF   (N'&SYSLIST LE 1).NPOK
         MNOTE 12,'UNSUPPORTED OPERANDS - &SYSLIST(1)'
.NPOK    ANOP
         AIF   (&N EQ '').SKIPDS
&N       DS    0H
.SKIPDS  ANOP
         AIF   (&MF NE '').MF
         BAL   1,*+(WTO#&SYSNDX._EOT-*+1)/2*2
         DC    AL2(WTO#&SYSNDX._EOT-*,0),C&MSG
WTO#&SYSNDX._EOT EQU *
         SVC   35
         MEXIT
.MF      ANOP
         AIF   (&MF EQ 'L').MFL
         AIF   (&MF(1) NE 'E').ERR1
         AIF   (&MF(2) EQ '').SVC
         AIF   ('&MF(2)'(1,1) EQ '(').MFEREG
         LA    1,&MF(2)
         AGO   .SVC
.MFEREG  ANOP
         LR    1,&MF(2)
.SVC     ANOP
         SVC   35
         MEXIT
.MFL     ANOP
&N       DC    AL2(WTO#&SYSNDX._EOT-*,0),C&MSG
WTO#&SYSNDX._EOT EQU *
         MEXIT
.ERR1    MNOTE 8,'WTO UNDEFINED TYPE - &MF'
         MEND

