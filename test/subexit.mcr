         MACRO
.*********************************************************************
.* Copyright 2005 Automated Software Tools Corporation               *
.* This source code is part of z390 assembler/emulator package       *
.* The z390 package is distributed under GNU general public license  *
.* Author - Don Higgins                                              *
.* Date   - 09/30/05                                                 *
.*********************************************************************
.* 10/20/05 RPI27 - issue error for any undefined operands     
.*********************************************************************
&N       SUBEXIT &RC=0   
         AIF   (&SYSLIST(1) EQ '').NPOK
         MNOTE 12,'UNSUPPORTED OPERANDS - &SYSLIST(1)'
.NPOK    ANOP
         AIF   (&N EQ '').SKIPDS
&N       DS    0H
.SKIPDS  ANOP
         AIF   ('&RC'(1,1) EQ '(').RCREG
         LA    15,&RC
         AGO   .RCEND
.RCREG   ANOP
         LR    15,&RC(1)
.RCEND   ANOP
         L     13,4(13)
         LM    0,12,20(13)
         L     14,12(13)
         BR    14
         MEND
