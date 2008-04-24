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
.* 01/05/06 RPI153 - remove duplicate label for MF=L                   
.* 01/09/06 RPI163 SKIP LR FOR SAME REGISTER
.* 01/12/06 RPI151 CHANGE BAL TO BRAS FOR USE WITHOUT BASE REG
.* 04/10/06 RPI244 IGNORE KEYWORDS EXCEPT TEXT= UNSUPPORTED ERROR  
.* 04/25/06 RPI290 ALWAYS GEN LABEL IF ANY, CHECK MF=                
.* 06/14/07 RPI 641 DEFAULT MF=I
.* 08/21/07 RPI 670 CORRECT REG OPTIMIZATION TO HANDLE ANY REG SYMBOL
.* 04/19/08 RPI 833 ADD STRING QUOTES FOR HLASM COMPATIBILITY 
.*********************************************************************
&N       WTO   &MSG,         'TEXT' OR TEXT= REQUIRED                  X
               &TEXT=,       MULTIPLE QUOTED TEXT ELSE ERROR           X
               &MF=I,        MF=(E,ADDR) OR MF=L DEFAULT IS STD.       X
               &CART=,       IGNORED                                   X
               &CONSID=,     IGNORED                                   X
               &CONSNAME=,   IGNORED                                   X
               &DESC=,       IGNORED                                   X
               &KEY=,        IGNORED                                   X
               &MCSFLAG=,    IGNORED                                   X
               &ROUTCDE=,    IGNORED                                   X
               &TOKEN=       IGNORED                                           
         AIF   ('&N' EQ '').SKIPDS
&N       DS    0H
.SKIPDS  ANOP
         AIF   (N'&SYSLIST LE 1).NPOK
         MNOTE 12,'UNSUPPORTED OPERAND - &SYSLIST(2)'
         MEXIT
.NPOK    ANOP
         AIF   ('&MF' NE 'I').MF
         AIF   ('&TEXT' NE '').ERR2
         AIF   ('&MSG' EQ '').ERR3
&MSGTEXT SETC  '&MSG'
         AIF   ('&MSG'(1,1) EQ '''').WTOMSG
         AIF   ('&MSG'(1,2) NE '(''').ERR2
&MSGTEXT SETC  '&MSG'(2,K'&MSG-2)
.WTOMSG  ANOP
         BRAS  1,*+(WTO#&SYSNDX._EOT-*+1)/2*2
         DC    AL2(WTO#&SYSNDX._EOT-*,0),C&MSGTEXT
WTO#&SYSNDX._EOT EQU *
         SVC   35
         MEXIT
.TEXT    ANOP
         AIF   ('&TEXT' EQ '').ERR3
.MF      ANOP
         AIF   ('&MF' EQ 'L').MFL
         AIF   ('&MF(1)' NE 'E').ERR1
         AIF   ('&MF(2)' EQ '').SVC
         AIF   ('&MF(2)'(1,1) EQ '(').MFEREG
         LA    1,&MF(2)
         AGO   .SVC
.MFEREG  ANOP
         ZOPTLR 1,&MF(2)
.SVC     ANOP
         SVC   35
         MEXIT
.MFL     ANOP
         DC    AL2(WTO#&SYSNDX._EOT-*,0),C&MSG
WTO#&SYSNDX._EOT EQU *
         MEXIT
.ERR1    MNOTE 8,'WTO UNDEFINED TYPE - &MF'
         MEXIT
.ERR2    MNOTE 8,'WTO TEXT OPTION NOT SUPPORTED'
         MEXIT
.ERR3    MNOTE 8,'WTO MISSING ''MSG'' OR TEXT=INED TYPE - &MF'
         MEND

