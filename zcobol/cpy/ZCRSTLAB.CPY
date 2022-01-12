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
.********************************************************************
.* RESET LAB LEN - REMOVE SS1 LENGTH IF ANY FROM LAB 
.* USED BY ZC_CALC, GEN_ADD
.********************************************************************
.* 05/26/09 RPI 1019 INITIAL CODING SEE TESTSIX1 CALC TEST
,* 04/06/11 ROU 1139 FIX SETA STRING FOR HLASM COMP.
.********************************************************************
         AENTRY RESET_LAB_LEN
         AIF   ('&LAB'(1,1) GE '0')  IS THIS O(L,R) 
               :&ILAB SETA ('&LAB' INDEX '(')
               AIF (&ILAB GT 1)
                   :&FIELD_OFF SETA D2A('&LAB'(1,&ILAB-1)) RPI 1139
                   :&JLAB SETA ('&LAB' INDEX ',')
                   AIF (&JLAB GT &ILAB+1)
                     :&FIELD_LEN SETA D2A('&LAB'(&ILAB+1,&JLAB-1)) RPI 1139
                     :&FIELD_REG SETC '&LAB'(&JLAB+1,K'&LAB-&JLAB-1)
                     :&LAB SETC '&LAB'(1,&ILAB).'&LAB'(&JLAB+1,*) O(R)
                   AEND
               AEND
         AELSEIF ('&LAB'(1,5) EQ 'ZCVT_') STRIP ZCVT_WORKAREA+N(LEN)
               :&ILAB SETA ('&LAB' INDEX '(')
               AIF (&ILAB GT 1)
                   :&LAB SETC '&LAB'(1,&ILAB-1)
               AEND
         AEND
         AEND
