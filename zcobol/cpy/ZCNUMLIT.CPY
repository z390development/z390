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
.*
.* SCALE NUL LIT - REMOVE DECIMAL POINT AND SCALE FIXED NUMERIC LITERAL  
.*    &NUM     - SETC LITERAL VALUE WHICH IS SCALED
.*    &DEC_SYM - DECIMAL PLACES REQUIRED FROM SYM_PIC_DEC (SET BY WS)
.*               OR -1 IF NONE REQUIRED FOR MPY/DIV
.*    &DEC_NUM = DECIMAL PLACES IN LITERAL
.********************************************************************
.* 04/29/12 RPI 1211 ADD &DEC_SYM = -1 FOR MPY/DIV
         AENTRY SCALE_NUM_LIT
         AIF (&ZC_DEC_IS_COMMA)
             :&DEC_IX SETA ('&NUM' FIND ',')
         AELSE
             :&DEC_IX SETA ('&NUM' FIND '.')
         AEND
         AIF (&DEC_IX GT 0)
               :&DEC_NUM SETA K'&NUM-&DEC_IX
               AIF (&DEC_IX GT 1)
                 AIF (&DEC_IX LT K'&NUM)
                   :&NUM SETC '&NUM'(1,&DEC_IX-1).'&NUM'(&DEC_IX+1,*)
                 AELSE
                   :&NUM SETC '&NUM'(1,&DEC_IX-1)
                 AEND
               AELSE
                   :&NUM SETC '&NUM'(2,*)
               AEND
         AELSE
               :&DEC_NUM SETA 0
         AEND
         AIF (&DEC_NUM EQ &DEC_SYM OR &DEC_SYM EQ -1)  RPI 1211
             AEXIT AENTRY
         AEND
         AIF (&DEC_NUM GT &DEC_SYM)
             :&LNUM SETA K'&NUM-(&DEC_NUM-&DEC_SYM)
             AIF (&LNUM GT 0)
                 :&NUM SETC '&NUM'(1,&LNUM)
             AELSE
                 :&NUM SETC '0'
             AEND
         AELSE 
             :&NUM SETC '&NUM'.(&DEC_SYM-&DEC_NUM)'0'
         AEND
         AEND
