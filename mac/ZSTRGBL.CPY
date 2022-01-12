.********************************************************************
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
.* 09/23/08 RPI 909 upgrade to use ZSTRMAC SPE's and extend compat.
.*          BY DON HIGGINS
.* 01/30/10 RPI 1139 NOALLOW REQUIRES SDT FOR GBL ARRAYS
.********************************************************************
.* SHARED GLOBAL VARIABLES FOR ZSTRMAC SPMS          
.********************************************************************
.*
.*  DEFINE SHARED CONDTION CODES FOR USE IN SPM OPERANDS
.*
         ZSTREQU
.*
.*  SHARED LEVEL CONTROL FOR IF, DO, SELECT, AND CASENTRY
.*
         GBLA &ZSTR_LVL      CURRENT LEVEL OF NESTING
         GBLA &ZSTR_MAX_LVL  MAX LEVEL OF NESTING
&ZSTR_MAX_LVL SETA 100
         GBLC &ZSTR_LVL_TYPE(100) IF,DO,SELECT,CASENTRY
         GBLA &ZSTR_LVL_TCNT(100) TYPE  COUNTER
         GBLA &ZSTR_LVL_BCNT(100) BLOCK COUNTER
         GBLB &ZSTR_LVL_ELSE(100) ELSE BLOCK DEFINED
         GBLB &ZSTR_LVL_TEND(100) END LABEL REQUESTED
.*
.*  IF, ELSEIF, ELSE, ENDIF
.*
         GBLA  &ZSTR_AIF_CNT                AIF COUNTER
.*
.*  DO, ENDDO                     
.*
         GBLA  &ZSTR_DO_CNT
.*
.*  SELECT, WHEN, OTHRWISE, ENDSEL
.*
         GBLA  &ZSTR_SEL_CNT             SELECT COUNTER
         GBLC  &ZSTR_SEL_OP(100)   SELECT OPCODE ('' IF NONE)
         GBLC  &ZSTR_SEL_FLD(100)  SELECT FIELD
         GBLC  &ZSTR_SEL_CMP(100)  SELECT COMPARE TYPE
.*
.*  CASENTRY, CASE, ENDCASE
.*
         GBLA &ZSTR_CASE_CNT                 CASENTRY COUNTER
         GBLA &ZSTR_CASE_LAST_INDEX          LAST CASE INDEX VALUE
         GBLC &ZSTR_CASE_REG(100)  REG WITH INDEX 1-N
         GBLA &ZSTR_CASE_MAX(100)  MAX CASE INDEX 1-N VALUE
.*
.*  END OF ZSTRGBL.CPY                     
.*
