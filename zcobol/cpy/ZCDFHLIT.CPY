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
.* 01/25/12   DSH RPI 1181 PREVENT DUP ALLOC ERROR
.* 18-02-2017 JH  RPI 1501 error message generated when DFHRESP(...)
.*                         is used in an ELSE IF clause.
.********************************************************************
.*
.* GET DFHRESP - SET &DFHLIT FOR &DFHRESP
.*
.********************************************************************
         AENTRY GET_DFHRESP
         LCLB   &ZC_DFHRESP          RPI 1501
         :&ZC_DFHRESP SETB &ZC_DFHRESP RPI 1181 PREVENT DUP ALLOC 
         AIF   (NOT &ZC_DFHRESP)
               :&ZC_DFHRESP SETB 1
               COPY ZC_DFHRESP                            RPI 1057
         AEND
         :&DFHSYM SETC 'DFHRESP_&DFHRESP'
         :&(&DFHSYM) SETC '&(&DFHSYM)' RPI 1181 PREVENT DUP ALLOC
         :&DFHLIT SETC '&(&DFHSYM)'
         AIF   (K'&DFHLIT EQ 0)
               MNOTE 8,'ZCDFHLIT DFHRESP VALUE NOT FOUND - &DFHRESP'
               MEXIT
         AEND
         AEND
.*
.* GET DFHVALUE - SET &DFHLIT FOR &DFHVALUE
.*
         AENTRY GET_DFHVALUE
         LCLB   &ZC_DFHVALUE            RPI 1501
         &ZC_DFHVALUE SETB &ZC_DFHVALUE RPI 1181 PREVENT DUP ALLOC
         AIF   (NOT &ZC_DFHVALUE)
               :&ZC_DFHVALUE SETB 1
               COPY ZC_DFHVALUE                           RPI 1057
         AEND
         :&DFHSYM SETC 'DFHVALUE_&DFHVALUE'
         :&(&DFHSYM) SETC &(&DFHSYM) RPI 1181 PREVENT DUP ALLOC
         :&DFHLIT SETC '&(&DFHSYM)'
         AIF   (K'&DFHLIT EQ 0)
               MNOTE 8,'ZCDFHLIT DFHVALUE NOT FOUND - &DFHVALUE'
               MEXIT
         AEND
         AEND
