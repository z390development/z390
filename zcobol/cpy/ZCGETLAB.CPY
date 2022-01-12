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
.*********************************************************************
.* Author - Don Higgins                                              *
.* Date   - 08/16/09                                                 *
.*********************************************************************
.* 08/16/09 move SN/PG logic shared by GEN_B, GEN_BC, GEN_ALTER
.* 08/05/09 RPI 1065 add support for PG OF SN    
.*********************************************************************
.*
.* SET PG_LAB FROM PG_NAME AND SN_NAME
.*
       AENTRY  GET_PG_LAB
       AIF     ('&PG_NAME' EQ '')
               :&ZC_PG_LAB SETC 'SN_&SN_NAME'
               AEXIT AENTRY
       AEND
       GBLA    &(ZC_PG_&PG_NAME)
       AIF     (&(ZC_PG_&PG_NAME) GT 0) IS THIS PG DEFINED
               AIF     (&(ZC_PG_&PG_NAME) GT 1) IS IT DUPLICATED
                       AIF  ('&SN_NAME' NE '')  IS THERE EXPLICIT SN
                            :&ZC_PG_LAB SETC 'SN_&SN_NAME._PG_&PG_NAME'
                       AELSEIF ('&LAST_SN_NAME' NE '') ARE WE IN SN
                            :&ZC_PG_LAB SETC 'SN_&LAST_SN_NAME._PG_&PG_X
               NAME'
                       AELSE
                            MNOTE 8,'GEN_B NO SECTION FOR DUP PG - &PG_X
               NAME' 
                            MEXIT
                       AEND
               AELSE
                   :&ZC_PG_LAB SETC 'PG_&PG_NAME'
               AEND
       AELSEIF (&(ZC_PG_&PG_NAME) EQ -1)  IS THIS SECTION
               :&ZC_PG_LAB SETC 'SN_&PG_NAME'
       AELSE
               :&ZC_PG_LAB SETC 'PG_&PG_NAME'
       AEND
       AEND
