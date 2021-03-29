.*********************************************************************
.* Copyright 2009 Automated Software Tools Corporation               *
.* This source code is part of z390 zCOBOL/assembler/emulator package*
.* The z390 package is distributed under GNU general public license  *
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
