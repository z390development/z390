         MACRO
.*********************************************************************
.* Copyright 2008 Automated Software Tools Corporation               *
.* This source code is part of z390 zCOBOL/assembler/emulator package*
.* The z390 package is distributed under GNU general public license  *
.* Author - Don Higgins                                              *
.* Date   - 04/07/08                                                 *
.*********************************************************************
.* 04/09/08 initial coding for global ACCEPT
.*********************************************************************
         ACCEPT &TARGET,&FROM,&SOURCE
         GEN_ACCEPT &TARGET,&FROM,&SOURCE
         MEND
         MACRO
.*********************************************************************
.* Copyright 2008 Automated Software Tools Corporation               *
.* This source code is part of z390 zCOBOL/assembler/emulator package*
.* The z390 package is distributed under GNU general public license  *
.* Author - Don Higgins                                              *
.* Date   - 04/07/08                                                 *
.*********************************************************************
.* 10/28/08 initial coding for global ADD
.* 12/22/08 use ZCGETFLD to get field with qual,base,subscript
.* 02/15/09 support mult s1 added to s2 or mult s1+s2 added to s3
.* 02/26/09 RPI 1012 move onsize end-add pending to add
.* 08/09/09 RPI 1065 add ACTR for mult operands
.*********************************************************************
         ADD
         COPY  ZC_WS
&PARM_IX SETA 2
&NP      SETA N'&SYSLIST
&TO_IX   SETA 0
&GIVING_IX SETA 0
&ROUNDED SETB 0
&ONSIZE  SETB 0
&LAST_PARM SETA 0
.*       AWHILE (&PARM_IX LT &NP)
.AWH_1_T   ANOP
           AIF    (NOT(&PARM_IX LT &NP)).AWH_1_E
               ACTR 10000
.*             AIF ('&SYSLIST(&PARM_IX)' EQ 'TO')
                 AIF (NOT('&SYSLIST(&PARM_IX)' EQ 'TO')).AIF_1_1
&TO_IX             SETA &PARM_IX
.*             AELSEIF ('&SYSLIST(&PARM_IX)' EQ 'GIVING')
                 AGO   .AIF_1_E
.AIF_1_1         ANOP
                 AIF     (NOT('&SYSLIST(&PARM_IX)' EQ 'GIVING')).AIF_1_X
               2
&GIVING_IX         SETA &PARM_IX
.*             AELSEIF ('&SYSLIST(&PARM_IX)' EQ 'ROUNDED')
                 AGO   .AIF_1_E
.AIF_1_2         ANOP
                 AIF     (NOT('&SYSLIST(&PARM_IX)' EQ 'ROUNDED')).AIF_1X
               _3
&ROUNDED           SETB 1
.*                 AIF (&LAST_PARM EQ 0)
                     AIF (NOT(&LAST_PARM EQ 0)).AIF_2_1
&LAST_PARM             SETA &PARM_IX-1
.*                 AEND
.AIF_2_1             ANOP
.*             AELSEIF ('&SYSLIST(&PARM_IX)' EQ 'ON'                   X
                        AND '&SYSLIST(&PARM_IX+1)' EQ 'SIZE'           X
                        AND '&SYSLIST(&PARM_IX+2)' EQ 'ERROR')
                 AGO   .AIF_1_E
.AIF_1_3         ANOP
                 AIF     (NOT('&SYSLIST(&PARM_IX)' EQ 'ON'             X
                              AND '&SYSLIST(&PARM_IX+1)' EQ 'SIZE'     X
                              AND '&SYSLIST(&PARM_IX+2)' EQ 'ERROR')).AX
               IF_1_4
&ONSIZE            SETB 1
.*                 AIF (&LAST_PARM EQ 0)
                     AIF (NOT(&LAST_PARM EQ 0)).AIF_3_1
&LAST_PARM             SETA &PARM_IX-1
.*                 AEND
.AIF_3_1             ANOP
&PARM_IX           SETA &PARM_IX+2
.*             AELSEIF ('&SYSLIST(&PARM_IX)' EQ 'SIZE'                 X
                        AND '&SYSLIST(&PARM_IX+1)' EQ 'ERROR')
                 AGO   .AIF_1_E
.AIF_1_4         ANOP
                 AIF     (NOT('&SYSLIST(&PARM_IX)' EQ 'SIZE'           X
                              AND '&SYSLIST(&PARM_IX+1)' EQ 'ERROR')).AX
               IF_1_5
&ONSIZE            SETB 1
.*                 AIF (&LAST_PARM EQ 0)
                     AIF (NOT(&LAST_PARM EQ 0)).AIF_4_1
&LAST_PARM             SETA &PARM_IX-1
.*                 AEND
.AIF_4_1             ANOP
&PARM_IX           SETA &PARM_IX+1
.*             AEND
.AIF_1_5         ANOP
.AIF_1_E         ANOP
&PARM_IX       SETA &PARM_IX+1
.*       AEND
           AGO   .AWH_1_T
.AWH_1_E   ANOP
.*       AIF   (&ONSIZE)
           AIF   (NOT(&ONSIZE)).AIF_5_1
&IE_LVL        SETA  &IE_LVL+1         ADD PENDING END_ADD
&IE_TYPE(&IE_LVL) SETA 11        FOR END_ADD OR PERIOD
&ADD_LAB       SETA &ADD_LAB+1
&IE_TCNT(&IE_LVL) SETA &ADD_LAB
.*       AEND
.AIF_5_1   ANOP
.*       AIF   (&LAST_PARM GT 0)
           AIF   (NOT(&LAST_PARM GT 0)).AIF_6_1
&NP            SETA &LAST_PARM
.*       AEND
.AIF_6_1   ANOP
.*       AIF   (&GIVING_IX GT 0)  ADD S1 [+S2] GIVING S3
           AIF   (NOT(&GIVING_IX GT 0)).AIF_7_1   ADD S1 [+S2] GIVING SX
               3
.*             ACALL ADD_GIVING
&ACALL_1_ADD_GIVING SETA    1
                 AGO   .ACL_1_ADD_GIVING
.ACL_1_1         ANOP
.*       AELSE
           AGO   .AIF_7_E
.AIF_7_1   ANOP
.*             ACALL ADD_TO
&ACALL_2_ADD_TO  SETA    1
                 AGO   .ACL_2_ADD_TO
.ACL_2_1         ANOP
.*       AEND
.AIF_7_E   ANOP
.*       AIF   (&ONSIZE)
           AIF   (NOT(&ONSIZE)).AIF_8_1
               GEN_ONSIZE_START ADD
.*       AEND
.AIF_8_1   ANOP
         MEXIT
.*
.* ADD GIVING
.*
.*       AENTRY ADD_GIVING
           AGO   .ACL_1_SKIP
.ACL_1_ADD_GIVING ANOP
&S1_PARM_IX SETA 1
&S3_PARM_IX SETA &GIVING_IX+1
&S1_OK   SETB 1
&S3_OK   SETB 1
.*       ACALL GET_S1
&ACALL_3_GET_S1 SETA    1
           AGO   .ACL_3_GET_S1
.ACL_3_1   ANOP
.*       ACALL GET_S3
&ACALL_4_GET_S3 SETA    1
           AGO   .ACL_4_GET_S3
.ACL_4_1   ANOP
.*       AWHILE (&S1_OK AND &S3_OK)
.AWH_2_T   ANOP
           AIF    (NOT(&S1_OK AND &S3_OK)).AWH_2_E
               GEN_MOVE &S3,&S3_IX,&S1,&S1_IX
.*             ACALL GET_S1
&ACALL_3_GET_S1  SETA    2
                 AGO   .ACL_3_GET_S1
.ACL_3_2         ANOP
.*             AWHILE (&S1_OK)
.AWH_3_T         ANOP
                 AIF    (NOT(&S1_OK)).AWH_3_E
                   GEN_ADD  &S3,&S3_IX,&S1,&S1_IX,&ROUNDED,&ONSIZE
.*                 ACALL GET_S1
&ACALL_3_GET_S1      SETA    3
                     AGO   .ACL_3_GET_S1
.ACL_3_3             ANOP
.*             AEND
                 AGO   .AWH_3_T
.AWH_3_E         ANOP
.*             ACALL GET_S3
&ACALL_4_GET_S3  SETA    2
                 AGO   .ACL_4_GET_S3
.ACL_4_2         ANOP
.*             AIF (&S3_OK)
                 AIF (NOT(&S3_OK)).AIF_9_1
&S1_PARM_IX        SETA 1
&S1_OK             SETB 1
.*                 ACALL GET_S1
&ACALL_3_GET_S1      SETA    4
                     AGO   .ACL_3_GET_S1
.ACL_3_4             ANOP
.*             AEND
.AIF_9_1         ANOP
.*       AEND
           AGO   .AWH_2_T
.AWH_2_E   ANOP
.*       AEND
           AGO   (&ACALL_1_ADD_GIVING).ACL_1_1
.ACL_1_SKIP ANOP
.*
.* ADD TO
.*
.*       AENTRY ADD_TO
           AGO   .ACL_2_SKIP
.ACL_2_ADD_TO ANOP
&S1_PARM_IX SETA 1
&S2_PARM_IX SETA &TO_IX+1
&S1_OK   SETB 1
&S2_OK   SETB 1
.*       ACALL GET_S1
&ACALL_3_GET_S1 SETA    5
           AGO   .ACL_3_GET_S1
.ACL_3_5   ANOP
.*       ACALL GET_S2
&ACALL_5_GET_S2 SETA    1
           AGO   .ACL_5_GET_S2
.ACL_5_1   ANOP
.*       AWHILE (&S1_OK AND &S2_OK)
.AWH_4_T   ANOP
           AIF    (NOT(&S1_OK AND &S2_OK)).AWH_4_E
               GEN_ADD  &S2,&S2_IX,&S1,&S1_IX,&ROUNDED,&ONSIZE
.*             ACALL GET_S1
&ACALL_3_GET_S1  SETA    6
                 AGO   .ACL_3_GET_S1
.ACL_3_6         ANOP
.*             AWHILE (&S1_OK)
.AWH_5_T         ANOP
                 AIF    (NOT(&S1_OK)).AWH_5_E
                   GEN_ADD  &S2,&S2_IX,&S1,&S1_IX,&ROUNDED,&ONSIZE
.*                 ACALL GET_S1
&ACALL_3_GET_S1      SETA    7
                     AGO   .ACL_3_GET_S1
.ACL_3_7             ANOP
.*             AEND
                 AGO   .AWH_5_T
.AWH_5_E         ANOP
.*             ACALL GET_S2
&ACALL_5_GET_S2  SETA    2
                 AGO   .ACL_5_GET_S2
.ACL_5_2         ANOP
.*             AIF (&S2_OK)
                 AIF (NOT(&S2_OK)).AIF_10_1
&S1_PARM_IX        SETA 1
&S1_OK             SETB 1
.*                 ACALL GET_S1
&ACALL_3_GET_S1      SETA    8
                     AGO   .ACL_3_GET_S1
.ACL_3_8             ANOP
.*             AEND
.AIF_10_1        ANOP
.*       AEND
           AGO   .AWH_4_T
.AWH_4_E   ANOP
.*       AEND
           AGO   (&ACALL_2_ADD_TO).ACL_2_1
.ACL_2_SKIP ANOP
.*
.* GET S1
.*
.*       AENTRY GET_S1
           AGO   .ACL_3_SKIP
.ACL_3_GET_S1 ANOP
&PARM_IX SETA &S1_PARM_IX
.*       AIF   ('&SYSLIST(&PARM_IX)' EQ 'TO')
           AIF   (NOT('&SYSLIST(&PARM_IX)' EQ 'TO')).AIF_11_1
.*             AIF (&GIVING_IX GT 0)
                 AIF (NOT(&GIVING_IX GT 0)).AIF_12_1
&PARM_IX           SETA &PARM_IX+1
.*             AELSE
                 AGO   .AIF_12_E
.AIF_12_1        ANOP
&S1_OK             SETB 0
.*                 AEXIT AENTRY
                     AGO   .ACL_3_E
.*             AEND
.AIF_12_E        ANOP
.*       AELSEIF (&PARM_IX EQ &GIVING_IX)
           AGO   .AIF_11_E
.AIF_11_1  ANOP
           AIF     (NOT(&PARM_IX EQ &GIVING_IX)).AIF_11_2
&S1_OK         SETB 0
.*             AEXIT AENTRY
                 AGO   .ACL_3_E
.*       AEND
.AIF_11_2  ANOP
.AIF_11_E  ANOP
&FIELD_REG SETC 'ZC_R1'         S1 IN ZC_R1
&FIELD_SS1 SETB 0
.*       ACALL GET_PARM_FIELD
&ACALL_6_GET_PARM_FIELD SETA    1
           AGO   .ACL_6_GET_PARM_FIELD
.ACL_6_1   ANOP
&S1_PARM_IX SETA &PARM_IX
&S1      SETC  '&FIELD_NAME'
&S1_IX   SETA  &FIELD_IX
.*       AIF   ('&S1' EQ '')
           AIF   (NOT('&S1' EQ '')).AIF_13_1
               MNOTE 8,'ADD FIELD1 NOT FOUND'
               MEXIT
.*       AEND
.AIF_13_1  ANOP
.*       AEND
.ACL_3_E   ANOP
           AGO   (&ACALL_3_GET_S1).ACL_3_1,.ACL_3_2,.ACL_3_3,.ACL_3_4,.X
               ACL_3_5,.ACL_3_6,.ACL_3_7,.ACL_3_8
.ACL_3_SKIP ANOP
.*
.* GET S2
.*
.*       AENTRY GET_S2
           AGO   .ACL_5_SKIP
.ACL_5_GET_S2 ANOP
&PARM_IX SETA &S2_PARM_IX
.*       AIF (&PARM_IX GT &NP)
           AIF (NOT(&PARM_IX GT &NP)).AIF_14_1
&S2_OK       SETB 0
.*           AEXIT AENTRY
               AGO   .ACL_5_E
.*       AEND
.AIF_14_1  ANOP
&FIELD_REG SETC 'ZC_R2'               S2 IN ZC_R2
&FIELD_SS1 SETB 0
.*       ACALL GET_PARM_FIELD
&ACALL_6_GET_PARM_FIELD SETA    2
           AGO   .ACL_6_GET_PARM_FIELD
.ACL_6_2   ANOP
&S2_PARM_IX SETA &PARM_IX
&S2      SETC '&FIELD_NAME'
&S2_IX   SETA  &FIELD_IX
.*       AIF   ('&S2' EQ '')
           AIF   (NOT('&S2' EQ '')).AIF_15_1
               MNOTE 8,'ADD FIELD2 NOT FOUND'
               MEXIT
.*       AEND
.AIF_15_1  ANOP
.*       AEND
.ACL_5_E   ANOP
           AGO   (&ACALL_5_GET_S2).ACL_5_1,.ACL_5_2
.ACL_5_SKIP ANOP
.*
.* GET S3
.*
.*       AENTRY GET_S3
           AGO   .ACL_4_SKIP
.ACL_4_GET_S3 ANOP
&PARM_IX SETA &S3_PARM_IX
.*       AIF (&PARM_IX GT &NP)
           AIF (NOT(&PARM_IX GT &NP)).AIF_16_1
&S3_OK       SETB 0
.*           AEXIT AENTRY
               AGO   .ACL_4_E
.*       AEND
.AIF_16_1  ANOP
&FIELD_REG SETC 'ZC_R3'            S3 IN ZC_R3
&FIELD_SS1 SETB 0
.*       ACALL GET_PARM_FIELD
&ACALL_6_GET_PARM_FIELD SETA    3
           AGO   .ACL_6_GET_PARM_FIELD
.ACL_6_3   ANOP
&S3_PARM_IX SETA &PARM_IX
&S3      SETC '&FIELD_NAME'
&S3_IX   SETA  &FIELD_IX
.*       AIF   ('&S3' EQ '')
           AIF   (NOT('&S3' EQ '')).AIF_17_1
               MNOTE 8,'ADD FIELD3 NOT FOUND'
               MEXIT
.*       AEND
.AIF_17_1  ANOP
.*       AEND
.ACL_4_E   ANOP
           AGO   (&ACALL_4_GET_S3).ACL_4_1,.ACL_4_2
.ACL_4_SKIP ANOP
         COPY ZCGETFLD
         MEND
         MACRO
.*********************************************************************
.* Copyright 2009 Automated Software Tools Corporation               *
.* This source code is part of z390 zCOBOL/assembler/emulator package*
.* The z390 package is distributed under GNU general public license  *
.* Author - Don Higgins                                              *
.* Date   - 08/02/09                                                 *
.*********************************************************************
.* 08/03/09 stub for pending support
.*********************************************************************
         ALPHABET
         MNOTE 8,'ALPHABET NOT SUPPORTED YET'
         MEXIT
         MEND
         MACRO
.*********************************************************************
.* Copyright 2008 Automated Software Tools Corporation               *
.* This source code is part of z390 zCOBOL/assembler/emulator package*
.* The z390 package is distributed under GNU general public license  *
.* Author - Don Higgins                                              *
.* Date   - 04/16/08                                                 *
.*********************************************************************
.* 04/16/08 initial coding for global ALTER
.* 09/29/08 ZSTRMAC
.*********************************************************************
.* WARNING: THIS CODE DOES NOT VERIFY THAT THE TARGET IS A GO TO.
.* PLEASE REPLACE USE OF ALTER WITH EVALUATE WHEN STRUCTURE.
.*********************************************************************
         ALTER
&NP      SETA  N'&SYSLIST
&PARM_IX SETA 1
.*       AWHILE (&PARM_IX LE &NP-2)
.AWH_6_T   ANOP
           AIF    (NOT(&PARM_IX LE &NP-2)).AWH_6_E
&PG_OLD        SETC  '&SYSLIST(&PARM_IX)'
.*             AIF ('&SYSLIST(&PARM_IX+1)' EQ 'OF'                     X
                    OR '&SYSLIST(&PARM_IX+1)' EQ 'IN')
                 AIF (NOT('&SYSLIST(&PARM_IX+1)' EQ 'OF'               X
                          OR '&SYSLIST(&PARM_IX+1)' EQ 'IN')).AIF_18_1
&SN_OLD            SETC '&SYSLIST(&PARM_IX+2)'
&PARM_IX           SETA &PARM_IX+2
.*             AELSE
                 AGO   .AIF_18_E
.AIF_18_1        ANOP
&SN_OLD            SETC ''
.*             AEND
.AIF_18_E        ANOP
.*             AIF ('&SYSLIST(&PARM_IX+1)' EQ 'TO')
                 AIF (NOT('&SYSLIST(&PARM_IX+1)' EQ 'TO')).AIF_19_1
.*                 AIF ('&SYSLIST(&PARM_IX+2)' EQ 'PROCEED'            X
                        AND '&SYSLIST(&PARM_IX+3)' EQ 'TO')
                     AIF (NOT('&SYSLIST(&PARM_IX+2)' EQ 'PROCEED'      X
                              AND '&SYSLIST(&PARM_IX+3)' EQ 'TO')).AIF_X
               20_1
&PARM_IX                SETA &PARM_IX+5
.*                 AELSE
                     AGO   .AIF_20_E
.AIF_20_1            ANOP
&PARM_IX                SETA &PARM_IX+3
.*                 AEND
.AIF_20_E            ANOP
&PG_NEW            SETC  '&SYSLIST(&PARM_IX-1)'
.*                 AIF ('&SYSLIST(&PARM_IX+1)' EQ 'OF'                 X
                        OR '&SYSLIST(&PARM_IX+1)' EQ 'IN')
                     AIF (NOT('&SYSLIST(&PARM_IX+1)' EQ 'OF'           X
                              OR '&SYSLIST(&PARM_IX+1)' EQ 'IN')).AIF_2X
               1_1
&SN_NEW                SETC '&SYSLIST(&PARM_IX+2)'
&PARM_IX               SETA &PARM_IX+2
.*                 AELSE
                     AGO   .AIF_21_E
.AIF_21_1            ANOP
&SN_NEW                SETC ''
.*                 AEND
.AIF_21_E            ANOP
.*             AELSE
                 AGO   .AIF_19_E
.AIF_19_1        ANOP
                   MNOTE 8,'ALTER TO NOT FOUND'
                   MEXIT
.*             AEND
.AIF_19_E        ANOP
               GEN_ALTER &PG_OLD,&SN_OLD,&PG_NEW,&SN_NEW
.*       AEND
           AGO   .AWH_6_T
.AWH_6_E   ANOP
.*       AIF   (&PARM_IX NE &NP+1)
           AIF   (NOT(&PARM_IX NE &NP+1)).AIF_22_1
               MNOTE 8,'ALTER TO OR TO PROCEED TO PARAMETER ERROR'
               MEXIT
.*       AEND
.AIF_22_1  ANOP
         MEND
         MACRO
.*********************************************************************
.* Copyright 2008 Automated Software Tools Corporation               *
.* This source code is part of z390 zCOBOL/assembler/emulator package*
.* The z390 package is distributed under GNU general public license  *
.* Author - Don Higgins                                              *
.* Date   - 04/07/08                                                 *
.*********************************************************************
.* 04/07/08 initial coding for z390 target
.*********************************************************************
         AUTHOR &NAME
         MEND
         MACRO
.*********************************************************************
.* Copyright 2008 Automated Software Tools Corporation               *
.* This source code is part of z390 zCOBOL/assembler/emulator package*
.* The z390 package is distributed under GNU general public license  *
.* Author - Don Higgins                                              *
.* Date   - 04/07/08                                                 *
.*********************************************************************
.* 10/14/08 initial coding for global CALL
.*********************************************************************
         CALL  &PGM,&USING
         COPY  ZC_WS
&NP      SETA N'&SYSLIST
.*       AIF   (&NP EQ 1)
           AIF   (NOT(&NP EQ 1)).AIF_23_1
               GEN_CALL &PGM
               MEXIT
.*       AEND
.AIF_23_1  ANOP
.*       AIF   ('&USING' NE 'USING')
           AIF   (NOT('&USING' NE 'USING')).AIF_24_1
               MNOTE 8,'CALL MISSING USING'
               MEXIT
.*       AEND
.AIF_24_1  ANOP
&RETURN_IX SETA 0
&I       SETA 3
&PARMS   SETC 'USING'
.*       AWHILE (&I LE &NP)
.AWH_7_T   ANOP
           AIF    (NOT(&I LE &NP)).AWH_7_E
.*             AIF ('&SYSLIST(&I)' EQ 'ON'                             X
                 OR '&SYSLIST(&I)' EQ 'NOT'                            X
                 OR '&SYSLIST(&I)' EQ 'OVERFLOW'                       X
                 OR '&SYSLIST(&I)' EQ 'EXCEPTION')
                 AIF (NOT('&SYSLIST(&I)' EQ 'ON'                       X
                       OR '&SYSLIST(&I)' EQ 'NOT'                      X
                       OR '&SYSLIST(&I)' EQ 'OVERFLOW'                 X
                       OR '&SYSLIST(&I)' EQ 'EXCEPTION')).AIF_25_1
                   MNOTE 8,'CALL ON EXCEPTION/OVERFLOW NOT SUPPORTED'
                   MEXIT
.*             AELSEIF ('&SYSLIST(&I)' EQ 'RETURNING')
                 AGO   .AIF_25_E
.AIF_25_1        ANOP
                 AIF     (NOT('&SYSLIST(&I)' EQ 'RETURNING')).AIF_25_2
&RETURN_IX         SETA &I+1
.*                 AEXIT AWHILE
                     AGO   .AWH_7_E
.*             AEND
.AIF_25_2        ANOP
.AIF_25_E        ANOP
&PARMS         SETC '&PARMS,&SYSLIST(&I)'
&I             SETA &I+1
.*       AEND
           AGO   .AWH_7_T
.AWH_7_E   ANOP
         GEN_CALL &PGM,&PARMS
.*       AIF (&RETURN_IX GT 0)
           AIF (NOT(&RETURN_IX GT 0)).AIF_26_1
&PARM_IX       SETA &RETURN_IX
&FIELD_REG     SETC 'ZC_R2'
&FIELD_SS1     SETB 1
.*             ACALL GET_PARM_FIELD
&ACALL_6_GET_PARM_FIELD SETA    4
                 AGO   .ACL_6_GET_PARM_FIELD
.ACL_6_4         ANOP
.*             AIF (&FIELD_IX GT 0)
                 AIF (NOT(&FIELD_IX GT 0)).AIF_27_1
                   ZC_SYM_FIND RETURN_CODE
                   GEN_MOVE &FIELD_NAME,&FIELD_IX,RETURN_CODE,&SYM_IX
.*             AELSE
                 AGO   .AIF_27_E
.AIF_27_1        ANOP
                   MNOTE 8,'CALL RETURNING FIELD NOT FOUND - &FIELD_NAMX
               E'
.*             AEND
.AIF_27_E        ANOP
.*       AEND
.AIF_26_1  ANOP
         MEXIT
         COPY  ZCGETFLD
         MEND
         MACRO
.*********************************************************************
.* Copyright 2008 Automated Software Tools Corporation               *
.* This source code is part of z390 zCOBOL/assembler/emulator package*
.* The z390 package is distributed under GNU general public license  *
.* Author - Don Higgins                                              *
.* Date   - 04/16/08                                                 *
.*********************************************************************
.* 11/17/08 stub for pending support
.*********************************************************************
         CANCEL
         MNOTE 8,'CANCEL NOT SUPPORTED YET'
         MEXIT
         MEND
         MACRO
.*********************************************************************
.* Copyright 2008 Automated Software Tools Corporation               *
.* This source code is part of z390 zCOBOL/assembler/emulator package*
.* The z390 package is distributed under GNU general public license  *
.* Author - Don Higgins                                              *
.* Date   - 04/29/08                                                 *
.*********************************************************************
.* 04/29/08 initial coding for global CBL options for MFI compatibility
.*********************************************************************
         CBL
         COPY ZC_WS
&NP      SETA N'&SYSLIST
&I       SETA 1
.*       AWHILE (&I LE &NP)
.AWH_8_T   ANOP
           AIF    (NOT(&I LE &NP)).AWH_8_E
.*            AIF ('&SYSLIST(&I)' EQ 'CICS')
                AIF (NOT('&SYSLIST(&I)' EQ 'CICS')).AIF_28_1
.*                AIF (NOT &ZC_CICS)
                    AIF (NOT(NOT &ZC_CICS)).AIF_29_1
                      MNOTE 8,'CBL THIS PROGRAM REQUIRES CICS OPTION'
.*                AEND
.AIF_29_1           ANOP
.*            AELSEIF (&ZC_WARN)
                AGO   .AIF_28_E
.AIF_28_1       ANOP
                AIF     (NOT(&ZC_WARN)).AIF_28_2
                  MNOTE 4,'CBL - OPTION IGNORED - &SYSLIST(&I)'
.*            AEND
.AIF_28_2       ANOP
.AIF_28_E       ANOP
&I            SETA &I+1
.*       AEND
           AGO   .AWH_8_T
.AWH_8_E   ANOP
         MEND
         MACRO
.*********************************************************************
.* Copyright 2008 Automated Software Tools Corporation               *
.* This source code is part of z390 zCOBOL/assembler/emulator package*
.* The z390 package is distributed under GNU general public license  *
.* Author - Don Higgins                                              *
.* Date   - 04/16/08                                                 *
.*********************************************************************
.* 02/25/09 stub for pending support
.*********************************************************************
         CD
         MNOTE 8,'CD NOT SUPPORTED YET'
         MEXIT
         MEND
         MACRO
.*********************************************************************
.* Copyright 2009 Automated Software Tools Corporation               *
.* This source code is part of z390 zCOBOL/assembler/emulator package*
.* The z390 package is distributed under GNU general public license  *
.* Author - Don Higgins                                              *
.* Date   - 08/02/09                                                 *
.*********************************************************************
.* 08/03/09 stub for pending support
.*********************************************************************
         CLASS
         MNOTE 8,'CLASS NOT SUPPORTED YET'
         MEXIT
         MEND
         MACRO
.*********************************************************************
.* Copyright 2008 Automated Software Tools Corporation               *
.* This source code is part of z390 zCOBOL/assembler/emulator package*
.* The z390 package is distributed under GNU general public license  *
.* Author - Don Higgins                                              *
.* Date   - 04/14/08                                                 *
.*********************************************************************
.* 04/14/08 initial coding for global ZCOBOL
.* 10/05/08 ZSTRMAC
.*********************************************************************
         CLOSE &FILE
         COPY  ZC_WS
         ZC_FILE_FIND &FILE
.*       AIF   (&FILE_IX GT 0)
           AIF   (NOT(&FILE_IX GT 0)).AIF_30_1
               GEN_CLOSE
.*       AELSE
           AGO   .AIF_30_E
.AIF_30_1  ANOP
               MNOTE 8,'CLOSE FILE NOT FOUND - &FILE'
.*       AEND
.AIF_30_E  ANOP
         MEND
         MACRO
.*********************************************************************
.* Copyright 2008 Automated Software Tools Corporation               *
.* This source code is part of z390 zCOBOL/assembler/emulator package*
.* The z390 package is distributed under GNU general public license  *
.* Author - Don Higgins                                              *
.* Date   - 04/16/08                                                 *
.*********************************************************************
.* 02/20/09 initial coding for global COMMUNICATION SECTION
.*********************************************************************
         COMMUNICATION
         COPY CD.MAC  REPLACE CD INSTRUCTION WITH CD MACRO
         MEND
         MACRO
.*********************************************************************
.* Copyright 2008 Automated Software Tools Corporation               *
.* This source code is part of z390 zCOBOL/assembler/emulator package*
.* The z390 package is distributed under GNU general public license  *
.* Author - Don Higgins                                              *
.* Date   - 04/07/08                                                 *
.*********************************************************************
.* 03/04/09 initial coding for COMPUTE using IF as model
.*********************************************************************
.* Pending work includes support for field,lit,expression in either
.* first or second operand.  Coming after COMPUTE dubugs GEN_EXP.
.*********************************************************************
         COMPUTE
         COPY  ZC_WS
&PARM_IX SETA 2
.*       AWHILE (&PARM_IX LT N'&SYSLIST                                X
                 AND '&SYSLIST(&PARM_IX)' NE '='                       X
                 AND '&SYSLIST(&PARM_IX)' NE 'EQUAL')
.AWH_9_T   ANOP
           AIF    (NOT(&PARM_IX LT N'&SYSLIST                          X
                       AND '&SYSLIST(&PARM_IX)' NE '='                 X
                       AND '&SYSLIST(&PARM_IX)' NE 'EQUAL')).AWH_9_E
&PARM_IX       SETA &PARM_IX+1
.*       AEND
           AGO   .AWH_9_T
.AWH_9_E   ANOP
.*       AIF   (&PARM_IX GT N'&SYSLIST)
           AIF   (NOT(&PARM_IX GT N'&SYSLIST)).AIF_31_1
               MNOTE 8,'COMPUTE = NOT FOUND'
               MEXIT
.*       AEND
.AIF_31_1  ANOP
&EXP_PARMS SETC '&SYSLIST(&PARM_IX+1)'
&PARM_IX SETA &PARM_IX+2
.*       AWHILE (&PARM_IX LE N'&SYSLIST                                X
                 AND '&SYSLIST(&PARM_IX)' NE 'ON'                      X
                 AND '&SYSLIST(&PARM_IX)' NE 'NOT') COLLECT EXP PARMS
.AWH_10_T  ANOP
           AIF    (NOT(&PARM_IX LE N'&SYSLIST                          X
                       AND '&SYSLIST(&PARM_IX)' NE 'ON'                X
                       AND '&SYSLIST(&PARM_IX)' NE 'NOT')).AWH_10_E  COX
               LLECT EXP PARMS
&EXP_PARMS     SETC '&EXP_PARMS,&SYSLIST(&PARM_IX)'
&PARM_IX       SETA &PARM_IX+1
.*       AEND
           AGO   .AWH_10_T
.AWH_10_E  ANOP
.*       AIF   ('&SYSLIST(&PARM_IX)' EQ 'ON'                           X
                OR '&SYSLIST(&PARM_IX)' EQ 'NOT') ON SIZE/NOT ON SIZE
           AIF   (NOT('&SYSLIST(&PARM_IX)' EQ 'ON'                     X
                      OR '&SYSLIST(&PARM_IX)' EQ 'NOT')).AIF_32_1  ON SX
               IZE/NOT ON SIZE
               MNOTE 8,'COMPUTE ON SIZE NOT SUPPORTED YET'
               MEXIT
.*       AEND
.AIF_32_1  ANOP
&PARM_IX SETA 1
&FIELD_REG SETC 'ZC_R15' NOTE GEN_ADD USES 1, 2, 14 FOR LITS
&FIELD_SS1 SETB 1
.*       ACALL GET_PARM_FIELD                      GET TARGET FIELD
&ACALL_6_GET_PARM_FIELD SETA    5
           AGO   .ACL_6_GET_PARM_FIELD
.ACL_6_5   ANOP
.*     AIF   (&FIELD_IX GT 0)
         AIF   (NOT(&FIELD_IX GT 0)).AIF_33_1
         ZC_CALC &FIELD_NAME,&FIELD_IX,&EXP_PARMS  CALC EXP
.*     AELSE
         AGO   .AIF_33_E
.AIF_33_1 ANOP
         MNOTE 8,'COMPUTE TARGET FIELD NOT FOUND - &SYSLIST(1)'
.*     AEND
.AIF_33_E ANOP
         MEXIT
         COPY ZCGETFLD
         MEND
         MACRO
.*********************************************************************
.* Copyright 2008 Automated Software Tools Corporation               *
.* This source code is part of z390 zCOBOL/assembler/emulator package*
.* The z390 package is distributed under GNU general public license  *
.* Author - Don Higgins                                              *
.* Date   - 04/16/08                                                 *
.*********************************************************************
.* 01/02/09 initial coding for gobal CONFIGURATION
.*********************************************************************
         CONFIGURATION
         MEND
         MACRO
.*********************************************************************
.* Copyright 2008 Automated Software Tools Corporation               *
.* This source code is part of z390 zCOBOL/assembler/emulator package*
.* The z390 package is distributed under GNU general public license  *
.* Author - Don Higgins                                              *
.* Date   - 04/16/08                                                 *
.*********************************************************************
.* 10/28/08 initial coding for CONTINUE no operation.
.*********************************************************************
         CONTINUE
         MEND
         MACRO
.*********************************************************************
.* Copyright 2009 Automated Software Tools Corporation               *
.* This source code is part of z390 zCOBOL/assembler/emulator package*
.* The z390 package is distributed under GNU general public license  *
.* Author - Don Higgins                                              *
.* Date   - 08/02/09                                                 *
.*********************************************************************
.* 08/03/09 stub for pending support
.*********************************************************************
         CRT
         MNOTE 8,'CRT NOT SUPPORTED YET'
         MEXIT
         MEND
         MACRO
.*********************************************************************
.* Copyright 2009 Automated Software Tools Corporation               *
.* This source code is part of z390 zCOBOL/assembler/emulator package*
.* The z390 package is distributed under GNU general public license  *
.* Author - Don Higgins                                              *
.* Date   - 08/02/09                                                 *
.*********************************************************************
.* 08/03/09 stub for pending support
.*********************************************************************
         CURRENCY
         MNOTE 8,'CURRENCY NOT SUPPORTED YET'
         MEXIT
         MEND
         MACRO
.*********************************************************************
.* Copyright 2009 Automated Software Tools Corporation               *
.* This source code is part of z390 zCOBOL/assembler/emulator package*
.* The z390 package is distributed under GNU general public license  *
.* Author - Don Higgins                                              *
.* Date   - 08/02/09                                                 *
.*********************************************************************
.* 08/03/09 stub for pending support
.*********************************************************************
         CURSOR
         MNOTE 8,'CURSOR NOT SUPPORTED YET'
         MEXIT
         MEND
         MACRO
.*********************************************************************
.* Copyright 2008 Automated Software Tools Corporation               *
.* This source code is part of z390 zCOBOL/assembler/emulator package*
.* The z390 package is distributed under GNU general public license  *
.* Author.  Don Higgins                                              *
.* Date   - 04/07/08                                                 *
.*********************************************************************
.* 04/11/08 initial coding for global data division
.* 05/03/08 add DATA END to generate WS list and data definitions
.* 10/05/08 ZSTRMAC
.* 02/25/09 RPI 1002 CREATE USAGE IS INDEX FOR INDEX DATA ITEMS
.* 02/27/09 RPI 1012 GENERATED DEBUG-ITEM AT END OF WS
.*********************************************************************
         DATA  &TYPE
         COPY  ZC_WS
.*       AIF   ('&TYPE' EQ 'END')
           AIF   (NOT('&TYPE' EQ 'END')).AIF_34_1
.*             ACALL ADD_PREDEFINED_DATA
&ACALL_7_ADD_PREDEFINED_DATA SETA    1
                 AGO   .ACL_7_ADD_PREDEFINED_DATA
.ACL_7_1         ANOP
.*       AELSEIF ('&TYPE' NE 'DIVISION')
           AGO   .AIF_34_E
.AIF_34_1  ANOP
           AIF     (NOT('&TYPE' NE 'DIVISION')).AIF_34_2
               MNOTE 8,'DATA UNRECOGNIZED TYPE - &TYPE'
.*       AEND
.AIF_34_2  ANOP
.AIF_34_E  ANOP
         MEXIT
.*
.* ADD PREDEFINED WS AND LK SECTIONS
.*
.*      AENTRY ADD_PREDEFINED_DATA
          AGO   .ACL_7_SKIP
.ACL_7_ADD_PREDEFINED_DATA ANOP
&FILE_IX SETA 0   END FILE RECORD DEFINITIONS
.*      AIF    (&LK_MODE)
          AIF    (NOT(&LK_MODE)).AIF_35_1
         WS 01,ZC_LK_END,PIC,X
.*      AEND
.AIF_35_1 ANOP
.*
.* TURN OFF LK LINKAGE SECTION MODE AND RESTORE LAST WS LOC
.*
&LK_MODE SETB 0          TURN OFF LK SECTION IF ON
.*      AIF    (&LK_SAVE_WS_LOC GT 0)
          AIF    (NOT(&LK_SAVE_WS_LOC GT 0)).AIF_36_1
&WS_LOC        SETA &LK_SAVE_WS_LOC RESTORE LAST WS LOC FOR PD
.*      AEND
.AIF_36_1 ANOP
.*
.* GENERATE INDEX/SUBSCRIPT/LINKAGE POINTERS IN WS
.*
&I       SETA 1
.*       AWHILE (&I LE &SET_TOT)
.AWH_11_T  ANOP
           AIF    (NOT(&I LE &SET_TOT)).AWH_11_E
&NAME          SETC  '&SET_INDEX(&I)'
.*             AIF   ('&NAME' EQ '')
                 AIF   (NOT('&NAME' EQ '')).AIF_37_1
&NAME                SETC '&SYM_NAME(&SET_SYM_IX(&I))_PTR'
                     WS 77,&NAME,USAGE,POINTER
.*             AELSE
                 AGO   .AIF_37_E
.AIF_37_1        ANOP
                     WS 77,&NAME,USAGE,INDEX                  RPI 1002
&SYM_REF(&SYM_TOT)   SETA &SET_SYM_IX(&I) RPI 1002
.*             AEND
.AIF_37_E        ANOP
&I             SETA &I+1
.*       AEND
           AGO   .AWH_11_T
.AWH_11_E  ANOP
.*
.* ADD PREDEFINED WS ITEMS
.*
   WS 01,DEBUG_ITEM
   WS   02,DEBUG_LINE,PIC,X(6)
   WS   02,FILLER,PIC,X,VALUE,SPACE
   WS   02,DEBUG_NAME,PIC,X(30)
   WS   02,FILLER,PIC,X,VALUE,SPACE
   WS   02,DEBUG_SUB_1,PIC,S9999,SIGN,IS,LEADING,SEPARATE,CHARACTER
   WS   02,FILLER,PIC,X,VALUE,SPACE
   WS   02,DEBUG_SUB_2,PIC,S9999,SIGN,IS,LEADING,SEPARATE,CHARACTER
   WS   02,FILLER,PIC,X,VALUE,SPACE
   WS   02,DEBUG_SUB_3,PIC,S9999,SIGN,IS,LEADING,SEPARATE,CHARACTER
   WS   02,FILLER,PIC,X,VALUE,SPACE
   WS   02,DEBUG_CONTENTS,PIC,X(35)
   WS 77,RETURN_CODE,PIC,S9(4),COMP,VALUE,0
.*         AIF  (&ZC_TRACE)
             AIF  (NOT(&ZC_TRACE)).AIF_38_1
   WS 77,READY_RESET_TRACE,PIC,X,VALUE,'T'
   WS 77,FILLER,PIC,X(5)
.*         AEND
.AIF_38_1    ANOP
.*
.* END OF PREDEFINED DATA (SEE GEN_PROC_END FOR WS/LK GEN)
.*
.*       AEND
           AGO   (&ACALL_7_ADD_PREDEFINED_DATA).ACL_7_1
.ACL_7_SKIP ANOP
         MEND
         MACRO
.*********************************************************************
.* Copyright 2008 Automated Software Tools Corporation               *
.* This source code is part of z390 zCOBOL/assembler/emulator package*
.* The z390 package is distributed under GNU general public license  *
.* Author - Don Higgins                                              *
.* Date   - 04/29/08                                                 *
.*********************************************************************
.* 04/29/08 initial coding for global DATE-COMPILED
.*********************************************************************
         DATE_COMPILED &DATE
         MEND
         MACRO
.*********************************************************************
.* Copyright 2008 Automated Software Tools Corporation               *
.* This source code is part of z390 zCOBOL/assembler/emulator package*
.* The z390 package is distributed under GNU general public license  *
.* Author - Don Higgins                                              *
.* Date   - 04/29/08                                                 *
.*********************************************************************
.* 04/29/08 initial coding for global DATE-WRITTEN
.*********************************************************************
         DATE_WRITTEN &DATE
         MEND
         MACRO
.*********************************************************************
.* Copyright 2009 Automated Software Tools Corporation               *
.* This source code is part of z390 zCOBOL/assembler/emulator package*
.* The z390 package is distributed under GNU general public license  *
.* Author - Don Higgins                                              *
.* Date   - 08/02/09                                                 *
.*********************************************************************
.* 08/03/09 support DECIMAL-POINT IS COMMA
.*********************************************************************
         DECIMAL_POINT &IS,&COMMA
         COPY ZC_WS
.*       AIF   ('&IS' EQ 'IS' AND '&COMMA' EQ 'COMMA')
           AIF   (NOT('&IS' EQ 'IS' AND '&COMMA' EQ 'COMMA')).AIF_39_1
&ZC_DEC_IS_COMMA SETB 1
.*       AELSE
           AGO   .AIF_39_E
.AIF_39_1  ANOP
               MNOTE 8,'DECIMAL-POINT UNKNOWN OPTION - &IS &COMMA'
.*       AEND
.AIF_39_E  ANOP
         MEND
         MACRO
.*********************************************************************
.* Copyright 2008 Automated Software Tools Corporation               *
.* This source code is part of z390 zCOBOL/assembler/emulator package*
.* The z390 package is distributed under GNU general public license  *
.* Author - Don Higgins                                              *
.* Date   - 04/16/08                                                 *
.*********************************************************************
.* 02/26/09 stub for pending support
.*********************************************************************
         DECLARATIVES
         MNOTE 8,'DECLARATIVES NOT SUPPORTED YET'
         MEND
         MACRO
.*********************************************************************
.* Copyright 2008 Automated Software Tools Corporation               *
.* This source code is part of z390 zCOBOL/assembler/emulator package*
.* The z390 package is distributed under GNU general public license  *
.* Author - Don Higgins                                              *
.* Date   - 04/16/08                                                 *
.*********************************************************************
.* 11/17/08 stub for pending support
.*********************************************************************
         DELETE
         MNOTE 8,'DELETE NOT SUPPORTED YET'
         MEXIT
         MEND
         MACRO
.*********************************************************************
.* Copyright 2008 Automated Software Tools Corporation               *
.* This source code is part of z390 zCOBOL/assembler/emulator package*
.* The z390 package is distributed under GNU general public license  *
.* Author - Don Higgins                                              *
.* Date   - 04/16/08                                                 *
.*********************************************************************
.* 02/20/09 stub for pending support
.*********************************************************************
         DISABLE
         MNOTE 8,'DISABLE NOT SUPPORTED YET'
         MEXIT
         MEND
         MACRO
.*********************************************************************
.* Copyright 2008 Automated Software Tools Corporation               *
.* This source code is part of z390 zCOBOL/assembler/emulator package*
.* The z390 package is distributed under GNU general public license  *
.* Author - Don Higgins                                              *
.* Date   - 04/07/08                                                 *
.*********************************************************************
.* 04/11/08 initial coding for global DISPLAY 'text' or label
.* 11/19/08 add support for multiple field types using DISPLAY routine
.* 07/25/08 RPI 1065 check for missing parms
.*********************************************************************
         DISPLAY
&PARMS   SETC '&SYSLIST(1)'
&NP      SETA N'&SYSLIST
.*       AIF   (&NP EQ 0)
           AIF   (NOT(&NP EQ 0)).AIF_40_1
               MNOTE 8,'DISPLAY MISSING OPERAND'
               MEXIT
.*       AEND
.AIF_40_1  ANOP
&I       SETA 2
.*       AWHILE (&I LE &NP)
.AWH_12_T  ANOP
           AIF    (NOT(&I LE &NP)).AWH_12_E
.*             AIF ('&SYSLIST(&I)' NE 'UPON')
                 AIF (NOT('&SYSLIST(&I)' NE 'UPON')).AIF_41_1
&PARMS             SETC '&PARMS,&SYSLIST(&I)'
&I                 SETA &I+1
.*             AELSE
                 AGO   .AIF_41_E
.AIF_41_1        ANOP
&I                 SETA &NP+1
.*             AEND
.AIF_41_E        ANOP
.*       AEND
           AGO   .AWH_12_T
.AWH_12_E  ANOP
         GEN_DISPLAY &PARMS
         MEND
         MACRO
.*********************************************************************
.* Copyright 2008 Automated Software Tools Corporation               *
.* This DR code is part of z390 zCOBOL/assembler/emulator package*
.* The z390 package is distributed under GNU general public license  *
.* Author - Don Higgins                                              *
.* Date   - 04/07/08                                                 *
.*********************************************************************
.* 10/30/08 initial coding for global MULTIPLY using ADD as model
.* 02/16/09 upgrade using ZCGETFLD to support subscripts and qual.
.* 02/26/09 RPI 1012 move onsize end-add pending to DIVIDE
.* 07/19/09 RPI 1065 parse both ROUNDED and REMAINING after GIVING
.* 07/19/09 RPI 1062 correct ON SIZE parsing
.*********************************************************************
         DIVIDE
         COPY  ZC_WS
&PARM_IX SETA 1
&FIELD_REG SETC 'ZC_R1'
&FIELD_SS1 SETB 0
.*       ACALL GET_PARM_FIELD
&ACALL_6_GET_PARM_FIELD SETA    6
           AGO   .ACL_6_GET_PARM_FIELD
.ACL_6_6   ANOP
&NDR     SETC '&FIELD_NAME'                NAME   DIVISOR
&XDR     SETA &FIELD_IX                    INDEX  DIVISOR
.*       AIF   (&XDR GT 0)
           AIF   (NOT(&XDR GT 0)).AIF_42_1
&TDR           SETC '&SYM_PIC_TYPE(&XDR)'  TYPE   DIVISOR
&LDR           SETA &SYM_LEN(&XDR)         LENGTH DIVISOR
&SDR           SETB &SYM_PIC_SIGN(&XDR)    SIGN   DIVISOR
&PDR           SETA &SYM_PIC_DEC(&XDR)     DEC-PL DIVISOR
.*       AELSEIF ('&FIELD_NAME'(1,1) LT '0'                            X
               AND '&FIELD_NAME'(1,1) NE '+'                           X
               AND '&FIELD_NAME'(1,1) NE '-'                           X
               AND '&FIELD_NAME'(1,1) NE '.')
           AGO   .AIF_42_E
.AIF_42_1  ANOP
           AIF     (NOT('&FIELD_NAME'(1,1) LT '0'                      X
                     AND '&FIELD_NAME'(1,1) NE '+'                     X
                     AND '&FIELD_NAME'(1,1) NE '-'                     X
                     AND '&FIELD_NAME'(1,1) NE '.')).AIF_42_2
               MNOTE 8,'DIVIDE FIELD NOT DEFINED - &FIELD_NAME'
               MEXIT
.*       AELSE
           AGO   .AIF_42_E
.AIF_42_2  ANOP
&TDR           SETC ''  DON'T KNOW YET
&LDR           SETA 0   DON'T KNOW YET
&SDR           SETB 0   DON'T KNOW YET
&PDR           SETA 0   DON'T KNOW YET
.*       AEND
.AIF_42_E  ANOP
&INTO_BY SETC '&SYSLIST(&PARM_IX)'
.*       AIF   ('&INTO_BY' NE 'INTO' AND '&INTO_BY' NE 'BY')
           AIF   (NOT('&INTO_BY' NE 'INTO' AND '&INTO_BY' NE 'BY')).AIFX
               _43_1
               MNOTE 8,'DIVIDE MISSING INTO OR BY'
               MEXIT
.*       AEND
.AIF_43_1  ANOP
&PARM_IX SETA &PARM_IX+1
&FIELD_REG SETC 'ZC_R2'
&FIELD_SS1 SETB 0
.*       ACALL GET_PARM_FIELD                    NAME   DIVIDEND
&ACALL_6_GET_PARM_FIELD SETA    7
           AGO   .ACL_6_GET_PARM_FIELD
.ACL_6_7   ANOP
&NDD     SETC '&FIELD_NAME'                INDEX  DIVIDEND
&XDD     SETA &FIELD_IX
.*       AIF   (&XDD GT 0)
           AIF   (NOT(&XDD GT 0)).AIF_44_1
&TDD           SETC '&SYM_PIC_TYPE(&XDD)'  TYPE   DIVIDEND
&LDD           SETA &SYM_LEN(&XDD)         LENGTH DIVIDEND
&SDD           SETB &SYM_PIC_SIGN(&XDD)    SIGN   DIVIDEND
&PDD           SETA &SYM_PIC_DEC(&XDD)     DEC-PL DIVIDEND
.*       AELSEIF ('&FIELD_NAME'(1,1) LT '0'                            X
               AND '&FIELD_NAME'(1,1) NE '+'                           X
               AND '&FIELD_NAME'(1,1) NE '-'                           X
               AND '&FIELD_NAME'(1,1) NE '.')
           AGO   .AIF_44_E
.AIF_44_1  ANOP
           AIF     (NOT('&FIELD_NAME'(1,1) LT '0'                      X
                     AND '&FIELD_NAME'(1,1) NE '+'                     X
                     AND '&FIELD_NAME'(1,1) NE '-'                     X
                     AND '&FIELD_NAME'(1,1) NE '.')).AIF_44_2
               MNOTE 8,'DIVIDE FIELD NOT DEFINED - &FIELD_NAME'
               MEXIT
.*       AELSE
           AGO   .AIF_44_E
.AIF_44_2  ANOP
&TDD           SETC ''  DON'T KNOW YET
&LDD           SETA 0   DON'T KNOW YET
&SDD           SETB 0   DON'T KNOW YET
&PDD           SETA 0   DON'T KNOW YET
.*       AEND
.AIF_44_E  ANOP
.*       AIF   ('&TDD' EQ '' AND '&TDR' NE '') COPY DR TYPE FOR DD LIT
           AIF   (NOT('&TDD' EQ '' AND '&TDR' NE '')).AIF_45_1  COPY DRX
                TYPE FOR DD LIT
&TDD           SETC '&TDR'
&LDD           SETA &LDR
&SDD           SETB &SDR
.*             ACALL SET_LIT_PDD
&ACALL_8_SET_LIT_PDD SETA    1
                 AGO   .ACL_8_SET_LIT_PDD
.ACL_8_1         ANOP
.*       AEND
.AIF_45_1  ANOP
.*       AIF   ('&TDR' EQ '' AND '&TDD' NE '') COPY DD TYPE FOR DR LIT
           AIF   (NOT('&TDR' EQ '' AND '&TDD' NE '')).AIF_46_1  COPY DDX
                TYPE FOR DR LIT
&TDR           SETC '&TDD'
&LDR           SETA &LDD
&SDR           SETB &SDD
.*             ACALL SET_LIT_PDR
&ACALL_9_SET_LIT_PDR SETA    1
                 AGO   .ACL_9_SET_LIT_PDR
.ACL_9_1         ANOP
.*       AEND
.AIF_46_1  ANOP
.*       AIF   ('&INTO_BY' EQ 'BY')  SWAP DIVIDEND AND DIVIOR
           AIF   (NOT('&INTO_BY' EQ 'BY')).AIF_47_1   SWAP DIVIDEND ANDX
                DIVIOR
&TN            SETC '&NDD'
&NDD           SETC '&NDR'
&NDR           SETC '&TN'
&TX            SETA &XDD
&XDD           SETA &XDR
&XDR           SETA &TX
&TT            SETC '&TDD'
&TDD           SETC '&TDR'
&TDR           SETC '&TT'
&TL            SETA &LDD
&LDD           SETA &LDR
&LDR           SETA &TL
&TS            SETB &SDD
&SDD           SETB &SDR
&SDR           SETB &TS
&TP            SETA &PDD
&PDD           SETA &PDR
&PDR           SETA &TP
.*       AEND
.AIF_47_1  ANOP
&NR      SETC ''   ASSUME NO REMAINDER STORED
&XR      SETA 0
&TR      SETC ''
&LR      SETA 0
&SR      SETB 0
&PR      SETA 0
.*       AIF   ('&SYSLIST(&PARM_IX)' EQ 'GIVING')
           AIF   (NOT('&SYSLIST(&PARM_IX)' EQ 'GIVING')).AIF_48_1
&PARM_IX       SETA &PARM_IX+1
.*             ACALL DIVIDE_GIVING
&ACALL_10_DIVIDE_GIVING SETA    1
                 AGO   .ACL_10_DIVIDE_GIVING
.ACL_10_1        ANOP
.*       AELSE
           AGO   .AIF_48_E
.AIF_48_1  ANOP
.*             ACALL DIVIDE_INTO_BY
&ACALL_11_DIVIDE_INTO_BY SETA    1
                 AGO   .ACL_11_DIVIDE_INTO_BY
.ACL_11_1        ANOP
.*       AEND
.AIF_48_E  ANOP
         MEXIT
.*
.* DIVIDE GIVING
.*
.*       AENTRY DIVIDE_GIVING
           AGO   .ACL_10_SKIP
.ACL_10_DIVIDE_GIVING ANOP
&FIELD_REG SETC 'ZC_R3'
&FIELD_SS1 SETB 0
.*       ACALL GET_PARM_FIELD
&ACALL_6_GET_PARM_FIELD SETA    8
           AGO   .ACL_6_GET_PARM_FIELD
.ACL_6_8   ANOP
&NQ      SETC '&FIELD_NAME'                 NAME   QUOTIENT
&XQ      SETA &FIELD_IX                     INDEX  QUOTIENT
.*       AIF   (&XQ EQ 0)
           AIF   (NOT(&XQ EQ 0)).AIF_49_1
                MNOTE 8,'DIVIDE UNDEFINED QUOTIENT - &NQ'
                MEXIT
.*       AEND
.AIF_49_1  ANOP
&TQ      SETC '&SYM_PIC_TYPE(&XQ)'          TYPE   QUOTIENT
&LQ      SETA &SYM_LEN(&XQ)                 LENGTH QUOTIENT
&SQ      SETB &SYM_PIC_SIGN(&XQ)            SIGN   QUOTIENT
&PQ      SETA &SYM_PIC_DEC(&XQ)             DEC-PL QUOTIENT
.*       AIF ('&TDR' EQ '')   COPY QUOTIENT TYPE IF DIVISOR LIT
           AIF (NOT('&TDR' EQ '')).AIF_50_1    COPY QUOTIENT TYPE IF DIX
               VISOR LIT
&TDR         SETC '&TQ'
&LDR         SETA &LQ
&SDR         SETB &SQ
.*           ACALL SET_LIT_PDR
&ACALL_9_SET_LIT_PDR SETA    2
               AGO   .ACL_9_SET_LIT_PDR
.ACL_9_2       ANOP
.*       AEND
.AIF_50_1  ANOP
.*       AIF ('&TDD' EQ '')   COPY QUOTIENT TYPE IF DIVIDEND LIT
           AIF (NOT('&TDD' EQ '')).AIF_51_1    COPY QUOTIENT TYPE IF DIX
               VIDEND LIT
&TDD         SETC '&TQ'
&LDD         SETA &LQ
&SDD         SETB &SQ
.*           ACALL SET_LIT_PDD
&ACALL_8_SET_LIT_PDD SETA    2
               AGO   .ACL_8_SET_LIT_PDD
.ACL_8_2       ANOP
.*       AEND
.AIF_51_1  ANOP
.*       AIF   ('&SYSLIST(&PARM_IX)' EQ 'ROUNDED')  RPI 1062 RND+REM
           AIF   (NOT('&SYSLIST(&PARM_IX)' EQ 'ROUNDED')).AIF_52_1   RPX
               I 1062 RND+REM
&ROUNDED       SETB 1
&PARM_IX       SETA &PARM_IX+1
.*       AELSE
           AGO   .AIF_52_E
.AIF_52_1  ANOP
&ROUNDED       SETB 0
.*       AEND
.AIF_52_E  ANOP
.*       AIF   ('&SYSLIST(&PARM_IX)' EQ 'REMAINDER')
           AIF   (NOT('&SYSLIST(&PARM_IX)' EQ 'REMAINDER')).AIF_53_1
.*             AIF ('&TQ' GE '1' AND '&TQ' LE '9')
                 AIF (NOT('&TQ' GE '1' AND '&TQ' LE '9')).AIF_54_1
                   MNOTE 8,'DIVIDE REMAINDER INVALID FOR HFP, BFP, DFP'
                   MEXIT
.*             AEND
.AIF_54_1        ANOP
&PARM_IX       SETA &PARM_IX+1
&FIELD_REG     SETC 'ZC_R14'
&FIELD_SS1     SETB 0
.*             ACALL GET_PARM_FIELD
&ACALL_6_GET_PARM_FIELD SETA    9
                 AGO   .ACL_6_GET_PARM_FIELD
.ACL_6_9         ANOP
&NR            SETC '&FIELD_NAME'                 NAME   REMAINDER
&XR            SETA &FIELD_IX                     INDEX  REMAINDER
.*             AIF   (&XR EQ 0)
                 AIF   (NOT(&XR EQ 0)).AIF_55_1
                      MNOTE 8,'DIVIDE UNDEFINED REMAINDER - &NR'
                      MEXIT
.*             AEND
.AIF_55_1        ANOP
&TR            SETC '&SYM_PIC_TYPE(&XR)'          TYPE   REMAINDER
&LR            SETA &SYM_LEN(&XR)                 LENGTH REMAINDER
&SR            SETB &SYM_PIC_SIGN(&XR)            SIGN   REMAINDER
&PR            SETA &SYM_PIC_DEC(&XR)             DEC-PL REMAINDER
.*       AEND
.AIF_53_1  ANOP
.*       ACALL OPTIONS_GEN_DIV_MOVE
&ACALL_12_OPTIONS_GEN_DIV_MOVE SETA    1
           AGO   .ACL_12_OPTIONS_GEN_DIV_MOVE
.ACL_12_1  ANOP
.*       AEND
           AGO   (&ACALL_10_DIVIDE_GIVING).ACL_10_1
.ACL_10_SKIP ANOP
.*
.* DIVIDE INTO BY - REPLACE ONE OR MORE DIVIDENDS WITH QUOTIENT
.*
.*       AENTRY DIVIDE_INTO_BY
           AGO   .ACL_11_SKIP
.ACL_11_DIVIDE_INTO_BY ANOP
.*       AIF   (&XDD EQ 0)
           AIF   (NOT(&XDD EQ 0)).AIF_56_1
               MNOTE 8,'LITERAL DIVIDEND REQUIRES GIVING'
               MEXIT
.*       AEND
.AIF_56_1  ANOP
&NQ      SETC '&NDD'  STORE QUOTIENT IN DIVIDEND
&XQ      SETA &XDD                    INDEX  QUOTIENT
&TQ      SETC '&TDD'                  TYPE   QUOTIENT
&LQ      SETA &LDD                    LENGTH QUOTIENT
&SQ      SETB &SDD                    SIGN   QUOTIENT
&PQ      SETA &PDD                    DEC-PL QUOTIENT
.*       ACALL OPTIONS_GEN_DIV_MOVE
&ACALL_12_OPTIONS_GEN_DIV_MOVE SETA    2
           AGO   .ACL_12_OPTIONS_GEN_DIV_MOVE
.ACL_12_2  ANOP
.*       AEND
           AGO   (&ACALL_11_DIVIDE_INTO_BY).ACL_11_1
.ACL_11_SKIP ANOP
.*
.* OPTIONS GEN DIV MOVE - SET ON SIZE ERROR OPTIONS, GEN DIV, MOVE DUP
.*
.*       AENTRY OPTIONS_GEN_DIV_MOVE
           AGO   .ACL_12_SKIP
.ACL_12_OPTIONS_GEN_DIV_MOVE ANOP
&MOVE_IX SETA &PARM_IX
.*       ACALL SET_OPTIONS
&ACALL_13_SET_OPTIONS SETA    1
           AGO   .ACL_13_SET_OPTIONS
.ACL_13_1  ANOP
.*       ACALL CALL_GEN_DIV
&ACALL_14_CALL_GEN_DIV SETA    1
           AGO   .ACL_14_CALL_GEN_DIV
.ACL_14_1  ANOP
&PARM_IX SETA &MOVE_IX
.*       AWHILE (&PARM_IX LE &LAST_PARM)
.AWH_13_T  ANOP
           AIF    (NOT(&PARM_IX LE &LAST_PARM)).AWH_13_E
               ACTR 4096
&FIELD_REG     SETC 'ZC_R1'
&FIELD_SS1     SETB 1
.*             ACALL GET_PARM_FIELD
&ACALL_6_GET_PARM_FIELD SETA    10
                 AGO   .ACL_6_GET_PARM_FIELD
.ACL_6_10        ANOP
               GEN_MOVE &FIELD_NAME,&FIELD_IX,&NQ,&XQ
.*       AEND
           AGO   .AWH_13_T
.AWH_13_E  ANOP
.*       AEND
           AGO   (&ACALL_12_OPTIONS_GEN_DIV_MOVE).ACL_12_1,.ACL_12_2
.ACL_12_SKIP ANOP
.*
.* SET OPTIONS - ROUND AND ON SIZE ERROR
.*
.*       AENTRY SET_OPTIONS
           AGO   .ACL_13_SKIP
.ACL_13_SET_OPTIONS ANOP
&ROUNDED SETB 0
&ONSIZE  SETB 0
&NOT_ONSIZE SETB 0
&LAST_PARM SETA 0
.*       AWHILE (&PARM_IX LE N'&SYSLIST)
.AWH_14_T  ANOP
           AIF    (NOT(&PARM_IX LE N'&SYSLIST)).AWH_14_E
               ACTR 4096
.*             AIF     ('&SYSLIST(&PARM_IX)' EQ 'ON'                   X
                        AND '&SYSLIST(&PARM_IX+1)' EQ 'SIZE'           X
                        AND '&SYSLIST(&PARM_IX+2)' EQ 'ERROR') RPI 1065
                 AIF     (NOT('&SYSLIST(&PARM_IX)' EQ 'ON'             X
                              AND '&SYSLIST(&PARM_IX+1)' EQ 'SIZE'     X
                              AND '&SYSLIST(&PARM_IX+2)' EQ 'ERROR')).AX
               IF_57_1  RPI 1065
&ONSIZE              SETB 1
.*                   AIF (&LAST_PARM EQ 0)
                       AIF (NOT(&LAST_PARM EQ 0)).AIF_58_1
&LAST_PARM               SETA &PARM_IX-1
.*                   AEND
.AIF_58_1              ANOP
&PARM_IX             SETA N'&SYSLIST+2
.*             AELSEIF ('&SYSLIST(&PARM_IX)' EQ 'NOT'                  X
                        AND '&SYSLIST(&PARM_IX+1)' EQ 'ON'             X
                        AND '&SYSLIST(&PARM_IX+2)' EQ 'SIZE'           X
                        AND '&SYSLIST(&PARM_IX+3)' EQ 'ERROR') RPI 1065
                 AGO   .AIF_57_E
.AIF_57_1        ANOP
                 AIF     (NOT('&SYSLIST(&PARM_IX)' EQ 'NOT'            X
                              AND '&SYSLIST(&PARM_IX+1)' EQ 'ON'       X
                              AND '&SYSLIST(&PARM_IX+2)' EQ 'SIZE'     X
                              AND '&SYSLIST(&PARM_IX+3)' EQ 'ERROR')).AX
               IF_57_2  RPI 1065
&NOT_ONSIZE          SETB 1
.*                   AIF (&LAST_PARM EQ 0)
                       AIF (NOT(&LAST_PARM EQ 0)).AIF_59_1
&LAST_PARM               SETA &PARM_IX-1
.*                   AEND
.AIF_59_1              ANOP
&PARM_IX             SETA N'&SYSLIST+3
.*             AELSEIF ('&SYSLIST(&PARM_IX)' EQ 'REMAINDER')
                 AGO   .AIF_57_E
.AIF_57_2        ANOP
                 AIF     (NOT('&SYSLIST(&PARM_IX)' EQ 'REMAINDER')).AIFX
               _57_3
                     MNOTE 8,'DIVIDE MULT GIVING WITH REMAINDER ERROR'
                     MEXIT
.*             AEND
.AIF_57_3        ANOP
.AIF_57_E        ANOP
&PARM_IX       SETA &PARM_IX+1
.*       AEND
           AGO   .AWH_14_T
.AWH_14_E  ANOP
.*       AIF (&LAST_PARM EQ 0)
           AIF (NOT(&LAST_PARM EQ 0)).AIF_60_1
&LAST_PARM   SETA N'&SYSLIST
.*       AEND
.AIF_60_1  ANOP
.*       AIF   (&ONSIZE)
           AIF   (NOT(&ONSIZE)).AIF_61_1
&IE_LVL        SETA  &IE_LVL+1         ADD PENDING END_DIV
&IE_TYPE(&IE_LVL) SETA 14        FOR END_DIV OR PERIOD
&DIV_LAB       SETA &DIV_LAB+1
&IE_TCNT(&IE_LVL) SETA &DIV_LAB
.*       AEND
.AIF_61_1  ANOP
.*       AEND
           AGO   (&ACALL_13_SET_OPTIONS).ACL_13_1
.ACL_13_SKIP ANOP
.*
.* CALL GEN DIV
.*
.*       AENTRY CALL_GEN_DIV
           AGO   .ACL_14_SKIP
.ACL_14_CALL_GEN_DIV ANOP
.*       AIF   ('&SYSLIST(&PARM_IX)' EQ 'ROUNDED')
           AIF   (NOT('&SYSLIST(&PARM_IX)' EQ 'ROUNDED')).AIF_62_1
&ROUNDED       SETB 1
&PARM_IX       SETA &PARM_IX+1
.*       AELSE
           AGO   .AIF_62_E
.AIF_62_1  ANOP
&ROUNDED       SETB 0
.*       AEND
.AIF_62_E  ANOP
       GEN_DIV &NDD,&TDD,&LDD,&PDD, DIVIDEND  NAME/LIT,TYPE,LEN,PLACES X
               &NDR,&TDR,&LDR,&PDR, DIVISOR   NAME/LIT,TYPE,LEN,PLACES X
               &NQ,&TQ,&LQ,&SQ,&PQ, QUOTIENT  NAME,TYPE,LEN,SIGN,PLCS  X
               &NR,&TR,&LR,&SR,&PR, REMAINDER NAME,TYPE,LEN,SIGN,PLCS  X
               &ROUNDED,       1 IF ROUNDED SPECIFIED                  X
               &ONSIZE,       1 IF ON SIZE SPECIFIED                   X
               &NOT_ONSIZE    1 IF NOT_ONSIZE SPECIFIED
.*       AEND
           AGO   (&ACALL_14_CALL_GEN_DIV).ACL_14_1
.ACL_14_SKIP ANOP
.*
.* SET LIT LDD=0 AND PDD
.*
.*       AENTRY SET_LIT_PDD
           AGO   .ACL_8_SKIP
.ACL_8_SET_LIT_PDD ANOP
&LDD     SETA 0
&DP      SETA ('&NDD' INDEX '.')
.*       AIF  (&DP GT 0)
           AIF  (NOT(&DP GT 0)).AIF_63_1
&PDD          SETA K'&NDD-&DP
.*       AELSE
           AGO   .AIF_63_E
.AIF_63_1  ANOP
&PDD          SETA 0
.*       AEND
.AIF_63_E  ANOP
.*       AEND
           AGO   (&ACALL_8_SET_LIT_PDD).ACL_8_1,.ACL_8_2
.ACL_8_SKIP ANOP
.*
.* SET LIT LDR=0 AND PDR
.*
.*       AENTRY SET_LIT_PDR
           AGO   .ACL_9_SKIP
.ACL_9_SET_LIT_PDR ANOP
&LDR     SETA 0
&DP      SETA ('&NDR' INDEX '.')
.*       AIF  (&DP GT 0)
           AIF  (NOT(&DP GT 0)).AIF_64_1
&PDR          SETA K'&NDR-&DP
.*       AELSE
           AGO   .AIF_64_E
.AIF_64_1  ANOP
&PDR          SETA 0
.*       AEND
.AIF_64_E  ANOP
.*       AEND
           AGO   (&ACALL_9_SET_LIT_PDR).ACL_9_1,.ACL_9_2
.ACL_9_SKIP ANOP
         COPY ZCGETFLD
         MEND
         MACRO
.*********************************************************************
.* Copyright 2008 Automated Software Tools Corporation               *
.* This source code is part of z390 zCOBOL/assembler/emulator package*
.* The z390 package is distributed under GNU general public license  *
.* Author - Don Higgins                                              *
.* Date   - 04/15/08                                                 *
.*********************************************************************
.* 06/13/08 compiler directive ignored for now
.*********************************************************************
         EJECT
         MEND

         MACRO
.*********************************************************************
.* Copyright 2008 Automated Software Tools Corporation               *
.* This source code is part of z390 zCOBOL/assembler/emulator package*
.* The z390 package is distributed under GNU general public license  *
.* Author - Don Higgins                                              *
.* Date   - 04/07/08                                                 *
.*********************************************************************
.* 04/09/08 initial coding for global ELSE
.* 10/05/08 ZSTRMAC
.*********************************************************************
         ELSE
         COPY  ZC_WS
.*       AIF   (N'&SYSLIST GT 0)
           AIF   (NOT(N'&SYSLIST GT 0)).AIF_65_1
               MNOTE 8,'ELSE - UNDEFINED VERB - &SYSLIST(1)'
.*       AEND
.AIF_65_1  ANOP
.*       AIF   (&IE_LVL LT 1)
           AIF   (NOT(&IE_LVL LT 1)).AIF_66_1
               MNOTE 8,'ELSE MISSING IF'
               MEXIT
.*       AEND
.AIF_66_1  ANOP
.*       AIF   (&IE_TYPE(&IE_LVL) NE 1)
           AIF   (NOT(&IE_TYPE(&IE_LVL) NE 1)).AIF_67_1
               MNOTE 8,'ELSE MISSING IF'
               MEXIT
.*       AEND
.AIF_67_1  ANOP
.*       AWHILE (&IE_ELSE(&IE_LVL))
.AWH_15_T  ANOP
           AIF    (NOT(&IE_ELSE(&IE_LVL))).AWH_15_E
.*             AIF (&IE_LVL GT 1)
                 AIF (NOT(&IE_LVL GT 1)).AIF_68_1
                   END_IF
.*             AELSE
                 AGO   .AIF_68_E
.AIF_68_1        ANOP
                   MNOTE 8,'ELSE DUPLICATE FOR SAME IF'
                   MEXIT
.*             AEND
.AIF_68_E        ANOP
.*       AEND
           AGO   .AWH_15_T
.AWH_15_E  ANOP
         GEN_B IF_&IE_TCNT(&IE_LVL)._E
&IE_ELSE(&IE_LVL) SETB 1
         GEN_LABEL IF_&IE_TCNT(&IE_LVL)_&IE_BCNT(&IE_LVL),ELSE
         MEND
         MACRO
.*********************************************************************
.* Copyright 2008 Automated Software Tools Corporation               *
.* This source code is part of z390 zCOBOL/assembler/emulator package*
.* The z390 package is distributed under GNU general public license  *
.* Author - Don Higgins                                              *
.* Date   - 04/16/08                                                 *
.*********************************************************************
.* 02/20/09 stub for pending support
.*********************************************************************
         ENABLE
         MNOTE 8,'ENABLE NOT SUPPORTED YET'
         MEXIT
         MEND
         MACRO
.*********************************************************************
.* Copyright 2008 Automated Software Tools Corporation               *
.* This source code is part of z390 zCOBOL/assembler/emulator package*
.* The z390 package is distributed under GNU general public license  *
.* Author - Don Higgins                                              *
.* Date   - 04/15/08                                                 *
.*********************************************************************
.* 01/11/09 initial coding
.*********************************************************************
         END_ADD
         COPY  ZC_WS
.*       AIF   (&IE_LVL LT 1)
           AIF   (NOT(&IE_LVL LT 1)).AIF_69_1
               MEXIT
.*       AEND
.AIF_69_1  ANOP
.*       AIF   (&IE_TYPE(&IE_LVL) EQ 11) IS THERE ON SIZE/ERROR
           AIF   (NOT(&IE_TYPE(&IE_LVL) EQ 11)).AIF_70_1  IS THERE ON SX
               IZE/ERROR
               GEN_LABEL ADD_&IE_TCNT(&IE_LVL),END_ADD
&IE_LVL        SETA  &IE_LVL-1
.*       AEND
.AIF_70_1  ANOP
         MEND
         MACRO
.*********************************************************************
.* Copyright 2008 Automated Software Tools Corporation               *
.* This source code is part of z390 zCOBOL/assembler/emulator package*
.* The z390 package is distributed under GNU general public license  *
.* Author - Don Higgins                                              *
.* Date   - 04/16/08                                                 *
.*********************************************************************
.* 02/26/09 stub for pending support
.*********************************************************************
         END_DECLARATIVES
         MNOTE 8,'END_DECLARATIVES NOT SUPPORTED YET'
         MEND
         MACRO
.*********************************************************************
.* Copyright 2008 Automated Software Tools Corporation               *
.* This source code is part of z390 zCOBOL/assembler/emulator package*
.* The z390 package is distributed under GNU general public license  *
.* Author - Don Higgins                                              *
.* Date   - 04/15/08                                                 *
.*********************************************************************
.* 01/11/09 initial coding
.*********************************************************************
         END_DIVIDE
         COPY  ZC_WS
.*       AIF   (&IE_LVL LT 1)
           AIF   (NOT(&IE_LVL LT 1)).AIF_71_1
               MEXIT
.*       AEND
.AIF_71_1  ANOP
.*       AIF   (&IE_TYPE(&IE_LVL) EQ 14) IS THERE ON SIZE/ERROR
           AIF   (NOT(&IE_TYPE(&IE_LVL) EQ 14)).AIF_72_1  IS THERE ON SX
               IZE/ERROR
               GEN_LABEL DIVIDE_&IE_TCNT(&IE_LVL),END_DIVIDE
&IE_LVL        SETA  &IE_LVL-1
.*       AEND
.AIF_72_1  ANOP
         MEND
         MACRO
.*********************************************************************
.* Copyright 2008 Automated Software Tools Corporation               *
.* This source code is part of z390 zCOBOL/assembler/emulator package*
.* The z390 package is distributed under GNU general public license  *
.* All rights reserved                                               *
.* Author - Don Higgins                                              *
.* Date   - 04/07/08                                                 *
.*********************************************************************
.* 04/09/08 initial coding for global END_EVALUATE
.* 10/05/08 ZSTRMAC
.*********************************************************************
         END_EVALUATE
         COPY  ZC_WS
.*       AIF   (&IE_LVL LT 1)
           AIF   (NOT(&IE_LVL LT 1)).AIF_73_1
               MNOTE 8,'END-EVALUATE MISSING EVALUATE'
               MEXIT
.*       AEND
.AIF_73_1  ANOP
.*       AIF   (&IE_TYPE(&IE_LVL) NE 2)
           AIF   (NOT(&IE_TYPE(&IE_LVL) NE 2)).AIF_74_1
               MNOTE 8,'END-EVALUATE MISSING EVALUATE'
               MNOTE 8,'END-EVALUATE MISSING EVALUATE'
.*       AEND
.AIF_74_1  ANOP
&NEXT_WHEN SETA  &IE_WHEN(&IE_LVL)+1
         GEN_LABEL EVAL&IE_EVAL(&IE_LVL)._WHEN&NEXT_WHEN,END_WHEN
         GEN_LABEL EVAL&IE_EVAL(&IE_LVL)._END,END_EVALUATE
&IE_LVL  SETA &IE_LVL-1
         MEND
         MACRO
.*********************************************************************
.* Copyright 2008 Automated Software Tools Corporation               *
.* This source code is part of z390 zCOBOL/assembler/emulator package*
.* The z390 package is distributed under GNU general public license  *
.* Author - Don Higgins                                              *
.* Date   - 04/07/08                                                 *
.*********************************************************************
.* 04/09/08 initial coding for global END_IF
.* 10/05/08 ZSTRMAC
.*********************************************************************
         END_IF
         COPY  ZC_WS
.*       AIF   (&IE_LVL LT 1)
           AIF   (NOT(&IE_LVL LT 1)).AIF_75_1
               MNOTE 8,'END_IF MISSING IF'
               MEXIT
.*       AEND
.AIF_75_1  ANOP
.*       AIF   (&IE_ELSE(&IE_LVL))
           AIF   (NOT(&IE_ELSE(&IE_LVL))).AIF_76_1
               GEN_LABEL IF_&IE_TCNT(&IE_LVL)_E,END_IF
.*       AELSE
           AGO   .AIF_76_E
.AIF_76_1  ANOP
               GEN_LABEL IF_&IE_TCNT(&IE_LVL)_&IE_BCNT(&IE_lVL),END_IF
.*       AEND
.AIF_76_E  ANOP
&IE_LVL  SETA  &IE_LVL-1
         MEND
         MACRO
.*********************************************************************
.* Copyright 2008 Automated Software Tools Corporation               *
.* This source code is part of z390 zCOBOL/assembler/emulator package*
.* The z390 package is distributed under GNU general public license  *
.* Author - Don Higgins                                              *
.* Date   - 04/15/08                                                 *
.*********************************************************************
.* 01/11/09 initial coding
.*********************************************************************
         END_MULTIPLY
         COPY  ZC_WS
.*       AIF   (&IE_LVL LT 1)
           AIF   (NOT(&IE_LVL LT 1)).AIF_77_1
               MEXIT
.*       AEND
.AIF_77_1  ANOP
.*       AIF   (&IE_TYPE(&IE_LVL) EQ 13) IS THERE ON SIZE/ERROR
           AIF   (NOT(&IE_TYPE(&IE_LVL) EQ 13)).AIF_78_1  IS THERE ON SX
               IZE/ERROR
               GEN_LABEL MULTIPLY_&IE_TCNT(&IE_LVL),END_MULTIPLY
&IE_LVL        SETA  &IE_LVL-1
.*       AEND
.AIF_78_1  ANOP
         MEND
         MACRO
.*********************************************************************
.* Copyright 2008 Automated Software Tools Corporation               *
.* This source code is part of z390 zCOBOL/assembler/emulator package*
.* The z390 package is distributed under GNU general public license  *
.* Author - Don Higgins                                              *
.* Date   - 04/07/08                                                 *
.*********************************************************************
.* 11/26/08 initial coding for global END_PERFORM
.*********************************************************************
         END_PERFORM
         COPY  ZC_WS
.*       AIF   (&IE_LVL LT 1)
           AIF   (NOT(&IE_LVL LT 1)).AIF_79_1
               MEXIT
.*       AEND
.AIF_79_1  ANOP
.*       AIF   (&IE_TYPE(&IE_LVL) NE 4)
           AIF   (NOT(&IE_TYPE(&IE_LVL) NE 4)).AIF_80_1
               MEXIT
.*       AEND
.AIF_80_1  ANOP
         GEN_B &IE_PM_LAB(&IE_LVL)
         GEN_LABEL PM_&IE_TCNT(&IE_LVL)_E,END_PERFORM
&IE_LVL  SETA  &IE_LVL-1
         MEND
         MACRO
.*********************************************************************
.* Copyright 2008 Automated Software Tools Corporation               *
.* This source code is part of z390 zCOBOL/assembler/emulator package*
.* The z390 package is distributed under GNU general public license  *
.* Author - Don Higgins                                              *
.* Date   - 04/15/08                                                 *
.*********************************************************************
.* 04/15/08 initial coding for global END_READ to termainate AT END
.* 10/05/08 ZSTRMAC
.*********************************************************************
         END_READ
         COPY  ZC_WS
.*       AIF   (&IE_LVL LT 1)
           AIF   (NOT(&IE_LVL LT 1)).AIF_81_1
               MNOTE 8,'END_READ MISSING READ'
               MEXIT
.*       AEND
.AIF_81_1  ANOP
.*       AIF   (&IE_TYPE(&IE_LVL) NE 3)
           AIF   (NOT(&IE_TYPE(&IE_LVL) NE 3)).AIF_82_1
               MNOTE 8,'END-READ INVALID TYPE - &IE_TYPE(&IE_LVL)'
               MEXIT
.*       AEND
.AIF_82_1  ANOP
         GEN_LABEL &FILE_NAME(&FILE_IX)._END_READ&IE_TCNT(&IE_LVL),END_X
               READ
&IE_LVL  SETA  &IE_LVL-1
         MEND
         MACRO
.*********************************************************************
.* Copyright 2008 Automated Software Tools Corporation               *
.* This source code is part of z390 zCOBOL/assembler/emulator package*
.* The z390 package is distributed under GNU general public license  *
.* Author - Don Higgins                                              *
.* Date   - 04/15/08                                                 *
.*********************************************************************
.* 01/11/09 initial coding
.*********************************************************************
         END_SUBTRACT
         COPY  ZC_WS
.*       AIF   (&IE_LVL LT 1)
           AIF   (NOT(&IE_LVL LT 1)).AIF_83_1
               MEXIT
.*       AEND
.AIF_83_1  ANOP
.*       AIF   (&IE_TYPE(&IE_LVL) EQ 12) IS THERE ON SIZE/ERROR
           AIF   (NOT(&IE_TYPE(&IE_LVL) EQ 12)).AIF_84_1  IS THERE ON SX
               IZE/ERROR
               GEN_LABEL SUBTRACT_&IE_TCNT(&IE_LVL),END_SUBTRACT
&IE_LVL        SETA  &IE_LVL-1
.*       AEND
.AIF_84_1  ANOP
         MEND
         MACRO
.*********************************************************************
.* Copyright 2008 Automated Software Tools Corporation               *
.* This source code is part of z390 zCOBOL/assembler/emulator package*
.* The z390 package is distributed under GNU general public license  *
.* Author - Don Higgins                                              *
.* Date   - 04/16/08                                                 *
.*********************************************************************
.* 11/17/08 stub for pending support
.*********************************************************************
         ENTRY
         MNOTE 8,'ENTRY NOT SUPPORTED YET'
         MEXIT
         MEND
         MACRO
.*********************************************************************
.* Copyright 2008 Automated Software Tools Corporation               *
.* This source code is part of z390 zCOBOL/assembler/emulator package*
.* The z390 package is distributed under GNU general public license  *
.* Author - Don Higgins                                              *
.* Date   - 04/07/08                                                 *
.*********************************************************************
.* 04/07/08 initial coding for z390 target
.* 10/05/08 ZSTRMAC
.*********************************************************************
         ENVIRONMENT &DIV
.*       AIF   ('&DIV' NE 'DIVISION')
           AIF   (NOT('&DIV' NE 'DIVISION')).AIF_85_1
               MNOTE 4,'ENVIRONMENT - MISSING "DIVISION"'
.*       AEND
.AIF_85_1  ANOP
         MEND
         MACRO
.*********************************************************************
.* Copyright 2008 Automated Software Tools Corporation               *
.* This source code is part of z390 zCOBOL/assembler/emulator package*
.* The z390 package is distributed under GNU general public license  *
.* Author - Don Higgins                                              *
.* Date   - 04/07/08                                                 *
.*********************************************************************
.* 04/09/08 initial coding for global EVALUATE
.* 10/05/08 ZSTRMAC
.*********************************************************************
         EVALUATE &SYM
         COPY  ZC_WS
         ZC_SYM_FIND &SYM
.*       AIF   (&SYM_IX GT 0)
           AIF   (NOT(&SYM_IX GT 0)).AIF_86_1
&IE_LVL        SETA  &IE_LVL+1
&IE_TYPE(&IE_LVL) SETA 2
&EVAL_LAB      SETA  &EVAL_LAB+1
&IE_EVAL(&IE_LVL) SETA &EVAL_LAB
&IE_WHEN(&IE_LVL) SETA 0
&IE_EVAL_IX(&IE_LVL) SETA &SYM_IX
.*       AELSE
           AGO   .AIF_86_E
.AIF_86_1  ANOP
               MNOTE 8,'EVALUATE VARIABLE NOT FOUND - &SYM'
.*       AEND
.AIF_86_E  ANOP
         MEND
         MACRO
.*********************************************************************
.* Copyright 2008 Automated Software Tools Corporation               *
.* This source code is part of z390 zCOBOL/assembler/emulator package*
.* The z390 package is distributed under GNU general public license  *
.* Author - Don Higgins                                              *
.* Date   - 04/16/08                                                 *
.*********************************************************************
.* 12/13/08 stub for pending support
.*********************************************************************
         EXAMINE
         MNOTE 8,'EXAMINE REPLACED BY INSPECT'
         MEXIT
         MEND
         MACRO
.*********************************************************************
.* Copyright 2008 Automated Software Tools Corporation               *
.* This source code is part of z390 zCOBOL/assembler/emulator package*
.* The z390 package is distributed under GNU general public license  *
.* Author - Don Higgins                                              *
.* Date   - 04/16/08                                                 *
.*********************************************************************
.* 12/13/08 stub for pending support
.*********************************************************************
         EXHIBIT
         MNOTE 8,'EXHIBIT REPLACED BY DISPLAY'
         MEXIT
         MEND
         MACRO
.*********************************************************************
.* Copyright 2008 Automated Software Tools Corporation               *
.* This source code is part of z390 zCOBOL/assembler/emulator package*
.* The z390 package is distributed under GNU general public license  *
.* Author - Don Higgins                                              *
.* Date   - 04/15/08                                                 *
.*********************************************************************
.* 06/13/08 generate perform paragraph exit check
.* 10/28/08 add support for EXIT PROGRAM
.*********************************************************************
         EXIT  &PGM
.*     AIF   ('&PGM' EQ 'PROGRAM')
         AIF   (NOT('&PGM' EQ 'PROGRAM')).AIF_87_1
         GEN_STOP RUN
.*     AEND
.AIF_87_1 ANOP
         MEND

        MACRO
.*********************************************************************
.* Copyright 2008 Automated Software Tools Corporation               *
.* This source code is part of z390 zCOBOL/assembler/emulator package*
.* The z390 package is distributed under GNU general public license  *
.* Author - Don Higgins                                              *
.* Date   - 04/07/08                                                 *
.*********************************************************************
.* 04/07/08 initial coding for global FD definition
.* 10/06/08 ZSTRMAC
.*********************************************************************
.* Generate file definition using file data from SELECT in ZC_FD.CPY
.*********************************************************************
         FD    &FILE
         COPY  ZC_WS
         ZC_FILE_FIND &FILE
.*       AIF   (&FILE_IX EQ 0)
           AIF   (NOT(&FILE_IX EQ 0)).AIF_88_1
               MNOTE 8,'FD FILE NAME NOT FOUND - &FILE'
               MEXIT
.*       AEND
.AIF_88_1  ANOP
&PARM_IX SETA 2
.*       AIF   ('&SYSLIST(&PARM_IX)' EQ 'IS')
           AIF   (NOT('&SYSLIST(&PARM_IX)' EQ 'IS')).AIF_89_1
&PARM_IX       SETA &PARM_IX+1
.*       AEND
.AIF_89_1  ANOP
.*       AIF ('&SYSPARM(&PARM_IX)' EQ 'EXTERNAL')
           AIF (NOT('&SYSPARM(&PARM_IX)' EQ 'EXTERNAL')).AIF_90_1
&EXTERNAL    SETB 1
&PARM_IX     SETA &PARM_IX+1
.*       AELSEIF ('&SYSPARM(&PARM_IX)' EQ 'GLOBAL')
           AGO   .AIF_90_E
.AIF_90_1  ANOP
           AIF     (NOT('&SYSPARM(&PARM_IX)' EQ 'GLOBAL')).AIF_90_2
&GLOBAL      SETB 1
&PARM_IX     SETA &PARM_IX+1
.*       AEND
.AIF_90_2  ANOP
.AIF_90_E  ANOP
.*
.* PROCESS FD OPTIONS IN ANY ORDER
.*
.*     AWHILE  ('&SYSLIST(&PARM_IX)' NE '')
.AWH_16_T ANOP
         AIF     (NOT('&SYSLIST(&PARM_IX)' NE '')).AWH_16_E
.*       AIF   ('&SYSLIST(&PARM_IX)' EQ 'DATA')
           AIF   (NOT('&SYSLIST(&PARM_IX)' EQ 'DATA')).AIF_91_1
&PARM_IX       SETA &PARM_IX+1
.*             AIF ('&SYSLIST(&PARM_IX)' EQ 'RECORD'                   X
                 OR '&SYSLIST(&PARM_IX)' EQ 'RECORDS')
                 AIF (NOT('&SYSLIST(&PARM_IX)' EQ 'RECORD'             X
                       OR '&SYSLIST(&PARM_IX)' EQ 'RECORDS')).AIF_92_1
&PARM_IX           SETA &PARM_IX+1
.*                 AIF ('&SYSLIST(&PARM_IX)' EQ 'IS'                   X
                     OR '&SYSLIST(&PARM_IX)' EQ 'ARE')
                     AIF (NOT('&SYSLIST(&PARM_IX)' EQ 'IS'             X
                           OR '&SYSLIST(&PARM_IX)' EQ 'ARE')).AIF_93_1
&PARM_IX               SETA &PARM_IX+1
.*                 AEND
.AIF_93_1            ANOP
.*             AELSE
                 AGO   .AIF_92_E
.AIF_92_1        ANOP
                   MNOTE 8,'FD UNKNOWN DATA PARM - &SYSLIST(&PARM_IX)'
&PARM_IX           SETA N'&SYSLIST+1
.*             AEND
.AIF_92_E        ANOP
&FILE_RECORD(&FILE_IX) SETC '&SYSLIST(&PARM_IX)'
&PARM_IX       SETA &PARM_IX+1
.*       AELSEIF ('&SYSLIST(&PARM_IX)' EQ 'BLOCK')
           AGO   .AIF_91_E
.AIF_91_1  ANOP
           AIF     (NOT('&SYSLIST(&PARM_IX)' EQ 'BLOCK')).AIF_91_2
&PARM_IX      SETA &PARM_IX+1
.*            AIF ('&SYSLIST(&PARM_IX)' EQ 'CONTAINS')
                AIF (NOT('&SYSLIST(&PARM_IX)' EQ 'CONTAINS')).AIF_94_1
&PARM_IX          SETA &PARM_IX+1
.*            AEND
.AIF_94_1       ANOP
&FILE_BLKSI2(&PARM_IX) SETC '&SYSLIST(&PARM_IX)'
&PARM_IX      SETA &PARM_IX+1
.*            AIF ('&SYSLIST(&PARM_IX)' EQ 'TO')
                AIF (NOT('&SYSLIST(&PARM_IX)' EQ 'TO')).AIF_95_1
&FILE_BLKSI1(&PARM_IX) SETC '&FILE_BLKSI2(&PARM_IX)'
&FILE_BLKSI2(&PARM_IX) SETC '&SYSLIST(&PARM_IX+1)'
&PARM_IX          SETA &PARM_IX+2
.*            AEND
.AIF_95_1       ANOP
.*            AIF ('&SYSLIST(&PARM_IX)' EQ 'CHARACTERS')
                AIF (NOT('&SYSLIST(&PARM_IX)' EQ 'CHARACTERS')).AIF_96_X
               1
&PARM_IX          SETA &PARM_IX+1
.*            AEND
.AIF_96_1       ANOP
.*            AIF ('&SYSLIST(&PARM_IX)' EQ 'RECORDS')
                AIF (NOT('&SYSLIST(&PARM_IX)' EQ 'RECORDS')).AIF_97_1
&FILE_BLOCK_RECS(&PARM_IX) SETB 1
&PARM_IX          SETA &PARM_IX+1
.*            AEND
.AIF_97_1       ANOP
.*       AELSEIF ('&SYSLIST(&PARM_IX)' EQ 'RECORD')
           AGO   .AIF_91_E
.AIF_91_2  ANOP
           AIF     (NOT('&SYSLIST(&PARM_IX)' EQ 'RECORD')).AIF_91_3
&PARM_IX      SETA &PARM_IX+1
.*            AIF ('&SYSLIST(&PARM_IX)' EQ 'CONTAINS')
                AIF (NOT('&SYSLIST(&PARM_IX)' EQ 'CONTAINS')).AIF_98_1
&PARM_IX          SETA &PARM_IX+1
.*            AELSE
                AGO   .AIF_98_E
.AIF_98_1       ANOP
.*                AIF ('&SYSLIST(&PARM_IX)' EQ 'IS')
                    AIF (NOT('&SYSLIST(&PARM_IX)' EQ 'IS')).AIF_99_1
&PARM_IX              SETA &PARM_IX+1
.*                AEND
.AIF_99_1           ANOP
.*                AIF ('&SYSLIST(&PARM_IX)' EQ 'VARYING')
                    AIF (NOT('&SYSLIST(&PARM_IX)' EQ 'VARYING')).AIF_10X
               0_1
&PARM_IX              SETA &PARM_IX+1
.*                    AIF ('&SYSLIST(&PARM_IX)' EQ 'IN')
                        AIF (NOT('&SYSLIST(&PARM_IX)' EQ 'IN')).AIF_101X
               _1
&PARM_IX                  SETA &PARM_IX+1
.*                    AEND
.AIF_101_1              ANOP
.*                    AIF ('&SYSLIST(&PARM_IX)' EQ 'SIZE')
                        AIF (NOT('&SYSLIST(&PARM_IX)' EQ 'SIZE')).AIF_1X
               02_1
&PARM_IX                  SETA &PARM_IX+1
.*                    AEND
.AIF_102_1              ANOP
.*                    AIF ('&SYSLIST(&PARM_IX)' EQ 'FROM')
                        AIF (NOT('&SYSLIST(&PARM_IX)' EQ 'FROM')).AIF_1X
               03_1
&PARM_IX                  SETA &PARM_IX+1
.*                    AEND
.AIF_103_1              ANOP
.*                AEND
.AIF_100_1          ANOP
.*            AEND
.AIF_98_E       ANOP
&FILE_LRECL2(&PARM_IX) SETC '&SYSLIST(&PARM_IX)'
&PARM_IX      SETA &PARM_IX+1
.*            AIF ('&SYSLIST(&PARM_IX)' EQ 'TO')
                AIF (NOT('&SYSLIST(&PARM_IX)' EQ 'TO')).AIF_104_1
&FILE_LRECL1(&PARM_IX) SETC '&FILE_LRECL2(&PARM_IX)'
&FILE_LRECL2(&PARM_IX) SETC '&SYSLIST(&PARM_IX+1)'
&PARM_IX          SETA &PARM_IX+2
.*            AEND
.AIF_104_1      ANOP
.*            AIF ('&SYSLIST(&PARM_IX)' EQ 'CHARACTERS')
                AIF (NOT('&SYSLIST(&PARM_IX)' EQ 'CHARACTERS')).AIF_105X
               _1
&PARM_IX          SETA &PARM_IX+1
.*            AEND
.AIF_105_1      ANOP
.*            AIF ('&SYSLIST(&PARM_IX)' EQ 'DEPENDING')
                AIF (NOT('&SYSLIST(&PARM_IX)' EQ 'DEPENDING')).AIF_106_X
               1
&PARM_IX          SETA &PARM_IX+1
.*                AIF ('&SYSLIST(&PARM_IX)' EQ 'ON')
                    AIF (NOT('&SYSLIST(&PARM_IX)' EQ 'ON')).AIF_107_1
&PARM_IX              SETA &PARM_IX+1
.*                AEND
.AIF_107_1          ANOP
&FILE_DEPENDING(&PARM_IX) SETC '&SYSLIST(&PARM_IX)'
&PARM_IX          SETA &PARM_IX+1
.*            AEND
.AIF_106_1      ANOP
.*       AELSEIF ('&SYSLIST(&PARM_IX)' EQ 'LABEL')
           AGO   .AIF_91_E
.AIF_91_3  ANOP
           AIF     (NOT('&SYSLIST(&PARM_IX)' EQ 'LABEL')).AIF_91_4
&PARM_IX      SETA &PARM_IX+1
.*            AIF ('&SYSLIST(&PARM_IX)' EQ 'RECORD')
                AIF (NOT('&SYSLIST(&PARM_IX)' EQ 'RECORD')).AIF_108_1
&PARM_IX          SETA &PARM_IX+1
.*            AEND
.AIF_108_1      ANOP
.*            AIF ('&SYSLIST(&PARM_IX)' EQ 'RECORDS')
                AIF (NOT('&SYSLIST(&PARM_IX)' EQ 'RECORDS')).AIF_109_1
&PARM_IX          SETA &PARM_IX+1
.*            AEND
.AIF_109_1      ANOP
.*            AIF ('&SYSLIST(&PARM_IX)' EQ 'IS')
                AIF (NOT('&SYSLIST(&PARM_IX)' EQ 'IS')).AIF_110_1
&PARM_IX          SETA &PARM_IX+1
.*            AEND
.AIF_110_1      ANOP
.*            AIF ('&SYSLIST(&PARM_IX)' EQ 'ARE')
                AIF (NOT('&SYSLIST(&PARM_IX)' EQ 'ARE')).AIF_111_1
&PARM_IX          SETA &PARM_IX+1
.*            AEND
.AIF_111_1      ANOP
.*            AIF ('&SYSLIST(&PARM_IX)' EQ 'STANDARD')
                AIF (NOT('&SYSLIST(&PARM_IX)' EQ 'STANDARD')).AIF_112_1
&PARM_IX          SETA &PARM_IX+1
.*            AEND
.AIF_112_1      ANOP
.*            AIF ('&SYSLIST(&PARM_IX)' EQ 'OMITTED')
                AIF (NOT('&SYSLIST(&PARM_IX)' EQ 'OMITTED')).AIF_113_1
&PARM_IX          SETA &PARM_IX+1
.*            AEND
.AIF_113_1      ANOP
.*       AELSE
           AGO   .AIF_91_E
.AIF_91_4  ANOP
              MNOTE 8,'FD OPTION NOT SUPPORTED - &SYSLIST(&PARM_IX)'
&PARM_IX      SETA N'&SYSLIST+1
.*       AEND
.AIF_91_E  ANOP
.*     AEND
         AGO   .AWH_16_T
.AWH_16_E ANOP
         WS    91,&FILE                         ADD FILE TO WS
         MEND
         MACRO
.*********************************************************************
.* Copyright 2008 Automated Software Tools Corporation               *
.* This source code is part of z390 zCOBOL/assembler/emulator package*
.* The z390 package is distributed under GNU general public license  *
.* Author - Don Higgins                                              *
.* Date   - 04/14/08                                                 *
.*********************************************************************
.* 04/14/08 initial coding for global ZCOBOL
.* 10/06/08 ZSTRMAC
.*********************************************************************
         FILE  &SECTION
.*       AIF   ('&SECTION' NE 'SECTION')
           AIF   (NOT('&SECTION' NE 'SECTION')).AIF_114_1
               MNOTE 8,'FILE SECTION MISSING'
.*       AEND
.AIF_114_1 ANOP
         MEND
         MACRO
.*********************************************************************
.* Copyright 2008 Automated Software Tools Corporation               *
.* This source code is part of z390 zCOBOL/assembler/emulator package*
.* The z390 package is distributed under GNU general public license  *
.* Author - Don Higgins                                              *
.* Date   - 04/14/08                                                 *
.*********************************************************************
.* 04/14/08 initial coding for global ZCOBOL
.*********************************************************************
         FILE_CONTROL
         MEND
         MACRO
.*********************************************************************
.* Copyright 2008 Automated Software Tools Corporation               *
.* This source code is part of z390 zCOBOL/assembler/emulator package*
.* The z390 package is distributed under GNU general public license  *
.* Author - Don Higgins                                              *
.* Date   - 04/16/08                                                 *
.*********************************************************************
.* 02/20/09 stub for pending support
.*********************************************************************
         GENERATE
         MNOTE 8,'GENERATE NOT SUPPORTED YET'
         MEXIT
         MEND
         MACRO
.*********************************************************************
.* Copyright 2008 Automated Software Tools Corporation               *
.* This source code is part of z390 zCOBOL/assembler/emulator package*
.* The z390 package is distributed under GNU general public license  *
.* Author - Don Higgins                                              *
.* Date   - 04/16/08                                                 *
.*********************************************************************
.* 04/16/08 initial coding for global GO TO
.* 10/06/08 ZSTRMAC
.* 08/05/09 RPI 1065 add support for SN-PG paragraph names
.* 08/16/09 RPI 1065 support GO TO without target as model for ALTER
.*          see OBNC1M NIST test.
.*********************************************************************
         GO
         COPY  ZC_WS
&NP      SETA  N'&SYSLIST
&PARM_IX SETA  1
.*       AIF   ('&SYSLIST(&PARM_IX)' EQ 'TO')
           AIF   (NOT('&SYSLIST(&PARM_IX)' EQ 'TO')).AIF_115_1
&PARM_IX       SETA  &PARM_IX+1
.*       AEND
.AIF_115_1 ANOP
.*       AIF   (&NP EQ 1)                          RPI 1065
           AIF   (NOT(&NP EQ 1)).AIF_116_1                           RPX
               I 1065
               GEN_B  &LAST_PG_NAME,&LAST_SN_NAME  MODEL FOR ALTER
               MEXIT
.*       AEND
.AIF_116_1 ANOP
&FIRST   SETA &PARM_IX                      RPI 1065
.*       AIF   ('&SYSLIST(&PARM_IX+1)' EQ 'OF'                         X
               OR '&SYSLIST(&PARM_IX+1)' EQ 'IN')
           AIF   (NOT('&SYSLIST(&PARM_IX+1)' EQ 'OF'                   X
                     OR '&SYSLIST(&PARM_IX+1)' EQ 'IN')).AIF_117_1
&SN_NAME       SETC '&SYSLIST(&PARM_IX+2)'
&PG_NAME       SETC '&SYSLIST(&PARM_IX)'
&PARM_IX       SETA &PARM_IX+3
.*       AELSE
           AGO   .AIF_117_E
.AIF_117_1 ANOP
&SN_NAME       SETC ''
&PG_NAME       SETC '&SYSLIST(&PARM_IX)'
&PARM_IX       SETA &PARM_IX+1
.*       AEND
.AIF_117_E ANOP
.*       AIF   (&NP LT &PARM_IX)
           AIF   (NOT(&NP LT &PARM_IX)).AIF_118_1
               GEN_B  &PG_NAME,&SN_NAME
.*       AELSE
           AGO   .AIF_118_E
.AIF_118_1 ANOP
&GO_LIST       SETC '&SYSLIST(&FIRST)'     RPI 1065
&PARM_IX       SETA &FIRST+1               RPI 1065
.*             AWHILE (&PARM_IX LT &NP-2                               X
                      AND '&SYSLIST(&PARM_IX)' NE 'DEPENDING')
.AWH_17_T        ANOP
                 AIF    (NOT(&PARM_IX LT &NP-2                         X
                            AND '&SYSLIST(&PARM_IX)' NE 'DEPENDING')).AX
               WH_17_E
&GO_LIST           SETC '&GO_LIST,&SYSLIST(&PARM_IX)'
&PARM_IX           SETA &PARM_IX+1
.*             AEND
                 AGO   .AWH_17_T
.AWH_17_E        ANOP
.*             AIF  ('&SYSLIST(&PARM_IX)' EQ 'DEPENDING')              X
                    AND '&SYSLIST(&PARM_IX+1)' EQ 'ON')
                 AIF  (NOT('&SYSLIST(&PARM_IX)' EQ 'DEPENDING')        X
                          AND '&SYSLIST(&PARM_IX+1)' EQ 'ON')).AIF_119_X
               1
&PARM_IX            SETA &PARM_IX+2
&FIELD_REG          SETC 'ZC_R2'
&FIELD_SS1          SETB 0
.*                  ACALL GET_PARM_FIELD
&ACALL_6_GET_PARM_FIELD SETA    11
                      AGO   .ACL_6_GET_PARM_FIELD
.ACL_6_11             ANOP
.*                  AIF (&FIELD_IX GT 0)
                      AIF (NOT(&FIELD_IX GT 0)).AIF_120_1
                        GEN_GO_DEPEND &GO_LIST
.*                  AELSE
                      AGO   .AIF_120_E
.AIF_120_1            ANOP
                        MNOTE 8,'GO TO DEPENDING VAR NOT FOUND - &FIELDX
               _NAME'
.*                  AEND
.AIF_120_E            ANOP
.*             AELSE
                 AGO   .AIF_119_E
.AIF_119_1       ANOP
                    MNOTE 8,'GO TO DEPENDING CLAUSE NOT FOUND'
.*             AEND
.AIF_119_E       ANOP
.*       AEND
.AIF_118_E ANOP
         COPY ZCGETFLD
         MEND
         MACRO
.*********************************************************************
.* Copyright 2008 Automated Software Tools Corporation               *
.* This source code is part of z390 zCOBOL/assembler/emulator package*
.* The z390 package is distributed under GNU general public license  *
.* Author - Don Higgins                                              *
.* Date   - 04/07/08                                                 *
.*********************************************************************
.* 04/11/08 initial coding for global GOBACK
.*********************************************************************
         GOBACK
         GEN_GOBACK
         MEND
         MACRO
.*********************************************************************
.* Copyright 2008 Automated Software Tools Corporation               *
.* This source code is part of z390 zCOBOL/assembler/emulator package*
.* The z390 package is distributed under GNU general public license  *
.* Author - Don Higgins                                              *
.* Date   - 04/07/08                                                 *
.*********************************************************************
.* 04/07/08 initial coding for global IDENTIFY
.* 10/06/08 ZSTRMAC
.*********************************************************************
         IDENTIFICATION &DIV
.*       AIF   ('&DIV' NE 'DIVISION')
           AIF   (NOT('&DIV' NE 'DIVISION')).AIF_121_1
               MNOTE 4,'IDENTIFICATION - MISSING "DIVISION"'
.*       AEND
.AIF_121_1 ANOP
         MEND
        MACRO
.*********************************************************************
.* Copyright 2008 Automated Software Tools Corporation               *
.* This source code is part of z390 zCOBOL/assembler/emulator package*
.* The z390 package is distributed under GNU general public license  *
.* Author - Don Higgins                                              *
.* Date   - 04/07/08                                                 *
.*********************************************************************
.* 04/09/08 initial coding for global IF
.* 05/01/08 add support for IF A NOT = B
.* 10/06/08 ZSTRMAC
.* 10/12/08 USE PUSH/POP CONDITION AND LOGICAL OPERATOR STACKS
.*          TO PARSE COMPLEX IF WITH PRECEDENCE (NOT, AND, OR)
.*          AND GENERATE INTERMEDIATE CODE IN TABLE FOR OPTIMIZATION
.*          PRIOR TO CODE GENERATION MACROS GEN_COMP AND GEN_BC
.* 01/05/09 add support for omitted subject+verb in condition test
.*          used in NIST EXEC85.CBL.  Test added to TESTIF1.CBL.
.* 02/22/09 RPI 1001 add support for conditional 88 value tests
.* 02/26/09 RPI 1012 issue MNOTE if first field not found for compare
.* 04/21/09 RPI 1020 correct support for mult OR for same test field
.* 06/14/09 RPI 1057 use ZC_DFHRESP.CPY and ZC_DFHVALUE.CPY sync AZ390
.*          and correct missing SQ in GET_DFH routines
.* 06/18/09 RPI 1042 add ZCDFHLIT shared by IF and MOVE
.* 08/09/09 RPI 1065 allow omitting subject and relation for AND OR
.* 08/14/09 RPI 1065 optimize mult OR's using same end target if true
.* 08/15/09 RPI 1065 detect and issue MNOTE for expressions for
.*********************************************************************
.* Pending work includes support for field,lit,expression in either
.* first or second operand.  Coming after COMPUTE dubugs GEN_EXP.
.*********************************************************************
         IF
         COPY  ZC_WS
&ZC_ERROR SETB 0 CLEAR ANY GEN_COMP ERRORS
&LAST_VAR SETB 0 WAS LAST VAR A FIELD OR LIT
.*
.* PUSH/POP COND INDEX AND LOGICAL OPERATOR STACKS FOR PARSING
.*
         LCLA  &STK_CD_TOT  CUR TOTAL CD/IOP IX'S ON STACK
         LCLA  &STK_CD(10)  IX TO CD PARM # OR -IOP RESULT #
         LCLA  &STK_OP_TOT  CUR TOTAL OP ON STACK
         LCLA  &STK_OP(10)  OP (1=NOT,2=AND,3=OR,4='(')
         LCLA  &LAST_CD_PARM_IX LAST CD PTR ON STK FOR OMITTED SUB/VERB
         LCLC  &FIELD1,&FIELD2
         LCLA  &FIELD1_IX,&FIELD2_IX
.*
.* GENERATED INTERMEDIATE OPCODE TABLE
.*
         LCLA  &IOP_TOT        TOTAL INTERMEDIATE LOG. OPS
         LCLA  &IOP_OP(50)     LOG. OPER. (1=NOT,2=AND,3=OR)
         LCLA  &IOP_CD1(50)     CONDITION1 +PARM IX OR -LOP IX
         LCLA  &IOP_CD2(50)     CONDITION2 +PARM IX OR -LOP IX
         LCLB  &IOP_TF(50)     SAVE IOP TRUE/FALSE I**N IOP_TF#
         LCLA  &IOP_OR_END(50) OPTIMIZE MULT OR USING SAME END TARGET
         LCLB  &V88_MULT       FLAG FOR GENERATING LABEL FOR MUL 88'S
.*
.* SET IF/EVAL LVL AND EXIT_LAB #
.*
&IE_LVL  SETA  &IE_LVL+1          IF LEVEL
&IE_TYPE(&IE_LVL) SETA 1          IF VS EVALUATE
&IF_CNT  SETA &IF_CNT+1            UNIQUE IF COUNTER
&IE_TCNT(&IE_LVL) SETA &IF_CNT
&IE_BCNT(&IE_LVL) SETA 1          CUR BLOCK COUNT WITHIN IF
&IE_NEXT(&IE_LVL) SETA 1          CUR EXIT BLOCK
&IE_ELSE(&IE_LVL) SETB 0          RESET ELSE FLAG
.*
.* BUILD IOP LIST WITH BACKWARD REF UPDATE AND THEN GEN CODE
.*
.*       ACALL IOP_BUILD      BUILD IOP TABLE
&ACALL_15_IOP_BUILD SETA    1
           AGO   .ACL_15_IOP_BUILD
.ACL_15_1  ANOP
.*  ACALL LIST_IOPS   DEBUG
.*       ACALL IOP_OPT_OR     OPTIMIZE MULT OR'S
&ACALL_16_IOP_OPT_OR SETA    1
           AGO   .ACL_16_IOP_OPT_OR
.ACL_16_1  ANOP
.*       ACALL IOP_GEN_CODE   GEN CODE FROM IOP TABLE
&ACALL_17_IOP_GEN_CODE SETA    1
           AGO   .ACL_17_IOP_GEN_CODE
.ACL_17_1  ANOP
         MEXIT
.*
.* BUILD IOP TABLE USING BNF CD AND LOP STAKS
.*   1.  PUSH +CONDITION PARM PTRS AND -IOP PTRS      ON CD_STK
.*   2.  PUSH LOGICAL OPERATORS '(',NOT, AND, OR,')'  ON OP_STK
.*   3.  POP  OP AND 1 OR 2 CD'S TO GEN NEXT IOP
.*       WHEN LOP PRECEDENCE OR AT END
.*
.*       AENTRY IOP_BUILD
           AGO   .ACL_15_SKIP
.ACL_15_IOP_BUILD ANOP
&NP      SETA  N'&SYSLIST  TOTAL IF PARMS
.*       AIF (&NP GT 0)
           AIF (NOT(&NP GT 0)).AIF_122_1
.*           AIF ('&SYSLIST(&NP)' EQ 'THEN')
               AIF (NOT('&SYSLIST(&NP)' EQ 'THEN')).AIF_123_1
&NP              SETA &NP-1  IGNORE THEN
.*           AEND
.AIF_123_1     ANOP
.*       AEND
.AIF_122_1 ANOP
&LVL     SETA  0           CUR (..) LEVEL WITHIN IF PARMS
&CD_PARM_TOT SETA 0     CUR CONDITION PARM COUNT (A EQ B ETC)
&STK_CD_TOT SETA 0      RESET CONDITION TEST/IOP RESULT STACK
&STK_OP_TOT SETA 0      RESET LOGICAL IOP OPERATOR STACK
&PARM_IX SETA  0        GET PARM INDEX
&PREV_PARM SETC ''      PREV PARM
&PARM    SETC ''           CUR  PARM
&SKIP_SIX_OR_LIT SETB 0  SKIP SUBSCRIPTS IN CD'S
.*       ACALL  GET_PARM           GET NEXT PARM OR ''
&ACALL_18_GET_PARM SETA    1
           AGO   .ACL_18_GET_PARM
.ACL_18_1  ANOP
.*       AWHILE ('&PARM' NE '')
.AWH_18_T  ANOP
           AIF    (NOT('&PARM' NE '')).AWH_18_E
.*             AIF ('&PARM' EQ '''(''')
                 AIF (NOT('&PARM' EQ '''(''')).AIF_124_1
&LAST_VAR          SETB 0
.*                 AIF ('&PREV_PARM' EQ 'DFHRESP'                      X
                        OR '&PREV_PARM' EQ 'DFHVALUE')
                     AIF (NOT('&PREV_PARM' EQ 'DFHRESP'                X
                              OR '&PREV_PARM' EQ 'DFHVALUE')).AIF_125_1
.*                      AIF ('&SYSLIST(&PARM_IX+2' EQ ''')''')
                          AIF (NOT('&SYSLIST(&PARM_IX+2' EQ ''')''')).AX
               IF_126_1
&PARM_IX                    SETA &PARM_IX+2 SKIP (,VALUE,)
.*                      AELSE
                          AGO   .AIF_126_E
.AIF_126_1                ANOP
                            MNOTE 8,'IF INVALID DFHRESP/DFHVALUE'
                            MEXIT
.*                      AEND
.AIF_126_E                ANOP
.*                 AELSE
                     AGO   .AIF_125_E
.AIF_125_1           ANOP
&LVL                    SETA &LVL+1
&IOP                    SETA 4
.*                      ACALL PUSH_OP      PUSH OP 4=(
&ACALL_19_PUSH_OP         SETA    1
                          AGO   .ACL_19_PUSH_OP
.ACL_19_1                 ANOP
.*                 AEND
.AIF_125_E           ANOP
.*             AELSEIF ('&PARM' EQ ''')''')
                 AGO   .AIF_124_E
.AIF_124_1       ANOP
                 AIF     (NOT('&PARM' EQ ''')''')).AIF_124_2
&LAST_VAR          SETB 0
&LVL               SETA &LVL-1
.*                 AIF (&STK_OP_TOT GT 0)
                     AIF (NOT(&STK_OP_TOT GT 0)).AIF_127_1
&LOOP_NE_4             SETB 1  RPI 1065
.*                     AWHILE (&STK_OP_TOT GT 0 AND &LOOP_NE_4)
.AWH_19_T                ANOP
                         AIF    (NOT(&STK_OP_TOT GT 0 AND &LOOP_NE_4)).X
               AWH_19_E
.*                       AIF (&STK_OP(&STK_OP_TOT) NE 4) FLUSH (..)
                           AIF (NOT(&STK_OP(&STK_OP_TOT) NE 4) FLUSH (.X
               .)).AIF_128_1
.*                         ACALL POP_OP
&ACALL_20_POP_OP             SETA    1
                             AGO   .ACL_20_POP_OP
.ACL_20_1                    ANOP
.*                         ACASE (&IOP)
                             AGO   .ACS_1_G
.*                         AWHEN 1
.ACS_1_1                     ANOP
.*                             ACALL IOP_ADD_NOT
&ACALL_21_IOP_ADD_NOT            SETA    1
                                 AGO   .ACL_21_IOP_ADD_NOT
.ACL_21_1                        ANOP
.*                         AWHEN 2
                             AGO   .ACS_1_E
.ACS_1_2                     ANOP
.*                             ACALL IOP_ADD_AND
&ACALL_22_IOP_ADD_AND            SETA    1
                                 AGO   .ACL_22_IOP_ADD_AND
.ACL_22_1                        ANOP
.*                         AWHEN 3
                             AGO   .ACS_1_E
.ACS_1_3                     ANOP
.*                             ACALL IOP_ADD_OR
&ACALL_23_IOP_ADD_OR             SETA    1
                                 AGO   .ACL_23_IOP_ADD_OR
.ACL_23_1                        ANOP
.*                         AELSE
                             AGO   .ACS_1_E
.ACS_1_X                     ANOP
                               MNOTE 8,'IF UNKNOWN OP STACK VALUE &OP'
                               MEXIT
.*                         AEND
                             AGO   .ACS_1_E
.ACS_1_G                     ANOP
                             AGO   (&IOP).ACS_1_1,.ACS_1_2,.ACS_1_3
                             AGO   .ACS_1_X
.ACS_1_E                     ANOP
.*                       AELSE
                           AGO   .AIF_128_E
.AIF_128_1                 ANOP
&LOOP_NE_4                 SETB 0
.*                       AEND
.AIF_128_E                 ANOP
.*                     AEND
                         AGO   .AWH_19_T
.AWH_19_E                ANOP
.*                     ACALL POP_OP  REMOVE ( FOR MATCHING )
&ACALL_20_POP_OP         SETA    2
                         AGO   .ACL_20_POP_OP
.ACL_20_2                ANOP
.*                 AELSE
                     AGO   .AIF_127_E
.AIF_127_1           ANOP
                       MNOTE 8,'IF UNBALANCED (..)'
                       MEXIT
.*                 AEND
.AIF_127_E           ANOP
.*             AELSEIF ('&PARM' EQ 'NOT')
                 AGO   .AIF_124_E
.AIF_124_2       ANOP
                 AIF     (NOT('&PARM' EQ 'NOT')).AIF_124_3
&LAST_VAR          SETB 0
.*                 AIF  (&CD_PARM_TOT EQ 0)
                     AIF  (NOT(&CD_PARM_TOT EQ 0)).AIF_129_1
&IOP                    SETA 1
.*                      ACALL PUSH_OP  PUSH LOC OPER NOT
&ACALL_19_PUSH_OP         SETA    2
                          AGO   .ACL_19_PUSH_OP
.ACL_19_2                 ANOP
.*                 AEND
.AIF_129_1           ANOP
.*             AELSEIF  ('&PARM' EQ 'AND') FLUSH PRIOR NOT/AND OPS
                 AGO   .AIF_124_E
.AIF_124_3       ANOP
                 AIF      (NOT('&PARM' EQ 'AND')).AIF_124_4  FLUSH PRIOX
               R NOT/AND OPS
&LAST_VAR          SETB 0
&LAST_STK_OP_TOT   SETA &STK_OP_TOT+1
.*                 AWHILE (&STK_OP_TOT LT &LAST_STK_OP_TOT             X
                           AND &STK_OP_TOT GT 0)
.AWH_20_T            ANOP
                     AIF    (NOT(&STK_OP_TOT LT &LAST_STK_OP_TOT       X
                                 AND &STK_OP_TOT GT 0)).AWH_20_E
&LAST_STK_OP_TOT       SETA &STK_OP_TOT
.*                     AIF (&STK_OP(&STK_OP_TOT) EQ 1)
                         AIF (NOT(&STK_OP(&STK_OP_TOT) EQ 1)).AIF_130_1
.*                         ACALL POP_OP
&ACALL_20_POP_OP             SETA    3
                             AGO   .ACL_20_POP_OP
.ACL_20_3                    ANOP
.*                         ACALL IOP_ADD_NOT
&ACALL_21_IOP_ADD_NOT        SETA    2
                             AGO   .ACL_21_IOP_ADD_NOT
.ACL_21_2                    ANOP
.*                     AELSEIF (&STK_OP(&STK_OP_TOT) EQ 2)
                         AGO   .AIF_130_E
.AIF_130_1               ANOP
                         AIF     (NOT(&STK_OP(&STK_OP_TOT) EQ 2)).AIF_1X
               30_2
.*                         ACALL POP_OP
&ACALL_20_POP_OP             SETA    4
                             AGO   .ACL_20_POP_OP
.ACL_20_4                    ANOP
.*                         ACALL IOP_ADD_AND
&ACALL_22_IOP_ADD_AND        SETA    2
                             AGO   .ACL_22_IOP_ADD_AND
.ACL_22_2                    ANOP
.*                     AEND
.AIF_130_2               ANOP
.AIF_130_E               ANOP
.*                 AEND
                     AGO   .AWH_20_T
.AWH_20_E            ANOP
&IOP               SETA 2
.*                 ACALL PUSH_OP
&ACALL_19_PUSH_OP    SETA    3
                     AGO   .ACL_19_PUSH_OP
.ACL_19_3            ANOP
.*             AELSEIF  ('&PARM' EQ 'OR')
                 AGO   .AIF_124_E
.AIF_124_4       ANOP
                 AIF      (NOT('&PARM' EQ 'OR')).AIF_124_5
&LAST_VAR          SETB 0
&LAST_STK_OP_TOT   SETA &STK_OP_TOT+1
.*                 AWHILE (&STK_OP_TOT LT &LAST_STK_OP_TOT             X
                           AND &STK_OP_TOT GT 0)
.AWH_21_T            ANOP
                     AIF    (NOT(&STK_OP_TOT LT &LAST_STK_OP_TOT       X
                                 AND &STK_OP_TOT GT 0)).AWH_21_E
&LAST_STK_OP_TOT       SETA &STK_OP_TOT
.*                     AIF (&STK_OP(&STK_OP_TOT) EQ 1)
                         AIF (NOT(&STK_OP(&STK_OP_TOT) EQ 1)).AIF_131_1
.*                         ACALL POP_OP
&ACALL_20_POP_OP             SETA    5
                             AGO   .ACL_20_POP_OP
.ACL_20_5                    ANOP
.*                         ACALL IOP_ADD_NOT
&ACALL_21_IOP_ADD_NOT        SETA    3
                             AGO   .ACL_21_IOP_ADD_NOT
.ACL_21_3                    ANOP
.*                     AELSEIF (&STK_OP(&STK_OP_TOT) EQ 2)
                         AGO   .AIF_131_E
.AIF_131_1               ANOP
                         AIF     (NOT(&STK_OP(&STK_OP_TOT) EQ 2)).AIF_1X
               31_2
.*                         ACALL POP_OP
&ACALL_20_POP_OP             SETA    6
                             AGO   .ACL_20_POP_OP
.ACL_20_6                    ANOP
.*                         ACALL IOP_ADD_AND
&ACALL_22_IOP_ADD_AND        SETA    3
                             AGO   .ACL_22_IOP_ADD_AND
.ACL_22_3                    ANOP
.*                     AELSEIF (&STK_OP(&STK_OP_TOT) EQ 3)
                         AGO   .AIF_131_E
.AIF_131_2               ANOP
                         AIF     (NOT(&STK_OP(&STK_OP_TOT) EQ 3)).AIF_1X
               31_3
.*                         ACALL POP_OP
&ACALL_20_POP_OP             SETA    7
                             AGO   .ACL_20_POP_OP
.ACL_20_7                    ANOP
.*                         ACALL IOP_ADD_OR
&ACALL_23_IOP_ADD_OR         SETA    2
                             AGO   .ACL_23_IOP_ADD_OR
.ACL_23_2                    ANOP
.*                     AEND
.AIF_131_3               ANOP
.AIF_131_E               ANOP
.*                 AEND
                     AGO   .AWH_21_T
.AWH_21_E            ANOP
&IOP               SETA 3
.*                 ACALL PUSH_OP
&ACALL_19_PUSH_OP    SETA    4
                     AGO   .ACL_19_PUSH_OP
.ACL_19_4            ANOP
.*             AELSEIF ('&PARM' EQ 'FUNCTION')
                 AGO   .AIF_124_E
.AIF_124_5       ANOP
                 AIF     (NOT('&PARM' EQ 'FUNCTION')).AIF_124_6
&LAST_VAR          SETB 1
                   MNOTE 8,'IF FUNCTION NOT SUPPORTED YET'
                   MEXIT
.*             AELSE
                 AGO   .AIF_124_E
.AIF_124_6       ANOP
&EXP_OP            SETA ('&PARM' FIND '+-*/')
.*                 AIF (&LAST_VAR AND &EXP_OP GT 0)
                     AIF (NOT(&LAST_VAR AND &EXP_OP GT 0)).AIF_132_1
                       MNOTE 8,'IF EXPRESSIONS NOT SUPPORTED YET'
                       MEXIT
.*                 AEND
.AIF_132_1           ANOP
.*                 AIF ('&PARM'(1,1) GE '0')
                     AIF (NOT('&PARM'(1,1) GE '0')).AIF_133_1
&LAST_VAR              SETB 1
.*                 AELSEIF ('&PARM' FIND '_') GT 0)
                     AGO   .AIF_133_E
.AIF_133_1           ANOP
                     AIF     (NOT('&PARM' FIND '_') GT 0)).AIF_133_2
&LAST_VAR              SETB 1
.*                 AELSEIF ('&PARM' FIND '=<>' GT 0)
                     AGO   .AIF_133_E
.AIF_133_2           ANOP
                     AIF     (NOT('&PARM' FIND '=<>' GT 0)).AIF_133_3
&LAST_VAR              SETB 0
.*                 AELSE
                     AGO   .AIF_133_E
.AIF_133_3           ANOP
                       GBLB &(ZCKW_&PARM)
.*                     AIF (&(ZCKW_&PARM))   IS THIS KEY WORK VS VAR
                         AIF (NOT(&(ZCKW_&PARM))).AIF_134_1    IS THIS X
               KEY WORK VS VAR
&LAST_VAR                  SETB 0    YES, NOT VAR
.*                     AELSE
                         AGO   .AIF_134_E
.AIF_134_1               ANOP
&LAST_VAR                  SETB 1    NO, ASSUME VAR
.*                     AEND
.AIF_134_E               ANOP
.*                 AEND
.AIF_133_E           ANOP
.*                 AIF  (&CD_PARM_TOT EQ 0)
                     AIF  (NOT(&CD_PARM_TOT EQ 0)).AIF_135_1
&CD_IOP_IX              SETA &PARM_IX
.*                      ACALL PUSH_CD    PUSH 1ST CD PARM PTR ON CD_STK
&ACALL_24_PUSH_CD         SETA    1
                          AGO   .ACL_24_PUSH_CD
.ACL_24_1                 ANOP
.*                 AEND
.AIF_135_1           ANOP
&CD_PARM_TOT       SETA &CD_PARM_TOT+1 SKIP CD NAME/QUAL
&SKIP_SIX_OR_LIT   SETB 1
.*             AEND
.AIF_124_E       ANOP
.*             ACALL GET_PARM
&ACALL_18_GET_PARM SETA    2
                 AGO   .ACL_18_GET_PARM
.ACL_18_2        ANOP
.*             AIF  (&SKIP_SIX_OR_LIT) SKIP SUBSCRIPTS IN CD'S
                 AIF  (NOT(&SKIP_SIX_OR_LIT)).AIF_136_1  SKIP SUBSCRIPTX
               S IN CD'S
&SKIP_SIX_OR_LIT    SETB 0
.*                  AWHILE ('&PARM' EQ '''(''') SKIP SIX AND RMOD
.AWH_22_T             ANOP
                      AIF    (NOT('&PARM' EQ '''(''')).AWH_22_E  SKIP SX
               IX AND RMOD
.*                      ACALL GET_PARM
&ACALL_18_GET_PARM        SETA    3
                          AGO   .ACL_18_GET_PARM
.ACL_18_3                 ANOP
.*                      AWHILE ('&PARM' NE '' AND '&PARM' NE ''')''')
.AWH_23_T                 ANOP
                          AIF    (NOT('&PARM' NE '' AND '&PARM' NE ''')X
               ''')).AWH_23_E
.*                          ACALL GET_PARM
&ACALL_18_GET_PARM            SETA    4
                              AGO   .ACL_18_GET_PARM
.ACL_18_4                     ANOP
.*                      AEND
                          AGO   .AWH_23_T
.AWH_23_E                 ANOP
.*                      AIF ('&PARM' NE '')
                          AIF (NOT('&PARM' NE '')).AIF_137_1
.*                          ACALL GET_PARM
&ACALL_18_GET_PARM            SETA    5
                              AGO   .ACL_18_GET_PARM
.ACL_18_5                     ANOP
.*                      AEND
.AIF_137_1                ANOP
.*                  AEND
                      AGO   .AWH_22_T
.AWH_22_E             ANOP
.*             AEND
.AIF_136_1       ANOP
.*       AEND
           AGO   .AWH_18_T
.AWH_18_E  ANOP
.*
.* FLUSH REMAINING OPS IF ANY AT END OR PARMS
.*
.*       AWHILE (&STK_OP_TOT GT 0)
.AWH_24_T  ANOP
           AIF    (NOT(&STK_OP_TOT GT 0)).AWH_24_E
.*            ACALL POP_OP
&ACALL_20_POP_OP SETA    8
                AGO   .ACL_20_POP_OP
.ACL_20_8       ANOP
.*            ACASE (&IOP)
                AGO   .ACS_2_G
.*            AWHEN 1
.ACS_2_1        ANOP
.*                ACALL IOP_ADD_NOT
&ACALL_21_IOP_ADD_NOT SETA    4
                    AGO   .ACL_21_IOP_ADD_NOT
.ACL_21_4           ANOP
.*            AWHEN 2
                AGO   .ACS_2_E
.ACS_2_2        ANOP
.*                ACALL IOP_ADD_AND
&ACALL_22_IOP_ADD_AND SETA    4
                    AGO   .ACL_22_IOP_ADD_AND
.ACL_22_4           ANOP
.*            AWHEN 3
                AGO   .ACS_2_E
.ACS_2_3        ANOP
.*                ACALL IOP_ADD_OR
&ACALL_23_IOP_ADD_OR SETA    3
                    AGO   .ACL_23_IOP_ADD_OR
.ACL_23_3           ANOP
.*            AELSE
                AGO   .ACS_2_E
.ACS_2_X        ANOP
                  MNOTE 8,'IF UNKNOWN OP ON STACK AT END &IOP'
.*                ACALL LIST_IOPS
&ACALL_25_LIST_IOPS SETA    1
                    AGO   .ACL_25_LIST_IOPS
.ACL_25_1           ANOP
                  MEXIT
.*            AEND
                AGO   .ACS_2_E
.ACS_2_G        ANOP
                AGO   (&IOP).ACS_2_1,.ACS_2_2,.ACS_2_3
                AGO   .ACS_2_X
.ACS_2_E        ANOP
.*       AEND
           AGO   .AWH_24_T
.AWH_24_E  ANOP
.*       AEND
           AGO   (&ACALL_15_IOP_BUILD).ACL_15_1
.ACL_15_SKIP ANOP
.*
.* OPTIMIZE MULTIPLE OR'S BY SETTING TRUE TARGET TO END OF LAST OR
.* AND DON'T GEN TF FLAG
.*
.*       AENTRY IOP_OPT_OR
           AGO   .ACL_16_SKIP
.ACL_16_IOP_OPT_OR ANOP
&J       SETA &IOP_TOT
.*       AWHILE (&J GT 1)
.AWH_25_T  ANOP
           AIF    (NOT(&J GT 1)).AWH_25_E
.*           AIF (&IOP_OP(&J) EQ 3) IS THIS OP = OR
               AIF (NOT(&IOP_OP(&J) EQ 3)).AIF_138_1  IS THIS OP = OR
&L               SETA &J   LAST OR END TARGET
&K               SETA &J-1
.*               AWHILE (&K GT 0)
.AWH_26_T          ANOP
                   AIF    (NOT(&K GT 0)).AWH_26_E
.*                   AIF (&IOP_OP(&K) EQ 3                             X
                          AND (&IOP_CD1(&J) EQ -&K                     X
                               OR &IOP_CD2(&J) EQ -K))
                       AIF (NOT(&IOP_OP(&K) EQ 3                       X
                                AND (&IOP_CD1(&J) EQ -&K               X
                                     OR &IOP_CD2(&J) EQ -K))).AIF_139_1
&IOP_OR_END(&K)          SETA &L  USE SAME END TARGET
&IOP_TF(&K)              SETB 0       AND TURN OFF T/F
&J                       SETA &K
&K                       SETA &K-1
.*                   AELSE
                       AGO   .AIF_139_E
.AIF_139_1             ANOP
&K                       SETA 0
.*                   AEND
.AIF_139_E             ANOP
.*               AEND
                   AGO   .AWH_26_T
.AWH_26_E          ANOP
.*           AEND
.AIF_138_1     ANOP
&J           SETA &J-1
.*       AEND
           AGO   .AWH_25_T
.AWH_25_E  ANOP
.*       AEND
           AGO   (&ACALL_16_IOP_OPT_OR).ACL_16_1
.ACL_16_SKIP ANOP
.*
.* GEN CODE FROM IOPS
.*
.*       AENTRY IOP_GEN_CODE
           AGO   .ACL_17_SKIP
.ACL_17_IOP_GEN_CODE ANOP
.*
.* GEN SINGLE CD FROM STACK IF NO IOPS
.*
.*       AIF   (&STK_CD_TOT EQ 1)
           AIF   (NOT(&STK_CD_TOT EQ 1)).AIF_140_1
.*             AIF   (&IOP_TOT EQ 0)
                 AIF   (NOT(&IOP_TOT EQ 0)).AIF_141_1
&CD_PARM_IX          SETA &STK_CD(1)
&CD_PREV_PARM        SETC ''
&I                   SETA 1
.*                   ACALL IOP_GEN_CD
&ACALL_26_IOP_GEN_CD   SETA    1
                       AGO   .ACL_26_IOP_GEN_CD
.ACL_26_1              ANOP
                     GEN_BC   15-&CCT,IF_&IE_TCNT(&IE_LVL)_&IE_BCNT(&IEX
               _LVL)
.*                   AIF   (&V88_MULT)
                       AIF   (NOT(&V88_MULT)).AIF_142_1
&V88_MULT                  SETB 0
                           GEN_LABEL IF_&IE_TCNT(&IE_LVL)_IOP_&I
.*                   AEND
.AIF_142_1             ANOP
                     MEXIT
.*             AEND
.AIF_141_1       ANOP
.*       AELSE
           AGO   .AIF_140_E
.AIF_140_1 ANOP
               MNOTE 8,'IF CONDITION STACK COUNT ERROR &STK_CD_TOT'
.*             ACALL LIST_IOPS
&ACALL_25_LIST_IOPS SETA    2
                 AGO   .ACL_25_LIST_IOPS
.ACL_25_2        ANOP
               MEXIT
.*       AEND
.AIF_140_E ANOP
.*
.* GEN CODE FROM IOP TABLE WITH MULTIPLE ENTRIES
.*
&I       SETA 1
.*       AWHILE (&I LE &IOP_TOT)
.AWH_27_T  ANOP
           AIF    (NOT(&I LE &IOP_TOT)).AWH_27_E
.*             ACASE (&IOP_OP(&I))
                 AGO   .ACS_3_G
.*             AWHEN 1  NOT
.ACS_3_1         ANOP
.*                 ACALL IOP_GEN_NOT
&ACALL_27_IOP_GEN_NOT SETA    1
                     AGO   .ACL_27_IOP_GEN_NOT
.ACL_27_1            ANOP
.*             AWHEN 2  AND
                 AGO   .ACS_3_E
.ACS_3_2         ANOP
.*                 ACALL IOP_GEN_AND
&ACALL_28_IOP_GEN_AND SETA    1
                     AGO   .ACL_28_IOP_GEN_AND
.ACL_28_1            ANOP
.*             AWHEN 3  OR
                 AGO   .ACS_3_E
.ACS_3_3         ANOP
.*                 ACALL IOP_GEN_OR
&ACALL_29_IOP_GEN_OR SETA    1
                     AGO   .ACL_29_IOP_GEN_OR
.ACL_29_1            ANOP
.*             AELSE
                 AGO   .ACS_3_E
.ACS_3_X         ANOP
                   MNOTE 8,'IF IOP INVALID OPCODE &IOP_OP(&I)'
.*             AEND
                 AGO   .ACS_3_E
.ACS_3_G         ANOP
                 AGO   (&IOP_OP(&I)).ACS_3_1,.ACS_3_2,.ACS_3_3
                 AGO   .ACS_3_X
.ACS_3_E         ANOP
&I             SETA &I+1
.*       AEND
           AGO   .AWH_27_T
.AWH_27_E  ANOP
.*       AIF   (&V88_MULT)
           AIF   (NOT(&V88_MULT)).AIF_143_1
&V88_MULT      SETB 0
               GEN_LABEL IF_&IE_TCNT(&IE_LVL)_IOP_&I
.*       AEND
.AIF_143_1 ANOP
.*       AEND
           AGO   (&ACALL_17_IOP_GEN_CODE).ACL_17_1
.ACL_17_SKIP ANOP
.*
.* IOP ADD NOT
.*
.*       AENTRY IOP_ADD_NOT
           AGO   .ACL_21_SKIP
.ACL_21_IOP_ADD_NOT ANOP
&IOP_TOT SETA &IOP_TOT+1
&IOP_OP(&IOP_TOT) SETA 1     NOT
.*       ACALL POP_CD_IOP_IX
&ACALL_30_POP_CD_IOP_IX SETA    1
           AGO   .ACL_30_POP_CD_IOP_IX
.ACL_30_1  ANOP
&IOP_CD1(&IOP_TOT) SETA &CD_IOP_IX
&CD_IOP_IX SETA -&IOP_TOT
.*       ACALL PUSH_CD
&ACALL_24_PUSH_CD SETA    2
           AGO   .ACL_24_PUSH_CD
.ACL_24_2  ANOP
.*       AEND
           AGO   (&ACALL_21_IOP_ADD_NOT).ACL_21_1,.ACL_21_2,.ACL_21_3,.X
               ACL_21_4
.ACL_21_SKIP ANOP
.*
.* IOP ADD AND
.*
.*       AENTRY IOP_ADD_AND
           AGO   .ACL_22_SKIP
.ACL_22_IOP_ADD_AND ANOP
&IOP_TOT SETA &IOP_TOT+1
&IOP_OP(&IOP_TOT) SETA 2     AND
.*       ACALL POP_CD_IOP_IX
&ACALL_30_POP_CD_IOP_IX SETA    2
           AGO   .ACL_30_POP_CD_IOP_IX
.ACL_30_2  ANOP
&IOP_CD2(&IOP_TOT) SETA &CD_IOP_IX
.*       ACALL POP_CD_IOP_IX
&ACALL_30_POP_CD_IOP_IX SETA    3
           AGO   .ACL_30_POP_CD_IOP_IX
.ACL_30_3  ANOP
&IOP_CD1(&IOP_TOT) SETA &CD_IOP_IX
&CD_IOP_IX SETA -&IOP_TOT
.*       ACALL PUSH_CD
&ACALL_24_PUSH_CD SETA    3
           AGO   .ACL_24_PUSH_CD
.ACL_24_3  ANOP
.*       AEND
           AGO   (&ACALL_22_IOP_ADD_AND).ACL_22_1,.ACL_22_2,.ACL_22_3,.X
               ACL_22_4
.ACL_22_SKIP ANOP
.*
.* IOP ADD OR
.*
.*       AENTRY IOP_ADD_OR
           AGO   .ACL_23_SKIP
.ACL_23_IOP_ADD_OR ANOP
&IOP_TOT SETA &IOP_TOT+1
&IOP_OP(&IOP_TOT) SETA 3     OR
.*       ACALL POP_CD_IOP_IX
&ACALL_30_POP_CD_IOP_IX SETA    4
           AGO   .ACL_30_POP_CD_IOP_IX
.ACL_30_4  ANOP
&IOP_CD2(&IOP_TOT) SETA &CD_IOP_IX
.*       ACALL POP_CD_IOP_IX
&ACALL_30_POP_CD_IOP_IX SETA    5
           AGO   .ACL_30_POP_CD_IOP_IX
.ACL_30_5  ANOP
&IOP_CD1(&IOP_TOT) SETA &CD_IOP_IX
&CD_IOP_IX SETA -&IOP_TOT
.*       ACALL PUSH_CD
&ACALL_24_PUSH_CD SETA    4
           AGO   .ACL_24_PUSH_CD
.ACL_24_4  ANOP
.*       AEND
           AGO   (&ACALL_23_IOP_ADD_OR).ACL_23_1,.ACL_23_2,.ACL_23_3
.ACL_23_SKIP ANOP
.*
.* POP CD/IOP AND SET CD_IOP_IX TO CD/IOP IX FROM TOP OF STK_CD
.* AND SET IOP_TF REQUIRED FOR IOP REF'S
.*
.*       AENTRY POP_CD_IOP_IX
           AGO   .ACL_30_SKIP
.ACL_30_POP_CD_IOP_IX ANOP
.*       AIF  (&STK_CD_TOT GT 0)
           AIF  (NOT(&STK_CD_TOT GT 0)).AIF_144_1
&CD_IOP_IX    SETA &STK_CD(&STK_CD_TOT)  +CD OR -IOP IX
&STK_CD_TOT   SETA &STK_CD_TOT-1
.*            AIF (&CD_IOP_IX LE 0)
                AIF (NOT(&CD_IOP_IX LE 0)).AIF_145_1
&J                SETA -&CD_IOP_IX
.*                AIF (&J GT 0)
                    AIF (NOT(&J GT 0)).AIF_146_1
.*                    AIF (NOT &IOP_TF(&J))
                        AIF (NOT(NOT &IOP_TF(&J))).AIF_147_1
&IOP_TF(&J)               SETB 1 REQ T/F SAVE FOR IOP REF
.*                    AELSE
                        AGO   .AIF_147_E
.AIF_147_1              ANOP
                          MNOTE 8,'IF IOP &J HAS DUP IOP T/F REF'
                          CALL LIST_IOPS
                          MEXIT
.*                    AEND
.AIF_147_E              ANOP
.*                AELSE
                    AGO   .AIF_146_E
.AIF_146_1          ANOP
                      MNOTE 8,'IF CD/IOP STACK PTR 0'
                      CALL LIST_IOPS
                      MEXIT
.*                AEND
.AIF_146_E          ANOP
.*            AEND
.AIF_145_1      ANOP
.*       AELSE
           AGO   .AIF_144_E
.AIF_144_1 ANOP
              MNOTE 8,'IF MISSING CD STACK PTR FOR NOT'
              CALL LIST_IOPS
              MEXIT
.*       AEND
.AIF_144_E ANOP
.*       AEND
           AGO   (&ACALL_30_POP_CD_IOP_IX).ACL_30_1,.ACL_30_2,.ACL_30_3X
               ,.ACL_30_4,.ACL_30_5
.ACL_30_SKIP ANOP
.*
.* IOP GEN NOT
.*
.*       AENTRY IOP_GEN_NOT
           AGO   .ACL_27_SKIP
.ACL_27_IOP_GEN_NOT ANOP
.*       AIF   (&IOP_CD1(&I) GT 0)
           AIF   (NOT(&IOP_CD1(&I) GT 0)).AIF_148_1
&CD_PARM_IX    SETA &IOP_CD1(&I)
&CD_PREV_PARM  SETC ''
.*             ACALL IOP_GEN_CD   GEN CONDITION CODE
&ACALL_26_IOP_GEN_CD SETA    2
                 AGO   .ACL_26_IOP_GEN_CD
.ACL_26_2        ANOP
.*       AELSE
           AGO   .AIF_148_E
.AIF_148_1 ANOP
&IOP           SETA -&IOP_CD1(&I)
.*             AIF  (&IOP GT 0)
                 AIF  (NOT(&IOP GT 0)).AIF_149_1
                    GEN_COMP_BYTE ZCVT_WORKAREA+&IOP,1  RPI 1065
&CCT                SETC 'ZC_EQ'      IS IOP T/F CC TRUE
.*             AELSE
                 AGO   .AIF_149_E
.AIF_149_1       ANOP
                    MNOTE 8,'IF IOP &I MISSING CD1'
.*                  ACALL LIST_IOPS
&ACALL_25_LIST_IOPS   SETA    3
                      AGO   .ACL_25_LIST_IOPS
.ACL_25_3             ANOP
                    MEXIT
.*             AEND
.AIF_149_E       ANOP
.*       AEND
.AIF_148_E ANOP
.*       AIF   (&IOP_TF(&I))         SAVE NOT IOP T/F FOR IOP REF.
           AIF   (NOT(&IOP_TF(&I))).AIF_150_1          SAVE NOT IOP T/FX
                FOR IOP REF.
.*             AIF (&I GT &MAX_IOP_TF)
                 AIF (NOT(&I GT &MAX_IOP_TF)).AIF_151_1
&MAX_IOP_TF        SETA &I
.*             AEND
.AIF_151_1       ANOP
               GEN_MOVE_BYTE ZCVT_WORKAREA+&I,0  ASSUME NOT CCT FALSE
               GEN_BC &CCT,IF_&IE_TCNT(&IE_LVL)._IOP&I CONT
               GEN_MOVE_BYTE ZCVT_WORKAREA+&I,1  SET (NOT CCT) TRUE
               GEN_LABEL IF_&IE_TCNT(&IE_LVL)._IOP&I,IF_IOP_NOT
.*       AELSE                         EXIT IF NOT CCT
           AGO   .AIF_150_E
.AIF_150_1 ANOP
               GEN_BC &CCT,IF_&IE_TCNT(&IE_LVL)_&IE_BCNT(&IE_LVL)
.*       AEND
.AIF_150_E ANOP
.*       AEND
           AGO   (&ACALL_27_IOP_GEN_NOT).ACL_27_1
.ACL_27_SKIP ANOP
.*
.* IOP GEN AND
.*
.*       AENTRY IOP_GEN_AND
           AGO   .ACL_28_SKIP
.ACL_28_IOP_GEN_AND ANOP
.*       AIF   (&IOP_CD1(&I) GT 0)
           AIF   (NOT(&IOP_CD1(&I) GT 0)).AIF_152_1
&CD_PARM_IX    SETA &IOP_CD1(&I)
&CD_PREV_PARM  SETC ''
.*             ACALL IOP_GEN_CD   GEN CONDITION CODE
&ACALL_26_IOP_GEN_CD SETA    3
                 AGO   .ACL_26_IOP_GEN_CD
.ACL_26_3        ANOP
.*       AELSE
           AGO   .AIF_152_E
.AIF_152_1 ANOP
&IOP           SETA -&IOP_CD1(&I)
.*             AIF  (&IOP GT 0)
                 AIF  (NOT(&IOP GT 0)).AIF_153_1
                    GEN_COMP_BYTE ZCVT_WORKAREA+&IOP,1
&CCT                SETC 'ZC_EQ'      IS IOP T/F CC TRUE
.*             AELSE
                 AGO   .AIF_153_E
.AIF_153_1       ANOP
                    MNOTE 8,'IF IOP &I MISSING CD1'
.*                  ACALL LIST_IOPS
&ACALL_25_LIST_IOPS   SETA    4
                      AGO   .ACL_25_LIST_IOPS
.ACL_25_4             ANOP
                    MEXIT
.*             AEND
.AIF_153_E       ANOP
.*       AEND
.AIF_152_E ANOP
.*       AIF   (&IOP_TF(&I))         SAVE T/F FOR AND
           AIF   (NOT(&IOP_TF(&I))).AIF_154_1          SAVE T/F FOR AND
               GEN_MOVE_BYTE ZCVT_WORKAREA+&I,0   ASSUME FALSE
               GEN_BC 15-&CCT,IF_&IE_TCNT(&IE_LVL)_IOP&I CD1>F
.*       AELSE                          EXIT IF FALSE
           AGO   .AIF_154_E
.AIF_154_1 ANOP
               GEN_BC 15-&CCT,IF_&IE_TCNT(&IE_LVL)_&IE_BCNT(&IE_LVL)
.*       AEND
.AIF_154_E ANOP
.*       AIF   (&IOP_CD2(&I) GT 0)
           AIF   (NOT(&IOP_CD2(&I) GT 0)).AIF_155_1
&CD_PARM_IX    SETA &IOP_CD2(&I)
&CD_PREV_PARM  SETC ''
.*             ACALL IOP_GEN_CD   GEN CONDITION CODE
&ACALL_26_IOP_GEN_CD SETA    4
                 AGO   .ACL_26_IOP_GEN_CD
.ACL_26_4        ANOP
.*       AELSE
           AGO   .AIF_155_E
.AIF_155_1 ANOP
&IOP           SETA -&IOP_CD2(&I)
.*             AIF  (&IOP GT 0)
                 AIF  (NOT(&IOP GT 0)).AIF_156_1
                    GEN_COMP_BYTE ZCVT_WORKAREA+&IOP,1
&CCT                SETC 'ZC_EQ'      IS IOP T/F CC TRUE
.*             AELSE
                 AGO   .AIF_156_E
.AIF_156_1       ANOP
                    MNOTE 8,'IF IOP &I MISSING CD2'
.*                  ACALL LIST_IOPS
&ACALL_25_LIST_IOPS   SETA    5
                      AGO   .ACL_25_LIST_IOPS
.ACL_25_5             ANOP
                    MEXIT
.*             AEND
.AIF_156_E       ANOP
.*       AEND
.AIF_155_E ANOP
.*       AIF   (&IOP_TF(&I))         SAVE T/F FOR IOP REF
           AIF   (NOT(&IOP_TF(&I))).AIF_157_1          SAVE T/F FOR IOPX
                REF
.*             AIF (&I GT &MAX_IOP_TF)
                 AIF (NOT(&I GT &MAX_IOP_TF)).AIF_158_1
&MAX_IOP_TF        SETA &I
.*             AEND
.AIF_158_1       ANOP
               GEN_BC 15-&CCT,IF_&IE_TCNT(&IE_LVL)_IOP&I CD2>F
               GEN_MOVE_BYTE ZCVT_WORKAREA+&I,1     CD1 AND CD2 > T
               GEN_LABEL IF_&IE_TCNT(&IE_LVL)_IOP&I,IF_IOP_AND
.*       AELSE                               EXIT IF CD2 FALSE
           AGO   .AIF_157_E
.AIF_157_1 ANOP
               GEN_BC 15-&CCT,IF_&IE_TCNT(&IE_LVL)_&IE_BCNT(&IE_LVL)
.*       AEND
.AIF_157_E ANOP
.*       AEND
           AGO   (&ACALL_28_IOP_GEN_AND).ACL_28_1
.ACL_28_SKIP ANOP
.*
.* IOP GEN OR
.*
.*       AENTRY IOP_GEN_OR
           AGO   .ACL_29_SKIP
.ACL_29_IOP_GEN_OR ANOP
&GEN_CD  SETB 1
.*       AIF   (&IOP_CD1(&I) GT 0)
           AIF   (NOT(&IOP_CD1(&I) GT 0)).AIF_159_1
&CD_PARM_IX    SETA &IOP_CD1(&I)
&CD_PREV_PARM  SETC ''
.*             ACALL IOP_GEN_CD                GEN COND FROM CD1
&ACALL_26_IOP_GEN_CD SETA    5
                 AGO   .ACL_26_IOP_GEN_CD
.ACL_26_5        ANOP
.*       AELSE
           AGO   .AIF_159_E
.AIF_159_1 ANOP
&IOP           SETA -&IOP_CD1(&I)     GEN COMD FROM T/F
.*             AIF  (&IOP GT 0)
                 AIF  (NOT(&IOP GT 0)).AIF_160_1
.*                  AIF (&IOP EQ &I-1)
                      AIF (NOT(&IOP EQ &I-1)).AIF_161_1
&GEN_CD                 SETB 0
.*                  AELSE
                      AGO   .AIF_161_E
.AIF_161_1            ANOP
                        GEN_COMP_BYTE ZCVT_WORKAREA+&IOP,1
&CCT                    SETC 'ZC_EQ'      IS IOP COND TRUE
.*                  AEND
.AIF_161_E            ANOP
.*             AELSE
                 AGO   .AIF_160_E
.AIF_160_1       ANOP
                    MNOTE 8,'IF IOP &I MISSING CD1'
.*                  ACALL LIST_IOPS
&ACALL_25_LIST_IOPS   SETA    6
                      AGO   .ACL_25_LIST_IOPS
.ACL_25_6             ANOP
                    MEXIT
.*             AEND
.AIF_160_E       ANOP
.*       AEND
.AIF_159_E ANOP
.*       AIF   (&IOP_TF(&I))                SAVE T/F FOR OR
           AIF   (NOT(&IOP_TF(&I))).AIF_162_1                 SAVE T/F X
               FOR OR
.*             AIF (&I GT &MAX_IOP_TF)
                 AIF (NOT(&I GT &MAX_IOP_TF)).AIF_163_1
&MAX_IOP_TF        SETA &I
.*             AEND
.AIF_163_1       ANOP
               GEN_MOVE_BYTE ZCVT_WORKAREA+&I,1   ASSUME TRUE
.*       AEND
.AIF_162_1 ANOP
.*       AIF   (&GEN_CD)
           AIF   (NOT(&GEN_CD)).AIF_164_1
.*             AIF (&IOP_OR_END(&I) GT 0)
                 AIF (NOT(&IOP_OR_END(&I) GT 0)).AIF_165_1
                   GEN_BC &CCT,IF_&IE_TCNT(&IE_LVL)_IOP&IOP_OR_END(&I).X
               _T   USE OPTIIMIZED TRUE TARGET
.*             AELSE
                 AGO   .AIF_165_E
.AIF_165_1       ANOP
                   GEN_BC &CCT,IF_&IE_TCNT(&IE_LVL)_IOP&I._T
.*             AEND
.AIF_165_E       ANOP
.*       AEND
.AIF_164_1 ANOP
&GEN_CD  SETB 1
.*       AIF   (&IOP_CD2(&I) GT 0)
           AIF   (NOT(&IOP_CD2(&I) GT 0)).AIF_166_1
&CD_PARM_IX    SETA &IOP_CD2(&I)
&CD_PREV_PARM  SETC ''
.*             ACALL IOP_GEN_CD             GEN COMP FROM CD2
&ACALL_26_IOP_GEN_CD SETA    6
                 AGO   .ACL_26_IOP_GEN_CD
.ACL_26_6        ANOP
.*       AELSE
           AGO   .AIF_166_E
.AIF_166_1 ANOP
&IOP           SETA -&IOP_CD2(&I)     GEN COMP FROM T/F
.*             AIF  (&IOP GT 0)
                 AIF  (NOT(&IOP GT 0)).AIF_167_1
.*                  AIF (&IOP EQ &I-1)
                      AIF (NOT(&IOP EQ &I-1)).AIF_168_1
&GEN_CD                 SETB 0
.*                  AELSE
                      AGO   .AIF_168_E
.AIF_168_1            ANOP
                        GEN_COMP_BYTE ZCVT_WORKAREA+&IOP,1
&CCT                    SETC 'ZC_EQ'      IS IOP COND TRUE
.*                  AEND
.AIF_168_E            ANOP
.*             AELSE
                 AGO   .AIF_167_E
.AIF_167_1       ANOP
                    MNOTE 8,'IF IOP &I MISSING CD2'
.*                  ACALL LIST_IOPS
&ACALL_25_LIST_IOPS   SETA    7
                      AGO   .ACL_25_LIST_IOPS
.ACL_25_7             ANOP
                    MEXIT
.*             AEND
.AIF_167_E       ANOP
.*       AEND
.AIF_166_E ANOP
.*       AIF   (&IOP_TF(&I))         SAVE T/F FOR AND
           AIF   (NOT(&IOP_TF(&I))).AIF_169_1          SAVE T/F FOR AND
               GEN_BC &CCT,IF_&IE_TCNT(&IE_LVL)_IOP&I._T CD2 TRUE
               GEN_MOVE_BYTE ZCVT_WORKAREA+&I,0     CD1 OR CD2 > F
.*       AEND                       EXIT IF FALSE
.AIF_169_1 ANOP
.*       AIF   (&IOP_OR_END(&I) GT 0)     USE OPTIMIZED OR END TARGET
           AIF   (NOT(&IOP_OR_END(&I) GT 0)).AIF_170_1      USE OPTIMIZX
               ED OR END TARGET
               GEN_BC &CCT,IF_&IE_TCNT(&IE_LVL)_IOP&IOP_OR_END(&I)._T
.*       AELSE
           AGO   .AIF_170_E
.AIF_170_1 ANOP
               GEN_BC 15-&CCT,IF_&IE_TCNT(&IE_LVL)_&IE_BCNT(&IE_LVL)
.*       AEND
.AIF_170_E ANOP
.*       AIF (&IOP_OR_END(&I) EQ 0)
           AIF (NOT(&IOP_OR_END(&I) EQ 0)).AIF_171_1
             GEN_LABEL IF_&IE_TCNT(&IE_LVL)_IOP&I._T,IF_IOP_OR_TRUE
.*       AEND
.AIF_171_1 ANOP
.*       AEND
           AGO   (&ACALL_29_IOP_GEN_OR).ACL_29_1
.ACL_29_SKIP ANOP
.*
.* PUSH COND - PUSH CD_IOP_IX (INDEX OF FIRST CD PARM OR -IOP INDEX)
.*
.*       AENTRY PUSH_CD
           AGO   .ACL_24_SKIP
.ACL_24_PUSH_CD ANOP
&STK_CD_TOT SETA &STK_CD_TOT+1
&STK_CD(&STK_CD_TOT) SETA &CD_IOP_IX
.*       AIF   (&CD_IOP_IX GT 0) RPI 1065
           AIF   (NOT(&CD_IOP_IX GT 0)).AIF_172_1  RPI 1065
.*       AEND
.AIF_172_1 ANOP
.*       AEND
           AGO   (&ACALL_24_PUSH_CD).ACL_24_1,.ACL_24_2,.ACL_24_3,.ACL_X
               24_4
.ACL_24_SKIP ANOP
.*
.* PUSH OP - PUSH LOGICAL OPERATOR 1=NOT, 2=AND, 3=OR, 4=(
.*
.*       AENTRY PUSH_OP
           AGO   .ACL_19_SKIP
.ACL_19_PUSH_OP ANOP
&CD_PARM_TOT SETA 0             RESET CD PARM COUNTER AT LOP
&STK_OP_TOT SETA &STK_OP_TOT+1
&STK_OP(&STK_OP_TOT) SETA &IOP
.*       AEND
           AGO   (&ACALL_19_PUSH_OP).ACL_19_1,.ACL_19_2,.ACL_19_3,.ACL_X
               19_4
.ACL_19_SKIP ANOP
.*
.* POP OP - POP LOGICAL OPERATOR NOT, AND, OR, '''('''
.*
.*       AENTRY POP_OP
           AGO   .ACL_20_SKIP
.ACL_20_POP_OP ANOP
.*       AIF   (&STK_OP_TOT GE 1)
           AIF   (NOT(&STK_OP_TOT GE 1)).AIF_173_1
&IOP           SETA   &STK_OP(&STK_OP_TOT)
&STK_OP_TOT    SETA &STK_OP_TOT-1
.*       AELSE
           AGO   .AIF_173_E
.AIF_173_1 ANOP
               MNOTE 8,'IF POP OP ERROR - EMPTY STACK'
.*       AEND
.AIF_173_E ANOP
.*       AEND
           AGO   (&ACALL_20_POP_OP).ACL_20_1,.ACL_20_2,.ACL_20_3,.ACL_2X
               0_4,.ACL_20_5,.ACL_20_6,.ACL_20_7,.ACL_20_8
.ACL_20_SKIP ANOP
.*
.* IOP GEN CD - GEN CODE FOR CONDITION TEST STARTING AT CD_PARM_IX
.*
.*  CD_PARM_IX - INPUT STARTING PARM INDEX
.*  CCT        - SET   CONDITION CODE TEST TRUE (EQ,NE,LT,GT,LE,GE)
.*  Notes:
.*   1.  IF SUBJECT AND/OR RELATION OMITTED, USE LAST CD SUBJECT/REL
.*
.*       AENTRY IOP_GEN_CD
           AGO   .ACL_26_SKIP
.ACL_26_IOP_GEN_CD ANOP
&SAVE_FIRST_CD_PARM_IX SETA &CD_PARM_IX
&CD_PARM_IX SETA &CD_PARM_IX-1
&FIELD_SS1 SETB 1
.*       ACALL  GET_CD_PARM
&ACALL_31_GET_CD_PARM SETA    1
           AGO   .ACL_31_GET_CD_PARM
.ACL_31_1  ANOP
.*       AIF   (&FIELD_88_IX GT 0)
           AIF   (NOT(&FIELD_88_IX GT 0)).AIF_174_1
&LAST_CD_PARM_IX SETA 0
.*             ACALL IOP_GEN_88
&ACALL_32_IOP_GEN_88 SETA    1
                 AGO   .ACL_32_IOP_GEN_88
.ACL_32_1        ANOP
.*             AEXIT AENTRY
                 AGO   .ACL_26_E
.*       AELSEIF (&FIELD_IX GT 0)  COULD BE FIELD1 OR FIELD2
           AGO   .AIF_174_E
.AIF_174_1 ANOP
           AIF     (NOT(&FIELD_IX GT 0)).AIF_174_2   COULD BE FIELD1 ORX
                FIELD2
&FIELD1        SETC '&FIELD_NAME'
&FIELD1_IX     SETA &FIELD_IX
&FIELD_SS1     SETB 0
.*             ACALL GET_CD_PARM
&ACALL_31_GET_CD_PARM SETA    2
                 AGO   .ACL_31_GET_CD_PARM
.ACL_31_2        ANOP
.*             ACALL SET_CCT
&ACALL_33_SET_CCT SETA    1
                 AGO   .ACL_33_SET_CCT
.ACL_33_1        ANOP
.*             AIF ('&CCT' EQ '')  AND/OR FIELD2 (OMITTED FIELD1,REL)
                 AIF (NOT('&CCT' EQ '')  AND/OR FIELD2 (OMITTED FIELD1,X
               REL)).AIF_175_1
.*                 ACALL RESET_SS1
&ACALL_34_RESET_SS1  SETA    1
                     AGO   .ACL_34_RESET_SS1
.ACL_34_1            ANOP
&FIELD2            SETC '&FIELD_NAME'
&FIELD2_IX         SETA &FIELD_IX
.*                 ACALL SET_OMITTED_FIELD1
&ACALL_35_SET_OMITTED_FIELD1 SETA    1
                     AGO   .ACL_35_SET_OMITTED_FIELD1
.ACL_35_1            ANOP
.*                 ACALL SET_OMITTED_CCT
&ACALL_36_SET_OMITTED_CCT SETA    1
                     AGO   .ACL_36_SET_OMITTED_CCT
.ACL_36_1            ANOP
.*             AELSE
                 AGO   .AIF_175_E
.AIF_175_1       ANOP
&LAST_CD_PARM_IX   SETA &SAVE_FIRST_CD_PARM_IX
.*                 ACALL GET_FIELD2  AND/OR FIELD1,REL,FIELD2
&ACALL_37_GET_FIELD2 SETA    1
                     AGO   .ACL_37_GET_FIELD2
.ACL_37_1            ANOP
.*             AEND
.AIF_175_E       ANOP
.*       AELSE
           AGO   .AIF_174_E
.AIF_174_2 ANOP
&CD_PARM_IX    SETA &SAVE_FIRST_CD_PARM_IX
.*             ACALL SET_CCT
&ACALL_33_SET_CCT SETA    2
                 AGO   .ACL_33_SET_CCT
.ACL_33_2        ANOP
.*             ACALL RESET_SS1               RPI 1065
&ACALL_34_RESET_SS1 SETA    2
                 AGO   .ACL_34_RESET_SS1
.ACL_34_2        ANOP
.*             AIF ('&CCT' EQ '') NOT REL SO ASSUME FIELD2 LIT
                 AIF (NOT('&CCT' EQ '')).AIF_176_1  NOT REL SO ASSUME FX
               IELD2 LIT
&FIELD2            SETC '&FIELD_NAME'
&FIELD2_IX         SETA &FIELD_IX
.*                 ACALL SET_OMITTED_FIELD1
&ACALL_35_SET_OMITTED_FIELD1 SETA    2
                     AGO   .ACL_35_SET_OMITTED_FIELD1
.ACL_35_2            ANOP
.*                 ACALL SET_OMITTED_CCT   AND/OR LIT (OMIT FLD1,REL)
&ACALL_36_SET_OMITTED_CCT SETA    2
                     AGO   .ACL_36_SET_OMITTED_CCT
.ACL_36_2            ANOP
.*             AELSE
                 AGO   .AIF_176_E
.AIF_176_1       ANOP
&FIELD_SS1         SETB 0
.*                 ACALL  GET_CD_PARM
&ACALL_31_GET_CD_PARM SETA    3
                     AGO   .ACL_31_GET_CD_PARM
.ACL_31_3            ANOP
&FIELD2            SETC '&FIELD_NAME'
&FIELD2_IX         SETA &FIELD_IX
.*                 ACALL SET_OMITTED_FIELD1 AND/OR REL,FLD2 (OMIT FLD1)
&ACALL_35_SET_OMITTED_FIELD1 SETA    3
                     AGO   .ACL_35_SET_OMITTED_FIELD1
.ACL_35_3            ANOP
.*             AEND
.AIF_176_E       ANOP
.*       AEND
.AIF_174_E ANOP
.*
.* GEN COMP OF FIELD1 AND FIELD2
.*
.*       AIF  (&FIELD2_IX GT 0)
           AIF  (NOT(&FIELD2_IX GT 0)).AIF_177_1
.*            AIF  (&SYM_LVL(&FIELD2_IX) EQ 88)
                AIF  (NOT(&SYM_LVL(&FIELD2_IX) EQ 88)).AIF_178_1
                   GEN_COMP &SYM_NAME(&SYM_REF(&FIELD2_IX)),&FIELD2_IX,X
               &SYM_VAL(&FIELD2_IX),0
.*            AELSE
                AGO   .AIF_178_E
.AIF_178_1      ANOP
                   GEN_COMP &FIELD1,&FIELD1_IX,&FIELD2,&FIELD2_IX
.*            AEND
.AIF_178_E      ANOP
.*            AIF (&ZC_ERROR)  RPI 1065
                AIF (NOT(&ZC_ERROR)).AIF_179_1   RPI 1065
                  MEXIT
.*            AEND
.AIF_179_1      ANOP
.*       AELSE
           AGO   .AIF_177_E
.AIF_177_1 ANOP
.*           ACALL GEN_CD_COMP_LIT
&ACALL_38_GEN_CD_COMP_LIT SETA    1
               AGO   .ACL_38_GEN_CD_COMP_LIT
.ACL_38_1      ANOP
.*       AEND
.AIF_177_E ANOP
.*       ACALL GET_CD_PARM
&ACALL_31_GET_CD_PARM SETA    4
           AGO   .ACL_31_GET_CD_PARM
.ACL_31_4  ANOP
.*       AEND
.ACL_26_E  ANOP
           AGO   (&ACALL_26_IOP_GEN_CD).ACL_26_1,.ACL_26_2,.ACL_26_3,.AX
               CL_26_4,.ACL_26_5,.ACL_26_6
.ACL_26_SKIP ANOP
.*
.* SET OMITTED CCT
.*
.*       AENTRY SET_OMITTED_CCT
           AGO   .ACL_36_SKIP
.ACL_36_SET_OMITTED_CCT ANOP
&CD_PARM_IX SETA &CD_CCT_IX
&PARM    SETC '&SYSLIST(&CD_CCT_IX)'
.*       ACALL SET_CCT
&ACALL_33_SET_CCT SETA    3
           AGO   .ACL_33_SET_CCT
.ACL_33_3  ANOP
&CD_PARM_IX SETA &SAVE_CUR_CD_IX RETURN TO CUR PARMS
.*       AEND
           AGO   (&ACALL_36_SET_OMITTED_CCT).ACL_36_1,.ACL_36_2
.ACL_36_SKIP ANOP
.*
.* SET CCT IF RELATION FOUND ELSE CCT NULL AND CD_PARM_IX UNCHANGED
.*
.*       AENTRY SET_CCT
           AGO   .ACL_33_SKIP
.ACL_33_SET_CCT ANOP
&CCT     SETC ''     ASSUME RELATION NOT FOUND
&TESTCLS SETB 0  ASSUME NOT CLASS TEST
.*       AIF ('&PARM' EQ 'IS')
           AIF (NOT('&PARM' EQ 'IS')).AIF_180_1
.*           ACALL GET_CD_PARM
&ACALL_31_GET_CD_PARM SETA    5
               AGO   .ACL_31_GET_CD_PARM
.ACL_31_5      ANOP
.*       AEND
.AIF_180_1 ANOP
.*       AIF   ('&PARM' EQ 'NOT')
           AIF   (NOT('&PARM' EQ 'NOT')).AIF_181_1
.*             ACALL GET_CD_PARM
&ACALL_31_GET_CD_PARM SETA    6
                 AGO   .ACL_31_GET_CD_PARM
.ACL_31_6        ANOP
.*             AIF   ('&PARM' EQ '='                                   X
                      OR '&PARM' EQ 'EQUAL'                            X
                      OR '&PARM' EQ 'EQUALS')
                 AIF   (NOT('&PARM' EQ '='                             X
                            OR '&PARM' EQ 'EQUAL'                      X
                            OR '&PARM' EQ 'EQUALS')).AIF_182_1
&CCT               SETC 'ZC_NE'
.*             AELSEIF ('&PARM' EQ '<'                                 X
                        OR '&PARM' EQ 'LESS')
                 AGO   .AIF_182_E
.AIF_182_1       ANOP
                 AIF     (NOT('&PARM' EQ '<'                           X
                              OR '&PARM' EQ 'LESS')).AIF_182_2
&CCT               SETC 'ZC_GE'
.*             AELSEIF   ('&PARM' EQ '>'                               X
                          OR '&PARM' EQ 'GREATER')
                 AGO   .AIF_182_E
.AIF_182_2       ANOP
                 AIF       (NOT('&PARM' EQ '>'                         X
                                OR '&PARM' EQ 'GREATER')).AIF_182_3
&CCT               SETC 'ZC_LE'
.*             AELSEIF ('&PARM' EQ '<=')     RPI 1065
                 AGO   .AIF_182_E
.AIF_182_3       ANOP
                 AIF     (NOT('&PARM' EQ '<=')).AIF_182_4      RPI 1065
&CCT               SETC 'ZC_GT'
.*             AELSEIF   ('&PARM' EQ '>=')   RPI 1065
                 AGO   .AIF_182_E
.AIF_182_4       ANOP
                 AIF       (NOT('&PARM' EQ '>=')).AIF_182_5    RPI 1065
&CCT               SETC 'ZC_LT'
.*             AELSEIF   ('&PARM' EQ 'SPACES'                          X
                          OR '&PARM' EQ 'SPACE'                        X
                          OR '&PARM' EQ 'ZERO'                         X
                          OR '&PARM' EQ 'ZEROS'                        X
                          OR '&PARM' EQ 'ZEROES'                       X
                          OR '&PARM' EQ 'LOW-VALUES'                   X
                          OR '&PARM' EQ 'LOW-VALUE'                    X
                          OR '&PARM' EQ 'HIGH-VALUES'                  X
                          OR '&PARM' EQ 'HIGH-VALUE'                   X
                          OR '&PARM' EQ 'NUMERIC'                      X
                          OR '&PARM' EQ 'ALPHABETIC'                   X
                          OR '&PARM' EQ 'ALPHANUMERIC')
                 AGO   .AIF_182_E
.AIF_182_5       ANOP
                 AIF       (NOT('&PARM' EQ 'SPACES'                    X
                                OR '&PARM' EQ 'SPACE'                  X
                                OR '&PARM' EQ 'ZERO'                   X
                                OR '&PARM' EQ 'ZEROS'                  X
                                OR '&PARM' EQ 'ZEROES'                 X
                                OR '&PARM' EQ 'LOW-VALUES'             X
                                OR '&PARM' EQ 'LOW-VALUE'              X
                                OR '&PARM' EQ 'HIGH-VALUES'            X
                                OR '&PARM' EQ 'HIGH-VALUE'             X
                                OR '&PARM' EQ 'NUMERIC'                X
                                OR '&PARM' EQ 'ALPHABETIC'             X
                                OR '&PARM' EQ 'ALPHANUMERIC')).AIF_182_X
               6
&TESTCLS           SETB 1
&CCT               SETC 'ZC_NE'
.*             AELSE
                 AGO   .AIF_182_E
.AIF_182_6       ANOP
                   MNOTE 8,'IF OPERAND UNKNOWN - &PARM'
                   MEXIT
.*             AEND
.AIF_182_E       ANOP
.*       AELSE
           AGO   .AIF_181_E
.AIF_181_1 ANOP
.*             AIF   ('&PARM' EQ '='                                   X
                      OR '&PARM' EQ 'EQUAL'                            X
                      OR '&PARM' EQ 'EQUALS')
                 AIF   (NOT('&PARM' EQ '='                             X
                            OR '&PARM' EQ 'EQUAL'                      X
                            OR '&PARM' EQ 'EQUALS')).AIF_183_1
&CCT                 SETC 'ZC_EQ'
.*             AELSEIF   ('&PARM' EQ '<'                               X
                          OR '&PARM' EQ 'LESS')
                 AGO   .AIF_183_E
.AIF_183_1       ANOP
                 AIF       (NOT('&PARM' EQ '<'                         X
                                OR '&PARM' EQ 'LESS')).AIF_183_2
&CCT                 SETC 'ZC_L'
.*             AELSEIF   ('&PARM' EQ '>'                               X
                          OR '&PARM' EQ 'GREATER')
                 AGO   .AIF_183_E
.AIF_183_2       ANOP
                 AIF       (NOT('&PARM' EQ '>'                         X
                                OR '&PARM' EQ 'GREATER')).AIF_183_3
&CCT                 SETC 'ZC_H'
.*             AELSEIF ('&PARM' EQ '<=')     RPI 1065
                 AGO   .AIF_183_E
.AIF_183_3       ANOP
                 AIF     (NOT('&PARM' EQ '<=')).AIF_183_4      RPI 1065
&CCT               SETC 'ZC_LE'
.*             AELSEIF   ('&PARM' EQ '>=')   RPI 1065
                 AGO   .AIF_183_E
.AIF_183_4       ANOP
                 AIF       (NOT('&PARM' EQ '>=')).AIF_183_5    RPI 1065
&CCT               SETC 'ZC_GE'
.*             AELSEIF   ('&PARM' EQ 'SPACES'                          X
                          OR '&PARM' EQ 'SPACE'                        X
                          OR '&PARM' EQ 'ZERO'                         X
                          OR '&PARM' EQ 'ZEROS'                        X
                          OR '&PARM' EQ 'ZEROES'                       X
                          OR '&PARM' EQ 'LOW-VALUES'                   X
                          OR '&PARM' EQ 'LOW-VALUE'                    X
                          OR '&PARM' EQ 'HIGH-VALUES'                  X
                          OR '&PARM' EQ 'HIGH-VALUE'                   X
                          OR '&PARM' EQ 'NUMERIC'                      X
                          OR '&PARM' EQ 'ALPHABETIC'                   X
                          OR '&PARM' EQ 'ALPHANUMERIC')
                 AGO   .AIF_183_E
.AIF_183_5       ANOP
                 AIF       (NOT('&PARM' EQ 'SPACES'                    X
                                OR '&PARM' EQ 'SPACE'                  X
                                OR '&PARM' EQ 'ZERO'                   X
                                OR '&PARM' EQ 'ZEROS'                  X
                                OR '&PARM' EQ 'ZEROES'                 X
                                OR '&PARM' EQ 'LOW-VALUES'             X
                                OR '&PARM' EQ 'LOW-VALUE'              X
                                OR '&PARM' EQ 'HIGH-VALUES'            X
                                OR '&PARM' EQ 'HIGH-VALUE'             X
                                OR '&PARM' EQ 'NUMERIC'                X
                                OR '&PARM' EQ 'ALPHABETIC'             X
                                OR '&PARM' EQ 'ALPHANUMERIC')).AIF_183_X
               6
&TESTCLS           SETB 1
&CCT               SETC 'ZC_EQ'
.*             AEND
.AIF_183_6       ANOP
.AIF_183_E       ANOP
.*       AEND
.AIF_181_E ANOP
.*       AIF  ('&CCT' NE '')
           AIF  (NOT('&CCT' NE '')).AIF_184_1
.*            AIF  (NOT &TESTCLS)
                AIF  (NOT(NOT &TESTCLS)).AIF_185_1
.*                 ACALL GET_CD_PARM
&ACALL_31_GET_CD_PARM SETA    7
                     AGO   .ACL_31_GET_CD_PARM
.ACL_31_7            ANOP
.*            AEND
.AIF_185_1      ANOP
.*            AIF  ('&PARM' EQ 'THAN')
                AIF  (NOT('&PARM' EQ 'THAN')).AIF_186_1
.*                 ACALL GET_CD_PARM
&ACALL_31_GET_CD_PARM SETA    8
                     AGO   .ACL_31_GET_CD_PARM
.ACL_31_8            ANOP
.*            AEND
.AIF_186_1      ANOP
.*            AIF  ('&PARM' EQ 'TO')
                AIF  (NOT('&PARM' EQ 'TO')).AIF_187_1
.*                 ACALL GET_CD_PARM
&ACALL_31_GET_CD_PARM SETA    9
                     AGO   .ACL_31_GET_CD_PARM
.ACL_31_9            ANOP
.*            AEND
.AIF_187_1      ANOP
.*       AEND
.AIF_184_1 ANOP
.*       AEND
           AGO   (&ACALL_33_SET_CCT).ACL_33_1,.ACL_33_2,.ACL_33_3
.ACL_33_SKIP ANOP
.*
.* SET OMITTED FIELD1
.*
.*       AENTRY SET_OMITTED_FIELD1
           AGO   .ACL_35_SKIP
.ACL_35_SET_OMITTED_FIELD1 ANOP
.*       AIF (&LAST_CD_PARM_IX GT 0)
           AIF (NOT(&LAST_CD_PARM_IX GT 0)).AIF_188_1
&SAVE_CUR_CD_IX SETA &CD_PARM_IX
&CD_PARM_IX  SETA &LAST_CD_PARM_IX-1
.*           ACALL GET_CD_PARM   GET LAST FIELD1
&ACALL_31_GET_CD_PARM SETA    10
               AGO   .ACL_31_GET_CD_PARM
.ACL_31_10     ANOP
.*           AIF (&FIELD_IX EQ 0)
               AIF (NOT(&FIELD_IX EQ 0)).AIF_189_1
                 MNOTE 8,'IF LAST FIELD1 NOT FOUND - &FIELD_NAME'
                 MEXIT
.*           AEND
.AIF_189_1     ANOP
&FIELD1      SETC '&FIELD_NAME'
&FIELD1_IX   SETA &FIELD_IX
.*           ACALL GET_CD_PARM
&ACALL_31_GET_CD_PARM SETA    11
               AGO   .ACL_31_GET_CD_PARM
.ACL_31_11     ANOP
&CD_CCT_IX   SETA &CD_PARM_IX SAVE FOR OMITTED CCT
&CD_PARM_IX  SETA &SAVE_CUR_CD_IX RETURN TO CUR PARMS
.*       AELSE
           AGO   .AIF_188_E
.AIF_188_1 ANOP
             MNOTE 8,'IF LAST CD PARM IX FOR FIELD1 NOT FOUND'
             MEXIT
.*       AEND
.AIF_188_E ANOP
.*       AEND
           AGO   (&ACALL_35_SET_OMITTED_FIELD1).ACL_35_1,.ACL_35_2,.ACLX
               _35_3
.ACL_35_SKIP ANOP
.*
.* GET FIELD2
.*
.*       AENTRY GET_FIELD2
           AGO   .ACL_37_SKIP
.ACL_37_GET_FIELD2 ANOP
.*       AIF (&FIELD_IX GT 0)
           AIF (NOT(&FIELD_IX GT 0)).AIF_190_1
.*           ACALL RESET_SS1               RPI 1065
&ACALL_34_RESET_SS1 SETA    3
               AGO   .ACL_34_RESET_SS1
.ACL_34_3      ANOP
&FIELD2      SETC '&FIELD_NAME'
&FIELD2_IX   SETA &FIELD_IX
.*       AELSE
           AGO   .AIF_190_E
.AIF_190_1 ANOP
&FIELD2      SETC '&PARM'
&FIELD2_IX   SETA 0
.*       AEND
.AIF_190_E ANOP
.*       AEND
           AGO   (&ACALL_37_GET_FIELD2).ACL_37_1
.ACL_37_SKIP ANOP
.*
.* GEN COMP LIT
.*
.*       AENTRY GEN_CD_COMP_LIT
           AGO   .ACL_38_SKIP
.ACL_38_GEN_CD_COMP_LIT ANOP
.*       AIF ('&PARM' EQ 'DFHRESP')
           AIF (NOT('&PARM' EQ 'DFHRESP')).AIF_191_1
&DFHRESP     SETC '&SYSLIST(&CD_PARM_IX+2)'
.*           ACALL GET_DFHRESP
&ACALL_39_GET_DFHRESP SETA    1
               AGO   .ACL_39_GET_DFHRESP
.ACL_39_1      ANOP
             GEN_COMP &FIELD1,&FIELD1_IX,&DFHLIT,0
.*       AELSEIF ('&PARM' EQ 'DFHVALUE')
           AGO   .AIF_191_E
.AIF_191_1 ANOP
           AIF     (NOT('&PARM' EQ 'DFHVALUE')).AIF_191_2
&DFHVALUE    SETC '&SYSLIST(&CD_PARM_IX+2)'
.*           ACALL GET_DFHVALUE
&ACALL_40_GET_DFHVALUE SETA    1
               AGO   .ACL_40_GET_DFHVALUE
.ACL_40_1      ANOP
             GEN_COMP &FIELD1,&FIELD1_IX,&DFHLIT,0
.*       AELSE
           AGO   .AIF_191_E
.AIF_191_2 ANOP
             GEN_COMP &FIELD1,&FIELD1_IX,&FIELD2,&FIELD2_IX
.*       AEND
.AIF_191_E ANOP
.*       AIF (&ZC_ERROR)  RPI 1065
           AIF (NOT(&ZC_ERROR)).AIF_192_1   RPI 1065
             MEXIT
.*       AEND
.AIF_192_1 ANOP
.*       AEND
           AGO   (&ACALL_38_GEN_CD_COMP_LIT).ACL_38_1
.ACL_38_SKIP ANOP
.*
.*
.* IOP GEN 88 - GEN 1 OR MORE CONDITION 88 LEVEL COMPARES
.*
.*       AENTRY IOP_GEN_88
           AGO   .ACL_32_SKIP
.ACL_32_IOP_GEN_88 ANOP
&FIELD1  SETC '&FIELD_NAME'
&FIELD1_IX SETA &FIELD_IX
&V88_IX  SETA &SYM_LOC(&FIELD_88_IX)  FIRST VALUE INDEX
&V88_MULT SETB 0                       ASSUME NOT MULT
&CCT     SETC 'ZC_EQ'       NO  THEN EQ = OK
.*       AWHILE (&V88_IX GT 0)
.AWH_28_T  ANOP
           AIF    (NOT(&V88_IX GT 0)).AWH_28_E
               GEN_COMP &FIELD1,&FIELD1_IX,&V88_VAL(&V88_IX),0
.*             AIF (&ZC_ERROR)  RPI 1065
                 AIF (NOT(&ZC_ERROR)).AIF_193_1   RPI 1065
                   MEXIT
.*             AEND
.AIF_193_1       ANOP
&V88_IX        SETA &V88_NEXT(&V88_IX)
.*             AIF (&V88_IX GT 0)
                 AIF (NOT(&V88_IX GT 0)).AIF_194_1
.*                 AIF (&IOP_OP(&I) NE 1)
                     AIF (NOT(&IOP_OP(&I) NE 1)).AIF_195_1
.*                     AIF (NOT &V88_MULT)
                         AIF (NOT(NOT &V88_MULT)).AIF_196_1
&V88_MULT                  SETB 1
.*                     AEND
.AIF_196_1               ANOP
                       GEN_BC &CCT,IF_&IE_TCNT(&IE_LVL)_IOP_&I
.*                 AELSE
                     AGO   .AIF_195_E
.AIF_195_1           ANOP
                       GEN_BC &CCT,IF_&IE_TCNT(&IE_LVL)_&IE_BCNT(&IE_LVX
               L)
.*                 AEND
.AIF_195_E           ANOP
.*             AEND
.AIF_194_1       ANOP
.*       AEND
           AGO   .AWH_28_T
.AWH_28_E  ANOP
.*       AEND
           AGO   (&ACALL_32_IOP_GEN_88).ACL_32_1
.ACL_32_SKIP ANOP
.*
.* GET NEXT IF PARM (SUBSTITUTE DFHRESP(CODE) WITH LITERAL VALUE)
.*
.*       AENTRY GET_PARM
           AGO   .ACL_18_SKIP
.ACL_18_GET_PARM ANOP
&PREV_PARM SETC '&PARM'
&PARM_IX SETA &PARM_IX+1
.*       AIF (&PARM_IX LE &NP)
           AIF (NOT(&PARM_IX LE &NP)).AIF_197_1
&PARM        SETC '&SYSLIST(&PARM_IX)'
.*       AELSE
           AGO   .AIF_197_E
.AIF_197_1 ANOP
.*           AIF (&LVL NE 0)
               AIF (NOT(&LVL NE 0)).AIF_198_1
                 MNOTE 8,'IF UNBALANCED (...)'
.*           AEND
.AIF_198_1     ANOP
&PARM        SETC ''
.*       AEND
.AIF_197_E ANOP
.*       AEND
           AGO   (&ACALL_18_GET_PARM).ACL_18_1,.ACL_18_2,.ACL_18_3,.ACLX
               _18_4,.ACL_18_5
.ACL_18_SKIP ANOP
.*
.* GET CD PARM USING CD_PARM_IX UNTIL NEXT LOG OP
.*
.*       AENTRY GET_CD_PARM
           AGO   .ACL_31_SKIP
.ACL_31_GET_CD_PARM ANOP
&PREV_PARM SETC '&PARM'
&CD_PARM_IX SETA &CD_PARM_IX+1
.*       AIF   (&CD_PARM_IX GE 1 AND &CD_PARM_IX LE &NP)
           AIF   (NOT(&CD_PARM_IX GE 1 AND &CD_PARM_IX LE &NP)).AIF_199X
               _1
&PARM          SETC '&SYSLIST(&CD_PARM_IX)'
.*             AIF ('&PARM' EQ '''('''                                 X
                    OR '&PARM' EQ ''')'''                              X
                    OR '&PARM' EQ 'AND'                                X
                    OR '&PARM' EQ 'OR') RPI 1065 AND/OR FORCES NEW COMP
                 AIF (NOT('&PARM' EQ '''('''                           X
                          OR '&PARM' EQ ''')'''                        X
                          OR '&PARM' EQ 'AND'                          X
                          OR '&PARM' EQ 'OR')).AIF_200_1  RPI 1065 AND/X
               OR FORCES NEW COMP
&PARM              SETC ''
.*             AELSEIF ('&PARM' EQ '+' OR '&PARM' EQ '-')
                 AGO   .AIF_200_E
.AIF_200_1       ANOP
                 AIF     (NOT('&PARM' EQ '+' OR '&PARM' EQ '-')).AIF_20X
               0_2
&PARM              SETC '&PARM&SYSLIST(&CD_PARM_IX+1)'
&CD_PARM_IX        SETA &CD_PARM_IX+1
.*             AELSEIF (('&PARM' EQ '<' OR '&PARM' EQ '>')             X
                       AND '&SYSLIST(&CD_PARM_IX+1)' EQ '=')  RPI 1065
                 AGO   .AIF_200_E
.AIF_200_2       ANOP
                 AIF     (NOT(('&PARM' EQ '<' OR '&PARM' EQ '>')       X
                             AND '&SYSLIST(&CD_PARM_IX+1)' EQ '=')).AIFX
               _200_3   RPI 1065
&PARM              SETC '&PARM&SYSLIST(&CD_PARM_IX+1)'
&CD_PARM_IX        SETA &CD_PARM_IX+1
.*             AELSEIF ('&PARM'(1,1) LT '0'                            X
                        AND '&PARM'(1,1) NE ''''                       X
                        AND '&PARM'(1,1) NE '.') RPI 1065 FIND FIELD
                 AGO   .AIF_200_E
.AIF_200_3       ANOP
                 AIF     (NOT('&PARM'(1,1) LT '0'                      X
                              AND '&PARM'(1,1) NE ''''                 X
                              AND '&PARM'(1,1) NE '.')).AIF_200_4  RPI X
               1065 FIND FIELD
&SAVE_PARM_IX      SETA &PARM_IX
&PARM_IX           SETA &CD_PARM_IX
&FIELD_REG         SETC 'ZC_R2'
.*                 ACALL GET_PARM_FIELD
&ACALL_6_GET_PARM_FIELD SETA    12
                     AGO   .ACL_6_GET_PARM_FIELD
.ACL_6_12            ANOP
&PARM              SETC '&FIELD_NAME'
&CD_PARM_IX        SETA &PARM_IX-1
&PARM_IX           SETA &SAVE_PARM_IX
.*             AELSE
                 AGO   .AIF_200_E
.AIF_200_4       ANOP
&FIELD_NAME        SETC '&PARM'
&FIELD_IX          SETA 0
.*             AEND
.AIF_200_E       ANOP
.*       AELSE
           AGO   .AIF_199_E
.AIF_199_1 ANOP
&PARM          SETC ''
.*       AEND
.AIF_199_E ANOP
.*       AIF   (&CD_PARM_IX LT 0)
           AIF   (NOT(&CD_PARM_IX LT 0)).AIF_201_1
&CD_PARM_IX    SETA 0
.*       AEND
.AIF_201_1 ANOP
.*       AEND
           AGO   (&ACALL_31_GET_CD_PARM).ACL_31_1,.ACL_31_2,.ACL_31_3,.X
               ACL_31_4,.ACL_31_5,.ACL_31_6,.ACL_31_7,.ACL_31_8,.ACL_31X
               _9,.ACL_31_10,.ACL_31_11
.ACL_31_SKIP ANOP
.*
.* LIST IOPS FOR AID IN DEBUGGING
.*
.*       AENTRY LIST_IOPS
           AGO   .ACL_25_SKIP
.ACL_25_LIST_IOPS ANOP
&J       SETA 1
         MNOTE 'ZCOBOL IOP TABLE LISTING - TOTAL IOPS = &IOP_TOT'
.*       AWHILE (&J LE &IOP_TOT)
.AWH_29_T  ANOP
           AIF    (NOT(&J LE &IOP_TOT)).AWH_29_E
               MNOTE 'IOP=&J OP=&IOP_OP(&J) CD1=&IOP_CD1(&J) CD2=&IOP_CX
               D2(&J) TF=&IOP_TF(&J) OR-END=&IOP_OR_END(&J)'
&J             SETA &J+1
.*       AEND
           AGO   .AWH_29_T
.AWH_29_E  ANOP
.*       AEND
           AGO   (&ACALL_25_LIST_IOPS).ACL_25_1,.ACL_25_2,.ACL_25_3,.ACX
               L_25_4,.ACL_25_5,.ACL_25_6,.ACL_25_7
.ACL_25_SKIP ANOP
.*
.* RESET SS1 - REMOVE LEN FROM OFF(LEN,REG) OR NAME(LEN)
.*
.*       AENTRY RESET_SS1
           AGO   .ACL_34_SKIP
.ACL_34_RESET_SS1 ANOP
&K       SETA ('&FIELD_NAME' FIND '(')
.*       AIF   ('&FIELD_NAME'(1,1) GE '0')  IS THIS O(L,R)
           AIF   (NOT('&FIELD_NAME'(1,1) GE '0')  IS THIS O(L,R)).AIF_2X
               02_1
.*             AIF (&K GT 1)
                 AIF (NOT(&K GT 1)).AIF_203_1
&FIELD_OFF         SETA '&FIELD_NAME'(1,&K-1)
&J                 SETA ('&FIELD_NAME' FIND ',')
.*                 AIF (&J GT &K+1)
                     AIF (NOT(&J GT &K+1)).AIF_204_1
&FIELD_LEN             SETA '&FIELD_NAME'(&K+1,&J-1)
&FIELD_REG             SETC '&FIELD_NAME'(&J+1,K'&FIELD_NAME-&J-1)
&FIELD_NAME            SETC '&FIELD_NAME'(1,&K).'&FIELD_NAME'(&J+1,*)  X
               O(R)
.*                 AEND
.AIF_204_1           ANOP
.*             AEND
.AIF_203_1       ANOP
.*       AELSEIF (&K GT 1)                        RPI 1065
           AGO   .AIF_202_E
.AIF_202_1 ANOP
           AIF     (NOT(&K GT 1)).AIF_202_2                         RPIX
                1065
&FIELD_NAME    SETC '&FIELD_NAME'(1,&K-1)
.*       AEND
.AIF_202_2 ANOP
.AIF_202_E ANOP
.*       AEND
           AGO   (&ACALL_34_RESET_SS1).ACL_34_1,.ACL_34_2,.ACL_34_3
.ACL_34_SKIP ANOP
         COPY ZCGETFLD
         COPY ZCDFHLIT
         MEND
         MACRO
.*********************************************************************
.* Copyright 2008 Automated Software Tools Corporation               *
.* This source code is part of z390 zCOBOL/assembler/emulator package*
.* The z390 package is distributed under GNU general public license  *
.* Author.  Don Higgins                                              *
.* Date   - 04/07/08                                                 *
.*********************************************************************
.* 06/13/08 INITIALIZE not supported yet
.*********************************************************************
         INITIALIZE
         MNOTE 8,'INITIALIZE NOT SUPPORTED YET'
         MEND
         MACRO
.*********************************************************************
.* Copyright 2008 Automated Software Tools Corporation               *
.* This source code is part of z390 zCOBOL/assembler/emulator package*
.* The z390 package is distributed under GNU general public license  *
.* Author - Don Higgins                                              *
.* Date   - 04/16/08                                                 *
.*********************************************************************
.* 02/20/09 stub for pending support
.*********************************************************************
         INITIATE
         MNOTE 8,'INITIATE NOT SUPPORTED YET'
         MEXIT
         MEND
         MACRO
.*********************************************************************
.* Copyright 2008 Automated Software Tools Corporation               *
.* This source code is part of z390 zCOBOL/assembler/emulator package*
.* The z390 package is distributed under GNU general public license  *
.* All rights reserved                                               *
.* Author - Don Higgins                                              *
.* Date   - 04/14/08                                                 *
.*********************************************************************
.* 04/14/08 initial coding for global ZCOBOL
.*********************************************************************
         INPUT_OUTPUT
         MEND
         MACRO
.*********************************************************************
.* Copyright 2008 Automated Software Tools Corporation               *
.* This source code is part of z390 zCOBOL/assembler/emulator package*
.* The z390 package is distributed under GNU general public license  *
.* Author - Don Higgins                                              *
.* Date   - 04/16/08                                                 *
.*********************************************************************
.* 11/17/08 stub for pending support
.* 01/14/09 initial coding
.* 07/19/09 RPI fix FIELD_REC ZC_R3
.*********************************************************************
.* R2=V1,R3=V2   THESE MAY BE PASSED MULTIPLE TIMES TO GEN/ZC390LIB
.* R14=V3,R15=V4 THESE MUST BE RESET FOR EACH CALL TO GEN
.* R1,R14,R15 USED IN STANDARD LINKAGE FROM GEN TO ZC390LIB INSPECT.MLC
.*********************************************************************
         INSPECT
         COPY ZC_WS
&TEST_OK SETB 0
&TEST_TYPE SETC ''
&TEST_VAR SETC ''
&TEST_IX SETA 0
&PARM_IX SETA 1
&FIELD_REG SETC 'ZC_R2'  R2=V1 INSPECT FIELD IF SUBSCRIPTED
&FIELD_SS1 SETB 0
.*       ACALL GET_PARM_FIELD
&ACALL_6_GET_PARM_FIELD SETA    13
           AGO   .ACL_6_GET_PARM_FIELD
.ACL_6_13  ANOP
.*       AIF   (&FIELD_IX EQ 0)
           AIF   (NOT(&FIELD_IX EQ 0)).AIF_205_1
               MNOTE 8,'INSPECT FIELD NOT FOUND &SYSLIST(1)'
               MEXIT
.*       AEND
.AIF_205_1 ANOP
&V1      SETC '&FIELD_NAME'
&V1_IX   SETA &FIELD_IX
.*       AIF   ('&SYSLIST(&PARM_IX)' EQ 'TALLYING')
           AIF   (NOT('&SYSLIST(&PARM_IX)' EQ 'TALLYING')).AIF_206_1
&PARM_IX       SETA &PARM_IX+1
.*             ACALL TALLYING
&ACALL_41_TALLYING SETA    1
                 AGO   .ACL_41_TALLYING
.ACL_41_1        ANOP
.*       AELSEIF ('&SYSLIST(&PARM_IX)' EQ 'REPLACING')
           AGO   .AIF_206_E
.AIF_206_1 ANOP
           AIF     (NOT('&SYSLIST(&PARM_IX)' EQ 'REPLACING')).AIF_206_2
&PARM_IX       SETA &PARM_IX+1
.*             ACALL REPLACING
&ACALL_42_REPLACING SETA    1
                 AGO   .ACL_42_REPLACING
.ACL_42_1        ANOP
.*       AELSEIF ('&SYSLIST(&PARM_IX)' EQ 'CONVERTING')
           AGO   .AIF_206_E
.AIF_206_2 ANOP
           AIF     (NOT('&SYSLIST(&PARM_IX)' EQ 'CONVERTING')).AIF_206_X
               3
&PARM_IX       SETA &PARM_IX+1
.*             ACALL CONVERTING
&ACALL_43_CONVERTING SETA    1
                 AGO   .ACL_43_CONVERTING
.ACL_43_1        ANOP
.*       AELSE
           AGO   .AIF_206_E
.AIF_206_3 ANOP
               MNOTE 8,'INSPECT UNKNOWN PARM - &SYSLIST(&PARM_IX)'
.*       AEND
.AIF_206_E ANOP
         MEXIT
.*
.* INSPECT V1 TALLYING V2 FOR [CHARACTERS TEST,ALL/LEADING V3 TEST]
.*  TEST = BEFORE/AFTER [INITIAL] V4
.* NOTES:
.*  1.  TALLYING MAY ALSO BE FOLLOWED BY REPLACING USING THE SAME V1
.*
.*       AENTRY TALLYING
           AGO   .ACL_41_SKIP
.ACL_41_TALLYING ANOP
&TALLYING_CNT SETA 0
.*    AWHILE   ('&SYSLIST(&PARM_IX)' NE ''                             X
                AND '&SYSLIST(&PARM_IX)' NE 'REPLACING')
.AWH_30_T ANOP
        AIF      (NOT('&SYSLIST(&PARM_IX)' NE ''                       X
                      AND '&SYSLIST(&PARM_IX)' NE 'REPLACING')).AWH_30_X
               E
&TALLYING_CNT SETA &TALLYING_CNT+1
&FIELD_REG SETC 'ZC_R3'  R3=V2 TALLY VAR IF SUBSC  RPI 1065
&FIELD_SS1 SETB 0
.*       ACALL GET_PARM_FIELD
&ACALL_6_GET_PARM_FIELD SETA    14
           AGO   .ACL_6_GET_PARM_FIELD
.ACL_6_14  ANOP
.*       AIF   (&FIELD_IX EQ 0)
           AIF   (NOT(&FIELD_IX EQ 0)).AIF_207_1
               MNOTE 8,'INSPECT TALLYING FIELD NOT FOUND &SYSLIST(&FIELX
               D_IX)'
               MEXIT
.*       AEND
.AIF_207_1 ANOP
&V2      SETC '&FIELD_NAME'
&V2_IX   SETA &FIELD_IX
.*       AIF   ('&SYSLIST(&PARM_IX)' NE 'FOR')
           AIF   (NOT('&SYSLIST(&PARM_IX)' NE 'FOR')).AIF_208_1
               MNOTE 8,'INSPECT MISSING FOR AFTER TALLYING'
               MEXIT
.*       AEND
.AIF_208_1 ANOP
&PARM_IX SETA &PARM_IX+1
&STYPE_CNT SETA 0
.*     AWHILE  ('&SYSLIST(&PARM_IX)' NE ''                             X
                AND '&SYSLIST(&PARM_IX)' NE 'REPLACING')
.AWH_31_T ANOP
         AIF     (NOT('&SYSLIST(&PARM_IX)' NE ''                       X
                      AND '&SYSLIST(&PARM_IX)' NE 'REPLACING')).AWH_31_X
               E
&STYPE_CNT SETA &STYPE_CNT+1
.*       AIF   ('&SYSLIST(&PARM_IX)' EQ 'CHARACTERS')
           AIF   (NOT('&SYSLIST(&PARM_IX)' EQ 'CHARACTERS')).AIF_209_1
&STYPE         SETC 'CHARACTERS'
&PARM_IX       SETA &PARM_IX+1
&TEST_CNT      SETA 0
.*             AWHILE ('&SYSLIST(&PARM_IX)' NE ''                      X
                      AND '&SYSLIST(&PARM_IX)' NE 'REPLACING')
.AWH_32_T        ANOP
                 AIF    (NOT('&SYSLIST(&PARM_IX)' NE ''                X
                            AND '&SYSLIST(&PARM_IX)' NE 'REPLACING')).AX
               WH_32_E
&TEST_CNT          SETA &TEST_CNT+1
.*                 ACALL GET_TEST_PHRASE
&ACALL_44_GET_TEST_PHRASE SETA    1
                     AGO   .ACL_44_GET_TEST_PHRASE
.ACL_44_1            ANOP
.*                 AIF (&TEST_OK)
                     AIF (NOT(&TEST_OK)).AIF_210_1
                       GEN_INSPECT TALLYING,&STYPE,&V1,&V1_IX,&V2,&V2_IX
               X,&TEST_TYPE,&TEST_VAR,&TEST_IX
.*                 AEND
.AIF_210_1           ANOP
.*             AEND
                 AGO   .AWH_32_T
.AWH_32_E        ANOP
.*             AIF (&TEST_CNT EQ 0)
                 AIF (NOT(&TEST_CNT EQ 0)).AIF_211_1
                   MNOTE 8,'INSPECT TALLYING CHARACTERS MISSING TEST'
                   MEXIT
.*             AEND
.AIF_211_1       ANOP
.*       AELSEIF   ('&SYSLIST(&PARM_IX)' EQ 'ALL'                      X
                    OR '&SYSLIST(&PARM_IX)' EQ 'LEADING')
           AGO   .AIF_209_E
.AIF_209_1 ANOP
           AIF       (NOT('&SYSLIST(&PARM_IX)' EQ 'ALL'                X
                          OR '&SYSLIST(&PARM_IX)' EQ 'LEADING')).AIF_20X
               9_2
&STYPE         SETC '&SYSLIST(&PARM_IX)'
&PARM_IX       SETA &PARM_IX+1
&FIELD_REG     SETC 'ZC_R3'
&FIELD_SS1     SETB 0
.*             ACALL GET_PARM_FIELD
&ACALL_6_GET_PARM_FIELD SETA    15
                 AGO   .ACL_6_GET_PARM_FIELD
.ACL_6_15        ANOP
.*             AIF (&FIELD_IX GT 0                                     X
                    OR '&FIELD_NAME'(1,1) EQ '''')
                 AIF (NOT(&FIELD_IX GT 0                               X
                          OR '&FIELD_NAME'(1,1) EQ '''')).AIF_212_1
&V3                 SETC '&FIELD_NAME'
&V3_IX              SETA &FIELD_IX
&TEST_CNT           SETA 0
.*                  AWHILE ('&SYSLIST(&PARM_IX)' NE ''                 X
                           AND '&SYSLIST(&PARM_IX)' NE 'REPLACING')
.AWH_33_T             ANOP
                      AIF    (NOT('&SYSLIST(&PARM_IX)' NE ''           X
                                 AND '&SYSLIST(&PARM_IX)' NE 'REPLACINGX
               ')).AWH_33_E
&TEST_CNT              SETA &TEST_CNT+1
.*                     ACALL GET_TEST_PHRASE
&ACALL_44_GET_TEST_PHRASE SETA    2
                         AGO   .ACL_44_GET_TEST_PHRASE
.ACL_44_2                ANOP
.*                     AIF (&TEST_OK)
                         AIF (NOT(&TEST_OK)).AIF_213_1
                           GEN_INSPECT TALLYING,&STYPE,&V1,&V1_IX,&V2,&X
               V2_IX,&V3,&V3_IX,&TEST_TYPE,&TEST_VAR,&TEST_IX
.*                     AEND
.AIF_213_1               ANOP
.*                  AEND
                      AGO   .AWH_33_T
.AWH_33_E             ANOP
.*                  AIF (&TEST_CNT EQ 0)
                      AIF (NOT(&TEST_CNT EQ 0)).AIF_214_1
.*                      AIF ('&SYSLIST(&PARM_IX)' EQ '')
                          AIF (NOT('&SYSLIST(&PARM_IX)' EQ '')).AIF_215X
               _1
                           GEN_INSPECT TALLYING,&STYPE,&V1,&V1_IX,&V2,&X
               V2_IX,&V3,&V3_IX,0   NO TEST
.*                      AELSE
                          AGO   .AIF_215_E
.AIF_215_1                ANOP
                            MNOTE 8,'INSPECT TALLYING UNKNOWN CLAUSE - X
               &SYSLIST(&PARM_IX)'
                            MEXIT
.*                      AEND
.AIF_215_E                ANOP
.*                  AEND
.AIF_214_1            ANOP
.*             AELSE
                 AGO   .AIF_212_E
.AIF_212_1       ANOP
                    MNOTE 8,'INSPECT MISSING ALL IDENTIFIER'
                    MEXIT
.*             AEND
.AIF_212_E       ANOP
.*       AELSE
           AGO   .AIF_209_E
.AIF_209_2 ANOP
               MNOTE 8,'INSPECT MISSING CHARACTERS/ALL/LEADING'
               MEXIT
.*       AEND
.AIF_209_E ANOP
.*     AEND
         AGO   .AWH_31_T
.AWH_31_E ANOP
.*    AEND
        AGO   .AWH_30_T
.AWH_30_E ANOP
.*    AIF      (&TALLYING_CNT EQ 0)
        AIF      (NOT(&TALLYING_CNT EQ 0)).AIF_216_1
               MNOTE 8,'INSPECT TALLYING MISSING VAR'
               MEXIT
.*    AELSEIF  ('&SYSLIST(&PARM_IX)' EQ 'REPLACING')
        AGO   .AIF_216_E
.AIF_216_1 ANOP
        AIF      (NOT('&SYSLIST(&PARM_IX)' EQ 'REPLACING')).AIF_216_2
&PARM_IX       SETA &PARM_IX+1
.*             ACALL REPLACING
&ACALL_42_REPLACING SETA    2
                 AGO   .ACL_42_REPLACING
.ACL_42_2        ANOP
.*    AEND
.AIF_216_2 ANOP
.AIF_216_E ANOP
.*       AEND
           AGO   (&ACALL_41_TALLYING).ACL_41_1
.ACL_41_SKIP ANOP
.*
.* INSPECT V1 REPLACING [CHARACTERS BY V3 TEST,
.*                      ALL/LEADING/FIRST V2 BY V3 TEST]
.*                      TEST = BEFORE/AFTER [INITIAL] V4
.*
.*       AENTRY REPLACING
           AGO   .ACL_42_SKIP
.ACL_42_REPLACING ANOP
&REPLACING_CNT SETA 0
.*    AWHILE ('&SYSLIST(&PARM_IX)' NE '')
.AWH_34_T ANOP
        AIF    (NOT('&SYSLIST(&PARM_IX)' NE '')).AWH_34_E
&REPLACING_CNT SETA &REPLACING_CNT+1
.*       AIF   ('&SYSLIST(&PARM_IX)' EQ 'CHARACTERS'                   X
                AND '&SYSLIST(&PARM_IX+1)' EQ 'BY')
           AIF   (NOT('&SYSLIST(&PARM_IX)' EQ 'CHARACTERS'             X
                      AND '&SYSLIST(&PARM_IX+1)' EQ 'BY')).AIF_217_1
&STYPE         SETC 'CHARACTERS'
&PARM_IX       SETA &PARM_IX+2
&V2            SETC ''
&V2_IX         SETA 0
&FIELD_REG     SETC 'ZC_R3'
&FIELD_SS1     SETB 0
.*             ACALL GET_PARM_FIELD
&ACALL_6_GET_PARM_FIELD SETA    16
                 AGO   .ACL_6_GET_PARM_FIELD
.ACL_6_16        ANOP
.*             AIF (&FIELD_IX GT 0                                     X
                    OR '&FIELD_NAME'(1,1) EQ '''')
                 AIF (NOT(&FIELD_IX GT 0                               X
                          OR '&FIELD_NAME'(1,1) EQ '''')).AIF_218_1
&V3                 SETC '&FIELD_NAME'
&V3_IX              SETA &FIELD_IX
&TEST_CNT           SETA 0
.*                  AWHILE ('&SYSLIST(&PARM_IX)' NE ''                 X
                           AND '&SYSLIST(&PARM_IX)' NE 'REPLACING')
.AWH_35_T             ANOP
                      AIF    (NOT('&SYSLIST(&PARM_IX)' NE ''           X
                                 AND '&SYSLIST(&PARM_IX)' NE 'REPLACINGX
               ')).AWH_35_E
&TEST_CNT              SETA &TEST_CNT+1
.*                     ACALL GET_TEST_PHRASE
&ACALL_44_GET_TEST_PHRASE SETA    3
                         AGO   .ACL_44_GET_TEST_PHRASE
.ACL_44_3                ANOP
.*                     AIF (&TEST_OK)
                         AIF (NOT(&TEST_OK)).AIF_219_1
                           GEN_INSPECT REPLACING,&STYPE,&V1,&V1_IX,&V2,X
               &V2_IX,&V3,&V3_IX,&TEST_TYPE,&TEST_VAR,&TEST_IX
.*                     AEND
.AIF_219_1               ANOP
.*                  AEND
                      AGO   .AWH_35_T
.AWH_35_E             ANOP
.*                  AIF (&TEST_CNT EQ 0)
                      AIF (NOT(&TEST_CNT EQ 0)).AIF_220_1
                        MNOTE 8,'INSPECT REPLACING CHARACTERS MISSING TX
               EST'
                        MEXIT
.*                  AEND
.AIF_220_1            ANOP
.*             AELSE
                 AGO   .AIF_218_E
.AIF_218_1       ANOP
                    MNOTE 8,'INSPECT REPLACING CHARACTERS MISSING IDENTX
               IFIER'
                    MEXIT
.*             AEND
.AIF_218_E       ANOP
.*       AELSEIF   ('&SYSLIST(&PARM_IX)' EQ 'ALL'                      X
                    OR '&SYSLIST(&PARM_IX)' EQ 'LEADING'               X
                    OR '&SYSLIST(&PARM_IX)' EQ 'FIRST')
           AGO   .AIF_217_E
.AIF_217_1 ANOP
           AIF       (NOT('&SYSLIST(&PARM_IX)' EQ 'ALL'                X
                          OR '&SYSLIST(&PARM_IX)' EQ 'LEADING'         X
                          OR '&SYSLIST(&PARM_IX)' EQ 'FIRST')).AIF_217_X
               2
&STYPE         SETC '&SYSLIST(&PARM_IX)'
&PARM_IX       SETA &PARM_IX+1
&STYPE_CNT   SETA 0
.*           AWHILE ('&SYSLIST(&PARM_IX)' NE ''                        X
                     AND '&SYSLIST(&PARM_IX)' NE 'REPLACING')
.AWH_36_T      ANOP
               AIF    (NOT('&SYSLIST(&PARM_IX)' NE ''                  X
                           AND '&SYSLIST(&PARM_IX)' NE 'REPLACING')).AWX
               H_36_E
&STYPE_CNT     SETA &STYPE+1
&FIELD_REG     SETC 'ZC_R2'
&FIELD_SS1     SETB 0
.*             ACALL GET_PARM_FIELD
&ACALL_6_GET_PARM_FIELD SETA    17
                 AGO   .ACL_6_GET_PARM_FIELD
.ACL_6_17        ANOP
.*             AIF (&FIELD_IX GT 0                                     X
                    OR '&FIELD_NAME'(1,1) EQ '''')
                 AIF (NOT(&FIELD_IX GT 0                               X
                          OR '&FIELD_NAME'(1,1) EQ '''')).AIF_221_1
&V2             SETC '&FIELD_NAME'
&V2_IX          SETA &FIELD_IX
.*              AIF ('&SYSLIST(&PARM_IX)' EQ 'BY')
                  AIF (NOT('&SYSLIST(&PARM_IX)' EQ 'BY')).AIF_222_1
&PARM_IX          SETA &PARM_IX+1
.*              AELSE
                  AGO   .AIF_222_E
.AIF_222_1        ANOP
                  MNOTE 8,'INSPECT REPLACING MISSING BY'
                  MEXIT
.*              AEND
.AIF_222_E        ANOP
&FIELD_REG      SETC 'ZC_R3'
&FIELD_SS1      SETB 0
.*              ACALL GET_PARM_FIELD
&ACALL_6_GET_PARM_FIELD SETA    18
                  AGO   .ACL_6_GET_PARM_FIELD
.ACL_6_18         ANOP
.*              AIF (&FIELD_IX GT 0                                    X
                       OR '&FIELD_NAME'(1,1) EQ '''')
                  AIF (NOT(&FIELD_IX GT 0                              X
                             OR '&FIELD_NAME'(1,1) EQ '''')).AIF_223_1
&V3                 SETC '&FIELD_NAME'
&V3_IX              SETA &FIELD_IX
&TEST_CNT           SETA 0
.*                  AWHILE ('&SYSLIST(&PARM_IX)' NE ''                 X
                           AND '&SYSLIST(&PARM_IX)' NE 'REPLACING')
.AWH_37_T             ANOP
                      AIF    (NOT('&SYSLIST(&PARM_IX)' NE ''           X
                                 AND '&SYSLIST(&PARM_IX)' NE 'REPLACINGX
               ')).AWH_37_E
&TEST_CNT              SETA &TEST_CNT+1
.*                      ACALL GET_TEST_PHRASE
&ACALL_44_GET_TEST_PHRASE SETA    4
                          AGO   .ACL_44_GET_TEST_PHRASE
.ACL_44_4                 ANOP
.*                      AIF (&TEST_OK)
                          AIF (NOT(&TEST_OK)).AIF_224_1
                            GEN_INSPECT REPLACING,&STYPE,&V1,&V1_IX,&V2X
               ,&V2_IX,&V3,&V3_IX,&TEST_TYPE,&TEST_VAR,&TEST_IX
.*                      AEND
.AIF_224_1                ANOP
.*                  AEND
                      AGO   .AWH_37_T
.AWH_37_E             ANOP
.*                  AIF (&TEST_CNT EQ 0)
                      AIF (NOT(&TEST_CNT EQ 0)).AIF_225_1
                        MNOTE 8,'INSPECT REPLACING ALL/LEADING/FIRST MIX
               SSING TEST'
                        MEXIT
.*                  AEND
.AIF_225_1            ANOP
.*               AELSE
                   AGO   .AIF_223_E
.AIF_223_1         ANOP
                    MNOTE 8,'INSPECT REPLACING UNDEFINED ID3'
                    MEXIT
.*               AEND
.AIF_223_E         ANOP
.*             AELSE
                 AGO   .AIF_221_E
.AIF_221_1       ANOP
                    MNOTE 8,'INSPECT REPLACING MISSING ID2'
                    MEXIT
.*             AEND
.AIF_221_E       ANOP
.*           AEND
               AGO   .AWH_36_T
.AWH_36_E      ANOP
.*           AIF (&STYPE_CNT EQ 0)
               AIF (NOT(&STYPE_CNT EQ 0)).AIF_226_1
                 MNOTE 8,'INSPECT REPLACING &STYPE MISSING VAR'
                 MEXIT
.*           AEND
.AIF_226_1     ANOP
.*       AELSE
           AGO   .AIF_217_E
.AIF_217_2 ANOP
               MNOTE 8,'INSPECT MISSING CHARACTERS/ALL/LEADING/FIRST'
               MEXIT
.*       AEND
.AIF_217_E ANOP
.*     AEND
         AGO   .AWH_34_T
.AWH_34_E ANOP
.*     AIF     (&REPLACING_CNT EQ 0)
         AIF     (NOT(&REPLACING_CNT EQ 0)).AIF_227_1
               MNOTE 8,'INSPECT REPLACING MISSING VAR'
               MEXIT
.*     AEND
.AIF_227_1 ANOP
.*       AEND
           AGO   (&ACALL_42_REPLACING).ACL_42_1,.ACL_42_2
.ACL_42_SKIP ANOP
.*
.* INSPECT V1 CONVERTING V2 TO V3 TEST
.*  TEST = BEFORE/AFTER [INITIAL] V4
.*
.*       AENTRY CONVERTING
           AGO   .ACL_43_SKIP
.ACL_43_CONVERTING ANOP
&FIELD_REG SETC 'ZC_R2'
&FIELD_SS1 SETB 0
.*       ACALL GET_PARM_FIELD
&ACALL_6_GET_PARM_FIELD SETA    19
           AGO   .ACL_6_GET_PARM_FIELD
.ACL_6_19  ANOP
.*       AIF   (&FIELD_IX EQ 0                                         X
               AND '&FIELD_NAME' EQ '')
           AIF   (NOT(&FIELD_IX EQ 0                                   X
                     AND '&FIELD_NAME' EQ '')).AIF_228_1
               MNOTE 8,'INSPECT CONVERTING FROM FIELD NOT FOUND'
               MEXIT
.*       AEND
.AIF_228_1 ANOP
&V2      SETC '&FIELD_NAME'
&V2_IX   SETA &FIELD_IX
.*       AIF   ('&SYSLIST(&PARM_IX)' NE 'TO')
           AIF   (NOT('&SYSLIST(&PARM_IX)' NE 'TO')).AIF_229_1
               MNOTE 8,'INSPECT CONVERTING MISSING TO'
               MEXIT
.*       AEND
.AIF_229_1 ANOP
&PARM_IX SETA &PARM_IX+1
&FIELD_REG SETC 'ZC_R3'
&FIELD_SS1 SETB 0
.*       ACALL GET_PARM_FIELD
&ACALL_6_GET_PARM_FIELD SETA    20
           AGO   .ACL_6_GET_PARM_FIELD
.ACL_6_20  ANOP
.*       AIF   (&FIELD_IX EQ 0                                         X
               AND '&FIELD_NAME' EQ '')
           AIF   (NOT(&FIELD_IX EQ 0                                   X
                     AND '&FIELD_NAME' EQ '')).AIF_230_1
              MNOTE 8,'INSPECT MISSING ALL IDENTIFIER'
              MEXIT
.*       AEND
.AIF_230_1 ANOP
&V3      SETC '&FIELD_NAME'
&V3_IX   SETA &FIELD_IX

.*       AIF   ('&SYSLIST(&PARM_IX)' NE '')
           AIF   (NOT('&SYSLIST(&PARM_IX)' NE '')).AIF_231_1
.*             ACALL GET_TEST_PHRASE
&ACALL_44_GET_TEST_PHRASE SETA    5
                 AGO   .ACL_44_GET_TEST_PHRASE
.ACL_44_5        ANOP
.*       AELSE
           AGO   .AIF_231_E
.AIF_231_1 ANOP
&TEST_OK       SETB 1
.*       AEND
.AIF_231_E ANOP
.*       AIF (&TEST_OK)
           AIF (NOT(&TEST_OK)).AIF_232_1
               GEN_INSPECT CONVERTING,&V1,&V1_IX,&V2,&V2_IX,&V3,&V3_IX,X
               &TEST_TYPE,&TEST_VAR,&TEST_IX
.*       AEND
.AIF_232_1 ANOP
.*       AEND
           AGO   (&ACALL_43_CONVERTING).ACL_43_1
.ACL_43_SKIP ANOP
.*
.* GET TEST PHRASE - BEFORE/AFTER [INITIAL] VAR
.*
.*       AENTRY GET_TEST_PHRASE
           AGO   .ACL_44_SKIP
.ACL_44_GET_TEST_PHRASE ANOP
.*       AIF   ('&SYSLIST(&PARM_IX)' EQ 'BEFORE')
           AIF   (NOT('&SYSLIST(&PARM_IX)' EQ 'BEFORE')).AIF_233_1
&TEST_TYPE     SETC 'BEFORE'
.*       AELSEIF ('&SYSLIST(&PARM_IX)' EQ 'AFTER')
           AGO   .AIF_233_E
.AIF_233_1 ANOP
           AIF     (NOT('&SYSLIST(&PARM_IX)' EQ 'AFTER')).AIF_233_2
&TEST_TYPE     SETC 'AFTER'
.*       AELSE
           AGO   .AIF_233_E
.AIF_233_2 ANOP
               MNOTE 8,'INSPECT TEST MISSING BEFORE/AFTER'
               MEXIT
.*       AEND
.AIF_233_E ANOP
&PARM_IX SETA &PARM_IX+1
.*       AIF   ('&SYSLIST(&PARM_IX)' EQ 'INITIAL')
           AIF   (NOT('&SYSLIST(&PARM_IX)' EQ 'INITIAL')).AIF_234_1
&PARM_IX       SETA &PARM_IX+1
.*       AEND
.AIF_234_1 ANOP
&FIELD_REG SETC 'ZC_R14'
&FIELD_SS1 SETB 0
.*       ACALL GET_PARM_FIELD
&ACALL_6_GET_PARM_FIELD SETA    21
           AGO   .ACL_6_GET_PARM_FIELD
.ACL_6_21  ANOP
.*       AIF   (&FIELD_IX GT 0                                         X
               OR '&FIELD_NAME'(1,1) EQ '''')
           AIF   (NOT(&FIELD_IX GT 0                                   X
                     OR '&FIELD_NAME'(1,1) EQ '''')).AIF_235_1
&TEST_VAR      SETC '&FIELD_NAME'
&TEST_IX       SETA &FIELD_IX
&TEST_OK       SETB 1
.*       AELSE
           AGO   .AIF_235_E
.AIF_235_1 ANOP
&TEST_OK      SETB 0
              MNOTE 8,'INSPECT MISSING TEST IDENTIFIER'
              MEXIT
.*       AEND
.AIF_235_E ANOP
.*       AEND
           AGO   (&ACALL_44_GET_TEST_PHRASE).ACL_44_1,.ACL_44_2,.ACL_44X
               _3,.ACL_44_4,.ACL_44_5
.ACL_44_SKIP ANOP
         MEXIT
         COPY ZCGETFLD
         MEND
         MACRO
.*********************************************************************
.* Copyright 2008 Automated Software Tools Corporation               *
.* This source code is part of z390 zCOBOL/assembler/emulator package*
.* The z390 package is distributed under GNU general public license  *
.* Author - Don Higgins                                              *
.* Date   - 04/16/08                                                 *
.*********************************************************************
.* 04/16/08 initial coding for global INSTALLATION
.*********************************************************************
         INSTALLATION
         MEND
         MACRO
.*********************************************************************
.* Copyright 2008 Automated Software Tools Corporation               *
.* This source code is part of z390 zCOBOL/assembler/emulator package*
.* The z390 package is distributed under GNU general public license  *
.* Author - Don Higgins                                              *
.* Date   - 04/16/08                                                 *
.*********************************************************************
.* 02/20/09 stub for pending support
.*********************************************************************
         I_O_CONTROL
         MNOTE 8,'I_O_CONTROL NOT SUPPORTED YET'
         MEXIT
         MEND
         MACRO
.*********************************************************************
.* Copyright 2008 Automated Software Tools Corporation               *
.* This source code is part of z390 zCOBOL/assembler/emulator package*
.* The z390 package is distributed under GNU general public license  *
.* Author - Don Higgins                                              *
.* Date   - 04/07/08                                                 *
.*********************************************************************
.* 04/09/08 initial coding for global LABEL
.* 10/06/08 ZSTRMAC
.*********************************************************************
         LABEL     &LABEL,&TYPE
.*       AIF   ('&TYPE' EQ '')
           AIF   (NOT('&TYPE' EQ '')).AIF_236_1
               GEN_LABEL &LABEL,PARAGRAPH
.*       AELSEIF ('&TYPE' EQ 'SECTION')
           AGO   .AIF_236_E
.AIF_236_1 ANOP
           AIF     (NOT('&TYPE' EQ 'SECTION')).AIF_236_2
               GEN_LABEL &LABEL,SECTION
.*       AELSE
           AGO   .AIF_236_E
.AIF_236_2 ANOP
               MNOTE 8,'LABEL TYPE UNKNOWN - &TYPE'
.*       AEND
.AIF_236_E ANOP
         MEND
         MACRO
.*********************************************************************
.* Copyright 2008 Automated Software Tools Corporation               *
.* This source code is part of z390 zCOBOL/assembler/emulator package*
.* The z390 package is distributed under GNU general public license  *
.* Author - Don Higgins                                              *
.* Date   - 04/16/08                                                 *
.*********************************************************************
.* 04/16/08 initial coding for global LINKAGE SECTION
.* 10/06/08 ZSTRMAC
.*********************************************************************
.* 1. LK_MODE SET FOR WS TO GEN DSECT FOR EACH 01 NAMED LK_NNNN
.* 2. &SYM_LK HAS INDEX TO 01 LVL WHICH HAS SYM_REF SET TO VAR_PTR IX
.* 3. PROC AND ENTRY SET 01 SYM_REF TO VAR_PTR, WS SETS SYM_LK
.* 4. GEN_BASE LOADS DSECT ADDR FROM VAR_PTR FOR SYM IN LINKAGE SECT
.*********************************************************************
         LINKAGE &SECT
         COPY  ZC_WS
.*       AIF   ('&SECT' NE 'SECTION')
           AIF   (NOT('&SECT' NE 'SECTION')).AIF_237_1
               MNOTE 4,'LINKAGE MISSING "SECTION"'
               MEXIT
.*       AEND
.AIF_237_1 ANOP
&FILE_IX SETA 0 END ANY FILE RECORD DEFINITIONS
&LK_SAVE_WS_LOC SETA &WS_LOC SAVE WS LAST LOC FOR PD
&LK_MODE SETB  1             TURN LK MODE
         MEND
         MACRO
.*********************************************************************
.* Copyright 2009 Automated Software Tools Corporation               *
.* This source code is part of z390 zCOBOL/assembler/emulator package*
.* The z390 package is distributed under GNU general public license  *
.* Author - Don Higgins                                              *
.* Date   - 08/02/09                                                 *
.*********************************************************************
.* 08/03/09 stub for pending support
.*********************************************************************
         LOCALE
         MNOTE 8,'LOCALE NOT SUPPORTED YET'
         MEXIT
         MEND
         MACRO
.*********************************************************************
.* Copyright 2008 Automated Software Tools Corporation               *
.* This source code is part of z390 zCOBOL/assembler/emulator package*
.* The z390 package is distributed under GNU general public license  *
.* Author - Don Higgins                                              *
.* Date   - 04/16/08                                                 *
.*********************************************************************
.* 11/17/08 stub for pending support
.*********************************************************************
         MERGE
         MNOTE 8,'MERGE NOT SUPPORTED YET'
         MEXIT
         MEND
         MACRO
.*********************************************************************
.* Copyright 2008 Automated Software Tools Corporation               *
.* This source code is part of z390 zCOBOL/assembler/emulator package*
.* The z390 package is distributed under GNU general public license  *
.* Author - Don Higgins                                              *
.* Date   - 04/07/08                                                 *
.*********************************************************************
.* 04/10/08 initial coding for global MOVE
.* 05/02/08 add support for literal values by PIC_TYPE
.* 10/06/08 ZSTRMAC
.* 02/27/09 RPI 1012 SUPPORT ALL '?' LIT
.* 06/18/09 RPI 1042 add ZCDFHLIT shared by IF and MOVE
.*********************************************************************
.* MOVE id1 TO id2 id3 idn
.*********************************************************************
         MOVE
         COPY  ZC_WS
&SOURCE  SETC '&SYSLIST(1)'
&ADDR_OF SETB 0
&PARM_IX SETA 2
&SOURCE_IX SETA 0
.*       AIF   ('&SOURCE' EQ 'ALL')
           AIF   (NOT('&SOURCE' EQ 'ALL')).AIF_238_1
&SOURCE        SETC '*&SYSLIST(2)' PREFIX *LIT FOR ALL LIT
&PARM_IX       SETA 3
.*       AELSEIF   ('&SOURCE' EQ 'LENGTH')
           AGO   .AIF_238_E
.AIF_238_1 ANOP
           AIF       (NOT('&SOURCE' EQ 'LENGTH')).AIF_238_2
.*             AIF ('&SYSLIST(2)' EQ 'OF')
                 AIF (NOT('&SYSLIST(2)' EQ 'OF')).AIF_239_1
&SOURCE            SETC '&SYSLIST(3)'
&PARM_IX           SETA 4
.*             AELSE
                 AGO   .AIF_239_E
.AIF_239_1       ANOP
&SOURCE            SETC '&SYSLIST(2)'
&PARM_IX           SETA 3
.*             AEND
.AIF_239_E       ANOP
               ZC_SYM_FIND &SOURCE
.*             AIF  (&SYM_IX GT 0)
                 AIF  (NOT(&SYM_IX GT 0)).AIF_240_1
&SOURCE             SETC '&SYM_LEN(&SYM_IX)'
.*             AELSE
                 AGO   .AIF_240_E
.AIF_240_1       ANOP
                    MNOTE 8,'MOVE LENGTH OF &SOURCE NOT FOUND'
                    MEXIT
.*             AEND
.AIF_240_E       ANOP
.*       AELSEIF ('&SOURCE' EQ 'ADDRESS')
           AGO   .AIF_238_E
.AIF_238_2 ANOP
           AIF     (NOT('&SOURCE' EQ 'ADDRESS')).AIF_238_3
.*             AIF ('&SYSLIST(2)' EQ 'OF')
                 AIF (NOT('&SYSLIST(2)' EQ 'OF')).AIF_241_1
&SOURCE            SETC '&SYSLIST(3)'
&PARM_IX           SETA 3
.*             AELSE
                 AGO   .AIF_241_E
.AIF_241_1       ANOP
&SOURCE            SETC '&SYSLIST(2)'
&PARM_IX           SETA 2
.*             AEND
.AIF_241_E       ANOP
&FIELD_REG     SETC 'ZC_R2'
&FIELD_SS1     SETB 0
.*             ACALL GET_PARM_FIELD
&ACALL_6_GET_PARM_FIELD SETA    22
                 AGO   .ACL_6_GET_PARM_FIELD
.ACL_6_22        ANOP
.*             AIF (&FIELD_IX EQ 0)
                 AIF (NOT(&FIELD_IX EQ 0)).AIF_242_1
                   MNOTE 8,'MOVE SOURCE FIELD NOT FOUND &FIELD_NAME'
                   MEXIT
.*             AEND
.AIF_242_1       ANOP
&SOURCE_IX     SETA &FIELD_IX
&SOURCE        SETC '&FIELD_NAME'
&ADDR_OF       SETB 1
.*       AELSEIF   ('&SOURCE' EQ '+' OR '&SOURCE' EQ '-')
           AGO   .AIF_238_E
.AIF_238_3 ANOP
           AIF       (NOT('&SOURCE' EQ '+' OR '&SOURCE' EQ '-')).AIF_23X
               8_4
&SOURCE        SETC '&SOURCE&SYSLIST(2)'
&PARM_IX       SETA 3
.*       AELSEIF   ('&SOURCE'(1,2) EQ 'X''') OR '&SOURCE'(1,2) EQ 'X"')
           AGO   .AIF_238_E
.AIF_238_4 ANOP
           AIF       (NOT('&SOURCE'(1,2) EQ 'X''') OR '&SOURCE'(1,2) EQX
                'X"')).AIF_238_5
&SOURCE        SETC 'X'''.'&SOURCE'(3,K'&SOURCE-3).''''
.*       AELSEIF  ('&SOURCE' EQ 'DFHRESP')
           AGO   .AIF_238_E
.AIF_238_5 ANOP
           AIF      (NOT('&SOURCE' EQ 'DFHRESP')).AIF_238_6
&DFHRESP       SETC '&SYSLIST(3)'
.*             ACALL GET_DFHRESP
&ACALL_39_GET_DFHRESP SETA    2
                 AGO   .ACL_39_GET_DFHRESP
.ACL_39_2        ANOP
&SOURCE        SETC '&DFHLIT'
&PARM_IX       SETA 5
.*       AELSEIF  ('&SOURCE' EQ 'DFHVALUE')
           AGO   .AIF_238_E
.AIF_238_6 ANOP
           AIF      (NOT('&SOURCE' EQ 'DFHVALUE')).AIF_238_7
&DFHVALUE      SETC '&SYSLIST(3)'
.*             ACALL GET_DFHVALUE
&ACALL_40_GET_DFHVALUE SETA    2
                 AGO   .ACL_40_GET_DFHVALUE
.ACL_40_2        ANOP
&SOURCE        SETC '&DFHLIT'
&PARM_IX       SETA 5
.*       AELSEIF   ('&SOURCE'(1,1) NE ''''                             X
                  AND ('&SOURCE'(1,1) GT '9' OR '&SOURCE'(1,1) LT '0') X
                  AND '&SOURCE'(1,1) NE '.')  RPI 1065
           AGO   .AIF_238_E
.AIF_238_7 ANOP
           AIF       (NOT('&SOURCE'(1,1) NE ''''                       X
                        AND ('&SOURCE'(1,1) GT '9' OR '&SOURCE'(1,1) LTX
                '0')    AND '&SOURCE'(1,1) NE '.')).AIF_238_8   RPI 106X
               5
.*             AIF ('&SOURCE' EQ 'FUNCTION')
                 AIF (NOT('&SOURCE' EQ 'FUNCTION')).AIF_243_1
                   MNOTE 8,'MOVE FUNCTION NOT SUPPORTED YET'
                   MEXIT
.*             AEND
.AIF_243_1       ANOP
&PARM_IX       SETA 1
&FIELD_REG     SETC 'ZC_R2'
&FIELD_SS1     SETB 0
.*             ACALL GET_PARM_FIELD
&ACALL_6_GET_PARM_FIELD SETA    23
                 AGO   .ACL_6_GET_PARM_FIELD
.ACL_6_23        ANOP
&SOURCE_IX     SETA &FIELD_IX
.*             AIF (&FIELD_IX GT 0)
                 AIF (NOT(&FIELD_IX GT 0)).AIF_244_1
&SOURCE            SETC '&FIELD_NAME'
.*             AEND
.AIF_244_1       ANOP
.*       AEND
.AIF_238_8 ANOP
.AIF_238_E ANOP
.*       AIF   ('&SYSLIST(&PARM_IX)' NE 'TO')
           AIF   (NOT('&SYSLIST(&PARM_IX)' NE 'TO')).AIF_245_1
               MNOTE 8,'MOVE MISSING TO'
               MEXIT
.*       AEND
.AIF_245_1 ANOP
&PARM_IX SETA &PARM_IX+1
.*     AWHILE ('&SYSLIST(&PARM_IX)' NE '')
.AWH_38_T ANOP
         AIF    (NOT('&SYSLIST(&PARM_IX)' NE '')).AWH_38_E
         ACTR 4096
&FIELD_REG SETC 'ZC_R2'
&FIELD_SS1 SETB 1
.*       ACALL GET_PARM_FIELD
&ACALL_6_GET_PARM_FIELD SETA    24
           AGO   .ACL_6_GET_PARM_FIELD
.ACL_6_24  ANOP
.*       AIF (&FIELD_IX EQ 0)
           AIF (NOT(&FIELD_IX EQ 0)).AIF_246_1
             MNOTE 8,'MOVE TARGET FIELD NOT FOUND - &FIELD_NAME'
             MEXIT
.*       AEND
.AIF_246_1 ANOP
&TARGET_IX SETA &FIELD_IX
&TARGET  SETC '&FIELD_NAME'
.*       AIF (&SOURCE_IX GT 0)
           AIF (NOT(&SOURCE_IX GT 0)).AIF_247_1
.*             AIF (&ADDR_OF AND '&SYM_PIC_TYPE(&TARGET_IX)' NE 'A')
                 AIF (NOT(&ADDR_OF AND '&SYM_PIC_TYPE(&TARGET_IX)' NE 'X
               A')).AIF_248_1
                   MNOTE 8,'MOVE TARGET OF ADDRESS OF MUST BE POINTER'
                   MEXIT
.*             AEND
.AIF_248_1       ANOP
.*       AEND
.AIF_247_1 ANOP
         GEN_MOVE &TARGET,&TARGET_IX,&SOURCE,&SOURCE_IX
.*     AEND
         AGO   .AWH_38_T
.AWH_38_E ANOP
         MEXIT
         COPY ZCGETFLD
         COPY ZCDFHLIT
         MEND
         MACRO
.*********************************************************************
.* Copyright 2008 Automated Software Tools Corporation               *
.* This source code is part of z390 zCOBOL/assembler/emulator package*
.* The z390 package is distributed under GNU general public license  *
.* Author - Don Higgins                                              *
.* Date   - 04/07/08                                                 *
.*********************************************************************
.* 10/30/08 initial coding for global MULTIPLY using ADD as model
.* 02/26/09 RPI 1012 move onsize end-add pending to MULTIPLY
.* 07/19/09 RPI 1062 correct ON SIZE parsing
.*********************************************************************
         MULTIPLY
         COPY  ZC_WS
&ROUNDED SETB 0
&ONSIZE  SETB 0
&PARM_IX SETA 1
&FIELD_REG SETC 'ZC_R1'
&FIELD_SS1 SETB 0
.*       ACALL GET_PARM_FIELD
&ACALL_6_GET_PARM_FIELD SETA    25
           AGO   .ACL_6_GET_PARM_FIELD
.ACL_6_25  ANOP
&S1      SETC '&FIELD_NAME'
&S1_IX   SETA &FIELD_IX
.*       AIF   ('&SYSLIST(&PARM_IX)' NE 'BY')
           AIF   (NOT('&SYSLIST(&PARM_IX)' NE 'BY')).AIF_249_1
               MNOTE 8,'MULTIPLY MISSING BY'
               MEXIT
.*       AEND
.AIF_249_1 ANOP
&PARM_IX SETA &PARM_IX+1
&S2_OK   SETB 1
.*       ACALL SET_OPTIONS
&ACALL_13_SET_OPTIONS SETA    2
           AGO   .ACL_13_SET_OPTIONS
.ACL_13_2  ANOP
.*       ACALL GET_S2
&ACALL_5_GET_S2 SETA    3
           AGO   .ACL_5_GET_S2
.ACL_5_3   ANOP
.*       AIF   (&S2_OK)
           AIF   (NOT(&S2_OK)).AIF_250_1
.*             AIF ('&SYSLIST(&PARM_IX)' EQ 'GIVING')
                 AIF (NOT('&SYSLIST(&PARM_IX)' EQ 'GIVING')).AIF_251_1
&PARM_IX           SETA &PARM_IX+1
.*                 ACALL MPY_GIVING
&ACALL_45_MPY_GIVING SETA    1
                     AGO   .ACL_45_MPY_GIVING
.ACL_45_1            ANOP
.*             AELSE
                 AGO   .AIF_251_E
.AIF_251_1       ANOP
.*                 ACALL MPY_BY
&ACALL_46_MPY_BY     SETA    1
                     AGO   .ACL_46_MPY_BY
.ACL_46_1            ANOP
.*             AEND
.AIF_251_E       ANOP
.*       AEND
.AIF_250_1 ANOP
.*       AIF   (&ONSIZE)
           AIF   (NOT(&ONSIZE)).AIF_252_1
               GEN_ONSIZE_START MPY
.*       AEND
.AIF_252_1 ANOP
         MEXIT
.*
.* MPY GIVING
.*
.*       AENTRY MPY_GIVING
           AGO   .ACL_45_SKIP
.ACL_45_MPY_GIVING ANOP
&S3_OK   SETB 1
.*       ACALL GET_S3
&ACALL_4_GET_S3 SETA    3
           AGO   .ACL_4_GET_S3
.ACL_4_3   ANOP
.*       AWHILE (&S3_OK)
.AWH_39_T  ANOP
           AIF    (NOT(&S3_OK)).AWH_39_E
             GEN_MOVE &S3,&S3_IX,&S1,&S1_IX
             GEN_MPY  &S3,&S3_IX,&S2,&S2_IX,&ROUNDED,&ONSIZE
.*           ACALL GET_S3
&ACALL_4_GET_S3 SETA    4
               AGO   .ACL_4_GET_S3
.ACL_4_4       ANOP
.*       AEND
           AGO   .AWH_39_T
.AWH_39_E  ANOP
.*       AEND
           AGO   (&ACALL_45_MPY_GIVING).ACL_45_1
.ACL_45_SKIP ANOP
.*
.* MPY BY (NO GIVING, MULT MULTIPLIERS)
.*
.*       AENTRY MPY_BY
           AGO   .ACL_46_SKIP
.ACL_46_MPY_BY ANOP
.*       AWHILE (&S2_OK)
.AWH_40_T  ANOP
           AIF    (NOT(&S2_OK)).AWH_40_E
             GEN_MPY  &S2,&S2_IX,&S1,&S1_IX,&ROUNDED,&ONSIZE
.*           ACALL GET_S2
&ACALL_5_GET_S2 SETA    4
               AGO   .ACL_5_GET_S2
.ACL_5_4       ANOP
.*       AEND
           AGO   .AWH_40_T
.AWH_40_E  ANOP
.*       AEND
           AGO   (&ACALL_46_MPY_BY).ACL_46_1
.ACL_46_SKIP ANOP
.*
.* GET S2 AND OPTIONAL ROUNDING
.*
.*       AENTRY GET_S2
 MNOTE 8'ZSTRMAC ERROR AENTRY DUPLICATE NAME FOUND - GET_S2
.*       AIF (&PARM_IX GT &LAST_PARM)
           AIF (NOT(&PARM_IX GT &LAST_PARM)).AIF_253_1
&S2_OK       SETB 0
.*           AEXIT AENTRY
 MNOTE 8'ZSTRMAC ERROR AEXIT NOT WITHIN AENTRY, AWHILE, ACASE
.*       AEND
.AIF_253_1 ANOP
&FIELD_REG SETC 'ZC_R2'
&FIELD_SS1 SETB 0
.*       ACALL GET_PARM_FIELD
&ACALL_6_GET_PARM_FIELD SETA    26
           AGO   .ACL_6_GET_PARM_FIELD
.ACL_6_26  ANOP
&S2      SETC '&FIELD_NAME'
&S2_IX   SETA &FIELD_IX
.*       AIF   ('&SYSLIST(&PARM_IX)' EQ 'ROUNDED')
           AIF   (NOT('&SYSLIST(&PARM_IX)' EQ 'ROUNDED')).AIF_254_1
&ROUNDED       SETB 1
&PARM_IX       SETA &PARM_IX+1
.*       AELSE
           AGO   .AIF_254_E
.AIF_254_1 ANOP
&ROUNDED       SETB 0
.*       AEND
.AIF_254_E ANOP
.*       AEND
 MNOTE 8'ZSTRMAC ERROR AEND MISSING AIF OR OTHER STRUCTURE
.*
.* GET S3 AND OPTIONAL ROUNDING
.*
.*       AENTRY GET_S3
 MNOTE 8'ZSTRMAC ERROR AENTRY DUPLICATE NAME FOUND - GET_S3
.*       AIF (&PARM_IX GT &LAST_PARM)
           AIF (NOT(&PARM_IX GT &LAST_PARM)).AIF_255_1
&S3_OK       SETB 0
.*           AEXIT AENTRY
 MNOTE 8'ZSTRMAC ERROR AEXIT NOT WITHIN AENTRY, AWHILE, ACASE
.*       AEND
.AIF_255_1 ANOP
&FIELD_REG SETC 'ZC_R3'
&FIELD_SS1 SETB 0
.*       ACALL GET_PARM_FIELD
&ACALL_6_GET_PARM_FIELD SETA    27
           AGO   .ACL_6_GET_PARM_FIELD
.ACL_6_27  ANOP
&S3      SETC '&FIELD_NAME'
&S3_IX   SETA &FIELD_IX
.*       AIF   ('&SYSLIST(&PARM_IX)' EQ 'ROUNDED')
           AIF   (NOT('&SYSLIST(&PARM_IX)' EQ 'ROUNDED')).AIF_256_1
&ROUNDED       SETB 1
&PARM_IX       SETA &PARM_IX+1
.*       AELSE
           AGO   .AIF_256_E
.AIF_256_1 ANOP
&ROUNDED       SETB 0
.*       AEND
.AIF_256_E ANOP
.*       AEND
 MNOTE 8'ZSTRMAC ERROR AEND MISSING AIF OR OTHER STRUCTURE
.*
.* SET OPTIONS - ROUND AND ON SIZE ERROR
.*
.*       AENTRY SET_OPTIONS
 MNOTE 8'ZSTRMAC ERROR AENTRY DUPLICATE NAME FOUND - SET_OPTIONS
&SAVE_PARM_IX SETA &PARM_IX
&ROUNDED SETB 0
&ON_SIZE SETB 0
&NOT_ON_SIZE SETB 0
&LAST_PARM SETA 0
.*       AWHILE (&PARM_IX LE N'&SYSLIST)
.AWH_41_T  ANOP
           AIF    (NOT(&PARM_IX LE N'&SYSLIST)).AWH_41_E
               ACTR 4096
.*             AIF     ('&SYSLIST(&PARM_IX)' EQ 'ON'                   X
                        AND '&SYSLIST(&PARM_IX+1)' EQ 'SIZE'           X
                        AND '&SYSLIST(&PARM_IX+2)' EQ 'ERROR') RPI 1065
                 AIF     (NOT('&SYSLIST(&PARM_IX)' EQ 'ON'             X
                              AND '&SYSLIST(&PARM_IX+1)' EQ 'SIZE'     X
                              AND '&SYSLIST(&PARM_IX+2)' EQ 'ERROR')).AX
               IF_257_1  RPI 1065
&ON_SIZE             SETB 1
.*                   AIF (&LAST_PARM EQ 0)
                       AIF (NOT(&LAST_PARM EQ 0)).AIF_258_1
&LAST_PARM               SETA &PARM_IX-1
.*                   AEND
.AIF_258_1             ANOP
&PARM_IX             SETA N'&SYSLIST+2
.*             AELSEIF ('&SYSLIST(&PARM_IX)' EQ 'NOT'                  X
                        AND '&SYSLIST(&PARM_IX+1)' EQ 'ON'             X
                        AND '&SYSLIST(&PARM_IX+2)' EQ 'SIZE'           X
                        AND '&SYSLIST(&PARM_IX+3)' EQ 'ERROR') RPI 1065
                 AGO   .AIF_257_E
.AIF_257_1       ANOP
                 AIF     (NOT('&SYSLIST(&PARM_IX)' EQ 'NOT'            X
                              AND '&SYSLIST(&PARM_IX+1)' EQ 'ON'       X
                              AND '&SYSLIST(&PARM_IX+2)' EQ 'SIZE'     X
                              AND '&SYSLIST(&PARM_IX+3)' EQ 'ERROR')).AX
               IF_257_2  RPI 1065
&NOT_ON_SIZE         SETB 1
.*                   AIF (&LAST_PARM EQ 0)
                       AIF (NOT(&LAST_PARM EQ 0)).AIF_259_1
&LAST_PARM               SETA &PARM_IX-1
.*                   AEND
.AIF_259_1             ANOP
&PARM_IX             SETA N'&SYSLIST+3
.*             AELSEIF ('&SYSLIST(&PARM_IX)' EQ 'REMAINDER')
                 AGO   .AIF_257_E
.AIF_257_2       ANOP
                 AIF     (NOT('&SYSLIST(&PARM_IX)' EQ 'REMAINDER')).AIFX
               _257_3
                     MNOTE 8,'DIVIDE MULT GIVING WITH REMAINDER ERROR'
                     MEXIT
.*             AEND
.AIF_257_3       ANOP
.AIF_257_E       ANOP
&PARM_IX       SETA &PARM_IX+1
.*       AEND
           AGO   .AWH_41_T
.AWH_41_E  ANOP
.*       AIF (&LAST_PARM EQ 0)
           AIF (NOT(&LAST_PARM EQ 0)).AIF_260_1
&LAST_PARM   SETA N'&SYSLIST
.*       AEND
.AIF_260_1 ANOP
&PARM_IX SETA &SAVE_PARM_IX
.*       AIF   (&ONSIZE)
           AIF   (NOT(&ONSIZE)).AIF_261_1
&IE_LVL        SETA  &IE_LVL+1         ADD PENDING END_MPY
&IE_TYPE(&IE_LVL) SETA 13        FOR END_MPY OR PERIOD
&MPY_LAB       SETA &MPY_LAB+1
&IE_TCNT(&IE_LVL) SETA &MPY_LAB
.*       AEND
.AIF_261_1 ANOP
.*       AEND
 MNOTE 8'ZSTRMAC ERROR AEND MISSING AIF OR OTHER STRUCTURE
         COPY ZCGETFLD
         MEND
         MACRO
.*********************************************************************
.* Copyright 2008 Automated Software Tools Corporation               *
.* This source code is part of z390 zCOBOL/assembler/emulator package*
.* The z390 package is distributed under GNU general public license  *
.* Author - Don Higgins                                              *
.* Date   - 04/16/08                                                 *
.*********************************************************************
.* 04/16/08 initial coding for global GO TO
.* 10/06/08 ZSTRMAC
.*********************************************************************
         NEXT  &SENTENCE
         COPY  ZC_WS
.*       AIF   ('&SENTENCE' EQ 'SENTENCE')
           AIF   (NOT('&SENTENCE' EQ 'SENTENCE')).AIF_262_1
.*             AIF (NOT &PERIOD_NEXT)
                 AIF (NOT(NOT &PERIOD_NEXT)).AIF_263_1
&PERIOD_NEXT       SETB 1
&PERIOD_NEXT_LAB   SETA &PERIOD_NEXT_LAB+1
.*             AEND
.AIF_263_1       ANOP
               GEN_B  PERIOD_NEXT_&PERIOD_NEXT_LAB
.*       AELSE
           AGO   .AIF_262_E
.AIF_262_1 ANOP
               MNOTE 8,'NEXT MISSING SENTENCE'
.*       AEND
.AIF_262_E ANOP
         MEND
         MACRO
.*********************************************************************
.* Copyright 2008 Automated Software Tools Corporation               *
.* This source code is part of z390 zCOBOL/assembler/emulator package*
.* The z390 package is distributed under GNU general public license  *
.* Author - Don Higgins                                              *
.* Date   - 04/16/08                                                 *
.*********************************************************************
.* 11/17/08 stub for pending support
.*********************************************************************
         NOT
         MNOTE 8,'NOT ON IS NOT SUPPORTED YET'
         MEXIT
         MEND
         MACRO
.*********************************************************************
.* Copyright 2008 Automated Software Tools Corporation               *
.* This source code is part of z390 zCOBOL/assembler/emulator package*
.* The z390 package is distributed under GNU general public license  *
.* Author - Don Higgins                                              *
.* Date   - 02/11/09                                                 *
.*********************************************************************
.* 02/11/09 initial coding for NOTE comment sentence
.*********************************************************************
         NOTE
         MEND
         MACRO
.*********************************************************************
.* Copyright 2008 Automated Software Tools Corporation               *
.* This source code is part of z390 zCOBOL/assembler/emulator package*
.* The z390 package is distributed under GNU general public license  *
.* Author - Don Higgins                                              *
.* Date   - 04/16/08                                                 *
.*********************************************************************
.* 04/16/08 initial coding for global OBJECT-COMPUTER
.*********************************************************************
         OBJECT_COMPUTER
         MEND
         MACRO
.*********************************************************************
.* Copyright 2008 Automated Software Tools Corporation               *
.* This source code is part of z390 zCOBOL/assembler/emulator package*
.* The z390 package is distributed under GNU general public license  *
.* Author - Don Higgins                                              *
.* Date   - 04/14/08                                                 *
.*********************************************************************
.* 04/14/08 initial coding for global ZCOBOL
.* 10/06/08 ZSTRMAC
.*********************************************************************
         OPEN  &TYPE,&FILE
         COPY  ZC_WS
.*       AIF   ('&TYPE' NE 'INPUT'                                     X
                AND '&TYPE' NE 'OUTPUT')
           AIF   (NOT('&TYPE' NE 'INPUT'                               X
                      AND '&TYPE' NE 'OUTPUT')).AIF_264_1
                MNOTE 8,'OPEN UNKNOWN TYPE - &TYPE'
                MEXIT
.*       AEND
.AIF_264_1 ANOP
         ZC_FILE_FIND &FILE
.*       AIF   (&FILE_IX GT 0)
           AIF   (NOT(&FILE_IX GT 0)).AIF_265_1
               GEN_OPEN &TYPE
.*       AELSE
           AGO   .AIF_265_E
.AIF_265_1 ANOP
               MNOTE 8,'OPEN FILE NOT FOUND - &FILE'
.*       AEND
.AIF_265_E ANOP
         MEND
         MACRO
.*********************************************************************
.* Copyright 2009 Automated Software Tools Corporation               *
.* This source code is part of z390 zCOBOL/assembler/emulator package*
.* The z390 package is distributed under GNU general public license  *
.* Author - Don Higgins                                              *
.* Date   - 08/02/09                                                 *
.*********************************************************************
.* 08/03/09 stub for pending support
.*********************************************************************
         ORDER
         MNOTE 8,'ORDER NOT SUPPORTED YET'
         MEXIT
         MEND
         MACRO
.*********************************************************************
.* Copyright 2008 Automated Software Tools Corporation               *
.* This source code is part of z390 zCOBOL/assembler/emulator package*
.* The z390 package is distributed under GNU general public license  *
.* Author - Don Higgins                                              *
.* Date   - 04/15/08                                                 *
.*********************************************************************
.* 04/15/08 initial coding for global PERFORM
.* 11/28/08 add formats 1-4 see TESTPM1.CBL
.* 07/09/09 RPI correct BY for negative increments
.* 08/04/09 RPI RPI support qualified PG names within section
.*********************************************************************
         PERFORM
         COPY  ZC_WS
&PM_LAB  SETA &PM_LAB+1
&NP      SETA  N'&SYSLIST
&TEST_BEFORE SETB 1
&I       SETA 1
.*       AIF   ('&SYSLIST(&I)' EQ 'WITH'                               X
               AND '&SYSLIST(&I+1)' EQ 'TEST')
           AIF   (NOT('&SYSLIST(&I)' EQ 'WITH'                         X
                     AND '&SYSLIST(&I+1)' EQ 'TEST')).AIF_266_1
.*             AIF  ('&SYSLIST(&I+2)' EQ 'BEFORE')
                 AIF  (NOT('&SYSLIST(&I+2)' EQ 'BEFORE')).AIF_267_1
&TEST_BEFORE        SETB 1
.*             AELSEIF ('&SYSLIST(&I+2)' EQ 'AFTER')
                 AGO   .AIF_267_E
.AIF_267_1       ANOP
                 AIF     (NOT('&SYSLIST(&I+2)' EQ 'AFTER')).AIF_267_2
&TEST_BEFORE        SETB 0
.*             AELSE
                 AGO   .AIF_267_E
.AIF_267_2       ANOP
                    MNOTE 8,'PERFORM WITH TEST MISSING BEFORE/AFTER'
                    MEXIT
.*             AEND
.AIF_267_E       ANOP
&I             SETA &I+3
.*       AEND
.AIF_266_1 ANOP
.*       AIF   ('&SYSLIST(&I)' EQ 'UNTIL')
           AIF   (NOT('&SYSLIST(&I)' EQ 'UNTIL')).AIF_268_1
.*             ACALL FORMAT_3B
&ACALL_47_FORMAT_3B SETA    1
                 AGO   .ACL_47_FORMAT_3B
.ACL_47_1        ANOP
               MEXIT
.*       AEND
.AIF_268_1 ANOP
.*       AIF   ('&SYSLIST(&I)' EQ 'VARYING')
           AIF   (NOT('&SYSLIST(&I)' EQ 'VARYING')).AIF_269_1
.*             ACALL FORMAT_4B
&ACALL_48_FORMAT_4B SETA    1
                 AGO   .ACL_48_FORMAT_4B
.ACL_48_1        ANOP
               MEXIT
.*       AEND
.AIF_269_1 ANOP
.*       AIF   ('&SYSLIST(&I+1)' EQ 'TIMES')
           AIF   (NOT('&SYSLIST(&I+1)' EQ 'TIMES')).AIF_270_1
.*             ACALL FORMAT_2B
&ACALL_49_FORMAT_2B SETA    1
                 AGO   .ACL_49_FORMAT_2B
.ACL_49_1        ANOP
.*       AELSE
           AGO   .AIF_270_E
.AIF_270_1 ANOP
.*             AIF (&NP GT 0)
                 AIF (NOT(&NP GT 0)).AIF_271_1
.*                 ACALL FORMAT_1A_2A_3A_4A
&ACALL_50_FORMAT_1A_2A_3A_4A SETA    1
                     AGO   .ACL_50_FORMAT_1A_2A_3A_4A
.ACL_50_1            ANOP
.*             AELSE
                 AGO   .AIF_271_E
.AIF_271_1       ANOP
.*                 ACALL FORMAT_1B
&ACALL_51_FORMAT_1B  SETA    1
                     AGO   .ACL_51_FORMAT_1B
.ACL_51_1            ANOP
.*             AEND
.AIF_271_E       ANOP
.*       AEND
.AIF_270_E ANOP
         MEXIT
.*
.* FORMAT 1A PERFORM PG1 [THRU PG2] END-PERFORM/.
.* FORMAT 2A PERFORM PG1 [THRU PG2] n TIMES END-PERFORM/.
.* FORMAT 3A PERFORM PG1 [THRU PG2] [UNTIL cond] END-PERFORM/.
.* FORMAT 3A PERFORM PG1 [THRU PG2] [VARYING ..] END-PERFORM/.
.*
.*       AENTRY FORMAT_1A_2A_3A_4A
           AGO   .ACL_50_SKIP
.ACL_50_FORMAT_1A_2A_3A_4A ANOP
&PG_NAME1 SETC '&SYSLIST(&I)' RPI 1065 SN/PG SUPPORT
&I       SETA &I+1
.*       AIF   ('&SYSLIST(&I)' EQ 'OF' OR '&SYSLIST(&I)' EQ 'IN')
           AIF   (NOT('&SYSLIST(&I)' EQ 'OF' OR '&SYSLIST(&I)' EQ 'IN')X
               ).AIF_272_1
&SN_NAME1      SETC '&SYSLIST(&I+1)'
&I             SETA &I+2
.*       AELSE
           AGO   .AIF_272_E
.AIF_272_1 ANOP
&SN_NAME1      SETC ''
.*       AEND
.AIF_272_E ANOP
.*       AIF   ('&SYSLIST(&I)' EQ 'THRU'                               X
                OR '&SYSLIST(&I)' EQ 'THROUGH')
           AIF   (NOT('&SYSLIST(&I)' EQ 'THRU'                         X
                      OR '&SYSLIST(&I)' EQ 'THROUGH')).AIF_273_1
&PG_NAME2      SETC  '&SYSLIST(&I+1)'
&I             SETA  &I+2
.*             AIF  ('&SYSLIST(&I)' EQ 'OF' OR '&SYSLIST(&I)' EQ 'IN')
                 AIF  (NOT('&SYSLIST(&I)' EQ 'OF' OR '&SYSLIST(&I)' EQ X
               'IN')).AIF_274_1
&SN_NAME2           SETC '&SYSLIST(&I+1)'
&I                  SETA &I+2
.*             AELSE
                 AGO   .AIF_274_E
.AIF_274_1       ANOP
&SN_NAME2           SETC '&SN_NAME1'
.*             AEND
.AIF_274_E       ANOP
.*       AELSE
           AGO   .AIF_273_E
.AIF_273_1 ANOP
&PG_NAME2      SETC '&PG_NAME1._END'
&SN_NAME2      SETC '&SN_NAME1'
.*       AEND
.AIF_273_E ANOP
&IF_PARM SETC  ''
.*       AIF   (&I LE &NP)
           AIF   (NOT(&I LE &NP)).AIF_275_1
.*             AIF   ('&SYSLIST(&I)' EQ 'WITH'                         X
               AND '&SYSLIST(&I+1)' EQ 'TEST')
                 AIF   (NOT('&SYSLIST(&I)' EQ 'WITH'                   X
                     AND '&SYSLIST(&I+1)' EQ 'TEST')).AIF_276_1
.*                  AIF  ('&SYSLIST(&I+2)' EQ 'BEFORE')
                      AIF  (NOT('&SYSLIST(&I+2)' EQ 'BEFORE')).AIF_277_X
               1
&TEST_BEFORE             SETB 1
.*                  AELSEIF ('&SYSLIST(&I+2)' EQ 'AFTER')
                      AGO   .AIF_277_E
.AIF_277_1            ANOP
                      AIF     (NOT('&SYSLIST(&I+2)' EQ 'AFTER')).AIF_27X
               7_2
&TEST_BEFORE             SETB 0
.*                  AELSE
                      AGO   .AIF_277_E
.AIF_277_2            ANOP
                         MNOTE 8,'PERFORM WITH MISSING BEFORE/AFTER'
                         MEXIT
.*                  AEND
.AIF_277_E            ANOP
&I                  SETA &I+3
.*             AEND
.AIF_276_1       ANOP
.*             AIF   ('&SYSLIST(&I)' EQ 'UNTIL')
                 AIF   (NOT('&SYSLIST(&I)' EQ 'UNTIL')).AIF_278_1
.*                   ACALL FORMAT_3A
&ACALL_52_FORMAT_3A    SETA    1
                       AGO   .ACL_52_FORMAT_3A
.ACL_52_1              ANOP
.*             AELSEIF ('&SYSLIST(&I)' EQ 'VARYING')
                 AGO   .AIF_278_E
.AIF_278_1       ANOP
                 AIF     (NOT('&SYSLIST(&I)' EQ 'VARYING')).AIF_278_2
.*                   ACALL FORMAT_4A
&ACALL_53_FORMAT_4A    SETA    1
                       AGO   .ACL_53_FORMAT_4A
.ACL_53_1              ANOP
.*             AELSEIF ('&SYSLIST(&I+1)' EQ 'TIMES')
                 AGO   .AIF_278_E
.AIF_278_2       ANOP
                 AIF     (NOT('&SYSLIST(&I+1)' EQ 'TIMES')).AIF_278_3
.*                   ACALL FORMAT_2A
&ACALL_54_FORMAT_2A    SETA    1
                       AGO   .ACL_54_FORMAT_2A
.ACL_54_1              ANOP
.*             AELSE
                 AGO   .AIF_278_E
.AIF_278_3       ANOP
                     MNOTE 8,'PERFORM UNKNOWN -  &SYSLIST(&I)'
                     MEXIT
.*             AEND
.AIF_278_E       ANOP
.*       AELSE
           AGO   .AIF_275_E
.AIF_275_1 ANOP
.*             ACALL FORMAT_1A
&ACALL_55_FORMAT_1A SETA    1
                 AGO   .ACL_55_FORMAT_1A
.ACL_55_1        ANOP
.*       AEND
.AIF_275_E ANOP
.*       AEND
           AGO   (&ACALL_50_FORMAT_1A_2A_3A_4A).ACL_50_1
.ACL_50_SKIP ANOP
.*
.* FORMAT 1A PERFORM PG1 [THRU PG2] END-PERFORM/.
.*
.*       AENTRY FORMAT_1A
           AGO   .ACL_55_SKIP
.ACL_55_FORMAT_1A ANOP
         GEN_PERFORM &PG_NAME1,&SN_NAME1,&PG_NAME2,&SN_NAME2
.*       AEND
           AGO   (&ACALL_55_FORMAT_1A).ACL_55_1
.ACL_55_SKIP ANOP
.*
.* FORMAT 1B PERFORM statement END-PERFORM.
.*
.*       AENTRY FORMAT_1B   ADD PM LEVEL TO MATCH END_PERFORM OR .
           AGO   .ACL_51_SKIP
.ACL_51_FORMAT_1B ANOP
.*       AEND
           AGO   (&ACALL_51_FORMAT_1B).ACL_51_1
.ACL_51_SKIP ANOP
.*
.* FORMAT 2A PERFORM pg1 [THRU pg2] n TIMES END-PM
.*
.*       AENTRY FORMAT_2A
           AGO   .ACL_54_SKIP
.ACL_54_FORMAT_2A ANOP
         GEN_VAR ZC_PM_NTIME_&VAR_TOT,F
&VAR_IX  SETA &SYM_IX
         MOVE &SYSLIST(&I),TO,&SYM_NAME(&VAR_IX)
         GEN_B     PM_&PM_LAB._P
         GEN_LABEL PM_&PM_LAB._T,PERFORM_TEST
         IF &SYM_NAME(&VAR_IX),=,0
               GEN_B PM_&PM_LAB._E
         END_IF
         GEN_LABEL   PM_&PM_LAB._P,PERFORM
         GEN_PERFORM &PG_NAME1,&SN_NAME1,&PG_NAME1,&SN_NAME2
         SUBTRACT 1,FROM,&SYM_NAME(&VAR_IX)
         GEN_B PM_&PM_LAB._T
         GEN_LABEL PM_&PM_LAB._E,PERFORM_END
.*       AEND
           AGO   (&ACALL_54_FORMAT_2A).ACL_54_1
.ACL_54_SKIP ANOP
.*
.* FORMAT 2B PERFORM N TIMES statement END-PERFORM
.*
.*       AENTRY FORMAT_2B
           AGO   .ACL_49_SKIP
.ACL_49_FORMAT_2B ANOP
         GEN_VAR ZC_PM_VARY_&VAR_TOT,F
&VAR_IX  SETA &SYM_IX
         MOVE &SYSLIST(&I),TO,&SYM_NAME(&VAR_IX)
         GEN_B     PM_&PM_LAB._T
         GEN_LABEL PM_&PM_LAB._D,PERFORM_DEC_TIMES
         SUBTRACT 1,FROM,&SYM_NAME(&VAR_IX)
         GEN_LABEL PM_&PM_LAB._T,PERFORM_TEST
         IF &SYM_NAME(&VAR_IX),=,0
               GEN_B PM_&PM_LAB._E
         END_IF
         GEN_PM_LVL PM_&PM_LAB._D
.*       AEND
           AGO   (&ACALL_49_FORMAT_2B).ACL_49_1
.ACL_49_SKIP ANOP
.*
.* FORMAT 3A PERFORM pg1 [THRU pg2 [WITH TEST BEFORE/AFTER] UNTIL cond
.*
.*       AENTRY FORMAT_3A
           AGO   .ACL_52_SKIP
.ACL_52_FORMAT_3A ANOP
.*       ACALL  SET_IF_PARMS
&ACALL_56_SET_IF_PARMS SETA    1
           AGO   .ACL_56_SET_IF_PARMS
.ACL_56_1  ANOP
         GEN_LABEL PM_&PM_LAB._T,PERFORM_TEST
         IF    &IF_PARM
               GEN_B PM_&PM_LAB._E
         END_IF
         GEN_PERFORM &PG_NAME1,&SN_NAME1,&PG_NAME2,&SN_NAME2
.*       AIF   ('&IF_PARM' NE '')
           AIF   (NOT('&IF_PARM' NE '')).AIF_279_1
               GEN_B PM_&PM_LAB._T
               GEN_LABEL PM_&PM_LAB._E,PERFORM_END
.*       AEND
.AIF_279_1 ANOP
.*       AEND
           AGO   (&ACALL_52_FORMAT_3A).ACL_52_1
.ACL_52_SKIP ANOP
.*
.* FORMAT 3B PERFORM [WITH TEST BEFORE/AFTER] UNTIL cond stmt END-PM
.*
.*       AENTRY FORMAT_3B
           AGO   .ACL_47_SKIP
.ACL_47_FORMAT_3B ANOP
.*       ACALL  SET_IF_PARMS
&ACALL_56_SET_IF_PARMS SETA    2
           AGO   .ACL_56_SET_IF_PARMS
.ACL_56_2  ANOP
         GEN_LABEL PM_&PM_LAB._T,PERFORM_TEST
         IF    &IF_PARM
               GEN_B PM_&PM_LAB._E
         END_IF
         GEN_PM_LVL PM_&PM_LAB._T
.*       AEND
           AGO   (&ACALL_47_FORMAT_3B).ACL_47_1
.ACL_47_SKIP ANOP
.*
.* FORMAT 4A PERFORM pg1 [THRU pg2 [WITH TEST BEFORE/AFTER]
.*   VARYING v1 FROM i1 BY i2 UNTIL cond
.*
.*       AENTRY FORMAT_4A
           AGO   .ACL_53_SKIP
.ACL_53_FORMAT_4A ANOP
.*       ACALL  SET_FROM_BY_UNTIL
&ACALL_57_SET_FROM_BY_UNTIL SETA    1
           AGO   .ACL_57_SET_FROM_BY_UNTIL
.ACL_57_1  ANOP
.*       ACALL  SET_IF_PARMS
&ACALL_56_SET_IF_PARMS SETA    3
           AGO   .ACL_56_SET_IF_PARMS
.ACL_56_3  ANOP
         MOVE  &FROM,TO,&VAR
.*       AIF   (&TEST_BEFORE)
           AIF   (NOT(&TEST_BEFORE)).AIF_280_1
               GEN_LABEL PM_&PM_LAB._T,PERFORM_TEST
               IF  &IF_PARM
                   GEN_B PM_&PM_LAB._E
               END_IF
               GEN_PERFORM &PG_NAME1,&SN_NAME1,&PG_NAME1,&SN_NAME2
               ADD &BY,TO,&VAR
               GEN_B PM_&PM_LAB._T
.*       AELSE
           AGO   .AIF_280_E
.AIF_280_1 ANOP
               GEN_LABEL PM_&PM_LAB._P,PERFORM_TEST
               GEN_PERFORM &PG_NAME1,&SN_NAME1,&PG_NAME2,&SN_NAME2
               IF  &IF_PARM
                   GEN_B PM_&PM_LAB._E
               END_IF
               ADD &BY,TO,&VAR
               GEN_B PM_&PM_LAB._P
.*       AEND
.AIF_280_E ANOP
         GEN_LABEL PM_&PM_LAB._E,PERFORM_END
.*       AEND
           AGO   (&ACALL_53_FORMAT_4A).ACL_53_1
.ACL_53_SKIP ANOP
.*
.* FORMAT 4B PERFORM [WITH TEST BEFORE/AFTER]
.*  VARYING v1 FROM i1 BY i2 UNTIL cond stmt END-PM
.*
.*       AENTRY FORMAT_4B
           AGO   .ACL_48_SKIP
.ACL_48_FORMAT_4B ANOP
.*       ACALL  SET_FROM_BY_UNTIL
&ACALL_57_SET_FROM_BY_UNTIL SETA    2
           AGO   .ACL_57_SET_FROM_BY_UNTIL
.ACL_57_2  ANOP
.*       ACALL  SET_IF_PARMS
&ACALL_56_SET_IF_PARMS SETA    4
           AGO   .ACL_56_SET_IF_PARMS
.ACL_56_4  ANOP
         MOVE  &FROM,TO,&VAR
.*       AIF   (&TEST_BEFORE)
           AIF   (NOT(&TEST_BEFORE)).AIF_281_1
               GEN_B PM_&PM_LAB._T
               GEN_LABEL PM_&PM_LAB._A,PERFORM_AUGMENT
               ADD &BY,TO,&VAR
               GEN_LABEL PM_&PM_LAB._T,PERFORM_TEST
               IF  &IF_PARM
                   GEN_B PM_&PM_LAB._E
               END_IF
               GEN_PM_LVL PM_&PM_LAB._A
.*       AELSE
           AGO   .AIF_281_E
.AIF_281_1 ANOP
               GEN_B PM_&PM_LAB._P
               GEN_LABEL PM_&PM_LAB._A,PERFORM_AUGMENT
               ADD &BY,TO,&VAR
               IF  &IF_PARM
                   GEN_B PM_&PM_LAB._E
               END_IF
               GEN_LABEL PM_&PM_LAB._P,PERFORM_STATEMENT
               GEN_PM_LVL PM_&PM_LAB._A
.*       AEND
.AIF_281_E ANOP
.*       AEND
           AGO   (&ACALL_48_FORMAT_4B).ACL_48_1
.ACL_48_SKIP ANOP
.*
.* SET VARYING FROM BY UNTIL
.*
.*       AENTRY SET_FROM_BY_UNTIL
           AGO   .ACL_57_SKIP
.ACL_57_SET_FROM_BY_UNTIL ANOP
&VAR     SETC '&SYSLIST(&I+1)'          SKIP VARYING
.*       AIF  ('&SYSLIST(&I+2)' NE 'FROM')
           AIF  (NOT('&SYSLIST(&I+2)' NE 'FROM')).AIF_282_1
              MNOTE 8,'PERFORM VARYING MISSING FROM'
              MEXIT
.*       AEND
.AIF_282_1 ANOP
&I       SETA &I+3
&FROM    SETC '&SYSLIST(&I)'
.*       AIF   ('&FROM' EQ '+' OR '&FROM' EQ '-')
           AIF   (NOT('&FROM' EQ '+' OR '&FROM' EQ '-')).AIF_283_1
&FROM          SETC '&SYSLIST(&I)&SYSLIST(&I+1)' RPI 1065
&I             SETA &I+1
.*       AEND
.AIF_283_1 ANOP
&I       SETA &I+1
.*       AIF   ('&SYSLIST(&I)' NE 'BY')
           AIF   (NOT('&SYSLIST(&I)' NE 'BY')).AIF_284_1
               MNOTE 8,'PERFORM VARYING MISSING BY'
               MEXIT
.*       AEND
.AIF_284_1 ANOP
&I       SETA &I+1
&BY      SETC '&SYSLIST(&I)'
.*       AIF   ('&BY' EQ '+' OR '&BY' EQ '-')
           AIF   (NOT('&BY' EQ '+' OR '&BY' EQ '-')).AIF_285_1
&BY            SETC '&BY&SYSLIST(&I+1)'
&I             SETA &I+1
.*       AEND
.AIF_285_1 ANOP
&I       SETA &I+1
.*       AIF   ('&SYSLIST(&I)' NE 'UNTIL')
           AIF   (NOT('&SYSLIST(&I)' NE 'UNTIL')).AIF_286_1
               MNOTE 8,'PERFORM VARYING MISSING UNTIL'
               MEXIT
.*       AEND
.AIF_286_1 ANOP
.*       AEND
           AGO   (&ACALL_57_SET_FROM_BY_UNTIL).ACL_57_1,.ACL_57_2
.ACL_57_SKIP ANOP
.*
.* SET_IF_PARMS
.*
.*       AENTRY SET_IF_PARMS
           AGO   .ACL_56_SKIP
.ACL_56_SET_IF_PARMS ANOP
&IF_PARM SETC  '&SYSLIST(&I+1)'
&I       SETA  &I+2
.*       AWHILE (&I LE &NP)
.AWH_42_T  ANOP
           AIF    (NOT(&I LE &NP)).AWH_42_E
&IF_PARM      SETC  '&IF_PARM'.','.'&SYSLIST(&I)'
&I            SETA &I+1
.*       AEND
           AGO   .AWH_42_T
.AWH_42_E  ANOP
.*       AEND
           AGO   (&ACALL_56_SET_IF_PARMS).ACL_56_1,.ACL_56_2,.ACL_56_3,X
               .ACL_56_4
.ACL_56_SKIP ANOP
         MEND
         MACRO
.*********************************************************************
.* Copyright 2008 Automated Software Tools Corporation               *
.* This source code is part of z390 zCOBOL/assembler/emulator package*
.* The z390 package is distributed under GNU general public license  *
.* Author - Don Higgins                                              *
.* Date   - 04/15/08                                                 *
.*********************************************************************
.* 04/15/08 initial coding for global flush pending IF,EVALUATE lvls
.* 10/06/08 ZSTRMAC
.* 07/14/09 RPI 1064 drop b2's at period if used
.*********************************************************************
         PERIOD
         COPY  ZC_WS
.*       AIF (&BASE_B1_USED)               RPI 1065
           AIF (NOT(&BASE_B1_USED)).AIF_287_1                RPI 1065
&BASE_B1_USED SETB 0
             GEN_DROP &BASE_B1
.*       AEND
.AIF_287_1 ANOP
.*       AIF (&LBASE_B1_USED)              RPI 1065
           AIF (NOT(&LBASE_B1_USED)).AIF_288_1               RPI 1065
&LBASE_B1_USED SETB 0
             GEN_DROP &LBASE_B1
.*       AEND
.AIF_288_1 ANOP
.*       AIF (&BASE_B2_USED)               RPI 1065
           AIF (NOT(&BASE_B2_USED)).AIF_289_1                RPI 1065
&BASE_B2_USED SETB 0
             GEN_DROP &BASE_B2
.*       AEND
.AIF_289_1 ANOP
.*       AIF (&LBASE_B2_USED)              RPI 1065
           AIF (NOT(&LBASE_B2_USED)).AIF_290_1               RPI 1065
&LBASE_B2_USED SETB 0
             GEN_DROP &LBASE_B2
.*       AEND
.AIF_290_1 ANOP
&BASE_LRU SETA 2   SET HIGH REG FIRST AND SKIP DROP
&BASE_B1_HIGH SETA 0   RESET DYNAMIC WS BASE REGS
&BASE_B2_HIGH SETA 0
&LBASE_LRU SETA 2  SET HIGH REG FIRST AND SKIP DROP
&LBASE_B1_HIGH SETA 0  RESET DYNAMIC LK BASE REGS
&LBASE_B2_HIGH SETA 0
.*       AWHILE (&IE_LVL GT 0)
.AWH_43_T  ANOP
           AIF    (NOT(&IE_LVL GT 0)).AWH_43_E
.*             AIF   (&IE_TYPE(&IE_LVL) EQ 1)
                 AIF   (NOT(&IE_TYPE(&IE_LVL) EQ 1)).AIF_291_1
                     END_IF
.*             AELSEIF (&IE_TYPE(&IE_LVL) EQ 2)
                 AGO   .AIF_291_E
.AIF_291_1       ANOP
                 AIF     (NOT(&IE_TYPE(&IE_LVL) EQ 2)).AIF_291_2
                     END_EVALUATE
.*             AELSEIF (&IE_TYPE(&IE_LVL) EQ 3)
                 AGO   .AIF_291_E
.AIF_291_2       ANOP
                 AIF     (NOT(&IE_TYPE(&IE_LVL) EQ 3)).AIF_291_3
                     END_READ
.*             AELSEIF (&IE_TYPE(&IE_LVL) EQ 4)
                 AGO   .AIF_291_E
.AIF_291_3       ANOP
                 AIF     (NOT(&IE_TYPE(&IE_LVL) EQ 4)).AIF_291_4
                     END_PERFORM
.*             AELSEIF (&IE_TYPE(&IE_LVL) EQ 11)  RPI 1012
                 AGO   .AIF_291_E
.AIF_291_4       ANOP
                 AIF     (NOT(&IE_TYPE(&IE_LVL) EQ 11)).AIF_291_5   RPIX
                1012
                     END_ADD
.*             AELSEIF (&IE_TYPE(&IE_LVL) EQ 12)  RPI 1012
                 AGO   .AIF_291_E
.AIF_291_5       ANOP
                 AIF     (NOT(&IE_TYPE(&IE_LVL) EQ 12)).AIF_291_6   RPIX
                1012
                     END_SUBTRACT
.*             AELSEIF (&IE_TYPE(&IE_LVL) EQ 13)  RPI 1012
                 AGO   .AIF_291_E
.AIF_291_6       ANOP
                 AIF     (NOT(&IE_TYPE(&IE_LVL) EQ 13)).AIF_291_7   RPIX
                1012
                     END_MULTIPLY
.*             AELSEIF (&IE_TYPE(&IE_LVL) EQ 14)  RPI 1012
                 AGO   .AIF_291_E
.AIF_291_7       ANOP
                 AIF     (NOT(&IE_TYPE(&IE_LVL) EQ 14)).AIF_291_8   RPIX
                1012
                     END_DIVIDE
.*             AELSE
                 AGO   .AIF_291_E
.AIF_291_8       ANOP
                     MNOTE 8,'PERIOD UNKNOWN LEVEL TYPE - &IE_TYPE(&IE_X
               LVL)'
                     MEXIT
.*             AEND
.AIF_291_E       ANOP
.*       AEND
           AGO   .AWH_43_T
.AWH_43_E  ANOP
.*       AIF   (&PERIOD_NEXT)
           AIF   (NOT(&PERIOD_NEXT)).AIF_292_1
&PERIOD_NEXT   SETB 0
               GEN_LABEL PERIOD_NEXT_&PERIOD_NEXT_LAB,NEXT_SENTENCE
.*       AEND
.AIF_292_1 ANOP
         MEND

         MACRO
.*********************************************************************
.* Copyright 2008 Automated Software Tools Corporation               *
.* This source code is part of z390 zCOBOL/assembler/emulator package*
.* The z390 package is distributed under GNU general public license  *
.* Author - Don Higgins                                              *
.* Date   - 04/07/08                                                 *
.*********************************************************************
.* 04/07/08 initial coding for z390 target
.* 04/11/08 generate ZC_PROC_START, ZC_PROC_END, ZC_WS_LIST
.* 05/03/08 move ZC_WS_END to DATA END
.* 10/06/08 ZSTRMAC
.*********************************************************************
         PROCEDURE &TYPE,&USING
         COPY  ZC_WS
&NP      SETA N'&SYSLIST
.*       AIF   ('&TYPE' EQ 'DIVISION')
           AIF   (NOT('&TYPE' EQ 'DIVISION')).AIF_293_1
.*             AIF   ('&ZC_PGM' EQ '')
                 AIF   (NOT('&ZC_PGM' EQ '')).AIF_294_1
                     MNOTE 4,'PROGRAM-ID MISSING NAME MAIN ASSUMED'
&ZC_PGM              SETC  'MAIN'
.*             AEND
.AIF_294_1       ANOP
&PARMS         SETC ''
.*             AIF ('&USING' EQ 'USING')
                 AIF (NOT('&USING' EQ 'USING')).AIF_295_1
&PARMS             SETC 'USING'
&I                 SETA 3
.*                 AWHILE (&I LE &NP)
.AWH_44_T            ANOP
                     AIF    (NOT(&I LE &NP)).AWH_44_E
&PARMS                 SETC '&PARMS,&SYSLIST(&I)'
&I                     SETA &I+1
.*                 AEND
                     AGO   .AWH_44_T
.AWH_44_E            ANOP
.*             AEND
.AIF_295_1       ANOP
&ZC_PGM        GEN_PROC_START &PARMS
.*       AELSEIF   ('&TYPE' EQ 'END')
           AGO   .AIF_293_E
.AIF_293_1 ANOP
           AIF       (NOT('&TYPE' EQ 'END')).AIF_293_2
               GEN_PROC_END     GEN STOP RUN, LITERALS
               ZC_STATS       GEN ZCOBOL COMPILE STATISTICS
.*       AELSE
           AGO   .AIF_293_E
.AIF_293_2 ANOP
               MNOTE 8,'PROCEDURE TYPE NOT RECOGNIZED - &TYPE'
.*       AEND
.AIF_293_E ANOP
         MEND
         MACRO
.*********************************************************************
.* Copyright 2008 Automated Software Tools Corporation               *
.* This source code is part of z390 zCOBOL/assembler/emulator package*
.* The z390 package is distributed under GNU general public license  *
.* Author - Don Higgins                                              *
.* Date   - 04/07/08                                                 *
.*********************************************************************
.* 04/11/08 initial coding for global PROGRAM_ID
.* 10/06/08 ZSTRMAC
.*********************************************************************
         PROGRAM_ID &PGM
         COPY  ZC_WS
         GBLA  &ZC_PGM_CNT
&ZC_PGM_CNT SETA &ZC_PGM_CNT+1
.*       AIF   (&ZC_PGM_CNT GT 1)
           AIF   (NOT(&ZC_PGM_CNT GT 1)).AIF_296_1
               MNOTE 8,'MULTIPLE PGM BATCH COMPILE NOT SUPPORTED YET'
               MEXIT
.*       AEND
.AIF_296_1 ANOP
.*       AIF   ('&PGM' EQ '')
           AIF   (NOT('&PGM' EQ '')).AIF_297_1
               MNOTE 4,'PROGRAM-ID MISSING PROGRAM NAME - MAIN ASSUMED'
&ZC_PGM        SETC  'MAIN'
.*       AEND
.AIF_297_1 ANOP
&ZC_PGM  SETC  '&PGM'
         GEN_ID
         MEND
         MACRO
.*********************************************************************
.* Copyright 2008 Automated Software Tools Corporation               *
.* This source code is part of z390 zCOBOL/assembler/emulator package*
.* The z390 package is distributed under GNU general public license  *
.* Author - Don Higgins                                              *
.* Date   - 04/16/08                                                 *
.*********************************************************************
.* 02/20/09 stub for pending support
.*********************************************************************
         RD
         MNOTE 8,'RD NOT SUPPORTED YET'
         MEXIT
         MEND
         MACRO
.*********************************************************************
.* Copyright 2008 Automated Software Tools Corporation               *
.* This source code is part of z390 zCOBOL/assembler/emulator package*
.* The z390 package is distributed under GNU general public license  *
.* Author - Don Higgins                                              *
.* Date   - 04/14/08                                                 *
.*********************************************************************
.* 04/14/08 initial coding for global ZCOBOL
.* 10/06/08 ZSTRMAC
.* 09/04/09 RPI 1065 use ZC_FILE_FIND to set FILE_IX
.*********************************************************************
         READ  &FILE
         COPY  ZC_WS
         ZC_FILE_FIND &FILE        RPI 1065
.*       AIF   (&FILE_IX EQ 0)
           AIF   (NOT(&FILE_IX EQ 0)).AIF_298_1
               MNOTE 8,'READ FILE NOT FOUND - &FILE'
               MEXIT
.*       AEND
.AIF_298_1 ANOP
&NP      SETA  N'&SYSLIST
&MOVE    SETC  ''
&AT      SETC  ''
&PARM_IX SETA 2
.*       AIF   (&PARM_IX LT &NP)
           AIF   (NOT(&PARM_IX LT &NP)).AIF_299_1
.*             AIF   ('&SYSLIST(&PARM_IX)' EQ 'INTO')
                 AIF   (NOT('&SYSLIST(&PARM_IX)' EQ 'INTO')).AIF_300_1
&MOVE                SETC  '&SYSLIST(&PARM_IX+1)'
&PARM_IX             SETA  &PARM_IX+2
.*             AEND
.AIF_300_1       ANOP
.*             AIF   ('&SYSLIST(&PARM_IX)' EQ 'AT'                     X
                      AND '&SYSLIST(&PARM_IX+1)' EQ 'END')
                 AIF   (NOT('&SYSLIST(&PARM_IX)' EQ 'AT'               X
                            AND '&SYSLIST(&PARM_IX+1)' EQ 'END')).AIF_3X
               01_1
&AT                  SETC  'AT' INDICATE AT END FOLLOWS READ
.*             AELSE
                 AGO   .AIF_301_E
.AIF_301_1       ANOP
                     MNOTE 8,'READ UNKNOWN OPERAND - &SYSLIST(&PARM_IX)X
               '
                     MEXIT
.*             AEND
.AIF_301_E       ANOP
.*       AEND
.AIF_299_1 ANOP
         GEN_READ &MOVE,&AT
         MEXIT
         MEND
         MACRO
.*********************************************************************
.* Copyright 2008 Automated Software Tools Corporation               *
.* This source code is part of z390 zCOBOL/assembler/emulator package*
.* The z390 package is distributed under GNU general public license  *
.* Author - Don Higgins                                              *
.* Date   - 04/16/08                                                 *
.*********************************************************************
.* 12/13/08 turn on trace if TRACE option on for compile (OS/VS)
.*********************************************************************
         READY
         COPY  ZC_WS
.*       AIF   ('&SYSLIST(1)' NE 'TRACE')
           AIF   (NOT('&SYSLIST(1)' NE 'TRACE')).AIF_302_1
               MNOTE 8,'READY TRACE OPTION NOT FOUND'
               MEXIT
.*       AEND
.AIF_302_1 ANOP
.*       AIF   (&ZC_TRACE) TURN ON IF TRACE OPTION ACTIVE
           AIF   (NOT(&ZC_TRACE)).AIF_303_1  TURN ON IF TRACE OPTION ACX
               TIVE
               MOVE  'T',TO,READY_RESET_TRACE
.*       AEND
.AIF_303_1 ANOP
         MEXIT
         MEND
         MACRO
.*********************************************************************
.* Copyright 2008 Automated Software Tools Corporation               *
.* This source code is part of z390 zCOBOL/assembler/emulator package*
.* The z390 package is distributed under GNU general public license  *
.* Author - Don Higgins                                              *
.* Date   - 04/16/08                                                 *
.*********************************************************************
.* 02/20/09 stub for pending support
.*********************************************************************
         RECEIVE
         MNOTE 8,'RECEIVE NOT SUPPORTED YET'
         MEXIT
         MEND
         MACRO
.*********************************************************************
.* Copyright 2008 Automated Software Tools Corporation               *
.* This source code is part of z390 zCOBOL/assembler/emulator package*
.* The z390 package is distributed under GNU general public license  *
.* Author - Don Higgins                                              *
.* Date   - 04/16/08                                                 *
.*********************************************************************
.* 11/17/08 stub for pending support
.*********************************************************************
         RELEASE
         MNOTE 8,'RELEASE NOT SUPPORTED YET'
         MEXIT
         MEND
         MACRO
.*********************************************************************
.* Copyright 2008 Automated Software Tools Corporation               *
.* This source code is part of z390 zCOBOL/assembler/emulator package*
.* The z390 package is distributed under GNU general public license  *
.* Author - Don Higgins                                              *
.* Date   - 04/16/08                                                 *
.*********************************************************************
.* 12/13/08 stub for pending support
.*********************************************************************
         REMARKS
         MNOTE 8,'REMARKS REPLACED BY COMMENTS WITH * IN POS. 7'
         MEXIT
         MEND
         MACRO
.*********************************************************************
.* Copyright 2009 Automated Software Tools Corporation               *
.* This source code is part of z390 zCOBOL/assembler/emulator package*
.* The z390 package is distributed under GNU general public license  *
.* Author - Don Higgins                                              *
.* Date   - 08/18/09                                                 *
.*********************************************************************
.* 08/18/09 stub for pending support
.*********************************************************************
         REPLACE
         MNOTE 8,'REPLACE NOT SUPPORTED YET'
         MEXIT
         MEND
         MACRO
.*********************************************************************
.* Copyright 2008 Automated Software Tools Corporation               *
.* This source code is part of z390 zCOBOL/assembler/emulator package*
.* The z390 package is distributed under GNU general public license  *
.* Author - Don Higgins                                              *
.* Date   - 04/16/08                                                 *
.*********************************************************************
.* 02/20/09 stub for pending support
.*********************************************************************
         REPORT
         MNOTE 8,'REPORT NOT SUPPORTED YET'
         MEXIT
         MEND
         MACRO
.*********************************************************************
.* Copyright 2008 Automated Software Tools Corporation               *
.* This source code is part of z390 zCOBOL/assembler/emulator package*
.* The z390 package is distributed under GNU general public license  *
.* Author - Don Higgins                                              *
.* Date   - 04/16/08                                                 *
.*********************************************************************
.* 12/13/08 reset paragraph name trace (OS/VS COBOL)
.*********************************************************************
         RESET
         COPY ZC_WS
.*       AIF   ('&SYSLIST(1)' NE 'TRACE')
           AIF   (NOT('&SYSLIST(1)' NE 'TRACE')).AIF_304_1
               MNOTE 8,'READY TRACE OPTION NOT FOUND'
               MEXIT
.*       AEND
.AIF_304_1 ANOP
.*       AIF   (&ZC_TRACE) TURN OFF IF TRACE OPTION ACTIVE
           AIF   (NOT(&ZC_TRACE)).AIF_305_1  TURN OFF IF TRACE OPTION AX
               CTIVE
               MOVE  'F',TO,READY_RESET_TRACE
.*       AEND
.AIF_305_1 ANOP
         MEXIT
         MEND
         MACRO
.*********************************************************************
.* Copyright 2008 Automated Software Tools Corporation               *
.* This source code is part of z390 zCOBOL/assembler/emulator package*
.* The z390 package is distributed under GNU general public license  *
.* Author - Don Higgins                                              *
.* Date   - 04/07/08                                                 *
.*********************************************************************
.* 04/11/08 initial coding for global RETURN
.*********************************************************************
         RETURN
         GEN_RETURN
         MEND
         MACRO
.*********************************************************************
.* Copyright 2008 Automated Software Tools Corporation               *
.* This source code is part of z390 zCOBOL/assembler/emulator package*
.* The z390 package is distributed under GNU general public license  *
.* Author - Don Higgins                                              *
.* Date   - 04/16/08                                                 *
.*********************************************************************
.* 11/17/08 stub for pending support
.*********************************************************************
         REWRITE
         MNOTE 8,'REWRITE NOT SUPPORTED YET'
         MEXIT
         MEND
         MACRO
.*********************************************************************
.* Copyright 2008 Automated Software Tools Corporation               *
.* This source code is part of z390 zCOBOL/assembler/emulator package*
.* The z390 package is distributed under GNU general public license  *
.* Author - Don Higgins                                              *
.* Date   - 04/16/08                                                 *
.*********************************************************************
.* 02/20/09 stub for pending support
.*********************************************************************
         SEARCH
         MNOTE 8,'SEARCH NOT SUPPORTED YET'
         MEXIT
         MEND
         MACRO
.*********************************************************************
.* Copyright 2008 Automated Software Tools Corporation               *
.* This source code is part of z390 zCOBOL/assembler/emulator package*
.* The z390 package is distributed under GNU general public license  *
.* Author - Don Higgins                                              *
.* Date   - 04/16/08                                                 *
.*********************************************************************
.* 02/20/09 stub for pending support
.*********************************************************************
         SECURITY
         MEXIT
         MEND
        MACRO
.*********************************************************************
.* Copyright 2008 Automated Software Tools Corporation               *
.* This source code is part of z390 zCOBOL/assembler/emulator package*
.* The z390 package is distributed under GNU general public license  *
.* Author - Don Higgins                                              *
.* Date   - 04/07/08                                                 *
.*********************************************************************
.* 04/07/08 initial coding for global ZCOBOL
.* 10/06/08 ZSTRMAC
.* 01/02/09 add optional OPTIONAL, and default ORGANIZATION LINE SEQ.
.* 09/07/09 RPI 1065 allow options in any order
.*********************************************************************
.* Add file name, ddname or dsname, and recfm to file table.
.* See FD which generated DCB in WS and adds record name to file table.
.* Notes:
.*   1.  RECFM/BLKSIZE set at open after WS defines record name
.*********************************************************************
         SELECT
         COPY  ZC_WS
&STATUS  SETC '' NO DEFAULT FILE STATUS FIELD
&PARM_IX SETA 1
.*       AIF   ('&SYSLIST(&PARM_IX)' EQ 'OPTIONAL')
           AIF   (NOT('&SYSLIST(&PARM_IX)' EQ 'OPTIONAL')).AIF_306_1
&PARM_IX       SETA &PARM_IX+1            IGNORE OPTIONAL
.*       AEND
.AIF_306_1 ANOP
&FILE    SETC '&SYSLIST(&PARM_IX)'
&PARM_IX SETA &PARM_IX+1
.*       AIF   ('&SYSLIST(&PARM_IX)' NE 'ASSIGN')
           AIF   (NOT('&SYSLIST(&PARM_IX)' NE 'ASSIGN')).AIF_307_1
               MNOTE 8,'SELECT ASSIGN TO NOT FOUND'
               MEXIT
.*       AEND
.AIF_307_1 ANOP
&PARM_IX SETA &PARM_IX+1
.*       AIF   ('&SYSLIST(&PARM_IX)' EQ 'TO')
           AIF   (NOT('&SYSLIST(&PARM_IX)' EQ 'TO')).AIF_308_1
&PARM_IX       SETA &PARM_IX+1
.*       AEND
.AIF_308_1 ANOP
&DDNAME  SETC '&SYSLIST(&PARM_IX)'
&PARM_IX SETA &PARM_IX+1
&FILE_TOT SETA &FILE_TOT+1
&FILE_NAME(&FILE_TOT) SETC '&FILE'
&FILE_DDNAME(&FILE_TOT) SETC '&DDNAME'
&ORG     SETC 'QSAM' QSAM,LSEQ,RRDS,KSDS
&MODE    SETC 'SEQ'  SEQ,RAN,DYN
&KEY     SETC '' INDEXED  KEY FIELD
&XRBA    SETC '' RELATIVE KEY FIELD
.*
.* PROCESS OPTIONAL PARMS IN ANY ORDER
.*
.*     AWHILE  ('&SYSLIST(&PARM_IX)' NE '')
.AWH_45_T ANOP
         AIF     (NOT('&SYSLIST(&PARM_IX)' NE '')).AWH_45_E
.*       AIF   ('&SYSLIST(&PARM_IX)' EQ 'ORGANIZATION')
           AIF   (NOT('&SYSLIST(&PARM_IX)' EQ 'ORGANIZATION')).AIF_309_X
               1
&PARM_IX       SETA &PARM_IX+1
.*             AIF ('&SYSLIST(&PARM_IX)' EQ 'IS')
                 AIF (NOT('&SYSLIST(&PARM_IX)' EQ 'IS')).AIF_310_1
&PARM_IX           SETA &PARM_IX+1
.*             AEND
.AIF_310_1       ANOP
.*             AIF   ('&SYSLIST(&PARM_IX)' EQ 'LINE'                   X
                      AND '&SYSLIST(&PARM_IX+1)' EQ 'SEQUENTIAL')
                 AIF   (NOT('&SYSLIST(&PARM_IX)' EQ 'LINE'             X
                            AND '&SYSLIST(&PARM_IX+1)' EQ 'SEQUENTIAL')X
               ).AIF_311_1
&ORG                 SETC 'LSEQ'
&MODE                SETC 'SEQ'
&PARM_IX             SETA &PARM_IX+2
.*             AELSEIF ('&SYSLIST(&PARM_IX)' EQ 'SEQUENTIAL')
                 AGO   .AIF_311_E
.AIF_311_1       ANOP
                 AIF     (NOT('&SYSLIST(&PARM_IX)' EQ 'SEQUENTIAL')).AIX
               F_311_2
&ORG                 SETC 'QSAM'
&PARM_IX             SETA &PARM_IX+1
.*             AELSEIF ('&SYSLIST(&PARM_IX)' EQ 'INDEXED')
                 AGO   .AIF_311_E
.AIF_311_2       ANOP
                 AIF     (NOT('&SYSLIST(&PARM_IX)' EQ 'INDEXED')).AIF_3X
               11_3
&ORG                 SETC 'KSDS'
&PARM_IX             SETA &PARM_IX+1
.*             AELSEIF ('&SYSLIST(&PARM_IX)' EQ 'RELATIVE')
                 AGO   .AIF_311_E
.AIF_311_3       ANOP
                 AIF     (NOT('&SYSLIST(&PARM_IX)' EQ 'RELATIVE')).AIF_X
               311_4
&ORG                 SETC 'RRDS'
&PARM_IX             SETA &PARM_IX+1
.*             AELSE
                 AGO   .AIF_311_E
.AIF_311_4       ANOP
                     MNOTE 8,'SELECT UNKNOWN ORGANIZATION TYPE = &SYSLIX
               ST(&PARM_IX)'
&PARM_IX             SETA N'&SYSLIST+1
.*             AEND
.AIF_311_E       ANOP
.*       AELSEIF ('&SYSLIST(&PARM_IX)' EQ 'ACCESS')
           AGO   .AIF_309_E
.AIF_309_1 ANOP
           AIF     (NOT('&SYSLIST(&PARM_IX)' EQ 'ACCESS')).AIF_309_2
&PARM_IX       SETA &PARM_IX+1
.*             AIF ('&SYSLIST(&PARM_IX)' EQ 'MODE')
                 AIF (NOT('&SYSLIST(&PARM_IX)' EQ 'MODE')).AIF_312_1
&PARM_IX           SETA &PARM_IX+1
.*             AEND
.AIF_312_1       ANOP
.*             AIF ('&SYSLIST(&PARM_IX)' EQ 'IS')
                 AIF (NOT('&SYSLIST(&PARM_IX)' EQ 'IS')).AIF_313_1
&PARM_IX           SETA &PARM_IX+1
.*             AEND
.AIF_313_1       ANOP
.*             AIF ('&SYSLIST(&PARM_IX)' EQ 'SEQUENTIAL')
                 AIF (NOT('&SYSLIST(&PARM_IX)' EQ 'SEQUENTIAL')).AIF_31X
               4_1
&PARM_IX           SETA &PARM_IX+1
&MODE              SETC 'SEQ'
.*             AELSEIF ('&SYSLIST(&PARM_IX)' EQ 'DYNAMIC')
                 AGO   .AIF_314_E
.AIF_314_1       ANOP
                 AIF     (NOT('&SYSLIST(&PARM_IX)' EQ 'DYNAMIC')).AIF_3X
               14_2
&PARM_IX           SETA &PARM_IX+1
&MODE              SETC 'DYN'
.*             AELSEIF ('&SYSLIST(&PARM_IX)' EQ 'RANDOM')
                 AGO   .AIF_314_E
.AIF_314_2       ANOP
                 AIF     (NOT('&SYSLIST(&PARM_IX)' EQ 'RANDOM')).AIF_31X
               4_3
&PARM_IX           SETA &PARM_IX+1
&MODE              SETC 'RAN'
.*             AELSE
                 AGO   .AIF_314_E
.AIF_314_3       ANOP
                   MNOTE 8,'SELECT ACCESS MODE UNKNOWN - &SYSLIST(&PARMX
               _IX)'
&PARM_IX           SETA N'&SYSLIST+1
.*             AEND
.AIF_314_E       ANOP
.*       AELSEIF ('&SYSLIST(&PARM_IX)' EQ 'RECORD')
           AGO   .AIF_309_E
.AIF_309_2 ANOP
           AIF     (NOT('&SYSLIST(&PARM_IX)' EQ 'RECORD')).AIF_309_3
&PARM_IX       SETA &PARM_IX+1
.*             AIF ('&SYSLIST(&PARM_IX)' EQ 'KEY')
                 AIF (NOT('&SYSLIST(&PARM_IX)' EQ 'KEY')).AIF_315_1
&PARM_IX           SETA &PARM_IX+1
.*             AEND
.AIF_315_1       ANOP
.*             AIF ('&SYSLIST(&PARM_IX)' EQ 'IS')
                 AIF (NOT('&SYSLIST(&PARM_IX)' EQ 'IS')).AIF_316_1
&PARM_IX           SETA &PARM_IX+1
.*             AEND
.AIF_316_1       ANOP
&KEY           SETC '&SYSLIST(&PARM_IX)'
&PARM_IX       SETA &PARM_IX+1
.*       AELSEIF ('&SYSLIST(&PARM_IX)' EQ 'RELATIVE')
           AGO   .AIF_309_E
.AIF_309_3 ANOP
           AIF     (NOT('&SYSLIST(&PARM_IX)' EQ 'RELATIVE')).AIF_309_4
&PARM_IX       SETA &PARM_IX+1
.*             AIF ('&SYSLIST(&PARM_IX)' EQ 'KEY')
                 AIF (NOT('&SYSLIST(&PARM_IX)' EQ 'KEY')).AIF_317_1
&PARM_IX           SETA &PARM_IX+1
.*             AEND
.AIF_317_1       ANOP
.*             AIF ('&SYSLIST(&PARM_IX)' EQ 'IS')
                 AIF (NOT('&SYSLIST(&PARM_IX)' EQ 'IS')).AIF_318_1
&PARM_IX           SETA &PARM_IX+1
.*             AEND
.AIF_318_1       ANOP
&XRBA          SETC '&SYSLIST(&PARM_IX)'
&PARM_IX       SETA &PARM_IX+1
.*       AELSEIF ('&SYSLIST(&PARM_IX)' EQ 'FILE'                       X
               OR '&SYSLIST(&PARM_IX)' EQ 'STATUS')
           AGO   .AIF_309_E
.AIF_309_4 ANOP
           AIF     (NOT('&SYSLIST(&PARM_IX)' EQ 'FILE'                 X
                     OR '&SYSLIST(&PARM_IX)' EQ 'STATUS')).AIF_309_5
&PARM_IX       SETA &PARM_IX+1
.*             AIF ('&SYSLIST(&PARM_IX)' EQ 'STATUS')
                 AIF (NOT('&SYSLIST(&PARM_IX)' EQ 'STATUS')).AIF_319_1
&PARM_IX           SETA &PARM_IX+1
.*             AEND
.AIF_319_1       ANOP
.*             AIF ('&SYSLIST(&PARM_IX)' EQ 'IS')
                 AIF (NOT('&SYSLIST(&PARM_IX)' EQ 'IS')).AIF_320_1
&PARM_IX           SETA &PARM_IX+1
.*             AEND
.AIF_320_1       ANOP
&STATUS        SETC '&SYSLIST(&PARM_IX)'
&PARM_IX       SETA &PARM_IX+1
.*       AELSE
           AGO   .AIF_309_E
.AIF_309_5 ANOP
               MNOTE 8,'SELECT UNKNOWN OPTION - &SYSLIST(&PARM_IX)'
&PARM_IX       SETA N'&SYSLIST+1
.*       AEND
.AIF_309_E ANOP
.*     AEND
         AGO   .AWH_45_T
.AWH_45_E ANOP
.*
.* SAVE FILE INFO
.*
&FILE_ORG(&FILE_TOT) SETC '&ORG'
&FILE_MODE(&FILE_TOT) SETC '&MODE'
&FILE_KEY(&FILE_TOT) SETC '&KEY'
&FILE_XRBA(&FILE_TOT) SETC '&XRBA'
&FILE_STATUS(&FILE_TOT) SETC '&STATUS'
.*     AIF     ('&SYSLIST(&PARM_IX)' NE '')
         AIF     (NOT('&SYSLIST(&PARM_IX)' NE '')).AIF_321_1
               MNOTE 8,'SELECT UNKNOWN OPTION - &SYSLIST(&PARM_IX)'
               MEXIT
.*     AEND
.AIF_321_1 ANOP

.*
.* VALIDATE LINE SEQUENTIAL ASCII TEXT FILE TO/FROM FIXED RECORD
.*
.*     AIF ('&ORG' EQ 'LSEQ')
         AIF (NOT('&ORG' EQ 'LSEQ')).AIF_322_1
.*         AIF ('&MODE' NE 'SEQ')
             AIF (NOT('&MODE' NE 'SEQ')).AIF_323_1
               MNOTE 8,'SELECT INVALID LINE SEQUENTIAL MODE - &MODE'
               MEXIT
.*         AEND
.AIF_323_1   ANOP
.*         AIF ('&KEY' NE '')
             AIF (NOT('&KEY' NE '')).AIF_324_1
               MNOTE 8,'SELECT INVALID LINE SEQUENTIAL KEY - &KEY'
               MEXIT
.*         AEND
.AIF_324_1   ANOP
.*         AIF ('&XRBA' NE '')
             AIF (NOT('&XRBA' NE '')).AIF_325_1
               MNOTE 8,'SELECT INVALID LINE SEQUENTIAL XRBA - &XRBA'
               MEXIT
.*         AEND
.AIF_325_1   ANOP
.*     AELSEIF ('&ORG' EQ 'QSAM')
         AGO   .AIF_322_E
.AIF_322_1 ANOP
         AIF     (NOT('&ORG' EQ 'QSAM')).AIF_322_2
.*         AIF ('&MODE' NE 'SEQ')
             AIF (NOT('&MODE' NE 'SEQ')).AIF_326_1
               MNOTE 8,'SELECT INVALID SEQUENTIAL MODE - &MODE'
               MEXIT
.*         AEND
.AIF_326_1   ANOP
.*         AIF ('&KEY' NE '')
             AIF (NOT('&KEY' NE '')).AIF_327_1
               MNOTE 8,'SELECT INVALID SEQUENTIAL KEY - &KEY'
               MEXIT
.*         AEND
.AIF_327_1   ANOP
.*         AIF ('&XRBA' NE '')
             AIF (NOT('&XRBA' NE '')).AIF_328_1
               MNOTE 8,'SELECT INVALID SEQUENTIAL XRBA - &XRBA'
               MEXIT
.*         AEND
.AIF_328_1   ANOP
.*     AELSEIF ('&ORG' EQ 'KSDS')
         AGO   .AIF_322_E
.AIF_322_2 ANOP
         AIF     (NOT('&ORG' EQ 'KSDS')).AIF_322_3
.*         AIF ('&KEY' EQ '')
             AIF (NOT('&KEY' EQ '')).AIF_329_1
               MNOTE 8,'SELECT INVALID INDEXED KEY - &KEY'
               MEXIT
.*         AEND
.AIF_329_1   ANOP
.*     AELSEIF ('&ORG' EQ 'RRDS')
         AGO   .AIF_322_E
.AIF_322_3 ANOP
         AIF     (NOT('&ORG' EQ 'RRDS')).AIF_322_4
.*         AIF ('&KEY' NE '')
             AIF (NOT('&KEY' NE '')).AIF_330_1
               MNOTE 8,'SELECT INVALID RELATIVE KEY - &KEY'
               MEXIT
.*         AEND
.AIF_330_1   ANOP
.*         AIF ('&XRBA' EQ '' AND '&MODE' NE 'SEQ')
             AIF (NOT('&XRBA' EQ '' AND '&MODE' NE 'SEQ')).AIF_331_1
               MNOTE 8,'SELECT INVALID RELATIVE XRBA - &XRBA'
               MEXIT
.*         AEND
.AIF_331_1   ANOP
.*     AELSE
         AGO   .AIF_322_E
.AIF_322_4 ANOP
         MNOTE 8,'SELECT ORG NOT SUPPORTED YET - &ORG'
         MEXIT
.*     AEND
.AIF_322_E ANOP
         MEND
         MACRO
.*********************************************************************
.* Copyright 2008 Automated Software Tools Corporation               *
.* This source code is part of z390 zCOBOL/assembler/emulator package*
.* The z390 package is distributed under GNU general public license  *
.* Author - Don Higgins                                              *
.* Date   - 04/16/08                                                 *
.*********************************************************************
.* 02/20/09 stub for pending support
.*********************************************************************
         SEND
         MNOTE 8,'SEND NOT SUPPORTED YET'
         MEXIT
         MEND
         MACRO
.*********************************************************************
.* Copyright 2008 Automated Software Tools Corporation               *
.* This source code is part of z390 zCOBOL/assembler/emulator package*
.* The z390 package is distributed under GNU general public license  *
.* Author - Don Higgins                                              *
.* Date   - 04/16/08                                                 *
.*********************************************************************
.* 02/23/09 initial coding for global SET
.*********************************************************************
         SET
         COPY  ZC_WS
&PARM_IX SETA 1
.*       AWHILE (&PARM_IX LT N'&SYSLIST)
.AWH_46_T  ANOP
           AIF    (NOT(&PARM_IX LT N'&SYSLIST)).AWH_46_E
.*              AIF   ('&SYSLIST(&PARM_IX)' EQ 'TO')
                  AIF   (NOT('&SYSLIST(&PARM_IX)' EQ 'TO')).AIF_332_1
&LAST_PARM            SETA &PARM_IX-1
&SOURCE_PARM          SETA &PARM_IX+1
.*                    AIF ('&SYSLIST(&SOURCE_PARM)' EQ 'TRUE')
                        AIF (NOT('&SYSLIST(&SOURCE_PARM)' EQ 'TRUE')).AX
               IF_333_1
&TYPE                     SETA 2
.*                    AELSE
                        AGO   .AIF_333_E
.AIF_333_1              ANOP
&TYPE                     SETA 1
.*                    AEND
.AIF_333_E              ANOP
.*                    AEXIT AWHILE
                        AGO   .AWH_46_E
.*              AELSEIF ('&SYSLIST(&PARM_IX)' EQ 'UP'                  X
                         AND '&SYSLIST(&PARM_IX+1)' EQ 'BY')
                  AGO   .AIF_332_E
.AIF_332_1        ANOP
                  AIF     (NOT('&SYSLIST(&PARM_IX)' EQ 'UP'            X
                               AND '&SYSLIST(&PARM_IX+1)' EQ 'BY')).AIFX
               _332_2
&LAST_PARM            SETA &PARM_IX-1
&SOURCE_PARM          SETA &PARM_IX+2
&TYPE                 SETA 3
.*                    AEXIT AWHILE
                        AGO   .AWH_46_E
.*              AELSEIF ('&SYSLIST(&PARM_IX)' EQ 'DOWN'                X
                        AND '&SYSLIST(&PARM_IX+1)' EQ 'BY')
                  AGO   .AIF_332_E
.AIF_332_2        ANOP
                  AIF     (NOT('&SYSLIST(&PARM_IX)' EQ 'DOWN'          X
                              AND '&SYSLIST(&PARM_IX+1)' EQ 'BY')).AIF_X
               332_3
&LAST_PARM            SETA &PARM_IX-1
&SOURCE_PARM          SETA &PARM_IX+2
&TYPE                 SETA 4
.*                    AEXIT AWHILE
                        AGO   .AWH_46_E
.*              AELSE
                  AGO   .AIF_332_E
.AIF_332_3        ANOP
&PARM_IX              SETA &PARM_IX+1
.*              AEND
.AIF_332_E        ANOP
.*       AEND
           AGO   .AWH_46_T
.AWH_46_E  ANOP
.*       AIF   (&TYPE EQ 0)
           AIF   (NOT(&TYPE EQ 0)).AIF_334_1
               MNOTE 8,'SET MISSING TO, UP BY, OR DOWN BY'
               MEXIT
.*       AEND
.AIF_334_1 ANOP
&PARM_IX SETA &SOURCE_PARM
&FIELD_REG SETC 'ZC_R3'
&FIELD_SS1 SETB 0
.*       ACALL GET_PARM_FIELD
&ACALL_6_GET_PARM_FIELD SETA    28
           AGO   .ACL_6_GET_PARM_FIELD
.ACL_6_28  ANOP
&SOURCE  SETC '&FIELD_NAME'
&SOURCE_IX SETA &FIELD_IX
&PARM_IX SETA 1
.*       AWHILE (&PARM_IX LE &LAST_PARM)
.AWH_47_T  ANOP
           AIF    (NOT(&PARM_IX LE &LAST_PARM)).AWH_47_E
.*             AIF (&SOURCE_IX GT 0 AND &PARM_IX GT 1)
                 AIF (NOT(&SOURCE_IX GT 0 AND &PARM_IX GT 1)).AIF_335_1
                   GEN_BASE &SOURCE_IX
.*             AEND
.AIF_335_1       ANOP
&FIELD_REG     SETC 'ZC_R2'
&FIELD_SS1     SETB 1
.*             ACALL GET_PARM_FIELD
&ACALL_6_GET_PARM_FIELD SETA    29
                 AGO   .ACL_6_GET_PARM_FIELD
.ACL_6_29        ANOP
.*             AIF   (&FIELD_IX GT 0)
                 AIF   (NOT(&FIELD_IX GT 0)).AIF_336_1
&TARGET              SETC '&FIELD_NAME'
&TARGET_IX           SETA &FIELD_IX
.*             AELSE
                 AGO   .AIF_336_E
.AIF_336_1       ANOP
                     MNOTE 8,'SET MISSING TARGET FIELD - &SYSLIST(1)'
                     MEXIT
.*             AEND
.AIF_336_E       ANOP
               GEN_SET &TYPE,&TARGET,&TARGET_IX,&SOURCE,&SOURCE_IX
.*       AEND
           AGO   .AWH_47_T
.AWH_47_E  ANOP
         MEXIT
         COPY  ZCGETFLD
         MEND
         MACRO
.*********************************************************************
.* Copyright 2008 Automated Software Tools Corporation               *
.* This source code is part of z390 zCOBOL/assembler/emulator package*
.* The z390 package is distributed under GNU general public license  *
.* Author - Don Higgins                                              *
.* Date   - 04/16/08                                                 *
.*********************************************************************
.* 11/17/08 stub for pending support
.*********************************************************************
         SORT
         MNOTE 8,'SORT NOT SUPPORTED YET'
         MEXIT
         MEND
         MACRO
.*********************************************************************
.* Copyright 2008 Automated Software Tools Corporation               *
.* This source code is part of z390 zCOBOL/assembler/emulator package*
.* The z390 package is distributed under GNU general public license  *
.* Author - Don Higgins                                              *
.* Date   - 04/16/08                                                 *
.*********************************************************************
.* 04/16/08 initial coding for global SOURCE-COMPUTER
.*********************************************************************
         SOURCE_COMPUTER
         MEND
         MACRO
.*********************************************************************
.* Copyright 2008 Automated Software Tools Corporation               *
.* This source code is part of z390 zCOBOL/assembler/emulator package*
.* The z390 package is distributed under GNU general public license  *
.* Author - Don Higgins                                              *
.* Date   - 04/16/08                                                 *
.*********************************************************************
.* 01/02/09 initial coding for gobal SPECIAL-NAMES
.*********************************************************************
         SPECIAL_NAMES
         MEND
         MACRO
.*********************************************************************
.* Copyright 2008 Automated Software Tools Corporation               *
.* This source code is part of z390 zCOBOL/assembler/emulator package*
.* The z390 package is distributed under GNU general public license  *
.* Author - Don Higgins                                              *
.* Date   - 04/07/08                                                 *
.*********************************************************************
.* 04/09/08 initial coding for global STOP RUN
.*********************************************************************
         STOP  &RUN
         GEN_STOP &RUN
         MEND
         MACRO
.*********************************************************************
.* Copyright 2008 Automated Software Tools Corporation               *
.* This source code is part of z390 zCOBOL/assembler/emulator package*
.* The z390 package is distributed under GNU general public license  *
.* Author - Don Higgins                                              *
.* Date   - 04/16/08                                                 *
.*********************************************************************
.* 11/17/08 stub for pending support
.*********************************************************************
         STRING
         MNOTE 8,'STRING NOT SUPPORTED YET'
         MEXIT
         MEND
         MACRO
.*********************************************************************
.* Copyright 2008 Automated Software Tools Corporation               *
.* This source code is part of z390 zCOBOL/assembler/emulator package*
.* The z390 package is distributed under GNU general public license  *
.* Author - Don Higgins                                              *
.* Date   - 04/07/08                                                 *
.*********************************************************************
.* 10/28/08 initial coding for global SUBTRACT
.* 12/22/08 use ZCGETFLD to get field with qual,base,subscript
.* 02/15/09 use ADD as model for mult source and target support
.* 02/26/09 RPI 1012 move onsize end-add pending to SUBTRACT
.* 08/09/09 RPI 1065 add ACTR for mult operands
.*********************************************************************
         SUBTRACT
         COPY  ZC_WS
&PARM_IX SETA 2
&NP      SETA N'&SYSLIST
&FROM_IX SETA 0
&GIVING_IX SETA 0
&ROUNDED SETB 0
&ONSIZE  SETB 0
&LAST_PARM SETA 0
.*       AWHILE (&PARM_IX LT &NP)
.AWH_48_T  ANOP
           AIF    (NOT(&PARM_IX LT &NP)).AWH_48_E
               ACTR 10000
.*             AIF ('&SYSLIST(&PARM_IX)' EQ 'FROM')
                 AIF (NOT('&SYSLIST(&PARM_IX)' EQ 'FROM')).AIF_337_1
&FROM_IX           SETA &PARM_IX
.*             AELSEIF ('&SYSLIST(&PARM_IX)' EQ 'GIVING')
                 AGO   .AIF_337_E
.AIF_337_1       ANOP
                 AIF     (NOT('&SYSLIST(&PARM_IX)' EQ 'GIVING')).AIF_33X
               7_2
&GIVING_IX         SETA &PARM_IX
.*             AELSEIF ('&SYSLIST(&PARM_IX)' EQ 'ROUNDED')
                 AGO   .AIF_337_E
.AIF_337_2       ANOP
                 AIF     (NOT('&SYSLIST(&PARM_IX)' EQ 'ROUNDED')).AIF_3X
               37_3
&ROUNDED           SETB 1
.*                 AIF (&LAST_PARM EQ 0)
                     AIF (NOT(&LAST_PARM EQ 0)).AIF_338_1
&LAST_PARM             SETA &PARM_IX-1
.*                 AEND
.AIF_338_1           ANOP
.*             AELSEIF ('&SYSLIST(&PARM_IX)' EQ 'ON'                   X
                        AND '&SYSLIST(&PARM_IX+1)' EQ 'SIZE'           X
                        AND '&SYSLIST(&PARM_IX+2)' EQ 'ERROR')
                 AGO   .AIF_337_E
.AIF_337_3       ANOP
                 AIF     (NOT('&SYSLIST(&PARM_IX)' EQ 'ON'             X
                              AND '&SYSLIST(&PARM_IX+1)' EQ 'SIZE'     X
                              AND '&SYSLIST(&PARM_IX+2)' EQ 'ERROR')).AX
               IF_337_4
&ONSIZE            SETB 1
.*                 AIF (&LAST_PARM EQ 0)
                     AIF (NOT(&LAST_PARM EQ 0)).AIF_339_1
&LAST_PARM             SETA &PARM_IX-1
.*                 AEND
.AIF_339_1           ANOP
&PARM_IX           SETA &PARM_IX+2
.*             AELSEIF ('&SYSLIST(&PARM_IX)' EQ 'SIZE'                 X
                        AND '&SYSLIST(&PARM_IX+1)' EQ 'ERROR')
                 AGO   .AIF_337_E
.AIF_337_4       ANOP
                 AIF     (NOT('&SYSLIST(&PARM_IX)' EQ 'SIZE'           X
                              AND '&SYSLIST(&PARM_IX+1)' EQ 'ERROR')).AX
               IF_337_5
&ONSIZE            SETB 1
.*                 AIF (&LAST_PARM EQ 0)
                     AIF (NOT(&LAST_PARM EQ 0)).AIF_340_1
&LAST_PARM             SETA &PARM_IX-1
.*                 AEND
.AIF_340_1           ANOP
&PARM_IX           SETA &PARM_IX+1
.*             AEND
.AIF_337_5       ANOP
.AIF_337_E       ANOP
&PARM_IX       SETA &PARM_IX+1
.*       AEND
           AGO   .AWH_48_T
.AWH_48_E  ANOP
.*       AIF   (&LAST_PARM GT 0)
           AIF   (NOT(&LAST_PARM GT 0)).AIF_341_1
&NP            SETA &LAST_PARM
.*       AEND
.AIF_341_1 ANOP
.*       AIF   (&FROM_IX EQ 0)
           AIF   (NOT(&FROM_IX EQ 0)).AIF_342_1
               MNOTE 8,'SUBTRACT MISSING FROM'
               MEXIT
.*       AEND
.AIF_342_1 ANOP
.*       AIF   (&ONSIZE)
           AIF   (NOT(&ONSIZE)).AIF_343_1
&IE_LVL        SETA  &IE_LVL+1         ADD PENDING END_SUB
&IE_TYPE(&IE_LVL) SETA 12        FOR END_SUB OR PERIOD
&SUB_LAB       SETA &SUB_LAB+1
&IE_TCNT(&IE_LVL) SETA &SUB_LAB
.*       AEND
.AIF_343_1 ANOP
.*       AIF   (&GIVING_IX GT 0)  SUBTRACT S1+ FROM S2 GIVING S3+
           AIF   (NOT(&GIVING_IX GT 0)).AIF_344_1   SUBTRACT S1+ FROM SX
               2 GIVING S3+
.*             ACALL SUB_GIVING
&ACALL_58_SUB_GIVING SETA    1
                 AGO   .ACL_58_SUB_GIVING
.ACL_58_1        ANOP
.*       AELSE
           AGO   .AIF_344_E
.AIF_344_1 ANOP
.*             ACALL SUB_FROM
&ACALL_59_SUB_FROM SETA    1
                 AGO   .ACL_59_SUB_FROM
.ACL_59_1        ANOP
.*       AEND
.AIF_344_E ANOP
.*       AIF   (&ONSIZE)
           AIF   (NOT(&ONSIZE)).AIF_345_1
               GEN_ONSIZE_START SUB
.*       AEND
.AIF_345_1 ANOP
         MEXIT
.*
.* SUB GIVING
.*
.*       AENTRY SUB_GIVING
           AGO   .ACL_58_SKIP
.ACL_58_SUB_GIVING ANOP
&S1_PARM_IX SETA 1
&S2_PARM_IX SETA &FROM_IX+1
&S3_PARM_IX SETA &GIVING_IX+1
&S1_OK   SETB 1
&S2_OK   SETB 1
&S3_OK   SETB 1
.*       ACALL GET_S1
&ACALL_3_GET_S1 SETA    9
           AGO   .ACL_3_GET_S1
.ACL_3_9   ANOP
.*       ACALL GET_S2
&ACALL_5_GET_S2 SETA    5
           AGO   .ACL_5_GET_S2
.ACL_5_5   ANOP
.*       ACALL GET_S3
&ACALL_4_GET_S3 SETA    5
           AGO   .ACL_4_GET_S3
.ACL_4_5   ANOP
.*       AWHILE (&S1_OK AND &S2_OK AND &S3_OK)
.AWH_49_T  ANOP
           AIF    (NOT(&S1_OK AND &S2_OK AND &S3_OK)).AWH_49_E
               GEN_MOVE &S3,&S3_IX,&S2,&S2_IX
               GEN_SUB  &S3,&S3_IX,&S1,&S1_IX,&ROUNDED,&ONSIZE
.*             ACALL GET_S1
&ACALL_3_GET_S1  SETA    10
                 AGO   .ACL_3_GET_S1
.ACL_3_10        ANOP
.*             AWHILE (&S1_OK)
.AWH_50_T        ANOP
                 AIF    (NOT(&S1_OK)).AWH_50_E
                   GEN_SUB  &S3,&S3_IX,&S1,&S1_IX,&ROUNDED,&ONSIZE
.*                 ACALL GET_S1
&ACALL_3_GET_S1      SETA    11
                     AGO   .ACL_3_GET_S1
.ACL_3_11            ANOP
.*             AEND
                 AGO   .AWH_50_T
.AWH_50_E        ANOP
.*             ACALL GET_S3
&ACALL_4_GET_S3  SETA    6
                 AGO   .ACL_4_GET_S3
.ACL_4_6         ANOP
.*             AIF (&S3_OK)
                 AIF (NOT(&S3_OK)).AIF_346_1
&S1_PARM_IX        SETA 1
&S1_OK             SETB 1
.*                 ACALL GET_S1
&ACALL_3_GET_S1      SETA    12
                     AGO   .ACL_3_GET_S1
.ACL_3_12            ANOP
.*             AEND
.AIF_346_1       ANOP
.*       AEND
           AGO   .AWH_49_T
.AWH_49_E  ANOP
.*       AEND
           AGO   (&ACALL_58_SUB_GIVING).ACL_58_1
.ACL_58_SKIP ANOP
.*
.* SUB FROM
.*
.*       AENTRY SUB_FROM
           AGO   .ACL_59_SKIP
.ACL_59_SUB_FROM ANOP
&S1_PARM_IX SETA 1
&S2_PARM_IX SETA &FROM_IX+1
&S1_OK   SETB 1
&S2_OK   SETB 1
.*       ACALL GET_S1
&ACALL_3_GET_S1 SETA    13
           AGO   .ACL_3_GET_S1
.ACL_3_13  ANOP
.*       ACALL GET_S2
&ACALL_5_GET_S2 SETA    6
           AGO   .ACL_5_GET_S2
.ACL_5_6   ANOP
.*       AWHILE (&S1_OK AND &S2_OK)
.AWH_51_T  ANOP
           AIF    (NOT(&S1_OK AND &S2_OK)).AWH_51_E
               GEN_SUB  &S2,&S2_IX,&S1,&S1_IX,&ROUNDED,&ONSIZE
.*             ACALL GET_S1
&ACALL_3_GET_S1  SETA    14
                 AGO   .ACL_3_GET_S1
.ACL_3_14        ANOP
.*             AWHILE (&S1_OK)
.AWH_52_T        ANOP
                 AIF    (NOT(&S1_OK)).AWH_52_E
                   GEN_SUB  &S2,&S2_IX,&S1,&S1_IX,&ROUNDED,&ONSIZE
.*                 ACALL GET_S1
&ACALL_3_GET_S1      SETA    15
                     AGO   .ACL_3_GET_S1
.ACL_3_15            ANOP
.*             AEND
                 AGO   .AWH_52_T
.AWH_52_E        ANOP
.*             ACALL GET_S2
&ACALL_5_GET_S2  SETA    7
                 AGO   .ACL_5_GET_S2
.ACL_5_7         ANOP
.*             AIF (&S2_OK)
                 AIF (NOT(&S2_OK)).AIF_347_1
&S1_PARM_IX        SETA 1
&S1_OK             SETB 1
.*                 ACALL GET_S1
&ACALL_3_GET_S1      SETA    16
                     AGO   .ACL_3_GET_S1
.ACL_3_16            ANOP
.*             AEND
.AIF_347_1       ANOP
.*       AEND
           AGO   .AWH_51_T
.AWH_51_E  ANOP
.*       AEND
           AGO   (&ACALL_59_SUB_FROM).ACL_59_1
.ACL_59_SKIP ANOP
.*
.* GET S1
.*
.*       AENTRY GET_S1
 MNOTE 8'ZSTRMAC ERROR AENTRY DUPLICATE NAME FOUND - GET_S1
&PARM_IX SETA &S1_PARM_IX
.*       AIF   (&PARM_IX EQ &FROM_IX)
           AIF   (NOT(&PARM_IX EQ &FROM_IX)).AIF_348_1
&S1_OK         SETB 0
.*             AEXIT AENTRY
 MNOTE 8'ZSTRMAC ERROR AEXIT NOT WITHIN AENTRY, AWHILE, ACASE
.*       AEND
.AIF_348_1 ANOP
&FIELD_REG SETC 'ZC_R1'         S1 IN R1
&FIELD_SS1 SETB 0
.*       ACALL GET_PARM_FIELD
&ACALL_6_GET_PARM_FIELD SETA    30
           AGO   .ACL_6_GET_PARM_FIELD
.ACL_6_30  ANOP
&S1_PARM_IX SETA &PARM_IX
&S1      SETC  '&FIELD_NAME'
&S1_IX   SETA  &FIELD_IX
.*       AEND
 MNOTE 8'ZSTRMAC ERROR AEND MISSING AIF OR OTHER STRUCTURE
.*
.* GET S2
.*
.*       AENTRY GET_S2
 MNOTE 8'ZSTRMAC ERROR AENTRY DUPLICATE NAME FOUND - GET_S2
&PARM_IX SETA &S2_PARM_IX
.*       AIF (&PARM_IX GT &NP)
           AIF (NOT(&PARM_IX GT &NP)).AIF_349_1
&S2_OK       SETB 0
.*           AEXIT AENTRY
 MNOTE 8'ZSTRMAC ERROR AEXIT NOT WITHIN AENTRY, AWHILE, ACASE
.*       AEND
.AIF_349_1 ANOP
&FIELD_REG SETC 'ZC_R2'               S2 IN R2
&FIELD_SS1 SETB 0
.*       ACALL GET_PARM_FIELD
&ACALL_6_GET_PARM_FIELD SETA    31
           AGO   .ACL_6_GET_PARM_FIELD
.ACL_6_31  ANOP
&S2_PARM_IX SETA &PARM_IX
&S2      SETC '&FIELD_NAME'
&S2_IX   SETA  &FIELD_IX
.*       AEND
 MNOTE 8'ZSTRMAC ERROR AEND MISSING AIF OR OTHER STRUCTURE
.*
.* GET S3
.*
.*       AENTRY GET_S3
 MNOTE 8'ZSTRMAC ERROR AENTRY DUPLICATE NAME FOUND - GET_S3
&PARM_IX SETA &S3_PARM_IX
.*       AIF (&PARM_IX GT &NP)
           AIF (NOT(&PARM_IX GT &NP)).AIF_350_1
&S3_OK       SETB 0
.*           AEXIT AENTRY
 MNOTE 8'ZSTRMAC ERROR AEXIT NOT WITHIN AENTRY, AWHILE, ACASE
.*       AEND
.AIF_350_1 ANOP
&FIELD_REG SETC 'ZC_R3'            S3 IN R3
&FIELD_SS1 SETB 0
.*       ACALL GET_PARM_FIELD
&ACALL_6_GET_PARM_FIELD SETA    32
           AGO   .ACL_6_GET_PARM_FIELD
.ACL_6_32  ANOP
&S3_PARM_IX SETA &PARM_IX
&S3      SETC '&FIELD_NAME'
&S3_IX   SETA  &FIELD_IX
.*       AEND
 MNOTE 8'ZSTRMAC ERROR AEND MISSING AIF OR OTHER STRUCTURE
         COPY ZCGETFLD
         MEND
         MACRO
.*********************************************************************
.* Copyright 2009 Automated Software Tools Corporation               *
.* This source code is part of z390 zCOBOL/assembler/emulator package*
.* The z390 package is distributed under GNU general public license  *
.* Author - Don Higgins                                              *
.* Date   - 08/02/09                                                 *
.*********************************************************************
.* 08/03/09 stub for pending support
.*********************************************************************
         SYMBOLIC
         MNOTE 8,'SYMBOLIC NOT SUPPORTED YET'
         MEXIT
         MEND
         MACRO
.*********************************************************************
.* Copyright 2008 Automated Software Tools Corporation               *
.* This source code is part of z390 zCOBOL/assembler/emulator package*
.* The z390 package is distributed under GNU general public license  *
.* Author - Don Higgins                                              *
.* Date   - 04/16/08                                                 *
.*********************************************************************
.* 02/20/09 stub for pending support
.*********************************************************************
         TERMINATE
         MNOTE 8,'TERMINATE NOT SUPPORTED YET'
         MEXIT
         MEND
         MACRO
.*********************************************************************
.* Copyright 2008 Automated Software Tools Corporation               *
.* This source code is part of z390 zCOBOL/assembler/emulator package*
.* The z390 package is distributed under GNU general public license  *
.* Author - Don Higgins                                              *
.* Date   - 04/16/08                                                 *
.*********************************************************************
.* 11/17/08 stub for pending support
.* 01/20/09 replace with call to INSPECT CONVERTING
.*********************************************************************
         TRANSFORM
         COPY  ZC_WS
&PARM_IX SETA 1
&FIELD_REG SETC 'ZC_R2'  R2=V1 INSPECT FIELD IF SUBSCRIPTED
&FIELD_SS1 SETB 0
.*       ACALL GET_PARM_FIELD
&ACALL_6_GET_PARM_FIELD SETA    33
           AGO   .ACL_6_GET_PARM_FIELD
.ACL_6_33  ANOP
.*       AIF   (&FIELD_IX EQ 0)
           AIF   (NOT(&FIELD_IX EQ 0)).AIF_351_1
               MNOTE 8,'TRANSFORM FIELD NOT FOUND &SYSLIST(1)'
               MEXIT
.*       AEND
.AIF_351_1 ANOP
&V1      SETC '&FIELD_NAME'
&V1_IX   SETA &FIELD_IX
.*       AIF   ('&SYSLIST(&PARM_IX)' EQ 'CHARACTERS'                   X
                AND '&SYSLIST(&PARM_IX+1)' EQ 'FROM')
           AIF   (NOT('&SYSLIST(&PARM_IX)' EQ 'CHARACTERS'             X
                      AND '&SYSLIST(&PARM_IX+1)' EQ 'FROM')).AIF_352_1
&PARM_IX        SETA &PARM_IX+2
.*       AELSE
           AGO   .AIF_352_E
.AIF_352_1 ANOP
               MNOTE 8,'TRANSFORM CHARACTERS FROM '..' TO '..' ERROR'
               MEXIT
.*       AEND
.AIF_352_E ANOP
&FIELD_REG SETC 'ZC_R3'
&FIELD_SS1 SETB 0
.*       ACALL GET_PARM_FIELD
&ACALL_6_GET_PARM_FIELD SETA    34
           AGO   .ACL_6_GET_PARM_FIELD
.ACL_6_34  ANOP
.*       AIF   (&FIELD_IX EQ 0                                         X
               AND '&FIELD_NAME'(1,1) NE '''')
           AIF   (NOT(&FIELD_IX EQ 0                                   X
                     AND '&FIELD_NAME'(1,1) NE '''')).AIF_353_1
               MNOTE 8,'TRANSFORM FROM FIELD NOT FOUND'
               MEXIT
.*       AEND
.AIF_353_1 ANOP
&V2      SETC '&FIELD_NAME'
&V2_IX   SETA &FIELD_IX
.*       AIF   ('&SYSLIST(&PARM_IX)' NE 'TO')
           AIF   (NOT('&SYSLIST(&PARM_IX)' NE 'TO')).AIF_354_1
               MNOTE 8,'TRANSFORM MISSING TO'
               MEXIT
.*       AEND
.AIF_354_1 ANOP
&PARM_IX SETA &PARM_IX+1
&FIELD_REG SETC 'ZC_R14'
&FIELD_SS1 SETB 0
.*       ACALL GET_PARM_FIELD
&ACALL_6_GET_PARM_FIELD SETA    35
           AGO   .ACL_6_GET_PARM_FIELD
.ACL_6_35  ANOP
.*       AIF   (&FIELD_IX EQ 0                                         X
               AND '&FIELD_NAME'(1,1) NE '''')
           AIF   (NOT(&FIELD_IX EQ 0                                   X
                     AND '&FIELD_NAME'(1,1) NE '''')).AIF_355_1
              MNOTE 8,'TRANSFORM MISSING TO FIELD'
              MEXIT
.*       AEND
.AIF_355_1 ANOP
&V3      SETC '&FIELD_NAME'
&V3_IX   SETA &FIELD_IX
         GEN_INSPECT CONVERTING,&V1,&V1_IX,&V2,&V2_IX,&V3,&V3_IX,,,0
         COPY ZCGETFLD
         MEND
         MACRO
.*********************************************************************
.* Copyright 2008 Automated Software Tools Corporation               *
.* This source code is part of z390 zCOBOL/assembler/emulator package*
.* The z390 package is distributed under GNU general public license  *
.* Author - Don Higgins                                              *
.* Date   - 04/16/08                                                 *
.*********************************************************************
.* 11/17/08 stub for pending support
.*********************************************************************
         UNSTRING
         MNOTE 8,'UNSTRING NOT SUPPORTED YET'
         MEXIT
         MEND
         MACRO
.*********************************************************************
.* Copyright 2008 Automated Software Tools Corporation               *
.* This source code is part of z390 zCOBOL/assembler/emulator package*
.* The z390 package is distributed under GNU general public license  *
.* Author - Don Higgins                                              *
.* Date   - 04/16/08                                                 *
.*********************************************************************
.* 02/26/09 RPI 1012 stub for pending support
.*********************************************************************
         ENABLE
         MNOTE 8,'USE NOT SUPPORTED YET'
         MEXIT
         MEND
         MACRO
.*********************************************************************
.* Copyright 2008 Automated Software Tools Corporation               *
.* This source code is part of z390 zCOBOL/assembler/emulator package*
.* The z390 package is distributed under GNU general public license  *
.* Author - Don Higgins                                              *
.* Date   - 04/07/08                                                 *
.*********************************************************************
.* 04/09/08 initial coding for global WHEN
.* 10/06/08 ZSTRMAC
.*********************************************************************
         WHEN  &VAL
         COPY  ZC_WS
.*       AIF   (&IE_LVL LT 1)
           AIF   (NOT(&IE_LVL LT 1)).AIF_356_1
               MNOTE 8,'WHEN MISSING EVALUATE'
               MEXIT
.*       AEND
.AIF_356_1 ANOP
.*       AIF   (&IE_TYPE(&IE_LVL) NE 2)
           AIF   (NOT(&IE_TYPE(&IE_LVL) NE 2)).AIF_357_1
               MNOTE 8,'WHEN MISSING EVALUATE'
               MEXIT
.*       AEND
.AIF_357_1 ANOP
&IE_WHEN(&IE_LVL) SETA &IE_WHEN(&IE_LVL)+1
.*       AIF   (&IE_WHEN(&IE_LVL) GT 1)
           AIF   (NOT(&IE_WHEN(&IE_LVL) GT 1)).AIF_358_1
               GEN_B EVAL&IE_EVAL(&IE_LVL)._END
.*       AEND
.AIF_358_1 ANOP
         GEN_LABEL EVAL&IE_EVAL(&IE_LVL)._WHEN&IE_WHEN(&IE_lVL),WHEN
.*       AIF   ('&VAL' NE 'OTHER')
           AIF   (NOT('&VAL' NE 'OTHER')).AIF_359_1
               GEN_COMP &SYM_NAME(&IE_EVAL_IX(&IE_LVL)),&IE_EVAL_IX(&IEX
               _LVL),&VAL,0
&NEXT_WHEN     SETA  &IE_WHEN(&IE_LVL)+1
               GEN_BC   ZC_NE,EVAL&IE_EVAL(&IE_LVL)._WHEN&NEXT_WHEN
.*       AEND
.AIF_359_1 ANOP
         MEND
         MACRO
.*********************************************************************
.* Copyright 2008 Automated Software Tools Corporation               *
.* This source code is part of z390 zCOBOL/assembler/emulator package*
.* The z390 package is distributed under GNU general public license  *
.* Author - Don Higgins                                              *
.* Date   - 04/07/08                                                 *
.*********************************************************************
.* 04/07/08 initial coding for z390 target
.* 06/02/09 reset global &FILE_IX at start of working-storage
.*          see zcobol\test\TESTFIL1.CBL regression test
.*********************************************************************
         WORKING_STORAGE &SECT
         COPY ZC_WS
&FILE_IX       SETA 0
         MEND
         MACRO
.*********************************************************************
.* Copyright 2008 Automated Software Tools Corporation               *
.* This source code is part of z390 zCOBOL/assembler/emulator package*
.* The z390 package is distributed under GNU general public license  *
.* Author - Don Higgins                                              *
.* Date   - 04/14/08                                                 *
.*********************************************************************
.* 04/14/08 initial coding for global ZCOBOL
.* 04/30/08 change SYM_FILE_IX to SYM_REF
.* 10/06/08 ZSTRMAC
.* 07/09/09 RPI 1065 move FILE_IX from SYM_REF to SYM_FD for groups
.*********************************************************************
         WRITE &RECORD
         COPY  ZC_WS
&NP      SETA  N'&SYSLIST
.*       AIF   (&NP GT 1 AND '&SYSLIST(2)' EQ 'FROM')
           AIF   (NOT(&NP GT 1 AND '&SYSLIST(2)' EQ 'FROM')).AIF_360_1
               MOVE  &SYSLIST(3),TO,&RECORD
.*       AEND
.AIF_360_1 ANOP
         ZC_SYM_FIND &RECORD
.*       AIF   (&SYM_IX EQ 0)
           AIF   (NOT(&SYM_IX EQ 0)).AIF_361_1
               MNOTE 8,'WRITE RECORD NOT FOUND - &RECORD'
               MEXIT
.*       AEND
.AIF_361_1 ANOP
&FILE_IX SETA  &SYM_FD(&SYM_IX)  RPI 1065
.*       AIF   (&FILE_IX EQ 0)
           AIF   (NOT(&FILE_IX EQ 0)).AIF_362_1
               MNOTE 8,'WRITE RECORD NOT A FILE RECORD - &RECORD'
               MEXIT
.*       AEND
.AIF_362_1 ANOP
         GEN_WRITE &RECORD
         MEXIT
         MEND
         MACRO
.*********************************************************************
.* Copyright 2008 Automated Software Tools Corporation               *
.* This source code is part of z390 zCOBOL/assembler/emulator package*
.* The z390 package is distributed under GNU general public license  *
.* Author - Don Higgins                                              *
.* Date   - 04/07/08                                                 *
.*********************************************************************
.* 04/08/08 initial coding to build global WS definition
.* 04/30/08 add 88's, change SYM_FILE_IX to SYM_REF, add SYM_LVL
.* 05/01/08 add PIC_TYPE support for DEFHPXZ based on PIC,VALUE,
.*          USAGE, and OCCURS clauses (REDEFINES pending)
.* 05/18/08 add REDEFINE support
.* 06/10/08 add USAGE IS POINTER support
.* 06/10/08 add INDEXED BY support
.* 10/06/08 ZSTRMAC, GROUP OCCURS, USAGE, DUP SYM IN DIFF LEVELS
.* 12/03/08 add level 91 filename
.* 12/21/08 inc SYM_FILLER_VAL_CNT for label used by CICS GEN_INIT_WS
.* 01/08/09 set SYM_SIT used by ZCGETFLD.CPY subscript parser
.* 02/23/09 RPI 1001 set SYM_LOC for 88 to first V88 value index
.* 02/25/09 RPI 1002 change index data items from type A to I
.* 02/26/09 RPI 1012 add support for VALUE ALL clause, 66 RENAMES,
.* 02/27/09 RPI 1012 support SIGN LEADING/TRAILING SEPARATE CHARACTER
.* 02/27/09 RPI 1012 support single & in text literal (double it)
.* 03/06/09 RPI 1004 add HFP/BFP/DFP/FLOAT-SHORT/LONG/EXTENDED
.* 03/11/09 RPI 1004 add COMP PIC S9(39) type Q 16 byte 128 bit int
.* 03/20/09 RPI 1004 rename explicit 1-9 FLOAT-HEX/BINARY/DECIMAL-N
.* 04/29/09 RPI 1032 issue error if PIC clause has quote or no 9ZX
.* 06/20/09 RPI 1042 add computational
.* 07/09/09 RPI 1065 move FILE_IX from SYM_REF to SYM_FD to supp groups
.* 07/19/09 RPI 1065 add SYNC clause
.* 07/23/09 RPI 1065 add support for P assumed decimal point in PIC
.* 07/25/09 RPI 1065 gen 2 quotes in value for WS_GEN to gen 1 quote
.* 08/02/09 RPI 1065 check for duplicates and allow 01/nn level dups
.* 08/08/09 RPI 1065 make BY in INDEXED BY optional
.* 08/08/09 RPI 1065 support DISPLAY clause (its the default)
.* 08/08/09 RPI 1065 use SCALE_NUM_LIT to scale PIC_VAL for FGHPQZ
.* 08/12/09 RPI 1065 set SYM_FXT to head of duplicate symbol list
.* 08/17/09 RPI 1065 support 88 level VALUES ARE n1 THRU n2
.* 08/18/09 RPI 1065 add clauses JUSTIFIED, LEADING, TRAILING,
.*          ASCENDING, DESCENDING, BLANK WHEN ZERO
.* 10/12/09 RPI 1090 CORRECT COMP-4/COMP-5 TO COMP_4/COMP_5 FOR COMP
.*********************************************************************
.*
.* WS level,name,clauses in any order (PIC,VALUE,USAGE supported)
.* Notes:
.*   1.  If name omitted FILLER is assumed.
.*   2.  For 88 level, must follow WS item, only VALUE or VALUES
.*   3.  Level 77 treated like 01
.*   4.  S9(4) COMP = H, S9(9) COMP = F, S9(18) COMP = G (FL8)
.*   5.  REDEFINE sets &WS_LVL_RDEF flag and &WS_LVL_RDEF_END
.*   6.  USAGE   on group propagated to lower level items
.*   6.  OCCURS on group duplicates calculated length at end of group
.*   7.  VALUE QUOTE(S) assumes APOST(') vs QUOTE(") for now
.*   8.  PIC_TYPE 123=HFP,456=BFP,789=DFP
.*   9.  PIC_TYPE Q COMP PIC S9(3) QUADWORD 16 byte 128 bit integer
.*********************************************************************
         WS    &LEVEL,&NAME
         COPY  ZC_WS
&NEW_LVL_NO SETA  '&LEVEL'   MAY CHANGE FROM 77 TO 01
&NEW_SYM SETC  '&NAME'    MAY CHANGE FROM ?  TO FILLER
&ERR_ID  SETC  '&NEW_LVL_NO &NEW_SYM'
&REF_IX  SETA  0
&PIC_SEP SETB  0 SEPARATE SIGN
&PIC_LEAD SETB  0 LEADING
&PIC_TRAIL SETB  0 TRAILING
.*       AIF   (&NEW_LVL_NO EQ 88)     CONDITION VALUES
           AIF   (NOT(&NEW_LVL_NO EQ 88)).AIF_363_1      CONDITION VALUX
               ES
.*             ACALL WS_88
&ACALL_60_WS_88  SETA    1
                 AGO   .ACL_60_WS_88
.ACL_60_1        ANOP
               MEXIT
.*       AELSEIF (&NEW_LVL_NO EQ 66)   RENAMES
           AGO   .AIF_363_E
.AIF_363_1 ANOP
           AIF     (NOT(&NEW_LVL_NO EQ 66)).AIF_363_2    RENAMES
.*             ACALL WS_66
&ACALL_61_WS_66  SETA    1
                 AGO   .ACL_61_WS_66
.ACL_61_1        ANOP
               MEXIT
.*       AEND
.AIF_363_2 ANOP
.AIF_363_E ANOP
.*       AIF   (&NEW_LVL_NO EQ 91)
           AIF   (NOT(&NEW_LVL_NO EQ 91)).AIF_364_1
&NEW_LVL_NO    SETA 1
&CUR_SET_LVL   SETA 0
.*             ACALL WS_END_PRIOR
&ACALL_62_WS_END_PRIOR SETA    1
                 AGO   .ACL_62_WS_END_PRIOR
.ACL_62_1        ANOP
&NEW_LVL_NO    SETA 91
.*             ACALL WS_91_FILE  ADD FILE DCB
&ACALL_63_WS_91_FILE SETA    1
                 AGO   .ACL_63_WS_91_FILE
.ACL_63_1        ANOP
               MEXIT
.*       AEND
.AIF_364_1 ANOP
.*       AIF   (&NEW_LVL_NO EQ 77)
           AIF   (NOT(&NEW_LVL_NO EQ 77)).AIF_365_1
&NEW_LVL_NO    SETA 1  TREAT 77 LIKE 01
.*       AEND
.AIF_365_1 ANOP
.*       AIF   (&NEW_LVL_NO EQ 1)
           AIF   (NOT(&NEW_LVL_NO EQ 1)).AIF_366_1
&CUR_SET_LVL   SETA 0  RESET CURRENT SUBSCRIPT LVL
.*       AEND
.AIF_366_1 ANOP
.*       ACALL WS_END_PRIOR
&ACALL_62_WS_END_PRIOR SETA    2
           AGO   .ACL_62_WS_END_PRIOR
.ACL_62_2  ANOP
.*
.*       ADD NEW LEVEL # AT SAME OR HIGHER LEVEL
.*
.*       AIF   (&WS_LVL EQ 0)
           AIF   (NOT(&WS_LVL EQ 0)).AIF_367_1
.*             ACALL WS_ADD_LVL
&ACALL_64_WS_ADD_LVL SETA    1
                 AGO   .ACL_64_WS_ADD_LVL
.ACL_64_1        ANOP
.*       AELSEIF (&NEW_LVL_NO GT &WS_LVL_NO(&WS_LVL))
           AGO   .AIF_367_E
.AIF_367_1 ANOP
           AIF     (NOT(&NEW_LVL_NO GT &WS_LVL_NO(&WS_LVL))).AIF_367_2
.*             ACALL WS_ADD_LVL
&ACALL_64_WS_ADD_LVL SETA    2
                 AGO   .ACL_64_WS_ADD_LVL
.ACL_64_2        ANOP
.*       AEND
.AIF_367_2 ANOP
.AIF_367_E ANOP
.*       AIF   (&WS_LVL EQ 1)
           AIF   (NOT(&WS_LVL EQ 1)).AIF_368_1
&WS_LVL_RDEF(1) SETB 0   RESET REDEFINE AT LVL 1
&WS_LVL_USE(1) SETC ''  RESET GROUP USAGE AT LVL 1
.*       AEND
.AIF_368_1 ANOP
.*       ACALL WS_STORE_NN
&ACALL_65_WS_STORE_NN SETA    1
           AGO   .ACL_65_WS_STORE_NN
.ACL_65_1  ANOP
.*       AIF (&FILE_IX GT 0)
           AIF (NOT(&FILE_IX GT 0)).AIF_369_1
.*           AIF (&FILE_RECORD(&FILE_IX) EQ '')
               AIF (NOT(&FILE_RECORD(&FILE_IX) EQ '')).AIF_370_1
&FILE_RECORD(&FILE_IX) SETC '&SYM_NAME(&SYM_TOT)'
.*           AEND
.AIF_370_1     ANOP
.*       AEND
.AIF_369_1 ANOP
.*       AIF   (&WS_LVL EQ 1 AND '&PIC' EQ '')
           AIF   (NOT(&WS_LVL EQ 1 AND '&PIC' EQ '')).AIF_371_1
&WS_LVL_USE(1) SETC '&USAGE' SET NEW LVL 1 USAGE DEF
.*       AEND
.AIF_371_1 ANOP
&WS_LVL_SYM_IX(&WS_LVL) SETA &SYM_TOT   WS_LVL SYM INDEX
         MEXIT
.*
.* END PRIOR
.*
.*       AENTRY WS_END_PRIOR
           AGO   .ACL_62_SKIP
.ACL_62_WS_END_PRIOR ANOP
&WS_LOC_SAVE SETA &WS_LOC
.*       AIF  (&WS_LVL GT 0)
           AIF  (NOT(&WS_LVL GT 0)).AIF_372_1
.*
.*            END HIGHER LEVEL # GROUPS/REDEF ITEMS
.*
.*            ACALL WS_END_NN
&ACALL_66_WS_END_NN SETA    1
                AGO   .ACL_66_WS_END_NN
.ACL_66_1       ANOP
.*
.*            END REDEFINE AT SAME LEVEL # IF ANY
.*
.*            AIF   (&WS_LVL_RDEF(&WS_LVL)                             X
                    AND &WS_LOC GE &WS_LVL_RDEF_END(&WS_LVL))
                AIF   (NOT(&WS_LVL_RDEF(&WS_LVL)                       X
                          AND &WS_LOC GE &WS_LVL_RDEF_END(&WS_LVL))).AIX
               F_373_1
&WS_LVL_RDEF(&WS_LVL) SETB 0
.*            AEND
.AIF_373_1      ANOP
.*       AEND
.AIF_372_1 ANOP
.*       AEND
           AGO   (&ACALL_62_WS_END_PRIOR).ACL_62_1,.ACL_62_2
.ACL_62_SKIP ANOP
.*
.* END ALL HIGHER LEVEL WS GROUP AND REDEFINE ITEMS
.*
.*       AENTRY WS_END_NN
           AGO   .ACL_66_SKIP
.ACL_66_WS_END_NN ANOP
.*       AWHILE (&WS_LVL GT 1 AND &NEW_LVL_NO LT &WS_LVL_NO(&WS_LVL))
.AWH_53_T  ANOP
           AIF    (NOT(&WS_LVL GT 1 AND &NEW_LVL_NO LT &WS_LVL_NO(&WS_LX
               VL))).AWH_53_E
.*            ACALL WS_END_GROUP
&ACALL_67_WS_END_GROUP SETA    1
                AGO   .ACL_67_WS_END_GROUP
.ACL_67_1       ANOP
.*            AIF (&WS_LVL_RDEF(&WS_LVL))
                AIF (NOT(&WS_LVL_RDEF(&WS_LVL))).AIF_374_1
.*                ACALL WS_END_REDEFINE
&ACALL_68_WS_END_REDEFINE SETA    1
                    AGO   .ACL_68_WS_END_REDEFINE
.ACL_68_1           ANOP
.*            AEND
.AIF_374_1      ANOP
&WS_LVL       SETA &WS_LVL-1
.*       AEND
           AGO   .AWH_53_T
.AWH_53_E  ANOP
.*       AIF   (&WS_LVL_SYM_IX(&WS_LVL) GT 0)
           AIF   (NOT(&WS_LVL_SYM_IX(&WS_LVL) GT 0)).AIF_375_1
.*             ACALL WS_END_GROUP
&ACALL_67_WS_END_GROUP SETA    2
                 AGO   .ACL_67_WS_END_GROUP
.ACL_67_2        ANOP
.*       AEND
.AIF_375_1 ANOP
.*       AIF   (&WS_LVL_RDEF(&WS_LVL)                                  X
                AND &WS_LOC GE &WS_LVL_RDEF_END(&WS_LVL))
           AIF   (NOT(&WS_LVL_RDEF(&WS_LVL)                            X
                      AND &WS_LOC GE &WS_LVL_RDEF_END(&WS_LVL))).AIF_37X
               6_1
&WS_LVL_RDEF(&WS_LVL) SETB 0  END REDEFINE
.*       AEND
.AIF_376_1 ANOP
.*       AEND
           AGO   (&ACALL_66_WS_END_NN).ACL_66_1
.ACL_66_SKIP ANOP
.*
.* WS END GROUP - SET SYM_LEN AND WS_LOC IF GROUP OCCURS > 1
.*
.*       AENTRY WS_END_GROUP
           AGO   .ACL_67_SKIP
.ACL_67_WS_END_GROUP ANOP
&IX      SETA  &WS_LVL_SYM_IX(&WS_LVL)
.*     AIF  (&IX GT 0)
         AIF  (NOT(&IX GT 0)).AIF_377_1
.*       AIF (&SYM_LEN(&IX) EQ 0)                     IS THIS GROUP LVL
           AIF (NOT(&SYM_LEN(&IX) EQ 0)).AIF_378_1                     X
                IS THIS GROUP LVL
&LEN         SETA  &WS_LOC-&SYM_LOC(&IX)    CALC GROUP LENGTH
.*           AIF (&LEN GT 0)
               AIF (NOT(&LEN GT 0)).AIF_379_1
&SYM_LEN(&IX)    SETA &LEN
&WS_PAD          SETA &SYM_LOC(&IX)+&SYM_DUP(&IX)*&LEN-&WS_LOC
.*               AIF (&WS_PAD GT 0)
                   AIF (NOT(&WS_PAD GT 0)).AIF_380_1
&WS_LOC              SETA &WS_LOC+&WS_PAD
.*               AEND
.AIF_380_1         ANOP
.*               AIF (&IX GT 1)
                   AIF (NOT(&IX GT 1)).AIF_381_1
.*                   AIF (&SYM_LEN(&IX-1) EQ 0) IS PRIOR WS ITEM 0 LEN
                       AIF (NOT(&SYM_LEN(&IX-1) EQ 0)).AIF_382_1  IS PRX
               IOR WS ITEM 0 LEN
.*                       AIF (&SYM_LVL(&IX-1) EQ &SYM_LVL(&IX))
                           AIF (NOT(&SYM_LVL(&IX-1) EQ &SYM_LVL(&IX))).X
               AIF_383_1
&SYM_LEN(&IX-1)              SETA &LEN      FIX 0 LEN
.*                       AEND
.AIF_383_1                 ANOP
.*                   AEND
.AIF_382_1             ANOP
.*               AEND
.AIF_381_1         ANOP
.*           AEND
.AIF_379_1     ANOP
.*       AEND
.AIF_378_1 ANOP
.*     AEND
.AIF_377_1 ANOP
.*       AEND
           AGO   (&ACALL_67_WS_END_GROUP).ACL_67_1,.ACL_67_2
.ACL_67_SKIP ANOP
.*
.* STORE NEW 01-49 LEVEL WS ITEM AT CURRENT WS_LVL LEVEL
.*
.*       AENTRY WS_STORE_NN
           AGO   .ACL_65_SKIP
.ACL_65_WS_STORE_NN ANOP
&SYM_TOT SETA  &SYM_TOT+1
&SYM_NAME(&SYM_TOT) SETC '&NEW_SYM' ASSUME NAME VS CLAUSE
.*       AIF  (&WS_LVL GT 1)
           AIF  (NOT(&WS_LVL GT 1)).AIF_384_1
&QIX          SETA &WS_LVL_SYM_IX(&WS_LVL-1)
.*       AELSE
           AGO   .AIF_384_E
.AIF_384_1 ANOP
&QIX          SETA 0
.*       AEND
.AIF_384_E ANOP
.*       AIF  (&LK_MODE)
           AIF  (NOT(&LK_MODE)).AIF_385_1
.*            AIF  (&NEW_LVL_NO EQ 1)
                AIF  (NOT(&NEW_LVL_NO EQ 1)).AIF_386_1
&LK_TOT            SETA &LK_TOT+1
&LAST_LK_DSECT_IX  SETA &SYM_TOT
&WS_LOC            SETA 0
.*            AEND
.AIF_386_1      ANOP
&SYM_LK(&SYM_TOT) SETA &LAST_LK_DSECT_IX
.*       AEND
.AIF_385_1 ANOP
&SYM_LVL(&SYM_TOT) SETA &NEW_LVL_NO     SYMBOL LEVEL
&SYM_REF(&SYM_TOT) SETA &REF_IX SYM 88,RDEF, OR LK VAR IX
&SYM_FD(&SYM_TOT) SETA &FILE_IX SYM FD INDEX
&SYM_LOC(&SYM_TOT) SETA &WS_LOC  SYM FIELD WS OFFSET
&SYM_LEN(&SYM_TOT) SETA 0        SYM FIELD LENGTH DEFAULT
&SYM_DUP(&SYM_TOT) SETA 1        SYM FIELD OCCURANCES
&SYM_PIC_TYPE(&SYM_TOT) SETC 'X'  SYM PIC TYPE DEFAULT DISP
.*       AIF   (&NEW_LVL_NO GT 49)
           AIF   (NOT(&NEW_LVL_NO GT 49)).AIF_387_1
               MNOTE 8,'WS &ERR_ID INVALID LVL'
               MEXIT
.*       AEND
.AIF_387_1 ANOP
&SYM_IX_LST SETA &SYM_TOT     SAVE WS ITEM BEING ADDED
.*       ACALL WS_SCAN_CLAUSES          DEFINE ALL CLAUSES IN ANY ORDER
&ACALL_69_WS_SCAN_CLAUSES SETA    1
           AGO   .ACL_69_WS_SCAN_CLAUSES
.ACL_69_1  ANOP
.*       ACALL WS_PROC_CLAUSES          PROCESS ALL CLAUSES FOR WS ITEM
&ACALL_70_WS_PROC_CLAUSES SETA    1
           AGO   .ACL_70_WS_PROC_CLAUSES
.ACL_70_1  ANOP
.*       AIF  (&ZC_CICS                                                X
               AND '&NEW_SYM' EQ 'FILLER'                              X
               AND '&SYM_VAL(&SYM_TOT)' NE '') GEN FILLER NAME FOR VAL
           AIF  (NOT(&ZC_CICS                                          X
                     AND '&NEW_SYM' EQ 'FILLER'                        X
                     AND '&SYM_VAL(&SYM_TOT)' NE '')).AIF_388_1  GEN FIX
               LLER NAME FOR VAL
&SYM_FILLER_VAL_CNT SETA &SYM_FILLER_VAL_CNT+1
&NEW_SYM       SETC 'ZC_FILLER_V&SYM_FILLER_VAL_CNT'
&SYM_NAME(&SYM_TOT) SETC '&NEW_SYM'
&QIX           SETA &SYM_QIX(&SYM_TOT)
.*       AEND
.AIF_388_1 ANOP
&SYM_QIX(&SYM_TOT) SETA &QIX SET QUALFIER IX OR 0
.*       AIF  ('&NEW_SYM' NE 'FILLER')
           AIF  (NOT('&NEW_SYM' NE 'FILLER')).AIF_389_1
              ZC_SYM_INDEX &NEW_SYM,&QIX   ADD QUALIFIED NAME
.*       AEND
.AIF_389_1 ANOP
.*       AIF   (&SYM_DUP(&SYM_TOT) GT 1)
           AIF   (NOT(&SYM_DUP(&SYM_TOT) GT 1)).AIF_390_1
&SYM_SIT(&SYM_TOT) SETA 1
.*       AEND
.AIF_390_1 ANOP
&CUR_QIX SETA &SYM_QIX(&SYM_TOT)
.*       AWHILE (&CUR_QIX GT 0)
.AWH_54_T  ANOP
           AIF    (NOT(&CUR_QIX GT 0)).AWH_54_E
.*             AIF (&SYM_DUP(&CUR_QIX) GT 1)
                 AIF (NOT(&SYM_DUP(&CUR_QIX) GT 1)).AIF_391_1
&SYM_SIT(&SYM_TOT) SETA &SYM_SIT(&SYM_TOT)+1
.*             AEND
.AIF_391_1       ANOP
&CUR_QIX       SETA &SYM_QIX(&CUR_QIX)
.*       AEND
           AGO   .AWH_54_T
.AWH_54_E  ANOP
.*       AIF   (&SYM_TOT GT 1)
           AIF   (NOT(&SYM_TOT GT 1)).AIF_392_1
.*             AIF (&SYM_LVL(&SYM_TOT-1) LT &SYM_LVL(&SYM_TOT)         X
                    AND '&SYM_PIC(&SYM_TOT-1)' NE '')
                 AIF (NOT(&SYM_LVL(&SYM_TOT-1) LT &SYM_LVL(&SYM_TOT)   X
                          AND '&SYM_PIC(&SYM_TOT-1)' NE '')).AIF_393_1
                   MNOTE 8,'WS ITEM LEVEL > PREV ELEMENTARY ITEM LEVEL'
.*             AEND
.AIF_393_1       ANOP
.*       AEND
.AIF_392_1 ANOP
.*       AEND
           AGO   (&ACALL_65_WS_STORE_NN).ACL_65_1
.ACL_65_SKIP ANOP
.*
.* END REDEFINE ADDING FILLER IF NEEDED AND RESET WS_LOC TO END
.*
.*       AENTRY WS_END_REDEFINE
           AGO   .ACL_68_SKIP
.ACL_68_WS_END_REDEFINE ANOP
.*       AIF   (&WS_LVL_RDEF_END(&WS_LVL) LT &WS_LOC)
           AIF   (NOT(&WS_LVL_RDEF_END(&WS_LVL) LT &WS_LOC)).AIF_394_1
&DIFF          SETA &WS_LOC-&WS_LVL_RDEF_END(&WS_LVL)
               MNOTE 8,'WS &ERR_ID REDEFINE EXCEEDS LENGTH BY &DIFF'
.*       AEND
.AIF_394_1 ANOP
.*       AIF   (&WS_LVL_RDEF_END(&WS_LVL) GT &WS_LOC)
           AIF   (NOT(&WS_LVL_RDEF_END(&WS_LVL) GT &WS_LOC)).AIF_395_1
&WS_PAD        SETA &WS_LVL_RDEF_END(&WS_LVL)-&WS_LOC
.*             ACALL GEN_WS_FILLER
&ACALL_71_GEN_WS_FILLER SETA    1
                 AGO   .ACL_71_GEN_WS_FILLER
.ACL_71_1        ANOP
.*       AEND
.AIF_395_1 ANOP
&WS_LOC  SETA &WS_LVL_RDEF_END(&WS_LVL)
&WS_LVL_RDEF(&WS_LVL) SETB 0
.*       AEND
           AGO   (&ACALL_68_WS_END_REDEFINE).ACL_68_1
.ACL_68_SKIP ANOP
.*
.* GEN WS FILLER TO PAD REDEFINE GAP = &WS_PAD
.*
.*       AENTRY GEN_WS_FILLER
           AGO   .ACL_71_SKIP
.ACL_71_GEN_WS_FILLER ANOP
&SYM_TOT SETA  &SYM_TOT+1
&SYM_NAME(&SYM_TOT) SETC 'FILLER' PAD REDEFINE
&SYM_LVL(&SYM_TOT) SETA &NEW_LVL_NO     SYMBOL LVL
&SYM_REF(&SYM_TOT) SETA &REF_IX  SYM FILE,88,REDF,LK-V
&SYM_FD(&SYM_TOT) SETA &FILE_IX SYM FD INDEX
&SYM_LOC(&SYM_TOT) SETA &WS_LOC  SYM FIELD WS OFFSET
&SYM_LEN(&SYM_TOT) SETA &WS_PAD
&SYM_DUP(&SYM_TOT) SETA 1        SYM FIELD OCCURANCES
&SYM_PIC_TYPE(&SYM_TOT) SETC 'X'  SYM PIC TYPE DEF DSP
.*       AEND
           AGO   (&ACALL_71_GEN_WS_FILLER).ACL_71_1
.ACL_71_SKIP ANOP
.*
.* STORE SINGLE OR MULTIPLE 88 CONDITION VALUES FOR PRIOR WS ITEM
.*
.*       AENTRY WS_88
           AGO   .ACL_60_SKIP
.ACL_60_WS_88 ANOP
&SYM_TOT SETA  &SYM_TOT+1
&SYM_NAME(&SYM_TOT) SETC '&NEW_SYM'
&SYM_QIX(&SYM_TOT) SETA &SYM_IX_LST  SET LEVEL RPI 1065
         ZC_SYM_INDEX &NEW_SYM
&SYM_LVL(&SYM_TOT) SETA &NEW_LVL_NO SYMBOL LVL
.*       AIF   (&SYM_IX_LST EQ 0)
           AIF   (NOT(&SYM_IX_LST EQ 0)).AIF_396_1
               MNOTE 8,'WS &ERR_ID  MISSING PREVIOUS ITEM'
               MEXIT
.*       AEND
.AIF_396_1 ANOP
.*       AIF   ('&SYM_NAME(&SYM_IX_LST)' EQ 'FILLER') MAKE UNIQUE NAME
           AIF   (NOT('&SYM_NAME(&SYM_IX_LST)' EQ 'FILLER')).AIF_397_1 X
                MAKE UNIQUE NAME
&SYM_FILLER_VAL_CNT SETA &SYM_FILLER_VAL_CNT+1 RPI 1065
&SYM_NAME(&SYM_IX_LST) SETC 'ZC_FILLER_V&SYM_FILLER_VAL_CNT'
.*       AEND
.AIF_397_1 ANOP
&SYM_REF(&SYM_TOT) SETA &SYM_IX_LST  POINT TO LAST WS ITEM
&SYM_LOC(&SYM_TOT) SETA &V88_TOT+1 PTR TO FIRST 88 VALUE
.*       AIF   ('&SYSLIST(3)' EQ 'VALUE')
           AIF   (NOT('&SYSLIST(3)' EQ 'VALUE')).AIF_398_1
&I             SETA  4
.*             AIF   ('&SYSLIST(&I)' EQ 'IS')
                 AIF   (NOT('&SYSLIST(&I)' EQ 'IS')).AIF_399_1
&I                   SETA &I+1
.*             AELSEIF ('&SYSLIST(&I)' EQ 'ALL')   RPI 1012
                 AGO   .AIF_399_E
.AIF_399_1       ANOP
                 AIF     (NOT('&SYSLIST(&I)' EQ 'ALL')).AIF_399_2    RPX
               I 1012
&I                   SETA &I+1
&SYM_VAL_ALL(&SYM_TOT) SETB 1 TURN ON ALL FLAG
.*             AEND
.AIF_399_2       ANOP
.AIF_399_E       ANOP
&V88_TOT       SETA &V88_TOT+1
&V88_VAL(&V88_TOT) SETC '&SYSLIST(&I)'
&I             SETA &I+1
.*             AIF  ('&V88_VAL(&V88_TOT)' EQ 'X')
                 AIF  (NOT('&V88_VAL(&V88_TOT)' EQ 'X')).AIF_400_1
&V88_VAL(&V88_TOT)  SETC 'X&SYSLIST(&I)'
&I                  SETA &I+1
.*             AEND
.AIF_400_1       ANOP
.*             AIF  ('&V88_VAL(&V88_TOT)' EQ 'X')
                 AIF  (NOT('&V88_VAL(&V88_TOT)' EQ 'X')).AIF_401_1
&V88_VAL(&V88_TOT)  SETC 'X&SYSLIST(&I)'
&I                  SETA &I+1
.*             AELSEIF  ('&V88_VAL(&V88_TOT)' EQ 'QUOTE'               X
                         OR '&V88_VAL(&V88_TOT)' EQ 'QUOTES')
                 AGO   .AIF_401_E
.AIF_401_1       ANOP
                 AIF      (NOT('&V88_VAL(&V88_TOT)' EQ 'QUOTE'         X
                               OR '&V88_VAL(&V88_TOT)' EQ 'QUOTES')).AIX
               F_401_2
&V88_VAL(&V88_TOT)  SETC ''''''''''  RPI 1065 'QQ'
.*             AEND
.AIF_401_2       ANOP
.AIF_401_E       ANOP
.*       AELSEIF ('&SYSLIST(3)' EQ 'VALUES')
           AGO   .AIF_398_E
.AIF_398_1 ANOP
           AIF     (NOT('&SYSLIST(3)' EQ 'VALUES')).AIF_398_2
&NP            SETA  N'&SYSLIST
&I             SETA  4
.*             AIF   ('&SYSLIST(4)' EQ 'ARE')
                 AIF   (NOT('&SYSLIST(4)' EQ 'ARE')).AIF_402_1
&I                   SETA  5
.*             AEND
.AIF_402_1       ANOP
.*             AWHILE (&I LE &NP)
.AWH_55_T        ANOP
                 AIF    (NOT(&I LE &NP)).AWH_55_E
.*               AIF  ('&SYSLIST(&I+1)' EQ 'THRU'                      X
                     OR '&SYSLIST(&I+1)' EQ 'THROUGH')
                   AIF  (NOT('&SYSLIST(&I+1)' EQ 'THRU'                X
                           OR '&SYSLIST(&I+1)' EQ 'THROUGH')).AIF_403_1
&V1                 SETA &SYSLIST(&I)
&V2                 SETA &SYSLIST(&I+2)
.*                  AIF (&V1 GT &V2)
                      AIF (NOT(&V1 GT &V2)).AIF_404_1
                        MNOTE 8,'WS 88 VALUE RANGE ERROR'
                        MEXIT
.*                  AEND
.AIF_404_1            ANOP
.*                  AWHILE (&V1 LE &V2)
.AWH_56_T             ANOP
                      AIF    (NOT(&V1 LE &V2)).AWH_56_E
&V88_TOT               SETA &V88_TOT+1
&V88_VAL(&V88_TOT)     SETC '&V1'
&V88_NEXT(&V88_TOT)    SETA &V88_TOT+1
&V1                    SETA &V1+1
.*                  AEND
                      AGO   .AWH_56_T
.AWH_56_E             ANOP
&I                  SETA &I+3
.*               AELSE
                   AGO   .AIF_403_E
.AIF_403_1         ANOP
&V88_TOT            SETA &V88_TOT+1
&V88_VAL(&V88_TOT)  SETC '&SYSLIST(&I)'
.*                  AIF  ('&V88_VAL(&V88_TOT)' EQ 'X')
                      AIF  (NOT('&V88_VAL(&V88_TOT)' EQ 'X')).AIF_405_1
&V88_VAL(&V88_TOT)      SETC 'X&SYSLIST(&I)'
&I                      SETA &I+1
.*                  AELSEIF  ('&V88_VAL(&V88_TOT)' EQ 'QUOTE'          X
                              OR '&V88_VAL(&V88_TOT)' EQ 'QUOTES')
                      AGO   .AIF_405_E
.AIF_405_1            ANOP
                      AIF      (NOT('&V88_VAL(&V88_TOT)' EQ 'QUOTE'    X
                                    OR '&V88_VAL(&V88_TOT)' EQ 'QUOTES'X
               )).AIF_405_2
&V88_VAL(&V88_TOT)      SETC ''''''''''  1065 'QQ'
.*                  AEND
.AIF_405_2            ANOP
.AIF_405_E            ANOP
&V88_NEXT(&V88_TOT) SETA &V88_TOT+1
&I                  SETA   &I+1
.*               AEND
.AIF_403_E         ANOP
.*             AEND
                 AGO   .AWH_55_T
.AWH_55_E        ANOP
&V88_NEXT(&V88_TOT) SETA 0
.*       AELSE
           AGO   .AIF_398_E
.AIF_398_2 ANOP
               MNOTE 8,'WS &ERR_ID INVALID LVL 88 VALUE &PIC_VAL'
.*       AEND
.AIF_398_E ANOP
.*       AEND
           AGO   (&ACALL_60_WS_88).ACL_60_1
.ACL_60_SKIP ANOP
.*
.* 66 name1 RENAMES name2 [THRU/THROUGH name3]  RPI 1012
.*
.*       AENTRY WS_66
           AGO   .ACL_61_SKIP
.ACL_61_WS_66 ANOP
&SYM_TOT SETA  &SYM_TOT+1
&SYM_NAME(&SYM_TOT) SETC '&NEW_SYM'
&SYM_QIX(&SYM_TOT) SETA &WS_LVL_SYM_IX(1)
&SYM_LVL(&SYM_TOT) SETA 2 REDEFINE 66 AS 02
         ZC_SYM_INDEX &NEW_SYM
.*       AIF   ('&SYSLIST(3)' NE 'RENAMES')
           AIF   (NOT('&SYSLIST(3)' NE 'RENAMES')).AIF_406_1
               MNOTE 8,'WS 66 MISSING RENAMES'
               MEXIT
.*       AEND
.AIF_406_1 ANOP
&PARM_IX SETA 4
.*       ACALL GET_PARM_FIELD
&ACALL_6_GET_PARM_FIELD SETA    36
           AGO   .ACL_6_GET_PARM_FIELD
.ACL_6_36  ANOP
.*       AIF   (&FIELD_IX EQ 0)
           AIF   (NOT(&FIELD_IX EQ 0)).AIF_407_1
               MNOTE 'WS RENAMED FIELD NOT FOUND - &SYSLIST(4)'
               MEXIT
.*       AEND
.AIF_407_1 ANOP
&NAME2_IX SETA &FIELD_IX
&SYM_LOC(&SYM_TOT) SETA &SYM_LOC(&NAME2_IX)
.*       AIF   (N'&SYSLIST LT &PARM_IX)  REDEFINE SINGLE ITEM
           AIF   (NOT(N'&SYSLIST LT &PARM_IX)).AIF_408_1   REDEFINE SINX
               GLE ITEM
&SYM_LEN(&SYM_TOT) SETA &SYM_LEN(&NAME2_IX)
&SYM_PIC_TYPE(&SYM_TOT) SETC 'X'
.*             AEXIT AENTRY
                 AGO   .ACL_61_E
.*       AEND
.AIF_408_1 ANOP
.*       AIF   ('&SYSLIST(&PARM_IX)' NE 'THRU'                         X
               AND '&SYSLIST(&PARM_IX)' NE 'THROUGH')
           AIF   (NOT('&SYSLIST(&PARM_IX)' NE 'THRU'                   X
                     AND '&SYSLIST(&PARM_IX)' NE 'THROUGH')).AIF_409_1
               MNOTE 8,'WS RENAME UNRECOGNIZED CLAUSE - &SYSLIST(&PARM_X
               IX)'
               MEXIT
.*       AEND
.AIF_409_1 ANOP
&PARM_IX SETA &PARM_IX+1
.*       ACALL GET_PARM_FIELD
&ACALL_6_GET_PARM_FIELD SETA    37
           AGO   .ACL_6_GET_PARM_FIELD
.ACL_6_37  ANOP
.*       AIF   (&FIELD_IX EQ 0)
           AIF   (NOT(&FIELD_IX EQ 0)).AIF_410_1
               MNOTE 'WS RENAMED FIELD NOT FOUND - &SYSLIST(&PARM_IX)'
               MEXIT
.*       AEND
.AIF_410_1 ANOP
&NAME3_IX SETA &FIELD_IX
&SYM_LEN(&SYM_TOT) SETA &SYM_LOC(&NAME3_IX)+&SYM_LEN(&NAME3_IX)-&SYM_LOX
               C(&NAME2_IX)
&SYM_PIC_TYPE(&SYM_TOT) SETC 'X'
.*       AEND
.ACL_61_E  ANOP
           AGO   (&ACALL_61_WS_66).ACL_61_1
.ACL_61_SKIP ANOP
.*
.* ADD FILE DCB
.*
.*       AENTRY WS_91_FILE
           AGO   .ACL_63_SKIP
.ACL_63_WS_91_FILE ANOP
&SYM_TOT SETA  &SYM_TOT+1
&SYM_NAME(&SYM_TOT) SETC '&NEW_SYM'
         ZC_SYM_INDEX &NEW_SYM
&SYM_LVL(&SYM_TOT) SETA &NEW_LVL_NO SYMBOL LVL
&SYM_FD(&SYM_TOT) SETA &FILE_IX  POINT TO FILE FOUND IN FD
&WS_LOC  SETA &WS_LOC+(&WS_LOC+7)/8*8-&WS_LOC
&SYM_LOC(&SYM_TOT) SETA &WS_LOC
&SYM_LEN(&SYM_TOT) SETA &ZC_DCB_LEN
&SYM_DUP(&SYM_TOT) SETA 1
&SYM_PIC_TYPE(&SYM_TOT) SETC 'X'
&WS_LOC  SETA &WS_LOC+&ZC_DCB_LEN
.*       AEND
           AGO   (&ACALL_63_WS_91_FILE).ACL_63_1
.ACL_63_SKIP ANOP
.*
.* SCAN FOR CLAUSES AND SET NAME TO FILLER IF NO NAME FIELD
.*
.*  INDEXED BY- SET  INDEX TO INDEX NAME FOR OCCURS FIELD
.*  OCCURS    - SET  DUP
.*  PIC       - SETS PIC
.*  REDEFINES - SAVE CUR WS_LOC, RESET WS_LOC, TURN ON RDEF FLAG
.*  SIGN      - SETS PIC_LEAD IF SIGN IS LEADING SEPARATE CHARACTER
.*  USAGE     - SETS USAGE
.*  VALUE     - SETS PIC_VAL
.*
.*       AENTRY WS_SCAN_CLAUSES
           AGO   .ACL_69_SKIP
.ACL_69_WS_SCAN_CLAUSES ANOP
.*       AIF   (&WS_LVL GT 1)
           AIF   (NOT(&WS_LVL GT 1)).AIF_411_1
&USAGE         SETC '&WS_LVL_USE(&WS_LVL-1)'
.*       AELSE
           AGO   .AIF_411_E
.AIF_411_1 ANOP
&USAGE         SETC ''
.*       AEND
.AIF_411_E ANOP
&INDEX   SETC ''
&PIC     SETC  ''
&PIC_TYPE SETC 'X'  DEFAULT DISPLAY X (INCLUDES A AND Z)
&PIC_LEN SETA  0   EXPLICIT LENGTH IF ANY
&PIC_SIGN SETB  0   DEFAULT UNSIGNED
&PIC_LEAD SETB  0   DEFAULT SIGN NOT LEADING
&PIC_VAL SETC  ''  INITIAL VALUE IF ANY
&PIC_VAL_ALL SETB 0 REPEAT VALUE           RPI 1012
&DUP     SETA  1    DEFAULT OCCURANCES
&NP      SETA  N'&SYSLIST
&I       SETA  2
.*       AWHILE (&I LE &NP)
.AWH_57_T  ANOP
           AIF    (NOT(&I LE &NP)).AWH_57_E
.*             AIF ('&SYSLIST(&I)' EQ 'PIC'                            X
                    OR '&SYSLIST(&I)' EQ 'PICTURE')
                 AIF (NOT('&SYSLIST(&I)' EQ 'PIC'                      X
                          OR '&SYSLIST(&I)' EQ 'PICTURE')).AIF_412_1
.*                  AIF  (&I EQ 2)
                      AIF  (NOT(&I EQ 2)).AIF_413_1
&NEW_SYM                 SETC 'FILLER'
&SYM_NAME(&SYM_TOT)      SETC 'FILLER'
.*                  AEND
.AIF_413_1            ANOP
&I                  SETA  &I+1
.*                  AIF   ('&SYSLIST(&I)' EQ 'IS')
                      AIF   (NOT('&SYSLIST(&I)' EQ 'IS')).AIF_414_1
&I                        SETA  &I+1
.*                  AEND
.AIF_414_1            ANOP
.*                  ACALL SET_PIC_AND_LEN
&ACALL_72_SET_PIC_AND_LEN SETA    1
                      AGO   .ACL_72_SET_PIC_AND_LEN
.ACL_72_1             ANOP
&I                  SETA  &I+1
.*             AELSEIF   ('&SYSLIST(&I)' EQ 'VALUE')
                 AGO   .AIF_412_E
.AIF_412_1       ANOP
                 AIF       (NOT('&SYSLIST(&I)' EQ 'VALUE')).AIF_412_2
.*                  AIF  (&I EQ 2)
                      AIF  (NOT(&I EQ 2)).AIF_415_1
&NEW_SYM                 SETC 'FILLER'
&SYM_NAME(&SYM_TOT)      SETC 'FILLER'
.*                  AEND
.AIF_415_1            ANOP
&I                  SETA  &I+1
.*                  AIF   ('&SYSLIST(&I)' EQ 'IS')
                      AIF   (NOT('&SYSLIST(&I)' EQ 'IS')).AIF_416_1
&I                       SETA  &I+1
.*                  AELSEIF ('&SYSLIST(&I)' EQ 'ALL')   RPI 1012
                      AGO   .AIF_416_E
.AIF_416_1            ANOP
                      AIF     (NOT('&SYSLIST(&I)' EQ 'ALL')).AIF_416_2 X
                  RPI 1012
&I                   SETA &I+1
&PIC_VAL_ALL         SETB 1
.*                  AEND
.AIF_416_2            ANOP
.AIF_416_E            ANOP
.*                  AIF  (&WS_LVL NE 0)
                      AIF  (NOT(&WS_LVL NE 0)).AIF_417_1
.*                       AIF (&WS_LVL_RDEF(&WS_LVL) AND &ZC_WARN)
                           AIF (NOT(&WS_LVL_RDEF(&WS_LVL) AND &ZC_WARN)X
               ).AIF_418_1
                             MNOTE 4,'WS &ERR_ID REDEFINE VALUE IGNOREDX
               '
.*                       AELSE
                           AGO   .AIF_418_E
.AIF_418_1                 ANOP
&PIC_VAL                     SETC '&SYSLIST(&I)'
.*                       AEND
.AIF_418_E                 ANOP
.*                  AELSE
                      AGO   .AIF_417_E
.AIF_417_1            ANOP
&PIC_VAL                 SETC '&SYSLIST(&I)'
.*                  AEND
.AIF_417_E            ANOP
&I                  SETA  &I+1
.*                  AIF  ('&PIC_VAL' EQ 'ZERO'                         X
                          OR '&PIC_VAL' EQ 'ZEROS'                     X
                          OR '&PIC_VAL' EQ 'ZEROES')
                      AIF  (NOT('&PIC_VAL' EQ 'ZERO'                   X
                                OR '&PIC_VAL' EQ 'ZEROS'               X
                                OR '&PIC_VAL' EQ 'ZEROES')).AIF_419_1
&PIC_VAL                 SETC '0'
.*                  AELSEIF  ('&PIC_VAL' EQ 'X')
                      AGO   .AIF_419_E
.AIF_419_1            ANOP
                      AIF      (NOT('&PIC_VAL' EQ 'X')).AIF_419_2
&PIC_VAL                 SETC 'X&SYSLIST(&I)'
&I                       SETA &I+1
.*                  AELSEIF ('&PIC_VAL' EQ '+' OR '&PIC_VAL' EQ '-')
                      AGO   .AIF_419_E
.AIF_419_2            ANOP
                      AIF     (NOT('&PIC_VAL' EQ '+' OR '&PIC_VAL' EQ 'X
               -')).AIF_419_3
&PIC_VAL                 SETC '&PIC_VAL&SYSLIST(&I)'
&I                       SETA &I+1
.*                  AELSEIF ('&PIC_VAL' EQ '''&&''')
                      AGO   .AIF_419_E
.AIF_419_3            ANOP
                      AIF     (NOT('&PIC_VAL' EQ '''&&''')).AIF_419_4
&PIC_VAL                 SETC '''&&&&''' REQ'D FOR DC
.*                  AELSEIF  ('&PIC_VAL' EQ 'QUOTE'                    X
                              OR '&PIC_VAL' EQ 'QUOTES')
                      AGO   .AIF_419_E
.AIF_419_4            ANOP
                      AIF      (NOT('&PIC_VAL' EQ 'QUOTE'              X
                                    OR '&PIC_VAL' EQ 'QUOTES')).AIF_419X
               _5
&PIC_VAL                 SETC ''''''''''  RPI 1065 'QQ'
.*                  AEND
.AIF_419_5            ANOP
.AIF_419_E            ANOP
.*             AELSEIF   ('&SYSLIST(&I)' EQ 'OCCURS')
                 AGO   .AIF_412_E
.AIF_412_2       ANOP
                 AIF       (NOT('&SYSLIST(&I)' EQ 'OCCURS')).AIF_412_3
&CUR_SET_LVL        SETA &CUR_SET_LVL+1  INC SUB LVL CTR
&DUP                SETA  &SYSLIST(&I+1)
&I                  SETA  &I+2
.*                  AIF   ('&SYSLIST(&I)' EQ 'TIMES')
                      AIF   (NOT('&SYSLIST(&I)' EQ 'TIMES')).AIF_420_1
&I                        SETA  &I+1
.*                  AELSEIF ('&SYSLIST(&I)' EQ 'TO') n1 TO n2 TIMES
                      AGO   .AIF_420_E
.AIF_420_1            ANOP
                      AIF     (NOT('&SYSLIST(&I)' EQ 'TO')).AIF_420_2  X
               n1 TO n2 TIMES
&DUP_MIN                  SETA &DUP
&DUP                      SETA &SYSLIST(&I+1)   (USE N2)
&I                        SETA &I+2
.*                        AIF   ('&SYSLIST(&I)' EQ 'TIMES')
                            AIF   (NOT('&SYSLIST(&I)' EQ 'TIMES')).AIF_X
               421_1
&I                              SETA  &I+1
.*                        AEND
.AIF_421_1                  ANOP
.*                        AIF   ('&SYSLIST(&I)' EQ 'DEPENDING'         X
                                 AND '&SYSLIST(&I+1)' EQ 'ON')
                            AIF   (NOT('&SYSLIST(&I)' EQ 'DEPENDING'   X
                                       AND '&SYSLIST(&I+1)' EQ 'ON')).AX
               IF_422_1
&SYM_DEPEND(&I)                 SETC '&SYSLIST(&I+2)'
&I                              SETA &I+3
.*                        AELSE
                            AGO   .AIF_422_E
.AIF_422_1                  ANOP
                                MNOTE 8,'WS &ERR_ID MISSING DEPENDING OX
               N'
.*                        AEND
.AIF_422_E                  ANOP
.*                  AEND
.AIF_420_2            ANOP
.AIF_420_E            ANOP
&SYM_DUP(&SYM_TOT)  SETA &DUP
.*             AELSEIF   ('&SYSLIST(&I)' EQ 'INDEXED')
                 AGO   .AIF_412_E
.AIF_412_3       ANOP
                 AIF       (NOT('&SYSLIST(&I)' EQ 'INDEXED')).AIF_412_4
&I                  SETA  &I+1
.*                  AIF  ('&SYSLIST(&I)' EQ 'BY')  RPI 1062
                      AIF  (NOT('&SYSLIST(&I)' EQ 'BY')).AIF_423_1   RPX
               I 1062
&I                       SETA &I+1
.*                  AEND
.AIF_423_1            ANOP
&INDEX              SETC '&SYSLIST(&I)'
&I                  SETA &I+1
.*             AELSEIF   ('&SYSLIST(&I)' EQ 'REDEFINES')
                 AGO   .AIF_412_E
.AIF_412_4       ANOP
                 AIF       (NOT('&SYSLIST(&I)' EQ 'REDEFINES')).AIF_412X
               _5
                    ZC_SYM_FIND &SYSLIST(&I+1)
.*                  AIF   (&SYM_IX EQ 0)
                      AIF   (NOT(&SYM_IX EQ 0)).AIF_424_1
                          MNOTE 8,'WS &ERR_ID REDEFINE ITEM NOT FOUND -X
                &SYSLIST(&I+1)'
                          MEXIT
.*                  AELSE
                      AGO   .AIF_424_E
.AIF_424_1            ANOP
.*                    AIF (&SYM_FXT(&SYM_IX) GT 0)  RPI 1065
                        AIF (NOT(&SYM_FXT(&SYM_IX) GT 0)).AIF_425_1   RX
               PI 1065
&SYM_IX                   SETA &SYM_FXT(&SYM_IX)
.*                        AWHILE (&SYM_IX GT 0)
.AWH_58_T                   ANOP
                            AIF    (NOT(&SYM_IX GT 0)).AWH_58_E
.*                            AIF (&WS_LVL_SYM_IX(&WS_LVL-1) EQ &SYM_QIX
               X(&SYM_IX))
                                AIF (NOT(&WS_LVL_SYM_IX(&WS_LVL-1) EQ &X
               SYM_QIX(&SYM_IX))).AIF_426_1
.*                                AEXIT AWHILE
                                    AGO   .AWH_58_E
.*                            AELSE
                                AGO   .AIF_426_E
.AIF_426_1                      ANOP
&SYM_IX                           SETA &SYM_NXT(&SYM_IX)
.*                                AIF (&SYM_IX EQ 0)
                                    AIF (NOT(&SYM_IX EQ 0)).AIF_427_1
         MNOTE 8,'WS QUAL REDEFINE NOT FOUND FOR - &SYM_NAME(&SYM_TOT)'
                          MEXIT
.*                                AEND
.AIF_427_1                          ANOP
.*                            AEND
.AIF_426_E                      ANOP
.*                        AEND
                            AGO   .AWH_58_T
.AWH_58_E                   ANOP
.*                    AEND
.AIF_425_1              ANOP
.*                  AEND
.AIF_424_E            ANOP
&SYM_REF(&SYM_TOT)  SETA &SYM_IX  SET REDEFINE IX
.*                  AIF (&SYM_LEN(&SYM_IX) EQ 0)
                      AIF (NOT(&SYM_LEN(&SYM_IX) EQ 0)).AIF_428_1
&SYM_LEN(&SYM_IX)       SETA &WS_LOC-&SYM_LOC(&SYM_IX)
.*                  AEND
.AIF_428_1            ANOP
&WS_LVL_RDEF_END(&WS_LVL) SETA &SYM_LOC(&SYM_IX)+&SYM_LEN(&SYM_IX)*&SYMX
               _DUP(&SYM_IX)
&WS_LOC             SETA  &SYM_LOC(&SYM_IX) RESET LOC TO RED
&WS_LVL_RDEF_IX(&WS_LVL) SETA &SYM_IX WS SYM IX
&SYM_LOC(&SYM_TOT)  SETA &WS_LOC
&WS_LVL_RDEF(&WS_LVL) SETB 1     TURN ON REDEFINE
&I                  SETA  &I+2
.*             AELSEIF   ('&SYSLIST(&I)' EQ 'SYNC'                     X
                       OR '&SYSLIST(&I)' EQ 'SYNCHRONIZED') RPI 1062
                 AGO   .AIF_412_E
.AIF_412_5       ANOP
                 AIF       (NOT('&SYSLIST(&I)' EQ 'SYNC'               X
                             OR '&SYSLIST(&I)' EQ 'SYNCHRONIZED')).AIF_X
               412_6  RPI 1062
&SYM_OPT(&SYM_TOT)  SETC '&SYM_OPT(&SYM_TOT).S'
&I                  SETA  &I+1
.*                  AIF   ('&SYSLIST(&I)' EQ 'RIGHT'                   X
                        OR '&SYSLIST(&I)' EQ 'LEFT') RPI 1062
                      AIF   (NOT('&SYSLIST(&I)' EQ 'RIGHT'             X
                              OR '&SYSLIST(&I)' EQ 'LEFT')).AIF_429_1  X
               RPI 1062
&I                        SETA  &I+1
.*                  AEND
.AIF_429_1            ANOP
.*             AELSEIF   ('&SYSLIST(&I)' EQ 'USAGE')
                 AGO   .AIF_412_E
.AIF_412_6       ANOP
                 AIF       (NOT('&SYSLIST(&I)' EQ 'USAGE')).AIF_412_7
&I                  SETA  &I+1
.*                  AIF   ('&SYSLIST(&I)' EQ 'IS')
                      AIF   (NOT('&SYSLIST(&I)' EQ 'IS')).AIF_430_1
&I                        SETA  &I+1
.*                  AEND
.AIF_430_1            ANOP
&USAGE              SETC  '&SYSLIST(&I)'
&I                  SETA  &I+1
.*             AELSEIF   ('&SYSLIST(&I)' EQ 'SIGN')
                 AGO   .AIF_412_E
.AIF_412_7       ANOP
                 AIF       (NOT('&SYSLIST(&I)' EQ 'SIGN')).AIF_412_8
&I                  SETA  &I+1
.*                  AIF   ('&SYSLIST(&I)' EQ 'IS')
                      AIF   (NOT('&SYSLIST(&I)' EQ 'IS')).AIF_431_1
&I                        SETA  &I+1
.*                  AEND
.AIF_431_1            ANOP
.*                  AIF   ('&SYSLIST(&I)' EQ 'LEADING')
                      AIF   (NOT('&SYSLIST(&I)' EQ 'LEADING')).AIF_432_X
               1
&PIC_LEAD                 SETB 1      LEADING SIGN
&I                        SETA  &I+1
.*                  AELSEIF ('&SYSLIST(&I)' EQ 'TRAILING')
                      AGO   .AIF_432_E
.AIF_432_1            ANOP
                      AIF     (NOT('&SYSLIST(&I)' EQ 'TRAILING')).AIF_4X
               32_2
&PIC_TRAIL                SETB 1      TRAILING SIGN
&I                        SETA  &I+1
.*                  AELSE
                      AGO   .AIF_432_E
.AIF_432_2            ANOP
                          MNOTE 8,'WS SIGN UNKNOWN OPTION &SYLIST(&I)'
                          MEXIT
.*                  AEND
.AIF_432_E            ANOP
.*                  AIF   ('&SYSLIST(&I)' EQ 'SEPARATE')
                      AIF   (NOT('&SYSLIST(&I)' EQ 'SEPARATE')).AIF_433X
               _1
&PIC_SEP                  SETB 1
&I                        SETA  &I+1
.*                        AIF  ('&SYSLIST(&I)' EQ 'CHARACTER')
                            AIF  (NOT('&SYSLIST(&I)' EQ 'CHARACTER')).AX
               IF_434_1
&I                             SETA  &I+1
.*                        AEND
.AIF_434_1                  ANOP
.*                  AEND
.AIF_433_1            ANOP
.*             AELSEIF  ('&SYSLIST(&I)' EQ 'LEADING')
                 AGO   .AIF_412_E
.AIF_412_8       ANOP
                 AIF      (NOT('&SYSLIST(&I)' EQ 'LEADING')).AIF_412_9
&PIC_LEAD           SETB 1      LEADING SIGN RPI 1012
&I                  SETA  &I+1
.*                  AIF   ('&SYSLIST(&I)' EQ 'SEPARATE')
                      AIF   (NOT('&SYSLIST(&I)' EQ 'SEPARATE')).AIF_435X
               _1
&PIC_SEP                  SETB 1
&I                        SETA  &I+1
.*                        AIF  ('&SYSLIST(&I)' EQ 'CHARACTER')
                            AIF  (NOT('&SYSLIST(&I)' EQ 'CHARACTER')).AX
               IF_436_1
&I                             SETA  &I+1
.*                        AEND
.AIF_436_1                  ANOP
.*                  AEND
.AIF_435_1            ANOP
.*             AELSEIF ('&SYSLIST(&I)' EQ 'TRAILING')
                 AGO   .AIF_412_E
.AIF_412_9       ANOP
                 AIF     (NOT('&SYSLIST(&I)' EQ 'TRAILING')).AIF_412_10
&PIC_TRAIL          SETB 1      TRAILING SIGN RPI 1012
&I                  SETA  &I+1
.*                  AIF   ('&SYSLIST(&I)' EQ 'SEPARATE')
                      AIF   (NOT('&SYSLIST(&I)' EQ 'SEPARATE')).AIF_437X
               _1
&PIC_SEP                  SETB 1
&I                        SETA  &I+1
.*                        AIF  ('&SYSLIST(&I)' EQ 'CHARACTER')
                            AIF  (NOT('&SYSLIST(&I)' EQ 'CHARACTER')).AX
               IF_438_1
&I                             SETA  &I+1
.*                        AEND
.AIF_438_1                  ANOP
.*                  AEND
.AIF_437_1            ANOP
.*             AELSEIF ('&SYSLIST(&I)'  EQ 'COMP'                      X
                      OR '&SYSLIST(&I)' EQ 'COMPUTATIONAL'             X
                      OR '&SYSLIST(&I)' EQ 'COMP_1'                    X
                      OR '&SYSLIST(&I)' EQ 'COMP_2'                    X
                      OR '&SYSLIST(&I)' EQ 'COMP_3'                    X
                      OR '&SYSLIST(&I)' EQ 'COMP_4'                    X
                      OR '&SYSLIST(&I)' EQ 'COMP_5'                    X
                      OR '&SYSLIST(&I)' EQ 'BINARY'                    X
                      OR '&SYSLIST(&I)' EQ 'FLOAT_BINARY_7'            X
                      OR '&SYSLIST(&I)' EQ 'FLOAT_BINARY_16'           X
                      OR '&SYSLIST(&I)' EQ 'FLOAT_BINARY_34'           X
                      OR '&SYSLIST(&I)' EQ 'FLOAT_DECIMAL_7'           X
                      OR '&SYSLIST(&I)' EQ 'FLOAT_DECIMAL_16'          X
                      OR '&SYSLIST(&I)' EQ 'FLOAT_DECIMAL_34'          X
                      OR '&SYSLIST(&I)' EQ 'FLOAT_HEX_7'               X
                      OR '&SYSLIST(&I)' EQ 'FLOAT_HEX_15'              X
                      OR '&SYSLIST(&I)' EQ 'FLOAT_HEX_30'              X
                      OR '&SYSLIST(&I)' EQ 'FLOAT_SHORT'               X
                      OR '&SYSLIST(&I)' EQ 'FLOAT_LONG'                X
                      OR '&SYSLIST(&I)' EQ 'FLOAT_EXTENDED'            X
                      OR '&SYSLIST(&I)' EQ 'INDEX'                     X
                      OR '&SYSLIST(&I)' EQ 'POINTER') 1002 1004 1042
                 AGO   .AIF_412_E
.AIF_412_10      ANOP
                 AIF     (NOT('&SYSLIST(&I)'  EQ 'COMP'                X
                            OR '&SYSLIST(&I)' EQ 'COMPUTATIONAL'       X
                            OR '&SYSLIST(&I)' EQ 'COMP_1'              X
                            OR '&SYSLIST(&I)' EQ 'COMP_2'              X
                            OR '&SYSLIST(&I)' EQ 'COMP_3'              X
                            OR '&SYSLIST(&I)' EQ 'COMP_4'              X
                            OR '&SYSLIST(&I)' EQ 'COMP_5'              X
                            OR '&SYSLIST(&I)' EQ 'BINARY'              X
                            OR '&SYSLIST(&I)' EQ 'FLOAT_BINARY_7'      X
                            OR '&SYSLIST(&I)' EQ 'FLOAT_BINARY_16'     X
                            OR '&SYSLIST(&I)' EQ 'FLOAT_BINARY_34'     X
                            OR '&SYSLIST(&I)' EQ 'FLOAT_DECIMAL_7'     X
                            OR '&SYSLIST(&I)' EQ 'FLOAT_DECIMAL_16'    X
                            OR '&SYSLIST(&I)' EQ 'FLOAT_DECIMAL_34'    X
                            OR '&SYSLIST(&I)' EQ 'FLOAT_HEX_7'         X
                            OR '&SYSLIST(&I)' EQ 'FLOAT_HEX_15'        X
                            OR '&SYSLIST(&I)' EQ 'FLOAT_HEX_30'        X
                            OR '&SYSLIST(&I)' EQ 'FLOAT_SHORT'         X
                            OR '&SYSLIST(&I)' EQ 'FLOAT_LONG'          X
                            OR '&SYSLIST(&I)' EQ 'FLOAT_EXTENDED'      X
                            OR '&SYSLIST(&I)' EQ 'INDEX'               X
                            OR '&SYSLIST(&I)' EQ 'POINTER')).AIF_412_11X
                 1002 1004 1042
&USAGE              SETC  '&SYSLIST(&I)'
&I                  SETA  &I+1
.*             AELSEIF ('&SYSLIST(&I)' EQ 'DISPLAY')
                 AGO   .AIF_412_E
.AIF_412_11      ANOP
                 AIF     (NOT('&SYSLIST(&I)' EQ 'DISPLAY')).AIF_412_12
&I                  SETA &I+1  RPI 1065 IGNORE (ITS THE DEFAULT)
.*             AELSEIF ('&SYSLIST(&I)' EQ 'LINE')
                 AGO   .AIF_412_E
.AIF_412_12      ANOP
                 AIF     (NOT('&SYSLIST(&I)' EQ 'LINE')).AIF_412_13
.*                  AIF ('&SYSLIST(&I+1)' EQ 'NUMBER'                  X
                         AND '&SYSLIST(&I+2)' EQ 'IS'                  X
                         AND '&SYSLIST(&I+3)' EQ 'PLUS')
                      AIF (NOT('&SYSLIST(&I+1)' EQ 'NUMBER'            X
                               AND '&SYSLIST(&I+2)' EQ 'IS'            X
                               AND '&SYSLIST(&I+3)' EQ 'PLUS')).AIF_439X
               _1
&I                      SETA &I+5
.*                  AELSE
                      AGO   .AIF_439_E
.AIF_439_1            ANOP
&I                      SETA &I+2
.*                  AEND
.AIF_439_E            ANOP
                    MNOTE 8,'WS REPORT WRITER LINE CLAUSE IGNORED'
.*             AELSEIF ('&SYSLIST(&I)' EQ 'COLUMN')
                 AGO   .AIF_412_E
.AIF_412_13      ANOP
                 AIF     (NOT('&SYSLIST(&I)' EQ 'COLUMN')).AIF_412_14
.*                  AIF  (&I EQ 2)
                      AIF  (NOT(&I EQ 2)).AIF_440_1
&NEW_SYM                SETC 'FILLER'
&SYM_NAME(&SYM_TOT)     SETC 'FILLER'
.*                  AEND
.AIF_440_1            ANOP
.*                  AIF ('&SYSLIST(&I+1)' EQ 'NUMBER'                  X
                        AND '&SYSLIST(&I+2)' EQ 'IS')
                      AIF (NOT('&SYSLIST(&I+1)' EQ 'NUMBER'            X
                              AND '&SYSLIST(&I+2)' EQ 'IS')).AIF_441_1
&I                      SETA &I+4
.*                  AELSE
                      AGO   .AIF_441_E
.AIF_441_1            ANOP
&I                      SETA &I+2  RPI 1065 IGNORE RW LINE NUM
.*                  AEND
.AIF_441_E            ANOP
                    MNOTE 8,'WS REPORT WRITER COLUMN CLAUSE IGNORED'
.*             AELSEIF ('&SYSLIST(&I)' EQ 'SOURCE')
                 AGO   .AIF_412_E
.AIF_412_14      ANOP
                 AIF     (NOT('&SYSLIST(&I)' EQ 'SOURCE')).AIF_412_15
.*                  AIF ('&SYSLIST(&I+1)' EQ 'IS')
                      AIF (NOT('&SYSLIST(&I+1)' EQ 'IS')).AIF_442_1
&I                      SETA &I+3
.*                  AELSE
                      AGO   .AIF_442_E
.AIF_442_1            ANOP
&I                      SETA &I+2  RPI 1065 IGNORE RW LINE NUM
.*                  AEND
.AIF_442_E            ANOP
                    MNOTE 8,'WS REPORT WRITER SOURCE CLAUSE IGNORED'
.*             AELSEIF ('&SYSLIST(&I)' EQ 'TYPE'                       X
                        AND '&SYSLIST(&I+1)' EQ 'IS')
                 AGO   .AIF_412_E
.AIF_412_15      ANOP
                 AIF     (NOT('&SYSLIST(&I)' EQ 'TYPE'                 X
                              AND '&SYSLIST(&I+1)' EQ 'IS')).AIF_412_16
&I                  SETA &I+2  RPI 1065 IGNORE RW TYPE
                    MNOTE 8,'WS REPORT WRITER TYPE CLAUSE IGNORED'
.*                  AIF ('&SYSLIST(&I+2)' EQ 'PAGE')
                      AIF (NOT('&SYSLIST(&I+2)' EQ 'PAGE')).AIF_443_1
&I                      SETA &I+4
.*                  AELSE
                      AGO   .AIF_443_E
.AIF_443_1            ANOP
&I                      SETA &I+3
.*                  AEND
.AIF_443_E            ANOP
.*             AELSEIF ('&SYSLIST(&I)' EQ 'JUSTIFIED'                  X
                        OR '&SYSLIST(&I)' EQ 'JUST') RPI 1065
                 AGO   .AIF_412_E
.AIF_412_16      ANOP
                 AIF     (NOT('&SYSLIST(&I)' EQ 'JUSTIFIED'            X
                              OR '&SYSLIST(&I)' EQ 'JUST')).AIF_412_17 X
                RPI 1065
&SYM_OPT(&SYM_TOT)      SETC '&SYM_OPT(&SYM_TOT).R'
.*                  AIF ('&SYSLIST(&I+1)' EQ 'RIGHT')
                      AIF (NOT('&SYSLIST(&I+1)' EQ 'RIGHT')).AIF_444_1
&I                      SETA &I+2
.*                  AELSE
                      AGO   .AIF_444_E
.AIF_444_1            ANOP
&I                      SETA &I+2
.*                  AEND
.AIF_444_E            ANOP
.*             AELSEIF ('&SYSLIST(&I)' EQ 'LEADING')
                 AGO   .AIF_412_E
.AIF_412_17      ANOP
                 AIF     (NOT('&SYSLIST(&I)' EQ 'LEADING')).AIF_412_18
&SYM_OPT(&SYM_TOT)  SETC '&SYM_OPT(&SYM_TOT).L'
&I                  SETA &I+1
.*             AELSEIF ('&SYSLIST(&I)' EQ 'BLANK')
                 AGO   .AIF_412_E
.AIF_412_18      ANOP
                 AIF     (NOT('&SYSLIST(&I)' EQ 'BLANK')).AIF_412_19
&SYM_OPT(&SYM_TOT)  SETC '&SYM_OPT(&SYM_TOT).B'
&I                  SETA &I+1
.*                  AIF ('&SYSLIST(&I)' EQ 'WHEN')
                      AIF (NOT('&SYSLIST(&I)' EQ 'WHEN')).AIF_445_1
&I                      SETA &I+1
.*                  AEND
.AIF_445_1            ANOP
.*                  AIF ('&SYSLIST(&I)' EQ 'ZERO')
                      AIF (NOT('&SYSLIST(&I)' EQ 'ZERO')).AIF_446_1
&I                      SETA &I+1
.*                  AEND
.AIF_446_1            ANOP
.*             AELSEIF ('&SYSLIST(&I)' EQ 'GLOBAL')
                 AGO   .AIF_412_E
.AIF_412_19      ANOP
                 AIF     (NOT('&SYSLIST(&I)' EQ 'GLOBAL')).AIF_412_20
&SYM_OPT(&SYM_TOT)  SETC '&SYM_OPT(&SYM_TOT).G'
&I                  SETA &I+1
.*             AELSEIF ('&SYSLIST(&I)' EQ 'EXTERNAL')
                 AGO   .AIF_412_E
.AIF_412_20      ANOP
                 AIF     (NOT('&SYSLIST(&I)' EQ 'EXTERNAL')).AIF_412_21
&SYM_OPT(&SYM_TOT)  SETC '&SYM_OPT(&SYM_TOT).E'
&I                  SETA &I+1
.*             AELSEIF (&I GT 2)
                 AGO   .AIF_412_E
.AIF_412_21      ANOP
                 AIF     (NOT(&I GT 2)).AIF_412_22
                    MNOTE 8,'WS &ERR_ID UNRECOGNIZED CLAUSE &SYSLIST(&IX
               )'
                    MEXIT
.*             AELSE
                 AGO   .AIF_412_E
.AIF_412_22      ANOP
&I                  SETA &I+1
.*             AEND
.AIF_412_E       ANOP
.*       AEND
           AGO   .AWH_57_T
.AWH_57_E  ANOP
.*       AEND
           AGO   (&ACALL_69_WS_SCAN_CLAUSES).ACL_69_1
.ACL_69_SKIP ANOP
.*
.* SET PIC AND DEFAULT PIC_LEN
.*
.*  1.  IF LEADING S SET PIC_SIGN AND REMOVE S
.*  2.  DO NOT ALLOW QUOTE IN PIC CLAUSE
.*
.*       AENTRY  SET_PIC_AND_LEN
           AGO   .ACL_72_SKIP
.ACL_72_SET_PIC_AND_LEN ANOP
&PIC     SETC '&SYSLIST(&I)'
.*       AIF  ('&PIC'(1,1) EQ '''')
           AIF  (NOT('&PIC'(1,1) EQ '''')).AIF_447_1
&PIC          SETC '&PIC'(2,K'&PIC-2)
.*       AEND
.AIF_447_1 ANOP
.*       AIF  ('&PIC'(1,1) EQ 'S')
           AIF  (NOT('&PIC'(1,1) EQ 'S')).AIF_448_1
&PIC_SIGN     SETB 1
&PIC          SETC  '&PIC'(2,*)
.*       AEND
.AIF_448_1 ANOP
&J       SETA 1
&DEC_MODE SETB 0
.*       AWHILE (&J LE K'&PIC)
.AWH_59_T  ANOP
           AIF    (NOT(&J LE K'&PIC)).AWH_59_E
.*              AIF ('&PIC'(&J,1) EQ 'V')
                  AIF (NOT('&PIC'(&J,1) EQ 'V')).AIF_449_1
&DEC_MODE           SETB 1
.*              AELSEIF ('&PIC'(&J,2) EQ 'X('                          X
                         OR '&PIC'(&J,2) EQ '9('                       X
                         OR '&PIC'(&J,2) EQ 'A('                       X
                         OR '&PIC'(&J,2) EQ 'P('                       X
                         OR '&PIC'(&J,2) EQ 'Z(')  RPI 1065 ADD P,A
                  AGO   .AIF_449_E
.AIF_449_1        ANOP
                  AIF     (NOT('&PIC'(&J,2) EQ 'X('                    X
                               OR '&PIC'(&J,2) EQ '9('                 X
                               OR '&PIC'(&J,2) EQ 'A('                 X
                               OR '&PIC'(&J,2) EQ 'P('                 X
                               OR '&PIC'(&J,2) EQ 'Z(')).AIF_449_2   RPX
               I 1065 ADD P,A
.*                  AIF ('&PIC'(&J,1) EQ 'P')
                      AIF (NOT('&PIC'(&J,1) EQ 'P')).AIF_450_1
&DEC_MODE              SETB 1
.*                  AEND
.AIF_450_1            ANOP
&PIC_DUP            SETA '&PIC'(&J+2,1)
&J                  SETA &J+3
.*                  AWHILE (&J LT K'&PIC                               X
                            AND '&PIC'(&J,1) NE ')')
.AWH_60_T             ANOP
                      AIF    (NOT(&J LT K'&PIC                         X
                                  AND '&PIC'(&J,1) NE ')')).AWH_60_E
&DIGIT                  SETA '&PIC'(&J,1)
&PIC_DUP                SETA &PIC_DUP*10+&DIGIT
&J                      SETA &J+1
.*                  AEND
                      AGO   .AWH_60_T
.AWH_60_E             ANOP
&PIC_LEN            SETA &PIC_LEN+&PIC_DUP
.*                  AIF (&DEC_MODE)
                      AIF (NOT(&DEC_MODE)).AIF_451_1
&SYM_PIC_DEC(&SYM_TOT)  SETA &SYM_PIC_DEC(&SYM_TOT)+&PIC_DUP
.*                  AEND
.AIF_451_1            ANOP
.*              AELSEIF ('&PIC'(&J,1) EQ '''')  RPI 1032
                  AGO   .AIF_449_E
.AIF_449_2        ANOP
                  AIF     (NOT('&PIC'(&J,1) EQ '''')).AIF_449_3   RPI 1X
               032
                    MNOTE 8,'WS UNKNOWN PIC CLAUSE &PIC'
.*                  AEXIT AWHILE
                      AGO   .AWH_59_E
.*              AELSE
                  AGO   .AIF_449_E
.AIF_449_3        ANOP
.*                  AIF ('&PIC'(&J,1) EQ 'P')
                      AIF (NOT('&PIC'(&J,1) EQ 'P')).AIF_452_1
&DEC_MODE              SETB 1
.*                  AEND
.AIF_452_1            ANOP
&PIC_LEN            SETA &PIC_LEN+1
.*                  AIF (&DEC_MODE)
                      AIF (NOT(&DEC_MODE)).AIF_453_1
&SYM_PIC_DEC(&SYM_TOT)  SETA &SYM_PIC_DEC(&SYM_TOT)+1
.*                  AEND
.AIF_453_1            ANOP
.*              AEND
.AIF_449_E        ANOP
&J              SETA &J+1
.*       AEND
           AGO   .AWH_59_T
.AWH_59_E  ANOP
.*       AEND
           AGO   (&ACALL_72_SET_PIC_AND_LEN).ACL_72_1
.ACL_72_SKIP ANOP
.*
.* PROCESS WS ITEM WITH ALL CLAUSE VALUES DEFINED
.*
.*       AENTRY WS_PROC_CLAUSES
           AGO   .ACL_70_SKIP
.ACL_70_WS_PROC_CLAUSES ANOP
.*       AIF   (&NP LT 2)
           AIF   (NOT(&NP LT 2)).AIF_454_1
&NEW_SYM       SETC 'FILLER'
&SYM_NAME(&SYM_TOT) SETC 'FILLER'
.*       AELSEIF ('&USAGE' EQ 'COMP'                                   X
                  OR '&USAGE' EQ 'COMPUTATIONAL'                       X
                  OR '&USAGE' EQ 'BINARY'                              X
                  OR '&USAGE' EQ 'COMP_4'                              X
                  OR '&USAGE' EQ 'COMP_5')  RPI 1042 RPI 1090
           AGO   .AIF_454_E
.AIF_454_1 ANOP
           AIF     (NOT('&USAGE' EQ 'COMP'                             X
                        OR '&USAGE' EQ 'COMPUTATIONAL'                 X
                        OR '&USAGE' EQ 'BINARY'                        X
                        OR '&USAGE' EQ 'COMP_4'                        X
                        OR '&USAGE' EQ 'COMP_5')).AIF_454_2   RPI 1042 X
               RPI 1090
.*             AIF  ('&PIC' NE '')
                 AIF  (NOT('&PIC' NE '')).AIF_455_1
.*                  AIF  (&PIC_LEN LE 0)
                      AIF  (NOT(&PIC_LEN LE 0)).AIF_456_1
&PIC_LEN                 SETA K'&PIC
.*                  AEND
.AIF_456_1            ANOP
.*                  AIF  (&PIC_LEN LE 4)
                      AIF  (NOT(&PIC_LEN LE 4)).AIF_457_1
&PIC_LEN                 SETA 2
&PIC_TYPE                SETC 'H'
.*                  AELSEIF (&PIC_LEN LE 9)
                      AGO   .AIF_457_E
.AIF_457_1            ANOP
                      AIF     (NOT(&PIC_LEN LE 9)).AIF_457_2
&PIC_LEN                 SETA 4
&PIC_TYPE                SETC 'F'
.*                  AELSEIF (&PIC_LEN LE 18)
                      AGO   .AIF_457_E
.AIF_457_2            ANOP
                      AIF     (NOT(&PIC_LEN LE 18)).AIF_457_3
&PIC_LEN                 SETA 8
&PIC_TYPE                SETC 'G'
.*                  AELSEIF (&ZC_EXTEND AND &PIC_LEN LE 39) RPI 1004
                      AGO   .AIF_457_E
.AIF_457_3            ANOP
                      AIF     (NOT(&ZC_EXTEND AND &PIC_LEN LE 39)).AIF_X
               457_4  RPI 1004
&PIC_LEN                 SETA 16
&PIC_TYPE                SETC 'Q'
.*                  AELSE
                      AGO   .AIF_457_E
.AIF_457_4            ANOP
                         MNOTE 8,'WS INVALID COMP PIC LEN - &PIC_LEN'
.*                  AEND
.AIF_457_E            ANOP
.*             AEND
.AIF_455_1       ANOP
&NUM           SETC '&PIC_VAL'
&DEC_SYM       SETA &SYM_PIC_DEC(&SYM_TOT)
.*             ACALL SCALE_NUM_LIT
&ACALL_73_SCALE_NUM_LIT SETA    1
                 AGO   .ACL_73_SCALE_NUM_LIT
.ACL_73_1        ANOP
&PIC_VAL       SETC '&NUM'
.*       AELSEIF ('&USAGE' EQ 'COMP_1')
           AGO   .AIF_454_E
.AIF_454_2 ANOP
           AIF     (NOT('&USAGE' EQ 'COMP_1')).AIF_454_3
&PIC_LEN       SETA 4
&PIC_TYPE      SETC '1'       RPI 1004 WAS E NOW 1=EH
.*       AELSEIF ('&USAGE' EQ 'COMP_2')
           AGO   .AIF_454_E
.AIF_454_3 ANOP
           AIF     (NOT('&USAGE' EQ 'COMP_2')).AIF_454_4
&PIC_LEN       SETA 8
&PIC_TYPE      SETC '2'       RPI 1004 WAS D NOW 2=DH
.*       AELSEIF ('&USAGE' EQ 'COMP_3')
           AGO   .AIF_454_E
.AIF_454_4 ANOP
           AIF     (NOT('&USAGE' EQ 'COMP_3')).AIF_454_5
.*             AIF   ('&PIC' NE '')
                 AIF   (NOT('&PIC' NE '')).AIF_458_1
.*                   AIF  (&PIC_LEN EQ 0)
                       AIF  (NOT(&PIC_LEN EQ 0)).AIF_459_1
&PIC_LEN                  SETA K'&PIC
.*                   AEND
.AIF_459_1             ANOP
.*             AEND
.AIF_458_1       ANOP
&PIC_TYPE      SETC 'P'
&NUM           SETC '&PIC_VAL'
&DEC_SYM       SETA &SYM_PIC_DEC(&SYM_TOT)
.*             ACALL SCALE_NUM_LIT
&ACALL_73_SCALE_NUM_LIT SETA    2
                 AGO   .ACL_73_SCALE_NUM_LIT
.ACL_73_2        ANOP
&PIC_VAL       SETC '&NUM'
.*       AELSEIF ('&USAGE' EQ 'FLOAT_HEX_7')     RPI 1004
           AGO   .AIF_454_E
.AIF_454_5 ANOP
           AIF     (NOT('&USAGE' EQ 'FLOAT_HEX_7')).AIF_454_6      RPI X
               1004
&PIC_LEN      SETA 4
&PIC_TYPE     SETC '1'
.*       AELSEIF ('&USAGE' EQ 'FLOAT_HEX_15')      RPI 1004
           AGO   .AIF_454_E
.AIF_454_6 ANOP
           AIF     (NOT('&USAGE' EQ 'FLOAT_HEX_15')).AIF_454_7       RPX
               I 1004
&PIC_LEN      SETA 8
&PIC_TYPE     SETC '2'
.*       AELSEIF ('&USAGE' EQ 'FLOAT_HEX_30' AND &ZC_EXTEND) RPI 1004
           AGO   .AIF_454_E
.AIF_454_7 ANOP
           AIF     (NOT('&USAGE' EQ 'FLOAT_HEX_30' AND &ZC_EXTEND)).AIFX
               _454_8  RPI 1004
&PIC_LEN      SETA 16
&PIC_TYPE     SETC '3'
.*       AELSEIF ('&USAGE' EQ 'FLOAT_BINARY_7' AND &ZC_EXTEND) RPI 1004
           AGO   .AIF_454_E
.AIF_454_8 ANOP
           AIF     (NOT('&USAGE' EQ 'FLOAT_BINARY_7' AND &ZC_EXTEND)).AX
               IF_454_9  RPI 1004
&PIC_LEN      SETA 4
&PIC_TYPE     SETC '4'
.*       AELSEIF ('&USAGE' EQ 'FLOAT_BINARY_16' AND &ZC_EXTEND) RPI1004
           AGO   .AIF_454_E
.AIF_454_9 ANOP
           AIF     (NOT('&USAGE' EQ 'FLOAT_BINARY_16' AND &ZC_EXTEND)).X
               AIF_454_10  RPI1004
&PIC_LEN      SETA 8
&PIC_TYPE     SETC '5'
.*       AELSEIF ('&USAGE' EQ 'FLOAT_BINARY_34' AND &ZC_EXTEND) RPI1004
           AGO   .AIF_454_E
.AIF_454_10 ANOP
           AIF     (NOT('&USAGE' EQ 'FLOAT_BINARY_34' AND &ZC_EXTEND)).X
               AIF_454_11  RPI1004
&PIC_LEN      SETA 16
&PIC_TYPE     SETC '6'
.*       AELSEIF ('&USAGE' EQ 'FLOAT_DECIMAL_7' AND &ZC_EXTEND) RPI1004
           AGO   .AIF_454_E
.AIF_454_11 ANOP
           AIF     (NOT('&USAGE' EQ 'FLOAT_DECIMAL_7' AND &ZC_EXTEND)).X
               AIF_454_12  RPI1004
&PIC_LEN      SETA 4
&PIC_TYPE     SETC '7'
.*       AELSEIF ('&USAGE' EQ 'FLOAT_DECIMAL_16' AND &ZC_EXTEND) RP1004
           AGO   .AIF_454_E
.AIF_454_12 ANOP
           AIF     (NOT('&USAGE' EQ 'FLOAT_DECIMAL_16' AND &ZC_EXTEND))X
               .AIF_454_13  RP1004
&PIC_LEN      SETA 8
&PIC_TYPE     SETC '8'
.*       AELSEIF ('&USAGE' EQ 'FLOAT_DECIMAL_34' AND &ZC_EXTEND) RP1004
           AGO   .AIF_454_E
.AIF_454_13 ANOP
           AIF     (NOT('&USAGE' EQ 'FLOAT_DECIMAL_34' AND &ZC_EXTEND))X
               .AIF_454_14  RP1004
&PIC_LEN      SETA 16
&PIC_TYPE     SETC '9'
.*       AELSEIF ('&USAGE' EQ 'FLOAT_SHORT' AND &ZC_EXTEND) RPI 1004
           AGO   .AIF_454_E
.AIF_454_14 ANOP
           AIF     (NOT('&USAGE' EQ 'FLOAT_SHORT' AND &ZC_EXTEND)).AIF_X
               454_15  RPI 1004
&PIC_LEN      SETA 4
&PIC_TYPE     SETC '&ZC_FLOAT_SHORT_TYPE'
.*       AELSEIF ('&USAGE' EQ 'FLOAT_LONG' AND &ZC_EXTEND)  RPI 1004
           AGO   .AIF_454_E
.AIF_454_15 ANOP
           AIF     (NOT('&USAGE' EQ 'FLOAT_LONG' AND &ZC_EXTEND)).AIF_4X
               54_16   RPI 1004
&PIC_LEN      SETA 8
&PIC_TYPE     SETC '&ZC_FLOAT_LONG_TYPE'
.*       AELSEIF ('&USAGE' EQ 'FLOAT_EXTENDED' AND &ZC_EXTEND) RPI 1004
           AGO   .AIF_454_E
.AIF_454_16 ANOP
           AIF     (NOT('&USAGE' EQ 'FLOAT_EXTENDED' AND &ZC_EXTEND)).AX
               IF_454_17  RPI 1004
&PIC_LEN      SETA 16
&PIC_TYPE     SETC '&ZC_FLOAT_EXTENDED_TYPE'
.*       AELSEIF ('&USAGE' EQ 'INDEX')           RPI 1002
           AGO   .AIF_454_E
.AIF_454_17 ANOP
           AIF     (NOT('&USAGE' EQ 'INDEX')).AIF_454_18            RPIX
                1002
&PIC_LEN      SETA 4
&PIC_TYPE     SETC 'I'
.*       AELSEIF ('&USAGE' EQ 'POINTER')
           AGO   .AIF_454_E
.AIF_454_18 ANOP
           AIF     (NOT('&USAGE' EQ 'POINTER')).AIF_454_19
&PIC_LEN      SETA 4
&PIC_TYPE     SETC 'A'
.*       AELSEIF ('&USAGE' EQ 'PROCEDURE_POINTER')   RPI 1010
           AGO   .AIF_454_E
.AIF_454_19 ANOP
           AIF     (NOT('&USAGE' EQ 'PROCEDURE_POINTER')).AIF_454_20   X
                RPI 1010
&PIC_LEN      SETA 4
&PIC_TYPE     SETC 'A'
.*       AELSEIF (K'&PIC GT 0)
           AGO   .AIF_454_E
.AIF_454_20 ANOP
           AIF     (NOT(K'&PIC GT 0)).AIF_454_21
.*             AIF   (('&PIC'(1,1) FIND '9PV') GT 0) RPI 1062 ADD PV
                 AIF   (NOT(('&PIC'(1,1) FIND '9PV') GT 0)).AIF_460_1  X
               RPI 1062 ADD PV
&PIC_TYPE            SETC 'Z'
&NUM                 SETC '&PIC_VAL'
&DEC_SYM             SETA &SYM_PIC_DEC(&SYM_TOT)
.*                   ACALL SCALE_NUM_LIT
&ACALL_73_SCALE_NUM_LIT SETA    3
                       AGO   .ACL_73_SCALE_NUM_LIT
.ACL_73_3              ANOP
&PIC_VAL             SETC '&NUM'
.*             AEND
.AIF_460_1       ANOP
.*             AIF   (&PIC_LEN EQ 0)
                 AIF   (NOT(&PIC_LEN EQ 0)).AIF_461_1
&PIC_LEN             SETA K'&PIC
.*             AEND
.AIF_461_1       ANOP
.*       AEND
.AIF_454_21 ANOP
.AIF_454_E ANOP
&SYM_PIC_TYPE(&SYM_TOT) SETC '&PIC_TYPE'
&SYM_PIC_SIGN(&SYM_TOT) SETB &PIC_SIGN
&SYM_PIC(&SYM_TOT) SETC '&PIC'     SYM FIELD PIC (999,X(3))
.*       AIF (K'&PIC_VAL GT 3)
           AIF (NOT(K'&PIC_VAL GT 3)).AIF_462_1
.*           AIF ('&PIC_VAL'(1,2) EQ 'X"')   CONVERT X"??" TO X'??'
               AIF (NOT('&PIC_VAL'(1,2) EQ 'X"')).AIF_463_1    CONVERT X
               X"??" TO X'??'
&PIC_VAL         SETC 'X'''.'&PIC_VAL'(3,K'&PIC_VAL-3).''''
.*           AEND
.AIF_463_1     ANOP
.*       AEND
.AIF_462_1 ANOP
.*       AIF  ('&PIC_TYPE' EQ 'X'                                      X
               AND '&PIC_VAL' NE '')
           AIF  (NOT('&PIC_TYPE' EQ 'X'                                X
                     AND '&PIC_VAL' NE '')).AIF_464_1
&AMP           SETA ('&PIC_VAL' FIND '&&')
.*             AWHILE (&AMP GT 0)
.AWH_61_T        ANOP
                 AIF    (NOT(&AMP GT 0)).AWH_61_E
&PIC_VAL          SETC '&PIC_VAL'(1,&AMP).'&&'.'&PIC_VAL'(&AMP+1,*)
&NEXT_AMP         SETA ('&PIC_VAL'(&AMP+2,*) FIND '&&')
.*                AIF (&NEXT_AMP GT 0)
                    AIF (NOT(&NEXT_AMP GT 0)).AIF_465_1
&AMP                  SETA &AMP+&NEXT_AMP+1
.*                AELSE
                    AGO   .AIF_465_E
.AIF_465_1          ANOP
&AMP                  SETA 0
.*                AEND
.AIF_465_E          ANOP
.*             AEND
                 AGO   .AWH_61_T
.AWH_61_E        ANOP
.*       AELSEIF (K'&PIC_VAL GT 2)  REMOVE '..' FROM NUMERIC VALUES
           AGO   .AIF_464_E
.AIF_464_1 ANOP
           AIF     (NOT(K'&PIC_VAL GT 2)).AIF_464_2   REMOVE '..' FROM X
               NUMERIC VALUES
.*             AIF ('&PIC_VAL'(1,1) EQ '''')   RPI 1065
                 AIF (NOT('&PIC_VAL'(1,1) EQ '''')).AIF_466_1    RPI 10X
               65
&PIC_VAL           SETC '&PIC_VAL'(2,K'&PIC_VAL-2)
.*             AEND
.AIF_466_1       ANOP
.*       AEND
.AIF_464_2 ANOP
.AIF_464_E ANOP
&SYM_VAL(&SYM_TOT) SETC '&PIC_VAL' SYM FIELD VALUE (0,SPACE)
&SYM_VAL_ALL(&SYM_TOT) SETB &PIC_VAL_ALL  RPI 1012
.*       AIF   ('&PIC_TYPE' EQ 'P')
           AIF   (NOT('&PIC_TYPE' EQ 'P')).AIF_467_1
&PIC_LEN       SETA &PIC_LEN/2+1
.*       AEND
.AIF_467_1 ANOP
&SYM_LEN(&SYM_TOT) SETA  &PIC_LEN  SYM FILED LENGTH
&SYM_DUP(&SYM_TOT) SETA  &DUP      SYM FIELD OCCURANCES
.*       AIF   (&PIC_LEN EQ 0)
           AIF   (NOT(&PIC_LEN EQ 0)).AIF_468_1
               ZC_PIC_LEN
&PIC_LEN       SETA  &SYM_LEN(&SYM_TOT)
.*       AEND
.AIF_468_1 ANOP
&WS_LOC  SETA  &WS_LOC+&SYM_DUP(&SYM_TOT)*&SYM_LEN(&SYM_TOT)
.*       AIF   (&DUP GT 1 AND '&INDEX' NE '')
           AIF   (NOT(&DUP GT 1 AND '&INDEX' NE '')).AIF_469_1
&SET_TOT       SETA  &SET_TOT+1
&SET_SYM_IX(&SET_TOT) SETA &SYM_TOT
&SET_SYM_LVL(&SET_TOT) SETA &CUR_SET_LVL
&SET_INDEX(&SET_TOT) SETC '&INDEX'
.*       AEND
.AIF_469_1 ANOP
.*       AEND
           AGO   (&ACALL_70_WS_PROC_CLAUSES).ACL_70_1
.ACL_70_SKIP ANOP
.*
.* ADD NEW LVL
.*
.*       AENTRY WS_ADD_LVL
           AGO   .ACL_64_SKIP
.ACL_64_WS_ADD_LVL ANOP
&WS_LVL  SETA  &WS_LVL+1
&WS_LVL_NO(&WS_LVL) SETA &NEW_LVL_NO
&WS_LVL_SYM_IX(&WS_LVL) SETA 0
.*       AEND
           AGO   (&ACALL_64_WS_ADD_LVL).ACL_64_1,.ACL_64_2
.ACL_64_SKIP ANOP
         COPY  ZCGETFLD
         COPY  ZCNUMLIT
         MEND
         MACRO
.*********************************************************************
.* Copyright 2009 Automated Software Tools Corporation               *
.* This source code is part of z390 zCOBOL/assembler/emulator package*
.* The z390 package is distributed under GNU general public license  *
.* Author - Don Higgins                                              *
.* Date   - 09/12/09                                                 *
.*********************************************************************
.* 09/12/09 zc390 gens ZCEND for END ???? to avoid END conflict
.*********************************************************************
         ZCEND &TYPE
.*       AIF   ('&TYPE' NE 'PROGRAM')
           AIF   (NOT('&TYPE' NE 'PROGRAM')).AIF_470_1
               MNOTE 8,'ZCEND END_&TYPE NOT SUPPORTED YET'
.*       AEND
.AIF_470_1 ANOP
         MEND
         MACRO
.*********************************************************************
.* Copyright 2008 Automated Software Tools Corporation               *
.* This source code is part of z390 zCOBOL/assembler/emulator package*
.* The z390 package is distributed under GNU general public license  *
.* Author - Don Higgins                                              *
.* Date   - 04/29/08                                                 *
.*********************************************************************
.* 04/29/08 initial coding for global CBL options call gen'd by zcobol
.* 02/25/09 RPI 1004 SET ZC_FLOAT to DECIMAL/BINARY/HEX def. DECIMAL
.* 03/06/09 RPI 1004 SET ZC_FLOAT from option FLOAT(?)
.*********************************************************************
        ZCOBOL &VER=,      z390 version from tz390.version             X
               &PGM=,      program name from args[0] overides PGM-ID.  X
               &CDATE=,    current compile date or MM/DD/YY if notimingX
               &CTIME=,    current compile start time if timing        X
               &OPTIONS=   sublist of all options following program
         COPY ZC_WS
.*
.* SAVE ALL ZCOBOL PARMS IN GLOBALS
.*
&ZC_VER  SETC '&VER'
&ZC_PGM  SETC '&PGM'
&ZC_CDATE SETC '&CDATE'
&ZC_CTIME SETC '&CTIME'
&ZC_LIST SETC 'NOLIST,NOGEN,NOLISTUSE'
&ZC_OPTIONS SETC '&OPTIONS'
.*
.* DEFINE ZCOBOL CLOBAL OPTION DEFAULTS
.*
&ZC_COMMENT SETB 1     DEFAULT ON
&ZC_EXTEND SETB 1     DEFAULT ON
&ZC_FLOAT SETC 'DECIMAL' VS BINARY OR HEX FOR FLOAT-S/L/E
&ZC_FLOAT_SHORT_TYPE SETC '7'
&ZC_FLOAT_LONG_TYPE SETC '8'
&ZC_FLOAT_EXTENDED_TYPE SETC '9'
&ZC_R64  SETB 1     ALLOW 64 BIT REGISTER INSTRUCTIONS
.*
.* SET OVERRIDES FROM PARM OPTIONS
.*
&I       SETA 1
&NP      SETA N'&OPTIONS
&OPT_LIST SETC ''
.*       AWHILE (&I LE &NP)
.AWH_62_T  ANOP
           AIF    (NOT(&I LE &NP)).AWH_62_E
&OPTION        SETC (UPPER '&OPTIONS(&I)')
.*             AIF ('&OPTION' EQ 'CICS')    GEN DFHEISTG CSECT
                 AIF (NOT('&OPTION' EQ 'CICS')).AIF_471_1     GEN DFHEIX
               STG CSECT
&ZC_CICS           SETB 1
&OPT_LIST          SETC '&OPT_LIST,&OPTION'
.*             AELSEIF ('&OPTION' EQ 'COMMENT') GEN CBL SOURCE COMMENTS
                 AGO   .AIF_471_E
.AIF_471_1       ANOP
                 AIF     (NOT('&OPTION' EQ 'COMMENT')).AIF_471_2  GEN CX
               BL SOURCE COMMENTS
&ZC_COMMENT        SETB 1
&OPT_LIST          SETC '&OPT_LIST,&OPTION'
.*             AELSEIF ('&OPTION'(1,6) EQ 'FLOAT(') FLOAT(DFP/BFP/HFP)
                 AGO   .AIF_471_E
.AIF_471_2       ANOP
                 AIF     (NOT('&OPTION'(1,6) EQ 'FLOAT(') FLOAT(DFP/BFPX
               /HFP)).AIF_471_3
&ZC_FLOAT          SETC '&OPTION'(7,K'&OPTION-7)
&OPT_LIST          SETC '&OPT_LIST,&OPTION'
.*                 AIF ('&ZC_FLOAT' EQ 'DECIMAL')
                     AIF (NOT('&ZC_FLOAT' EQ 'DECIMAL')).AIF_472_1
&ZC_FLOAT_SHORT_TYPE   SETC '7'
&ZC_FLOAT_LONG_TYPE    SETC '8'
&ZC_FLOAT_EXTENDED_TYPE SETC '9'
.*                 AELSEIF ('&ZC_FLOAT' EQ 'HEX')
                     AGO   .AIF_472_E
.AIF_472_1           ANOP
                     AIF     (NOT('&ZC_FLOAT' EQ 'HEX')).AIF_472_2
&ZC_FLOAT_SHORT_TYPE   SETC '1'
&ZC_FLOAT_LONG_TYPE    SETC '2'
&ZC_FLOAT_EXTENDED_TYPE SETC '3'
.*                 AELSEIF ('&ZC_FLOAT' EQ 'BINARY')
                     AGO   .AIF_472_E
.AIF_472_2           ANOP
                     AIF     (NOT('&ZC_FLOAT' EQ 'BINARY')).AIF_472_3
&ZC_FLOAT_SHORT_TYPE   SETC '4'
&ZC_FLOAT_LONG_TYPE    SETC '5'
&ZC_FLOAT_EXTENDED_TYPE SETC '6'
.*                 AELSE
                     AGO   .AIF_472_E
.AIF_472_3           ANOP
                       MNOTE 8,'ZCOBOL INVALID FLOAT TYPE - &ZC_FLOAT'
.*                 AEND
.AIF_472_E           ANOP
.*             AELSEIF ('&OPTION' EQ 'EXTEND')  ALLOW Q34 AND P31/Z31
                 AGO   .AIF_471_E
.AIF_471_3       ANOP
                 AIF     (NOT('&OPTION' EQ 'EXTEND')).AIF_471_4   ALLOWX
                Q34 AND P31/Z31
&ZC_EXTEND         SETB 1
&OPT_LIST          SETC '&OPT_LIST,&OPTION'
.*             AELSEIF ('&OPTION' EQ 'NOR64')  TURN OFF 64 BIT REG CODE
                 AGO   .AIF_471_E
.AIF_471_4       ANOP
                 AIF     (NOT('&OPTION' EQ 'NOR64')).AIF_471_5   TURN OX
               FF 64 BIT REG CODE
&ZC_R64            SETB 0
&OPT_LIST          SETC '&OPT_LIST,&OPTION'
.*             AELSEIF ('&OPTION' EQ 'TRACE')   TRACE PARAGRAPHS ON LOG
                 AGO   .AIF_471_E
.AIF_471_5       ANOP
                 AIF     (NOT('&OPTION' EQ 'TRACE')).AIF_471_6    TRACEX
                PARAGRAPHS ON LOG
&ZC_TRACE          SETB 1
&OPT_LIST          SETC '&OPT_LIST,&OPTION'
.*             AELSEIF ('&OPTION' EQ 'TRUNC')   TRUNCATE NUM TO PIC(N)
                 AGO   .AIF_471_E
.AIF_471_6       ANOP
                 AIF     (NOT('&OPTION' EQ 'TRUNC')   TRUNCATE NUM TO PX
               IC(N)).AIF_471_7
&ZC_TRUNC          SETB 1
&OPT_LIST          SETC '&OPT_LIST,&OPTION'
.*             AELSEIF ('&OPTION' EQ 'WARN')    ISSUE M NOTE LVL 4
                 AGO   .AIF_471_E
.AIF_471_7       ANOP
                 AIF     (NOT('&OPTION' EQ 'WARN')).AIF_471_8     ISSUEX
                M NOTE LVL 4
&ZC_WARN           SETB 1
&OPT_LIST          SETC '&OPT_LIST,&OPTION'
.*             AELSEIF ('&OPTION' EQ 'NOCICS')  NO EXEC CICS
                 AGO   .AIF_471_E
.AIF_471_8       ANOP
                 AIF     (NOT('&OPTION' EQ 'NOCICS')).AIF_471_9   NO EXX
               EC CICS
&ZC_CICS           SETB 0
&OPT_LIST          SETC '&OPT_LIST,&OPTION'
.*             AELSEIF ('&OPTION' EQ 'NOCOMMENT') NO SOURCE COMMENTS
                 AGO   .AIF_471_E
.AIF_471_9       ANOP
                 AIF     (NOT('&OPTION' EQ 'NOCOMMENT')).AIF_471_10  NOX
                SOURCE COMMENTS
&ZC_COMMENT        SETB 0
&OPT_LIST          SETC '&OPT_LIST,&OPTION'
.*             AELSEIF ('&OPTION' EQ 'NOEXTEND')  NO G64 AND P31 COMPS
                 AGO   .AIF_471_E
.AIF_471_10      ANOP
                 AIF     (NOT('&OPTION' EQ 'NOEXTEND')).AIF_471_11   NOX
                G64 AND P31 COMPS
&ZC_EXTEND         SETB 0
&OPT_LIST          SETC '&OPT_LIST,&OPTION'
.*             AELSEIF ('&OPTION' EQ 'NOTRACE') NO TRACE PARAGRAPHS
                 AGO   .AIF_471_E
.AIF_471_11      ANOP
                 AIF     (NOT('&OPTION' EQ 'NOTRACE')).AIF_471_12  NO TX
               RACE PARAGRAPHS
&ZC_TRACE          SETB 0
&OPT_LIST          SETC '&OPT_LIST,&OPTION'
.*             AELSEIF ('&OPTION' EQ 'NOTRUNC') NO TRUNCATE NUM TO PIC
                 AGO   .AIF_471_E
.AIF_471_12      ANOP
                 AIF     (NOT('&OPTION' EQ 'NOTRUNC')).AIF_471_13  NO TX
               RUNCATE NUM TO PIC
&ZC_TRUNC          SETB 0
&OPT_LIST          SETC '&OPT_LIST,&OPTION'
.*             AELSEIF ('&OPTION' EQ 'NOWARN') NO M NOTE LVL 4 WARNINGS
                 AGO   .AIF_471_E
.AIF_471_13      ANOP
                 AIF     (NOT('&OPTION' EQ 'NOWARN')).AIF_471_14  NO M X
               NOTE LVL 4 WARNINGS
&ZC_WARN           SETB 0
&OPT_LIST          SETC '&OPT_LIST,&OPTION'
.*             AEND
.AIF_471_14      ANOP
.AIF_471_E       ANOP
&I             SETA &I+1
.*       AEND
           AGO   .AWH_62_T
.AWH_62_E  ANOP
.*       AIF   (K'&OPT_LIST GT 0)
           AIF   (NOT(K'&OPT_LIST GT 0)).AIF_473_1
&OPT_LIST      SETC '&OPT_LIST'(2,*)
.*       AEND
.AIF_473_1 ANOP
         COPY  ZC_KEYS
     GEN_COMMENT 'ZCOBOL PGM=&ZC_PGM VER=&ZC_VER &ZC_CDATE &ZC_CTIME'
     GEN_COMMENT 'ZCOBOL OPTIONS=(&OPT_LIST)'
         MEND
         MACRO
.*********************************************************************
.* Copyright 2008 Automated Software Tools Corporation               *
.* This source code is part of z390 zCOBOL/assembler/emulator package*
.* The z390 package is distributed under GNU general public license  *
.* Author - Don Higgins                                              *
.* Date   - 04/16/08                                                 *
.*********************************************************************
.* 07/19/09 RPI 1065 stub for pending support ZCSD REPLACING SD
.*********************************************************************
         ZCSD
         MNOTE 8,'SD SORT DEFINITION NOT SUPPORTED YET'
         MEXIT
         MEND
         MACRO
.*********************************************************************
.* Copyright 2008 Automated Software Tools Corporation               *
.* This source code is part of z390 zCOBOL/assembler/emulator package*
.* The z390 package is distributed under GNU general public license  *
.* Author - Don Higgins                                              *
.* Date   - 07/16/09                                                 *
.*********************************************************************
.* 07/16/09 RPI 1065 stub for START verb mapped to ZCSTART by ZC390
.*********************************************************************
         SORT
         MNOTE 8,'START NOT SUPPORTED YET'
         MEXIT
         MEND
        MACRO
.*********************************************************************
.* Copyright 2009 Automated Software Tools Corporation               *
.* This source code is part of z390 zCOBOL/assembler/emulator package*
.* The z390 package is distributed under GNU general public license  *
.* Author - Don Higgins                                              *
.* Date   - 03/04/09                                                 *
.*********************************************************************
.* 04/24/09 RPI 1003 initial coding for ZC_CALC using IF as model
.* 07/30/09 RPI 1065 FUNCTION not supported yet (see IF101A)
.*********************************************************************
.* ZC_CALC used by COMPUTE to gen code to calculate expression
.* defined by positional parm list.  The result is stored in data
.* field defined by T_NAME and positive T_IX or by temporary storage
.* field defined in ZCVT using negative T_IX to fetch characteristics
.* from global tables &TMP_NAME/TYPE/LEN/SIGN
.*********************************************************************
         ZC_CALC &T_NAME,&T_IX,&EXP
         COPY  ZC_WS
&ROUNDED SETB  0
&ONSIZE  SETB  0
&PREV_OP# SETA 0 PREV OP# ON STK OR 0 FOR NONE
&TMP_OFF  SETA 0 OFFSET TO NEXT TEMP STORE IN ZCVT_WORKAREA
.*
.* PUSH/POP VAR/LIT lNDEX AND OPERATOR STACKS FOR PARSING
.*
         LCLA  &STK_VAR_TOT      CUR TOTAL VAR/IOP IX'S ON STACK
         LCLA  &STK_VAR_IX(100)  VAR IX,-TMP IX, OR 0 FOR LIT VAL
         LCLA  &STK_OP_TOT       CUR TOTAL OP ON STACK
         LCLA  &STK_OP(10)       OP 1-6  '+-*/()'
         LCLA  &VAR_IOP_IX       POS PARM IX,NEG TMP IX, OR 0 FOR LIT
.*
.* GENERATED INTERMEDIATE OPCODE TABLE
.*
         LCLA  &IOP_TOT       TOTAL INTERMEDIATE LOG. OPS
         LCLA  &IOP_OP(100)   LOG. OPER. (+=1,-=2,*=3,/=4)
         LCLA  &IOP_F1_X(100) +PARM1 IX OR -TMP IX
         LCLA  &IOP_F2_X(100) +PARM2 IX OR -TMP IX
         LCLA  &IOP_F3_X(100) +TMP   IX FOR RESULT
.*
.*
.*
         LCLA  &MAX_TMP          MAX TMP ALLOCATED
         LCLA  &TMP_OFF          OFFSET FOR TMP FIELD
         LCLA  &CUR_TMP_FREE     CURRENT FREE TMP
         LCLA  &TMP_FREE_IX(100) CURRENT FREE TMP INDEXES 1-N
.*
.*  BUILD IOP'S AND THEN GEN CODE
.*
.*       ACALL IOP_BUILD
&ACALL_15_IOP_BUILD SETA    2
           AGO   .ACL_15_IOP_BUILD
.ACL_15_2  ANOP
.*  ACALL LIST_IOPS   DEBUG
.*       ACALL IOP_GEN_CODE
&ACALL_17_IOP_GEN_CODE SETA    2
           AGO   .ACL_17_IOP_GEN_CODE
.ACL_17_2  ANOP
         MEXIT
.*
.* BUILD IOP TABLE USING BNF PARM AND OPERATOR STACKS
.*   1.  PUSH +PARM PTRS AND -IOP PTRS ON STK_VAR
.*   2.  PUSH OPERATORS ON STK_OP (+-*/() = 123456)
.*   3.  POP  OP AND 2 VAR IX'S TO GEN IOP
.*       WHEN OP PRECEDENCE DICTATES
.*
.*       AENTRY IOP_BUILD
 MNOTE 8'ZSTRMAC ERROR AENTRY DUPLICATE NAME FOUND - IOP_BUILD
&NP      SETA  N'&SYSLIST  TOTAL EXP PARMS
.*       AIF (&NP EQ 0)
           AIF (NOT(&NP EQ 0)).AIF_474_1
             MNOTE 8,'ZC_CALC MISSING EXPRESSION'
             MEXIT
.*       AEND
.AIF_474_1 ANOP
&LVL     SETA  0           CUR (..) LEVEL WITHIN IF PARMS
&IX_PARM_TOT SETA 0     CUR CONDITION PARM COUNT (A EQ B ETC)
&STK_OP_TOT SETA 0      RESET LOGICAL IOP OPERATOR STACK
&PARM_IX SETA  2        GET PARM INDEX (SKIP TARGET/INDEX)
&PREV_PARM SETC ''      PREV PARM
&PARM    SETC ''           CUR  PARM
&SKIP_SIX SETB 0      SKIP SUBSCRIPTS IN CD'S
.*       ACALL  GET_PARM          GET NEXT PARM OR ''
&ACALL_18_GET_PARM SETA    6
           AGO   .ACL_18_GET_PARM
.ACL_18_6  ANOP
.*       AWHILE ('&PARM' NE '')
.AWH_63_T  ANOP
           AIF    (NOT('&PARM' NE '')).AWH_63_E
.*             AIF ('&PARM' EQ '''(''')
                 AIF (NOT('&PARM' EQ '''(''')).AIF_475_1
&LVL               SETA &LVL+1
&OP#               SETA 5  OP_OPEN
.*                 ACALL PUSH_OP      PUSH OP 5=(
&ACALL_19_PUSH_OP    SETA    5
                     AGO   .ACL_19_PUSH_OP
.ACL_19_5            ANOP
.*             AELSEIF ('&PARM' EQ ''')''')  OP_CLOSE
                 AGO   .AIF_475_E
.AIF_475_1       ANOP
                 AIF     (NOT('&PARM' EQ ''')''')).AIF_475_2   OP_CLOSE
&LVL               SETA &LVL-1
.*                 AIF (&STK_OP_TOT GT 0)
                     AIF (NOT(&STK_OP_TOT GT 0)).AIF_476_1
.*                     AWHILE (&STK_OP(&STK_OP_TOT) NE 5) FLUSH (..)
.AWH_64_T                ANOP
                         AIF    (NOT(&STK_OP(&STK_OP_TOT) NE 5) FLUSH (X
               ..)).AWH_64_E
.*                         ACALL IOP_ADD_OP
&ACALL_74_IOP_ADD_OP         SETA    1
                             AGO   .ACL_74_IOP_ADD_OP
.ACL_74_1                    ANOP
.*                     AEND
                         AGO   .AWH_64_T
.AWH_64_E                ANOP
.*                     ACALL POP_OP  REMOVE ( FOR MATCHING )
&ACALL_20_POP_OP         SETA    9
                         AGO   .ACL_20_POP_OP
.ACL_20_9                ANOP
.*                 AELSE
                     AGO   .AIF_476_E
.AIF_476_1           ANOP
                       MNOTE 8,'ZC_CALC UNBALANCED (..)'
                       MEXIT
.*                 AEND
.AIF_476_E           ANOP
.*             AELSEIF ('&PARM' EQ '+' OR '&PARM' EQ '-')
                 AGO   .AIF_475_E
.AIF_475_2       ANOP
                 AIF     (NOT('&PARM' EQ '+' OR '&PARM' EQ '-')).AIF_47X
               5_3
.*                 AWHILE (&PREV_OP# EQ 1                              X
                           OR &PREV_OP# EQ 2                           X
                           OR &PREV_OP# EQ 3                           X
                           OR &PREV_OP# EQ 4)
.AWH_65_T            ANOP
                     AIF    (NOT(&PREV_OP# EQ 1                        X
                                 OR &PREV_OP# EQ 2                     X
                                 OR &PREV_OP# EQ 3                     X
                                 OR &PREV_OP# EQ 4)).AWH_65_E
.*                     ACALL IOP_ADD_OP
&ACALL_74_IOP_ADD_OP     SETA    2
                         AGO   .ACL_74_IOP_ADD_OP
.ACL_74_2                ANOP
.*                 AEND
                     AGO   .AWH_65_T
.AWH_65_E            ANOP
.*                 AIF ('&PARM' EQ '+')
                     AIF (NOT('&PARM' EQ '+')).AIF_477_1
&OP#                   SETA 1
.*                 AELSE
                     AGO   .AIF_477_E
.AIF_477_1           ANOP
&OP#                   SETA 2
.*                 AEND
.AIF_477_E           ANOP
.*                 ACALL PUSH_OP      PUSH OP 5=(
&ACALL_19_PUSH_OP    SETA    6
                     AGO   .ACL_19_PUSH_OP
.ACL_19_6            ANOP
.*             AELSEIF ('&PARM' EQ '*' OR '&PARM' EQ '/')
                 AGO   .AIF_475_E
.AIF_475_3       ANOP
                 AIF     (NOT('&PARM' EQ '*' OR '&PARM' EQ '/')).AIF_47X
               5_4
.*                 AWHILE (&PREV_OP# EQ 3 OR &PREV_OP# EQ 4)
.AWH_66_T            ANOP
                     AIF    (NOT(&PREV_OP# EQ 3 OR &PREV_OP# EQ 4)).AWHX
               _66_E
.*                     ACALL IOP_ADD_OP
&ACALL_74_IOP_ADD_OP     SETA    3
                         AGO   .ACL_74_IOP_ADD_OP
.ACL_74_3                ANOP
.*                 AEND
                     AGO   .AWH_66_T
.AWH_66_E            ANOP
.*                 AIF ('&PARM' EQ '*')
                     AIF (NOT('&PARM' EQ '*')).AIF_478_1
&OP#                   SETA 3 OP_MPY
.*                 AELSE
                     AGO   .AIF_478_E
.AIF_478_1           ANOP
&OP#                   SETA 4 OP_DIV
.*                 AEND
.AIF_478_E           ANOP
.*                 ACALL PUSH_OP      PUSH OP 5=(
&ACALL_19_PUSH_OP    SETA    7
                     AGO   .ACL_19_PUSH_OP
.ACL_19_7            ANOP
.*             AELSEIF ('&PARM' EQ 'FUNCTION')
                 AGO   .AIF_475_E
.AIF_475_4       ANOP
                 AIF     (NOT('&PARM' EQ 'FUNCTION')).AIF_475_5
                   MNOTE 8,'ZC_CALC FUNCTION NOT SUPPORTED YET'
                   MEXIT
.*             AELSE
                 AGO   .AIF_475_E
.AIF_475_5       ANOP
&VAR_IOP_IX        SETA &PARM_IX
.*                 ACALL PUSH_VAR   PUSH 1ST PARM PTR FOR VAR
&ACALL_75_PUSH_VAR   SETA    1
                     AGO   .ACL_75_PUSH_VAR
.ACL_75_1            ANOP
&SKIP_SIX          SETB 1
.*             AEND
.AIF_475_E       ANOP
.*             ACALL GET_PARM
&ACALL_18_GET_PARM SETA    7
                 AGO   .ACL_18_GET_PARM
.ACL_18_7        ANOP
.*             AIF  (&SKIP_SIX) SKIP SUBSCRIPTS FOR VAR'S
                 AIF  (NOT(&SKIP_SIX)).AIF_479_1  SKIP SUBSCRIPTS FOR VX
               AR'S
&SKIP_SIX           SETB 0
.*                  AWHILE ('&PARM' EQ '''(''') SKIP SIX AND RMOD
.AWH_67_T             ANOP
                      AIF    (NOT('&PARM' EQ '''(''')).AWH_67_E  SKIP SX
               IX AND RMOD
.*                      ACALL GET_PARM
&ACALL_18_GET_PARM        SETA    8
                          AGO   .ACL_18_GET_PARM
.ACL_18_8                 ANOP
.*                      AWHILE ('&PARM' NE '' AND '&PARM' NE ''')''')
.AWH_68_T                 ANOP
                          AIF    (NOT('&PARM' NE '' AND '&PARM' NE ''')X
               ''')).AWH_68_E
.*                          ACALL GET_PARM
&ACALL_18_GET_PARM            SETA    9
                              AGO   .ACL_18_GET_PARM
.ACL_18_9                     ANOP
.*                      AEND
                          AGO   .AWH_68_T
.AWH_68_E                 ANOP
.*                      AIF ('&PARM' NE '')
                          AIF (NOT('&PARM' NE '')).AIF_480_1
.*                          ACALL GET_PARM
&ACALL_18_GET_PARM            SETA    10
                              AGO   .ACL_18_GET_PARM
.ACL_18_10                    ANOP
.*                      AEND
.AIF_480_1                ANOP
.*                  AEND
                      AGO   .AWH_67_T
.AWH_67_E             ANOP
.*             AEND
.AIF_479_1       ANOP
.*       AEND
           AGO   .AWH_63_T
.AWH_63_E  ANOP
.*
.* FLUSH REMAINING OPS IF ANY AT END OR PARMS
.*
.*       AWHILE (&STK_OP_TOT GT 0)
.AWH_69_T  ANOP
           AIF    (NOT(&STK_OP_TOT GT 0)).AWH_69_E
.*            ACALL IOP_ADD_OP
&ACALL_74_IOP_ADD_OP SETA    4
                AGO   .ACL_74_IOP_ADD_OP
.ACL_74_4       ANOP
.*       AEND
           AGO   .AWH_69_T
.AWH_69_E  ANOP
.*       AEND
 MNOTE 8'ZSTRMAC ERROR AEND MISSING AIF OR OTHER STRUCTURE
.*
.* GEN CODE FROM IOPS
.*
.*       AENTRY IOP_GEN_CODE
 MNOTE 8'ZSTRMAC ERROR AENTRY DUPLICATE NAME FOUND - IOP_GEN_CODE
.*
.* GEN SINGLE IOP FROM STACK IF NO IOPS
.*
.*
.* GEN CODE FROM IOP TABLE WITH MULTIPLE ENTRIES
.*
&I       SETA 1
.*       AWHILE (&I LE &IOP_TOT)
.AWH_70_T  ANOP
           AIF    (NOT(&I LE &IOP_TOT)).AWH_70_E
.*             ACALL IOP_GEN_OP
&ACALL_76_IOP_GEN_OP SETA    1
                 AGO   .ACL_76_IOP_GEN_OP
.ACL_76_1        ANOP
&I             SETA &I+1
.*       AEND
           AGO   .AWH_70_T
.AWH_70_E  ANOP
.*       AEND
 MNOTE 8'ZSTRMAC ERROR AEND MISSING AIF OR OTHER STRUCTURE
.*
.* IOP ADD OP - ADD OP ON STK TO IOP TABLE
.*
.*       AENTRY IOP_ADD_OP
           AGO   .ACL_74_SKIP
.ACL_74_IOP_ADD_OP ANOP
.*       ACALL POP_OP
&ACALL_20_POP_OP SETA    10
           AGO   .ACL_20_POP_OP
.ACL_20_10 ANOP
&IOP_TOT SETA &IOP_TOT+1
&IOP_OP(&IOP_TOT) SETA &OP#  (1-4 = ADD,SUB,MPY,DIV)
.*       ACALL POP_VAR_IOP_IX
&ACALL_77_POP_VAR_IOP_IX SETA    1
           AGO   .ACL_77_POP_VAR_IOP_IX
.ACL_77_1  ANOP
&IOP_F2_X(&IOP_TOT) SETA &VAR_IOP_IX
.*       ACALL POP_VAR_IOP_IX
&ACALL_77_POP_VAR_IOP_IX SETA    2
           AGO   .ACL_77_POP_VAR_IOP_IX
.ACL_77_2  ANOP
&IOP_F1_X(&IOP_TOT) SETA &VAR_IOP_IX
.*       ACALL GET_TMP
&ACALL_78_GET_TMP SETA    1
           AGO   .ACL_78_GET_TMP
.ACL_78_1  ANOP
&IOP_F3_X(&IOP_TOT) SETA &TMP_IX
&VAR_IOP_IX SETA -&TMP_IX
.*       ACALL PUSH_VAR
&ACALL_75_PUSH_VAR SETA    2
           AGO   .ACL_75_PUSH_VAR
.ACL_75_2  ANOP
.*       AIF  (&IOP_F1_X(&IOP_TOT) LT 0)
           AIF  (NOT(&IOP_F1_X(&IOP_TOT) LT 0)).AIF_481_1
&TMP_IX       SETA -&IOP_F1_X(&IOP_TOT)
.*            ACALL FREE_TMP
&ACALL_79_FREE_TMP SETA    1
                AGO   .ACL_79_FREE_TMP
.ACL_79_1       ANOP
.*       AEND
.AIF_481_1 ANOP
.*       AIF  (&IOP_F2_X(&IOP_TOT) LT 0)
           AIF  (NOT(&IOP_F2_X(&IOP_TOT) LT 0)).AIF_482_1
&TMP_IX       SETA -&IOP_F2_X(&IOP_TOT)
.*            ACALL FREE_TMP
&ACALL_79_FREE_TMP SETA    2
                AGO   .ACL_79_FREE_TMP
.ACL_79_2       ANOP
.*       AEND
.AIF_482_1 ANOP
.*       AEND
           AGO   (&ACALL_74_IOP_ADD_OP).ACL_74_1,.ACL_74_2,.ACL_74_3,.AX
               CL_74_4
.ACL_74_SKIP ANOP
.*
.* IOP GEN OP
.*
.*       AENTRY IOP_GEN_OP
           AGO   .ACL_76_SKIP
.ACL_76_IOP_GEN_OP ANOP
.*       ACASE (&IOP_OP(&I))
           AGO   .ACS_4_G
.*       AWHEN 1  OP_ADD
.ACS_4_1   ANOP
.*           ACALL IOP_GEN_ADD
&ACALL_80_IOP_GEN_ADD SETA    1
               AGO   .ACL_80_IOP_GEN_ADD
.ACL_80_1      ANOP
.*       AWHEN 2  OP_SUB
           AGO   .ACS_4_E
.ACS_4_2   ANOP
.*           ACALL IOP_GEN_SUB
&ACALL_81_IOP_GEN_SUB SETA    1
               AGO   .ACL_81_IOP_GEN_SUB
.ACL_81_1      ANOP
.*       AWHEN 3  OP_MPY
           AGO   .ACS_4_E
.ACS_4_3   ANOP
.*           ACALL IOP_GEN_MPY
&ACALL_82_IOP_GEN_MPY SETA    1
               AGO   .ACL_82_IOP_GEN_MPY
.ACL_82_1      ANOP
.*       AWHEN 4  OP_DIV
           AGO   .ACS_4_E
.ACS_4_4   ANOP
.*           ACALL IOP_GEN_DIV
&ACALL_83_IOP_GEN_DIV SETA    1
               AGO   .ACL_83_IOP_GEN_DIV
.ACL_83_1      ANOP
.*       AELSE
           AGO   .ACS_4_E
.ACS_4_X   ANOP
             MNOTE 8,'ZC_CALC IOP INVALID OPCODE &IOP_OP(&I)'
             MEXIT
.*       AEND
           AGO   .ACS_4_E
.ACS_4_G   ANOP
           AGO   (&IOP_OP(&I)).ACS_4_1,.ACS_4_2,.ACS_4_3,.ACS_4_4
           AGO   .ACS_4_X
.ACS_4_E   ANOP
.*       AEND
           AGO   (&ACALL_76_IOP_GEN_OP).ACL_76_1
.ACL_76_SKIP ANOP
.*
.* POP VAR_IOP AND SET VAR_IOP_IX FROM TOP OF STK_VAR
.*
.*       AENTRY POP_VAR_IOP_IX
           AGO   .ACL_77_SKIP
.ACL_77_POP_VAR_IOP_IX ANOP
.*       AIF  (&STK_VAR_TOT GT 0)
           AIF  (NOT(&STK_VAR_TOT GT 0)).AIF_483_1
&VAR_IOP_IX   SETA &STK_VAR(&STK_VAR_TOT) +VAR,-TMP
&STK_VAR_TOT  SETA &STK_VAR_TOT-1
.*       AELSE
           AGO   .AIF_483_E
.AIF_483_1 ANOP
              MNOTE 8,'ZC_CALC MISSING VAR STACK PTR'
              CALL LIST_IOPS
              MEXIT
.*       AEND
.AIF_483_E ANOP
.*       AEND
           AGO   (&ACALL_77_POP_VAR_IOP_IX).ACL_77_1,.ACL_77_2
.ACL_77_SKIP ANOP
.*
.* IOP GEN ADD
.*
.*       AENTRY IOP_GEN_ADD
           AGO   .ACL_80_SKIP
.ACL_80_IOP_GEN_ADD ANOP
.*       ACALL CALC_SETUP
&ACALL_84_CALC_SETUP SETA    1
           AGO   .ACL_84_CALC_SETUP
.ACL_84_1  ANOP
         GEN_ADD &F3_N,&F3_XC,&F2_N,&F2_XC,&ROUNDED,&ONSIZE
.*       AEND
           AGO   (&ACALL_80_IOP_GEN_ADD).ACL_80_1
.ACL_80_SKIP ANOP
.*
.* IOP GEN SUB
.*
.*       AENTRY IOP_GEN_SUB
           AGO   .ACL_81_SKIP
.ACL_81_IOP_GEN_SUB ANOP
.*       ACALL CALC_SETUP
&ACALL_84_CALC_SETUP SETA    2
           AGO   .ACL_84_CALC_SETUP
.ACL_84_2  ANOP
         GEN_SUB &F3_N,&F3_XC,&F2_N,&F2_XC,&ROUNDED,&ONSIZE
.*       AEND
           AGO   (&ACALL_81_IOP_GEN_SUB).ACL_81_1
.ACL_81_SKIP ANOP
.*
.* IOP GEN MPY
.*
.*       AENTRY IOP_GEN_MPY
           AGO   .ACL_82_SKIP
.ACL_82_IOP_GEN_MPY ANOP
.*       ACALL CALC_SETUP
&ACALL_84_CALC_SETUP SETA    3
           AGO   .ACL_84_CALC_SETUP
.ACL_84_3  ANOP
         GEN_MPY &F3_N,&F3_XC,&F2_N,&F2_XC,&ROUNDED,&ONSIZE
.*       AEND
           AGO   (&ACALL_82_IOP_GEN_MPY).ACL_82_1
.ACL_82_SKIP ANOP
.*
.* IOP GEN DIV
.*
.*       AENTRY IOP_GEN_DIV
           AGO   .ACL_83_SKIP
.ACL_83_IOP_GEN_DIV ANOP
.*       ACALL CALC_SETUP
&ACALL_84_CALC_SETUP SETA    4
           AGO   .ACL_84_CALC_SETUP
.ACL_84_4  ANOP
         GEN_DIV &F1_N,&F1_T,&F1_L,0,      DIVIDEND                    X
               &F2_N,&F2_T,&F2_L,0,        DIVISOR                     X
               &F3_N,&F3_T,&F3_L,&F3_S,0,  QUOTIENT                    X
               ,,0,0,0,                    NO REMAINDER                X
               &ROUNDED,&ONSIZE
.*       AEND
           AGO   (&ACALL_83_IOP_GEN_DIV).ACL_83_1
.ACL_83_SKIP ANOP
.*
.*
.* CALC_SETUP
.*
.*     1.  GEN SIX FOR F1 ABD F2 FIELDS IF ANY
.*     2.  SET TMP_NAME/TYPE/LEN/SIGN TARGET FIELD FROM F1/F2 TYPES
.*     3.  GEN MOVE F1 TO TARGET FIELD IF REQUIRED FOR ADD,SUB,MPY
.*
.*       AENTRY CALC_SETUP
           AGO   .ACL_84_SKIP
.ACL_84_CALC_SETUP ANOP
.*       AIF   (&IOP_F1_X(&I) GE 0)
           AIF   (NOT(&IOP_F1_X(&I) GE 0)).AIF_484_1
&VAR_PARM_IX   SETA &IOP_F1_X(&I)-1
&VAR_PREV_PARM SETC ''
&FIELD_SS1     SETB 1
&FIELD_REG     SETC 'ZC_R2'
.*             ACALL  GET_VAR_PARM   GEN SIX CODE FOR VAR IF ANY
&ACALL_85_GET_VAR_PARM SETA    1
                 AGO   .ACL_85_GET_VAR_PARM
.ACL_85_1        ANOP
&F1_N          SETC '&FIELD_NAME'
&F1_X          SETA &FIELD_IX
&F1_XC         SETC '&F1_X'
.*             AIF (&F1_X GT 0)
                 AIF (NOT(&F1_X GT 0)).AIF_485_1
&F1_T              SETC '&SYM_PIC_TYPE(&F1_X)'
&F1_L              SETA &SYM_LEN(&F1_X)
.*             AELSE
                 AGO   .AIF_485_E
.AIF_485_1       ANOP
&F1_X              SETA 0
&F1_XC             SETC '0'
.*                 ACALL SET_LIT_TYPE_LEN
&ACALL_86_SET_LIT_TYPE_LEN SETA    1
                     AGO   .ACL_86_SET_LIT_TYPE_LEN
.ACL_86_1            ANOP
&F1_T              SETC '&LIT_TYPE'
&F1_L              SETA &LIT_LEN
.*             AEND
.AIF_485_E       ANOP
.*       AELSE
           AGO   .AIF_484_E
.AIF_484_1 ANOP
&IOP           SETA -&IOP_F1_X(&I)
&F1_N          SETC '&TMP_NAME(&IOP)'
&F1_T          SETC '&TMP_TYPE(&IOP)'
&F1_L          SETA &TMP_LEN(&IOP)
&F1_X          SETA -&IOP
&F1_XC         SETC '-&IOP'
.*       AEND
.AIF_484_E ANOP
.*       AIF   (&IOP_F2_X(&I) GE 0)
           AIF   (NOT(&IOP_F2_X(&I) GE 0)).AIF_486_1
&VAR_PARM_IX   SETA &IOP_F2_X(&I)-1
&VAR_PREV_PARM SETC ''
&FIELD_SS1     SETB 1
&FIELD_REG     SETC 'ZC_R3'
.*             ACALL  GET_VAR_PARM   GEN SIX CODE FOR VAR IF ANY
&ACALL_85_GET_VAR_PARM SETA    2
                 AGO   .ACL_85_GET_VAR_PARM
.ACL_85_2        ANOP
&F2_N          SETC '&FIELD_NAME'
&F2_X          SETA &FIELD_IX
&F2_XC         SETC '&F2_X'
.*             AIF (&F2_X GT 0)
                 AIF (NOT(&F2_X GT 0)).AIF_487_1
&F2_T              SETC '&SYM_PIC_TYPE(&F2_X)'
&F2_L              SETA &SYM_LEN(&F2_X)
.*             AELSE
                 AGO   .AIF_487_E
.AIF_487_1       ANOP
&F2_X              SETA 0
&F2_XC             SETC '0'
.*                 ACALL SET_LIT_TYPE_LEN
&ACALL_86_SET_LIT_TYPE_LEN SETA    2
                     AGO   .ACL_86_SET_LIT_TYPE_LEN
.ACL_86_2            ANOP
&F2_T              SETC '&LIT_TYPE'
&F2_L              SETA &LIT_LEN
.*             AEND
.AIF_487_E       ANOP
.*       AELSE
           AGO   .AIF_486_E
.AIF_486_1 ANOP
&IOP           SETA -&IOP_F2_X(&I)
&F2_N          SETC '&TMP_NAME(&IOP)'
&F2_T          SETC '&TMP_TYPE(&IOP)'
&F2_L          SETA &TMP_LEN(&IOP)
&F2_X          SETA -&IOP
&F2_XC         SETC '-&IOP'
.*       AEND
.AIF_486_E ANOP
.*       AIF   (&I LT &IOP_TOT)
           AIF   (NOT(&I LT &IOP_TOT)).AIF_488_1
&TMP_IX        SETA &IOP_F3_X(&I)
&F3_X          SETA -&TMP_IX
&F3_XC         SETC '-&TMP_IX'
.*             ACALL CALC_F3_TYPE
&ACALL_87_CALC_F3_TYPE SETA    1
                 AGO   .ACL_87_CALC_F3_TYPE
.ACL_87_1        ANOP
&F3_N          SETC '&TMP_NAME(&TMP_IX)(&F3_L)'
&TMP_TYPE(&TMP_IX) SETC '&F3_T'
&TMP_LEN(&TMP_IX) SETA '&F3_L'
&F3_S          SETB 1
&TMP_SIGN(&TMP_IX) SETB &F3_S
.*             AIF (&IOP_OP(&I) NE 4)  MOVE 1ST OP TO TEMP IF NOT DIV
                 AIF (NOT(&IOP_OP(&I) NE 4)).AIF_489_1   MOVE 1ST OP TOX
                TEMP IF NOT DIV
&LAB               SETC '&F1_N'
.*                 ACALL RESET_LAB_LEN
&ACALL_88_RESET_LAB_LEN SETA    1
                     AGO   .ACL_88_RESET_LAB_LEN
.ACL_88_1            ANOP
                   GEN_MOVE &F3_N,&F3_XC,&LAB,&F1_XC
.*             AEND
.AIF_489_1       ANOP
.*       AELSE
           AGO   .AIF_488_E
.AIF_488_1 ANOP
&F3_N          SETC '&T_NAME'
&F3_X          SETA &T_IX
&F3_XC         SETC '&F3_X'
&F3_T          SETC '&SYM_PIC_TYPE(&T_IX)'
&F3_L          SETA &SYM_LEN(&T_IX)
&F3_S          SETB &SYM_PIC_SIGN(&T_IX)
.*             AIF  ('&F3_N' NE '&F1_N')
                 AIF  (NOT('&F3_N' NE '&F1_N')).AIF_490_1
&LAB               SETC '&F1_N'
.*                 ACALL RESET_LAB_LEN
&ACALL_88_RESET_LAB_LEN SETA    2
                     AGO   .ACL_88_RESET_LAB_LEN
.ACL_88_2            ANOP
                   GEN_MOVE &F3_N,&F3_XC,&LAB,&F1_XC
.*             AEND
.AIF_490_1       ANOP
.*       AEND
.AIF_488_E ANOP
.*       AEND
           AGO   (&ACALL_84_CALC_SETUP).ACL_84_1,.ACL_84_2,.ACL_84_3,.AX
               CL_84_4
.ACL_84_SKIP ANOP
.*
.* GET TMP - SET TMP_IX TO FREE TMP
.*
.*       AENTRY GET_TMP
           AGO   .ACL_78_SKIP
.ACL_78_GET_TMP ANOP
.*       AIF   (&CUR_TMP_FREE EQ 0)
           AIF   (NOT(&CUR_TMP_FREE EQ 0)).AIF_491_1
&MAX_TMP       SETA &MAX_TMP+1
&TMP_NAME(&MAX_TMP) SETC 'ZCVT_WORKAREA+&TMP_OFF'
&TMP_OFF       SETA &TMP_OFF+16
&TMP_IX        SETA &MAX_TMP
.*       AELSE
           AGO   .AIF_491_E
.AIF_491_1 ANOP
&TMP_IX        SETA &TMP_FREE_IX(&CUR_TMP_FREE)
&CUR_TMP_FREE  SETA &CUR_TMP_FREE-1
.*       AEND
.AIF_491_E ANOP
.*       AEND
           AGO   (&ACALL_78_GET_TMP).ACL_78_1
.ACL_78_SKIP ANOP
.*
.* FREE TMP ADD TMP_IX TO FREE TMP FEILD LIST
.*
.*       AENTRY FREE_TMP
           AGO   .ACL_79_SKIP
.ACL_79_FREE_TMP ANOP
&CUR_TMP_FREE SETA &CUR_TMP_FREE+1
&TMP_FREE_IX(&CUR_TMP_FREE) SETA &TMP_IX
.*       AEND
           AGO   (&ACALL_79_FREE_TMP).ACL_79_1,.ACL_79_2
.ACL_79_SKIP ANOP
.*
.* SET LIT TYPE LEN FROM FIELD_NAME VALUE
.*
.*       AENTRY SET_LIT_TYPE_LEN
           AGO   .ACL_86_SKIP
.ACL_86_SET_LIT_TYPE_LEN ANOP
&LIT_LEN SETA 0
&FLT     SETA ('&FIELD_NAME' INDEX '.')
.*       AIF (&FLT GT 0)
           AIF (NOT(&FLT GT 0)).AIF_492_1
.*           AIF (K'&FIELD_NAME GT 16)
               AIF (NOT(K'&FIELD_NAME GT 16)).AIF_493_1
&LIT_TYPE        SETC '9'
.*           AELSE
               AGO   .AIF_493_E
.AIF_493_1     ANOP
&LIT_TYPE        SETC '8'
.*           AEND
.AIF_493_E     ANOP
.*       AELSE
           AGO   .AIF_492_E
.AIF_492_1 ANOP
.*           AIF (K'&FIELD_NAME GT 18)
               AIF (NOT(K'&FIELD_NAME GT 18)).AIF_494_1
&LIT_TYPE        SETC 'Q'
.*           AELSEIF (K'&FIELD_NAME GT 9)
               AGO   .AIF_494_E
.AIF_494_1     ANOP
               AIF     (NOT(K'&FIELD_NAME GT 9)).AIF_494_2
&LIT_TYPE        SETC 'G'
.*           AELSEIF (K'&FIELD_NAME GT 5)
               AGO   .AIF_494_E
.AIF_494_2     ANOP
               AIF     (NOT(K'&FIELD_NAME GT 5)).AIF_494_3
&LIT_TYPE        SETC 'F'
.*           AELSE
               AGO   .AIF_494_E
.AIF_494_3     ANOP
&LIT_TYPE        SETC 'H'
.*           AEND
.AIF_494_E     ANOP
.*       AEND
.AIF_492_E ANOP
.*       AEND
           AGO   (&ACALL_86_SET_LIT_TYPE_LEN).ACL_86_1,.ACL_86_2
.ACL_86_SKIP ANOP
,*
.* PUSH VAR - PUSH VAR_IOP_IX (INDEX OF FIRST CD PARM OR -IOP INDEX)
.*
.*       AENTRY PUSH_VAR
           AGO   .ACL_75_SKIP
.ACL_75_PUSH_VAR ANOP
&STK_VAR_TOT SETA &STK_VAR_TOT+1
&STK_VAR(&STK_VAR_TOT) SETA &VAR_IOP_IX
.*       AEND
           AGO   (&ACALL_75_PUSH_VAR).ACL_75_1,.ACL_75_2
.ACL_75_SKIP ANOP
.*
.* PUSH OP_# - PUSH LOGICAL OPERATOR '(+-*/()' = '123456'
.*
.*       AENTRY PUSH_OP
 MNOTE 8'ZSTRMAC ERROR AENTRY DUPLICATE NAME FOUND - PUSH_OP
&IX_PARM_TOT SETA 0             RESET CD PARM COUNTER AT LOP
&STK_OP_TOT SETA &STK_OP_TOT+1
&STK_OP(&STK_OP_TOT) SETA &OP#
&PREV_OP# SETA &OP#
.*       AEND
 MNOTE 8'ZSTRMAC ERROR AEND MISSING AIF OR OTHER STRUCTURE
.*
.* POP OP - POP LOGICAL OPERATOR NOT, AND, OR, '''('''
.*
.*       AENTRY POP_OP
 MNOTE 8'ZSTRMAC ERROR AENTRY DUPLICATE NAME FOUND - POP_OP
.*       AIF   (&STK_OP_TOT GE 1)
           AIF   (NOT(&STK_OP_TOT GE 1)).AIF_495_1
&OP#           SETA   &STK_OP(&STK_OP_TOT)
&STK_OP_TOT    SETA &STK_OP_TOT-1
.*             AIF (&STK_OP_TOT GT 0)
                 AIF (NOT(&STK_OP_TOT GT 0)).AIF_496_1
&PREV_OP#          SETA &STK_OP(&STK_OP_TOT)
.*             AELSE
                 AGO   .AIF_496_E
.AIF_496_1       ANOP
&PREV_OP#          SETA 0  NONE
.*             AEND
.AIF_496_E       ANOP
.*       AELSE
           AGO   .AIF_495_E
.AIF_495_1 ANOP
               MNOTE 8,'ZC_CALC POP OP ERROR - EMPTY STACK'
.*       AEND
.AIF_495_E ANOP
.*       AEND
 MNOTE 8'ZSTRMAC ERROR AEND MISSING AIF OR OTHER STRUCTURE
.*
.* IOP GEN VAR - GEN SIX CODE FOR VAR AT VAR_PARM_IX IF ANY
.*
.*       AENTRY IOP_GEN_VAR
           AGO   .ACL_89_SKIP
.ACL_89_IOP_GEN_VAR ANOP
&VAR_PARM_IX SETA &VAR_PARM_IX-1
&FIELD_SS1 SETB 1
.*       ACALL  GET_VAR_PARM
&ACALL_85_GET_VAR_PARM SETA    3
           AGO   .ACL_85_GET_VAR_PARM
.ACL_85_3  ANOP
.*       AEND
 MNOTE 8'ZSTRMAC ERROR AENTRY IOP_GEN_VAR NOT USED
.ACL_89_SKIP ANOP
.*
.*
.* IOP GEN 88 - GEN 1 OR MORE CONDITION 88 LEVEL COMPARES
.*
.*       AENTRY IOP_GEN_88
 MNOTE 8'ZSTRMAC ERROR AENTRY DUPLICATE NAME FOUND - IOP_GEN_88
&FIELD1  SETC '&FIELD_NAME'
&FIELD1_IX SETA &FIELD_IX
&V88_IX  SETA &SYM_LOC(&FIELD_88_IX)  FIRST VALUE INDEX
&V88_MULT SETB 0                       ASSUME NOT MULT
&CCT     SETC 'EQ'       NO  THEN EQ = OK
.*       AWHILE (&V88_IX GT 0)
.AWH_71_T  ANOP
           AIF    (NOT(&V88_IX GT 0)).AWH_71_E
               GEN_COMP &FIELD1,&FIELD1_IX,&V88_VAL(&V88_IX),0
&V88_IX        SETA &V88_NEXT(&V88_IX)
.*             AIF (&V88_IX GT 0)
                 AIF (NOT(&V88_IX GT 0)).AIF_497_1
.*                 AIF (&IOP_OP(&I) NE 1)
                     AIF (NOT(&IOP_OP(&I) NE 1)).AIF_498_1
.*                     AIF (NOT &V88_MULT)
                         AIF (NOT(NOT &V88_MULT)).AIF_499_1
&V88_MULT                  SETB 1
&IOP_LAB                   SETA &IOP_LAB+1   IOP T LABEL
.*                     AEND
.AIF_499_1               ANOP
                       GEN_BC &CCT,IF_&IE_TCNT(&IE_LVL)_IOP_&IOP_LAB
.*                 AELSE
                     AGO   .AIF_498_E
.AIF_498_1           ANOP
                       GEN_BC &CCT,IF_&IE_TCNT(&IE_LVL)_&IE_BCNT(&IE_LVX
               L)
.*                 AEND
.AIF_498_E           ANOP
.*             AEND
.AIF_497_1       ANOP
.*       AEND
           AGO   .AWH_71_T
.AWH_71_E  ANOP
.*       AEND
 MNOTE 8'ZSTRMAC ERROR AEND MISSING AIF OR OTHER STRUCTURE
.*
.* GET DFHRESP VALUE
.*
.*       AENTRY GET_DFHRESP_VALUE
           AGO   .ACL_90_SKIP
.ACL_90_GET_DFHRESP_VALUE ANOP
         GBLA  &DFHRESP_TOT
         GBLC  &DFHRESP_KEY(30),&DFHRESP_VALUE(30)
.*       AIF   (&DFHRESP_TOT EQ 0)
           AIF   (NOT(&DFHRESP_TOT EQ 0)).AIF_500_1
&DFHRESP_VALUE(01) SETC '0' DFHRESP(NORMAL)
&DFHRESP_VALUE(02) SETC '1' DFHRESP(ERROR)
&DFHRESP_VALUE(03) SETC '12' DFHRESP(FILENOTFOUND)
&DFHRESP_VALUE(04) SETC '12' DFHRESP(DSIDERR)
&DFHRESP_VALUE(05) SETC '13' DFHRESP(NOTFND)
&DFHRESP_VALUE(06) SETC '14' DFHRESP(DUPREC)
&DFHRESP_VALUE(07) SETC '15' DFHRESP(DUPKEY)
&DFHRESP_VALUE(08) SETC '16' DFHRESP(INVREQ)
&DFHRESP_VALUE(09) SETC '18' DFHRESP(NOSPACE)
&DFHRESP_VALUE(10) SETC '19' DFHRESP(NOTOPEN)
&DFHRESP_VALUE(11) SETC '20' DFHRESP(ENDFILE)
&DFHRESP_VALUE(13) SETC '21' DFHRESP(ILLOGIC)
&DFHRESP_VALUE(14) SETC '22' DFHRESP(LENGERR)
&DFHRESP_VALUE(15) SETC '26' DFHRESP(ITEMERR)
&DFHRESP_VALUE(16) SETC '27' DFHRESP(PGMIDERR)
&DFHRESP_VALUE(17) SETC '31' DFHRESP(EXPIRED)
&DFHRESP_VALUE(18) SETC '36' DFHRESP(MAPFAIL)
&DFHRESP_VALUE(19) SETC '38' DFHRESP(INVMPSZ)
&DFHRESP_VALUE(20) SETC '40' DFHRESP(OVERFLOW)
&DFHRESP_VALUE(21) SETC '44' DFHRESP(QIDERR)
&DFHRESP_VALUE(22) SETC '72' DFHRESP(SUPPRESSED)
&DFHRESP_VALUE(23) SETC '84' DFHRESP(DISABLED)
&DFHRESP_KEY(01) SETC 'NORMAL'
&DFHRESP_KEY(02) SETC 'ERROR'
&DFHRESP_KEY(03) SETC 'FILENOTFOUND'
&DFHRESP_KEY(04) SETC 'DSIDERR'
&DFHRESP_KEY(05) SETC 'NOTFND'
&DFHRESP_KEY(06) SETC 'DUPREC'
&DFHRESP_KEY(07) SETC 'DUPKEY'
&DFHRESP_KEY(08) SETC 'INVREQ'
&DFHRESP_KEY(09) SETC 'NOSPACE'
&DFHRESP_KEY(10) SETC 'NOTOPEN'
&DFHRESP_KEY(11) SETC 'ENDFILE'
&DFHRESP_KEY(13) SETC 'ILLOGIC'
&DFHRESP_KEY(14) SETC 'LENGERR'
&DFHRESP_KEY(15) SETC 'ITEMERR'
&DFHRESP_KEY(16) SETC 'PGMIDERR'
&DFHRESP_KEY(17) SETC 'EXPIRED'
&DFHRESP_KEY(18) SETC 'MAPFAIL'
&DFHRESP_KEY(19) SETC 'INVMPSZ'
&DFHRESP_KEY(20) SETC 'OVERFLOW'
&DFHRESP_KEY(21) SETC 'QIDERR'
&DFHRESP_KEY(22) SETC 'SUPPRESSED'
&DFHRESP_KEY(23) SETC 'DISABLED'
&DFHRESP_TOT   SETA 23
.*       AEND
.AIF_500_1 ANOP
.*       AIF   ('&SYSLIST(&VAR_PARM_IX+1)' EQ '''('''                  X
                OR '&SYSLIST(&VAR_PARM_IX+3)' EQ ''')''')
           AIF   (NOT('&SYSLIST(&VAR_PARM_IX+1)' EQ '''('''            X
                      OR '&SYSLIST(&VAR_PARM_IX+3)' EQ ''')''')).AIF_50X
               1_1
&PARM          SETC '&SYSLIST(&VAR_PARM_IX+2)'
&VAR_PARM_IX   SETA &VAR_PARM_IX+3
.*       AELSE
           AGO   .AIF_501_E
.AIF_501_1 ANOP
               MNOTE 8,'ZC_CALC INVALID DFHRESP PARM FORMAT AT &PARM'
               MEXIT
.*       AEND
.AIF_501_E ANOP
&J       SETA 1
.*       AWHILE (&J LE &DFHRESP_TOT)
.AWH_72_T  ANOP
           AIF    (NOT(&J LE &DFHRESP_TOT)).AWH_72_E
.*             AIF ('&PARM' EQ '&DFHRESP_KEY(&J)')
                 AIF (NOT('&PARM' EQ '&DFHRESP_KEY(&J)')).AIF_502_1
&PARM              SETC '&DFHRESP_VALUE(&J)'
.*                 AEXIT AENTRY
                     AGO   .ACL_90_E
.*             AEND
.AIF_502_1       ANOP
&J             SETA &J+1
.*       AEND
           AGO   .AWH_72_T
.AWH_72_E  ANOP
         MNOTE 8,'ZC_CALC INVALID OR UNKNOWN DFHRESP VALUE - &PARM'
.*       AEND
 MNOTE 8'ZSTRMAC ERROR AENTRY GET_DFHRESP_VALUE NOT USED
.ACL_90_SKIP ANOP
.*
.* GET DFHVALUE VALUE
.*
.*       AENTRY GET_DFHVALUE_VALUE
           AGO   .ACL_91_SKIP
.ACL_91_GET_DFHVALUE_VALUE ANOP
         GBLA  &DFHVALUE_TOT
         GBLC  &DFHVALUE_KEY(30),&DFHVALUE_VALUE(30)
.*       AIF   (&DFHVALUE_TOT EQ 0)
           AIF   (NOT(&DFHVALUE_TOT EQ 0)).AIF_503_1
&DFHVALUE_VALUE(01) SETC '18' DFHVALUE(OPEN)
&DFHVALUE_VALUE(02) SETC '19' DFHVALUE(CLOSED)
&DFHVALUE_KEY(01) SETC 'OPEN'
&DFHVALUE_KEY(02) SETC 'CLOSED'
&DFHVALUE_TOT  SETA 2
.*       AEND
.AIF_503_1 ANOP
.*       AIF   ('&SYSLIST(&VAR_PARM_IX+1)' EQ '''('''                  X
                OR '&SYSLIST(&VAR_PARM_IX+3)' EQ ''')''')
           AIF   (NOT('&SYSLIST(&VAR_PARM_IX+1)' EQ '''('''            X
                      OR '&SYSLIST(&VAR_PARM_IX+3)' EQ ''')''')).AIF_50X
               4_1
&PARM          SETC '&SYSLIST(&VAR_PARM_IX+2)'
&VAR_PARM_IX   SETA &VAR_PARM_IX+3
.*       AELSE
           AGO   .AIF_504_E
.AIF_504_1 ANOP
               MNOTE 8,'ZC_CALC INVALID DFHVALUE PARM FORMAT AT &PARM'
               MEXIT
.*       AEND
.AIF_504_E ANOP
&J       SETA 1
.*       AWHILE (&J LE &DFHVALUE_TOT)
.AWH_73_T  ANOP
           AIF    (NOT(&J LE &DFHVALUE_TOT)).AWH_73_E
.*             AIF ('&PARM' EQ '&DFHVALUE_KEY(&J)')
                 AIF (NOT('&PARM' EQ '&DFHVALUE_KEY(&J)')).AIF_505_1
&PARM              SETC '&DFHVALUE_VALUE(&J)'
.*                 AEXIT AENTRY
                     AGO   .ACL_91_E
.*             AEND
.AIF_505_1       ANOP
&J             SETA &J+1
.*       AEND
           AGO   .AWH_73_T
.AWH_73_E  ANOP
         MNOTE 8,'ZC_CALC INVALID DFHVALUE VALUE - &PARM'
.*       AEND
 MNOTE 8'ZSTRMAC ERROR AENTRY GET_DFHVALUE_VALUE NOT USED
.ACL_91_SKIP ANOP
.*
.* GET NEXT IF PARM (SUBSTITUTE DFHRESP(CODE) WITH LITERAL VALUE)
.*
.*       AENTRY GET_PARM
 MNOTE 8'ZSTRMAC ERROR AENTRY DUPLICATE NAME FOUND - GET_PARM
&PREV_PARM SETC '&PARM'
&PARM_IX SETA &PARM_IX+1
.*       AIF (&PARM_IX LE &NP)
           AIF (NOT(&PARM_IX LE &NP)).AIF_506_1
&PARM        SETC '&SYSLIST(&PARM_IX)'
.*       AELSE
           AGO   .AIF_506_E
.AIF_506_1 ANOP
.*           AIF (&LVL NE 0)
               AIF (NOT(&LVL NE 0)).AIF_507_1
                 MNOTE 8,'ZC_CALC UNBALANCED (...)'
.*           AEND
.AIF_507_1     ANOP
&PARM        SETC ''
.*       AEND
.AIF_506_E ANOP
.*       AEND
 MNOTE 8'ZSTRMAC ERROR AEND MISSING AIF OR OTHER STRUCTURE
.*
.* GET VAR PARM USING VAR_PARM_IX UNTIL NEXT OP
.*
.*       AENTRY GET_VAR_PARM
           AGO   .ACL_85_SKIP
.ACL_85_GET_VAR_PARM ANOP
&PREV_PARM SETC '&PARM'
&VAR_PARM_IX SETA &VAR_PARM_IX+1
.*       AIF   (&VAR_PARM_IX LE &NP)
           AIF   (NOT(&VAR_PARM_IX LE &NP)).AIF_508_1
&PARM          SETC '&SYSLIST(&VAR_PARM_IX)'
.*             AIF ('&PARM'(1,1) NE '''')
                 AIF (NOT('&PARM'(1,1) NE '''')).AIF_509_1
&SAVE_PARM_IX      SETA &PARM_IX
&PARM_IX           SETA &VAR_PARM_IX
.*                 ACALL GET_PARM_FIELD
&ACALL_6_GET_PARM_FIELD SETA    38
                     AGO   .ACL_6_GET_PARM_FIELD
.ACL_6_38            ANOP
&VAR_PARM_IX       SETA &PARM_IX-1
&PARM_IX           SETA &SAVE_PARM_IX
.*             AELSE
                 AGO   .AIF_509_E
.AIF_509_1       ANOP
&FIELD_IX          SETA 0
.*             AEND
.AIF_509_E       ANOP
.*       AELSE
           AGO   .AIF_508_E
.AIF_508_1 ANOP
&PARM          SETC ''
.*       AEND
.AIF_508_E ANOP
.*       AEND
           AGO   (&ACALL_85_GET_VAR_PARM).ACL_85_1,.ACL_85_2,.ACL_85_3
.ACL_85_SKIP ANOP
.*
.* LIST IOPS FOR AID IN DEBUGGING
.*
.*       AENTRY LIST_IOPS
 MNOTE 8'ZSTRMAC ERROR AENTRY DUPLICATE NAME FOUND - LIST_IOPS
&J       SETA 1
         MNOTE 'ZCOBOL IOP TABLE LISTING - TOTAL IOPS = &IOP_TOT'
.*       AWHILE (&J LE &IOP_TOT)
.AWH_74_T  ANOP
           AIF    (NOT(&J LE &IOP_TOT)).AWH_74_E
               MNOTE 'IOP=&J OP=&IOP_OP(&J) F1=&IOP_F1_X(&J) F2=&IOP_F1X
               _X(&J) F3=&TMP_TYPE(&J)'
&J             SETA &J+1
.*       AEND
           AGO   .AWH_74_T
.AWH_74_E  ANOP
.*       AEND
 MNOTE 8'ZSTRMAC ERROR AEND MISSING AIF OR OTHER STRUCTURE
.*
.* CALC F3_T TYPE FROM F1_T AND F2_T ALSO SET F3_L FOR TYPE P
.*
.*       AENTRY CALC_F3_TYPE
           AGO   .ACL_87_SKIP
.ACL_87_CALC_F3_TYPE ANOP
&EXTEND_FP_TYPE SETC '9'
&EXTEND_FP_LNG SETA 16
&NOEXTEND_FP_TYPE SETC '2'
&NOEXTEND_FP_LNG SETA 8
.*       ACASE (C2A('&F1_T'))
           AGO   .ACS_5_G
.*       AWHEN C'1',C'2',C'3',C'4',C'5',C'6',C'7',C'8',C'9'
.ACS_5_1   ANOP
.*             AIF (&ZC_EXTEND)
                 AIF (NOT(&ZC_EXTEND)).AIF_510_1
&F3_T              SETC '&EXTEND_FP_TYPE'
&F3_L              SETA &EXTEND_FP_LNG
.*             AELSE
                 AGO   .AIF_510_E
.AIF_510_1       ANOP
&F3_T              SETC '&NOEXTEND_FP_TYPE'
&F3_L              SETA &NOEXTEND_FP_LNG
.*             AEND
.AIF_510_E       ANOP
.*       AWHEN C'F'
           AGO   .ACS_5_E
.ACS_5_2   ANOP
.*             ACASE (C2A('&F2_T'))
                 AGO   .ACS_6_G
.*             AWHEN C'1',C'2',C'3',C'4',C'5',C'6',C'7',C'8',C'9'
.ACS_6_1         ANOP
.*                 AIF (&ZC_EXTEND)
                     AIF (NOT(&ZC_EXTEND)).AIF_511_1
&F3_T                  SETC '&EXTEND_FP_TYPE'
&F3_L                  SETA &EXTEND_FP_LNG
.*                 AELSE
                     AGO   .AIF_511_E
.AIF_511_1           ANOP
&F3_T                  SETC '&NOEXTEND_FP_TYPE'
&F3_L                  SETA &NOEXTEND_FP_LNG
.*                 AEND
.AIF_511_E           ANOP
.*             AWHEN C'F'
                 AGO   .ACS_6_E
.ACS_6_2         ANOP
&F3_T            SETC 'Q'
.*             AWHEN C'G'
                 AGO   .ACS_6_E
.ACS_6_3         ANOP
&F3_T            SETC 'Q'
.*             AWHEN C'H'
                 AGO   .ACS_6_E
.ACS_6_4         ANOP
&F3_T            SETC 'G'
.*             AWHEN C'P'
                 AGO   .ACS_6_E
.ACS_6_5         ANOP
.*               AIF (&F2_L GT 5)
                   AIF (NOT(&F2_L GT 5)).AIF_512_1
&F3_T               SETC 'Q'
.*               AELSE
                   AGO   .AIF_512_E
.AIF_512_1         ANOP
&F3_T               SETC 'G'
.*               AEND
.AIF_512_E         ANOP
.*             AWHEN C'Q'
                 AGO   .ACS_6_E
.ACS_6_6         ANOP
&F3_T            SETC 'Q'
.*             AWHEN C'Z'
                 AGO   .ACS_6_E
.ACS_6_7         ANOP
.*               AIF (&F2_L GT 9)
                   AIF (NOT(&F2_L GT 9)).AIF_513_1
&F3_T               SETC 'Q'
.*               AELSE
                   AGO   .AIF_513_E
.AIF_513_1         ANOP
&F3_T               SETC 'G'
.*               AEND
.AIF_513_E         ANOP
.*             AELSE
                 AGO   .ACS_6_E
.ACS_6_X         ANOP
                MNOTE 8,'ZC_CALC INVALID TYPE &F2_T'
                MEXIT
.*             AEND
                 AGO   .ACS_6_E
.ACS_6_G         ANOP
                 AGO   (C2A('&F2_T')+1-198).ACS_6_2,.ACS_6_3,.ACS_6_4,.X
               ACS_6_X,.ACS_6_X,.ACS_6_X,.ACS_6_X,.ACS_6_X,.ACS_6_X,.ACX
               S_6_X,.ACS_6_X,.ACS_6_X,.ACS_6_X,.ACS_6_X,.ACS_6_X,.ACS_X
               6_X,.ACS_6_X,.ACS_6_5,.ACS_6_6,.ACS_6_X,.ACS_6_X,.ACS_6_X
               X,.ACS_6_X,.ACS_6_X,.ACS_6_X,.ACS_6_X,.ACS_6_X,.ACS_6_X,X
               .ACS_6_X,.ACS_6_X,.ACS_6_X,.ACS_6_X,.ACS_6_X,.ACS_6_X,.AX
               CS_6_X,.ACS_6_7,.ACS_6_X,.ACS_6_X,.ACS_6_X,.ACS_6_X,.ACSX
               _6_X,.ACS_6_X,.ACS_6_X,.ACS_6_1,.ACS_6_1,.ACS_6_1,.ACS_6X
               _1,.ACS_6_1,.ACS_6_1,.ACS_6_1,.ACS_6_1,.ACS_6_1
                 AGO   .ACS_6_X
.ACS_6_E         ANOP
.*       AWHEN C'G'
           AGO   .ACS_5_E
.ACS_5_3   ANOP
.*             ACASE (C2A('&F2_T'))
                 AGO   .ACS_7_G
.*             AWHEN C'1',C'2',C'3',C'4',C'5',C'6',C'7',C'8',C'9'
.ACS_7_1         ANOP
.*                 AIF (&ZC_EXTEND)
                     AIF (NOT(&ZC_EXTEND)).AIF_514_1
&F3_T                  SETC '&EXTEND_FP_TYPE'
&F3_L                  SETA &EXTEND_FP_LNG
.*                 AELSE
                     AGO   .AIF_514_E
.AIF_514_1           ANOP
&F3_T                  SETC '&NOEXTEND_FP_TYPE'
&F3_L                  SETA &NOEXTEND_FP_LNG
.*                 AEND
.AIF_514_E           ANOP
.*             AWHEN C'F'
                 AGO   .ACS_7_E
.ACS_7_2         ANOP
&F3_T            SETC 'Q'
.*             AWHEN C'G'
                 AGO   .ACS_7_E
.ACS_7_3         ANOP
&F3_T            SETC 'Q'
.*             AWHEN C'H'
                 AGO   .ACS_7_E
.ACS_7_4         ANOP
&F3_T            SETC 'Q'
.*             AWHEN C'P'
                 AGO   .ACS_7_E
.ACS_7_5         ANOP
&F3_T            SETC 'Q'
.*             AWHEN C'Q'
                 AGO   .ACS_7_E
.ACS_7_6         ANOP
&F3_T            SETC 'Q'
.*             AWHEN C'Z'
                 AGO   .ACS_7_E
.ACS_7_7         ANOP
&F3_T            SETC 'Q'
.*             AELSE
                 AGO   .ACS_7_E
.ACS_7_X         ANOP
                MNOTE 8,'ZC_CALC INVALID TYPE &F2_T'
                MEXIT
.*             AEND
                 AGO   .ACS_7_E
.ACS_7_G         ANOP
                 AGO   (C2A('&F2_T')+1-198).ACS_7_2,.ACS_7_3,.ACS_7_4,.X
               ACS_7_X,.ACS_7_X,.ACS_7_X,.ACS_7_X,.ACS_7_X,.ACS_7_X,.ACX
               S_7_X,.ACS_7_X,.ACS_7_X,.ACS_7_X,.ACS_7_X,.ACS_7_X,.ACS_X
               7_X,.ACS_7_X,.ACS_7_5,.ACS_7_6,.ACS_7_X,.ACS_7_X,.ACS_7_X
               X,.ACS_7_X,.ACS_7_X,.ACS_7_X,.ACS_7_X,.ACS_7_X,.ACS_7_X,X
               .ACS_7_X,.ACS_7_X,.ACS_7_X,.ACS_7_X,.ACS_7_X,.ACS_7_X,.AX
               CS_7_X,.ACS_7_7,.ACS_7_X,.ACS_7_X,.ACS_7_X,.ACS_7_X,.ACSX
               _7_X,.ACS_7_X,.ACS_7_X,.ACS_7_1,.ACS_7_1,.ACS_7_1,.ACS_7X
               _1,.ACS_7_1,.ACS_7_1,.ACS_7_1,.ACS_7_1,.ACS_7_1
                 AGO   .ACS_7_X
.ACS_7_E         ANOP
.*       AWHEN C'H'
           AGO   .ACS_5_E
.ACS_5_4   ANOP
.*             ACASE (C2A('&F2_T'))
                 AGO   .ACS_8_G
.*             AWHEN C'1',C'2',C'3',C'4',C'5',C'6',C'7',C'8',C'9'
.ACS_8_1         ANOP
.*                 AIF (&ZC_EXTEND)
                     AIF (NOT(&ZC_EXTEND)).AIF_515_1
&F3_T                  SETC '&EXTEND_FP_TYPE'
&F3_L                  SETA &EXTEND_FP_LNG
.*                 AELSE
                     AGO   .AIF_515_E
.AIF_515_1           ANOP
&F3_T                  SETC '&NOEXTEND_FP_TYPE'
&F3_L                  SETA &NOEXTEND_FP_LNG
.*                 AEND
.AIF_515_E           ANOP
.*             AWHEN C'F'
                 AGO   .ACS_8_E
.ACS_8_2         ANOP
&F3_T            SETC 'G'
.*             AWHEN C'G'
                 AGO   .ACS_8_E
.ACS_8_3         ANOP
&F3_T            SETC 'Q'
.*             AWHEN C'H'
                 AGO   .ACS_8_E
.ACS_8_4         ANOP
&F3_T            SETC 'G'
.*             AWHEN C'P'
                 AGO   .ACS_8_E
.ACS_8_5         ANOP
.*               AIF (&F2_L LE 2)
                   AIF (NOT(&F2_L LE 2)).AIF_516_1
&F3_T               SETC 'F'
.*               AELSEIF (&F2_L LE 7)
                   AGO   .AIF_516_E
.AIF_516_1         ANOP
                   AIF     (NOT(&F2_L LE 7)).AIF_516_2
&F3_T               SETC 'G'
.*               AELSE
                   AGO   .AIF_516_E
.AIF_516_2         ANOP
&F3_T               SETC 'Q'
.*               AEND
.AIF_516_E         ANOP
.*             AWHEN C'Q'
                 AGO   .ACS_8_E
.ACS_8_6         ANOP
&F3_T            SETC 'Q'
.*             AWHEN C'Z'
                 AGO   .ACS_8_E
.ACS_8_7         ANOP
.*               AIF (&F2_L LE 4)
                   AIF (NOT(&F2_L LE 4)).AIF_517_1
&F3_T               SETC 'F'
.*               AELSEIF (&F2_L LE 13)
                   AGO   .AIF_517_E
.AIF_517_1         ANOP
                   AIF     (NOT(&F2_L LE 13)).AIF_517_2
&F3_T               SETC 'G'
.*               AELSE
                   AGO   .AIF_517_E
.AIF_517_2         ANOP
&F3_T               SETC 'Q'
.*               AEND
.AIF_517_E         ANOP
.*             AELSE
                 AGO   .ACS_8_E
.ACS_8_X         ANOP
                MNOTE 8,'ZC_CALC INVALID TYPE &F2_T'
                MEXIT
.*             AEND
                 AGO   .ACS_8_E
.ACS_8_G         ANOP
                 AGO   (C2A('&F2_T')+1-198).ACS_8_2,.ACS_8_3,.ACS_8_4,.X
               ACS_8_X,.ACS_8_X,.ACS_8_X,.ACS_8_X,.ACS_8_X,.ACS_8_X,.ACX
               S_8_X,.ACS_8_X,.ACS_8_X,.ACS_8_X,.ACS_8_X,.ACS_8_X,.ACS_X
               8_X,.ACS_8_X,.ACS_8_5,.ACS_8_6,.ACS_8_X,.ACS_8_X,.ACS_8_X
               X,.ACS_8_X,.ACS_8_X,.ACS_8_X,.ACS_8_X,.ACS_8_X,.ACS_8_X,X
               .ACS_8_X,.ACS_8_X,.ACS_8_X,.ACS_8_X,.ACS_8_X,.ACS_8_X,.AX
               CS_8_X,.ACS_8_7,.ACS_8_X,.ACS_8_X,.ACS_8_X,.ACS_8_X,.ACSX
               _8_X,.ACS_8_X,.ACS_8_X,.ACS_8_1,.ACS_8_1,.ACS_8_1,.ACS_8X
               _1,.ACS_8_1,.ACS_8_1,.ACS_8_1,.ACS_8_1,.ACS_8_1
                 AGO   .ACS_8_X
.ACS_8_E         ANOP
.*       AWHEN C'P'
           AGO   .ACS_5_E
.ACS_5_5   ANOP
.*             ACASE (C2A('&F2_T'))
                 AGO   .ACS_9_G
.*             AWHEN C'1',C'2',C'3',C'4',C'5',C'6',C'7',C'8',C'9'
.ACS_9_1         ANOP
.*                 AIF (&ZC_EXTEND)
                     AIF (NOT(&ZC_EXTEND)).AIF_518_1
&F3_T                  SETC '&EXTEND_FP_TYPE'
&F3_L                  SETA &EXTEND_FP_LNG
.*                 AELSE
                     AGO   .AIF_518_E
.AIF_518_1           ANOP
&F3_T                  SETC '&NOEXTEND_FP_TYPE'
&F3_L                  SETA &NOEXTEND_FP_LNG
.*                 AEND
.AIF_518_E           ANOP
.*             AWHEN C'F'
                 AGO   .ACS_9_E
.ACS_9_2         ANOP
.*               AIF (&F1_L LE 5)
                   AIF (NOT(&F1_L LE 5)).AIF_519_1
&F3_T               SETC 'G'
.*               AELSE
                   AGO   .AIF_519_E
.AIF_519_1         ANOP
&F3_T               SETC 'Q'
.*               AEND
.AIF_519_E         ANOP
.*             AWHEN C'G'
                 AGO   .ACS_9_E
.ACS_9_3         ANOP
&F3_T            SETC 'Q'
.*             AWHEN C'H'
                 AGO   .ACS_9_E
.ACS_9_4         ANOP
.*               AIF (&F1_L LE 2)
                   AIF (NOT(&F1_L LE 2)).AIF_520_1
&F3_T               SETC 'F'
.*               AELSEIF (&F1_L LE 7)
                   AGO   .AIF_520_E
.AIF_520_1         ANOP
                   AIF     (NOT(&F1_L LE 7)).AIF_520_2
&F3_T               SETC 'G'
.*               AELSE
                   AGO   .AIF_520_E
.AIF_520_2         ANOP
&F3_T               SETC 'Q'
.*               AEND
.AIF_520_E         ANOP
.*             AWHEN C'P'
                 AGO   .ACS_9_E
.ACS_9_5         ANOP
.*               AIF (&F1_L+&F2_L  LE 16)
                   AIF (NOT(&F1_L+&F2_L  LE 16)).AIF_521_1
&F3_T               SETC 'P'
&F3_L               SETA &F1_L+&F2_L
.*               AELSE
                   AGO   .AIF_521_E
.AIF_521_1         ANOP
&F3_T               SETC 'Q'
.*               AEND
.AIF_521_E         ANOP
.*             AWHEN C'Q'
                 AGO   .ACS_9_E
.ACS_9_6         ANOP
&F3_T            SETC 'Q'
.*             AWHEN C'Z'
                 AGO   .ACS_9_E
.ACS_9_7         ANOP
.*               AIF (&F1_L+&F2_L LE 31)
                   AIF (NOT(&F1_L+&F2_L LE 31)).AIF_522_1
&F3_T                SETC 'P'
&F3_L                SETA (&F1_L+&F2_L)/2+1
.*               AELSE
                   AGO   .AIF_522_E
.AIF_522_1         ANOP
&F3_T               SETC 'Q'
.*               AEND
.AIF_522_E         ANOP
.*             AELSE
                 AGO   .ACS_9_E
.ACS_9_X         ANOP
                MNOTE 8,'ZC_CALC INVALID TYPE &F2_T'
                MEXIT
.*             AEND
                 AGO   .ACS_9_E
.ACS_9_G         ANOP
                 AGO   (C2A('&F2_T')+1-198).ACS_9_2,.ACS_9_3,.ACS_9_4,.X
               ACS_9_X,.ACS_9_X,.ACS_9_X,.ACS_9_X,.ACS_9_X,.ACS_9_X,.ACX
               S_9_X,.ACS_9_X,.ACS_9_X,.ACS_9_X,.ACS_9_X,.ACS_9_X,.ACS_X
               9_X,.ACS_9_X,.ACS_9_5,.ACS_9_6,.ACS_9_X,.ACS_9_X,.ACS_9_X
               X,.ACS_9_X,.ACS_9_X,.ACS_9_X,.ACS_9_X,.ACS_9_X,.ACS_9_X,X
               .ACS_9_X,.ACS_9_X,.ACS_9_X,.ACS_9_X,.ACS_9_X,.ACS_9_X,.AX
               CS_9_X,.ACS_9_7,.ACS_9_X,.ACS_9_X,.ACS_9_X,.ACS_9_X,.ACSX
               _9_X,.ACS_9_X,.ACS_9_X,.ACS_9_1,.ACS_9_1,.ACS_9_1,.ACS_9X
               _1,.ACS_9_1,.ACS_9_1,.ACS_9_1,.ACS_9_1,.ACS_9_1
                 AGO   .ACS_9_X
.ACS_9_E         ANOP
.*       AWHEN C'Q'
           AGO   .ACS_5_E
.ACS_5_6   ANOP
.*             ACASE (C2A('&F2_T'))
                 AGO   .ACS_10_G
.*             AWHEN C'1',C'2',C'3',C'4',C'5',C'6',C'7',C'8',C'9'
.ACS_10_1        ANOP
.*                 AIF (&ZC_EXTEND)
                     AIF (NOT(&ZC_EXTEND)).AIF_523_1
&F3_T                  SETC '&EXTEND_FP_TYPE'
&F3_L                  SETA &EXTEND_FP_LNG
.*                 AELSE
                     AGO   .AIF_523_E
.AIF_523_1           ANOP
&F3_T                  SETC '&NOEXTEND_FP_TYPE'
&F3_L                  SETA &NOEXTEND_FP_LNG
.*                 AEND
.AIF_523_E           ANOP
.*             AWHEN C'F'
                 AGO   .ACS_10_E
.ACS_10_2        ANOP
&F3_T            SETC 'Q'
.*             AWHEN C'G'
                 AGO   .ACS_10_E
.ACS_10_3        ANOP
&F3_T            SETC 'Q'
.*             AWHEN C'H'
                 AGO   .ACS_10_E
.ACS_10_4        ANOP
&F3_T            SETC 'Q'
.*             AWHEN C'P'
                 AGO   .ACS_10_E
.ACS_10_5        ANOP
&F3_T            SETC 'Q'
.*             AWHEN C'Q'
                 AGO   .ACS_10_E
.ACS_10_6        ANOP
&F3_T            SETC 'Q'
.*             AWHEN C'Z'
                 AGO   .ACS_10_E
.ACS_10_7        ANOP
&F3_T            SETC 'Q'
.*             AELSE
                 AGO   .ACS_10_E
.ACS_10_X        ANOP
                MNOTE 8,'ZC_CALC INVALID TYPE &F2_T'
                MEXIT
.*             AEND
                 AGO   .ACS_10_E
.ACS_10_G        ANOP
                 AGO   (C2A('&F2_T')+1-198).ACS_10_2,.ACS_10_3,.ACS_10_X
               4,.ACS_10_X,.ACS_10_X,.ACS_10_X,.ACS_10_X,.ACS_10_X,.ACSX
               _10_X,.ACS_10_X,.ACS_10_X,.ACS_10_X,.ACS_10_X,.ACS_10_X,X
               .ACS_10_X,.ACS_10_X,.ACS_10_X,.ACS_10_5,.ACS_10_6,.ACS_1X
               0_X,.ACS_10_X,.ACS_10_X,.ACS_10_X,.ACS_10_X,.ACS_10_X,.AX
               CS_10_X,.ACS_10_X,.ACS_10_X,.ACS_10_X,.ACS_10_X,.ACS_10_X
               X,.ACS_10_X,.ACS_10_X,.ACS_10_X,.ACS_10_X,.ACS_10_7,.ACSX
               _10_X,.ACS_10_X,.ACS_10_X,.ACS_10_X,.ACS_10_X,.ACS_10_X,X
               .ACS_10_X,.ACS_10_1,.ACS_10_1,.ACS_10_1,.ACS_10_1,.ACS_1X
               0_1,.ACS_10_1,.ACS_10_1,.ACS_10_1,.ACS_10_1
                 AGO   .ACS_10_X
.ACS_10_E        ANOP
.*       AWHEN C'Z'
           AGO   .ACS_5_E
.ACS_5_7   ANOP
.*             ACASE (C2A('&F2_T'))
                 AGO   .ACS_11_G
.*             AWHEN C'1',C'2',C'3',C'4',C'5',C'6',C'7',C'8',C'9'
.ACS_11_1        ANOP
.*                 AIF (&ZC_EXTEND)
                     AIF (NOT(&ZC_EXTEND)).AIF_524_1
&F3_T                  SETC '&EXTEND_FP_TYPE'
&F3_L                  SETA &EXTEND_FP_LNG
.*                 AELSE
                     AGO   .AIF_524_E
.AIF_524_1           ANOP
&F3_T                  SETC '&NOEXTEND_FP_TYPE'
&F3_L                  SETA &NOEXTEND_FP_LNG
.*                 AEND
.AIF_524_E           ANOP
.*             AWHEN C'F'
                 AGO   .ACS_11_E
.ACS_11_2        ANOP
.*               AIF (&F1_L GE 9)
                   AIF (NOT(&F1_L GE 9)).AIF_525_1
&F3_T               SETC 'G'
.*               AELSE
                   AGO   .AIF_525_E
.AIF_525_1         ANOP
&F3_T               SETC 'Q'
.*               AEND
.AIF_525_E         ANOP
.*             AWHEN C'G'
                 AGO   .ACS_11_E
.ACS_11_3        ANOP
&F3_T            SETC 'Q'
.*             AWHEN C'H'
                 AGO   .ACS_11_E
.ACS_11_4        ANOP
.*               AIF (&F1_L LE 4)
                   AIF (NOT(&F1_L LE 4)).AIF_526_1
&F3_T               SETC 'F'
.*               AELSEIF (&F1_L LE 13)
                   AGO   .AIF_526_E
.AIF_526_1         ANOP
                   AIF     (NOT(&F1_L LE 13)).AIF_526_2
&F3_T               SETC 'G'
.*               AELSE
                   AGO   .AIF_526_E
.AIF_526_2         ANOP
&F3_T               SETC 'Q'
.*               AEND
.AIF_526_E         ANOP
.*             AWHEN C'P'
                 AGO   .ACS_11_E
.ACS_11_5        ANOP
.*               AIF (&F1_L+2*&F2_L-1 LE 31)
                   AIF (NOT(&F1_L+2*&F2_L-1 LE 31)).AIF_527_1
&F3_T                SETC 'P'
&F3_L                SETA &F1_L/2+1+&F2_L
.*               AELSE
                   AGO   .AIF_527_E
.AIF_527_1         ANOP
&F3_T               SETC 'Q'
.*               AEND
.AIF_527_E         ANOP
.*             AWHEN C'Q'
                 AGO   .ACS_11_E
.ACS_11_6        ANOP
&F3_T            SETC 'Q'
.*             AWHEN C'Z'
                 AGO   .ACS_11_E
.ACS_11_7        ANOP
.*               AIF (&F1_L+&F2_L LE 31)
                   AIF (NOT(&F1_L+&F2_L LE 31)).AIF_528_1
&F3_T                SETC 'P'
&F3_L                SETA (&F1_L+&F2_L)/2+1
.*               AELSE
                   AGO   .AIF_528_E
.AIF_528_1         ANOP
&F3_T               SETC 'Q'
.*               AEND
.AIF_528_E         ANOP
.*             AELSE
                 AGO   .ACS_11_E
.ACS_11_X        ANOP
                MNOTE 8,'ZC_CALC INVALID F2 TYPE &F2_T'
                MEXIT
.*             AEND
                 AGO   .ACS_11_E
.ACS_11_G        ANOP
                 AGO   (C2A('&F2_T')+1-198).ACS_11_2,.ACS_11_3,.ACS_11_X
               4,.ACS_11_X,.ACS_11_X,.ACS_11_X,.ACS_11_X,.ACS_11_X,.ACSX
               _11_X,.ACS_11_X,.ACS_11_X,.ACS_11_X,.ACS_11_X,.ACS_11_X,X
               .ACS_11_X,.ACS_11_X,.ACS_11_X,.ACS_11_5,.ACS_11_6,.ACS_1X
               1_X,.ACS_11_X,.ACS_11_X,.ACS_11_X,.ACS_11_X,.ACS_11_X,.AX
               CS_11_X,.ACS_11_X,.ACS_11_X,.ACS_11_X,.ACS_11_X,.ACS_11_X
               X,.ACS_11_X,.ACS_11_X,.ACS_11_X,.ACS_11_X,.ACS_11_7,.ACSX
               _11_X,.ACS_11_X,.ACS_11_X,.ACS_11_X,.ACS_11_X,.ACS_11_X,X
               .ACS_11_X,.ACS_11_1,.ACS_11_1,.ACS_11_1,.ACS_11_1,.ACS_1X
               1_1,.ACS_11_1,.ACS_11_1,.ACS_11_1,.ACS_11_1
                 AGO   .ACS_11_X
.ACS_11_E        ANOP
.*       AELSE
           AGO   .ACS_5_E
.ACS_5_X   ANOP
               MNOTE  8,'ZC_CALC INVALID F1 TYPE &F1_T'
               MEXIT
.*       AEND
           AGO   .ACS_5_E
.ACS_5_G   ANOP
           AGO   (C2A('&F1_T')+1-198).ACS_5_2,.ACS_5_3,.ACS_5_4,.ACS_5_X
               X,.ACS_5_X,.ACS_5_X,.ACS_5_X,.ACS_5_X,.ACS_5_X,.ACS_5_X,X
               .ACS_5_X,.ACS_5_X,.ACS_5_X,.ACS_5_X,.ACS_5_X,.ACS_5_X,.AX
               CS_5_X,.ACS_5_5,.ACS_5_6,.ACS_5_X,.ACS_5_X,.ACS_5_X,.ACSX
               _5_X,.ACS_5_X,.ACS_5_X,.ACS_5_X,.ACS_5_X,.ACS_5_X,.ACS_5X
               _X,.ACS_5_X,.ACS_5_X,.ACS_5_X,.ACS_5_X,.ACS_5_X,.ACS_5_XX
               ,.ACS_5_7,.ACS_5_X,.ACS_5_X,.ACS_5_X,.ACS_5_X,.ACS_5_X,.X
               ACS_5_X,.ACS_5_X,.ACS_5_1,.ACS_5_1,.ACS_5_1,.ACS_5_1,.ACX
               S_5_1,.ACS_5_1,.ACS_5_1,.ACS_5_1,.ACS_5_1
           AGO   .ACS_5_X
.ACS_5_E   ANOP
.*       ACASE (C2A('&F3_T'))
           AGO   .ACS_12_G
.*       AWHEN C'1'
.ACS_12_1  ANOP
&F3_L         SETA 4
.*       AWHEN C'2'
           AGO   .ACS_12_E
.ACS_12_2  ANOP
&F3_L         SETA 8
.*       AWHEN C'3'
           AGO   .ACS_12_E
.ACS_12_3  ANOP
&F3_L         SETA 16
.*       AWHEN C'4'
           AGO   .ACS_12_E
.ACS_12_4  ANOP
&F3_L         SETA 4
.*       AWHEN C'5'
           AGO   .ACS_12_E
.ACS_12_5  ANOP
&F3_L         SETA 8
.*       AWHEN C'6'
           AGO   .ACS_12_E
.ACS_12_6  ANOP
&F3_L         SETA 16
.*       AWHEN C'7'
           AGO   .ACS_12_E
.ACS_12_7  ANOP
&F3_L         SETA 4
.*       AWHEN C'8'
           AGO   .ACS_12_E
.ACS_12_8  ANOP
&F3_L         SETA 8
.*       AWHEN C'9'
           AGO   .ACS_12_E
.ACS_12_9  ANOP
&F3_L         SETA 16
.*       AWHEN C'F'
           AGO   .ACS_12_E
.ACS_12_10 ANOP
&F3_L         SETA 4
.*       AWHEN C'G'
           AGO   .ACS_12_E
.ACS_12_11 ANOP
&F3_L         SETA 8
.*       AWHEN C'H'
           AGO   .ACS_12_E
.ACS_12_12 ANOP
&F3_L         SETA 2
.*       AWHEN C'Q'
           AGO   .ACS_12_E
.ACS_12_13 ANOP
&F3_L         SETA 16
.*       AEND
           AGO   .ACS_12_E
.ACS_12_G  ANOP
           AGO   (C2A('&F3_T')+1-198).ACS_12_10,.ACS_12_11,.ACS_12_12,.X
               ACS_12_E,.ACS_12_E,.ACS_12_E,.ACS_12_E,.ACS_12_E,.ACS_12X
               _E,.ACS_12_E,.ACS_12_E,.ACS_12_E,.ACS_12_E,.ACS_12_E,.ACX
               S_12_E,.ACS_12_E,.ACS_12_E,.ACS_12_E,.ACS_12_13,.ACS_12_X
               E,.ACS_12_E,.ACS_12_E,.ACS_12_E,.ACS_12_E,.ACS_12_E,.ACSX
               _12_E,.ACS_12_E,.ACS_12_E,.ACS_12_E,.ACS_12_E,.ACS_12_E,X
               .ACS_12_E,.ACS_12_E,.ACS_12_E,.ACS_12_E,.ACS_12_E,.ACS_1X
               2_E,.ACS_12_E,.ACS_12_E,.ACS_12_E,.ACS_12_E,.ACS_12_E,.AX
               CS_12_E,.ACS_12_1,.ACS_12_2,.ACS_12_3,.ACS_12_4,.ACS_12_X
               5,.ACS_12_6,.ACS_12_7,.ACS_12_8,.ACS_12_9
.ACS_12_E  ANOP
.*       AEND
           AGO   (&ACALL_87_CALC_F3_TYPE).ACL_87_1
.ACL_87_SKIP ANOP
         COPY ZCRSTLAB
         COPY ZCGETFLD
         MEND
         MACRO
.*********************************************************************
.* Copyright 2008 Automated Software Tools Corporation               *
.* This source code is part of z390 zCOBOL/assembler/emulator package*
.* The z390 package is distributed under GNU general public license  *
.* Author - Don Higgins                                              *
.* Date   - 04/07/08                                                 *
.*********************************************************************
.* 04/08/08 initial coding to list all ws sym addr, len, pic, val
.* 05/03/08 align fields and call GEN_COMMENT for report lines
.* 10/06/08 ZSTRMAC
.*********************************************************************
         ZC_DATA_LIST
         COPY  ZC_WS
.*       AIF   (&SYM_TOT EQ 0)
           AIF   (NOT(&SYM_TOT EQ 0)).AIF_529_1
               MEXIT
.*       AEND
.AIF_529_1 ANOP
.*       AIF   (NOT &LK_MODE)
           AIF   (NOT(NOT &LK_MODE)).AIF_530_1
               GEN_COMMENT 'ZCOBOL WORKING STORAGE SECTION DATA ITEMS'
.*       AELSE
           AGO   .AIF_530_E
.AIF_530_1 ANOP
               GEN_COMMENT 'ZCOBOL LINKAGE SECTION DATA ITEMS'
.*       AEND
.AIF_530_E ANOP
         GEN_COMMENT 'LVL DATA NAME                      ADDR     LEN  X
                   DUP TYP PIC        VALUE'
         ACTR  100000
&I       SETA  1
.*       AWHILE (&I LE &SYM_TOT)
.AWH_75_T  ANOP
           AIF    (NOT(&I LE &SYM_TOT)).AWH_75_E
.*              AIF   ((NOT &LK_MODE AND &SYM_LK(&I) EQ 0)             X
                      OR (&LK_MODE AND &SYM_LK(&I) GT 0))
                  AIF   (NOT((NOT &LK_MODE AND &SYM_LK(&I) EQ 0)       X
                            OR (&LK_MODE AND &SYM_LK(&I) GT 0))).AIF_53X
               1_1
&LVL                SETC  '0&SYM_LVL(&I)'
&LVL                SETC  '&LVL'(K'&LVL-1,2).' '
&SYM                SETC  '&SYM_NAME(&I)'
.*                  AIF  (K'&SYM LT 30)
                      AIF  (NOT(K'&SYM LT 30)).AIF_532_1
&SYM                     SETC  '&SYM                              '(1,3X
               0)
.*                  AEND
.AIF_532_1            ANOP
&ADDR               SETC  A2X(&SYM_LOC(&I))
&LEN                SETC  A2X(&SYM_LEN(&I))
&DUP                SETC  '&SYM_DUP(&I)  '
&PICTYPE            SETC  '&SYM_PIC_TYPE(&I)  '
&PIC                SETC  '&SYM_PIC(&I)'
.*                  AIF   (K'&PIC LE 15)
                      AIF   (NOT(K'&PIC LE 15)).AIF_533_1
&PIC                     SETC  '&PIC               '(1,10)
.*                  AEND
.AIF_533_1            ANOP
&VAL                SETC  (DOUBLE '&SYM_VAL(&I)')
                    GEN_COMMENT  '&LVL &SYM &ADDR &LEN &DUP &PICTYPE &PX
               IC &VAL'
.*             AEND
.AIF_531_1       ANOP
&I             SETA &I+1
.*       AEND
           AGO   .AWH_75_T
.AWH_75_E  ANOP
         MEND
         MACRO
.*********************************************************************
.* Copyright 2008 Automated Software Tools Corporation               *
.* This source code is part of z390 zCOBOL/assembler/emulator package*
.* The z390 package is distributed under GNU general public license  *
.* Author - Don Higgins                                              *
.* Date   - 04/14/08                                                 *
.*********************************************************************
.* 04/14/08 initial coding for global find symbol
.* 10/06/08 ZSTRMAC
.*********************************************************************
         ZC_FILE_FIND &FILE
         COPY  ZC_WS
&FILE_IX  SETA  1
.*       AWHILE (&FILE_IX LE &FILE_TOT)
.AWH_76_T  ANOP
           AIF    (NOT(&FILE_IX LE &FILE_TOT)).AWH_76_E
.*              AIF   ('&FILE_NAME(&FILE_IX)' EQ '&FILE')
                  AIF   (NOT('&FILE_NAME(&FILE_IX)' EQ '&FILE')).AIF_53X
               4_1
                      MEXIT
.*              AEND
.AIF_534_1        ANOP
&FILE_IX        SETA &FILE_IX+1
.*       AEND
           AGO   .AWH_76_T
.AWH_76_E  ANOP
&FILE_IX SETA 0
        MEND
         MACRO
.*********************************************************************
.* Copyright 2009 Automated Software Tools Corporation               *
.* This source code is part of z390 zCOBOL/assembler/emulator package*
.* The z390 package is distributed under GNU general public license  *
.* Author - Don Higgins                                              *
.* Date   - 07/17/09                                                 *
.*********************************************************************
.* 07/17/09 RPI 1065 get qualified name for ZCGETFLD and_SYM_INDEX
.* 07/17/09 RPI 1065 set heading to 30 for max base name +#n#+ tail
.*********************************************************************
.*  1.   Build qualified FIELD_NAME FROM GBL QNAME vars
.*  2.   If qualified name > 63, truncate to leading 30 + #n# +
.*       trailing up to 63.
.*  3.   n is unique trunc counter
.*********************************************************************
         ZC_GET_QNAME
         COPY  ZC_WS
.*       AIF   (&QNAME_LEN GT 63)  RPI 1065 BUILD TRUN NAME ALIAS
           AIF   (NOT(&QNAME_LEN GT 63)).AIF_535_1   RPI 1065 BUILD TRUX
               N NAME ALIAS
&HEAD          SETC '&QNAME(1)'
&QI            SETA 2
.*             AWHILE (K'&HEAD LT 30)
.AWH_77_T        ANOP
                 AIF    (NOT(K'&HEAD LT 30)).AWH_77_E
&HEAD              SETC '&HEAD._OF_&QNAME(&QI)'
&QI                SETA &QI+1
.*             AEND
                 AGO   .AWH_77_T
.AWH_77_E        ANOP
&QI            SETA &QNAME_TOT-1
&TAIL          SETC '&QNAME(&QNAME_TOT)'
.*             AWHILE (K'&TAIL LT 30)
.AWH_78_T        ANOP
                 AIF    (NOT(K'&TAIL LT 30)).AWH_78_E
&TAIL                SETC '&QNAME(&QI)._OF_&TAIL'
&QI                  SETA &QI-1
.*             AEND
                 AGO   .AWH_78_T
.AWH_78_E        ANOP
               GBLA &ZC_TRNC_LAB
&ZC_TRNC_LAB   SETA &ZC_TRNC_LAB+1
&HEAD          SETC '&HEAD'(1,30).'#&ZC_TRNC_LAB.#'
&LTAIL         SETA 63-K'&HEAD
&FIELD_NAME    SETC '&HEAD'.'&TAIL'(K'&TAIL-&LTAIL+1,*)
.*       AELSE
           AGO   .AIF_535_E
.AIF_535_1 ANOP
&QI            SETA 2
&FIELD_NAME    SETC '&QNAME(1)'
.*             AWHILE (&QI LE &QNAME_TOT)
.AWH_79_T        ANOP
                 AIF    (NOT(&QI LE &QNAME_TOT)).AWH_79_E
&FIELD_NAME        SETC '&FIELD_NAME._OF_&QNAME(&QI)'
&QI                SETA &QI+1
.*             AEND
                 AGO   .AWH_79_T
.AWH_79_E        ANOP
.*       AEND
.AIF_535_E ANOP
         MEND
         MACRO
.*********************************************************************
.* Copyright 2009 Automated Software Tools Corporation               *
.* This source code is part of z390 zCOBOL/assembler/emulator package*
.* The z390 package is distributed under GNU general public license  *
.* Author - Don Higgins                                              *
.* Date   - 01/26/09                                                 *
.*********************************************************************
.* 01/26/09 initial coding to set &PIC_DIGITS from &SYM_PIC(&IX)
.*********************************************************************
         ZC_PIC_DIGITS &IX
         COPY  ZC_WS
&PIC     SETC '&SYM_PIC(&IX)'
&PIC_DIGITS SETA 0
&I       SETA 1
.*       AWHILE (&I LE K'&PIC)
.AWH_80_T  ANOP
           AIF    (NOT(&I LE K'&PIC)).AWH_80_E
&CHAR          SETC '&PIC'(&I,1)
.*             AIF ('&CHAR' EQ '9' OR '&CHAR' EQ 'Z')
                 AIF (NOT('&CHAR' EQ '9' OR '&CHAR' EQ 'Z')).AIF_536_1
&PIC_DIGITS        SETA &PIC_DIGITS+1
.*             AELSEIF ('&CHAR' EQ '(')
                 AGO   .AIF_536_E
.AIF_536_1       ANOP
                 AIF     (NOT('&CHAR' EQ '(')).AIF_536_2
&DUP               SETA '&PIC'(&I+1,1)
&I                 SETA &I+2
.*                 AWHILE (&I LT K'&PIC AND '&PIC'(&I,1) NE ')')
.AWH_81_T            ANOP
                     AIF    (NOT(&I LT K'&PIC AND '&PIC'(&I,1) NE ')'))X
               .AWH_81_E
&DIG                   SETA '&PIC'(&I,1)
&DUP                   SETA &DUP*10+&DIG
&I                     SETA &I+1
.*                 AEND
                     AGO   .AWH_81_T
.AWH_81_E            ANOP
&PIC_DIGITS        SETA &PIC_DIGITS+&DUP-1
.*             AEND
.AIF_536_2       ANOP
.AIF_536_E       ANOP
&I             SETA &I+1
.*       AEND
           AGO   .AWH_80_T
.AWH_80_E  ANOP
         MEND
         MACRO
.*********************************************************************
.* Copyright 2008 Automated Software Tools Corporation               *
.* This source code is part of z390 zCOBOL/assembler/emulator package*
.* The z390 package is distributed under GNU general public license  *
.* Author.  Don Higgins                                              *
.* Date   - 04/07/08                                                 *
.*********************************************************************
.* 04/08/08 initial coding to set SYM_LEN(&SYM_TOT) from SYM_PIC
.* 05/02/08 just set SYM_LEN(&SYM_TOT) based on &SYM_PIC(&SYM_TOT)
.* 10/06/08 ZSTRMAC
.*********************************************************************
         ZC_PIC_LEN
         COPY  ZC_WS
&LEN     SETA  0
&MASK    SETC  '&SYM_PIC(&SYM_TOT)'
&MASK_LEN SETC K'&MASK
&I       SETA  1
.*       AWHILE (&I LE &MASK_LEN)
.AWH_82_T  ANOP
           AIF    (NOT(&I LE &MASK_LEN)).AWH_82_E
&CHAR          SETC  '&MASK'(&I,1)
.*             AIF   ('&CHAR' EQ '9'                                   X
                      OR '&CHAR' EQ 'Z'                                X
                      OR '&CHAR' EQ 'X')
                 AIF   (NOT('&CHAR' EQ '9'                             X
                            OR '&CHAR' EQ 'Z'                          X
                            OR '&CHAR' EQ 'X')).AIF_537_1
&LEN                SETA  &LEN+1
.*             AELSEIF ('&CHAR' EQ '(')
                 AGO   .AIF_537_E
.AIF_537_1       ANOP
                 AIF     (NOT('&CHAR' EQ '(')).AIF_537_2
&LEN                SETA   0
&I                  SETA   &I+1
.*                  AWHILE (&I LE &MASK_LEN                            X
                            AND '&MASK'(&I,1) NE ')')
.AWH_83_T             ANOP
                      AIF    (NOT(&I LE &MASK_LEN                      X
                                  AND '&MASK'(&I,1) NE ')')).AWH_83_E
&CHAR                   SETC  '&MASK'(&I,1)
.*                      AIF ('&CHAR' GE '0' AND '&CHAR' LE '9')
                          AIF (NOT('&CHAR' GE '0' AND '&CHAR' LE '9')).X
               AIF_538_1
&LEN                        SETA  &LEN*10+&CHAR
.*                      AELSE
                          AGO   .AIF_538_E
.AIF_538_1                ANOP
                            MNOTE 8,'PIC MASK ERROR &LVL &SYM_NAME(&SYMX
               _TOT)'
                            MEXIT
.*                      AEND
.AIF_538_E                ANOP
&I                      SETA &I+1
.*                  AEND
                      AGO   .AWH_83_T
.AWH_83_E             ANOP
.*             AELSEIF ('&CHAR' NE 'S' AND '&CHAR' NE '.')
                 AGO   .AIF_537_E
.AIF_537_2       ANOP
                 AIF     (NOT('&CHAR' NE 'S' AND '&CHAR' NE '.')).AIF_5X
               37_3
                    MNOTE 8,'PIC MASK ERROR &LVL &SYM_NAME(&SYMTOT)'
                    MEXIT
.*             AEND
.AIF_537_3       ANOP
.AIF_537_E       ANOP
&I             SETA &I+1
.*       AEND
           AGO   .AWH_82_T
.AWH_82_E  ANOP
&SYM_LEN(&SYM_TOT) SETA &LEN
         MEND
         MACRO
.*********************************************************************
.* Copyright 2008 Automated Software Tools Corporation               *
.* This source code is part of z390 zCOBOL/assembler/emulator package*
.* The z390 package is distributed under GNU general public license  *
.* Author - Don Higgins                                              *
.* Date   - 04/07/08                                                 *
.*********************************************************************
.* 10/14/08 INITIAL CODDING
.* 08/06/09 RPI 1062 ADD PROCEDURE SECTION AND PARAGRAPH STATISTICS
.*********************************************************************
         ZC_STATS
         COPY  ZC_WS
         MNOTE *.'ZCOBOL TOTAL SYMBOLS                 = &SYM_TOT'
         MNOTE *.'ZCOBOL TOTAL SYMBOL DUPLICATES       = &SYM_DUP_TOT'
         MNOTE *.'ZCOBOL TOTAL SYMBOL MAX DUP CHAIN    = &SYM_DUP_MAX'
         MNOTE *.'ZCOBOL TOTAL SYMBOL LEVEL 88         = &V88_TOT'
         MNOTE *.'ZCOBOL TOTAL SYMBOL SET INDEXES      = &SET_TOT'
         MNOTE *.'ZCOBOL TOTAL SYMBOL LINKAGE SECTIONS = &LK_TOT'
         MNOTE *.'ZCOBOL TOTAL PROCEDURE SECTIONS      = &SN_TOT'
         MNOTE *.'ZCOBOL TOTAL PROCEDURE PARAGRAPHS    = &PG_TOT'
         MNOTE *.'ZCOBOL TOTAL PROCEDURE PARAGRAPH DUP = &PG_DUP_TOT'
         MEND
         MACRO
.*********************************************************************
.* Copyright 2008 Automated Software Tools Corporation               *
.* This source code is part of z390 zCOBOL/assembler/emulator package*
.* The z390 package is distributed under GNU general public license  *
.* Author - Don Higgins                                              *
.* Date   - 04/07/08                                                 *
.*********************************************************************
.* 04/09/08 initial coding for global find symbol
.* 07/26/08 add hash table search replacing serial search
.* 10/06/08 ZSTRMAC
.* 11/17/08  replace hash calc with GBLA &(ZC_IX_&SYM) to save/fetch ix
.*********************************************************************
         ZC_SYM_FIND &SYM
         GBLA  &(ZC_IX_&SYM)
         GBLA  &SYM_IX
&SYM_IX  SETA &(ZC_IX_&SYM)
         MEND
         MACRO
.*********************************************************************
.* Copyright 2008 Automated Software Tools Corporation               *
.* This source code is part of z390 zCOBOL/assembler/emulator package*
.* The z390 package is distributed under GNU general public license  *
.* Author - Don Higgins                                              *
.* Date   - 04/07/08                                                 *
.*********************************************************************
.* 07/27/08 initial coding
.* 10/06/08 ZSTRMAC
.* 10/14/08 USE ZC_SYM_FIND TO ELIM DUP CODE
.* 11/17/08  replace hash calc with GBLA &(ZC_IX_&SYM) to save/fetch ix
.* 07/08/09 RPI 1065 truncate to 63 via ..FIRST 25.._T#N_..REMAINING..
.* 08/10/09 RPI 1065 only replace base name if not proc div alias name
.*********************************************************************
         ZC_SYM_INDEX &SYM,&QIX
         COPY  ZC_WS
         ZC_SYM_FIND &SYM
.*       AIF   (&SYM_IX EQ 0)  NO MATCHING BASE FOUND
           AIF   (NOT(&SYM_IX EQ 0)).AIF_539_1   NO MATCHING BASE FOUND
               GBLA &(ZC_IX_&SYM)
&(ZC_IX_&SYM)  SETA &SYM_TOT
.*       AELSE
           AGO   .AIF_539_E
.AIF_539_1 ANOP
.*             AIF  (&SYM_LVL(&SYM_TOT) EQ 1                           X
                     AND &SYM_LVL(&SYM_IX) EQ 1)  RPI 1065 01'S UNIQUE
                 AIF  (NOT(&SYM_LVL(&SYM_TOT) EQ 1                     X
                           AND &SYM_LVL(&SYM_IX) EQ 1)).AIF_540_1   RPIX
                1065 01'S UNIQUE
                   MNOTE 8,'ZC_SYM_INDEX - DUPLICATE 01 SYMBOLS - &SYM'
.*             AELSE
                 AGO   .AIF_540_E
.AIF_540_1       ANOP
&SYM_DUP_TOT        SETA &SYM_DUP_TOT+1
.*                  AIF (&SYM_FXT(&SYM_IX) EQ 0) IS THIS FIRST DUP
                      AIF (NOT(&SYM_FXT(&SYM_IX) EQ 0)).AIF_541_1  IS TX
               HIS FIRST DUP
&SYM_FXT(&SYM_IX)        SETA &SYM_IX
&SYM_FXT(&SYM_TOT)       SETA &SYM_IX
&SYM_NXT(&SYM_IX)        SETA &SYM_TOT
&QSYM_IX                 SETA &SYM_IX   YES, FIX FIRST
.*                       ACALL FIX_DUP_QUAL_SYM
&ACALL_92_FIX_DUP_QUAL_SYM SETA    1
                           AGO   .ACL_92_FIX_DUP_QUAL_SYM
.ACL_92_1                  ANOP
.*                  AELSE
                      AGO   .AIF_541_E
.AIF_541_1            ANOP
&SYM_FXT(&SYM_TOT)       SETA &SYM_IX
&SYM_NXT(&SYM_TOT)       SETA &SYM_NXT(&SYM_IX)
&SYM_NXT(&SYM_IX)        SETA &SYM_TOT
.*                  AEND
.AIF_541_E            ANOP
&QSYM_IX            SETA &SYM_TOT       FIX NEXT DUP
.*                  ACALL FIX_DUP_QUAL_SYM
&ACALL_92_FIX_DUP_QUAL_SYM SETA    2
                      AGO   .ACL_92_FIX_DUP_QUAL_SYM
.ACL_92_2             ANOP
.*             AEND
.AIF_540_E       ANOP
.*       AEND
.AIF_539_E ANOP
         MEXIT
.*
.* FIX DUP QUALIFIED SYMBOL NAMES TO BE UNIQUE BY ADDING _OF_QUAL
.*
.*       AENTRY FIX_DUP_QUAL_SYM
           AGO   .ACL_92_SKIP
.ACL_92_FIX_DUP_QUAL_SYM ANOP
.*       AIF (&SYM_LVL(&QSYM_IX) EQ 1)  RPI 1065 DON'T CHANGE 01
           AIF (NOT(&SYM_LVL(&QSYM_IX) EQ 1)).AIF_542_1   RPI 1065 DON'X
               T CHANGE 01
.*            AEXIT AENTRY
                AGO   .ACL_92_E
.*       AEND
.AIF_542_1 ANOP
&CUR_QIX SETA &SYM_QIX(&QSYM_IX)  FIRST INDEX LEVEL
&QNAME(1) SETC '&SYM_NAME(&QSYM_IX)' BASE NAME
&QNAME_LEN SETA K'&QNAME(1)
&QNAME_TOT SETA 1
.*       AWHILE (&CUR_QIX GT 0)        RPI 1065 USE ZC_GET_QNAME
.AWH_84_T  ANOP
           AIF    (NOT(&CUR_QIX GT 0)).AWH_84_E         RPI 1065 USE ZCX
               _GET_QNAME
.*            AIF (&QNAME_TOT LT 50)
                AIF (NOT(&QNAME_TOT LT 50)).AIF_543_1
&QNAME_TOT        SETA &QNAME_TOT+1
&QNAME(&QNAME_TOT) SETC '&SYM_NAME(&CUR_QIX)'
&QNAME_LEN        SETA &QNAME_LEN+4+K'&QNAME(&QNAME_TOT)
.*            AELSE
                AGO   .AIF_543_E
.AIF_543_1      ANOP
                  MNOTE 8,'ZC_SYM_INDEX MAX LEVELS EXCEEDED'
                  MEXIT
.*            AEND
.AIF_543_E      ANOP
&CUR_QIX      SETA &SYM_QIX(&CUR_QIX)
.*       AEND
           AGO   .AWH_84_T
.AWH_84_E  ANOP
         ZC_GET_QNAME
         ZC_SYM_FIND &FIELD_NAME
.*       AIF (&SYM_IX GT 0)
           AIF (NOT(&SYM_IX GT 0)).AIF_544_1
             MNOTE 8,'ZC_SYM_INDEX DUP SYM - &FIELD_NAME'
             MEXIT
.*       AEND
.AIF_544_1 ANOP
         GBLA &(ZC_IX_&FIELD_NAME)               CREATE ALIAS FOR FIND
&(ZC_IX_&FIELD_NAME) SETA &QSYM_IX
.*       AIF (NOT &ZC_PROC_DIV) RPI 1065 IS THIS PROC DIV REF
           AIF (NOT(NOT &ZC_PROC_DIV)).AIF_545_1  RPI 1065 IS THIS PROCX
                DIV REF
&SYM_NAME(&QSYM_IX) SETC '&FIELD_NAME' NO, REPLACE NAME
.*       AELSE
           AGO   .AIF_545_E
.AIF_545_1 ANOP
&FIELD_NAME  SETC '&SYM_NAME(&QSYM_IX)' USE PRIOR NAME
.*       AEND
.AIF_545_E ANOP
.*       AEND
.ACL_92_E  ANOP
           AGO   (&ACALL_92_FIX_DUP_QUAL_SYM).ACL_92_1,.ACL_92_2
.ACL_92_SKIP ANOP
         MEND
.*
.* GET DFHRESP - SET &DFHLIT FOR &DFHRESP
.*
.*       AENTRY GET_DFHRESP
           AGO   .ACL_39_SKIP
.ACL_39_GET_DFHRESP ANOP
         GBLB  &ZC_DFHRESP
.*       AIF   (NOT &ZC_DFHRESP)
           AIF   (NOT(NOT &ZC_DFHRESP)).AIF_546_1
&ZC_DFHRESP    SETB 1
               COPY ZC_DFHRESP                            RPI 1057
.*       AEND
.AIF_546_1 ANOP
&DFHSYM  SETC 'DFHRESP_&DFHRESP'
         GBLC  &(&DFHSYM)
&DFHLIT  SETC '&(&DFHSYM)'
.*       AIF   (K'&DFHLIT EQ 0)
           AIF   (NOT(K'&DFHLIT EQ 0)).AIF_547_1
               MNOTE 8,'ZCDFHLIT DFHRESP VALUE NOT FOUND - &DFHRESP'
               MEXIT
.*       AEND
.AIF_547_1 ANOP
.*       AEND
           AGO   (&ACALL_39_GET_DFHRESP).ACL_39_1,.ACL_39_2
.ACL_39_SKIP ANOP
.*
.* GET DFHVALUE - SET &DFHLIT FOR &DFHVALUE
.*
.*       AENTRY GET_DFHVALUE
           AGO   .ACL_40_SKIP
.ACL_40_GET_DFHVALUE ANOP
         GBLB  &ZC_DFHVALUE
.*       AIF   (NOT &ZC_DFHVALUE)
           AIF   (NOT(NOT &ZC_DFHVALUE)).AIF_548_1
&ZC_DFHVALUE   SETB 1
               COPY ZC_DFHVALUE                           RPI 1057
.*       AEND
.AIF_548_1 ANOP
&DFHSYM  SETC 'DFHVALUE_&DFHVALUE'
         GBLC  &(&DFHSYM)
&DFHLIT  SETC '&(&DFHSYM)'
.*       AIF   (K'&DFHLIT EQ 0)
           AIF   (NOT(K'&DFHLIT EQ 0)).AIF_549_1
               MNOTE 8,'ZCDFHLIT DFHVALUE NOT FOUND - &DFHVALUE'
               MEXIT
.*       AEND
.AIF_549_1 ANOP
.*       AEND
           AGO   (&ACALL_40_GET_DFHVALUE).ACL_40_1,.ACL_40_2
.ACL_40_SKIP ANOP
.*********************************************************************
.* Copyright 2008 Automated Software Tools Corporation               *
.* This source code is part of z390 zCOBOL/assembler/emulator package*
.* The z390 package is distributed under GNU general public license  *
.* Author - Don Higgins                                              *
.* Date   - 04/07/08                                                 *
.*********************************************************************
.* 02/23/08 RPI 1001 ADD SUPPORT FOR SIX ON CONDITION 88 LEVEL FIELD
.* 03/17/08 RPI 1004 COMBINE - WITH LITERAL VALUE
.* 07/09/09 RPI 1065 ONLY COMBINE +- IF FIELD IS ONLY +-
.* 07/17/09 RPI 1065 TRUNC QUAL NAMES GT 63 USING ZC_GET_QNAME
.* 08/11/09 RPI 1065 REPLACE ZC_GET_QNAME USING ONLY FIND_QUAL_ALIAS
.*********************************************************************
.* GET RX ADDRESSABLE QUALIFIED PARM FIELD FROM &SYSLIST(&PARM_IX)
.* INPUT
.*  1. &PARM_IX   SETA MUST BE SET TO CURRENT SYSLIST PARM
.*  2. &FIELD_REG SETC MUST BE SET TO TARGET REG FOR SUBSC VAR ADDR
.*  3. &FIELD_SS1 SETB MUST BE SET TO 1 IF OFF(LEN,REG) REQ VS OFF(REG)
.* OUTPUT
.*  1. &FIELD_IX   SETA = SYM_IX FOR FIELD IF FIELD FOUND ELSE 0 (LIT)
.*  2. &FIELD_SIX  SETB = 1 IF ADDR IN FIELD_REG DUE TO SUBS/INDEX/RMOD
.*  3. &FIELD_RMD  SETB = 1 IF (OFF:LEN) RFF. MOD. CHANGED FOLLOWING:
.*  3. &FIELD_OFF  SETA = OFFSET DEFAULT 0 MAY BE CHANGED BY RMOD
.*  4. &FIELD_LEN  SETA = LENGTH DEFAULT SYM_LEN(FIELD_IX) RMOD CHNG
.*  5. &FIELD_NAME SETC = FIELD NAME, OFF(REG), OR OFF(LEN,REG) IF SS1
.*  6. &FIELD_88_IX SETA - 88 LVL INDEX ELSE 0
.* WHERE USED:
.*  1. ADD, DIVIDE, IF, GO, INSPECT, MULTIPLY, SUBTRACT, TRANSFORM
.*     GEN_DISPLAY.
.* USES:
.*  1. ZC_SYM_FIND  - find field &FIELD_NAME and set &FIELD_IX
.*  2. ZC_SYM_INDEX - resolve duplicate names if qualified
.*  3. ZC_GET_QNAME - build qualified name with turncation if > 63
.* NOTES
.*  1. May generate base setting code if required
.*  2. May generate subscript code if required
.*  3. Errors - not found, no base, missing subsc., invalid subsc.
.*********************************************************************
.*       AENTRY GET_PARM_FIELD
           AGO   .ACL_6_SKIP
.ACL_6_GET_PARM_FIELD ANOP
&QNAME(1) SETC '&SYSLIST(&PARM_IX)'
&QNAME_LEN SETA K'&QNAME(1)    RPI 1065 CALC QUAL LENGTH
&QNAME_TOT SETA 1
&FIELD_NAME SETC '&QNAME(1)'
&FIELD_IX SETA 0           RPI 1065 ASSUME NOT FOUND
&FIELD_SIX SETB 0           ASSUME NO SUBSCRIPT/INDEX
&FIELD_RMD SETB 0           ASSUME NO REF. MOD. (OFF:LEN)
&PARM_IX SETA &PARM_IX+1
.*       AIF ('&FIELD_NAME' EQ '-' OR '&FIELD_NAME' EQ '+') RPI 1065
           AIF (NOT('&FIELD_NAME' EQ '-' OR '&FIELD_NAME' EQ '+')).AIF_X
               550_1  RPI 1065
&FIELD_NAME  SETC '&FIELD_NAME&SYSLIST(&PARM_IX)'
&PARM_IX     SETA &PARM_IX+1
.*           AEXIT AENTRY
               AGO   .ACL_6_E
.*       AEND
.AIF_550_1 ANOP
.*       AWHILE ('&SYSLIST(&PARM_IX)' EQ 'OF'                          X
               OR '&SYSLIST(&PARM_IX)' EQ 'IN')
.AWH_85_T  ANOP
           AIF    (NOT('&SYSLIST(&PARM_IX)' EQ 'OF'                    X
                     OR '&SYSLIST(&PARM_IX)' EQ 'IN')).AWH_85_E
.*             AIF (&QNAME_TOT GT 50)
                 AIF (NOT(&QNAME_TOT GT 50)).AIF_551_1
                   MNOTE 8,'ZCGENFLD MAX QUAL LEVELS EXCEEDED'
                   MEXIT
.*             AEND
.AIF_551_1       ANOP
&QNAME_TOT     SETA &QNAME_TOT+1
&QNAME(&QNAME_TOT) SETC '&SYSLIST(&PARM_IX+1)'
&QNAME_LEN     SETA &QNAME_LEN+4+K'&QNAME(&QNAME_TOT)
&PARM_IX       SETA &PARM_IX+2
.*       AEND
           AGO   .AWH_85_T
.AWH_85_E  ANOP
         ZC_SYM_FIND &FIELD_NAME
&FIELD_IX SETA &SYM_IX
.*       AIF   (&QNAME_TOT EQ 1)
           AIF   (NOT(&QNAME_TOT EQ 1)).AIF_552_1
.*             AIF   (&FIELD_IX EQ 0)  FIELD NOT FOUND
                 AIF   (NOT(&FIELD_IX EQ 0)).AIF_553_1   FIELD NOT FOUNX
               D
.*                   AEXIT AENTRY
                       AGO   .ACL_6_E
.*             AEND
.AIF_553_1       ANOP
.*       AELSE
           AGO   .AIF_552_E
.AIF_552_1 ANOP
.*             ACALL FIND_QUAL_ALIAS  FIND ALIAS OF QUAL FIELD
&ACALL_93_FIND_QUAL_ALIAS SETA    1
                 AGO   .ACL_93_FIND_QUAL_ALIAS
.ACL_93_1        ANOP
.*       AEND
.AIF_552_E ANOP
.*       AIF   (&FIELD_IX GT 0)
           AIF   (NOT(&FIELD_IX GT 0)).AIF_554_1
.*             AIF (&SYM_LVL(&FIELD_IX) EQ 88)
                 AIF (NOT(&SYM_LVL(&FIELD_IX) EQ 88)).AIF_555_1
&FIELD_88_IX       SETA &FIELD_IX
&FIELD_IX          SETA &SYM_REF(&FIELD_88_IX)
.*             AELSE
                 AGO   .AIF_555_E
.AIF_555_1       ANOP
&FIELD_88_IX       SETA 0
.*             AEND
.AIF_555_E       ANOP
&FIELD_NAME    SETC '&SYM_NAME(&FIELD_IX)'
.*             AIF (NOT &ZC_PROC_DIV) RPI 1065 NO BASE/SIX IN WS
                 AIF (NOT(NOT &ZC_PROC_DIV)).AIF_556_1  RPI 1065 NO BASX
               E/SIX IN WS
.*                 AEXIT AENTRY
                     AGO   .ACL_6_E
.*             AEND
.AIF_556_1       ANOP
               GEN_BASE &FIELD_IX  GEN BASE IF REQ'D FOR RX ACCESS
.*             AIF   ('&SYSLIST(&PARM_IX)' EQ '''(''') SUBSC FOUND
                 AIF   (NOT('&SYSLIST(&PARM_IX)' EQ '''(''')).AIF_557_1X
                 SUBSC FOUND
.*                   AIF (&SYM_SIT(&FIELD_IX) GT 0)
                       AIF (NOT(&SYM_SIT(&FIELD_IX) GT 0)).AIF_558_1
.*                       ACALL GEN_SUBSCRIPT
&ACALL_94_GEN_SUBSCRIPT    SETA    1
                           AGO   .ACL_94_GEN_SUBSCRIPT
.ACL_94_1                  ANOP
.*                       AIF ('&SYSLIST(&PARM_IX)' EQ '''(''') RMOD FND
                           AIF (NOT('&SYSLIST(&PARM_IX)' EQ '''(''')).AX
               IF_559_1  RMOD FND
.*                           ACALL CALC_RMOD_OFF_LEN
&ACALL_95_CALC_RMOD_OFF_LEN    SETA    1
                               AGO   .ACL_95_CALC_RMOD_OFF_LEN
.ACL_95_1                      ANOP
.*                       AEND
.AIF_559_1                 ANOP
.*                   AELSE
                       AGO   .AIF_558_E
.AIF_558_1             ANOP
.*                       ACALL CALC_RMOD_OFF_LEN
&ACALL_95_CALC_RMOD_OFF_LEN SETA    2
                           AGO   .ACL_95_CALC_RMOD_OFF_LEN
.ACL_95_2                  ANOP
.*                   AEND
.AIF_558_E             ANOP
.*             AELSEIF ('&SYM_SIT(&FIELD_IX)' GT 0)
                 AGO   .AIF_557_E
.AIF_557_1       ANOP
                 AIF     (NOT('&SYM_SIT(&FIELD_IX)' GT 0)).AIF_557_2
                     MNOTE 8,'ZCGETFLD SUBSCRIPT MISSING FOR &FIELD_NAMX
               E'
                     MEXIT
.*             AEND
.AIF_557_2       ANOP
.AIF_557_E       ANOP
.*       AEND
.AIF_554_1 ANOP
.*       AEND
.ACL_6_E   ANOP
           AGO   (&ACALL_6_GET_PARM_FIELD).ACL_6_1,.ACL_6_2,.ACL_6_3,.AX
               CL_6_4,.ACL_6_5,.ACL_6_6,.ACL_6_7,.ACL_6_8,.ACL_6_9,.ACLX
               _6_10,.ACL_6_11,.ACL_6_12,.ACL_6_13,.ACL_6_14,.ACL_6_15,X
               .ACL_6_16,.ACL_6_17,.ACL_6_18,.ACL_6_19,.ACL_6_20,.ACL_6X
               _21,.ACL_6_22,.ACL_6_23,.ACL_6_24,.ACL_6_25,.ACL_6_26,.AX
               CL_6_27,.ACL_6_28,.ACL_6_29,.ACL_6_30,.ACL_6_31,.ACL_6_3X
               2,.ACL_6_33,.ACL_6_34,.ACL_6_35,.ACL_6_36,.ACL_6_37,.ACLX
               _6_38
.ACL_6_SKIP ANOP
.*
.* FIND QUAL ALIAS CREATE ALIAS AND SET FIELD NAME RPI 1062
.*
.*       AENTRY FIND_QUAL_ALIAS
           AGO   .ACL_93_SKIP
.ACL_93_FIND_QUAL_ALIAS ANOP
&CUR_SYM_IX SETA &SYM_FXT(&FIELD_IX)
.*       AIF (&CUR_SYM_IX EQ 0)   ARE THERE DUP SYMBOLS
           AIF (NOT(&CUR_SYM_IX EQ 0)).AIF_560_1    ARE THERE DUP SYMBOX
               LS
.*           AEXIT AENTRY
               AGO   .ACL_93_E
.*       AEND
.AIF_560_1 ANOP
.*       AWHILE (&CUR_SYM_IX GT 0)  SEARCH DUP SYMBOLS FOR MATCH
.AWH_86_T  ANOP
           AIF    (NOT(&CUR_SYM_IX GT 0)).AWH_86_E   SEARCH DUP SYMBOLSX
                FOR MATCH
               ACTR 4096
&QI            SETA 2
&CUR_QIX       SETA &SYM_QIX(&CUR_SYM_IX)
&QNAME_FOUND   SETB 1
.*             AWHILE (&QNAME_FOUND AND &QI LE &QNAME_TOT)
.AWH_87_T        ANOP
                 AIF    (NOT(&QNAME_FOUND AND &QI LE &QNAME_TOT)).AWH_8X
               7_E
.*                 ACALL FIND_QNAME
&ACALL_96_FIND_QNAME SETA    1
                     AGO   .ACL_96_FIND_QNAME
.ACL_96_1            ANOP
.*                 AIF (&QNAME_FOUND)
                     AIF (NOT(&QNAME_FOUND)).AIF_561_1
.*                     AIF (&QI EQ &QNAME_TOT)
                         AIF (NOT(&QI EQ &QNAME_TOT)).AIF_562_1
&FIELD_NAME                SETC '&SYM_NAME(&CUR_SYM_IX)'
&FIELD_IX                  SETA &CUR_SYM_IX  RPI 1062
.*                         AEXIT AENTRY
                             AGO   .ACL_93_E
.*                     AELSE
                         AGO   .AIF_562_E
.AIF_562_1               ANOP
&QI                        SETA &QI+1  NEXT QNAME. NEXT LVL
&CUR_QIX                   SETA &SYM_QIX(&CUR_QIX_TEST)
.*                     AEND
.AIF_562_E               ANOP
.*                 AEND
.AIF_561_1           ANOP
.*             AEND
                 AGO   .AWH_87_T
.AWH_87_E        ANOP
&CUR_SYM_IX    SETA &SYM_NXT(&CUR_SYM_IX)  NEXT DUP SYM
.*       AEND
           AGO   .AWH_86_T
.AWH_86_E  ANOP
         MNOTE 8,'ZCGETFLD QUAL FIELD NOT FOUND - &FIELD_NAME'
.*       AEND
.ACL_93_E  ANOP
           AGO   (&ACALL_93_FIND_QUAL_ALIAS).ACL_93_1
.ACL_93_SKIP ANOP
.*
.* FIND SYM LEVEL MATCHING QNAME(QI) STARTING AT SYM_NAME(CUR_QIX)
.* RETURN WITH QNAME_FOUND T/F
.*
.*       AENTRY FIND_QNAME
           AGO   .ACL_96_SKIP
.ACL_96_FIND_QNAME ANOP
&CUR_QIX_TEST SETA &CUR_QIX
.*       AWHILE (&CUR_QIX_TEST GT 0)
.AWH_88_T  ANOP
           AIF    (NOT(&CUR_QIX_TEST GT 0)).AWH_88_E
.*             AIF   ('&QNAME(&QI)' EQ '&SYM_NAME(&CUR_QIX_TEST)')
                 AIF   (NOT('&QNAME(&QI)' EQ '&SYM_NAME(&CUR_QIX_TEST)'X
               )).AIF_563_1
.*                   AEXIT AENTRY
                       AGO   .ACL_96_E
.*             AELSEIF ('&QNAME(&QI)_OF_' EQ '&SYM_NAME(&CUR_QIX_TEST)'X
               (1,K'&QNAME(&QI)+4))  CHECK BASE OF QUALIFIED LEVEL
                 AGO   .AIF_563_E
.AIF_563_1       ANOP
                 AIF     (NOT('&QNAME(&QI)_OF_' EQ '&SYM_NAME(&CUR_QIX_X
               TEST)'(1,K'&QNAME(&QI)+4))).AIF_563_2   CHECK BASE OF QUX
               ALIFIED LEVEL
.*                   AEXIT AENTRY
                       AGO   .ACL_96_E
.*             AELSE
                 AGO   .AIF_563_E
.AIF_563_2       ANOP
.*             AEND
.AIF_563_E       ANOP
&CUR_QIX_TEST  SETA &SYM_QIX(&CUR_QIX_TEST)  NEXT LVL
.*       AEND
           AGO   .AWH_88_T
.AWH_88_E  ANOP
&QNAME_FOUND SETB 0  NOT FOUND, FORCE NEXT DUP SYM SEARCH
.*       AEND
.ACL_96_E  ANOP
           AGO   (&ACALL_96_FIND_QNAME).ACL_96_1
.ACL_96_SKIP ANOP
.*
.* PARSE AND GENERATE CODE FOR FIELD WITH 1 OR MORE SUBSCRIPTS
.*
.*       AENTRY GEN_SUBSCRIPT
           AGO   .ACL_94_SKIP
.ACL_94_GEN_SUBSCRIPT ANOP
&TOT_SIX SETA &SYM_SIT(&FIELD_IX)
&PARM_IX SETA &PARM_IX+1    POS TO FIRST SUBSCRIPT/INDEX
&CUR_SYM_IX SETA &FIELD_IX  CUR SYM WITH OCCURS
.*       AIF   (&SYM_DUP(&CUR_SYM_IX) LE 1)
           AIF   (NOT(&SYM_DUP(&CUR_SYM_IX) LE 1)).AIF_564_1
.*             ACALL GET_NEXT_OCCURS_SYM
&ACALL_97_GET_NEXT_OCCURS_SYM SETA    1
                 AGO   .ACL_97_GET_NEXT_OCCURS_SYM
.ACL_97_1        ANOP
.*       AEND
.AIF_564_1 ANOP
&CUR_SIX SETA 0
.*       AWHILE (&PARM_IX LE N'&SYSLIST                                X
                 AND &CUR_SYM_IX GT 0                                  X
                 AND &CUR_SIX LE &TOT_SIX                              X
                 AND '&SYSLIST(&PARM_IX)' NE ''')''')
.AWH_89_T  ANOP
           AIF    (NOT(&PARM_IX LE N'&SYSLIST                          X
                       AND &CUR_SYM_IX GT 0                            X
                       AND &CUR_SIX LE &TOT_SIX                        X
                       AND '&SYSLIST(&PARM_IX)' NE ''')''')).AWH_89_E
&CUR_SIX       SETA &CUR_SIX+1
&SIX_NAME(&CUR_SIX) SETC '&SYSLIST(&PARM_IX)' NAME/LIT
&SIX_OCCUR(&TOT_SIX-&CUR_SIX+1) SETA &CUR_SYM_IX
.*             AIF ('&SYSLIST(&PARM_IX+1)' EQ '+'                      X
                    OR '&SYSLIST(&PARM_IX+1)' EQ '-')
                 AIF (NOT('&SYSLIST(&PARM_IX+1)' EQ '+'                X
                          OR '&SYSLIST(&PARM_IX+1)' EQ '-')).AIF_565_1
&SIX_OFF(&CUR_SIX)  SETC '&SYSLIST(&PARM_IX+1)&SYSLIST(&PARM_IX+2)'
&PARM_IX            SETA &PARM_IX+2
.*             AELSE
                 AGO   .AIF_565_E
.AIF_565_1       ANOP
&SIX_OFF(&CUR_SIX)  SETC ''
.*             AEND
.AIF_565_E       ANOP
&PARM_IX       SETA &PARM_IX+1
.*             ACALL GET_NEXT_OCCURS_SYM
&ACALL_97_GET_NEXT_OCCURS_SYM SETA    2
                 AGO   .ACL_97_GET_NEXT_OCCURS_SYM
.ACL_97_2        ANOP
.*       AEND
           AGO   .AWH_89_T
.AWH_89_E  ANOP
.*       AIF   (&CUR_SIX NE &TOT_SIX                                   X
               OR '&SYSLIST(&PARM_IX) NE ''')''')
           AIF   (NOT(&CUR_SIX NE &TOT_SIX                             X
                     OR '&SYSLIST(&PARM_IX) NE ''')''')).AIF_566_1
               MNOTE 8,'ZCGETFLD SUBSCRIPT MISMATCH'
               MEXIT
.*       AEND
.AIF_566_1 ANOP
&PARM_IX SETA &PARM_IX+1
.*
.* GENERATE SUBSCRIPT CODE
.*
         GEN_SIX
.*       AEND
           AGO   (&ACALL_94_GEN_SUBSCRIPT).ACL_94_1
.ACL_94_SKIP ANOP
.*
.* GET NEXT OCCURS SYM
.*
.*     AENTRY GET_NEXT_OCCURS_SYM
         AGO   .ACL_97_SKIP
.ACL_97_GET_NEXT_OCCURS_SYM ANOP
&CUR_SYM_IX SETA &SYM_QIX(&CUR_SYM_IX)
.*     AWHILE (&CUR_SYM_IX GT 0)
.AWH_90_T ANOP
         AIF    (NOT(&CUR_SYM_IX GT 0)).AWH_90_E
.*        AIF (&SYM_DUP(&CUR_SYM_IX) GT 1)
            AIF (NOT(&SYM_DUP(&CUR_SYM_IX) GT 1)).AIF_567_1
.*            AEXIT AENTRY  EXIT WITH NEXT OCCURS CUR_SYM_IX
                AGO   .ACL_97_E
.*        AEND
.AIF_567_1  ANOP
&CUR_SYM_IX SETA &SYM_QIX(&CUR_SYM_IX)
.*     AEND
         AGO   .AWH_90_T
.AWH_90_E ANOP
.*     AEND
.ACL_97_E ANOP
         AGO   (&ACALL_97_GET_NEXT_OCCURS_SYM).ACL_97_1,.ACL_97_2
.ACL_97_SKIP ANOP
.*
.* CALC RMOD OFF SET - CALC OFFSET AND LENGTH FROM (OFF:LEN)
.*
.*       AENTRY CALC_RMOD_OFF_LEN
           AGO   .ACL_95_SKIP
.ACL_95_CALC_RMOD_OFF_LEN ANOP
.*       AIF   ('&SYSLIST(&PARM_IX+2)' EQ ':'                          X
               AND '&SYSLIST(&PARM_IX+4)' EQ ''')''')
           AIF   (NOT('&SYSLIST(&PARM_IX+2)' EQ ':'                    X
                     AND '&SYSLIST(&PARM_IX+4)' EQ ''')''')).AIF_568_1
&FIELD_OFF     SETA &SYSLIST(&PARM_IX+1)-1
&FIELD_LEN     SETA &SYSLIST(&PARM_IX+3)
.*             AIF (&FIELD_OFF+&FIELD_LEN GT &SYM_LEN(&FIELD_IX))
                 AIF (NOT(&FIELD_OFF+&FIELD_LEN GT &SYM_LEN(&FIELD_IX))X
               ).AIF_569_1
                   MNOTE 8,'ZCGETFLD RMOD OFF+LEN > FIELD LENGTH'
                   MEXIT
.*             AEND
.AIF_569_1       ANOP
.*             AIF (&FIELD_SS1)
                 AIF (NOT(&FIELD_SS1)).AIF_570_1
.*                 AIF (&FIELD_SIX)
                     AIF (NOT(&FIELD_SIX)).AIF_571_1
&FIELD_NAME            SETC '&FIELD_OFF.(&FIELD_LEN,&FIELD_REG)'
.*                 AELSE
                     AGO   .AIF_571_E
.AIF_571_1           ANOP
&FIELD_NAME            SETC '&FIELD_NAME+&FIELD_OFF.(&FIELD_LEN)'
.*                 AEND
.AIF_571_E           ANOP
.*             AELSE
                 AGO   .AIF_570_E
.AIF_570_1       ANOP
.*                 AIF (&FIELD_SIX)
                     AIF (NOT(&FIELD_SIX)).AIF_572_1
&FIELD_NAME            SETC '&FIELD_OFF.(&FIELD_REG)'
.*                 AELSE
                     AGO   .AIF_572_E
.AIF_572_1           ANOP
&FIELD_NAME            SETC '&FIELD_NAME+&FIELD_OFF'
.*                 AEND
.AIF_572_E           ANOP
.*             AEND
.AIF_570_E       ANOP
&PARM_IX       SETA &PARM_IX+5
.*       AELSE
           AGO   .AIF_568_E
.AIF_568_1 ANOP
               MNOTE 8,'ZCGETFLD RMOD SYNTAX ERROR (LIT:LIT) ONLY'
               MEXIT
.*       AEND
.AIF_568_E ANOP
.*       AEND
           AGO   (&ACALL_95_CALC_RMOD_OFF_LEN).ACL_95_1,.ACL_95_2
.ACL_95_SKIP ANOP
.*
.* END OF ZCGETFLD.CPY
.*
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
.*     AENTRY  GET_PG_LAB
         AGO   .ACL_98_SKIP
.ACL_98_GET_PG_LAB ANOP
.*     AIF     ('&PG_NAME' EQ '')
         AIF     (NOT('&PG_NAME' EQ '')).AIF_573_1
&ZC_PG_LAB     SETC 'SN_&SN_NAME'
.*             AEXIT AENTRY
                 AGO   .ACL_98_E
.*     AEND
.AIF_573_1 ANOP
       GBLA    &(ZC_PG_&PG_NAME)
.*     AIF     (&(ZC_PG_&PG_NAME) GT 0) IS THIS PG DEFINED
         AIF     (NOT(&(ZC_PG_&PG_NAME) GT 0)).AIF_574_1  IS THIS PG DEX
               FINED
.*             AIF     (&(ZC_PG_&PG_NAME) GT 1) IS IT DUPLICATED
                 AIF     (NOT(&(ZC_PG_&PG_NAME) GT 1)).AIF_575_1  IS ITX
                DUPLICATED
.*                     AIF  ('&SN_NAME' NE '')  IS THERE EXPLICIT SN
                         AIF  (NOT('&SN_NAME' NE '')).AIF_576_1   IS THX
               ERE EXPLICIT SN
&ZC_PG_LAB                  SETC 'SN_&SN_NAME._PG_&PG_NAME'
.*                     AELSEIF ('&LAST_SN_NAME' NE '') ARE WE IN SN
                         AGO   .AIF_576_E
.AIF_576_1               ANOP
                         AIF     (NOT('&LAST_SN_NAME' NE '')).AIF_576_2X
                 ARE WE IN SN
&ZC_PG_LAB                  SETC 'SN_&LAST_SN_NAME._PG_&PG_NAME'
.*                     AELSE
                         AGO   .AIF_576_E
.AIF_576_2               ANOP
                            MNOTE 8,'GEN_B NO SECTION FOR DUP PG - &PG_X
               NAME'
                            MEXIT
.*                     AEND
.AIF_576_E               ANOP
.*             AELSE
                 AGO   .AIF_575_E
.AIF_575_1       ANOP
&ZC_PG_LAB         SETC 'PG_&PG_NAME'
.*             AEND
.AIF_575_E       ANOP
.*     AELSEIF (&(ZC_PG_&PG_NAME) EQ -1)  IS THIS SECTION
         AGO   .AIF_574_E
.AIF_574_1 ANOP
         AIF     (NOT(&(ZC_PG_&PG_NAME) EQ -1)).AIF_574_2   IS THIS SECX
               TION
&ZC_PG_LAB     SETC 'SN_&PG_NAME'
.*     AELSE
         AGO   .AIF_574_E
.AIF_574_2 ANOP
&ZC_PG_LAB     SETC 'PG_&PG_NAME'
.*     AEND
.AIF_574_E ANOP
.*     AEND
 MNOTE 8'ZSTRMAC ERROR AENTRY GET_PG_LAB NOT USED
.ACL_98_SKIP ANOP
.*
.* SCALE NUL LIT - REMOVE DECIMAL POINT AND SCALE FIXED NUMERIC LITERAL
.*    &NUM     - SETC LITERAL VALUE WHICH IS SCALED
.*    &DEC_SYM - DECIMAL PLACES REQUIRED FROM SYM_PIC_DEC (SET BY WS)
.*
.*       AENTRY SCALE_NUM_LIT
           AGO   .ACL_73_SKIP
.ACL_73_SCALE_NUM_LIT ANOP
.*       AIF (&ZC_DEC_IS_COMMA)
           AIF (NOT(&ZC_DEC_IS_COMMA)).AIF_577_1
&DEC_IX      SETA ('&NUM' FIND ',')
.*       AELSE
           AGO   .AIF_577_E
.AIF_577_1 ANOP
&DEC_IX      SETA ('&NUM' FIND '.')
.*       AEND
.AIF_577_E ANOP
.*       AIF (&DEC_IX GT 0)
           AIF (NOT(&DEC_IX GT 0)).AIF_578_1
&DEC_NUM       SETA K'&NUM-&DEC_IX
.*             AIF (&DEC_IX GT 1)
                 AIF (NOT(&DEC_IX GT 1)).AIF_579_1
.*               AIF (&DEC_IX LT K'&NUM)
                   AIF (NOT(&DEC_IX LT K'&NUM)).AIF_580_1
&NUM               SETC '&NUM'(1,&DEC_IX-1).'&NUM'(&DEC_IX+1,*)
.*               AELSE
                   AGO   .AIF_580_E
.AIF_580_1         ANOP
&NUM               SETC '&NUM'(1,&DEC_IX-1)
.*               AEND
.AIF_580_E         ANOP
.*             AELSE
                 AGO   .AIF_579_E
.AIF_579_1       ANOP
&NUM               SETC '&NUM'(2,*)
.*             AEND
.AIF_579_E       ANOP
.*       AELSE
           AGO   .AIF_578_E
.AIF_578_1 ANOP
&DEC_NUM       SETA 0
.*       AEND
.AIF_578_E ANOP
.*       AIF (&DEC_NUM EQ &DEC_SYM)
           AIF (NOT(&DEC_NUM EQ &DEC_SYM)).AIF_581_1
.*           AEXIT AENTRY
               AGO   .ACL_73_E
.*       AEND
.AIF_581_1 ANOP
.*       AIF (&DEC_NUM GT &DEC_SYM)
           AIF (NOT(&DEC_NUM GT &DEC_SYM)).AIF_582_1
&LNUM        SETA K'&NUM-(&DEC_NUM-&DEC_SYM)
.*           AIF (&LNUM GT 0)
               AIF (NOT(&LNUM GT 0)).AIF_583_1
&NUM             SETC '&NUM'(1,&LNUM)
.*           AELSE
               AGO   .AIF_583_E
.AIF_583_1     ANOP
&NUM             SETC '0'
.*           AEND
.AIF_583_E     ANOP
.*       AELSE
           AGO   .AIF_582_E
.AIF_582_1 ANOP
&NUM         SETC '&NUM'.(&DEC_SYM-&DEC_NUM)'0'
.*       AEND
.AIF_582_E ANOP
.*       AEND
.ACL_73_E  ANOP
           AGO   (&ACALL_73_SCALE_NUM_LIT).ACL_73_1,.ACL_73_2,.ACL_73_3
.ACL_73_SKIP ANOP
.********************************************************************
.* RESET LAB LEN - REMOVE SS1 LENGTH IF ANY FROM LAB
.* USED BY ZC_CALC, GEN_ADD
.********************************************************************
.* 05/26/09 RPI 1019 INITIAL CODING SEE TESTSIX1 CALC TEST
.********************************************************************
.*       AENTRY RESET_LAB_LEN
           AGO   .ACL_88_SKIP
.ACL_88_RESET_LAB_LEN ANOP
.*       AIF   ('&LAB'(1,1) GE '0')  IS THIS O(L,R)
           AIF   (NOT('&LAB'(1,1) GE '0')  IS THIS O(L,R)).AIF_584_1
&ILAB          SETA ('&LAB' INDEX '(')
.*             AIF (&ILAB GT 1)
                 AIF (NOT(&ILAB GT 1)).AIF_585_1
&FIELD_OFF         SETA '&LAB'(1,&ILAB-1)
&JLAB              SETA ('&LAB' INDEX ',')
.*                 AIF (&JLAB GT &ILAB+1)
                     AIF (NOT(&JLAB GT &ILAB+1)).AIF_586_1
&FIELD_LEN           SETA '&LAB'(&ILAB+1,&JLAB-1)
&FIELD_REG           SETC '&LAB'(&JLAB+1,K'&LAB-&JLAB-1)
&LAB                 SETC '&LAB'(1,&ILAB).'&LAB'(&JLAB+1,*) O(R)
.*                 AEND
.AIF_586_1           ANOP
.*             AEND
.AIF_585_1       ANOP
.*       AELSEIF ('&LAB'(1,5) EQ 'ZCVT_') STRIP ZCVT_WORKAREA+N(LEN)
           AGO   .AIF_584_E
.AIF_584_1 ANOP
           AIF     (NOT('&LAB'(1,5) EQ 'ZCVT_') STRIP ZCVT_WORKAREA+N(LX
               EN)).AIF_584_2
&ILAB          SETA ('&LAB' INDEX '(')
.*             AIF (&ILAB GT 1)
                 AIF (NOT(&ILAB GT 1)).AIF_587_1
&LAB               SETC '&LAB'(1,&ILAB-1)
.*             AEND
.AIF_587_1       ANOP
.*       AEND
.AIF_584_2 ANOP
.AIF_584_E ANOP
.*       AEND
           AGO   (&ACALL_88_RESET_LAB_LEN).ACL_88_1,.ACL_88_2
.ACL_88_SKIP ANOP
.*********************************************************************
.* Copyright 2008 Automated Software Tools Corporation               *
.* This source code is part of z390 zCOBOL/assembler/emulator package*
.* The z390 package is distributed under GNU general public license  *
.* Author - Don Higgins                                              *
.* Date   - 04/07/08                                                 *
.*********************************************************************
.* 06/14/09 RPI 1057 sync with AZ390 DFHRESP and DFHVALUE support
.* 09/02/09 RPI 1079 add DFHRESP(NOTALLOC)=F'61'
.*********************************************************************
               GBLC &(DFHRESP_NORMAL)
               GBLC &(DFHRESP_ERROR)
               GBLC &(DFHRESP_TERMIDERR)
               GBLC &(DFHRESP_FILENOTFOUND)
               GBLC &(DFHRESP_DSIDERR)
               GBLC &(DFHRESP_NOTFND)
               GBLC &(DFHRESP_DUPREC)
               GBLC &(DFHRESP_DUPKEY)
               GBLC &(DFHRESP_INVREQ)
               GBLC &(DFHRESP_IOERR)
               GBLC &(DFHRESP_NOSPACE)
               GBLC &(DFHRESP_NOTOPEN)
               GBLC &(DFHRESP_ENDFILE)
               GBLC &(DFHRESP_ILLOGIC)
               GBLC &(DFHRESP_LENGERR)
               GBLC &(DFHRESP_ITEMERR)
               GBLC &(DFHRESP_PGMIDERR)
               GBLC &(DFHRESP_TRANSIDERR)
               GBLC &(DFHRESP_ENDDATA)
               GBLC &(DFHRESP_EXPIRED)
               GBLC &(DFHRESP_MAPFAIL)
               GBLC &(DFHRESP_INVMPSZ)
               GBLC &(DFHRESP_OVERFLOW)
               GBLC &(DFHRESP_QIDERR)
               GBLC &(DFHRESP_ENQBUSY)
               GBLC &(DFHRESP_ENVDEFERR)
               GBLC &(DFHRESP_NOTALLOC)     RPI 1079
               GBLC &(DFHRESP_SUPPRESSED)
               GBLC &(DFHRESP_END)
               GBLC &(DFHRESP_DISABLED)
&(DFHRESP_NORMAL) SETC   '0'
&(DFHRESP_ERROR) SETC   '1'
&(DFHRESP_TERMIDERR) SETC   '11'
&(DFHRESP_FILENOTFOUND) SETC   '12'
&(DFHRESP_DSIDERR) SETC   '12'
&(DFHRESP_NOTFND) SETC   '13'
&(DFHRESP_DUPREC) SETC   '14'
&(DFHRESP_DUPKEY) SETC   '15'
&(DFHRESP_INVREQ) SETC   '16'
&(DFHRESP_IOERR) SETC   '17'
&(DFHRESP_NOSPACE) SETC   '18'
&(DFHRESP_NOTOPEN) SETC   '19'
&(DFHRESP_ENDFILE) SETC   '20'
&(DFHRESP_ILLOGIC) SETC   '21'
&(DFHRESP_LENGERR) SETC   '22'
&(DFHRESP_ITEMERR) SETC   '26'
&(DFHRESP_PGMIDERR) SETC   '27'
&(DFHRESP_TRANSIDERR) SETC   '28'
&(DFHRESP_ENDDATA) SETC   '29'
&(DFHRESP_EXPIRED) SETC   '31'
&(DFHRESP_MAPFAIL) SETC   '36'
&(DFHRESP_INVMPSZ) SETC   '38'
&(DFHRESP_OVERFLOW) SETC   '40'
&(DFHRESP_QIDERR) SETC   '44'
&(DFHRESP_ENQBUSY) SETC   '55'
&(DFHRESP_ENVDEFERR) SETC   '56'
&(DFHRESP_NOTALLOC) SETC   '61'   RPI 1079
&(DFHRESP_SUPPRESSED) SETC   '72'
&(DFHRESP_END) SETC   '83'
&(DFHRESP_DISABLED) SETC   '84'
.*********************************************************************
.* Copyright 2008 Automated Software Tools Corporation               *
.* This source code is part of z390 zCOBOL/assembler/emulator package*
.* The z390 package is distributed under GNU general public license  *
.* Author - Don Higgins                                              *
.* Date   - 04/07/08                                                 *
.*********************************************************************
.* 06/14/09 RPI 1057 sync with AZ390 DFHVALUE support
.*********************************************************************
               GBLC &(DFHVALUE_NOTAPPLIC)
               GBLC &(DFHVALUE_VSAM)
               GBLC &(DFHVALUE_ESDS)
               GBLC &(DFHVALUE_KSDS)
               GBLC &(DFHVALUE_RRDS)
               GBLC &(DFHVALUE_BASE)
               GBLC &(DFHVALUE_PATH)
               GBLC &(DFHVALUE_FIXED)
               GBLC &(DFHVALUE_VARIABLE)
               GBLC &(DFHVALUE_OPEN)
               GBLC &(DFHVALUE_CLOSED)
               GBLC &(DFHVALUE_ENABLED)
               GBLC &(DFHVALUE_DISABLED)
               GBLC &(DFHVALUE_UNENABLED)
               GBLC &(DFHVALUE_READABLE)
               GBLC &(DFHVALUE_NOTREADABLE)
               GBLC &(DFHVALUE_UPDATABLE)
               GBLC &(DFHVALUE_NOTUPDATABLE)
               GBLC &(DFHVALUE_BROWSABLE)
               GBLC &(DFHVALUE_NOTBROWSABLE)
               GBLC &(DFHVALUE_ADDABLE)
               GBLC &(DFHVALUE_NOTADDABLE)
               GBLC &(DFHVALUE_DELETABLE)
               GBLC &(DFHVALUE_NOTDELETABLE)
               GBLC &(DFHVALUE_VRRDS)
&(DFHVALUE_NOTAPPLIC) SETC   '1'
&(DFHVALUE_VSAM) SETC   '2'
&(DFHVALUE_ESDS) SETC   '5'
&(DFHVALUE_KSDS) SETC   '6'
&(DFHVALUE_RRDS) SETC   '7'
&(DFHVALUE_BASE) SETC   '10'
&(DFHVALUE_PATH) SETC   '11'
&(DFHVALUE_FIXED) SETC   '12'
&(DFHVALUE_VARIABLE) SETC   '13'
&(DFHVALUE_OPEN) SETC   '18'
&(DFHVALUE_CLOSED) SETC   '19'
&(DFHVALUE_ENABLED) SETC   '23'
&(DFHVALUE_DISABLED) SETC   '24'
&(DFHVALUE_UNENABLED) SETC   '33'
&(DFHVALUE_READABLE) SETC   '35'
&(DFHVALUE_NOTREADABLE) SETC   '36'
&(DFHVALUE_UPDATABLE) SETC   '37'
&(DFHVALUE_NOTUPDATABLE) SETC   '38'
&(DFHVALUE_BROWSABLE) SETC   '39'
&(DFHVALUE_CNOTBROWSABLE) SETC   '40'
&(DFHVALUE_ADDABLE) SETC   '41'
&(DFHVALUE_NOTADDABLE) SETC   '42'
&(DFHVALUE_DELETABLE) SETC   '43'
&(DFHVALUE_NOTDELETABLE) SETC   '44'
&(DFHVALUE_VRRDS) SETC   '732'
.*********************************************************************
.* Copyright 2009 Automated Software Tools Corporation               *
.* This source code is part of z390 zCOBOL/assembler/emulator package*
.* The z390 package is distributed under GNU general public license  *
.* Author - Don Higgins                                              *
.* Date   - 08/15/09                                                 *
.*********************************************************************
.* 08/15/09 RPI 106 global keywords to identifed as not vars or lits
.*********************************************************************
         GBLB &ZCKW_EQUAL
         GBLB &ZCKW_GREATER
         GBLB &ZCKW_LESS
         GBLB &ZCKW_THAN
         GBLB &ZCKW_TO
&ZCKW_EQUAL   SETB 1
&ZCKW_GREATER SETB 1
&ZCKW_LESS    SETB 1
&ZCKW_THAN    SETB 1
&ZCKW_TO      SETB 1
.*********************************************************************
.* Copyright 2008 Automated Software Tools Corporation               *
.* This source code is part of z390 zCOBOL/assembler/emulator package*
.* The z390 package is distributed under GNU general public license  *
.* Author - Don Higgins                                              *
.* Date   - 04/07/08                                                 *
.*********************************************************************
.* 04/08/08 initial coding to define global WS variables
.* 04/30/08 add 88 definitions for use in literal compares/moves etc.
.* 06/10/08 add LINKAGE SECTION support
.* 07/26/08 add zcob_sym_find hash table
.* 11/26/08 add IE_TYPE 4 FOR PERFORM n TIMES stmt, UNTIL stmt
.* 12/21/08 add SYM_FILLER_VAL_CNT for use by GEN_INIT_WS
.* 12/22/08 add SYM_SUB_CNT for use by ZCGETFLD.CPY subscript parser
.* 02/05/09 add TOT_SIX and SIX_NAME,SIX_OFF,SIX_OCCURS for subscripts
.* 02/08/09 add FIELD_RMD to indicate (off:len) reference modification
.* 02/23/09 RPI 1001 add &FIELD_88_IX set by ZCGETFLD.CPY
.* 02/25/09 RPI 1004 add &ZC_FLOAT SET TO DEFUALT DFP
.* 02/26/09 RPI 1012 add &SYM_VAL_ALL FLAG
.* 03/10/09 RPI 1013 add &ZC_FLOAT_SHORT/LONG/EXTENDED_TYPE
.* 04/24/09 RPI 1003 add &TMP_NAME/TYPE/LEN/SIGN for ZC_CALC,GEN_???
.* 07/09/09 RPI 1065 move &FILE_IX from SYM_REF to SYM_FD
.* 07/17/09 RPI 1065 add &QNAME(50) &QNAME_LEN &QNAME_TOT
.*          for ZC_GET_QNAME used by ZCGETFLD and ZC_SYM_INDEX
.* 08/03/09 RPI 1065 add &ZC_DEC_IS_COMMA special name support
.* 08/04/09 RPI 1065 support qualified paragraph names SN_NAME/PG_NAME
.* 08/09/09 RPI 1065 add ZC_ERROR to skip GEN_BC after GEN_COMP error
.* 08/09/09 RPI 1065 use FXT for head of dup sym, NXT for next dup.
.* 08/18/09 RPI 1065 add SYM_OPT for options R=JUST,
.* 09/08/09 RPI 1065 replace SYM_DEP_IX with SYM_DEPEND name
.* 09/09/09 RPI 1065 replace FILE_TYPE with FILE_ORG,FILE_MODE,FILE_KEY
.*********************************************************************
.*
.* PGM NAME AND ZCOBOL VERSION CHECKED BY ZC390NUC IN ZC390LIB.390
.*
         GBLC  &ZC_PGM       NAME FROM FIRST CMD PARM (IGNORE PGM-ID)
         GBLC  &ZC_VER       CURRENT Z390 VERSION FROM TZ390.VER
         GBLC  &ZC_CDATE     COMPILE DATE OR MM/DD/YY IF NOTIMING
         GBLC  &ZC_CTIME     COMPILE START TIME IF TIMING ELSE ''
         GBLC  &ZC_LIST      LISTING OPTION FOR INTERNAL CODE GEN
         GBLC  &ZC_OPTIONS   ALL OPTIONS FOLLOWING PGM NAME
.*
.* ZCOBOL OPTIONS
.*
         GBLB  &ZC_PROC_DIV  SET AT START OF PROCEDURE DIVISION
         GBLB  &ZC_CICS      CICS COBOL PROGRAM WITH DFHEISTG WS CSECT
         GBLB  &ZC_CICS_DSA_VALUE IGNORE VALUES AND ISSUE 1 WARNING
         GBLB  &ZC_COMMENT   LIST SOURCE AS COMMENTS IN MLC FOR PRN
         GBLC  &ZC_LIST      LIST OPTION FOR ZCOBOL INTERNAL CODE GEN
         GBLB  &ZC_EXTEND    COMP S9(39) TYPE G, COMP-3 S9(31) TYPE P
         GBLB  &ZC_R64       USE 64 BIT REGS FOR G TYPE S9(18) DSG ETC.
         GBLB  &ZC_TRACE     DISPLAY PARAGRAPH LABEL TRACE DURING EXEC
         GBLB  &ZC_TRUNC     TRUNCATE NUM DATA TO PIC SIZE (NOT YET)
         GBLB  &ZC_WARN      GENERATE WARNING LEVEL 4 M NOTES
         GBLC  &ZC_FLOAT     FLOATING POINT TYPE (DECIMAL,HEX,BINARY)
         GBLC  &ZC_FLOAT_SHORT_TYPE    1/4/7 SET FROM FLOAT(???)
         GBLC  &ZC_FLOAT_LONG_TYPE     2/5/8 SET FROM FLOAT(???)
         GBLC  &ZC_FLOAT_EXTENDED_TYPE 3/7/9 SET FROM FLOAT(???)
         GBLB  &ZC_DEC_IS_COMMA        SPEC NAMES OPTION
         GBLB  &ZC_ERROR               FLAG TO INDICATE STMT ERROR
.*
.* GLOBAL LIMITS
.*
&MAX_PM     SETA 20    MAXIMUM NESTED PERFORMS (CAN BE SET HIGHER)
&MAX_IE     SETA 100   MAXIMUM NEXTED IF, EVALUATE, READ END-READ
&MAX_WS_LVL SETA 50 MAXIMUM WS LEVELS
&MAX_SYM    SETA 20000 MAXIMUM WS SYMBOLS
&MAX_FILE   SETA  100
.*
.* FILE TABLE DEFINED FROM SELECT AND FD STATEMENTS
.*
         GBLA  &FILE_TOT               TOTAL FILES DEFINED
         GBLA  &FILE_IX                FILE INDEX ELSE 0 FOR NOT FOUND
         GBLC  &FILE_NAME(&MAX_FILE)   FILE NAME FROM SELECT
         GBLC  &FILE_ORG(&MAX_FILE)    FILE TYPE (QSAM,LSEQ,KSDS,RRDS)
         GBLC  &FILE_MODE(&MAX_FILE)   FILE MODE (SEQ,RAN,DYN)
         GBLC  &FILE_KEY(&MAX_FILE)    FILE INDEXED RECORD KEY FIELD
         GBLC  &FILE_XRBA(&MAX_FILE)   FILE RELATIVE XRBA FIELD
         GBLC  &FILE_DDNAME(&MAX_FILE) FILE DDNAME OR DSN IN QUOTES
         GBLC  &FILE_RECORD(&MAX_FILE) FILE RECORD FOR READ
         GBLC  &FILE_STATUS(&MAX_FILE) FILE STATUS FIELD NAME
         GBLC  &FILE_BLKSI1(&MAX_FILE) MIN BLOCK SIZE (DEF CHAR)
         GBLC  &FILE_BLKSI2(&MAX_FILE) MAX BLOCK SIZE (DEF CHAR)
         GBLB  &FILE_BLKSI_RCDS(&MAX_FILE) MIN/MAX BLKSI IN RECORDS
         GBLC  &FILE_LRECL1(&MAX_FILE) MIN REC SIZE (DEF CHAR)
         GBLC  &FILE_LRECL2(&MAX_FILE) MAX REC SIZE (DEF CHAR)
         GBLC  &FILE_DEPENDING(&MAX_FILE) VAR LRECL FLD FOR FILE
.*
.* WS BASE REGISTERS (GEN_ID sys init, GEN_BASE dynamic,PERIOD reset)
.*
.*   1.  Cnrrent preset WS base regs  = R7
.*   2.  Currnet dynamic WS base regs = R6 and R5
.*
         GBLC &BASE_WS
&BASE_SYS SETC 'ZC_R13'           FIRST WS SYSTEM BASE
         GBLA &BASE_WS_HIGH
&BASE_WS_HIGH SETA 4096-16        MAX WS SYSTEM BASE OFFSET RPI 1065
         GBLC &BASE_B1,&BASE_B2
         GBLB &BASE_B1_USED,&BASE_B2_USED  FOR DROP AT PERIOD
&BASE_B1 SETC 'ZC_R6'                FIRST  DYNAMIC WS BASE REG
&BASE_B2 SETC 'ZC_R7'                SECOND DYNAMIC WS BASE REG
         GBLA &BASE_B1_LOW,&BASE_B1_HIGH
         GBLA &BASE_B2_LOW,&BASE_B2_HIGH
         GBLA &BASE_LRU           LEAST RECENTLY USED WS DYNAMIC BASE
.*
.* LK BASE REGISTERS (GEN_BASE for dynamic,PERIOD for reset)
.*
.*   1.  Currnet dynamic WS base regs = R4 and R5
.*
         GBLC &LBASE_B1,&LBASE_B2
         GBLB &LBASE_B1_USED,&LBASE_B2_USED FOR DROP AT PERIOD
&LBASE_B1 SETC 'ZC_R4'               FIRST  DYNAMIC LK BASE REG
&LBASE_B2 SETC 'ZC_R5'               SECOND DYNAMIC LK BASE REG
         GBLA &LBASE_B1_LOW,&LBASE_B1_HIGH LOW HIGH BASE OFFSETS
         GBLA &LBASE_B2_LOW,&LBASE_B2_HIGH LOW HIGH BASE OFFSETS
         GBLA &LBASE_B1_IX        SYM IX OF BASE VAR IN WS
         GBLA &LBASE_B2_IX        SYM IX OF BASE VAR IN WS
         GBLA &LBASE_LRU          LEAST RECENTLY USED LK DYNAMIC BASE
.*
.* GET PARM FIELD (ZCGETFLD.CPY) TO GET QUAL. SUBSC. FIELD NAME/ADDR
.*
         GBLA  &PARM_IX                INPUT/UPDATE PARM INDEX
         GBLC  &FIELD_REG              INPUT REG FOR SUBSC/RMOD FLD ADR
         GBLB  &FIELD_SS1              INPUT SS1 FIELD OFF(LEN,REG)
         GBLB  &FIELD_SIX              OUTPUT REG USED FOR SUB/IDX/RMOD
         GBLB  &FIELD_RMD              OUTPUT REF MODS (OFF:LEN)
         GBLA  &FIELD_IX               OUTPUT SYM_IX OF FIELD
         GBLA  &FIELD_88_IX            OUTPUT SYM_IX OF 88 FIELD
         GBLC  &FIELD_NAME             OUTPUT SOURCE/TARGET NAME
.*                                       SS1 AND     SIX > O(L,R)
.*                                       SS1 AND NOT SIX > N+O(L)
.*                                       NOT SS1 AND SIX > O(R)
.*                                       NOT SS1 NOT SIX > N+O
         GBLA  &FIELD_OFF              OUTPUT OFF IF RMOD ELSE 0
         GBLA  &FIELD_LEN              OUTPUT LEN IF RMOD ELSE 0
.*
.*
.* SYMBOL TABLE
.*
         GBLA  &SYM_TOT                TOT FD AND WS SYMBOLS
         GBLA  &SYM_IX                 SYM INDEX IF FOUND ELSE 0
         GBLC  &SYM_RX                 SYM BDDD SET BY GEN_SYM_RX
         GBLA  &SYM_IX_LST             SYM LAST WS TYPE 1 SYM INDEX
         GBLA  &SYM_DUP_TOT            SYM TOTAL DUPLICATES
         GBLA  &SYM_DUP_MAX            SYM MAX DUPLICATE CHAIN LENGTH
         GBLA  &SYM_CUR_DUP            SYM CURRENT DUP CHAIN COUNT
         GBLA  &SYM_FILLER_VAL_CNT     FILLER WITH VALUE CNT IF CICS
         GBLA  &PIC_DIGITS             DIGITS IN PIC BY ZC_PIC_DIGITS
         GBLC  &SYM_NAME(&MAX_SYM)     SYM NAME
         GBLA  &SYM_FXT(&MAX_SYM)      SYM NAME FIRST DUPLICATE SYM_IX
         GBLA  &SYM_NXT(&MAX_SYM)      SYM NAME DUPLICATE NEXT SYM_IX
         GBLA  &SYM_LK(&MAX_SYM)       LINKAGE SECTION SYM_IX OF 01 LVL
         GBLA  &SYM_QIX(&MAX_SYM)  SYM QUALIFIER INDEX TO NEXT LVL NAME
         GBLA  &SYM_SIT(&MAX_SYM)  SYM SUBSCRIPT/INDEX TOTAL REQ'D
         GBLA  &SYM_LVL(&MAX_SYM)  SYM LEVEL
         GBLA  &SYM_REF(&MAX_SYM)  SYM REF INDEX (88 SYM,RDEF,OR VAR)
         GBLA  &SYM_FD(&MAX_SYM)   SYM FD INDEX OR ZERO
         GBLA  &SYM_LOC(&MAX_SYM)  SYM WS OFFSET OR 88 VAL INDEX
         GBLA  &SYM_LEN(&MAX_SYM)  SYM LENGTH
         GBLA  &SYM_DUP(&MAX_SYM)  SYM OCCURS N TIMES (MAX FOR DEPEND)
         GBLA  &SYM_DUP_MIN(&MAX_SYM)  SYM MIN OCCURS DEPENDING
         GBLC  &SYM_DEPEND(&MAX_SYM)   SYM NAME FOR DEPENDING ON
         GBLC  &SYM_VAL(&MAX_SYM)  SYM VALUE (0,SPACE,'...') T=1,2
         GBLB  &SYM_VAL_ALL(&MAX_SYM)  SYM VALUE ALL FLAG
         GBLC  &SYM_PIC(&MAX_SYM)  SYM PIC (999,X(3) ETC.)
         GBLC  &SYM_PIC_TYPE(&MAX_SYM)  SYM PIC TYPE A,D,E,F,H,P,X,Z
         GBLB  &SYM_PIC_SIGN(&MAX_SYM)  SYM PIC (S9...)
         GBLA  &SYM_PIC_DEC(&MAX_SYM)   SYM DECIMAL PLACES (999V99)
         GBLC  &SYM_OPT(&MAX_SYM)       SYM OPTIONS
.*               B = BLANK WHEN ZERO
.*               E = EXTERNAL
.*               G = GLOBAL
.*               L = LEADING
.*               R = JUST/JUSTIFY RIGHT
.*               S = SYNC/SYNCHRONIZE (ALIGN TO OPTIMUM BOUNDARY)
.*
.* SECTION AND PARAGRAPH NAMES
.*
         GBLA &SN_TOT              TOTAL SECTIONS (SEE LABEL MACRO)
         GBLA &PG_TOT              TOTAL PARAGRAPH NAMES
         GBLA &PG_DUP_TOT          TOTAL DUPLICATE PARAGRAH NAMES
.*
.* QNAME - QUALIFIED NAME LEVELS
.*
         GBLC &QNAME(50)           QUALIFEID BASE AND LEVEL NAMES
         GBLA &QNAME_LEN           TOT LEN OF QUAL NAME A_OF_B_OF_C ETC
         GBLA &QNAME_TOT           TOT NUMBER OF LEVELS INC 1=BASE NAME
.*
.* SYM 88 VALUES - SYM_LOC POINTS TO STARTING VALUE IF MORE THAN 1
.*
         GBLA &V88_TOT            TOTAL 88 LEVEL VALUES
         GBLC &V88_VAL(&MAX_SYM)  88 VALUE
         GBLA &V88_NEXT(&MAX_SYM) INDEX OF NEXT VALUE OR 0
         GBLC &V88(&MAX_SYM)
.*
.* SET POINTERS FOR ALL SYMBOLS WITH OCCURS
.*
         GBLA &CUR_SET_LVL    CURRENT SUBSCRIPT LEVEL RESET TO 1 AT 01
         GBLA &SET_TOT  TOTAL OCCURS AND SET POINTERS TO GENERATED
         GBLA &SET_SYM_IX(&MAX_SYM) INDEX TO SYM WITH OCCURS
         GBLA &SET_SYM_LVL(&MAX_SYM) SUBSCRIPT LEVEL 1-N
         GBLC &SET_INDEX(&MAX_SYM)  INDEX BY ALIAS NAME IF ANY
.*
.* LK LINKAGE SECTION - SEE &SYM_LK AND SYM_REF FOR DSECT ALSO
.*
         GBLB &LK_MODE            SET LK MODE ON AT LINKAGE SECTION
         GBLA &LK_SAVE_WS_LOC     SAVE LAST WS_LOC RESTORED AT DATA END
         GBLA &LAST_LK_DSECT_IX   SAVE IX OF LAST 01 DSECT IN LK SECT
         GBLA &LK_TOT             TOTAL LK 01 LEVEL DSECTS
.*
.* IF/ELSE, EVALUATE, READ, AND PERFORM - NESTED COMPARE/BRANCH DATA
.*
         GBLA &IF_CNT               UNIQUE IF COUNTER
         GBLA &MAX_IOP_TF           MAX IF_IOP_TF FIELDS REQUIRED
         GBLB &PERIOD_NEXT          REQ LABEL FOR NEXT SENTENCE AT .
         GBLA &PERIOD_NEXT_LAB      UNIQUE NEXT SENENCE LABEL COUNTER
         GBLA &EVAL_LAB             UNIQUE EVALUTE LABEL COUNTER
         GBLA &AT_END_LAB           UNIQUE READ AT END LABEL COUNTER
         GBLA &PM_LAB               UNIQUE PERFORM LABEL COUNTER
         GBLA &ADD_LAB               UNIQUE ADD      LABEL COUNTER
         GBLA &SUB_LAB               UNIQUE SUBTRACT LABEL COUNTER
         GBLA &MPY_LAB               UNIQUE MULTIPLY LABEL COUNTER
         GBLA &DIV_LAB               UNIQUE DIVIDE   LABEL COUNTER
         GBLC &LAST_PG_NAME         LAST PROC DIV PARAGRAPH NAME
         GBLC &LAST_SN_NAME         LAST PROC DIV SECTION NAME
         GBLA &IE_LVL               CURRENT LEVEL FOR IF OR EVALUATE
         GBLA &IE_TYPE(&MAX_IE)     1=IF, 2=EVALUATE, 3=END_READ 4=PM
.*                                  11=ADD,12=SUB,13=MPY,14=DIV
         GBLA &IE_TCNT(&MAX_IE)     IE TYPE  COUNT
         GBLA &IE_BCNT(&MAX_IE)     IE BLOCK COUNT WITHIN TYPE
         GBLB &IE_ELSE(&MAX_IE)     CURRENT IF OR ELSE BLOCK FLAG
         GBLA &IE_EVAL(&MAX_IE)     CURRENT EVAL LABEL #
         GBLA &IE_EVAL_IX(&MAX_IE)  CURRENT EVAL SYM INDEX
         GBLA &IE_WHEN(&MAX_IE)     CURRENT EVAL WHEN LABEL #
         GBLC &IE_PM_LAB(&MAX_IE)   CURRENT PM STMT LOOP LABEL
.*
.* GO TO DEPENDING ON LABEL
.*
         GBLA &ZC_GO_DEP            COUNT OF GO TO DEPENDING ON  TABLES
.*
.* CURRENT WS LOC, LEVEL, AND PENDING LENGTH SYMBOL PTRS
.*
         GBLA  &WS_LOC            CURRENT WS LOC OFFSET (SEE GEN_ID)
         GBLA  &ZC_DCB_LEN        LENGTH OF DCB GEN'D BY GEN_FD IN WS
&ZC_DCB_LEN SETA X'68'
         GBLA  &WS_LVL            CURRENT WS INDENT LEVEL
         GBLA  &WS_LVL_NO(&MAX_WS_LVL)       CUR LV # 01-49 (77=01)
         GBLA  &WS_LVL_SYM_IX(&MAX_WS_LVL)   CUR WS ITEM SYMBOL INDEX
         GBLC  &WS_LVL_USE(&MAX_WS_LVL)      CUR WS LEVEL USAGE
         GBLB  &WS_LVL_RDEF(&MAX_WS_LVL)     CUR WS ITEM IS  REDEFINE
         GBLA  &WS_LVL_RDEF_END(&MAX_WS_LVL) CUR WS ITEM RDEF END LOC
         GBLA  &WS_LVL_RDEF_IX(&MAX_WS_LVL)  CUR WS ITEM RDEF SYM IX
.*
.* VAR - GENERATED WS 77 ITEMS FOR USE BY GENERATED CODE
.*
         GBLA  &VAR_TOT     TOTAL VARS NAMED ZC_VAR_&VAR_TOT
.*
.* SIX SUBSCRIPT DATA PASSED BETWEEN ZCGETFLD AND GEN_SIX
.*
         GBLA &TOT_SIX       TOTAL SUBSCRIPTS
         GBLC &SIX_NAME(10)  SUBSCRIPT/INDEX/LIT FROM FIELD(PARMS)
         GBLC &SIX_OFF(10)   SUBSCRIPT OFFSETS +/-NUM ELSE ''
         GBLA &SIX_OCCUR(10) OCCURS SYM_IX (TO GET LENGTH, ETC.)
.*
.* TMP variables for ZC_CALC, GEN_ADD, GEN_SUB, GEN_MPY, GEN_DIV
.* wheb target index is negative (used to store and fetch temp values)
.*
         GBLC &TMP_NAME(100) - NAME -  ZCVT_WORKAREA+16*(IX-1)
         GBLC &TMP_TYPE(100) - TYPE OF TEMP VALUE '1-9FGHPQ'
         GBLA &TMP_LEN(100)  - LENGTH OF TEMP VALUE FOR P
         GBLB &TMP_SIGN(100) - SIGNED OR UNSIGNED P OR Z FIELD
.*
.* END OF CBL_WS.CPY
.*

