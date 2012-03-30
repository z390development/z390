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
.* 02/16/12 RPI 1181 IGNORE (1) SUBSCRIPT IF OCCURS 1
.* 03/06/12 RPI 1182 SUPPORT RMD (SUB+LIT:LIT) FORMAT
.* 03/23/12 RPI 1182 ONLY REQ 1 GEN_BASE PER FIELD (NONE FOR SUB,OFF)
.* 03/30/12 RPI 1182 SET &FIELD_LEN DEF IF FIELD NAME FOUND
.*********************************************************************
.* GET RX ADDRESSABLE QUALIFIED PARM FIELD FROM &SYSLIST(&PARM_IX)
.* INPUT
.*  1. &PARM_IX   SETA MUST BE SET TO CURRENT SYSLIST PARM
.*  2. &FIELD_REG SETC MUST BE SET TO TARGET REG FOR SUBSC/RMOD ADDR
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
.*  1. ALL VERBS THAT SUPPORT SUBSCRIPTED AND MODIFIED FIELD REFERENCES
.*     SUCH AS MOVE.MAC WITH BOTH SUBSCRIPTED AND MODIFIED FIELDS.
.* USES:
.*  1. ZC_SYM_FIND  - find field &FIELD_NAME and set &FIELD_IX
.*  2. ZC_SYM_INDEX - resolve duplicate names if qualified
.*  3. ZC_GET_QNAME - build qualified name with turncation if > 63
.* NOTES
.*  1. May generate one new base reg per field if required. RPI 1182
.*  2. May generate subscript or RMOD code if required 
.*  3. Errors - not found, no base, missing subsc, (var+lit:lit).
.*********************************************************************
         AENTRY GET_PARM_FIELD
         :&QNAME(1) SETC '&SYSLIST(&PARM_IX)'
         :&QNAME_LEN SETA K'&QNAME(1)    RPI 1065 CALC QUAL LENGTH
         :&QNAME_TOT SETA 1
         :&FIELD_NAME SETC '&QNAME(1)'
         :&FIELD_IX  SETA 0           RPI 1065 ASSUME NOT FOUND
         :&FIELD_SIX SETB 0           ASSUME NO SUBSCRIPT/INDEX
         :&FIELD_RMD SETB 0           ASSUME NO REF. MOD. (OFF:LEN)
         :&RMD_VLEN  SETB 0           ASSUME NO RMD VARIABLE LENGTH
         :&PARM_IX SETA &PARM_IX+1
         AIF ('&FIELD_NAME' EQ '-' OR '&FIELD_NAME' EQ '+') RPI 1065
             :&FIELD_NAME SETC '&FIELD_NAME&SYSLIST(&PARM_IX)'
             :&PARM_IX SETA &PARM_IX+1
             AEXIT AENTRY
         AEND
         AWHILE ('&SYSLIST(&PARM_IX)' EQ 'OF'                          X
               OR '&SYSLIST(&PARM_IX)' EQ 'IN')
               AIF (&QNAME_TOT GT 50)
                   MNOTE 8,'ZCGENFLD MAX QUAL LEVELS EXCEEDED'
                   MEXIT
               AEND
               :&QNAME_TOT SETA &QNAME_TOT+1
               :&QNAME(&QNAME_TOT) SETC '&SYSLIST(&PARM_IX+1)'
               :&QNAME_LEN SETA &QNAME_LEN+4+K'&QNAME(&QNAME_TOT)
               :&PARM_IX SETA &PARM_IX+2
         AEND
         ZC_SYM_FIND &FIELD_NAME
         :&FIELD_IX SETA &SYM_IX
         AIF   (&QNAME_TOT EQ 1)
               AIF   (&FIELD_IX EQ 0)  FIELD NOT FOUND
                     AEXIT AENTRY
               AEND
         AELSE
               ACALL FIND_QUAL_ALIAS  FIND ALIAS OF QUAL FIELD
         AEND
         AIF   (&FIELD_IX GT 0)
               AIF (&SYM_LVL(&FIELD_IX) EQ 88)
                   :&FIELD_88_IX SETA &FIELD_IX
                   :&FIELD_IX   SETA &SYM_REF(&FIELD_88_IX)
               AELSE
                   :&FIELD_88_IX SETA 0
               AEND
               :&FIELD_NAME SETC '&SYM_NAME(&FIELD_IX)' 
               :&FIELD_LEN  SETA &SYM_LEN(&FIELD_IX) RPI 1182 DEF
               AIF (NOT &ZC_PROC_DIV) RPI 1065 NO BASE/SIX IN WS
                   AEXIT AENTRY
               AEND
               GEN_BASE &FIELD_IX  GEN BASE REQ'D FOR RX ACCESS
               AIF   ('&SYSLIST(&PARM_IX)' EQ '''(''') SUBSC FOUND
                     AIF (&SYM_SIT(&FIELD_IX) GT 0)
                         ACALL GEN_SUBSCRIPT
                         AIF ('&SYSLIST(&PARM_IX)' EQ '''(''') RMOD FND
                             ACALL CALC_RMOD_OFF_LEN
                         AEND
                     AELSE
                         ACALL CALC_RMOD_OFF_LEN
                     AEND
               AELSEIF ('&SYM_SIT(&FIELD_IX)' GT 0)
                     MNOTE 8,'ZCGETFLD SUBSCRIPT MISSING FOR &FIELD_NAMX
               E'
                     MEXIT
               AEND
         AEND
         AEND
.*
.* FIND QUAL ALIAS CREATE ALIAS AND SET FIELD NAME RPI 1062
.*
         AENTRY FIND_QUAL_ALIAS
         :&CUR_SYM_IX SETA &SYM_FXT(&FIELD_IX)
         AIF (&CUR_SYM_IX EQ 0)   ARE THERE DUP SYMBOLS
             AEXIT AENTRY
         AEND
         AWHILE (&CUR_SYM_IX GT 0)  SEARCH DUP SYMBOLS FOR MATCH
               ACTR 4096
               :&QI SETA 2
               :&CUR_QIX SETA &SYM_QIX(&CUR_SYM_IX)
               :&QNAME_FOUND SETB 1
               AWHILE (&QNAME_FOUND AND &QI LE &QNAME_TOT)
                   ACALL FIND_QNAME
                   AIF (&QNAME_FOUND)
                       AIF (&QI EQ &QNAME_TOT)
                           :&FIELD_NAME SETC '&SYM_NAME(&CUR_SYM_IX)'
                           :&FIELD_IX   SETA &CUR_SYM_IX  RPI 1062
                           AEXIT AENTRY
                       AELSE
                           :&QI      SETA &QI+1  NEXT QNAME. NEXT LVL
                           :&CUR_QIX SETA &SYM_QIX(&CUR_QIX_TEST) 
                       AEND
                   AEND
               AEND 
               :&CUR_SYM_IX SETA &SYM_NXT(&CUR_SYM_IX)  NEXT DUP SYM 
         AEND
         MNOTE 8,'ZCGETFLD QUAL FIELD NOT FOUND - &FIELD_NAME'
         AEND
.*
.* FIND SYM LEVEL MATCHING QNAME(QI) STARTING AT SYM_NAME(CUR_QIX)
.* RETURN WITH QNAME_FOUND T/F
.*
         AENTRY FIND_QNAME
         :&CUR_QIX_TEST SETA &CUR_QIX
         AWHILE (&CUR_QIX_TEST GT 0) 
               AIF   ('&QNAME(&QI)' EQ '&SYM_NAME(&CUR_QIX_TEST)')
                     AEXIT AENTRY
               AELSEIF ('&QNAME(&QI)_OF_' EQ '&SYM_NAME(&CUR_QIX_TEST)'X
               (1,K'&QNAME(&QI)+4))  CHECK BASE OF QUALIFIED LEVEL 
                     AEXIT AENTRY
               AELSE
               AEND
               :&CUR_QIX_TEST SETA &SYM_QIX(&CUR_QIX_TEST)  NEXT LVL
         AEND
         :&QNAME_FOUND SETB 0  NOT FOUND, FORCE NEXT DUP SYM SEARCH
         AEND
.*
.* PARSE AND GENERATE CODE FOR FIELD WITH 1 OR MORE SUBSCRIPTS
.*
         AENTRY GEN_SUBSCRIPT
         :&TOT_SIX SETA &SYM_SIT(&FIELD_IX)
         :&PARM_IX SETA &PARM_IX+1    POS TO FIRST SUBSCRIPT/INDEX
         :&CUR_SYM_IX SETA &FIELD_IX  CUR SYM WITH OCCURS
         AIF   (&SYM_DUP(&CUR_SYM_IX) LE 1)
               ACALL GET_NEXT_OCCURS_SYM 
         AEND
         :&CUR_SIX SETA 0
         AWHILE (&PARM_IX LE N'&SYSLIST                                X
                 AND &CUR_SYM_IX GT 0                                  X
                 AND &CUR_SIX LE &TOT_SIX                              X
                 AND '&SYSLIST(&PARM_IX)' NE ''')''')
               :&CUR_SIX SETA &CUR_SIX+1
               :&SIX_NAME(&CUR_SIX) SETC '&SYSLIST(&PARM_IX)' NAME/LIT 
               :&SIX_OCCUR(&TOT_SIX-&CUR_SIX+1) SETA &CUR_SYM_IX 
               AIF ('&SYSLIST(&PARM_IX+1)' EQ '+'                      X
                    OR '&SYSLIST(&PARM_IX+1)' EQ '-')
                    :&SIX_OFF(&CUR_SIX) SETC '&SYSLIST(&PARM_IX+1)&SYSLX
               IST(&PARM_IX+2)'      
                    :&PARM_IX SETA &PARM_IX+2   
               AELSE
                    :&SIX_OFF(&CUR_SIX) SETC ''
               AEND
               :&PARM_IX SETA &PARM_IX+1
               ACALL GET_NEXT_OCCURS_SYM
         AEND
         AIF   (&CUR_SIX NE &TOT_SIX                                   X
               OR '&SYSLIST(&PARM_IX)' NE ''')''')
               MNOTE 8,'ZCGETFLD SUBSCRIPT MISMATCH'
               MEXIT
         AEND
         :&PARM_IX SETA &PARM_IX+1
.*
.* GENERATE SUBSCRIPT CODE
.*
         GEN_SIX
         AEND
.*
.* GET NEXT OCCURS SYM
.*
       AENTRY GET_NEXT_OCCURS_SYM
       :&CUR_SYM_IX SETA &SYM_QIX(&CUR_SYM_IX)
       AWHILE (&CUR_SYM_IX GT 0)
          AIF (&SYM_DUP(&CUR_SYM_IX) GT 1)
              AEXIT AENTRY  EXIT WITH NEXT OCCURS CUR_SYM_IX
          AEND
          :&CUR_SYM_IX SETA &SYM_QIX(&CUR_SYM_IX)
       AEND
       AEND
.*
.* CALC RMOD OFF SET - CALC OFFSET AND LENGTH FROM (OFF:LEN)
.*
         AENTRY CALC_RMOD_OFF_LEN
         AIF ('&SYSLIST(&PARM_IX+1)' EQ '1'                            X
               AND '&SYSLIST(&PARM_IX+2)' EQ ''')''') RPI 1181
               :&PARM_IX SETA &PARM_IX+3 
               AEXIT AENTRY
         AEND
         :&RMD_OFF     SETC '&SYSLIST(&PARM_IX+1)'
         :&RMD_OFF_ADJ SETC '0'
         AIF ('&SYSLIST(&PARM_IX+2)' EQ '+')
             :&RMD_OFF_ADJ SETC '&SYSLIST(&PARM_IX+3)'
             :&PARM_IX SETA &PARM_IX+4
         AELSEIF ('&SYSLIST(&PARM_IX+2)' EQ '-')
             :&RMD_OFF_ADJ SETC '-&SYSLIST(&PARM_IX+3)'
             :&PARM_IX SETA &PARM_IX+4
         AELSE
             :&PARM_IX SETA &PARM_IX+2
         AEND
         AIF ('&SYSLIST(&PARM_IX)' NE ':')
             MNOTE 8,'ZCGETFLD RMD INVALID FORMAT (OFF:LEN)'
             MEXIT
         AEND
         :&RMD_LEN     SETC '&SYSLIST(&PARM_IX+1)' 
         AIF ('&SYSLIST(&PARM_IX+2)' NE ''')''')
             MNOTE 8,'ZCGETFLD RMD INVALID FORMAT (OFF:LEN)'
             MEXIT
         AEND
         :&PARM_IX SETA &PARM_IX+3
         GEN_RMD &RMD_OFF,&RMD_OFF_ADJ,&RMD_LEN          
         AEND
.*
.* END OF ZCGETFLD.CPY
.*
