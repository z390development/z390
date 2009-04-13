.*********************************************************************
.* Copyright 2008 Automated Software Tools Corporation               *
.* This source code is part of z390 zCOBOL/assembler/emulator package*
.* The z390 package is distributed under GNU general public license  *
.* Author - Don Higgins                                              *
.* Date   - 04/07/08                                                 *
.*********************************************************************
.* 02/23/08 RPI 1001 ADD SUPPORT FOR SIX ON CONDITION 88 LEVEL FIELD
.* 03/17/08 RPI 1004 COMBINE - WITH LITERAL VALUE                   
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
.* NOTES
.*  1. May generate base setting code if required
.*  2. May generate subscript code if required 
.*  3. Errors - not found, no base, missing subsc., invalid subsc.
.*********************************************************************
         AENTRY GET_PARM_FIELD
         LCLC &QNAME(10)     QUALIFIER NAMES FOR A FIELD
         :&TQI SETA 1
         :&QNAME(1) SETC '&SYSLIST(&PARM_IX)'
         :&FIELD_NAME SETC '&QNAME(1)'
         :&FIELD_SIX SETB 0           ASSUME NO SUBSCRIPT/INDEX
         :&FIELD_RMD SETB 0           ASSUME NO REF. MOD. (OFF:LEN)
         :&PARM_IX SETA &PARM_IX+1
         AIF ('&FIELD_NAME'(1,1) EQ '-' OR '&FIELD_NAME'(1,1) EQ '+')
             :&FIELD_NAME SETC '&FIELD_NAME&SYSLIST(&PARM_IX)'
             :&PARM_IX SETA &PARM_IX+1
             AEXIT AENTRY
         AEND
         AWHILE ('&SYSLIST(&PARM_IX)' EQ 'OF'                          X
               OR '&SYSLIST(&PARM_IX)' EQ 'IN')
               :&TQI SETA &TQI+1
               :&QNAME(&TQI) SETC '&SYSLIST(&PARM_IX+1)'
               :&FIELD_NAME SETC '&FIELD_NAME._OF_&QNAME(&TQI)'
               AIF (K'&FIELD_NAME GT 63)
                   MNOTE 8,'ZCGETFLD QUAL NAME TOO LONG &FIELD_NAME'
                   MEXIT
               AEND 
               :&PARM_IX SETA &PARM_IX+2
         AEND
         ZC_SYM_FIND &FIELD_NAME
         :&FIELD_IX SETA &SYM_IX
         AIF   (&FIELD_IX EQ 0)  FIELD NOT FOUND
               AIF (&TQI EQ 1)   SINGLE FIELD MAY BE SPEC. LIT
                   AEXIT AENTRY
               AEND
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
               GEN_BASE &FIELD_IX  GEN BASE IF REQ'D FOR RX ACCESS
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
.* FIND QUAL ALIAS CREATE ALIAS AND UPDATE DEFAULT QUAL NAME
.*
         AENTRY FIND_QUAL_ALIAS
         ZC_SYM_FIND &QNAME(1)  FIND BASE NAME WITH DUP CHAIN
         AIF   (&SYM_IX EQ 0)    
               MNOTE 8,'ZCGETFLD FIELD NOT FOUND - &FIELD_NAME'
               MEXIT
         AELSEIF (&SYM_NXT(&SYM_IX) EQ 0) USE BASE WITH NO DUPS
               :&FIELD_NAME SETC '&SYM_NAME(&SYM_IX)'
               :&FIELD_IX   SETA &SYM_IX
               AEXIT AENTRY
         AEND
         :&CUR_SYM_IX SETA &SYM_IX
         AWHILE (&CUR_SYM_IX GT 0)  SEARCH DUP SYMBOLS FOR MATCH
               :&QI SETA 2
               :&CUR_QIX SETA &SYM_QIX(&CUR_SYM_IX)
               AWHILE (&QI LE &TQI)       IS THERE ANOTHER SPEC QUAL
                   AWHILE (&CUR_QIX GT 0)    IS THERE ANOTHER LEVEL 
                      AIF ('&QNAME(&QI)' EQ '&SYM_NAME(&CUR_QIX)')
                          AIF (&QI EQ &TQI) ALL SPEC'D QUAL FOUND
                              GBLA &(ZC_IX_&FIELD_NAME)  CREATE ALIAS
                              :&(ZC_IX_&FIELD_NAME) SETA &CUR_SYM_IX   
                              :&FIELD_IX SETA &CUR_SYM_IX               
                              :&SYM_NAME(&CUR_SYM_IX) SETC '&FIELD_NAMEX
               ' 
                              AEXIT AENTRY
                          AEND
                          :&QI SETA &QI+1                   NEXT QUAL
                      AEND
                      :&CUR_QIX SETA &SYM_QIX(&CUR_QIX)  NEXT LVL
                   AEND
                   AEXIT AWHILE  OUT OF LEVELS, EXIT TO NEXT DUP SYM
               AEND 
               :&CUR_SYM_IX SETA &SYM_NXT(&CUR_SYM_IX)  NEXT DUP SYM         
         AEND
         MNOTE 8,'ZCGETFLD QUAL FIELD NOT FOUND - &FIELD_NAME'
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
               OR '&SYSLIST(&PARM_IX) NE ''')''')
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
         AIF   ('&SYSLIST(&PARM_IX+2)' EQ ':'                          X
               AND '&SYSLIST(&PARM_IX+4)' EQ ''')''')
               :&FIELD_OFF SETA &SYSLIST(&PARM_IX+1)-1
               :&FIELD_LEN SETA &SYSLIST(&PARM_IX+3)
               AIF (&FIELD_OFF+&FIELD_LEN GT &SYM_LEN(&FIELD_IX))
                   MNOTE 8,'ZCGETFLD RMOD OFF+LEN > FIELD LENGTH'
                   MEXIT
               AEND
               AIF (&FIELD_SS1)
                   AIF (&FIELD_SIX)
                       :&FIELD_NAME SETC '&FIELD_OFF.(&FIELD_LEN,&FIELDX
               _REG)'
                   AELSE
                       :&FIELD_NAME SETC '&FIELD_NAME+&FIELD_OFF.(&FIELX
               D_LEN)'
                   AEND
               AELSE
                   AIF (&FIELD_SIX)
                       :&FIELD_NAME SETC '&FIELD_OFF.(&FIELD_REG)'
                   AELSE
                       :&FIELD_NAME SETC '&FIELD_NAME+&FIELD_OFF'
                   AEND
               AEND
               :&PARM_IX SETA &PARM_IX+5
         AELSE
               MNOTE 8,'ZCGETFLD RMOD SYNTAX ERROR (LIT:LIT) ONLY'
               MEXIT
         AEND
         AEND
.*
.* END OF ZCGETFLD.CPY
.*
