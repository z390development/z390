.*   Copyright (C) <2020>  Daniel H. Snyder
.*   Sructured Programming Macros for the IBM® High Level Assembler
.*   Daniel.H.Snyder@protonmail.com
.*
.* This program is free software: you can redistribute it and/or modify
.* it under the terms of the GNU General Public License as published by
.* the Free Software Foundation, either version 3 of the License, or
.* any later version.
.*
.* This program is distributed in the hope that it will be useful,
.* but WITHOUT ANY WARRANTY; without even the implied warranty of
.* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
.* GNU General Public License for more details.
.*
.* You should have received a copy of the GNU General Public License
.* along with this program. See <https://www.gnu.org/licenses/>.
.*
         COPY  $MGBLDEF            BRING IN GLOBAL SYMBOLS
         LCLA  &I,&J,&K
         LCLC  &LENGTH1,&LENGTH2,&LENGTH3 ANALYZE &$BT
         LCLC  &X,&Y,&OP,&OPND1,&OPND2,&OPND3,&C,&NC,&TSTCOND
         LCLC  &OPND4,&OPND5,&OPND6
.*
.* =*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*
.*                                                                    *
.* IMPORTANT SET SYMBOL NAMES                                         *
.*                                                                    *
.* &$MFNAME WILL CONTAIN THE UNIQUE TAG FOR THE CONDITION WHERE A     *
.*          BRANCH ON FALSE WILL OCCUR.                               *
.* &$MTNAME WILL CONTAIN THE UNIQUE TAG FOR THE CONDITION WHERE A     *
.*          BRANCH ON TRUE WILL OCCUR.                                *
.* &$BT     IS THE EXTENDED CONDITION CODE MNEMONIC SPECIFIED IN THE  *
.*          MACRO CALL STATING THE CONDITION IN WHICH THE PREMISE OF  *
.*          THE LOGIC PHRASE IS TRUE.                                 *
.* &$BF     IS THE EXTENDED CONDITION CODE MNEMONIC CALCULATED WITHIN *
.*          THE MACRO GENERATION STATING THE CONDITION IN WHICH THE   *
.*          PREMISE OF THE LOGIC PHRASE IS FALSE.                     *
.*                                                                    *
.* =*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*
.*
.* =*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*
.*                                                                    *
.* INITIALIZATION.                                                    *
.*                                                                    *
.* =*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*
.*
&$MLVL   SETA  &$MLVL+1            INCREASE RECURSION/NESTING COUNT
&I       SETA  1                   SET TO FIRST POSITIONAL
&$MNAME  SETC  '&NAME'             TAG FOR FIRST TEST
.* 
.* =*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*
.*                                                                    *
.* PROCESS THE LOGIC PACK FOR OP-CODE, OPERAND(S), AND THE CONDITION  *
.* UNDER WHICH THE FOLLOWING CODE IS TO BE EXECUTED.                  *
.*                                                                    *
.* =*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*
.* 
.LOOP020 ANOP  START THE PROCESS (MAYBE) AGAIN
         AIF   ('&SYSLIST(&I)' EQ '').FINISH260  GO TO FINISH IF DONE
&$ERRSW  SETC  'N'                 NO ERROR EXISTS AT THIS POINT     
         AIF   ('&SYSLIST(&I)'(1,1) NE '(').FATAL180   MUST BE '('
         AIF   ('&SYSLIST(&I)'(2,1) EQ '(').RECURS120 GO IF NESTED
&OP      SETC  '&SYSLIST(&I,1)'    OP-CODE IS ALWAYS 1ST SUB-OPERAND
&J       SETA  (N'&SYSLIST(&I))    NUMBER OF SUBPARAMETERS FOR THIS ONE
         AIF   (&J NE 2).TEST030     GO TEST FOR 1-OPERAND INST
&$MNAME  &OP   ,
&$BT     SETC  '&SYSLIST(&I,2)'    CONDITION IS ALWAYS LAST SUB-OPERAND
         AGO   .FINGEN070             GO SET UP BRANCH INST
.TEST030 ANOP  ,
&OPND1   SETC  '&SYSLIST(&I,2)'    FIRST (ONLY?) OPERAND
         AIF   (&J NE 3).TEST040     GO TEST FOR 2-OPERAND INST
&$MNAME  &OP   &OPND1
&$BT     SETC  '&SYSLIST(&I,3)'    CONDITION IS ALWAYS LAST SUB-OPERAND
         AGO   .FINGEN070             GO SET UP BRANCH INST
.TEST040 ANOP  ,
&OPND2   SETC  '&SYSLIST(&I,3)'    SECOND OPERAND
         AIF   (&J NE 4).TEST050     GO TEST FOR 3-OPERAND INST
&$MNAME  &OP   &OPND1,&OPND2
&$BT     SETC  '&SYSLIST(&I,4)'    CONDITION IS ALWAYS LAST SUB-OPERAND
         AGO   .FINGEN070             GO SET UP BRANCH INST
.TEST050 ANOP  ,
&OPND3   SETC  '&SYSLIST(&I,4)'    THIRD OPERAND
         AIF   (&J NE 5).TEST052
&$MNAME  &OP   &OPND1,&OPND2,&OPND3
&$BT     SETC  '&SYSLIST(&I,5)'   CONDITION IS ALWAYS LAST SUB-OPERAND
         AGO   .FINGEN070            GO SET UP BRANCH INST
.TEST052 ANOP
&OPND4   SETC  '&SYSLIST(&I,5)'    FOURTH OPERAND
         AIF   (&J NE 6).TEST054
&$MNAME  &OP   &OPND1,&OPND2,&OPND3,&OPND4
&$BT     SETC  '&SYSLIST(&I,6)'   CONDITION IS ALWAYS LAST SUB-OPERAND
         AGO   .FINGEN070            GO SET UP BRANCH INST
.TEST054 ANOP
&OPND5   SETC  '&SYSLIST(&I,6)'    FIFTH OPERAND
         AIF   (&J NE 7).TEST056
&$MNAME  &OP   &OPND1,&OPND2,&OPND3,&OPND4,&OPND5
&$BT     SETC  '&SYSLIST(&I,7)'   CONDITION IS ALWAYS LAST SUB-OPERAND
         AGO   .FINGEN070            GO SET UP BRANCH INST
.TEST056 ANOP
&OPND6   SETC  '&SYSLIST(&I,7)'    SIXTH OPERAND
         AIF   (&J NE 8).ERROR060
&$MNAME  &OP   &OPND1,&OPND2,&OPND3,&OPND4,&OPND5,&OPND6
&$BT     SETC  '&SYSLIST(&I,8)'   CONDITION IS ALWAYS LAST SUB-OPERAND
         AGO   .FINGEN070            GO SET UP B
.ERROR060 ANOP ,
&K       SETA  ((&I+3)/2)          GET SEQUENCE # OF PHRASE IN ERROR
         MNOTE 08,'INVALID NUMBER OF PARMS (&J) IN CONDITIONAL PHRASE #*
               &K &SYSLIST(&I); IT WILL BE DELETED FROM THE MACRO EXPAN*
               SION'
&I       SETA  &I+2                INCREMENT
         AGO   .LOOP020             BACK FOR MORE
.* 
.* =*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*
.*                                                                    *
.* THE TEST INSTRUCTION THAT SETS THE CONDITION CODE HAS BEEN         *
.* GENERATED; NOW SET UP FOR CREATING THE BRANCH-ON-CONDITION.        *
.*                                                                    *
.* =*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*
.* 
.FINGEN070 ANOP
&$MNAME  SETC  ''                  PREVENT DUPLICATE TAG
.* 
.* =*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*
.*                                                                    *
.* &$BT IS THE VALUE OF THE CONDITION SPECIFIED IN THE MACRO CALL.    *
.* &$BT IS THE MNEMONIC PORTION OF THE BRANCH INSTRUCTION THAT WILL   *
.* GENERATE A BRANCH CONDITION THAT WILL BRANCH IF THE CONDITION      *
.* BEING TESTED IS TRUE. IF &$BT HAS THE VALUE OF "NE", THAT MEANS    *
.* THAT THE BODY OF THE IF-GROUP WILL BE EXECUTED IF THE RESULT OF    *
.* PREVIOUSLY GENERATED INSTRUCTION IS "NE", AND THE INSTRUCTION      *
.* GENERATED WILL NORMALLY BE "JE" TO BRANCH AROUND THE EXECUTABLE    *
.* BODY OF THE IF-GROUP. THE ONLY EXCEPTION IS IF THE LOGIC AROSE     *
.* FROM A "DO UNTIL=..." MACRO CALL, WHERE THE BRANCH AROUND THE      *
.* EXECUTABLE BODY OF THE DO-GROUP WOULD BE "JNE".                    *
.*                                                                    *
.* &$BF (BRANCH ON FALSE) WILL HAVE THE OPPOSITE VALUE TO &$BT; IN    *
.* THIS INSTANCE, IT WOULD BE "E".                                    *
.*                                                                    *
.* =*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*
.* 
&LENGTH1 SETC  '&$BT'(1,1)        1ST CHAR OF USER-SPECIFIED CONDITION
&LENGTH2 SETC  '&$BT'(1,2)        FIRST TWO CHARACTERS
&LENGTH3 SETC  '&$BT'             ALL CHARACTERS
         AIF ('&LENGTH2' NE '&LENGTH3').ERR100 LENGTH OF COND PARM > 2?
         AIF   ('&LENGTH1' EQ '&LENGTH2').ONE080 ONE CHAR CONDITION 
         AIF   ('&LENGTH1' NE 'N').ERR100 IF 2 CHAR 1ST MUST BE "N"
&$BF     SETC  '&LENGTH2'(2,1) &$BT 1ST CHAR IS "N", 2ND IS CONDITION
&TSTCOND SETC  '&$BF'   FOR VALIDATING CONDITION
         AGO   .FIRST090
.ONE080  ANOP
&$BF     SETC  'N&LENGTH1' 1 CHAR CONDITION; "N" WILL BE 1ST CHAR
&TSTCOND SETC  '&$BT'   FOR VALIDATING CONDITION
.FIRST090 ANOP , MNOTE '$BT "&$BT" $BF "&$BF"'
         AIF   ('&TSTCOND' EQ 'E').OK065
         AIF   ('&TSTCOND' EQ 'H').OK065
         AIF   ('&TSTCOND' EQ 'L').OK065
         AIF   ('&TSTCOND' EQ 'M').OK065
         AIF   ('&TSTCOND' EQ 'O').OK065
         AIF   ('&TSTCOND' EQ 'P').OK065
         AIF   ('&TSTCOND' EQ 'Z').OK065
.* =*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*
.ERR100  MNOTE 8,'INVALID CONDITION REQUESTED: "&LENGTH3"'
         MNOTE 'THIS CLAUSE DELETED FROM ASSEMBLY'
&$ERRSW  SETC  'Y'                 FLAG ERROR
&$BT     SETC  'NOP'               FLAG ERROR
&$BF     SETC  'NOP'               FLAG ERROR
.OK065   AIF   ('&$ERRSW' EQ 'N').DISTR175
&I       SETA  &I+2
         AGO   .LOOP020
.* 
.* =*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*
.*                                                                    *
.* PROCESS A NEW LOGIC PACK OPENED WITH MULTIPLE PARENTHESES, MEANING *
.* THAT "OR" CONJUNCTIONS MAY BE FORCED TO HIGHER PRIORITIES THAN THE *
.* "AND" CONJUNCTIONS.                                                *
.*                                                                    *
.* =*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*
.* 
.RECURS120 ANOP                    HERE FOR RECURSIVE CALL
&$MNAME  IF2   &SYSLIST(&I,1),&SYSLIST(&I,2),&SYSLIST(&I,3),           C
               &SYSLIST(&I,4),&SYSLIST(&I,5),&SYSLIST(&I,6),           C
               &SYSLIST(&I,7),&SYSLIST(&I,8),&SYSLIST(&I,9),           C
               &SYSLIST(&I,10),&SYSLIST(&I,11),&SYSLIST(&I,12),        C
               &SYSLIST(&I,13),&SYSLIST(&I,14),&SYSLIST(&I,15),        C
               &SYSLIST(&I,16),&SYSLIST(&I,17),&SYSLIST(&I,18),        C
               &SYSLIST(&I,19),&SYSLIST(&I,20),&SYSLIST(&I,21)
&J       SETA  &$MLVL+1              VALUE OF LEVEL JUST RETURNED FROM
.* =*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*
.* RESOLVE THE TWO POSSIBLE &$FNAME VALUES FROM RECURSIVE LEVEL AND   *
.* THIS LEVEL.                                                        *
.* =*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*
         AIF   ('&$MFNAME(&J)' EQ '').RECURS150
         AIF   ('&$MFNAME(&$MLVL)' EQ '').RECURS140
&$MPTR   SETA  &$MPTR+1
&$MEQ1(&$MPTR) SETC '&$MFNAME(&J)'
&$MEQ2(&$MPTR) SETC '&$MFNAME(&$MLVL)'
         AGO   .RECURS150
.RECURS140 ANOP
&$MFNAME(&$MLVL) SETC '&$MFNAME(&J)'
.RECURS150 ANOP
&$MFNAME(&J) SETC ''
.* =*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*
.* IF THERE IS AN "AND" CONJUNCTION AS THE NEXT PHRASE, THEN DO NOT   *
.* MIGRATE THE MTNAME VALUES; THE &$MTNAME PASSED FROM THE RECURSIVE  *
.* LEVEL WILL BE TRAPPED FOLLOWING THE BRANCH-ON-FALSE GENERATED IN   *
.* THE ".AND220" ROUTINE.                                             *
.* =*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*
         AIF   ('&SYSLIST(&I+1)' EQ 'AND').AND220
         AIF   ('&$MTNAME(&J)' EQ '').RECURS170
         AIF   ('&$MTNAME(&$MLVL)' EQ '').RECURS160
&$MPTR   SETA  &$MPTR+1
&$MEQ1(&$MPTR) SETC '&$MTNAME(&J)'
&$MEQ2(&$MPTR) SETC '&$MTNAME(&$MLVL)'
         AGO   .RECURS170
.RECURS160 ANOP
&$MTNAME(&$MLVL) SETC '&$MTNAME(&J)'
.RECURS170 ANOP
&$MTNAME(&J) SETC ''
.* =*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*
.DISTR175 AIF ('&SYSLIST(&I+1)' EQ 'OR').OR190
         AIF   ('&SYSLIST(&I+1)' EQ 'AND').AND220
         AIF   ('&SYSLIST(&I+1)' EQ '').FINISH260 GO IF NO CONJUNCTION
         MNOTE 8,'INVALID CONJUNCTION "&SYSLIST(&I+1)"'
         MNOTE 'THIS PHRASE DELETED FROM MACRO EXPANSION'         
&I       SETA  &I+2                BUMP TO NEXT CLAUSE
         AGO   .LOOP020
.* 
.* =*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*
.*                                                                    *
.* A LOGIC PACK DID NOT OPEN WITH A "(". ===> FATAL MISTAKE <===      *
.*                                                                    *
.* =*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*
.* 
.FATAL180   MNOTE 8,'MISSING PARENS AT "&SYSLIST(&I)" - TERMINATED'
         MEXIT
.* 
.* =*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*
.*                                                                    *
.* PROCESS A CLAUSE THAT IS FOLLOWED BY AN "OR" CONJUNCTION.          *
.*                                                                    *
.* =*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*
.* 
.OR190   AIF   ('&SYSLIST(&I+2)'(1,1) NE '(').ERROR255 MUST BE '('
         AIF   ('&$MTNAME(&$MLVL)' NE '').OR200 SKIP IF NOT NEEDED
&$Y$NDX  SETA  &$Y$NDX+1           BUMP FOR UNIQUENESS       
&$MTNAME(&$MLVL) SETC '$MDT&$Y$NDX' CREATE UNIQUE TAG
.OR200   J&$BT &$MTNAME(&$MLVL)
&$MNAME  SETC  '&$MFNAME(&$MLVL)'      QUEUE NAME FOR USE IN .LOOP020
&$MFNAME(&$MLVL) SETC  ''
&I       SETA  &I+2                BUMP TO NEXT CLAUSE
         AGO   .LOOP020
.* 
.* =*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*
.*                                                                    *
.* PROCESS A CLAUSE THAT IS FOLLOWED BY AN "AND" CONJUNCTION.         *
.*                                                                    *
.* =*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*
.* 
.AND220  ANOP
         AIF   ('&SYSLIST(&I+2)'(1,1) NE '(').ERROR255 MUST BE '('
         AIF   ('&$MFNAME(&$MLVL)' NE '').AND230
&$Y$NDX  SETA  &$Y$NDX+1           MUST HAVE A UNIQUE -
&$MFNAME(&$MLVL) SETC '$MDF&$Y$NDX'  TAG TO BRANCH TO
.AND230  J&$BF &$MFNAME(&$MLVL)
&J       SETA  &$MLVL+1            INDEX OF POSSIBLE RECURSIVE NAME
&$MNAME  SETC  '&$MTNAME(&J)'      QUEUE NAME FOR USE IN .LOOP020
&$MTNAME(&J) SETC  ''
&I       SETA  &I+2                BUMP TO NEXT
         AGO   .LOOP020             AND BACK
.* =*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*
.ERROR255 ANOP
         MNOTE 8,'NO VALID PHRASE FOLLOWING THE "&SYSLIST(&I+1)"'
&$ERRSW  SETC  'Y'                 FLAG ERROR
.* 
.* =*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*
.*                                                                    *
.* THIS IS THE END OF CONDITIONAL PHRASES AT THIS RECURSIVE LEVEL.    *
.*                                                                    *
.* =*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*
.*