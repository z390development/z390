      *****************************************************************

      * z390 - Mainframe assembler emulator and run-time engine
      * Copyright (C) 2021 z390 Assembler LLC
      *
      * This file is part of z390.
      *
      * z390 is free software; you can redistribute it and/or modify
      * it under the terms of the GNU General Public License as 
      * published by the Free Software Foundation; either version 2 of 
      * the License, or (at your option) any later version.
      * z390 is distributed in the hope that it will be useful,
      * but WITHOUT ANY WARRANTY; without even the implied warranty of
      * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
      * GNU General Public License for more details.
      *
      * You should have received a copy of the GNU General Public 
      * License along with this program; if not, see 
      * https://www.gnu.org/licenses.
      *****************************************************************
      * Author - Don Higgins                                          *
      *****************************************************************
        IDENTIFICATION DIVISION.
      * TEST SEND/RECEIVE MAP AND SEND CONTROL
      *
      * CODE AND DISPLAYS ARE ALMOST IDENTICAL TO GUI4/6
      * AFTER GUI4 IS ENDED, THE PROGRAM SWITCHES INTO GUI6

        PROGRAM-ID. TESTBMC1.
        ENVIRONMENT DIVISION.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        COPY  MAP01C.
        COPY  DFHAID.
        COPY  DFHBMSCA.

        01  MYCOMM.
            02  COMMIND  PIC X.
            02  COMMNAME PIC X(40).
            02  COMMADDR PIC X(40).
            02  COMMZIP  PIC X(5).

        77  MYGUI6   PIC X(7)  VALUE 'GUI6   '.
        77  OCCSUM   PIC 9(7)  COMP-3.
        77  OCCINDX  PIC 9(2)  COMP.
        77  LOOPCTRL PIC 9(2)  COMP.
        77  SUMLEN   PIC 9(2)  COMP.

        01  OCCZONE  PIC 99.
        01  FILLER REDEFINES OCCZONE.
            02  FILLER   PIC X.
            02  OCCZONE2 PIC X.

        01  OCCCURSO.
            02  FILLER  PIC X(7)  VALUE 'OCCURS('.
            02  OCCNUM  PIC X.
            02  FILLER  PIC X     VALUE ')'.

        01  SHOWMSG.
            02  FILLER  PIC X(3)  VALUE 'MO='.
            02  SHOWMO  PIC XX.
            02  FILLER  PIC X(5)  VALUE ' DAY='.
            02  SHOWDAY PIC XX.
            02  FILLER  PIC X(4)  VALUE ' YR='.
            02  SHOWYR  PIC XX.

        01  BADKEY.
            02  FILLER  PIC X(26) VALUE 'NO ACTION FOR AID KEY HEX '.
            02  HEXKEY  PIC X(2).
            02  FILLER  PIC X(6)  VALUE SPACES.

        77  NOMODS   PIC X(34)
                     VALUE 'NO FIELDS CHANGED                 '.
        01  FAILMSG.
            02  FILLER  PIC X(8)  VALUE 'MAPFAIL/'.
            02  FAILIND PIC X.
            02  FILLER  PIC X(25) VALUE ' HAS OCCURRED            '.

        77  ENDMSG   PIC X(34)
                     VALUE 'TRANSACTION COMPLETE - PRESS CLEAR'.

        01  WORK1.
            02  FILLER PIC X(3).
            02  RESP2  PIC X.

        LINKAGE SECTION.
        01  DFHCOMMAREA.
            02  FILLER PIC X(86).

        PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA.

      *  XC    COMMIND(86),COMMIND CLEAR COMMAREA
            MOVE LOW-VALUES TO MYCOMM.
      *  ICM   R5,15,TCTTECA      ADDRESS RECEIVED COMMAREA ?
      *  BNZ   NOT1ST             EXIT IF OK
            IF EIBCALEN NOT = 0 THEN GO TO NOT1ST.
      * SETUP DSA COMMAREA
      *  LA    R5,COMMIND         ADDRESS DSA COMMAREA
      *  USING COMMIND,R5
      * RESEND4  EQU   *
        RESEND4.
      * INITIAL GUI4 MAP
            EXEC CICS SEND MAP('GUI4') MAPSET('MAP01C') MAPONLY ERASE
                      END-EXEC.
      *  MVI   COMMIND,C'1'       SET GUI4 REENTRY POINT
            MOVE '1' TO COMMIND.
      *  B     RETURN             EXIT
            GO TO ECRETURN.

      * NOT1ST   EQU   *
        NOT1ST.
      * PROCESS INPUT
            MOVE DFHCOMMAREA TO MYCOMM.
      *  CLI   COMMIND,C'1'       COMMAREA=1 ?
      *  BE    GUI4RENT           EXIT IF IT IS
      *  CLI   COMMIND,C'2'       COMMAREA=2 ?
      *  BE    GUI6RENT           EXIT IF IT IS
      *  CLI   COMMIND,C'3'       COMMAREA=3 ?
      *  BE    GUI6HRNT           EXIT IF IT IS
      *  DC    H'0'               BAD COMMAREA
            IF COMMIND = '1' THEN GO TO GUI4RENT.
            IF COMMIND = '2' THEN GO TO GUI6RENT.
            IF COMMIND = '3' THEN GO TO GUI6HRNT.
            GO TO ABEND.

      * GUI4RENT EQU   *
        GUI4RENT.
      * PROCESS GUI4 INPUT
            EXEC CICS IGNORE CONDITION MAPFAIL END-EXEC.
            EXEC CICS RECEIVE MAP('GUI4') MAPSET('MAP01C') END-EXEC.
      *  CLI   EIBAID,DFHPF3      PF3 ?
      *  BE    GUI6               YES, EXIT TO START GUI6
            IF EIBAID = DFHPF3   THEN GO TO GUI6.
      *  CLI   EIBAID,DFHCLEAR    CLEAR ?
      *  BE    RESEND4            YES, EXIT TO REFRESH DISPLAY
            IF EIBAID = DFHCLEAR THEN GO TO RESEND4.
      *  CLC   INPUTI,=C'END'     REQUEST TO END ?
      *  BE    GUI6               EXIT IF IT IS
            IF INPUTI = 'END' THEN GO TO GUI6.
      *  MVC   REPLYO,INPUTI      DISPLAY THE INPUT
            MOVE INPUTI TO REPLYO.
            EXEC CICS SEND MAP('GUI4') MAPSET('MAP01C') END-EXEC.
      *  B     RETURN             LOOP
            GO TO ECRETURN.

      * GUI6     EQU   *
        GUI6.
      * INITIAL GUI6 MAP
      *  XC    COMMIND(86),COMMIND CLEAR COMMAREA
            MOVE LOW-VALUES TO MYCOMM.
            EXEC CICS SEND MAP('GUI6') MAPSET('MAP01C') MAPONLY ERASE
                      END-EXEC.
      *  MVI   COMMIND,C'2'       SET GUI6 REENTRY POINT
            MOVE '2' TO COMMIND.
      *  B     RETURN             EXIT
            GO TO ECRETURN.

      * GUI6RENT EQU   *
        GUI6RENT.
            EXEC  CICS HANDLE CONDITION MAPFAIL(MAPFAILS) END-EXEC.
            EXEC  CICS RECEIVE MAP('GUI6') MAPSET('MAP01C') END-EXEC.

      * SHORTRD  EQU   *
        SHORTRD.
      *  CLI   EIBAID,DFHENTER    ENTER ?
      *  BE    GUI6DATA           EXIT IF IT IS
            IF EIBAID = DFHENTER THEN GO TO GUI6DATA.
      *  CLI   EIBAID,DFHPF1      PF1 ?
      *  BE    GUI6HELP           EXIT IF IT IS
            IF EIBAID = DFHPF1   THEN GO TO GUI6HELP.
      *  CLI   EIBAID,DFHPF2      PF2 ?
      *  BE    EAU                EXIT IF IT IS
            IF EIBAID = DFHPF2   THEN GO TO EAU.
      *  CLI   EIBAID,DFHPF3      PF3 ?
      *  BE    ALLDONE            EXIT IF IT IS
            IF EIBAID = DFHPF3   THEN GO TO ALLDONE.
      *  CLI   EIBAID,DFHCLEAR    CLEAR ?
      *  BE    GUI6               YES, EXIT TO REFRESH DISPLAY
            IF EIBAID = DFHCLEAR THEN GO TO GUI6.
      * BAD KEY
      *  UNPK  HEXKEY(3),EIBAID(2) UNPACK AID
      *  TR    HEXKEY(2),HEXTAB-240 TRANSLATE
      *  MVI   HEXKEY+2,C' '      CLEAR EXTRA BYTE
            IF EIBAID = DFHPF4  THEN MOVE 'F4' TO HEXKEY.
            IF EIBAID = DFHPF5  THEN MOVE 'F5' TO HEXKEY.
            IF EIBAID = DFHPF6  THEN MOVE 'F6' TO HEXKEY.
            IF EIBAID = DFHPF7  THEN MOVE 'F7' TO HEXKEY.
            IF EIBAID = DFHPF8  THEN MOVE 'F8' TO HEXKEY.
            IF EIBAID = DFHPF9  THEN MOVE 'F9' TO HEXKEY.
            IF EIBAID = DFHPF10 THEN MOVE '7A' TO HEXKEY.
            IF EIBAID = DFHPF11 THEN MOVE '7B' TO HEXKEY.
            IF EIBAID = DFHPF12 THEN MOVE '7C' TO HEXKEY.
            IF EIBAID = DFHPF13 THEN MOVE 'C1' TO HEXKEY.
            IF EIBAID = DFHPF14 THEN MOVE 'C2' TO HEXKEY.
            IF EIBAID = DFHPF15 THEN MOVE 'C3' TO HEXKEY.
            IF EIBAID = DFHPF16 THEN MOVE 'C4' TO HEXKEY.
            IF EIBAID = DFHPF17 THEN MOVE 'C5' TO HEXKEY.
            IF EIBAID = DFHPF18 THEN MOVE 'C6' TO HEXKEY.
            IF EIBAID = DFHPF19 THEN MOVE 'C7' TO HEXKEY.
            IF EIBAID = DFHPF20 THEN MOVE 'C8' TO HEXKEY.
            IF EIBAID = DFHPF21 THEN MOVE 'C9' TO HEXKEY.
            IF EIBAID = DFHPF22 THEN MOVE '4A' TO HEXKEY.
            IF EIBAID = DFHPF23 THEN MOVE '4B' TO HEXKEY.
            IF EIBAID = DFHPF24 THEN MOVE '4C' TO HEXKEY.
            IF EIBAID = DFHPA1  THEN MOVE '6C' TO HEXKEY.
            IF EIBAID = DFHPA2  THEN MOVE '6E' TO HEXKEY.
            IF EIBAID = DFHPA3  THEN MOVE '6B' TO HEXKEY.
      *  MVC   ERRMSGO,BADKEY     MOVE BAD KEY MESSAGE
            MOVE BADKEY TO ERRMSGO.
      *  B     REDISP             EXIT
            GO TO REDISP.

      * EAU      EQU   *
        EAU.
            EXEC CICS SEND CONTROL ERASEAUP CURSOR END-EXEC.
      *  B     RETURN             EXIT
            GO TO ECRETURN.

      * GUI6DATA EQU   *
        GUI6DATA.
      * ENTER PRESSED
      *
      * DISPLAY CURSOR LOCATION
      *  MVC   CURSO,=9C'?'       MARK CURSOR LOCATION UNKNOWN
            MOVE '?????????' TO CURSO.
      *  TM    NAMEF,DFHBMCUR     CURSOR ON NAME ?
      *  BNO   CURS01             EXIT IF NOT
      *  MVC   CURSO,=CL9'NAME'   MARK CURSOR LOCATION
            IF NAMEF = DFHBMCUR OR
               NAMEF = DFHBMEF  THEN
            MOVE 'NAME     ' TO CURSO
      *  B     GUI6PROC           EXIT
            GO TO GUI6PROC.

      * CURS01   EQU   *
      *  TM    ADDRF,DFHBMCUR     CURSOR ON ADDR ?
      *  BNO   CURS02             EXIT IF NOT
      *  MVC   CURSO,=CL9'ADDR'   MARK CURSOR LOCATION
            IF ADDRF = DFHBMCUR OR
               ADDRF = DFHBMEF  THEN
            MOVE 'ADDR     ' TO CURSO
      *  B     GUI6PROC           EXIT
            GO TO GUI6PROC.

      * CURS02   EQU   *
      *  TM    ZIPF,DFHBMCUR      CURSOR ON ZIP ?
      *  BNO   CURS03             EXIT IF NOT
      *  MVC   CURSO,=CL9'ZIP'    MARK CURSOR LOCATION
            IF ZIPF  = DFHBMCUR OR
               ZIPF  = DFHBMEF  THEN
            MOVE 'ZIP      ' TO CURSO
      *  B     GUI6PROC           EXIT
            GO TO GUI6PROC.

      * CURS03   EQU   *
      *  LA    R6,1               COUNT OCCURS
      *  LA    R7,TESTOCCF        ADDRESS OCCURS
            MOVE 1 TO OCCINDX.
      * OCCLOOP  EQU   *
        OCCLOOP.
      *  TM    0(R7),DFHBMCUR     CURSOR ON THIS OCCURS ?
      *  BO    CURSOCC            EXIT IF IT IS
            IF TESTOCCF IN TESTOCCD (OCCINDX) = DFHBMCUR OR
               TESTOCCF IN TESTOCCD (OCCINDX) = DFHBMEF  THEN
            GO TO CURSOCC.
      *  AHI   R6,1               BUMP OCCURS COUNT
            ADD 1 TO OCCINDX.
      *  CHI   R6,6               GOT TO END ?
      *  BE    CURS04             EXIT
            IF OCCINDX = 6 THEN GO TO CURS04.
      *  AHI   R7,3+5             BUMP TO NEXT OCCURS
      *  B     OCCLOOP            LOOP
            GO TO OCCLOOP.

      * CURSOCC  EQU   *
        CURSOCC.
      *  MVC   CURSO,=CL9'OCCURS(.)' MARK CURSOR LOCATION
      *  STC   R6,CURSO+7         STORE OCCURS NUMBER
      *  OI    CURSO+7,X'F0'      CONVERT TO CHAR
            MOVE OCCINDX  TO OCCZONE.
            MOVE OCCZONE2 TO OCCNUM.
            MOVE OCCCURSO TO CURSO.
      *  B     GUI6PROC           EXIT
            GO TO GUI6PROC.

      * CURS04   EQU   *
        CURS04.
      *  TM    DATEF,DFHBMCUR     CURSOR ON DATE ?
      *  BNO   CURS05             EXIT IF NOT
      *  MVC   CURSO,=CL9'GRPNAME' MARK CURSOR LOCATION
            IF DATEF = DFHBMCUR OR
               DATEF = DFHBMEF  THEN
            MOVE 'GRPNAME  ' TO CURSO
      *  B     GUI6PROC           EXIT
            GO TO GUI6PROC.

      * CURS05   EQU   *
      *  TM    TESTPCOF,DFHBMCUR  CURSOR ON TESTPCO ?
      *  BNO   GUI6PROC           EXIT IF NOT
      *  MVC   CURSO,=CL9'TESTPCO' MARK CURSOR LOCATION
            IF TESTPCOF = DFHBMCUR OR
               TESTPCOF = DFHBMEF  THEN
            MOVE 'TESTPCO  ' TO CURSO.

      * GUI6PROC EQU   *
        GUI6PROC.
      *  LH    R6,NAMEL           TOTAL
      *  AH    R6,ADDRL                 ALL
      *  AH    R6,ZIPL                      LENGTHS
      *  BNZ   SOMEDATA           EXIT IF NOT ZERO
            MOVE 0 TO SUMLEN.
            ADD NAMEL, ADDRL, ZIPL TO SUMLEN.
            IF SUMLEN NOT = 0 THEN GO TO SOMEDATA.
      * NO FIELDS MODIFIED
      *  MVC   ERRMSGO,NOMODS     MOVE NO FIELDS MESSAGE
            MOVE NOMODS TO ERRMSGO.
      * REDISP   EQU   *
        REDISP.
      *  CLI   COMMNAME,X'00'     ANY PREVIOUS NAME ?
      *  BE    TRYADDR2           EXIT IF NOT
      *  MVC   NAMEHDO,=C'NAME'   SHOW HEADER
      *  MVC   NEWNAMEO,COMMNAME  MOVE ANY PREVIOUS NAME
            IF COMMNAME NOT = LOW-VALUES THEN
            MOVE 'NAME' TO NAMEHDO
            MOVE COMMNAME TO NEWNAMEO.
      * TRYADDR2 EQU   *
      *  CLI   COMMADDR,X'00'     ANY PREVIOUS ADDR ?
      *  BE    TRYZIP2            EXIT IF NOT
      *  MVC   ADDRHDO,=C'ADDR'   SHOW HEADER
      *  MVC   NEWADDRO,COMMADDR  MOVE ANY PREVIOUS ADDR
            IF COMMADDR NOT = LOW-VALUES THEN
            MOVE 'ADDR' TO ADDRHDO
            MOVE COMMADDR TO NEWADDRO.
      * TRYZIP2  EQU   *
      *  CLI   COMMZIP,X'00'      ANY PREVIOUS ZIP ?
      *  BE    TRYPAY             EXIT IF NOT
      *  MVC   ZIPHDO,=C'ZIP '    SHOW HEADER
      *  MVC   NEWZIPO,COMMZIP    MOVE ANY PREVIOUS ZIP
            IF COMMZIP NOT = LOW-VALUES THEN
            MOVE 'ZIP ' TO ZIPHDO
            MOVE COMMZIP TO NEWZIPO.
      *  B     TRYPAY             EXIT
            GO TO TRYPAY.

      * SOMEDATA EQU   *
        SOMEDATA.
      *  XC    ERRMSGO,ERRMSGO    CLEAR ERROR MESSAGE
      *  MVI   NAMEHDO,X'00'      ASSUME NO NAME HEADER
      *  MVI   NEWNAMEO,X'00'     ASSUME NO NAME
            MOVE LOW-VALUES TO ERRMSGO.
            MOVE LOW-VALUES TO NAMEHDO.
            MOVE LOW-VALUES TO NEWNAMEO.
      *  OC    NAMEL,NAMEL        ANY NAME ENTERED ?
      *  BZ    NONAME             EXIT IF NOT
            IF NAMEL = 0 THEN GO TO NONAME.
      *  MVC   NAMEHDO,=C'NAME'   SHOW HEADER
      *  MVC   NEWNAMEO,NAMEI     SHOW NAME
      *  MVC   COMMNAME,NAMEI     SAVE NAME
            MOVE 'NAME' TO NAMEHDO.
            MOVE NAMEI  TO NEWNAMEO.
            MOVE NAMEI  TO COMMNAME.
      *  B     TRYADDR            EXIT
            GO TO TRYADDR.

      * NONAME   EQU   *
        NONAME.
      *  CLI   COMMNAME,X'00'     ANY PREVIOUS NAME ?
      *  BE    TRYADDR            EXIT IF NONE
            IF COMMNAME NOT = LOW-VALUES THEN
      *  MVC   NAMEHDO,=C'NAME'   SHOW HEADER
      *  MVC   NEWNAMEO,COMMNAME  SHOW PREVIOUS NAME
            MOVE 'NAME'   TO NAMEHDO
            MOVE COMMNAME TO NEWNAMEO.

      * TRYADDR  EQU   *
        TRYADDR.
      *  MVI   ADDRHDO,X'00'      ASSUME NO ADDR HEADER
      *  MVI   NEWADDRO,X'00'     ASSUME NO ADDR
      *  OC    ADDRL,ADDRL        ANY ADDR ENTERED ?
      *  BZ    NOADDR             EXIT IF NOT
            IF ADDRL = 0 THEN GO TO NOADDR.
      *  MVC   ADDRHDO,=C'ADDR'   SHOW HEADER
      *  MVC   NEWADDRO,ADDRI     SHOW ADDR
      *  MVC   COMMADDR,ADDRI     SAVE ADDR
            MOVE 'ADDR' TO ADDRHDO.
            MOVE ADDRI  TO NEWADDRO.
            MOVE ADDRI  TO COMMADDR.
      *  B     TRYZIP             EXIT
            GO TO TRYZIP.

      * NOADDR   EQU   *
        NOADDR.
      *  CLI   COMMADDR,X'00'     ANY PREVIOUS ADDR ?
      *  BE    TRYZIP             EXIT IF NONE
            IF COMMADDR NOT = LOW-VALUES THEN
      *  MVC   ADDRHDO,=C'ADDR'   SHOW HEADER
      *  MVC   NEWADDRO,COMMADDR  SHOW PREVIOUS ADDR
            MOVE 'ADDR'   TO ADDRHDO
            MOVE COMMADDR TO NEWADDRO.

      * TRYZIP   EQU   *
        TRYZIP.
      *  MVI   ZIPHDO,X'00'       ASSUME NO ZIP HEADER
      *  MVI   NEWZIPO,X'00'      ASSUME NO ZIP
            MOVE LOW-VALUES TO ZIPHDO.
            MOVE LOW-VALUES TO NEWZIPO.
      *  OC    ZIPL,ZIPL          ANY ZIP ENTERED ?
      *  BZ    NOZIP              EXIT IF NOT
            IF ZIPL = 0 THEN GO TO NOZIP.
      *  MVC   ZIPHDO,=C'ZIP '    SHOW HEADER
      *  MVC   NEWZIPO,ZIPI       SHOW ZIP
      *  MVC   COMMZIP,ZIPI       SAVE ZIP
            MOVE 'ZIP ' TO ZIPHDO.
            MOVE ZIPI   TO NEWZIPO.
            MOVE ZIPI   TO COMMZIP.
      *  B     TRYPAY             EXIT
            GO TO TRYPAY.

      * NOZIP    EQU   *
        NOZIP.
      *  CLI   COMMZIP,X'00'      ANY PREVIOUS ZIP ?
      *  BE    TRYPAY             EXIT IF NONE
            IF COMMZIP NOT = LOW-VALUES THEN
      *  MVC   ZIPHDO,=C'ZIP '    SHOW HEADER
      *  MVC   NEWZIPO,COMMZIP    SHOW PREVIOUS ZIP
            MOVE 'ZIP '  TO ZIPHDO
            MOVE COMMZIP TO NEWZIPO.

      * TRYPAY   EQU   *
        TRYPAY.
      *  OC    TESTPCOL,TESTPCOL  ANY TESTPCO ?
      *  BZ    DOGRP              EXIT IF NONE
            IF TESTPCOL = 0 THEN GO TO DOGRP.
      *  MVC   PAYMEO,=C'Pay the Author' MOVE MESSAGE
      *  MVC   PAYME1O,TESTPCOI   MOVE INPUT DATA
      *  XC    TESTPCOO,TESTPCOO  CLEAR THE INPUT DATA, NOT WANTED
            MOVE 'Pay the Author' TO PAYMEO.
            MOVE TESTPCOI         TO PAYME1O.
            MOVE LOW-VALUES       TO TESTPCOO.

      * DOGRP    EQU   *
        DOGRP.
      * PROCESS GRPNAME
      *  MVC   SHOWMDYO,=C'MO=.. DAY=.. YR=..' MOVE MESSAGE
      *  MVC   SHOWMDYO+3(2),MOO  MOVE MONTH
      *  MVC   SHOWMDYO+10(2),DAYO MOVE DAY
      *  MVC   SHOWMDYO+16(2),YRO MOVE YEAR
            MOVE MOO TO SHOWMO.
            MOVE DAYO TO SHOWDAY.
            MOVE YRO TO SHOWYR.
            MOVE SHOWMSG TO SHOWMDYO.
      * PROCESS OCCURS
      *  SP    OCCSUM,OCCSUM      CLEAR OCCURS SUM
            MOVE 0 TO OCCSUM.
      *  LA    R6,5               R6=NO. OF OCCURS
            MOVE 1 TO LOOPCTRL.
      *  LA    R7,TESTOCCO        OCCURS DATA
      * OCCSUMLP EQU   *
        OCCSUMLP.
      *  CLI   0(R7),X'00'        ANY DATA HERE ?
      *  BE    OCCSUMIN           EXIT IF NONE
            IF TESTOCCI IN TESTOCCD (LOOPCTRL) NOT = LOW-VALUES THEN
      *  PACK  WORK,0(5,R7)       PACK DATA
      *  AP    OCCSUM,WORK        SUM IT
            ADD TESTOCCI IN TESTOCCD (LOOPCTRL) TO OCCSUM.
      * OCCSUMIN EQU   *
      *  AHI   R7,3+5             BUMP TO NEXT OCCURS
      *  BCT   R6,OCCSUMLP        LOOP
            ADD 1 TO LOOPCTRL.
            IF LOOPCTRL NOT = 6 THEN GO TO OCCSUMLP.
      *  UNPK  OCCSUMO(7),OCCSUM  UNPACK RESULT
            MOVE OCCSUM TO OCCSUMO.
      *  B     SENDIT             EXIT
            GO TO SENDIT.

      * GUI6HELP EQU   *
        GUI6HELP.
      * GUI6 HELP MAP
            EXEC CICS SEND MAP('GUI6H') MAPSET('MAP01C') MAPONLY ERASE
                      END-EXEC.
      *  MVI   COMMIND,C'3'       SET GUI6 REENTRY POINT
            MOVE '3' TO COMMIND.

      * RETURN   EQU   *
        ECRETURN.
            EXEC CICS RETURN TRANSID('BMC1') COMMAREA(MYCOMM)
                      LENGTH(86) END-EXEC.

      * GUI6HRNT EQU   *
        GUI6HRNT.
      * RETURN FROM HELP SCREEN
      *  MVI   COMMIND,C'2'       SET GUI6 REENTRY POINT
            MOVE '2' TO COMMIND.
      *  CLI   EIBAID,DFHCLEAR    CLEAR ?
      *  BE    GUI6               YES, EXIT TO REFRESH DISPLAY
            IF EIBAID = DFHCLEAR THEN GO TO GUI6.
      *  CLI   EIBAID,DFHPF3      PF3 ?
      *  BNE   REDISP             EXIT IF NOT
            IF EIBAID NOT = DFHPF3 THEN GO TO REDISP.

      * ALLDONE  EQU   *
        ALLDONE.
      *  MVC   ERRMSGO,ENDMSG     MOVE ENDING MESSAGE
            MOVE ENDMSG TO ERRMSGO.
            EXEC CICS SEND MAP('GUI6') MAPSET('MAP01C') END-EXEC.
            EXEC CICS RETURN END-EXEC.

      * SENDIT   EQU   *
        SENDIT.
      * TEST MAP AS LABEL WITH FROM AND LENGTH
            EXEC CICS SEND MAP(MYGUI6) MAPSET('MAP01C') FROM(GUI6O)
                      LENGTH(LENGTH OF GUI6O) ERASE END-EXEC.
      *  B     RETURN             EXIT
            GO TO ECRETURN.

      * MAPFAILS EQU   *
        MAPFAILS.
      *  CLC   EIBRESP2,=F'2'     SHORT READ ?
      *  BE    SHORTRD            EXIT IF IT IS
            MOVE EIBRESP2 TO WORK1.
            IF RESP2 = X'02' THEN GO TO SHORTRD.
      *  MVC   FAILMSG+8(1),EIBRESP2+3 SET RESP2
      *  OI    FAILMSG+8,X'F0'    CONVERT TO CHAR
            IF RESP2 = X'01' THEN MOVE '1' TO FAILIND.
            IF RESP2 = X'03' THEN MOVE '3' TO FAILIND.
            IF RESP2 = X'04' THEN MOVE '4' TO FAILIND.
            IF RESP2 = X'05' THEN MOVE '5' TO FAILIND.
            IF RESP2 = X'06' THEN MOVE '6' TO FAILIND.
            IF RESP2 = X'07' THEN MOVE '7' TO FAILIND.
            IF RESP2 = X'08' THEN MOVE '8' TO FAILIND.
      *  MVC   ERRMSGO,FAILMSG    MOVE FAILURE MESSAGE
            MOVE FAILMSG TO ERRMSGO.
      *  B     SENDIT             EXIT
            GO TO SENDIT.

        ABEND.
            EXEC CICS ABEND ABCODE('BMC1') END-EXEC.
