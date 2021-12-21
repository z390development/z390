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
      *
      * TEST VSAM 2 (COBOL)
      *
      * RRDS/FIX MYFILE06 READ/RRN  3 REC  3  Merry Wives of Windsor
      * RRDS/FIX MYFILE06 READ/RRN  2 REC  2  Two Gentlemen of Verona
      * RRDS/FIX MYFILE06 READ/RRN  1 REC  1  The Tempest
      * RRDS/VAR MYFILE07 READ/RRN  5 REC  1  All's Well That Ends Well
      * RRDS/VAR MYFILE07 READ/RRN 10 REC  3  The Winter's Tale
      * NOTFND       CONDITION RAISED (READ INVALID RRN)
      * ILLOGIC      CONDITION RAISED (READ WITH RBA)
      * LENGERR      CONDITION RAISED         The Ta
      * RRDS/VAR MYFILE07 BROWSE FORWARD RRN 7-11 (READ RECS 2-4)
      *                                       The Taming of the Shrew
      *                                       The Winter's Tale
      *                                       The Comedy of Errors
      * RRDS/VAR MYFILE07 BROWSE BACK    RRN 11-8 (READ RECS 4-2)
      *                                       The Comedy of Errors
      *                                       The Winter's Tale
      *                                       The Taming of the Shrew
      * RRDS/VAR MYFILE07 BROWSE RANDOM  RRN 17 (REC 7) NEXT
      *                                       First Part of Henry IV
      *                                  RRN 22 (REC 6) PREV
      *                                       Richard II
      *                                  RRN 1  (REC 1)
      *                                       All's Well That Ends Well
      * ENDFILE      CONDITION RAISED (READNEXT RRN 99 BEYOND END)
      * ENDFILE      CONDITION RAISED (READPREV RRN 1)
      * RRDS COMPLEX TEST...DONE IN THIS SEQUENCE
      *    BROWSE FORWARD MYFILE06 RRN 6-9  (RECS 5-7)
      *    BROWSE BACK    MYFILE06 RRN 10-6 (RECS 7-5)
      *                                       Measure for Measure
      *                                       A Midsummer Night's Dream
      *                                       Much Ado About Nothing
      *                                       Much Ado About Nothing
      *                                       A Midsummer Night's Dream
      *                                       Measure for Measure
      * RRDS/VAR MYFILE07, RRN 10 (RECS 3-5), RESETBR, RRN 99
      *                                                     (RECS 10-8)
      *                                       The Winter's Tale
      *                                       The Comedy of Errors
      *                                       King John
      *                                       Henry V
      *                                       Second Part of Henry IV
      *                                       First Part of Henry IV
        PROGRAM-ID. TESTVSC2.
        ENVIRONMENT DIVISION.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        77  MYDATA   PIC X(50).
        77  MYLEN    PIC 9(4)  COMP.
        77  MYRID    PIC 9(8)  COMP.
        77  MYRID1   PIC 9(8)  COMP.
        77  MYRID2   PIC 9(9)  COMP.
        77  FOP1     PIC X(4)   VALUE X'C311C260'.
        77  FOP2     PIC X(4)   VALUE X'C311C3F0'.
        77  FOP3     PIC X(4)   VALUE X'C311C540'.
        77  FOP4     PIC X(4)   VALUE X'C311C650'.
        77  FOP5     PIC X(4)   VALUE X'C311C760'.
        77  FOP6     PIC X(4)   VALUE X'C311C8F0'.
        77  FOP7     PIC X(4)   VALUE X'C3114A40'.
        77  FOP8     PIC X(4)   VALUE X'C3114B50'.
        77  FOP9     PIC X(4)   VALUE X'C3114C60'.
        77  FOP10    PIC X(4)   VALUE X'C3114DF0'.
        77  FOP11    PIC X(4)   VALUE X'C3114F40'.
        77  FOP12    PIC X(4)   VALUE X'C3115050'.
        77  FOP13    PIC X(4)   VALUE X'C311D160'.
        77  FOP14    PIC X(4)   VALUE X'C311D2F0'.
        77  FOP15    PIC X(4)   VALUE X'C311D440'.
        77  FOP16    PIC X(4)   VALUE X'C311D550'.
        77  FOP17    PIC X(4)   VALUE X'C311D660'.
        77  FOP18    PIC X(4)   VALUE X'C311D7F0'.
        77  FOP19    PIC X(4)   VALUE X'C311D940'.
        77  FOP20    PIC X(4)   VALUE X'C3115A50'.
        77  FOP21    PIC X(4)   VALUE X'C3115B60'.
        77  FOP1C2   PIC X(4)   VALUE X'C311C3C9'.

      * RRDS MYFILE06 READ   RBA REC  3
        01  FOPLINE.
            02  FOPSBA   PIC X(4).
            02  FOPTYPE  PIC X(4).
            02  FILLER   PIC X     VALUE '/'.
            02  FOPFIXVR PIC X(3).
            02  FILLER   PIC X     VALUE SPACE.
            02  FOPNAME  PIC X(8).
            02  FILLER   PIC X     VALUE SPACE.
            02  FOPFUNC  PIC X(8)  VALUE 'READNEXT'.
            02  FILLER   PIC X     VALUE SPACE.
            02  FOPMETH  PIC X(4)  VALUE 'XRBA'.
            02  FILLER   PIC X(4)  VALUE ' REC'.
            02  FOPRECNO PIC X(3).
            02  FILLER   PIC X     VALUE SPACE.
            02  FOPSEP   PIC X     VALUE SPACE.
        77  FOPL     PIC 9(4) COMP.

        01  FHAND.
            02  FHASBA   PIC X(4).
            02  FHANDCON.
                03  FILLER   PIC X(8) VALUE SPACES.
                03  FHALNG   PIC X(2).
                03  FILLER   PIC X(2) VALUE SPACES.
            02  FILLER   REDEFINES FHANDCON.
                03  FILLER   PIC X(8) VALUE SPACES.
                03  FHAILL   PIC X(3).
                03  FILLER   PIC X    VALUE SPACE.
            02  FILLER   PIC X(18) VALUE ' CONDITION RAISED '.
            02  FHFDBK1  PIC X(2).
            02  FILLER   PIC X     VALUE SPACE.
            02  FHFDBK2  PIC X(2).
            02  FILLER   PIC X(4)  VALUE SPACES.
            02  FHSEP    PIC X     VALUE SPACE.
        77  FHANDL   PIC 9(4) COMP.

        01  ENDMSG.
            02  FILLER   PIC X(4)  VALUE X'C3115CF0'.
            02  FILLER   PIC X(31)
                         VALUE 'TESTVSC2 COMPLETED, PRESS CLEAR'.
        77  ENDMSGL  PIC 9(4) COMP.

        01  FILLER.
            02  WORK1A   PIC 9(4).
            02  FILLER   REDEFINES WORK1A.
                03  FILLER   PIC X(2).
                03  WORK1B   PIC X(2).
            02  FILLER   REDEFINES WORK1A.
                03  FILLER   PIC X(1).
                03  WORK1C   PIC X(3).

        01  WORK2.
            02  FILLER PIC X.
            02  FDBK1  PIC X.
            02  FDBK2  PIC X.
            02  FILLER PIC X(3).

      * GO TO DEPENDING CONTROL
      *   1=VSAMRTN1
      *   2=VSAMRTN2
      *   3=VSAMRTN3
      *   4=VSAMRTN4
      *   5=VSAMRTN5
      *   6=ABEND
        77  DEPCTRL   PIC 9(4) COMP.

        77  ENDFLENO  PIC 9(4) COMP.

        77  LOOPCTRL  PIC 9(2) COMP.

        PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA.

            MOVE LENGTH OF FOPLINE  TO FOPL.
            SUBTRACT 1 FROM FOPL.
            MOVE LENGTH OF FHAND    TO FHANDL.
            SUBTRACT 1 FROM FHANDL.
            MOVE LENGTH OF ENDMSG   TO ENDMSGL.
      *  CLI   FOPSEP+1,C' '      SEND LENGTH ADJUSTED ?
      *  BNE   NOTADJL            EXIT IF NOT
            IF FOPSEP NOT = SPACE THEN GO TO NOTADJL.
      *  MVI   FOPSEP+1,C'|'      SET SEPARATOR
            MOVE '|' TO FOPSEP.
      *  LH    R7,FOPL            MESSAGE LENGTH
      *  AHI   R7,1               LENGTHEN BY 1
      *  STH   R7,FOPL            STORE BACK
            ADD 1 TO FOPL.

      * NOTADJL  EQU   *
        NOTADJL.
      *  CLI   FHSEP,C' '         SEND CONDITION LENGTH ADJUSTED ?
      *  BNE   NOTADJC            EXIT IF NOT
            IF FHSEP NOT = SPACE THEN GO TO NOTADJC.
      *  MVI   FHSEP,C'|'         SET SEPARATOR
            MOVE '|' TO FHSEP.
      *  LH    R7,FHANDL          MESSAGE LENGTH
      *  AHI   R7,1               LENGTHEN BY 1
      *  STH   R7,FHANDL          STORE BACK
            ADD 1 TO FHANDL.

      * NOTADJC  EQU   *
        NOTADJC.
      *  MVC   FOPTYPE,=C'RRDS'   SET FILE TYPE IN MESSAGE
      *  MVC   FOPFIXVR,=C'FIX'   SET FIX/VAR IN MESSAGE
      *  MVC   FOPNAME,=C'MYFILE06' SET FILENAME IN MESSAGE
      *  MVC   FOPFUNC,=CL8'READ' SET FUNCTION IN MESSAGE
      *  MVC   FOPMETH,=CL4'RRN'  SET METHOD IN MESSAGE
      *  MVC   MYRID,=F'3'        RECORD 3
      *  MVC   MYLEN,=H'30'       SET MAXIMUM LENGTH
            MOVE 'RRDS'     TO FOPTYPE.
            MOVE 'FIX'      TO FOPFIXVR.
            MOVE 'MYFILE06' TO FOPNAME.
            MOVE 'READ    ' TO FOPFUNC.
            MOVE 'RRN '     TO FOPMETH.
            MOVE 3          TO MYRID.
            MOVE 30         TO MYLEN.
            EXEC CICS READ FILE('MYFILE06') INTO(MYDATA) LENGTH(MYLEN)
                      RIDFLD(MYRID) RRN END-EXEC.
      *  BAL   R9,WQTS            WRITE IT TO TS Q
            PERFORM WQTS.
      *  MVC   FWTSBA,FOP1        MOVE WCC,SBA
      *  MVC   FOPRECNO,=C'  3'   SET RECORD NUMBER IN MESSAGE
      *  MVC   FWTMSG,FOPTYPE     MOVE MESSAGE TEXT
            MOVE FOP1  TO FOPSBA.
            MOVE '  3' TO FOPRECNO.
            EXEC CICS SEND FROM(FOPLINE) LENGTH(FOPL) END-EXEC.

      *  MVC   MYRID,=F'2'        RECORD 2
      *  MVC   MYLEN,=H'30'       SET MAXIMUM LENGTH
            MOVE 2  TO MYRID.
            MOVE 30 TO MYLEN.
            EXEC CICS READ FILE('MYFILE06') INTO(MYDATA) LENGTH(MYLEN)
                      RIDFLD(MYRID) RRN END-EXEC.
      *  BAL   R9,WQTS            WRITE IT TO TS Q
            PERFORM WQTS.
      *  MVC   FWTSBA,FOP2        MOVE WCC,SBA
      *  MVC   FOPRECNO,=C'  2'   SET RECORD NUMBER IN MESSAGE
      *  MVC   FWTMSG,FOPTYPE     MOVE MESSAGE TEXT
            MOVE FOP2  TO FOPSBA.
            MOVE '  2' TO FOPRECNO.
            EXEC CICS SEND FROM(FOPLINE) LENGTH(FOPL) END-EXEC.

      *  MVC   MYRID,=F'1'        RECORD 1
      *  MVC   MYLEN,=H'30'       SET MAXIMUM LENGTH
            MOVE 1  TO MYRID.
            MOVE 30 TO MYLEN.
            EXEC CICS READ FILE('MYFILE06') INTO(MYDATA) LENGTH(MYLEN)
                      RIDFLD(MYRID) RRN END-EXEC.
      *  BAL   R9,WQTS            WRITE IT TO TS Q
            PERFORM WQTS.
      *  MVC   FWTSBA,FOP3        MOVE WCC,SBA
      *  MVC   FOPRECNO,=C'  1'   SET RECORD NUMBER IN MESSAGE
      *  MVC   FWTMSG,FOPTYPE     MOVE MESSAGE TEXT
            MOVE FOP3  TO FOPSBA.
            MOVE '  1' TO FOPRECNO.
            EXEC CICS SEND FROM(FOPLINE) LENGTH(FOPL) END-EXEC.

      *  MVC   FOPFIXVR,=C'VAR'   SET FIX/VAR IN MESSAGE
      *  MVC   FOPNAME,=C'MYFILE07' SET FILENAME IN MESSAGE
      *  MVC   MYRID,=F'5'        RRN 5  RECORD 1
      *  MVC   MYLEN,=H'50'       SET MAXIMUM LENGTH
            MOVE 'VAR'      TO FOPFIXVR.
            MOVE 'MYFILE07' TO FOPNAME.
            MOVE 5          TO MYRID.
            MOVE 50         TO MYLEN.
            EXEC CICS READ FILE('MYFILE07') INTO(MYDATA) LENGTH(MYLEN)
                      RIDFLD(MYRID) RRN END-EXEC.
      *  BAL   R9,WQTS            WRITE IT TO TS Q
            PERFORM WQTS.
      *  MVC   FWTSBA,FOP4        MOVE WCC,SBA
      *  MVC   FOPRECNO,=C'  5'   SET RECORD NUMBER IN MESSAGE
      *  MVC   FWTMSG,FOPTYPE     MOVE MESSAGE TEXT
            MOVE FOP4  TO FOPSBA.
            MOVE '  5' TO FOPRECNO.
            EXEC CICS SEND FROM(FOPLINE) LENGTH(FOPL) END-EXEC.

      *  MVC   MYRID,=F'10'       RRN 10  RECORD 3
      *  MVC   MYLEN,=H'50'       SET MAXIMUM LENGTH
            MOVE 10 TO MYRID.
            MOVE 50 TO MYLEN.
            EXEC CICS READ FILE('MYFILE07') INTO(MYDATA) LENGTH(MYLEN)
                      RIDFLD(MYRID) RRN END-EXEC.
      *  BAL   R9,WQTS            WRITE IT TO TS Q
            PERFORM WQTS.
      *  MVC   FWTSBA,FOP5        MOVE WCC,SBA
      *  MVC   FOPRECNO,=C' 10'   SET RECORD NUMBER IN MESSAGE
      *  MVC   FWTMSG,FOPTYPE     MOVE MESSAGE TEXT
            MOVE FOP5  TO FOPSBA.
            MOVE ' 10' TO FOPRECNO.
            EXEC CICS SEND FROM(FOPLINE) LENGTH(FOPL) END-EXEC.
      *
      * RAISE SOME CONDITIONS, WE MAY NOT DO ALL THESE FOR EVERY
      * FUNCTION
      *
            EXEC CICS HANDLE CONDITION
                 NOTFND(VSAMNOTF)
                 LENGERR(VSAMLNG)
                 ENDFILE(VSAMENDF)
                 ILLOGIC(VSAMILL)
                 END-EXEC.

      * NOTFND
      *  MVC   FWTSBA,FOP6        MOVE WCC,SBA
      *  MVC   FOPNAME,=CL8'MYFILE06' SET FILENAME IN MESSAGE
      *  MVC   FOPRECNO,=C'  4'   SET RECORD NUMBER IN MESSAGE
      *  MVC   FWTMSG,FOPTYPE     MOVE MESSAGE TEXT
            MOVE FOP6       TO FOPSBA.
            MOVE 'MYFILE06' TO FOPNAME.
            MOVE '  4'      TO FOPRECNO.
            EXEC CICS SEND FROM(FOPLINE) LENGTH(FOPL) END-EXEC.
      *  MVC   MYRID,=F'4'        RECORD 4  EMPTY RECORD
      *  MVC   MYLEN,=H'30'       SET MAXIMUM LENGTH
            MOVE 4  TO MYRID.
            MOVE 30 TO MYLEN.
      *  LA    R7,VSAMRTN1        R7=HANDLE RETURN
            MOVE 1    TO DEPCTRL.
            EXEC CICS READ FILE('MYFILE06') INTO(MYDATA) LENGTH(MYLEN)
                      RIDFLD(MYRID) RRN END-EXEC.

      * VSAMRTN1 DS    0H
        VSAMRTN1.
      * ILLOGIC
      *  MVC   FWTSBA,FOP8        MOVE WCC,SBA
      *  MVC   FOPRECNO,=C'  2'   SET RECORD NUMBER IN MESSAGE
      *  MVC   FOPMETH,=CL4'RBA'  SET METHOD IN MESSAGE
      *  MVC   FWTMSG,FOPTYPE     MOVE MESSAGE TEXT
            MOVE FOP8   TO FOPSBA.
            MOVE '  2'  TO FOPRECNO.
            MOVE 'RBA ' TO FOPMETH.
            EXEC CICS SEND FROM(FOPLINE) LENGTH(FOPL) END-EXEC.
      *  MVC   MYRID,=F'2'        RECORD 2
      *  MVC   MYLEN,=H'30'       SET MAXIMUM LENGTH
            MOVE 2  TO MYRID.
            MOVE 30 TO MYLEN.
      *  LA    R7,VSAMRTN2        R7=HANDLE RETURN
            MOVE 2 TO DEPCTRL.
            EXEC CICS READ FILE('MYFILE06') INTO(MYDATA) LENGTH(MYLEN)
                      RIDFLD(MYRID) RBA END-EXEC.

      * VSAMRTN2 DS    0H
        VSAMRTN2.
      * LENGERR/11
      *  MVC   FWTSBA,FOP10       MOVE WCC,SBA
      *  MVC   FOPNAME,=CL8'MYFILE07' SET FILENAME IN MESSAGE
      *  MVC   FOPRECNO,=C'  8'   SET RECORD NUMBER IN MESSAGE
      *  MVC   FOPMETH,=CL4'RRN'  SET METHOD IN MESSAGE
      *  MVC   FWTMSG,FOPTYPE     MOVE MESSAGE TEXT
            MOVE FOP10      TO FOPSBA.
            MOVE 'MYFILE07' TO FOPNAME.
            MOVE '  8'      TO FOPRECNO.
            MOVE 'RRN ' TO FOPMETH.
            EXEC CICS SEND FROM(FOPLINE) LENGTH(FOPL) END-EXEC.
      *  MVC   MYRID,=F'8'        RECORD 8
      *  MVC   MYLEN,=H'6'        LENGTH TOO SHORT
            MOVE 8 TO MYRID.
            MOVE 6 TO MYLEN.
      *  LA    R7,VSAMRTN3        R7=HANDLE RETURN
            MOVE 3 TO DEPCTRL.
            EXEC CICS READ FILE('MYFILE07') INTO(MYDATA) LENGTH(MYLEN)
                      RIDFLD(MYRID) RRN END-EXEC.

      * VSAMRTN3 DS    0H
        VSAMRTN3.
      *  MVC   MYLEN,=H'6'        LENGTH REQUESTED
            MOVE 6 TO MYLEN.
      *  BAL   R9,WQTS            WRITE IT TO TS Q
            PERFORM WQTS.
      *  SR    R7,R7              NO ERROR RETURN
            MOVE 6 TO DEPCTRL.
      * START BROWSE, POINT AT EMPTY SLOT 7
      *  MVC   FWTSBA,FOP12       MOVE WCC,SBA
      *  MVC   FOPRECNO,=C'  7'   SET RECORD NUMBER IN MESSAGE
      *  MVC   FOPFUNC,=CL8'STARTBR' SET FUNCTION IN MESSAGE
      *  MVC   FWTMSG,FOPTYPE     MOVE MESSAGE TEXT
            MOVE FOP12      TO FOPSBA.
            MOVE '  7'      TO FOPRECNO.
            MOVE 'STARTBR ' TO FOPFUNC.
            EXEC CICS SEND FROM(FOPLINE) LENGTH(FOPL) END-EXEC.
      *  MVC   MYRID,=F'7'        RECORD 7 (EMPTY SLOT)
            MOVE 7 TO MYRID.
            EXEC CICS STARTBR FILE('MYFILE07') RIDFLD(MYRID) RRN
                      END-EXEC.

      * READNEXT 3 RECORDS
      *  MVC   FWTSBA,FOP13       MOVE WCC,SBA
      *  MVC   FOPRECNO,=C'S  '   SET RECORD NUMBERS IN MESSAGE
      *  MVC   FOPFUNC,=CL8'READNEXT' SET FUNCTION IN MESSAGE
      *  MVC   FWTMSG,FOPTYPE     MOVE MESSAGE TEXT
            MOVE FOP13      TO FOPSBA.
            MOVE 'S  '      TO FOPRECNO.
            MOVE 'READNEXT' TO FOPFUNC.
            EXEC CICS SEND FROM(FOPLINE) LENGTH(FOPL) END-EXEC.
      *  LA    R8,3               READ 3 FORWARDS
            MOVE 1 TO LOOPCTRL.
      * RDNXLOOP EQU   *
        RDNXLOOP.
      *  MVC   MYLEN,=H'50'       SET MAXIMUM LENGTH
            MOVE 50 TO MYLEN.
            EXEC CICS READNEXT FILE('MYFILE07') INTO(MYDATA)
                      LENGTH(MYLEN) RIDFLD(MYRID) RRN END-EXEC.
      *  BAL   R9,WQTS            WRITE IT TO TS Q
            PERFORM WQTS.
      *  BCT   R8,RDNXLOOP        LOOP
            ADD 1 TO LOOPCTRL.
            IF LOOPCTRL NOT = 4 THEN GO TO RDNXLOOP.
      * READPREV 3 RECORDS
      *  MVC   FWTSBA,FOP14       MOVE WCC,SBA
      *  MVC   FOPRECNO,=C'S  '   SET RECORD NUMBERS IN MESSAGE
      *  MVC   FOPFUNC,=CL8'READPREV' SET FUNCTION IN MESSAGE
      *  MVC   FWTMSG,FOPTYPE     MOVE MESSAGE TEXT
            MOVE FOP14      TO FOPSBA.
            MOVE 'S  '      TO FOPRECNO.
            MOVE 'READPREV' TO FOPFUNC.
            EXEC CICS SEND FROM(FOPLINE) LENGTH(FOPL) END-EXEC.
      *  LA    R8,3               READ 3 BACKWARDS
            MOVE 1 TO LOOPCTRL.
      * RDPVLOOP EQU   *
        RDPVLOOP.
      *  MVC   MYLEN,=H'50'       SET MAXIMUM LENGTH
            MOVE 50 TO MYLEN.
            EXEC CICS READPREV FILE('MYFILE07') INTO(MYDATA)
                      LENGTH(MYLEN) RIDFLD(MYRID) RRN END-EXEC.
      *  BAL   R9,WQTS            WRITE IT TO TS Q
            PERFORM WQTS.
      *  BCT   R8,RDPVLOOP        LOOP
            ADD 1 TO LOOPCTRL.
            IF LOOPCTRL NOT = 4 THEN GO TO RDPVLOOP.
      * READ RANDOM 3 RECORDS
      *  MVC   FWTSBA,FOP15       MOVE WCC,SBA
      *  MVC   FOPRECNO,=C'S  '   SET RECORD NUMBERS IN MESSAGE
      *  MVC   FOPFUNC,=CL8'READ RND' SET FUNCTION IN MESSAGE
      *  MVC   FWTMSG,FOPTYPE     MOVE MESSAGE TEXT
            MOVE FOP15      TO FOPSBA.
            MOVE 'S  '      TO FOPRECNO.
            MOVE 'READ RND' TO FOPFUNC.
            EXEC CICS SEND FROM(FOPLINE) LENGTH(FOPL) END-EXEC.
      *  MVC   MYLEN,=H'50'       SET MAXIMUM LENGTH
      *  MVC   MYRID,=F'17'       RRN 17
            MOVE 50 TO MYLEN.
            MOVE 17 TO MYRID.
      * POINT AT RRN 17, READ RECORD 7
            EXEC CICS READNEXT FILE('MYFILE07') INTO(MYDATA)
                      LENGTH(MYLEN) RIDFLD(MYRID) RRN END-EXEC.
      *  BAL   R9,WQTS            WRITE IT TO TS Q
            PERFORM WQTS.
      *  MVC   MYLEN,=H'50'       SET MAXIMUM LENGTH
      *  MVC   MYRID,=F'22'       RRN 22
            MOVE 50 TO MYLEN.
            MOVE 22 TO MYRID.
      * POINT AT RRN 22, READ RECORD 6
            EXEC CICS READPREV FILE('MYFILE07') INTO(MYDATA)
                      LENGTH(MYLEN) RIDFLD(MYRID) RRN END-EXEC.
      *  BAL   R9,WQTS            WRITE IT TO TS Q
            PERFORM WQTS.
      *  MVC   MYLEN,=H'50'       SET MAXIMUM LENGTH
      *  MVC   MYRID,=F'1'        RRN 1
            MOVE 50 TO MYLEN.
            MOVE 1  TO MYRID.
      * POINT AT RRN 1, READ RECORD 1
            EXEC CICS READNEXT FILE('MYFILE07') INTO(MYDATA)
                      LENGTH(MYLEN) RIDFLD(MYRID) RRN END-EXEC.
      *  BAL   R9,WQTS            WRITE IT TO TS Q
            PERFORM WQTS.
      * END BROWSE
      *  MVC   FWTSBA,FOP16       MOVE WCC,SBA
      *  MVC   FOPRECNO,=C'   '   SET RECORD NUMBERS IN MESSAGE
      *  MVC   FOPFUNC,=CL8'ENDBR' SET FUNCTION IN MESSAGE
      *  MVC   FWTMSG,FOPTYPE     MOVE MESSAGE TEXT
            MOVE FOP16      TO FOPSBA.
            MOVE SPACES     TO FOPRECNO.
            MOVE 'ENDBR   ' TO FOPFUNC.
            EXEC CICS SEND FROM(FOPLINE) LENGTH(FOPL) END-EXEC.
            EXEC CICS ENDBR FILE('MYFILE07') END-EXEC.
      * ENDFILE 1 RAISED BY READING FORWARD FROM END
      *  MVC   FWTSBA,FOP17       MOVE WCC,SBA
      *  MVC   FOPNAME,=CL8'MYFILE06' SET FILENAME IN MESSAGE
      *  MVC   MYLEN,=H'50'       SET MAXIMUM LENGTH
      *  MVC   MYRID,=F'99'       RRN 99   RRN BEYOND END
      *  MVC   FOPRECNO,=C' 99'   SET RECORD NUMBER IN MESSAGE
      *  MVC   FOPFUNC,=CL8'READNEXT' SET FUNCTION IN MESSAGE
      *  MVC   FWTMSG,FOPTYPE     MOVE MESSAGE TEXT
            MOVE FOP17      TO FOPSBA.
            MOVE 'MYFILE06' TO FOPNAME.
            MOVE 50         TO MYLEN.
            MOVE 99         TO MYRID.
            MOVE ' 99'      TO FOPRECNO.
            MOVE 'READNEXT' TO FOPFUNC.
            EXEC CICS SEND FROM(FOPLINE) LENGTH(FOPL) END-EXEC.
      *  MVI   ENDFLENO,C'1'      SET 1ST ENDFILE
            MOVE 1 TO ENDFLENO.
      *  LA    R7,VSAMRTN4        R7=HANDLE RETURN
            MOVE 4 TO DEPCTRL.
            EXEC CICS STARTBR FILE('MYFILE06') RIDFLD(MYRID) RRN
                      END-EXEC.
            EXEC CICS READNEXT FILE('MYFILE06') INTO(MYDATA)
                      LENGTH(MYLEN) RIDFLD(MYRID) RRN END-EXEC.

      * VSAMRTN4 DS    0H
        VSAMRTN4.
      * ENDFILE 2 RAISED BY READING BACK FROM START
      *  MVC   FWTSBA,FOP19       MOVE WCC,SBA
      *  MVC   MYLEN,=H'50'       SET MAXIMUM LENGTH
      *  MVC   MYRID,=F'1'        RRN 1  FIRST RECORD
      *  MVC   FOPRECNO,=C'  1'   SET RECORD NUMBER IN MESSAGE
      *  MVC   FOPFUNC,=CL8'READPREV' SET FUNCTION IN MESSAGE
      *  MVC   FWTMSG,FOPTYPE     MOVE MESSAGE TEXT
            MOVE FOP19      TO FOPSBA.
            MOVE 50         TO MYLEN.
            MOVE 1          TO MYRID.
            MOVE '  1'      TO FOPRECNO.
            MOVE 'READPREV' TO FOPFUNC.
            EXEC CICS SEND FROM(FOPLINE) LENGTH(FOPL) END-EXEC.
      *  MVI   ENDFLENO,C'2'      SET 2ND ENDFILE
            MOVE 2 TO ENDFLENO.
      *  LA    R7,VSAMRTN5        R7=HANDLE RETURN
            MOVE 5 TO DEPCTRL.
            EXEC CICS READPREV FILE('MYFILE06') INTO(MYDATA)
                      LENGTH(MYLEN) RIDFLD(MYRID) RRN END-EXEC.

      * VSAMRTN5 DS    0H
        VSAMRTN5.
            EXEC CICS ENDBR FILE('MYFILE06') END-EXEC.

      * COMPLEX TEST, 2 OVERLAPPING BROWSES OF SAME FILE
      *  MVC   FWTSBA,FOP21       MOVE WCC,SBA
      *  MVC   FOPNAME,=CL8'COMPLEX' SET FILENAME IN MESSAGE
      *  MVC   FOPRECNO,=C'S  '   SET RECORD NUMBERS IN MESSAGE
      *  MVC   FOPFUNC,=CL8'COMPLEX' SET FUNCTION IN MESSAGE
      *  MVC   FWTMSG,FOPTYPE     MOVE MESSAGE TEXT
            MOVE FOP21      TO FOPSBA.
            MOVE 'COMPLEX ' TO FOPNAME.
            MOVE 'S  '      TO FOPRECNO.
            MOVE 'COMPLEX ' TO FOPFUNC.
            EXEC CICS SEND FROM(FOPLINE) LENGTH(FOPL) END-EXEC.
      *  MVC   MYRID1,=F'6'       RRN 6  RECORD 5
            MOVE 6 TO MYRID1.
            EXEC CICS STARTBR FILE('MYFILE06') RIDFLD(MYRID1) RRN
                      REQID(1) END-EXEC.
      *  MVC   MYRID2,=F'10'      RRN 10 RECORD 8(BACK TO 7)
            MOVE 10 TO MYRID2.
            EXEC CICS STARTBR FILE('MYFILE06') RIDFLD(MYRID2) RRN
                      REQID(2) END-EXEC.
      *  LA    R8,3               R8=LOOP LIMIT
            MOVE 1 TO LOOPCTRL.
      * PLEXLOOP EQU   *
        PLEXLOOP.
      *  MVC   MYLEN,=H'50'       SET MAXIMUM LENGTH
            MOVE 50 TO MYLEN.
            EXEC CICS READNEXT FILE('MYFILE06') INTO(MYDATA)
                      LENGTH(MYLEN) RIDFLD(MYRID1) RRN REQID(1)
                      END-EXEC.
      *  BAL   R9,WQTS            WRITE IT TO TS Q
            PERFORM WQTS.
      *  MVC   MYLEN,=H'50'       SET MAXIMUM LENGTH
            MOVE 50 TO MYLEN.
            EXEC CICS READPREV FILE('MYFILE06') INTO(MYDATA)
                      LENGTH(MYLEN) RIDFLD(MYRID2) RRN REQID(2)
                      END-EXEC.
      *  BAL   R9,WQTS            WRITE IT TO TS Q
            PERFORM WQTS.
      *  BCT   R8,PLEXLOOP        LOOP
            ADD 1 TO LOOPCTRL.
            IF LOOPCTRL NOT = 4 THEN GO TO PLEXLOOP.
            EXEC CICS ENDBR FILE('MYFILE06') REQID(1) END-EXEC.
            EXEC CICS ENDBR FILE('MYFILE06') REQID(2) END-EXEC.

      * MYFILE07, RRN 10 (RECS 3-5), RESETBR, RRN 99 (RECS 10-8)
      *  MVC   FWTSBA,FOP1C2      MOVE WCC,SBA
      *  MVI   FOPSEP+1,C' '      RESET SEPARATOR
      *  LH    R7,FOPL            MESSAGE LENGTH
      *  AHI   R7,-1              SHORTEN BY 1
      *  STH   R7,FOPL            STORE BACK
      *  MVC   FOPNAME,=CL8'MYFILE07' SET FILENAME IN MESSAGE
      *  MVC   FOPRECNO,=C'S  '   SET RECORD NUMBERS IN MESSAGE
      *  MVC   FOPFUNC,=CL8'RESETBR' SET FUNCTION IN MESSAGE
      *  MVC   FWTMSG,FOPTYPE     MOVE MESSAGE TEXT
            MOVE FOP1C2     TO FOPSBA.
            MOVE SPACE      TO FOPSEP
            SUBTRACT 1 FROM FOPL.
            MOVE 'MYFILE07' TO FOPNAME.
            MOVE 'S  '      TO FOPRECNO.
            MOVE 'RESETBR ' TO FOPFUNC.
            EXEC CICS SEND FROM(FOPLINE) LENGTH(FOPL) END-EXEC.
      *  MVC   MYRID1,=F'10'      RRN 3  RECORD 3
            MOVE 10 TO MYRID1.
            EXEC CICS STARTBR FILE('MYFILE07') RIDFLD(MYRID1) RRN
                      REQID(1) END-EXEC.
      *  LA    R8,3               R8=LOOP LIMIT
            MOVE 1 TO LOOPCTRL.
      * RSBRLP1  EQU   *
        RSBRLP1.
      *  MVC   MYLEN,=H'50'       SET MAXIMUM LENGTH
            MOVE 50 TO MYLEN.
            EXEC CICS READNEXT FILE('MYFILE07') INTO(MYDATA)
                      LENGTH(MYLEN) RIDFLD(MYRID1) RRN REQID(1)
                      END-EXEC.
      *  BAL   R9,WQTS            WRITE IT TO TS Q
            PERFORM WQTS.
      *  BCT   R8,RSBRLP1         LOOP
            ADD 1 TO LOOPCTRL.
            IF LOOPCTRL NOT = 4 THEN GO TO RSBRLP1.
      *  MVC   MYRID1,=F'99'      RRN 99  PAST END
            MOVE 99 TO MYRID1.
            EXEC CICS RESETBR FILE('MYFILE07') RIDFLD(MYRID1) RRN
                      REQID(1) END-EXEC.
      *  LA    R8,3               R8=LOOP LIMIT
            MOVE 1 TO LOOPCTRL.
      * RSBRLP2  EQU   *
        RSBRLP2.
      *  MVC   MYLEN,=H'50'       SET MAXIMUM LENGTH
            MOVE 50 TO MYLEN.
            EXEC CICS READPREV FILE('MYFILE07') INTO(MYDATA)
                      LENGTH(MYLEN) RIDFLD(MYRID1) RRN REQID(1)
                      END-EXEC.
      *  BAL   R9,WQTS            WRITE IT TO TS Q
            PERFORM WQTS.
      *  BCT   R8,RSBRLP2         LOOP
            ADD 1 TO LOOPCTRL.
            IF LOOPCTRL NOT = 4 THEN GO TO RSBRLP2.
            EXEC CICS ENDBR FILE('MYFILE07') REQID(1) END-EXEC.
            EXEC CICS SEND FROM(ENDMSG) LENGTH(ENDMSGL) END-EXEC.
            EXEC CICS RETURN END-EXEC.
      *
      * HANDLE ROUTINES
      *
      * VSAMNOTF EQU   *
        VSAMNOTF.
      *  MVC   FWTSBA,FOP7        MOVE WCC,SBA
      *  MVC   FHANDCON,=CL12'NOTFND' MOVE CONDITION
            MOVE FOP7           TO FHASBA.
            MOVE 'NOTFND      ' TO FHANDCON.
      *  BAL   R9,HEXFDBK         DISPLAY FEEDBACK CODES
            PERFORM HEXFDBK.
      *  B     FWTHSEND           EXIT
            GO TO FWTHSEND.

      * VSAMILL  DS    0H
        VSAMILL.
      *  MVC   FWTSBA,FOP9        MOVE WCC,SBA
            MOVE FOP9 TO FHASBA.
      *  L     R8,EIBRESP2        R8=RESP2
      *  CVD   R8,DEC             CONVERT TO DECIMAL
      *  XC    FHANDCON,FHANDCON  CLEAR CONDITION
      *  MVC   FHANDCON+7(4),=X'40202020' MOVE EDIT WORD
      *  ED    FHANDCON+7(4),DEC+6 EDIT RESP2
      *  MVC   FHANDCON(8),=C'ILLOGIC/' MOVE CONDITION
            MOVE 'ILLOGIC/    ' TO FHANDCON.
            MOVE EIBRESP2 TO WORK1A.
            MOVE WORK1C TO FHAILL.
      *  BAL   R9,HEXFDBK         DISPLAY FEEDBACK CODES
            PERFORM HEXFDBK.
      *  B     FWTHSEND           EXIT
            GO TO FWTHSEND.

      * VSAMLNG  DS    0H
        VSAMLNG.
      *  MVC   FWTSBA,FOP11       MOVE WCC,SBA
            MOVE FOP11 TO FHASBA.
      *  L     R8,EIBRESP2        R8=RESP2
      *  CVD   R8,DEC             CONVERT TO DECIMAL
      *  XC    FHANDCON,FHANDCON  CLEAR CONDITION
      *  MVC   FHANDCON+6(4),=X'40202020' MOVE EDIT WORD
      *  ED    FHANDCON+6(4),DEC+6 EDIT RESP2
      *  MVC   FHANDCON(8),=C'LENGERR/' MOVE CONDITION
            MOVE 'LENGERR/    ' TO FHANDCON.
            MOVE EIBRESP2 TO WORK1A.
            MOVE WORK1B TO FHALNG.
      *  BAL   R9,HEXFDBK         DISPLAY FEEDBACK CODES
            PERFORM HEXFDBK.
      *  B     FWTHSEND           EXIT
            GO TO FWTHSEND.

      * VSAMENDF DS    0H
        VSAMENDF.
      *  CLI   ENDFLENO,C'1'      1ST ?
      *  BE    ENDFLEGO           EXIT IF IT IS
            GO TO ENDF1 ENDF2 DEPENDING ON ENDFLENO.
            GO TO ABEND.
        ENDF1.
      *  MVC   FWTSBA,FOP18       MOVE WCC,SBA
            MOVE FOP18 TO FHASBA.
            GO TO ENDFEND.
        ENDF2.
      *  MVC   FWTSBA,FOP20       MOVE WCC,SBA
            MOVE FOP20 TO FHASBA.
      * ENDFLEGO EQU   *
        ENDFEND.
      *  MVC   FHANDCON,=CL12'ENDFILE' MOVE CONDITION
            MOVE 'ENDFILE     ' TO FHANDCON.
      *  BAL   R9,HEXFDBK         DISPLAY FEEDBACK CODES
            PERFORM HEXFDBK.

      * FWTHSEND EQU   *
        FWTHSEND.
      *  MVC   FWTMSG,FHANDCON    MOVE MESSAGE TEXT
            EXEC CICS SEND FROM(FHAND) LENGTH(FHANDL) END-EXEC.
      *  BR    R7                 RETURN TO MAIN LINE
            GO TO VSAMRTN1 VSAMRTN2 VSAMRTN3 VSAMRTN4 VSAMRTN5 ABEND
                  DEPENDING ON DEPCTRL.

        ABEND.
            EXEC CICS ABEND ABCODE('VSC2') END-EXEC.

      *
      * SUBROUTINES
      *
      * WQTS     DS    0H
        WQTS.
            EXEC CICS WRITEQ TS QUEUE('VSC2') FROM(MYDATA)
                      LENGTH(MYLEN) END-EXEC.
      *  BR    R9                 RETURN

      * HEXFDBK  DS    0H
        HEXFDBK.
      *  UNPK  WORK(5),EIBRCODE+1(3) UNPACK VSAM FEEDBACK CODES
      *  TR    WORK(4),TABLE-240  TRANSLATE TO PRINTABLE HEX
      *  MVC   FHFDBK1,WORK       MOVE FDBK RETURN
      *  MVC   FHFDBK2,WORK+2     MOVE FDBK CODE
            MOVE EIBRCODE TO WORK2.
            IF FDBK1 = X'00' THEN MOVE '00' TO FHFDBK1.
            IF FDBK1 = X'08' THEN MOVE '08' TO FHFDBK1.
            IF FDBK2 = X'00' THEN MOVE '00' TO FHFDBK2.
            IF FDBK2 = X'04' THEN MOVE '04' TO FHFDBK2.
            IF FDBK2 = X'10' THEN MOVE '10' TO FHFDBK2.
            IF FDBK2 = X'68' THEN MOVE '68' TO FHFDBK2.
      *  BR    R9                 RETURN
