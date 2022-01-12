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
      * TEST VSAM 1 (COBOL)
      *
      * ESDS/VAR MYFILE01 READ RBA  REC  3
      * ESDS/VAR MYFILE01 READ RBA  REC  2
      * ESDS/VAR MYFILE01 READ XRBA REC  1
      * DSIDERR      CONDITION RAISED (FILENOTFOUND)
      * DISABLED     CONDITION RAISED
      * NOTOPEN      CONDITION RAISED
      * NOTFND       CONDITION RAISED (READ INVALID RBA)
      * LENGERR/11   CONDITION RAISED
      * INVREQ/20    CONDITION RAISED (OPEN MYFILE02, READ NOT ALLOWED)
      * ILLOGIC/110  CONDITION RAISED (MYFILE01, REC 1, RRN)
      * ESDS/VAR MYFILE01 BROWSE FORWARD RECS 15-17
      * ESDS/VAR MYFILE01 BROWSE BACK    RECS 17-15
      * ESDS/VAR MYFILE01 BROWSE RANDOM  RECS 26,14(PREV=13),18
      * NOTFND       CONDITION RAISED (READNEXT INVALID RBA)
      * ENDFILE      CONDITION RAISED (READNEXT RBA AT END)
      * ENDFILE      CONDITION RAISED (READPREV RBA AT START)
      * ESDS COMPLEX TEST...DONE IN THIS SEQUENCE
      *    BROWSE FORWARD MYFILE01 RECS 7-10
      *    BROWSE BACK    MYFILE01 RECS 10-7
      *    READ SEQ       MYFILE05 1ST 4 RECS
      * ESDS/FIX MYFILE05, READ RECS 2-4, RESETBR, READ RECS 8-6

        PROGRAM-ID. TESTVSC1.
        ENVIRONMENT DIVISION.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        77  MYDATA   PIC X(50).
        77  MYLEN    PIC 9(4)  COMP.
        77  MYRID    PIC 9(8)  COMP.
        77  MYRID1   PIC 9(8)  COMP.
        77  MYRID2   PIC 9(9)  COMP.
        77  MYRID8   PIC 9(18) COMP.
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
        77  FOP2C2   PIC X(4)   VALUE X'C311C4D9'.
        77  FOP3C2   PIC X(4)   VALUE X'C311C5E9'.
        77  FOP4C2   PIC X(4)   VALUE X'C311C6F9'.
        77  FOP5C2   PIC X(4)   VALUE X'C311C8C9'.
        77  FOP6C2   PIC X(4)   VALUE X'C311C9D9'.
        77  FOP7C2   PIC X(4)   VALUE X'C3114AE9'.
        77  FOP8C2   PIC X(4)   VALUE X'C3114BF9'.
        77  FOP9C2   PIC X(4)   VALUE X'C3114DC9'.

      * ESDS MYFILE01 READ   RBA REC  3
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
                03  FILLER   PIC X(7) VALUE SPACES.
                03  FHAINV   PIC X(2).
                03  FILLER   PIC X(3) VALUE SPACES.
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
                         VALUE 'TESTVSC1 COMPLETED, PRESS CLEAR'.
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
      *   6=VSAMRTN6
      *   7=VSAMRTN7
      *   8=VSAMRTN8
      *   9=VSAMRTN9
      *  10=VSAMRTNA
        77  DEPCTRL   PIC 9(4) COMP.

        77  NOTFNDNO  PIC 9(4) COMP.
        77  ENDFLENO  PIC 9(4) COMP.

        77  LOOPCTRL  PIC 9(2) COMP.

        LINKAGE SECTION.
        77  MYDATALK PIC X(50).

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
      *  MVC   FOPTYPE,=C'ESDS'   SET FILE TYPE IN MESSAGE
      *  MVC   FOPFIXVR,=C'VAR'   SET FIX/VAR IN MESSAGE
      *  MVC   FOPNAME,=C'MYFILE01' SET FILENAME IN MESSAGE
      *  MVC   FOPFUNC,=CL8'READ' SET FUNCTION IN MESSAGE
      *  MVC   FOPMETH,=CL4'RBA'  SET METHOD IN MESSAGE
      *  MVC   MYRID,=F'19'       RIDFLD=19  RECORD 3
            MOVE 'ESDS'     TO FOPTYPE.
            MOVE 'VAR'      TO FOPFIXVR.
            MOVE 'MYFILE01' TO FOPNAME.
            MOVE 'READ    ' TO FOPFUNC.
            MOVE 'RBA '     TO FOPMETH.
            MOVE 19         TO MYRID.
            MOVE 50         TO MYLEN.
      *  BAL   R9,MY01            READ MYFILE01 RBA
            PERFORM MY01.
      *  BAL   R9,WQTS            WRITE IT TO TS Q
            PERFORM WQTS.
      *  MVC   FWTSBA,FOP1        MOVE WCC,SBA
      *  MVC   FOPRECNO,=C'  3'   SET RECORD NUMBER IN MESSAGE
      *  MVC   FWTMSG,FOPTYPE     MOVE MESSAGE TEXT
            MOVE FOP1  TO FOPSBA.
            MOVE '  3' TO FOPRECNO.
            EXEC CICS SEND FROM(FOPLINE) LENGTH(FOPL) END-EXEC.

      *  MVC   MYRID,=F'9'        RIDFLD=9   RECORD 2
            MOVE 9  TO MYRID.
            MOVE 50 TO MYLEN.
      *  BAL   R9,MY01            READ MYFILE01 RBA
            PERFORM MY01.
      *  BAL   R9,WQTS            WRITE IT TO TS Q
            PERFORM WQTS.
      *  MVC   FWTSBA,FOP2        MOVE WCC,SBA
      *  MVC   FOPRECNO,=C'  2'   SET RECORD NUMBER IN MESSAGE
      *  MVC   FWTMSG,FOPTYPE     MOVE MESSAGE TEXT
            MOVE FOP2  TO FOPSBA.
            MOVE '  2' TO FOPRECNO.
            EXEC CICS SEND FROM(FOPLINE) LENGTH(FOPL) END-EXEC.

      * MAKE SURE SET WORKS
      *  MVC   MYRID8,=FL8'0'     RIDFLD=0  RECORD 1
      *  MVC   MYLEN,=H'50'       SET MAXIMUM LENGTH
            MOVE ZERO  TO MYRID8.
            MOVE 50    TO MYLEN.
            EXEC CICS READ FILE('MYFILE01') SET(ADDRESS OF MYDATALK)
                      LENGTH(MYLEN) RIDFLD(MYRID8) XRBA END-EXEC.
      *  MVC   MYRID8,=FL8'0'     RIDFLD=0  RECORD 1
      *  MVC   MYLEN,=H'50'       SET MAXIMUM LENGTH
            MOVE ZERO  TO MYRID8.
            MOVE 50    TO MYLEN.
            EXEC CICS READ FILE('MYFILE01') INTO(MYDATA) LENGTH(MYLEN)
                      RIDFLD(MYRID8) XRBA END-EXEC.
      *  BAL   R9,WQTS            WRITE IT TO TS Q
            PERFORM WQTS.
      *  MVC   FWTSBA,FOP3        MOVE WCC,SBA
      *  MVC   FOPMETH,=CL4'XRBA' SET METHOD IN MESSAGE
      *  MVC   FOPRECNO,=C'  1'   SET RECORD NUMBER IN MESSAGE
      *  MVC   FWTMSG,FOPTYPE     MOVE MESSAGE TEXT
            MOVE FOP3   TO FOPSBA.
            MOVE 'XRBA' TO FOPMETH.
            MOVE '  1'  TO FOPRECNO.
            EXEC CICS SEND FROM(FOPLINE) LENGTH(FOPL) END-EXEC.

      *
      * RAISE SOME CONDITIONS, WE MAY NOT DO ALL THESE FOR EVERY
      * FUNCTION
      *
            EXEC CICS HANDLE CONDITION
                 DSIDERR(VSAMFNF)
                 DISABLED(VSAMDIS)
                 NOTFND(VSAMNOTF)
                 LENGERR(VSAMLNG)
                 NOTOPEN(VSAMNOTO)
                 ENDFILE(VSAMENDF)
                 INVREQ(VSAMINV)
                 ILLOGIC(VSAMILL)
                 END-EXEC.

      * FILENOTFOUND=DSIDERR
      *  MVC   FWTSBA,FOP4        MOVE WCC,SBA
      *  MVC   FOPNAME,=CL8'FRED' SET FILENAME IN MESSAGE
      *  MVC   FOPRECNO,=C'  1'   SET RECORD NUMBER IN MESSAGE
      *  MVC   FWTMSG,FOPTYPE     MOVE MESSAGE TEXT
            MOVE FOP4       TO FOPSBA.
            MOVE 'FRED    ' TO FOPNAME.
            MOVE '  1'      TO FOPRECNO.
            EXEC CICS SEND FROM(FOPLINE) LENGTH(FOPL) END-EXEC.
      *  MVC   MYRID8,=FL8'0'     RIDFLD=0  RECORD 1
      *  MVC   MYLEN,=H'50'       SET MAXIMUM LENGTH
      *  LA    R7,VSAMRTN1        R7=HANDLE RETURN
            MOVE ZERO TO MYRID8.
            MOVE 50   TO MYLEN.
            MOVE 1    TO DEPCTRL.
            EXEC CICS READ FILE('FRED') INTO(MYDATA) LENGTH(MYLEN)
                      RIDFLD(MYRID8) XRBA END-EXEC.

      * VSAMRTN1 DS    0H
        VSAMRTN1.
      * DISABLED
      *  MVC   FWTSBA,FOP6        MOVE WCC,SBA
      *  MVC   FOPNAME,=CL8'MYFILE04' SET FILENAME IN MESSAGE
      *  MVC   FOPRECNO,=C'  1'   SET RECORD NUMBER IN MESSAGE
      *  MVC   FWTMSG,FOPTYPE     MOVE MESSAGE TEXT
            MOVE FOP6       TO FOPSBA.
            MOVE 'MYFILE04' TO FOPNAME.
            MOVE '  1'      TO FOPRECNO.
            EXEC CICS SEND FROM(FOPLINE) LENGTH(FOPL) END-EXEC.
      *  MVC   MYRID8,=FL8'0'     RIDFLD=0  RECORD 1
      *  MVC   MYLEN,=H'50'       SET MAXIMUM LENGTH
      *  LA    R7,VSAMRTN2        R7=HANDLE RETURN
            MOVE ZERO TO MYRID8.
            MOVE 50   TO MYLEN.
            MOVE 2    TO DEPCTRL.
            EXEC CICS READ FILE('MYFILE04') INTO(MYDATA) LENGTH(MYLEN)
                      RIDFLD(MYRID8) XRBA END-EXEC.

      * VSAMRTN2 DS    0H
        VSAMRTN2.
      * NOTOPEN
      *  MVC   FWTSBA,FOP8        MOVE WCC,SBA
      *  MVC   FOPNAME,=CL8'MYFILE03' SET FILENAME IN MESSAGE
      *  MVC   FOPRECNO,=C'  1'   SET RECORD NUMBER IN MESSAGE
      *  MVC   FWTMSG,FOPTYPE     MOVE MESSAGE TEXT
            MOVE FOP8       TO FOPSBA.
            MOVE 'MYFILE03' TO FOPNAME.
            MOVE '  1'      TO FOPRECNO.
            EXEC CICS SEND FROM(FOPLINE) LENGTH(FOPL) END-EXEC.
      *  MVC   MYRID8,=FL8'0'     RIDFLD=0  RECORD 1
      *  MVC   MYLEN,=H'50'       SET MAXIMUM LENGTH
            MOVE ZERO TO MYRID8.
            MOVE 50   TO MYLEN.
      *  LA    R7,VSAMRTN3        R7=HANDLE RETURN
            MOVE 3 TO DEPCTRL.
            EXEC CICS READ FILE('MYFILE03') INTO(MYDATA) LENGTH(MYLEN)
                      RIDFLD(MYRID8) XRBA END-EXEC.

      * VSAMRTN3 DS    0H
        VSAMRTN3.
      * NOTFND 1
      *  MVC   FWTSBA,FOP10       MOVE WCC,SBA
      *  MVC   FOPNAME,=CL8'MYFILE01' SET FILENAME IN MESSAGE
      *  MVC   FOPRECNO,=C'  ?'   SET RECORD NUMBER IN MESSAGE
      *  MVC   FOPMETH,=CL4'RBA'  SET METHOD IN MESSAGE
      *  MVC   FWTMSG,FOPTYPE     MOVE MESSAGE TEXT
            MOVE FOP10      TO FOPSBA.
            MOVE 'MYFILE01' TO FOPNAME.
            MOVE '  ?'      TO FOPRECNO.
            MOVE 'RBA '     TO FOPMETH.
            EXEC CICS SEND FROM(FOPLINE) LENGTH(FOPL) END-EXEC.
      *  MVC   MYRID,=F'-1'       RIDFLD=1  INVALID RBA
      *  LA    R7,VSAMRTN4        R7=HANDLE RETURN
      *  MVI   NOTFNDNO,C'1'      SET 1ST NOTFND
            MOVE -1 TO MYRID.
            MOVE 1  TO NOTFNDNO.
            MOVE 4  TO DEPCTRL.
            MOVE 50 TO MYLEN.
      *  BAL   R9,MY01            READ MYFILE01 RBA
            PERFORM MY01.

      * VSAMRTN4 DS    0H
        VSAMRTN4.
      * LENGERR/11
      *  MVC   FWTSBA,FOP12       MOVE WCC,SBA
      *  MVC   FOPRECNO,=C' 26'   SET RECORD NUMBER IN MESSAGE
      *  MVC   FWTMSG,FOPTYPE     MOVE MESSAGE TEXT
            MOVE FOP12 TO FOPSBA.
            MOVE ' 26' TO FOPRECNO.
            EXEC CICS SEND FROM(FOPLINE) LENGTH(FOPL) END-EXEC.
      *  MVC   MYRID,=XL4'20D'    RIDFLD=X'20D' RECORD 26
      *  LA    R7,VSAMRTN5        R7=HANDLE RETURN
      *  MVC   MYLEN,=H'20'       LENGTH TOO SHORT
            MOVE 525 TO MYRID.
            MOVE 20  TO MYLEN.
            MOVE 5   TO DEPCTRL.
      *  BAL   R9,MY01_02         READ MYFILE01 RBA NO LENGTH=50
            PERFORM MY01.

      * VSAMRTN5 DS    0H
        VSAMRTN5.
      *  MVC   MYLEN,=H'20'       LENGTH REQUESTED
            MOVE 20 TO MYLEN.
      *  BAL   R9,WQTS            WRITE IT TO TS Q
            PERFORM WQTS.
      * INVREQ/20
      *  MVC   FWTSBA,FOP14       MOVE WCC,SBA
      *  MVC   FOPNAME,=CL8'MYFILE02' SET FILENAME IN MESSAGE
      *  MVC   FOPRECNO,=C'  1'   SET RECORD NUMBER IN MESSAGE
      *  MVC   FWTMSG,FOPTYPE     MOVE MESSAGE TEXT
            MOVE FOP14      TO FOPSBA.
            MOVE 'MYFILE02' TO FOPNAME.
            MOVE '  1'      TO FOPRECNO.
            EXEC CICS SEND FROM(FOPLINE) LENGTH(FOPL) END-EXEC.
      *  MVC   MYRID,=F'1'        RIDFLD=1 RECORD  1
      *  MVC   MYLEN,=H'50'       SET LENGTH
      *  LA    R7,VSAMRTN6        R7=HANDLE RETURN
            MOVE 1  TO MYRID.
            MOVE 50 TO MYLEN.
            MOVE 6  TO DEPCTRL.
            EXEC CICS READ FILE('MYFILE02') INTO(MYDATA) LENGTH(MYLEN)
                      RIDFLD(MYRID) RBA END-EXEC.

      * VSAMRTN6 DS    0H
        VSAMRTN6.
      * ILLOGIC
      *  MVC   FWTSBA,FOP16       MOVE WCC,SBA
      *  MVC   FOPNAME,=CL8'MYFILE01' SET FILENAME IN MESSAGE
      *  MVC   FOPRECNO,=C'  1'   SET RECORD NUMBER IN MESSAGE
      *  MVC   FOPMETH,=CL4'RRN'  SET METHOD IN MESSAGE
      *  MVC   FWTMSG,FOPTYPE     MOVE MESSAGE TEXT
            MOVE FOP16      TO FOPSBA.
            MOVE 'MYFILE01' TO FOPNAME.
            MOVE '  1'      TO FOPRECNO.
            MOVE 'RRN '     TO FOPMETH.
            EXEC CICS SEND FROM(FOPLINE) LENGTH(FOPL) END-EXEC.
      *  MVC   MYRID,=F'1'        RIDFLD=1 RECORD  1
      *  MVC   MYLEN,=H'50'       SET LENGTH
      *  LA    R7,VSAMRTNA        R7=HANDLE RETURN
            MOVE 1  TO MYRID.
            MOVE 50 TO MYLEN.
            MOVE 10 TO DEPCTRL.
            EXEC CICS READ FILE('MYFILE01') INTO(MYDATA) LENGTH(MYLEN)
                      RIDFLD(MYRID) RRN END-EXEC.

      * VSAMRTNA DS    0H
        VSAMRTNA.
      * START BROWSE
      *  MVC   FWTSBA,FOP18       MOVE WCC,SBA
      *  MVC   FOPNAME,=CL8'MYFILE01' SET FILENAME IN MESSAGE
      *  MVC   FOPRECNO,=C'   '   SET RECORD NUMBER IN MESSAGE
      *  MVC   FOPMETH,=CL4'RBA'  SET METHOD IN MESSAGE
      *  MVC   FOPFUNC,=CL8'STARTBR' SET FUNCTION IN MESSAGE
      *  MVC   FWTMSG,FOPTYPE     MOVE MESSAGE TEXT
            MOVE FOP18      TO FOPSBA.
            MOVE 'MYFILE01' TO FOPNAME.
            MOVE SPACES     TO FOPRECNO.
            MOVE 'RBA '     TO FOPMETH.
            MOVE 'STARTBR ' TO FOPFUNC.
            EXEC CICS SEND FROM(FOPLINE) LENGTH(FOPL) END-EXEC.
      *  MVC   MYRID,=XL4'D9'     RIDFLD=X'D9' RECORD 15
            MOVE 217 TO MYRID.
            EXEC CICS STARTBR FILE('MYFILE01') RIDFLD(MYRID) RBA
                      END-EXEC.
      * READNEXT 3 RECORDS
      *  MVC   FWTSBA,FOP19       MOVE WCC,SBA
      *  MVC   FOPRECNO,=C'S  '   SET RECORD NUMBERS IN MESSAGE
      *  MVC   FOPFUNC,=CL8'READNEXT' SET FUNCTION IN MESSAGE
      *  MVC   FWTMSG,FOPTYPE     MOVE MESSAGE TEXT
            MOVE FOP19      TO FOPSBA.
            MOVE 'S  '      TO FOPRECNO.
            MOVE 'READNEXT' TO FOPFUNC.
            EXEC CICS SEND FROM(FOPLINE) LENGTH(FOPL) END-EXEC.
      *  LA    R8,3               READ 3 FORWARDS
      *  RDNXLOOP EQU   *
      *  BAL   R9,RDNX01          READNEXT MYFILE01 RBA
      *  BAL   R9,WQTS            WRITE IT TO TS Q
      *  BCT   R8,RDNXLOOP        LOOP
            MOVE 1 TO LOOPCTRL.
        RDNXLOOP.
            PERFORM RDNX01.
            PERFORM WQTS.
            ADD 1 TO LOOPCTRL.
            IF LOOPCTRL NOT = 4 THEN GO TO RDNXLOOP.
      * READPREV 3 RECORDS
      *  MVC   FWTSBA,FOP20       MOVE WCC,SBA
      *  MVC   FOPRECNO,=C'S  '   SET RECORD NUMBERS IN MESSAGE
      *  MVC   FOPFUNC,=CL8'READPREV' SET FUNCTION IN MESSAGE
      *  MVC   FWTMSG,FOPTYPE     MOVE MESSAGE TEXT
            MOVE FOP20      TO FOPSBA.
            MOVE 'S  '      TO FOPRECNO.
            MOVE 'READPREV' TO FOPFUNC.
            EXEC CICS SEND FROM(FOPLINE) LENGTH(FOPL) END-EXEC.
      *  LA    R8,3               READ 3 BACKWARDS
      * RDPVLOOP EQU   *
      *  MVC   MYLEN,=H'50'       SET MAXIMUM LENGTH
      *  EXEC  CICS READPREV FILE('MYFILE01') INTO(MYDATA)
      *        LENGTH(MYLEN) RIDFLD(MYRID) RBA
      *  BAL   R9,WQTS            WRITE IT TO TS Q
      *  BCT   R8,RDPVLOOP        LOOP
            MOVE 1 TO LOOPCTRL.
        RDPVLOOP.
            MOVE 50 TO MYLEN.
            EXEC CICS READPREV FILE('MYFILE01') INTO(MYDATA)
                      LENGTH(MYLEN) RIDFLD(MYRID) RBA END-EXEC.
            PERFORM WQTS.
            ADD 1 TO LOOPCTRL.
            IF LOOPCTRL NOT = 4 THEN GO TO RDPVLOOP.
      * READ RANDOM 3 RECORDS
      *  MVC   FWTSBA,FOP21       MOVE WCC,SBA
      *  MVC   FOPRECNO,=C'S  '   SET RECORD NUMBERS IN MESSAGE
      *  MVC   FOPFUNC,=CL8'READ RND' SET FUNCTION IN MESSAGE
      *  MVC   FWTMSG,FOPTYPE     MOVE MESSAGE TEXT
            MOVE FOP21      TO FOPSBA.
            MOVE 'S  '      TO FOPRECNO.
            MOVE 'READ RND' TO FOPFUNC.
            EXEC CICS SEND FROM(FOPLINE) LENGTH(FOPL) END-EXEC.
      *  MVC   MYRID,=XL4'20D'    RIDFLD=X'20D' RECORD 26
            MOVE 525 TO MYRID.
      *  BAL   R9,RDNX01          READNEXT MYFILE01 RBA
      *  BAL   R9,WQTS            WRITE IT TO TS Q
            PERFORM RDNX01.
            PERFORM WQTS.
      *  MVC   MYLEN,=H'50'       SET MAXIMUM LENGTH
      *  MVC   MYRID,=XL4'C3'     RIDFLD=X'C3' RECORD 14
            MOVE 50  TO MYLEN.
            MOVE 195 TO MYRID.
      * POINT AT RECORD 14, READ RECORD 13
            EXEC CICS READPREV FILE('MYFILE01') INTO(MYDATA)
                      LENGTH(MYLEN) RIDFLD(MYRID) RBA END-EXEC.
      *  BAL   R9,WQTS            WRITE IT TO TS Q
            PERFORM WQTS.
      *  MVC   MYRID,=XL4'121'    RIDFLD=X'121' RECORD 18
            MOVE 289 TO MYRID.
      *  BAL   R9,RDNX01          READNEXT MYFILE01 RBA
      *  BAL   R9,WQTS            WRITE IT TO TS Q
            PERFORM RDNX01.
            PERFORM WQTS.
      * END BROWSE
      *  MVC   FWTSBA,FOP1C2      MOVE WCC,SBA
      *  MVI   FOPSEP+1,C' '      RESET SEPARATOR
      *  LH    R7,FOPL            MESSAGE LENGTH
      *  AHI   R7,-1              SHORTEN BY 1
      *  STH   R7,FOPL            STORE BACK
      *  MVC   FOPRECNO,=C'   '   SET RECORD NUMBERS IN MESSAGE
      *  MVC   FOPFUNC,=CL8'ENDBR' SET FUNCTION IN MESSAGE
      *  MVC   FWTMSG,FOPTYPE     MOVE MESSAGE TEXT
            MOVE FOP1C2     TO FOPSBA.
            MOVE SPACE      TO FOPSEP.
            SUBTRACT 1 FROM FOPL.
            MOVE SPACES     TO FOPRECNO.
            MOVE 'ENDBR   ' TO FOPFUNC.
            EXEC CICS SEND FROM(FOPLINE) LENGTH(FOPL) END-EXEC.
            EXEC CICS ENDBR FILE('MYFILE01') END-EXEC.

      * NOTFND 2
      * START BROWSE, READNEXT, INVALID RBA, RAISE NOTFND 2
      *  MVC   FWTSBA,FOP2C2      MOVE WCC,SBA
      *  MVC   FOPNAME,=CL8'MYFILE01' SET FILENAME IN MESSAGE
      *  MVC   FOPRECNO,=C'   '   SET RECORD NUMBER IN MESSAGE
      *  MVC   FOPFUNC,=CL8'READNEXT' SET FUNCTION IN MESSAGE
      *  MVC   FWTMSG,FOPTYPE     MOVE MESSAGE TEXT
            MOVE FOP2C2     TO FOPSBA.
            MOVE 'MYFILE01' TO FOPNAME.
            MOVE SPACES     TO FOPRECNO.
            MOVE 'READNEXT' TO FOPFUNC.
            EXEC CICS SEND FROM(FOPLINE) LENGTH(FOPL) END-EXEC.
      *  MVC   MYRID,=XL4'C0'     RIDFLD=X'C0' BAD RBA
      *  MVI   NOTFNDNO,C'2'      SET 2ND NOTFND
      *  LH    R7,FHANDL          MESSAGE LENGTH
      *  AHI   R7,-1              SHORTEN BY 1
      *  STH   R7,FHANDL          STORE BACK
      *  LA    R7,VSAMRTN7        R7=HANDLE RETURN
            MOVE 192 TO MYRID.
            MOVE 2   TO NOTFNDNO.
            SUBTRACT 1 FROM FHANDL.
            MOVE 7   TO DEPCTRL.
            EXEC CICS STARTBR FILE('MYFILE01') RIDFLD(MYRID) RBA
                      END-EXEC.
      *  BAL   R9,RDNX01          READNEXT MYFILE01 RBA
            PERFORM RDNX01.

      * VSAMRTN7 DS    0H
        VSAMRTN7.
      * ENDFILE 1 RAISED BY READING FORWARD FROM END
      *  MVC   FWTSBA,FOP4C2      MOVE WCC,SBA
      *  MVC   MYRID,=F'-1'       RIDFLD=-1  LAST RECORD
      *  MVC   FOPRECNO,=C'END'   SET RECORD NUMBER IN MESSAGE
      *  MVC   FOPFUNC,=CL8'READNEXT' SET FUNCTION IN MESSAGE
      *  MVC   FWTMSG,FOPTYPE     MOVE MESSAGE TEXT
            MOVE FOP4C2     TO FOPSBA.
            MOVE -1         TO MYRID.
            MOVE 'END'      TO FOPRECNO.
            MOVE 'READNEXT' TO FOPFUNC.
            EXEC CICS SEND FROM(FOPLINE) LENGTH(FOPL) END-EXEC.
      *  MVI   ENDFLENO,C'1'      SET 1ST ENDFILE
      *  LA    R7,VSAMRTN8        R7=HANDLE RETURN
            MOVE 1 TO ENDFLENO.
            MOVE 8 TO DEPCTRL.
      *  BAL   R9,RDNX01          READNEXT MYFILE01 RBA
            PERFORM RDNX01.

      * VSAMRTN8 DS    0H
        VSAMRTN8.
      * ENDFILE 2 RAISED BY READING BACK FROM START
      *  MVC   FWTSBA,FOP6C2      MOVE WCC,SBA
      *  MVC   MYLEN,=H'50'       SET MAXIMUM LENGTH
      *  MVC   MYRID,=F'0'        RIDFLD=0  FIRST RECORD
      *  MVC   FOPRECNO,=C'BEG'   SET RECORD NUMBER IN MESSAGE
      *  MVC   FOPFUNC,=CL8'READPREV' SET FUNCTION IN MESSAGE
      *  MVC   FWTMSG,FOPTYPE     MOVE MESSAGE TEXT
            MOVE FOP6C2     TO FOPSBA.
            MOVE 50         TO MYLEN.
            MOVE ZERO       TO MYRID.
            MOVE 'BEG'      TO FOPRECNO.
            MOVE 'READPREV' TO FOPFUNC.
            EXEC CICS SEND FROM(FOPLINE) LENGTH(FOPL) END-EXEC.
      *  MVI   ENDFLENO,C'2'      SET 2ND ENDFILE
      *  LA    R7,VSAMRTN9        R7=HANDLE RETURN
            MOVE 2 TO ENDFLENO.
            MOVE 9 TO DEPCTRL.
            EXEC CICS READPREV FILE('MYFILE01') INTO(MYDATA)
                      LENGTH(MYLEN) RIDFLD(MYRID) RBA END-EXEC.

      * VSAMRTN9 DS    0H
        VSAMRTN9.
            EXEC CICS ENDBR FILE('MYFILE01') END-EXEC.

      * COMPLEX TEST, 2 OVERLAPPING BROWSES OF SAME FILE, PLUS READ OF
      * FILE THAT IS A MIRROR OF ANOTHER
      *  MVC   FWTSBA,FOP8C2      MOVE WCC,SBA
      *  MVC   FOPNAME,=CL8'COMPLEX' SET FILENAME IN MESSAGE
      *  MVC   FOPRECNO,=C'   '   SET RECORD NUMBER IN MESSAGE
      *  MVC   FOPFUNC,=CL8'COMPLEX' SET FUNCTION IN MESSAGE
      *  MVC   FWTMSG,FOPTYPE     MOVE MESSAGE TEXT
            MOVE FOP8C2     TO FOPSBA.
            MOVE 'COMPLEX ' TO FOPNAME.
            MOVE SPACES     TO FOPRECNO.
            MOVE 'COMPLEX ' TO FOPFUNC.
            EXEC CICS SEND FROM(FOPLINE) LENGTH(FOPL) END-EXEC.
      *  MVC   MYRID1,=XL4'45'    RIDFLD=X'45' RECORD 7
            MOVE 69 TO MYRID1.
            EXEC CICS STARTBR FILE('MYFILE01') RIDFLD(MYRID1) RBA
                      REQID(1) END-EXEC.
      *  MVC   MYRID2,=XL4'87'    RIDFLD=X'87' RECORD 11(BACK TO 10)
            MOVE 135 TO MYRID2.
            EXEC CICS STARTBR FILE('MYFILE01') RIDFLD(MYRID2) RBA
                      REQID(2) END-EXEC.
      *  XC    MYRID,MYRID        MYFILE05 START
            MOVE ZERO TO MYRID.
      *  LA    R8,4               R8=LOOP LIMIT
            MOVE 1 TO LOOPCTRL.
      * PLEXLOOP EQU   *
        PLEXLOOP.
      *  MVC   MYLEN,=H'50'       SET MAXIMUM LENGTH
            MOVE 50 TO MYLEN.
            EXEC CICS READNEXT FILE('MYFILE01') INTO(MYDATA)
                 LENGTH(MYLEN) RIDFLD(MYRID1) RBA REQID(1) END-EXEC.
      *  BAL   R9,WQTS            WRITE IT TO TS Q
            PERFORM WQTS.
      *  MVC   MYLEN,=H'50'       SET MAXIMUM LENGTH
            MOVE 50 TO MYLEN.
            EXEC CICS READPREV FILE('MYFILE01') INTO(MYDATA)
                      LENGTH(MYLEN) RIDFLD(MYRID2) RBA REQID(2)
                      END-EXEC.
      *  BAL   R9,WQTS            WRITE IT TO TS Q
            PERFORM WQTS.
      *  MVC   MYLEN,=H'50'       SET MAXIMUM LENGTH
            MOVE 50 TO MYLEN.
            EXEC  CICS READ FILE('MYFILE05') INTO(MYDATA)
                       LENGTH(MYLEN) RIDFLD(MYRID) RBA END-EXEC.
      *  BAL   R9,WQTS            WRITE IT TO TS Q
            PERFORM WQTS.
      *  L     R9,MYRID           CURRENT RBA
      *  AHI   R9,50              +RECORD LENGTH
      *  ST    R9,MYRID           NEW RBA
            ADD 50 TO MYRID.
      *  BCT   R8,PLEXLOOP        LOOP
            ADD 1 TO LOOPCTRL.
            IF LOOPCTRL NOT = 5 THEN GO TO PLEXLOOP.
            EXEC CICS ENDBR FILE('MYFILE01') REQID(1) END-EXEC.
            EXEC CICS ENDBR FILE('MYFILE01') REQID(2) END-EXEC.

      * MYFILE05, READ RECS 2-4, RESETBR, READ RECS 8-6
      *  MVC   FWTSBA,FOP9C2      MOVE WCC,SBA
      *  MVC   FOPNAME,=CL8'MYFILE05' SET FILENAME IN MESSAGE
      *  MVC   FOPRECNO,=C'   '   SET RECORD NUMBER IN MESSAGE
      *  MVC   FOPFUNC,=CL8'RESETBR' SET FUNCTION IN MESSAGE
      *  MVC   FWTMSG,FOPTYPE     MOVE MESSAGE TEXT
            MOVE FOP9C2     TO FOPSBA.
            MOVE 'MYFILE05' TO FOPNAME.
            MOVE SPACES     TO FOPRECNO.
            MOVE 'RESETBR ' TO FOPFUNC.
            EXEC CICS SEND FROM(FOPLINE) LENGTH(FOPL) END-EXEC.
      *  MVC   MYRID1,=AL4(2*50)  RIDFLD=X'64' RECORD 2
            MOVE 100 TO MYRID1.
            EXEC CICS STARTBR FILE('MYFILE05') RIDFLD(MYRID1) RBA
                      REQID(1) END-EXEC.
      *  LA    R8,3               R8=LOOP LIMIT
            MOVE 1 TO LOOPCTRL.
      * RSBRLP1  EQU   *
        RSBRLP1.
      *  MVC   MYLEN,=H'50'       SET MAXIMUM LENGTH
            MOVE 50 TO MYLEN.
            EXEC CICS READNEXT FILE('MYFILE05') INTO(MYDATA)
                      LENGTH(MYLEN) RIDFLD(MYRID1) RBA REQID(1)
                      END-EXEC.
      *  BAL   R9,WQTS            WRITE IT TO TS Q
            PERFORM WQTS.
      *  BCT   R8,RSBRLP1         LOOP
            ADD 1 TO LOOPCTRL.
            IF LOOPCTRL NOT = 4 THEN GO TO RSBRLP1.
      *  MVC   MYRID1,=AL4(8*50)  RIDFLD=X'190' RECORD 8
            MOVE 400 TO MYRID1.
            EXEC CICS RESETBR FILE('MYFILE05') RIDFLD(MYRID1) RBA
                      REQID(1) END-EXEC.
      *  LA    R8,3               R8=LOOP LIMIT
            MOVE 1 TO LOOPCTRL.
      * RSBRLP2  EQU   *
        RSBRLP2.
      *  MVC   MYLEN,=H'50'       SET MAXIMUM LENGTH
            MOVE 50 TO MYLEN.
            EXEC CICS READPREV FILE('MYFILE05') INTO(MYDATA)
                      LENGTH(MYLEN) RIDFLD(MYRID1) RBA REQID(1)
                      END-EXEC.
      *  BAL   R9,WQTS            WRITE IT TO TS Q
            PERFORM WQTS.
      *  BCT   R8,RSBRLP2         LOOP
            ADD 1 TO LOOPCTRL.
            IF LOOPCTRL NOT = 4 THEN GO TO RSBRLP2.
            EXEC CICS ENDBR FILE('MYFILE05') REQID(1) END-EXEC.
            EXEC CICS SEND FROM(ENDMSG) LENGTH(ENDMSGL) END-EXEC.
            EXEC CICS RETURN END-EXEC.

      *
      * HANDLE ROUTINES
      *
      * VSAMFNF  DS    0H
        VSAMFNF.
      *  MVC   FWTSBA,FOP5        MOVE WCC,SBA
      *  MVC   FHANDCON,=CL12'FILENOTFOUND' MOVE CONDITION
            MOVE FOP5           TO FHASBA.
            MOVE 'FILENOTFOUND' TO FHANDCON.
      *  BAL   R9,HEXFDBK         DISPLAY FEEDBACK CODES
            PERFORM HEXFDBK.
      *  B     FWTHSEND           EXIT
            GO TO FWTHSEND.

      * VSAMDIS  DS    0H
        VSAMDIS.
      *  MVC   FWTSBA,FOP7        MOVE WCC,SBA
      *  MVC   FHANDCON,=CL12'DISABLED' MOVE CONDITION
            MOVE FOP7           TO FHASBA.
            MOVE 'DISABLED    ' TO FHANDCON.
      *  BAL   R9,HEXFDBK         DISPLAY FEEDBACK CODES
            PERFORM HEXFDBK.
      *  B     FWTHSEND           EXIT
            GO TO FWTHSEND.

      * VSAMNOTO DS    0H
        VSAMNOTO.
      *  MVC   FWTSBA,FOP9        MOVE WCC,SBA
      *  MVC   FHANDCON,=CL12'NOTOPEN' MOVE CONDITION
            MOVE FOP9           TO FHASBA.
            MOVE 'NOTOPEN     ' TO FHANDCON.
      *  BAL   R9,HEXFDBK         DISPLAY FEEDBACK CODES
            PERFORM HEXFDBK.
      *  B     FWTHSEND           EXIT
            GO TO FWTHSEND.

      * VSAMNOTF DS    0H
        VSAMNOTF.
      *  CLI   NOTFNDNO,C'1'      1ST ?
      *  BE    NOTFNDGO           EXIT IF IT IS
            GO TO NOTF1 NOTF2 DEPENDING ON NOTFNDNO.
            GO TO ABEND.
        NOTF1.
      *  MVC   FWTSBA,FOP11       MOVE WCC,SBA
            MOVE FOP11 TO FHASBA.
            GO TO NOTFEND.
        NOTF2.
      *  MVI   FHSEP,C' '         RESET SEPARATOR
      *  MVC   FWTSBA,FOP3C2      MOVE WCC,SBA
            MOVE SPACE  TO FHSEP.
            MOVE FOP3C2 TO FHASBA.
      * NOTFNDGO EQU   *
        NOTFEND.
      *  MVC   FHANDCON,=CL12'NOTFND' MOVE CONDITION
            MOVE 'NOTFND      ' TO FHANDCON.
      *  BAL   R9,HEXFDBK         DISPLAY FEEDBACK CODES
            PERFORM HEXFDBK.
      *  B     FWTHSEND           EXIT
            GO TO FWTHSEND.

      * VSAMLNG  DS    0H
        VSAMLNG.
      *  MVC   FWTSBA,FOP13       MOVE WCC,SBA
            MOVE FOP13 TO FHASBA.
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

      * VSAMINV  DS    0H
        VSAMINV.
      *  MVC   FWTSBA,FOP15       MOVE WCC,SBA
            MOVE FOP15 TO FHASBA.
      *  L     R8,EIBRESP2        R8=RESP2
      *  CVD   R8,DEC             CONVERT TO DECIMAL
      *  XC    FHANDCON,FHANDCON  CLEAR CONDITION
      *  MVC   FHANDCON+5(4),=X'40202020' MOVE EDIT WORD
      *  ED    FHANDCON+5(4),DEC+6 EDIT RESP2
      *  MVC   FHANDCON(7),=C'INVREQ/' MOVE CONDITION
            MOVE 'INVREQ/     ' TO FHANDCON.
            MOVE EIBRESP2 TO WORK1A.
            MOVE WORK1B TO FHAINV.
      *  BAL   R9,HEXFDBK         DISPLAY FEEDBACK CODES
            PERFORM HEXFDBK.
      *  B     FWTHSEND           EXIT
            GO TO FWTHSEND.

      * VSAMILL  DS    0H
        VSAMILL.
      *  MVC   FWTSBA,FOP17       MOVE WCC,SBA
            MOVE FOP17 TO FHASBA.
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

      * VSAMENDF DS    0H
        VSAMENDF.
      *  CLI   ENDFLENO,C'1'      1ST ?
      *  BE    ENDFLEGO           EXIT IF IT IS
            GO TO ENDF1 ENDF2 DEPENDING ON ENDFLENO.
            GO TO ABEND.
        ENDF1.
      *  MVC   FWTSBA,FOP5C2      MOVE WCC,SBA
            MOVE FOP5C2 TO FHASBA.
            GO TO ENDFEND
        ENDF2.
      *  MVC   FWTSBA,FOP7C2      MOVE WCC,SBA
            MOVE FOP7C2 TO FHASBA.
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
            GO TO VSAMRTN1 VSAMRTN2 VSAMRTN3 VSAMRTN4 VSAMRTN5
                  VSAMRTN6 VSAMRTN7 VSAMRTN8 VSAMRTN9 VSAMRTNA
                  DEPENDING ON DEPCTRL.

        ABEND.
            EXEC CICS ABEND ABCODE('VSC1') END-EXEC.

      *
      * SUBROUTINES
      *
      * MY01     EQU   *
        MY01.
            EXEC CICS READ FILE('MYFILE01') INTO(MYDATA) LENGTH(MYLEN)
                      RIDFLD(MYRID) RBA END-EXEC.
      *  BR    R9                 RETURN

      * RDNX01   EQU   *
        RDNX01.
      *  MVC   MYLEN,=H'50'       SET MAXIMUM LENGTH
            MOVE 50 TO MYLEN.
            EXEC CICS READNEXT FILE('MYFILE01') INTO(MYDATA)
                      LENGTH(MYLEN) RIDFLD(MYRID) RBA END-EXEC.
      *  BR    R9

      * WQTS     EQU   *
        WQTS.
            EXEC CICS WRITEQ TS QUEUE('VSC1') FROM(MYDATA)
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
            IF FDBK2 = X'20' THEN MOVE '20' TO FHFDBK2.
            IF FDBK2 = X'68' THEN MOVE '68' TO FHFDBK2.
            IF FDBK2 = X'6C' THEN MOVE '6C' TO FHFDBK2.
      *  BR    R9                 RETURN
