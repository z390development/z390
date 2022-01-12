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
      * 11/11/08 RPI 946 CORRECTIONS TO MESSAGES TO PREVENT 6'S       *
      *          DISPLAYED                                            *
      *****************************************************************
        IDENTIFICATION DIVISION.
      *
      * TEST INTERVAL CONTROL FUNCTIONS
      *
      *  ASKTIME
      *     SHOW BEFORE AND AFTER EIBDATE/EIBTIME
      *
      *  ASKTIME ASKTIME(STCKTIME)
      *     SHOW AFTER EIBDATE/EIBTIME
      *     SHOW DECIMAL STCK TIME IN 0.001 SECS SINCE 1/1/1900
      *
      *  DELAY TO SHOW IT WORKS
      *     INVREQ/ 4, 5, 6
      *
      *  FORMATTIME LINE 1
      *     DATE WITH/WITHOUT DATESEP
      *     FULLDATE WITH/WITHOUT DATESEP
      *  FORMATTIME LINE 2
      *     DDMMYY MMDDYY YYDDMM YYMMDD DATEFORM
      *  FORMATTIME LINE 3
      *     DDMMYYYY MMDDYYYY YYYYDDMM YYYYMMDD YYDDD YYYYDDD
      *     WITH DATESEP
      *  FORMATTIME LINE 4
      *     DATESTRING AND TIME WITH/WITHOUT TIMESEP
      *  FORMATTIME LINE 5
      *     INVREQ/ 1

        PROGRAM-ID. TESTICC1.
        ENVIRONMENT DIVISION.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        77  STCKTIME PIC 9(15)  COMP-3.
        01  FILLER REDEFINES STCKTIME.
            02  STCKBAD  PIC X.
            02  FILLER   PIC X(7).

        77  SBA3     PIC X(3)   VALUE X'11C260'.
        77  SBA4     PIC X(3)   VALUE X'11C3F0'.
        77  SBA5     PIC X(3)   VALUE X'11C540'.
        77  SBA6     PIC X(3)   VALUE X'11C650'.
        77  SBA7     PIC X(3)   VALUE X'11C760'.
        77  SBA8     PIC X(3)   VALUE X'11C8F0'.
        77  SBA9     PIC X(3)   VALUE X'114A40'.
        77  SBA10    PIC X(3)   VALUE X'114B50'.
        77  SBA11    PIC X(3)   VALUE X'114C60'.
        77  SBA12    PIC X(3)   VALUE X'114DF0'.
        77  SBA13    PIC X(3)   VALUE X'114F40'.
        77  SBA14    PIC X(3)   VALUE X'115050'.
        77  SBA15    PIC X(3)   VALUE X'11D160'.
        77  SBA16    PIC X(3)   VALUE X'11D2F0'.
        77  SBA17    PIC X(3)   VALUE X'11D440'.
        77  SBA18    PIC X(3)   VALUE X'11D550'.
        77  SBA19    PIC X(3)   VALUE X'11D660'.

        01  TDBEF.
            02  FILLER   PIC X     VALUE X'C3'.
            02  TDBEFSBA PIC X(3).
            02  FILLER   PIC X(13) VALUE 'EIBTIME/DATE '.
            02  FILLER   PIC X(16) VALUE 'AT TASK START   '.
            02  TIMEBEF  PIC 9(8).
            02  FILLER   REDEFINES TIMEBEF.
                03  TIMEBEFS PIC X(2).
                03  FILLER   PIC X(6).
            02  DATEBEF  PIC 9(8).
            02  FILLER   REDEFINES DATEBEF.
                03  DATEBEFS PIC X.
                03  FILLER   PIC X(7).
        77  TDBEFLEN PIC 9(4) COMP.

        01  TDAFT.
            02  FILLER   PIC X     VALUE X'C3'.
            02  TDAFTSBA PIC X(3).
            02  FILLER   PIC X(13) VALUE 'EIBTIME/DATE '.
            02  TIMESPEC PIC X(16) VALUE 'AFTER TASK START'.
            02  TIMEAFT  PIC 9(8).
            02  FILLER   REDEFINES TIMEAFT.
                03  TIMEAFTS PIC X(2)  VALUE SPACES.
                03  TIMEAFDL PIC X(6)  VALUE SPACES.
            02  FILLER   REDEFINES TIMEAFT.
                03  FILLER   PIC X(6).
                03  TIMEAF59 PIC X(2).
            02  DATEAFT  PIC 9(8).
            02  FILLER   REDEFINES DATEAFT.
                03  DATEAFTS PIC X.
                03  FILLER   PIC X(7).
            02  STCKAFT  PIC 9(16).
            02  FILLER   REDEFINES STCKAFT.
                03  STCKAFTS PIC X     VALUE SPACE.
                03  FILLER   PIC X(15) VALUE SPACES.
        77  TDAFTLEN PIC 9(4) COMP.

        01  FORMAT.
            02  FILLER   PIC X     VALUE X'C3'.
            02  FMTSBA   PIC X(3).
            02  FILLER   PIC X(11) VALUE 'FORMATTIME '.
            02  FMTDATE1 PIC X(10) VALUE SPACES.
            02  FILLER   PIC X     VALUE SPACE.
            02  FMTDATE2 PIC X(10) VALUE SPACES.
            02  FILLER   PIC X     VALUE SPACE.
            02  FMTDATE3 PIC X(10) VALUE SPACES.
            02  FILLER   PIC X     VALUE SPACE.
            02  FMTDATE4 PIC X(10) VALUE SPACES.
            02  FILLER   PIC X     VALUE SPACE.
            02  FMTDATE5 PIC X(10) VALUE SPACES.
            02  FILLER   PIC X     VALUE SPACE.
            02  FMTDATE6 PIC X(10) VALUE SPACES.
        77  FORMATL  PIC 9(4) COMP.

        01  FORMAT2.
            02  FILLER   PIC X     VALUE X'C3'.
            02  FMT2SBA  PIC X(3).
            02  FILLER   PIC X(11) VALUE 'FORMATTIME '.
            02  DAYCOUNT PIC Z(5)9.
            02  FILLER   PIC X     VALUE SPACE.
            02  DAYWEEK  PIC ZZZ9.
            02  FILLER   PIC X     VALUE SPACE.
            02  DAYMONTH PIC ZZZ9.
            02  FILLER   PIC X     VALUE SPACE.
            02  MONTHYR  PIC ZZZ9.
            02  FILLER   PIC X     VALUE SPACE.
            02  YEARNUM  PIC Z(5)9.
            02  FILLER   PIC X     VALUE SPACE.
        77  FORMAT2L PIC 9(4) COMP.

        01  FORMAT3.
            02  FILLER   PIC X     VALUE X'C3'.
            02  FMT3SBA  PIC X(3).
            02  FILLER   PIC X(11) VALUE 'FORMATTIME '.
            02  DSTRING  PIC X(25) VALUE SPACES.
            02  FILLER   PIC X     VALUE SPACE.
            02  FMTTIME1 PIC X(6)  VALUE SPACES.
            02  FILLER   PIC X     VALUE SPACE.
            02  FMTTIME2 PIC X(8)  VALUE SPACES.
            02  FILLER   PIC X     VALUE SPACE.
            02  FMTTIME3 PIC X(8)  VALUE SPACES.
        77  FORMAT3L PIC 9(4) COMP.

        01  ICCOND.
            02  FILLER   PIC X     VALUE X'C3'.
            02  ICSBA    PIC X(3).
            02  ICFUNC   PIC X(10) VALUE 'DELAY     '.
            02  FILLER   PIC X     VALUE SPACE.
            02  FHANDCON.
                03  FILLER   PIC X(7) VALUE SPACES.
                03  FHAINV   PIC X(2).
                03  FILLER   PIC X(3) VALUE SPACES.
            02  FILLER   PIC X(18) VALUE ' CONDITION RAISED '.
        77  FHANDL   PIC 9(4) COMP.

        01  ENDMSG.
            02  FILLER   PIC X(4)  VALUE X'C3115CF0'.
            02  FILLER   PIC X(31)
                         VALUE 'TESTICC1 COMPLETED, PRESS CLEAR'.
        77  ENDMSGL  PIC 9(4) COMP.

        01  FILLER.
            02  WORK1A   PIC 9(4).
            02  FILLER   REDEFINES WORK1A.
                03  FILLER   PIC X(2).
                03  WORK1B   PIC X(2).

        77  SECS19   PIC X(6)  VALUE '000019'.
        77  DAYCNT   PIC 9(9)  COMP.
        77  DAYOFWK  PIC 9(9)  COMP.
        77  DAYOFMN  PIC 9(9)  COMP.
        77  MONTH    PIC 9(9)  COMP.
        77  YEAR     PIC 9(9)  COMP.
        77  MYSEP    PIC X     VALUE ':'.

      * GO TO DEPENDING CONTROL
      *   1=IC01RTN1
      *   2=IC01RTN2
      *   3=IC01RTN3
      *   4=IC01RTN4
      *   5=IC01RTN5
        77  DEPCTRL   PIC 9(4) COMP.

        LINKAGE SECTION.
        77  MYCWA    PIC X(50).

        PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA.

            MOVE LENGTH OF TDBEF    TO TDBEFLEN.
            MOVE LENGTH OF TDAFT    TO TDAFTLEN.
            MOVE LENGTH OF FORMAT   TO FORMATL.
            MOVE LENGTH OF FORMAT2  TO FORMAT2L.
            MOVE LENGTH OF FORMAT3  TO FORMAT3L.
            MOVE LENGTH OF ICCOND   TO FHANDL.
            MOVE LENGTH OF ENDMSG   TO ENDMSGL.

      * DISPLAY TASK START TIME
            EXEC CICS ADDRESS CWA(ADDRESS OF MYCWA) END-EXEC.
      *  MVC   TDBEFSBA,SBA3      1ST SBA
            MOVE SBA3 TO TDBEFSBA.
      *  MVC   TIMEBEF,=X'F020202020202020' MOVE EDIT WORD
      *  ED    TIMEBEF,EIBTIME    EDIT EIBTIME
            MOVE EIBTIME TO TIMEBEF.
      *  MVI   TIMEBEF,C' '       BLANK 1ST BYTE
      *  MVI   TIMEBEF+1,C' '     BLANK 2ND BYTE
            MOVE SPACES TO TIMEBEFS
      *  MVC   DATEBEF,=X'F020202020202020' MOVE EDIT WORD
      *  ED    DATEBEF,EIBDATE    EDIT EIBDATE
            MOVE EIBDATE TO DATEBEF.
      *  MVI   DATEBEF,C' '       BLANK 1ST BYTE
            MOVE SPACE TO DATEBEFS.
            EXEC CICS SEND FROM(TDBEF) LENGTH(TDBEFLEN) END-EXEC.

      * DISPLAY TIME NOW
            EXEC CICS ASKTIME END-EXEC.
      *  MVC   TDAFTSBA,SBA4      2ND SBA
            MOVE SBA4 TO TDAFTSBA.
      *  MVC   TIMEAFT,=X'F020202020202020' MOVE EDIT WORD
      *  ED    TIMEAFT,EIBTIME    EDIT EIBTIME
            MOVE EIBTIME TO TIMEAFT.
      *  MVI   TIMEAFT,C' '       BLANK 1ST BYTE
      *  MVI   TIMEAFT+1,C' '     BLANK 2ND BYTE
            MOVE SPACES TO TIMEAFTS.
      *  MVC   DATEAFT,=X'F020202020202020' MOVE EDIT WORD
      *  ED    DATEAFT,EIBDATE    EDIT EIBDATE
            MOVE EIBDATE TO DATEAFT.
      *  MVI   DATEAFT,C' '       BLANK 1ST BYTE
            MOVE SPACE TO DATEAFTS.
      *  MVC   TIMESPEC,=C'AFTER TASK START'
            MOVE 'AFTER TASK START' TO TIMESPEC.
            EXEC CICS SEND FROM(TDAFT) LENGTH(TDAFTLEN) END-EXEC.

      * DISPLAY TIME AFTER 9 SECS
            EXEC CICS DELAY INTERVAL(9) END-EXEC.
            EXEC CICS ASKTIME ABSTIME(STCKTIME) END-EXEC.
      *  MVC   TDAFTSBA,SBA5      2ND SBA
            MOVE SBA5 TO TDAFTSBA.
      *  MVC   TIMESPEC,=C'AFTER  9 SECONDS'
            MOVE 'AFTER  9 SECONDS' TO TIMESPEC.
      *  BAL   R9,DOAFT           SHOW TIME/DATE/STCKTIME
            PERFORM DOAFT.

      * DISPLAY TIME AFTER 9+12=21 SECS
            EXEC CICS DELAY INTERVAL(12) END-EXEC.
            EXEC CICS ASKTIME ABSTIME(STCKTIME) END-EXEC.
      *  MVC   TDAFTSBA,SBA6      3RD SBA
            MOVE SBA6 TO TDAFTSBA.
      *  MVC   TIMESPEC,=C'AFTER 12 SECONDS'
            MOVE 'AFTER 12 SECONDS' TO TIMESPEC.
      *  BAL   R9,DOAFT           SHOW TIME/DATE/STCKTIME
            PERFORM DOAFT.

      * DISPLAY TIME AFTER 9+12+19=40 SECS
            EXEC CICS DELAY INTERVAL(SECS19) END-EXEC.
            EXEC CICS ASKTIME ABSTIME(STCKTIME) END-EXEC.
      *  MVC   TDAFTSBA,SBA7      4TH SBA
            MOVE SBA7 TO TDAFTSBA.
      *  MVC   TIMESPEC,=C'AFTER 19 SECONDS'
            MOVE 'AFTER 19 SECONDS' TO TIMESPEC.
      *  BAL   R9,DOAFT           SHOW TIME/DATE/STCKTIME
            PERFORM DOAFT.

      * DISPLAY TIME AFTER 9+12+19+5=45 SECS
            EXEC CICS DELAY FOR SECONDS(5) END-EXEC.
            EXEC CICS ASKTIME ABSTIME(STCKTIME) END-EXEC.
      *  MVC   TDAFTSBA,SBA8      5TH SBA
            MOVE SBA8 TO TDAFTSBA.
      *  MVC   TIMESPEC,=C'AFTER  5 SECONDS'
            MOVE 'AFTER  5 SECONDS' TO TIMESPEC.
      *  BAL   R9,DOAFT           SHOW TIME/DATE/STCKTIME
            PERFORM DOAFT.

      * TIMEAFT='  HHMMSS'
      *  MVC   TIMEAFT+6(2),=C'59' WAIT TO END OF CURRENT MINUTE
            MOVE '59' TO TIMEAF59.
            EXEC CICS DELAY TIME(TIMEAFDL) END-EXEC.
            EXEC CICS ASKTIME ABSTIME(STCKTIME) END-EXEC.
      *  MVC   TDAFTSBA,SBA9      6TH SBA
            MOVE SBA9 TO TDAFTSBA.
      *  MVC   TIMESPEC,=C'UNTIL HHMM59    '
            MOVE 'UNTIL HHMM59    ' TO TIMESPEC.
      *  BAL   R9,DOAFT           SHOW TIME/DATE/STCKTIME
            PERFORM DOAFT.
      *
      * HANDLE ALL CONDITIONS, ALWAYS RETURN TO MAIN LINE
      *
            EXEC CICS HANDLE CONDITION
                      EXPIRED(IC01EXP)
                      INVREQ(IC01INV)
                      END-EXEC.
      *
      * INTERVAL EXPIRED, RAISE EXPIRED
      *
      *  MVC   ICSBA,SBA10        7TH SBA
            MOVE SBA10 TO ICSBA.
      *  LA    R7,IC01RTN1        R7=HANDLE RETURN
            MOVE 1 TO DEPCTRL.
            EXEC CICS DELAY INTERVAL(0) END-EXEC.

      *
      * BAD HOURS, RAISE INVREQ/4
      *
      * IC01RTN1 DS    0H
        IC01RTN1.
      *  MVC   ICSBA,SBA11        8TH SBA
            MOVE SBA11 TO ICSBA.
      *  LA    R7,IC01RTN2        R7=HANDLE RETURN
            MOVE 2 TO DEPCTRL.
            EXEC CICS DELAY FOR HOURS(456) END-EXEC.

      *
      * BAD MINUTES, RAISE INVREQ/5
      *
      * IC01RTN2 DS    0H
        IC01RTN2.
      *  MVC   ICSBA,SBA12        9TH SBA
            MOVE SBA12 TO ICSBA.
      *  LA    R7,IC01RTN3        R7=HANDLE RETURN
            MOVE 3 TO DEPCTRL.
            EXEC CICS DELAY TIME(128812) END-EXEC.

      *
      * BAD SECONDS, RAISE INVREQ/6
      *
      * IC01RTN3 DS    0H
        IC01RTN3.
      *  MVC   ICSBA,SBA13        10TH SBA
            MOVE SBA13 TO ICSBA.
      *  LA    R7,IC01RTN4        R7=HANDLE RETURN
            MOVE 4 TO DEPCTRL.
            EXEC CICS DELAY INTERVAL(77) END-EXEC.

      *
      * FORMATTIME TESTS
      *
      * IC01RTN4 DS    0H
        IC01RTN4.
      *  MVC   FMTSBA,SBA14       11TH SBA
            MOVE SBA14 TO FMTSBA.
            EXEC CICS FORMATTIME ABSTIME(STCKTIME)
                      DATE(FMTDATE1) FULLDATE(FMTDATE4) END-EXEC.
            EXEC CICS FORMATTIME ABSTIME(STCKTIME) DATE(FMTDATE2)
                      FULLDATE(FMTDATE5) DATESEP END-EXEC.
            EXEC CICS FORMATTIME ABSTIME(STCKTIME) DATE(FMTDATE3)
                      FULLDATE(FMTDATE6) DATESEP(MYSEP) END-EXEC.
            EXEC CICS SEND FROM(FORMAT) LENGTH(FORMATL) END-EXEC.

      *  XC    FMTDATE1(FMTDATE6+10-FMTDATE1),FMTDATE1 REUSE AREA
            MOVE SPACES TO FMTDATE1.
            MOVE SPACES TO FMTDATE2.
            MOVE SPACES TO FMTDATE3.
            MOVE SPACES TO FMTDATE4.
            MOVE SPACES TO FMTDATE5.
            MOVE SPACES TO FMTDATE6.
      *  MVC   FMTSBA,SBA15       12TH SBA
            MOVE SBA15 TO FMTSBA.
            EXEC CICS FORMATTIME ABSTIME(STCKTIME)
                      DDMMYY(FMTDATE1) MMDDYY(FMTDATE2)
                      YYDDMM(FMTDATE3) YYMMDD(FMTDATE4)
                      DATEFORM(FMTDATE5) END-EXEC.
            EXEC CICS SEND FROM(FORMAT) LENGTH(FORMATL) END-EXEC.

      *  XC    FMTDATE1(FMTDATE6+10-FMTDATE1),FMTDATE1 REUSE AREA
            MOVE SPACES TO FMTDATE1.
            MOVE SPACES TO FMTDATE2.
            MOVE SPACES TO FMTDATE3.
            MOVE SPACES TO FMTDATE4.
            MOVE SPACES TO FMTDATE5.
            MOVE SPACES TO FMTDATE6.
      *  MVC   FMTSBA,SBA16       13TH SBA
            MOVE SBA16 TO FMTSBA.
            EXEC CICS FORMATTIME ABSTIME(STCKTIME)
                      DDMMYYYY(FMTDATE1) MMDDYYYY(FMTDATE2)
                      YYYYDDMM(FMTDATE3) YYYYMMDD(FMTDATE4)
                      YYDDD(FMTDATE5) YYYYDDD(FMTDATE6) DATESEP(',')
                      END-EXEC.
            EXEC CICS SEND FROM(FORMAT) LENGTH(FORMATL) END-EXEC.

      *  MVC   FMT2SBA,SBA17      14TH SBA
            MOVE SBA17 TO FMT2SBA.
            EXEC CICS FORMATTIME ABSTIME(STCKTIME)
                      DAYCOUNT(DAYCNT) DAYOFWEEK(DAYOFWK)
                      DAYOFMONTH(DAYOFMN) MONTHOFYEAR(MONTH) YEAR(YEAR)
                      END-EXEC.
      *  L     R8,DAYCNT          LOAD DAY COUNT
      *  CVD   R8,DEC             CONVERT TO DECIMAL
      *  MVC   DAYCOUNT,=X'402020202120' MOVE MASK
      *  ED    DAYCOUNT,DEC+5     EDIT DAY COUNT
            MOVE DAYCNT TO DAYCOUNT.
      *  L     R8,DAYOFWK         LOAD DAY OF WEEK
      *  CVD   R8,DEC             CONVERT TO DECIMAL
      *  MVC   DAYWEEK,=X'40202120' MOVE MASK
      *  ED    DAYWEEK,DEC+6      EDIT DAY OF WEEK
            MOVE DAYOFWK TO DAYWEEK.
      *  L     R8,DAYOFMN         LOAD DAY OF MONTH
      *  CVD   R8,DEC             CONVERT TO DECIMAL
      *  MVC   DAYMONTH,=X'40202120' MOVE MASK
      *  ED    DAYMONTH,DEC+6      EDIT DAY OF MONTH
            MOVE DAYOFMN TO DAYMONTH.
      *  L     R8,MONTH           LOAD MONTH
      *  CVD   R8,DEC             CONVERT TO DECIMAL
      *  MVC   MONTHYR,=X'40202120' MOVE MASK
      *  ED    MONTHYR,DEC+6      EDIT MONTH
            MOVE MONTH TO MONTHYR.
      *  L     R8,YEAR            LOAD YEAR
      *  CVD   R8,DEC             CONVERT TO DECIMAL
      *  MVC   YEARNUM,=X'402020202120' MOVE MASK
      *  ED    YEARNUM,DEC+5      EDIT YEAR
            MOVE YEAR TO YEARNUM.
            EXEC CICS SEND FROM(FORMAT2) LENGTH(FORMAT2L) END-EXEC.

      *  MVC   FMT3SBA,SBA18      15TH SBA
            MOVE SBA18 TO FMT3SBA.
            EXEC CICS FORMATTIME ABSTIME(STCKTIME) DATESTRING(DSTRING)
                      TIME(FMTTIME1) END-EXEC.
            EXEC CICS FORMATTIME ABSTIME(STCKTIME)
                      TIME(FMTTIME2) TIMESEP END-EXEC.
            EXEC CICS FORMATTIME ABSTIME(STCKTIME)
                      TIME(FMTTIME3) TIMESEP('-') END-EXEC.
            EXEC CICS SEND FROM(FORMAT3) LENGTH(FORMAT3L) END-EXEC.

      *
      * BAD ABSTIME, RAISE INVREQ/1
      *
      *  MVC   ICSBA,SBA19        16TH SBA
            MOVE SBA19 TO ICSBA.
      *  MVC   ICFUNC(10),=C'FORMATTIME' SET FUNCTION
            MOVE 'FORMATTIME' TO ICFUNC.
      *  LA    R7,IC01RTN5        R7=HANDLE RETURN
            MOVE 5 TO DEPCTRL.
      *  MVI   STCKTIME,X'0A'     MAKE ABSTIME INVALID
            MOVE X'0A' TO STCKBAD.
            EXEC CICS FORMATTIME ABSTIME(STCKTIME) END-EXEC.

      * IC01RTN5 DS    0H
        IC01RTN5.
            EXEC CICS SEND FROM(ENDMSG) LENGTH(ENDMSGL) END-EXEC.
            EXEC CICS RETURN END-EXEC.

      * IC01EXP  DS    0H
        IC01EXP.
      *  MVC   FHANDCON(7),=C'EXPIRED' MOVE CONDITION
            MOVE 'EXPIRED     ' TO FHANDCON.
      *  B     ICONSEND           EXIT
            GO TO ICONSEND.

      * IC01INV  DS    0H
        IC01INV.
      *  L     R8,EIBRESP2        R8=RESP2
      *  CVD   R8,DEC             CONVERT TO DECIMAL
      *  XC    FHANDCON,FHANDCON  CLEAR CONDITION
      *  MVC   FHANDCON+5(4),=X'40202020' MOVE EDIT WORD
      *  ED    FHANDCON+5(4),DEC+6 EDIT RESP2
      *  MVC   FHANDCON(7),=C'INVREQ/' MOVE CONDITION
            MOVE 'INVREQ/     ' TO FHANDCON.
            MOVE EIBRESP2 TO WORK1A.
            MOVE WORK1B TO FHAINV.

      * ICONSEND DS    0H
        ICONSEND.
            EXEC CICS SEND FROM(ICCOND) LENGTH(FHANDL) END-EXEC.
      *  BR    R7                 RETURN TO MAIN LINE
            GO TO IC01RTN1 IC01RTN2 IC01RTN3 IC01RTN4 IC01RTN5
                  DEPENDING ON DEPCTRL.
      *
      * SUBROUTINES
      *
      * DOAFT    DS    0H
        DOAFT.
      *  MVC   TIMEAFT,=X'F020202020202020' MOVE EDIT WORD
      *  ED    TIMEAFT,EIBTIME    EDIT EIBTIME
            MOVE EIBTIME TO TIMEAFT.
      *  MVI   TIMEAFT,C' '       BLANK 1ST BYTE
      *  MVI   TIMEAFT+1,C' '     BLANK 2ND BYTE
            MOVE SPACES TO TIMEAFTS.
      *  MVC   DATEAFT,=X'F020202020202020' MOVE EDIT WORD
      *  ED    DATEAFT,EIBDATE    EDIT EIBDATE
            MOVE EIBDATE TO DATEAFT.
      *  MVI   DATEAFT,C' '       BLANK 1ST BYTE
            MOVE SPACE TO DATEAFTS.
      *  MVC   STCKAFT,=X'F0202020202020202020202020202020' MOVE EDWD
      *  ED    STCKAFT,STCKTIME   EDIT STCKTIME
            MOVE STCKTIME TO STCKAFT.
      *  MVI   STCKAFT,C' '       BLANK 1ST BYTE
            MOVE SPACE TO STCKAFTS.
            EXEC CICS SEND FROM(TDAFT) LENGTH(TDAFTLEN) END-EXEC.
      *  BR    R9                 RETURN
