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
      * TEST VSAM 3 (COBOL)
      *
      * KSDS/FIX MYFILE08 READ/KEY/FKS/KEQ  XI
      * KSDS/FIX MYFILE08 READ/KEY/FKS/KEQ  XENOMORPHICALLY
      * KSDS/FIX MYFILE08 READ/KEY/FKS/KEQ  XYLYL
      * KSDS/VAR MYFILE09 READ/KEY/FKS/KEQ  YNAMBUS *2
      * KSDS/VAR MYFILE09 READ/KEY/FKS/KEQ  YELLOWISHNESSES *2
      * KSDS/FIX MYFILE08 READ/KEY/FKS/KGE  XEBEC       (=> XB)
      * KSDS/VAR MYFILE09 READ/KEY/FKS/KGE  YMOLTEN *2  (=>YJ)
      * KSDS/FIX MYFILE08 READ/KEY/GEN/KGE  XANTHAM     (KL=0, 1ST REC)
      * KSDS/VAR MYFILE09 READ/KEY/GEN/KGE  YABBERED *2 (KL=0, 1ST REC)
      * KSDS/FIX MYFILE08 READ/KEY/GEN/KGE  XERAFIN     (KL=3, =>XEO)
      * KSDS/VAR MYFILE09 READ/KEY/GEN/KGE  YPLIGHT *2  (KL=4, =>YPES)
      * KSDS/FIX MYFILE08 READ/KEY/GEN/KEQ  XOANA       (KL=2, =XO)
      * KSDS/VAR MYFILE09 READ/KEY/GEN/KEQ  YCLEEPE *2  (KL=3, =>YCL)
      * INVREQ/25    CONDITION RAISED (READ INVALID GENERIC KEYLENGTH)
      * INVREQ/42    CONDITION RAISED (READ NEGATIVE GENERIC KEYLENGTH)
      * NOTFND       CONDITION RAISED (READ INVALID KEY EQUAL)
      * NOTFND       CONDITION RAISED (READ INVALID GEN KEY EQUAL)
      * NOTFND       CONDITION RAISED (READ INVALID KEY GTEQ,
      *                                                     BEYOND END)
      * NOTFND       CONDITION RAISED (READ INVALID GEN KEY GTEQ,
      *                                                     BEYOND END)
      *
      * KSDS/FIX MYFILE08 BROWSE FORWARD KEY=XERUS
      *                                        XERUS
      *                                        XERUSES
      *                                        XI
      *                                        XIPHIHUMERALIS
      *                   CHANGE KEY AND READ 2 KEY=XERIC
      *                                        XERIC
      *                                        XERICALLY
      *
      * KSDS/VAR MYFILE09 BROWSE BACK KGE KEY=YTT
      *                                        YTHUNDERED *2
      *                                        YSLAKED *2
      *                                        YSHENDS *2
      *                                        YSHENDING *2
      *                   CHANGE KEY TO END AND READ BACK KEY=15X'FF'
      *                                        YUPPIFYING *2
      *                                        YUPPIFY *2
      *
      * COMPLEX TEST, MULTIPLE REQIDS ON MYFILE08
      * KSDS/VAR MYFILE08 BROWSE FORWARD GENERIC EQUAL KEY=XOA
      * KSDS/VAR MYFILE08 BROWSE FORWARD GENERIC GTEQ  KEY=XYMM
      *    PROVES GENERIC IS POSITIONAL, NOT RETRIEVAL
      *                                        XOANA
      *                                        XYRIDACEOUS
      *                                        XOANON
      *                                        XYST
      *                                        XU
      *                                        XYSTER
      *
      *    CHANGE GENERIC KEYS
      *                                        XERAFIN
      *                                        XENIA
      *                                        XERAFINS
      *                                        XENIAL
      *                                        XERANSES
      *                                        XENIAS
      *    CHANGE GENERIC KEYS AND KEYLENGTHS
      *                                        XANTHOCHROIA
      *                                        XERODERMIA
      *                                        XANTHOCHROIAS
      *                                        XERODERMIAS
      *                                        XANTHOCHROIC
      *                                        XERODERMIC
      *    CHANGE TO GTEQ USING RESETBR
      *                                        XANTHOCHROIA
      *                                        XERODERMIA
      *                                        XANTHOCHROIAS
      *                                        XERODERMIAS
      *                                        XANTHOCHROIC
      *                                        XERODERMIC
      *
      * INVREQ/25    CONDITION RAISED (STARTBR INVALID GENERIC KLEN)
      * INVREQ/33    CONDITION RAISED (STARTBR REQID IN USE)
      * INVREQ/42    CONDITION RAISED (STARTBR NEGATIVE GENERIC KLEN)
      * INVREQ/24    CONDITION RAISED (READPREV AND STARTBR GENERIC)
      * INVREQ/41    CONDITION RAISED (READPREV NO STARTBR REQID)
      * ENDFILE      CONDITION RAISED (READNEXT EQUAL BEYOND END)
      * ENDFILE      CONDITION RAISED (READPREV EQUAL BEYOND BEG)

        PROGRAM-ID. TESTVSC3.
        ENVIRONMENT DIVISION.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        77  MYDATA   PIC X(35).
        77  MYLEN    PIC 9(4)  COMP.
        77  MYRID    PIC X(15).
        77  MYRID1   PIC X(15).
        77  MYKL1    PIC 9(4)  COMP.
        77  MYKL2    PIC 9(4)  COMP.
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
        77  FOP16    PIC X(4)   VALUE X'C311D550'.
        77  FOP18    PIC X(4)   VALUE X'C311D7F0'.
        77  FOP20    PIC X(4)   VALUE X'C3115A50'.
        77  FOP1C2   PIC X(4)   VALUE X'C311C3C9'.
        77  FOP3C2   PIC X(4)   VALUE X'C311C5E9'.
        77  FOP5C2   PIC X(4)   VALUE X'C311C8C9'.
        77  FOP6C2   PIC X(4)   VALUE X'C311C9D9'.
        77  FOP7C2   PIC X(4)   VALUE X'C3114AE9'.
        77  FOP8C2   PIC X(4)   VALUE X'C3114BF9'.
        77  FOP10C2  PIC X(4)   VALUE X'C3114ED9'.
        77  FOP12C2  PIC X(4)   VALUE X'C31150F9'.
        77  FOP14C2  PIC X(4)   VALUE X'C311D3D9'.
        77  FOP16C2  PIC X(4)   VALUE X'C311D5F9'.
        77  FOP18C2  PIC X(4)   VALUE X'C311D8D9'.
        77  FOP20C2  PIC X(4)   VALUE X'C3115AF9'.

      * KSDS FIX MYFILE08 READNEXT KEY GEN KEQ
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
            02  FOPMETH  PIC X(4)  VALUE 'KEY '.
            02  FOPGNFL  PIC X(4)  VALUE SPACES.
            02  FOPMTYP  PIC X(3)  VALUE SPACES.
            02  FILLER   PIC X     VALUE SPACE.
            02  FOPSEP   PIC X     VALUE SPACE.
        77  FOPL     PIC 9(4) COMP.

        01  FHAND.
            02  FHASBA   PIC X(4).
            02  FHANDCON.
                03  FILLER   PIC X(7) VALUE SPACES.
                03  FHAINV   PIC X(2).
                03  FILLER   PIC X(3) VALUE SPACES.
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
                         VALUE 'TESTVSC3 COMPLETED, PRESS CLEAR'.
        77  ENDMSGL  PIC 9(4) COMP.

        01  FILLER.
            02  WORK1A   PIC 9(4).
            02  FILLER   REDEFINES WORK1A.
                03  FILLER   PIC X(2).
                03  WORK1B   PIC X(2).

        01  WORK2.
            02  FILLER PIC X.
            02  FDBK1  PIC X.
            02  FDBK2  PIC X.
            02  FILLER PIC X(3).

        77  BADKL    PIC 9(4)  COMP  VALUE -5.

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
      *  11=VSAMRTNB
      *  12=VSAMRTNC
      *  13=VSAMRTND
      *  14=ABEND
        77  DEPCTRL   PIC 9(4) COMP.

      * GO TO DEPENDING CONTROL
      *   1=RDCXTST1
      *   2=RDCXTST2
      *   3=RDCXTST3
      *   4=RDCXTST4
        77  PLEXCTRL  PIC 9(4) COMP.

        77  INVREQNO  PIC 9(4) COMP.
        77  NOTFNDNO  PIC 9(4) COMP.
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
      *  MVC   FOPTYPE,=C'KSDS'   SET FILE TYPE IN MESSAGE
      *  MVC   FOPFIXVR,=C'FIX'   SET FIX/VAR IN MESSAGE
      *  MVC   FOPNAME,=C'MYFILE08' SET FILENAME IN MESSAGE
      *  MVC   FOPFUNC,=CL8'READ' SET FUNCTION IN MESSAGE
      *  MVC   FOPMETH,=CL4'KEY'  SET METHOD IN MESSAGE
      *  MVC   FOPGNFL,=CL4'FKS'  SET FULL KEY
      *  MVC   FOPMTYP,=C'KEQ'    SET KEY EQUAL
      *  MVC   MYRID,=CL15'XI'    RECORD BY KEY
            MOVE 'KSDS'            TO FOPTYPE.
            MOVE 'FIX'             TO FOPFIXVR.
            MOVE 'MYFILE08'        TO FOPNAME.
            MOVE 'READ    '        TO FOPFUNC.
            MOVE 'KEY '            TO FOPMETH.
            MOVE 'FKS '            TO FOPGNFL.
            MOVE 'KEQ'             TO FOPMTYP.
            MOVE 'XI             ' TO MYRID.
      *  BAL   R9,RDFL08          READ MYFILE08
            PERFORM RDFL08.
      *  BAL   R9,WQTS            WRITE IT TO TS Q
            PERFORM WQTS.
      *  MVC   FWTSBA,FOP1        MOVE WCC,SBA
            MOVE FOP1 TO FOPSBA.
      *  MVC   FWTMSG,FOPTYPE     MOVE MESSAGE TEXT
      *  BAL   R9,SENDIT          SCREEN DISPLAY
            PERFORM SENDIT.

      *  MVC   MYRID,=CL15'XENOMORPHICALLY' RECORD BY KEY
            MOVE 'XENOMORPHICALLY' TO MYRID.
      *  BAL   R9,RDFL08          READ MYFILE08
            PERFORM RDFL08.
      *  BAL   R9,WQTS            WRITE IT TO TS Q
            PERFORM WQTS.
      *  MVC   FWTSBA,FOP2        MOVE WCC,SBA
            MOVE FOP2 TO FOPSBA.
      *  MVC   FWTMSG,FOPTYPE     MOVE MESSAGE TEXT
      *  BAL   R9,SENDIT          SCREEN DISPLAY
            PERFORM SENDIT.

      *  MVC   MYRID,=CL15'XYLYL' RECORD BY KEY
            MOVE 'XYLYL          ' TO MYRID.
      *  BAL   R9,RDFL08          READ MYFILE08
            PERFORM RDFL08.
      *  BAL   R9,WQTS            WRITE IT TO TS Q
            PERFORM WQTS.
      *  MVC   FWTSBA,FOP3        MOVE WCC,SBA
            MOVE FOP3 TO FOPSBA.
      *  MVC   FWTMSG,FOPTYPE     MOVE MESSAGE TEXT
      *  BAL   R9,SENDIT          SCREEN DISPLAY
            PERFORM SENDIT.

      *  MVC   FOPFIXVR,=C'VAR'   SET FIX/VAR IN MESSAGE
      *  MVC   FOPNAME,=C'MYFILE09' SET FILENAME IN MESSAGE
      *  MVC   MYRID,=CL15'YNAMBUS' RECORD BY KEY
            MOVE 'VAR'             TO FOPFIXVR.
            MOVE 'MYFILE09'        TO FOPNAME.
            MOVE 'YNAMBUS        ' TO MYRID.
      *  BAL   R9,RDFL09          READ MYFILE09
            PERFORM RDFL09.
      *  BAL   R9,WQTS            WRITE IT TO TS Q
            PERFORM WQTS.
      *  MVC   FWTSBA,FOP4        MOVE WCC,SBA
            MOVE FOP4 TO FOPSBA.
      *  MVC   FWTMSG,FOPTYPE     MOVE MESSAGE TEXT
      *  BAL   R9,SENDIT          SCREEN DISPLAY
            PERFORM SENDIT.

      *  MVC   MYRID,=CL15'YELLOWISHNESSES' RECORD BY KEY
            MOVE 'YELLOWISHNESSES' TO MYRID.
      *  BAL   R9,RDFL09          READ MYFILE09
            PERFORM RDFL09.
      *  BAL   R9,WQTS            WRITE IT TO TS Q
            PERFORM WQTS.
      *  MVC   FWTSBA,FOP5        MOVE WCC,SBA
            MOVE FOP5 TO FOPSBA.
      *  MVC   FWTMSG,FOPTYPE     MOVE MESSAGE TEXT
      *  BAL   R9,SENDIT          SCREEN DISPLAY
            PERFORM SENDIT.

      *  MVC   FOPNAME,=C'MYFILE08' SET FILENAME IN MESSAGE
      *  MVC   FOPFIXVR,=C'FIX'   SET FIX/VAR IN MESSAGE
      *  MVC   FOPMTYP,=C'KGE'    SET KEY GTEQ
      *  MVC   MYRID,=CL15'XB'    RECORD BY KEY
      *  MVC   MYLEN,=H'15'       SET MAXIMUM LENGTH
            MOVE 'MYFILE08'        TO FOPNAME.
            MOVE 'FIX'             TO FOPFIXVR.
            MOVE 'KGE'             TO FOPMTYP.
            MOVE 'XB             ' TO MYRID.
            MOVE 15                TO MYLEN.
            EXEC CICS READ FILE('MYFILE08') INTO(MYDATA) LENGTH(MYLEN)
                      RIDFLD(MYRID) GTEQ END-EXEC.
      *  BAL   R9,WQTS            WRITE IT TO TS Q
            PERFORM WQTS.
      *  MVC   FWTSBA,FOP6        MOVE WCC,SBA
            MOVE FOP6 TO FOPSBA.
      *  MVC   FWTMSG,FOPTYPE     MOVE MESSAGE TEXT
      *  BAL   R9,SENDIT          SCREEN DISPLAY
            PERFORM SENDIT.

      *  MVC   FOPNAME,=C'MYFILE09' SET FILENAME IN MESSAGE
      *  MVC   FOPFIXVR,=C'VAR'   SET FIX/VAR IN MESSAGE
      *  MVC   MYRID,=CL15'YJ'    RECORD BY KEY
      *  MVC   MYLEN,=H'35'       SET MAXIMUM LENGTH
            MOVE 'MYFILE09'        TO FOPNAME.
            MOVE 'VAR'             TO FOPFIXVR.
            MOVE 'YJ             ' TO MYRID.
            MOVE 35                TO MYLEN.
            EXEC CICS READ FILE('MYFILE09') INTO(MYDATA) LENGTH(MYLEN)
                      RIDFLD(MYRID) GTEQ END-EXEC.
      *  BAL   R9,WQTS            WRITE IT TO TS Q
            PERFORM WQTS.
      *  MVC   FWTSBA,FOP7        MOVE WCC,SBA
            MOVE FOP7 TO FOPSBA.
      *  MVC   FWTMSG,FOPTYPE     MOVE MESSAGE TEXT
      *  BAL   R9,SENDIT          SCREEN DISPLAY
            PERFORM SENDIT.

      *  MVC   FOPNAME,=C'MYFILE08' SET FILENAME IN MESSAGE
      *  MVC   FOPFIXVR,=C'FIX'   SET FIX/VAR IN MESSAGE
      *  MVC   FOPGNFL,=CL4'GEN'  SET GENERIC KEY
      *  MVC   MYRID,=CL15'ZZ'    RECORD BY KEY (IGNORED)
      *  MVC   MYLEN,=H'15'       SET MAXIMUM LENGTH
            MOVE 'MYFILE08'        TO FOPNAME.
            MOVE 'FIX'             TO FOPFIXVR.
            MOVE 'GEN '            TO FOPGNFL.
            MOVE 'ZZ             ' TO MYRID.
            MOVE 15                TO MYLEN.
            EXEC CICS READ FILE('MYFILE08') INTO(MYDATA) LENGTH(MYLEN)
                      RIDFLD(MYRID) KEYLENGTH(0) GENERIC END-EXEC.
      *  BAL   R9,WQTS            WRITE IT TO TS Q
            PERFORM WQTS.
      *  MVC   FWTSBA,FOP8        MOVE WCC,SBA
            MOVE FOP8 TO FOPSBA.
      *  MVC   FWTMSG,FOPTYPE     MOVE MESSAGE TEXT
      *  BAL   R9,SENDIT          SCREEN DISPLAY
            PERFORM SENDIT.

      *  MVC   FOPNAME,=C'MYFILE09' SET FILENAME IN MESSAGE
      *  MVC   FOPFIXVR,=C'VAR'   SET FIX/VAR IN MESSAGE
      *  MVC   MYRID,=CL15'ZZ'    RECORD BY KEY (IGNORED)
      *  MVC   MYLEN,=H'35'       SET MAXIMUM LENGTH
            MOVE 'MYFILE09'        TO FOPNAME.
            MOVE 'VAR'             TO FOPFIXVR.
            MOVE 'ZZ             ' TO MYRID.
            MOVE 35                TO MYLEN.
            EXEC CICS READ FILE('MYFILE09') INTO(MYDATA) LENGTH(MYLEN)
                      RIDFLD(MYRID) KEYLENGTH(0) GENERIC GTEQ END-EXEC.
      *  BAL   R9,WQTS            WRITE IT TO TS Q
            PERFORM WQTS.
      *  MVC   FWTSBA,FOP9        MOVE WCC,SBA
            MOVE FOP9 TO FOPSBA.
      *  MVC   FWTMSG,FOPTYPE     MOVE MESSAGE TEXT
      *  BAL   R9,SENDIT          SCREEN DISPLAY
            PERFORM SENDIT.

      *  MVC   FOPNAME,=C'MYFILE08' SET FILENAME IN MESSAGE
      *  MVC   FOPFIXVR,=C'FIX'   SET FIX/VAR IN MESSAGE
      *  MVC   FOPGNFL,=CL4'GEN'  SET GENERIC KEY
      *  MVC   MYRID,=CL15'XEO'   RECORD BY KEY
      *  MVC   MYLEN,=H'15'       SET MAXIMUM LENGTH
            MOVE 'MYFILE08'        TO FOPNAME.
            MOVE 'FIX'             TO FOPFIXVR.
            MOVE 'GEN '            TO FOPGNFL.
            MOVE 'XEO            ' TO MYRID.
            MOVE 15                TO MYLEN.
            EXEC CICS READ FILE('MYFILE08') INTO(MYDATA) LENGTH(MYLEN)
                      RIDFLD(MYRID) KEYLENGTH(3) GENERIC GTEQ END-EXEC.
      *  BAL   R9,WQTS            WRITE IT TO TS Q
            PERFORM WQTS.
      *  MVC   FWTSBA,FOP10       MOVE WCC,SBA
            MOVE FOP10 TO FOPSBA.
      *  MVC   FWTMSG,FOPTYPE     MOVE MESSAGE TEXT
      *  BAL   R9,SENDIT          SCREEN DISPLAY
            PERFORM SENDIT.

      *  MVC   FOPNAME,=C'MYFILE09' SET FILENAME IN MESSAGE
      *  MVC   FOPFIXVR,=C'VAR'   SET FIX/VAR IN MESSAGE
      *  MVC   MYRID,=CL15'YPES'  RECORD BY KEY
      *  MVC   MYLEN,=H'35'       SET MAXIMUM LENGTH
            MOVE 'MYFILE09'        TO FOPNAME.
            MOVE 'VAR'             TO FOPFIXVR.
            MOVE 'YPES           ' TO MYRID.
            MOVE 35                TO MYLEN.
            EXEC CICS READ FILE('MYFILE09') INTO(MYDATA) LENGTH(MYLEN)
                      RIDFLD(MYRID) KEYLENGTH(4) GENERIC GTEQ END-EXEC.
      *  BAL   R9,WQTS            WRITE IT TO TS Q
            PERFORM WQTS.
      *  MVC   FWTSBA,FOP11       MOVE WCC,SBA
            MOVE FOP11 TO FOPSBA.
      *  MVC   FWTMSG,FOPTYPE     MOVE MESSAGE TEXT
      *  BAL   R9,SENDIT          SCREEN DISPLAY
            PERFORM SENDIT.

      *  MVC   FOPNAME,=C'MYFILE08' SET FILENAME IN MESSAGE
      *  MVC   FOPFIXVR,=C'FIX'   SET FIX/VAR IN MESSAGE
      *  MVC   FOPGNFL,=CL4'GEN'  SET GENERIC KEY
      *  MVC   MYRID,=CL15'XO'    RECORD BY KEY
      *  MVC   MYLEN,=H'15'       SET MAXIMUM LENGTH
            MOVE 'MYFILE08'        TO FOPNAME.
            MOVE 'FIX'             TO FOPFIXVR.
            MOVE 'GEN '            TO FOPGNFL.
            MOVE 'XO             ' TO MYRID.
            MOVE 15                TO MYLEN.
            EXEC CICS READ FILE('MYFILE08') INTO(MYDATA) LENGTH(MYLEN)
                      RIDFLD(MYRID) KEYLENGTH(2) GENERIC EQUAL
                      END-EXEC.
      *  BAL   R9,WQTS            WRITE IT TO TS Q
            PERFORM WQTS.
      *  MVC   FWTSBA,FOP12       MOVE WCC,SBA
            MOVE FOP12 TO FOPSBA.
      *  MVC   FWTMSG,FOPTYPE     MOVE MESSAGE TEXT
      *  BAL   R9,SENDIT          SCREEN DISPLAY
            PERFORM SENDIT.

      *  MVC   FOPNAME,=C'MYFILE09' SET FILENAME IN MESSAGE
      *  MVC   FOPFIXVR,=C'VAR'   SET FIX/VAR IN MESSAGE
      *  MVC   MYRID,=CL15'YCL'   RECORD BY KEY
      *  MVC   MYLEN,=H'35'       SET MAXIMUM LENGTH
            MOVE 'MYFILE09'        TO FOPNAME.
            MOVE 'VAR'             TO FOPFIXVR.
            MOVE 'YCL            ' TO MYRID.
            MOVE 35                TO MYLEN.
            EXEC CICS READ FILE('MYFILE09') INTO(MYDATA) LENGTH(MYLEN)
                      RIDFLD(MYRID) KEYLENGTH(3) GENERIC EQUAL
                      END-EXEC.
      *  BAL   R9,WQTS            WRITE IT TO TS Q
            PERFORM WQTS.
      *  MVC   FWTSBA,FOP13       MOVE WCC,SBA
            MOVE FOP13 TO FOPSBA.
      *  MVC   FWTMSG,FOPTYPE     MOVE MESSAGE TEXT
      *  BAL   R9,SENDIT          SCREEN DISPLAY
            PERFORM SENDIT.

      *
      * RAISE SOME CONDITIONS, WE MAY NOT DO ALL OF THESE FOR EVERY
      * FUNCTION
      *
            EXEC CICS HANDLE CONDITION
                 INVREQ(VSAMINV)
                 NOTFND(VSAMNOTF)
                 ENDFILE(VSAMENDF)
                 END-EXEC.

      * INVREQ/25    READ INVALID GENERIC KEYLENGTH
      *  MVC   FOPNAME,=C'MYFILE08' SET FILENAME IN MESSAGE
      *  MVC   FOPFIXVR,=C'FIX'   SET FIX/VAR IN MESSAGE
      *  MVC   FOPGNFL,=CL4'GEN'  SET GENERIC KEY
      *  MVC   MYRID,=CL15'AA'    RECORD BY KEY
      *  MVC   MYLEN,=H'15'       SET MAXIMUM LENGTH
            MOVE 'MYFILE08'        TO FOPNAME.
            MOVE 'FIX'             TO FOPFIXVR.
            MOVE 'GEN '            TO FOPGNFL.
            MOVE 'AA             ' TO MYRID.
            MOVE 15                TO MYLEN.
      *  MVC   FWTSBA,FOP14       MOVE WCC,SBA
            MOVE FOP14 TO FOPSBA.
      *  MVC   FWTMSG,FOPTYPE     MOVE MESSAGE TEXT
      *  BAL   R9,SENDIT          SCREEN DISPLAY
            PERFORM SENDIT.
      *  LA    R7,VSAMRTN1        R7=HANDLE RETURN
            MOVE 1 TO DEPCTRL.
      *  MVI   INVREQNO,C'1'      SET 1ST INVREQ
            MOVE 1 TO INVREQNO.
            EXEC CICS READ FILE('MYFILE08') INTO(MYDATA) LENGTH(MYLEN)
                      RIDFLD(MYRID) KEYLENGTH(15) GENERIC EQUAL
                      END-EXEC.

      * VSAMRTN1 DS    0H
        VSAMRTN1.
      * INVREQ/42    READ NEGATIVE GENERIC KEYLENGTH
      *  MVC   MYLEN,=H'15'       SET MAXIMUM LENGTH
            MOVE 15 TO MYLEN.
      *  MVC   FWTSBA,FOP16       MOVE WCC,SBA
            MOVE FOP16 TO FOPSBA.
      *  MVC   FWTMSG,FOPTYPE     MOVE MESSAGE TEXT
      *  BAL   R9,SENDIT          SCREEN DISPLAY
            PERFORM SENDIT.
      *  LA    R7,VSAMRTN2        R7=HANDLE RETURN
            MOVE 2 TO DEPCTRL.
      *  MVI   INVREQNO,C'2'      SET 2ND INVREQ
            MOVE 2 TO INVREQNO.
            EXEC CICS READ FILE('MYFILE08') INTO(MYDATA) LENGTH(MYLEN)
                      RIDFLD(MYRID) KEYLENGTH(BADKL) GENERIC EQUAL
                      END-EXEC.

      * VSAMRTN2 DS    0H
        VSAMRTN2.
      * NOTFND 1     READ INVALID KEY EQUAL
      *  MVC   FOPNAME,=C'MYFILE09' SET FILENAME IN MESSAGE
      *  MVC   FOPFIXVR,=C'VAR'   SET FIX/VAR IN MESSAGE
      *  MVC   FOPMETH,=CL4'KEY'  SET METHOD IN MESSAGE
      *  MVC   FOPGNFL,=CL4'FKS'  SET FULL KEY
      *  MVC   FOPMTYP,=C'KEQ'    SET KEY EQUAL
      *  MVC   MYRID,=CL15'ABC'   RECORD BY KEY
            MOVE 'MYFILE09'        TO FOPNAME.
            MOVE 'VAR'             TO FOPFIXVR.
            MOVE 'KEY '            TO FOPMETH.
            MOVE 'FKS '            TO FOPGNFL.
            MOVE 'KEQ'             TO FOPMTYP.
            MOVE 'ABC            ' TO MYRID.
      *  MVC   FWTSBA,FOP18       MOVE WCC,SBA
            MOVE FOP18 TO FOPSBA.
      *  MVC   FWTMSG,FOPTYPE     MOVE MESSAGE TEXT
      *  BAL   R9,SENDIT          SCREEN DISPLAY
            PERFORM SENDIT.
      *  LA    R7,VSAMRTN3        R7=HANDLE RETURN
            MOVE 3 TO DEPCTRL.
      *  MVI   NOTFNDNO,C'1'      SET 1ST NOTFND
            MOVE 1 TO NOTFNDNO.
      *  BAL   R9,RDFL09          READ MYFILE09
            PERFORM RDFL09.

      * VSAMRTN3 DS    0H
        VSAMRTN3.
      * NOTFND 2     READ INVALID GEN KEY EQUAL
      *  MVC   FOPNAME,=C'MYFILE08' SET FILENAME IN MESSAGE
      *  MVC   FOPFIXVR,=C'FIX'   SET FIX/VAR IN MESSAGE
      *  MVC   FOPGNFL,=CL4'GEN'  SET GENERIC KEY
      *  MVC   MYRID,=CL15'XF'    RECORD BY KEY
      *  MVC   MYLEN,=H'15'       SET MAXIMUM LENGTH
            MOVE 'MYFILE08'        TO FOPNAME.
            MOVE 'FIX'             TO FOPFIXVR.
            MOVE 'GEN '            TO FOPGNFL.
            MOVE 'XF             ' TO MYRID.
            MOVE 15                TO MYLEN.
      *  MVC   FWTSBA,FOP20       MOVE WCC,SBA
            MOVE FOP20 TO FOPSBA.
      *  MVC   FWTMSG,FOPTYPE     MOVE MESSAGE TEXT
      *  BAL   R9,SENDIT          SCREEN DISPLAY
            PERFORM SENDIT.
      *  LA    R7,VSAMRTN4        R7=HANDLE RETURN
            MOVE 4 TO DEPCTRL.
      *  MVI   NOTFNDNO,C'2'      SET 2ND NOTFND
            MOVE 2 TO NOTFNDNO.
            EXEC CICS READ FILE('MYFILE08') INTO(MYDATA) LENGTH(MYLEN)
                      RIDFLD(MYRID) KEYLENGTH(2) GENERIC EQUAL
                      END-EXEC.

      * VSAMRTN4 DS    0H
        VSAMRTN4.
      * NOTFND 3     READ INVALID KEY GTEQ, BEYOND END
      *  MVC   FOPNAME,=C'MYFILE09' SET FILENAME IN MESSAGE
      *  MVC   FOPFIXVR,=C'VAR'   SET FIX/VAR IN MESSAGE
      *  MVC   FOPMETH,=CL4'KEY'  SET METHOD IN MESSAGE
      *  MVC   FOPGNFL,=CL4'FKS'  SET FULL KEY
      *  MVC   FOPMTYP,=C'KGE'    SET KEY GTEQ
      *  MVC   MYRID,=CL15'YUPQ'  RECORD BY KEY
            MOVE 'MYFILE09'        TO FOPNAME.
            MOVE 'VAR'             TO FOPFIXVR.
            MOVE 'KEY '            TO FOPMETH.
            MOVE 'FKS '            TO FOPGNFL.
            MOVE 'KGE'             TO FOPMTYP.
            MOVE 'YUPQ           ' TO MYRID.
      *  MVC   FWTSBA,FOP1C2      MOVE WCC,SBA
            MOVE FOP1C2 TO FOPSBA.
      *  MVI   FOPSEP+1,C' '      RESET SEPARATOR
      *  LH    R7,FOPL            MESSAGE LENGTH
      *  AHI   R7,-1              SHORTEN BY 1
      *  STH   R7,FOPL            STORE BACK
            MOVE SPACE TO FOPSEP.
            SUBTRACT 1 FROM FOPL.
      *  MVI   FHSEP,C' '         RESET SEPARATOR
      *  LH    R7,FHANDL          MESSAGE LENGTH
      *  AHI   R7,-1              SHORTEN BY 1
      *  STH   R7,FHANDL          STORE BACK
            MOVE SPACE TO FHSEP.
            SUBTRACT 1 FROM FHANDL.
      *  MVC   FWTMSG,FOPTYPE     MOVE MESSAGE TEXT
      *  BAL   R9,SENDIT          SCREEN DISPLAY
            PERFORM SENDIT.
      *  LA    R7,VSAMRTN5        R7=HANDLE RETURN
            MOVE 5 TO DEPCTRL.
      *  MVI   NOTFNDNO,C'3'      SET 3RD NOTFND
            MOVE 3 TO NOTFNDNO.
      *  BAL   R9,RDFL09          READ MYFILE09
            PERFORM RDFL09.

      * VSAMRTN5 DS    0H
        VSAMRTN5.
      * NOTFND 4     READ INVALID GEN KEY GTEQ, BEYOND END
      *  MVC   FOPNAME,=C'MYFILE08' SET FILENAME IN MESSAGE
      *  MVC   FOPFIXVR,=C'FIX'   SET FIX/VAR IN MESSAGE
      *  MVC   FOPGNFL,=CL4'GEN'  SET GENERIC KEY
      *  MVC   FOPMTYP,=C'KGE'    SET KEY GTEQ
      *  MVC   MYRID,=CL15'XYT'   RECORD BY KEY
      *  MVC   MYLEN,=H'15'       SET MAXIMUM LENGTH
            MOVE 'MYFILE08'        TO FOPNAME.
            MOVE 'FIX'             TO FOPFIXVR.
            MOVE 'GEN '            TO FOPGNFL.
            MOVE 'KGE'             TO FOPMTYP.
            MOVE 'XYT            ' TO MYRID.
            MOVE 15                TO MYLEN.
      *  MVC   FWTSBA,FOP3C2      MOVE WCC,SBA
            MOVE FOP3C2 TO FOPSBA.
      *  MVC   FWTMSG,FOPTYPE     MOVE MESSAGE TEXT
      *  BAL   R9,SENDIT          SCREEN DISPLAY
            PERFORM SENDIT.
      *  LA    R7,VSAMRTN6        R7=HANDLE RETURN
            MOVE 6 TO DEPCTRL.
      *  MVI   NOTFNDNO,C'4'      SET 4TH NOTFND
            MOVE 4 TO NOTFNDNO.
            EXEC CICS READ FILE('MYFILE08') INTO(MYDATA) LENGTH(MYLEN)
                      RIDFLD(MYRID) KEYLENGTH(3) GENERIC GTEQ END-EXEC.

      * VSAMRTN6 DS    0H
        VSAMRTN6.
      *  SR    R7,R7              CONDITION NOT EXPECTED
            MOVE 14 TO DEPCTRL.
      * START BROWSE, POINT AT XERUS, FULLKEY, READ 4 RECS
      *  MVC   MYRID,=CL15'XERUS' RECORD BY KEY
            MOVE 'XERUS          ' TO MYRID.
      *  BAL   R9,SB08KEQ         STARTBR MYFILE08 EQUAL
            PERFORM SB08KEQ.
      * READNEXT 4 RECORDS
      *  MVC   FWTSBA,FOP5C2      MOVE WCC,SBA
            MOVE FOP5C2 TO FOPSBA.
      *  MVC   FOPFUNC,=CL8'READNEXT' SET FUNCTION IN MESSAGE
      *  MVC   FOPMETH,=CL4'KEY'  SET METHOD IN MESSAGE
      *  MVC   FOPGNFL,=CL4'FKS'  SET FULL KEY
      *  MVC   FOPMTYP,=C'KEQ'    SET KEY EQUAL
            MOVE 'READNEXT' TO FOPFUNC.
            MOVE 'KEY '     TO FOPMETH.
            MOVE 'FKS '     TO FOPGNFL.
            MOVE 'KEQ'      TO FOPMTYP.
      *  MVC   FWTMSG,FOPTYPE     MOVE MESSAGE TEXT
      *  BAL   R9,SENDIT          SCREEN DISPLAY
            PERFORM SENDIT.
      *  LA    R7,4               READ 4 FORWARDS
            MOVE 1 TO LOOPCTRL.
      * RDNXLP1  EQU   *
        RDNXLP1.
      *  MVC   MYLEN,=H'15'       SET MAXIMUM LENGTH
            MOVE 15 TO MYLEN.
            EXEC CICS READNEXT FILE('MYFILE08') INTO(MYDATA)
                      LENGTH(MYLEN) RIDFLD(MYRID) END-EXEC.
      *  BAL   R9,WQTS            WRITE IT TO TS Q
            PERFORM WQTS.
      *  BCT   R7,RDNXLP1         LOOP
            ADD 1 TO LOOPCTRL.
            IF LOOPCTRL NOT = 5 THEN GO TO RDNXLP1.
      * CHANGE FULL KEY AND READ 2
      *  MVC   MYRID,=CL15'XERIC' RECORD BY KEY
            MOVE 'XERIC          ' TO MYRID.
      *  LA    R7,2               READ 2 FORWARDS
            MOVE 1 TO LOOPCTRL.
      * RDNXLP2  EQU   *
        RDNXLP2.
      *  MVC   MYLEN,=H'15'       SET MAXIMUM LENGTH
            MOVE 15 TO MYLEN.
            EXEC CICS READNEXT FILE('MYFILE08') INTO(MYDATA)
                      LENGTH(MYLEN) RIDFLD(MYRID) END-EXEC.
      *  BAL   R9,WQTS            WRITE IT TO TS Q
            PERFORM WQTS.
      *  BCT   R7,RDNXLP2         LOOP
            ADD 1 TO LOOPCTRL.
            IF LOOPCTRL NOT = 3 THEN GO TO RDNXLP2.
      *  BAL   R9,ENDBR08         ENDBR MYFILE08
            PERFORM ENDBR08.

      * START BROWSE, POINT AT YTT, FULLKEY, KGE READ BACK 4 RECS
      *  MVC   MYRID,=CL15'YTT'   RECORD BY KEY
            MOVE 'YTT            ' TO MYRID.
            EXEC CICS STARTBR FILE('MYFILE09') RIDFLD(MYRID) GTEQ
                      END-EXEC.
      * READPREV 4 RECORDS
      *  MVC   FWTSBA,FOP6C2      MOVE WCC,SBA
            MOVE FOP6C2 TO FOPSBA.
      *  MVC   FOPNAME,=C'MYFILE09' SET FILENAME IN MESSAGE
      *  MVC   FOPFIXVR,=C'VAR'   SET FIX/VAR IN MESSAGE
      *  MVC   FOPMTYP,=C'KGE'    SET KEY GTEQ
      *  MVC   FOPFUNC,=CL8'READPREV' SET FUNCTION IN MESSAGE
            MOVE 'MYFILE09' TO FOPNAME.
            MOVE 'VAR'      TO FOPFIXVR.
            MOVE 'KGE'      TO FOPMTYP.
            MOVE 'READPREV' TO FOPFUNC.
      *  MVC   FWTMSG,FOPTYPE     MOVE MESSAGE TEXT
      *  BAL   R9,SENDIT          SCREEN DISPLAY
            PERFORM SENDIT.
      *  LA    R7,4               READ 4 BACKWARDS
            MOVE 1 TO LOOPCTRL.
      * RDPVLP1  EQU   *
        RDPVLP1.
      *  MVC   MYLEN,=H'35'       SET MAXIMUM LENGTH
            MOVE 35 TO MYLEN.
            EXEC CICS READPREV FILE('MYFILE09') INTO(MYDATA)
                      LENGTH(MYLEN) RIDFLD(MYRID) END-EXEC.
      *  BAL   R9,WQTS            WRITE IT TO TS Q
            PERFORM WQTS.
      *  BCT   R7,RDPVLP1         LOOP
            ADD 1 TO LOOPCTRL.
            IF LOOPCTRL NOT = 5 THEN GO TO RDPVLP1.
      * CHANGE FULL KEY TO END AND READ BACK 2
      *  MVC   MYRID,=15X'FF'     RECORD BY KEY
            MOVE HIGH-VALUES TO MYRID.
      *  LA    R7,2               READ 2 BACKWARDS
            MOVE 1 TO LOOPCTRL.
      * RDPVLP2  EQU   *
        RDPVLP2.
      *  MVC   MYLEN,=H'35'       SET MAXIMUM LENGTH
            MOVE 35 TO MYLEN.
            EXEC CICS READPREV FILE('MYFILE09') INTO(MYDATA)
                      LENGTH(MYLEN) RIDFLD(MYRID) END-EXEC.
      *  BAL   R9,WQTS            WRITE IT TO TS Q
            PERFORM WQTS.
      *  BCT   R7,RDPVLP2         LOOP
            ADD 1 TO LOOPCTRL.
            IF LOOPCTRL NOT = 3 THEN GO TO RDPVLP2.
            EXEC CICS ENDBR FILE('MYFILE09') END-EXEC.

      * COMPLEX TESTS
      * START BROWSE FOR BOTH FILES, BOTH GENERIC
      *  MVC   FWTSBA,FOP7C2      MOVE WCC,SBA
            MOVE FOP7C2 TO FOPSBA.
      *  MVC   FOPNAME,=C'COMPLEX ' SET FILENAME IN MESSAGE
      *  MVC   FOPFIXVR,=C'   '   RESET
      *  MVC   FOPMTYP,=C'   '    RESET
      *  MVC   FOPFUNC,=CL8'READS   ' SET FUNCTION IN MESSAGE
      *  MVC   FOPGNFL,=CL4'GEN'  SET GENERIC KEY
      *  MVC   MYRID,=CL15'XOA'   RECORD BY KEY
      *  MVC   MYKL1,=H'3'        MOVE KEYLENGTH
            MOVE 'COMPLEX '        TO FOPNAME.
            MOVE SPACES            TO FOPFIXVR.
            MOVE SPACES            TO FOPMTYP.
            MOVE 'READS   '        TO FOPFUNC.
            MOVE 'GEN '            TO FOPGNFL.
            MOVE 'XOA            ' TO MYRID.
            MOVE 3                 TO MYKL1
            EXEC CICS STARTBR FILE('MYFILE08') RIDFLD(MYRID)
                      GENERIC EQUAL KEYLENGTH(MYKL1) REQID(1) END-EXEC.
      *  MVC   MYRID1,=CL15'XYMM' RECORD BY KEY
      *  MVC   MYKL2,=H'4'        MOVE KEYLENGTH
            MOVE 'XYMM           ' TO MYRID1.
            MOVE 4                 TO MYKL2
            EXEC CICS STARTBR FILE('MYFILE08') RIDFLD(MYRID1)
                      GENERIC GTEQ  KEYLENGTH(MYKL2) REQID(2) END-EXEC.
      *  MVC   FWTMSG,FOPTYPE     MOVE MESSAGE TEXT
      *  BAL   R9,SENDIT          SCREEN DISPLAY
            PERFORM SENDIT.
      *  MVI   RDCXGT1+1,X'00'    RESET SWITCH 1
      *  MVI   RDCXGT2+1,X'00'    RESET SWITCH 2
      *  MVI   RDCXGT3+1,X'00'    RESET SWITCH 3
            MOVE 1 TO PLEXCTRL.
      * RDCXREAD EQU   *
        RDCXREAD.
      *  LA    R7,3               READ 3 FORWARD GENERIC
            MOVE 1 TO LOOPCTRL.
      * RDCXLP1  EQU   *
        RDCXLP1.
      *  MVC   MYLEN,=H'15'       SET MAXIMUM LENGTH
            MOVE 15 TO MYLEN.
            EXEC CICS READNEXT FILE('MYFILE08') INTO(MYDATA)
                      LENGTH(MYLEN) KEYLENGTH(MYKL1) RIDFLD(MYRID)
                      REQID(1) END-EXEC.
      *  BAL   R9,WQTS            WRITE IT TO TS Q
            PERFORM WQTS.
      *  MVC   MYLEN,=H'15'       SET MAXIMUM LENGTH
            MOVE 15 TO MYLEN.
            EXEC CICS READNEXT FILE('MYFILE08') INTO(MYDATA)
                      LENGTH(MYLEN) KEYLENGTH(MYKL2) RIDFLD(MYRID1)
                      REQID(2) END-EXEC.
      *  BAL   R9,WQTS            WRITE IT TO TS Q
            PERFORM WQTS.
      *  BCT   R7,RDCXLP1         LOOP
            ADD 1 TO LOOPCTRL.
            IF LOOPCTRL NOT = 4 THEN GO TO RDCXLP1.
      * RDCXGT1  NOP   RDCXTST2           SWITCH 1
      * RDCXGT2  NOP   RDCXTST3           SWITCH 2
      * RDCXGT3  NOP   RDCXTST4           SWITCH 3
            GO TO RDCXTST1 RDCXTST2 RDCXTST3 RDCXTST4
                  DEPENDING ON PLEXCTRL.
            GO TO ABEND.

        RDCXTST1.
      * CHANGE GENERIC KEYS
      *  MVC   MYRID,=CL15'XER'   RECORD BY KEY
      *  MVC   MYRID1,=CL15'XENB' RECORD BY KEY
            MOVE 'XER            ' TO MYRID.
            MOVE 'XENB           ' TO MYRID1.
      *  MVI   RDCXGT1+1,X'F0'    SET SWITCH 1
            MOVE 2 TO PLEXCTRL.
      *  B     RDCXREAD           EXIT
            GO TO RDCXREAD.

      * RDCXTST2 EQU   *
        RDCXTST2.
      * CHANGE GENERIC KEYS AND KEYLENGTHS
      *  MVC   MYRID,=CL15'XANTHO' RECORD BY KEY
      *  MVC   MYKL1,=H'6'        MOVE KEYLENGTH
      *  MVC   MYRID1,=CL15'XERODERMB' RECORD BY KEY
      *  MVC   MYKL2,=H'9'        MOVE KEYLENGTH
            MOVE 'XANTHO         ' TO MYRID.
            MOVE 6                 TO MYKL1.
            MOVE 'XERODERMB      ' TO MYRID1.
            MOVE 9                 TO MYKL2.
      *  MVI   RDCXGT1+1,X'00'    RESET SWITCH 1
      *  MVI   RDCXGT2+1,X'F0'    SET SWITCH 2
            MOVE 3 TO PLEXCTRL.
      *  B     RDCXREAD           EXIT
            GO TO RDCXREAD.

      * RDCXTST3 EQU   *
        RDCXTST3.
      * RESET 1ST BROWSE TO GTEQ
      *  MVC   MYRID,=CL15'XANTHJ' RECORD BY KEY
      *  MVC   MYRID1,=CL15'XERODERMB' RECORD BY KEY
            MOVE 'XANTHJ         ' TO MYRID.
            MOVE 'XERODERMB      ' TO MYRID1.
            EXEC CICS RESETBR FILE('MYFILE08') RIDFLD(MYRID)
                      GENERIC GTEQ  KEYLENGTH(MYKL1) REQID(1) END-EXEC.
      *  MVI   RDCXGT1+1,X'00'    RESET SWITCH 1
      *  MVI   RDCXGT2+1,X'00'    RESET SWITCH 2
      *  MVI   RDCXGT3+1,X'F0'    SET SWITCH 3
            MOVE 4 TO PLEXCTRL.
      *  B     RDCXREAD           EXIT
            GO TO RDCXREAD.

      * RDCXTST4 EQU   *
        RDCXTST4.
            EXEC CICS ENDBR FILE('MYFILE08') REQID(2) END-EXEC.
            EXEC CICS ENDBR FILE('MYFILE08') REQID(1) END-EXEC.

      * INVREQ/25    STARTBR INVALID GENERIC KEYLENGTH
      *  MVC   FOPNAME,=C'MYFILE08' SET FILENAME IN MESSAGE
      *  MVC   FOPFIXVR,=C'FIX'   SET FIX/VAR IN MESSAGE
      *  MVC   FOPGNFL,=CL4'GEN'  SET GENERIC KEY
      *  MVC   FOPMTYP,=C'KEQ'    SET EQUAL
      *  MVC   FOPFUNC,=CL8'STARTBR ' SET FUNCTION IN MESSAGE
      *  MVC   MYRID,=CL15'AA'    RECORD BY KEY
      *  MVC   MYLEN,=H'15'       SET MAXIMUM LENGTH
            MOVE 'MYFILE08'        TO FOPNAME.
            MOVE 'FIX'             TO FOPFIXVR.
            MOVE 'GEN '            TO FOPGNFL.
            MOVE 'KEQ'             TO FOPMTYP.
            MOVE 'STARTBR '        TO FOPFUNC.
            MOVE 'AA             ' TO MYRID.
            MOVE 15                TO MYLEN.
      *  MVC   FWTSBA,FOP8C2      MOVE WCC,SBA
            MOVE FOP8C2 TO FOPSBA.
      *  MVC   FWTMSG,FOPTYPE     MOVE MESSAGE TEXT
      *  BAL   R9,SENDIT          SCREEN DISPLAY
            PERFORM SENDIT.
      *  LA    R7,VSAMRTN7        R7=HANDLE RETURN
            MOVE 7 TO DEPCTRL.
      *  MVI   INVREQNO,C'3'      SET 3RD INVREQ
            MOVE 3 TO INVREQNO.
            EXEC CICS STARTBR FILE('MYFILE08') RIDFLD(MYRID)
                      GENERIC EQUAL KEYLENGTH(15) END-EXEC.

      * VSAMRTN7 EQU   *
        VSAMRTN7.
      * INVREQ/33    STARTBR REQID IN USE
      *  MVC   FOPGNFL,=CL4'FKS'  SET FULL KEY
      *  MVC   FOPMTYP,=C'KEQ'    SET EQUAL
      *  MVC   MYRID,=CL15'XERIC' RECORD BY KEY
      *  MVC   MYLEN,=H'15'       SET MAXIMUM LENGTH
            MOVE 'FKS '            TO FOPGNFL.
            MOVE 'KEQ'             TO FOPMTYP.
            MOVE 'XERIC          ' TO MYRID.
            MOVE 15                TO MYLEN.
      *  MVC   FWTSBA,FOP10C2     MOVE WCC,SBA
            MOVE FOP10C2 TO FOPSBA.
      *  MVC   FWTMSG,FOPTYPE     MOVE MESSAGE TEXT
      *  BAL   R9,SENDIT          SCREEN DISPLAY
            PERFORM SENDIT.
      *  LA    R7,VSAMRTN8        R7=HANDLE RETURN
            MOVE 8 TO DEPCTRL.
      *  MVI   INVREQNO,C'4'      SET 4TH INVREQ
            MOVE 4 TO INVREQNO.
      *  BAL   R9,SB08KEQ         STARTBR MYFILE08 EQUAL
            PERFORM SB08KEQ.
      *  BAL   R9,SB08KEQ         STARTBR MYFILE08 EQUAL
            PERFORM SB08KEQ.

      * VSAMRTN8 EQU   *
        VSAMRTN8.
      *  BAL   R9,ENDBR08         ENDBR MYFILE08
            PERFORM ENDBR08.
      * INVREQ/42    STARTBR NEGATIVE GENERIC KEYLENGTH
      *  MVC   FOPGNFL,=CL4'GEN'  SET GENERIC KEY
      *  MVC   FOPMTYP,=C'KEQ'    SET EQUAL
      *  MVC   MYRID,=CL15'XERIC' RECORD BY KEY
      *  MVC   MYLEN,=H'15'       SET MAXIMUM LENGTH
            MOVE 'GEN '            TO FOPGNFL.
            MOVE 'KEQ'             TO FOPMTYP.
            MOVE 'XERIC          ' TO MYRID.
            MOVE 15                TO MYLEN.
      *  MVC   FWTSBA,FOP12C2     MOVE WCC,SBA
            MOVE FOP12C2 TO FOPSBA.
      *  MVC   FWTMSG,FOPTYPE     MOVE MESSAGE TEXT
      *  BAL   R9,SENDIT          SCREEN DISPLAY
            PERFORM SENDIT.
      *  LA    R7,VSAMRTN9        R7=HANDLE RETURN
            MOVE 9 TO DEPCTRL.
      *  MVI   INVREQNO,C'5'      SET 5TH INVREQ
            MOVE 5 TO INVREQNO.
      *  MVC   MYKL1,=H'-1'       MOVE KEYLENGTH
            MOVE -1 TO MYKL1.
            EXEC CICS STARTBR FILE('MYFILE08') RIDFLD(MYRID)
                      GENERIC EQUAL KEYLENGTH(MYKL1) END-EXEC.

      * VSAMRTN9 EQU   *
        VSAMRTN9.
      *  BAL   R9,ENDBR08         ENDBR MYFILE08
            PERFORM ENDBR08.
      * INVREQ/24    READPREV AND STARTBR GENERIC
      *  MVC   FOPFUNC,=CL8'READPREV' SET FUNCTION IN MESSAGE
      *  MVC   MYRID,=CL15'XERIC' RECORD BY KEY
      *  MVC   MYLEN,=H'15'       SET MAXIMUM LENGTH
            MOVE 'READPREV'        TO FOPFUNC.
            MOVE 'XERIC          ' TO MYRID.
            MOVE 15                TO MYLEN.
      *  MVC   FWTSBA,FOP14C2     MOVE WCC,SBA
            MOVE FOP14C2 TO FOPSBA.
      *  MVC   FWTMSG,FOPTYPE     MOVE MESSAGE TEXT
      *  BAL   R9,SENDIT          SCREEN DISPLAY
            PERFORM SENDIT.
      *  LA    R7,VSAMRTNA        R7=HANDLE RETURN
            MOVE 10 TO DEPCTRL.
      *  MVI   INVREQNO,C'6'      SET 6TH INVREQ
            MOVE 6 TO INVREQNO.
            EXEC CICS STARTBR FILE('MYFILE08') RIDFLD(MYRID)
                      GENERIC EQUAL KEYLENGTH(5) END-EXEC.
            EXEC CICS READPREV FILE('MYFILE08') INTO(MYDATA)
                      LENGTH(MYLEN) RIDFLD(MYRID) END-EXEC.

      * VSAMRTNA EQU   *
        VSAMRTNA.
      *  BAL   R9,ENDBR08         ENDBR MYFILE08
            PERFORM ENDBR08.
      * INVREQ/41    READPREV NO STARTBR REQID
      *  MVC   FOPFUNC,=CL8'READPREV' SET FUNCTION IN MESSAGE
      *  MVC   MYRID,=CL15'XERIC' RECORD BY KEY
      *  MVC   MYLEN,=H'15'       SET MAXIMUM LENGTH
            MOVE 'READPREV'        TO FOPFUNC.
            MOVE 'XERIC          ' TO MYRID.
            MOVE 15                TO MYLEN.
      *  MVC   FWTSBA,FOP16C2     MOVE WCC,SBA
            MOVE FOP16C2 TO FOPSBA.
      *  MVC   FWTMSG,FOPTYPE     MOVE MESSAGE TEXT
      *  BAL   R9,SENDIT          SCREEN DISPLAY
            PERFORM SENDIT.
      *  LA    R7,VSAMRTNB        R7=HANDLE RETURN
            MOVE 11 TO DEPCTRL.
      *  MVI   INVREQNO,C'7'      SET 7TH INVREQ
            MOVE 7 TO INVREQNO.
            EXEC CICS READPREV FILE('MYFILE08') INTO(MYDATA)
                      LENGTH(MYLEN) RIDFLD(MYRID) REQID(5) END-EXEC.

      * VSAMRTNB EQU   *
        VSAMRTNB.
      * ENDFLE 1     READNEXT EQUAL BEYOND END
      *  MVC   FOPFUNC,=CL8'READNEXT' SET FUNCTION IN MESSAGE
      *  MVC   FOPGNFL,=CL4'FKS'  SET FULL KEY
      *  MVC   MYRID,=15X'FF'     RECORD BY KEY
      *  MVC   MYLEN,=H'15'       SET MAXIMUM LENGTH
            MOVE 'READNEXT'  TO FOPFUNC.
            MOVE 'FKS '      TO FOPGNFL.
            MOVE HIGH-VALUES TO MYRID.
            MOVE 15          TO MYLEN.
      *  MVC   FWTSBA,FOP18C2     MOVE WCC,SBA
            MOVE FOP18C2 TO FOPSBA.
      *  MVC   FWTMSG,FOPTYPE     MOVE MESSAGE TEXT
      *  BAL   R9,SENDIT          SCREEN DISPLAY
            PERFORM SENDIT.
      *  LA    R7,VSAMRTNC        R7=HANDLE RETURN
            MOVE 12 TO DEPCTRL.
      *  MVI   ENDFLENO,C'1'      SET 1ST ENDFLE
            MOVE 1 TO ENDFLENO.
      *  BAL   R9,SB08KEQ         STARTBR MYFILE08 EQUAL
            PERFORM SB08KEQ.
            EXEC CICS READNEXT FILE('MYFILE08') INTO(MYDATA)
                      LENGTH(MYLEN) RIDFLD(MYRID) END-EXEC.

      * VSAMRTNC EQU   *
        VSAMRTNC.
      *  BAL   R9,ENDBR08         ENDBR MYFILE08
            PERFORM ENDBR08.
      * ENDFLE 2     READPREV EQUAL BEYOND BEG
      *  MVC   FOPFUNC,=CL8'READPREV' SET FUNCTION IN MESSAGE
      *  MVC   FOPMTYP,=C'KGE'    SET GTEQ
      *  MVC   MYRID,=15X'00'     RECORD BY KEY
      *  MVC   MYLEN,=H'15'       SET MAXIMUM LENGTH
            MOVE 'READPREV' TO FOPFUNC.
            MOVE 'KGE'      TO FOPMTYP.
            MOVE LOW-VALUES TO MYRID.
            MOVE 15         TO MYLEN.
      *  MVC   FWTSBA,FOP20C2     MOVE WCC,SBA
            MOVE FOP20C2 TO FOPSBA.
      *  MVC   FWTMSG,FOPTYPE     MOVE MESSAGE TEXT
      *  BAL   R9,SENDIT          SCREEN DISPLAY
            PERFORM SENDIT.
      *  LA    R7,VSAMRTND        R7=HANDLE RETURN
            MOVE 13 TO DEPCTRL.
      *  MVI   ENDFLENO,C'2'      SET 2ND NOTFND
            MOVE 2 TO ENDFLENO.
            EXEC CICS STARTBR FILE('MYFILE08') RIDFLD(MYRID) GTEQ
                      END-EXEC.
            EXEC CICS READPREV FILE('MYFILE08') INTO(MYDATA)
                      LENGTH(MYLEN) RIDFLD(MYRID) END-EXEC.

      * VSAMRTND EQU   *
        VSAMRTND.
      *  BAL   R9,ENDBR08         ENDBR MYFILE08
            PERFORM ENDBR08.
            EXEC CICS SEND FROM(ENDMSG) LENGTH(ENDMSGL) END-EXEC.
            EXEC CICS RETURN END-EXEC.
      *
      * HANDLE ROUTINES
      *
      * VSAMINV  EQU   *
        VSAMINV.
      *  LA    R9,INVREQTB        ADDRESS SBA TABLE
      * VSAMINVL EQU   *
      *  CLC   INVREQNO,0(R9)     MATCH ?
      *  BNE   VSAMINVI           EXIT IF NOT
      *  MVC   FWTSBA,1(R9)       MOVE WCC,SBA
      *  B     INVREQGO           EXIT

      * VSAMINVI EQU   *
      *  AHI   R9,5               BUMP POINTER
      *  CLI   0(R9),X'FF'        STOPPER ?
      *  BNE   VSAMINVL           LOOP IF NOT
      *  DC    H'0'               PROGRAMMING ERROR
            GO TO INVSBA1 INVSBA2 INVSBA3 INVSBA4
                  INVSBA5 INVSBA6 INVSBA7
                  DEPENDING ON INVREQNO.
            GO TO ABEND.

        INVSBA1.
            MOVE X'C311D440' TO FHASBA.
            GO TO INVREQGO.
        INVSBA2.
            MOVE X'C311D660' TO FHASBA.
            GO TO INVREQGO.
        INVSBA3.
            MOVE X'C3114DC9' TO FHASBA.
            GO TO INVREQGO.
        INVSBA4.
            MOVE X'C3114FE9' TO FHASBA.
            GO TO INVREQGO.
        INVSBA5.
            MOVE X'C311D2C9' TO FHASBA.
            GO TO INVREQGO.
        INVSBA6.
            MOVE X'C311D4E9' TO FHASBA.
            GO TO INVREQGO.
        INVSBA7.
            MOVE X'C311D7C9' TO FHASBA.
      * INVREQGO EQU   *
        INVREQGO.
      *  L     R9,EIBRESP2        R9=RESP2
      *  CVD   R9,DEC             CONVERT TO DECIMAL
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

      * INVREQTB DS    0H
      *  DC    C'1'
      *  TN3270 WCC_KD,(SBA,17,1)
      *  DC    C'2'
      *  TN3270 WCC_KD,(SBA,19,1)
      *  DC    C'3'
      *  TN3270 WCC_KD,(SBA,11,42)
      *  DC    C'4'
      *  TN3270 WCC_KD,(SBA,13,42)
      *  DC    C'5'
      *  TN3270 WCC_KD,(SBA,15,42)
      *  DC    C'6'
      *  TN3270 WCC_KD,(SBA,17,42)
      *  DC    C'7'
      *  TN3270 WCC_KD,(SBA,19,42)
      *  DC    X'FF'              STOPPER

      * VSAMNOTF EQU   *
        VSAMNOTF.
      *  LA    R9,NOTFNDTB        ADDRESS SBA TABLE
      * VSAMNTFL EQU   *
      *  CLC   NOTFNDNO,0(R9)     MATCH ?
      *  BNE   VSAMNTFI           EXIT IF NOT
      *  MVC   FWTSBA,1(R9)       MOVE WCC,SBA
      *  B     NOTFNDGO           EXIT

      * VSAMNTFI EQU   *
      *  AHI   R9,5               BUMP POINTER
      *  CLI   0(R9),X'FF'        STOPPER ?
      *  BNE   VSAMNTFL           LOOP IF NOT
      *  DC    H'0'               PROGRAMMING ERROR
            GO TO NTFSBA1 NTFSBA2 NTFSBA3 NTFSBA4
                  DEPENDING ON NOTFNDNO.
            GO TO ABEND.

        NTFSBA1.
            MOVE X'C311D940' TO FHASBA.
            GO TO NOTFNDGO.
        NTFSBA2.
            MOVE X'C3115B60' TO FHASBA.
            GO TO NOTFNDGO.
        NTFSBA3.
            MOVE X'C311C4D9' TO FHASBA.
            GO TO NOTFNDGO.
        NTFSBA4.
            MOVE X'C311C6F9' TO FHASBA.
      * NOTFNDGO EQU   *
        NOTFNDGO.
      *  MVC   FHANDCON,=CL12'NOTFND' MOVE CONDITION
            MOVE 'NOTFND      ' TO FHANDCON.
      *  BAL   R9,HEXFDBK         DISPLAY FEEDBACK CODES
            PERFORM HEXFDBK.
      *  B     FWTHSEND           EXIT
            GO TO FWTHSEND.

      * NOTFNDTB DS    0H
      *  DC    C'1'
      *  TN3270 WCC_KD,(SBA,21,1)
      *  DC    C'2'
      *  TN3270 WCC_KD,(SBA,23,1)
      *  DC    C'3'
      *  TN3270 WCC_KD,(SBA,4,42)
      *  DC    C'4'
      *  TN3270 WCC_KD,(SBA,6,42)
      *  DC    X'FF'              STOPPER

      * VSAMENDF DS    0H
        VSAMENDF.
      *  LA    R9,ENDFLETB        ADDRESS SBA TABLE
      * VSAMENDL EQU   *
      *  CLC   ENDFLENO,0(R9)     MATCH ?
      *  BNE   VSAMENDI           EXIT IF NOT
      *  MVC   FWTSBA,1(R9)       MOVE WCC,SBA
      *  B     ENDFLEGO           EXIT

      * VSAMENDI EQU   *
      *  AHI   R9,5               BUMP POINTER
      *  CLI   0(R9),X'FF'        STOPPER ?
      *  BNE   VSAMENDL           LOOP IF NOT
      *  DC    H'0'               PROGRAMMING ERROR
            GO TO ENDSBA1 ENDSBA2 DEPENDING ON ENDFLENO.
            GO TO ABEND.

        ENDSBA1.
            MOVE X'C311D9E9' TO FHASBA.
            GO TO ENDFLEGO.
        ENDSBA2.
            MOVE X'C3115CC9' TO FHASBA.
      * ENDFLEGO EQU   *
        ENDFLEGO.
      *  MVC   FHANDCON,=CL12'ENDFLE' MOVE CONDITION
            MOVE 'ENDFLE      ' TO FHANDCON.
      *  BAL   R9,HEXFDBK         DISPLAY FEEDBACK CODES
            PERFORM HEXFDBK.
      *  B     FWTHSEND           EXIT

      * ENDFLETB DS    0H
      *  DC    C'1'
      *  TN3270 WCC_KD,(SBA,21,42)
      *  DC    C'2'
      *  TN3270 WCC_KD,(SBA,23,42)
      *  DC    X'FF'              STOPPER

      * FWTHSEND EQU   *
        FWTHSEND.
      *  MVC   FWTMSG,FHANDCON    MOVE MESSAGE TEXT
            EXEC CICS SEND FROM(FHAND) LENGTH(FHANDL) END-EXEC.
      *  BR    R7                 RETURN TO MAIN LINE
            GO TO VSAMRTN1 VSAMRTN2 VSAMRTN3 VSAMRTN4 VSAMRTN5
                  VSAMRTN6 VSAMRTN7 VSAMRTN8 VSAMRTN9 VSAMRTNA
                  VSAMRTNB VSAMRTNC VSAMRTND ABEND
                  DEPENDING ON DEPCTRL.

        ABEND.
            EXEC CICS ABEND ABCODE('VSC3') END-EXEC.

      *
      * SUBROUTINES
      *
      * SENDIT   DS    0H
        SENDIT.
            EXEC CICS SEND FROM(FOPLINE) LENGTH(FOPL) END-EXEC.
      *  BR    R9                 RETURN

      * RDFL08   DS    0H
        RDFL08.
      *  MVC   MYLEN,=H'15'       SET MAXIMUM LENGTH
            MOVE 15 TO MYLEN.
            EXEC CICS READ FILE('MYFILE08') INTO(MYDATA) LENGTH(MYLEN)
                      RIDFLD(MYRID) END-EXEC.
      *  BR    R9                 RETURN

      * RDFL09   DS    0H
        RDFL09.
      *  MVC   MYLEN,=H'35'       SET MAXIMUM LENGTH
            MOVE 35 TO MYLEN.
            EXEC CICS READ FILE('MYFILE09') INTO(MYDATA) LENGTH(MYLEN)
                      RIDFLD(MYRID) END-EXEC.
      *  BR    R9                 RETURN

      * SB08KEQ  DS    0H
        SB08KEQ.
            EXEC CICS STARTBR FILE('MYFILE08') RIDFLD(MYRID) EQUAL
                      END-EXEC.
      *  BR    R9                 RETURN

      * ENDBR08  DS    0H
        ENDBR08.
            EXEC CICS ENDBR FILE('MYFILE08') END-EXEC.
      *  BR    R9                 RETURN

      * WQTS     DS    0H
        WQTS.
            EXEC CICS WRITEQ TS QUEUE('VSC3') FROM(MYDATA)
                      LENGTH(MYLEN) END-EXEC.
      * BR    R9                 RETURN

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
      *  BR    R9                 RETURN
