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
      * TEST HANDLE AID/PUSH/POP
      * TEST GETMAIN/FREEMAIN
      *
        PROGRAM-ID. TESTBEC5.
        ENVIRONMENT DIVISION.

        DATA DIVISION.
        WORKING-STORAGE SECTION.
        77  GET1     PIC 9(9) COMP USAGE IS POINTER.
        77  GET2     PIC 9(9) COMP USAGE IS POINTER.
        77  GET3     PIC 9(9) COMP USAGE IS POINTER.
        77  GET4     PIC 9(9) COMP USAGE IS POINTER.
        77  GET5     PIC 9(9) COMP USAGE IS POINTER.
        77  GETBAD   PIC 9(9) COMP USAGE IS POINTER.
        77  RECAREA  PIC X(80).
        77  RECLEN   PIC 9(4) COMP.
        01  HEAD.
            02  FILLER  PIC X(6)  VALUE X'27F5C311C1D1'.
            02  FILLER  PIC X(51)
           VALUE 'TESTBEC5 TEST HANDLE, PUSH/POP AND GETMAIN/FREEMAIN'.
            02  FILLER  PIC X(3)  VALUE X'11C261'.
            02  FILLER  PIC X(14) VALUE 'PRESS PF KEY 4'.
            02  FILLER  PIC X(3)  VALUE X'11C3F1'.
            02  FILLER  PIC X(25) VALUE 'CLEAR WILL END TASK EARLY'.
        77  HEADL    PIC 9(4) COMP.
        01  BADMSG1.
            02  FILLER  PIC X(4)  VALUE X'C311C5C1'.
            02  FILLER  PIC X(19) VALUE 'THAT KEY WASN''T PF4'.
        77  BADMSG1L PIC 9(4) COMP.
        01  BADMSG2.
            02  FILLER  PIC X(4)  VALUE X'C311C761'.
            02  FILLER  PIC X(19) VALUE 'THAT KEY WASN''T PF5'.
        77  BADMSG2L PIC 9(4) COMP.
        01  BADMSG3.
            02  FILLER  PIC X(4)  VALUE X'C3114AC1'.
            02  FILLER  PIC X(19) VALUE 'THAT KEY WASN''T PF6'.
        77  BADMSG3L PIC 9(4) COMP.
        01  PF4MSG.
            02  FILLER  PIC X(4)  VALUE X'C311C5C1'.
            02  FILLER  PIC X(20) VALUE 'I GOT PF4, THANKYOU '.
        77  PF4MSGL  PIC 9(4) COMP.
        01  PF5ASK.
            02  FILLER  PIC X(4)  VALUE X'C311C6D1'.
            02  FILLER  PIC X(14) VALUE 'PRESS PF KEY 5'.
        77  PF5ASKL  PIC 9(4) COMP.
        01  PF5MSG.
            02  FILLER  PIC X(4)  VALUE X'C311C761'.
            02  FILLER  PIC X(20) VALUE 'I GOT PF5, THANKYOU '.
        77  PF5MSGL  PIC 9(4) COMP.
        01  PF6ASK.
            02  FILLER  PIC X(4)  VALUE X'C311C8F1'.
            02  FILLER  PIC X(14) VALUE 'PRESS PF KEY 6'.
        77  PF6ASKL  PIC 9(4) COMP.
        01  PF6MSG.
            02  FILLER  PIC X(4)  VALUE X'C3114AC1'.
            02  FILLER  PIC X(20) VALUE 'I GOT PF6, THANKYOU '.
        77  PF6MSGL  PIC 9(4) COMP.
        01  PGMMSG.
            02  FILLER  PIC X(4)  VALUE X'C3114BD1'.
            02  FILLER  PIC X(27) VALUE 'PGMIDERR WILL NOW BE RAISED'.
        77  PGMMSGL  PIC 9(4) COMP.
        01  PGMMSGE.
            02  FILLER  PIC X(4)  VALUE X'C3114C61'.
            02  FILLER  PIC X(29)
                        VALUE 'PGMIDERR CONDITION HANDLED OK'.
        77  PGMMSGEL PIC 9(4) COMP.
        01  PGMMSG2.
            02  FILLER  PIC X(4)  VALUE X'C3114DF1'.
            02  FILLER  PIC X(33)
                        VALUE 'PGMIDERR WILL NOW BE RAISED AGAIN'.
        77  PGMMSG2L PIC 9(4) COMP.
        01  ERRMSG.
            02  FILLER  PIC X(4)  VALUE X'C3114FC1'.
            02  FILLER  PIC X(26) VALUE 'ERROR CONDITION HANDLED OK'.
        77  ERRMSGL  PIC 9(4) COMP.
        01  IGNMSG.
            02  FILLER  PIC X(4)  VALUE X'C31150D1'.
            02  FILLER  PIC X(27) VALUE 'IGNORE CONDITION HANDLED OK'.
        77  IGNMSGL  PIC 9(4) COMP.
        01  GTMMSG.
            02  FILLER  PIC X(4)  VALUE X'C311D161'.
            02  FILLER  PIC X(40)
                     VALUE 'GETMAIN/FREEMAIN TESTING, OK IF NO ABEND'.
        77  GTMMSGL  PIC 9(4) COMP.
        01  ABDMSG.
            02  FILLER  PIC X(4)  VALUE X'C311D2F1'.
            02  FILLER  PIC X(34)
                        VALUE 'PGMIDERR WILL NOW CAUSE ABEND AEI0'.
            02  FILLER  PIC X(3)  VALUE X'11D4C1'.
            02  FILLER  PIC X(11) VALUE 'PRESS CLEAR'.
        77  ABDMSGL  PIC 9(4) COMP.
        77  INIT5    PIC X    VALUE X'05'.
        COPY DFHAID.

        PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA.

            MOVE LENGTH OF HEAD    TO HEADL.
            MOVE LENGTH OF BADMSG1 TO BADMSG1L.
            MOVE LENGTH OF BADMSG2 TO BADMSG2L.
            MOVE LENGTH OF BADMSG3 TO BADMSG3L.
            MOVE LENGTH OF PF4MSG  TO PF4MSGL.
            MOVE LENGTH OF PF5ASK  TO PF5ASKL.
            MOVE LENGTH OF PF5MSG  TO PF5MSGL.
            MOVE LENGTH OF PF6ASK  TO PF6ASKL.
            MOVE LENGTH OF PF6MSG  TO PF6MSGL.
            MOVE LENGTH OF PGMMSG  TO PGMMSGL.
            MOVE LENGTH OF PGMMSGE TO PGMMSGEL.
            MOVE LENGTH OF PGMMSG2 TO PGMMSG2L.
            MOVE LENGTH OF ERRMSG  TO ERRMSGL.
            MOVE LENGTH OF IGNMSG  TO IGNMSGL.
            MOVE LENGTH OF GTMMSG  TO GTMMSGL.
            MOVE LENGTH OF ABDMSG  TO ABDMSGL.
            EXEC CICS SEND FROM(HEAD) LENGTH(HEADL) END-EXEC.

      * CREATE AND MODIFY 1ST AID BLOCK
            EXEC CICS HANDLE AID
                 ENTER(BADKEY1)
                 ANYKEY(BADKEY1)
                 PF4(GOTPF4) CLEAR
                 END-EXEC.

      *  KEYPRES1 EQU   *
        KEYPRES1.
            EXEC CICS RECEIVE INTO(RECAREA) LENGTH(RECLEN) END-EXEC.
      *  CLI   EIBAID,DFHCLEAR    CLEAR PRESSED ?
      *  BE    RETURN             END TASK IF IT IS
      *  B     KEYPRES1           LOOP
            IF EIBAID = DFHCLEAR THEN GO TO ALLDONE
            ELSE GO TO KEYPRES1.

      * BADKEY1  EQU   *
        BADKEY1.
            EXEC CICS SEND FROM(BADMSG1) LENGTH(BADMSG1L) END-EXEC.
      *  B     KEYPRES1           WAIT FOR NEXT KEY
            GO TO KEYPRES1.

      * GOTPF4   EQU   *
        GOTPF4.
            EXEC CICS SEND FROM(PF4MSG) LENGTH(PF4MSGL) END-EXEC.
            EXEC CICS SEND FROM(PF5ASK) LENGTH(PF5ASKL) END-EXEC.
      * CREATE 2ND AID BLOCK
            EXEC CICS PUSH HANDLE END-EXEC.
      * MODIFY 2ND AID BLOCK
            EXEC CICS HANDLE AID
                 ENTER(BADKEY2)
                 ANYKEY(BADKEY2)
                 PF5(GOTPF5) CLEAR
                 END-EXEC.

      * KEYPRES2 EQU   *
        KEYPRES2.
            EXEC CICS RECEIVE INTO(RECAREA) LENGTH(RECLEN) END-EXEC.
      *  CLI   EIBAID,DFHCLEAR    CLEAR PRESSED ?
      *  BE    RETURN             END TASK IF IT IS
      *  B     KEYPRES2           LOOP
            IF EIBAID = DFHCLEAR THEN GO TO ALLDONE
            ELSE GO TO KEYPRES2.

      * BADKEY2  EQU   *
        BADKEY2.
            EXEC CICS SEND FROM(BADMSG2) LENGTH(BADMSG2L) END-EXEC.
      *  B     KEYPRES2           WAIT FOR NEXT KEY
            GO TO KEYPRES2.

      * GOTPF5   EQU   *
        GOTPF5.
            EXEC CICS SEND FROM(PF5MSG) LENGTH(PF5MSGL) END-EXEC.
            EXEC CICS SEND FROM(PF6ASK) LENGTH(PF6ASKL) END-EXEC.
      * DELETE 2ND AID BLOCK
            EXEC CICS POP HANDLE END-EXEC.
      * MODIFY 1ST AID BLOCK
            EXEC CICS HANDLE AID
                 ENTER(BADKEY3)
                 ANYKEY(BADKEY3)
                 PF4 PF6(GOTPF6) CLEAR
                 END-EXEC.

      * KEYPRES3 EQU   *
        KEYPRES3.
            EXEC CICS RECEIVE INTO(RECAREA) LENGTH(RECLEN) END-EXEC.
      *  CLI   EIBAID,DFHCLEAR    CLEAR PRESSED ?
      *  BE    RETURN             END TASK IF IT IS
      *  B     KEYPRES3           LOOP
            IF EIBAID = DFHCLEAR THEN GO TO ALLDONE
            ELSE GO TO KEYPRES3.

      * BADKEY3  EQU   *
        BADKEY3.
            EXEC CICS SEND FROM(BADMSG3) LENGTH(BADMSG3L) END-EXEC.
      *  B     KEYPRES3           WAIT FOR NEXT KEY
            GO TO KEYPRES3.

      * GOTPF6   EQU   *
        GOTPF6.
            EXEC CICS SEND FROM(PF6MSG) LENGTH(PF6MSGL) END-EXEC.
      * DELETE 1ST AID BLOCK
            EXEC CICS POP HANDLE END-EXEC.
      * CREATE AND MODIFY 1ST CONDITION BLOCK
            EXEC CICS HANDLE CONDITION PGMIDERR(GOTPGMER) END-EXEC.
            EXEC CICS SEND FROM(PGMMSG) LENGTH(PGMMSGL) END-EXEC.
      * THIS SHOULD RAISE PGMIDERR
            EXEC CICS LINK PROGRAM('BADPROG1') END-EXEC.

      * NEXT1    EQU   *
        NEXT1.
      * CREATE 2ND CONDITION BLOCK
            EXEC CICS PUSH HANDLE END-EXEC.
      * MODIFY 2ND CONDITION BLOCK
            EXEC CICS HANDLE CONDITION ERROR(GOTERROR) END-EXEC.
            EXEC CICS SEND FROM(PGMMSG2) LENGTH(PGMMSG2L) END-EXEC.
      * THIS SHOULD RAISE PGMIDERR, BUT HANDLED BY ERROR
            EXEC CICS LINK PROGRAM('BADPROG2') END-EXEC.

      * NEXT2    EQU   *
        NEXT2.
            EXEC CICS HANDLE CONDITION ERROR END-EXEC.
            EXEC CICS IGNORE CONDITION PGMIDERR END-EXEC.
      * THIS SHOULD BE IGNORED
            EXEC CICS LINK PROGRAM('BADPROG3') END-EXEC.
            EXEC CICS SEND FROM(IGNMSG) LENGTH(IGNMSGL) END-EXEC.
            EXEC CICS HANDLE CONDITION PGMIDERR END-EXEC.
      * THIS SHOULD SUCCEED
            EXEC CICS LINK PROGRAM('MMMMCIC2') END-EXEC.
      * GETMAIN/FREEMAIN TESTS
            EXEC CICS GETMAIN SET(GET1) LENGTH(100) INITIMG(1)
                      END-EXEC.
      *  ST    R5,GET1            SAVE ADDRESS
            EXEC CICS GETMAIN SET(GET2) LENGTH(200) INITIMG(X'02')
                      END-EXEC.
      *  ST    R5,GET2            SAVE ADDRESS
            EXEC CICS GETMAIN SET(GET3) LENGTH(300) INITIMG(B'11')
                      END-EXEC.
      *  ST    R5,GET3            SAVE ADDRESS
            EXEC CICS GETMAIN SET(GET4) LENGTH(400) INITIMG(4)
                      END-EXEC.
      *  ST    R5,GET4            SAVE ADDRESS
            EXEC CICS GETMAIN SET(GET5) LENGTH(500) INITIMG(INIT5)
                      END-EXEC.
      *  ST    R5,GET5            SAVE ADDRESS
      * FREEMAIN IN RANDOM ORDER...42153
      *  L     R5,GET4            ADDRESS TO FREEMAIN
            EXEC CICS FREEMAIN DATAPOINTER(GET4) END-EXEC.
            EXEC CICS FREEMAIN DATAPOINTER(GET2) END-EXEC.
      *  L     R5,GET1            ADDRESS TO FREEMAIN
            EXEC CICS FREEMAIN DATAPOINTER(GET1) END-EXEC.
            EXEC CICS FREEMAIN DATAPOINTER(GET5) END-EXEC.
            EXEC CICS FREEMAIN DATAPOINTER(GET3) END-EXEC
            EXEC CICS IGNORE CONDITION INVREQ END-EXEC.
      *  L     R5,=4X'FF'         INVALID ADDRESS
            EXEC CICS FREEMAIN DATAPOINTER(GETBAD) END-EXEC.
            EXEC CICS SEND FROM(GTMMSG) LENGTH(GTMMSGL) END-EXEC.

            EXEC CICS SEND FROM(ABDMSG) LENGTH(ABDMSGL) END-EXEC
      * THIS SHOULD ABEND PGMIDERR (AEI0)
            EXEC CICS LINK PROGRAM('BADPROG4') END-EXEC.

      * RETURN   EQU   *
        ALLDONE.
            EXEC CICS RETURN END-EXEC.

      * GOTPGMER DS    0H
        GOTPGMER.
            EXEC CICS SEND FROM(PGMMSGE) LENGTH(PGMMSGEL) END-EXEC.
      *  B     NEXT1              EXIT
            GO TO NEXT1.

      * GOTERROR DS    0H
        GOTERROR.
            EXEC CICS SEND FROM(ERRMSG) LENGTH(ERRMSGL) END-EXEC.
      *  B     NEXT2              EXIT
            GO TO NEXT2.
