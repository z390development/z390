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
      * TEST BED 4 IN COBOL
      *
      * TEST HANDLE AID AND DUMP
      *
        PROGRAM-ID. TESTBEC4.
        ENVIRONMENT DIVISION.

        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01  WORDSEND.
          02  FILLER   PIC X    VALUE X'C3'.
          02  WORDMSG.
              03  FILLER   PIC X(10).
              03  WORDLEN  PIC 9(4) COMP.
        77  KEYINDS  PIC X(18).
        77  KEYINTAB REDEFINES KEYINDS OCCURS 18 TIMES PIC X.
        77  KEYIDX   PIC 9(4) COMP.
        77  RECAREA  PIC X(80).
        77  RECLEN   PIC 9(4) COMP.
        77  DUMP1    PIC X(6)   VALUE 'ABCDEF'.
        77  DUMP2    PIC X(4)   VALUE 'STUV'.
        77  DUMP3    PIC X(11)  VALUE 'PQRSTUVWXYZ'.
        77  DUMP4    PIC X(26)  VALUE 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'.
        77  DUMP5    PIC X(22)  VALUE 'ALPHA BETA GAMMA DELTA'.
        01  KEYTABLE PIC X(19)
                     VALUE X'F1F2F3F4F5F6F7F8F97A7B7CC1C2C3C4C5C6FF'.
        01  KEYTAB REDEFINES KEYTABLE.
            02  KEYS  OCCURS 19 TIMES PIC X.
        01  HEAD.
            02  FILLER  PIC X(6)  VALUE X'27F5C311C1D1'.
            02  FILLER  PIC X(24) VALUE 'TESTBEC4 TEST HANDLE AID'.
            02  FILLER  PIC X(3)  VALUE X'11C261'.
            02  FILLER  PIC X(31)
                        VALUE 'PRESS PF KEYS 1-18 IN ANY ORDER'.
            02  FILLER  PIC X(3)  VALUE X'11C3F1'.
            02  FILLER  PIC X(25) VALUE 'CLEAR WILL END TASK EARLY'.
        77  HEADL    PIC 9(4) COMP.
        01  TAIL.
            02  FILLER  PIC X(4)  VALUE X'C3114C61'.
            02  FILLER  PIC X(34)
                        VALUE 'TESTBEC4 ENDED OK, NOW PRESS CLEAR'.
        77  TAILL    PIC 9(4) COMP.
        01  WORDS.
            02  FILLER  PIC X(3)  VALUE X'11C6E7'.
            02  FILLER  PIC X(7)  VALUE 'that'.
            02  FILLER  PIC 9(4)  VALUE 8  COMP.
            02  FILLER  PIC X(3)  VALUE X'11C7F4'.
            02  FILLER  PIC X(7)  VALUE 'the'.
            02  FILLER  PIC 9(4)  VALUE 7  COMP.
            02  FILLER  PIC X(3)  VALUE X'11C6D8'.
            02  FILLER  PIC X(7)  VALUE 'go'.
            02  FILLER  PIC 9(4)  VALUE 6  COMP.
            02  FILLER  PIC X(3)  VALUE X'11C7E7'.
            02  FILLER  PIC X(7)  VALUE 'rage'.
            02  FILLER  PIC 9(4)  VALUE 8  COMP.
            02  FILLER  PIC X(3)  VALUE X'114AC7'.
            02  FILLER  PIC X(7)  VALUE 'Thomas'.
            02  FILLER  PIC 9(4)  VALUE 10 COMP.
            02  FILLER  PIC X(3)  VALUE X'11C77E'.
            02  FILLER  PIC X(7)  VALUE 'of'.
            02  FILLER  PIC 9(4)  VALUE 6  COMP.
            02  FILLER  PIC X(3)  VALUE X'11C6D1'.
            02  FILLER  PIC X(7)  VALUE 'Do'.
            02  FILLER  PIC 9(4)  VALUE 6  COMP.
            02  FILLER  PIC X(3)  VALUE X'11C6F1'.
            02  FILLER  PIC X(7)  VALUE 'night,'.
            02  FILLER  PIC 9(4)  VALUE 10 COMP.
            02  FILLER  PIC X(3)  VALUE X'11C76C'.
            02  FILLER  PIC X(7)  VALUE 'against'.
            02  FILLER  PIC 9(4)  VALUE 11 COMP.
            02  FILLER  PIC X(3)  VALUE X'11C6E2'.
            02  FILLER  PIC X(7)  VALUE 'into'.
            02  FILLER  PIC 9(4)  VALUE 8  COMP.
            02  FILLER  PIC X(3)  VALUE X'11C8C5'.
            02  FILLER  PIC X(7)  VALUE 'light.'.
            02  FILLER  PIC 9(4)  VALUE 10 COMP.
            02  FILLER  PIC X(3)  VALUE X'11C761'.
            02  FILLER  PIC X(7)  VALUE 'Rage,'.
            02  FILLER  PIC 9(4)  VALUE 9  COMP.
            02  FILLER  PIC X(3)  VALUE X'114AC1'.
            02  FILLER  PIC X(7)  VALUE 'Dylan'.
            02  FILLER  PIC 9(4)  VALUE 9  COMP.
            02  FILLER  PIC X(3)  VALUE X'11C66C'.
            02  FILLER  PIC X(7)  VALUE 'good'.
            02  FILLER  PIC 9(4)  VALUE 8  COMP.
            02  FILLER  PIC X(3)  VALUE X'11C8C1'.
            02  FILLER  PIC X(7)  VALUE 'the'.
            02  FILLER  PIC 9(4)  VALUE 7  COMP.
            02  FILLER  PIC X(3)  VALUE X'11C65B'.
            02  FILLER  PIC X(7)  VALUE 'gentle'.
            02  FILLER  PIC 9(4)  VALUE 10 COMP.
            02  FILLER  PIC X(3)  VALUE X'11C7F8'.
            02  FILLER  PIC X(7)  VALUE 'dying'.
            02  FILLER  PIC 9(4)  VALUE 9  COMP.
            02  FILLER  PIC X(3)  VALUE X'11C6D4'.
            02  FILLER  PIC X(7)  VALUE 'not'.
            02  FILLER  PIC 9(4)  VALUE 7  COMP.
        01  FILLER REDEFINES WORDS.
            02  WORDENT OCCURS 18 TIMES PIC X(12).
        01  SEGS.
            02  ADUMP1  PIC 9(9) COMP USAGE IS POINTER.
            02  ADUMP2  PIC 9(9) COMP USAGE IS POINTER.
            02  ADUMP3  PIC 9(9) COMP USAGE IS POINTER.
            02  ADUMP4  PIC 9(9) COMP USAGE IS POINTER.
            02  ADUMP5  PIC 9(9) COMP USAGE IS POINTER.
        01  LENS.
            02  LDUMP1  PIC 9(9) COMP.
            02  LDUMP2  PIC 9(9) COMP.
            02  LDUMP3  PIC 9(9) COMP.
            02  LDUMP4  PIC 9(9) COMP.
            02  LDUMP5  PIC 9(9) COMP.
        COPY DFHAID.

        PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA.

            MOVE LENGTH OF HEAD TO HEADL.
            MOVE LENGTH OF TAIL TO TAILL.
      *  MVC   KEYINDS,=18X'FF'   MARK ALL KEYS UNPRESSED
            MOVE HIGH-VALUES TO KEYINDS.
      *  MVC   WORDWCC,WCC        SET WORD WCC
            EXEC CICS SEND FROM(HEAD) LENGTH(HEADL) END-EXEC.
      *
      * HANDLE PF KEYS 1-18
      *
            EXEC CICS HANDLE AID
                 ANYKEY(PROCKEY)
                 CLEAR PA1 PA2 PA3
                 PF19 PF20 PF21 PF22 PF23 PF24
                 END-EXEC.
      *
        KEYPRESS.
            MOVE 1 TO KEYIDX.
            EXEC CICS RECEIVE INTO(RECAREA) LENGTH(RECLEN) END-EXEC.
      *  CLI   EIBAID,CLEAR       CLEAR PRESSED ?
      *  BE    RETURN             END TASK IF IT IS
      *  B     KEYPRESS           LOOP
            IF EIBAID = DFHCLEAR THEN GO TO ALLDONE
            ELSE GO TO KEYPRESS.
      *
      *  LA    R3,KEYTAB          ADDRESS KEY TABLE
      *  SR    R4,R4              CLEAR R4
      *  KEYLOOP  EQU   *
      *  CLC   0(1,R3),EIBAID     FOUND AID ?
      *  BE    GETWORD            EXIT TO PROCESS IT
      *  AHI   R4,1               BUMP INDEX
      *  AHI   R3,1               BUMP TABLE POINTER
      *  CLI   0(R3),X'FF'        STOPPER ?
      *  BE    ABEND              EXIT IF BAD AID
      *  B     KEYLOOP

        PROCKEY.
            IF EIBAID = KEYS (KEYIDX) THEN GO TO GETWORD.
            ADD 1 TO KEYIDX
            IF KEYS (KEYIDX) = X'FF' THEN GO TO ABEND.
            GO TO PROCKEY.

      * GETWORD  EQU   *
        GETWORD.
      * GOT A VALID AID
      *  LA    R5,KEYINDS(R4)     INDEX TO KEY INDICATORS
      *  MVI   0(R5),X'00'        RESET INDICATOR
            MOVE LOW-VALUE TO KEYINTAB (KEYIDX).
      *  MS    R4,=F'12'          INDEX TO WORD LIST
      *  LA    R4,WORDS(R4)       + WORD BASE
      *  MVC   WORDMSG(12),0(R4)  MOVE WORD
            MOVE WORDENT (KEYIDX) TO WORDMSG.
            EXEC CICS SEND FROM(WORDSEND) LENGTH(WORDLEN) END-EXEC.
      *  OC    KEYINDS,KEYINDS    ALL DONE YET ?
      *  BNZ   KEYPRESS           LOOP
            IF KEYINDS NOT = LOW-VALUES THEN GO TO KEYPRESS.
            EXEC CICS SEND FROM(TAIL) LENGTH(TAILL) END-EXEC.

        ALLDONE.
      * TEST DUMP
            EXEC CICS DUMP TRANSACTION DUMPCODE('MEL1') COMPLETE
                 END-EXEC.
            MOVE ADDRESS OF DUMP1 TO ADUMP1.
            MOVE ADDRESS OF DUMP2 TO ADUMP2.
            MOVE ADDRESS OF DUMP3 TO ADUMP3.
            MOVE ADDRESS OF DUMP4 TO ADUMP4.
            MOVE ADDRESS OF DUMP5 TO ADUMP5.
            MOVE LENGTH OF DUMP1 TO LDUMP1.
            MOVE LENGTH OF DUMP2 TO LDUMP2.
            MOVE LENGTH OF DUMP3 TO LDUMP3.
            MOVE LENGTH OF DUMP4 TO LDUMP4.
            MOVE LENGTH OF DUMP5 TO LDUMP5.
            EXEC CICS DUMP TRANSACTION DUMPCODE('MEL2')
                 FROM(KEYTAB) LENGTH(19)
                 SEGMENTLIST(SEGS) LENGTHLIST(LENS) NUMSEGMENTS(5)
                 END-EXEC.
            EXEC CICS RETURN END-EXEC.

        ABEND.
            EXEC CICS ABEND ABCODE('BEC4') END-EXEC.
