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
      * TEST HANDLE ABEND
      *
      * TEST 1: HANDLE ASRA ABEND BY LABEL
      *         PRETEND NOTHING HAPPENED
      *
      * TEST 2: RAISE PGMIDERR
      *         HANDLE AEI0 ABEND BY LABEL
      *         PRETEND NOTHING HAPPENED
      *
      * TEST 3: HANDLE ASRA ABEND BY PROGRAM TESTBEC7
      *         PRETEND NOTHING HAPPENED
      *
      * THE FOLLOWING TESTS ARE DRIVEN BY TRANSID BEC5
      *
      * TEST 4: SET HANDLE ABEND LABEL HERE
      *         LINK TO TESTBEC7 AND ABEND ASRA
      *         RETURN IS AFTER OUR LINK
      *
      * TEST 5: LINK TO TESTBEC7 AND HANDLE ABEND AEI0 THERE BY PROGRAM
      *         TESTBEC8
      *         RETURN IS AFTER OUR LINK
      *
      * TEST 6: EXEC CICS ABEND WITH DUMP HANDLED HERE BY LABEL
      *         PRETEND NOTHING HAPPENED
      *
        PROGRAM-ID. TESTBEC6.
        ENVIRONMENT DIVISION.

        DATA DIVISION.
        WORKING-STORAGE SECTION.
        77  RECAREA  PIC X(80).
        77  RECLEN   PIC 9(4) COMP.
        01  TEST1.
            02  FILLER  PIC X(4)  VALUE X'C311C1D1'.
            02  FILLER  PIC X(59)
                VALUE 'TESTBEC6 TEST 1:PRESS AID KEY TO HANDLE ASRA ABEN
      -               'D BY LABEL'.
        77  TEST1L   PIC 9(4) COMP.
        01  TEST1R.
            02  FILLER  PIC X(4)  VALUE X'C311C261'.
            02  FILLER  PIC X(33)
                        VALUE 'TESTBEC6 ASRA HANDLED OK BY LABEL'.
            02  FILLER  PIC X(3)  VALUE X'11C3F1'.
            02  FILLER  PIC X(59)
                VALUE 'TESTBEC6 TEST 2:PRESS AID KEY TO HANDLE AEI0 ABEN
      -               'D BY LABEL'.
        77  TEST1RL  PIC 9(4) COMP.
        01  TEST2R.
            02  FILLER  PIC X(4)  VALUE X'C311C5C1'.
            02  FILLER  PIC X(33)
                        VALUE 'TESTBEC6 AEI0 HANDLED OK BY LABEL'.
            02  FILLER  PIC X(3)  VALUE X'11C6D1'.
            02  FILLER  PIC X(61)
                VALUE 'TESTBEC6 TEST 3:PRESS AID KEY TO HANDLE ASRA ABEN
      -               'D BY PROGRAM'.
        77  TEST2RL  PIC 9(4) COMP.
        01  TEST4.
            02  FILLER  PIC X(4)  VALUE X'C311C1D1'.
            02  FILLER  PIC X(77)
                VALUE 'TESTBEC6 TEST 4:PRESS AID KEY TO HANDLE ASRA ABEN
      -               'D IN LINKED PROGRAM BY LABEL'.
        77  TEST4L   PIC 9(4) COMP.
        01  TEST4R.
            02  FILLER  PIC X(4)  VALUE X'C311C3F1'.
            02  FILLER  PIC X(45)
                VALUE 'TESTBEC6 ASRA IN TESTBEC7 HANDLED OK BY LABEL'.
        77  TEST4RL  PIC 9(4) COMP.
        01  TEST4K.
            02  FILLER  PIC X(4)  VALUE X'C311C5C1'.
            02  FILLER  PIC X(43)
                VALUE 'TESTBEC6 HANDLE ABEND RETURNED SUCCESSFULLY'.
            02  FILLER  PIC X(3)  VALUE X'11C6D1'.
            02  FILLER  PIC X(79)
                VALUE 'TESTBEC6 TEST 5:PRESS AID KEY TO HANDLE AEI0 ABEN
      -               'D IN LINKED PROGRAM BY PROGRAM'.
        77  TEST4KL  PIC 9(4) COMP.
        01  TEST5K.
            02  FILLER  PIC X(4)  VALUE X'C3114AC1'.
            02  FILLER  PIC X(43)
                VALUE 'TESTBEC6 HANDLE ABEND RETURNED SUCCESSFULLY'.
            02  FILLER  PIC X(3)  VALUE X'114BD1'.
            02  FILLER  PIC X(55)
                VALUE 'TESTBEC6 TEST6:PRESS AID KEY TO ABEND, HANDLED BY
      -               ' LABEL'.
        77  TEST5KL  PIC 9(4) COMP.
        01  TEST6R.
            02  FILLER  PIC X(4)  VALUE X'C3114C61'.
            02  FILLER  PIC X(39)
                VALUE 'TESTBEC6 ABEND ZZZZ HANDLED OK BY LABEL'.
            02  FILLER  PIC X(3)  VALUE X'114FC1'.
            02  FILLER  PIC X(30)
                VALUE 'BEC5 ENDED OK, NOW PRESS CLEAR'.
        77  TEST6RL  PIC 9(4) COMP.
        77  ACOMM    PIC X(4) VALUE 'AEI0'.
        77  BADFLD1  PIC 9(4) COMP VALUE 0.
        77  BADFLD2  PIC S9(3) REDEFINES BADFLD1 COMP-3.

        PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA.

            MOVE LENGTH OF TEST1   TO TEST1L.
            MOVE LENGTH OF TEST1R  TO TEST1RL.
            MOVE LENGTH OF TEST2R  TO TEST2RL.
            MOVE LENGTH OF TEST4   TO TEST4L.
            MOVE LENGTH OF TEST4R  TO TEST4RL.
            MOVE LENGTH OF TEST4K  TO TEST4KL.
            MOVE LENGTH OF TEST5K  TO TEST5KL.
            MOVE LENGTH OF TEST6R  TO TEST6RL.

      *  CLC   EIBTRNID,=C'BEC5'  SECOND TEST TRANSID ?
      *  BE    BEC5               EXIT IF IT IS
            IF EIBTRNID = 'BEC5' THEN GO TO BEC5.
            EXEC CICS SEND FROM(TEST1) LENGTH(TEST1L) END-EXEC.
      * CREATE AND MODIFY HANDLE ABEND BLOCK
            EXEC CICS HANDLE ABEND LABEL(BEC6ASRA) END-EXEC.
            EXEC CICS RECEIVE INTO(RECAREA) LENGTH(RECLEN) END-EXEC.
      * CAUSE ASRA
      * THIS IS 'REPLACED' BY SEND AND B AB2
      *  DC    H'0'
            ADD 1 TO BADFLD2.

      * AB2      EQU   *
        AB2.
      * ASRA HANDLED OK
      * MODIFY HANDLE ABEND BLOCK
            EXEC CICS HANDLE ABEND LABEL(BEC6AEI0) END-EXEC.
            EXEC CICS RECEIVE INTO(RECAREA) LENGTH(RECLEN) END-EXEC.

      * CAUSE AEI0
      * THIS IS 'REPLACED' BY SEND AND B AB3
            EXEC CICS LOAD PROGRAM('BADPROG') END-EXEC.

      * AB3      EQU   *
        AB3.
      * AEI0 HANDLED OK
      * MODIFY HANDLE ABEND BLOCK
            EXEC CICS HANDLE ABEND PROGRAM('TESTBEC7') END-EXEC.
            EXEC CICS RECEIVE INTO(RECAREA) LENGTH(RECLEN) END-EXEC.

      * CAUSE ASRA
      * THIS ACTS AS THOUGH WE HAD AN XCTL TO TESTBEC7 HERE
      * THE RETURN IN TESTBEC7 ENDS THE TASK NORMALLY
      *  DC    H'0'
            MOVE 0 TO BADFLD1.
            ADD 1 TO BADFLD2.

      * BEC6ASRA EQU   *
        BEC6ASRA.
            EXEC CICS SEND FROM(TEST1R) LENGTH(TEST1RL) END-EXEC.
      * B     AB2                CONTINUE TESTS
            GO TO AB2.

      * BEC6AEI0 EQU   *
        BEC6AEI0.
            EXEC CICS SEND FROM(TEST2R) LENGTH(TEST2RL) END-EXEC.
      * B     AB3                CONTINUE TESTS
            GO TO AB3.
      *
      * BEC5 TESTS
      *
      * BEC5     EQU   *
        BEC5.
            EXEC CICS SEND FROM(TEST4) LENGTH(TEST4L) END-EXEC.
            EXEC CICS HANDLE ABEND LABEL(BEC7ASRA) END-EXEC.
            EXEC CICS RECEIVE INTO(RECAREA) LENGTH(RECLEN) END-EXEC.
      * TESTBEC7 WILL ABEND ASRA
            EXEC CICS LINK PROGRAM('TESTBEC7') END-EXEC.
      *
        BEC7ASOK.
      * RETURN HERE AFTER HANDLING ASRA ABEND
            EXEC CICS SEND FROM(TEST4K) LENGTH(TEST4KL) END-EXEC.
            EXEC CICS RECEIVE INTO(RECAREA) LENGTH(RECLEN) END-EXEC.
      * TESTBEC7 WILL ABEND AEI0
            EXEC CICS LINK PROGRAM('TESTBEC7') COMMAREA(ACOMM)
                      END-EXEC.

      * RETURN HERE AFTER HANDLING AEI0 ABEND
            EXEC CICS SEND FROM(TEST5K) LENGTH(TEST5KL) END-EXEC.
            EXEC CICS HANDLE ABEND LABEL(BEC6ZZZZ) END-EXEC.
            EXEC CICS RECEIVE INTO(RECAREA) LENGTH(RECLEN) END-EXEC.
      * TAKE THE DUMP, IGNORE THE ABEND
            EXEC CICS ABEND ABCODE('ZZZZ') END-EXEC.
      * AB4      EQU   *
        AB4.
            EXEC CICS RETURN END-EXEC.

      * BEC7ASRA EQU   *
        BEC7ASRA.
            EXEC CICS SEND FROM(TEST4R) LENGTH(TEST4RL) END-EXEC.
            GO TO BEC7ASOK.

      * BEC6ZZZZ EQU   *
        BEC6ZZZZ.
            EXEC CICS SEND FROM(TEST6R) LENGTH(TEST6RL) END-EXEC.
      *  B     AB4                EXIT
            GO TO AB4.
