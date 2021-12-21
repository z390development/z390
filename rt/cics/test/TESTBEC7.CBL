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
      * 1) HANDLE ABEND BY PROGRAM
      * 2) LINKED PROGRAM TO DEMO COMPLEX HANDLE ABEND
      *
        PROGRAM-ID. TESTBEC7.
        ENVIRONMENT DIVISION.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01  TEST1.
            02  FILLER  PIC X(4)  VALUE X'C311C761'.
            02  FILLER  PIC X(35)
                        VALUE 'TESTBEC7 IN TRANSID BEC4 SAYS HELLO'.
            02  FILLER  PIC X(3)  VALUE X'114AC1'.
            02  FILLER  PIC X(34)
                        VALUE 'BEC4 COMPLETED OK, NOW PRESS CLEAR'.
        77  TEST1L   PIC 9(4) COMP.
        01  TEST2.
            02  FILLER  PIC X(4)  VALUE X'C311C261'.
            02  FILLER  PIC X(51)
           VALUE 'TESTBEC7 IN TRANSID BEC5 SAYS HELLO AND ABENDS ASRA'.
        77  TEST2L   PIC 9(4) COMP.
        01  TEST3.
            02  FILLER  PIC X(4)  VALUE X'C311C761'.
            02  FILLER  PIC X(51)
           VALUE 'TESTBEC7 IN TRANSID BEC5 SAYS HELLO AND ABENDS AEI0'.
        77  TEST3L   PIC 9(4) COMP.
        77  BADFLD1  PIC 9(4) COMP VALUE 0.
        77  BADFLD2  PIC S9(3) REDEFINES BADFLD1 COMP-3.

        PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA.

            MOVE LENGTH OF TEST1   TO TEST1L.
            MOVE LENGTH OF TEST2   TO TEST2L.
            MOVE LENGTH OF TEST3   TO TEST3L.

      *  CLC   EIBTRNID,=C'BEC5'  SECOND TEST TRANSID ?
      *  BE    BEC5               EXIT IF IT IS
            IF EIBTRNID = 'BEC5' THEN GO TO BEC5.
      * BEC4 ABEND HANDLER
            EXEC CICS SEND FROM(TEST1) LENGTH(TEST1L) END-EXEC.
            EXEC CICS RETURN END-EXEC.
      *
      * ABEND PRODUCER FOR TRANSID BEC5
      *
      * BEC5     EQU   *
        BEC5.
      *  OC    DFHEICAP,DFHEICAP  ANY COMMAREA ?
      *  BNZ   BEC5COMM           EXIT IF WE HAVE
            IF EIBCALEN NOT = 0 THEN GO TO BEC5COMM.
      * CAUSE ASRA TO BE HANDLED BY TESTBEC6
            EXEC CICS SEND FROM(TEST2) LENGTH(TEST2L) END-EXEC.
      *  DC    H'0'
            ADD 1 TO BADFLD2.

      * BEC5COMM EQU   *
        BEC5COMM.
            EXEC CICS HANDLE ABEND PROGRAM('TESTBEC8') END-EXEC.
            EXEC CICS SEND FROM(TEST3) LENGTH(TEST3L) END-EXEC.
      * CAUSE AEI0
      * THIS IS 'REPLACED' BY XCTL TO TESTBEC8 WITH OUR COMMAREA
            EXEC CICS LOAD PROGRAM('BADPROG7') END-EXEC.
