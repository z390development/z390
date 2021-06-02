      *****************************************************************
      * Copyright 2006 Automated Software Tools Corporation           *
      * This source code is part of z390 assembler/emulator package   *
      * The z390 package is distributed under GNU general public      *
      * license                                                       *
      * Author - Don Higgins                                          *
      *****************************************************************
        IDENTIFICATION DIVISION.
      *
      * HANDLE ABEND BY PROGRAM (PART OF TRANSID BEC5)
      *
        PROGRAM-ID. TESTBEC8.
        ENVIRONMENT DIVISION.

        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01  TEST1.
            02  FILLER   PIC X(4)  VALUE X'C311C8F1'.
            02  FILLER   PIC X(42)
                VALUE 'TESTBEC8 SAYS HELLO AND THE COMMAREA SAYS '.
            02  TESTAEI0 PIC X(4).
        77  TEST1L   PIC 9(4) COMP.

        LINKAGE SECTION.
        01  DFHCOMMAREA.
            02  COMMMSG  PIC X(4).

        PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA.

            MOVE LENGTH OF TEST1   TO TEST1L.

      * BEC5 ABEND HANDLER
      *  L     R6,DFHEICAP        R6=COMMAREA ADDRESS
      *  MVC   TESTAEI0,0(R6)     MOVE TO MESSAGE
            MOVE COMMMSG TO TESTAEI0.
            EXEC CICS SEND FROM(TEST1) LENGTH(TEST1L) END-EXEC.
            EXEC CICS RETURN END-EXEC.
