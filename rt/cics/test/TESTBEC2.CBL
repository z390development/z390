      *****************************************************************
      * Copyright 2006 Automated Software Tools Corporation           *
      * This source code is part of z390 assembler/emulator package   *
      * The z390 package is distributed under GNU general public      *
      * license                                                       *
      * Author - Don Higgins                                          *
      *****************************************************************
        IDENTIFICATION DIVISION.
      *
      * TEST BED 2 (COBOL)
      *
      *    TESTBED3 HAS BEEN LEFT AS ASM TO PROVE LINKAGE
      *
      *    LINKED-TO FROM TESTBEC1
      *    DISPLAYS ANOTHER QUOTE
      *
      *    IF A COMMAREA IS PASSED, ANOTHER QUOTE IS DISPLAYED DEPENDING
      *    ON COMMAREA PASSED
      *
      *    XCTL TO TESTBED3 FOR FINAL QUOTE
      *    RETURN TO TESTBEC1
      *

        PROGRAM-ID. TESTBEC2.
        ENVIRONMENT DIVISION.
        DATA DIVISION.

        WORKING-STORAGE SECTION.
        01 SENDLNS1.
           02 FILLER PIC X      VALUE X'C3'.
           02 FILLER PIC X(3)   VALUE X'114DF1'.
           02 FILLER PIC X(55)  VALUE 'How sour sweet music is'.
           02 FILLER PIC X(3)   VALUE X'114FC1'.
           02 FILLER PIC X(55)  VALUE
                         'When time is broke and no proportion kept'.
           02 FILLER PIC X(3)   VALUE X'1150D1'.
           02 FILLER PIC X(55)  VALUE
                         'So is it in the music of men''s lives'.
           02 FILLER PIC X(3)   VALUE X'11D161'.
           02 FILLER PIC X(55)  VALUE 'END'.
        01 SENDLNS2.
           02 FILLER PIC X      VALUE X'C3'.
           02 FILLER PIC X(3)   VALUE X'11D2F1'.
           02 FILLER PIC X(55)  VALUE
              'This blessed plot, this earth, this realm, this England'.
           02 FILLER PIC X(3)   VALUE X'11D4C1'.
           02 FILLER PIC X(55)  VALUE 'END'.
        01 SENDLNS3.
           02 FILLER PIC X      VALUE X'C3'.
           02 FILLER PIC X(3)   VALUE X'11D5D1'.
           02 FILLER PIC X(55)  VALUE 'How beauteous mankind is!'.
           02 FILLER PIC X(3)   VALUE X'11D661'.
           02 FILLER PIC X(55)  VALUE
              'O brave new world, that hath such people in''t!'.
           02 FILLER PIC X(3)   VALUE X'11D7F1'.
           02 FILLER PIC X(55)  VALUE 'END'.

        LINKAGE SECTION.
        01  DFHCOMMAREA.
            02  INCOMM   PIC X(6).

        PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA.
      *  MVC   SENDLNS1(LINE1END),LINE1 MESSAGE
            EXEC CICS SEND FROM(SENDLNS1) LENGTH(233) END-EXEC.

      *  ICM   R4,15,DFHEICAP     ANY COMMAREA ?
      *  BNZ   TRYCOMM1           EXIT IF THERE IS
            IF EIBCALEN = 0 THEN EXEC CICS RETURN END-EXEC
            ELSE
      *  TRYCOMM1 EQU   *
      *  CLC   0(6,R4),=C'QUOTE2' IS IT QUOTE 2 ?
      *  BNE   TRYCOMM2           EXIT IF NOT
            IF INCOMM = 'QUOTE2' THEN
      *  MVC   SENDLNS2(LINE2END),LINE2 MESSAGE
               EXEC CICS SEND FROM(SENDLNS2) LENGTH(117) END-EXEC
               EXEC CICS RETURN END-EXEC
            ELSE
            IF INCOMM = 'QUOTE3' THEN
      *  TRYCOMM2 EQU   *
      *  CLC   0(6,R4),=C'QUOTE3' IS IT QUOTE 3 ?
      *  BNE   ABORT              EXIT IF NOT
      *  MVC   SENDLNS3(LINE3END),LINE3 MESSAGE
               EXEC CICS SEND FROM(SENDLNS3) LENGTH(174) END-EXEC
      *
      * XCTL WITH NO COMMAREA TO TESTBED3 FOR THE NEXT QUOTE
      * CONTROL WILL RETURN TO TESTBEC1
      *
               EXEC CICS XCTL PROGRAM('TESTBED3') END-EXEC
            ELSE
      * ABORT    ABEND 999,DUMP
               EXEC CICS ABEND ABCODE(' 999') END-EXEC.
