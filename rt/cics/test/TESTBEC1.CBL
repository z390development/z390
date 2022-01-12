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
      * TEST BED 1 (COBOL)
      *
      *    TESTBED3 HAS BEEN LEFT AS ASM TO PROVE LINKAGE
      *
      *    TEST RETURN TRANSID AND COMMAREA
      *    BEC1 DISPLAYS 1ST LINE
      *    EACH SUBSEQUENT LINE INDEX COMES FROM IMPORTED COMMAREA
      *
      *    TEST LINK WITH DIFFERENT COMMAREAS
      *
      *    TEST XCTL WITH COMMAREA
      *
        PROGRAM-ID. TESTBEC1.
        ENVIRONMENT DIVISION.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        77  SENDWCC  PIC X.
        77  SENDSBA  PIC X(3).
        77  SENDLINE PIC X(45).
        77  MYCOMM   PIC 9(4)  COMP.
        77  WCC      PIC X     VALUE X'F5'.
        77  MYBEC2   PIC X(8)  VALUE 'TESTBEC2'.
        77  COMM1    PIC X(6)  VALUE 'QUOTE2'.
        77  COMM2    PIC X(6)  VALUE 'QUOTE3'.
        77  COMM3    PIC X(6)  VALUE 'QUOTE6'.

        01  SBAS.
      * SBA,3,2 THRU SBA,11,2
            02  FILLER   PIC X(3)  VALUE X'11C261'.
            02  FILLER   PIC X(3)  VALUE X'11C3F1'.
            02  FILLER   PIC X(3)  VALUE X'11C5C1'.
            02  FILLER   PIC X(3)  VALUE X'11C6D1'.
            02  FILLER   PIC X(3)  VALUE X'11C761'.
            02  FILLER   PIC X(3)  VALUE X'11C8F1'.
            02  FILLER   PIC X(3)  VALUE X'114AC1'.
            02  FILLER   PIC X(3)  VALUE X'114BD1'.
            02  FILLER   PIC X(3)  VALUE X'114C61'.
         01 FILLER REDEFINES SBAS.
            02  SBA      OCCURS 9 TIMES PIC X(3).

        01  LINES.
            02  LINE1    PIC X(45) VALUE 'What a piece of work is man'.
            02  FILLER   PIC X(45) VALUE 'How noble in reason'.
            02  FILLER   PIC X(45) VALUE 'How infinite in faculties'.
            02  FILLER   PIC X(45) VALUE
                      'In form and moving, how express and admirable'.
            02  FILLER   PIC X(45) VALUE 'In action, how like an angel'.
            02  FILLER   PIC X(45) VALUE
                      'In apprehension, how like a god'.
            02  FILLER   PIC X(45) VALUE 'The beauty of the world'.
            02  FILLER   PIC X(45) VALUE 'The paragon of animals'.
            02  FILLER   PIC X(45) VALUE 'END'.
         01 FILLER REDEFINES LINES.
            02  LINE_N   OCCURS 9 TIMES PIC X(45).

        LINKAGE SECTION.
        01  DFHCOMMAREA.
            02  INCOMM   PIC 9(4)  COMP.

        PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA.
      * XC    MYCOMM,MYCOMM      INITIALISE COMMAREA
            MOVE 1 TO MYCOMM.
      *  MVC   SENDWCC,WCC        MOVE WCC
            MOVE WCC TO SENDWCC.
      *  OC    TCTTECA,TCTTECA    ANY COMMAREA ?
      *  BNZ   NOT1ST             EXIT IF NOT FIRST
            IF EIBCALEN = 0 THEN
      * FIRST LINE
      *  MVC   SENDSBA,SBA1       1ST SBA
               MOVE SBA (1) TO SENDSBA
      *  MVC   SENDLINE,LINE1     1ST LINE
               MOVE LINE_N (1) TO SENDLINE
               EXEC CICS SEND FROM(SENDWCC) LENGTH(49) END-EXEC
               EXEC CICS RETURN TRANSID('BEC1')
                                COMMAREA(MYCOMM) END-EXEC
            ELSE
      * NOT FIRST LINE
      *  L     R5,TCTTECA         ADDRESS RECEIVED COMMAREA
      *  LH    R6,0(R5)           R6=INDEX
      *  AHI   R6,1               INCREMENT
      *  STH   R6,0(R5)           SAVE
               ADD 1 TO INCOMM
      *  MS    R6,=F'3'           INDEX TO SBA
      *  LA    R7,SBA1(R6)        R7=ADDRESS OF NEXT SBA
      *  MVC   SENDSBA,0(R7)      MOVE NEXT SBA TO OUTPUT
               MOVE SBA (INCOMM) TO SENDSBA
      *  LH    R6,0(R5)           R6=INDEX
      *  MS    R6,=F'45'          INDEX TO NEXT LINE
      *  LA    R7,LINE1(R6)       R7=ADDRESS OF NEXT LINE
      *  MVC   SENDLINE,0(R7)     MOVE NEXT LINE TO OUTPUT
               MOVE LINE_N (INCOMM) TO SENDLINE
               EXEC CICS SEND FROM(SENDWCC) LENGTH(49) END-EXEC
      *  CLC   0(2,R5),=H'8'      DISPLAYED LAST ?
      *  BE    NEXT1              EXIT IF WE HAVE
               IF INCOMM NOT = 9 THEN
                  EXEC CICS RETURN TRANSID('BEC1') COMMAREA(INCOMM)
                                   LENGTH(2) END-EXEC
               ELSE
      * TEST LINK
                  EXEC CICS LINK PROGRAM(MYBEC2) END-EXEC
      * TEST LINK WITH COMMAREA 1
                  EXEC CICS LINK PROGRAM('TESTBEC2') COMMAREA(COMM1)
                                 END-EXEC
      * TEST LINK WITH COMMAREA 2
                  EXEC CICS LINK PROGRAM('TESTBEC2') COMMAREA(COMM2)
                                 LENGTH(6) END-EXEC
      * TEST XCTL WITH COMMAREA 3
                  EXEC CICS XCTL PROGRAM('TESTBED3') COMMAREA(COMM3)
                                 END-EXEC.
