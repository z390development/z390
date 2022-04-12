      *****************************************************************
      * Copyright 2006 Automated Software Tools Corporation           *
      * This source code is part of z390 assembler/emulator package   *
      * The z390 package is distributed under GNU general public      *
      * license                                                       *
      * Author - Don Higgins                                          *
      *****************************************************************
        IDENTIFICATION DIVISION.
      *
      * Hello world program (COBOL)
      *
        PROGRAM-ID. TESTGUIC.
        ENVIRONMENT DIVISION.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
                
      *****************************************************************
      * Variables not used but document fields in MSG0                *
      *****************************************************************
        77  ESCAPE       PIC X(1)  VALUE X'27'.
        77  ERASE_WRITE  PIC X(1)  VALUE X'F5'.
        77  WCC          PIC X(1)  VALUE X'C3'.
        
      *****************************************************************
      * SBAs used in MSG0: row 11, col 35 and row 14, col 25          *
      *****************************************************************
        77  SBA_R11_C35  PIC X(3)  VALUE X'114DC2'.
        77  SBA_R14_C25  PIC X(3)  VALUE X'1150E8'.
        
        77  MSG0L        PIC 9(4)  COMP VALUE 53.
        
        01  MSG0.
            02  M0_ESC   PIC X(1)  VALUE X'27'.
            02  M0_EW    PIC X(1)  VALUE X'F5'.
            02  M0_WCC   PIC X(1)  VALUE X'C3'.
            02  M0_SBA1  PIC X(3)  VALUE X'114DC2'.
            02  FILLER   PIC X(12) VALUE 'Hello World!'.
            02  M0_SBA2  PIC X(3)  VALUE X'1150E8'.
            02  FILLER   PIC X(32) VALUE
                                   'Press Ctrl+C to clear the screen'.

        LINKAGE SECTION.
        01  DFHCOMMAREA.
            02  INCOMM   PIC 9(4)  COMP.

        01  DFHCWA.
            02  FILLER   PIC X.

        PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA.
      *
           EXEC CICS SEND FROM(MSG0) LENGTH(MSG0L) END-EXEC.
           EXEC CICS RETURN END-EXEC.
