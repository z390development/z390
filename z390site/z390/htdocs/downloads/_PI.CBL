
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PII.
       AUTHOR. PF/TR.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       OBJECT-COMPUTER. ANY-THING.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *
       01  FULL-SCREEN         PIC X(1920).
      *
       01  DI-1 REDEFINES FULL-SCREEN.
           02 FILLER           PIC X(160).
           02 DI-TX1           PIC X(160).
           02 DI-TX2           PIC X(13).
           02 DI-TERM          PIC X(15).
           02 FILLER           PIC X(136).
           02 DI-TX3           PIC X(6).
           02 DI-PI            PIC X(15).
           02 FILLER           PIC X(1415).
      *
       01  DI-2 REDEFINES FULL-SCREEN.
           02 FILLER           PIC X(333).
           02 DI-TERM2         PIC X(15).
           02 FILLER           PIC X(142).
           02 DI-PI2           PIC X(15).
           02 FILLER           PIC X(1415).
      *
       01  WORK-AREA.
           02 PI               PIC S9V9(14).
           02 TERM             PIC S9V9(14).
           02 W                PIC S9V9(14).
           02 N                PIC 9999.
           02 N1               PIC 9999.
           02 N2               PIC 9999.
           02 ED               PIC -9.9(12).
      *
       01  CONSTANTS.
           02 TX1              PIC X(17) VALUE "CALCULATION OF PI".
           02 TX2              PIC X(12) VALUE "NEXT TERM IS".
           02 TX3              PIC X(5) VALUE "PI IS".
      *
       PROCEDURE DIVISION.
       LA-START.
           DISPLAY SPACE.
           MOVE SPACE TO FULL-SCREEN.
           MOVE TX1 TO DI-TX1.
           MOVE TX2 TO DI-TX2.
           MOVE TX3 TO DI-TX3.
           MOVE 0.5 TO ED.
           MOVE ED TO DI-TERM.
           MOVE 3 TO ED.
           MOVE ED TO DI-PI.
           DISPLAY DI-1.
           MOVE 0.5 TO PI.
           MOVE 0.5 TO TERM.
           MOVE 3 TO N.
       LOOP.
           MOVE N TO N2.
           SUBTRACT 2 FROM N2.
           MULTIPLY N2 BY N2.
           MULTIPLY N2 BY TERM.
           MOVE N TO N1.
           SUBTRACT 1 FROM N1.
           MULTIPLY N BY N1.
           MULTIPLY 4 BY N1.
           DIVIDE N1 INTO TERM.
           IF TERM < 0.0000000000001 THEN GO TO HALT.
           ADD TERM TO PI.
           MOVE PI TO W.
           MULTIPLY 6 BY W.
           MOVE W TO ED.
           MOVE ED TO DI-PI2.
           MOVE TERM TO ED.
           MOVE ED TO DI-TERM2.
           DISPLAY DI-2.
           ADD 2 TO N.
           IF N < 100 GO TO LOOP.
       HALT.
           STOP RUN.
