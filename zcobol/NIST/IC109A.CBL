000100 IDENTIFICATION DIVISION.
000200 PROGRAM-ID.
000300     IC109A.
000400****************************************************************
000500*                                                              *
000600*    VALIDATION FOR:-                                          *
000700*                                                              *
000800*    "ON-SITE VALIDATION, NATIONAL INSTITUTE OF STD & TECH.     ".
000900*                                                              *
001000*    "COBOL 85 VERSION 4.2, Apr  1993 SSVG                      ".
001100*                                                              *
001200****************************************************************
001300*                                                              *
001400*      X-CARDS USED BY THIS PROGRAM ARE :-                     *
001500*                                                              *
001600*        X-55  - SYSTEM PRINTER NAME.                          *
001700*        X-82  - SOURCE COMPUTER NAME.                         *
001800*        X-83  - OBJECT COMPUTER NAME.                         *
001900*                                                              *
002000****************************************************************
002100*        THE SUBPROGRAM IC109 IS THE FIRST SUBPROGRAM IN A
002200*    SEQUENCE OF CALLS WHICH START IN THE MAIN PROGRAM IC108.
002300*    IC109 CALLS IC110 WITH ONE OPERAND IN THE WORKING-STORAGE
002400*    SECTION AND ONE OPERAND IN THE LINKAGE SECTION.
002500 ENVIRONMENT DIVISION.
002600 CONFIGURATION SECTION.
002700 SOURCE-COMPUTER.
002800     XXXXX082.
002900 OBJECT-COMPUTER.
003000     XXXXX083.
003100 INPUT-OUTPUT SECTION.
003200 FILE-CONTROL.
003300     SELECT PRINT-FILE ASSIGN TO
003400     XXXXX055.
003500 DATA DIVISION.
003600 FILE SECTION.
003700 FD  PRINT-FILE.
003800 01  PRINT-REC PICTURE X(120).
003900 01  DUMMY-RECORD PICTURE X(120).
004000 WORKING-STORAGE SECTION.
004100 77  WS1 PICTURE X.
004200 LINKAGE SECTION.
004300 01  GRP-01.
004400     02  SUB-CALLED.
004500         03  DN1  PICTURE X(6).
004600         03  DN2  PICTURE X(6).
004700         03  DN3  PICTURE X(6).
004800     02  TIMES-CALLED.
004900         03  DN4  PICTURE S999.
005000         03  DN5  PICTURE S999.
005100         03  DN6  PICTURE S999.
005200     02  SPECIAL-FLAGS.
005300         03  DN7 PICTURE X.
005400         03  DN8 PICTURE X.
005500         03  DN9 PICTURE X.
005600 PROCEDURE DIVISION USING GRP-01.
005700 SECT-IC109-0001 SECTION.
005800 PARA-IC109.
005900     MOVE "IC109A" TO DN1.
006000     MOVE SPACE TO WS1.
006100     CALL "IC110A" USING WS1 GRP-01.
006200     ADD 1 TO DN4.
006300     MOVE WS1 TO DN9.
006400 EXIT-IC109.
006500     EXIT PROGRAM.
