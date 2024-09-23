000100 IDENTIFICATION DIVISION.
000200 PROGRAM-ID.
000300     IC202A.
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
002100*        THE SUBPROGRAM IC202 IS CALLED BY THE PROGRAM IC201.  *
002200*    THE SUBPROGRAM HAS FOUR OPERANDS IN THE USING PHRASE      *
002300*    OF THE PROCEDURE DIVISION HEADER.                         *
002400****************************************************************
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
004100 77  WS1 PICTURE S999.
004200 77  WS2 PICTURE S999
004300         USAGE COMPUTATIONAL, VALUE ZERO.
004400 LINKAGE SECTION.
004500 77  DN1 PICTURE S99.
004600 77  DN2 PICTURE S99 USAGE COMPUTATIONAL.
004700 77  DN3 PICTURE S99.
004800 77  DN4 PICTURE S99 USAGE COMPUTATIONAL.
004900 PROCEDURE DIVISION USING DN1, DN2, DN3, DN4.
005000 SECT-IC202-0001 SECTION.
005100 CALL-TEST-001.
005200     MOVE DN1 TO WS1.
005300     ADD 1 TO WS1.
005400     ADD 1 TO WS2.
005500     MOVE WS1 TO DN3.
005600     MOVE WS2 TO DN4.
005700 CALL-EXIT-001.
005800     EXIT PROGRAM.
