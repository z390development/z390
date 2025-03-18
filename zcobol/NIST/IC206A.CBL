000100 IDENTIFICATION DIVISION.
000200 PROGRAM-ID.
000300     IC206A.
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
002100*        THE SUBPROGRAM IC206 IS CALLED BY THE SUBPROGRAM
002200*    IC205.  THE SUBPROGRAM IS THEN CANCELED AND CALLED
002300*    AGAIN.  THE PROGRAM IC205 CHECKS IF IC206 WAS IN ITS
002400*    INITIAL STATE ON THE FIRST CALL AFTER THE PROGRAM WAS
002500*    CANCELED.
002600*        THE LINKAGE PARAMETER DN1 CONTAINS THE NUMBER OF
002700*    TIMES IC206 HAS BEEN CALLED SINCE INITIALIZATION WHEN
002800*    CONTROL IS RETURNED TO THE CALLING PROGRAM.
002900****************************************************************
003000 ENVIRONMENT DIVISION.
003100 CONFIGURATION SECTION.
003200 SOURCE-COMPUTER.
003300     XXXXX082.
003400 OBJECT-COMPUTER.
003500     XXXXX083.
003600 INPUT-OUTPUT SECTION.
003700 FILE-CONTROL.
003800     SELECT PRINT-FILE ASSIGN TO
003900     XXXXX055.
004000 DATA DIVISION.
004100 FILE SECTION.
004200 FD  PRINT-FILE.
004300 01  PRINT-REC PICTURE X(120).
004400 01  DUMMY-RECORD PICTURE X(120).
004500 WORKING-STORAGE SECTION.
004600 77  WS1 PICTURE S9(8) USAGE COMPUTATIONAL
004700         VALUE ZERO.
004800 LINKAGE SECTION.
004900 01  DN1 PICTURE S9(8)  USAGE COMPUTATIONAL.
005000 PROCEDURE DIVISION USING DN1.
005100 SECT-IC206-0001 SECTION.
005200 TEST-PARAGRAPH.
005300     ADD 1 TO WS1.
005400     MOVE WS1 TO DN1.
005500 EXIT-IC206.
005600     EXIT PROGRAM.
