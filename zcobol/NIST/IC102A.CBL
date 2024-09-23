000100 IDENTIFICATION DIVISION.
000200 PROGRAM-ID.
000300     IC102A.
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
002100*        THIS PROGRAM TESTS THE USE OF THE LINKAGE SECTION
002200*    AND USING PHRASE IN THE PROCEDURE DIVISION HEADER.
002300 ENVIRONMENT DIVISION.
002400 CONFIGURATION SECTION.
002500 SOURCE-COMPUTER.
002600     XXXXX082.
002700 OBJECT-COMPUTER.
002800     XXXXX083.
002900 INPUT-OUTPUT SECTION.
003000 FILE-CONTROL.
003100     SELECT PRINT-FILE ASSIGN TO
003200     XXXXX055.
003300 DATA DIVISION.
003400 FILE SECTION.
003500 FD  PRINT-FILE.
003600 01  PRINT-REC PICTURE X(120).
003700 01  DUMMY-RECORD PICTURE X(120).
003800 WORKING-STORAGE SECTION.
003900 77  DN2 PICTURE S9  VALUE ZERO.
004000 LINKAGE SECTION.
004100 77  DN1 PICTURE S9.
004200 PROCEDURE DIVISION USING DN1.
004300 SECT-IC102-0001 SECTION.
004400 CALL-TEST-001.
004500     ADD 1 TO DN2.
004600     MOVE DN2 TO DN1.
004700 CALL-EXIT-001.
004800     EXIT PROGRAM.
