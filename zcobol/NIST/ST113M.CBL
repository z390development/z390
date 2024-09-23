000100 IDENTIFICATION DIVISION.
000200 PROGRAM-ID.
000300     ST113M.
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
002100 ENVIRONMENT DIVISION.
002200 CONFIGURATION SECTION.
002300 SOURCE-COMPUTER.
002400     XXXXX082.
002500 OBJECT-COMPUTER.
002600     XXXXX083.
002700 INPUT-OUTPUT SECTION.
002800 FILE-CONTROL.
002900     SELECT PRINT-FILE ASSIGN TO
003000     XXXXX055.
003100     SELECT   SORTIN-1M ASSIGN TO
003200     XXXXP006.
003300     SELECT   SORTOUT-1M ASSIGN TO
003400     XXXXP001.
003500     SELECT   SORTFILE-1M ASSIGN TO
003600     XXXXX027.
003700 DATA DIVISION.
003800 FILE SECTION.
003900 FD  PRINT-FILE.
004000 01  PRINT-REC PICTURE X(120).
004100 01  DUMMY-RECORD PICTURE X(120).
004200 FD  SORTIN-1M
004300C    VALUE OF
004400C    XXXXX074
004500C    IS
004600C    XXXXX079
004700G    XXXXX069
004800     .
004900 01  SORT-KEY-IN        PICTURE X(33).
005000 FD  SORTOUT-1M
005100C    VALUE OF
005200C    XXXXX074
005300C    IS
005400C    XXXXX075
005500G    XXXXX069
005600     .
005700 01  SORT-KEY-OUT       PICTURE X(33).
005800 SD  SORTFILE-1M.
005900 01  SORT-KEY           PICTURE X(33).
006000 PROCEDURE    DIVISION.
006100 SORT-PARA SECTION.
006200 SORT-PARAGRAPH.
006300     SORT     SORTFILE-1M DESCENDING
006400              SORT-KEY
006500              USING SORTIN-1M
006600              GIVING SORTOUT-1M.
006700     STOP     RUN.
