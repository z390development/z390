000100 IDENTIFICATION DIVISION.
000200 PROGRAM-ID.
000300     IC118M.
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
002100*
002200*        THE SUBPROGRAM IC118 IS CALLED BY THE SUBPROGRAM IC117.
002300*    THE SUBPROGRAM IC118 DOES NOT CONTAIN A LINKAGE SECTION OR
002400*    AN USING PHRASE IN THE PROCEDURE DIVISION HEADER.  A DISPLAY
002500*    STATEMENT IS EXECUTED TO VERIFY THAT THIS SUBPROGRAM WAS
002600*    EXECUTED.
002700*
002800******************************************************************
002900 ENVIRONMENT DIVISION.
003000 CONFIGURATION SECTION.
003100 SOURCE-COMPUTER.
003200     XXXXX082.
003300 OBJECT-COMPUTER.
003400     XXXXX083.
003500 DATA DIVISION.
003600 WORKING-STORAGE SECTION.
003700 01  IC118-TEMP1         PICTURE 9.
003800 01  TWO                 PICTURE 9   VALUE 2.
003900 01  IC118-TEMP2         PICTURE 99   VALUE 97.
004000 PROCEDURE DIVISION.
004100*USNG-TEST-04.
004200*
004300*    THIS TEST VERIFIES THAT A SUBPROGRAM PROCEDURE DIVISION
004400*    HEADER IS NOT REQUIRED TO HAVE THE OPTIONAL USING PHRASE.
004500*
004600 USNG-VERIFY-04.
004700     MOVE 2 TO IC118-TEMP1.
004800     ADD TWO TO IC118-TEMP2.
004900*
005000*    THE RESULTS OF THE ABOVE STATEMENTS ARE NOT TESTED.
005100*
005200 USNG-DISPLAY-04.
005300     DISPLAY "IC118M CALLED".
005400 IC118-EXIT.
005500     EXIT PROGRAM.
