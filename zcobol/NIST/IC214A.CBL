000100 IDENTIFICATION DIVISION.
000200 PROGRAM-ID.
000300     IC214A.
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
002100*    THIS IS SUBPROGRAM IC214.
002200****************************************************************
002300 ENVIRONMENT DIVISION.
002400 CONFIGURATION SECTION.
002500 SOURCE-COMPUTER.
002600     XXXXX082.
002700 OBJECT-COMPUTER.
002800     XXXXX083.
002900 DATA DIVISION.
003000 LINKAGE SECTION.
003100 01  DN1                         PICTURE S9.
003200 PROCEDURE DIVISION USING DN1.
003300 CALL-TEST-1.
003400     MOVE 1 TO DN1.
003500 IC214-EXIT.
003600     EXIT PROGRAM.
