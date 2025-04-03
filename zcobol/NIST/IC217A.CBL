000100 IDENTIFICATION DIVISION.
000200 PROGRAM-ID.
000300     IC217A.
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
002100*    THIS IS SUBPROGRAM IC217.
002200****************************************************************
002300 ENVIRONMENT DIVISION.
002400 CONFIGURATION SECTION.
002500 SOURCE-COMPUTER.
002600     XXXXX082.
002700 OBJECT-COMPUTER.
002800     XXXXX083.
002900 DATA DIVISION.
003000 LINKAGE SECTION.
003100 01  DN1.
003200     02  DN2                     PICTURE X(5).
003300     02  DN3                     REDEFINES DN2   PICTURE 9(5).
003400 01  DN4.
003500     02  DN5.
003600         03  DN6                 PICTURE X(3).
003700         03  DN7                 PICTURE X(3).
003800         03  DN8                 REDEFINES DN7   PICTURE 9(3).
003900     02  DN9                     PICTURE XX.
004000 PROCEDURE DIVISION USING DN1, DN4.
004100 CALL-TEST-1.
004200     MOVE 12345 TO DN3.
004300 CALL-TEST-2.
004400     MOVE "YES" TO DN6.
004500     MOVE 987 TO DN8.
004600     MOVE "NO" TO DN9.
004700 IC217-EXIT.
004800     EXIT PROGRAM.
