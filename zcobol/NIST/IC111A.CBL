000100 IDENTIFICATION DIVISION.
000200 PROGRAM-ID.
000300     IC111A.
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
002100*        THE SUBPROGRAM IC111 IS THE LAST SUBPROGRAM CALLED
002200*    IN A SEQUENCE OF SUBPROGRAM CALLS WHICH IS STARTED IN
002300*    MAIN PROGRAM IC108.  THE SUBPROGRAM IC111 IS CALLED BY
002400*    THE SUBPROGRAM IC110.
002500 ENVIRONMENT DIVISION.
002600 CONFIGURATION SECTION.
002700 SOURCE-COMPUTER.
002800     XXXXX082.
002900 OBJECT-COMPUTER.
003000     XXXXX083.
003100 DATA DIVISION.
003200 LINKAGE SECTION.
003300 77  LS1 PICTURE X.
003400 77  LS2 PICTURE X.
003500 01  GRP-01.
003600     02  SUB-CALLED.
003700         03  DN1 PICTURE X(6).
003800         03  DN2 PICTURE X(6).
003900         03  DN3 PICTURE X(6).
004000     02  TIMES-CALLED.
004100         03  DN4 PICTURE S999.
004200         03  DN5 PICTURE S999.
004300         03  DN6 PICTURE S999.
004400     02  SPECIAL-FLAGS.
004500         03  DN7 PICTURE X.
004600         03  DN8 PICTURE X.
004700         03  DN9 PICTURE X.
004800 PROCEDURE DIVISION USING LS1 GRP-01 LS2.
004900 SECT-IC111-0001 SECTION.
005000 PARA-IC111.
005100     MOVE "IC111A" TO DN3.
005200     ADD 1 TO DN6.
005300     MOVE "A" TO LS2.
005400     MOVE "A" TO LS1.
005500 EXIT-IC111.
005600     EXIT PROGRAM.
