000100 IDENTIFICATION DIVISION.
000200 PROGRAM-ID.
000300     IC205A.
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
002100*        THE SUBPROGRAM IC205 TESTS THE USE OF THE CANCEL      *
002200*    STATEMENT WITHIN A SUBPROGRAM.  THIS SUBPROGRAM IS        *
002300*    CALLED BY IC203 AND CALLS THE SUBPROGRAMS IC204 AND IC206.*
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
004100 77  ID1 PICTURE X(6) VALUE "IC204A".
004200 77  DN2 PICTURE S9(8) USAGE COMP VALUE ZERO.
004300 LINKAGE SECTION.
004400 01  TABLE-1.
004500     02  T-DN1 PIC XXX.
004600     02  T-DN2 PIC 99.
004700     02  T-DN3 PIC X(5).
004800 77  DN1 PICTURE S999.
004900 01  TABLE-2.
005000     02  TV-1 PIC X.
005100     02  TV-2 PIC X.
005200 PROCEDURE DIVISION USING TABLE-1, TABLE-2, DN1.
005300 CNCL-TEST-05.
005400     CALL "IC206A" USING DN2.
005500     CALL "IC206A" USING DN2.
005600     CALL "IC206A" USING DN2.
005700     MOVE "X" TO TV-1.
005800     IF DN2 EQUAL TO 3
005900         MOVE "A" TO TV-1.
006000     CANCEL "IC206A".
006100     MOVE ZERO TO DN2.
006200     CALL "IC206A" USING DN2.
006300     IF DN2 NOT EQUAL TO 1
006400         MOVE "Y" TO TV-2,
006500         GO TO CNCL-TEST-06.
006600     MOVE "B" TO TV-2.
006700 CNCL-TEST-06.
006800     CANCEL ID1.
006900 EXIT-IC205.
007000     EXIT PROGRAM.
