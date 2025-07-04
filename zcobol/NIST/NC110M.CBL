000100 IDENTIFICATION DIVISION.
000200 PROGRAM-ID.
000300     NC110M.
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
002200*    THE PROCEDURE DIVISION OF NC110M CONSISTS ENTIRELY OF
002300*    PARAGRAPH NAMES AND "DISPLAY" LITERAL STATEMENTS.
002400*
002500 ENVIRONMENT DIVISION.
002600 CONFIGURATION SECTION.
002700 SOURCE-COMPUTER.
002800     XXXXX082.
002900 OBJECT-COMPUTER.
003000     XXXXX083.
003100 DATA DIVISION.
003200 PROCEDURE    DIVISION.
003300 HEADER-PRINT.
003400     DISPLAY " ".
003500     DISPLAY
003600     " FOR OFFICIAL USE ONLY                                    ".
003700     DISPLAY
003800     "     OFFICIAL COBOL COMPILER TEST SYSTEM.     ".
003900     DISPLAY
004000     "                TEST RESULTS SET- NC110M                  ".
004100     DISPLAY
004200     "                                                          ".
004300     DISPLAY
004400     " FOR OFFICIAL USE ONLY "
004500     "                                                         ".
004600     DISPLAY
004700     "COPYRIGHT  1985".
004800     DISPLAY
004900     "                                                          ".
005000     DISPLAY
005100     " FEATURE     RESULTS AND                                  ".
005200     DISPLAY
005300     " TESTED      REMARKS                                      ".
005400     DISPLAY
005500     "                                                          ".
005600     DISPLAY
005700     " ---------------------------------------------------------".
005800 GO-TEST.
005900     DISPLAY
006000     " GO TO       THIS TEST PASSES UNLESS FAIL APPEARS BELOW.  ".
006100     GO       TO PERFORM-TEST.
006200 GO-FAIL.
006300     DISPLAY
006400     "             FAIL".
006500 PERFORM-TEST.
006600     DISPLAY
006700     " PERFORM     THIS TEST FAILS UNLESS PASS APPEARS BELOW.   ".
006800     PERFORM  PASS.
006900 ENDER-PRINT.
007000     DISPLAY
007100     " ---------------------------------------------------------".
007200     DISPLAY
007300     SPACE.
007400     DISPLAY
007500     "                  END OF TEST - NC110M                    ".
007600     DISPLAY
007700     "                  CHECK FOR ERRORS                        ".
007800     DISPLAY
007900     "                                                          ".
008000     DISPLAY
008100     " FOR OFFICIAL USE ONLY "
008200     DISPLAY
008300     "COPYRIGHT  1985".
008400     DISPLAY
008500     "       NTIS DISTRIBUTION COBOL 1985               ".
008600     STOP RUN.
008700 PASS.
008800     DISPLAY
008900     "             PASS".
