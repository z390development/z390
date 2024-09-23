000100 IDENTIFICATION DIVISION.
000200 PROGRAM-ID.
000300     ST123A.
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
001600*        X-01                                                  *
001700*        X-02                                                  *
001800*        X-27                                                  *
001900*        X-55  - SYSTEM PRINTER NAME.                          *
002000*        X-69                                                  *
002100*        X-74                                                  *
002200*        X-75                                                  *
002300*        X-76                                                  *
002400*        X-82  - SOURCE COMPUTER NAME.                         *
002500*        X-83  - OBJECT COMPUTER NAME.                         *
002600*                                                              *
002700****************************************************************
002800*                                                              *
002900*    PROGRAM ST123A TESTS THE SORTING OF VARIABLE LENGTH       *
003000*    RECORDS.  THIS PROGRAM CAN BE USED ONLY IF LEVEL 2 OF THE *
003100*    SEQUENTIAL I-O MODULE IS SUPPORTED AS THE                 *
003200*    "RECORD IS VARYING IN SIZE" CLAUSE IS USED IN THE SD      *
003300*    ENTRY.  (ST123A WILL BE RUN AS PART OF THE SET ST122A,    *
003400*    ST123A, ST124A).                                          *
003500*                                                              *
003600****************************************************************
003700 ENVIRONMENT DIVISION.
003800 CONFIGURATION SECTION.
003900 SOURCE-COMPUTER.
004000     XXXXX082.
004100 OBJECT-COMPUTER.
004200     XXXXX083.
004300 INPUT-OUTPUT SECTION.
004400 FILE-CONTROL.
004500     SELECT PRINT-FILE ASSIGN TO
004600     XXXXX055.
004700     SELECT   SORTIN-1J ASSIGN TO
004800     XXXXD001.
004900     SELECT   SORTOUT-1J ASSIGN TO
005000     XXXXP002.
005100     SELECT   SORTFILE-1J ASSIGN TO
005200     XXXXX027.
005300 DATA DIVISION.
005400 FILE SECTION.
005500 FD  PRINT-FILE.
005600 01  PRINT-REC PICTURE X(120).
005700 01  DUMMY-RECORD PICTURE X(120).
005800 FD  SORTIN-1J
005900     LABEL RECORDS STANDARD
006000C    VALUE OF
006100C    XXXXX074
006200C    IS
006300C    XXXXX075
006400G    XXXXX069
006500     RECORD IS VARYING IN SIZE
006600     DATA RECORDS ARE SHORT-IN
006700                      MEDIUM-IN
006800                      LONG-IN.
006900 01  SHORT-IN           PICTURE X(50).
007000 01  MEDIUM-IN          PICTURE X(75).
007100 01  LONG-IN.
007200     02 FALSE-LENGTH-1  PICTURE X(25).
007300     02 FALSE-LENGTH-2  PICTURE A(20).
007400     02 FALSE-LENGTH-3  PICTURE 9(15).
007500     02 FALSE-LENGTH-4  PICTURE X(40).
007600 FD  SORTOUT-1J
007700     LABEL RECORDS ARE STANDARD
007800C    VALUE OF
007900C    XXXXX074
008000C    IS
008100C    XXXXX076
008200G    XXXXX069
008300     RECORD IS VARYING IN SIZE
008400     DATA RECORD SHORT-OUT
008500                 MEDIUM-OUT
008600                 LONG-OUT.
008700 01  SHORT-OUT.
008800     02 FAKE-LENGTH-1   PICTURE X(10).
008900     02 FAKE-LENGTH-2   PICTURE A(10).
009000     02 FAKE-LENGTH-3   PICTURE 9(10).
009100     02 FAKE-LENGTH-4   PICTURE X(20).
009200 01  MEDIUM-OUT         PICTURE X(75).
009300 01  LONG-OUT           PICTURE X(100).
009400 SD  SORTFILE-1J
009500     RECORD IS VARYING IN SIZE
009600     DATA RECORD SHORT-SORT
009700                 MEDIUM-SORT
009800                 LONG-SORT.
009900 01  SHORT-SORT.
010000     02 SHORT-NON-KEY   PICTURE XX.
010100     02 SHORT-KEY-1     PICTURE X(10).
010200     02 SHORT-KEY-2     PICTURE X(38).
010300 01  MEDIUM-SORT.
010400     02 MEDIUM-NON-KEY  PICTURE XX.
010500     02 MEDIUM-KEY-1    PICTURE X(10).
010600     02 MEDIUM-KEY-2    PICTURE X(38).
010700     02 MEDIUM-FILLER   PICTURE X(25).
010800 01  LONG-SORT.
010900     02 LONG-NON-KEY    PICTURE XX.
011000     02 LONG-KEY-1      PICTURE X(10).
011100     02 LONG-KEY-2      PICTURE X(38).
011200     02 LONG-FILLER     PICTURE X(50).
011300 PROCEDURE    DIVISION.
011400 SORT-PARAGRAPH.
011500     SORT     SORTFILE-1J
011600              DESCENDING KEY
011700              MEDIUM-KEY-1
011800              MEDIUM-KEY-2
011900              USING SORTIN-1J
012000              GIVING SORTOUT-1J.
012100     STOP     RUN.
