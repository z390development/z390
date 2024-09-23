000100 IDENTIFICATION DIVISION.
000200
000400 PROGRAM-ID.
000500     EXEC85.
000600 INSTALLATION.
000700     "ON-SITE VALIDATION, NATIONAL INSTITUTE OF STD & TECH.     ".
000800     "COBOL 85 VERSION 4.2, Apr  1993 SSVG                      ".
000900 ENVIRONMENT DIVISION.
001000
001100****************************************************************
001200*                                                              *
001300*    VALIDATION FOR:-                                          *
001400*                                                              *
001500*    "ON-SITE VALIDATION, NATIONAL INSTITUTE OF STD & TECH.     ".
001600*                                                              *
001700*    "COBOL 85 VERSION 4.2, Apr  1993 SSVG                      ".
001800*                                                              *
001900****************************************************************
002000 CONFIGURATION SECTION.
002100
002200 SPECIAL-NAMES.
002300 INPUT-OUTPUT SECTION.
002400 FILE-CONTROL.
002500     SELECT  OPTIONAL POPULATION-FILE
002600     ASSIGN TO
002700     XXXXX001.
002800     SELECT  SOURCE-COBOL-PROGRAMS
002900     ASSIGN TO
003000     XXXXX002
003100     ORGANIZATION SEQUENTIAL.
003200     SELECT  UPDATED-POPULATION-FILE
003300     ASSIGN TO
003400     XXXXX003.
003500     SELECT  PRINT-FILE
003600     ASSIGN TO
003700     XXXXX055.
003800     SELECT  CONTROL-CARD-FILE
003900     ASSIGN TO
004000     XXXXX058.
004100 DATA DIVISION.
004200 FILE SECTION.
004300 FD  POPULATION-FILE.
004400*    RECORD CONTAINS 2400 CHARACTERS.
004500 01  SOURCE-IN-2400.
004600     02 SOURCE-IN                    PIC X(80).
004700*                                              OCCURS 30.
004800 FD  CONTROL-CARD-FILE.
004900 01  CONTROL-RECORD                  PIC X(80).
005000 FD  PRINT-FILE.
005100 01  PRINT-REC.
005200   05        FILLER                  PIC X.
005300   05        PRINT-DATA              PIC X(131).
005400 FD  SOURCE-COBOL-PROGRAMS
005500     BLOCK CONTAINS 1 RECORDS.
005600 01  CT-OUT.
005700     02 FILLER PIC X(72).
005800     02 FILLER PIC X(8).
005900 FD  UPDATED-POPULATION-FILE
006000     RECORD CONTAINS 2400 CHARACTERS.
006100 01  UPDATED-SOURCE-OUT-2400.
006200     02 UD-SOURCE-OUT                PIC X(80)  OCCURS 30.
006300
006400 WORKING-STORAGE SECTION.
006500
006600 01  FILLER                          PIC X(40)  VALUE
006700            "NEWEXEC WORKING-STORAGE STARTS HERE ==->".
006800 01  BLOCK-TYPE                      PIC X(5).
006900 01  SUB1                            PIC S9(3)  COMP.
007000 01  SUB2                            PIC S9(3)  COMP.
007100 01  SUB3                            PIC S9(3)  COMP.
007200 01  SUB4                            PIC S9(3)  COMP.
007300 01  SUB5                            PIC S9(3)  COMP.
007400 01  SUB6                            PIC S9(3)  COMP.
007500 01  SUB7                            PIC S9(3)  COMP.
007600 01  WA-ERR-IND                      PIC 9 VALUE ZEROES.
007700 01  WA-FIRST-IND                    PIC 9 VALUE ZEROES.
007800 01  WA-ZCARD-TABLE.
007900   05        WA-ZCARD                OCCURS 10
008000                                     PIC X(60).
008100 01  WA-TOP-OF-PAGE-LINE.
008200   05        FILLER                  PIC X(4)   VALUE SPACES.
008300   05        WA-VERSION.
008400     07      WA-VERSION-TEXT         PIC X(22)  VALUE
008500            "CCVS85 VERSION NUMBER ".
008600     07      WA-VERSION-NUM          PIC X(3) VALUE SPACES.
008700   05        WA-RELEASE.
008800     07      WA-RELEASE-TEXT         PIC X(14)  VALUE
008900            ", RELEASED ON ".
009000     07      WA-VERSION-DATE         PIC X(11) VALUE SPACES.
009100   05        FILLER                  PIC X(4)   VALUE SPACES.
009200   05        WA-COMPANY-AND-COMPILER PIC X(30) VALUE SPACES.
009300   05        FILLER                  PIC X(5)   VALUE SPACES.
009400   05        WA-DATE                 PIC XXBXXBXX.
009500   05        FILLER                  PIC X(4)   VALUE SPACES.
009600   05        FILLER                  PIC X(5)   VALUE "PAGE ".
009700   05        WA-PAGE-CT              PIC Z(5)9.
009800
009900 01  WA-ACCT-LINE-1.
010000   05        FILLER                  PIC X(19)  VALUE
010100            " ** END OF PROGRAM ".
010200   05        WA-CURRENT-PROG         PIC X(6).
010300   05        FILLER                  PIC X(32)  VALUE
010400            " FOUND,  COBOL LINES PROCESSED: ".
010500   05        WA-LINES-COBOL          PIC Z(5)9.
010600 01  WA-ACCT-LINE-2.
010700   05        FILLER                  PIC X(19)  VALUE
010800            " ** LINES INSERTED ".
010900   05        WA-LINES-INSERTED       PIC Z(5)9.
011000   05        FILLER                  PIC X(19)  VALUE
011100            " ** LINES REPLACED ".
011200   05        WA-LINES-REPLACED       PIC Z(5)9.
011300   05        FILLER                  PIC X(19)  VALUE
011400            " ** LINES DELETED  ".
011500   05        WA-LINES-DELETED        PIC Z(5)9.
011600 01  WA-ACCT-LINE-3.
011700   05        FILLER                  PIC X(18)  VALUE
011800            " ** OPTIONAL CODE ".
011900   05        WA-OPTIONAL-CODE        PIC X(8).
012000   05        WA-CODE-REMOVED         PIC Z(5)9.
012100   05        WA-CODE-KILLED          PIC X(21)  VALUE
012200            " ** COMMENTS DELETED ".
012300   05        WA-COMMENTS-DEL         PIC Z(5)9.
012400 01  WA-FINAL-LINE-1.
012500   05        FILLER                  PIC X(34)  VALUE
012600            " ** END OF POPULATION FILE REACHED".
012700   05        FILLER                  PIC X(27)  VALUE
012800            " NUMBER OF PROGRAMS FOUND: ".
012900   05        WA-PROGS-FOUND          PIC Z(5)9.
013000 01  WA-FINAL-LINE-2.
013100   05        FILLER                  PIC X(47)  VALUE
013200            " ** NUMBER OF PROGRAMS WRITTEN TO SOURCE FILE: ".
013300   05        WA-SOURCE-PROGS         PIC Z(5)9.
013400 01  WA-FINAL-LINE-3.
013500   05        FILLER                  PIC X(48)  VALUE
013600            " ** NUMBER OF PROGRAMS WRITTEN TO NEW POPULATION".
013700   05        FILLER                  PIC X(7)   VALUE " FILE: ".
013800   05        WA-NEWPOP-PROGS         PIC Z(5)9.
013900 01  WB-CONTROL-DATA.
014000   05        WB-FILL                 PIC X(80).
014100   05        FILLER                  REDEFINES  WB-FILL.
014200     10      WB-3                    PIC X(3).
014300     10      FILLER                  PIC X(77).
014400   05        FILLER                  REDEFINES  WB-FILL.
014500     10      WB-4                    PIC X(4).
014600     10      WB-NN                   PIC 99.
014700     10      FILLER                  PIC X.
014800     10      WB-X                    PIC X.
014900     10      FILLER                  PIC X(72).
015000   05        FILLER                  REDEFINES  WB-FILL.
015100     10      WB-6                    PIC X(6).
015200     10      FILLER                  PIC X(74).
015300   05        FILLER                  REDEFINES  WB-FILL.
015400     10      WB-7                    PIC X(7).
015500     10      FILLER                  PIC X(73).
015600   05        FILLER                  REDEFINES  WB-FILL.
015700     10      WB-8                    PIC X(8).
015800     10      FILLER                  PIC X(72).
015900   05        FILLER                  REDEFINES  WB-FILL.
016000     10      WB-9                    PIC X(9).
016100     10      FILLER                  PIC X(71).
016200   05        FILLER                  REDEFINES  WB-FILL.
016300     10      WB-10                   PIC X(10).
016400     10      FILLER                  PIC X(70).
016500   05        FILLER                  REDEFINES  WB-FILL.
016600     10      WB-11                   PIC X(11).
016700     10      FILLER                  PIC X(69).
016800   05        FILLER                  REDEFINES  WB-FILL.
016900     10      WB-12                   PIC X(12).
017000     10      FILLER                  PIC X.
017100     10      WB-PROG                 PIC X(5).
017200     10      FILLER                  PIC X(62).
017300   05        FILLER                  REDEFINES  WB-FILL.
017400     10      WB-13                   PIC X(13).
017500     10      FILLER                  PIC X(67).
017600   05        FILLER                  REDEFINES  WB-FILL.
017700     10      WB-14                   PIC X(14).
017800     10      FILLER                  PIC X.
017900     10      WB-MODULE               PIC XX.
018000     10      FILLER                  PIC X.
018100     10      WB-LEVEL                PIC X.
018200     10      FILLER                  PIC X(61).
018300   05        FILLER                  REDEFINES  WB-FILL.
018400     10      WB-15                   PIC X(15).
018500     10      FILLER                  PIC X(65).
018600   05        FILLER                  REDEFINES  WB-FILL.
018700     10      WB-16                   PIC X(16).
018800     10      FILLER                  PIC X(64).
018900   05        WB-X-CARD               REDEFINES  WB-FILL.
019000     10      WB-X-HYPHEN             PIC XX.
019100     10      WB-X-CARD-NUM           PIC 9(3).
019200     10      WB-PROG-POS.
019300       15    WB-PROG-POS-NUM         PIC 99.
019400     10      FILLER                  PIC X.
019500     10      WB-SUBS-TEXT            PIC X(60).
019600     10      FILLER                  PIC X(12).
019700   05        WB-START-CARD           REDEFINES  WB-FILL.
019800     10      WB-STAR-START           PIC X(6).
019900     10      FILLER                  PIC X.
020000     10      WB-UPDATE-PROG          PIC X(6).
020100     10      FILLER                  PIC X.
020200     10      WB-RENUMBER             PIC X.
020300     10      FILLER                  PIC X(65).
020400   05        WB-LINE-UPDATE          REDEFINES  WB-FILL.
020500     10      WB-SEQ-1                PIC X(6).
020600     10      WB-COBOL-LINE           PIC X(74).
020700     10      FILLER                  REDEFINES  WB-COBOL-LINE.
020800       15    WB-COL-7                PIC X.
020900       15    FILLER                  PIC X(73).
021000     10      FILLER                  REDEFINES  WB-COBOL-LINE.
021100       15    WB-CHAR                 PIC X.
021200       15    WB-SEQ-2                PIC X(6).
021300/
021400 01  WC-CURRENT-POP-RECORD.
021500   05        WC-1.
021600     10      WC-END-OF-POPFILE       PIC X(16).
021700     10      FILLER                  PIC X(64).
021800   05        WC-HEADER               REDEFINES WC-1.
021900     10      WC-STAR-HEADER          PIC X(7).
022000     10      FILLER                  PIC X.
022100     10      WC-COBOL                PIC X(5).
022200     10      FILLER                  PIC X.
022300     10      WC-PROG-ID.
022400      12     WC-PROG-ID-1-5.
022500       15    WC-PROG-ID-1-4.
022600        18   WC-MODULE               PIC XX.
022700        18   WC-LEVEL                PIC X.
022800        18   FILLER                  PIC X.
022900       15    FILLER                  PIC X.
023000      12     WC-PROG-ID-6            PIC X.
023100     10      FILLER                  PIC X.
023200     10      WC-SUBPRG               PIC X(6).
023300     10      FILLER                  PIC X.
023400     10      WC-PROG2ID.
023500      12     WC-PROG2ID-1-5          PIC X(5).
023600      12     FILLER                  PIC X.
023700     10      FILLER                  PIC X(46).
023800   05        FILLER                  REDEFINES WC-1.
023900     10      WC-1-72.
024000       15    WC-6.
024100         20  WC-STAR                 PIC X.
024200         20  FILLER                  PIC X(5).
024300       15    FILLER                  REDEFINES  WC-6.
024400         20  WC-1-5                  PIC X(5).
024500         20  FILLER                  PIC X.
024600       15    WC-COL-7                PIC X.
024700       15    WC-COL-8                PIC X.
024800       15    FILLER                  PIC X(3).
024900       15    WC-SUB-DATA.
025000         20  WC-12-15                PIC X(4).
025100         20  FILLER                  PIC X.
025200         20  WC-17-19                PIC 9(3).
025300         20  WC-20                   PIC X.
025400         20  FILLER                  PIC X(52).
025500     10      WC-73-80                PIC X(8).
025600
025700 01  WD-SOURCE-REC.
025800   05        WD-1.
025900     10      FILLER                  PIC X(6).
026000     10      WD-HEADER               PIC X(74).
026100
026200 01  WE-PRINT-DATA.
026300   05        WE-COBOL-LINE           PIC X(80).
026400   05        FILLER                  PIC X      VALUE SPACE.
026500   05        WE-X-CARD               PIC X(9).
026600   05        FILLER                  PIC XX     VALUE SPACES.
026700   05        WE-CHANGE-TYPE          PIC X(12).
026800
026900 01  WF-PROGRAM-SELECTED-TABLE.
027000   05        WF-PROGRAM-SELECTED     PIC X(5)   OCCURS 50.
027100
027200 01  WG-MODULE-SELECTED-TABLE.
027300   05        FILLER                             OCCURS 10.
027400     10      WG-MODULE-SELECTED      PIC XX.
027500     10      WG-MODULE-LEVEL         PIC X.
027600
027700 01  WV-PRINT-MISCELLANEOUS.
027800   05        WV-OPTION-HEADING       PIC X(25)  VALUE
027900            " OPTION SWITCH SETTINGS -".
028000   05        WV-OPT-1                PIC X(40)  VALUE
028100         " 0                 1                   2".
028200   05        WV-OPT-2                PIC X(52)  VALUE
028300         " 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6".
028400   05        WV-OPT-SWITCHES.
028500     10      FILLER                  PIC X      VALUE SPACE.
028600     10      FILLER                  OCCURS 26.
028700       15    WV-OPT                  PIC X.
028800       15    FILLER                  PIC X.
028900 01  WX-X-CARD-TABLE.
029000   05        WX-X-CARD               OCCURS 200.
029100     10      WX-X-CHAR               PIC X
029200                                     OCCURS 60.
029300 01  WX-PROG-POS-TABLE.
029400   05        WX-PROG-POS             OCCURS 200
029500                                     PIC 99.
029600 01  WY-SWITCHES.
029700   05        WY-OPTION-SWITCHES.
029800     10      WY-OPT-SW-1             PIC X.
029900     10      WY-OPT-SW-2             PIC X.
030000     10      WY-OPT-SW-3             PIC X.
030100     10      WY-OPT-SW-4             PIC X.
030200     10      WY-OPT-SW-5             PIC X.
030300     10      WY-OPT-SW-6             PIC X.
030400     10      WY-OPT-SW-7             PIC X.
030500     10      WY-OPT-SW-8             PIC X.
030600     10      WY-OPT-SW-9             PIC X.
030700     10      WY-OPT-SW-10            PIC X.
030800     10      WY-OPT-SW-11            PIC X.
030900     10      WY-OPT-SW-12            PIC X.
031000     10      WY-OPT-SW-13            PIC X.
031100     10      WY-OPT-SW-14            PIC X.
031200     10      WY-OPT-SW-15            PIC X.
031300     10      WY-OPT-SW-16            PIC X.
031400     10      WY-OPT-SW-17            PIC X.
031500     10      WY-OPT-SW-18            PIC X.
031600     10      WY-OPT-SW-19            PIC X.
031700     10      WY-OPT-SW-20            PIC X.
031800     10      WY-OPT-SW-21            PIC X.
031900     10      WY-OPT-SW-22            PIC X.
032000     10      WY-OPT-SW-23            PIC X.
032100     10      WY-OPT-SW-24            PIC X.
032200     10      WY-OPT-SW-25            PIC X.
032300     10      WY-OPT-SW-26            PIC X.
032400   05        FILLER                  REDEFINES WY-OPTION-SWITCHES.
032500     10      WY-OPT-SW               PIC X
032600                                     OCCURS 26.
032700   05        WY-PRINT-SWITCHES.
032800     10      WY-EXTRACT-ALL          PIC X.
032900     10      WY-EXTRACT-AUTO         PIC X.
033000     10      WY-EXTRACT-MAN          PIC X.
033100     10      WY-KILL-DELETIONS       PIC X.
033200     10      WY-LIST-NO-UPDATES      PIC X.
033300     10      WY-LIST-X-CARDS         PIC X.
033400     10      WY-LIST-PROGRAMS        PIC X.
033500     10      WY-LIST-COMPACT         PIC X.
033600     10      WY-NO-DATA              PIC X.
033700     10      WY-NO-LIBRARY           PIC X.
033800     10      WY-NO-SOURCE            PIC X.
033900     10      WY-REMOVE-COMMENTS      PIC X.
034000     10      WY-NEW-POP              PIC X.
034100     10      WY-SELECT-PROG          PIC X.
034200     10      WY-SELECT-MODULE        PIC X.
034300     10      WY-SELECT-LEVEL         PIC X.
034400
034500 01  WZ-MISCELLANEOUS.
034600   05        WZ-PROGRAM-SELECTED     PIC X.
034700   05        WZ-END-OF-POPFILE       PIC X.
034800   05        WZ-FULL-STOP            PIC X.
034900   05        WZ-DONT-READ-POPFILE    PIC X.
035000   05        WZ-UPDATE-THIS-PROG     PIC X.
035100   05        WZ-REPLACE-FLAG         PIC X.
035200   05        WZ-LINE-UPDATE          PIC X.
035300   05        WZ-RESEQUENCE-THIS      PIC X.
035400   05        WZ-RESEQUENCE-NEXT      PIC X.
035500   05        WZ-END-OF-UPDATES       PIC X.
035600   05        WZ-OPTIONAL-SELECTED    PIC X.
035700   05        WZ-DELETE-FLAG          PIC X.
035800   05        WZ-NOT-THIS-COMMENT     PIC X.
035900   05        WZ-CURRENT-HEADER       PIC X(5).
036000   05        WZ-INVALID-DATA.
036100     10      FILLER                  PIC X(20).
036200     10      WZ-ERROR-MESSAGE        PIC X(60).
036300   05        WZ-CURRENT-UPD-PROG.
036400     10      WZ-UPD-PROG-CHAR        PIC X.
036500     10      FILLER                  PIC X(5).
036600   05        WZ-CURRENT-MAIN-PROG.
036700     10      WZ-MAIN-PROG-CHAR       PIC X      OCCURS 6.
036800   05        WZ-PROG-BREAK.
036900     10      WZ-1CHAR                PIC X      OCCURS 6.
037000   05        WZ-CURRENT-POP-PROG.
037100     10      FILLER                  PIC X(5).
037200     10      WZ-PROG-ID-6            PIC X.
037300   05        WZ-MAIN-PROG-FLAG       PIC X.
037400   05        WZ-LINES-COBOL          PIC 9(6).
037500   05        WZ-LINES-INSERTED       PIC 9(6).
037600   05        WZ-LINES-REPLACED       PIC 9(6).
037700   05        WZ-LINES-DELETED        PIC 9(6).
037800   05        WZ-COMMENTS-DELETED     PIC 9(6).
037900   05        WZ-CODE-REMOVED         PIC 9(6).
038000   05        WZ-SOURCE-PROGS         PIC 9(6).
038100   05        WZ-NEWPOP-PROGS         PIC 9(6).
038200   05        WZ-PROGS-FOUND          PIC 9(6).
038300   05        WZ-COMMENTS-DEL         PIC 9(6).
038400   05        WZ-SEQ-NO               PIC 9(6).
038500   05        WZ-SAVE-POP-RECORD.
038600     10      WZ-SAVE-SEQ             PIC X(6).
038700     10      FILLER                  PIC X(5).
038800     10      WZ-SAVE-12-20.
038900       15    WZ-SAVE-12-15           PIC X(4).
039000       15    FILLER                  PIC X(5).
039100     10      FILLER                  PIC X(60).
039200   05        WZ-PAGE-CT              PIC 9(6).
039300   05        WZ-LINE-CT              PIC 9(6).
039400   05        WZ-MODULE               PIC XX.
039500   05        WZ-LEVEL                PIC X.
039600   05        WZ-PRINT-HOLD           PIC X(132).
039700   05        WZ-X-CARD.
039800     10      WZ-X-CHAR               PIC X
039900                                     OCCURS 60.
040000   05        WZ-WITHIN-DELETE-SERIES-FLAG  PIC X.
040100 01  WZ-VERSION-CARD.
040200     10  FILLER                      PIC X(55) VALUE
040300     "CCVS85  VERSION 4.2   01 OCT 1992 0032                 ".
040400 01  WZ-VERSION-CONTROL REDEFINES WZ-VERSION-CARD.
040500     10      FILLER                  PIC X(16).
040600     10      WZ-VERSION-NUM          PIC X(3).
040700     10      FILLER                  PIC X(3).
040800     10      WZ-VERSION-DATE         PIC X(11).
040900
041000/
041100 PROCEDURE DIVISION.
041200*==================
041300*
041400 A10-MAIN SECTION.
041500*================
041600*
041700****************************************************************
041800*    THIS IS THE HIGHEST LEVEL CONTROL MODULE                  *
041900*                                                              *
042000****************************************************************
042100 A10-1-MAIN.
042200     PERFORM B10-INITIALISE.
042300
042400     PERFORM C10-PROCESS-MONITOR.
042500
042600     PERFORM D10-MERGE-UPDATE-CARDS.
042700
042800     PERFORM E10-TERMINATE.
042900
043000 A10-EXIT.
043100     EXIT.
043200
043300/
043400 B10-INITIALISE SECTION.
043500*======================
043600*
043700****************************************************************
043800* THIS SECTION INITIALIZES THE OPTION SWITCH AND X-CARD FIELDS *
043900* PRIOR TO READING IN CONTROL CARD FILE.                       *
044000*                                                              *
044100*                                                              *
044200*                                                              *
044300*                                                              *
044400****************************************************************
044500 B10-1-INIT-OPTION-SWITCHES.
044600     MOVE    SPACES  TO WZ-MISCELLANEOUS.
044700     MOVE    SPACES  TO WF-PROGRAM-SELECTED-TABLE.
044800     MOVE    SPACES  TO WG-MODULE-SELECTED-TABLE.
044900     MOVE    SPACES  TO WY-SWITCHES.
045000     MOVE    "A"     TO WY-OPT-SW-1.
045100     MOVE    "E"     TO WY-OPT-SW-2.
045200     MOVE    "H"     TO WY-OPT-SW-3.
045300     MOVE    "L"     TO WY-OPT-SW-4.
045400     MOVE    "Y"     TO WY-OPT-SW-7.
045500     MOVE    "T"     TO WY-OPT-SW-11.
045600
045700 B10-2-INIT-X-CARDS.
045800     MOVE    ZERO TO SUB1.
045900     MOVE    ZERO TO SUB6.
046000     MOVE    ZERO TO SUB7.
046100     MOVE    1    TO SUB5.
046200     PERFORM B20-INIT-X-CARDS 200 TIMES.
046300     MOVE   "    OMITTED" TO WX-X-CARD (84).
046400     MOVE    ZERO TO WZ-LINES-COBOL.
046500     MOVE    ZERO TO WZ-LINES-INSERTED.
046600     MOVE    ZERO TO WZ-LINES-REPLACED.
046700     MOVE    ZERO TO WZ-LINES-DELETED.
046800     MOVE    ZERO TO WZ-COMMENTS-DELETED.
046900     MOVE    ZERO TO WZ-CODE-REMOVED.
047000     MOVE    ZERO TO WZ-SOURCE-PROGS.
047100     MOVE    ZERO TO WZ-NEWPOP-PROGS.
047200     MOVE    ZERO TO WZ-PROGS-FOUND.
047300     MOVE    ZERO TO WZ-COMMENTS-DEL.
047400     MOVE    ZERO TO WZ-SEQ-NO.
047500     MOVE    ZERO TO WZ-PAGE-CT.
047600     MOVE    ZERO TO WZ-LINE-CT.
047700     ACCEPT WA-DATE FROM DATE.
047800 B10-EXIT.
047900     EXIT.
048000
048100
048200
048300
048400 B20-INIT-X-CARDS SECTION.
048500*========================
048600 B20-1-INIT.
048700     ADD     1 TO SUB1.
048800     MOVE   "**** X-CARD UNDEFINED ****" TO WX-X-CARD (SUB1).
048900     MOVE    ZERO TO WX-PROG-POS (SUB1).
049000
049100 B20-EXIT.
049200     EXIT.
049300/
049400 C10-PROCESS-MONITOR SECTION.
049500*===========================
049600
049700****************************************************************
049800*    THIS SECTION PROCESSES THE RECORDS COMMENCING WITH "*"    *
049900*    AND "X-" (THE MONITOR PART OF THE INPUT FILE ) AND READS  *
050000*    THE FIRST "*START" UPDATE RECORD.                         *
050100*                                                              *
050200*    PERFORMED BY    A10-MAIN                                  *
050300*    PERFORMS        C20-PROCESS-STAR-CARDS                    *
050400*                    C30-CHECK-COMBINATIONS                    *
050500*                    C40-PROCESS-X-CARDS                       *
050600****************************************************************
050700 C10-1-OPEN-FILES.
050800     OPEN    OUTPUT  PRINT-FILE.
050900     MOVE    SPACES TO PRINT-REC.
051000     OPEN    INPUT   CONTROL-CARD-FILE.
051100     READ    CONTROL-CARD-FILE INTO  WB-CONTROL-DATA
051200             AT END  MOVE "CONTROL-CARD-FILE IS EMPTY"
051300                  TO PRINT-DATA
051400             PERFORM  X20-PRINT-DETAIL
051500             STOP RUN.
051600     PERFORM C20-PROCESS-STAR-CARDS
051700             UNTIL   WB-X-HYPHEN = "X-".
051800     PERFORM C30-CHECK-COMBINATIONS.
051900     PERFORM C40-PROCESS-X-CARDS
052000             UNTIL   WB-12 = "*END-MONITOR".
052100
052200     PERFORM C50-PRINT-OPTIONS.
052300
052400 C10-10-GET-FIRST-START-CARD.
052500     READ    CONTROL-CARD-FILE INTO  WB-CONTROL-DATA
052600             AT END  MOVE "NO BEGIN-UPDATE CARD FOUND"
052700                  TO PRINT-DATA
052800             PERFORM  X20-PRINT-DETAIL
052900             STOP RUN.
053000     IF WB-13 NOT = "*BEGIN-UPDATE"
053100             MOVE "*BEGIN-UPDATE CARD MISSING"
053200                  TO PRINT-DATA
053300             PERFORM X20-PRINT-DETAIL
053400             STOP RUN.
053500     READ    CONTROL-CARD-FILE INTO  WB-CONTROL-DATA
053600             AT END  MOVE "NO END-UPDATE CARD FOUND"
053700                  TO PRINT-DATA
053800             PERFORM  X20-PRINT-DETAIL
053900             STOP RUN.
054000     IF      WB-11 = "*END-UPDATE"
054100             MOVE   "Y" TO WZ-END-OF-UPDATES
054200             GO TO   C10-EXIT.
054300     IF      WB-STAR-START = "*START"
054400             PERFORM C60-START-CARD.
054500
054600 C10-EXIT.
054700     EXIT.
054800/
054900 C20-PROCESS-STAR-CARDS SECTION.
055000*==============================
055100*
055200****************************************************************
055300*    THIS SECTION PROCESSES THE INPUT PARAMETER FILE RECORDS   *
055400*    WHICH START WITH AN ASTERISK IN COLUMN 1 AND SETS VARIOUS *
055500*    FLAGS WHICH CONTROL THE WAY THIS EXECUTIVE ROUTINE WORKS. *
055600*                                                              *
055700*    PERFORMED BY  C10-PROCESS-MONITOR                         *
055800*    PERFORMS      C25-SET-FLAGS                               *
055900****************************************************************
056000 C20-1-UPDATE-CHECK.
056100     IF      WB-13 = "*BEGIN-UPDATE"
056200             MOVE    WB-13 TO WZ-INVALID-DATA
056300             MOVE   "ENCOUNTERED BEFORE *END-MONITOR CARD"
056400                  TO WZ-ERROR-MESSAGE
056500             MOVE    WZ-INVALID-DATA TO PRINT-DATA
056600             PERFORM X20-PRINT-DETAIL
056700             STOP RUN.
056800
056900     IF      WB-6  = "*START"
057000             MOVE    WB-6  TO WZ-INVALID-DATA
057100             MOVE   "ENCOUNTERED BEFORE *END-MONITOR CARD"
057200                  TO WZ-ERROR-MESSAGE
057300             MOVE    WZ-INVALID-DATA TO PRINT-DATA
057400             PERFORM X20-PRINT-DETAIL
057500             STOP RUN.
057600
057700     IF      WB-11 = "*END-UPDATE"
057800             MOVE    WB-11 TO WZ-INVALID-DATA
057900             MOVE   "ENCOUNTERED BEFORE *END-MONITOR CARD"
058000                  TO WZ-ERROR-MESSAGE
058100             MOVE    WZ-INVALID-DATA TO PRINT-DATA
058200             PERFORM X20-PRINT-DETAIL
058300             STOP RUN.
058400
058500     PERFORM C25-SET-FLAGS.
058600
058700 C20-EXIT.
058800     EXIT.
058900
059000/
059100 C25-SET-FLAGS SECTION.
059200*=====================
059300 C25-1.
059400     MOVE    WB-CONTROL-DATA TO PRINT-DATA.
059500     PERFORM X20-PRINT-DETAIL.
059600     IF      WB-12 = "*EXTRACT-ALL"
059700             MOVE   "Y" TO WY-EXTRACT-ALL.
059800     IF      WB-13 = "*EXTRACT-AUTO"
059900             MOVE   "Y" TO WY-EXTRACT-AUTO.
060000     IF      WB-12 = "*EXTRACT-MAN"
060100             MOVE   "Y" TO WY-EXTRACT-MAN.
060200     IF      WB-15 = "*KILL-DELETIONS"
060300             MOVE   "Y" TO WY-KILL-DELETIONS.
060400     IF      WB-16 = "*LIST NO-UPDATES"
060500             MOVE   "Y" TO WY-LIST-NO-UPDATES.
060600     IF      WB-13 = "*LIST X-CARDS"
060700             MOVE   "Y" TO WY-LIST-X-CARDS.
060800     IF      WB-14 = "*LIST PROGRAMS"
060900             MOVE   "Y" TO WY-LIST-PROGRAMS.
061000     IF      WB-13 = "*LIST COMPACT"
061100             MOVE   "Y" TO WY-LIST-COMPACT.
061200     IF      WB-8  = "*NO-DATA"
061300             MOVE   "Y" TO WY-NO-DATA.
061400     IF      WB-11 = "*NO-LIBRARY"
061500             MOVE   "Y" TO WY-NO-LIBRARY.
061600     IF      WB-10 = "*NO-SOURCE"
061700             MOVE   "Y" TO WY-NO-SOURCE.
061800     IF      WB-16 = "*REMOVE-COMMENTS"
061900             MOVE   "Y" TO WY-REMOVE-COMMENTS.
062000     IF      WB-8  = "*NEW-POP"
062100             MOVE   "Y" TO WY-NEW-POP.
062200     IF      WB-4  = "*OPT"
062300             MOVE   WB-X TO WY-OPT-SW (WB-NN).
062400     IF      WB-14 = "*SELECT-MODULE"
062500             IF      WB-MODULE = SPACE
062600                     MOVE  "SELECTED MODULE NOT SPECIFIED"
062700                         TO PRINT-DATA
062800                     PERFORM X20-PRINT-DETAIL
062900                     STOP RUN.
063000
063100     IF      WB-14 = "*SELECT-MODULE"
063200             ADD     1 TO SUB6
063300             IF      SUB6 > 10
063400                     MOVE   "MORE THAN 10 MODULES SELECTED"
063500                          TO PRINT-DATA
063600                     PERFORM X20-PRINT-DETAIL
063700                     STOP RUN
063800             ELSE
063900                     MOVE   "Y" TO WY-SELECT-MODULE
064000                     MOVE    WB-MODULE
064100                          TO WG-MODULE-SELECTED (SUB6)
064200                     MOVE    WB-LEVEL  TO WG-MODULE-LEVEL (SUB6).
064300     IF      WB-12 = "*SELECT-PROG"
064400             ADD     1 TO SUB7
064500             IF      SUB7 > 50
064600                     MOVE   "MORE THAN 50 RECORDS SELECTED"
064700                          TO PRINT-DATA
064800                     PERFORM X20-PRINT-DETAIL
064900                     STOP RUN
065000             ELSE
065100                     MOVE   "Y" TO WY-SELECT-PROG
065200                     MOVE    WB-PROG
065300                          TO WF-PROGRAM-SELECTED (SUB7).
065400
065500 C25-10-READ-FILE.
065600     MOVE    SPACES TO WB-CONTROL-DATA.
065700     READ    CONTROL-CARD-FILE INTO  WB-CONTROL-DATA
065800             AT END  MOVE "*END-MONITOR NOT ENCOUNTERED"
065900                  TO PRINT-DATA
066000             PERFORM X20-PRINT-DETAIL
066100             STOP RUN.
066200 C25-EXIT.
066300     EXIT.
066400/
066500 C30-CHECK-COMBINATIONS SECTION.
066600*==============================
066700*
066800****************************************************************
066900*    THIS SECTION CHECKS FOR ANY INVALID COMBINATIONS OF       *
067000*    CARDS INPUT WITH AN ASTERISK IN COLUMN 1.                 *
067100*                                                              *
067200*    PERFORMED BY  C10-PROCESS-MONITOR                         *
067300*    PERFORMS      NONE                                        *
067400****************************************************************
067500 C30-1-SELECTION-CHECK.
067600     IF      WY-EXTRACT-ALL = SPACE
067700             GO TO   C30-10.
067800     IF      WY-EXTRACT-AUTO = "Y"
067900             MOVE   "ALL AND AUTO PROGRAMS SELECTED"
068000                  TO PRINT-DATA
068100             PERFORM X20-PRINT-DETAIL
068200             STOP RUN.
068300
068400 C30-10.
068500     IF      WY-EXTRACT-ALL = SPACE
068600             GO TO   C30-20.
068700     IF      WY-EXTRACT-MAN = "Y"
068800             MOVE   "ALL AND MANUAL PROGRAMS SELECTED"
068900                  TO PRINT-DATA
069000             PERFORM X20-PRINT-DETAIL
069100             STOP RUN.
069200
069300 C30-20.
069400     IF      WY-LIST-PROGRAMS = SPACE
069500             GO TO   C30-30.
069600     IF      WY-LIST-NO-UPDATES = SPACE
069700             MOVE   "BOTH UPDATES AND PROGRAMS SELECTED"
069800                  TO PRINT-DATA
069900             PERFORM X20-PRINT-DETAIL
070000             STOP RUN.
070100
070200 C30-30.
070300     IF      WY-EXTRACT-AUTO = SPACE
070400             GO TO   C30-40-CHECK-FOR-NEW-FILE.
070500     IF      WY-EXTRACT-MAN  = "Y"
070600             MOVE   "AUTO AND MANUAL PROGRAMS SELECTED"
070700                  TO PRINT-DATA
070800             PERFORM X20-PRINT-DETAIL
070900             STOP RUN.
071000
071100 C30-40-CHECK-FOR-NEW-FILE.
071200     IF      WY-NO-SOURCE = SPACE
071300             GO TO   C30-50.
071400     IF      WY-NEW-POP   = "Y"
071500             GO TO   C30-50.
071600     MOVE   "NO SOURCE OR UPDATED POPFILE SELECTED" TO PRINT-DATA.
071700     PERFORM X20-PRINT-DETAIL
071800     STOP RUN.
071900
072000 C30-50.
072100     IF      WY-EXTRACT-ALL = "Y"
072200             GO TO   C30-55.
072300     IF      WY-EXTRACT-MAN = "Y"
072400             GO TO   C30-55.
072500     IF      WY-EXTRACT-AUTO = SPACE
072600             GO TO   C30-60.
072700 C30-55.
072800     IF      WY-SELECT-PROG = "Y"
072900             MOVE  "SINGLE PROGRAM SELECTED WITH ALL/AUTO/MANUAL"
073000                  TO PRINT-DATA
073100             PERFORM X20-PRINT-DETAIL
073200             STOP RUN.
073300     IF      WY-SELECT-MODULE = "Y"
073400             MOVE  "SINGLE MODULE SELECTED WITH ALL/AUTO/MANUAL"
073500                  TO PRINT-DATA
073600             PERFORM X20-PRINT-DETAIL
073700             STOP RUN.
073800
073900 C30-60.
074000     IF      WY-SELECT-PROG = SPACE
074100             GO TO   C30-70.
074200     IF      WY-SELECT-MODULE = "Y"
074300             MOVE  "SINGLE MODULE AND SINGLE PROGRAM SELECTED"
074400                  TO PRINT-DATA
074500             PERFORM X20-PRINT-DETAIL
074600             STOP RUN.
074700
074800
074900 C30-70.
075000     IF      WY-EXTRACT-ALL = SPACE
075100        IF      WY-EXTRACT-AUTO = SPACE
075200             IF      WY-EXTRACT-MAN = SPACE
075300                 IF      WY-SELECT-PROG = SPACE
075400                     IF      WY-SELECT-MODULE = SPACE
075500                             MOVE   "NO PROGRAMS SELECTED"
075600                                  TO PRINT-DATA
075700                             PERFORM X20-PRINT-DETAIL
075800                             STOP RUN.
075900
076000 C30-EXIT.
076100     EXIT.
076200/
076300 C40-PROCESS-X-CARDS SECTION.
076400*===========================
076500*
076600****************************************************************
076700*    THIS SECTION PROCESSES THE INPUT PARAMETER FILE RECORDS   *
076800*    WHICH START WITH AN "X" IN COLUMN 1 AND SETS A TABLE WHICH*
076900*    CONTAINS TEXT TO BE SUBSTITUTED BY THIS EXECUTIVE ROUTINE.*
077000*                                                              *
077100*    PERFORMED BY  C10-PROCESS-MONITOR                         *
077200*    PERFORMS      NONE                                        *
077300****************************************************************
077400 C40-1-PROCESS-CARD.
077500     IF      WB-X-HYPHEN NOT = "X-"
077600             MOVE   "INVALID X-CARD:" TO WZ-INVALID-DATA
077700             MOVE    WB-CONTROL-DATA  TO WZ-ERROR-MESSAGE
077800             MOVE    WZ-INVALID-DATA TO PRINT-DATA
077900             PERFORM X20-PRINT-DETAIL
078000             GO TO   C40-90-READ-FILE.
078100
078200     IF      WB-X-CARD-NUM > 200
078300             MOVE   "INVALID X-CARD:" TO WZ-INVALID-DATA
078400             MOVE    WB-CONTROL-DATA  TO WZ-ERROR-MESSAGE
078500             MOVE    WZ-INVALID-DATA  TO PRINT-DATA
078600             PERFORM X20-PRINT-DETAIL
078700             GO TO   C40-90-READ-FILE.
078800
078900 C40-20-MOVE-DATA.
079000     MOVE    WB-SUBS-TEXT TO WX-X-CARD (WB-X-CARD-NUM).
079100     IF      WB-PROG-POS = SPACES
079200             GO TO   C40-30-MOVE-DATA.
079300     IF      WB-PROG-POS-NUM < 55
079400             MOVE   WB-PROG-POS-NUM
079500                 TO WX-PROG-POS (WB-X-CARD-NUM).
079600
079700 C40-30-MOVE-DATA.
079800     MOVE    WB-CONTROL-DATA TO PRINT-DATA.
079900     PERFORM X20-PRINT-DETAIL.
080000
080100 C40-90-READ-FILE.
080200     READ    CONTROL-CARD-FILE INTO  WB-CONTROL-DATA
080300             AT END  MOVE
080400     "*BEGIN-UPDATE AND *END-UPDATE CARDS NOT ENCOUNTERED"
080500                  TO PRINT-DATA
080600             PERFORM  X20-PRINT-DETAIL
080700             STOP RUN.
080800
080900 C40-EXIT.
081000     EXIT.
081100
081200/
081300 C50-PRINT-OPTIONS SECTION.
081400*=========================
081500*
081600****************************************************************
081700*    THIS SECTION PRINTS DETAILS OF THE PROCESSING OPTIONS     *
081800*    SELECTED BY THE VARIOUS CONTROL CARDS INPUT               *
081900*                                                              *
082000*    PERFORMED BY  C10-PROCESS-MONITOR                         *
082100*    PERFORMS      NONE                                        *
082200****************************************************************
082300 C50-1-PRINT-OPTION-SWITCHES.
082400     WRITE   PRINT-REC FROM WV-OPTION-HEADING AFTER PAGE.
082500     WRITE   PRINT-REC FROM WV-OPT-1          AFTER 1.
082600     WRITE   PRINT-REC FROM WV-OPT-2          AFTER 1.
082700     MOVE    SPACES TO WV-OPT-SWITCHES.
082800     MOVE    ZERO   TO SUB1.
082900     PERFORM C50-4 26 TIMES.
083000     GO TO   C50-5.
083100 C50-4.
083200*    PERFORM 26 TIMES
083300             ADD     1 TO SUB1
083400             MOVE    WY-OPT-SW (SUB1) TO WV-OPT (SUB1).
083500*    END-PERFORM.
083600 C50-5.
083700     WRITE   PRINT-REC FROM WV-OPT-SWITCHES AFTER 1.
083800
083900 C50-20-LISTING-OPTIONS.
084000     IF      WY-LIST-NO-UPDATES = "Y"
084100             MOVE   "UPDATES WILL NOT BE REPORTED" TO PRINT-DATA
084200     ELSE
084300             MOVE   "UPDATES WILL BE REPORTED"     TO PRINT-DATA.
084400     WRITE   PRINT-REC AFTER 3.
084500
084600     IF      WY-LIST-X-CARDS = "Y"
084700             MOVE   "X-CARD SUBSTITUTIONS WILL BE SHOWN"
084800                  TO PRINT-DATA
084900     ELSE
085000             MOVE   "X-CARD SUBSTITUTIONS WILL NOT BE SHOWN"
085100                  TO PRINT-DATA.
085200     WRITE   PRINT-REC AFTER 1.
085300
085400     IF      WY-LIST-PROGRAMS = "Y"
085500             MOVE   "PROGRAM LISTINGS WILL BE PRINTED"
085600                  TO PRINT-DATA
085700     ELSE
085800             MOVE   "PROGRAM LISTINGS WILL NOT BE PRINTED"
085900                  TO PRINT-DATA.
086000     WRITE   PRINT-REC AFTER 1.
086100
086200     IF      WY-LIST-COMPACT = "Y"
086300             MOVE   "REPORT WILL BE COMPRESSED" TO PRINT-DATA
086400     ELSE
086500             MOVE   "REPORT WILL BE EXPANDED"   TO PRINT-DATA.
086600     WRITE   PRINT-REC AFTER 1.
086700
086800     IF      WY-NO-SOURCE = "Y"
086900             MOVE   "SOURCE FILE WILL BE SUPPRESSED" TO PRINT-DATA
087000     ELSE
087100             MOVE   "SOURCE FILE WILL BE CREATED" TO PRINT-DATA.
087200     WRITE   PRINT-REC AFTER 1.
087300
087400     IF      WY-NEW-POP = "Y"
087500             MOVE   "NEW POPULATION FILE WILL BE CREATED"
087600                  TO PRINT-DATA
087700     ELSE
087800             MOVE   "NEW POPULATION FILE WILL BE SUPPRESSED"
087900                  TO PRINT-DATA.
088000     WRITE   PRINT-REC AFTER 1.
088100
088200
088300 C50-30.
088400     IF      WY-NO-LIBRARY = "Y"
088500             MOVE "LIBRARY NOT SELECTED"
088600                  TO PRINT-DATA
088700     ELSE
088800             MOVE "LIBRARY FILES WILL BE SELECTED"
088900                  TO PRINT-DATA.
089000     WRITE   PRINT-REC AFTER 1.
089100
089200 C50-35.
089300     IF      WY-NO-DATA = "Y"
089400             MOVE   "DATA BLOCKS WILL BE IGNORED"
089500                  TO PRINT-DATA
089600     ELSE
089700             MOVE   "DATA BLOCKS WILL BE SENT TO SOURCE FILE"
089800                  TO PRINT-DATA.
089900     WRITE   PRINT-REC AFTER 1.
090000
090100 C50-40.
090200     IF      WY-EXTRACT-ALL = "Y"
090300             MOVE   "ALL PROGRAMS SELECTED"     TO PRINT-DATA.
090400     IF      WY-EXTRACT-AUTO = "Y"
090500             MOVE   "ALL AUTOMATIC PROGRAMS SELECTED"
090600                  TO PRINT-DATA.
090700     IF      WY-EXTRACT-MAN = "Y"
090800             MOVE   "ALL MANUAL PROGRAMS SELECTED"
090900                  TO PRINT-DATA.
091000     IF      WY-SELECT-PROG = "Y"
091100             MOVE   "SEPARATE PROGRAMS SELECTED"
091200                  TO PRINT-DATA.
091300     IF      WY-SELECT-MODULE = "Y"
091400             MOVE   "SEPARATE MODULES SELECTED"
091500                  TO PRINT-DATA.
091600     WRITE   PRINT-REC AFTER 1.
091700
091800     IF      WY-REMOVE-COMMENTS = "Y"
091900             MOVE   "COMMENT LINES WILL BE REMOVED"
092000                  TO PRINT-DATA
092100     ELSE
092200             MOVE   "COMMENT LINES WILL BE RETAINED"
092300                  TO PRINT-DATA.
092400     WRITE   PRINT-REC AFTER 1.
092500
092600
092700     IF      WY-KILL-DELETIONS = "Y"
092800             MOVE   "UNUSED OPTIONAL CODE WILL BE REMOVED"
092900                  TO PRINT-DATA
093000     ELSE
093100             MOVE   "UNUSED OPTIONAL CODE WILL BE CONVERTED TO COM
093200-    "MENTS AND RETAINED"
093300                  TO PRINT-DATA.
093400     WRITE   PRINT-REC AFTER 1.
093500
093600 C50-EXIT.
093700     EXIT.
093800
093900/
094000 C60-START-CARD SECTION.
094100*======================
094200*
094300****************************************************************
094400*    WHEN A "*START" CARD IS READ THIS SECTION INITIALISES     *
094500*    VARIOUS FIELDS AND SETS CERTAIN FLAGS.                    *
094600*                                                              *
094700*    PERFORMED BY    C10-PROCESS-MONITOR                       *
094800*                    D62-SERIES-UPDATE                         *
094900*                    D67-LINE-UPDATE                           *
095000*                    D68-LINE-EQUAL                            *
095100*                    D69-LINE-BLANK-OR-LESS                    *
095200*                    D40-ANY-UPDATES
095300*    PERFORMS        NONE                                      *
095400****************************************************************
095500 C60-1.
095600
095700     MOVE    WB-UPDATE-PROG TO WZ-CURRENT-UPD-PROG.
095800
095900     IF      WZ-CURRENT-UPD-PROG = WZ-CURRENT-POP-PROG
096000             MOVE   "Y"    TO WZ-UPDATE-THIS-PROG
096100     ELSE
096200             MOVE    SPACE TO WZ-UPDATE-THIS-PROG.
096300
096400 C60-EXIT.
096500     EXIT.
096600/
096700 D10-MERGE-UPDATE-CARDS SECTION.
096800*==============================
096900*
097000****************************************************************
097100*    THIS IS THE MAIN CONTROL SECTION FOR THE PROCESSING OF    *
097200*    THE POPULATION FILE.  IT OBTAINS THE HEADER FOR THE FIRST *
097300*    AUDIT ROUTINE IN THE POPFILE AND PROCESSES ALL RECORDS.   *
097400*                                                              *
097500*    PERFORMED BY    A10-MAIN                                  *
097600*    PERFORMS        D11-GET-NEXT-PROGRAM                      *
097700*                    D15-END-OF-POPFILE                        *
097800*                    D20-PROCESS-POPFILE                       *
097900****************************************************************
098000 D10-1-OPEN-FILES.
098100     IF      WY-NEW-POP = "Y"
098200             OPEN    OUTPUT UPDATED-POPULATION-FILE.
098300
098400     IF      WY-NO-SOURCE = SPACE
098500             OPEN    OUTPUT SOURCE-COBOL-PROGRAMS.
098600
098700     OPEN    INPUT   POPULATION-FILE.
098800     PERFORM D10-20-GET-VERSION.
098900     MOVE WZ-VERSION-CARD TO PRINT-DATA.
099000     PERFORM X20-PRINT-DETAIL.
099100
099200 D10-10-GET-HEADER.
099300     PERFORM D11-GET-NEXT-PROGRAM.
099400
099500     PERFORM D20-PROCESS-POPFILE
099600             UNTIL   WZ-END-OF-POPFILE = "Y".
099700
099800     PERFORM D15-END-OF-POPFILE.
099900
100000 D10-EXIT.
100100     EXIT.
100200/
100300 D10-20-GET-VERSION SECTION.
100400*============================
100500*
100600 D10-20-INIT-VER.
100700     MOVE WZ-VERSION-NUM TO WA-VERSION-NUM.
100800     MOVE WZ-VERSION-DATE TO WA-VERSION-DATE.
100900
101000 D10-20-EXIT.
101100     EXIT.
101200/
101300 D11-GET-NEXT-PROGRAM SECTION.
101400*============================
101500*
101600****************************************************************
101700*    PERFORMED BY    D10-MERGE-UPDATE-CARDS                    *
101800*                    D30-PROCESS-HEADER                        *
101900****************************************************************
102000 D11-1-READ-POPFILE.
102100     MOVE SPACES  TO WC-STAR-HEADER.
102200
102300     PERFORM X30-READ-POPFILE
102400             UNTIL   WC-STAR-HEADER = "*HEADER".
102500 D11-TEST-END.
102600     IF      WZ-END-OF-POPFILE = "Y"
102700             GO TO   D11-EXIT.
102800
102900     IF      WC-COBOL NOT = "COBOL"
103000             IF      WC-COBOL = "CLBRY"
103100                     IF      WY-NO-LIBRARY = "Y"
103200                             GO TO   D11-1-READ-POPFILE
103300                     ELSE
103400                             NEXT SENTENCE
103500              ELSE
103600                     IF      WC-COBOL = "DATA*"
103700                             IF    WY-NO-DATA = "Y"
103800                                   GO TO D11-1-READ-POPFILE
103900                             ELSE
104000                                   NEXT SENTENCE
104100                     ELSE
104200                             GO TO   D11-1-READ-POPFILE.
104300
104400 D11-EXIT.
104500     EXIT.
104600/
104700 D15-END-OF-POPFILE SECTION.
104800*==========================
104900*
105000****************************************************************
105100*    THIS SECTION PRINTS END OF RUN ACCOUNTING INFORMATION AND *
105200*    FLAGS ANY OUTSTANDING *UPDATE RECORDS AS UNUSED.          *
105300*                                                              *
105400*    PERFORMED BY           D10-MERGE-UPDATE-CARD              *
105500*    PERFORMS               D16-FLAG-UNUSED-UPDATES            *
105600*                           X20-PRINT-DETAIL                   *
105700****************************************************************
105800 D15-1-SET-UP-FIELDS.
105900     MOVE    WZ-PROGS-FOUND  TO WA-PROGS-FOUND.
106000     MOVE    WA-FINAL-LINE-1 TO PRINT-DATA.
106100     PERFORM X20-PRINT-DETAIL.
106200
106300     IF      WY-NO-SOURCE = SPACE
106400             MOVE    WZ-SOURCE-PROGS TO WA-SOURCE-PROGS
106500             MOVE    WA-FINAL-LINE-2 TO PRINT-DATA
106600             PERFORM X20-PRINT-DETAIL.
106700
106800     IF      WY-NEW-POP = "Y"
106900             MOVE    WZ-NEWPOP-PROGS TO WA-NEWPOP-PROGS
107000             MOVE    WA-FINAL-LINE-3 TO PRINT-DATA
107100             PERFORM X20-PRINT-DETAIL.
107200
107300     IF      WZ-END-OF-UPDATES = SPACE
107400             READ    CONTROL-CARD-FILE INTO  WB-CONTROL-DATA
107500                     AT END  MOVE "Y" TO WZ-END-OF-UPDATES.
107600
107700     PERFORM D16-FLAG-UNUSED-UPDATES
107800       UNTIL WZ-END-OF-UPDATES = "Y".
107900
108000     IF WY-SELECT-PROG = "Y" AND
108100       WF-PROGRAM-SELECTED-TABLE NOT = SPACES
108200       MOVE "SOME PROGRAMS NOT FOUND" TO PRINT-DATA
108300       PERFORM X20-PRINT-DETAIL
108400       PERFORM VARYING SUB7 FROM 1 BY 1 UNTIL SUB7 > 50
108500         IF WF-PROGRAM-SELECTED (SUB7) NOT = SPACE
108600              MOVE WF-PROGRAM-SELECTED (SUB7) TO PRINT-DATA
108700              PERFORM X20-PRINT-DETAIL
108800              END-IF
108900         END-PERFORM.
109000
109100 D15-EXIT.
109200     EXIT.
109300
109400
109500
109600
109700 D16-FLAG-UNUSED-UPDATES SECTION.
109800*===============================
109900 D16-1-PRINT-UPDATES.
110000     MOVE    WB-CONTROL-DATA TO PRINT-DATA.
110100     PERFORM X20-PRINT-DETAIL.
110200     READ    CONTROL-CARD-FILE INTO  WB-CONTROL-DATA
110300             AT END  MOVE "Y" TO WZ-END-OF-UPDATES.
110400
110500 D16-EXIT.
110600     EXIT.
110700
110800
110900/
111000 D20-PROCESS-POPFILE SECTION.
111100*===========================
111200*
111300****************************************************************
111400*    THIS IS THE MAIN CONTROL SECTION FOR POPULATION FILE      *
111500*    PROCESSING.                                               *
111600*                                                              *
111700*    PERFORMED BY    D10-MERGE-UPDATE-CARDS                    *
111800*    PERFORMS        D25-PRINT-REPORT                          *
111900*                    D30-PROCESS-HEADER                        *
112000*                    D90-PROCESS-END-OF                        *
112100*                    X10-WRITE-UPDATED-POPFILE                 *
112200*                    X30-READ-POPFILE                          *
112300*                    X40-WRITE-SOURCE-FILE                     *
112400****************************************************************
112500 D20-1-CHECK-RECORD-TYPE.
112600     MOVE    SPACE TO WZ-NOT-THIS-COMMENT.
112700     MOVE    SPACE TO WZ-LINE-UPDATE.
112800     MOVE    SPACE TO WZ-REPLACE-FLAG.
112900*    MOVE   "Y"    TO WZ-UPDATE-THIS-PROG.
113000     MOVE    SPACE TO WZ-OPTIONAL-SELECTED.
113100     MOVE    SPACE TO WZ-DELETE-FLAG.
113200
113300     IF      WC-STAR-HEADER = "*HEADER"
113400             MOVE    SPACE  TO WZ-PROGRAM-SELECTED
113500             PERFORM D30-PROCESS-HEADER
113600                     UNTIL  WZ-PROGRAM-SELECTED = "Y"
113700     ELSE
113800             IF      WC-STAR-HEADER = "*END-OF"
113900                     PERFORM D90-PROCESS-END-OF
114000             ELSE
114100                     PERFORM D50-PROCESS-COBOL-LINE.
114200
114300     PERFORM D25-PRINT-REPORT.
114400
114500 D20-10-SOURCE-CHECK.
114600     IF      WY-NO-SOURCE        = "Y"
114700             GO TO   D20-20-UPDATED-POP-CHECK.
114800     IF      WZ-DELETE-FLAG      = "Y"
114900             GO TO   D20-20-UPDATED-POP-CHECK.
115000     IF      WZ-NOT-THIS-COMMENT = "Y"
115100             GO TO   D20-20-UPDATED-POP-CHECK.
115200*    IF      WZ-UPDATE-THIS-PROG =  SPACE
115300*            GO TO   D20-30-READ-FILE.
115400
115500     IF      WZ-END-OF-POPFILE NOT = "Y"
115600             PERFORM X40-WRITE-SOURCE-FILE.
115700
115800 D20-20-UPDATED-POP-CHECK.
115900     IF      WY-NEW-POP = "Y"
116000             PERFORM X10-WRITE-UPDATED-POPFILE.
116100             MOVE WZ-SAVE-POP-RECORD TO WC-CURRENT-POP-RECORD.
116200
116300 D20-30-READ-FILE.
116400     IF      WZ-END-OF-POPFILE = "Y"
116500             GO TO   D20-EXIT.
116600     IF      WB-SEQ-1 = SPACES
116700             MOVE    WZ-SAVE-SEQ TO WC-6
116800             GO TO   D20-EXIT.
116900     PERFORM X30-READ-POPFILE.
117000
117100 D20-EXIT.
117200     EXIT.
117300
117400/
117500 D25-PRINT-REPORT SECTION.
117600*========================
117700*
117800****************************************************************
117900*    DEPENDING ON THE PRINT OPTIONS SELECTED, THIS SECTION     *
118000*    PRINTS EITHER THE EXTRACTED PROGRAMS OR ANY UPDATES AND   *
118100*    X-CARD SUBSTITUTIONS MADE.                                *
118200*                                                              *
118300*    PERFORMED BY    D20-PROCESS-POPFILE                       *
118400*    PERFORMS        X20-PRINT-DETAIL                          *
118500*                    D27-PRINT-UPDATES                         *
118600****************************************************************
118700 D25-1-CHECK-LIST-PROGRAMS.
118800     IF      WY-LIST-PROGRAMS = "Y"
118900             MOVE    WC-CURRENT-POP-RECORD TO PRINT-DATA
119000             PERFORM X20-PRINT-DETAIL
119100             GO TO   D25-EXIT.
119200
119300 D25-10-UPDATES-CHECK.
119400     IF      WY-LIST-NO-UPDATES = SPACE
119500             IF      WZ-LINE-UPDATE = "Y"
119600                     PERFORM D27-PRINT-UPDATES
119700                     GO TO   D25-EXIT.
119800
119900 D25-20-X-CARD-CHECK.
120000     IF      WY-LIST-X-CARDS = "Y"
120100             IF      WZ-SAVE-12-15 = "XXXX"
120200                     MOVE    WZ-SAVE-12-20 TO WE-X-CARD
120300                     MOVE   "SUBSTITUTION" TO WE-CHANGE-TYPE
120400                     MOVE    WC-CURRENT-POP-RECORD
120500                                           TO WE-COBOL-LINE
120600                     MOVE    WE-PRINT-DATA TO PRINT-DATA
120700                     PERFORM X20-PRINT-DETAIL.
120800
120900 D25-EXIT.
121000     EXIT.
121100
121200
121300
121400/
121500 D27-PRINT-UPDATES SECTION.
121600*=========================
121700*
121800****************************************************************
121900*    THIS SECTION WILL PRINT THE ORIGINAL POPULATION FILE      *
122000*    RECORD (IF "*LIST NO-UPDATES" HAS NOT BEEN SPECIFIED) AND,*
122100*    IF A REPLACEMENT, THE NEW IMAGE OR, IF AN INSERTION, JUST *
122200*    THE NEW LINE OF CODE.                                     *
122300*                                                              *
122400*    PERFORMED BY    D25-PRINT-REPORT                          *
122500*    PERFORMS        X20-PRINT-DETAIL                          *
122600****************************************************************
122700 D27-1-PRINT-REPLACEMENTS.
122800     IF      WZ-REPLACE-FLAG = "Y"
122900             MOVE    WZ-SAVE-POP-RECORD  TO WE-COBOL-LINE
123000             MOVE    SPACES              TO WE-X-CARD
123100             MOVE   "ORIGINAL"           TO WE-CHANGE-TYPE
123200             MOVE    WE-PRINT-DATA       TO PRINT-DATA
123300             PERFORM X20-PRINT-DETAIL
123400             MOVE   "REPLACEMENT"        TO WE-CHANGE-TYPE
123500             MOVE    SPACES              TO WE-X-CARD
123600             MOVE    WC-CURRENT-POP-RECORD  TO WE-COBOL-LINE
123700             MOVE    WE-PRINT-DATA       TO PRINT-DATA
123800             PERFORM X20-PRINT-DETAIL
123900             GO TO   D27-EXIT.
124000
124100 D27-10-PRINT-NEW-DATA.
124200     IF      WZ-DONT-READ-POPFILE = "Y"
124300             MOVE    WC-CURRENT-POP-RECORD TO WE-COBOL-LINE
124400             MOVE    WE-PRINT-DATA         TO PRINT-DATA
124500             PERFORM X20-PRINT-DETAIL
124600             GO TO   D27-EXIT.
124700
124800 D27-20-PRINT-DELETIONS.
124900     IF      WZ-DELETE-FLAG = "Y"
125000             MOVE    WZ-SAVE-POP-RECORD  TO WE-COBOL-LINE
125100             MOVE    WE-PRINT-DATA       TO PRINT-DATA
125200             PERFORM X20-PRINT-DETAIL.
125300
125400 D27-EXIT.
125500     EXIT.
125600
125700/
125800 D30-PROCESS-HEADER SECTION.
125900*==========================
126000*
126100****************************************************************
126200*    THIS SECTION PROCESSES ANY "*HEADER" RECORDS.             *
126300*                                                              *
126400*    PERFORMED BY    D20-PROCESS-POPFILE                       *
126500*    PERFORMS        D90-PROCESS-END-OF                        *
126600*                    D11-GET-NEXT-PROGRAM                      *
126700*                    D31-PROCESS-DATA-HEADER                   *
126800*                    D32-PROCESS-COBOL-HEADER                  *
126900*                    D40-ANY-UPDATES                           *
127000*                    X10-WRITE-UPDATED-POPFILE                 *
127100*                    D43-PROGRAM-SELECTED                      *
127200****************************************************************
127300 D30-1-CHECK-HEADER-TYPE.
127400     MOVE    WC-COBOL TO BLOCK-TYPE
127500     IF      WC-COBOL = "CLBRY"
127600     AND WY-NO-LIBRARY NOT = "Y"
127700             MOVE WC-HEADER TO PRINT-DATA
127800             MOVE WC-PROG-ID TO WZ-CURRENT-MAIN-PROG
127900                                WZ-CURRENT-POP-PROG
128000*****************************************************************
128100* NOTE FOLLOWING CHANGES                                      ***
128200*****************************************************************
128300*            PERFORM D36-INITIALISE-PROGRAM-TOTALS
128400             MOVE   "Y" TO WZ-PROGRAM-SELECTED
128500             PERFORM D34-PROGRAM-SELECTED.
128600
128700     IF      WC-COBOL = "DATA*"
128800             PERFORM D31-PROCESS-DATA-HEADER.
128900
129000     IF      WC-COBOL = "COBOL"
129100             ADD     1 TO WZ-PROGS-FOUND
129200             PERFORM D32-PROCESS-COBOL-HEADER.
129300
129400     IF      WZ-PROGRAM-SELECTED = "Y"
129500             GO TO   D30-20-HEADER-SELECTED.
129600
129700     PERFORM D11-GET-NEXT-PROGRAM.
129800     IF      WZ-END-OF-POPFILE = "Y"
129900             MOVE    "Y" TO WZ-PROGRAM-SELECTED
130000             GO TO    D30-EXIT
130100     ELSE
130200             GO TO    D30-1-CHECK-HEADER-TYPE.
130300
130400 D30-20-HEADER-SELECTED.
130500     MOVE    WC-COBOL TO WZ-CURRENT-HEADER.
130600     IF      WZ-END-OF-UPDATES = SPACE
130700             PERFORM D40-ANY-UPDATES.
130800
130900 D30-EXIT.
131000     EXIT.
131100/
131200 D31-PROCESS-DATA-HEADER SECTION.
131300*===============================
131400*
131500 D31-1-CHECK-FOR-DATA-SELECTED.
131600     IF      WY-NO-DATA = "Y"
131700             GO TO   D31-EXIT.
131800
131900     IF      WZ-PROG-ID-6 = "M"
132000             GO TO   D31-30-MANUAL-CHECK.
132100
132200 D31-20-AUTO-CHECK.
132300     IF      WY-EXTRACT-MAN = SPACE
132400             MOVE   "Y"        TO WZ-PROGRAM-SELECTED
132500     ELSE
132600             GO TO   D31-EXIT.
132700
132800 D31-30-MANUAL-CHECK.
132900     IF      WY-EXTRACT-AUTO = SPACE
133000             MOVE   "Y"        TO WZ-PROGRAM-SELECTED.
133100
133200
133300 D31-EXIT.
133400     EXIT.
133500
133600/
133700 D32-PROCESS-COBOL-HEADER SECTION.
133800*================================
133900*
134000****************************************************************
134100*    THIS SECTION CHECKS TO SEE IF THE CURRENT HEADER IS       *
134200*    IS WITHIN THE RANGE SPECIFIED BY THE "*EXTRACT" OR        *
134300*    "*SELECT" RECORDS OF THE INPUT CONTROL-FILE.              *
134400*                                                              *
134500*    PERFORMED BY    D30-PROCESS-HEADER                        *
134600*    PERFORMS        D33-MODULE-CHECK                          *
134700*                    D34-PROGRAM-SELECTED                      *
134800****************************************************************
134900*                                                              *
135000****************************************************************
135100*  IF PROGRAM DOES NOT END WITH '5','A','B' OR 'M' IT          *
135200*  SHOULD HAVE A SPACE CHARACTER FOR POSITION 6.               *
135300****************************************************************
135400*                                                              *
135500 D32-0-CHECK-A-OR-M.
135600     IF WC-PROG-ID-6 = "A"
135700        GO TO D32-1-CHECK-FOR-ALL-SELECTED.
135800     IF WC-PROG-ID-6 = "B"
135900        GO TO D32-1-CHECK-FOR-ALL-SELECTED.
136000     IF WC-PROG-ID-6 = "M"
136100        GO TO D32-1-CHECK-FOR-ALL-SELECTED.
136200     IF WC-PROG-ID-6 = "5"
136300        GO TO D32-1-CHECK-FOR-ALL-SELECTED.
136400     MOVE SPACE TO WC-PROG-ID-6.
136500 D32-1-CHECK-FOR-ALL-SELECTED.
136600     IF      WY-EXTRACT-ALL = "Y"
136700             MOVE   "Y" TO WZ-PROGRAM-SELECTED
136800             MOVE    WC-PROG-ID TO WZ-CURRENT-POP-PROG
136900             GO TO   D32-50-CHECK-IF-SELECTED.
137000
137100 D32-10-MANUAL-CHECK.
137200     IF      WY-EXTRACT-MAN = "Y"
137300             IF      WC-PROG-ID-6 = "M"
137400                     MOVE   "Y" TO WZ-PROGRAM-SELECTED
137500                     MOVE    WC-PROG-ID TO WZ-CURRENT-POP-PROG
137600                     GO TO   D32-50-CHECK-IF-SELECTED.
137700
137800 D32-20-AUTO-CHECK.
137900     IF      WY-EXTRACT-AUTO = "Y"
138000             IF      WC-PROG-ID-6 = "A" OR "B"
138100                     MOVE   "Y" TO WZ-PROGRAM-SELECTED
138200                     MOVE    WC-PROG-ID TO WZ-CURRENT-POP-PROG
138300                     GO TO   D32-50-CHECK-IF-SELECTED.
138400
138500 D32-30-MODULE-CHECK.
138600     IF      WY-SELECT-MODULE = "Y"
138700*THE FOLLOWING CHECK ASSUMES THAT THE FILES ON THE POPFILE
138800*ARE IN ORDER!
138900             IF WG-MODULE-SELECTED-TABLE = SPACES
139000                MOVE "Y" TO WZ-END-OF-POPFILE
139100             ELSE
139200             MOVE    1 TO SUB6
139300             PERFORM D33-MODULE-CHECK
139400             GO TO   D32-50-CHECK-IF-SELECTED.
139500
139600     MOVE    1 TO SUB7.
139700 D32-40-PROGRAM-CHECK.
139800     IF      WY-SELECT-PROG = "Y"
139900             IF WF-PROGRAM-SELECTED-TABLE = SPACES
140000                     MOVE "Y" TO WZ-END-OF-POPFILE
140100             ELSE
140200             IF      WC-PROG-ID-1-5 = WF-PROGRAM-SELECTED (SUB7)
140300                     MOVE SPACE TO WF-PROGRAM-SELECTED (SUB7)
140400                     MOVE   "Y" TO WZ-PROGRAM-SELECTED
140500                     MOVE    WC-PROG-ID TO WZ-CURRENT-POP-PROG
140600                     GO TO   D32-50-CHECK-IF-SELECTED
140700             ELSE
140800                     IF      SUB7  < 50
140900                             ADD     1 TO SUB7
141000                             GO TO   D32-40-PROGRAM-CHECK
141100                     ELSE
141200                          IF WC-SUBPRG = "SUBPRG" OR "SUBRTN"
141300                             MOVE 1 TO SUB7
141400                             PERFORM 50 TIMES
141500                                IF WC-PROG2ID-1-5 =
141600                                  WF-PROGRAM-SELECTED (SUB7)
141700                                  MOVE SPACE TO
141800                                     WF-PROGRAM-SELECTED (SUB7)
141900                                  MOVE "Y" TO
142000                                     WZ-PROGRAM-SELECTED
142100                                  MOVE WC-PROG2ID TO
142200                                     WZ-CURRENT-POP-PROG
142300                                  GO D32-50-CHECK-IF-SELECTED
142400                                  END-IF
142500                             ADD 1 TO SUB7
142600                             END-PERFORM.
142700
142800 D32-50-CHECK-IF-SELECTED.
142900     IF      WZ-PROGRAM-SELECTED = "Y"
143000             PERFORM D34-PROGRAM-SELECTED.
143100
143200 D32-EXIT.
143300     EXIT.
143400/
143500 D33-MODULE-CHECK SECTION.
143600*========================
143700*
143800****************************************************************
143900*    IF ONE OR MORE MODULES HAVE BEEN SELECTED, THIS SECTION   *
144000*    CHECKS THAT THE CURRENT POPULATION FILE PROGRAM IS FROM   *
144100*    A SELECTED MODULE (AND LEVEL, IF LEVEL SELECTED).         *
144200*                                                              *
144300*    PERFORMED BY    D32-PROCESS-COBOL-HEADER                  *
144400*    PERFORMS        NONE                                      *
144500****************************************************************
144600 D33-1-MODULE-CHECK.
144700     IF      WC-MODULE = WG-MODULE-SELECTED (SUB6)
144800             GO TO D33-10-CHECK-MODULE-LEVEL.
144900     IF      WC-MODULE > WG-MODULE-SELECTED (SUB6) AND
145000*THE FOLLOWING CHECK MAKES SURE SOME FILES GET OFF
145100             WZ-SOURCE-PROGS NOT = 0
145200             MOVE SPACE TO WG-MODULE-SELECTED (SUB6)
145300             MOVE SPACE TO WG-MODULE-LEVEL (SUB6)
145400             IF      SUB6  < 10
145500                     ADD     1 TO SUB6
145600                     GO TO   D33-1-MODULE-CHECK
145700             ELSE
145800                     GO TO   D33-EXIT
145900     ELSE
146000             GO TO D33-EXIT.
146100
146200 D33-10-CHECK-MODULE-LEVEL.
146300     IF      WG-MODULE-LEVEL (SUB6) = SPACE
146400             MOVE    WC-PROG-ID TO WZ-CURRENT-POP-PROG
146500             MOVE   "Y" TO WZ-PROGRAM-SELECTED
146600             GO TO   D33-EXIT.
146700
146800     IF      WG-MODULE-LEVEL (SUB6) = WC-LEVEL
146900             MOVE    WC-PROG-ID TO WZ-CURRENT-POP-PROG
147000             MOVE   "Y" TO WZ-PROGRAM-SELECTED.
147100
147200 D33-EXIT.
147300     EXIT.
147400
147500/
147600 D34-PROGRAM-SELECTED SECTION.
147700*============================
147800*
147900****************************************************************
148000*    IF THE CURRENT POPULATION FILE PROGRAM HAS BEEN SELECTED  *
148100*    FOR PROCESSING, THIS SECTION UPDATES VARIOUS FIELDS.      *
148200*                                                              *
148300*    PERFORMED BY    D32-PROCESS-COBOL-HEADER                  *
148400*    PERFORMS        NONE                                      *
148500****************************************************************
148600 D34-1-SELECTION-CHECK.
148700     IF      WZ-PROGRAM-SELECTED = SPACE
148800             GO TO   D34-EXIT.
148900
149000     IF      WY-NO-SOURCE = SPACE
149100             ADD     1 TO WZ-SOURCE-PROGS.
149200
149300     IF      WY-NEW-POP = "Y"
149400             ADD     1 TO WZ-NEWPOP-PROGS.
149500
149600*    MOVE    SPACE TO WZ-CURRENT-MAIN-PROG.
149700     MOVE    WZ-CURRENT-POP-PROG TO WZ-CURRENT-MAIN-PROG.
149800
149900     PERFORM D36-INITIALISE-PROGRAM-TOTALS.
150000
150100     MOVE    WC-HEADER TO PRINT-DATA.
150200     IF      WC-SUBPRG = "SUBPRG"
150300             MOVE    WZ-CURRENT-POP-PROG TO WZ-CURRENT-MAIN-PROG
150400*            MOVE    WC-PROG2ID          TO WZ-CURRENT-UPD-PROG
150500             MOVE    WC-PROG2ID          TO WZ-CURRENT-POP-PROG
150600     END-IF.
150700     IF      WC-SUBPRG = "SUBRTN"
150800             MOVE    WZ-CURRENT-POP-PROG TO WZ-CURRENT-MAIN-PROG
150900*            MOVE    WC-PROG2ID          TO WZ-CURRENT-UPD-PROG
151000             MOVE    WC-PROG2ID          TO WZ-CURRENT-POP-PROG
151100     END-IF.
151200
151300 D34-2-OPEN-OUTPUT.
151400     IF      WY-LIST-COMPACT = SPACE
151500             MOVE    51 TO WZ-LINE-CT.
151600
151700     PERFORM X20-PRINT-DETAIL.
151800 D34-EXIT.
151900     EXIT.
152000/
152100 D36-INITIALISE-PROGRAM-TOTALS SECTION.
152200*=====================================
152300*
152400****************************************************************
152500*    WHEN A PROGRAM "*HEADER" IS SELECTED THIS SECTION RESETS  *
152600*    ALL THE VARIOUS TOTALS FOR THAT PROGRAM.                  *
152700*                                                              *
152800*    PERFORMED BY    D32-PROCESS-COBOL-HEADER                  *
152900*    PERFORMS        NONE                                      *
153000****************************************************************
153100 D36-1-RESET-TOTALS.
153200     MOVE    ZERO  TO WZ-LINES-COBOL.
153300     MOVE    ZERO  TO WZ-LINES-INSERTED.
153400     MOVE    ZERO  TO WZ-LINES-REPLACED.
153500     MOVE    ZERO  TO WZ-LINES-DELETED.
153600     MOVE    ZERO  TO WZ-CODE-REMOVED.
153700     MOVE    ZERO  TO WZ-COMMENTS-DEL.
153800     MOVE    ZERO  TO WZ-SEQ-NO.
153900
154000 D36-EXIT.
154100     EXIT.
154200/
154300 D40-ANY-UPDATES SECTION.
154400*=======================
154500*
154600****************************************************************
154700*    THIS SECTION FINDS THE FIRST UPDATE RECORD (IF ANY) FOR   *
154800*    THE CURRENT POPULATION FILE PROGRAM BEING PROCESSED.      *
154900*                                                              *
155000*    PERFORMED BY    D30-PROCESS-HEADER                        *
155100*    PERFORMS        D42-READ-UPDATE                           *
155200****************************************************************
155300 D40-1-CHECK-UPDATE-AND-POP.
155400     MOVE    SPACE TO WZ-UPDATE-THIS-PROG.
155500     IF      WZ-CURRENT-UPD-PROG > WZ-CURRENT-POP-PROG
155600             GO TO   D40-EXIT.
155700
155800     IF      WZ-CURRENT-UPD-PROG = WZ-CURRENT-POP-PROG
155900             GO TO   D40-20-PROGRAMS-EQUAL.
156000
156100*    IF      WZ-CURRENT-POP-PROG = WB-UPDATE-PROG
156200*            GO TO   D40-20-PROGRAMS-EQUAL.
156300
156400     IF      WZ-UPD-PROG-CHAR  = "A"
156500             GO TO D40-05-CHECK-LIBRARY.
156600
156700     IF      WZ-UPD-PROG-CHAR = "K"
156800             GO TO D40-05-CHECK-LIBRARY
156900     ELSE    GO TO D40-10-GET-NEXT-STAR-START.
157000*
157100 D40-05-CHECK-LIBRARY.
157200     IF      WZ-CURRENT-POP-PROG < "SQ000"
157300             GO TO D40-EXIT.
157400
157500 D40-10-GET-NEXT-STAR-START.
157600     MOVE    SPACES TO WB-STAR-START.
157700     PERFORM D42-READ-UPDATE-FILE
157800             UNTIL   WB-STAR-START = "*START".
157900
158000     IF      WZ-END-OF-UPDATES = "Y"
158100             GO TO   D40-EXIT.
158200     PERFORM C60-START-CARD.
158300
158400     GO TO   D40-1-CHECK-UPDATE-AND-POP.
158500
158600 D40-20-PROGRAMS-EQUAL.
158700     MOVE   "Y"   TO WZ-UPDATE-THIS-PROG.
158800     IF      WB-RENUMBER = "R"
158900             MOVE    "Y"    TO WZ-RESEQUENCE-THIS
159000     ELSE
159100             MOVE     SPACE TO WZ-RESEQUENCE-THIS.
159200     READ    CONTROL-CARD-FILE INTO WB-CONTROL-DATA
159300             AT END  MOVE "Y" TO WZ-END-OF-UPDATES.
159400
159500     IF      WB-11 = "*END-UPDATE"
159600             MOVE "Y" TO WZ-END-OF-UPDATES.
159700
159800 D40-EXIT.
159900     EXIT.
160000
160100/
160200 D42-READ-UPDATE-FILE SECTION.
160300*============================
160400 D42-1.
160500     READ    CONTROL-CARD-FILE INTO WB-CONTROL-DATA
160600             AT END  MOVE   "Y"      TO WZ-END-OF-UPDATES
160700                     MOVE   "*START" TO WB-STAR-START
160800                     GO TO   D42-EXIT.
160900
161000     IF      WB-11 = "*END-UPDATE"
161100             MOVE   "Y"      TO WZ-END-OF-UPDATES
161200             MOVE   "*START" TO WB-STAR-START.
161300
161400 D42-EXIT.
161500     EXIT.
161600
161700
161800
161900
162000 D50-PROCESS-COBOL-LINE SECTION.
162100*==============================
162200*
162300****************************************************************
162400*    THIS IS THE MAIN SECTION FOR PROCESSING THE COBOL SOURCE  *
162500*    CODE OF THE POPULATION FILE                               *
162600*                                                              *
162700*                                                              *
162800****************************************************************
162900 D50-1-CHECK-FOR-UPDATING.
163000
163100     IF      WZ-UPDATE-THIS-PROG = "Y"
163200             PERFORM D60-PROCESS-UPDATE-CARD.
163300
163400     IF      WZ-DELETE-FLAG = "Y"
163500             GO TO   D50-EXIT.
163600
163700     IF      WY-NO-SOURCE = SPACE
163800             IF    BLOCK-TYPE NOT = "DATA*"
163900                   PERFORM D80-RAW-LINE.
164000
164100
164200 D50-EXIT.
164300     EXIT.
164400/
164500 D60-PROCESS-UPDATE-CARD SECTION.
164600*===============================
164700*
164800****************************************************************
164900*    THIS IS THE MAIN SECTION FOR PROCESSING ANY UPDATE CARDS  *
165000*    WHICH EXIST FOR THE CURRENT POPULATION FILE PROGRAM       *
165100*    BEING PROCESSED.                                          *
165200*                                                              *
165300*    PERFORMED BY    D50-PROCESS-COBOL-LINE                    *
165400*    PERFORMS        D67-LINE-UPDATE                           *
165500*                    D62-SERIES-UPDATE                         *
165600****************************************************************
165700 D60-1-CHECK-BLOCK-INSERT.
165800     IF      WB-SEQ-1 = SPACES
165900             PERFORM D67-LINE-UPDATE
166000             GO TO   D60-30-CHECK-END.
166100
166200 D60-10-CHECK-FOR-SERIES-UPDATE.
166300     IF      WB-SEQ-1 NOT NUMERIC
166400             GO TO   D60-30-CHECK-END.
166500
166600     IF      WB-SEQ-2 NUMERIC
166700             PERFORM D62-SERIES-UPDATE
166800             GO TO   D60-30-CHECK-END
166900     ELSE
167000             PERFORM D67-LINE-UPDATE.
167100
167200 D60-30-CHECK-END.
167300     IF      WB-11 = "*END-UPDATE"
167400             MOVE   "Y" TO WZ-END-OF-UPDATES.
167500
167600
167700 D60-EXIT.
167800     EXIT.
167900/
168000 D62-SERIES-UPDATE SECTION.
168100*=========================
168200*
168300****************************************************************
168400*    THIS SECTION WILL PROCESS SERIES INSERTIONS OR DELETIONS  *
168500*                                                              *
168600*    PERFORMED BY    D60-PROCESS-UPDATE-CARD                   *
168700*    PERFORMS        C60-START-CARD                            *
168800****************************************************************
168900 D62-1-CHECK-FOR-WITHIN-RANGE.
169000     IF      WC-6 < WB-SEQ-1
169100         IF WZ-WITHIN-DELETE-SERIES-FLAG = "Y"
169200*  ALLOWS FOR OUT-OF-SEQUENCE LINE NUMBER WITHIN DELETE RANGE
169300             GO TO D62-20-NOW-WITHIN-RANGE
169400         END-IF
169500             GO TO   D62-EXIT.
169600
169700     IF      WC-6 NOT > WB-SEQ-2
169800             GO TO   D62-20-NOW-WITHIN-RANGE.
169900
170000 D62-10-READ-NEXT-UPDATE-CARD.
170100             MOVE SPACES TO WZ-WITHIN-DELETE-SERIES-FLAG.
170200             READ    CONTROL-CARD-FILE INTO WB-CONTROL-DATA
170300             AT END  MOVE "Y" TO WZ-END-OF-UPDATES
170400                     GO TO   D62-EXIT.
170500
170600     IF      WB-STAR-START = "*START"
170700             PERFORM C60-START-CARD
170800             GO TO   D62-EXIT.
170900
171000     IF      WB-11 = "*END-UPDATE"
171100             MOVE   "Y" TO WZ-END-OF-UPDATES
171200             GO TO   D62-EXIT.
171300
171400     IF WB-SEQ-2 NUMERIC
171500     GO TO   D62-1-CHECK-FOR-WITHIN-RANGE.
171600     PERFORM D67-LINE-UPDATE.
171700     GO TO D62-EXIT.
171800
171900 D62-20-NOW-WITHIN-RANGE.
172000     MOVE   "Y" TO WZ-LINE-UPDATE.
172100     IF      WB-COL-7 = ","
172200             MOVE "Y" TO WZ-WITHIN-DELETE-SERIES-FLAG
172300             ADD     1  TO WZ-LINES-DELETED
172400             MOVE   "Y" TO WZ-DELETE-FLAG
172500             MOVE    SPACES     TO WE-X-CARD
172600             MOVE   "DELETIONS" TO WE-CHANGE-TYPE
172700             IF WB-SEQ-1 = WB-SEQ-2
172800                  MOVE SPACE TO WZ-WITHIN-DELETE-SERIES-FLAG
172900             END-IF
173000             GO TO   D62-EXIT.
173100
173200     MOVE   "Y"       TO WZ-REPLACE-FLAG.
173300     MOVE    WB-COL-7 TO WC-COL-7.
173400
173500 D62-EXIT.
173600     EXIT.
173700
173800/
173900 D67-LINE-UPDATE SECTION.
174000*=======================
174100*
174200****************************************************************
174300*    THIS SECTION PERFORMS SINGLE LINE AMENDMENTS OR DELETIONS *
174400*                                                              *
174500*    PERFORMED BY    D60-PROCESS-UPDATE-CARD                   *
174600*                    D62-SERIES-UPDATE                         *
174700*    PERFORMS        C60-START-CARD                            *
174800*                    D68-LINE-EQUAL                            *
174900*                    D69-LINE-BLANK-OR-LESS                    *
175000****************************************************************
175100 D67-1-CHECK-FOR-WITHIN-RANGE.
175200     IF      WC-6  < WB-SEQ-1
175300             GO TO   D67-EXIT.
175400
175500 D67-10-LINES-EQUAL-OR-LESS.
175600     MOVE   "Y" TO WZ-LINE-UPDATE.
175700     IF      WB-COL-7 = ","
175800             ADD     1  TO WZ-LINES-DELETED
175900             MOVE   "Y" TO WZ-DELETE-FLAG
176000             MOVE    SPACES     TO WE-X-CARD
176100             MOVE   "DELETION"  TO WE-CHANGE-TYPE
176200             GO TO   D67-20-READ-CONTROL.
176300
176400     IF      WC-6  = WB-SEQ-1
176500             PERFORM D68-LINE-EQUAL
176600             GO TO   D67-EXIT.
176700
176800     IF      WB-SEQ-1 = SPACES
176900             PERFORM D69-LINE-BLANK-OR-LESS
177000             GO TO   D67-EXIT.
177100
177200     IF      WC-6  > WB-SEQ-1
177300             PERFORM D69-LINE-BLANK-OR-LESS
177400             GO TO   D67-EXIT.
177500
177600 D67-20-READ-CONTROL.
177700     READ    CONTROL-CARD-FILE INTO  WB-CONTROL-DATA
177800             AT END  MOVE "Y" TO WZ-END-OF-UPDATES.
177900     IF WB-STAR-START = "*START"
178000             PERFORM C60-START-CARD.
178100
178200     IF WB-11 = "*END-UPDATE"
178300             MOVE "Y" TO WZ-END-OF-UPDATES.
178400
178500 D67-EXIT.
178600     EXIT.
178700/
178800 D68-LINE-EQUAL SECTION.
178900*======================
179000*
179100****************************************************************
179200*    THIS SECTION IS PERFORMED WHEN THE SEQUENCE NUMBER OF A   *
179300*    SINGLE UPDATE AMEND CARD IS THE SAME AS THE SEQUENCE      *
179400*    NUMBER OF THE CURRENT POPULATION FILE LINE BEING PROCESSED*
179500****************************************************************
179600*
179700 D68-1.
179800     MOVE    WB-CONTROL-DATA TO WC-CURRENT-POP-RECORD.
179900     ADD     1    TO WZ-LINES-REPLACED.
180000     MOVE   "Y"   TO WZ-REPLACE-FLAG.
180100
180200     READ    CONTROL-CARD-FILE INTO  WB-CONTROL-DATA
180300             AT END  MOVE "Y" TO WZ-END-OF-UPDATES.
180400
180500     IF      WB-STAR-START = "*START"
180600             PERFORM C60-START-CARD.
180700
180800     IF      WB-11 = "*END-UPDATE"
180900             MOVE   "Y" TO WZ-END-OF-UPDATES.
181000
181100 D68-EXIT.
181200     EXIT.
181300
181400
181500
181600
181700 D69-LINE-BLANK-OR-LESS SECTION.
181800*==============================
181900 D69-1.
182000     MOVE    WB-CONTROL-DATA TO WC-CURRENT-POP-RECORD.
182100     ADD     1    TO WZ-LINES-INSERTED.
182200     MOVE   "Y"   TO WZ-DONT-READ-POPFILE.
182300     MOVE   "INSERTIONS" TO WE-CHANGE-TYPE.
182400     MOVE    SPACES      TO WE-X-CARD.
182500     READ    CONTROL-CARD-FILE INTO  WB-CONTROL-DATA
182600             AT END  MOVE "Y" TO WZ-END-OF-UPDATES.
182700
182800     IF      WB-STAR-START = "*START"
182900             PERFORM C60-START-CARD.
183000
183100     IF      WB-11 = "*END-UPDATE"
183200             MOVE   "Y" TO WZ-END-OF-UPDATES.
183300
183400 D69-EXIT.
183500     EXIT.
183600
183700/
183800 D80-RAW-LINE SECTION.
183900*====================
184000*
184100****************************************************************
184200*    IF COLUMN 7 OF THE POPULATION FILE PROGRAM CONTAINS A     *
184300*    SPECIAL CHARACTER OR COLS. 12-16 CONTAIN X-CARD INFO.,    *
184400*    THIS SECTION WILL MAKE THE NECESSARY CHANGES.             *
184500*                                                              *
184600*    PERFORMED BY    D50-PROCESS-COBOL-LINE                    *
184700*    PERFORMS        D82-OPTIONAL-LETTER                       *
184800*                    D86-X-CARD-CHECK                          *
184900*                    X40-WRITE-SOURCE-FILE                     *
185000****************************************************************
185100 D80-1-ASTERISK-CHECK.
185200     IF      WC-COL-7 = "D" GO TO D80-30-PRINT-LINE.
185300     IF      WC-COL-7  NOT = "*"
185400             GO TO   D80-20-ALPHABETIC-CHECK.
185500
185600     IF      WY-REMOVE-COMMENTS = "Y"
185700             ADD     1  TO WZ-COMMENTS-DELETED
185800             MOVE   "Y" TO WZ-NOT-THIS-COMMENT
185900             GO TO   D80-EXIT.
186000
186100 D80-20-ALPHABETIC-CHECK.
186200     IF      WC-COL-7 NOT ALPHABETIC
186300             GO TO   D80-30-PRINT-LINE.
186400
186500     IF      WC-COL-7 NOT = SPACE
186600             PERFORM D82-OPTIONAL-LETTER.
186700
186800 D80-30-PRINT-LINE.
186900     IF      WZ-DELETE-FLAG = SPACE
187000             PERFORM D85-X-CARD-CHECK.
187100
187200 D80-EXIT.
187300     EXIT.
187400
187500/
187600 D82-OPTIONAL-LETTER SECTION.
187700*===========================
187800*
187900 D82-1-SELECT-OPTIONAL.
188000     PERFORM D83-CHECK-OPTION-SWITCHES
188100             VARYING SUB4 FROM 1 BY 1
188200               UNTIL SUB4 > 26.
188300
188400     IF      WZ-OPTIONAL-SELECTED = "Y"
188500             MOVE    SPACE TO WC-COL-7
188600             GO TO   D82-EXIT.
188700
188800 D82-10-SAVE-OPTIONAL.
188900     ADD     1    TO WZ-CODE-REMOVED.
189000     IF      WY-KILL-DELETIONS = "Y"
189100             MOVE   "Y" TO WZ-DELETE-FLAG
189200     ELSE
189300             MOVE    WC-COL-7 TO WC-COL-8
189400             MOVE   "*"       TO WC-COL-7.
189500
189600 D82-EXIT.
189700     EXIT.
189800
189900
190000
190100
190200
190300 D83-CHECK-OPTION-SWITCHES SECTION.
190400*=================================
190500*
190600 D83-1-CHECK-SWITCH.
190700     IF      WY-OPT-SW (SUB4) = WC-COL-7
190800             MOVE    99 TO SUB4
190900             MOVE   "Y" TO WZ-OPTIONAL-SELECTED.
191000
191100 D83-EXIT.
191200     EXIT.
191300/
191400 D85-X-CARD-CHECK SECTION.
191500*========================
191600*
191700****************************************************************
191800*    WHEN "XXXX" IS ENCOUNTERED IN COLUMNS 12 - 15 THIS        *
191900*    SECTION EITHER SUBSTITUTES THE IMPLEMENTOR ASSIGNED TEXT  *
192000*    OR MOVES THE PROGRAM NAME TO AN IMPLEMENTOR SPECIFIED     *
192100*    POSITION IN THE PROGRAM CODE.                             *
192200*                                                              *
192300*    PERFORMED BY    D80-RAW-LINE                              *
192400*    PERFORMS        D87-PROGRAM-NAME                          *
192500*                    D86-REMOVE-FULL-STOP                      *
192600*                    D86-INSERT-FULL-STOP                      *
192700****************************************************************
192800 D85-1-CHECK-FOR-XXXXX.
192900     IF      WC-12-15 NOT = "XXXX"
193000             GO TO   D85-EXIT.
193100     IF      WC-17-19 NOT NUMERIC
193200* XXXXX ENTRY IN POPULATION FILE IN ERROR.
193300             GO TO D85-EXIT.
193400
193500 D85-10-SUBSTITUTION-REQUIRED.
193600     MOVE    WC-17-19 TO SUB1.
193700     MOVE    WX-X-CARD (SUB1) TO WZ-X-CARD.
193800     IF      WX-PROG-POS (SUB1) NOT = ZERO
193900             PERFORM D88-PROGRAM-NAME
194000             GO TO   D85-EXIT.
194100
194200 D85-20-TEXT-SUBSTITUTION.
194300     IF      WC-20 = SPACE
194400             PERFORM D86-REMOVE-FULL-STOP
194500             VARYING SUB4 FROM 60 BY -1
194600               UNTIL SUB4 < 1
194700             MOVE    WZ-X-CARD TO WC-SUB-DATA
194800             GO TO   D85-EXIT.
194900
195000     IF      WC-20 = "."
195100             PERFORM D87-INSERT-FULL-STOP
195200             VARYING SUB4 FROM 60 BY -1
195300               UNTIL SUB4 < 1
195400             MOVE    WZ-X-CARD  TO WC-SUB-DATA.
195500 D85-EXIT.
195600     EXIT.
195700
195800/
195900 D86-REMOVE-FULL-STOP SECTION.
196000*============================
196100*
196200 D86-1.
196300     IF      WZ-X-CHAR (SUB4) = SPACE
196400             GO TO   D86-EXIT.
196500
196600     IF      WZ-X-CHAR (SUB4) = "."
196700             MOVE    SPACE TO WZ-X-CHAR (SUB4)
196800             MOVE    ZERO  TO SUB4
196900     ELSE
197000             MOVE    ZERO  TO SUB4.
197100
197200 D86-EXIT.
197300     EXIT.
197400
197500
197600
197700
197800
197900 D87-INSERT-FULL-STOP SECTION.
198000*============================
198100*
198200 D87-1.
198300     IF      WZ-X-CHAR (SUB4) = SPACE
198400             GO TO   D87-EXIT.
198500
198600     IF      WZ-X-CHAR (SUB4) = "."
198700             MOVE    ZERO  TO SUB4
198800     ELSE
198900             ADD     1     TO SUB4
199000             MOVE   "."    TO WZ-X-CHAR (SUB4)
199100             MOVE    ZERO  TO SUB4.
199200
199300 D87-EXIT.
199400     EXIT.
199500/
199600 D88-PROGRAM-NAME SECTION.
199700*========================
199800*
199900 D88-1.
200000     MOVE    WX-PROG-POS (SUB1) TO SUB2.
200100     MOVE    SPACE TO WZ-MAIN-PROG-FLAG.
200200     MOVE    SPACE TO WZ-FULL-STOP.
200300     IF      WC-20 = "."
200400             MOVE    "Y" TO WZ-FULL-STOP.
200500
200600     IF      WX-X-CHAR (SUB1 SUB2) = "J"
200700             MOVE WZ-CURRENT-MAIN-PROG TO WZ-PROG-BREAK
200800             MOVE   "Y"    TO WZ-MAIN-PROG-FLAG
200900     ELSE
201000             MOVE WZ-CURRENT-POP-PROG TO WZ-PROG-BREAK.
201100
201200     IF      WZ-CURRENT-MAIN-PROG = SPACE
201300             MOVE    SPACE TO WZ-MAIN-PROG-FLAG.
201400
201500     MOVE    1 TO SUB4.
201600     PERFORM D89-MOVE-PROGRAM-NAME 6 TIMES.
201700
201800     IF      WZ-FULL-STOP = "Y"
201900             PERFORM D87-INSERT-FULL-STOP
202000             VARYING SUB4 FROM 60 BY -1
202100               UNTIL SUB4 < 1
202200     ELSE
202300             PERFORM D86-REMOVE-FULL-STOP
202400             VARYING SUB4 FROM 60 BY -1
202500               UNTIL SUB4 < 1.
202600
202700     MOVE    WZ-X-CARD  TO WC-SUB-DATA.
202800
202900 D88-EXIT.
203000     EXIT.
203100
203200
203300
203400
203500 D89-MOVE-PROGRAM-NAME SECTION.
203600*=============================
203700*
203800 D89-1.
203900*    IF      WZ-MAIN-PROG-FLAG = "Y"
204000*            MOVE    WZ-MAIN-PROG-CHAR (SUB4)
204100*                 TO WZ-X-CHAR (SUB2)
204200*    ELSE
204300*            MOVE    WZ-UPD-PROG-CHAR  (SUB4)
204400*                 TO WZ-X-CHAR (SUB2).
204500
204600     MOVE    WZ-1CHAR(SUB4) TO WZ-X-CHAR(SUB2).
204700
204800     IF      SUB4 = 6 AND WZ-X-CHAR(SUB2) = SPACE
204900             ADD 1 SUB2 GIVING SUB4
205000             PERFORM D89-2-CLOSE-SPACE UNTIL WZ-X-CHAR(SUB4) =
205100                  SPACE
205200             MOVE SPACE TO WZ-X-CHAR(SUB2).
205300
205400     ADD     1 TO SUB2.
205500     ADD     1 TO SUB4.
205600
205700 D89-EXIT.
205800     EXIT.
205900
206000 D89-2-CLOSE-SPACE SECTION.
206100 D89-2-1.
206200
206300     MOVE WZ-X-CHAR(SUB4) TO WZ-X-CHAR(SUB2).
206400     ADD 1 TO SUB2 SUB4.
206500
206600/
206700 D90-PROCESS-END-OF SECTION.
206800*==========================
206900*
207000****************************************************************
207100*    WHEN A "*END-OF"CARD IS ENCOUNTERED THIS SECTION PERFORMS *
207200*    THE END-OF-PROGRAMS ACTIONS.                              *
207300*                                                              *
207400*    PERFORMED BY    D20-PROCESS-POPFILE                       *
207500*    PERFORMS        X10-WRITE-UPDATED-POP                     *
207600****************************************************************
207700 D90-1-CHECK-HEADER-TYPE.
207800     IF      WY-KILL-DELETIONS = "Y"
207900             MOVE   "DELETED"    TO WA-OPTIONAL-CODE
208000     ELSE
208100             MOVE   "UNUSED "    TO WA-OPTIONAL-CODE.
208200
208300     MOVE    WZ-CURRENT-POP-PROG TO WA-CURRENT-PROG
208400     MOVE    WZ-LINES-COBOL      TO WA-LINES-COBOL
208500     MOVE    WZ-LINES-INSERTED   TO WA-LINES-INSERTED
208600     MOVE    WZ-LINES-REPLACED   TO WA-LINES-REPLACED
208700     MOVE    WZ-LINES-DELETED    TO WA-LINES-DELETED
208800     MOVE    WZ-CODE-REMOVED     TO WA-CODE-REMOVED
208900     MOVE    WZ-COMMENTS-DELETED TO WA-COMMENTS-DEL
209000     MOVE    WA-ACCT-LINE-1      TO PRINT-DATA
209100     PERFORM X20-PRINT-DETAIL
209200     MOVE    WA-ACCT-LINE-2      TO PRINT-DATA
209300     PERFORM X20-PRINT-DETAIL
209400     MOVE    WA-ACCT-LINE-3      TO PRINT-DATA
209500     PERFORM X20-PRINT-DETAIL
209600
209700     MOVE    SPACES  TO WZ-CURRENT-POP-PROG.
209800*    CLOSE SOURCE-COBOL-PROGRAMS.
209900
210000 D90-EXIT.
210100     EXIT.
210200
210300
210400 D100-PROCESS-DATA.
210500     EXIT.
210600
210700/
210800 E10-TERMINATE SECTION.
210900*=====================
211000*
211100****************************************************************
211200*    THIS SECTION CLOSES ANY OPEN FILES                        *
211300*                                                              *
211400*                                                              *
211500****************************************************************
211600 E10-1.
211700     CLOSE   PRINT-FILE.
211800
211900     CLOSE   CONTROL-CARD-FILE.
212000
212100     IF      WY-NEW-POP  = "Y"
212200             CLOSE   UPDATED-POPULATION-FILE.
212300
212400*****************************************************************
212500     IF      WY-NO-SOURCE = SPACE
212600             CLOSE   SOURCE-COBOL-PROGRAMS.
212700
212800     CLOSE   POPULATION-FILE.
212900     STOP    RUN.
213000 E20-EXIT.
213100     EXIT.
213200
213300/
213400 X10-WRITE-UPDATED-POPFILE SECTION.
213500*=================================
213600*
213700 X10-1-CHECK-FOR-NC114.
213800*    N.B.
213900*    NC114A HAS MISCELLANEOUS CHARACTERS IN COLUMNS 1-6 AND SO
214000*    SHOULD NOT BE RESEQUENCED BY THIS EXECUTIVE.
214100
214200     IF      WZ-CURRENT-POP-PROG = "NC114M"
214300             GO TO   X10-30-WRITE-FILE.
214400
214500 X10-20-RENUMBER-CHECK.
214600     IF      WC-STAR = "*"
214700             GO TO   X10-30-WRITE-FILE.
214800     IF      WZ-RESEQUENCE-THIS = "Y"
214900             ADD     100 TO WZ-SEQ-NO
215000             MOVE    WZ-SEQ-NO TO WC-6.
215100
215200 X10-30-WRITE-FILE.
215300     MOVE    WC-CURRENT-POP-RECORD TO UD-SOURCE-OUT (SUB5).
215400     ADD     1       TO SUB5.
215500     IF      SUB5    GREATER 30
215600             WRITE   UPDATED-SOURCE-OUT-2400
215700             MOVE    1 TO SUB5
215800             MOVE   SPACE TO UPDATED-SOURCE-OUT-2400.
215900
216000
216100 X10-EXIT.
216200     EXIT.
216300
216400
216500/
216600 X20-PRINT-DETAIL SECTION.
216700*========================
216800*
216900 X20-1.
217000
217100     IF      WZ-LINE-CT > 50
217200             MOVE    PRINT-REC    TO WZ-PRINT-HOLD
217300             ADD     1            TO WZ-PAGE-CT
217400             MOVE    WZ-PAGE-CT   TO WA-PAGE-CT
217500             MOVE    WA-TOP-OF-PAGE-LINE
217600                                  TO PRINT-REC
217700             WRITE   PRINT-REC AFTER PAGE
217800             MOVE    SPACE        TO PRINT-REC
217900             WRITE   PRINT-REC
218000             MOVE    WZ-PRINT-HOLD TO PRINT-REC
218100             MOVE    ZERO TO WZ-LINE-CT.
218200     ADD     1 TO WZ-LINE-CT.
218300
218400 X20-2.
218500     WRITE   PRINT-REC AFTER 1.
218600     MOVE    SPACE TO PRINT-REC.
218700
218800 X20-EXIT.
218900     EXIT.
219000/
219100 X30-READ-POPFILE SECTION.
219200*========================
219300 X30-1.
219400     IF      WZ-DONT-READ-POPFILE = "Y"
219500             MOVE    SPACE TO WZ-DONT-READ-POPFILE
219600             GO TO   X30-EXIT.
219700
219800*    IF      SUB1 GREATER THAN 30
219900     READ    POPULATION-FILE INTO WC-CURRENT-POP-RECORD
220000             AT END  MOVE  "Y"       TO WZ-END-OF-POPFILE
220100                     MOVE  "*HEADER" TO WC-STAR-HEADER
220200                     GO TO  X30-EXIT.
220300     MOVE    WC-CURRENT-POP-RECORD   TO WZ-SAVE-POP-RECORD.
220400
220500     IF      WC-STAR = "*"
220600             IF      WZ-CURRENT-HEADER = "COBOL"
220700     IF WC-1-5 = "ABCDE"
220800          ADD 1 TO WZ-LINES-COBOL
220900     ELSE
221000                     IF      WC-1-5 = "*/+(>"
221100                             ADD     1 TO WZ-LINES-COBOL
221200                     ELSE
221300                     IF      WC-1-5 = "999-9"
221400                             ADD     1 TO WZ-LINES-COBOL
221500                     ELSE
221600                     IF      WC-1-5 = "Z=.,;"
221700                             ADD     1 TO WZ-LINES-COBOL
221800                     ELSE
221900                     IF      WC-1-5 = ")14$ "
222000                             ADD     1 TO WZ-LINES-COBOL
222100                     ELSE
222200                     IF      WC-1-5 = "  23 "
222300                             ADD     1 TO WZ-LINES-COBOL
222400                     ELSE
222500                             NEXT    SENTENCE
222600             ELSE
222700                     NEXT    SENTENCE
222800     ELSE
222900             IF      WZ-CURRENT-HEADER = "COBOL"
223000                     ADD     1    TO WZ-LINES-COBOL.
223100
223200*    MOVE SOURCE-IN (SUB1) TO WC-CURRENT-POP-RECORD.
223300*    ADD     1 TO SUB1.
223400
223500
223600 X30-EXIT.
223700     EXIT.
223800
223900
224000
224100
224200
224300 X40-WRITE-SOURCE-FILE SECTION.
224400*= ===========================
224500 X40-1.
224600     MOVE    SPACES  TO WD-1.
224700*    IF    BLOCK-TYPE NOT = "DATA*"
224800*          MOVE SPACES TO WC-73-80.
224900     IF      WC-STAR = "*"
225000             IF      WC-1-5 = "*/+(>"
225100                     WRITE   CT-OUT FROM WC-CURRENT-POP-RECORD
225200             ELSE
225300                     MOVE    WC-CURRENT-POP-RECORD TO WD-HEADER
225400                     WRITE   CT-OUT FROM WD-SOURCE-REC
225500     ELSE
225600             WRITE   CT-OUT  FROM WC-CURRENT-POP-RECORD.
225700
225800 X40-EXIT.
225900     EXIT.
226000
