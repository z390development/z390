       IDENTIFICATION DIVISION.
       PROGRAM-ID.  COPYFILE.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
TTTTTT     SELECT LINE-FILE
TTTTTT         ASSIGN TO 'ZCOBOL\DEMO\UPROGRAM.LIN'
TTTTTT         ORGANIZATION IS LINE SEQUENTIAL.
TTTTTT     SELECT MZ-ARRAY-FILE
TTTTTT         ASSIGN TO 'ZCOBOL\DEMO\MZARRAY.CBL'
TTTTTT         ORGANIZATION IS LINE SEQUENTIAL.
           SELECT IN-FILE
               ASSIGN TO 'ZCOBOL\DEMO\COPYFILE.IN'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUT-FILE
               ASSIGN TO 'ZCOBOL\DEMO\COPYFILE.OUT'
               ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
TTTTTT FD  LINE-FILE
TTTTTT     DATA RECORD IS LINE-RECORD.
TTTTTT 01  LINE-RECORD.
TTTTTT     03  LINE-LINE-NO          PIC X(06).
TTTTTT     03  FILLER                PIC X(74).
TTTTTT FD  MZ-ARRAY-FILE
TTTTTT     DATA RECORD IS MZ-ARRAY-RECORD.
TTTTTT 01  MZ-ARRAY-RECORD           PIC X(80).
       FD  IN-FILE
           DATA RECORD IS IN-RECORD.
       01  IN-RECORD PIC X(80).
       FD  OUT-FILE
           DATA RECORD IS OUT-RECORD.
       01  OUT-RECORD PIC X(80).
       WORKING-STORAGE SECTION.
       01  WS-LINE PIC X(80).
       01  END-OF-FILE-FLAG PIC X VALUE 'F'.
       77  COUNT COMP-3 PIC S9(4) VALUE 0.

TTTTTT 01  D1 PIC X(52) VALUE
TTTTTT       'U - Debugger for ZCOBOL by John Hennesy'.

TTTTTT 01  D2 PIC X(52) VALUE
TTTTTT       'Enter - single step'.
TTTTTT 01  D3 PIC X(52) VALUE
TTTTTT       'H     - Here'.
TTTTTT 01  D4 PIC X(52) VALUE
TTTTTT       'E     - Erase              - E nnnnnn'.
TTTTTT 01  D4B PIC X(52) VALUE
TTTTTT       '                             E vvv...'.
TTTTTT 01  D5 PIC X(52) VALUE
TTTTTT       'M     - Monitor            - M vvv... (max 30 char)'.
TTTTTT 01  D6 PIC X(52) VALUE
TTTTTT       '                             M         List variables'.
TTTTTT 01  D7 PIC X(52) VALUE
TTTTTT       'B     - Break              - B nnnnnn'.
TTTTTT 01  D8 PIC X(52) VALUE
TTTTTT       '                             B         List lines'.
TTTTTT 01  D9 PIC X(52) VALUE
TTTTTT       'G     - Go                 - G nnnnnn'.
TTTTTT 01  D9B PIC X(52) VALUE
TTTTTT       '                             G vvv...'.
TTTTTT 01  DA PIC X(52) VALUE
TTTTTT       '                             G         Go next break'.
TTTTTT 01  DB PIC X(52) VALUE
TTTTTT       'W     - Working storage    - W         Show w/s'.
TTTTTT 01  DC PIC X(52) VALUE
TTTTTT       'P     - Procedure division - P         Show proc div'.
TTTTTT 01  DD PIC X(52) VALUE
TTTTTT       'D     - Display            - D vvv... (max 30 char)'.
TTTTTT 01  DE PIC X(52) VALUE
TTTTTT       'X     - eXit               - X         Exit'.
TTTTTT 01  DF PIC X(52) VALUE
TTTTTT       '+     - another screen     - +         of W or P'.
TTTTTT 01  DG PIC X(52) VALUE
TTTTTT       '?     - HELP'.
TTTTTT 01  DH PIC X(52) VALUE
TTTTTT       'T     - Trace'.
TTTTTT 01  BREAKIT-ARRAY.
TTTTTT     03  BREAKIT-NUMBER OCCURS 9
TTTTTT                               PIC X(06).
TTTTTT 01  BUB                       PIC 9(06).
TTTTTT 01  BUB2                      PIC 9(06).
TTTTTT 01  SIX-SPACES                PIC X(06) VALUE SPACES.
TTTTTT 01  SIX-LOW-VALUES            PIC X(06) VALUE LOW-VALUES.
TTTTTT 01  SIXCH                     PIC X(06).
TTTTTT 01  GOIT-FOUND                PIC X(01).
TTTTTT 01  MONITORIT-ARRAY.
TTTTTT     03  MONITORIT-ENTRY OCCURS 9.
TTTTTT         05  MONITORIT-VARIABLE
TTTTTT                               PIC X(32).
TTTTTT         05  MONITORIT-FLAG    PIC X(01).
TTTTTT         05  MONITORIT-POINTER PIC 9(06).
TTTTTT         05  MONITORIT-LENGTH  PIC 9(06).
TTTTTT         05  MONITORIT-SUB1    PIC X(30).
TTTTTT         05  MONITORIT-SUB2    PIC X(30).
TTTTTT         05  MONITORIT-SUB3    PIC X(30).
TTTTTT 01  MONITORIT-MAX             PIC 9(06) VALUE 1.
TTTTTT 01  MONITORIT-CHUNK.
TTTTTT     03  MONITORIT-CHUNK-C     PIC X(01) OCCURS 60000.
TTTTTT 01  MONITORIT-80              PIC X(80).
TTTTTT 01  MONITORIT-SHORT           PIC X(30).
TTTTTT 01  EUB                       PIC 9(06).
TTTTTT 01  NUB                       PIC 9(06).
TTTTTT 01  NUB2                      PIC 9(06).
TTTTTT 01  TH2-SPACES                PIC X(32) VALUE SPACES.
TTTTTT 01  TH2-LOW-VALUES            PIC X(32) VALUE LOW-VALUES.
TTTTTT 01  DUB                       PIC 9(06).
TTTTTT 01  MONITORIT-CHANGE          PIC X(01).
TTTTTT 01  A-CHAR                    PIC X(01).
TTTTTT 01  B-CHAR                    PIC X(01).

TTTTTT 01  GOIT-OUTSTANDING          PIC X(01) VALUE 'N'.
TTTTTT 01  GOIT-NUMBER               PIC X(06).
TTTTTT 01  WORKIT-END                PIC X(01).
TTTTTT 01  PROCIT-END                PIC X(01).
TTTTTT 01  PREV-COMMAND              PIC X(01).
TTTTTT 01  CARROTS                   PIC X(01).
TTTTTT 01  MUB2                      PIC 9(06).
TTTTTT 01  MUB3                      PIC 9(06).
TTTTTT 01  MUB4                      PIC 9(06).
TTTTTT 01  LUMP.
TTTTT+     03  LUMP-C OCCURS 64000   PIC X(01).
TTTTTT 01  FILLER REDEFINES LUMP.
TTTTT+     03  LUMP-BLOCK OCCURS 2000 PIC X(32).
TTTTTT 01  LUMB                      PIC 9(06).
TTTTTT 01  BLOC.
TTTTTT     03  BLOC-C OCCURS 32      PIC X(01).
TTTTTT 01  BLOCH.
TTTTTT     03  BLOCH-BI OCCURS 32.
TTTTTT         05  BLOCH-BI-LHS      PIC X(01).
TTTTTT         05  BLOCH-BI-RHS      PIC X(01).
TTTTTT 01  GUB                       PIC 9(06).
TTTTTT 01  HUB                       PIC 9(06).
TTTTTT 01  GUB2                      PIC 9(06).
TTTTTT 01  BLOCG.
TTTTTT     03  BLOCG-BI OCCURS 32.
TTTTTT         05  BLOCG-BI-LHS      PIC X(01).
TTTTTT         05  BLOCG-BI-RHS      PIC X(01).
TTTTTT 01  GUB3                      PIC 9(06).
TTTTTT 01  GUB4                      PIC 9(06).
TTTTTT 01  GUB5                      PIC 9(06).
TTTTTT 01  GUB6                      PIC 9(06).
TTTTTT 01  FLAGG                     PIC X(01).
TTTTTT 01  NINE-04-COMP              PIC 9(04) COMP.
TTTTTT 01  FILLER REDEFINES NINE-04-COMP.
TTTTTT     03  THE-1ST-CHAR          PIC X(01).
TTTTTT     03  THE-2ND-CHAR          PIC X(01).
TTTTTT 01  LHS                       PIC 9(03).
TTTTTT 01  RHS                       PIC 9(03).
TTTTTT 01  THE-ARRAY                 PIC X(16) VALUE '0123456789ABCDEF'.
TTTTTT 01  FILLER REDEFINES THE-ARRAY.
TTTTTT     03  THE-ARRAY-C OCCURS 16 PIC X(01).
TTTTTT 01  DISPLAYABLE               PIC X(01).
TTTTTT 01  NONDISPLAYABLE            PIC X(01).
TTTTTT 01  T-FIRST                   PIC X(01) VALUE SPACE.
TTTTTT 01  A-LINE                    PIC X(06).
TTTTTT 01  MUB                       PIC 9(06).
TTTTTT 01  THE-COMMAND.
TTTTTT     03  THE-COMMAND-1         PIC X(01).
TTTTTT     03  FILLER                PIC X(01).
TTTTTT     03  THE-COMMAND-3-32.
TTTTTT         05  THE-COMMAND-3-8   PIC X(06).
TTTTTT         05  FILLER            PIC X(24).
TTTTTT     03  FILLER                PIC X(148).
TTTTTT 01  FILLER REDEFINES THE-COMMAND.
TTTTTT     03  THE-COMMAND-C OCCURS 180 PIC X(01).
TTTTTT 01  THUB                      PIC 9(03).
TTTTTT 01  GLOB-COMMAND              PIC X(01).
TTTTTT 01  GLOB-NAME.
TTTTTT     03  GLOB-NAME-C OCCURS 30 PIC X(01).
TTTTTT 01  GLOB-SIX-DIGITS           PIC X(06).
TTTTTT 01  GLUB1                     PIC 9(03).
TTTTTT 01  GLUB2                     PIC 9(03).
TTTTTT 01  GLUB3                     PIC 9(03).
TTTTTT 01  GLOB-SUB1                 PIC X(30).
TTTTTT 01  GLOB-SUB2                 PIC X(30).
TTTTTT 01  GLOB-SUB3                 PIC X(30).
TTTTTT 01  TEST-FIELD                PIC X(30).
TTTTTT 01  FILLER REDEFINES TEST-FIELD.
TTTTTT     03  TEST-FIELD-C OCCURS 30 PIC X(01).
TTTTTT 01  TEST-NAME                 PIC X(30).
TTTTTT 01  FUB                       PIC 9(02).
TTTTTT 01  FUB-NAME0                 PIC X(30).
TTTTTT 01  FUB-NAME1                 PIC X(30).
TTTTTT 01  FUB-NAME2                 PIC X(30).
TTTTTT 01  FUB-NAME3                 PIC X(30).
TTTTTT 01  FUB-NAME4                 PIC X(30).
TTTTTT 01  FUB-NAME5                 PIC X(30).
TTTTTT 01  FUB-NAME6                 PIC X(30).
TTTTTT 01  LEN0                      PIC 9(02).
TTTTTT 01  KEEP-LEN0                 PIC 9(02).
TTTTTT 01  LEN1                      PIC 9(02).
TTTTTT 01  LEN2                      PIC 9(02).
TTTTTT 01  LEN3                      PIC 9(02).
TTTTTT 01  LEN4                      PIC 9(02).
TTTTTT 01  LEN5                      PIC 9(02).
TTTTTT 01  LEN6                      PIC 9(02).
TTTTTT 01  THIRTY-SPACES             PIC X(30) VALUE SPACES.
TTTTTT 01  T-SERVICE.
TTTTTT     03  T-PROG                PIC X(08).
TTTTTT     03  T-PARA                PIC X(30).
TTTTTT     03  T-LINE                PIC X(06).
TTTTTT* AFTER T-SERVICE
TTTTTT* BEGINNING AFTER T-SERVICE
TTTTTT 01  MZ-ARRAY.
TTTTTT     03  MZ-ENTRY OCCURS 999.
TTTTTT         05  MZ-LVL           PIC X(02).
TTTTTT         05  MZ-NAME          PIC X(30).
TTTTTT         05  MZ-ADDR          PIC 9(09).
TTTTTT         05  MZ-LEN           PIC 9(09).
TTTTTT         05  MZ-DUP           PIC X(06).
TTTTTT         05  MZ-SUBS          PIC 9(01).
TTTTTT         05  MZ-TYPE          PIC X(01).
TTTTTT         05  MZ-PIC.
TTTTTT             07  FILLER       PIC X(01).
TTTTTT             07  MZ-PIC-2     PIC X(01).
TTTTTT             07  FILLER       PIC X(08).
TTTTTT         05  MZ-SUBS-IDS OCCURS 3.
TTTTTT             07  MZ-SUBS-ID   PIC X(30).
TTTTTT 01  MZ-MAX                   PIC 9(03) VALUE 999.
TTTTTT 01  MZ-SUB                   PIC 9(03) VALUE 0.
TTTTTT 01  MZ-SUB2                  PIC 9(03).
TTTTTT 01  MZ-SUB3                  PIC 9(03).
TTTTTT 01  MZ-END-FILE-FLAG         PIC X(01).
TTTTTT 01  A-NUMBER                  PIC X(06).
TTTTTT 01  B-NUMBER                  PIC X(06).
TTTTTT 01  LINE-COUNT                PIC 9(06) VALUE ZERO.
TTTTTT 01  LINE-COUNT2               PIC 9(06) VALUE ZERO.
TTTTTT 01  LINE-ARRAY.
TTTTTT     03  LINE-ENTRY OCCURS 999.
TTTTTT         05  LINE-NUMBER       PIC X(06).
TTTTTT         05  LINE-ASTER        PIC X(01).
TTTTTT         05  LINE-30           PIC X(30).
TTTTTT         05  FILLER            PIC X(35).
TTTTTT 01  LUB                       PIC 9(06).
TTTTTT 01  LUB2                      PIC 9(06).
TTTTTT 01  LUB-MAX                   PIC 9(06).
TTTTTT 01  LUB3                      PIC 9(06).
TTTTTT 01  LUB4                      PIC 9(06).
TTTTTT 01  ONECH                     PIC X(01).
TTTTTT 01  THIRTYCH                  PIC X(30).
TTTTTT 01  PARA-FOUND                PIC X(01).
TTTTTT 01  LINE-FILE-ENDED           PIC X(01).
TTTTTT 01  MATCH-FOUND               PIC X(01).
TTTTTT 01  SCREEN-ARRAY.
TTTTTT     03  SCREEN-ENTRY OCCURS 17.
TTTTTT         05  SCREEN-NUMBER     PIC X(06).
TTTTTT         05  SCREEN-POINTER    PIC X(01).
TTTTTT         05  SCREEN-REST       PIC X(66).
TTTTTT 01  SCUB                      PIC 9(06).
TTTTTT 01  SCREEN-FIRST              PIC 9(06).
TTTTTT 01  SCUB2                     PIC 9(06).
TTTTTT 01  SCREEN-LAST               PIC 9(06).
TTTTTT 01  SCREEN-1ST-FLAG           PIC X(01).
TTTTTT 01  LEFFED                    PIC X(06).
TTTTTT 01  RITE                      PIC X(06).
TTTTTT 01  SEVENT2                   PIC X(72).
TTTTTT 01  THIRSTY.
TTTTTT     03  THIRSTY-C OCCURS 30   PIC X(01).
TTTTTT 01  NINE06                    PIC 9(06).
TTTTTT 01  DISP-SUB1                 PIC 9(06).
TTTTTT 01  DISP-SUB2                 PIC 9(06).
TTTTTT 01  DISP-SUB3                 PIC 9(06).
TTTTTT 01  TEMP-SUB1                 PIC X(30).
TTTTTT 01  TEMP-NAME                 PIC X(30).
TTTTTT 01  DIZPLAY                   PIC X(01).
TTTTTT 01  MONITORING                PIC X(01).
TTTTTT 01  GOIT-NAME                 PIC X(30).
TTTTTT 01  GOIT-NAME-MATCHED         PIC X(01).
TTTTTT 01  SUBS-NUM                  PIC 9(01).
TTTTTT 01  TRACE-ARRAY.
TTTTTT     03  TRACE-ENTRY OCCURS 15 PIC X(30).
TTTTTT 01  TUB                       PIC 9(06).
TTTTTT 01  TUB2                      PIC 9(06).
TTTTTT 01  TUB3                      PIC 9(06).
TTTTTT 01  XOK                       PIC X(01).
       LINKAGE SECTION.
       PROCEDURE DIVISION.
       MAINLINE.
TTTTTT     MOVE 'COPYFILE' TO T-PROG
TTTTTT     MOVE 'MAINLINE                      ' TO T-PARA
TTTTTT     MOVE '000001' TO T-LINE
TTTTTT     PERFORM T
000001     DISPLAY 'COPYFILE STARTED'
TTTTTT     MOVE 'COPYFILE' TO T-PROG
TTTTTT     MOVE 'MAINLINE                      ' TO T-PARA
TTTTTT     MOVE '000002' TO T-LINE
TTTTTT     PERFORM T
000002     PERFORM INITIALIZATION-ROUTINE.
TTTTTT     MOVE 'COPYFILE' TO T-PROG
TTTTTT     MOVE 'MAINLINE                      ' TO T-PARA
TTTTTT     MOVE '000003' TO T-LINE
TTTTTT     PERFORM T
000003     PERFORM THE-LOOP
                   UNTIL END-OF-FILE-FLAG = 'Y'.
TTTTTT     MOVE 'COPYFILE' TO T-PROG
TTTTTT     MOVE 'MAINLINE                      ' TO T-PARA
TTTTTT     MOVE '000004' TO T-LINE
TTTTTT     PERFORM T
000004     PERFORM EOJ-ROUTINE.
TTTTTT     MOVE 'COPYFILE' TO T-PROG
TTTTTT     MOVE 'MAINLINE                      ' TO T-PARA
TTTTTT     MOVE '000005' TO T-LINE
TTTTTT     PERFORM T
000005     DISPLAY 'COPYFILE RECORDS COPIED = ' COUNT.
TTTTTT     MOVE 'COPYFILE' TO T-PROG
TTTTTT     MOVE 'MAINLINE                      ' TO T-PARA
TTTTTT     MOVE '000006' TO T-LINE
TTTTTT     PERFORM T
000006     IF COUNT NOT = 17
TTTTTT     MOVE 'COPYFILE' TO T-PROG
TTTTTT     MOVE 'MAINLINE                      ' TO T-PARA
TTTTTT     MOVE '000007' TO T-LINE
TTTTTT     PERFORM T
000007        DISPLAY 'COPYFILE RECORD COUNT ERROR'
TTTTTT     MOVE 'COPYFILE' TO T-PROG
TTTTTT     MOVE 'MAINLINE                      ' TO T-PARA
TTTTTT     MOVE '000008' TO T-LINE
TTTTTT     PERFORM T
000008        MOVE 16 TO RETURN-CODE
TTTTTT     MOVE 'COPYFILE' TO T-PROG
TTTTTT     MOVE 'MAINLINE                      ' TO T-PARA
TTTTTT     MOVE '000009' TO T-LINE
TTTTTT     PERFORM T
000009        STOP RUN
           END-IF
TTTTTT     MOVE 'COPYFILE' TO T-PROG
TTTTTT     MOVE 'MAINLINE                      ' TO T-PARA
TTTTTT     MOVE '000010' TO T-LINE
TTTTTT     PERFORM T
000010     DISPLAY 'COPYFILE ENDED OK'
TTTTTT     MOVE 'COPYFILE' TO T-PROG
TTTTTT     MOVE 'MAINLINE                      ' TO T-PARA
TTTTTT     MOVE '000011' TO T-LINE
TTTTTT     PERFORM T
000011     STOP RUN.
       INITIALIZATION-ROUTINE.
TTTTTT     MOVE 'COPYFILE' TO T-PROG
TTTTTT     MOVE 'INITIALIZATION-ROUTINE        ' TO T-PARA
TTTTTT     MOVE '000012' TO T-LINE
TTTTTT     PERFORM T
000012     OPEN INPUT IN-FILE.
TTTTTT     MOVE 'COPYFILE' TO T-PROG
TTTTTT     MOVE 'INITIALIZATION-ROUTINE        ' TO T-PARA
TTTTTT     MOVE '000013' TO T-LINE
TTTTTT     PERFORM T
000013     OPEN OUTPUT OUT-FILE.
TTTTTT     MOVE 'COPYFILE' TO T-PROG
TTTTTT     MOVE 'INITIALIZATION-ROUTINE        ' TO T-PARA
TTTTTT     MOVE '000014' TO T-LINE
TTTTTT     PERFORM T
000014     READ IN-FILE INTO OUT-RECORD
                AT END MOVE 'Y' TO END-OF-FILE-FLAG.
       THE-LOOP.
TTTTTT     MOVE 'COPYFILE' TO T-PROG
TTTTTT     MOVE 'THE-LOOP                      ' TO T-PARA
TTTTTT     MOVE '000015' TO T-LINE
TTTTTT     PERFORM T
000015     WRITE OUT-RECORD.
TTTTTT     MOVE 'COPYFILE' TO T-PROG
TTTTTT     MOVE 'THE-LOOP                      ' TO T-PARA
TTTTTT     MOVE '000016' TO T-LINE
TTTTTT     PERFORM T
000016     ADD 1 TO COUNT
TTTTTT     MOVE 'COPYFILE' TO T-PROG
TTTTTT     MOVE 'THE-LOOP                      ' TO T-PARA
TTTTTT     MOVE '000017' TO T-LINE
TTTTTT     PERFORM T
000017     READ IN-FILE INTO OUT-RECORD
                AT END MOVE 'Y' TO END-OF-FILE-FLAG.
       EOJ-ROUTINE.
TTTTTT     MOVE 'COPYFILE' TO T-PROG
TTTTTT     MOVE 'EOJ-ROUTINE                   ' TO T-PARA
TTTTTT     MOVE '000018' TO T-LINE
TTTTTT     PERFORM T
000018     CLOSE IN-FILE.
TTTTTT     MOVE 'COPYFILE' TO T-PROG
TTTTTT     MOVE 'EOJ-ROUTINE                   ' TO T-PARA
TTTTTT     MOVE '000019' TO T-LINE
TTTTTT     PERFORM T
000019     CLOSE OUT-FILE.
TTTTTT/
TTTTTT T SECTION.
TTTTTT     MOVE T-LINE               TO A-LINE.
TTTTTT     IF T-FIRST = SPACE
TTTTTT         MOVE SPACES           TO BREAKIT-ARRAY
TTTTTT         MOVE SPACES           TO MONITORIT-ARRAY
TTTTTT         MOVE SPACES           TO TRACE-ARRAY
TTTTTT         MOVE 1                TO TUB
TTTTTT         MOVE ZEROES           TO BUB
TTTTTT         MOVE ZEROES           TO NUB
TTTTTT         MOVE 'N'              TO T-FIRST
TTTTTT                                  MZ-END-FILE-FLAG
TTTTTT         OPEN INPUT MZ-ARRAY-FILE
TTTTTT         PERFORM VARYING MZ-SUB
TTTTTT            FROM 1 BY 1
TTTTTT           UNTIL MZ-END-FILE-FLAG = 'Y'
TTTTTT               READ MZ-ARRAY-FILE AT END
TTTTTT                       MOVE 'Y' TO MZ-END-FILE-FLAG
TTTTTT               END-READ
TTTTTT               IF MZ-END-FILE-FLAG = 'N'
TTTTTT                   MOVE MZ-ARRAY-RECORD
TTTTTT                     TO MZ-ENTRY (MZ-SUB)
TTTTTT               END-IF
TTTTTT         END-PERFORM
TTTTTT         SUBTRACT 2          FROM MZ-SUB
TTTTTT         CLOSE MZ-ARRAY-FILE
TTTTTT         OPEN INPUT LINE-FILE
TTTTTT         MOVE 'N'              TO LINE-FILE-ENDED
TTTTTT         PERFORM READ-LINE-FILE
TTTTTT           UNTIL LINE-FILE-ENDED = 'Y'
TTTTTT         CLOSE LINE-FILE
TTTTTT         DISPLAY D1
TTTTTT         DISPLAY D2
TTTTTT         DISPLAY D3
TTTTTT         DISPLAY D4
TTTTTT         DISPLAY D4B
TTTTTT         DISPLAY D5
TTTTTT         DISPLAY D6
TTTTTT         DISPLAY D7
TTTTTT         DISPLAY D8
TTTTTT         DISPLAY D9
TTTTTT         DISPLAY D9B
TTTTTT         DISPLAY DA
TTTTTT         DISPLAY DB
TTTTTT         DISPLAY DC
TTTTTT         DISPLAY DD
TTTTTT         DISPLAY DE
TTTTTT         DISPLAY DF
TTTTTT         DISPLAY DG
TTTTTT         DISPLAY DH
TTTTTT         ACCEPT THE-COMMAND FROM CONSOLE
TTTTTT     END-IF.
TTTTTT
TTTTTT     IF TRACE-ENTRY (TUB) = T-PARA
TTTTTT         CONTINUE
TTTTTT     ELSE
TTTTTT         ADD 1                 TO TUB
TTTTTT         IF TUB > 15
TTTTTT             MOVE 1            TO TUB
TTTTTT         END-IF
TTTTTT         MOVE T-PARA           TO TRACE-ENTRY (TUB)
TTTTTT     END-IF.
TTTTTT
TTTTTT     PERFORM
TTTTTT         VARYING LINE-COUNT2
TTTTTT            FROM 1 BY 1
TTTTTT           UNTIL LINE-NUMBER (LINE-COUNT2) = A-LINE
TTTTTT              OR LINE-COUNT2 > LINE-COUNT
TTTTTT     END-PERFORM.
TTTTTT     IF LINE-COUNT2 > LINE-COUNT
TTTTTT         DISPLAY 'T-LINE (' T-LINE ') NOT FOUND'
TTTTTT         STOP RUN
TTTTTT     END-IF.
TTTTTT     PERFORM MONITORIT-FIND
TTTTTT         VARYING NUB2 FROM 1 BY 1 UNTIL NUB2 > NUB.
TTTTTT     IF GOIT-NAME-MATCHED = 'Y'
TTTTTT         MOVE 'N' TO GOIT-NAME-MATCHED
TTTTTT         MOVE SPACES TO GOIT-NAME
TTTTTT         MOVE 'N' TO GOIT-OUTSTANDING
TTTTTT     END-IF.
TTTTTT
TTTTTT     IF GOIT-OUTSTANDING = 'Y'
TTTTTT         IF GOIT-NUMBER = 'BREAK'
TTTTTT             PERFORM BREAKIT-FIND
TTTTTT             IF A-NUMBER
TTTTTT              = B-NUMBER
TTTTTT                 MOVE 'N'          TO GOIT-OUTSTANDING
TTTTTT             ELSE
TTTTTT                 GO TO T-EXIT
TTTTTT             END-IF
TTTTTT         ELSE
TTTTTT             IF GOIT-NUMBER = LINE-NUMBER (LINE-COUNT2)
TTTTTT                 MOVE 'N'          TO GOIT-OUTSTANDING
TTTTTT             ELSE
TTTTTT                 GO TO T-EXIT
TTTTTT             END-IF
TTTTTT         END-IF
TTTTTT     END-IF.
TTTTTT     MOVE  1                   TO LUB.
TTTTTT     MOVE 'Y'                  TO SCREEN-1ST-FLAG.
TTTTTT     MOVE ZEROES               TO SCREEN-FIRST.
TTTTTT     MOVE ZEROES               TO SCREEN-LAST.
TTTTTT     MOVE SPACES               TO SCREEN-ARRAY.
TTTTTT     MOVE  15                  TO LUB-MAX.
TTTTTT     DISPLAY '*-------------------------------*'.
TTTTTT     DISPLAY 'PROG=' T-PROG ' PARA=' T-PARA.
TTTTTT     DISPLAY ' '.
TTTTTT     PERFORM SCREENIT.
TTTTTT     DISPLAY '*-------------------------------*'.
TTTTTT T-ACCEPT.
TTTTTT
TTTTTT     IF TRACE-ENTRY (TUB) = T-PARA
TTTTTT         CONTINUE
TTTTTT     ELSE
TTTTTT         ADD 1                 TO TUB
TTTTTT         IF TUB > 15
TTTTTT             MOVE 1            TO TUB
TTTTTT         END-IF
TTTTTT         MOVE T-PARA           TO TRACE-ENTRY (TUB)
TTTTTT     END-IF.
TTTTTT
TTTTTT     ACCEPT THE-COMMAND      FROM CONSOLE.
TTTTTT     PERFORM LOWER-TO-UPPER.
TTTTTT     MOVE SPACES               TO GLOB-COMMAND
TTTTTT                                  GLOB-NAME
TTTTTT                                  GLOB-SUB1
TTTTTT                                  GLOB-SUB2
TTTTTT                                  GLOB-SUB3.
TTTTTT     MOVE THE-COMMAND-1        TO GLOB-COMMAND.
TTTTTT     MOVE 2                    TO GLUB1.
TTTTTT TA1.
TTTTTT     ADD  1                    TO GLUB1.
TTTTTT     IF GLUB1 > 180
TTTTTT         GO TO TA2
TTTTTT     END-IF.
TTTTTT     IF THE-COMMAND-C (GLUB1) = SPACE
TTTTTT         GO TO TA2
TTTTTT     END-IF.
TTTTTT     IF THE-COMMAND-C (GLUB1) = '('
TTTTTT         GO TO TA2
TTTTTT     END-IF.

TTTTTT     GO TO TA1.

TTTTTT TA2.
TTTTTT     MOVE SPACES TO GLOB-NAME.
TTTTTT     IF GLUB1 NOT > 180
TTTTTT         MOVE SPACES TO GLOB-NAME
TTTTTT         SUBTRACT 3 FROM GLUB1 GIVING GLUB2
TTTTTT         MOVE THE-COMMAND (3:GLUB2)
TTTTTT           TO GLOB-NAME
TTTTTT     END-IF.
TTTTTT     IF  GLOB-NAME (1:1) NOT GREATER THAN '9'
TTTTTT     AND GLOB-NAME (1:1) NOT LESS    THAN '0'
TTTTTT         MOVE '000000' TO SIXCH
TTTTTT         COMPUTE GLUB3 = 7 - GLUB2
TTTTTT         MOVE GLOB-NAME (1:GLUB2)
TTTTTT           TO SIXCH (GLUB3:GLUB2)
TTTTTT         MOVE SIXCH TO GLOB-NAME
TTTTTT     END-IF.
TTTTTT TA3.
TTTTTT     ADD 1 TO GLUB1
TTTTTT     IF GLUB1 > 180
TTTTTT         GO TO TA4
TTTTTT     END-IF.
TTTTTT     IF (THE-COMMAND-C (GLUB1) NOT < 'A'
TTTTTT         AND
TTTTTT         THE-COMMAND-C (GLUB1) NOT > 'Z')
TTTTTT             GO TO TA4
TTTTTT     END-IF.
TTTTTT     IF (THE-COMMAND-C (GLUB1) NOT < '0'
TTTTTT         AND
TTTTTT         THE-COMMAND-C (GLUB1) NOT > '9')
TTTTTT             GO TO TA4
TTTTTT     END-IF.
TTTTTT     GO TO TA3.

TTTTTT TA4.
TTTTTT     MOVE GLUB1 TO GLUB3.
TTTTTT TA5.
TTTTTT     IF GLUB1 > 180
TTTTTT         GO TO TA6
TTTTTT     END-IF.
TTTTTT     IF THE-COMMAND-C (GLUB1) = SPACE
TTTTTT         GO TO TA6
TTTTTT     END-IF.
TTTTTT     IF THE-COMMAND-C (GLUB1) = ')'
TTTTTT         GO TO TA6
TTTTTT     END-IF.
TTTTTT     IF THE-COMMAND-C (GLUB1) = ','
TTTTTT         GO TO TA6
TTTTTT     END-IF.
TTTTTT     ADD  1 TO GLUB1
TTTTTT     GO TO TA5.
TTTTTT
TTTTTT TA6.
TTTTTT     MOVE SPACES TO GLOB-SUB1.
TTTTTT     IF GLUB1 NOT > 180
TTTTTT         MOVE SPACES TO GLOB-SUB1
TTTTTT         SUBTRACT GLUB3 FROM GLUB1 GIVING GLUB2
TTTTTT         MOVE THE-COMMAND (GLUB3:GLUB2)
TTTTTT           TO GLOB-SUB1
TTTTTT     END-IF.
TTTTTT     IF  GLOB-SUB1 (1:1) NOT GREATER THAN '9'
TTTTTT     AND GLOB-SUB1 (1:1) NOT LESS    THAN '0'
TTTTTT         MOVE '000000' TO SIXCH
TTTTTT         COMPUTE GLUB3 = 7 - GLUB2
TTTTTT         MOVE GLOB-SUB1 (1:GLUB2)
TTTTTT           TO SIXCH (GLUB3:GLUB2)
TTTTTT         MOVE SIXCH TO GLOB-SUB1
TTTTTT     END-IF.
TTTTTT TA7.
TTTTTT     ADD 1 TO GLUB1.
TTTTTT     IF GLUB1 > 180
TTTTTT         GO TO TA8
TTTTTT     END-IF.
TTTTTT     IF (THE-COMMAND-C (GLUB1) NOT < 'A'
TTTTTT         AND
TTTTTT         THE-COMMAND-C (GLUB1) NOT > 'Z')
TTTTTT             GO TO TA8
TTTTTT     END-IF.
TTTTTT     IF (THE-COMMAND-C (GLUB1) NOT < '0'
TTTTTT         AND
TTTTTT         THE-COMMAND-C (GLUB1) NOT > '9')
TTTTTT             GO TO TA8
TTTTTT     END-IF.
TTTTTT     GO TO TA7.
TTTTTT
TTTTTT TA8.
TTTTTT     MOVE GLUB1 TO GLUB3.
TTTTTT TA9.
TTTTTT     IF GLUB1 > 180
TTTTTT         GO TO TA10
TTTTTT     END-IF.
TTTTTT     IF THE-COMMAND-C (GLUB1) = SPACE
TTTTTT         GO TO TA10
TTTTTT     END-IF,
TTTTTT     IF THE-COMMAND-C (GLUB1) = ','
TTTTTT         GO TO TA10
TTTTTT     END-IF.
TTTTTT     IF THE-COMMAND-C (GLUB1) = ')'
TTTTTT         GO TO TA10
TTTTTT     END-IF.
TTTTTT     ADD  1 TO GLUB1.
TTTTTT     GO TO TA9.
TTTTTT
TTTTTT TA10.
TTTTTT     MOVE SPACES TO GLOB-SUB2.
TTTTTT     IF GLUB1 NOT > 180
TTTTTT         MOVE SPACES TO GLOB-SUB2
TTTTTT         SUBTRACT GLUB3 FROM GLUB1 GIVING GLUB2
TTTTTT         MOVE THE-COMMAND (GLUB3:GLUB2)
TTTTTT           TO GLOB-SUB2
TTTTTT     END-IF.
TTTTTT     IF  GLOB-SUB2 (1:1) NOT GREATER THAN '9'
TTTTTT     AND GLOB-SUB2 (1:1) NOT LESS    THAN '0'
TTTTTT         MOVE '000000' TO SIXCH
TTTTTT         COMPUTE GLUB3 = 7 - GLUB2
TTTTTT         MOVE GLOB-SUB2 (1:GLUB2)
TTTTTT           TO SIXCH (GLUB3:GLUB2)
TTTTTT         MOVE SIXCH TO GLOB-SUB2
TTTTTT     END-IF.
TTTTTT TA11.
TTTTTT     ADD 1 TO GLUB1.
TTTTTT     IF GLUB1 > 180
TTTTTT         GO TO TA12
TTTTTT     END-IF.
TTTTTT     IF (THE-COMMAND-C (GLUB1) NOT < 'A'
TTTTTT         AND
TTTTTT         THE-COMMAND-C (GLUB1) NOT > 'Z')
TTTTTT             GO TO TA12
TTTTTT     END-IF.
TTTTTT     IF (THE-COMMAND-C (GLUB1) NOT < '0'
TTTTTT         AND
TTTTTT         THE-COMMAND-C (GLUB1) NOT > '9')
TTTTTT             GO TO TA12
TTTTTT     END-IF.
TTTTTT     GO TO TA11.
TTTTTT
TTTTTT TA12.
TTTTTT     MOVE GLUB1 TO GLUB3.
TTTTTT TA13.
TTTTTT     IF GLUB1 > 180
TTTTTT         GO TO TA14
TTTTTT     END-IF.
TTTTTT     IF THE-COMMAND-C (GLUB1) = SPACE
TTTTTT         GO TO TA14
TTTTTT     END-IF,
TTTTTT     IF THE-COMMAND-C (GLUB1) = ','
TTTTTT         GO TO TA14
TTTTTT     END-IF.
TTTTTT     IF THE-COMMAND-C (GLUB1) = ')'
TTTTTT         GO TO TA14
TTTTTT     END-IF.
TTTTTT     ADD  1 TO GLUB1.
TTTTTT     GO TO TA13.
TTTTTT
TTTTTT TA14.
TTTTTT     MOVE SPACES TO GLOB-SUB3.
TTTTTT     IF GLUB1 NOT > 180
TTTTTT         MOVE SPACES TO GLOB-SUB3
TTTTTT         SUBTRACT GLUB3 FROM GLUB1 GIVING GLUB2
TTTTTT         MOVE THE-COMMAND (GLUB3:GLUB2)
TTTTTT           TO GLOB-SUB3
TTTTTT     END-IF.
TTTTTT     IF  GLOB-SUB3 (1:1) NOT GREATER THAN '9'
TTTTTT     AND GLOB-SUB3 (1:1) NOT LESS    THAN '0'
TTTTTT         MOVE '000000' TO SIXCH
TTTTTT         COMPUTE GLUB3 = 7 - GLUB2
TTTTTT         MOVE GLOB-SUB3 (1:GLUB2)
TTTTTT           TO SIXCH (GLUB3:GLUB2)
TTTTTT         MOVE SIXCH TO GLOB-SUB3
TTTTTT     END-IF.
TTTTTT TA15.
TTTTTT*     DISPLAY '>>>> GLOB-COMMAND=' GLOB-COMMAND
TTTTTT*     DISPLAY '>>>+ GLOB-NAME====' GLOB-NAME
TTTTTT*     DISPLAY '>>>+ GLOB-SUB1====' GLOB-SUB1
TTTTTT*     DISPLAY '>>>+ GLOB-SUB2====' GLOB-SUB2
TTTTTT*     DISPLAY '>>>. GLOB-SUB3====' GLOB-SUB3
TTTTTT     IF  GLOB-NAME (1:1) NOT GREATER THAN '9'
TTTTTT     AND GLOB-NAME (1:1) NOT LESS    THAN '0'
TTTTTT         MOVE GLOB-NAME (1:6)
TTTTTT           TO THE-COMMAND-3-8
TTTTTT     END-IF.
TTTTTT
TTTTTT*---------------------------------------------------------------
TTTTTT*       'H     - Here'.
TTTTTT*       'E     - Erase              - E nnnnnn'.
TTTTTT*       'M     - Monitor            - M vvv... (max 30 char)'.
TTTTTT*       '                             M         List variables'.
TTTTTT*       'B     - Break              - B nnnnnn'.
TTTTTT*       '                             B         List lines'.
TTTTTT*       'G     - Go                 - G nnnnnn'.
TTTTTT*       '                             G         Go next break'.
TTTTTT*       'W     - Working storage    - W         Show w/s'.
TTTTTT*       'P     - Procedure division - P         Show proc div'.
TTTTTT*       'D     - Display            - D vvv... (max 30 char)'.
TTTTTT*       'X     - eXit               - X         Exit'.
TTTTTT*       '+     - another screen     - +         of W or P'.
TTTTTT*--1---------2---------3---------4---------5---------6---------712
TTTTTT     IF GLOB-COMMAND = '?'
TTTTTT         IF THE-COMMAND-C (3) = 'H'
TTTTTT             DISPLAY 'H    Here'
TTTTTT             DISPLAY ' '
TTTTTT             DISPLAY 'Redisplay the page of your code upon'
TTTTTT             DISPLAY 'which your current statement occurs'
TTTTTT             DISPLAY ' '
TTTTTT             DISPLAY ' '
TTTTTT             DISPLAY ' '
TTTTTT             DISPLAY ' '
TTTTTT             DISPLAY ' '
TTTTTT             DISPLAY ' '
TTTTTT             DISPLAY ' '
TTTTTT             DISPLAY ' '
TTTTTT             DISPLAY ' '
TTTTTT             DISPLAY ' '
TTTTTT             DISPLAY ' '
TTTTTT             DISPLAY ' '
TTTTTT             DISPLAY ' '
TTTTTT             DISPLAY ' '
TTTTTT             DISPLAY ' '
TTTTTT         END-IF
TTTTTT         IF THE-COMMAND-C (3) = 'E'
TTTTTT             DISPLAY 'E    Erase'
TTTTTT             DISPLAY ' '
TTTTTT             DISPLAY 'E nnnnnn'
TTTTTT             DISPLAY 'Erases a breakpoint'
TTTTTT             DISPLAY '  You do not need leading zeroes now'
TTTTTT             DISPLAY ' '
TTTTTT             DISPLAY 'E vvv... (max 30 char)'
TTTTTT             DISPLAY 'Erases a monitor'
TTTTTT             DISPLAY '  it is not necessary to enter the whole'
TTTTTT             DISPLAY '  variable name, just enter sufficient'
TTTTTT             DISPLAY '  characters to uniquely identify the'
TTTTTT             DISPLAY '  monitor to be erased'
TTTTTT             DISPLAY ' '
TTTTTT             DISPLAY ' '
TTTTTT             DISPLAY ' '
TTTTTT             DISPLAY ' '
TTTTTT             DISPLAY ' '
TTTTTT             DISPLAY ' '
TTTTTT             DISPLAY ' '
TTTTTT         END-IF
TTTTTT         IF THE-COMMAND-C (3) = 'M'
TTTTTT             DISPLAY 'M    Monitor'
TTTTTT             DISPLAY ' '
TTTTTT             DISPLAY 'M vvv... (max 30 char)'
TTTTTT             DISPLAY '  the identified field is monitored and'
TTTTTT             DISPLAY '  will be displayed when its value changes'
TTTTTT             DISPLAY ' '
TTTTTT             DISPLAY '  the field identified may be followed'
TTTTTT             DISPLAY '  by 1 thru 3 subscripts'
TTTTTT             DISPLAY '  starting with an open bracket "("'
TTTTTT             DISPLAY '  ending with a close bracket ")"'
TTTTTT             DISPLAY '  and separated by a comma ","'
TTTTTT             DISPLAY '  if the subscript is numeric, it doesnt'
TTTTTT             DISPLAY '  require leading zeroes now'
TTTTTT             DISPLAY '  the subscript may be the name of a '
TTTTTT             DISPLAY '  variable'
TTTTTT             DISPLAY '  e.g. M FRED(1, MY-SUBSCRIPT, 2)'
TTTTTT             DISPLAY ' '
TTTTTT             DISPLAY 'M'
TTTTTT             DISPLAY '  displays a list of the current monitors'
TTTTTT         END-IF
TTTTTT         IF THE-COMMAND-C (3) = 'B'
TTTTTT             DISPLAY 'B    Break'
TTTTTT             DISPLAY ' '
TTTTTT             DISPLAY 'B nnnnnn'
TTTTTT             DISPLAY '  a breakpoint is created for the '
TTTTTT             DISPLAY '  identified line. You dont need leading 0s'
TTTTTT             DISPLAY '  you can issue a G (Go) command without a'
TTTTTT             DISPLAY '  line number to go to the next breakpoint'
TTTTTT             DISPLAY ' '
TTTTTT             DISPLAY 'B'
TTTTTT             DISPLAY '  displays a list of the current '
TTTTTT             DISPLAY '  breakpoints'
TTTTTT             DISPLAY ' '
TTTTTT             DISPLAY ' '
TTTTTT             DISPLAY ' '
TTTTTT             DISPLAY ' '
TTTTTT             DISPLAY ' '
TTTTTT             DISPLAY ' '
TTTTTT             DISPLAY ' '
TTTTTT             DISPLAY ' '
TTTTTT         END-IF
TTTTTT         IF THE-COMMAND-C (3) = 'G'
TTTTTT             DISPLAY 'G    Go'
TTTTTT             DISPLAY ' '
TTTTTT             DISPLAY 'G nnnnnn'
TTTTTT             DISPLAY '  go to the identified line number'
TTTTTT             DISPLAY ' (no leading zeroes)'
TTTTTT             DISPLAY '  it still performs all the statements til'
TTTTTT             DISPLAY '  it reaches the chosen line number but it'
TTTTTT             DISPLAY '  doesnt show the pages of code'
TTTTTT             DISPLAY ' '
TTTTTT             DISPLAY 'G vvv...'
TTTTTT             DISPLAY '  go until the monitored variable changes'
TTTTTT             DISPLAY '  value'
TTTTTT             DISPLAY ' '
TTTTTT             DISPLAY 'G'
TTTTTT             DISPLAY '  go to the next breakpoint'
TTTTTT             DISPLAY '  it, too, still performs all the'
TTTTTT             DISPLAY '  statements that are in between'
TTTTTT             DISPLAY ' '
TTTTTT             DISPLAY '  you have to single-step (ie Hit Enter) to'
TTTTTT             DISPLAY '  the next instruction before you can G '
TTTTTT             DISPLAY '  again'
TTTTTT         END-IF
TTTTTT         IF THE-COMMAND-C (3) = 'W'
TTTTTT             DISPLAY 'W    Working-Storage'
TTTTTT             DISPLAY ' '
TTTTTT             DISPLAY 'Actually lists the fields that are '
TTTTTT             DISPLAY 'accessible for display, a page at a time,'
TTTTTT             DISPLAY 'see "+" command'
TTTTTT             DISPLAY ' '
TTTTTT             DISPLAY 'these fields may occur in the File Section'
TTTTTT             DISPLAY 'or Working Storage Section'
TTTTTT             DISPLAY ' '
TTTTTT             DISPLAY ' '
TTTTTT             DISPLAY ' '
TTTTTT             DISPLAY ' '
TTTTTT             DISPLAY ' '
TTTTTT             DISPLAY ' '
TTTTTT             DISPLAY ' '
TTTTTT             DISPLAY ' '
TTTTTT             DISPLAY ' '
TTTTTT             DISPLAY ' '
TTTTTT             DISPLAY ' '
TTTTTT         END-IF
TTTTTT         IF THE-COMMAND-C (3) = 'P'
TTTTTT             DISPLAY 'P    Procedure Division'
TTTTTT             DISPLAY ' '
TTTTTT             DISPLAY 'lists the code, a page at a time, with line'
TTTTTT             DISPLAY 'numbers against the verbs'
TTTTTT             DISPLAY ' '
TTTTTT             DISPLAY 'P nnnnnn '
TTTTTT             DISPLAY ' '
TTTTTT             DISPLAY 'lists the code, starting at or above '
TTTTTT             DISPLAY 'given line number (no leading zeroes)'
TTTTTT             DISPLAY ' '
TTTTTT             DISPLAY ' '
TTTTTT             DISPLAY 'P pppsss... '
TTTTTT             DISPLAY 'lists the code, starting at the paragraph '
TTTTTT             DISPLAY 'or section given'
TTTTTT             DISPLAY ' '
TTTTTT             DISPLAY ' '
TTTTTT             DISPLAY ' '
TTTTTT             DISPLAY ' '
TTTTTT         END-IF
TTTTTT         IF THE-COMMAND-C (3) = 'D'
TTTTTT             DISPLAY 'D    Display'
TTTTTT             DISPLAY ' '
TTTTTT             DISPLAY 'D vvv... (max 30 char)'
TTTTTT             DISPLAY '  the identified field is displayed'
TTTTTT             DISPLAY ' '
TTTTTT             DISPLAY '  the field identified may be followed'
TTTTTT             DISPLAY '  by 1 thru 3 subscripts'
TTTTTT             DISPLAY '  starting with an open bracket "("'
TTTTTT             DISPLAY '  ending with a close bracket ")"'
TTTTTT             DISPLAY '  and separated by a comma ","'
TTTTTT             DISPLAY '  if the subscript is numeric, it doesnt'
TTTTTT             DISPLAY '  need leading zeroes'
TTTTTT             DISPLAY '  the subscript may be the name of a '
TTTTTT             DISPLAY '  variable'
TTTTTT             DISPLAY '  e.g. D ARTHUR(MY-SUB1, MY-SUB2, MY-SUB3)'
TTTTTT             DISPLAY ' '
TTTTTT             DISPLAY ' '
TTTTTT             DISPLAY ' '
TTTTTT             DISPLAY ' '
TTTTTT         END-IF
TTTTTT         IF THE-COMMAND-C (3) = 'X'
TTTTTT             DISPLAY 'X    eXit'
TTTTTT             DISPLAY 'exit from your program'
TTTTTT             DISPLAY ' '
TTTTTT             DISPLAY ' '
TTTTTT             DISPLAY ' '
TTTTTT             DISPLAY ' '
TTTTTT             DISPLAY ' '
TTTTTT             DISPLAY ' '
TTTTTT             DISPLAY ' '
TTTTTT             DISPLAY ' '
TTTTTT             DISPLAY ' '
TTTTTT             DISPLAY ' '
TTTTTT             DISPLAY ' '
TTTTTT             DISPLAY ' '
TTTTTT             DISPLAY ' '
TTTTTT             DISPLAY ' '
TTTTTT             DISPLAY ' '
TTTTTT             DISPLAY ' '
TTTTTT             DISPLAY ' '
TTTTTT         END-IF
TTTTTT         IF THE-COMMAND-C (3) = '+'
TTTTTT             DISPLAY '+    another page'
TTTTTT             DISPLAY ' '
TTTTTT             DISPLAY 'if previous command was W, then another'
TTTTTT             DISPLAY 'page of Working Storage is displayed'
TTTTTT             DISPLAY ' '
TTTTTT             DISPLAY 'if previous command was P, then another'
TTTTTT             DISPLAY 'page of the Procedure Division is displayed'
      *  1         2         3         4         5         6         712
TTTTTT             DISPLAY ' '
TTTTTT             DISPLAY ' '
TTTTTT             DISPLAY ' '
TTTTTT             DISPLAY ' '
TTTTTT             DISPLAY ' '
TTTTTT             DISPLAY ' '
TTTTTT             DISPLAY ' '
TTTTTT             DISPLAY ' '
TTTTTT             DISPLAY ' '
TTTTTT             DISPLAY ' '
TTTTTT             DISPLAY ' '
TTTTTT             DISPLAY ' '
TTTTTT         END-IF
TTTTTT         IF THE-COMMAND-C (3) = 'T'
TTTTTT             DISPLAY 'T    Trace'
TTTTTT             DISPLAY 'shows you the last 15 para/sections'
TTTTTT             DISPLAY 'executed '
TTTTTT             DISPLAY ' '
TTTTTT             DISPLAY ' '
TTTTTT             DISPLAY ' '
TTTTTT             DISPLAY ' '
TTTTTT             DISPLAY ' '
TTTTTT             DISPLAY ' '
TTTTTT             DISPLAY ' '
TTTTTT             DISPLAY ' '
TTTTTT             DISPLAY ' '
TTTTTT             DISPLAY ' '
TTTTTT             DISPLAY ' '
TTTTTT             DISPLAY ' '
TTTTTT             DISPLAY ' '
TTTTTT             DISPLAY ' '
TTTTTT             DISPLAY ' '
TTTTTT             DISPLAY ' '
TTTTTT         END-IF
TTTTTT         GO TO T-ACCEPT
TTTTTT     END-IF.
TTTTTT
TTTTTT     IF GLOB-COMMAND = 'H'
TTTTTT         MOVE 'H'              TO PREV-COMMAND
TTTTTT         GO TO T
TTTTTT     END-IF.
TTTTTT
TTTTTT     IF GLOB-COMMAND = 'T'
TTTTTT         ADD 1 TO TUB GIVING TUB3
TTTTTT         PERFORM VARYING TUB2 FROM 1 BY 1 UNTIL TUB2 > 15
TTTTTT             IF TUB3 > 15
TTTTTT                 MOVE 1 TO TUB3
TTTTTT             END-IF
TTTTTT             IF TRACE-ENTRY (TUB3) NOT = SPACES
TTTTTT                 DISPLAY TRACE-ENTRY (TUB3)
TTTTTT             END-IF
TTTTTT             ADD 1 TO TUB3
TTTTTT         END-PERFORM
TTTTTT         MOVE 'T'              TO PREV-COMMAND
TTTTTT         GO TO T-ACCEPT
TTTTTT     END-IF.
TTTTTT
TTTTTT     IF GLOB-COMMAND = 'E'
TTTTTT         IF THE-COMMAND-3-8 = SIX-SPACES
TTTTTT             GO TO T-ACCEPT
TTTTTT         END-IF
TTTTTT         PERFORM VARYING BUB2
TTTTTT            FROM 1 BY 1
TTTTTT           UNTIL BREAKIT-NUMBER (BUB2) = THE-COMMAND-3-8
TTTTTT              OR BUB2 > 8
TTTTTT         END-PERFORM
TTTTTT         IF BUB2 NOT > 8
TTTTTT              DISPLAY 'Erasing ' BREAKIT-NUMBER (BUB2)
TTTTTT              MOVE SIX-LOW-VALUES   TO BREAKIT-NUMBER (BUB2)
TTTTTT         END-IF
TTTTTT         IF  BUB2 > 8
TTTTTT         AND GLOB-NAME (1:1) NOT GREATER THAN '9'
TTTTTT         AND GLOB-NAME (1:1) NOT LESS    THAN '0'
TTTTTT             DISPLAY 'INVALID BREAK LINE NUMBER'
TTTTTT             MOVE 'E' TO PREV-COMMAND
TTTTTT             GO TO T-ACCEPT
TTTTTT         END-IF
TTTTTT         PERFORM VARYING NUB2
TTTTTT            FROM 1 BY 1
TTTTTT           UNTIL MONITORIT-VARIABLE (NUB2) = GLOB-NAME
TTTTTT              OR NUB2 > 8
TTTTTT         END-PERFORM
TTTTTT         IF  NUB2 NOT > 8
TTTTTT             MOVE TH2-LOW-VALUES   TO MONITORIT-VARIABLE (NUB2)
TTTTTT         ELSE
TTTTTT             PERFORM VARYING MUB2
TTTTTT                FROM 1 BY 1
TTTTTT               UNTIL GLOB-NAME-C (MUB2) = ' '
TTTTTT                  OR GLOB-NAME-C (MUB2) = '('
TTTTTT             END-PERFORM
TTTTTT             SUBTRACT 1 FROM MUB2
TTTTTT             PERFORM VARYING NUB2
TTTTTT                FROM 1 BY 1
TTTTTT               UNTIL NUB2 > 8
TTTTTT                 MOVE SPACES TO MONITORIT-SHORT
TTTTTT                 MOVE MONITORIT-VARIABLE (NUB2) (1:MUB2)
TTTTTT                   TO MONITORIT-SHORT
TTTTTT                 IF GLOB-NAME = MONITORIT-SHORT
TTTTTT                     DISPLAY 'Erasing '
TTTTTT                              MONITORIT-VARIABLE (NUB2)
TTTTTT                     MOVE TH2-LOW-VALUES
TTTTTT                       TO MONITORIT-VARIABLE (NUB2)
TTTTTT                 END-IF
TTTTTT             END-PERFORM
TTTTTT         END-IF
TTTTTT         MOVE 'E'              TO PREV-COMMAND
TTTTTT         GO TO T-ACCEPT
TTTTTT     END-IF.
TTTTTT
TTTTTT     IF GLOB-COMMAND = 'M'
TTTTTT         IF THE-COMMAND-3-8 = SIX-SPACES
TTTTTT             DISPLAY '*----------------------------*'
TTTTTT             DISPLAY 'MONITOR LIST'
TTTTTT             DISPLAY ' '
TTTTTT             PERFORM MONITORIT-LIST VARYING NUB2
TTTTTT                FROM 1 BY 1
TTTTTT               UNTIL MONITORIT-VARIABLE (NUB2) = TH2-SPACES
TTTTTT                  OR NUB2 > 9
TTTTTT             DISPLAY '*----------------------------*'
TTTTTT             MOVE 'M'          TO PREV-COMMAND
TTTTTT             GO TO T-ACCEPT
TTTTTT         ELSE
TTTTTT             PERFORM MATCHIT
TTTTTT             IF MATCH-FOUND = 'N'
TTTTTT                 MOVE 'M'      TO PREV-COMMAND
TTTTTT                 GO TO T-ACCEPT
TTTTTT             END-IF
TTTTTT             PERFORM VARYING NUB
TTTTTT                FROM 1 BY 1
TTTTTT               UNTIL MONITORIT-VARIABLE (NUB) = TH2-SPACES
TTTTTT                  OR MONITORIT-VARIABLE (NUB) = GLOB-NAME
TTTTTT                  OR MONITORIT-VARIABLE (NUB) = TH2-LOW-VALUES
TTTTTT                  OR NUB > 8
TTTTTT             END-PERFORM
TTTTTT             IF NUB > 8
TTTTTT                 DISPLAY 'MAX MONITORS EXCEEDED'
TTTTTT             ELSE
TTTTTT                 MOVE GLOB-NAME TO MONITORIT-VARIABLE (NUB)
TTTTTT                 MOVE GLOB-SUB1 TO MONITORIT-SUB1     (NUB)
TTTTTT                 MOVE GLOB-SUB2 TO MONITORIT-SUB2     (NUB)
TTTTTT                 MOVE GLOB-SUB3 TO MONITORIT-SUB3     (NUB)
TTTTTT                 MOVE 'N'              TO MONITORIT-FLAG     (NUB)
TTTTTT                 MOVE ZEROES           TO MONITORIT-POINTER  (NUB)
TTTTTT                 MOVE ZEROES           TO MONITORIT-LENGTH   (NUB)
TTTTTT             END-IF
TTTTTT             PERFORM VARYING NUB
TTTTTT                FROM 1 BY 1
TTTTTT               UNTIL MONITORIT-VARIABLE (NUB) = TH2-SPACES
TTTTTT             END-PERFORM
TTTTTT             MOVE 'M'              TO PREV-COMMAND
TTTTTT             GO TO T-ACCEPT
TTTTTT         END-IF
TTTTTT     END-IF.
TTTTTT
TTTTTT     IF GLOB-COMMAND = 'B'
TTTTTT         IF THE-COMMAND-3-8 = SIX-SPACES
TTTTTT             DISPLAY '*----------------------------*'
TTTTTT             DISPLAY 'BREAK LIST'
TTTTTT             DISPLAY ' '
TTTTTT             PERFORM BREAKIT-LIST VARYING BUB2
TTTTTT                FROM 1 BY 1
TTTTTT               UNTIL BREAKIT-NUMBER (BUB2) = SIX-SPACES
TTTTTT                  OR BUB2 > 9
TTTTTT             DISPLAY '*----------------------------*'
TTTTTT             MOVE 'B'          TO PREV-COMMAND
TTTTTT             GO TO T-ACCEPT
TTTTTT         ELSE
TTTTTT             MOVE 'BREAK'      TO GOIT-NUMBER
TTTTTT             PERFORM VARYING BUB
TTTTTT                FROM 1 BY 1
TTTTTT               UNTIL BREAKIT-NUMBER (BUB) = SIX-SPACES
TTTTTT                  OR BREAKIT-NUMBER (BUB) = THE-COMMAND-3-8
TTTTTT                  OR BREAKIT-NUMBER (BUB) = SIX-LOW-VALUES
TTTTTT                  OR BUB > 8
TTTTTT             END-PERFORM
TTTTTT             IF BUB > 8
TTTTTT                 DISPLAY 'MAX BREAKS EXCEEDED'
TTTTTT             ELSE
TTTTTT                 MOVE THE-COMMAND-3-8 TO BREAKIT-NUMBER (BUB)
TTTTTT             END-IF
TTTTTT             PERFORM VARYING BUB
TTTTTT                FROM 1 BY 1
TTTTTT               UNTIL BREAKIT-NUMBER (BUB) = SIX-SPACES
TTTTTT             END-PERFORM
TTTTTT             MOVE 'B'              TO PREV-COMMAND
TTTTTT             GO TO T-ACCEPT
TTTTTT         END-IF
TTTTTT     END-IF.
TTTTTT
TTTTTT     IF GLOB-COMMAND = 'G'
TTTTTT         MOVE 'Y'              TO GOIT-OUTSTANDING
TTTTTT         MOVE GLOB-NAME        TO GOIT-NAME
TTTTTT         MOVE 'N'              TO GOIT-NAME-MATCHED
TTTTTT         IF THE-COMMAND-3-8 NOT = SIX-SPACES
TTTTTT             MOVE GLOB-NAME (1:6) TO GOIT-NUMBER
TTTTTT         END-IF
TTTTTT         MOVE GLOB-COMMAND    TO PREV-COMMAND
TTTTTT         GO TO T
TTTTTT     END-IF.
TTTTTT
TTTTTT     IF GLOB-COMMAND = 'W'
TTTTTT         PERFORM WORKIT
TTTTTT         MOVE GLOB-COMMAND    TO PREV-COMMAND
TTTTTT         GO TO T-ACCEPT
TTTTTT     END-IF.
TTTTTT
TTTTTT     IF GLOB-COMMAND = 'P'
TTTTTT         PERFORM PROCIT
TTTTTT         MOVE GLOB-COMMAND    TO PREV-COMMAND
TTTTTT         GO TO T-ACCEPT
TTTTTT     END-IF.
TTTTTT
TTTTTT     IF GLOB-COMMAND = 'D'
TTTTTT         MOVE 'N' TO MONITORING
TTTTTT         MOVE 'Y' TO DIZPLAY
TTTTTT         MOVE 1 TO NUB2
TTTTTT         PERFORM PROCESS-DISPLAY
TTTTTT         GO TO T-ACCEPT
TTTTTT     END-IF.
TTTTTT
TTTTTT     IF GLOB-COMMAND = 'X'
TTTTTT         DISPLAY 'EXITING'
TTTTTT         STOP RUN
TTTTIT     END-IF.
TTTTTT
TTTTTT     IF GLOB-COMMAND = '+'
TTTTTT         IF PREV-COMMAND = 'W'
TTTTTT             PERFORM WORKIT-PLUS
TTTTTT             GO TO T-ACCEPT
TTTTTT         END-IF
TTTTTT         IF PREV-COMMAND = 'P'
TTTTTT             PERFORM PROCIT-PLUS
TTTTTT             GO TO T-ACCEPT
TTTTTT         END-IF
TTTTTT     END-IF.
TTTTTT
TTTTTT T-EXIT. EXIT.
TTTTTT
TTTTTT PROCESS-DISPLAY SECTION.
TTTTTT     PERFORM VARYING MZ-SUB2
TTTTTT        FROM 1 BY 1
TTTTTT       UNTIL MZ-SUB2 > MZ-SUB
TTTTTT          OR GLOB-NAME = MZ-NAME (MZ-SUB2)
TTTTTT     END-PERFORM.
TTTTTT     IF MZ-SUB2 > MZ-SUB
TTTTTT         DISPLAY 'VARIABLE NOT FOUND '
TTTTTT                  GLOB-NAME
TTTTTT     ELSE
TTTTTT         IF MZ-PIC-2 (MZ-SUB2) = '9'
TTTTTT         OR MZ-PIC-2 (MZ-SUB2) = 'Z'
TTTTTT             PERFORM NAME9-PROCESS
TTTTTT         ELSE
TTTTTT             IF MZ-PIC-2 (MZ-SUB2)
TTTTTT             NOT ='A'
TTTTTT                 PERFORM NAMEX-PROCESS
TTTTTT                 IF XOK = 'Y'
TTTTTT                     MOVE 'N' TO NONDISPLAYABLE
TTTTTT                     MOVE 'N' TO DISPLAYABLE
TTTTTT                     PERFORM DISPLAYIT
TTTTTT                         VARYING LUMB FROM 1 BY 1
TTTTTT                           UNTIL LUMB > MZ-LEN (MZ-SUB2)
TTTTTT                              OR LUMB > 64000
TTTTTT                     MOVE MZ-LEN (MZ-SUB2) TO MUB2
TTTTTT                     COMPUTE GUB = (MUB2 / 32)
TTTTTT                     COMPUTE HUB = MUB2 - (GUB * 32)
TTTTTT                     COMPUTE GUB = GUB + 1
TTTTTT                     MOVE 1                    TO GUB2
TTTTTT                     MOVE 1                    TO GUB6
TTTTTT                     IF DIZPLAY = 'Y'
TTTTTT                     IF DISPLAYABLE = 'Y'
TTTTTT                         IF NONDISPLAYABLE = 'N'
TTTTTT                             PERFORM DISPLAYABLE-LUMP GUB TIMES
TTTTTT                         ELSE
TTTTTT                             MOVE 1            TO GUB3
TTTTTT                             PERFORM NONDISP-LUMP
TTTTTT                         END-IF
TTTTTT                     ELSE
TTTTTT                         MOVE 1            TO GUB3
TTTTTT                         PERFORM NONDISP-LUMP
TTTTTT                     END-IF
TTTTTT                     END-IF
TTTTTT                 END-IF
TTTTTT             END-IF
TTTTTT         END-IF
TTTTTT         MOVE GLOB-COMMAND    TO PREV-COMMAND
TTTTTT     END-IF.
TTTTTT PROCESS-DISPLAY-EXIT. EXIT.
TTTTTT
TTTTTT BREAKIT-LIST SECTION.
TTTTTT     IF BREAKIT-NUMBER (BUB2) NOT = SIX-LOW-VALUES
TTTTTT         DISPLAY BREAKIT-NUMBER (BUB2)
TTTTTT     END-IF.
TTTTTT BREAKIT-LIST-EXIT. EXIT.
TTTTTT
TTTTTT BREAKIT-FIND SECTION.
TTTTTT     MOVE 1                    TO BUB2.
TTTTTT BREAKIT-FIND-LOOP.
TTTTTT     IF BUB2 > BUB
TTTTTT        GO TO BREAKIT-FIND-EXIT
TTTTTT     END-IF.
TTTTTT     MOVE BREAKIT-NUMBER (BUB2) TO A-NUMBER.
TTTTTT     MOVE LINE-NUMBER (LINE-COUNT2) TO B-NUMBER.
TTTTTT     IF A-NUMBER = B-NUMBER
TTTTTT         NEXT SENTENCE
TTTTTT     ELSE
TTTTTT         ADD 1                 TO BUB2
TTTTTT         GO TO BREAKIT-FIND-LOOP
TTTTTT     END-IF.
TTTTTT BREAKIT-FIND-EXIT. EXIT.
TTTTTT
TTTTTT MONITORIT-FIND SECTION.
TTTTTT     MOVE 'Y' TO MONITORING.
TTTTTT     MOVE 'N' TO DIZPLAY.
TTTTTT     MOVE MONITORIT-VARIABLE (NUB2)
TTTTTT       TO GLOB-NAME.
TTTTTT     PERFORM MATCHIT.
TTTTTT
TTTTTT     IF MATCH-FOUND = 'Y'
TTTTTT         MOVE MONITORIT-SUB1 (NUB2) TO GLOB-SUB1
TTTTTT         MOVE MONITORIT-SUB2 (NUB2) TO GLOB-SUB2
TTTTTT         MOVE MONITORIT-SUB3 (NUB2) TO GLOB-SUB3
TTTTTT         PERFORM PROCESS-DISPLAY
TTTTTT         IF MONITORIT-FLAG (NUB2) = 'N'
TTTTTT             MOVE MONITORIT-MAX
TTTTTT               TO MONITORIT-POINTER (NUB2)
TTTTTT             ADD  MZ-LEN (MZ-SUB2) TO MONITORIT-MAX
TTTTTT             MOVE MZ-LEN (MZ-SUB2)
TTTTTT               TO MONITORIT-LENGTH  (NUB2)
TTTTTT             MOVE 'Y'
TTTTTT               TO MONITORIT-FLAG    (NUB2)
TTTTTT         END-IF
TTTTTT         PERFORM MONITORIT-CHECK
TTTTTT         IF MONITORIT-CHANGE = 'Y'
TTTTTT             MOVE 'Y' TO DIZPLAY
TTTTTT             PERFORM PROCESS-DISPLAY
TTTTTT             IF GLOB-NAME = GOIT-NAME
TTTTTT                 MOVE 'Y' TO GOIT-NAME-MATCHED
TTTTTT             ELSE
TTTTTT                 MOVE 'N' TO GOIT-NAME-MATCHED
TTTTTT             END-IF
TTTTTT         END-IF
TTTTTT         PERFORM MONITORIT-UPDATE
TTTTTT     END-IF.
TTTTTT MONITORIT-FIND-EXIT. EXIT.
TTTTTT
TTTTTT MONITORIT-CHECK SECTION.
TTTTTT     MOVE 1                     TO DUB.
TTTTTT     MOVE MONITORIT-POINTER (NUB2)
TTTTTT                                TO EUB.
TTTTTT     MOVE 'N'                   TO MONITORIT-CHANGE.
TTTTTT MONITORIT-CHECK-LOOP.
TTTTTT     IF DUB > MONITORIT-LENGTH (NUB2)
TTTTTT         GO TO MONITORIT-CHECK-EXIT
TTTTTT     END-IF.
TTTTTT     MOVE MONITORIT-CHUNK-C (EUB) TO A-CHAR.
TTTTTT     MOVE LUMP-C (DUB)            TO B-CHAR.
TTTTTT     IF A-CHAR NOT = B-CHAR
TTTTTT         MOVE 'Y'                 TO MONITORIT-CHANGE
TTTTTT         GO TO MONITORIT-CHECK-EXIT
TTTTTT     END-IF.
TTTTTT     ADD 1                        TO DUB.
TTTTTT     ADD 1                        TO EUB.
TTTTTT     GO TO MONITORIT-CHECK-LOOP.
TTTTTT MONITORIT-CHECK-EXIT. EXIT.
TTTTTT
TTTTTT MONITORIT-UPDATE SECTION.
TTTTTT     MOVE 1                     TO DUB.
TTTTTT     MOVE MONITORIT-POINTER (NUB2)
TTTTTT                                TO EUB.
TTTTTT     MOVE 'N'                   TO MONITORIT-CHANGE.
TTTTTT MONITORIT-UPDATE-LOOP.
TTTTTT     IF DUB > MONITORIT-LENGTH (NUB2)
TTTTTT         GO TO MONITORIT-UPDATE-EXIT
TTTTTT     END-IF.
TTTTTT     MOVE LUMP-C (DUB)
TTTTTT       TO MONITORIT-CHUNK-C (EUB).
TTTTTT     ADD 1                        TO DUB.
TTTTTT     ADD 1                        TO EUB.
TTTTTT     GO TO MONITORIT-UPDATE-LOOP.
TTTTTT MONITORIT-UPDATE-EXIT. EXIT.
TTTTTT
TTTTTT MONITORIT-LIST SECTION.
TTTTTT     IF MONITORIT-VARIABLE (NUB2) = TH2-LOW-VALUES
TTTTTT         GO MONITORIT-LIST-EXIT
TTTTTT     END-IF.
TTTTTT
TTTTTT     IF MONITORIT-SUB1 (NUB2) = SPACES
TTTTTT         DISPLAY MONITORIT-VARIABLE (NUB2)
TTTTTT         GO MONITORIT-LIST-EXIT
TTTTTT     END-IF.
TTTTTT
TTTTTT     IF MONITORIT-SUB2 (NUB2) = THIRTY-SPACES
TTTTTT         DISPLAY MONITORIT-VARIABLE (NUB2)
TTTTTT                ' WITH 1 SUBSCRIPT'
TTTTTT         GO MONITORIT-LIST-EXIT
TTTTTT     END-IF.
TTTTTT
TTTTTT     IF MONITORIT-SUB3 (NUB2) = THIRTY-SPACES
TTTTTT         DISPLAY MONITORIT-VARIABLE (NUB2)
TTTTTT                ' WITH 2 SUBSCRIPTS'
TTTTTT         GO MONITORIT-LIST-EXIT
TTTTTT     END-IF.
TTTTTT
TTTTTT     DISPLAY MONITORIT-VARIABLE (NUB2)
TTTTTT            ' WITH 3 SUBSCRIPTS'.
TTTTTT
TTTTTT MONITORIT-LIST-EXIT. EXIT.
TTTTTT
TTTTTT MATCHIT SECTION.
TTTTTT     MOVE 'N'                  TO MATCH-FOUND.
TTTTTT     PERFORM VARYING MZ-SUB2
TTTTTT        FROM 1 BY 1
TTTTTT       UNTIL MZ-SUB2 > MZ-SUB
TTTTTT          OR GLOB-NAME = MZ-NAME (MZ-SUB2)
TTTTTT     END-PERFORM.
TTTTTT     IF GLOB-NAME = MZ-NAME (MZ-SUB2)
TTTTTT         MOVE 'Y' TO MATCH-FOUND
TTTTTT     END-IF.
TTTTTT MATCHIT-EXIT. EXIT.
TTTTTT
TTTTTT DISPLAYABLE-LUMP SECTION.
TTTTTT     IF MZ-LEN (MZ-SUB2) < 32
TTTTTT         DISPLAY LUMP-BLOCK (GUB2)
TTTTTT     ELSE
TTTTTT         DISPLAY GUB6 ' ' LUMP-BLOCK (GUB2)
TTTTTT     END-IF.
TTTTTT     ADD 1                     TO GUB2.
TTTTTT     ADD 32                    TO GUB6.
TTTTTT DISPLAYABLE-LUMP-EXIT. EXIT.
TTTTTT
TTTTTT NONDISP-LUMP SECTION.
TTTTTT     MOVE LUMP-BLOCK (GUB3)    TO BLOC.
TTTTTT     MOVE 1                    TO GUB4.
TTTTTT     MOVE SPACES               TO BLOCG.
TTTTTT     MOVE SPACES               TO BLOCH.
TTTTTT     PERFORM EACH-ND-LUMP 32 TIMES.
TTTTTT     IF MZ-LEN (MZ-SUB2) < 32
TTTTTT         DISPLAY BLOCG
TTTTTT         DISPLAY BLOCH
TTTTTT     ELSE
TTTTTT         DISPLAY GUB6  ' ' BLOCG
TTTTTT         DISPLAY '       ' BLOCH
TTTTTT     END-IF.
TTTTTT     ADD 1                     TO GUB3.
TTTTTT     IF GUB3 > GUB
TTTTTT         GO TO NONDISP-LUMP-EXIT
TTTTTT     ELSE
TTTTTT         ADD 32                TO GUB6
TTTTTT         GO TO NONDISP-LUMP
TTTTTT     END-IF.
TTTTTT NONDISP-LUMP-EXIT. EXIT.
TTTTTT
TTTTTT EACH-ND-LUMP SECTION.
TTTTTT     IF GUB3 = GUB
TTTTTT         IF GUB4 > HUB
TTTTTT             ADD 1             TO GUB4
TTTTTT             GO TO EACH-ND-LUMP-EXIT
TTTTTT         END-IF
TTTTTT     END-IF.
TTTTTT     MOVE '. '                 TO BLOCG-BI (GUB4).
TTTTTT     IF (BLOC-C (GUB4) = SPACE)
TTTTTT         MOVE '  '             TO BLOCG-BI (GUB4)
TTTTTT         GO TO EACH-ND
TTTTTT     END-IF.
TTTTTT     IF (BLOC-C (GUB4) NOT < 'A' AND  BLOC-C (GUB4) NOT > 'Z')
TTTTTT         MOVE BLOC-C (GUB4)    TO BLOCG-BI (GUB4)
TTTTTT         GO TO EACH-ND
TTTTTT     END-IF.
TTTTTT     IF (BLOC-C (GUB4) NOT < 'a' AND BLOC-C (GUB4) NOT > 'z')
TTTTTT         MOVE BLOC-C (GUB4)    TO BLOCG-BI (GUB4)
TTTTTT         GO TO EACH-ND
TTTTTT     END-IF.
TTTTTT     IF (BLOC-C (GUB4) = '@' OR '$' OR '*' OR '&' OR '%' OR '+')
TTTTTT         MOVE BLOC-C (GUB4)    TO BLOCG-BI (GUB4)
TTTTTT         GO TO EACH-ND
TTTTTT     END-IF.
TTTTTT     IF (BLOC-C (GUB4) = X'4D')
TTTTTT         MOVE BLOC-C (GUB4)    TO BLOCG-BI (GUB4)
TTTTTT         GO TO EACH-ND
TTTTTT     END-IF.
TTTTTT     IF (BLOC-C (GUB4) = X'5D')
TTTTTT         MOVE BLOC-C (GUB4)    TO BLOCG-BI (GUB4)
TTTTTT         GO TO EACH-ND
TTTTTT     END-IF.
TTTTTT     IF (BLOC-C (GUB4) = X'6B')
TTTTTT         MOVE BLOC-C (GUB4)    TO BLOCG-BI (GUB4)
TTTTTT         GO TO EACH-ND
TTTTTT     END-IF.
TTTTTT     IF (BLOC-C (GUB4) = '-' OR '=' OR '{' OR '}' OR '[' OR ']')
TTTTTT         MOVE BLOC-C (GUB4)    TO BLOCG-BI (GUB4)
TTTTTT         GO TO EACH-ND
TTTTTT     END-IF.
TTTTTT     IF (BLOC-C (GUB4) = ':' OR '"' OR '<' OR '>' OR '?' OR '.')
TTTTTT         MOVE BLOC-C (GUB4)    TO BLOCG-BI (GUB4)
TTTTTT         GO TO EACH-ND
TTTTTT     END-IF.
TTTTTT     IF (BLOC-C (GUB4) = '/' OR '''')
TTTTTT         MOVE BLOC-C (GUB4)    TO BLOCG-BI (GUB4)
TTTTTT         GO TO EACH-ND
TTTTTT     END-IF.
TTTTT+     IF (BLOC-C (GUB4) NOT < '0' AND BLOC-C (GUB4) NOT > '9')
TTTTT+         MOVE BLOC-C (GUB4)    TO BLOCG-BI (GUB4)
TTTTT+         GO TO EACH-ND
TTTTT+     END-IF.
TTTTTT
TTTTTT EACH-ND.
TTTTTT     MOVE LOW-VALUES           TO THE-1ST-CHAR.
TTTTTT     MOVE BLOC-C (GUB4)        TO THE-2ND-CHAR.
TTTTTT     COMPUTE LHS = NINE-04-COMP / 16.
TTTTTT     COMPUTE RHS = NINE-04-COMP - (LHS * 16).
TTTTTT     ADD 1                     TO LHS.
TTTTTT     ADD 1                     TO RHS.
TTTTTT     MOVE THE-ARRAY-C (LHS)    TO BLOCH-BI-LHS (GUB4).
TTTTTT     MOVE THE-ARRAY-C (RHS)    TO BLOCH-BI-RHS (GUB4).
TTTTTT     ADD 1                     TO GUB4.
TTTTTT EACH-ND-LUMP-EXIT. EXIT.
TTTTTT
TTTTTT DISPLAYIT SECTION.
TTTTTT     IF (LUMP-C (LUMB) = SPACE)
TTTTTT         GO TO D-ADD
TTTTTT     END-IF.
TTTTTT     IF (LUMP-C (LUMB) NOT < 'A' AND LUMP-C (LUMB) NOT > 'Z')
TTTTTT         GO TO D-ADD
TTTTTT     END-IF.
TTTTTT     IF (LUMP-C (LUMB) NOT < 'a' AND LUMP-C (LUMB) NOT > 'z')
TTTTTT         GO TO D-ADD
TTTTTT     END-IF.
TTTTTT     IF (LUMP-C (LUMB) = '@' OR '$' OR '*' OR '&' OR '%' OR '+')
TTTTTT         GO TO D-ADD
TTTTTT     END-IF.
TTTTTT     IF (LUMP-C (LUMB) = X'4D')
TTTTTT         GO TO D-ADD
TTTTTT     END-IF.
TTTTTT     IF (LUMP-C (LUMB) = X'5D')
TTTTTT         GO TO D-ADD
TTTTTT     END-IF.
TTTTTT     IF (LUMP-C (LUMB) = X'6B')
TTTTTT         GO TO D-ADD
TTTTTT     END-IF.
TTTTTT     IF (LUMP-C (LUMB) = '-' OR '=' OR '{' OR '}' OR '[' OR ']')
TTTTTT         GO TO D-ADD
TTTTTT     END-IF.
TTTTTT     IF (LUMP-C (LUMB) = ':' OR '"' OR '<' OR '>' OR '?' OR '.')
TTTTTT         GO TO D-ADD
TTTTTT     END-IF.
TTTTTT     IF (LUMP-C (LUMB) = '/' OR '''')
TTTTTT         GO TO D-ADD
TTTTTT     END-IF.
TTTTT+     IF (LUMP-C (LUMB) NOT < '0' AND LUMP-C (LUMB) NOT > '9')
TTTTT+         GO TO D-ADD
TTTTT+     END-IF.
TTTTTT
TTTTTT     MOVE 'Y'                  TO NONDISPLAYABLE.
TTTTTT     GO TO DISPLAYIT-EXIT.
TTTTTT D-ADD.
TTTTTT     MOVE 'Y'                  TO DISPLAYABLE.
TTTTTT     GO TO DISPLAYIT-EXIT.
TTTTTT DISPLAYIT-EXIT. EXIT.
TTTTTT
TTTTTT READ-LINE-FILE SECTION.
TTTTTT     READ LINE-FILE
TTTTTT         AT END
TTTTTT             MOVE 'Y' TO LINE-FILE-ENDED.
TTTTTT     DISPLAY LINE-COUNT ' ' LINE-FILE-ENDED.
TTTTTT     IF LINE-FILE-ENDED = 'N'
TTTTTT         ADD 1                 TO LINE-COUNT
TTTTTT         IF LINE-COUNT > 999
TTTTTT             DISPLAY 'MAX OF 999 LINES EXCEEDED'
TTTTTT             DISPLAY 'BOMBING'
TTTTTT             STOP RUN
TTTTTT         END-IF
TTTTTT         MOVE LINE-RECORD      TO LINE-ENTRY (LINE-COUNT)
TTTTTT     END-IF.
TTTTTT READ-LINE-FILE-EXIT. EXIT.
TTTTTT
TTTTTT WORKIT SECTION.
TTTTTT     MOVE  1                   TO MUB4.
TTTTTT     DISPLAY '*----------------------------*'.
TTTTTT     PERFORM VARYING MZ-SUB2
TTTTTT        FROM 1 BY 1
TTTTTT       UNTIL MZ-SUB2 > MZ-SUB
TTTTTT          OR MUB4 > 15
TTTTTT           IF MZ-NAME (MZ-SUB2) (1:6) NOT = 'FILLER'
TTTTTT              DISPLAY '>>>>' MZ-NAME (MZ-SUB2)
TTTTTT              ADD 1 TO MUB4
TTTTTT           END-IF
TTTTTT     END-PERFORM.
TTTTTT     DISPLAY '*----------------------------*'.
TTTTTT WORKIT-EXIT. EXIT.
TTTTTT
TTTTTT WORKIT-PLUS SECTION.
TTTTTT     MOVE  1                   TO MUB4.
TTTTTT     DISPLAY '*----------------------------*'.
TTTTTT     PERFORM VARYING MZ-SUB2
TTTTTT        FROM MZ-SUB2 BY 1
TTTTTT       UNTIL MZ-SUB2 > MZ-SUB
TTTTTT          OR MUB4 > 15
TTTTTT           IF MZ-NAME (MZ-SUB2) (1:6) NOT = 'FILLER'
TTTTTT              DISPLAY '>>>>' MZ-NAME (MZ-SUB2)
TTTTTT              ADD 1 TO MUB4
TTTTTT           END-IF
TTTTTT     END-PERFORM.
TTTTTT     DISPLAY '*----------------------------*'.
TTTTTT WORKIT-PLUS-EXIT. EXIT.
TTTTTT
TTTTTT PROCIT SECTION.
TTTTTT     MOVE  1                    TO LUB.
TTTTTT     DISPLAY '*----------------------------*'.
TTTTTT     IF THE-COMMAND-3-8 NOT = SIX-SPACES
TTTTTT         PERFORM VARYING LUB2 FROM 1 BY 1
TTTTTT           UNTIL LUB2 > LINE-COUNT
TTTTTT              OR THE-COMMAND-3-8  = LINE-NUMBER (LUB2)
TTTTTT              OR THE-COMMAND-3-8  < LINE-NUMBER (LUB2)
TTTTTT         END-PERFORM
TTTTTT         IF LUB2 NOT > LINE-COUNT
TTTTTT             MOVE LUB2 TO LUB
TTTTTT         END-IF
TTTTTT     END-IF.
TTTTTT
TTTTTT     MOVE 'N'          TO PARA-FOUND.
TTTTTT     IF GLOB-NAME NOT = SPACES
TTTTTT         PERFORM VARYING LUB2 FROM 1 BY 1
TTTTTT           UNTIL LUB2 > LINE-COUNT
TTTTTT              OR PARA-FOUND = 'Y'
TTTTTT                 IF  LINE-NUMBER (LUB2) = SPACES
TTTTTT                 AND LINE-ASTER  (LUB2) = SPACES
TTTTTT                 AND LINE-30     (LUB2) NOT = SPACES
TTTTTT                   MOVE 0 TO LUB4
TTTTTT                   PERFORM VARYING LUB3
TTTTTT                      FROM 1 BY 1
TTTTTT                     UNTIL LUB3 > 30
TTTTTT                       MOVE LINE-30 (LUB2) (LUB3:1)
TTTTTT                         TO ONECH
TTTTTT                       IF ONECH = ' ' OR '.'
TTTTTT                         MOVE SPACES TO THIRTYCH
TTTTTT                         MOVE LINE-30 (LUB2)
TTTTTT                             (1:LUB4)
TTTTTT                           TO THIRTYCH
TTTTTT                         IF THIRTYCH = GLOB-NAME
TTTTTT                             MOVE 'Y' TO PARA-FOUND
TTTTTT                             MOVE 98  TO LUB3
TTTTTT                         END-IF
TTTTTT                       END-IF
TTTTTT                       ADD 1 TO LUB4
TTTTTT                   END-PERFORM
TTTTTT                 END-IF
TTTTTT         END-PERFORM
TTTTTT         IF PARA-FOUND = 'Y'
TTTTTT             SUBTRACT 1 FROM LUB2
TTTTTT             MOVE LUB2 TO LUB
TTTTTT         END-IF
TTTTTT     END-IF.
TTTTTT
TTTTTT     MOVE 1                     TO LUB2.
TTTTTT PROCIT-LOOP.
TTTTTT     DISPLAY LINE-ENTRY (LUB).
TTTTTT     IF LUB = LINE-COUNT
TTTTTT         GO TO PROCIT-LAST
TTTTTT     END-IF.
TTTTTT     ADD  1                     TO LUB.
TTTTTT     ADD  1                     TO LUB2.
TTTTTT     IF LUB2 > 15
TTTTTT         GO TO PROCIT-LAST
TTTTTT     END-IF.
TTTTTT     GO TO PROCIT-LOOP.
TTTTTT PROCIT-LAST.
TTTTTT     DISPLAY '*----------------------------*'.
TTTTTT PROCIT-EXIT. EXIT.
TTTTTT PROCIT-PLUS SECTION.
TTTTTT     MOVE 1                     TO LUB2.
TTTTTT     DISPLAY '*----------------------------*'.
TTTTTT PROCIT-PLUS-LOOP.
TTTTTT     DISPLAY LINE-ENTRY (LUB).
TTTTTT     IF LUB = LINE-COUNT
TTTTTT         GO TO PROCIT-PLUS-LAST
TTTTTT     END-IF.
TTTTTT     ADD  1                     TO LUB.
TTTTTT     ADD  1                     TO LUB2.
TTTTTT     IF LUB2 > 15
TTTTTT         GO TO PROCIT-PLUS-LAST
TTTTTT     END-IF.
TTTTTT     GO TO PROCIT-PLUS-LOOP.
TTTTTT PROCIT-PLUS-LAST.
TTTTTT     DISPLAY '*----------------------------*'.
TTTTTT PROCIT-PLUS-EXIT. EXIT.
TTTTTT
TTTTTT SCREENIT SECTION.
TTTTTT     MOVE  1                    TO SCUB.
TTTTTT SCREENIT-LOOP.
TTTTTT     MOVE LINE-ENTRY (LUB)      TO SCREEN-ENTRY (SCUB).
TTTTTT     IF SCREEN-NUMBER (SCUB) IS NUMERIC
TTTTTT         IF SCREEN-1ST-FLAG = 'Y'
TTTTTT             MOVE SCREEN-NUMBER (SCUB)
TTTTTT                                TO SCREEN-FIRST
TTTTTT             MOVE 'N'           TO SCREEN-1ST-FLAG
TTTTTT         END-IF
TTTTTT     END-IF.
TTTTTT     IF SCREEN-NUMBER (SCUB) IS NUMERIC
TTTTTT         MOVE SCREEN-NUMBER (SCUB)
TTTTTT                                TO SCREEN-LAST
TTTTTT     END-IF.
TTTTTT     IF LUB = LINE-COUNT
TTTTTT         GO TO SCREENIT-DISPLAY
TTTTTT     END-IF.
TTTTTT     ADD  1                     TO LUB.
TTTTTT     IF LUB > LUB-MAX
TTTTTT         ADD 15                 TO LUB-MAX
TTTTTT         GO TO SCREENIT-LAST
TTTTTT     END-IF.
TTTTTT     ADD  1                     TO SCUB.
TTTTTT     GO TO SCREENIT-LOOP.
TTTTTT SCREENIT-LAST.
TTTTTT     IF LINE-NUMBER (LINE-COUNT2) NOT LESS THAN SCREEN-FIRST
TTTTTT        IF LINE-NUMBER (LINE-COUNT2) NOT GREATER THAN SCREEN-LAST
TTTTTT            GO TO SCREENIT-DISPLAY
TTTTTT        ELSE
TTTTTT            MOVE 'Y'            TO SCREEN-1ST-FLAG
TTTTTT            MOVE ZEROES         TO SCREEN-FIRST
TTTTTT            MOVE ZEROES         TO SCREEN-LAST
TTTTTT            MOVE SPACES         TO SCREEN-ARRAY
TTTTTT            GO TO SCREENIT
TTTTTT        END-IF
TTTTTT     ELSE
TTTTTT        MOVE 'Y'                TO SCREEN-1ST-FLAG
TTTTTT        MOVE ZEROES             TO SCREEN-FIRST
TTTTTT        MOVE ZEROES             TO SCREEN-LAST
TTTTTT        MOVE SPACES             TO SCREEN-ARRAY
TTTTTT        GO TO SCREENIT
TTTTTT     END-IF.
TTTTTT SCREENIT-DISPLAY.
TTTTTT     MOVE 1                     TO SCUB2.
TTTTTT SCREEN-D-LOOP.
TTTTTT     MOVE LINE-NUMBER (LINE-COUNT2) TO LEFFED.
TTTTTT     MOVE SCREEN-NUMBER (SCUB2)     TO RITE.
TTTTTT     MOVE SPACE                 TO SCREEN-POINTER (SCUB2).
TTTTTT     IF LEFFED = RITE
TTTTTT         MOVE '>'               TO SCREEN-POINTER (SCUB2)
TTTTTT     END-IF.
TTTTTT     MOVE SCREEN-ENTRY (SCUB2)  TO SEVENT2.
TTTTTT     DISPLAY SEVENT2.
TTTTTT     ADD 1                      TO SCUB2.
TTTTTT     IF SCUB2 > SCUB
TTTTTT         IF SCUB2 < 16
TTTTTT             MOVE '*------- END OF PROGRAM - -----*'
TTTTTT                                TO SCREEN-ENTRY (SCUB2)
TTTTTT             GO TO SCREEN-D-LOOP
TTTTTT         ELSE
TTTTTT             GO TO SCREENIT-EXIT
TTTTTT         END-IF
TTTTTT     ELSE
TTTTTT         GO TO SCREEN-D-LOOP
TTTTTT     END-IF.
TTTTTT SCREENIT-EXIT. EXIT.
TTTTTT
TTTTTT FIND-LENGTH SECTION.
TTTTTT     PERFORM VARYING FUB FROM 1 BY 1
TTTTTT       UNTIL FUB > 30
TTTTTT          OR TEST-FIELD-C (FUB) = SPACES
TTTTTT     END-PERFORM.
TTTTTT     IF FUB > 30
TTTTTT         SUBTRACT 1 FROM FUB
TTTTTT     END-IF.
TTTTTT     IF TEST-FIELD-C (FUB) = SPACES
TTTTTT         SUBTRACT 1 FROM FUB
TTTTTT     END-IF.
TTTTTT FIND-LENGTH-EXIT. EXIT.
TTTTTT
TTTTTT LOWER-TO-UPPER SECTION.
TTTTTT     PERFORM VARYING THUB FROM 1 BY 1
TTTTTT       UNTIL THUB > 180
TTTTTT           IF THE-COMMAND-C (THUB) = 'a'
TTTTTT              MOVE 'A' TO THE-COMMAND-C (THUB)
TTTTTT           END-IF
TTTTTT           IF THE-COMMAND-C (THUB) = 'b'
TTTTTT              MOVE 'B' TO THE-COMMAND-C (THUB)
TTTTTT           END-IF
TTTTTT           IF THE-COMMAND-C (THUB) = 'c'
TTTTTT              MOVE 'C' TO THE-COMMAND-C (THUB)
TTTTTT           END-IF
TTTTTT           IF THE-COMMAND-C (THUB) = 'd'
TTTTTT              MOVE 'D' TO THE-COMMAND-C (THUB)
TTTTTT           END-IF
TTTTTT           IF THE-COMMAND-C (THUB) = 'e'
TTTTTT              MOVE 'E' TO THE-COMMAND-C (THUB)
TTTTTT           END-IF
TTTTTT           IF THE-COMMAND-C (THUB) = 'f'
TTTTTT              MOVE 'F' TO THE-COMMAND-C (THUB)
TTTTTT           END-IF
TTTTTT           IF THE-COMMAND-C (THUB) = 'g'
TTTTTT              MOVE 'G' TO THE-COMMAND-C (THUB)
TTTTTT           END-IF
TTTTTT           IF THE-COMMAND-C (THUB) = 'h'
TTTTTT              MOVE 'H' TO THE-COMMAND-C (THUB)
TTTTTT           END-IF
TTTTTT           IF THE-COMMAND-C (THUB) = 'i'
TTTTTT              MOVE 'I' TO THE-COMMAND-C (THUB)
TTTTTT           END-IF
TTTTTT           IF THE-COMMAND-C (THUB) = 'j'
TTTTTT              MOVE 'J' TO THE-COMMAND-C (THUB)
TTTTTT           END-IF
TTTTTT           IF THE-COMMAND-C (THUB) = 'k'
TTTTTT              MOVE 'K' TO THE-COMMAND-C (THUB)
TTTTTT           END-IF
TTTTTT           IF THE-COMMAND-C (THUB) = 'l'
TTTTTT              MOVE 'L' TO THE-COMMAND-C (THUB)
TTTTTT           END-IF
TTTTTT           IF THE-COMMAND-C (THUB) = 'm'
TTTTTT              MOVE 'M' TO THE-COMMAND-C (THUB)
TTTTTT           END-IF
TTTTTT           IF THE-COMMAND-C (THUB) = 'n'
TTTTTT              MOVE 'N' TO THE-COMMAND-C (THUB)
TTTTTT           END-IF
TTTTTT           IF THE-COMMAND-C (THUB) = 'o'
TTTTTT              MOVE 'O' TO THE-COMMAND-C (THUB)
TTTTTT           END-IF
TTTTTT           IF THE-COMMAND-C (THUB) = 'p'
TTTTTT              MOVE 'P' TO THE-COMMAND-C (THUB)
TTTTTT           END-IF
TTTTTT           IF THE-COMMAND-C (THUB) = 'q'
TTTTTT              MOVE 'Q' TO THE-COMMAND-C (THUB)
TTTTTT           END-IF
TTTTTT           IF THE-COMMAND-C (THUB) = 'r'
TTTTTT              MOVE 'R' TO THE-COMMAND-C (THUB)
TTTTTT           END-IF
TTTTTT           IF THE-COMMAND-C (THUB) = 's'
TTTTTT              MOVE 'S' TO THE-COMMAND-C (THUB)
TTTTTT           END-IF
TTTTTT           IF THE-COMMAND-C (THUB) = 't'
TTTTTT              MOVE 'T' TO THE-COMMAND-C (THUB)
TTTTTT           END-IF
TTTTTT           IF THE-COMMAND-C (THUB) = 'u'
TTTTTT              MOVE 'U' TO THE-COMMAND-C (THUB)
TTTTTT           END-IF
TTTTTT           IF THE-COMMAND-C (THUB) = 'v'
TTTTTT              MOVE 'V' TO THE-COMMAND-C (THUB)
TTTTTT           END-IF
TTTTTT           IF THE-COMMAND-C (THUB) = 'w'
TTTTTT              MOVE 'W' TO THE-COMMAND-C (THUB)
TTTTTT           END-IF
TTTTTT           IF THE-COMMAND-C (THUB) = 'x'
TTTTTT              MOVE 'X' TO THE-COMMAND-C (THUB)
TTTTTT           END-IF
TTTTTT           IF THE-COMMAND-C (THUB) = 'y'
TTTTTT              MOVE 'Y' TO THE-COMMAND-C (THUB)
TTTTTT           END-IF
TTTTTT           IF THE-COMMAND-C (THUB) = 'z'
TTTTTT              MOVE 'Z' TO THE-COMMAND-C (THUB)
TTTTTT           END-IF
TTTTTT     END-PERFORM.
TTTTTT LOWER-TO-UPPER-EXIT. EXIT.

TTTTTT*--1---------2--------3---------4---------5---------6---------7-*
TTTTTT SUBS-PROCESS SECTION.
TTTTTT     IF TEMP-NAME = 'COUNT                         '
TTTTTT         MOVE COUNT                         TO NINE06
TTTTTT     END-IF.
TTTTTT SUBS-PROCESS-EXIT. EXIT.
TTTTTT NAME9-PROCESS SECTION.
TTTTTT     IF GLOB-NAME = 'COUNT                         '
TTTTTT         MOVE 0         TO SUBS-NUM
TTTTTT         MOVE GLOB-NAME TO TEST-FIELD
TTTTTT                           FUB-NAME0
TTTTTT         PERFORM FIND-LENGTH
TTTTTT         MOVE FUB TO LEN0 KEEP-LEN0
TTTTTT
TTTTTT         IF  GLOB-SUB1 = SPACES
TTTTTT         AND GLOB-SUB2 = SPACES
TTTTTT         AND GLOB-SUB3 = SPACES
TTTTTT             CONTINUE
TTTTTT         ELSE
TTTTTT             DISPLAY GLOB-NAME
TTTTTT                   ' SHOULD NOT HAVE SUBSCRIPTS'
TTTTTT             GO TO NAME9-PROCESS-EXIT
TTTTTT         END-IF
TTTTTT
TTTTTT         IF  MONITORING = 'Y'
TTTTTT           IF DIZPLAY = 'Y'
TTTTTT            MOVE SPACES        TO MONITORIT-80
TTTTTT            MOVE 'MONITORING ' TO MONITORIT-80
TTTTTT            MOVE FUB-NAME0 (1:KEEP-LEN0)
TTTTTT                               TO MONITORIT-80 (12:KEEP-LEN0)
TTTTTT            ADD  12            TO LEN0
TTTTTT            MOVE ' AT LINE '   TO MONITORIT-80 (LEN0:9)
TTTTTT            ADD   9            TO LEN0
TTTTTT            MOVE LINE-NUMBER (LINE-COUNT2)
TTTTTT                               TO MONITORIT-80 (LEN0:6)
TTTTTT            DISPLAY MONITORIT-80 (1:72)
TTTTTT            DISPLAY  COUNT
TTTTTT           ELSE
TTTTTT            MOVE     COUNT
TTTTTT              TO LUMP
TTTTTT           END-IF
TTTTTT         ELSE
TTTTTT            DISPLAY FUB-NAME0
TTTTTT            DISPLAY  COUNT
TTTTTT         END-IF
TTTTTT     END-IF.
TTTTTT NAME9-PROCESS-EXIT. EXIT.
TTTTTT NAMEX-PROCESS SECTION.
TTTTTT     IF GLOB-NAME = 'IN-RECORD                     '
TTTTTT         MOVE 0         TO SUBS-NUM
TTTTTT         MOVE GLOB-NAME TO TEST-FIELD
TTTTTT                           FUB-NAME0
TTTTTT         PERFORM FIND-LENGTH
TTTTTT         MOVE FUB TO LEN0 KEEP-LEN0
TTTTTT
TTTTTT         IF  GLOB-SUB1 = SPACES
TTTTTT         AND GLOB-SUB2 = SPACES
TTTTTT         AND GLOB-SUB3 = SPACES
TTTTTT             MOVE 'Y' TO XOK
TTTTTT         ELSE
TTTTTT             DISPLAY GLOB-NAME
TTTTTT                   ' SHOULD NOT HAVE SUBSCRIPTS'
TTTTTT             MOVE 'N' TO XOK
TTTTTT         END-IF
TTTTTT         IF XOK = 'Y'
TTTTTT         IF  MONITORING = 'Y'
TTTTTT           IF DIZPLAY = 'Y'
TTTTTT            MOVE SPACES        TO MONITORIT-80
TTTTTT            MOVE 'MONITORING ' TO MONITORIT-80
TTTTTT            MOVE FUB-NAME0 (1:KEEP-LEN0)
TTTTTT                               TO MONITORIT-80 (12:LEN0)
TTTTTT            ADD  12            TO LEN0
TTTTTT            MOVE ' AT LINE '   TO MONITORIT-80 (LEN0:9)
TTTTTT            ADD   9            TO LEN0
TTTTTT            MOVE LINE-NUMBER (LINE-COUNT2)
TTTTTT                               TO MONITORIT-80 (LEN0:6)
TTTTTT            DISPLAY MONITORIT-80 (1:72)
TTTTTT            MOVE     IN-RECORD
TTTTTT             TO LUMP
TTTTTT           ELSE
TTTTTT            MOVE     IN-RECORD
TTTTTT             TO LUMP
TTTTTT           END-IF
TTTTTT         ELSE
TTTTTT            DISPLAY FUB-NAME0
TTTTTT            MOVE     IN-RECORD
TTTTTT              TO LUMP
TTTTTT         END-IF
TTTTTT         END-IF
TTTTTT     END-IF.
TTTTTT     IF GLOB-NAME = 'OUT-RECORD                    '
TTTTTT         MOVE 0         TO SUBS-NUM
TTTTTT         MOVE GLOB-NAME TO TEST-FIELD
TTTTTT                           FUB-NAME0
TTTTTT         PERFORM FIND-LENGTH
TTTTTT         MOVE FUB TO LEN0 KEEP-LEN0
TTTTTT
TTTTTT         IF  GLOB-SUB1 = SPACES
TTTTTT         AND GLOB-SUB2 = SPACES
TTTTTT         AND GLOB-SUB3 = SPACES
TTTTTT             MOVE 'Y' TO XOK
TTTTTT         ELSE
TTTTTT             DISPLAY GLOB-NAME
TTTTTT                   ' SHOULD NOT HAVE SUBSCRIPTS'
TTTTTT             MOVE 'N' TO XOK
TTTTTT         END-IF
TTTTTT         IF XOK = 'Y'
TTTTTT         IF  MONITORING = 'Y'
TTTTTT           IF DIZPLAY = 'Y'
TTTTTT            MOVE SPACES        TO MONITORIT-80
TTTTTT            MOVE 'MONITORING ' TO MONITORIT-80
TTTTTT            MOVE FUB-NAME0 (1:KEEP-LEN0)
TTTTTT                               TO MONITORIT-80 (12:LEN0)
TTTTTT            ADD  12            TO LEN0
TTTTTT            MOVE ' AT LINE '   TO MONITORIT-80 (LEN0:9)
TTTTTT            ADD   9            TO LEN0
TTTTTT            MOVE LINE-NUMBER (LINE-COUNT2)
TTTTTT                               TO MONITORIT-80 (LEN0:6)
TTTTTT            DISPLAY MONITORIT-80 (1:72)
TTTTTT            MOVE     OUT-RECORD
TTTTTT             TO LUMP
TTTTTT           ELSE
TTTTTT            MOVE     OUT-RECORD
TTTTTT             TO LUMP
TTTTTT           END-IF
TTTTTT         ELSE
TTTTTT            DISPLAY FUB-NAME0
TTTTTT            MOVE     OUT-RECORD
TTTTTT              TO LUMP
TTTTTT         END-IF
TTTTTT         END-IF
TTTTTT     END-IF.
TTTTTT     IF GLOB-NAME = 'WS-LINE                       '
TTTTTT         MOVE 0         TO SUBS-NUM
TTTTTT         MOVE GLOB-NAME TO TEST-FIELD
TTTTTT                           FUB-NAME0
TTTTTT         PERFORM FIND-LENGTH
TTTTTT         MOVE FUB TO LEN0 KEEP-LEN0
TTTTTT
TTTTTT         IF  GLOB-SUB1 = SPACES
TTTTTT         AND GLOB-SUB2 = SPACES
TTTTTT         AND GLOB-SUB3 = SPACES
TTTTTT             MOVE 'Y' TO XOK
TTTTTT         ELSE
TTTTTT             DISPLAY GLOB-NAME
TTTTTT                   ' SHOULD NOT HAVE SUBSCRIPTS'
TTTTTT             MOVE 'N' TO XOK
TTTTTT         END-IF
TTTTTT         IF XOK = 'Y'
TTTTTT         IF  MONITORING = 'Y'
TTTTTT           IF DIZPLAY = 'Y'
TTTTTT            MOVE SPACES        TO MONITORIT-80
TTTTTT            MOVE 'MONITORING ' TO MONITORIT-80
TTTTTT            MOVE FUB-NAME0 (1:KEEP-LEN0)
TTTTTT                               TO MONITORIT-80 (12:LEN0)
TTTTTT            ADD  12            TO LEN0
TTTTTT            MOVE ' AT LINE '   TO MONITORIT-80 (LEN0:9)
TTTTTT            ADD   9            TO LEN0
TTTTTT            MOVE LINE-NUMBER (LINE-COUNT2)
TTTTTT                               TO MONITORIT-80 (LEN0:6)
TTTTTT            DISPLAY MONITORIT-80 (1:72)
TTTTTT            MOVE     WS-LINE
TTTTTT             TO LUMP
TTTTTT           ELSE
TTTTTT            MOVE     WS-LINE
TTTTTT             TO LUMP
TTTTTT           END-IF
TTTTTT         ELSE
TTTTTT            DISPLAY FUB-NAME0
TTTTTT            MOVE     WS-LINE
TTTTTT              TO LUMP
TTTTTT         END-IF
TTTTTT         END-IF
TTTTTT     END-IF.
TTTTTT     IF GLOB-NAME = 'END-OF-FILE-FLAG              '
TTTTTT         MOVE 0         TO SUBS-NUM
TTTTTT         MOVE GLOB-NAME TO TEST-FIELD
TTTTTT                           FUB-NAME0
TTTTTT         PERFORM FIND-LENGTH
TTTTTT         MOVE FUB TO LEN0 KEEP-LEN0
TTTTTT
TTTTTT         IF  GLOB-SUB1 = SPACES
TTTTTT         AND GLOB-SUB2 = SPACES
TTTTTT         AND GLOB-SUB3 = SPACES
TTTTTT             MOVE 'Y' TO XOK
TTTTTT         ELSE
TTTTTT             DISPLAY GLOB-NAME
TTTTTT                   ' SHOULD NOT HAVE SUBSCRIPTS'
TTTTTT             MOVE 'N' TO XOK
TTTTTT         END-IF
TTTTTT         IF XOK = 'Y'
TTTTTT         IF  MONITORING = 'Y'
TTTTTT           IF DIZPLAY = 'Y'
TTTTTT            MOVE SPACES        TO MONITORIT-80
TTTTTT            MOVE 'MONITORING ' TO MONITORIT-80
TTTTTT            MOVE FUB-NAME0 (1:KEEP-LEN0)
TTTTTT                               TO MONITORIT-80 (12:LEN0)
TTTTTT            ADD  12            TO LEN0
TTTTTT            MOVE ' AT LINE '   TO MONITORIT-80 (LEN0:9)
TTTTTT            ADD   9            TO LEN0
TTTTTT            MOVE LINE-NUMBER (LINE-COUNT2)
TTTTTT                               TO MONITORIT-80 (LEN0:6)
TTTTTT            DISPLAY MONITORIT-80 (1:72)
TTTTTT            MOVE     END-OF-FILE-FLAG
TTTTTT             TO LUMP
TTTTTT           ELSE
TTTTTT            MOVE     END-OF-FILE-FLAG
TTTTTT             TO LUMP
TTTTTT           END-IF
TTTTTT         ELSE
TTTTTT            DISPLAY FUB-NAME0
TTTTTT            MOVE     END-OF-FILE-FLAG
TTTTTT              TO LUMP
TTTTTT         END-IF
TTTTTT         END-IF
TTTTTT     END-IF.
TTTTTT NAMEX-PROCESS-EXIT. EXIT.
