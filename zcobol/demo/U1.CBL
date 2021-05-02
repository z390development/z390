       IDENTIFICATION DIVISION.
       PROGRAM-ID.  U.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT IN-FILE
              ASSIGN TO 'ZCOBOL\DEMO\TESTMOV1.CBL'
               ORGANIZATION IS LINE SEQUENTIAL.
U5U5U5     SELECT PRN-FILE
              ASSIGN TO 'ZCOBOL\DEMO\TESTMOV1.PRN'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUT-FILE
               ASSIGN TO 'ZCOBOL\DEMO\UPROGRAM.CBL'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT TCBL-FILE
               ASSIGN TO 'ZCOBOL\DEMO\U2.CBL'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUTLINE-FILE
               ASSIGN TO 'ZCOBOL\DEMO\UPROGRAM.LIN'
               ORGANIZATION IS LINE SEQUENTIAL.

U7U7U7     SELECT U6-FILE
              ASSIGN TO 'ZCOBOL\DEMO\U6.CBL'
               ORGANIZATION IS LINE SEQUENTIAL.
U7U7U7     SELECT U6B-FILE
              ASSIGN TO 'ZCOBOL\DEMO\U6B.CBL'
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT MZ-ARRAY-FILE
               ASSIGN TO 'ZCOBOL\DEMO\MZARRAY.CBL'
               ORGANIZATION IS LINE SEQUENTIAL.
         
       DATA DIVISION.
       FILE SECTION.
       FD  IN-FILE
           DATA RECORD IS IN-RECORD.
       01  IN-RECORD.
           03  FILLER               PIC X(06).
           03  IN-7                 PIC X(01).
           03  FILLER               PIC X(73).

U5U5U5 FD  PRN-FILE
           DATA RECORD IS PRN-REC.
       01  PRN-REC                  PIC X(1024).

       FD  OUT-FILE
           DATA RECORD IS OUT-RECORD.
       01  OUT-RECORD               PIC X(80).
       FD  TCBL-FILE
           DATA RECORD IS TCBL-RECORD.
       01  TCBL-RECORD.
           03  TCBL-1-6             PIC X(06).
           03  FILLER               PIC X(74).
       FD  OUTLINE-FILE
           DATA RECORD IS OUTLINE-RECORD.
       01  OUTLINE-RECORD.
           03  OUTLINE-LINE-NO      PIC 9(06).
           03  FILLER               PIC X(74).

U7U7U7 FD  U6-FILE
           DATA RECORD IS U6-RECORD.
       01  U6-RECORD                PIC X(80).
U7U7U7 FD  U6B-FILE
           DATA RECORD IS U6B-RECORD.
       01  U6B-RECORD               PIC X(80).

       FD  MZ-ARRAY-FILE
           DATA RECORD IS MZ-ARRAY-RECORD.
       01  MZ-ARRAY-RECORD          PIC X(80).
       WORKING-STORAGE SECTION.
       01  ITS-CLOSED               PIC X(01) VALUE 'N'.
       01  FURST-THYME              PIC X(01) VALUE 'Y'.
140312 01  MY-SUB                   PIC 9(02).
       01  THINGY                   PIC X(01).
       01  WS-LINE.
           03  WS-1-7.
               05  WS-1-6           PIC 9(06).
               05  FILLER           PIC X(01).
           03  WS-8-30.
               05  WS-8-26.
                   07  WS-8-22.
                       09  WS-8-18  PIC X(11).
                       09  FILLER   PIC X(04).
                   07  FILLER       PIC X(04).
               05  FILLER           PIC X(04).
           03  FILLER               PIC X(50).
       01  FILLER REDEFINES WS-LINE.
           03  WSL-C OCCURS 80      PIC X(01).
       01  LUB                      PIC 9(02).
       01  LUB2                     PIC 9(02).
       01  PROGRAM-ID-LINE.
           03  PIL-C OCCURS 80      PIC X(01).
       01  PUB                      PIC 9(02).
       01  PUB2                     PIC 9(02).
       01  THE-PROGRAM-ID           PIC X(08).
       01  PARA-LINE.
           03  PAL-C OCCURS 80      PIC X(01).
       01  THE-PARA                 PIC X(80).
       01  VERB-LINE.
           03  VEL-C OCCURS 80      PIC X(01).
       01  THE-VERB                 PIC X(80).
       88  ITS-A-VERB               VALUES 'OPEN', 'CLOSE',            -
            'DISPLAY', 'ACCEPT', 'CALL',                               -
            'COMPUTE', 'ADD', 'MULTIPLY', 'DIVIDE',                    -
            'SUBTRACT', 'MOVE', 'IF', 'PERFORM',                       -
            'STOP', 'GO', 'READ', 'WRITE',                             -
            'CONTINUE', 'EVALUATE', 'EXAMINE', 'EXEC', 'GOBACK',       -
            'INSPECT', 'MERGE', 'RELEASE', 'REWRITE', 'SEARCH',        -
            'SET', 'SORT', 'START' 'TRANSFORM'.
       01  WS-ARRAY.
           03  WS-ENTRY OCCURS 999.
               05  WSE-C OCCURS 80  PIC X(01).
       01  WUB                      PIC 9(03) VALUE  0.
       01  WUB2                     PIC 9(03).
130312 01  WUB3                     PIC 9(03).
       01  EUB                      PIC 9(02).
       01  EUB2                     PIC 9(02).
130312 01  EUB3                     PIC 9(02).
130312 01  EUB4                     PIC 9(02).
130312 01  EUB5                     PIC 9(02).
130312 01  EUB6                     PIC 9(02).
130312 01  EUB7                     PIC 9(02).
130312 01  PIC-FOUND                PIC X(01).
130312 01  EIGHTY-EIGHT-FOUND       PIC X(01).
130312 01  OH1-77-FOUND             PIC X(01).
       01  B4-VARIABLE              PIC X(39) 
           VALUE 'TTTTTT 01  FILLER PIC X(02) VALUE ''!!''.'.
       01  VARIABLE-LINE.
           03  FILLER               PIC X(35) 
               VALUE 'TTTTTT 01  FILLER PIC X(30) VALUE '''.
           03  VARIABLE-NAME.
               05  VN-C OCCURS 30   PIC X(01).
           03  FILLER               PIC X(02) VALUE '''.'.
       01  AFTA-VARIABLE            PIC X(39) 
           VALUE 'TTTTTT 01  FILLER PIC X(02) VALUE ''~~''.'.
       01  VUB                      PIC 9(02).
       01  VUB2                     PIC 9(02).
       01  SKIPIT                   PIC X(01).
       01  END-OF-FILE-FLAG         PIC X(01) VALUE 'F'.
       01  END-OF-TCBL-FLAG         PIC X(01) VALUE 'F'.
       01  LINE-NO                  PIC 9(06) VALUE  0.
       01  THE-LINE                 PIC 9(06) VALUE  0.
       01  PROCEDURE-DIVISION-FOUND PIC X(01) VALUE 'N'.
       01  WORKING-STORAGE-SECTION-FOUND 
                                    PIC X(01) VALUE 'N'.
       01  LINKAGE-SECTION-FOUND    PIC X(01) VALUE 'N'.
       01  INPUT-OUTPUT-SECTION-FOUND 
                                    PIC X(01) VALUE 'N'.
       01  FILE-CONTROL-FOUND       PIC X(01) VALUE 'N'.
       01  FILE-SECTION-FOUND       PIC X(01) VALUE 'N'.
       01  IOS                      PIC X(80) VALUE 
           '       INPUT-OUTPUT SECTION.'.
       01  FC                       PIC X(80) VALUE  
           '       FILE-CONTROL.'.
       01  FS                       PIC X(80) VALUE  
           '       FILE SECTION.'.
                    
       01  VERB-MOVES.
           03  VM-1.
               05  FILLER           PIC X(17) VALUE
              'TTTTTT     MOVE '''.      
               05  VM-1-PROG        PIC X(08).
               05  FILLER           PIC X(11) VALUE
              ''' TO T-PROG'.
           03  VM-2.
               05  FILLER           PIC X(17) VALUE
              'TTTTTT     MOVE '''.      
               05  VM-2-PARA        PIC X(30).
               05  FILLER           PIC X(11) VALUE
              ''' TO T-PARA'. 
           03  VM-3.
               05  FILLER           PIC X(17) VALUE
              'TTTTTT     MOVE '''.      
               05  VM-3-LINE        PIC 9(06).
               05  FILLER           PIC X(11) VALUE
              ''' TO T-LINE'. 
           03  VM-4.
               05  FILLER           PIC X(21) VALUE
              'TTTTTT     PERFORM T'.    



U5U5U5 01  FU5ST-THYME              PIC X(01) VALUE 'Y'.
       01  END-OF-PRN-FLAG          PIC X(01) VALUE 'F'.
       01  U5-WS-FOUND              PIC X(01) VALUE 'N'.
       01  U5-LS-FOUND              PIC X(01) VALUE 'N'.
       01  PRN-RECORD               PIC X(1024).
       01  MY-ARRAY.
           03  MY-ENTRY OCCURS 999.
               05  MY-LVL           PIC X(02).
               05  MY-NAME          PIC X(30).
               05  MY-ADDR          PIC X(08).
               05  MY-LEN           PIC X(08).
               05  MY-DUP           PIC X(06).
               05  MY-TYPE          PIC X(01).
               05  MY-PIC           PIC X(10).
       01  MY-MAX                   PIC 9(03) VALUE 999.
       01  MY-SU5                   PIC 9(03) VALUE 0.
       01  MY-SUB2                  PIC 9(03).
       01  MY-SUB3                  PIC 9(03).
       01  ONECH                    PIC X(01).
       01  SIXCH                    PIC X(06).
       01  FIVECH                   PIC X(05).
       01  EIGHTCH                  PIC X(08).
       01  MULTIP                   PIC 9(09).
       01  MZ-ARRAY.
           03  MZ-ENTRY OCCURS 999.
               05  MZ-LVL           PIC X(02).
               05  MZ-NAME          PIC X(30).
               05  MZ-ADDR          PIC 9(09).
               05  MZ-LEN           PIC 9(09).
               05  MZ-DUP           PIC X(06).
               05  MZ-SUBS          PIC 9(01).
               05  MZ-TYPE          PIC X(01).
               05  MZ-PIC           PIC X(10).
               05  MZ-SUBS-IDS OCCURS 3.
                   07  MZ-SUBS-ID   PIC X(30).
       01  MZ-MAX                   PIC 9(03) VALUE 999.
       01  MZ-SUB                   PIC 9(03) VALUE 0.
       01  MZ-SUB2                  PIC 9(03).
       01  MZ-SUB3                  PIC 9(03).
       01  ADDR9                    PIC 9(09).
       01  LEN9                     PIC 9(09).
       01  SUBS-COUNT               PIC 9(01).
       01  INTERMED                 PIC 9(09).
       01  TENCH                    PIC X(10).
       01  THIRTYCH                 PIC X(30).

       01  SUBS-PROCESS-SECTION     PIC X(80) 
               VALUE 'TTTTTT SUBS-PROCESS SECTION.'.
       01  SUBS-IF-GLOB.
           03  FILLER               PIC X(27)
               VALUE 'TTTTTT     IF TEMP-NAME = '''.
           03  SUBS-IF-GLOB-NAME    PIC X(30).
           03  FILLER               PIC X(23)
               VALUE ''''.
       01  SUBS-MOVE-NAME.
           03  FILLER               PIC X(20)
               VALUE 'TTTTTT         MOVE '.
           03  SUBS-MOVE-NAME-NAME  PIC X(30).
           03  FILLER               PIC X(09)
               VALUE 'TO NINE06'.
       01  END-IF-STMT              PIC X(18)
               VALUE 'TTTTTT     END-IF.'.
       01  SUBS-PROCESS-EXIT        PIC X(80)
               VALUE 'TTTTTT SUBS-PROCESS-EXIT. EXIT.'.

       01  NAME9-PROCESS-SECTION    PIC X(80) 
               VALUE 'TTTTTT NAME9-PROCESS SECTION.'.
       01  NAME9-PROCESS-EXIT       PIC X(80)
               VALUE 'TTTTTT NAME9-PROCESS-EXIT. EXIT.'.
       01  NAMEX-PROCESS-SECTION    PIC X(80) 
               VALUE 'TTTTTT NAMEX-PROCESS SECTION.'.
       01  NAMEX-PROCESS-EXIT       PIC X(80)
               VALUE 'TTTTTT NAMEX-PROCESS-EXIT. EXIT.'.

U7U7U7 01  WS-U6.
           03  WS-U6-1              PIC X(01).
           03  FILLER               PIC X(79).
       01  WS-U8.
           03  WS-U8-1-FOR-6        PIC X(06).
           03  FILLER               PIC X(21).
           03  WS-U8-28-FOR-30      PIC X(30).
           03  FILLER               PIC X(23).
       01  U8-LINE.
           03  U8-1-FOR-6           PIC X(06).
           03  FILLER               PIC X(12).
           03  U8-VERB              PIC X(07).
           03  FILLER               PIC X(01).
           03  U8-NAME              PIC X(30).
           03  FILLER               PIC X(01).
           03  U8-OPEN              PIC X(01).
           03  FILLER               PIC X(01).
           03  U8-SUB1              PIC X(30).
           03  FILLER               PIC X(01).
           03  U8-COMMA1            PIC X(01).
           03  FILLER               PIC X(01).
           03  U8-SUB2              PIC X(30).
           03  FILLER               PIC X(01).
           03  U8-COMMA2            PIC X(01).
           03  FILLER               PIC X(01).
           03  U8-SUB3              PIC X(30).
           03  FILLER               PIC X(01).
           03  U8-CLOSE             PIC X(01).
       01  U8-LINE2                 PIC X(80).
       01  OSUB1                    PIC 9(03).
       01  OSUB2                    PIC 9(03).
       01  END-OF-U6-FLAG           PIC X(01).
       01  U7-SUB                   PIC 9(03).
       01  GLOB-NAME                PIC X(30).
       01  GLOB-SUB1                PIC X(30).
       01  GLOB-SUB2                PIC X(30).
       01  GLOB-SUB3                PIC X(30).
       01  TEMP-SUB1                PIC 9(02).
       01  TEMP-SUB2                PIC 9(02).
       01  TEMP-SUB3                PIC 9(02).
       01  DISP-SUB1                PIC 9(02).
       01  DISP-SUB2                PIC 9(02).
       01  DISP-SUB3                PIC 9(02).
       01  NINE06                   PIC 9(06).
       01  GLOB-SUBS                PIC 9(01).
       01  TEMP                     PIC 9(02).
/                  
       PROCEDURE DIVISION.
       MAINLINE.
      *DISPLAY 'U STARTED'.
           PERFORM INITIALIZATION-ROUTINE.
           PERFORM THE-LOOP UNTIL END-OF-FILE-FLAG = 'Y'.
           PERFORM WRITE-AND-READ-CBL
             UNTIL END-OF-TCBL-FLAG = 'Y'. 
           PERFORM EOJ-ROUTINE.
           PERFORM U5.
           PERFORM U7.
           OPEN OUTPUT MZ-ARRAY-FILE.
           PERFORM VARYING MZ-SUB2
              FROM 1 BY 1
             UNTIL MZ-SUB2 > MZ-SUB
               IF  MZ-LVL  (MZ-SUB2) NOT = '00'
               AND MZ-TYPE (MZ-SUB2) NOT = 'A'
               AND MZ-LVL  (MZ-SUB2) NOT = '91'
                   WRITE MZ-ARRAY-RECORD FROM MZ-ENTRY (MZ-SUB2)
               END-IF
           END-PERFORM.
           CLOSE MZ-ARRAY-FILE.
      *DISPLAY 'U ENDED OK'
           STOP RUN.

       INITIALIZATION-ROUTINE SECTION.
           OPEN INPUT  IN-FILE.
           OPEN OUTPUT OUT-FILE.
           OPEN OUTPUT OUTLINE-FILE.
           OPEN INPUT  TCBL-FILE.
           PERFORM READ-INFILE.
           PERFORM READ-TCBL. 
           PERFORM READ-TCBL.
       INITIALIZATION-EXIT. EXIT.

       THE-LOOP SECTION.

           IF WS-8-18 = 'PROGRAM-ID.'
               PERFORM VARYING LUB
                  FROM 20  BY 1 
                 UNTIL LUB > 72
                    OR WSL-C (LUB) NOT = SPACE
               END-PERFORM
               PERFORM VARYING LUB2
                  FROM LUB BY 1 
                 UNTIL LUB2 > 72
                    OR WSL-C (LUB2) = '.'
               END-PERFORM
               MOVE SPACES          TO PROGRAM-ID-LINE
               MOVE 0               TO PUB
               PERFORM MOVEIT VARYING LUB
                  FROM LUB BY 1 
                 UNTIL LUB = LUB2
               MOVE PROGRAM-ID-LINE TO THE-PROGRAM-ID
           END-IF.

           MOVE 'N'                 TO SKIPIT.

           IF WS-8-26 = 'INPUT-OUTPUT SECTIO'
               MOVE 'Y'             TO INPUT-OUTPUT-SECTION-FOUND
           END-IF.

           IF WS-8-26 = 'FILE CONTROL.      '
               MOVE 'Y'             TO FILE-CONTROL-FOUND
           END-IF.

           IF WS-8-26 = 'FILE SECTION.      '
               MOVE 'Y'             TO FILE-SECTION-FOUND
           END-IF.

           IF  WS-8-26 = 'DATA DIVISION.     '
               IF INPUT-OUTPUT-SECTION-FOUND = 'N'
                   IF FILE-CONTROL-FOUND = 'N'
                       WRITE OUT-RECORD FROM IOS
                       WRITE OUT-RECORD FROM FC
                       PERFORM WRITE-AND-READ-CBL
                         UNTIL TCBL-1-6 = '------' 
                       PERFORM READ-TCBL
                   END-IF
               END-IF    
           END-IF.

           IF WS-8-26 = 'FILE-CONTROL.      '
               PERFORM WRITE-AND-READ-CBL
                 UNTIL TCBL-1-6 = '------' 
               PERFORM READ-TCBL
               MOVE 'Y'              TO SKIPIT
           END-IF.

           IF  WS-8-26 = 'WORKING-STORAGE SEC'
               IF FILE-SECTION-FOUND = 'N'
                   WRITE OUT-RECORD FROM FS
                   PERFORM WRITE-AND-READ-CBL
                       UNTIL TCBL-1-6 = '------' 
                   PERFORM READ-TCBL
               END-IF
           END-IF.

           IF WS-8-26 = 'FILE SECTION.      '
               PERFORM WRITE-AND-READ-CBL
                   UNTIL TCBL-1-6 = '------' 
               PERFORM READ-TCBL
               MOVE 'Y'              TO SKIPIT
           END-IF.

           IF WS-8-26 = 'PROCEDURE DIVISION.'
               MOVE 'Y'             TO PROCEDURE-DIVISION-FOUND
           END-IF.

           IF WS-8-22 = 'LINKAGE SECTION'
               MOVE 'Y'             TO LINKAGE-SECTION-FOUND
           END-IF.

           IF WORKING-STORAGE-SECTION-FOUND        = 'Y'
               IF LINKAGE-SECTION-FOUND            = 'N'
                  IF PROCEDURE-DIVISION-FOUND      = 'N'
      *DISPLAY 'U1 - WS/LS/PD=YNN'
                     WRITE OUT-RECORD FROM IN-RECORD
                     PERFORM READ-INFILE
                     GO THE-LOOP-EXIT
                  END-IF
               END-IF
           END-IF.

           IF WORKING-STORAGE-SECTION-FOUND        = 'Y'
              IF LINKAGE-SECTION-FOUND             = 'Y'
                 IF FU5ST-THYME                    = 'Y'
      *DISPLAY 'U1 - WS/LS/FT=YYY'
                     MOVE 'N'       TO FU5ST-THYME
                     PERFORM WRITE-AND-READ-CBL
                         UNTIL TCBL-1-6 = '------' 
                     PERFORM READ-TCBL
                     PERFORM WRITE-AND-READ-CBL
                         UNTIL TCBL-1-6 = '------' 
                     PERFORM READ-TCBL
                     GO THE-LOOP-EXIT
                 END-IF
              END-IF
           END-IF.
     
           IF WORKING-STORAGE-SECTION-FOUND        = 'Y'
              IF PROCEDURE-DIVISION-FOUND          = 'Y'
                 IF FU5ST-THYME                    = 'Y'
      *DISPLAY 'U1 - WS/PD/FT=YYY'
                     MOVE 'N'       TO FU5ST-THYME
                     PERFORM WRITE-AND-READ-CBL
                         UNTIL TCBL-1-6 = '------' 
                     PERFORM READ-TCBL
                     PERFORM WRITE-AND-READ-CBL
                         UNTIL TCBL-1-6 = '------' 
                     PERFORM READ-TCBL
                     GO THE-LOOP-EXIT
                 END-IF
              END-IF
           END-IF.
                                  
           IF WS-8-30 = 'WORKING-STORAGE SECTION'
               MOVE 'Y'             TO WORKING-STORAGE-SECTION-FOUND
           END-IF.

           IF PROCEDURE-DIVISION-FOUND = 'Y'
              IF WSL-C (08) NOT = SPACE
                  PERFORM VARYING LUB
                     FROM 8 BY 1 
                    UNTIL LUB > 72
                       OR (WSL-C (LUB) = SPACE OR '.')
                  END-PERFORM
                  MOVE SPACES          TO PARA-LINE
                  MOVE 0               TO PUB
                  PERFORM MOVEIT3 VARYING LUB2
                     FROM 8 BY 1 
                    UNTIL LUB2 = LUB
                  MOVE PARA-LINE       TO THE-PARA
              ELSE               
                  PERFORM VARYING LUB
                     FROM 11  BY 1 
                    UNTIL LUB > 72
                       OR WSL-C (LUB) NOT = SPACE
                  END-PERFORM
                  PERFORM VARYING LUB2
                     FROM LUB BY 1 
                    UNTIL LUB2 > 72
                       OR WSL-C (LUB2) = SPACE
                  END-PERFORM 
                  MOVE SPACES          TO VERB-LINE
                  MOVE 0               TO PUB
                  PERFORM MOVEIT4 VARYING LUB
                     FROM LUB BY 1 
                    UNTIL LUB = LUB2
                  MOVE VERB-LINE       TO THE-VERB
                  IF ITS-A-VERB
      *DISPLAY 'U1 - VERB FOUND' 
                      ADD  1           TO THE-LINE
                      MOVE THE-PROGRAM-ID TO VM-1-PROG
                      MOVE THE-PARA    TO VM-2-PARA
                      MOVE THE-LINE    TO VM-3-LINE
                      WRITE OUT-RECORD FROM VM-1
                      WRITE OUT-RECORD FROM VM-2
                      WRITE OUT-RECORD FROM VM-3
                      WRITE OUT-RECORD FROM VM-4
                      MOVE THE-LINE    TO WS-1-6
                  END-IF
               END-IF
           END-IF.
           IF SKIPIT = 'N'
      *DISPLAY 'U1 - SKIP=N'            
 
               WRITE OUT-RECORD       FROM WS-LINE
               IF WS-1-6 NOT = 'TTTTTT'
                   IF PROCEDURE-DIVISION-FOUND = 'Y'
                       WRITE OUTLINE-RECORD FROM WS-LINE
                   END-IF
               END-IF
           END-IF.
           PERFORM READ-INFILE.
       THE-LOOP-EXIT. EXIT.
      
       EOJ-ROUTINE SECTION.
           CLOSE IN-FILE.
           CLOSE OUTLINE-FILE.
           CLOSE TCBL-FILE.
       EOJ-EXIT. EXIT.

       MOVEIT SECTION.
           ADD 1                    TO PUB.
           MOVE WSL-C (LUB)         TO PIL-C (PUB).
       MOVEIT-EXIT. EXIT.

       MOVEIT3 SECTION.
           ADD 1                    TO PUB.
           MOVE WSL-C (LUB2)        TO PAL-C (PUB).
       MOVEIT3-EXIT. EXIT.

       MOVEIT4 SECTION.
           ADD 1                    TO PUB.
           MOVE WSL-C (LUB)         TO VEL-C (PUB).
       MOVEIT4-EXIT. EXIT.

       MOVEITB SECTION.
           ADD 1                    TO VUB.
           MOVE WSE-C (WUB2, EUB)   TO VN-C (VUB).
           
           ADD 1                    TO EUB. 
       MOVEITB-EXIT. EXIT.

       READ-INFILE SECTION.
           READ IN-FILE           INTO WS-LINE
                AT END 
                   MOVE 'Y'         TO END-OF-FILE-FLAG.
           IF END-OF-FILE-FLAG NOT = 'Y'
               IF IN-7 = '*'
                   GO TO READ-INFILE
               ELSE
                   PERFORM VARYING MY-SUB FROM 1 BY 1 
                     UNTIL MY-SUB > 80
                      IF WS-LINE (MY-SUB :1) = 'a'
                         MOVE 'A' TO WS-LINE (MY-SUB :1)
                      END-IF
                      IF WS-LINE (MY-SUB :1) = 'b'
                         MOVE 'B' TO WS-LINE (MY-SUB :1)
                      END-IF
                      IF WS-LINE (MY-SUB :1) = 'c'
                         MOVE 'C' TO WS-LINE (MY-SUB :1)
                      END-IF
                      IF WS-LINE (MY-SUB :1) = 'd'
                         MOVE 'D' TO WS-LINE (MY-SUB :1)
                      END-IF
                      IF WS-LINE (MY-SUB :1) = 'e'
                         MOVE 'E' TO WS-LINE (MY-SUB :1)
                      END-IF
                      IF WS-LINE (MY-SUB :1) = 'f'
                         MOVE 'F' TO WS-LINE (MY-SUB :1)
                      END-IF
                      IF WS-LINE (MY-SUB :1) = 'g'
                         MOVE 'G' TO WS-LINE (MY-SUB :1)
                      END-IF
                      IF WS-LINE (MY-SUB :1) = 'h'
                         MOVE 'H' TO WS-LINE (MY-SUB :1)
                      END-IF
                      IF WS-LINE (MY-SUB :1) = 'i'
                         MOVE 'I' TO WS-LINE (MY-SUB :1)
                      END-IF
                      IF WS-LINE (MY-SUB :1) = 'j'
                         MOVE 'J' TO WS-LINE (MY-SUB :1)
                      END-IF
                      IF WS-LINE (MY-SUB :1) = 'k'
                         MOVE 'K' TO WS-LINE (MY-SUB :1)
                      END-IF
                      IF WS-LINE (MY-SUB :1) = 'l'
                         MOVE 'L' TO WS-LINE (MY-SUB :1)
                      END-IF
                      IF WS-LINE (MY-SUB :1) = 'm'
                         MOVE 'M' TO WS-LINE (MY-SUB :1)
                      END-IF
                      IF WS-LINE (MY-SUB :1) = 'n'
                         MOVE 'N' TO WS-LINE (MY-SUB :1)
                      END-IF
                      IF WS-LINE (MY-SUB :1) = 'o'
                         MOVE 'O' TO WS-LINE (MY-SUB :1)
                      END-IF
                      IF WS-LINE (MY-SUB :1) = 'p'
                         MOVE 'P' TO WS-LINE (MY-SUB :1)
                      END-IF
                      IF WS-LINE (MY-SUB :1) = 'q'
                         MOVE 'Q' TO WS-LINE (MY-SUB :1)
                      END-IF
                      IF WS-LINE (MY-SUB :1) = 'r'
                         MOVE 'R' TO WS-LINE (MY-SUB :1)
                      END-IF
                      IF WS-LINE (MY-SUB :1) = 's'
                         MOVE 'S' TO WS-LINE (MY-SUB :1)
                      END-IF
                      IF WS-LINE (MY-SUB :1) = 't'
                         MOVE 'T' TO WS-LINE (MY-SUB :1)
                      END-IF
                      IF WS-LINE (MY-SUB :1) = 'u'
                         MOVE 'U' TO WS-LINE (MY-SUB :1)
                      END-IF
                      IF WS-LINE (MY-SUB :1) = 'v'
                         MOVE 'V' TO WS-LINE (MY-SUB :1)
                      END-IF
                      IF WS-LINE (MY-SUB :1) = 'w'
                         MOVE 'W' TO WS-LINE (MY-SUB :1)
                      END-IF
                      IF WS-LINE (MY-SUB :1) = 'x'
                         MOVE 'X' TO WS-LINE (MY-SUB :1)
                      END-IF
                      IF WS-LINE (MY-SUB :1) = 'y'
                         MOVE 'Y' TO WS-LINE (MY-SUB :1)
                      END-IF
                      IF WS-LINE (MY-SUB :1) = 'z'
                         MOVE 'Z' TO WS-LINE (MY-SUB :1)
                      END-IF
                   END-PERFORM
               END-IF
           END-IF.
       READ-INFILE-EXIT. EXIT.


       READ-TCBL SECTION.
           READ TCBL-FILE
               AT END
                   MOVE 'Y'         TO END-OF-TCBL-FLAG.
       READ-TCBL-EXIT. EXIT.

       WRITE-AND-READ-CBL SECTION.
           IF END-OF-TCBL-FLAG NOT = 'Y'
               WRITE OUT-RECORD   FROM TCBL-RECORD
               PERFORM READ-TCBL
           END-IF.
       WRITE-AND-READ-CBL-EXIT. EXIT.







U5U5U5 U5 SECTION.
           PERFORM INITIALIZATION-U5-ROUTINE.
           PERFORM U5-LOOP UNTIL END-OF-PRN-FLAG = 'Y'.
           MOVE 0 TO MZ-SUB.

           PERFORM PROCESS-MY-ARRAY
               VARYING MY-SUB2
                  FROM 1 BY 1
                 UNTIL MY-SUB2 > MY-SU5.

           PERFORM GENERATE-SUBS-PROCESS
               VARYING MZ-SUB2
                  FROM 1 BY 1 
                 UNTIL MZ-SUB2 > MZ-SUB.

           PERFORM EOJ-U5-ROUTINE.

       U5-EXIT. EXIT.

       INITIALIZATION-U5-ROUTINE SECTION.
           OPEN INPUT  PRN-FILE.

           PERFORM READ-PRNFILE.
       INITIALIZATION-U5-EXIT. EXIT.

       U5-LOOP SECTION.


           IF PRN-RECORD (59 : 41 ) 
           = 'ZCOBOL WORKING STORAGE SECTION DATA ITEMS'
               MOVE 'Y' TO U5-WS-FOUND 
           END-IF.

           IF U5-WS-FOUND = 'Y'
               IF PRN-RECORD (59 : 10) = '01  FILLER'
                   PERFORM READ-PRNFILE
                   PERFORM 
                    UNTIL PRN-RECORD (59 : 7)   = '01  D1 '
                       OR PRN-RECORD (59 : 15 ) = '01  DEBUG_ITEM '
                       OR PRN-RECORD (59 : 15 ) = '01  ZC_WS_ID1  '
                       OR END-OF-PRN-FLAG      = 'Y'
                       PERFORM STORE-MY-STUFF
                       PERFORM READ-PRNFILE
                   END-PERFORM
                   IF END-OF-PRN-FLAG = 'Y'
                       DISPLAY 'U5(1) - PRN FILE ENDS PREMATURELY'
                       STOP RUN
                   END-IF
                   PERFORM 
                    UNTIL PRN-RECORD (59 : 16 )   = '01  RETURN_CODE '
                       OR END-OF-PRN-FLAG        = 'Y'
                       PERFORM READ-PRNFILE
                   END-PERFORM
                   IF END-OF-PRN-FLAG = 'Y'
                       DISPLAY 'U5(2) - PRN FILE ENDS PREMATURELY'
                       STOP RUN
                   END-IF
                   PERFORM READ-PRNFILE
                   PERFORM 
                    UNTIL PRN-RECORD (59 : 03 )   NOT = '01 '
                       OR END-OF-PRN-FLAG            = 'Y'
                       PERFORM STORE-MY-STUFF
                       PERFORM READ-PRNFILE
                   END-PERFORM
                   IF END-OF-PRN-FLAG = 'Y'
                       DISPLAY 'U5(3) - PRN FILE ENDS PREMATURELY'
                       STOP RUN
                   END-IF
               END-IF
           END-IF.

           IF PRN-RECORD (59 : 33) 
           = 'ZCOBOL LINKAGE SECTION DATA ITEMS'
               MOVE 'Y' TO U5-LS-FOUND
           END-IF.

*******           IF U5-LS-FOUND = 'Y'
*******               IF PRN-RECORD (59 : 03) = '01 '
      *DISPLAY 'U5 - 01  FOUND'
*******                   PERFORM 
*******                    UNTIL PRN-RECORD (59 : 6)   = '01  ZC'
*******                       OR END-OF-PRN-FLAG      = 'Y'
*******                       PERFORM STORE-MY-STUFF
*******                       PERFORM READ-PRNFILE
*******                   END-PERFORM
*******                   IF END-OF-PRN-FLAG = 'Y'
*******                       DISPLAY 'U5 - PRN FILE ENDS PREMATURELY'
*******                       STOP RUN
*******                   END-IF
*******                   IF PRN-RECORD (59:6) = '01  ZC'
*******                       DISPLAY 'U5-LOOP - FINISHED OK'
*******                       MOVE 'Y' TO END-OF-PRN-FLAG
*******                       GO TO U5-LOOP-EXIT
*******                   END-IF
*******               END-IF
*******           END-IF.

           PERFORM READ-PRNFILE.
       U5-LOOP-EXIT. EXIT.
      
       EOJ-U5-ROUTINE SECTION.
           CLOSE PRN-FILE.
       EOJ-U5-EXIT. EXIT.

       READ-PRNFILE SECTION.
           READ PRN-FILE           INTO PRN-RECORD
                AT END 
                   MOVE 'Y'          TO END-OF-PRN-FLAG.
       READ-PRNFILE-EXIT. EXIT.

       STORE-MY-STUFF SECTION.
           IF PRN-RECORD (63:01) = ' '
           OR PRN-RECORD (63:01) = '6'
           OR PRN-RECORD (63:02) = 'ZC'
           0R PRN-RECORD (63:06) = 'FILLER'
           OR PRN-RECORD (59:02) = '91'
               GO TO STORE-MY-STUFF-EXIT
           END-IF.
           ADD 1                      TO MY-SU5.
           IF MY-SU5 > MY-MAX
               DISPLAY 'U5 - MY-ARRAY FULL - BOMBING'
               STOP RUN
           END-IF.

           MOVE SPACES                TO MY-DUP  (MY-SU5).
           MOVE PRN-RECORD (59  : 02) TO MY-LVL  (MY-SU5).
           MOVE PRN-RECORD (63  : 30) TO MY-NAME (MY-SU5).
           MOVE PRN-RECORD (94  : 08) TO MY-ADDR (MY-SU5).
           MOVE PRN-RECORD (103 : 08) TO MY-LEN  (MY-SU5).
           MOVE 0                     TO MY-SUB3.
           MOVE SPACES                TO SIXCH.
           PERFORM VARYING MY-SUB2 FROM 112 
                BY 1 UNTIL PRN-RECORD (MY-SUB2 : 1) = SPACE
                 ADD 1             TO MY-SUB3
                 MOVE PRN-RECORD (MY-SUB2 : 1) TO SIXCH (MY-SUB3 : 1)
           END-PERFORM.
           PERFORM UNTIL SIXCH (6 : 1) NOT = SPACE
                 MOVE SIXCH (1 : 5) TO FIVECH
                 MOVE FIVECH TO SIXCH (2 : 5)
                 MOVE '0'    TO SIXCH (1 : 1)
           END-PERFORM.
           MOVE SIXCH                 TO MY-DUP  (MY-SU5).
           ADD  3                     TO MY-SUB2.
           MOVE PRN-RECORD (MY-SUB2 : 1)
                                      TO MY-TYPE (MY-SU5).
           ADD  3                     TO MY-SUB2.
           MOVE PRN-RECORD (MY-SUB2 : 10)
                                      TO MY-PIC  (MY-SU5).             
*******-----------------------------------------------------
           DISPLAY                  '|'
                   MY-LVL  (MY-SU5) '|'
                   MY-NAME (MY-SU5) '|'
                   MY-ADDR (MY-SU5) '|'
                   MY-LEN  (MY-SU5) '|'
                   MY-DUP  (MY-SU5) '|' 
                   MY-TYPE (MY-SU5) '|'
                   MY-PIC  (MY-SU5) '|'.             

       STORE-MY-STUFF-EXIT. EXIT.

       PROCESS-MY-ARRAY SECTION.
           ADD  1                         TO MZ-SUB.
           IF MZ-SUB > MZ-MAX
               DISPLAY 'U5 - MZ-ARRAY FULL - BOMBING'
               STOP RUN
           END-IF.
           IF MY-LVL (MY-SUB2) = '01'
               MOVE '00'                  TO MZ-LVL  (MZ-SUB)
               ADD 1                      TO MZ-SUB
               IF MY-SUB > MY-MAX
                   DISPLAY 'U5 - MY-ARRAY FULL - BOMBING'
                   STOP RUN
               END-IF
           END-IF.
           MOVE MY-LVL  (MY-SUB2)         TO MZ-LVL  (MZ-SUB).
           MOVE MY-NAME (MY-SUB2)         TO THIRTYCH.
           PERFORM VARYING MZ-SUB3 FROM 1 BY 1
             UNTIL MZ-SUB3 > 30
              IF THIRTYCH (MZ-SUB3 : 1) = '_'
                  MOVE '-' TO THIRTYCH (MZ-SUB3 : 1)
              END-IF
           END-PERFORM.

           MOVE THIRTYCH                  TO MZ-NAME (MZ-SUB).
           MOVE MY-TYPE (MY-SUB2)         TO MZ-TYPE (MZ-SUB).
           MOVE MY-PIC  (MY-SUB2)         TO MZ-PIC  (MZ-SUB).
           MOVE MY-DUP  (MY-SUB2)         TO MZ-DUP  (MZ-SUB).
           MOVE 0                         TO ADDR9
                                             LEN9. 
           MOVE MY-ADDR (MY-SUB2)         TO EIGHTCH.
           MOVE 1                         TO MULTIP.
           PERFORM VARYING MZ-SUB2 FROM 8 BY -1 
             UNTIL MZ-SUB2 = 3
              MOVE EIGHTCH (MZ-SUB2 : 1)
                TO ONECH
              MOVE ZEROES TO INTERMED
              IF ONECH = '0'
                  CONTINUE
              END-IF 
              IF ONECH = '1'
                  COMPUTE INTERMED = 1  * MULTIP
              END-IF 
              IF ONECH = '2'
                  COMPUTE INTERMED = 2  * MULTIP
              END-IF 
              IF ONECH = '3'
                  COMPUTE INTERMED = 3  * MULTIP
              END-IF 
              IF ONECH = '4'
                  COMPUTE INTERMED = 4  * MULTIP
              END-IF 
              IF ONECH = '5'
                  COMPUTE INTERMED = 5  * MULTIP
              END-IF 
              IF ONECH = '6'
                  COMPUTE INTERMED = 6  * MULTIP
              END-IF 
              IF ONECH = '7'
                  COMPUTE INTERMED = 7  * MULTIP
              END-IF 
              IF ONECH = '8'
                  COMPUTE INTERMED = 8  * MULTIP
              END-IF 
              IF ONECH = '9'
                  COMPUTE INTERMED = 9  * MULTIP
              END-IF 
              IF ONECH = 'A'
                  COMPUTE INTERMED = 10 * MULTIP
              END-IF 
              IF ONECH = 'B'
                  COMPUTE INTERMED = 11 * MULTIP
              END-IF 
              IF ONECH = 'C'
                  COMPUTE INTERMED = 12 * MULTIP
              END-IF 
              IF ONECH = 'D'
                  COMPUTE INTERMED = 13 * MULTIP
              END-IF 
              IF ONECH = 'E'
                  COMPUTE INTERMED = 14 * MULTIP
              END-IF 
              IF ONECH = 'F'
                  COMPUTE INTERMED = 15 * MULTIP
              END-IF 
              ADD INTERMED TO ADDR9
              COMPUTE MULTIP = MULTIP * 16
           END-PERFORM.

           MOVE ADDR9 TO MZ-ADDR (MZ-SUB).

           MOVE MY-LEN (MY-SUB2) TO EIGHTCH.
           MOVE 1                TO MULTIP.

           PERFORM VARYING MZ-SUB2 FROM 8 BY -1 
             UNTIL MZ-SUB2 = 3
              MOVE EIGHTCH (MZ-SUB2 : 1)
                TO ONECH
              MOVE ZEROES TO INTERMED
              IF ONECH = '0'
                  CONTINUE
              END-IF 
              IF ONECH = '1'
                  COMPUTE INTERMED = 1  * MULTIP
              END-IF 
              IF ONECH = '2'
                  COMPUTE INTERMED = 2  * MULTIP
              END-IF 
              IF ONECH = '3'
                  COMPUTE INTERMED = 3  * MULTIP
              END-IF 
              IF ONECH = '4'
                  COMPUTE INTERMED = 4  * MULTIP
              END-IF 
              IF ONECH = '5'
                  COMPUTE INTERMED = 5  * MULTIP
              END-IF 
              IF ONECH = '6'
                  COMPUTE INTERMED = 6  * MULTIP
              END-IF 
              IF ONECH = '7'
                  COMPUTE INTERMED = 7  * MULTIP
              END-IF 
              IF ONECH = '8'
                  COMPUTE INTERMED = 8  * MULTIP
              END-IF 
              IF ONECH = '9'
                  COMPUTE INTERMED = 9  * MULTIP
              END-IF 
              IF ONECH = 'A'
                  COMPUTE INTERMED = 10 * MULTIP
              END-IF 
              IF ONECH = 'B'
                  COMPUTE INTERMED = 11 * MULTIP
              END-IF 
              IF ONECH = 'C'
                  COMPUTE INTERMED = 12 * MULTIP 
              END-IF 
              IF ONECH = 'D'
                  COMPUTE INTERMED = 13 * MULTIP
              END-IF 
              IF ONECH = 'E'
                  COMPUTE INTERMED = 14 * MULTIP
              END-IF 
              IF ONECH = 'F'
                  COMPUTE INTERMED = 15 * MULTIP
              END-IF 
              ADD INTERMED TO LEN9
              COMPUTE MULTIP = MULTIP * 16
           END-PERFORM.

           MOVE LEN9 TO MZ-LEN (MZ-SUB).

           MOVE 0 TO SUBS-COUNT.
           IF MZ-DUP (MZ-SUB) > '000001'
               ADD 1 TO SUBS-COUNT
           END-IF.

           PERFORM VARYING MZ-SUB2
              FROM MZ-SUB
                BY -1
             UNTIL MZ-SUB2 = 0 
                OR MZ-LVL (MZ-SUB2) = '00'
                 IF MZ-LVL (MZ-SUB2) < MZ-LVL (MZ-SUB)
                     IF MZ-DUP (MZ-SUB2) > '000001'
                         ADD 1 TO SUBS-COUNT
                     END-IF
                 END-IF  
           END-PERFORM.

           MOVE SUBS-COUNT TO MZ-SUBS (MZ-SUB).

*******-----------------------------------------------------
           DISPLAY                  '2|'
                   MZ-LVL  (MZ-SUB) '|'
                   MZ-NAME (MZ-SUB) '|'
                   MZ-ADDR (MZ-SUB) '|'
                   MZ-LEN  (MZ-SUB) '|'
                   MZ-DUP  (MZ-SUB) '|'
                   MZ-SUBS (MZ-SUB) '|' 
                   MZ-TYPE (MZ-SUB) '|'
                   MZ-PIC  (MZ-SUB) '|'.             
      *
       PROCESS-MY-ARRAY-EXIT. EXIT.

       GENERATE-SUBS-PROCESS SECTION.
           IF MZ-SUB2 = 1
               WRITE OUT-RECORD 
                FROM SUBS-PROCESS-SECTION
           END-IF.
 
           IF MZ-SUBS (MZ-SUB2) > '000000'
               GO TO GENERATE-SUBS-PROCESS-LAST
           END-IF.
 
           IF MZ-PIC (MZ-SUB2) (2:1) NOT = '9'
               GO TO GENERATE-SUBS-PROCESS-LAST
           END-IF.

           MOVE MZ-PIC (MZ-SUB2) TO TENCH.
           PERFORM VARYING MZ-SUB3 FROM 1 BY 1
             UNTIL MZ-SUB3 > 10
               IF TENCH (MZ-SUB3 : 1) = 'V'
                   GO TO GENERATE-SUBS-PROCESS-LAST
               END-IF
           END-PERFORM.

           MOVE MZ-NAME (MZ-SUB2) TO THIRTYCH.
           PERFORM VARYING MZ-SUB3 FROM 1 BY 1
             UNTIL MZ-SUB3 > 30
              IF THIRTYCH (MZ-SUB3 : 1) = '_'
                  MOVE '-' TO THIRTYCH (MZ-SUB3 : 1)
              END-IF
           END-PERFORM.

           MOVE THIRTYCH TO SUBS-IF-GLOB-NAME.
           WRITE OUT-RECORD
            FROM SUBS-IF-GLOB.
   
           MOVE THIRTYCH TO SUBS-MOVE-NAME-NAME.
           WRITE OUT-RECORD
            FROM SUBS-MOVE-NAME.


           WRITE OUT-RECORD
            FROM END-IF-STMT.
       GENERATE-SUBS-PROCESS-LAST.
           IF MZ-SUB2 = MZ-SUB
               WRITE OUT-RECORD
                FROM SUBS-PROCESS-EXIT
           END-IF.
       GENERATE-SUBS-PROCESS-EXIT. EXIT.









U7U7U7 U7 SECTION.
           WRITE OUT-RECORD FROM NAME9-PROCESS-SECTION.

           PERFORM U7-9-LOOP 
            VARYING U7-SUB 
               FROM 1 BY 1
              UNTIL U7-SUB > MZ-SUB.

           WRITE OUT-RECORD FROM NAME9-PROCESS-EXIT. 
      
           WRITE OUT-RECORD FROM NAMEX-PROCESS-SECTION.

           PERFORM U7-X-LOOP 
            VARYING U7-SUB 
               FROM 1 BY 1
              UNTIL U7-SUB > MZ-SUB.

           WRITE OUT-RECORD FROM NAMEX-PROCESS-EXIT.

       U7-EXIT. EXIT.

       U7-9-LOOP SECTION.
           IF MZ-LVL (U7-SUB) = '00'
               GO TO U7-9-LOOP-EXIT
           END-IF.
           IF  MZ-PIC (U7-SUB) (2:1) NOT = '9'
           AND MZ-PIC (U7-SUB) (2:1) NOT = 'Z'
               GO TO U7-9-LOOP-EXIT
           END-IF.

           IF MZ-NAME (U7-SUB) (1:6) = 'FILLER'
               GO TO U7-9-LOOP-EXIT
           END-IF.

           MOVE MZ-SUBS     (U7-SUB) TO GLOB-SUBS.

           MOVE MZ-NAME     (U7-SUB) TO GLOB-NAME.

           MOVE MZ-SUBS-ID  (U7-SUB, 1)
                                     TO GLOB-SUB1.
           MOVE MZ-SUBS-ID  (U7-SUB, 2)
                                     TO GLOB-SUB2.
           MOVE MZ-SUBS-ID  (U7-SUB, 3)
                                     TO GLOB-SUB3.
           PERFORM INITIALIZATION-U7-9-ROUTINE.
           PERFORM UNTIL END-OF-U6-FLAG = 'Y'
               IF WS-U6-1 = GLOB-SUBS
                   IF WS-U6 (3:1) = '*'
                       MOVE WS-U6     TO WS-U8
                       MOVE 'TTTTTT ' TO WS-U8 (1:7)
     
                       WRITE OUT-RECORD FROM WS-U8

                       PERFORM READ-U6FILE
                   ELSE
                       IF WS-U6 (2:1) = '>'
                           MOVE SPACES     TO U8-LINE
                           MOVE 'TTTTTT '  TO U8-1-FOR-6
                           MOVE 'DISPLAY ' TO U8-VERB
                           MOVE GLOB-NAME  TO U8-NAME
                           IF GLOB-SUBS = 1
                               MOVE '('    TO U8-OPEN
                               MOVE 'GLOB-SUB1' 
                                           TO U8-SUB1
                               MOVE ')'    TO U8-CLOSE
                           END-IF
                           IF GLOB-SUBS = 2
                               MOVE '('    TO U8-OPEN
                               MOVE 'GLOB-SUB1'
                                           TO U8-SUB1
                               MOVE ','    TO U8-COMMA1
                               MOVE 'GLOB-SUB2'
                                           TO U8-SUB2
                               MOVE ')'    TO U8-CLOSE
                           END-IF
                           IF GLOB-SUBS = 3
                               MOVE '('    TO U8-OPEN
                               MOVE 'GLOB-SUB1'
                                           TO U8-SUB1
                               MOVE ' , '  TO U8-COMMA1
                               MOVE 'GLOB-SUB2' 
                                           TO U8-SUB2
                               MOVE ' , '  TO U8-COMMA2
                               MOVE 'GLOB-SUB3' 
                                           TO U8-SUB3
                               MOVE ')'    TO U8-CLOSE
                           END-IF
                           PERFORM VARYING OSUB1
                              FROM 1 BY 1
                             UNTIL OSUB1 > 131
                               IF U8-LINE (OSUB1:2) = '  '
                                   MOVE LOW-VALUE 
                                     TO U8-LINE (OSUB1:1)
                               END-IF
                               IF U8-LINE (OSUB1:2) = ' 0'
                                   MOVE X'00F0'
                                     TO U8-LINE (OSUB1:2)
                               END-IF
                           END-PERFORM
                           MOVE 0 TO OSUB2
                           MOVE SPACES TO U8-LINE2
                           PERFORM VARYING OSUB1
                              FROM 1 BY 1
                             UNTIL OSUB1 > 131
                               IF U8-LINE (OSUB1:1)
                               NOT = LOW-VALUE
                                   ADD 1 TO OSUB2
                                   MOVE U8-LINE  (OSUB1:1)
                                     TO ONECH
                                   MOVE ONECH
                                     TO U8-LINE2 (OSUB2:1)
                               END-IF
                           END-PERFORM

                           WRITE OUT-RECORD FROM U8-LINE2 
            
                           PERFORM READ-U6FILE
                       ELSE
                           MOVE WS-U6      TO WS-U8
                           MOVE 'TTTTTT '  TO WS-U8-1-FOR-6
                           MOVE GLOB-NAME  TO WS-U8-28-FOR-30

                           WRITE OUT-RECORD FROM WS-U8

                           PERFORM READ-U6FILE     
                       END-IF
                   END-IF
               ELSE
                   PERFORM READ-U6FILE
               END-IF 
           END-PERFORM.
           PERFORM EOJ-U7-9-ROUTINE.
       U7-9-LOOP-EXIT. EXIT.

       INITIALIZATION-U7-9-ROUTINE SECTION.
           MOVE 'N' TO END-OF-U6-FLAG.
           OPEN INPUT  U6-FILE.
           PERFORM READ-U6FILE.
       INITIALIZATION-U7-9-EXIT. EXIT.


       EOJ-U7-9-ROUTINE SECTION.
           CLOSE U6-FILE.
       EOJ-U7-9-EXIT. EXIT.

       READ-U6FILE SECTION.
           READ U6-FILE           INTO WS-U6
                AT END 
                   MOVE 'Y'         TO END-OF-U6-FLAG.
       READ-U6FILE-EXIT. EXIT.
      *
       U7-X-LOOP SECTION.
           IF MZ-LVL (U7-SUB) = '00'
               GO U7-X-LOOP-EXIT
           END-IF.
           IF MZ-PIC (U7-SUB) (2:1) = '9'
           OR MZ-PIC (U7-SUB) (2:1) = 'Z'
               GO TO U7-X-LOOP-EXIT
           END-IF.

           IF MZ-NAME (U7-SUB) (1:6) = 'FILLER'
               GO TO U7-X-LOOP-EXIT
           END-IF.

           MOVE MZ-SUBS     (U7-SUB) TO GLOB-SUBS.

           MOVE MZ-NAME     (U7-SUB) TO GLOB-NAME.

           MOVE MZ-SUBS-ID  (U7-SUB, 1)
                                     TO GLOB-SUB1.
           MOVE MZ-SUBS-ID  (U7-SUB, 2)
                                     TO GLOB-SUB2.
           MOVE MZ-SUBS-ID  (U7-SUB, 3)
                                     TO GLOB-SUB3.
           PERFORM INITIALIZATION-U7-X-ROUTINE.
           PERFORM UNTIL END-OF-U6-FLAG = 'Y'
               IF WS-U6-1 = GLOB-SUBS
                   IF WS-U6 (3:1) = '*'
                       MOVE WS-U6     TO WS-U8
                       MOVE 'TTTTTT ' TO WS-U8 (1:7)
                       WRITE OUT-RECORD FROM WS-U8
                       PERFORM READ-U6BFILE
                   ELSE
                       IF WS-U6 (2:1) = '>'
                           MOVE SPACES     TO U8-LINE
                           MOVE 'TTTTTT '  TO U8-1-FOR-6
                           MOVE 'DISPLAY ' TO U8-VERB
                           MOVE GLOB-NAME  TO U8-NAME
                           IF GLOB-SUBS = 1
                               MOVE '('    TO U8-OPEN
                               MOVE 'GLOB-SUB1' 
                                           TO U8-SUB1
                               MOVE ')'    TO U8-CLOSE
                           END-IF
                           IF GLOB-SUBS = 2
                               MOVE '('    TO U8-OPEN
                               MOVE 'GLOB-SUB1'
                                           TO U8-SUB1
                               MOVE ','    TO U8-COMMA1
                               MOVE 'GLOB-SUB2'
                                           TO U8-SUB2
                               MOVE ')'    TO U8-CLOSE
                           END-IF
                           IF GLOB-SUBS = 3
                               MOVE '('    TO U8-OPEN
                               MOVE 'GLOB-SUB1'
                                           TO U8-SUB1
                               MOVE ' , '  TO U8-COMMA1
                               MOVE 'GLOB-SUB2' 
                                           TO U8-SUB2
                               MOVE ' , '  TO U8-COMMA2
                               MOVE 'GLOB-SUB3' 
                                           TO U8-SUB3
                               MOVE ')'    TO U8-CLOSE
                           END-IF
                           PERFORM VARYING OSUB1
                              FROM 1 BY 1
                             UNTIL OSUB1 > 131
                               IF U8-LINE (OSUB1:2) = '  '
                                   MOVE LOW-VALUE 
                                     TO U8-LINE (OSUB1:1)
                               END-IF
                               IF U8-LINE (OSUB1:2) = ' 0'
                                   MOVE X'00F0'
                                     TO U8-LINE (OSUB1:2)
                               END-IF
                           END-PERFORM
                           MOVE 0 TO OSUB2
                           MOVE SPACES TO U8-LINE2
                           PERFORM VARYING OSUB1
                              FROM 1 BY 1
                             UNTIL OSUB1 > 131
                               IF U8-LINE (OSUB1:1)
                               NOT = LOW-VALUE
                                   ADD 1 TO OSUB2
                                   MOVE U8-LINE  (OSUB1:1)
                                     TO ONECH
                                   MOVE ONECH
                                     TO U8-LINE2 (OSUB2:1)
                               END-IF
                           END-PERFORM
                           WRITE OUT-RECORD FROM U8-LINE2
            
                           PERFORM READ-U6BFILE
                       ELSE
                           MOVE WS-U6      TO WS-U8
                           MOVE 'TTTTTT '  TO WS-U8-1-FOR-6
                           MOVE GLOB-NAME  TO WS-U8-28-FOR-30

                           WRITE OUT-RECORD FROM WS-U8
                           PERFORM READ-U6BFILE 
                       END-IF
                   END-IF
               ELSE
                   PERFORM READ-U6BFILE
               END-IF 
           END-PERFORM.
           PERFORM EOJ-U7-X-ROUTINE.
       U7-X-LOOP-EXIT. EXIT.

       INITIALIZATION-U7-X-ROUTINE SECTION.
           MOVE 'N' TO END-OF-U6-FLAG.
           OPEN INPUT  U6B-FILE.
           PERFORM READ-U6BFILE.
       INITIALIZATION-U7-X-EXIT. EXIT.


       EOJ-U7-X-ROUTINE SECTION.
           CLOSE U6B-FILE.
           MOVE 'Y' TO ITS-CLOSED.
       EOJ-U7-X-EXIT. EXIT.

       READ-U6BFILE SECTION.
           READ U6B-FILE           INTO WS-U6
                AT END 
                   MOVE 'Y'         TO END-OF-U6-FLAG.
       READ-U6BFILE-EXIT. EXIT.
