       IDENTIFICATION DIVISION.
       PROGRAM-ID.  T.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT IN-FILE
              ASSIGN TO 'zcobol\demo\copyfile.cbl'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUT-FILE
               ASSIGN TO 'ZCOBOL\DEMO\PROGRAM.CBL'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT TCBL-FILE
               ASSIGN TO 'ZCOBOL\DEMO\T2.CBL'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUTLINE-FILE
               ASSIGN TO 'ZCOBOL\DEMO\PROGRAM.LIN'
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  IN-FILE
           DATA RECORD IS IN-RECORD.
       01  IN-RECORD.
           03  FILLER               PIC X(06).
           03  IN-7                 PIC X(01).
           03  FILLER               PIC X(73).
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

       WORKING-STORAGE SECTION.
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
            'SET', 'SORT', 'START'.
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
/
       PROCEDURE DIVISION.
       MAINLINE.
           DISPLAY 'T STARTED'.
           PERFORM INITIALIZATION-ROUTINE.
           PERFORM THE-LOOP UNTIL END-OF-FILE-FLAG = 'Y'.
           PERFORM WRITE-AND-READ-CBL
             UNTIL END-OF-TCBL-FLAG = 'Y'.
           PERFORM EOJ-ROUTINE.
           DISPLAY 'T ENDED OK'
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
               DISPLAY 'T1 - PROGRAM-ID FOUND'
220312         PERFORM DUMMMY VARYING LUB
                             FROM 20  BY 1
                            UNTIL LUB > 72
                               OR WSL-C (LUB) NOT = SPACE
220312         PERFORM DUMMMY VARYING LUB2
                             FROM LUB BY 1
                            UNTIL LUB2 > 72
                               OR WSL-C (LUB2) = '.'
               MOVE SPACES          TO PROGRAM-ID-LINE
               MOVE 0               TO PUB
               PERFORM MOVEIT VARYING LUB
                              FROM LUB BY 1
                             UNTIL LUB = LUB2
               MOVE PROGRAM-ID-LINE TO THE-PROGRAM-ID
           END-IF.

           MOVE 'N'                 TO SKIPIT.

           IF WS-8-26 = 'INPUT-OUTPUT SECTIO'
               DISPLAY 'T1 - INPUT-OUTPUT SECTION FOUND'
               MOVE 'Y'             TO INPUT-OUTPUT-SECTION-FOUND
           END-IF.

           IF WS-8-26 = 'FILE CONTROL.      '
               DISPLAY 'T1 - FILE CONTROL FOUND'
               MOVE 'Y'             TO FILE-CONTROL-FOUND
           END-IF.

           IF WS-8-26 = 'FILE SECTION.      '
               DISPLAY 'T1 - FILE SECTION FOUND'
               MOVE 'Y'             TO FILE-SECTION-FOUND
           END-IF.

           IF  WS-8-26 = 'DATA DIVISION.     '
               DISPLAY 'T1 - DATA DIVISION FOUND'
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
               DISPLAY 'T1 - WORKING-STORAGE SECTION FOUND'
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
               DISPLAY 'T1 - PROCEDURE DIVISION FOUND'
               MOVE 'Y'             TO PROCEDURE-DIVISION-FOUND
           END-IF.

           IF WS-8-22 = 'LINKAGE SECTION'
               DISPLAY 'T1 - LINKAGE SECTION FOUND'
               MOVE 'Y'             TO LINKAGE-SECTION-FOUND
           END-IF.

           IF WORKING-STORAGE-SECTION-FOUND        = 'Y'
               IF LINKAGE-SECTION-FOUND            = 'N'
                  IF PROCEDURE-DIVISION-FOUND      = 'N'
                     DISPLAY 'T1 - WS/LS/PD=YNN'
                     ADD 1          TO WUB
                     IF WUB > 999
                         DISPLAY 'MAX OF 999 CBL LINES EXCEEDED'
                         DISPLAY 'BOMBING'
                         STOP RUN
                     END-IF
                     MOVE WS-LINE   TO WS-ENTRY (WUB)
                     DISPLAY 'T1 - WS-LINE=' WS-LINE
                     PERFORM READ-INFILE
                     GO THE-LOOP-EXIT
                  END-IF
               END-IF
           END-IF.

           IF WORKING-STORAGE-SECTION-FOUND        = 'Y'
              IF LINKAGE-SECTION-FOUND             = 'Y'
                 IF FURST-THYME                    = 'Y'
                     DISPLAY 'T1 - WS/LS/FT=YYY'
                     MOVE 'N'       TO FURST-THYME
                     PERFORM WRITE-AND-READ-CBL
                         UNTIL TCBL-1-6 = '------'
                     PERFORM READ-TCBL
                     MOVE 1         TO WUB2
                     PERFORM MOVEIT2
                       UNTIL WUB2 > WUB
                     PERFORM WRITE-AND-READ-CBL
                         UNTIL TCBL-1-6 = '------'
                     PERFORM READ-TCBL
                     GO THE-LOOP-EXIT
                 END-IF
              END-IF
           END-IF.

           IF WORKING-STORAGE-SECTION-FOUND        = 'Y'
              IF PROCEDURE-DIVISION-FOUND          = 'Y'
                 IF FURST-THYME                    = 'Y'
                     DISPLAY 'T1 - WS/PD/FT=YYY'
                     MOVE 'N'       TO FURST-THYME
                     PERFORM WRITE-AND-READ-CBL
                         UNTIL TCBL-1-6 = '------'
                     PERFORM READ-TCBL
                     MOVE 1         TO WUB2
                     PERFORM MOVEIT2
                       UNTIL WUB2 > WUB
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
                  DISPLAY 'T1 - COMMENT FOUND'
220312            PERFORM DUMMMY VARYING LUB
                                FROM 8 BY 1
                               UNTIL LUB > 72
                                  OR WSL-C (LUB) = SPACE OR '.'
                  MOVE SPACES          TO PARA-LINE
                  MOVE 0               TO PUB
                  PERFORM MOVEIT3 VARYING LUB2
                                 FROM 8 BY 1
                                UNTIL LUB2 = LUB
                  MOVE PARA-LINE       TO THE-PARA
              ELSE
220312            PERFORM DUMMMY VARYING LUB
                                   FROM 11  BY 1
                                  UNTIL LUB > 72
                                     OR WSL-C (LUB) NOT = SPACE
220312            PERFORM DUMMMY VARYING LUB2
                                   FROM LUB BY 1
                                  UNTIL LUB2 > 72
                                     OR WSL-C (LUB2) = SPACE
                  MOVE SPACES          TO VERB-LINE
                  MOVE 0               TO PUB
                  PERFORM MOVEIT4 VARYING LUB
                                  FROM LUB BY 1
                                 UNTIL LUB = LUB2
                  MOVE VERB-LINE       TO THE-VERB
                  IF ITS-A-VERB
                      DISPLAY 'T1 - VERB FOUND'
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
               DISPLAY 'T1 - SKIP=N'

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
           CLOSE OUT-FILE.
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

       MOVEIT2 SECTION.
           IF WSE-C (WUB2, 08) = '0'
               IF WSE-C (WUB2, 09) = '1'
                   NEXT SENTENCE
               ELSE
                   MOVE WS-ENTRY (WUB2) TO OUT-RECORD
                   WRITE OUT-RECORD
                   ADD 1                TO WUB2
                   GO MOVEIT2-EXIT
               END-IF
           ELSE
           IF WSE-C (WUB2, 08) = '7'
               IF WSE-C (WUB2, 09) = '7'
                   NEXT SENTENCE
               ELSE
                   MOVE WS-ENTRY (WUB2) TO OUT-RECORD
                   WRITE OUT-RECORD
                   ADD 1                TO WUB2
                   GO MOVEIT2-EXIT
               END-IF
           END-IF.


220312     PERFORM DUMMMY VARYING EUB
               FROM 10 BY 1 UNTIL EUB > 72
                 OR WSE-C (WUB2, EUB)
                NOT = SPACE.

220312     PERFORM DUMMMY VARYING EUB2
              FROM EUB BY 1 UNTIL EUB2 > 72
                OR WSE-C (WUB2, EUB2)
                 = '.' OR SPACE.

           MOVE SPACES              TO VARIABLE-NAME.
           MOVE 0                   TO VUB.

           PERFORM MOVEITB UNTIL EUB = EUB2.


130312     MOVE 'N' TO PIC-FOUND.
130312     PERFORM VARYING EUB3
130312        FROM 8 BY 1 UNTIL EUB3 > 72
130312          OR PIC-FOUND         = 'Y'
130312                 COMPUTE EUB4 = EUB3 + 1
130312                 COMPUTE EUB5 = EUB4 + 1
130312                 COMPUTE EUB6 = EUB5 + 1
130312                 COMPUTE EUB7 = EUB6 + 1
130312                 IF  WSE-C (WUB2, EUB3) = SPACE
130312                 AND WSE-C (WUB2, EUB4) = 'P'
130312                 AND WSE-C (WUB2, EUB5) = 'I'
130312                 AND WSE-C (WUB2, EUB6) = 'C'
130312                 AND WSE-C (WUB2, EUB7) = SPACE
130312                  MOVE 'Y' TO PIC-FOUND
130312                 END-IF
130312     END-PERFORM.
130312     DISPLAY '------------------------'.
130312     DISPLAY '1 >>>> WUB2=' WUB2.
130312     DISPLAY '1 >>>> WS-ENTRY=' WS-ENTRY (WUB2) (1:72).
130312     DISPLAY '1 EUB3=' EUB3.
130312     DISPLAY '1 PIC-FOUND=' PIC-FOUND.
130312
130312     MOVE 'N' TO EIGHTY-EIGHT-FOUND.
130312     PERFORM VARYING EUB3
130312        FROM 7 BY 1 UNTIL EUB3 > 72
130312          OR EIGHTY-EIGHT-FOUND         = 'Y'
130312                 COMPUTE EUB4 = EUB3 + 1
130312                 COMPUTE EUB5 = EUB4 + 1
130312                 COMPUTE EUB6 = EUB5 + 1
130312                 COMPUTE WUB3 = WUB2 + 1
130312                 IF  WSE-C (WUB3, EUB3) = SPACE
130312                 AND WSE-C (WUB3, EUB4) = '8'
130312                 AND WSE-C (WUB3, EUB5) = '8'
130312                 AND WSE-C (WUB3, EUB6) = SPACE
130312                  MOVE 'Y' TO EIGHTY-EIGHT-FOUND
130312                 END-IF
130312     END-PERFORM.
130312     DISPLAY ' '.
130312     DISPLAY '2 >>>> WS-ENTRY=' WS-ENTRY (WUB2 + 1) (1:72).
130312     DISPLAY '2 EUB3=' EUB3.
130312     DISPLAY '2 EIGHTY-EIGHT-FOUND=' EIGHTY-EIGHT-FOUND.

130312     IF PIC-FOUND = 'N'
130312     OR EIGHTY-EIGHT-FOUND = 'Y'
130312             DISPLAY '3 ' PIC-FOUND ' & ' EIGHTY-EIGHT-FOUND
130312             MOVE 'N' TO OH1-77-FOUND
130312             COMPUTE WUB3 = WUB2 + 1
130312             PERFORM VARYING WUB3 FROM WUB3 BY 1
130312               UNTIL WUB3 > WUB
130312                  OR OH1-77-FOUND = 'Y'
130312                   DISPLAY '3 @@@@ WUB3=' WUB3
130312                   DISPLAY '3 @@@@ WS-ENTRY=' WS-ENTRY (WUB3)
130312                   PERFORM VARYING EUB3 FROM 7 BY 1
130312                     UNTIL EUB3 > 72
130312                        OR OH1-77-FOUND = 'Y'
130312                         COMPUTE EUB4 = EUB3 + 1
130312                         COMPUTE EUB5 = EUB4 + 1
130312                         COMPUTE EUB6 = EUB5 + 1
130312                         IF  (WSE-C (WUB3, EUB3) = SPACE AND
130312                              WSE-C (WUB3, EUB4) = '0'   AND
130312                              WSE-C (WUB3, EUB5) = '1'   AND
130312                              WSE-C (WUB3, EUB6) = SPACE)
130312                         OR  (WSE-C (WUB3, EUB3) = SPACE AND
130312                              WSE-C (WUB3, EUB4) = '7'   AND
130312                              WSE-C (WUB3, EUB5) = '7'   AND
130312                              WSE-C (WUB3, EUB6) = SPACE)
130372                          DISPLAY '3 @@@@ WUB3=' WUB3
130312                          DISPLAY '3 @@@@ OH1-77-FOUND=Y'
130312                          MOVE 'Y' TO OH1-77-FOUND
130312                         END-IF
130312                   END-PERFORM
130312             END-PERFORM
130312             DISPLAY ' '
130212             DISPLAY '3 WUB3=' WUB3
130312             DISPLAY '3 OH1-77-FOUND=' OH1-77-FOUND
130312             IF OH1-77-FOUND = 'Y'
130312                 COMPUTE WUB3 = WUB3 - 2
130312                 DISPLAY ' '
130312                 DISPLAY '3 OH1-77-FOUND=Y'
130312             END-IF
130312             DISPLAY '3 WUB3=' WUB3
130312             WRITE OUT-RECORD       FROM B4-VARIABLE
130312             WRITE OUT-RECORD       FROM VARIABLE-LINE
130312             MOVE WS-ENTRY (WUB2)     TO OUT-RECORD
130312             WRITE OUT-RECORD
130312             DISPLAY '3 >>>>' B4-VARIABLE
130312             DISPLAY '3 >>>>' VARIABLE-LINE
130312             DISPLAY '3 >>>> WUB2=' WUB2
130312             DISPLAY '3 >>>> WS-ENTRY=' WS-ENTRY (WUB2) (1:72)
130312             COMPUTE WUB2 = WUB2 + 1
130312             DISPLAY '3 ++++ WS-ENTRY=' WS-ENTRY (WUB2) (1:72)
130312             DISPLAY '3 ++++ WUB2=' WUB2 ' & WUB3=' WUB3
130312             PERFORM VARYING WUB2 FROM WUB2 BY 1
130312               UNTIL WUB2 > WUB3
130312                IF WSE-C (WUB2, 1) = '6'
130312                    DISPLAY '3 ++++ 666666666666666666666'
130312                ELSE
130312                    MOVE WS-ENTRY (WUB2)  TO OUT-RECORD
130312                    WRITE OUT-RECORD
130312                    DISPLAY '3 ---- WUB2=' WUB2
130312                    DISPLAY '3 ---- WS-ENTRY=' WS-ENTRY (WUB2)
130312                END-IF
130312             END-PERFORM
130312             WRITE OUT-RECORD       FROM AFTA-VARIABLE
130312             DISPLAY '4 >>>>' AFTA-VARIABLE
130312             DISPLAY '4 >>>> ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^'

130312             GO TO MOVEIT2-EXIT
130312     END-IF.
130312

           WRITE OUT-RECORD       FROM B4-VARIABLE.
           WRITE OUT-RECORD       FROM VARIABLE-LINE.
           MOVE WS-ENTRY (WUB2)     TO OUT-RECORD.
           WRITE OUT-RECORD.
           WRITE OUT-RECORD       FROM AFTA-VARIABLE.
           ADD 1                    TO WUB2.
       MOVEIT2-EXIT. EXIT.

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
140312         ELSE
140312             PERFORM VARYING MY-SUB FROM 1 BY 1
140312               UNTIL MY-SUB > 80
140312                IF WS-LINE (MY-SUB :1) = 'a'
140312                   MOVE 'A' TO WS-LINE (MY-SUB :1)
140312                END-IF
140312                IF WS-LINE (MY-SUB :1) = 'b'
140312                   MOVE 'B' TO WS-LINE (MY-SUB :1)
140312                END-IF
140312                IF WS-LINE (MY-SUB :1) = 'c'
140312                   MOVE 'C' TO WS-LINE (MY-SUB :1)
140312                END-IF
140312                IF WS-LINE (MY-SUB :1) = 'd'
140312                   MOVE 'D' TO WS-LINE (MY-SUB :1)
140312                END-IF
140312                IF WS-LINE (MY-SUB :1) = 'e'
140312                   MOVE 'E' TO WS-LINE (MY-SUB :1)
140312                END-IF
140312                IF WS-LINE (MY-SUB :1) = 'f'
140312                   MOVE 'F' TO WS-LINE (MY-SUB :1)
140312                END-IF
140312                IF WS-LINE (MY-SUB :1) = 'g'
140312                   MOVE 'G' TO WS-LINE (MY-SUB :1)
140312                END-IF
140312                IF WS-LINE (MY-SUB :1) = 'h'
140312                   MOVE 'H' TO WS-LINE (MY-SUB :1)
140312                END-IF
140312                IF WS-LINE (MY-SUB :1) = 'i'
140312                   MOVE 'I' TO WS-LINE (MY-SUB :1)
140312                END-IF
140312                IF WS-LINE (MY-SUB :1) = 'j'
140312                   MOVE 'J' TO WS-LINE (MY-SUB :1)
140312                END-IF
140312                IF WS-LINE (MY-SUB :1) = 'k'
140312                   MOVE 'K' TO WS-LINE (MY-SUB :1)
140312                END-IF
140312                IF WS-LINE (MY-SUB :1) = 'l'
140312                   MOVE 'L' TO WS-LINE (MY-SUB :1)
140312                END-IF
140312                IF WS-LINE (MY-SUB :1) = 'm'
140312                   MOVE 'M' TO WS-LINE (MY-SUB :1)
140312                END-IF
140312                IF WS-LINE (MY-SUB :1) = 'n'
140312                   MOVE 'N' TO WS-LINE (MY-SUB :1)
140312                END-IF
140312                IF WS-LINE (MY-SUB :1) = 'o'
140312                   MOVE 'O' TO WS-LINE (MY-SUB :1)
140312                END-IF
140312                IF WS-LINE (MY-SUB :1) = 'p'
140312                   MOVE 'P' TO WS-LINE (MY-SUB :1)
140312                END-IF
140312                IF WS-LINE (MY-SUB :1) = 'q'
140312                   MOVE 'Q' TO WS-LINE (MY-SUB :1)
140312                END-IF
140312                IF WS-LINE (MY-SUB :1) = 'r'
140312                   MOVE 'R' TO WS-LINE (MY-SUB :1)
140312                END-IF
140312                IF WS-LINE (MY-SUB :1) = 's'
140312                   MOVE 'S' TO WS-LINE (MY-SUB :1)
140312                END-IF
140312                IF WS-LINE (MY-SUB :1) = 't'
140312                   MOVE 'T' TO WS-LINE (MY-SUB :1)
140312                END-IF
140312                IF WS-LINE (MY-SUB :1) = 'u'
140312                   MOVE 'U' TO WS-LINE (MY-SUB :1)
140312                END-IF
140312                IF WS-LINE (MY-SUB :1) = 'v'
140312                   MOVE 'V' TO WS-LINE (MY-SUB :1)
140312                END-IF
140312                IF WS-LINE (MY-SUB :1) = 'w'
140312                   MOVE 'W' TO WS-LINE (MY-SUB :1)
140312                END-IF
140312                IF WS-LINE (MY-SUB :1) = 'x'
140312                   MOVE 'X' TO WS-LINE (MY-SUB :1)
140312                END-IF
140312                IF WS-LINE (MY-SUB :1) = 'y'
140312                   MOVE 'Y' TO WS-LINE (MY-SUB :1)
140312                END-IF
140312                IF WS-LINE (MY-SUB :1) = 'z'
140312                   MOVE 'Z' TO WS-LINE (MY-SUB :1)
140312                END-IF
140312             END-PERFORM
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
               DISPLAY 'T1 - WRITE-AND-READ-CBL - ' TCBL-RECORD
               PERFORM READ-TCBL
           END-IF.
       WRITE-AND-READ-CBL-EXIT. EXIT.

220312 DUMMMY SECTION. EXIT.
