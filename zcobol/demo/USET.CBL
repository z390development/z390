       IDENTIFICATION DIVISION.
       PROGRAM-ID.  USET.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT IN-FILE
               ASSIGN TO 'ZCOBOL\DEMO\U1.CBL'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUT-FILE
               ASSIGN TO 'ZCOBOL\DEMO\U4.CBL'
               ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD  IN-FILE
           DATA RECORD IS IN-RECORD.
       01  IN-RECORD.
           03  FILLER             PIC X(24).
           03  IN-FILE-NAME       PIC X(46).
           03  FILLER             PIC X(08).  
       FD  OUT-FILE
           DATA RECORD IS OUT-RECORD.
       01  OUT-RECORD             PIC X(80).
       WORKING-STORAGE SECTION.
       01  END-OF-FILE-FLAG       PIC X VALUE 'F'.
       01  THE-FILE-NAME          PIC X(46).
       01  MY-SUB                 PIC 9(02).
       01  SUB                    PIC 9(02).
       PROCEDURE DIVISION.
       MAINLINE.
           PERFORM INITIALIZATION-ROUTINE.
           PERFORM THE-LOOP
                   UNTIL END-OF-FILE-FLAG = 'Y'.
           PERFORM EOJ-ROUTINE.
           STOP RUN.
       INITIALIZATION-ROUTINE.
           OPEN INPUT IN-FILE.
           OPEN OUTPUT OUT-FILE.
           READ IN-FILE INTO OUT-RECORD
                AT END MOVE 'Y' TO END-OF-FILE-FLAG.
           WRITE OUT-RECORD.
           READ IN-FILE       INTO OUT-RECORD
                AT END MOVE 'Y' TO END-OF-FILE-FLAG.
           WRITE OUT-RECORD.
           READ IN-FILE       INTO OUT-RECORD
                AT END MOVE 'Y' TO END-OF-FILE-FLAG.
           WRITE OUT-RECORD.
           READ IN-FILE       INTO OUT-RECORD
                AT END MOVE 'Y' TO END-OF-FILE-FLAG.
           WRITE OUT-RECORD.
           READ IN-FILE       INTO OUT-RECORD
                AT END MOVE 'Y' TO END-OF-FILE-FLAG.
           WRITE OUT-RECORD.
           READ IN-FILE       INTO OUT-RECORD
                AT END MOVE 'Y' TO END-OF-FILE-FLAG.
           WRITE OUT-RECORD.
           READ IN-FILE
                AT END MOVE 'Y' TO END-OF-FILE-FLAG.
           ACCEPT THE-FILE-NAME 
                              FROM CONSOLE.
           PERFORM L-TO-U VARYING MY-SUB FROM 1 BY 1
             UNTIL MY-SUB > 46.
           MOVE THE-FILE-NAME   TO IN-FILE-NAME.
           WRITE OUT-RECORD   FROM IN-RECORD.
           READ IN-FILE       INTO OUT-RECORD
                AT END MOVE 'Y' TO END-OF-FILE-FLAG.
           WRITE OUT-RECORD   FROM IN-RECORD.
           READ IN-FILE       INTO OUT-RECORD
                AT END MOVE 'Y' TO END-OF-FILE-FLAG.
           WRITE OUT-RECORD   FROM IN-RECORD.
           READ IN-FILE       INTO OUT-RECORD
                AT END MOVE 'Y' TO END-OF-FILE-FLAG.
           MOVE THE-FILE-NAME   TO IN-FILE-NAME.
           PERFORM VARYING SUB FROM 1 BY 1 
             UNTIL SUB > 72
                IF IN-RECORD (SUB:3) = 'CBL'
                    MOVE 'PRN'  TO IN-RECORD (SUB:3)
                END-IF
           END-PERFORM.
           WRITE OUT-RECORD   FROM IN-RECORD.
           READ IN-FILE       INTO OUT-RECORD
                AT END MOVE 'Y' TO END-OF-FILE-FLAG.
       THE-LOOP.
           WRITE OUT-RECORD.
           READ IN-FILE       INTO OUT-RECORD
                AT END MOVE 'Y' TO END-OF-FILE-FLAG.
       EOJ-ROUTINE.
           CLOSE IN-FILE.
           CLOSE OUT-FILE.

       L-TO-U.
          IF THE-FILE-NAME (MY-SUB :1) = 'a'
             MOVE 'A' TO THE-FILE-NAME (MY-SUB :1).
          IF THE-FILE-NAME (MY-SUB :1) = 'b'
             MOVE 'B' TO THE-FILE-NAME (MY-SUB :1).
          IF THE-FILE-NAME (MY-SUB :1) = 'c'
             MOVE 'C' TO THE-FILE-NAME (MY-SUB :1).
          IF THE-FILE-NAME (MY-SUB :1) = 'd'
             MOVE 'D' TO THE-FILE-NAME (MY-SUB :1).
          IF THE-FILE-NAME (MY-SUB :1) = 'e'
             MOVE 'E' TO THE-FILE-NAME (MY-SUB :1).
          IF THE-FILE-NAME (MY-SUB :1) = 'f'
             MOVE 'F' TO THE-FILE-NAME (MY-SUB :1).
          IF THE-FILE-NAME (MY-SUB :1) = 'g'
             MOVE 'G' TO THE-FILE-NAME (MY-SUB :1).
          IF THE-FILE-NAME (MY-SUB :1) = 'h'
             MOVE 'H' TO THE-FILE-NAME (MY-SUB :1).
          IF THE-FILE-NAME (MY-SUB :1) = 'i'
             MOVE 'I' TO THE-FILE-NAME (MY-SUB :1).
          IF THE-FILE-NAME (MY-SUB :1) = 'j'
             MOVE 'J' TO THE-FILE-NAME (MY-SUB :1).
          IF THE-FILE-NAME (MY-SUB :1) = 'k'
             MOVE 'K' TO THE-FILE-NAME (MY-SUB :1).
          IF THE-FILE-NAME (MY-SUB :1) = 'l'
             MOVE 'L' TO THE-FILE-NAME (MY-SUB :1).
          IF THE-FILE-NAME (MY-SUB :1) = 'm'
             MOVE 'M' TO THE-FILE-NAME (MY-SUB :1).
          IF THE-FILE-NAME (MY-SUB :1) = 'n'
             MOVE 'N' TO THE-FILE-NAME (MY-SUB :1).
          IF THE-FILE-NAME (MY-SUB :1) = 'o'
             MOVE 'O' TO THE-FILE-NAME (MY-SUB :1).
          IF THE-FILE-NAME (MY-SUB :1) = 'p'
             MOVE 'P' TO THE-FILE-NAME (MY-SUB :1).
          IF THE-FILE-NAME (MY-SUB :1) = 'q'
             MOVE 'Q' TO THE-FILE-NAME (MY-SUB :1).
          IF THE-FILE-NAME (MY-SUB :1) = 'r'
             MOVE 'R' TO THE-FILE-NAME (MY-SUB :1).
          IF THE-FILE-NAME (MY-SUB :1) = 's'
             MOVE 'S' TO THE-FILE-NAME (MY-SUB :1).
          IF THE-FILE-NAME (MY-SUB :1) = 't'
             MOVE 'T' TO THE-FILE-NAME (MY-SUB :1).
          IF THE-FILE-NAME (MY-SUB :1) = 'u'
             MOVE 'U' TO THE-FILE-NAME (MY-SUB :1).
          IF THE-FILE-NAME (MY-SUB :1) = 'v'
             MOVE 'V' TO THE-FILE-NAME (MY-SUB :1).
          IF THE-FILE-NAME (MY-SUB :1) = 'w'
             MOVE 'W' TO THE-FILE-NAME (MY-SUB :1).
          IF THE-FILE-NAME (MY-SUB :1) = 'x'
             MOVE 'X' TO THE-FILE-NAME (MY-SUB :1).
          IF THE-FILE-NAME (MY-SUB :1) = 'y'
             MOVE 'Y' TO THE-FILE-NAME (MY-SUB :1).
          IF THE-FILE-NAME (MY-SUB :1) = 'z'
             MOVE 'Z' TO THE-FILE-NAME (MY-SUB :1).
