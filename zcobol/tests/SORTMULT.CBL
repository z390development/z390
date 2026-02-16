       IDENTIFICATION DIVISION.
       PROGRAM-ID.  SORTMULT.
       AUTHOR.      ZANE HAMBLY.
       DATE-WRITTEN. 02/12/26.
      ******************************************************************
      * Multiple SORTs in one programme - ascending then descending
      * on the same input, same SD. Proves label generation (&SORT_LAB)
      * handles repeated SORT expansion without collision.
      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT UNSORTED-FILE
               ASSIGN TO INFILE
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT SORTED-ASC-FILE
               ASSIGN TO ASCFILE
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT SORTED-DESC-FILE
               ASSIGN TO DESCFILE
               ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD  UNSORTED-FILE
           DATA RECORD IS UNSORTED-REC.
       01  UNSORTED-REC.
           05  UR-KEY        PIC X(10).
           05  UR-DATA       PIC X(70).
       FD  SORTED-ASC-FILE
           DATA RECORD IS SORTED-ASC-REC.
       01  SORTED-ASC-REC.
           05  SA-KEY        PIC X(10).
           05  SA-DATA       PIC X(70).
       FD  SORTED-DESC-FILE
           DATA RECORD IS SORTED-DESC-REC.
       01  SORTED-DESC-REC.
           05  SD-KEY        PIC X(10).
           05  SD-DATA       PIC X(70).
       SD  SORT-FILE
           DATA RECORD IS SORT-REC.
       01  SORT-REC.
           05  SORT-KEY      PIC X(10).
           05  SORT-DATA     PIC X(70).
       WORKING-STORAGE SECTION.
       01  WS-RECORD-COUNT   PIC 9(4) COMP VALUE 0.
       01  WS-EOF-FLAG       PIC X VALUE 'N'.
       01  WS-PREV-KEY       PIC X(10) VALUE LOW-VALUES.
       01  WS-CURR-KEY       PIC X(10).
       01  WS-ERROR-FLAG     PIC X VALUE 'N'.
       PROCEDURE DIVISION.
       MAIN-PARA.
           DISPLAY 'SORTMULT - MULTI-SORT TEST STARTED'.
           DISPLAY 'SORTMULT - SORT 1: ASCENDING'.
           SORT SORT-FILE
               ON ASCENDING KEY SORT-KEY
               USING UNSORTED-FILE
               GIVING SORTED-ASC-FILE.
           DISPLAY 'SORTMULT - SORT 1 COMPLETED'.
           PERFORM VERIFY-ASC-ORDER.
           IF WS-ERROR-FLAG = 'Y'
               DISPLAY 'SORTMULT - ERROR: ASC OUTPUT NOT SORTED'
               STOP RUN
           END-IF.
           DISPLAY 'SORTMULT - ASC VERIFIED ' WS-RECORD-COUNT
               ' RECORDS'.
           DISPLAY 'SORTMULT - SORT 2: DESCENDING'.
           SORT SORT-FILE
               ON DESCENDING KEY SORT-KEY
               USING UNSORTED-FILE
               GIVING SORTED-DESC-FILE.
           DISPLAY 'SORTMULT - SORT 2 COMPLETED'.
           PERFORM VERIFY-DESC-ORDER.
           IF WS-ERROR-FLAG = 'Y'
               DISPLAY 'SORTMULT - ERROR: DESC OUTPUT NOT SORTED'
               STOP RUN
           END-IF.
           DISPLAY 'SORTMULT - DESC VERIFIED ' WS-RECORD-COUNT
               ' RECORDS'.
      *
           DISPLAY 'SORTMULT - TEST ENDED OK'.
           STOP RUN.
      *
       VERIFY-ASC-ORDER.
           OPEN INPUT SORTED-ASC-FILE.
           MOVE 'N' TO WS-EOF-FLAG.
           MOVE LOW-VALUES TO WS-PREV-KEY.
           MOVE 0 TO WS-RECORD-COUNT.
           PERFORM READ-ASC UNTIL WS-EOF-FLAG = 'Y'.
           CLOSE SORTED-ASC-FILE.
      *
       READ-ASC.
           READ SORTED-ASC-FILE INTO SORTED-ASC-REC
               AT END MOVE 'Y' TO WS-EOF-FLAG.
           IF WS-EOF-FLAG NOT = 'Y'
               ADD 1 TO WS-RECORD-COUNT
               MOVE SA-KEY TO WS-CURR-KEY
               IF WS-CURR-KEY < WS-PREV-KEY
                   DISPLAY 'ASC OUT OF ORDER: ' WS-CURR-KEY
                   MOVE 'Y' TO WS-ERROR-FLAG
               END-IF
               MOVE WS-CURR-KEY TO WS-PREV-KEY
           END-IF.
      *
       VERIFY-DESC-ORDER.
           OPEN INPUT SORTED-DESC-FILE.
           MOVE 'N' TO WS-EOF-FLAG.
           MOVE HIGH-VALUES TO WS-PREV-KEY.
           MOVE 0 TO WS-RECORD-COUNT.
           PERFORM READ-DESC UNTIL WS-EOF-FLAG = 'Y'.
           CLOSE SORTED-DESC-FILE.
      *
       READ-DESC.
           READ SORTED-DESC-FILE INTO SORTED-DESC-REC
               AT END MOVE 'Y' TO WS-EOF-FLAG.
           IF WS-EOF-FLAG NOT = 'Y'
               ADD 1 TO WS-RECORD-COUNT
               MOVE SD-KEY TO WS-CURR-KEY
               IF WS-CURR-KEY > WS-PREV-KEY
                   DISPLAY 'DESC OUT OF ORDER: ' WS-CURR-KEY
                   MOVE 'Y' TO WS-ERROR-FLAG
               END-IF
               MOVE WS-CURR-KEY TO WS-PREV-KEY
           END-IF.
