       IDENTIFICATION DIVISION.
       PROGRAM-ID.  SORTASC.
       AUTHOR.      ZANE HAMBLY.
       DATE-WRITTEN. 12/17/25.
      ******************************************************************
      * Basic SORT with USING/GIVING on LINE SEQUENTIAL files.
      * Reads 5 unsorted records, sorts ascending by key, verifies.
      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT UNSORTED-FILE
               ASSIGN TO INFILE
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT SORTED-FILE
               ASSIGN TO OUTFILE
               ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD  UNSORTED-FILE
           DATA RECORD IS UNSORTED-REC.
       01  UNSORTED-REC.
           05  UR-KEY        PIC X(10).
           05  UR-DATA       PIC X(70).
       FD  SORTED-FILE
           DATA RECORD IS SORTED-REC.
       01  SORTED-REC.
           05  SR-KEY        PIC X(10).
           05  SR-DATA       PIC X(70).
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
           DISPLAY 'SORTASC - COBOL SORT TEST STARTED'.
           DISPLAY 'SORTING RECORDS BY KEY ASCENDING'.
           SORT SORT-FILE
               ON ASCENDING KEY SORT-KEY
               USING UNSORTED-FILE
               GIVING SORTED-FILE.
      *
           DISPLAY 'SORTASC - SORT COMPLETED'.
           PERFORM VERIFY-SORT-ORDER.
           IF WS-ERROR-FLAG = 'Y'
               DISPLAY 'SORTASC - ERROR: OUTPUT NOT SORTED'
               STOP RUN
           END-IF.
           DISPLAY 'SORTASC - VERIFIED ' WS-RECORD-COUNT ' RECORDS'.
           DISPLAY 'SORTASC - TEST ENDED OK'.
           STOP RUN.
      *
       VERIFY-SORT-ORDER.
           OPEN INPUT SORTED-FILE.
           MOVE 'N' TO WS-EOF-FLAG.
           MOVE LOW-VALUES TO WS-PREV-KEY.
           MOVE 0 TO WS-RECORD-COUNT.
           PERFORM READ-AND-CHECK UNTIL WS-EOF-FLAG = 'Y'.
           CLOSE SORTED-FILE.
      *
       READ-AND-CHECK.
           READ SORTED-FILE INTO SORTED-REC
               AT END MOVE 'Y' TO WS-EOF-FLAG.
           IF WS-EOF-FLAG NOT = 'Y'
               ADD 1 TO WS-RECORD-COUNT
               MOVE SR-KEY TO WS-CURR-KEY
               IF WS-CURR-KEY < WS-PREV-KEY
                   DISPLAY 'OUT OF ORDER: ' WS-CURR-KEY
                   MOVE 'Y' TO WS-ERROR-FLAG
               END-IF
               MOVE WS-CURR-KEY TO WS-PREV-KEY
           END-IF.
