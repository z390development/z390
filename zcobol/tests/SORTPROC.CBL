       IDENTIFICATION DIVISION.
       PROGRAM-ID.  SORTPROC.
       AUTHOR.      ZANE HAMBLY.
       DATE-WRITTEN. 02/13/26.
      ******************************************************************
      * SORT with INPUT PROCEDURE and OUTPUT PROCEDURE.
      * Exercises RELEASE (feed records to sort) and RETURN (read
      * sorted records back). Same 5-record data as SORTASC.
      *
      * Helper logic is in separate sections to avoid inline
      * fall-through during section-level PERFORM.
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
       01  WS-EOF-FLAG       PIC X VALUE 'N'.
       01  WS-RECORD-COUNT   PIC 9(4) COMP VALUE 0.
       01  WS-PREV-KEY       PIC X(10) VALUE LOW-VALUES.
       01  WS-CURR-KEY       PIC X(10).
       01  WS-ERROR-FLAG     PIC X VALUE 'N'.
       PROCEDURE DIVISION.
       MAIN-PARA.
           DISPLAY 'SORTPROC - INPUT/OUTPUT PROCEDURE TEST'.
           SORT SORT-FILE
               ON ASCENDING KEY SORT-KEY
               INPUT PROCEDURE IS FEED-RECORDS
               OUTPUT PROCEDURE IS RETRIEVE-RECORDS.
           DISPLAY 'SORTPROC - SORT COMPLETED'.
           IF WS-ERROR-FLAG = 'Y'
               DISPLAY 'SORTPROC - ERROR: OUTPUT NOT SORTED'
               STOP RUN
           END-IF.
           DISPLAY 'SORTPROC - VERIFIED ' WS-RECORD-COUNT
               ' RECORDS'.
           DISPLAY 'SORTPROC - TEST ENDED OK'.
           STOP RUN.
      *
       FEED-RECORDS SECTION.
           OPEN INPUT UNSORTED-FILE.
           MOVE 'N' TO WS-EOF-FLAG.
           PERFORM READ-AND-RELEASE
               UNTIL WS-EOF-FLAG = 'Y'.
           CLOSE UNSORTED-FILE.
       FEED-RECORDS-EXIT.
           EXIT.
      *
       READ-AND-RELEASE SECTION.
           READ UNSORTED-FILE INTO UNSORTED-REC
               AT END MOVE 'Y' TO WS-EOF-FLAG.
           IF WS-EOF-FLAG NOT = 'Y'
               RELEASE SORT-REC FROM UNSORTED-REC
           END-IF.
       READ-AND-RELEASE-EXIT.
           EXIT.
      *
       RETRIEVE-RECORDS SECTION.
           OPEN OUTPUT SORTED-FILE.
           MOVE 'N' TO WS-EOF-FLAG.
           MOVE LOW-VALUES TO WS-PREV-KEY.
           MOVE 0 TO WS-RECORD-COUNT.
           PERFORM RETURN-AND-WRITE
               UNTIL WS-EOF-FLAG = 'Y'.
           CLOSE SORTED-FILE.
       RETRIEVE-RECORDS-EXIT.
           EXIT.
      *
       RETURN-AND-WRITE SECTION.
           RETURN SORT-FILE RECORD INTO SORTED-REC
               AT END MOVE 'Y' TO WS-EOF-FLAG.
           IF WS-EOF-FLAG NOT = 'Y'
               ADD 1 TO WS-RECORD-COUNT
               MOVE SR-KEY TO WS-CURR-KEY
               IF WS-CURR-KEY < WS-PREV-KEY
                   DISPLAY 'OUT OF ORDER: ' WS-CURR-KEY
                   MOVE 'Y' TO WS-ERROR-FLAG
               END-IF
               MOVE WS-CURR-KEY TO WS-PREV-KEY
               WRITE SORTED-REC
           END-IF.
       RETURN-AND-WRITE-EXIT.
           EXIT.
