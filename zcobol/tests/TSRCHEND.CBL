       IDENTIFICATION DIVISION.
       PROGRAM-ID.  TSRCHEND.
      ******************************************************************
      * Repro for issue #825 - SEARCH not generating AT END label.
      * Mirrors NIST NC401M: SEARCH with AT END plus a WHEN condition
      * written with COBOL relational noise words (IS EQUAL TO).
      *
      * Expected (after fix): translates, assembles, runs, AT END path
      * taken for a missing key, WHEN path taken for a present key.
      ******************************************************************
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  EMPLOYEE-TABLE.
           05  EMP-ENTRY OCCURS 5 TIMES INDEXED BY EMP-IX.
               10  EMP-ID    PIC 9(3).
               10  EMP-NAME  PIC X(10).
       01  PASS-COUNT     PIC 9(2)    VALUE 0.
       01  FAIL-COUNT     PIC 9(2)    VALUE 0.
       01  WS-FOUND       PIC 9       VALUE 0.
       01  SEARCH-ID      PIC 9(3)    VALUE 0.

       PROCEDURE DIVISION.
       MAIN-PARA.
           DISPLAY 'TSRCHEND - SEARCH AT END NOISE-WORD TEST'.
           MOVE 101 TO EMP-ID(1).
           MOVE 102 TO EMP-ID(2).
           MOVE 103 TO EMP-ID(3).
           MOVE 104 TO EMP-ID(4).
           MOVE 105 TO EMP-ID(5).
      *
      *    Test 1: present key, WHEN ... IS EQUAL TO ...
      *
           MOVE 0 TO WS-FOUND.
           MOVE 103 TO SEARCH-ID.
           SET EMP-IX TO 1.
           SEARCH EMP-ENTRY
               AT END
                   MOVE 9 TO WS-FOUND
               WHEN EMP-ID(EMP-IX) IS EQUAL TO SEARCH-ID
                   MOVE 1 TO WS-FOUND
           END-SEARCH.
           IF WS-FOUND = 1
               DISPLAY 'TEST 1: PASS - FOUND ID 103'
               ADD 1 TO PASS-COUNT
           ELSE
               DISPLAY 'TEST 1: FAIL - DID NOT FIND ID 103'
               ADD 1 TO FAIL-COUNT.
      *
      *    Test 2: missing key, AT END must fire
      *
           MOVE 0 TO WS-FOUND.
           MOVE 999 TO SEARCH-ID.
           SET EMP-IX TO 1.
           SEARCH EMP-ENTRY
               AT END
                   MOVE 2 TO WS-FOUND
               WHEN EMP-ID(EMP-IX) IS EQUAL TO SEARCH-ID
                   MOVE 1 TO WS-FOUND
           END-SEARCH.
           IF WS-FOUND = 2
               DISPLAY 'TEST 2: PASS - AT END EXECUTED FOR ID 999'
               ADD 1 TO PASS-COUNT
           ELSE
               DISPLAY 'TEST 2: FAIL - AT END NOT EXECUTED'
               ADD 1 TO FAIL-COUNT.
           DISPLAY 'TSRCHEND - COMPLETED'.
           DISPLAY 'PASSED: ' PASS-COUNT ' FAILED: ' FAIL-COUNT.
           IF FAIL-COUNT = 0
               DISPLAY 'TSRCHEND - ALL TESTS PASSED'
           ELSE
               DISPLAY 'TSRCHEND - SOME TESTS FAILED'
               MOVE 8 TO RETURN-CODE.
           STOP RUN.
