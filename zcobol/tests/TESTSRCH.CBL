       IDENTIFICATION DIVISION.
       PROGRAM-ID.  TESTSRCH.
      ******************************************************************
      * Test program for SEARCH statement (Format 1 - Sequential).
      * Tests per FIPS PUB 21-2 and COBOL-85 specification.
      *
      * Author - Zane Hambly
      * Date   - 2025/12
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
           DISPLAY 'TESTSRCH - SEARCH STATEMENT TEST'.
      *
      *    Initialize table
      *
           MOVE 101 TO EMP-ID(1).
           MOVE 'ALICE' TO EMP-NAME(1).
           MOVE 102 TO EMP-ID(2).
           MOVE 'BOB' TO EMP-NAME(2).
           MOVE 103 TO EMP-ID(3).
           MOVE 'CHARLIE' TO EMP-NAME(3).
           MOVE 104 TO EMP-ID(4).
           MOVE 'DIANA' TO EMP-NAME(4).
           MOVE 105 TO EMP-ID(5).
           MOVE 'EVE' TO EMP-NAME(5).
      *
      *    Test 1: Search for existing entry (ID 103)
      *
           MOVE 0 TO WS-FOUND.
           MOVE 103 TO SEARCH-ID.
           SET EMP-IX TO 1.
           SEARCH EMP-ENTRY
               AT END
                   MOVE 0 TO WS-FOUND
               WHEN EMP-ID(EMP-IX) = SEARCH-ID
                   MOVE 1 TO WS-FOUND
           END-SEARCH.
           IF WS-FOUND = 1
               DISPLAY 'TEST 1: PASS - FOUND ID 103'
               ADD 1 TO PASS-COUNT
           ELSE
               DISPLAY 'TEST 1: FAIL - DID NOT FIND ID 103'
               ADD 1 TO FAIL-COUNT.
      *
      *    Test 2: Search for non-existing entry (ID 999)
      *
           MOVE 0 TO WS-FOUND.
           MOVE 999 TO SEARCH-ID.
           SET EMP-IX TO 1.
           SEARCH EMP-ENTRY
               AT END
                   MOVE 2 TO WS-FOUND
               WHEN EMP-ID(EMP-IX) = SEARCH-ID
                   MOVE 1 TO WS-FOUND
           END-SEARCH.
           IF WS-FOUND = 2
               DISPLAY 'TEST 2: PASS - AT END EXECUTED FOR ID 999'
               ADD 1 TO PASS-COUNT
           ELSE
               DISPLAY 'TEST 2: FAIL - AT END NOT EXECUTED'
               ADD 1 TO FAIL-COUNT.
      *
      *    Summary
      *
           DISPLAY 'TESTSRCH - COMPLETED'.
           DISPLAY 'PASSED: ' PASS-COUNT ' FAILED: ' FAIL-COUNT.
           IF FAIL-COUNT = 0
               DISPLAY 'TESTSRCH - ALL TESTS PASSED'
           ELSE
               DISPLAY 'TESTSRCH - SOME TESTS FAILED'.
           STOP RUN.
