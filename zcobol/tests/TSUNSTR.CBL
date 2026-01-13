       IDENTIFICATION DIVISION.
       PROGRAM-ID. TSUNSTR.
      *****************************************************************
      * TSUNSTR - Test COBOL UNSTRING Statement
      * Author: Zane Hambly
      * Date: December 2025
      * Reference: FIPS PUB 21-2 Section VI-6.27
      *****************************************************************
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *
      * Test 1: Basic UNSTRING with comma delimiter
      *
       01  WS-SOURCE-1           PIC X(20) VALUE 'JOHN,DOE,SMITH'.
       01  WS-FIRST-NAME         PIC X(10) VALUE SPACES.
       01  WS-MIDDLE-NAME        PIC X(10) VALUE SPACES.
       01  WS-LAST-NAME          PIC X(10) VALUE SPACES.
      *
      * Test 2: UNSTRING with POINTER
      *
       01  WS-SOURCE-2           PIC X(30) VALUE 'ABC:DEF:GHI:JKL'.
       01  WS-FIELD-1            PIC X(5) VALUE SPACES.
       01  WS-FIELD-2            PIC X(5) VALUE SPACES.
       01  WS-POINTER            PIC S9(9) COMP VALUE 1.
      *
      * Test 3: UNSTRING with TALLYING
      *
       01  WS-SOURCE-3           PIC X(20) VALUE 'ONE|TWO|THREE'.
       01  WS-OUT-1              PIC X(10) VALUE SPACES.
       01  WS-OUT-2              PIC X(10) VALUE SPACES.
       01  WS-OUT-3              PIC X(10) VALUE SPACES.
       01  WS-TALLY-COUNT        PIC S9(9) COMP VALUE 0.
      *
      * Test result fields
      *
       01  WS-PASS-COUNT         PIC 9(4) VALUE 0.
       01  WS-FAIL-COUNT         PIC 9(4) VALUE 0.
      *
       PROCEDURE DIVISION.
       MAIN-PARA.
           DISPLAY 'TSUNSTR - COBOL UNSTRING TEST STARTED'.
           DISPLAY '========================================'.
      *
      * Test 1: Basic UNSTRING with comma delimiter
      *
           DISPLAY ' '.
           DISPLAY 'TEST 1: Basic UNSTRING with comma delimiter'.
           DISPLAY 'SOURCE: [' WS-SOURCE-1 ']'.

           UNSTRING WS-SOURCE-1 DELIMITED BY ','
               INTO WS-FIRST-NAME WS-MIDDLE-NAME WS-LAST-NAME.

           DISPLAY 'FIRST:  [' WS-FIRST-NAME ']'.
           DISPLAY 'MIDDLE: [' WS-MIDDLE-NAME ']'.
           DISPLAY 'LAST:   [' WS-LAST-NAME ']'.

           IF WS-FIRST-NAME = 'JOHN      '
               DISPLAY 'TEST 1 FIRST NAME: PASS'
               ADD 1 TO WS-PASS-COUNT
           ELSE
               DISPLAY 'TEST 1 FIRST NAME: FAIL'
               ADD 1 TO WS-FAIL-COUNT
           END-IF.

           IF WS-MIDDLE-NAME = 'DOE       '
               DISPLAY 'TEST 1 MIDDLE NAME: PASS'
               ADD 1 TO WS-PASS-COUNT
           ELSE
               DISPLAY 'TEST 1 MIDDLE NAME: FAIL'
               ADD 1 TO WS-FAIL-COUNT
           END-IF.

           IF WS-LAST-NAME = 'SMITH     '
               DISPLAY 'TEST 1 LAST NAME: PASS'
               ADD 1 TO WS-PASS-COUNT
           ELSE
               DISPLAY 'TEST 1 LAST NAME: FAIL'
               ADD 1 TO WS-FAIL-COUNT
           END-IF.
      *
      * Test 2: UNSTRING with POINTER
      *
           DISPLAY ' '.
           DISPLAY 'TEST 2: UNSTRING with POINTER'.
           DISPLAY 'SOURCE: [' WS-SOURCE-2 ']'.
           MOVE 5 TO WS-POINTER.

           UNSTRING WS-SOURCE-2 DELIMITED BY ':'
               INTO WS-FIELD-1 WS-FIELD-2
               WITH POINTER WS-POINTER.

           DISPLAY 'FIELD-1: [' WS-FIELD-1 ']'.
           DISPLAY 'FIELD-2: [' WS-FIELD-2 ']'.
           DISPLAY 'POINTER: ' WS-POINTER.

           IF WS-FIELD-1 = 'DEF  '
               DISPLAY 'TEST 2 FIELD-1: PASS'
               ADD 1 TO WS-PASS-COUNT
           ELSE
               DISPLAY 'TEST 2 FIELD-1: FAIL - Expected DEF'
               ADD 1 TO WS-FAIL-COUNT
           END-IF.
      *
      * Test 3: UNSTRING with TALLYING
      *
           DISPLAY ' '.
           DISPLAY 'TEST 3: UNSTRING with TALLYING'.
           DISPLAY 'SOURCE: [' WS-SOURCE-3 ']'.
           MOVE 0 TO WS-TALLY-COUNT.

           UNSTRING WS-SOURCE-3 DELIMITED BY '|'
               INTO WS-OUT-1 WS-OUT-2 WS-OUT-3
               TALLYING IN WS-TALLY-COUNT.

           DISPLAY 'OUT-1: [' WS-OUT-1 ']'.
           DISPLAY 'OUT-2: [' WS-OUT-2 ']'.
           DISPLAY 'OUT-3: [' WS-OUT-3 ']'.
           DISPLAY 'TALLY: ' WS-TALLY-COUNT.

           IF WS-TALLY-COUNT = 3
               DISPLAY 'TEST 3 TALLYING: PASS'
               ADD 1 TO WS-PASS-COUNT
           ELSE
               DISPLAY 'TEST 3 TALLYING: FAIL - Expected 3'
               ADD 1 TO WS-FAIL-COUNT
           END-IF.
      *
      * Summary
      *
           DISPLAY ' '.
           DISPLAY '========================================'.
           DISPLAY 'TSUNSTR - UNSTRING TEST COMPLETED'.
           DISPLAY 'PASSED: ' WS-PASS-COUNT.
           DISPLAY 'FAILED: ' WS-FAIL-COUNT.

           IF WS-FAIL-COUNT = 0
               DISPLAY 'TSUNSTR - ALL TESTS PASSED'
           ELSE
               DISPLAY 'TSUNSTR - SOME TESTS FAILED'
           END-IF.

           STOP RUN.
