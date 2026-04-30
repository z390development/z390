       IDENTIFICATION DIVISION.
       PROGRAM-ID.  TESTSTR3.
      ******************************************************************
      * Test program for COBOL STRING statement WITH POINTER clause.
      *
      * Per FIPS PUB 21-2 Section 6.25:
      * - Pointer specifies position in target where transfer begins
      * - Pointer is incremented by number of characters transferred
      * - Initial pointer value must be >= 1
      *
      * Note: Pointer must be COMP (binary) for this implementation.
      *
      * Author - Zane Hambly
      * Date   - 2025/12
      ******************************************************************
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-FIRST        PIC X(5)  VALUE 'HELLO'.
       01  WS-SECOND       PIC X(5)  VALUE 'WORLD'.
       01  WS-RESULT       PIC X(20) VALUE SPACES.
       01  WS-PTR          PIC 9(9)  COMP VALUE 1.
       01  PASS-COUNT      PIC 9(2)  VALUE 0.
       01  FAIL-COUNT      PIC 9(2)  VALUE 0.

       PROCEDURE DIVISION.
       MAIN-PARA.
           DISPLAY 'TESTSTR3 - STRING WITH POINTER TEST'.
      *
      *    Test 1: STRING with POINTER starting at 1
      *
           MOVE SPACES TO WS-RESULT.
           MOVE 1 TO WS-PTR.
           STRING WS-FIRST DELIMITED BY SIZE
                  INTO WS-RESULT
                  WITH POINTER WS-PTR.
           DISPLAY 'TEST 1: RESULT=[' WS-RESULT '] PTR=' WS-PTR.
           IF WS-RESULT(1:5) = 'HELLO' AND WS-PTR = 6
               DISPLAY 'TEST 1: PASS - POINTER starts at 1'
               ADD 1 TO PASS-COUNT
           ELSE
               DISPLAY 'TEST 1: FAIL'
               DISPLAY '  EXPECTED RESULT: [HELLO...] PTR=6'
               ADD 1 TO FAIL-COUNT.
      *
      *    Test 2: STRING with POINTER continuing from previous
      *            (simulates multiple STRING operations)
      *
           STRING WS-SECOND DELIMITED BY SIZE
                  INTO WS-RESULT
                  WITH POINTER WS-PTR.
           DISPLAY 'TEST 2: RESULT=[' WS-RESULT '] PTR=' WS-PTR.
           IF WS-RESULT(1:10) = 'HELLOWORLD' AND WS-PTR = 11
               DISPLAY 'TEST 2: PASS - POINTER continues correctly'
               ADD 1 TO PASS-COUNT
           ELSE
               DISPLAY 'TEST 2: FAIL'
               DISPLAY '  EXPECTED RESULT: [HELLOWORLD...] PTR=11'
               ADD 1 TO FAIL-COUNT.
      *
      *    Test 3: STRING with POINTER starting at offset position
      *
           MOVE SPACES TO WS-RESULT.
           MOVE 6 TO WS-PTR.
           STRING WS-FIRST DELIMITED BY SIZE
                  INTO WS-RESULT
                  WITH POINTER WS-PTR.
           DISPLAY 'TEST 3: RESULT=[' WS-RESULT '] PTR=' WS-PTR.
           IF WS-RESULT(6:5) = 'HELLO' AND WS-PTR = 11
               DISPLAY 'TEST 3: PASS - POINTER offset start'
               ADD 1 TO PASS-COUNT
           ELSE
               DISPLAY 'TEST 3: FAIL'
               DISPLAY '  EXPECTED: spaces then HELLO at pos 6'
               ADD 1 TO FAIL-COUNT.
      *
      *    Summary
      *
           DISPLAY 'TESTSTR3 - COMPLETED'.
           DISPLAY 'PASSED: ' PASS-COUNT ' FAILED: ' FAIL-COUNT.
           IF FAIL-COUNT = 0
               DISPLAY 'TESTSTR3 - ALL TESTS PASSED'
           ELSE
               DISPLAY 'TESTSTR3 - SOME TESTS FAILED'.
           STOP RUN.
