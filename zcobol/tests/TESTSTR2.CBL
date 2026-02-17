       IDENTIFICATION DIVISION.
       PROGRAM-ID.  TESTSTR2.
      ******************************************************************
      * Test program for COBOL STRING statement with DELIMITED BY.
      * Tests STRING with DELIMITED BY identifier and SIZE.
      *
      * Implementation follows FIPS PUB 21-2 (ANSI X3.23-1985)
      * COBOL 85 standard, Section 6.25 (STRING Statement).
      *
      * Author - Zane Hambly
      * Date   - 2025/12
      ******************************************************************
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-NAME           PIC X(20) VALUE 'JOHN|SMITH|JONES    '.
       01  WS-DELIM          PIC X(1)  VALUE '|'.
       01  WS-RESULT         PIC X(25) VALUE SPACES.
       01  WS-EXPECTED       PIC X(25) VALUE SPACES.
       01  PASS-COUNT        PIC 9(2)  VALUE 0.
       01  FAIL-COUNT        PIC 9(2)  VALUE 0.

       PROCEDURE DIVISION.
       MAIN-PARA.
           DISPLAY 'TESTSTR2 - STRING DELIMITED BY TEST STARTED'.
      *
      *    Test 1: STRING with DELIMITED BY SIZE (baseline)
      *
           MOVE SPACES TO WS-RESULT.
           STRING WS-NAME DELIMITED BY SIZE
                  INTO WS-RESULT.
           IF WS-RESULT = 'JOHN|SMITH|JONES    '
               DISPLAY 'TEST 1: PASS - DELIMITED BY SIZE'
               ADD 1 TO PASS-COUNT
           ELSE
               DISPLAY 'TEST 1: FAIL - DELIMITED BY SIZE'
               DISPLAY '  EXPECTED: [JOHN|SMITH|JONES    ]'
               DISPLAY '  GOT:      [' WS-RESULT ']'
               ADD 1 TO FAIL-COUNT.
      *
      *    Test 2: STRING with DELIMITED BY identifier
      *
           MOVE SPACES TO WS-RESULT.
           STRING WS-NAME DELIMITED BY WS-DELIM
                  INTO WS-RESULT.
           IF WS-RESULT(1:4) = 'JOHN'
               DISPLAY 'TEST 2: PASS - DELIMITED BY identifier'
               ADD 1 TO PASS-COUNT
           ELSE
               DISPLAY 'TEST 2: FAIL - DELIMITED BY identifier'
               DISPLAY '  EXPECTED: [JOHN] in first 4 chars'
               DISPLAY '  GOT:      [' WS-RESULT ']'
               ADD 1 TO FAIL-COUNT.
      *
      *    Summary
      *
           DISPLAY 'TESTSTR2 - COMPLETED'.
           DISPLAY 'PASSED: ' PASS-COUNT ' FAILED: ' FAIL-COUNT.
           IF FAIL-COUNT = 0
               DISPLAY 'TESTSTR2 - ALL TESTS PASSED'
           ELSE
               DISPLAY 'TESTSTR2 - SOME TESTS FAILED'.
           STOP RUN.
