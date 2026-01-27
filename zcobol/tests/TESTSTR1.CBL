       IDENTIFICATION DIVISION.
       PROGRAM-ID.  TESTSTR1.
      ******************************************************************
      * Test program for COBOL STRING statement implementation.
      * Tests basic STRING with DELIMITED BY SIZE.
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
       01  WS-FIRST-NAME    PIC X(10) VALUE 'JOHN      '.
       01  WS-LAST-NAME     PIC X(10) VALUE 'SMITH     '.
       01  WS-RESULT        PIC X(25) VALUE SPACES.
       
       PROCEDURE DIVISION.
       MAIN-PARA.
           DISPLAY 'TESTSTR1 - COBOL STRING TEST STARTED'.
      *
      *    Test: STRING with DELIMITED BY SIZE
      *
           STRING WS-FIRST-NAME DELIMITED BY SIZE
                  INTO WS-RESULT.
           DISPLAY 'RESULT: [' WS-RESULT ']'.
           IF WS-RESULT(1:10) = 'JOHN      '
               DISPLAY 'TEST 1: PASS - STRING COPIED CORRECTLY'
           ELSE
               DISPLAY 'TEST 1: FAIL - STRING NOT COPIED CORRECTLY'.
           DISPLAY 'TESTSTR1 - STRING TEST COMPLETED'.
           DISPLAY 'TESTSTR1 - TEST ENDED OK'.
           STOP RUN.
