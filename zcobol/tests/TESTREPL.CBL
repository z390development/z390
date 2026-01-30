       IDENTIFICATION DIVISION.
       PROGRAM-ID.  TESTREPL.
      ******************************************************************
      * Test program for COBOL REPLACE statement implementation.
      * Tests text substitution as per FIPS PUB 21-2 Section XII.
      *
      * Implementation follows FIPS PUB 21-2 (ANSI X3.23-1985)
      * COBOL 85 standard, Compiler-Directing Statements.
      *
      * Tests:
      * 1. REPLACE in open code - variable name substitution
      * 2. REPLACE OFF - stops substitution
      * 3. Multiple REPLACE pairs in one statement
      * 4. COPY REPLACING - text substitution in copy members
      *
      * Author - Zane Hambly
      * Date   - 2025/12
      ******************************************************************
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-RESULT        PIC X(30) VALUE SPACES.

      *    Test 1: Activate REPLACE in open code (AAA becomes BBB)
       REPLACE ==AAA== BY ==BBB==.

      *    These variable names contain AAA which will be replaced
       01  WS-AAA-FIELD     PIC X(20) VALUE 'TEST VALUE 1'.
       01  WS-AAA-DATA      PIC X(20) VALUE 'TEST VALUE 2'.

      *    Test 2: Turn off REPLACE
       REPLACE OFF.

      *    This variable should NOT have AAA replaced
       01  WS-AAA-KEPT      PIC X(15) VALUE 'NOT REPLACED'.

      *    Test 3: Multiple replacement pairs
       REPLACE ==XXX== BY ==FIRST==
               ==YYY== BY ==SECOND==.

       01  WS-XXX-VAR       PIC X(10) VALUE 'MULTI 1'.
       01  WS-YYY-VAR       PIC X(10) VALUE 'MULTI 2'.

       REPLACE OFF.

      *    Test 4: COPY REPLACING - substitution in copy member
           COPY REPLCOPY REPLACING ==:PREFIX:== BY ==WS-CPY==.

       PROCEDURE DIVISION.
       MAIN-PARA.
           DISPLAY 'TESTREPL - COBOL REPLACE TEST STARTED'.

      *    Test 1: Replaced variable names work (AAA became BBB)
           DISPLAY 'TEST 1: ' WS-BBB-FIELD.
           DISPLAY 'TEST 2: ' WS-BBB-DATA.

      *    Test 2: Non-replaced variable works (REPLACE OFF)
           DISPLAY 'TEST 3: ' WS-AAA-KEPT.

      *    Test 3: Multiple replacement pairs
           DISPLAY 'TEST 4: ' WS-FIRST-VAR.
           DISPLAY 'TEST 5: ' WS-SECOND-VAR.

      *    Test 4: COPY REPLACING
           DISPLAY 'TEST 6: ' WS-CPY-NAME.
           DISPLAY 'TEST 7: ' WS-CPY-VALUE.

           DISPLAY 'TESTREPL - ALL TESTS COMPLETED'.
           DISPLAY 'TESTREPL - TEST ENDED OK'.
           STOP RUN.
