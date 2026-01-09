       IDENTIFICATION DIVISION.
       PROGRAM-ID.  TESTREPL.
      ******************************************************************
      * Test program for COBOL REPLACE statement implementation.
      * Tests text substitution as per FIPS PUB 21-2 Section XII.
      *
      * Implementation follows FIPS PUB 21-2 (ANSI X3.23-1985)
      * COBOL 85 standard, Compiler-Directing Statements.
      *
      * Author - Zane Hambly
      * Date   - 2025/12
      ******************************************************************
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-RESULT        PIC X(30) VALUE SPACES.
       
      *    Activate REPLACE: AAA becomes BBB
       REPLACE ==AAA== BY ==BBB==.
       
      *    These variable names contain AAA which will be replaced 
       01  WS-AAA-FIELD     PIC X(20) VALUE 'TEST VALUE 1'.
       01  WS-AAA-DATA      PIC X(20) VALUE 'TEST VALUE 2'.
       
      *    Turn off REPLACE before defining this variable
       REPLACE OFF.
       
      *    This variable should NOT have AAA replaced
       01  WS-AAA-KEPT      PIC X(15) VALUE 'NOT REPLACED'.
       
       PROCEDURE DIVISION.
       MAIN-PARA.
           DISPLAY 'TESTREPL - COBOL REPLACE TEST STARTED'.
           
      *    Test 1: Replaced variable names work
           DISPLAY 'TEST 1: ' WS-BBB-FIELD.
           DISPLAY 'TEST 2: ' WS-BBB-DATA.
           
      *    Test 3: Non-replaced variable works  
           DISPLAY 'TEST 3: ' WS-AAA-KEPT.
           
           DISPLAY 'TESTREPL - ALL TESTS COMPLETED'.
           DISPLAY 'TESTREPL - TEST ENDED OK'.
           STOP RUN.
