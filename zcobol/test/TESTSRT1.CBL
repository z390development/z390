       IDENTIFICATION DIVISION.
       PROGRAM-ID.  TESTSRT1.
      ******************************************************************
      * Test program for COBOL SORT statement implementation.
      * Tests basic SORT with USING/GIVING files.
      *
      * Implementation follows FIPS PUB 21-2 (ANSI X3.23-1985)
      * COBOL 85 standard, Section XI (Sort-Merge).
      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT UNSORTED-FILE
               ASSIGN TO 'zcobol\test\TESTSRT1.IN'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT SORTED-FILE
               ASSIGN TO 'zcobol\test\TESTSRT1.OUT'
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
       PROCEDURE DIVISION.
       MAIN-PARA.
           DISPLAY 'TESTSRT1 - COBOL SORT TEST STARTED'.
           DISPLAY 'SORTING RECORDS BY KEY ASCENDING'.
      *
      *    SORT USING/GIVING - SIMPLEST FORM
      *
           SORT SORT-FILE
               ON ASCENDING KEY SORT-KEY
               USING UNSORTED-FILE
               GIVING SORTED-FILE.
      *
           DISPLAY 'TESTSRT1 - SORT COMPLETED'.
           DISPLAY 'TESTSRT1 - TEST ENDED OK'.
           STOP RUN.

