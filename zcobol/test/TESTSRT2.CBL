       IDENTIFICATION DIVISION.
       PROGRAM-ID.  TESTSRT2.
      ******************************************************************
      * Test program for COBOL SORT - no ORGANIZATION clause (default)
      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT UNSORTED-FILE
               ASSIGN TO 'zcobol\test\TESTSRT2.IN'.
           SELECT SORTED-FILE
               ASSIGN TO 'zcobol\test\TESTSRT2.OUT'.
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
       PROCEDURE DIVISION.
       MAIN-PARA.
           DISPLAY 'TESTSRT2 - DEFAULT FORMAT SORT TEST'.
           SORT SORT-FILE
               ON ASCENDING KEY SORT-KEY
               USING UNSORTED-FILE
               GIVING SORTED-FILE.
           DISPLAY 'TESTSRT2 - SORT COMPLETED'.
           STOP RUN.

