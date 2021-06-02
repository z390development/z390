       IDENTIFICATION DIVISION.
       PROGRAM-ID.    TESTIF3.
       AUTHOR.        DON HIGGINS.
       DATE-WRITTEN.  04/12/12.
      * TEST IF WITH (,,,) AND/OR (....)                                   
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  A PIC X VALUE 'A'.
       77  B PIC X VALUE 'B'.
       77  C PIC X VALUE 'C'.
       77  D PIC X VALUE 'D'.
       77  ERRORS  PIC X VALUE 'F'.
       77  GOOD COMP-3 PIC S9(4) VALUE 0.
       PROCEDURE DIVISION.
       MAINLINE.
           DISPLAY 'TESTIF3 STARTING'
     1     IF (A = 'A' OR B = ' ') AND (A = ' ' OR B = 'B') 
               DISPLAY 'TEST 1'
               PERFORM GOOD ELSE PERFORM BAD.
     2     IF NOT ((A = ' ' OR B = ' ') AND (A = ' ' OR B = 'B'))
               DISPLAY 'TEST 2'
               PERFORM GOOD ELSE PERFORM BAD.
     3     IF (A = 'A' AND B = 'B') AND (C = 'C' AND D = 'D')
               DISPLAY 'TEST 3'
               PERFORM GOOD ELSE PERFORM BAD.
     4     IF (A = 'A' AND B = 'B') OR (C = 'C' AND D = 'D')
                DISPLAY 'TEST 4'
                PERFORM GOOD ELSE PERFORM BAD.
     5     IF NOT ((A = 'A' AND B = ' ') OR (C = 'C' AND D = ' '))
                DISPLAY 'TEST 5'
                PERFORM GOOD ELSE PERFORM BAD.    
     6     IF (A = 'A' OR B = ' ') AND (C = ' ' OR D = 'D') 
               DISPLAY 'TEST 6'
               PERFORM GOOD ELSE PERFORM BAD.
           DISPLAY 'TESTIF2 GOOD TESTS =' GOOD
           STOP RUN.
       GOOD.
           ADD 1 TO GOOD.
       BAD.
           DISPLAY 'ERROR AFTER ' GOOD  ' GOOD TESTS'
           MOVE 16 TO RETURN-CODE
           STOP RUN.
