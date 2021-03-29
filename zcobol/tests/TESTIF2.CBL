       IDENTIFICATION DIVISION.
       PROGRAM-ID.    TESTIF2.
       AUTHOR.        DON HIGGINS.
       DATE-WRITTEN.  08/15/09.
      * TEST COMPLETE IF WITH OMITTED OPERANDS, OPTIMIZED OR, ETC.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  A PIC X VALUE 'A'.
       77  B PIC X VALUE 'B'.
       77  C PIC X VALUE 'C'.
       77  D PIC X VALUE 'A'.
       77  ERRORS  PIC X VALUE 'F'.
       77  GOOD COMP-3 PIC S9(4) VALUE 0.
       PROCEDURE DIVISION.
       MAINLINE.
           DISPLAY 'TESTIF2 STARTING'
     1     IF A = B OR > 'B' OR < 'B' 
           DISPLAY 'A=B OR > ''B'' OR < ''B''' 
           PERFORM GOOD ELSE PERFORM BAD.
     2     IF A = B OR 'C' OR 'A' 
           DISPLAY 'A=B OR ''C'' OR ''A''' 
           PERFORM GOOD ELSE PERFORM BAD.
     3     IF A = B OR C OR D 
           DISPLAY 'A=B OR C OR D' 
           PERFORM GOOD ELSE PERFORM BAD.
     4     IF A = B OR A = C OR A = D 
           DISPLAY 'A=B OR A=C OR A=D'     
           PERFORM GOOD ELSE PERFORM BAD.
           DISPLAY 'TESTIF2 GOOD TESTS =' GOOD
           STOP RUN.
       GOOD.
           ADD 1 TO GOOD.
       BAD.
           MOVE 16 TO RETURN-CODE
           STOP RUN.
