       IDENTIFICATION DIVISION.
       PROGRAM-ID.    TESTIF1.
       AUTHOR.        DON HIGGINS.
       DATE-WRITTEN.  10/13/08.
      * TEST IF ELSE END-IF                              
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77 A       PIC 9 VALUE 1.           
       77 B       PIC 9 VALUE 2.           
       77 C       PIC 9 VALUE 3.           
       77 D       PIC 9 VALUE 4.           
       77 ERRORS  PIC X VALUE 'F'.
       PROCEDURE DIVISION.
       MAINLINE.
           DISPLAY 'TESTIF1 STARTING'
     1     IF A = B 
              DISPLAY 'A NOT < B' MOVE 'T' TO ERRORS.
     2     IF A EQUAL B 
              DISPLAY 'A NOT < B' MOVE 'T' TO ERRORS.
     3     IF A > B 
              DISPLAY 'A NOT < B' MOVE 'T' TO ERRORS.
     4     IF A GREATER THAN B 
              DISPLAY 'A NOT < B' MOVE 'T' TO ERRORS.
     5     IF B < A 
              DISPLAY 'A NOT < B' MOVE 'T' TO ERRORS.
     6     IF B LESS THAN A 
              DISPLAY 'A NOT < B' MOVE 'T' TO ERRORS.
     7     IF A NOT < B 
              DISPLAY 'A NOT < B' MOVE 'T' TO ERRORS.
     8     IF A > B AND C > D 
              DISPLAY 'A NOT < B' MOVE 'T' TO ERRORS.
     9     IF A NOT < B OR C NOT < D 
              DISPLAY 'A NOT < B' MOVE 'T' TO ERRORS.
    10     IF (A > B) AND (C > D) 
              DISPLAY 'A NOT < B' MOVE 'T' TO ERRORS.
    11     IF (A > B OR C > D) AND (A = B OR C = D) 
              DISPLAY 'A NOT < B' MOVE 'T' TO ERRORS.
    12     IF A < B 
              NEXT SENTENCE ELSE
              DISPLAY 'A NOT < B' MOVE 'T' TO ERRORS.
    13     IF A EQUAL A 
              NEXT SENTENCE ELSE
              DISPLAY 'A NOT < B' MOVE 'T' TO ERRORS.
    14     IF B > A 
              NEXT SENTENCE ELSE
              DISPLAY 'A NOT < B' MOVE 'T' TO ERRORS.
    15     IF B GREATER THAN A 
              NEXT SENTENCE ELSE
              DISPLAY 'A NOT < B' MOVE 'T' TO ERRORS.
    16     IF A < B 
              NEXT SENTENCE ELSE
              DISPLAY 'A NOT < B' MOVE 'T' TO ERRORS.
    17     IF A LESS THAN B 
              NEXT SENTENCE ELSE
              DISPLAY 'A NOT < B' MOVE 'T' TO ERRORS.
    18     IF B NOT < A 
              NEXT SENTENCE ELSE
              DISPLAY 'A NOT < B' MOVE 'T' TO ERRORS.
    19     IF B > A AND D > C 
              NEXT SENTENCE ELSE
              DISPLAY 'A NOT < B' MOVE 'T' TO ERRORS.
    20     IF B NOT < A OR D NOT < C 
              NEXT SENTENCE ELSE
              DISPLAY 'A NOT < B' MOVE 'T' TO ERRORS.
    21     IF (B > A) AND (D > C) 
              NEXT SENTENCE ELSE
              DISPLAY 'A NOT < B' MOVE 'T' TO ERRORS.
    22     IF (B > A OR D > C) AND (B = B OR C = C) 
              NEXT SENTENCE ELSE
              DISPLAY 'A NOT < B' MOVE 'T' TO ERRORS.
           IF ERRORS = 'T'
              MOVE 16 TO RETURN-CODE
              DISPLAY 'TESTIF1 ENDED WITH ERRORS'
           ELSE
              DISPLAY 'TESTIF1 ENDED OK'
           END-IF
           STOP RUN.
