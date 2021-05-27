       IDENTIFICATION DIVISION.
       PROGRAM-ID.    TESTPM2.
       AUTHOR.        DON HIGGINS.
      * TEST SECTIONS WITH DUPLICATE PARAGRAPHS AND PG QUAL.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       PROCEDURE DIVISION.
       S1 SECTION.
           DISPLAY 'TESTZC1 STARTED'
       P1.
           PERFORM P3.
       P2.
           PERFORM P1 OF S2.
       P3.
           DISPLAY 'S1 P3'.
       S2 SECTION.
       P1.
           PERFORM P3.
       P2.
           PERFORM P1 OF S1
       P3.
           DISPLAY 'S2 P3'
       P4.
           DISPLAY 'TESTZC1 ENDED'
           STOP RUN.
