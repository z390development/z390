       IDENTIFICATION DIVISION.
       PROGRAM-ID.    TESTSIX1.
       AUTHOR.        DON HIGGINS.
       DATE-WRITTEN.  01/08/09.
      * TEST GO TO DEPENDING ON                                                 
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  IF1   COMP   PIC S9(9)  VALUE 0.
       77  IG1   COMP   PIC S9(18) VALUE 0.
       77  IH1   COMP   PIC S9(4)  VALUE 0.
       77  IP1   COMP-3 PIC S9(9)  VALUE 0.
       77  IZ1          PIC S9(9)  VALUE 0.
       PROCEDURE DIVISION.
       MAINLINE.
           DISPLAY 'TESTGO1 STARTING'
           MOVE 1 TO IF1                
           GO TO P1 P2 DEPENDING ON IF1
           MOVE 16 TO RETURN-CODE
           DISPLAY 'ERROR BEFORE P2'.
       P2.
           MOVE 16 TO RETURN-CODE
           DISPLAY 'ERROR AT PE2'.
       P1.    
           MOVE 2 TO IG1
           GO TO P3 P4 DEPENDING ON IG1
           MOVE 16 TO RETURN-CODE
           DISPLAY 'ERROR BEFORE P3'.
       P3. 
           MOVE 16 TO RETURN-CODE
           DISPLAY 'ERROR AT P3'.
       P4. 
           MOVE 0 TO IH1
           GO TO P5 DEPENDING ON IH1
           GO TO P6.
       P5.
           MOVE 16 TO RETURN-CODE
           DISPLAY 'ERROR AT P5'.
       P6.
           MOVE 2 TO IP1
           GO TO P7 DEPENDING ON IP1
           GO TO P8.
       P7.
           MOVE 16 TO RETURN-CODE
           DISPLAY 'ERROR AT P7'.
       P8.
           MOVE 1 TO IZ1
           GO TO P10 DEPENDING ON IZ1
           MOVE 16 TO RETURN-CODE
           DISPLAY 'ERROR BEFORE P10'.
       P10.
           DISPLAY 'TESTGO1 ENDED OK'
           STOP RUN.
