       IDENTIFICATION DIVISION.
       PROGRAM-ID.
           DB101A01.
       ENVIRONMENT DIVISION.
       PROCEDURE DIVISION.
       CCVS1 SECTION.
      *    First call displays message1 and switches the GOTO
           PERFORM MAINLINE.
      *    Second call displays message2
           PERFORM MAINLINE.
           EXIT PROGRAM.
           STOP     RUN.
       MAINLINE.
           DISPLAY "MAINLINE ENTERED".
       MESSAGE-SWITCH.
           GO TO MESSAGE1.
       ERROR-MESAGE.
           DISPLAY "GO TO FAILED".
           GO TO MAINLINE-END.
       MESSAGE1.
           DISPLAY "MESSAGE 1".
           ALTER MESSAGE-SWITCH TO PROCEED TO MESSAGE2.
           GO TO MAINLINE-END
       MESSAGE2.
           DISPLAY "MESSAGE 2".
       MAINLINE-END.
