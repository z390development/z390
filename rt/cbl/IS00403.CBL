         IDENTIFICATION DIVISION.
         PROGRAM-ID.  IS00403.
         ENVIRONMENT DIVISION.
         INPUT-OUTPUT SECTION.
         FILE-CONTROL.
         DATA DIVISION.
         FILE SECTION.
         WORKING-STORAGE SECTION.
         01  W1 PIC 9V9 VALUE 1.2.
         01  W2 PIC 9.9 VALUE 1.2.
         01  W3 PIC 9(7)V9(2).
         LINKAGE SECTION.
         PROCEDURE DIVISION.
         MAINLINE.
             DISPLAY W1.
             DISPLAY W2.
             COMPUTE W3 = 123456789 / 100.
             DISPLAY W3.
             STOP RUN.
