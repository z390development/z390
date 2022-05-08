         IDENTIFICATION DIVISION.
         PROGRAM-ID.  IS00404.
         ENVIRONMENT DIVISION.
         INPUT-OUTPUT SECTION.
         FILE-CONTROL.
         DATA DIVISION.
         FILE SECTION.
         WORKING-STORAGE SECTION.
         01  W4 PIC s9V9 VALUE -1.2.
         01  W5 PIC s9.9 VALUE -1.2.
         01  W6 PIC s9(7)V9(2).
         LINKAGE SECTION.
         PROCEDURE DIVISION.
         MAINLINE.
             DISPLAY W4.
             DISPLAY W5.
             COMPUTE W6 = (123456789 / 100) * -1.
             DISPLAY W6.
             STOP RUN.
