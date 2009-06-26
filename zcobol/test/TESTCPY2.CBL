       IDENTIFICATION DIVISION.
       PROGRAM-ID.    TESTCPY1.
       AUTHOR.        DON HIGGINS.
       DATE-WRITTEN.  12/01/08.
      ***************************************************************
      * REGRESSION TEST NESTED COPY SUPPORT
      ***************************************************************
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 VER1.
       COPY TESTCPY2.
       01 VER2.        COPY TESTCPY2.
       PROCEDURE DIVISION.
       MAINLINE.
           DISPLAY 'TESTCPY2 STARTING'
           MOVE 'VER1 FLD' TO FIELD OF VER1
           MOVE 'VER2 FLD' TO FIELD OF VER2
           DISPLAY 'TESTCPY2 VER1 FIELD =' FIELD OF VER1
           DISPLAY 'TESTCPY2 VER2 FIELD =' FIELD OF VER2
           IF  FIELD OF VER1 = 'VER1 FLD'
           AND FIELD OF VER2 = 'VER2 FLD'
               DISPLAY 'TESTCPY2 ENDING OK'
           ELSE
               DISPLAY 'TESTCPY2 FAILED'
               MOVE 16 TO RETURN-CODE
           END-IF
           STOP RUN.
