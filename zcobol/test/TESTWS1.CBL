       IDENTIFICATION DIVISION.
       PROGRAM-ID.    TESTWS1.
       AUTHOR.        DON HIGGINS.
       DATE-WRITTEN.  05/01/08.
      ***************************************************************
      * REGRESSION TEST WORKING STORAGE DATA FIELD GENERATION
      ***************************************************************
      * 10/16/08 ADD TEST FOR OCCURS AT GROUP LEVEL
      * 01/01/09 ADD CONTINUATION TEST                  
      ***************************************************************
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77 FIELD   PIC X(30) VALUE SPACES.
       77 P1      PIC 9 VALUE 1.
       77 P2      PIC 9 VALUE 2.
       01 G1.
          05 PP3   PIC 99 VALUE 3.
          05 X1    PIC X  VALUE '1'.
          05 XX02  PIC XX VALUE '02'.
          05 G2-TABLE-DATA.
             10 PIC X VALUE 'A'.
             10 PIC X VALUE 'B'.
          05 G2-TABLE REDEFINES G2-TABLE-DATA OCCURS 2.
             10 G2-TABLE-ENTRY PIC X.
       01 G1-REDEF REDEFINES G1.
          05 PP3C  PIC XX.
          05 X3    PIC XXX.
          05 G2V   PIC XX.
       01 G1V PIC X(7) VALUE '03102AB'.
       77 TF COMP  PIC S9(9)  VALUE X'ff'.
       77 TH COMP  PIC S9(4)  VALUE X'ff'.
       77 TX       PIC X(1)   VALUE X'c1'.
       77 TF2 COMP  PIC S9(9)  VALUE X"ff".
       77 TH2 COMP  PIC S9(4)  VALUE X"ff".
       77 TX2       PIC X(1)   VALUE X"c1".
       77 TPN       PIC S9     VALUE +1.
       77 TMN       PIC S9     VALUE -1.
       77 TX1       PIC X(1).
       77 TX3       PIC X(3).
        77 SPLIT-LIT  PIC X(79)
                VALUE 'TESTBEC6 TEST 5:PRESS AID KEY TO HANDLE AEI0 ABEN
      -               'D IN LINKED PROGRAM BY PROGRAM'.
       PROCEDURE DIVISION.
       MAINLINE.
           DISPLAY 'TESTWS1 STARTING'
           IF P1 NOT = 1   MOVE 'P1' TO FIELD GO TO ABORT.
           IF PP3C NOT = '03' MOVE 'P1C' TO FIELD GO TO ABORT.
           IF X3 NOT = '102' MOVE 'X3' TO FIELD GO TO ABORT.
           IF P2 NOT = 2   MOVE 'P2' TO FIELD GO TO ABORT.
           IF G1 NOT = G1V MOVE 'G1' TO FIELD GO TO ABORT.
           IF G1-REDEF NOT = G1V MOVE 'G1-REDEF' TO FIELD GO TO ABORT.
           IF G2V      NOT = 'AB' MOVE 'G2-TABLE' TO FIELD GO TO ABORT.
           DISPLAY 'TF =' TF.
           IF TF NOT = 255 MOVE 'TF-HEX' TO FIELD GO TO ABORT.
           DISPLAY 'TH =' TH.
           IF TH NOT = 255 MOVE 'TH-HEX' TO FIELD GO TO ABORT.
           DISPLAY 'TX =' TX.
           IF TX NOT = 'A' MOVE 'TX-HEX' TO FIELD GO TO ABORT.
           DISPLAY 'TF =' TF.
           DISPLAY 'TH =' TH.
           DISPLAY 'TX =' TX.
           DISPLAY 'TF2=' TF2.
           DISPLAY 'TH2=' TH2.
           DISPLAY 'TX2=' TX2.
           IF TPN NOT = 1  MOVE 'TPN' TO FIELD GO TO ABORT.
           DISPLAY 'TX2=' TX2.
           IF TMN NOT = -1 MOVE 'TMN' TO FIELD GO TO ABORT.
           MOVE X'C1' TO TX1
           IF TX1 NOT = 'A' MOVE 'TX1' TO FIELD GO TO ABORT. 
           MOVE X'C1C2C3' TO TX3
           IF TX3 NOT = 'ABC' MOVE 'TX3' TO FIELD GO TO ABORT. 
           DISPLAY 'SPLIT-LIT=' SPLIT-LIT
           IF SPLIT-LIT NOT = 
                      'TESTBEC6 TEST 5:PRESS AID KEY TO HANDLE AEI0 ABEN
      -               'D IN LINKED PROGRAM BY PROGRAM' 
               MOVE 'SPLIT-LIT' TO FIELD GO TO ABORT.
           DISPLAY 'TESTWS1 ENDED OK'
           STOP RUN.
       ABORT.
           DISPLAY 'TESTWS1 ERROR ON ' FIELD
           MOVE 16 TO RETURN-CODE
           STOP RUN.
