       IDENTIFICATION DIVISION.
       PROGRAM-ID.    TESTMOV1.
       AUTHOR.        DON HIGGINS.
      * TEST MOVE.
      ***************************************************************
      * 01/26/12 DSH RPI 1181 ADD TEST 21
      ***************************************************************
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  ABCDEF PIC X(6) VALUE 'ABCDEF'.
       01  ABC    PIC X(3) REDEFINES ABCDEF OCCURS 2.
        77  NF1    COMP PIC S9(9).
        77  DF1    PIC Z(9).
        77  DF2    PIC Z(8)9.
        77  DF3    PIC ZZZ,ZZZ,ZZ9.
        77  NH1    COMP PIC S9(4).
        77  DH1    PIC Z(4).
        77  DH2    PIC Z(3)9.
        77  NP1    COMP-3 PIC S9.
        77  NP2    COMP-3 PIC S9(4).
        77  DP1    PIC Z.
        77  DP2    PIC Z(3)9.
        77  DP3    PIC ZZ-99.
        77  FLD1   PIC XX VALUE  'XY'.
        77  FLD2   PIC XXX VALUE 'ABC'.
        77  GOOD   COMP PIC S9(9) VALUE 0..
       77 SUB1 PIC 9.
       PROCEDURE DIVISION.
       MAINLINE.
           MOVE 0 TO NF1
     1     MOVE NF1 TO DF1                                           
           IF DF1 NOT = '         ' PERFORM BAD ELSE PERFORM GOOD.
           MOVE 1 TO NF1
     2     MOVE NF1 TO DF1                                            
           IF DF1 NOT = '        1' PERFORM BAD ELSE PERFORM GOOD.
           MOVE 0 TO NF1
     3     MOVE NF1 TO DF2                                            
           IF DF2 NOT = '        0' PERFORM BAD ELSE PERFORM GOOD.
           MOVE 123456789 TO NF1
     4     MOVE NF1 TO DF2                                            
           IF DF2 NOT = '123456789' PERFORM BAD ELSE PERFORM GOOD.
           MOVE 0 TO NF1
     5     MOVE NF1 TO DF3                                            
           IF DF3 NOT = '          0' PERFORM BAD ELSE PERFORM GOOD.
           MOVE 123456789 TO NF1
     6     MOVE NF1 TO DF3                                            
           IF DF3 NOT = '123,456,789' PERFORM BAD ELSE PERFORM GOOD.
           MOVE 0 TO NH1
     7     MOVE NH1 TO DH1                                            
           IF DH1 NOT = '    ' PERFORM BAD ELSE PERFORM GOOD.
           MOVE 1 TO NH1
     8     MOVE NH1 TO DH1                                            
           IF DH1 NOT = '   1' PERFORM BAD ELSE PERFORM GOOD.
           MOVE 0 TO NH1
     9     MOVE NH1 TO DH2                                            
           IF DH2 NOT = '   0' PERFORM BAD ELSE PERFORM GOOD.
           MOVE 1234 TO NH1
    10     MOVE NH1 TO DH2                                            
           IF DH2 NOT = '1234' PERFORM BAD ELSE PERFORM GOOD.
           MOVE 0 TO NP1
    11     MOVE NP1 TO DP1                                           
           IF DP1 NOT = ' ' PERFORM BAD ELSE PERFORM GOOD.
           MOVE 1 TO NP1
    12     MOVE NP1 TO DP1                                            
           IF DP1 NOT = '1' PERFORM BAD ELSE PERFORM GOOD.
           MOVE 0 TO NP1
    13     MOVE NP1 TO DP2                                            
           IF DP2 NOT = '   0' PERFORM BAD ELSE PERFORM GOOD.
           MOVE 0 TO NP2
    14     MOVE NP2 TO DP2                                            
           IF DP2 NOT = '   0' PERFORM BAD ELSE PERFORM GOOD.
           MOVE 1234 TO NP2
    15     MOVE NP2 TO DP2                                            
           IF DP2 NOT = '1234' PERFORM BAD ELSE PERFORM GOOD.
           MOVE 12 TO NP2
    16     MOVE NP2 TO DP3                                            
           IF DP3 NOT = '   12' PERFORM BAD ELSE PERFORM GOOD.
           MOVE 1234 TO NP2
    17     MOVE NP2 TO DP3                                            
           IF DP3 NOT = '12-34' PERFORM BAD ELSE PERFORM GOOD.
    18     MOVE ABCDEF(2:1) TO ABCDEF(5:1)
           IF ABCDEF NOT = 'ABCDBF' PERFORM BAD ELSE PERFORM GOOD.
    19     IF ABC(1)(2:1) NOT = ABC(2)(2:1)
               PERFORM BAD ELSE PERFORM GOOD.
    20     MOVE ABC(1)(2:2) TO ABC(2)(2:2)
           IF ABC(2) NOT = 'DBC' PERFORM BAD ELSE PERFORM GOOD.
    21     MOVE FLD1 TO FLD2(2:2)
           IF FLD2 NOT = 'AXY' PERFORM BAD ELSE PERFORM GOOD.
    22     MOVE 2 TO SUB1
           MOVE '12' TO FLD2(SUB1:2)
           IF FLD2 NOT = 'A12' PERFORM BAD ELSE PERFORM GOOD.
    23     MOVE 1 TO SUB1
            MOVE '12' TO FLD2(SUB1+1:2)
           IF FLD2 NOT = 'A12' PERFORM BAD ELSE PERFORM GOOD.
    24     MOVE 3 TO SUB1
            MOVE '12' TO FLD2(SUB1 -1:2)
           IF FLD2 NOT = 'A12' PERFORM BAD ELSE PERFORM GOOD.
       WRAP-UP.
           DISPLAY 'TESTMOV1 GOOD TESTS =' GOOD 
           STOP RUN.
       GOOD.
           ADD 1 TO GOOD
       BAD.
           DISPLAY 'TESTMOV1 GOOD TESTS =' GOOD 'BAD TESTS=1'.
           MOVE 16 TO RETURN-CODE
           STOP RUN.
