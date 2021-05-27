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
       01  H1 PIC S9(4) COMP OCCURS 2 TIMES.
           88 H1-ZERO VALUE 10.
       01  H2 PIC S9(4) COMP OCCURS 2 TIMES.
           88 H2-ONE-TWO  VALUES 11 12.
      * THE FOLLOWING ARE 9 SUPPORTED FLOATING POINT TYPES
       77 HFP-SRT FLOAT-HEX-7      VALUE 1234.
       77 HFP-LNG FLOAT-HEX-15     VALUE 1234.
       77 HFP-EXT FLOAT-HEX-30     VALUE 1234.
       77 BFP-SRT FLOAT-BINARY-7   VALUE 1234.
       77 BFP-LNG FLOAT-BINARY-16  VALUE 1234.
       77 BFP-EXT FLOAT-BINARY-34  VALUE 1234.
       77 DFP-SRT FLOAT-DECIMAL-7  VALUE 1234.
       77 DFP-LNG FLOAT-DECIMAL-16 VALUE 1234.
       77 DFP-EXT FLOAT-DECIMAL-34 VALUE 1234.
      * THE FOLLOWING 4 TYPES DEFINE 16, 32, 64, AND 128 BINARY TYPES
       77 HW      COMP PIC S9(4)  VALUE 1234.
       77 FW      COMP PIC S9(9)  VALUE 1234.
       77 DW      COMP PIC S9(18) VALUE 1234.
       77 QW      COMP PIC S9(39) VALUE 1234.
      * THE FOLLOWING 2 TYPES DEFINE 31 DIGIT PACKED AND ZONED DECIMAL
       77 PD      COMP-3 PIC S9(31) VALUE 1234.
       77 ZD             PIC S9(31) VALUE 1234.
       77  GOOD COMP-3 PIC S9(4) VALUE 0.
       PROCEDURE DIVISION.
       MAINLINE.
           DISPLAY 'TESTIF1 STARTING'
     1     IF A = B 
              DISPLAY 'A NOT < B' PERFORM BAD ELSE PERFORM GOOD.
     2     IF A EQUAL B 
              DISPLAY 'A NOT < B' PERFORM BAD ELSE PERFORM GOOD.
     3     IF A > B 
              DISPLAY 'A NOT < B' PERFORM BAD ELSE PERFORM GOOD.
     4     IF A GREATER THAN B 
              DISPLAY 'A NOT < B' PERFORM BAD ELSE PERFORM GOOD.
     5     IF B < A 
              DISPLAY 'A NOT < B' PERFORM BAD ELSE PERFORM GOOD.
     6     IF B LESS THAN A 
              DISPLAY 'A NOT < B' PERFORM BAD ELSE PERFORM GOOD.
     7     IF A NOT < B 
              DISPLAY 'A NOT < B' PERFORM BAD ELSE PERFORM GOOD.
     8     IF A > B AND C > D 
              DISPLAY 'A NOT < B' PERFORM BAD ELSE PERFORM GOOD.
     9     IF A NOT < B OR C NOT < D 
              DISPLAY 'A NOT < B' PERFORM BAD ELSE PERFORM GOOD.
    10     IF (A > B) AND (C > D) 
              DISPLAY 'A NOT < B' PERFORM BAD ELSE PERFORM GOOD.
    11     IF (A > B OR C > D) AND (A = B OR C = D) 
              DISPLAY 'A NOT < B' PERFORM BAD ELSE PERFORM GOOD.
    12     IF A < B 
              PERFORM GOOD
              NEXT SENTENCE ELSE  PERFORM BAD.
    13     IF A EQUAL A 
              PERFORM GOOD
              NEXT SENTENCE ELSE  PERFORM BAD.
    14     IF B > A 
              PERFORM GOOD
              NEXT SENTENCE ELSE PERFORM BAD.               
    15     IF B GREATER THAN A 
              PERFORM GOOD
              NEXT SENTENCE ELSE PERFORM BAD.               
    16     IF A < B 
              PERFORM GOOD
              NEXT SENTENCE ELSE PERFORM BAD.               
    17     IF A LESS THAN B 
              PERFORM GOOD
              NEXT SENTENCE ELSE PERFORM BAD.               
    18     IF B NOT < A 
              PERFORM GOOD
              NEXT SENTENCE ELSE PERFORM BAD.               
    19     IF B > A AND D > C 
              PERFORM GOOD
              NEXT SENTENCE ELSE PERFORM BAD.               
    20     IF B NOT < A OR D NOT < C 
              PERFORM GOOD
              NEXT SENTENCE ELSE PERFORM BAD.               
    21     IF (B > A) AND (D > C) 
              PERFORM GOOD
              NEXT SENTENCE ELSE PERFORM BAD.               
    22     IF (B > A OR D > C) AND (B = B OR C = C) 
              PERFORM GOOD
              NEXT SENTENCE ELSE PERFORM BAD.               
      * TEST CONDITIONAL 88'S
           MOVE 10 TO H1(2)
    23     IF H1-ZERO(2) 
              PERFORM GOOD
              NEXT SENTENCE ELSE PERFORM BAD.               
           MOVE 13 TO H1(2)
    24     IF NOT H1-ZERO(2) 
              PERFORM GOOD
              NEXT SENTENCE ELSE PERFORM BAD.               
           MOVE 12 TO H2(2)  
    25     IF H2-ONE-TWO(2) 
              PERFORM GOOD
              NEXT SENTENCE ELSE PERFORM BAD.               
           MOVE 13 TO H2(2)  
    26     IF NOT H2-ONE-TWO(2) 
              PERFORM GOOD
              NEXT SENTENCE ELSE PERFORM BAD. 
      * TEST ALL FIELD TYPE COMBINATIONS
           IF HFP-SRT = HFP-SRT      PERFORM GOOD ELSE PERFORM BAD.              
           IF HFP-SRT = HFP-LNG      PERFORM GOOD ELSE PERFORM BAD.              
           IF HFP-SRT = HFP-EXT      PERFORM GOOD ELSE PERFORM BAD.              
           IF HFP-SRT = BFP-SRT      PERFORM GOOD ELSE PERFORM BAD.              
           IF HFP-SRT = BFP-LNG      PERFORM GOOD ELSE PERFORM BAD.              
           IF HFP-SRT = BFP-EXT      PERFORM GOOD ELSE PERFORM BAD.              
           IF HFP-SRT = DFP-SRT      PERFORM GOOD ELSE PERFORM BAD.              
           IF HFP-SRT = DFP-LNG      PERFORM GOOD ELSE PERFORM BAD.              
           IF HFP-SRT = DFP-EXT      PERFORM GOOD ELSE PERFORM BAD.              
           IF HFP-SRT = HW           PERFORM GOOD ELSE PERFORM BAD.              
           IF HFP-SRT = FW           PERFORM GOOD ELSE PERFORM BAD.              
           IF HFP-SRT = DW           PERFORM GOOD ELSE PERFORM BAD.              
           IF HFP-SRT = QW           PERFORM GOOD ELSE PERFORM BAD.              
           IF HFP-SRT = PD           PERFORM GOOD ELSE PERFORM BAD.              
           IF HFP-SRT = ZD           PERFORM GOOD ELSE PERFORM BAD.              
           IF HFP-LNG = HFP-SRT      PERFORM GOOD ELSE PERFORM BAD.              
           IF HFP-LNG = HFP-LNG      PERFORM GOOD ELSE PERFORM BAD.              
           IF HFP-LNG = HFP-EXT      PERFORM GOOD ELSE PERFORM BAD.              
           IF HFP-LNG = BFP-SRT      PERFORM GOOD ELSE PERFORM BAD.              
           IF HFP-LNG = BFP-LNG      PERFORM GOOD ELSE PERFORM BAD.              
           IF HFP-LNG = BFP-EXT      PERFORM GOOD ELSE PERFORM BAD.              
           IF HFP-LNG = DFP-SRT      PERFORM GOOD ELSE PERFORM BAD.              
           IF HFP-LNG = DFP-LNG      PERFORM GOOD ELSE PERFORM BAD.              
           IF HFP-LNG = DFP-EXT      PERFORM GOOD ELSE PERFORM BAD.              
           IF HFP-LNG = HW           PERFORM GOOD ELSE PERFORM BAD.              
           IF HFP-LNG = FW           PERFORM GOOD ELSE PERFORM BAD.              
           IF HFP-LNG = DW           PERFORM GOOD ELSE PERFORM BAD.              
           IF HFP-LNG = QW           PERFORM GOOD ELSE PERFORM BAD.              
           IF HFP-LNG = PD           PERFORM GOOD ELSE PERFORM BAD.              
           IF HFP-LNG = ZD           PERFORM GOOD ELSE PERFORM BAD.              
           IF HFP-EXT = HFP-SRT      PERFORM GOOD ELSE PERFORM BAD.              
           IF HFP-EXT = HFP-LNG      PERFORM GOOD ELSE PERFORM BAD.              
           IF HFP-EXT = HFP-EXT      PERFORM GOOD ELSE PERFORM BAD.              
           IF HFP-EXT = BFP-SRT      PERFORM GOOD ELSE PERFORM BAD.              
           IF HFP-EXT = BFP-LNG      PERFORM GOOD ELSE PERFORM BAD.              
           IF HFP-EXT = BFP-EXT      PERFORM GOOD ELSE PERFORM BAD.              
           IF HFP-EXT = DFP-SRT      PERFORM GOOD ELSE PERFORM BAD.              
           IF HFP-EXT = DFP-LNG      PERFORM GOOD ELSE PERFORM BAD.              
           IF HFP-EXT = DFP-EXT      PERFORM GOOD ELSE PERFORM BAD.              
           IF HFP-EXT = HW           PERFORM GOOD ELSE PERFORM BAD.              
           IF HFP-EXT = FW           PERFORM GOOD ELSE PERFORM BAD.              
           IF HFP-EXT = DW           PERFORM GOOD ELSE PERFORM BAD.              
           IF HFP-EXT = QW           PERFORM GOOD ELSE PERFORM BAD.              
           IF HFP-EXT = PD           PERFORM GOOD ELSE PERFORM BAD.              
           IF HFP-EXT = ZD           PERFORM GOOD ELSE PERFORM BAD.              
           IF BFP-SRT = HFP-SRT      PERFORM GOOD ELSE PERFORM BAD.              
           IF BFP-SRT = HFP-LNG      PERFORM GOOD ELSE PERFORM BAD.              
           IF BFP-SRT = HFP-EXT      PERFORM GOOD ELSE PERFORM BAD.              
           IF BFP-SRT = BFP-SRT      PERFORM GOOD ELSE PERFORM BAD.              
           IF BFP-SRT = BFP-LNG      PERFORM GOOD ELSE PERFORM BAD.              
           IF BFP-SRT = BFP-EXT      PERFORM GOOD ELSE PERFORM BAD.              
           IF BFP-SRT = DFP-SRT      PERFORM GOOD ELSE PERFORM BAD.              
           IF BFP-SRT = DFP-LNG      PERFORM GOOD ELSE PERFORM BAD.              
           IF BFP-SRT = DFP-EXT      PERFORM GOOD ELSE PERFORM BAD.              
           IF BFP-SRT = HW           PERFORM GOOD ELSE PERFORM BAD.              
           IF BFP-SRT = FW           PERFORM GOOD ELSE PERFORM BAD.              
           IF BFP-SRT = DW           PERFORM GOOD ELSE PERFORM BAD.              
           IF BFP-SRT = QW           PERFORM GOOD ELSE PERFORM BAD.              
           IF BFP-SRT = PD           PERFORM GOOD ELSE PERFORM BAD.              
           IF BFP-SRT = ZD           PERFORM GOOD ELSE PERFORM BAD.              
           IF BFP-LNG = HFP-SRT      PERFORM GOOD ELSE PERFORM BAD.              
           IF BFP-LNG = HFP-LNG      PERFORM GOOD ELSE PERFORM BAD.              
           IF BFP-LNG = HFP-EXT      PERFORM GOOD ELSE PERFORM BAD.              
           IF BFP-LNG = BFP-SRT      PERFORM GOOD ELSE PERFORM BAD.              
           IF BFP-LNG = BFP-LNG      PERFORM GOOD ELSE PERFORM BAD.              
           IF BFP-LNG = BFP-EXT      PERFORM GOOD ELSE PERFORM BAD.              
           IF BFP-LNG = DFP-SRT      PERFORM GOOD ELSE PERFORM BAD.              
           IF BFP-LNG = DFP-LNG      PERFORM GOOD ELSE PERFORM BAD.              
           IF BFP-LNG = DFP-EXT      PERFORM GOOD ELSE PERFORM BAD.              
           IF BFP-LNG = HW           PERFORM GOOD ELSE PERFORM BAD.              
           IF BFP-LNG = FW           PERFORM GOOD ELSE PERFORM BAD.              
           IF BFP-LNG = DW           PERFORM GOOD ELSE PERFORM BAD.              
           IF BFP-LNG = QW           PERFORM GOOD ELSE PERFORM BAD.              
           IF BFP-LNG = PD           PERFORM GOOD ELSE PERFORM BAD.              
           IF BFP-LNG = ZD           PERFORM GOOD ELSE PERFORM BAD.              
           IF BFP-EXT = HFP-SRT      PERFORM GOOD ELSE PERFORM BAD.              
           IF BFP-EXT = HFP-LNG      PERFORM GOOD ELSE PERFORM BAD.              
           IF BFP-EXT = HFP-EXT      PERFORM GOOD ELSE PERFORM BAD.              
           IF BFP-EXT = BFP-SRT      PERFORM GOOD ELSE PERFORM BAD.              
           IF BFP-EXT = BFP-LNG      PERFORM GOOD ELSE PERFORM BAD.              
           IF BFP-EXT = BFP-EXT      PERFORM GOOD ELSE PERFORM BAD.              
           IF BFP-EXT = DFP-SRT      PERFORM GOOD ELSE PERFORM BAD.              
           IF BFP-EXT = DFP-LNG      PERFORM GOOD ELSE PERFORM BAD.              
           IF BFP-EXT = DFP-EXT      PERFORM GOOD ELSE PERFORM BAD.              
           IF BFP-EXT = HW           PERFORM GOOD ELSE PERFORM BAD.              
           IF BFP-EXT = FW           PERFORM GOOD ELSE PERFORM BAD.              
           IF BFP-EXT = DW           PERFORM GOOD ELSE PERFORM BAD.              
           IF BFP-EXT = QW           PERFORM GOOD ELSE PERFORM BAD.              
           IF BFP-EXT = PD           PERFORM GOOD ELSE PERFORM BAD.              
           IF BFP-EXT = ZD           PERFORM GOOD ELSE PERFORM BAD.              
           IF DFP-SRT = HFP-SRT      PERFORM GOOD ELSE PERFORM BAD.              
           IF DFP-SRT = HFP-LNG      PERFORM GOOD ELSE PERFORM BAD.              
           IF DFP-SRT = HFP-EXT      PERFORM GOOD ELSE PERFORM BAD.              
           IF DFP-SRT = BFP-SRT      PERFORM GOOD ELSE PERFORM BAD.              
           IF DFP-SRT = BFP-LNG      PERFORM GOOD ELSE PERFORM BAD.              
           IF DFP-SRT = BFP-EXT      PERFORM GOOD ELSE PERFORM BAD.              
           IF DFP-SRT = DFP-SRT      PERFORM GOOD ELSE PERFORM BAD.              
           IF DFP-SRT = DFP-LNG      PERFORM GOOD ELSE PERFORM BAD.              
           IF DFP-SRT = DFP-EXT      PERFORM GOOD ELSE PERFORM BAD.              
           IF DFP-SRT = HW           PERFORM GOOD ELSE PERFORM BAD.              
           IF DFP-SRT = FW           PERFORM GOOD ELSE PERFORM BAD.              
           IF DFP-SRT = DW           PERFORM GOOD ELSE PERFORM BAD.              
           IF DFP-SRT = QW           PERFORM GOOD ELSE PERFORM BAD.              
           IF DFP-SRT = PD           PERFORM GOOD ELSE PERFORM BAD.              
           IF DFP-SRT = ZD           PERFORM GOOD ELSE PERFORM BAD.              
           IF DFP-LNG = HFP-SRT      PERFORM GOOD ELSE PERFORM BAD.              
           IF DFP-LNG = HFP-LNG      PERFORM GOOD ELSE PERFORM BAD.              
           IF DFP-LNG = HFP-EXT      PERFORM GOOD ELSE PERFORM BAD.              
           IF DFP-LNG = BFP-SRT      PERFORM GOOD ELSE PERFORM BAD.              
           IF DFP-LNG = BFP-LNG      PERFORM GOOD ELSE PERFORM BAD.              
           IF DFP-LNG = BFP-EXT      PERFORM GOOD ELSE PERFORM BAD.              
           IF DFP-LNG = DFP-SRT      PERFORM GOOD ELSE PERFORM BAD.              
           IF DFP-LNG = DFP-LNG      PERFORM GOOD ELSE PERFORM BAD.              
           IF DFP-LNG = DFP-EXT      PERFORM GOOD ELSE PERFORM BAD.              
           IF DFP-LNG = HW           PERFORM GOOD ELSE PERFORM BAD.              
           IF DFP-LNG = FW           PERFORM GOOD ELSE PERFORM BAD.              
           IF DFP-LNG = DW           PERFORM GOOD ELSE PERFORM BAD.              
           IF DFP-LNG = QW           PERFORM GOOD ELSE PERFORM BAD.              
           IF DFP-LNG = PD           PERFORM GOOD ELSE PERFORM BAD.              
           IF DFP-LNG = ZD           PERFORM GOOD ELSE PERFORM BAD.              
           IF DFP-EXT = HFP-SRT      PERFORM GOOD ELSE PERFORM BAD.              
           IF DFP-EXT = HFP-LNG      PERFORM GOOD ELSE PERFORM BAD.              
           IF DFP-EXT = HFP-EXT      PERFORM GOOD ELSE PERFORM BAD.              
           IF DFP-EXT = BFP-SRT      PERFORM GOOD ELSE PERFORM BAD.              
           IF DFP-EXT = BFP-LNG      PERFORM GOOD ELSE PERFORM BAD.              
           IF DFP-EXT = BFP-EXT      PERFORM GOOD ELSE PERFORM BAD.              
           IF DFP-EXT = DFP-SRT      PERFORM GOOD ELSE PERFORM BAD.              
           IF DFP-EXT = DFP-LNG      PERFORM GOOD ELSE PERFORM BAD.              
           IF DFP-EXT = DFP-EXT      PERFORM GOOD ELSE PERFORM BAD.              
           IF DFP-EXT = HW           PERFORM GOOD ELSE PERFORM BAD.              
           IF DFP-EXT = FW           PERFORM GOOD ELSE PERFORM BAD.              
           IF DFP-EXT = DW           PERFORM GOOD ELSE PERFORM BAD.              
           IF DFP-EXT = QW           PERFORM GOOD ELSE PERFORM BAD.              
           IF DFP-EXT = PD           PERFORM GOOD ELSE PERFORM BAD.              
           IF DFP-EXT = ZD           PERFORM GOOD ELSE PERFORM BAD.              
           IF HW      = HFP-SRT      PERFORM GOOD ELSE PERFORM BAD.              
           IF HW      = HFP-LNG      PERFORM GOOD ELSE PERFORM BAD.              
           IF HW      = HFP-EXT      PERFORM GOOD ELSE PERFORM BAD.              
           IF HW      = BFP-SRT      PERFORM GOOD ELSE PERFORM BAD.              
           IF HW      = BFP-LNG      PERFORM GOOD ELSE PERFORM BAD.              
           IF HW      = BFP-EXT      PERFORM GOOD ELSE PERFORM BAD.              
           IF HW      = DFP-SRT      PERFORM GOOD ELSE PERFORM BAD.              
           IF HW      = DFP-LNG      PERFORM GOOD ELSE PERFORM BAD.              
           IF HW      = DFP-EXT      PERFORM GOOD ELSE PERFORM BAD.              
           IF HW      = HW           PERFORM GOOD ELSE PERFORM BAD.              
           IF HW      = FW           PERFORM GOOD ELSE PERFORM BAD.              
           IF HW      = DW           PERFORM GOOD ELSE PERFORM BAD.              
           IF HW      = QW           PERFORM GOOD ELSE PERFORM BAD.              
           IF HW      = PD           PERFORM GOOD ELSE PERFORM BAD.              
           IF HW      = ZD           PERFORM GOOD ELSE PERFORM BAD.              
           IF FW      = HFP-SRT      PERFORM GOOD ELSE PERFORM BAD.              
           IF FW      = HFP-LNG      PERFORM GOOD ELSE PERFORM BAD.              
           IF FW      = HFP-EXT      PERFORM GOOD ELSE PERFORM BAD.              
           IF FW      = BFP-SRT      PERFORM GOOD ELSE PERFORM BAD.              
           IF FW      = BFP-LNG      PERFORM GOOD ELSE PERFORM BAD.              
           IF FW      = BFP-EXT      PERFORM GOOD ELSE PERFORM BAD.              
           IF FW      = DFP-SRT      PERFORM GOOD ELSE PERFORM BAD.              
           IF FW      = DFP-LNG      PERFORM GOOD ELSE PERFORM BAD.              
           IF FW      = DFP-EXT      PERFORM GOOD ELSE PERFORM BAD.              
           IF FW      = HW           PERFORM GOOD ELSE PERFORM BAD.              
           IF FW      = FW           PERFORM GOOD ELSE PERFORM BAD.              
           IF FW      = DW           PERFORM GOOD ELSE PERFORM BAD.              
           IF FW      = QW           PERFORM GOOD ELSE PERFORM BAD.              
           IF FW      = PD           PERFORM GOOD ELSE PERFORM BAD.              
           IF FW      = ZD           PERFORM GOOD ELSE PERFORM BAD.              
           IF DW      = HFP-SRT      PERFORM GOOD ELSE PERFORM BAD.              
           IF DW      = HFP-LNG      PERFORM GOOD ELSE PERFORM BAD.              
           IF DW      = HFP-EXT      PERFORM GOOD ELSE PERFORM BAD.              
           IF DW      = BFP-SRT      PERFORM GOOD ELSE PERFORM BAD.              
           IF DW      = BFP-LNG      PERFORM GOOD ELSE PERFORM BAD.              
           IF DW      = BFP-EXT      PERFORM GOOD ELSE PERFORM BAD.              
           IF DW      = DFP-SRT      PERFORM GOOD ELSE PERFORM BAD.              
           IF DW      = DFP-LNG      PERFORM GOOD ELSE PERFORM BAD.              
           IF DW      = DFP-EXT      PERFORM GOOD ELSE PERFORM BAD.              
           IF DW      = HW           PERFORM GOOD ELSE PERFORM BAD.              
           IF DW      = FW           PERFORM GOOD ELSE PERFORM BAD.              
           IF DW      = DW           PERFORM GOOD ELSE PERFORM BAD.              
           IF DW      = QW           PERFORM GOOD ELSE PERFORM BAD.              
           IF DW      = PD           PERFORM GOOD ELSE PERFORM BAD.              
           IF DW      = ZD           PERFORM GOOD ELSE PERFORM BAD.              
           IF QW      = HFP-SRT      PERFORM GOOD ELSE PERFORM BAD.              
           IF QW      = HFP-LNG      PERFORM GOOD ELSE PERFORM BAD.              
           IF QW      = HFP-EXT      PERFORM GOOD ELSE PERFORM BAD.              
           IF QW      = BFP-SRT      PERFORM GOOD ELSE PERFORM BAD.              
           IF QW      = BFP-LNG      PERFORM GOOD ELSE PERFORM BAD.              
           IF QW      = BFP-EXT      PERFORM GOOD ELSE PERFORM BAD.              
           IF QW      = DFP-SRT      PERFORM GOOD ELSE PERFORM BAD.              
           IF QW      = DFP-LNG      PERFORM GOOD ELSE PERFORM BAD.              
           IF QW      = DFP-EXT      PERFORM GOOD ELSE PERFORM BAD.              
           IF QW      = HW           PERFORM GOOD ELSE PERFORM BAD.              
           IF QW      = FW           PERFORM GOOD ELSE PERFORM BAD.              
           IF QW      = DW           PERFORM GOOD ELSE PERFORM BAD.              
           IF QW      = QW           PERFORM GOOD ELSE PERFORM BAD.              
           IF QW      = PD           PERFORM GOOD ELSE PERFORM BAD.              
           IF QW      = ZD           PERFORM GOOD ELSE PERFORM BAD.              
           IF PD      = HFP-SRT      PERFORM GOOD ELSE PERFORM BAD.              
           IF PD      = HFP-LNG      PERFORM GOOD ELSE PERFORM BAD.              
           IF PD      = HFP-EXT      PERFORM GOOD ELSE PERFORM BAD.              
           IF PD      = BFP-SRT      PERFORM GOOD ELSE PERFORM BAD.              
           IF PD      = BFP-LNG      PERFORM GOOD ELSE PERFORM BAD.              
           IF PD      = BFP-EXT      PERFORM GOOD ELSE PERFORM BAD.              
           IF PD      = DFP-SRT      PERFORM GOOD ELSE PERFORM BAD.              
           IF PD      = DFP-LNG      PERFORM GOOD ELSE PERFORM BAD.              
           IF PD      = DFP-EXT      PERFORM GOOD ELSE PERFORM BAD.              
           IF PD      = HW           PERFORM GOOD ELSE PERFORM BAD.              
           IF PD      = FW           PERFORM GOOD ELSE PERFORM BAD.              
           IF PD      = DW           PERFORM GOOD ELSE PERFORM BAD.              
           IF PD      = QW           PERFORM GOOD ELSE PERFORM BAD.              
           IF PD      = PD           PERFORM GOOD ELSE PERFORM BAD.              
           IF PD      = ZD           PERFORM GOOD ELSE PERFORM BAD.              
           IF ZD      = HFP-SRT      PERFORM GOOD ELSE PERFORM BAD.              
           IF ZD      = HFP-LNG      PERFORM GOOD ELSE PERFORM BAD.              
           IF ZD      = HFP-EXT      PERFORM GOOD ELSE PERFORM BAD.              
           IF ZD      = BFP-SRT      PERFORM GOOD ELSE PERFORM BAD.              
           IF ZD      = BFP-LNG      PERFORM GOOD ELSE PERFORM BAD.              
           IF ZD      = BFP-EXT      PERFORM GOOD ELSE PERFORM BAD.              
           IF ZD      = DFP-SRT      PERFORM GOOD ELSE PERFORM BAD.              
           IF ZD      = DFP-LNG      PERFORM GOOD ELSE PERFORM BAD.              
           IF ZD      = DFP-EXT      PERFORM GOOD ELSE PERFORM BAD.              
           IF ZD      = HW           PERFORM GOOD ELSE PERFORM BAD.              
           IF ZD      = DW           PERFORM GOOD ELSE PERFORM BAD.              
           IF ZD      = QW           PERFORM GOOD ELSE PERFORM BAD.              
           IF ZD      = PD           PERFORM GOOD ELSE PERFORM BAD.              
           IF ZD      = ZD           PERFORM GOOD ELSE PERFORM BAD.              
           DISPLAY 'TESTIF1 GOOD TESTS =' GOOD
           STOP RUN.
       GOOD.
           ADD 1 TO GOOD.
       BAD.
           MOVE 16 TO RETURN-CODE
           STOP RUN.
