       IDENTIFICATION DIVISION.
       PROGRAM-ID.    TESTSIX2.
       AUTHOR.        DON HIGGINS.
       DATE-WRITTEN.  01/08/09.
      ***************************************************************
      * TEST SET AND INDEXES                              
      ***************************************************************
      * 02/24/09 RPI 1002 TEST SET AND INDEX SUPPORT
      ***************************************************************
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  T0.
           10  T1 OCCURS 2 TIMES.
               15 T1-ID PIC X.
               15 T2 OCCURS 3 TIMES.
                  20 T2-ID PIC X.
       77  S1 COMP PIC S9(9).
       77  S2 COMP PIC S9(9).
       77  NFS2 COMP   PIC S9(9)  OCCURS 2 TIMES.
       77  NGS2 COMP   PIC S9(18) OCCURS 2 TIMES.
       77  NHS2 COMP   PIC S9(4)  OCCURS 2 TIMES.
       77  I    COMP   PIC S99    VALUE 1.
       77  J    COMP   PIC S99    VALUE 2.
       77  GOOD COMP-3 PIC S9(9) VALUE 0.
       01  H1 PIC S9(4) COMP OCCURS 3 TIMES INDEXED BY H1X.
           88 H1-ZERO VALUE 10.
       77  H1X-SAVE  INDEX.
       77  H1X-SAVE2 INDEX.
       77  SUB PIC S9(9) COMP.
       PROCEDURE DIVISION.
       MAINLINE.
           DISPLAY 'TESTSIX2 STARTING'
           MOVE '1ABC2DEF' TO T0
     1     IF T1-ID(1) = '1'   PERFORM GOOD ELSE PERFORM BAD.
     2     IF T1-ID(2) = '2'   PERFORM GOOD ELSE PERFORM BAD.  
     3     IF T2-ID(1 1) = 'A' PERFORM GOOD ELSE PERFORM BAD.  
     4     IF T2-ID(2 1) = 'D' PERFORM GOOD ELSE PERFORM BAD.  
     5     IF T2-ID(1 3) = 'C' PERFORM GOOD ELSE PERFORM BAD.  
     6     IF T2-ID(2 3) = 'F' PERFORM GOOD ELSE PERFORM BAD.  
           MOVE 1 TO S1 MOVE 3 TO S2
     7     IF T2-ID(S1 S2) = 'C' PERFORM GOOD ELSE PERFORM BAD.  
           PERFORM VARYING S1 FROM 1 BY 1 UNTIL S1 > 2
               PERFORM VARYING S2 FROM 1 BY 1 UNTIL S2 > 3
                  DISPLAY 'T2-ID(' S1 ',' S2 ')=' T2-ID(S1 S2).
           MOVE 1 TO NFS2(I) NFS2(I+1)
           ADD       NFS2(I) TO NFS2(I+1)
     8     IF NFS2(I+1) = 2 PERFORM GOOD ELSE PERFORM BAD.
           ADD  1 TO NFS2(I) NFS2(I+1)
     9     IF NFS2(I)   = 2 PERFORM GOOD ELSE PERFORM BAD.
    10     IF NFS2(I+1) = 3 PERFORM GOOD ELSE PERFORM BAD.
      * TEST SET 88 LEVEL 
    11     SET H1-ZERO(2) TO TRUE
           IF H1-ZERO(2) 
              PERFORM GOOD ELSE PERFORM BAD.
      * TEST SET INDEXE-MAME, INDEX DATA FEILD, INT FIELD
           SET H1X TO 2
           MOVE 10 TO H1(2)
    12     IF H1(H1X) = 10 
              PERFORM GOOD ELSE PERFORM BAD.
           SET H1X UP BY 1
           SET SUB TO H1X
    13     IF SUB = 3 
              PERFORM GOOD ELSE PERFORM BAD.
           SET H1X-SAVE TO H1X
    14     IF H1X-SAVE = H1X 
              PERFORM GOOD ELSE PERFORM BAD.
           SET H1X-SAVE2 TO H1X-SAVE
    15     IF H1X-SAVE = H1X 
              PERFORM GOOD ELSE PERFORM BAD.
           MOVE 11 TO H1(3)
    16     IF H1(H1X) = 11 
              PERFORM GOOD ELSE PERFORM BAD.
           SET H1X DOWN BY 1
           MOVE 12 TO H1(H1X)
    17     IF H1(2) = 12 
              PERFORM GOOD ELSE PERFORM BAD.
           SET H1X TO H1X-SAVE
           MOVE 11 TO H1(3)
    18     IF H1(H1X) = 11 
              PERFORM GOOD ELSE PERFORM BAD.
           DISPLAY 'TESTSIX2 GOOD TESTS =' GOOD
           STOP RUN.
       GOOD.
           ADD 1 TO GOOD.
       BAD.
           MOVE 16 TO RETURN-CODE
           STOP RUN.
