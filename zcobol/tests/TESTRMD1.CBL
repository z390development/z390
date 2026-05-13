 
        IDENTIFICATION DIVISION. 
        PROGRAM-ID. TESTRMD1.
      * AUTHOR.  DON HIGGINS.
      * DATE.    03/28/12.
      * REMARKS. TEST RMD FIELD MODS OF THE FORM (OFFSET:LENGTH)
      *            1. (LIT:LIT)
      *            2  (VAR:LIT)
      *            3  (LIT:VAR)
      *            3  (VAR:VAR)
        ENVIRONMENT DIVISION.
        DATA DIVISION. 
        WORKING-STORAGE SECTION.
        77 F1 PIC X(3) VALUE 'ABC'.
        77 F2 PIC X(5) VALUE 'DEFGH'.
        77 O1 PIC 9 VALUE 2.
        77 L1 PIC 9 VALUE 1.
        77 O2 PIC 9 VALUE 3.
        77 L2 PIC 9 VALUE 2.
        77 GOOD PIC 999 VALUE 0.
        77 BAD  PIC 999 VALUE 0.
        PROCEDURE DIVISION.
            DISPLAY 'TESTRMD1 STARTED'
            MOVE 'DEFGH' TO F2
     1      MOVE F1(2:1) TO F2(3:2)
            IF F2 NOT = 'DEB H' 
               DISPLAY 'ERROR 1 ''DEB H'' NOT =' F2
               PERFORM BAD 
            ELSE PERFORM GOOD.
            MOVE 'DEFGH' TO F2
     2      MOVE F1(O1:1) TO F2(O2:2)
            IF F2 NOT = 'DEB H' 
               DISPLAY 'ERROR 2 ''DEB H'' NOT =' F2
               PERFORM BAD 
            ELSE PERFORM GOOD.
     3      MOVE F1(2:L1) TO F2(3:L2)
            IF F2 NOT = 'DEB H' 
               DISPLAY 'ERROR 3 ''DEB H'' NOT =' F2
               PERFORM BAD 
            ELSE PERFORM GOOD.             
     4      MOVE 'DEFGH' TO F2
            MOVE F1(O1:L1) TO F2(O2:L2)
            IF F2 NOT = 'DEB H' 
               DISPLAY 'ERROR 4 ''DEB H'' NOT =' F2
               PERFORM BAD 
            ELSE PERFORM GOOD.
            DISPLAY  'TOTAL TESTS GOOD= ' GOOD ' BAD=' BAD.
            STOP RUN.
        GOOD.
            ADD 1 TO GOOD.
        BAD.
            ADD 1 TO BAD.