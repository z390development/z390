       IDENTIFICATION DIVISION.
       PROGRAM-ID.    TESTCMP2.
       AUTHOR.        DON HIGGINS.
       DATE-WRITTEN.  10/13/08.
      * TEST ADD, SUBTRACT, MULTIPLEY, DIVIDE.                              
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77 HFP-SRT FLOAT-HEX-7       VALUE 1234.
       77 HFP-LNG FLOAT-HEX-15      VALUE 1234.
       77 HFP-EXT FLOAT-HEX-30      VALUE 1234.
       77 BFP-SRT FLOAT-BINARY-7    VALUE 1234.
       77 BFP-LNG FLOAT-BINARY-16   VALUE 1234.
       77 BFP-EXT FLOAT-BINARY-34   VALUE 1234.
       77 DFP-SRT FLOAT-DECIMAL-7   VALUE 1234.
       77 DFP-LNG FLOAT-DECIMAL-16  VALUE 1234.
       77 DFP-EXT FLOAT-DECIMAL-34  VALUE 1234.
       77 HW      COMP PIC S9(4)    VALUE 1234.
       77 FW      COMP PIC S9(9)    VALUE 1234.
       77 DW      COMP PIC S9(18)   VALUE 1234.
       77 QW      COMP PIC S9(39)   VALUE 1234.
       77 PD      COMP-3 PIC S9(31) VALUE 1234.
       77 ZD             PIC S9(31) VALUE 1234.
       77 FLT-SRT USAGE FLOAT-SHORT.
       77 FLT-LNG USAGE FLOAT-LONG.
       77 FLT-EXT USAGE FLOAT-EXTENDED.
       77 HW2     COMP PIC S9(4).
       77 FW2     COMP PIC S9(9).
       77 DW2     COMP PIC S9(18).
       77 QW2     COMP PIC S9(39).
       77 PD2     COMP-3 PIC S9(31).
       77 ZD2            PIC S9(31).
       77 GOOD    COMP-3 PIC 9(4)   VALUE 0.
       PROCEDURE DIVISION.
       MAINLINE.
           DISPLAY 'TESTCMP2 TEST COMPUTE ADDITION'
           COMPUTE FLT-SRT = 
            (HFP-SRT +HFP-SRT)+(HFP-SRT +HFP-LNG)+(HFP-SRT +HFP-EXT)
           +(HFP-SRT +BFP-SRT)+(HFP-SRT +BFP-LNG)+(HFP-SRT +BFP-EXT)
           +(HFP-SRT +DFP-SRT)+(HFP-SRT +DFP-LNG)+(HFP-SRT +DFP-EXT)
           +(HFP-SRT +HW     )+(HFP-SRT +FW     )+(HFP-SRT +DW     )
           +(HFP-SRT +QW     )+(HFP-SRT +PD     )+(HFP-SRT +ZD     )
           -30*1234
           IF FLT-SRT = 0 PERFORM GOOD ELSE PERFORM BAD.
           COMPUTE FLT-LNG = 
            (HFP-LNG +HFP-SRT)+(HFP-LNG +HFP-LNG)+(HFP-LNG +HFP-EXT)
           +(HFP-LNG +BFP-SRT)+(HFP-LNG +BFP-LNG)+(HFP-LNG +BFP-EXT)
           +(HFP-LNG +DFP-SRT)+(HFP-LNG +DFP-LNG)+(HFP-LNG +DFP-EXT)
           +(HFP-LNG +HW     )+(HFP-LNG +FW     )+(HFP-LNG +DW     )
           +(HFP-LNG +QW     )+(HFP-LNG +PD     )+(HFP-LNG +ZD     )
           -30*1234
           IF FLT-LNG = 0 PERFORM GOOD ELSE PERFORM BAD.
           COMPUTE FLT-EXT = 
            (HFP-EXT +HFP-SRT)+(HFP-EXT +HFP-LNG)+(HFP-EXT +HFP-EXT)
           +(HFP-EXT +BFP-SRT)+(HFP-EXT +BFP-LNG)+(HFP-EXT +BFP-EXT)
           +(HFP-EXT +DFP-SRT)+(HFP-EXT +DFP-LNG)+(HFP-EXT +DFP-EXT)
           +(HFP-EXT +HW     )+(HFP-EXT +FW     )+(HFP-EXT +DW     )
           +(HFP-EXT +QW     )+(HFP-EXT +PD     )+(HFP-EXT +ZD     )
           -30*1234
           IF FLT-EXT = 0 PERFORM GOOD ELSE PERFORM BAD.
           COMPUTE FLT-SRT = 
            (BFP-SRT +HFP-SRT)+(BFP-SRT +HFP-LNG)+(BFP-SRT +HFP-EXT)
           +(BFP-SRT +BFP-SRT)+(BFP-SRT +BFP-LNG)+(BFP-SRT +BFP-EXT)
           +(BFP-SRT +DFP-SRT)+(BFP-SRT +DFP-LNG)+(BFP-SRT +DFP-EXT)
           +(BFP-SRT +HW     )+(BFP-SRT +FW     )+(BFP-SRT +DW     )
           +(BFP-SRT +QW     )+(BFP-SRT +PD     )+(BFP-SRT +ZD     )
           -30*1234
           IF FLT-SRT = 0 PERFORM GOOD ELSE PERFORM BAD.
           COMPUTE FLT-SRT = 
            (BFP-LNG +HFP-SRT)+(BFP-LNG +HFP-LNG)+(BFP-LNG +HFP-EXT)
           +(BFP-LNG +BFP-SRT)+(BFP-LNG +BFP-LNG)+(BFP-LNG +BFP-EXT)
           +(BFP-LNG +DFP-SRT)+(BFP-LNG +DFP-LNG)+(BFP-LNG +DFP-EXT)
           +(BFP-LNG +HW     )+(BFP-LNG +FW     )+(BFP-LNG +DW     )
           +(BFP-LNG +QW     )+(BFP-LNG +PD     )+(BFP-LNG +ZD     )
           -30*1234
           IF FLT-SRT = 0 PERFORM GOOD ELSE PERFORM BAD.
           COMPUTE FLT-SRT = 
            (BFP-EXT +HFP-SRT)+(BFP-EXT +HFP-LNG)+(BFP-EXT +HFP-EXT)
           +(BFP-EXT +BFP-SRT)+(BFP-EXT +BFP-LNG)+(BFP-EXT +BFP-EXT)
           +(BFP-EXT +DFP-SRT)+(BFP-EXT +DFP-LNG)+(BFP-EXT +DFP-EXT)
           +(BFP-EXT +HW     )+(BFP-EXT +FW     )+(BFP-EXT +DW     )
           +(BFP-EXT +QW     )+(BFP-EXT +PD     )+(BFP-EXT +ZD     )
           -30*1234
           IF FLT-SRT = 0 PERFORM GOOD ELSE PERFORM BAD.
           COMPUTE FLT-SRT = 
            (DFP-SRT +HFP-SRT)+(DFP-SRT +HFP-LNG)+(DFP-SRT +HFP-EXT)
           +(DFP-SRT +BFP-SRT)+(DFP-SRT +BFP-LNG)+(DFP-SRT +BFP-EXT)
           +(DFP-SRT +DFP-SRT)+(DFP-SRT +DFP-LNG)+(DFP-SRT +DFP-EXT)
           +(DFP-SRT +HW     )+(DFP-SRT +FW     )+(DFP-SRT +DW     )
           +(DFP-SRT +QW     )+(DFP-SRT +PD     )+(DFP-SRT +ZD     )
           -30*1234
           IF FLT-SRT = 0 PERFORM GOOD ELSE PERFORM BAD.
           COMPUTE FLT-SRT = 
            (DFP-LNG +HFP-SRT)+(DFP-LNG +HFP-LNG)+(DFP-LNG +HFP-EXT)
           +(DFP-LNG +BFP-SRT)+(DFP-LNG +BFP-LNG)+(DFP-LNG +BFP-EXT)
           +(DFP-LNG +DFP-SRT)+(DFP-LNG +DFP-LNG)+(DFP-LNG +DFP-EXT)
           +(DFP-LNG +HW     )+(DFP-LNG +FW     )+(DFP-LNG +DW     )
           +(DFP-LNG +QW     )+(DFP-LNG +PD     )+(DFP-LNG +ZD     )
           -30*1234
           IF FLT-SRT = 0 PERFORM GOOD ELSE PERFORM BAD.
           COMPUTE FLT-SRT = 
            (DFP-EXT +HFP-SRT)+(DFP-EXT +HFP-LNG)+(DFP-EXT +HFP-EXT)
           +(DFP-EXT +BFP-SRT)+(DFP-EXT +BFP-LNG)+(DFP-EXT +BFP-EXT)
           +(DFP-EXT +DFP-SRT)+(DFP-EXT +DFP-LNG)+(DFP-EXT +DFP-EXT)
           +(DFP-EXT +HW     )+(DFP-EXT +FW     )+(DFP-EXT +DW     )
           +(DFP-EXT +QW     )+(DFP-EXT +PD     )+(DFP-EXT +ZD     )
           -30*1234
           IF FLT-SRT = 0 PERFORM GOOD ELSE PERFORM BAD.
           COMPUTE FLT-SRT = 
            (HW      +HFP-SRT)+(HW      +HFP-LNG)+(HW      +HFP-EXT)
           +(HW      +BFP-SRT)+(HW      +BFP-LNG)+(HW      +BFP-EXT)
           +(HW      +DFP-SRT)+(HW      +DFP-LNG)+(HW      +DFP-EXT)
           +(HW      +HW     )+(HW      +FW     )+(HW      +DW     )
           +(HW      +QW     )+(HW      +PD     )+(HW      +ZD     )
           -30*1234
           IF FLT-SRT = 0 PERFORM GOOD ELSE PERFORM BAD.
           COMPUTE FLT-SRT = 
            (FW      +HFP-SRT)+(FW      +HFP-LNG)+(FW      +HFP-EXT)
           +(FW      +BFP-SRT)+(FW      +BFP-LNG)+(FW      +BFP-EXT)
           +(FW      +DFP-SRT)+(FW      +DFP-LNG)+(FW      +DFP-EXT)
           +(FW      +HW     )+(FW      +FW     )+(FW      +DW     )
           +(FW      +QW     )+(FW      +PD     )+(FW      +ZD     )
           -30*1234
           IF FLT-SRT = 0 PERFORM GOOD ELSE PERFORM BAD.
           COMPUTE FLT-SRT = 
            (DW      +HFP-SRT)+(DW      +HFP-LNG)+(DW      +HFP-EXT)
           +(DW      +BFP-SRT)+(DW      +BFP-LNG)+(DW      +BFP-EXT)
           +(DW      +DFP-SRT)+(DW      +DFP-LNG)+(DW      +DFP-EXT)
           +(DW      +HW     )+(DW      +FW     )+(DW      +DW     )
           +(DW      +QW     )+(DW      +PD     )+(DW      +ZD     )
           -30*1234
           IF FLT-SRT = 0 PERFORM GOOD ELSE PERFORM BAD.
           COMPUTE FLT-SRT = 
            (QW      +HFP-SRT)+(QW      +HFP-LNG)+(QW      +HFP-EXT)
           +(QW      +BFP-SRT)+(QW      +BFP-LNG)+(QW      +BFP-EXT)
           +(QW      +DFP-SRT)+(QW      +DFP-LNG)+(QW      +DFP-EXT)
           +(QW      +HW     )+(QW      +FW     )+(QW      +DW     )
           +(QW      +QW     )+(QW      +PD     )+(QW      +ZD     )
           -30*1234
           IF FLT-SRT = 0 PERFORM GOOD ELSE PERFORM BAD.
           COMPUTE FLT-SRT = 
            (PD      +HFP-SRT)+(PD      +HFP-LNG)+(PD      +HFP-EXT)
           +(PD      +BFP-SRT)+(PD      +BFP-LNG)+(PD      +BFP-EXT)
           +(PD      +DFP-SRT)+(PD      +DFP-LNG)+(PD      +DFP-EXT)
           +(PD      +HW     )+(PD      +FW     )+(PD      +DW     )
           +(PD      +QW     )+(PD      +PD     )+(PD      +ZD     )
           -30*1234
           IF FLT-SRT = 0 PERFORM GOOD ELSE PERFORM BAD.
           COMPUTE FLT-SRT = 
            (ZD      +HFP-SRT)+(ZD      +HFP-LNG)+(ZD      +HFP-EXT)
           +(ZD      +BFP-SRT)+(ZD      +BFP-LNG)+(ZD      +BFP-EXT)
           +(ZD      +DFP-SRT)+(ZD      +DFP-LNG)+(ZD      +DFP-EXT)
           +(ZD      +HW     )+(ZD      +FW     )+(ZD      +DW     )
           +(ZD      +QW     )+(ZD      +PD     )+(ZD      +ZD     )
           -30*1234
           IF FLT-SRT = 0 PERFORM GOOD ELSE PERFORM BAD.
           DISPLAY 'TESTCMP2 TEST COMPUTE SUBTRACTION'
           COMPUTE FLT-SRT = 
            (HFP-SRT -HFP-SRT)+(HFP-SRT -HFP-LNG)+(HFP-SRT -HFP-EXT)
           +(HFP-SRT -BFP-SRT)+(HFP-SRT -BFP-LNG)+(HFP-SRT -BFP-EXT)
           +(HFP-SRT -DFP-SRT)+(HFP-SRT -DFP-LNG)+(HFP-SRT -DFP-EXT)
           +(HFP-SRT -HW     )+(HFP-SRT -FW     )+(HFP-SRT -DW     )
           +(HFP-SRT -QW     )+(HFP-SRT -PD     )+(HFP-SRT -ZD     )
           IF FLT-SRT = 0 PERFORM GOOD ELSE PERFORM BAD.
           COMPUTE FLT-LNG = 
            (HFP-LNG -HFP-SRT)+(HFP-LNG -HFP-LNG)+(HFP-LNG -HFP-EXT)
           +(HFP-LNG -BFP-SRT)+(HFP-LNG -BFP-LNG)+(HFP-LNG -BFP-EXT)
           +(HFP-LNG -DFP-SRT)+(HFP-LNG -DFP-LNG)+(HFP-LNG -DFP-EXT)
           +(HFP-LNG -HW     )+(HFP-LNG -FW     )+(HFP-LNG -DW     )
           +(HFP-LNG -QW     )+(HFP-LNG -PD     )+(HFP-LNG -ZD     )
           IF FLT-LNG = 0 PERFORM GOOD ELSE PERFORM BAD.
           COMPUTE FLT-EXT = 
            (HFP-EXT -HFP-SRT)+(HFP-EXT -HFP-LNG)+(HFP-EXT -HFP-EXT)
           +(HFP-EXT -BFP-SRT)+(HFP-EXT -BFP-LNG)+(HFP-EXT -BFP-EXT)
           +(HFP-EXT -DFP-SRT)+(HFP-EXT -DFP-LNG)+(HFP-EXT -DFP-EXT)
           +(HFP-EXT -HW     )+(HFP-EXT -FW     )+(HFP-EXT -DW     )
           +(HFP-EXT -QW     )+(HFP-EXT -PD     )+(HFP-EXT -ZD     )
           IF FLT-EXT = 0 PERFORM GOOD ELSE PERFORM BAD.
           COMPUTE FLT-SRT = 
            (BFP-SRT -HFP-SRT)+(BFP-SRT -HFP-LNG)+(BFP-SRT -HFP-EXT)
           +(BFP-SRT -BFP-SRT)+(BFP-SRT -BFP-LNG)+(BFP-SRT -BFP-EXT)
           +(BFP-SRT -DFP-SRT)+(BFP-SRT -DFP-LNG)+(BFP-SRT -DFP-EXT)
           +(BFP-SRT -HW     )+(BFP-SRT -FW     )+(BFP-SRT -DW     )
           +(BFP-SRT -QW     )+(BFP-SRT -PD     )+(BFP-SRT -ZD     )
           IF FLT-SRT = 0 PERFORM GOOD ELSE PERFORM BAD.
           COMPUTE FLT-SRT = 
            (BFP-LNG -HFP-SRT)+(BFP-LNG -HFP-LNG)+(BFP-LNG -HFP-EXT)
           +(BFP-LNG -BFP-SRT)+(BFP-LNG -BFP-LNG)+(BFP-LNG -BFP-EXT)
           +(BFP-LNG -DFP-SRT)+(BFP-LNG -DFP-LNG)+(BFP-LNG -DFP-EXT)
           +(BFP-LNG -HW     )+(BFP-LNG -FW     )+(BFP-LNG -DW     )
           +(BFP-LNG -QW     )+(BFP-LNG -PD     )+(BFP-LNG -ZD     )
           IF FLT-SRT = 0 PERFORM GOOD ELSE PERFORM BAD.
           COMPUTE FLT-SRT = 
            (BFP-EXT -HFP-SRT)+(BFP-EXT -HFP-LNG)+(BFP-EXT -HFP-EXT)
           +(BFP-EXT -BFP-SRT)+(BFP-EXT -BFP-LNG)+(BFP-EXT -BFP-EXT)
           +(BFP-EXT -DFP-SRT)+(BFP-EXT -DFP-LNG)+(BFP-EXT -DFP-EXT)
           +(BFP-EXT -HW     )+(BFP-EXT -FW     )+(BFP-EXT -DW     )
           +(BFP-EXT -QW     )+(BFP-EXT -PD     )+(BFP-EXT -ZD     )
           IF FLT-SRT = 0 PERFORM GOOD ELSE PERFORM BAD.
           COMPUTE FLT-SRT = 
            (DFP-SRT -HFP-SRT)+(DFP-SRT -HFP-LNG)+(DFP-SRT -HFP-EXT)
           +(DFP-SRT -BFP-SRT)+(DFP-SRT -BFP-LNG)+(DFP-SRT -BFP-EXT)
           +(DFP-SRT -DFP-SRT)+(DFP-SRT -DFP-LNG)+(DFP-SRT -DFP-EXT)
           +(DFP-SRT -HW     )+(DFP-SRT -FW     )+(DFP-SRT -DW     )
           +(DFP-SRT -QW     )+(DFP-SRT -PD     )+(DFP-SRT -ZD     )
           IF FLT-SRT = 0 PERFORM GOOD ELSE PERFORM BAD.
           COMPUTE FLT-SRT = 
            (DFP-LNG -HFP-SRT)+(DFP-LNG -HFP-LNG)+(DFP-LNG -HFP-EXT)
           +(DFP-LNG -BFP-SRT)+(DFP-LNG -BFP-LNG)+(DFP-LNG -BFP-EXT)
           +(DFP-LNG -DFP-SRT)+(DFP-LNG -DFP-LNG)+(DFP-LNG -DFP-EXT)
           +(DFP-LNG -HW     )+(DFP-LNG -FW     )+(DFP-LNG -DW     )
           +(DFP-LNG -QW     )+(DFP-LNG -PD     )+(DFP-LNG -ZD     )
           IF FLT-SRT = 0 PERFORM GOOD ELSE PERFORM BAD.
           COMPUTE FLT-SRT = 
            (DFP-EXT -HFP-SRT)+(DFP-EXT -HFP-LNG)+(DFP-EXT -HFP-EXT)
           +(DFP-EXT -BFP-SRT)+(DFP-EXT -BFP-LNG)+(DFP-EXT -BFP-EXT)
           +(DFP-EXT -DFP-SRT)+(DFP-EXT -DFP-LNG)+(DFP-EXT -DFP-EXT)
           +(DFP-EXT -HW     )+(DFP-EXT -FW     )+(DFP-EXT -DW     )
           +(DFP-EXT -QW     )+(DFP-EXT -PD     )+(DFP-EXT -ZD     )
           IF FLT-SRT = 0 PERFORM GOOD ELSE PERFORM BAD.
           COMPUTE FLT-SRT = 
            (HW      -HFP-SRT)+(HW      -HFP-LNG)+(HW      -HFP-EXT)
           +(HW      -BFP-SRT)+(HW      -BFP-LNG)+(HW      -BFP-EXT)
           +(HW      -DFP-SRT)+(HW      -DFP-LNG)+(HW      -DFP-EXT)
           +(HW      -HW     )+(HW      -FW     )+(HW      -DW     )
           +(HW      -QW     )+(HW      -PD     )+(HW      -ZD     )
           IF FLT-SRT = 0 PERFORM GOOD ELSE PERFORM BAD.
           COMPUTE FLT-SRT = 
            (FW      -HFP-SRT)+(FW      -HFP-LNG)+(FW      -HFP-EXT)
           +(FW      -BFP-SRT)+(FW      -BFP-LNG)+(FW      -BFP-EXT)
           +(FW      -DFP-SRT)+(FW      -DFP-LNG)+(FW      -DFP-EXT)
           +(FW      -HW     )+(FW      -FW     )+(FW      -DW     )
           +(FW      -QW     )+(FW      -PD     )+(FW      -ZD     )
           IF FLT-SRT = 0 PERFORM GOOD ELSE PERFORM BAD.
           COMPUTE FLT-SRT = 
            (DW      -HFP-SRT)+(DW      -HFP-LNG)+(DW      -HFP-EXT)
           +(DW      -BFP-SRT)+(DW      -BFP-LNG)+(DW      -BFP-EXT)
           +(DW      -DFP-SRT)+(DW      -DFP-LNG)+(DW      -DFP-EXT)
           +(DW      -HW     )+(DW      -FW     )+(DW      -DW     )
           +(DW      -QW     )+(DW      -PD     )+(DW      -ZD     )
           IF FLT-SRT = 0 PERFORM GOOD ELSE PERFORM BAD.
           COMPUTE FLT-SRT = 
            (QW      -HFP-SRT)+(QW      -HFP-LNG)+(QW      -HFP-EXT)
           +(QW      -BFP-SRT)+(QW      -BFP-LNG)+(QW      -BFP-EXT)
           +(QW      -DFP-SRT)+(QW      -DFP-LNG)+(QW      -DFP-EXT)
           +(QW      -HW     )+(QW      -FW     )+(QW      -DW     )
           +(QW      -QW     )+(QW      -PD     )+(QW      -ZD     )
           IF FLT-SRT = 0 PERFORM GOOD ELSE PERFORM BAD.
           COMPUTE FLT-SRT = 
            (PD      -HFP-SRT)+(PD      -HFP-LNG)+(PD      -HFP-EXT)
           +(PD      -BFP-SRT)+(PD      -BFP-LNG)+(PD      -BFP-EXT)
           +(PD      -DFP-SRT)+(PD      -DFP-LNG)+(PD      -DFP-EXT)
           +(PD      -HW     )+(PD      -FW     )+(PD      -DW     )
           +(PD      -QW     )+(PD      -PD     )+(PD      -ZD     )
           IF FLT-SRT = 0 PERFORM GOOD ELSE PERFORM BAD.
           COMPUTE FLT-SRT = 
            (ZD      -HFP-SRT)+(ZD      -HFP-LNG)+(ZD      -HFP-EXT)
           +(ZD      -BFP-SRT)+(ZD      -BFP-LNG)+(ZD      -BFP-EXT)
           +(ZD      -DFP-SRT)+(ZD      -DFP-LNG)+(ZD      -DFP-EXT)
           +(ZD      -HW     )+(ZD      -FW     )+(ZD      -DW     )
           +(ZD      -QW     )+(ZD      -PD     )+(ZD      -ZD     )
           IF FLT-SRT = 0 PERFORM GOOD ELSE PERFORM BAD.
           DISPLAY 'TESTCMP2 TEST COMPUTE MULTIPLICATION'
           MOVE 2 TO        
             HFP-SRT HFP-LNG HFP-EXT
             BFP-SRT BFP-LNG BFP-EXT
             DFP-SRT DFP-LNG DFP-EXT
             HW      FW      DW
             QW      PD      ZD
           COMPUTE FLT-SRT = 
            (HFP-SRT *HFP-SRT)+(HFP-SRT *HFP-LNG)+(HFP-SRT *HFP-EXT)
           +(HFP-SRT *BFP-SRT)+(HFP-SRT *BFP-LNG)+(HFP-SRT *BFP-EXT)
           +(HFP-SRT *DFP-SRT)+(HFP-SRT *DFP-LNG)+(HFP-SRT *DFP-EXT)
           +(HFP-SRT *HW     )+(HFP-SRT *FW     )+(HFP-SRT *DW     )
           +(HFP-SRT *QW     )+(HFP-SRT *PD     )+(HFP-SRT *ZD     )
           -15*2*2
           IF FLT-SRT = 0 PERFORM GOOD ELSE PERFORM BAD.
           COMPUTE FLT-LNG = 
            (HFP-LNG *HFP-SRT)+(HFP-LNG *HFP-LNG)+(HFP-LNG *HFP-EXT)
           +(HFP-LNG *BFP-SRT)+(HFP-LNG *BFP-LNG)+(HFP-LNG *BFP-EXT)
           +(HFP-LNG *DFP-SRT)+(HFP-LNG *DFP-LNG)+(HFP-LNG *DFP-EXT)
           +(HFP-LNG *HW     )+(HFP-LNG *FW     )+(HFP-LNG *DW     )
           +(HFP-LNG *QW     )+(HFP-LNG *PD     )+(HFP-LNG *ZD     )
           -15*2*2
           IF FLT-LNG = 0 PERFORM GOOD ELSE PERFORM BAD.
           COMPUTE FLT-EXT = 
            (HFP-EXT *HFP-SRT)+(HFP-EXT *HFP-LNG)+(HFP-EXT *HFP-EXT)
           +(HFP-EXT *BFP-SRT)+(HFP-EXT *BFP-LNG)+(HFP-EXT *BFP-EXT)
           +(HFP-EXT *DFP-SRT)+(HFP-EXT *DFP-LNG)+(HFP-EXT *DFP-EXT)
           +(HFP-EXT *HW     )+(HFP-EXT *FW     )+(HFP-EXT *DW     )
           +(HFP-EXT *QW     )+(HFP-EXT *PD     )+(HFP-EXT *ZD     )
           -15*2*2
           IF FLT-EXT = 0 PERFORM GOOD ELSE PERFORM BAD.
           COMPUTE FLT-SRT = 
            (BFP-SRT *HFP-SRT)+(BFP-SRT *HFP-LNG)+(BFP-SRT *HFP-EXT)
           +(BFP-SRT *BFP-SRT)+(BFP-SRT *BFP-LNG)+(BFP-SRT *BFP-EXT)
           +(BFP-SRT *DFP-SRT)+(BFP-SRT *DFP-LNG)+(BFP-SRT *DFP-EXT)
           +(BFP-SRT *HW     )+(BFP-SRT *FW     )+(BFP-SRT *DW     )
           +(BFP-SRT *QW     )+(BFP-SRT *PD     )+(BFP-SRT *ZD     )
           -15*2*2
           IF FLT-SRT = 0 PERFORM GOOD ELSE PERFORM BAD.
           COMPUTE FLT-SRT = 
            (BFP-LNG *HFP-SRT)+(BFP-LNG *HFP-LNG)+(BFP-LNG *HFP-EXT)
           +(BFP-LNG *BFP-SRT)+(BFP-LNG *BFP-LNG)+(BFP-LNG *BFP-EXT)
           +(BFP-LNG *DFP-SRT)+(BFP-LNG *DFP-LNG)+(BFP-LNG *DFP-EXT)
           +(BFP-LNG *HW     )+(BFP-LNG *FW     )+(BFP-LNG *DW     )
           +(BFP-LNG *QW     )+(BFP-LNG *PD     )+(BFP-LNG *ZD     )
           -15*2*2
           IF FLT-SRT = 0 PERFORM GOOD ELSE PERFORM BAD.
           COMPUTE FLT-SRT = 
            (BFP-EXT *HFP-SRT)+(BFP-EXT *HFP-LNG)+(BFP-EXT *HFP-EXT)
           +(BFP-EXT *BFP-SRT)+(BFP-EXT *BFP-LNG)+(BFP-EXT *BFP-EXT)
           +(BFP-EXT *DFP-SRT)+(BFP-EXT *DFP-LNG)+(BFP-EXT *DFP-EXT)
           +(BFP-EXT *HW     )+(BFP-EXT *FW     )+(BFP-EXT *DW     )
           +(BFP-EXT *QW     )+(BFP-EXT *PD     )+(BFP-EXT *ZD     )
           -15*2*2
           IF FLT-SRT = 0 PERFORM GOOD ELSE PERFORM BAD.
           COMPUTE FLT-SRT = 
            (DFP-SRT *HFP-SRT)+(DFP-SRT *HFP-LNG)+(DFP-SRT *HFP-EXT)
           +(DFP-SRT *BFP-SRT)+(DFP-SRT *BFP-LNG)+(DFP-SRT *BFP-EXT)
           +(DFP-SRT *DFP-SRT)+(DFP-SRT *DFP-LNG)+(DFP-SRT *DFP-EXT)
           +(DFP-SRT *HW     )+(DFP-SRT *FW     )+(DFP-SRT *DW     )
           +(DFP-SRT *QW     )+(DFP-SRT *PD     )+(DFP-SRT *ZD     )
           -15*2*2
           IF FLT-SRT = 0 PERFORM GOOD ELSE PERFORM BAD.
           COMPUTE FLT-SRT = 
            (DFP-LNG *HFP-SRT)+(DFP-LNG *HFP-LNG)+(DFP-LNG *HFP-EXT)
           +(DFP-LNG *BFP-SRT)+(DFP-LNG *BFP-LNG)+(DFP-LNG *BFP-EXT)
           +(DFP-LNG *DFP-SRT)+(DFP-LNG *DFP-LNG)+(DFP-LNG *DFP-EXT)
           +(DFP-LNG *HW     )+(DFP-LNG *FW     )+(DFP-LNG *DW     )
           +(DFP-LNG *QW     )+(DFP-LNG *PD     )+(DFP-LNG *ZD     )
           -15*2*2
           IF FLT-SRT = 0 PERFORM GOOD ELSE PERFORM BAD.
           COMPUTE FLT-SRT = 
            (DFP-EXT *HFP-SRT)+(DFP-EXT *HFP-LNG)+(DFP-EXT *HFP-EXT)
           +(DFP-EXT *BFP-SRT)+(DFP-EXT *BFP-LNG)+(DFP-EXT *BFP-EXT)
           +(DFP-EXT *DFP-SRT)+(DFP-EXT *DFP-LNG)+(DFP-EXT *DFP-EXT)
           +(DFP-EXT *HW     )+(DFP-EXT *FW     )+(DFP-EXT *DW     )
           +(DFP-EXT *QW     )+(DFP-EXT *PD     )+(DFP-EXT *ZD     )
           -15*2*2
           IF FLT-SRT = 0 PERFORM GOOD ELSE PERFORM BAD.
           COMPUTE FLT-SRT = 
            (HW      *HFP-SRT)+(HW      *HFP-LNG)+(HW      *HFP-EXT)
           +(HW      *BFP-SRT)+(HW      *BFP-LNG)+(HW      *BFP-EXT)
           +(HW      *DFP-SRT)+(HW      *DFP-LNG)+(HW      *DFP-EXT)
           +(HW      *HW     )+(HW      *FW     )+(HW      *DW     )
           +(HW      *QW     )+(HW      *PD     )+(HW      *ZD     )
           -15*2*2
           IF FLT-SRT = 0 PERFORM GOOD ELSE PERFORM BAD.
           COMPUTE FLT-SRT = 
            (FW      *HFP-SRT)+(FW      *HFP-LNG)+(FW      *HFP-EXT)
           +(FW      *BFP-SRT)+(FW      *BFP-LNG)+(FW      *BFP-EXT)
           +(FW      *DFP-SRT)+(FW      *DFP-LNG)+(FW      *DFP-EXT)
           +(FW      *HW     )+(FW      *FW     )+(FW      *DW     )
           +(FW      *QW     )+(FW      *PD     )+(FW      *ZD     )
           -15*2*2
           IF FLT-SRT = 0 PERFORM GOOD ELSE PERFORM BAD.
           COMPUTE FLT-SRT = 
            (DW      *HFP-SRT)+(DW      *HFP-LNG)+(DW      *HFP-EXT)
           +(DW      *BFP-SRT)+(DW      *BFP-LNG)+(DW      *BFP-EXT)
           +(DW      *DFP-SRT)+(DW      *DFP-LNG)+(DW      *DFP-EXT)
           +(DW      *HW     )+(DW      *FW     )+(DW      *DW     )
           +(DW      *QW     )+(DW      *PD     )+(DW      *ZD     )
           -15*2*2
           IF FLT-SRT = 0 PERFORM GOOD ELSE PERFORM BAD.
           COMPUTE FLT-SRT = 
            (QW      *HFP-SRT)+(QW      *HFP-LNG)+(QW      *HFP-EXT)
           +(QW      *BFP-SRT)+(QW      *BFP-LNG)+(QW      *BFP-EXT)
           +(QW      *DFP-SRT)+(QW      *DFP-LNG)+(QW      *DFP-EXT)
           +(QW      *HW     )+(QW      *FW     )+(QW      *DW     )
           +(QW      *QW     )+(QW      *PD     )+(QW      *ZD     )
           -15*2*2
           IF FLT-SRT = 0 PERFORM GOOD ELSE PERFORM BAD.
           COMPUTE FLT-SRT = 
            (PD      *HFP-SRT)+(PD      *HFP-LNG)+(PD      *HFP-EXT)
           +(PD      *BFP-SRT)+(PD      *BFP-LNG)+(PD      *BFP-EXT)
           +(PD      *DFP-SRT)+(PD      *DFP-LNG)+(PD      *DFP-EXT)
           +(PD      *HW     )+(PD      *FW     )+(PD      *DW     )
           +(PD      *QW     )+(PD      *PD     )+(PD      *ZD     )
           -15*2*2
           IF FLT-SRT = 0 PERFORM GOOD ELSE PERFORM BAD.
           COMPUTE FLT-SRT = 
            (ZD      *HFP-SRT)+(ZD      *HFP-LNG)+(ZD      *HFP-EXT)
           +(ZD      *BFP-SRT)+(ZD      *BFP-LNG)+(ZD      *BFP-EXT)
           +(ZD      *DFP-SRT)+(ZD      *DFP-LNG)+(ZD      *DFP-EXT)
           +(ZD      *HW     )+(ZD      *FW     )+(ZD      *DW     )
           +(ZD      *QW     )+(ZD      *PD     )+(ZD      *ZD     )
           -15*2*2
           IF FLT-SRT = 0 PERFORM GOOD ELSE PERFORM BAD.
           DISPLAY 'TESTCMP2 TEST COMPUTE DIVIDE'
           COMPUTE FLT-SRT = 
            (HFP-SRT /HFP-SRT)+(HFP-SRT /HFP-LNG)+(HFP-SRT /HFP-EXT)
           +(HFP-SRT /BFP-SRT)+(HFP-SRT /BFP-LNG)+(HFP-SRT /BFP-EXT)
           +(HFP-SRT /DFP-SRT)+(HFP-SRT /DFP-LNG)+(HFP-SRT /DFP-EXT)
           +(HFP-SRT /HW     )+(HFP-SRT /FW     )+(HFP-SRT /DW     )
           +(HFP-SRT /QW     )+(HFP-SRT /PD     )+(HFP-SRT /ZD     )
           -15
           IF FLT-SRT = 0 PERFORM GOOD ELSE PERFORM BAD.
           COMPUTE FLT-LNG = 
            (HFP-LNG /HFP-SRT)+(HFP-LNG /HFP-LNG)+(HFP-LNG /HFP-EXT)
           +(HFP-LNG /BFP-SRT)+(HFP-LNG /BFP-LNG)+(HFP-LNG /BFP-EXT)
           +(HFP-LNG /DFP-SRT)+(HFP-LNG /DFP-LNG)+(HFP-LNG /DFP-EXT)
           +(HFP-LNG /HW     )+(HFP-LNG /FW     )+(HFP-LNG /DW     )
           +(HFP-LNG /QW     )+(HFP-LNG /PD     )+(HFP-LNG /ZD     )
           -15     
           IF FLT-LNG = 0 PERFORM GOOD ELSE PERFORM BAD.
           COMPUTE FLT-EXT = 
            (HFP-EXT /HFP-SRT)+(HFP-EXT /HFP-LNG)+(HFP-EXT /HFP-EXT)
           +(HFP-EXT /BFP-SRT)+(HFP-EXT /BFP-LNG)+(HFP-EXT /BFP-EXT)
           +(HFP-EXT /DFP-SRT)+(HFP-EXT /DFP-LNG)+(HFP-EXT /DFP-EXT)
           +(HFP-EXT /HW     )+(HFP-EXT /FW     )+(HFP-EXT /DW     )
           +(HFP-EXT /QW     )+(HFP-EXT /PD     )+(HFP-EXT /ZD     )
           -15       
           IF FLT-EXT = 0 PERFORM GOOD ELSE PERFORM BAD.
           COMPUTE FLT-SRT = 
            (BFP-SRT /HFP-SRT)+(BFP-SRT /HFP-LNG)+(BFP-SRT /HFP-EXT)
           +(BFP-SRT /BFP-SRT)+(BFP-SRT /BFP-LNG)+(BFP-SRT /BFP-EXT)
           +(BFP-SRT /DFP-SRT)+(BFP-SRT /DFP-LNG)+(BFP-SRT /DFP-EXT)
           +(BFP-SRT /HW     )+(BFP-SRT /FW     )+(BFP-SRT /DW     )
           +(BFP-SRT /QW     )+(BFP-SRT /PD     )+(BFP-SRT /ZD     )
           -15     
           IF FLT-SRT = 0 PERFORM GOOD ELSE PERFORM BAD.
           COMPUTE FLT-SRT = 
            (BFP-LNG /HFP-SRT)+(BFP-LNG /HFP-LNG)+(BFP-LNG /HFP-EXT)
           +(BFP-LNG /BFP-SRT)+(BFP-LNG /BFP-LNG)+(BFP-LNG /BFP-EXT)
           +(BFP-LNG /DFP-SRT)+(BFP-LNG /DFP-LNG)+(BFP-LNG /DFP-EXT)
           +(BFP-LNG /HW     )+(BFP-LNG /FW     )+(BFP-LNG /DW     )
           +(BFP-LNG /QW     )+(BFP-LNG /PD     )+(BFP-LNG /ZD     )
           -15     
           IF FLT-SRT = 0 PERFORM GOOD ELSE PERFORM BAD.
           COMPUTE FLT-SRT = 
            (BFP-EXT /HFP-SRT)+(BFP-EXT /HFP-LNG)+(BFP-EXT /HFP-EXT)
           +(BFP-EXT /BFP-SRT)+(BFP-EXT /BFP-LNG)+(BFP-EXT /BFP-EXT)
           +(BFP-EXT /DFP-SRT)+(BFP-EXT /DFP-LNG)+(BFP-EXT /DFP-EXT)
           +(BFP-EXT /HW     )+(BFP-EXT /FW     )+(BFP-EXT /DW     )
           +(BFP-EXT /QW     )+(BFP-EXT /PD     )+(BFP-EXT /ZD     )
           -15       
           IF FLT-SRT = 0 PERFORM GOOD ELSE PERFORM BAD.
           COMPUTE FLT-SRT = 
            (DFP-SRT /HFP-SRT)+(DFP-SRT /HFP-LNG)+(DFP-SRT /HFP-EXT)
           +(DFP-SRT /BFP-SRT)+(DFP-SRT /BFP-LNG)+(DFP-SRT /BFP-EXT)
           +(DFP-SRT /DFP-SRT)+(DFP-SRT /DFP-LNG)+(DFP-SRT /DFP-EXT)
           +(DFP-SRT /HW     )+(DFP-SRT /FW     )+(DFP-SRT /DW     )
           +(DFP-SRT /QW     )+(DFP-SRT /PD     )+(DFP-SRT /ZD     )
           -15     
           IF FLT-SRT = 0 PERFORM GOOD ELSE PERFORM BAD.
           COMPUTE FLT-SRT = 
            (DFP-LNG /HFP-SRT)+(DFP-LNG /HFP-LNG)+(DFP-LNG /HFP-EXT)
           +(DFP-LNG /BFP-SRT)+(DFP-LNG /BFP-LNG)+(DFP-LNG /BFP-EXT)
           +(DFP-LNG /DFP-SRT)+(DFP-LNG /DFP-LNG)+(DFP-LNG /DFP-EXT)
           +(DFP-LNG /HW     )+(DFP-LNG /FW     )+(DFP-LNG /DW     )
           +(DFP-LNG /QW     )+(DFP-LNG /PD     )+(DFP-LNG /ZD     )
           -15     
           IF FLT-SRT = 0 PERFORM GOOD ELSE PERFORM BAD.
           COMPUTE FLT-SRT = 
            (DFP-EXT /HFP-SRT)+(DFP-EXT /HFP-LNG)+(DFP-EXT /HFP-EXT)
           +(DFP-EXT /BFP-SRT)+(DFP-EXT /BFP-LNG)+(DFP-EXT /BFP-EXT)
           +(DFP-EXT /DFP-SRT)+(DFP-EXT /DFP-LNG)+(DFP-EXT /DFP-EXT)
           +(DFP-EXT /HW     )+(DFP-EXT /FW     )+(DFP-EXT /DW     )
           +(DFP-EXT /QW     )+(DFP-EXT /PD     )+(DFP-EXT /ZD     )
           -15       
           IF FLT-SRT = 0 PERFORM GOOD ELSE PERFORM BAD.
           COMPUTE FLT-SRT = 
            (HW      /HFP-SRT)+(HW      /HFP-LNG)+(HW      /HFP-EXT)
           +(HW      /BFP-SRT)+(HW      /BFP-LNG)+(HW      /BFP-EXT)
           +(HW      /DFP-SRT)+(HW      /DFP-LNG)+(HW      /DFP-EXT)
           +(HW      /HW     )+(HW      /FW     )+(HW      /DW     )
           +(HW      /QW     )+(HW      /PD     )+(HW      /ZD     )
           -15       
           IF FLT-SRT = 0 PERFORM GOOD ELSE PERFORM BAD.
           COMPUTE FLT-SRT = 
            (FW      /HFP-SRT)+(FW      /HFP-LNG)+(FW      /HFP-EXT)
           +(FW      /BFP-SRT)+(FW      /BFP-LNG)+(FW      /BFP-EXT)
           +(FW      /DFP-SRT)+(FW      /DFP-LNG)+(FW      /DFP-EXT)
           +(FW      /HW     )+(FW      /FW     )+(FW      /DW     )
           +(FW      /QW     )+(FW      /PD     )+(FW      /ZD     )
           -15       
           IF FLT-SRT = 0 PERFORM GOOD ELSE PERFORM BAD.
           COMPUTE FLT-SRT = 
            (DW      /HFP-SRT)+(DW      /HFP-LNG)+(DW      /HFP-EXT)
           +(DW      /BFP-SRT)+(DW      /BFP-LNG)+(DW      /BFP-EXT)
           +(DW      /DFP-SRT)+(DW      /DFP-LNG)+(DW      /DFP-EXT)
           +(DW      /HW     )+(DW      /FW     )+(DW      /DW     )
           +(DW      /QW     )+(DW      /PD     )+(DW      /ZD     )
           -15     
           IF FLT-SRT = 0 PERFORM GOOD ELSE PERFORM BAD.
           COMPUTE FLT-SRT = 
            (QW      /HFP-SRT)+(QW      /HFP-LNG)+(QW      /HFP-EXT)
           +(QW      /BFP-SRT)+(QW      /BFP-LNG)+(QW      /BFP-EXT)
           +(QW      /DFP-SRT)+(QW      /DFP-LNG)+(QW      /DFP-EXT)
           +(QW      /HW     )+(QW      /FW     )+(QW      /DW     )
           +(QW      /QW     )+(QW      /PD     )+(QW      /ZD     )
           -15     
           IF FLT-SRT = 0 PERFORM GOOD ELSE PERFORM BAD.
           COMPUTE FLT-SRT = 
            (PD      /HFP-SRT)+(PD      /HFP-LNG)+(PD      /HFP-EXT)
           +(PD      /BFP-SRT)+(PD      /BFP-LNG)+(PD      /BFP-EXT)
           +(PD      /DFP-SRT)+(PD      /DFP-LNG)+(PD      /DFP-EXT)
           +(PD      /HW     )+(PD      /FW     )+(PD      /DW     )
           +(PD      /QW     )+(PD      /PD     )+(PD      /ZD     )
           -15     
           IF FLT-SRT = 0 PERFORM GOOD ELSE PERFORM BAD.
           COMPUTE FLT-SRT = 
            (ZD      /HFP-SRT)+(ZD      /HFP-LNG)+(ZD      /HFP-EXT)
           +(ZD      /BFP-SRT)+(ZD      /BFP-LNG)+(ZD      /BFP-EXT)
           +(ZD      /DFP-SRT)+(ZD      /DFP-LNG)+(ZD      /DFP-EXT)
           +(ZD      /HW     )+(ZD      /FW     )+(ZD      /DW     )
           +(ZD      /QW     )+(ZD      /PD     )+(ZD      /ZD     )
           -15     
           IF FLT-SRT = 0 PERFORM GOOD ELSE PERFORM BAD.
           DISPLAY 'TESTCMP2 GOOD TESTS =' GOOD
           STOP RUN.
       GOOD.
           ADD 1 TO GOOD.
       BAD.
           MOVE 16 TO RETURN-CODE
           STOP RUN.
