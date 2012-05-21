       IDENTIFICATION DIVISION.
       PROGRAM-ID.    COMPSUM
       AUTHOR.        DON HIGGINS.
       DATE-WRITTEN.  05/12/12.
      * SUBROUTINE CALLED TO ADD 15 DATA FIELDS AND RETURN SUM.                              
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       LINKAGE SECTION.
       01 ZDATA.
          05 EH1 FLOAT-HEX-7.
          05 DH1 FLOAT-HEX-15.
          05 LH1 FLOAT-HEX-30.
          05 EB1 FLOAT-BINARY-7.
          05 DB1 FLOAT-BINARY-16.
          05 LB1 FLOAT-BINARY-34.
          05 ED1 FLOAT-DECIMAL-7.
          05 DD1 FLOAT-DECIMAL-16.
          05 LD1 FLOAT-DECIMAL-34.
          05 H1      COMP PIC S9(3)V9.
          05 F1      COMP PIC S9(7)V99.
          05 G1      COMP PIC S9(15)V999.
          05 Q1      COMP PIC S9(35)V9999.
          05 P1      COMP-3 PIC S9(26)V9(5).
          05 Z1             PIC S9(25)V9(6).
          05 PSUM    COMP-3 PIC S9(4)V9.
       PROCEDURE DIVISION USING ZDATA.
           COMPUTE PSUM = EH1+DH1+LH1
                         +EB1+DB1+LB1  
                         +ED1+DD1+LD1
                         +H1+F1+G1
                         +Q1+P1+Z1           .
           GOBACK.