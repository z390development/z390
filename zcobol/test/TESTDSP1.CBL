       IDENTIFICATION DIVISION.
       PROGRAM-ID.    TESTDSP1.
       AUTHOR.        DON HIGGINS.
       DATE-WRITTEN.  11/28/08.
      * TEST DISPLAY OF LITERLS AND ALL FIELD TYPES                         
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77 TA USAGE POINTER VALUE 1.
       77 TD COMP-1 VALUE 1.234567.  
       77 TE COMP-2 VALUE 1.234567890123456.             
      *77 TL COMP-L VALUE 1.234567890123456789012345678901234.             
       77 TF COMP   PIC S9(9)  VALUE 123456789.       
       77 TG COMP   PIC S9(18) VALUE 123456789012345678.
       77 TH COMP   PIC S9(4)  VALUE 1234.
       77 TP COMP-3 PIC S9(31) VALUE 1234567890123456789012345678901.
       77 TP3 COMP-3 PIC S9(3) VALUE 123.
       77 TP4 COMP-3 PIC S9(4) VALUE 1234.
       77 TX        PIC X(5)   VALUE 'ABCDE'
       77 TZ        PIC S9(31) VALUE 1234567890123456789012345678901.
       77 TZ3       PIC S9(3)  VALUE 123.
       77 TZ4       PIC S9(4)  VALUE 1234.
       77 I  COMP   PIC S9(4).
       PROCEDURE DIVISION.
       MAINLINE.
           DISPLAY 'TESTDSP1 DISPLAY LITERALS AND ALL FIELD TYPES'
           DISPLAY 'TA =' TA
           DISPLAY 'TD =' TD
           DISPLAY 'TE =' TE
           DISPLAY 'TF =' TF
           DISPLAY 'TG =' TG
           DISPLAY 'TH =' TH
      *     DISPLAY 'TL=' TL
           DISPLAY 'TP =' TP
           DISPLAY 'TP3=' TP3
           DISPLAY 'TP4=' TP4
           DISPLAY 'TX =' TX
           DISPLAY 'TZ =' TZ
           DISPLAY 'TZ =' TZ3
           DISPLAY 'TZ4=' TZ4
           DISPLAY '  TH' '  TH' '  TH'
           DISPLAY TH TH TH
           MOVE    -1 TO TA
           MOVE    -1 TO TD
           MOVE    -1 TO TE
           MOVE    -1 TO TF
           MOVE    -1 TO TG
           MOVE    -1 TO TH
      *     MOVE    -1 TP TL
           MOVE    -1 TO TP
           MOVE    -1 TO TZ
           DISPLAY 'DISPLAY -1 FOR EACH TYPE'
           DISPLAY 'TA=' TA
           DISPLAY 'TD=' TD
           DISPLAY 'TE=' TE
           DISPLAY 'TF=' TF
           DISPLAY 'TG=' TG
           DISPLAY 'TH=' TH
      *     DISPLAY 'TH=' TL
           DISPLAY 'TP=' TP
           DISPLAY 'TX=' TX
           DISPLAY 'TZ=' TZ
           DISPLAY 'TESTDSP1 ENDED OK'
           STOP RUN.
          
