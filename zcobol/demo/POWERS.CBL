      * Author.  Don Higgins
      * Date.    06/29/06
      * Remarks. COBOL Powers of 2
       IDENTIFICATION DIVISION.
       PROGRAM-ID. POWERS.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 POWER  PIC S9(3)  COMP-3 VALUE 0.
       01 PNUM   PIC S9(18) COMP-3 VALUE 1.
       PROCEDURE DIVISION.
           PERFORM UNTIL POWER EQUALS 32
               DISPLAY 'POWER OF 2**' POWER ' = ' PNUM
               ADD PNUM TO PNUM
               ADD 1    TO POWER
           END-PERFORM.
