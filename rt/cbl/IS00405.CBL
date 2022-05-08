         IDENTIFICATION DIVISION.
         PROGRAM-ID.  IS00405.
         ENVIRONMENT DIVISION.
         INPUT-OUTPUT SECTION.
         FILE-CONTROL.

             SELECT SQL-RESULT-FILE
                 ASSIGN TO 'IS00405.LOG'
                 ORGANIZATION IS LINE SEQUENTIAL.

         DATA DIVISION.
         FILE SECTION.
         FD  SQL-RESULT-FILE
             DATA RECORD IS SQL-RESULT-RECORD.
         01  SQL-RESULT-RECORD.
             03  a1 PIC 9.9.
             03  a2 PIC 9.9.
             03  a3 PIC 9.9.
             03  a4 PIC 9.9.

         WORKING-STORAGE SECTION.
         01  W2 PIC 9V9 VALUE 1.2.
         01  W3 PIC 9V9 COMP-3 VALUE 1.2.
         01  W4 PIC 9V9 COMP VALUE 1.2.
         LINKAGE SECTION.
         PROCEDURE DIVISION.
         MAINLINE.

             OPEN outPUT SQL-RESULT-FILE.
             move 1.2 to a1.
             display '>'w2'<'.
             display '>'w3'<'.
             display '>'w4'<'.
             move W2 to a2.
             move W3 to a3.
             move W4 to a4.
             write SQL-RESULT-RECORD.
             CLOSE SQL-RESULT-FILE.
             STOP RUN.
