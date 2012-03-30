       IDENTIFICATION DIVISION.
       PROGRAM-ID.  FORMAT-DATE-EXAMPLE.
       AUTHOR.      BILL ROGERS.
      * http://web.sxu.edu/rogers/cobol/datetime.html
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 SYSTEM-WEEKDAY            PIC 9.

       01 SYSTEM-DATE.
          02 SYSTEM-YEAR            PIC 99.
          02 SYSTEM-MONTH           PIC 99.
          02 SYSTEM-DAY             PIC 99.

       01 SYSTEM-TIME.
          02 SYSTEM-HOUR            PIC 99.
          02 SYSTEM-MINUTE          PIC 99.
          02 SYSTEM-SECOND          PIC 99.
          02 SYSTEM-HUNDREDTH       PIC 99.

       01 HEADER-LINE.
          02 HEADER-WEEKDAY         PIC X(3).
          02                        PIC X        VALUE SPACE.
          02 HEADER-MONTH           PIC X(3).
          02                        PIC X        VALUE SPACE.
          02 HEADER-DAY             PIC Z9.
          02                        PIC X        VALUE SPACE.
          02 HEADER-HOUR            PIC Z9.
          02                        PIC X        VALUE ':'.
          02 HEADER-MINUTE          PIC 99.
          02                        PIC X        VALUE SPACE.
          02 HEADER-AM-PM           PIC X(2).

       PROCEDURE DIVISION.
       MAINLINE.
           DISPLAY 'DATETIME STARTED'
           ACCEPT SYSTEM-WEEKDAY FROM DAY-OF-WEEK.
           ACCEPT SYSTEM-DATE FROM DATE.
           ACCEPT SYSTEM-TIME FROM TIME.

           EVALUATE SYSTEM-WEEKDAY
               WHEN  2  MOVE 'Mon' TO HEADER-WEEKDAY
               WHEN  3  MOVE 'Tue' TO HEADER-WEEKDAY
               WHEN  4  MOVE 'Wed' TO HEADER-WEEKDAY
               WHEN  5  MOVE 'Thu' TO HEADER-WEEKDAY
               WHEN  6  MOVE 'Fri' TO HEADER-WEEKDAY
               WHEN  7  MOVE 'Sat' TO HEADER-WEEKDAY
               WHEN  1  MOVE 'Sun' TO HEADER-WEEKDAY
           END-EVALUATE.

           EVALUATE SYSTEM-MONTH
               WHEN  1  MOVE 'Jan' TO HEADER-MONTH
               WHEN  2  MOVE 'Feb' TO HEADER-MONTH
               WHEN  3  MOVE 'Mar' TO HEADER-MONTH
               WHEN  4  MOVE 'Apr' TO HEADER-MONTH
               WHEN  5  MOVE 'May' TO HEADER-MONTH
               WHEN  6  MOVE 'Jun' TO HEADER-MONTH
               WHEN  7  MOVE 'Jul' TO HEADER-MONTH
               WHEN  8  MOVE 'Aug' TO HEADER-MONTH
               WHEN  9  MOVE 'Sep' TO HEADER-MONTH
               WHEN 10  MOVE 'Oct' TO HEADER-MONTH
               WHEN 11  MOVE 'Nov' TO HEADER-MONTH
               WHEN 12  MOVE 'Dec' TO HEADER-MONTH
           END-EVALUATE.

           MOVE SYSTEM-DAY TO HEADER-DAY.

           IF SYSTEM-HOUR IS LESS THAN 12
              MOVE 'AM' TO HEADER-AM-PM
           ELSE
              MOVE 'PM' TO HEADER-AM-PM
              IF SYSTEM-HOUR IS NOT LESS THAN 13
                 SUBTRACT 12 FROM SYSTEM-HOUR
              END-IF
           END-IF.
           MOVE SYSTEM-HOUR TO HEADER-HOUR.

           MOVE SYSTEM-MINUTE TO HEADER-MINUTE.

           DISPLAY HEADER-LINE.
           DISPLAY 'DATATIME ENDED OK'
           STOP RUN.
