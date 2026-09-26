       IDENTIFICATION DIVISION.
       PROGRAM-ID. TSRCHOP.
      ******************************************************************
      * Exercises SEARCH WHEN relational-operator forms in WHEN.MAC
      * (>, IS GREATER THAN OR EQUAL TO, IS LESS THAN, IS NOT EQUAL TO)
      * comparing against WORKING-STORAGE FIELDS (not literals), so the
      * test isolates operator/condition-code handling from the
      * separate GEN_COMP literal-operand defect.
      * Table values 10 20 30 40 50, index starts at 1, hits are known.
      ******************************************************************
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  T.
           05  E OCCURS 5 TIMES INDEXED BY IX.
               10  V  PIC 9(3).
       01  HIT        PIC 9(3) VALUE 0.
       01  LIM25      PIC 9(3) VALUE 25.
       01  LIM30      PIC 9(3) VALUE 30.
       01  LIM20      PIC 9(3) VALUE 20.
       01  LIM10      PIC 9(3) VALUE 10.
       01  PASS-COUNT PIC 9(2) VALUE 0.
       01  FAIL-COUNT PIC 9(2) VALUE 0.

       PROCEDURE DIVISION.
       MAIN.
           MOVE 10 TO V(1).
           MOVE 20 TO V(2).
           MOVE 30 TO V(3).
           MOVE 40 TO V(4).
           MOVE 50 TO V(5).
      *    > 25  => first hit 30
           MOVE 0 TO HIT.
           SET IX TO 1.
           SEARCH E AT END MOVE 0 TO HIT
               WHEN V(IX) > LIM25 MOVE V(IX) TO HIT
           END-SEARCH.
           PERFORM CHECK-30.
      *    >= 30 => first hit 30
           MOVE 0 TO HIT.
           SET IX TO 1.
           SEARCH E AT END MOVE 0 TO HIT
               WHEN V(IX) IS GREATER THAN OR EQUAL TO LIM30
                   MOVE V(IX) TO HIT
           END-SEARCH.
           PERFORM CHECK-30.
      *    IS LESS THAN 20 => first hit 10
           MOVE 0 TO HIT.
           SET IX TO 1.
           SEARCH E AT END MOVE 0 TO HIT
               WHEN V(IX) IS LESS THAN LIM20 MOVE V(IX) TO HIT
           END-SEARCH.
           IF HIT = 10
               ADD 1 TO PASS-COUNT
           ELSE
               DISPLAY 'FAIL LESS THAN, HIT=' HIT
               ADD 1 TO FAIL-COUNT.
      *    IS NOT EQUAL TO 10 => first hit 20
           MOVE 0 TO HIT.
           SET IX TO 1.
           SEARCH E AT END MOVE 0 TO HIT
               WHEN V(IX) IS NOT EQUAL TO LIM10 MOVE V(IX) TO HIT
           END-SEARCH.
           IF HIT = 20
               ADD 1 TO PASS-COUNT
           ELSE
               DISPLAY 'FAIL NOT EQUAL, HIT=' HIT
               ADD 1 TO FAIL-COUNT.
           DISPLAY 'TSRCHOP PASSED: ' PASS-COUNT ' FAILED: ' FAIL-COUNT.
           IF FAIL-COUNT = 0
               DISPLAY 'TSRCHOP - ALL TESTS PASSED'
           ELSE
               MOVE 8 TO RETURN-CODE.
           STOP RUN.
       CHECK-30.
           IF HIT = 30
               ADD 1 TO PASS-COUNT
           ELSE
               DISPLAY 'FAIL EXPECTED 30, HIT=' HIT
               ADD 1 TO FAIL-COUNT.
