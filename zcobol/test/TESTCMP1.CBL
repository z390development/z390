       IDENTIFICATION DIVISION.
       PROGRAM-ID.    TESTCMP1.
       AUTHOR.        DON HIGGINS.
       DATE-WRITTEN.  10/13/08.
      * TEST ADD, SUBTRACT, MULTIPLEY, DIVIDE.                              
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77 D1      COMP-2            VALUE 1.           
       77 E1      COMP-1            VALUE 1.           
       77 W1      COMP   PIC S9(9)  VALUE 1.           
       77 G1      COMP   PIC S9(18) VALUE 1.           
       77 H1      COMP   PIC S9(4)  VALUE 1.           
       77 P1      COMP-3 PIC S9(4)  VALUE 1.           
       77 Z1             PIC S9(4)  VALUE 1.           
       77 D2      COMP-2            VALUE 2.           
       77 E2      COMP-1            VALUE 2.           
       77 W2      COMP   PIC S9(9)  VALUE 2.           
       77 G2      COMP   PIC S9(18) VALUE 2.           
       77 H2      COMP   PIC S9(4)  VALUE 2.           
       77 P2      COMP-3 PIC S9(4)  VALUE 2.           
       77 Z2             PIC S9(4)  VALUE 2.           
       77 D3      COMP-2            VALUE 3.           
       77 E3      COMP-1            VALUE 3.           
       77 W3      COMP   PIC S9(9)  VALUE 3.           
       77 G3      COMP   PIC S9(18) VALUE 3.           
       77 H3      COMP   PIC S9(4)  VALUE 3.           
       77 P3      COMP-3 PIC S9(4)  VALUE 3.           
       77 Z3             PIC S9(4)  VALUE 3.           
       77 GOOD   COMP-3 PIC 9(4)   VALUE 0.
       PROCEDURE DIVISION.
       MAINLINE.
           DISPLAY 'TESTCMP1 ADD, SUB, MPY, DIV, COMPUTE STARTING'
           DISPLAY 'TESTCMP1 SUBTRACT 2 GOOD'
       1.  SUBTRACT 2 FROM D1 
            IF D1 = -1 PERFORM GOOD ELSE PERFORM BAD.
       2.  SUBTRACT 2 FROM E1 
            IF E1 = -1 PERFORM GOOD ELSE PERFORM BAD.
       3.  SUBTRACT 2 FROM W1 
            IF W1 = -1 PERFORM GOOD ELSE PERFORM BAD.
       4.  SUBTRACT 2 FROM G1 
            IF G1 = -1 PERFORM GOOD ELSE PERFORM BAD.
       5.  SUBTRACT 2 FROM H1
            IF H1 = -1 PERFORM GOOD ELSE PERFORM BAD.
       6.  SUBTRACT 2 FROM P1
            IF P1 = -1 PERFORM GOOD ELSE PERFORM BAD.
       7.  SUBTRACT 2 FROM Z1 
            IF Z1 = -1 PERFORM GOOD ELSE PERFORM BAD.
       8.  DISPLAY 'TESTCMP1 SUBTRACT D2 GOOD'
           MOVE 1 TO D1
           MOVE 1 TO E1
           MOVE 1 TO W1
           MOVE 1 TO G1
           MOVE 1 TO H1
           MOVE 1 TO P1
           MOVE 1 TO Z1
           SUBTRACT D2 FROM D1 
            IF D1 = -1 PERFORM GOOD ELSE PERFORM BAD.
       9.  SUBTRACT D2 FROM E1 
            IF E1 = -1 PERFORM GOOD ELSE PERFORM BAD.
       10. SUBTRACT D2 FROM W1 
            IF W1 = -1 PERFORM GOOD ELSE PERFORM BAD.
       11. SUBTRACT D2 FROM G1 
            IF G1 = -1 PERFORM GOOD ELSE PERFORM BAD.
       12. SUBTRACT D2 FROM H1 
            IF H1 = -1 PERFORM GOOD ELSE PERFORM BAD.
       13. SUBTRACT D2 FROM P1 
            IF P1 = -1 PERFORM GOOD ELSE PERFORM BAD.
       14. SUBTRACT D2 FROM Z1 
            IF Z1 = -1 PERFORM GOOD ELSE PERFORM BAD.
       15. DISPLAY 'TESTCMP1 SUBTRACT E2 GOOD'
           MOVE 1 TO D1
           MOVE 1 TO E1
           MOVE 1 TO W1
           MOVE 1 TO G1
           MOVE 1 TO H1
           MOVE 1 TO P1
           MOVE 1 TO Z1
           SUBTRACT E2 FROM D1 
            IF D1 = -1 PERFORM GOOD ELSE PERFORM BAD.
       16. SUBTRACT E2 FROM E1 
            IF E1 = -1 PERFORM GOOD ELSE PERFORM BAD.
       17. SUBTRACT E2 FROM W1 
            IF W1 = -1 PERFORM GOOD ELSE PERFORM BAD.
       18. SUBTRACT E2 FROM G1 
            IF G1 = -1 PERFORM GOOD ELSE PERFORM BAD.
       19. SUBTRACT E2 FROM H1 
            IF H1 = -1 PERFORM GOOD ELSE PERFORM BAD.
       20. SUBTRACT E2 FROM P1 
            IF P1 = -1 PERFORM GOOD ELSE PERFORM BAD.
       21. SUBTRACT E2 FROM Z1 
            IF Z1 = -1 PERFORM GOOD ELSE PERFORM BAD.
       22. DISPLAY 'TESTCMP1 SUBTRACT W2 GOOD'
           MOVE 1 TO D1
           MOVE 1 TO E1
           MOVE 1 TO W1
           MOVE 1 TO G1
           MOVE 1 TO H1
           MOVE 1 TO P1
           MOVE 1 TO Z1
           SUBTRACT W2 FROM D1 
            IF D1 = -1 PERFORM GOOD ELSE PERFORM BAD.
       23. SUBTRACT W2 FROM E1 
            IF E1 = -1 PERFORM GOOD ELSE PERFORM BAD.
       24. SUBTRACT W2 FROM W1 
            IF W1 = -1 PERFORM GOOD ELSE PERFORM BAD.
       25. SUBTRACT W2 FROM G1 
            IF G1 = -1 PERFORM GOOD ELSE PERFORM BAD.
       26. SUBTRACT W2 FROM H1 
            IF H1 = -1 PERFORM GOOD ELSE PERFORM BAD.
       27. SUBTRACT W2 FROM P1 
            IF P1 = -1 PERFORM GOOD ELSE PERFORM BAD.
       28. SUBTRACT W2 FROM Z1 
            IF Z1 = -1 PERFORM GOOD ELSE PERFORM BAD.
       29. DISPLAY 'TESTCMP1 SUBTRACT G2 GOOD'
           MOVE 1 TO D1
           MOVE 1 TO E1
           MOVE 1 TO W1
           MOVE 1 TO G1
           MOVE 1 TO H1
           MOVE 1 TO P1
           MOVE 1 TO Z1
           SUBTRACT G2 FROM D1 
            IF D1 = -1 PERFORM GOOD ELSE PERFORM BAD.
       30. SUBTRACT G2 FROM E1 
            IF E1 = -1 PERFORM GOOD ELSE PERFORM BAD.
       31. SUBTRACT G2 FROM W1 
            IF W1 = -1 PERFORM GOOD ELSE PERFORM BAD.
       32. SUBTRACT G2 FROM G1 
            IF G1 = -1 PERFORM GOOD ELSE PERFORM BAD.
       33. SUBTRACT G2 FROM H1 
            IF H1 = -1 PERFORM GOOD ELSE PERFORM BAD.
       34. SUBTRACT G2 FROM P1 
            IF P1 = -1 PERFORM GOOD ELSE PERFORM BAD.
       35. SUBTRACT G2 FROM Z1 
            IF Z1 = -1 PERFORM GOOD ELSE PERFORM BAD.
       36. DISPLAY 'TESTCMP1 SUBTRACT H2 GOOD'
           MOVE 1 TO D1
           MOVE 1 TO E1
           MOVE 1 TO W1
           MOVE 1 TO G1
           MOVE 1 TO H1
           MOVE 1 TO P1
           MOVE 1 TO Z1
           SUBTRACT H2 FROM D1 
            IF D1 = -1 PERFORM GOOD ELSE PERFORM BAD.
       37. SUBTRACT H2 FROM E1 
            IF E1 = -1 PERFORM GOOD ELSE PERFORM BAD.
       38. SUBTRACT H2 FROM W1 
            IF W1 = -1 PERFORM GOOD ELSE PERFORM BAD.
       39. SUBTRACT H2 FROM G1 
            IF G1 = -1 PERFORM GOOD ELSE PERFORM BAD.
       40. SUBTRACT H2 FROM H1 
            IF H1 = -1 PERFORM GOOD ELSE PERFORM BAD.
       41. SUBTRACT H2 FROM P1 
            IF P1 = -1 PERFORM GOOD ELSE PERFORM BAD.
       42. SUBTRACT H2 FROM Z1 
            IF Z1 = -1 PERFORM GOOD ELSE PERFORM BAD.
       43. DISPLAY 'TESTCMP1 SUBTRACT P2 GOOD'
           MOVE 1 TO D1
           MOVE 1 TO E1
           MOVE 1 TO W1
           MOVE 1 TO G1
           MOVE 1 TO H1
           MOVE 1 TO P1
           MOVE 1 TO Z1
           SUBTRACT P2 FROM D1 
            IF D1 = -1 PERFORM GOOD ELSE PERFORM BAD.
       44. SUBTRACT P2 FROM E1 
            IF E1 = -1 PERFORM GOOD ELSE PERFORM BAD.
       45. SUBTRACT P2 FROM W1 
            IF W1 = -1 PERFORM GOOD ELSE PERFORM BAD.
       46. SUBTRACT P2 FROM G1 
            IF G1 = -1 PERFORM GOOD ELSE PERFORM BAD.
       47. SUBTRACT P2 FROM H1 
            IF H1 = -1 PERFORM GOOD ELSE PERFORM BAD.
       48. SUBTRACT P2 FROM P1 
            IF P1 = -1 PERFORM GOOD ELSE PERFORM BAD.
       49. SUBTRACT P2 FROM Z1 
            IF Z1 = -1 PERFORM GOOD ELSE PERFORM BAD.
       50. DISPLAY 'TESTCMP1 SUBTRACT Z2 GOOD'
           MOVE 1 TO D1
           MOVE 1 TO E1
           MOVE 1 TO W1
           MOVE 1 TO G1
           MOVE 1 TO H1
           MOVE 1 TO P1
           MOVE 1 TO Z1
           SUBTRACT Z2 FROM D1 
            IF D1 = -1 PERFORM GOOD ELSE PERFORM BAD.
       51. SUBTRACT Z2 FROM E1 
            IF E1 = -1 PERFORM GOOD ELSE PERFORM BAD.
       52. SUBTRACT Z2 FROM W1 
            IF W1 = -1 PERFORM GOOD ELSE PERFORM BAD.
       53. SUBTRACT Z2 FROM G1 
            IF G1 = -1 PERFORM GOOD ELSE PERFORM BAD.
       54. SUBTRACT Z2 FROM H1 
            IF H1 = -1 PERFORM GOOD ELSE PERFORM BAD.
       55. SUBTRACT Z2 FROM P1 
            IF P1 = -1 PERFORM GOOD ELSE PERFORM BAD.
       56. SUBTRACT Z2 FROM Z1 
            IF Z1 = -1 PERFORM GOOD ELSE PERFORM BAD.
       57. DISPLAY 'TESTCMP1 ADD 2 GOOD'
           MOVE -1 TO D1
           MOVE -1 TO E1
           MOVE -1 TO W1
           MOVE -1 TO G1
           MOVE -1 TO H1
           MOVE -1 TO P1
           MOVE -1 TO Z1
           ADD 2 TO D1 
            IF D1 = 1 PERFORM GOOD ELSE PERFORM BAD.
       58. ADD 2 TO E1 
            IF E1 = 1 PERFORM GOOD ELSE PERFORM BAD.
       59. ADD 2 TO W1 
            IF W1 = 1 PERFORM GOOD ELSE PERFORM BAD.
       60. ADD 2 TO G1 
            IF G1 = 1 PERFORM GOOD ELSE PERFORM BAD.
       61. ADD 2 TO H1 
            IF H1 = 1 PERFORM GOOD ELSE PERFORM BAD.
       62. ADD 2 TO P1 
            IF P1 = 1 PERFORM GOOD ELSE PERFORM BAD.
       63. ADD 2 TO Z1 
            IF Z1 = 1 PERFORM GOOD ELSE PERFORM BAD.
       64. DISPLAY 'TESTCMP1 ADD D2 GOOD'
           MOVE -1 TO D1
           MOVE -1 TO E1
           MOVE -1 TO W1
           MOVE -1 TO G1
           MOVE -1 TO H1
           MOVE -1 TO P1
           MOVE -1 TO Z1
           ADD D2 TO D1 
            IF D1 = 1 PERFORM GOOD ELSE PERFORM BAD.
           ADD D2 TO E1 
            IF E1 = 1 PERFORM GOOD ELSE PERFORM BAD.
           ADD D2 TO W1 
            IF W1 = 1 PERFORM GOOD ELSE PERFORM BAD.
           ADD D2 TO G1 
            IF G1 = 1 PERFORM GOOD ELSE PERFORM BAD.
           ADD D2 TO H1 
            IF H1 = 1 PERFORM GOOD ELSE PERFORM BAD.
           ADD D2 TO P1 
            IF P1 = 1 PERFORM GOOD ELSE PERFORM BAD.
           ADD D2 TO Z1 
            IF Z1 = 1 PERFORM GOOD ELSE PERFORM BAD.
           DISPLAY 'TESTCMP1 ADD E2 GOOD'
           MOVE -1 TO D1
           MOVE -1 TO E1
           MOVE -1 TO W1
           MOVE -1 TO G1
           MOVE -1 TO H1
           MOVE -1 TO P1
           MOVE -1 TO Z1
           ADD E2 TO D1 
            IF D1 = 1 PERFORM GOOD ELSE PERFORM BAD.
           ADD E2 TO E1 
            IF E1 = 1 PERFORM GOOD ELSE PERFORM BAD.
           ADD E2 TO W1 
            IF W1 = 1 PERFORM GOOD ELSE PERFORM BAD.
           ADD E2 TO G1 
            IF G1 = 1 PERFORM GOOD ELSE PERFORM BAD.
           ADD E2 TO H1 
            IF H1 = 1 PERFORM GOOD ELSE PERFORM BAD.
           ADD E2 TO P1 
            IF P1 = 1 PERFORM GOOD ELSE PERFORM BAD.
           ADD E2 TO Z1 
            IF Z1 = 1 PERFORM GOOD ELSE PERFORM BAD.
           DISPLAY 'TESTCMP1 ADD W2 GOOD'
           MOVE -1 TO D1
           MOVE -1 TO E1
           MOVE -1 TO W1
           MOVE -1 TO G1
           MOVE -1 TO H1
           MOVE -1 TO P1
           MOVE -1 TO Z1
           ADD W2 TO D1 
            IF D1 = 1 PERFORM GOOD ELSE PERFORM BAD.
           ADD W2 TO E1 
            IF E1 = 1 PERFORM GOOD ELSE PERFORM BAD.
           ADD W2 TO W1 
            IF W1 = 1 PERFORM GOOD ELSE PERFORM BAD.
           ADD W2 TO G1 
            IF G1 = 1 PERFORM GOOD ELSE PERFORM BAD.
           ADD W2 TO H1 
            IF H1 = 1 PERFORM GOOD ELSE PERFORM BAD.
           ADD W2 TO P1 
            IF P1 = 1 PERFORM GOOD ELSE PERFORM BAD.
           ADD W2 TO Z1 
            IF Z1 = 1 PERFORM GOOD ELSE PERFORM BAD.
           DISPLAY 'TESTCMP1 ADD G2 GOOD'
           MOVE -1 TO D1
           MOVE -1 TO E1
           MOVE -1 TO W1
           MOVE -1 TO G1
           MOVE -1 TO H1
           MOVE -1 TO P1
           MOVE -1 TO Z1
           ADD G2 TO D1 
            IF D1 = 1 PERFORM GOOD ELSE PERFORM BAD.
           ADD G2 TO E1 
            IF E1 = 1 PERFORM GOOD ELSE PERFORM BAD.
           ADD G2 TO W1 
            IF W1 = 1 PERFORM GOOD ELSE PERFORM BAD.
           ADD G2 TO G1 
            IF G1 = 1 PERFORM GOOD ELSE PERFORM BAD.
           ADD G2 TO H1 
            IF H1 = 1 PERFORM GOOD ELSE PERFORM BAD.
           ADD G2 TO P1 
            IF P1 = 1 PERFORM GOOD ELSE PERFORM BAD.
           ADD G2 TO Z1 
            IF Z1 = 1 PERFORM GOOD ELSE PERFORM BAD.
           DISPLAY 'TESTCMP1 ADD H2 GOOD'
           MOVE -1 TO D1
           MOVE -1 TO E1
           MOVE -1 TO W1
           MOVE -1 TO G1
           MOVE -1 TO H1
           MOVE -1 TO P1
           MOVE -1 TO Z1
           ADD H2 TO D1 
            IF D1 = 1 PERFORM GOOD ELSE PERFORM BAD.
           ADD H2 TO E1 
            IF E1 = 1 PERFORM GOOD ELSE PERFORM BAD.
           ADD H2 TO W1 
            IF W1 = 1 PERFORM GOOD ELSE PERFORM BAD.
           ADD H2 TO G1 
            IF G1 = 1 PERFORM GOOD ELSE PERFORM BAD.
           ADD H2 TO H1 
            IF H1 = 1 PERFORM GOOD ELSE PERFORM BAD.
           ADD H2 TO P1 
            IF P1 = 1 PERFORM GOOD ELSE PERFORM BAD.
           ADD H2 TO Z1 
            IF Z1 = 1 PERFORM GOOD ELSE PERFORM BAD.
           DISPLAY 'TESTCMP1 ADD P2 GOOD'
           MOVE -1 TO D1
           MOVE -1 TO E1
           MOVE -1 TO W1
           MOVE -1 TO G1
           MOVE -1 TO H1
           MOVE -1 TO P1
           MOVE -1 TO Z1
           ADD P2 TO D1 
            IF D1 = 1 PERFORM GOOD ELSE PERFORM BAD.
           ADD P2 TO E1 
            IF E1 = 1 PERFORM GOOD ELSE PERFORM BAD.
           ADD P2 TO W1 
            IF W1 = 1 PERFORM GOOD ELSE PERFORM BAD.
           ADD P2 TO G1 
            IF G1 = 1 PERFORM GOOD ELSE PERFORM BAD.
           ADD P2 TO H1 
            IF H1 = 1 PERFORM GOOD ELSE PERFORM BAD.
           ADD P2 TO P1 
            IF P1 = 1 PERFORM GOOD ELSE PERFORM BAD.
           ADD P2 TO Z1 
            IF Z1 = 1 PERFORM GOOD ELSE PERFORM BAD.
           DISPLAY 'TESTCMP1 ADD Z2 GOOD'
           MOVE -1 TO D1
           MOVE -1 TO E1
           MOVE -1 TO W1
           MOVE -1 TO G1
           MOVE -1 TO H1
           MOVE -1 TO P1
           MOVE -1 TO Z1
           ADD Z2 TO D1 
            IF D1 = 1 PERFORM GOOD ELSE PERFORM BAD.
           ADD Z2 TO E1 
            IF E1 = 1 PERFORM GOOD ELSE PERFORM BAD.
           ADD Z2 TO W1 
            IF W1 = 1 PERFORM GOOD ELSE PERFORM BAD.
           ADD Z2 TO G1 
            IF G1 = 1 PERFORM GOOD ELSE PERFORM BAD.
           ADD Z2 TO H1 
            IF H1 = 1 PERFORM GOOD ELSE PERFORM BAD.
           ADD Z2 TO P1 
            IF P1 = 1 PERFORM GOOD ELSE PERFORM BAD.
           ADD Z2 TO Z1 
            IF Z1 = 1 PERFORM GOOD ELSE PERFORM BAD.
           DISPLAY 'TESTCMP1 MULTIPLY 2 GOOD'
           MOVE 3 TO D2
           MOVE 3 TO E2
           MOVE 3 TO W2
           MOVE 3 TO G2
           MOVE 3 TO H2
           MOVE 3 TO P2
           MOVE 3 TO Z2
           MOVE -2 TO D1
           MOVE -2 TO E1
           MOVE -2 TO W1
           MOVE -2 TO G1
           MOVE -2 TO H1
           MOVE -2 TO P1
           MOVE -2 TO Z1
           MULTIPLY  3 BY D1 
            IF D1 = -6 PERFORM GOOD ELSE PERFORM BAD.
           MULTIPLY  3 BY E1 
            IF E1 = -6 PERFORM GOOD ELSE PERFORM BAD.
           MULTIPLY  3 BY W1 
            IF W1 = -6 PERFORM GOOD ELSE PERFORM BAD.
           MULTIPLY  3 BY G1 
            IF G1 = -6 PERFORM GOOD ELSE PERFORM BAD.
           MULTIPLY  3 BY H1 
            IF H1 = -6 PERFORM GOOD ELSE PERFORM BAD.
           MULTIPLY  3 BY P1 
            IF P1 = -6 PERFORM GOOD ELSE PERFORM BAD.
           MULTIPLY  3 BY Z1 
            IF Z1 = -6 PERFORM GOOD ELSE PERFORM BAD.
           DISPLAY 'TESTCMP1 MULTIPLY D2 GOOD'
           MOVE -2 TO D1
           MOVE -2 TO E1
           MOVE -2 TO W1
           MOVE -2 TO G1
           MOVE -2 TO H1
           MOVE -2 TO P1
           MOVE -2 TO Z1
           MULTIPLY D2 BY D1 
            IF D1 = -6 PERFORM GOOD ELSE PERFORM BAD.
           MULTIPLY D2 BY E1 
            IF E1 = -6 PERFORM GOOD ELSE PERFORM BAD.
           MULTIPLY D2 BY W1 
            IF W1 = -6 PERFORM GOOD ELSE PERFORM BAD.
           MULTIPLY D2 BY G1 
            IF G1 = -6 PERFORM GOOD ELSE PERFORM BAD.
           MULTIPLY D2 BY H1 
            IF H1 = -6 PERFORM GOOD ELSE PERFORM BAD.
           MULTIPLY D2 BY P1 
            IF P1 = -6 PERFORM GOOD ELSE PERFORM BAD.
           MULTIPLY D2 BY Z1 
            IF Z1 = -6 PERFORM GOOD ELSE PERFORM BAD.
           DISPLAY 'TESTCMP1 MULTIPLY E2 GOOD'
           MOVE -2 TO D1
           MOVE -2 TO E1
           MOVE -2 TO W1
           MOVE -2 TO G1
           MOVE -2 TO H1
           MOVE -2 TO P1
           MOVE -2 TO Z1
           MULTIPLY E2 BY D1 
            IF D1 = -6 PERFORM GOOD ELSE PERFORM BAD.
           MULTIPLY E2 BY E1 
            IF E1 = -6 PERFORM GOOD ELSE PERFORM BAD.
           MULTIPLY E2 BY W1 
            IF W1 = -6 PERFORM GOOD ELSE PERFORM BAD.
           MULTIPLY E2 BY G1 
            IF G1 = -6 PERFORM GOOD ELSE PERFORM BAD.
           MULTIPLY E2 BY H1 
            IF H1 = -6 PERFORM GOOD ELSE PERFORM BAD.
           MULTIPLY E2 BY P1 
            IF P1 = -6 PERFORM GOOD ELSE PERFORM BAD.
           MULTIPLY E2 BY Z1 
            IF Z1 = -6 PERFORM GOOD ELSE PERFORM BAD.
           DISPLAY 'TESTCMP1 MULTIPLY W2 GOOD'
           MOVE -2 TO D1
           MOVE -2 TO E1
           MOVE -2 TO W1
           MOVE -2 TO G1
           MOVE -2 TO H1
           MOVE -2 TO P1
           MOVE -2 TO Z1
           MULTIPLY W2 BY D1 
            IF D1 = -6 PERFORM GOOD ELSE PERFORM BAD.
           MULTIPLY W2 BY E1 
            IF E1 = -6 PERFORM GOOD ELSE PERFORM BAD.
           MULTIPLY W2 BY W1 
            IF W1 = -6 PERFORM GOOD ELSE PERFORM BAD.
           MULTIPLY W2 BY G1 
            IF G1 = -6 PERFORM GOOD ELSE PERFORM BAD.
           MULTIPLY W2 BY H1 
            IF H1 = -6 PERFORM GOOD ELSE PERFORM BAD.
           MULTIPLY W2 BY P1 
            IF P1 = -6 PERFORM GOOD ELSE PERFORM BAD.
           MULTIPLY W2 BY Z1 
            IF Z1 = -6 PERFORM GOOD ELSE PERFORM BAD.
           DISPLAY 'TESTCMP1 MULTIPLY G2 GOOD'
           MOVE -2 TO D1
           MOVE -2 TO E1
           MOVE -2 TO W1
           MOVE -2 TO G1
           MOVE -2 TO H1
           MOVE -2 TO P1
           MOVE -2 TO Z1
           MULTIPLY G2 BY D1 
            IF D1 = -6 PERFORM GOOD ELSE PERFORM BAD.
           MULTIPLY G2 BY E1 
            IF E1 = -6 PERFORM GOOD ELSE PERFORM BAD.
           MULTIPLY G2 BY W1 
            IF W1 = -6 PERFORM GOOD ELSE PERFORM BAD.
           MULTIPLY G2 BY G1 
            IF G1 = -6 PERFORM GOOD ELSE PERFORM BAD.
           MULTIPLY G2 BY H1 
            IF H1 = -6 PERFORM GOOD ELSE PERFORM BAD.
           MULTIPLY G2 BY P1 
            IF P1 = -6 PERFORM GOOD ELSE PERFORM BAD.
           MULTIPLY G2 BY Z1 
            IF Z1 = -6 PERFORM GOOD ELSE PERFORM BAD.
           DISPLAY 'TESTCMP1 MULTIPLY H2 GOOD'
           MOVE -2 TO D1
           MOVE -2 TO E1
           MOVE -2 TO W1
           MOVE -2 TO G1
           MOVE -2 TO H1
           MOVE -2 TO P1
           MOVE -2 TO Z1
           MULTIPLY H2 BY D1 
            IF D1 = -6 PERFORM GOOD ELSE PERFORM BAD.
           MULTIPLY H2 BY E1 
            IF E1 = -6 PERFORM GOOD ELSE PERFORM BAD.
           MULTIPLY H2 BY W1 
            IF W1 = -6 PERFORM GOOD ELSE PERFORM BAD.
           MULTIPLY H2 BY G1 
            IF G1 = -6 PERFORM GOOD ELSE PERFORM BAD.
           MULTIPLY H2 BY H1 
            IF H1 = -6 PERFORM GOOD ELSE PERFORM BAD.
           MULTIPLY H2 BY P1 
            IF P1 = -6 PERFORM GOOD ELSE PERFORM BAD.
           MULTIPLY H2 BY Z1 
            IF Z1 = -6 PERFORM GOOD ELSE PERFORM BAD.
           DISPLAY 'TESTCMP1 MULTIPLY P2 GOOD'
           MOVE -2 TO D1
           MOVE -2 TO E1
           MOVE -2 TO W1
           MOVE -2 TO G1
           MOVE -2 TO H1
           MOVE -2 TO P1
           MOVE -2 TO Z1
           MULTIPLY P2 BY D1 
            IF D1 = -6 PERFORM GOOD ELSE PERFORM BAD.
           MULTIPLY P2 BY E1 
            IF E1 = -6 PERFORM GOOD ELSE PERFORM BAD.
           MULTIPLY P2 BY W1 
            IF W1 = -6 PERFORM GOOD ELSE PERFORM BAD.
           MULTIPLY P2 BY G1 
            IF G1 = -6 PERFORM GOOD ELSE PERFORM BAD.
           MULTIPLY P2 BY H1 
            IF H1 = -6 PERFORM GOOD ELSE PERFORM BAD.
           MULTIPLY P2 BY P1 
            IF P1 = -6 PERFORM GOOD ELSE PERFORM BAD.
           MULTIPLY P2 BY Z1 
            IF Z1 = -6 PERFORM GOOD ELSE PERFORM BAD.
           DISPLAY 'TESTCMP1 MULTIPLY Z2 GOOD'
           MOVE -2 TO D1
           MOVE -2 TO E1
           MOVE -2 TO W1
           MOVE -2 TO G1
           MOVE -2 TO H1
           MOVE -2 TO P1
           MOVE -2 TO Z1
           MULTIPLY Z2 BY D1 
            IF D1 = -6 PERFORM GOOD ELSE PERFORM BAD.
           MULTIPLY Z2 BY E1 
            IF E1 = -6 PERFORM GOOD ELSE PERFORM BAD.
           MULTIPLY Z2 BY W1 
            IF W1 = -6 PERFORM GOOD ELSE PERFORM BAD.
           MULTIPLY Z2 BY G1 
            IF G1 = -6 PERFORM GOOD ELSE PERFORM BAD.
           MULTIPLY Z2 BY H1 
            IF H1 = -6 PERFORM GOOD ELSE PERFORM BAD.
           MULTIPLY Z2 BY P1 
            IF P1 = -6 PERFORM GOOD ELSE PERFORM BAD.
           MULTIPLY Z2 BY Z1 
            IF Z1 = -6 PERFORM GOOD ELSE PERFORM BAD.
           DISPLAY 'TESTCMP1 DIVIDE COMP-1/2 GOOD'
           DIVIDE 2 INTO 7 GIVING D2 
            IF D2 = 3.5 PERFORM GOOD ELSE PERFORM BAD.
           DIVIDE 2 INTO 7 GIVING E2 
            IF E2 = 3.5 PERFORM GOOD ELSE PERFORM BAD.
           DIVIDE 2 INTO 7 GIVING W2 REMAINDER W3
            IF W2 = 3 AND W3 = 1 PERFORM GOOD ELSE PERFORM BAD.
           DIVIDE 2 INTO 7 GIVING G2 REMAINDER G3
            IF G2 = 3 AND G3 = 1 PERFORM GOOD ELSE PERFORM BAD.
           DIVIDE 2 INTO 7 GIVING H2 REMAINDER H3
            IF H2 = 3 AND H3 = 1 PERFORM GOOD ELSE PERFORM BAD.
           DIVIDE 2 INTO 7 GIVING P2 REMAINDER P3
            IF P2 = 3 AND P3 = 1 PERFORM GOOD ELSE PERFORM BAD.
           DIVIDE 2 INTO 7 GIVING Z2 REMAINDER Z3
            IF Z2 = 3 AND Z3 = 1 PERFORM GOOD ELSE PERFORM BAD.
           DIVIDE 7 BY 2 GIVING D2 
            IF D2 = 3.5 PERFORM GOOD ELSE PERFORM BAD.
           DIVIDE 7 BY 2 GIVING E2 
            IF E2 = 3.5 PERFORM GOOD ELSE PERFORM BAD.
           DIVIDE 7 BY 2 GIVING W2 REMAINDER W3
            IF W2 = 3 AND W3 = 1 PERFORM GOOD ELSE PERFORM BAD.
           MOVE 7 TO D2
           DIVIDE 2 INTO D2 
            IF D2 = 3.5 PERFORM GOOD ELSE PERFORM BAD.
           MOVE 7 TO E2
           DIVIDE 2 INTO E2 
            IF E2 = 3.5 PERFORM GOOD ELSE PERFORM BAD.
           MOVE 7 TO D2
           DIVIDE D2 BY 2 
            IF D2 = 3.5 PERFORM GOOD ELSE PERFORM BAD.
           MOVE 7 TO E2
           DIVIDE E2 BY 2 
            IF E2 = 3.5 PERFORM GOOD ELSE PERFORM BAD.
      * TEST ADD WITH MULT SOURCE AND TARGETS
           MOVE 0 TO H1 H2
           ADD 1 2 3 TO H1 H2
           IF H1 = 6 AND H2 = 6 PERFORM GOOD ELSE PERFORM BAD.
           ADD 1 2 3 GIVING H1 H2
           IF H1 = 6 AND H2 = 6 PERFORM GOOD ELSE PERFORM BAD.
           ADD 1 2 TO 3 GIVING H1 H2
           IF H1 = 6 AND H2 = 6 PERFORM GOOD ELSE PERFORM BAD.
      * TEST SUBTRACT WITH MULT SOURCE AND TARGETS
           MOVE 7 TO H1 H2
           SUBTRACT 1 2 3 FROM H1 H2
           IF H1 = 1 AND H2 = 1 PERFORM GOOD ELSE PERFORM BAD.
           SUBTRACT 1 2 3 FROM 8 GIVING H1 H2
           IF H1 = 2 AND H2 = 2 PERFORM GOOD ELSE PERFORM BAD.
      * TEST DIVIDE WITH MULT TARGETS 
           MOVE 3 TO H1 H2
           DIVIDE 2 INTO H1 H2
           IF H1 = 1 AND H2 = 1 
              PERFORM GOOD ELSE PERFORM BAD.
           DIVIDE 2 INTO 7 GIVING H1 H2
           IF H1 = 3 AND H2 = 3 
              PERFORM GOOD ELSE PERFORM BAD.
           DIVIDE 2 INTO 7 GIVING H1 REMAINDER H2
           IF H1 = 3 AND H2 = 1 
              PERFORM GOOD ELSE PERFORM BAD.
      * TEST MULTIPLY WITH MULT MULTIPLIERS AND MULT GIVING
           MOVE 3 TO H1
           MULTIPLY 2 BY H1   
           IF H1 = 6 
              PERFORM GOOD ELSE PERFORM BAD.
           MOVE 3 TO H1
           MULTIPLY 2 BY H1 H1  
           IF H1 = 12 
              PERFORM GOOD ELSE PERFORM BAD.
           MULTIPLY 3 BY 2 GIVING H1 H2  
           IF H1 = 6 AND H2 = 6 
              PERFORM GOOD ELSE PERFORM BAD.
           DISPLAY 'TESTCMP1 GOOD TESTS =' GOOD
           STOP RUN.
       GOOD.
           ADD 1 TO GOOD.
       BAD.
           MOVE 16 TO RETURN-CODE
           STOP RUN.
