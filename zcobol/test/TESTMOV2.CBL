       IDENTIFICATION DIVISION.
       PROGRAM-ID.    TESTMOV1.
       AUTHOR.        DON HIGGINS.
      * TEST MOVE ALIGNMENT  FOR NON FP TYPES
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
        01  DF30   PIC ' ZZ9.'.
        01  DF31   PIC ' ZZ9.9'.
        01  DF32   PIC ' ZZ9.99'.
        77  ZF30   PIC S9(3)    VALUE 123.
        77  ZF31   PIC S9(3)V9  VALUE 123.4.
        77  ZF32   PIC S9(3)V99 VALUE 123.45.
        77  PF30   COMP-3 PIC S9(3)    VALUE 123.
        77  PF31   COMP-3 PIC S9(3)V9  VALUE 123.4.
        77  PF32   COMP-3 PIC S9(3)V99 VALUE 123.45.        
        77  HF30   COMP   PIC S9(3)    VALUE 123.
        77  HF31   COMP   PIC S9(3)V9  VALUE 123.4.
        77  HF32   COMP   PIC S9(2)V99 VALUE 23.45.
        77  FF30   COMP   PIC S9(6)    VALUE 123.
        77  FF31   COMP   PIC S9(6)V9  VALUE 123.4.
        77  FF32   COMP   PIC S9(6)V99 VALUE 123.45.
        77  GF30   COMP   PIC S9(16)    VALUE 123.
        77  GF31   COMP   PIC S9(16)V9  VALUE 123.4.
        77  GF32   COMP   PIC S9(16)V99 VALUE 23.45.
        77  QF30   COMP   PIC S9(29)    VALUE 123.
        77  QF31   COMP   PIC S9(29)V9  VALUE 123.4.
        77  QF32   COMP   PIC S9(29)V99 VALUE 123.45.
        77  GOOD   COMP PIC S9(9) VALUE 0.
       PROCEDURE DIVISION.
       MAINLINE.
    Z1     MOVE ZF30 TO DF30                                           
           IF DF30 NOT = ' 123'     PERFORM BAD ELSE PERFORM GOOD.
    Z2     MOVE ZF30 TO DF31                                           
           IF DF31 NOT = ' 123.0'   PERFORM BAD ELSE PERFORM GOOD.      
    Z3     MOVE ZF30 TO DF32                                           
           IF DF32 NOT = ' 123.00'  PERFORM BAD ELSE PERFORM GOOD.
    Z4     MOVE ZF31 TO DF30                                           
           IF DF30 NOT = ' 123'     PERFORM BAD ELSE PERFORM GOOD.
    Z5     MOVE ZF31 TO DF31                                           
           IF DF31 NOT = ' 123.4'   PERFORM BAD ELSE PERFORM GOOD.      
    Z6     MOVE ZF31 TO DF32                                           
           IF DF32 NOT = ' 123.40'  PERFORM BAD ELSE PERFORM GOOD.
    Z7     MOVE ZF32 TO DF30                                           
           IF DF30 NOT = ' 123'     PERFORM BAD ELSE PERFORM GOOD.
    Z8     MOVE ZF32 TO DF31                                           
           IF DF31 NOT = ' 123.4'   PERFORM BAD ELSE PERFORM GOOD.      
    Z9     MOVE ZF32 TO DF32                                           
           IF DF32 NOT = ' 123.45'  PERFORM BAD ELSE PERFORM GOOD.     
    P1     MOVE PF30 TO DF30                                           
            IF DF30 NOT = ' 123'     PERFORM BAD ELSE PERFORM GOOD.
    P2     MOVE PF30 TO DF31                                           
            IF DF31 NOT = ' 123.0'   PERFORM BAD ELSE PERFORM GOOD.      
    P3     MOVE PF30 TO DF32                                           
            IF DF32 NOT = ' 123.00'  PERFORM BAD ELSE PERFORM GOOD.
    P4     MOVE PF31 TO DF30                                           
            IF DF30 NOT = ' 123'     PERFORM BAD ELSE PERFORM GOOD.
    P5     MOVE PF31 TO DF31                                           
            IF DF31 NOT = ' 123.4'   PERFORM BAD ELSE PERFORM GOOD.      
    P6     MOVE PF31 TO DF32                                           
            IF DF32 NOT = ' 123.40'  PERFORM BAD ELSE PERFORM GOOD.
    P7     MOVE PF32 TO DF30                                           
            IF DF30 NOT = ' 123'     PERFORM BAD ELSE PERFORM GOOD.
    P8     MOVE PF32 TO DF31                                           
            IF DF31 NOT = ' 123.4'   PERFORM BAD ELSE PERFORM GOOD.      
    P9     MOVE PF32 TO DF32                                           
            IF DF32 NOT = ' 123.45'  PERFORM BAD ELSE PERFORM GOOD.     
    H1     MOVE HF30 TO DF30                                           
           IF DF30 NOT = ' 123'     PERFORM BAD ELSE PERFORM GOOD.
    H2     MOVE HF30 TO DF31                                           
           IF DF31 NOT = ' 123.0'   PERFORM BAD ELSE PERFORM GOOD.      
    H3     MOVE HF30 TO DF32                                           
           IF DF32 NOT = ' 123.00'  PERFORM BAD ELSE PERFORM GOOD.
    H4     MOVE HF31 TO DF30                                           
           IF DF30 NOT = ' 123'     PERFORM BAD ELSE PERFORM GOOD.
    H5     MOVE HF31 TO DF31                                           
           IF DF31 NOT = ' 123.4'   PERFORM BAD ELSE PERFORM GOOD.      
    H6     MOVE HF31 TO DF32                                           
           IF DF32 NOT = ' 123.40'  PERFORM BAD ELSE PERFORM GOOD.
    H7     MOVE HF32 TO DF30                                           
           IF DF30 NOT = '  23'     PERFORM BAD ELSE PERFORM GOOD.
    H8     MOVE HF32 TO DF31                                           
           IF DF31 NOT = '  23.4'   PERFORM BAD ELSE PERFORM GOOD.      
    H9     MOVE HF32 TO DF32                                           
           IF DF32 NOT = '  23.45'  PERFORM BAD ELSE PERFORM GOOD.     
    F1     MOVE FF30 TO DF30                                           
            IF DF30 NOT = ' 123'     PERFORM BAD ELSE PERFORM GOOD.
    F2     MOVE FF30 TO DF31                                           
            IF DF31 NOT = ' 123.0'   PERFORM BAD ELSE PERFORM GOOD.      
    F3     MOVE FF30 TO DF32                                           
            IF DF32 NOT = ' 123.00'  PERFORM BAD ELSE PERFORM GOOD.
    F4     MOVE FF31 TO DF30                                           
            IF DF30 NOT = ' 123'     PERFORM BAD ELSE PERFORM GOOD.
    F5     MOVE FF31 TO DF31                                           
            IF DF31 NOT = ' 123.4'   PERFORM BAD ELSE PERFORM GOOD.      
    F6     MOVE FF31 TO DF32                                           
            IF DF32 NOT = ' 123.40'  PERFORM BAD ELSE PERFORM GOOD.
    F7     MOVE FF32 TO DF30                                           
            IF DF30 NOT = ' 123'     PERFORM BAD ELSE PERFORM GOOD.
    F8     MOVE FF32 TO DF31                                           
            IF DF31 NOT = ' 123.4'   PERFORM BAD ELSE PERFORM GOOD.      
    F9     MOVE FF32 TO DF32                                           
            IF DF32 NOT = ' 123.45'  PERFORM BAD ELSE PERFORM GOOD. 
    G1     MOVE GF30 TO DF30                                           
           IF DF30 NOT = ' 123'     PERFORM BAD ELSE PERFORM GOOD.
    G2     MOVE GF30 TO DF31                                           
           IF DF31 NOT = ' 123.0'   PERFORM BAD ELSE PERFORM GOOD.      
    G3     MOVE GF30 TO DF32                                           
           IF DF32 NOT = ' 123.00'  PERFORM BAD ELSE PERFORM GOOD.
    G4     MOVE GF31 TO DF30                                           
           IF DF30 NOT = ' 123'     PERFORM BAD ELSE PERFORM GOOD.
    G5     MOVE GF31 TO DF31                                           
           IF DF31 NOT = ' 123.4'   PERFORM BAD ELSE PERFORM GOOD.      
    G6     MOVE GF31 TO DF32                                           
           IF DF32 NOT = ' 123.40'  PERFORM BAD ELSE PERFORM GOOD.
    G7     MOVE GF32 TO DF30                                           
           IF DF30 NOT = '  23'     PERFORM BAD ELSE PERFORM GOOD.
    G8     MOVE GF32 TO DF31                                           
           IF DF31 NOT = '  23.4'   PERFORM BAD ELSE PERFORM GOOD.      
    G9     MOVE GF32 TO DF32                                           
           IF DF32 NOT = '  23.45'  PERFORM BAD ELSE PERFORM GOOD.     
    Q1     MOVE QF30 TO DF30                                           
            IF DF30 NOT = ' 123'     PERFORM BAD ELSE PERFORM GOOD.
    Q2     MOVE QF30 TO DF31                                           
            IF DF31 NOT = ' 123.0'   PERFORM BAD ELSE PERFORM GOOD.      
    Q3     MOVE QF30 TO DF32                                           
            IF DF32 NOT = ' 123.00'  PERFORM BAD ELSE PERFORM GOOD.
    Q4     MOVE QF31 TO DF30                                           
            IF DF30 NOT = ' 123'     PERFORM BAD ELSE PERFORM GOOD.
    Q5     MOVE QF31 TO DF31                                           
            IF DF31 NOT = ' 123.4'   PERFORM BAD ELSE PERFORM GOOD.      
    Q6     MOVE QF31 TO DF32                                           
            IF DF32 NOT = ' 123.40'  PERFORM BAD ELSE PERFORM GOOD.
    Q7     MOVE QF32 TO DF30                                           
            IF DF30 NOT = ' 123'     PERFORM BAD ELSE PERFORM GOOD.
    Q8     MOVE QF32 TO DF31                                           
            IF DF31 NOT = ' 123.4'   PERFORM BAD ELSE PERFORM GOOD.      
    Q9     MOVE QF32 TO DF32                                           
            IF DF32 NOT = ' 123.45'  PERFORM BAD ELSE PERFORM GOOD. 
           DISPLAY 'TESTMOV2 GOOD TESTS =' GOOD 
           STOP RUN.
       GOOD.
           ADD 1 TO GOOD
       BAD.
           MOVE 16 TO RETURN-CODE
           STOP RUN.
