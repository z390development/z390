         TITLE 'Test BAL'
***********************************************************************
* Start create : 02-22-2016
* 1st delivery :
* Designer     : J Ganci
* Programmer   : J Ganci
* Purpose      : Test BAL
***********************************************************************
*
* Define constants DSECTs and environment
*
***********************************************************************
         EQUREGS ,
***********************************************************************
*
* Define workareas and fixed constants
*
***********************************************************************
WORKAREA DSECT
WASAVE   DS    18F                 Save area
WAEYE    DS    CL8                 Eyecatcher
WAPARM@  DS    A                   Address of parameter string/list
WAR12H   DS    A                   Original GG12 high half
WALEN    EQU   *-WORKAREA          Length of work area
*
***********************************************************************
*
RPI2002C CSECT ,
RPI2002C AMODE 24
RPI2002C RMODE 24
         B     RPI20020-RPI2002C(,R15) Branch around header
         DC    AL1(RPI20020-*),C'RPI2002C:&SYSDATC:&SYSTIME'
RPI20020 DS    0H
         STM   R14,R12,12(R13)         Save caller's registers
         LR    R12,R15                 R12 = base register
         USING RPI2002C,R12            Establish addressability
         LA    R0,WALEN                Length of work area
         GETMAIN R,LV=(R0),LOC=BELOW   Obtain work area storage
         ST    R1,8(,R13)              Set forward chain in caller
         ST    R13,4(,R1)              Set back chain
         LR    R13,R1                  Current save area
         USING WORKAREA,R13            Overlay work area
         XR    R0,R0                   Zero
         ST    R0,WASAVE                    first word in save area
         L     R1,8(,R13)              Caller's save area
         LM    R14,R1,12(R13)          Restore altered registers
         ST    R1,WAPARM@              Save pointer to parameter list
         STMH  R12,R12,WAR12H          Save high half of R12
         LLGTR R12,R12                 Ensure high half of R12 is zero
         MVC   WAEYE,=CL8'RPI2002A'    Set eyecatcher
***********************************************************************
*
* 45 = BAL - Branch and Link
*
* operand1 is 32-bit or 64-bit link information
* operand2 is 32-bit or 64-bit branch address
*
***********************************************************************
*
* Amode 24 tests
*
         SAM24 ,                   Ensure amode 24
*
         LGHI  R14,-1              Set GG14 high half to known value
         LGR   R3,R14              Also GG3
*
         SR    R5,R5               Prepare to get pgm mask
         LR    R0,R5               For new pgm mask
         OILH  R0,X'0600'          New pgm mask: dec, hfp ovfl
         IPM   R5                  Save current pgm mask
         SPM   R0                  Set pgm mask to 0110
*
         LGHI  R4,1                Used to set CC
*
         CR    R4,R4               Set CC = 0
         BAL   R14,SUB             Call subroutine
         LA    R3,*                Address after BAL
         IPM   R3                  Get CC, pgm mask
         OILH  R3,X'8000'          Set ILC
         CGR   R3,R14              Should be equal (CGR intentional)
         BE    BAL241              Continue if equal
         DC    H'0'                Stop if not
BAL241   DS    0H
*
         LCR   R0,R4               Set CC = 1
         BAL   R14,SUB             Call subroutine
         LA    R3,*                Address after BAL
         IPM   R3                  Get CC, pgm mask
         OILH  R3,X'8000'          Set ILC
         CGR   R3,R14              Should be equal (CGR intentional)
         BE    BAL242              Continue if equal
         DC    H'0'                Stop if not
BAL242   DS    0H
*
         LTR   R4,R4               Set CC = 2
         BAL   R14,SUB             Call subroutine
         LA    R3,*                Address after BAL
         IPM   R3                  Get CC, pgm mask
         OILH  R3,X'8000'          Set ILC
         CGR   R3,R14              Should be equal (CGR intentional)
         BE    BAL243              Continue if equal
         DC    H'0'                Stop if not
BAL243   DS    0H
*
         TM    *+1,B'0001'         Set CC = 3
         BAL   R14,SUB             Call subroutine
         LA    R3,*                Address after BAL
         IPM   R3                  Get CC, pgm mask
         OILH  R3,X'8000'          Set ILC
         CGR   R3,R14              Should be equal (CGR intentional)
         BE    BAL244              Continue if equal
         DC    H'0'                Stop if not
BAL244   DS    0H
*
* Executed BALs - 4 EX and 4 EXRL
*
         LA    R1,14*16            Execute mask
*
         CR    R4,R4               Set CC = 0
         EX    R1,EBAL             Call subroutine
         LA    R3,*                Address after BAL
         IPM   R3                  Get CC, pgm mask
         OILH  R3,X'8000'          Set ILC
         CGR   R3,R14              Should be equal (CGR intentional)
         BE    BAL24E1             Continue if equal
         DC    H'0'                Stop if not
BAL24E1  DS    0H
*
         LCR   R0,R4               Set CC = 1
         EX    R1,EBAL             Call subroutine
         LA    R3,*                Address after BAL
         IPM   R3                  Get CC, pgm mask
         OILH  R3,X'8000'          Set ILC
         CGR   R3,R14              Should be equal (CGR intentional)
         BE    BAL24E2             Continue if equal
         DC    H'0'                Stop if not
BAL24E2  DS    0H
*
         LTR   R4,R4               Set CC = 2
         EX    R1,EBAL             Call subroutine
         LA    R3,*                Address after BAL
         IPM   R3                  Get CC, pgm mask
         OILH  R3,X'8000'          Set ILC
         CGR   R3,R14              Should be equal (CGR intentional)
         BE    BAL24E3             Continue if equal
         DC    H'0'                Stop if not
BAL24E3  DS    0H
*
         TM    *+1,B'0001'         Set CC = 3
         EX    R1,EBAL             Call subroutine
         LA    R3,*                Address after BAL
         IPM   R3                  Get CC, pgm mask
         OILH  R3,X'8000'          Set ILC
         CGR   R3,R14              Should be equal (CGR intentional)
         BE    BAL24E4             Continue if equal
         DC    H'0'                Stop if not
BAL24E4  DS    0H
*
         CR    R4,R4               Set CC = 0
         EXRL  R1,EBAL             Call subroutine
         LA    R3,*                Address after BAL
         IPM   R3                  Get CC, pgm mask
         OILH  R3,X'C000'          Set ILC
         CGR   R3,R14              Should be equal (CGR intentional)
         BE    BAL24E5             Continue if equal
         DC    H'0'                Stop if not
BAL24E5  DS    0H
*
         LCR   R0,R4               Set CC = 1
         EXRL  R1,EBAL             Call subroutine
         LA    R3,*                Address after BAL
         IPM   R3                  Get CC, pgm mask
         OILH  R3,X'C000'          Set ILC
         CGR   R3,R14              Should be equal (CGR intentional)
         BE    BAL24E6             Continue if equal
         DC    H'0'                Stop if not
BAL24E6  DS    0H
*
         LTR   R4,R4               Set CC = 2
         EXRL  R1,EBAL             Call subroutine
         LA    R3,*                Address after BAL
         IPM   R3                  Get CC, pgm mask
         OILH  R3,X'C000'          Set ILC
         CGR   R3,R14              Should be equal (CGR intentional)
         BE    BAL24E7             Continue if equal
         DC    H'0'                Stop if not
BAL24E7  DS    0H
*
         TM    *+1,B'0001'         Set CC = 3
         EXRL  R1,EBAL             Call subroutine
         LA    R3,*                Address after BAL
         IPM   R3                  Get CC, pgm mask
         OILH  R3,X'C000'          Set ILC
         CGR   R3,R14              Should be equal (CGR intentional)
         BE    BAL24E8             Continue if equal
         DC    H'0'                Stop if not
BAL24E8  DS    0H
*
* End amode 24 tests
*
         SPM   R5                  Restore pgm mask
         SAM31 ,                   Restore amode 31
***********************************************************************
*
* Amode 31 tests
*
         SAM31 ,                   Ensure amode 31
*
         BAL   R14,SUB             Call subroutine
         LA    R3,*                Address after BAL
         OILH  R3,X'8000'          Set amode 31
         CGR   R3,R14              Should be equal (CGR intentional)
         BE    BAL311              Continue if equal
         DC    H'0'                Stop if not
BAL311   DS    0H
*
* Executed BALs - 1 EX and 1 EXRL
*
         LA    R1,14*16            Execute mask
*
         EX    R1,EBAL             Call subroutine
         LA    R3,*                Address after BAL
         OILH  R3,X'8000'          Set amode 31
         CGR   R3,R14              Should be equal (CGR intentional)
         BE    BAL31E1             Continue if equal
         DC    H'0'                Stop if not
BAL31E1  DS    0H
*
         EXRL  R1,EBAL             Call subroutine
         LA    R3,*                Address after BAL
         OILH  R3,X'8000'          Set amode 31
         CGR   R3,R14              Should be equal (CGR intentional)
         BE    BAL31E2             Continue if equal
         DC    H'0'                Stop if not
BAL31E2  DS    0H
*
* End amode 31 tests
*
         SAM31 ,                   Restore amode 31
***********************************************************************
*
* Amode 64 tests
*
         SAM64 ,                   Ensure amode 64
*
         SGR   R3,R3               Set GG3 high half to 0; LA bug
*
         LGFI  R14,-1              Initialize GG14
         BAL   R14,SUB             Call subroutine
         LA    R3,*                Address after BAL
         CGR   R3,R14              Should be equal
         BE    BAL641              Continue if equal
         DC    H'0'                Stop if not
BAL641   DS    0H
*
* Executed BALs - 1 EX and 1 EXRL
*
         LA    R1,14*16            Execute mask
*
         LGFI  R14,-1              Set GG14 to known value
         EX    R1,EBAL             Call subroutine
         LA    R3,*                Address after BAL
         CGR   R3,R14              Should be equal
         BE    BAL64E1             Continue if equal
         DC    H'0'                Stop if not
BAL64E1  DS    0H
*
         LGFI  R14,-1              Set GG14 to known value
         EXRL  R1,EBAL             Call subroutine
         LA    R3,*                Address after BAL
         CGR   R3,R14              Should be equal
         BE    BAL64E2             Continue if equal
         DC    H'0'                Stop if not
BAL64E2  DS    0H
*
* End amode 64 tests
*
         SAM31 ,                   Restore amode 31
***********************************************************************
* Done
***********************************************************************
*NSI     B     EXIT                Done
***********************************************************************
EXIT     DS    0H
         LR    R1,R13              R1 --> work area
         L     R13,4(,R13)         R13 --> Caller's save area
         LA    R0,WALEN            Length of work area
         FREEMAIN R,LV=(R0),A=(R1) Release work area storage
         LM    R14,R12,12(R13)     Restore caller's registers
         SR    R15,R15             Set return code to zero
         BR    R14                 Return to caller
***********************************************************************
* Internal subroutine
***********************************************************************
SUB      DS    0H 
         BR    R14                 Return
***********************************************************************
* Executed BAL
***********************************************************************
EBAL     BAL   *-*,SUB             Executed
*
         LTORG
*
         END
