         TITLE 'Test BASR'
***********************************************************************
* Start create : 02-22-2016
* 1st delivery :
* Designer     : J Ganci
* Programmer   : J Ganci
* Purpose      : Test BASR
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
RPI2002A CSECT ,
RPI2002A AMODE 24
RPI2002A RMODE 24
         B     RPI20020-RPI2002A(,R15) Branch around header
         DC    AL1(RPI20020-*),C'RPI2002A:&SYSDATC:&SYSTIME'
RPI20020 DS    0H
         STM   R14,R12,12(R13)         Save caller's registers
         LR    R12,R15                 R12 = base register
         USING RPI2002A,R12            Establish addressability
         LA    R0,WALEN                Length of work area
         GETMAIN R,LV=(R0),LOC=BELOW   Obtain work area storage
         ST    R1,8(,R13)              Set forward chain in caller
         ST    R13,4(,R1)              Set back chain
         LR    R13,R1                  Current save area
         USING WORKAREA,R13            Overlay work area
         XR    R0,R0                   Zero
         ST    R0,WASAVE                    first word in save area
         L     R1,4(,R13)              Caller's save area
         LM    R14,R1,12(R1)           Restore altered registers
         ST    R1,WAPARM@              Save pointer to parameter list
         STMH  R12,R12,WAR12H          Save high half of R12
         LLGTR R12,R12                 Ensure high half of R12 is zero
***********************************************************************
*
* Set up working storage areas
*
***********************************************************************
         MVC   WAEYE,=CL8'RPI2002A'    Set eyecatcher
***********************************************************************
*
* 0D = BASR - Branch and Save
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
         LGHI  R15,-1              Set GG15 to known value
         LGR   R14,R15             Also GG14
         LGR   R3,R15              Also GG3
*
         LA    R15,SUB             Subroutine; high R15 = X'FFFFFFFF'
*
         BASR  R14,R15             Call subroutine
         LA    R3,*                Address after BASR
         CGR   R3,R14              Should be equal (CGR intentional)
         BE    BASR241             Continue if equal
         DC    H'0'                Stop if not
BASR241  DS    0H
*
         LA    R14,SUB             Subroutine; high R14 = X'FFFFFFFF'
*
         BASR  R14,R14             Call subroutine
         LA    R3,*                Address after BASR
         CGR   R3,R14              Should be equal (CGR intentional)
         BE    BASR242             Continue if equal
         DC    H'0'                Stop if not
BASR242  DS    0H
*
         BASR  R14,R0              No branch
         LA    R3,*                Address after BASR
         CGR   R3,R14              Should be equal (CGR intentional)
         BE    BASR243             Continue if equal
         DC    H'0'                Stop if not
BASR243  DS    0H
*
* Executed BASRs
*
         LA    R1,14*16            Execute mask
*
         EX    R1,EBASR            Call subroutine
         LA    R3,*                Address after BASR
         CGR   R3,R14              Should be equal (CGR intentional)
         BE    BASR24E1            Continue if equal
         DC    H'0'                Stop if not
BASR24E1 DS    0H
*
         EX    R1,EBASR0           No branch
         LA    R3,*                Address after BASR
         CGR   R3,R14              Should be equal (CGR intentional)
         BE    BASR24E2            Continue if equal
         DC    H'0'                Stop if not
BASR24E2 DS    0H
*
         EXRL  R1,EBASR            Call subroutine
         LA    R3,*                Address after BASR
         CGR   R3,R14              Should be equal (CGR intentional)
         BE    BASR24E3            Continue if equal
         DC    H'0'                Stop if not
BASR24E3 DS    0H
*
         EXRL  R1,EBASR0           No branch
         LA    R3,*                Address after BASR
         CGR   R3,R14              Should be equal (CGR intentional)
         BE    BASR24E4            Continue if equal
         DC    H'0'                Stop if not
BASR24E4 DS    0H
*
* End amode 24 tests
*
         SAM31 ,                   Restore amode 31
***********************************************************************
*
* Amode 31 tests
*
         SAM31 ,                   Ensure amode 31
*
         LA    R15,SUB             Subroutine; high R15 = X'FFFFFFFF'
*
         BASR  R14,R15             Call subroutine
         LA    R3,*                Address after BASR
         OILH  R3,X'8000'          Set amode 31
         CGR   R3,R14              Should be equal (CGR intentional)
         BE    BASR311             Continue if equal
         DC    H'0'                Stop if not
BASR311  DS    0H
*
         LA    R14,SUB             Subroutine; high R14 = X'FFFFFFFF'
*
         BASR  R14,R14             Call subroutine
         LA    R3,*                Address after BASR
         OILH  R3,X'8000'          Set amode 31
         CGR   R3,R14              Should be equal (CGR intentional)
         BE    BASR312             Continue if equal
         DC    H'0'                Stop if not
BASR312  DS    0H
*
         BASR  R14,R0              No branch
         LA    R3,*                Address after BASR
         OILH  R3,X'8000'          Set amode 31
         CGR   R3,R14              Should be equal (CGR intentional)
         BE    BASR313             Continue if equal
         DC    H'0'                Stop if not
BASR313  DS    0H
*
* Executed BASRs
*
         LA    R1,14*16            Execute mask
*
         EX    R1,EBASR            Call subroutine
         LA    R3,*                Address after BASR
         OILH  R3,X'8000'          Set amode 31
         CGR   R3,R14              Should be equal (CGR intentional)
         BE    BASR31E1            Continue if equal
         DC    H'0'                Stop if not
BASR31E1 DS    0H
*
         EX    R1,EBASR0           No branch
         LA    R3,*                Address after BASR
         OILH  R3,X'8000'          Set amode 31
         CGR   R3,R14              Should be equal (CGR intentional)
         BE    BASR31E2            Continue if equal
         DC    H'0'                Stop if not
BASR31E2 DS    0H
*
         EXRL  R1,EBASR            Call subroutine
         LA    R3,*                Address after BASR
         OILH  R3,X'8000'          Set amode 31
         CGR   R3,R14              Should be equal (CGR intentional)
         BE    BASR31E3            Continue if equal
         DC    H'0'                Stop if not
BASR31E3 DS    0H
*
         EXRL  R1,EBASR0           No branch
         LA    R3,*                Address after BASR
         OILH  R3,X'8000'          Set amode 31
         CGR   R3,R14              Should be equal (CGR intentional)
         BE    BASR31E4            Continue if equal
         DC    H'0'                Stop if not
BASR31E4 DS    0H
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
         SGR   R15,R15             Set GG15 high half to 0; LA bug
         SGR   R3,R3               Also GG3
*
         LA    R15,SUB             Subroutine
*
         LGFI  R14,-1              Set GG14 to known value
         BASR  R14,R15             Call subroutine
         LA    R3,*                Address after BASR
         CGR   R3,R14              Should be equal
         BE    BASR641             Continue if equal
         DC    H'0'                Stop if not
BASR641  DS    0H
*
         SGR   R14,R14             Set GG14 high half to 0; LA bug
         LA    R14,SUB             Subroutine
         BASR  R14,R14             Call subroutine
         LA    R3,*                Address after BASR
         CGR   R3,R14              Should be equal
         BE    BASR642             Continue if equal
         DC    H'0'                Stop if not
BASR642  DS    0H
*
         LGFI  R14,-1              Set GG14 to known value
         BASR  R14,R0              No branch
         LA    R3,*                Address after BASR
         CGR   R3,R14              Should be equal
         BE    BASR643             Continue if equal
         DC    H'0'                Stop if not
BASR643  DS    0H
*
* Executed BASRs
*
         LA    R1,14*16            Execute mask
*
         SGR   R14,R14             Set GG14 high half to 0; LA bug
         EX    R1,EBASR            Call subroutine
         LA    R3,*                Address after BASR
         CGR   R3,R14              Should be equal
         BE    BASR64E1            Continue if equal
         DC    H'0'                Stop if not
BASR64E1 DS    0H
*
         LGFI  R14,-1              Initialize GG14
         EX    R1,EBASR0           No branch
         LA    R3,*                Address after BASR
         CGR   R3,R14              Should be equal
         BE    BASR64E2            Continue if equal
         DC    H'0'                Stop if not
BASR64E2 DS    0H
*
         SGR   R14,R14             Set GG14 high half to 0; LA bug
         EXRL  R1,EBASR            Call subroutine
         LA    R3,*                Address after BASR
         CGR   R3,R14              Should be equal
         BE    BASR64E3            Continue if equal
         DC    H'0'                Stop if not
BASR64E3 DS    0H
*
         LGFI  R14,-1              Set GG14 to known value
         EXRL  R1,EBASR0           No branch
         LA    R3,*                Address after BASR
         CGR   R3,R14              Should be equal
         BE    BASR64E4            Continue if equal
         DC    H'0'                Stop if not
BASR64E4 DS    0H
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
         LMH   R12,R12,WAR12H      Restore high half of R12
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
* Executed BASRs
***********************************************************************
EBASR    BASR  *-*,R15             Executed
EBASR0   BASR  *-*,R0              Executed
*
         LTORG
*
         END
