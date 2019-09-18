         TITLE 'Test BAS'
***********************************************************************
* Start create : 02-22-2016
* 1st delivery :
* Designer     : J Ganci
* Programmer   : J Ganci
* Purpose      : Test BAS
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
RPI2002B CSECT ,
RPI2002B AMODE 24
RPI2002B RMODE 24
         B     RPI20020-RPI2002B(,R15) Branch around header
         DC    AL1(RPI20020-*),C'RPI2002B:&SYSDATC:&SYSTIME'
RPI20020 DS    0H
         STM   R14,R12,12(R13)         Save caller's registers
         LR    R12,R15                 R12 = base register
         USING RPI2002B,R12            Establish addressability
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
         MVC   WAEYE,=CL8'RPI2002B'    Set eyecatcher
***********************************************************************
*
* 4D = BAS - Branch and Save
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
         LGHI  R14,-1              Set GG14 to known value
         LGR   R3,R14              Also GG3
*
         BAS   R14,SUB             Call subroutine
         LA    R3,*                Address after BAS
         CGR   R3,R14              Should be equal (CGR intentional)
         BE    BAS241              Continue if equal
         DC    H'0'                Stop if not
BAS241   DS    0H
*
* Executed BASs
*
         LA    R1,14*16            Execute mask
*
         EX    R1,EBAS             Call subroutine
         LA    R3,*                Address after BAS
         CGR   R3,R14              Should be equal (CGR intentional)
         BE    BAS24E1             Continue if equal
         DC    H'0'                Stop if not
BAS24E1  DS    0H
*
         EXRL  R1,EBAS             Call subroutine
         LA    R3,*                Address after BAS
         CGR   R3,R14              Should be equal (CGR intentional)
         BE    BAS24E2             Continue if equal
         DC    H'0'                Stop if not
BAS24E2  DS    0H
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
         BAS   R14,SUB             Call subroutine
         LA    R3,*                Address after BAS
         OILH  R3,X'8000'          Set amode 31
         CGR   R3,R14              Should be equal (CGR intentional)
         BE    BAS311              Continue if equal
         DC    H'0'                Stop if not
BAS311   DS    0H
*
* Executed BASs
*
         LA    R1,14*16            Execute mask
*
         EX    R1,EBAS             Call subroutine
         LA    R3,*                Address after BAS
         OILH  R3,X'8000'          Set amode 31
         CGR   R3,R14              Should be equal (CGR intentional)
         BE    BAS31E1             Continue if equal
         DC    H'0'                Stop if not
BAS31E1  DS    0H
*
         EXRL  R1,EBAS             Call subroutine
         LA    R3,*                Address after BAS
         OILH  R3,X'8000'          Set amode 31
         CGR   R3,R14              Should be equal (CGR intentional)
         BE    BAS31E2             Continue if equal
         DC    H'0'                Stop if not
BAS31E2  DS    0H
*
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
         SGR   R3,R3               Needed due to error in LA
*
         LGFI  R14,-1              Set GG14 to known value
         BAS   R14,SUB             Call subroutine
         LA    R3,*                Address after BAS
         CGR   R3,R14              Should be equal
         BE    BAS641              Continue if equal
         DC    H'0'                Stop if not
BAS641   DS    0H
*
* Executed BASs
*
         LA    R1,14*16            Execute mask
*
         LGFI  R14,-1              Set GG14 to known value
         EX    R1,EBAS             Call subroutine
         LA    R3,*                Address after BAS
         CGR   R3,R14              Should be equal
         BE    BAS64E1             Continue if equal
         DC    H'0'                Stop if not
BAS64E1  DS    0H
*
         LGFI  R14,-1              Set GG14 to known value
         EXRL  R1,EBAS             Call subroutine
         LA    R3,*                Address after BAS
         CGR   R3,R14              Should be equal
         BE    BAS64E2             Continue if equal
         DC    H'0'                Stop if not
BAS64E2  DS    0H
*
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
* Executed BAS
***********************************************************************
EBAS     BAS   *-*,SUB             Executed
*
         LTORG
*
         END
