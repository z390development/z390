         TITLE 'Test TAM'
***********************************************************************
* Start create : 03-01-2016
* 1st delivery :
* Designer     : J Ganci
* Programmer   : J Ganci
* Purpose      : Test TAM
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
RPI2004  CSECT ,
RPI2004  AMODE 24
RPI2004  RMODE 24
         B     RPI2004A-RPI2004(,R15)  Branch around header
         DC    AL1(RPI2004A-*),C'RPI2004:&SYSDATC:&SYSTIME'
RPI2004A DS    0H
         STM   R14,R12,12(R13)         Save caller's registers
         LR    R12,R15                 R12 = base register
         USING RPI2004,R12             Establish addressability
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
         MVC   WAEYE,=CL8'TLAM'        Set eyecatcher
*
         SAM31 ,                       Ensure amode 31
***********************************************************************
*
* 010B = TAM - Test Addressing Mode
*
* no operands
*
***********************************************************************
* Test TAM
***********************************************************************
         LHI   R1,1                used to set CC
*
         SAM24 ,                   Amode 24
*        v psw.amode = x'0'
*
         TM   *+1,B'1000'          Set CC to 3
*        v psw.cc = x'3'
*
         TAM   ,                   Test addressing mode
*        v psw.cc    = x'0'
*        v psw.amode = x'0'
         BC    B'1000',T24E        Branch if correct CC
         DC    H'0'                Stop if not
T24E     DS    0H
*
         SAM31 ,                   Amode 31
*        v psw.amode = x'1'
*
         TM   *+1,B'1000'          Set CC to 3
*        v psw.cc = x'3'
*
         TAM   ,                   Test addressing mode
*        v psw.cc = x'1'
*        v psw.amode = '1'
         BC    B'0100',T31E        Branch if correct CC
         DC    H'0'                Stop if not
T31E     DS    0H
*
         SAM64 ,                   Amode 64
*        v psw.amode = x'3'
*
         CHI   R1,2                Set CC to 1
*        v psw.cc = x'1'
         TAM   ,                   Test addressing mode
*        v psw.cc = x'3'
*        v psw.amode = '3'
         BC    B'0001',T64E        Branch if correct CC
         DC    H'0'                Stop if not
T64E     DS    0H
*
* Done
*
         SAM31 ,                   Back to amode 31
***********************************************************************
EXIT     DS    0H
         LR    R1,R13              R1 --> work area
         L     R13,4(,R13)         R13 --> Caller's save area
         LA    R0,WALEN            Length of work area
         FREEMAIN R,LV=(R0),A=(R1) Release work area storage
         LM    R14,R12,12(R13)     Restore caller's registers
         SR    R15,R15             Set return code to zero
         BR    R14                 Return to caller
*
         LTORG
*
         END
