         TITLE 'Set access registers for test of AR display command'
***********************************************************************
* Start create : 03-04-2016
* 1st delivery :
* Designer     : J Ganci
* Programmer   : J Ganci
* Purpose      : Set access registers to values; used with a script to
*                test the new test-mode command that displays one or
*                all of the access registers for test of AR command
**********************************************************************
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
RPI2000  CSECT ,
RPI2000  AMODE 24
RPI2000  RMODE 24
         B     RPI2000A-RPI2000(,R15)  Branch around header
         DC    AL1(RPI2000A-*),C'RPI2000:&SYSDATC:&SYSTIME'
RPI2000A DS    0H
         STM   R14,R12,12(R13)         Save caller's registers
         LR    R12,R15                 R12 = base register
         USING RPI2000,R12             Establish addressability
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
         MVC   WAEYE,=CL8'RPI2000'     Set eyecatcher
*********************************************************************
* Set access registers for testing of test command AR               *
*********************************************************************
*         LAM   0,15,TARVALS       Load all access registers; S0C1
*
*                                  Abe: if you're reading this, note
*                                  that z390 has not yet implemented
*                                  LAM and STAM. I've implemented it.
*                                  It will be in the next batch
*                                  of RPIs.
*
         LM    R0,R9,TARVALS       Values for AR0-AR9
         SAR   0,0                 AR0  = 0
         SAR   1,1                 AR1  = 1
         SAR   2,2                 AR2  = 2
         SAR   3,3                 AR3  = 3
         SAR   4,4                 AR4  = 4
         SAR   5,5                 AR5  = 5
         SAR   6,6                 AR6  = 6
         SAR   7,7                 AR7  = 7
         SAR   8,8                 AR8  = 8
         SAR   9,9                 AR9  = 9
         LM    R0,R5,TARVALS+4*10  Values for AR10-AR15
         SAR   10,0                AR10 = 10
         SAR   11,1                AR11 = 11
         SAR   12,2                AR12 = 12
         SAR   13,3                AR1  = 1
         SAR   14,4                AR0  = 0
         SAR   15,5                AR1  = 1
*
         L     R1,4(,R13)          Caller's save area
         LM    R0,R9,20(R1)        Restore altered registers
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
         PRINT DATA
TARVALS  DC    16A((*-TARVALS)/4)  VALUES FOR ACCESS REGS: 0-15
*
         END
