         TITLE 'Test new test-mode command PSW16'
***********************************************************************
* Start create : 03-09-2016
* 1st delivery :
* Designer     : J Ganci
* Programmer   : J Ganci
* Purpose      : Test new test-command PSW16
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
RPI2008  CSECT ,
RPI2008  AMODE 31
RPI2008  RMODE 24
         B     RPI2008A-RPI2008(,R15)  Branch around header
         DC    AL1(RPI2008A-*),C'RPI2008:&SYSDATC:&SYSTIME'
RPI2008A DS    0H
         STM   R14,R12,12(R13)         Save caller's registers
         LR    R12,R15                 Base register
         USING RPI2008,R12             Establish addressability
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
         STMH  R12,R12,WAR12H          Save R12 high half
         LLGTR R12,R12                 Ensure high half is zero
***********************************************************************
*
* Set up working storage areas
*
***********************************************************************
         MVC   WAEYE,=CL8'RPI2008'     Set eyecatcher
***********************************************************************
*
* Set addressing modes; using test, verify PSW displays key 8
*
***********************************************************************
*
         TM    *+1,B'1000'         Set CC = 3
*
         SAM24 ,                   Amode 24
*
         SAM31 ,                   Amode 31
*
         SAM64 ,                   Amode 64
*
         SAM31 ,                   Restore amode 31
***********************************************************************
* Done
***********************************************************************
EXIT     DS    0H
         LMH   R12,R12,WAR12H      Restore R12 high half
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
