        TITLE 'Test fix for test-mode command 1r=x''8000000000000000'''
***********************************************************************
* Start create : 03-09-2016
* 1st delivery :
* Designer     : J Ganci
* Programmer   : J Ganci
* Purpose      : Test fix so "1r=x'8000000000000000'" is okay
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
RPI2007  CSECT ,
RPI2007  AMODE 31
RPI2007  RMODE 24
         B     RPI2007A-RPI2007(,R15)  Branch around header
         DC    AL1(RPI2007A-*),C'RPI2007:&SYSDATC:&SYSTIME'
RPI2007A DS    0H
         STM   R14,R12,12(R13)         Save caller's registers
         LR    R12,R15                 Base register
         USING RPI2007,R12             Establish addressability
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
         MVC   WAEYE,=CL8'RPI2007'     Set eyecatcher
***********************************************************************
*
* Set addressing modes and initial GG1
*
* Using test commands set GG1 to values; display them
*
***********************************************************************
*
         SAM24 ,                   Amode 24
*
         LGHI  R1,-1               Set GG1 to known value
*        v 1r = x'ffffffffffffffff'
*        l 1r
*        1r   = x'7fffffffffffffff'
*        v 1r = x'7fffffffffffffff'
*        l 1r
*        1r   = x'8000000000000000'
*        v 1r = x'8000000000000000'
*        l 1r
*        1r   = x'90000000000000000' <--- fails intentionally
*        v 1r = x'8000000000000000'
*        l 1r
*
         LGHI  R1,-1               Set GG1 to -1
         NIHH  R1,X'7FFF'          Now it's largest positive
*        s 1r = x'8000000000000000'
         AGHI  R1,1                Add 1; breakpoint hits
*        v 1r = x'8000000000000000'
*        l 1r
*
         SGR   R1,R1               Set GG1 to 0
*        s 1r = x'0000000000000001'
         AGHI  R1,1                Add 1; breakpoint hits
*         
         SAM31 ,                   Amode 31
*
         LGHI  R1,-1               Set GG1 to known value
*        v 1r = x'ffffffffffffffff'
*        l 1r
*        1r   = x'7fffffffffffffff'
*        v 1r = x'7fffffffffffffff'
*        l 1r
*        1r   = x'8000000000000000'
*        v 1r = x'8000000000000000'
*        l 1r
*        1r   = x'90000000000000000' <--- fails intentionally
*        v 1r = x'8000000000000000'
*        l 1r
*
         LGHI  R1,-1               Set GG1 to -1
         NIHH  R1,X'7FFF'          Now it's largest positive
*        s 1r = x'8000000000000000'
         AGHI  R1,1                Add 1; breakpoint hits
*        v 1r = x'8000000000000000'
*        l 1r
*
         SGR   R1,R1               Set GG1 to 0
*        s 1r = x'0000000000000001'
         AGHI  R1,1                Add 1; breakpoint hits
*         
         SAM64 ,                   Amode 64
*
         LGHI  R1,-1               Set GG1 to known value
*        v 1r = x'ffffffffffffffff'
*        l 1r
*        1r   = x'7fffffffffffffff'
*        v 1r = x'7fffffffffffffff'
*        l 1r
*        1r   = x'8000000000000000'
*        v 1r = x'8000000000000000'
*        l 1r
*        1r   = x'90000000000000000' <--- fails intentionally
*        v 1r = x'8000000000000000'
*        l 1r
*
         LGHI  R1,-1               Set GG1 to -1
         NIHH  R1,X'7FFF'          Now it's largest positive
*        s 1r = x'8000000000000000'
         AGHI  R1,1                Add 1; breakpoint hits
*        v 1r = x'8000000000000000'
*        l 1r
*
         SGR   R1,R1               Set GG1 to 0
*        s 1r = x'0000000000000001'
         AGHI  R1,1                Add 1; breakpoint hits
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
