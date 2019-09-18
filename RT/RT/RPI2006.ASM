         TITLE 'Test List of storage above the line running amode 24'
***********************************************************************
* Start create : 03-01-2016
* 1st delivery :
* Designer     : J Ganci
* Programmer   : J Ganci
* Purpose      : Test LAM, LAMY, STAM, and STAMY
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
STOR     DSECT
STLIT1   DS    CL32                Character data
STF1     DS    F                   32-bit binary data
STF2     DS    F                   32-bit binary data
STF3     DS    F                   32-bit binary data
STF4     DS    F                   32-bit binary data
STLIT2   DS    CL32                Character data
STLEN    EQU   *-STOR              Length of storage area
***********************************************************************
*
RPI2006  CSECT ,
RPI2006  AMODE 24
RPI2006  RMODE 24
         B     RPI2006A-RPI2006(,R15)  Branch around header
         DC    AL1(RPI2006A-*),C'RPI2006:&SYSDATC:&SYSTIME'
RPI2006A DS    0H
         STM   R14,R12,12(R13)         Save caller's registers
         LR    R12,R15                 R12 = base register
         USING RPI2006,R12             Establish addressability
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
***********************************************************************
*
* Obtain some storage above the line
*
* Use the test-mode V command to check the storage
*
***********************************************************************
* 1. Obtain the storage
***********************************************************************
         SAM31 ,                   Ensure amode 31
*
         LA    R0,STLEN            Length of storage to obtain
         GETMAIN RU,LV=(R0),LOC=31 Obtain storage above the line
         USING STOR,R1             Overlay the storage
         LLGTR R1,R1               Set GG1 high half to 0
*        v 1r = x'0000000001000000'
***********************************************************************
* 2. Initialize the storage
***********************************************************************
         MVC   STLIT1,LIT1         Copy first literal to storage
         L     R15,F1              First fullword value
         ST    R15,STF1            Put in storage
         L     R15,F2              Second fullword value
         ST    R15,STF2            Put in storage
         L     R15,F3              Third fullword value
         ST    R15,STF3            Put in storage
         L     R15,F4              Fourth fullword value
         ST    R15,STF4            Put in storage
         MVC   STLIT2,LIT2         Copy second literal to storage
***********************************************************************
* 3. List the storage when running amode 24, 31, and 64
***********************************************************************
*
* amode 24 test
*
         SAM24 ,                   amode 24
*
*        l 1000000. 80
*        l 1r? 80
*        l 1r% 80
*
*
* amode 31 test
*
         SAM31 ,                   amode 31
*
*        l 1000000. 80
*        l 1r? 80
*        l 1r% 80
*
*
* amode 64 test
*
         SAM64 ,                   amode 64
*
*        l 1000000. 80
*        l 1r? 80
*        l 1r% 80
*
* done
*
         SAM31 ,                   Restore amode 31
***********************************************************************
* Done
***********************************************************************
EXIT     DS    0H
         DROP  R1                      End storage area overlay
         FREEMAIN RU,LV=STLEN,A=(R1)   Release the storage area
*
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
***********************************************************************
*        Constants
***********************************************************************
*
F1       DC    F'2'                Fullword value
F2       DC    F'3'                Fullword value
F3       DC    F'5'                Fullword value
F4       DC    F'7'                Fullword value
*
LIT1     DC    CL(L'STLIT1)'The first four prime numbers are'
LIT2     DC    CL(L'STLIT2)'Don''t you think z390 is fun?'
*
         END
