         TITLE 'Test ALSIH and ALSIHN Instructions'
***********************************************************************
* Start create : 03-06-2017
* 1st delivery :
* Designer     : J Ganci
* Programmer   : J Ganci
* Purpose      : Test ALSIH, ALSIHN
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
RPI2009F CSECT ,
RPI2009F AMODE 31
RPI2009F RMODE 31
         B     RPI20090-RPI2009F(,R15) Branch around header
         DC    AL1(RPI20090-*),C'RPI2009F:&SYSDATC:&SYSTIME'
RPI20090 DS    0H
         STM   R14,R12,12(R13)         Save caller's registers
         LR    R12,R15                 R12 = base register
         USING RPI2009F,R12            Establish addressability
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
         MVC   WAEYE,=CL8'RPI2009F'    Set eyecatcher
*
***********************************************************************
*
*        Program mask = 1000; not needed since logical instructions
*
***********************************************************************
*
         IILF  R0,X'08000000'       Set program mask bits to 1000
         SPM   R0                   Set program mask to 1000
*
         SGR   R2,R2                R2 used to fetch CC
*
***********************************************************************
*
* CCA = ALSIH - Add Logical With Signed Immediate High (CC set)
*
* operand1 is 32-bit signed value in high half of 64-bit register
* operand2 is immediate 32-bit signed value
*
***********************************************************************
*
*        Test 1a: ALSIH zero; no carry; CC=0
*
         SR    R0,R0               Initialize CC
         TM    *,X'80'             Set CC=3
         LG    R0,TST01a           Initialize register 0
         ALSIH R0,0                CC should be 0
         IPM   R2                  Fetch CC
*
*        Test 1b: ALSIH; not zero; no carry; CC=1
*
         SR    R0,R0               Initialize CC
         LG    R0,TST01b           Initialize register 0
         ALSIH R0,1                CC should be 1
         IPM   R2                  Fetch CC
*
*        Test 1c: ALSIH; zero; carry; CC=2
*
         SR    R0,R0               Initialize CC
         LG    R0,TST01c           Initialize register 0
         ALSIH R0,1                CC should be 2
         IPM   R2                  Fetch CC
*
*        Test 1d: ALSIH; not zero; carry; CC=3
*
         SR    R0,R0               Initialize CC
         LG    R0,TST01d           Initialize register 0
         ALSIH R0,2                CC should be 3
         IPM   R2                  Fetch CC
*
***********************************************************************
*
* CCB = ALSIHN - Add Logical With Signed Immediate High (CC unchanged)
*
* operand1 is 32-bit signed value in high half of 64-bit register
* operand2 is immediate 32-bit signed value
*
***********************************************************************
*
*        Test 1a: ALSIHN zero; no carry; CC=0
*
         SR    R0,R0               Initialize CC
         TM    *,X'80'             Set CC=3
         LG    R0,TST01a           Initialize register 0
         ALSIHN R0,0               CC should still be 3
         IPM   R2                  Fetch CC
*
*        Test 1b: ALSIHN; not zero; no carry; CC=1
*
         SR    R0,R0               Initialize CC
         LG    R0,TST01b           Initialize register 0
         ALSIHN R0,1               CC should still be 0
         IPM   R2                  Fetch CC
*
*        Test 1c: ALSIHN; zero; carry; CC=2
*
         SR    R0,R0               Initialize CC
         LG    R0,TST01c           Initialize register 0
         ALSIHN R0,1               CC should still be 0
         IPM   R2                  Fetch CC
*
*        Test 1d: ALSIHN; not zero; carry
*
         SR    R0,R0               Initialize CC
         LG    R0,TST01d           Initialize register 0
         ALSIHN R0,2               CC should still be 0
         IPM   R2                  Fetch CC
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
*
         LTORG
*
         DS    0D
TST01a   DC    X'00000000FFFFFFFF'
TST01b   DC    X'7FFFFFFFFFFFFFFF'
TST01c   DC    X'FFFFFFFFFFFFFFFF'
TST01d   DC    X'FFFFFFFFFFFFFFFF'
*
         END
