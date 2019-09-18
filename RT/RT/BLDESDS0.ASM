         TITLE 'Build a zVSAM ESDS dataset'
***********************************************************************
* Start create : 27-12-2016
* 1st delivery :
* Designer     : Abe Kornelis
* Programmer   : Abe Kornelis
* Purpose      : Test creation of a zVSAM ESDS dataset
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
WALEN    EQU   *-WORKAREA          Length of work area
*
***********************************************************************
*
BLDESDS0 CSECT ,
BLDESDS0 AMODE 31
BLDESDS0 RMODE 24
         B     BLDESDS-BLDESDS0(,R15)  Branch around header
         DC    AL1(BLDESDS-*),C'BLDESDS0:&SYSDATC:&SYSTIME'
BLDESDS  DS    0H
         STM   R14,R12,12(R13)         Save caller's registers
         LR    R12,R15                 Base register
         USING BLDESDS0,R12            Establish addressability
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
***********************************************************************
*
* Set up working storage areas
*
***********************************************************************
         MVC   WAEYE,=CL8'BLDESDS0'    Set eyecatcher
***********************************************************************
*
* Open the ACB
*
***********************************************************************
         OPEN  (MYACB)
*
         MVC   MYRECORD,RECORD1
         PUT   RPL=MYRPL
         MVC   MYRECORD,RECORD2
         PUT   RPL=MYRPL
*
         CLOSE (MYACB)
***********************************************************************
* Done
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
MYACB    ACB   AM=VSAM,                * Access method                 *
               DDNAME=MYESDS0,         *                               *
               MACRF=(SEQ,DFR,OUT),    * Options for this ACB
*
MYRPL    RPL   ACB=MYACB,              *                               *
               AM=VSAM,                *                               *
               AREA=MYRECORD,          *                               *
               AREALEN=50,             *                               *
               OPTCD=(SEQ,SYN)         * Sequential, Synchronous
MYRECORD DC    CL50''                  * Record work area
*
RECORD1  DC    CL50'Record number 1'
RECORD2  DC    CL50'Just another record'
*
         END
