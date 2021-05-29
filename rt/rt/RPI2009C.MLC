         TITLE 'Test LCR and LCGR Instructions'
***********************************************************************
* Start create : 02-27-2017
* 1st delivery :
* Designer     : J Ganci
* Programmer   : J Ganci
* Purpose      : Test LCR, LCGR
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
RPI2009C CSECT ,
RPI2009C AMODE 31
RPI2009C RMODE 31
         B     RPI20090-RPI2009C(,R15) Branch around header
         DC    AL1(RPI20090-*),C'RPI2009C:&SYSDATC:&SYSTIME'
RPI20090 DS    0H
         STM   R14,R12,12(R13)         Save caller's registers
         LR    R12,R15                 R12 = base register
         USING RPI2009C,R12            Establish addressability
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
         MVC   WAEYE,=CL8'RPI2009C'    Set eyecatcher
*
***********************************************************************
*
*        Part 1: Program mask = 0000; no fixed-point-overflow
*
***********************************************************************
*
         SR    R0,R0                Set program mask bits to zero in R0
         SPM   R0                   Set program mask to zero
*
         SGR   R2,R2                R2 used to fetch CC
*
***********************************************************************
*
* 13 = LCR - Load Complement
*
* operand1 is 32-bit signed value
* operand2 is 32-bit signed value
*
***********************************************************************
*
*        Test 1a: LCR; max neg; CC=3
*
         SR    R0,R0               Initialize CC
         LM    R0,R1,TST01a        Initialize registers 0 and 1
         LCR   R0,R1               Should set CC=3
         IPM   R2                  Fetch CC
*
*        Test 1b: LCR; zero; CC=0
*
         SR    R0,R0               Initialize CC
         TM    *,X'80'             Set CC=3
         LM    R0,R1,TST01b        Initialize registers 0 and 1
         LCR   R0,R1               Should set CC=0
         IPM   R2                  Fetch CC
*
*        Test 1c: LCR; neg; CC=1
*
         SR    R0,R0               Initialize CC
         LM    R0,R1,TST01c        Initialize registers 0 and 1
         LCR   R0,R1               Should set CC=1
         IPM   R2                  Fetch CC
*
*        Test 1d: LCR; pos; CC=2
*
         SR    R0,R0               Initialize CC
         LM    R0,R1,TST01d        Initialize registers 0 and 1
         LCR   R0,R1               Should set CC=2
         IPM   R2                  Fetch CC
***********************************************************************
*
* B903 = LCGR - Load Complement
*
* operand1 is 64-bit signed value
* operand2 is 64-bit signed value
*
***********************************************************************
*
*        Test 2a: LCR; max neg; CC=3
*
         SR    R0,R0               Initialize CC
         LMG   R0,R1,TST02a        Initialize registers 0 and 1
         LCGR  R0,R1               Should set CC=3
         IPM   R2                  Fetch CC
*
*        Test 2b: LCGR; zero; CC=0
*
         SR    R0,R0               Initialize CC
         TM    *,X'80'             Set CC=3
         LMG   R0,R1,TST02b        Initialize registers 0 and 1
         LCGR  R0,R1               Should set CC=0
         IPM   R2                  Fetch CC
*
*        Test 2c: LCGR; neg; CC=1
*
         SR    R0,R0               Initialize CC
         LMG   R0,R1,TST02c        Initialize registers 0 and 1
         LCGR  R0,R1               Should set CC=1
         IPM   R2                  Fetch CC
*
*        Test 2d: LCGR; pos; CC=2
*
         SR    R0,R0               Initialize CC
         LMG   R0,R1,TST02d        Initialize registers 0 and 1
         LCGR  R0,R1               Should set CC=2
         IPM   R2                  Fetch CC
*
***********************************************************************
*
*        Part 2: Program mask = 1000; fixed-point-overflow
*
***********************************************************************
*
         IILF  R0,X'08000000'       Program mask bits 1000; fix pt ovf
         SPM   R0                   Set program mask to 1000
*
         SGR   R2,R2                R2 used to fetch CC
*
*        Set ESPIE to trap fixed-point overflow
*
         ESPIE SET,ESPIEEX,(8),PARAM=ESPARM    Set ESPIE
         ST    R1,ESTOKEN                      Save token for reset
*
         IIHF  R0,X'f4f4f4f4'      Reset R0 high half
         LGR   R1,R0               Reset R1 high half
*
***********************************************************************
*
* 13 = LCR - Load Complement
*
* operand1 is 32-bit signed value
* operand2 is 32-bit signed value
*
***********************************************************************
*
*        Test 3a: LCR; max neg; CC=3 and overflow
*
         SR    R0,R0               Initialize CC
         LM    R0,R1,TST01a        Initialize registers 0 and 1
         LA    R3,Ret03a           Return address
         ST    R3,ESRET@           Save for ESPIE exit
         LCR   R0,R1               Fixed-point-overflow exception (0C8)
         DC    H'0,1'              Should not get here
Ret03a   DS    0H
         IPM   R2                  Fetch CC
***********************************************************************
*
* B903 = LCGR - Load Complement
*
* operand1 is 64-bit signed value
* operand2 is 64-bit signed value
*
***********************************************************************
*
*        Test 4a: LCR; max neg; CC=3 and overflow
*
         SR    R0,R0               Initialize CC
         LMG   R0,R1,TST02a        Initialize registers 0 and 1
         LA    R3,Ret04a           Return address
         ST    R3,ESRET@           Save for ESPIE exit
         LCGR  R0,R1               Fixed-point-overflow exception (0C8)
         DC    H'0,2'              Should not get here
Ret04a   DS    0H
         IPM   R2                  Fetch CC
*
*        End of Part 2
*
         SR    R0,R0               Place to stop
*
         ESPIE RESET  ,ESTOKEN     Cancel ESPIE; token not implemented
*
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
ESPARM   DS    0A                  Parameter list for ESPIE exit
         DC    A(ESRET@)           A(Return address for ESPIE)
         DC    A(ESCOMP)           Completion code from ESPIE exit
         DC    A(ESPSW)            PSW from ESPIE exit
         DS    0D
ESRET@   DC    A(0)                Return address for ESPIE exit
ESCOMP   DC    F'0'                Completion code from ESPIE exit
ESPSW    DC    AD(0)               PSW from ESPIE exit
*
ESTOKEN  DC    F'0'                ESPIE token
*
         DS    0F
TST01a   DC    X'12345678',X'80000000'
TST01b   DC    X'12345678',X'00000000'
TST01c   DC    X'12345678',X'00000001'
TST01d   DC    X'12345678',X'ffffffff'
         DS    0D
TST02a   DC    x'12345678a1b2c3d4',x'8000000000000000'
TST02b   DC    x'12345678a1b2c3d4',x'0000000000000000'
TST02c   DC    x'12345678a1b2c3d4',x'0000000000000001'
TST02d   DC    x'12345678a1b2c3d4',x'ffffffffffffffff'
*
         TITLE 'ESPIE exit'
         PUSH  USING               Save USING state
         DROP  ,                   End all addressability
*
ESPIEEX  DS    0H
         LR    R12,R15             R12 = base register
         USING ESPIEEX,R12         Establish addressability
*
         USING EPIE,R1             Overlay EPIE
         L     R2,EPIEPARM         R2 --> user parameter list
         LM    R3,R5,0(R2)         R3 = A(return address)
*                                  R4 --> word for pgm interrupt info
*                                  R5 --> doubleword for PSW (8 bytes)
         L     R3,0(,R3)           R3 = return address
*
         MVC   0(4,R4),EPIEINT     Return program interrupt info
*
         LM    R8,R9,EPIEPSW       EC PSW at error
         STM   R8,R9,0(R5)         Return the EC PSW
*
         NILF  R9,X'80000000'      Isolate amode bit in next inst addr
*
         OR    R3,R9               Add amode bit to return address
         ST    R3,EPIENXT1         Set EC PSW next instruction address
*
         BR    R14                 Return to operating system
*
         LTORG
*
         POP   USING               Restore USING state
*
         TITLE 'DSECTs'
         IHAEPIE ,                 DSECT for EPIE
*
         END
