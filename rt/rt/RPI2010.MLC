         TITLE 'Test save, restore pz390.psw_ins_len in trace_psw()'
***********************************************************************
* Start create : 03-06-2017
* 1st delivery :
* Designer     : J Ganci
* Programmer   : J Ganci
* Purpose      : Test save, restore pz390.psw_ins_len in trace_psw()
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
RPI2010  CSECT ,
RPI2010  AMODE 31
RPI2010  RMODE 24
         B     RPI20100-RPI2010(,R15) Branch around header
         DC    AL1(RPI20100-*),C'RPI2010:&SYSDATC:&SYSTIME'
RPI20100 DS    0H
         STM   R14,R12,12(R13)         Save caller's registers
         LR    R12,R15                 R12 = base register
         USING RPI2010,R12             Establish addressability
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
         MVC   WAEYE,=CL8'RPI2010'     Set eyecatcher
*
***********************************************************************
*        Set program mask to known state then set ESPIE
***********************************************************************
*
         SR    R0,R0               Program mask bits all zero
         SPM   R0                  Set program mask to known state
*
*        Set ESPIE to trap fixed-point-overflow exceptions
*
         ESPIE SET,ESPIEEX,(8),PARAM=ESPARM    Set ESPIE
         ST    R1,ESTOKEN                      Save token for reset
*
***********************************************************************
*        AR that yields CC=3
***********************************************************************
*
*        Test 1a: AR; pos + pos = neg; break after L R4,ESCOMP
*
         SR    R0,R0               Initialize CC
         LM    R0,R1,TST01a        Initialize registers 0 and 1
         LA    R15,Ret1a           Return address
         ST    R15,ESRET@          Put in ESPIE exit parm list
         AFI   R15,1               Precede with 6 byte instruction
         AR    R0,R1               Fixed-point-overflow exception
         ST    R15,0               Follow with 4 byte instruction; 0C4
Ret1a    DS    0H
         L     R4,ESCOMP           Get completion code
*
*        Test 1b: AR; pos + pos = neg; Txx thru AFI; brk af L R4,ESCOMP
*
         SR    R0,R0               Initialize CC
         LM    R0,R1,TST01a        Initialize registers 0 and 1
         LA    R15,Ret1b           Return address
         ST    R15,ESRET@          Put in ESPIE exit parm list
         AFI   R15,1               Precede with 6 byte instruction
         AR    R0,R1               Fixed-point-overflow exception
         ST    R15,0               Follow with 4 byte instruction; 0C4
Ret1b    DS    0H
         L     R4,ESCOMP           Get completion code
*
*        Cancel ESPIE
*
         ESPIE RESET               Cancel ESPIE
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
         DS    0F
TST01a   DC    X'7fffffff',X'00000001'
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
         TITLE 'ESPIE exit'
         PUSH  USING               Save USING state
         DROP  ,                   End all addressability
*
ESPIEEX  DS    0H
         LR    R12,R15             R12 = base register
         USING ESPIEEX,R12         Establish addressability
*
         LR    R2,R1               R2 --> EPIE
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
***********************************************************************
*
* Notes:
*
* 1. EPIEINT is wrong for z390.
*    Contains wrong length code (has len code of previous instr).
*    Interruption code = X'C8'; should be X'08'.
*        It is set in mainframe EPIEPSW.
*
*    WARNING: When testing using a test script and issuing the t1
*             command, sz390 calls pz390.trace_psw() to print the
*             next instruction to be executed. pz390.trace_psw()
*             calls the appropriate pz390.ins_setup_xx() which
*             modifies psw_ins_len for the instruction being traced
*             but not yet executed.
*
*             This is not good! The current instruction being executed
*             has not yet finished. When it does finish, on the next
*             instruction fetch cycle the statement
*                 last_psw_ins_len = psw_ins_len;
*             results in last_psw_ins_len being set incorrectly.
*
*             This resulted in a problem with the test script when
*             it tried to validate the returned EPIEINT word. The
*             instruction length byte was X'02' (actually the correct
*             value) because of the preceding paragraph, so the script
*             command
*                 v 4r = x'000400c8'
*             failed since last_psw_ins_len was 2, not 4 as it should've
*             been.
*
*             RPI 2010 fixes this problem by modifying pz390.trace_psw()
*             to save and restore psw_ins_len. Note that
*             pz390.trace_psw() already saves and restores psw_loc since
*             it is also altered in all pz390.ins_setup_xx() routines.
*
*    Note that the fix provided by RPI 2010 results in the length code
*    of the previous instruction to ALWAYS be passed to an ESPIE exit!
*
* 2, Amode bit not set in z390 EPIEPSW field EPIENXT1!!!
*    It is set in mainframe EPIEPSW.
*
* The ESPIE problems mentioned here, among others, will be fixed in
* another RPI.
*
***********************************************************************
*
         BR    R14                 Return to operating system
*
         LTORG
*
         POP   USING               Restore USING state
*
         IHAEPIE ,                 DSECT for EPIE
*
         END
