         TITLE 'Test Subtract Instructions'
***********************************************************************
* Start create : 03-09-2017
* 1st delivery :
* Designer     : J Ganci
* Programmer   : J Ganci
* Purpose      : Test Subtract instructions that can generate
*                fixed-point-overflow exceptions
***********************************************************************
* The subtract instructions that can generate fixed-point overflow:
*
* Mnemonic  Format
* S         RX-a
* SR        RR
* SRK       RRF-a
* SY        RXY-a
* SG        RXY-a
* SGR       RRE
* SGRK      RRF-a
* SGF       RXY-a
* SGFR      RRE
* SH        RX-a
* SHY       RXY-a
* SHHHR     RRF-a
* SHHLR     RRF-a
*
***********************************************************************
*
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
*        Test entry; for entries in test table
*
***********************************************************************
TESTENT  DSECT
TEMNE    DS    CL8                 Instruction mnemonic
TE@CC    DS    A                   Address of CC=3 test; no overflow
TE@OV    DS    A                   Address of overflow test
TELEN    EQU   *-TESTENT           Length of test entry
*
***********************************************************************
         EJECT ,
***********************************************************************
*
* There are two ways to run this program:
*
*     1. Execute it without providing a PARM value;
*     2. Execute it providing a PARM value.
*
* If no PARM value is entered, then all tests are run.
*
* If a PARM value is entered, it must be one of the Subtract
* instruction mnemonics. In that case, only that instruction is tested.
*
* Examples:
*
*   Run all tests:
*
*     "exec <path>/RPI2009A noloadhigh nocodepage test(RT)"
*
*   Run test set for SR:
*
*      "exec <path>/RPI2009B noloadhigh nocodepage parm(SR) test(RT)"
*
*   The test script "DD statement" environment variable RT must be
*   set to the appropriate test script.
*
***********************************************************************
*
* More instructions for running/modifying this program appear at the
* end of the source. Put there so that the statement numbers in the
* assembled listing do not change if more comments are added. The
* test scripts have statement numbers as comments. It is important to
* keep them in sync with the actual statement numbers.
*
***********************************************************************
         EJECT ,
RPI2009B CSECT
RPI2009B AMODE 31
RPI2009B RMODE 31
         B     RPI20020-RPI2009B(,R15) Branch around header
         DC    AL1(RPI20020-*),C'RPI2009B:&SYSDATC:&SYSTIME'
RPI20020 DS    0H
         STM   R14,R12,12(R13)         Save caller's registers
         LR    R12,R15                 R12 = base register
         USING RPI2009B,R12            Establish addressability
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
         MVC   WAEYE,=CL8'RPI2009B'    Set eyecatcher
*
         LARL  R11,STOR                R11 --> common storage
         USING STOR,R11                Overlay common storage
*
**********************************************************************
*        Initialize return code and test entry table work registers
**********************************************************************
         SR    R0,R0               Initialize return code
         ST    R0,RetCode          ... to zero
*
         SGR   R7,R7               Initialize current test pointer
         SGR   R8,R8               Initialize test increment
         SGR   R9,R9               Initialize ast test pointer
**********************************************************************
*        Get EXEC PARM value; test name
*        If no name, do all tests
*        If name, find test entry for name; test it
*        If name not found, issue error message and exit
**********************************************************************
         L     R2,0(,R1)           R2 --> parm; 1st 2 bytes = length
         LH    R3,0(,R2)           R3 = length of value
         LTR   R3,R3               Any parameter?
         BNP   AllTests            No; do all tests
         CHI   R3,L'TestMne        Too big?
         BH    InvMne              Yes; issue message and exit
         MVI   TestMne,C' '        Initialize field for test name
         MVC   TestMne+1(L'TestMne-1),TestMne  ... to spaces
         LA    R1,TestMne          R1 --> destination
         BCTR  R3,0                Convert length to length code
         EX    R3,CopyValue        Copy test instruction mnemonic
         OC    TestMne,Spaces      Convert to upper case
***********************************************************************
*        Find test entry for mnemonic
***********************************************************************
         LM    R7,R9,Tests         R7 --> 1st, R8 = len 1, R9 --> last
FindTest DS    0H
         CLC   TestMne,0(R7)       Found test entry for mnemonic?
         BE    FoundTst            Yes; exit loop
         BXLE  R7,R8,FindTest      No; continue search
         B     InvMne              Invalid mnemonic; error
FoundTst DS    0H
***********************************************************************
*        Do one test (two parts)
***********************************************************************
*
         USING TESTENT,R7          Overlay test entry for both parts
*
*        Initialize register used by tests to fetch CC
*
         SGR   R3,R3               Initialize R3 for test
*
*        Part 1: CC=3 no overflow
*
         SR    R0,R0               Program mask bits 0000
         SPM   R0                  Set program mask
*
         MVC   W1Mne,TEMNE         Copy instruction mnemonic
         WTO   MF=(E,WTO1)         Issue WTO
         LT    R15,TE@CC           Test routine
         BNP   Skip1CC             No routine; skip
         BASR  R14,R15             Do the test
Skip1CC  DS    0H
*
*        Part 2: CC=3 overflow
*
         MVC   W2Mne,0(R7)         Copy instruction mnemonic
         WTO   MF=(E,WTO2)         Issue WTO
         LT    R15,TE@OV           Test routine
         BNP   Skip1OV             No routine; skip
         LR    R10,R15             Save R15
*
         IILF  R0,x'08000000'      Program mask bits 1000
         SPM   R0                  Set program mask
*
         LARL  R2,ESPIEEX                  ESPIE exit routine
         ESPIE SET,(R2),(8),PARAM=ESPARM   Set ESPIE
         ST    R1,ESTOKEN                  Save token for reset
*
         LR    R15,R10             Restore R15
         BASR  R14,R15             Do the test
*
         ESPIE RESET  ,TOKEN=ESTOKEN
*
         SR    R0,R0               Program mask bits 0000
         SPM   R0                  Reset program mask
Skip1OV  DS    0H
*
         DROP  R7                  End test entry overlay
*
         B     EXIT                All done; exit
***********************************************************************
*        Do all tests (do all first parts then do all second parts)
***********************************************************************
AllTests DS    0H
*
*        Initialize register used by tests to fetch CC
*
         SGR   R3,R3               Initialize R3 for tests
*
         USING TESTENT,R7          Overlay test entry for both parts
*
*        Part 1: CC=3 no overflow
*
         SR    R0,R0               Program mask bits 0000
         SPM   R0                  Set program mask
*
         LM    R7,R9,Tests         R7 --> 1st, R8 = len 1, R9 --> last
LoopACC  DS    0H
         MVC   W1Mne,TEMNE         Copy instruction mnemonic
         WTO   MF=(E,WTO1)         Issue WTO
         LT    R15,TE@CC           Test routine
         BNP   SkipACC             No routine; skip
         BASR  R14,R15             Do the test
SkipACC  DS    0H
         BXLE  R7,R8,LoopACC       Do all tests
*
*        Part 2: CC=3 overflow
*
         IILF  R0,x'08000000'      Program mask bits 1000
         SPM   R0                  Set program mask
*
         LARL  R2,ESPIEEX                  ESPIE exit routine
         ESPIE SET,(R2),(8),PARAM=ESPARM   Set ESPIE
         ST    R1,ESTOKEN                  Save token for reset
*
         LM    R7,R9,Tests         R7 --> 1st, R8 = len 1, R9 --> last
LoopAOV  DS    0H
         MVC   W2Mne,TEMNE         Copy instruction mnemonic
         WTO   MF=(E,WTO2)         Issue WTO
         LT    R15,TE@OV          Test routine
         BNP   SkipAOV             No routine; skip
         BASR  R14,R15             Do the test
SkipAOV  DS    0H
         BXLE  R7,R8,LoopAOV       Do all tests
*
         ESPIE RESET  ,TOKEN=ESTOKEN
*
         SR    R0,R0               Program mask bits 0000
         SPM   R0                  Reset program mask
*
         DROP  R7                  End test entry overlay
*
         B     EXIT                Exit
***********************************************************************
*        Invalid test mnemonic; issue error message
***********************************************************************
InvMne   DS    0H
         WTO   'Error: invalid mnemonic value for PARM value'
         LA    R15,4               Return code value
         ST    R15,RetCode         Save it
*NSI     B     EXIT                Exit
***********************************************************************
*        Exit with return code
***********************************************************************
EXIT     DS    0H
         L     R2,RetCode          R2 = return code
         LR    R1,R13              R1 --> work area
         L     R13,4(,R13)         R13 --> Caller's save area
         LA    R0,WALEN            Length of work area
         FREEMAIN R,LV=(R0),A=(R1) Release work area storage
         LR    R15,R2              R15 = return code
         L     R14,12(R13)         Restore all registers
         LM    R0,R12,20(R13)      Except R15
         BR    R14                 Return to caller
*
         LTORG
*
CopyValue MVC   0(*-*,R1),2(R2)     Copy test name
*
         DROP  R12                 End main program addressability
         TITLE 'Test routines - two for each instruction'
***********************************************************************
*
* Registers on entry to each test routine:
*
* R3   Work register preset to extract CC and program mask
* R11  Address of STOR with active USING STOR,R11
* R14  Return address
* R15  Address of test routine
*
* Do not use R7, R8, R9, R12, or R13
*
* Register usage by the test routines; not restored
*
* R0   Work register
* R1   Work register
* R2   Work register
* R3   Contains CC and program mask
* R4   Work register
*
***********************************************************************
*
* There are two sets of tests for the instructions.
*
* The first set tests all possible CC values (0, 1, 2, 3).
* No fixed-point-overflows when CC=3 (the program mask is 0000).
*
* The second set tests only CC=3 with fixed-point overflows.
* That is, the program mask is 1000 and each instruction execution
* results in a fixed-point overflow. 
*
***********************************************************************
*
*
*
***********************************************************************
*
* Begin Part 1 test set: test CC=0,1,2,3; no fixed-point-overflow
*
***********************************************************************
***********************************************************************
*        S instruction test
***********************************************************************
*
* 5B = S - Subtract
*
* operand1 is 32-bit signed value
* operand2 is 32-bit signed value
*
***********************************************************************
*
TS1      DS    0H
         USING *,R15               Establish addressability
***********************************************************************
*
*        Test 1.1.0.1: zero - zero = zero; CC=0
*
         SR    R0,R0               Set CC
         TM    *,X'80'             Set CC=3
         LMG   R0,R1,TST_SR_ZZZ0   Initialize registers 0 and 1
         ST    R1,FW               Value to subtract
         S     R0,FW               CC=0
         IPM   R3                  Fetch CC
*
*        Test 1.1.0.2: pos - pos = zero; CC=0
*
         SR    R0,R0               Set CC
         TM    *,X'80'             Set CC=3
         LMG   R0,R1,TST_SR_PPZ0   Initialize registers 0 and 1
         ST    R1,FW               Value to subtract
         S     R0,FW               CC=0
         IPM   R3                  Fetch CC
*
*        Test 1.1.0.3: neg - neg = zero; CC=0
*
         SR    R0,R0               Set CC
         TM    *,X'80'             Set CC=3
         LMG   R0,R1,TST_SR_NNZ0   Initialize registers 0 and 1
         ST    R1,FW               Value to subtract
         S     R0,FW               CC=0
         IPM   R3                  Fetch CC
*
*        Test 1.1.0.4: max neg - max neg = zero; CC=0
*
         SR    R0,R0               Set CC
         TM    *,X'80'             Set CC=3
         LMG   R0,R1,TST_SR_NNZ0_M Initialize registers 0 and 1
         ST    R1,FW               Value to subtract
         S     R0,FW               CC=0
         IPM   R3                  Fetch CC
***********************************************************************
*
*        Test 1.1.1.1: zero - pos = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SR_ZPN1   Initialize registers 0 and 1
         ST    R1,FW               Value to subtract
         S     R0,FW               CC=1
         IPM   R3                  Fetch CC
*
*        Test 1.1.1.2: pos - pos = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SR_PPN1   Initialize registers 0 and 1
         ST    R1,FW               Value to subtract
         S     R0,FW               CC=1
         IPM   R3                  Fetch CC
*
*        Test 1.1.1.3: neg - zero = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SR_NZN1   Initialize registers 0 and 1
         ST    R1,FW               Value to subtract
         S     R0,FW               CC=1
         IPM   R3                  Fetch CC
*
*        Test 1.1.1.4: neg - pos = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SR_NPN1   Initialize registers 0 and 1
         ST    R1,FW               Value to subtract
         S     R0,FW               CC=1
         IPM   R3                  Fetch CC
*
*        Test 1.1.1.5: neg - neg = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SR_NNN1   Initialize registers 0 and 1
         ST    R1,FW               Value to subtract
         S     R0,FW               CC=1
         IPM   R3                  Fetch CC
***********************************************************************
*
*        Test 1.1.2.1: zero - neg = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SR_ZNP2   Initialize registers 0 and 1
         ST    R1,FW               Value to subtract
         S     R0,FW               CC=2
         IPM   R3                  Fetch CC
*
*        Test 1.1.2.2: pos - zero = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SR_PZP2   Initialize registers 0 and 1
         ST    R1,FW               Value to subtract
         S     R0,FW               CC=2
         IPM   R3                  Fetch CC
*
*        Test 1.1.2.3: pos - pos = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SR_PPP2   Initialize registers 0 and 1
         ST    R1,FW               Value to subtract
         S     R0,FW               CC=2
         IPM   R3                  Fetch CC
*
*        Test 1.1.2.4: pos - neg = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SR_PNP2   Initialize registers 0 and 1
         ST    R1,FW               Value to subtract
         S     R0,FW               CC=2
         IPM   R3                  Fetch CC
*
*        Test 1.1.2.5: neg - neg = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SR_NNP2   Initialize registers 0 and 1
         ST    R1,FW               Value to subtract
         S     R0,FW               CC=2
         IPM   R3                  Fetch CC
***********************************************************************
*
*        Test 1.1.3.1: zero - neg = neg; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SR_ZNN3   Initialize registers 0 and 1
         ST    R1,FW               Value to subtract
         S     R0,FW               CC=3
         IPM   R3                  Fetch CC
*
*        Test 1.1.3.2: pos - neg = neg; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SR_PNN3   Initialize registers 0 and 1
         ST    R1,FW               Value to subtract
         S     R0,FW               CC=3
         IPM   R3                  Fetch CC
*
*        Test 1.1.3.3: neg - pos = pos; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SR_NPP3   Initialize registers 0 and 1
         ST    R1,FW               Value to subtract
         S     R0,FW               CC=3
         IPM   R3                  Fetch CC
***********************************************************************
*
*        End test
*
         BR    R14                 Return to caller
*
         DROP  R15                 End addressability
*
***********************************************************************
*        SR instruction test
***********************************************************************
*
* 1B = SR - Subtract
*
* operand1 is 32-bit signed value
* operand2 is 32-bit signed value
*
***********************************************************************
TSR1     DS    0H
         USING *,R15               Establish addressability
***********************************************************************
*
*        Test 1.2.0.1: zero - zero = zero; CC=0
*
         SR    R0,R0               Set CC
         TM    *,X'80'             Set CC=3
         LMG   R0,R1,TST_SR_ZZZ0   Initialize registers 0 and 1
         SR    R0,R1               CC=0
         IPM   R3                  Fetch CC
*
*        Test 1.2.0.2: pos - pos = zero; CC=0
*
         SR    R0,R0               Set CC
         TM    *,X'80'             Set CC=3
         LMG   R0,R1,TST_SR_PPZ0   Initialize registers 0 and 1
         SR    R0,R1               CC=0
         IPM   R3                  Fetch CC
*
*        Test 1.2.0.3: neg - neg = zero; CC=0
*
         SR    R0,R0               Set CC
         TM    *,X'80'             Set CC=3
         LMG   R0,R1,TST_SR_NNZ0   Initialize registers 0 and 1
         SR    R0,R1               CC=0
         IPM   R3                  Fetch CC
*
*        Test 1.2.0.4: max neg - max neg = zero; CC=0
*
         SR    R0,R0               Set CC
         TM    *,X'80'             Set CC=3
         LMG   R0,R1,TST_SR_NNZ0_M Initialize registers 0 and 1
         SR    R0,R1               CC=0
         IPM   R3                  Fetch CC
***********************************************************************
*
*        Test 1.2.1.1: zero - pos = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SR_ZPN1   Initialize registers 0 and 1
         SR    R0,R1               CC=1
         IPM   R3                  Fetch CC
*
*        Test 1.2.1.2: pos - pos = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SR_PPN1   Initialize registers 0 and 1
         SR    R0,R1               CC=1
         IPM   R3                  Fetch CC
*
*        Test 1.2.1.3: neg - zero = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SR_NZN1   Initialize registers 0 and 1
         SR    R0,R1               CC=1
         IPM   R3                  Fetch CC
*
*        Test 1.2.1.4: neg - pos = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SR_NPN1   Initialize registers 0 and 1
         SR    R0,R1               CC=1
         IPM   R3                  Fetch CC
*
*        Test 1.2.1.5: neg - neg = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SR_NNN1   Initialize registers 0 and 1
         SR    R0,R1               CC=1
         IPM   R3                  Fetch CC
***********************************************************************
*
*        Test 1.2.2.1: zero - neg = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SR_ZNP2   Initialize registers 0 and 1
         SR    R0,R1               CC=2
         IPM   R3                  Fetch CC
*
*        Test 1.2.2.2: pos - zero = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SR_PZP2   Initialize registers 0 and 1
         SR    R0,R1               CC=2
         IPM   R3                  Fetch CC
*
*        Test 1.2.2.3: pos - pos = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SR_PPP2   Initialize registers 0 and 1
         SR    R0,R1               CC=2
         IPM   R3                  Fetch CC
*
*        Test 1.2.2.4: pos - neg = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SR_PNP2   Initialize registers 0 and 1
         SR    R0,R1               CC=2
         IPM   R3                  Fetch CC
*
*        Test 1.2.2.5: neg - neg = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SR_NNP2   Initialize registers 0 and 1
         SR    R0,R1               CC=2
         IPM   R3                  Fetch CC
***********************************************************************
*
*        Test 1.2.3.1: zero - neg = neg; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SR_ZNN3   Initialize registers 0 and 1
         SR    R0,R1               CC=3
         IPM   R3                  Fetch CC
*
*        Test 1.2.3.2: pos - neg = neg; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SR_PNN3   Initialize registers 0 and 1
         SR    R0,R1               CC=3
         IPM   R3                  Fetch CC
*
*        Test 1.2.3.3: neg - pos = pos; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SR_NPP3   Initialize registers 0 and 1
         SR    R0,R1               CC=3
         IPM   R3                  Fetch CC
***********************************************************************
*
*        End test
*
         BR    R14                 Return to caller
*
         DROP  R15                 End addressability
*
***********************************************************************
*        SRK instruction test
***********************************************************************
*
* B9F9 = SRK - Subtract
*
* operand1 is 32-bit signed value
* operand2 is 32-bit signed value
* operand3 is 32-bit signed value
*
***********************************************************************
*
TSRK1    DS    0H
         USING *,R15               Establish addressability
***********************************************************************
*
*        Test 1.3.0.1: zero - zero = zero; CC=0
*
         SR    R0,R0               Set CC
         TM    *,X'80'             Set CC=3
         LMG   R0,R1,TST_SR_ZZZ0   Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         SRK   R2,R0,R1            CC=0
         IPM   R3                  Fetch CC
*
*        Test 1.3.0.2: pos - pos = zero; CC=0
*
         SR    R0,R0               Set CC
         TM    *,X'80'             Set CC=3
         LMG   R0,R1,TST_SR_PPZ0   Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         SRK   R2,R0,R1            CC=0
         IPM   R3                  Fetch CC
*
*        Test 1.3.0.3: neg - neg = zero; CC=0
*
         SR    R0,R0               Set CC
         TM    *,X'80'             Set CC=3
         LMG   R0,R1,TST_SR_NNZ0   Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         SRK   R2,R0,R1            CC=0
         IPM   R3                  Fetch CC
*
*        Test 1.3.0.4: max neg - max neg = zero; CC=0
*
         SR    R0,R0               Set CC
         TM    *,X'80'             Set CC=3
         LMG   R0,R1,TST_SR_NNZ0_M Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         SRK   R2,R0,R1            CC=0
         IPM   R3                  Fetch CC
***********************************************************************
*
*        Test 1.3.1.1: zero - pos = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SR_ZPN1   Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         SRK   R2,R0,R1            CC=1
         IPM   R3                  Fetch CC
*
*        Test 1.3.1.2: pos - pos = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SR_PPN1   Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         SRK   R2,R0,R1            CC=1
         IPM   R3                  Fetch CC
*
*        Test 1.3.1.3: neg - zero = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SR_NZN1   Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         SRK   R2,R0,R1            CC=1
         IPM   R3                  Fetch CC
*
*        Test 1.3.1.4: neg - pos = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SR_NPN1   Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         SRK   R2,R0,R1            CC=1
         IPM   R3                  Fetch CC
*
*        Test 1.3.1.5: neg - neg = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SR_NNN1   Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         SRK   R2,R0,R1            CC=1
         IPM   R3                  Fetch CC
***********************************************************************
*
*        Test 1.3.2.1: zero - neg = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SR_ZNP2   Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         SRK   R2,R0,R1            CC=2
         IPM   R3                  Fetch CC
*
*        Test 1.3.2.2: pos - zero = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SR_PZP2   Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         SRK   R2,R0,R1            CC=2
         IPM   R3                  Fetch CC
*
*        Test 1.3.2.3: pos - pos = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SR_PPP2   Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         SRK   R2,R0,R1            CC=2
         IPM   R3                  Fetch CC
*
*        Test 1.3.2.4: pos - neg = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SR_PNP2   Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         SRK   R2,R0,R1            CC=2
         IPM   R3                  Fetch CC
*
*        Test 1.3.2.5: neg - neg = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SR_NNP2   Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         SRK   R2,R0,R1            CC=2
         IPM   R3                  Fetch CC
***********************************************************************
*
*        Test 1.3.3.1: zero - neg = neg; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SR_ZNN3   Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         SRK   R2,R0,R1            CC=3
         IPM   R3                  Fetch CC
*
*        Test 1.3.3.2: pos - neg = neg; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SR_PNN3   Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         SRK   R2,R0,R1            CC=3
         IPM   R3                  Fetch CC
*
*        Test 1.3.3.3: neg - pos = pos; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SR_NPP3   Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         SRK   R2,R0,R1            CC=3
         IPM   R3                  Fetch CC
***********************************************************************
*
*        End test
*
         BR    R14                 Return to caller
*
         DROP  R15                 End addressability
*
***********************************************************************
*        SY instruction test
***********************************************************************
*
* E35B = SY - Subtract
*
* operand1 is 32-bit signed value
* operand2 is 32-bit signed value
*
***********************************************************************
*
TSY1     DS    0H
         USING *,R15               Establish addressability
***********************************************************************
*
*        Test 1.4.0.1: zero - zero = zero; CC=0
*
         SR    R0,R0               Set CC
         TM    *,X'80'             Set CC=3
         LMG   R0,R1,TST_SR_ZZZ0   Initialize registers 0 and 1
         ST    R1,FW               Value to subtract
         SY    R0,FW               CC=0
         IPM   R3                  Fetch CC
*
*        Test 1.4.0.2: pos - pos = zero; CC=0
*
         SR    R0,R0               Set CC
         TM    *,X'80'             Set CC=3
         LMG   R0,R1,TST_SR_PPZ0   Initialize registers 0 and 1
         ST    R1,FW               Value to subtract
         SY    R0,FW               CC=0
         IPM   R3                  Fetch CC
*
*        Test 1.4.0.3: neg - neg = zero; CC=0
*
         SR    R0,R0               Set CC
         TM    *,X'80'             Set CC=3
         LMG   R0,R1,TST_SR_NNZ0   Initialize registers 0 and 1
         ST    R1,FW               Value to subtract
         SY    R0,FW               CC=0
         IPM   R3                  Fetch CC
*
*        Test 1.4.0.4: max neg - max neg = zero; CC=0
*
         SR    R0,R0               Set CC
         TM    *,X'80'             Set CC=3
         LMG   R0,R1,TST_SR_NNZ0_M Initialize registers 0 and 1
         ST    R1,FW               Value to subtract
         SY    R0,FW               CC=0
         IPM   R3                  Fetch CC
***********************************************************************
*
*        Test 1.4.1.1: zero - pos = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SR_ZPN1   Initialize registers 0 and 1
         ST    R1,FW               Value to subtract
         SY    R0,FW               CC=1
         IPM   R3                  Fetch CC
*
*        Test 1.4.1.2: pos - pos = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SR_PPN1   Initialize registers 0 and 1
         ST    R1,FW               Value to subtract
         SY    R0,FW               CC=1
         IPM   R3                  Fetch CC
*
*        Test 1.4.1.3: neg - zero = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SR_NZN1   Initialize registers 0 and 1
         ST    R1,FW               Value to subtract
         SY    R0,FW               CC=1
         IPM   R3                  Fetch CC
*
*        Test 1.4.1.4: neg - pos = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SR_NPN1   Initialize registers 0 and 1
         ST    R1,FW               Value to subtract
         SY    R0,FW               CC=1
         IPM   R3                  Fetch CC
*
*        Test 1.4.1.5: neg - neg = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SR_NNN1   Initialize registers 0 and 1
         ST    R1,FW               Value to subtract
         SY    R0,FW               CC=1
         IPM   R3                  Fetch CC
***********************************************************************
*
*        Test 1.4.2.1: zero - neg = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SR_ZNP2   Initialize registers 0 and 1
         ST    R1,FW               Value to subtract
         SY    R0,FW               CC=2
         IPM   R3                  Fetch CC
*
*        Test 1.4.2.2: pos - zero = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SR_PZP2   Initialize registers 0 and 1
         ST    R1,FW               Value to subtract
         SY    R0,FW               CC=2
         IPM   R3                  Fetch CC
*
*        Test 1.4.2.3: pos - pos = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SR_PPP2   Initialize registers 0 and 1
         ST    R1,FW               Value to subtract
         SY    R0,FW               CC=2
         IPM   R3                  Fetch CC
*
*        Test 1.4.2.4: pos - neg = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SR_PNP2   Initialize registers 0 and 1
         ST    R1,FW               Value to subtract
         SY    R0,FW               CC=2
         IPM   R3                  Fetch CC
*
*        Test 1.4.2.5: neg - neg = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SR_NNP2   Initialize registers 0 and 1
         ST    R1,FW               Value to subtract
         SY    R0,FW               CC=2
         IPM   R3                  Fetch CC
***********************************************************************
*
*        Test 1.4.3.1: zero - neg = neg; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SR_ZNN3   Initialize registers 0 and 1
         ST    R1,FW               Value to subtract
         SY    R0,FW               CC=3
         IPM   R3                  Fetch CC
*
*        Test 1.4.3.2: pos - neg = neg; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SR_PNN3   Initialize registers 0 and 1
         ST    R1,FW               Value to subtract
         SY    R0,FW               CC=3
         IPM   R3                  Fetch CC
*
*        Test 1.4.3.3: neg - pos = pos; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SR_NPP3   Initialize registers 0 and 1
         ST    R1,FW               Value to subtract
         SY    R0,FW               CC=3
         IPM   R3                  Fetch CC
***********************************************************************
*
*        End test
*
         BR    R14                 Return to caller
*
         DROP  R15                 End addressability
*
***********************************************************************
*        SG instruction test
***********************************************************************
*
* E309 = SG - Subtract
*
* operand1 is 64-bit signed value
* operand2 is 64-bit signed value
*
***********************************************************************
*
TSG1     DS    0H
         USING *,R15               Establish addressability
***********************************************************************
*
*        Test 1.5.0.1: zero - zero = zero; CC=0
*
         SR    R0,R0               Set CC
         TM    *,X'80'             Set CC=3
         LMG   R0,R1,TST_SGR_ZZZ0  Initialize registers 0 and 1
         STG   R1,DW               Value to subtract
         SG    R0,DW               CC=0
         IPM   R3                  Fetch CC
*
*        Test 1.5.0.2: pos - pos = zero; CC=0
*
         SR    R0,R0               Set CC
         TM    *,X'80'             Set CC=3
         LMG   R0,R1,TST_SGR_PPZ0  Initialize registers 0 and 1
         STG   R1,DW               Value to subtract
         SG    R0,DW               CC=0
         IPM   R3                  Fetch CC
*
*        Test 1.5.0.3: neg - neg = zero; CC=0
*
         SR    R0,R0               Set CC
         TM    *,X'80'             Set CC=3
         LMG   R0,R1,TST_SGR_NNZ0  Initialize registers 0 and 1
         STG   R1,DW               Value to subtract
         SG    R0,DW               CC=0
         IPM   R3                  Fetch CC
*
*        Test 1.5.0.4: max neg - max neg = zero; CC=0
*
         SR    R0,R0               Set CC
         TM    *,X'80'             Set CC=3
         LMG   R0,R1,TST_SGR_NNZ0_M Initialize registers 0 and 1
         STG   R1,DW               Value to subtract
         SG    R0,DW               CC=0
         IPM   R3                  Fetch CC
***********************************************************************
*
*        Test 1.5.1.1: zero - pos = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SGR_ZPN1  Initialize registers 0 and 1
         STG   R1,DW               Value to subtract
         SG    R0,DW               CC=1
         IPM   R3                  Fetch CC
*
*        Test 1.5.1.2: pos - pos = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SGR_PPN1  Initialize registers 0 and 1
         STG   R1,DW               Value to subtract
         SG    R0,DW               CC=1
         IPM   R3                  Fetch CC
*
*        Test 1.5.1.3: neg - zero = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SGR_NZN1  Initialize registers 0 and 1
         STG   R1,DW               Value to subtract
         SG    R0,DW               CC=1
         IPM   R3                  Fetch CC
*
*        Test 1.5.1.4: neg - pos = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SGR_NPN1  Initialize registers 0 and 1
         STG   R1,DW               Value to subtract
         SG    R0,DW               CC=1
         IPM   R3                  Fetch CC
*
*        Test 1.5.1.5: neg - neg = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SGR_NNN1  Initialize registers 0 and 1
         STG   R1,DW               Value to subtract
         SG    R0,DW               CC=1
         IPM   R3                  Fetch CC
***********************************************************************
*
*        Test 1.5.2.1: zero - neg = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SGR_ZNP2  Initialize registers 0 and 1
         STG   R1,DW               Value to subtract
         SG    R0,DW               CC=2
         IPM   R3                  Fetch CC
*
*        Test 1.5.2.2: pos - zero = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SGR_PZP2  Initialize registers 0 and 1
         STG   R1,DW               Value to subtract
         SG    R0,DW               CC=2
         IPM   R3                  Fetch CC
*
*        Test 1.5.2.3: pos - pos = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SGR_PPP2  Initialize registers 0 and 1
         STG   R1,DW               Value to subtract
         SG    R0,DW               CC=2
         IPM   R3                  Fetch CC
*
*        Test 1.5.2.4: pos - neg = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SGR_PNP2  Initialize registers 0 and 1
         STG   R1,DW               Value to subtract
         SG    R0,DW               CC=2
         IPM   R3                  Fetch CC
*
*        Test 1.5.2.5: neg - neg = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SGR_NNP2  Initialize registers 0 and 1
         STG   R1,DW               Value to subtract
         SG    R0,DW               CC=2
         IPM   R3                  Fetch CC
***********************************************************************
*
*        Test 1.5.3.1: zero - neg = neg; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SGR_ZNN3  Initialize registers 0 and 1
         STG   R1,DW               Value to subtract
         SG    R0,DW               CC=3
         IPM   R3                  Fetch CC
*
*        Test 1.5.3.2: pos - neg = neg; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SGR_PNN3  Initialize registers 0 and 1
         STG   R1,DW               Value to subtract
         SG    R0,DW               CC=3
         IPM   R3                  Fetch CC
*
*        Test 1.5.3.3: neg - pos = pos; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SGR_NPP3  Initialize registers 0 and 1
         STG   R1,DW               Value to subtract
         SG    R0,DW               CC=3
         IPM   R3                  Fetch CC
***********************************************************************
*
*        End test
*
         BR    R14                 Return to caller
*
         DROP  R15                 End addressability
*
***********************************************************************
*        SGR instruction test
***********************************************************************
*
* B909 = SGR - Subtract
*
* operand1 is 64-bit signed value
* operand2 is 64-bit signed value
*
***********************************************************************
*
TSGR1    DS    0H
         USING *,R15               Establish addressability
***********************************************************************
*
*        Test 1.6.0.1: zero - zero = zero; CC=0
*
         SR    R0,R0               Set CC
         TM    *,X'80'             Set CC=3
         LMG   R0,R1,TST_SGR_ZZZ0  Initialize registers 0 and 1
         SGR   R0,R1               CC=0
         IPM   R3                  Fetch CC
*
*        Test 1.6.0.2: pos - pos = zero; CC=0
*
         SR    R0,R0               Set CC
         TM    *,X'80'             Set CC=3
         LMG   R0,R1,TST_SGR_PPZ0  Initialize registers 0 and 1
         SGR   R0,R1               CC=0
         IPM   R3                  Fetch CC
*
*        Test 1.6.0.3: neg - neg = zero; CC=0
*
         SR    R0,R0               Set CC
         TM    *,X'80'             Set CC=3
         LMG   R0,R1,TST_SGR_NNZ0  Initialize registers 0 and 1
         SGR   R0,R1               CC=0
         IPM   R3                  Fetch CC
*
*        Test 1.6.0.4: max neg - max neg = zero; CC=0
*
         SR    R0,R0               Set CC
         TM    *,X'80'             Set CC=3
         LMG   R0,R1,TST_SGR_NNZ0_M Initialize registers 0 and 1
         SGR   R0,R1               CC=0
         IPM   R3                  Fetch CC
***********************************************************************
*
*        Test 1.6.1.1: zero - pos = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SGR_ZPN1  Initialize registers 0 and 1
         SGR   R0,R1               CC=1
         IPM   R3                  Fetch CC
*
*        Test 1.6.1.2: pos - pos = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SGR_PPN1  Initialize registers 0 and 1
         SGR   R0,R1               CC=1
         IPM   R3                  Fetch CC
*
*        Test 1.6.1.3: neg - zero = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SGR_NZN1  Initialize registers 0 and 1
         SGR   R0,R1               CC=1
         IPM   R3                  Fetch CC
*
*        Test 1.6.1.4: neg - pos = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SGR_NPN1  Initialize registers 0 and 1
         SGR   R0,R1               CC=1
         IPM   R3                  Fetch CC
*
*        Test 1.6.1.5: neg - neg = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SGR_NNN1  Initialize registers 0 and 1
         SGR   R0,R1               CC=1
         IPM   R3                  Fetch CC
***********************************************************************
*
*        Test 1.6.2.1: zero - neg = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SGR_ZNP2  Initialize registers 0 and 1
         SGR   R0,R1               CC=2
         IPM   R3                  Fetch CC
*
*        Test 1.6.2.2: pos - zero = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SGR_PZP2  Initialize registers 0 and 1
         SGR   R0,R1               CC=2
         IPM   R3                  Fetch CC
*
*        Test 1.6.2.3: pos - pos = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SGR_PPP2  Initialize registers 0 and 1
         SGR   R0,R1               CC=2
         IPM   R3                  Fetch CC
*
*        Test 1.6.2.4: pos - neg = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SGR_PNP2  Initialize registers 0 and 1
         SGR   R0,R1               CC=2
         IPM   R3                  Fetch CC
*
*        Test 1.6.2.5: neg - neg = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SGR_NNP2  Initialize registers 0 and 1
         SGR   R0,R1               CC=2
         IPM   R3                  Fetch CC
***********************************************************************
*
*        Test 1.6.3.1: zero - neg = neg; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SGR_ZNN3  Initialize registers 0 and 1
         SGR   R0,R1               CC=3
         IPM   R3                  Fetch CC
*
*        Test 1.6.3.2: pos - neg = neg; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SGR_PNN3  Initialize registers 0 and 1
         SGR   R0,R1               CC=3
         IPM   R3                  Fetch CC
*
*        Test 1.6.3.3: neg - pos = pos; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SGR_NPP3  Initialize registers 0 and 1
         SGR   R0,R1               CC=3
         IPM   R3                  Fetch CC
***********************************************************************
*
*        End test
*
         BR    R14                 Return to caller
*
         DROP  R15                 End addressability
*
***********************************************************************
*        SGRK instruction test
***********************************************************************
*
* B9E9 = SGRK - Subtract
*
* operand1 is 64-bit signed value
* operand2 is 64-bit signed value
* operand3 is 64-bit signed value
*
***********************************************************************
*
TSGRK1   DS    0H
         USING *,R15               Establish addressability
***********************************************************************
*
*        Test 1.7.0.1: zero - zero = zero; CC=0
*
         SR    R0,R0               Set CC
         TM    *,X'80'             Set CC=3
         LMG   R0,R1,TST_SGR_ZZZ0  Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         SGRK  R2,R0,R1            CC=0
         IPM   R3                  Fetch CC
*
*        Test 1.7.0.2: pos - pos = zero; CC=0
*
         SR    R0,R0               Set CC
         TM    *,X'80'             Set CC=3
         LMG   R0,R1,TST_SGR_PPZ0  Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         SGRK  R2,R0,R1            CC=0
         IPM   R3                  Fetch CC
*
*        Test 1.7.0.3: neg - neg = zero; CC=0
*
         SR    R0,R0               Set CC
         TM    *,X'80'             Set CC=3
         LMG   R0,R1,TST_SGR_NNZ0  Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         SGRK  R2,R0,R1            CC=0
         IPM   R3                  Fetch CC
*
*        Test 1.7.0.4: max neg - max neg = zero; CC=0
*
         SR    R0,R0               Set CC
         TM    *,X'80'             Set CC=3
         LMG   R0,R1,TST_SGR_NNZ0_M Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         SGRK  R2,R0,R1            CC=0
         IPM   R3                  Fetch CC
***********************************************************************
*
*        Test 1.7.1.1: zero - pos = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SGR_ZPN1  Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         SGRK  R2,R0,R1            CC=1
         IPM   R3                  Fetch CC
*
*        Test 1.7.1.2: pos - pos = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SGR_PPN1  Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         SGRK  R2,R0,R1            CC=1
         IPM   R3                  Fetch CC
*
*        Test 1.7.1.3: neg - zero = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SGR_NZN1  Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         SGRK  R2,R0,R1            CC=1
         IPM   R3                  Fetch CC
*
*        Test 1.7.1.4: neg - pos = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SGR_NPN1  Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         SGRK  R2,R0,R1            CC=1
         IPM   R3                  Fetch CC
*
*        Test 1.7.1.5: neg - neg = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SGR_NNN1  Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         SGRK  R2,R0,R1            CC=1
         IPM   R3                  Fetch CC
***********************************************************************
*
*        Test 1.7.2.1: zero - neg = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SGR_ZNP2  Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         SGRK  R2,R0,R1            CC=2
         IPM   R3                  Fetch CC
*
*        Test 1.7.2.2: pos - zero = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SGR_PZP2  Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         SGRK  R2,R0,R1            CC=2
         IPM   R3                  Fetch CC
*
*        Test 1.7.2.3: pos - pos = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SGR_PPP2  Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         SGRK  R2,R0,R1            CC=2
         IPM   R3                  Fetch CC
*
*        Test 1.7.2.4: pos - neg = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SGR_PNP2  Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         SGRK  R2,R0,R1            CC=2
         IPM   R3                  Fetch CC
*
*        Test 1.7.2.5: neg - neg = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SGR_NNP2  Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         SGRK  R2,R0,R1            CC=2
         IPM   R3                  Fetch CC
***********************************************************************
*
*        Test 1.7.3.1: zero - neg = neg; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SGR_ZNN3  Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         SGRK  R2,R0,R1            CC=3
         IPM   R3                  Fetch CC
*
*        Test 1.7.3.2: pos - neg = neg; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SGR_PNN3  Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         SGRK  R2,R0,R1            CC=3
         IPM   R3                  Fetch CC
*
*        Test 1.7.3.3: neg - pos = pos; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SGR_NPP3  Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         SGRK  R2,R0,R1            CC=3
         IPM   R3                  Fetch CC
***********************************************************************
*
*        End test
*
         BR    R14                 Return to caller
*
         DROP  R15                 End addressability
*
***********************************************************************
*        SGF instruction test
***********************************************************************
*
* E319 = SGF - Subtract
*
* operand1 is 64-bit signed value
* operand2 is 32-bit signed value
*
***********************************************************************
*
TSGF1    DS    0H
         USING *,R15               Establish addressability
***********************************************************************
*
*        Test 1.8.0.1: zero - zero = zero; CC=0
*
         SR    R0,R0               Set CC
         TM    *,X'80'             Set CC=3
         LMG   R0,R1,TST_SGR_ZZZ0  Initialize registers 0 and 1
         ST    R1,FW               Value to subtract
         SGF   R0,FW               CC=0
         IPM   R3                  Fetch CC
*
*        Test 1.8.0.2: pos - pos = zero; CC=0
*
         SR    R0,R0               Set CC
         TM    *,X'80'             Set CC=3
         LMG   R0,R1,TST_SGR_PPZ0  Initialize registers 0 and 1
         ST    R1,FW               Value to subtract
         SGF   R0,FW               CC=0
         IPM   R3                  Fetch CC
*
*        Test 1.8.0.3: neg - neg = zero; CC=0
*
         SR    R0,R0               Set CC
         TM    *,X'80'             Set CC=3
         LMG   R0,R1,TST_SGR_NNZ0  Initialize registers 0 and 1
         ST    R1,FW               Value to subtract
         SGF   R0,FW               CC=0
         IPM   R3                  Fetch CC
*
*        Test 1.8.0.4: max neg - max neg = zero; CC=0; can't occur
*
***      SR    R0,R0               Set CC
***      TM    *,X'80'             Set CC=3
***      LMG   R0,R1,TST_SGR_NNZ0_M Initialize registers 0 and 1
***      ST    R1,FW               Value to subtract
***      SGF   R0,FW               CC=0
***      IPM   R3                  Fetch CC
***********************************************************************
*
*        Test 1.8.1.1: zero - pos = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SGR_ZPN1  Initialize registers 0 and 1
         ST    R1,FW               Value to subtract
         SGF   R0,FW               CC=1
         IPM   R3                  Fetch CC
*
*        Test 1.8.1.2: pos - pos = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SGR_PPN1  Initialize registers 0 and 1
         ST    R1,FW               Value to subtract
         SGF   R0,FW               CC=1
         IPM   R3                  Fetch CC
*
*        Test 1.8.1.3: neg - zero = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SGR_NZN1  Initialize registers 0 and 1
         ST    R1,FW               Value to subtract
         SGF   R0,FW               CC=1
         IPM   R3                  Fetch CC
*
*        Test 1.8.1.4: neg - pos = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SGR_NPN1  Initialize registers 0 and 1
         ST    R1,FW               Value to subtract
         SGF   R0,FW               CC=1
         IPM   R3                  Fetch CC
*
*        Test 1.8.1.5: neg - neg = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SGR_NNN1  Initialize registers 0 and 1
         ST    R1,FW               Value to subtract
         SGF   R0,FW               CC=1
         IPM   R3                  Fetch CC
***********************************************************************
*
*        Test 1.8.2.1: zero - neg = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SGR_ZNP2  Initialize registers 0 and 1
         ST    R1,FW               Value to subtract
         SGF   R0,FW               CC=2
         IPM   R3                  Fetch CC
*
*        Test 1.8.2.2: pos - zero = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SGR_PZP2  Initialize registers 0 and 1
         ST    R1,FW               Value to subtract
         SGF   R0,FW               CC=2
         IPM   R3                  Fetch CC
*
*        Test 1.8.2.3: pos - pos = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SGR_PPP2  Initialize registers 0 and 1
         ST    R1,FW               Value to subtract
         SGF   R0,FW               CC=2
         IPM   R3                  Fetch CC
*
*        Test 1.8.2.4: pos - neg = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SGR_PNP2  Initialize registers 0 and 1
         ST    R1,FW               Value to subtract
         SGF   R0,FW               CC=2
         IPM   R3                  Fetch CC
*
*        Test 1.8.2.5: neg - neg = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SGR_NNP2  Initialize registers 0 and 1
         ST    R1,FW               Value to subtract
         SGF   R0,FW               CC=2
         IPM   R3                  Fetch CC
***********************************************************************
*
*        Test 1.8.3.1: zero - neg = neg; CC=3; can't occur
*
***      SR    R0,R0               Set CC
***      LMG   R0,R1,TST_SGR_ZNN3  Initialize registers 0 and 1
***      ST    R1,FW               Value to subtract
***      SGF   R0,FW               CC=3
***      IPM   R3                  Fetch CC
*
*        Test 1.8.3.2: pos - neg = neg; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SGR_PNN3  Initialize registers 0 and 1
         ST    R1,FW               Value to subtract
         SGF   R0,FW               CC=3
         IPM   R3                  Fetch CC
*
*        Test 1.8.3.3: neg - pos = pos; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SGR_NPP3  Initialize registers 0 and 1
         ST    R1,FW               Value to subtract
         SGF   R0,FW               CC=3
         IPM   R3                  Fetch CC
***********************************************************************
*
*        End test
*
         BR    R14                 Return to caller
*
         DROP  R15                 End addressability
*
***********************************************************************
*        SGFR instruction test
***********************************************************************
*
* B919 = SGFR - Subtract
*
* operand1 is 64-bit signed value
* operand2 is 32-bit signed value
*
***********************************************************************
*
TSGFR1   DS    0H
         USING *,R15               Establish addressability
***********************************************************************
*
*        Test 1.9.0.1: zero - zero = zero; CC=0
*
         SR    R0,R0               Set CC
         TM    *,X'80'             Set CC=3
         LMG   R0,R1,TST_SGR_ZZZ0  Initialize registers 0 and 1
         SGFR  R0,R1               CC=0
         IPM   R3                  Fetch CC
*
*        Test 1.9.0.2: pos - pos = zero; CC=0
*
         SR    R0,R0               Set CC
         TM    *,X'80'             Set CC=3
         LMG   R0,R1,TST_SGR_PPZ0  Initialize registers 0 and 1
         SGFR  R0,R1               CC=0
         IPM   R3                  Fetch CC
*
*        Test 1.9.0.3: neg - neg = zero; CC=0
*
         SR    R0,R0               Set CC
         TM    *,X'80'             Set CC=3
         LMG   R0,R1,TST_SGR_NNZ0  Initialize registers 0 and 1
         SGFR  R0,R1               CC=0
         IPM   R3                  Fetch CC
*
*        Test 1.9.0.4: max neg - max neg = zero; CC=0; can't occur
*
***      SR    R0,R0               Set CC
***      TM    *,X'80'             Set CC=3
***      LMG   R0,R1,TST_SGR_NNZ0_M Initialize registers 0 and 1
***      SGFR  R0,R1               CC=0
***      IPM   R3                  Fetch CC
***********************************************************************
*
*        Test 1.9.1.1: zero - pos = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SGR_ZPN1  Initialize registers 0 and 1
         SGFR  R0,R1               CC=1
         IPM   R3                  Fetch CC
*
*        Test 1.9.1.2: pos - pos = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SGR_PPN1  Initialize registers 0 and 1
         SGFR  R0,R1               CC=1
         IPM   R3                  Fetch CC
*
*        Test 1.9.1.3: neg - zero = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SGR_NZN1  Initialize registers 0 and 1
         SGFR  R0,R1               CC=1
         IPM   R3                  Fetch CC
*
*        Test 1.9.1.4: neg - pos = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SGR_NPN1  Initialize registers 0 and 1
         SGFR  R0,R1               CC=1
         IPM   R3                  Fetch CC
*
*        Test 1.9.1.5: neg - neg = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SGR_NNN1  Initialize registers 0 and 1
         SGFR  R0,R1               CC=1
         IPM   R3                  Fetch CC
***********************************************************************
*
*        Test 1.9.2.1: zero - neg = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SGR_ZNP2  Initialize registers 0 and 1
         SGFR  R0,R1               CC=2
         IPM   R3                  Fetch CC
*
*        Test 1.9.2.2: pos - zero = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SGR_PZP2  Initialize registers 0 and 1
         SGFR  R0,R1               CC=2
         IPM   R3                  Fetch CC
*
*        Test 1.9.2.3: pos - pos = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SGR_PPP2  Initialize registers 0 and 1
         SGFR  R0,R1               CC=2
         IPM   R3                  Fetch CC
*
*        Test 1.9.2.4: pos - neg = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SGR_PNP2  Initialize registers 0 and 1
         SGFR  R0,R1               CC=2
         IPM   R3                  Fetch CC
*
*        Test 1.9.2.5: neg - neg = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SGR_NNP2  Initialize registers 0 and 1
         SGFR  R0,R1               CC=2
         IPM   R3                  Fetch CC
***********************************************************************
*
*        Test 1.9.3.1: zero - neg = neg; CC=3; can't occur
*
***      SR    R0,R0               Set CC
***      LMG   R0,R1,TST_SGR_ZNN3  Initialize registers 0 and 1
***      SGFR  R0,R1               CC=3
***      IPM   R3                  Fetch CC
*
*        Test 1.9.3.2: pos - neg = neg; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SGR_PNN3  Initialize registers 0 and 1
         SGFR  R0,R1               CC=3
         IPM   R3                  Fetch CC
*
*        Test 1.9.3.3: neg - pos = pos; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SGR_NPP3  Initialize registers 0 and 1
         SGFR  R0,R1               CC=3
         IPM   R3                  Fetch CC
***********************************************************************
*
*        End test
*
         BR    R14                 Return to caller
*
         DROP  R15                 End addressability
*
***********************************************************************
*        SH instruction test
***********************************************************************
*
* 4B = SH - Subtract Halfword
*
* operand1 is 32-bit signed value
* operand2 is 16-bit signed value
*
***********************************************************************
*
TSH1     DS    0H
         USING *,R15               Establish addressability
***********************************************************************
*
*        Test 1.10.0.1: zero - zero = zero; CC=0
*
         SR    R0,R0               Set CC
         TM    *,X'80'             Set CC=3
         LMG   R0,R1,TST_SR_ZZZ0   Initialize registers 0 and 1
         STH   R1,HW               Value to subtract
         SH    R0,HW               CC=0
         IPM   R3                  Fetch CC
*
*        Test 1.10.0.2: pos - pos = zero; CC=0
*
         SR    R0,R0               Set CC
         TM    *,X'80'             Set CC=3
         LMG   R0,R1,TST_SR_PPZ0   Initialize registers 0 and 1
         STH   R1,HW               Value to subtract
         SH    R0,HW               CC=0
         IPM   R3                  Fetch CC
*
*        Test 1.10.0.3: neg - neg = zero; CC=0
*
         SR    R0,R0               Set CC
         TM    *,X'80'             Set CC=3
         LMG   R0,R1,TST_SR_NNZ0   Initialize registers 0 and 1
         STH   R1,HW               Value to subtract
         SH    R0,HW               CC=0
         IPM   R3                  Fetch CC
*
*        Test 1.10.0.4: max neg - max neg = zero; CC=0; can't occur
*
***      SR    R0,R0               Set CC
***      TM    *,X'80'             Set CC=3
***      LMG   R0,R1,TST_SR_NNZ0_M Initialize registers 0 and 1
***      STH   R1,HW               Value to subtract
***      SH    R0,HW               CC=0
***      IPM   R3                  Fetch CC
***********************************************************************
*
*        Test 1.10.1.1: zero - pos = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SR_ZPN1   Initialize registers 0 and 1
         STH   R1,HW               Value to subtract
         SH    R0,HW               CC=1
         IPM   R3                  Fetch CC
*
*        Test 1.10.1.2: pos - pos = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SR_PPN1   Initialize registers 0 and 1
         STH   R1,HW               Value to subtract
         SH    R0,HW               CC=1
         IPM   R3                  Fetch CC
*
*        Test 1.10.1.3: neg - zero = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SR_NZN1   Initialize registers 0 and 1
         STH   R1,HW               Value to subtract
         SH    R0,HW               CC=1
         IPM   R3                  Fetch CC
*
*        Test 1.10.1.4: neg - pos = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SR_NPN1   Initialize registers 0 and 1
         STH   R1,HW               Value to subtract
         SH    R0,HW               CC=1
         IPM   R3                  Fetch CC
*
*        Test 1.10.1.5: neg - neg = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SR_NNN1   Initialize registers 0 and 1
         STH   R1,HW               Value to subtract
         SH    R0,HW               CC=1
         IPM   R3                  Fetch CC
***********************************************************************
*
*        Test 1.10.2.1: zero - neg = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SR_ZNP2   Initialize registers 0 and 1
         STH   R1,HW               Value to subtract
         SH    R0,HW               CC=2
         IPM   R3                  Fetch CC
*
*        Test 1.10.2.2: pos - zero = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SR_PZP2   Initialize registers 0 and 1
         STH   R1,HW               Value to subtract
         SH    R0,HW               CC=2
         IPM   R3                  Fetch CC
*
*        Test 1.10.2.3: pos - pos = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SR_PPP2   Initialize registers 0 and 1
         STH   R1,HW               Value to subtract
         SH    R0,HW               CC=2
         IPM   R3                  Fetch CC
*
*        Test 1.10.2.4: pos - neg = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SR_PNP2   Initialize registers 0 and 1
         STH   R1,HW               Value to subtract
         SH    R0,HW               CC=2
         IPM   R3                  Fetch CC
*
*        Test 1.10.2.5: neg - neg = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SR_NNP2   Initialize registers 0 and 1
         STH   R1,HW               Value to subtract
         SH    R0,HW               CC=2
         IPM   R3                  Fetch CC
***********************************************************************
*
*        Test 1.10.3.1: zero - neg = neg; CC=3; can't occur
*
***      SR    R0,R0               Set CC
***      LMG   R0,R1,TST_SR_ZNN3   Initialize registers 0 and 1
***      STH   R1,HW               Value to subtract
***      SH    R0,HW               CC=3
***      IPM   R3                  Fetch CC
*
*        Test 1.10.3.2: pos - neg = neg; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SR_PNN3   Initialize registers 0 and 1
         STH   R1,HW               Value to subtract
         SH    R0,HW               CC=3
         IPM   R3                  Fetch CC
*
*        Test 1.10.3.3: neg - pos = pos; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SR_NPP3   Initialize registers 0 and 1
         STH   R1,HW               Value to subtract
         SH    R0,HW               CC=3
         IPM   R3                  Fetch CC
***********************************************************************
*
*        End test
*
         BR    R14                 Return to caller
*
         DROP  R15                 End addressability
*
***********************************************************************
*        SHY instruction test
***********************************************************************
*
* E37B = SHY - Subtract Halfword
*
* operand1 is 32-bit signed value
* operand2 is 16-bit signed value
*
***********************************************************************
*
TSHY1    DS    0H
         USING *,R15               Establish addressability
***********************************************************************
*
*        Test 1.11.0.1: zero - zero = zero; CC=0
*
         SR    R0,R0               Set CC
         TM    *,X'80'             Set CC=3
         LMG   R0,R1,TST_SR_ZZZ0   Initialize registers 0 and 1
         STH   R1,HW               Value to subtract
         SHY   R0,HW               CC=0
         IPM   R3                  Fetch CC
*
*        Test 1.11.0.2: pos - pos = zero; CC=0
*
         SR    R0,R0               Set CC
         TM    *,X'80'             Set CC=3
         LMG   R0,R1,TST_SR_PPZ0   Initialize registers 0 and 1
         STH   R1,HW               Value to subtract
         SHY   R0,HW               CC=0
         IPM   R3                  Fetch CC
*
*        Test 1.11.0.3: neg - neg = zero; CC=0
*
         SR    R0,R0               Set CC
         TM    *,X'80'             Set CC=3
         LMG   R0,R1,TST_SR_NNZ0   Initialize registers 0 and 1
         STH   R1,HW               Value to subtract
         SHY   R0,HW               CC=0
         IPM   R3                  Fetch CC
*
*        Test 1.11.0.4: max neg - max neg = zero; CC=0; can't occur
*
***      SR    R0,R0               Set CC
***      TM    *,X'80'             Set CC=3
***      LMG   R0,R1,TST_SR_NNZ0_M Initialize registers 0 and 1
***      STH   R1,HW               Value to subtract
***      SHY   R0,HW               CC=0
***      IPM   R3                  Fetch CC
***********************************************************************
*
*        Test 1.11.1.1: zero - pos = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SR_ZPN1   Initialize registers 0 and 1
         STH   R1,HW               Value to subtract
         SHY   R0,HW               CC=1
         IPM   R3                  Fetch CC
*
*        Test 1.11.1.2: pos - pos = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SR_PPN1   Initialize registers 0 and 1
         STH   R1,HW               Value to subtract
         SHY   R0,HW               CC=1
         IPM   R3                  Fetch CC
*
*        Test 1.11.1.3: neg - zero = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SR_NZN1   Initialize registers 0 and 1
         STH   R1,HW               Value to subtract
         SHY   R0,HW               CC=1
         IPM   R3                  Fetch CC
*
*        Test 1.11.1.4: neg - pos = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SR_NPN1   Initialize registers 0 and 1
         STH   R1,HW               Value to subtract
         SHY   R0,HW               CC=1
         IPM   R3                  Fetch CC
*
*        Test 1.11.1.5: neg - neg = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SR_NNN1   Initialize registers 0 and 1
         STH   R1,HW               Value to subtract
         SHY   R0,HW               CC=1
         IPM   R3                  Fetch CC
***********************************************************************
*
*        Test 1.11.2.1: zero - neg = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SR_ZNP2   Initialize registers 0 and 1
         STH   R1,HW               Value to subtract
         SHY   R0,HW               CC=2
         IPM   R3                  Fetch CC
*
*        Test 1.11.2.2: pos - zero = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SR_PZP2   Initialize registers 0 and 1
         STH   R1,HW               Value to subtract
         SHY   R0,HW               CC=2
         IPM   R3                  Fetch CC
*
*        Test 1.11.2.3: pos - pos = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SR_PPP2   Initialize registers 0 and 1
         STH   R1,HW               Value to subtract
         SHY   R0,HW               CC=2
         IPM   R3                  Fetch CC
*
*        Test 1.11.2.4: pos - neg = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SR_PNP2   Initialize registers 0 and 1
         STH   R1,HW               Value to subtract
         SHY   R0,HW               CC=2
         IPM   R3                  Fetch CC
*
*        Test 1.11.2.5: neg - neg = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SR_NNP2   Initialize registers 0 and 1
         STH   R1,HW               Value to subtract
         SHY   R0,HW               CC=2
         IPM   R3                  Fetch CC
***********************************************************************
*
*        Test 1.11.3.1: zero - neg = neg; CC=3; can't occur
*
***      SR    R0,R0               Set CC
***      LMG   R0,R1,TST_SR_ZNN3   Initialize registers 0 and 1
***      STH   R1,HW               Value to subtract
***      SHY   R0,HW               CC=3
***      IPM   R3                  Fetch CC
*
*        Test 1.11.3.2: pos - neg = neg; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SR_PNN3   Initialize registers 0 and 1
         STH   R1,HW               Value to subtract
         SHY   R0,HW               CC=3
         IPM   R3                  Fetch CC
*
*        Test 1.11.3.3: neg - pos = pos; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SR_NPP3   Initialize registers 0 and 1
         STH   R1,HW               Value to subtract
         SHY   R0,HW               CC=3
         IPM   R3                  Fetch CC
***********************************************************************
*
*        End test
*
         BR    R14                 Return to caller
*
         DROP  R15                 End addressability
*
***********************************************************************
*        SHHHR instruction test
***********************************************************************
*
* B9C9 = SHHHR - Subtract High
*
* operand1 is 32-bit signed value in high half of register
* operand2 is 32-bit signed value in high half of register
* operand3 is 32-bit signed value in high half of register
*
***********************************************************************
*
TSHHHR1  DS    0H
         USING *,R15               Establish addressability
***********************************************************************
*
*        Test 1.12.0.1: zero - zero = zero; CC=0
*
         SR    R0,R0               Set CC
         TM    *,X'80'             Set CC=3
         LMG   R0,R1,TST_SHHHR_ZZZ0 Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         SHHHR R2,R0,R1            CC=0
         IPM   R3                  Fetch CC
*
*        Test 1.12.0.2: pos - pos = zero; CC=0
*
         SR    R0,R0               Set CC
         TM    *,X'80'             Set CC=3
         LMG   R0,R1,TST_SHHHR_PPZ0 Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         SHHHR R2,R0,R1            CC=0
         IPM   R3                  Fetch CC
*
*        Test 1.12.0.3: neg - neg = zero; CC=0
*
         SR    R0,R0               Set CC
         TM    *,X'80'             Set CC=3
         LMG   R0,R1,TST_SHHHR_NNZ0 Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         SHHHR R2,R0,R1            CC=0
         IPM   R3                  Fetch CC
*
*        Test 1.12.0.4: max neg - max neg = zero; CC=0
*
         SR    R0,R0               Set CC
         TM    *,X'80'             Set CC=3
         LMG   R0,R1,TST_SHHHR_NNZ0_M Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         SHHHR R2,R0,R1            CC=0
         IPM   R3                  Fetch CC
***********************************************************************
*
*        Test 1.12.1.1: zero - neg = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SHHHR_ZPN1 Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         SHHHR R2,R0,R1            CC=1
         IPM   R3                  Fetch CC
*
*        Test 1.12.1.2: pos - pos = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SHHHR_PPN1 Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         SHHHR R2,R0,R1            CC=1
         IPM   R3                  Fetch CC
*
*        Test 1.12.1.3: neg - zero = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SHHHR_NZN1 Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         SHHHR R2,R0,R1            CC=1
         IPM   R3                  Fetch CC
*
*        Test 1.12.1.4: neg - pos = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SHHHR_NPN1 Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         SHHHR R2,R0,R1            CC=1
         IPM   R3                  Fetch CC
*
*        Test 1.12.1.5: neg - neg = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SHHHR_NNN1 Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         SHHHR R2,R0,R1            CC=1
         IPM   R3                  Fetch CC
***********************************************************************
*
*        Test 1.12.2.1: zero - neg = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SHHHR_ZNP2 Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         SHHHR R2,R0,R1            CC=2
         IPM   R3                  Fetch CC
*
*        Test 1.12.2.2: pos - zero = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SHHHR_PZP2 Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         SHHHR R2,R0,R1            CC=2
         IPM   R3                  Fetch CC
*
*        Test 1.12.2.3: pos - pos = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SHHHR_PPP2 Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         SHHHR R2,R0,R1            CC=2
         IPM   R3                  Fetch CC
*
*        Test 1.12.2.4: pos - neg = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SHHHR_PNP2 Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         SHHHR R2,R0,R1            CC=2
         IPM   R3                  Fetch CC
*
*        Test 1.12.2.5: neg - neg = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SHHHR_NNP2 Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         SHHHR R2,R0,R1            CC=2
         IPM   R3                  Fetch CC
***********************************************************************
*
*        Test 1.12.3.1: zero - neg = neg; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SHHHR_ZNN3 Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         SHHHR R2,R0,R1            CC=3
         IPM   R3                  Fetch CC
*
*        Test 1.12.3.2: pos - neg = neg; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SHHHR_PNN3 Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         SHHHR R2,R0,R1            CC=3
         IPM   R3                  Fetch CC
*
*        Test 1.12.3.3: neg - pos = pos; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SHHHR_NPP3 Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         SHHHR R2,R0,R1            CC=3
         IPM   R3                  Fetch CC
***********************************************************************
*
*        End test
*
         BR    R14                 Return to caller
*
         DROP  R15                 End addressability
*
***********************************************************************
*        SHHLR instruction test
***********************************************************************
*
* B9C9 = SHHLR - Subtract High
*
* operand1 is 32-bit signed value in high half of register
* operand2 is 32-bit signed value in high half of register
* operand3 is 32-bit signed value in low half of register
*
***********************************************************************
*
TSHHLR1  DS    0H
         USING *,R15               Establish addressability
***********************************************************************
*
*        Test 1.13.0.1: zero - zero = zero; CC=0
*
         SR    R0,R0               Set CC
         TM    *,X'80'             Set CC=3
         LMG   R0,R1,TST_SHHLR_ZZZ0 Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         SHHLR R2,R0,R1            CC=0
         IPM   R3                  Fetch CC
*
*        Test 1.13.0.2: pos - pos = zero; CC=0
*
         SR    R0,R0               Set CC
         TM    *,X'80'             Set CC=3
         LMG   R0,R1,TST_SHHLR_PPZ0 Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         SHHLR R2,R0,R1            CC=0
         IPM   R3                  Fetch CC
*
*        Test 1.13.0.3: neg - neg = zero; CC=0
*
         SR    R0,R0               Set CC
         TM    *,X'80'             Set CC=3
         LMG   R0,R1,TST_SHHLR_NNZ0 Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         SHHLR R2,R0,R1            CC=0
         IPM   R3                  Fetch CC
*
*        Test 1.13.0.4: max neg - max neg = zero; CC=0
*
         SR    R0,R0               Set CC
         TM    *,X'80'             Set CC=3
         LMG   R0,R1,TST_SHHLR_NNZ0_M Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         SHHLR R2,R0,R1            CC=0
         IPM   R3                  Fetch CC
***********************************************************************
*
*        Test 1.13.1.1: zero - neg = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SHHLR_ZPN1 Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         SHHLR R2,R0,R1            CC=1
         IPM   R3                  Fetch CC
*
*        Test 1.13.1.2: pos - pos = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SHHLR_PPN1 Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         SHHLR R2,R0,R1            CC=1
         IPM   R3                  Fetch CC
*
*        Test 1.13.1.3: neg - zero = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SHHLR_NZN1 Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         SHHLR R2,R0,R1            CC=1
         IPM   R3                  Fetch CC
*
*        Test 1.13.1.4: neg - pos = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SHHLR_NPN1 Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         SHHLR R2,R0,R1            CC=1
         IPM   R3                  Fetch CC
*
*        Test 1.13.1.5: neg - neg = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SHHLR_NNN1 Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         SHHLR R2,R0,R1            CC=1
         IPM   R3                  Fetch CC
***********************************************************************
*
*        Test 1.13.2.1: zero - neg = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SHHLR_ZNP2 Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         SHHLR R2,R0,R1            CC=2
         IPM   R3                  Fetch CC
*
*        Test 1.13.2.2: pos - zero = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SHHLR_PZP2 Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         SHHLR R2,R0,R1            CC=2
         IPM   R3                  Fetch CC
*
*        Test 1.13.2.3: pos - pos = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SHHLR_PPP2 Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         SHHLR R2,R0,R1            CC=2
         IPM   R3                  Fetch CC
*
*        Test 1.13.2.4: pos - neg = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SHHLR_PNP2 Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         SHHLR R2,R0,R1            CC=2
         IPM   R3                  Fetch CC
*
*        Test 1.13.2.5: neg - neg = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SHHLR_NNP2 Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         SHHLR R2,R0,R1            CC=2
         IPM   R3                  Fetch CC
***********************************************************************
*
*        Test 1.13.3.1: zero - neg = neg; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SHHLR_ZNN3 Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         SHHLR R2,R0,R1            CC=3
         IPM   R3                  Fetch CC
*
*        Test 1.13.3.2: pos - neg = neg; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SHHLR_PNN3 Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         SHHLR R2,R0,R1            CC=3
         IPM   R3                  Fetch CC
*
*        Test 1.13.3.3: neg - pos = pos; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SHHLR_NPP3 Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         SHHLR R2,R0,R1            CC=3
         IPM   R3                  Fetch CC
***********************************************************************
*
*        End test
*
         BR    R14                 Return to caller
*
         DROP  R15                 End addressability
*
*
*
***********************************************************************
*
* End Part 1 test set
*
***********************************************************************
*
*
*
***********************************************************************
*
* Begin Part 2 test set: test CC=3; fixed-point-overflow (PIC = 0008)
*
***********************************************************************
*
*
*
***********************************************************************
*        S instruction test
***********************************************************************
TS2      DS    0H
         USING *,R15               Establish addressability
***********************************************************************
*
*        Test 2.1.3.1: zero - neg = neg; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SR_ZNN3   Initialize registers 0 and 1
         ST    R1,FW               Value to subtract
         LA    R4,S_Ret1           Return address
         ST    R4,ESRET@           Save for ESPIE exit
         S     R0,FW               Fixed-point-overflow exception (0C8)
         DC    H'0'                Should not get here
S_Ret1   DS    0H
         IPM   R3                  Fetch CC
*
*        Test 2.1.3.2: pos - neg = neg; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SR_PNN3   Initialize registers 0 and 1
         ST    R1,FW               Value to subtract
         LA    R4,S_Ret2           Return address
         ST    R4,ESRET@           Save for ESPIE exit
         S     R0,FW               Fixed-point-overflow exception (0C8)
         DC    H'0'                Should not get here
S_Ret2   DS    0H
         IPM   R3                  Fetch CC
*
*        Test 2.1.3.3: neg - pos = pos; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SR_NPP3   Initialize registers 0 and 1
         ST    R1,FW               Value to subtract
         LA    R4,S_Ret3           Return address
         ST    R4,ESRET@           Save for ESPIE exit
         S     R0,FW               Fixed-point-overflow exception (0C8)
         DC    H'0'                Should not get here
S_Ret3   DS    0H
         IPM   R3                  Fetch CC
***********************************************************************
*
*        End test
*
         BR    R14                 Return to caller
*
         DROP  R15                 End addressability
*
***********************************************************************
*        SR instruction test
***********************************************************************
TSR2     DS    0H
         USING *,R15               Establish addressability
***********************************************************************
*
*        Test 2.2.3.1: zero - neg = neg; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SR_ZNN3   Initialize registers 0 and 1
         LA    R4,SR_Ret1          Return address
         ST    R4,ESRET@           Save for ESPIE exit
         SR    R0,R1               Fixed-point-overflow exception (0C8)
         DC    H'0'                Should not get here
SR_Ret1  DS    0H
         IPM   R3                  Fetch CC
*
*        Test 2.2.3.2: pos - neg = neg; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SR_PNN3   Initialize registers 0 and 1
         LA    R4,SR_Ret2          Return address
         ST    R4,ESRET@           Save for ESPIE exit
         SR    R0,R1               Fixed-point-overflow exception (0C8)
         DC    H'0'                Should not get here
SR_Ret2  DS    0H
         IPM   R3                  Fetch CC
*
*        Test 2.2.3.3: neg - pos = pos; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SR_NPP3   Initialize registers 0 and 1
         LA    R4,SR_Ret3          Return address
         ST    R4,ESRET@           Save for ESPIE exit
         SR    R0,R1               Fixed-point-overflow exception (0C8)
         DC    H'0'                Should not get here
SR_Ret3  DS    0H
         IPM   R3                  Fetch CC
***********************************************************************
*
*        End test
*
         BR    R14                 Return to caller
*
         DROP  R15                 End addressability
*
***********************************************************************
*        SRK instruction test
***********************************************************************
TSRK2    DS    0H
         USING *,R15               Establish addressability
***********************************************************************
*
*        Test 2.3.3.1: zero - neg = neg; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SR_ZNN3   Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         LA    R4,SRK_Ret1         Return address
         ST    R4,ESRET@           Save for ESPIE exit
         SRK   R2,R0,R1            Fixed-point-overflow exception (0C8)
         DC    H'0'                Should not get here
SRK_Ret1 DS    0H
         IPM   R3                  Fetch CC
*
*        Test 2.3.3.2: pos - neg = neg; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SR_PNN3   Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         LA    R4,SRK_Ret2         Return address
         ST    R4,ESRET@           Save for ESPIE exit
         SRK   R2,R0,R1            Fixed-point-overflow exception (0C8)
         DC    H'0'                Should not get here
SRK_Ret2 DS    0H
         IPM   R3                  Fetch CC
*
*        Test 2.3.3.3: neg - pos = pos; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SR_NPP3   Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         LA    R4,SRK_Ret3         Return address
         ST    R4,ESRET@           Save for ESPIE exit
         SRK   R2,R0,R1            Fixed-point-overflow exception (0C8)
         DC    H'0'                Should not get here
SRK_Ret3 DS    0H
         IPM   R3                  Fetch CC
***********************************************************************
*
*        End test
*
         BR    R14                 Return to caller
*
         DROP  R15                 End addressability
*
***********************************************************************
*        SY instruction test
***********************************************************************
TSY2     DS    0H
         USING *,R15               Establish addressability
***********************************************************************
*
*        Test 2.4.3.1: zero - neg = neg; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SR_ZNN3   Initialize registers 0 and 1
         ST    R1,FW               Value to subtract
         LA    R4,SY_Ret1          Return address
         ST    R4,ESRET@           Save for ESPIE exit
         SY    R0,FW               Fixed-point-overflow exception (0C8)
         DC    H'0'                Should not get here
SY_Ret1  DS    0H
         IPM   R3                  Fetch CC
*
*        Test 2.4.3.2: pos - neg = neg; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SR_PNN3   Initialize registers 0 and 1
         ST    R1,FW               Value to subtract
         LA    R4,SY_Ret2          Return address
         ST    R4,ESRET@           Save for ESPIE exit
         SY    R0,FW               Fixed-point-overflow exception (0C8)
         DC    H'0'                Should not get here
SY_Ret2  DS    0H
         IPM   R3                  Fetch CC
*
*        Test 2.4.3.3: neg - pos = pos; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SR_NPP3   Initialize registers 0 and 1
         ST    R1,FW               Value to subtract
         LA    R4,SY_Ret3          Return address
         ST    R4,ESRET@           Save for ESPIE exit
         SY    R0,FW               Fixed-point-overflow exception (0C8)
         DC    H'0'                Should not get here
SY_Ret3  DS    0H
         IPM   R3                  Fetch CC
***********************************************************************
*
*        End test
*
         BR    R14                 Return to caller
*
         DROP  R15                 End addressability
*
***********************************************************************
*        SG instruction test
***********************************************************************
TSG2     DS    0H
         USING *,R15               Establish addressability
***********************************************************************
*
*        Test 2.5.3.1: zero - neg = neg; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SGR_ZNN3  Initialize registers 0 and 1
         STG   R1,DW               Value to subtract
         LA    R4,SG_Ret1          Return address
         ST    R4,ESRET@           Save for ESPIE exit
         SG    R0,DW               Fixed-point-overflow exception (0C8)
         DC    H'0'                Should not get here
SG_Ret1  DS    0H
         IPM   R3                  Fetch CC
*
*        Test 2.5.3.2: pos - neg = neg; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SGR_PNN3  Initialize registers 0 and 1
         STG   R1,DW               Value to subtract
         LA    R4,SG_Ret2          Return address
         ST    R4,ESRET@           Save for ESPIE exit
         SG    R0,DW               Fixed-point-overflow exception (0C8)
         DC    H'0'                Should not get here
SG_Ret2  DS    0H
         IPM   R3                  Fetch CC
*
*        Test 2.5.3.3: neg - pos = pos; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SGR_NPP3  Initialize registers 0 and 1
         STG   R1,DW               Value to subtract
         LA    R4,SG_Ret3          Return address
         ST    R4,ESRET@           Save for ESPIE exit
         SG    R0,DW               Fixed-point-overflow exception (0C8)
         DC    H'0'                Should not get here
SG_Ret3  DS    0H
         IPM   R3                  Fetch CC
***********************************************************************
*
*        End test
*
         BR    R14                 Return to caller
*
         DROP  R15                 End addressability
*
***********************************************************************
*        SGR instruction test
***********************************************************************
TSGR2    DS    0H
         USING *,R15               Establish addressability
***********************************************************************
*
*        Test 2.6.3.1: zero - neg = neg; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SGR_ZNN3  Initialize registers 0 and 1
         LA    R4,SGR_Ret1         Return address
         ST    R4,ESRET@           Save for ESPIE exit
         SGR   R0,R1               Fixed-point-overflow exception (0C8)
         DC    H'0'                Should not get here
SGR_Ret1 DS    0H
         IPM   R3                  Fetch CC
*
*        Test 2.6.3.2: pos - neg = neg; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SGR_PNN3  Initialize registers 0 and 1
         LA    R4,SGR_Ret2         Return address
         ST    R4,ESRET@           Save for ESPIE exit
         SGR   R0,R1               Fixed-point-overflow exception (0C8)
         DC    H'0'                Should not get here
SGR_Ret2 DS    0H
         IPM   R3                  Fetch CC
*
*        Test 2.6.3.3: neg - pos = pos; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SGR_NPP3  Initialize registers 0 and 1
         LA    R4,SGR_Ret3         Return address
         ST    R4,ESRET@           Save for ESPIE exit
         SGR   R0,R1               Fixed-point-overflow exception (0C8)
         DC    H'0'                Should not get here
SGR_Ret3 DS    0H
         IPM   R3                  Fetch CC
***********************************************************************
*
*        End test
*
         BR    R14                 Return to caller
*
         DROP  R15                 End addressability
*
***********************************************************************
*        SGRK instruction test
***********************************************************************
TSGRK2   DS    0H
         USING *,R15               Establish addressability
***********************************************************************
*
*        Test 2.7.3.1: zero - neg = neg; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SGR_ZNN3  Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         LA    R4,SGRK_Ret1        Return address
         ST    R4,ESRET@           Save for ESPIE exit
         SGRK  R2,R0,R1            Fixed-point-overflow exception (0C8)
         DC    H'0'                Should not get here
SGRK_Ret1 DS   0H
         IPM   R3                  Fetch CC
*
*        Test 2.7.3.2: pos - neg = neg; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SGR_PNN3  Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         LA    R4,SGRK_Ret2        Return address
         ST    R4,ESRET@           Save for ESPIE exit
         SGRK  R2,R0,R1            Fixed-point-overflow exception (0C8)
         DC    H'0'                Should not get here
SGRK_Ret2 DS   0H
         IPM   R3                  Fetch CC
*
*        Test 2.7.3.3: neg - pos = pos; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SGR_NPP3  Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         LA    R4,SGRK_Ret3        Return address
         ST    R4,ESRET@           Save for ESPIE exit
         SGRK  R2,R0,R1            Fixed-point-overflow exception (0C8)
         DC    H'0'                Should not get here
SGRK_Ret3 DS   0H
         IPM   R3                  Fetch CC
***********************************************************************
*
*        End test
*
         BR    R14                 Return to caller
*
         DROP  R15                 End addressability
*
***********************************************************************
*        SGF instruction test
***********************************************************************
TSGF2    DS    0H
         USING *,R15               Establish addressability
***********************************************************************
*
*        Test 2.8.3.1: zero - neg = neg; CC=3; can't occur
*
***      SR    R0,R0               Set CC
***      LMG   R0,R1,TST_SGR_ZNN3  Initialize registers 0 and 1
***      ST    R1,FW               Value to subtract
***      LA    R4,SGF_Ret1         Return address
***      ST    R4,ESRET@           Save for ESPIE exit
***      SGF   R0,FW               Fixed-point-overflow exception (0C8)
***      DC    H'0'                Should not get here
*SGF_Ret1 DS    0H
***      IPM   R3                  Fetch CC
*
*        Test 2.8.3.2: pos - neg = neg; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SGR_PNN3  Initialize registers 0 and 1
         ST    R1,FW               Value to subtract
         LA    R4,SGF_Ret2         Return address
         ST    R4,ESRET@           Save for ESPIE exit
         SGF   R0,FW               Fixed-point-overflow exception (0C8)
         DC    H'0'                Should not get here
SGF_Ret2 DS    0H
         IPM   R3                  Fetch CC
*
*        Test 2.8.3.3: neg - pos = pos; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SGR_NPP3  Initialize registers 0 and 1
         ST    R1,FW               Value to subtract
         LA    R4,SGF_Ret3         Return address
         ST    R4,ESRET@           Save for ESPIE exit
         SGF   R0,FW               Fixed-point-overflow exception (0C8)
         DC    H'0'                Should not get here
SGF_Ret3 DS    0H
         IPM   R3                  Fetch CC
***********************************************************************
*
*        End test
*
         BR    R14                 Return to caller
*
         DROP  R15                 End addressability
*
***********************************************************************
*        SGFR instruction test
***********************************************************************
TSGFR2   DS    0H
         USING *,R15               Establish addressability
***********************************************************************
*
*        Test 2.9.3.1: zero - neg = neg; CC=3; can't occur
*
***      SR    R0,R0               Set CC
***      LMG   R0,R1,TST_SGR_ZNN3  Initialize registers 0 and 1
***      LA    R4,SGFR_Ret1        Return address
***      ST    R4,ESRET@           Save for ESPIE exit
***      SGFR  R0,R1               Fixed-point-overflow exception (0C8)
***      DC    H'0'                Should not get here
*SGFR_Ret1 DS    0H
***      IPM   R3                  Fetch CC
*
*        Test 2.9.3.2: pos - neg = neg; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SGR_PNN3  Initialize registers 0 and 1
         LA    R4,SGFR_Ret2        Return address
         ST    R4,ESRET@           Save for ESPIE exit
         SGFR  R0,R1               Fixed-point-overflow exception (0C8)
         DC    H'0'                Should not get here
SGFR_Ret2 DS    0H
         IPM   R3                  Fetch CC
*
*        Test 2.9.3.3: neg - pos = pos; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SGR_NPP3  Initialize registers 0 and 1
         LA    R4,SGFR_Ret3        Return address
         ST    R4,ESRET@           Save for ESPIE exit
         SGFR  R0,R1               Fixed-point-overflow exception (0C8)
         DC    H'0'                Should not get here
SGFR_Ret3 DS    0H
         IPM   R3                  Fetch CC
***********************************************************************
*
*        End test
*
         BR    R14                 Return to caller
*
         DROP  R15                 End addressability
*
***********************************************************************
*        SH instruction test
***********************************************************************
TSH2     DS    0H
         USING *,R15               Establish addressability
***********************************************************************
*
*        Test 2.10.3.1: zero - neg = neg; CC=3; can't occur
*
***      SR    R0,R0               Set CC
***      LMG   R0,R1,TST_SR_ZNN3   Initialize registers 0 and 1
***      STH   R1,HW               Value to subtract
***      LA    R4,SH_Ret1          Return address
***      ST    R4,ESRET@           Save for ESPIE exit
***      SH    R0,HW               Fixed-point-overflow exception (0C8)
***      DC    H'0'                Should not get here
*SH_Ret1  DS    0H
***      IPM   R3                  Fetch CC
*
*        Test 2.10.3.2: pos - neg = neg; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SR_PNN3   Initialize registers 0 and 1
         STH   R1,HW               Value to subtract
         LA    R4,SH_Ret2          Return address
         ST    R4,ESRET@           Save for ESPIE exit
         SH    R0,HW               Fixed-point-overflow exception (0C8)
         DC    H'0'                Should not get here
SH_Ret2  DS    0H
         IPM   R3                  Fetch CC
*
*        Test 2.10.3.3: neg - pos = pos; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SR_NPP3   Initialize registers 0 and 1
         STH   R1,HW               Value to subtract
         LA    R4,SH_Ret3          Return address
         ST    R4,ESRET@           Save for ESPIE exit
         SH    R0,HW               Fixed-point-overflow exception (0C8)
         DC    H'0'                Should not get here
SH_Ret3  DS    0H
         IPM   R3                  Fetch CC
***********************************************************************
*
*        End test
*
         BR    R14                 Return to caller
*
         DROP  R15                 End addressability
*
***********************************************************************
*        SHY instruction test
***********************************************************************
TSHY2    DS    0H
         USING *,R15               Establish addressability
***********************************************************************
*
*        Test 2.11.3.1: zero - neg = neg; CC=3; can't occur
*
***      SR    R0,R0               Set CC
***      LMG   R0,R1,TST_SR_ZNN3   Initialize registers 0 and 1
***      STH   R1,HW               Value to subtract
***      LA    R4,SHY_Ret1         Return address
***      ST    R4,ESRET@           Save for ESPIE exit
***      SHY   R0,HW               Fixed-point-overflow exception (0C8)
***      DC    H'0'                Should not get here
*SHY_Ret1 DS    0H
***      IPM   R3                  Fetch CC
*
*        Test 2.11.3.2: pos - neg = neg; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SR_PNN3   Initialize registers 0 and 1
         STH   R1,HW               Value to subtract
         LA    R4,SHY_Ret2         Return address
         ST    R4,ESRET@           Save for ESPIE exit
         SHY   R0,HW               Fixed-point-overflow exception (0C8)
         DC    H'0'                Should not get here
SHY_Ret2 DS    0H
         IPM   R3                  Fetch CC
*
*        Test 2.11.3.3: neg - pos = pos; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SR_NPP3   Initialize registers 0 and 1
         STH   R1,HW               Value to subtract
         LA    R4,SHY_Ret3         Return address
         ST    R4,ESRET@           Save for ESPIE exit
         SHY   R0,HW               Fixed-point-overflow exception (0C8)
         DC    H'0'                Should not get here
SHY_Ret3 DS    0H
         IPM   R3                  Fetch CC
***********************************************************************
*
*        End test
*
         BR    R14                 Return to caller
*
         DROP  R15                 End addressability
*
***********************************************************************
*        SHHHR instruction test
***********************************************************************
TSHHHR2  DS    0H
         USING *,R15               Establish addressability
***********************************************************************
*
*        Test 2.12.3.1: zero - neg = neg; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SHHHR_ZNN3 Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         LA    R4,SHHHR_Ret1       Return address
         ST    R4,ESRET@           Save for ESPIE exit
         SHHHR R2,R0,R1            Fixed-point-overflow exception (0C8)
         DC    H'0'                Should not get here
SHHHR_Ret1 DS    0H
         IPM   R3                  Fetch CC
*
*        Test 2.12.3.2: pos - neg = neg; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SHHHR_PNN3 Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         LA    R4,SHHHR_Ret2       Return address
         ST    R4,ESRET@           Save for ESPIE exit
         SHHHR R2,R0,R1            Fixed-point-overflow exception (0C8)
         DC    H'0'                Should not get here
SHHHR_Ret2 DS    0H
         IPM   R3                  Fetch CC
*
*        Test 2.12.3.3: neg - pos = pos; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SHHHR_NPP3 Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         LA    R4,SHHHR_Ret3       Return address
         ST    R4,ESRET@           Save for ESPIE exit
         SHHHR R2,R0,R1            Fixed-point-overflow exception (0C8)
         DC    H'0'                Should not get here
SHHHR_Ret3 DS    0H
         IPM   R3                  Fetch CC
***********************************************************************
*
*        End test
*
         BR    R14                 Return to caller
*
         DROP  R15                 End addressability
*
***********************************************************************
*        SHHLR instruction test
***********************************************************************
TSHHLR2  DS    0H
         USING *,R15               Establish addressability
***********************************************************************
*
*        Test 2.13.3.1: zero - neg = neg; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SHHLR_ZNN3 Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         LA    R4,SHHLR_Ret1       Return address
         ST    R4,ESRET@           Save for ESPIE exit
         SHHLR R2,R0,R1            Fixed-point-overflow exception (0C8)
         DC    H'0'                Should not get here
SHHLR_Ret1 DS    0H
         IPM   R3                  Fetch CC
*
*        Test 2.13.3.2: pos - neg = neg; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SHHLR_PNN3 Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         LA    R4,SHHLR_Ret2       Return address
         ST    R4,ESRET@           Save for ESPIE exit
         SHHLR R2,R0,R1            Fixed-point-overflow exception (0C8)
         DC    H'0'                Should not get here
SHHLR_Ret2 DS    0H
         IPM   R3                  Fetch CC
*
*        Test 2.13.3.3: neg - pos = pos; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_SHHLR_NPP3 Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         LA    R4,SHHLR_Ret3       Return address
         ST    R4,ESRET@           Save for ESPIE exit
         SHHLR R2,R0,R1            Fixed-point-overflow exception (0C8)
         DC    H'0'                Should not get here
SHHLR_Ret3 DS    0H
         IPM   R3                  Fetch CC
***********************************************************************
*
*        End test
*
         BR    R14                 Return to caller
*
         DROP  R15                 End addressability
*
*
*
***********************************************************************
*
* End Part 2 test set
*
***********************************************************************
*
*
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
         BR    R14                 Return to operating system
*
         LTORG
*
         POP   USING               Restore USING state
*
STOR     DS    0D
***********************************************************************
*        Subtract instruction tests
***********************************************************************
               DS    0D
TST_SR_ZZZ0    DC    X'ffffffff00000000',X'ffffffff00000000'
TST_SR_PPZ0    DC    X'ffffffff00000001',X'ffffffff00000001'
TST_SR_NNZ0    DC    X'ffffffffffffffff',X'ffffffffffffffff'
TST_SR_NNZ0_M  DC    X'ffffffff80000000',X'ffffffff80000000'
*
TST_SR_ZPN1    DC    X'ffffffff00000000',X'ffffffff00000001'
TST_SR_PPN1    DC    X'ffffffff00000001',X'ffffffff00000002'
TST_SR_NZN1    DC    X'ffffffffffffffff',X'ffffffff00000000'
TST_SR_NPN1    DC    X'ffffffffffffffff',X'ffffffff00000002'
TST_SR_NNN1    DC    X'fffffffffffffffd',X'ffffffffffffffff'
*
TST_SR_ZNP2    DC    X'ffffffff00000000',X'ffffffffffffffff'
TST_SR_PZP2    DC    X'ffffffff00000001',X'ffffffff00000000'
TST_SR_PPP2    DC    X'ffffffff00000003',X'ffffffff00000001'
TST_SR_PNP2    DC    X'ffffffff00000001',X'fffffffffffffffe'
TST_SR_NNP2    DC    X'ffffffffffffffff',X'fffffffffffffffe'
*
TST_SR_ZNN3    DC    X'ffffffff00000000',X'ffffffff80000000'
TST_SR_PNN3    DC    X'ffffffff7fffffff',X'ffffffffffffffff'
TST_SR_NPP3    DC    X'ffffffff80000001',X'ffffffff00000002'
*
***********************************************************************
*
TST_SGR_ZZZ0   DC    X'0000000000000000',X'0000000000000000'
TST_SGR_PPZ0   DC    X'0000000000000001',X'0000000000000001'
TST_SGR_NNZ0   DC    X'ffffffffffffffff',X'ffffffffffffffff'
TST_SGR_NNZ0_M DC    X'8000000000000000',X'8000000000000000'
*
TST_SGR_ZPN1   DC    X'0000000000000000',X'0000000000000001'
TST_SGR_PPN1   DC    X'0000000000000001',X'0000000000000002'
TST_SGR_NZN1   DC    X'ffffffffffffffff',X'0000000000000000'
TST_SGR_NPN1   DC    X'ffffffffffffffff',X'0000000000000002'
TST_SGR_NNN1   DC    X'fffffffffffffffd',X'ffffffffffffffff'
*
TST_SGR_ZNP2   DC    X'0000000000000000',X'ffffffffffffffff'
TST_SGR_PZP2   DC    X'0000000000000001',X'0000000000000000'
TST_SGR_PPP2   DC    X'0000000000000003',X'0000000000000001'
TST_SGR_PNP2   DC    X'0000000000000001',X'fffffffffffffffe'
TST_SGR_NNP2   DC    X'ffffffffffffffff',X'fffffffffffffffe'
*
TST_SGR_ZNN3   DC    X'0000000000000000',X'8000000000000000'
TST_SGR_PNN3   DC    X'7fffffffffffffff',X'ffffffffffffffff'
TST_SGR_NPP3   DC    X'8000000000000001',X'0000000000000002'
*
***********************************************************************
                  DS    0D
TST_SHHHR_ZZZ0    DC    X'00000000ffffffff',X'00000000ffffffff'
TST_SHHHR_PPZ0    DC    X'00000001ffffffff',X'00000001ffffffff'
TST_SHHHR_NNZ0    DC    X'ffffffffffffffff',X'ffffffffffffffff'
TST_SHHHR_NNZ0_M  DC    X'80000000ffffffff',X'80000000ffffffff'
*
TST_SHHHR_ZPN1    DC    X'00000000ffffffff',X'00000001ffffffff'
TST_SHHHR_PPN1    DC    X'00000001ffffffff',X'00000002ffffffff'
TST_SHHHR_NZN1    DC    X'ffffffffffffffff',X'00000000ffffffff'
TST_SHHHR_NPN1    DC    X'ffffffffffffffff',X'00000002ffffffff'
TST_SHHHR_NNN1    DC    X'fffffffdffffffff',X'ffffffffffffffff'
*
TST_SHHHR_ZNP2    DC    X'00000000ffffffff',X'ffffffffffffffff'
TST_SHHHR_PZP2    DC    X'00000001ffffffff',X'00000000ffffffff'
TST_SHHHR_PPP2    DC    X'00000003ffffffff',X'00000001ffffffff'
TST_SHHHR_PNP2    DC    X'00000001ffffffff',X'fffffffeffffffff'
TST_SHHHR_NNP2    DC    X'ffffffffffffffff',X'fffffffeffffffff'
*
TST_SHHHR_ZNN3    DC    X'00000000ffffffff',X'80000000ffffffff'
TST_SHHHR_PNN3    DC    X'7fffffffffffffff',X'ffffffffffffffff'
TST_SHHHR_NPP3    DC    X'80000001ffffffff',X'00000002ffffffff'
*
***********************************************************************
                  DS    0D
TST_SHHLR_ZZZ0    DC    X'00000000ffffffff',X'ffffffff00000000'
TST_SHHLR_PPZ0    DC    X'00000001ffffffff',X'ffffffff00000001'
TST_SHHLR_NNZ0    DC    X'ffffffffffffffff',X'ffffffffffffffff'
TST_SHHLR_NNZ0_M  DC    X'80000000ffffffff',X'ffffffff80000000'
*
TST_SHHLR_ZPN1    DC    X'00000000ffffffff',X'ffffffff00000001'
TST_SHHLR_PPN1    DC    X'00000001ffffffff',X'ffffffff00000002'
TST_SHHLR_NZN1    DC    X'ffffffffffffffff',X'ffffffff00000000'
TST_SHHLR_NPN1    DC    X'ffffffffffffffff',X'ffffffff00000002'
TST_SHHLR_NNN1    DC    X'fffffffdffffffff',X'ffffffffffffffff'
*
TST_SHHLR_ZNP2    DC    X'00000000ffffffff',X'ffffffffffffffff'
TST_SHHLR_PZP2    DC    X'00000001ffffffff',X'ffffffff00000000'
TST_SHHLR_PPP2    DC    X'00000003ffffffff',X'ffffffff00000001'
TST_SHHLR_PNP2    DC    X'00000001ffffffff',X'fffffffffffffffe'
TST_SHHLR_NNP2    DC    X'ffffffffffffffff',X'fffffffffffffffe'
*
TST_SHHLR_ZNN3    DC    X'00000000ffffffff',X'ffffffff80000000'
TST_SHHLR_PNN3    DC    X'7fffffffffffffff',X'ffffffffffffffff'
TST_SHHLR_NPP3    DC    X'80000001ffffffff',X'ffffffff00000002'
*
***********************************************************************
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
DW       DS    D                   Doubleword to add/subtract
FW       DS    F                   Fullword to add/subtact
HW       DS    H                   Halfword to add/subtract
*
RetCode  DS    F                   Return code
*
***********************************************************************
         DS    0D
InitGR2  DC    X'12345678a1b2c3d4' Initial value for GR2
***********************************************************************
*        WTOs
***********************************************************************
*
WTO1     WTO   'Running CC test of instruction xxxxxxxx',MF=L
W1Mne    EQU   WTO1+4+31,8
*
WTO2     WTO   'Running OV test of instruction xxxxxxxx',MF=L
W2Mne    EQU   WTO2+4+31,8
*
***********************************************************************
*
TestMne  DS    CL8                 Test mnemonic if only one test
*
Spaces   DC    CL(L'TestMne)' '    Spaces
*
***********************************************************************
*        Table of all tests.
*
*        The TESTENT DSECT overlays an entry in the table.
*
*        DO NOT CHANGE THE ORDER OF THE ENTRIES IN THE TABLE!!!
*
*        The test script that runs all the tests depends on the order.
*
***********************************************************************
*
Tests    DC    A(Test1,16,TestN,0) First, length, last, unused
*
Test1    DS    0D
         DC    CL8'S',A(TS1),A(TS2) Mnemonic, CC=3 test, overflow test
         DC    CL8'SR',A(TSR1),A(TSR2)
         DC    CL8'SRK',A(TSRK1),A(TSRK2)
         DC    CL8'SY',A(TSY1),A(TSY2)
         DC    CL8'SG',A(TSG1),A(TSG2)
         DC    CL8'SGR',A(TSGR1),A(TSGR2)
         DC    CL8'SGRK',A(TSGRK1),A(TSGRK2)
         DC    CL8'SGF',A(TSGF1),A(TSGF2)
         DC    CL8'SGFR',A(TSGFR1),A(TSGFR2)
         DC    CL8'SH',A(TSH1),A(TSH2)
         DC    CL8'SHY',A(TSHY1),A(TSHY2)
         DC    CL8'SHHHR',A(TSHHHR1),A(TSHHHR2)
         DC    CL8'SHHLR',A(TSHHLR1),A(TSHHLR2)
***********************************************************************
*        Add new tests above this comment
***********************************************************************
TestN    EQU   *-16                Last test
***********************************************************************
         TITLE 'DSECT for EPIE'
         IHAEPIE ,                 DSECT for EPIE
*
***********************************************************************
* Additional comments about this program.
*
* No additional comments.
*
***********************************************************************
*
         END   RPI2009B
