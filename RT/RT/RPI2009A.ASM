         TITLE 'Test Add Instructions'
***********************************************************************
* Start create : 03-09-2017
* 1st delivery :
* Designer     : J Ganci
* Programmer   : J Ganci
* Purpose      : Test Add instructions that can generate
*                fixed-point-overflow exceptions
***********************************************************************
* The add instructions that can generate fixed-point overflow:
*
* Mnemonic  Format
* A         RX-a
* AR        RR
* ARK       RRF-a
* AY        RXY-a
* AG        RXY-a
* AGR       RRE
* AGRK      RRF-a
* AGF       RXY-a
* AGFR      RRE
* AH        RX-a
* AHY       RXY-a
* AHI       RI-a
* AGHI      RI-a
* AHHHR     RRF-a
* AHHLR     RRF-a
* AFI       RIL-a
* AHIK      RIE-d
* ASI       SIY
* AGHIK     RIE-d
* AGFI      RIL-a
* AGSI      SIY
* AIH       RIL-a
* LAA       RSY-a
* LAAG      RSY-a
*
***********************************************************************
*
***********************************************************************
*
* Define constants, DSECTs, and environment
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
* Equated values used for add immediate instructions
*
***********************************************************************
*
SHW_MAX  EQU   32767               Maximum signed halfword value
SHW_MIN  EQU   -32768              Minimum signed halfword value
SFW_MAX  EQU   2147483647          Maximum signed fullword value
SFW_MIN  EQU   -2147483648         Minimum signed fullword value
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
* If a PARM value is entered, it must be one of the Add instruction
* mnemonics. In that case, only that instruction is tested.
*
* Examples:
*
*   Run all tests:
*
*     "exec <path>/RPI2009A noloadhigh nocodepage test(RT)"
*
*   Run test set for AR:
*
*      "exec <path>/RPI2009A noloadhigh nocodepage parm(AR) test(RT)"
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
RPI2009A CSECT
RPI2009A AMODE 31
RPI2009A RMODE 31
         B     RPI20020-RPI2009A(,R15) Branch around header
         DC    AL1(RPI20020-*),C'RPI2009A:&SYSDATC:&SYSTIME'
RPI20020 DS    0H
         STM   R14,R12,12(R13)         Save caller's registers
         LR    R12,R15                 R12 = base register
         USING RPI2009A,R12            Establish addressability
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
         MVC   WAEYE,=CL8'RPI2009A'    Set eyecatcher
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
         SGR   R9,R9               Initialize last test pointer
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
FoundTst DS    0H                  Found test entry for mnemonic
***********************************************************************
*        Do one test (two parts: CC and overflow)
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
         WTO   MF=(E,WTO1)         Issue WTO indicating test
         LT    R15,TE@CC           Test routine
         BNP   Skip1CC             No routine; skip
         BASR  R14,R15             Do the test
Skip1CC  DS    0H
*
*        Part 2: CC=3 overflow
*
         MVC   W2Mne,0(R7)         Copy instruction mnemonic
         WTO   MF=(E,WTO2)         Issue WTO indicating test
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
         LR    R15,R10                     Restore R15
         BASR  R14,R15                     Do the test
*
         ESPIE RESET  ,TOKEN=ESTOKEN       Cancel ESPIE
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
         WTO   MF=(E,WTO1)         Issue WTO indicating test
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
         WTO   MF=(E,WTO2)         Issue WTO indicating test
         LT    R15,TE@OV           Test routine
         BNP   SkipAOV             No routine; skip
         BASR  R14,R15             Do the test
SkipAOV  DS    0H
         BXLE  R7,R8,LoopAOV       Do all tests
*
         ESPIE RESET  ,TOKEN=ESTOKEN   Cancel ESPIE
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
CopyValue MVC  0(*-*,R1),2(R2)     Copy test name
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
* Do not modify R7, R8, R9, R11, or R13
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
*        A instruction test
***********************************************************************
*
* 5A = A - Add
*
* operand1 is 32-bit signed value
* operand2 is 32-bit signed value
*
***********************************************************************
*
TA1      DS    0H
         USING *,R15               Establish addressability
***********************************************************************
*
*        Test 1.1.0.1: zero + zero = zero; CC=0
*
         SR    R0,R0               Set CC
         TM    *,X'80'             Set CC=3
         LMG   R0,R1,TST_AR_ZZZ0   Initialize registers 0 and 1
         ST    R1,FW               Value to add
         A     R0,FW               CC=0
         IPM   R3                  Fetch CC
*
*        Test 1.1.0.2: pos + neg = zero; CC=0
*
         SR    R0,R0               Set CC
         TM    *,X'80'             Set CC=3
         LMG   R0,R1,TST_AR_PNZ0   Initialize registers 0 and 1
         ST    R1,FW               Value to add
         A     R0,FW               CC=0
         IPM   R3                  Fetch CC
*
*        Test 1.1.0.3: neg + pos = zero; CC=0
*
         SR    R0,R0               Set CC
         TM    *,X'80'             Set CC=3
         LMG   R0,R1,TST_AR_NPZ0   Initialize registers 0 and 1
         ST    R1,FW               Value to add
         A     R0,FW               CC=0
         IPM   R3                  Fetch CC
***********************************************************************
*
*        Test 1.1.1.1: zero + neg = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_ZNN1   Initialize registers 0 and 1
         ST    R1,FW               Value to add
         A     R0,FW               CC=1
         IPM   R3                  Fetch CC
*
*        Test 1.1.1.2: pos + neg = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_PNN1   Initialize registers 0 and 1
         ST    R1,FW               Value to add
         A     R0,FW               CC=1
         IPM   R3                  Fetch CC
*
*        Test 1.1.1.3: neg + zero = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_NZN1   Initialize registers 0 and 1
         ST    R1,FW               Value to add
         A     R0,FW               CC=1
         IPM   R3                  Fetch CC
*
*        Test 1.1.1.4: neg + pos = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_NPN1   Initialize registers 0 and 1
         ST    R1,FW               Value to add
         A     R0,FW               CC=1
         IPM   R3                  Fetch CC
*
*        Test 1.1.1.5: neg + neg = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_NNN1   Initialize registers 0 and 1
         ST    R1,FW               Value to add
         A     R0,FW               CC=1
         IPM   R3                  Fetch CC
***********************************************************************
*
*        Test 1.1.2.1: zero + pos = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_ZPP2   Initialize registers 0 and 1
         ST    R1,FW               Value to add
         A     R0,FW               CC=2
         IPM   R3                  Fetch CC
*
*        Test 1.1.2.2: pos + zero = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_PZP2   Initialize registers 0 and 1
         ST    R1,FW               Value to add
         A     R0,FW               CC=2
         IPM   R3                  Fetch CC
*
*        Test 1.1.2.3: pos + pos = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_PPP2   Initialize registers 0 and 1
         ST    R1,FW               Value to add
         A     R0,FW               CC=2
         IPM   R3                  Fetch CC
*
*        Test 1.1.2.4: pos + neg = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_PNP2   Initialize registers 0 and 1
         ST    R1,FW               Value to add
         A     R0,FW               CC=2
         IPM   R3                  Fetch CC
*
*        Test 1.1.2.5: neg + pos = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_NPP2   Initialize registers 0 and 1
         ST    R1,FW               Value to add
         A     R0,FW               CC=2
         IPM   R3                  Fetch CC
***********************************************************************
*
*        Test 1.1.3.1: pos + pos = neg; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_PPN3   Initialize registers 0 and 1
         ST    R1,FW               Value to add
         A     R0,FW               CC=3
         IPM   R3                  Fetch CC
*
*        Test 1.1.3.2: neg + neg = zero; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_NNZ3   Initialize registers 0 and 1
         ST    R1,FW               Value to add
         A     R0,FW               CC=3
         IPM   R3                  Fetch CC
*
*        Test 1.1.3.3: neg + neg = pos; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_NNP3   Initialize registers 0 and 1
         ST    R1,FW               Value to add
         A     R0,FW               CC=3
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
*        AR instruction test
***********************************************************************
*
* 1A = AR - Add
*
* operand1 is 32-bit signed value
* operand2 is 32-bit signed value
*
***********************************************************************
*
TAR1     DS    0H
         USING *,R15               Establish addressability
***********************************************************************
*
*        Test 1.2.0.1: zero + zero = zero; CC=0
*
         SR    R0,R0               Set CC
         TM    *,X'80'             Set CC=3
         LMG   R0,R1,TST_AR_ZZZ0   Initialize registers 0 and 1
         AR    R0,R1               CC=0
         IPM   R3                  Fetch CC
*
*        Test 1.2.0.2: pos + neg = zero; CC=0
*
         SR    R0,R0               Set CC
         TM    *,X'80'             Set CC=3
         LMG   R0,R1,TST_AR_PNZ0   Initialize registers 0 and 1
         AR    R0,R1               CC=0
         IPM   R3                  Fetch CC
*
*        Test 1.2.0.3: neg + pos = zero; CC=0
*
         SR    R0,R0               Set CC
         TM    *,X'80'             Set CC=3
         LMG   R0,R1,TST_AR_NPZ0   Initialize registers 0 and 1
         AR    R0,R1               CC=0
         IPM   R3                  Fetch CC
***********************************************************************
*
*        Test 1.2.1.1: zero + neg = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_ZNN1   Initialize registers 0 and 1
         AR    R0,R1               CC=1
         IPM   R3                  Fetch CC
*
*        Test 1.2.1.2: pos + neg = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_PNN1   Initialize registers 0 and 1
         AR    R0,R1               CC=1
         IPM   R3                  Fetch CC
*
*        Test 1.2.1.3: neg + zero = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_NZN1   Initialize registers 0 and 1
         AR    R0,R1               CC=1
         IPM   R3                  Fetch CC
*
*        Test 1.2.1.4: neg + pos = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_NPN1   Initialize registers 0 and 1
         AR    R0,R1               CC=1
         IPM   R3                  Fetch CC
*
*        Test 1.2.1.5: neg + neg = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_NNN1   Initialize registers 0 and 1
         AR    R0,R1               CC=1
         IPM   R3                  Fetch CC
***********************************************************************
*
*        Test 1.2.2.1: zero + pos = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_ZPP2   Initialize registers 0 and 1
         AR    R0,R1               CC=2
         IPM   R3                  Fetch CC
*
*        Test 1.2.2.2: pos + zero = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_PZP2   Initialize registers 0 and 1
         AR    R0,R1               CC=2
         IPM   R3                  Fetch CC
*
*        Test 1.2.2.3: pos + pos = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_PPP2   Initialize registers 0 and 1
         AR    R0,R1               CC=2
         IPM   R3                  Fetch CC
*
*        Test 1.2.2.4: pos + neg = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_PNP2   Initialize registers 0 and 1
         AR    R0,R1               CC=2
         IPM   R3                  Fetch CC
*
*        Test 1.2.2.5: neg + pos = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_NPP2   Initialize registers 0 and 1
         AR    R0,R1               CC=2
         IPM   R3                  Fetch CC
***********************************************************************
*
*        Test 1.2.3.1: pos + pos = neg; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_PPN3   Initialize registers 0 and 1
         AR    R0,R1               CC=3
         IPM   R3                  Fetch CC
*
*        Test 1.2.3.2: neg + neg = zero; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_NNZ3   Initialize registers 0 and 1
         AR    R0,R1               CC=3
         IPM   R3                  Fetch CC
*
*        Test 1.2.3.3: neg + neg = pos; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_NNP3   Initialize registers 0 and 1
         AR    R0,R1               CC=3
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
*        ARK instruction test
***********************************************************************
*
* B9F8 = ARK - Add
*
* operand1 is 32-bit signed value
* operand2 is 32-bit signed value
* operand3 is 32-bit signed value
*
***********************************************************************
*
TARK1    DS    0H
         USING *,R15               Establish addressability
***********************************************************************
*
*        Test 1.3.0.1: zero + zero = zero; CC=0
*
         SR    R0,R0               Set CC
         TM    *,X'80'             Set CC=3
         LMG   R0,R1,TST_AR_ZZZ0   Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         ARK   R2,R1,R0            CC=0
         IPM   R3                  Fetch CC
*
*        Test 1.3.0.2: pos + neg = zero; CC=0
*
         SR    R0,R0               Set CC
         TM    *,X'80'             Set CC=3
         LMG   R0,R1,TST_AR_PNZ0   Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         ARK   R2,R1,R0            CC=0
         IPM   R3                  Fetch CC
*
*        Test 1.3.0.3: neg + pos = zero; CC=0
*
         SR    R0,R0               Set CC
         TM    *,X'80'             Set CC=3
         LMG   R0,R1,TST_AR_NPZ0   Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         ARK   R2,R1,R0            CC=0
         IPM   R3                  Fetch CC
***********************************************************************
*
*        Test 1.3.1.1: zero + neg = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_ZNN1   Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         ARK   R2,R1,R0            CC=1
         IPM   R3                  Fetch CC
*
*        Test 1.3.1.2: pos + neg = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_PNN1   Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         ARK   R2,R1,R0            CC=1
         IPM   R3                  Fetch CC
*
*        Test 1.3.1.3: neg + zero = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_NZN1   Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         ARK   R2,R1,R0            CC=1
         IPM   R3                  Fetch CC
*
*        Test 1.3.1.4: neg + pos = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_NPN1   Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         ARK   R2,R1,R0            CC=1
         IPM   R3                  Fetch CC
*
*        Test 1.3.1.5: neg + neg = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_NNN1   Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         ARK   R2,R1,R0            CC=1
         IPM   R3                  Fetch CC
***********************************************************************
*
*        Test 1.3.2.1: zero + pos = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_ZPP2   Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         ARK   R2,R1,R0            CC=2
         IPM   R3                  Fetch CC
*
*        Test 1.3.2.2: pos + zero = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_PZP2   Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         ARK   R2,R1,R0            CC=2
         IPM   R3                  Fetch CC
*
*        Test 1.3.2.3: pos + pos = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_PPP2   Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         ARK   R2,R1,R0            CC=2
         IPM   R3                  Fetch CC
*
*        Test 1.3.2.4: pos + neg = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_PNP2   Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         ARK   R2,R1,R0            CC=2
         IPM   R3                  Fetch CC
*
*        Test 1.3.2.5: neg + pos = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_NPP2   Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         ARK   R2,R1,R0            CC=2
         IPM   R3                  Fetch CC
***********************************************************************
*
*        Test 1.3.3.1: pos + pos = neg; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_PPN3   Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         ARK   R2,R1,R0            CC=3
         IPM   R3                  Fetch CC
*
*        Test 1.3.3.2: neg + neg = zero; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_NNZ3   Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         ARK   R2,R1,R0            CC=3
         IPM   R3                  Fetch CC
*
*        Test 1.3.3.3: neg + neg = pos; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_NNP3   Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         ARK   R2,R1,R0            CC=3
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
*        AY instruction test
***********************************************************************
*
* E35A = AY - Add
*
* operand1 is 32-bit signed value
* operand2 is 32-bit signed value
*
***********************************************************************
*
TAY1     DS    0H
         USING *,R15               Establish addressability
***********************************************************************
*
*        Test 1.4.0.1: zero + zero = zero; CC=0
*
         SR    R0,R0               Set CC
         TM    *,X'80'             Set CC=3
         LMG   R0,R1,TST_AR_ZZZ0   Initialize registers 0 and 1
         ST    R1,FW               Value to add
         AY    R0,FW               CC=0
         IPM   R3                  Fetch CC
*
*        Test 1.4.0.2: pos + neg = zero; CC=0
*
         SR    R0,R0               Set CC
         TM    *,X'80'             Set CC=3
         LMG   R0,R1,TST_AR_PNZ0   Initialize registers 0 and 1
         ST    R1,FW               Value to add
         AY    R0,FW               CC=0
         IPM   R3                  Fetch CC
*
*        Test 1.4.0.3: neg + pos = zero; CC=0
*
         SR    R0,R0               Set CC
         TM    *,X'80'             Set CC=3
         LMG   R0,R1,TST_AR_NPZ0   Initialize registers 0 and 1
         ST    R1,FW               Value to add
         AY    R0,FW               CC=0
         IPM   R3                  Fetch CC
***********************************************************************
*
*        Test 1.4.1.1: zero + neg = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_ZNN1   Initialize registers 0 and 1
         ST    R1,FW               Value to add
         AY    R0,FW               CC=1
         IPM   R3                  Fetch CC
*
*        Test 1.4.1.2: pos + neg = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_PNN1   Initialize registers 0 and 1
         ST    R1,FW               Value to add
         AY    R0,FW               CC=1
         IPM   R3                  Fetch CC
*
*        Test 1.4.1.3: neg + zero = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_NZN1   Initialize registers 0 and 1
         ST    R1,FW               Value to add
         AY    R0,FW               CC=1
         IPM   R3                  Fetch CC
*
*        Test 1.4.1.4: neg + pos = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_NPN1   Initialize registers 0 and 1
         ST    R1,FW               Value to add
         AY    R0,FW               CC=1
         IPM   R3                  Fetch CC
*
*        Test 1.4.1.5: neg + neg = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_NNN1   Initialize registers 0 and 1
         ST    R1,FW               Value to add
         AY    R0,FW               CC=1
         IPM   R3                  Fetch CC
***********************************************************************
*
*        Test 1.4.2.1: zero + pos = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_ZPP2   Initialize registers 0 and 1
         ST    R1,FW               Value to add
         AY    R0,FW               CC=2
         IPM   R3                  Fetch CC
*
*        Test 1.4.2.2: pos + zero = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_PZP2   Initialize registers 0 and 1
         ST    R1,FW               Value to add
         AY    R0,FW               CC=2
         IPM   R3                  Fetch CC
*
*        Test 1.4.2.3: pos + pos = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_PPP2   Initialize registers 0 and 1
         ST    R1,FW               Value to add
         AY    R0,FW               CC=2
         IPM   R3                  Fetch CC
*
*        Test 1.4.2.4: pos + neg = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_PNP2   Initialize registers 0 and 1
         ST    R1,FW               Value to add
         AY    R0,FW               CC=2
         IPM   R3                  Fetch CC
*
*        Test 1.4.2.5: neg + pos = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_NPP2   Initialize registers 0 and 1
         ST    R1,FW               Value to add
         AY    R0,FW               CC=2
         IPM   R3                  Fetch CC
***********************************************************************
*
*        Test 1.4.3.1: pos + pos = neg; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_PPN3   Initialize registers 0 and 1
         ST    R1,FW               Value to add
         AY    R0,FW               CC=3
         IPM   R3                  Fetch CC
*
*        Test 1.4.3.2: neg + neg = zero; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_NNZ3   Initialize registers 0 and 1
         ST    R1,FW               Value to add
         AY    R0,FW               CC=3
         IPM   R3                  Fetch CC
*
*        Test 1.4.3.3: neg + neg = pos; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_NNP3   Initialize registers 0 and 1
         ST    R1,FW               Value to add
         AY    R0,FW               CC=3
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
*        AG instruction test
***********************************************************************
*
* E308 = AG - Add
*
* operand1 is 64-bit signed value
* operand2 is 64-bit signed value
*
***********************************************************************
*
TAG1     DS    0H
         USING *,R15               Establish addressability
***********************************************************************
*
*        Test 1.5.0.1: zero + zero = zero; CC=0
*
         SR    R0,R0               Set CC
         TM    *,X'80'             Set CC=3
         LMG   R0,R1,TST_AGR_ZZZ0  Initialize registers 0 and 1
         STG   R1,DW               Value to add
         AG    R0,DW               CC=0
         IPM   R3                  Fetch CC
*
*        Test 1.5.0.2: pos + neg = zero; CC=0
*
         SR    R0,R0               Set CC
         TM    *,X'80'             Set CC=3
         LMG   R0,R1,TST_AGR_PNZ0  Initialize registers 0 and 1
         STG   R1,DW               Value to add
         AG    R0,DW               CC=0
         IPM   R3                  Fetch CC
*
*        Test 1.5.0.3: neg + pos = zero; CC=0
*
         SR    R0,R0               Set CC
         TM    *,X'80'             Set CC=3
         LMG   R0,R1,TST_AGR_NPZ0  Initialize registers 0 and 1
         STG   R1,DW               Value to add
         AG    R0,DW               CC=0
         IPM   R3                  Fetch CC
***********************************************************************
*
*        Test 1.5.1.1: zero + neg = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AGR_ZNN1  Initialize registers 0 and 1
         STG   R1,DW               Value to add
         AG    R0,DW               CC=1
         IPM   R3                  Fetch CC
*
*        Test 1.5.1.2: pos + neg = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AGR_PNN1  Initialize registers 0 and 1
         STG   R1,DW               Value to add
         AG    R0,DW               CC=1
         IPM   R3                  Fetch CC
*
*        Test 1.5.1.3: neg + zero = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AGR_NZN1  Initialize registers 0 and 1
         STG   R1,DW               Value to add
         AG    R0,DW               CC=1
         IPM   R3                  Fetch CC
*
*        Test 1.5.1.4: neg + pos = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AGR_NPN1  Initialize registers 0 and 1
         STG   R1,DW               Value to add
         AG    R0,DW               CC=1
         IPM   R3                  Fetch CC
*
*        Test 1.5.1.5: neg + neg = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AGR_NNN1  Initialize registers 0 and 1
         STG   R1,DW               Value to add
         AG    R0,DW               CC=1
         IPM   R3                  Fetch CC
***********************************************************************
*
*        Test 1.5.2.1: zero + pos = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AGR_ZPP2  Initialize registers 0 and 1
         STG   R1,DW               Value to add
         AG    R0,DW               CC=2
         IPM   R3                  Fetch CC
*
*        Test 1.5.2.2: pos + zero = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AGR_PZP2  Initialize registers 0 and 1
         STG   R1,DW               Value to add
         AG    R0,DW               CC=2
         IPM   R3                  Fetch CC
*
*        Test 1.5.2.3: pos + pos = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AGR_PPP2  Initialize registers 0 and 1
         STG   R1,DW               Value to add
         AG    R0,DW               CC=2
         IPM   R3                  Fetch CC
*
*        Test 1.5.2.4: pos + neg = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AGR_PNP2  Initialize registers 0 and 1
         STG   R1,DW               Value to add
         AG    R0,DW               CC=2
         IPM   R3                  Fetch CC
*
*        Test 1.5.2.5: neg + pos = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AGR_NPP2  Initialize registers 0 and 1
         STG   R1,DW               Value to add
         AG    R0,DW               CC=2
         IPM   R3                  Fetch CC
***********************************************************************
*
*        Test 1.5.3.1: pos + pos = neg; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AGR_PPN3  Initialize registers 0 and 1
         STG   R1,DW               Value to add
         AG    R0,DW               CC=3
         IPM   R3                  Fetch CC
*
*        Test 1.5.5.2: neg + neg = zero; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AGR_NNZ3  Initialize registers 0 and 1
         STG   R1,DW               Value to add
         AG    R0,DW               CC=3
         IPM   R3                  Fetch CC
*
*        Test 1.5.3.3: neg + neg = pos; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AGR_NNP3  Initialize registers 0 and 1
         STG   R1,DW               Value to add
         AG    R0,DW               CC=3
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
*        AGR instruction test
***********************************************************************
*
* B908 = AGR - Add
*
* operand1 is 64-bit signed value
* operand2 is 64-bit signed value
*
***********************************************************************
*
TAGR1    DS    0H
         USING *,R15               Establish addressability
***********************************************************************
*
*        Test 1.6.0.1: zero + zero = zero; CC=0
*
         SR    R0,R0               Set CC
         TM    *,X'80'             Set CC=3
         LMG   R0,R1,TST_AGR_ZZZ0  Initialize registers 0 and 1
         AGR   R0,R1               CC=0
         IPM   R3                  Fetch CC
*
*        Test 1.6.0.2: pos + neg = zero; CC=0
*
         SR    R0,R0               Set CC
         TM    *,X'80'             Set CC=3
         LMG   R0,R1,TST_AGR_PNZ0  Initialize registers 0 and 1
         AGR   R0,R1               CC=0
         IPM   R3                  Fetch CC
*
*        Test 1.2.0.3: neg + pos = zero; CC=0
*
         SR    R0,R0               Set CC
         TM    *,X'80'             Set CC=3
         LMG   R0,R1,TST_AGR_NPZ0  Initialize registers 0 and 1
         AGR   R0,R1               CC=0
         IPM   R3                  Fetch CC
***********************************************************************
*
*        Test 1.6.1.1: zero + neg = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AGR_ZNN1  Initialize registers 0 and 1
         AGR   R0,R1               CC=1
         IPM   R3                  Fetch CC
*
*        Test 1.6.1.2: pos + neg = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AGR_PNN1  Initialize registers 0 and 1
         AGR   R0,R1               CC=1
         IPM   R3                  Fetch CC
*
*        Test 1.6.1.3: neg + zero = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AGR_NZN1  Initialize registers 0 and 1
         AGR   R0,R1               CC=1
         IPM   R3                  Fetch CC
*
*        Test 1.6.1.4: neg + pos = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AGR_NPN1  Initialize registers 0 and 1
         AGR   R0,R1               CC=1
         IPM   R3                  Fetch CC
*
*        Test 1.6.1.5: neg + neg = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AGR_NNN1  Initialize registers 0 and 1
         AGR   R0,R1               CC=1
         IPM   R3                  Fetch CC
***********************************************************************
*
*        Test 1.6.2.1: zero + pos = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AGR_ZPP2  Initialize registers 0 and 1
         AGR   R0,R1               CC=2
         IPM   R3                  Fetch CC
*
*        Test 1.6.2.2: pos + zero = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AGR_PZP2  Initialize registers 0 and 1
         AGR   R0,R1               CC=2
         IPM   R3                  Fetch CC
*
*        Test 1.6.2.3: pos + pos = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AGR_PPP2  Initialize registers 0 and 1
         AGR   R0,R1               CC=2
         IPM   R3                  Fetch CC
*
*        Test 1.6.2.4: pos + neg = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AGR_PNP2  Initialize registers 0 and 1
         AGR   R0,R1               CC=2
         IPM   R3                  Fetch CC
*
*        Test 1.6.2.5: neg + pos = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AGR_NPP2  Initialize registers 0 and 1
         AGR   R0,R1               CC=2
         IPM   R3                  Fetch CC
***********************************************************************
*
*        Test 1.6.3.1: pos + pos = neg; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AGR_PPN3  Initialize registers 0 and 1
         AGR   R0,R1               CC=0
         IPM   R3                  Fetch CC
*
*        Test 1.6.3.2: neg + neg = zero; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AGR_NNZ3  Initialize registers 0 and 1
         AGR   R0,R1               CC=0
         IPM   R3                  Fetch CC
*
*        Test 1.6.3.3: neg + neg = pos; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AGR_NNP3  Initialize registers 0 and 1
         AGR   R0,R1               CC=0
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
*        AGRK instruction test
***********************************************************************
*
* B9E8 = AGRK - Add
*
* operand1 is 64-bit signed value
* operand2 is 64-bit signed value
* operand3 is 64-bit signed value
*
***********************************************************************
*
TAGRK1   DS    0H
         USING *,R15               Establish addressability
***********************************************************************
*
*        Test 1.7.0.1: zero + zero = zero; CC=0
*
         SR    R0,R0               Set CC
         TM    *,X'80'             Set CC=3
         LMG   R0,R1,TST_AGR_ZZZ0  Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         AGRK  R2,R1,R0            CC=0
         IPM   R3                  Fetch CC
*
*        Test 1.7.0.2: pos + neg = zero; CC=0
*
         SR    R0,R0               Set CC
         TM    *,X'80'             Set CC=3
         LMG   R0,R1,TST_AGR_PNZ0  Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         AGRK  R2,R1,R0            CC=0
         IPM   R3                  Fetch CC
*
*        Test 1.7.0.3: neg + pos = zero; CC=0
*
         SR    R0,R0               Set CC
         TM    *,X'80'             Set CC=3
         LMG   R0,R1,TST_AGR_NPZ0  Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         AGRK  R2,R1,R0            CC=0
         IPM   R3                  Fetch CC
***********************************************************************
*
*        Test 1.7.1.1: zero + neg = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AGR_ZNN1  Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         AGRK  R2,R1,R0            CC=1
         IPM   R3                  Fetch CC
*
*        Test 1.7.1.2: pos + neg = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AGR_PNN1  Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         AGRK  R2,R1,R0            CC=1
         IPM   R3                  Fetch CC
*
*        Test 1.7.1.3: neg + zero = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AGR_NZN1  Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         AGRK  R2,R1,R0            CC=1
         IPM   R3                  Fetch CC
*
*        Test 1.7.1.4: neg + pos = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AGR_NPN1  Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         AGRK  R2,R1,R0            CC=1
         IPM   R3                  Fetch CC
*
*        Test 1.7.1.5: neg + neg = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AGR_NNN1  Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         AGRK  R2,R1,R0            CC=1
         IPM   R3                  Fetch CC
***********************************************************************
*
*        Test 1.7.2.1: zero + pos = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AGR_ZPP2  Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         AGRK  R2,R1,R0            CC=2
         IPM   R3                  Fetch CC
*
*        Test 1.7.2.2: pos + zero = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AGR_PZP2  Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         AGRK  R2,R1,R0            CC=2
         IPM   R3                  Fetch CC
*
*        Test 1.7.2.3: pos + pos = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AGR_PPP2  Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         AGRK  R2,R1,R0            CC=2
         IPM   R3                  Fetch CC
*
*        Test 1.7.2.4: pos + neg = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AGR_PNP2  Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         AGRK  R2,R1,R0            CC=2
         IPM   R3                  Fetch CC
*
*        Test 1.7.2.5: neg + pos = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AGR_NPP2  Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         AGRK  R2,R1,R0            CC=2
         IPM   R3                  Fetch CC
***********************************************************************
*
*        Test 1.7.3.1: pos + pos = neg; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AGR_PPN3  Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         AGRK  R2,R1,R0            CC=3
         IPM   R3                  Fetch CC
*
*        Test 1.7.3.2: neg + neg = zero; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AGR_NNZ3  Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         AGRK  R2,R1,R0            CC=3
         IPM   R3                  Fetch CC
*
*        Test 1.7.3.3: neg + neg = pos; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AGR_NNP3  Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         AGRK  R2,R1,R0            CC=3
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
*        AGF instruction test
***********************************************************************
*
* E318 = AGF - Add
*
* operand1 is 64-bit signed value
* operand2 is 32-bit signed value
*
***********************************************************************
*
TAGF1    DS    0H
         USING *,R15               Establish addressability
***********************************************************************
*
*        Test 1.8.0.1: zero + zero = zero; CC=0
*
         SR    R0,R0               Set CC
         TM    *,X'80'             Set CC=3
         LMG   R0,R1,TST_AGR_ZZZ0  Initialize registers 0 and 1
         ST    R1,FW               Value to add
         AGF   R0,FW               CC=0
         IPM   R3                  Fetch CC
*
*        Test 1.8.0.2: pos + neg = zero; CC=0
*
         SR    R0,R0               Set CC
         TM    *,X'80'             Set CC=3
         LMG   R0,R1,TST_AGR_PNZ0  Initialize registers 0 and 1
         ST    R1,FW               Value to add
         AGF   R0,FW               CC=0
         IPM   R3                  Fetch CC
*
*        Test 1.8.0.3: neg + pos = zero; CC=0
*
         SR    R0,R0               Set CC
         TM    *,X'80'             Set CC=3
         LMG   R0,R1,TST_AGR_NPZ0  Initialize registers 0 and 1
         ST    R1,FW               Value to add
         AGF   R0,FW               CC=0
         IPM   R3                  Fetch CC
***********************************************************************
*
*        Test 1.8.1.1: zero + neg = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AGR_ZNN1  Initialize registers 0 and 1
         ST    R1,FW               Value to add
         AGF   R0,FW               CC=1
         IPM   R3                  Fetch CC
*
*        Test 1.8.1.2: pos + neg = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AGR_PNN1  Initialize registers 0 and 1
         ST    R1,FW               Value to add
         AGF   R0,FW               CC=1
         IPM   R3                  Fetch CC
*
*        Test 1.8.1.3: neg + zero = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AGR_NZN1  Initialize registers 0 and 1
         ST    R1,FW               Value to add
         AGF   R0,FW               CC=1
         IPM   R3                  Fetch CC
*
*        Test 1.8.1.4: neg + pos = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AGR_NPN1  Initialize registers 0 and 1
         ST    R1,FW               Value to add
         AGF   R0,FW               CC=1
         IPM   R3                  Fetch CC
*
*        Test 1.8.1.5: neg + neg = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AGR_NNN1  Initialize registers 0 and 1
         ST    R1,FW               Value to add
         AGF   R0,FW               CC=1
         IPM   R3                  Fetch CC
***********************************************************************
*
*        Test 1.8.2.1: zero + pos = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AGR_ZPP2  Initialize registers 0 and 1
         ST    R1,FW               Value to add
         AGF   R0,FW               CC=2
         IPM   R3                  Fetch CC
*
*        Test 1.8.2.2: pos + zero = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AGR_PZP2  Initialize registers 0 and 1
         ST    R1,FW               Value to add
         AGF   R0,FW               CC=2
         IPM   R3                  Fetch CC
*
*        Test 1.8.2.3: pos + pos = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AGR_PPP2  Initialize registers 0 and 1
         ST    R1,FW               Value to add
         AGF   R0,FW               CC=2
         IPM   R3                  Fetch CC
*
*        Test 1.8.2.4: pos + neg = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AGR_PNP2  Initialize registers 0 and 1
         ST    R1,FW               Value to add
         AGF   R0,FW               CC=2
         IPM   R3                  Fetch CC
*
*        Test 1.8.2.5: neg + pos = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AGR_NPP2  Initialize registers 0 and 1
         ST    R1,FW               Value to add
         AGF   R0,FW               CC=2
         IPM   R3                  Fetch CC
***********************************************************************
*
*        Test 1.8.3.1: pos + pos = neg; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AGR_PPN3  Initialize registers 0 and 1
         ST    R1,FW               Value to add
         AGF   R0,FW               CC=3
         IPM   R3                  Fetch CC
*
*        Test 1.8.3.2: neg + neg = zero; CC=3; cannot occur
*
***      SR    R0,R0               Set CC
***      LMG   R0,R1,TST_AGFR_NNZ3 Initialize registers 0 and 1
***      ST    R1,FW               Value to add
***      AGF   R0,FW               CC=3
***      IPM   R3                  Fetch CC
*
*        Test 1.8.3.3: neg + neg = pos; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AGR_NNP3  Initialize registers 0 and 1
         ST    R1,FW               Value to add
         AGF   R0,FW               CC=3
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
*        AGFR instruction test
***********************************************************************
*
* B918 = AGFR - Add
*
* operand1 is 64-bit signed value
* operand2 is 32-bit signed value
*
***********************************************************************
*
TAGFR1   DS    0H
         USING *,R15               Establish addressability
***********************************************************************
*
*        Test 1.9.0.1: zero + zero = zero; CC=0
*
         SR    R0,R0               Set CC
         TM    *,X'80'             Set CC=3
         LMG   R0,R1,TST_AGR_ZZZ0  Initialize registers 0 and 1
         AGFR  R0,R1               CC=0
         IPM   R3                  Fetch CC
*
*        Test 1.9.0.2: pos + neg = zero; CC=0
*
         SR    R0,R0               Set CC
         TM    *,X'80'             Set CC=3
         LMG   R0,R1,TST_AGR_PNZ0  Initialize registers 0 and 1
         AGFR  R0,R1               CC=0
         IPM   R3                  Fetch CC
*
*        Test 1.9.0.3: neg + pos = zero; CC=0
*
         SR    R0,R0               Set CC
         TM    *,X'80'             Set CC=3
         LMG   R0,R1,TST_AGR_NPZ0  Initialize registers 0 and 1
         AGFR  R0,R1               CC=0
         IPM   R3                  Fetch CC
***********************************************************************
*
*        Test 1.9.1.1: zero + neg = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AGR_ZNN1  Initialize registers 0 and 1
         AGFR  R0,R1               CC=1
         IPM   R3                  Fetch CC
*
*        Test 1.9.1.2: pos + neg = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AGR_PNN1  Initialize registers 0 and 1
         AGFR  R0,R1               CC=1
         IPM   R3                  Fetch CC
*
*        Test 1.9.1.3: neg + zero = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AGR_NZN1  Initialize registers 0 and 1
         AGFR  R0,R1               CC=1
         IPM   R3                  Fetch CC
*
*        Test 1.9.1.4: neg + pos = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AGR_NPN1  Initialize registers 0 and 1
         AGFR  R0,R1               CC=1
         IPM   R3                  Fetch CC
*
*        Test 1.9.1.5: neg + neg = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AGR_NNN1  Initialize registers 0 and 1
         AGFR  R0,R1               CC=1
         IPM   R3                  Fetch CC
***********************************************************************
*
*        Test 1.9.2.1: zero + pos = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AGR_ZPP2  Initialize registers 0 and 1
         AGFR  R0,R1               CC=2
         IPM   R3                  Fetch CC
*
*        Test 1.9.2.2: pos + zero = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AGR_PZP2  Initialize registers 0 and 1
         AGFR  R0,R1               CC=2
         IPM   R3                  Fetch CC
*
*        Test 1.9.2.3: pos + pos = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AGR_PPP2  Initialize registers 0 and 1
         AGFR  R0,R1               CC=2
         IPM   R3                  Fetch CC
*
*        Test 1.9.2.4: pos + neg = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AGR_PNP2  Initialize registers 0 and 1
         AGFR  R0,R1               CC=2
         IPM   R3                  Fetch CC
*
*        Test 1.9.2.5: neg + pos = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AGR_NPP2  Initialize registers 0 and 1
         AGFR  R0,R1               CC=2
         IPM   R3                  Fetch CC
***********************************************************************
*
*        Test 1.9.3.1: pos + pos = neg; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AGR_PPN3  Initialize registers 0 and 1
         AGFR  R0,R1               CC=0
         IPM   R3                  Fetch CC
*
*        Test 1.9.3.2: neg + neg = zero; CC=3; cannot occur
*
***      SR    R0,R0               Set CC
***      LMG   R0,R1,TST_AGFR_NNZ3 Initialize registers 0 and 1
***      AGFR  R0,R1               CC=0
***      IPM   R3                  Fetch CC
*
*        Test 1.9.3.3: neg + neg = pos; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AGR_NNP3  Initialize registers 0 and 1
         AGFR  R0,R1               CC=0
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
*        AH instruction test
***********************************************************************
*
* 4A = AH - Add Halfword
*
* operand1 is 32-bit signed value
* operand2 is 16-bit signed value
*
***********************************************************************
*
TAH1     DS    0H
         USING *,R15               Establish addressability
***********************************************************************
*
*        Test 1.10.0.1: zero + zero = zero; CC=0
*
         SR    R0,R0               Set CC
         TM    *,X'80'             Set CC=3
         LMG   R0,R1,TST_AR_ZZZ0   Initialize registers 0 and 1
         STH   R1,HW               Value to add
         AH    R0,HW               CC=0
         IPM   R3                  Fetch CC
*
*        Test 1.10.0.2: pos + neg = zero; CC=0
*
         SR    R0,R0               Set CC
         TM    *,X'80'             Set CC=3
         LMG   R0,R1,TST_AR_PNZ0   Initialize registers 0 and 1
         STH   R1,HW               Value to add
         AH    R0,HW               CC=0
         IPM   R3                  Fetch CC
*
*        Test 1.10.0.3: neg + pos = zero; CC=0
*
         SR    R0,R0               Set CC
         TM    *,X'80'             Set CC=3
         LMG   R0,R1,TST_AR_NPZ0   Initialize registers 0 and 1
         STH   R1,HW               Value to add
         AH    R0,HW               CC=0
         IPM   R3                  Fetch CC
***********************************************************************
*
*        Test 1.10.1.1: zero + neg = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_ZNN1   Initialize registers 0 and 1
         STH   R1,HW               Value to add
         AH    R0,HW               CC=1
         IPM   R3                  Fetch CC
*
*        Test 1.10.1.2: pos + neg = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_PNN1   Initialize registers 0 and 1
         STH   R1,HW               Value to add
         AH    R0,HW               CC=1
         IPM   R3                  Fetch CC
*
*        Test 1.10.1.3: neg + zero = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_NZN1   Initialize registers 0 and 1
         STH   R1,HW               Value to add
         AH    R0,HW               CC=1
         IPM   R3                  Fetch CC
*
*        Test 1.10.1.4: neg + pos = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_NPN1   Initialize registers 0 and 1
         STH   R1,HW               Value to add
         AH    R0,HW               CC=1
         IPM   R3                  Fetch CC
*
*        Test 1.10.1.5: neg + neg = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_NNN1   Initialize registers 0 and 1
         STH   R1,HW               Value to add
         AH    R0,HW               CC=1
         IPM   R3                  Fetch CC
***********************************************************************
*
*        Test 1.10.2.1: zero + pos = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_ZPP2   Initialize registers 0 and 1
         STH   R1,HW               Value to add
         AH    R0,HW               CC=2
         IPM   R3                  Fetch CC
*
*        Test 1.10.2.2: pos + zero = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_PZP2   Initialize registers 0 and 1
         STH   R1,HW               Value to add
         AH    R0,HW               CC=2
         IPM   R3                  Fetch CC
*
*        Test 1.10.2.3: pos + pos = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_PPP2   Initialize registers 0 and 1
         STH   R1,HW               Value to add
         AH    R0,HW               CC=2
         IPM   R3                  Fetch CC
*
*        Test 1.10.2.4: pos + neg = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_PNP2   Initialize registers 0 and 1
         STH   R1,HW               Value to add
         AH    R0,HW               CC=2
         IPM   R3                  Fetch CC
*
*        Test 1.10.2.5: neg + pos = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_NPP2   Initialize registers 0 and 1
         STH   R1,HW               Value to add
         AH    R0,HW               CC=2
         IPM   R3                  Fetch CC
***********************************************************************
*
*        Test 1.10.3.1: pos + pos = neg; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_PPN3   Initialize registers 0 and 1
         STH   R1,HW               Value to add
         AH    R0,HW               CC=3
         IPM   R3                  Fetch CC
*
*        Test 1.10.3.2: neg + neg = zero; CC=3; can't happen
*
***      SR    R0,R0               Set CC
***      LMG   R0,R1,TST_AR_NNZ3   Initialize registers 0 and 1
***      STH   R1,HW               Value to add
***      AH    R0,HW               CC=3
***      IPM   R3                  Fetch CC
*
*        Test 1.10.3.3: neg + neg = pos; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_NNP3   Initialize registers 0 and 1
         STH   R1,HW               Value to add
         AH    R0,HW               CC=3
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
*        AHY instruction test
***********************************************************************
*
* E37A = AY - Add Halfword
*
* operand1 is 32-bit signed value
* operand2 is 16-bit signed value
*
***********************************************************************
*
TAHY1    DS    0H
         USING *,R15               Establish addressability
***********************************************************************
*
*        Test 1.11.0.1: zero + zero = zero; CC=0
*
         SR    R0,R0               Set CC
         TM    *,X'80'             Set CC=3
         LMG   R0,R1,TST_AR_ZZZ0   Initialize registers 0 and 1
         STH   R1,HW               Value to add
         AHY   R0,HW               CC=0
         IPM   R3                  Fetch CC
*
*        Test 1.11.0.2: pos + neg = zero; CC=0
*
         SR    R0,R0               Set CC
         TM    *,X'80'             Set CC=3
         LMG   R0,R1,TST_AR_PNZ0   Initialize registers 0 and 1
         STH   R1,HW               Value to add
         AHY   R0,HW               CC=0
         IPM   R3                  Fetch CC
*
*        Test 1.11.0.3: neg + pos = zero; CC=0
*
         SR    R0,R0               Set CC
         TM    *,X'80'             Set CC=3
         LMG   R0,R1,TST_AR_NPZ0   Initialize registers 0 and 1
         STH   R1,HW               Value to add
         AHY   R0,HW               CC=0
         IPM   R3                  Fetch CC
***********************************************************************
*
*        Test 1.11.1.1: zero + neg = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_ZNN1   Initialize registers 0 and 1
         STH   R1,HW               Value to add
         AHY   R0,HW               CC=1
         IPM   R3                  Fetch CC
*
*        Test 1.11.1.2: pos + neg = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_PNN1   Initialize registers 0 and 1
         STH   R1,HW               Value to add
         AHY   R0,HW               CC=1
         IPM   R3                  Fetch CC
*
*        Test 1.11.1.3: neg + zero = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_NZN1   Initialize registers 0 and 1
         STH   R1,HW               Value to add
         AHY   R0,HW               CC=1
         IPM   R3                  Fetch CC
*
*        Test 1.11.1.4: neg + pos = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_NPN1   Initialize registers 0 and 1
         STH   R1,HW               Value to add
         AHY   R0,HW               CC=1
         IPM   R3                  Fetch CC
*
*        Test 1.11.1.5: neg + neg = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_NNN1   Initialize registers 0 and 1
         STH   R1,HW               Value to add
         AHY   R0,HW               CC=1
         IPM   R3                  Fetch CC
***********************************************************************
*
*        Test 1.11.2.1: zero + pos = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_ZPP2   Initialize registers 0 and 1
         STH   R1,HW               Value to add
         AHY   R0,HW               CC=2
         IPM   R3                  Fetch CC
*
*        Test 1.11.2.2: pos + zero = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_PZP2   Initialize registers 0 and 1
         STH   R1,HW               Value to add
         AHY   R0,HW               CC=2
         IPM   R3                  Fetch CC
*
*        Test 1.11.2.3: pos + pos = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_PPP2   Initialize registers 0 and 1
         STH   R1,HW               Value to add
         AHY   R0,HW               CC=2
         IPM   R3                  Fetch CC
*
*        Test 1.11.2.4: pos + neg = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_PNP2   Initialize registers 0 and 1
         STH   R1,HW               Value to add
         AHY   R0,HW               CC=2
         IPM   R3                  Fetch CC
*
*        Test 1.11.2.5: neg + pos = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_NPP2   Initialize registers 0 and 1
         STH   R1,HW               Value to add
         AHY   R0,HW               CC=2
         IPM   R3                  Fetch CC
***********************************************************************
*
*        Test 1.11.3.1: pos + pos = neg; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_PPN3   Initialize registers 0 and 1
         STH   R1,HW               Value to add
         AHY   R0,HW               CC=3
         IPM   R3                  Fetch CC
*
*        Test 1.11.3.2: neg + neg = zero; CC=3; can't happen
*
***      SR    R0,R0               Set CC
***      LMG   R0,R1,TST_AR_NNZ3   Initialize registers 0 and 1
***      STH   R1,HW               Value to add
***      AHY   R0,HW               CC=3
***      IPM   R3                  Fetch CC
*
*        Test 1.11.3.3: neg + neg = pos; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_NNP3   Initialize registers 0 and 1
         STH   R1,HW               Value to add
         AHY   R0,HW               CC=3
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
*        AHI instruction test
***********************************************************************
*
* A7A = AHI - Add Halfword Immediate
*
* operand1 is 32-bit signed value
* operand2 is 16-bit signed value
*
***********************************************************************
*
TAHI1    DS    0H
         USING *,R15               Establish addressability
***********************************************************************
*
*        Test 1.12.0.1: zero + zero = zero; CC=0
*
         SR    R0,R0               Set CC
         TM    *,X'80'             Set CC=3
         LMG   R0,R1,TST_AR_ZZZ0   Initialize registers 0 and 1
         AHI   R0,0                CC=0
         IPM   R3                  Fetch CC
*
*        Test 1.12.0.2: pos + neg = zero; CC=0
*
         SR    R0,R0               Set CC
         TM    *,X'80'             Set CC=3
         LMG   R0,R1,TST_AR_PNZ0   Initialize registers 0 and 1
         AHI   R0,-1               CC=0
         IPM   R3                  Fetch CC
*
*        Test 1.12.0.3: neg + pos = zero; CC=0
*
         SR    R0,R0               Set CC
         TM    *,X'80'             Set CC=3
         LMG   R0,R1,TST_AR_NPZ0   Initialize registers 0 and 1
         AHI   R0,1                CC=0
         IPM   R3                  Fetch CC
***********************************************************************
*
*        Test 1.12.1.1: zero + neg = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_ZNN1   Initialize registers 0 and 1
         AHI   R0,-1               CC=1
         IPM   R3                  Fetch CC
*
*        Test 1.12.1.2: pos + neg = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_PNN1   Initialize registers 0 and 1
         AHI   R0,-2               CC=1
         IPM   R3                  Fetch CC
*
*        Test 1.12.1.3: neg + zero = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_NZN1   Initialize registers 0 and 1
         AHI   R0,0                CC=1
         IPM   R3                  Fetch CC
*
*        Test 1.12.1.4: neg + pos = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_NPN1   Initialize registers 0 and 1
         AHI   R0,1                CC=1
         IPM   R3                  Fetch CC
*
*        Test 1.12.1.5: neg + neg = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_NNN1   Initialize registers 0 and 1
         AHI   R0,-2               CC=1
         IPM   R3                  Fetch CC
***********************************************************************
*
*        Test 1.12.2.1: zero + pos = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_ZPP2   Initialize registers 0 and 1
         AHI   R0,1                CC=2
         IPM   R3                  Fetch CC
*
*        Test 1.12.2.2: pos + zero = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_PZP2   Initialize registers 0 and 1
         AHI   R0,0                CC=2
         IPM   R3                  Fetch CC
*
*        Test 1.12.2.3: pos + pos = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_PPP2   Initialize registers 0 and 1
         AHI   R0,2                CC=2
         IPM   R3                  Fetch CC
*
*        Test 1.12.2.4: pos + neg = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_PNP2   Initialize registers 0 and 1
         AHI   R0,-1               CC=2
         IPM   R3                  Fetch CC
*
*        Test 1.12.2.5: neg + pos = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_NPP2   Initialize registers 0 and 1
         AHI   R0,2                CC=2
         IPM   R3                  Fetch CC
***********************************************************************
*
*        Test 1.12.3.1: pos + pos = neg; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_PPN3   Initialize registers 0 and 1
         AHI   R0,1                CC=3
         IPM   R3                  Fetch CC
*
*        Test 1.12.3.2: neg + neg = zero; CC=3; cannot occur
*
***      SR    R0,R0               Set CC
***      LMG   R0,R1,TST_AR_NNZ3   Initialize registers 0 and 1
***      AHI   R0,SHW_MIN          CC=3
***      IPM   R3                  Fetch CC
*
*        Test 1.12.3.3: neg + neg = pos; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_NNP3   Initialize registers 0 and 1
         AHI   R0,-2               CC=3
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
*        AGHI instruction test
***********************************************************************
*
* A7B = AGHI - Add Halfword Immediate
*
* operand1 is 64-bit signed value
* operand2 is 16-bit signed value
*
***********************************************************************
*
TAGHI1   DS    0H
         USING *,R15               Establish addressability
*
*        Test 1.13.0.1: zero + zero = zero; CC=0
*
         SR    R0,R0               Set CC
         TM    *,X'80'             Set CC=3
         LMG   R0,R1,TST_AGR_ZZZ0  Initialize registers 0 and 1
         AGHI  R0,0                CC=0
         IPM   R3                  Fetch CC
*
*        Test 1.13.0.2: pos + neg = zero; CC=0
*
         SR    R0,R0               Set CC
         TM    *,X'80'             Set CC=3
         LMG   R0,R1,TST_AGR_PNZ0  Initialize registers 0 and 1
         AGHI  R0,-1               CC=0
         IPM   R3                  Fetch CC
*
*        Test 1.13.0.3: neg + pos = zero; CC=0
*
         SR    R0,R0               Set CC
         TM    *,X'80'             Set CC=3
         LMG   R0,R1,TST_AGR_NPZ0  Initialize registers 0 and 1
         AGHI  R0,1                CC=0
         IPM   R3                  Fetch CC
***********************************************************************
*
*        Test 1.13.1.1: zero + neg = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AGR_ZNN1  Initialize registers 0 and 1
         AGHI  R0,-1               CC=1
         IPM   R3                  Fetch CC
*
*        Test 1.13.1.2: pos + neg = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AGR_PNN1  Initialize registers 0 and 1
         AGHI  R0,-2               CC=1
         IPM   R3                  Fetch CC
*
*        Test 1.13.1.3: neg + zero = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AGR_NZN1  Initialize registers 0 and 1
         AGHI  R0,0                CC=1
         IPM   R3                  Fetch CC
*
*        Test 1.13.1.4: neg + pos = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AGR_NPN1  Initialize registers 0 and 1
         AGHI  R0,1                CC=1
         IPM   R3                  Fetch CC
*
*        Test 1.13.1.5: neg + neg = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AGR_NNN1  Initialize registers 0 and 1
         AGHI  R0,-2               CC=1
         IPM   R3                  Fetch CC
***********************************************************************
*
*        Test 1.13.2.1: zero + pos = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AGR_ZPP2  Initialize registers 0 and 1
         AGHI  R0,1                CC=2
         IPM   R3                  Fetch CC
*
*        Test 1.13.2.2: pos + zero = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AGR_PZP2  Initialize registers 0 and 1
         AGHI  R0,0                CC=2
         IPM   R3                  Fetch CC
*
*        Test 1.13.2.3: pos + pos = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AGR_PPP2  Initialize registers 0 and 1
         AGHI  R0,2                CC=2
         IPM   R3                  Fetch CC
*
*        Test 1.13.2.4: pos + neg = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AGR_PNP2  Initialize registers 0 and 1
         AGHI  R0,-1               CC=2
         IPM   R3                  Fetch CC
*
*        Test 1.13.2.5: neg + pos = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AGR_NPP2  Initialize registers 0 and 1
         AGHI  R0,2                CC=2
         IPM   R3                  Fetch CC
***********************************************************************
*
*        Test 1.13.3.1: pos + pos = neg; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AGR_PPN3  Initialize registers 0 and 1
         AGHI  R0,1                CC=3
         IPM   R3                  Fetch CC
*
*        Test 1.13.3.2: neg + neg = zero; CC=3; cannot occur
*
***      SR    R0,R0               Set CC
***      LMG   R0,R1,TST_AGR_NNZ3  Initialize registers 0 and 1
***      AGHI  R0,SHW_MIN          CC=3
***      IPM   R3                  Fetch CC
*
*        Test 1.13.3.3: neg + neg = pos; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AGR_NNP3  Initialize registers 0 and 1
         AGHI  R0,-2               CC=3
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
*        AHHHR instruction test
***********************************************************************
*
* B9C8 = AHHHR - Add High
*
* operand1 is 32-bit signed value in high half of register
* operand2 is 32-bit signed value in high half of register
* operand3 is 32-bit signed value in high half of register
*
***********************************************************************
*
TAHHHR1  DS    0H
         USING *,R15               Establish addressability
***********************************************************************
*
*        Test 1.14.0.1: zero + zero = zero; CC=0
*
         SR    R0,R0               Set CC
         TM    *,X'80'             Set CC=3
         LMG   R0,R1,TST_AHHHR_ZZZ0 Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         AHHHR R2,R1,R0            CC=0
         IPM   R3                  Fetch CC
*
*        Test 1.14.0.2: pos + neg = zero; CC=0
*
         SR    R0,R0               Set CC
         TM    *,X'80'             Set CC=3
         LMG   R0,R1,TST_AHHHR_PNZ0 Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         AHHHR R2,R1,R0            CC=0
         IPM   R3                  Fetch CC
*
*        Test 1.14.0.3: neg + pos = zero; CC=0
*
         SR    R0,R0               Set CC
         TM    *,X'80'             Set CC=3
         LMG   R0,R1,TST_AHHHR_NPZ0 Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         AHHHR R2,R1,R0            CC=0
         IPM   R3                  Fetch CC
***********************************************************************
*
*        Test 1.14.1.1: zero + neg = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AHHHR_ZNN1 Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         AHHHR R2,R1,R0            CC=1
         IPM   R3                  Fetch CC
*
*        Test 1.14.1.2: pos + neg = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AHHHR_PNN1 Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         AHHHR R2,R1,R0            CC=1
         IPM   R3                  Fetch CC
*
*        Test 1.14.1.3: neg + zero = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AHHHR_NZN1 Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         AHHHR R2,R1,R0            CC=1
         IPM   R3                  Fetch CC
*
*        Test 1.14.1.4: neg + pos = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AHHHR_NPN1 Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         AHHHR R2,R1,R0            CC=1
         IPM   R3                  Fetch CC
*
*        Test 1.14.1.5: neg + neg = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AHHHR_NNN1 Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         AHHHR R2,R1,R0            CC=1
         IPM   R3                  Fetch CC
***********************************************************************
*
*        Test 1.14.2.1: zero + pos = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AHHHR_ZPP2 Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         AHHHR R2,R1,R0            CC=2
         IPM   R3                  Fetch CC
*
*        Test 1.14.2.2: pos + zero = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AHHHR_PZP2 Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         AHHHR R2,R1,R0            CC=2
         IPM   R3                  Fetch CC
*
*        Test 1.14.2.3: pos + pos = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AHHHR_PPP2 Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         AHHHR R2,R1,R0            CC=2
         IPM   R3                  Fetch CC
*
*        Test 1.14.2.4: pos + neg = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AHHHR_PNP2 Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         AHHHR R2,R1,R0            CC=2
         IPM   R3                  Fetch CC
*
*        Test 1.14.2.5: neg + pos = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AHHHR_NPP2 Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         AHHHR R2,R1,R0            CC=2
         IPM   R3                  Fetch CC
***********************************************************************
*
*        Test 1.14.3.1: pos + pos = neg; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AHHHR_PPN3 Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         AHHHR R2,R1,R0            CC=3
         IPM   R3                  Fetch CC
*
*        Test 1.14.3.2: neg + neg = zero; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AHHHR_NNZ3 Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         AHHHR R2,R1,R0            CC=3
         IPM   R3                  Fetch CC
*
*        Test 1.14.3.3: neg + neg = pos; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AHHHR_NNP3 Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         AHHHR R2,R1,R0            CC=3
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
*        AHHLR instruction test
***********************************************************************
*
* B9D8 = AHHLR - Add High
*
* operand1 is 32-bit signed value in high half of register
* operand2 is 32-bit signed value in high half of register
* operand3 is 32-bit signed value in low half of register
*
***********************************************************************
*
TAHHLR1  DS    0H
         USING *,R15               Establish addressability
***********************************************************************
*
*        Test 1.15.0.1: zero + zero = zero; CC=0
*
         SR    R0,R0               Set CC
         TM    *,X'80'             Set CC=3
         LMG   R0,R1,TST_AHHLR_ZZZ0 Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         AHHLR R2,R1,R0            CC=0
         IPM   R3                  Fetch CC
*
*        Test 1.15.0.2: pos + neg = zero; CC=0
*
         SR    R0,R0               Set CC
         TM    *,X'80'             Set CC=3
         LMG   R0,R1,TST_AHHLR_PNZ0 Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         AHHLR R2,R1,R0            CC=0
         IPM   R3                  Fetch CC
*
*        Test 1.15.0.3: neg + pos = zero; CC=0
*
         SR    R0,R0               Set CC
         TM    *,X'80'             Set CC=3
         LMG   R0,R1,TST_AHHLR_NPZ0 Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         AHHLR R2,R1,R0            CC=0
         IPM   R3                  Fetch CC
***********************************************************************
*
*        Test 1.15.1.1: zero + neg = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AHHLR_ZNN1 Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         AHHLR R2,R1,R0            CC=1
         IPM   R3                  Fetch CC
*
*        Test 1.15.1.2: pos + neg = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AHHLR_PNN1 Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         AHHLR R2,R1,R0            CC=1
         IPM   R3                  Fetch CC
*
*        Test 1.15.1.3: neg + zero = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AHHLR_NZN1 Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         AHHLR R2,R1,R0            CC=1
         IPM   R3                  Fetch CC
*
*        Test 1.15.1.4: neg + pos = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AHHLR_NPN1 Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         AHHLR R2,R1,R0            CC=1
         IPM   R3                  Fetch CC
*
*        Test 1.15.1.5: neg + neg = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AHHLR_NNN1 Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         AHHLR R2,R1,R0            CC=1
         IPM   R3                  Fetch CC
***********************************************************************
*
*        Test 1.15.2.1: zero + pos = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AHHLR_ZPP2 Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         AHHLR R2,R1,R0            CC=2
         IPM   R3                  Fetch CC
*
*        Test 1.15.2.2: pos + zero = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AHHLR_PZP2 Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         AHHLR R2,R1,R0            CC=2
         IPM   R3                  Fetch CC
*
*        Test 1.15.2.3: pos + pos = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AHHLR_PPP2 Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         AHHLR R2,R1,R0            CC=2
         IPM   R3                  Fetch CC
*
*        Test 1.15.2.4: pos + neg = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AHHLR_PNP2 Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         AHHLR R2,R1,R0            CC=2
         IPM   R3                  Fetch CC
*
*        Test 1.15.2.5: neg + pos = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AHHLR_NPP2 Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         AHHLR R2,R1,R0            CC=2
         IPM   R3                  Fetch CC
***********************************************************************
*
*        Test 1.15.3.1: pos + pos = neg; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AHHLR_PPN3 Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         AHHLR R2,R1,R0            CC=3
         IPM   R3                  Fetch CC
*
*        Test 1.15.3.2: neg + neg = zero; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AHHLR_NNZ3 Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         AHHLR R2,R1,R0            CC=3
         IPM   R3                  Fetch CC
*
*        Test 1.15.3.3: neg + neg = pos; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AHHLR_NNP3 Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         AHHLR R2,R1,R0            CC=3
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
*        AFI instruction test
***********************************************************************
*
* C29 = AFI - Add Immediate
*
* operand1 is 32-bit signed value
* operand2 is 32-bit signed value
*
***********************************************************************
*
TAFI1    DS    0H
         USING *,R15               Establish addressability
***********************************************************************
*
*        Test 1.16.0.1: zero + zero = zero; CC=0
*
         SR    R0,R0               Set CC
         TM    *,X'80'             Set CC=3
         LMG   R0,R1,TST_AR_ZZZ0   Initialize registers 0 and 1
         AFI   R0,0                CC=0
         IPM   R3                  Fetch CC
*
*        Test 1.16.0.2: pos + neg = zero; CC=0
*
         SR    R0,R0               Set CC
         TM    *,X'80'             Set CC=3
         LMG   R0,R1,TST_AR_PNZ0   Initialize registers 0 and 1
         AFI   R0,-1               CC=0
         IPM   R3                  Fetch CC
*
*        Test 1.16.0.3: neg + pos = zero; CC=0
*
         SR    R0,R0               Set CC
         TM    *,X'80'             Set CC=3
         LMG   R0,R1,TST_AR_NPZ0   Initialize registers 0 and 1
         AFI   R0,1                CC=0
         IPM   R3                  Fetch CC
***********************************************************************
*
*        Test 1.16.1.1: zero + neg = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_ZNN1   Initialize registers 0 and 1
         AFI   R0,-1               CC=1
         IPM   R3                  Fetch CC
*
*        Test 1.16.1.2: pos + neg = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_PNN1   Initialize registers 0 and 1
         AFI   R0,-2               CC=1
         IPM   R3                  Fetch CC
*
*        Test 1.16.1.3: neg + zero = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_NZN1   Initialize registers 0 and 1
         AFI   R0,0                CC=1
         IPM   R3                  Fetch CC
*
*        Test 1.16.1.4: neg + pos = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_NPN1   Initialize registers 0 and 1
         AFI   R0,1                CC=1
         IPM   R3                  Fetch CC
*
*        Test 1.16.1.5: neg + neg = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_NNN1   Initialize registers 0 and 1
         AFI   R0,-2               CC=1
         IPM   R3                  Fetch CC
***********************************************************************
*
*        Test 1.16.2.1: zero + pos = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_ZPP2   Initialize registers 0 and 1
         AFI   R0,1                CC=2
         IPM   R3                  Fetch CC
*
*        Test 1.16.2.2: pos + zero = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_PZP2   Initialize registers 0 and 1
         AFI   R0,0                CC=2
         IPM   R3                  Fetch CC
*
*        Test 1.16.2.3: pos + pos = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_PPP2   Initialize registers 0 and 1
         AFI   R0,2                CC=2
         IPM   R3                  Fetch CC
*
*        Test 1.16.2.4: pos + neg = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_PNP2   Initialize registers 0 and 1
         AFI   R0,-1               CC=2
         IPM   R3                  Fetch CC
*
*        Test 1.16.2.5: neg + pos = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_NPP2   Initialize registers 0 and 1
         AFI   R0,2                CC=2
         IPM   R3                  Fetch CC
***********************************************************************
*
*        Test 1.16.3.1: pos + pos = neg; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_PPN3   Initialize registers 0 and 1
         AFI   R0,1                CC=3
         IPM   R3                  Fetch CC
*
*        Test 1.16.3.2: neg + neg = zero; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_NNZ3   Initialize registers 0 and 1
         AFI   R0,SFW_MIN          CC=3
         IPM   R3                  Fetch CC
*
*        Test 1.16.3.3: neg + neg = pos; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_NNP3   Initialize registers 0 and 1
         AFI   R0,-2               CC=3
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
*        AHIK instruction test
***********************************************************************
*
* ECD8 = AHIK - Add Immediate
*
* operand1 is 32-bit signed value
* operand2 is 16-bit signed value
* operand3 is 32-bit signed value
*
***********************************************************************
*
TAHIK1   DS    0H
         USING *,R15               Establish addressability
***********************************************************************
*
*        Test 1.17.0.1: zero + zero = zero; CC=0
*
         SR    R0,R0               Set CC
         TM    *,X'80'             Set CC=3
         LMG   R0,R1,TST_AR_ZZZ0   Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         AHIK  R2,R0,0             CC=0
         IPM   R3                  Fetch CC
*
*        Test 1.17.0.2: pos + neg = zero; CC=0
*
         SR    R0,R0               Set CC
         TM    *,X'80'             Set CC=3
         LMG   R0,R1,TST_AR_PNZ0   Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         AHIK  R2,R0,-1            CC=0
         IPM   R3                  Fetch CC
*
*        Test 1.12.0.3: neg + pos = zero; CC=0
*
         SR    R0,R0               Set CC
         TM    *,X'80'             Set CC=3
         LMG   R0,R1,TST_AR_NPZ0   Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         AHIK  R2,R0,1             CC=0
         IPM   R3                  Fetch CC
***********************************************************************
*
*        Test 1.17.1.1: zero + neg = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_ZNN1   Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         AHIK  R2,R0,-1            CC=1
         IPM   R3                  Fetch CC
*
*        Test 1.17.1.2: pos + neg = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_PNN1   Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         AHIK  R2,R0,-2            CC=1
         IPM   R3                  Fetch CC
*
*        Test 1.17.1.3: neg + zero = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_NZN1   Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         AHIK  R2,R0,0             CC=1
         IPM   R3                  Fetch CC
*
*        Test 1.17.1.4: neg + pos = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_NPN1   Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         AHIK  R2,R0,1             CC=1
         IPM   R3                  Fetch CC
*
*        Test 1.17.1.5: neg + neg = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_NNN1   Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         AHIK  R2,R0,-2            CC=1
         IPM   R3                  Fetch CC
***********************************************************************
*
*        Test 1.17.2.1: zero + pos = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_ZPP2   Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         AHIK  R2,R0,1             CC=2
         IPM   R3                  Fetch CC
*
*        Test 1.17.2.2: pos + zero = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_PZP2   Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         AHIK  R2,R0,0             CC=2
         IPM   R3                  Fetch CC
*
*        Test 1.17.2.3: pos + pos = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_PPP2   Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         AHIK  R2,R0,2             CC=2
         IPM   R3                  Fetch CC
*
*        Test 1.17.2.4: pos + neg = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_PNP2   Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         AHIK  R2,R0,-1            CC=2
         IPM   R3                  Fetch CC
*
*        Test 1.17.2.5: neg + pos = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_NPP2   Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         AHIK  R2,R0,2             CC=2
         IPM   R3                  Fetch CC
***********************************************************************
*
*        Test 1.17.3.1: pos + pos = neg; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_PPN3   Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         AHIK  R2,R0,1             CC=3
         IPM   R3                  Fetch CC
*
*        Test 1.17.3.2: neg + neg = zero; CC=3; cannot occur
*
***      SR    R0,R0               Set CC
***      LMG   R0,R1,TST_AR_NNZ3   Initialize registers 0 and 1
***      LG    R2,InitGR2          Initialize register 2
***      AHIK  R2,R0,SHW_MIN       CC=3
***      IPM   R3                  Fetch CC
*
*        Test 1.17.3.3: neg + neg = pos; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_NNP3   Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         AHIK  R2,R0,-2            CC=3
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
*        ASI instruction test
***********************************************************************
*
* EB6A = ASI - Add Immediate
*
* operand1 is 32-bit signed value (storage)
* operand2 is 8-bit signed value
*
***********************************************************************
*
TASI1    DS    0H
         USING *,R15               Establish addressability
***********************************************************************
*
*        Test 1.18.0.1: zero + zero = zero; CC=0
*
         SR    R0,R0               Set CC
         TM    *,X'80'             Set CC=3
         LMG   R0,R1,TST_AR_ZZZ0   Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         ST    R0,FW               Initialize destination
         ASI   FW,0                CC=0
         IPM   R3                  Fetch CC
         L     R2,FW               Get result
*
*        Test 1.18.0.2: pos + neg = zero; CC=0
*
         SR    R0,R0               Set CC
         TM    *,X'80'             Set CC=3
         LMG   R0,R1,TST_AR_PNZ0   Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         ST    R0,FW               Initialize destination
         ASI   FW,-1               CC=0
         IPM   R3                  Fetch CC
         L     R2,FW               Get result
*
*        Test 1.18.0.3: neg + pos = zero; CC=0
*
         SR    R0,R0               Set CC
         TM    *,X'80'             Set CC=3
         LMG   R0,R1,TST_AR_NPZ0   Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         ST    R0,FW               Initialize destination
         ASI   FW,1                CC=0
         IPM   R3                  Fetch CC
         L     R2,FW               Get result
***********************************************************************
*
*        Test 1.18.1.1: zero + neg = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_ZNN1   Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         ST    R0,FW               Initialize destination
         ASI   FW,-1               CC=1
         IPM   R3                  Fetch CC
         L     R2,FW               Get result
*
*        Test 1.18.1.2: pos + neg = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_PNN1   Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         ST    R0,FW               Initialize destination
         ASI   FW,-2               CC=1
         IPM   R3                  Fetch CC
         L     R2,FW               Get result
*
*        Test 1.18.1.3: neg + zero = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_NZN1   Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         ST    R0,FW               Initialize destination
         ASI   FW,0                CC=1
         IPM   R3                  Fetch CC
         L     R2,FW               Get result
*
*        Test 1.18.1.4: neg + pos = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_NPN1   Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         ST    R0,FW               Initialize destination
         ASI   FW,1                CC=1
         IPM   R3                  Fetch CC
         L     R2,FW               Get result
*
*        Test 1.18.1.5: neg + neg = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_NNN1   Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         ST    R0,FW               Initialize destination
         ASI   FW,-2               CC=1
         IPM   R3                  Fetch CC
         L     R2,FW               Get result
***********************************************************************
*
*        Test 1.18.2.1: zero + pos = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_ZPP2   Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         ST    R0,FW               Initialize destination
         ASI   FW,1                CC=2
         IPM   R3                  Fetch CC
         L     R2,FW               Get result
*
*        Test 1.18.2.2: pos + zero = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_PZP2   Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         ST    R0,FW               Initialize destination
         ASI   FW,0                CC=2
         IPM   R3                  Fetch CC
         L     R2,FW               Get result
*
*        Test 1.18.2.3: pos + pos = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_PPP2   Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         ST    R0,FW               Initialize destination
         ASI   FW,2                CC=2
         IPM   R3                  Fetch CC
         L     R2,FW               Get result
*
*        Test 1.18.2.4: pos + neg = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_PNP2   Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         ST    R0,FW               Initialize destination
         ASI   FW,-1               CC=2
         IPM   R3                  Fetch CC
         L     R2,FW               Get result
*
*        Test 1.18.2.5: neg + pos = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_NPP2   Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         ST    R0,FW               Initialize destination
         ASI   FW,2                CC=2
         IPM   R3                  Fetch CC
         L     R2,FW               Get result
***********************************************************************
*
*        Test 1.18.3.1: pos + pos = neg; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_PPN3   Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         ST    R0,FW               Initialize destination
         ASI   FW,1                CC=3
         IPM   R3                  Fetch CC
         L     R2,FW               Get result
*
*        Test 1.18.3.2: neg + neg = zero; CC=3; cannot occur
*
***      SR    R0,R0               Set CC
***      LMG   R0,R1,TST_AR_NNZ3   Initialize registers 0 and 1
***      LG    R2,InitGR2          Initialize register 2
***      ST    R0,FW               Initialize destination
***      ASI   FW,-1               CC=3
***      IPM   R3                  Fetch CC
***      L     R2,FW               Get result
*
*        Test 1.18.3.3: neg + neg = pos; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_NNP3   Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         ST    R0,FW               Initialize destination
         ASI   FW,-2               CC=3
         IPM   R3                  Fetch CC
         L     R2,FW               Get result
***********************************************************************
*
*        End test
*
         BR    R14                 Return to caller
*
         DROP  R15                 End addressability
*
***********************************************************************
*        AGHIK instruction test
***********************************************************************
*
* ECD9 = AGHIK - Add Immediate
*
* operand1 is 64-bit signed value
* operand2 is 16-bit signed value
* operand3 is 64-bit signed value
*
***********************************************************************
*
TAGHIK1  DS    0H
         USING *,R15               Establish addressability
***********************************************************************
*
*        Test 1.19.0.1: zero + zero = zero; CC=0
*
         SR    R0,R0               Set CC
         TM    *,X'80'             Set CC=3
         LMG   R0,R1,TST_AGR_ZZZ0  Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         AGHIK R2,R0,0             CC=0
         IPM   R3                  Fetch CC
*
*        Test 1.19.0.2: pos + neg = zero; CC=0
*
         SR    R0,R0               Set CC
         TM    *,X'80'             Set CC=3
         LMG   R0,R1,TST_AGR_PNZ0  Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         AGHIK R2,R0,-1            CC=0
         IPM   R3                  Fetch CC
*
*        Test 1.19.0.3: neg + pos = zero; CC=0
*
         SR    R0,R0               Set CC
         TM    *,X'80'             Set CC=3
         LMG   R0,R1,TST_AGR_NPZ0  Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         AGHIK R2,R0,1             CC=0
         IPM   R3                  Fetch CC
***********************************************************************
*
*        Test 1.19.1.1: zero + neg = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AGR_ZNN1  Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         AGHIK R2,R0,-1            CC=1
         IPM   R3                  Fetch CC
*
*        Test 1.19.1.2: pos + neg = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AGR_PNN1  Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         AGHIK R2,R0,-2            CC=1
         IPM   R3                  Fetch CC
*
*        Test 1.19.1.3: neg + zero = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AGR_NZN1  Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         AGHIK R2,R0,0             CC=1
         IPM   R3                  Fetch CC
*
*        Test 1.19.1.4: neg + pos = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AGR_NPN1  Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         AGHIK R2,R0,1             CC=1
         IPM   R3                  Fetch CC
*
*        Test 1.19.1.5: neg + neg = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AGR_NNN1  Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         AGHIK R2,R0,-2            CC=1
         IPM   R3                  Fetch CC
***********************************************************************
*
*        Test 1.19.2.1: zero + pos = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AGR_ZPP2  Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         AGHIK R2,R0,1             CC=2
         IPM   R3                  Fetch CC
*
*        Test 1.19.2.2: pos + zero = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AGR_PZP2  Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         AGHIK R2,R0,0             CC=2
         IPM   R3                  Fetch CC
*
*        Test 1.19.2.3: pos + pos = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AGR_PPP2  Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         AGHIK R2,R0,2             CC=2
         IPM   R3                  Fetch CC
*
*        Test 1.19.2.4: pos + neg = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AGR_PNP2  Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         AGHIK R2,R0,-1            CC=2
         IPM   R3                  Fetch CC
*
*        Test 1.19.2.5: neg + pos = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AGR_NPP2  Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         AGHIK R2,R0,2             CC=2
         IPM   R3                  Fetch CC
***********************************************************************
*
*        Test 1.19.3.1: pos + pos = neg; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AGR_PPN3  Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         AGHIK R2,R0,1             CC=3
         IPM   R3                  Fetch CC
*
*        Test 1.19.3.2: neg + neg = zero; CC=3; cannot occur
*
***      SR    R0,R0               Set CC
***      LMG   R0,R1,TST_AGR_NNZ3  Initialize registers 0 and 1
***      LG    R2,InitGR2          Initialize register 2
***      AGHIK R2,R0,SHW_MIN       CC=3
***      IPM   R3                  Fetch CC
*
*        Test 1.19.3.3: neg + neg = pos; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AGR_NNP3  Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         AGHIK R2,R0,-2            CC=3
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
*        AGFI instruction test
***********************************************************************
*
* C28 = AGFI - Add Immediate
*
* operand1 is 64-bit signed value
* operand2 is 32-bit signed value
*
***********************************************************************
*
TAGFI1   DS    0H
         USING *,R15               Establish addressability
***********************************************************************
*
*        Test 1.20.0.1: zero + zero = zero; CC=0
*
         SR    R0,R0               Set CC
         TM    *,X'80'             Set CC=3
         LMG   R0,R1,TST_AGR_ZZZ0  Initialize registers 0 and 1
         AGFI  R0,0                CC=0
         IPM   R3                  Fetch CC
*
*        Test 1.20.0.2: pos + neg = zero; CC=0
*
         SR    R0,R0               Set CC
         TM    *,X'80'             Set CC=3
         LMG   R0,R1,TST_AGR_PNZ0  Initialize registers 0 and 1
         AGFI  R0,-1               CC=0
         IPM   R3                  Fetch CC
*
*        Test 1.20.0.3: neg + pos = zero; CC=0
*
         SR    R0,R0               Set CC
         TM    *,X'80'             Set CC=3
         LMG   R0,R1,TST_AGR_NPZ0  Initialize registers 0 and 1
         AGFI  R0,1                CC=0
         IPM   R3                  Fetch CC
***********************************************************************
*
*        Test 1.20.1.1: zero + neg = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AGR_ZNN1  Initialize registers 0 and 1
         AGFI  R0,-1               CC=1
         IPM   R3                  Fetch CC
*
*        Test 1.20.1.2: pos + neg = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AGR_PNN1  Initialize registers 0 and 1
         AGFI  R0,-2               CC=1
         IPM   R3                  Fetch CC
*
*        Test 1.20.1.3: neg + zero = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AGR_NZN1  Initialize registers 0 and 1
         AGFI  R0,0                CC=1
         IPM   R3                  Fetch CC
*
*        Test 1.20.1.4: neg + pos = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AGR_NPN1  Initialize registers 0 and 1
         AGFI  R0,1                CC=1
         IPM   R3                  Fetch CC
*
*        Test 1.20.1.5: neg + neg = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AGR_NNN1  Initialize registers 0 and 1
         AGFI  R0,-2               CC=1
         IPM   R3                  Fetch CC
***********************************************************************
*
*        Test 1.20.2.1: zero + pos = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AGR_ZPP2  Initialize registers 0 and 1
         AGFI  R0,1                CC=2
         IPM   R3                  Fetch CC
*
*        Test 1.20.2.2: pos + zero = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AGR_PZP2  Initialize registers 0 and 1
         AGFI  R0,0                CC=2
         IPM   R3                  Fetch CC
*
*        Test 1.20.2.3: pos + pos = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AGR_PPP2  Initialize registers 0 and 1
         AGFI  R0,2                CC=2
         IPM   R3                  Fetch CC
*
*        Test 1.20.2.4: pos + neg = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AGR_PNP2  Initialize registers 0 and 1
         AGFI  R0,-1               CC=2
         IPM   R3                  Fetch CC
*
*        Test 1.20.2.5: neg + pos = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AGR_NPP2  Initialize registers 0 and 1
         AGFI  R0,2                CC=2
         IPM   R3                  Fetch CC
***********************************************************************
*
*        Test 1.20.3.1: pos + pos = neg; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AGR_PPN3  Initialize registers 0 and 1
         AGFI  R0,1                CC=3
         IPM   R3                  Fetch CC
*
*        Test 1.20.3.2: neg + neg = zero; CC=3; cannot occur
*
***      SR    R0,R0               Set CC
***      LMG   R0,R1,TST_AGR_NNZ3  Initialize registers 0 and 1
***      AGFI  R0,SFW_MIN          CC=3
***      IPM   R3                  Fetch CC
*
*        Test 1.20.3.3: neg + neg = pos; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AGR_NNP3  Initialize registers 0 and 1
         AGFI  R0,-2               CC=3
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
*        AGSI instruction test
***********************************************************************
*
* EB7A = AGSI - Add Immediate
*
* operand1 is 64-bit signed value (storage)
* operand2 is 8-bit signed value
*
***********************************************************************
*
TAGSI1   DS    0H
         USING *,R15               Establish addressability
***********************************************************************
*
*        Test 1.21.0.1: zero + zero = zero; CC=0
*
         SR    R0,R0               Set CC
         TM    *,X'80'             Set CC=3
         LMG   R0,R1,TST_AGR_ZZZ0  Initialize registers 0 and 1
         STG   R0,DW               Initialize destination
         AGSI  DW,0                CC=0
         IPM   R3                  Fetch CC
         LG    R2,DW               Get result
*
*        Test 1.21.0.2: pos + neg = zero; CC=0
*
         SR    R0,R0               Set CC
         TM    *,X'80'             Set CC=3
         LMG   R0,R1,TST_AGR_PNZ0  Initialize registers 0 and 1
         STG   R0,DW               Initialize destination
         AGSI  DW,-1               CC=0
         IPM   R3                  Fetch CC
         LG    R2,DW               Get result
*
*        Test 1.21.0.3: neg + pos = zero; CC=0
*
         SR    R0,R0               Set CC
         TM    *,X'80'             Set CC=3
         LMG   R0,R1,TST_AGR_NPZ0  Initialize registers 0 and 1
         STG   R0,DW               Initialize destination
         AGSI  DW,1                CC=0
         IPM   R3                  Fetch CC
         LG    R2,DW               Get result
***********************************************************************
*
*        Test 1.21.1.1: zero + neg = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AGR_ZNN1  Initialize registers 0 and 1
         STG   R0,DW               Initialize destination
         AGSI  DW,-1               CC=1
         IPM   R3                  Fetch CC
         LG    R2,DW               Get result
*
*        Test 1.21.1.2: pos + neg = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AGR_PNN1  Initialize registers 0 and 1
         STG   R0,DW               Initialize destination
         AGSI  DW,-2               CC=1
         IPM   R3                  Fetch CC
         LG    R2,DW               Get result
*
*        Test 1.21.1.3: neg + zero = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AGR_NZN1  Initialize registers 0 and 1
         STG   R0,DW               Initialize destination
         AGSI  DW,0                CC=1
         IPM   R3                  Fetch CC
         LG    R2,DW               Get result
*
*        Test 1.21.1.4: neg + pos = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AGR_NPN1  Initialize registers 0 and 1
         STG   R0,DW               Initialize destination
         AGSI  DW,1                CC=1
         IPM   R3                  Fetch CC
         LG    R2,DW               Get result
*
*        Test 1.21.1.5: neg + neg = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AGR_NNN1  Initialize registers 0 and 1
         STG   R0,DW               Initialize destination
         AGSI  DW,-2               CC=1
         IPM   R3                  Fetch CC
         LG    R2,DW               Get result
***********************************************************************
*
*        Test 1.21.2.1: zero + pos = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AGR_ZPP2  Initialize registers 0 and 1
         STG   R0,DW               Initialize destination
         AGSI  DW,1                CC=2
         IPM   R3                  Fetch CC
         LG    R2,DW               Get result
*
*        Test 1.21.2.2: pos + zero = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AGR_PZP2  Initialize registers 0 and 1
         STG   R0,DW               Initialize destination
         AGSI  DW,0                CC=2
         IPM   R3                  Fetch CC
         LG    R2,DW               Get result
*
*        Test 1.21.2.3: pos + pos = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AGR_PPP2  Initialize registers 0 and 1
         STG   R0,DW               Initialize destination
         AGSI  DW,2                CC=2
         IPM   R3                  Fetch CC
         LG    R2,DW               Get result
*
*        Test 1.21.2.4: pos + neg = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AGR_PNP2  Initialize registers 0 and 1
         STG   R0,DW               Initialize destination
         AGSI  DW,-1               CC=2
         IPM   R3                  Fetch CC
         LG    R2,DW               Get result
*
*        Test 1.21.2.5: neg + pos = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AGR_NPP2   Initialize registers 0 and 1
         STG   R0,DW               Initialize destination
         AGSI  DW,2                CC=2
         IPM   R3                  Fetch CC
         LG    R2,DW               Get result
***********************************************************************
*
*        Test 1.21.3.1: pos + pos = neg; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AGR_PPN3  Initialize registers 0 and 1
         STG   R0,DW               Initialize destination
         AGSI  DW,1                CC=3
         IPM   R3                  Fetch CC
         LG    R2,DW               Get result
*
*        Test 1.21.3.2: neg + neg = zero; CC=3; cannot occur
*
***      SR    R0,R0               Set CC
***      LMG   R0,R1,TST_AGR_NNZ3  Initialize registers 0 and 1
***      STG   R0,DW               Initialize destination
***      AGSI  DW,-1               CC=3
***      IPM   R3                  Fetch CC
***      LG    R2,DW               Get result
*
*        Test 1.21.3.3: neg + neg = pos; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AGR_NNP3  Initialize registers 0 and 1
         STG   R0,DW               Initialize destination
         AGSI  DW,-2               CC=3
         IPM   R3                  Fetch CC
         LG    R2,DW               Get result
***********************************************************************
*
*        End test
*
         BR    R14                 Return to caller
*
         DROP  R15                 End addressability
*
***********************************************************************
*        AIH instruction test
***********************************************************************
*
* CC8 = AIH - Add Immediate High
*
* operand1 is 32-bit signed value in high half of register
* operand2 is 32-bit signed value
*
***********************************************************************
*
TAIH1    DS    0H
         USING *,R15               Establish addressability
***********************************************************************
*
*        Test 1.22.0.1: zero + zero = zero; CC=0
*
         SR    R0,R0               Set CC
         TM    *,X'80'             Set CC=3
         LMG   R0,R1,TST_AHHHR_ZZZ0 Initialize registers 0 and 1
         AIH   R0,0                CC=0
         IPM   R3                  Fetch CC
*
*        Test 1.22.0.2: pos + neg = zero; CC=0
*
         SR    R0,R0               Set CC
         TM    *,X'80'             Set CC=3
         LMG   R0,R1,TST_AHHHR_PNZ0 Initialize registers 0 and 1
         AIH   R0,-1               CC=0
         IPM   R3                  Fetch CC
*
*        Test 1.22.0.3: neg + pos = zero; CC=0
*
         SR    R0,R0               Set CC
         TM    *,X'80'             Set CC=3
         LMG   R0,R1,TST_AHHHR_NPZ0 Initialize registers 0 and 1
         AIH   R0,1                CC=0
         IPM   R3                  Fetch CC
***********************************************************************
*
*        Test 1.22.1.1: zero + neg = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AHHHR_ZNN1 Initialize registers 0 and 1
         AIH   R0,-1               CC=1
         IPM   R3                  Fetch CC
*
*        Test 1.22.1.2: pos + neg = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AHHHR_PNN1 Initialize registers 0 and 1
         AIH   R0,-2               CC=1
         IPM   R3                  Fetch CC
*
*        Test 1.22.1.3: neg + zero = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AHHHR_NZN1 Initialize registers 0 and 1
         AIH   R0,0                CC=1
         IPM   R3                  Fetch CC
*
*        Test 1.22.1.4: neg + pos = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AHHHR_NPN1 Initialize registers 0 and 1
         AIH   R0,1                CC=1
         IPM   R3                  Fetch CC
*
*        Test 1.22.1.5: neg + neg = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AHHHR_NNN1 Initialize registers 0 and 1
         AIH   R0,-2               CC=1
         IPM   R3                  Fetch CC
***********************************************************************
*
*        Test 1.22.2.1: zero + pos = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AHHHR_ZPP2 Initialize registers 0 and 1
         AIH   R0,1                CC=2
         IPM   R3                  Fetch CC
*
*        Test 1.22.2.2: pos + zero = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AHHHR_PZP2 Initialize registers 0 and 1
         AIH   R0,0                CC=2
         IPM   R3                  Fetch CC
*
*        Test 1.22.2.3: pos + pos = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AHHHR_PPP2 Initialize registers 0 and 1
         AIH   R0,2                CC=2
         IPM   R3                  Fetch CC
*
*        Test 1.22.2.4: pos + neg = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AHHHR_PNP2 Initialize registers 0 and 1
         AIH   R0,-1               CC=2
         IPM   R3                  Fetch CC
*
*        Test 1.22.2.5: neg + pos = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AHHHR_NPP2 Initialize registers 0 and 1
         AIH   R0,2                CC=2
         IPM   R3                  Fetch CC
***********************************************************************
*
*        Test 1.22.3.1: pos + pos = neg; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AHHHR_PPN3 Initialize registers 0 and 1
         AIH   R0,1                CC=3
         IPM   R3                  Fetch CC
*
*        Test 1.22.3.2: neg + neg = zero; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AHHHR_NNZ3 Initialize registers 0 and 1
         AIH   R0,SFW_MIN          CC=3
         IPM   R3                  Fetch CC
*
*        Test 1.22.3.3: neg + neg = pos; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AHHHR_NNP3 Initialize registers 0 and 1
         AIH   R0,-2               CC=3
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
*        LAA instruction test
***********************************************************************
*
* EBF8 = LAA - Load and Add
*
* operand1 is 32-bit signed value
* operand2 is 32-bit signed value
* operand3 is 32-bit signed value
*
***********************************************************************
*
TLAA1    DS    0H
         USING *,R15               Establish addressability
***********************************************************************
*
*        Test 1.23.0.1: zero + zero = zero; CC=0
*
         SR    R0,R0               Set CC
         TM    *,X'80'             Set CC=3
         LMG   R0,R1,TST_AR_ZZZ0   Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         ST    R0,FW               Initialize second operand
         LAA   R0,R1,FW            CC=0
         IPM   R3                  Fetch CC
         L     R2,FW               Get result
*
*        Test 1.23.0.2: pos + neg = zero; CC=0
*
         SR    R0,R0               Set CC
         TM    *,X'80'             Set CC=3
         LMG   R0,R1,TST_AR_PNZ0   Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         ST    R0,FW               Initialize second operand
         LAA   R0,R1,FW            CC=0
         IPM   R3                  Fetch CC
         L     R2,FW               Get result
*
*        Test 1.23.0.3: neg + pos = zero; CC=0
*
         SR    R0,R0               Set CC
         TM    *,X'80'             Set CC=3
         LMG   R0,R1,TST_AR_NPZ0   Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         ST    R0,FW               Initialize second operand
         LAA   R0,R1,FW            CC=0
         IPM   R3                  Fetch CC
         L     R2,FW               Get result
***********************************************************************
*
*        Test 1.23.1.1: zero + neg = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_ZNN1   Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         ST    R0,FW               Initialize second operand
         LAA   R0,R1,FW            CC=1
         IPM   R3                  Fetch CC
         L     R2,FW               Get result
*
*        Test 1.23.1.2: pos + neg = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_PNN1   Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         ST    R0,FW               Initialize second operand
         LAA   R0,R1,FW            CC=1
         IPM   R3                  Fetch CC
         L     R2,FW               Get result
*
*        Test 1.23.1.3: neg + zero = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_NZN1   Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         ST    R0,FW               Initialize second operand
         LAA   R0,R1,FW            CC=1
         IPM   R3                  Fetch CC
         L     R2,FW               Get result
*
*        Test 1.23.1.4: neg + pos = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_NPN1   Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         ST    R0,FW               Initialize second operand
         LAA   R0,R1,FW            CC=1
         IPM   R3                  Fetch CC
         L     R2,FW               Get result
*
*        Test 1.23.1.5: neg + neg = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_NNN1   Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         ST    R0,FW               Initialize second operand
         LAA   R0,R1,FW            CC=1
         IPM   R3                  Fetch CC
         L     R2,FW               Get result
***********************************************************************
*
*        Test 1.23.2.1: zero + pos = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_ZPP2   Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         ST    R0,FW               Initialize second operand
         LAA   R0,R1,FW            CC=2
         IPM   R3                  Fetch CC
         L     R2,FW               Get result
*
*        Test 1.23.2.2: pos + zero = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_PZP2   Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         ST    R0,FW               Initialize second operand
         LAA   R0,R1,FW            CC=2
         IPM   R3                  Fetch CC
         L     R2,FW               Get result
*
*        Test 1.23.2.3: pos + pos = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_PPP2   Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         ST    R0,FW               Initialize second operand
         LAA   R0,R1,FW            CC=2
         IPM   R3                  Fetch CC
         L     R2,FW               Get result
*
*        Test 1.23.2.4: pos + neg = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_PNP2   Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         ST    R0,FW               Initialize second operand
         LAA   R0,R1,FW            CC=2
         IPM   R3                  Fetch CC
         L     R2,FW               Get result
*
*        Test 1.23.2.5: neg + pos = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_NPP2   Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         ST    R0,FW               Initialize second operand
         LAA   R0,R1,FW            CC=2
         IPM   R3                  Fetch CC
         L     R2,FW               Get result
***********************************************************************
*
*        Test 1.23.3.1: pos + pos = neg; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_PPN3   Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         ST    R0,FW               Initialize second operand
         LAA   R0,R1,FW            CC=3
         IPM   R3                  Fetch CC
         L     R2,FW               Get result
*
*        Test 1.23.3.2: neg + neg = zero; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_NNZ3   Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         ST    R0,FW               Initialize second operand
         LAA   R0,R1,FW            CC=3
         IPM   R3                  Fetch CC
         L     R2,FW               Get result
*
*        Test 1.23.3.3: neg + neg = pos; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_NNP3   Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         ST    R0,FW               Initialize second operand
         LAA   R0,R1,FW            CC=3
         IPM   R3                  Fetch CC
         L     R2,FW               Get result
***********************************************************************
*
*        End test
*
         BR    R14                 Return to caller
*
         DROP  R15                 End addressability
*
***********************************************************************
*        LAAG instruction test
***********************************************************************
*
* EBE8 = LAAG - Load and Add
*
* operand1 is 64-bit signed value
* operand2 is 64-bit signed value
* operand3 is 64-bit signed value
*
***********************************************************************
*
TLAAG1   DS    0H
         USING *,R15               Establish addressability
***********************************************************************
*
*        Test 1.24.0.1: zero + zero = zero; CC=0
*
         SR    R0,R0               Set CC
         TM    *,X'80'             Set CC=3
         LMG   R0,R1,TST_AGR_ZZZ0  Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         STG   R0,DW               Initialize second operand
         LAAG  R0,R1,DW            CC=0
         IPM   R3                  Fetch CC
         LG    R2,DW               Get result
*
*        Test 1.24.0.2: pos + neg = zero; CC=0
*
         SR    R0,R0               Set CC
         TM    *,X'80'             Set CC=3
         LMG   R0,R1,TST_AGR_PNZ0  Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         STG   R0,DW               Initialize second operand
         LAAG  R0,R1,DW            CC=0
         IPM   R3                  Fetch CC
         LG    R2,DW               Get result
*
*        Test 1.24.0.3: neg + pos = zero; CC=0
*
         SR    R0,R0               Set CC
         TM    *,X'80'             Set CC=3
         LMG   R0,R1,TST_AGR_NPZ0  Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         STG   R0,DW               Initialize second operand
         LAAG  R0,R1,DW            CC=0
         IPM   R3                  Fetch CC
         LG    R2,DW               Get result
***********************************************************************
*
*        Test 1.24.1.1: zero + neg = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AGR_ZNN1  Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         STG   R0,DW               Initialize second operand
         LAAG  R0,R1,DW            CC=1
         IPM   R3                  Fetch CC
         LG    R2,DW               Get result
*
*        Test 1.24.1.2: pos + neg = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AGR_PNN1  Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         STG   R0,DW               Initialize second operand
         LAAG  R0,R1,DW            CC=1
         IPM   R3                  Fetch CC
         LG    R2,DW               Get result
*
*        Test 1.24.1.3: neg + zero = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AGR_NZN1  Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         STG   R0,DW               Initialize second operand
         LAAG  R0,R1,DW            CC=1
         IPM   R3                  Fetch CC
         LG    R2,DW               Get result
*
*        Test 1.24.1.4: neg + pos = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AGR_NPN1  Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         STG   R0,DW               Initialize second operand
         LAAG  R0,R1,DW            CC=1
         IPM   R3                  Fetch CC
         LG    R2,DW               Get result
*
*        Test 1.24.1.5: neg + neg = neg; CC=1
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AGR_NNN1  Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         STG   R0,DW               Initialize second operand
         LAAG  R0,R1,DW            CC=1
         IPM   R3                  Fetch CC
         LG    R2,DW               Get result
***********************************************************************
*
*        Test 1.24.2.1: zero + pos = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AGR_ZPP2  Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         STG   R0,DW               Initialize second operand
         LAAG  R0,R1,DW            CC=2
         IPM   R3                  Fetch CC
         LG    R2,DW               Get result
*
*        Test 1.24.2.2: pos + zero = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AGR_PZP2  Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         STG   R0,DW               Initialize second operand
         LAAG  R0,R1,DW            CC=2
         IPM   R3                  Fetch CC
         LG    R2,DW               Get result
*
*        Test 1.24.2.3: pos + pos = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AGR_PPP2  Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         STG   R0,DW               Initialize second operand
         LAAG  R0,R1,DW            CC=2
         IPM   R3                  Fetch CC
         LG    R2,DW               Get result
*
*        Test 1.24.2.4: pos + neg = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AGR_PNP2  Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         STG   R0,DW               Initialize second operand
         LAAG  R0,R1,DW            CC=2
         IPM   R3                  Fetch CC
         LG    R2,DW               Get result
*
*        Test 1.24.2.5: neg + pos = pos; CC=2
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AGR_NPP2  Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         STG   R0,DW               Initialize second operand
         LAAG  R0,R1,DW            CC=2
         IPM   R3                  Fetch CC
         LG    R2,DW               Get result
***********************************************************************
*
*        Test 1.24.3.1: pos + pos = neg; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AGR_PPN3  Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         STG   R0,DW               Initialize second operand
         LAAG  R0,R1,DW            CC=3
         IPM   R3                  Fetch CC
         LG    R2,DW               Get result
*
*        Test 1.24.3.2: neg + neg = zero; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AGR_NNZ3  Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         STG   R0,DW               Initialize second operand
         LAAG  R0,R1,DW            CC=3
         IPM   R3                  Fetch CC
         LG    R2,DW               Get result
*
*        Test 1.24.3.3: neg + neg = pos; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AGR_NNP3  Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         STG   R0,DW               Initialize second operand
         LAAG  R0,R1,DW            CC=3
         IPM   R3                  Fetch CC
         LG    R2,DW               Get result
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
*        A instruction test
***********************************************************************
TA2      DS    0H
         USING *,R15               Establish addressability
***********************************************************************
*
*        Test 2.1.3.1: pos + pos = neg; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_PPN3   Initialize registers 0 and 1
         ST    R1,FW               Value to add
         LA    R4,A_Ret1           Return address
         ST    R4,ESRET@           Save for ESPIE exit
         A     R0,FW               Fixed-point-overflow exception (0C8)
         DC    H'0'                Should not get here
A_Ret1   DS    0H
         IPM   R3                  Fetch CC
*
*        Test 2.1.3.2: neg + neg = zero; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_NNZ3   Initialize registers 0 and 1
         ST    R1,FW               Value to add
         LA    R4,A_Ret2           Return address
         ST    R4,ESRET@           Save for ESPIE exit
         A     R0,FW               Fixed-point-overflow exception (0C8)
         DC    H'0'                Should not get here
A_Ret2   DS    0H
         IPM   R3                  Fetch CC
*
*        Test 2.1.3.3: neg + neg = pos; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_NNP3   Initialize registers 0 and 1
         ST    R1,FW               Value to add
         LA    R4,A_Ret3           Return address
         ST    R4,ESRET@           Save for ESPIE exit
         A     R0,FW               Fixed-point-overflow exception (0C8)
         DC    H'0'                Should not get here
A_Ret3   DS    0H
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
*        AR instruction test
***********************************************************************
TAR2     DS    0H
         USING *,R15               Establish addressability
***********************************************************************
*
*        Test 2.2.3.1: pos + pos = neg; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_PPN3   Initialize registers 0 and 1
         LA    R4,AR_Ret1          Return address
         ST    R4,ESRET@           Save for ESPIE exit
         AR    R0,R1               Fixed-point-overflow exception (0C8)
         DC    H'0'                Should not get here
AR_Ret1  DS    0H
         IPM   R3                  Fetch CC
*
*        Test 2.2.3.2: neg + neg = zero; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_NNZ3   Initialize registers 0 and 1
         LA    R4,AR_Ret2          Return address
         ST    R4,ESRET@           Save for ESPIE exit
         AR    R0,R1               Fixed-point-overflow exception (0C8)
         DC    H'0'                Should not get here
AR_Ret2  DS    0H
         IPM   R3                  Fetch CC
*
*        Test 2.2.3.3: neg + neg = pos; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_NNP3   Initialize registers 0 and 1
         LA    R4,AR_Ret3          Return address
         ST    R4,ESRET@           Save for ESPIE exit
         AR    R0,R1               Fixed-point-overflow exception (0C8)
         DC    H'0'                Should not get here
AR_Ret3  DS    0H
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
*        ARK instruction test
***********************************************************************
TARK2    DS    0H
         USING *,R15               Establish addressability
***********************************************************************
*
*        Test 2.3.3.1: pos + pos = neg; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_PPN3   Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         LA    R4,ARK_Ret1         Return address
         ST    R4,ESRET@           Save for ESPIE exit
         ARK   R2,R1,R0            Fixed-point-overflow exception (0C8)
         DC    H'0'                Should not get here
ARK_Ret1 DS    0H
         IPM   R3                  Fetch CC
*
*        Test 2.3.3.2: neg + neg = zero; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_NNZ3   Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         LA    R4,ARK_Ret2         Return address
         ST    R4,ESRET@           Save for ESPIE exit
         ARK   R2,R1,R0            Fixed-point-overflow exception (0C8)
         DC    H'0'                Should not get here
ARK_Ret2 DS    0H
         IPM   R3                  Fetch CC
*
*        Test 2.3.3.3: neg + neg = pos; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_NNP3   Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         LA    R4,ARK_Ret3         Return address
         ST    R4,ESRET@           Save for ESPIE exit
         ARK   R2,R1,R0            Fixed-point-overflow exception (0C8)
         DC    H'0'                Should not get here
ARK_Ret3 DS    0H
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
*        AY instruction test
***********************************************************************
TAY2     DS    0H
         USING *,R15               Establish addressability
***********************************************************************
*
*        Test 2.4.3.1: pos + pos = neg; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_PPN3   Initialize registers 0 and 1
         ST    R1,FW               Value to add
         LA    R4,AY_Ret1          Return address
         ST    R4,ESRET@           Save for ESPIE exit
         AY    R0,FW               Fixed-point-overflow exception (0C8)
         DC    H'0'                Should not get here
AY_Ret1  DS    0H
         IPM   R3                  Fetch CC
*
*        Test 2.4.3.2: neg + neg = zero; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_NNZ3   Initialize registers 0 and 1
         ST    R1,FW               Value to add
         LA    R4,AY_Ret2          Return address
         ST    R4,ESRET@           Save for ESPIE exit
         AY    R0,FW               Fixed-point-overflow exception (0C8)
         DC    H'0'                Should not get here
AY_Ret2  DS    0H
         IPM   R3                  Fetch CC
*
*        Test 2.4.3.3: neg + neg = pos; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_NNP3   Initialize registers 0 and 1
         ST    R1,FW               Value to add
         LA    R4,AY_Ret3          Return address
         ST    R4,ESRET@           Save for ESPIE exit
         AY    R0,FW               Fixed-point-overflow exception (0C8)
         DC    H'0'                Should not get here
AY_Ret3  DS    0H
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
*        AG instruction test
***********************************************************************
TAG2     DS    0H
         USING *,R15               Establish addressability
***********************************************************************
*
*        Test 2.5.3.1: pos + pos = neg; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AGR_PPN3  Initialize registers 0 and 1
         STG   R1,DW               Value to add
         LA    R4,AG_Ret1          Return address
         ST    R4,ESRET@           Save for ESPIE exit
         AG    R0,DW               Fixed-point-overflow exception (0C8)
         DC    H'0'                Should not get here
AG_Ret1  DS    0H
         IPM   R3                  Fetch CC
*
*        Test 2.5.5.2: neg + neg = zero; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AGR_NNZ3  Initialize registers 0 and 1
         STG   R1,DW               Value to add
         LA    R4,AG_Ret2          Return address
         ST    R4,ESRET@           Save for ESPIE exit
         AG    R0,DW               Fixed-point-overflow exception (0C8)
         DC    H'0'                Should not get here
AG_Ret2  DS    0H
         IPM   R3                  Fetch CC
*
*        Test 2.5.3.3: neg + neg = pos; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AGR_NNP3  Initialize registers 0 and 1
         STG   R1,DW               Value to add
         LA    R4,AG_Ret3          Return address
         ST    R4,ESRET@           Save for ESPIE exit
         AG    R0,DW               Fixed-point-overflow exception (0C8)
         DC    H'0'                Should not get here
AG_Ret3  DS    0H
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
*        AGR instruction test
***********************************************************************
TAGR2    DS    0H
         USING *,R15               Establish addressability
***********************************************************************
*
*        Test 2.6.3.1: pos + pos = neg; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AGR_PPN3  Initialize registers 0 and 1
         LA    R4,AGR_Ret1         Return address
         ST    R4,ESRET@           Save for ESPIE exit
         AGR   R0,R1               Fixed-point-overflow exception (0C8)
         DC    H'0'                Should not get here
AGR_Ret1 DS    0H
         IPM   R3                  Fetch CC
*
*        Test 2.6.3.2: neg + neg = zero; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AGR_NNZ3  Initialize registers 0 and 1
         LA    R4,AGR_Ret2         Return address
         ST    R4,ESRET@           Save for ESPIE exit
         AGR   R0,R1               Fixed-point-overflow exception (0C8)
         DC    H'0'                Should not get here
AGR_Ret2 DS    0H
         IPM   R3                  Fetch CC
*
*        Test 2.6.3.3: neg + neg = pos; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AGR_NNP3  Initialize registers 0 and 1
         LA    R4,AGR_Ret3         Return address
         ST    R4,ESRET@           Save for ESPIE exit
         AGR   R0,R1               Fixed-point-overflow exception (0C8)
         DC    H'0'                Should not get here
AGR_Ret3 DS    0H
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
*        AGRK instruction test
***********************************************************************
TAGRK2   DS    0H
         USING *,R15               Establish addressability
***********************************************************************
*
*        Test 2.7.3.1: pos + pos = neg; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AGR_PPN3  Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         LA    R4,AGRK_Ret1        Return address
         ST    R4,ESRET@           Save for ESPIE exit
         AGRK  R2,R1,R0            Fixed-point-overflow exception (0C8)
         DC    H'0'                Should not get here
AGRK_Ret1 DS   0H
         IPM   R3                  Fetch CC
*
*        Test 2.7.3.2: neg + neg = zero; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AGR_NNZ3  Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         LA    R4,AGRK_Ret2        Return address
         ST    R4,ESRET@           Save for ESPIE exit
         AGRK  R2,R1,R0            Fixed-point-overflow exception (0C8)
         DC    H'0'                Should not get here
AGRK_Ret2 DS   0H
         IPM   R3                  Fetch CC
*
*        Test 2.7.3.3: neg + neg = pos; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AGR_NNP3  Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         LA    R4,AGRK_Ret3        Return address
         ST    R4,ESRET@           Save for ESPIE exit
         AGRK  R2,R1,R0            Fixed-point-overflow exception (0C8)
         DC    H'0'                Should not get here
AGRK_Ret3 DS   0H
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
*        AGF instruction test
***********************************************************************
TAGF2    DS    0H
         USING *,R15               Establish addressability
***********************************************************************
*
*        Test 2.8.3.1: pos + pos = neg; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AGR_PPN3  Initialize registers 0 and 1
         ST    R1,FW               Value to add
         LA    R4,AGF_Ret1         Return address
         ST    R4,ESRET@           Save for ESPIE exit
         AGF   R0,FW               Fixed-point-overflow exception (0C8)
         DC    H'0'                Should not get here
AGF_Ret1 DS    0H
         IPM   R3                  Fetch CC
*
*        Test 2.8.3.2: neg + neg = zero; CC=3; cannot occur
*
***      SR    R0,R0               Set CC
***      LMG   R0,R1,TST_AGFR_NNZ3 Initialize registers 0 and 1
***      ST    R1,FW               Value to add
***      LA    R4,AGF_Ret2         Return address
***      ST    R4,ESRET@           Save for ESPIE exit
***      AGF   R0,FW               Fixed-point-overflow exception (0C8)
***      DC    H'0'                Should not get here
*AGF_Ret2 DS    0H
***      IPM   R3                  Fetch CC
*
*        Test 2.8.3.3: neg + neg = pos; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AGR_NNP3  Initialize registers 0 and 1
         ST    R1,FW               Value to add
         LA    R4,AGF_Ret3         Return address
         ST    R4,ESRET@           Save for ESPIE exit
         AGF   R0,FW               Fixed-point-overflow exception (0C8)
         DC    H'0'                Should not get here
AGF_Ret3 DS    0H
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
*        AGFR instruction test
***********************************************************************
TAGFR2   DS    0H
         USING *,R15               Establish addressability
***********************************************************************
*
*        Test 2.9.3.1: pos + pos = neg; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AGR_PPN3  Initialize registers 0 and 1
         LA    R4,AGFR_Ret1        Return address
         ST    R4,ESRET@           Save for ESPIE exit
         AGFR  R0,R1               Fixed-point-overflow exception (0C8)
         DC    H'0'                Should not get here
AGFR_Ret1 DS    0H
         IPM   R3                  Fetch CC
*
*        Test 2.9.3.2: neg + neg = zero; CC=3; cannot occur
*
***      SR    R0,R0               Set CC
***      LMG   R0,R1,TST_AGFR_NNZ3 Initialize registers 0 and 1
***      LA    R4,AGFR_Ret2        Return address
***      ST    R4,ESRET@           Save for ESPIE exit
***      AGFR  R0,R1               Fixed-point-overflow exception (0C8)
***      DC    H'0'                Should not get here
*AGFR_Ret2 DS    0H
***      IPM   R3                  Fetch CC
*
*        Test 2.9.3.3: neg + neg = pos; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AGR_NNP3  Initialize registers 0 and 1
         LA    R4,AGFR_Ret3        Return address
         ST    R4,ESRET@           Save for ESPIE exit
         AGFR  R0,R1               Fixed-point-overflow exception (0C8)
         DC    H'0'                Should not get here
AGFR_Ret3 DS    0H
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
*        AH instruction test
***********************************************************************
TAH2     DS    0H
         USING *,R15               Establish addressability
***********************************************************************
*
*        Test 2.10.3.1: pos + pos = neg; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_PPN3   Initialize registers 0 and 1
         STH   R1,HW               Value to add
         LA    R4,AH_Ret1          Return address
         ST    R4,ESRET@           Save for ESPIE exit
         AH    R0,HW               Fixed-point-overflow exception (0C8)
         DC    H'0'                Should not get here
AH_Ret1  DS    0H
         IPM   R3                  Fetch CC
*
*        Test 2.10.3.2: neg + neg = zero; CC=3; can't happen
*
***      SR    R0,R0               Set CC
***      LMG   R0,R1,TST_AR_NNZ3   Initialize registers 0 and 1
***      STH   R1,HW               Value to add
***      LA    R4,AH_Ret2          Return address
***      ST    R4,ESRET@           Save for ESPIE exit
***      AH    R0,HW               Fixed-point-overflow exception (0C8)
***      DC    H'0'                Should not get here
*AH_Ret2  DS    0H
***      IPM   R3                  Fetch CC
*
*        Test 2.10.3.3: neg + neg = pos; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_NNP3   Initialize registers 0 and 1
         STH   R1,HW               Value to add
         LA    R4,AH_Ret3          Return address
         ST    R4,ESRET@           Save for ESPIE exit
         AH    R0,HW               Fixed-point-overflow exception (0C8)
         DC    H'0'                Should not get here
AH_Ret3  DS    0H
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
*        AHY instruction test
***********************************************************************
TAHY2    DS    0H
         USING *,R15               Establish addressability
***********************************************************************
*
*        Test 2.11.3.1: pos + pos = neg; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_PPN3   Initialize registers 0 and 1
         STH   R1,HW               Value to add
         LA    R4,AHY_Ret1         Return address
         ST    R4,ESRET@           Save for ESPIE exit
         AHY   R0,HW               Fixed-point-overflow exception (0C8)
         DC    H'0'                Should not get here
AHY_Ret1 DS    0H
         IPM   R3                  Fetch CC
*
*        Test 2.11.3.2: neg + neg = zero; CC=3; can't happen
*
***      SR    R0,R0               Set CC
***      LMG   R0,R1,TST_AR_NNZ3   Initialize registers 0 and 1
***      STH   R1,HW               Value to add
***      LA    R4,AHY_Ret2         Return address
***      ST    R4,ESRET@           Save for ESPIE exit
***      AHY   R0,HW               Fixed-point-overflow exception (0C8)
***      DC    H'0'                Should not get here
*AHY_Ret2 DS    0H
***      IPM   R3                  Fetch CC
*
*        Test 2.11.3.3: neg + neg = pos; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_NNP3   Initialize registers 0 and 1
         STH   R1,HW               Value to add
         LA    R4,AHY_Ret3         Return address
         ST    R4,ESRET@           Save for ESPIE exit
         AHY   R0,HW               Fixed-point-overflow exception (0C8)
         DC    H'0'                Should not get here
AHY_Ret3 DS    0H
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
*        AHI instruction test
***********************************************************************
TAHI2    DS    0H
         USING *,R15               Establish addressability
***********************************************************************
*
*        Test 2.12.3.1: pos + pos = neg; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_PPN3   Initialize registers 0 and 1
         LA    R4,AHI_Ret1         Return address
         ST    R4,ESRET@           Save for ESPIE exit
         AHI   R0,1                Fixed-point-overflow exception (0C8)
         DC    H'0'                Should not get here
AHI_Ret1 DS    0H
         IPM   R3                  Fetch CC
*
*        Test 2.12.3.2: neg + neg = zero; CC=3; cannot occur
*
***      SR    R0,R0               Set CC
***      LMG   R0,R1,TST_AR_NNZ3   Initialize registers 0 and 1
***      LA    R4,AHI_Ret2         Return address
***      ST    R4,ESRET@           Save for ESPIE exit
***      AHI   R0,SHW_MIN          Fixed-point-overflow exception (0C8)
***      DC    H'0'                Should not get here
*AHI_Ret2 DS    0H
***      IPM   R3                  Fetch CC
*
*        Test 2.12.3.3: neg + neg = pos; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_NNP3   Initialize registers 0 and 1
         LA    R4,AHI_Ret3         Return address
         ST    R4,ESRET@           Save for ESPIE exit
         AHI   R0,-2               Fixed-point-overflow exception (0C8)
         DC    H'0'                Should not get here
AHI_Ret3 DS    0H
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
*        AGHI instruction test
***********************************************************************
TAGHI2   DS    0H
         USING *,R15               Establish addressability
***********************************************************************
*
*        Test 2.13.3.1: pos + pos = neg; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AGR_PPN3  Initialize registers 0 and 1
         LA    R4,AGHI_Ret1        Return address
         ST    R4,ESRET@           Save for ESPIE exit
         AGHI  R0,1                Fixed-point-overflow exception (0C8)
         DC    H'0'                Should not get here
AGHI_Ret1 DS    0H
         IPM   R3                  Fetch CC
*
*        Test 2.13.3.2: neg + neg = zero; CC=3; cannot occur
*
***      SR    R0,R0               Set CC
***      LMG   R0,R1,TST_AGR_NNZ3  Initialize registers 0 and 1
***      LA    R4,AGHI_Ret2        Return address
***      ST    R4,ESRET@           Save for ESPIE exit
***      AGHI  R0,1                Fixed-point-overflow exception (0C8)
***      DC    H'0'                Should not get here
*AGHI_Ret2 DS    0H
***      IPM   R3                  Fetch CC
*
*        Test 2.13.3.3: neg + neg = pos; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AGR_NNP3  Initialize registers 0 and 1
         LA    R4,AGHI_Ret3        Return address
         ST    R4,ESRET@           Save for ESPIE exit
         AGHI  R0,-2               Fixed-point-overflow exception (0C8)
         DC    H'0'                Should not get here
AGHI_Ret3 DS    0H
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
*        AHHHR instruction test
***********************************************************************
TAHHHR2  DS    0H
         USING *,R15               Establish addressability
***********************************************************************
*
*        Test 2.14.3.1: pos + pos = neg; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AHHHR_PPN3 Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         LA    R4,AHHHR_Ret1       Return address
         ST    R4,ESRET@           Save for ESPIE exit
         AHHHR R2,R1,R0            Fixed-point-overflow exception (0C8)
         DC    H'0'                Should not get here
AHHHR_Ret1 DS    0H
         IPM   R3                  Fetch CC
*
*        Test 2.14.3.2: neg + neg = zero; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AHHHR_NNZ3 Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         LA    R4,AHHHR_Ret2       Return address
         ST    R4,ESRET@           Save for ESPIE exit
         AHHHR R2,R1,R0            Fixed-point-overflow exception (0C8)
         DC    H'0'                Should not get here
AHHHR_Ret2 DS    0H
         IPM   R3                  Fetch CC
*
*        Test 2.14.3.3: neg + neg = pos; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AHHHR_NNP3 Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         LA    R4,AHHHR_Ret3       Return address
         ST    R4,ESRET@           Save for ESPIE exit
         AHHHR R2,R1,R0            Fixed-point-overflow exception (0C8)
         DC    H'0'                Should not get here
AHHHR_Ret3 DS    0H
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
*        AHHLR instruction test
***********************************************************************
TAHHLR2  DS    0H
         USING *,R15               Establish addressability
***********************************************************************
*
*        Test 2.15.3.1: pos + pos = neg; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AHHLR_PPN3 Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         LA    R4,AHHLR_Ret1       Return address
         ST    R4,ESRET@           Save for ESPIE exit
         AHHLR R2,R1,R0            Fixed-point-overflow exception (0C8)
         DC    H'0'                Should not get here
AHHLR_Ret1 DS    0H
         IPM   R3                  Fetch CC
*
*        Test 2.15.3.2: neg + neg = zero; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AHHLR_NNZ3 Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         LA    R4,AHHLR_Ret2       Return address
         ST    R4,ESRET@           Save for ESPIE exit
         AHHLR R2,R1,R0            Fixed-point-overflow exception (0C8)
         DC    H'0'                Should not get here
AHHLR_Ret2 DS    0H
         IPM   R3                  Fetch CC
*
*        Test 2.15.3.3: neg + neg = pos; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AHHLR_NNP3 Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         LA    R4,AHHLR_Ret3       Return address
         ST    R4,ESRET@           Save for ESPIE exit
         AHHLR R2,R1,R0            Fixed-point-overflow exception (0C8)
         DC    H'0'                Should not get here
AHHLR_Ret3 DS    0H
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
*        AFI instruction test
***********************************************************************
TAFI2    DS    0H
         USING *,R15               Establish addressability
***********************************************************************
*
*        Test 2.16.3.1: pos + pos = neg; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_PPN3   Initialize registers 0 and 1
         LA    R4,AFI_Ret1         Return address
         ST    R4,ESRET@           Save for ESPIE exit
         AFI   R0,1                Fixed-point-overflow exception (0C8)
         DC    H'0'                Should not get here
AFI_Ret1 DS    0H
         IPM   R3                  Fetch CC
*
*        Test 2.16.3.2: neg + neg = zero; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_NNZ3   Initialize registers 0 and 1
         LA    R4,AFI_Ret2         Return address
         ST    R4,ESRET@           Save for ESPIE exit
         AFI   R0,SFW_MIN          Fixed-point-overflow exception (0C8)
         DC    H'0'                Should not get here
AFI_Ret2 DS    0H
         IPM   R3                  Fetch CC
*
*        Test 2.16.3.3: neg + neg = pos; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_NNP3   Initialize registers 0 and 1
         LA    R4,AFI_Ret3         Return address
         ST    R4,ESRET@           Save for ESPIE exit
         AFI   R0,-2               Fixed-point-overflow exception (0C8)
         DC    H'0'                Should not get here
AFI_Ret3 DS    0H
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
*        AHIK instruction test
***********************************************************************
TAHIK2   DS    0H
         USING *,R15               Establish addressability
***********************************************************************
*
*        Test 2.17.3.1: pos + pos = neg; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_PPN3   Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         LA    R4,AHIK_Ret1        Return address
         ST    R4,ESRET@           Save for ESPIE exit
         AHIK  R2,R0,1             Fixed-point-overflow exception (0C8)
         DC    H'0'                Should not get here
AHIK_Ret1 DS    0H
         IPM   R3                  Fetch CC
*
*        Test 2.17.3.2: neg + neg = zero; CC=3; cannot occur
*
***      SR    R0,R0               Set CC
***      LMG   R0,R1,TST_AR_NNZ3   Initialize registers 0 and 1
***      LG    R2,InitGR2          Initialize register 2
***      LA    R4,AHIK_Ret2        Return address
***      ST    R4,ESRET@           Save for ESPIE exit
***      AHIK  R2,R0,SHW_MIN       Fixed-point-overflow exception (0C8)
***      DC    H'0'                Should not get here
*AHIK_Ret2 DS    0H
***         IPM   R3                  Fetch CC
*
*        Test 2.17.3.3: neg + neg = pos; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_NNP3   Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         LA    R4,AHIK_Ret3        Return address
         ST    R4,ESRET@           Save for ESPIE exit
         AHIK  R2,R0,-2            Fixed-point-overflow exception (0C8)
         DC    H'0'                Should not get here
AHIK_Ret3 DS    0H
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
*        ASI instruction test
***********************************************************************
TASI2    DS    0H
         USING *,R15               Establish addressability
***********************************************************************
*
*        Test 2.18.3.1: pos + pos = neg; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_PPN3   Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         ST    R0,FW               Initialize destination
         LA    R4,ASI_Ret1         Return address
         ST    R4,ESRET@           Save for ESPIE exit
         ASI   FW,1                Fixed-point-overflow exception (0C8)
         DC    H'0'                Should not get here
ASI_Ret1 DS    0H
         IPM   R3                  Fetch CC
         L     R2,FW               Get result
*
*        Test 2.18.3.2: neg + neg = zero; CC=3; cannot occur
*
***      SR    R0,R0               Set CC
***      LMG   R0,R1,TST_AR_NNZ3   Initialize registers 0 and 1
***      LG    R2,InitGR2          Initialize register 2
***      ST    R0,FW               Initialize destination
***      LA    R4,ASI_Ret2         Return address
***      ST    R4,ESRET@           Save for ESPIE exit
***      ASI   FW,-1               Fixed-point-overflow exception (0C8)
***      DC    H'0'                Should not get here
*ASI_Ret2 DS    0H
***      IPM   R3                  Fetch CC
***      L     R2,FW               Get result
*
*        Test 2.18.3.3: neg + neg = pos; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_NNP3   Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         ST    R0,FW               Initialize destination
         LA    R4,ASI_Ret3         Return address
         ST    R4,ESRET@           Save for ESPIE exit
         ASI   FW,-2               Fixed-point-overflow exception (0C8)
         DC    H'0'                Should not get here
ASI_Ret3 DS    0H
         IPM   R3                  Fetch CC
         L     R2,FW               Get result
***********************************************************************
*
*        End test
*
         BR    R14                 Return to caller
*
         DROP  R15                 End addressability
*
***********************************************************************
*        AGHIK instruction test
***********************************************************************
TAGHIK2  DS    0H
         USING *,R15               Establish addressability
***********************************************************************
*
*        Test 2.19.3.1: pos + pos = neg; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AGR_PPN3  Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         LA    R4,AGHIK_Ret1       Return address
         ST    R4,ESRET@           Save for ESPIE exit
         AGHIK  R2,R0,1            Fixed-point-overflow exception (0C8)
         DC    H'0'                Should not get here
AGHIK_Ret1 DS    0H
         IPM   R3                  Fetch CC
*
*        Test 2.19.3.2: neg + neg = zero; CC=3; cannot occur
*
***      SR    R0,R0               Set CC
***      LMG   R0,R1,TST_AGR_NNZ3  Initialize registers 0 and 1
***      LG    R2,InitGR2          Initialize register 2
***      LA    R4,AGHIK_Ret2       Return address
***      ST    R4,ESRET@           Save for ESPIE exit
***      AGHIK  R2,R0,SHW_MIN      Fixed-point-overflow exception (0C8)
***      DC    H'0'                Should not get here
*AGHIK_Ret2 DS    0H
***      IPM   R3                  Fetch CC
*
*        Test 2.19.3.3: neg + neg = pos; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AGR_NNP3  Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         LA    R4,AGHIK_Ret3       Return address
         ST    R4,ESRET@           Save for ESPIE exit
         AGHIK  R2,R0,-2           Fixed-point-overflow exception (0C8)
         DC    H'0'                Should not get here
AGHIK_Ret3 DS    0H
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
*        AGFI instruction test
***********************************************************************
TAGFI2   DS    0H
         USING *,R15               Establish addressability
***********************************************************************
*
*        Test 2.20.3.1: pos + pos = neg; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AGR_PPN3  Initialize registers 0 and 1
         LA    R4,AGFI_Ret1        Return address
         ST    R4,ESRET@           Save for ESPIE exit
         AGFI   R0,1               Fixed-point-overflow exception (0C8)
         DC    H'0'                Should not get here
AGFI_Ret1 DS    0H
         IPM   R3                  Fetch CC
*
*        Test 2.20.3.2: neg + neg = zero; CC=3; cannot occur
*
***      SR    R0,R0               Set CC
***      LMG   R0,R1,TST_AGR_NNZ3  Initialize registers 0 and 1
***      LA    R4,AGFI_Ret2        Return address
***      ST    R4,ESRET@           Save for ESPIE exit
***      AGFI  R0,SFW_MIN          Fixed-point-overflow exception (0C8)
***      DC    H'0'                Should not get here
*AGFI_Ret2 DS    0H
***      IPM   R3                  Fetch CC
*
*        Test 2.20.3.3: neg + neg = pos; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AGR_NNP3  Initialize registers 0 and 1
         LA    R4,AGFI_Ret3        Return address
         ST    R4,ESRET@           Save for ESPIE exit
         AGFI   R0,-2              Fixed-point-overflow exception (0C8)
         DC    H'0'                Should not get here
AGFI_Ret3 DS    0H
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
*        AGSI instruction test
***********************************************************************
TAGSI2   DS    0H
         USING *,R15               Establish addressability
***********************************************************************
*
*        Test 2.21.3.1: pos + pos = neg; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AGR_PPN3  Initialize registers 0 and 1
         STG   R0,DW               Initialize destination
         LA    R4,AGSI_Ret1        Return address
         ST    R4,ESRET@           Save for ESPIE exit
         AGSI  DW,1                Fixed-point-overflow exception (0C8)
         DC    H'0'                Should not get here
AGSI_Ret1 DS    0H
         IPM   R3                  Fetch CC
         LG    R2,DW               Get result
*
*        Test 2.21.3.2: neg + neg = zero; CC=3; cannot occur
*
***      SR    R0,R0               Set CC
***      LMG   R0,R1,TST_AGR_NNZ3  Initialize registers 0 and 1
***      STG   R0,DW               Initialize destination
***      LA    R4,AGSI_Ret2        Return address
***      ST    R4,ESRET@           Save for ESPIE exit
***      AGSI  DW,-1               Fixed-point-overflow exception (0C8)
***      DC    H'0'                Should not get here
*AGSI_Ret2 DS    0H
***      IPM   R3                  Fetch CC
***      LG    R2,DW               Get result
*
*        Test 2.21.3.3: neg + neg = pos; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AGR_NNP3  Initialize registers 0 and 1
         STG   R0,DW               Initialize destination
         LA    R4,AGSI_Ret3        Return address
         ST    R4,ESRET@           Save for ESPIE exit
         AGSI  DW,-2               Fixed-point-overflow exception (0C8)
         DC    H'0'                Should not get here
AGSI_Ret3 DS    0H
         IPM   R3                  Fetch CC
         LG    R2,DW               Get result
***********************************************************************
*
*        End test
*
         BR    R14                 Return to caller
*
         DROP  R15                 End addressability
*
***********************************************************************
*        AIH instruction test
***********************************************************************
TAIH2    DS    0H
         USING *,R15               Establish addressability
***********************************************************************
*
*        Test 2.22.3.1: pos + pos = neg; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AHHHR_PPN3 Initialize registers 0 and 1
         LA    R4,AIH_Ret1         Return address
         ST    R4,ESRET@           Save for ESPIE exit
         AIH   R0,1                Fixed-point-overflow exception (0C8)
         DC    H'0'                Should not get here
AIH_Ret1 DS    0H
         IPM   R3                  Fetch CC
*
*        Test 2.22.3.2: neg + neg = zero; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AHHHR_NNZ3 Initialize registers 0 and 1
         LA    R4,AIH_Ret2         Return address
         ST    R4,ESRET@           Save for ESPIE exit
         AIH   R0,SFW_MIN          Fixed-point-overflow exception (0C8)
         DC    H'0'                Should not get here
AIH_Ret2 DS    0H
         IPM   R3                  Fetch CC
*
*        Test 2.22.3.3: neg + neg = pos; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AHHHR_NNP3 Initialize registers 0 and 1
         LA    R4,AIH_Ret3         Return address
         ST    R4,ESRET@           Save for ESPIE exit
         AIH   R0,-2               Fixed-point-overflow exception (0C8)
         DC    H'0'                Should not get here
AIH_Ret3 DS    0H
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
*        LAA instruction test
***********************************************************************
TLAA2    DS    0H
         USING *,R15               Establish addressability
***********************************************************************
*
*        Test 2.23.3.1: pos + pos = neg; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_PPN3   Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         ST    R0,FW               Initialize second operand
         LA    R4,LAA_Ret1         Return address
         ST    R4,ESRET@           Save for ESPIE exit
         LAA   R0,R1,FW            Fixed-point-overflow exception (0C8)
         DC    H'0'                Should not get here
LAA_Ret1 DS    0H
         IPM   R3                  Fetch CC
         L     R2,FW               Get result
*
*        Test 2.23.3.2: neg + neg = zero; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_NNZ3   Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         ST    R0,FW               Initialize second operand
         LA    R4,LAA_Ret2         Return address
         ST    R4,ESRET@           Save for ESPIE exit
         LAA   R0,R1,FW            Fixed-point-overflow exception (0C8)
         DC    H'0'                Should not get here
LAA_Ret2 DS    0H
         IPM   R3                  Fetch CC
         L     R2,FW               Get result
*
*        Test 2.23.3.3: neg + neg = pos; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AR_NNP3   Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         ST    R0,FW               Initialize second operand
         LA    R4,LAA_Ret3         Return address
         ST    R4,ESRET@           Save for ESPIE exit
         LAA   R0,R1,FW            Fixed-point-overflow exception (0C8)
         DC    H'0'                Should not get here
LAA_Ret3 DS    0H
         IPM   R3                  Fetch CC
         L     R2,FW               Get result
***********************************************************************
*
*        End test
*
         BR    R14                 Return to caller
*
         DROP  R15                 End addressability
*
***********************************************************************
*        LAAG instruction test
***********************************************************************
TLAAG2   DS    0H
         USING *,R15               Establish addressability
***********************************************************************
*
*        Test 2.24.3.1: pos + pos = neg; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AGR_PPN3  Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         STG   R0,DW               Initialize second operand
         LA    R4,LAAG_Ret1        Return address
         ST    R4,ESRET@           Save for ESPIE exit
         LAAG  R0,R1,DW            Fixed-point-overflow exception (0C8)
         DC    H'0'                Should not get here
LAAG_Ret1 DS    0H
         IPM   R3                  Fetch CC
         LG    R2,DW               Get result
*
*        Test 2.24.3.2: neg + neg = zero; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AGR_NNZ3  Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         STG   R0,DW               Initialize second operand
         LA    R4,LAAG_Ret2        Return address
         ST    R4,ESRET@           Save for ESPIE exit
         LAAG  R0,R1,DW            Fixed-point-overflow exception (0C8)
         DC    H'0'                Should not get here
LAAG_Ret2 DS    0H
         IPM   R3                  Fetch CC
         LG    R2,DW               Get result
*
*        Test 2.24.3.3: neg + neg = pos; CC=3
*
         SR    R0,R0               Set CC
         LMG   R0,R1,TST_AGR_NNP3  Initialize registers 0 and 1
         LG    R2,InitGR2          Initialize register 2
         STG   R0,DW               Initialize second operand
         LA    R4,LAAG_Ret3        Return address
         ST    R4,ESRET@           Save for ESPIE exit
         LAAG  R0,R1,DW            Fixed-point-overflow exception (0C8)
         DC    H'0'                Should not get here
LAAG_Ret3 DS    0H
         IPM   R3                  Fetch CC
         LG    R2,DW               Get result
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
*        Add instruction tests
***********************************************************************
               DS    0D
TST_AR_ZZZ0    DC    X'ffffffff00000000',X'ffffffff00000000'
TST_AR_PNZ0    DC    X'ffffffff00000001',X'ffffffffffffffff'
TST_AR_NPZ0    DC    X'ffffffffffffffff',X'ffffffff00000001'
*
TST_AR_ZNN1    DC    X'ffffffff00000000',X'ffffffffffffffff'
TST_AR_PNN1    DC    X'ffffffff00000001',X'fffffffffffffffe'
TST_AR_NZN1    DC    X'ffffffffffffffff',X'ffffffff00000000'
TST_AR_NPN1    DC    X'fffffffffffffffe',X'ffffffff00000001'
TST_AR_NNN1    DC    X'ffffffffffffffff',X'fffffffffffffffe'
*
TST_AR_ZPP2    DC    X'ffffffff00000000',X'ffffffff00000001'
TST_AR_PZP2    DC    X'ffffffff00000001',X'ffffffff00000000'
TST_AR_PPP2    DC    X'ffffffff00000001',X'ffffffff00000002'
TST_AR_PNP2    DC    X'ffffffff00000002',X'ffffffffffffffff'
TST_AR_NPP2    DC    X'ffffffffffffffff',X'ffffffff00000002'
*
TST_AR_PPN3    DC    X'ffffffff7fffffff',X'ffffffff00000001'
TST_AR_NNZ3    DC    X'ffffffff80000000',X'ffffffff80000000'
TST_AR_NNP3    DC    X'ffffffff80000001',X'fffffffffffffffe'
*
***********************************************************************
*
TST_AGR_ZZZ0   DC    X'0000000000000000',X'0000000000000000'
TST_AGR_PNZ0   DC    X'0000000000000001',X'ffffffffffffffff'
TST_AGR_NPZ0   DC    X'ffffffffffffffff',X'0000000000000001'
*
TST_AGR_ZNN1   DC    X'0000000000000000',X'ffffffffffffffff'
TST_AGR_PNN1   DC    X'0000000000000001',X'fffffffffffffffe'
TST_AGR_NZN1   DC    X'ffffffffffffffff',X'0000000000000000'
TST_AGR_NPN1   DC    X'fffffffffffffffe',X'0000000000000001'
TST_AGR_NNN1   DC    X'ffffffffffffffff',X'fffffffffffffffe'
*
TST_AGR_ZPP2   DC    X'0000000000000000',X'0000000000000001'
TST_AGR_PZP2   DC    X'0000000000000001',X'0000000000000000'
TST_AGR_PPP2   DC    X'0000000000000001',X'0000000000000002'
TST_AGR_PNP2   DC    X'0000000000000002',X'ffffffffffffffff'
TST_AGR_NPP2   DC    X'ffffffffffffffff',X'0000000000000002'
*
TST_AGR_PPN3   DC    X'7fffffffffffffff',X'0000000000000001'
TST_AGR_NNZ3   DC    X'8000000000000000',X'8000000000000000'
TST_AGR_NNP3   DC    X'8000000000000001',X'fffffffffffffffe'
*
*TST_AGFR_NNZ3  DC    X'8000000000000000',X'0000000080000000' N/A
*
***********************************************************************
*
TST_AHHHR_ZZZ0 DC    X'00000000ffffffff',X'00000000ffffffff'
TST_AHHHR_PNZ0 DC    X'00000001ffffffff',X'ffffffffffffffff'
TST_AHHHR_NPZ0 DC    X'ffffffffffffffff',X'00000001ffffffff'
*
TST_AHHHR_ZNN1 DC    X'00000000ffffffff',X'ffffffffffffffff'
TST_AHHHR_PNN1 DC    X'00000001ffffffff',X'fffffffeffffffff'
TST_AHHHR_NZN1 DC    X'ffffffffffffffff',X'00000000ffffffff'
TST_AHHHR_NPN1 DC    X'fffffffeffffffff',X'00000001ffffffff'
TST_AHHHR_NNN1 DC    X'ffffffffffffffff',X'fffffffeffffffff'
*
TST_AHHHR_ZPP2 DC    X'00000000ffffffff',X'00000001ffffffff'
TST_AHHHR_PZP2 DC    X'00000001ffffffff',X'00000000ffffffff'
TST_AHHHR_PPP2 DC    X'00000001ffffffff',X'00000002ffffffff'
TST_AHHHR_PNP2 DC    X'00000002ffffffff',X'ffffffffffffffff'
TST_AHHHR_NPP2 DC    X'ffffffffffffffff',X'00000002ffffffff'
*
TST_AHHHR_PPN3 DC    X'7fffffffffffffff',X'00000001ffffffff'
TST_AHHHR_NNZ3 DC    X'80000000ffffffff',X'80000000ffffffff'
TST_AHHHR_NNP3 DC    X'80000001ffffffff',X'fffffffeffffffff'
*
***********************************************************************
*
TST_AHHLR_ZZZ0 DC    X'ffffffff00000000',X'00000000ffffffff'
TST_AHHLR_PNZ0 DC    X'ffffffff00000001',X'ffffffffffffffff'
TST_AHHLR_NPZ0 DC    X'ffffffffffffffff',X'00000001ffffffff'
*
TST_AHHLR_ZNN1 DC    X'ffffffff00000000',X'ffffffffffffffff'
TST_AHHLR_PNN1 DC    X'ffffffff00000001',X'fffffffeffffffff'
TST_AHHLR_NZN1 DC    X'ffffffffffffffff',X'00000000ffffffff'
TST_AHHLR_NPN1 DC    X'fffffffffffffffe',X'00000001ffffffff'
TST_AHHLR_NNN1 DC    X'ffffffffffffffff',X'fffffffeffffffff'
*
TST_AHHLR_ZPP2 DC    X'ffffffff00000000',X'00000001ffffffff'
TST_AHHLR_PZP2 DC    X'ffffffff00000001',X'00000000ffffffff'
TST_AHHLR_PPP2 DC    X'ffffffff00000001',X'00000002ffffffff'
TST_AHHLR_PNP2 DC    X'ffffffff00000002',X'ffffffffffffffff'
TST_AHHLR_NPP2 DC    X'ffffffffffffffff',X'00000002ffffffff'
*
TST_AHHLR_PPN3 DC    X'ffffffff7fffffff',X'00000001ffffffff'
TST_AHHLR_NNZ3 DC    X'ffffffff80000000',X'80000000ffffffff'
TST_AHHLR_NNP3 DC    X'ffffffff80000001',X'fffffffeffffffff'
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
         DC    CL8'A',A(TA1),A(TA2) Mnemonic, CC=3 test, overflow test
         DC    CL8'AR',A(TAR1),A(TAR2)
         DC    CL8'ARK',A(TARK1),A(TARK2)
         DC    CL8'AY',A(TAY1),A(TAY2)
         DC    CL8'AG',A(TAG1),A(TAG2)
         DC    CL8'AGR',A(TAGR1),A(TAGR2)
         DC    CL8'AGRK',A(TAGRK1),A(TAGRK2)
         DC    CL8'AGF',A(TAGF1),A(TAGF2)
         DC    CL8'AGFR',A(TAGFR1),A(TAGFR2)
         DC    CL8'AH',A(TAH1),A(TAH2)
         DC    CL8'AHY',A(TAHY1),A(TAHY2)
         DC    CL8'AHI',A(TAHI1),A(TAHI2)
         DC    CL8'AGHI',A(TAGHI1),A(TAGHI2)
         DC    CL8'AHHHR',A(TAHHHR1),A(TAHHHR2)
         DC    CL8'AHHLR',A(TAHHLR1),A(TAHHLR2)
         DC    CL8'AFI',A(TAFI1),A(TAFI2)
         DC    CL8'AHIK',A(TAHIK1),A(TAHIK2)
         DC    CL8'ASI',A(TASI1),A(TASI2)
         DC    CL8'AGHIK',A(TAGHIK1),A(TAGHIK2)
         DC    CL8'AGFI',A(TAGFI1),A(TAGFI2)
         DC    CL8'AGSI',A(TAGSI1),A(TAGSI2)
         DC    CL8'AIH',A(TAIH1),A(TAIH2)
         DC    CL8'LAA',A(TLAA1),A(TLAA2)
         DC    CL8'LAAG',A(TLAAG1),A(TLAAG2)
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
* 1. The test values defined above (see labels beginning with "TST_")
*    contain values for the 64-bit registers R0 and R1. For the
*    Add Immediate tests, the R1 value is not used but its value must
*    be equal to the immediate value in the tested instruction.
*
***********************************************************************
*
         END   RPI2009A
