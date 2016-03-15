         TITLE 'Test LAM, LAMY, STAM, and STAMY'
***********************************************************************
* Start create : 03-01-2016
* 1st delivery :
* Designer     : J Ganci
* Programmer   : J Ganci
* Purpose      : Test LAM, LAMY, STAM, and STAMY
***********************************************************************
*
* Define constants
*
***********************************************************************
* General Purpose Register equates
R0       EQU   0
R1       EQU   1
R2       EQU   2
R3       EQU   3
R4       EQU   4
R5       EQU   5
R6       EQU   6
R7       EQU   7
R8       EQU   8
R9       EQU   9
R10      EQU   10
R11      EQU   11
R12      EQU   12
R13      EQU   13
R14      EQU   14
R15      EQU   15
*
* Access Register equates
AR0      EQU   0
AR1      EQU   1
AR2      EQU   2
AR3      EQU   3
AR4      EQU   4
AR5      EQU   5
AR6      EQU   6
AR7      EQU   7
AR8      EQU   8
AR9      EQU   9
AR10     EQU   10
AR11     EQU   11
AR12     EQU   12
AR13     EQU   13
AR14     EQU   14
AR15     EQU   15
*
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
RPI2003  CSECT ,
RPI2003  AMODE 24
RPI2003  RMODE 24
         B     RPI2003A-RPI2003(,R15)  Branch around header
         DC    AL1(RPI2003C-*),C'RPI2003:&SYSDATC:&SYSTIME'
RPI2003C DS    0C
RPI2003A DS    0H
         STM   R14,R12,12(R13)         Save caller's registers
         LR    R12,R15                 R12 = base register
         USING RPI2003,R12             Establish addressability
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
         MVC   WAEYE,=CL8'RPI2003'     Set eyecatcher
***********************************************************************
*
* 9A = LAM - Load Access Multiple
*
* operand1 is access register value
* operand2 is access register value
* operand3 is 32-bit or 64-bit RS-type storage address
*
*
* EB9A = LAMY - Load Access Multiple
*
* operand1 is access register value
* operand2 is access register value
* operand3 is 32-bit or 64-bit RSY-type storage address
*
*
* 9B = STAM - Store Access Multiple
*
* operand1 is access register value
* operand2 is access register value
* operand3 is 32-bit or 64-bit RS-type storage address
*
*
* EB9B = STAMY - Store Access Multiple
*
* operand1 is access register value
* operand2 is access register value
* operand3 is 32-bit or 64-bit RSY-type storage address
*
***********************************************************************
* 1. Test LAM and STAM
***********************************************************************
         SAM31 ,                   Ensure amode 31
*
*        Set registers for test script V commands
*
         LA    R10,InitSTV         R10 ---> initial STAM area values
         LA    R9,LAV              R9 ---> initial AR values; doubled
         LA    R8,SAV-8            R8 ---> prefix to area for STAM
*
         BAS   R14,InitAR          Initialize acc regs to known values
         BAS   R14,InitStor        Initialize destination storage
*
         LAM   AR0,AR15,LAV        Load all access registers
         STAM  AR0,AR15,SAV        Store all access registers
*
         BAS   R14,InitAR          Initialize acc regs to known values
         BAS   R14,InitStor        Initialize destination storage
*
         LAM   AR15,AR15,LAV+15*4  Load one access register
         STAM  AR15,AR15,SAV       Store one access register
*
         BAS   R14,InitAR          Initialize acc regs to known values
         BAS   R14,InitStor        Initialize destination storage
*
         LAM   AR14,AR1,LAV+14*4   Wrap load some access registers
         STAM  AR14,AR1,SAV        Wrap store some access registers
*
         SAM31 ,                   Restore amode 31
*
***********************************************************************
* 2. Test LAMY and STAMY
***********************************************************************
         SAM31 ,                   Ensure amode 31
*
*        Set registers for test script V commands
*
*                                  R10 ---> initial STAMY area values
         LARL  R9,YLAV             R9 ---> initial AR values; doubled
         LARL  R8,YSAV-8           R8 ---> prefix to area for STAMY
*
         BAS   R14,YInitAR         Initialize acc regs to known values
         BAS   R14,YInitStor       Initialize destination storage
*
         LAMY  AR0,AR15,YLAV       Load all access registers
         STAMY AR0,AR15,YSAV       Store all access registers
*
         BAS   R14,YInitAR         Initialize acc regs to known values
         BAS   R14,YInitStor       Initialize destination storage
*
         LAMY  AR15,AR15,YLAV+15*4 Load one access register
         STAMY AR15,AR15,YSAV      Store one access register
*
         BAS   R14,YInitAR         Initialize acc regs to known values
         BAS   R14,YInitStor       Initialize destination storage
*
         LAMY  AR14,AR1,YLAV+14*4  Wrap load some access registers
         STAMY AR14,AR1,YSAV       Wrap store some access registers
*
         SAM31 ,                   Restore amode 31
***********************************************************************
* Done
***********************************************************************
         B     EXIT                Done
***********************************************************************
* Use LAM to initialize access registers to known values
***********************************************************************
InitAR   DS    0H
         LAM   0,15,InitARV        Initial values for access regs: 0-15
         BR    R14                 Return
***********************************************************************
* Initialize area that is used to store access registers using STAM
***********************************************************************
InitStor DS    0H
         MVC   SAV,InitSTV         Reset store area to initial value
         BR    R14                 Return
***********************************************************************
* Use LAMY to initialize access registers to known values
***********************************************************************
YInitAR  DS    0H
         LAMY  0,15,InitYARV       Initial values for access regs: 0-15
         BR    R14                 Return
***********************************************************************
* Initialize area that is used to store access registers using STAMY
***********************************************************************
YInitStor DS   0H
         LARL  R15,YSAV                Store area
         MVC   0(L'YSAV,R15),InitSTV   Reset store area to init value
         BR    R14                     Return
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
***********************************************************************
*        Storage
***********************************************************************
         PRINT DATA
InitARV  DS    0A                  Initial values for access regs 0-15
AARV     DC    16A(X'A0A0A0A0'+((*-AARV)/4)*256*256*256+((*-AARV)/4)*25x
               6*256+((*-AARV)/4)*256+((*-AARV)/4))
LAV0     EQU   *
         DC    C'ALAVALAV'
LAV      DC    16A((*-LAV)/4)      Values for access regs: 0-15
LAV2     DC    16A((*-LAV2)/4)     Second copy for LAM   r,s when r > s
         DC    C'ALAVALAV'
         DS    0D                  Align on doubleword
SAV0     EQU   *
         DC    C'AAAAAAAA'
SAV      DS    CL64                Area to store access regs
         DC    C'BBBBBBBB'
*
         PRINT NODATA
*
InitSTV  DS    0CL64               Initial value for storage area
         DC    64C'5'
*
         DS    CL4096              Ensure beyond base register
*
*        Storage to test LAMY and STAMY
*
         DS    0D

InitYARV DS    0A                  Initial values for access regs: 0-15
BARV     DC    16A(X'B0B0B0B0'+((*-BARV)/4)*256*256*256+((*-BARV)/4)*25x
               6*256+((*-BARV)/4)*256+((*-BARV)/4))
YLAV0    EQU   *
         DC    C'YLAVYLAV'
YLAV     DC    16A((*-YLAV)/4)     Values for access regs: 0-15
YLAV2    DC    16A((*-YLAV2)/4)    Second copy for LAMY  r,s when r > s
         DC    C'ZLAVZLAV'
         DS    0D                  Align on doubleword
YSAV0    EQU   *
         DC    C'YYYYYYYY'
YSAV     DS    CL64                Area to store access regs
         DC    C'ZZZZZZZZ'
*
         END
