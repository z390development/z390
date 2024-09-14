* COPYRIGHT (C) PAUL A. SCOTT, 1989. ALL RIGHTS RESERVED.
* ANY USE OF THIS CODE MUST RETAIN THE ABOVE COPYRIGHT NOTICE
*
* MODULE     PSU004
* AUTHOR     P. SCOTT
*
* PSU004 IS RENT,REUS
*
* EXAMPLE:
*    //STEP1   EXEC PGM=PSU004,PARM=('1989')
*    //SYSOUT  DD  SYSOUT=*
*
*MCALL #=     1 LV= 1 PSU004   PENTER MAIN,VARS=(WRKDSECT,WRKLEN),STACKX
               =1
PSMSVA   DSECT ,                   SAVE AREA STRUCTURE
PSMPLI   DS    F                   UNUSED
PSMHSA   DS    A                   HIGH SAVE AREA PTR
PSMLSA   DS    A                   LOW SAVE AREA PTR
PSMRET   DS    A                   RETURN ADDRESS
PSMEPA   DS    A                   ENTRY POINT ADDRESS
PSMGPR0  DS    A                   REGISTER 0
PSMGPR1  DS    A                   REGISTER 1
PSMGPR2  DS    A                   REGISTER 2
PSMGPR3  DS    A                   REGISTER 3
PSMGPR4  DS    A                   REGISTER 4
PSMGPR5  DS    A                   REGISTER 5
PSMGPR6  DS    A                   REGISTER 6
PSMGPR7  DS    A                   REGISTER 7
PSMGPR8  DS    A                   REGISTER 8
PSMGPR9  DS    A                   REGISTER 9
PSMGPR10 DS    A                   REGISTER 10
PSMGPR11 DS    A                   REGISTER 11
PSMGPR12 DS    A                   REGISTER 12
PSMGDS   DS    A                   GLOBAL DATA STRUCTURE PTR
PSMLDS   DS    A                   LOCAL DATA STRUCTURE PTR
PSMTOS   DS    A                   TOP OF STACK PTR
PSMBOS   DS    A                   BOTTOM OF STACK PTR
PSMSVALN EQU   *-PSMSVA            LENGTH OF SAVE AREA STRUCTURE
PSMLVARS DS    0F                  START OF LOCAL VARIABLES
*MCALL #=     2 LV= 2          PSMRVAL 12              GET VALUE OF REGX
               ISTER
*MCALL #=     3 LV= 3          PSMASYM 12
*MEXIT #=     3 LV= 3 PSMASYM
*MEXIT #=     2 LV= 2 PSMRVAL
R0      EQU   0                           GENERATE REGISTER EQUATE
R1      EQU   1                           GENERATE REGISTER EQUATE
R2      EQU   2                           GENERATE REGISTER EQUATE
R4      EQU   4                           GENERATE REGISTER EQUATE
R5      EQU   5                           GENERATE REGISTER EQUATE
R6      EQU   6                           GENERATE REGISTER EQUATE
R7      EQU   7                           GENERATE REGISTER EQUATE
R8      EQU   8                           GENERATE REGISTER EQUATE
R9      EQU   9                           GENERATE REGISTER EQUATE
R10      EQU   10                           GENERATE REGISTER EQUATE
R11      EQU   11                           GENERATE REGISTER EQUATE
R12      EQU   12                           GENERATE REGISTER EQUATE
R14      EQU   14                           GENERATE REGISTER EQUATE
R15      EQU   15                           GENERATE REGISTER EQUATE
*MCALL #=     4 LV= 2          PSMRLSE ,
*MEXIT #=     4 LV= 2 PSMRLSE
PSU004    CSECT ,
       B     52(,15)            BRANCH AROUND CSECT PREFACE
         DC    AL1(39)            . LENGTH OF DUMP TEXT
         DC    CL9'PSU004'          . CSECT NAME
         DC    CL9'08/27/08'       . ASSEMBLY DATE
         DC    CL9'18.41'       . ASSEMBLY TIME
         DC    CL12'PSM 1.0.0'     . PRODUCT RELEASE ID
         DC    A(((PSMSVALN+WRKLEN+7)/8)*8)               . INITIAL STAX
               CK OFFSET
         DC    AL1(127),AL3(4096)  . STACK ALLOCATION
         STM   14,12,12(13)
         LR    12,15
         USING PSU004,12
         LR    2,13
         L     0,48(,12)
         AL    0,44(,12)
*MCALL #=     5 LV= 2          GETMAIN R,LV=(0)
*      OS/VS2 RELEASE 4 VERSION -- 10/21/75
    BAL   1,*+4                             INDICATE GETMAIN
         SVC   10                             ISSUE GETMAIN SVC
*MEXIT #=     5 LV= 2 GETMAIN
         LR    13,1
         LR    3,1
         LA    0,0(,13)
         L     1,44(,12)
         LR    14,0
         SR    15,15
         MVCL  0,14
         ST    2,4(,13)
         ST    13,8(,2)
         LM    1,2,24(2)
         A     3,44(,12)
         USING WRKDSECT-PSMSVALN,13
*MEXIT #=     1 LV= 1 PENTER
*
*---------------------------------------------------------------------*
*        MAINLINE                                                     *
*---------------------------------------------------------------------*
*
*        GET FULL FOUR DIGIT YEAR IN CHARACTER FORMAT
*        ACCOUNTING FOR CENTURY CHANGES.  USE CURRENT
*        YEAR UNLESS PARM SPECIFIES A YEAR.
*
*MCALL #=     6 LV= 1          PUSHREG R1                SAVE PARM ADDRX
               ESS
    DS    0H
*MCALL #=     7 LV= 2          PSMROP R1
*MCALL #=     8 LV= 3          PSMRVAL R1
*MCALL #=     9 LV= 4          PSMASYM 1
*MEXIT #=     9 LV= 4 PSMASYM
*MEXIT #=     8 LV= 3 PSMRVAL
*MEXIT #=     7 LV= 2 PSMROP
         ST    1,0(,3)
         LA    3,4*1(,3)
*MEXIT #=     6 LV= 1 PUSHREG
*
*MCALL #=    10 LV= 1          TIME  DEC
* /* MACDATE Y-1 72277                                               */
* /*
    LA    1,2(0,0)                      LOAD 1 TO SPECIFY UNIT
.SVC     SVC   11                                ISSUE TIME SVC
*MEXIT #=    10 LV= 1 TIME
*
         LR    R15,R1              COPY DATE
         SRL   R15,24              ISOLATE CENTURY
         SLL   R15,4               MAKE ROOM FOR SIGN
         LA    R15,12(,R15)        INSERT SIGN
         ST    R15,WRKDWORD+4      SAVE IN DOUBLEWORD
         CVB   R15,WRKDWORD        MAKE BINARY VALUE
         MH    R15,=H'100'         CONVERT TO YEARS
         AH    R15,=H'1900'        ADD IN BASE CENTURY
*
         SLL   R1,8                STRIP OFF CENTURY
         SRL   R1,24               STRIP OFF DAYS
         SLL   R1,4                MAKE ROOM FOR SIGN
         LA    R1,12(,R1)          INSERT SIGN
         ST    R1,WRKDWORD+4       SAVE IN DOUBLEWORD
         CVB   R1,WRKDWORD         MAKE BINARY VALUE
*
         AR    R1,R15              INCLUDE CENTURIES
         STH   R1,WRKYEAR          SAVE YEAR
*
*MCALL #=    11 LV= 1          POPREG R1                 RESTORE PARM AX
               DDRESS
    DS    0H
*MCALL #=    12 LV= 2          PSMROP R1
*MCALL #=    13 LV= 3          PSMRVAL R1
*MCALL #=    14 LV= 4          PSMASYM 1
*MEXIT #=    14 LV= 4 PSMASYM
*MEXIT #=    13 LV= 3 PSMRVAL
*MEXIT #=    12 LV= 2 PSMROP
*MCALL #=    15 LV= 2          PSMRVAL R1
*MCALL #=    16 LV= 3          PSMASYM 1
*MEXIT #=    16 LV= 3 PSMASYM
*MEXIT #=    15 LV= 2 PSMRVAL
         LA    1,4*1
         SR    3,1
*MCALL #=    17 LV= 2          PSMROP R1
*MCALL #=    18 LV= 3          PSMRVAL R1
*MCALL #=    19 LV= 4          PSMASYM 1
*MEXIT #=    19 LV= 4 PSMASYM
*MEXIT #=    18 LV= 3 PSMRVAL
*MEXIT #=    17 LV= 2 PSMROP
         L     1,0(,3)
*MEXIT #=    11 LV= 1 POPREG
*
         L     R1,0(,R1)           CALLER'S PARM
         LH    R15,0(,R1)          GET PARM LENGTH
         CH    R15,=H'4'           CORRECT LENGTH FOR YEAR ?
         BNE   A0010               NO,  IGNORE IT
         PACK  WRKDWORD,2(4,R1)    MAKE PACKED DECIMAL
         CVB   R1,WRKDWORD         MAKE BINARY
         STH   R1,WRKYEAR          SAVE YEAR
*
A0010    LH    R1,WRKYEAR          YEAR IN BINARY
         CVD   R1,WRKDWORD         MAKE PACKED DECIMAL
         UNPK  WRKYEARZ,WRKDWORD   MAKE ZONED DECIMAL
         OI    WRKYEARZ+3,C'0'     REMOVE SIGN
*
*        COMPUTE FIRST DAY OF EACH MONTH
*
         LA    R2,12               NUMBER OF MONTHS
         LA    R11,WRKDAY1-1       FIRST DAY OF MONTH TABLE
A0020    STH   R2,WRKMONTH         SAVE MONTH FOR DAYOFWK
*MCALL #=    20 LV= 1          PCALL DAYOFWK,(WRKMONTH,=H'1',WRKYEAR)
    LR    1,3
         LA    3,4*3(,3)
*MCALL #=    21 LV= 2          CALL  DAYOFWK,(WRKMONTH,=H'1',WRKYEAR),VX
               L,MF=(E,(1))
         CNOP  0,4
    B     *+8                               BRANCH AROUND VCON
IHB0021B DC    A(DAYOFWK)                         ENTRY POINT ADDRESS
*MCALL #=    22 LV= 3          IHBOPLST DAYOFWK,(WRKMONTH,=H'1',WRKYEARX
               ),,MF=(E,(1))
*MCALL #=    23 LV= 4          IHBINNRA (1)                    LOAD REGX
                1 WITH LIST ADDR
*MEXIT #=    23 LV= 4 IHBINNRA
         LA    14,WRKMONTH   PICKUP PARAMETER
         LA    15,=H'1'   PICKUP PARAMETER
         LA    0,WRKYEAR   PICKUP PARAMETER
         STM   14,0,0(1)                      STORE INTO PARAM. LIST
         MVI   8(1),X'80'                      SET LAST WORD BIT ON
*MEXIT #=    22 LV= 3 IHBOPLST
         L     15,IHB0021B                       LOAD 15 WITH ENTRY ADR
.CONTD   BALR  14,15                             BRANCH TO ENTRY POINT
*MEXIT #=    21 LV= 2 CALL
         LA    14,4*3
         SR    3,14
*MEXIT #=    20 LV= 1 PCALL
         STC   R15,0(R2,R11)       SAVE FIRST DAY OF MONTH
         BCT   R2,A0020            COMPUTE FOR ALL MONTHS
*
*        CHECK FOR LEAP YEAR
*
         MVC   WRKDAYS(12),DAYS    SET DEFAULT WEEKDAYS
*MCALL #=    24 LV= 1          PCALL DAYOFWK,(=H'2',=H'28',WRKYEAR)
    LR    1,3
         LA    3,4*3(,3)
*MCALL #=    25 LV= 2          CALL  DAYOFWK,(=H'2',=H'28',WRKYEAR),VL,X
               MF=(E,(1))
         CNOP  0,4
    B     *+8                               BRANCH AROUND VCON
IHB0025B DC    A(DAYOFWK)                         ENTRY POINT ADDRESS
*MCALL #=    26 LV= 3          IHBOPLST DAYOFWK,(=H'2',=H'28',WRKYEAR),X
               ,MF=(E,(1))
*MCALL #=    27 LV= 4          IHBINNRA (1)                    LOAD REGX
                1 WITH LIST ADDR
*MEXIT #=    27 LV= 4 IHBINNRA
         LA    14,=H'2'   PICKUP PARAMETER
         LA    15,=H'28'   PICKUP PARAMETER
         LA    0,WRKYEAR   PICKUP PARAMETER
         STM   14,0,0(1)                      STORE INTO PARAM. LIST
         MVI   8(1),X'80'                      SET LAST WORD BIT ON
*MEXIT #=    26 LV= 3 IHBOPLST
         L     15,IHB0025B                       LOAD 15 WITH ENTRY ADR
.CONTD   BALR  14,15                             BRANCH TO ENTRY POINT
*MEXIT #=    25 LV= 2 CALL
         LA    14,4*3
         SR    3,14
*MEXIT #=    24 LV= 1 PCALL
         LR    R2,R15              SAVE WEEKDAY OF 2/28
*MCALL #=    28 LV= 1          PCALL DAYOFWK,(=H'3',=H'1',WRKYEAR)
    LR    1,3
         LA    3,4*3(,3)
*MCALL #=    29 LV= 2          CALL  DAYOFWK,(=H'3',=H'1',WRKYEAR),VL,MX
               F=(E,(1))
         CNOP  0,4
    B     *+8                               BRANCH AROUND VCON
IHB0029B DC    A(DAYOFWK)                         ENTRY POINT ADDRESS
*MCALL #=    30 LV= 3          IHBOPLST DAYOFWK,(=H'3',=H'1',WRKYEAR),,X
               MF=(E,(1))
*MCALL #=    31 LV= 4          IHBINNRA (1)                    LOAD REGX
                1 WITH LIST ADDR
*MEXIT #=    31 LV= 4 IHBINNRA
         LA    14,=H'3'   PICKUP PARAMETER
         LA    15,=H'1'   PICKUP PARAMETER
         LA    0,WRKYEAR   PICKUP PARAMETER
         STM   14,0,0(1)                      STORE INTO PARAM. LIST
         MVI   8(1),X'80'                      SET LAST WORD BIT ON
*MEXIT #=    30 LV= 3 IHBOPLST
         L     15,IHB0029B                       LOAD 15 WITH ENTRY ADR
.CONTD   BALR  14,15                             BRANCH TO ENTRY POINT
*MEXIT #=    29 LV= 2 CALL
         LA    14,4*3
         SR    3,14
*MEXIT #=    28 LV= 1 PCALL
         CR    R15,R2              DID WEEKDAY WRAP AROUND ?
         BH    A0030               NO, COMPARE DIFFERENCE IN DAYS
         LA    R15,7(,R15)         ADJUST FOR WEEKDAY WRAP
A0030    SR    R15,R2              COMPUTE DIFFERENCE IN DAYS
         CH    R15,=H'1'           ONE DAY BETWEEN 2/28 AND 3/1 ?
         BNE   A0040               NO, IT'S A LEAP YEAR
         MVI   WRKDAYS+1,28        RESET
*
*        CLEAR CALENDAR TO ALL BLANKS
*
A0040    LA    R15,29              NUMBER OF LINES
         LA    R14,WRKLINE         START OF LINES
A0050    MVC   0(L'WRKLINE,R14),BLANKS CLEAR LINE
         LA    R14,L'WRKLINE(,R14) ADVANCE TO NEXT LINE
         BCT   R15,A0050           CLEAR NEXT LINE
*
*        FILL IN NAME OF EACH MONTH AND YEAR
*
         LA    R15,WRKLINE               START OF LINE
         MVI   0(R15),C'1'               EJECT TO TOP OF FORM
         MVC   1(7,R15),=C'JANUARY'      SET JANUARY
         MVC   24(4,R15),WRKYEARZ        SET YEAR
         MVC   32(8,R15),=C'FEBRUARY'    SET FEBRUARY
         MVC   55(4,R15),WRKYEARZ        SET YEAR
         MVC   63(5,R15),=C'MARCH'       SET MARCH
         MVC   86(4,R15),WRKYEARZ        SET YEAR
         MVC   94(5,R15),=C'APRIL'       SET APRIL
         MVC   117(4,R15),WRKYEARZ       SET YEAR
*
         LA    R15,10*L'WRKLINE(,R15)    SKIP TO NEXT ROW
         MVC   1(3,R15),=C'MAY'          SET MAY
         MVC   24(4,R15),WRKYEARZ        SET YEAR
         MVC   32(4,R15),=C'JUNE'        SET JUNE
         MVC   55(4,R15),WRKYEARZ        SET YEAR
         MVC   63(5,R15),=C'JULY'        SET JULY
         MVC   86(4,R15),WRKYEARZ        SET YEAR
         MVC   94(6,R15),=C'AUGUST'      SET AUGUST
         MVC   117(4,R15),WRKYEARZ       SET YEAR
*
         LA    R15,10*L'WRKLINE(,R15)    SKIP TO NEXT ROW
         MVC   1(9,R15),=C'SEPTEMBER'    SET SEPTEMBER
         MVC   24(4,R15),WRKYEARZ        SET YEAR
         MVC   32(7,R15),=C'OCTOBER'     SET OCTOBER
         MVC   55(4,R15),WRKYEARZ        SET YEAR
         MVC   63(8,R15),=C'NOVEMBER'    SET NOVEMBER
         MVC   86(4,R15),WRKYEARZ        SET YEAR
         MVC   94(8,R15),=C'DECEMBER'    SET DECEMBER
         MVC   117(4,R15),WRKYEARZ       SET YEAR
*
*        FILL IN NAME OF DAY OF WEEK FOR EACH MONTH
*
         LA    R14,3                     NUMBER OF ROWS
         LA    R15,WRKLINE               ADDRESS FIRST ROW
         SH    R15,=Y(9*L'WRKLINE)       BACK UP FOR LOOP
A0060    LA    R15,10*L'WRKLINE(,R15)    NEXT ROW
         MVC   1(27,R15),=C'SUN MON TUE WED THU FRI SAT'
         MVC   32(27,R15),1(R15)         FILL NEXT COLUMN
         MVC   63(27,R15),1(R15)         FILL NEXT COLUMN
         MVC   94(27,R15),1(R15)         FILL NEXT COLUMN
         BCT   R14,A0060                 ADVANCE TO NEXT ROW
*
*        UNDERLINE DAY OF WEEK FOR EACH MONTH
*
         LA    R14,3                     NUMBER OF ROWS
         LA    R15,WRKLINE               ADDRESS FIRST ROW
         SH    R15,=Y(8*L'WRKLINE)       BACK UP FOR LOOP
A0070    LA    R15,10*L'WRKLINE(,R15)    NEXT ROW
         MVI   1(R15),C'-'               UNDERLINE WEEKDAYS
         MVC   2(26,R15),1(R15)          FILL FIRST COLUMN
         MVC   32(27,R15),1(R15)         FILL NEXT COLUMN
         MVC   63(27,R15),1(R15)         FILL NEXT COLUMN
         MVC   94(27,R15),1(R15)         FILL NEXT COLUMN
         BCT   R14,A0070                 ADVANCE TO NEXT ROW
*
*        GENERATE CALENDAR OF DAYS IN EACH MONTH
*
         LA    R2,1                INITIALIZE MONTH
         LA    R4,3                NUMBER OF ROWS
         LA    R5,WRKLINE          START OF LINE
         SH    R5,=Y(7*L'WRKLINE)  BACK UP FOR LOOP
A0080    LA    R5,10*L'WRKLINE(,R5)  NEXT ROW
         SR    R6,R6               CLEAR OFFSET TO MONTH
         LA    R11,4               NUMBER OF MONTHS PER ROW
A0090    LA    R7,0(R6,R5)         ADDRESS MONTH
         SR    R8,R8               CLEAR DAYS IN MONTH
         SR    R9,R9               CLEAR DAY OF WEEK
         IC    R8,WRKDAYS-1(R2)    GET DAYS IN MONTH
         IC    R9,WRKDAY1-1(R2)    GET FIRST DAY OF WEEK
         LA    R10,1               GET FIRST DAY OF MONTH
A0100    LR    R15,R9              COPY DAY OF WEEK
         MH    R15,=H'4'           COMPUTE OFFSET IN LINE
         LA    R15,0(R15,R7)       COMPUTE ADDRESS IN LINE
         CVD   R10,WRKDWORD        MAKE DAY PACKED DECIMAL
         MVC   0(4,R15),=X'40202120' EDIT PATTERN ZZ9
         ED    0(4,R15),WRKDWORD+6 MAKE DAY DISPLAYABLE
         LA    R10,1(,R10)         NEXT DAY
         LA    R9,1(,R9)           NEXT DAY OF WEEK
         CH    R9,=H'7'            GREATER THAN SATURADAY ?
         BL    A0110               NO,  CONTINUE
         SR    R9,R9               YES, RESET TO SUNDAY
         LA    R7,L'WRKLINE(,R7)   GO TO NEXT LINE
A0110    BCT   R8,A0100            DO ALL DAYS
         LA    R2,1(,R2)           NEXT MONTH
         LA    R6,31(,R6)          ADDRESS OF NEXT MONTH
         BCT   R11,A0090           ALL MONTHS THIS ROW
         BCT   R4,A0080            ALL ROWS
*
*        PUT CALENDAR TO DDNAME SYSOUT
*
*MCALL #=    32 LV= 1          OPEN  (DCB,(OUTPUT))
*MCALL #=    33 LV= 2          IHBERMAC 63,,             INVALID TYPE OX
               PERAND     M0626
         MNOTE 12,'***  IHB002  INVALID TYPE OPERAND SPECIFIED-'
*MEXIT #=    33 LV= 2 IHBERMAC
*MEXIT #=    32 LV= 1 OPEN
         LA    R2,29
         LA    R11,WRKLINE
*MCALL #=    34 LV= 1 PUT      PUT   DCB,(R11)
*MCALL #=    35 LV= 2 PUT    IHBINNRA DCB,(R11)
PUT    LA    1,DCB                              LOAD PARAMETER REG 1
.REGB    LR    0,R11                           LOAD PARAMETER REG 0
*MEXIT #=    35 LV= 2 IHBINNRA
         L     15,48(0,1)               LOAD PUT ROUTINE ADDR
         BALR  14,15                    LINK TO PUT ROUTINE
*MEXIT #=    34 LV= 1 PUT
         LA    R11,L'WRKLINE(,R11)
         BCT   R2,PUT
*MCALL #=    36 LV= 1          CLOSE (DCB)
*MCALL #=    37 LV= 2          IHBERMAC 63,,             INVALID TYPE OX
               PERAND     M0626
         MNOTE 12,'***  IHB002  INVALID TYPE OPERAND SPECIFIED-'
*MEXIT #=    37 LV= 2 IHBERMAC
*MEXIT #=    36 LV= 1 CLOSE
*
*        RETURN TO CALLER
*
*MCALL #=    38 LV= 1          PEXIT RC=0
PSU004       CSECT ,
    LR    1,13
         L     13,4(,13)
         LA    15,0
         ST    15,16(,13)
         L     0,48(,12)
         AL    0,44(,12)
*MCALL #=    39 LV= 2          FREEMAIN R,LV=(0),A=(1)
*      OS/VS2 RELEASE 3 VERSION  -- 10/25/74
.CTUC    LA    1,0(0,1)                          CLEAR HI ORDER BYTE
         SVC   10                             ISSUE FREEMAIN SVC
*MEXIT #=    39 LV= 2 FREEMAIN
         LM    14,12,12(13)
         OI    15(13),1           MARK RETURN
         BR    14
         LTORG ,
*MEXIT #=    38 LV= 1 PEXIT
         LTORG ,
*
DAYS     DC    AL1(31,29,31,30,31,30,31,31,30,31,30,31)
BLANKS   DC    256C' '
*
*MCALL #=    40 LV= 1 DCB      DCB   DDNAME=SYSOUT,DSORG=PS,MACRF=PM,REX
               CFM=FBA,LRECL=121,BLKSIZE=6171
*MCALL #=    41 LV= 2          IHB01 PS,PM,,,1,FBA,0,1,0,1,0,0,0,0,0,0,X
               NO,,0,,N
*MCALL #=    42 LV= 3          IHBERMAC 166,,,PS
         MNOTE 8,'***  IHB060   INVALID CODE FOR DEVD WITH DSORG=PS-IGNX
               ORED'
*MEXIT #=    42 LV= 3 IHBERMAC
*MEXIT #=    41 LV= 2 IHB01
*MCALL #=    43 LV= 2          IHBERMAC 101,CODE
         MNOTE 4,'***  IHB050  CODE OPERAND INCONSISTENT-IGNORED'
*MEXIT #=    43 LV= 2 IHBERMAC
*MCALL #=    44 LV= 2          IHBERMAC 102,MODE
         MNOTE 4,'***  IHB050  MODE OPERAND INCONSISTENT-IGNORED'
*MEXIT #=    44 LV= 2 IHBERMAC
*MCALL #=    45 LV= 2          IHBERMAC 103,STACK
         MNOTE 4,'***  IHB050  STACK OPERAND INCONSISTENT-IGNORED'
*MEXIT #=    45 LV= 2 IHBERMAC
*MCALL #=    46 LV= 2          IHBERMAC 104,PRTSP
         MNOTE 4,'***  IHB050  PRTSP OPERAND INCONSISTENT-IGNORED'
*MEXIT #=    46 LV= 2 IHBERMAC
*MCALL #=    47 LV= 2          IHBERMAC 105,TRTCH
         MNOTE 4,'***  IHB050  TRTCH OPERAND INCONSISTENT-IGNORED'
*MEXIT #=    47 LV= 2 IHBERMAC
*MCALL #=    48 LV= 2          IHBERMAC 106,DEN
         MNOTE 4,'***  IHB050  DEN OPERAND INCONSISTENT-IGNORED'
*MEXIT #=    48 LV= 2 IHBERMAC
*MCALL #=    49 LV= 2          IHBERMAC 107,MON
         MNOTE 4,'***  IHB050  MON OPERAND INCONSISTENT-IGNORED'
*MEXIT #=    49 LV= 2 IHBERMAC
*MCALL #=    50 LV= 2          IHBERMAC 108,MONDLY
         MNOTE 4,'***  IHB050  MONDLY OPERAND INCONSISTENT-IGNORED'
*MEXIT #=    50 LV= 2 IHBERMAC
*MCALL #=    51 LV= 2          IHBERMAC 109,WRU
         MNOTE 4,'***  IHB050  WRU OPERAND INCONSISTENT-IGNORED'
*MEXIT #=    51 LV= 2 IHBERMAC
*MCALL #=    52 LV= 2          IHBERMAC 110,IAM
         MNOTE 4,'***  IHB050  IAM OPERAND INCONSISTENT-IGNORED'
*MEXIT #=    52 LV= 2 IHBERMAC
*MCALL #=    53 LV= 2          IHBERMAC 111,EOM
         MNOTE 4,'***  IHB050  EOM OPERAND INCONSISTENT-IGNORED'
*MEXIT #=    53 LV= 2 IHBERMAC
*MCALL #=    54 LV= 2          IHBERMAC 112,EOT
         MNOTE 4,'***  IHB050  EOT OPERAND INCONSISTENT-IGNORED'
*MEXIT #=    54 LV= 2 IHBERMAC
*MCALL #=    55 LV= 2          IHBERMAC 111,CPRI
         MNOTE 4,'***  IHB050  CPRI OPERAND INCONSISTENT-IGNORED'
*MEXIT #=    55 LV= 2 IHBERMAC
*MCALL #=    56 LV= 2          IHBERMAC 113,CPOLL
         MNOTE 4,'***  IHB050  CPOLL OPERAND INCONSISTENT-IGNORED'
*MEXIT #=    56 LV= 2 IHBERMAC
*MCALL #=    57 LV= 2          IHBERMAC 54,,ERROPT
         MNOTE 12,'***  IHB002  INVALID KEYWORD OPERAND SPECIFIED-ERROPX
               T'
*MEXIT #=    57 LV= 2 IHBERMAC
*MEXIT #=    40 LV= 1 DCB
*
WRKDSECT DSECT ,
WRKSAVEA DS    18F                 FIRST SAVE AREA
WRKSAVE2 DS    18F                 SECOND SAVE AREA
WRKID    EQU   WRKSAVEA,4
*
WRKDWORD DS    D                   DOUBLE WORD
WRKFWORD DS    F                   FULL WORD
WRKHWORD DS    H                   HALF WORD
WRKBYTE  DS    B                   BYTE
*
WRKYEARZ DS    CL4                 CHARACTER YEAR
WRKYEAR  DS    H                   NUMERIC YEAR
WRKMONTH DS    H                   NUMERIC MONTH
WRKDAY1  DS    12AL1               FIRST DAY OF EACH MONTH
WRKDAYS  DS    12AL1               NUMBER OF DAYS EACH MONTH
*
WRKLINE  DS    29CL121
*
WRKEND   DS    0D
WRKLEN   EQU   *-WRKDSECT
*
         EJECT ,
*---------------------------------------------------------------------*
*        DAYOFWK SUBROUTINE                                           *
*---------------------------------------------------------------------*
*
*MCALL #=    58 LV= 1 DAYOFWK  PENTER FUNC
*MCALL #=    59 LV= 2          PSMRVAL 12              GET VALUE OF REGX
               ISTER
*MCALL #=    60 LV= 3          PSMASYM 12
*MEXIT #=    60 LV= 3 PSMASYM
*MEXIT #=    59 LV= 2 PSMRVAL
         DROP  12
         DROP  13
*MCALL #=    61 LV= 2          PSMRLSE ,
*MEXIT #=    61 LV= 2 PSMRLSE
DAYOFWK    CSECT ,
       B     48(,15)            BRANCH AROUND CSECT PREFACE
         DC    AL1(39)            . LENGTH OF DUMP TEXT
         DC    CL9'DAYOFWK'          . CSECT NAME
         DC    CL9'08/27/08'       . ASSEMBLY DATE
         DC    CL9'18.41'       . ASSEMBLY TIME
         DC    CL12'PSM 1.0.0'     . PRODUCT RELEASE ID
         DC    A(((PSMSVALN++7)/8)*8)               . INITIAL STACK OFFX
               SET
         STM   14,12,12(13)
         LR    12,15
         USING DAYOFWK,12
         LR    2,13
         LR    13,3
         LA    0,0(,13)
         L     1,44(,12)
         LR    14,0
         SR    15,15
         MVCL  0,14
         MVC   PSMGDS-PSMSVA(PSMLVARS-PSMGDS,13),PSMGDS-PSMSVA(2)
         ST    2,4(,13)
         ST    13,8(,2)
         LM    1,2,24(2)
         A     3,44(,12)
         USING -PSMSVALN,13
*MEXIT #=    58 LV= 1 PENTER
*
         L     R4,0(,R1)           MONTH
         LH    R4,0(,R4)
*
         L     R5,4(,R1)           DAY
         LH    R5,0(,R5)
*
         L     R6,8(,R1)           YEAR
         LH    R6,0(,R6)
*
*        COMPUTE CENTURY = YEAR / 100
*        COMPUTE DECADE  = YEAR MOD 100
*
         XR    R14,R14             CLEAR HIGH WORD OF DIVISOR
         LR    R15,R6              GET DIVISOR ( YEAR )
         LA    R2,100              GET DIVIDEND
         DR    R14,R2              COMPUTE
         LR    R7,R15              R7 = CENTURY (QUOTIENT)
         LR    R8,R14              R8 = DECADE  (REMAINDER)
*
*        THE DAY OF THE MONTH INCREMENTS THE BASE BY THE
*        SAME NUMBER OF DAYS, THE PASSING OF EACH YEAR ADDS
*        A DAY, AND EACH LEAP YEAR ADDS A DAY
*
*        WEEKDAY = ( ( YEAR / 4 ) - CENTURY ) + ( YEAR / 400 ) +
*                  DAY + YEAR + BMC( MONTH-1 )
*
         LR    R9,R6               GET YEAR
         SRL   R9,2                WEEKDAY = ( YEAR / 4 )
*
         SR    R9,R7               WEEKDAY = WEEKDAY - CENTURY
*
         LR    R14,R7              GET CENTURY ( YEAR / 100 )
         SRL   R14,2               COMPUTE ( YEAR / 400 )
         AR    R9,R14              WEEKDAY = WEEKDAY + ( YEAR / 400 )
*
         AR    R9,R5               WEEKDAY = WEEKDAY + DAY
*
         AR    R9,R6               WEEKDAY = WEEKDAY + YEAR
*
         LR    R14,R4              GET MONTH
         BCTR  R14,0               MINUS 1
         SR    R15,R15             CLEAR REGISTER
         IC    R15,BMC(R14)        INDEX INTO BASE MONTH CODE TABLE
         AR    R9,R15              WEEKDAY = WEEKDAY + BMC( MONTH-1 )
*
*        FOR A LEAP YEAR, REMOVE THE EXTRA DAY BEFORE IT OCCURS,
*        I.E. JANUARY AND FEBRUARY
*
*        IF ( ( YEAR MOD 400 = 0 ) OR
*             ( ( YEAR MOD 4 = 0 ) AND ( DECADE 1/4 = 0 ) ) )
*           IF ( MONTH < 3 )
*              WEEKDAY = WEEKDAY - 1
*
         XR    R14,R14             CLEAR HIGH WORD OF DIVISOR
         LR    R15,R6              GET DIVISOR ( YEAR )
         LA    R2,400              GET DIVIDEND
         DR    R14,R2              COMPUTE ( YEAR MOD 400 )
*
         LTR   R14,R14             CHECK LEAP CENTURY ( YEAR MOD 400 )
         BZ    D0010               YES, ALSO LEAP YEAR
         XR    R14,R14             CLEAR HIGH WORD OF DIVISOR
         LR    R15,R6              GET DIVISOR ( YEAR )
         LA    R2,4                GET DIVIDEND
         DR    R14,R2              COMPUTE ( YEAR MOD 4 )
         LTR   R14,R14             POSSIBLE LEAP YEAR ?
         BNZ   D00005              NO,  SHOW NOT A LEAP YEAR
         LTR   R8,R8               YES, BUT IS IT A CENTURY YEAR ?
         BNZ   D0010               NO,  SO IT'S A LEAP YEAR
D00005   LA    R14,1               YES, NOT A LEAP CENTURY YEAR
*
D0010    LTR   R14,R14             LEAP YEAR ?
         BNZ   D0020               NO,  SKIP ADJUSTMENT
         CH    R4,=H'3'            JANUARY OR FEBRUARY ?
         BNL   D0020               NO,  SKIP ADJUSTMENT
         BCTR  R9,0                WEEKDAY = WEEKDAY - 1
*
*        RETURN ( WEEKDAY MOD 7 )
*
D0020    XR    R14,R14             CLEAR HIGH WORD OF DIVISOR
         LR    R15,R9              GET LOW WORD OF DIVISOR (WEEKDAY)
         LA    R2,7                GET DIVIDEND
         DR    R14,R2              COMPUTE WEEKDAY MOD 7
         LR    R15,R14             RETURN VALUE (WEEKDAY MOD 7)
*
*MCALL #=    62 LV= 1          PEXIT RC=(15)
DAYOFWK       CSECT ,
    LR    1,13
         L     13,4(,13)
*MCALL #=    63 LV= 2          PSMRVAL 15
*MCALL #=    64 LV= 3          PSMASYM 15
*MEXIT #=    64 LV= 3 PSMASYM
*MEXIT #=    63 LV= 2 PSMRVAL
         ST    15,16(,13)
         L     0,48(,12)
         AL    0,44(,12)
*MCALL #=    65 LV= 2          FREEMAIN R,LV=(0),A=(1)
*      OS/VS2 RELEASE 3 VERSION  -- 10/25/74
.CTUC    LA    1,0(0,1)                          CLEAR HI ORDER BYTE
         SVC   10                             ISSUE FREEMAIN SVC
*MEXIT #=    65 LV= 2 FREEMAIN
         LM    14,12,12(13)
         OI    15(13),1           MARK RETURN
         BR    14
         LTORG ,
*MEXIT #=    62 LV= 1 PEXIT
         LTORG ,
*
SUN      EQU   0
MON      EQU   1
TUE      EQU   2
WED      EQU   3
THU      EQU   4
FRI      EQU   5
SAT      EQU   6
*
*        BASE MONTH CODE TABLE
BMC      DC    AL1(SAT)            JANUARY   1, 0000
         DC    AL1(TUE)            FEBRUARY  1, 0000
         DC    AL1(TUE)            MARCH     1, 0000
         DC    AL1(FRI)            APRIL     1, 0000
         DC    AL1(SUN)            MAY       1, 0000
         DC    AL1(WED)            JUNE      1, 0000
         DC    AL1(FRI)            JULY      1, 0000
         DC    AL1(MON)            AUGUST    1, 0000
         DC    AL1(THU)            SEPTEMBER 1, 0000
         DC    AL1(SAT)            OCTOBER   1, 0000
         DC    AL1(TUE)            NOVEMBER  1, 0000
         DC    AL1(THU)            DECEMBER  1, 0000
*
         END   PSU004
* MZ390I total mnote warnings = 14
* MZ390I total mnote errors   = 4
* MZ390I max   mnote level    = 12
* MZ390I total mz390 errors   = 0
