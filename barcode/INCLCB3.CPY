*---------------------------------------------------------------------* 00010000
* MODULE: ENCXLOG                                                     * 00020000
*  USAGE: 4-STATE BARCODE CONVERTER - TRACE LOGGER                    * 00030000
*   DESC: LOGS TRACE MESSAGES.                                        * 00040000
*                                                                     * 00050000
*   REGISTERS AT ENTRY                                                * 00060000
*      REG      USAGE                                                 * 00070000
*      R0       REQUEST CODE.                                         * 00080000
*      R1-R9    N/A                                                   * 00090000
*      R10      GENERAL WORK AREA PTR                                 * 00100000
*      R11-R12  N/A                                                   * 00110000
*      R13      SAVE AREA PTR (18 FULLWORDS)                          * 00120000
*      R14      RETURN ADDRESS                                        * 00130000
*      R15      N/A                                                   * 00140000
*                                                                     * 00150000
*   REGISTERS AT RETURN                                               * 00160000
*      REG      USAGE                                                 * 00170000
*      R0-R15   SAME AS AT ENTRY.                                     * 00180000
*                                                                     * 00190000
*   REGISTER USAGE                                                    * 00200000
*      REG      USAGE                                                 * 00210000
*      R0-R8    WORK REGISTERS                                        * 00220000
*      R9       I/O WORK AREA PTR                                     * 00230000
*      R10      GENERAL WORK AREA PTR                                 * 00240000
*      R11-R12  CSECT ADDRESSABILITY                                  * 00250000
*      R13      SAVE AREA PTR                                         * 00260000
*      R14-R15  WORK REGISTERS                                        * 00270000
*---------------------------------------------------------------------* 00280000
         EJECT                                                          00290000
ENCXLOG  CSECT                                                          00300000
ENCXLOG  AMODE 31                                                       00310000
ENCXLOG  RMODE ANY                                                      00320000
         USING ENCXLOG,R12,R11       MAP THIS MODULE                    00330000
         USING GWA,R10               MAP GENERAL WORK AREA              00340000
         USING IOA,R9                MAP I/O WORK AREA                  00350000
         SPACE 2                                                        00360000
         STM   R14,R12,12(R13)       SAVE REGISTERS                     00370000
         LA    R12,0(,R15)           MAP...                             00380000
         LA    R11,4095(,R12)        ... THIS...                        00390000
         LA    R11,1(,R11)           ... CSECT                          00400000
         ST    R13,I3SAVA+4          OLD SAVEAREA                       00410000
         LA    R13,I3SAVA            NEW SAVEAREA                       00420000
         EJECT                                                          00430000
SCHEX    DS    0H -                  CHECK VALIDITY BASED ON USER FLAGS 00440000
         C     R0,=A(2)              INIT/TERM REQUEST?                 00450000
         BH    SCHTRC                  NO  - PROCEED                    00460000
         SPACE 2                                                        00470000
SCHINI   DS    0H -                  INIT/TERM REQUEST                  00480000
         CLI   IF1SHM,X'00'          FLAG SET?                          00490000
         BNE   RMX000                  YES - PROCEED                    00500000
         CLI   IF1STG,X'00'          FLAG SET?                          00510000
         BNE   RMX000                  YES - PROCEED                    00520000
         B     I3XIT                 QUIT                               00530000
         SPACE 2                                                        00540000
SCHTRC   DS    0H -                  TRACE REQUEST                      00550000
         L     R9,IOAPTR             LOAD IOA PTR                       00560000
         LTR   R9,R9                 VALID POINTER?                     00570000
         BNP   I3XIT                   NO  - QUIT                       00580000
         TM    DEBUGLOG+48,X'10'     LOG FILE OPEN?                     00590000
         BNO   I3XIT                   NO  - QUIT                       00600000
         SR    R4,R4                 CLEAR                              00610000
         LR    R5,R0                 XFER REQUEST CODE                  00620000
         D     R4,=A(100)            GET 1ST REQUEST CODE DIGIT         00630000
         SPACE 2                                                        00640000
XSHM     DS    0H -                                                     00650000
         C     R5,=A(4)              SHOW_INTERMEDIATES?                00660000
         BNE   ZSHM                    NO                               00670000
         CLI   IF1SHM,X'00'          FLAG SET?                          00680000
         BNE   RMX000                 YES - PROCEED                     00690000
         B     I3XIT                  NO  - QUIT                        00700000
ZSHM     DS    0H -                                                     00710000
         SPACE 2                                                        00720000
XSTG     DS    0H -                                                     00730000
         C     R5,=A(5)              SHOW_TABLE_GENERATION?             00740000
         BNE   ZSTG                    NO                               00750000
         CLI   IF1STG,X'00'          FLAG SET?                          00760000
         BNE   RMX000                 YES - PROCEED                     00770000
         B     I3XIT                  NO  - QUIT                        00780000
ZSTG     DS    0H -                                                     00790000
         EJECT                                                          00800000
RMX000   DS    0H -                  MATCH REQUEST TO METHOD            00810000
         LM    R3,R5,REQTABLE        LOAD TABLE PTRS                    00820000
RMX001   DS    0H -                                                     00830000
         C     R0,0(,R3)             MATCHING REQUEST ID?               00840000
         BE    RMX002                 YES - PROCEED                     00850000
         BXLE  R3,R4,RMX001          LOOP TILL ALL CHECKED              00860000
         B     I3XIT                 QUIT                               00870000
         SPACE 2                                                        00880000
RMX002   DS    0H -                                                     00890000
         L     R15,4(,R3)            LOAD ROUTINE ADDRESS               00900000
         L     R1,8(,R3)             LOAD MESSAGE TEXT ADDRESS          00910000
         LTR   R1,R1                 ASSOCIATED MESSAGE TEXT?           00920000
         BNP   RMX010                 NO                                00930000
         SPACE 2                                                        00940000
RMX003   DS    0H -                                                     00950000
         MVC   AMSG,0(R1)            LOAD THE SOURCE TEXT               00960000
         L     R1,12(,R3)            LOAD MESSAGE DATA OFFSET           00970000
         LA    R6,AMSG(R1)           POINT TO THE FIELD                 00980000
         SPACE 2                                                        00990000
RMX010   DS    0H -                                                     01000000
         BASR  R14,R15               INVOKE ROUTINE                     01010000
         SPACE 2                                                        01020000
I3XIT    DS    0H -                  RETURN TO THE CALLER               01030000
         L     R13,I3SAVA+4          CALLER'S SAVE AREA                 01040000
         LM    R14,R12,12(R13)       RETURN REGISTERS                   01050000
         BR    R14                   RETURN TO CALLER                   01060000
         EJECT                                                          01070000
*---------------------------------------------------------------------* 01080000
* REQUEST ROUTINES                                                    * 01090000
*---------------------------------------------------------------------* 01100000
         SPACE 2                                                        01110000
XOPEN    DS    0H -                                                     01120000
         ST    R14,I3PRET             SAVE RETURN ADDRESS               01130000
         XC    IOAPTR,IOAPTR          CLEAR I/O AREA PTR                01140000
         L     R0,=A(IOL)             LOAD AREA LENGTH                  01150000
         STORAGE OBTAIN,LENGTH=(0),LOC=BELOW,COND=YES                   01160000
         LTR   R15,R15                STORAGE GOTTEN?                   01170000
         BNZ   XCL01                   NO  - QUIT                       01180000
         ST    R1,IOAPTR              SAVE ADDRESS                      01190000
         L     R9,IOAPTR              RELOAD IT                         01200000
         MVC   DEBUGLOG(MDCBL),MDCB   INIT DCB                          01210000
         LA    R1,DEBUGLOG            DCB ADDRESS                       01220000
         ST    R1,OPENPL              SAVE IN PLIST                     01230000
         MVI   OPENPL,X'8F'           MARK AS ONE AND ONLY OUTPUT DCB   01240000
*        OPEN  MF=(E,OPENPL)          OPEN THE FILE                     01250000
         LTR   R15,R15                SUCCESSFUL?                       01260000
         BNZ   XCL01                   NO  - QUIT                       01270000
         L     R14,I3PRET             LOAD RETURN ADDRESS               01280000
         BR    R14                    RETURN TO CALLER                  01290000
         SPACE 2                                                        01300000
XCLOSE   DS    0H -                                                     01310000
         ST    R14,I3PRET             SAVE RETURN ADDRESS               01320000
XCL01    L     R9,IOAPTR              LOAD I/O AREA PTR                 01330000
         LTR   R9,R9                  VALID AREA?                       01340000
         BNP   XCL99                   NO  - QUIT                       01350000
XCL70    TM    DEBUGLOG+48,X'10'      DCB OPEN?                         01360000
         BNO   XCL80                   NO  - BYPASS CLOSE               01370000
*        CLOSE MF=(E,OPENPL)          CLOSE THE LOG                     01380000
XCL80    L     R0,=A(IOL)             LOAD AREA LENGTH                  01390000
         LR    R1,R9                  LOAD AREA ADDRESS                 01400000
         STORAGE RELEASE,LENGTH=(0),ADDR=(1)                            01410000
         XC    IOAPTR,IOAPTR          CLEAR I/O AREA PTR                01420000
XCL99    L     R14,I3PRET             LOAD RETURN ADDRESS               01430000
         BR    R14                    RETURN TO CALLER                  01440000
         EJECT                                                          01450000
*---------------------------------------------------------------------* 01460000
* PRINT MESSAGE ROUTINES                                              * 01470000
*---------------------------------------------------------------------* 01480000
         SPACE 2                                                        01490000
IMSG401  DS    0H -                    TRACKING DATA AS DIGIT STRING    01500000
         ST    R14,I3PRET              SAVE RETURN ADDRESS              01510000
         BAS   R14,FNDLAST             GET LAST FREE CHAR               01520000
         MVI   0(R1),X'7F'             INSERT QUOTE                     01530000
IM401A   DS    0H -                    TRACKING STRING LEN/ADDR         01540000
         LM    R3,R4,TRACKL            TRACKING STRING LEN/ADDR         01550000
         LTR   R3,R3                   VALID LENGTH?                    01560000
         BNP   IM401AZ                   NO                             01570000
         BCTR  R3,R0                   EXECUTE LENGTH                   01580000
         B     *+10                                                     01590000
         MVC   1(0,R1),0(R4)                                            01600000
         EX    R3,*-6                  MOVE DATA INTO MSG               01610000
IM401AZ  DS    0H -                                                     01620000
         BAS   R14,FNDLAST             GET LAST FREE CHAR               01630000
         MVI   0(R1),X'7F'             INSERT QUOTE                     01640000
         LA    R0,AMSG                 POINT TO MESSAGE                 01650000
         BAS   R14,IPRINT              PRINT THE MESSAGE                01660000
         L     R14,I3PRET              LOAD RETURN ADDRESS              01670000
         BR    R14                     RETURN TO CALLER                 01680000
         EJECT                                                          01690000
IMSG402  DS    0H -                    ROUTING DATA AS DIGIT STRING     01700000
         ST    R14,I3PRET              SAVE RETURN ADDRESS              01710000
         BAS   R14,FNDLAST             GET LAST FREE BYTE               01720000
         MVI   0(R1),X'7F'             INSERT QUOTE                     01730000
IM402A   DS    0H -                    TRACKING STRING LEN/ADDR         01740000
         LM    R3,R4,ROUTEL            TRACKING STRING LEN/ADDR         01750000
         LTR   R3,R3                   VALID LENGTH?                    01760000
         BNP   IM402AZ                   NO                             01770000
         BCTR  R3,R0                   EXECUTE LENGTH                   01780000
         B     *+10                                                     01790000
         MVC   1(0,R1),0(R4)                                            01800000
         EX    R3,*-6                  MOVE DATA INTO MSG               01810000
IM402AZ  DS    0H -                                                     01820000
         BAS   R14,FNDLAST             GET LAST FREE BYTE               01830000
         MVI   0(R1),X'7F'             INSERT QUOTE                     01840000
         LA    R0,AMSG                 POINT TO MESSAGE                 01850000
         BAS   R14,IPRINT              PRINT THE MESSAGE                01860000
         L     R14,I3PRET              LOAD RETURN ADDRESS              01870000
         BR    R14                     RETURN TO CALLER                 01880000
         EJECT                                                          01890000
IMSG403  DS    0H -                    ROUTING DATA AS 37 BIT VALUE     01900000
         ST    R14,I3PRET              SAVE RETURN ADDRESS              01910000
         LA    R1,LROUTE               PTR TO INT64 VALUE               01920000
         BAS   R14,PRTI64              PRINT TO TARGET AREA             01930000
         LA    R0,AMSG                 POINT TO MESSAGE                 01940000
         BAS   R14,IPRINT              PRINT THE MESSAGE                01950000
         L     R14,I3PRET              LOAD RETURN ADDRESS              01960000
         BR    R14                     RETURN TO CALLER                 01970000
         SPACE 2                                                        01980000
IMSG404  DS    0H -                    ROUTING DATA W/EMBEDDED LENGTH   01990000
         ST    R14,I3PRET              SAVE RETURN ADDRESS              02000000
         LA    R1,LROUTE               PTR TO INT64 VALUE               02010000
         BAS   R14,PRTI64              PRINT TO TARGET AREA             02020000
         LA    R0,AMSG                 POINT TO MESSAGE                 02030000
         BAS   R14,IPRINT              PRINT THE MESSAGE                02040000
         L     R14,I3PRET              LOAD RETURN ADDRESS              02050000
         BR    R14                     RETURN TO CALLER                 02060000
         SPACE 2                                                        02070000
IMSG405  DS    0H -                    BYTES WITH ROUTING DATA          02080000
         ST    R14,I3PRET              SAVE RETURN ADDRESS              02090000
         LA    R0,13                   BYTE ARRAY LENGTH                02100000
         LA    R1,BYTARRAY             BYTE ARRAY ADDRESS               02110000
         BAS   R14,PRTHEXA2            PRINT TO TARGET AREA             02120000
         LA    R0,AMSG                 POINT TO MESSAGE                 02130000
         BAS   R14,IPRINT              PRINT THE MESSAGE                02140000
         L     R14,I3PRET              LOAD RETURN ADDRESS              02150000
         BR    R14                     RETURN TO CALLER                 02160000
         SPACE 2                                                        02170000
IMSG406  DS    0H -                    BYTES WITH ROUTING/TRACKING DATA 02180000
         ST    R14,I3PRET              SAVE RETURN ADDRESS              02190000
         LA    R0,13                   BYTE ARRAY LENGTH                02200000
         LA    R1,BYTARRAY             BYTE ARRAY ADDRESS               02210000
         BAS   R14,PRTHEXA2            PRINT TO TARGET AREA             02220000
         LA    R0,AMSG                 POINT TO MESSAGE                 02230000
         BAS   R14,IPRINT              PRINT THE MESSAGE                02240000
         L     R14,I3PRET              LOAD RETURN ADDRESS              02250000
         BR    R14                     RETURN TO CALLER                 02260000
         SPACE 2                                                        02270000
IMSG407  DS    0H -                    FCS (11 BITS)                    02280000
         ST    R14,I3PRET              SAVE RETURN ADDRESS              02290000
         LA    R1,LFCS                 SOURCE ADDRESS                   02300000
         BAS   R14,PRTHEX3             PRINT TO TARGET AREA             02310000
         LA    R0,AMSG                 POINT TO MESSAGE                 02320000
         BAS   R14,IPRINT              PRINT THE MESSAGE                02330000
         L     R14,I3PRET              LOAD RETURN ADDRESS              02340000
         BR    R14                     RETURN TO CALLER                 02350000
         EJECT                                                          02360000
         USING RECTYPE,R8                                               02370000
IMSG408  DS    0H -                    CODEWORDS                        02380000
         LA    R4,CWARRAY              CODEWORD ARRAY ADDRESS           02390000
         MVC   PTYPE,=A(IM408DEC)      SET FORMATTING ROUTINE           02400000
         B     IMSGRTD                 PROCEED                          02410000
IMSG409  DS    0H -                    CODEWORDS W/ORIENTATION (CHAR J) 02420000
         LA    R4,CWARRAY              CODEWORD ARRAY ADDRESS           02430000
         MVC   PTYPE,=A(IM408DEC)      SET FORMATTING ROUTINE           02440000
         B     IMSGRTD                 PROCEED                          02450000
IMSG410  DS    0H -                    CHARACTERS                       02460000
         LA    R4,CHARRAY              CHARACTER ARRAY ADDRESS          02470000
         MVC   PTYPE,=A(IM408HEX)      SET FORMATTING ROUTINE           02480000
         B     IMSGRTD                 PROCEED                          02490000
IMSG411  DS    0H -                    CHARACTERS WITH FCS BITS 0-9     02500000
         LA    R4,CHARRAY              CHARACTER ARRAY ADDRESS          02510000
         MVC   PTYPE,=A(IM408HEX)      SET FORMATTING ROUTINE           02520000
*        B     IMSGRTD                 PROCEED                          02530000
         SPACE 2                                                        02540000
IMSGRTD  DS    0H -                    PRINT RECTYPE ARRAY              02550000
         ST    R14,I3PRET              SAVE RETURN ADDRESS              02560000
         LA    R5,0                    LOOP CONTROL                     02570000
IM408A   DS    0H -                                                     02580000
         C     R5,=A(10)               ARRAY EXHAUSTED?                 02590000
         BNL   IM408Z                   YUP                             02600000
         SR    R14,R14                 CLEAR                            02610000
         LR    R15,R5               CODE.JCL(LINKA)'                    02620000
         M     R14,=A(RCL)             CONVERT TO OFFSET                02630000
         LA    R8,0(R15,R4)            CURRENT CODEWORD ENTRY           02640000
         L     R15,PTYPE               FORMATTING RTN EPA               02650000
         BR    R15                     INVOKE IT                        02660000
         EJECT                                                          02670000
IM408DEC DS    0H -                    PRINT DECIMAL                    02680000
         L     R1,RTNUMBER             LOAD NUMBER VALUE                02690000
         CVD   R1,DPAKK                CONVERT TO DECIMAL               02700000
         MVC   DPATT,=XL16'40202020202020202020202020202120'            02710000
         ED    DPATT,DPAKK             MAKE PRINTABLE                   02720000
         MVC   0(4,R6),DPATT+12        STOW NUMERIC                     02730000
         B     IM408Y                  PROCEED                          02740000
IM408HEX DS    0H -                    PRINT HEXADECIMAL                02750000
         LA    R1,RTNUMBER+2          PTR - SOURCE CHAR                 02760000
         LA    R2,0(,R6)              PTR - TARGET CHARS                02770000
         BAS   R14,PHEX               CONVERT CHAR                      02780000
         LA    R1,RTNUMBER+3          PTR - SOURCE CHAR                 02790000
         LA    R2,2(,R6)              PTR - TARGET CHARS                02800000
         BAS   R14,PHEX               CONVERT CHAR                      02810000
*        B     IM408Y                  PROCEED                          02811000
IM408Y   LA    R6,6(,R6)               INCR TARGET PTR                  02812000
         LA    R5,1(,R5)               INCR COUNT                       02813000
         B     IM408A                  LOOP TILL DONE                   02814000
IM408Z   DS    0H -                                                     02815000
         LA    R0,AMSG                 POINT TO MESSAGE                 02816000
         BAS   R14,IPRINT              PRINT THE MESSAGE                02817000
         L     R14,I3PRET              LOAD RETURN ADDRESS              02818000
         BR    R14                     RETURN TO CALLER                 02819000
         DROP  R8                                                       02820000
         EJECT                                                          02830000
IMSG412  DS    0H -                    BARCODE                          02840000
         ST    R14,I3PRET              SAVE RETURN ADDRESS              02850000
IM412A   DS    0H -                                                     02860000
         LM    R3,R4,REPLYL            REPLY AREA LEN/ADDR              02870000
         LTR   R3,R3                   VALID LENGTH?                    02880000
         BNP   IM412AZ                   NO                             02890000
         BCTR  R3,R0                   EXECUTE LENGTH                   02900000
         B     *+10                                                     02910000
         MVC   0(0,R6),0(R4)                                            02920000
         EX    R3,*-6                  MOVE DATA INTO MSG               02930000
IM412AZ  DS    0H -                                                     02940000
         LA    R0,AMSG                 POINT TO MESSAGE                 02950000
         BAS   R14,IPRINT              PRINT THE MESSAGE                02960000
         L     R14,I3PRET              LOAD RETURN ADDRESS              02970000
         BR    R14                     RETURN TO CALLER                 02980000
         EJECT                                                          02990000
IMSG501  DS    0H -                    COUNT/VALUE/REVERSE              03000000
         ST    R14,I3PRET              SAVE RETURN ADDRESS              03010000
IX501D1  DS    0H -                                                     03020000
         LA    R6,AMSG+(M501D1-MSG501) TARGET AREA                      03030000
         L     R1,LCOUNT+0             LOAD COUNT VALUE                 03040000
         CVD   R1,DPAKK                CONVERT TO DECIMAL               03050000
         MVC   DPATT,=XL16'40202020202020202020202020202120'            03060000
         ED    DPATT,DPAKK             CONVERT NUMERIC                  03070000
         MVC   00(04,R6),DPATT+12      STOW IN TARGET AREA              03080000
IX501D2  DS    0H -                                                     03090000
         LA    R6,AMSG+(M501D2-MSG501) TARGET AREA                      03100000
         LA    R1,LCOUNT+2            PTR - SOURCE CHAR                 03110000
         LA    R2,0(,R6)              PTR - TARGET CHARS                03120000
         BAS   R14,PHEX               CONVERT CHAR                      03130000
         LA    R1,LCOUNT+3            PTR - SOURCE CHAR                 03140000
         LA    R2,2(,R6)              PTR - TARGET CHARS                03150000
         BAS   R14,PHEX               CONVERT CHAR                      03160000
IX501D3  DS    0H -                                                     03170000
         LA    R6,AMSG+(M501D3-MSG501) TARGET AREA                      03180000
         LA    R1,LCOUNT+6            PTR - SOURCE CHAR                 03190000
         LA    R2,0(,R6)              PTR - TARGET CHARS                03200000
         BAS   R14,PHEX               CONVERT CHAR                      03210000
         LA    R1,LCOUNT+7            PTR - SOURCE CHAR                 03220000
         LA    R2,2(,R6)              PTR - TARGET CHARS                03230000
         BAS   R14,PHEX               CONVERT CHAR                      03240000
IX501D4  DS    0H -                                                     03250000
         LA    R6,AMSG+(M501D4-MSG501) TARGET AREA                      03260000
         LM    R14,R15,LCOUNT         LOAD COUNT/REVERSE VALUES         03270000
         CR    R15,R14                COMPARE REVERSE VS COUNT          03280000
         BL    IX501AUS               REVERSE < COUNT                   03290000
         BH    IX501DX                REVERSE > COUNT                   03300000
IX501SYM MVC   0(L'ISYM,R6),ISYM       STOW CONSTANT                    03310000
         B     IX501DX                 PROCEED                          03320000
IX501AUS MVC   0(L'IAUS,R6),IAUS       STOW CONSTANT                    03330000
*        B     IX501DX                 PROCEED                          03340000
IX501DX  LA    R0,AMSG                 POINT TO MESSAGE                 03350000
         BAS   R14,IPRINT              PRINT THE MESSAGE                03360000
         L     R14,I3PRET              LOAD RETURN ADDRESS              03370000
         BR    R14                     RETURN TO CALLER                 03380000
         EJECT                                                          03390000
IMSG502  DS    0H -                    INDEX/VALUE                      03400000
         ST    R14,I3PRET              SAVE RETURN ADDRESS              03410000
IX502D1  DS    0H -                                                     03420000
         LA    R6,AMSG+(M502D1-MSG502) TARGET AREA                      03430000
         L     R1,LCOUNT+0             GET THE INDEX                    03440000
         CVD   R1,DPAKK                CONVERT TO DECIMAL               03450000
         MVC   DPATT,=XL16'40202020202020202020202020202120'            03460000
         ED    DPATT,DPAKK             CONVERT NUMERIC                  03470000
         MVC   00(04,R6),DPATT+12      STOW IN TARGET AREA              03480000
IX502D2  DS    0H -                                                     03490000
         LA    R6,AMSG+(M502D2-MSG502) TARGET AREA                      03500000
         LA    R1,LCOUNT+6            PTR - SOURCE CHAR                 03510000
         LA    R2,0(,R6)              PTR - TARGET CHARS                03520000
         BAS   R14,PHEX               CONVERT CHAR                      03530000
         LA    R1,LCOUNT+7            PTR - SOURCE CHAR                 03540000
         LA    R2,2(,R6)              PTR - TARGET CHARS                03550000
         BAS   R14,PHEX               CONVERT CHAR                      03560000
IX502X   DS    0H -                                                     03570000
         LA    R0,AMSG                 POINT TO MESSAGE                 03580000
         BAS   R14,IPRINT              PRINT THE MESSAGE                03590000
         L     R14,I3PRET              LOAD RETURN ADDRESS              03600000
         BR    R14                     RETURN TO CALLER                 03610000
         EJECT                                                          03620000
*---------------------------------------------------------------------* 03630000
* SUBROUTINES                                                         * 03640000
*---------------------------------------------------------------------* 03650000
         SPACE 2                                                        03660000
FNDLAST  DS    0H -                                                     03670000
         LA    R15,80                 LOOP CONTROL                      03680000
         LA    R1,0(R15,R6)           FIRST DATA PAST DATA              03690000
FLS001   BCTR  R1,R0                  PREV BYTE                         03700000
         CLI   0(R1),C' '             SPACE CHAR?                       03710000
         BE    FLS002                  YES - CONTINUE                   03720000
         LA    R1,1(,R1)              1ST SPACE CHAR PAST FIELD         03730000
         BR    R14                    RETURN TO CALLER                  03740000
FLS002   BCT   R15,FLS001             ... AND CONTINUE                  03750000
         LR    R1,R6                  POINT TO FIRST AREA BYTE          03760000
         BR    R14                    RETURN TO CALLER                  03770000
         SPACE 2                                                        03780000
PRTI64   DS    0H -                    PRINT INT64                      03790000
         STM   R14,R8,I3RRET           SAVE RETURN REGISTERS            03800000
         ZAP   DPAKK2,=P'0'            CLEAR ACCUMULATOR                03810000
         ICM   R1,B'1111',LROUTE+0     LOAD HI FULLWORD                 03820000
         CVD   R1,DPAKK                CONVERT TO DECIMAL               03830000
         MP    DPAKK,QDIVA4            CONVERT TO HI-ORDER VALUE        03840000
         AP    DPAKK2,DPAKK            ACCUMULATE                       03850000
         SR    R1,R1                   CLEAR                            03860000
         ICM   R1,B'0011',LROUTE+4     LOAD HI HALFWORD                 03870000
         CVD   R1,DPAKK                CONVERT TO DECIMAL               03880000
         MP    DPAKK,QDIVA2            SHIFT TO HI-HALFWORD             03890000
         AP    DPAKK2,DPAKK            ACCUMULATE                       03900000
         SR    R1,R1                   CLEAR                            03910000
         ICM   R1,B'0011',LROUTE+6     LOAD LO HALFWORD                 03920000
         CVD   R1,DPAKK                CONVERT TO DECIMAL               03930000
         AP    DPAKK2,DPAKK            ACCUMULATE                       03940000
         MVC   DPATT,=XL16'40212020202020202020202020202020'            03950000
         ED    DPATT,DPAKK2            MAKE PRINTABLE                   03960000
         TRT   DPATT,TRSIGD            FIND FIRST SIGNIFICANT DIGIT     03970000
         BNZ   P64YSIG                 PROCEED IF FOUND                 03980000
P64NSIG  DS    0H -                    NO SIGNIFICANT DIGIT             03990000
         MVI   0(R6),C'0'              INSERT ZERO                      04000000
         B     P64Z                    EXIT                             04010000
P64YSIG  DS    0H -                    SIGNIFICANT DIGIT                04020000
         LA    R14,DPATT+L'DPATT       1ST BYTE PAST PRINT FIELD        04030000
         SR    R14,R1                  FIELD LENGTH                     04040000
         BCTR  R14,R0                  EXECUTE LENGTH                   04050000
         B     *+10                                                     04060000
         MVC   00(00,R6),0(R1)         INSERT IN MSG                    04070000
         EX    R14,*-6                                                  04080000
P64Z     LM    R14,R8,I3RRET           LOAD RETURN REGISTERS            04090000
         BR    R14                     RETURN TO CALLER                 04100000
         EJECT                                                          04110000
PRTDECA4 DS    0H -                   PRINT DECIMAL ARRAY               04120000
*                                                                       04130000
* INPUTS  -                                                             04140000
*     R0  - SOURCE CHARACTER ARRAY LENGTH                               04150000
*     R1  - SOURCE CHARACTER ARRAY ADDRESS                              04160000
*     R6  - TARGET FIELD ADDRESS (80 BYTES)                             04170000
*                                                                       04180000
         STM   R14,R8,I3RRET          SAVE RETURN REGISTERS             04190000
PD4X00   LR    R3,R0                  SOURCE ARRAY LENGTH               04200000
         LR    R4,R1                  SOURCE ARRAY ADDRESS              04210000
PD4X01   DS    0H -                                                     04220000
         SR    R1,R1                  CLEAR                             04230000
         IC    R1,0(,R4)              LOAD CHARACTER                    04240000
         CVD   R1,DPAKK               CONVERT TO DECIMAL                04250000
         MVC   DPATT,=XL16'40202020202020202020202020202120'            04260000
         ED    DPATT,DPAKK            CONVERT NUMERIC                   04270000
         MVC   00(04,R6),DPATT         INSERT IN MSG                    04280000
PD4X98   DS    0H -                                                     04290000
         LA    R4,01(,R4)             INCR SOURCE                       04300000
         LA    R6,06(,R6)             INCR TARGET                       04310000
         BCT   R3,PD4X01              LOOP TILL ALL DONE                04320000
PD4X99   LM    R14,R8,I3RRET          LOAD RETURN REGISTERS             04330000
         BR    R14                    RETURN TO CALLER                  04340000
         SPACE 2                                                        04350000
PRTHEXA2 DS    0H -                   PRINT HEXADECIMAL ARRAY           04360000
*                                                                       04370000
* INPUTS  -                                                             04380000
*     R0  - SOURCE CHARACTER ARRAY LENGTH                               04390000
*     R1  - SOURCE CHARACTER ARRAY ADDRESS                              04400000
*     R6  - TARGET FIELD ADDRESS (80 BYTES)                             04410000
*                                                                       04420000
         STM   R14,R8,I3RRET          SAVE RETURN REGISTERS             04430000
PH2X00   LR    R3,R0                  SOURCE ARRAY LENGTH               04440000
         LR    R4,R1                  SOURCE ARRAY ADDRESS              04450000
PH2X01   DS    0H -                                                     04460000
         LA    R1,0(,R4)              PTR - SOURCE CHAR                 04470000
         LA    R2,0(,R6)              PTR - TARGET CHARS                04480000
         BAS   R14,PHEX               CONVERT CHAR                      04490000
PH2X98   DS    0H -                                                     04500000
         LA    R4,01(,R4)             INCR SOURCE                       04510000
         LA    R6,04(,R6)             INCR TARGET                       04520000
         BCT   R3,PH2X01              LOOP TILL ALL DONE                04530000
PH2X99   LM    R14,R8,I3RRET          LOAD RETURN REGISTERS             04540000
         BR    R14                    RETURN TO CALLER                  04550000
         SPACE 2                                                        04560000
PRTHEXA4 DS    0H -                   PRINT HEXADECIMAL ARRAY           04570000
*                                                                       04580000
* INPUTS  -                                                             04590000
*     R0  - SOURCE CHARACTER ARRAY LENGTH                               04600000
*     R1  - SOURCE CHARACTER ARRAY ADDRESS                              04610000
*     R6  - TARGET FIELD ADDRESS (80 BYTES)                             04620000
*                                                                       04630000
         STM   R14,R8,I3RRET          SAVE RETURN REGISTERS             04640000
PH4X00   LR    R3,R0                  SOURCE ARRAY LENGTH               04650000
         LR    R4,R1                  SOURCE ARRAY ADDRESS              04660000
PH4X01   DS    0H -                                                     04670000
         LA    R1,0(,R4)              PTR - SOURCE CHAR                 04680000
         LA    R2,0(,R6)              PTR - TARGET CHARS                04690000
         BAS   R14,PHEX               CONVERT CHAR                      04700000
         LA    R1,1(,R4)              PTR - SOURCE CHAR                 04710000
         LA    R2,2(,R6)              PTR - TARGET CHARS                04720000
         BAS   R14,PHEX               CONVERT CHAR                      04730000
PH4X98   DS    0H -                                                     04740000
         LA    R4,02(,R4)             INCR SOURCE                       04750000
         LA    R6,06(,R6)             INCR TARGET                       04760000
         BCT   R3,PH4X01              LOOP TILL ALL DONE                04770000
PH4X99   LM    R14,R8,I3RRET          LOAD RETURN REGISTERS             04780000
         BR    R14                    RETURN TO CALLER                  04790000
         SPACE 2                                                        04800000
PRTHEX3  DS    0H -                   PRINT HEXADECIMAL ARRAY           04810000
*                                                                       04820000
* INPUTS  -                                                             04830000
*     R1  - SOURCE FULLWORD PTR                                         04840000
*     R6  - TARGET FIELD ADDRESS (80 BYTES)                             04850000
         STM   R14,R8,I3RRET          SAVE RETURN REGISTERS             04860000
PH3X00   LA    R3,4                   SOURCE ARRAY LENGTH               04870000
         LR    R4,R1                  SOURCE ARRAY ADDRESS              04880000
         LA    R5,HEXBUF              TARGET PRINT AREA                 04890000
PH3X01   DS    0H -                                                     04900000
         LA    R1,0(,R4)              PTR - SOURCE CHAR                 04910000
         LA    R2,0(,R5)              PTR - TARGET CHARS                04920000
         BAS   R14,PHEX               CONVERT CHAR                      04930000
PH3X98   DS    0H -                                                     04940000
         LA    R4,01(,R4)             INCR SOURCE                       04950000
         LA    R5,02(,R5)             INCR TARGET                       04960000
         BCT   R3,PH3X01              LOOP TILL ALL DONE                04970000
         MVC   0(3,R6),HEXBUF+5       MOVE RESULT INTO TARGET           04980000
PH3X99   LM    R14,R8,I3RRET          LOAD RETURN REGISTERS             04990000
         BR    R14                    RETURN TO CALLER                  05000000
         EJECT                                                          05010000
IPRINT   DS    0H -                   PRINT TEXT LINE                   05020000
*                                                                       05030000
* INPUTS  -                                                             05040000
*     R0  - PTR TO MSG TEXT                                             05050000
*                                                                       05060000
         STM   R14,R1,I3WRET          SAVE RETURN REGISTERS             05070000
         LR    R14,R0                 XFER MESSAGE TEXT PTR             05080000
         MVC   DMSG,0(R14)            STOW MESSAGE BELOW THE LINE       05090000
         LA    R0,DMSG                MSG ADDRESS                       05100000
         LA    R1,DEBUGLOG            DCB ADDRESS                       05110000
         PUT   (1),(0)                WRITE THE LINE                    05120000
         LM    R14,R1,I3WRET          LOAD RETURN REGISTERS             05130000
         BR    R14                    RETURN TO CALLER                  05140000
         EJECT                                                          05150000
IDEBUG   DS    0H -                   PRINT DEBUG MSG                   05160000
*                                                                       05170000
* INPUTS  -                                                             05180000
*     R0  - SEQUENCE NUMBER                                             05190000
*     R1  - MSG LENGTH (MAX 80 BYTES)                                   05200000
*     R2  - MSG ADDRESS                                                 05210000
*                                                                       05220000
         STM   R14,R2,I3WRET          SAVE RETURN REGISTERS             05230000
         CVD   R0,XPAKK               CONVERT TO DECIMAL                05240000
         MVC   XPATT,=CL16'40212020202020202020202020202020'            05250000
         ED    XPATT,XPAKK            MAKE PRINTABLE                    05260000
         MVC   DMSG,M3SPACES                                            05270000
         MVC   DMSG(16),=CL16'DEBUG SEQ(XXXX):'                         05280000
         MVC   DMSG+10(4),XPATT+12                                      05290000
         LM    R14,R15,I3WRET+12      LOAD LENGTH/ADDRESS               05300000
         LTR   R14,R14                ANY DATA?                         05310000
         BNP   IDB002                   NO  - NOTHING TO INSERT         05320000
         BCTR  R14,R0                                                   05330000
         B     *+10                                                     05340000
         MVC   DMSG+17(0),0(R15)                                        05350000
         EX    R14,*-6                                                  05360000
IDB002   LA    R0,DMSG                MSG ADDRESS                       05370000
         LA    R1,DEBUGLOG            DCB ADDRESS                       05380000
         PUT   (1),(0)                WRITE THE LINE                    05390000
         LM    R14,R2,I3WRET          LOAD RETURN REGISTERS             05400000
         BR    R14                    RETURN TO CALLER                  05410000
         SPACE 2                                                        05420000
PHEX     DS    0H -                   PRINT HEXADECIMAL CHARS           05430000
*                                                                       05440000
* INPUTS  -                                                             05450000
*     R1  - SOURCE CHARACTER ARRAY ADDRESS                              05460000
*     R2  - TARGET FIELD ADDRESS                                        05470000
*                                                                       05480000
         SR    R0,R0                  CLEAR                             05490000
         IC    R0,0(,R1)              LOAD CHAR                         05500000
         SRL   R0,4                   PUSH HI TO LO                     05510000
         STC   R0,0(,R2)              STOW IN TARGET                    05520000
         SR    R0,R0                  CLEAR                             05530000
         IC    R0,0(,R1)              LOAD CHAR                         05540000
         N     R0,=A(X'0000000F')     ISOLATE LO-ORDER BITS             05550000
         STC   R0,1(,R2)              STOW IN TARGET                    05560000
         TR    0(2,R2),TRH2P          MAKE PRINTABLE                    05570000
         BR    R14                    RETURN TO CALLER                  05580000
         EJECT                                                          05590000
*---------------------------------------------------------------------* 05600000
* CONSTANTS                                                           * 05610000
*---------------------------------------------------------------------* 05620000
         SPACE 2                                                        05630000
M3SPACES DC    256C' '                                                  05640000
QDIVA4   DC    PL6'4294967296'       (X'100000000' - MAX INT + 1)       05650000
QDIVA2   DC    PL5'65536'            (X'10000' - MAX HALFWORD + 1)      05660000
         SPACE 2                                                        05670000
         PRINT NOGEN                                                    05680000
MDCB     DCB   DSORG=PS,MACRF=(PM),DDNAME=SYSPRINT,                    *05690000
               RECFM=FBA,LRECL=133                                      05700000
         PRINT GEN                                                      05710000
MDCBL    EQU   *-MDCB                                                   05720000
         PRINT GEN                                                      05730000
         SPACE 2                                                        05740000
REQTABLE DS    0H -                                                     05750000
         DC    A(RQT001)                                                05760000
         DC    A(16)                                                    05770000
         DC    A(RQT999-16)                                             05780000
RQT001   DS    0A -                                                     05790000
*----------------------------------------------------------*            05800000
* TABLE ENTRY LAYOUT                                       *            05810000
* OFFSET  CONTENTS                                         *            05820000
*   +0    REQUEST CODE                                     *            05830000
*   +4    ROUTINE ENTRY POINT                              *            05840000
*   +8    MESSAGE TEXT ADDRESS                             *            05850000
*   +12   OFFSET TO START OF MESSAGE DATA FIELD            *            05860000
*----------------------------------------------------------*            05870000
         DC    A(1),A(XOPEN),A(0),A(0)                                  05880000
         DC    A(2),A(XCLOSE),A(0),A(0)                                 05890000
         DC    A(401),A(IMSG401),A(MSG401),A(M401DATA-MSG401)           05900000
         DC    A(402),A(IMSG402),A(MSG402),A(M402DATA-MSG402)           05910000
         DC    A(403),A(IMSG403),A(MSG403),A(M403DATA-MSG403)           05920000
         DC    A(404),A(IMSG404),A(MSG404),A(M404DATA-MSG404)           05930000
         DC    A(405),A(IMSG405),A(MSG405),A(M405DATA-MSG405)           05940000
         DC    A(406),A(IMSG406),A(MSG406),A(M406DATA-MSG406)           05950000
         DC    A(407),A(IMSG407),A(MSG407),A(M407DATA-MSG407)           05960000
         DC    A(408),A(IMSG408),A(MSG408),A(M408DATA-MSG408)           05970000
         DC    A(409),A(IMSG409),A(MSG409),A(M409DATA-MSG409)           05980000
         DC    A(410),A(IMSG410),A(MSG410),A(M410DATA-MSG410)           05990000
         DC    A(411),A(IMSG411),A(MSG411),A(M411DATA-MSG411)           06000000
         DC    A(412),A(IMSG412),A(MSG412),A(M412DATA-MSG412)           06010000
         DC    A(501),A(IMSG501),A(MSG501),A(0)                         06020000
         DC    A(502),A(IMSG502),A(MSG502),A(0)                         06030000
RQT999   DS    0A -                                                     06040000
         SPACE 2                                                        06050000
TRH2P    DC    CL16'0123456789ABCDEF'                                   06060000
TRSIGD   DS    0CL256                                                   06070000
         DC    241X'00'                                                 06080000
         DC    C'123456789'                                             06090000
         DC    (256-(*-TRSIGD))X'00'                                    06100000
         SPACE 2                                                        06110000
         $MSGDLL                                                        06120000
         LTORG                                                          06130000
         EJECT                                                          06140000
*---------------------------------------------------------------------* 06150000
* WORK AREAS                                                          * 06160000
*---------------------------------------------------------------------* 06170000
I3SAVA   DS    18F                   REGISTER SAVE AREA                 06180000
IOAPTR   DC    A(0)                  I/O WORK AREA ADDRESS              06190000
         DS    0D -                  DOUBLEWORD ALIGNMENT               06200000
DPAKK    DS    PL8                   CONVERT BIN -> DEC                 06210000
DPAKK2   DS    PL8                   CONVERT BIN -> DEC                 06220000
XPAKK    DS    PL8                   CONVERT BIN -> DEC                 06230000
DPATT    DS    CL16                  CONVERT BIN -> DEC                 06240000
XPATT    DS    CL16                  CONVERT BIN -> DEC                 06250000
PTYPE    DS    A                     PRINT FORMATTING RTN ADDRESS       06260000
I3PRET   DS    A                     ROUTINE RETURN ADDRESS             06270000
I3RRET   DS    12A                   ROUTINE RETURN ADDRESS             06280000
I3WRET   DS    6A                    ROUTINE RETURN ADDRESS             06290000
HEXBUF   DS    CL8                   USED TO CONVERT HEX -> PRINTABLE   06300000
AMSG     DS    CL133                 MESSAGE WORK AREA                  06310000
         SPACE 2                                                        06320000
         DROP  R12,R11                                                  06330000
         DROP  R10                                                      06340000
         DROP  R9                                                       06350000
