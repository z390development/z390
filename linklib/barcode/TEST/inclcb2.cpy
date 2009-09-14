*---------------------------------------------------------------------* 00010000
* MODULE: ENCODE                                                      * 00020000
*  USAGE: 4-STATE BARCODE CONVERTER - ENCODING ROUTINES               * 00030000
*   DESC: ROUTINES USED TO ENCODE THE DATA.                           * 00040000
*                                                                     * 00050000
*   REGISTERS AT ENTRY                                                * 00060000
*      REG      USAGE                                                 * 00070000
*      R0       REQUEST CODE.                                         * 00080000
*      R1       POINTER TO FUNCTION PARAMETER LIST.                   * 00090000
*      R2-R9    N/A                                                   * 00100000
*      R10      GENERAL WORK AREA PTR                                 * 00110000
*      R11-R12  N/A                                                   * 00120000
*      R13      SAVE AREA PTR (18 FULLWORDS)                          * 00130000
*      R14      RETURN ADDRESS                                        * 00140000
*      R15      N/A                                                   * 00150000
*                                                                     * 00160000
*   REGISTERS AT RETURN                                               * 00170000
*      REG      USAGE                                                 * 00180000
*      R0-R14   SAME AS AT ENTRY.                                     * 00190000
*      R15      RETURN CODE (MEANING BASED ON FUNCTION).              * 00200000
*                                                                     * 00210000
*   REGISTER USAGE                                                    * 00220000
*      REG      USAGE                                                 * 00230000
*      R0-R9    WORK REGISTERS                                        * 00240000
*      R10      GENERAL WORK AREA PTR                                 * 00250000
*      R11-R12  CSECT ADDRESSABILITY                                  * 00260000
*      R13      SAVE AREA PTR                                         * 00270000
*      R14-R15  WORK REGISTERS                                        * 00280000
*---------------------------------------------------------------------* 00290000
         EJECT                                                          00300000
ENCODE   CSECT                                                          00310000
ENCODE   AMODE 31                                                       00320000
ENCODE   RMODE ANY                                                      00330000
         USING ENCODE,R12,R11        MAP THIS MODULE                    00340000
         USING GWA,R10               MAP GENERAL WORK AREA              00350000
         SPACE 2                                                        00360000
         STM   R14,R12,12(R13)       SAVE REGISTERS                     00370000
         LA    R12,0(,R15)           MAP...                             00380000
         LA    R11,4095(,R12)        ... THIS...                        00390000
         LA    R11,1(,R11)           ... CSECT                          00400000
         ST    R13,I2SAVA+4          SAVEAREA PTR                       00410000
         LA    R13,I2SAVA            NEW SAVEAREA                       00420000
         B     I2MAIN                PROCEED                            00430000
         SPACE 2                                                        00440000
I2EXIT   DS    0H -                  RETURN TO THE CALLER               00450000
         L     R13,I2SAVA+4          CALLER'S SAVE AREA                 00460000
         L     R14,12(,R13)          RETURN ADDRESS                     00470000
         L     R15,CCODE             RETURN CODE                        00480000
         LM    R0,R12,20(R13)        REMAINING REGISTERS                00490000
         BR    R14                   RETURN TO CALLER                   00500000
         EJECT                                                          00510000
I2MAIN   DS    0H -                  ENCODER LOGIC                      00520000
         LA    R0,401                EVENT ID                           00530000
         BAS   R14,M2LOG             LOG THE EVENT                      00540000
         LA    R0,402                EVENT ID                           00550000
         BAS   R14,M2LOG             LOG THE EVENT                      00560000
*---------------------------------------------------------------------* 00570000
* CONVERT ZIP CODE TO 64-BIT VALUE (ONLY LO-ORDER 37 BITS NEEDED)     * 00580000
*---------------------------------------------------------------------* 00590000
ZPN010   DS    0H -                                                     00600000
         XC    LROUTE,LROUTE         CLEAR 64-BIT ZIP CODE              00610000
         LM    R3,R4,ROUTEL          ZIP CODE STRING LEN/ADDR           00620000
         LTR   R3,R3                 VALID LENGTH?                      00630000
         BNP   ZPN010Z                 NO  - BYPASS                     00640000
         BCTR  R3,R0                 EXECUTE LENGTH                     00650000
         B     *+10                  PROCEED                            00660000
         PACK  DPAKK16,0(0,R4)                                          00670000
         EX    R3,*-6                CONVERT TO DECIMAL                 00680000
         DP    DPAKK16+00(16),QDIVFW         SPLIT BITS 00-31, 32-63    00690000
         DP    DPAKK16+08(08),QDIVHW         SPLIT BITS 32-47, 48-63    00691000
ZH00T31  CVB   R14,DPAKK16+00                CONVERT HI TO BINARY       00700000
         STCM  R14,B'1111',LROUTE+00         SAVE BITS 00-31            00700101
ZH32T47  ZAP   DPAKK16+00(08),DPAKK16+08(04) ISOLATE BITS 32-47         00700300
         CVB   R14,DPAKK16+00                CONVERT TO BINARY          00700400
         STCM  R14,B'0011',LROUTE+04         SAVE BITS 32-47            00701000
ZH48T63  ZAP   DPAKK16+00(08),DPAKK16+12(04) ISOLATE BITS 48-63         00702000
         CVB   R14,DPAKK16+00                CONVERT TO BINARY          00703000
         STCM  R14,B'0011',LROUTE+06         SAVE BITS 48-63            00704000
ZPN010Z  DS    0H -                                                     00730000
         LA    R0,403                EVENT ID                           00740000
         BAS   R14,M2LOG             LOG THE EVENT                      00750000
         EJECT                                                          00760000
*---------------------------------------------------------------------* 00770000
* PUT LENGTH INFORMATION INTO ZIP                                     * 00780000
*---------------------------------------------------------------------* 00790000
ZPN020   DS    0H -                                                     00800000
         L     R3,ROUTEL             ZIP CODE STRING LEN                00810000
         C     R3,=A(11)             MATCHING LENGTH?                   00820000
         BE    ZPN02L11               YUP                               00830000
         C     R3,=A(9)              MATCHING LENGTH?                   00840000
         BE    ZPN02L09               YUP                               00850000
         C     R3,=A(5)              MATCHING LENGTH?                   00860000
         BE    ZPN02L05               YUP                               00870000
         C     R3,=A(0)              MATCHING LENGTH?                   00880000
         BNE   ZPN020Z                NOPE                              00890000
ZPN02L00 DS    0H -                  0 CHARS                            00900000
         L     R2,=A(0)              ADDEND                             00910000
         B     ZPN020Y               PROCEED                            00920000
ZPN02L05 DS    0H -                  5 CHARS                            00930000
         L     R2,=A(1)              ADDEND                             00940000
         B     ZPN020Y               PROCEED                            00950000
ZPN02L09 DS    0H -                  9 CHARS                            00960000
         L     R2,=A(100001)         ADDEND                             00970000
         B     ZPN020Y               PROCEED                            00980000
ZPN02L11 DS    0H -                  11 CHARS                           00990000
         L     R2,=A(1000100001)     ADDEND                             01000000
*        B     ZPN020Y               PROCEED                            01010000
ZPN020Y  DS    0H -                                                     01020000
         LM    R14,R15,LROUTE        LOAD ZIP NUMBER                    01030000
         #LNADDI (R14,R15),(R2)      ADD LENGTH INFO                    01040000
         STM   R14,R15,LROUTE        SAVE ZIP NUMBER                    01050000
ZPN020Z  DS    0H -                                                     01060000
         LA    R0,404                EVENT ID                           01070000
         BAS   R14,M2LOG             LOG THE EVENT                      01080000
         SPACE 2                                                        01090000
*---------------------------------------------------------------------* 01100000
* STUFF ZIP INTO LOW END OF ZIP ARRAY                                 * 01110000
*---------------------------------------------------------------------* 01120000
ZPN030   DS    0H -                                                     01130000
         XC    BYTARRAY,BYTARRAY     CLEAR ZIP ARRAY                    01140000
         MVC   BYTARRAY+08(5),LROUTE+3                                  01150000
ZPN030Z  DS    0H -                                                     01160000
         LA    R0,405                EVENT ID                           01170000
         BAS   R14,M2LOG             LOG THE EVENT                      01180000
         EJECT                                                          01190000
*---------------------------------------------------------------------* 01200000
* PUT TRACKING DATA INTO BYTE ARRAY                                   * 01210000
*---------------------------------------------------------------------* 01220000
ZPN040   DS    0H -                  TRACKING DIGITS 0-1                01230000
         LA    R7,USPS_FSB_ENCODER_API_BYTE_CONVERSION_FAILED           01240000
         L     R4,TRACKP             POINT TO TRACKING DATA             01250000
ZMBS1    LA    R14,BYTARRAY          PTR TO BYTE ARRAY                  01260000
         LA    R15,13                BYTE COUNT                         01270000
         LA    R0,10                 MULTIPLICAND                       01280000
         STM   R14,R0,GPARMS         SAVE PARAMETER LIST                01290000
         BAS   R14,MBSHORT           MULTIPLY BYTES BY SHORT            01300000
         LTR   R15,R15               SUCCESSFUL?                        01310000
         BNZ   I2XERR                  NO  - EXIT                       01320000
ZASB1    LA    R14,BYTARRAY          PTR TO BYTE ARRAY                  01330000
         LA    R15,13                BYTE COUNT                         01340000
         IC    R0,0(,R4)             TRACKING BYTE 0                    01350000
         N     R0,=A(X'0000000F')    CONVERT TO INTEGER                 01360000
         STM   R14,R0,GPARMS         SAVE PARAMETER LIST                01370000
         BAS   R14,ASBYTES           ADD SHORT TO BYTES                 01380000
         LTR   R15,R15               SUCCESSFUL?                        01390000
         BNZ   I2XERR                  NO  - EXIT                       01400000
ZMBS2    LA    R14,BYTARRAY          PTR TO BYTE ARRAY                  01410000
         LA    R15,13                BYTE COUNT                         01420000
         LA    R0,5                  MULTIPLICAND                       01430000
         STM   R14,R0,GPARMS         SAVE PARAMETER LIST                01440000
         BAS   R14,MBSHORT           MULTIPLY BYTES BY SHORT            01450000
         LTR   R15,R15               SUCCESSFUL?                        01460000
         BNZ   I2XERR                  NO  - EXIT                       01470000
ZASB2    LA    R14,BYTARRAY          PTR TO BYTE ARRAY                  01480000
         LA    R15,13                BYTE COUNT                         01490000
         IC    R0,1(,R4)             TRACKING BYTE 1                    01500000
         N     R0,=A(X'0000000F')    CONVERT TO INTEGER                 01510000
         STM   R14,R0,GPARMS         SAVE PARAMETER LIST                01520000
         BAS   R14,ASBYTES           ADD SHORT TO BYTES                 01530000
         LTR   R15,R15               SUCCESSFUL?                        01540000
         BNZ   I2XERR                  NO  - EXIT                       01550000
         EJECT                                                          01560000
ZPN040A  DS    0H -                  REMAINING TRACKING DIGITS 2-20     01570000
         LA    R9,2                  REMAINING TRACKING DIGIT COUNT     01580000
ZPN040B  DS    0H -                                                     01590000
         C     R9,=A(20)             TRACKING DIGITS EXHAUSTED?         01600000
         BNL   ZPN040Z                YUP                               01610000
Z4MBS    LA    R14,BYTARRAY          PTR TO BYTE ARRAY                  01620000
         LA    R15,13                BYTE COUNT                         01630000
         LA    R0,10                 MULTIPLICAND                       01640000
         STM   R14,R0,GPARMS         SAVE PARAMETER LIST                01650000
         BAS   R14,MBSHORT           MULTIPLY BYTES BY SHORT            01660000
         LTR   R15,R15               SUCCESSFUL?                        01670000
         BNZ   I2XERR                  NO  - EXIT                       01680000
Z4ASB    LA    R14,BYTARRAY          PTR TO BYTE ARRAY                  01690000
         LA    R15,13                BYTE COUNT                         01700000
         IC    R0,0(R9,R4)           TRACKING BYTE N                    01710000
         N     R0,=A(X'0000000F')    CONVERT TO INTEGER                 01720000
         STM   R14,R0,GPARMS         SAVE PARAMETER LIST                01730000
         BAS   R14,ASBYTES           ADD SHORT TO BYTES                 01740000
         LTR   R15,R15               SUCCESSFUL?                        01750000
         BNZ   I2XERR                  NO  - EXIT                       01760000
ZPN040Y  LA    R9,1(,R9)             INCR COUNTER                       01770000
         B     ZPN040B               LOOP TILL DONE                     01780000
ZPN040Z  DS    0H -                                                     01790000
         LA    R0,406                EVENT ID                           01800000
         BAS   R14,M2LOG             LOG THE EVENT                      01810000
         EJECT                                                          01820000
*---------------------------------------------------------------------* 01830000
* GENERATE A CRC FCS CHARACTER ON THE 102-BIT VALUE                   * 01840000
*---------------------------------------------------------------------* 01850000
ZPN050   DS    0H -                                                     01860000
         LA    R14,BYTARRAY          PTR TO BYTE ARRAY                  01870000
         ST    R14,FPARMS            SAVE PARAMETER LIST                01880000
         BAS   R14,GENCRC            GENERATE CRC FCS                   01890000
         ST    R15,LFCS              SAVE THE VALUE                     01900000
ZPN050Z  DS    0H -                                                     01910000
         LA    R0,407                EVENT ID                           01920000
         BAS   R14,M2LOG             LOG THE EVENT                      01930000
         SPACE 2                                                        01940000
*---------------------------------------------------------------------* 01950000
* INITIALIZE N-OF-13 TABLES                                           * 01960000
*---------------------------------------------------------------------* 01970000
EXN010   DS    0H -                                                     01980000
         LA    R1,TBL2OF13           TABLE ADDRESS                      01990000
         LA    R0,TABLE_2_OF_13_SIZE TABLE LENGTH (IN INTS)             02000000
         SLL   R0,2                  TABLE LENGTH (IN BYTES)            02010000
         #CLRSTOR ADDR=(R1),LV=(R0)  CLEAR THE TABLE                    02020000
         LA    R1,TBL5OF13           TABLE ADDRESS                      02030000
         LA    R0,TABLE_5_OF_13_SIZE TABLE LENGTH (IN INTS)             02040000
         SLL   R0,2                  TABLE LENGTH (IN BYTES)            02050000
         #CLRSTOR ADDR=(R1),LV=(R0)  CLEAR THE TABLE                    02060000
EXN010A  DS    0H -                                                     02070000
         LA    R7,USPS_FSB_ENCODER_API_RETRIEVE_TABLE_FAILED            02080000
         LA    R14,TBL5OF13          TABLE PTR                          02090000
         LA    R15,5                 TABLE TYPE                         02100000
         LA    R0,TABLE_5_OF_13_SIZE TABLE ENTRY SIZE                   02110000
         STM   R14,R0,FPARMS         STOW PARAMETERS                    02120000
         BAS   R14,INITN13T          INITIALIZE THE TABLE               02130000
         LTR   R15,R15               INITIALIZED?                       02140000
         BNZ   I2XERR                 NO  - EXIT                        02150000
         LA    R14,TBL2OF13          TABLE PTR                          02160000
         LA    R15,2                 TABLE TYPE                         02170000
         LA    R0,TABLE_2_OF_13_SIZE TABLE ENTRY SIZE                   02180000
         STM   R14,R0,FPARMS         STOW PARAMETERS                    02190000
         BAS   R14,INITN13T          INITIALIZE THE TABLE               02200000
         LTR   R15,R15               INITIALIZED?                       02210000
         BNZ   I2XERR                 NO  - EXIT                        02220000
         EJECT                                                          02230000
*---------------------------------------------------------------------* 02240000
* CREATE THE CODEWORD ARRAY                                           * 02250000
*---------------------------------------------------------------------* 02260000
         USING RECTYPE,R2            MAP RECORD ARRAY ENTRY             02270000
CWD010   DS    0H -                                                     02280000
         L     R0,=A(TABLE_2_OF_13_SIZE) 2-SIZE                         02290000
         A     R0,=A(TABLE_5_OF_13_SIZE) 5-SIZE                         02300000
         ST    R0,CUMSIZE            SAVE COMBINED SIZE                 02310000
         LA    R6,CWARRAY            PTR TO CODE WORD ARRAY             02320000
         LA    R9,0                  SET LOOP CONTROL                   02330000
CWD010B  DS    0H -                                                     02340000
         C     R9,=A(10)             DONE YET?                          02350000
         BNL   CWD010Z                YUP                               02360000
         SR    R14,R14               CLEAR                              02370000
         LR    R15,R9                CURRENT ARRAY COUNT                02380000
         M     R14,=A(RCL)           NUMBER ARRAY ENTRY OFFSET          02390000
         LA    R2,0(R15,R6)          NUMBER ARRAY ENTRY ADDRESS         02400000
         XC    RECTYPE(RCL),RECTYPE  CLEAR THE ENTRY                    02410000
         MVC   RTBASE,CUMSIZE        SAVE COMBINED SIZE IN ENTRY        02420000
         C     R9,=A(0)              ENTRY[0]?                          02430000
         BNE   *+6                    NO                                02440000
         LR    R4,R2                 SAVE ENTRY ADDRESS                 02450000
         C     R9,=A(9)              ENTRY[9]?                          02460000
         BNE   *+6                    NO                                02470000
         LR    R5,R2                 SAVE ENTRY ADDRESS                 02480000
CWD010Y  LA    R9,1(,R9)             INCR INDEX                         02490000
         B     CWD010B               LOOP TILL DONE                     02500000
CWD010Z  DS    0H -                                                     02510000
         SPACE 2                                                        02520000
CWD015   DS    0H -                                                     02530000
         LR    R2,R4                 NUMBER ARRAY ENTRY[0] ADDRESS      02540000
         MVC   RTBASE,=A(659)        OVERRIDE BASE[0]                   02550000
         LR    R2,R5                 NUMBER ARRAY ENTRY[9] ADDRESS      02560000
         MVC   RTBASE,=A(636)        OVERRIDE BASE[9]                   02570000
         EJECT                                                          02580000
*---------------------------------------------------------------------* 02590000
* BUILD AND VALIDATE CODEWORD ARRAY                                   * 02600000
* PUT ORIENTATION INFO IN RIGHTMOST CODEWORD                          * 02610000
* PUT LEFTMOST FCS BIT INTO THE LEFTMOST CODEWORD                     * 02620000
*---------------------------------------------------------------------* 02630000
CWD020   DS    0H -                  PTR TO BYTE ARRAY                  02640000
         LA    R7,USPS_FSB_ENCODER_API_CODEWORD_CONVERSION_FAILED       02650000
         LA    R14,BYTARRAY          PTR TO BYTE ARRAY                  02660000
         LA    R15,13                BYTE COUNT                         02670000
         LA    R0,CWARRAY            PTR TO CODEWORD ARRAY              02680000
         LA    R1,10                 # CODEWORD ARRAY ENTRIES           02690000
         STM   R14,R1,FPARMS         SAVE PARAMETER LIST                02700000
         BAS   R14,CNVB2MB           CONVERT FROM BYTES TO MULTIBASE    02710000
         LTR   R15,R15               SUCCESSFUL?                        02720000
         BNZ   I2XERR                  NO  - EXIT                       02730000
         LA    R0,408                EVENT ID                           02740000
         BAS   R14,M2LOG             LOG THE EVENT                      02750000
         SPACE 2                                                        02760000
CWD030   DS    0H -                  VALIDATE CODEWORDS                 02770000
         LR    R2,R4                 NUMBER ARRAY ENTRY[0] ADDRESS      02780000
         CLC   RTNUMBER,=A(659)      VALID NUMBER VALUE?                02790000
         BNL   I2XERR                  NO  - EXIT                       02800000
         LR    R2,R5                 NUMBER ARRAY ENTRY[9] ADDRESS      02810000
         CLC   RTNUMBER,=A(636)      VALID NUMBER VALUE?                02820000
         BNL   I2XERR                  NO  - EXIT                       02830000
         SPACE 2                                                        02840000
CWD040   DS    0H -                  INSERT ORIENTATION INFO            02850000
         LR    R2,R5                 NUMBER ARRAY ENTRY[9] ADDRESS      02860000
         L     R0,RTNUMBER           GET NUMBER VALUE                   02870000
         SLL   R0,1                  MULTIPLY BY 2                      02880000
         ST    R0,RTNUMBER           SAVE UPDATED VALUE IN NUMBER[9]    02890000
         LA    R0,409                EVENT ID                           02900000
         BAS   R14,M2LOG             LOG THE EVENT                      02910000
         SPACE 2                                                        02920000
CWD050   DS    0H -                  PUT LEFTMOST FCS BIT IN CODEWORD   02930000
         L     R0,LFCS               LOAD FCS                           02940000
         SRL   R0,10                 SHIFT RIGHT 10 BITS                02950000
         LTR   R0,R0                 ANY BITS STILL ON?                 02960000
         BZ    CWD999                  NO                               02970000
         LR    R2,R4                 NUMBER ARRAY ENTRY[0] ADDRESS      02980000
         L     R0,RTNUMBER           GET NUMBER VALUE                   02990000
         A     R0,=A(659)            ADD THIS VALUE                     03000000
         ST    R0,RTNUMBER           SAVE UPDATED VALUE IN NUMBER[0]    03010000
CWD999   DS    0H -                                                     03020000
         LA    R0,409                EVENT ID                           03030000
         BAS   R14,M2LOG             LOG THE EVENT                      03040000
         DROP  R2                                                       03050000
         EJECT                                                          03060000
*---------------------------------------------------------------------* 03070000
* CONVERT FROM CODEWORDS TO 13-BIT CHARACTERS                         * 03080000
*---------------------------------------------------------------------* 03090000
CHD010   DS    0H -                                                     03100000
         LA    R7,USPS_FSB_ENCODER_API_CHARACTER_RANGE_ERROR            03110000
         LA    R5,CWARRAY            PTR TO SOURCE CODE WORD ARRAY      03120000
         LA    R6,CHARRAY            PTR TO TARGET CHARACTER ARRAY      03130000
         LA    R9,0                  SET LOOP CONTROL                   03140000
CHD010B  DS    0H -                  LOAD ENTRY POINTERS                03150000
         C     R9,=A(10)             DONE YET?                          03160000
         BNL   CHD010Z                YUP                               03170000
         SR    R14,R14               CLEAR                              03180000
         LR    R15,R9                CURRENT ARRAY COUNT                03190000
         M     R14,=A(RCL)           CURRENT ARRAY ENTRY OFFSET         03200000
         LA    R2,0(R15,R5)          CODEWORD ARRAY ENTRY ADDRESS       03210000
         LA    R3,0(R15,R6)          CHARACTER ARRAY ENTRY ADDRESS      03220000
CHD010C  DS    0H -                                                     03230000
         CLC   RTNUMBER-RECTYPE(L'RTNUMBER,R2),CUMSIZE VALID VALUE?     03240000
         BNL   I2XERR                                   NO - QUIT       03250000
         CLC   RTNUMBER-RECTYPE(L'RTNUMBER,R2),=A(TABLE_5_OF_13_SIZE)   03260000
         BL    CHD010E                                                  03270000
CHD010D  DS    0H -                                                     03280000
         MVC   RTBASE-RECTYPE(L'RTBASE,R3),=A(8192)                     03290000
         L     R1,RTNUMBER-RECTYPE(,R2)                                 03300000
         S     R1,=A(TABLE_5_OF_13_SIZE)                                03310000
         SLL   R1,2                                                     03320000
         LA    R14,TBL2OF13(R1)                                         03330000
         MVC   RTNUMBER-RECTYPE(L'RTNUMBER,R3),0(R14)                   03340000
         B     CHD010Y               PROCEED                            03350000
CHD010E  DS    0H -                                                     03360000
         MVC   RTBASE-RECTYPE(L'RTBASE,R3),=A(8192)                     03370000
         L     R1,RTNUMBER-RECTYPE(,R2)                                 03380000
         SLL   R1,2                                                     03390000
         LA    R14,TBL5OF13(R1)                                         03400000
         MVC   RTNUMBER-RECTYPE(L'RTNUMBER,R3),0(R14)                   03410000
*        B     CHD010Y               PROCEED                            03420000
CHD010Y  LA    R9,1(,R9)             INCR INDEX                         03430000
         B     CHD010B               LOOP TILL DONE                     03440000
CHD010Z  DS    0H -                                                     03450000
         LA    R0,410                EVENT ID                           03460000
         BAS   R14,M2LOG             LOG THE EVENT                      03470000
         EJECT                                                          03480000
*---------------------------------------------------------------------* 03490000
*                                                                     * 03500000
*    INSERT THE FCS INTO THE DATA BY THE FOLLOWING PROCESS:           * 03510000
*      FOR EACH CHARACTER GET THE CORRESPONDING BIT OF THE FCS        * 03520000
*        NOTE THAT CHARACTER 0 IS THE LEFTMOST CHARACTER WHILE ITS    * 03530000
*        CORRESPONDING FCS BIT (0) IS THE RIGHTMOST IN THE FCS        * 03540000
*        IF THE BIT VALUE IS:                                         * 03550000
*          0 - THEN LEAVE THE CHARACTER AS 5 OF 13                    * 03560000
*          1 - REVERSE ALL BITS VALUES IN THE CHARACTER WHICH         * 03570000
*              MAKES IT 8 OF 13                                       * 03580000
*---------------------------------------------------------------------* 03590000
         USING RECTYPE,R2           MAP NUMBER RECORD ARRAY ENTRY       03600000
FCS010   DS    0H -                                                     03610000
         MVC   ISHIFT(MSLLL),MSLL    STOW LEFT SHIFT INSTRUCTION        03620000
         LA    R6,CHARRAY            PTR TO CHARACTER ARRAY             03630000
         LA    R9,0                  SET LOOP CONTROL                   03640000
FCS010B  DS    0H -                  LOAD ENTRY POINTERS                03650000
         C     R9,=A(10)             DONE YET?                          03660000
         BNL   FCS010Z                YUP                               03670000
         SR    R14,R14               CLEAR                              03680000
         LR    R15,R9                CURRENT ARRAY COUNT                03690000
         M     R14,=A(RCL)           CURRENT ARRAY ENTRY OFFSET         03700000
         LA    R2,0(R15,R6)          NUMBER ARRAY ENTRY ADDRESS         03710000
         LA    R15,1                 BIT TO BE SHIFTED                  03720000
         STCM  R9,B'0011',ISHIFT+2   STOW IN SLL INSTRUCTION            03730000
         EX    R0,ISHIFT             SHIFT BY CURRENT INDEX VALUE       03740000
         L     R0,LFCS               GET FCS                            03750000
         NR    R0,R15                ISOLATE CORRECT BIT                03760000
         N     R0,=A(X'0000FFFF')    ISOLATE TO HALFWORD                03770000
         LTR   R0,R0                 ANY BITS ON?                       03780000
         BZ    FCS010Y                NOPE                              03790000
         L     R0,RTNUMBER           LOAD NUMERIC VALUE                 03800000
         LCR   R0,R0                 TOGGLE ALL B'1' TO B'0'...         03810000
         BCTR  R0,R0                 ... AND VICE-VERSA                 03820000
         N     R0,=A(X'00001FFF')    KEEP ONLY LO-ORDER 13 BITS         03830000
         ST    R0,RTNUMBER           STOW REVISED NUMBER                03840000
FCS010Y  LA    R9,1(,R9)             INCR INDEX                         03850000
         B     FCS010B               LOOP TILL DONE                     03860000
FCS010Z  DS    0H -                                                     03870000
         LA    R0,411                EVENT ID                           03880000
         BAS   R14,M2LOG             LOG THE EVENT                      03890000
         DROP  R2                                                       03900000
         EJECT                                                          03910000
*---------------------------------------------------------------------* 03920000
* MAP 13-BIT CHARACTERS TO THEIR CORRESPONDING POSITIONS              * 03930000
* WITHIN THE BARCODE                                                  * 03940000
*---------------------------------------------------------------------* 03950000
BCD010   DS    0H -                                                     03960000
         LA    R2,BTARRAY                       (BT)                    03970000
         LA    R3,BARTOPCHARACTERINDEXARRAY     (BTCI)                  03980000
         LA    R4,BARTOPCHARACTERSHIFTARRAY     (BTCS)                  03990000
         LA    R5,BBARRAY                       (BB)                    04000000
         LA    R6,BARBOTTOMCHARACTERINDEXARRAY  (BBCI)                  04010000
         LA    R7,BARBOTTOMCHARACTERSHIFTARRAY  (BBCS)                  04020000
         LA    R8,CHARRAY                       (CH)                    04030000
         STM   R2,R7,AXPTRS          SAVE POINTERS                      04040000
         MVC   ISHIFT(MSRLL),MSRL    STOW RIGHT SHIFT INSTRUCTION       04050000
         LA    R9,0                  SET LOOP CONTROL                   04060000
BCD010B  DS    0H -                  LOAD ENTRY POINTERS                04070000
         C     R9,=A(65)             DONE YET?                          04080000
         BNL   BCD010Z                YUP                               04090000
         LR    R15,R9                LOAD INDEX                         04100000
         SLL   R15,2                 CONVERT TO INT OFFSET              04110000
         LM    R2,R7,AXPTRS          LOAD BASE POINTERS                 04120000
         AR    R2,R15                ENTRY - BAR TOP                    04130000
         AR    R3,R15                ENTRY - BAR TOP CHAR INDEX         04140000
         AR    R4,R15                ENTRY - BAR TOP CHAR SHIFT         04150000
         AR    R5,R15                ENTRY - BAR BOTTOM                 04160000
         AR    R6,R15                ENTRY - BAR BOTTOM CHAR INDEX      04170000
         AR    R7,R15                ENTRY - BAR BOTTOM CHAR SHIFT      04180000
BTSET    DS    0H -                  SET BAR TOP VALUE                  04190000
         SR    R14,R14               CLEAR                              04200000
         L     R15,0(,R3)            BAR TOP INDEX VALUE                04210000
         M     R14,=A(RCL)           CURRENT ARRAY ENTRY OFFSET         04220000
         AR    R15,R8                CHARACTER ARRAY ENTRY ADDRESS      04230000
         L     R15,RTNUMBER-RECTYPE(,R15)  NUM. VALUE TO BE SHIFTED     04240000
         L     R14,0(,R4)            BAR TOP SHIFT VALUE                04250000
         STCM  R14,B'0011',ISHIFT+2  STOW IN SRL INSTRUCTION            04260000
         EX    R0,ISHIFT             SHIFT BY THE REQUESTED VALUE       04270000
         N     R15,=A(X'00000001')   ISOLATE LO BIT 0                   04280000
         ST    R15,0(,R2)            STOW IN BAR TOP                    04290000
BBSET    DS    0H -                  SET BAR BOTTOM VALUE               04300000
         SR    R14,R14               CLEAR                              04310000
         L     R15,0(,R6)            BAR BOTTOM INDEX VALUE             04320000
         M     R14,=A(RCL)           CURRENT ARRAY ENTRY OFFSET         04330000
         AR    R15,R8                CHARACTER ARRAY ENTRY ADDRESS      04340000
         L     R15,RTNUMBER-RECTYPE(,R15)  NUM. VALUE TO BE SHIFTED     04350000
         L     R14,0(,R7)            BAR BOTTOM SHIFT VALUE             04360000
         STCM  R14,B'0011',ISHIFT+2  STOW IN SRL INSTRUCTION            04370000
         EX    R0,ISHIFT             SHIFT BY THE REQUESTED VALUE       04380000
         N     R15,=A(X'00000001')   ISOLATE LO BIT 0                   04390000
         ST    R15,0(,R5)            STOW IN BAR BOTTOM                 04400000
BCD010Y  LA    R9,1(,R9)             INCR INDEX                         04410000
         B     BCD010B               LOOP TILL DONE                     04420000
BCD010Z  DS    0H -                                                     04430000
         EJECT                                                          04440000
*---------------------------------------------------------------------* 04450000
* CONVERT THE BARCODE TO A STRING OF CHARACTERS REPRESENTING          * 04460000
* THE 4-STATE BARCODE                                                 * 04470000
*---------------------------------------------------------------------* 04480000
CBC010   DS    0H -                                                     04490000
         LA    R2,BTARRAY            BAR TOP ARRAY                      04500000
         LA    R3,BBARRAY            BAR BOTTOM ARRAY                   04510000
         L     R8,REPLYP             REPLY STRING ADDRESS               04520000
         MVC   0(65,R8),I2SPACES     CLEAR THE REPLY AREA               04530000
         STM   R2,R3,AXPTRS          SAVE POINTERS                      04540000
         LA    R9,0                  SET LOOP CONTROL                   04550000
CBC010B  DS    0H -                  LOAD ENTRY POINTERS                04560000
         C     R9,=A(65)             DONE YET?                          04570000
         BNL   CBC010Z                YUP                               04580000
         LR    R15,R9                LOAD INDEX                         04590000
         SLL   R15,2                 CONVERT TO INT OFFSET              04600000
         LM    R2,R3,AXPTRS          LOAD BASE POINTERS                 04610000
         AR    R2,R15                ENTRY - BAR TOP                    04620000
         AR    R3,R15                ENTRY - BAR BOTTOM                 04630000
         LA    R4,0(R9,R8)           PTR TO CORRECT REPLY BYTE          04640000
CBC010C  DS    0H -                  CHECK TOP AND BOTTOM VALUES        04650000
         OC    0(4,R2),0(R2)         BAR TOP CHAR == 0?                 04660000
         BNZ   CBN001                 NO                                04670000
CBZ001   DS    0H -                  BAR TOP CHAR = 0                   04680000
         MVI   0(R4),C'T'            ASSUME BAR BOTTOM = 0              04690000
         OC    0(4,R3),0(R3)         IS THIS TRUE?                      04700000
         BZ    *+8                    YES                               04710000
         MVI   0(R4),C'D'            BAR BOTTOM = 1                     04720000
         B     CBC010Y               PROCEED                            04730000
CBN001   DS    0H -                  BAR TOP CHAR = 1                   04740000
         MVI   0(R4),C'A'            ASSUME BAR BOTTOM = 0              04750000
         OC    0(4,R3),0(R3)         IS THIS TRUE?                      04760000
         BZ    *+8                    YES                               04770000
         MVI   0(R4),C'F'            BAR BOTTOM = 1                     04780000
*        B     CBC010Y               PROCEED                            04790000
CBC010Y  LA    R9,1(,R9)             INCR INDEX                         04800000
         B     CBC010B               LOOP TILL DONE                     04810000
CBC010Z  DS    0H -                                                     04820000
         LA    R0,412                EVENT ID                           04830000
         BAS   R14,M2LOG             LOG THE EVENT                      04840000
         SPACE 2                                                        04850000
*---------------------------------------------------------------------* 04860000
* WRAP IT UP AND RETURN TO THE CALLER                                 * 04870000
*---------------------------------------------------------------------* 04880000
         B     I2XOK                 RETURN TO CALLER                   04890000
         EJECT                                                          04900000
*---------------------------------------------------------------------* 04910000
* SUBROUTINES                                                         * 04920000
*---------------------------------------------------------------------* 04930000
         SPACE 2                                                        04940000
MBSHORT  DS    0H -                  MULTIPLY BYTES BY SHORT            04950000
*  INPUTS (AT LABEL GPARMS)                                             04960000
*    OFFSET    CONTENTS                                                 04970000
*       0      PTR TO BYTE ARRAY                                        04980000
*       4      NUMBER OF BYTES                                          04990000
*       8      MULTIPLICAND                                             05000000
* OUTPUTS (R15)                                                         05010000
*    OFFSET    CONTENTS                                                 05020000
*       0      SUCCESS                                                  05030000
*       4      FAILURE                                                  05040000
*                                                                       05050000
         STM   R14,R9,GRET           SAVE RETURN REGISTERS              05060000
         MVC   GCODE,=A(4)           ASSUME ERROR                       05070000
MBS001   DS    0H -                  PROCESS CHARACTERS                 05080000
         L     R8,GPARMS+0           PTR TO BYTE ARRAY                  05090000
         LA    R8,0(,R8)             CLEAR HI-ORDER BIT(S)              05100000
         LTR   R8,R8                 VALID ADDRESS?                     05110000
         BNP   MBS999                 NO  - EXIT                        05120000
         L     R7,GPARMS+4           # BYTES TO PROCESS                 05130000
         LA    R7,0(,R7)             CLEAR HI-ORDER BIT(S)              05140000
         C     R7,=A(1)              VALID COUNT?                       05150000
         BL    MBS999                 NO  - EXIT                        05160000
         SR    R6,R6                 CLEAR                              05170000
         D     R6,=A(2)              GET REMAINDER + #PAIRS             05180000
         STM   R6,R7,GMODULO         SAVE RESULTS                       05190000
         L     R6,GPARMS+8           LOAD MULTIPLICAND                  05200000
         XC    GCARRY,GCARRY         CLEAR CARRY VALUE                  05210000
         EJECT                                                          05220000
MBS010   DS    0H -                  PROCESS BYTE PAIR                  05230000
         OC    GPAIRS,GPAIRS         ANY PAIRS?                         05240000
         BZ    MBS020                 NOPE                              05250000
         L     R9,GPAIRS             LOAD PAIR COUNT                    05260000
MBS011   DS    0H -                  PROCESS BYTE PAIR                  05270000
         LR    R15,R9                CURRENT...                         05280000
         BCTR  R15,R0                ... PAIR INDEX                     05290000
         SLL   R15,1                 CONVERT TO BYTE INDEX              05300000
         A     R15,GMODULO           ADD MODULO                         05310000
         LA    R5,0(R15,R8)          POINT TO CURRENT PAIR              05320000
         SR    R1,R1                 CLEAR                              05330000
         ICM   R1,B'0011',0(R5)      GRAB THE PAIR                      05340000
         SR    R0,R0                 PREPARE TO MULTIPLY                05350000
         MR    R0,R6                 MULTIPLY                           05360000
         A     R1,GCARRY             ADD CURRENT CARRY VALUE            05370000
         STCM  R1,B'0011',0(R5)      STOW 2 LO BYTES INTO THE DATA      05380000
         STCM  R1,B'1100',GCARRY+2   STOW 2 HI-BYTES INTO THE CARRY     05390000
MBS011Y  BCT   R9,MBS011             LOOP TILL DONE                     05400000
         SPACE 2                                                        05410000
MBS020   DS    0H -                  PROCESS REMAINING CHARACTER        05420000
         OC    GMODULO,GMODULO       REMAINING BYTE?                    05430000
         BZ    MBS998                 NOPE                              05440000
MBS021   DS    0H -                  PROCESS REMAINING CHARACTER        05450000
         SR    R0,R0                 CLEAR                              05460000
         SR    R1,R1                 CLEAR                              05470000
         IC    R1,0(,R8)             FIRST ARRAY BYTE                   05480000
         MR    R0,R6                 MULTIPLY                           05490000
         A     R1,GCARRY             ADD CURRENT CARRY VALUE            05500000
         STCM  R1,B'0001',0(R8)      STOW FIRST ARRAY BYTE              05510000
MBS998   XC    GCODE,GCODE           INDICATE SUCCESS                   05520000
MBS999   LM    R14,R9,GRET           LOAD RETURN REGISTERS              05530000
         L     R15,GCODE             LOAD RETURN CODE                   05540000
         BR    R14                   RETURN TO CALLER                   05550000
         EJECT                                                          05560000
DBSHORT  DS    0H -                  DIVIDE BYTES BY SHORT              05570000
*  INPUTS (AT LABEL GPARMS)                                             05580000
*    OFFSET    CONTENTS                                                 05590000
*       0      PTR TO BYTE ARRAY                                        05600000
*       4      NUMBER OF BYTES                                          05610000
*       8      DIVISOR                                                  05620000
*      12      PTR TO REMAINDER INT                                     05630000
* OUTPUTS (R15)                                                         05640000
*    OFFSET    CONTENTS                                                 05650000
*       0      SUCCESS                                                  05660000
*       4      FAILURE                                                  05670000
*                                                                       05680000
         STM   R14,R9,GRET           SAVE RETURN REGISTERS              05690000
         MVC   GCODE,=A(4)           ASSUME ERROR                       05700000
DBS001   DS    0H -                  PROCESS CHARACTERS                 05710000
         L     R8,GPARMS+0           PTR TO BYTE ARRAY                  05720000
         LA    R8,0(,R8)             CLEAR HI-ORDER BIT(S)              05730000
         LTR   R8,R8                 VALID ADDRESS?                     05740000
         BNP   DBS999                 NO  - EXIT                        05750000
         L     R7,GPARMS+4           # BYTES TO PROCESS                 05760000
         LA    R7,0(,R7)             CLEAR HI-ORDER BIT(S)              05770000
         C     R7,=A(2)              VALID COUNT?                       05780000
         BL    DBS999                 NO  - EXIT                        05790000
         SR    R6,R6                 CLEAR                              05800000
         D     R6,=A(2)              GET REMAINDER + #PAIRS             05810000
         STM   R6,R7,GMODULO         SAVE RESULTS                       05820000
         L     R6,GPARMS+8           LOAD DIVISOR                       05830000
         LA    R6,0(,R6)             CLEAR HI-ORDER BIT(S)              05840000
         C     R6,=A(0)              VALID VALUE?                       05850000
         BNH   DBS999                 NO  - EXIT                        05860000
         L     R2,GPARMS+12          PTR TO REMAINDER                   05870000
         LA    R2,0(,R2)             CLEAR HI-ORDER BIT(S)              05880000
         LTR   R2,R2                 VALID ADDRESS?                     05890000
         BNP   DBS999                 NO  - EXIT                        05900000
         XC    GCARRY,GCARRY         CLEAR CARRY VALUE                  05910000
         EJECT                                                          05920000
DBS010   DS    0H -                  PROCESS ODD BYTE                   05930000
         OC    GMODULO,GMODULO       ODD BYTE COUNT?                    05940000
         BZ    DBS020                 NOPE                              05950000
DBS011   DS    0H -                  PROCESS REMAINING CHARACTER        05960000
         SR    R0,R0                 CLEAR                              05970000
         SR    R1,R1                 CLEAR                              05980000
         IC    R1,0(,R8)             BYTE AT ARRAY[0]                   05990000
         DR    R0,R6                 DIVIDE                             06000000
         STCM  R0,B'1111',GCARRY     SAVE CARRY VALUE                   06010000
         STCM  R1,B'0001',0(R8)      UPDATE BYTE AT ARRAY[0]            06020000
         SPACE 2                                                        06030000
DBS020   DS    0H -                  PROCESS BYTE PAIR                  06040000
         OC    GPAIRS,GPAIRS         ANY PAIRS?                         06050000
         BZ    DBS998                 NOPE                              06060000
         SR    R9,R9                 RESET PAIR COUNT                   06070000
DBS021   DS    0H -                  PROCESS BYTE PAIR                  06080000
         LR    R1,R9                 CURRENT PAIR COUNT                 06090000
         SLL   R1,1                  CONVERT TO BYTE INDEX              06100000
         A     R1,GMODULO            ADD MODULO                         06110000
         LA    R4,0(R1,R8)           PTR TO ARRAY[INDEX + 0]            06120000
         LA    R5,1(R1,R8)           PTR TO ARRAY[INDEX + 1]            06130000
         SR    R0,R0                 CLEAR                              06140000
         SR    R1,R1                 CLEAR                              06150000
         L     R2,GCARRY             LOAD CARRY VALUE                   06160000
         SLL   R2,16                 SHIFT LEFT 16 BITS                 06170000
         IC    R1,0(,R4)             BYTE AT ARRAY[INDEX + 0]           06180000
         SLL   R1,8                  SHIFT LEFT 8 BITS                  06190000
         IC    R0,0(,R5)             BYTE AT ARRAY[INDEX + 1]           06200000
         OR    R1,R2                 COMBINE...                         06210000
         OR    R1,R0                 ... VALUES                         06220000
         SR    R0,R0                 PREPARE FOR DIVIDE                 06230000
         DR    R0,R6                 DIVIDE                             06240000
         STCM  R0,B'1111',GCARRY     SAVE CARRY VALUE                   06250000
         STCM  R1,B'0010',0(R4)      UPDATE BYTE AT ARRAY[INDEX + 0]    06260000
         STCM  R1,B'0001',0(R5)      UPDATE BYTE AT ARRAY[INDEX + 1]    06270000
DBS021Y  LA    R9,1(,R9)             INCR PAIR COUNT                    06280000
         C     R9,GPAIRS             PAIR COUNT EXCEEDED?               06290000
         BL    DBS021                  NO  - LOOP TILL DONE             06300000
DBS998   L     R2,GPARMS+12          PTR TO REMAINDER                   06310000
         LA    R2,0(,R2)             CLEAR HI-ORDER BIT(S)              06320000
         MVC   0(4,R2),GCARRY        STOW REMAINDER                     06330000
         XC    GCODE,GCODE           INDICATE SUCCESS                   06340000
DBS999   LM    R14,R9,GRET           LOAD RETURN REGISTERS              06350000
         L     R15,GCODE             LOAD RETURN CODE                   06360000
         BR    R14                   RETURN TO CALLER                   06370000
         EJECT                                                          06380000
ASBYTES  DS    0H -                  ADD SHORT TO BYTES                 06390000
*  INPUTS (AT LABEL GPARMS)                                             06400000
*    OFFSET    CONTENTS                                                 06410000
*       0      PTR TO BYTE ARRAY                                        06420000
*       4      NUMBER OF BYTES                                          06430000
*       8      ADDEND                                                   06440000
* OUTPUTS (R15)                                                         06450000
*    OFFSET    CONTENTS                                                 06460000
*       0      SUCCESS                                                  06470000
*       4      FAILURE                                                  06480000
*                                                                       06490000
         STM   R14,R9,GRET           SAVE RETURN REGISTERS              06500000
         MVC   GCODE,=A(4)           ASSUME ERROR                       06510000
ASB001   DS    0H -                  PROCESS CHARACTERS                 06520000
         L     R8,GPARMS+0           PTR TO BYTE ARRAY                  06530000
         LA    R8,0(,R8)             CLEAR HI-ORDER BIT(S)              06540000
         LTR   R8,R8                 VALID ADDRESS?                     06550000
         BNP   ASB999                 NO  - EXIT                        06560000
         L     R7,GPARMS+4           # BYTES TO PROCESS                 06570000
         LA    R7,0(,R7)             CLEAR HI-ORDER BIT(S)              06580000
         C     R7,=A(2)              VALID COUNT?                       06590000
         BL    ASB999                 NO  - EXIT                        06600000
         S     R7,=A(2)              ADJUST                             06610000
         ST    R7,GPAIRS             SAVE ADJUSTED COUNT                06620000
         L     R6,GPARMS+8           ADDEND                             06630000
         EJECT                                                          06640000
ASB010   DS    0H -                  GENERATE CARRY VALUE               06650000
         L     R1,GPAIRS             LOAD ADJUSTED COUNT                06660000
         LA    R5,1(R1,R8)           PTR TO ARRAY[COUNT - 1]            06670000
         LA    R4,0(R1,R8)           PTR TO ARRAY[COUNT - 2]            06680000
         SR    R1,R1                 CLEAR                              06690000
         ICM   R1,B'0001',0(R5)      BYTE AT ARRAY[COUNT - 1]           06700000
         ICM   R1,B'0010',0(R4)      BYTE AT ARRAY[COUNT - 2]           06710000
         AR    R1,R6                 ADD                                06720000
         STCM  R1,B'0001',0(R5)      BYTE AT ARRAY[COUNT - 1]           06730000
         STCM  R1,B'0010',0(R4)      BYTE AT ARRAY[COUNT - 2]           06740000
         XC    GCARRY,GCARRY         CLEAR CARRY BIT                    06750000
         C     R1,=A(X'0000FFFF')    GREATER THAN LARGEST HALFWORD?     06760000
         BNH   *+8                    NO                                06770000
         MVI   GCARRY+3,X'01'        SINGLE-BIT CARRY                   06780000
         SPACE 2                                                        06790000
ASB020   DS    0H -                  PROCESS BYTE PAIR                  06800000
         OC    GPAIRS,GPAIRS         ANY BYTES LEFT?                    06810000
         BZ    ASB998                 NOPE                              06820000
         LA    R8,2(,R8)             BUMP PAST FIRST 2 BYTES            06830000
         L     R9,GPAIRS             SET BYTE COUNT                     06840000
ASB021   DS    0H -                  PROCESS BYTE PAIR                  06850000
         OC    GCARRY,GCARRY         CARRY BIT ON?                      06860000
         BZ    ASB998                 NO  - QUIT                        06870000
         LR    R1,R9                 CURRENT BYTE COUNT                 06880000
         BCTR  R1,R0                 CONVERT TO BYTE INDEX              06890000
         LA    R5,0(R1,R8)           PTR TO ARRAY[INDEX - 0]            06900000
         SR    R0,R0                 CLEAR                              06910000
         IC    R0,0(,R5)             BYTE AT ARRAY[INDEX - 0]           06920000
         L     R1,GCARRY             LOAD CARRY VALUE                   06930000
         AR    R1,R0                 ADD BYTE VALUE                     06940000
         STCM  R1,B'0001',0(R5)      UPDATE BYTE AT ARRAY[INDEX - 0]    06950000
         XC    GCARRY,GCARRY         CLEAR CARRY BIT                    06960000
         C     R1,=A(X'000000FF')    GREATER THAN LARGEST BYTE?         06970000
         BNH   *+8                    NO                                06980000
         MVI   GCARRY+3,X'01'        SINGLE-BIT CARRY                   06990000
ASB021Y  BCT   R9,ASB021             LOOP TILL DONE                     07000000
ASB998   XC    GCODE,GCODE           INDICATE SUCCESS                   07010000
ASB999   LM    R14,R9,GRET           LOAD RETURN REGISTERS              07020000
         L     R15,GCODE             LOAD RETURN CODE                   07030000
         BR    R14                   RETURN TO CALLER                   07040000
         EJECT                                                          07050000
CNVB2MB  DS    0H -                  CONVERT FROM BYTES TO MULTIBASE    07060000
*  INPUTS (AT LABEL FPARMS)                                             07070000
*    OFFSET    CONTENTS                                                 07080000
*       0      PTR TO BYTE ARRAY                                        07090000
*       4      NUMBER OF BYTES                                          07100000
*       8      PTR TO NUMBER RECORD ARRAY                               07110000
*      12      NUMBER OF NUMBERS IN THE ARRAY                           07120000
* OUTPUTS (R15)                                                         07130000
*    OFFSET    CONTENTS                                                 07140000
*       0      SUCCESS                                                  07150000
*       4      FAILURE                                                  07160000
*                                                                       07170000
         USING RECTYPE,R2            MAP NUMBER RECORD ARRAY            07180000
         STM   R14,R9,FRET           SAVE RETURN REGISTERS              07190000
         MVC   FCODE,=A(4)           ASSUME ERROR                       07200000
BMB001   DS    0H -                  PROCESS CHARACTERS                 07210000
         L     R8,FPARMS+0           PTR TO BYTE ARRAY                  07220000
         LA    R8,0(,R8)             CLEAR HI-ORDER BIT(S)              07230000
         LTR   R8,R8                 VALID ADDRESS?                     07240000
         BNP   BMB999                 NO  - EXIT                        07250000
         L     R7,FPARMS+4           # BYTES TO PROCESS                 07260000
         LA    R7,0(,R7)             CLEAR HI-ORDER BIT(S)              07270000
         C     R7,=A(2)              VALID COUNT?                       07280000
         BL    BMB999                 NO  - EXIT                        07290000
         L     R6,FPARMS+8           PTR TO NUMBER RECORD ARRAY         07300000
         LA    R6,0(,R6)             CLEAR HI-ORDER BIT(S)              07310000
         LTR   R6,R6                 VALID ADDRESS?                     07320000
         BNP   BMB999                 NO  - EXIT                        07330000
         L     R7,FPARMS+12          # ARRAY ENTRIES                    07340000
         LA    R7,0(,R7)             CLEAR HI-ORDER BIT(S)              07350000
         C     R7,=A(1)              VALID COUNT?                       07360000
         BL    BMB999                 NO  - EXIT                        07370000
         LR    R9,R7                 LOOP CONTROL                       07380000
         EJECT                                                          07390000
BMB010   DS    0H -                  PERFORM CONVERSION                 07400000
         SR    R14,R14               CLEAR                              07410000
         LR    R15,R9                CURRENT ARRAY COUNT                07420000
         BCTR  R15,R0                CONVERT TO ARRAY INDEX             07430000
         M     R14,=A(RCL)           NUMBER ARRAY ENTRY OFFSET          07440000
         LA    R2,0(R15,R6)          NUMBER ARRAY ENTRY ADDRESS         07450000
         L     R14,FPARMS+0          PTR TO BYTE ARRAY                  07460000
         L     R15,FPARMS+4          NUMBER OF BYTES                    07470000
         L     R0,RTBASE             NUMBER ARRAY ENTRY BASE (DIVISOR)  07480000
         LA    R1,FREM               PTR TO REMAINDER                   07490000
         STM   R14,R1,GPARMS         STOW PARAMETER LIST                07500000
         BAS   R14,DBSHORT           DIVIDE BYTES BY SHORT              07510000
         LTR   R15,R15               ERROR?                             07520000
         BNZ   BMB999                 YES - QUIT                        07530000
         MVC   RTNUMBER,FREM         STOW REMAINDER                     07540000
BMB010Y  BCT   R9,BMB010             LOOP TILL DONE                     07550000
BMB998   XC    FCODE,FCODE           INDICATE SUCCESS                   07560000
BMB999   LM    R14,R9,FRET           LOAD RETURN REGISTERS              07570000
         L     R15,FCODE             LOAD RETURN CODE                   07580000
         BR    R14                   RETURN TO CALLER                   07590000
         DROP  R2                                                       07600000
         EJECT                                                          07610000
INITN13T DS    0H -                  INITIALIZE N OF 13 TABLE           07620000
*  INPUTS (AT LABEL FPARMS)                                             07630000
*    OFFSET    CONTENTS                                                 07640000
*       0      PTR TO N OF 13 TABLE                                     07650000
*       4      N VALUE                                                  07660000
*       8      TABLE LENGTH                                             07670000
* OUTPUTS (R15)                                                         07680000
*    OFFSET    CONTENTS                                                 07690000
*       0      SUCCESS                                                  07700000
*       4      FAILURE                                                  07710000
*                                                                       07720000
         STM   R14,R9,FRET           SAVE RETURN REGISTERS              07730000
*---------------------------------------------------------------------* 07740000
* . COUNT UP TO 2 ** 13 AND FIND ALL THOSE VALUES THAT HAVE N BITS ON * 07750000
*---------------------------------------------------------------------* 07760000
ITN000   DS    0H -                                                     07770000
         SR    R0,R0                 LOWER INDEX                        07780000
         L     R1,FPARMS+8           UPPER INDEX + 1                    07790000
         BCTR  R1,R0                 UPPER INDEX                        07800000
         STM   R0,R1,LUTINDEX        SAVE INDICES                       07810000
         SR    R5,R5                 CLEAR COUNT                        07820000
         BCTR  R5,R0                 ADJUST FOR LOOP                    07830000
ITN001   DS    0H -                  LOOP 0                             07840000
         LA    R5,1(,R5)             NEXT COUNT VALUE                   07850000
         C     R5,=A(8192)           MAX EXCEEDED?                      07860000
         BNL   ITN010                  YES - STOP                       07870000
         SR    R6,R6                 RESET BIT COUNT                    07880000
         #BIDX 00                    ADD IF LO-ORDER BIT  0 ON          07890000
         #BIDX 01                    ADD IF LO-ORDER BIT  1 ON          07900000
         #BIDX 02                    ADD IF LO-ORDER BIT  2 ON          07910000
         #BIDX 03                    ADD IF LO-ORDER BIT  3 ON          07920000
         #BIDX 04                    ADD IF LO-ORDER BIT  4 ON          07930000
         #BIDX 05                    ADD IF LO-ORDER BIT  5 ON          07940000
         #BIDX 06                    ADD IF LO-ORDER BIT  6 ON          07950000
         #BIDX 07                    ADD IF LO-ORDER BIT  7 ON          07960000
         #BIDX 08                    ADD IF LO-ORDER BIT  8 ON          07970000
         #BIDX 09                    ADD IF LO-ORDER BIT  9 ON          07980000
         #BIDX 10                    ADD IF LO-ORDER BIT 10 ON          07990000
         #BIDX 11                     DD IF LO-ORDER BIT 11 ON          08000000
         #BIDX 12                    ADD IF LO-ORDER BIT 12 ON          08010000
         C     R6,FPARMS+4           MATCHING NUMBER OF BITS?           08020000
         BNE   ITN001                  NO  - CONTINUE                   08030000
         EJECT                                                          08040000
*---------------------------------------------------------------------* 08050000
* . REVERSE THE BITS IN THIS SHORT                                    * 08060000
*   (BIT 15 BECOMES BIT 0, 1 BECOMES 14, ETC)                         * 08070000
* . IF REVERSE < COUNT, THEN WE HAVE ALREADY VISITED THIS PAIR BEFORE * 08080000
*---------------------------------------------------------------------* 08090000
ITN002   DS    0H -                  REVERSE THE BITS                   08100000
         SR    R6,R6                 RESET REVERSE VALUE                08110000
         #BRVS 00                    REVERSE LO-ORDER BIT  0            08120000
         #BRVS 01                    REVERSE LO-ORDER BIT  1            08130000
         #BRVS 02                    REVERSE LO-ORDER BIT  2            08140000
         #BRVS 03                    REVERSE LO-ORDER BIT  3            08150000
         #BRVS 04                    REVERSE LO-ORDER BIT  4            08160000
         #BRVS 05                    REVERSE LO-ORDER BIT  5            08170000
         #BRVS 06                    REVERSE LO-ORDER BIT  6            08180000
         #BRVS 07                    REVERSE LO-ORDER BIT  7            08190000
         #BRVS 08                    REVERSE LO-ORDER BIT  8            08200000
         #BRVS 09                    REVERSE LO-ORDER BIT  9            08210000
         #BRVS 10                    REVERSE LO-ORDER BIT 10            08220000
         #BRVS 11                    REVERSE LO-ORDER BIT 11            08230000
         #BRVS 12                    REVERSE LO-ORDER BIT 12            08240000
         #BRVS 13                    REVERSE LO-ORDER BIT 13            08250000
         #BRVS 14                    REVERSE LO-ORDER BIT 14            08260000
         #BRVS 15                    REVERSE LO-ORDER BIT 15            08270000
         SRL   R6,3                  DIVIDE BY 8                        08280000
ITN02TR  DS    0H -                                                     08290000
         CLI   IF1STG,X'00'          SHOW_TABLE_GENERATION SET?         08300000
         BE    ITN02TZ                 NO  - BYPASS TRACE               08310000
         STM   R5,R6,LCOUNT          STOW CURRENT VALUES                08320000
         LA    R0,501                TRACE REQUEST CODE                 08330000
         BAS   R14,M2LOG             ISSUE TRACE MSG                    08340000
ITN02TZ  DS    0H -                                                     08350000
         EJECT                                                          08360000
ITN003   DS    0H -                  REVERSE THE BITS                   08370000
*---------------------------------------------------------------------* 08380000
* . SET TABLE VALUES BASED ON SYMMETRIC STATUS                        * 08390000
*   (IF REVERSE == COUNT, SYMMETRIC; OTHERWISE ASYMMETRIC)            * 08400000
*---------------------------------------------------------------------* 08410000
         CR    R6,R5                 COMPARE REVERSE VS COUNT           08420000
         BL    ITN001                REVERSE < COUNT (BYPASS)           08430000
         BH    ITN03NS               REVERSE > COUNT (ASYMMETRIC)       08440000
         SPACE 2                                                        08450000
ITN03SY  DS    0H -                  SYMMETRIC PAIR                     08460000
         L     R4,FPARMS+0           TABLE PTR                          08470000
         L     R3,LUTINDEX+4         UPPER INDEX VALUE                  08480000
         LR    R1,R3                 XFER                               08490000
         SLL   R1,2                  CALC OFFSET                        08500000
         ST    R5,0(R1,R4)           STOW COUNT                         08510000
         BCTR  R3,R0                 DECR UPPER INDEX                   08520000
         ST    R3,LUTINDEX+4         SAVE FOR NEXT PASS                 08530000
         B     ITN001                CONTINUE                           08540000
ITN03NS  DS    0H -                  ASYMMETRIC PAIR                    08550000
         L     R4,FPARMS+0           TABLE PTR                          08560000
         L     R3,LUTINDEX+0         LOWER INDEX VALUE                  08570000
         LR    R1,R3                 XFER                               08580000
         SLL   R1,2                  CALC OFFSET                        08590000
         ST    R5,0(R1,R4)           STOW COUNT                         08600000
         LA    R3,1(,R3)             INCR LOWER INDEX                   08610000
         LR    R1,R3                 XFER                               08620000
         SLL   R1,2                  CALC OFFSET                        08630000
         ST    R6,0(R1,R4)           STOW REVERSE                       08640000
         LA    R3,1(,R3)             INCR LOWER INDEX                   08650000
         ST    R3,LUTINDEX+0         SAVE FOR NEXT PASS                 08660000
         B     ITN001                CONTINUE                           08670000
*---------------------------------------------------------------------* 08680000
* . END COUNT LOOP                                                    * 08690000
*---------------------------------------------------------------------* 08700000
*---------------------------------------------------------------------* 08710000
* . WE BETTER HAVE THE EXACT CORRECT NUMBER OF TABLE ENTRIES          * 08720000
*---------------------------------------------------------------------* 08730000
ITN010   DS    0H -                  ASYMMETRIC PAIR                    08740000
         LM    R0,R1,LUTINDEX        LOAD LOWER/UPPER INDICES           08750000
         LA    R1,1(,R1)             ADJUST                             08760000
         CR    R0,R1                 EXACT NUMBER OF ENTRIES?           08770000
         BNE   ITN999                 NO  - FOOBAR                      08780000
ITN020   DS    0H -                                                     08790000
         CLI   IF1STG,X'00'          SHOW_TABLE_GENERATION SET?         08800000
         BE    ITN029                  NO  - BYPASS TRACE               08810000
         SR    R5,R5                 CLEAR COUNT                        08820000
         BCTR  R5,R0                 ADJUST FOR LOOP                    08830000
ITN021   DS    0H -                  LOOP 0                             08840000
         LA    R5,1(,R5)             NEXT COUNT VALUE                   08850000
         C     R5,FPARMS+8           TABLE LENGTH EXCEEDED?             08860000
         BNL   ITN029                  YES - STOP                       08870000
         L     R6,FPARMS+0           TABLE PTR                          08880000
         LR    R1,R5                 XFER INDEX                         08890000
         SLL   R1,2                  CALC OFFSET                        08900000
         L     R6,0(R1,R6)           LOAD VALUE                         08910000
         STM   R5,R6,LCOUNT          STOW VALUES                        08920000
         LA    R0,502                TRACE REQUEST CODE                 08930000
         BAS   R14,M2LOG             ISSUE TRACE MSG                    08940000
         B     ITN021                LOOP TILL DONE                     08950000
ITN029   DS    0H -                                                     08960000
ITN998   XC    FCODE,FCODE           INDICATE SUCCESS                   08970000
ITN999   LM    R14,R9,FRET           LOAD RETURN REGISTERS              08980000
         L     R15,FCODE             LOAD RETURN CODE                   08990000
         BR    R14                   RETURN TO CALLER                   09000000
         EJECT                                                          09010000
GENCRC   DS    0H -                  GENERATE CRC11 FRAME CHECK SEQ     09020000
*  INPUTS (AT LABEL FPARMS)                                             09030000
*    OFFSET    CONTENTS                                                 09040000
*       0      PTR TO BYTE ARRAY                                        09050000
* OUTPUTS (R15)                                                         09060000
*    FRAME CHECK SEQUENCE                                               09070000
*                                                                       09080000
         STM   R14,R9,FRET           SAVE RETURN REGISTERS              09090000
*---------------------------------------------------------------------* 09100000
* . SETUP FOR CRC GENERATION                                          * 09110000
*---------------------------------------------------------------------* 09120000
GRC000   DS    0H -                                                     09130000
         L     R6,RFCS               INITIAL FRAME CHECK SEQUENCE       09140000
*---------------------------------------------------------------------* 09150000
* . DO MOST SIGNIFICANT BYTE SKIPPING MOST SIGNIFICANT BIT            * 09160000
*---------------------------------------------------------------------* 09170000
GRC010   DS    0H -                                                     09180000
         L     R5,FPARMS             BYTE ARRAY                         09190000
         LA    R5,0(,R5)             FIRST BYTE                         09200000
         SR    R4,R4                 CLEAR                              09210000
         IC    R4,0(,R5)             LOAD BYTE VALUE                    09220000
         SLL   R4,5                  MULTIPLY BY 32                     09230000
         L     R9,=A(8-2)            LOOP CONTROL (BITS 2-7)            09240000
GRC011   #BFCS                       ADJUST FCS                         09250000
         BCT   R9,GRC011             LOOP TILL DONE                     09260000
*---------------------------------------------------------------------* 09270000
* . DO THE REST OF THE BYTES                                          * 09280000
*---------------------------------------------------------------------* 09290000
GRC020   DS    0H -                                                     09300000
         L     R5,FPARMS             BYTE ARRAY                         09310000
         LA    R5,1(,R5)             SUBSEQUENT BYTES                   09320000
         L     R8,=A(13-1)           LOOP CONTROL (BYTES 1-12)          09330000
GRC021   SR    R4,R4                 CLEAR                              09340000
         IC    R4,0(,R5)             LOAD BYTE VALUE                    09350000
         SLL   R4,3                  MULTIPLY BY 8                      09360000
         L     R9,=A(8-0)            LOOP CONTROL (BITS 0-7)            09370000
GRC023   #BFCS                       ADJUST FCS                         09380000
         BCT   R9,GRC023             LOOP TILL DONE                     09390000
GRC029   LA    R5,1(,R5)             NEXT DATA BYTE                     09400000
         BCT   R8,GRC021             LOOP TILL DONE                     09410000
GRC998   ST    R6,FRET+4             RETURN FCS CODE.JCL(LINKA)'        09420000
GRC999   LM    R14,R9,FRET           LOAD RETURN REGISTERS              09430000
         BR    R14                   RETURN TO CALLER                   09440000
*---------------------------------------------------------------------* 09450000
* TRACE MSG                                                           * 09460000
*---------------------------------------------------------------------* 09470000
         SPACE 2                                                        09480000
M2LOG    DS    0H -                   PERFORM TRACE ACTION              09490000
*                                                                       09500000
* INPUTS  -                                                             09510000
*     R0  - REQUEST CODE                                                09520000
*                                                                       09530000
         STM   R14,R1,T2RET           SAVE RETURN ADDRESS               09540000
         L     R15,=V(ENCXLOG)        LOG ROUTINES                      09550000
         BASR  R14,R15                CALL LOGGER                       09560000
         LM    R14,R1,T2RET           LOAD RETURN ADDRESS               09570000
         BR    R14                    RETURN TO CALLER                  09580000
         SPACE 2                                                        09590000
*---------------------------------------------------------------------* 09600000
* ERROR ROUTINES                                                      * 09610000
*---------------------------------------------------------------------* 09620000
         SPACE 2                                                        09630000
I2XERR   DS    0H -                   ERROR DETECTED                    09640000
         ST    R7,CCODE                                                 09650000
         B     I2EXIT                                                   09660000
         SPACE 2                                                        09670000
I2XOK    DS    0H -                   EVERYTHING A-OK                   09680000
         MVC   CCODE,=A(USPS_FSB_ENCODER_API_SUCCESS)                   09690000
         B     I2EXIT                                                   09700000
         EJECT                                                          09710000
*---------------------------------------------------------------------* 09720000
* BARCODE CHARACTER CONVERSION TABLES                                 * 09730000
*---------------------------------------------------------------------* 09740000
         SPACE 2                                                        09750000
BARTOPCHARACTERINDEXARRAY DS    0A -                                    09760000
         DC    A(004),A(000),A(002),A(006),A(003)                       09770000
         DC    A(005),A(001),A(009),A(008),A(007)                       09780000
         DC    A(001),A(002),A(000),A(006),A(004)                       09790000
         DC    A(008),A(002),A(009),A(005),A(003)                       09800000
         DC    A(000),A(001),A(003),A(007),A(004)                       09810000
         DC    A(006),A(008),A(009),A(002),A(000)                       09820000
         DC    A(005),A(001),A(009),A(004),A(003)                       09830000
         DC    A(008),A(006),A(007),A(001),A(002)                       09840000
         DC    A(004),A(003),A(009),A(005),A(007)                       09850000
         DC    A(008),A(003),A(000),A(002),A(001)                       09860000
         DC    A(004),A(000),A(009),A(001),A(007)                       09870000
         DC    A(000),A(002),A(004),A(006),A(003)                       09880000
         DC    A(007),A(001),A(009),A(005),A(008)                       09890000
BARTOPCHARACTERINDEXARRAY_L EQU ((*-BARTOPCHARACTERINDEXARRAY)/4)       09900000
BARBOTTOMCHARACTERINDEXARRAY DS    0A -                                 09910000
         DC    A(007),A(001),A(009),A(005),A(008)                       09920000
         DC    A(000),A(002),A(004),A(006),A(003)                       09930000
         DC    A(005),A(008),A(009),A(007),A(003)                       09940000
         DC    A(000),A(006),A(001),A(007),A(004)                       09950000
         DC    A(006),A(008),A(009),A(002),A(005)                       09960000
         DC    A(001),A(007),A(005),A(004),A(003)                       09970000
         DC    A(008),A(007),A(006),A(000),A(002)                       09980000
         DC    A(005),A(004),A(009),A(003),A(000)                       09990000
         DC    A(001),A(006),A(008),A(002),A(000)                       10000000
         DC    A(004),A(005),A(009),A(006),A(007)                       10010000
         DC    A(005),A(002),A(006),A(003),A(008)                       10020000
         DC    A(005),A(001),A(009),A(008),A(007)                       10030000
         DC    A(004),A(000),A(002),A(006),A(003)                       10040000
BARBOTTOMCHARACTERINDEXARRAY_L EQU ((*-BARBOTTOMCHARACTERINDEXARRAY)/4) 10050000
BARTOPCHARACTERSHIFTARRAY DS    0A -                                    10060000
         DC    A(003),A(000),A(008),A(011),A(001)                       10070000
         DC    A(012),A(008),A(011),A(010),A(006)                       10080000
         DC    A(004),A(012),A(002),A(007),A(009)                       10090000
         DC    A(006),A(007),A(009),A(002),A(008)                       10100000
         DC    A(004),A(000),A(012),A(007),A(010)                       10110000
         DC    A(009),A(000),A(007),A(010),A(005)                       10120000
         DC    A(007),A(009),A(006),A(008),A(002)                       10130000
         DC    A(012),A(001),A(004),A(002),A(000)                       10140000
         DC    A(001),A(005),A(004),A(006),A(012)                       10150000
         DC    A(001),A(000),A(009),A(004),A(007)                       10160000
         DC    A(005),A(010),A(002),A(006),A(009)                       10170000
         DC    A(011),A(002),A(012),A(006),A(007)                       10180000
         DC    A(005),A(011),A(000),A(003),A(002)                       10190000
BARTOPCHARACTERSHIFTARRAY_L EQU ((*-BARTOPCHARACTERSHIFTARRAY)/4)       10200000
BARBOTTOMCHARACTERSHIFTARRAY DS    0A -                                 10210000
         DC    A(002),A(010),A(012),A(005),A(009)                       10220000
         DC    A(001),A(005),A(004),A(003),A(009)                       10230000
         DC    A(011),A(005),A(010),A(001),A(006)                       10240000
         DC    A(003),A(004),A(001),A(010),A(000)                       10250000
         DC    A(002),A(011),A(008),A(006),A(001)                       10260000
         DC    A(012),A(003),A(008),A(006),A(004)                       10270000
         DC    A(004),A(011),A(000),A(006),A(001)                       10280000
         DC    A(009),A(011),A(005),A(003),A(007)                       10290000
         DC    A(003),A(010),A(007),A(011),A(008)                       10300000
         DC    A(002),A(010),A(003),A(005),A(008)                       10310000
         DC    A(000),A(003),A(012),A(011),A(008)                       10320000
         DC    A(004),A(005),A(001),A(003),A(000)                       10330000
         DC    A(007),A(012),A(009),A(008),A(010)                       10340000
BARBOTTOMCHARACTERSHIFTARRAY_L EQU ((*-BARBOTTOMCHARACTERSHIFTARRAY)/4) 10350000
         SPACE 2                                                        10360000
*---------------------------------------------------------------------* 10370000
* CONSTANTS                                                           * 10380000
*---------------------------------------------------------------------* 10390000
         SPACE 2                                                        10400000
I2SPACES DC    256C' '                                                  10410000
*------------------------------------------------------------*          10420000
* HALFWORD BITWISE MASKS                                     *          10430000
* BIT NUMBERING IS FROM 0 (X'0001') THRU 15 (X'8000')        *          10440000
*------------------------------------------------------------*          10450000
MASK00   DC    A(1)         ISOLATE BIT  0                              10460000
MASK01   DC    A(2)         ISOLATE BIT  1                              10470000
MASK02   DC    A(4)         ISOLATE BIT  2                              10480000
MASK03   DC    A(8)         ISOLATE BIT  3                              10490000
MASK04   DC    A(16)        ISOLATE BIT  4                              10500000
MASK05   DC    A(32)        ISOLATE BIT  5                              10510000
MASK06   DC    A(64)        ISOLATE BIT  6                              10520000
MASK07   DC    A(128)       ISOLATE BIT  7                              10530000
MASK08   DC    A(256)       ISOLATE BIT  8                              10540000
MASK09   DC    A(512)       ISOLATE BIT  9                              10550000
MASK10   DC    A(1024)      ISOLATE BIT 10                              10560000
MASK11   DC    A(2048)      ISOLATE BIT 11                              10570000
MASK12   DC    A(4096)      ISOLATE BIT 12                              10580000
MASK13   DC    A(8192)      ISOLATE BIT 13                              10590000
MASK14   DC    A(16384)     ISOLATE BIT 14                              10600000
MASK15   DC    A(32768)     ISOLATE BIT 15                              10610000
REVR00   DC    A(32768)     BIT  0 BECOMES BIT 15                       10620000
REVR01   DC    A(16384)     BIT  1 BECOMES BIT 14                       10630000
REVR02   DC    A(8192)      BIT  2 BECOMES BIT 13                       10640000
REVR03   DC    A(4096)      BIT  3 BECOMES BIT 12                       10650000
REVR04   DC    A(2048)      BIT  4 BECOMES BIT 11                       10660000
REVR05   DC    A(1024)      BIT  5 BECOMES BIT 10                       10670000
REVR06   DC    A(512)       BIT  6 BECOMES BIT  9                       10680000
REVR07   DC    A(256)       BIT  7 BECOMES BIT  8                       10690000
REVR08   DC    A(128)       BIT  8 BECOMES BIT  7                       10700000
REVR09   DC    A(64)        BIT  9 BECOMES BIT  6                       10710000
REVR10   DC    A(32)        BIT 10 BECOMES BIT  5                       10720000
REVR11   DC    A(16)        BIT 11 BECOMES BIT  4                       10730000
REVR12   DC    A(8)         BIT 12 BECOMES BIT  3                       10740000
REVR13   DC    A(4)         BIT 13 BECOMES BIT  2                       10750000
REVR14   DC    A(2)         BIT 14 BECOMES BIT  1                       10760000
REVR15   DC    A(1)         BIT 15 BECOMES BIT  0                       10770000
*------------------------------------------------------------*          10780000
* CRC VALUES                                                 *          10790000
*------------------------------------------------------------*          10800000
RPOLY    DC    A(X'0F35')                                               10810000
RFCS     DC    A(X'07FF')                                               10820000
QDIVFW   DC    PL8'4294967296'       (X'100000000')                     10830000
QDIVHW   DC    PL4'65536'            (X'10000')                         10831000
MSLL     DS    0D                    ROOM FOR SLL INSTRUCTION           10840000
         SLL   R15,0                 DUMMY SHIFT INSTRUCTION            10850000
MSLLL    EQU   *-MSLL                INSTRUCTION LENGTH                 10860000
MSRL     DS    0D                    ROOM FOR SRL INSTRUCTION           10870000
         SRL   R15,0                 DUMMY SHIFT INSTRUCTION            10880000
MSRLL    EQU   *-MSRL                INSTRUCTION LENGTH                 10890000
         SPACE 2                                                        10900000
         LTORG                                                          10910000
         EJECT                                                          10920000
*---------------------------------------------------------------------* 10930000
* WORK AREAS                                                          * 10940000
*---------------------------------------------------------------------* 10950000
I2WORKA  DS    0D -                  WORK AREA                          10960000
I2SAVA   DS    18F                   REGISTER SAVE AREA                 10970000
CCODE    DS    A                     RETURN CODE                        10980000
FRET     DS    12A                   REGISTER SAVE AREA                 10990000
FPARMS   DS    4A                    METHOD PARAMETER LIST              11000000
FCODE    DS    A                     RETURN CODE                        11010000
FREM     DS    A                     REMAINDER VALUE                    11020000
GRET     DS    12A                   REGISTER SAVE AREA                 11030000
GPARMS   DS    4A                    METHOD PARAMETER LIST              11040000
GCODE    DS    A                     RETURN CODE                        11050000
GMODULO  DS    A                     DIVISION REMAINDER                 11060000
GPAIRS   DS    A                     DIVISION QUOTIENT                  11070000
GCARRY   DS    A                     ARITHMETIC CARRY VALUE             11080000
CUMSIZE  DS    A                     CUMULATIVE SIZE VALUE              11090000
LUTINDEX DS    2A                    LOWER/UPPER TABLE INDEX VALUES     11100000
AXPTRS   DS    8A                    BAR ARRAY POINTERS                 11110000
DPAKK16  DS    PL16                  PACKED DECIMAL WORK AREA           11120000
NCCH     DS    X                     SINGLE CHAR                        11130000
T2RET    DS    4A                    ROUTINE RETURN ADDRESS             11140000
ISHIFT   DS    D                     ROOM FOR SLL INSTRUCTION           11150000
BTARRAY  DS    0D,65F                BAR TOP ARRAY                      11160000
BBARRAY  DS    0D,65F                BAR BOTTOM ARRAY                   11170000
TBL2OF13 DS    (TABLE_2_OF_13_SIZE)F 2 OF 13 TABLE                      11180000
TBL5OF13 DS    (TABLE_5_OF_13_SIZE)F 5 OF_13 TABLE                      11190000
         DS    0D                    FORCE DOUBLEWORD ALIGNMENT         11200000
I2WLEN   EQU   *-I2WORKA             WORK AREA SIZE                     11210000
         DROP  R12,R11                                                  11220000
