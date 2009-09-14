*---------------------------------------------------------------------* 00010000
* MODULE: ENCODETR                                                    * 00020000
*  USAGE: 4-STATE BARCODE CONVERTER                                   * 00030000
*   DESC: THIS MODULE GENERATES VALID BARCODES FROM                   * 00040000
*         CHARACTER INPUT STRINGS.                                    * 00050000
*                                                                     * 00060000
*   REGISTERS AT ENTRY                                                * 00070000
*      REG      USAGE                                                 * 00080000
*      R0       N/A                                                   * 00090000
*      R1       POINTER TO A LIST OF 5 FULLWORD ENTRIES, AS FOLLOWS:  * 00100000
*               OFFSET   CONTENTS                                     * 00110000
*                 0      ADDRESS OF A 20-BYTE BUFFER CONTAINING       * 00120000
*                        THE TRACKING CODE DECIMAL DIGITS.            * 00130000
*                 4      ADDRESS OF A 12-BYTE BUFFER CONTAINING       * 00140000
*                        0, 5, 9, OR 11 ROUTER DIGITS.                * 00150000
*                 8      ADDRESS OF A 65-BYTE BUFFER WHICH            * 00160000
*                        WILL CONTAIN THE BARCODE RESULT.             * 00170000
*                        CHARACTERS WILL BE 'T', 'F', 'A', OR 'D',    * 00180000
*                        WHERE:                                       * 00190000
*                        T IS TRACKER (NEITHER ASCENDER NOR DESCENDER)* 00200000
*                        F IS FULL (TRACKER + ASCENDER + DESCENDER)   * 00210000
*                        A IS ASCENDER (TRACKER + ASCENDER)           * 00220000
*                        D IS DESCENDER (TRACKER + DESCENDER)         * 00230000
*                        BARS PROCEED FROM LEFT TO RIGHT, WITH THE    * 00240000
*                        LEFTMOST BAR AS THE FIRST BAR IN THE BARCODE * 00250000
*                12      IF NON-ZERO, DISPLAY                         * 00260000
*                        INTERMEDIATE RESULTS WHILE                   * 00270000
*                        GENERATING THE BARCODE;                      * 00280000
*                        IGNORED IF ZERO                              * 00290000
*                16      IF NON-ZERO, DISPLAY                         * 00300000
*                        TABLE DATA WHILE                             * 00310000
*                        GENERATING INTERNAL TABLES;                  * 00320000
*                        IGNORED IF ZERO                              * 00330000
*               THE FIRST 3 FULLWORDS ARE REQUIRED; THE LAST LIST     * 00340000
*               ENTRY SHOULD ITS HI-ORDER BIT ON TO DENOTE            * 00350000
*               THE END OF THE LIST. A MAXIMUM OF 5 LIST ENTRIES      * 00360000
*               WILL BE RECOGNIZED.                                   * 00370000
*      R2-R12   N/A                                                   * 00380000
*      R13      SAVE AREA PTR (18 FULLWORDS)                          * 00390000
*      R14      RETURN ADDRESS                                        * 00400000
*      R15      N/A                                                   * 00410000
*                                                                     * 00420000
*   REGISTERS AT RETURN                                               * 00430000
*      REG      USAGE                                                 * 00440000
*      R0-R14   SAME AS AT ENTRY.                                     * 00450000
*      R15      A RETURN CODE, AS FOLLOWS:                            * 00460000
*               RC   MEANING                                          * 00470000
*                0   BARCODE GENERATED.                               * 00480000
*                1   SELF-TEST FAILED.                                * 00490000
*                2   BAR STRING IS NULL.                              * 00500000
*                3   BYTE CONVERSION FAILED.                          * 00510000
*                4   RETRIEVE TABLE FAILED.                           * 00520000
*                5   CODEWORD CONVERSION FAILED.                      * 00530000
*                6   CHARACTER RANGE ERROR DETECTED.                  * 00540000
*                7   TRACK STRING IS NULL.                            * 00550000
*                8   ROUTE STRING IS NULL.                            * 00560000
*                9   TRACK STRING HAS BAD LENGTH.                     * 00570000
*               10   TRACK STRING HAS INVALID DATA.                   * 00580000
*               11   TRACK STRING HAS INVALID DIGIT2.                 * 00590000
*               12   ROUTE STRING HAS BAD LENGTH.                     * 00600000
*               13   ROUTE STRING HAS INVALID DATA.                   * 00610000
*              100   INVALID ARGUMENT(S) DETECTED.                    * 00620000
*                                                                     * 00630000
*   REGISTER USAGE                                                    * 00640000
*      REG      USAGE                                                 * 00650000
*      R0-R8    WORK REGISTERS                                        * 00660000
*      R9       I/O WORK AREA PTR                                     * 00670000
*      R10      GENERAL WORK AREA PTR                                 * 00680000
*      R11-R12  CSECT ADDRESSABILITY                                  * 00690000
*      R13      SAVE AREA PTR                                         * 00700000
*      R14-R15  WORK REGISTERS                                        * 00710000
*---------------------------------------------------------------------* 00720000
         EJECT                                                          00730000
ENCODETR CSECT                                                          00740000
ENCODETR AMODE 31 -                                                     00750000
ENCODETR RMODE ANY                                                      00760000
         USING ENCODETR,R12,R11      MAP THIS MODULE                    00770000
         USING GWA,R10               MAP GENERAL WORK AREA              00780000
         SPACE 2                                                        00790000
         STM   R14,R12,12(R13)       SAVE REGISTERS                     00800000
         LA    R12,0(,R15)           MAP...                             00810000
         LA    R11,4095(,R12)        ... THIS...                        00820000
         LA    R11,1(,R11)           ... CSECT                          00830000
         L     R0,=A(WLEN)           LOAD COMBINED...                   00840000
         LA    R1,WORKA              LOAD WORK AREA ADDRESS             00850000
         #CLRSTOR ADDR=(R1),LV=(R0)  CLEAR WORK AREA                    00860000
         L     R1,24(,R13)           R1 AT ENTRY                        00870000
         ST    R13,ISAVA+4           SAVEAREA PTR                       00880000
         LA    R13,ISAVA             NEW SAVEAREA                       00890000
         LA    R10,MGWA0             POINT TO PARM AREA                 00900000
         B     AMAIN                 PROCEED                            00910000
         SPACE 2                                                        00920000
EXIT     DS    0H -                  RETURN TO THE CALLER               00930000
         LA    R0,2                  CLOSE...                           00940000
         BAS   R14,MLOG              ... TRACE LOG                      00950000
         L     R13,ISAVA+4           CALLER'S SAVE AREA                 00960000
         L     R14,12(,R13)          RETURN ADDRESS                     00970000
         L     R15,XCODE             RETURN CODE                        00980000
         LM    R0,R12,20(R13)        REMAINING REGISTERS                00990000
         BR    R14                   RETURN TO CALLER                   01000000
         EJECT                                                          01010000
AMAIN    DS    0H -                                                     01020000
         SPACE 2                                                        01030000
VALID01  DS    0H -                                                     01040000
         LR    R8,R1                 PARAMETER LIST ADDRESS             01050000
         BAS   R14,GETCOUNT          GET PARAMETER LIST ENTRY COUNT     01060000
         LA    R7,USPS_FSB_ENCODER_API_TRACK_STRING_IS_NULL             01070000
         C     R15,=A(1)             REQUIRED PARAMETER MISSING?        01080000
         BL    EXITERR                YES - QUIT                        01090000
         LA    R7,USPS_FSB_ENCODER_API_ROUTE_STRING_IS_NULL             01100000
         C     R15,=A(2)             REQUIRED PARAMETER MISSING?        01110000
         BL    EXITERR                YES - QUIT                        01120000
         LA    R7,USPS_FSB_ENCODER_API_BAR_STRING_IS_NULL               01130000
         C     R15,=A(3)             REQUIRED PARAMETER MISSING?        01140000
         BL    EXITERR                YES - QUIT                        01150000
         ST    R15,NPARMS            SAVE LIST COUNT                    01160000
         SPACE 2                                                        01170000
VLD001   DS    0H -                                                     01180000
         L     R4,0(,R8)             TRACKER PTR                        01190000
         LA    R4,0(,R4)             CLEAR HI-ORDER BIT(S)              01200000
         LTR   R4,R4                 VALID ADDRESS?                     01210000
         LA    R7,USPS_FSB_ENCODER_API_TRACK_STRING_IS_NULL             01220000
         BNP   EXITERR                NO  - EXIT                        01230000
         LA    R3,20                 FIELD LENGTH                       01240000
         BAS   R14,MSTRLEN           GET STRING LENGTH                  01250000
         C     R0,=A(20)             CORRECT DIGIT COUNT?               01260000
         LA    R7,USPS_FSB_ENCODER_API_TRACK_STRING_BAD_LENGTH          01270000
         BNE   EXITERR                NO  - EXIT                        01280000
         LR    R3,R0                 CALCULATE...                       01290000
         BCTR  R3,R0                 ... EXECUTE LENGTH                 01300000
         EX    R3,CHECKNUM           ENSURE NUMERICS                    01310000
         LA    R7,USPS_FSB_ENCODER_API_TRACK_STRING_HAS_INVALID_DATA    01320000
         BNZ   EXITERR               EXIT IF NOT VALID                  01330000
         LA    R7,USPS_FSB_ENCODER_API_TRACK_STRING_HAS_INVALID_DIGIT2  01340000
         CLI   1(R4),C'0'            VALID DIGIT2?                      01350000
         BE    VLD001B                YES - PROCEED                     01360000
         CLI   1(R4),C'1'            VALID DIGIT2?                      01370000
         BE    VLD001B                YES - PROCEED                     01380000
         CLI   1(R4),C'2'            VALID DIGIT2?                      01390000
         BE    VLD001B                YES - PROCEED                     01400000
         CLI   1(R4),C'3'            VALID DIGIT2?                      01410000
         BE    VLD001B                YES - PROCEED                     01420000
         CLI   1(R4),C'4'            VALID DIGIT2?                      01430000
         BNE   EXITERR               EXIT IF NOT VALID                  01440000
VLD001B  ST    R0,TRACKL             SAVE LENGTH                        01450000
         ST    R4,TRACKP             SAVE ADDRESS                       01460000
VLD001Z  DS    0H -                                                     01470000
         EJECT                                                          01480000
VLD002   DS    0H -                                                     01490000
         L     R4,4(,R8)             ROUTER PTR                         01500000
         LA    R4,0(,R4)             CLEAR HI-ORDER BIT(S)              01510000
         LTR   R4,R4                 VALID ADDRESS?                     01520000
         BNP   VLD002Z                NO  - BYPASS                      01530000
         LA    R7,USPS_FSB_ENCODER_API_ROUTE_STRING_BAD_LENGTH          01540000
         LA    R3,12                 FIELD LENGTH                       01550000
         BAS   R14,MSTRLEN           GET STRING LENGTH                  01560000
         C     R0,=A(0)              CORRECT DIGIT COUNT?               01570000
         BE    VLD002Z                YES - BYPASS                      01580000
         C     R0,=A(5)              CORRECT DIGIT COUNT?               01590000
         BE    VLD002B                YES - PROCEED                     01600000
         C     R0,=A(9)              CORRECT DIGIT COUNT?               01610000
         BE    VLD002B                YES - PROCEED                     01620000
         C     R0,=A(11)             CORRECT DIGIT COUNT?               01630000
         BNE   EXITERR               EXIT IF NOT VALID                  01640000
VLD002B  LA    R7,USPS_FSB_ENCODER_API_ROUTE_STRING_HAS_INVALID_DATA    01650000
         LR    R3,R0                 CALCULATE...                       01660000
         BCTR  R3,R0                 ... EXECUTE LENGTH                 01670000
         EX    R3,CHECKNUM           ENSURE NUMERICS                    01680000
         BNZ   EXITERR               EXIT IF NOT VALID                  01690000
         ST    R0,ROUTEL             SAVE LENGTH                        01700000
         ST    R4,ROUTEP             SAVE ADDRESS                       01710000
VLD002Z  DS    0H -                                                     01720000
         SPACE 2                                                        01730000
VLD003   DS    0H -                                                     01740000
         L     R4,8(,R8)             REPLY AREA PTR                     01750000
         LA    R4,0(,R4)             CLEAR HI-ORDER BIT(S)              01760000
         LTR   R4,R4                 VALID ADDRESS?                     01770000
         LA    R7,USPS_FSB_ENCODER_API_BAR_STRING_IS_NULL               01780000
         BNP   EXITERR                NO  - EXIT                        01790000
         LA    R3,65                 REPLY AREA SIZE                    01800000
         STM   R3,R4,REPLYL          SAVE LENGTH/ADDRESS                01810000
         MVC   0(65,R4),SPACES       CLEAR REPLY AREA                   01820000
VLD003Z  DS    0H -                                                     01830000
         SPACE 2                                                        01840000
VLD004   DS    0H -                                                     01850000
         CLC   NPARMS,=A(4)          PARAMETER SPECIFIED?               01860000
         BL    VLD999                 NO  - WE ARE DONE                 01870000
         L     R4,12(,R8)            SHOW_INTERMEDIATES FLAG            01880000
         LA    R4,0(,R4)             CLEAR HI-ORDER BIT(S)              01890000
         LTR   R4,R4                 NON-ZERO VALUE?                    01900000
         BZ    VLD004Z                NO  - BYPASS                      01910000
         MVI   IF1SHM,C'1'           SET FLAG                           01920000
VLD004Z  DS    0H -                                                     01930000
         SPACE 2                                                        01940000
VLD005   DS    0H -                                                     01950000
         CLC   NPARMS,=A(5)          PARAMETER SPECIFIED?               01960000
         BL    VLD999                 NO  - WE ARE DONE                 01970000
         L     R4,16(,R8)            SHOW_TABLE_GENERATION FLAG         01980000
         LA    R4,0(,R4)             CLEAR HI-ORDER BIT(S)              01990000
         LTR   R4,R4                 NON-ZERO VALUE?                    02000000
         BZ    VLD005Z                NO  - BYPASS                      02010000
         MVI   IF1STG,C'1'           SET FLAG                           02020000
VLD005Z  DS    0H -                                                     02030000
VLD999   DS    0H -                                                     02040000
         LA    R0,1                  OPEN...                            02050000
         BAS   R14,MLOG              ... TRACE LOG                      02060000
         EJECT                                                          02070000
*---------------------------------------------------------------------* 02080000
* INPUTS APPEAR OK, SO CALL THE ENCODER                               * 02090000
*---------------------------------------------------------------------* 02100000
PRX001   DS    0H -                                                     02110000
         L     R15,=V(ENCODE)        ROUTINE ADDRESS                    02120000
         BASR  R14,R15               INVOKE IT                          02130000
         ST    R15,XCODE             SAVE RETURN CODE                   02140000
         B     EXIT                  RETURN TO CALLER                   02150000
         SPACE 2                                                        02160000
*---------------------------------------------------------------------* 02170000
* SUBROUTINES                                                         * 02180000
*---------------------------------------------------------------------* 02190000
         SPACE 2                                                        02200000
GETCOUNT DS    0H -                   GET PARAMETER LIST COUNT          02210000
*                                                                       02220000
* INPUTS  -                                                             02230000
*     R8  - POINTER TO PARAMETER LIST                                   02240000
*                                                                       02250000
* OUTPUTS -                                                             02260000
*     R15 - # PARAMETERS SPECIFIED BY CALLER                            02270000
*                                                                       02280000
         SR    R15,R15                ASSUME NO PARAMETERS              02290000
         LA    R8,0(,R8)              CLEAR HI-ORDER BIT(S)             02300000
         LTR   R8,R8                  VALID ADDRESS?                    02310000
         BNPR  R14                     NO  - QUIT                       02320000
         LA    R1,0(,R8)              POINT TO FIRST PARAMETER          02330000
         LA    R0,5                   MAX PARAMETERS ALLOWED            02340000
GCM001   DS    0H -                                                     02350000
         LA    R15,1(,R15)            INCR PARM COUNTER                 02360000
         TM    0(R1),X'80'            LAST IN THE LIST?                 02370000
         BOR   R14                     YES - EXIT                       02380000
         LA    R1,4(,R1)              BUMP TO NEXT PARAMETER            02390000
         BCT   R0,GCM001              LOOP TILL MAX CHECKED             02400000
         BR    R14                    RETURN TO CALLER                  02410000
         SPACE 2                                                        02420000
MSTRLEN  DS    0H -                   GET STRING LENGTH                 02430000
*                                                                       02440000
* INPUTS  -                                                             02450000
*     R3  - FIELD LENGTH                                                02460000
*     R4  - FIELD ADDRESS                                               02470000
*                                                                       02480000
* OUTPUTS -                                                             02490000
*     R0  - LENGTH OF STRING WITHIN FIELD                               02500000
*                                                                       02510000
         LR    R0,R3                  XFER LENGTH                       02520000
         LR    R1,R4                  XFER ADDRESS                      02530000
         AR    R1,R0                  FIRST BYTE PAST FIELD             02540000
MSTR01   BCTR  R1,R0                  PREV FIELD ADDRESS                02550000
         CLI   0(R1),X'00'            NUL TERMINATOR?                   02560000
         BE    MSTR02                  YES - CONTINUE                   02570000
         CLI   0(R1),C' '             SPACE?                            02580000
         BE    MSTR02                  YES - CONTINUE                   02590000
         LA    R0,1(,R1)              1ST CHAR PAST FIELD               02600000
         SR    R0,R4                  STRING LENGTH                     02610000
         BR    R14                    RETURN TO CALLER                  02620000
MSTR02   BCT   R0,MSTR01              LOOP TILL DONE                    02630000
         BR    R14                    RETURN TO CALLER                  02640000
         EJECT                                                          02650000
MLOG     DS    0H -                   PERFORM TRACE ACTION              02660000
*                                                                       02670000
* INPUTS  -                                                             02680000
*     R0  - REQUEST CODE                                                02690000
*                                                                       02700000
         STM   R14,R1,TRET            SAVE RETURN ADDRESS               02710000
         L     R15,=V(ENCXLOG)        LOG ROUTINES                      02720000
         BASR  R14,R15                CALL LOGGER                       02730000
         LM    R14,R1,TRET            LOAD RETURN ADDRESS               02740000
         BR    R14                    RETURN TO CALLER                  02750000
         SPACE 2                                                        02760000
*---------------------------------------------------------------------* 02770000
* ERROR ROUTINES                                                      * 02780000
*---------------------------------------------------------------------* 02790000
         SPACE 2                                                        02800000
EXITERR  DS    0H -                   ERROR DETECTED                    02810000
         ST    R7,XCODE               STOW ERROR CODE                   02820000
         B     EXIT                   LEAVE THIS EARTH                  02830000
         EJECT                                                          02840000
*---------------------------------------------------------------------* 02850000
* CONSTANTS                                                           * 02860000
*---------------------------------------------------------------------* 02870000
         DS    0H -                                                     02880000
CHECKNUM TRT   0(0,R4),TRNUM                                            02890000
         SPACE 2                                                        02900000
SPACES   DC    256C' '                                                  02910000
ZEROS    DC    256X'00'                                                 02920000
TRNUM    DS    0A -                                                     02930000
         DC    240X'FF'                                                 02940000
         DC    10X'00'                                                  02950000
         DC    (256-(*-TRNUM))X'FF'                                     02960000
         SPACE 2                                                        02970000
         LTORG                                                          02980000
         EJECT                                                          02990000
*---------------------------------------------------------------------* 03000000
* WORK AREAS                                                          * 03010000
*---------------------------------------------------------------------* 03020000
WORKA    DS    0D -                  WORK AREA                          03030000
ISAVA    DS    18F                   REGISTER SAVE AREA                 03040000
PRET     DS    12A                   ROUTINE RETURN ADDRESS             03050000
TRET     DS    4A                    ROUTINE RETURN ADDRESS             03060000
XCODE    DS    A                     API RETURN CODE                    03070000
NPARMS   DS    A                     # PARAMETERS SPECIFIED ON CALL     03080000
         DS    0D                    FORCE DOUBLEWORD ALIGNMENT         03090000
MGWA0    DS    (GWL)X                PARAMETER WORK AREA                03100000
WLEN     EQU   *-WORKA               WORK AREA SIZE                     03110000
         DROP  R12,R11                                                  03120000
