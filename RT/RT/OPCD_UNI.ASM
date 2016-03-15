OPCD_UNI TITLE 'Reference program to test opcode generation'
***********************************************************************
* Start create : 18-10-2014
* 1st delivery :
* Designer     : AF Kornelis
* Programmer   : AF Kornelis
* Purpose      : Reference assembly of HLASM-compatible instructions
***********************************************************************

***********************************************************************
*
* Define constants and environment
*
***********************************************************************
*
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
* Floating Point Register equates
FPR0     EQU   0
FPR1     EQU   1
FPR2     EQU   2
FPR3     EQU   3
FPR4     EQU   4
FPR5     EQU   5
FPR6     EQU   6
FPR7     EQU   7
FPR8     EQU   8
FPR9     EQU   9
FPR10    EQU   10
FPR11    EQU   11
FPR12    EQU   12
FPR13    EQU   13
FPR14    EQU   14
FPR15    EQU   15
*
* Control Register equates
CR0      EQU   0
CR1      EQU   1
CR2      EQU   2
CR3      EQU   3
CR4      EQU   4
CR5      EQU   5
CR6      EQU   6
CR7      EQU   7
CR8      EQU   8
CR9      EQU   9
CR10     EQU   10
CR11     EQU   11
CR12     EQU   12
CR13     EQU   13
CR14     EQU   14
CR15     EQU   15

***********************************************************************
*
* Define workareas and fixed constants
*
***********************************************************************
WORKAREA DSECT ,                       *
SAVEAREA DS    18F                     * Save area for GPRs
WAEYE    DS    CL8                     * Eye Catcher
WAPRMPTR DS    A                       * Ptr to parmstring
         DS    0D
WORKAREA_LEN EQU *-WORKAREA

***********************************************************************
*
* Program entry and linkage
*
***********************************************************************
OPCD_UNI RSECT ,                       *
OPCD_UNI AMODE 24
OPCD_UNI RMODE 24
***********************************************************************
* Intro (15 instructions)
         USING OPCD_UNI,R15            * Establish addressability
         B     TESTOPC2                * Skip header data
         DC    AL1(TESTOPC2-*),C'OPCD_UNI &SYSDATE &SYSTIME'
TESTOPC2 EQU   *
         STM   R14,R12,12(R13)         * Save GPRs
         LR    R12,R15                 * Copy base address
         LR    R2,R1                   * Copy ptr to parm string
         DROP  R15                     * No longer needed
         USING OPCD_UNI,R12            * Re-establish addressability
         LA    R11,WORKAREA_LEN        * Size of req'd workarea
         GETMAIN R,LV=(R11),LOC=BELOW  * Acquire working storage
         ST    R1,8(,R13)              * Set ptr to new savearea
         ST    R13,4(,R1)              * Set ptr from ne to prev save
         LR    R13,R1                  * Activate new savearea
         USING WORKAREA,R13            * Set addressable
         XR    R0,R0                   * Set to zero
         ST    R0,SAVEAREA             * Nullify header-word
         ST    R2,WAPRMPTR             * Save pointer to parmstring

***********************************************************************
*
* Set up working storage areas
*
***********************************************************************
         MVC   WAEYE,=CL8'OPCD_UNI'    * Init Eye-catcher

&V(4)   SETC   'L'
        LA     0,=A&V(4)(3)(2)(1)

***********************************************************************
*
* Test RR instruction formats
*
***********************************************************************
* 05 = BALR (modal)
* Amode 24: R14:=ILC+CC+ProgMask+RetAddr
         SAM24
         LA    R15,BALR24
         C     R15,=X'FFFFFFFF'        * Force CC=1
         BALR  R14,R0                  * 05E0 (no branch)
         BALR  R14,R15                 * 05EF
         DC    X'0000'
* Amode 31: R4:=1+RetAddr
BALR24   SAM31
         LA    R5,BALR31
         BALR  R4,R0                   * 0540 (no branch)
         BALR  R4,R5                   * 0545
         DC    X'0000'
* Amode 64: R14:=RetAddr
BALR31   SAM64
         XGR   R15,R15
         LGFR  R15,R15
         LA    R15,BALR64
         BALR  R14,R0                  * 05E0 (no branch)
         BALR  R14,R15                 * 05EF
         DC    X'0000'
BALR64   SAM24

* 06 = BCTR
         LA    R6,2
         LA    R7,BCTR
BCTR     BCTR  R6,R7                   * 0667 (loop-back+drop-through)
         LA    R7,BCTR2
         BCTR  R6,R7                   * 0667 (wrap to X'FFFFFFFF')
         DC    X'0000'
BCTR2    DS    0H

* 0C = BASSM (modal)
         LA    R15,BASSM24A            * To Amode 24
         BASSM R14,R0                  * 0CE0 no branch
         BASSM R14,R15                 * 0CEF
         DC    X'0000'
BASSM24A LA    R15,BASSM24B
         O     R15,=X'80000000'        * To Amode31
         BASSM R14,R15                 * 0CEF
         DC    X'0000'
BASSM24B SAM24
         XGR   R15,R15
         LA    R15,BASSM24C+1          * To Amode 64
         BASSM R14,R15                 * 0CEF
         DC    X'0000'
BASSM24C SAM31
         LA    R5,BASSM31A             * To Amode 24
         BASSM R4,R0                   * 0C40 no branch
         BASSM R4,R5                   * 0C45
         DC    X'0000'
BASSM31A SAM31
         LA    R5,BASSM31B
         O     R5,=X'80000000'         * To Amode31
         BASSM R4,R5                   * 0C45
         DC    X'0000'
BASSM31B XGR   R5,R5
         LA    R5,BASSM31C+1           * To Amode 64
         BASSM R4,R5                   * 0C45
         DC    X'0000'
BASSM31C LA    R15,BASSM64A            * To Amode 24
         BASSM R14,R0                  * 0CE0 no branch
         BASSM R14,R15                 * 0CEF
         DC    X'0000'
BASSM64A SAM64
         LA    R15,BASSM64B
         O     R15,=X'80000000'        * To Amode31
         BASSM R14,R15                 * 0CEF
         DC    X'0000'
BASSM64B SAM64
         XGR   R15,R15
         LA    R14,BASSM64C+1          * To Amode 64
         BASSM R14,R15                 * 0CEF
         DC    X'0000'
BASSM64C SAM24

         BSM   R15,R14                 * 0BFE
         BASR  R14,R15                 * 0DEF
         LPR   R4,R5                   * 1045
         LNR   R4,R5                   * 1145
         LTR   R14,R15                 * 12EF
         LCR   R4,R5                   * 1345
         NR    R6,R8                   * 1468
         CLR   R7,R4                   * 1574
         OR    R8,R9                   * 1689
         XR    R9,R8                   * 1798
         LR    R12,R15                 * 18CF
         CR    R8,R5                   * 1985
         AR    R7,R10                  * 1A7A
         SR    R5,R6                   * 1B56
         MR    R6,R8                   * 1C68
         MR    R6,R7                   * 1C67
         DR    R8,R6                   * 1D86
         DR    R8,R9                   * 1D89
         ALR   R8,R3                   * 1E83
         SLR   R4,R7                   * 1F47

***********************************************************************
*
* Test RR-f instruction formats
*
***********************************************************************
         LPDR  FPR6,FPR7               * 2067
         LNDR  FPR6,FPR7               * 2167
         LTDR  FPR4,FPR14              * 224E
         LCDR  FPR3,FPR4               * 2334
         HDR   FPR2,FPR7               * 2427
         LDXR  FPR4,FPR9               * 2549
         LRDR  FPR9,FPR4               * 2549
         MXR   FPR6,FPR7               * 2667
         MXDR  FPR6,FPR7               * 2767
         LDR   FPR14,FPR15             * 28EF
         CDR   FPR3,FPR8               * 2938
         ADR   FPR5,FPR4               * 2A54
         SDR   FPR5,FPR6               * 2B56
         MDR   FPR9,FPR11              * 2C9B
         DDR   FPR5,FPR1               * 2D51
         AWR   FPR3,FPR2               * 2E32
         SWR   FPR7,FPR6               * 2F76
         LPER  FPR13,FPR0              * 30D0
         LNER  FPR1,FPR15              * 311F
         LTER  FPR4,FPR7               * 3247
         LCER  FPR4,FPR7               * 3347
         HER   FPR5,FPR5               * 3455
         LEDR  FPR6,FPR7               * 3567
         LRER  FPR7,FPR6               * 3576
         AXR   FPR3,FPR8               * 3638
         SXR   FPR5,FPR2               * 3752
         LER   FPR4,FPR9               * 3849
         CER   FPR6,FPR13              * 396D
         AER   FPR11,FPR12             * 3ABC
         SER   FPR7,FPR0               * 3B70
         MDER  FPR4,FPR11              * 3C4B
         MER   FPR5,FPR10              * 3C5A
         DER   FPR2,FPR7               * 3D27
         AUR   FPR14,FPR2              * 2EE2
         SUR   FPR15,FPR2              * 3FF2

***********************************************************************
*
* Test RR-m instruction formats
*
***********************************************************************
         BCR   X'07',R1                * 0771
* Extended mnemonics RR-mx format
         NOPR  R5                      * 0705
         BOR   R5                      * 0715
         BHR   R5                      * 0725
         BPR   R5                      * 0725
         BLR   R5                      * 0745
         BMR   R5                      * 0745
         BNER  R5                      * 0775
         BNZR  R5                      * 0775
         BER   R5                      * 0785
         BZR   R5                      * 0785
         BNLR  R5                      * 07B5
         BNMR  R5                      * 07B5
         BNHR  R5                      * 07D5
         BNPR  R5                      * 07D5
         BNOR  R5                      * 07E5
         BR    R5                      * 07F5

***********************************************************************
*
* Test RR-n instruction formats
*
***********************************************************************
         SPM   R4                      * 0440

***********************************************************************
*
* Test RR-p instruction formats
*
***********************************************************************
         MVCL  R2,R4                   * 0E24
         CLCL  R2,R4                   * 0F24

***********************************************************************
*
* Test RX instruction formats
*
***********************************************************************

***********************************************************************
*
* Test RS instruction formats
*
***********************************************************************

***********************************************************************
*
* Test SI instruction formats
*
***********************************************************************

***********************************************************************
*
* Test SS instruction formats
*
***********************************************************************

***********************************************************************
*
* To exit program - release all acquired storage
*
* Register allocations:
* R13 - Workarea
* R12 - Codebase
* R11 - PRIMHDR area
* R10 - bitmap pointer
*
***********************************************************************
EXIT     DS    0H
         L     R11,4(,R13)             * Get ptr to prev savearea
         LA    R10,WORKAREA_LEN        * Size of area to be freed
         FREEMAIN RU,LV=(R10),A=(R13)  * Free workarea
         DROP  R13                     * End addressability of workarea
         LR    R13,R11                 * Set prev savearea current
         LM    R14,R12,12(R13)         * Reset all registers
         XR    R15,R15                 * Set RC=0
         BR    R14                     * Return

         DROP  ,
***********************************************************************
         LTORG
         END
