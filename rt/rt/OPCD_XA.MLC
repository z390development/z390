OPCD_XA  TITLE 'Reference program to test opcode generation'
***********************************************************************
* Start create : 18-10-2014
* 1st delivery :
* Designer     : AF Kornelis
* Programmer   : AF Kornelis
* Purpose      : Reference assembly of HLASM-compatible instructions
*                Regression test for opcodes in optable DOS
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
OPCD_XA  RSECT ,                       *
OPCD_XA  AMODE 24
OPCD_XA  RMODE 24
***********************************************************************
         USING OPCD_XA,R15             * Establish addressability
         B     TESTOPC2                * Skip header data
         DC    AL1(TESTOPC2-*),C'OPCD_XA &SYSDATE &SYSTIME'
TESTOPC2 EQU   *
         STM   R14,R12,12(R13)         * Save GPRs
         LR    R12,R15                 * Copy base address
         LR    R2,R1                   * Copy ptr to parm string
         DROP  R15                     * No longer needed
         USING OPCD_XA,R12             * Re-establish addressability
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
         MVC   WAEYE,=CL8'OPCD_XA'     * Init Eye-catcher

***********************************************************************
*
* 0B = BSM - Branch and Set Mode
* --> modal instruction !!
*
***********************************************************************
*
* BSM in Amode 24
         LG    R14,=X'F1F2F3F4F5F6F7F8'
         LA    R15,BSM24A              * To Amode 24
         BSM   R14,R0                  * 0BE0 no branch
         BSM   R14,R15                 * 0BEF Amode remains 24
         DC    X'0000'                 *
BSM24A   LA    R15,BSM24B              *
         O     R15,=X'80000000'        * To Amode31
         O     R14,=X'80000000'        * basic Amode save-bit on
         BSM   R14,R15                 * 0BEF
         DC    X'0000'                 *
BSM24B   SAM24                         * Back to amode24
         XGR   R15,R15                 *
         LA    R15,BSM24C+1            * To Amode 64
         LG    R0,=X'F1F2F3F4F5F6F7F8'
         BSM   R0,R15                  * 0B0F
         DC    X'0000'                 *
BSM_EX   BSM   R0,R0                   * 0B00 Target of EX
BSM24C   SAM24                         * Back to amode24
*
* EX of BSM in Amode24
*
* BSM in Amode 31
*
* EX of BSM in Amode31
*
* BSM in Amode 64
*
* EX of BSM in Amode64
***********************************************************************
*
* 0C = BASSM - Branch And Save and Set Mode
* --> modal instruction !!
*
***********************************************************************
*
* BASSM in Amode 24
         LG    R14,=X'F1F2F3F4F5F6F7F8'
         LA    R15,BASSM24A            * To Amode 24
         BASSM R14,R0                  * 0CE0 no branch
         BASSM R14,R15                 * 0CEF Amode remains 24
         DC    X'0000'                 *
BASSM24A LA    R15,BASSM24B            *
         O     R15,=X'80000000'        * To Amode31
         BASSM R14,R15                 * 0CEF
         DC    X'0000'                 *
BASSM24B SAM24                         * Back to amode24
         XGR   R15,R15                 *
         LA    R15,BASSM24C+1          * To Amode 64
         BASSM R14,R15                 * 0CEF
         DC    X'0000'                 *
BASSM_EX BASSM R0,R0                   * 0C00 Target of EX
BASSM24C SAM24                         * Back to amode24
*
* EX of BASSM in Amode24
         LA    R5,X'67'                * Alter BASSM to use R6,R7
         LG    R6,=X'F1F2F3F4F5F6F7F8' *
         LGR   R0,R6                   *
         LA    R7,BASSM24D             *
         EX    R0,BASSM_EX             * No branch
         EX    R5,BASSM_EX             * Branch, keep amode24
         DC    X'0000'                 *
BASSM24D LA    R5,X'07'                * Alter BASSM to R0,R7
         LA    R7,BASSM24E             *
         O     R7,=X'80000000'         * Force amode31
         EX    R5,BASSM_EX             * Branch, set amode31
         DC    X'0000'                 *
BASSM24E SAM24                         * Back to amode24
         XGR   R7,R7                   *
         LA    R7,BASSM24X+1           * To Amode 64
         EX    R5,BASSM_EX             * Branch, set amode31
         DC    X'0000'                 *
BASSM24X DS    0H                      * End of Amode 24 test
*
* BASSM in Amode 31
BASSM31A SAM31                         * Back to amode31
         LA    R15,BASSM31B            * To Amode 24
         BASSM R14,R0                  * 0CE0 no branch
         BASSM R14,R15                 * 0CEF branch, set amode24
         DC    X'0000'                 *
BASSM31B SAM31                         * Back to amode31
         LA    R5,BASSM31C             *
         O     R5,=X'80000000'         * To Amode31
         BASSM R4,R5                   * 0C45
         DC    X'0000'                 *
BASSM31C XGR   R5,R5                   *
         LA    R5,BASSM31D+1           * To Amode 64
         BASSM R4,R5                   * 0C45
         DC    X'0000'                 *
*
* EX of BASSM in Amode31
BASSM31D SAM31                         * Back to amode31
         LA    R5,X'67'                * Alter BASSM to use R6,R7
         LG    R6,=X'F1F2F3F4F5F6F7F8' *
         LGR   R0,R6                   *
         LA    R7,BASSM31E             *
         EX    R0,BASSM_EX             * No branch
         EX    R5,BASSM_EX             * Branch, keep amode24
         DC    X'0000'                 *
BASSM31E LA    R5,X'07'                * Alter BASSM to R0,R7
         LA    R7,BASSM31F             *
         O     R7,=X'80000000'         * Force amode31
         EX    R5,BASSM_EX             * Branch, set amode31
         DC    X'0000'                 *
BASSM31F SAM31                         * Back to amode31
         XGR   R7,R7                   *
         LA    R7,BASSM31X+1           * To Amode 64
         EX    R5,BASSM_EX             * Branch, set amode31
         DC    X'0000'                 *
BASSM31X DS    0H                      * End of Amode 31 test
*
*
BASSM64A SAM64                         * Back to amode64
         LA    R15,BASSM64B            *
         O     R15,=X'80000000'        * To Amode31
         BASSM R14,R15                 * 0CEF Branch, set amode31
         DC    X'0000'                 *
BASSM64B SAM64                         * Back to amode64
         XGR   R15,R15                 *
         LA    R14,BASSM64C+1          * To Amode 64
         BASSM R14,R15                 * 0CEF
         DC    X'0000'                 *
BASSM64C SAM24                         * From amode64 to amode24

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
