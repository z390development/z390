OPCDX_UNI TITLE 'Reference program to test z390-only opcode generation'
***********************************************************************
* Start create : 18-10-2014
* 1st delivery :
* Designer     : AF Kornelis
* Programmer   : AF Kornelis
* Purpose      : Reference assembly of instructions not compatible
*                   with HLASM
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
OPCDX_UNI RSECT ,                       *
OPCDX_UNI AMODE 31
OPCDX_UNI RMODE ANY
***********************************************************************
         USING OPCDX_UNI,R15            * Establish addressability
         B     TESTOPC2                * Skip header data
         DC    AL1(TESTOPC2-*),C'OPCDX_UNI &SYSDATE &SYSTIME'
TESTOPC2 EQU   *
         STM   R14,R12,12(R13)         * Save GPRs
         LR    R12,R15                 * Copy base address
         LR    R2,R1                   * Copy ptr to parm string
         DROP  R15                     * No longer needed
         USING OPCDX_UNI,R12            * Re-establish addressability
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
         MVC   WAEYE,=CL8'OPCDX_UNI'    * Init Eye-catcher
         B     EXIT                    * Never execute code below

***********************************************************************
*
* Test RR instruction formats
*
***********************************************************************
         SSK   R3,R4                   * 0834 (obsolete)
         ISK   R3,R4                   * 0934 (obsolete)

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
