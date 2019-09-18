RPI1625  CSECT
         USING  *,R15
         STM    R14,R12,12(R13)     * SAve regs
         LA     R1,SAVEAREA         * Point new SA
         ST     R1,8(,R13)          * Set forward ptr in old SA
         ST     R13,SAVEPREV        * Set backward ptr
         LR     R13,R1              * Point new SA
         DROP   R15                 * Switch
         LR     R12,R15             *        base
         USING  R15                 *             register
*
         LR     R5,5                * Set repeat counter
REPEAT   DS     0H
         WTOR   
         BCT    R5,REPEAT           *
*
         L      R13,SAVEPREV        * Point prior SA
         LM     R14,R12,12(R13)     * Reset all registers
         XR     R15,R15             * Set RC=0
         BR     R14                 * Return
*
WORKAREA DS     0D
SAVEAREA DS     18F
SAVEPREV EQU    SAVEAREA+4
SAVENEXT EQU    SAVEAREA+8
         END
