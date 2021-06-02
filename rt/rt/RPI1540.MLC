RPI1540  TITLE 'Reference program to test CLIJ / CLGIJ'
***********************************************************************
* Start create : 11-02-2016
* 1st delivery :
* Designer     : AF Kornelis
* Programmer   : AF Kornelis
* Purpose      : Test CLIJ / CLGIJ operation
***********************************************************************

***********************************************************************
*
* Define constants and environment
*
***********************************************************************
         EQUREGS
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
RPI1540  RSECT ,                       *
RPI1540  AMODE 24
RPI1540  RMODE 24
***********************************************************************
         USING RPI1540,R15             * Establish addressability
         B     TESTOPC2                * Skip header data
         DC    AL1(TESTOPC2-*),C'RPI1540  &SYSDATE &SYSTIME'
TESTOPC2 EQU   *
         STM   R14,R12,12(R13)         * Save GPRs
         LR    R12,R15                 * Copy base address
         LR    R2,R1                   * Copy ptr to parm string
         DROP  R15                     * No longer needed
         USING RPI1540,R12             * Re-establish addressability
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
         MVC   WAEYE,=CL8'RPI1540 '    * Init Eye-catcher

***********************************************************************
*
* EC64 = CGRJ - Compare Grande Register and Jump
*
* operand1 is 64-bit signed binary integer
* operand2 is 64-bit signed binary integer
* R0 is usable as operand1 and functions normally
* R0 is usable as operand2 and functions normally
*
* CC remains unchanged
*
***********************************************************************
CGRJ     LARL  R12,*
         USING CGRJ,R12                 * Set Base
* Compare positive with positive
         LGHI  R4,64
         CGR   R4,R4                    * Set CC=0
         LGHI  R5,80
         LGHI  R6,48
         CGRJL R4,R6,CGRJ_ERR           * EC4600064064 should not jump
         CGRJL R4,R5,CGRJ_1             * EC4500044064 should jump
CGRJ_ERR DC    X'0000'
* Compare positive with negative
CGRJ_1   LGHI  R5,-64
         CGRJL R4,R5,CGRJ_ERR           * EC45FFFD4064 should not jump
         CGRJH R4,R5,CGRJ_2             * EC4500042064 should jump
         DC    X'0000'
* Compare negative with negative
CGRJ_2   LGHI  R5,-8
         LGHI  R8,-16
         LGHI  R9,-4
         CGRJL R5,R8,CGRJ_ERR           * EC58FFF04064 should not jump
         CGRJH R5,R9,CGRJ_ERR           * EC59FFED2064 should not jump
* Compare negative with positive
         LGHI  R9,10
         CGRJH R5,R9,CGRJ_ERR           * EC59FFE82064 should not jump
         CGRJL R5,R9,CGRJ_3             * EC5900044064 should jump
         DC    X'0000'
* Compare equal
CGRJ_3   LGR   R6,R5
         CGRJE R5,R6,CGRJ_4             * EC5600048064 should jump
         DC    X'0000'
* Try with R0
CGRJ_4   LGHI  R0,64
         LGHI  R4,48
         LGHI  R5,80
         CGRJL R0,R4,CGRJ_5             * EC0400074064 should not jump
         CGRJL R0,R5,CGRJ_5             * EC0500044064 should jump
         DC    X'0000'
CGRJ_5   EQU   *
         CGRJH R4,R0,CGRJ_6             * EC4000072064 should not jump
         CGRJH R5,R0,CGRJ_6             * EC5000042064 should jump
         DC    X'0000'
CGRJ_6   EQU   *

***********************************************************************
*
* EC65 = CLGRJ - Compare Logical Grande Register and Jump
*
* operand1 is 64-bit unsigned binary integer
* operand2 is 64-bit unsigned binary integer
* R0 is usable as operand1 and functions normally
* R0 is usable as operand2 and functions normally
*
* CC remains unchanged
*
***********************************************************************
CLGRJ    LARL   R12,*
         USING  CLGRJ,R12               * Set Base
* Compare positive with positive
         LGHI   R4,64
         CGR    R4,R4                   * Set CC=0
         LGHI   R5,80
         LGHI   R6,48
         CLGRJL R4,R6,CLGRJ_ERR         * EC4600064065 should not jump
         CLGRJL R4,R5,CLGRJ_1           * EC4500044065 should jump
CLGRJ_ERR DC    X'0000'
* Compare positive with negative
CLGRJ_1  LGHI   R5,-64
         CLGRJH R4,R5,CLGRJ_ERR         * EC45FFFD2065 should not jump
         CLGRJL R4,R5,CLGRJ_2           * EC4500044065 should jump
         DC     X'0000'
* Compare negative with negative
CLGRJ_2  LGHI   R5,-8
         LGHI   R8,-16
         LGHI   R9,-4
         CLGRJH R5,R9,CLGRJ_ERR         * EC59FFF02065 should not jump
         CLGRJL R5,R8,CLGRJ_ERR         * EC58FFED4065 should not jump
* Compare negative with positive
         LGHI   R9,10
         CLGRJL R5,R9,CLGRJ_ERR         * EC59FFE84065 should not jump
         CLGRJH R5,R9,CLGRJ_3           * EC5900042065 should jump
         DC     X'0000'
* Compare equal
CLGRJ_3  LGR    R6,R5
         CLGRJE R5,R6,CLGRJ_4           * EC5600048065 should jump
         DC     X'0000'
* Try with R0
CLGRJ_4  LGHI   R0,64
         LGHI   R4,48
         LGHI   R5,80
         CLGRJL R0,R4,CLGRJ_5           * EC0400074065 should not jump
         CLGRJL R0,R5,CLGRJ_5           * EC0500044065 should jump
         DC     X'0000'
CLGRJ_5  EQU    *
         CLGRJH R4,R0,CLGRJ_6           * EC4000072065 should not jump
         CLGRJH R5,R0,CLGRJ_6           * EC5000042065 should jump
         DC     X'0000'
CLGRJ_6  EQU    *

***********************************************************************
*
* EC76 = CRJ - Compare Register and Jump
*
* operand1 is 32-bit signed binary integer
* operand2 is 32-bit signed binary integer
* R0 is usable as operand1 and functions normally
* R0 is usable as operand2 and functions normally
*
* CC remains unchanged
*
***********************************************************************
CRJ      LARL  R12,*
         USING CRJ,R12                  * Set Base
* Compare positive with positive
         LG    R4,=X'00FF00FF00000040'  * X'40' = 64
         CR    R4,R4                    * Set CC=0
         LHI   R5,80
         LHI   R6,48
         CRJL  R4,R6,CRJ_ERR            * EC4600064076 should not jump
         CRJL  R4,R5,CRJ_1              * EC4500044076 should jump
CRJ_ERR  DC    X'0000'
* Compare positive with negative
CRJ_1    LHI   R5,-64
         CRJL  R4,R5,CRJ_ERR            * EC45FFFD4076 should not jump
         CRJH  R4,R5,CRJ_2              * EC4500042076 should jump
         DC    X'0000'
* Compare negative with negative
CRJ_2    LHI   R5,-8
         LHI   R8,-16
         LHI   R9,-4
         CRJL  R5,R8,CRJ_ERR            * EC58FFF04076 should not jump
         CRJH  R5,R9,CRJ_ERR            * EC59FFED2076 should not jump
* Compare negative with positive
         LHI   R9,10
         CRJH  R5,R9,CRJ_ERR            * EC59FFE82076 should not jump
         CRJL  R5,R9,CRJ_3              * EC5900044076 should jump
         DC    X'0000'
* Compare equal
CRJ_3    LR    R6,R5
         CRJE  R5,R6,CRJ_4              * EC5600048076 should jump
         DC    X'0000'
* Try with R0
CRJ_4    LHI   R0,64
         LHI   R4,48
         LHI   R5,80
         CRJL  R0,R4,CRJ_5              * EC0400074076 should not jump
         CRJL  R0,R5,CRJ_5              * EC0500044076 should jump
         DC    X'0000'
CRJ_5    EQU   *
         CRJH  R4,R0,CRJ_6              * EC4000072076 should not jump
         CRJH  R5,R0,CRJ_6              * EC5000042076 should jump
         DC    X'0000'
CRJ_6    EQU   *

***********************************************************************
*
* EC77 = CLRJ - Compare Logical Register and Jump
*
* operand1 is 32-bit unsigned binary integer
* operand2 is 32-bit unsigned binary integer
* R0 is usable as operand1 and functions normally
* R0 is usable as operand2 and functions normally
*
* CC remains unchanged
*
***********************************************************************
CLRJ     LARL  R12,*
         USING CLRJ,R12                 * Set Base
* Compare positive with positive
         LGHI  R4,64
         CGR   R4,R4                    * Set CC=0
         LGHI  R5,80
         LGHI  R6,48
         CLRJL R4,R6,CLRJ_ERR           * EC4600064077 should not jump
         CLRJL R4,R5,CLRJ_1             * EC4500044077 should jump
CLRJ_ERR DC    X'0000'
* Compare positive with negative
CLRJ_1   LGHI  R5,-64
         CLRJH R4,R5,CLRJ_ERR           * EC45FFFD2077 should not jump
         CLRJL R4,R5,CLRJ_2             * EC4500044077 should jump
         DC    X'0000'
* Compare negative with negative
CLRJ_2   LGHI  R5,-8
         LGHI  R8,-16
         LGHI  R9,-4
         CLRJH R5,R9,CLRJ_ERR           * EC59FFF02077 should not jump
         CLRJL R5,R8,CLRJ_ERR           * EC58FFED4077 should not jump
* Compare negative with positive
         LGHI  R9,10
         CLRJL R5,R9,CLRJ_ERR           * EC59FFE84077 should not jump
         CLRJH R5,R9,CLRJ_3             * EC5900042077 should jump
         DC    X'0000'
* Compare equal
CLRJ_3   LGR   R6,R5
         CLRJE R5,R6,CLRJ_4             * EC5600048077 should jump
         DC    X'0000'
* Try with R0
CLRJ_4   LGHI  R0,64
         LGHI  R4,48
         LGHI  R5,80
         CLRJL R0,R4,CLRJ_5             * EC0400074077 should not jump
         CLRJL R0,R5,CLRJ_5             * EC0500044077 should jump
         DC    X'0000'
CLRJ_5   EQU   *
         CLRJH R4,R0,CLRJ_6             * EC4000072077 should not jump
         CLRJH R5,R0,CLRJ_6             * EC5000042077 should jump
         DC    X'0000'
CLRJ_6   EQU   *

***********************************************************************
*
* EC7C = CGIJ - Compare Grande Immediate and Jump
*
* operand1 is 64-bit signed binary integer
* operand2 is  8-bit signed binary integer
* R0 is usable as operand1 and functions normally
*
* CC remains unchanged
*
***********************************************************************
CGIJ     LARL  R12,*
         USING CGIJ,R12                 * Set Base
* Compare positive with positive
         LGHI  R4,64
         CGR   R4,R4                    * Set CC=0
         CGIJL R4,48,CGIJ_ERR           * EC440006307C should not jump
         CGIJL R4,80,CGIJ_1             * EC440004507C should jump
CGIJ_ERR DC    X'0000'
* Compare positive with negative
CGIJ_1   CGIJL R4,-64,CGIJ_ERR          * EC44FFFFC07C should not jump
         CGIJH R4,-64,CGIJ_2            * EC420004C07C should jump
         DC    X'0000'
* Compare negative with negative
CGIJ_2   LGHI  R5,-8
         CGIJL R5,-16,CGIJ_ERR          * EC54FFF6F07C should not jump
         CGIJH R5,-4,CGIJ_ERR           * EC52FFF3FC7C should not jump
* Compare negative with positive
         CGIJH R5,10,CGIJ_ERR           * EC52FFF00A7C should not jump
         CGIJL R5,10,CGIJ_3             * EC5400040A7C should jump
         DC    X'0000'
* Compare equal
CGIJ_3   CGIJE R5,-8,CGIJ_4             * EC580004F87C should jump
         DC    X'0000'
* Try with R0
CGIJ_4   LGHI  R0,64
         CGIJL R0,48,CGIJ_5             * EC040007307C should not jump
         CGIJL R0,80,CGIJ_5             * EC040004507C should jump
         DC    X'0000'
CGIJ_5   EQU   *

***********************************************************************
*
* EC7D = CLGIJ - Compare Logical Grande Immediate and Jump
*
* operand1 is 64-bit unsigned binary integer
* operand2 is  8-bit unsigned binary integer
* R0 is usable as operand1 and functions normally
*
* CC remains unchanged
*
***********************************************************************
CLGIJ     LARL   R12,*
          USING  CLGIJ,R12              * Set Base
* Compare positive with positive
          LGHI   R4,64
          CR     R4,R4                  * Set CC=0
          CLGIJL R4,48,CLGIJ_ERR        * EC440006307D should not jump
          CLGIJL R4,80,CLGIJ_1          * EC440004507D should jump
CLGIJ_ERR DC     X'0000'
* Compare positive with negative
CLGIJ_1   CLGIJH R4,-64,CLGIJ_ERR       * EC42FFFFC07D should not jump
          CLGIJL R4,-64,CLGIJ_2         * EC440004C07D should jump
          DC     X'0000'
* Compare negative with negative
CLGIJ_2   LGHI   R5,-8
* RPI 1540: arith: -8 < -4 But logically: X'FFFFFFF8' > X'000000FC' 
          CLGIJL R5,-4,CLGIJ_ERR        * EC54FFF6FC7D should not jump
****      CLGIJH R5,-4,CLGIJ_ERR        * Fixed: CLGIJL _does_ jump
          CLGIJL R5,-16,CLGIJ_ERR       * EC54FFF3F07D should not jump
* Compare negative with positive
          CLGIJL R5,10,CLGIJ_ERR        * EC54FFF00A7D should not jump
          CLGIJH R5,10,CLGIJ_3          * EC5200040A7D should jump
          DC     X'0000'
* Compare equal
CLGIJ_3   CLGIJE R5,-8,CLGIJ_ERR        * EC58FFE9F87D should not jump
          LGHI   R5,25
          CLGIJE R5,25,CLGIJ_4          * EC580004197D should jump
          DC     X'0000'
* Try with R0
CLGIJ_4   LGHI   R0,64
          CLGIJL R0,48,CLGIJ_5          * EC040007307D should not jump
          CLGIJL R0,80,CLGIJ_5          * EC040004507D should jump
          DC     X'0000'
CLGIJ_5   EQU    *

***********************************************************************
*
* EC7E = CIJ - Compare Immediate and Jump
*
* operand1 is 32-bit signed binary integer
* operand2 is  8-bit signed binary integer
* R0 is usable as operand1 and functions normally
*
* CC remains unchanged
*
***********************************************************************
CIJ      LARL  R12,*
         USING CIJ,R12                  * Set Base
* Compare positive with positive
         LG    R4,=X'00FF00FF00000040'  * X'40' = 64
         CR    R4,R4                    * Set CC=0
         CIJL  R4,48,CIJ_ERR            * EC440006307E should not jump
         CIJL  R4,80,CIJ_1              * EC440004507E should jump
CIJ_ERR  DC    X'0000'
* Compare positive with negative
CIJ_1    CIJL  R4,-64,CIJ_ERR           * EC44FFFFC07E should not jump
         CIJH  R4,-64,CIJ_2             * EC420004C07E should jump
         DC    X'0000'
* Compare negative with negative
CIJ_2    LHI   R5,-8
         CIJL  R5,-16,CIJ_ERR           * EC54FFF6F07E should not jump
         CIJH  R5,-4,CIJ_ERR            * EC52FFF3FC7E should not jump
* Compare negative with positive
         CIJH  R5,10,CIJ_ERR            * EC52FFF00A7E should not jump
         CIJL  R5,10,CIJ_3              * EC5400040A7E should jump
         DC    X'0000'
* Compare equal
CIJ_3    CIJE  R5,-8,CIJ_4              * EC580004F87E should jump
         DC    X'0000'
* Try with R0
CIJ_4    LHI   R0,64
         CIJL  R0,48,CIJ_5              * EC040007307E should not jump
         CIJL  R0,80,CIJ_5              * EC040004507E should jump
         DC    X'0000'
CIJ_5    EQU   *

***********************************************************************
*
* EC7F = CLIJ - Compare Logical Immediate and Jump
*
* operand1 is 32-bit unsigned binary integer
* operand2 is  8-bit unsigned binary integer
* R0 is usable as operand1 and functions normally
*
* CC remains unchanged
*
***********************************************************************
CLIJ     LARL  R12,*
         USING CLIJ,R12                 * Set Base
* Compare positive with positive
         LG    R4,=X'00FF00FF00000040'  * X'40' = 64
         CR    R4,R4                    * Set CC=0
         CLIJL R4,48,CLIJ_ERR           * EC440006307F should not jump
         CLIJL R4,80,CLIJ_1             * EC440004507F should jump
CLIJ_ERR DC    X'0000'
* Compare positive with negative
CLIJ_1   CLIJH R4,-64,CLIJ_ERR          * EC42FFFFC07F should not jump
         CLIJL R4,-64,CLIJ_2            * EC440004C07F should jump
         DC    X'0000'
* Compare negative with negative
CLIJ_2   LGHI  R5,-8
* RPI 1540: arith: -8 < -4 But logically: X'FFFFFFF8' > X'000000FC' 
         CLIJL R5,-4,CLIJ_ERR           * EC52FFF6FC7F should not jump
****     CLIJH R5,-4,CLIJ_ERR           * Fixed: CLIJL _does_ jump
         CLIJL R5,-16,CLIJ_ERR          * EC54FFF3F07F should not jump
* Compare negative with positive
         CLIJL R5,10,CLIJ_ERR           * EC54FFF00A7F should not jump
         CLIJH R5,10,CLIJ_3             * EC5200040A7F should jump
         DC    X'0000'
* Compare equal
CLIJ_3   CLIJE R5,-8,CLIJ_ERR           * EC58FFE9F87F should not jump
         LGHI  R5,25
         CLIJE R5,25,CLIJ_4             * EC580004197F should jump
         DC    X'0000'
* Try with R0
CLIJ_4   LGHI  R0,64
         CLIJL R0,48,CLIJ_5             * EC040007307F should not jump
         CLIJL R0,80,CLIJ_5             * EC040004507F should jump
         DC    X'0000'
CLIJ_5   EQU   *

***********************************************************************
*
* ECE4 = CGRB - Compare Grande Register and Branch
*
* operand1 is 64-bit signed binary integer
* operand2 is 64-bit signed binary integer
* R0 is usable as operand1 and functions normally
* R0 is usable as operand2 and functions normally
*
* CC remains unchanged
*
***********************************************************************
CGRB     LARL  R12,*
         USING CGRB,R12                 * Set Base
* Compare positive with positive
         LGHI  R4,64
         CGR   R4,R4                    * Set CC=0
         LGHI  R5,80
         LGHI  R6,48
         CGRBL R4,R6,CGRB_ERR           * EC46C02240E4 should not jump
         CGRBL R4,R5,CGRB_1             * EC45C02440E4 should jump
CGRB_ERR DC    X'0000'
* Compare positive with negative
CGRB_1   LGHI  R5,-64
         CGRBL R4,R5,CGRB_ERR           * EC45C02240E4 should not jump
         CGRBH R4,R5,CGRB_2             * EC45C03620E4 should jump
         DC    X'0000'
* Compare negative with negative
CGRB_2   LGHI  R5,-8
         LGHI  R8,-16
         LGHI  R9,-4
         CGRBL R5,R8,CGRB_ERR           * EC58C02240E4 should not jump
         CGRBH R5,R9,CGRB_ERR           * EC59C02220E4 should not jump
* Compare negative with positive
         LGHI  R9,10
         CGRBH R5,R9,CGRB_ERR           * EC59C02220E4 should not jump
         CGRBL R5,R9,CGRB_3             * EC59C06040E4 should jump
         DC    X'0000'
* Compare equal
CGRB_3   LGR   R6,R5
         CGRBE R5,R6,CGRB_4             * EC56C06C80E4 should jump
         DC    X'0000'
* Try with R0
CGRB_4   LGHI  R0,64
         LGHI  R4,48
         LGHI  R5,80
         CGRBL R0,R4,CGRB_5             * EC04C08640E4 should not jump
         CGRBL R0,R5,CGRB_5             * EC05C08640E4 should jump
         DC    X'0000'
CGRB_5   EQU   *
         CGRBH R4,R0,CGRB_6             * EC40C09420E4 should not jump
         CGRBH R5,R0,CGRB_6             * EC50C09420E4 should jump
         DC    X'0000'
CGRB_6   EQU   *

***********************************************************************
*
* ECE5 = CLGRB - Compare Logical Grande Register and Branch
*
* operand1 is 64-bit unsigned binary integer
* operand2 is 64-bit unsigned binary integer
* R0 is usable as operand1 and functions normally
* R0 is usable as operand2 and functions normally
*
* CC remains unchanged
*
***********************************************************************
CLGRB    LARL   R12,*
         USING  CLGRB,R12               * Set Base
* Compare positive with positive
         LGHI   R4,64
         CGR    R4,R4                   * Set CC=0
         LGHI   R5,80
         LGHI   R6,48
         CLGRBL R4,R6,CLGRB_ERR         * EC46C02240E5 should not jump
         CLGRBL R4,R5,CLGRB_1           * EC45C02440E5 should jump
CLGRB_ERR DC    X'0000'
* Compare positive with negative
CLGRB_1  LGHI   R5,-64
         CLGRBH R4,R5,CLGRB_ERR         * EC45C02220E5 should not jump
         CLGRBL R4,R5,CLGRB_2           * EC45C03640E5 should jump
         DC     X'0000'
* Compare negative with negative
CLGRB_2  LGHI   R5,-8
         LGHI   R8,-16
         LGHI   R9,-4
         CLGRBH R5,R9,CLGRB_ERR         * EC59C02220E5 should not jump
         CLGRBL R5,R8,CLGRB_ERR         * EC58C02240E5 should not jump
* Compare negative with positive
         LGHI   R9,10
         CLGRBL R5,R9,CLGRB_ERR         * EC59C02240E5 should not jump
         CLGRBH R5,R9,CLGRB_3           * EC59X06020E5 should jump
         DC     X'0000'
* Compare equal
CLGRB_3  LGR    R6,R5
         CLGRBE R5,R6,CLGRB_4           * EC56C06C80E5 should jump
         DC     X'0000'
* Try with R0
CLGRB_4  LGHI   R0,64
         LGHI   R4,48
         LGHI   R5,80
         CLGRBL R0,R4,CLGRB_5           * EC04C08640E5 should not jump
         CLGRBL R0,R5,CLGRB_5           * EC05C08640E5 should jump
         DC     X'0000'
CLGRB_5  EQU    *
         CLGRBH R4,R0,CLGRB_6           * EC40C09420E5 should not jump
         CLGRBH R5,R0,CLGRB_6           * EC50C09420E5 should jump
         DC     X'0000'
CLGRB_6  EQU    *

***********************************************************************
*
* ECF6 = CRB - Compare Register and Branch
*
* operand1 is 32-bit signed binary integer
* operand2 is 32-bit signed binary integer
* R0 is usable as operand1 and functions normally
* R0 is usable as operand2 and functions normally
*
* CC remains unchanged
*
***********************************************************************
CRB      LARL  R12,*
         USING CRB,R12                  * Set Base
* Compare positive with positive
         LHI   R4,64
         CR    R4,R4                    * Set CC=0
         LHI   R5,80
         LHI   R6,48
         CRBL  R4,R6,CRB_ERR            * EC46C02040F6 should not jump
         CRBL  R4,R5,CRB_1              * EC45C02240F6 should jump
CRB_ERR  DC    X'0000'
* Compare positive with negative
CRB_1    LHI   R5,-64
         CRBL  R4,R5,CRB_ERR            * EC45C02040F6 should not jump
         CRBH  R4,R5,CRB_2              * EC45C03420F6 should jump
         DC    X'0000'
* Compare negative with negative
CRB_2    LHI   R5,-8
         LHI   R8,-16
         LHI   R9,-4
         CRBL  R5,R8,CRB_ERR            * EC58C02040F6 should not jump
         CRBH  R5,R9,CRB_ERR            * EC59C02020F6 should not jump
* Compare negative with positive
         LHI   R9,10
         CRBH  R5,R9,CRB_ERR            * EC59C02020F6 should not jump
         CRBL  R5,R9,CRB_3              * EC59C05E40F6 should jump
         DC    X'0000'
* Compare equal
CRB_3    LR    R6,R5
         CRBE  R5,R6,CRB_4              * EC56C06880F6 should jump
         DC    X'0000'
* Try with R0
CRB_4    LHI   R0,64
         LHI   R4,48
         LHI   R5,80
         CRBL  R0,R4,CRB_5              * EC04C08240F6 should not jump
         CRBL  R0,R5,CRB_5              * EC05C08240F6 should jump
         DC    X'0000'
CRB_5    EQU   *
         CRBH  R4,R0,CRB_6              * EC40C09020F6 should not jump
         CRBH  R5,R0,CRB_6              * EC50C09020F6 should jump
         DC    X'0000'
CRB_6    EQU   *

***********************************************************************
*
* ECF7 = CLRB - Compare Logical Register and Branch
*
* operand1 is 32-bit unsigned binary integer
* operand2 is 32-bit unsigned binary integer
* R0 is usable as operand1 and functions normally
* R0 is usable as operand2 and functions normally
*
* CC remains unchanged
*
***********************************************************************
CLRB     LARL  R12,*
         USING CLRB,R12                 * Set Base
* Compare positive with positive
         LGHI  R4,64
         CGR   R4,R4                    * Set CC=0
         LGHI  R5,80
         LGHI  R6,48
         CLRBL R4,R6,CLRB_ERR           * EC46C02240F7 should not jump
         CLRBL R4,R5,CLRB_1             * EC45C02440F7 should jump
CLRB_ERR DC    X'0000'
* Compare positive with negative
CLRB_1   LGHI  R5,-64
         CLRBH R4,R5,CLRB_ERR           * EC45C02220F7 should not jump
         CLRBL R4,R5,CLRB_2             * EC45C03640F7 should jump
         DC    X'0000'
* Compare negative with negative
CLRB_2   LGHI  R5,-8
         LGHI  R8,-16
         LGHI  R9,-4
         CLRBH R5,R9,CLRB_ERR           * EC59C02220F7 should not jump
         CLRBL R5,R8,CLRB_ERR           * EC58C02240F7 should not jump
* Compare negative with positive
         LGHI  R9,10
         CLRBL R5,R9,CLRB_ERR           * EC59C02240F7 should not jump
         CLRBH R5,R9,CLRB_3             * EC59C06020F7 should jump
         DC    X'0000'
* Compare equal
CLRB_3   LGR   R6,R5
         CLRBE R5,R6,CLRB_4             * EC56C06C80F7 should jump
         DC    X'0000'
* Try with R0
CLRB_4   LGHI  R0,64
         LGHI  R4,48
         LGHI  R5,80
         CLRBL R0,R4,CLRB_5             * EC04C08640F7 should not jump
         CLRBL R0,R5,CLRB_5             * EC05C08640F7 should jump
         DC    X'0000'
CLRB_5   EQU   *
         CLRBH R4,R0,CLRB_6             * EC40C09420F7 should not jump
         CLRBH R5,R0,CLRB_6             * EC50C09420F7 should jump
         DC    X'0000'
CLRB_6   EQU   *

***********************************************************************
*
* ECFC = CGIB - Compare Grande Immediate and Branch
*
* operand1 is 64-bit signed binary integer
* operand2 is  8-bit signed binary integer
* R0 is usable as operand1 and functions normally
*
* CC remains unchanged
*
***********************************************************************
CGIB     LARL  R12,*
         USING CGIB,R12                 * Set Base
* Compare positive with positive
         LGHI  R4,64
         CGR   R4,R4                    * Set CC=0
         CGIBL R4,48,CGIB_ERR           * EC44C01A30FC should not jump
         CGIBL R4,80,CGIB_1             * EC44C01C50FC should jump
CGIB_ERR DC    X'0000'
* Compare positive with negative
CGIB_1   CGIBL R4,-64,CGIB_ERR          * EC44C01AC0FC should not jump
         CGIBH R4,-64,CGIB_2            * EC42C02AC0FC should jump
         DC    X'0000'
* Compare negative with negative
CGIB_2   LGHI  R5,-8
         CGIBL R5,-16,CGIB_ERR          * EC54C01AF0FC should not jump
         CGIBH R5,-4,CGIB_ERR           * EC52C01AFCFC should not jump
* Compare negative with positive
         CGIBH R5,10,CGIB_ERR           * EC52C01A0AFC should not jump
         CGIBL R5,10,CGIB_3             * EC54C0480AFC should jump
         DC    X'0000'
* Compare equal
CGIB_3   CGIBE R5,-8,CGIB_4             * EC58C050F8FC should jump
         DC    X'0000'
* Try with R0
CGIB_4   LGHI  R0,64
         CGIBL R0,48,CGIB_5             * EC04C06230FC should not jump
         CGIBL R0,80,CGIB_5             * EC04C06250FC should jump
         DC    X'0000'
CGIB_5   EQU   *

***********************************************************************
*
* ECFD = CLGIB - Compare Logical Grande Immediate and Branch
*
* operand1 is 64-bit unsigned binary integer
* operand2 is  8-bit unsigned binary integer
* R0 is usable as operand1 and functions normally
*
* CC remains unchanged
*
***********************************************************************
CLGIB     LARL   R12,*
          USING  CLGIB,R12              * Set Base
* Compare positive with positive
          LGHI   R4,64
          CR     R4,R4                  * Set CC=0
          CLGIBL R4,48,CLGIB_ERR        * EC44C01830FD should not jump
          CLGIBL R4,80,CLGIB_1          * EC44C01A50FD should jump
CLGIB_ERR DC     X'0000'
* Compare positive with negative
CLGIB_1   CLGIBH R4,-64,CLGIB_ERR       * EC42C018C0FD should not jump
          CLGIBL R4,-64,CLGIB_2         * EC44C028C0FD should jump
          DC     X'0000'
* Compare negative with negative
CLGIB_2   LGHI   R5,-8
* RPI 1540: arith: -8 < -4 But logically: X'FFFFFFF8' > X'000000FC' 
          CLGIBL R5,-4,CLGIB_ERR        * EC54C018FCFD should not jump
****      CLGIBH R5,-4,CLGIB_ERR        * Fixed: CLGIBL does branch
          CLGIBL R5,-16,CLGIB_ERR       * EC54C018F0FD should not jump
* Compare negative with positive
          CLGIBL R5,10,CLGIB_ERR        * EC54C0180AFD should not jump
          CLGIBH R5,10,CLGIB_3          * EC52C0460AFD should jump
          DC     X'0000'
* Compare equal
CLGIB_3   CLGIBE R5,-8,CLGIB_ERR        * EC58C018F8FD should not jump
          LGHI   R5,25
          CLGIBE R5,25,CLGIB_4          * EC58C05819FD should jump
          DC     X'0000'
* Try with R0
CLGIB_4   LGHI   R0,64
          CLGIBL R0,48,CLGIB_5          * EC04C06A30FD should not jump
          CLGIBL R0,80,CLGIB_5          * EC04C06A50FD should jump
          DC     X'0000'
CLGIB_5   EQU    *

***********************************************************************
*
* ECFE = CIB - Compare Immediate and Branch
*
* operand1 is 32-bit signed binary integer
* operand2 is  8-bit signed binary integer
* R0 is usable as operand1 and functions normally
*
* CC remains unchanged
*
***********************************************************************
CIB      LARL  R12,*
         USING CIB,R12                  * Set Base
* Compare positive with positive
         LG    R4,=X'00FF00FF00000040'  * X'40' = 64
         CR    R4,R4                    * Set CC=0
         CIBL  R4,48,CIB_ERR            * EC44C01830FE should not jump
         CIBL  R4,80,CIB_1              * EC44C01A50FE should jump
CIB_ERR  DC    X'0000'
* Compare positive with negative
CIB_1    CIBL  R4,-64,CIB_ERR           * EC44C018C0FE should not jump
         CIBH  R4,-64,CIB_2             * EC42C028C0FE should jump
         DC    X'0000'
* Compare negative with negative
CIB_2    LHI   R5,-8
         CIBL  R5,-16,CIB_ERR           * EC54C018F0FE should not jump
         CIBH  R5,-4,CIB_ERR            * EC52C018FCFE should not jump
* Compare negative with positive
         CIBH  R5,10,CIB_ERR            * EC52C0180AFE should not jump
         CIBL  R5,10,CIB_3              * EC54C0460AFE should jump
         DC    X'0000'
* Compare equal
CIB_3    CIBE  R5,-8,CIB_4              * EC58C04EF8FE should jump
         DC    X'0000'
* Try with R0
CIB_4    LHI   R0,64
         CIBL  R0,48,CIB_5              * EC04C06030FE should not jump
         CIBL  R0,80,CIB_5              * EC04C06050FE should jump
         DC    X'0000'
CIB_5    EQU   *

***********************************************************************
*
* ECFF = CLIB - Compare Logical Immediate and Branch
*
* operand1 is 32-bit unsigned binary integer
* operand2 is  8-bit unsigned binary integer
* R0 is usable as operand1 and functions normally
*
* CC remains unchanged
*
***********************************************************************
CLIB     LARL  R12,*
         USING CLIB,R12                 * Set Base
* Compare positive with positive
         LG    R4,=X'00FF00FF00000040'  * X'40' = 64
         CR    R4,R4                    * Set CC=0
         CLIBL R4,48,CLIB_ERR           * EC44C01A30FF should not jump
         CLIBL R4,80,CLIB_1             * EC44C01C50FF should jump
CLIB_ERR DC    X'0000'
* Compare positive with negative
CLIB_1   CLIBH R4,-64,CLIB_ERR          * EC42C01AC0FF should not jump
         CLIBL R4,-64,CLIB_2            * EC44C02AC0FF should jump
         DC    X'0000'
* Compare negative with negative
CLIB_2   LGHI  R5,-8
* RPI 1540: arith: -8 < -4 But logically: X'FFFFFFF8' > X'000000FC' 
         CLIBL R5,-4,CLIB_ERR           * EC54C01AFCFF should not jump
****     CLIBH R5,-4,CLIB_ERR           * Fixed: CLGIBL does branch
         CLIBL R5,-16,CLIB_ERR          * EC54C01AF0FF should not jump
* Compare negative with positive
         CLIBL R5,10,CLIB_ERR           * EC54C01A0AFF should not jump
         CLIBH R5,10,CLIB_3             * EC52C0480AFF should jump
         DC    X'0000'
* Compare equal
CLIB_3   CLIBE R5,-8,CLIB_ERR           * EC58C01AF8FF should jump
         LGHI  R5,25
         CLIBE R5,25,CLIB_4             * EC58C05A19FF should jump
         DC    X'0000'
* Try with R0
CLIB_4   LGHI  R0,64
         CLIBL R0,48,CLIB_5             * EC04C06C30FF should not jump
         CLIBL R0,80,CLIB_5             * EC04C06C50FF should jump
         DC    X'0000'
CLIB_5   EQU   *

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
