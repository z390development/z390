.*********************************************************************
.* Copyright 2009 Automated Software Tools Corporation               *
.* This source code is part of z390 zCOBOL/assembler/emulator package*
.* The z390 package is distributed under GNU general public license  *
.* Author - Don Higgins                                              *
.* Date   - 07/08/09                                                 *
.*********************************************************************
.* 07/08/09 RPI 1065 replace EQUREGS, ZSTREQUS with ZC_??? to avoid 
.*                   conflicts with COBOL data names like R1 or Z.
.*********************************************************************
         PUSH  PRINT,NOPRINT
         PRINT OFF,NOPRINT
.*                 GENERAL PURPOSE 64 BIT REGISTERS
ZC_R0    EQU   0
ZC_R1    EQU   1
ZC_R2    EQU   2
ZC_R3    EQU   3
ZC_R4    EQU   4
ZC_R5    EQU   5
ZC_R6    EQU   6
ZC_R7    EQU   7
ZC_R8    EQU   8
ZC_R9    EQU   9
ZC_R10   EQU  10
ZC_R11   EQU  11
ZC_R12   EQU  12
ZC_R13   EQU  13
ZC_R14   EQU  14
ZC_R15   EQU  15
.*                 FLOATING POINT 64 BIT REGISTERS (HFP,BFP,DFP)
ZC_F0    EQU   0
ZC_F1    EQU   1
ZC_F2    EQU   2
ZC_F3    EQU   3
ZC_F4    EQU   4
ZC_F5    EQU   5
ZC_F6    EQU   6
ZC_F7    EQU   7
ZC_F8    EQU   8
ZC_F9    EQU   9
ZC_F10   EQU  10
ZC_F11   EQU  11
ZC_F12   EQU  12
ZC_F13   EQU  13
ZC_F14   EQU  14
ZC_F15   EQU  15
.*                 CONDITIONAL BRANCH MASKS                        
ZC_H     EQU   2   HIGH                                                    
ZC_L     EQU   4   LOW                                                     
ZC_E     EQU   8   EQUAL                                                   
ZC_NH    EQU   13  NOT HIGHT                                               
ZC_NL    EQU   11  NOT LOW                                                 
ZC_NE    EQU   7   NOT EQUAL                                               
ZC_O     EQU   1   OVERFLOW                                                
ZC_P     EQU   2   POSITIVE                                                
ZC_M     EQU   4   MINUS                                                   
ZC_Z     EQU   8   ZERO                                                    
ZC_NP    EQU   13  NOT POSITIVE                                            
ZC_NM    EQU   11  NOT MINUS                                               
ZC_NZ    EQU   7   NOT ZERO                                                
ZC_NO    EQU   14  NOT OVERFLOW                                            
ZC_EQ    EQU   8   EQUAL                                                   
ZC_GT    EQU   2   GREATER THAN                                            
ZC_LT    EQU   4   LESS    THAN                                            
ZC_GE    EQU   11  GREATER THAN OR EQUAL                                   
ZC_LE    EQU   13  LESS    THAN OR EQUAL
         POP   PRINT,NOPRINT
.*
.* END OF ZC_EQUS.CPY
.*
