         MACRO                                                           
.*********************************************************************
.* Copyright 2007 Automated Software Tools Corporation               *
.* This source code is part of z390 assembler/emulator package       *
.* The z390 package is distributed under GNU general public license  *
.* Author - Don Higgins                                              *
.* Date   - 04/12/07                                                 *
.*********************************************************************
.* 04/12/07 RPI 590 COPIED FROM FPC SHARE SPLA FILE 178 CONTRIBUTION
.*          TO SUPPORT MULTIPLE BASES AND RENT OPTIONS
.* 04/26/07 RPI 601 CHANGE BASE OFFSETS FROM 4095 TO STD 4096        
.* 07/11/07 RPI 654 CLEANUP - ADD DROP, CHECK RENT+R13, RWA+RWALNG
.* 08/21/07 RPI 670 CORRECT REG OPTIMIZATION TO HANDLE ANY REG SYMBOL
.* 10/01/07 RPI 709 FIX MISSING QUOTE IN AIF AFTER .NOSTK2           
.* 02/17/12 RPI 1189 REMOVE DROP 13 WHEN NOT USING 13
.*********************************************************************
.*             MACRO-ID. 'SUBENTRY'                                      
.*             AUTHOR.   DON HIGGINS.                                    
.*             DATE WRITTEN.  07/28/70.                                  
.*             REMARKS.  THIS MACRO GENERATES THE CODE TO ESTABLISH A    
.*                       CSECT WITH SAVE AREA AND BASE REGISTER USING    
.*                       STANDARD OS LINKAGE CONVENTIONS.                
.*             REV1 11/14/74 ADD REENTRANT OPTION WITH WORK AREA         
.*                 1.  FOR REENTRANT LINKAGE YOU MUST SPECIFY            
.*                     A.  RENT=YES                                      
.*                     B.  AT LEAST ONE BASE REGISTER OTHER THAN 13      
.*                         SUCH AS BASES=(R12)                           
.*                 2.  OPTIONALLY YOU CAN SPECIFY A DSECT NAME AND       
.*                     LENGTH FOR A WORK AREA BUILT BEHIND SAVE AREA.    
.*                     FOR EXAMPLE  RWA=MYWORK,RWALNG=8             
.*             REV2 02/10/76 DSH - ADD SUBPOOL OPTION                    
.*             REV3 XX/XX/77 FDB - ADD STACK OPTION TO SUPPORT           
.*                                 REENTRANT PERFORM, PENTRY, PEXIT      
.*                                                                       
.*                                 STACK=  SETS MAXIMUM DEPTH            
.*                                         OF PERFORMS                   
.*                                                                       
.*             REV4 01/04/78 DSH - ADD OPTIONS TO OPTIMIZE STACK CODE    
.*                                 AND PROVIDE EXTENDED BASE REGISTERS   
.*                                                                       
.*                                 PSTACK= DEFINES PERMANENT REGISTER    
.*                                         FOR STACK TO SAVE LOADS       
.*                                         AND STORES                    
.*                                                                       
.*                                 PBASE=YES CAUSES BASE REGISTER        
.*                                           DEFINED BY BASES= TO        
.*                                           BE STACKED AND A NEW        
.*                                           BASE ESTABLISHED FOR        
.*                                           EACH PERFORMED ROUTINE      
.*                                                                       
.*                                 PSAVE=NO  CAUSES R14 AND R15          
.*                                           TO NOT BE SAVED ACROSS      
.*                                           PERFORMS TO SAVE LOADS      
.*                                           AND STORES                  
.*                                                                       
.*                                 PCHECK=NO ELIMINATES CODE TO          
.*                                           CHECK FOR STACK             
.*                                           OVERFLOW/UNDERFLOW          
.*                                                                       
.*                                                                       
.*                                                                       
.*********************************************************************
&NAME    SUBENTRY &CSECT=YES,&BASES=(13),                              X 
               &RENT=NO,&RWA=,&RWALNG=0,&SP=0,&STACK=0,                X 
               &PSTACK=0,&PBASE=NO,&PSAVE=YES,&PCHECK=YES                
         GBLC  &PSTKREG,&PBASREG,&PBASLAB                                
         GBLB  &PSTKSW,&PBASESW,&PSAVESW,&PCHKSW                         
         GBLA  &STKSIZE,&STKENT                                          
         GBLB  &STKOPT,&RENTOPT                                          
         LCLA  &NBASE,&S,&OFFSET                                         
&PSTKSW  SETB  0                                                         
&PBASESW SETB  0                                                         
&PSAVESW SETB  0                                                         
&PCHKSW  SETB  0                                                         
&STKOPT  SETB  0                                                         
&RENTOPT SETB  0                                                         
         AIF   ('&CSECT'(1,1) EQ 'N').NOCSECT                            
&NAME    CSECT                                                           
         AGO   .STM                                                      
.NOCSECT ANOP                                                            
&NAME    DS    0D                                                        
.STM     ANOP                                                            
         AIF   ('&RENT'(1,1) EQ 'N').NORENT                              
&RENTOPT SETB  1                                                         
         AIF   ('&BASES(1)' EQ 'R13').ERR2  RPI 654
         AIF   (&BASES(1) EQ 13).ERR2     RPI 654
         STM   14,12,12(13)                                              
         BAL   &BASES(1),40(15)                                          
         USING &NAME+8,&BASES(1)                                         
         DC    CL8'&NAME'                                                
         DC    CL8'SESA'                                                 
         DC    CL8'&SYSDATE'                                             
         DC    CL8'&SYSTIME'                                             
         AIF   ('&STACK' EQ '0').NOSTK1                                  
&STKOPT  SETB  1                                                         
&STKENT  SETA  4                                                         
         AIF   ('&PBASE'(1,1) EQ 'N').STK1                               
&PBASESW SETB  1                                                         
&STKENT  SETA  8                                                         
&PBASREG SETC  '&BASES(1)'                                               
&PBASLAB SETC  '&NAME+8'                                                 
.STK1    ANOP                                                            
&STKSIZE SETA  20+&STKENT*&STACK+4                                       
.NOSTK1  ANOP                                                            
         LA    0,80+&RWALNG+&STKSIZE                                     
         AIF   ('&SP' EQ '0').SKIPSP                                     
         LA    1,&SP                                                     
         SLL   1,24                                                      
         OR    0,1                                                       
.SKIPSP  ANOP                                                            
         LR    2,0                                                       
         GETMAIN R,LV=(0)                                                
.*                                                                       
.*       SAVE AREA, STACK, AND WORK AREA LAYOUT                          
.*                                                                       
.*       -8     4  'SESA'                                                
.*       -4     4  SUB POOL AND TOTAL LENGTH FOR SUBEXIT FREEMAIN        
.*        0    72  STANDARD SAVE AREA                                    
.*       72     4  R14 SAVE AREA DURING PERFORM IF PSAVE=YES             
.*       76     4  R15 SAVE AREA DURING PERFORM IF PSAVE=YES             
.*       80     4  STACK POINTER SAVE AREA IF NO PSTACK DEFINED          
.*       84     4  LENGTH OF STACK ENTRY (4 OR 8 IF PBASE DEFINED)       
.*       88     4  LOW STACK ENTRY WITH HIGH BIT ON FOR CHECKING         
.*       92     X  STACK OF LENGTH DEFINED BY STACK=                     
.*       92+X   4  HIGH STACK ENTRY WITH HIGH BIT ON FOR CHECKING        
.*       96+X   Y  WORK AREA DEFINED BY RWA= AND RWALNG=                 
.*                                                                       
.*                                                                       
         MVC   0(4,1),8(&BASES(1))                                       
         ST    2,4(,1)                                                   
         LA    1,8(,1)                                                   
         AIF   ('&STACK' EQ '0').NOSTK2                                  
         AIF   ('&PSAVE'(1,1) EQ 'N').L1                                 
.*                                          IF SAVE REQUIRED             
.*                                             SET SWITCH                
&PSAVESW SETB  1                                                         
.L1      AIF   ('&PCHECK'(1,1) EQ 'N').L2                                
.*                                          IF CHECKING REQUIRED         
.*                                             SET SWITCH                
&PCHKSW  SETB  1                                                         
         XC    88(8+&STKENT*&STACK,1),88(1) CLEAR STACK AREA             
         MVI   88(1),X'80'                  SET LOW BAD BIT              
         MVI   92+&STKENT*&STACK.(1),X'80'  SET HIGH BAD BIT             
.L2      ANOP                                                            
         LA    15,&STKENT                                                
         ST    15,84(1)                     SET ENTRY LENGTH             
         AIF   ('&PSTACK' NE '0').L3                                     
.*                                          IF NO STACK REGISTER         
         LA    15,92(1)                                                  
         ST    15,80(1)                       SAVE STACK POINTER         
         AGO   .L4                                                       
.*                                          ELSE                         
.L3      ANOP                                                            
&PSTKSW  SETB  1                               SET PSTACK SWITCH         
&PSTKREG SETC  '&PSTACK'                       SAVE REG FOR MACROS       
         LA    &PSTKREG,92(1)                  LOAD STACK REGISTER       
.L4      ANOP                                                            
.NOSTK2  ANOP                                                            
         ST    1,8(13)                                                   
         ST    13,4(1)                                                   
         LM    14,2,12(13)                                               
         L     13,8(13)                                                  
         AIF   ('&RWALNG' EQ '0' AND '&RWA' EQ '').GEN   RPI 654
         AIF   ('&RWALNG' EQ '0' OR  '&RWA' EQ '').ERR1  RPI 654,709
         USING &RWA-(72+&STKSIZE),13                                     
         AGO   .GEN                                                      
.NORENT  ANOP                                                            
         STM   14,12,12(13)                                              
         BAL   15,104(15)                                                
         DC    18F'0'                                                    
         DC    CL8'&NAME'                                                
         DC    CL8'&SYSDATE'                                             
         DC    CL8'&SYSTIME'                                             
         ST    15,8(13)                                                  
         ST    13,4(15)                                                  
         LR    13,15                                                     
         AIF   (T'&BASES(1) NE 'N').CHKR13                               
         AIF   (&BASES(1) EQ 13).GENB13
         AGO   .GENBX                                                    
.CHKR13  ANOP                                                            
         AIF   ('&BASES(1)' EQ 'R13').GENB13      RPI 654
.GENBX   ANOP
         ZOPTLR &BASES(1),13             
.SKIPR13 ANOP               
.*       DROP  13                               RPI 654, RPI 1189        
         USING &NAME+8,&BASES(1)
         AGO   .GEN                                                      
.GENB13  ANOP                                                            
         USING &NAME+8,13                                                
.GEN     ANOP                                                            
&NBASE   SETA  N'&BASES                                                  
&S       SETA  1                                                         
.LOOP    ANOP                                                            
&S       SETA  &S+1                                                      
         AIF   (&S GT &NBASE).EXIT                                       
&OFFSET  SETA  &OFFSET+4096                    RPI 601                          
         LAY   &BASES(&S),4096(&BASES(&S-1))   RPI 601                          
         USING &NAME+8+&OFFSET,&BASES(&S)
         AGO   .LOOP 
.EXIT    ANOP                                                            
         MEXIT
.ERR1    MNOTE 'SUBENTRY PARM ERROR RWA= REQUIRES RWALNG= ALSO'
         MEXIT
.ERR2    MNOTE 'SUBENTRY PARM ERROR RENT=YES REQUIRES BASE(1) NE 13'
         MEXIT
         MEND                                                            

