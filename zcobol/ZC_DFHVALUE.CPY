.*********************************************************************
.* Copyright 2008 Automated Software Tools Corporation               *
.* This source code is part of z390 zCOBOL/assembler/emulator package*
.* The z390 package is distributed under GNU general public license  *
.* Author - Don Higgins                                              *
.* Date   - 04/07/08                                                 *
.*********************************************************************
.* 06/14/09 RPI 1057 sync with AZ390 DFHVALUE support
.*********************************************************************
               GBLC &(DFHVALUE_NOTAPPLIC) 
               GBLC &(DFHVALUE_VSAM) 
               GBLC &(DFHVALUE_ESDS) 
               GBLC &(DFHVALUE_KSDS) 
               GBLC &(DFHVALUE_RRDS) 
               GBLC &(DFHVALUE_BASE) 
               GBLC &(DFHVALUE_PATH) 
               GBLC &(DFHVALUE_FIXED) 
               GBLC &(DFHVALUE_VARIABLE) 
               GBLC &(DFHVALUE_OPEN) 
               GBLC &(DFHVALUE_CLOSED)
               GBLC &(DFHVALUE_ENABLED)
               GBLC &(DFHVALUE_DISABLED)
               GBLC &(DFHVALUE_UNENABLED)
               GBLC &(DFHVALUE_READABLE)
               GBLC &(DFHVALUE_NOTREADABLE)
               GBLC &(DFHVALUE_UPDATABLE)
               GBLC &(DFHVALUE_NOTUPDATABLE)
               GBLC &(DFHVALUE_BROWSABLE)
               GBLC &(DFHVALUE_NOTBROWSABLE)
               GBLC &(DFHVALUE_ADDABLE)   
               GBLC &(DFHVALUE_NOTADDABLE)
               GBLC &(DFHVALUE_DELETABLE)
               GBLC &(DFHVALUE_NOTDELETABLE)
               GBLC &(DFHVALUE_VRRDS)     
               :&(DFHVALUE_NOTAPPLIC)     SETC   '1'   
               :&(DFHVALUE_VSAM)          SETC   '2'   
               :&(DFHVALUE_ESDS)          SETC   '5'   
               :&(DFHVALUE_KSDS)          SETC   '6'   
               :&(DFHVALUE_RRDS)          SETC   '7'   
               :&(DFHVALUE_BASE)          SETC   '10'  
               :&(DFHVALUE_PATH)          SETC   '11'  
               :&(DFHVALUE_FIXED)         SETC   '12'  
               :&(DFHVALUE_VARIABLE)      SETC   '13'  
               :&(DFHVALUE_OPEN)          SETC   '18'  
               :&(DFHVALUE_CLOSED)        SETC   '19'  
               :&(DFHVALUE_ENABLED)       SETC   '23'  
               :&(DFHVALUE_DISABLED)      SETC   '24'  
               :&(DFHVALUE_UNENABLED)     SETC   '33'  
               :&(DFHVALUE_READABLE)      SETC   '35'  
               :&(DFHVALUE_NOTREADABLE)   SETC   '36'  
               :&(DFHVALUE_UPDATABLE)     SETC   '37'  
               :&(DFHVALUE_NOTUPDATABLE)  SETC   '38'  
               :&(DFHVALUE_BROWSABLE)     SETC   '39'  
               :&(DFHVALUE_CNOTBROWSABLE) SETC   '40'  
               :&(DFHVALUE_ADDABLE)       SETC   '41'  
               :&(DFHVALUE_NOTADDABLE)    SETC   '42'  
               :&(DFHVALUE_DELETABLE)     SETC   '43'  
               :&(DFHVALUE_NOTDELETABLE)  SETC   '44'  
               :&(DFHVALUE_VRRDS)         SETC   '732' 
