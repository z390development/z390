.*********************************************************************
.* Copyright 2009 Automated Software Tools Corporation               *
.* This source code is part of z390 zCOBOL/assembler/emulator package*
.* The z390 package is distributed under GNU general public license  *
.* Author - Don Higgins                                              *
.* Date   - 08/15/09                                                 *
.*********************************************************************
.* 08/15/09 RPI 106 global keywords to identifed as not vars or lits        
.*********************************************************************
         GBLB &ZCKW_EQUAL
         GBLB &ZCKW_GREATER
         GBLB &ZCKW_LESS
         GBLB &ZCKW_THAN
         GBLB &ZCKW_TO
&ZCKW_EQUAL   SETB 1
&ZCKW_GREATER SETB 1
&ZCKW_LESS    SETB 1
&ZCKW_THAN    SETB 1
&ZCKW_TO      SETB 1
