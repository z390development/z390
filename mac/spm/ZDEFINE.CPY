.*********************************************************************
.* Copyright 2007 Automated Software Tools Corporation               *
.* This source code is part of z390 assembler/emulator package       *
.* The z390 package is distributed under GNU general public license  *
.* Author - Don Higgins                                              *
.* Date   - 04/19/07                                                 *
.*********************************************************************
.* 09/02/07 RPI 681 DEFINE GLOBALS FOR DEFINE AND ZDEFINE MACROS     
.* 09/23/07 RPI 704 ADD CONTROL INTERVAL SIZE FOR LINEAR SUPPORT     
.*********************************************************************
.*
.* DEFINE GLOBAL DATA FOR Z390 VSAM CATALOG DEFINITION TABLE (VCDT)
.*
         GBLC  &CATALOG          CATALOG NAME
         GBLB  &ERROR            SET IF ANY DEFINE ERRORS
.*
.* CLUSTER GLOBAL DATA          
.*
         GBLA  &CLR_TOT          TOTAL CLR BASE CLUSTERS DEFINED
         GBLC  &CLR_NAME(100)    NAMES OF BASE CLUSTERS
         GBLC  &CLR_TYPE(100)    TYPE ESDS, RRDS, KSDS, OR LDS
         GBLA  &CLR_CI(100)      OPTIONAL CI SIZE (DEFAULT 0)
         GBLB  &CLR_VREC(100)    VARIABLE LENGTH RECORDS (ELSE FIXED)
         GBLB  &CLR_REUSE(100)   ERASE ALL DATA FOR REUSE AT OPEN
         GBLB  &CLR_KSDS(100)    KSDS KEY SEQUENCED
         GBLB  &CLR_RRDS(100)    RRDS RELATIVE RECORD 
         GBLB  &CLR_ESDS(100)    ESDS ENTRY SEQUENCED
         GBLB  &CLR_LDS(100)     LDS  LINEAR 
         GBLA  &CLR_LAVG(100)    AVERAGE RECORD LENGTH 
         GBLA  &CLR_LMAX(100)    MAX OR FIXED RECORD LENGTH
         GBLA  &CLR_KLEN(100)    KSDS PRIMARY KEY LENGTH
         GBLA  &CLR_KOFF(100)    KSDS PRIMARY KEY OFFSET
         GBLC  &CLR_VES(100)     OPTIONAL VES DSNAME DEFAULT NAME.VES
         GBLC  &CLR_VX0(100)     OPTIONAL VX0 DSNAME DEFAULT NAME.VX0
         GBLA  &CLR_AIX_TOT(100) TOTAL AIX UPGRADES FOR THIS CLUSTER
.*
.* AIX GLOBAL DATA                                   
.*
         GBLA  &AIX_TOT          TOTAL AIX INDEXES DEFINED
         GBLC  &AIX_NAME(100)    NAME OF KSDS ALTERNATE INDEX
         GBLC  &AIX_RELATE(100)  NAME OF RELATED BASE CLUSTER
         GBLB  &AIX_REUSE(100)   ERASE ASLL DATA FOR AIX AT OPEN
         GBLB  &AIX_UNIQUEKEY(100) INFORCE UNIQUE AIX KEYS
         GBLB  &AIX_UPGRADE(100) UPDATE AIX ON BASE CHANGES
         GBLA  &AIX_KLEN(100)    AIX KEY LENGTH IN RELATED BASE CLUSTER
         GBLA  &AIX_KOFF(100)    AIX KEY OFFSET IN RELATED BASE CLUSTER
         GBLC  &AIX_VXN(100)     OPTIONAL VXN DSNAME DEFAULT NAME.VXN
.*
.* PATH GLOBAL DATA
.*
         GBLA  &PTH_TOT          TOTAL PATHS DEFINED
         GBLC  &PTH_NAME(100)    NAME OF ALTERNATE PATH
         GBLC  &PTH_ENTRY(100)   NAME OF AIX OR BASE CLUSTER 
         GBLB  &PTH_AIXP(100)    USE AIX PATH ENTRY VS PRIMARY KEY
         GBLB  &PTH_AIXU(100)    UPDATE AIX'S ON BASE CHANGES
.*
.* END OF ZDEFINE.CPY
.*
