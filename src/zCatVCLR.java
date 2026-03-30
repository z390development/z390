/*
z390 - Mainframe assembler emulator and run-time engine
Copyright (C) 2021 z390 Assembler LLC

This file is part of z390.
z390 is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

z390 is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License along
with this program; if not, see <https://www.gnu.org/licenses/>.
*/
import java.util.LinkedList;



/**
 * The zCatVCLR object is the Java-level representation of a Catalog definition
 *              for a zVSAM base cluster
 */
public class zCatVCLR
{/* *************************************************** Maintenance
  * 2016-12-28 initial coding 
  * 2026-03-23 AFK Fix/Add javadoc comments
  *****************************************************/
  /** ID C'VCLR'                                    */ private String   VCLRID;   // DS    CL4   ID C'VCLR'
  /** NAME OF BASE CLUSTER                          */ private String   VCLRNAME; // DS    CL8   NAME OF BASE CLUSTER
  /** TYPE OF BASE CLUSTER (ESDS/RRDS/KSDS/LDS)     */ private String   VCLRTYPE; // DS    CL4   TYPE OF BASE CLUSTER (ESDS/RRDS/KSDS/LDS)
                                                       //               VCLRFLAG; // DS    0XL4  OPTION FLAGS
                                                       //               VCLRFLG1; // DS    X     VCDT OPTION BYTE 1
  /** RECFM=V VARIABLE LENGTH VS FIXED              */ private boolean  VCLRVREC; // EQU   X'80' RECFM=V VARIABLE LENGTH VS FIXED
  /** REUSE OPTION RESETS EOF TO 0 AT ACB OPEN      */ private boolean  VCLRRUSE; // EQU   X'40' REUSE OPTION RESETS EOF TO 0 AT ACB OPEN
  /** SPANNED                                       */ private boolean  VCLRSPAN; // EQU   X'10' SPANNED
  /** KSDS KEY SEQUENCED                            */ private boolean  VCLRKSDS; // EQU   X'08' KSDS KEY SEQUENCED
  /** RRDS RELATIVE RECORD                          */ private boolean  VCLRRRDS; // EQU   X'04' RRDS RELATIVE RECORD
  /** ESDS ENTRY SEQUENCED                          */ private boolean  VCLRESDS; // EQU   X'02' ESDS ENTRY SEQUENCED
  /** LDS  LINEAR CI SEQUENCE                       */ private boolean  VCLRLDS;  // EQU   X'01' LDS  LINEAR CI SEQUENCE
                                                       //               VCLRFLG2; // DS    X     VCDT OPTION BYTE 2
  /** DATAADJUST=YES                                */ private boolean  VCLRDADJ; // EQU   X'80' DATAADJUST=YES
  /** INDEXADJUST=YES                               */ private boolean  VCLRIADJ; // EQU   X'40' INDEXADJUST=YES
                                                       //               VCLRFLG3; // DS    X     VCDT OPTION BYTE 3
                                                       //               VCLRFLG4; // DS    X     VCDT OPTION BYTE 4
  /** AVG RECORD LENGTH FOR VREC                    */ private int      VCLRLAVG; // DS    F     AVG RECORD LENGTH FOR VREC
  /** MAX RECORD LENGTH (EXCLUDES 4 BYTE RDW)       */ private int      VCLRLMAX; // DS    F     MAX RECORD LENGTH (EXCLUDES 4 BYTE RDW)
  /** KSDS PRIMARY KEY LENGTH                       */ private int      VCLRKLEN; // DS    F     KSDS PRIMARY KEY LENGTH
  /** KSDS PRIMARY KEY OFFSET                       */ private int      VCLRKOFF; // DS    F     KSDS PRIMARY KEY OFFSET
  /** OVERRIDE DTA DSNAME                           */ private String   VCLRDTA;  // DS    A     OVERRIDE DTA DSNAME
  /** OVERRIDE IDX DSNAME                           */ private String   VCLRIDX;  // DS    A     OVERRIDE IDX DSNAME
  /** DATA BLOCKSIZE IN BYTES                       */ private int      VCLRDBS;  // DS    F     DATA BLOCKSIZE IN BYTES
  /** INDEX BLOCKSIZE IN BYTES                      */ private int      VCLRIBS;  // DS    F     INDEX BLOCKSIZE IN BYTES
  /** DATA CA PERCENT                               */ private int      VCLRDCA;  // DS    X     DATA CA PERCENT
  /** DATA CI PERCENT                               */ private int      VCLRDCI;  // DS    X     DATA CI PERCENT
  /** INDEX CA PERCENT                              */ private int      VCLRICA;  // DS    X     INDEX CA PERCENT
  /** INDEX CI PERCENT                              */ private int      VCLRICI;  // DS    X     INDEX CI PERCENT
  /** NUMBER OF AIX'S WITH UPGRADE FOR THIS CLUSTER */ private int      VCLRAIXN; // DS    F     NUMBER OF AIX'S WITH UPGRADE FOR THIS CLUSTER
  /** AIX UPGRADE TABLE OF VAIX ADDRESSES           */ private LinkedList<zCatVAIX> VCLRAIX_list;
                                                                                  // DS    A     ADDR AIX UPGRADE TABLE OF VAIX ADDRESSES

  /**
   * Constructor for new Base Cluster definition
   *
   * @param sz390 svc-handler instance
   * @param tz390 table data instance
   * @param VCLR_ptr pointer to VCLR
   */
  public zCatVCLR(sz390 sz390
                , tz390 tz390
                , int VCLR_ptr
                  )
   {VCLRID   = sz390.get_ascii_string(VCLR_ptr +  0, 4, true);
               if (tz390.opt_tracev)
                  {//tz390.put_trace("VCLRID  =" + VCLRID);
                   System.out.println("VCLRID  =" + VCLRID);
                   }
    VCLRNAME = sz390.get_ascii_string(VCLR_ptr +  4, 8, true);
               if (tz390.opt_tracev)
                  {//tz390.put_trace("VCLRNAME=" + VCLRNAME);
                   System.out.println("VCLRNAME=" + VCLRNAME);
                   }
    VCLRTYPE = sz390.get_ascii_string(VCLR_ptr + 12, 4, true);
               if (tz390.opt_tracev)
                  {//tz390.put_trace("VCLRTYPE=" + VCLRTYPE);
                   System.out.println("VCLRTYPE=" + VCLRTYPE);
                   }
    VCLRVREC = false;
    VCLRRUSE = false;
    VCLRSPAN = false;
    VCLRKSDS = false;
    VCLRRRDS = false;
    VCLRESDS = false;
    VCLRLDS  = false;
    VCLRDADJ = false;
    VCLRIADJ = false;
    VCLRLAVG = 0;
    VCLRLMAX = 0;
    VCLRKLEN = 0;
    VCLRKOFF = 0;
    VCLRDTA  = null;
    VCLRIDX  = null;
    VCLRDBS  = 0;
    VCLRIBS  = 0;
    VCLRDCA  = 0;
    VCLRDCI  = 0;
    VCLRICA  = 0;
    VCLRICI  = 0;
    VCLRAIXN = 0;
    VCLRAIX_list = null;
    System.out.println("zCatVCLR constructed");
    }

 /* end of module zCatVCLR */
 }