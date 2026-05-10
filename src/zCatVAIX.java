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



/**
 * The zCatVAIX object is the Java-level representation of a Catalog definition
 * for a zVSAM Alternate IndeX
 */
public class zCatVAIX
{/* *************************************************** Maintenance
  * 2016-12-28 initial coding 
  * 2026-03-23 AFK Fix/Add javadoc comments
  *****************************************************/
    /** ID C'VAIX'                               */ private String  VAIXID;   // DS    CL4   ID C'VAIX'
    /** NAME OF ALTERNATE INDEX                  */ private String  VAIXNAME; // DS    CL8   NAME OF ALTERNATE INDEX
    /** NAME OF RELATED VCLR BASE CLUSTER        */ private String  VAIXRELN; // DS    CL8   NAME OF RELATED VCLR BASE CLUSTER
                                                    //              VAIXFLAG; // DS    0F    OPTION FLAGS
                                                    //              VAIXFLG1; // DS    X     OPTION FLAG 1
    /** REUSE OPTION RESETS EOF TO 0 AT ACB OPEN */ private boolean VAIXRUSE; // EQU   X'80' REUSE OPTION RESETS EOF TO 0 AT ACB OPEN
    /** UNIQUE KSDS KEYS                         */ private boolean VAIXUKEY; // EQU   X'40' UNIQUE KSDS KEYS
    /** UPGRADE AIX ON BASE CLUSTER CHANGES      */ private boolean VAIXUAIX; // EQU   X'20' UPGRADE AIX ON BASE CLUSTER CHANGES
    /** SPANNED                                  */ private boolean VAIXSPAN; // EQU   X'10' SPANNED
                                                    //              VAIXFLG2; // DS    X     OPTION FLAG 2
    /** OPTION FLAG 2                            */ private boolean VAIXDADJ; // EQU   X'80' DATAADJUST=YES
    /** INDEXADJUST=YES                          */ private boolean VAIXIADJ; // EQU   X'40' INDEXADJUST=YES
                                                    //              VAIXFLG3; // DS    X     OPTION FLAG 3
                                                    //              VAIXFLG4; // DS    X     OPTION FLAG 4
    /** KSDS AIX KEY LENGTH                      */ private int     VAIXKLEN; // DS    F     KSDS AIX KEY LENGTH
    /** KSDS AIX KEY OFFSET                      */ private int     VAIXKOFF; // DS    F     KSDS AIX KEY OFFSET
    /** OVERRIDE DTA DSNAME                      */ private String  VAIXDTA;  // DS    A     OVERRIDE DTA DSNAME
    /** OVERRIDE IDX DSNAME                      */ private String  VAIXIDX;  // DS    A     OVERRIDE IDX DSNAME
                                                    //              VAIXRELA; // DS    A     ADDR RELATED VCLR BASE CLUSTER
    /** DATA BLOCKSIZE IN BYTES                  */ private int     VAIXDBS;  // DS    F     DATA BLOCKSIZE IN BYTES
    /** INDEX BLOCKSIZE IN BYTES                 */ private int     VAIXIBS;  // DS    F     INDEX BLOCKSIZE IN BYTES
    /** DATA CA PERCENT                          */ private int     VAIXDCA;  // DS    AL1   DATA CA PERCENT
    /** DATA CI PERCENT                          */ private int     VAIXDCI;  // DS    AL1   DATA CI PERCENT
    /** INDEX CA PERCENT                         */ private int     VAIXICA;  // DS    AL1   INDEX CA PERCENT
    /** INDEX CI PERCENT                         */ private int     VAIXICI;  // DS    AL1   INDEX CI PERCENT

/**
 * Constructor for new AIX definition
 *
 * @param sz390 instance of svc-handling code
 * @param VAIX_ptr pointer to VAIX
 */
  public zCatVAIX(sz390 sz390, int VAIX_ptr)
   {VAIXID   = sz390.get_ascii_string(VAIX_ptr + 0, 4, true);
    VAIXNAME = null;
    VAIXRELN = null;
    VAIXRUSE = false;
    VAIXUKEY = false;
    VAIXUAIX = false;
    VAIXSPAN = false;
    VAIXDADJ = false;
    VAIXIADJ = false;
    VAIXKLEN = 0;
    VAIXKOFF = 0;
    VAIXDTA  = null;
    VAIXIDX  = null;
    VAIXDBS  = 0;
    VAIXIBS  = 0;
    VAIXDCA  = 0;
    VAIXDCI  = 0;
    VAIXICA  = 0;
    VAIXICI  = 0;
    System.out.println("zCatVAIX constructed");
    }

 /* end of module zCatVAIX */
 }