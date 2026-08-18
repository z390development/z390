/* -------------------------------------------------------------------------- *\
 |                                                                            |
 |  Definition of a zVSAM Access Control Block                                |
 |                                                                            |
 |  The Access Control Block (or ACB for short) represents a zVSAM cluster    |
 |      in the memory of the emualator                                        |
 |                                                                            |
 |  Supported operations are Open, Close, ShowCB, TestCB                      |
 |                                                                            |
\* -------------------------------------------------------------------------- */

/* ***************************************************
 * 
 * z390 portable mainframe assembler and emulator.
 * 
 * Copyright 2011 Automated Software Tools Corporation
 * 
 * z390 is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 * 
 * z390 is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with z390; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 * 
 * zACB is part of the zVSAM v2 component of z390.
 * It is the java implementation of the ACB with its associated operations
 * and thus represents a single cluster.
 *
 ****************************************************
 * Maintenance
 ****************************************************
 * 2019/2020  Initial construction by Hugh Sweeney
 * 2021-04-03 AFK Include copy of ACB-before-open
 *                Change zFCB pointer to an ArrayList
 *                Various small changes throughout
 * 
 ****************************************************/

import java.util.ArrayList;
import java.util.regex.*;

/**
 *  Shadow ACB
 *
 *  This class shadows a real ACB that resides in Z memory and provides all
 *  services that operate on an ACB.
 *
 <p>
 *  It is keyed on ACB address (z390 does not support multiple address spaces)
 *
 *
 *  @author   Hugh Sweeney
 *  @version  2.0
 *  @see      "zVSAM Design and Logic"
 *
 */
public class vzACB extends zControlBlock
   {private static final String  pattDDN = "^[A-Z\u0023\u0024\u0040][A-Z0-9\u0023\u0024\u0040]{0,7}$"; // 1*alpha/national, 0-7*(alphameric|national)
    private static final Pattern PattDDN = Pattern.compile(pattDDN); 
    
    private final byte[]         original_ACB;  // Saved copy of ACB before open
    
    private ArrayList<vzFCB>     vzFCB_list;    // One zFCB for each physical file
    private int                  vzFCB_count;   // Nr of zFCB objects on the list
    private boolean              vzACB_open;    // zACB status flag
    private String               vzACB_DDname;  // DDname from ACB converted to ascii
    private String               path_n_name;   // path and name of file to be opened

    private zException           my_zException; // Current exception status

    /**
     *  Offsets of various fields in user`s zACB. This is information about ACB internals.
     *  These offsets should be used only in getter functions, above.
     *  No information about ACB internal layout should be defined outside this class.
     */
    private static final short ACBID    = 0x0004;
    private static final short ACBSTYPE = 0x0005;
    private static final short ACBLEN   = 0x0006;
    private static final short ACBDDNAM = 0x0008;
    private static final short ACBMACRF = 0x0010;
    private static final short ACBMACR1 = 0x0010;
    private static final short ACBMACR2 = 0x0011;
    private static final short ACBMACR3 = 0x0012;
    private static final short ACBMACR4 = 0x0013;
    private static final short ACBPASSW = 0x001E;
    private static final short ACBEXLST = 0x0022;
    private static final short ACBINFLG = 0x0026;
    private static final short ACBINFL1 = 0x0026;
    private static final short ACBINFL2 = 0x0027;
    private static final short ACBOFLGS = 0x0028;
    private static final short ACBERFLG = 0x0029;
    private static final short ACBBSTNO = 0x002A;
    private static final short ACBSTRNO = 0x002B;
    private static final short ACBSHRP  = 0x002C;
    private static final short ACBVER   = 0x002D;
    private static final short ACBPFX   = 0x002E;
    private static final short ACBXPFX  = 0x0032;
    private static final short ACBBUFD  = 0x0036;
    private static final short ACBBUFI  = 0x003A;
    private static final short ACBEND   = 0x0040;

    /**
     *  Values of equated symbols in ACB DSECT.
     *
     */
    private static final byte ACBIDVAL  = (byte) 0xA0; // ACBID:    VSAM
    private static final byte ACBSVSAM  = (byte) 0x10; // ACBSTYPE: VSAM
    private static final byte ACBV2     = (byte) 0x02; // ACBVER:   Version: 2
    private static final byte ACBKEY    = (byte) 0x80; // ACBMACR1: Indexed access by logical  
    private static final byte ACBADR    = (byte) 0x40; // ACBMACR1: Non-indexed access by address
    private static final byte ACBCNV    = (byte) 0x20; // ACBMACR1: CI access (not supported)
    private static final byte ACBSEQ    = (byte) 0x10; // ACBMACR1: Sequential access
    private static final byte ACBDIR    = (byte) 0x08; // ACBMACR1: Direct access
    private static final byte ACBIN     = (byte) 0x04; // ACBMACR1: Get allowed
    private static final byte ACBOUT    = (byte) 0x02; // ACBMACR1: Get, Put, Erase allowed
    private static final byte ACBUBF    = (byte) 0x01; // ACBMACR1: User Buffering (not supported)
    private static final byte ACBSKP    = (byte) 0x10; // ACBMACR2: Skip sequential access
    private static final byte ACBLOGON  = (byte) 0x08; // ACBMACR2: No logon required (not supported)
    private static final byte ACBRST    = (byte) 0x04; // ACBMACR2: Reusable dataset (not supported)
    private static final byte ACBDSN    = (byte) 0x02; // ACBMACR2: Subtask sharing (not supported)
    private static final byte ACBAIX    = (byte) 0x01; // ACBMACR2: Access through AIX
    private static final byte ACBNLW    = (byte) 0x80; // ACBMACR3: No LSR exc. ctrl wt (not supported)
    private static final byte ACBLSR    = (byte) 0x40; // ACBMACR3: Local Shared Resources (not supported)
    private static final byte ACBGSR    = (byte) 0x20; // ACBMACR3: Global Shared Resources (not supported)
    private static final byte ACBICI    = (byte) 0x10; // ACBMACR3: Improved CI processing (not supported)
    private static final byte ACBDFR    = (byte) 0x08; // ACBMACR3: Deferred Writes allowed
    private static final byte ACBSIS    = (byte) 0x04; // ACBMACR3: Sequential Insert Strategy
    private static final byte ACBCFX    = (byte) 0x02; // ACBMACR3: Fix Control Blocks and Buffers (not supported)
    private static final byte ACBMODE   = (byte) 0x01; // ACBMACR3: Buffers allowed above the line
    private static final byte ACBRLS    = (byte) 0x80; // ACBMACR4: Record Level Sharing
    private static final byte ACBCAT    = (byte) 0x10; // ACBINFL1: ACB is for a catalog (not supported)
    private static final byte ACBSWARN  = (byte) 0x80; // ACBINFL2: Suppress Open Warnings
    private static final byte ACBSHR02  = (byte) 0x02; // ACBINFL2: Share options 02
    private static final byte ACBSHR01  = (byte) 0x01; // ACBINFL2: Share options 01
    private static final byte ACBR31B   = (byte) 0x80; // ACBOFLGS: 31-BIT ADDRESSING FOR BUFFERS
    private static final byte ACBR31C   = (byte) 0x40; // ACBOFLGS: 31-BIT ADDRESSING FOR CONTROL BLKS
    private static final byte ACBEOV    = (byte) 0x20; // ACBOFLGS: EOV CONCATENATION
    private static final byte ACBOPEN   = (byte) 0x10; // ACBOFLGS: ACB CURRENTLY OPEN
    private static final byte ACBDSERR  = (byte) 0x08; // ACBOFLGS: ERROR-ACB MUST BE CLOSED
    private static final byte ACBRECOV  = (byte) 0x04; // ACBOFLGS: OPEN FOR RECOVERY
    private static final byte ACBEXFG   = (byte) 0x02; // ACBOFLGS: OFF WHEN USER EXIT IN PROGRESS
    private static final byte ACBIOSFG  = (byte) 0x01; // ACBOFLGS: OPEN/CLOSE IN PROGRESS




     /**
     *
     * Constructor, stores the pointer to user's ACB, and validates it.
     *
     * Parameters: byte[]     holding the z390 memory array
     *             int        holding the offset of the ACB in the memory array
     *             int        specifying the size of the underlying byte array
     *
     */
   vzACB(byte[] parm_memBuff, int parm_addr, int parm_size)
       {super(parm_memBuff, parm_addr, ACBEND);
        
        // Ensure the input is a valid ACB
        if (parm_addr < 0x2000)
           throw new zException("ACB must never be in low storage", 8, 8);
        if (parm_addr + ACBEND - 1 > parm_size)
           throw new zException("ACB area is too small", 8, 8);
        
        // Allocate dependent fields and refresh all object data
        my_zException = new zException();
        original_ACB  = new byte[ACBEND];      // Room for ACB - save before open, restore after close
        vzFCB_list    = new ArrayList<vzFCB>(); // each file opened will add an entry
        vzFCB_count   = 0;
        vzACB_open    = false;
        refresh();
        }




    /**
     *
     * Refresh ACB object from underlying array data
     *
     * Parameters: none
     *
     */
    public void refresh()
       {// Retrieve all ACB fields from the input buffer        
        try
           {if (id()    != ACBIDVAL) throw new zException("vz390.V2.zACB: Invalid ACBID",    8, 0); 
            if (stype() != ACBSVSAM) throw new zException("vz390.V2.zACB: Invalid ACBSTYPE", 8, 0); 
            if (ver()   != ACBV2)    throw new zException("vz390.V2.zACB: Invalid ACBVER",   8, 0); 
            System.arraycopy(this.array(), this.get_offset(), original_ACB, 0, ACBEND);
            }
        // ****************************************************************************
        // zACB was not created so we cannot set ACBerflg to hold reasoncode
        // do not issue message - that's done by our caller: handle_vsam_request
        catch(zException ze)
           {my_zException.activate(ze.getMessage(), ze.getReturnCode(), ze.getReasonCode());
            }
        catch(Exception e)
           {my_zException.activate(e.getMessage(), 8, 255);
            }
        finally
           {if (my_zException.isActive())
                throw my_zException; // How to get zException.MessageText into the encapsulated RuntimeException message?
            }
        }




    /**
    *  Open this ACB.
    *  @return      return code (for R15)
    *
    * Actions taken by this routine:
    *  1. Validate ACB has not yet been opened
    *  2. Save content of caller-supplied ACB (done by constructor)
    *  3. Get DDname from ACB, error if invalid (implemented in constructor)
    *  4. Retrieve environment variable holding path and filename
    *  5. Create FCB, start FCB chain off zACB
    *  6. Open file, populate FCB;error if open fails
    *  7. Allocate PFX buffer, chain buffer off FCB
    *  8. Read PFX into buffer
    *  9. Validate Block (8 validations)
    * 10. Validate PFX (42 validations)
    *
    */
    int open()
       {vzFCB TempFCB;
        
        //  1. Validate ACB has not yet been opened
        if ((oflgs() & ACBOPEN) == ACBOPEN)
            throw new zException("vzACB.open: ACB already open", 8, 0); 
        if (vzACB_open)
            throw new zException("vzACB.open: zACB already open", 8, 0); 
         // Mark open in progress
        set_boolean(ACBOFLGS, ACBIOSFG, true);
        
        // Additional ACB checks
        if (len() != ACBEND)
            throw new zException("vzACB.open: ACB length incorrect", 8, 0); 
        if ((macr1() & ACBCNV) == ACBCNV)
            throw new zException("vzACB.open: CI access not supported", 8, 0); 
        if ((macr1() & ACBUBF) == ACBUBF)
            throw new zException("vzACB.open: User buffering not supported", 8, 0); 
        if ((macr2() & ACBLOGON) == ACBLOGON)
            throw new zException("vzACB.open: No-logon processing not supported", 8, 0); 
        if ((macr2() & ACBRST) == ACBRST)
            throw new zException("vzACB.open: Re-usable datasets not supported", 8, 0); 
        if ((macr2() & ACBDSN) == ACBDSN)
            throw new zException("vzACB.open: Subtask sharing not supported", 8, 0); 
        if (((int)macr2() & ~ACBSKP & ~ACBLOGON & ~ACBRST & ~ACBDSN & ~ACBAIX) != 0)
            throw new zException("vzACB.open: Unassigned bits in ACBMACR2 not zero", 8, 0); 
        if ((macr3() & ACBNLW) == ACBNLW)
            throw new zException("vzACB.open: NLW processing not supported", 8, 0); 
        if ((macr3() & ACBLSR) == ACBLSR)
            throw new zException("vzACB.open: Local Shared Resources not supported", 8, 0); 
        if ((macr3() & ACBGSR) == ACBGSR)
            throw new zException("vzACB.open: Global Shared Resources not supported", 8, 0); 
        if ((macr3() & ACBICI) == ACBICI)
            throw new zException("vzACB.open: Improved CI processing not supported", 8, 0); 
        if ((macr3() & ACBCFX) == ACBCFX)
            throw new zException("vzACB.open: Fixed control blocks and buffers not supported", 8, 0); 
        if ((macr4() & ACBRLS) == ACBRLS)
            throw new zException("vzACB.open: Record Level Sharing not supported", 8, 0); 
        if (((int)macr4() & ~ACBRLS) != 0)
            throw new zException("vzACB.open: Unassigned bits in ACBMACR4 not zero", 8, 0); 
        if (passw() != 0 && passw() < 4096)
            throw new zException("vzACB.open: Password in page 0 not supported", 8, 0); 
        if (exlst() != 0)
            throw new zException("vzACB.open: User exit list not supported", 8, 0); 
        if ((infl1() & ACBCAT) == ACBCAT)
            throw new zException("vzACB.open: zVSAM catalogs not supported", 8, 0); 
        if (((int)infl1() & ~ACBCAT) != 0)
            throw new zException("vzACB.open: Unassigned bits in ACBINFL1 not zero", 8, 0); 
        if ((infl2() & ACBSHR02) == ACBSHR02)
            throw new zException("vzACB.open: Share options 02 not supported", 8, 0); 
        if ((infl2() & ACBSHR01) == ACBSHR01)
            throw new zException("vzACB.open: Share options 01 not supported", 8, 0); 
        if (((int)infl2() & ~ACBSWARN & ~ACBSHR02 & ~ACBSHR01) != 0)
            throw new zException("vzACB.open: Unassigned bits in ACBINFL2 not zero", 8, 0); 
        if (oflgs() != 0)
            throw new zException("vzACB.open: All bits in ACBOFLGS must be zero", 8, 0); 
        
        //  3. Get DDname from ACB, error if invalid
        try
           {vzACB_DDname = ddnam();
            }
        catch (Exception e)
           {throw new zException("vzACB.open: DDNAME not valid EBCDIC", 8, 0); 
            }
        if (vzACB_DDname.isEmpty())
            throw new zException("vzACB.open: DDNAME is null", 8, 0); 
        Matcher Pattern_match = PattDDN.matcher(vzACB_DDname);
        if (!Pattern_match.matches())
            throw new zException("vzACB.open: DDNAME format invalid", 8, 0); 
        
        //  4. Retrieve environment variable holding path and filename
        try
           {path_n_name = System.getenv(vzACB_DDname);
            }
        catch (Exception e)
           {throw new zException("vzACB.open: Missing environment variable " + vzACB_DDname, 8, 0); 
            }
        if (path_n_name.isEmpty())
           {throw new zException("vzACB.open: Empty environment variable " + vzACB_DDname, 8, 0); 
            }
        
        //  5. Create FCB, start FCB chain off vzACB
        try
           {TempFCB = new vzFCB(this, vzFCB_count + 1, path_n_name);
            vzFCB_list.add(TempFCB);
            vzFCB_count++;
            }
        catch (zException e)
           {throw new zException(e.getMessage(), e.getReturnCode(), e.getReasonCode()); 
            }
        
        //  6. Open file, populate FCB; error if open fails
        try
           {vzFCB_list.get(vzFCB_count).open();
            }
        catch (zException e)
           {throw new zException(e.getMessage(), e.getReturnCode(), e.getReasonCode()); 
            }
        vzACB_open = true; 
        
        // Retrieve catalog info









        set_boolean(ACBOFLGS, ACBOPEN , true ); // mark open complete
        set_boolean(ACBOFLGS, ACBIOSFG, false); // mark open no longer in progress
        
        return 16;    //!! Because the routine is not complete yet
        }




    /*
    *  Get functions for each of the ACB fields.
    *
    *  These are the only methods that should be available to other classes to access ACB fields.
    */

    /** ACB address in z memory */
    int     Get_ACB_addr() {return get_offset();}

    /** DDNAME (UTF-8) */
    String ddnam()
       {String ddname;
        try
           {ddname = new String(original_ACB, ACBDDNAM, 8, "cp1047");
            }
        catch (java.io.UnsupportedEncodingException e)
           {throw new zException("!! can't convert DDNAM from EBCDIC cp1047: " + e, 8, 8);
            }
        // Trim trailing spaces - only if first character is not a space
        if (!ddname.isEmpty())
           {if (ddname.charAt(0) != ' ')
               {ddname = ddname.trim();
                }
            }
        return ddname;
        }

    /** Error field (reason code) */
    byte    erflg   () {return get_byte(ACBERFLG);}

    /** ID field: X'A0' indicates VSAM */
    byte    id      () {return get_byte(ACBID);}

    /** ACB length */
    short   len     () {return get_byte(ACBLEN);}

    /** Macro access type flags byte 1 */
    byte    macr1   () {return get_byte(ACBMACR1);}
    boolean acbkey  () {return get_boolean(ACBMACR1, ACBKEY  );}
    boolean acbadr  () {return get_boolean(ACBMACR1, ACBADR  );}
    boolean acbcnv  () {return get_boolean(ACBMACR1, ACBCNV  );}
    boolean acbseq  () {return get_boolean(ACBMACR1, ACBSEQ  );}
    boolean acbdir  () {return get_boolean(ACBMACR1, ACBDIR  );}
    boolean acbin   () {return get_boolean(ACBMACR1, ACBIN   );}
    boolean acbout  () {return get_boolean(ACBMACR1, ACBOUT  );}
    boolean acbubf  () {return get_boolean(ACBMACR1, ACBUBF  );}

    /** Macro access type flags byte 2 */
    byte    macr2   () {return get_byte(ACBMACR2);}
    boolean acbskp  () {return get_boolean(ACBMACR2, ACBSKP  );}
    boolean acblogon() {return get_boolean(ACBMACR2, ACBLOGON);}
    boolean acbrst  () {return get_boolean(ACBMACR2, ACBRST  );}
    boolean acbdsn  () {return get_boolean(ACBMACR2, ACBDSN  );}
    boolean acbaix  () {return get_boolean(ACBMACR2, ACBAIX  );}

    /** Macro access type flags byte 3 */
    byte    macr3   () {return get_byte(ACBMACR3);}
    boolean acbnlw  () {return get_boolean(ACBMACR3, ACBNLW  );}
    boolean acblsr  () {return get_boolean(ACBMACR3, ACBLSR  );}
    boolean acbgsr  () {return get_boolean(ACBMACR3, ACBGSR  );}
    boolean acbici  () {return get_boolean(ACBMACR3, ACBICI  );}
    boolean acbdfr  () {return get_boolean(ACBMACR3, ACBDFR  );}
    boolean acbsis  () {return get_boolean(ACBMACR3, ACBSIS  );}
    boolean acbcfx  () {return get_boolean(ACBMACR3, ACBCFX  );}
    boolean acbmode () {return get_boolean(ACBMACR3, ACBMODE );}

    /** Macro access type flags byte 4 */
    byte    macr4   () {return get_byte(ACBMACR4);}
    boolean acbrls  () {return get_boolean(ACBMACR4, ACBRLS  );}

    /** Password Pointer */
    int     passw   () {return get_int(ACBPASSW);}

    /** Exit List Pointer */
    int     exlst   () {return get_int(ACBEXLST);}

    /** Input flags byte 1 */
    byte    infl1   () {return get_byte(ACBINFL1);}
    boolean acbcat  () {return get_boolean(ACBINFL1, ACBCAT  );}

    /** Input flags byte 2 */
    byte    infl2   () {return get_byte(ACBINFL2);}
    boolean acbswarn() {return get_boolean(ACBINFL2, ACBSWARN);}
    boolean acbshr02() {return get_boolean(ACBINFL2, ACBSHR02);}
    boolean acbshr01() {return get_boolean(ACBINFL2, ACBSHR01);}

    /** Output flags */
    byte    oflgs()   {return get_byte(ACBOFLGS);}
    boolean acbr31b () {return get_boolean(ACBOFLGS, ACBR31B );}
    boolean acbr31c () {return get_boolean(ACBOFLGS, ACBR31C );}
    boolean acbeov  () {return get_boolean(ACBOFLGS, ACBEOV  );}
    boolean acbopen () {return get_boolean(ACBOFLGS, ACBOPEN );}
    boolean acbdserr() {return get_boolean(ACBOFLGS, ACBDSERR);}
    boolean acbrecov() {return get_boolean(ACBOFLGS, ACBRECOV);}
    boolean acbexfg () {return get_boolean(ACBOFLGS, ACBEXFG );}
    boolean acbiosfg() {return get_boolean(ACBOFLGS, ACBIOSFG);}

    /** Subtype indicator field: 10 indicates VSAM */
    byte    stype   () {return get_byte(ACBSTYPE);}

    /** Version indicator: should be 02 (Version 2) */
    byte    ver     () {return get_byte(ACBVER);}




    /**
    *  Set reasoncode in this ACB's erflg field
    *  @return      return code (for R15)
    */
    void Set_ACBerflg(byte rsn)
       {set_byte(ACBERFLG, rsn);
        return;
        }




    // The above data was extracted from:
    // 16:11:03 ACBD2     MZ390 START USING z390 V1.6.00b12 ON J2SE 1.8.0_111 06/22/20
    // AZ390I Copyright 2011 Automated Software Tools Corporation
    // AZ390I z390 is licensed under GNU General Public License
    // AZ390I program = /home/hsweeney/handover/2/test/ACBD2
    // AZ390I options = 
    //   sysmac(/media/CRUZER3/Dev/zVSAM/MACVSAM2_MM+.) 
    //   sysmac(+/media/CRUZER3/Dev/z/mac) syscpy(/media/CRUZER3/Dev/z/mac+.)
    // External Symbol Definitions
    // Assembler Listing
    // 000000                                        (1/1)1          ACBD  ,
    // 000000                                       (2/32)2+IHAACB   DSECT 
    // 000000                  00000000             (2/33)3+IFGACB   EQU   IHAACB
    // 000000                                       (2/34)4+ACBEYE   DS    CL4                EYECATCHER
    // 000004                  A9C1C3C2             (2/35)5+ACBZACB  EQU   C'zACB'               EYECATCHER VALUE
    // 000004                                       (2/36)6+ACBID    DS    X                  IDENTIFIER
    // 000005                  000000A0             (2/37)7+ACBIDVAL EQU   X'A0'                 VSAM
    // 000005                                       (2/38)8+ACBSTYP  DS    X                  SUBTYPE
    // 000006                  00000005             (2/39)9+ACBSTYPE EQU   ACBSTYP
    // 000006                  00000005            (2/40)10+ACBTYPE  EQU   ACBSTYP
    // 000006                  00000010            (2/41)11+ACBSVSAM EQU   X'10'                 VSAM SUBTYPE
    // 000006                                      (2/42)12+ACBLENG  DS    XL2                ACB LENGTH
    // 000008                  00000006            (2/43)13+ACBLENG2 EQU   ACBLENG
    // 000008                  00000006            (2/44)14+ACBLEN   EQU   ACBLENG
    // 000008                  00000006            (2/45)15+ACBLEN2  EQU   ACBLENG
    // 000008                                      (2/46)16+ACBDDNM  DS    CL8                DDNAME > SET > PATH\CAT.CLUSTER
    // 000010                  00000008            (2/47)17+ACBDDNAM EQU   ACBDDNM
    // 000010                                      (2/48)18+ACBMACRF DS    0XL2               MACRO ACCESS TYPE FLAGS 1 & 2
    // 000010                                      (2/49)19+ACBMACR1 DS    B                  MACRF FLAGS BYTE 1
    // 000011                  00000080            (2/50)20+ACBKEY   EQU   X'80'                 INDEXED ACCESS BY LOGICAL KEY
    // 000011                  00000080            (2/51)21+ACBMACR1_KEY EQU ACBKEY
    // 000011                  00000040            (2/52)22+ACBADR   EQU   X'40'                 NON-INDEXED ACCESS BY ADDRESS
    // 000011                  00000040            (2/53)23+ACBADD   EQU   ACBADR
    // 000011                  00000040            (2/54)24+ACBMACR1_ADR EQU ACBADR
    // 000011                                      (2/55)25+* ACBMACR1_CNV EQU X'20'             CI ACCESS (NOT SUPPORTED)
    // 000011                  00000010            (2/56)26+ACBSEQ   EQU   X'10'                 SEQUENTIAL ACCESS
    // 000011                  00000010            (2/57)27+ACBMACR1_SEQ EQU ACBSEQ
    // 000011                  00000008            (2/58)28+ACBDIR   EQU   X'08'                 DIRECT ACCESS
    // 000011                  00000008            (2/59)29+ACBMACR1_DIR EQU ACBDIR
    // 000011                  00000004            (2/60)30+ACBIN    EQU   X'04'                 GET ALLOWED
    // 000011                  00000004            (2/61)31+ACBGET   EQU   ACBIN
    // 000011                  00000004            (2/62)32+ACBMACR1_IN  EQU ACBIN
    // 000011                  00000002            (2/63)33+ACBOUT   EQU   X'02'                 GET, PUT, ERASE ALLOWED
    // 000011                  00000002            (2/64)34+ACBPUT   EQU   ACBOUT
    // 000011                  00000002            (2/65)35+ACBMACR1_OUT EQU ACBOUT
    // 000011                                      (2/66)36+* ACBMACR1_UBF EQU X'01'             USER BUFFER MGT (NOT SUPPORTED)
    // 000011                                      (2/67)37+*
    // 000011                                      (2/68)38+ACBMACR2 DS    B                  MACRF FLAGS BYTE 2
    // 000012                  00000010            (2/69)39+ACBSKP   EQU   X'10'                 SKIP SEQUENTIAL ACCESS
    // 000012                  00000010            (2/70)40+ACBMACR2_SKP EQU ACBSKP
    // 000012                                      (2/71)41+* ACBMACR2_NLOGON EQU X'08'          NO LOGON REQUIRED (NOT SUPPORTED)
    // 000012                                      (2/72)42+* ACBRST   EQU   X'04'               DATASET REUSABLE (NOT SUPPORTED)
    // 000012                                      (2/73)43+* ACBMACR2_RST EQU ACBRST
    // 000012                                      (2/74)44+* ACBMACR2_DSN EQU X'10'             SUBTASK SHARING (NOT SUPPORTED)
    // 000012                  00000001            (2/75)45+ACBAIX   EQU   X'01'                 ACCESS THROUGH AIX
    // 000012                  00000001            (2/76)46+ACBAIXP  EQU   ACBAIX
    // 000012                  00000001            (2/77)47+ACBMACR2_AIX EQU ACBAIX
    // 000012                                      (2/78)48+*
    // 000012                                      (2/79)49+ACBMACR3 DS    B                  MACRF FLAGS BYTE 3
    // 000013                                      (2/80)50+* ACBNLW   EQU   X'80'               NO LSR EXC. CTRL WT (NOT SUPP)
    // 000013                  00000040            (2/81)51+ACBLSR   EQU   X'40'                 LOCAL SHARED RESOURCES
    // 000013                  00000040            (2/82)52+ACBMACR3_LSR EQU ACBLSR
    // 000013                  00000020            (2/83)53+ACBGSR   EQU   X'20'                 GLOBAL SHARED RESOURCES
    // 000013                  00000020            (2/84)54+ACBMACR3_GSR EQU ACBGSR
    // 000013                                      (2/85)55+* ACBMACR2_ICI EQU X'10'             IMPROVED CI PROC. (NOT ALLOWED)
    // 000013                  00000008            (2/86)56+ACBDFR   EQU   X'08'                 DEFERRED WRITES ALLOWED
    // 000013                  00000008            (2/87)57+ACBMACR3_DFR EQU ACBDFR
    // 000013                  00000004            (2/88)58+ACBSIS   EQU   X'04'                 SEQUENTIAL INSERT STRATEGY
    // 000013                  00000004            (2/89)59+ACBMACR3_SIS EQU ACBSIS
    // 000013                                      (2/90)60+* ACBMACR3_CFX EQU X'20'             FIX CBS AND BUFFS (NOT ALLOWED)
    // 000013                  00000001            (2/91)61+ACBMODE  EQU   X'01'                 BUFFERS ALLOWED ABOVE THE LINE
    // 000013                                      (2/92)62+*
    // 000013                                      (2/93)63+ACBMACR4 DS    B                  MACRF FLAGS BYTE 4
    // 000014                                      (2/94)64+* ACBRLS EQU   X'80'                 RES. LEVEL SHARING (NOT ALLOWED)
    // 000014                                      (2/95)65+*
    // 000014                                      (2/96)66+ACBBUFND DS    XL2                NO. OF DATA BUFFERS
    // 000016                                      (2/97)67+ACBBUFNI DS    XL2                NO. OF INDEX BUFFERS
    // 000018                                      (2/98)68+ACBBUFSP DS    XL4                MAX BUFFER SPACE IN BYTES
    // 00001C                                      (2/99)69+ACBLRECL DS    XL2                RECORD LENGTH FROM CATALOG
    // 00001E                                     (2/100)70+ACBPASSW DS    AL4                PASSWORD POINTER
    // 000022                                     (2/101)71+ACBEXLST DS    AL4                EXLST POINTER
    // 000026                  00000022           (2/102)72+ACBUEL   EQU   ACBEXLST
    // 000026                                     (2/103)73+*
    // 000026                                     (2/104)74+ACBINFL  DS    0BL2               INPUT FLAGS 1 & 2
    // 000026                  00000026           (2/105)75+ACBINFLG EQU   ACBINFL
    // 000026                                     (2/106)76+ACBINFL1 DS    B                  INPUT FLAG 1
    // 000027                  00000010           (2/107)77+ACBCAT   EQU   X'10'                 ACB FOR VSAM CATALOG (NOT SUPP)
    // 000027                                     (2/108)78+*
    // 000027                                     (2/109)79+ACBINFL2 DS    B                  INPUT FLAG 2
    // 000028                  00000080           (2/110)80+ACBSWARN EQU   X'80'                 SUPPRESS OPEN WARNING
    // 000028                  00000003           (2/111)81+ACBSHROP EQU   X'03'                 SHAREOPTIONS
    // 000028                  00000002           (2/112)82+ACBSHR02 EQU   X'02'                 CROSS REGION SHARE 2
    // 000028                  00000001           (2/113)83+ACBSHR01 EQU   X'01'                 CROSS REGION SHARE 1
    // 000028                                     (2/114)84+*
    // 000028                                     (2/115)85+ACBOFLGS DS    B                  OPEN FLAGS
    // 000029                  00000080           (2/116)86+ACBR31B  EQU   X'80'                 31-BIT ADDRESSING FOR BUFFERS
    // 000029                  00000040           (2/117)87+ACBR31C  EQU   X'40'                 31-BIT ADDRESSING FOR CONTROL BLKS
    // 000029                  00000020           (2/118)88+ACBEOV   EQU   X'20'                 EOV CONCATENATION
    // 000029                  00000010           (2/119)89+ACBOPEN  EQU   X'10'                 ACB CURRENTLY OPEN
    // 000029                  00000008           (2/120)90+ACBDSERR EQU   X'08'                 ERROR-ACB MUST BE CLOSED
    // 000029                  00000004           (2/121)91+ACBRECOV EQU   X'04'                 OPEN FOR RECOVERY
    // 000029                  00000002           (2/122)92+ACBEXFG  EQU   X'02'                 OFF WHEN USER EXIT IN PROGRESS
    // 000029                  00000002           (2/123)93+ACBLOCK  EQU   ACBEXFG
    // 000029                  00000001           (2/124)94+ACBIOSFG EQU   X'01'                 OPEN/CLOSE IN PROGRESS
    // 000029                  00000001           (2/125)95+ACBBUSY  EQU   ACBIOSFG
    // 000029                                     (2/126)96+*
    // 000029                                     (2/127)97+ACBERFLG DS    B                  ERROR FLAGS
    // 00002A                                     (2/128)98+*
    // 00002A                                     (2/129)99+ACBBSTNO DS    X                  NO. OF AIX CONCURRENT STRINGS
    // 00002B                                    (2/130)100+ACBSTRNO DS    X                  NO. OF CONCURRENT REQUEST STRINGS
    // 00002C                                    (2/131)101+ACBSHRP  DS    X                  LSR SHARE POOL NUMBER
    // 00002D                                    (2/133)102+ACBVER   DS    X                  zACB LAYOUT VERSION
    // 00002E                  00000002          (2/134)103+ACBV2    EQU   X'02'              zACB V2
    // 00002E                                    (2/135)104+ACBPFX   DS    AL4                ADDRESS OF DATA PREFIX BLOCK
    // 000032                                    (2/136)105+ACBXPFX  DS    AL4                ADDRESS OF INDEX PREFIX BLOCK
    // 000036                                    (2/137)106+ACBBUFD  DS    AL4                ADDRESS OF DATA BUFFERS
    // 00003A                                    (2/138)107+ACBBUFI  DS    AL4                ADDRESS OF INDEX BUFFERS
    // 00003E                                    (2/139)108+         DS    XL2                ALIGN
    // 000040                                    (2/140)109+         DS    0D
    // 000040                  00000040          (2/141)110+ACBEND   EQU   *
    // 000040                                      (1/2)112          END   ,
   }
