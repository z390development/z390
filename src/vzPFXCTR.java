/* -------------------------------------------------------------------------- *\
 |                                                                            |
 |  Prefix Block - Counters area                                              |
 |                                                                            |
 |  The PFXCTR resides in the Prefix Block vzPFX                              |
 |      and contains various zVSAM counters for use with file management      |
 |                                      and for use with TESTCB and SHOWCB    |
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
 * 
 * 2021-04    Initially created by Abe
 * 
 ****************************************************/

/**
 * Defines the Counters area of a Prefix Block
 * 
 * @author  Hugh Sweeney / Abe Kornelis
 * 
 */
public class vzPFXCTR extends zControlBlock
   {private byte[] ctreye     ; // eyecatcher
    private int    ctravgrl   ; // average record length
    private long   ctravspac  ; // available space
    private long   ctrhalcrba ; // high-allocated RBA
    private long   ctrhlrba   ; // high-allocated index block RBA
    private long   ctrendrba  ; // high water mark for the component
    private long   ctrnbfrfnd ; // number of LSR reads
    private long   ctrnbufno  ; // number of buffers allocated
    private long   ctrnbufuse ; // number of buffers in use
    private long   ctrnbufrds ; // number of buffer reads
    private long   ctrncis    ; // number of block-split operations
    private long   ctrndelr   ; // number of delete operations
    private long   ctrnexcp   ; // number of I/O operations
    private long   ctrnext    ; // number of physical files (always 1)
    private long   ctrninsr   ; // number of insert operations
    private long   ctrnlogr   ; // number of records in this component
    private long   ctrnretr   ; // number of retrieve operations
    private long   ctrnnuiw   ; // number of non-user writes
    private long   ctrnupdr   ; // number of updates
    private long   ctrsdtasz  ; // uncompressed data size
    private long   ctrstmst   ; // STCK of last close
    private long   ctrstrmax  ; // maximum number of active strings
    private long   ctrnuiw    ; // number of user writes
    private short  ctrlokey_at; // offset of lowest valid key value
    
    /* 
     * Offsets of fields within the Counters block
     * 
     */
    private static final short CTREYE      = 0x0000;
    private static final short CTREYE_LEN  = 0x0004;
    private static final short CTRAVGRL    = 0x0004;
    private static final short CTRAVSPAC   = 0x0008;
    private static final short CTRHALCRBA  = 0x0010;
    private static final short CTRHLRBA    = 0x0018;
    private static final short CTRENDRBA   = 0x0020;
    private static final short CTRNBFRFND  = 0x0028;
    private static final short CTRNBUFNO   = 0x0030;
    private static final short CTRNBUFUSE  = 0x0038;
    private static final short CTRNBUFRDS  = 0x0040;
    private static final short CTRNCIS     = 0x0048;
    private static final short CTRNDELR    = 0x0050;
    private static final short CTRNEXCP    = 0x0058;
    private static final short CTRNEXT     = 0x0060;
    private static final short CTRNINSR    = 0x0068;
    private static final short CTRNLOGR    = 0x0070;
    private static final short CTRNRETR    = 0x0078;
    private static final short CTRNNUIW    = 0x0080;
    private static final short CTRNUPDR    = 0x0088;
    private static final short CTRSDTASZ   = 0x0090;
    private static final short CTRSTMST    = 0x0098;
    private static final short CTRSTRMAX   = 0x00A0;
    private static final short CTRNUIW     = 0x00A8;
    private static final short CTRLOKEY_AT = 0x00AA;
    private static final short CTRLENG     = 0x00AA;
    
    /* 
     * Values of constants
     * 
     */
    private static final String       EyeCatcher       = "zCTR";
    private static final ebcdicStryng ebcdicEyeCatcher = new ebcdicStryng(EyeCatcher);




    /**
     *
     * Constructor to create vzPFXCTR object from an existing prefix Block
     *
     * Parameters: byte[]     holding a Prefix Block
     *             int        holding the offset of the Prefix counters area in the Block
     *             int        specifying the size of the underlying byte array
     *
     */
    public vzPFXCTR(byte[] parm_memBuff, int parm_addr, int parm_size)
       {super(parm_memBuff, parm_addr, CTRLENG);
        
        // Ensure the input is a valid Prefix data area
        if (parm_addr != vzBHDR.BHDRLENG + vzPFXPFX.PFXLENG)
           throw new zException("Prefix counters area must always follow the Prefix data", 8, 8);
        if (parm_addr + CTRLENG - 1 > parm_size)
           throw new zException("Prefix counters area is too small", 8, 8);
        
        // Allocate dependent fields and refresh all object data
        ctreye  = new byte[CTREYE_LEN];
        refresh();
        }




    /**
     *
     * Refresh Prefix counters from underlying array data
     *
     * Parameters: none
     *
     */
    public void refresh()
       {// Retrieve all Counter fields from the input buffer
        System.arraycopy(this.array(), this.get_offset() + CTREYE, ctreye, 0, CTREYE_LEN);
        ctravgrl     = get_int   (CTRAVGRL   );
        ctravspac    = get_long  (CTRAVSPAC  );
        ctrhalcrba   = get_long  (CTRHALCRBA );
        ctrhlrba     = get_long  (CTRHLRBA   );
        ctrendrba    = get_long  (CTRENDRBA  );
        ctrnbfrfnd   = get_long  (CTRNBFRFND );
        ctrnbufno    = get_long  (CTRNBUFNO  );
        ctrnbufuse   = get_long  (CTRNBUFUSE );
        ctrnbufrds   = get_long  (CTRNBUFRDS );
        ctrncis      = get_long  (CTRNCIS    );
        ctrndelr     = get_long  (CTRNDELR   );
        ctrnexcp     = get_long  (CTRNEXCP   );
        ctrnext      = get_long  (CTRNEXT    );
        ctrninsr     = get_long  (CTRNINSR   );
        ctrnlogr     = get_long  (CTRNLOGR   );
        ctrnretr     = get_long  (CTRNRETR   );
        ctrnnuiw     = get_long  (CTRNNUIW   );
        ctrnupdr     = get_long  (CTRNUPDR   );
        ctrsdtasz    = get_long  (CTRSDTASZ  );
        ctrstmst     = get_long  (CTRSTMST   );
        ctrstrmax    = get_long  (CTRSTRMAX  );
        ctrnuiw      = get_long  (CTRNUIW    );
        ctrlokey_at  = get_short (CTRLOKEY_AT);
        
        // Additional validations
        if (! ebcdicEyeCatcher.equals(ctreye) )
           throw new zException("Prefix Counters eyecatcher is incorrect", 8, 8);
        return;
        }
    }
