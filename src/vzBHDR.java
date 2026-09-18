/* *********************************************************************
 *                                                                     *
 *  Definition of a zVSAM Block Header                                 *
 *                                                                     *
 *  The Block Header is the leading structure in any Block in any      *
 *    zVSAM V2 file, regardles of the type of file, cluster, or Block  *
 *                                                                     *
 *  The Block Header is regarded and treated as a Control Block        *
 *    structure because zVSAM data Blocks and therefore also the       *
 *    Block Header may exist in z390 memory                            *
 *  More precisely: when a cluster is opened in locate mode the Blocks *
 *    of the file must reside in z390 memory. In move mode the Blocks  *
 *    may or may not reside in z390 memory                             *
 *                                                                     *
 * *********************************************************************
 *                                                                     *
 * z390 portable mainframe assembler and emulator.                     *
 *                                                                     *
 * Copyright 2011 Automated Software Tools Corporation                 *
 *                                                                     *
 * z390 is free software; you can redistribute it and/or modify        *
 * it under the terms of the GNU General Public License as published by*
 * the Free Software Foundation; either version 2 of the License, or   *
 * (at your option) any later version.                                 *
 *                                                                     *
 * z390 is distributed in the hope that it will be useful,             *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of      *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the       *
 * GNU General Public License for more details.                        *
 *                                                                     *
 * You should have received a copy of the GNU General Public License   *
 * along with z390; if not, write to the Free Software Foundation, Inc.*
 * 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA             *
 *                                                                     *
 * ********************************************************************* */

/**
 * Class vzBHDR holds data for
 *    a single BHDR area
 */
public class vzBHDR extends zControlBlock
   {private byte[] bhdreye;    // Eyecatcher field
    private byte   bhdrseqnum; // Write control value
    private byte   bhdrver;    // Design sequence number
    private byte   bhdrflg1;   // Flags to indicate Block type (1)
    private byte   bhdrflg2;   // Flags to indicate Block type (2)
    private byte   bhdrnumrec; // No. of records in this Block
    private byte   bhdrxlvl;   // Index level. 0 for Leaf Block
    private long   bhdrself;   // XLRA pointer to self
    private long   bhdrnext;   // XLRA pointer to next sequential block
    private long   bhdrprev;   // XLRA pointer to prev sequential block
    private int    bhdrfre_at; // Offset to free space area
    private int    bhdrfree;   // Remaining free space in Block

    /*
     * Offsets of fields within the BHDR block
     *
     */
    private static final short BHDREYE        = 0x0000;
    private static final short BHDREYE_LEN    = 0x0003;
    private static final short BHDRSEQNUM     = 0x0003;
    private static final short BHDRVER        = 0x0004;
    private static final short BHDRFLG1       = 0x0005;
    private static final short BHDRFLG2       = 0x0006;
    private static final short BHDRNUMREC     = 0x0007;
    private static final short BHDRXLVL       = 0x0008;
    private static final short BHDRSELF       = 0x0009;
    private static final short BHDRNEXT       = 0x0011;
    private static final short BHDRPREV       = 0x0019;
    private static final short BHDRFRE_AT     = 0x0021;
    private static final short BHDRFREE       = 0x0025;
    public  static final short BHDRLENG       = 0x0030;

    /*
     * Values of constants
     *
     */
    private static final String       BHDRBHDR        = "HDR"; // BHDREYE Fixed value
    private static final ebcdicStryng BHDRBHDR_ebcdic = new ebcdicStryng(BHDRBHDR);
    private static final byte         BHDR_V2         = (byte) 0x02; // BHDRVER - Version 2 (this version)
    private static final byte         BHDR_PFX        = (byte) 0x80; // BHDRFLG1 - Prefix Block
    private static final byte         BHDR_MAP        = (byte) 0x40; // BHDRFLG1 - Spacemap Block
    private static final byte         BHDR_DTA        = (byte) 0x20; // BHDRFLG1 - Data Block
    private static final byte         BHDR_IDX        = (byte) 0x10; // BHDRFLG1 - Index Block
    private static final byte         BHDR_SEG        = (byte) 0x08; // BHDRFLG1 - Segment Block
    private static final byte         BHDR_LEF        = (byte) 0x04; // BHDRFLG1 - Index Block (Leaf)
    private static final byte         BHDR_INT        = (byte) 0x02; // BHDRFLG1 - Index Block (Intermediate)
    private static final byte         BHDR_ROT        = (byte) 0x01; // BHDRFLG1 - Index Block (Root)
    private static final byte         BHDR_ELX        = (byte) 0x80; // BHDRFLG2 - Extended Level Index (ELIX)




    /**
     *
     * Constructor to create BHDR object from existing data in a byte array
     *
     * Parameters: byte[]     the containing byte array
     *             int        holding the offset of the BHDR block in the byte array
     *
     */
    public vzBHDR(byte[] parm_memBuff, int parm_addr)
       {super(parm_memBuff, parm_addr, BHDRLENG);

        // Ensure the input is a valid BHDR
        if (parm_addr + BHDRLENG - 1 > parm_memBuff.length)
           throw new zException("BHDR area is too small", 8, 8);

        // Allocate dependent fields and refresh all object data
        bhdreye = new byte[BHDREYE_LEN];
        refresh();
        validate();
        }




    /**
     *
     * Refresh BHDR object from underlying array data
     *
     * Parameters: none
     *
     */
    public void refresh()
       {// Retrieve all BHDR fields from the input buffer
        System.arraycopy(this.array(), this.get_offset() + BHDREYE, bhdreye, 0, BHDREYE_LEN);
        bhdrseqnum = get_byte(BHDRSEQNUM);
        bhdrver    = get_byte(BHDRVER);
        bhdrflg1   = get_byte(BHDRFLG1);
        bhdrflg2   = get_byte(BHDRFLG2);
        bhdrnumrec = get_byte(BHDRNUMREC);
        bhdrxlvl   = get_byte(BHDRXLVL);
        bhdrself   = get_long(BHDRSELF);
        bhdrnext   = get_long(BHDRNEXT);
        bhdrprev   = get_long(BHDRPREV);
        bhdrfre_at = get_int(BHDRFRE_AT);
        bhdrfree   = get_int(BHDRFREE);
        return;
        }




    /**
     *
     * Getter functions
     *
     * Parameters: none
     *
     */
    byte[] bhdreye()    {return bhdreye;}
    byte   bhdrseqnum() {return bhdrseqnum;}
    byte   bhdrver()    {return bhdrver;}
    byte   bhdrflg1()   {return bhdrflg1;}
    byte   bhdrflg2()   {return bhdrflg2;}
    byte   bhdrnumrec() {return bhdrnumrec;}
    byte   bhdrxlvl()   {return bhdrxlvl;}
    long   bhdrself()   {return bhdrself;}
    long   bhdrnext()   {return bhdrnext;}
    long   bhdrprev()   {return bhdrprev;}
    int    bhdrfre_at() {return bhdrfre_at;}
    int    bhdrfree()   {return bhdrfree;}
    String bhdreye_utf()
       {String result;
        try {result = new String(this.array(), BHDREYE, BHDREYE_LEN, "cp1047");}
        catch (java.io.UnsupportedEncodingException e)
           {throw new zException("Cannot convert BHDREYE from EBCDIC: " + e, 8, 8);}
        if (!result.isEmpty()) // Trim trailing spaces - only if first character is not a space
           {if (result.charAt(0) != ' ')
               {result = result.trim();
            }   }
        return result;
        }




    /**
     *
     * Validate BHDR object data
     *
     * Parameters: none
     *
     */
    public void validate()
       {if (! BHDRBHDR_ebcdic.equals(bhdreye) )
            throw new zException("BHDR eyecatcher is incorrect", 8, 8);
        if (bhdrver != BHDR_V2)
            throw new zException("BHDR not marked as a V2 conforming Block", 8, 8);
        if ((bhdrflg1 & BHDR_DTA) == BHDR_DTA)
            {}
        else if (((bhdrflg1 & BHDR_IDX) == BHDR_IDX)
              && ((bhdrflg1 & BHDR_LEF) == BHDR_LEF))
            {}
        else if (((bhdrflg1 & BHDR_IDX) == BHDR_IDX)
              && ((bhdrflg1 & BHDR_INT) == BHDR_INT))
            {}
        else if (((bhdrflg1 & BHDR_IDX) == BHDR_IDX)
              && ((bhdrflg1 & BHDR_ROT) == BHDR_ROT))
            {}
        else if ((bhdrflg1 & BHDR_MAP) == BHDR_MAP)
            {}
        else if ((bhdrflg1 & BHDR_PFX) == BHDR_PFX)
            {}
        else if ((bhdrflg1 & BHDR_SEG) == BHDR_SEG)
            {}
        else
            throw new zException("BHDR Invalid Block type", 8, 8);
        if (bhdrxlvl != 0
         && ((bhdrflg1 & BHDR_IDX) != BHDR_IDX))
            throw new zException("BHDR Invalid index level", 8, 8);
        if (((bhdrflg1 & BHDR_LEF) == BHDR_LEF)
         && (bhdrxlvl != 0))
            throw new zException("BHDR Incorrect index level for leaf Block", 8, 8);
        if (((bhdrflg1 & BHDR_IDX) == BHDR_IDX)
         && ((bhdrflg1 & BHDR_LEF) != BHDR_LEF)
         && (bhdrxlvl == 0))
            throw new zException("BHDR Incorrect index level for non-leaf Block", 8, 8);
        if (bhdrnext == 0)
            throw new zException("BHDR Incorrect pointer to next Block", 8, 8);
        if (bhdrprev == 0)
            throw new zException("BHDR Incorrect pointer to previous Block", 8, 8);
        return;
        }
    }
