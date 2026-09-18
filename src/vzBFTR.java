/* -------------------------------------------------------------------------- *\
 |                                                                            |
 |  Definition of a zVSAM Block Footer                                        |
 |                                                                            |
 |  The Block Footer is the terminating structure in any Block in any         |
 |       zVSAM V2 file, regardles of the type of file, cluster, or Block      |
 |                                                                            |
 |  The Block Footer is regarded and treated as a Control Block structure     |
 |      because zVSAM data Blocks and therefore also the Block Footer         |
 |      may exist in z390 memory                                              |
 |  More precisely: when a cluster is opened in locate mode the Blocks        |
 |      of the file must reside in z390 memory. In move mode the Blocks       |
 |      may or may not reside in z390 memory                                  |
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
 * 2021-4     Initial creation by Abe / Hugh
 * 
 ****************************************************/

/**
 * Holds footer data for a single Block Footer
 * 
 * 
 * @author  Hugh Sweeney / Abe Kornelis
 * 
 */
public class vzBFTR extends zControlBlock
   {private byte[]  bftreye;
    private byte    bftrseqnum;

    /* 
     * Offsets of fields within the BFTR block
     * 
     */
    private static final short BFTREYE     = 0x0000;
    private static final short BFTREYE_LEN = 0x0003;
    private static final short BFTRSEQNUM  = 0x0003;
    public  static final short BFTR_LEN    = 0x0028;

    /* 
     * Values of constants
     * 
     */
    private static final String       EyeCatcher       = "FTR";
    private static final ebcdicStryng ebcdicEyeCatcher = new ebcdicStryng(EyeCatcher);




    /**
     *
     * Constructor to create BFTR object form an existing file Block
     *
     * Parameters: byte[]     holding a Block from the file
     *             int        holding the offset of the BFTR in the Block
     *             int        specifying the size of the underlying byte array
     *
     */
    public vzBFTR(byte[] parm_memBuff, int parm_addr, int parm_size)
       {super(parm_memBuff, parm_addr, BFTR_LEN);
        
        // Ensure the input is a valid BFTR
        if (parm_addr == 0)
           throw new zException("BFTR area must always be at the end of the Block", 8, 8);
        if (parm_addr + BFTR_LEN - 1 > parm_size)
           throw new zException("BFTR area is too small", 8, 8);
        
        // Allocate dependent fields and refresh all object data
        bftreye = new byte[BFTREYE_LEN];
        refresh();
        }




    /**
     *
     * Refresh BFTR object from underlying array data
     *
     * Parameters: none
     *
     */
    public void refresh()
       {// Retrieve all BFTR fields from the input buffer
        System.arraycopy(this.array(), this.get_offset() + BFTREYE, bftreye, 0, BFTREYE_LEN);
        bftrseqnum   = get_byte(BFTRSEQNUM);
        
        // Additional validations
        if (! ebcdicEyeCatcher.equals(bftreye) )
           throw new zException("BFTR eyecatcher is incorrect", 8, 8);
        return;
        }




    /**
     *
     * Getter functions
     *
     * Parameters: none
     *
     */
     byte bftrseqnum() {return bftrseqnum;}
   }

