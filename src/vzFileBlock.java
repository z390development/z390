/* -------------------------------------------------------------------------- *\
 |                                                                            |
 |  Definition of a zVSAM file Block                                          |
 |                                                                            |
 |  The Block resides in a File Block Buffer (vzBuffer), and is a Block of a  |
 |      zVSAM V2 file, regardles of the type of file, cluster, or Block       |
 |                                                                            |
 |  The Block is regarded and treated as a Control Block structure            |
 |      because zVSAM data Blocks and therefore also the Buffer containing it |
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
 * 2021-04    Initially created by Abe
 * 
 ****************************************************/

/**
 * Holds a single Block of any subtype
 * 
 * 
 * @author  Hugh Sweeney / Abe Kornelis
 * 
 */
public class vzFileBlock
   {private vzBuffer   buffer;
    private vzBHDR     bhdr;
    private vzBFTR     bftr;




    /**
     *
     * Constructor to create vzBlock object
     *
     * Parameters: vzBuffer   vzBuffer holding the Block as read from the file
     *             int        holding the length of the Block
     *
     */
    public vzFileBlock(vzBuffer parm_buffer, int parm_length)
       {if (parm_length < vzBHDR.BHDRLENG + vzBFTR.BFTR_LEN) //**!! More space is needed!
           throw new zException("vzBlock length is too small", 8, 8);
        
        buffer = parm_buffer;
        bhdr   = null; // new vzBHDR(buffer, 0, parm_length);
        bftr   = null; // new vzBFTR(buffer, 0, parm_length);
        }




    /**
     *
     * Refresh vzBlock object from underlying array data
     *
     * Parameters: none
     *
     */
    public void refresh()
       {// Load or reload the Block Header
        if (bhdr == null)
            bhdr = new vzBHDR(buffer.array(), 0);
        else
            bhdr.refresh();
        
        // Load or reload the Block Footer
        if (bftr == null)
            bftr = new vzBFTR(buffer.array(), 0, buffer.get_length() - vzBFTR.BFTR_LEN);
        else
            bftr.refresh();
        
        // Validate BHDR agains BFTR
        if (bhdr.bhdrseqnum() != bftr.bftrseqnum())
            throw new zException("Write control value of Block is invalid", 8, 8);
        }
    }
