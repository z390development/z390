/* -------------------------------------------------------------------------- *\
 |                                                                            |
 |  Prefix Block - first block of each and every physical file                |
 |                                                                            |
 |  The PFX resides in a File Block Buffer (vzBuffer), and is a Block of a    |
 |      zVSAM V2 file, regardles of the type of file or cluster               |
 |                                                                            |
 |  The PFX is regarded and treated as a Control Block structure              |
 |      because zVSAM PFX Blocks may need to exist in z390 memory             |
 |  More precisely: when a cluster is opened a SHOWCB or TESTCB request       |
 |      may be issued against it and the data to be returned will have to be  |
 |      gathered from the PFX block                                           |
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
 * Defines a Prefix Block
 * 
 * @author  Hugh Sweeney / Abe Kornelis
 * 
 */
public class vzPFX extends vzFileBlock
   {private vzPFXPFX        pfxpfx;
    private vzPFXCTR        pfxctr;
    
    public final static int PFX_LEN = 4096; // Fixed value defined by architecture




    /**
     *
     * Constructor to create vzPFX object from an existing prefix Block
     *
     * Parameters: byte[]     holding a Prefix Block
     *             int        specifying the number of bytes in the Block
     *
     */
    public vzPFX(vzBuffer parm_buffer, int parm_length)
       {super(parm_buffer, parm_length);
        refresh(); // load BHDR + BFTR
        pfxpfx = new vzPFXPFX(parm_buffer.array(), vzBFTR.BFTR_LEN,                    parm_length);
        pfxctr = new vzPFXCTR(parm_buffer.array(), vzBFTR.BFTR_LEN + vzPFXPFX.PFXLENG, parm_length);
        }




    /**
     *
     * Refresh to reload vzPFX object from underlying array data
     *
     * Parameters: none
     *
     */
    public void refresh()
       {super.refresh(); // reload BHDR + BFTR
        pfxpfx.refresh();
        pfxctr.refresh();
        }
   }
