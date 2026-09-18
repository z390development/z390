/* -------------------------------------------------------------------------- *\
 |                                                                            |
 |  Definition of a zVSAM Buffer (for a File Block)                           |
 |                                                                            |
 |  The Buffer is actually a File Block Buffer, used for holding a Block of a |
 |      zVSAM V2 file, regardles of the type of file, cluster, or Block       |
 |                                                                            |
 |  The Buffer is regarded and treated as a Control Block structure           |
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
 * Holds a single Block's worth of data
 * 
 * 
 * @author  Hugh Sweeney / Abe Kornelis
 * 
 */
public class vzBuffer extends zControlBlock
   {private long    XLRA;
    private boolean has_data;
    private boolean modified;




    /**
     * Constructor for a buffer mapped on a pre-existing byte array
     */
    public vzBuffer(byte[] parm_memBuff, int parm_addr, int parm_length)
       {super(parm_memBuff, parm_addr, parm_length);
        XLRA = 0;
        has_data = false;
        modified = false;
        }




    /**
     * Mark buffer as either having or not having data in it
     */
    public void mark_in_use(long parm_XLRA, boolean parm_value)
       {XLRA     = parm_XLRA;
        has_data = parm_value;
        return;
        }




    /**
    Flushes the buffer
    */
    public int flush()
       {if (modified)
           {//!! write it
            modified = false;
            }
        return 0;
        }




    /**
    Frees the buffer
    */
    //public int free() { 
    //  if (modified) throw(new RuntimeException("Attempt to free a modified buffer!!")); 
    //  bufferList.remove(this);
    //  // !! Don't forget: caller must nullify its reference to the buffer.
    //return 0;
    //}

// end of class
}