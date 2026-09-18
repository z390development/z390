/* -------------------------------------------------------------------------- *\
 |                                                                            |
 |  Definition of a File Control Block                                        |
 |                                                                            |
 |  The File Control Block (or FCB for short) represents a file used for      |
 |      a zVSAM cluster. A cluster may consist of multiple physical files     |
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
 * 2021-04-03 Created by Abe
 * 
 ****************************************************/

import java.io.File;
import java.io.RandomAccessFile;

/**
 * A Java object that represents a physical file that is part of a zVSAM cluster
 * 
 * 
 * @author  Hugh Sweeney / Abe Kornelis
 * 
 */
public class vzFCB
   {private final vzACB          Parent_ACB;
    private final int            ACB_addr;     // in z memory
    private final int            FCB_seqno;
    private final String         path_n_file;
    
    private File                 file;
    private RandomAccessFile     RAfile;
    private byte[]               PFXmem;
    private vzBuffer             PFXbuff;
    private vzPFX                PFX;

    /**
    Constructor
    */
    public vzFCB(vzACB parm_vzACB, int parm_FCB_seq, String parm_path_n_file)
       {Parent_ACB = parm_vzACB;
        ACB_addr = parm_vzACB.Get_ACB_addr();
        FCB_seqno = parm_FCB_seq;
        path_n_file = parm_path_n_file;
        //PFX = new vzPFX();
        }




    /**
     * Opens the physical file and validates it
     * 
     */
    public void open()
       {//First create the file object to validate whether the file exists
        file = new File(path_n_file);
        if (file.exists() && file.isDirectory())
            throw new zException("Cannot open directory " + path_n_file, 8, 88);
        if (file.exists() && !file.isFile())
            throw new zException("Cannot open file " + path_n_file + " - incompatible attributes", 8, 88);
        
        // Allocate buffer for the Prefix area
        PFXmem = new byte[vzPFX.PFX_LEN];
        PFXbuff = new vzBuffer(PFXmem, 0, vzPFX.PFX_LEN);
        
        // Allow file to be created if it does not exist and ACB specifies output, no input
        if (!file.exists() && !Parent_ACB.acbin() && Parent_ACB.acbout())
           {try
               {if (!file.createNewFile())
                    throw new zException("Cannot create file " + path_n_file, 8, 88);
                }
            catch (Exception e)
               {throw new zException("Error while creating file " + path_n_file, 8, 88);
                }
            // **!! Need to create Prefix Block and first spacemap page
            throw new zException("zFCB.open: cannot create new cluster", 8, 8);
            }
        
        // Open the file as a RandomAccessFile
        try
           {RAfile = new RandomAccessFile(file, (Parent_ACB.acbout()) ? "rw" : "r");
            }
        catch(Exception e)
           {throw new zException(e.getMessage(), 8, 88); 
            }
        
        // Read the PFX block
        try
           {RAfile.readFully(PFXbuff.array(), 0 , vzPFX.PFX_LEN);
            PFXbuff.mark_in_use(0, true);
            }
        catch(Exception e)
           {throw new zException(e.getMessage(), 8, 88); 
            }
        
        // Create the PFX object
        if (PFX == null)
            PFX = new vzPFX(PFXbuff, vzPFX.PFX_LEN);
        else
            PFX.refresh();
        
        // Validate PFX content


        
        return;
        }




    /**
    Closes the physical file
    */
    //public int close()
    //   {/*!! From zVSAM_design.ods
    //    close base data component
    //    close base index component
    //    close associated index component
    //    Buffer.flush_buffers
    //    Buffer.free_buffers
    //    update_pfx
    //    pfx_buffer.flush_buffer
    //    pfx_buffer.free_buffer
    //    Close physical file
    //    */
    //    return 0;
    //    }




    /**
    Reads the PFX
    */
    //public int read_pfx() {
    //return 0;
    //}




     /**
    Validates the PFX
    */
    //public int validate_pfx() {
    //return 0;
    //}




    /**
    Flushes all the buffers
    */
    //public int flush_buffers() {
    //return 0;
    //}




    /**
    Frees the buffers
    */
    //public int free_buffers() {
    //return 0;
    //}




    /**
    Updates the PFX
    */
    //public int update_pfx() {
    //return 0;
    //}
    }
