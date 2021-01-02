import java.io.File;
import java.util.List;
import java.util.LinkedList;

/* -------------------------------------------------------------------------- *\
 |                                                                            |
 |  File Control Block                                                        |
 |                                                                            |
\* -------------------------------------------------------------------------- */

/**
 * A Java object that maintains state for a File Control Block
 * 
 * 
 * @author  Hugh Sweeney
 * 
 */
public class zFCB {

  LinkedList data_blocks;
  zACB acb;     // The 'owning' ACB

//!! undefined  PFX_buffer pfx;
  File the_file;                  //!! what is this?
  zBuffer index_root;             //!! whhat kind of buffer is this?
  List<zBuffer> index_buffers;    //!! likewise

  /**
  Constructor
  */
  public zFCB(zACB parmACB) {
    acb = parmACB;
    //!! copy attributes from ACB
  }

  //!! I made all methods return 'int' to keep the compiler happy. HS
  /**
  Opens the physical file
  */
  public int open() {
  /*!! From zVSAM_design.ods
  Buffer create(4K)
  Set ptr to PFX-buffer
  pfx.create(file handle, PFX-buffer)
  Set ptr to PFX
  pfx.validate
  check pfx against catalog entry
  pfx.get_index_level
  Allocate data buffers
  Set ptr to Data buffer chain
  Allocate index buffers
  Set ptr to index buffer chains
  open associated index component
  open base index component
  open base data component
*/
  return 0;
  }


  /**
  Closes the physical file
  */
  public int close() {
  /*!! From zVSAM_design.ods
  close base data component
  close base index component
  close associated index component
  Buffer.flush_buffers
  Buffer.free_buffers
  update_pfx
  pfx_buffer.flush_buffer
  pfx_buffer.free_buffer
  Close physical file
  */
  return 0;
  }


  /**
  Reads the PFX
  */
  public int read_pfx() {
  return 0;
  }


  /**
  Validates the PFX
  */
  public int validate_pfx() {
  return 0;
  }


  /**
  Flushes all the buffers
  */
  public int flush_buffers() {
  return 0;
  }


  /**
  Frees the buffers
  */
  public int free_buffers() {
  return 0;
  }


  /**
  Updates the PFX
  */
  public int update_pfx() {
  return 0;
  }

}

