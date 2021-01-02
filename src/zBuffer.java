import java.util.LinkedList;

/* -------------------------------------------------------------------------- *\
 |                                                                            |
 |  Buffer                                                                    |
 |                                                                            |
\* -------------------------------------------------------------------------- */

/**
 * I/O data for an FCB.                                     
 * 
 * 
 * @author  Hugh Sweeney
 * 
 */
public class zBuffer {

  static LinkedList bufferList = new LinkedList();

  private byte[] data;           //!! size=BLKSIZE

  private zBuffer prev;           //!! Do we need these? Or let LinkedList methods deal with it?
  private zBuffer next;           //!!   "

  private long XLRA;
  private boolean modified;


  /**
  Constructor
  */
  public zBuffer(int blkSize) {
    data = new byte[blkSize];    //!! ? is this the best way?
    bufferList.add(this);
  }


  //!! returns int just to keep the compiler happyHS
  /**
  Flushes the buffer
  */
  public int flush() { 
    if (modified) { 
      //!! write it
      modified = false;
    }
  return 0;
  }


  /**
  Frees the buffer
  */
  public int free() { 
    if (modified) throw(new RuntimeException("Attempt to free a modified buffer!!")); 
    bufferList.remove(this);
    // !! Don't forget: caller must nullify its reference to the buffer.
  return 0;
  }

// end of class
}

