/* ========================================================================== *\
||                                                                            ||
||                       C O N T R O L    B L O C K S                         ||
||                                                                            ||
\* ========================================================================== */



/* -------------------------------------------------------------------------- *\
 |                                                                            |
 |  Access Control Block                                                      |
 |                                                                            |
\* -------------------------------------------------------------------------- */

/**
 * A Java object that shadows an ACB that is in emulator memory.
 * 
 * This object is created on execution of the Open SVC. It is built from the ACB
 * in e-memory that is passed with the SVC.
 * 
 * @author  Hugh Sweeney
 * 
 */
public class ACB {
   // !! all members private except those that need otherwise.
   // !! keep a copy of all mACB fields here, or access as needed?
   // !! See IFGACB

   private int mACB;   // address of ACB in mem_byte
   private FCB theFCB;


/**
 * Populates the ACB from the e-memory contents.
 * 
 * @param   mem      int pointing to a memory location that should be an ACB.
 * 
 */
   public ACB(mem) {
      //!! store time?
   }



/**
 * Accesses the ddname->file, reads, and verifies the Prefix block.
 *
 *
 */
   public int open() {
      // called from constructor?
      // cead catlg
      // create FCB
   }


/**
 * Writes pending blocks, Prefix Block. Closes file component(s).
 * Discards FCB.
 * Marks mACB closed. Marks ACB closed.
 *
 *
 */
   public int close() {
      // called from constructor?
      // create FCB
   }

}               


/* -------------------------------------------------------------------------- *\
 |                                                                            |
 | Prefix Block                                                               |
 |                                                                            |
\* -------------------------------------------------------------------------- */


/* These blocks can have their own self-validation methods, 
   and methods to eg copy themselves to a byte array. 
   */

/* -------------------------------------------------------------------------- *\
 |                                                                            |
 | Block Header                                                               |
 |                                                                            |
\* -------------------------------------------------------------------------- */

/* -------------------------------------------------------------------------- *\
 |                                                                            |
 | Block Footer                                                               |
 |                                                                            |
\* -------------------------------------------------------------------------- */

/* -------------------------------------------------------------------------- *\
 |                                                                            |
 |                                                                            |
 |                                                                            |
\* -------------------------------------------------------------------------- */

