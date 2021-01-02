/* -------------------------------------------------------------------------- *\
 |                                                                            |
 |  Request Parameter List                                                    |
 |                                                                            |
\* -------------------------------------------------------------------------- */

/**
 * A Java object that shadows an RPL in emulator memory.
 * 
 * This object is created on issuance of a request against an ACB.
 * 
 * @author  Hugh Sweeney
 * 
 */
public class zRPL {
  private zACB acb;

  //!! How is ACB acquired?
  public zRPL(zACB parmACB) { 
    acb = parmACB;
  }
}

