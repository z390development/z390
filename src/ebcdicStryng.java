/* ---------------------------------------------------------------- *\
   ebcdicStryng: Contains the EBCDIC re-encoding of a Unicode string.
\* ---------------------------------------------------------------- */
//!! GPL boilerplate

import java.io.*;

/**
*  This class contains the EBCDIC re-encoding of a Unicode string. (The class name is spelt with a 'y'
*  to emphasize that it's not really a java.lang.String. There is no such thing as an EBCDIC String in
*  Java.) It is actually stored as a ByteArray. The 'equals' method performs a byte-by-byte comparison
*  with any other given ByteArray.
*  <p>
*  Where a single Unicode String, such as an eyecatcher, is to be compared again and again with abritrary
*  EBCDIC data, this class may provide better performance than creating a String(ByteArray, enc) from 
*  a sequence of EBCDIC bytes, as the encoding is done only once. And the exception handling.
*/
public class ebcdicStryng {

  /**
  * The bytes containing the EBCDIC encodng of the String.
  */
  public final byte[] referenceBytes;

  /**
  * @param   referenceString The String to be re-encoded in EBCDIC.
  * @author  Hugh Sweeney
  * @version 1.6.00
  */
  public ebcdicStryng(String referenceString) throws IOException {
    OutputStreamWriter osw = null;
    ByteArrayOutputStream baos = null;

    baos = new ByteArrayOutputStream(referenceString.length()); 
    osw = new OutputStreamWriter(baos, "cp1047");    /* "De facto EBCDIC" codepage. */
    osw.write(referenceString, 0, referenceString.length()); 
    osw.close();
    referenceBytes = baos.toByteArray();
  }


  /**
  * Determines whether a given slice of a given array matches the ebcdic array.
  * @param   testBytes the array containing the sequence to be compared with the encoded basetring.
  * @param   start     the offset in the array where the sequence starts.
  * The comparison continues for as many bytes as the length of the reference String or until mismatch.
  */
  public boolean equals(byte[] testBytes, int start) {
    for (int i=0, j=start; i<referenceBytes.length; i++, j++) if (testBytes[j] != referenceBytes[i]) return false;
    return true;
  }
}
