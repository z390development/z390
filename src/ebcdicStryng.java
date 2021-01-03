/* ---------------------------------------------------------------- *\
   ebcdicStryng: Contains the EBCDIC re-encoding of a Unicode string.
\* ---------------------------------------------------------------- */
//!! GPL boilerplate

import java.io.*;




/**
*  This class contains the EBCDIC re-encoding of a Unicode string. 
<p>
*  (The class name is spelt with a 'y' to emphasize that it's not really a java.lang.String. 
*  There is no such thing as an EBCDIC String in Java.) It is actually stored as a byte array. 
*  The 'equals' method performs a byte-by-byte comparison with any other given byte array.
<p>
*  Where a single Unicode String, such as an eyecatcher, is to be compared again and again with arbitrary
*  EBCDIC data, this class should provide better performance than creating a String(byte[], enc) from 
*  a sequence of EBCDIC bytes every time, as the encoding is done only once. And the exception handling.
*
*  @author  Hugh Sweeney
*  @version 1.6.00
*/
public class ebcdicStryng {


/**
* The bytes containing the EBCDIC encodng of the String.
*/
  private final byte[] referenceBytes;




/**
* A constructor exception to be percolated upward.
*
* In order to simplify coding in the caller, these constructors do NOT throw exceptions.
* Instead, they save any exception in this field for the caller to examine.
*
<p>
* I designed this class to enable the z390 java coder to declare EBCDIC constants for
* later comparison with supposed EBCDIC data in z-memory. Declaring a constant should
* be a one-liner in Java. However, a good Java coder will declare a constant with the
* <code>final</code> attribute. Otherwise any fool could later make a change to the
* Java code to alter the value of the EBCDIC variable that was intended to be
* constant. (The fool is not necessarily a different person.) The <code>final</code>
* poses a problem for this class, because it uses methods from the JRE that can throw
* exceptions. If this class's constructor just re-throws the exception upwards, then
* the Java coder will have to wrap the assignment of the value in a try-catch block.
* The compiler, noticing that the assignment is to a final field, will check that it
* can prove <em>both</em> of the following assertions, otherwise it will generate an 
* error message:
<ul>
  <li> the field gets assigned a value at least once; </li>
  <li> the field gets assigned a value at most once;  </li>
</ul>
<p>
* The possible paths through a <code>try-catch[-finally]</code> block make it very
* difficult to meet the compiler's requirements. I can't do it in less than about
* 9 lines of code. That's a ridiculous amount for a coder just to define a constant.
* So I have placed all that complex code in the constructor, to hide it. 
<p>
* The z390 Java coder now need only write one line after their constant declaration, 
* <b><code>final ebcdicStryng myField = new ebcdicStryng("XYZ")</code></b>, thus:
* <b><code>if (myField.ctorXcpn != null) ...</code></b> (instead of 9) to determine 
* if an exception was thrown in the constructor.
<p>
* 
* AFAIK, the exceptions that could be thrown by the JRE methods used here are all
* indicative of very serious problems (1. the EBCDIC code page is not available in
* this JVM instance, and 2. some part of the reference String is untranslateable 
* to EBCDIC) so in most cases it would be pointless for z390 to continue. 
* In such a case the Java coder need only write their declaration as
* <b><code>final ebcdicStryng myField = new ebcdicStryng("XYZ", true)</code></b> and
* need not test the result. That boolean passed in the <b><code>abendOnExcpn</code></b>
* constructor parameter tells the constructor to abend the JVM if a problem occurs.
* So, if control returns to z390 after the <b><code>new</code></b> operator, there 
* was no problem.
*
<p>
* If the caller finds this field non-null, it should trust no other member(s) of this
* object to have been rationally initialised, as it is not possible for the try-catch
* construct to detect exactly where in the try block the exception was thrown.
*/
  public Exception ctorXcpn = null;    // The exception thrown by the constructor.




/**
* Simple constructor. Caller should test for exception on return.
* @param   referenceString    The String to be re-encoded in EBCDIC.
*/
  public ebcdicStryng(String referenceString)  
  {
    OutputStreamWriter osw = null;
    ByteArrayOutputStream baos = null;
    baos = new ByteArrayOutputStream(referenceString.length()); 

    byte[] temp = null;
    try {   
      osw = new OutputStreamWriter(baos, "cp1047");    /* "De facto EBCDIC" codepage. */
      osw.write(referenceString, 0, referenceString.length()); 
      osw.close();
      temp = baos.toByteArray();
    }
    catch(Exception e) {ctorXcpn = e;} // Caller MUST examine constructionException to detect failure.
    finally { referenceBytes = temp; }
  }




/**
* A constructor that can terminate the JVM in case of error.
<p>
* It is a wrapper around a call to the simple constructor.
<p>
* Experimental: this may suit callers during z390 initialisation where continuing after error is pointless.
* The one-parameter constructor may be more suitable for other cases.
*
* @param   referenceString The String to be re-encoded in EBCDIC.
* @param   excpnAction     Terminate JVM, or return exception, if any error.
*
*/
  public ebcdicStryng(String referenceString, onException excpnAction ) {
    this(referenceString);
    if (ctorXcpn != null) {                   // There was a problem.
      if (excpnAction == onException.ABNDZ390) {             // We've been told to abend if a problem occurs.
        System.out.println("Fatal Error: ebdicStryng(\""+referenceString+"\") failed initialisation: " + ctorXcpn);
        System.exit(100);                     // !! Is exit code unique? Does it matter?
      }
      // else allow caller to examine exception.
    } 
  }

/** Constants to allow a client to document what it wants to happen if constructor throws an exception. */
  enum onException { ABNDZ390, RETURNX };


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
