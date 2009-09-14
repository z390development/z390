import java.nio.charset.Charset;
import java.util.Iterator;
import java.util.Map;


public class  test_unicode{
	public static void main(String argv[]) {		
	      /*
	       * start instance of zcobol class
	       */
		  test_unicode pgm = new test_unicode();
	      pgm.main();
	}
public void main(){
// System 390 EBCDIC
String encoding = "Cp1047";

// microsoft proprietary USA
// encoding = "Cp037";

// IBM PC OEM DOS
// encoding = "Cp437";
try {
	String unicode = "1234567890abcdefghijklmnopqrstuvwxyz";
	byte[] ebcdic = unicode.getBytes( encoding );
	String reconsituted = new String( ebcdic, encoding );
	System.out.println( unicode );
	System.out.println( reconsituted );
	list_default_charset();
	list_available_charsets();
} catch (Exception e){
	System.out.println("test_unicode trap - " + e);
}
}
private void list_default_charset(){
	String name = Charset.defaultCharset().name();
	System.out.println("default charset = " + name);
}
@SuppressWarnings("unchecked")
private void list_available_charsets(){
	Map map = Charset.availableCharsets();
	Iterator it = map.keySet().iterator();
	while (it.hasNext()) {
    // Get charset name
		String charsetName = (String)it.next();

    // Get charset
		Charset charset = Charset.forName(charsetName);
		System.out.println(charset);
	}

}

}

