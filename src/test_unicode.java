/*
z390 - Mainframe assembler emulator and run-time engine
Copyright (C) 2021 z390 Assembler LLC

This file is part of z390.
z390 is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

z390 is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License along
with this program; if not, see <https://www.gnu.org/licenses/>.
*/

import java.nio.charset.Charset;
import java.util.Iterator;
import java.util.Map;


/**
 * test_unicode is probably for testing only.
 */
public class  test_unicode{
   /****************************************************
    * Maintenance
    ****************************************************
    * 2026-02-15 AFK        Add javadoc comments
    */



   /**
    * Dummy constructor - no initialization needed
    */
    public test_unicode()
       {// dummy constructor - no initialization needed.
        }



   /**
    * start instance of zcobol class
    * @param argv standard argument list
    */
	public static void main(String argv[]) {
		  test_unicode pgm = new test_unicode();
	      pgm.main();
	}


   /**
    * start instance of zcobol class
    */
    public void main(){
        // System 390 EBCDIC
        // 2019-09-20 dsh fix raw interator by adding <?>
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



   /**
    * report default character set
    */
    private void list_default_charset(){
    	String name = Charset.defaultCharset().name();
    	System.out.println("default charset = " + name);
    }



   /**
    * report all available character sets
    */
    private void list_available_charsets(){
    	Map<?, ?> map = Charset.availableCharsets();
    	Iterator<?> it = map.keySet().iterator();
    	while (it.hasNext()) {
        // Get charset name
    	String charsetName = (String)it.next();
    
        // Get charset
    	Charset charset = Charset.forName(charsetName);
    	System.out.println(charset);
    	}
    
    }
}
