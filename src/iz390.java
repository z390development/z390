import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;


public class iz390{
	
	public static void main(String argv[]) {
		/*****************************************************
		
	    z390 portable mainframe assembler and emulator.
		
	    Copyright 2011 Automated Software Tools Corporation
		 
	    z390 is free software; you can redistribute it and/or modify
	    it under the terms of the GNU General Public License as published by
	    the Free Software Foundation; either version 2 of the License, or
	    (at your option) any later version.

	    z390 is distributed in the hope that it will be useful,
	    but WITHOUT ANY WARRANTY; without even the implied warranty of
	    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	    GNU General Public License for more details.

	    You should have received a copy of the GNU General Public License
	    along with z390; if not, write to the Free Software
	    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

	    iz390 is a standalone  installation verification
	    program which displays current version of the 
	    following components:
	      1. z390.jar - version from tz390
	      2. OS from  - os.name  J2SE property 

	    ****************************************************
	    * Maintenance
	    ****************************************************
	    * 05/06/07 initial coding for v1303b 
	    * 07/12/07 RPI  656 change RT to MVS if MVS not installed
	    * 10/20/07 RPI  713 change file name case for Linux
	    * 09/25/09 RPI 1080 replace init_tables with init_tz390
	    ****************************************************
	    *                                       last RPI
	    *                                        
    	 */
		tz390  tz390 = null;
		String os_ver   = System.getProperty("os.name"); 
		String j2se_ver = System.getProperty("java.version");
		String z390_ver = null;
		String z390_base_ver = null;
		String z390_dir = System.getProperty("user.dir");
		String ivp_file_name = null;
		File   ivp_file = null;
		BufferedReader ivp_buff = null;
		String ivp_ver = null;
		int    ivp_rc  = 0;
	    /*
		 * init iz390
		 */
    	tz390 = new tz390();
    	tz390.init_tz390();   // RPI 1080 
        z390_ver = tz390.version;
        z390_base_ver = z390_ver.substring(0,7);
        System.out.println("IVP OS   version = " + os_ver);
        System.out.println("IVP J2SE version = " + j2se_ver);
        System.out.println("IVP z390 version = " + z390_ver);
	    ivp_file_name = z390_dir + File.separator + "Z390.IVP"; // RPI 713
        ivp_file      = new File(ivp_file_name);
	    if (ivp_file.isFile()){
        	try {
				ivp_buff = new BufferedReader(new FileReader(ivp_file_name));
				ivp_ver = ivp_buff.readLine();
				if (ivp_ver.length() < 7 || !ivp_ver.substring(0,7).equals(z390_ver.substring(0,7))){
					System.out.println("IVP z390 base install not equal z390.jar PTF version");
					ivp_rc = 8;
				}
        	} catch (Exception e){
        		System.out.println("IVP I/O error - " + e.toString());
        		ivp_rc = 16;
        	}
	    } else if (z390_base_ver.compareTo("V1.3.03") > 0) {
	    	System.out.println("IVP z390 base version file Z390.IVP not found");
	    	ivp_rc = 8;
	    }
	    if (j2se_ver.length() < 3 || j2se_ver.substring(0,3).compareTo("1.5") < 0){
	    	System.out.println("IVP J2SE 1.5+ required for z390");
	    	ivp_rc = 16;
	    }
	    /*
	     * check if RT optional installs ok
	     */
	    ivp_file_name = z390_dir + File.separator + "rt";
        ivp_file      = new File(ivp_file_name);
	    if (!ivp_file.isDirectory()){
	    	System.out.println("IVP RT optional  zip not installed");
	    	ivp_rc = 4;
	    } else {
	    	ivp_file_name = z390_dir + File.separator + "rt" + File.separator + "Z390.IVP";
	    	ivp_file      = new File(ivp_file_name);
	    	if (ivp_file.isFile()){
	    		try {
	    			ivp_buff = new BufferedReader(new FileReader(ivp_file_name));
	    			ivp_ver = ivp_buff.readLine();
	    			System.out.println("IVP z390 install = " + ivp_ver);
	    			if (ivp_ver.length() < 7 || !ivp_ver.substring(0,7).equals(z390_ver.substring(0,7))){
	    				System.out.println("IVP z390 RT.zip install not equal base version");
	    				ivp_rc = 8;
	    			} else {
	    				System.out.println("IVP RT   install = " + ivp_ver);
	    			}
	    		} catch (Exception e){
	    			System.out.println("IVP I/O error - " + e.toString());
	    			ivp_rc = 8;
	    		}
	    	}
	    }
	    /*
	     * check if MVS optional install ok
	     */
	    ivp_file_name = z390_dir + File.separator + "mvs";
        ivp_file      = new File(ivp_file_name);
	    if (!ivp_file.isDirectory()){
	    	System.out.println("IVP MVS optional  zip not installed");
	    	ivp_rc = 4;
	    } else {
	    	ivp_file_name = z390_dir + File.separator + "mvs" + File.separator + "Z390.IVP";
	    	ivp_file      = new File(ivp_file_name);
	    	if (ivp_file.isFile()){
	    		try {
	    			ivp_buff = new BufferedReader(new FileReader(ivp_file_name));
	    			ivp_ver = ivp_buff.readLine();
	    			System.out.println("IVP z390 install = " + ivp_ver);
	    			if (ivp_ver.length() < 7 || !ivp_ver.substring(0,7).equals(z390_ver.substring(0,7))){
	    				System.out.println("IVP z390 MVS.zip install not equal base version");
	    				ivp_rc = 8;
	    			} else {
	    				System.out.println("IVP MVS  install = " + ivp_ver);
	    			}
	    		} catch (Exception e){
	    			System.out.println("IVP I/O error - " + e.toString());
	    			ivp_rc = 8;
	    		}
	    	}
	    }
	    /*
	     * check return code and exit
	     */
	    if (ivp_rc > 0){
	    	if (ivp_rc > 4){
	    		System.out.println("IVP see errors above");
	    	} else {
	    		System.out.println("IVP see warnings above");
	    	}
	    }
        System.out.println("IVP exiting with RC = " + ivp_rc);
        System.exit(ivp_rc);
	}
}

