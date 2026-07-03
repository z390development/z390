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

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;


/**
 * iz390 is the IVP component of z390
 */
public class iz390{



/**
 * Dummy constructor - no initialization needed
 */
public iz390()
       {// dummy constructor - no initialization needed.
        }



/**
 * iz390 is a standalone  installation verification
 * program which displays current version of the 
 * following components:
 * <ol>
 *  <li>z390.jar - version from tz390</li>
 *  <li>OS from  - os.name  J2SE property</li>
 * </ol>
 *
 * @param argv argument string - same as z390
 */
	public static void main(String argv[]) {
       /*
	    ****************************************************
	    * Maintenance
	    ****************************************************
	    * 05/06/07 initial coding for v1303b 
	    * 07/12/07 RPI  656 change RT to MVS if MVS not installed
	    * 10/20/07 RPI  713 change file name case for Linux
	    * 09/25/09 RPI 1080 replace init_tables with init_tz390
        * 2025-09-14 AFK  Add/fix javadoc comments
        * 2026-07-03 AFK  #857 IVP behaviour inconsistent
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
        /*                                                                              // #857
         * when this program runs - the jar must be okay!                               // #857
         */                                                                             // #857
        System.out.println("IVP z390   assembler operational");                         // #857
        /*                                                                              // #857
         * check if zCobol install is ok                                                // #857
         */                                                                             // #857
        ivp_file_name = z390_dir + File.separator + "zcobol" + File.separator + "lib";  // #857
        ivp_file      = new File(ivp_file_name);                                        // #857
        if (!ivp_file.isDirectory()){                                                   // #857
            System.out.println("IVP zCobol component not installed");                   // #857
            ivp_rc = 4;                                                                 // #857
        } else {                                                                        // #857
            ivp_file_name = ivp_file_name + File.separator + "ZC390LIB.390";            // #857
            ivp_file      = new File(ivp_file_name);                                    // #857
            if (!ivp_file.isFile()){                                                    // #857
                System.out.println("IVP zCobol component not built");                   // #857
                ivp_rc = 4;                                                             // #857
            } else {                                                                    // #857
                System.out.println("IVP zCobol component operational");                 // #857
            }                                                                           // #857
        }                                                                               // #857
        /*                                                                              // #857
         * check if zCics install is ok                                                 // #857
         */                                                                             // #857
        ivp_file_name = z390_dir + File.separator + "cics";                             // #857
        ivp_file      = new File(ivp_file_name);                                        // #857
        if (!ivp_file.isDirectory()){                                                   // #857
            System.out.println("IVP zCics  component not installed");                   // #857
            ivp_rc = 4;                                                                 // #857
        } else {                                                                        // #857
            ivp_file_name = ivp_file_name + File.separator + "Z390KCP.390";             // #857
            ivp_file      = new File(ivp_file_name);                                    // #857
            if (!ivp_file.isFile()){                                                    // #857
                System.out.println("IVP zCics  component not built");                   // #857
            //  ivp_rc = 4;       // **!! Treat as normal condition                     // #857
            } else {                                                                    // #857
                System.out.println("IVP zCics  component operational");                 // #857
            }                                                                           // #857
        }                                                                               // #857
        /*                                                                              // #857
         * check if zSort install is ok                                                 // #857
         */                                                                             // #857
        ivp_file_name = z390_dir + File.separator + "linklib";                          // #857
        ivp_file      = new File(ivp_file_name);                                        // #857
        if (!ivp_file.isDirectory()){                                                   // #857
            System.out.println("IVP zSort  component not installed");                   // #857
            ivp_rc = 4;                                                                 // #857
        } else {                                                                        // #857
            ivp_file_name = ivp_file_name + File.separator + "SORT.390";                // #857
            ivp_file      = new File(ivp_file_name);                                    // #857
            if (!ivp_file.isFile()){                                                    // #857
                System.out.println("IVP zSort  component not built");                   // #857
                ivp_rc = 4;                                                             // #857
            } else {                                                                    // #857
                System.out.println("IVP zSort  component operational");                 // #857
            }                                                                           // #857
        }                                                                               // #857
        /*                                                                              // #857
         * zVSAM v1 built into z390 - always operational!                               // #857
         */                                                                             // #857
        System.out.println("IVP zVSAM  component operational");                         // #857
        /*                                                                              // #857
         * check if RT suite is available                                               // #857
         */                                                                             // #857
        ivp_file_name = z390_dir + File.separator + "z390test";                         // #857
        ivp_file      = new File(ivp_file_name);                                        // #857
        if (!ivp_file.isDirectory()){                                                   // #857
            System.out.println("IVP Regr. test suite not installed");                   // #857
            ivp_rc = 4;                                                                 // #857
        } else {                                                                        // #857
            ivp_file_name = ivp_file_name + File.separator + "gradlew.bat";             // #857
            ivp_file      = new File(ivp_file_name);                                    // #857
            if (!ivp_file.isFile()){                                                    // #857
                System.out.println("IVP Regr. test suite not built");                   // #857
                ivp_rc = 4;                                                             // #857
            } else {                                                                    // #857
                System.out.println("IVP Regr. test suite operational");                 // #857
            }                                                                           // #857
        }                                                                               // #857
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
