import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.lang.reflect.Array;
import java.nio.ByteBuffer;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Random;

import javax.swing.JTextArea;

public  class  lz390 {
   /*****************************************************
	
    z390 portable mainframe assembler and emulator.
	
    Copyright 2005 Automated Software Tools Corporation
	 
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

    lz390 is the linker component of z390 which can be called from
    z390 gui interface or from command line to read obj     relocatble object code files and generate single 390 load
    module file.  Both obj and 390 files are in ascii text     file format with hex codes for all binary data.

    ****************************************************
    * Maintenance
    ****************************************************
    * 04/15/05 copied from lz390.java and modified
    * 06/22/05 add AMODE and RMODE options
    * 08/17/05 add EXT AND ent SUPPORT
    * 08/22/05 add SYSOBJ, SYSLST, and SYS390 dir options
    ********************************************************
    * Global variables
    *****************************************************/
    String version = null;
    int lz390_rc = 0;
    int lz390_errors = 0;
    Date cur_date = new Date();
	long tod_start = cur_date.getTime();
    long tod_end   = 0;
    long tot_sec = 0;
    int tot_obj_bytes = 0;
    int tot_find_gbl_esd = 0;
    String pgm_name = null;
    static boolean load_esds_only = true;
    String obj_file_name = null;
    File obj_file = null;
    BufferedReader obj_file_buff = null;
    boolean obj_eod = false;
    RandomAccessFile z390_file = null;
    File lst_file = null;
    BufferedWriter lst_file_buff = null;
    String obj_line = null;
    boolean obj_eof = false;
    String  opt_parms = " ";
    boolean opt_ok = false;
    boolean opt_amode24  = false;
    boolean opt_amode31  = true;
    boolean opt_con      = true;
    boolean opt_list     = true;
    boolean opt_rmode24  = true;
    boolean opt_rmode31  = false;
    boolean opt_stats    = true;
    boolean opt_timing   = true;
    boolean opt_time     = true;
    boolean opt_trace    = false;
    boolean opt_traceall = false;
    boolean opt_trap     = true;
    SimpleDateFormat mmddyy = new SimpleDateFormat("MM/dd/yy");
    SimpleDateFormat hhmmss = new SimpleDateFormat("HH:mm:ss");
    boolean log_tod = true; 
    JTextArea z390_log_text = null;
    boolean lz390_aborted = false;
    /*
     * static limits
     */
    static int max_errors = 100;
    static int max_gbl_esd = 10000;
    static int max_obj_files = 1000;
    static int max_obj_esd = 1000;
    static int max_rld = 100000;
    long max_time_seconds = 15;     // max elapsed time
    /*
     * key search table data
     */
    static int max_key_root = 1023;
    int max_key_tab = max_key_root + max_gbl_esd;
    int tot_key_tab = max_key_root+1;
    int tot_key = 0;
    String key_text = null;
    int key_index = 0;
    int key_index_last = 0;
    Random key_rand = new Random();
    int key_hash = 0;
    int tot_key_search = 0;
    int tot_key_comp  = 0;
    int avg_key_comp  = 0;
    int cur_key_comp = 0;
    int max_key_comp = 0;
    String[]  key_tab_key   = new String[max_key_tab];
    int[]     key_tab_hash  = (int[])Array.newInstance(int.class,max_key_tab);
    int[]     key_tab_index = (int[])Array.newInstance(int.class,max_key_tab);
    int[]     key_tab_low   = (int[])Array.newInstance(int.class,max_key_tab);
    int[]     key_tab_high  = (int[])Array.newInstance(int.class,max_key_tab);
    /*
     * global ESD tables
     */
    long    tod_time_limit = 0;
    int     next_time_ins   = 0x1000;
    int     next_time_check = next_time_ins;
    int tot_gbl_esd = 0;
    int cur_gbl_esd = 0;
    int cur_gbl_ext = 0;
    String[]  gbl_esd_name = new String[max_gbl_esd];
    int[]     gbl_esd_loc  = (int[])Array.newInstance(int.class,max_gbl_esd);
    byte[]    gbl_esd_type = (byte[])Array.newInstance(byte.class,max_gbl_esd);
    static byte gbl_esd_ext = 0; // undefined ext
    static byte gbl_esd_ent = 1; // found cst/ent
    int loc_ctr = 0;
    /*
     * object files loaded
     */
    int tot_obj_files = 0;
    int cur_obj_file = 0;
    String[]  obj_file_names = new String[max_obj_files];
    /*
     * current obj esd table
     */
    boolean ext_found = false;
    int tot_obj_esd = 0;
    int cur_obj_esd = 0;
    int[]     obj_esd      = (int[])Array.newInstance(int.class,max_obj_esd);
    String[]  obj_esd_name = new String[max_obj_esd];
    int[]     obj_esd_loc  = (int[])Array.newInstance(int.class,max_obj_esd);
    int[]     obj_esd_len  = (int[])Array.newInstance(int.class,max_obj_esd);
    String[]  obj_esd_type = new String[max_obj_esd];
  /*
   * current obj esd to gbl_esd_loc index table
   */
    int[]    obj_gbl_esd = (int[])Array.newInstance(int.class,max_obj_esd);
  
  /*
   * load module header, code, and rld variables
   */  
   /*
    * 20 byte header with 5 fields as follows
    * offset  0 - 4 character format version
    * offset  4 - 4 character options as follows:
    *   1 - AMODE31 T/F - default T 
    *   2 - RMODE31 T/F - default F
    *   3 - RESERVED
    *   4 - RESERVED
    * offset  4 - full word length of code
    * offset  8 - full word entry offset
    * offset 12 - full word count of rlds

    */ 
    String z390_code_ver = "1002";
    String z390_flags = null;
    char   z390_amode31 = 'T';
    char   z390_rmode31 = 'F';
    /*
     * binary load module image in byte buffer
     */
    byte[] z390_code = null;
    ByteBuffer z390_code_buff = null;
    /*
     * rld entries in following format
     *   offset 0 full workd rld field offset
     *   offset 4 signed byte rld field len
     * (negative len means subtract the base address
     * versus adding it to rld field.)
     */
  /*
   * z390 load module rld table
   */  
    int tot_rld = 0;
    int[]     rld_loc = (int[])Array.newInstance(int.class,max_rld);
    byte[]    rld_len = (byte[])Array.newInstance(byte.class,max_rld);
  /*
   * current directory global variables
   */   
    String dir_cur = null;
    String dir_obj = null;
    String dir_lst = null;
    String dir_390 = null;
  /*
   * end of global lz390class data and start of procs
   */
public static void main(String[] args) {
  /*
   * main is entry when executed from command line
   * Create instance of lz390class and pass
   * parms to lz390 like z390 does.
   */
      lz390 pgm = new lz390();
      pgm.process_lz390(args,null);
}
public int process_lz390(String[] args,JTextArea log_text){
   /*
    *  link obj include files into single 390 load module
    *
    *  Note this may be called directly from z390 GUI or
    *  from main when lz370 run from windows command line.
    *  if called from main, the log_text balect will be null
    *  and local put_log function will route to console instead
    *  of the z390 log window.
    */
	    init_lz390(args,log_text);
    	if (opt_trap){
     	   try {
               resolve_esds();
               load_obj_code();
               gen_load_module();
     	   } catch (Exception e){
     		   abort_error(23,"internal system exception - " + e.toString());
     	   }
     	} else {
            resolve_esds();
            load_obj_code();
            gen_load_module();
     	}
	    exit_lz390();
	    if (log_text == null){
	    	System.exit(lz390_rc);
	    }
	    return lz390_rc;
}
private void init_lz390(String[] args, JTextArea log_text){
	/*
	 * 1.  initialize log routing
	 * 2.  set options
	 * 3.  compile regular expression parsers
	 * 4.  open bal and obj buffered I/O files
	 * 5.  Init ascii/ebcdic translation table
	 */
	    if  (log_text != null){
	    	z390_log_text = log_text;
	    }
        version = "V1.0.00 09/30/05";  //dsh
        process_args(args);
        set_options();
		open_files();
        put_copyright();
        tod_time_limit = max_time_seconds * 1000 + tod_start;
}
private void process_args(String[] args){
	/*
	 * process bal bal macs options
	 *  1.  Set dir_cur and dir_obj from
	 *      file name path if specified
	 */
    if  (args.length >= 1){
        set_dir_and_pgm_name(args[0]);
        if (args.length > 1){
           opt_parms = args[1];
           int index1 = 2;
           while (index1 < args.length){
              	opt_parms = opt_parms.concat(" " + args[index1]);
           	 	index1++;
           }
        }
    } else {
	    abort_error(1,"missing OBJ file");
    }
}
private void set_dir_and_pgm_name(String file_name){
	/*
	 * set program name and default directory
	 * for all files from first parm.
	 * Notes:
	 *   1.  Use current directory if not path
	 *       specified with program name.
	 *   2.  Options SYSOBJ, SYS390, and SYSLST
	 *       can override default directories.
	 */
	if (file_name.charAt(0) == '\"'   // strip lsn quotes
		|| file_name.charAt(0) == '\''){
		file_name = file_name.substring(1,file_name.length() - 1);
	}
    int index = file_name.lastIndexOf(File.separator);
    if (index != -1){  // get dir path if any
    	dir_cur = file_name.substring(0,index+1).toUpperCase();
    	file_name = file_name.substring(index + 1).toUpperCase();
    } else {
	  	dir_cur = System.getProperty("user.dir").toUpperCase() + File.separator;
	  	file_name = file_name.toUpperCase();
    }
	dir_obj = dir_cur;
	dir_lst = dir_cur;
	dir_390 = dir_cur;
    index = file_name.lastIndexOf("\\.");
    if (index != -1){  // strip extension if any
    	pgm_name = file_name.substring(0,index);
    } else {
    	pgm_name = file_name;
    }
}
private void exit_lz390(){
	/*
	 * display total errors
	 * close files and exit
	 */
	  if  (lz390_rc == 0 && lz390_errors > 0){
    	  lz390_rc = 16;
      }
  	  put_stats();
      close_files();
	  if    (lz390_aborted){
	    	System.exit(lz390_rc);
	  }
}
private void put_stats(){
	/*
	 * display statistics as comments at end of bal
	 */
	if (opt_stats){
	   put_log("Stats total obj files = " + tot_obj_files);
	   put_log("Stats total esds      = " + tot_gbl_esd);
	   put_log("Stats Keys            = " + tot_key);
	   put_log("Stats Key searches    = " + tot_key_search);
	   if (tot_key_search > 0){
	       avg_key_comp = tot_key_comp/tot_key_search;
	   }
	   put_log("Stats Key avg comps   = " + avg_key_comp);
	   put_log("Stats Key max comps   = " + max_key_comp);
	   put_log("Stats total obj bytes = " + tot_obj_bytes);
	   put_log("Stats total obj rlds  = " + tot_rld);
	   if (opt_timing){
	      cur_date = new Date();
	      tod_end = cur_date.getTime();
	      tot_sec = (tod_end - tod_start)/1000;
	      put_log("Stats total seconds         = " + tot_sec);
	   }
	}
	put_log("LZ390 total errors          = " + lz390_errors);
	put_log("LZ390 return code           = " + lz390_rc);
}
private void close_files(){
	  if (obj_file != null && obj_file.isFile()){
	  	  try {
	  	  	  obj_file_buff.close();
	  	  } catch (IOException e){
	  	  	  abort_error(3,"I/O error on obj close - " + e.toString());
	  	  }
	  }
	  if  (opt_list){
		  if (lst_file != null && lst_file.isFile()){
		  	  try {
		  	  	  lst_file_buff.close();
		  	  } catch (IOException e){
		  	  	  abort_error(4,"I/O error on lst close - " + e.toString());
		  	  }
		  }
	  }
}
private void log_error(int error,String msg){
	/*
	 * issue error msg to log with prefix and
	 * inc error total
	 * 1.  supress if not gen_obj and not trace
	 */
      put_log("lz390 error " + error + " " + msg);
	  lz390_errors++;
	  if (lz390_errors > max_errors){
	  	 abort_error(5,"max errors exceeded");	 
	  }
}
private void abort_error(int error,String msg){
	/*
	 * issue error msg to log with prefix and
	 * inc error total
	 */
	  lz390_errors++;
	  if (lz390_aborted){
	  	 System.exit(16);
	  }
	  lz390_aborted = true;
	  put_log("lz390 error " + error + " " + msg);
      exit_lz390();
}
private void put_copyright(){
	   /*
	    * display lz390 version, timestamp,
	    * and copyright if running standalone
	    */
	   	if  (opt_timing){
			cur_date = new Date();
	   	    put_log("lz390 " + version 
	   			+ " Current Date " +mmddyy.format(cur_date)
	   			+ " Time " + mmddyy.format(cur_date));
	   	} else {
	   	    put_log("lz390 " + version);
	   	}
	   	if  (z390_log_text == null){
	   	    put_log("Copyright 2005 Automated Software Tools Corporation");
	   	    put_log("z390 is licensed under GNU General Public License");
	   	}
	   	put_log("LZ390 program = " + dir_obj + pgm_name + ".OBJ");
	   	put_log("LZ390 options = " + opt_parms);
	   }
	   private synchronized void put_log(String msg) {
	   	/*
	   	 * Write message to z390_log_text or console
	   	 * if running standalone
	   	 * 
	   	 */
   	    	put_prn_line(msg);
	        if  (z390_log_text != null){
  	        	z390_log_text.append(msg + "\n");
   	        }
	        if (opt_con){
   	    	    System.out.println(msg);
   	        }
	   }
	   private void put_prn_line(String msg){
	   /*
	    * put line to listing file
	    */
	   	   if (opt_list || opt_trace){
	   	      try {
	   	          lst_file_buff.write(msg + "\r\n");
	   	      } catch (Exception e){
	   	          abort_error(6,"I/O error on LST listing file write");
	   	      }
	   	   }
	   }
private void open_files(){
	/*
	 * open 390 and lst files
	 */
       	if (opt_list){
            lst_file = new File(dir_lst + pgm_name + ".LST");
         	try {
       	       lst_file_buff = new BufferedWriter(new FileWriter(lst_file));
       	    } catch (IOException e){
       		   abort_error(9,"I/O error on lst open - " + e.toString());
       	    }
       	}
}
private void set_options(){
	/*
	 * parse and set options
	 */
        String[] tokens = opt_parms.toUpperCase().split("\\s+");
        int index1 = 0;
        while (index1 < tokens.length){
            if (tokens[index1].equals("CON")){
            	opt_con = true;
            } else if (tokens[index1].equals("AMODE24")){
        		opt_amode24 = true;
        		opt_amode31 = false;
        		z390_amode31 = 'F';
        	}
           	if (tokens[index1].equals("AMODE31")){
        		opt_amode24 = false;
        		opt_amode31 = true;
        		z390_amode31 = 'T';
        	}
        	if (tokens[index1].equals("NOCON")){
        		opt_con = false;
        	}
            if (tokens[index1].equals("NOLIST")){
            	opt_list = false;
            } else if (tokens[index1].equals("NOSTATS")){
            	opt_stats = false;
            } else if (tokens[index1].equals("NOTIME")){
            	opt_time = false;
            } else if (tokens[index1].equals("NOTIMING")){
            	opt_timing = false;
            } else if (tokens[index1].length() > 7 
            		&& tokens[index1].substring(0,7).equals("SYS390(")){
            	dir_390 = tokens[index1].substring(7,tokens[index1].length()-1) + File.separator;
            } else if (tokens[index1].length() > 7 
            		&& tokens[index1].substring(0,7).equals("SYSLST(")){
            	dir_lst = tokens[index1].substring(7,tokens[index1].length()-1) + File.separator;
            } else if (tokens[index1].length() > 7 
            		&& tokens[index1].substring(0,7).equals("SYSOBJ(")){
            	dir_obj = tokens[index1].substring(7,tokens[index1].length()-1) + File.separator;
            } else if (tokens[index1].equals("NOTRAP")){
            	opt_trap = false;
            } else if (tokens[index1].length() > 5
            		&& tokens[index1].substring(0,5).equals("TIME(")){
            	max_time_seconds = Long.valueOf(tokens[index1].substring(5,tokens[index1].length()-1)).longValue();
            	if (tokens[index1].equals("RMODE24")){
            		opt_rmode24 = true;
            		opt_rmode31 = false;
            		z390_rmode31 = 'F';
            	}
               	if (tokens[index1].equals("RMODE31")){
            		opt_rmode24 = false;
            		opt_rmode31 = true;
            		z390_rmode31 = 'T';
            	}
            } else if (tokens[index1].equals("TRACE")){
            	opt_trace = true;
            } else if (tokens[index1].equals("TRACEALL")){
            	opt_traceall = true;
            	opt_trace = true;
            }
            index1++;
        }
        if (dir_obj.equals(".")){
        	dir_obj = dir_cur;
        }
   }
private void resolve_esds(){
	/*
	 * 1. load primary obj with any include
	 *    and name commands to define initial
	 *    list of obj files to load.
	 * 2. load all explicit include obj files
	 *    to get initial list of external esds
	 * 3. search and load obj files for extrns
	 *    until all found or no more can be resolved 
	 */
	obj_file_name = dir_obj + pgm_name + ".OBJ"; // set primary
	if (load_obj_file(load_esds_only)){
	   add_gbl_esds();
       while (find_ext_file()){ 
    	  if (load_obj_file(load_esds_only)){
             add_gbl_esds();
          }
   	   }
       cur_gbl_ext = 1;
       while (cur_gbl_ext <= tot_gbl_esd){
    	   if (gbl_esd_type[cur_gbl_ext] == gbl_esd_ext){
    	      log_error(27,"unresolved external reference - " + gbl_esd_name[cur_gbl_ext]);
    	   }
    	   cur_gbl_ext++;
       }
    }
}
private boolean load_obj_file(boolean esds_only){
	/*
	 * load object file esds only or entire file
	 * using obj_file_name and
	 * return true if successful
	 */
    obj_file = new File(obj_file_name);
	    try {
	        obj_file_buff = new BufferedReader(new FileReader(obj_file));
	    } catch (IOException e){
		    abort_error(11,"I/O error on obj open - " + e.toString());
	        return false;
	    }
    if (opt_trace){
  	  	 put_log("TRACE LOADING OBJ FILE - " + obj_file_name);
    }
    tot_obj_esd = 0;
    obj_eod = false;
	get_obj_esd_line();
	if (esds_only && !obj_line.substring(0,4).equals(".ESD")){
		obj_eod = true;
	}
	while (!obj_eod
			&& tot_obj_esd < max_obj_esd){
		if (opt_trace){
			put_log("  LOADING " + obj_line);
		}
		if (obj_line.substring(0,4).equals(".END")){
			obj_eod = true;
		} else if (obj_line.substring(0,4).equals(".ESD")){
			tot_obj_esd++;
			obj_esd[tot_obj_esd] = Integer.valueOf(obj_line.substring(9,13),16).intValue();
			obj_esd_name[tot_obj_esd] = obj_line.substring(54);
			obj_esd_loc[tot_obj_esd]  = Integer.valueOf(obj_line.substring(18,26),16).intValue();
			obj_esd_len[tot_obj_esd]  = Integer.valueOf(obj_line.substring(31,39),16).intValue();
			obj_esd_type[tot_obj_esd] = obj_line.substring(45,48);
			if (!esds_only
				&& (obj_esd_type[tot_obj_esd].equals("CST")
				 || obj_esd_type[tot_obj_esd].equals("EXT"))){
				if (find_gbl_esd(obj_esd_name[tot_obj_esd])){
					obj_gbl_esd[obj_esd[tot_obj_esd]] = cur_gbl_esd;
				}
			}
		} else if (obj_line.substring(0,4).equals(".TXT")){
			int    obj_text_esd = Integer.valueOf(obj_line.substring(9,13),16).intValue();
			int    obj_text_loc = Integer.valueOf(obj_line.substring(18,26),16).intValue();
			int    obj_text_len = Integer.valueOf(obj_line.substring(31,33),16).intValue();
			String obj_text = obj_line.substring(34,34 + 2 * obj_text_len);
			int code_off = gbl_esd_loc[obj_gbl_esd[obj_text_esd]] + obj_text_loc;
			if (code_off > loc_ctr){
				abort_error(25,"invalid object code offset - " + obj_line);
				return false;
			} 
			z390_code_buff.position(code_off);
			int index = 0;
			while (index < 2 * obj_text_len){
				z390_code_buff.put((byte) Integer.valueOf(obj_text.substring(index,index+2),16).intValue());
				index = index + 2;
			}
			tot_obj_bytes = tot_obj_bytes + obj_text_len;
		} else if (obj_line.substring(0,4).equals(".RLD")){
			int  obj_rld_esd = Integer.valueOf(obj_line.substring(9,13),16).intValue();
			int  obj_rld_loc = Integer.valueOf(obj_line.substring(18,26),16).intValue();
			byte obj_rld_len = (byte) Integer.valueOf(obj_line.substring(31,32),16).intValue();
			char obj_rld_sgn = obj_line.charAt(38);
			int  obj_rld_xesd = Integer.valueOf(obj_line.substring(45,49),16).intValue();
			int rld_off = 0;
			int rld_fld = 0;
			if (tot_rld < max_rld){
				rld_loc[tot_rld] = obj_rld_loc;
				if (obj_rld_sgn == '+'){
					rld_len[tot_rld] = obj_rld_len;
				} else {
					rld_len[tot_rld] = (byte)- obj_rld_len;
				}
				if (gbl_esd_loc[obj_gbl_esd[obj_rld_xesd]] > 0){
					switch (obj_rld_len){
					    case 3:
					    	rld_off = gbl_esd_loc[obj_gbl_esd[obj_rld_esd]] + obj_rld_loc;
					    	rld_fld = z390_code_buff.getInt(rld_off);
					        int rld_save_byte = rld_fld & 0xff;
					    	rld_fld = rld_fld/256;
					    	if (obj_rld_sgn == '+'){
					    	    rld_fld = rld_fld + gbl_esd_loc[obj_gbl_esd[obj_rld_xesd]];
					    	} else {
						    	rld_fld = rld_fld - gbl_esd_loc[obj_gbl_esd[obj_rld_xesd]];
					    	}
					    	rld_fld = (rld_fld << 8) | rld_save_byte;
					    	z390_code_buff.putInt(rld_off,rld_fld);
					    	break;
					    case 4:
					    	rld_off = gbl_esd_loc[obj_gbl_esd[obj_rld_esd]] + obj_rld_loc;
					    	rld_fld = z390_code_buff.getInt(rld_off);
					    	if (obj_rld_sgn == '+'){
					    	    rld_fld = rld_fld + gbl_esd_loc[obj_gbl_esd[obj_rld_xesd]];
					    	} else {
						    	rld_fld = rld_fld - gbl_esd_loc[obj_gbl_esd[obj_rld_xesd]];
					    	}
					    	z390_code_buff.putInt(rld_off,rld_fld);
					    	break;
					}
				}
				tot_rld++;
			} else {
				abort_error(21,"z390 rld table exceeded");
			}
		} else {
			abort_error(20,"unknown obj record type - " + obj_line);
		}
        get_obj_esd_line();
		if ((esds_only && !obj_line.substring(0,4).equals(".ESD"))
			|| obj_line.substring(0,4).equals(".END")){
			obj_eod = true;
		}
	}
	try {
	    obj_file_buff.close();
	    if (tot_obj_files < max_obj_files){
	    	if (esds_only){
	    	   obj_file_names[tot_obj_files] = obj_file_name;
	    	   tot_obj_files++;
	    	}
	    } else {
	    	abort_error(18,"maximum obj files exceeded");
	    }
	    return true;
	} catch (IOException e){
		abort_error(13,"I/O error on BAL file close " + e.toString());
		return false;
	}
}
private void get_obj_esd_line(){
	/*
	 * get next esd line from obj file else 
	 * set obj_eod
	 */
	try {
		obj_line = obj_file_buff.readLine();
	} catch (IOException e){
		abort_error(14,"I/O error on obj read - " + e.toString());
	}
}
private void add_gbl_esds(){
	/*
	 * add any new global esds found
	 */
	int obj_index1 = 1;
	while (obj_index1 <= tot_obj_esd){
		if (tot_gbl_esd >= max_gbl_esd){
			abort_error(15,"maximum global ESDS exceeded");
		}
		if (obj_esd_type[obj_index1].equals("CST")){
            add_gbl_cst(obj_index1);
		} else if (obj_esd_type[obj_index1].equals("EXT")){
			add_gbl_ext(obj_index1);
		} else if (obj_esd_type[obj_index1].equals("ENT")){
            add_gbl_ent(obj_index1);
		}        
		obj_index1++;
	}
}
private void add_gbl_cst(int obj_index1){
	/*
	 * add obj cst to gbl table
	 */
	boolean esd_ok = true;
	if (find_gbl_esd(obj_esd_name[obj_index1])){
		if (gbl_esd_type[cur_gbl_esd] != gbl_esd_ext){
		    esd_ok = false;
			log_error(16,"ignoring duplicate CSECT - " + obj_esd_name[obj_index1]);
		}
	} else {
		tot_gbl_esd++;
		cur_gbl_esd = tot_gbl_esd;
		add_key_index(cur_gbl_esd);
	}
	if  (esd_ok){
		gbl_esd_name[cur_gbl_esd] = obj_esd_name[obj_index1];
		gbl_esd_loc[cur_gbl_esd] = loc_ctr;
		loc_ctr = loc_ctr + obj_esd_len[obj_index1];
		gbl_esd_type[cur_gbl_esd] = gbl_esd_ent;
	}
}
private void add_gbl_ext(int obj_index1){
	/*
	 * add ext ref to gbl table
	 */
	if (!find_gbl_esd(obj_esd_name[obj_index1])){
		tot_gbl_esd++;
		add_key_index(tot_gbl_esd);
		gbl_esd_name[tot_gbl_esd] = obj_esd_name[obj_index1];
		gbl_esd_type[tot_gbl_esd] = gbl_esd_ext;
	}
}
private void add_gbl_ent(int obj_index1){
	/*
	 * add obj entry to gbl table
	 */
	boolean esd_ok = true;
	if (find_gbl_esd(obj_esd_name[obj_index1])){
		if (gbl_esd_type[cur_gbl_esd] != gbl_esd_ext){
		    esd_ok = false;
			log_error(17,"ignoring duplicate ENTRY - " + obj_esd_name[obj_index1]);
		}
	} else {
		tot_gbl_esd++;
		cur_gbl_esd = tot_gbl_esd;
		add_key_index(cur_gbl_esd);
	}
	if  (esd_ok){
		esd_ok = false;				}
		int gbl_ent_esd = cur_gbl_esd;
		int obj_index2 = 1;
		while (obj_index2 <= tot_obj_esd){
			if (obj_esd[obj_index2] == obj_esd[obj_index1]
			    && obj_esd_type[obj_index2].equals("CST")){
				if (find_gbl_esd(obj_esd_name[obj_index2])){
				   esd_ok = true;
				   gbl_esd_loc[gbl_ent_esd] = obj_esd_loc[obj_index1] - obj_esd_loc[obj_index2] + gbl_esd_loc[cur_gbl_esd];
				   gbl_esd_type[gbl_ent_esd] = gbl_esd_ent;
				   obj_index2 = tot_obj_esd;
				}
			}
			obj_index2++;
		}
		if (!esd_ok){
			log_error(26,"entry csect not found for - " + obj_esd_name[obj_index1]);
		}
    }
private boolean find_gbl_esd(String esd_name){
	/*
	 * set cur_gbl_esd to entry for esd_name
	 * else return false
	 * abort if time exceeded
	 */
	  tot_find_gbl_esd++;
	  if (opt_time
			&& (tot_find_gbl_esd > next_time_check)){
			next_time_check = tot_find_gbl_esd + next_time_ins;
			cur_date = new Date();
			tod_end = cur_date.getTime();
			if (tod_end > tod_time_limit){
               abort_error(24,"time limit exceeded");
			}
		}
	    cur_gbl_esd = find_key_index("G:" + esd_name);
	    if (cur_gbl_esd != -1){
	    	return true;
	    } else {
	    	return false;
	    }
}
private boolean find_ext_file(){
	/*
	 * find next external esds file
	 * to load else return false
	 */
	cur_gbl_ext++;
	while (cur_gbl_ext <= tot_gbl_esd){
		if (gbl_esd_type[cur_gbl_ext] == gbl_esd_ext){
			obj_file_name = dir_obj + gbl_esd_name[cur_gbl_ext] + ".OBJ";
			obj_file = new File(obj_file_name);
			if (obj_file.isFile()){
			   return true;
			}
		}
		cur_gbl_ext++;
	}
	return false;
}
private void load_obj_code(){
	/*
	 * load all object code from files
	 * and build load module rlds
	 */
	if (loc_ctr > 0){
		z390_code = new byte[loc_ctr];
		z390_code_buff = ByteBuffer.wrap(z390_code,0,loc_ctr);
	} else {
		abort_error(19,"no csects defined for load module");
	}
	cur_obj_file = 0;
	while (cur_obj_file < tot_obj_files){
		obj_file_name = obj_file_names[cur_obj_file];
		load_obj_file(!load_esds_only);
		cur_obj_file++;
	}
}
private void gen_load_module(){
	/*
	 * output 390 load module in ascii hex format
	 */
	try {
        z390_file = new RandomAccessFile(dir_390 + pgm_name + ".390","rw");
        z390_file.setLength(0);
        z390_file.seek(0);
        z390_file.writeBytes(z390_code_ver);
        z390_flags = "" + z390_amode31 + z390_rmode31 + "??";
        z390_file.writeBytes(z390_flags);
        z390_file.writeInt(loc_ctr);
        z390_file.writeInt(0);
        z390_file.writeInt(tot_rld);
        z390_file.write(z390_code,0,loc_ctr);
        int cur_rld = 0;
        while (cur_rld < tot_rld){
        	z390_file.writeInt(rld_loc[cur_rld]);
        	z390_file.write(rld_len[cur_rld]);
        	cur_rld++;
        }
        z390_file.close();
	} catch (Exception e){
	 	abort_error(22,"I/O error on z390 load module file - " + e.toString());
	}
}
private int find_key_index(String user_key){
	/*
	 * return user_key_index for user_key else -1
	 * and set following for possible add_key_index:
	 *    1.  key_text = user_key
	 *    2.  key_hash = hash code for key
	 *    3.  key_index_last = last search entry
	 */
	tot_key_search++;
	key_text = user_key;
    key_rand.setSeed((long) key_text.hashCode());
    key_hash  = key_rand.nextInt();
    key_index = key_rand.nextInt(max_key_root)+1;
	if (key_tab_key[key_index] == null){
		key_index_last = key_index;
		return -1;
	}
    cur_key_comp = 0;
	while (key_index > 0){
		tot_key_comp++;
		cur_key_comp++;
		if (cur_key_comp > max_key_comp){
			max_key_comp = cur_key_comp;
		}
		if (opt_traceall == true){
			put_log("TRACE KEY SEARCHS=" + tot_key_search + " MAX DEPTH=" + max_key_comp + " COMPS=" + tot_key_comp);
		}
		if (key_hash == key_tab_hash[key_index]
		    && user_key.equals(key_tab_key[key_index])){			
			key_index_last = -1;
	    	return key_tab_index[key_index];
	    }
		key_index_last = key_index;
		if (key_hash < key_tab_hash[key_index]){
		    key_index = key_tab_low[key_index];
		} else {
			key_index = key_tab_high[key_index];
		}
	}
	return -1;
}
private void add_key_index(int user_index){
	/*
	 * add user_index entry based on
	 * key_text, key_hash, and key_index_last
	 * set by prior find_key_index
	 * 
	 */
	if (key_tab_key[key_index_last] == null){
		key_index = key_index_last;
	} else {
		if (tot_key_tab < max_key_tab){
			key_index = tot_key_tab;
			tot_key_tab++;
		} else {
			abort_error(87,"key search table exceeded");
		}
		if (key_hash < key_tab_hash[key_index_last]){
		    key_tab_low[key_index_last] = key_index;
	    } else {
		    key_tab_high[key_index_last] = key_index;
	    }
	}
	if (opt_traceall == true){
		put_log("TRACE KEY ADD LAST INDEX =" + key_index_last + " NEW INDEX=" + key_index + " KEY=" + key_text);
	}
	tot_key++;
	key_tab_key[key_index]   = key_text;
	key_tab_hash[key_index]  = key_hash;
	key_tab_index[key_index] = user_index;
}
/*
 *  end of lz390code 
 */
}