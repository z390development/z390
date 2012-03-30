import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.lang.reflect.Array;
import java.nio.ByteBuffer;
import java.text.SimpleDateFormat;
import java.util.Arrays;
import java.util.Date;

import javax.swing.JTextArea;

public  class  lz390 {
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

    lz390 is the linker component of z390 which can be called from
    z390 GUI interface or from command line to read obj     relocatble object code files and generate single 390 load
    module file.  Both obj and 390 files are in ascii text     file format with hex codes for all binary data.

    ****************************************************
    * Maintenance
    ****************************************************
    * 04/15/05 copied from lz390.java and modified
    * 06/22/05 add AMODE and RMODE options
    * 08/17/05 add EXT AND ent SUPPORT
    * 08/22/05 add SYSOBJ, SYSLST, and SYS390 dir options
    * 10/04/05 RPI5 - option ASCII use ASCII vs EBCDIC
    * 10/04/05 RPI6 - option ERR(nn) limit errors
    * 10/17/05 RPI25 - change TRACE to TRACEL option
    * 10/18/05 RPI29 - use LZ390E and LZ390E prefixes
    * 10/19/05 RPI32 - add SYSOBJ dir list support
    * 11/28/05 RPI113 file path with drive: and no separator
    * 12/07/05 RPI123 fix support for mult. paths
    * 12/15/05 RPI135 use tz390 shared tables (keys)
    * 12/23/05 RPI127 remove user mlc type from file name
    *          and use shared set_pgm_dir_name_type
    * 12/23/05 RPI131 limit file output to maxfile(mb)
    * 01/21/06 RPI182 add support for WXTRN (see TESTWXT1)
    * 01/25/06 RPI128 read binary or hex obj file
    * 01/26/06 RPI191 correct setting of RMODE/AMODE
    * 01/26/06 RPI 172 move options to tz390
    * 02/21/06 RPI 208 use tz390.z390_abort to term
    * 03/16/06 RPI 240 correct rld loc for mult csect rlds
    * 04/05/06 RPI 270 support RLD length of 8
    * 04/12/06 RPI 244 correct to use first dir on SYS390 for output
    * 04/28/06 RPI 301 validate obj esd's
    * 05/09/06 RPI 312 add pgm name to return code msg
    * 07/20/06 RPI 378 correct to use first SYSOBJ and SYS390
    * 07/26/06 RPI 385 show ESD CSECT's on LST and ignore 0 length
    * 09/18/06 RPI 459 prevent trap if ENTRY not found (OBJ error)
    * 09/20/06 RPI 453 only route stats to BAL, copyright+rc to con
    * 10/19/06 RPI 483 prevent trap and issue error if obj truncated
    * 11/04/06 RPI 484 add TRL trace file support for TRACEL and TRACEALL
    * 11/28/06 RPI 500 use system newline for Win/Linux
    * 07/06/07 RPI 646 synchronize abort_error to prevent other task abort errors
    * 07/15/07 RPI 656 change error message 19 to indicate missing code
    * 07/21/07 RPI 659 add LZ390I to stats on LST
    * 10/15/07 RPI 719 support LOG(file) override of log, trace, err files
    * 10/08/07 RPI 732 add support for .LNK command input file
    *          with INCLUDE, ALIAS, ENTRY, NAME commands
    * 11/10/07 RPI 735 change LNK to LKD 
    *          assign EXTRN with matching OBJ to start if no CSECT/ENTRY
    * 11/12/07 RPI 737 add STATS(file) option 
    * 12/24/07 RPI 759 align stats for all pgms on STA file 
    * 12/25/07 RPI 755 cleanup msgs to log, sta, tr*, CON 
    * 12/27/07 RPI 770 don't search for EXTRN's if NOAUTOLINK 
    * 02/28/08 RPI 814 default search of obj+linklib for AUTOLINK 
    * 03/27/08 RPI 827 show obj file name on I/O errors      
    * 06/23/08 RPI 866 use get_file_name to parse LST and ALIAS 390 file names
    * 07/29/08 RPI 883 add MOD support for code.MOD
    *          with no header/trailer/rlds and no rounding 
    * 08/12/08 RPI 894 support RLD 2 byte fields in 390's upt to 64k 
    * 09/16/08 RPI 908 trap error on SYSLST file output override
    * 10/24/08 RPI 935 prevent recursive abort  
    * 11/09/08 RPI 946 init 390 load module to x'F6' if option INIT (NOINIT sets x'00') 
    * 05/28/09 RPI 1046 correct trace display of AL3 and reset rld_cnt for each NAME cmd
    * 06/10/09 RPI 1051 add LZ390I include = file spec for use by ZPARTRS  
    * 07/11/09 RPI 1062 set RC=12 for errors RC=16 for abort
    * 07/18/09 RPI 1062 change abort msg from error to abort
    * 08/24/09 RPI 1069 add CODEPAGE(ascii+ebcdic+LIST) option
    * 09/26/09 RPI 1080 replace init_tables with init_tz390
    * 07/30/11 RPI 1175 use tz390.check_java_version()
    * 03/07/12 RPI 1197 support OBJ optional entry on .END TXT
    *          1) last .END entry overrides default 0
    *          2) ENTRY command overrides any .END entry 
    ********************************************************
    * Global variables                    (last RPI)
    *****************************************************/
    tz390 tz390 = null;
    String msg_id = "LZ390I ";
    int lz390_rc = 0;
    int lz390_errors = 0;
    boolean lz390_recursive_abort = false; // RPI 935
    Date cur_date = new Date();
	long tod_start = cur_date.getTime();
    long tod_end   = 0;
    long tot_sec = 0;
    int tot_obj_bytes = 0;
    int tot_find_gbl_esd = 0;
    boolean load_esds_only = true;
    int max_obj_esd = 0;
    String lkd_file_name = null;      // RPI 732
    RandomAccessFile lkd_file = null; // RPI 732
    String lkd_cmd = null;            // RPI 732
    String lkd_op = null;
    String lkd_parm = null;
    String lkd_entry = null;          // RPI 732
    int    lkd_entry_loc = -1;         // RPI 732
    String lkd_alias = null;          // RPI 732
    String lkd_name  = null;          // RPI 732
    String obj_file_name = null;
    boolean obj_file_bin = false;
    byte    obj_bin_id   = 0x02;
    RandomAccessFile obj_file = null;
    boolean obj_eod = false;
    int obj_end_entry_esd = 0;      // RPI 1197 cur  .END entry esd or 0
    int obj_end_entry_loc = 0;      // RPI 1197 cur  .END entry esd offset
    int obj_end_entry_last_loc = 0; // RPI 1197 last .END entry load module offset
    RandomAccessFile z390_file = null;
    File lst_file = null;
    BufferedWriter lst_file_buff = null;
    int tot_name  = 0;  // RPI 724
    int max_alias = 10;
    int tot_alias = 0;
    String alias_name[] = new String[10];
    File alias_file = null;
    String alias_file_name = null;
    BufferedWriter alias_file_buff = null;
    String obj_line = null;
    boolean obj_eof = false;
    SimpleDateFormat mmddyy = new SimpleDateFormat("MM/dd/yy");
    SimpleDateFormat hhmmss = new SimpleDateFormat("HH:mm:ss");
    boolean log_tod = true; 
    JTextArea z390_log_text = null;
    /*
     * global ESD tables
     */
    long    tod_time_limit = 0;
    int     next_time_ins   = 0x1000;
    int     next_time_check = next_time_ins;
    int tot_csect   = 0;
    int tot_entry   = 0;
    int tot_missing_wxtrn = 0;
    int tot_gbl_esd = 0;
    int cur_gbl_esd = 0;
    int cur_gbl_ext = 0;
    String[]  gbl_esd_name = null;
    int[]     gbl_esd_loc  = null;
    byte[]    gbl_esd_type = null;
    byte gbl_esd_ext = 0; // undefined ext
    byte gbl_esd_ent = 1; // found cst/ent
    byte gbl_esd_wxt = 2; // undefined wxt
    int loc_ctr = 0;
    int mod_loc_ctr = 0; // RPI 883
    /*
     * object files loaded
     */
    int tot_obj_files = 0;
    int cur_obj_file = 0;
    String[]  obj_file_names = null;
    /*
     * binary object file data
     */
    byte[] bin_byte = new byte[80];
    ByteBuffer bin_byte_buff = ByteBuffer.wrap(bin_byte,0,80);
    /*
     * current obj esd table
     */
    boolean ext_found = false;
    int tot_obj_esd = 0;
    int cur_obj_esd = 0;
    int[]     obj_esd      = null;
    String[]  obj_esd_name = null;
    int[]     obj_esd_loc  = null;
    int[]     obj_esd_len  = null;
    String[]  obj_esd_type = null;
  /*
   * current obj esd to gbl_esd_loc index table
   */
    int[]    obj_gbl_esd = null;
  
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
    * offset  8 - full word length of code
    * offset 12 - full word entry offset
    * offset 16 - full word count of rlds

    */ 
    String z390_code_ver = "1002";
    String z390_flags = null;
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
    int[]     rld_loc = null;
    byte[]    rld_len = null;
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
    *  link 1 or more obj files into single 390 load module
    *
    *  Note this may be called directly from z390 GUI or
    *  from main when lz370 run from windows command line.
    *  if called from main, the log_text will be null
    *  and local put_log function will route to console instead
    *  of the z390 log window.
    */
	    init_lz390(args,log_text);
    	if (tz390.opt_trap){
     	   try {
     		   process_lkd_cmds(); // RPI 732
               if (tot_name == 0){
            	   resolve_esds();
            	   load_obj_code();
            	   gen_load_module();
               }
     	   } catch (Exception e){
     		   abort_error(23,"internal system exception - " + e.toString());
     	   }
     	} else {
 		    process_lkd_cmds(); // RPI 732
            if (tot_name == 0){
            	resolve_esds();
            	load_obj_code();
            	gen_load_module();
            }
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
	 */
	    if  (log_text != null){
	    	z390_log_text = log_text;
	    }
	    tz390 = new tz390();
	    tz390.init_tz390();  // RPI 1080
    	if (!tz390.check_java_version()){ // RPI 1175
    		abort_error(41,"unknown java version "
    	    + tz390.java_vendor + " " + tz390.java_version);  
    	}
        tz390.init_options(args,".OBJ");
		tz390.open_systerm("LZ390");
		tz390.init_codepage(tz390.codepage); // RPI 1069
        open_files();
		tz390.force_nocon = true;   // RPI 755
		put_log(tz390.started_msg); // RPI 755
		tz390.force_nocon = false;  // RPI 755
        put_copyright();
        init_arrays();
        tod_time_limit = tz390.max_time_seconds * 1000 + tod_start;
}
private void init_arrays(){
	/*
	 * initialize arrays using opt_max??? limits
	 */
	/*
	 * opt_maxesd - external symbol definitions
	 */
    obj_esd      = (int[])Array.newInstance(int.class,tz390.opt_maxesd);
    obj_esd_name = new String[tz390.opt_maxesd];
    obj_esd_loc  = (int[])Array.newInstance(int.class,tz390.opt_maxesd);
    obj_esd_len  = (int[])Array.newInstance(int.class,tz390.opt_maxesd);
    obj_esd_type = new String[tz390.opt_maxesd];
    obj_gbl_esd = (int[])Array.newInstance(int.class,tz390.opt_maxesd);
    gbl_esd_name = new String[tz390.opt_maxesd];
    gbl_esd_loc  = (int[])Array.newInstance(int.class,tz390.opt_maxesd);
    gbl_esd_type = (byte[])Array.newInstance(byte.class,tz390.opt_maxesd);
    /*
	 * opt_maxfile - totoal object files
	 */
    obj_file_names = new String[tz390.opt_maxfile];
    /*
	 * opt_maxrld - relocation definitions
	 */
    rld_loc = (int[])Array.newInstance(int.class,tz390.opt_maxrld);
    rld_len = (byte[])Array.newInstance(byte.class,tz390.opt_maxrld);
}
private void exit_lz390(){
	/*
	 * display total errors
	 * close files and exit
	 */
      close_files();
   	  System.exit(lz390_rc);
}
private void put_stats(){
	/*
	 * display statistics as comments at end of bal
	 */
	tz390.force_nocon = true; // RPI 755
	if (tz390.opt_stats){
	   tz390.put_stat_final_options(); // rpi 755
	   put_stat_line("Stats total obj files = " + tot_obj_files);
	   put_stat_line("Stats total esds      = " + tot_gbl_esd);
	   put_stat_line("Stats total csects    = " + tot_csect);
	   put_stat_line("Stats total entries   = " + tot_entry);
	   put_stat_line("Stats missing wxtrn's = " + tot_missing_wxtrn);
	   put_stat_line("Stats Keys            = " + tz390.tot_key);
	   put_stat_line("Stats Key searches    = " + tz390.tot_key_search);
	   if (tz390.tot_key_search > 0){
	       tz390.avg_key_comp = tz390.tot_key_comp/tz390.tot_key_search;
	   }
	   put_stat_line("Stats Key avg comps   = " + tz390.avg_key_comp);
	   put_stat_line("Stats Key max comps   = " + tz390.max_key_comp);
	   put_stat_line("Stats total obj byte  = " + tot_obj_bytes);
	   put_stat_line("Stats total obj rlds  = " + tot_rld);
	   if (tz390.opt_timing){
	      cur_date = new Date();
	      tod_end = cur_date.getTime();
	      tot_sec = (tod_end - tod_start)/1000;
	   }
		put_stat_line("total errors          = " + lz390_errors);
	}
	put_log(msg_id +"total errors         = " + lz390_errors);
	tz390.force_nocon = false; // RPI 755
}
private void put_stat_line(String msg){
	/*
	 * routine statistics line to LST or STATS(file)
	 */
	if (tz390.stats_file != null){
		tz390.put_stat_line(msg);
	} else {
		put_log(msg_id + msg);
	}
}
private void close_files(){
	  /*
	   * close obj, lst, err, trl
	   */
	  if (obj_file != null){
	  	  try {
	  	  	  obj_file.close();
	  	  } catch (IOException e){
	  	  	  abort_error(3,"I/O error on obj close - " + e.toString());
	  	  }
	  }
	  tz390.close_systerm(lz390_rc);
      tz390.force_nocon = true;   // rpi 755
	  put_log(tz390.ended_msg);   // rpi 755
	  tz390.force_nocon = false;  // rpi 755
	  if  (tz390.opt_list){
		  if (lst_file != null && lst_file.isFile()){
		  	  try {
		  	  	  lst_file_buff.close();
		  	  } catch (IOException e){
		  	  	  abort_error(4,"I/O error on lst close - " + e.toString());
		  	  }
		  }
	  }
	  tz390.close_trace_file();
}
private void log_error(int error,String msg){
	/*
	 * issue error msg to log with prefix and
	 * inc error total
	 * 1.  supress if not gen_obj and not trace
	 */
      lz390_errors++;
      if (lz390_rc < 12){
    	  lz390_rc = 12; // RPI 1062
      }
	  String error_msg = "LZ390E error " + tz390.right_justify("" + error,3) + " " + msg;
      put_log(error_msg);
	  tz390.put_systerm(error_msg);
	  if (tz390.max_errors != 0 && lz390_errors > tz390.max_errors){
	  	 abort_error(5,"max errors exceeded");	 
	  }
}
private synchronized void abort_error(int error,String msg){ // RPI 646
	/*
	 * issue error msg to log with prefix and
	 * inc error total
	 */
	  String error_msg = null;
	  lz390_errors++;
	  lz390_rc = 16; // RPI 1062
	  if (tz390.z390_abort){
		 error_msg = "LZ390E abort due to recursive abort for " + msg;
		 System.out.println(error_msg);
	  	 tz390.close_systerm(lz390_rc);
		 System.exit(lz390_rc);
	  }
	  tz390.z390_abort = true;
	  tz390.opt_con = true; // RPI 453
	  error_msg = "LZ390E abort " + error + " " + msg;
	  put_log(error_msg);
	  tz390.put_systerm(error_msg);
      exit_lz390();
}
private void put_copyright(){
	   /*
	    * display lz390 version, timestamp,
	    * and copyright if running standalone
	    */
	    tz390.force_nocon = true; // RPI 755
	   	if  (z390_log_text == null){
	   	    put_log(msg_id + "Copyright 2011 Automated Software Tools Corporation");
	   	    put_log(msg_id + "z390 is licensed under GNU General Public License");
	   	}
		if (tz390.opt_stats){
			put_stat_line("Copyright 2011 Automated Software Tools Corporation");
			put_stat_line("z390 is licensed under GNU General Public License");
			put_stat_line("options = " + tz390.cmd_parms);
			put_stat_line("program = " + tz390.dir_mlc + tz390.pgm_name + tz390.pgm_type);
		}
	   	// RPI 378
	   	put_log(msg_id + "options = " + tz390.cmd_parms);
	   	put_log(msg_id + "program = " 
	   			+ tz390.dir_mlc 
	   			+ tz390.pgm_name 
	   			+ tz390.pgm_type); // RPI 1051
	   	tz390.force_nocon = false; // RPI 755
       }
	   private synchronized void put_log(String msg) {
	   	/*
	   	 * Write message to z390_log_text or console
	   	 * if running standalone
	   	 * 
	   	 */
   	    	put_lst_line(msg);
   	    	if (tz390.force_nocon){
   	    		return; // RPI 755
   	    	}
	        if  (z390_log_text != null){
  	        	z390_log_text.append(msg + "\n");
   	        }
	        if (tz390.opt_con){ // RPI 453
   	    	    System.out.println(msg);
   	        }
	   }
	   private void put_lst_line(String msg){
	   /*
	    * put line to listing file
	    */
	   	   if (tz390.opt_list){ // RPI 484
	   	      try {
	   	    	  tz390.systerm_io++;
	   	          lst_file_buff.write(msg + tz390.newline); // RPI 500
	   	          if (lst_file.length() > tz390.max_file_size){
	   	        	  abort_error(34,"maximum lst file size exceeded");
	   	          }
	   	      } catch (Exception e){
	   	          lz390_errors++;
	   	      }
	   	   }
	   	   if (tz390.opt_tracel){
	   		   tz390.put_trace(msg);
	   	   }
	   }
private void open_files(){
	/*
	 * 1.  Set trace file name for TRACEL TRACEALL
	 * 2.  Open 390 and lst files
	 */
        if (tz390.trace_file_name == null){  // RPI 719
        	tz390.trace_file_name = tz390.dir_trc + tz390.pgm_name + tz390.trl_type;
	    } else {
	    	tz390.trace_file_name = tz390.trace_file_name + tz390.trl_type;
	    }
       	if (tz390.opt_list){
       		String lst_file_name = tz390.get_file_name(tz390.dir_lst,tz390.pgm_name,tz390.lst_type); // RPI 866
         	try {
               lst_file = new File(lst_file_name); // RPI 908
       	       lst_file_buff = new BufferedWriter(new FileWriter(lst_file));
       	    } catch (IOException e){
       		   abort_error(9,"I/O error on lst open - " + e.toString());
       	    }
       	}
}
private void process_lkd_cmds(){
	/*
	 * process .LNK input commands
	 *   1.  INCLUDE name - load obj
	 *   2.  ENTRY   name - set entry addr
	 *   3.  ALIAS   name - gen stub
	 *   4.  NAME    name - rename 390
	 * If no .LNK try loading primary obj
	 */
	String inc_ddname = "";
	String inc_path = "";
	String inc_pgm = "";
	lkd_file_name = tz390.find_file_name(tz390.dir_mlc,tz390.pgm_name,tz390.lkd_type,tz390.dir_cur); 
	if (!tz390.lkd_ignore 
		&& lkd_file_name != null){
		try {
			lkd_file = new RandomAccessFile(lkd_file_name,"r");
			tz390.systerm_io++;
			lkd_cmd = lkd_file.readLine();
			while (lkd_cmd != null){
				put_log("LNK command - " + lkd_cmd.trim());
				if (lkd_cmd.length() > 1
					&& lkd_cmd.charAt(0) != '*'){		
					tz390.split_line(lkd_cmd);
					if (tz390.split_op != null){
                        lkd_op = tz390.split_op.toUpperCase();
					} else {
						lkd_op = "";
					}
					if (tz390.split_parms != null){
						tz390.split_line(tz390.split_parms);
						lkd_parm = tz390.split_label.toUpperCase();
					} else {
						lkd_parm = "";
					}
					if (lkd_op.equals("INCLUDE")){
						// INCLUDE
						int index1 = lkd_parm.indexOf('(');
						int index2 = lkd_parm.indexOf(')');
						if (index1 > 0  && index2 > index1 + 1){
							inc_ddname = lkd_parm.substring(0,index1);
							inc_pgm    = lkd_parm.substring(index1+1,index2);
							inc_path = System.getenv(inc_ddname);
							if (inc_path != null){
								obj_file_name = tz390.find_file_name(inc_path,inc_pgm,tz390.obj_type,tz390.dir_cur);
							} else {
								obj_file_name = null;
							}
							if (obj_file_name != null 
								&& load_obj_file(load_esds_only)){
								add_gbl_esds();
							} else {
								log_error(46,"LNK INCLUDE NOT FOUND - " + lkd_cmd.trim());
							}
						} else {
							log_error(48,"LNK INCLUDE SYNTAX ERROR - " + lkd_cmd.trim());
						}
					} else	if (lkd_op.equals("ENTRY")){
						// ENTRY
						lkd_entry = lkd_parm;
					} else	if (lkd_op.equals("ALIAS")){
						// ALIAS - write 8 byte alias.390 containing name to load
						if (tot_alias < max_alias){
							alias_name[tot_alias] = lkd_parm;
							tot_alias++;
						}
					} else	if (lkd_op.equals("NAME")){
						// NAME
						tot_name++;
						tz390.pgm_name = lkd_parm;
						int index = tz390.pgm_name.indexOf('(');
						if (index > 0){  // strip off (R) if any
							tz390.pgm_name = tz390.pgm_name.substring(0,index);
						}
						resolve_esds();
		            	load_obj_code();
		            	gen_load_module();
		            	while (tot_alias > 0){
							tot_alias--;
							create_alias_390(alias_name[tot_alias],tz390.pgm_name);
						}
						reset_esds();
					} else {
						put_log("LNK unknown command ignored - " + lkd_cmd.trim());
					}
				}
				lkd_cmd = lkd_file.readLine();
			}
		} catch (Exception e){
			log_error(39,"LNK command file I/O error " + e.toString());
		}
	} else {
		obj_file_name = tz390.find_file_name(tz390.dir_obj,tz390.pgm_name,tz390.obj_type,tz390.dir_cur); 
		if (obj_file_name != null 
			&& load_obj_file(load_esds_only)){
			add_gbl_esds();
		}
	}
}
private void create_alias_390(String alias,String pgm){
	/*
	 * create ascii 390 file 
	 * named alias.390 containing ascii pgm name
	 */
	try {
		alias_file_name = tz390.get_file_name(tz390.get_first_dir(tz390.dir_390),alias,tz390.z390_type); // RPI 866
		alias_file = new File(alias_file_name);
		alias_file_buff = new BufferedWriter(new FileWriter(alias_file));
		alias_file_buff.write(pgm.toUpperCase());
		alias_file_buff.close();
	} catch (Exception e){
		log_error(47,"LNK ALIAS CREATE FAILED FOR - " + alias);
	}
}
private void resolve_esds(){
	/*
	 * search and load obj files for extrns
	 * until all found or no more can be resolved 
	 */
       while (find_ext_file()){
    	  int loc_ctr_start = loc_ctr;  // RPI 735
    	  if (load_obj_file(load_esds_only)){
             add_gbl_esds();
             if (gbl_esd_type[cur_gbl_ext] == gbl_esd_ext){ // RPI 735
         		gbl_esd_loc[cur_gbl_ext] = loc_ctr_start;   // if matching load ok
        		gbl_esd_type[cur_gbl_ext] = gbl_esd_ent;    // assign default entry
             }
          }
   	   }
       cur_gbl_ext = 1;
       while (cur_gbl_ext <= tot_gbl_esd){
    	   if (gbl_esd_type[cur_gbl_ext] == gbl_esd_ext){
    	      log_error(27,"unresolved external reference - " + gbl_esd_name[cur_gbl_ext]);
    	   } else if (gbl_esd_type[cur_gbl_ext] == gbl_esd_wxt){
    		   tot_missing_wxtrn++;
    	   } else if (lkd_entry != null 
      			      && gbl_esd_type[cur_gbl_ext] == gbl_esd_ent
      			      && lkd_entry.equals(gbl_esd_name[cur_gbl_ext])){
    		   lkd_entry_loc = gbl_esd_loc[cur_gbl_ext];  // RPI 732
    	   }
    	   cur_gbl_ext++;
       }
       if (lkd_entry_loc == -1){
    	   if (lkd_entry == null || lkd_entry.equals(tz390.pgm_name)){
  			   // else default to start and first csect
   		       lkd_entry_loc = 0;
    	   } else {
    		   log_error(41,"LNK ENTRY " + lkd_entry + " NOT FOUND"); // RPI 732
    	   }
       }
}
private boolean load_obj_file(boolean esds_only){
	/*
	 * load object file esds only or entire file
	 * using obj_file_name and
	 * return true if successful
	 */
    open_obj_file(obj_file_name);
    if (!esds_only){
    	tz390.force_nocon = true;
   	    put_log(msg_id + "INCLUDE = " 
            + obj_file_name); // RPI 1051
        tz390.force_nocon = false;
    }
    if (tz390.opt_tracel){
  	  	 tz390.put_trace("LOADING OBJ FILE - " + obj_file_name);
    }
    tot_obj_esd = 0;
    obj_eod = false;
	get_obj_line();
	if (obj_line == null){ // PRI 483
		abort_error(38,"object file truncated " + obj_file_name); // RPI 827
	} else if (esds_only && !obj_line.substring(0,4).equals(".ESD")){
		obj_eod = true;
	}
	int max_obj_esd = 0;
	while (!obj_eod
			&& tot_obj_esd < tz390.opt_maxesd){
		if (tz390.opt_tracel && (!esds_only || obj_line.substring(0,4).equals(".ESD"))){
			tz390.put_trace("LOADING " + obj_line);
		}
		if (obj_line.substring(0,4).equals(".END")){
			obj_eod = true;
			// set last .END entry address if any RPI 1197
			obj_end_entry_esd = Integer.valueOf(obj_line.substring(9,13),16).intValue();
		    obj_end_entry_loc = Integer.valueOf(obj_line.substring(18,26),16).intValue();
			if (obj_end_entry_esd > 0 &&!esds_only){
			   obj_end_entry_last_loc = obj_end_entry_loc + gbl_esd_loc[obj_gbl_esd[obj_end_entry_esd]];
			}
		} else if (obj_line.substring(0,4).equals(".ESD")){
			tot_obj_esd++;
			obj_esd[tot_obj_esd] = Integer.valueOf(obj_line.substring(9,13),16).intValue();
			if (obj_esd[tot_obj_esd] > max_obj_esd){
				max_obj_esd = obj_esd[tot_obj_esd];
			}
			obj_esd_name[tot_obj_esd] = obj_line.substring(54);
			obj_esd_loc[tot_obj_esd]  = Integer.valueOf(obj_line.substring(18,26),16).intValue();
			obj_esd_len[tot_obj_esd]  = Integer.valueOf(obj_line.substring(31,39),16).intValue();
			obj_esd_type[tot_obj_esd] = obj_line.substring(45,48);
			if (!esds_only
				&& (obj_esd_type[tot_obj_esd].equals("CST")
				    || obj_esd_type[tot_obj_esd].equals("EXT")		
				    || obj_esd_type[tot_obj_esd].equals("WXT"))){ //RPI182
				if (find_gbl_esd(obj_esd_name[tot_obj_esd])){
					obj_gbl_esd[obj_esd[tot_obj_esd]] = cur_gbl_esd;
				}
			}
		} else if (obj_line.substring(0,4).equals(".TXT")){
		  if (!esds_only){
			int    obj_text_esd = Integer.valueOf(obj_line.substring(9,13),16).intValue();
			if (obj_text_esd < 1 || obj_text_esd > max_obj_esd){
				abort_error(29,"invalid object text esd - " + obj_text_esd + " in " + obj_file_name); // RPI 827
				return false;  // RPI 301
			}
			int    obj_text_loc = Integer.valueOf(obj_line.substring(18,26),16).intValue();
			int    obj_text_len = Integer.valueOf(obj_line.substring(31,33),16).intValue();
			String obj_text = obj_line.substring(34,34 + 2 * obj_text_len);
			int code_off = gbl_esd_loc[obj_gbl_esd[obj_text_esd]] + obj_text_loc;
			if (code_off > loc_ctr){
				abort_error(25,"invalid object code offset - " + obj_line + " in " + obj_file_name); // RPI 827
				return false;
			} 
			z390_code_buff.position(code_off);
			int index = 0;
			while (index < 2 * obj_text_len){
				z390_code_buff.put((byte) Integer.valueOf(obj_text.substring(index,index+2),16).intValue());
				index = index + 2;
			}
			tot_obj_bytes = tot_obj_bytes + obj_text_len;
			if (code_off + obj_text_len > mod_loc_ctr){
				mod_loc_ctr = code_off + obj_text_len; // RPI 883
			}
		  }
		} else if (obj_line.substring(0,4).equals(".RLD")){
		  if (!esds_only){
			int  obj_rld_esd = Integer.valueOf(obj_line.substring(9,13),16).intValue();
			if (obj_rld_esd < 1 || obj_rld_esd > max_obj_esd){
				abort_error(30,"invalid rld esd - " + obj_rld_esd + " in " + obj_file_name);  // RPI 827
				return false;  // RPI 301
			}
			int  obj_rld_loc = Integer.valueOf(obj_line.substring(18,26),16).intValue();
			byte obj_rld_len = (byte) Integer.valueOf(obj_line.substring(31,32),16).intValue();
			char obj_rld_sgn = obj_line.charAt(38);
			int  obj_rld_xesd = Integer.valueOf(obj_line.substring(45,49),16).intValue();
			int rld_off = 0;
			int rld_fld = 0;
			if (tot_rld < tz390.opt_maxrld){
				rld_loc[tot_rld] = obj_rld_loc  + gbl_esd_loc[obj_gbl_esd[obj_rld_esd]]; //RPI 240
				if (obj_rld_sgn == '+'){
					rld_len[tot_rld] = obj_rld_len;
				} else {
					rld_len[tot_rld] = (byte)- obj_rld_len;
				}
				if (gbl_esd_type[obj_gbl_esd[obj_rld_xesd]] == gbl_esd_ent){ //RPI182
			    	rld_off = gbl_esd_loc[obj_gbl_esd[obj_rld_esd]] + obj_rld_loc;
			    	rld_fld = z390_code_buff.getInt(rld_off);
					if (tz390.opt_list){
						int rld_fld_temp = rld_fld; // RPI 1046
						if (obj_rld_len == 3){
							rld_fld_temp = rld_fld_temp >>> 8; // RPI 1046
						}
						put_lst_line(msg_id + "RLD LOC=" + tz390.get_hex(rld_off,8)
								        + " FLD=" + tz390.get_hex(rld_fld_temp,obj_rld_len*2) // RPI 1046
								        + " EXT=" + tz390.get_hex(gbl_esd_loc[obj_gbl_esd[obj_rld_xesd]],8));
					}
					int rld_save;
			    	switch (obj_rld_len){
				    case 2:  // RPI 894 
				        rld_save = rld_fld & 0xffff;
				    	rld_fld = rld_fld >>> 16;  // rpi 894
				    	if (obj_rld_sgn == '+'){
				    	    rld_fld = rld_fld + gbl_esd_loc[obj_gbl_esd[obj_rld_xesd]];
				    	    if (gbl_esd_loc[obj_gbl_esd[obj_rld_xesd]] > 0xffff){
				    	    	log_error(42,"invalid 2 byte RLD offset over 64k"); // RPI 894
				    	    }
				    	} else {
					    	rld_fld = rld_fld - gbl_esd_loc[obj_gbl_esd[obj_rld_xesd]];
				    	}
				    	rld_fld = (rld_fld << 16) | rld_save;
				    	z390_code_buff.putInt(rld_off,rld_fld);
				    	break;    
			    	case 3:
					        rld_save = rld_fld & 0xff;
					    	rld_fld = rld_fld >>> 8;  // rpi 894
					    	if (obj_rld_sgn == '+'){
					    	    rld_fld = rld_fld + gbl_esd_loc[obj_gbl_esd[obj_rld_xesd]];
					    	    if (gbl_esd_loc[obj_gbl_esd[obj_rld_xesd]] > 0xffffff){
					    	    	log_error(42,"invalid 3 byte RLD offset over 16 MB"); // RPI 894
					    	    }
					    	} else {
						    	rld_fld = rld_fld - gbl_esd_loc[obj_gbl_esd[obj_rld_xesd]];
					    	}
					    	rld_fld = (rld_fld << 8) | rld_save;
					    	z390_code_buff.putInt(rld_off,rld_fld);
					    	break;
					    case 4:
					    	if (obj_rld_sgn == '+'){
					    	    rld_fld = rld_fld + gbl_esd_loc[obj_gbl_esd[obj_rld_xesd]];
					    	} else {
						    	rld_fld = rld_fld - gbl_esd_loc[obj_gbl_esd[obj_rld_xesd]];
					    	}
					    	z390_code_buff.putInt(rld_off,rld_fld);
					    	break;
					    case 8:  // RPI 270
					    	if (rld_fld != 0){ // verify first half of 8 byte rld field is 0
					    		log_error(28,"invalid 8 byte RLD field at offset " + obj_line);
					    	}
					    	rld_off = rld_off+4;
					    	rld_fld = z390_code_buff.getInt(rld_off);
					    	if (obj_rld_sgn == '+'){
					    	    rld_fld = rld_fld + gbl_esd_loc[obj_gbl_esd[obj_rld_xesd]];
					    	} else {
						    	rld_fld = rld_fld - gbl_esd_loc[obj_gbl_esd[obj_rld_xesd]];
					    	}
					    	z390_code_buff.putInt(rld_off,rld_fld);
					    	break;
					}
					tot_rld++;
				}
			} else {
				abort_error(21,"z390 rld table exceeded");
			}
		  }
		} else {
			abort_error(20,"unknown obj record type - " + obj_line);
		}
        get_obj_line();
		if (!obj_eod && obj_line == null){ // RPI 483
			abort_error(38,"object file truncated - " + obj_file_name); // RPI 827
		}
	}
	try {
	    obj_file.close();
	    if (tot_obj_files < tz390.opt_maxfile){
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
private void get_obj_line(){
	/*
	 * get next esd line from obj file else 
	 * set obj_eod
	 */
	try {
		if (obj_file_bin){
			tz390.systerm_io++;
			if (obj_file.read(bin_byte,0,80) == 80){
				obj_line = cvt_obj_bin_to_hex();
			} else {
			    obj_line = null;
			}
		} else {
			tz390.systerm_io++;
			obj_line = obj_file.readLine();
		}
	} catch (IOException e){
		abort_error(14,"I/O error on obj read - " + e.toString());
	}
}
private String cvt_obj_bin_to_hex(){
	/*
	 * convert binary obj file record in obj_bin
	 * to ascii string text format
	 */
	String text     = "." + tz390.ascii_table.charAt(tz390.ebcdic_to_ascii[bin_byte[1] & 0xff] & 0xff) // RPI 1069
	                      + tz390.ascii_table.charAt(tz390.ebcdic_to_ascii[bin_byte[2] & 0xff] & 0xff) // RPI 1069
	                      + tz390.ascii_table.charAt(tz390.ebcdic_to_ascii[bin_byte[3] & 0xff] & 0xff) // RPI 1069
	                      ; // ascii hex obj record
	String esd_id   = tz390.get_hex(bin_byte_buff.getShort(14),4);  // ESD ID number
	String esd_loc  = "";  // ESD address
	String esd_len  = "";  // ESD length
    int index = 0;
	if (text.equals(".ESD")){
		String esd_name = "";  // ESD name
		String esd_type = "";  // ESD type = CST,ENT,EXT,WXT
		int esd_align = 0; // currently ignored
		index = 16;
		while (index < 24){
			esd_name = esd_name + tz390.ascii_table.charAt(tz390.ebcdic_to_ascii[bin_byte[index] & 0xff] & 0xff); // RPI 1069
			index++;
		}
		switch (bin_byte[24]){
		case 0x00: // SD type
			esd_type = "CST";
			bin_byte[24] = 0;
			esd_loc = tz390.get_hex(bin_byte_buff.getInt(24),8);
			esd_align = bin_byte[28];
			if (esd_align != 7){
				abort_error(138,"unsupported SD alignment code - " + esd_align + " in " + obj_file_name); // RPI 827
			}
			bin_byte[28] = 0;
			esd_len = tz390.get_hex(bin_byte_buff.getInt(28),8);
			break;
		case 0x01: // LD type
			esd_type = "ENT";
			bin_byte[24] = 0;
			esd_loc = tz390.get_hex(bin_byte_buff.getInt(24),8);
			esd_align = bin_byte[28];
			esd_len = tz390.get_hex(0,8);
			esd_id = tz390.get_hex(bin_byte_buff.getShort(30),4);
			break;
		case 0x02: // ER type
			esd_type = "EXT";
			esd_loc = tz390.get_hex(0,8);
			esd_align = bin_byte[28];
			esd_len = tz390.get_hex(0,8);
			break;
		case 0x0a: // WR type
			esd_type = "WXT";
			esd_loc = tz390.get_hex(0,8);
			esd_align = bin_byte[28];
			esd_len = tz390.get_hex(0,8);
			break;
		default:
			abort_error(37,"invalid ESD type in " + obj_file_name); // RPI 827
		}
		text = text + " ESD=" + esd_id + " LOC=" + esd_loc + " LEN=" + esd_len + " TYPE=" + esd_type + " NAME=" + esd_name.trim();
	} else if (text.equals(".TXT")){
		bin_byte[4] = 0;
		esd_loc = tz390.get_hex(bin_byte_buff.getInt(4),8);
		bin_byte[10] = 0;
		int count = bin_byte_buff.getShort(10);
		esd_len = tz390.get_hex(count,2);
		text = text + " ESD=" + esd_id 
		            + " LOC=" + esd_loc 
		            + " LEN=" + esd_len + " ";
		index = 16;
		while (count > 0){
			text = text + tz390.get_hex(bin_byte[index] & 0xff,2);
			index++;
		    count--;
		}
	} else if (text.equals(".RLD")){
		String xesd_id  = tz390.get_hex(bin_byte_buff.getShort(16),4);;  // XESD ID number ref or RLD
		esd_id   = tz390.get_hex(bin_byte_buff.getShort(18),4);          // ESD ID number SD with RLD
		String esd_rld_len = tz390.get_hex((bin_byte[20] >> 2)+1,1);
		if (esd_rld_len.charAt(0) == '1'){ // RPI 894 was 2
			esd_rld_len = "8"; // RPI 270
		}
    	String esd_rld_sign = "+";
    	if ((bin_byte[20] & 0x02) != 0){
	    	esd_rld_sign = "-";
	    }
    	bin_byte[20] = 0;
		esd_loc = tz390.get_hex(bin_byte_buff.getInt(20),8);
    	text = text + " ESD=" + esd_id + " LOC=" + esd_loc + " LEN=" + esd_rld_len + " SIGN=" + esd_rld_sign + " XESD=" + xesd_id;
	} else if (text.equals(".END")){
		esd_loc = tz390.get_hex(bin_byte_buff.getInt(5) >>> 8,8); // RPI 1197
		text = text + " ESD=" + esd_id + " LOC=" + esd_loc; // RPI 1197		
	} else {
		abort_error(36,"invalid object record type - " + text + " in " + obj_file_name); // RPI 827
	}
	return text;
}
private void add_gbl_esds(){
	/*
	 * add any new global esds found
	 */
	int obj_index1 = 1;
	while (obj_index1 <= tot_obj_esd){
		if (tot_gbl_esd >= tz390.opt_maxesd){
			abort_error(15,"maximum global ESDS exceeded");
		}
		if (obj_esd_type[obj_index1].equals("CST")){
            add_gbl_cst(obj_index1);
		} else if (obj_esd_type[obj_index1].equals("EXT")){
			add_gbl_ref(obj_index1,gbl_esd_ext);
		} else if (obj_esd_type[obj_index1].equals("WXT")){ //RPI182
			add_gbl_ref(obj_index1,gbl_esd_wxt);
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
	if (obj_esd_len[obj_index1] == 0){
		return; // RPI 385 ignore 0 length csects
	}
	tot_csect++;
	boolean esd_ok = true;
	if (find_gbl_esd(obj_esd_name[obj_index1])){
		if (gbl_esd_type[cur_gbl_esd] != gbl_esd_ext
			&& gbl_esd_type[cur_gbl_esd] != gbl_esd_wxt
			){ //RPI182
		    esd_ok = false;
			put_log("LZ390W warning - ignoring duplicate CSECT - " + obj_esd_name[obj_index1]);
		}
	} else {
		tot_gbl_esd++;
		cur_gbl_esd = tot_gbl_esd;
		tz390.add_key_index(cur_gbl_esd);
	}
	if  (esd_ok){
		if (tz390.opt_list){ // RPI 385
			put_lst_line(msg_id + "ESD=" + tz390.left_justify(obj_esd_name[obj_index1],8) + " LOC=" + tz390.get_hex(loc_ctr,8) + " LEN=" + tz390.get_hex(obj_esd_len[obj_index1],8));
		}
		gbl_esd_name[cur_gbl_esd] = obj_esd_name[obj_index1];
		gbl_esd_loc[cur_gbl_esd] = loc_ctr;
		loc_ctr = loc_ctr + obj_esd_len[obj_index1];
		gbl_esd_type[cur_gbl_esd] = gbl_esd_ent;
	}
}
private void add_gbl_ref(int obj_index1,byte esd_type){
	/*
	 * add ext or wxt ref to gbl table
	 */
	if (!find_gbl_esd(obj_esd_name[obj_index1])){
		tot_gbl_esd++;
		tz390.add_key_index(tot_gbl_esd);
		gbl_esd_name[tot_gbl_esd] = obj_esd_name[obj_index1];
		gbl_esd_type[tot_gbl_esd] = esd_type;
	}
}
private void add_gbl_ent(int obj_index1){
	/*
	 * add obj entry to gbl table
	 */
	tot_entry++;
	boolean esd_ok = true;
	if (find_gbl_esd(obj_esd_name[obj_index1])){
		if (gbl_esd_type[cur_gbl_esd] != gbl_esd_ext
				&& gbl_esd_type[cur_gbl_esd] != gbl_esd_wxt){ //RPI182
		    esd_ok = false;
			log_error(17,"ignoring duplicate ENTRY - " + obj_esd_name[obj_index1]);
		}
	} else {
		tot_gbl_esd++;
		cur_gbl_esd = tot_gbl_esd;
		tz390.add_key_index(cur_gbl_esd);
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
				   if (tz390.opt_list){ // RPI 385
					   put_lst_line(msg_id + "ESD=" + tz390.left_justify(obj_esd_name[obj_index1],8) + " ENT=" + tz390.get_hex(gbl_esd_loc[gbl_ent_esd],8));
				   }
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
	  if (tz390.opt_time
			&& (tot_find_gbl_esd > next_time_check)){
			next_time_check = tot_find_gbl_esd + next_time_ins;
			cur_date = new Date();
			tod_end = cur_date.getTime();
			if (tod_end > tod_time_limit){
               abort_error(24,"time limit exceeded");
			}
		}
	    cur_gbl_esd = tz390.find_key_index('G',esd_name);
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
		if (gbl_esd_type[cur_gbl_ext] == gbl_esd_ext
			&& gbl_esd_name[cur_gbl_ext] != null){ // RPI 459
			// RPI 378
			if (tz390.opt_autolink){ // RPI 770
				obj_file_name = tz390.find_file_name(tz390.dir_obj + "+linklib",gbl_esd_name[cur_gbl_ext],tz390.pgm_type,tz390.dir_cur);  // RPI 814
				if (obj_file_name != null){ // RPI 378
					open_obj_file(obj_file_name);
					return true;
				}
			} else {
				return false;
			}
		}
		cur_gbl_ext++;
	}
	return false;
}
private boolean open_obj_file(String file){
	/*
	 * open object file and set type else
	 * return false
	 */
	try {
		obj_file = new RandomAccessFile(file,"r");
		tz390.systerm_io++;
		int test_byte = obj_file.read();
    	if (test_byte == obj_bin_id){
    		obj_file_bin = true;
    	} else if (test_byte == '.') {
    		obj_file_bin = false;
    	} else {
    		abort_error(35,"invalid obj file " + file); // RPI 827
    	}
    	obj_file.seek(0);
    	return true;
	} catch (Exception e){
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
		if (tz390.opt_init){
			Arrays.fill(z390_code,0,loc_ctr,(byte)0xF6);  // RPI 946
		}
		z390_code_buff = ByteBuffer.wrap(z390_code,0,loc_ctr);
	} else {
		abort_error(19,"no csect object code text defined for load module " + obj_file_name);  // RPI 656 RPI 827
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
	 * output 390 load module in binary format
	 * skipping rlds for unresolved wxtrn's 
	 */
	if (obj_end_entry_last_loc > 0 && lkd_entry_loc == 0){
	   // if no ENTRY command, use last END Label entry
	   lkd_entry_loc = obj_end_entry_last_loc; // RPI 1197
	}
    put_stats();
	if (tz390.opt_mod && tot_rld > 0){
		abort_error(40,"MOD file cannot contain RLD's =" + tot_rld); // RPI 883
	}
    if (loc_ctr > tz390.max_file_size){
    	abort_error(32,"maximum 390 file size exceeded");
    }
	try {
		String z390_sfx = tz390.z390_type;
		if (tz390.opt_mod){
			z390_sfx = tz390.mod_type; // RPI 883
		}
        z390_file = new RandomAccessFile(tz390.get_first_dir(tz390.dir_390) + tz390.pgm_name + z390_sfx,"rw"); // RPI 883
        z390_file.setLength(0);
        z390_file.seek(0);
        if (!tz390.opt_mod){  // RPI 883
        	tz390.systerm_io = tz390.systerm_io + 6;
        	z390_file.writeBytes(z390_code_ver);
        	z390_flags = "" + tz390.z390_amode31 + tz390.z390_rmode31 + "??";
        	z390_file.writeBytes(z390_flags);  // z390_flags
        	z390_file.writeInt(loc_ctr);       // z390_code_len
        	z390_file.writeInt(lkd_entry_loc); // z390_code_ent offset RPI 732
        	z390_file.writeInt(tot_rld);       // z390_code_rlds 
        }
        if (tz390.opt_mod){
        	z390_file.write(z390_code,0,mod_loc_ctr); // RPI 883
        } else {
        	z390_file.write(z390_code,0,loc_ctr);
        }
        int cur_rld = 0;
        while (cur_rld < tot_rld){
        	tz390.systerm_io++;
        	z390_file.writeInt(rld_loc[cur_rld]);
        	tz390.systerm_io++;
        	z390_file.write(rld_len[cur_rld]);
            if (z390_file.length() > tz390.max_file_size){
            	abort_error(33,"maximum 390 file size exceeded");
            }
        	cur_rld++;
        }
        z390_file.close();
	} catch (Exception e){
	 	abort_error(22,"I/O error on z390 load module file - " + e.toString());
	}
}
private void reset_esds(){
	/*
	 * reset ESD global data for next
	 * LNK name command
	 */
	tot_obj_bytes = 0;
	tot_find_gbl_esd = 0;
	tot_obj_files = 0;
    load_esds_only = true;
    max_obj_esd = 0;
    lkd_file_name = null;      // RPI 732
    lkd_cmd = null;            // RPI 732
    lkd_entry = null;          // RPI 732
    lkd_entry_loc = -1;         // RPI 732
    lkd_alias = null;          // RPI 732
    lkd_name  = null;          // RPI 732
    obj_file_name = null;
    obj_file_bin = false;
    obj_eod = false;
    tot_alias = 0;
    alias_file_name = null;
    obj_line = null;
    obj_eof = false;
    log_tod = true; 
    tot_csect   = 0;
    tot_entry   = 0;
    tot_missing_wxtrn = 0;
    tot_gbl_esd = 0;
    cur_gbl_esd = 0;
    cur_gbl_ext = 0;
    loc_ctr = 0;
    tot_rld = 0; // RPI 1046
}
/*
 *  end of lz390code 
 */
}