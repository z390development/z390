import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.lang.reflect.Array;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.swing.JTextArea;


public class zc390{
	/*****************************************************
	
    ZC390 zcobol COBOL translator to z390 macro assembler
    meta language which can then be expanded to
    z390 mainframe assembler, i586 Intel assembler,
    MS VCE++, or J2SE Java using macro libraries.
	
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

    zc390 is the component of z390 used to translate
    COBOL source programs (.CBL) to
    z390 assembler source programs (.MLC).
    
    zcoboljava is a COBOL source translator
    which does the following
      1. read source ascii cobol from parm1
         with CBL default file type.
      2. parse space delimited COBOL into
         comma delimited z390 macro assembler
         source statements using MLC default file type.
         a.  New macro for each new COBOL verb.
         b.  labels with dashes converted to underscores.     
         c.  Continuation of words and literals
             handled. 
         d.  Periods and commas allowed within parms which
             are then enclosed in single quotes to avoid
             conflict with macro assembler parm parsing.
         e.  Open/close () put in single quotes to avoid 
             conflict with macro assembler parm parsing.
         f.  Literals with single or double quotes allowed
             with double single/double quotes inclosed.
         g.  Single '" wrapped in
             opposite type quotes.  
         h.  Single () wrapped in single quotes.
         i.  Single . generates PERIOD vero in Proc. Div. 
         j.  Generate DATA END, PROCEDURE END,and END  
         k.  Treat verbs as parms within EXEC to END-EXEC.
             
    ****************************************************
    * Maintenance
    ****************************************************
    * 04/06/08 initial coding
    * 04/11/08 add continuation line support and
    *          verb lookup for mult verbs on a line.
    * 04/28/08 allow period within pictures.
    * 06/10/08 truncate source to 72 characters, add EXEC verb
    * 11/11/08 pass parms to CBL macro call
    * 11/21/08 rename to zc390 and add ERR/STA START/STOP, copyright, stats
    * 12/01/08 add COPY support using mz390 as model
    * 02/20/09 ignore AUTHOR, SECURITY, DATE-WRITTEN,
    *          and INSTALLATION pg.
    * 02/26/09 RPI 1012 force END DECLARATIVES into END_DECLARATIVES.  
    * 07/10/09 RPI 1062 replace START with ZCSTART to avoid HLASM conflist 
    * 07/11/09 RPI 1062 set RC=12 for errors and RC=16 for abort 
    * 07/14/09 RPI 1062 use init_pat() for use by options  
    * 07/18/09 RPI 1062 add abort msg
    * 07/19/09 RPI 1062 convert data names starting with 0-9 to #name 
    * 07/21/09 RPI 1062 replace SD with ZCSD to  avoid SD instr. conflict 
    * 07/21/09 RPI 1062 ignore semicolons in addition to commas  
    * 07/24/09 RPI 1062 support lower case, DISPLAY verb only in Proc. Div.
    * 07/29/09 RPI 1062 fix mult split lines followed by tokens
    * 07/31/09 RPI 1062 fix continued ""..." or ''...' (see NC215A ALPHABET)
    * 08/02/09 RPI 1062 fix "."".' to '.".'. ".'." to '.''.', and & to &&
    * 08/03/09 RPI 1062 support special names verbs
    * 08/06/09 RPI 1062 wrote LABEL macros to pgm_ZC_LABELS.CPY
    *          and generate COPY after ZCOBOL macro call to init SN/PG counts
    * 08/09/09 RPI 1062 comment lines must have * or / in position 
    * 08/10/09 RPI 1062 use look-ahead in get_zc_line to append 
    *          continued text not in split literals.      
    * 09/03/09 RPI 1062 WHEN verb only in proc div.   
    * 09/12/09 RPI 1062 END to ZCEND for DECLARIVES and PROGRAM   
    * 09/18/09 RPI 1074 put path and name in quotes for LSN support 
    * 09/26/09 RPI 1080 use compiled patterns for replace all
    *          allow any cbl suffix to mlc, use init_tzz390
    * 09/29/09 RPI 1086 add *ZC NNNNNN IIIIII MLC SOURCE comment
    * 07/25/10 RPI 1126 prevent replacing - with _ in floating point exp constant
    * 03/04/12 RPI 1182 don't allow embedded commas in parms for proc div
    ****************************************************
    *                                         last RPI *
	****************************************************
	*/
	/*
	 * Global variables
	 */    
	int    zc390_rc = 0; // return code 
    tz390  tz390;        // shared z390 routines   
	int    tot_cbl = 0;  // total CBL lines read
	int    tot_mlc  = 0; // total MLC lines written
	int    tot_lab  = 0; // total LABEL lines for SN/PG labels
	int    tot_err  = 0; // total errors
	/*
	 * COBOL CBL input file variables
	 */
	String  zc_line_id  = "";     // RPI 1086
	String  zc_line_num = "";    // RPI 1086
	String  zc_line = null;      // logical line with continuations added
	String  zc_line_lookahead;   // lookahead rec for non split lit continuations
	String  zc_file_name = null;
	boolean zc_comment = true; 
	boolean zc_comment_copy = false; // RPI 1062 comment COPY except for ZC_LABELS.CPY
	boolean zc_pg_comment_mode = false;
	boolean zc_cics = false;
	boolean zc_extend = true;
	boolean zc_trunc  = false;
	boolean linkage_sect = false;
	boolean request_dfheiblk = false;
	boolean request_dfhcommarea = false;
	boolean request_proc = false;
	boolean request_data_end = false;  // RPI 1086
	boolean dfheiblk = false;
	boolean dfheiblk_loading = false;
	boolean dfhcommarea = false;
	boolean zc_eof = false;
	boolean zc_comment_pending = false;
	int zc_comment_cnt = 0;
	int pic_token_cnt = 0; // tokens since pic mode turned on
	boolean pic_mode = false;
	boolean value_mode = false; // RPI 1126
	boolean exec_mode = false;
	String[] exec_parm = new String[256];
	int exec_parm_index = 0;
	boolean data_div  = false;
	boolean allow_verb = false;
	boolean skip_period = false;
	String  zc_comment_line;
	/*
	 * MLC meta macro assembler output file variables
	 */
	String mlc_file_name = null;
	File   mlc_file = null;
	BufferedWriter mlc_file_buff = null;
	String lab_file_name = null; // RPI 1062 LABEL macros for section/paragraph
	String lab_file_dir  = null; // RPI 1080 remove dir from COPY statement to support LSN
	File   lab_file = null;
	BufferedWriter lab_file_buff = null;
	/*
	 * Nested COPY file input variables
	 */
	String zc_copy_file_name = null;
	String zc_copy_member = null;
	String zc_copy_ddname = null;
	String cpz_type = ".CPZ";
    int     tot_cpz_file_name = 0;
    String[] cpz_file_name = null;
	int cur_zc_file = 0; // 0 is primary CBL input
	File[] zc_file                = null;
	BufferedReader[] zc_file_buff = null;
	boolean  zc_copy_trailer = false;
	int      zc_copy_rep_ix  = 1;
	int      cur_rep_ix      = 0;
	String[] zc_copy_line         = null; // RPI 1062 has trailing line after COPY .
	int[]    zc_copy_line_ix      = null; // RPI 1062 next token on  line index
	int[]    zc_copy_rep_fst_ix   = null; // RPI 1062 next token on  line index
	int[]    zc_copy_rep_lst_ix   = null; // RPI 1062 next token on  line index
	String[] zc_copy_rep_lit1     = null; // RPI 1062 replacing lit1
	String[] zc_copy_rep_lit2     = null; // RPI 1062 replacing lit2
    /*
     * zcob token variables
     */
	int     zc_token_count = 0;
	int     zc_token_line_cnt = 0;
	int     zc_prev_line_cnt = 0;
	int     zc_next_line_cnt = 0;
	boolean zc_flush_cont_token = false;
	String  zc_prev_token = null;
	String  zc_next_token = null;
	int     zc_next_index = 0;
	int     zc_prev_index = 0;
	char    zc_prev_area  = 'A';
	char    zc_next_area  = 'A';
	boolean zc_prev_first = true;
	boolean zc_next_first;
	boolean zc_token_first = true;
	String  zc_token   = null;   // next token or null at eof
	int     zc_token_index = 0;
	Pattern zc_name_pattern = null;    // RPI 1062 validate names
	Pattern zc_data_token_pattern = null;   // parsing regular expression pattern
	Pattern zc_proc_token_pattern = null; // RPI 1182
	Matcher zc_name_match    = null;   // data name patrern matching
	Matcher zc_token_match   = null;   // token pattern matching class
	int     zc_match_offset  = 0;      // offset to start of matcher
	char    zc_token_area = 'A'; // token area A  8-11 or B 12 or greater
	boolean zc_split_lit = false; // literal split across lines
	char    zc_split_char = '\'';  // literal sq or dq RPI 1062
	boolean zc_proc_div = false; // PRODECUDE DIV started
	String proc_using_parms = null;
	String mlc_lab = null;
	String mlc_op  = null;
	String mlc_parms = "";
	int mlc_parm_cnt = 0; // mlc parm count 
	int zc_level = 0; // parm (...) level used to allow verbs in DFHRESP/DFHVALUE etc.
	/*
	 * working storage global data
	 */
	int ws_item_lvl = 0;
	int ws_lvl_index = 0;
	int[] ws_lvl = new int[50];
	String[] ws_indent = new String[50];
	/*
	 * end of global data
	 */
	public static void main(String argv[]) {		
	      /*
	       * start instance of zcobol class
	       */
		  zc390 pgm = new zc390();
	      pgm.translate_cbl_to_mlc(argv,null);
	}
	private void translate_cbl_to_mlc(String[] args,JTextArea log_text){
	    /*
		 * translate cobol (CBL) to 
		 * z390 macro assembler (.MLC)
		 */
		init_zc390(args);
		if (tz390.opt_trap){ // RPI 1058
            try {
                    process_cbl();
            } catch (Exception e){
                    abort_error("zc390 exception - " + e.toString());
            }
		} else {
            process_cbl();
		}
	}
	private void process_cbl(){
    /*
     * process cbl to mlc with or 
     * without trap exception handler
     */
		get_zc_token();
		while (zc_token != null){
			process_zc_token();
			get_zc_token();
		}
		if (mlc_op != null){
            put_zc_line(); // RPI 1086
			put_mlc_line(" ",mlc_op,mlc_parms);
		}
		term_zc();
	}
	private void init_zc390(String[] args){
		/*
		 * 1.  Display zcobol version
		 * 2.  Compile regular expression pattern
		 * 3.  Open CBL and MLC files
		 */
		tz390 = new tz390();
		tz390.init_tz390();   // RPI 1080
		tz390.init_tz390();   // RPI 1080
    	tz390.init_options(args,tz390.mlc_type);
		tz390.open_systerm("ZC390");
       	put_copyright();
		String zcobol_options = "";
        String comma = "";
       	int index = 1;
       	while (index < args.length){
       		zcobol_options = zcobol_options + comma + args[index];
       		comma = ",";
   		    if (args[index].toUpperCase().equals("NOEXTEND")){
   			zc_extend = false;
   		    } else if (args[index].toUpperCase().equals("NOTIMING")){
       			tz390.opt_timing = false; // turn off timing for CDATE/CTIME
       		} else if (args[index].toUpperCase().equals("NOCOMMENT")){
       			zc_comment = false;
       		} else if (args[index].toUpperCase().equals("CICS")){
       			zc_cics = true;
       		} else if (args[index].toUpperCase().equals("TRUNC")){
       			zc_trunc = true;
       		}
       		index++;
       	}
		String zcobol_parms =
			"PGM='"    + args[0]   // RPI 1074
		  + "',VER="    + tz390.version
		  + ",CDATE=" + tz390.cur_date()
		  + ",CTIME=" + tz390.cur_time(false)
		  + ",OPTIONS=(" + zcobol_options + ")";
		if (tz390.opt_traces){
			System.out.println("ZCOBOL " + zcobol_parms);
		}
		index = args[0].indexOf('.'); // RPI 1080 allow any cbl suffix
		if (index == -1){
			zc_file_name = args[0] + ".CBL"; 
			mlc_file_name = args[0] + ".MLC";
		} else {
			zc_file_name = args[0]; 
			mlc_file_name = args[0].substring(0,index) + ".MLC";
		}
		lab_file_name = tz390.fix_file_separators(args[0]);  // RPI 1086
	    index = args[0].lastIndexOf(File.separator); // RPI 1080
	    if (index >= 0){
	    	lab_file_dir = lab_file_name.substring(0,index+1);
	    } else {
	    	lab_file_dir = "";
	    }
	    lab_file_name = lab_file_name.substring(index+1) + "_ZC_LABELS.CPY"; // RPI 1080 
	    mlc_file      = new File(mlc_file_name);
		zc_file              = (File[])Array.newInstance(File.class,tz390.opt_maxfile);
		zc_file_buff         = (BufferedReader[])Array.newInstance(BufferedReader.class,tz390.opt_maxfile);
        cpz_file_name        = new String[tz390.opt_maxfile];
		zc_copy_line         = (String[])Array.newInstance(String.class,tz390.opt_maxfile);
		zc_copy_line_ix      = (int[])Array.newInstance(int.class,tz390.opt_maxfile);
		zc_copy_rep_fst_ix   = (int[])Array.newInstance(int.class,tz390.opt_maxfile);
		zc_copy_rep_lst_ix   = (int[])Array.newInstance(int.class,tz390.opt_maxfile);
		zc_copy_rep_lit1     = (String[])Array.newInstance(String.class,tz390.opt_maxfile);
		zc_copy_rep_lit2     = (String[])Array.newInstance(String.class,tz390.opt_maxfile);
		try {
       		mlc_file_buff  = new BufferedWriter(new FileWriter(mlc_file_name));
       		lab_file_buff  = new BufferedWriter(new FileWriter(lab_file_dir + lab_file_name)); // RPI 1062
			zc_file[cur_zc_file] = new File(zc_file_name);
			zc_file_buff[cur_zc_file] = new BufferedReader(new FileReader(zc_file[cur_zc_file]));			
       	} catch (Exception e){
   			abort_error("zcobol file I/O error - " + e.toString());
       	}
       	put_mlc_line(" ","ZCOBOL",zcobol_parms);
       	put_mlc_line(" ","COPY",lab_file_name); // RPI 1062  RPI 1074 RPI 1080
       	zc_comment_copy = true;
       	try {
			zc_data_token_pattern = Pattern.compile(
				  	 // parm in single or double quotes
				   	"([']([^']|(['][']))*['])"  // parm in single quotes // RPI 1062
		    	  +	"|([xX]['][0-9a-fA-F]+['])"   // hex value x'??'
		    	  +	"|([xX][\"][0-9a-fA-F]+[\"])" // hex value x"??"
		    	  +	"|([bB]['][01]+['])"   // binary value b'??'
		    	  +	"|([bB][\"][01]+[\"])" // binary value b"??"
		    	  +	"|([\"]([^\"]|([\"][\"]))*[\"])"  // parm in double quotes // RPI 1062
					 // any parm such as PIC may have leading period and embedded .,- but not '"()=+/*
				  +	"|([\\.]*[^\\s\\.\\,:;\\'\"()=<>\\+\\-\\*\\/]+(([^\\s\\.\\,:;\\'\"()=<>\\+\\*\\/]+)|([\\.\\,][^\\s\\.\\,:;\\'\"()=<>\\+\\-\\*\\/]+))*)"	
				     // COPY REPLACE lit ==???==
				  + "|([=][=][.]+[=][=])"
				     // .,:'"() single special char requiring processing
				  + "|([=][=])|([\\.\\,:;\\'\"()=<>\\+\\-\\*\\/])"            
			);
		} catch (Exception e){
			abort_error("zcobol cbl data token pattern errror - " + e.toString());
		}
       	try {
			zc_proc_token_pattern = Pattern.compile(
				  	 // parm in single or double quotes
				   	"([']([^']|(['][']))*['])"  // parm in single quotes // RPI 1062
		    	  +	"|([xX]['][0-9a-fA-F]+['])"   // hex value x'??'
		    	  +	"|([xX][\"][0-9a-fA-F]+[\"])" // hex value x"??"
		    	  +	"|([bB]['][01]+['])"   // binary value b'??'
		    	  +	"|([bB][\"][01]+[\"])" // binary value b"??"
		    	  +	"|([\"]([^\"]|([\"][\"]))*[\"])"  // parm in double quotes // RPI 1062
					 // any parm similar to data but without embedded commas
				  +	"|([\\.]*[^\\s\\.\\,:;\\'\"()=<>\\+\\-\\*\\/]+(([^\\s\\.\\,:;\\'\"()=<>\\+\\*\\/]+)|([\\.][^\\s\\.\\,:;\\'\"()=<>\\+\\-\\*\\/]+))*)"	
				     // COPY REPLACE lit ==???==
				  + "|([=][=][.]+[=][=])"
				     // .,:'"() single special char requiring processing
				  + "|([=][=])|([\\.\\,:;\\'\"()=<>\\+\\-\\*\\/])"            
			);
		} catch (Exception e){
			abort_error("zcobol cbl proc token pattern errror - " + e.toString());
		}
		try {
			zc_name_pattern = Pattern.compile(
				  	 // data name starting with 0-9 and
					 // containing letters or dashes
					 // but no dashes on end
					 // note 1E1 etc. is allowed in PIC mode
					 // any parm such as PIC may have leading period and embedded .,- but not '"()=+/*
				   	"[0-9]+(([a-zA-Z]+)|([\\-]+[a-zA-Z0-9]+))+"	
           
			);
		} catch (Exception e){
			abort_error("zcobol cbl data name pattern errror - " + e.toString());
		}
		ws_indent[0] = "  ";
		index = 1;
		while (index < 50){
			ws_indent[index] = ws_indent[index-1] + "  ";
		    index++;
		}
	}	
	private void term_zc(){
		/*
		 * 1.  Add PROCEDURE END and END
		 *     to mlc meta file.
		 * 2.  Display statistics.
		 * 3.  Exit
		 * 
		 */
		put_mlc_line(" ","PROCEDURE","END");
		put_mlc_line(" ","END","");
		try {
			zc_file_buff[0].close();
			mlc_file_buff.close();
			lab_file_buff.close(); // RPI 1062
    	} catch (Exception e){
    		abort_error("zcobol file close error " + zc_file_name);
    	}
        if (tz390.opt_stats){
            int index = 0;
            while (index < tot_cpz_file_name){
                    String xref_msg = "FID=" + tz390.right_justify(""+(index+1),3)
                                                    + " " + cpz_file_name[index];
                    tz390.put_stat_line(xref_msg);
                    index++;
            }
    		tz390.put_stat_line("total CBL lines in  =" + tot_cbl);
    		tz390.put_stat_line("total MLC lines out =" + tot_mlc);
    		tz390.put_stat_line("total errors        =" + tot_err);
    	}
  		tz390.close_systerm(zc390_rc);
        System.exit(zc390_rc);
	}
	private void put_copyright(){
		   /*
		    * display zc390 version, timestamp,
		    * and copyright on statstics file
		    */
		   	if  (tz390.opt_stats){
		   	    tz390.put_stat_line("Copyright 2011 Automated Software Tools Corporation");
		   	    tz390.put_stat_line("z390 is licensed under GNU General Public License");
		   	    tz390.put_stat_line("program = " + tz390.dir_mlc + tz390.pgm_name);
		   	    tz390.put_stat_line("options = " + tz390.cmd_parms);
		   	}
	       }
	private void put_mlc_line(String put_lab,String put_op,String put_parms){
		/*
		 * 1.  write line to MLC file
		 * 2.  if LABEL macros for PROCEDURE DIVISION
		 *     section and paraggraph names,
		 *     write to CPY file concatenated 
		 *     in front of MLC to preload all
		 *     label definitions for compile.
		 * 
		 * Notes:
		 *   1.  If first char is " and not data_div
		 *       assume comment-entry and make comment line
		 */
		String put_line;
		if (put_lab.charAt(0) == '*'){
			put_line = put_lab;
		} else if (zc_comment_copy && put_op.equals("COPY")){
			put_line = "*         " + put_op + " " + put_parms;
		} else if (put_lab.charAt(0) == ' '){
			put_line = "         " + tz390.left_justify(put_op,5) + " " + put_parms;
	        if (put_op.equals("LABEL")){
	        	put_text_rec(lab_file_buff,put_line);
	            tot_lab++;
	        }
		} else {
			put_line = tz390.left_justify(put_lab,8) + " " + tz390.left_justify(put_op,5) + " " + put_parms;
		}
		if (tz390.opt_traces){
			System.out.println("put mlc " + put_line);
		}
        put_text_rec(mlc_file_buff,put_line);
        tot_mlc++;
;	}
	private void put_text_rec(BufferedWriter file_buff,String text){
		/* 
		 * write text record to MLC or CPY file
		 */
		try {
			if (text.length() >=72){
				file_buff.write(text.substring(0,71) + "X\r\n");
				int index = 71;
				while (text.length()-index > 56){
					file_buff.write("               " + text.substring(index,index+56) + "X\r\n");
					index = index + 56;
				}
				file_buff.write("               " + text.substring(index) + "\r\n");
			} else {
				file_buff.write(text + "\r\n");
			}
            tot_mlc++;
		} catch (Exception e){
			abort_error("write MLC file error - " + e.toString());
		}
	}
	private void abort_error(String msg){
		/*
		 * display error and terminate
		 */
        tot_err++;
		zc390_rc = 16;
		msg = "ZC390E abort " + msg + " on line " + tot_cbl;
		System.out.println(msg);
        tz390.put_systerm(msg);
        put_mlc_line("* " + msg,"",""); // RPI 1042
		tz390.close_systerm(zc390_rc);
		System.exit(zc390_rc);
	}
	private void log_error(String msg){
		/*
		 * display error and terminate
		 */
        tot_err++;
        if (zc390_rc < 12){
        	zc390_rc = 12;
        }
		msg = "ZC390E " + msg + " on line " + tot_cbl;
		System.out.println(msg);
		tz390.put_systerm(msg);
		put_mlc_line("* " + msg,"","");
	}
	private void get_zc_token(){
		/*
		 * 1.  Set zc_token to next logical 
		 *     cobol token including continued literal token
		 *     split across 1 or more lines and non-literal
		 *     tokens continued at next non-blank char on
		 *     continuation line.
		 * 3   Set to null at end of file.
		 * 4.  Set zc_token_area to A for token
		 *     starting in col 8-11.
		 * 5.  Set zc_token_area to B for token
		 *     starting in 12-72.
		 * 4.  If token not in quotes, make upper-case.
		 * 5.  If quotes
		 *     a.  Convert & to &&
		 *     b.  If orig ", convert "" to ", ' to ''
		 */
		int index;
		if (zc_next_token == null || zc_flush_cont_token){
			zc_flush_cont_token = false;
			if (!zc_eof){
				set_next_token();
                next_to_prev();
			}
			if (zc_eof){
				// return null at eof
				zc_token = null;
				return;
			}
		} else if (zc_next_token.equals("''") 
				   && zc_line.length() > 7
				   && zc_line.charAt(6) == '-'){
			// append ""..." or ''...' to prior token on mlc_line
			index = zc_line.lastIndexOf(zc_split_char);
			if (index > zc_next_index+1){  // skip skip ''
				// append last lit to prev
				zc_prev_token = "'" + zc_split_char + zc_line.substring(zc_next_index,index) + '\'';
			} else {
				zc_prev_token = "'" + zc_split_char + "'";
			}
			rep_amp_sq_dq();
			mlc_parms = mlc_parms.substring(0,mlc_parms.length()-1) + zc_prev_token.substring(1);
			// restart matcher after ending "/' 
			// and exit split mode
			if (zc_proc_div){
			   zc_token_match = zc_proc_token_pattern
			   .matcher(zc_line.substring(index+1));
			} else {
				zc_token_match = zc_data_token_pattern
				.matcher(zc_line.substring(index+1));
			}
			zc_match_offset = index+1;
			zc_split_lit  = false;  // end split lit at quote
			zc_split_char = '\'';   // reset to default
			set_next_token();
			next_to_prev();
		} else {
            next_to_prev();
		}
		if (zc_prev_token != null
			&& zc_prev_token.equals("'")){ 
			// start split literal with single " or ' 
			zc_split_lit  = true;
			zc_prev_token = "'" + zc_line.substring(zc_prev_index);	
			zc_line = null;
			set_next_token();
			while (zc_split_lit
				&& zc_line != null
				&& zc_line.length() > 7
				&& zc_line.charAt(6) == '-'
				&& zc_next_token != null
				&& (zc_next_token.charAt(0) == '\''
		            || zc_next_token.charAt(0) == '=') ){ 
				// we have continuation of literal '..' ".." or ==..==
				// in zc_next_token from next line
				if (zc_next_token.length() == 1){
					// add rest of continued line
					index = zc_line.lastIndexOf(zc_split_char);
				    zc_prev_token = zc_prev_token + zc_line.substring(index+1); 
					zc_line = null;
					set_next_token();
				} else if (zc_next_token.length() == 2){ // RPI 1062 support - ""..." or ''...'
					// we have continuation ""... or ''... to insert " or ', or "". end
					zc_prev_token = zc_prev_token + zc_split_char;
					index = zc_line.lastIndexOf(zc_split_char);
					if (index > zc_next_index){  // skip 6 to pos 8 and 2 to skip ''
						// append last lit to prev
						zc_prev_token = zc_prev_token + zc_line.substring(zc_next_index,index) + '\'';
					} else {
						zc_prev_token = zc_prev_token + "'";
					}
					// restart matcher after ending "/' 
					// and exit split mode
					if (zc_proc_div){
					   zc_token_match = zc_proc_token_pattern
					   .matcher(zc_line.substring(index+1));
					} else {
						zc_token_match = zc_data_token_pattern
						.matcher(zc_line.substring(index+1));
					}					
					zc_match_offset = index+1;
					zc_split_lit  = false;  // end split lit at quote
					zc_split_char = '\'';   // reset to default
					zc_flush_cont_token = true; // flush continue token appended to prev.	
				} else {
					// add to end of literal
					if (zc_prev_token.charAt(zc_prev_token.length()-1) == '\''){
						zc_prev_token = zc_prev_token.substring(0,zc_prev_token.length()-1) + zc_next_token.substring(1,zc_next_token.length()-1) + '\''; // RPI 1062
					} else {
						zc_prev_token = zc_prev_token.substring(0,zc_prev_token.length()) + zc_next_token.substring(1,zc_next_token.length()-1) + '\''; // RPI 1062
					}
					zc_split_lit  = false;  // end split lit at quote
					zc_split_char = '\''; // RPI 1062 reset to default
					zc_flush_cont_token = true; // flush continue token appended to prev.									
				}	
			}
	        if (zc_split_lit){
	        	log_error("zcob split literal format error");
	        	zc_split_lit = false;
	        }
	        zc_prev_area = 'B';
			// we have complete literal in zc_prev_token
		} else {
			// prev token not split literal
			if (!pic_mode){
				if  (zc_prev_token != null 
				     && (zc_prev_token.toUpperCase().equals("PIC")
					     || zc_prev_token.toUpperCase().equals("PICTURE"))){
				     pic_mode = true;
				     pic_token_cnt = 0;
				}
			} else if (pic_token_cnt > 1){
				pic_mode = false; // turn off after processing token after pic
			}
			if (!value_mode){
				if  (zc_prev_token != null 
				     && (zc_prev_token.toUpperCase().equals("VALUE"))){
				     value_mode = true;
				}
			}
			set_next_token();
		}
		if (zc_prev_token.charAt(0) != '\'' ){
			zc_prev_token = zc_prev_token.toUpperCase();
		} else if (zc_prev_token.length() > 2){
            rep_amp_sq_dq();
		}
        // we now have a complete prev token
		zc_token      = zc_prev_token;
		zc_token_index = zc_prev_index;
		zc_token_area = zc_prev_area;
		zc_token_first = zc_prev_first;
		zc_token_line_cnt = zc_prev_line_cnt;
		return;
	}
	private void rep_amp_sq_dq(){
		/*
		 * replace & with &&
		 * replace "" with "
		 * replace ' with '' if split_char = "
		 */
		zc_prev_token = tz390.find_amp.matcher(zc_prev_token).replaceAll("&&"); // RPI 1080
		if (zc_split_char == '"'){
			zc_prev_token = tz390.find_squote.matcher(zc_prev_token.substring(1,zc_prev_token.length()-1)).replaceAll("''"); // RPI 1080
			zc_prev_token = "'" + zc_prev_token + "'";
			zc_prev_token = tz390.find_ddquote.matcher(zc_prev_token).replaceAll("\""); // RPI 1080
		}
	}
	private void next_to_prev(){
		/*
		 * move next token to prev token
		 */
		zc_prev_token = zc_next_token;
		zc_prev_index = zc_next_index;
		zc_prev_area  = zc_next_area;
		zc_prev_first = zc_next_first;
		zc_prev_line_cnt = zc_next_line_cnt;
	}
	private void set_next_token(){
		/*
		 * get next token from CBL file
		 * and set pending CBL comment line
		 */
		if (zc_line != null){
			if (zc_next_token != null){
				find_next_token();
			}
		}
		while (!zc_eof 
				&& zc_line == null){
			get_zc_line();
			if (!zc_eof){ 
				if (zc_line.length() < 8){
					// ignore short lines
					zc_line = null;
				} else {
					if (zc_line.length() > 72){
				    	zc_line = zc_line.substring(0,72);
					}
					if  (zc_line.charAt(6) == '*'      
						|| zc_line.charAt(6) == '/'
						|| zc_line.charAt(6) == 'D'){ // RPI 1062
                         put_mlc_comment();
					} else if ((zc_line.length() >= 14 
							    && zc_line.substring(7).toUpperCase().equals("AUTHOR."))
							 || (zc_line.length() >= 16
								&& zc_line.substring(7).toUpperCase().equals("SECURITY."))
							 || (zc_line.length() >= 20
								&& zc_line.substring(7).toUpperCase().equals("INSTALLATION."))
							 || (zc_line.length() >= 20
								&& zc_line.substring(7).toUpperCase().equals("DATE-WRITTEN."))
							 || (zc_line.length() >= 23
								&& zc_line.substring(7).toUpperCase().equals("SOURCE-COMPUTER.")) // RPI 1062
							 || (zc_line.length() >= 23
								&& zc_line.substring(7).toUpperCase().equals("OBJECT-COMPUTER.")) // RPI 1062
					         ){
						put_mlc_comment();
					    zc_pg_comment_mode = true;
					} else if (zc_pg_comment_mode){
						if (zc_line.length() > 12
							&& zc_line.substring(7,11).equals("    ")){
							put_mlc_comment();
						} else if (zc_line.length() > 13
								   && zc_line.substring(7,12).equals("DATA ")){
							zc_pg_comment_mode = false;
						} else {
							int index = zc_line.substring(7).indexOf('.');
							if (index > 0){
								zc_pg_comment_mode = false;
							}
						}
					}
				} 
			}
			if  (zc_line != null){
				if (zc_copy_trailer){
					zc_copy_trailer = false;
				} else {
					if (zc_proc_div){
					   zc_token_match = zc_proc_token_pattern
					   .matcher(zc_line.substring(7));
					} else {
						zc_token_match = zc_data_token_pattern
						.matcher(zc_line.substring(7));
					}
					zc_match_offset = 8;
				}
		        find_next_token();
			}
		}
		if (zc_eof){
			zc_next_token = null;
		} else {
			if (tz390.opt_traceall){
				put_mlc_line("* trace get next token = " +zc_next_token,"","");
			}
		}
	}
	private void put_mlc_comment(){
		/*
		 * put zc_line as comment on MLC
		 */
		if (zc_comment_pending){
			put_mlc_line(zc_comment_line,"","");
		}
		zc_comment_pending = true;
		zc_comment_line = "*" + zc_line;
		zc_line = null;
	}
	private void get_zc_line(){
			/*
			 * 1.  read next CBL file line
			 * 2.  set cbl_eof if end of file
			 * 3.  write each line as comment on MLC
			 * 4.  ignore lines < 8 or blank
			 * 5.  ignore comment lines with non-space in 7
			 */			
		get_next_zc_line();
		boolean zc_line_blank = true;
		while (zc_line != null && zc_line_blank) {
			tot_cbl++;
			if (zc_line.length() >= 7) {
				zc_line_blank = false;
			} else {
				// ignore blank lines
				get_next_zc_line();
			}
		}
		if (zc_line != null) {
			zc_token_count = 0;
		} else {
			zc_eof = true;
			return;
		}
	}
	private void zc_cbl_comment(String text){
		/*
		 * display CBL as comments if ZC_COMMENT
		 */
		if (text != null 
			&& zc_comment // request to gen CBL comments
			&& (text.length() < 7 
				|| text.charAt(6) != '*')) {
			if (zc_comment_pending) {
				put_mlc_line(zc_comment_line,"","");
			}
			zc_comment_pending = true;
			zc_comment_line = "* " + text;
			zc_comment_cnt = tot_cbl;
		}
	}
	private void get_next_zc_line(){
		/*
		 * get next zc_line from nested copy files
		 */
		if (request_dfheiblk){
			request_dfheiblk = false;
			zc_line = "         COPY DFHEIBLK."; // RPI 1062
			return;
		}
			if (zc_copy_trailer){
				zc_copy_trailer = false;
			} else {
                get_zc_read_cont();
				if (zc_line != null){
					cur_rep_ix = zc_copy_rep_fst_ix[cur_zc_file];
					if (cur_rep_ix > 0){
						while (cur_rep_ix <= zc_copy_rep_lst_ix[cur_zc_file]){
							zc_line = zc_line.replaceAll(zc_copy_rep_lit1[cur_rep_ix],zc_copy_rep_lit2[cur_rep_ix]); // RPI 1080
							cur_rep_ix++;
						}
					}
				} else if (zc_copy_line[cur_zc_file] != null){ 
					zc_copy_trailer = true;
					zc_line = zc_copy_line[cur_zc_file]; // RPI 1062 add +1 for all 3
					zc_copy_line[cur_zc_file] =null;
					if (zc_proc_div){
					   zc_token_match = zc_proc_token_pattern
					   .matcher(zc_line.substring(zc_copy_line_ix[cur_zc_file]));
					} else {
						zc_token_match = zc_data_token_pattern
						.matcher(zc_line.substring(zc_copy_line_ix[cur_zc_file]));
					}
					zc_match_offset = zc_copy_line_ix[cur_zc_file]+1;

				}
			}
			while (zc_line == null && cur_zc_file > 0){
				try {
					zc_file_buff[cur_zc_file].close();
				} catch (Exception e){
					abort_error("zcobol read error on CBL/CPY close file - " + e.toString());
				}
				if (zc_copy_rep_fst_ix[cur_zc_file] >= 0){
					// restore current rep index for use in next copy
					zc_copy_rep_ix = zc_copy_rep_fst_ix[cur_zc_file];
				}
				cur_zc_file--;
				if (dfheiblk_loading){
					dfheiblk_loading = false;
					flush_last_mlc_line();
					dfheiblk = true;
				}
			    if (request_proc){   	
			    	gen_data_end();  // RPI 1086
		    	}
			    get_zc_read_cont();			    
			}
			if (zc_line != null && zc_line.length() > 72){
				zc_line = zc_line.substring(0,72);
			}
	}
	private void get_zc_read_cont(){
		/*
		 * read next zc_line with concatenated
		 * non split lit continuations
		 */
		try {
    	    zc_line = zc_file_buff[cur_zc_file].readLine();
    	    zc_cbl_comment(zc_line);
	    	if (zc_line != null && !zc_split_lit){
	    		zc_file_buff[cur_zc_file].mark(100);
	    		zc_line_lookahead = zc_file_buff[cur_zc_file].readLine();
	    		boolean cont_split_lit = false;
	    		while (zc_line_lookahead != null
	    				&& !cont_split_lit
	    				&& zc_line_lookahead.length() > 7 
	    			    && zc_line_lookahead.charAt(6) == '-'){
	    			zc_cbl_comment(zc_line_lookahead);
	    			if (zc_line_lookahead.length() > 72){
	    				zc_line_lookahead = zc_line_lookahead.substring(0,72);
	    			}
	    			int index = 7;
	    		    while (index < zc_line_lookahead.length()
	    		    	&& zc_line_lookahead.charAt(index) == ' '){
	    		    	index++;
	    		    }
	    		    if (index < zc_line_lookahead.length()
	    		    	&& zc_line_lookahead.charAt(index) != '\''
	    		    	&& zc_line_lookahead.charAt(index) != '"'){
	    		        zc_cbl_comment(zc_line_lookahead);
	    		    	zc_line = zc_line.concat(zc_line_lookahead.substring(index));
	    		    	tot_cbl++;
	    		    	zc_file_buff[cur_zc_file].mark(100);
	    		    	zc_line_lookahead = zc_file_buff[cur_zc_file].readLine();	    
	    		    } else {
	    		    	cont_split_lit = true;
	    		    }
	    		}
	    		if (zc_line_lookahead != null && zc_line_lookahead.length() > 0){
	    			zc_file_buff[cur_zc_file].reset();
	    		}
	    	}
		} catch (Exception e){
			abort_error("zcobol read error on CBL/CPY file - " + e.toString());
			zc_line = null;
		}
	}
	private void flush_comment_line(){
		/*
		 * write pending CBL line comment
		 * if pending
		 */
		if (zc_comment_pending
			&& zc_comment_cnt <= zc_token_line_cnt){
			put_mlc_line(zc_comment_line,"","");
			zc_comment_pending = false;
		}
	}
	private void find_next_token(){
		/*
		 * find next token in zc_line
		 * else set zc_line = null
		 */
		if (!zc_token_match.find()){
			zc_line = null;
		} else {
			zc_token_count++;
			if (zc_token_count == 1){
				zc_next_first = true;
				value_mode = false; // RPI 1126
			} else {
			    zc_next_first = false;
			}
			pic_token_cnt++;
			zc_next_token = zc_token_match.group();
			zc_next_index = zc_token_match.start() + zc_match_offset;
			zc_next_line_cnt = tot_cbl;
			if (zc_token_match.start() + zc_match_offset < 12 && zc_next_first){
				zc_next_area = 'A';
			} else {
				zc_next_area = 'B';
			}
			if (zc_next_token.length() >= 2 
				&& zc_next_token.charAt(0) == '"'){
				zc_split_char = '"'; // RPI 1062
				zc_next_token = "'" + zc_next_token.substring(1,zc_next_token.length()-1)+"'"; // cvt ".." to '..'					
			} else if (zc_next_token.equals("\"")){
				zc_split_char = '"'; // RPI 1062
				zc_next_token = "'";
			} else if (zc_next_token.toUpperCase().equals("COPY")){
				process_copy();
			}
			if (pic_mode){
				int index2 = zc_next_index + zc_token_match.group().length()-1;
				while (zc_line.length() > index2
					   && zc_line.charAt(index2) > ' '
				       && (zc_line.charAt(index2) != '.' 
				    	   || (zc_line.length() > index2+1 
			    			   && zc_line.charAt(index2+1) > ' '
				    	      )
				    	   )    
				    	&& zc_token_match.find()){
					  zc_next_token = zc_next_token + zc_token_match.group();
				      zc_next_index = zc_token_match.start() + zc_match_offset;
				      index2 = zc_next_index + zc_token_match.group().length()-1;
				}
			} else if (zc_next_token.charAt(0) <= '9' 
				       && zc_next_token.charAt(0) >= '0'){
				// convert data names starting with digit to #name
				zc_name_match = zc_name_pattern
				.matcher(zc_next_token);
				if (zc_name_match.find()){
					zc_next_token = "#" + zc_next_token; // RPI 1062
				}
			}
		}
	}
	private void process_zc_token(){
		/*
		 * process zc_token
		 *   1.  If token length > 1 and not literal
		 *       replace - with _ and if ., included
		 *       wrap in single quotes.
		 *   2.  Single char processing
		 *       a.  Flush line at . and gen PERIOD if Proc. div.
		 *           else ignore period.
		 *       b.  Ignore commas.
		 *       c.  If '" wrap in opposite quotes
		 *       d.  If () wrap in single quotes. 
		 *       e.  comma or semicolon ignored         
		 */
		if (zc_token.length() > 1){
			if (zc_token.charAt(0) != '\''
				&& zc_token.charAt(0) != '"'){
				if (zc_token.indexOf(',') >= 0){// ALLOW . IN OPEN PARMS zc_token.indexOf('.') >= 0 
					// wrap strings with ., in quotes to make single parm
					zc_token = "'" + zc_token + "'";				
				} else if (!pic_mode && !value_mode && zc_token.indexOf('-') >= 0){ // RPI 1126 allow nE-n value
			    	// replace token "-" with "_"
			    	// for z390 meta macro assembler compatiblity
		    		zc_token = tz390.find_dash.matcher(zc_token).replaceAll("_");  // RPI 1080
				} 			    
			}
		} else if (zc_token.length() == 1){
			// single char token . or ,
			if (zc_token.charAt(0) == '.'){
				if (mlc_op != null
					&& mlc_parm_cnt >= 1){
					if (exec_mode){
						log_error("EXEC statement missing END-EXEC");
					    exec_mode = false;
					    exec_parm_index = 0;
					}
					// flush line at period after parms
					// and generate PERIOD in proc div
					if (zc_proc_div && !skip_period){
						new_mlc_line(" ","PERIOD","");
					}
					flush_last_mlc_line();
					zc_level = 0;
				}
				skip_period = false;
				// ignore period
				return;
			} else if (zc_token.charAt(0) == ','
				       || zc_token.charAt(0) == ';'){
				// ignore commas and semicolons
				return;
			} else if (zc_token.charAt(0) == '\''){
				zc_token = "'" + zc_token + "'";
			} else if (zc_token.charAt(0) == '"'){
				zc_token = "\"" + zc_token + "\"";
 			} else if (zc_token.charAt(0) == '('){
 				zc_level++;
 				zc_token = "'('";
		    } else if (zc_token.charAt(0) == ')'){
 				zc_level--;
		    	zc_token = "')'";	
		    }
			// may be single char label 
		} else {
			abort_error("zero length token parsing error");
		}
		if (zc_token_area == 'A'){
			// non procedure division section operation
			// or procedure division label
			if (zc_proc_div 
				&& zc_next_token != null // RPI 1062
				&& (zc_next_token.equals(".")            // RPI 1012
					|| zc_next_token.toUpperCase().equals("SECTION"))){ // RPI 1012	RPI 1063			
				// gen procedure div label
				new_mlc_line(" ","LABEL",zc_token); // RPI 1086
				mlc_parm_cnt = 1;  // RPI 1062 append SECTION if present to first parm anme
				skip_period = true;
			} else {
				if  (zc_token.charAt(0) >= '0' 
					 && zc_token.charAt(0) <= '9'){
					new_ws_line();
				} else {
					if (zc_token.equals("DATA")){
						data_div = true;
					} else if (zc_token.equals("LINKAGE")){
						linkage_sect = true;
					} else if (zc_token.equals("PROCEDURE")){
						if (!data_div){
                            zc_comment_pending = false; // cancel proc div comment
							data_div = true;
							put_mlc_line(" ","DATA","DIVISION");
						}
						if (zc_cics){
							// check if PROCEDURE statement needs
							// to be preceeded by generated 
							// DFHEIBLK and DFHCOMMAREA linkage sections
							if (!linkage_sect){
	                            zc_comment_pending = false;
								linkage_sect = true;
								put_mlc_line(" ","LINKAGE","SECTION");
							}
							if (!dfheiblk){
								request_dfheiblk = true;
							}
							if (!dfhcommarea){
								dfhcommarea = true;
								put_mlc_line(" ","WS","01,DFHCOMMAREA");
							}
							if (!dfheiblk){
								set_proc_using_parms();
								zc_comment_pending = false;
								request_proc = true;
								return;
							}
						} 
						gen_data_end();  // RPI 1086
					} else if (zc_token.equals("SD")){
						zc_token = "ZCSD"; // RPI 1062
					} else if (zc_token.equals("END")){ // RPI 1062
                        zc_token = "ZCEND";
					}
					new_mlc_line(" ",zc_token,"");
				}
			}
		} else if (find_verb()){
			if (zc_token_first){
				flush_last_mlc_line();
				flush_comment_line();
			}
            set_zc_line_id_num(); // RPI 1086
			new_mlc_line(" ",zc_token,"");	
			if (zc_next_token.equals(".")){
				new_mlc_line(" ","PERIOD","");	
			}		
		} else {
			// assume parm for prior verb
			if (mlc_op == null){
				// assume unknown verb starting new line
				flush_comment_line();
				if (!zc_proc_div 
					&& zc_token.charAt(0) >= '0' 
					&& zc_token.charAt(0) <= '9'){
					new_ws_line();
				} else {
					set_zc_line_id_num(); // RPI 1086
					new_mlc_line(" ",zc_token,"");
				}
			} else {
				add_mlc_parm(zc_token);
			}
		}
	}
	private void gen_data_end(){
		/*
		 * gen DATA END after any cics generated COPY
		 * for DFHEIBLK or DFHCOMMAREA
		 */
		put_mlc_line(" ","DATA","END");	
		if (request_proc){
			request_proc = false;
			if (proc_using_parms.length() > 0){
				put_mlc_line(" ","PROCEDURE","DIVISION,USING,DFHEIBLK,DFHCOMMAREA," + proc_using_parms);
			} else {
				put_mlc_line(" ","PROCEDURE","DIVISION,USING,DFHEIBLK,DFHCOMMAREA");
			}
		}
		zc_proc_div = true;
		skip_period = true;
	}
	private void set_zc_line_id_num(){
		/*
		 * set zc_line_id and zc_line_num
		 */
		zc_line_id = zc_line.substring(0,7);  // RPI 1086
		zc_line_num = tz390.right_justify("" + tot_cbl,6); // RPI 1086
	}
	private void set_proc_using_parms(){
		/*
		 * 1.  insert DFHEIBLK and DFHCOMMAREA parms
		 * 2.  add any user parms after above.
		 * 3.  flush through period.
		 */
		proc_using_parms = "";
		get_zc_token(); 
		while (!zc_token.equals(".")){
			if (   !zc_token.equals("DIVISION")
				&& !zc_token.equals("USING")
				&& !zc_token.equals("DFHEIBLK")
				&& !zc_token.equals("DFHCOMMAREA")
				&& !zc_token.equals(",")){
				proc_using_parms = proc_using_parms + " " + zc_token;
			}
			get_zc_token();
		}
	}
	private void new_ws_line(){
		/*
		 * 1.  If new level # less than
		 *     current level #, 
		 *     decrement group level #.
		 * 2.  Generate new WS verb with
		 *     level # as first parm
		 *     indented to current group level.
		 * 
		 */
		int ws_item_lvl = 0;
		try {
			ws_item_lvl = Integer.valueOf(zc_token);
		} catch (Exception e){
			log_error("invalid WS level number");
			ws_item_lvl = 1;
		}
		switch (ws_item_lvl){
		case 1:
			if (zc_token_area != 'A'){
				log_error("zcobol level 01 must be in A field");
			}
			ws_lvl_index = 0;
			ws_lvl[0] = ws_item_lvl;
			break;
		case 66:  // redefine
			ws_lvl_index = 0;
			ws_lvl[0] = ws_item_lvl;
			break;
		case 77:  // item
			ws_lvl_index = 0;	
			ws_lvl[0] = ws_item_lvl;
			break;
		case 88:  // value 
			// use current level / indent
			break;
		default: // 02-49 
			if (ws_item_lvl < 1 || ws_item_lvl > 49){
				log_error("zcobol invalid ws level " + ws_item_lvl);
			}
			if (ws_item_lvl > ws_lvl[ws_lvl_index]){
				ws_lvl_index++;
			} else {
				while (ws_lvl_index > 0
						&& ws_item_lvl < ws_lvl[ws_lvl_index]){
					ws_lvl_index--;
				}
			}
			ws_lvl[ws_lvl_index] = ws_item_lvl;
		}
		new_mlc_line(" ","WS","" + ws_indent[ws_lvl_index] + zc_token);
		mlc_parm_cnt = 1;
		if (zc_token.equals("01") && zc_next_token.equals("DFHCOMMAREA")){
			dfhcommarea = true;
		}
	}
    private void new_mlc_line(String new_lab, String new_op, String new_parms){
    	/*
    	 * write pending mlc line
    	 * and start new line
    	 * and reset parm count
    	 */
        flush_last_mlc_line();
		mlc_lab   = new_lab;
		mlc_op    = new_op;
		mlc_parms = new_parms;
		mlc_parm_cnt = 0;
    }
    private void flush_last_mlc_line(){
    	/*
    	 * write pending mlc_line if any
    	 */
    	if (mlc_op != null){
            put_zc_line();  // RPI 1086
    		put_mlc_line(mlc_lab,mlc_op,mlc_parms);
    	}
    	mlc_op = null;
    }
    private void put_zc_line(){
    	/*
    	 * put zcobol call source comment 
    	 */
		if (zc_proc_div){ 
    		if (zc_comment
    			&& !mlc_op.equals("PERIOD")
    			&& !mlc_op.equals("LABEL")
    			){
    			put_mlc_line("*ZC " + zc_line_num + " " + zc_line_id + " " + tz390.left_justify(mlc_op,5) + " " + mlc_parms,"",""); // RPI 1086
    		}
    	}
    }
    private void add_mlc_parm(String token){
    	/*
    	 * add token parm to mlc_line
    	 * Notes:
    	 *   1.  If exec_mode and parm,
    	 */
    	if (mlc_parm_cnt == 0) {
			mlc_parms = zc_token;
		} else if (!exec_mode){
			mlc_parms = mlc_parms + "," + zc_token;
		} else {
			if (zc_token.compareTo("END_EXEC") == 0){
				gen_exec_stmt();
				zc_token_first = true;
			} else {
		        exec_parm[exec_parm_index] = zc_token;
				exec_parm_index++;						
			}
		}
    	mlc_parm_cnt++;
    }
    private void gen_exec_stmt(){
    	/*
    	 * generate EXEC call with parm(value)
    	 * parameters combined
    	 */
    	int index = 0;
    	while (index < exec_parm_index){
			if (exec_parm[index].equals("'('")){
				mlc_parms = mlc_parms + "(" + exec_parm[index+1];
				index = index + 2;
				while (index < exec_parm_index 
					&& exec_parm[index] != "')'"){
					mlc_parms = mlc_parms + "," + exec_parm[index];
					index++;
				}
			    mlc_parms = mlc_parms + ")";
			    index = index + 1;
			} else {
				mlc_parms = mlc_parms + "," + exec_parm[index];
                index++;
			}
    	}  
		put_mlc_line("*ZC " + zc_line_num + " " + zc_line_id + " " + tz390.left_justify(mlc_op,5) + " " + mlc_parms,"",""); // RPI 1086
    	put_mlc_line(mlc_lab,mlc_op,mlc_parms);
    	if (zc_next_token.equals(".")){
    		put_mlc_line(" ","PERIOD","");
    	}
		if (!zc_cics 
			&& mlc_op.equals("EXEC")
			&& mlc_parms.length() > 4
			&& mlc_parms.substring(0,4).equals("CICS")){
			abort_error("EXEC CICS statements require CICS option");
		}
    	mlc_op = null;
    	exec_mode = false;
    	exec_parm_index = 0;
    }
	private boolean find_verb(){
		/*
		 * return true if zc_token
		 * is a known COBOL verb
		 */
		if (allow_verb){
			allow_verb = false;
			return false;
		}
		if (exec_mode || zc_level > 0){
			// ignore verbs within exec statements and (...) parms
			return false;
		}
		String key = zc_token.toUpperCase();
		switch (key.charAt(0)){
		case 'A':
			if (key.equals("ACCEPT")
		        || key.equals("ADD")
		        || key.equals("ALPHABET") // RPI 1062 spec. name
		        || key.equals("ALTER")
		        ){
				return true;
			}		
			return false;
		case 'C': 
			if (key.equals("CALL")
			        || key.equals("CANCEL")
			        || key.equals("CLOSE")
			        || key.equals("COMPUTE")
			        || key.equals("CONTINUE")
			        || key.equals("CLASS") // RPI 1062 spec names
			        || key.equals("CURRENCY") // RPI 1062 spec names
			        || key.equals("CURSOR") // RPI 1062 spec names
			        || key.equals("CRT") // RPI 1062 spec names
			        ){
					return true;
				}		
				return false;	
		case 'D': 
			if (key.equals("DIVIDE")  // most frequent
					|| key.equals("DELETE")
					|| key.equals("DISABLE")
			        || key.equals("DISPLAY")
			        || key.equals("DECIMAL_POINT") // RPI 1062 spec name
			        ){
				    if (!zc_proc_div && key.equals("DISPLAY")){
				    	return false; // RPI 1062
				    }
					return true;
				}		
				return false;
		case 'E':
			if (key.equals("EXEC")
			    ){
				exec_mode = true;				
				return true;
			}
			if (key.equals("END_EXEC")){
				log_error("zcob END_EXEC not preceeded by EXEC statement");
				return true;
			}
			if (key.equals("EJECT")
				|| key.equals("ELSE")
				|| key.equals("ENABLE")
				|| key.equals("END_ADD")
				|| key.equals("END_DIVIDE")
				|| key.equals("END_EVALUATE")
				|| key.equals("END_IF")
				|| key.equals("END_MULTIPLY")
				|| key.equals("END_PERFORM")
				|| key.equals("END_READ")
				|| key.equals("END_SUBTRACT")
				|| key.equals("ENTRY")
				|| key.equals("EVALUATE")
			    || key.equals("EXAMINE")  // OS/VS replaced by INSPECT
				|| key.equals("EXHIBIT")  // OS/VS replaced by DISPLAY
		        || key.equals("EXIT")
			    ){
				return true;
			}
			return false;
		case 'G': 
			if (key.equals("GENERATE")
					|| key.equals("GO")
					|| key.equals("GOBACK")
			        ){
					return true;
				}		
				return false;
		case 'I': 
			if (key.equals("IF")
			        || key.equals("INITIALIZE")
			        || key.equals("INITIATE")
			        || key.equals("INSPECT")
			        ){
					return true;
				}		
				return false;
		case 'L': 
			if (key.equals("LOCALE")
			        ){
					return true;
				}		
				return false;
		case 'M': 
			if (key.equals("MERGE")
			        || key.equals("MOVE")
			        || key.equals("MULTIPLY")
			        ){
					return true;
				}		
				return false;
		case 'N': 
			if (key.equals("NEXT")
				|| key.equals("NOTE")
				|| key.equals("NOT")
			    ){
				if (key.equals("NOT")){ // RPI 1062
					if (zc_next_token.equals("ON")){
					    return true;
					} else {
						return false;
					}
				}		
				return true;
			}
			return false;
		case 'O': 
			if (key.equals("OPEN")
				|| key.equals("ORDER") // RPI 1062 spec names
			    ){
					return true;
				}		
				return false;
		case 'P': 
			if (key.equals("PERFORM")){
				return true;
			}
			return false;
		case 'R': 
			if (key.equals("READ")
					|| key.equals("READY")   // OS/VS READY TRACE
					|| key.equals("RECEIVE")
			        || key.equals("RELEASE")
			        || key.equals("REMARKS") // OS/VS replaced with comments
			        || key.equals("RESET")   // OS/VS RESET TRACE
			        || key.equals("RETURN")
			        || key.equals("REWRITE")
			        ){
					return true;
				}		
				return false;	
		case 'S': 
			if (key.equals("SUBTRACT") // MFU first for speed
					|| key.equals("SEARCH")
					|| key.equals("SELECT")
					|| key.equals("SEND")
			        || key.equals("SET")
			        || key.equals("SORT")
			        || key.equals("START")
			        || key.equals("STOP")
			        || key.equals("STRING")
			        || key.equals("SYMBOLIC") // RPI 1062 spec names
			        ){
				    if (key.equals("START")){
				    	zc_token = "ZCSTART"; // avoid assembler START conflist
				    }
					return true;
				}		
				return false;
		case 'T': 
			if (key.equals("TERMINATE")
				|| key.equals("TRANSFORM") // OS/VS REPLACED BY INSPECT
			    ){
				return true;
			}		
			return false;				
		case 'U': 
			if (key.equals("UNSTRING")
			       ){
				return true;
			}		
			return false;	
		case 'W': 
			if (key.equals("WHEN") 
				|| key.equals("WRITE")
			    ){
			    if (!zc_proc_div && key.equals("WHEN")){
			    	return false; // RPI 1062
			    }
				return true;
			}		
			return false;	
		}
		return false;  // UNDEFINED WORD
	}
	private void process_copy(){
		/*
		 * expand copy which may appear
		 * in the middle of sentence with
		 * the following options:
		 * 1.  COPY member (uses SYSCPY paths for dir search)
		 * 2.  COPY member OF/IN library (ddname of dir)
		 * 3.  COPY ... REPLACING lit1 BY lit2
		 *
		 */
		int parm = 0; // count and check  parms to period
		zc_copy_member = null;
		zc_copy_ddname = null;
		if (cur_zc_file + 1 >= tz390.opt_maxfile){
			log_error("maximum nested copy files exceeded");
			set_next_token();
			return;
		}
		zc_copy_line[cur_zc_file]       = null; // assume  no trailing tokens 
		zc_copy_rep_fst_ix[cur_zc_file] = 0; // assume  no REPLACING
		zc_copy_rep_lst_ix[cur_zc_file] = 0; // force exit from while loop when index high
		set_next_token();
		while (!zc_eof 
				&& !zc_next_token.equals(".")){
			switch (parm){
			case 0: // member name
				zc_copy_member = zc_next_token;
				parm = 1;
				break;
			case 1: // OF/IN ddname or REPLACING else error
				if (zc_next_token.toUpperCase().equals("OF")
					|| zc_next_token.toUpperCase().equals("IN")){
					parm = 2;
				} else if (zc_next_token.toUpperCase().equals("REPLACING")){
					parm = 4;
				} else {
					copy_error("unknown COPY parm - " + zc_next_token);
					return;
				}
				break;
			case 2: // ddname following OF/IN
				zc_copy_ddname = zc_next_token;
				parm = 3;
				break;
			case 3: // REPLACING
			    if (zc_next_token.toUpperCase().equals("REPLACING")){
			    	parm = 4;
			    } else {
			    	copy_error("unknown COPY parm - " + zc_next_token);
			    	return;
			    }
			    break;
			case 4: // lit1 or OF/IN lit2 (split pseudo ==...== not supported)
				if (zc_next_token.toUpperCase().equals("OF")
					|| zc_next_token.toUpperCase().equals("IN")){
			    	parm = 7;
			    	return;
			    }
				if (zc_next_token.length() > 2
					&& zc_next_token.charAt(0) == '='){
					zc_next_token = zc_next_token.substring(2,zc_next_token.length()-2);
				} else if (zc_next_token.equals("==")){
					copy_error("split pseudo COPY replacement not supported");
				}
                if (zc_copy_rep_fst_ix[cur_zc_file+1] == 0){
                	zc_copy_rep_fst_ix[cur_zc_file+1] = zc_copy_rep_ix;
                }
                zc_copy_rep_lst_ix[cur_zc_file+1] = zc_copy_rep_ix;
				zc_copy_rep_lit1[zc_copy_rep_lst_ix[cur_zc_file+1]] = zc_next_token;
                zc_copy_rep_ix++;
        		if (zc_copy_rep_ix >= tz390.opt_maxfile){
        			copy_error("maximum nested copy reps exceeded");
        			return;
        		}
				parm = 5;
			    break;
			case 5:
				if (zc_next_token.toUpperCase().equals("BY")){
			    	parm = 6;
			    } else {
			    	copy_error("unknown COPY parm - " + zc_next_token);
			    	return;
			    }
			    break;
			case 6: // store first lit2 token
				zc_copy_rep_lit2[zc_copy_rep_lst_ix[cur_zc_file+1]] = zc_next_token;
			    parm = 4;
			    break;
			case 7: // append OF/IN field to lit2
				zc_copy_rep_lit2[zc_copy_rep_lst_ix[cur_zc_file+1]] = zc_copy_rep_lit2[zc_copy_rep_lst_ix[cur_zc_file+1]] + " " + zc_next_token;
			    parm = 4;
			    break;    
			default:
		    	copy_error("unknown COPY parm - " + zc_next_token);
	    		return;
			}
			set_next_token();
		}
        if (zc_copy_member == null){
	    	copy_error("missing COPY member");
    	   	return;
        } else if (zc_copy_ddname != null){
        	zc_copy_file_name = System.getenv(zc_copy_ddname); 
        	if (zc_copy_file_name == null){
        		copy_error("copy ddname not found - " + zc_copy_ddname);
        		return;
        	}
        	zc_copy_file_name = zc_copy_file_name + File.separator + zc_copy_member + cpz_type;
        } else {
		    zc_copy_file_name = tz390.find_file_name(tz390.dir_cpy,zc_copy_member,cpz_type,tz390.dir_cur);
		    if (zc_copy_file_name == null){ // RPI 970 add key if found
		    	copy_error("COPY file not found - " + zc_copy_file_name);
		    	return;
		    } else if (zc_cics){ 
		    	if (zc_copy_member.equals("DFHEIBLK")){
		    		if (!dfheiblk){
		    			dfheiblk_loading = true;
		    		} else {
		    			put_mlc_line("* ZC390I DUPLICATE COPY DFHEIBLK IGNORED","","");
		    			set_next_token();
		    			return;
		    		}
		    	}
		    }
        }
        cur_zc_file++;
        add_cpz_file();
		try {
			zc_file[cur_zc_file] = new File(zc_copy_file_name);
			zc_file_buff[cur_zc_file] = new BufferedReader(new FileReader(zc_file[cur_zc_file]));
		} catch (Exception e){
			cur_zc_file--;
			copy_error("I/O error opening copy file - " + e.toString());
		    return;
		}
		if (zc_line != null && zc_token_match.find()){
			zc_copy_line[cur_zc_file]    = zc_line; 
			zc_copy_line_ix[cur_zc_file] = zc_token_match.start()+ zc_match_offset - 1; // RPI 1062 
			zc_line = null; // RPI 1062
		}
		set_next_token(); // RPI 1062 get next token from copy file
	}
	private void copy_error(String msg){
		/*
		 * terminate COPY with error and 
	 	* dec cur_zc_file and flush line
	 	*/
		log_error(msg + "\r" + zc_line);
		zc_line = null;
		zc_copy_trailer = false;
	}
	private void add_cpz_file(){
        /*
         * add cpz file for stats list  RPI 1042
         */
        if (!tz390.opt_stats){
                return;
        }
        int index = tz390.find_key_index(
                        'F',zc_copy_file_name);         
        if (index == -1){
                if (tot_cpz_file_name < tz390.opt_maxfile){
                        if (!tz390.add_key_index(tot_cpz_file_name)){
                                abort_error("key search table exceeded adding " + tot_cpz_file_name);
                        }
                        cpz_file_name[tot_cpz_file_name] = zc_copy_file_name;
                        tot_cpz_file_name++;
                }
        }
}
}
