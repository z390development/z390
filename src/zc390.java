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
	
    Copyright 2008 Automated Software Tools Corporation
	 
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
	int    tot_err  = 0; // total errors
	/*
	 * COBOL CBL input file variables
	 */
	String zc_line = null;      // logical line with continuations added
	String zc_file_name = null;
	boolean zc_comment = true; 
	boolean zc_pg_comment_mode = false;
	boolean zc_cics = false;
	boolean zc_extend = true;
	boolean zc_trunc  = false;
	boolean linkage_sect = false;
	boolean request_dfheiblk = false;
	boolean request_dfhcommarea = false;
	boolean request_proc = false;
	boolean dfheiblk = false;
	boolean dfheiblk_loading = false;
	boolean dfhcommarea = false;
	boolean zc_eof = false;
	boolean zc_comment_pending = false;
	int zc_comment_cnt = 0;
	int pic_token_cnt = 0; // tokens since pic mode turned on
	boolean pic_mode = false;
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
	String mlc_line = null;
	String mlc_file_name = null;
	File   mlc_file = null;
	BufferedWriter mlc_file_buff = null;
	/*
	 * Nested COPY file input variables
	 */
	boolean copy_found = false;
	String zc_copy_file_name = null;
	String cpz_type = ".CPZ";
	int cur_zc_file = 0; // 0 is primary CBL input
	File[] zc_file                = null;
	BufferedReader[] zc_file_buff = null;
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
	Pattern zc_pattern = null;   // parsing regular expression pattern
	Matcher zc_match   = null;   // token pattern matching class
	char    zc_token_area = 'A'; // token area A or B
	boolean zc_split_lit = false; // literal split across lines
	boolean zc_proc_div = false; // PRODECUDE DIV started
	String proc_using_parms = null;
	int mlc_parms = 0; // mlc parm count 
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
		init_zc(args);        
		get_zc_token();
		while (zc_token != null){
			process_zc_token();
			get_zc_token();
		}
		if (mlc_line != null){
			put_mlc_line(mlc_line);
		}
		term_zc();
	}
	private void init_zc(String[] args){
		/*
		 * 1.  Display zcobol version
		 * 2.  Compile regular expression pattern
		 * 3.  Open CBL and MLC files
		 */
		tz390 = new tz390();
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
			"PGM="    + args[0]
		  + ",VER="    + tz390.version
		  + ",CDATE=" + tz390.cur_date()
		  + ",CTIME=" + tz390.cur_time(false)
		  + ",OPTIONS=(" + zcobol_options + ")";
		if (tz390.opt_traces){
			System.out.println("ZCOBOL " + zcobol_parms);
		}
	    zc_file_name = args[0] + ".CBL"; 
	    mlc_file_name = args[0] + ".MLC"; 
        mlc_file      = new File(mlc_file_name);
		zc_file              = (File[])Array.newInstance(File.class,tz390.opt_maxfile);
		zc_file_buff         = (BufferedReader[])Array.newInstance(BufferedReader.class,tz390.opt_maxfile);
       	try {
       		mlc_file_buff  = new BufferedWriter(new FileWriter(mlc_file_name));
			zc_file[cur_zc_file] = new File(zc_file_name);
			zc_file_buff[cur_zc_file] = new BufferedReader(new FileReader(zc_file[cur_zc_file]));			
       	} catch (Exception e){
   			abort_error("zcobol file I/O error - " + e.toString());
       	}
       	put_mlc_line("         ZCOBOL " + zcobol_parms);
       	try {
			zc_pattern = Pattern.compile(
				  	 // parm in single or double quotes
				   	"([']([^']|(['][']))+['])"  // parm in single quotes
		    	  +	"|([xX]['][0-9a-fA-F]+['])"   // hex value x'??'
		    	  +	"|([xX][\"][0-9a-fA-F]+[\"])" // hex value x"??"
		    	  +	"|([bB]['][01]+['])"   // binary value b'??'
		    	  +	"|([bB][\"][01]+[\"])" // binary value b"??"
		    	  +	"|([\"]([^\"]|([\"][\"]))+[\"])"  // parm in double quotes
					 // any parm such as PIC may have embedded .,- but not '"()=+/*
				  +	"|([^\\s\\.\\,:;\\'\"()=<>\\+\\-\\*\\/]+(([^\\s\\.\\,:;\\'\"()=<>\\+\\*\\/]+)|([\\.\\,][^\\s\\.\\,:;\\'\"()=<>\\+\\-\\*\\/]+))*)"	 
				     // .,:'"() single special char requiring processing
				  + "|([\\.\\,:;\\'\"()=<>\\+\\-\\*\\/])"            
			);
		} catch (Exception e){
			abort_error("zcobol cbl pattern errror - " + e.toString());
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
		put_mlc_line("         PROCEDURE END");
		put_mlc_line("         END");
		try {
			zc_file_buff[0].close();
			mlc_file_buff.close();
    	} catch (Exception e){
    		abort_error("zcobol file close error " + zc_file_name);
    	}
    	if (tz390.opt_stats){
    		tz390.put_stat_line("total CBL lines in  =" + tot_cbl);
    		tz390.put_stat_line("total MLC lines out =" + tot_mlc);
    		tz390.put_stat_line("total errors        =" + tot_err);
    	}
        if (tot_err == 0){
        	zc390_rc = 0;
        } else {
        	zc390_rc = 8;
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
		   	    tz390.put_stat_line("Copyright 2008 Automated Software Tools Corporation");
		   	    tz390.put_stat_line("z390 is licensed under GNU General Public License");
		   	    tz390.put_stat_line("program = " + tz390.dir_mlc + tz390.pgm_name);
		   	    tz390.put_stat_line("options = " + tz390.cmd_parms);
		   	}
	       }
	private void put_mlc_line(String text){
		/*
		 * write line to MLC file
		 * 
		 * Notes:
		 *   1.  If first char is " and not data_div
		 *       assume comment-entry and make comment line
		 */
		if (text.length() > 14){
			if (!data_div && text.charAt(9) == '\''){
		       text = "*" + text.substring(1); // make comment-entry comment line
			} else if (text.substring(0,14).equals("         COPY ")){
			   return;  // flush copy
		    }
		}
		if (tz390.opt_traces){
			System.out.println("put mlc " + text);
		}
		try {
			if (text.length() >=72){
				mlc_file_buff.write(text.substring(0,71) + "X\r\n");
				int index = 71;
				while (text.length()-index > 56){
					mlc_file_buff.write("               " + text.substring(index,index+56) + "X\r\n");
					index = index + 56;
				}
				mlc_file_buff.write("               " + text.substring(index) + "\r\n");
			} else {
				mlc_file_buff.write(text + "\r\n");
			}
            tot_mlc++;
		} catch (Exception e){
			System.out.println("zcobol write MLC file error - " + e.toString());
		    System.exit(16);
		}
	}
	private void abort_error(String msg){
		/*
		 * display error and terminate
		 */
		System.out.println(msg);
		tz390.put_systerm(msg);
		put_mlc_line("* zcobol error - " + msg);
		tz390.close_systerm(16);
		System.exit(16);
	}
	private void log_error(String msg){
		/*
		 * display error and terminate
		 */
		System.out.println(msg);
		tz390.put_systerm(msg);
		put_mlc_line("* zcobol error on line " + tot_cbl + " = " + msg);
        tot_err++;
	}
	private void get_zc_token(){
		/*
		 * 1.  Set zc_token to next logical 
		 *     cobol token including continued token
		 *     split across 1 or more lines.
		 *     Set to null at end of file.
		 * 2.  Set zc_token_area to A for token
		 *     starting in col 8-11.
		 * 3.  Set zc_token_area to B for token
		 *     starting in 12-72.
		 * 4.  If token not in quotes, make upper-case.
		 */
		if (zc_next_token == null || zc_flush_cont_token){
			zc_flush_cont_token = false;
			if (!zc_eof){
				set_next_token();
				zc_prev_token = zc_next_token;
				zc_prev_index = zc_next_index;
				zc_prev_area  = zc_next_area;
				zc_prev_first = zc_next_first;
				zc_prev_line_cnt = zc_next_line_cnt;
			}
			if (zc_eof){
				// return null at eof
				zc_token = null;
				return;
			}
		} else {
			zc_prev_token = zc_next_token;
			zc_prev_index = zc_next_index;
			zc_prev_area  = zc_next_area;
			zc_prev_first = zc_next_first;
			zc_prev_line_cnt = zc_next_line_cnt;
		}
		if (zc_prev_token != null
			&& (zc_prev_token.equals("'") 
				|| zc_prev_token.equals("\""))){
			// start split literal found
			zc_split_lit  = true;
			zc_prev_token = "'" + zc_line.substring(zc_prev_index+8);	
			zc_line = null;
			set_next_token();
			while (zc_split_lit
				&& zc_line != null
				&& zc_line.length() > 7
				&& zc_line.charAt(6) == '-'
				&& zc_next_token != null
				&& zc_next_token.charAt(0) == '\''){
				// we have continuation of literal
				// in zc_next_token from next line
				if (zc_next_token.length() == 1){
					// add rest of contined line
					int index = zc_line.lastIndexOf("'");
					if (index == -1){
						index = zc_line.lastIndexOf("\"");
					}
				    zc_prev_token = zc_prev_token + zc_line.substring(index+1); 
					zc_line = null;
					set_next_token();
				} else {
					// add to end of literal
					zc_prev_token = zc_prev_token + zc_next_token.substring(1); 
					if (zc_next_token.charAt(zc_next_token.length()-1) == '\''){
						zc_split_lit  = false;  // end split lit at quote
					
						zc_flush_cont_token = true; // flush continue token appended to prev.				
					} else {
						zc_line = null;
						set_next_token();
					}					
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
				     && (zc_prev_token.equals("PIC")
					     || zc_prev_token.equals("PICTURE"))){
				     pic_mode = true;
				     pic_token_cnt = 0;
				}
			} else if (pic_token_cnt > 1){
				pic_mode = false; // turn off after processing token after pic
			}
			set_next_token();
			while (zc_line != null
					&& zc_line.length() > 7
					&& zc_line.charAt(6) == '-'
					&& zc_next_token != null){
                    // append next token to prev
				    zc_prev_token = zc_prev_token + zc_next_token;
				    set_next_token();
			}
		}
		if (zc_prev_token.charAt(0) != '\'' 
			&& zc_prev_token.charAt(0) != '"'){
			zc_prev_token = zc_prev_token.toUpperCase();
		}
        // we now have a complete prev token
		zc_token      = zc_prev_token;
		zc_token_index = zc_prev_index;
		zc_token_area = zc_prev_area;
		zc_token_first = zc_prev_first;
		zc_token_line_cnt = zc_prev_line_cnt;
		return;
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
					if  (zc_line.charAt(6) != ' ' && zc_line.charAt(6) != '-'){
                         put_mlc_comment();
					} else if ((zc_line.length() >= 14 
							    && zc_line.substring(7).toUpperCase().equals("AUTHOR."))
							    || (zc_line.length() >= 16
								&& zc_line.substring(7).toUpperCase().equals("SECURITY."))
							   || (zc_line.length() >= 20
								&& zc_line.substring(7).toUpperCase().equals("INSTALLATION."))
															   || (zc_line.length() >= 20
								&& zc_line.substring(7).toUpperCase().equals("DATE-WRITTEN."))
							    ){
						put_mlc_comment();
					    zc_pg_comment_mode = true;
					} else if (zc_pg_comment_mode){
						if (zc_line.length() > 12 && zc_line.substring(7,11).equals("    ")){
							put_mlc_comment();
						} else {
							zc_pg_comment_mode = false;
						}
					}
				} 
			}
			if  (zc_line != null){
			    zc_match = zc_pattern
			     .matcher(zc_line.substring(7));        	
		        find_next_token();
			}
		}
		if (zc_eof){
			zc_next_token = null;
		} else {
			if (tz390.opt_traceall){
				put_mlc_line("* trace get next token = " +zc_next_token);
			}
		}
	}
	private void put_mlc_comment(){
		/*
		 * put zc_line as comment on MLC
		 */
		if (zc_comment_pending){
			put_mlc_line(zc_comment_line);
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
			if (zc_comment // request to gen CBL comments
					&& zc_line.charAt(6) != '*') {
				if (zc_comment_pending) {
					put_mlc_line(zc_comment_line);
				}
				zc_comment_pending = true;
				zc_comment_line = "* " + zc_line;
				zc_comment_cnt = tot_cbl;
			}
		} else {
			zc_eof = true;
			return;
		}
	}
	private void get_next_zc_line(){
		/*
		 * get next zc_line from nested copy files
		 */
		if (request_dfheiblk){
			request_dfheiblk = false;
			zc_line = "         COPY DFHEIBLK";
			return;
		} else if (dfheiblk && request_dfhcommarea){
            gen_dfhcommarea();
			return;
		} else if (dfheiblk && dfhcommarea && request_proc){
            gen_proc_using();
			return;
		}
		if (copy_found){
		    copy_found = false;
		    open_copy_file();
		}
		try {
			zc_line = zc_file_buff[cur_zc_file].readLine();
			while (zc_line == null && cur_zc_file > 0){
				zc_file_buff[cur_zc_file].close();
				cur_zc_file--;
				if (dfheiblk_loading){
					dfheiblk_loading = false;
					dfheiblk = true;
				}
			    if (dfheiblk && request_dfhcommarea){
                    gen_dfhcommarea();
			    	return;
			    }
			    if (dfheiblk && dfhcommarea && request_proc){
                    gen_proc_using();
		    	    return;
		    	}
				zc_line = zc_file_buff[cur_zc_file].readLine();
			}
			if (zc_line != null && zc_line.length() > 72){
				zc_line = zc_line.substring(0,72);
			}
		} catch (Exception e){
			abort_error("zcobol read error on CBL/CPY file - " + e.toString());
		}
	}
	private void gen_dfhcommarea(){
		/*
		 * gen 01 DFHCOMMAREA
		 */
		request_dfhcommarea = false;
		dfhcommarea = true;
    	zc_line = "         01  DFHCOMMAREA.";
	}
	private void gen_proc_using(){
		/*
		 * gen PROCEDURE DIVISION USING
		 * with DFHEIBLK, DFHCOMMAREA
		 * and any user parms.
		 */
    	request_proc = false;
		zc_line = "         PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA" + proc_using_parms;
	}
	private void flush_comment_line(){
		/*
		 * write pending CBL line comment
		 * if pending
		 */
		if (zc_comment_pending
			&& zc_comment_cnt <= zc_token_line_cnt){
			put_mlc_line(zc_comment_line);
			zc_comment_pending = false;
		}
	}
	private void find_next_token(){
		/*
		 * find next token in zc_line
		 * else set zc_line = null
		 */
		if (!zc_match.find()){
			zc_line = null;
		} else {
			zc_token_count++;
			if (zc_token_count == 1){
				zc_next_first = true;
			} else {
			    zc_next_first = false;
			}
			pic_token_cnt++;
			zc_next_token = zc_match.group();
			zc_next_index = zc_match.start();
			zc_next_line_cnt = tot_cbl;
			if (zc_match.start() < 4 && zc_next_first){
				zc_next_area = 'A';
			} else {
				zc_next_area = 'B';
			}
			if (zc_next_token.length() > 2 
				&& zc_next_token.charAt(0) == '"'){
				zc_next_token = "'" + zc_next_token.substring(1,zc_next_token.length()-1)+"'"; // cvt ".." to '..'
			} else if (zc_next_token.equals("\"")){
				zc_next_token = "'";
			}
			if (pic_mode){
				int index2 = zc_next_index + zc_match.group().length()+7;
				while (zc_line.length() > index2
					   && zc_line.charAt(index2) > ' '
				       && (zc_line.charAt(index2) != '.' 
				    	   || (zc_line.length() > index2+1 
			    			   && zc_line.charAt(index2+1) > ' '
				    	      )
				    	   )    
				    	&& zc_match.find()){
					  zc_next_token = zc_next_token + zc_match.group();
				      zc_next_index = zc_match.start();
				      index2 = zc_next_index + zc_match.group().length()+7;
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
		 */
		if (zc_token.length() > 1){
			if (zc_token.charAt(0) != '\''
				&& zc_token.charAt(0) != '"'){
				if ( // ALLOW . IN OPEN PARMS zc_token.indexOf('.') >= 0 
					zc_token.indexOf(',') >= 0){
					// wrap strings with ., in quotes
					zc_token = "'" + zc_token + "'";				
				} else if (!pic_mode && zc_token.indexOf('-') >= 0){
			    	// replace token "-" with "_"
			    	// for z390 meta macro assembler compatiblity
		    		zc_token = zc_token.replaceAll("-","_");
			    } else if (zc_token.equals("COPY")){ 
					copy_found = true;
					zc_copy_file_name = zc_next_token + cpz_type;
					if (mlc_parms > 1){ // don't gen 01 without name 
						flush_last_mlc_line();
					} else {
						mlc_parms = 0;
						mlc_line = null;
					}
				}			    
			}
		} else if (zc_token.length() == 1){
			// single char token . or ,
			if (zc_token.charAt(0) == '.'){
				if (mlc_line != null
					&& mlc_parms >= 1){
					if (exec_mode){
						log_error("EXEC statement missing END-EXEC");
					    exec_mode = false;
					    exec_parm_index = 0;
					}
					// flush line at period after parms
					// and generate PERIOD in proc div
					if (zc_proc_div && !skip_period){
						new_mlc_line("             PERIOD");
					}
					flush_last_mlc_line();
					zc_level = 0;
				}
				skip_period = false;
				// ignore period
				return;
			} else if (zc_token.charAt(0) == ','){
				// ignore commas
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
				&& (zc_next_token.equals(".")            // RPI 1012
					|| zc_next_token.equals("SECTION"))){ // RPI 1012				
				// gen procedure div label
				new_mlc_line("         LABEL " + zc_token);
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
							put_mlc_line("         DATA DIVISION");
						}
						if (zc_cics){
							if (!linkage_sect){
	                            zc_comment_pending = false;
								linkage_sect = true;
								put_mlc_line("         LINKAGE SECTION");
							}
							if (!dfhcommarea){
								request_dfhcommarea = true;
							}
							if (!dfheiblk){
								request_dfheiblk = true;
							}
							if (request_dfhcommarea
								|| request_dfheiblk){
								set_proc_using_parms();
								zc_comment_pending = false;
								request_proc = true;
								return;
							}
						} 
						put_mlc_line("         DATA END");						
						zc_proc_div = true;
						skip_period = true;
					} else if (zc_token.equals("END") && zc_next_token.equals("DECLARATIVES")){
						zc_token = "END_DECLARATIVES";   // RPI 1012
						zc_next_token = " "; 
						skip_period = true;
					}
					new_mlc_line("         " + zc_token);
				}
			}
		} else if (find_verb()){
			if (zc_token_first){
				flush_last_mlc_line();
				flush_comment_line();
			}
			new_mlc_line("             " + zc_token);	
			if (zc_next_token.equals(".")){
				new_mlc_line("             PERIOD");	
			}		
		} else {
			// assume parm for prior verb
			if (mlc_line == null){
				// assume unknown verb starting new line
				flush_comment_line();
				if (!zc_proc_div 
					&& zc_token.charAt(0) >= '0' 
					&& zc_token.charAt(0) <= '9'){
					new_ws_line();
				} else {
					new_mlc_line("         " + zc_token);
				}
			} else {
				add_mlc_parm(zc_token);
			}
		}
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
		new_mlc_line("      WS" + ws_indent[ws_lvl_index] + zc_token);
		mlc_parms = 1;
		if (zc_token.equals("01") && zc_next_token.equals("DFHCOMMAREA")){
			dfhcommarea = true;
		}
	}
    private void new_mlc_line(String line){
    	/*
    	 * write pending mlc line
    	 * and start new line
    	 * and reset parm count
    	 */
        flush_last_mlc_line();
		mlc_line = line;
		mlc_parms = 0;
    }
    private void flush_last_mlc_line(){
    	/*
    	 * write pending mlc_line if any
    	 */
    	if (mlc_line != null){ 
    		put_mlc_line(mlc_line);
    	}
    	mlc_line = null;
    }
    private void add_mlc_parm(String token){
    	/*
    	 * add token parm to mlc_line
    	 * Notes:
    	 *   1.  If exec_mode and parm,
    	 */
    	if (mlc_parms == 0) {
			mlc_line = mlc_line + " " + zc_token;
		} else if (!exec_mode){
			mlc_line = mlc_line + "," + zc_token;
		} else {
			if (zc_token.compareTo("END_EXEC") == 0){
				gen_exec_stmt();
				zc_token_first = true;
			} else {
		        exec_parm[exec_parm_index] = zc_token;
				exec_parm_index++;						
			}
		}
    	mlc_parms++;
    }
    private void gen_exec_stmt(){
    	/*
    	 * generate EXEC call with parm(value)
    	 * paramters combined
    	 */
    	int index = 0;
    	while (index < exec_parm_index){
			if (exec_parm[index].equals("'('")){
				mlc_line = mlc_line + "(" + exec_parm[index+1];
				index = index + 2;
				while (index < exec_parm_index 
					&& exec_parm[index] != "')'"){
					mlc_line = mlc_line + "," + exec_parm[index];
					index++;
				}
			    mlc_line = mlc_line + ")";
			    index = index + 1;
			} else {
				mlc_line = mlc_line + "," + exec_parm[index];
                index++;
			}
    	}    	
    	put_mlc_line(mlc_line);
    	if (zc_next_token.equals(".")){
    		put_mlc_line("             PERIOD");
    	}
		if (!zc_cics 
			&& mlc_line.length() > 22
			&& mlc_line.substring(13,22).equals("EXEC CICS")){
			abort_error("EXEC CICS statements require CICS option");
		}
    	mlc_line = null;
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
			        ){
					return true;
				}		
				return false;	
		case 'D': 
			if (key.equals("DELETE")
					|| key.equals("DISABLE")
			        || key.equals("DISPLAY")
			        || key.equals("DIVIDE")
			        ){
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
			        ){
					return true;
				}		
				return false;
		case 'O': 
			if (key.equals("OPEN")
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
			        ){
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
				return true;
			}		
			return false;	
		}
		return false;  // UNDEFINED WORD
	}
	private void open_copy_file(){
		/*
		 * open copy file 
		 */
		String new_zc_file_name = tz390.find_file_name(tz390.dir_cpy,zc_copy_file_name,tz390.cpy_type,tz390.dir_cur);
		if (new_zc_file_name == null){ // RPI 970 add key if found
			log_error("COPY file not found - " + zc_copy_file_name);
		    return;
		} else if (zc_cics){ 
			if (zc_copy_file_name.equals("DFHEIBLK.CPZ")){
				if (!dfheiblk){
					dfheiblk_loading = true;
				} else {
					put_mlc_line("* ZC390I DUPLICATE COPY DFHEIBLK IGNORED");
				    return;
				}
			}
		}
		cur_zc_file++;
		if (cur_zc_file >= tz390.opt_maxfile){
			cur_zc_file--;
			log_error("maximum nested copy files exceeded");
			return;
		}
		try {
			zc_file[cur_zc_file] = new File(new_zc_file_name);
			zc_file_buff[cur_zc_file] = new BufferedReader(new FileReader(zc_file[cur_zc_file]));
		} catch (Exception e){
			cur_zc_file--;
			log_error("I/O error opening copy file - " + e.toString());
		}
	}
}

