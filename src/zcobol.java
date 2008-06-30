import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.swing.JTextArea;


public class zcobol{
	/*****************************************************
	
    zcobol COBOL translator to z390 macro assembler
    meta language which can then be expanded to
    z390 mainframe assembler, i586 Intel assembler,
    MS VC++, or J2SE Java using macro libraries.
	
    Copyright 2008 Automated Software Tools Corporation
	All rights reserved. 
    
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
    ****************************************************
    *                                         last RPI *
	****************************************************
	*/
	/*
	 * Global variables
	 */    
	String zcobol_ver = "v1.0.00";
	int    tot_zcob = 0; // total CBL lines read
	int    tot_mlc  = 0; // total MLC lines written
	int    tot_err  = 0; // total errors
	/*
	 * COBOL CBL input file variables
	 */
	String zcob_line = null;      // logical line with continuations added
	String zcob_file_name = null;
	File   zcob_file = null;
	BufferedReader zcob_buff = null;
	boolean zcob_eof = false;
	boolean zcob_put_pending = false;
	boolean exec_mode = false;
	boolean data_div  = false;
	boolean allow_verb = false;
	String  zcob_line_pending;
	/*
	 * MLC meta macro assembler output file variables
	 */
	String mlc_line = null;
	String mlc_file_name = null;
	File   mlc_file = null;
	BufferedWriter mlc_buff = null;
    /*
     * zcob token variables
     */
	int     zcob_token_count = 0;
	String  zcob_prev_token = null;
	String  zcob_next_token = null;
	char    zcob_prev_area  = 'A';
	char    zcob_next_area  = 'A';
	boolean zcob_prev_first = true;
	boolean zcob_next_first;
	boolean zcob_token_first = true;
	String  zcob_token   = null;   // next token or null at eof
	Pattern zcob_pattern = null;   // parsing regular expression pattern
	Matcher zcob_match   = null;   // token pattern matching class
	char    zcob_token_area = 'A'; // token area A or B
	boolean zcob_split_lit = false; // literal split across lines
	boolean zcob_proc_div = false; // PRODECUDE DIV started
	int mlc_parms = 0; // mlc parm count 
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
		  zcobol pgm = new zcobol();
	      pgm.process_zcob(argv,null);
	}
	private void process_zcob(String[] args,JTextArea log_text){
	    /*
		 * translate cobol to z390 macro assembler
		 */
		init_zcob(args);        
		get_zcob_token();
		while (zcob_token != null){
			process_zcob_token();
			get_zcob_token();
		}
		if (mlc_line != null){
			put_mlc_line(mlc_line);
		}
		term_zcob();
	}
	private void init_zcob(String[] args){
		/*
		 * 1.  Display zcobol version
		 * 2.  Compile regular expression pattern
		 * 3.  Open CBL and MLC files
		 */	
        System.out.println("zcobol version = " + zcobol_ver);
		try {
			zcob_pattern = Pattern.compile(
					 // parm in single or double quotes
				   	"([']([^']|(['][']))+['])"  // parm in quotes
				  +	"|([\"]([^\"]|([\"][\"]))+[\"])"  // parm in quotes
					 // any parm such as PIC may have embedded ., but not '"()
				  +	"|([^\\s\\.\\,\\'\"()]+(([^\\s\\.\\,\\'\"()]+)|([\\.\\,][^\\s\\.\\,\\'\"()]+))*)"	 
				     // .,'"() single special char requiring processing
				  + "|([\\.\\,\\'\"()])"            
			);
		} catch (Exception e){
			abort_error("zcobol cbl pattern errror - " + e.toString());
		}
	    zcob_file_name = args[0] + ".CBL"; 
        zcob_file      = new File(zcob_file_name);
	    mlc_file_name = args[0] + ".MLC"; 
        mlc_file      = new File(mlc_file_name);
       	try {
			zcob_buff = new BufferedReader(new FileReader(zcob_file_name));
			mlc_buff  = new BufferedWriter(new FileWriter(mlc_file_name));
       	} catch (Exception e){
   			abort_error("zcobol file I/O error - " + e.toString());
       	}
	}
	private void term_zcob(){
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
			zcob_buff.close();
			mlc_buff.close();
    	} catch (Exception e){
    		abort_error("zcobol file close error " + zcob_file_name);
    	}
		System.out.println("zcobol total CBL lines in  =" + tot_zcob);
		System.out.println("zcobol total MLC lines out =" + tot_mlc);
		System.out.println("zcobol total errors        =" + tot_err);
        if (tot_err == 0){
        	System.exit(0);
        } else {
        	System.exit(8);
        }
	}	
	private void put_mlc_line(String text){
		/*
		 * write line to MLC file
		 */
		try {
			if (text.length() >=72){
				mlc_buff.write(text.substring(0,71) + "X\r\n");
				int index = 71;
				while (text.length()-index > 56){
					mlc_buff.write("               " + text.substring(index,index+56) + "X\r\n");
					index = index + 56;
				}
				mlc_buff.write("               " + text.substring(index) + "\r\n");
			} else {
				mlc_buff.write(text + "\r\n");
			}
            tot_mlc++;
		} catch (Exception e){
			abort_error("zcobol write MLC file error - " + e.toString());
		}
	}
	private void abort_error(String msg){
		/*
		 * display error and terminate
		 */
		System.out.println(msg);
		System.exit(16);
	}
	private void log_error(String msg){
		/*
		 * display error and terminate
		 */
		System.out.println(msg);
		put_mlc_line("* zcobol error - " + msg);
        tot_err++;
	}
	private void get_zcob_token(){
		/*
		 * 1.  Set zcob_token to next logical 
		 *     cobol token including continued token
		 *     split across 1 or more lines.
		 *     Set to null at end of file.
		 * 2.  Set zcob_token_area to A for token
		 *     starting in col 8-11.
		 * 3.  Set zcob_token_area to B for token
		 *     starting in 12-72.
		 */
		if (zcob_next_token == null){
			if (!zcob_eof){
				set_next_token();
				zcob_prev_token = zcob_next_token;
				zcob_prev_area  = zcob_next_area;
				zcob_prev_first = zcob_next_first;
			}
			if (zcob_eof){
				// return null at eof
				zcob_token = null;
				return;
			}
		} else {
			zcob_prev_token = zcob_next_token;
			zcob_prev_area  = zcob_next_area;
			zcob_prev_first = zcob_next_first;
		}
		if (zcob_prev_token.equals("'")){
			// start split literal found
			zcob_split_lit  = true;
			zcob_prev_token = zcob_line.substring(zcob_match.start());	
			zcob_line = null;
			set_next_token();
			while (zcob_split_lit
				&& zcob_line != null
				&& zcob_line.length() > 7
				&& zcob_line.charAt(6) == '-'
				&& zcob_next_token != null
				&& zcob_next_token.charAt(0) == '\''){
				// we have continuation of literal
				// in zcob_next_token from next line
				if (zcob_next_token.length() == 1){
					// add rest of contined line
					zcob_prev_token = zcob_prev_token + zcob_line.substring(zcob_match.start()+1);
					zcob_line = null;
					set_next_token();
				} else {
					// add end of literal
					zcob_split_lit  = false;
					zcob_prev_token = zcob_prev_token + zcob_next_token.substring(1);
				}
			}
	        if (zcob_split_lit){
	        	log_error("zcob split literal format error");
	        	zcob_split_lit = false;
	        }
	        zcob_prev_area = 'B';
			// we have complete literal in zcob_prev_token
		} else {
			// prev token not split literal
			set_next_token();
			while (zcob_line != null
					&& zcob_line.length() > 7
					&& zcob_line.charAt(6) == '-'
					&& zcob_next_token != null){
                    // append next token to prev
				    zcob_prev_token = zcob_prev_token + zcob_next_token;
				    set_next_token();
			}
		}
        // we now have a complete prev token
		zcob_token      = zcob_prev_token;
		zcob_token_area = zcob_prev_area;
		zcob_token_first = zcob_prev_first;
		return;
	}
	private void set_next_token(){
		/*
		 * get next token from CBL file
		 * and set pending CBL comment line
		 */
		if (zcob_line != null){
			if (zcob_next_token != null){
				find_next_token();
			}
		}
		while (!zcob_eof 
				&& zcob_line == null){
			get_zcob_line();
			if (!zcob_eof){
				if (zcob_line.length() < 8){
					// ignore short lines
					zcob_line = null;
				} else if (zcob_line.length() > 72){
					zcob_line = zcob_line.substring(0,72);
				}
				if  (zcob_line.charAt(6) != ' '){
					put_mlc_line("*" + zcob_line);
					zcob_line = null;
				}
			}
			if  (zcob_line != null){
			    zcob_match = zcob_pattern
			     .matcher(zcob_line.substring(7));        	
		        find_next_token();
			}
		}
		if (zcob_eof){
			zcob_next_token = null;
		}	
	}
	private void get_zcob_line(){
			/*
			 * 1.  read next CBL file line
			 * 2.  set cbl_eof if end of file
			 * 3.  write each line as comment on MLC
			 * 4.  ignore lines < 8 or blank
			 * 5.  ignore comment lines with non-space in 7
			 */
			try {
                flush_pend_mlc_line();
				zcob_line = zcob_buff.readLine();
				boolean check_zcob = true;
				while (zcob_line != null
						&& check_zcob){ 
					if (zcob_line.length() >= 7){
						if (zcob_line.length() > 72){
							zcob_line = zcob_line.substring(0,72);
						}
						if (zcob_line.charAt(6) == '*'
							|| zcob_line.charAt(6) == '/'){
							tot_zcob++;
							if (zcob_line.length() > 70){
								put_mlc_line("*" + zcob_line.substring(0,70));
							} else {
								put_mlc_line("*" + zcob_line);
							}
							zcob_line = zcob_buff.readLine();
						} else {
							check_zcob = false;
						}
					} else {
						// ignore blank lines
						zcob_line = zcob_buff.readLine();
					}
				}
				if (zcob_line != null){
					tot_zcob++;
					zcob_token_count = 0;
					zcob_put_pending = true;
					zcob_line_pending = "* " + zcob_line;
				} else {
					zcob_eof = true;
					return;
				}		        
			} catch (Exception e){
				abort_error("zcobol read error on MLC file - " + e.toString());
			}
		}
	private void flush_pend_mlc_line(){
		/*
		 * write pending CBL line comment
		 * if pending
		 */
		if (zcob_put_pending){
			put_mlc_line(zcob_line_pending);
			zcob_put_pending = false;
		}
	}
	private void find_next_token(){
		/*
		 * find next token in zcob_line
		 * else set zcob_line = null
		 */
		if (!zcob_match.find()){
			zcob_line = null;
		} else {
			zcob_token_count++;
			if (zcob_token_count == 1){
				zcob_next_first = true;
			} else {
			    zcob_next_first = false;
			}
			zcob_next_token = zcob_match.group();
			if (zcob_match.start() < 3){
				zcob_next_area = 'A';
			} else {
				zcob_next_area = 'B';
			}
		}
	}
	private void process_zcob_token(){
		/*
		 * process zcob_token
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
		if (zcob_token.length() > 1){
			if (zcob_token.charAt(0) != '\''
				&& zcob_token.charAt(0) != '"'){
				if (zcob_token.indexOf('.') >= 0 
					|| zcob_token.indexOf(',') >= 0){
					// wrap strings with ., in quotes
					zcob_token = "'" + zcob_token + "'";				
				} else if (zcob_token.indexOf('-') >= 0){
			    	// replace token "-" with "_"
			    	// for z390 meta macro assembler compatiblity
			    	zcob_token = zcob_token.replaceAll("-","_");
			    }
			    
			}
		} else if (zcob_token.length() == 1){
			// single char token . or ,
			if (zcob_token.charAt(0) == '.'){
				if (mlc_line != null 
					&& mlc_parms > 0){
					// flush line at period after parms
					// and generate PERIOD in proc div
					if (zcob_proc_div){
						new_mlc_line("             PERIOD");
					}
					flush_last_mlc_line();
					exec_mode = false;
				}
				// ignore period
				return;
			} else if (zcob_token.charAt(0) == ','){
				// ignore commas
				return;
			} else if (zcob_token.charAt(0) == '\''){
				zcob_token = "'" + zcob_token + "'";
			} else if (zcob_token.charAt(0) == '"'){
				zcob_token = "\"" + zcob_token + "\"";
 			} else if (zcob_token.charAt(0) == '(' || zcob_token.charAt(0) == ')'){
 				zcob_token = "'" + zcob_token + "'";
 			}			
			// may be single char label 
		} else {
			abort_error("zero length token parsing error");
		}
		if (zcob_token_area == 'A'){
			// non procedure division section operation
			// or procedure division label
			if (zcob_proc_div){				
				// gen procedure div label
				new_mlc_line("         LABEL " + zcob_token);
			} else {
				if  (zcob_token.charAt(0) >= '0' 
					 && zcob_token.charAt(0) <= '9'){
					new_ws_line();
				} else {
					if (zcob_token.equals("DATA")){
						data_div = true;
					} else if (zcob_token.equals("PROCEDURE")){
						if (!data_div){
							data_div = true;
							put_mlc_line("         DATA DIVISION");
						}
						put_mlc_line("         DATA END");						
						zcob_proc_div = true;
					}
					new_mlc_line("         " + zcob_token);
				}
			}
		} else if (find_verb()){
			if (zcob_token_first){
				flush_last_mlc_line();
				flush_pend_mlc_line();
			}
            new_mlc_line("             " + zcob_token);		
		} else {
			// unknown verb or parm
			if (mlc_line == null){
				flush_pend_mlc_line();
				if (!zcob_proc_div 
					&& zcob_token.charAt(0) >= '0' 
					&& zcob_token.charAt(0) <= '9'){
					new_ws_line();
				} else {
					new_mlc_line("         " + zcob_token);
				}
			} else {
				add_mlc_parm(zcob_token);
			}
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
		int ws_item_lvl = Integer.valueOf(zcob_token);
		switch (ws_item_lvl){
		case 1:
			if (zcob_token_area != 'A'){
				log_error("zcobol level 01 must be in A field");
			}
			ws_lvl_index = 0;
			ws_lvl[0] = ws_item_lvl;
			ws_indent[0] = " ";
			break;
		case 66:  // redefine
			ws_lvl_index = 0;
			ws_lvl[0] = ws_item_lvl;
			ws_indent[0] = " ";
			break;
		case 77:  // item
			ws_lvl_index = 0;	
			ws_lvl[0] = ws_item_lvl;
			ws_indent[0] = " ";
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
				ws_indent[ws_lvl_index] = ws_indent[ws_lvl_index-1] + "   ";
			} else {
				while (ws_item_lvl > 0
						&& ws_item_lvl < ws_lvl[ws_lvl_index]){
					ws_lvl_index--;
				}
			}
			ws_lvl[ws_lvl_index] = ws_item_lvl;
		}
		new_mlc_line("      WS" + ws_indent[ws_lvl_index] + zcob_token);
		mlc_parms = 1;
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
    	 */
    	if (mlc_parms == 0) {
			mlc_line = mlc_line + " " + zcob_token;
		} else if (zcob_token.compareTo("END_EXEC") != 0){
			mlc_line = mlc_line + "," + zcob_token;
		}
    	mlc_parms++;
    }
	private boolean find_verb(){
		/*
		 * return true if zcob_token
		 * is a known COBOL verb
		 */
		if (allow_verb){
			allow_verb = false;
			return false;
		}
		if (exec_mode){
			// ignore verbs within exec statement
			return false;
		}
		String key = zcob_token.toUpperCase();
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
			        || key.equals("DISPLAY")
			        || key.equals("DIVIDE")
			        ){
					return true;
				}		
				return false;
		case 'E':
			if (key.equals("EXEC")){
				exec_mode = true;
				return true;
			}
			if (key.equals("END_EXEC")){
				exec_mode = false;
				return false;
			}
			if (key.equals("EJECT")
				|| key.equals("END_IF")
				|| key.equals("END_EVALUATE")
				|| key.equals("EVALUATE")
		        || key.equals("EXIT")
			    || key.equals("EXIT_PROGRAM")
			    ){
				return true;
			}
			return false;
		case 'G': 
			if (key.equals("GO")
					|| key.equals("GOBACK")
			        ){
					return true;
				}		
				return false;
		case 'I': 
			if (key.equals("IF")
			        || key.equals("INITIALIZE")
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
		case 'O': 
			if (key.equals("OPEN")
			    ){
					return true;
				}		
				return false;
		case 'P': 
			if (key.equals("PERFORM")){
				allow_verb = true;
				return true;
			}		
			return false;
		case 'R': 
			if (key.equals("READ")
			        || key.equals("RELEASE")
			        || key.equals("RETURN")
			        || key.equals("REWRITE")
			        ){
					return true;
				}		
				return false;	
		case 'S': 
			if (key.equals("SUBTRACT")
					|| key.equals("SEARCH")
					|| key.equals("SELECT")
			        || key.equals("SET")
			        || key.equals("SORT")
			        || key.equals("START")
			        || key.equals("STOP")
			        || key.equals("STRING")
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
		return false;
	}
}

