import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.lang.reflect.Array;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Random;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.swing.JTextArea;

public  class  mz390 {
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

    mz390 is the macro processor component of z390 which can be called from
    z390 gui interface or from command line to read mlc macro source and
    any referenced macros and copybooks to generate expanded BAL source file.

    ****************************************************
    * Maintenance
    * ***************************************************
    * 03/09/05 copied from z390.java and modified
    * 03/25/05 debug seta support, add error 35
    * 05/09/05 completed expression substring support
    * 05/22/05 completed SYSLIST support
    * 06/11/05 allow commas in quoted parm strings
    * 06/20/05 add find_lcl_key_index support
    * 07/06/05 add MNOTE error level support
    * 07/25/06 fix sublist reference by pos parm name
    * 07/26/05 add string operations DOUBLE, FIND, INDEX
    * 07/26/05 report error when set tries to change parm
    * 08/08/05 add nested copy support
    * 08/10/05 add error reporting by file & line #
    * 08/15/05 change max time limit to 10 seconds 
    * 08/22/05 add SYS - BAL, CPY, DAT, MAC, MLC, PCH
    * 08/23/05 add T' and L' operator support for ordinary symbols
    * 08/27/05 fix lower case support for var & ops
    * 08/28/05 fix reset of cur_mac_line_num for each load
    * 09/03/05 add comma delimited continue
    * 09/10/05 add created set symbols
    * 09/14/05 change AREAD and PUNCH to use DDNAME vs file
    * 09/15/05 strip macro labels off bal output
    ********************************************************
    * Global variables
    *****************************************************/
    String version = null;
    int mz390_rc = 0;
    int mz390_errors = 0;
    boolean mac_abort = false;
    Date cur_date = new Date();
    long tod_start = cur_date.getTime();
    long tod_end   = 0;
    long tot_msec = 0;
    long ins_rate    = 0;
    boolean log_to_bal = false;
    int tot_bal_line = 0;
	String bal_text = null;     // curr bal_line text
    int    bal_text_index0 = 0; // end of prev. parm
    int    bal_text_index1 = 0; // start of cur parm
    int    bal_text_index2 = 0; // end   of cur parm
    int tot_mac_ins  = 0;
    int tot_mac_load = 0;
    int tot_mac_call = 0;
    int mlc_line_end = 0;
    boolean loading_mlc = true;
    File bal_file = null;
    BufferedWriter bal_file_buff = null;
    String dat_file_name = null;
    File   dat_file = null;
    BufferedReader dat_file_buff = null;
    String pch_file_name = null;
    File   pch_file = null;
    BufferedWriter pch_file_buff = null;
    String bal_line = null;
    String bal_label = null;
    String bal_op = null;
    boolean bal_op_ok = false;
    String bal_parms = null;
    boolean mlc_eof = false;
    static int actr_limit = 4096;
    int     actr_count = actr_limit;
    String  opt_parms = " ";
    boolean opt_ok = false;
    boolean opt_con      = true;
    boolean opt_listfile = true;
    boolean opt_listcall = true;
    boolean opt_stats    = true;
    boolean opt_sym      = true;
    boolean opt_time     = true;
    boolean opt_trace    = false;
    boolean opt_trap     = true;
    boolean opt_traceall = false;
    boolean opt_timing   = true;
	SimpleDateFormat mmddyy = new SimpleDateFormat("MM/dd/yy");
	SimpleDateFormat hhmmss = new SimpleDateFormat("HH:mm:ss");
    boolean log_tod = true; 
    JTextArea z390_log_text = null;
    boolean mz390_aborted = false;
    /*
     * static limits
     */
    static int max_errors = 100;
    static int max_mac_name = 5000;  // inline and external macros defined  
    static int max_mac_file_name = 10000; // MLC, MAC, CPY's
    static int max_mac_line = 200000;
    static int max_mac_lab = 10000;
    static int max_mac_call_level = 50; 
    static int max_pos_parm = 10000;
    static int max_kwd_parm = 10000;
    static int max_lcl_name = 10000;
    static int max_lcl_set  = 10000;
    static int max_sym      = 10000;
    static int max_gbl_name = 100000;
    static int max_gbl_set  = 100000;
    static int max_exp_stk = 500;
    long max_time_seconds = 15;     // max elapsed time
    /*
     * macro execution global data
     */
    long    tod_time_limit = 0;
    int     next_time_ins   = 0x1000;
    int     next_time_check = next_time_ins;
    String cur_mac_file_name = null;
    int max_mac_file = 20;
    int cur_mac_file = 0;
    File[] mac_file                = (File[])Array.newInstance(File.class,max_mac_file);
    BufferedReader[] mac_file_buff = (BufferedReader[])Array.newInstance(BufferedReader.class, max_mac_file);
    int[]        mac_file_cur_file_num = (int[])Array.newInstance(int.class, max_mac_file);
    int[]        mac_file_cur_line_num = (int[])Array.newInstance(int.class, max_mac_file);
    int cur_mac_line_num = 0;
    int cur_mac_file_num = 0;
    String mac_line = null;
    String mac_label = null;
    String mac_op = null;
    String mac_parms = null;
    String proto_label = null;
    String proto_op = null;
    String proto_parms = null;
    String parm_name = null;
    String parm_value = null;
    /*
     * macro name table
     */
    int mac_name_index = 0; //current mlc or macro
    int find_mac_name_index = 0;  //call macro or -1
    int tot_mac_name = 0;      // next avail name
    String[] mac_name = new String[max_mac_name];
   	int[]    mac_name_line_start =(int[])Array.newInstance(int.class, max_mac_name);
   	int[]    mac_name_line_end   =(int[])Array.newInstance(int.class, max_mac_name);
    int[]    mac_name_lab_start = (int[])Array.newInstance(int.class,max_mac_name);
    int[]    mac_name_lab_end   = (int[])Array.newInstance(int.class,max_mac_name);
    /*
     * macro bal line tables
     */
    int mac_line_index = 0; //current mac line index
    int tot_mac_line = 0;      // next avail line
    String[] mac_file_line   = new String[max_mac_line];
    int[]    mac_file_line_num = (int[])Array.newInstance(int.class,max_mac_line);
    int[]    mac_file_name_num = (int[])Array.newInstance(int.class,max_mac_line);
    int tot_mac_file_name = 0;
    String[] mac_file_name = new String[max_mac_file_name];
    /*
     * macro labels for loaded mlc and macros
     */
    int tot_mac_lab = 0;
    String[] mac_lab_name  = new String[max_mac_lab]; 
    int[]    mac_lab_index = (int[])Array.newInstance(int.class,max_mac_lab);
    /*
     * macro call stack variables
     */
    int mac_mend_level = 0;      //macro/mend level
    int mac_call_level = 0; //level of macro nesting
    int[]    mac_call_name_index = (int[])Array.newInstance(int.class,max_mac_call_level);
    int[]    mac_call_return = (int[])Array.newInstance(int.class,max_mac_call_level);
    int[]    mac_call_actr   = (int[])Array.newInstance(int.class,max_mac_call_level);
    int[]    mac_call_pos_start = (int[])Array.newInstance(int.class,max_mac_call_level);
    int[]    mac_call_kwd_start = (int[])Array.newInstance(int.class,max_mac_call_level);
    int[]    mac_call_lcl_name_start = (int[])Array.newInstance(int.class,max_mac_call_level);
    int[]    mac_call_lcl_seta_start = (int[])Array.newInstance(int.class,max_mac_call_level);
    int[]    mac_call_lcl_setb_start = (int[])Array.newInstance(int.class,max_mac_call_level);
    int[]    mac_call_lcl_setc_start = (int[])Array.newInstance(int.class,max_mac_call_level);
    int[]    mac_call_lcl_key_start  = (int[])Array.newInstance(int.class,max_mac_call_level);
    int[]    mac_call_lcl_key_root   = (int[])Array.newInstance(int.class,max_mac_call_level);
    /*
      * macro positional and key word global variables
      */
    Pattern var_pattern = null;
    Matcher var_match   = null;
    Pattern parm_pattern = null;
    Matcher parm_match   = null;
    Pattern proto_pattern = null;
    Matcher proto_match   = null;
    Pattern exp_pattern = null;
    Matcher exp_match   = null;
    Pattern pch_pattern = null;
    Matcher pch_match   = null;
    Pattern label_pattern = null;
    Matcher label_match   = null;
    int sublist_index = 0;
    int sublist_count = 0;
    int tot_pos_parm = 0; // cur pos parms on stack
    int cur_pos_parm = 0; // cur pos parm during init (may exceed prototype pos)
    int tot_kwd_parm = 0; // cur kwd parms on stack
    int tot_pos = 0;      // tot pos parms defined
    int tot_kwd = 0;      // tot kwd parms defined
    String[] mac_call_pos_name = new String[max_pos_parm]; 
    String[] mac_call_pos_parm = new String[max_pos_parm]; 
    String[] mac_call_kwd_name = new String[max_kwd_parm]; 
    String[] mac_call_kwd_parm = new String[max_kwd_parm]; 
    /*
     * global and local macro variables
     */
    static byte var_seta_type = 1;      // loc= lcl or gbl
    static byte var_setb_type = 2;      // loc= lcl or gbl
    static byte var_setc_type = 3;      // loc= lcl or gbl
    static byte var_parm_type = 4;      // loc= pos, kw, sys 
    static byte var_subscript_type = 5; // stk_setb=loc, stk_seta=name_index
    static byte var_sublist_type = 6;   // stk_setb=loc, stk_seta=sublist_index, stk_setc=cur sublist
    static byte lcl_loc = 11;     // lcl_seta, lcl_setb, lcl_setc
    static byte gbl_loc = 12;     // gbl_seta, gbl_setb, gbl_setc
    static byte pos_loc = 13;     // named positional parm
    static byte kw_loc  = 14;     // named keyword parm
    static byte syslist_loc = 15; // syslist pos parm ref
    int tot_lcl_name = 0; // cur lcl sets on stack
    int tot_lcl_set  = 0; // tot lcl sets defined
    int tot_lcl_seta = 0;
    int tot_lcl_setb = 0;
    int tot_lcl_setc = 0;
    String[] lcl_set_name  = new String[max_lcl_name]; 
    byte[]   lcl_set_type  = (byte[])Array.newInstance(byte.class,max_lcl_name);
    int[]    lcl_set_start = (int[])Array.newInstance(int.class,max_lcl_name);
    int[]    lcl_set_end   = (int[])Array.newInstance(int.class,max_lcl_name);
    int[]    lcl_seta      = (int[])Array.newInstance(int.class,max_lcl_set);
    byte[]   lcl_setb      = (byte[])Array.newInstance(byte.class,max_lcl_set);
    String[] lcl_setc      = new String[max_lcl_set]; 
    /*
     * global set variables set by find_set
     * and add_lcl_set, add_gbl_set
     */
    int    var_name_index = 0;
    byte   var_loc   = lcl_loc;
    byte   var_type = var_seta_type;
    int    set_size = 0;
    int    seta_index = 0;
    int    setb_index = 0;
    int    setc_index = 0;
    int    store_name_index = 0;
    byte   store_loc   = lcl_loc;
    byte   store_type = var_seta_type;
    int    store_seta_index = 0;
    int    store_setb_index = 0;
    int    store_setc_index = 0;
    String set_name = "";
    int    set_sub  = 0;
    int    seta_value = 0;
    byte   setb_value = 0;
    String setc_value = "";
    int    setc_len   = 0;
    /*
     * define global gbla, gblb, gblc, and system
     * predefined globals
     */
    int gbl_sysndx = -1;  // macro call counter
    int tot_gbl_name = 0;
    int tot_gbl_seta = 0;
    int tot_gbl_setb = 0;
    int tot_gbl_setc = 0;
    String[] gbl_set_name = new String[max_gbl_name]; 
    byte[]   gbl_set_type = (byte[])Array.newInstance(byte.class,max_gbl_name);
    int[]    gbl_set_start = (int[])Array.newInstance(int.class,max_lcl_name);
    int[]    gbl_set_end   = (int[])Array.newInstance(int.class,max_lcl_name);
    int[]    gbl_seta = (int[])Array.newInstance(int.class,max_gbl_set);
    byte[]   gbl_setb = (byte[])Array.newInstance(byte.class,max_gbl_set);
    String[] gbl_setc = new String[max_gbl_set]; 
    /*
     * ordinary symbol table for use by T' L' D'
     */
    int tot_sym = 0;
    int cur_sym = 0;
    String[] sym_name = new String[max_sym]; 
    char[]   sym_type = (char[])Array.newInstance(char.class,max_sym);
    int[]    sym_len  = (int[])Array.newInstance(int.class,max_sym);
    /*
     * DS/DC sym type and default length tables
     * copied from AZ390
     */
    static String dc_valid_types = "ABCDEFHLPSVX";
    static int[] dc_type_len = {
    		4,  // A
			1,  // B
			1,  // C
			8,  // D
			4,  // E
			4,  // F
			2,  // H
			16, // L
			1,  // P
			2,  // S
			4,  // V
			1   // X
			};
    /*
     * macro operation global variables
     */
    String[] mac_op_name = {
  	           "ACTR",    // 1 
               "AGO",     // 2
               "AIF",     // 3 
    	       "AINSERT", // 4 
               "ANOP",    // 5
               "AREAD",   // 6
               "GBLA",    // 7
               "GBLB",    // 8
               "GBLC",    // 9
               "LCLA",   // 10
               "LCLB",   // 11
               "LCLC",   // 12
               "MHELP",  // 13
               "MNOTE",  // 14
               "SETA",   // 15
               "SETAF",  // 16
               "SETB",   // 17
               "SETC",   // 18
               "SETCF",  // 19
               "MACRO",  // 20
               "MEND",   // 21
               "MEXIT",   // 22
			   "PUNCH",   // 23
			   "COPY"     // 24
			   };
    int max_op = mac_op_name.length;
    int mac_op_index = 0;
    /*
     * global key search table data
     *   O: - conditional macro opcodes
     *   M: - user defined macros 
     *   G: - global set variables
     * 
     */
    static int max_key_root = 1023;
    int max_key_tab = max_key_root + max_op + max_mac_name + max_gbl_name;
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
     * local key search table data
     *   P: - positional parm        
     *   K: - keyword parm        
     *   L: - local set variables
     * 
     */
    static int max_lcl_key_root = 13;
    int max_lcl_key_tab = max_mac_call_level * max_lcl_key_root + max_lcl_set + max_pos_parm + max_kwd_parm;
    int tot_lcl_key_tab  = max_lcl_key_root+1;
    int cur_lcl_key_root = 1;
    String lcl_key_text = null;
    int lcl_key_index = 0;
    int lcl_key_index_last = 0;
    Random lcl_key_rand = new Random();
    int lcl_key_hash = 0;
    String[]  lcl_key_tab_key   = new String[max_lcl_key_tab];
    int[]     lcl_key_tab_hash  = (int[])Array.newInstance(int.class,max_lcl_key_tab);
    int[]     lcl_key_tab_index = (int[])Array.newInstance(int.class,max_lcl_key_tab);
    int[]     lcl_key_tab_low   = (int[])Array.newInstance(int.class,max_lcl_key_tab);
    int[]     lcl_key_tab_high  = (int[])Array.newInstance(int.class,max_lcl_key_tab);

    /*
     * set expression global variables
     * including polish notation var and op stacks
     */
    String  exp_text  = null;
    int     exp_text_len = 0;
    int     exp_level = 0;
    boolean exp_end = false;
    boolean exp_ok  = false;
    char    exp_term_op = '~';      // terminate exp    
    char    exp_start_op = '~';     // start exp
    char    exp_string_op = '\'';   // start/end setc string
    char    exp_create_set_op = '&'; // created set &(...) oper
    boolean exp_var_replacement_mode = false; // for repace_vars()
    boolean exp_var_replacement_change = false; // set if replacements made
    boolean exp_parse_set_mode = false;  // for set/lcl/gbl alloc
    byte    exp_parse_set_type = 0;
    byte    exp_parse_set_loc  = 0;
    String  exp_parse_set_name = null;
    int     exp_parse_set_sub  = 0;
    char    exp_substring_op = ','; // calc substring '...'(e1,e2)
    char    exp_subscript_op = ')'; // calc var subscript value &var(subscript)
    int     exp_start_index = 0; // index to start of exp text
    int     exp_next_index = 0; // index to next op
    byte    exp_type = 0;       // requested type
    int     exp_seta = 0;
    byte    exp_setb = 0;
    String  exp_setc = "";
    String  exp_token = null;
    String  exp_prev_op = "" + exp_start_op;
    char    exp_prev_first = exp_start_op;
    byte    exp_prev_class = 0;
    char    exp_next_first = ' ';
    byte    exp_next_class = 0;
    int     exp_var_index = -1;  // lcl set index
    int     exp_var_count = -1;  // vars since last op
    boolean exp_var_last = false;
    int tot_exp_stk_var = 0;
    int tot_exp_stk_op  = 0;
    /*
     * set or sdt variable stack
     */
    byte[]    exp_stk_type = (byte[])Array.newInstance(byte.class,max_exp_stk);
    int[]     exp_stk_seta = (int[])Array.newInstance(int.class,max_exp_stk);
    byte[]    exp_stk_setb = (byte[])Array.newInstance(byte.class,max_exp_stk);
    String[]  exp_stk_setc = new String[max_exp_stk];
    /*
     * operator stack
     */
    String    exp_next_op = null;
    boolean exp_check_prev_op = false;;
    String[]  exp_stk_op   = new String[max_exp_stk];
    byte[]    exp_stk_op_class = (byte[])Array.newInstance(byte.class,max_exp_stk);
    /*
     * operator classes
     */
    static byte exp_class_add_sub = 1;
    static byte exp_class_mpy_div = 2;
    static byte exp_class_open    = 3;
    static byte exp_class_cls_sub = 4;
    static byte exp_class_str_op  = 5;
    static byte exp_class_term    = 6;
    static byte exp_class_comp    = 7;
    static byte exp_class_str_sub1= 8;
    static byte exp_class_str_sub2= 9;
    static byte exp_class_oper    = 10;
    static byte exp_class_not     = 11;
    static byte exp_class_and     = 12;
    static byte exp_class_or      = 13;
    static byte exp_class_xor     = 14;
    static byte exp_class_create_set = 15;
    /*
     * define exp actions based on last and
     * next operator class
     *     1  2  3  4  5  6  7  8  9 10 11 12 13 14 15
     *    +- * / (  )  .  ~ EQ  '  , ?'NOT AND OR XOR &( col = next_op
     *                                                   row = prev_op
     */ 
          int tot_classes = 15;
          int[] exp_action = {  
           1, 3, 3, 1, 0, 1, 1, 8, 1, 3, 1, 1, 1, 1, 3, // 1 +-  prev add/sub
           2, 2, 3, 2, 0, 2, 2, 8, 0, 3, 2, 2, 2, 2, 3, // 2 * / prev mpy/div
           3, 3, 3, 4, 3, 0, 3, 8, 0, 3, 3, 3, 3, 3, 3, // 3 (   prev open (...)
           3, 3, 3,11, 0, 0, 0, 8,11, 0, 3, 3, 3, 3, 0, // 4 )   prev var subscript
		   0, 0, 3, 5, 5, 5, 5, 8, 0, 3, 0, 0, 0, 0, 3, // 5 .   prev concat
           3, 3, 3, 6, 3, 6, 3, 8, 3, 3, 3, 3, 3, 3, 3, // 6 ~   prev terminator
		   1, 2, 3, 7, 3, 7, 7, 8, 0, 3, 7, 7, 7, 7, 3, // 7 LL  logical compares
		   3, 3, 3, 0, 0, 0, 0, 8, 0, 3, 0, 0, 0, 0, 3, // 8 '   string '....'
		   3, 3, 3,10, 0, 0, 0, 0, 9, 3, 0, 0, 0, 0, 3, // 9 ,   substring '...'(e1,e2)
		  12,12, 0,12,12,12,12, 0, 0, 0,12,12,12,12, 3, //10 ?'  prefix operator 
		   3, 3, 3,13, 3, 0, 3, 3, 0, 3,13,13,13,13, 3, //11 NOT logical
		   3, 3, 3,14, 3, 0, 3, 3, 0, 3, 3,14,14,14, 3, //11 AND logical
		   3, 3, 3,15, 3, 0, 3, 3, 0, 3, 3, 3,15,15, 3, //11 OR  logical
		   3, 3, 3,16, 3, 0, 3, 3, 0, 3, 3, 3, 3,16, 3, //11 XOR logical
		   0, 0, 0,17, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3 //10 ?'  prefix operator 
          };
     /* action code routines:
      *   0 error
      *   1 add/sub
      *   2 mpy/div
      *   3 push op
      *   4 POP  op
      *   5 string operation (concat,index,find)
      *   6 exit with result of expression
      *   7 compare operands (EQ,GE,GT,LE,LT,NE)
      *   8 switch substring mode '&v(&s)..'(&s1,&s2) on/off
      *   9 process 1st substring subscript 
      *  10 process 2nd substring subscript
	  *  11 process variable subscript or sublist
	  *  12 process prefix operator K', N' DOUBLE, etc
	  *  13 logical NOT
	  *  14 logical AND
	  *  15 logical OR
	  *  16 logical XOR
	  *  17 process created set symbol &(...)
	  */
    int    seta_value1 = 0;
    int    seta_value2 = 0;
    byte   setb_value1 = 0;
    byte   setb_value2 = 0;
    String setc_value1 = "";
    String setc_value2 = "";
    int    var_type1 = 0;
    int    var_type2 = 0;
    /*
     * current directory global variables
     */   
    String dir_cur = null;
    String dir_bal = null;
    String dir_cpy = null;
    String dir_dat = null;
    String dir_mac = null;
    String dir_mlc = null;
    String dir_pch = null;
    String pgm_name = null;
        /*
         * ASCII and EBCDIC printable character tables
         */
            String ascii_table = 
            "................" + //00
            "................" + //10
            " !" + '"' + "#$%&'()*+,-./" + //20
            "0123456789:;<=>?" + //30
            "@ABCDEFGHIJKLMNO" + //40
            "PQRSTUVWXYZ[\\]^_" + //50
            "`abcdefghijklmno" + //60
            "pqrstuvwxyz{|}~." + //70
            "................" + //80
            "................" + //90
            "................" + //A0
            "................" + //B0
            "................" + //C0
            "................" + //D0
            "................" + //E0
            "................";  //F0
            String ebcdic_table =
            "................" + //00
            "................" + //10
            "................" + //20
            "................" + //30
            " ...........<(+|" + //40
            "&.........!$*);." + //50
            "-/.........,%_>?" + //60
            ".........`:#@'=" + '"' + //70
            ".abcdefghi......" + //80
            ".jklmnopqr......" + //90
            ".~stuvwxyz......" + //A0
            "^.........[]...." + //B0
            "{ABCDEFGHI......" + //C0
            "}JKLMNOPQR......" + //D0
            "\\.STUVWXYZ......" + //E0
            "0123456789......";   //F0
            byte[] ascii_to_ebcdic = new byte[256];
            int ascii_lf = 10;
            int ascii_cr = 13;
            int ascii_period =  (int)'.';
            int ascii_space = (int) ' ';
            int ebcdic_period = 75;
            int ebcdic_space = 64;
  /* 
   * end of global mz390 class data and start of procs
   */
public static void main(String[] args) {
  /*
   * main is entry when executed from command line
   * Create instance of mz390 class and pass
   * parms to mz390 like z390 does.
   */
      mz390 pgm = new mz390();
      pgm.process_mz390(args,null);
}
public int process_mz390(String[] args,JTextArea log_text){
   /*
    *  expand macro MLC source file to BAL source file
    *
    *  Note this may be called directly from z390 GUI or
    *  from main when mz370 run from windows command line.
    *  if called from main, the log_text object will be null
    *  and local put_log function will route to console instead
    *  of the z390 log window.
    */
	    z390_log_text = log_text;
	    init_mz390(args,log_text);
    	if (load_mac(dir_mlc + pgm_name + ".MLC") != 0){
    		abort_error(39,"mlc file not found - " + dir_mlc + pgm_name + ".MLC");
    	} else {
    		mlc_line_end = tot_mac_line;
    	}
    	loading_mlc = false;
    	add_lcl_sys();
    	if (opt_trap){
    	   try {
               process_mac();
    	   } catch (Exception e){
    		   abort_error(84,"internal system exception - " + e.toString());
    	   }
    	} else {
    		process_mac();
    	}
	    exit_mz390();
		if    (log_text == null){
	    	System.exit(mz390_rc);
	    }
	    return mz390_rc;
}
private void init_mz390(String[] args, JTextArea log_text){
	/*
	 * 1.  initialize log routing
	 * 2.  set options
	 * 3.  open MLC and BAL buffered I/O files
	 */
	    mac_file_line[0] = "";
        version = "V1.0.00 09/30/05";  //dsh
        process_args(args);
        set_options();        
        set_gbl_var();
        init_ascii_ebcdic();
        init_mac_opcodes();
        tod_time_limit = max_time_seconds * 1000 + tod_start;
        /*
         * var_pattern used for finding and replacing       
         * scalar, subscripted, and crated &variables
         */
        try {
    	     var_pattern = Pattern.compile(
    			  "([&][\\(])"
    			+ "|([&][a-zA-Z$@#][a-zA-Z0-9$@#_]*)"    	
    			);
        } catch (Exception e){
  		     abort_error(1,"var pattern errror - " + e.toString());
        }
        /*
         * parm_pattern tokens:
         *   1.  ppp=   parm followed by = for detecting key vs pos
         *   2.  C'xxx' spaces, commas, and '' ok in xxx
         *   3.  'xxx' spaces, commas, and '' ok in xxx
         *   4.  xxx    no spaces or commas in xxx ('s ok)
         *   5.  ,      return to parse sublist and null parms
         *   6.  (      return to parse sublist parm
         *   7.  )      return to parse sublist parm
         *   8.  '      single quotes appended to parm text
         *   9.  space - detect end of parms and comments
         * */
    	try {
    	    parm_pattern = Pattern.compile(
    			"([a-zA-Z$@#][a-zA-Z0-9$@#_]*[=])"
    		  +	"|([c|C][']([^'|''])*['])" 
    		  + "|([']([^'|''])*['])"   
 			  + "|([^' ,()]+)"           
    	      + "|([' ,()])"
			  );
    	} catch (Exception e){
    		  abort_error(1,"parm pattern errror - " + e.toString());
    	}
       /*
        * proto_pattern is any one of 6 choices:
        *   1.  &vvv=  var followed by = for detecting key vs pos
        *   2.  C'xxx' spaces and '' ok in xxx
        *   3.  'xxx' spaces and '' ok in xxx
        *   4.  xxx    no spaces or commas in xxx ('s ok)
        *   5.  ,      return commas for detecting null pos
        *   6.  space  return space for detecting  comments
        * */
       	try {
       	    proto_pattern = Pattern.compile(
       			"([&][a-zA-Z$@#][a-zA-Z0-9$@#_]*[=])"
       		  +	"|([c|C']([^'|''])*['])" 
       		  + "|([']([^'|''])*['])"   
   			  + "|([^ ,]+)"           
       	      + "|([ ,])"              
   			  );
       	} catch (Exception e){
       		  abort_error(2,"proto pattern errror - " + e.toString());
       	}
        /*
         * macro label_pattern  .lll
         */
        	try {
        	    label_pattern = Pattern.compile(
        			"([.][a-zA-Z$@#][a-zA-Z0-9$@#_]*)"           
    			  );
        	} catch (Exception e){
        		  abort_error(3,"label pattern errror - " + e.toString());
        	}
            /*
             * pch_pattern for quoted string:
             *   1.  '...''...'
             * */
        	try {
        	    pch_pattern = Pattern.compile(
        			"([^']+)"
        	      + "|([']['])"
				  + "|(['])"             
    			  );
        	} catch (Exception e){
        		  abort_error(1,"pch pattern errror - " + e.toString());
        	}
        	/*
             * macro set/aif expression pattern
             *   1. &var   set variable
             *   2. &(...) set created variable
             *   3. ?'     prefix operators
             *   4. b'...' c'...' or x'...' sdts 
             *   5. nnnn   integer
             *   6. x      operator
             *   7. symbols and char strings
             */
            	try {
            	    exp_pattern = Pattern.compile(
            			"([&][&])"
            	      + "|([&][(])"
            	      + "|([&][a-zA-Z$@#][a-zA-Z0-9$@#_]*)" // &var
					  + "|([0-9]+)"                        // number
					  + "|([ /()',\\.\\+\\-\\*])"          // operators (\\ for reg exp. opers)
					  + "|([kK]['])"                       // K' character length of var
					  + "|([l|L]['])"                      // L' length attribute of symbol
					  + "|([n|N]['])"                      // N' number of parm subparms (n1,n2,n3)
					  + "|([t|T]['])"                      // T' type attribute of symbol
	  		    	  + "|([bB]['][0|1]+['])"              // B'0110' binary self def. term
	  		    	  +	"|([cC][']([^']|(['][']))*['])"    // C'ABCD' character self def. term
		    		  +	"|([xX]['][0-9a-fA-F]+['])"        // X'0F'   hex self defining term
					  + "|([a-zA-Z$@#][a-zA-Z0-9$@#_]*)"   // symbol or logical operator (AND, OR, XOR, NOT, GT, LT etc.)
					  + "|([^']+)"                         // string text including spaces
            	    );
            	} catch (Exception e){
            		  abort_error(4,"expression pattern errror - " + e.toString());
            	}

               /*
                * open MLC and BAL files
                */
		 open_files();
		 put_copyright();
}private void init_mac_opcodes(){
	/*
	 * add conditional macro opcodes to key index table
	 */
	int index = 0;
	while (index < max_op){
		if (find_key_index("O:" + mac_op_name[index]) == -1){
			add_key_index(index);
		} else {
			abort_error(88,"init opcode table error at " + mac_op_name[index]);
		}
		index++;
	}
}
private void init_ascii_ebcdic(){
	/*
	 * init ascii/ebcdic conversion tables
	 */	
	        int index1 = 0;
			while (index1 < 256){
			  ascii_to_ebcdic[index1] = (byte) ebcdic_period;
		      index1++;
			}
			index1 = 0;
	        int index2 = 0;
			while (index1 < 256){
				  index2 = (int) ebcdic_table.charAt(index1);
				  ascii_to_ebcdic[index2] = (byte) index1;
			      index1++;
			}
}
private void process_args(String[] args){
	/*
	 * process mlc bal macs options
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
	    abort_error(5,"missing MLC file - " + dir_mlc + pgm_name + ".MLC");
    }
}
private void set_dir_and_pgm_name(String file_name){
	/*
	 * set program name and default directory
	 * for all files from first parm.
	 * Notes:
	 *   1.  Use current directory if not path
	 *       specified with program name.
	 *   2.  Options SYSMLC, SYSMAC, SYSCPY
	 *       and SYSBAL can override default
	 *       directories.
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
	dir_bal = dir_cur;
	dir_cpy = dir_cur;
    dir_dat = dir_cur;
    dir_mac = dir_cur;
	dir_mlc = dir_cur;
	dir_pch = dir_cur;
    index = file_name.lastIndexOf("\\.");
    if (index != -1){  // strip extension if any
    	pgm_name = file_name.substring(0,index);
    } else {
    	pgm_name = file_name;
    }
}
private void open_files(){
	/*
	 * open BAL and set MAC directory
	 */
        bal_file = new File(dir_bal + pgm_name + ".BAL");
       	try {
       	    bal_file_buff = new BufferedWriter(new FileWriter(bal_file));
       	} catch (IOException e){
       		abort_error(8,"I/O error on BAL open - " + e.toString());
       	}
        File mac_dir_file = new File(dir_mac);
        if (!mac_dir_file.isDirectory()){
        	abort_error(9,"MAC directory invalid - " + dir_mac);
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
            } else if (tokens[index1].equals("NOLISTCALL")){
            	opt_listcall = false;
            } else if (tokens[index1].equals("NOLISTFILE")){
            	opt_listfile = false;
            } else if (tokens[index1].equals("NOSTATS")){
            	opt_stats = false;
            } else if (tokens[index1].equals("NOSYM")){
            	opt_sym = false;
            } else if (tokens[index1].equals("NOTIME")){
            	opt_time = false;
            } else if (tokens[index1].equals("NOTRAP")){
            	opt_trap = false;
            } else if (tokens[index1].length() > 5
            		&& tokens[index1].substring(0,5).equals("TIME(")){
            	max_time_seconds = Long.valueOf(tokens[index1].substring(5,tokens[index1].length()-1)).longValue();
            } else if (tokens[index1].length() > 7 
            		&& tokens[index1].substring(0,7).equals("SYSBAL(")){
            	dir_bal = tokens[index1].substring(7,tokens[index1].length()-1) + File.separator; 
            } else if (tokens[index1].length() > 7 
            		&& tokens[index1].substring(0,7).equals("SYSCPY(")){
            	dir_cpy = tokens[index1].substring(7,tokens[index1].length()-1) + File.separator; 
            } else if (tokens[index1].length() > 7 
            		&& tokens[index1].substring(0,7).equals("SYSDAT(")){
            	dir_dat = tokens[index1].substring(7,tokens[index1].length()-1) + File.separator; 
            } else if (tokens[index1].length() > 7 
            		&& tokens[index1].substring(0,7).equals("SYSMAC(")){
            	dir_mac = tokens[index1].substring(7,tokens[index1].length()-1) + File.separator; 
            } else if (tokens[index1].length() > 7 
            		&& tokens[index1].substring(0,7).equals("SYSMLC(")){
            	dir_mlc = tokens[index1].substring(7,tokens[index1].length()-1) + File.separator; 
            } else if (tokens[index1].length() > 7 
            		&& tokens[index1].substring(0,7).equals("SYSPCH(")){
            	dir_pch = tokens[index1].substring(7,tokens[index1].length()-1) + File.separator; 
            } else if (tokens[index1].equals("TRACE")){
            	opt_trace = true;
            	opt_listcall = true;
            } else if (tokens[index1].equals("TRACEALL")){
            	opt_trace = true;
            	opt_traceall = true;
            	opt_listcall = true;
            } else if (tokens[index1].equals("NOTIMING")){
            	opt_timing = false;
            }
            index1++;
        }
   }
private void set_gbl_var(){
	/*
	 * set global predefined variables
	 */
}
private String get_file_name(String parm_dir,String parm,String parm_type){
	   /*
	    * 1.  Strip long spacey name quotes if found
	    * 2.  Add directory and type if not specified
	    */
	   	    String file_name = null;
	    	if (parm.charAt(0) == '\"' 
	    		|| parm.charAt(0) == '\''){
	    		file_name = parm.substring(1,parm.length() - 1);
	    	} else {
	    	    file_name = parm;
	    	}
	    	if  (file_name.length() > 0
	            && file_name.indexOf('\\') == -1
	    		&& file_name.indexOf(':')  == -1){
	    		file_name = parm_dir.concat(File.separator + file_name);	
	    	}
	    	int index = file_name.indexOf(".");
	    	if (index == -1){
	    		file_name = file_name.trim() + parm_type;
	    	}
	    	return file_name;
}
private void process_mac(){
	/* 
	 * execute mlc as open code macro expanding
	 * any macros found and outputing all model
	 * statements to BAL file after substitution
	 * of any parms and macro variables.
	 *  
	 */
	     while (!mlc_eof){
	    	/*
	    	 * repeat executing nested macro code previously
	    	 * started by macro call 
	    	 */
	     	  mac_abort = false;
	     	  tot_mac_ins++;
		      if  (mac_line_index == mac_name_line_end[mac_call_name_index[mac_call_level]]){
	             if (mac_call_level > 0 && opt_trace){
	           	  	   put_log("TRACE MACRO CALL END " + mac_name[mac_call_name_index[mac_call_level]]);
	           	 }
		         if  (opt_listcall){
		         	 if  (mac_call_level > 0){
		           	     String lcl_sysndx = "    " + get_set_string("&SYSNDX",1);
				 	     lcl_sysndx = lcl_sysndx.substring(lcl_sysndx.length() - 4);
		   	 	         put_bal_line("* EXIT " + lcl_sysndx + " " + mac_name[mac_call_name_index[mac_call_level]]);
		         	 }
		   	     }
		         mac_call_level--;
		         if (mac_call_level >= 0){
		         	  mac_name_index = mac_call_name_index[mac_call_level];
		           	  mac_line_index = mac_call_return[mac_call_level];
		           	  actr_count     = mac_call_actr[mac_call_level];
		              tot_pos_parm = mac_call_pos_start[mac_call_level + 1];
		              tot_kwd_parm = mac_call_kwd_start[mac_call_level + 1];
		              tot_lcl_name  = mac_call_lcl_name_start[mac_call_level + 1];
		              tot_lcl_seta  = mac_call_lcl_seta_start[mac_call_level + 1];
		              tot_lcl_setb  = mac_call_lcl_setb_start[mac_call_level + 1];
		              tot_lcl_setc  = mac_call_lcl_setc_start[mac_call_level + 1];
		              tot_lcl_key_tab = mac_call_lcl_key_start[mac_call_level + 1];
		              cur_lcl_key_root = mac_call_lcl_key_root[mac_call_level + 1];
		         } else {
		           	  mlc_eof = true;
		         }
		         bal_line = null;
		      } else {
	               bal_line = mac_file_line[mac_line_index];
	               parse_bal_line();
	               mac_op_index = find_mac_op();
	               if (mac_op_index != -1){
	           	      exec_mac_op();      // execute macro operation
       		          bal_line = null;    // force macro execution cycle
	               } else if (bal_op != null) {
	           	      find_mac_name_index = find_mac(bal_op);
	           	      if (find_mac_name_index == -1){
	           	      	 cur_mac_file_name = dir_mac + bal_op + ".MAC";
	           	      	 find_mac_name_index = load_mac(cur_mac_file_name);
	           	      }
	             	  if (find_mac_name_index >= 0){
	           	         call_mac();      // call a nested macro
	           	         bal_line = null; // force macro exeuction cycle
	           	      }
	               }
			       mac_line_index++;
	          }
		      if   (bal_line != null){
		    	   if (opt_trace){
		    		   put_log("TRACE BAL OUTPUT - " + bal_line);
		    	   }
		      	   put_bal_line(bal_line);
	          }
		      if (!mlc_eof && actr_count <= 0){
		      	 abort_error(82,"actr limit exceeded");
		      }
			  if (opt_time
					&& (tot_mac_ins > next_time_check)){
					next_time_check = tot_mac_ins + next_time_ins;
					cur_date = new Date();
					tod_end = cur_date.getTime();
					if (tod_end > tod_time_limit){
                       abort_error(86,"time limit exceeded");
					}
				}
	     }
}
private int load_mac(String file_name){
	/*
	 * load macro from file (may be MLC or MAC)
	 * and return index of name else -2
	 * 
	 * 1.  Concatentate any continuations indicated
	 *     by non-blank in position 72.  Each 
	 *     continuation must start at position 16.
	 * 2.  Ignore .* macro comments
	 * 3.  Define any macro labels .xxx 
	 * 4.  if file not found, add entry with -2
	 *     index to avoid mult searches.
	 * 5.  if continue char in 72, delimit at first
	 *     ", " after 16.
	 */
	cur_mac_file = 0;
	cur_mac_line_num = 0;
	mac_file[cur_mac_file] = new File(file_name);
	String new_mac_name = mac_file[cur_mac_file].getName().toUpperCase();
	int index = new_mac_name.indexOf(".MAC");
	if (index > 0){
	   new_mac_name = new_mac_name.substring(0,index);
    }
	int save_mac_name_index = mac_name_index;
	if (tot_mac_name < max_mac_name){
		mac_name_index = tot_mac_name;
		add_key_index(mac_name_index);
		tot_mac_name++;
		mac_name[mac_name_index] = new_mac_name;
		mac_name_line_start[mac_name_index] = tot_mac_line;
		mac_name_lab_start[mac_name_index]  = tot_mac_lab;
	} else {
		abort_error(27,"max macros exceeded");
	}
	if (mac_file[cur_mac_file].isFile()){
		try {
			cur_mac_file = 0;
		    mac_file_buff[cur_mac_file] = new BufferedReader(new FileReader(mac_file[cur_mac_file]));
		    set_mac_file_num();
		} catch (IOException e){
			abort_error(26,"I/O error opening file - " + e.toString());
		}
		if (opt_trace){
			put_log("TRACE LOADING " + file_name);
		}
		tot_mac_load++;
		int mac_proto_index = 0;
		get_mac_line();
		while (mac_line != null
				&& tot_mac_line < max_mac_line){
			mac_file_line[tot_mac_line]     = mac_line;
			mac_file_name_num[tot_mac_line] = cur_mac_file_num;
			mac_file_line_num[tot_mac_line] = cur_mac_line_num;
			if (mac_op.length() > 0){
				if (tot_mac_name > 0 
					&& mac_op.equals(new_mac_name)
					&& mac_proto_index == 0){
				   mac_proto_index = tot_mac_line;
				   mac_name_line_start[mac_name_index] = tot_mac_line;
				} else if (mac_op.equals("MACRO")){
					mac_mend_level++;
				} else if (mac_op.equals("MEND")){
					mac_mend_level--;
					if (mac_mend_level == 0
						&& tot_mac_name > 1){
						mac_line = null;
					}
				}
			}
		    if  (mac_label.length() > 1
				&& mac_label.charAt(0) == '.'){
		    	if (mac_label.charAt(1) != '*'){
		    	  if (mac_mend_level == 0
		    	  	  || (mac_mend_level == 1 && tot_mac_name > 1)){
		            label_match = label_pattern.matcher(mac_label);
		            if (label_match.find()){
		    	       String label_name = label_match.group();
		               mac_lab_name[tot_mac_lab] = label_name.toUpperCase();
		               mac_lab_index[tot_mac_lab] = tot_mac_line -1;
		               if (mac_op.equals("ANOP")){
		            	   mac_lab_index[tot_mac_lab]++;
		               }
		               tot_mac_lab++;
		               mac_label = null;
		            } else {
		            	log_error(40,"invalid macro label - " + mac_label);
		            }
		    	  }
		    	} else {
		    		tot_mac_line--;  // remove .*
		        }
		    } else if (mac_label != null 
		    		   && mac_label.length() > 0
		    		   && mac_label.charAt(0) != '&'){
		    	add_sym(mac_label,mac_op,mac_parms);
		    }
			if  (mac_line != null){
				tot_mac_line++;
	            get_mac_line();
			}
		}
		if (tot_mac_line >= max_mac_line){
			abort_error(87,"maximum source lines exceeded");
		}
		mac_name_line_end[mac_name_index] = tot_mac_line;
		mac_name_lab_end[mac_name_index]  = tot_mac_lab;
	    mac_name_index = save_mac_name_index;
	    return tot_mac_name - 1;
	} else {
		mac_name_line_start[mac_name_index] = -2;
	}
	mac_name_index = save_mac_name_index;
	return -2;
}
private void set_mac_file_num(){
	/*
	 * find/add file name and set cur_mac_file_num
	 */
	String mac_file_key = mac_file[cur_mac_file].getPath(); 
	cur_mac_file_num = find_key_index(
			"F:" + mac_file_key);		
	if (cur_mac_file_num == -1){
		if (tot_mac_file_name < max_mac_file_name){
			cur_mac_file_num = tot_mac_file_name;
			tot_mac_file_name++;
			add_key_index(cur_mac_file_num);
			mac_file_name[cur_mac_file_num] = mac_file_key;
		}
	}
	mac_file_name_num[cur_mac_file] = cur_mac_file_num;
}
private void load_inline_mac(){
	/*
	 * load inline macro replacing existing macro
	 * if found
	 * 
	 * 1.  Define any macro labels .xxx 
	 */
	if (opt_trace){
		put_log("TRACE LOADING INLINE MACRO");
	}
	int new_mac_name_index = -1;
	mac_mend_level++; // load thru matching macro/mend
	int mac_proto_index = 0;
	mac_line_index++; // skip the macro statement
	while (mac_mend_level > 0
		&& mac_line_index < mac_name_line_end[mac_call_level]){ 
		mac_line = mac_file_line[mac_line_index];
		parse_mac_line();
		if (mac_op.length() > 0){
			if (mac_proto_index == 0){
			   mac_proto_index = mac_line_index;
			   new_mac_name_index = find_mac(mac_op);
			   if (new_mac_name_index < 0){
			   	   if (tot_mac_name < max_mac_name){
			   	      new_mac_name_index = tot_mac_name;
		   	          add_key_index(new_mac_name_index);
			   	      tot_mac_name++;
			       } else {
			   	      abort_error(60,"maximum macros exceeded for - " + bal_op);
			   	      return;
			       }
			   }
			   mac_name[new_mac_name_index] = mac_op;
			   mac_name_line_start[new_mac_name_index] = mac_proto_index;
			   mac_name_lab_start[new_mac_name_index] = tot_mac_lab;
			} else {
			    if (mac_op.equals("MACRO")){
				   mac_mend_level++;
			    } else if (mac_op.equals("MEND")){
				   mac_mend_level--;
				   if (mac_mend_level == 0){
					  mac_name_line_end[new_mac_name_index] = mac_line_index;
					  mac_name_lab_end[new_mac_name_index]  = tot_mac_lab;
					  return;
				   }
				}
			}
	        if  (mac_label.length() > 1
			     && mac_label.charAt(0) == '.'){
	    	     if (mac_label.charAt(1) != '*'){
	    	        if (mac_mend_level == 1){ // don't def inline macro labs yet
	    		       label_match = label_pattern.matcher(mac_label);
	                   if (label_match.find()){
	    	              String label_name = label_match.group();
	                      mac_lab_name[tot_mac_lab] = label_name.toUpperCase();
	                      mac_lab_index[tot_mac_lab] = mac_line_index;
	                      tot_mac_lab++;
	                      mac_label = null;
	                   } else {
	            	      log_error(40,"invalid macro label - " + mac_label);
	                   }
	    	       }
	             }
	        }
		}
        mac_line_index++;
	}
	mac_line = "";
	mac_line_index--;
    abort_error(61,"ending mend for inline macro not found");
}
private void get_mac_line(){
	/*
	 * get next mac line from mlc file
	 *   1.  Concatenating continuation lines
	 *       and parse mac line
	 *   2.  Truncate continued lines at first ", "
	 *   2.  Read nested copy files
	 */
	String temp_line = null;
    try {
    	boolean retry = true;
    	while (retry){
    		retry = false;
            temp_line = mac_file_buff[cur_mac_file].readLine();
            if (temp_line == null){
            	mac_file_buff[cur_mac_file].close();
            	cur_mac_file--;
            	if (cur_mac_file >= 0){
            		retry = true;
            		cur_mac_file_num = mac_file_cur_file_num[cur_mac_file];
            		cur_mac_line_num = mac_file_cur_line_num[cur_mac_file];
            		if (opt_trace){
            		    put_bal_line("* END OF COPY");
            		}
            	}
            }
    	}
        cur_mac_line_num++;
    	if  (temp_line == null){
    			mac_line = null;
   		} else if (temp_line.length() < 72
   				   || temp_line.charAt(71) == ' '){
   			mac_line = temp_line;
   		} else {
   		    mac_line = temp_line.substring(0,71);
   		    mac_line = trim_continue(mac_line);
            while (temp_line.length() > 71
            		&& temp_line.charAt(71) != ' '){
            	    temp_line = mac_file_buff[cur_mac_file].readLine();
            	    cur_mac_line_num++;
            	    if  (temp_line.length() > 16){
            	    	int temp_end = temp_line.length();
            	    	if (temp_end > 71)temp_end = 71;
            	    	mac_line = mac_line.concat(temp_line.substring(15,temp_end));
               		    mac_line = trim_continue(mac_line);
            	    } else { 
            	    	log_error(11,"continuation line < 16 characters - " + temp_line);
            	    }
            }   
   		}
    } catch (IOException e){
       	abort_error(29,"I/O error on file read " + e.toString());
    }
    parse_mac_line();
}
private void parse_mac_line(){
	/*
	 * parse mac line into label, op, parms
	 */
    mac_label = null;
    mac_op    = null;
    mac_parms = null;
    if (mac_line == null)return;
    if (opt_trace){
    	put_log("TRACE MAC LINE " + mac_line);
    }
    String[] tokens = split_line(mac_line);
    mac_label = tokens[0];
    mac_op    = tokens[1];
    mac_parms = tokens[2];
    if (loading_mlc && mac_op.toUpperCase().equals("COPY")){
       open_mac_copy_file();
    }
}
private void open_mac_copy_file(){
	/*
	 * open copy file specified in mac_parms
	 * Notes:
	 *   1.  ignore error duing MLC loading and
	 *       issue error on copy file not found
	 *       if not loading MLC.
	 */
	String new_mac_name = null;
	cur_mac_file++;
	if (cur_mac_file >= max_mac_file){
		cur_mac_file--;
		abort_error(100,"maximum nested copy files exceeded");
		return;
	}
	new_mac_name = get_file_name(dir_cpy,mac_parms,".CPY");
	mac_file[cur_mac_file] = new File(new_mac_name);
	new_mac_name = mac_file[cur_mac_file].getPath().toUpperCase();
	mac_file[cur_mac_file] = new File(new_mac_name);
	if (mac_file[cur_mac_file].isFile()){
		try {
			if (loading_mlc){
		       mac_file_buff[cur_mac_file] = new BufferedReader(new FileReader(mac_file[cur_mac_file]));
			   set_mac_file_num();
			   mac_file_cur_line_num[cur_mac_file - 1] = cur_mac_line_num;
			   cur_mac_line_num = 0;
			} else {
				cur_mac_file--; // ignore copy after loading
			}
		} catch (IOException e){
			cur_mac_file--;
			abort_error(26,"I/O error opening file - " + e.toString());
		}
	} else {
		cur_mac_file--;
		if (!loading_mlc){
		    log_error(101,"copy file not found - " + new_mac_name);
		}
	}
}
private void put_bal_line(String bal_line){
	/*
	 * write bal_line to bal file
	 * after reformating:
	 * 
	 * 1.  Strip off macro labels .xxx
	 * 2.  align opcode to postion 10
	 * 3.  align parms  to position 16
	 * 4.  split into continuation lines if any
	 */
	if (bal_file == null){
		return;
	}
	if (bal_line.length() > 0 && bal_line.charAt(0) != '*'){
	   boolean reformat = false;
	   String tokens[] = split_line(bal_line);
	   if (tokens[0].length() > 0 && tokens[0].charAt(0) == '.'){
		   tokens[0] = "";
	   }
	   if  (opt_sym && tokens[0].length() > 0){
           add_sym(tokens[0].toUpperCase(),tokens[1].toUpperCase(),tokens[2].toUpperCase());
	   }
	   if  (tokens[0].length() == 0 && tokens[1].length() > 0){
	   	   reformat = true;
	   	   tokens[0] = "        ";
	   } else if (tokens[0].length() < 8){
	   	   reformat = true;
	   	   tokens[0] = tokens[0].concat("        ").substring(0,8);   
	   }
	   if  (tokens[1].length() > 0 && tokens[0].length() + tokens[1].length() < 14){
	   	   reformat = true;
	   	   tokens[1] = tokens[1].concat("    ").substring(0,13 - tokens[0].length());
	   }
	   if  (reformat){
	   	   if (tokens[2] == null)tokens[2] = "";
	       bal_line = tokens[0] + " " + tokens[1] + " " +tokens[2];
	   }
	}
	try {
		if  (bal_line.length() < 72){
	        bal_file_buff.write(bal_line + "\r\n");
	        tot_bal_line++;
		} else {
			bal_file_buff.write(bal_line.substring(0,71) + "X\r\n");
			tot_bal_line++;
			String text_left = bal_line.substring(71);
			while (text_left.length() > 0){
				if  (text_left.length() > 56){
					String cont_line = "               " 
						             + text_left.substring(0,56) 
									 + "X\r\n";
					bal_file_buff.write(cont_line);
					tot_bal_line++;
					text_left = text_left.substring(56);
				} else {
					bal_file_buff.write("               " 
							            + text_left + "\r\n");
					text_left = "";
				}
			}
		}
	} catch (IOException e){
		abort_error(13,"I/O error on BAL write - " + e.toString());
	}
}
private void parse_bal_line(){
	/*
	 * 1.  Substitute any macro variables found
	 * 2.  Set bal_label and bal_op
	 * 3.  If macro label .xxx add definition and
	 *     then remove label from source
	 */
	if (opt_trace){
		put_log("TRACE BAL PARSING  " + bal_line);
	}
	bal_label = null;
	bal_op    = null;
	bal_parms = null;
	if  (bal_line == null 
			|| bal_line.length() == 0
			|| bal_line.charAt(0) == '*'){
		return;
	}
	split_bal_line();
    if  (find_mac_op() == -1){
        bal_line = replace_vars(bal_line);
        if (exp_var_replacement_change){
        	split_bal_line();
        }
    }
}
private void split_bal_line(){
	/*
	 * split bal_line into bal_label, bal_op,
	 * and bal_parms
	 */
    String[] tokens = split_line(bal_line);
    bal_label = tokens[0];
    bal_op    = tokens[1];
    bal_parms = tokens[2];
}
private String replace_vars(String text){
	/* 
	 * replace all variables in text
	 * and set var_replacement if changed
	 */
	exp_var_replacement_mode = false;
	exp_var_replacement_change = false;
	bal_text = text;
	var_match   = var_pattern.matcher(bal_text);
    bal_text_index0 = 0; // end of prev. parm
    String new_text = "";
    while (var_match.find()){
        exp_var_replacement_mode = true;
        exp_var_replacement_change = true;
        bal_text_index1 = var_match.start();
        parm_value = calc_setc_exp(bal_text.substring(bal_text_index1),0);
       	bal_text_index2 = bal_text_index1 + exp_next_index;
	    if  (bal_text_index2 < bal_text.length()
	   		&& bal_text.charAt(bal_text_index2) == '.'){
	   	    bal_text_index2++; // skip trailing . in parm substitution
	    }
   	    if (bal_text_index0 < bal_text_index1){
   	       new_text = new_text + bal_text.substring(bal_text_index0,bal_text_index1);
        }
        new_text = new_text + parm_value;
        bal_text = bal_text.substring(bal_text_index2);
	    var_match   = var_pattern.matcher(bal_text);
        bal_text_index0 = 0;
    }
    if (exp_var_replacement_mode){
       if (bal_text_index0 < bal_text.length()){
       	  new_text = new_text.concat(bal_text.substring(bal_text_index0));
       }
       text = new_text;
    }
    exp_var_replacement_mode = false;
    return text;
}
private String[] split_line(String line){
	/*
	 * split line into label, opcode, parms 
	 * 
	 * 3 fields are non-null but may have 0 length 
	 * and there may be trailing comment on parms
	 */
	String[] tokens = line.split("\\s+",3);
	String[] split_tokens = new String[3];
	if (tokens.length > 0){
		split_tokens[0] = tokens[0];
		if (tokens.length > 1){
			split_tokens[1] = tokens[1];
			if (tokens.length > 2){
				split_tokens[2] = tokens[2];
			} else {
				split_tokens[2] = "";
			}
		} else {
			split_tokens[1] = "";
			split_tokens[2] = "";
		}
	} else {
		split_tokens[0] = "";
		split_tokens[1] = "";
		split_tokens[2] = "";
	}
    return split_tokens;
}
private boolean find_var(String var_name){
	/*
	 * find parm or set variable and return true if found
	 * also set the following:
	 * 1.  var_type = seta|setb|setc|parm (1-4)
	 * 2.  var_loc  = lcl|gbl|pos|kw|syslist
	 * 3.  setc_value = parm value if not syslist
	 * 4.  var_name_index = index to name found else -1 
	 *
	 * Note caller must handle subscript or 
	 * sublist in exp or bal parm processing
	 * Notes:
	 *   1.  First search parms and then set variables
	 *   2.  Convert to upper case
	 */
	var_name = var_name.toUpperCase(); 
	if  (var_name.equals("&SYSLIST")) {
		var_type = var_sublist_type;
		var_loc = syslist_loc;
		var_name_index = -1;
		return true;
	}
	/*
	 * search pos parms 
	 */
	var_name_index = find_lcl_key_index("P:" + var_name);
	if (var_name_index != -1){
		var_type = var_parm_type;
		var_loc  = pos_loc;
		setc_value = mac_call_pos_parm[var_name_index];
		return true;
	}
	/*
	 * search keyword parms
	 */
	var_name_index = find_lcl_key_index("K:" + var_name);
	if (var_name_index != -1){
		var_type = var_parm_type;
		var_loc  = kw_loc;
		setc_value = mac_call_kwd_parm[var_name_index];			
		return true;
	}
	if (find_set(var_name,1)){
		return true;
	}
	return false;
}
private int get_sublist_count(String list){
	/* 
	 * return number of parms in sublist
	 */
	if (list.length() == 0){
	    return 0;
	} else if (list.charAt(0) != '('){
		return 1;
	} else {
		sublist_count = 1;
	    get_sublist(list,0x7fffff); // get tot count
	    return sublist_count;
	}
}
private String get_sublist(String list,int sublist_index){
	/*
	 * 1.  parse list and return sublist requested 
	 * or empty list
	 * 2.  incr sublist_count for each , at level 1
	 */
	if (sublist_index < 1){
		log_error(85,"invalid sublist index - " + sublist_index);
	}
	if (list.length() < 3 || list.charAt(0) != '('){
		if (sublist_index == 1){
			return list;
		} else {
			return "";
		}
	}
	parm_match = parm_pattern.matcher(list.substring(1));
	int index = 1;
	int level = 1;
	String sublist = "";
	while (parm_match.find()){
		String token = parm_match.group();
		switch (token.charAt(0)){
		case ',':
			if (level == 1){
				sublist_count++;
				if (index == sublist_index){
				   return sublist;
				} else {
					index++;
					sublist = "";
				}
			} else {
				sublist = sublist + ',';
			}
			break;
		case '(':
			level++;
			sublist = sublist + '(';
			break;
		case ')':
			level--;
			if (level == 0 && index == sublist_index){
				return sublist;
			}
			sublist = sublist + ')';
			break;
		default:
			sublist = sublist.concat(token);
		    break;
		}		
	}
	if (index == sublist_index){
		return sublist;
	} else {
	    return "";
	}
}
private String trim_continue(String line){
	/*
	 * use parm parser to find ", " on continued
	 * line and trim to comma.  This allows ", "
	 * to appear in recognized quoted parms
	 */
	parm_match = parm_pattern.matcher(line);
	int index = 0;
	while (parm_match.find()){
		if (parm_match.group().equals(" ")){
			index = parm_match.start();
			if (index > 0 && line.charAt(index-1) == ','){
				return line.substring(0,index);
			}
		}
	}
	return line;
}
private String get_set_string(String name,int sub){
	/*
	 * return string value of set variable
	 * else return null
	 */
	if (find_set(name,sub)){
       switch (var_type){
	   case 1:
		   return "" + seta_value;
	   case 2:
		   return "" + setb_value;
	   case 3:
	   	   if (setc_value == null){
	   	   	  return "";
	   	   } else {
		      return setc_value;
	   	   }
	   default: 
	  	 abort_error(68,"invalid case index");
       }
	}
	return null;
}
private int find_mac_op(){
	/*
	 * return index of macro operation 
	 * or return -1 if model statement
	 */
	if (bal_op == null || bal_op.length() == 0){
		return -1;
	}
	return find_key_index(("O:" + bal_op).toUpperCase());
}
private void exec_mac_op(){
	/*
	 * execute macro operation (set,aif, ago, etc.)
	 * 
	 * Note case index values must match
	 * mac_op_name array values.
	 */
	bal_op_ok = false;
	switch (mac_op_index + 1){
	case 1:  // ACTR  
		bal_op_ok = true;
		actr_count = calc_seta_exp(bal_parms,0);
		break;
    case 2:  // AGO 
       	bal_op_ok = true;
	    actr_count--;
    	mac_line_index = get_label_index(bal_parms);
    	if (mac_line_index == -1){
    		abort_error(16,"macro label not found - " + bal_parms);
    	}
    	break;
    case 3:  // AIF  
       	bal_op_ok = true;
    	if (calc_setb_exp(bal_parms,0) != 0){
		    actr_count--;
    		mac_line_index = get_label_index(bal_parms.substring(exp_next_index));
        	if (mac_line_index == -1){
        		abort_error(16,"macro label not found - " + bal_parms);
        	}
    	}
    	break;
    case 4:  // AINSERT
    	break;
    case 5:  // ANOP 
       	bal_op_ok = true;
    	break;
    case 6:  // AREAD
    	bal_op_ok = true;
    	String dat_line = get_aread_string();
    	put_setc_string(dat_line);
    	break;
    case 7:  // GBLA 
       	bal_op_ok = true;
    	exp_parse_set_loc = gbl_loc;
    	exp_parse_set_type = var_seta_type;
    	alloc_set();
    	break;
    case 8:  // GBLB 
       	bal_op_ok = true;
    	exp_parse_set_loc = gbl_loc;
    	exp_parse_set_type = var_setb_type;
    	alloc_set();
    	break;
    case 9:  // GBLC
       	bal_op_ok = true;
    	exp_parse_set_loc = gbl_loc;
    	exp_parse_set_type = var_setc_type;
    	alloc_set();
    	break;
    case 10:  // LCLA
       	bal_op_ok = true;
    	exp_parse_set_loc = lcl_loc;
    	exp_parse_set_type = var_seta_type;
    	alloc_set();
    	break;
    case 11:  // LCLB
       	bal_op_ok = true;
    	exp_parse_set_loc = lcl_loc;
    	exp_parse_set_type = var_setb_type;
    	alloc_set();
    	break;
    case 12:  // LCLC
       	bal_op_ok = true;
    	exp_parse_set_loc = lcl_loc;
    	exp_parse_set_type = var_setc_type;
    	alloc_set();
    	break;
    case 13:  // MHELP 
    	break;
    case 14:  // MNOTE 
       	bal_op_ok = true;
    	bal_line = replace_vars(bal_line);
    	if (bal_parms.length() > 0 
    		&& bal_parms.charAt(0) != '\''
    	  	&& bal_parms.charAt(0) != ','
    		&& bal_parms.charAt(0) != '*'){
    		seta_value1 = calc_seta_exp(bal_parms,0);
    		if (seta_value1 > 0){
    			mz390_rc = seta_value1;
    			log_error(93,bal_line);
    		} else {
    	    	put_bal_line(bal_line);
    		}    		
    	} else {
    		put_bal_line(bal_line);
    	}
    	break;
    case 15:  // SETA 
       	bal_op_ok = true;
       	exp_parse_set_type = var_seta_type;
    	if (parse_set_var(bal_label,0)){
        	if (var_type != var_seta_type){
            	log_error(17,"invalid set variable type for - " + bal_label);
            	break;
        	}
   	        store_loc = var_loc;
 	        store_name_index = var_name_index;
    	    store_seta_index = seta_index;
    	} else { 
      		if (set_sub != 1){
    			log_error(64,"subscripted set not defined - " + set_name);
    			break;
    		} 
	        add_lcl_set(exp_parse_set_name,var_seta_type,1);
    		store_loc = lcl_loc;
    		store_name_index = var_name_index;
    		store_seta_index = seta_index;
    	}
    	if  (store_loc == lcl_loc){
       	    lcl_seta[store_seta_index] = calc_seta_exp(bal_parms,0);
       	    if (opt_trace){
       	    	put_log("TRACE SETA " + lcl_set_name[store_name_index] + "(" + set_sub + ")= " + lcl_seta[store_seta_index]);
       	    }
    	} else {
    	    gbl_seta[store_seta_index] = calc_seta_exp(bal_parms,0);
       	    if (opt_trace){
       	    	put_log("TRACE SETA " + gbl_set_name[store_name_index] + "(" + set_sub + ")= " + gbl_seta[store_seta_index]);
       	    }
    	}
    	break;
    case 16:  // SETAF
    	break;
    case 17:  // SETB 
       	bal_op_ok = true;
       	exp_parse_set_type = var_setb_type;
    	if (parse_set_var(bal_label,0)){
       	    if (var_type != var_setb_type){
           		log_error(17,"invalid set variable type for - " + bal_label);
           		break;
       		}
    	    store_loc = var_loc;
    	    store_name_index = var_name_index;
    	    store_setb_index = setb_index;
    	} else {
    		if (set_sub != 1){
    			log_error(64,"subscripted set not defined - " + set_name);
    			break;
    		}
	        add_lcl_set(exp_parse_set_name,var_setb_type,1);
    		store_loc = lcl_loc;
    		store_name_index = var_name_index;
    		store_setb_index = setb_index;
    	}
        if  (store_loc == lcl_loc){
       	    lcl_setb[store_setb_index] = calc_setb_exp(bal_parms,0);
       	    if (opt_trace){
       	    	put_log("TRACE SETB " + lcl_set_name[store_name_index] + "(" + set_sub + ")= " + lcl_setb[store_setb_index]);
       	    }
        } else {
    	    gbl_setb[store_setb_index] = calc_setb_exp(bal_parms,0);
       	    if (opt_trace){
       	    	put_log("TRACE SETB " + gbl_set_name[store_name_index] + "(" + set_sub + ")= " + gbl_setb[store_setb_index]);
       	    }
        }
    	break;
    case 18:  // SETC  
       	bal_op_ok = true;
       	exp_parse_set_type = var_setc_type;
       	String setc_string = calc_setc_exp(bal_parms,0);
        put_setc_string(setc_string);
    	break;
    case 19:  // SETCF
    	break;
    case 20:  // MACRO
    	load_inline_mac();
    	bal_op_ok = true;
    	break;
    case 21:  // MEND
    	break;
    case 22:  // MEXIT 
    	mac_line_index = mac_name_line_end[mac_call_name_index[mac_call_level]] - 1;
    	bal_op_ok = true;
    	break;
    case 23:  // PUNCH
    	bal_op_ok = true;
    	bal_parms = replace_vars(bal_parms);
    	put_pch_line(bal_parms);
   	    put_bal_line(bal_line);
    	break;
    case 24:  // COPY
    	bal_op_ok = true;
   	    put_bal_line(bal_line);
    	if (!loading_mlc){
    	   mac_parms = bal_parms;
    	   open_mac_copy_file(); // issues error if not found else just list
    	}
    	break;
	default: 
	  	 abort_error(68,"invalid case index");
	}
	if (!bal_op_ok){
		put_bal_line(bal_line);
		log_error(47,"macro operation not supported - " + bal_op);
	}
}
private void alloc_set(){
	/*
	 * parse set scalar,array, created set variables
	 * exp_parse_set_loc = lcl_set | gbl_set
	 * exp_parse_set_type = var_seta_type| var_setb_type | var_setc_type
	 */
	String text = bal_parms;
    int index = 0;
    while (!mac_abort && index < text.length()){
    	if (text.charAt(index) == ','){
    		index++;
    	}
    	if (text.charAt(index) != '&'){
    		text = "&" + text;
    	}
   		if (!parse_set_var(text,index)){
   			if (exp_parse_set_name != null){
   				if (exp_next_index < text.length() 
   					&& text.charAt(exp_next_index) == '('){
   					exp_parse_set_sub = calc_seta_exp(text,exp_next_index + 1);
   					exp_next_index++;  // skip subscript )
   				}
   				if (exp_parse_set_loc == lcl_loc){
   					add_lcl_set(exp_parse_set_name,exp_parse_set_type,exp_parse_set_sub);
   				} else {
   					add_gbl_set(exp_parse_set_name,exp_parse_set_type,exp_parse_set_sub);
   				}
   			} else {
   				log_error(105,"syntax error at " + text.substring(index));
   			}
   		} else if (exp_parse_set_loc != var_loc){
   			log_error(106,"set local/global conflict for - " + text.substring(index));
   		} else if (exp_parse_set_loc != var_loc){
   			log_error(107,"set type conflict for - " + text.substring(index));
   		}
   		index = exp_next_index;
    }
}
private int calc_seta_exp(String text,int text_index){
	/*
	 * evaluate seta expression 
	 */
	exp_type = var_seta_type;
	calc_exp(text,text_index);
	switch (exp_type){
	   case 1:
	   	  return exp_seta;
	   case 2:
	   	  if (exp_setb == 1){
	   	  	 return 1;
	   	  } else {
	   	  	 return 0;
	   	  }
       case 3:
       	  return get_int_from_string(exp_setc,10);
	   default: 
		  abort_error(68,"invalid case index");
	}
	return -1;
}
private byte calc_setb_exp(String text,int text_index){
	/*
	 * evaluate setb expression 
	 * 
	 */
	exp_type = var_setb_type;
	calc_exp(text,text_index);
	switch (exp_type){
	   case 1:
          return (byte) exp_seta;
	   case 2:
          return exp_setb;
       case 3:
       	  if (get_int_from_string(exp_setc,10) == 1){
       	  	 return 1;
       	  } else {
       	  	 return 0;
       	  }
	   default: 
		  abort_error(68,"invalid case index");
	}
	return 0;
}
private String calc_setc_exp(String text,int text_index){
	/*
	 * evaluate setc expression
	 */
	exp_type = var_setc_type;
	calc_exp(text,text_index);
	switch (exp_type){
	   case 1:
	   	  return "" + exp_seta;
	   case 2:
	   	  if (exp_setb == 1){
	   	  	 return "1";
	   	  } else {
	   	  	 return "0";
	   	  }
       case 3:
       	  return exp_setc;
	   default: 
		  	 abort_error(68,"invalid case index");
	}
	return "";
}
private void put_setc_string(String setc_string){
	/*
	 * store setc string at variable in bal_label
	 * (used by setc and aread)
	 */
	if (parse_set_var(bal_label,0)){
		if (var_type != var_setc_type){
	    	log_error(17,"invalid set variable type for - " + bal_label);
	    	return;
		}
	    store_loc = var_loc;
	    store_name_index = var_name_index;
	    store_setc_index = setc_index;
	} else {
		if (set_sub != 1){
			log_error(64,"subscripted set no defined - " + set_name);
			return;
		}
        add_lcl_set(exp_parse_set_name,var_setc_type,1);
		store_loc = lcl_loc;
		store_name_index = var_name_index;
		store_setc_index = setc_index;
	}
	if  (store_loc == lcl_loc){
   	    lcl_setc[store_setc_index] = setc_string;
   	    if (opt_trace){
   	    	put_log("TRACE SETC " + lcl_set_name[store_name_index] + "(" + set_sub + ")= " + lcl_setc[store_setc_index]);
   	    }
	} else {
	    gbl_setc[store_setc_index] = setc_string;
   	    if (opt_trace){
   	    	put_log("TRACE SETC " + gbl_set_name[store_name_index] + "(" + set_sub + ")= " + gbl_setc[store_setc_index]);
   	    }
    }
}
private String get_aread_string(){
	/*
	 * read next mlc source line or next record 
	 * from file specified in keword DDNAME= 
	 * 1.  DDNAME= is extention to HLL assembler
	 *     where external variable defines file to
	 *     read for AREAD.
	 */
	if (bal_parms.length() > 6
		&& bal_parms.substring(0,7).toUpperCase().equals("DDNAME=")){
		String ddname = bal_parms.substring(7);
		String file_name = get_ddname_file_name(ddname);
		file_name = get_file_name(dir_dat,file_name,".DAT");
		if (dat_file_name == null 				
				|| dat_file == null
				|| !dat_file_name.equals(file_name)
				){
			if (dat_file != null){
				try {
				    dat_file_buff.close();
				} catch (IOException e){
					abort_error(69,"I/O error on AREAD close - " + e.toString());
					return "";
				}
			}
			dat_file_name = file_name;
			dat_file = new File(dat_file_name);
			try {
			    dat_file_buff = new BufferedReader(new FileReader(dat_file));
			} catch (IOException e){
				abort_error(70,"I/O error on AREAD open - " + e.toString());
				return "";
			}
		}
		try {
			String text = dat_file_buff.readLine();
			if (text == null || text.length() == 0){
				dat_file_buff.close();
				dat_file = null;
				return "";
			} else {
				return text;
			}
		} catch (IOException e){
			abort_error(71,"I/O error on AREAD file read - " + e.toString());
		    return "";
		}		
	} else {
		if (mac_call_level > 0
				&& mac_call_return[0] < mlc_line_end){
			mac_call_return[0]++;
			return mac_file_line[mac_call_return[0]-1];
		} else {
			abort_error(72,"read past end of inline AREAD data");
		}
	}
	return "";
}
private String get_ddname_file_name(String ddname){
	/*
	 * verify ddname is defined as environment
	 * variable pointing to valid file and 
	 * return full path else abort
	 */
	String temp_file_name = System.getenv(ddname);
	if (temp_file_name != null && temp_file_name.length() > 0){
		File temp_file = new File(temp_file_name);
		if (temp_file.isFile()){
			return temp_file.getPath();
		} else {
			abort_error(62,"ddname=" + ddname + " invalid file specification=" + temp_file_name);
		}
	} else {
		abort_error(62,"ddname=" + ddname + " not found");
	}
	return "";
}
private void put_pch_line(String pch_parms){
	/* 
	 * write PUNCH 'text' to pch file
	 * 1.  If ,DDNAME= follows 'text' write to
	 *     specified file instead of default
	 *     filename.pch
	 */
	String pch_text = "";
	String token = null;
    pch_match = pch_pattern.matcher(pch_parms.substring(1));
    boolean pch_eod = false;
	while (!pch_eod && pch_match.find()){
	       token = pch_match.group();
	       if (token.charAt(0) != '\''){
	       	  pch_text = pch_text + token;
	       } else if (token.length() == 2){
	       	  pch_text = pch_text + "'";
	       } else {
	       	  pch_eod = true;
	       }
	}
	if (!pch_eod){
		log_error(73,"invalid punch text - " + pch_parms);
		return;
	} else {
		int index = pch_match.end()+1;
		if (pch_parms.substring(index).length() > 8
		    && pch_parms.substring(index,index+8).toUpperCase().equals(",DDNAME=")){
		   String ddname = pch_parms.substring(index+8);
		   String file_name = get_ddname_file_name(ddname);
		   file_name = get_file_name(dir_pch,file_name,".PCH");
		   if  (pch_file_name == null 				
				|| pch_file == null
				|| !pch_file_name.equals(file_name)
				){
			   if  (pch_file != null){
				   try {
				       pch_file_buff.close();
				   } catch (IOException e){
					   abort_error(74,"I/O error on PUNCH close - " + e.toString());
					   return;
				   }
			   }
			   pch_file_name = file_name;
			   open_pch_file();
		   }
		} else {
		    if (pch_file_name == null){
		       pch_file_name = dir_pch + pgm_name + ".PCH";
		       open_pch_file();
		    }
		}
	    try {
		   pch_file_buff.write(pch_text + "\r\n");
		} catch (IOException e){
		   abort_error(76,"I/O error on PUNCH file write - " + e.toString());
		}		
	}
}
private void open_pch_file(){
	/*
	 * open pch_file_name
	 * 
	 */
	   pch_file = new File(pch_file_name);
	   try {
	       pch_file_buff = new BufferedWriter(new FileWriter(pch_file));
	   } catch (IOException e){
		   abort_error(75,"I/O error on PUNCH open - " + e.toString());
	   }
}
private boolean calc_exp(String text,int text_index){
	/*
	 * parse set/aif/variable expression and 
	 * return true if ok and set
	 * set exp_var_index to result
	 * Note:
	 *   1.  If parse_set_mode then exit without
	 *       error true/false indicating if
	 *       set variable exists or not.
	 *   2.  If var_replacement_mode, exit after
	 *       retrieving first variable and before
	 *       proceeding to next operator.
	 *   3.  Always turn off parse_set_mode at exit
	 */
	   exp_text       = text;
	   exp_start_index = text_index;
	   exp_text_len   = text.length();
       exp_match = exp_pattern.matcher(text.substring(exp_start_index));
       tot_exp_stk_var = 0;
       tot_exp_stk_op  = 0;
   	   exp_level = 0;
   	   exp_next_first = '?';  // not space or exp_term_op
       exp_end = false;
       exp_ok  = false;
   	   exp_var_count = -1;  // reset var count for unary minus
       exp_set_prev_op();
       exp_set_next_op();
       while (!exp_end){
       	    exp_check_prev_op = false;
            exp_perform_op();
	        if (!exp_end){
	        	exp_set_prev_op();
	            if (!exp_check_prev_op){
	            	exp_set_next_op();
	            }
           }
	   }
       exp_parse_set_mode = false;
       return exp_ok;
}
private void exp_set_prev_op(){
	/*
	 * set exp_prev_op from stack or 
	 * set to exp_start_op (same as exp_term_op)
	 * also set exp_prev_class and exp_prev_first
	 */
	 if  (tot_exp_stk_op > 0){
	    exp_prev_op = exp_stk_op[tot_exp_stk_op - 1];
	    exp_prev_class = exp_stk_op_class[tot_exp_stk_op - 1];
	 } else {
		exp_prev_op = "" + exp_start_op;
		exp_prev_class = exp_class_term;
	 }
	 exp_prev_first = exp_prev_op.charAt(0);
}
private void exp_set_next_op(){
	/*
	 * get next expression operator
	 * and push preceding variables on stack
	 * Also set the following:
	 * 1.  exp_token
	 * 2.  exp_next_first
	 * 3.  exp_next_op (uppercase)
	 * 4.  exp_next_class
	 * Notes:
	 * 1.  push zero for unary +- based on
	 *     previous setting of exp_last_var.
	 * 2.  push ordinary symbols starting with A-Z$@#
	 *     as SDT string assuming there preceding T' type oper
	 */
    exp_var_last  = true; // force first try
	while (!exp_end && exp_var_last){
		exp_var_count++;      // count vars pushed (reset by exp_next_op)
		exp_var_last = false; // assume no more
		if (exp_next_first == exp_term_op){
			return;
	    } else if ((exp_var_replacement_mode 
			        && tot_exp_stk_op  == 0
			        && tot_exp_stk_var == 1
		    	   )
	   		    || !exp_match.find()){
	   			   exp_set_term_op();
		        return;
		}
	    exp_token = exp_match.group();
	    exp_next_index = exp_start_index + exp_match.end();
	    exp_next_first = exp_token.charAt(0);
	    exp_next_op = exp_token.toUpperCase();
	   /*
	    * push &var in string and non-string mode
	    */
	    if (exp_next_first == '&'){ 
	     	if  (exp_token.length() > 1 && exp_token.charAt(1) != '&'){
	     		if (exp_token.charAt(1) == '('){
	     			exp_next_class = exp_class_create_set;
	     			exp_level++;   // force level up 1
	     			exp_push_op(); // put created set op on stack
	     			exp_set_prev_op();
	     			exp_push_string("");
	     		} else {
	                exp_push_var();
	     		}
	     	} else if (exp_prev_first == exp_string_op
	     			   || exp_prev_first == exp_create_set_op){
	     		exp_stk_setc[tot_exp_stk_var - 1] = exp_stk_setc[tot_exp_stk_var - 1].concat("&&");
	     		if (exp_next_char() == '.'){
	     			skip_next_token(); // skip trailing . in string substitution
	     		}
	     	} else {
	     		log_error(37,"invalid expression variable - " + exp_token);
	     	}
            exp_var_last = true;
       /*
        * concat the rest if string mode
        */
	    } else if (exp_next_first != exp_term_op
	    		   && (
	    				   (exp_prev_first == exp_string_op  
	 		                && exp_next_first != exp_string_op
	 		                )
			           ||
			               (exp_prev_first == exp_create_set_op
			                && exp_next_first != ')'
			                )
			          )
			       ){
	 	    exp_stk_setc[tot_exp_stk_var - 1] = exp_stk_setc[tot_exp_stk_var - 1].concat(exp_token);
	 	    exp_var_last = true;
	    /*
	     * else get var or op from next token
	     */
	    } else {
    	    exp_set_next_token();
	    }
	}
}
private void exp_set_term_op(){
	/*
	 * set next token and op to exp_term_op
	 */
	    exp_token = "" + exp_term_op;
   	   	exp_next_first = exp_term_op;
   	   	exp_next_class = exp_class_term;
}
private void exp_set_next_token(){
	/*
	 * set op from exp_token
	 */
	 exp_next_op = exp_next_op.toUpperCase();
	 switch (exp_next_op.charAt(0)){
	       case '0':
	       case '1':
	       case '2':
	       case '3':
	       case '4':
	       case '5':
	       case '6':
	       case '7':
	       case '8':
	       case '9':
	        exp_push_sdt(exp_token);
	        exp_var_last   = true;
	        break;
	       case '+':
	       case '-':
	       	if (exp_var_count == 0 
	       		&& exp_prev_first != exp_string_op
	       		&& exp_prev_first != exp_create_set_op){
	       		exp_push_sdt("0");
	       	}
	       	exp_next_class = exp_class_add_sub;
	       	break;
	       case '*':
	       case '/':
	       	exp_next_class = exp_class_mpy_div;
	       	break;
	       case '(':
	       	exp_next_class = exp_class_open;
	       	break;
	       case ')':
	    	if (exp_level == 0){ 
	    		// not string, subscript, or create_set
                exp_set_term_op();
	        } else {
	     	    exp_next_class = exp_class_cls_sub;
	        }
	       	break;
	       case '.':
	       	exp_next_class = exp_class_str_op;
	       	break;
	       case '~':
	       case ' ':
	        if (exp_level == 0){ // not string or subscript
               exp_set_term_op();
	        } else {
	        	exp_var_last = true; // ignore (spaces)
	        }
	       	break;
	       case '\'':
	       	exp_next_class = exp_class_str_sub1;
	       	break;
	       case ',':
	        if (exp_level == 0){ // not string or subscript
                exp_set_term_op();
	        } else {
	       	    exp_next_class = exp_class_str_sub2;
	        }
	       	break;
	    	 case 'A':
	    	 	if (exp_next_op.equals("AND")){
	    	 		exp_next_class = exp_class_and;
	    	 	} else {
	    	 		push_sym();
	    	 		exp_var_last = true;
	    	 	}
	    	 	break;
	    	 case 'B':
	   	 	    if (exp_next_op.length() > 2 && exp_next_op.charAt(1) == '\''){
	   	            exp_push_sdt(exp_token);
	   	            exp_var_last = true;
	    	    } else {
	    	    	push_sym();
	    	    	exp_var_last = true;
	    	    }
	    	 	break;
	    	 case 'C':

	    	 	if (exp_next_op.length() > 2 && exp_next_op.charAt(1) == '\''){
	   	            exp_push_sdt(exp_token);
	   	            exp_var_last = true;
	    	 	} else {
	    	 		push_sym();
	    	 		exp_var_last = true;
	    	 	}
	    	 	break;
	    	 case 'D':
	    	 	if (exp_next_op.equals("DOUBLE")){
	    	 		exp_next_class = exp_class_oper;
	    	 	} else {
	    	 		push_sym();
	    	 		exp_var_last = true;
	    	 	}
	    	 	break;
	    	 case 'E':
	    	 	if (exp_next_op.equals("EQ")){
	    	 		exp_next_class = exp_class_comp;
	    	 	} else {
	    	 		push_sym();
	    	 		exp_var_last = true;
	    	 	}
	    	 	break;
	    	 case 'F':
	    	 	if (exp_next_op.equals("FIND")){
	    	 		exp_next_class = exp_class_str_op;
	    	 	} else {
	    	 		push_sym();
	    	 		exp_var_last = true;
	    	 	}
	    	 	break;
	    	 case 'G':
	    	 	if (exp_next_op.equals("GE")){
	    	 		exp_next_class = exp_class_comp;
	    	 	} else if (exp_next_op.equals("GT")){
	    	 		exp_next_class = exp_class_comp;
	    	 	} else {
	    	 		push_sym();
	    	 		exp_var_last = true;
	    	 	}
	    	 	break;
	    	 case 'I':
	    	 	if (exp_next_op.equals("INDEX")){
	    	 		exp_next_class = exp_class_str_op;
	    	 	} else {
	    	 		push_sym();
	    	 		exp_var_last = true;
	    	 	}
	    	 	break;
	    	 case 'K':
	    	 	if (exp_next_op.equals("K'")){
	    	 		exp_next_class = exp_class_oper;
	    	 	} else {
	    	 		push_sym();
	    	 		exp_var_last = true;
	    	 	}
	    	 	break;
	    	 case 'L':
	    	 	if (exp_next_op.equals("LE")){
	    	 		exp_next_class = exp_class_comp;
	    	 	} else if (exp_next_op.equals("LT")){
	    	 		exp_next_class = exp_class_comp;
	    	 	} else if (exp_next_op.equals("L'")){
	    	 		exp_next_class = exp_class_oper;
	    	 	} else {
	    	 		push_sym();
	    	 		exp_var_last = true;
	    	 	}
	    	 	break;
	    	 case 'N':
	    	 	if (exp_next_op.equals("NE")){
	    	 		exp_next_class = exp_class_comp;
	    	 	} else if (exp_next_op.equals("NOT")){
	    	 		exp_next_class = exp_class_not;
	    	 	} else if (exp_next_op.equals("N'")){
	    	 		exp_next_class = exp_class_oper;
	    	 	} else {
	    	 		push_sym();
	    	 		exp_var_last = true;
	    	 	}
	    	 	break;
	    	 case 'O':
	    	 	if (exp_next_op.equals("OR")){
	    	 		exp_next_class = exp_class_or;
	    	 	} else {
	    	 		push_sym();
	    	 		exp_var_last = true;
	    	 	}
	    	 	break;
	    	 case 'T':
	     	 	if (exp_next_op.equals("T'")){
	     	 		exp_next_class = exp_class_oper;
	    	 	} else {
	    	 		push_sym();
	    	 		exp_var_last = true;
	    	 	}
	     	 	break;
	    	 case 'X':
	    	 	if (exp_next_op.length() > 2 && exp_next_op.charAt(1) == '\''){
	    	 		exp_push_sdt(exp_token);
	    	 		exp_var_last = true;
	    	 	} else if (exp_next_op.equals("XOR")){
	    	 		exp_next_class = exp_class_xor;
	    	 	} else {
	    	 		push_sym();
	    	 		exp_var_last = true;
	    	 	}
	    	 	break;
	    	 case '$':
	    	 case '@':
	    	 case '#':
	    	 	push_sym();
	    	 	exp_var_last = true;
	    		break;
	    	 default:
	    	 	exp_next_class = 0;
	    	 break;
	 }
}
private void exp_perform_op(){
	/*
	 * perform next exp action based
	 * on precedence of exp_next_op and
	 * exp_prev_op
	 */
    if (opt_traceall){
	        put_log("TRACE EXP OPS=" + tot_exp_stk_op + " VARS=" + tot_exp_stk_var + " PREV OP = " + exp_prev_op +  " NEXT OP = " + exp_token);
    }
	exp_var_count = -1;  // reset var count for unary minus
	if  (exp_prev_class == 0){
		log_error(37,"invalid expression operator class for - " + exp_token);
	}
	if  (exp_next_class == 0){
		log_error(37,"invalid expression operator class - " + exp_token);
	}
    int action = exp_action[tot_classes*(exp_prev_class-1)+exp_next_class-1];
    if (opt_traceall){
    	put_log("TRACE EXP OPS=" + tot_exp_stk_op + " VARS=" + tot_exp_stk_var + " ACTION = " + action + " PREV CLASS = " + exp_prev_class + " NEXT CLASS = " + exp_next_class);
    }
    switch (action){
    case  1: // + or - add/sub
       exp_pop_op();
       if (exp_prev_first == '+'){
          exp_add();
       } else {
       	  exp_sub();
       }
	   break;
    case  2:  // * or / mpy/div
       exp_pop_op();
       if (exp_prev_first == '*'){
          exp_mpy();
       } else {
       	  exp_div();
       }
  	   break;
    case  3: // (..) 
  	   exp_push_op();
  	   if (exp_next_first == '('){
  	      exp_level++;
  	   }
  	   break;
    case  4: // )
  	   exp_pop_op();
  	   exp_level--;
  	   if (exp_level == 0 && bal_op.toUpperCase().equals("AIF")){
          exp_set_term_op();
     	  exp_term(); 
  	   } else {
   	      exp_check_prev_op = false;
  	   }
       break;
    case  5: // . concatentate
        exp_pop_op();
        if (exp_prev_op.equals(".")){
            exp_str_concat();
        } else if (exp_prev_op.equals("INDEX")){
        	exp_str_index();
        } else if (exp_prev_op.equals("FIND")){
        	exp_str_find(); 
        } else {
        	log_error(94,"invalid string operator - " + exp_prev_op);
        }
       break;
    case  6: // ~ terminator
  	   exp_term();
  	   break;
    case  7: // logical operator
       exp_pop_op();
       exp_compare();
       break;
    case  8: // '...'(s1,s2) start/end substring text
       /*
        * if prev op not ' then
        *    put ' op and null string on stacks
        * else
        *   if next op is not ( then
        *       remove ' op leaving setc string
        *   else 
        *       leave ' op and push , substring op
        */
       exp_string_quote();
       break;
    case  9: // ,e1, substring e1
       /* 
        * leave e1 on stack and proceed to get e2
        * and return to case 10 for substring calc
        * substring setc and e1 now on stack
        */
       break;
    case 10: // ,e2) substring e2
       /*
        * replace string with substring
        * and remove , op and e1,e2
        */
    	exp_substring();
    	break;
    case 11: // replace &var(sub) with value
    	exp_var_subscript();
    	exp_var_count = 0; // prevent unary minus
    	break;
    case 12: // prefix operator ?'
    	exp_pop_op();
    	switch (exp_prev_first){
    	case 'D': // DOUBLE
    		if (tot_exp_stk_var > 0){
    			setc_value = get_setc_stack_value().replaceAll(".","..");
    			put_setc_stack_var();
    		} else {
    			log_error(95,"missing variable for DOUBLE operator");
    		}
    		break;
    	case 'K': // K'var returns character count
    		if (tot_exp_stk_var > 0){
    			setc_value = get_setc_stack_value();
    			seta_value1 = setc_value.length();
    			put_seta_stack_var();
    		} else {
    			log_error(67,"missing variable for K' operator");
    		}
    		break;
    	case 'L': // L'sym returns length attribute
    		if (tot_exp_stk_var > 0){
    			setc_value = get_setc_stack_value();
    			seta_value1 = get_sym_len(setc_value);
    			put_seta_stack_var();
    		} else {
    			log_error(67,"missing variable for L' operator");
    		}
    		break;
    	case 'N': // N'var returns sublist count
    		if (tot_exp_stk_var > 0){
    			if (exp_stk_type[tot_exp_stk_var - 1] == var_sublist_type
    					&& exp_stk_setb[tot_exp_stk_var - 1] == syslist_loc
						&& exp_stk_seta[tot_exp_stk_var - 1] == -1
						&& mac_call_level > 0){
    				seta_value1 = tot_pos_parm - mac_call_pos_start[mac_call_level]-1;
    				tot_exp_stk_var--; // remove syslist var
    				put_seta_stack_var();
    			} else {
    				setc_value = get_setc_stack_value();
    				seta_value1 = get_sublist_count(setc_value);
    				put_seta_stack_var();
    			}
    			exp_var_count = 0; // prevent unary minus
    		} else {
    			log_error(67,"missing variable for N' operator");
    		}
    		break;
    	case 'T': // T'sym returns type attribute
    		if (tot_exp_stk_var > 0){
    			setc_value = get_setc_stack_value();
    			setc_value = "" + get_sym_type(setc_value);
    			put_setc_stack_var();
    		} else {
    			log_error(67,"missing variable for L' operator");
    		}
    	}
    	break;
    case 13: // NOT
    	exp_pop_op();
    	exp_not();
    	break;
    case 14: // AND
    	exp_pop_op();
    	exp_and();
    	break;
    case 15: // OR
    	exp_pop_op();
    	exp_or();
    	break;
    case 16: // XOR	
    	exp_pop_op();
    	exp_xor();
    	break;
    case 17: // process created set &(...)
    	exp_level--;  // reduce forced level by 1
    	exp_pop_op(); // remove & create op
    	exp_check_prev_op = false;
    	exp_set_prev_op();
    	if (tot_exp_stk_var > 0){
    		exp_token = '&' + get_setc_stack_value();
    		exp_push_var();
    	} else {
    		log_error(103,"missing variable for created set variable");
    	}
    	break;
    default:
	    log_error(38,"invalid sequence - prev=" + exp_prev_first + " next=" + exp_next_first);
    }
}
private void exp_add(){
	/* add top of stack value to prev. value
	 * and pop the top stack value off
	 */
	get_seta_stack_values();
	seta_value1 = seta_value1 + seta_value2;
	put_seta_stack_var();
}
private void exp_sub(){
	/* sub top of stack value from prev. value
	 * and pop the top stack value off
	 */
	get_seta_stack_values();
	seta_value1 = seta_value1 - seta_value2;
	put_seta_stack_var();
}
private void exp_mpy(){
	/* mpy top of stack value to prev. value
	 * and pop the top stack value off
	 */
	get_seta_stack_values();
	seta_value1 = seta_value1 * seta_value2;
	put_seta_stack_var();
}
private void exp_div(){
	/* div top of stack value into prev. value
	 * and pop the top stack value off
	 */
	get_seta_stack_values();
	if (seta_value2 != 0){
	    seta_value1 = seta_value1 / seta_value2;
	} else {
		seta_value1 = 0; // by definition for HLASM
	}
	put_seta_stack_var();
}
private void exp_str_concat(){
	/*
	 * concatenate two variables on stack
	 */
	get_setc_stack_values();
	setc_value1 = setc_value1.concat(setc_value2);
	tot_exp_stk_var++;
	exp_stk_type[tot_exp_stk_var - 1] = var_setc_type;
	exp_stk_setc[tot_exp_stk_var - 1] = setc_value1;
}
private void exp_str_index(){
	/*
	 * return seta index of first occurance of 
	 * second string within the first string
	 */
	get_setc_stack_values();
	int str1_len = setc_value1.length();
	int str2_len = setc_value2.length();
	seta_value1 = 0;
	if (str1_len > 0 
		&& str2_len > 0
		&& str1_len >= str2_len){
		boolean str_found = false;
		int index1 = 0;
		while (!str_found 
				&& index1 < str1_len - str2_len + 1){      
			if (setc_value1.substring(index1,index1+str2_len).equals(setc_value2)){
			    str_found = true;
			    seta_value1 = index1 + 1;
			}
			index1++;
		}
	}
	tot_exp_stk_var++;
	exp_stk_type[tot_exp_stk_var - 1] = var_seta_type;
	exp_stk_seta[tot_exp_stk_var - 1] = seta_value1;
}
private void exp_str_find(){
	/*
	 * return seta index of anyfirst character
	 * in str2 found in str1
	 */
	get_setc_stack_values();
	int str1_len = setc_value1.length();
	int str2_len = setc_value2.length();
	seta_value1 = 0;
	if (str1_len > 0 
		&& str2_len > 0){
		boolean str_found = false;
		int index1 = 0;
		while (!str_found 
				&& index1 < str1_len){
			int index2 = 0;
			while (!str_found
					&& index2 < str2_len){
				if (setc_value1.charAt(index1) 
					== setc_value2.charAt(index2)){
					str_found = true;
					seta_value1 = index1 + 1;
				}
				index2++;
			}
			index1++;
		}
	}
	tot_exp_stk_var++;
	exp_stk_type[tot_exp_stk_var - 1] = var_seta_type;
	exp_stk_seta[tot_exp_stk_var - 1] = seta_value1;

}
private void exp_compare(){
	/*
	 * perform compare EQ,GE,GT,LE,LT, or NE
	 */
	get_compare_stack_values();
	if  (exp_prev_op.equals("EQ")){
		switch (var_type1){
		   case 1:
		   	  if (seta_value1 == seta_value2){
		   	  	  set_compare(true);
		   	  } else {
		   	  	  set_compare(false);
		   	  }
		   	  break;
		   case 2:
		   	  if (setb_value1 == setb_value2){
		   	  	  set_compare(true);
		   	  } else {
		   	  	  set_compare(false);
		   	  }
		   	  break;
		   case 3:
		   	  if (setc_compare() == 0){
		   	  	  set_compare(true);
		   	  } else {
		   	  	  set_compare(false);
		   	  }
		   	  break;
		  default: 
			  abort_error(68,"invalid case index");
		}
	} else if  (exp_prev_op.equals("GE")){
		switch (var_type1){
		   case 1:
		   	  if (seta_value1 >= seta_value2){
		   	  	  set_compare(true);
		   	  } else {
		   	  	  set_compare(false);
		   	  }
		   	  break;
		   case 2:
		   	  if (setb_value1 >= setb_value2){
		   	  	  set_compare(true);
		   	  } else {
		   	  	  set_compare(false);
		   	  }
		   	  break;
		   case 3:
		   	  if (setc_compare() >= 0){
		   	  	  set_compare(true);
		   	  } else {
		   	  	  set_compare(false);
		   	  }
		   	  break;
		  default: 
			  	 abort_error(68,"invalid case index");
		}
	} else if  (exp_prev_op.equals("GT")){
		switch (var_type1){
		   case 1:
		   	  if (seta_value1 > seta_value2){
		   	  	  set_compare(true);
		   	  } else {
		   	  	  set_compare(false);
		   	  }
		   	  break;
		   case 2:
		   	  if (setb_value1 > setb_value2){
		   	  	  set_compare(true);
		   	  } else {
		   	  	  set_compare(false);
		   	  }
		   	  break;
		   case 3:
		   	  if (setc_compare() > 0){
		   	  	  set_compare(true);
		   	  } else {
		   	  	  set_compare(false);
		   	  }
		   	  break;
			default: 
			  	 abort_error(68,"invalid case index");
		}
	} else if  (exp_prev_op.equals("LE")){
		switch (var_type1){
		   case 1:
		   	  if (seta_value1 <= seta_value2){
		   	  	  set_compare(true);
		   	  } else {
		   	  	  set_compare(false);
		   	  }
		   	  break;
		   case 2:
		   	  if (setb_value1 <= setb_value2){
		   	  	  set_compare(true);
		   	  } else {
		   	  	  set_compare(false);
		   	  }
		   	  break;
		   case 3:
		   	  if (setc_compare() <= 0){
		   	  	  set_compare(true);
		   	  } else {
		   	  	  set_compare(false);
		   	  }
		   	  break;
			default: 
			  	 abort_error(68,"invalid case index");
		}
	} else if  (exp_prev_op.equals("LT")){
		switch (var_type1){
		   case 1:
		   	  if (seta_value1 < seta_value2){
		   	  	  set_compare(true);
		   	  } else {
		   	  	  set_compare(false);
		   	  }
		   	  break;
		   case 2:
		   	  if (setb_value1 < setb_value2){
		   	  	  set_compare(true);
		   	  } else {
		   	  	  set_compare(false);
		   	  }
		   	  break;
		   case 3:
		   	  if (setc_compare() < 0){
		   	  	  set_compare(true);
		   	  } else {
		   	  	  set_compare(false);
		   	  }
		   	  break;
			default: 
			  	 abort_error(68,"invalid case index");
		}
	} else if  (exp_prev_op.equals("NE")){
		switch (var_type1){
		   case 1:
		   	  if (seta_value1 != seta_value2){
		   	  	  set_compare(true);
		   	  } else {
		   	  	  set_compare(false);
		   	  }
		   	  break;
		   case 2:
		   	  if (setb_value1 != setb_value2){
		   	  	  set_compare(true);
		   	  } else {
		   	  	  set_compare(false);
		   	  }
		   	  break;
		   case 3:
		   	  if (setc_compare() != 0){
		   	  	  set_compare(true);
		   	  } else {
		   	  	  set_compare(false);
		   	  }
		   	  break;
			default: 
			  	 abort_error(68,"invalid case index");
		}
	}
}
private int setc_compare(){
	/*
	 * compare setc_value1 and setc_value2
	 * in EBCDIC and return -1, 0, or 1
	 * for low, equal, high
	 */
	int len1 = setc_value1.length();
	int len2 = setc_value2.length();
	int len_comp = len1;
	if (len1 > len2){
		len_comp = len2;
	}
	int index = 0;
	while (index < len_comp
			&& ascii_to_ebcdic[setc_value1.charAt(index)]
			== ascii_to_ebcdic[setc_value2.charAt(index)]){
		index++;
	}
	if (index < len_comp){
		if (ascii_to_ebcdic[setc_value1.charAt(index)]
            > ascii_to_ebcdic[setc_value2.charAt(index)]){
			return 1;
		} else {
			return -1;
		}
	} else {
		if (len1 == len2){
	        return 0;
		} else if (len1 > len2){
			return 1;
		} else {
			return -1;
		}
	}
}
private void exp_not(){
	/*
	 * perform logical not operation on stk var
	 */
	if (tot_exp_stk_var > 0){
	   switch (exp_stk_type[tot_exp_stk_var - 1]){
	   case 1: // not seta
	   	  exp_stk_seta[tot_exp_stk_var - 1] = ~ exp_stk_seta[tot_exp_stk_var - 1]; 
	   	  break;
	   case 2: // not setb
	      exp_stk_setb[tot_exp_stk_var - 1] = (byte) ~ exp_stk_setb[tot_exp_stk_var - 1];
	      break;
	   case 3: // not setc
	   	  seta_value = get_seta_stack_value(-1);
	   	  exp_stk_type[tot_exp_stk_var - 1] = var_seta_type;
	   	  exp_stk_seta[tot_exp_stk_var - 1] = ~ seta_value;
	      break;
	   }
	} else {
		log_error(78,"missing NOT operand");
	}
}
private void exp_and(){
	/*
	 * perform logical and operation on stk vars
	 */

	if (tot_exp_stk_var > 1){
	   seta_value1 = get_seta_stack_value(-2);
	   seta_value2 = get_seta_stack_value(-1);
	   switch (exp_stk_type[tot_exp_stk_var - 2]){
	   case 1: // and seta
	   	  exp_stk_seta[tot_exp_stk_var - 2] = seta_value1 & seta_value2; 
	      break;
	   case 2: // and setb
	      exp_stk_setb[tot_exp_stk_var - 2] = (byte) (seta_value1 & seta_value2);
	      break;
	   case 3: // and setc
          exp_stk_type[tot_exp_stk_var - 2] = var_setb_type;
          exp_stk_setb[tot_exp_stk_var - 2] = (byte) (seta_value1 & seta_value2);
	      break;
	   }
	   tot_exp_stk_var--;
	} else {
		log_error(79,"missing AND operand");
	}
}
private void exp_or(){
	/*
	 * perform logical or operation on stk vars
	 */
	if (tot_exp_stk_var > 0){
		seta_value1 = get_seta_stack_value(-2);
		seta_value2 = get_seta_stack_value(-1);
	    switch (exp_stk_type[tot_exp_stk_var - 2]){
	    case 1: // or seta
	   	  exp_stk_seta[tot_exp_stk_var - 2] = seta_value1 | seta_value2; 
	      break;
	    case 2: // or setb
	      exp_stk_setb[tot_exp_stk_var - 2] = (byte) (seta_value1 | seta_value2);
	      break;
	    case 3: // or setc
          exp_stk_type[tot_exp_stk_var - 2] = var_setb_type;
          exp_stk_setb[tot_exp_stk_var - 2] = (byte) (seta_value1 | seta_value2);
	      break;
	   }
		tot_exp_stk_var--;
	} else {
		log_error(80,"missing OR operand");
	}
}
private void exp_xor(){
	/*
	 * perform logical xor operation on stk vars
	 */

	if (tot_exp_stk_var > 0){
		seta_value1 = get_seta_stack_value(-2);
		seta_value2 = get_seta_stack_value(-1);
	    switch (exp_stk_type[tot_exp_stk_var - 2]){
	    case 1: // xor seta
	   	  exp_stk_seta[tot_exp_stk_var - 2] = seta_value1 ^ seta_value2; 
	      break;
	    case 2: // xor setb
	      exp_stk_setb[tot_exp_stk_var - 2] = (byte) (seta_value1 ^ seta_value2);
	      break;
	    case 3: // xor setc
          exp_stk_type[tot_exp_stk_var - 2] = var_setb_type;
          exp_stk_setb[tot_exp_stk_var - 2] = (byte) (seta_value1 ^ seta_value2);
	      break;
	   }
		tot_exp_stk_var--;
	} else {
		log_error(81,"missing XOR operand");
	}
}
private void exp_string_quote(){
	/*
	 * start or end string or substring
	 * defining setc value for exp_stack
	 */
	 if (exp_prev_first != exp_string_op){
        exp_level++;         // add substring extra level to handel spaces
        exp_push_op();       // push exp_string_op
	 	exp_push_string(""); // push empty string val
	 } else {   // we are in string mode
	 	if (exp_next_char() == exp_string_op){
	 		/* 
	 		 * add quote for each 2 inside string
	 		 */
	 		skip_next_token();
	 		exp_stk_setc[tot_exp_stk_var - 1] = exp_stk_setc[tot_exp_stk_var - 1].concat("" + exp_string_op);
	 	} else {
	 	    if (exp_next_char() == '('){
	 	    	skip_next_token(); // skip substring (
	 	    	exp_stk_op[tot_exp_stk_op - 1] = "" + exp_substring_op;  // replace ' with , substring oper
	 	    	exp_stk_op_class[tot_exp_stk_op - 1] = exp_class_str_sub2;
	 	    	exp_set_prev_op(); 
	 	    } else {
	 	    	exp_pop_op(); // remove ' string op
	 	    	exp_check_prev_op = false;  
	 	    	exp_level--; // no more spaces in '...'
	 	    }
	 	}
	 }
}
private void exp_substring(){
	/*
	 * replace string, e1, e2 values with substring
	 */
	if (tot_exp_stk_var >= 3 && tot_exp_stk_op >= 1
		&& exp_stk_type[tot_exp_stk_var - 3] == var_setc_type){
		exp_pop_op();  // remove , operator
		exp_check_prev_op = false;
	    exp_level--;   // remove substring extra level
	    get_seta_stack_values();
	    setc_len = exp_stk_setc[tot_exp_stk_var - 1].length();
        if (seta_value1 >= 0 && seta_value2 >= 0){
       	  if (seta_value1 <= setc_len && seta_value2 > 0){
       	  	 int e1 = seta_value1 - 1; 
       	  	 int e2 = e1 + seta_value2;
       	  	 if (e2 > setc_len)e2 = setc_len;
       	  	 exp_stk_setc[tot_exp_stk_var - 1] = exp_stk_setc[tot_exp_stk_var - 1].substring(e1,e2);
       	  } else {
       	  	 exp_stk_setc[tot_exp_stk_var - 1] = "";
       	  }
        } else {
       	  log_error(51,"invalid substring subscripts");
        }
	} else {
		log_error(52,"invalid substring expression");
	}	
}
private void exp_var_subscript(){
	/*
	 * called with var ptr and subscript on stack
	 * 
	 * 1. if subscripted set var
	 *       replace stack var set ptr
	 *       with subscripted set value
	 * 2. if subscripted parm var
	 *       update or replace stack var parm ptr
	 *       with sublist parm value
	 * 3.  skip trailing . if any
	 */
	if (tot_exp_stk_var >= 2){ 
	  if (exp_stk_type[tot_exp_stk_var - 2] == var_subscript_type){
		set_sub = get_seta_stack_value(-1);
		tot_exp_stk_var--;
		var_name_index = exp_stk_seta[tot_exp_stk_var - 1];
		var_loc = exp_stk_setb[tot_exp_stk_var - 1];
		switch (var_loc){
		case 11: // lcl set var(sub)
			   var_type = lcl_set_type[var_name_index];
               get_lcl_set_value();
               switch (var_type){
               case 1: 
           	       exp_stk_type[tot_exp_stk_var - 1] = var_seta_type;
           	       exp_stk_seta[tot_exp_stk_var - 1] = lcl_seta[seta_index];
                   break;
               case 2: 
        	       exp_stk_type[tot_exp_stk_var - 1] = var_setb_type;
        	       exp_stk_setb[tot_exp_stk_var - 1] = lcl_setb[setb_index];
                   break;
               case 3: 
        	       exp_stk_type[tot_exp_stk_var - 1] = var_setc_type;
        	       exp_stk_setc[tot_exp_stk_var - 1] = lcl_setc[setc_index];
		           break;
     		   default: 
   		  	       abort_error(68,"invalid case index");
               }
               exp_level--;
	           exp_pop_op();
	           exp_check_prev_op = false;
	   		   if (tot_exp_stk_op >= 1
					&& (exp_stk_op[tot_exp_stk_op - 1].charAt(0) == exp_string_op
					    || exp_stk_op[tot_exp_stk_op - 1].charAt(0) == exp_create_set_op)){
	              exp_append_string();
	   		   }
	           if (exp_next_char() == '.'){
	              skip_next_token();  // skip trailing . in string substitution
	           }
               break;
		case 12: // gbl set var(sub)
			   var_type = gbl_set_type[var_name_index];
	           get_gbl_set_value();
	           switch (var_type){
	           case 1: 
	           	   exp_stk_type[tot_exp_stk_var - 1] = var_seta_type;
	           	   exp_stk_seta[tot_exp_stk_var - 1] = gbl_seta[seta_index];
	               break;
	           case 2: 
	        	   exp_stk_type[tot_exp_stk_var - 1] = var_setb_type;
	        	   exp_stk_setb[tot_exp_stk_var - 1] = gbl_setb[setb_index];
	               break;
	           case 3: 
	        	   exp_stk_type[tot_exp_stk_var - 1] = var_setc_type;
	        	   exp_stk_setc[tot_exp_stk_var - 1] = gbl_setc[setc_index];
			       break;
	 		   default: 
			  	 abort_error(68,"invalid case index");
		       }
	           exp_level--;
	           exp_pop_op();
	           exp_check_prev_op = false;
	   		   if (tot_exp_stk_op >= 1
					&& (exp_stk_op[tot_exp_stk_op - 1].charAt(0) == exp_string_op
						|| exp_stk_op[tot_exp_stk_op - 1].charAt(0) == exp_create_set_op)){
	              exp_append_string();
	   		   }
	           if (exp_next_char() == '.'){
	              skip_next_token();  // skip trailing . in string substitution
	           }
	           break;
	    case 13: // pos parm var(sub) or var(sub,
	    case 14: // kw  parm var(sub) or var(sub,
			   setc_value = exp_stk_setc[tot_exp_stk_var - 1]; // get parm value set by find_var
			   setc_value = get_sublist(setc_value,set_sub);
			   if  (exp_next_first == ')'){
			   	   exp_level--;
			   	   exp_pop_op();
			   	   exp_check_prev_op = false;
			   	   exp_stk_type[tot_exp_stk_var - 1] = var_setc_type;
			   	   exp_stk_setc[tot_exp_stk_var - 1] = setc_value;
		   		   if (tot_exp_stk_op >= 1
						&& (exp_stk_op[tot_exp_stk_op - 1].charAt(0) == exp_string_op
							|| exp_stk_op[tot_exp_stk_op - 1].charAt(0) == exp_create_set_op)){
		              exp_append_string();
		   		   }
			   } else {
			   	   exp_stk_setc[tot_exp_stk_var - 1] = setc_value; // update sublist string for next index
			   }
	    	   break;
	    case 15: // syslist(sub) or syslist(sub,
	    	   if (var_name_index == -1){
	    	   	  if (mac_call_level > 0 && set_sub >= 0){
	    	   	  	 var_name_index = mac_call_pos_start[mac_call_level] + set_sub;
	    	   	  	 if (set_sub >= 0 && var_name_index < tot_pos_parm){
	    	   	  	 	setc_value = mac_call_pos_parm[var_name_index];
	    	   	  	 } else {
	    	   	  	 	setc_value = "";
	    	   	  	 }
	    	   	  } else {
	    	   	  	 log_error(66,"syslist reference only allowed in macro");
	    	   	  }
	    	   } else {
				   setc_value = exp_stk_setc[tot_exp_stk_var - 1]; // get prev syslist(sub) value
				   setc_value = get_sublist(setc_value,set_sub);
	    	   }
			   if  (exp_next_first == ')'){
			   	   exp_level--;
			   	   exp_pop_op();
			   	   exp_check_prev_op = false;
			   	   exp_stk_type[tot_exp_stk_var - 1] = var_setc_type;
			   	   exp_stk_setc[tot_exp_stk_var - 1] = setc_value;
		   		   if (tot_exp_stk_op >= 1
						&& (exp_stk_op[tot_exp_stk_op - 1].charAt(0) == exp_string_op
							|| exp_stk_op[tot_exp_stk_op - 1].charAt(0) == exp_create_set_op)){
		              exp_append_string();
		   		   }
			   } else {
			   	   exp_stk_seta[tot_exp_stk_var - 1] = var_name_index;
			   	   exp_stk_setc[tot_exp_stk_var - 1] = setc_value;
			   }
	    	   break;
		}
	  } else if (exp_stk_type[tot_exp_stk_var - 2] == var_sublist_type){
		   set_sub = get_seta_stack_value(-1);
		   tot_exp_stk_var--;
		   setc_value = exp_stk_setc[tot_exp_stk_var - 1]; // get parm value set by find_var
		   setc_value = get_sublist(setc_value,set_sub);
		   if  (exp_next_first == ')'){
		   	   exp_level--;
		   	   exp_pop_op();
		   	   exp_stk_type[tot_exp_stk_var - 1] = var_setc_type;
		   	   exp_stk_setc[tot_exp_stk_var - 1] = setc_value;
	   		   if (tot_exp_stk_op >= 1
					&& (exp_stk_op[tot_exp_stk_op - 1].charAt(0) == exp_string_op
						|| exp_stk_op[tot_exp_stk_op - 1].charAt(0) == exp_create_set_op)){
	              exp_append_string();
	   		   }
		   } else {
		   	   exp_stk_setc[tot_exp_stk_var - 1] = setc_value; // update sublist string for next index
		   }
	  }
	} else {
		log_error(54,"invalid subscripted variable");
	}
}
private void exp_append_string(){
	/*
	 * append var on top of stack to string var
	 */
	switch (exp_stk_type[tot_exp_stk_var - 1]){
    case 1: 
    	   exp_stk_setc[tot_exp_stk_var - 2] = exp_stk_setc[tot_exp_stk_var - 2].concat("" + exp_stk_seta[tot_exp_stk_var - 1]);
        break;
    case 2: 
    	   exp_stk_setc[tot_exp_stk_var - 2] = exp_stk_setc[tot_exp_stk_var - 2].concat("" + exp_stk_setb[tot_exp_stk_var - 1]);
        break;
    case 3: 
    	   exp_stk_setc[tot_exp_stk_var - 2] = exp_stk_setc[tot_exp_stk_var - 2].concat(exp_stk_setc[tot_exp_stk_var - 1]);
	       break;
	default: 
	  	 abort_error(68,"invalid case index");
    }
    tot_exp_stk_var--;
}
private char exp_next_char(){
	/* 
	 * return next char in expression else terminator
	 * 
	 */
	 if (exp_next_index < exp_text_len){
	 	return exp_text.charAt(exp_next_index);
	 } else {
	 	return '~';
	 }
}
private void skip_next_token(){
	/*
	 * skip next token
	 */
	   if (exp_match.find()){
          exp_token = exp_match.group();
          exp_next_index = exp_start_index + exp_match.end();
	   }
}
private void set_compare(boolean compare_result){
	/*
     * add true or false setb to stack
	 */
	if (tot_exp_stk_var < max_exp_stk){
		tot_exp_stk_var++;
        exp_stk_type[tot_exp_stk_var - 1] = var_setb_type;
        if (compare_result){
		   exp_stk_setb[tot_exp_stk_var - 1] = 1;
        } else {
           exp_stk_setb[tot_exp_stk_var - 1] = 0;
        }
	} else {
		log_error(36,"expression compare error");
	}
}
private void get_compare_stack_values(){
    /*
     * get set values from top of stack
     * and set compare_type as follows:
     * 1.  If either is setb, make setb
     * 2.  If either is seta, make seta
     * 3.  else setc
     */
	if (tot_exp_stk_var >= 2){
		var_type1 = exp_stk_type[tot_exp_stk_var - 2];
		var_type2 = exp_stk_type[tot_exp_stk_var - 1];
		switch (var_type1){
	        case 1: // seta
	        	if (var_type2 == var_setb_type){
	 	        	get_setb_stack_values();
	        	} else {
	        		get_seta_stack_values();
	        	}
 			    break;
		    case 2: // setb
        		get_setb_stack_values();
			    break;
		    case 3: // setc
	        	if (var_type2 == var_setb_type){
	        		get_setb_stack_values();
	        	} else if (var_type2 == var_seta_type){
	        		get_seta_stack_values();
	        	} else {
	        	    get_setc_stack_values();
	        	}
		    	break;
			default: 
			  	 abort_error(68,"invalid case index");
		}
	} else {
		log_error(63,"expression compare error");
	}
}
private void get_seta_stack_values(){
	/*
	 * get seta_value1 & 2 from top of stack
	 * and remove from stack
	 */
	var_type1 = 1;
	var_type2 = 1;
	if (tot_exp_stk_var >= 2){
		seta_value1 = get_seta_stack_value(-2);
        seta_value2 = get_seta_stack_value(-1);
	} else {
		log_error(18,"expression missing value error");
		tot_exp_stk_var = 2;
	}
	tot_exp_stk_var = tot_exp_stk_var - 2;
}
private int get_seta_stack_value(int offset){
	/*
	 * return seta value of stk + offset
	 */
	switch (exp_stk_type[tot_exp_stk_var + offset]){
    case 1:
		return exp_stk_seta[tot_exp_stk_var + offset];
    case 2:
	    return exp_stk_setb[tot_exp_stk_var + offset];
    case 3:
    	return get_int_from_string(exp_stk_setc[tot_exp_stk_var + offset],10);
    default:
    	log_error(53,"expression type error");
	}
	return 0;
}
private void put_seta_stack_var(){
	/*
     * add seta_value1 to stack 
	 */
	if (tot_exp_stk_var < max_exp_stk){
		tot_exp_stk_var++;
        exp_stk_type[tot_exp_stk_var - 1] = var_seta_type;
		exp_stk_seta[tot_exp_stk_var - 1] = seta_value1;
	} else {
		log_error(19,"seta expression stack overflow error");
	}
}
private void put_setc_stack_var(){
	/*
     * add setc_value to stack 
	 */
	if (tot_exp_stk_var < max_exp_stk){
		tot_exp_stk_var++;
        exp_stk_type[tot_exp_stk_var - 1] = var_setc_type;
		exp_stk_setc[tot_exp_stk_var - 1] = setc_value;
	} else {
		log_error(96,"setc expression stack overflow error");
	}
}

private void get_setb_stack_values(){
	/*
	 * set setb_value1 & 2 from top of stack
	 */
	var_type1 = 2;
	var_type2 = 2;
	if (tot_exp_stk_var >= 2){
		switch (exp_stk_type[tot_exp_stk_var - 2]){
		    case 1:
 			    setb_value1 = (byte) exp_stk_seta[tot_exp_stk_var - 2];
 			    break;
		    case 2:
			    setb_value1 = exp_stk_setb[tot_exp_stk_var - 2];
			    break;
		    case 3:
		    	setb_value1 = (byte) get_int_from_string(exp_stk_setc[tot_exp_stk_var - 2],10);
		    	break;
			default: 
			  	 abort_error(68,"invalid case index");
		}
		switch (exp_stk_type[tot_exp_stk_var - 1]){
	    case 1:
			    setb_value2 = (byte) exp_stk_seta[tot_exp_stk_var - 1];
			    break;
	    case 2:
		    setb_value2 = exp_stk_setb[tot_exp_stk_var - 1];
		    break;
	    case 3:
	    	setb_value2 = (byte) get_int_from_string(exp_stk_setc[tot_exp_stk_var - 1],10);
	    	break;
		default: 
		  	 abort_error(68,"invalid case index");
	    }
		tot_exp_stk_var = tot_exp_stk_var - 2;
	} else {
		log_error(18,"expression error");
	}
}
private void get_setc_stack_values(){
	/*
	 * set setc_value1 & 2 from top of stack
	 * without removing
	 */
	var_type2 = var_setc_type;
	setc_value2 = get_setc_stack_value();
	var_type1 = var_setc_type;
	setc_value1 = get_setc_stack_value();
}
private String get_setc_stack_value(){
	/*
	 * return setc string from top of stack
	 * without removing
	 */
	if (tot_exp_stk_var >= 1){
		tot_exp_stk_var--;
		switch (exp_stk_type[tot_exp_stk_var]){
		    case 1:
 			    return "" + exp_stk_seta[tot_exp_stk_var];
		    case 2:
			    return "" + exp_stk_setb[tot_exp_stk_var];
		    case 3:
		    	return exp_stk_setc[tot_exp_stk_var];
			default: 
			  	 abort_error(68,"invalid case index");
		}
	}
	log_error(20,"expression error");
	return "";
}
private void exp_push_op(){
	/*
	 * put op on stack
	 * 
	 */
	  if (tot_exp_stk_op >= max_exp_stk){
		  abort_error(44,"maximum stack operations exceeded");
		  return;
	  }
      exp_stk_op[tot_exp_stk_op] = exp_token.toUpperCase();
      exp_stk_op_class[tot_exp_stk_op] = exp_next_class;
      tot_exp_stk_op++;
      exp_set_prev_op();
}
private void exp_pop_op(){
	/*
	 * pop current op on stack
	 */
      tot_exp_stk_op--;
      if (tot_exp_stk_op < 0){
      	 log_error(23,"expression error");
      } else {
    	  exp_check_prev_op = true;
      }
}
private void exp_term(){
	/*
	 * terminate expression returning
	 * value on stack if no errors
	 * Note:
	 *   1.  Don't return value if parse_mode
	 */
	if (exp_parse_set_mode){
	      exp_end = true;
	      exp_ok  = true;
	      return;
	}
	if (tot_exp_stk_var == 1 && tot_exp_stk_op == 0){
      switch (exp_type){
          case 1:
          	switch (exp_stk_type[0]){
          	    case 1:
          	    	exp_seta = exp_stk_seta[0];
          	    	break;
          	    case 2:
          	    	exp_seta = exp_stk_setb[0];
          	    	break;
          	    case 3:
          	    	exp_seta = get_int_from_string(exp_stk_setc[0],10);
          	    	break;
      		    default: 
   		  	        abort_error(68,"invalid case index");
          	}
          	break;
          case 2:
          	switch (exp_stk_type[0]){
          	    case 1:
          	    	exp_setb = (byte) exp_stk_seta[0];
          	    	break;
          	    case 2:
          	    	exp_setb = exp_stk_setb[0];
          	    	break;
          	    case 3:
          	    	exp_setb = (byte) get_int_from_string(exp_stk_setc[0],10);
          	    	break;
      		    default: 
   		  	        abort_error(68,"invalid case index");
          	}
          	break;
          case 3:
          	switch (exp_stk_type[0]){
          	    case 1:
          	    	exp_setc = "" + exp_stk_seta[0];
          	    	break;
          	    case 2:
          	    	exp_setc = "" + exp_stk_setb[0];
          	    	break;
          	    case 3:
          	    	exp_setc = exp_stk_setc[0];
          	    	break;
      		    default: 
   		  	        abort_error(68,"invalid case index");
          	}
          	break;
      }
      exp_end = true;
      exp_ok  = true;
	} else {
	  log_error(35,"expression error");
	}
}
private int get_int_from_string(String number,int base){
	/*
	 * return integer from string using specified base
	 * Notes:
	 *   1.  If string has no digits, return 0 default
	 */
	try {
		return Integer.valueOf(number,base).intValue();
	} catch (Exception e) {
	    return 0;
	}
}
private void exp_push_var(){
	/*
	 * push set variable on stack
	 * 
	 * if &var followed by ( then
	 *    put var pointer on value stack
	 *    and put ) subscript op on op stack
	 * else
	 *    push unscripted var value on value stack
	 *    and skip trailing . if any 
	 * 
	 */
	if (tot_exp_stk_var >= max_exp_stk){
		abort_error(42,"maximum stack variables exceeded");
	}
    if (find_var(exp_token)){  // find set or parm var
       if (exp_next_char() == '('){
       	  if (var_type == var_parm_type){
              exp_stk_type[tot_exp_stk_var] = var_sublist_type;
          } else {
              exp_stk_type[tot_exp_stk_var] = var_subscript_type;
          }
          skip_next_token();
          exp_token = "" + exp_subscript_op;
          exp_next_first = exp_subscript_op;
          exp_next_class = exp_class_cls_sub;
          exp_push_op();  // push ) subscript/sublist op
     	  exp_level++;
          exp_stk_setb[tot_exp_stk_var] = var_loc;         // set/parm loc
      	  exp_stk_seta[tot_exp_stk_var] = var_name_index;  // set/sub subscript
      	  exp_stk_setc[tot_exp_stk_var] = setc_value;      // sublist parm
          tot_exp_stk_var++;
       } else {
       	  if (exp_prev_first == exp_string_op
       		  || exp_prev_first == exp_create_set_op){
	          switch (var_type){
	          case 1: // seta
	   	          exp_stk_setc[tot_exp_stk_var - 1] = exp_stk_setc[tot_exp_stk_var - 1].concat("" + seta_value);
	   	          break;
	          case 2: // setb
	   	          exp_stk_setc[tot_exp_stk_var - 1] = exp_stk_setc[tot_exp_stk_var - 1].concat("" + setb_value);
	   	          break;
	          case 3: // setc
	   	          exp_stk_setc[tot_exp_stk_var - 1] = exp_stk_setc[tot_exp_stk_var - 1].concat(setc_value);
	   	          break;
	   	      case 4: // positional or key word parm
	   	          exp_stk_setc[tot_exp_stk_var - 1] = exp_stk_setc[tot_exp_stk_var - 1].concat(setc_value);
	   	          break;
			  default: 
			  	 abort_error(68,"invalid case index");
	          } 
       	  } else { 
	          switch (var_type){
	          case 1:
	   	          exp_stk_seta[tot_exp_stk_var] = seta_value;
	   	          break;
	          case 2:
	   	          exp_stk_setb[tot_exp_stk_var] = setb_value;
	   	          break;
	   	      case 4:  // push parm as setc string
	   	      	  var_type = var_setc_type;
	          case 3:  // push setc for parm and setc
	   	          exp_stk_setc[tot_exp_stk_var] = setc_value;
	   	          break;
	   	      case 6:  // syslist
	   	      	  exp_stk_setb[tot_exp_stk_var] = var_loc;
	   	      	  exp_stk_seta[tot_exp_stk_var] = var_name_index;
	   	      	  break;
			  default: 
			  	 abort_error(68,"invalid case index");
	          }
	          exp_stk_type[tot_exp_stk_var] = var_type;
	   	      tot_exp_stk_var++;
       	  }
       	  if (exp_next_char() == '.'){
       	  	 skip_next_token();  // skip trailing .
       	  }
	   }
    } else {
    	if (exp_parse_set_mode){
		    exp_end = true;
    	} else {
    	    log_error(24,"undefined macro variable - " + exp_token);
    	}
    }
}
private void exp_push_sdt(String sdt){
	/*
	 * push self defining abs term on stack
	 * Note:
	 *   1.  Ordinary symbols are pushed as strings
	 *       for use by prefix operators T', L'.
	 */
	   if (tot_exp_stk_var >= max_exp_stk){
		   abort_error(42,"maximum stack variables exceeded");
	   } else if (sdt.length() >1 && sdt.charAt(1) == '\''
		          || (sdt.charAt(0) <= '9'
		        	  && sdt.charAt(0) >= '0')){
           exp_stk_type[tot_exp_stk_var] = var_seta_type;
           switch (sdt.substring(0,1).toUpperCase().charAt(0)){
           case 'B': // B'11000001' binary
        	   exp_stk_seta[tot_exp_stk_var] = get_int_from_string(sdt.substring(2,sdt.length()-1),2);
           	   break;
           case 'C': // C'A' EBCDIC character
           	   int index = 2;
           	   int sdt_int = 0;
           	   while (index < sdt.length()-1){
           	   	   sdt_int = (sdt_int << 8) + (ascii_to_ebcdic[sdt.charAt(index)] & 0xff);
           	       index++;
           	   }
           	   exp_stk_seta[tot_exp_stk_var] = sdt_int; 
           	   break;
           case 'X': // X'C1' hex
           	   exp_stk_seta[tot_exp_stk_var] = Long.valueOf(sdt.substring(2,sdt.length()-1),16).intValue();
           	   break;
           default:  // must be ascii number
               exp_stk_seta[tot_exp_stk_var] = get_int_from_string(sdt,10);
               break;
           }
	       tot_exp_stk_var++;
	   } else {  // push ordinary symbol for T',L'
           exp_stk_type[tot_exp_stk_var] = var_setc_type;
           exp_stk_setc[tot_exp_stk_var] = sdt;
	   }
}
private void push_sym(){
	/*
	 * push current exp_token symbol  on stack
	 * as setc for use by prefix operators T', L'
	 */
	exp_push_string(exp_token);
}
private void add_sym(String sym_lab,String sym_op,String sym_parms){
	/*
	 * add ordinary symbol type and length
	 * 
	 * Notes:
	 *   1.  If no type found, ignore symbol.
	 */
	 int parm_len = sym_parms.length();
     int parm_index = 0;
     int parm_level = 0;
     char parm_char = ' ';
	 if (parm_len > 0 
    	 && (sym_op.toUpperCase().equals("DC")
   		 || sym_op.toUpperCase().equals("DS"))){
         cur_sym = find_key_index("S:" + sym_lab);
         if (cur_sym == -1
        	&& tot_sym < max_sym){
        	while (parm_index < parm_len){
        		 parm_char = sym_parms.charAt(parm_index);
     		     if (parm_char == '('){
   			         parm_level++;
        	     } else if (parm_char == ')'){
   			         parm_level--;
        	     } else if (parm_level == 0 
        			 && (parm_char < '0'
        			     || parm_char > '9')){
        			  cur_sym = tot_sym;
        			  tot_sym++;
        			  add_key_index(cur_sym);
                      sym_type[cur_sym] = parm_char;
        			  if (parm_index < parm_len -2
        			      && sym_parms.charAt(parm_index +1) == 'L'){
        				  int temp_len = 0;
        				  parm_index = parm_index +2;
        				  while (parm_index < parm_len){
        					  parm_char = sym_parms.charAt(parm_index);
        					  if (parm_char >= '0' && parm_char <= '9'){
        						  temp_len = temp_len * 10 + parm_char - 0x30;
        					  }
        					  parm_index++;
        				  }
        				  sym_len[cur_sym] = temp_len;
        			  } else {
        				  parm_index = dc_valid_types.indexOf(sym_type[cur_sym]);
        				  if (parm_index != -1){
        					  sym_len[cur_sym] = dc_type_len[parm_index];
        				  } else {
        					  sym_type[cur_sym] = 'U';
        					  sym_len[cur_sym] = 1;
        				  }
        			  }
        			  return;
        		  }
        		  parm_index++;	  
        	}
         }
	 }
     }
private char get_sym_type(String symbol){
	/*
	 * return type for ordinary symbol if found
	 * else return 'U'
	 */
	int cur_sym = find_key_index("S:" + symbol);
	if (cur_sym != -1){
		return sym_type[cur_sym];
	} else {
		return 'U';
	}
}private int get_sym_len(String symbol){
	/*
	 * return length for ordinary symbol if found
	 * else return 1
	 */
	int cur_sym = find_key_index("S:" + symbol);
	if (cur_sym != -1){
		return sym_len[cur_sym];
	} else {
		return 1;
	}
}
private void exp_push_string(String value){
	/*
	 * push string on stack as setc
	 */
	   if (tot_exp_stk_var >= max_exp_stk){
		   abort_error(42,"maximum stack variables exceeded");
	   }
       exp_stk_type[tot_exp_stk_var] = var_setc_type;
       exp_stk_setc[tot_exp_stk_var] = value;
	   tot_exp_stk_var++;
}
private void add_lcl_set(String new_name,byte new_type,int new_size){
	/*
	 * add lcl set variable not found by find_set
	 * 
	 */
	if (tot_lcl_name >= max_lcl_name){
		abort_error(43,"maximum local variables exceeded");
		return;
	}
	var_name_index = tot_lcl_name;
	add_lcl_key_index(tot_lcl_name);
   	tot_lcl_name++;
	var_loc   = lcl_loc;
	var_type  = new_type;
	set_name  = new_name.toUpperCase();
	lcl_set_name[var_name_index] = set_name;
	lcl_set_type[var_name_index] = var_type;
	switch (var_type){
	   case 1:  // lcl seta
	   	  if (tot_lcl_seta >= max_lcl_set + new_size){
	   	  	 abort_error(44,"maximum local seta exceeded");
	   	  	 return;
	   	  }
	   	  lcl_set_start[var_name_index] = tot_lcl_seta;
	   	  seta_index = tot_lcl_seta;
	   	  tot_lcl_seta = tot_lcl_seta + new_size;
	   	  lcl_set_end[var_name_index]   = tot_lcl_seta;
	   	  seta_value = 0;
	   	  while (seta_index < tot_lcl_seta){
	   	        lcl_seta[seta_index] = seta_value;
	   	        seta_index++;
	   	  }
	   	  seta_index = lcl_set_start[var_name_index];
	   	  break;
	   case 2:  // lcl setb
	   	  if (tot_lcl_setb >= max_lcl_set + new_size){
	   	  	 abort_error(45,"maximum local setb exceeded");
	   	  	 return;
	   	  }
	   	  lcl_set_start[var_name_index] = tot_lcl_setb;
	   	  setb_index = tot_lcl_setb;
	   	  tot_lcl_setb = tot_lcl_setb + new_size;
	   	  lcl_set_end[var_name_index]   = tot_lcl_setb;
	   	  setb_value = 0;
	   	  while (setb_index <tot_lcl_setb){
	   	        lcl_setb[setb_index] = setb_value;
	   	        setb_index++;
	   	  }
	   	  setb_index = lcl_set_start[var_name_index];
	   	  break;
	   case 3:  // lcl setc
	   	  if (tot_lcl_setc >= max_lcl_set + new_size){
	   	  	 abort_error(46,"maximum local setc exceeded");
	   	  	 return;
	   	  }
	   	  lcl_set_start[var_name_index] = tot_lcl_setc;
	   	  setc_index = tot_lcl_setc;
	   	  tot_lcl_setc = tot_lcl_setc + new_size;
	   	  lcl_set_end[var_name_index]   = tot_lcl_setc;
	   	  setc_value = "";
	   	  while (setc_index <tot_lcl_seta){
	   	        lcl_setc[setc_index] = setc_value;
	   	        setc_index++;
	   	  }
	   	  setc_index = lcl_set_start[var_name_index];
	   	  break;
	default: 
		  abort_error(68,"invalid case index");
	}
}
private void add_gbl_set(String new_name,byte new_type,int new_size){
	/*
	 * add gbl set variable not found by find_set
	 * 
	 */
	if (tot_gbl_name >= max_gbl_name){
		abort_error(55,"maximum global variables exceeded");
		return;
	}
	var_name_index = tot_gbl_name;
	add_key_index(var_name_index);
   	tot_gbl_name++;
	var_loc   = gbl_loc;
	var_type  = new_type;
	set_name  = new_name.toUpperCase();
	gbl_set_name[var_name_index] = set_name;
	gbl_set_type[var_name_index] = var_type;
	switch (var_type){
	   case 1:  // gbl seta
	   	  if (tot_gbl_seta + new_size >= max_gbl_set){
	   	  	 abort_error(56,"maximum global seta exceeded");
	   	  	 return;
	   	  }
	   	  gbl_set_start[var_name_index] = tot_gbl_seta;
	   	  seta_index = tot_gbl_seta;
	   	  tot_gbl_seta = tot_gbl_seta + new_size;
	   	  gbl_set_end[var_name_index]   = tot_gbl_seta;
	   	  seta_value = 0;
	   	  while (seta_index < tot_gbl_seta){
	   	        gbl_seta[seta_index] = seta_value;
	   	        seta_index++;
	   	  }
	   	  seta_index = gbl_set_start[var_name_index];
	   	  break;
	   case 2:  // gbl setb
	   	  if (tot_gbl_setb + new_size >= max_gbl_set){
	   	  	 abort_error(57,"maximum global setb exceeded");
	   	  	 return;
	   	  }
	   	  gbl_set_start[var_name_index] = tot_gbl_setb;
	   	  setb_index = tot_gbl_setb;
	   	  tot_gbl_setb = tot_gbl_setb + new_size;
	   	  gbl_set_end[var_name_index]   = tot_gbl_setb;
	   	  setb_value = 0;
	   	  while (setb_index <tot_gbl_setb){
	   	        gbl_setb[setb_index] = setb_value;
	   	        setb_index++;
	   	  }
	   	  setb_index = gbl_set_start[var_name_index];
	   	  break;
	   case 3:  // gbl setc
	   	  if (tot_gbl_setc + new_size >= max_gbl_set){
	   	  	 abort_error(58,"maximum global setc exceeded");
	   	  	 return;
	   	  }
	   	  gbl_set_start[var_name_index] = tot_gbl_setc;
	   	  setc_index = tot_gbl_setc;
	   	  tot_gbl_setc = tot_gbl_setc + new_size;
	   	  gbl_set_end[var_name_index]   = tot_gbl_setc;
	   	  setc_value = "";
	   	  while (setc_index <tot_gbl_setc){
	   	        gbl_setc[setc_index] = setc_value;
	   	        setc_index++;
	   	  }
	   	  setc_index = gbl_set_start[var_name_index];
	   	  break;
	default: 
	  	 abort_error(68,"invalid case index");
	}
}
private boolean parse_set_var(String text,int text_index){
	/*
	 * parse scalar subscripted or created set
	 * variable with or without subscript using
	 * expression parser in parse_set_var_mode
	 * and return true if it exists or false if not.
	 * Notes:
	 *  1. find_var also sets:
	 *     a. set_name
	 *     b. set_sub
	 *  2. If var found but exp_parse_set_name
	 *     is null, then issue error for parms
	 * 
	 */
	exp_parse_set_mode = true;
	exp_parse_set_name = null;
	exp_parse_set_sub  = 0;
    calc_exp(text,text_index);
    if (exp_ok){
    	if (exp_parse_set_name == null){	
    		log_error(104,"set/parm variable conflict - " + text.substring(text_index));
    	}
    }
    return exp_ok;
}
private boolean find_set(String var_name,int var_sub){
	/*
	 * find lcl or gbl set variable else false
	 * and set var_name_index = -1 if not found.
	 * set following globals if found
	 * 1.  var_loc   = lcl_loc or gbl_loc
	 * 2.  var_type  = var_seta_type|var_setb_type|var_setc_type
	 * 3.  var_name_index = for lcl/gbl seta, setb, setc array 
	 * 4.  set_sub  = set variable subscript
	 * 5.  seta_value|setb_value|setc_value
	 * 6.  seta_index|setb_index|setc_index 
     *
     * Notes:
     *  1.  Saves create set name for possible
     *      scalar allocation for set.
     */
	if (exp_parse_set_mode 
		&& exp_level == 0){
		exp_parse_set_name = var_name;
		exp_parse_set_sub  = var_sub;
	}
	if (find_lcl_set(var_name,var_sub)){
		return true;
	}
	if (find_gbl_set(var_name,var_sub)){
		return true;
	}
	var_name_index = -1;
	return false;
}
private boolean find_lcl_set(String var_name,int var_sub){
	/*
	 * find lcl set variable else false
	 * also set var_name_index = -1 if not found
	 * set following globals if found
	 * 1.  var_loc   = lcl_loc or gbl_loc
	 * 2.  var_type  = var_seta_type|var_setb_type|var_setc_type
	 * 3.  var_name_index = for lcl/gbl seta, setb, setc array 
	 * 4.  set_sub = subscript
	 * 5.  seta_value|setb_value|setc_value
	 * 6.  seta_index|setb_index|setc_index 
     */
	set_sub = var_sub;
	var_name_index = find_lcl_key_index("L:" + var_name);
    if (var_name_index != -1){
		var_loc = lcl_loc;
		var_type = lcl_set_type[var_name_index];
		get_lcl_set_value();
		return true;
	}
	var_name_index = -1;
	return false;
}
private boolean find_gbl_set(String var_name,int var_sub){
	/*
	 * find gbl set variable else false
	 * also set var_name_index = -1 if not found
	 * set following globals if found
	 * 1.  var_loc   = lcl_loc or gbl_loc
	 * 2.  var_type  = var_seta_type|var_setb_type|var_setc_type
	 * 3.  var_name_index = for lcl/gbl seta, setb, setc array 
	 * 4.  set_sub = subscript
	 * 5.  seta_value|setb_value|setc_value
	 * 6.  seta_index|setb_index|setc_index 
     */
	var_name_index = find_key_index("G:" + var_name);
	if (var_name_index != -1){
		var_loc = gbl_loc;  
		var_type = gbl_set_type[var_name_index];
		get_gbl_set_value();
	    return true;
 	}
	var_name_index = -1;
	return false;
}
private void get_lcl_set_value(){
	/* 
	 * set seta_value|setb_value|setc_value
	 * based on var_name_index and set_sub
	 */
	switch (var_type){
    case 1:
    	 seta_index = lcl_set_start[var_name_index] + set_sub - 1;
    	 if (seta_index >= lcl_set_end[var_name_index]
			|| seta_index < lcl_set_start[var_name_index]){
    	 	log_error(48,"seta subscript out of range - " + set_name + "(" + set_sub +")");
    	 	seta_index = lcl_set_start[var_name_index];
    	 }
    	 seta_value = lcl_seta[seta_index];
    	 break;
    case 2:
    	 setb_index = lcl_set_start[var_name_index] + set_sub - 1;
    	 if (setb_index >= lcl_set_end[var_name_index]
			|| setb_index < lcl_set_start[var_name_index]){
    	 	log_error(49,"setb subscript out of range - " + set_name + "(" + set_sub +")");
    	 	setb_index = lcl_set_start[var_name_index];
    	 }
    	 setb_value = lcl_setb[setb_index];
    	 break;
    case 3:
    	 setc_index = lcl_set_start[var_name_index] + set_sub - 1;
    	 if (setc_index >= lcl_set_end[var_name_index]
			|| setc_index < lcl_set_start[var_name_index]){
    	 	log_error(50,"setc subscript out of range - " + set_name + "(" + set_sub +")");
    	    setc_index = lcl_set_start[var_name_index];
    	 }
    	 setc_value = lcl_setc[setc_index];
    	 break;
	 default: 
	  	 abort_error(68,"invalid case index");
	}
}
private void get_gbl_set_value(){
	/* 
	 * set seta_value|setb_value|setc_value
	 * based on var_name_index and set_sub
	 */
	switch (var_type){
    case 1:
    	 seta_index = gbl_set_start[var_name_index] + set_sub - 1;
    	 if (seta_index >= gbl_set_end[var_name_index]
			|| seta_index < gbl_set_start[var_name_index]){
    	 	log_error(48,"seta subscript out of range - " + set_name + "(" + set_sub +")");
    	    seta_index = gbl_set_start[var_name_index];
    	 }
    	 seta_value = gbl_seta[seta_index];
    	 break;
    case 2:
    	 setb_index = gbl_set_start[var_name_index] + set_sub - 1;
    	 if (setb_index >= gbl_set_end[var_name_index]
			|| setb_index < gbl_set_start[var_name_index]){
    	 	log_error(49,"setb subscript out of range - " + set_name + "(" + set_sub +")");
    	    setb_index = gbl_set_end[var_name_index];
    	 }
    	 setb_value = gbl_setb[setb_index];
    	 break;
    case 3:
    	 setc_index = gbl_set_start[var_name_index] + set_sub - 1;
    	 if (setc_index >= gbl_set_end[var_name_index]
			|| setc_index < gbl_set_start[var_name_index]){
    	 	log_error(50,"setc subscript out of range - " + set_name + "(" + set_sub +")");
    	    setc_index = gbl_set_start[var_name_index];
    	 }
    	 setc_value = gbl_setc[setc_index];
    	 break;
	  default: 
	  	 abort_error(68,"invalid case index");
	}
}
private int get_label_index(String label_source){
	/*
	 * find macro label and return line index
	 * else abort
	 */
    label_match = label_pattern.matcher(label_source);
	String label_name = label_source;
    if (label_match.find()){
	   label_name = label_match.group().toUpperCase();
       int    label_name_index = mac_name_lab_start[mac_name_index];
       while (label_name_index < mac_name_lab_end[mac_name_index]){
       	  if (mac_lab_name[label_name_index].equals(label_name)){
       	     if (opt_trace){
       	    	put_log("TRACE BRANCH " + label_name);
       	     }
       	  	 return mac_lab_index[label_name_index];
       	  } else {
       	  	 label_name_index++;
       	  }
       }
	}
    abort_error(25,"macro label not found - " + label_name);
    return -1;
}
private int find_mac(String macro_name){
	/*
	 * return mac_name index if found else -1 
	 * 
	 * 1. Note load_mac adds entry with -2 
	 *    index to prevent mult search for 
	 *    macros not found.
	 */
	int index = find_key_index("M:" + macro_name.toUpperCase());
	if (index != -1){
		if (mac_name_line_start[index] < 0){
		    return mac_name_line_start[index];
		} else {
			return index;
		}
	}
	return -1;
}
private void call_mac(){
	/*
	 * 1. add macro to call stack
	 * 2. If listcall option, add comment to bal
	 * 2. process proto-type and set parms
	 * 3. init mac_line_index to first macro statement
	 * 
	 */
	 tot_mac_call++;
	 mac_name_index = find_mac_name_index;
	 mac_call_return[mac_call_level] = mac_line_index + 1;
	 mac_call_actr[mac_call_level] = actr_count;
	 actr_count = actr_limit;
	 if  (mac_call_level < max_mac_call_level){
	 	 mac_call_level++;
	 	 mac_line_index = mac_name_line_start[mac_name_index];
	 	 mac_call_name_index[mac_call_level] = mac_name_index;
	 	 mac_call_return[mac_call_level] = -1;
	 	 mac_call_return[mac_call_level] = actr_limit;
	 	 mac_call_pos_start[mac_call_level] = tot_pos_parm;
		 mac_call_kwd_start[mac_call_level] = tot_kwd_parm;
		 mac_call_lcl_name_start[mac_call_level] = tot_lcl_name;
		 mac_call_lcl_seta_start[mac_call_level] = tot_lcl_seta;
		 mac_call_lcl_setb_start[mac_call_level] = tot_lcl_setb;
		 mac_call_lcl_setc_start[mac_call_level] = tot_lcl_setc;
		 mac_call_lcl_key_start[mac_call_level] = tot_lcl_key_tab;
		 mac_call_lcl_key_root[mac_call_level] = cur_lcl_key_root;
		 /*
		  * init lcl key index table variables
		  */
		 cur_lcl_key_root = tot_lcl_key_tab + 1;
		 tot_lcl_key_tab  = tot_lcl_key_tab + max_lcl_key_root + 1;
		 lcl_key_index = cur_lcl_key_root;
		 while (lcl_key_index < tot_lcl_key_tab){
		 	lcl_key_tab_key[lcl_key_index] = null;
		 	lcl_key_index++;
		 }
		 /*
		  * add lcl system variables (sysndx etc)
		  */
         add_lcl_sys();
		 if  (opt_listcall){
		 	 String call_label = bal_label;
		 	 String call_parms = bal_parms;
		 	 if (call_label == null){
		 	 	call_label = "        ";
		 	 } else if (call_label.length() < 8){
		 	       call_label = call_label.concat("        ").substring(0,8);
		 	 }
		 	 String call_op = bal_op;
		 	 if (call_op.length() < 8){
		 	 	call_op = call_op.concat("       ").substring(0,8);
		 	 }
		 	 String lcl_sysndx = "    " + gbl_sysndx;
		 	 lcl_sysndx = lcl_sysndx.substring(lcl_sysndx.length() - 4);
		 	 if (call_parms == null)call_parms = "";
		 	 put_bal_line("* CALL " + lcl_sysndx + " " + call_op + " - " + call_label + " " + bal_op + " " + call_parms);
		 }
		 /*
		  * 1.  parse proto-type and add initial
		  *     key word parms with values
		  * 2.  parse macro call statement and set
		  *     positional and key word parm values
		  * 
		  */
		 init_call_parms();
		 cur_pos_parm = mac_call_pos_start[mac_call_level];
		 set_call_parm_values();
		 mac_line_index = mac_name_line_start[mac_name_index];
	 } else {
	 	 abort_error(30,"max level of nested macros exceeded");
	 }
}
private void add_lcl_sys(){
	/*
	 * add local system macro variables
	 */
	 gbl_sysndx++;
	 lcl_set_name[tot_lcl_name] = "&SYSNDX";
	 lcl_set_type[tot_lcl_name] = var_seta_type;
	 lcl_set_start[tot_lcl_name] = tot_lcl_seta;
	 lcl_set_end[tot_lcl_name] = tot_lcl_seta+1;
	 lcl_seta[tot_lcl_seta] = gbl_sysndx;
	 if (find_lcl_key_index("L:" + lcl_set_name[tot_lcl_name]) == -1){
	 	add_lcl_key_index(tot_lcl_name);
	 	tot_lcl_set++;
	 } else {
	 	log_error(92,"duplicate lcl sys variable - " + lcl_set_name[tot_lcl_name]);
	 }
	 tot_lcl_name++;
	 tot_lcl_seta++;
}
private void init_call_parms(){
	/*
	 * parse proto-type to set pos and key
	 * parm initial values
	 */
    String proto_type_line = mac_file_line[mac_name_line_start[mac_name_index]];
    proto_label = null;
    proto_op    = null;
    proto_parms = null;
    String[] tokens = split_line(proto_type_line);
    proto_label = tokens[0];
    proto_op    = tokens[1];
    proto_parms = tokens[2];
    if  (proto_label.length() > 0){
        init_pos_parm(proto_label);  // set syslist(0) label
    } else {
    	init_pos_parm(""); // syslist(0) null
    }
    if  (proto_parms.length() > 0){
	    String key_name = null;
	    String key_value = null;
	    proto_match = proto_pattern.matcher(proto_parms);
        byte state = 1;
 	    while (proto_match.find()){
 	       parm_value = proto_match.group();
 	       switch (state){
			   case 1: // new parm
 	               if  (parm_value.equals(",")){
       	               init_pos_parm(""); 
 	               } else if (parm_value.equals(" ")){
 	               	   state = 4;
 	               } else {
 	       	  	       if  (parm_value.charAt(parm_value.length()-1) == '='){
 	       	  	   	       key_name  = parm_value.substring(0,parm_value.length()-1);
                           key_value = "";
                           state = 2; // possible key
 	       	  	       } else {
 	       	  	           init_pos_parm(parm_value);
 	       	  	           state = 3; // skip next commas
 	       	  	       }
 	               }
 	               break;
 	           case 2:
 	       	       if  (parm_value.equals(",")
 	       	       		|| parm_value.equals(" ")){
 	       			   init_key_parm(key_name,key_value);
 	       	   	       state = 1;
 	       	       } else {
 	       	   	       key_value = key_value.concat(parm_value);
 	       	       }
 	       	       break;
 	           case 3:
 	       	       if (parm_value.equals(",")){
 	       	   	      state = 1;
 	       	       } else if (parm_value.equals(" ")){
 	       	       	  state = 4;
 	       	       } else {
 	       	   	      log_error(31,"unexpected parm value after pos parm value");
 	       	       }
 	       	       break;
 	       	  case 4: // ignore spaces and comments
 	       	  	   break;
 			  default: 
 			  	 abort_error(68,"invalid case index");
 	       }
 	   }
 	   if (state == 2){
		  init_key_parm(key_name,key_value);	   	  	
 	   }
    }
}
private void set_call_parm_values(){
    /*
     * set positional and key word parm values
     * from macro call statement
     * 
     * Note mult commas force null pos parms
     */
       if  (bal_label.length() > 0){
           set_pos_parm(bal_label);  // set syslist(0) label
       } else {
       	   set_pos_parm(""); // syslist(0) null
       }
       if  (bal_parms.length() > 0){
   	       String key_name = null;
   	       parm_match = parm_pattern.matcher(bal_parms);
           byte state = 1;
           String token = null;
           char   token_first = '?';
           int    token_len   = 0;
           int level = 0;
    	   while (parm_match.find()){
    	       token = parm_match.group();
    	       token_first = token.charAt(0);
    	       token_len   = token.length();
    	       switch (state){
			       case 1: // new parm
			       	   parm_value = "";
    	               if  (token_first == ','){
          	               set_pos_parm(""); 
    	               } else if (token_first == ' '){
    	               	   state = 4;
    	               } else {
    	                   state = 2; 
                           level = 0;
    	       	  	       if  (token_len >=2 && token.charAt(token_len-1) == '='){
    	       	  	   	       key_name  = token.substring(0,token_len-1); 
    	       	  	   	       parm_value = "";
    	       	  	       } else {
	       	  	       	   	   key_name = null;
	       	  	       	   	   parm_value = token;
	       	  	       	   	   if (token_first == '('){
	       	  	       	   	   	  level = 1;
	       	  	       	   	   }			   
    	       	  	       }
    	               }
    	               break;
    	           case 2: // build simple or sublist parm
    	       	       if (token_first == ' '  
    	       	       		|| (token_first == ','
    	       	       		    && level == 0)){
    	       	       	  if   (key_name == null){
    	       	   	           set_pos_parm(parm_value);
    	       	       	  } else {
   	    	       			   if (!set_key_parm("&" + key_name,parm_value)){
    	    	       	  	       set_pos_parm(key_name + "=" + parm_value);
    	    	       		   }
    	       	       	  }
    	       	       	  if (token_first != ' '){
	       	   	             state = 1;
    	       	       	  } else {
    	       	       	  	 state = 4;
    	       	       	  }
	       	   	          break;
    	       	       } else if (token_first == '('){
    	       	       	  level++;   	       	       	
    	       	       } else if (token_first == ')'){
    	       	       	  level--;    	       	       	  
    	       	       }
    	       	   	   parm_value = parm_value + token;
    	       	       break;
    	       	  case 4:  // ignore spaces and comments
    	       	  	break;
    	 		  default: 
    			  	 abort_error(68,"invalid case index");
    	       }
    	   }
    	   if (state == 2){
    	   	  if (key_name != null){
			     if (!set_key_parm("&" + key_name,parm_value)){
 	  	            set_pos_parm(key_name + "=" + parm_value);
 			     }	   	  	
    	      } else {
    	   	     set_pos_parm(parm_value);
    	   	  }
    	   }
    	   if (cur_pos_parm > tot_pos_parm){
    	   	  tot_pos_parm = cur_pos_parm;
    	   }
         }
}
private void init_pos_parm(String pos_parm_name){
	/*
	 * init positional or key word parm
	 */
    mac_call_pos_name[tot_pos_parm] = pos_parm_name;
    mac_call_pos_parm[tot_pos_parm] = "";
    if (pos_parm_name.length() > 0){
       if (find_lcl_key_index("P:" + pos_parm_name) == -1){
       	   add_lcl_key_index(tot_pos_parm);
       	   tot_pos++;
       } else {
       	   log_error(90,"duplicate positional parm - " + pos_parm_name);
       }
    }
    tot_pos_parm++;
}
private void init_key_parm(String kwd_parm_name,String kwd_parm_value){
	   /*
	    * add key work parm name and default value
	    */
		mac_call_kwd_name[tot_kwd_parm] = kwd_parm_name;
		mac_call_kwd_parm[tot_kwd_parm] = kwd_parm_value;
	    if (kwd_parm_name.length() > 0){
	        if (find_lcl_key_index("K:" + kwd_parm_name) == -1){
	        	   add_lcl_key_index(tot_kwd_parm);
	        	   tot_kwd++;
	        } else {
	        	   log_error(91,"duplicate keyword parm - " + kwd_parm_name);
	        }
	     }
		tot_kwd_parm++;
}
private void set_pos_parm(String pos_parm){
		/*
		 * init positional parm and increment 
		 * cur_pos_parm set by 
		 */
	    mac_call_pos_parm[cur_pos_parm] = pos_parm;
	    if (cur_pos_parm >= tot_pos_parm){
	    	mac_call_pos_name[cur_pos_parm] = "";
	    }
	    cur_pos_parm++;
	}
private boolean set_key_parm(String key, String key_parm){
	/*
	 * set keyword parm
	 */
	int key_index = find_kwd_parm(key);
    if  (key_index != -1){
		mac_call_kwd_parm[key_index] = key_parm;
	    return true;
    } else {
	    return false;
    }
}
private int find_kwd_parm(String kwd_name){
	/*
	 * find keywork parm and return index else -1
	 */
	int kwd_index = mac_call_kwd_start[mac_call_level];
	while (kwd_index < tot_kwd_parm){
		if (mac_call_kwd_name[kwd_index].equals(kwd_name)){
	        return kwd_index;
		}
		kwd_index++;
	}
	return -1;
}
private void exit_mz390(){
	/*
	 * display total errors
	 * set rc=16 if errors and no rc set
	 * close files and exit to system or caller
	 */
	  if    (mz390_rc == 0 && mz390_errors > 0){
    	mz390_rc = 16;
      }
  	  put_stats();
      close_files();
	  if    (mz390_aborted){
	    	System.exit(mz390_rc);
	  }
}
private void put_stats(){
	/*
	 * display statistics as comments at end of BAL
	 */
	log_to_bal = false;
	if  (opt_stats || (mz390_errors > 0)){
  	      log_to_bal = true;
	      put_log("Stats total MLC source      = " + tot_mac_line);
	      put_log("Stats total BAL output      = " + tot_bal_line);
	      put_log("Stats total opcodes         = " + tot_mac_name);
	      put_log("Stats total macro loads     = " + tot_mac_load);
	      put_log("Stats total macro calls     = " + tot_mac_call);	
	      put_log("Stats total global set var  = " + tot_gbl_name);
	      put_log("Stats total local pos parms = " + tot_pos);
	      put_log("Stats total local key parms = " + tot_kwd);
	      put_log("Stats total local set var   = " + tot_lcl_set);
		  put_log("Stats total Keys            = " + tot_key);
		  put_log("Stats Key searches          = " + tot_key_search);
		  if (tot_key_search > 0){
		      avg_key_comp = tot_key_comp/tot_key_search;
		  }
		  put_log("Stats Key avg comps         = " + avg_key_comp);
		  put_log("Stats Key max comps         = " + max_key_comp);
	      put_log("Stats total macro instr.    = " + tot_mac_ins);
	      if  (opt_timing){
	          cur_date = new Date();
	          tod_end = cur_date.getTime();
	          tot_msec = tod_end-tod_start+1;
	          put_log("Stats total milliseconds    = " + tot_msec);
	          ins_rate = tot_mac_ins*1000/tot_msec;
	          put_log("Stats instructions/second   = " + ins_rate);
	      }
	      if (opt_listfile){
	      	 int index = 0;
	      	 while (index < tot_mac_file_name){
	      	 	put_log("MZ390 file=" + (index+1)
	      	 		  + " path=" + mac_file_name[index]
					  );
	      	 	index++;
	      	 }
	      }
	}
	put_log("MZ390 total errors          = " + mz390_errors);
	put_log("MZ390 return code           = " + mz390_rc);
	log_to_bal = false;
}
private void close_files(){
	  if (bal_file != null && bal_file.isFile()){
	  	  try {
	  	  	  bal_file_buff.close();
	  	  } catch (IOException e){
	  	  	  abort_error(34,"I/O error on BAL close - " + e.toString());
	  	  }
	  }
	  if (pch_file != null && pch_file.isFile()){
	  	  try {
	  	  	  pch_file_buff.close();
	  	  } catch (IOException e){
	  	  	  abort_error(77,"I/O error on PUNCH close - " + e.toString());
	  	  }
	  }
}
private void log_error(int error,String msg){
	/*
	 * issue error msg to log with prefix and
	 * inc error total
	 */
	  if (mac_abort)return;
	  mac_abort = true;
	  exp_end = true;
	  log_to_bal = true;
	  put_log("MZ390 error=" + error
	  		         + " file=" + (mac_file_name_num[mac_line_index]+1)
					 + " line=" + mac_file_line_num[mac_line_index]
	  		           + " " + msg);
	  mz390_errors++;
	  if (mz390_errors > max_errors){
	  	 abort_error(83,"maximum errors exceeded");
	  }
}
private void abort_error(int error,String msg){
	/*
	 * issue error msg to log with prefix and
	 * inc error total
	 */
	  if (mz390_aborted){
	  	 System.exit(mz390_rc);
	  }
	  mz390_errors++;
	  mz390_aborted = true;
      log_to_bal = true;
	  put_log("MZ390 error " + error 
		         + " file=" + (mac_file_name_num[mac_line_index]+1)
				 + " line=" + mac_file_line_num[mac_line_index]
				 + " " + msg);	 
      exit_mz390();
}
private void put_copyright(){
	   /*
	    * display mz390 version, timestamp,
	    * and copyright if running standalone
	    */
	   	if  (opt_timing){
			cur_date = new Date();
	   	    put_log("mz390 " + version 
	   			+ " Current Date " +mmddyy.format(cur_date)
	   			+ " Time " + mmddyy.format(cur_date));
	   	} else {
	   	    put_log("mz390 " + version);
	   	}
	   	if  (z390_log_text == null){
	   	    put_log("Copyright 2005 Automated Software Tools Corporation");
	   	    put_log("z390 is licensed under GNU General Public License");
	   	}
	   	put_log("MZ390 program = " + dir_mlc + pgm_name + ".MLC");
	   	put_log("MZ390 options = " + opt_parms);
	   }
	   private synchronized void put_log(String msg) {
	   	/*
	   	 * Write message to z390_log_text or console
	   	 * if running standalone
	   	 * 
	   	 */
	   	    if  (opt_trace || log_to_bal){
	   	    	put_bal_line("* " + msg);
	   	    }
	        if  (z390_log_text != null){
  	        	z390_log_text.append(msg + "\n");
   	        }
	        if (opt_con){
   	    	    System.out.println(msg);
   	        }
	   }
	   private int find_key_index(String user_key){
		/*
		 * return user_key_index for user_key else -1
		 * and set following for possible add_key_index:
		 *    1.  key_text = user_key
		 *    2.  key_hash = hash code for key
		 *    3.  key_index_last = last search entry
		 * Notes:
		 *    1.  F: - file name for mac,cpy,dat,pch
		 *    2.  G: - global variable
		 *    3.  M: - macro name
		 *    4.  S: - symbol name (X DS XL8)
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
	private int find_lcl_key_index(String user_key){
		/*
		 * return user_key_index for lcl user_key else -1
		 * and set following for possible add_key_index:
		 *    1.  lcl_key_text = user_key
		 *    2.  lcl_key_hash = hash code for key
		 *    3.  lcl_key_index_last = last search entry
		 */
		tot_key_search++;
		lcl_key_text = user_key;
	    lcl_key_rand.setSeed((long) lcl_key_text.hashCode());
	    lcl_key_hash  = lcl_key_rand.nextInt();
	    lcl_key_index = lcl_key_rand.nextInt(max_lcl_key_root)+cur_lcl_key_root;
		if (lcl_key_tab_key[lcl_key_index] == null){
			lcl_key_index_last = lcl_key_index;
			return -1;
		}
	    cur_key_comp = 0;
		while (lcl_key_index >= cur_lcl_key_root){
			tot_key_comp++;
			cur_key_comp++;
			if (cur_key_comp > max_key_comp){
				max_key_comp = cur_key_comp;
			}
			if (opt_traceall == true){
				put_log("TRACE LCL KEY SEARCHS=" + tot_key_search + " MAX DEPTH=" + max_key_comp + " COMPS=" + tot_key_comp);
			}
			if (lcl_key_hash == lcl_key_tab_hash[lcl_key_index]
			    && user_key.equals(lcl_key_tab_key[lcl_key_index])){			
				lcl_key_index_last = -1;
		    	return lcl_key_tab_index[lcl_key_index];
		    }
			lcl_key_index_last = lcl_key_index;
			if (lcl_key_hash < lcl_key_tab_hash[lcl_key_index]){
			    lcl_key_index = lcl_key_tab_low[lcl_key_index];
			} else {
				lcl_key_index = lcl_key_tab_high[lcl_key_index];
			}
		}
		return -1;
	}
	private void add_lcl_key_index(int user_index){
		/*
		 * add lcl user_index entry based on
		 * lcl_key_text, lcl_key_hash, and lcl_key_index_last
		 * set by prior find_lcl_key_index
		 * 
		 */
		if (lcl_key_tab_key[lcl_key_index_last] == null){
			lcl_key_index = lcl_key_index_last;
		} else {
			if (tot_lcl_key_tab < max_lcl_key_tab){
				lcl_key_index = tot_lcl_key_tab;
				tot_lcl_key_tab++;
			} else {
				abort_error(89,"lcl key search table exceeded");
			}
			if (lcl_key_hash < lcl_key_tab_hash[lcl_key_index_last]){
			    lcl_key_tab_low[lcl_key_index_last] = lcl_key_index;
		    } else {
			    lcl_key_tab_high[lcl_key_index_last] = lcl_key_index;
		    }
		}
		if (opt_traceall == true){
			put_log("TRACE LCL KEY ADD LAST INDEX =" + lcl_key_index_last + " NEW INDEX=" + lcl_key_index + " KEY=" + lcl_key_text);
		}
		tot_key++;
		lcl_key_tab_key[lcl_key_index]   = lcl_key_text;
		lcl_key_tab_hash[lcl_key_index]  = lcl_key_hash;
		lcl_key_tab_index[lcl_key_index] = user_index;
	    lcl_key_tab_low[lcl_key_index] = 0;
	    lcl_key_tab_high[lcl_key_index] = 0;
	}
}