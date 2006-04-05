import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.lang.reflect.Array;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.GregorianCalendar;
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
    * 10/03/05 RPI1 - fix trap on invalid directory
    * 10/04/05 RPI5 - option ASCII use ASCII vs EBCDIC
    *            character string compares in ASCII
    *            C'ABC' self defining terms in ASCII
    * 10/04/05 RPI6 - option ERR(nn) limit errors
    * 10/04/05 put mz390 version on BAL with stats option
    * 10/05/05 RPI5 add C".." ascii sdt support
    * 10/17/05 RPI25 change TRACE to TRACEM option
    * 10/18/05 RPI29 use MZ390E and MZ390I prefixes
    * 10/19/05 RPI32 add SYSMAC and SYSCPY dir lists
    * 10/19/05 RPI32 full ascii / ebcdic translate
    * 10/21/05 RPI40 correct parser bug on '?' string
    *          literal where ? is valid prefix operator
    *          and parser bug on '&MF(2)'(1,1)
    * 10/29/05 RPI60 fix AIF parsing of sublist exp. 
    * 11/07/05 RPI84 ignore comments after copy parm
    * 11/07/05 RPI73 C!...! always EBCDIC sdt
    * 11/10/05 RPI80 report duplicate and missing labels
    * 11/17/05 RPI99 fix substring error after log. oper.
    * 11/28/05 RPI113 file path with drive: and no separator
    * 12/03/05 RPI115 fix continuation of lit comma
    * 12/07/05 RPI123 fix support for multiple paths
    * 12/07/05 RPI124 remove traling spaces from source code
    * 12/07/05 RPI125 set multiple SET array values 
    * 12/08/05 RPI129 restart exp_match for '..' with sdt
    * 12/14/05 RPI135 use tz390 shared tables
    * 12/18/05 RPI139 allow comments on LCL/GBL, SET,AIF
    * 12/18/05 RPI 140 correct hanlding of * and .*
    * 12/18/05 RPI 141 handle lower case opcodes
    * 12/18/05 RPI 142 add T' and L' for DS/DC, instr
    *          and control section, plus share tables
    * 12/20/05 RPI 143 return actual parm count for N'&SYSLIST
    *          instead of total positional parms defined
    * 12/20/05 RPI 144 correct precedence for LE < + - * /
    * 12/21/05 RPI 145 correct precedence for K' and , or K'
    * 12/21/05 RPI 147 correct NOT for SETB vars to return 0 or 1
    *          instead of numeric complement.
    * 12/21/05 RPI 146 add option MFC replacing SYM to
    *          control both symbol T'/L' support and 
    *          ingoring instructions rather than doing file 
    *          search for macro.  Use inline to replace ins.
    * 12/23/05 RPI 127 allow user override for MLC type
    * 12/22/05 RPI131 limit file output to maxfile(mb)
    * 12/22/05 RPI130 add &SYS global variables
    * 01/10/06 RPI167 issue error if cont text < 16
    * 01/13/06 RPI171 correct unary +- support
    * 01/17/06 RPI178 allow macro to define lcl dup of gbl
    *          (also correct check for type conflict)
    * 01/17/06 RPI179 support macro array expansion
    * 01/18/06 RPI180 support type N for T'
    * 01/19/06 RPI181 term on any white space char
    * 01/26/06 RPI 172 move options to tz390
    * 01/31/06 RPI 195 fix DOUBLE operator to handel ' and &
    *          and fix PUNCH to handle '', &&, and trim
    * 02/02/06 RPI 196 add UPPER and LOWER operators
    * 02/21/06 RPI 208 use tz390.z390_abort to term.
    * 02/22/06 RPI 213 support label on MEND for file and inline
    * 02/23/06 RPI 214 support * as last oper in first substr exp.
    *          and allow '...'(e1,e2)'...' without concat dot
    * 03/13/06 RPI 223 correct parsing of keyword value in (..)
    * 03/16/06 RPI 238 mnote total and no errors
    * 03/16/06 RPI 239 treat any white space char as space
    * 03/17/06 RPI 233 add macro call/exit level of nesting
    * 03/20/06 RPI 242 fix LCLC and GBLC init errors
    * 03/20/06 RPI 250 fix keyword parser to handle special characters in ()
    * 03/21/06 RPI 253 allow _ to start symbols
    * 03/21/06 RPI 257 allow * for substring 2nd oper
    * 03/22/06 RPI 260 improve error msg for parsing errors
    * 03/25/06 RPI 266 fix loading inline macros within macro
    *          and correct line #'s and drop ANOP except in inlines
    * 04/02/06 RPI 264 verify ascii source and allow long lines
    *          if option text.
    * 04/03/06 RPI 268 support AREAD and PUNCH DSNAME=&var
    *          and correct expression parser to stop on &
    * 04/04/06 RPI 270 support DS/DC/SDT CA and CE
    ********************************************************
    * Global variables
    *****************************************************/
    tz390 tz390 = null;
    int mz390_rc = 0;
    int mz390_errors = 0;
    boolean mac_abort = false;
    Date cur_date = null;
    GregorianCalendar cur_date_cal = null;
    long tod_start = 0;
    long tod_end   = 0;
    long tot_msec = 0;
    long ins_rate    = 0;
    boolean log_to_bal = false;
    int tot_bal_line = 0;
    int tot_mnote = 0;
    int max_mnote_level = 0;
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
	SimpleDateFormat sdf_mmddyy = new SimpleDateFormat("MM/dd/yy");
	SimpleDateFormat sdf_hhmmss = new SimpleDateFormat("HH:mm:ss");
    SimpleDateFormat sdf_sysclock = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss.SSS000");
	SimpleDateFormat sdf_sysdatc  = new SimpleDateFormat("yyyyMMdd");
	SimpleDateFormat sdf_sysdate  = new SimpleDateFormat("MM/dd/yy");
	SimpleDateFormat sdf_systime = new SimpleDateFormat("HH.mm");
	boolean log_tod = true; 
    JTextArea z390_log_text = null;
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
    String max_substring_len = "100000";
    /*
     * macro execution global data
     */
    long    tod_time_limit = 0;
    int     next_time_ins   = 0x1000;
    int     next_time_check = next_time_ins;
    String cur_mac_file_name = null;
    String mac_type = ".MAC"; // default macro type
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
    int mac_last_find_index = -1;
    int tot_ins = 0; // count instr/cntl for MFC option
    int tot_mac_name = 0;      // next avail name
    int load_macro_mend_level = 0;
    boolean load_proto_type = false;
    int load_mac_inline_end = 0;
    int find_mac_name_index = 0;
    int load_mac_name_index = 0;
    byte     load_type       = 0;
    byte     load_mlc_file   = 0; // no MACRO, no prototype, stop on END
    byte     load_mac_file   = 1; // MACRO, prototype, MEND, read file and verify name = prototype
    byte     load_mac_inline = 2; // MACRO, prototype, MEND, read in memory source
    int      load_proto_index = 0; // line index of proto_type statement
    String   load_macro_name = null;
    String   load_file_name = null;
    String[] mac_name = new String[max_mac_name];
    int[]    mac_name_line_start =(int[])Array.newInstance(int.class, max_mac_name);
   	int[]    mac_name_line_end   =(int[])Array.newInstance(int.class, max_mac_name);
    int[]    mac_name_lab_start = (int[])Array.newInstance(int.class,max_mac_name);
    int[]    mac_name_lab_end   = (int[])Array.newInstance(int.class,max_mac_name);
    /*
     * macro bal line tables
     */
    int old_mac_line_index = 0; //prev    mac_line_index
    int mac_line_index = 0;     //current mac line index
    int tot_mac_line = 0;       // next avail line
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
    int[]    mac_lab_num   = (int[])Array.newInstance(int.class,max_mac_lab); // RPI 266
    /*
     * macro call stack variables
     */
    int tot_expand = 0;
    int expand_inc = 100;        //macro array expansion
    int mac_call_level = 0;      //level of macro nesting during loading
    int[]    mac_call_name_index = (int[])Array.newInstance(int.class,max_mac_call_level);
    int[]    mac_call_return = (int[])Array.newInstance(int.class,max_mac_call_level);
    int[]    mac_call_actr   = (int[])Array.newInstance(int.class,max_mac_call_level);
    int[]    mac_call_pos_start = (int[])Array.newInstance(int.class,max_mac_call_level);
    int[]    mac_call_pos_tot   = (int[])Array.newInstance(int.class,max_mac_call_level);
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
    boolean var_subscript_calc = false;
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
    int tot_lcl_set  = 0; // total lcl set var allocated
    int tot_lcl_name = 0; // cur lcl sets on stack
    int tot_lcl_seta = 0;
    int tot_lcl_setb = 0;
    int tot_lcl_setc = 0;
    int lcl_sysndx = -1;  // macro call counter
    String lcl_sysect = "$$CSECT";
    String lcl_sysloc = lcl_sysect;
    String lcl_sysstyp = "";
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
    int    store_max_index  = 0; // array max index+1
    int    store_min_index  = 0; // array min index  
    int    store_seta_index = 0;
    int    store_setb_index = 0;
    int    store_setc_index = 0;
    int    store_seta_value = 0;
    byte   store_setb_value = 0;
    String store_setc_value = null;
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
    int gbl_sysclock_index = 0; // YYYY-MM-DD HH:MM:SS.mmmmmm
    int gbl_sysmac_index = 0;   // macro name at specified level
    String gbl_sysmac  = "";
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
    char[]   sym_etype = (char[])Array.newInstance(char.class,max_sym); // RPI 270
    int[]    sym_len  = (int[])Array.newInstance(int.class,max_sym);
    /*
     * macro operation global variables
     */
    int mac_op_index = 0;
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
    char    asc_space_char = ' '; // white space <= asc_space_char
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
    boolean exp_parse_set_mode = false;  // for set target and lcl/gbl alloc
    boolean exp_alloc_set_mode = false;  // for lcl/gbl alloc
    byte    exp_parse_set_type = 0;
    byte    exp_parse_set_loc  = 0;
    String  exp_parse_set_name = null;
    int     exp_parse_set_sub  = 0;
    int     exp_parse_set_name_index = 0;
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
    boolean exp_prev_substring = false;
    char    exp_prev_first = exp_start_op;
    byte    exp_prev_class = 0;
    char    exp_next_first = asc_space_char;
    byte    exp_next_class = 0;
    int     exp_var_index = -1;  // lcl set index
    boolean exp_var_pushed = false;  // var pushed since last reset
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
           2, 2, 3, 2, 0, 2, 2, 8, 2, 3, 2, 2, 2, 2, 3, // 2 * / prev mpy/div  RPI 214
           3, 3, 3, 4, 3, 0, 3, 8, 0, 3, 3, 3, 3, 3, 3, // 3 (   prev open (...)
           3, 3, 3,11, 0, 0, 0, 8,11, 0, 3, 3, 3, 3, 0, // 4 )   prev var subscript
		   0, 0, 3, 5, 5, 5, 5, 8, 0, 3, 0, 0, 0, 0, 3, // 5 .   prev concat
           3, 3, 3, 6, 3, 6, 3, 8, 3, 3, 3, 3, 3, 3, 3, // 6 ~   prev terminator
		   3, 3, 3, 7, 3, 7, 7, 8, 0, 3, 7, 7, 7, 7, 3, // 7 LL  logical compares //RPI144 (was 1,2 now 3,3)
		   3, 3, 3, 0, 0, 0, 0, 8, 0, 3, 0, 0, 0, 0, 3, // 8 '   string '....'
		   3, 3, 3,10, 0, 0, 0, 0, 9, 3, 0, 0, 0, 0, 3, // 9 ,   substring '...'(e1,e2)
		  12,12,12,12,12,12,12, 3,12,12,12,12,12,12, 3, //10 ?'  prefix operator  //RPI145, RPI196
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
	    load_type = load_mlc_file;
    	load_file_name = tz390.dir_mlc + tz390.pgm_name + tz390.pgm_type;
  		load_mac();
    	mlc_line_end = tot_mac_line;
    	loading_mlc = false;
    	init_lcl_sys();
    	if (tz390.opt_trap){
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
        tz390 = new tz390();
        tz390.init_tables();
        tz390.init_options(args,".MLC");  
	    if (tz390.opt_timing){
	    	cur_date = new Date();
	    } else {
		    cur_date_cal = new GregorianCalendar(2005,0,2,22,33,44);
		    cur_date = new Date(cur_date_cal.getTime().getTime()+567);
	    }
        tod_start = cur_date.getTime();
        if (!tz390.init_opcode_name_keys()){
        	abort_error(118,"opcode key table error - aborting");
        }
        init_gbl_sys();
        tod_time_limit = max_time_seconds * 1000 + tod_start;
        /*
         * var_pattern used for finding and replacing       
         * scalar, subscripted, and crated &variables
         */
        try {
    	     var_pattern = Pattern.compile(
    	    	  "([&][&])"	   //RPI192
    	        + "|([']['])"      //RPI192
    			+ "|([&][\\(])"
    			+ "|([&][a-zA-Z$@#_][a-zA-Z0-9$@#_]*)"  // RPI 253 
    			+ "|([&])"         //RPI192
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
        		   	"([a-zA-Z$@#_][a-zA-Z0-9$@#_]*[=])"    // RPI 253
    	    		  +	"|([c|C][']([^']|(['][']))*['])" 
    	  	          + "|([']([^']|(['][']))*['])" 
    	    		  + "|([^\\s',()]+)"  //RPI181
    	    	      + "|([\\s',()])"    //RPI181
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
        *   7.  return ( and ) to parse kw parm value (a,b)  RPI 223
        * */
       	try {
       	    proto_pattern = Pattern.compile(
        			"([&][&])"
          	      + "|([&][(])"
          	      + "|([&][a-zA-Z$@#_][a-zA-Z0-9$@#_]*[=]*)" // &var or &var= for keyword  RPI 253
				  + "|([0-9]+)"                        // number
	       		  + "|([']([^']|(['][']))*['])"        // parm in quotes
				  + "|([\\s/()',\\.\\+\\-\\*=])"       // operators and white space RPI181 (\\ for reg exp. opers)
				  + "|([kK]['])"                       // K' character length of var
				  + "|([l|L]['])"                      // L' length attribute of symbol
				  + "|([n|N]['])"                      // N' number of parm subparms (n1,n2,n3)
				  + "|([t|T]['])"                      // T' type attribute of symbol
  		    	  + "|([bB]['][0|1]+['])"              // B'0110' binary self def. term
  		    	  +	"|([cC][aAeE]*[']([^']|(['][']))*['])"    // C'ABCD' ebcdic or ascii self def. term // RPI 270
  		    	  +	"|([cC][\"]([^\"]|([\"][\"]))*[\"])"    // C"ABCD" ascii self def. term   RPI73
  		    	  +	"|([cC][!]([^!]|([!][!]))*[!])"        // C"ABCD" ebcdic self def. term  RPI84
	    		  +	"|([xX]['][0-9a-fA-F]+['])"        // X'0F'   hex self defining term
				  + "|([a-zA-Z$@#_][a-zA-Z0-9$@#_]*)"   // symbol or logical operator (AND, OR, XOR, NOT, GT, LT etc.) // RPI 253
				  + "|([^',()\\s]+)"   // RPI 223, RPI 250                  // any other text
   			  );
       	} catch (Exception e){
       		  abort_error(2,"proto pattern errror - " + e.toString());
       	}
        /*
         * macro label_pattern  .lll
         */
        	try {
        	    label_pattern = Pattern.compile(
        			"([.][a-zA-Z$@#_][a-zA-Z0-9$@#_]*)"  // RPI 253         
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
        	        "([']['])"
        	      + "|([&][&])" //RPI195
      			  + "|([^'&]+)"
				  + "|(['&])"             
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
            	      + "|([&][a-zA-Z$@#_][a-zA-Z0-9$@#_]*)"    // &var  RPI 253
					  + "|([0-9]+)"                             // number
					  + "|([\\s&/()',\\.\\+\\-\\*=])"           // operators and white space RPI181 (\\ for reg exp. opers)
					  + "|([kK]['])"                            // K' character length of var
					  + "|([l|L]['])"                           // L' length attribute of symbol
					  + "|([n|N]['])"                           // N' number of parm subparms (n1,n2,n3)
					  + "|([t|T]['])"                           // T' type attribute of symbol
	  		    	  + "|([bB]['][0|1]+['])"                   // B'0110' binary self def. term
	  		    	  +	"|([cC][aAeE]*[']([^']|(['][']))*['])"         // C'ABCD' ebcdic or ascii self def. term // RPI 270
	  		    	  +	"|([cC][\"]([^\"]|([\"][\"]))*[\"])"    // C"ABCD" ascii self def. term   RPI73
	  		    	  +	"|([cC][!]([^!]|([!][!]))*[!])"         // C"ABCD" ebcdic self def. term  RPI84
		    		  +	"|([xX]['][0-9a-fA-F]+['])"             // X'0F'   hex self defining term
					  + "|([a-zA-Z$@#_][a-zA-Z0-9$@#_]*)"       // symbol or logical operator (AND, OR, XOR, NOT, GT, LT etc.)
					  + "|([^'&]+)"  // RPI 268                            // string text
            	    );
            	} catch (Exception e){
            		  abort_error(4,"expression pattern errror - " + e.toString());
            	}

               /*
                * open MLC and BAL files
                */
		 open_files();
		 put_copyright();
}
private void open_files(){
	/*
	 * open BAL and set MAC directory
	 */
        bal_file = new File(tz390.dir_bal + tz390.pgm_name + ".BAL");
       	try {
       	    bal_file_buff = new BufferedWriter(new FileWriter(bal_file));
       	} catch (IOException e){
       		abort_error(8,"I/O error on BAL open - " + e.toString());
       	}
}

private void process_mac(){
	/* 
	 * execute mlc as open code macro expanding
	 * any macros found and outputing all model
	 * statements to BAL file after substitution
	 * of any parms and macro variables.
	 *  
	 */
	     while (!mlc_eof && !tz390.z390_abort){
	    	/*
	    	 * repeat executing nested macro code previously
	    	 * started by macro call 
	    	 */
	     	  mac_abort = false;
	     	  tot_mac_ins++;
		      if  (mac_line_index >= mac_name_line_end[mac_call_name_index[mac_call_level]]){
	             if (tz390.opt_tracem){ 
	           	  	   put_log("TRACE MACRO CALL END " + mac_name[mac_call_name_index[mac_call_level]]);
	           	 }
		         if  (tz390.opt_listcall){
		         	 if  (mac_call_level > 0){
		           	     String sysndx = "    " + get_set_string("&SYSNDX",1);
				 	     sysndx = sysndx.substring(sysndx.length() - 4);				 	     
					 	 String sysnest = "  " + mac_call_level;
					 	 sysnest = sysnest.substring(sysnest.length() - 2);
		   	 	         put_bal_line("*MEXIT #=" + sysndx + " LV=" + sysnest + " " + mac_name[mac_call_name_index[mac_call_level]]);
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
		    	   if (mac_line_index >= tot_mac_line || mac_line_index < 0){
		    		   abort_error(137,"macro source index error - " + mac_line_index);
		    	   }
	               bal_line = mac_file_line[mac_line_index];
	               parse_bal_line();
	               mac_op_index = find_mac_op();
	               if (mac_op_index != -1){
	           	      exec_mac_op();      // execute macro operation
       		          bal_line = null;    // force macro execution cycle
	               } else if (bal_op != null) {
	           	      find_mac_name_index = find_mac(bal_op);
	           	      if (find_mac_name_index == -1){
             	      	 cur_mac_file_name = tz390.find_file_name(tz390.dir_mac,bal_op,mac_type,tz390.dir_cur);
           	      		 if (cur_mac_file_name != null){
           	      			 load_type = load_mac_file;
           	      			 load_file_name = cur_mac_file_name;
           	      			 load_mac();
           	      			 find_mac_name_index = load_mac_name_index;
           	      		 } else { // add dummy mac to prevent search
           	      		     tz390.find_key_index("M:" + bal_op);
           	      		     if (!tz390.add_key_index(-2)){
           	      		    	 abort_error(119,"macro index table exceeded");
           	      		     }
           	      			 find_mac_name_index = -2;
           	      		 }
	           	      }
	             	  if (find_mac_name_index >= 0){
	           	         call_mac();      // call a nested macro
	           	         bal_line = null; // force macro exeuction cycle
	           	      }
	               }		   
			       mac_line_index++;
	          }
		      if   (bal_line != null){
		    	   if (tz390.opt_tracem){
		    		   put_log("TRACE BAL OUTPUT - " + bal_line);
		    	   }
		      	   put_bal_line(bal_line);
	          }
		      if (!mlc_eof && actr_count <= 0){
		      	 abort_error(82,"actr limit exceeded");
		      }
			  if (tz390.opt_time
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
private void load_mac(){
	/*
	 * load macro from file or inline and 
	 * set load_mac_name_index else 
     * abort with error
	 * 
	 * load type 0 = MLC file
	 *   no MACRO, no proto-type, end on END
	 * load type 1 = MAC file
	 *   MACRO, MEND, and verify proto-type name = file name
	 * load type 2 = inline macro
	 *   MACRO, MEND, and proto-type defines macro name
	 *   
	 * 1.  Return -2 if file not found
	 * 2.  Concatentate any continuations indicated
	 *     by non-blank in position 72.  Each 
	 *     continuation must start at position 16.
	 * 3.  Ignore .* macro comments
	 * 4.  Define any macro labels .xxx and check
	 *     references.
	 * 5.  if continue char in 72, delimit at first
	 *     ", " after 16.
	 * 6.  initial program MLC loads as 0 mac name entry
	 * 7.  Load inline macros without processing labels etc.
	 *     and includes are not expanded until inline load
	 */
	tot_mac_load++;
	cur_mac_file = 0;
	cur_mac_line_num = 0;
	load_macro_mend_level = 0;
	load_proto_index = 0;
	load_macro_name = "";
	int save_mac_name_index = mac_name_index;
	int save_mac_line_index = mac_line_index;
    switch (load_type){
    case 0: // MLC
    	load_open_macro_file();
    	load_macro_mend_level = 1; // no macro statement
    	mac_line_index = tot_mac_line;
    	break;
    case 1: // macro file
    	load_open_macro_file();
    	load_macro_mend_level = 0; // read macro from file
    	mac_line_index = tot_mac_line;
    	break;
    case 2: // macro inline
    	mac_line_index++; // skip macro statement
    	if (tz390.opt_tracem){
    		put_log("TRACE LOADING INLINE MACRO");
    	}
    	load_macro_mend_level = 1; // macro statment read inline
    	load_mac_inline_end = mac_name_line_end[mac_name_index];
    	break;
    }
	load_get_mac_line();
	while (mac_line != null
			&& mac_line_index < max_mac_line){
		if  (mac_line != null){
			if (load_type != load_mac_inline
			    && (mac_line.length() < 2
				    || !mac_line.substring(0,2).equals(".*"))
				){
				mac_file_line[mac_line_index]     = mac_line;
				mac_file_name_num[mac_line_index] = cur_mac_file_num;
				mac_file_line_num[mac_line_index] = cur_mac_line_num;
			}
		}
		load_proto_type = false;
		if (mac_op != null && mac_op.length() > 0){
			load_macro_mend_proto_type();
		    if (load_macro_mend_level == 1){ 
                  load_macro_ago_aif_refs();
		    }
		}
	    if  (load_macro_mend_level <= 1
		    	&& !load_proto_type // skip prototype
		    	&& mac_label != null
		    	&& mac_label.length() > 0){
		    	load_macro_label_sym();
		}
   	    if (load_macro_mend_level == 0
      		&& load_type != load_mlc_file
      		&& load_proto_index != 0){
     		 mac_line = null; // eof at level 0 for macro
     	}
		if  (mac_line != null){
			if (load_type == load_mac_inline
				|| !(mac_line.length() > 1
				     && (mac_line.substring(0,2).equals(".*")
				         || (mac_op != null 
				        	 && mac_op.equals("ANOP")
				     	     && load_macro_mend_level == 1
				    	    )
				        )
		            )
				){    
				mac_line_index++;
			}
			load_get_mac_line();
		}
	}
	if (mac_line_index >= max_mac_line){
		abort_error(87,"maximum source lines exceeded");
	}
	switch (load_type){
	case 0: // MLC file 
	    if (load_macro_mend_level != 1){
	    	log_error(133,"unbalanced macro mend in " + load_macro_name);
	    }
		tot_mac_line = mac_line_index;
		mac_name_line_end[mac_name_index] = tot_mac_line;
		mac_line_index = save_mac_line_index;
		break;
	case 1: // macro file
	    if (load_macro_mend_level != 0){
			log_error(134,"unbalanced macro mend in " + load_macro_name);
		}
		tot_mac_line = mac_line_index;
		mac_name_line_end[mac_name_index] = tot_mac_line;
		mac_line_index = save_mac_line_index;
		break;
	case 2: // inline macro file
	    if (load_macro_mend_level != 0){
			log_error(135,"unbalanced macro mend in " + load_macro_name);
		}
		mac_name_line_end[mac_name_index] = mac_line_index;
	}
	mac_name_lab_end[mac_name_index] = tot_mac_lab;
	check_undefined_labs(mac_name_index);
	load_mac_name_index = mac_name_index;
	mac_name_index = save_mac_name_index;
}
private void load_open_macro_file(){
	/*
	 * open file for MLC or macro file
	 * else abort with error
	 */	
	mac_file[cur_mac_file] = new File(load_file_name);
	if (!mac_file[cur_mac_file].isFile()){
		abort_error(39,"macro file not found - " + load_file_name); //RPI169
	}
	load_macro_name = mac_file[cur_mac_file].getName().toUpperCase();
	if (tot_mac_name > 0){ // RPI127 leave suffix on main pgm loaded as macro
		int index = load_macro_name.indexOf('.');
		if (index > 0){
			load_macro_name = load_macro_name.substring(0,index);
		}
	}
	add_mac(load_macro_name);
	try {
		cur_mac_file = 0;
		mac_file_buff[cur_mac_file] = new BufferedReader(new FileReader(mac_file[cur_mac_file]));
		set_mac_file_num();
	} catch (IOException e){
		abort_error(26,"I/O error opening file - " + e.toString());
	}
	if (tz390.opt_tracem){
		put_log("TRACE LOADING " + load_file_name);
	}
}
private void load_macro_mend_proto_type(){
	/* 
	 * process macro, mend, and proto-type
	 * during loading of MLC or macro
	 */
     if (mac_op.equals("MACRO")){
    	 load_macro_mend_level++;
     } else if (mac_op.equals("MEND")){
    	 load_macro_mend_level--;
     } else if (load_type != load_mlc_file 
		 	   && load_proto_index == 0
    		   && load_macro_mend_level == 1
    		   && mac_op != null
               ){ // process proto-type
    	 load_proto_type = true;
		 load_proto_index = mac_line_index;
		 mac_name_line_start[mac_name_index] = mac_line_index;
    	 if (load_type == load_mac_file){
			 if (!mac_op.equals(load_macro_name)){
				 log_error(132,"macro proto-type name " + mac_op + " not = file name " + load_macro_name);
			 }
		 } else {  // define inline macro
			 load_macro_name = mac_op;
			 load_proto_index = mac_line_index;
			 mac_name_index = find_mac(load_macro_name);
			 if (mac_name_index < 0){
				   if (tot_mac_name < max_mac_name){
				    	mac_name_index = tot_mac_name;
					    if (tz390.find_key_index("M:" + mac_op) == -2){
					    	tz390.update_key_index(mac_name_index);
					    } else {
					    	if (!tz390.add_key_index(mac_name_index)){
					    		abort_error(87,"key search table exceeded");
					    	}
					    }
			   	    	tot_mac_name++;
			       } else {
			   	      	abort_error(60,"maximum macros exceeded for - " + mac_op);
			   	      	return;
			       }
			   }
			   mac_name[mac_name_index] = mac_op;
			   mac_name_line_start[mac_name_index] = load_proto_index;
			   mac_name_lab_start[mac_name_index] = tot_mac_lab;
		 }
	 }
}
private void load_macro_ago_aif_refs(){
	/*
	 * check ago and aif references during loading
	 */
	if (mac_op.equals("AGO")){
        label_match = label_pattern.matcher(mac_parms);
        if (label_match.find()){
           add_mac_label(mac_name_index
        		        ,label_match.group().toUpperCase()
        		        ,-mac_line_index);
        } else {
        	log_error(112,mac_name[mac_name_index] + " invalid AGO label - " + mac_parms);
        }
	} else if (load_macro_mend_level == 1 
			   && mac_op.equals("AIF")){
		int lab_index = mac_parms.lastIndexOf(").");
		if (lab_index > 0){
			label_match = label_pattern.matcher(mac_parms.substring(lab_index+1));
			if (label_match.find()){
				add_mac_label(mac_name_index
						     ,label_match.group().toUpperCase()
						     ,-mac_line_index);
			} else {
				log_error(113,mac_name[mac_name_index] + " invalid AIF label - " + mac_parms);
			}
		} else {
			log_error(114,mac_name[mac_name_index] + " invalid AIF label - " + mac_parms);
		}
	}
}
private void load_macro_label_sym(){
	/*
	 * check macro labels and symbol labels
	 * during macro loading
	 * 1.  remove .*
	 * 2.  add macro labels
	 * 3.  add symbol and length
	 */
	if (mac_label.length() > 1
		&& mac_label.charAt(0) == '.'
		&& mac_label.charAt(1) != '*'){
		label_match = label_pattern.matcher(mac_label);
		if (label_match.find()){
			add_mac_label(mac_name_index
        	     ,label_match.group().toUpperCase()
        	     ,mac_line_index);
			mac_label = null;
		} else {
			log_error(40,"invalid macro label - " + mac_label);
		}
	} else if (mac_label.length() > 0
				&& mac_label.charAt(0) != '*'  // RPI 140
				&& mac_label.charAt(0) != '&'){
		set_sym_type_len(mac_label,mac_op,mac_parms);
	}

}
private void add_mac(String macro_name){
	/*
	 * add macro file entry and 
	 * set mac_name_index else abort
	 */
	if (tot_mac_name < max_mac_name){
		mac_name_index = tot_mac_name;
		if (tot_mac_name > 0){  // RPI127 skip main pgm to allow macro later
			tz390.find_key_index("M:" + macro_name);
			if (!tz390.add_key_index(mac_name_index)){
				abort_error(87,"key search table exceeded");
			}
		} else {
			macro_name = "OPEN CODE";
		}
		tot_mac_name++;
		mac_name[mac_name_index] = macro_name.toUpperCase();
		mac_name_line_start[mac_name_index] = mac_line_index; 
		mac_name_lab_start[mac_name_index]  = tot_mac_lab;
	} else {
		abort_error(27,"max macros exceeded");
	}
}
private void set_mac_file_num(){
	/*
	 * find/add file name and set cur_mac_file_num
	 */
	String mac_file_key = mac_file[cur_mac_file].getPath(); 
	cur_mac_file_num = tz390.find_key_index(
			"F:" + mac_file_key);		
	if (cur_mac_file_num == -1){
		if (tot_mac_file_name < max_mac_file_name){
			cur_mac_file_num = tot_mac_file_name;
			tot_mac_file_name++;
			if (!tz390.add_key_index(cur_mac_file_num)){
				abort_error(87,"key search table exceeded");
			}
			mac_file_name[cur_mac_file_num] = mac_file_key;
		}
	}
	mac_file_name_num[cur_mac_file] = cur_mac_file_num;
}
private void add_mac_label(int mac_index
		,String mac_label, int lab_line){
	/*
	 * add macro label and check for duplicates
	 */
	int index = mac_name_lab_start[mac_index];
	while (index < tot_mac_lab){
		if (mac_label.equals(mac_lab_name[index])){
			if (mac_lab_index[index] <= 0){ 
				// found forward ref - set lab line/num
				if (lab_line > 0){
					mac_lab_index[index] = lab_line;
					mac_lab_num[index] = mac_file_line_num[lab_line];
				}
			} else if (lab_line > 0){
				log_error(111,(mac_name[mac_index]) + " duplicate " + mac_label + " at " + mac_lab_num[index]);
			}
		    return;
		}
		index++;
	}
	// add new unreferenced label
	if (tot_mac_lab < max_mac_lab){
		String label_name = label_match.group().toUpperCase();
		mac_lab_name[tot_mac_lab] = label_name;
		mac_lab_index[tot_mac_lab] = lab_line;
		mac_lab_num[tot_mac_lab] = mac_file_line_num[mac_line_index];
		tot_mac_lab++;
	} else {
		abort_error(110,mac_name[mac_index] + " maximum macro labels exceeded");
	}
}
private void check_undefined_labs(int mac_index){
	/*
	 * issue errors for any undefined macro
	 * labels.
	 */
	int index = mac_name_lab_start[mac_index];
	while (index < mac_name_lab_end[mac_index]){
		if (mac_lab_index[index] <= 0){
			mac_abort = false;
			int old_mac_line_index = mac_line_index;
			mac_line_index = -mac_lab_index[index];
			log_error(115,mac_name[mac_index] + " undefined " + mac_lab_name[index]);
		    mac_line_index = old_mac_line_index;
		}
		index++;
	}
}
private void load_get_mac_line(){
	/*
	 * get next mac line from file or inline
	 *   1.  Concatenating continuation lines
	 *       and parse mac line
	 *   2.  Truncate continued lines at first ", "
	 *   2.  Read nested copy files
	 */
	if (load_type == load_mac_inline){
		if (load_macro_mend_level > 0 
			&& mac_line_index < load_mac_inline_end){ 
			mac_line = mac_file_line[mac_line_index];
			parse_mac_line();
		} else {
			mac_line = null;
		}
		return;
	}
	String temp_line = null;
    try {
    	boolean retry = true;
    	while (retry){
    		retry = false;  
    		temp_line = mac_file_buff[cur_mac_file].readLine();
            cur_mac_line_num++;
            if (temp_line == null){
            	mac_file_buff[cur_mac_file].close();
            	cur_mac_file--;
            	if (cur_mac_file >= 0){
            		retry = true;
            		cur_mac_file_num = mac_file_cur_file_num[cur_mac_file];
            		cur_mac_line_num = mac_file_cur_line_num[cur_mac_file];
            		if (tz390.opt_tracem){
            		    put_bal_line("* END OF COPY");
            		}
            	}
            } else {
            	temp_line = trim_line(temp_line);
                if (!tz390.verify_ascii_source(temp_line)){
                	abort_error(138,"invalid ascii source line " + cur_mac_line_num + " in " + mac_file[cur_mac_file].getPath());
                }
            }
    	}
    	if  (temp_line == null){
    			mac_line = null;
   		} else if (tz390.opt_text  // RPI 264 
   				   || temp_line.length() < 72
   				   || temp_line.charAt(71) <= asc_space_char
   				  ){ //RPI181
   			mac_line = trim_line(temp_line);  //RPI124
   		} else {
   		    mac_line = temp_line.substring(0,71);
   		    mac_line = trim_continue(mac_line);
            while (temp_line.length() > 71
            		&& temp_line.charAt(71) > asc_space_char){ //RPI181
            	    temp_line = mac_file_buff[cur_mac_file].readLine();
                    cur_mac_line_num++;
            	    if (temp_line == null){
            	    	abort_error(139,"missing continuation line " + cur_mac_line_num + " in " + mac_file[cur_mac_file].getPath());
            	    }
            	    temp_line = trim_line(temp_line);
            	    if (!tz390.verify_ascii_source(temp_line)){
            	    	abort_error(140,"invalid ascii source line " + cur_mac_line_num + " in " + mac_file[cur_mac_file].getPath());
            	    }
            	    if (temp_line.length() < 72 || temp_line.charAt(71) <= asc_space_char){ //RPI181
            	    	temp_line = trim_line(temp_line); //RPI124
            	    }
            	    if  (temp_line.length() >= 16
            	    		&& temp_line.substring(0,15).equals("               ")){  // RPI167
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
    if (mac_line == null   // RPI 139
    	|| mac_line.length() == 0
        || (mac_line.length() > 1 
            && mac_line.substring(0,2).equals(".*"))){
    	return;
    } else if (mac_line.charAt(0) == '*'){
    	mac_label = "*";
    	return;
    }
    if (tz390.opt_tracem){
    	put_log("TRACE MAC LINE " + mac_line);
    }
    String[] tokens = split_line(mac_line);
    mac_label = tokens[0];
    mac_op    = tokens[1];
    mac_parms = tokens[2];
    if (mac_op != null)mac_op = mac_op.toUpperCase();  // RPI 141
    if (loading_mlc && mac_op.equals("COPY")){
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
	String[] tokens = mac_parms.split("\\s+",2); //RPI84
	new_mac_name = tz390.find_file_name(tz390.dir_cpy,tokens[0],".CPY",tz390.dir_cur);
	if (new_mac_name != null){
		mac_file[cur_mac_file] = new File(new_mac_name);
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
		    log_error(101,"copy file not found - " + mac_parms);
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
	   if  (tz390.opt_mfc && tokens[0].length() > 0){
           set_sym_type_len(tokens[0].toUpperCase(),tokens[1].toUpperCase(),tokens[2].toUpperCase());
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
		if  (tz390.opt_text || bal_line.length() < 72){ // RPI 264
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
        if (bal_file.length() > tz390.max_file_size){
        	abort_error(119,"maximum bal file size exceeded");
        }
	} catch (Exception e){    //RPI1
		System.out.println("MZ390E error 13 - I/O error on BAL write - " + e.toString());
	  	System.exit(16);
	}
}
private void parse_bal_line(){
	/*
	 * 1.  Substitute any macro variables found
	 * 2.  Set bal_label and bal_o
	 */
	if (tz390.opt_tracem){
		put_log("TRACE BAL PARSING  " + bal_line);
	}
	bal_label = null;
	bal_op    = null;
	bal_parms = null;
	if  (bal_line == null 
			|| bal_line.length() == 0
            || (bal_line.length() > 1 && bal_line.substring(0,2).equals(".*"))){
		return;
	} else if (bal_line.charAt(0) == '*') {  
		bal_label = "*";
		return;
	}
	split_bal_line();
	int index = find_mac_op();
    if  (index == -1){ 
    	bal_line = replace_vars(bal_line,true);
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
private String replace_vars(String text,boolean bal_source){
	/* 
	 * replace all variables in text
	 * and set var_replacement if changed
	 * if reduce true, replace && with & and '' with '
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
        parm_value = var_match.group();
        if (parm_value.length() == 1){
        	bal_text_index2 = bal_text_index1 + 1;
        } else if (parm_value.equals("&&")
        		|| parm_value.equals("''")){
        	if (!bal_source){ //RPI192 leave ?? and '' for BAL compatibility in az390
        		// reduce ?? and '' for mnote and punch output
        		parm_value = parm_value.substring(1);
        	}
        	bal_text_index2 = bal_text_index1 + 2;
        } else {
        	parm_value = calc_setc_exp(bal_text.substring(bal_text_index1),0);
        	bal_text_index2 = bal_text_index1 + exp_next_index;
        	if  (bal_text_index2 < bal_text.length()
        			&& bal_text.charAt(bal_text_index2) == '.'){
        		bal_text_index2++; // skip trailing . in parm substitution
        	}
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
private String trim_line(String line){
	/*
	 * remove trailing spaces from non-continued
	 * source line
	 */
	if (line.length() > 72){
	    return ("X" + line.substring(0,72)).trim().substring(1);  //RPI124
	} else {
		return ("X" + line).trim().substring(1);
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
	boolean single_quote = false;  // RPI115
	while (parm_match.find()){
		switch (parm_match.group().charAt(0)){
			case ',':
				index = parm_match.start();
				if (!single_quote 
						&& index > 0 
						&& line.length() > index+1
						&& line.charAt(index+1) <= asc_space_char){  //RPI181
					return line.substring(0,index+1);
				}
				break;
			case '\'': // single quote found 
				if (parm_match.group().length() == 1){
	                single_quote = true;
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
	 * Notes:
	 *   1.  tz390 opcode table used to find
	 *       mz390 opcodes over 200.
	 */
	if (bal_op == null || bal_op.length() == 0){
		return -1;
	}
	int index = tz390.find_key_index(("O:" + bal_op).toUpperCase());
    if (index > 0 && tz390.op_type[index] > 200){
    	return tz390.op_type[index];
    } else {
    	return -1;
    }
}
private void exec_mac_op(){
	/*
	 * execute macro operation (set,aif, ago, etc.)
	 * 
	 * Note case index values must match
	 * mac_op_name array values.
	 */
	bal_op_ok = false;
	switch (mac_op_index){
	case 201:  // ACTR  
		bal_op_ok = true;
		actr_count = calc_seta_exp(bal_parms,0);
		break;
    case 202:  // AGO 
       	bal_op_ok = true;
	    actr_count--;
	    old_mac_line_index = mac_line_index;
    	mac_line_index = get_label_index(bal_parms);
    	if (mac_line_index <= 0){
    		mac_line_index = old_mac_line_index;
    		abort_error(16,mac_name[mac_name_index] + " undefined " + bal_parms);
    	}
    	break;
    case 203:  // AIF  
       	bal_op_ok = true;
    	if (calc_setb_exp(bal_parms,0) != 0){
		    actr_count--;
		    old_mac_line_index = mac_line_index;
    		mac_line_index = get_label_index(bal_parms.substring(exp_next_index));
        	if (mac_line_index <= 0){
        		mac_line_index = old_mac_line_index;
        		abort_error(16,"macro label not found - " + bal_parms.substring(exp_next_index));
        	}
    	}
    	break;
    case 204:  // AINSERT
    	break;
    case 205:  // ANOP 
       	bal_op_ok = true;
    	break;
    case 206:  // AREAD
    	bal_op_ok = true;
    	if (get_setc_target()){
    		String dat_line = get_aread_string();
    		put_setc_string(dat_line);
    	}
    	break;
    case 207:  // GBLA 
       	bal_op_ok = true;
    	exp_parse_set_loc = gbl_loc;
    	exp_parse_set_type = var_seta_type;
    	alloc_set();
    	break;
    case 208:  // GBLB 
       	bal_op_ok = true;
    	exp_parse_set_loc = gbl_loc;
    	exp_parse_set_type = var_setb_type;
    	alloc_set();
    	break;
    case 209:  // GBLC
       	bal_op_ok = true;
    	exp_parse_set_loc = gbl_loc;
    	exp_parse_set_type = var_setc_type;
    	alloc_set();
    	break;
    case 210:  // LCLA
       	bal_op_ok = true;
    	exp_parse_set_loc = lcl_loc;
    	exp_parse_set_type = var_seta_type;
    	alloc_set();
    	break;
    case 211:  // LCLB
       	bal_op_ok = true;
    	exp_parse_set_loc = lcl_loc;
    	exp_parse_set_type = var_setb_type;
    	alloc_set();
    	break;
    case 212:  // LCLC
       	bal_op_ok = true;
    	exp_parse_set_loc = lcl_loc;
    	exp_parse_set_type = var_setc_type;
    	alloc_set();
    	break;
    case 213:  // MHELP 
    	break;
    case 214:  // MNOTE  RPI 238
       	bal_op_ok = true;
   		tot_mnote++;
    	bal_line = replace_vars(bal_line,false);
        int mnote_level = 0;
    	if (bal_parms.length() > 0 
            && bal_parms.charAt(0) != '\''
            && bal_parms.charAt(0) != ','
            && bal_parms.charAt(0) != '*'){
            mnote_level = calc_seta_exp(bal_parms,0);
    	}
    	if (mnote_level > max_mnote_level){
    		max_mnote_level = mnote_level;
    	}
    	put_bal_line(bal_line);
    	break;
    case 215:  // SETA 
       	bal_op_ok = true;
       	exp_parse_set_type = var_seta_type;
       	if (get_seta_target()){
       		while (bal_parms != null){
       			store_seta_value = calc_seta_exp(bal_parms,0);
       			put_seta_value(store_seta_value);
       			if (bal_parms.length() >= exp_next_index 
       				&& bal_parms.charAt(exp_next_index-1) == ','){
       				bal_parms = bal_parms.substring(exp_next_index);
       				store_seta_index++;
       				if (store_seta_index >= store_max_index){
                        log_error(118,"seta multiple values beyond linit");
       				} 
       			} else {
       				bal_parms = null;
       			}
       		}
       	}
    	break;
    case 216:  // SETAF
    	break;
    case 217:  // SETB 
       	bal_op_ok = true;
       	exp_parse_set_type = var_setb_type;
       	if (get_setb_target()){
       		while (bal_parms != null){
       			store_setb_value = calc_setb_exp(bal_parms,0);
       			put_setb_value(store_setb_value);
       			if (bal_parms.length() >= exp_next_index 
   					&& bal_parms.charAt(exp_next_index-1) == ','){
       				bal_parms = bal_parms.substring(exp_next_index);
       				store_setb_index++;
       				if (store_setb_index >= store_max_index){
                        log_error(118,"setb multiple values beyond linit");
       				} 
       			} else {
       				bal_parms = null;
       			}
       		}
       	}
    	break;
    case 218:  // SETC  
       	bal_op_ok = true;
       	exp_parse_set_type = var_setc_type;
       	if (get_setc_target()){
       		while (!mac_abort && bal_parms != null){
       			String setc_string = calc_setc_exp(bal_parms,0);
       			put_setc_string(setc_string);
                if  (bal_parms.length() >= exp_next_index
                	&& bal_parms.charAt(exp_next_index-1) == ','){
           			bal_parms = bal_parms.substring(exp_next_index);
           			store_setc_index++;
           			if (store_setc_index >= store_max_index){
                        log_error(117,"setc multiple values beyond linit");
           			} 
           		} else {
           			bal_parms = null;
           		}
       		}
       	}
    	break;
    case 219:  // SETCF
    	break;
    case 220:  // MACRO
    	load_type = load_mac_inline;
    	load_mac();
    	bal_op_ok = true;
    	break;
    case 221:  // MEND
    	break;
    case 222:  // MEXIT 
    	mac_line_index = mac_name_line_end[mac_call_name_index[mac_call_level]] - 1;
    	bal_op_ok = true;
    	break;
    case 223:  // PUNCH
    	bal_op_ok = true;
    	bal_parms = replace_vars(bal_parms,false);
    	put_pch_line(bal_parms);
   	    put_bal_line(bal_line);
    	break;
    case 224:  // COPY
    	bal_op_ok = true;
   	    put_bal_line(bal_line);
    	if (!loading_mlc){
    	   mac_parms = bal_parms;
    	   open_mac_copy_file(); // issues error if not found else just list
    	}
    	break;
    case 225:  // OPSYN
    	bal_op_ok = true;
    	put_bal_line(bal_line);
    	tz390.opsyn_opcode_update(bal_label,bal_parms);
    	break;
	default: 
	  	 abort_error(68,"invalid case index - " + mac_op_index);
	}
	if (!bal_op_ok){
		put_bal_line(bal_line);
		abort_error(47,"macro operation not supported - " + bal_op);
	}
}
private void alloc_set(){
	/*
	 * parse set scalar,array, created set variables
	 * exp_parse_set_loc = lcl_set | gbl_set
	 * exp_parse_set_type = var_seta_type| var_setb_type | var_setc_type
	 */
	exp_alloc_set_mode = true; //RPI126
	String text = bal_parms;
    int index = 0;
    while (!mac_abort
    		&& index < text.length()
    		&& text.charAt(index) > asc_space_char){  // RPI 139
    	if (text.charAt(index) == ','){
    		index++;
    	}
    	if (index < text.length() && text.charAt(index) != '&'){
    		
    		text = "&" + text.substring(index);
    	} else {
    		text = text.substring(index);
    	}
    	index = 0;
   		if (!parse_set_var(text,index) 
   			|| (exp_parse_set_loc == lcl_loc
   				&& var_loc == gbl_loc)){  //RPI178
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
   		} else if (exp_parse_set_type != var_type){  //RPI178
   			log_error(107,"set type conflict for - " + text.substring(index));
   		}
   		index = exp_next_index;
    }
    exp_alloc_set_mode = false; // RPI126
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
private boolean get_seta_target(){
	/*
	 * set seta store info form bal_label
	 * and return true if ok else false
	 */
	if (parse_set_var(bal_label,0)){
    	if (var_type != var_seta_type){
        	log_error(17,"invalid set variable type for - " + bal_label);
        	return false;
    	}
	    store_loc = var_loc;
	    store_name_index = var_name_index;
	    store_seta_index = seta_index;
	    if (var_loc == lcl_loc){
	    	store_min_index = lcl_set_start[var_name_index];
	    	store_max_index = lcl_set_end[var_name_index];
	    } else {
	    	store_min_index = gbl_set_start[var_name_index];
	    	store_max_index = gbl_set_end[var_name_index];
	    }
	} else { 
  		if (set_sub != 1){
			log_error(64,"subscripted seta not defined - " + set_name);
			return false;
		} 
        add_lcl_set(exp_parse_set_name,var_seta_type,1);
		store_loc = lcl_loc;
		store_name_index = var_name_index;
		store_seta_index = seta_index;
		store_max_index = seta_index+1;
	}
	return true;
}
private boolean get_setb_target(){
	/*
	 * set setb store info form bal_label
	 * and return true if ok else false
	 */
	if (parse_set_var(bal_label,0)){
   	    if (var_type != var_setb_type){
       		log_error(17,"invalid setb variable type for - " + bal_label);
       		return false;
   		}
	    store_loc = var_loc;
	    store_name_index = var_name_index;
	    store_setb_index = setb_index;
	    if (var_loc == lcl_loc){
	    	store_max_index = lcl_set_end[var_name_index];
	    } else {
	    	store_max_index = gbl_set_end[var_name_index];
	    }
	} else {
		if (set_sub != 1){
			log_error(64,"subscripted setc not defined - " + set_name);
			return false;
		}
        add_lcl_set(exp_parse_set_name,var_setb_type,1);
		store_loc = lcl_loc;
		store_name_index = var_name_index;
		store_setb_index = setb_index;
		store_min_index = setb_index;
		store_max_index = setb_index+1;
	}
	return true;
}
private boolean get_setc_target(){
	/*
	 * set setc store info from bal_label
	 * and return true if ok else false
	 */
	if (parse_set_var(bal_label,0)){
		if (var_type != var_setc_type){
	    	log_error(17,"invalid set variable type for - " + bal_label);
	    	return false;
		}
	    store_loc = var_loc;
	    store_name_index = var_name_index;
	    store_setc_index = setc_index;
	    if (var_loc == lcl_loc){
	    	store_min_index = lcl_set_start[var_name_index];
	    	store_max_index = lcl_set_end[var_name_index];
	    } else {
	    	store_min_index = gbl_set_start[var_name_index];
	    	store_max_index = gbl_set_end[var_name_index];
	    }
	} else {
		if (set_sub != 1){
			log_error(64,"subscripted set not defined - " + set_name);
			return false;
		}
        add_lcl_set(exp_parse_set_name,var_setc_type,1);
		store_loc = lcl_loc;
		store_name_index = var_name_index;
		store_setc_index = setc_index;
		store_min_index = setc_index;
		store_max_index = setc_index+1;
	}
	return true;
}
private void put_seta_value(int seta_value){
	/*
	 * store seta string at store loc set by
	 * get_setc_target
	 */
	if  (store_loc == lcl_loc){
   	    lcl_seta[store_seta_index] = seta_value;
   	    if (tz390.opt_tracem){
   	    	put_log("TRACE SETA " + lcl_set_name[store_name_index] + "(" + (store_seta_index - store_min_index + 1) + ")= " + lcl_seta[store_seta_index]);
   	    }
	} else {
	    gbl_seta[store_seta_index] = seta_value;
   	    if (tz390.opt_tracem){
   	    	put_log("TRACE SETA " + gbl_set_name[store_name_index] + "(" + (store_seta_index - store_min_index + 1) + ")= " + gbl_seta[store_seta_index]);
   	    }
	}
}
private void put_setb_value(int setb_value){
	/*
	 * 
	 */
    if  (store_loc == lcl_loc){
   	    lcl_setb[store_setb_index] = store_setb_value;
   	    if (tz390.opt_tracem){
   	    	put_log("TRACE SETB " + lcl_set_name[store_name_index] + "(" + (store_setb_index - store_min_index + 1) + ")= " + lcl_setb[store_setb_index]);
   	    }
    } else {
	    gbl_setb[store_setb_index] = store_setb_value;
   	    if (tz390.opt_tracem){
   	    	put_log("TRACE SETB " + gbl_set_name[store_name_index] + "(" + (store_setb_index - store_min_index + 1) + ")= " + gbl_setb[store_setb_index]);
   	    }
    }
}
private void put_setc_string(String setc_string){
	/*
	 * store setc string at store loc set by
	 * get_setc_target
	 * (used by setc and aread)
	 */

	if  (store_loc == lcl_loc){
   	    lcl_setc[store_setc_index] = setc_string;
   	    if (tz390.opt_tracem){
   	    	put_log("TRACE SETC " + lcl_set_name[store_name_index] + "(" + (store_setc_index - store_min_index + 1) + ")= " + lcl_setc[store_setc_index]);
   	    }
	} else {
	    gbl_setc[store_setc_index] = setc_string;
   	    if (tz390.opt_tracem){
   	    	put_log("TRACE SETC " + gbl_set_name[store_name_index] + "(" + (store_setc_index - store_min_index + 1) + ")= " + gbl_setc[store_setc_index]);
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
	 * 2.  DSNAME= is extention to HLL assembler
	 *     where macro variable defines file
	 *     to read for AREAD.
	 * Notes:
	 *   1.  Only DDNAME or DSNAME can be coded
	 */
	String file_name = null;
	if (bal_parms.length() > 6){ 
		// read from file using DDNAME= or DSNAME=
		bal_parms = replace_vars(bal_parms,false); // support DSNAME=&var etc.
		if (bal_parms.substring(0,7).toUpperCase().equals("DDNAME=")){
	    	String ddname = bal_parms.substring(7);
	    	file_name = get_ddname_file_name(ddname);
	    	file_name = tz390.get_file_name(tz390.dir_dat,file_name,".DAT");
	    } else if (bal_parms.substring(0,7).toUpperCase().equals("DSNAME=")){
	    	file_name = bal_parms.substring(7);
	    	file_name = tz390.get_file_name(tz390.dir_dat,file_name,".DAT");
	    }
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
			if (text == null){
				dat_file_buff.close();
				dat_file = null;
				return "";
			} else {
				text = trim_line(text);
				if (!tz390.verify_ascii_source(text)){
					abort_error(141,"invalid ascii source line " + cur_mac_line_num + " in " + dat_file.getPath());
				}
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
		return temp_file.getPath();
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
	 * 2.  If ,DSNAME= follows 'text' write to
	 *     specified file instead of default 
	 *     filename.pch
	 *     
	 */
	String file_name = null;
	String pch_text = "";
	String token = null;
    pch_match = pch_pattern.matcher(pch_parms.substring(1));
    boolean pch_eod = false;
	while (!pch_eod && pch_match.find()){
	       token = pch_match.group();
	       if (token.charAt(0) != '\'' && token.charAt(0) != '&'){ //RPI195
	       	  pch_text = pch_text + token;
	       } else if (token.length() == 2){
	       	  pch_text = pch_text + token.charAt(0);
	       } else if (token.charAt(0) == '\'') {
	       	  pch_eod = true;
	       } else {
		      pch_text = pch_text + token.charAt(0);
	       }
	}
	if (!pch_eod){
		log_error(73,"invalid punch parm " + pch_parms);
	} else {
		int index = pch_match.end()+1;
		if (pch_parms.substring(index).length() > 8){
		   if (pch_parms.substring(index,index+8).toUpperCase().equals(",DDNAME=")){
			   String ddname = pch_parms.substring(index+8);
			   file_name = get_ddname_file_name(ddname);
			   file_name = tz390.get_file_name(tz390.dir_pch,file_name,".PCH");
		   } else if (pch_parms.substring(index,index+8).toUpperCase().equals(",DSNAME=")){
			    file_name = bal_parms.substring(index+8);
			    file_name = tz390.get_file_name(tz390.dir_pch,file_name,".PCH");
		   }
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
		       pch_file_name = tz390.dir_pch + tz390.pgm_name + ".PCH";
		       open_pch_file();
		    }
		}
	    try {
	       pch_text = ("X" + pch_text).trim().substring(1); //RPI 195
		   pch_file_buff.write(pch_text + "\r\n");
	       if (pch_file.length() > tz390.max_file_size){
	    	   abort_error(120,"maximum pch file size exceeded");
	       }
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
	   if (exp_text == null || exp_start_index >= exp_text.length()){
		   log_error(128,"invalid exp_calc text index - " + exp_text + "(" + exp_start_index + ")");
	   }
	   exp_text_len   = text.length();
       exp_match = exp_pattern.matcher(text.substring(exp_start_index));
       tot_exp_stk_var = 0;
       tot_exp_stk_op  = 0;
   	   exp_level = 0;
   	   exp_next_first = '?';  // not space or exp_term_op
       exp_end = false;
       exp_ok  = false;
   	   exp_var_pushed = false;     // reset var pused for unary 
       var_subscript_calc = false; // reset explicit subscript
   	   exp_prev_substring = false;
       exp_set_prev_op();
       exp_set_next_op();
       while (!exp_end && !mac_abort){ 
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
	 * 2.  push ordinary symbols starting with A-Z$@#_
	 *     as SDT string assuming there preceding T' type oper
	 */
    exp_var_last  = true; // force first try
	while (!exp_end && exp_var_last){
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
	    exp_next_op = exp_token.toUpperCase();
	    exp_next_first = exp_next_op.charAt(0);
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
	     	} else if (exp_token.equals("&&") 
	     			   && (exp_prev_first == exp_string_op
	     			       || exp_prev_first == exp_create_set_op)){
	     		// RPI192 substitute & for && in setc strings
	     		exp_stk_setc[tot_exp_stk_var - 1] = exp_stk_setc[tot_exp_stk_var - 1].concat("&");
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
            //	if ?' then split into last char and end quote
	    	if (exp_token.length() >= 2 && exp_token.charAt(1) == '\''){ 
	    		exp_stk_setc[tot_exp_stk_var - 1] = exp_stk_setc[tot_exp_stk_var - 1].concat(exp_token.substring(0,1));
	    		exp_next_class = exp_class_str_sub1;
	    		if (exp_token.length() > 2){  //RPI129
	    			exp_start_index = exp_start_index + exp_next_index - exp_token.length() + 2;
	    			exp_match = exp_pattern.matcher(exp_text.substring(exp_start_index));
	    		}
	    	} else { // concatenate token string chars
	    	    exp_stk_setc[tot_exp_stk_var - 1] = exp_stk_setc[tot_exp_stk_var - 1].concat(exp_token);
	    	    exp_var_last = true;
	    	}
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
	 * set op from exp_next_op
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
	       	if (exp_var_pushed == false  //RPI171
	       		&& exp_prev_first != exp_string_op
	       		&& exp_prev_first != exp_create_set_op){
	       		exp_token = "U" + exp_token;     //RPI171 make unary oper
	       		exp_next_class = exp_class_oper; //RPI171
	       		exp_next_first = 'U';
	       	} else {
	       		exp_next_class = exp_class_add_sub;
	       	}
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
	       case ' ':   //asc_space_char
	       case '\t':  //tab
	       case '\r':  //cr
	       case '\n':  //lf
	       	exp_next_index--; //RPI181 backup to space
	       case '~':
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
	   	 	    if (exp_next_op.length() > 2 && exp_next_op.charAt(exp_next_op.length()-1) == '\''){
	   	            exp_push_sdt(exp_token);
	   	            exp_var_last = true;
	    	    } else {
	    	    	push_sym();
	    	    	exp_var_last = true;
	    	    }
	    	 	break;
	    	 case 'C':
	    	 	if (exp_next_op.length() > 2 
	    	 		&& (exp_next_op.charAt(exp_next_op.length()-1) == '\''      //RPI 270 CA'..' or CE'..'
	    	 			|| exp_next_op.charAt(exp_next_op.length()-1) == '"'    //RPI5
    	 			    || exp_next_op.charAt(exp_next_op.length()-1) == '!')){ //RPI84
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
	    	 	} else if (exp_next_op.equals("LOWER")){ //RPI 196
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
	    	 case 'U':
		    	if (exp_next_op.equals("UPPER")){
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
	 * Notes:
	 *   1.  If substring set prev_substring else
	 *       reset after operation.  Used by exp_substring. RPI 214
	 */
    if (tz390.opt_traceall){
	        put_log("TRACE EXP OPS=" + tot_exp_stk_op + " VARS=" + tot_exp_stk_var + " PREV OP = " + exp_prev_op +  " NEXT OP = " + exp_token);
    }
	if  (exp_prev_class == 0){
		log_error(37,"invalid expression operator class for - " + exp_token);
	}
	if  (exp_next_class == 0){
		log_error(37,"invalid expression operator class - " + exp_token);
	}
    int action = exp_action[tot_classes*(exp_prev_class-1)+exp_next_class-1];
    if (tz390.opt_traceall){
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
  	   } else if (exp_next_first == '\''){  //RPI99
  		   exp_level++;
  		   exp_push_string("");
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
        *    put ' op
        *    if not prev_substring then  RPI 214
        *       put null string on stack
        *    reset prev_substring
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
    	if (exp_next_char() == '*'){  // RPI 257
    		exp_push_sdt(max_substring_len);
    		skip_next_token();
    	}
       break;
    case 10: // ,e2) substring e2
       /*
        * replace string with substring
        */
    	exp_substring();
    	break;
    case 11: // replace &var(sub) with value
    	exp_var_subscript();
    	exp_var_pushed = true; // prevent unary minus
    	break;
    case 12: // prefix operator ?'
    	exp_pop_op();
    	switch (exp_prev_first){
    	case 'D': // DOUBLE
    		if (tot_exp_stk_var > 0){
    			setc_value = get_setc_stack_value().replaceAll("\\'","\\'\\'").replaceAll("\\&","\\&\\&"); //RPI195
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
    	case 'L':
    		if (exp_stk_op[tot_exp_stk_op].equals("L'")){// L'sym returns length attribute
    			if (tot_exp_stk_var > 0){
    				setc_value = get_setc_stack_value();
    				seta_value1 = get_sym_len(setc_value);
    				put_seta_stack_var();
    			} else {
    				log_error(67,"missing variable for L' operator");
    			}
   			} else { // LOWER  RPI196
   				setc_value = get_setc_stack_value();
   				setc_value = setc_value.toLowerCase();
   				put_setc_stack_var();
   			}
    		break;
    	case 'N': // N'var returns sublist count
    		if (tot_exp_stk_var > 0){
    			if (exp_stk_type[tot_exp_stk_var - 1] == var_sublist_type
    					&& exp_stk_setb[tot_exp_stk_var - 1] == syslist_loc
						&& exp_stk_seta[tot_exp_stk_var - 1] == -1
						&& mac_call_level > 0){
    				seta_value1 = mac_call_pos_tot[mac_call_level];
    				tot_exp_stk_var--; // remove syslist var
    				put_seta_stack_var();
    			} else {
    				setc_value = get_setc_stack_value();
    				seta_value1 = get_sublist_count(setc_value);
    				put_seta_stack_var();
    			}
    			exp_var_pushed = true; // prevent unary minus
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
    		break;
    	case 'U': 
    		if (exp_stk_op[tot_exp_stk_op].equals("UPPER")){ //UPPER RPI196
				setc_value = get_setc_stack_value();
				setc_value = setc_value.toUpperCase();
				put_setc_stack_var();
    		} else {
    			exp_unary_op();
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
	    log_error(38,"expression parsing error - prev op =" + exp_prev_first + " next op =" + exp_next_first); // RPI 260
    }
    if (action == 10){
    	exp_prev_substring = true;
    } else {
    	exp_prev_substring = false;
    }
}
private void exp_unary_op(){
	/*
	 * execute unary operator U+ or U-
	 */
	if (exp_stk_op[tot_exp_stk_op].charAt(1) == '-'){
		if (exp_var_pushed){
			switch (exp_stk_type[tot_exp_stk_var-1]){
			case 1:
				exp_stk_seta[tot_exp_stk_var-1] = - exp_stk_seta[tot_exp_stk_var -1];
				break;
			case 2:
				exp_stk_setb[tot_exp_stk_var-1] = (byte) - exp_stk_setb[tot_exp_stk_var -1];
				break;
			case 3:
				exp_stk_seta[tot_exp_stk_var-1] = - get_seta_stack_value(-1);
			    exp_stk_type[tot_exp_stk_var-1] = var_seta_type;
			    break;
			}
		} else if (exp_stk_op[tot_exp_stk_op].charAt(0) == 'U'){
			if (exp_token.charAt(1) == exp_stk_op[tot_exp_stk_op].charAt(1)){
				exp_token = "U+";
			} else {
				exp_token = "U-";
			}
		} else {
			log_error(124,"missing unary operator value");
		}
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
	if (inc_tot_exp_stk_var()){
		exp_stk_type[tot_exp_stk_var - 1] = var_setc_type;
		exp_stk_setc[tot_exp_stk_var - 1] = setc_value1;
	}
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
	if (inc_tot_exp_stk_var()){
		exp_stk_type[tot_exp_stk_var - 1] = var_seta_type;
		exp_stk_seta[tot_exp_stk_var - 1] = seta_value1;
	}
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
	if (inc_tot_exp_stk_var()){
		exp_stk_type[tot_exp_stk_var - 1] = var_seta_type;
		exp_stk_seta[tot_exp_stk_var - 1] = seta_value1;
	}

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
	if (tz390.opt_ascii){
		while (index < len_comp
				&& setc_value1.charAt(index)
				== setc_value2.charAt(index)){
				index++;
			}
	} else {
		while (index < len_comp
			&& tz390.ascii_to_ebcdic[setc_value1.charAt(index)]
			== tz390.ascii_to_ebcdic[setc_value2.charAt(index)]){
			index++;
		}
	}
	if (index < len_comp){
		if (tz390.opt_ascii){
			if (setc_value1.charAt(index)
			    > setc_value2.charAt(index)){
				return 1;
			} else {
				return -1;
			}
		} else {
			if (tz390.ascii_to_ebcdic[setc_value1.charAt(index)]
			    > tz390.ascii_to_ebcdic[setc_value2.charAt(index)]){
				return 1;
			} else {
				return -1;
			}
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
		  if (exp_stk_setb[tot_exp_stk_var - 1] != 1){ //RPI147
			  exp_stk_setb[tot_exp_stk_var - 1] = 1;
		  } else {
			  exp_stk_setb[tot_exp_stk_var - 1] = 0;
		  }
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
	 * Note:
	 *   1. if exp_prev_substring_op set then 
	 *      don't put null string on stack
	 */
	 if (exp_prev_first != exp_string_op){
        exp_level++;         // add substring extra level to handel spaces
        exp_push_op();       // push exp_string_op
        if (!exp_prev_substring){    // RPI 214
        	exp_push_string(""); // push empty string val
        }
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
	var_subscript_calc = true;
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
		   	   exp_check_prev_op = false;  //RPI60
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
	var_subscript_calc = false;
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
	if (inc_tot_exp_stk_var()){
        exp_stk_type[tot_exp_stk_var - 1] = var_setb_type;
        if (compare_result){
		   exp_stk_setb[tot_exp_stk_var - 1] = 1;
        } else {
           exp_stk_setb[tot_exp_stk_var - 1] = 0;
        }
        if (tz390.opt_traceall){
        	put_log("TRACE LOGICAL COMPARE = " + exp_stk_setb[tot_exp_stk_var -1]);
        }
	} else {
		log_error(36,"expression compare error");
	}
}
private boolean inc_tot_exp_stk_var(){
	/*
	 * inc tot_exp_stk_var if ok else
	 * abort and return false
	 */
	if (tot_exp_stk_var < max_exp_stk){
		exp_var_pushed = true;
		tot_exp_stk_var++;
		return true;
	} else {
		abort_error(19,"exp variable stack exceeded");
		return false;
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
	if (inc_tot_exp_stk_var()){
        exp_stk_type[tot_exp_stk_var - 1] = var_seta_type;
		exp_stk_seta[tot_exp_stk_var - 1] = seta_value1;
	}
}
private void put_setc_stack_var(){
	/*
     * add setc_value to stack 
	 */
	if (inc_tot_exp_stk_var()){
        exp_stk_type[tot_exp_stk_var - 1] = var_setc_type;
		exp_stk_setc[tot_exp_stk_var - 1] = setc_value;
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
	  exp_var_pushed = false;  //RPI171
	  if (tot_exp_stk_op >= max_exp_stk){
		  abort_error(44,"maximum stack operations exceeded");
		  return;
	  }
	  if (tz390.opt_traceall){
		  put_log("TRACE PUSHING OP - " + exp_token);
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
	  log_error(35,"expression parsing error - total stack values=" + tot_exp_stk_var + "  total ops=" + tot_exp_stk_op); // RPI 260
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
		if (base == 10){
			int index = 0;
			int value = 0;
			while (index < number.length()){
				if (number.charAt(index) >= '0' && number.charAt(index) <= '9'){
					value = value*10 + number.charAt(index) - 0x30;
				} else {
					return value;
				}
				index++;
			}
			return value;
		} else {
			log_error(123,"invalid hex string - " + number);
			return 0;
		}
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
	if (tz390.opt_traceall){
		put_log("TRACE PUSHING VAR - " + exp_token);
	}
    if (find_var(exp_token)){  // find set or parm var
       if (exp_next_char() == '('
    	   && inc_tot_exp_stk_var()){
       	  if (var_type == var_parm_type){
              exp_stk_type[tot_exp_stk_var - 1] = var_sublist_type;
          } else {
              exp_stk_type[tot_exp_stk_var - 1] = var_subscript_type;
          }
          skip_next_token();
          exp_token = "" + exp_subscript_op;
          exp_next_first = exp_subscript_op;
          exp_next_class = exp_class_cls_sub;
          exp_push_op();  // push ) subscript/sublist op
     	  exp_level++;
          exp_stk_setb[tot_exp_stk_var - 1] = var_loc;         // set/parm loc
      	  exp_stk_seta[tot_exp_stk_var - 1] = var_name_index;  // set/sub subscript
      	  exp_stk_setc[tot_exp_stk_var - 1] = setc_value;      // sublist parm
       } else {
       	  if (exp_prev_first == exp_string_op
       		  || exp_prev_first == exp_create_set_op){
       		  if (tz390.opt_traceall){
       			  put_log("TRACE STRING CONCAT - " + exp_token);
       		  }
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
       	  } else if (inc_tot_exp_stk_var()){
	          switch (var_type){
	          case 1:
	   	          exp_stk_seta[tot_exp_stk_var - 1] = seta_value;
	   	          break;
	          case 2:
	   	          exp_stk_setb[tot_exp_stk_var - 1] = setb_value;
	   	          break;
	   	      case 4:  // push parm as setc string
	   	      	  var_type = var_setc_type;
	          case 3:  // push setc for parm and setc
	   	          exp_stk_setc[tot_exp_stk_var - 1] = setc_value;
	   	          break;
	   	      case 6:  // syslist
	   	      	  exp_stk_setb[tot_exp_stk_var - 1] = var_loc;
	   	      	  exp_stk_seta[tot_exp_stk_var - 1] = var_name_index;
	   	      	  break;
			  default: 
			  	 abort_error(68,"invalid case index");
	          }
	          exp_stk_type[tot_exp_stk_var - 1] = var_type;
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
	    if (tz390.opt_traceall){
		   put_log("TRACE PUSHING SDT - " + sdt);
	    }
		if (inc_tot_exp_stk_var()
		    && ((sdt.length() > 1
		    	 && (sdt.charAt(sdt.length()-1) == '\'' // RPI 270
			   	    || sdt.charAt(sdt.length()-1) == '"'   //RPI5
	    		    || sdt.charAt(sdt.length()-1) == '!')
	    		 )  //RP84		    	  
	    		 ||   (sdt.charAt(0) <= '9'
		        	  && sdt.charAt(0) >= '0')
		       )
		   ){
           exp_stk_type[tot_exp_stk_var - 1] = var_seta_type;
           switch (sdt.substring(0,1).toUpperCase().charAt(0)){
           case 'B': // B'11000001' binary
        	   exp_stk_seta[tot_exp_stk_var-1] = get_int_from_string(sdt.substring(2,sdt.length()-1),2);
           	   break;
           case 'C': // RPI192 C'..'|C".."|C!..! char sdt 
           	   if (!tz390.get_sdt_char_int(sdt)){
           		   log_error(129,"invalid character sdt " + sdt);
           	   }
        	   exp_stk_seta[tot_exp_stk_var-1] = tz390.sdt_char_int; 
           	   break;
           case 'X': // X'C1' hex
           	   exp_stk_seta[tot_exp_stk_var-1] = Long.valueOf(sdt.substring(2,sdt.length()-1),16).intValue();
           	   break;
           default:  // must be ascii number
               exp_stk_seta[tot_exp_stk_var-1] = get_int_from_string(sdt,10);
               break;
           }
	   } else {  // push ordinary symbol for T',L'
           exp_stk_type[tot_exp_stk_var -1] = var_setc_type;
           exp_stk_setc[tot_exp_stk_var -1] = sdt;
	   }
}
private void push_sym(){
	/*
	 * push current exp_token symbol  on stack
	 * as setc for use by prefix operators T', L'
	 */
	exp_push_string(exp_token);
}
private void set_sym_type_len(String sym_lab,String sym_op,String sym_parms){
	/*
	 * set symbol type and length 
	 * Notes:
	 *   1.  Called during macro load to define all
	 *       symbols in open code allowing forward
	 *       reference during macro execution.
	 *   2.  Called again during BAL output to define
	 *       generated symbols and to correctly 
	 *       resolve variable symbol substitution
	 *       in open code symbol statements that 
	 *       could change type and length in which
	 *       case these cannot be forward referenced.
	 * 
	 * Notes:
	 *   1.  If no type found, ignore symbol.
	 */
	 int parm_len = sym_parms.length();
     int parm_index = 0;
     int parm_level = 0;
     char parm_char = 'U';
     cur_sym = tz390.find_key_index("S:" + sym_lab);
     if (cur_sym == -1
         && tot_sym < max_sym){
		 cur_sym = tot_sym;
		 tot_sym++;
		 if (!tz390.add_key_index(cur_sym)){
			abort_error(87,"key search table exceeded");
		 }
         sym_name[cur_sym] = sym_lab;
         sym_type[cur_sym] = 'U';
         sym_len[cur_sym]  = 1;
     }
	 if (sym_op.toUpperCase().equals("DC")
   		 || sym_op.toUpperCase().equals("DS")){
       	 while (parm_index < parm_len){
        	 parm_char = sym_parms.charAt(parm_index);
     	     if (parm_char == '('){
   		         parm_level++;
             } else if (parm_char == ')'){
   		         parm_level--;
             } else if (parm_level == 0 
        		 && (parm_char < '0'
        		     || parm_char > '9')){
                 sym_type[cur_sym] = parm_char;
                 sym_etype[cur_sym] = ' ';
                 if (parm_index < parm_len -1){
                	 sym_etype[cur_sym] = get_sym_etype(sym_parms.charAt(parm_index+1));
                	 if (sym_etype[cur_sym] != ' '){
                		 parm_index++;
                	 }
                 }
                 if (parm_index < parm_len -2
        		      && sym_parms.charAt(parm_index +1) == 'L'){
                	  // explicit length
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
        			  parm_index = tz390.dc_valid_types.indexOf(sym_type[cur_sym]);
        			  if (parm_index != -1){
        				  sym_type[cur_sym] = tz390.dc_type_explicit.charAt(parm_index);
        			  }
        		  } else if (parm_index == sym_parms.length() - 1
        				     || sym_parms.charAt(parm_index + 1) == ','
        				     || (parm_char != 'C' 
        				    	 && parm_char != 'P'
        				    	 && parm_char != 'Z')){
        			  // no data so use default length
        			  parm_index = tz390.dc_valid_types.indexOf(sym_type[cur_sym]);
        			  if (parm_index != -1){
        				  sym_len[cur_sym] = tz390.dc_type_len[parm_index];
        			  } else {
        				  sym_type[cur_sym] = 'U';
        				  sym_len[cur_sym] = 1;
        			  }
        		  } else {
        			  if (parm_char == 'C'){
        				  int chars = get_sym_char_data_len(sym_parms.substring(parm_index+1));
        				  if (chars != -1){
        					  sym_len[cur_sym] = chars;
        				  } else {
        					  log_error(130,"invalid character data field " + sym_parms);
        				  }
        			  } else {
        				  int digits = get_sym_num_data_len(sym_parms.substring(parm_index+1));
        				  if (digits == -1){
        					  log_error(131,"invalid numeric data field " + sym_parms);
        				  }
        				  if (parm_char == 'P'){
        					  sym_len[cur_sym] = (digits + 1)/2;
        				  } else if (parm_char == 'Z'){
        					  sym_len[cur_sym] = digits;
        				  }
        			  }
        		  }
        		  return;
       		  }
       		  parm_index++;	  
       	}
	 } else {  // not DS/DC so check if instr
		 int op_index = tz390.find_key_index("O:" + sym_op);
		 if (op_index != -1){
			 int op_type = tz390.op_type[op_index];
			 if (op_type <= tz390.max_op_type_offset){
			     sym_type[cur_sym] = 'I';
			     sym_len[cur_sym] = tz390.op_type_len[op_type];
			 } else {
				 if (sym_op.equals("MACRO")){
					 sym_type[cur_sym] = 'M';
			     } else if (sym_op.equals("CSECT")){
					 sym_type[cur_sym] = 'J';
				 }
			 }
		 }
	 }
}
private char get_sym_type(String symbol){
	/*
	 * return type for string:
	 *   1. If symbol found, return type.
	 *   2. If null or length return 'O'
	 *   3. If numeric return 'N'
	 *   4. Else return 'N'
	 */
	if (symbol == null || symbol.length() == 0){
		return 'O';  // omitted
	}
	int cur_sym = tz390.find_key_index("S:" + symbol);
	if (cur_sym != -1){
		return sym_type[cur_sym];
	} else {
		if (symbol.charAt(0) >= '0' 
			&& symbol.charAt(0) <= '9'){ //RPI180
			return 'N';
		} else {
			return 'U';
		}
	}
}
private char get_sym_etype(char etype){
	/*
	 * if etype is AE return it else space
	 */
	switch (etype){
	case 'a':
	case 'A':
		return 'A';
	case 'e':
	case 'E':
		return 'E';
	}
	return ' ';
}
private int get_sym_num_data_len(String data){
	/*
	 * return number of digits for P or Z data
	 * return -1 if invalid
	 */
	int digits = 0;
	char eod_char = data.charAt(0);
	int index = 1;
	while (index < data.length()){
		char data_char = data.charAt(index);
		if (data_char == eod_char || data_char == ','){
            return digits;
		}
		if (data_char >= '0' && data_char <= '9'){
			digits++;
		}
		index++;
	}
	return -1;
}
private int get_sym_char_data_len(String data){
	/*
	 * return length of C'..', C"..", C!..!
	 * and only count 1 for double ' or &
	 * return -1 if invalid field
	 */
	int chars = 0;
    char eod_char = data.charAt(0);
    if (eod_char <= asc_space_char){  // RPI 239
    	return 1;
    }
    int data_len = data.length();
    char data_char = asc_space_char;
	int index = 1;
	while (index < data.length()){
		data_char = data.charAt(index);
		if (data_char == eod_char){
			if (index < data_len - 1 && data.charAt(index+1)== eod_char){
				chars++;
				index++;
			} else {
				return chars;
			}
		} else {
			if (data_char == '\'' || data_char == '&'){
				 if (index < data_len -1 && data_char == data.charAt(index+1)){
					 index++;
				 }
			}
			chars++;
		}
		index++;
	}
	return -1;
}
private int get_sym_len(String symbol){
	/*
	 * return length for ordinary symbol if found
	 * else return 1
	 */
	int cur_sym = tz390.find_key_index("S:" + symbol);
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
	if (inc_tot_exp_stk_var()){
       exp_stk_type[tot_exp_stk_var-1] = var_setc_type;
       exp_stk_setc[tot_exp_stk_var-1] = value;
	}
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
   	tot_lcl_set++;
	var_loc   = lcl_loc;
	var_type  = new_type;
	set_name  = new_name.toUpperCase();
	lcl_set_name[var_name_index] = set_name;
	lcl_set_type[var_name_index] = var_type;
	switch (var_type){
	   case 1:  // lcl seta
	   	  if (tot_lcl_seta + new_size >= max_lcl_set){
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
	   	  if (tot_lcl_setb + new_size >= max_lcl_set){
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
	   	  if (tot_lcl_setc + new_size >= max_lcl_set){
	   	  	 abort_error(46,"maximum local setc exceeded");
	   	  	 return;
	   	  }
	   	  lcl_set_start[var_name_index] = tot_lcl_setc;
	   	  setc_index = tot_lcl_setc;
	   	  tot_lcl_setc = tot_lcl_setc + new_size;
	   	  lcl_set_end[var_name_index]   = tot_lcl_setc;
	   	  setc_value = "";
	   	  while (setc_index <tot_lcl_setc){  // RPI 242
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
	 * add gbl set variable 
	 */
	if (tot_gbl_name >= max_gbl_name){
		abort_error(55,"maximum global variables exceeded");
		return;
	}
	var_name_index = tot_gbl_name;
	if (!tz390.add_key_index(var_name_index)){
		abort_error(87,"key search table exceeded");
	}
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
	 * parse scalar, subscripted, or created set
	 * variable with or without subscript using
	 * expression parser in parse_set_var_mode
	 * to set:
	 *  1.  exp_parse_set_name
	 *  2.  exp_parse_set_sub
	 * and return true if it exists or false if not.
	 * Notes:
	 *  1. If var found but exp_parse_set_name
	 *     is null, then issue error for parms
	 * 
	 */
	exp_parse_set_mode = true;
	exp_parse_set_name = null;
	exp_parse_set_sub  = 0;
	exp_parse_set_name_index = -1;
    calc_exp(text,text_index);
    if (exp_ok){
    	if (exp_parse_set_name == null){	
    		log_error(104,"set/parm variable conflict - " + text.substring(text_index));
    	}
    	return true;
    }
    return false;
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
		if (exp_parse_set_name_index == -1){
			exp_parse_set_name_index = var_name_index;
		}
		return true;
	}
	if (find_gbl_set(var_name,var_sub)){
		if (exp_parse_set_name_index == -1){
			exp_parse_set_name_index = var_name_index;
		}
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
	var_name_index = tz390.find_key_index("G:" + var_name);
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
	 * 1.  Calc seta_index|setb_index|setc_index
	 *     If subscript out of range and alloc mode
	 *     use previous ending subscript (i.e. first
	 *     allocation sets size per RPI 126).
	 * 2.  Calc seta_value|setb_value|setc_value
	 * Notes:
	 *   1.  Requires var_name_index and set_sub
	 */
	switch (var_type){
    case 1:
    	 seta_index = lcl_set_start[var_name_index] + set_sub - 1;
    	 if (seta_index >= lcl_set_end[var_name_index]
             || seta_index < lcl_set_start[var_name_index]){
       		 expand_lcl_seta(); //RPI179
       	 }
    	 seta_value = lcl_seta[seta_index];
    	 break;
    case 2:
    	 setb_index = lcl_set_start[var_name_index] + set_sub - 1;
    	 if (setb_index >= lcl_set_end[var_name_index]
    	     || setb_index < lcl_set_start[var_name_index]){
    		 expand_lcl_setb(); //RPI179
    	 }
    	 setb_value = lcl_setb[setb_index];
    	 break;
    case 3:
    	 setc_index = lcl_set_start[var_name_index] + set_sub - 1;
    	 if (setc_index >= lcl_set_end[var_name_index]
           	|| setc_index < lcl_set_start[var_name_index]){
     		 expand_lcl_setc(); //RPI179
      	 }
    	 setc_value = lcl_setc[setc_index];
    	 break;
	 default: 
	  	 abort_error(68,"invalid case index");
	}
}
private void expand_lcl_seta(){  //RPI179
	/*
	 * expand local seta array else
	 * issue error and set setb_index to last
	 * element
	 */
	 tot_expand++;
     if (set_sub < 1
    	 || tot_lcl_seta + set_sub + expand_inc > max_lcl_set){
		log_error(48,"lcl seta sub out of range - " + lcl_set_name[var_name_index] + "(" + set_sub +")");
		seta_index = lcl_set_end[var_name_index];
		return;
     }
     // move existing elements to end if not already there
     if (lcl_set_end[var_name_index] != tot_lcl_seta){
    	 int index = lcl_set_start[var_name_index];
    	 lcl_set_start[var_name_index] = tot_lcl_seta;
    	 while (index < lcl_set_end[var_name_index]){
    		 lcl_seta[tot_lcl_seta] = lcl_seta[index];
    		 index++;
    		 tot_lcl_seta++;
    	 }
    	 lcl_set_end[var_name_index] = tot_lcl_seta; 
     }
     // expand array to include set_sub + expand_inc
     tot_lcl_seta = lcl_set_start[var_name_index] + set_sub + expand_inc;
     adjust_expand_inc(lcl_loc); // grow to reduce repeats
     int index = lcl_set_end[var_name_index];
     while (index < tot_lcl_seta){
    	 lcl_seta[index] = 0; // clear expanded elements  // RPI 242
    	 index++;
     }
     lcl_set_end[var_name_index] = tot_lcl_seta;
     seta_index = lcl_set_start[var_name_index] + set_sub - 1;
}
private void expand_lcl_setb(){  //RPI179
	/*
	 * expand local setb array else
	 * issue error and set setb_index to last
	 * element
	 */
	 tot_expand++;
     if (set_sub < 1
    	 || tot_lcl_setb + set_sub + expand_inc > max_lcl_set){
		log_error(49,"lcl setb sub out of range - " + lcl_set_name[var_name_index] + "(" + set_sub +")");
		setb_index = lcl_set_end[var_name_index];
		return;
     }
     // move existing elements to end if not already there
     if (lcl_set_end[var_name_index] != tot_lcl_setb){
    	 int index = lcl_set_start[var_name_index];
    	 lcl_set_start[var_name_index] = tot_lcl_setb;
    	 while (index < lcl_set_end[var_name_index]){
    		 lcl_setb[tot_lcl_setb] = lcl_setb[index];
    		 index++;
    		 tot_lcl_setb++;
    	 }
    	 lcl_set_end[var_name_index] = tot_lcl_setb; 
     }
     // expand array to include set_sub + expand_inc
     tot_lcl_setb = lcl_set_start[var_name_index] + set_sub + expand_inc;
     adjust_expand_inc(lcl_loc); // grow to reduce repeats
     int index = lcl_set_end[var_name_index];
     while (index < tot_lcl_setb){
    	 lcl_setb[index] = 0; // clear expanded elements
    	 index++;
     }
     lcl_set_end[var_name_index] = tot_lcl_setb;
     setb_index = lcl_set_start[var_name_index] + set_sub - 1;
}
private void expand_lcl_setc(){ //RPI179
    /*
     * expand local setc array else
     * issue error and set setb_index to last
     * element
     */
	 tot_expand++;
	 if (set_sub < 1
			 || tot_lcl_setc + set_sub + expand_inc > max_lcl_set){
		 log_error(50,"lcl setc sub out of range - " + lcl_set_name[var_name_index] + "(" + set_sub +")");
		 setc_index = lcl_set_end[var_name_index];
		 return;
	 }
	 // move existing elements to end if not already there
	 if (lcl_set_end[var_name_index] != tot_lcl_setc){
		 int index = lcl_set_start[var_name_index];
		 lcl_set_start[var_name_index] = tot_lcl_setc;
		 while (index < lcl_set_end[var_name_index]){
			 lcl_setc[tot_lcl_setc] = lcl_setc[index];
			 index++;
			 tot_lcl_setc++;
		 }
		 lcl_set_end[var_name_index] = tot_lcl_setc; 
	 }
	 // expand array to include set_sub + expand_inc
	 tot_lcl_setc = lcl_set_start[var_name_index] + set_sub + expand_inc;
	 adjust_expand_inc(lcl_loc); // grow to reduce repeats
	 int index = lcl_set_end[var_name_index];
	 while (index < tot_lcl_setc){
		 lcl_setc[index] = ""; // clear expanded elements
		 index++;
	 }
	 lcl_set_end[var_name_index] = tot_lcl_setc;
	 setc_index = lcl_set_start[var_name_index] + set_sub - 1;
} 
private void expand_gbl_seta(){
	/*
	 * expand global seta array else
	 * issue error and set setb_index to last
	 * element
	 */
	 tot_expand++;
     if (set_sub < 1
    	 || tot_gbl_seta + set_sub + expand_inc > max_gbl_set){
		log_error(125,"gbl seta sub out of range - " + gbl_set_name[var_name_index] + "(" + set_sub +")");
		seta_index = gbl_set_end[var_name_index];
		return;
     }
     // move existing elements to end if not already there
     if (gbl_set_end[var_name_index] != tot_gbl_seta){
    	 int index = gbl_set_start[var_name_index];
    	 gbl_set_start[var_name_index] = tot_gbl_seta;
    	 while (index < gbl_set_end[var_name_index]){
    		 gbl_seta[tot_gbl_seta] = gbl_seta[index];
    		 index++;
    		 tot_gbl_seta++;
    	 }
    	 gbl_set_end[var_name_index] = tot_gbl_seta; 
     }
     // expand array to include set_sub + expand_inc
     tot_gbl_seta = gbl_set_start[var_name_index] + set_sub + expand_inc;
     adjust_expand_inc(gbl_loc); // grow to reduce repeats
     // note no need to clear new gbl_seta
     gbl_set_end[var_name_index] = tot_gbl_seta;
     seta_index = gbl_set_start[var_name_index] + set_sub - 1;
}
private void expand_gbl_setb(){
	/*
	 * expand global setb array else
	 * issue error and set setb_index to last
	 * element
	 */
	 tot_expand++;
     if (set_sub < 1
    	 || tot_gbl_setb + set_sub + expand_inc > max_gbl_set){
		log_error(126,"gbl setb sub out of range - " + gbl_set_name[var_name_index] + "(" + set_sub +")");
		setb_index = gbl_set_end[var_name_index];
		return;
     }
     // move existing elements to end if not already there
     if (gbl_set_end[var_name_index] != tot_gbl_setb){
    	 int index = gbl_set_start[var_name_index];
    	 gbl_set_start[var_name_index] = tot_gbl_setb;
    	 while (index < gbl_set_end[var_name_index]){
    		 gbl_setb[tot_gbl_setb] = gbl_setb[index];
    		 index++;
    		 tot_gbl_setb++;
    	 }
    	 gbl_set_end[var_name_index] = tot_gbl_setb; 
     }
     // expand array to include set_sub + expand_inc
     tot_gbl_setb = gbl_set_start[var_name_index] + set_sub + expand_inc;
     adjust_expand_inc(gbl_loc); // grow to reduce repeats
     // note no need to clear new gbl_setb
     gbl_set_end[var_name_index] = tot_gbl_setb;
     setb_index = gbl_set_start[var_name_index] + set_sub - 1;
}
private void expand_gbl_setc(){
    		/*
    		 * expand global setc array else
    		 * issue error and set setb_index to last
    		 * element
    		 */
	 tot_expand++;
	 if (set_sub < 1
			 || tot_gbl_setc + set_sub + expand_inc > max_gbl_set){
		 log_error(127,"gbl setc sub out of range - " + gbl_set_name[var_name_index] + "(" + set_sub +")");
		 setc_index = gbl_set_end[var_name_index];
		 return;
	 }
	 // move existing elements to end if not already there
	 if (gbl_set_end[var_name_index] != tot_gbl_setc){
		 int index = gbl_set_start[var_name_index];
		 gbl_set_start[var_name_index] = tot_gbl_setc;
		 while (index < gbl_set_end[var_name_index]){
			 gbl_setc[tot_gbl_setc] = gbl_setc[index];
			 index++;
			 tot_gbl_setc++;
		 }
		 gbl_set_end[var_name_index] = tot_gbl_setc; 
	 }
	 // expand array to include set_sub + expand_inc
	 tot_gbl_setc = gbl_set_start[var_name_index] + set_sub + expand_inc;
	 adjust_expand_inc(gbl_loc); // grow to reduce repeats
     int index = gbl_set_end[var_name_index];
     while (index < tot_gbl_setc){
    	 gbl_setc[index] = ""; // clear expanded elements  // RPI 242
    	 index++;
     }
	 gbl_set_end[var_name_index] = tot_gbl_setc;
	 setc_index = lcl_set_start[var_name_index] + set_sub - 1;
} 
private void adjust_expand_inc(int var_loc){
	/*
	 * increase expansion increment to reduce
	 * overhead of repeated expansions.  This
	 * is a trade-off with running out of memory
	 */
	expand_inc = expand_inc * 2;
	if (tz390.opt_traceall){
		if (var_loc == lcl_loc){
			put_log("TRACEALL EXPANSION OF LCL " + lcl_set_name[var_name_index] + "(" + (lcl_set_end[var_name_index]-lcl_set_start[var_name_index]) + ") INC=" + expand_inc);
		} else {
			put_log("TRACEALL EXPANSION OF GBL " + gbl_set_name[var_name_index] + "(" + (gbl_set_end[var_name_index]-gbl_set_start[var_name_index]) + ") INC=" + expand_inc);
		}
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
    	     expand_gbl_seta();
    	 }
    	 seta_value = gbl_seta[seta_index];
    	 break;
    case 2:
    	 setb_index = gbl_set_start[var_name_index] + set_sub - 1;
    	 if (setb_index >= gbl_set_end[var_name_index]
    	     || setb_index < gbl_set_start[var_name_index]){
    	     expand_gbl_setb();
    	 }
    	 setb_value = gbl_setb[setb_index];
    	 break;
    case 3:
    	 if (gbl_sysmac_index == gbl_set_start[var_name_index]){
    		 if (!var_subscript_calc){
    			 gbl_sysmac = mac_name[mac_call_name_index[mac_call_level]];
    		 } else if (set_sub >= 0 && set_sub <= mac_call_level){
    			 gbl_sysmac = mac_name[mac_call_name_index[mac_call_level - set_sub]];
    		 } else {
    			 gbl_sysmac = "";
    		 }
    		 setc_index = gbl_set_start[var_name_index];
    		 gbl_setc[setc_index] = gbl_sysmac;
    		 setc_value = gbl_sysmac;
    		 break;
    	 }
    	 setc_index = gbl_set_start[var_name_index] + set_sub - 1;
    	 if (setc_index >= gbl_set_end[var_name_index]
    	     || setc_index < gbl_set_start[var_name_index]){
    	     expand_gbl_setc();
    	 }
   		 if (setc_index == gbl_sysclock_index){
   			 if (tz390.opt_timing){
   				 cur_date = new Date();
   			 }
   			 gbl_setc[setc_index] = sdf_sysclock.format(cur_date);
    	 }
    	 setc_value = gbl_setc[setc_index];
    	 break;
	  default: 
	  	 abort_error(68,"invalid case index");
	}
}
private int get_label_index(String label_source){
	/*
	 * find macro label and return line index-1
	 * else abort
	 */
	label_match = label_pattern.matcher(label_source); 
	String label_name = label_source;
    if (label_match.find()){
	   label_name = label_match.group().toUpperCase();
       int    label_name_index = mac_name_lab_start[mac_name_index];
       while (label_name_index < mac_name_lab_end[mac_name_index]){
       	  if (mac_lab_name[label_name_index].equals(label_name)){
       	     if (tz390.opt_tracem){
       	    	put_log("TRACE BRANCH " + label_name);
       	     }
       	  	 return mac_lab_index[label_name_index]-1; // -1 req'd for following ++ cycle
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
	 * 2.  If MFC option on, then instructions and
	 *     assembler control statements will not be
	 *     expanded as macros.
	 * 3.  Any non-conditional macro operator can be
	 *     expanded via an inline macro that can be
	 *     defined via COPY statement.
	 */
	int index = tz390.find_key_index("M:" + macro_name.toUpperCase());
	if (index != -1){
		if (index == -2){
			check_sysops();
		}
        return index;
	}
	if (tz390.opt_mfc){
		index = tz390.find_key_index("O:" + macro_name.toUpperCase());
		if (index != -1){
			if (tz390.find_key_index("M:" + macro_name.toUpperCase()) == -1){
				tz390.add_key_index(-2); // prevent repeat searches
				tot_ins++;
			}
			check_sysops();
			return -2; // don't allow search for instructions
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
	 int save_mac_line_index = mac_line_index;
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
		  * add lcl system variables 
		  */
         init_lcl_sys();
		 if  (tz390.opt_listcall){
		 	 String call_label = bal_label;
		 	 String call_parms = bal_parms;
		 	 if (call_label == null){
		 	 	call_label = "        ";
		 	 } 
		 	 if (call_label.length() < 8){
		 	       call_label = call_label.concat("        ").substring(0,8);
		 	 }
		 	 String call_op = bal_op;
		 	 if (call_op.length() < 5){
		 	 	call_op = call_op.concat("     ").substring(0,5);
		 	 }
		 	 String sysndx = "    " + lcl_sysndx;
		 	 sysndx = sysndx.substring(sysndx.length() - 4);
		 	 String sysnest = "  " + mac_call_level;
		 	 sysnest = sysnest.substring(sysnest.length() - 2);
		 	 if (call_parms == null)call_parms = "";
		 	 String call_line = "      " + mac_file_line_num[save_mac_line_index];
		 	 call_line = call_line.substring(call_line.length() - 6);
		 	 put_bal_line("*MCALL #=" + sysndx + " LV=" +  sysnest + " LN=" + call_line + " " + call_label + " " + call_op + " " + call_parms);
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
private void init_gbl_sys(){
	/*
	 * add global system variables
	 */
	add_gbl_sys("&SYSASM",var_setc_type);
	gbl_setc[tot_gbl_setc-1] = "z390";
	add_gbl_sys("&SYSCLOCK",var_setc_type);
	gbl_sysclock_index = tot_gbl_setc-1;
	add_gbl_sys("&SYSDATC",var_setc_type);
	gbl_setc[tot_gbl_setc-1] = sdf_sysdatc.format(cur_date);
	add_gbl_sys("&SYSDATE",var_setc_type);
	gbl_setc[tot_gbl_setc-1] = sdf_sysdate.format(cur_date);
	add_gbl_sys("&SYSMAC",var_setc_type);
	gbl_sysmac_index = tot_gbl_setc-1;
	add_gbl_sys("&SYSPARM",var_setc_type);
	gbl_setc[tot_gbl_setc-1] = tz390.opt_sysparm;
	add_gbl_sys("&SYSTEM_ID",var_setc_type);
	gbl_setc[tot_gbl_setc-1] = 
		System.getProperty("os.name") 
		+ " " + System.getProperty("os.version");
	add_gbl_sys("&SYSTEM_JAVA",var_setc_type);
	gbl_setc[tot_gbl_setc-1] = 
		System.getProperty("java.vendor") 
		+ " " + System.getProperty("java.version");
	add_gbl_sys("&SYSTIME",var_setc_type);
	gbl_setc[tot_gbl_setc-1] = sdf_systime.format(cur_date);
	add_gbl_sys("&SYSVER",var_setc_type);
	gbl_setc[tot_gbl_setc-1] = tz390.version;
}
private void add_gbl_sys(String sys_name,byte sys_type){
	/*
	 * add global system variables
	 */
	tz390.find_key_index("G:" + sys_name);
	add_gbl_set(sys_name,sys_type,1);
}
private void init_lcl_sys(){
	/*
	 * init local system macro variables
	 */
	 add_lcl_sys("&SYSNDX",var_seta_type);
	 lcl_sysndx++;
	 lcl_seta[tot_lcl_seta-1] = lcl_sysndx;
	 add_lcl_sys("&SYSNEST",var_seta_type);
	 lcl_seta[tot_lcl_seta-1] = mac_call_level;
	 add_lcl_sys("&SYSECT",var_setc_type);
	 lcl_setc[tot_lcl_setc-1] = lcl_sysect;
	 add_lcl_sys("&SYSLOC",var_setc_type);
	 lcl_setc[tot_lcl_setc-1] = lcl_sysloc;
	 add_lcl_sys("&SYSSTYP",var_setc_type);
	 lcl_setc[tot_lcl_setc-1] = lcl_sysstyp;
}
private void add_lcl_sys(String sys_name,byte sys_type){
	/*
	 * add local system variable
	 */
	 if (find_lcl_key_index("L:" + sys_name) == -1){
		 add_lcl_set(sys_name,sys_type,1);
	 } else {
		 abort_error(122,"duplicate lcl system variable - " + sys_name);
	 }
}
private void check_sysops(){
	/*
	 * check for opcodes that update system variables
	 */
	 if (bal_op == null || bal_op.length() == 0){
		 return;
	 }
     switch (bal_op.charAt(0)){
     case 'C':
    	 if (bal_op.equals("CSECT")){
    		 lcl_sysect = bal_label.toUpperCase();
    		 lcl_sysloc = lcl_sysect;
    		 lcl_sysstyp = "CSECT";
    	 }
    	 break;
     case 'D':
    	 if (bal_op.equals("DSECT")){
    		 lcl_sysect = bal_label.toUpperCase();
    		 lcl_sysloc = lcl_sysect;
    		 lcl_sysstyp = "DSECT";
    	 }
    	 break;
     case 'L':
    	 if (bal_op.equals("LOCTR")){
    		 lcl_sysloc = bal_label.toUpperCase();
    	 }
     case 'R':
    	 if (bal_op.equals("RSECT")){
    		 lcl_sysect = bal_label.toUpperCase();
    		 lcl_sysloc = lcl_sysect;
    		 lcl_sysstyp = "RSECT";
    	 }
    	 break;
     case 'S':
    	 if (bal_op.equals("START")){
    		 lcl_sysect = bal_label.toUpperCase();
    		 lcl_sysloc = lcl_sysect;
    		 lcl_sysstyp = "START";
    	 }
    	 break;
     }
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
	    int key_value_level = 0;
	    proto_match = proto_pattern.matcher(proto_parms);
        byte state = 1;
 	    while (proto_match.find()){
 	       parm_value = proto_match.group();
 	       switch (state){
			   case 1: // new parm
 	               if  (parm_value.equals(",")){
       	               init_pos_parm(""); 
 	               } else if (parm_value.charAt(0) <= asc_space_char){ // RPI 239
 	               	   state = 4;
 	               } else {
 	       	  	       if  (parm_value.charAt(parm_value.length()-1) == '='){
 	       	  	   	       key_name  = parm_value.substring(0,parm_value.length()-1);
                           key_value = "";
                           key_value_level = 0;
                           state = 2; // possible key
 	       	  	       } else {
 	       	  	           init_pos_parm(parm_value);
 	       	  	           state = 3; // skip next commas
 	       	  	       }
 	               }
 	               break;
 	           case 2: // possible keyword parm initial value text
 	       	       if  (key_value_level == 0 //RPI 223
 	       	    		&& (parm_value.equals(",")
 	       	       		    || parm_value.charAt(0) <= asc_space_char)){ // RPI 239
 	       			   init_key_parm(key_name,key_value);
 	       	   	       state = 1;
 	       	       } else {
 	       	    	   if (parm_value.equals("(")){ //RPI 223
 	       	    		   key_value_level++;
 	       	    	   } else if (parm_value.equals(")")){
 	       	    		   key_value_level--;
 	       	    	   }
 	       	   	       key_value = key_value.concat(parm_value);
 	       	       }
 	       	       break;
 	           case 3:  // ignore comma before next parm
 	       	       if (parm_value.equals(",")){
 	       	   	      state = 1; // start next parm
 	       	       } else if (parm_value.charAt(0) <= asc_space_char){ // RPI 239
 	       	       	  state = 4; // switch to flush comments
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
    	               } else if (token_first <= asc_space_char){ //RPI181
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
    	       	       if (token_first <= asc_space_char //RPI181  
    	       	       		|| (token_first == ','
    	       	       		    && level == 0)){
    	       	       	  if   (key_name == null){
    	       	   	           set_pos_parm(parm_value);
    	       	       	  } else {
   	    	       			   if (!set_key_parm("&" + key_name,parm_value)){
    	    	       	  	       set_pos_parm(key_name + "=" + parm_value);
    	    	       		   }
    	       	       	  }
    	       	       	  if (token_first > asc_space_char){  //RPI181
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
       mac_call_pos_tot[mac_call_level] = cur_pos_parm - mac_call_pos_start[mac_call_level]-1;
       if (mac_call_pos_tot[mac_call_level] < 0){
    	   mac_call_pos_tot[mac_call_level] = 0;
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
	  if    (tz390.z390_abort){
	    	System.exit(mz390_rc);
	  }
}
private void put_stats(){
	/*
	 * display statistics as comments at end of BAL
	 */
	log_to_bal = false;
	if  (tz390.opt_stats || (mz390_errors > 0)){
  	      log_to_bal = true;
  	   	  if  (tz390.opt_timing){
			   cur_date = new Date();
	   	       put_log("MZ390I " + tz390.version 
	   			+ " Current Date " +sdf_mmddyy.format(cur_date)
	   			+ " Time " + sdf_hhmmss.format(cur_date));
	   	  } else {
	   	        put_log("MZ390I " + tz390.version);
	   	  }
  	   	  put_log("MZ390I program = " + tz390.dir_mlc + tz390.pgm_name + tz390.pgm_type);
	   	  put_log("MZ390I options = " + tz390.cmd_parms);
	      put_log("Stats total MLC/MAC loaded  = " + tot_mac_line);
	      put_log("Stats total BAL output      = " + tot_bal_line);
	      put_log("Stats total instructions    = " + tot_ins);
	      put_log("Stats total macros          = " + tot_mac_name);
	      put_log("Stats total macro loads     = " + tot_mac_load);
	      put_log("Stats total macro calls     = " + tot_mac_call);	
	      put_log("Stats total global set var  = " + tot_gbl_name);
	      put_log("Stats total local pos parms = " + tot_pos);
	      put_log("Stats total local key parms = " + tot_kwd);
	      put_log("Stats total local set var   = " + tot_lcl_set);
		  put_log("Stats total array expansions= " + tot_expand);
	      put_log("Stats total Keys            = " + tz390.tot_key);
		  put_log("Stats Key searches          = " + tz390.tot_key_search);
		  if (tz390.tot_key_search > 0){
		      tz390.avg_key_comp = tz390.tot_key_comp/tz390.tot_key_search;
		  }
		  put_log("Stats Key avg comps         = " + tz390.avg_key_comp);
		  put_log("Stats Key max comps         = " + tz390.max_key_comp);
	      put_log("Stats total macro instr.    = " + tot_mac_ins);
	      if  (tz390.opt_timing){
	          cur_date = new Date();
	          tod_end = cur_date.getTime();
	          tot_msec = tod_end-tod_start+1;
	          put_log("Stats total milliseconds    = " + tot_msec);
	          ins_rate = tot_mac_ins*1000/tot_msec;
	          put_log("Stats instructions/second   = " + ins_rate);
	      }
	      if (tz390.opt_listfile){
	      	 int index = 0;
	      	 while (index < tot_mac_file_name){
	      	 	put_log("MZ390I file=" + (index+1)
	      	 		  + " path=" + mac_file_name[index]
					  );
	      	 	index++;
	      	 }
	      }
	}
	put_log("MZ390I total mnotes          = " + tot_mnote + "  max level= " + max_mnote_level);
	put_log("MZ390I total errors          = " + mz390_errors);
	put_log("MZ390I return code           = " + mz390_rc);
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
	  put_log("MZ390E error " + error
	  		         + " file=" + (mac_file_name_num[mac_line_index]+1)
					 + " line=" + mac_file_line_num[mac_line_index]
	  		           + " " + msg);
	  mz390_errors++;
	  if (max_errors != 0 && mz390_errors > max_errors){
	  	 abort_error(83,"maximum errors exceeded");
	  }
}
private void abort_error(int error,String msg){
	/*
	 * issue error msg to log with prefix and
	 * inc error total
	 */
	  if (tz390.z390_abort){
		 System.out.println("mz390 aborting due to recursive abort for " + msg);;
	  	 System.exit(16);
	  }
	  mz390_errors++;
	  tz390.z390_abort = true;
      log_to_bal = true;
	  put_log("MZ390E error " + error 
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
	   	if  (tz390.opt_timing){
			cur_date = new Date();
	   	    put_log("MZ390I " + tz390.version 
	   			+ " Current Date " +sdf_mmddyy.format(cur_date)
	   			+ " Time " + sdf_hhmmss.format(cur_date));
	   	} else {
	   	    put_log("NZ390I " + tz390.version);
	   	}
	   	if  (z390_log_text == null){
	   	    put_log("Copyright 2005 Automated Software Tools Corporation");
	   	    put_log("z390 is licensed under GNU General Public License");
	   	}
	   	put_log("MZ390I program = " + tz390.dir_mlc + tz390.pgm_name + tz390.pgm_type);
	   	put_log("MZ390I options = " + tz390.cmd_parms);
	   }
	   private synchronized void put_log(String msg) {
	   	/*
	   	 * Write message to z390_log_text or console
	   	 * if running standalone
	   	 * 
	   	 */
	   	    if  (tz390.opt_tracem
	   	    		|| tz390.opt_traceall 
	   	    		|| log_to_bal){
	   	    	put_bal_line("* " + msg);
	   	    }
	        if  (z390_log_text != null){
  	        	z390_log_text.append(msg + "\n");
   	        }
	        if (tz390.opt_con || tz390.z390_abort){
   	    	    System.out.println(msg);
   	        }
	   }
	private int find_lcl_key_index(String user_key){
		/*
		 * return user_key_index for lcl user_key else -1
		 * and set following for possible add_key_index:
		 *    1.  lcl_key_text = user_key
		 *    2.  lcl_key_hash = hash code for key
		 *    3.  lcl_key_index_last = last search entry
		 */
		tz390.tot_key_search++;
		lcl_key_text = user_key;
	    lcl_key_rand.setSeed((long) lcl_key_text.hashCode());
	    lcl_key_hash  = lcl_key_rand.nextInt();
	    lcl_key_index = lcl_key_rand.nextInt(max_lcl_key_root)+cur_lcl_key_root;
		if (lcl_key_tab_key[lcl_key_index] == null){
			lcl_key_index_last = lcl_key_index;
			return -1;
		}
	    int cur_key_comp = 0;
		while (lcl_key_index >= cur_lcl_key_root){
			tz390.tot_key_comp++;
			cur_key_comp++;
			if (cur_key_comp > tz390.max_key_comp){
				tz390.max_key_comp = cur_key_comp;
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
		tz390.tot_key++;
		lcl_key_tab_key[lcl_key_index]   = lcl_key_text;
		lcl_key_tab_hash[lcl_key_index]  = lcl_key_hash;
		lcl_key_tab_index[lcl_key_index] = user_index;
	    lcl_key_tab_low[lcl_key_index] = 0;
	    lcl_key_tab_high[lcl_key_index] = 0;
	}
}