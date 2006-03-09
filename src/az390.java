import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.lang.reflect.Array;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.MathContext;
import java.nio.ByteBuffer;
import java.text.SimpleDateFormat;
import java.util.Arrays;
import java.util.Date;
import java.util.Iterator;
import java.util.TreeSet;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.swing.JTextArea;

public  class  az390 {
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

    az390 is the assembler component of z390 which can be called from
    z390 gui interface or from command line to read bal source and
    generate obj relocatable object code file.

    ****************************************************
    * Maintenance
    * ***************************************************
    * 03/30/05 copied from mz390.java and modified
    * 04/03/05 completed basic assembly of demo with
    *          obj and prn file output but missing 
    *          operand parsing.
    * 04/15/05 completed demo support with literal
    *          and RLD support for DC A type fields
    * 05/17/05 add support for '' reduction in dcc
    * 05/29/05 add DCP support 
    * 05/31/05 add packed dec L1 and L2 support
    * 06/25/05 fix r3,r2 code sequence in RRF format
    * 07/05/05 fix shift ins format to skip r3
    * 07/11/05 add DB,DH,EB,EH,LB, and LH fp data
    * 07/19/05 add r1 only exception for SPM and IPM
    * 07/23/05 fix RSL setup for TP d1(l1,b1) packed field type
    * 07/24/05 fix fp constant calc to handle LB/LH
    *          exponent range beyond double by using
    *          equivalent log(X*10**N)=Log(X)+N*log(10)
    * 07/27/05 fix RRE to allow 1 opr (IPM,EFPC,SFPC)
    * 07/30/05 fix sequence of r3,r2 in RRF format for FIEBR
    * 07/31/05 add format RR4 for RRF DIEBR and DIDBR
    * 08/04/05 fix trap error when ins has missing operands
    * 08/17/05 add EXTRN support - unit test TESTEXT1
    * 08/19/05 fix esd and offset in obj for mult sect6s
    * 08/19/05 add dcv_data Vcon and ENTRY support
    * 08/22/05 add SYSBAL, SYSOBJ, SYSPRN dir options
    * 08/28/05 add DS/DC S type support
    * 08/28/05 add dependant and labeled USING support
    * 09/01/05 ADD ORG support and comma delimited continue
    * 09/08/05 fix address errors for mult sect pgms by
    *          forcing pass for any sect change
    * 09/09/05 add CNOP support for use in READ/WRITE
    * 10/03/05 RPI2  fix DC duplication factor error 51
    * 10/03/05 RPI3  fix RS/RX with ddd(,B) syntax
    * 10/03/05 RPI9  fix L' operator parsing
    * 10/03/05 RPI10 fix ORG with no operand error
    * 10/03/05 RPI11 fix DC S(1) error 38 no base
    * 10/03/05 RPI11 fix DC S(X) error 88 xref error
    * 10/04/05 RPI5 - option ASCII use ASCII vs EBCDIC
    *                    DC C'...' ascii char data
    *                    C'..' self def. term value
    * 10/04/05 RPI6 - option ERR(nn) limit errors
    * 10/05/05 RPI5 - add DC and SDT C".." ascii char
    * 10/05/05 RPI12 - reset lit_ref/gen after errors
    * 10/17/05 RPI25 - change TRACE to TRACEA option
    * 10/18/05 RPI29 - use AZ390E and AZ390I prefixes
    * 10/19/05 RPI34 - full ascii / ebcdic translate
    * 11/07/05 RPI73 support C!..! EBCDIC always
    * 11/08/05 RPI73 fix PKA X'E9' rflen from s2
    * 11/11/05 RPI87 fix ORG when preceeded by abs exp. calc
    * 11/12/05 RPI85 issue error if AFHVY value
    *          exceeds size of field.
    * 11/13/05 RPI90 fix regression in RPI73 causing
    *          B, C, and X symbols to trap
    * 11/28/05 RPI113 file path with drive: and no separator
    * 12/03/05 RPI115 fix continuation support for lit comma
    * 12/03/05 RPI116 issue error if no END found
    * 12/07/05 RPI122 ignore following opcodes AMODE, RMODE,
    *          EJECT, SPACE, 
    * 12/07/05 RPI124 remove trailing spaces from source
    * 12/08/05 RPI120 fix SRP explicit d2(b2) format
    * 12/12/05 RPI131 ignore label on TITLE to avoid dup.
    * 12/15/05 RPI135 use tz390 shared tables
    * 12/17/05 RPI57 - symbol cross reference option xref
    * 12/19/05 RPI142 add DS/DC Y and share type tables
    * 12/23/05 RPI127 remove user mlc type from file name
    *          and use shared set_pgm_dir_name_type
    * 12/23/05 RPI131 limit file output to maxfile(mb)
    * 12/31/05 change MNOTE opcode case value for opsyn
    * 01/01/06 RPI150 add OPSYN support 
    * 01/06/06 RPI157 check for extra instruction parms
    * 01/06/06 RPI159 trap = as literal error in expression
    * 01/09/06 RPI161 allow d(,b) in RXY by eliminating
    *          duplicate code not fixed by RPI3
    * 01/09/06 RPI164 convert EXTRN to CSECT or ENTRY
    * 01/10/06 RPI165 xref USAGE references
    * 01/10/06 RPI167 issue error for contiunation text < 16
    * 01/11/06 RPI166 correct RSY b(d) generation
    * 01/13/05 RPI171 correct unary +- support
    * 01/19/06 RPI181 terminate on any white space char
    * 01/24/06 RPI182 PRINT, PUSH, POP, WXTRN
    * 01/25/06 RPI128 add bin obj with hex obj option
    * 01/26/06 RPI 172 move options to tz390
    * 02/10/06 RPI 199 add BLX branch relative on condition long
    * 02/12/06 RPI 189 sort lits and symbols in XREF
    * 02/18/06 RPI 206 correct RRF 3 formats
    *          a) case 30 - DIEBR, DIDBR     > r1,r3,r2,m4 > 3412 
    *          b) case 15 - MA?R, MS?R, MY?R = r1,r3,r2    > 1032 
    *          c) case 34 - CG?R, CF?R, FI?R, IDTE, TB?R > r1,m3,r2 > 3012
    * 02/21/06 RPI 208 use tz390.z390_abort flag
    ********************************************************
    * Global variables
    *****************************************************/
	tz390 tz390 = null;
    int az390_rc = 0;
    int az390_errors = 0;
    int cur_pass = 1;
    Date cur_date = new Date();
    long tod_start = cur_date.getTime();
    long tod_end   = 0;
    long tot_sec = 0;
    boolean stats_to_obj = false;
    int tot_obj_bytes = 0;
    File bal_file = null;
    BufferedReader bal_file_buff = null;
    RandomAccessFile obj_file = null;
    File prn_file = null;
    BufferedWriter prn_file_buff = null;
    String bal_line = null;
    String bal_label   = null;
    String opsyn_label = null;
    String bal_op = null;
    boolean bal_op_ok = false;
    String bal_parms = null;
    boolean list_bal_line = false;
    int     mac_call_level = 0;
    boolean mac_call_first = false;
    boolean mac_call_last  = false;
	boolean bal_abort = false;
    int bal_op_index = 0;
    boolean bal_eof = false;
    boolean end_found = false;
	SimpleDateFormat mmddyy = new SimpleDateFormat("MM/dd/yy");
	SimpleDateFormat hhmmss = new SimpleDateFormat("HH:mm:ss");
    boolean log_tod = true; 
    JTextArea z390_log_text = null;
    /*
     * static limits
     */
    static int max_errors = 100;
    static int max_push   = 100;
    static int max_pass = 4;
    static int max_bal_line = 200000;
    static int max_line_len = 80;
    static int max_sym = 20000;
    static int max_lit = 20000;
    int sort_index_bias = 100000; // must be > max_sym and max_lit
    int sort_index_len  = 6;      // digits in key_index_bias
    static int max_esd = 1000;
    static int max_use = 500;
    static int max_exp_stk = 500;
    static int max_exp_rld = 500;
    static int max_rld = 10000;
    static int max_text_buff_len = 16;
    long max_time_seconds = 15;     // max elapsed time
     /*
     * bal file global variables
     */
    long    tod_time_limit = 0;
    int     next_time_ins   = 0x1000;
    int     next_time_check = next_time_ins;
    int tot_bal_line = 0;
    String[]  bal_name_line = new String[max_bal_line]; //logical bal line from 1 or more physical lines
    int[]     bal_line_num = (int[])Array.newInstance(int.class,max_bal_line); //starting physical line #
    boolean bal_line_gen = true;
    int cur_line_num = 0;
    String parm_name = null;
    String parm_value = null;
    int bal_line_index = 0; //current mac line index
    Pattern exp_pattern = null;
    Matcher exp_match   = null;
    Pattern parm_pattern = null;
    Matcher parm_match   = null;
    Pattern label_pattern = null;
    Matcher label_match   = null;
    Pattern extrn_pattern = null;
    Matcher extrn_match   = null;
    Pattern dcc_sq_pattern = null;  // EBCDIC or ASCII
    Pattern dcc_dq_pattern = null;  //RPI5  C".." ASCII
    Pattern dcc_eq_pattern = null;  //RPI73 C!..! EBCDIC
    Matcher dcc_match   = null;
    /*
     * location counter and ESD tables
     */
    int loc_ctr = 0;
    int loc_start = 0;
    int loc_len = 0;
	int cur_esd_sid = 0;
    int tot_esd = 0;
    int cur_esd = 0;
    int first_cst_esd = 0;
    static int esd_sdt = 0;
    static int esd_rld = -1;
    int[]     esd_sid  = (int[])Array.newInstance(int.class,max_esd);
    /*
     * using global data
     */
    int cur_use_start = 0;
    int cur_use_end   = 0;
    int[] push_cur_use_start = (int[])Array.newInstance(int.class,max_push);
    int[] push_cur_use_end   = (int[])Array.newInstance(int.class,max_push);
    int cur_use = 0;
    boolean cur_use_depend = false;
    boolean use_eof = false;
    int cur_use_base_esd = 0;
    int cur_use_base_loc = 0;
    int cur_use_base_len = 0;
    int cur_use_reg = 0;
    int cur_use_reg_loc = 0;
    String cur_use_parms = null;
    String cur_use_lab = "";
    String[] use_lab      = new String[max_use];
    int[]    use_base_esd = (int[])Array.newInstance(int.class,max_use);
    int[]    use_base_loc = (int[])Array.newInstance(int.class,max_use);
    int[]    use_base_len = (int[])Array.newInstance(int.class,max_use);
    int[]    use_reg      = (int[])Array.newInstance(int.class,max_use);
    int[]    use_reg_loc  = (int[])Array.newInstance(int.class,max_use);
    /*
     * push, pop, and, print data
     */
    int using_level = 0;
    int print_level = 0;
    int[]     using_start = (int[])Array.newInstance(int.class,max_push);
    int[]     using_end   = (int[])Array.newInstance(int.class,max_push);
    boolean[] print_on   = (boolean[])Array.newInstance(boolean.class,max_push);
    boolean[] print_gen  = (boolean[])Array.newInstance(boolean.class,max_push);
    boolean[] print_data = (boolean[])Array.newInstance(boolean.class,max_push);
    /*
     * symbol table global variables
     */
    static byte sym_sdt   = 0;  // dec, b'', c'', h''
    static byte sym_cst   = 1;  // CSECT )alias REL)
    static byte sym_dst   = 2;  // DSECT (alias REL)
    static byte sym_ent   = 3;  // ENTRY (alias REL)
    static byte sym_ext   = 4;  // EXTRN external link
    static byte sym_rel   = 5;  // RX (CST.DST,ENT)_
    static byte sym_rld   = 6;  // complex rld exp
    static byte sym_lct   = 7;  // loctr (changed to cst/dst). 
    static byte sym_wxt   = 8;  // WXTRN weak external link RPI182
    int tot_sym = 0;
    int tot_sym_find = 0;
    int tot_sym_comp = 0;
    int cur_sid = 0;
    int prev_sect_sid = 0;
    int prev_sect_esd = 0;
    boolean sect_change = false;
    byte prev_sect_type = sym_cst;
    static String[] sym_type_desc = {
    	"ABS","CST","DST","ENT","EXT","REL","RLD","LCT","WXT"}; //RPI182
    String[]  sym_name         = new String[max_sym];
    int[]     sym_def          = (int[])Array.newInstance(int.class,max_sym);
    byte[]    sym_type         = (byte[])Array.newInstance(byte.class,max_sym);
    int[]     sym_esd          = (int[])Array.newInstance(int.class,max_sym);
    int[]     sym_loc          = (int[])Array.newInstance(int.class,max_sym);
    int[]     sym_max_loc      = (int[])Array.newInstance(int.class,max_sym);
    int[]     sym_len          = (int[])Array.newInstance(int.class,max_sym);
    int[]     sym_sect_type    = (int[])Array.newInstance(int.class,max_sym);
    int[]     sym_sect_prev    = (int[])Array.newInstance(int.class,max_sym);
    int[]     sym_sect_next    = (int[])Array.newInstance(int.class,max_sym);
    TreeSet<Integer>[] sym_xref = (TreeSet<Integer>[])Array.newInstance(TreeSet.class,max_sym);
    int last_xref_index = 0;
    int last_xref_line  = 0;
    /*
     * literal table for next pool at LTORG or END
     */
    int tot_lit = 0;
    int cur_lit = 0;
    boolean lit_loc_ref = false;
    int cur_lit_pool = 1;
    String[]  lit_name         = new String[max_sym];
    int[]     lit_pool         = (int[])Array.newInstance(int.class,max_lit);
    int[]     lit_line         = (int[])Array.newInstance(int.class,max_lit);
    int[]     lit_line_loc     = (int[])Array.newInstance(int.class,max_lit);
    int[]     lit_esd          = (int[])Array.newInstance(int.class,max_lit);
    int[]     lit_loc          = (int[])Array.newInstance(int.class,max_lit);
    int[]     lit_len          = (int[])Array.newInstance(int.class,max_lit);
    byte[]    lit_gen          = (byte[])Array.newInstance(byte.class,max_lit);
    int[]     lit_def          = (int[])Array.newInstance(int.class,max_lit);
    TreeSet<Integer>[] lit_xref = (TreeSet<Integer>[])Array.newInstance(TreeSet.class,max_lit);
    
    /*
     * bal operation code data and tables
     */
    String hex_tab   = "0123456789ABCDEF";
    String hex_op    = null;
    String hex_len   = null;
    String hex_len1  = null;
    String hex_len2  = null;
    String hex_bddd  = null;
    String hex_bddd1 = null;
    String hex_bddd2 = null;
    String hex_bddd_loc  = null;
    String hex_bddd1_loc = null;
    String hex_bddd2_loc = null;
    /*
     * expression global variables
     * including polish notation var and op stacks
     */
	String  exp_text  = ""; // expression text
    int     exp_index = 0;  // current starting index
	boolean check_prev_op = true;
	int     exp_val   = 0;
	int     exp_esd   = 0;
	byte    exp_type  = 0;
    int     exp_state = 0;
    int     exp_level = 0;
    String  exp_use_lab = null;
    boolean exp_term = false;
    boolean exp_eot  = false;  // end of text terminator
    String  exp_term_op = "~";
    String  exp_start_op = exp_term_op;
    String  exp_token = null;
    String  exp_op    = " ";
    int     sym_sid1  = 0;
    int     sym_sid2  = 0;
    int     sym_esd1  = 0;
    int     sym_esd2  = 0;
    byte    sym_type1 = 0;
    byte    sym_type2 = 0;
    int     sym_val1  = 0;
    int     sym_val2  = 0;
    String  exp_prev_op = exp_start_op;
    int     exp_sym_index = -1;  // symbol index
    boolean exp_sym_pushed = false;
    boolean exp_sym_last = false; 
    boolean exp_first_len = false;
    int exp_len = 1;
    int tot_exp_stk_sym = 0;
    int tot_exp_stk_op  = 0;
    int[]     exp_stk_sym_esd  = (int[])Array.newInstance(int.class,max_exp_stk);
    int[]     exp_stk_sym_val  = (int[])Array.newInstance(int.class,max_exp_stk);
    String[]  exp_stk_op   = new String[max_exp_stk];
    int[]     exp_op_class = (int[])Array.newInstance(int.class,256);
    /*
     * define exp actions based on last and
     * next operator class
     *    1 2 3 4 5 6
     *   +-* /( ) L'~             col = next_op
     *                            row = prev_op
     */ 
          int tot_classes = 6;
          int[] exp_action = {  
          1,3,3,1,3,1,   // 1 +-  prev add/sub
          2,2,3,2,3,2,   // 2 * / prev mpy/div
          3,3,3,4,3,0,   // 3 (   prev open
          0,0,0,0,3,0,   // 4 )   prev close
		  5,5,0,5,5,5,   // 5 L'  prev length attr
          3,3,7,6,3,6,   // 6 ~   prev terminator
		  };
     /* action code routines:
      *   0 error
      *   1 add/sub
      *   2 mpy/div
      *   3 push op
      *   4 POP  op
      *   5 length attribute of symbol or * instr.
      *   6 exit with result of expression
      *   7 check ( for terminator if last_val 
      */
     /*
      * expression relocation definitions RLDS
      */
      byte exp_rld_len = 0;  // gen rlds if 3 or 4
      int tot_exp_rld_add = 0;
      int tot_exp_rld_sub = 0;
      int[]     exp_rld_add_esd = (int[])Array.newInstance(int.class,max_exp_rld);
      int[]     exp_rld_sub_esd = (int[])Array.newInstance(int.class,max_exp_rld);
      /*
       * global relocation definitions RLDS
       */
       int tot_rld = 0;
       static char rld_add = '+';
       static char rld_sub = '-';
       int[]     rld_fld_esd = (int[])Array.newInstance(int.class,max_rld);
       int[]     rld_fld_loc = (int[])Array.newInstance(int.class,max_rld);
       byte[]    rld_fld_len = (byte[])Array.newInstance(byte.class,max_rld);
       char[]    rld_fld_sgn = (char[])Array.newInstance(char.class,max_rld);
       int[]     rld_xrf_esd = (int[])Array.newInstance(int.class,max_rld);
  /*
   * object code text buffer variables
   */  
      boolean gen_obj_code = false;
  	  String obj_code = "";
  	  int    list_obj_loc  = 0;
  	  String list_obj_code = "";
      String cur_text_buff = null;
      int cur_text_loc = 0;
      int cur_text_len = 0;
      int cur_text_esd = 0;
  /*
   * binary obj file buffer and layouts
   */
     byte[] bin_byte = new byte[80];
     static byte obj_bin_id = 0x02; // first bin obj byte
     byte[] bin_esd_type = {'E','S','D'};
     byte[] bin_txt_type = {'T','X','T'};
     byte[] bin_rld_type = {'R','L','D'};
     byte[] bin_end_type = {'E','N','D'};
  /*
   * DS/DC global variables
   */
      boolean dc_op   = false;  // ds vs dc bal op
      boolean dc_eod  = false;  // ds/dc end of fields
      boolean dc_len_explicit = false;
      boolean dc_first_field = false;
      boolean dc_lit_ref = false;
      boolean dc_lit_gen = false;
      int     dc_lit_index_start = 0;
      String dc_field = null;
      char   dc_type = ' ';
      boolean dcv_type = false;
      boolean dca_ignore_refs = false;
      char   dc_type_sfx = ' ';
      static int fp_db_type = 0;
      static int fp_dh_type = 1;
      static int fp_eb_type = 2;
      static int fp_eh_type = 3;
      static int fp_lb_type = 4;
      static int fp_lh_type = 5;
      int    fp_type = 0;
      String fp_hex = null;
      int[]  fp_man_bits = {52,56,23,24,112,112};
      /*
       * Note:  The following big decimal precision
       *        array used in both az390 and ez390
       *        should be maintained consistently
       *        as it is used for rounding 
       *        during conversions between types.
       */
      int[]  fp_precision = {18,18,8,8,36,36}; 
      int[]  fp_sign_bit = {0x800,0x80,0x100,0x80,0x8000,0x80};
      int[]  fp_one_bit_adj = {2,1,2,1,2,1};
      int[]  fp_exp_bias = {0x3ff,0x40,0x7f,0x40,0x3fff,0x40};
      int[]  fp_exp_max  = {0x7ff,0x7f,0xff,0x7f,0x7fff,0x7f};
  	  static double fp_log2  = Math.log(2);
	  static double fp_log10 = Math.log(10);
      int fp_sign = 0;
      int fp_exp   = 0; // scale * log10/log2
      MathContext fp_context = null;
      BigDecimal fp_big_dec1 = new BigDecimal("0");
      BigDecimal fp_big_dec2 = new BigDecimal("0");
      BigDecimal fp_big_dec3 = new BigDecimal("0");
      byte[] fp_big_byte = null;
      byte[] fp_data_byte = new byte[16];
      ByteBuffer fp_data_buff = ByteBuffer.wrap(fp_data_byte,0,16);
      BigInteger fp_big_int1 = new BigInteger("0");
      BigInteger fp_big_int2 = new BigInteger("0");
	  BigInteger fp_big_int_one_bits = BigInteger.ONE.shiftLeft(113).subtract(BigInteger.ONE);
	  BigInteger fp_big_int_man_bits = BigInteger.ONE.shiftLeft(112).subtract(BigInteger.ONE);
	  int    fp_int1 = 0;
	  int    fp_round_bit = 0;
      static int fp_int_eb_one_bits  = 0xffffff;
      static int fp_int_eb_man_bits  = 0x7fffff;
      static int fp_int_eh_man_bits  = 0xffffff;
      long   fp_long1 = 0;
      static long fp_long_db_one_bits = ((long)(1) << 53) - 1;
      static long fp_long_db_man_bits = ((long)(1) << 52) - 1;
      static long fp_long_dh_man_bits = ((long)(1) << 56) - 1;
      int    dc_index = 0;
      int    dc_data_start = 0;
      int    dc_dup   = 0;
      int    dc_dup_loc = 0; // rel offset for dup of a/v/s data with loc_ctr
      int    dc_len   = 0;
      int    dc_first_len = 0;
      int    dc_first_loc = 0;
      String dc_first_type = null;
      String dc_hex = null;
      byte[]     dc_data_byte = (byte[])Array.newInstance(byte.class,256);
      ByteBuffer dc_data = ByteBuffer.wrap(dc_data_byte,0,256);
      int dc_type_index = 0;
      
      byte ascii_lf = 0x0a;
      byte ascii_cr = 0x0d;
      byte ascii_period =  (int)'.';
      byte ascii_space = (int) ' ';
      byte ebcdic_period = 0x4B;
      byte ebcdic_space = 0x40;
  /* 
   * end of global az390 class data and start of procs
   */
public static void main(String[] args) {
  /*
   * main is entry when executed from command line
   * Create instance of az390 class and pass
   * parms to az390 like z390 does.
   */
      az390 pgm = new az390();
      pgm.process_az390(args,null);
}
public int process_az390(String[] args,JTextArea log_text){
   /*
    *  assembler bal source file into relocatable obj source file
    *
    *  Note this may be called directly from z390 GUI or
    *  from main when az370 run from windows command line.
    *  if called from main, the log_text balect will be null
    *  and local put_log function will route to console instead
    *  of the z390 log window.
    */
	    init_az390(args,log_text);
    	if (tz390.opt_trap){
     	   try {
        	    load_bal();
                process_bal();
     	   } catch (Exception e){
     		   abort_error(79,"internal system exception - " + e.toString());
     	   }
     	} else {
        	load_bal();
     		process_bal();
     	}
	    exit_az390();
	    if (log_text == null){
	    	System.exit(az390_rc);
	    }
    	return az390_rc;
}
private void init_az390(String[] args, JTextArea log_text){
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
	    tz390 = new tz390();
	    tz390.init_tables();
	    init_push_pop();
        tz390.init_options(args,".BAL");
		open_files();
	    if (!tz390.init_opcode_name_keys()){
	    	abort_error(87,"opcode key search table exceeded");
	    }
        put_copyright();
        compile_patterns();
        tod_time_limit = max_time_seconds * 1000 + tod_start;
}
private void init_push_pop(){
	/*
	 * init push/pop using and print
	 */
    using_start[0] = 0;
    using_end[0]   = 0;
    print_on[0] = true;
    print_gen[0] = true;
    print_data[0] = false;
}
private void compile_patterns(){
	/* 
	 * compile regular expression parsers
	 */
	/*
     * label_pattern  .lll
     */
    	try {
    	    label_pattern = Pattern.compile(
    			"([a-zA-Z$@#][a-zA-Z0-9$@#_]*)"           
			  );
    	} catch (Exception e){
    		  abort_error(1,"label pattern errror - " + e.toString());
    	}
    	/*
         * extrn and entry pattern
         */
        	try {
        	    extrn_pattern = Pattern.compile(
        			"([a-zA-Z$@#][a-zA-Z0-9$@#_]*)"
        	      +"|([,\\s])" //RPI181
    			  );
        	} catch (Exception e){
        		  abort_error(1,"extrn pattern errror - " + e.toString());
        	}
        /*
         * expression pattern
         *   1. B'01', C'ABC', X'0F' sdts
         *   2. USING label.
         *   3. symbolst  
         *   3. + - * / ( ) L'
         */
        	try {
        	    exp_pattern = Pattern.compile(
  		    	    "([bB]['][0|1]+['])" 
  		    	  +	"|([cC][']([^']|(['][']))*['])"       // ebcdic/ascii mode
  		    	  +	"|([cC][!]([^!]|([!][!]))*[!])"       // ebcdic always
  		    	  +	"|([cC][\"]([^\"]|([\"][\"]))*[\"])"  // ascii  always
	    		  +	"|([xX]['][0-9a-fA-F]+['])" 
        		  + "|([lL]['])"                           // length op  RPI9
        		  +	"|([a-zA-Z$@#][a-zA-Z0-9$@#_]*[\\.])" // labeled using 
        		  +	"|([a-zA-Z$@#][a-zA-Z0-9$@#_]*)"      // symbol
				  + "|([0-9]+)"                           // number
 		          + "|([\\s,'\\+\\-\\*\\/\\(\\)=])"  // RPI 159, RPI181
        	    );
        	} catch (Exception e){
        		  abort_error(2,"expression pattern errror - " + e.toString());
        	}
            /*
             * define exp_class with operator
             * precedence classes indexed by 
             * expression operator
             */
             exp_op_class['+'] = 1;
             exp_op_class['-'] = 1;
             exp_op_class['*'] = 2;
             exp_op_class['/'] = 2;
             exp_op_class['('] = 3;
             exp_op_class[')'] = 4;
             exp_op_class['L'] = 5; // length pfx
             exp_op_class['U'] = 5; // unary  pfx
             exp_op_class[' '] = 6;
             exp_op_class[','] = 6;
             exp_op_class['~'] = 6;
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
         		  +	"|([cC][']([^']|(['][']))*['])" 
         		  +	"|([cC][!]([^!]|([!][!]))*[!])" 
         		  + "|([cC][\"]([^\"]|([\"][\"]))*[\"])" 
         		  + "|([']([^']|(['][']))*['])"
      			  + "|([^\\s',()]+)"  // any chars except white space or "',()"  RPI181       
         	      + "|([\\s',()])" // white space char or ";,()"                 RPI181
     			  );
         	} catch (Exception e){
         		  abort_error(1,"parm pattern errror - " + e.toString());
         	}
             /*
              * dcc_sq_pattern for quoted string:
              *   1.  '...''...'
              * */
         	try {
         	    dcc_sq_pattern = Pattern.compile(
         	        "([']['])"
         	      + "|([&][&])" //RPI192
				  + "|(['&])"   //RPI192
       			  + "|([^'&]+)" 
     			  );
         	} catch (Exception e){
         		  abort_error(1,"dcc pattern errror - " + e.toString());
         	}
            /*
             * dcc_dq_pattern for quoted string:
             *   1.  "...""..."
             * */
        	try {
        	    dcc_dq_pattern = Pattern.compile(
        	        "([\"][\"])"
            	  + "|([']['])"	//RPI192
         	      + "|([&][&])" //RPI192
				  + "|([\"'&])" //RPI192
      			  + "|([^\"]+)" 
    			  );
        	} catch (Exception e){
        		  abort_error(1,"dcc pattern errror - " + e.toString());
        	}
            /*
             * dcc_eq_pattern for quoted string:
             *   1.  !...!!...!
             * */
        	try {
        	    dcc_eq_pattern = Pattern.compile(
        	        "([!][!])"
                  + "|([']['])"	//RPI192
         	      + "|([&][&])" //RPI192
				  + "|([!'&])"  //RPI192
      			  + "|([^!]+)" 
    			  );
        	} catch (Exception e){
        		  abort_error(1,"dcc pattern errror - " + e.toString());
        	}

}
private void open_files(){
	/*
	 * open obj and prn files
	 */
       	try {
       		obj_file = new RandomAccessFile(tz390.dir_obj + tz390.pgm_name + ".OBJ","rw");
       	} catch (IOException e){
       		abort_error(4,"I/O error on obj open - " + e.toString());
       	}
       	if (tz390.opt_list){
            prn_file = new File(tz390.dir_prn + tz390.pgm_name + ".PRN");
         	try {
       	       prn_file_buff = new BufferedWriter(new FileWriter(prn_file));
       	    } catch (IOException e){
       		   abort_error(4,"I/O error on prn open - " + e.toString());
       	    }
       	}
}
private void process_bal(){
	/* 
	 * assemble bal source into obj relocatable
	 * object code file                           
	 *   
	 */
	     resolve_symbols();
	     gen_obj_esds();
	     gen_obj_text();
	     gen_obj_rlds();
	     put_obj_line(".END");
	     if (tz390.opt_list){
	     	gen_sym_list();
	     	gen_lit_xref_list(); //RPI198
	     }
}
private void resolve_symbols(){
	/*
	 * if errors occurred during loading of bal
	 * repeat symbol update passes until there 
	 * are no errors or minimum error or max
	 * passes are reached.
	 */
	reset_lits();
	if (tz390.opsyn_name_change){
		tz390.reset_op_name_index();
	}
    if  (az390_errors > 0){
    	 int prev_az390_errors = az390_errors + 1;
    	 while (cur_pass < max_pass 
    	 		&& az390_errors !=0
    	 		&& (sect_change || az390_errors < prev_az390_errors)){
    	 	 prev_az390_errors = az390_errors;
    	 	 az390_errors = 0;
    	 	 cur_pass++;
    	     update_symbols();
    		 reset_lits();
    		 if (tz390.opsyn_name_change){
    			tz390.reset_op_name_index();
    		 }
             if (tz390.opt_tracea){
             	put_log("TRACE SYMBOL UPDATE PASS " + cur_pass + " TOTAL ERRORS = " + az390_errors);
             }
         }
    	 az390_errors = 0;
    }
	cur_pass++;  // incr to last pass
}
private void update_symbols(){
    /*
     * scan bal source and update symbols
     */
         loc_ctr = 0;
         cur_lit_pool = 1;
         cur_esd = 0;
         bal_eof = false;
         bal_line_index = 0;
	     while (!bal_eof){
		      if  (bal_line_index == tot_bal_line){
	           	  bal_eof = true;
	              if (tz390.opt_tracea){
	           	  	 put_log("TRACE SYMBOL UPDATE PASS " + cur_pass + "ERRORS = " + az390_errors);
		          }
		      } else {
	               bal_line = bal_name_line[bal_line_index];
	               parse_bal_line();
	               bal_op_index = find_bal_op();
	               if (bal_op_index != -1){
	           	      process_bal_op();    
	               }
			       bal_line_index++;
	          }
	     }
}
private void update_sects(){
	/*
	 * update each section starting address
	 * and max length, and reset current length
	 * 
	 * Notes:
	 *   1.  If any section start address or 
	 *       max length changes issue error
	 *       to force additional passes.
	 *   2.  sym_cst CSECT's start at 0 and are
	 *       contiguous within LOCTR's
	 *   3.  Each new CSECT is aligned to *8
	 *   4.  sym_dst DSECT's always start at 0
	 **/
	sect_change = false;
	int cst_ctr = 0;
	int index = 1;
	while (index <= tot_esd){
		cur_sid = esd_sid[index];
		if (sym_type[cur_sid] == sym_cst
			&& sym_sect_prev[cur_sid] == 0){
			loc_ctr = cst_ctr;
			if (sym_loc[cur_sid] != loc_ctr){
				sect_change = true;
				log_error(91,"csect start change error - " 
						      + sym_name[cur_sid]
							  + " old start=" + sym_loc[cur_sid]
							  + " new start=" + loc_ctr);
			}
			sym_loc[cur_sid] = loc_ctr;
			if (sym_sect_next[cur_sid] == 0){
			    loc_ctr = (loc_ctr + sym_len[cur_sid]+7)/8*8;
			} else {
				loc_ctr = loc_ctr + sym_len[cur_sid];
			}
			if (sym_max_loc[cur_sid] != loc_ctr
				&& tot_esd > 1){
				sect_change = true;
				log_error(92,"csect end change error - " 
						     + sym_name[cur_sid]
							 + " old end=" + sym_max_loc[cur_sid]
							 + " new end=" + loc_ctr);   
			}	
			sym_max_loc[cur_sid] = loc_ctr;
			sym_len[cur_sid] = loc_ctr - sym_loc[cur_sid];
			update_loctrs();
			sym_len[cur_sid] = 0;
            cst_ctr = loc_ctr; // save end of CSECT
		} else if (sym_type[cur_sid] == sym_dst              
			       && sym_sect_prev[cur_sid] == 0){
			loc_ctr = 0;
			sym_loc[esd_sid[index]] = loc_ctr;
			loc_ctr = loc_ctr + sym_len[cur_sid];
			if (sym_max_loc[cur_sid] != loc_ctr){
				sect_change = true;
				log_error(93,"dsect end change error - " 
						     + sym_name[cur_sid]
							 + " old end=" + sym_max_loc[cur_sid]
							 + " new end=" + loc_ctr);   
			}
			sym_max_loc[cur_sid] = loc_ctr;
			sym_len[cur_sid] = loc_ctr - sym_loc[cur_sid];
			update_loctrs();
			sym_len[cur_sid] = 0;
		}
		index++;
	}
	loc_ctr = 0;
}
private void update_loctrs(){
	/*
	 * update loctr sections with contiguous
	 * starting addresses from CSECT/DSECT
	 * and issue errors if any start address
	 * or length changes and reset length for
	 * next pass.
	 */
	int index = cur_sid;
	while (sym_sect_next[index] > 0){
		index = sym_sect_next[index];
		if (sym_loc[index] != loc_ctr){
			sect_change = true;
			log_error(94,"loctr section start change error - " 
					   + sym_name[index]
					   + " old start=" + sym_loc[index]
					   + " new start=" + loc_ctr 
						);
		}
		sym_loc[index] = loc_ctr;
        loc_ctr = loc_ctr + sym_len[index];
		if (loc_ctr != sym_max_loc[index]){
			sect_change = true;
			log_error(95,"loctr section length change error - " 
					   + sym_name[index]
					   + " old end=" + sym_max_loc[index]
					   + " new end=" + loc_ctr  
			           );
		}
		sym_max_loc[index] = loc_ctr;
		sym_len[index] = 0;
	}
}
private void reset_lits(){
	/*
	 * reset lit_gen flags to force reallocation
	 * on each pass
	 */
	int index = 0;
	while (index < tot_lit){
		lit_gen[index] = 0;  // indicate not alloc
		index++;
	}
}
private void gen_obj_esds(){
	/*
	 * write ESD's for CSECTS, EXTRNS, and ENTRIES
	 * to the OBJ file in ascii hex 
	 * and list on PRN if option LIST
	 */
	if (tot_esd > 0 && tz390.opt_list){
		put_prn_line("External Symbol Definitions");
	}
	cur_esd = 1;
	while (cur_esd <= tot_esd){
        if (sym_type[esd_sid[cur_esd]] != sym_dst
        	&& sym_sect_prev[esd_sid[cur_esd]] == 0){
    		String esd_code = 
    			" ESD=" + tz390.get_hex(sym_esd[esd_sid[cur_esd]],4)
    		  + " LOC=" + tz390.get_hex(sym_loc[esd_sid[cur_esd]],8)
    		  + " LEN=" + tz390.get_hex(get_sym_len(esd_sid[cur_esd]),8)
    		  + " TYPE=" + get_esd_type()
    		  + " NAME=" + sym_name[esd_sid[cur_esd]]
    		  ;
        	if (tz390.opt_list){	
                put_prn_line(esd_code);
    		}
        	put_obj_line(".ESD" + esd_code);
        }
		cur_esd++;
	}
}
private String get_esd_type(){
	/*
	 * return esd type
	 */
	String esd_type = sym_type_desc[sym_type[esd_sid[cur_esd]]];
    if (esd_type.equals("REL")){
    	esd_type = "ENT";
    }
	return esd_type;
}
private void gen_obj_text(){
	/*
	 * generate object code for bal instructions
	 * in CSECT's on final pass
	 */
	gen_obj_code = true;
	put_prn_line("Assembler Listing");
	loc_ctr = 0;
    cur_lit_pool = 1;
	cur_esd = 0;
    bal_eof = false;
    bal_line_index = 0;
    while (!bal_eof){
	      if  (bal_line_index == tot_bal_line){
          	  bal_eof = true;
	      } else {
              bal_line = bal_name_line[bal_line_index];
              parse_bal_line();
              bal_op_index = find_bal_op();
              if (bal_op_index != -1){
          	     process_bal_op();    
              }
		       bal_line_index++;
         }
    }
	if (!end_found){
		bal_line_index = tot_bal_line-1;
		log_error(115,"END statement not found");
	}
}
private void process_bal_op(){
	/*
	 * allocate or generate object code for bal op
	 * 
	 * 1.  Note op_type index values must match
	 *     op_name array values.  
	 * 2.  Indexes < 100 are machine instr. types
	 * 3.  Indexes > 100 are assembler instr.
	 *
	 */
	loc_len = 0;
	exp_text  = bal_parms;
	exp_index = 0;
	obj_code = "";
	list_obj_code = "";
	hex_bddd1_loc = "      ";
	hex_bddd2_loc = "      ";
	if (tz390.opt_tracea || (gen_obj_code && tz390.opt_list)){
		list_bal_line = true;
	} else {
		list_bal_line = false;
	}
	loc_start = loc_ctr;
    dc_lit_ref = false;  // RPI12
    dc_lit_gen = false;
	bal_op_ok = false;
	switch (tz390.op_type[bal_op_index]){ 
	case 0:  // * comments 
		bal_op_ok = true;
    	if (gen_obj_code && bal_line.length() > 9){
       		if (bal_line.substring(0,9).equals("*MCALL #=")){
       			hex_bddd2_loc = bal_line.substring(19,25);   //put mlc line # in data area
       			bal_line = bal_line.substring(26); //strip * call prefix
       			if (mac_call_level == 0){
       				mac_call_first = true; // delay setting level to print call if nogen
       			} else {
       				mac_call_level++;
       			}
       		} else if (bal_line.substring(0,9).equals("*MEXIT #=")){
   				list_bal_line = false;
       			mac_call_level--;
       		}
       	}
		break;
    case 1:  // "E" 8 PR oooo
    	bal_op_ok = true;
    	loc_ctr = (loc_ctr+1)/2*2;
    	loc_start = loc_ctr;
    	loc_len = 2;
	    get_hex_op(1,4);
    	put_obj_text();
    	break;
    case 2:  // "RR" 60  LR  oorr
    	bal_op_ok = true;
    	loc_ctr = (loc_ctr+1)/2*2;
    	loc_start = loc_ctr;
    	loc_len = 2;
    	get_hex_op(1,2); 
    	get_hex_reg();
    	if (obj_code.substring(0,2).equals("04")){ // SPM
    		obj_code = obj_code.concat("0");
    	} else {
    	    skip_comma();
    	    get_hex_reg();
    	}
    	check_end_parms();
 	    put_obj_text();
    	break;
    case 3:  // "BRX" 16  BER oomr
    	bal_op_ok = true;
    	loc_ctr = (loc_ctr+1)/2*2;
    	loc_start = loc_ctr;
    	loc_len = 2;
    	get_hex_op(1,3);  // BCR OP includes mask
    	get_hex_reg();
    	check_end_parms();
 	    put_obj_text();
    	break;
    case 4:  // "I" 1 SVC 00ii
    	bal_op_ok = true;
    	loc_ctr = (loc_ctr+1)/2*2;
    	loc_start = loc_ctr;
    	loc_len = 2;
	    get_hex_op(1,2);
    	get_hex_byte();
    	check_end_parms();
    	put_obj_text();
    	break;
    case 5:  // "RX" 52  L  oorxbddd
    	bal_op_ok = true;
    	loc_ctr = (loc_ctr+1)/2*2;
    	loc_start = loc_ctr;
    	loc_len = 4;
    	get_hex_op(1,2);
    	get_hex_reg();
    	skip_comma();
    	get_hex_xbddd();
    	check_end_parms();
    	put_obj_text();
    	break;
    case 6:  // "BCX" 16 BE  oomxbddd
    	bal_op_ok = true;
    	loc_ctr = (loc_ctr+1)/2*2;
    	loc_start = loc_ctr;
    	loc_len = 4;
    	get_hex_op(1,3); // BCX op includes mask
    	get_hex_xbddd();
    	check_end_parms();
    	put_obj_text();
    	break;
    case 7:  // "S" 43 SPM oo00bddd
    	bal_op_ok = true;
    	loc_ctr = (loc_ctr+1)/2*2;
    	loc_start = loc_ctr;
    	loc_len = 4;
    	get_hex_op(1,4);
    	get_hex_bddd2(true);
    	check_end_parms();
    	put_obj_text();
    	break;
    case 8:  // "DM" 1  DIAGNOSE 83000000
    	bal_op_ok = true;
    	loc_ctr = (loc_ctr+1)/2*2;
    	loc_start = loc_ctr;
    	loc_len = 4;
    	get_hex_op(1,2); 
    	get_hex_zero(4);
    	put_obj_text();
    	break;
    case 9:  // "RSI" 4 BRXH  oorriiii
    	bal_op_ok = true;
    	loc_ctr = (loc_ctr+1)/2*2;
    	loc_start = loc_ctr;
    	loc_len = 4;
    	get_hex_op(1,2); 
       	get_hex_reg();
    	skip_comma();
    	get_hex_reg();
    	skip_comma();
    	get_hex_rel();
    	check_end_parms();
    	put_obj_text();
    	break;
    case 10:  // "RS" 25  oorrbddd
    	bal_op_ok = true;
    	loc_ctr = (loc_ctr+1)/2*2;
    	loc_start = loc_ctr;
    	loc_len = 4;
    	get_hex_op(1,2); 
       	get_hex_reg();
    	skip_comma();
    	if (hex_op.compareTo("88") >=0
    		&& hex_op.compareTo("8F") <= 0){
    		obj_code = obj_code + "0"; // r3=0 for shift
    	} else {
    	    get_hex_reg();
        	skip_comma();
    	}
    	get_hex_bddd2(true);
    	check_end_parms();
    	put_obj_text();
    	break;
    case 11:  // "SI" 9 CLI  ooiibddd
    	bal_op_ok = true;
    	loc_ctr = (loc_ctr+1)/2*2;
    	loc_start = loc_ctr;
    	loc_len = 4;
    	get_hex_op(1,2); 
    	get_hex_bddd2(true);
    	skip_comma();
    	get_hex_byte();
    	check_end_parms();
    	obj_code = obj_code.substring(0,2) + obj_code.substring(6,8) + obj_code.substring(2,6);
    	put_obj_text();
    	break;
    case 12:  // "RI" 37 IIHH  ooroiiii
    	bal_op_ok = true;
    	loc_ctr = (loc_ctr+1)/2*2;
    	loc_start = loc_ctr;
    	loc_len = 4;
    	get_hex_op(1,2); 
       	get_hex_reg(); 
       	get_hex_op(3,1);
    	skip_comma();
    	get_hex_rel();
    	check_end_parms();
    	put_obj_text();
    	break;
    case 13:  // "BRC" 31 BRE  oomoiiii
    	bal_op_ok = true;
    	loc_ctr = (loc_ctr+1)/2*2;
    	loc_start = loc_ctr;
    	loc_len = 4;
    	get_hex_op(1,2); 
       	get_hex_op(4,1);
       	get_hex_op(3,1);
    	get_hex_rel();
    	check_end_parms();
    	put_obj_text();
    	break;
    case 14:  // "RRE" 185  MSR oooo00rr
    	bal_op_ok = true;
    	loc_ctr = (loc_ctr+1)/2*2;
    	loc_start = loc_ctr;
    	loc_len = 4;
    	get_hex_op(1,4);
    	get_hex_zero(2);
       	get_hex_reg();
   	    if (exp_index >= exp_text.length()
   	    	|| exp_text.charAt(exp_index) != ','){ 
    		obj_code = obj_code.concat("0"); // IPM,EFPC,SFPC
    	} else {
    	    skip_comma();
    	    get_hex_reg();
    	}
    	check_end_parms();
    	put_obj_text();
    	break;
    case 15:  // RPI 206 "RRF1" MA?R, MS?R, MY?R (r1,r3,r2 maps to oooo1032)
    	bal_op_ok = true;
    	loc_ctr = (loc_ctr+1)/2*2;
    	loc_start = loc_ctr;
    	loc_len = 4;
    	get_hex_op(1,4);
    	get_hex_reg();
    	get_hex_zero(1);
    	skip_comma();
    	get_hex_reg();
    	skip_comma();
    	get_hex_reg();
    	check_end_parms();
    	put_obj_text();
    	break;
    case 16:  // "RIL" 6  BRCL  oomollllllll
    	bal_op_ok = true;
    	loc_ctr = (loc_ctr+1)/2*2;
    	loc_start = loc_ctr;
    	loc_len = 6;
    	get_hex_op(1,2); 
    	get_hex_reg();
       	get_hex_op(3,1);
       	skip_comma();
    	get_hex_long();
    	check_end_parms();
    	put_obj_text();
    	break;
    case 17:  // "SS" 32  MVC oollbdddbddd  
    	bal_op_ok = true;
    	loc_ctr = (loc_ctr+1)/2*2;
    	loc_start = loc_ctr;
    	loc_len = 6;
    	get_hex_op(1,2); 
       	get_hex_len_bddd();
       	hex_bddd1     = hex_bddd;
       	hex_bddd1_loc = hex_bddd_loc;
    	obj_code = obj_code + hex_len + hex_bddd1;
       	skip_comma();
    	get_hex_bddd2(true);
    	check_end_parms();
    	put_obj_text();
    	break;
    case 18:  // "RXY" 76 MLG oorxbdddhhoo
    	bal_op_ok = true;
    	loc_ctr = (loc_ctr+1)/2*2;
    	loc_start = loc_ctr;
    	loc_len = 6;
    	get_hex_op(1,2); 
       	get_hex_reg();
       	skip_comma();
    	get_hex_xbdddhh2();
    	get_hex_op(3,2);
    	check_end_parms();
    	put_obj_text();
    	break;
    case 19:  // "SSE" 5  LASP  oooobdddbddd
    	bal_op_ok = true;
    	loc_ctr = (loc_ctr+1)/2*2;
    	loc_start = loc_ctr;
    	loc_len = 6;
    	get_hex_op(1,4); 
       	get_hex_bddd2(true);
       	skip_comma();
    	get_hex_bddd2(true);
    	check_end_parms();
    	put_obj_text();
    	break;
    case 20:  // "RSY" 31  LMG  oorrbdddhhoo
    	bal_op_ok = true;
    	loc_ctr = (loc_ctr+1)/2*2;
    	loc_start = loc_ctr;
    	loc_len = 6;
    	get_hex_op(1,2); 
       	get_hex_reg();
       	skip_comma();
       	get_hex_reg();
       	skip_comma();
    	get_hex_bdddhh2();
    	get_hex_op(3,2);
    	check_end_parms();
    	put_obj_text();
    	break;
    case 21:  // "SIY" 6  TMY  ooiibdddhhoo
    	bal_op_ok = true;
    	loc_ctr = (loc_ctr+1)/2*2;
    	loc_start = loc_ctr;
    	loc_len = 6;
    	get_hex_op(1,2);
    	get_hex_bdddhh2();
    	skip_comma();
    	get_hex_byte();
    	obj_code = obj_code.substring(0,2) + obj_code.substring(8,10) + obj_code.substring(2,8); 
       	get_hex_op(3,2);
    	check_end_parms();
    	put_obj_text();
    	break;
    case 22:  // "RSL" 1  TP  oor0bddd00oo
    	bal_op_ok = true;
    	loc_ctr = (loc_ctr+1)/2*2;
    	loc_start = loc_ctr;
    	loc_len = 6;
    	get_hex_op(1,2); 
       	get_hex_len_bddd();
       	if (hex_len.charAt(0) == '0'){
       		hex_len1 = hex_len.substring(1);
       		hex_bddd1     = hex_bddd;
       		hex_bddd1_loc = hex_bddd_loc;
       	} else {
       		log_error(69,"field 1 hex length > 16 = " + hex_len);
       	}

       	obj_code = obj_code + hex_len1 + "0" + hex_bddd1 + "00";
       	get_hex_op(3,2);
    	check_end_parms();
       	put_obj_text();
    	break;
    case 23:  // "RIE" 4  BRXLG  oorriiii00oo
    	bal_op_ok = true;
    	loc_ctr = (loc_ctr+1)/2*2;
    	loc_start = loc_ctr;
    	loc_len = 6;
    	get_hex_op(1,2);
    	get_hex_reg(); 
    	skip_comma();
       	get_hex_reg();
       	skip_comma();
       	get_hex_rel();
       	get_hex_zero(2);
       	get_hex_op(3,2);
    	check_end_parms();
    	put_obj_text();
    	break;
    case 24:  // "RXE" 28  ADB oorxbddd00oo
    	bal_op_ok = true;
    	loc_ctr = (loc_ctr+1)/2*2;
    	loc_start = loc_ctr;
    	loc_len = 6;
    	get_hex_op(1,2);
    	get_hex_reg(); 
    	skip_comma();
       	get_hex_xbddd();
        get_hex_zero(2);
       	get_hex_op(3,2);
    	check_end_parms();
    	put_obj_text();
    	break;
    case 25:  // "RXF" 8   MAE  oorxbdddr0oo (note r3 before r1)
    	bal_op_ok = true;
    	loc_ctr = (loc_ctr+1)/2*2;
    	loc_start = loc_ctr;
    	loc_len = 6;
    	get_hex_op(1,2);
       	get_hex_reg();
    	skip_comma();
    	get_hex_reg();
    	skip_comma();
       	get_hex_xbddd();
        get_hex_zero(1);
       	get_hex_op(3,2);  
       	obj_code = obj_code.substring(0,2)  // oo 
		         + obj_code.substring(3,4)  // r3
				 + obj_code.substring(4,9)  // xbddd
				 + obj_code.substring(2,3)  // r1
				 + obj_code.substring(9);   // 0oo
    	check_end_parms();
       	put_obj_text();
    	break;
    case 26:   // AP SS2  oollbdddbddd
    	bal_op_ok = true;
    	loc_ctr = (loc_ctr+1)/2*2;
    	loc_start = loc_ctr;
    	loc_len = 6;
    	get_hex_op(1,2); 
       	get_hex_len_bddd();
       	if (hex_len.charAt(0) == '0'){
       		hex_len1 = hex_len.substring(1);
       		hex_bddd1     = hex_bddd;
       		hex_bddd1_loc = hex_bddd_loc;
       	} else {
       		log_error(69,"field 1 hex length > 16 = " + hex_len);
       	}
       	skip_comma();
    	get_hex_len_bddd();
       	if (hex_len.charAt(0) == '0'){
       		hex_len2 = hex_len.substring(1);
       		hex_bddd2     = hex_bddd;
       		hex_bddd2_loc = hex_bddd_loc;
       	} else {
       		log_error(70,"field 2 hex length > 16 = " + hex_len);
       	}
       	obj_code = obj_code + hex_len1 + hex_len2 + hex_bddd1 + hex_bddd2;
    	check_end_parms();
       	put_obj_text();
    	break;
    case 27:   // PLO SS3  oorrbdddbddd  r1,s2,r3,s4
    	bal_op_ok = true;
    	loc_ctr = (loc_ctr+1)/2*2;
    	loc_start = loc_ctr;
    	loc_len = 6;
    	get_hex_op(1,2); 
    	hex_len1 = get_hex_nib();
    	skip_comma();
       	get_hex_len_bddd();
 		hex_bddd1     = hex_bddd;
       	hex_bddd1_loc = hex_bddd_loc;
       	skip_comma();
      	hex_len2 = get_hex_nib();
    	skip_comma();
    	get_hex_len_bddd();
   		hex_bddd2     = hex_bddd;
   		hex_bddd2_loc = hex_bddd_loc;
      	obj_code = obj_code + hex_len1 + hex_len2 + hex_bddd1 + hex_bddd2;
    	check_end_parms();
      	put_obj_text();
    	break;
    case 28:   // LMD SS4  oorrbdddbddd  r1,r3,s2,s4
    	bal_op_ok = true;
    	loc_ctr = (loc_ctr+1)/2*2;
    	loc_start = loc_ctr;
    	loc_len = 6;
    	get_hex_op(1,2); 
    	get_hex_reg();
    	skip_comma();
    	get_hex_reg();
    	skip_comma();
       	get_hex_bddd2(true);
       	skip_comma();
    	get_hex_bddd2(true);
    	check_end_parms();
    	put_obj_text();
    	break;
    case 29:   // SRP SS5  oolibdddbddd s1(l1),s2,i3
    	bal_op_ok = true;
    	loc_ctr = (loc_ctr+1)/2*2;
    	loc_start = loc_ctr;
    	loc_len = 6;
    	get_hex_op(1,2); 
       	get_hex_len_bddd();
       	if (hex_len.charAt(0) == '0'){
       		hex_len1 = hex_len.substring(1);
       		hex_bddd1     = hex_bddd;
       		hex_bddd1_loc = hex_bddd_loc;
       	} else {
       		log_error(69,"field 1 hex length > 16 = " + hex_len);
       	}
       	skip_comma();
    	get_hex_bddd2(false); //RPI120 get bddd2 to add later
        skip_comma();
        hex_len2 = get_hex_nib();
    	obj_code = obj_code + hex_len1 + hex_len2 + hex_bddd1 + hex_bddd2;  //RPI120
    	check_end_parms();
    	put_obj_text();
    	break;
    case 30:  // RPI 206 "RRF3" 30 DIEBR/DIDBR oooormrr (r1,r3,r2,m4 maps to oooo3412) 
    	bal_op_ok = true;
    	loc_ctr = (loc_ctr+1)/2*2;
    	loc_start = loc_ctr;
    	loc_len = 4;
    	get_hex_op(1,4);
    	get_hex_reg();
    	skip_comma();
    	get_hex_reg();
		skip_comma();
    	get_hex_reg();
    	skip_comma();
    	get_hex_reg();
       	obj_code = obj_code.substring(0,4)  // oooo 
        + obj_code.substring(5,6)  // r3
		+ obj_code.substring(7,8)  // m4
		+ obj_code.substring(4,5)  // r1
		+ obj_code.substring(6,7); // r2
    	check_end_parms();
    	put_obj_text();
    	break;
    case 31:  // "SS" PKA oollbdddbddd  ll from S2  
    	bal_op_ok = true;
    	loc_ctr = (loc_ctr+1)/2*2;
    	loc_start = loc_ctr;
    	loc_len = 6;
    	get_hex_op(1,2); 
       	get_hex_len_bddd();
       	hex_bddd1     = hex_bddd;
       	hex_bddd1_loc = hex_bddd_loc;
       	skip_comma();
    	get_hex_len_bddd();
    	hex_bddd2 = hex_bddd;
    	hex_bddd2_loc = hex_bddd_loc;
    	obj_code = obj_code + hex_len + hex_bddd1 + hex_bddd2;
    	check_end_parms();
    	put_obj_text();
    	break;
    case 32:   // SSF MVCOS oor0bdddbddd (s1,s2,r3) "C80" 32 Z9-41
    	bal_op_ok = true;
    	loc_ctr = (loc_ctr+1)/2*2;
    	loc_start = loc_ctr;
    	loc_len = 6;
    	get_hex_op(1,2); 
       	get_hex_len_bddd();
       	hex_bddd1     = hex_bddd;
       	hex_bddd1_loc = hex_bddd_loc;
       	skip_comma();
    	get_hex_bddd2(false); 
        skip_comma();
        hex_len2 = get_hex_nib();
    	obj_code = obj_code + hex_len2 + "0" + hex_bddd1 + hex_bddd2; 
    	check_end_parms();
    	put_obj_text();
    	break;
    case 33:   // "BLX" branch relative on condition long RPI199
    	bal_op_ok = true;
    	loc_ctr = (loc_ctr+1)/2*2;
    	loc_start = loc_ctr;
    	loc_len = 6;
    	get_hex_op(1,2); //BRCL C0 OP
    	get_hex_op(4,1); //BRCL MASK 
    	get_hex_op(3,1); //BRCL 4  OP
    	get_hex_long();
    	check_end_parms();
    	put_obj_text();
    	break;
    case 34:   // RPI 206 CG?R, CF?R, FI?R, IDTE, TB?R (r1,m3,r2 maps to oooo3012)
    	bal_op_ok = true;
    	loc_ctr = (loc_ctr+1)/2*2;
    	loc_start = loc_ctr;
    	loc_len = 4;
    	get_hex_op(1,4);
     	get_hex_reg();
     	get_hex_zero(1);
     	skip_comma();
     	get_hex_reg();
     	skip_comma();
     	get_hex_reg();
    	obj_code = obj_code.substring(0,4)  // oooo
     	    + obj_code.substring(6,7)   // m3
	    	+ "0"
	    	+ obj_code.substring(4,5)  // r1
	    	+ obj_code.substring(7,8); // r2
    	check_end_parms();
    	put_obj_text();
    	break;
    case 101:  // CCW 0 
    	break;
    case 102:  // CCW0 0
    	break;
    case 103:  // CCW1 0 
    	break;
    case 104:  // DC 0
    	bal_op_ok = true;
       	process_dc(1);	
    	break;
    case 105:  // DS 0 
    	bal_op_ok = true;
       	process_dc(1);	    	
    	break;
    case 106:  // ALIAS 0 
    	break;
    case 107:  // AMODE 0 
    	bal_op_ok = true; //RPI122 IGNORE
    case 108:  // CATTR 0 
    	break;
    case 109:  // COM 0 
    	break;
    case 110:  // CSECT 0 
    	bal_op_ok = true;
    	process_sect(sym_cst,bal_label);
    	if (first_cst_esd == 0)first_cst_esd = cur_esd;
    	break;
    case 111:  // CXD 0 
    	break;
    case 112:  // DSECT 0 
    	bal_op_ok = true;
    	process_sect(sym_dst,bal_label);
    	break;
    case 113:  // DXD 0 
    	break;
    case 114:  // ENTRY 0 
    	bal_op_ok = true;
    	process_esd(sym_ent);
    	break;
    case 115:  // EXTRN 0
    	bal_op_ok = true;
        process_esd(sym_ext);
    	break;
    case 116:  // LOCTR 0 
    	bal_op_ok = true;
    	process_sect(sym_lct,bal_label);
    	break;
    case 117:  // RMODE 0 
    	bal_op_ok = true; //RPI122 IGNORE
    	break;
    case 118:  // RSECT 0
    	break;
    case 119:  // START 0
    	break;
    case 120:  // WXTRN 0
    	bal_op_ok = true;
        process_esd(sym_wxt); //RPI182
    	break;
    case 121:  // XATTR 0 
    	break;
    case 123:  // DROP 0
    	bal_op_ok = true;
    	if (gen_obj_code){
            drop_using();
     	}
    	break;
    case 124:  // USING 0 
    	bal_op_ok = true;
    	check_private_csect();
    	if (gen_obj_code){
            add_using();
     	}
     	break;
    case 125:  // AEJECT 0
    	bal_op_ok = true; //RPI122 IGNORE
    	break;
    case 126:  // ASPACE 0
    	bal_op_ok = true; //RPI122 IGNORE
    	break;
    case 127:  // CEJECT 0 
    	bal_op_ok = true; //RPI122 IGNORE
    	break;
    case 128:  // EJECT 0 
    	bal_op_ok = true; //RPI122 IGNORE
    	break;
    case 129:  // PRINT 0 
    	bal_op_ok = true; 
    	if (gen_obj_code){
    		process_print();
    	}
    	break;
    case 130:  // SPACE 0 
    	bal_op_ok = true; //RPI122 IGNORE
    	break;
    case 131:  // TITLE 0 
    	bal_op_ok = true;
    	bal_label = null; // RPI 131
    	break;
    case 132:  // ADATA 0 
    	break;
    case 133:  // CNOP 0 
    	bal_op_ok = true;
    	process_cnop();
    	break;
    case 224:  // COPY 0 
    	bal_op_ok = true;  // already expanded in mz390
    	break;
    case 225:  // OPSYN
    	bal_op_ok = true;
    	opsyn_label = bal_label;  // save opsyn target
    	bal_label = null;         // reset to avoid dup. label
    	tz390.opsyn_opcode_update(opsyn_label,bal_parms);
    	break;
    case 135:  // END 0 
    	bal_op_ok = true;
    	end_found = true;
    	if (cur_esd > 0){
      		update_sect_len();
    	}
   		list_bal_line();
    	if (tot_lit > 0){
    		cur_esd = 1;
			while (cur_esd <= tot_esd 
					&& sym_type[esd_sid[cur_esd]] != sym_cst){
				cur_esd++;
			}
    		if (cur_esd <= tot_esd){
    			cur_esd_sid = esd_sid[cur_esd];
   	   	    	while (sym_sect_next[cur_esd_sid] > 0){
   	   	    		cur_esd_sid = sym_sect_next[cur_esd_sid];
   	   	    	}
   	   	        loc_ctr = (sym_loc[cur_esd_sid] + sym_len[cur_esd_sid] + 7)/8*8;
    			gen_ltorg();
    			update_sect_len();
    		} else {
    			cur_esd = 0;
    		}
    	}
    	bal_eof = true;
    	put_obj_text(); // flush buffer
    	update_sects();
    	loc_ctr = 0;
    	cur_esd = 0;
    	break;
    case 136:  // EQU 0
    	bal_op_ok = true;  	
    	process_equ();
    	break;
    case 137:  // EXITCTL 0 
    	break;
    case 138:  // ICTL 0
    	break;
    case 139:  // ISEQ 0
    	break;
    case 140:  // LTORG 0
    	bal_op_ok = true;
   		list_bal_line();
    	if (tot_lit > 0
    		&& cur_esd > 0
    		&& sym_type[esd_sid[cur_esd]] == sym_cst){
     	   gen_ltorg();
     	}
    	cur_lit_pool++;
    	break;
    case 141:  // OPSYN 0 
    	break;
    case 142:  // ORG 0 
    	bal_op_ok = true;
    	process_org();
    	break;
    case 143:  // POP 0 
    	bal_op_ok = true;
    	if (gen_obj_code){
    		process_pop();
    	}
    	break;
    case 223:  // PUNCH 0
    	bal_op_ok = true; // pass thru after gen by mz390
    	break;
    case 145:  // PUSH 0 
    	bal_op_ok = true;
    	if (gen_obj_code){
    		process_push();
    	}
    	break;
    case 146:  // REPRO 0
    	break;
    case 201:  // ACTR 0
    	break;
    case 202:  // AGO 0
    	break;
    case 203:  // AIF 0
    	break;
    case 204:  // AINSERT 0
    	break;
    case 205:  // ANOP 0
    	break;
    case 206:  // AREAD 0
    	break;
    case 207:  // GBLA 0
    	break;
    case 208:  // GBLB 0
    	break;
    case 209:  // GBLC 0
    	break;
    case 210:  // LCLA 0
    	break;
    case 211:  // LCLB 0
    	break;
    case 212:  // LCLC 0
    	break;
    case 213:  // MHELP 0 
    	break;
    case 214:  // MNOTE 0
    	bal_op_ok = true;  // pass true from mz390
    	break;
    case 215:  // SETA 0
    	break;
    case 216:  // SETAF 0
    	break;
    case 217:  // SETB 0
    	break;
    case 218:  // SETC 0
    	break;
    case 219:  // SETCF 0
    	break;
    case 220:  // MACRO 0
    	break;
    case 221:  // MEND 0
    	break;
    case 222:  // MEXIT 0 
        break;
    default:
    	// should not occur - see tz390 opcode_type table
    	abort_error(139,"invalid opcode type index");
	}
	if (!bal_op_ok){
   	   log_error(62,"undefined operation");
	}
    list_bal_line();
	if (bal_label != null){
       update_label();
	}
	loc_ctr = loc_ctr + loc_len;
}
private void list_bal_line(){
	/*
	 * list bal line with first 8 bytes of
	 * object code if any 
	 * and turn off list_bal_line request
	 * Notes:
	 *   1.  See comments processing case 0
	 *       for update of mac_call_level,
	 *       call reformating, and delay flags
	 *       mac_call_first and mac_call_last.
	 */
	    if (!list_bal_line || (!tz390.opt_list && !tz390.opt_tracea)){
	    	return;
	    }
	    if (list_obj_code.length() < 16){
	    	list_obj_code = list_obj_code.concat("                ").substring(0,16);
	    } 
	    list_obj_loc = loc_start;
	    put_prn_line(tz390.get_hex(list_obj_loc,6) + " " + list_obj_code.substring(0,16) + " " + hex_bddd1_loc + " " + hex_bddd2_loc + " " + bal_line);
	    list_bal_line = false; 
	    if (mac_call_first){
	    	mac_call_level = 1;
	    	mac_call_first = false;
	    } 
	    if (mac_call_last){
	    	mac_call_level = 0;
	    	mac_call_last = false;
	    }
}
private void add_rld(int exp_esd){
	/*
	 * add plus rld
	 */
	if (tot_exp_rld_add < max_exp_rld){
		exp_rld_add_esd[tot_exp_rld_add] = exp_esd;
		tot_exp_rld_add++;
	}
}
private void sub_rld(int exp_esd){
	/*
	 * sub rld
	 */
	if (tot_exp_rld_sub < max_exp_rld){
		exp_rld_sub_esd[tot_exp_rld_sub] = exp_esd;
		tot_exp_rld_sub++;
	}
}
private void reduce_exp_rld(){
	/*
	 * reduce rld on stack 
	 */
	int index1 = 0;
	int index2 = 0;
	while (index1 < tot_exp_rld_add){
		index2 = 0;
		while (index2 < tot_exp_rld_sub){
			if (exp_rld_add_esd[index1] == exp_rld_sub_esd[index2]){
				tot_exp_rld_add--;
				if (index1 < tot_exp_rld_add -1){
					exp_rld_add_esd[index1] = exp_rld_add_esd[tot_exp_rld_add];
					index1--;
				}
				tot_exp_rld_sub--;
				if (index2 < tot_exp_rld_sub - 1){
					exp_rld_sub_esd[index2] = exp_rld_sub_esd[tot_exp_rld_sub];
				}
			}
			index2++;
		}
		index1++;
	}
	if ((tot_exp_rld_add + tot_exp_rld_sub) == 0){
		exp_type = sym_sdt;
		return;
	} else if (tot_exp_rld_add == 1 && tot_exp_rld_sub == 0){
		exp_type = sym_rel;
		exp_esd = exp_rld_add_esd[0];
	} else { 
		exp_type = sym_rld;
	}
}
private void gen_exp_rld(){
	/*
	 * generate rlds for expression
	 * Notes:
	 *   1.  convert to rel csect vs rel module
	 *       offsets for linker use. 
	 */
	int index1 = 0;
	int index2 = 0;
	if (exp_rld_len > 0){
		index1 = 0;
		while (index1 < tot_exp_rld_add){
			if (tot_rld < max_rld){
				rld_fld_esd[tot_rld] = cur_esd;
				rld_fld_loc[tot_rld] = loc_ctr - sym_loc[esd_sid[cur_esd]]; 
				rld_fld_len[tot_rld] = exp_rld_len;
				rld_fld_sgn[tot_rld] = rld_add;
				rld_xrf_esd[tot_rld] = exp_rld_add_esd[index1];
				exp_val = exp_val - sym_loc[esd_sid[exp_rld_add_esd[index1]]]; 
				tot_rld++;
			} else {
				abort_error(103,"rld table exceeded");
			}
			index1++;
		}
		index2 = 0;
		while (index2 < tot_exp_rld_sub){
			if (tot_rld < max_rld){
				rld_fld_esd[tot_rld] = cur_esd;
				rld_fld_loc[tot_rld] = loc_ctr - sym_loc[esd_sid[cur_esd]]; 
				rld_fld_len[tot_rld] = exp_rld_len;
				rld_fld_sgn[tot_rld] = rld_sub;
				rld_xrf_esd[tot_rld] = exp_rld_sub_esd[index2];
				exp_val = exp_val + sym_loc[esd_sid[exp_rld_sub_esd[index2]]]; 
				tot_rld++;
			} else {
				abort_error(103,"rld table exceeded");
			}
			index2++;
		}
	}
}
private void gen_obj_rlds(){
	/*
	 * write RLD's to the OBJ file in ascii hex
	 */
	if (tot_rld > 0 && tz390.opt_list){
		put_prn_line("Relocation Definitions");
	}
	int index = 0;
	while (index < tot_rld){
		String rld_code = 
			" ESD=" + tz390.get_hex(rld_fld_esd[index],4)
		  + " LOC=" + tz390.get_hex(rld_fld_loc[index],8)
		  + " LEN=" + tz390.get_hex(rld_fld_len[index],1)
		  + " SIGN=" + rld_fld_sgn[index]
		  + " XESD=" + tz390.get_hex(rld_xrf_esd[index],4)
		  ;
		if (tz390.opt_list){	
            put_prn_line(rld_code);
		}
       	put_obj_line(".RLD" + rld_code);
		index++;
	}
}
private void gen_sym_list(){
	/*
	 * list symbols in alpah order 
	 * with optional cross reference
	 */
	 put_prn_line("" +
	 		"\r\nSymbol Table Listing\r\n");
	 TreeSet<String> sort_sym = new TreeSet<String>();
	 int index = 1;
	 while (index <= tot_sym){
		 sort_sym.add(sym_name[index] + (sort_index_bias + index));
		 index++;
	 }
	 Iterator<String> sym_key_it = sort_sym.iterator();
	 while (sym_key_it.hasNext()){
	 	String key = sym_key_it.next();
	 	index = Integer.valueOf(key.substring(key.length()-sort_index_len)) - sort_index_bias;	 	
	 	String name = sym_name[index];
	 	if (name.length() < 8){
	 		name = name.concat("       ").substring(0,8);
	 	}
	 	String sym_line = " SYM=" + name
		           + " LOC=" + tz390.get_hex(sym_loc[index],8) 
		           + " LEN=" + tz390.get_hex(get_sym_len(index),8)
		           + " ESD=" + tz390.get_hex(sym_esd[index],4) 
	 			   + " TYPE=" + sym_type_desc[sym_type[index]] 
				   ; 
        if (tz390.opt_xref){
        	sym_line = sym_line + "  XREF=";
        	if (sym_def[index] != 0){
        		sym_line = sym_line + bal_line_num[sym_def[index]] + " ";
        	}
        	Iterator<Integer> sym_xref_it = sym_xref[index].iterator();
        	while (sym_xref_it.hasNext()){
        		int sym_xref_num = sym_xref_it.next();
        		if (sym_xref_num != bal_line_num[sym_def[index]]){
        			sym_line = sym_line + sym_xref_num + " ";
        			if (sym_line.length() > max_line_len){
        				put_prn_line(sym_line);
        				sym_line = "  ";
        			}
        		}
        	}
        	if (sym_line.length() > 2){
        		put_prn_line(sym_line);
        	}
        } else {
        	put_prn_line(sym_line);
        }
	 }
}
private void gen_lit_xref_list(){
	/*
	 * list literals in alpha order
	 * with optional cross reference
	 */
	 put_prn_line("\r\nLiteral Table Listing\r\n");
	 TreeSet<String> sort_lit = new TreeSet<String>();
	 int index = 0;
	 while (index < tot_lit){
		 sort_lit.add(lit_name[index] + (sort_index_bias + index));
		 index++;
	 }
	 Iterator<String> lit_key_it = sort_lit.iterator();
	 while (lit_key_it.hasNext()){
	 	String key = lit_key_it.next();
	 	cur_lit = Integer.valueOf(key.substring(key.length()-sort_index_len)) - sort_index_bias;
 	    String lit = lit_name[cur_lit];
	 	if (lit.length() < 8){
	 		lit = lit.concat("       ").substring(0,8);
	 	}
	 	String lit_line = " LIT=" + lit 
                        + " LOC=" + tz390.get_hex(lit_loc[cur_lit],8) 
                        + " LEN=" + tz390.get_hex(lit_len[cur_lit],8)
		                + " ESD=" + tz390.get_hex(lit_esd[cur_lit],4) 
		                + " POOL=" + tz390.get_hex(lit_pool[cur_lit],4)
		                ;
        if (tz390.opt_xref){
		    lit_line = lit_line + " XREF=";
		    Iterator<Integer> lit_xref_it = lit_xref[cur_lit].iterator();
		    while (lit_xref_it.hasNext()){
		    	int lit_xref_num = lit_xref_it.next();
		    	lit_line = lit_line + lit_xref_num + " ";
		    	if (lit_line.length() > max_line_len){
		    		put_prn_line(lit_line);
		    		lit_line = "  ";
		    	}
		    }
       	}
       	if (lit_line.length() > 2){
       		put_prn_line(lit_line);
       	}
	 }
	 put_prn_line(" ");
}
private void load_bal(){
	/*
	 * load bal source
	 * 
	 * 1.  Concatentate any continuations indicated
	 *     by non-blank in position 72.  Each 
	 *     continuation must start at position 16.
	 */
        bal_file = new File(tz390.dir_bal + tz390.pgm_name + tz390.pgm_type);
   	    try {
   	        bal_file_buff = new BufferedReader(new FileReader(bal_file));
   	    } catch (IOException e){
   		    abort_error(6,"I/O error on bal open - " + e.toString());
   	    }
        if (tz390.opt_tracea){
      	  	 put_log("TRACE LOADING " + tz390.pgm_dir + tz390.pgm_name + tz390.pgm_type);
        }
		get_bal_line();
		while (!bal_eof && bal_line != null
				&& tot_bal_line < max_bal_line){
			bal_line_index = tot_bal_line;
			bal_name_line[tot_bal_line] = bal_line;
			bal_line_num[tot_bal_line] = cur_line_num;
			parse_bal_line();
            bal_op_index = find_bal_op();
            if (bal_op_index != -1){
	           	process_bal_op();    
	        }
			if  (bal_line != null){
				tot_bal_line++;
	            get_bal_line();
			}
		}
		if (tot_bal_line >= max_bal_line){
			abort_error(83,"maximum source lines exceeded");
		}
        if (tz390.opt_tracea){
         	put_log("TRACE BAL LOADING PASS " + cur_pass + " TOTAL ERRORS = " + az390_errors);
        }
		try {
		    bal_file_buff.close();
		} catch (IOException e){
			abort_error(7,"I/O error on BAL file close " + e.toString());
		}
}
private void get_bal_line(){
	/*
	 * get next bal line from bal file
	 * concatenating continuation lines
	 */
	String temp_line;
    try {
        temp_line = bal_file_buff.readLine();
        cur_line_num++;
    	if  (temp_line == null){
    			bal_line = null;
   		} else if (temp_line.length() < 72
   				   || temp_line.charAt(71) <= ' '){  //RPI181
   			bal_line = trim_line(temp_line);  //RPI124
   		} else {
   		    bal_line = temp_line.substring(0,71);
   		    bal_line = trim_continue(bal_line);
            while (temp_line.length() > 71
            		&& temp_line.charAt(71) > ' '){  //RPI181
            	    temp_line = bal_file_buff.readLine();
            	    if (temp_line.length() < 72 || temp_line.charAt(71) <= ' '){ //RPI181
            	    	temp_line = trim_line(temp_line); //RPI124
            	    }
            	    cur_line_num++;
            	    if  (temp_line.length() >= 16
            	    	&& temp_line.substring(0,15).equals("               ")){ // RPI167
            	    	int temp_end = temp_line.length();
            	    	if (temp_end > 71)temp_end = 71;  
            	    	bal_line = bal_line.concat(temp_line.substring(15,temp_end));
               		    bal_line = trim_continue(bal_line);
            	    } else { 
            	    	log_error(8,"continuation line < 16 characters - " + temp_line);
            	    }
            }   
   		}
    } catch (IOException e){
       	abort_error(9,"I/O error on file read " + e.toString());
    }
}
private void parse_bal_line(){
	/*
	 * set bal_label and bal_op
	 */
	if (tz390.opt_tracea){
		put_log("TRACE BAL PARSING       " + bal_line);
	}
	bal_abort = false;
	bal_label = null;
	bal_op    = null;
	bal_parms = null;
	if  (bal_line == null 
		 || bal_line.length() == 0
		 || bal_line.charAt(0) == '*'){
		return;
	} 
    String[] tokens = split_line(bal_line);
    bal_label = tokens[0];
    if (tokens[1] != null){
    	bal_op    = tokens[1].toUpperCase();
    } else {
    	bal_op = null;
    }
    bal_parms = tokens[2];
}
private String[] split_line(String line){
	/*
	 * split line into label, opcode, parms 
	 * 
	 * 3 fields are null if none and 
	 * there may be trailing comment on parms
	 */
	String[] tokens = line.split("\\s+",3);
	String[] return_tokens = new String[3];
    if (tokens.length > 0){
       if  (line.charAt(0) > ' '){ //RPI181
           return_tokens[0] = tokens[0];  //label with substitiution
       }
       if  (tokens.length > 1){
       	   return_tokens[1] = tokens[1]; // opcode with substitution
       }
       if  (tokens.length > 2){
    	   return_tokens[2] = tokens[2]; // parms with substitution
       }
    }
    return return_tokens;
}
private int find_bal_op(){
	/*
	 * return index of bal operation 
	 * or return -1 if undefined operation
	 * 
	 * return 0 for comments
	 */
	int index = 0;
	if  (bal_op != null 
		 && bal_op.length() > 0){
		index = tz390.find_key_index("O:" + bal_op);
		if (index != -1){
			return index;
		}
	    log_error(29,"undefined operation code - " + bal_op);
	    return -1;
	} 
	if (bal_line.length() == 0 || bal_line.charAt(0) == '*'){
		return 0;
	} else {
		log_error(71,"missing opcode - " + bal_line);
		return - 1;
	}
}
private void process_esd(byte esd_type){
	/*
	 * process EXTRN, ENTRY, or WXTRN statement
	 */
	String token = null;
	boolean extrn_eod = false;
    extrn_match = extrn_pattern.matcher(bal_parms);
	while (!bal_abort && !extrn_eod
			&& extrn_match.find()){
	       token = extrn_match.group();
	       switch (token.charAt(0)){
	       case ',':
	    	   break;
	       case '\t':  //tab  RPI181
	       case '\r':  //cr
	       case '\n':  //lf
	       case ' ':   //space
	           extrn_eod = true;
	           break;
	       default:
	    	   switch (esd_type){
	    	   case 3: // sym_ent
	    	   	   cur_sid = find_sym(token);
	               if (cur_sid != -1){
	        	      add_entry(token);
	        	   } else {
	        		   log_error(124,"ENTRY not found - " + token);
	        	   }
	               break;
    	       case 4: // sym_ext
    	    	   cur_sid = find_sym(token);
    	    	   if (cur_sid == -1){
    	    		   add_extrn(token);
    	    	   }
    	    	   break;
    	       case 8: // sym_wxt
    	    	   cur_sid = find_sym(token);
    	    	   if (cur_sid == -1){
    	    		   add_wxtrn(token);
    	    	   } else if (sym_type[cur_sid] == sym_ext){
        	    		sym_type[cur_sid] = sym_wxt;
    	    	   }
    	    	   break;
	    	   }
	       }
	}
}
private void add_extrn(String token){
	/*
	 * add EXTRN 
	 */
	   cur_sid = add_sym(token);
	   sym_type[cur_sid] = sym_ext;
	   sym_esd[cur_sid] = add_esd(cur_sid,sym_ext);
}
private void add_wxtrn(String token){
	/*
	 * add WXTRN 
	 */
	   cur_sid = add_sym(token);
	   sym_type[cur_sid] = sym_wxt;
	   sym_esd[cur_sid] = add_esd(cur_sid,sym_wxt);
}
private void add_entry(String token){
	/*
	 * add ENTRY 
	 */
	   if (sym_type[cur_sid] == sym_rel){
           int index = 1;
           while (index <= tot_esd){ 
        	   if (esd_sid[index] == cur_sid){
        		   return;  // ESD already defined
        	   }
        	   index++;
           }
		   add_esd(cur_sid,sym_ent);
       } else {
		   log_error(97,"invalid entry type symbol - " + sym_name[cur_sid]);
		   cur_sid = -1;
       }
}
private void process_sect(byte sect_type,String sect_name){
	/*
	 * add or update csect, dsect, or loctr
	 * indicated by sym_cst, sym_dst, or sym_lct type parm
	 * Steps:
	 *   1.  Update previous section if any with 
	 *       max length and any loctr pointers
	 *   2.  Add new section if not found or external
	 *       reference found as local label.
	 *   3.  Reset location counter to end of 
	 *       current section.
	 *   4.  Update prev section type and sid for
	 *       use in processing sym_lct sections.
	 */
	if (sect_name.length() == 0){
		sect_name = "$PRIVATE";  // private code
	}
	if (cur_esd_sid > 0){
		update_sect_len();
	}
	cur_esd_sid = find_sym(sect_name);
	if (cur_esd_sid < 1){
	   cur_sid = add_sym(sect_name);
   	   cur_esd = add_esd(cur_sid,sect_type);
	}
	if  (cur_esd_sid < 1    // new section or extrn redefine
		|| (sym_type[cur_esd_sid] == sym_ext
				|| sym_type[cur_esd_sid] == sym_wxt)){  //RPI182
		if (sect_type != sym_lct){
		   loc_ctr = (loc_ctr + 7)/8*8;
		}
        if (cur_esd_sid < 1){
    		cur_esd_sid = cur_sid; // new sect sid
        } else {
        	cur_sid = cur_esd_sid;          // cvt ext to csect
        	cur_esd = sym_esd[cur_esd_sid]; 
        }
	   	esd_sid[cur_esd]  = cur_sid;
		sym_esd[cur_sid]  = cur_esd;
		sym_def[cur_sid]  = bal_line_index;
		sym_type[cur_sid] = sect_type;
		sym_loc[cur_sid]  = loc_ctr;
		sym_len[cur_sid]  = 0;
		add_sym_xref(cur_sid);
     	if (sect_type == sym_lct){
     		if (prev_sect_type != 0){
     			sym_sect_prev[cur_esd_sid] = prev_sect_sid;
        	    sym_sect_next[prev_sect_sid] = cur_esd_sid;
        	    sym_type[cur_esd_sid] = prev_sect_type;
        	    sym_esd[cur_esd_sid]  = prev_sect_esd;
        	} else {
        	 	log_error(90,"LOCTR must follow CSECT or DSECT");
        	    sym_type[cur_esd_sid]  = sym_cst;
        	}
        }
	} else {  // update prev section
		cur_esd = sym_esd[cur_esd_sid];
        loc_ctr = sym_loc[cur_esd_sid] + sym_len[cur_esd_sid];
	}
	prev_sect_type = sym_type[cur_esd_sid];
	prev_sect_esd  = sym_esd[cur_esd_sid];
	prev_sect_sid = cur_esd_sid;
	loc_start = loc_ctr;
}
private int find_sym(String name){
	/*
	 * return symbol index else -1
	 * abort if time exceeded
	 */
	name = name.toUpperCase();
	tot_sym_find++;
	if (tz390.opt_time
		&& (tot_sym_find > next_time_check)){
		next_time_check = tot_sym_find + next_time_ins;
		cur_date = new Date();
		tod_end = cur_date.getTime();
	    if (tod_end > tod_time_limit){
           abort_error(80,"time limit exceeded");
		}
	}
	int index = tz390.find_key_index("S:" + name);
	if (index != -1){
        add_sym_xref(index);
		return index;
	} else if (dcv_type){
		add_extrn(name);
	}
	return index;
}
private void update_label(){
	/*
	 * add or update relative labels
	 * and exclude CST, DST, EQU, USING symbols
	 */
	cur_sid = find_sym(bal_label);
	if (cur_sid < 1){
	   if (bal_op.equals("USING")){
		   return;
	   }
	   cur_sid = add_sym(bal_label);
	   sym_def[cur_sid]  = bal_line_index;
	   sym_type[cur_sid] = sym_rel;
	   sym_esd[cur_sid]  = cur_esd;
	   sym_loc[cur_sid] = loc_start;
	   if (loc_len == 0){
		   sym_len[cur_sid] = dc_first_len;
	   } else {
		   sym_len[cur_sid] = loc_len;
	   }
	} else if (sym_def[cur_sid] == bal_line_index){
		if (sym_type[cur_sid] == sym_rel
		    && !bal_op.equals("EQU")){
	 	    sym_loc[cur_sid] = loc_start;
	   	    if (loc_len == 0){
	   	        sym_len[cur_sid] = dc_first_len;
	   	    } else {
	   	        sym_len[cur_sid] = loc_len;
	   	    }
	   	}
	} else if (sym_type[cur_sid] != sym_cst
			&& sym_type[cur_sid] != sym_dst
			&& sym_type[cur_sid] != sym_ext
			&& sym_type[cur_sid] != sym_wxt) { //RPI182
		duplicate_symbol_error();
	}
}
private int get_sym_len(int index){
	/*
	 * return total length of csect or dsect
	 */
    switch (sym_type[index]){
    case 1: // CSECT or CSECT LOCTR
    case 2: // DSECT or DSECT LOCTR
    	int tot_len = sym_max_loc[index] - sym_loc[index];
	    if (sym_sect_prev[index] == 0){
    	   while (sym_sect_next[index] > 0){
		         index = sym_sect_next[index];
		         tot_len = tot_len + sym_max_loc[index] - sym_loc[index];
	       }
    	   tot_len = (tot_len + 7)/8*8;
	    }
	    return tot_len;
    default: // other types (abs, rel, etc.)
    	return sym_len[index];
    }
}
private void update_sect_len(){
	/*
	 * update length of current section
	 */
	 if (loc_ctr - sym_loc[cur_esd_sid] > sym_len[cur_esd_sid]){
	 	sym_len[cur_esd_sid] = loc_ctr - sym_loc[cur_esd_sid];
	 }
}
private void process_dc(int request_type){
    /*
     * processing by request type:
     * 1.  parse ds/dc bal statement and allocate
     *     or gen data items including alignment
     *     bytes where required.
     * 2.  find or add literal table entry using
     *     ds/dc type single parm following = parm
     * 3.  generate literal pool dc using loc_ctr
     *     set to referenced instruction.
     * 
     * if LTORG, gen lits
     * 
     * if not DC/DS/LTORG set dc_lit and process first
     * field as literal and update exp_index
     */
	 switch (request_type){
	     case 1: // process ds/dc statements
      	     dc_field = bal_parms;
	 	     dc_index = 0;
	         dc_lit_ref = false;
	         dc_lit_gen = false;
	         if (bal_op.equals("DC")
	 	         && gen_obj_code
		         && sym_type[cur_esd_sid] == sym_cst){
			 	 if (gen_obj_code && sym_type[cur_esd_sid] == sym_cst){
			 	 	dc_op = true;
			 	 } else { 
			 	 	dc_op = false;
			 	 }
	         } else if (bal_op.equals("DS")){
	 	        dc_op = false;
	         }
	         break;
	     case 2:  // find or add literal table entry 
	    	 lit_loc_ref = false;
	 	     dc_field = exp_text;
		     dc_index = exp_index + 1;
	 	     dc_lit_ref = true;
	 	     dc_lit_gen = false;
	 	     dc_op = false;
		     dc_lit_index_start = dc_index;
		     break;
		 case 3:  // generate literal table entry
		 	 dc_field = lit_name[cur_lit];
		 	 dc_index = 0;
		 	 obj_code = "";
		 	 list_obj_code = "";
		 	 dc_lit_ref = false;
		 	 dc_lit_gen = true;
		 	 if (gen_obj_code && sym_type[cur_esd_sid] == sym_cst){
		 	 	dc_op = true;
		 	 } else { 
		 	 	dc_op = false;
		 	 }
		 	 break;
	 }
	 dc_first_field = true;
	 dc_len_explicit = false;
	 while (!bal_abort 
			&& dc_index < dc_field.length()){
	       if (dc_field.charAt(dc_index) == ',' 
	       	   && !dc_lit_ref){
	       	  dc_index++;
	       } else if (dc_field.charAt(dc_index) <= ' '){ //RPI181
	       	  dc_index = dc_field.length();  // flush trailing comments
	       }
	       get_dc_field_dup();
	       get_dc_field_type();
	       get_dc_field_len(); // align and set length
	       if  (dc_index < dc_field.length() 
	       		&& dc_field.charAt(dc_index) != ','
	       	    && dc_field.charAt(dc_index) > ' '){ //RPI181
	       	   if (bal_abort || dc_field.charAt(dc_index) 
	       	  		!= tz390.dc_type_delimiter[dc_type_index]){
	       		  if (dc_type != 'C' || 
	       				  (dc_field.charAt(dc_index) != '"'      //RPI5
	       			       && dc_field.charAt(dc_index) != '!')){ //RPI73  
	       			  log_error(45,"invalid dc delimiter for type - " + dc_field.substring(0,dc_index+1));
	       		      return;
	       		  }
	       	   }
	       	   dc_eod = false;
	           switch (dc_type){
		          case 'A': // (exp1,,expn)
		       	     process_dca_data();
   	  	             break;
   	  	       	  case 'B': // '0|1,0|1'
  	  	       	  	 process_dcb_data();
  	  	       	  	 break;
   	  	          case 'C': // 'text'
   	  	       	     process_dcc_data();
   	  	       	     break;
   	  	       	  case 'D': // 'fp,fp'
  	  	       	  	 process_dc_fp_data();
  	  	       	  	 break;
   	  	       	  case 'E': // 'fp,fp'
  	  	       	  	 process_dc_fp_data();
  	  	       	  	 break;
   	  	       	  case 'F': // 'int,int'
   	  	       	  	 process_dcf_data();
   	  	       	  	 break;
   	  	       	  case 'H': // 'int,int'
  	  	       	  	 process_dch_data();
  	  	       	  	 break;
   	  	       	  case 'L': // 'fp,fp'
 	  	       	  	 process_dc_fp_data();
 	  	       	  	 break;
  	  	       	  case 'P': // 'int,int'
  	  	       	  	 process_dcp_data();
  	  	       	  	 break;
  	  	       	  case 'S': // (exp1,expn)
  	  	       	     process_dcs_data();
  	  	       	  break;
  	  	       	  case 'V': // (exp1,expn)
  	  	      		 if (cur_esd > 0 && sym_type[cur_esd_sid] == sym_cst){
  	  	       			 dcv_type = true;
  	  	       			 process_dca_data();
  	  	       			 dcv_type = false;
  	  	      		 } else {
  	  	      			 dc_op = false;
  	  	       			 process_dca_data();
  	  	      		 }
   	  	       	  	 break;	 
   	  	       	  case 'X': // 'int,int'
  	  	       	  	 process_dcx_data();
  	  	       	  	 break;
		          case 'Y': // (exp1,,expn) length 2
			       	     process_dca_data();
	   	  	             break;
   	  	          case 'Z': // 'zoned decimals'
    	  	       	     process_dcc_data();
    	  	       	     break;
   	  	          default:
   	  	       	     log_error(44,"invalid dc type delimiter");
   	  	       	     break;
	           }
	       } else {
	       	   if (!dc_op && !dc_lit_ref){
	       	   	  loc_ctr = loc_ctr + dc_dup * dc_len;
	       	   	  dc_len = 0;
	       	   } else {
	       	   	  log_error(46,"missing dc type delimiter");
	       	   }
	       }
	       dc_first_field = false;
	       if (dc_lit_ref || dc_lit_gen){
	   	      if (dc_lit_gen && !gen_obj_code){
		   	     lit_loc[cur_lit] = loc_start;
		      }
	 	   	  exp_index = dc_index;
	 	   	  dc_lit_ref = false;
			  dc_lit_gen = false;
	 	   	  return;
	       }
	       if (!(dc_index < dc_field.length()) 
	       		|| dc_field.charAt(dc_index) <= ' '  //RPI181
	       		|| dc_field.charAt(dc_index) == tz390.dc_type_delimiter[dc_type_index]){ 
	       	  return;
	       }
	 }
}
private boolean calc_abs_exp(){
	/*
	 * calculate abs value else abort
	 */
	if (calc_exp()
			&& exp_esd == 0){
		return true;
	} else {
		log_error(32,"invalid absolute value");
	}
	return false;
}
private boolean calc_rel_exp(){
	/*
	 * calculate rel value else abort
	 */
	if (calc_exp()
			&& exp_esd > 0){
		return true;
	} else {
		log_error(33,"invalid relative value");
	}
	return false;
}
private boolean calc_dca_exp(){
	/*
	 * set dca_ignore_refs for A and V type
	 * symbol refs if DS or DSECT
	 */
	if (!dc_op || dcv_type){
		dca_ignore_refs = true;
		boolean temp_rc = calc_exp();
		dca_ignore_refs = false;
		return temp_rc;
	} else {
		return calc_exp();
	}
}

private boolean calc_exp(){
	/*
	 * parse abs/rel expression starting at
	 * exp_text.charAt(exp_index)
	 * return true if ok and set
	 * 1. exp_val = abs or rel offset
	 * 2. exp_esd = abs 0 or cst/dst esd
	 * 3. exp_index = index to terminator
	 * which can be end of string, or (),
	 */
	   if (exp_text == null || exp_index > exp_text.length()){
	   	  return false;
	   }
       exp_match = exp_pattern.matcher(exp_text.substring(exp_index));
       exp_state = 1;
       exp_term = false;
       exp_eot  = false;
       exp_first_len = false;
       exp_len = 1;
       tot_exp_stk_sym = 0;
       tot_exp_stk_op  = 0;
       tot_exp_rld_add = 0;
       tot_exp_rld_sub = 0;
       exp_sym_pushed = false;
       exp_sym_last = false;
   	   exp_level = 0;
   	   exp_op = " ";
	   while (!exp_term && !bal_abort){
	   	   if (!exp_op.equals(exp_term_op) && exp_match.find()){
	          exp_token = exp_match.group();
	          exp_index = exp_index + exp_token.length();
	   	   } else {
	   	   	  exp_token = "" + exp_term_op;
	   	   	  exp_eot = true;
	   	   }
		   proc_exp_token();
	   }
	   if (!bal_abort){
		   if (!exp_eot){
			   exp_index--;  // backup to terminator
		   }
   	      return true;
       } else {
       	  return false;
       }
}
private void proc_exp_token(){
	/*
	 * parse general expression tokens
	 *   1. push sym or sdt
	 *   2. exec or push operations + - * /
	 *   3. terminate on end of string or (),
	 */
	check_prev_op = true;
	while (check_prev_op && !bal_abort){
	    exp_op = exp_token.toUpperCase();
	    switch (exp_op.charAt(0)){
	        case '+':
	        	if (!exp_sym_pushed){
	        		exp_token = "U+";
	        		exp_op = exp_token;
	        	}
	        	proc_exp_op();
	            break;
	        case '-':
	        	if (!exp_sym_pushed){
	        		exp_token = "U-";
	        		exp_op = exp_token;
	        	}
	        	proc_exp_op();
	            break;
	        case '*':
	        	if  (exp_sym_last){
	        	    proc_exp_op();
	        	} else {
	        		proc_loc_ctr();
	        	}
	            break;
	        case '/':
	        	proc_exp_op();
	            break;
	        case '(':
	        	if (exp_level == 0
	        		&& exp_sym_last
	        		&& tot_exp_stk_sym > 0){
	        		exp_op = exp_term_op;
	        	}
	        	proc_exp_op();
	            break;
	        case ')':
	        	proc_exp_op();
	            break;
	        case '\t': //tab  RPI181
	        case '\r': //cr
	        case '\n': //lf
	        case ' ':  //space
	        	exp_op = exp_term_op;
	        	proc_exp_op();
	            break;
	        case ',':
	        case '\'': // terminator for DCF, DCH, expressions
	        	exp_op = exp_term_op;
	        	proc_exp_op();
	            break;
	        case '~':  // terminator
	        	proc_exp_op();
	            break;
	        case 'B':
	        	if (exp_token.length() > 2 && exp_token.charAt(1) == '\''){
	        	   proc_exp_sdt();
	        	} else {
	        	   proc_exp_sym();
	        	}
	            break;
	        case 'C':
	        	if (exp_token.length() > 1 
	        		&& (exp_token.charAt(1) == '\''
	        			|| exp_token.charAt(1) == '"'    //RPI5
	        		    || exp_token.charAt(1) == '!')){ //RPI73,RPI90
	        	   proc_exp_sdt();
	        	} else {
	        	   proc_exp_sym();
	        	}
	            break;
	        case 'L':
	        	if (exp_token.length() > 1 && exp_token.charAt(1) == '\''){
	        	   proc_exp_op();
	        	} else {
	        	   proc_exp_sym();
	        	}
	            break;
	        case 'U':
	        	if (exp_token.length() == 2
	        		&& (exp_token.charAt(1) == '-'
	        			|| exp_token.charAt(1) == '+')){
		        	   proc_exp_op();
		        	} else {
		        	   proc_exp_sym();
		        	}
		        	break;
	        case 'X':
	        	if (exp_token.length() > 1 && exp_token.charAt(1) == '\''){
	        	   proc_exp_sdt();
	        	} else {
	        	   proc_exp_sym();
	        	}
	            break;
	        case '=':
	        	log_error(121,"literal invalid in expression");
	        	break;
	        default:
		        if (exp_op.charAt(0) <= '9' && exp_op.charAt(0) >= '0'){
		        	proc_exp_sdt();
		        } else {
	        	    proc_exp_sym();
		        }
	            break;
	    }
	}
}
private void proc_loc_ctr(){
	/*
	 * push current location counter on stack
	 */
    exp_sym_last = true;
    check_prev_op = false;
	if (inc_tot_exp_stk_sym()){
	   if (cur_esd > 0){ 
          exp_stk_sym_esd[tot_exp_stk_sym-1]= cur_esd;
          if (dc_lit_ref || dc_lit_gen){
          	 lit_loc_ref = true;
          	 exp_stk_sym_val[tot_exp_stk_sym-1] = lit_line_loc[cur_lit] + dc_dup_loc;
          } else {
             exp_stk_sym_val[tot_exp_stk_sym-1] = loc_ctr;
          }
	   } else {
	   	  log_error(27,"location counter undefined");
	   }
    }
}
private void proc_exp_sym(){
	if (exp_token.length() > 1 && exp_token.charAt(exp_token.length()-1) == '.'){
		if (gen_obj_code){
		   exp_use_lab = exp_token.substring(0,exp_token.length()-1);
		}
	} else {
        exp_sym_last = true;
        push_exp_sym();
	}
    check_prev_op = false;
}
private void proc_exp_sdt(){
	exp_sym_last = true;
    push_exp_sdt(exp_op);
    check_prev_op = false;
}
private void proc_exp_op(){
	if  (tot_exp_stk_op > 0){
	    exp_prev_op = exp_stk_op[tot_exp_stk_op -1];
	} else {
		exp_prev_op = exp_start_op;
	}
	int last_op_class = exp_op_class[exp_prev_op.charAt(0)];
	if  (last_op_class == 0){
		log_error(11,"invalid operator class for - " + exp_op);
	} 
	int next_op_class = exp_op_class[exp_op.charAt(0)];
	if  (next_op_class == 0){
		log_error(12,"invalid operator class - " + exp_op);
	}
    int action = exp_action[tot_classes*(last_op_class-1)+next_op_class-1];
    switch (action){
    case 1: // add/sub
       if (exp_prev_op.equals("+")){
          exp_add();
       } else {
       	  exp_sub();
       }
	   break;
    case 2:  // mpy/div
       if (exp_prev_op.equals("*")){
          exp_mpy();
       } else {
       	  exp_div();
       }
  	   break;
    case 3: // (
  	   exp_push_op();
  	   if (exp_op.charAt(0) == '('){
  	      exp_level++;
  	   }
  	   check_prev_op = false;
  	   break;
    case 4: // )
  	   exp_pop_op();
  	   exp_level--;
  	   if (exp_level == 0 && bal_op.equals("AIF")){
  	   	  exp_op = exp_term_op;
  	   	  exp_term();
  	   }
  	   check_prev_op = false;
       break;
    case 5: //  PFX operators (L' or U+ or U-
    	 exp_pop_op();          //RPI9
    	 switch (exp_stk_op[tot_exp_stk_op].charAt(0)){
    	 case 'L': // length operator
    	 	 exp_sym_len_op();
    	 	 break;
    	 case 'U': // unary operator
    		 if (exp_sym_pushed){
    			 if (exp_stk_op[tot_exp_stk_op].charAt(1) == '-'){
    				 exp_stk_sym_val[tot_exp_stk_sym-1] = - exp_stk_sym_val[tot_exp_stk_sym -1];
    			 }
    		 } else if (exp_token.charAt(0) == 'U'){
    			 if (exp_token.charAt(1) == exp_stk_op[tot_exp_stk_op].charAt(1)){
    				 exp_token = "U+";
    			 } else {
    				 exp_token = "U-";
    			 }
    			 exp_op = exp_token;
    		 } else {
    			 log_error(123,"missing unary operand value");
    		 }
    		 break;
    	 default:
    		 log_error(124,"invalid prefix operator type");
    	 }
         break;
    case 6: // terminator space, comma, unmatched )
  	   exp_term();
  	   check_prev_op = false;
  	   break;
  	case 7: // check if ( is terminator after value
  	   if (exp_sym_last){
   	       exp_term();
  	   } else {
   	       exp_push_op();
           exp_level++;
  	   }
  	   check_prev_op = false;
  	   break;
    default:
    	log_error(13,"invalid operation sequence");
    }
}
private void exp_add(){
	/* add top of stack value to prev. value
	 * and pop the top stack value off
	 */
	get_stk_sym();
	if  (sym_esd1 != 0 || sym_esd2 != 0){
	    if  ((sym_esd1 > 0) && (sym_esd2 > 0)){
		    add_rld(sym_esd1);
		    add_rld(sym_esd2);
		    sym_type1 = sym_rld;
		    sym_esd1  = esd_rld;
	    } else if ((sym_esd1 == esd_rld) || (sym_esd2 == esd_rld)){
		    if (sym_esd1 > 0){
		       add_rld(sym_esd1);
		    } else if (sym_esd2 > 0){
			   add_rld(sym_esd2);
		    }
		    sym_type1 = sym_rld;
		    sym_esd1  = esd_rld;
	    } else {
	    	if (sym_esd2 > 0){
	    		sym_esd1 = sym_esd2;
	    	}
	    }
	}
	sym_val1 = sym_val1 + sym_val2;
	put_stk_sym();
}
private void exp_sub(){
	/* sub top of stack value from prev. value
	 * and pop the top stack value off
	 */
	get_stk_sym();
	if  (sym_esd1 > 0 || sym_esd2 > 0){
	    if  (sym_esd1 > 0 && sym_esd2 > 0){
		    if (sym_esd1 == sym_esd2){
		 	   sym_esd1 = 0;
		 	   sym_esd2 = 0;
		 	   sym_type1 = sym_sdt;
		 	   sym_type2 = sym_sdt;
		    } else {
		       add_rld(sym_esd1);
		       sub_rld(sym_esd2);
		       sym_type1 = sym_rld;
		       sym_esd1  = esd_rld;
		    }
	    } else if (sym_esd1 == esd_rld 
			|| sym_esd2 == esd_rld
			|| sym_esd2 > 0){
		    if (sym_esd1 > 0){
		       add_rld(sym_esd1);
		    } else if (sym_esd2 > 0){
			   sub_rld(sym_esd2);
		    }
		    sym_type1 = sym_rld;
		    sym_esd1  = esd_rld;
	    }
	}
	sym_val1 = sym_val1 - sym_val2;
	put_stk_sym();
}
private void exp_mpy(){
	/* mpy top of stack value to prev. value * and pop the top stack value off
	 */
	get_stk_sym();
	if (sym_esd1 != 0 || sym_esd2 != 0){
		log_error(58,"invalid rld multiplication - " + exp_text.substring(0,exp_index));
		return;
	}
	sym_val1 = sym_val1 * sym_val2;
	put_stk_sym();
}
private void exp_div(){
	/* div top of stack value into prev. value
	 * and pop the top stack value off
	 */
	get_stk_sym();
	if (sym_esd1 != 0 || sym_esd2 != 0){
		log_error(59,"invalid rld division - " + exp_text.substring(0,exp_index));
		return;
	}
	if (sym_val2 == 0){
		log_error(60,"invalid rld division - " + exp_text.substring(0,exp_index));
		return;
	}
	if (sym_val2 != 0){
	    sym_val1 = sym_val1 / sym_val2;
	} else {
		sym_val1 = 0;  // by definition for HLASM
	}
	put_stk_sym();
}
private void exp_sym_len_op(){
	/*
	 * replace symbol on stack with length value
	 */
	if (tot_exp_stk_sym >= 1){
		if (cur_sid >= 0){
		   exp_stk_sym_val[tot_exp_stk_sym - 1] = sym_len[cur_sid];
		   exp_stk_sym_esd[tot_exp_stk_sym - 1] = sym_sdt;
		} else {
		   log_error(25,"invalid symbol for length attribute operator");
		}
	} else {
		log_error(26,"missing symbol for length attribute");
	}
}
private void get_stk_sym(){
	/*
	 * set stk_value1 & 2 from top of stack
	 */
	if (tot_exp_stk_sym >= 2){
	    sym_esd1 = exp_stk_sym_esd[tot_exp_stk_sym - 2];
		sym_val1 = exp_stk_sym_val[tot_exp_stk_sym - 2];
	    if (sym_sid1 > 0){
	    	sym_type1 = sym_rel;
	    } else {
	    	sym_type1 = sym_sdt;
	    }
	    sym_esd2 = exp_stk_sym_esd[tot_exp_stk_sym - 1];
	    sym_val2 = exp_stk_sym_val[tot_exp_stk_sym - 1];
	    if (sym_esd2 > 0){
	    	sym_type2 = sym_rel;
	    } else {
	    	sym_type2 = sym_sdt;
	    }
	} else {
		log_error(17,"stack get error");
	}
}
private void put_stk_sym(){
	/*
	 * pop operator from op stack
	 * pop sym_val2 off var stack
	 * replace original sym_val1 
	 * on top of stack with result
	 */
	if ((tot_exp_stk_sym >= 2) && (tot_exp_stk_op > 0)){
		tot_exp_stk_op--;
		tot_exp_stk_sym--;
	    exp_stk_sym_esd[tot_exp_stk_sym - 1] = sym_esd1;
		exp_stk_sym_val[tot_exp_stk_sym - 1] = sym_val1;
	} else {
		log_error(18,"stack put error");
	}
	exp_sym_last = true;
}

private void exp_push_op(){
	/*
	 * put current op on stack
	 * 
	 * if unary minus push 0 var first
	 * if unary plus skip the push
	 */
   	if (tot_exp_stk_op > max_exp_stk){
   		abort_error(20,"stack operation size exceeded");
   	}
   	exp_stk_op[tot_exp_stk_op] = exp_token;
   	tot_exp_stk_op++;
   	exp_sym_pushed = false;
   	exp_sym_last = false;
}
private void exp_pop_op(){
	/*
	 * pop current op on stack
	 */
      tot_exp_stk_op--;
      if (tot_exp_stk_op < 0){
      	 log_error(21,"stack pop operation error");
      }
}
private void exp_term(){
	/*
	 * terminate expression returning
	 * value on stack if no errors
	 */
	if (tot_exp_stk_sym == 1 && tot_exp_stk_op == 0){
		exp_term = true;
    	exp_val = exp_stk_sym_val[0];
        exp_esd = exp_stk_sym_esd[0];
        if (exp_esd == esd_rld){
        	reduce_exp_rld();
        }
        if (exp_esd == esd_sdt){
           	exp_type = sym_sdt;
        } else if (exp_esd == esd_rld){
        	if (exp_rld_len > 0){
        		if (gen_obj_code){
                    gen_exp_rld();
        		}    
            } else {
            	log_error(61,"invalid complex rld expression" + exp_text.substring(0,exp_index));
            }
        } else {  
        	if (exp_rld_len > 0 && gen_obj_code){
        		exp_rld_add_esd[0] = exp_esd;
        		tot_exp_rld_add = 1;
        		gen_exp_rld();
        	}
            exp_type = sym_rel;
        }
	} else {
		log_error(35,"invalid expression result");
	}
}
private void push_exp_sym(){
	/*
	 * push symbol on stack else abort
	 */
	if (inc_tot_exp_stk_sym()){
	   cur_sid = find_sym(exp_token);
	   if (cur_sid > 0){ 
	   	  if (exp_first_len){
	   	  	 exp_first_len = false;
	   	  	 exp_len = sym_len[cur_sid];
	   	  }
          exp_stk_sym_esd[tot_exp_stk_sym-1]  = sym_esd[cur_sid];
          exp_stk_sym_val[tot_exp_stk_sym-1]  = sym_loc[cur_sid];
	   } else {
		  if (dca_ignore_refs){
	          exp_stk_sym_esd[tot_exp_stk_sym-1]  = sym_sdt;
	          exp_stk_sym_val[tot_exp_stk_sym-1]  = 0;
		  } else {
			  log_error(98,"symbol not found - " + exp_token);
		  }
	   }
    }
}
private boolean inc_tot_exp_stk_sym(){
	/*
	 * check if room to add to exp_stack
	 * and return true else abort
	 */
	if (tot_exp_stk_sym < max_exp_stk){
		tot_exp_stk_sym++;
		exp_sym_pushed = true;
		return true;
	} else {
		abort_error(22,"maximum stack variables exceeded");
	    return false;
	}
}
private void push_exp_sdt(String sdt){
	/*
	 * push self defining abs term on stack
	 */
    	if (inc_tot_exp_stk_sym()){
           exp_stk_sym_esd[tot_exp_stk_sym-1] = sym_sdt;
           switch (sdt.charAt(0)){
           case 'B': // B'11000001' binary
        	   exp_stk_sym_val[tot_exp_stk_sym-1] = Integer.valueOf(sdt.substring(2,sdt.length()-1),2).intValue();
           	   break;
           case 'C': //RPI192

           	   if (!tz390.get_sdt_char_int(sdt)){
           		   log_error(138,"invalid character sdt " + sdt);
           	   }
        	   exp_stk_sym_val[tot_exp_stk_sym-1] = tz390.sdt_char_int; 
           	   break;
           case 'X': // X'C1' hex
           	   exp_stk_sym_val[tot_exp_stk_sym-1] = Long.valueOf(sdt.substring(2,sdt.length()-1),16).intValue();
           	   break;
           default:
               exp_stk_sym_val[tot_exp_stk_sym-1] = Integer.valueOf(sdt).intValue();
               break;
           }
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
	boolean single_quote = false; // RPI115
	while (parm_match.find()){
		switch (parm_match.group().charAt(0)){
			case ',':
				index = parm_match.start();
				if (!single_quote 
						&& index > 0 
						&& line.length() > index+1
						&& line.charAt(index+1) <= ' '){  //RPI181
					return line.substring(0,index+1);
				}
				break;
			case '\'': // single quote found 
				index = parm_match.start();
				if (index > 0){
	                single_quote = true;
				}
		}
	}
	return line;
}
private void exit_az390(){
	/*
	 * display total errors
	 * close files and exit
	 */
	  if    (az390_rc == 0 && az390_errors > 0){
    	az390_rc = 16;
      }
  	  put_stats();
      close_files();
	  if    (tz390.z390_abort){
	    	System.exit(az390_rc);
	  }
}
private void put_stats(){
	/*
	 * display statistics as comments at end of bal
	 */
	if (tz390.opt_stats || az390_errors > 0){
	   put_log("Stats BAL lines       = " + tot_bal_line);
	   put_log("Stats symbols         = " + tot_sym);
	   put_log("Stats Literals        = " + tot_lit);
	   put_log("Stats alloc passes    = " + (cur_pass-1));
	   put_log("Stats Keys            = " + tz390.tot_key);
	   put_log("Stats Key searches    = " + tz390.tot_key_search);
	   if (tz390.tot_key_search > 0){
	       tz390.avg_key_comp = tz390.tot_key_comp/tz390.tot_key_search;
	   }
	   put_log("Stats Key avg comps   = " + tz390.avg_key_comp);
	   put_log("Stats Key max comps   = " + tz390.max_key_comp);
	   put_log("Stats ESD symbols     = " + tot_esd);
	   put_log("Stats object bytes    = " + tot_obj_bytes);
	   put_log("Stats object rlds     = " + tot_rld);
	   if (tz390.opt_timing){
	      cur_date = new Date();
	      tod_end = cur_date.getTime();
	      tot_sec = (tod_end - tod_start)/1000;
	      put_log("Stats total seconds         = " + tot_sec);
	   }
	}
	put_log("AZ390I total errors          = " + az390_errors);
	put_log("AZ390I return code           = " + az390_rc);
}
private void close_files(){
	  if (obj_file != null){
	  	  try {
	  	  	  obj_file.close();
	  	  } catch (IOException e){
	  	  	  abort_error(24,"I/O error on obj close - " + e.toString());
	  	  }
	  }
	  if  (tz390.opt_list){
		  if (prn_file != null && prn_file.isFile()){
		  	  try {
		  	  	  prn_file_buff.close();
		  	  } catch (IOException e){
		  	  	  abort_error(24,"I/O error on prn close - " + e.toString());
		  	  }
		  }
	  }
}
private void log_error(int error,String msg){
	/*
	 * issue error msg to log with prefix and
	 * inc error total
	 * Notes:
	 *   1.  Set bal_abort if not set else exit
	 *   2.  supress if not gen_obj and not trace
	 *   3.  print bal line first if list on
	 */
	  if (bal_abort)return;
	  bal_abort = true;
	  if (gen_obj_code || tz390.opt_tracea){
		 list_bal_line = true;
    	 list_bal_line();
	     put_log("AZ390E error " + error + " line " + bal_line_num[bal_line_index] + "   " + bal_name_line[bal_line_index]);
	     put_log("AZ390I " + msg);
	  }
	  az390_errors++;
	  if (gen_obj_code && max_errors != 0 && az390_errors > max_errors){
	  	 abort_error(49,"max errors exceeded");	 
	  }
}
private void abort_error(int error,String msg){
	/*
	 * issue error msg to log with prefix and
	 * inc error total
	 */
	  az390_errors++;
	  if (tz390.z390_abort){
		 System.out.println("az390 aborting due to recursive abort request for " + msg);
	  	 System.exit(16);
	  }
	  tz390.z390_abort = true;
	  list_bal_line();
	  put_log("AZ390E error " + error + " on line " + bal_line_num[bal_line_index] + " " + bal_name_line[bal_line_index]);
	  put_log("AZ390I " + msg);
      exit_az390();
}
private void put_copyright(){
	   /*
	    * display az390 version, timestamp,
	    * and copyright if running standalone
	    */
	   	if  (tz390.opt_timing){
			cur_date = new Date();
	   	    put_log("AZ390I " + tz390.version 
	   			+ " Current Date " +mmddyy.format(cur_date)
	   			+ " Time " + hhmmss.format(cur_date));
	   	} else {
	   	    put_log("AZ390I " + tz390.version);
	   	}
	   	if  (z390_log_text == null){
	   	    put_log("Copyright 2005 Automated Software Tools Corporation");
	   	    put_log("z390 is licensed under GNU General Public License");
	   	}
	   	put_log("AZ390I program = " + tz390.dir_bal + tz390.pgm_name + tz390.pgm_type);
	   	put_log("AZ390I options = " + tz390.cmd_parms);
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
   	        } else {
   	        	if (tz390.opt_con || tz390.z390_abort){
   	    	        System.out.println(msg);
   	        	}
   	        }
	   }
	   private void put_prn_line(String msg){
	   /*
	    * put line to prt listing file
	    * if print_on and not surpressed by nogen.
	    * if print data, print all data.
	    */
		   String temp_hex;
	   	   if (tz390.opt_list || tz390.opt_tracea){
	   		   if (!print_on[print_level]        //PRINT OFF
	   		       || (!print_gen[print_level]  //PRINT NOGEN 
	   		           && mac_call_level > 0)){ 
	   			   if (!tz390.opt_tracea){
	   				   return; // supress prn RPI182
	   			   }
	   		   }
	   	      try {
	   	          prn_file_buff.write(msg + "\r\n");
	   	          if (prn_file.length() > tz390.max_file){
	   	        	  abort_error(118,"maximum prn file size exceeded");
	   	          }
	   	          int index = 16;
	   	          while (print_data[print_level]
	   	                 && index < list_obj_code.length()){
	   	        	  list_obj_loc = list_obj_loc + 8;
	   	        	  if (index + 16 > list_obj_code.length()){
	   	        		  temp_hex = list_obj_code.substring(index);
	   	        	  } else {
	   	        		  temp_hex = list_obj_code.substring(index,index+16);
	   	        	  }
	   	        	  String data_line = tz390.get_hex(list_obj_loc,6) + " " + temp_hex;
	   	        	  prn_file_buff.write(data_line + "\r\n");
		   	          if (prn_file.length() > tz390.max_file){
		   	        	  abort_error(118,"maximum prn file size exceeded");
		   	          }
	   	        	  index = index + 16;
	   	          }
	   	      } catch (Exception e){
	              az390_errors++;
	   	      }
	   	   }
	   }
	   private void check_end_parms(){
		   /*
		    * check for extra parms beyond end
		    * of last instruction parm and issue error
		    */
		   if (!exp_eot 
			   && exp_index < exp_text.length() 
			   && exp_text.charAt(exp_index) > ' '){  //RPI181
			   log_error(122,"extra parameter found - " + exp_text.substring(exp_index));
		   }
	   }
	   private void put_obj_line(String msg){
		   /*
		    * put object code to obj file in
		    * hex or binary format
		    */
		   	try {
		   		if (tz390.opt_objhex){
		   			obj_file.writeBytes(msg + "\r\n");
		   		} else {
		   			cvt_obj_hex_to_bin(msg);
		   			obj_file.write(bin_byte);
		   		}
		   		if (obj_file.length() > tz390.max_file){
	   	       	  abort_error(119,"maximum obj file size exceeded");
		   		}
		   	} catch (Exception e){
		   	    abort_error(28,"I/O error on OBJ file write - " + e.toString());
		   	}
		   }
	   private void cvt_obj_hex_to_bin(String hex_rcd){
		   /*
		    * convert ascii hex object string to
		    * binary 80 byte EBCDIC format for 
		    * mainframe compatiblity.  See DFSMS
		    * Program Management Manual reference.
		    */
		try {
		   int index = 0;
		   int index1 = 0;
		   bin_byte[0] = 0x02;        // 1    binary OBJ ID code
		   String type = hex_rcd.substring(1,4);
		   if (type.equals("ESD")){   // 2-4 .ESD
			    bin_byte[1] = tz390.ascii_to_ebcdic['E'];
				bin_byte[2] = tz390.ascii_to_ebcdic['S'];
				bin_byte[3] = tz390.ascii_to_ebcdic['D'];
				Arrays.fill(bin_byte,4,14,ebcdic_space);
  		                             // 15-16 ESD ID
				bin_byte[14] = (byte)Integer.valueOf(hex_rcd.substring(9,11),16).intValue();
				bin_byte[15] = (byte)Integer.valueOf(hex_rcd.substring(11,13),16).intValue();
				index = 16;          // 17-72 up to 3 ESD entries
				index1 = 54;
				while (index < 24){  // name at entry 1 - 8
					if (index1 < hex_rcd.length()){
						bin_byte[index] = tz390.ascii_to_ebcdic[hex_rcd.charAt(index1)];
						index1++;
					} else {
						bin_byte[index] = ebcdic_space;
					}
					index++;
				}
				if (hex_rcd.substring(45,48).equals("CST")){ 
					bin_byte[10] = 0;    // 11-12 SD entry bytes
					bin_byte[11] = 16;
					bin_byte[24] = 0x00; // SD type at entry 9
					                     // 24 bit address at entry 10-12
					if (!hex_rcd.substring(18,20).equals("00")){
						abort_error(132,"SD invalid 24 bit address - " + hex_rcd);
					}
					bin_byte[25] = (byte)Integer.valueOf(hex_rcd.substring(20,22),16).intValue();
					bin_byte[26] = (byte)Integer.valueOf(hex_rcd.substring(22,24),16).intValue();
					bin_byte[27] = (byte)Integer.valueOf(hex_rcd.substring(24,26),16).intValue();
					bin_byte[28] = 0x07;  // double word align at entry 13
					if (!hex_rcd.substring(31,33).equals("00")){
						abort_error(133,"SD invalid 24 bit length - " + hex_rcd);
					}
					bin_byte[29] = (byte)Integer.valueOf(hex_rcd.substring(33,35),16).intValue();
					bin_byte[30] = (byte)Integer.valueOf(hex_rcd.substring(35,37),16).intValue();
					bin_byte[31] = (byte)Integer.valueOf(hex_rcd.substring(37,39),16).intValue();
					Arrays.fill(bin_byte,32,80,ebcdic_space);
				} else if (hex_rcd.substring(45,48).equals("EXT")){ 
					bin_byte[10] = 0;    // 11-12 ER entry bytes
					bin_byte[11] = 13;
					bin_byte[24] = 0x02; // ER type at entry 9
					Arrays.fill(bin_byte,25,28,ebcdic_space); // blank address at entry 10
					bin_byte[28] = 0x00; // byte alignment at entry 13
					Arrays.fill(bin_byte,29,80,ebcdic_space);
				} else if (hex_rcd.substring(45,48).equals("WXT")){ 
					bin_byte[10] = 0;    // 11-12 WX entry bytes
					bin_byte[11] = 13;
					bin_byte[24] = 0x0A; // WX type at entry 9
					Arrays.fill(bin_byte,25,28,ebcdic_space); // blank address at entry 10
					bin_byte[28] = 0x00; // byte alignment at entry 13
					Arrays.fill(bin_byte,29,80,ebcdic_space);
				} else if (hex_rcd.substring(45,48).equals("ENT")){ 
					bin_byte[10] = 0;    // 11-12 :D entry bytes
					bin_byte[11] = 16;
					bin_byte[24] = 0x01; // LD type at entry 9
					Arrays.fill(bin_byte,25,28,ebcdic_space); // blank ESD type 12-14 for LD
					if (!hex_rcd.substring(18,20).equals("00")){
						abort_error(134,"LD invalid 24 bit address - " + hex_rcd);
					}
					bin_byte[25] = (byte)Integer.valueOf(hex_rcd.substring(20,22),16).intValue();
					bin_byte[26] = (byte)Integer.valueOf(hex_rcd.substring(22,24),16).intValue();
					bin_byte[27] = (byte)Integer.valueOf(hex_rcd.substring(24,26),16).intValue();
					bin_byte[28] = 0x00; // byte align at entry 13}
					bin_byte[29] = 0x00; // SD identifier for entry
					bin_byte[30] = (byte)Integer.valueOf(hex_rcd.substring(9,11),16).intValue();
					bin_byte[31] = (byte)Integer.valueOf(hex_rcd.substring(11,13),16).intValue();
					Arrays.fill(bin_byte,32,80,ebcdic_space);
				} else {
					abort_error(131,"invalid ESD type " + hex_rcd);
				}
		   } else if (type.equals("TXT")){
				bin_byte[1] = tz390.ascii_to_ebcdic['T'];
				bin_byte[2] = tz390.ascii_to_ebcdic['X'];
				bin_byte[3] = tz390.ascii_to_ebcdic['T'];
				bin_byte[4] = ebcdic_space;
				                        // 6-8 address at 
				if (!hex_rcd.substring(18,20).equals("00")){
					abort_error(134,"TXT invalid 24 bit address - " + hex_rcd);
				}
				bin_byte[5] = (byte)Integer.valueOf(hex_rcd.substring(20,22),16).intValue();
				bin_byte[6] = (byte)Integer.valueOf(hex_rcd.substring(22,24),16).intValue();
				bin_byte[7] = (byte)Integer.valueOf(hex_rcd.substring(24,26),16).intValue();
				bin_byte[8] = ebcdic_space;
				bin_byte[9] = ebcdic_space;
				bin_byte[10] = 00;       // 11-12 number of text bytes
				bin_byte[11] = (byte)Integer.valueOf(hex_rcd.substring(31,33),16).intValue();
				bin_byte[12] = ebcdic_space;
				bin_byte[13] = ebcdic_space;
				                         // 15-16 SD type
				bin_byte[14] = (byte)Integer.valueOf(hex_rcd.substring(9,11),16).intValue();
				bin_byte[15] = (byte)Integer.valueOf(hex_rcd.substring(11,13),16).intValue();
				index = 16;
				index1 = 34;
				int count = bin_byte[11];
				while (count > 0){
					bin_byte[index] = (byte)Integer.valueOf(hex_rcd.substring(index1,index1+2),16).intValue();
				    index++;
				    index1 = index1+2;
				    count--;
				}
				Arrays.fill(bin_byte,index,80,ebcdic_space);
		   } else if (type.equals("RLD")){
				bin_byte[1] = tz390.ascii_to_ebcdic['R'];
				bin_byte[2] = tz390.ascii_to_ebcdic['L'];
				bin_byte[3] = tz390.ascii_to_ebcdic['D'];
				Arrays.fill(bin_byte,4,10,ebcdic_space);
				bin_byte[10] = 0;        // 11-12 number of bytes for RLD entries
				bin_byte[11] = 8;
				Arrays.fill(bin_byte,12,16,ebcdic_space);
                                         // 17-18 ESD ID of referenced ESD
				bin_byte[16] = (byte)Integer.valueOf(hex_rcd.substring(45,47),16).intValue();
				bin_byte[17] = (byte)Integer.valueOf(hex_rcd.substring(47,49),16).intValue();
                                         // 19-20 ESD ID of SD containing RLD field
				bin_byte[18] = (byte)Integer.valueOf(hex_rcd.substring(9,11),16).intValue();
				bin_byte[19] = (byte)Integer.valueOf(hex_rcd.substring(11,13),16).intValue();
                                         // 20 flags TTTTLLSN
				int  rld_len =  Integer.valueOf(hex_rcd.substring(31,32),16).intValue()
				             - 1; // rld field len -1
				char rld_sign = hex_rcd.charAt(38);
				if (rld_sign == '+'){
					bin_byte[20] = (byte)(rld_len << 2); // pos rld
				} else {
					bin_byte[20] = (byte)(rld_len << 2 + 2); // neg rld
				}
				                         // 22-24 address at 
				if (!hex_rcd.substring(18,20).equals("00")){
					abort_error(135,"RLD invalid 24 bit address - " + hex_rcd);
				}
				bin_byte[21] = (byte)Integer.valueOf(hex_rcd.substring(20,22),16).intValue();
				bin_byte[22] = (byte)Integer.valueOf(hex_rcd.substring(22,24),16).intValue();
				bin_byte[23] = (byte)Integer.valueOf(hex_rcd.substring(24,26),16).intValue();
				Arrays.fill(bin_byte,24,80,ebcdic_space);
		   } else if (type.equals("END")){
				bin_byte[1] = tz390.ascii_to_ebcdic['E'];
				bin_byte[2] = tz390.ascii_to_ebcdic['N'];
				bin_byte[3] = tz390.ascii_to_ebcdic['D'];
				Arrays.fill(bin_byte,4,80,ebcdic_space);
		   } else {
			   abort_error(130,"invalid object record - " + hex_rcd);
		   }
		} catch (Exception e){
			if (az390_errors == 0){ // ignore if prior errors
				log_error(136,"Invalid ascii hex object code - " + hex_rcd);
			}
		}
	   }
	   private String get_long_hex(long work_long) {
	   	/*
	   	 * Format long into 16 byte hex string
	   	 */
	   	    String work_hex = Long.toHexString(work_long);
			return ("0000000000000000" + work_hex).substring(work_hex.length()).toUpperCase();
	   }
	   private String string_to_hex(String text,boolean ascii_req){
	   	/*
	   	 * Format text string into hex string
	   	 * If ascii_req true, gen ascii else ebcdic hex
	   	 */
		    int work_int = 0;
            StringBuffer hex = new StringBuffer(2 * text.length());
            int index = 0;
            while (index < text.length()){
            	if (ascii_req){
            		work_int = text.charAt(index) & 0xff;
            	} else {
            		work_int = tz390.ascii_to_ebcdic[text.charAt(index)] & 0xff;
            	}
				String temp_string = Integer.toHexString(work_int);
                if  (temp_string.length() == 1){
                	hex.append("0" + temp_string);
                } else {
                	hex.append(temp_string);
                }
				index++;
            }
            return hex.toString().toUpperCase();            
	   } 
private String bytes_to_hex(byte[] bytes,int byte_start,int byte_length,int chunk){
	   	/*
	   	 * Format bytes into hex string
	   	 * If chuck > 0 insert space after each chuck
	   	 */
	        StringBuffer hex = new StringBuffer(72);
	        int index1 = 0;
	        int hex_bytes = 0;
	        while (index1 < byte_length){
	        	int work_int = bytes[byte_start + index1] & 0xff;
				String temp_string = Integer.toHexString(work_int);
	            if  (temp_string.length() == 1){
	            	hex.append("0" + temp_string);
	            } else {
	            	hex.append(temp_string);
	            }
	            if (chunk > 0){
	               hex_bytes++;
	               if (hex_bytes >= chunk){
	            	  hex.append(" ");
	            	  hex_bytes = 0;
	               }
	            }
			    index1++;
	        }
	        return hex.toString().toUpperCase();   
}
private void put_obj_text(){
	/*
	 * 1.  Append obj_code to list_obj_code for 
	 *     print line (reguired by mult DC calls).
	 * 2.  Exit if gen_obj_code not on.
	 * 3.  Buffer output of ojbect text code for
	 *     contiguous data in same ESD.
	 * 4.  Called from END processing with BAL_EOF
	 *     to flush butter.
	 * 5.  Reset obj_code for use by DC routines
	 */
     check_private_csect();
	 if (!gen_obj_code){
	 	return;
	 }
	 String temp_obj_line;
     list_obj_code = list_obj_code.concat(obj_code);
	 int obj_code_len = obj_code.length()/2;
	 tot_obj_bytes = tot_obj_bytes + obj_code_len;
	 if (cur_text_len > 0
	 	&& (bal_eof 
	 		|| cur_text_esd != sym_esd[esd_sid[cur_esd]] 
		 	|| cur_text_loc != loc_ctr)){
		cur_text_loc = cur_text_loc - cur_text_len;
		temp_obj_line = ".TXT ESD=" + tz390.get_hex(cur_text_esd,4) + " LOC=" + tz390.get_hex(cur_text_loc - sym_loc[esd_sid[cur_text_esd]],8) + " LEN=" + tz390.get_hex(cur_text_len,2) + " " + cur_text_buff;
		put_obj_line(temp_obj_line);
		cur_text_len = 0; 
	}
	if (bal_eof)return;
	if (cur_text_len == 0){
		cur_text_esd = sym_esd[esd_sid[cur_esd]];
		cur_text_loc = loc_ctr;
		cur_text_buff = "";
	} 
	cur_text_buff = cur_text_buff.concat(obj_code);
	cur_text_len = cur_text_len + obj_code_len;
	cur_text_loc = cur_text_loc + obj_code_len;
	while (cur_text_len >= max_text_buff_len){	 	 
        cur_text_loc = cur_text_loc - cur_text_len;
	 	temp_obj_line = ".TXT ESD=" + tz390.get_hex(cur_text_esd,4) 
	 			   + " LOC=" + tz390.get_hex(cur_text_loc - sym_loc[esd_sid[cur_text_esd]],8) 
	 			   + " LEN=" + tz390.get_hex(max_text_buff_len,2) 
	 			   + " " + cur_text_buff.substring(0,2*max_text_buff_len);
	 	put_obj_line(temp_obj_line);
		cur_text_loc = cur_text_loc + cur_text_len;
	 	cur_text_buff = cur_text_buff.substring(2*max_text_buff_len);
	 	cur_text_len = cur_text_buff.length()/2;
	}
	obj_code = "";
}
private void check_private_csect(){
	/*
	 * start private csect if no csect or dsect
	 */
    if (cur_esd == 0){
     	process_sect(sym_cst,"");
    	first_cst_esd = cur_esd;
     }
}
private void add_using(){
	/*
	 * add or replace USING for code generation
	 */
	get_use_range();
	if (bal_label != null){
		cur_use_lab = bal_label.toUpperCase();
        drop_cur_use_label();
	} else {
		cur_use_lab = "";
	}
	use_eof = false;
    get_use_domain();
    while (!use_eof){
      	if (cur_use_lab.length() == 0){
      		if (cur_use_depend){
      	        drop_cur_use_range();
      		} else {
      		    drop_cur_use_reg();
      		}
      	}
      	add_use_entry();
       	get_use_domain();
       	cur_use_base_loc = cur_use_base_loc + 4096;
    }
}
private void get_use_range(){
	/*
	 * set cur_use_base esd,loc, and len
	 */
	cur_use_base_esd = 0;
	cur_use_base_loc = 0;
	cur_use_base_len = 0;
	int next_comma = 0;
	cur_use_parms = bal_parms.toUpperCase();
	if (cur_use_parms.length() > 0){
		if (cur_use_parms.charAt(0) == '('){
			next_comma = cur_use_parms.indexOf(",");
			if (next_comma != -1){
				exp_text = cur_use_parms.substring(1,next_comma);
				exp_index = 0;
				if (calc_rel_exp()){
					cur_use_base_esd = exp_esd;
					cur_use_base_loc = exp_val;
				}
				cur_use_parms = cur_use_parms.substring(next_comma +1);
				next_comma = cur_use_parms.indexOf(",");
				if (next_comma != -1){
				    exp_text = cur_use_parms.substring(0,next_comma);
				    exp_index = 0;
				    cur_use_parms = cur_use_parms.substring(next_comma+1);
				} else {
					log_error(104,"missing domain for using");
					return;
				}
				if (calc_rel_exp() && cur_use_base_esd == exp_esd){
					cur_use_base_len = exp_val - cur_use_base_loc;
				}				
			} else {
				log_error(103,"missing end of range value");
				return;
			}
		} else {
			next_comma = cur_use_parms.indexOf(",");
			if (next_comma != -1){
				exp_text = cur_use_parms.substring(0,next_comma);
				cur_use_parms = cur_use_parms.substring(next_comma+1);
			} else {
				log_error(104,"missing domain for using");
				return;
			}
			exp_index = 0;
			if (calc_rel_exp()){
				cur_use_base_esd = exp_esd;
				cur_use_base_loc = exp_val;
			}
			cur_use_base_len = 4096;
		}
	}
}
private void get_use_domain(){
	/*
	 * set cur_use_reg and cur_use_reg_loc
	 * from cur_use_parms set by get_range
	 * Notes:
	 *   1.  get_rel_exp_bddd is called for dependant
	 *       using expressions to find reg and loc
	 */
	cur_use_depend = false;
	cur_use_reg = 0;
	cur_use_reg_loc = 0;
	if (cur_use_parms.length() > 0){
		int next_comma = cur_use_parms.indexOf(",");
		if (next_comma != -1){
			exp_text = cur_use_parms.substring(0,next_comma);
            cur_use_parms = cur_use_parms.substring(next_comma+1);
		} else {
			exp_text = cur_use_parms;
			cur_use_parms = "";
		}
		exp_index = 0;
		if (calc_exp()){
			if (exp_type == sym_sdt){
				cur_use_reg = exp_val;
			} else if (exp_type == sym_rel
					   || exp_type == sym_cst){
				cur_use_depend =true;
				hex_bddd = get_exp_bddd();
			}
		}
	} else {
		use_eof =true;
	}
}
private void drop_using(){
	/*
	 * drop one or more using registers or labeled using
	 */
	String cur_drop_parms = bal_parms;
	if  (cur_drop_parms == null || cur_drop_parms.length() == 0
			|| cur_drop_parms.charAt(0) == ','){
		if (tz390.opt_tracea){
			int index = cur_use_start;
			while (index < cur_use_end){
				trace_use("DROP",index);
				index++;
			}
		}
		cur_use_end = cur_use_start;  // drop all using
		return;
	}
	while (cur_drop_parms != null && cur_drop_parms.length() > 0){
		int next_comma = cur_drop_parms.indexOf(",");
    	if (next_comma != -1){
			cur_use_lab = cur_drop_parms.substring(0,next_comma);
			cur_drop_parms = cur_drop_parms.substring(next_comma+1);
		} else {
			cur_use_lab = cur_drop_parms;
			cur_drop_parms = "";
		}
		if (tz390.find_key_index("U:" + cur_use_lab) != -1){
			drop_cur_use_label();
		} else {
			exp_text = cur_use_lab;
			exp_index = 0;
			if (calc_abs_exp()){ 
			   cur_use_reg = exp_val;
			   drop_cur_use_reg();
			} else {
				log_error(101,"invalid register expression - " + exp_text);
			}
		}
	}
}
private void drop_cur_use_label(){
	/*
	 * remove labeled using if found
	 */
	int index = cur_use_start;
	while (index < cur_use_end){
		if (use_lab[index] != null && use_lab[index].equals(cur_use_lab)){
			if (tz390.opt_tracea){
				trace_use("DROP",index);
			}
			cur_use_end--;
			if (index < cur_use_end){
				move_use_entry(cur_use_end,index);
			}
		}
		index++;
	}
	
}
private void drop_cur_use_range(){
	/*
	 * remove unlabeld using range if found
	 */
	int index = cur_use_start;
	while (index < cur_use_end){
		if (use_lab[index].length() == 0
				&& use_base_esd[index] == cur_use_base_esd
				&& use_base_loc[index] == cur_use_base_loc){
			if (tz390.opt_tracea){
				trace_use("DROP",index);
			}
			cur_use_end--;
			if (index < cur_use_end){
				move_use_entry(cur_use_end,index);
			}
		}
		index++;
	}
	
}
private void drop_cur_use_reg(){
	/*
	 * remove using reg entries if found
	 */
	int index = cur_use_start;
	while (index < cur_use_end){
		if (use_reg[index] == cur_use_reg){
			if (tz390.opt_tracea){
				trace_use("DROP",index);
			}
			cur_use_end--;
			if (index < cur_use_end){
				move_use_entry(cur_use_end,index);
			}
		}
		index++;
	}
}
private void move_use_entry(int index1,int index2){
	/*
	 * move use entry for delete, push, pop
	 */
	use_lab[index2] = use_lab[index1];
	use_base_esd[index2] = use_base_esd[index1];
	use_base_loc[index2] = use_base_loc[index1];
	use_base_len[index2] = use_base_len[index1];
	use_reg[index2] = use_reg[index1];
	use_reg_loc[index2] = use_reg_loc[index1];
}
private void add_use_entry(){
	/*
	 * add use entry
	 */
	if (cur_use_end < max_use){
		cur_use = cur_use_end;
		cur_use_end++;
		use_lab[cur_use] = cur_use_lab;
		if (cur_use_lab.length() > 0 
			&& tz390.find_key_index("U:" + cur_use_lab) == -1){
			// create key to indicate using label
			if (!tz390.add_key_index(0)){
			    abort_error(87,"key search table exceeded");
			}
		}
		use_base_esd[cur_use] = cur_use_base_esd;
		use_base_loc[cur_use] = cur_use_base_loc;
		use_base_len[cur_use] = cur_use_base_len;
		use_reg[cur_use] = cur_use_reg;
		use_reg_loc[cur_use] = cur_use_reg_loc;
		if (tz390.opt_tracea){
			trace_use("ADD ",cur_use);
		}
	} else {
		log_error(100,"maximum active using table exceeded");
	}
}
private void trace_use(String use_op,int index){
	/*
	 * display trace info for using entry
	 */
	put_log("TRACE USING " 
			+ use_op
			+ " ESD=" + tz390.get_hex(use_base_esd[index],4)
			+ " LOC=" + tz390.get_hex(use_base_loc[index],8)
			+ " LEN=" + tz390.get_hex(use_base_len[index],4)	
			+ " REG=" + tz390.get_hex(use_reg[index],2)
			+ " LOC=" + tz390.get_hex(use_reg_loc[index],4)
			+ " LAB=" + use_lab[index]
			);
}
private void get_hex_op(int op_offset, int op_len){
	/*
	 * initialize object code with op code
	 * and initialize exp parser with parms
	 * if op_offset = 1
	 */
	hex_op = tz390.op_code[bal_op_index].substring(op_offset-1,op_offset - 1 + op_len);
	obj_code = obj_code + hex_op;
	if (op_offset == 1){
	   exp_text = bal_parms;
	   exp_index = 0;
	}
}
private String get_hex_nib(){
	/*
	 * return single hex nibble char 0-f
	 */
	if (calc_abs_exp()){
		if (exp_val >= 0 && exp_val <= 15){
		    return tz390.get_hex(exp_val,1);
		} else {
       		log_error(81,"invalid field value 0-15 " + exp_val);
		}
	} else {
		log_error(82,"invalid field");
	}
	return "h";
}
private void get_hex_reg(){
	/*
	 * append hex reg from next parm
	 */
	if (calc_abs_exp()){
		if (exp_val >= 0 && exp_val <= 15){
		    obj_code = obj_code + tz390.get_hex(exp_val,1);
		} else {
			log_error(55,"invalid register expression - " + exp_val);
			obj_code = obj_code + "r";
		}
	} else {
		log_error(41,"invalid register value");
		obj_code = obj_code + "r";
	}
}
private void get_hex_zero(int hex_len){
	/*
	 * append zero nibbles
	 */
	String zeros = "00000000";
	obj_code = obj_code.concat(zeros.substring(0,hex_len));
}
private void skip_comma(){
	/*
	 * verify and skip comma
	 */
	 if (exp_index < exp_text.length() && exp_text.charAt(exp_index) == ','){
	 	exp_index++;
	 } else {
	 	log_error(50,"missing operand comma - " + exp_text.substring(0,exp_index));
	 }
}
private void get_hex_byte(){
	/*
	 * append hex byte from next parm
	 */
	if (calc_abs_exp() && exp_val >= 0 && exp_val <= 255){
		obj_code = obj_code + tz390.get_hex(exp_val,2);
	} else {
		log_error(42,"invalid byte value");
		obj_code = obj_code + "hh";
	}
}
private void get_hex_len_bddd(){
	/*
	 * append llbddd hex object code from next parm
	 */
	hex_len          = "ll";
	String hex_b     = "0";
	String hex_ddd   = "ddd";
	hex_bddd         = "bddd";
	hex_bddd_loc     = "      ";
	if (exp_text.length() > exp_index && exp_text.charAt(exp_index) == '='){
		hex_bddd = get_lit_bddd();
		hex_bddd_loc = tz390.get_hex(exp_val,6);
		exp_val = dc_first_len;
		if (exp_val >= 0 && exp_val <= 0x100){
			if (exp_val > 0)exp_val = exp_val - 1;
			hex_len = tz390.get_hex(exp_val,2);
		} else {
			log_error(64,"invalid length - " + exp_val);
		}
	} else if (calc_exp()){
		if  (exp_type == sym_rel){
			hex_bddd = get_exp_bddd();
			hex_bddd_loc = tz390.get_hex(exp_val,6);
			if  (exp_index < exp_text.length() && exp_text.charAt(exp_index) == '('){
				exp_index++;
				if (calc_abs_exp() && exp_val >= 0 && exp_val <= 0x100){
					if (exp_val > 0)exp_val = exp_val - 1;
					hex_len = tz390.get_hex(exp_val,2);
					exp_index++;
				} else {
					log_error(64,"invalid length - " + exp_val);
				}
			} else {
				exp_val = sym_len[cur_sid];
				if (exp_val >= 0 && exp_val <= 0x100){
					if (exp_val > 0)exp_val = exp_val - 1;
					hex_len = tz390.get_hex(exp_val,2);
				} else {
					log_error(64,"invalid length - " + exp_val);
				}
			}
		} else {
			if (exp_val >= 0 && exp_val < 0x1000){
			   hex_ddd = tz390.get_hex(exp_val,3);
			} else {
				log_error(56,"invalid displacement - " + exp_val);
			}
			if  (exp_text.length() > exp_index && exp_text.charAt(exp_index) == '('){
				exp_index++;
				if (calc_abs_exp() && exp_val >= 0 && exp_val <= 0x100){
					if (exp_val > 0)exp_val = exp_val -1;
					hex_len = tz390.get_hex(exp_val,2);
					if (exp_text.charAt(exp_index) == ','){
						exp_index++;
						if (calc_abs_exp() && exp_val >= 0 && exp_val <= 15){
							hex_b = tz390.get_hex(exp_val,1);
						} else {
							log_error(38,"invalid index register expression");
						}
					}
					exp_index++;
				} else {
					log_error(64,"invalid length expression");
				}
			} else {
				hex_b = "0";
			}
			hex_bddd = hex_b + hex_ddd;
		}
	}	
}
private void get_hex_xbddd(){
	/*
	 * append xbddd hex object code from next parm
	 */
	String hex_x   = "x";
	String hex_b   = "b";
	String hex_ddd = "ddd";
	hex_bddd2      = "bddd";
	if (exp_text.length() > exp_index && exp_text.charAt(exp_index) == '='){
		hex_bddd2 = get_lit_bddd();
		hex_bddd2_loc = tz390.get_hex(exp_val,6);
		hex_x = "0";
	} else if (calc_exp()){
		if  (exp_type == sym_rel){
			hex_bddd2 = get_exp_bddd();
			hex_bddd2_loc = tz390.get_hex(exp_val,6);
			if  (exp_text.length() > exp_index && exp_text.charAt(exp_index) == '('){
				exp_index++;
				if (calc_abs_exp() && exp_val >= 0 && exp_val <= 15){
					hex_x = tz390.get_hex(exp_val,1);
					exp_index++;
				} else {
					log_error(40,"invalid index register");
				}
			} else {
				hex_x = "0";
			}
		} else {
			if (exp_val >= 0 && exp_val < 0x1000){
			   hex_ddd = tz390.get_hex(exp_val,3);
			} else {
				log_error(56,"invalid displacement - " + exp_val);
			}
			if  (exp_text.length() > exp_index && exp_text.charAt(exp_index) == '('){
				exp_index++;
				if (exp_text.length() > exp_index && exp_text.charAt(exp_index) == ','){
					hex_x = "0";  //RPI3
				} else if (calc_abs_exp() 
						&& exp_val >= 0 && exp_val <= 15){
					hex_x = tz390.get_hex(exp_val,1);
				} else {
					log_error(39,"invalid index register expression");
				}
				if (exp_text.length() > exp_index && exp_text.charAt(exp_index) == ','){
					exp_index++;
					if (calc_abs_exp() 
						&& exp_text.length() > exp_index 
						&& exp_text.charAt(exp_index) == ')'
						&& exp_val >= 0 && exp_val <= 15){
						exp_index++;
						hex_b = tz390.get_hex(exp_val,1);
					} else {
						log_error(38,"invalid base register expression");
					}
				} else {
					if (exp_text.length() > exp_index && exp_text.charAt(exp_index) == ')'){
						hex_b = "0";
						exp_index++;
					} else {
						log_error(38,"invalid base register expression");
					}
				}
				hex_bddd2 = hex_b + hex_ddd;
			} else {
				hex_x = "0";
				hex_b = "0";
			}
			hex_bddd2 = hex_b + hex_ddd;
		}
	}
	obj_code = obj_code + hex_x + hex_bddd2;
}
private void get_hex_xbdddhh2(){
	/*
	 * append xbddd hex object code from next parm
	 */
	get_hex_xbddd();              //RPI161,RPI166
	obj_code = obj_code + "00";   //RPI161
}
private void get_hex_bddd2(boolean add_code){
	/*
	 * append bddd hex object code from next parm
	 * if add_code else just set hex_bddd2
	 */
	String hex_b   = "b";
	String hex_ddd = "ddd";
	hex_bddd2      = "bddd";
	if (exp_text.length() > exp_index && exp_text.charAt(exp_index) == '='){
		hex_bddd2 = get_lit_bddd();
		hex_bddd2_loc = tz390.get_hex(exp_val,6);
	} else if (calc_exp()){
		if  (exp_type == sym_rel){
			hex_bddd2 = get_exp_bddd();
			hex_bddd2_loc = tz390.get_hex(exp_val,6);
		} else {
			if (exp_val >= 0 && exp_val < 0x1000){
			   hex_b   = "0";
			   hex_ddd = tz390.get_hex(exp_val,3);
			} else {
				log_error(56,"invalid displacement - " + exp_val);
			}
			if  (exp_text.length() > exp_index && exp_text.charAt(exp_index) == '('){
				exp_index++;
				if (exp_text.length() > exp_index && exp_text.charAt(exp_index) == ','){
					exp_index++; //RPI3 skip index reg comma if any
				}
				if (calc_abs_exp() 
						&& exp_text.length() > exp_index 
						&& exp_text.charAt(exp_index) == ')' 
						&& exp_val >= 0 && exp_val <= 15){
					exp_index++;
					hex_b = tz390.get_hex(exp_val,1);
				} else {
					log_error(39,"invalid base expression");
				}
			} else {
				hex_b = "0";
			}
			hex_bddd2 = hex_b + hex_ddd;
		}
	}
	if (add_code){  //RPI120
		obj_code = obj_code + hex_bddd2;
	}
}
private void get_hex_bdddhh2(){
	/*
	 * append bdddhh hex object code from next parm
	 */
	get_hex_bddd2(true);
	obj_code = obj_code + "00";
}
private void get_hex_rel(){
	/*
	 * append iiii signed offset (calc for label)
	 */
    String hex_iiii = "iiii";
	if (calc_exp()){
		if  (exp_type == sym_rel){
			hex_iiii = get_rel_exp_iiii();
		} else {
			if (exp_val > 0xffff0000 && exp_val <= 0xffff){
			   hex_iiii = tz390.get_hex(exp_val,4);
			} else {
				log_error(63,"relative offset too large - " + exp_val);
			}
		}
	}
	obj_code = obj_code + hex_iiii;
}
private void get_hex_long(){
	/*
	 * append llllllll signed offset (calc for label)
	 */
    String hex_llllllll = "llllllll";
	if (calc_exp()){
		if  (exp_type == sym_rel){
			hex_llllllll = get_rel_exp_llllllll();
		} else {
		    hex_llllllll = tz390.get_hex(exp_val,8);
		}
	}
	obj_code = obj_code + hex_llllllll;
}
private String get_rel_exp_iiii(){
	/*
	 * return relative signed half word offset
	 * from psw_loc to symbol in same csect at
	 * even address
	 * Notes:
	 *   1.  Error if not same csect or too large
	 *       or odd address.
	 */
	String hex_iiii = "iiii";
	if (exp_esd == cur_esd){
		int hw_off = (exp_val - loc_start)/2;
		if (hw_off >= -0x8000 && hw_off <= 0x7fff){
			if ((exp_val & 0x1) == 0){
				hex_iiii = tz390.get_hex(hw_off,4);
			} else {
				log_error(111,"relative target address is odd - " + tz390.get_hex(exp_val,8));
			}
		} else {
			log_error(74,"relative offset too large - " + tz390.get_hex(hw_off,8));
		}
	} else {
		log_error(75,"relative offset not in same esd");
	}
	return hex_iiii;
}
private String get_rel_exp_llllllll(){
	/*
	 * return relative signed hald word offset
	 * from psw_loc to symbol in same csect at
	 * even address
	 * Notes:
	 *   1.  Error if not same csect or odd address
	 */
	String hex_llllllll= "llllllll";
	if (exp_esd == cur_esd){
		if ((exp_val & 0x1) == 0){
			exp_val = (exp_val - loc_start)/2;
			hex_llllllll = tz390.get_hex(exp_val,8);
		} else {
			log_error(112,"relate target address odd - " + tz390.get_hex(exp_val,8));
		}
	} else {
		log_error(76,"relative offset not in same esd");
	}
	return hex_llllllll;
}
private String get_exp_bddd(){
	/*
	 * 1.  Return hex bddd for expression
	 *     or literal based on exp_esd and exp_val.
	 * 
	 * 2.  Set cur_reg and cur_reg_loc for use
	 *     when called from dependant using with
	 *     domain expression.
	 * 3.  If exp_use_lab is not null restrict
	 *     using entries to labelled using.  
	 */
	if (!gen_obj_code){
		return "bddd";
	} else if (exp_esd == 0 && exp_val < 4096){ //rpi11
		String ddd = Integer.toHexString(exp_val);
		return ("000" + ddd).substring(ddd.length()-1);
	}
	cur_use_reg = -1;  // assume not found
	cur_use_reg_loc = 4096;
	int test_offset = 0;
	int index = cur_use_start;
	while (index < cur_use_end){
		if (use_base_esd[index] == exp_esd
			&& ((exp_use_lab == null 
				 && use_lab[index].length() == 0)
				||(exp_use_lab != null 
				   && use_lab[index].equals(exp_use_lab))
				)
			){
			test_offset = exp_val - use_base_loc[index];
			if (test_offset < cur_use_reg_loc
					&& test_offset >= 0
					&& test_offset < use_base_len[index]){
				cur_use_reg = use_reg[index];
				cur_use_reg_loc = test_offset + use_reg_loc[index];
			}
		}
		index++;
	}
	if (cur_use_reg != -1){
	    exp_use_lab = null;
		return tz390.get_hex(cur_use_reg,1) + tz390.get_hex(cur_use_reg_loc,3);
	} else {
		log_error(38,"no base register found");
	    exp_use_lab = null;
		return "bddd";
	}
}
private void get_dc_field_dup(){
    /*
     * return dup factor for dc_field else 1
     */
	 dc_dup_loc = 0;
	 dc_dup = 1;
     if (dc_field.charAt(dc_index) == '('){
     	exp_text = dc_field;
     	exp_index = dc_index + 1;
     	if (calc_abs_exp()){
     		dc_index = exp_index + 1;
     		dc_dup = exp_val;
     	} else {
     		log_error(43,"invalid dc duplication factor");
     	}
     } else {
        dc_dup = get_dc_int(dc_index);
     }
}
private void get_dc_field_type(){
	/* 
	 * 1.  set dc_type and dc_type_index 
	 *     and verify else abort
	 * 2,  if DEF check for B/H and set fp_type
	 */
      dc_type = dc_field.substring(dc_index,dc_index+1).toUpperCase().charAt(0);
      dc_index++;
      dc_type_index = tz390.dc_valid_types.indexOf(dc_type);
      if (dc_type_index == -1){
      	 log_error(51,"invalid dc type - " + dc_field.substring(0,dc_index));
      } else {
      	 if (dc_index < dc_field.length()){
      	 	dc_type_sfx = dc_field.substring(dc_index,dc_index+1).toUpperCase().charAt(0);
      	 	switch (dc_type){
      	 	case 'D':
      	 		if (dc_type_sfx == 'B'){
      	 			fp_type = fp_db_type;
      	 			dc_index++;
      	 		} else {
      	 			fp_type = fp_dh_type;
      	 			if (dc_type_sfx == 'H'){
      	 			   dc_index++;
      	 			}
      	 		}
      	 		break;
      	    case 'E':
      	 		if (dc_type_sfx == 'B'){
      	 			fp_type = fp_eb_type;
      	 			dc_index++;
      	 		} else {
      	 			fp_type = fp_eh_type;
      	 			if (dc_type_sfx == 'H'){
       	 			   dc_index++;
       	 			}
      	 		}
      	 		break;
      	    case 'L':
      	 		if (dc_type_sfx == 'B'){
      	 			fp_type = fp_lb_type;
      	 			dc_index++;
      	 		} else {
      	 			fp_type = fp_lh_type;
      	 			if (dc_type_sfx == 'H'){
       	 			   dc_index++;
       	 			}
      	 		}
      	 	}
      	 }
      }
}
private void get_dc_field_len(){
/*
 * 1. set dc_len based on either explicit length
 *    or default for type.
 * 2. Align if required for first field if
 *    not literal reference and not explicit len
 * 3. Set loc_start of first operand for listing 
 */
 if (dc_type_index != -1){
    dc_len = tz390.dc_type_len[dc_type_index];
 } else {
 	dc_len = 1;
 }
 dc_len_explicit = false;
 if (dc_index < dc_field.length() 
 	&& dc_field.substring(dc_index,dc_index+1).toUpperCase().charAt(0) == 'L'){
 	dc_len_explicit = true;
 	if (dc_field.charAt(dc_index+1) == '('){
    	exp_text = dc_field;
 	    exp_index = dc_index+2;
 	    if (calc_abs_exp()
 	    		&& dc_field.charAt(exp_index) == ')'){
 	       dc_index = exp_index+1;
 		   dc_len = exp_val;
 	    } else {
 		   log_error(43,"invalid dc duplication factor");
 	    }
 	} else {
        dc_len = get_dc_int(dc_index+1);
 	}
 } 
 if (dc_first_field){
 	dc_first_len = dc_len; // may be overridden by non-explicit data length
 	if (!dc_lit_ref && !dc_len_explicit){
 		loc_ctr = (loc_ctr + dc_len -1)/dc_len*dc_len;
 	}
 	loc_start = loc_ctr;
 }
}
private void process_dca_data(){
	/*
	 * alloc or gen DS/DC A type parms using prev.
	 * settings for dc_dup and dc_len.  Also save
	 * first field dc_type, dc_len
	 */
	exp_text = dc_field;
	dc_index++;   // start inside (,,,)
	dc_data_start = dc_index; 
	if (dc_op && !dc_lit_ref && (dc_len == 3 || dc_len == 4)){  //RPI182
		exp_rld_len = (byte) dc_len;
	} else {
		exp_rld_len = 0;
	}
	exp_index = dc_index;
	while (!dc_eod && !bal_abort){
		while (!dc_eod && !bal_abort){
		    if  (calc_dca_exp()){
			    dc_index = exp_index;
			    if (gen_obj_code
			    	&& !dc_lit_ref
                    && sym_type[cur_esd_sid] == sym_cst
					&& dc_dup > 0
					&& dc_op){
			    	if (dc_len <= 4 || exp_val >= 0){
				        obj_code = obj_code + tz390.get_hex(exp_val,2*dc_len);
			    	} else {
				        obj_code = obj_code + ("FFFFFFFF").substring(0,2*dc_len-8) + tz390.get_hex(exp_val,8);
			    	}
					put_obj_text();
			    } 
			    if (!dc_lit_ref && dc_dup > 0){
				   loc_ctr = loc_ctr + dc_len;
				   dc_dup_loc = dc_dup_loc + dc_len;
			    }
			    if (dc_field.charAt(dc_index) == ','){
			    	exp_index++;
			    } else if (dc_field.charAt(dc_index) == ')'){
			    	if (dc_dup > 1){         //RPI2 start
					    dc_index = dc_data_start;
					    exp_index = dc_index; 
					    dc_dup--; 
			    	} else { 
			    		dc_eod = true;	
			    	}                        //RPI2
			    } else {
			    	log_error(105,"invalid dc data terminator - " + dc_field.substring(dc_index));
			    }
		    }
		}
	    dc_index++; // skip dca terminator
	    dc_len = 0; // don't double count
	}
	exp_rld_len = 0;
}
private void check_dc_value(){
	/*
	 * issue error if F or H type exp_val
	 * exceeds size of filed dc_len.
	 * Notes:
	 *   1.  RPI85
	 */
    int[] max_fh = {0x7f,0x7fff,0x7fffff};
    int[] min_fh = {0xffffff80,0xffff8000,0xff800000};
	switch (dc_len){
	case 1:
	case 2:
	case 3:
		if (exp_val > max_fh[dc_len-1] 
	         || exp_val < min_fh[dc_len-1]){
		    log_error(113,"signed value out of range - x'" + tz390.get_hex(exp_val,8) + "'");
		}
	}
}
private void process_dcb_data(){
	/*
	 * alloc or gen DS/DC B type parms using prev.
	 * settings for dc_dup and dc_len.  Also save
	 * first field dc_len
	 * Notes:
	 *   1.  binary values are right aligned in 
	 *       explicit length fields.
	 */
	dc_index++;   // start inside 'bin1,bin2,,'
	dc_data_start = dc_index; 
	while (!dc_eod && !bal_abort){
		int dcb_start = dc_index;
		while (!dc_eod && !bal_abort
				&& dc_index < dc_field.length()
				&& dc_field.charAt(dc_index) != '\''
				&& dc_field.charAt(dc_index) != ','){
			    dc_index++;
		}
		if (dc_index >= dc_field.length()){
			log_error(65,"invalid binary dc data " + dc_field.substring(dc_data_start));
			return;
		}
		int dcb_len = dc_index - dcb_start;
		int dcb_pad = 8 - (dcb_len - dcb_len/8*8);
		String dcb_bin = "";
		if (dcb_pad != 8){
			dcb_bin = "00000000".substring(0,dcb_pad) + dc_field.substring(dcb_start,dc_index);
		} else {
			dcb_bin = dc_field.substring(dcb_start,dc_index);
		}
		int index = 0;
		dc_hex = "";
		while (index < dcb_bin.length()){
			String dcb_hex = Integer.toHexString(Integer.valueOf(dcb_bin.substring(index,index+8),2).intValue()).toUpperCase();
			if (dcb_hex.length() < 2){
				dc_hex = dc_hex + "0" + dcb_hex;
			} else {
				dc_hex = dc_hex + dcb_hex;
			}
			index = index + 8;
		}
		dcb_len = dc_hex.length()/2;
		if (dc_len_explicit){
			while (dcb_len < dc_len){
				dc_hex = "00" + dc_hex;
				dcb_len++;
			}
			if (dcb_len > dc_len){
				dc_hex = dc_hex.substring(2*(dcb_len-dc_len));
				dcb_len = dc_len;
			}
		} else {
			dc_len = dcb_len;
		}
		if (!dc_len_explicit && dc_first_field){
			dc_first_len = dc_len;
			dc_first_field = false;
		}
		if (gen_obj_code
		   	&& !dc_lit_ref
            && sym_type[cur_esd_sid] == sym_cst
		    && dc_dup > 0
			&& dc_op){
			obj_code = obj_code + dc_hex;
			put_obj_text();
		}
		if (!dc_lit_ref && dc_dup > 0){
		   loc_ctr = loc_ctr + dc_len;
		}
		if (dc_field.charAt(dc_index) == ','){
		   	dc_index++;
		} else if (dc_field.charAt(dc_index) == '\'') {
	        dc_index++; // skip dch terminator
	        dc_len = 0; // don't double count
	        if  (!bal_abort){
		        if  (dc_dup > 1){  //rpi2
			        dc_index = dc_data_start;
			        dcb_start = dc_index;
			        exp_index = dc_index;
			        dc_dup--;
		        } else {
			        dc_eod = true;
		        }                  //rpi2
	        }
	    } else {
	    	log_error(106,"invalid dc data terminator - " + dc_field.substring(dc_index));
	    }
	}
	exp_rld_len = 0;
}
private void process_dcc_data(){
	/*
	 * allocate or generate dc Cln'...' data
	 * using dc_dup and explicit dc_len if any
	 * Notes:
	 *   1.  C'..' default EBCDIC unless ASCII option
	 *   2.  C".." always ASCII regardless of option
	 *   3.  C!..! always EBCDIC regardless of option
	 *   4.  ''|""|!! or && replaced with single '|"|! or &
	 */
	String dcc_text = "";
	String token = null;
	int dcc_len  = 0;
	int dcc_next = 0;
	char dcc_quote = dc_field.charAt(dc_index); // ',", or ! delimiter
    if (dcc_quote == '\''){
    	dcc_match = dcc_sq_pattern.matcher(dc_field.substring(dc_index + 1));
    } else if (dcc_quote == '"') {
    	dcc_match = dcc_dq_pattern.matcher(dc_field.substring(dc_index + 1));
    } else {
    	dcc_match = dcc_eq_pattern.matcher(dc_field.substring(dc_index + 1));
    }
	while (!dc_eod && !bal_abort
			&& dcc_match.find()){
	       token = dcc_match.group();
	       dcc_next = dcc_match.end();
	       if (token.charAt(0) != dcc_quote 
	    		   && token.charAt(0) != '\''  //RPI192
	    		   && token.charAt(0) != '&'){ //RPI192
	       	  dcc_text = dcc_text + token;
	       } else if (token.length() == 2){
	       	  dcc_text = dcc_text + token.charAt(0); //RPI192
	       } else if (token.charAt(0) == dcc_quote){
	       	  dc_eod = true;
	       } else {
	    	  log_error(137,"invalid single " + token.charAt(0)); 
	       }
	}
	if (!dc_eod){
		log_error(52,"invalid dc character literal - " + dc_field.substring(dc_index));
	}
	dc_index = dc_index + dcc_next + 1;
	dcc_len = dcc_text.length();
	if  (dc_len_explicit){
    	if  (dc_len > dcc_len){
            while (dcc_len < dc_len){
	      	 	dcc_text = dcc_text + " ";
	      	 	dcc_len++;
	        }
	    } else {
	    	dcc_text = dcc_text.substring(0,dc_len);
	    }
	} else {
	    dc_len = dcc_len;
	}
	if (dc_first_field && !dc_len_explicit){
	    dc_first_len = dc_len;
		dc_first_field = false;
	}
	while (!bal_abort
		&& dc_dup > 0){
		if (gen_obj_code
			&& !dc_lit_ref
		    && sym_type[cur_esd_sid] == sym_cst
			&& dc_op){
			 boolean ascii_req = (tz390.opt_ascii && dcc_quote == '\'')
			 | dcc_quote == '"';  //RPI5 and RPI73
 	         obj_code = obj_code + string_to_hex(dcc_text,ascii_req);
  		     put_obj_text();
		}
		if (!dc_lit_ref){
  	       loc_ctr = loc_ctr + dc_len;
		}
   	    dc_dup--;
	}
	dc_len = 0;
}
private void process_dc_fp_data(){
	/*
	 * alloc or gen DS/DC D, E, or F type data using
	 * prev settings for dc_type, dc_type_sfx,
	 * dc_dup and dc_len.  Also save
	 * first field dc_type, dc_len
	 */
	exp_text = dc_field;
	dc_index++;   // start inside (,,,)
	exp_index = dc_index;
	dc_data_start = dc_index; 
	exp_rld_len = 0;
	while (!dc_eod && !bal_abort){
		while (!dc_eod && !bal_abort 
				&& dc_field.charAt(dc_index) != '\''){
			dc_hex = get_dc_fp_hex(dc_field,dc_index);
			if (gen_obj_code
			    	&& !dc_lit_ref
                    && sym_type[cur_esd_sid] == sym_cst
					&& dc_dup > 0
					&& dc_op){
				obj_code = obj_code + dc_hex;
				put_obj_text();
			}
			if (!dc_lit_ref && dc_dup > 0){
			   loc_ctr = loc_ctr + dc_len;
			}
			if (dc_field.charAt(dc_index) == ','){
			   	exp_index++;
			}
		}
	    dc_index++; // skip dca ) terminator
	    dc_len = 0; // don't double count
	    if  (!bal_abort){
		    if  (dc_dup > 1){
			    dc_index = dc_data_start;
			    dc_dup--;
		    } else {
			    dc_eod = true;
		    }
	    }
	}
	exp_rld_len = 0;
}
private String get_dc_fp_hex(String text,int index){
	/*
	 * return hex for D, E, or F floating point sdt
	 */
	String hex = "";
	int text_len = index;
	while (index < text.length()){
		if (text.charAt(index) == '\''
			|| text.charAt(index) == ','){
			text_len = index - text_len;
		    hex = get_fp_hex(fp_type,text.substring(index-text_len,index));
		    dc_index = index;
		    return hex;
		} else {
			index++;
		}
	}
	log_error(66,"invalide floating point data field");
	return "00";
}
private void process_dcf_data(){
	/*
	 * alloc or gen DS/DC F type parms using prev.
	 * settings for dc_dup and dc_len.  Also save
	 * first field dc_type, dc_len
	 */
	exp_text = dc_field;
	dc_index++;   // start inside ',,,'
	dc_data_start = dc_index; 
	exp_rld_len = 0;
	exp_index = dc_index;  //RPI2
	while (!dc_eod && !bal_abort){
		while (!dc_eod && !bal_abort){
		    if  (calc_abs_exp()){
		    	check_dc_value();
			    dc_index = exp_index;
			    if (gen_obj_code
			    	&& !dc_lit_ref
                    && sym_type[cur_esd_sid] == sym_cst
					&& dc_dup > 0
					&& dc_op){
			    	if (dc_len <= 4 || exp_val >= 0){
				        obj_code = obj_code + tz390.get_hex(exp_val,2*dc_len);
			    	} else {
				        obj_code = obj_code + ("FFFFFFFF").substring(0,2*dc_len-8) + tz390.get_hex(exp_val,8);
			    	}
					put_obj_text();
			    }
			    if (!dc_lit_ref && dc_dup > 0){
				   loc_ctr = loc_ctr + dc_len;
			    }
			    if (dc_field.charAt(dc_index) == ','){
			    	exp_index++;
			    } else if (dc_field.charAt(dc_index) == '\''){
			    	if (dc_dup > 1){         //RPI2 start
					    dc_index = dc_data_start;
					    exp_index = dc_index; 
					    dc_dup--; 
			    	} else { 
			    		dc_eod = true;	
			    	}                        // RPI2 end
			    } else {
				    log_error(107,"invalid data field terminator - " + dc_field);
			    }
		    } else {
			    log_error(88,"invalid data field expression - " + dc_field);
		    }
		}
	    dc_index++; // skip dcf terminator
	    dc_len = 0; // don't double count
	}
	exp_rld_len = 0;
}
private void process_dch_data(){
	/*
	 * alloc or gen DS/DC H type parms using prev.
	 * settings for dc_dup and dc_len.  Also save
	 * first field dc_type, dc_len
	 */
	exp_text = dc_field;
	dc_index++;   // start inside (,,,)
	dc_data_start = dc_index; 
	exp_rld_len = 0;
	exp_index = dc_index;
	while (!dc_eod && !bal_abort){
		while (!dc_eod && !bal_abort){
		    if  (calc_abs_exp()){
		    	check_dc_value();
			    dc_index = exp_index;
			    if (gen_obj_code
			    	&& !dc_lit_ref
                    && sym_type[cur_esd_sid] == sym_cst
					&& dc_dup > 0
					&& dc_op){
			    	if (dc_len <= 4 || exp_val >= 0){
				        obj_code = obj_code + tz390.get_hex(exp_val,2*dc_len);
			    	} else {
				        obj_code = obj_code + ("FFFFFFFF").substring(0,2*dc_len-8) + tz390.get_hex(exp_val,8);
			    	}
					put_obj_text();
			    }
			    if (!dc_lit_ref && dc_dup > 0){
				   loc_ctr = loc_ctr + dc_len;
			    }
			    if (dc_field.charAt(dc_index) == ','){
			    	exp_index++;
			    } else if (dc_field.charAt(dc_index) == '\''){
			    	if (dc_dup > 1){         //RPI2 start
					    dc_index = dc_data_start;
					    exp_index = dc_index; 
					    dc_dup--; 
			    	} else { 
			    		dc_eod = true;	
			    	}                        // RPI2 end
			    } else {
				    log_error(108,"invalid data field terminator - " + dc_field);
			    }
		    } else {
			    dc_index = exp_index;
	         	log_error(88,"invalid data field expression - " + dc_field);
		    }
		}
	    dc_index++; // skip dch terminator
	    dc_len = 0; // don't double count
	}
	exp_rld_len = 0;
}
private void process_dcp_data(){
	/*
	 * alloc or gen DS/DC P type parms using prev.
	 * settings for dc_dup and dc_len.  Also save
	 * first field dc_type, dc_len
	 */
	char dcp_sign;
	String dcp_text;
	dc_index++;   // start inside delimiter 'n,n'
	dc_data_start = dc_index; 
	while (!dc_eod && !bal_abort){
		while (!dc_eod && !bal_abort
				&& dc_index < dc_field.length()
				&& dc_field.charAt(dc_index) != '\''){
			    dcp_sign = 'C';
			    int index1 = dc_index;
			    while (dc_index < dc_field.length() 
			    		&& dc_field.charAt(dc_index) != ','
			    	    && dc_field.charAt(dc_index) != '\''){
			         if (dc_field.charAt(dc_index) >= '0'
			         	&& dc_field.charAt(dc_index) <= '9'){
			         	dc_index++;
			         } else if (dc_field.charAt(dc_index) == '+'){
			         	dc_index++;
			         	index1 = dc_index;
			         } else if (dc_field.charAt(dc_index) == '-'){
			         	dcp_sign = 'D';
			         	dc_index++;
			         	index1 = dc_index;
			         } else {
			         	log_error(67,"invalid character in P type data field - " + dc_field);
			         }
			    }
			    int dcp_digits = dc_index - index1;
			    if (dcp_digits - dcp_digits/2*2 == 0){
			    	dcp_text = '0' + dc_field.substring(index1,dc_index) + dcp_sign;
			    } else {
			    	dcp_text = dc_field.substring(index1,dc_index) + dcp_sign;
			    }
			    int dcp_len = dcp_text.length()/2;
			    if (dc_len_explicit){
			        while (dcp_len < dc_len){
			        	dcp_text = "00" + dcp_text;
			        	dcp_len++;
			    	}
			        if (dcp_len > dc_len){
			        	dcp_text = dcp_text.substring(2*(dcp_len - dc_len));
			        }
			    } else {
			        dc_len = dcp_len;
			    }
				if (!dc_len_explicit && dc_first_field){
					dc_first_len = dcp_len;
					dc_first_field = false;
				}
			    if (dc_len > 16){
			       log_error(68,"P type field too long - " + dc_field);
			    } else if (gen_obj_code
			    	&& !dc_lit_ref
                    && sym_type[cur_esd_sid] == sym_cst
					&& dc_dup > 0
					&& dc_op){
			    	obj_code = dcp_text;
					put_obj_text();
			    }
			    if (!dc_lit_ref && dc_dup > 0){
				   loc_ctr = loc_ctr + dc_len;
			    }
			    if (dc_field.charAt(dc_index) == ','){
			    	dc_index++;
			    }
		}
	    dc_index++; // skip dcp terminator
	    dc_len = 0; // don't double count
	    if  (!bal_abort){
		    if  (dc_dup > 1){
			    dc_index = dc_data_start;
			    dc_dup--;
		    } else {
			    dc_eod = true;
		    }
	    }
	}
}
private void process_dcs_data(){
	/*
	 * alloc or gen DS/DC S type parms using prev.
	 * settings for dc_dup and dc_len.  Also save
	 * first field dc_type, dc_len
	 */
	exp_text = dc_field;
	dc_index++;   // start inside (,,,)
	exp_index = dc_index;
	dc_data_start = dc_index; 
    exp_rld_len = 0;
	while (!dc_eod && !bal_abort){
		while (!dc_eod && !bal_abort){
		    if  (calc_exp()){
			    dc_index = exp_index;
			    if (gen_obj_code
			    	&& !dc_lit_ref
                    && sym_type[cur_esd_sid] == sym_cst
					&& dc_dup > 0
					&& dc_op){
			    	if  (dc_len == 2){
		    		    obj_code = obj_code + get_exp_bddd();
			    	} else {
			    		log_error(99,"invalid length for S type");
			    	}
					put_obj_text();
			    } 
			    if (!dc_lit_ref && dc_dup > 0){
				   loc_ctr = loc_ctr + dc_len;
				   dc_dup_loc = dc_dup_loc + dc_len;
			    }
			    if (dc_field.charAt(dc_index) == ','){
			    	exp_index++;
			    } else if (dc_field.charAt(dc_index) == ')'){
			    	if (dc_dup > 1){         //RPI2 start
					    dc_index = dc_data_start;
					    exp_index = dc_index; 
					    dc_dup--; 
			    	} else { 
			    		dc_eod = true;	
			    	}                        // RPI2 end
			    } else {
				    log_error(109,"invalid data field terminator - " + dc_field);
			    }
		    } else {
			    dc_index = exp_index;
	         	log_error(88,"invalid data field expression - " + dc_field);
		    }
		}
	    dc_index++; // skip dca terminator
	    dc_len = 0; // don't double count
	}
	exp_rld_len = 0;
}
private void process_dcx_data(){
	/*
	 * alloc or gen DS/DC X type parms using prev.
	 * settings for dc_dup and dc_len.  Also save
	 * first field dc_len
	 * Notes:
	 *   1.  hex values are right aligned in 
	 *       explicit length fields.
	 */
	dc_index++;   // start inside 'hex1,hex2,,'
	dc_data_start = dc_index; 
	while (!dc_eod && !bal_abort){
		int dcx_start = dc_index;
		while (!dc_eod && !bal_abort
				&& dc_index < dc_field.length()
				&& dc_field.charAt(dc_index) != '\''
				&& dc_field.charAt(dc_index) != ','){
			    char hex_code = dc_field.substring(dc_index,dc_index + 1).toUpperCase().charAt(0);
			    if ((hex_code >= '0' && hex_code <= '9')
			    	||
					(hex_code >= 'A' && hex_code <= 'F')){
			    	dc_index++;
			    } else {
			    	log_error(77,"invalid hex code " + hex_code);
			    }
		}
		if (dc_index >= dc_field.length()){
			log_error(78,"invalid hex dc data " + dc_field.substring(dc_data_start));
			return;
		}
		int dcx_len = dc_index - dcx_start;
		if (dcx_len != dcx_len/2*2){
			dc_hex = "0" + dc_field.substring(dcx_start,dc_index);
		} else {
			dc_hex = dc_field.substring(dcx_start,dc_index);
		}
		dcx_len = dc_hex.length()/2;
		if (dc_len_explicit){
			while (dcx_len < dc_len){
				dc_hex = "00" + dc_hex;
				dcx_len++;
			}
			if (dcx_len > dc_len){
				dc_hex = dc_hex.substring(2*(dcx_len-dc_len));
				dcx_len = dc_len;
			}
		} else {
			dc_len = dcx_len;
		}
		if (!dc_len_explicit && dc_first_field){
			dc_first_len = dc_len;
			dc_first_field = false;
		}
		if (gen_obj_code
		   	&& !dc_lit_ref
            && sym_type[cur_esd_sid] == sym_cst
		    && dc_dup > 0
			&& dc_op){
			obj_code = obj_code + dc_hex;
			put_obj_text();
		}
		if (!dc_lit_ref && dc_dup > 0){
		   loc_ctr = loc_ctr + dc_len;
		}
		if (dc_field.charAt(dc_index) == ','){
		   	dc_index++;
		} else if (dc_field.charAt(dc_index) == '\''){
	        if  (dc_dup > 1){    //rpi2
		        dc_index = dc_data_start;
		        exp_index = dc_index;
		        dc_dup--;
	        } else {
		        dc_eod = true;
	        }                    //rpi2
	    } else {
	    	log_error(109,"invalid dc data terminator - " + dc_field.substring(dc_index));
	    }
	}
    dc_index++; // skip terminator
    dc_len = 0; // don't double count
}
private int get_dc_int(int index){
	/*
	 * return next number from dc_field at index
	 * else return 1 and update dc_index
	 */
	dc_index = index;
 	while (dc_index < dc_field.length() 
 			&& dc_field.charAt(dc_index) <= '9'
 		    && dc_field.charAt(dc_index) >= '0'){
 		dc_index++;
 	}
 	if (dc_index > index){
 		return Integer.valueOf(dc_field.substring(index,dc_index)).intValue();
 	} else {
 		return 1;
 	}
}
private void process_cnop(){
	/*
	 * generate one or more 0700 instr.
	 * to align to specified boundary
	 */
	exp_text = bal_parms;
	exp_index = 0;
	int req_off = 0;
	int cur_off = 0;
	if (calc_abs_exp() 
			&& exp_val >= 0 
			&& exp_val <  8){ 
		 req_off = exp_val;
		 if (exp_text.charAt(exp_index) == ','){
			 exp_index++;
			 if (calc_abs_exp() 
					 && (exp_val == 4
					     || exp_val == 8)){
				 cur_off = loc_ctr - loc_ctr/exp_val*exp_val;
                 int gap_bytes = req_off - cur_off;
                 if (gap_bytes < 0){
                	 gap_bytes = exp_val - cur_off + req_off;
                 }
                 if ((gap_bytes & 0x1) > 0){
                	 loc_len = 1;
                	 gap_bytes--;
                	 obj_code = "00";       
                 }
                 while (gap_bytes > 0){
                	 loc_len = loc_len+2;
                	 gap_bytes = gap_bytes -2;
               		 obj_code = obj_code + "0700";
                 }
                 put_obj_text();
			 } 
		 }
	}
}
private void process_equ(){
	/* 
	 * define or update symbol definition
	 */
	check_private_csect();
	loc_start = loc_ctr;
	if (bal_label != null){
		cur_sid = find_sym(bal_label);
		if (cur_sid < 1){
			cur_sid = add_sym(bal_label);
		}
		int store_sid = cur_sid;
		sym_name[store_sid] = bal_label;
		if (sym_def[store_sid] == 0){ 
			sym_def[store_sid] = bal_line_index;
		} else if (sym_def[store_sid] != bal_line_index){
			duplicate_symbol_error();
		}
		exp_text = bal_parms;
		exp_index = 0;
		if (calc_exp()){
			sym_type[store_sid] = exp_type;
			sym_esd[store_sid] = exp_esd;
			sym_loc[store_sid] = exp_val;
			sym_len[store_sid] = 1;
			hex_bddd1_loc = tz390.get_hex(exp_val,6);
		} else {
			log_error(53,"invalid equ expression");
		}
	}
}
private void process_org(){
	/*
	 * reset current location in same csect
	 */
	loc_start = loc_ctr;
	if (bal_parms == null || bal_parms.length() == 0){
		if (cur_esd > 0){  //RPI10, RPI87
			loc_ctr = sym_loc[esd_sid[cur_esd]] + sym_len[esd_sid[cur_esd]];
			loc_start = loc_ctr;
		} else {
			log_error(102,"org expression must be in same section");
		}
		return;
	}
	exp_text = bal_parms;
	exp_index = 0;
	if (cur_esd > 0
		&& calc_rel_exp()
		&& exp_esd == cur_esd){
		if (loc_ctr > exp_val){
			update_sect_len();  //RPI10
		}
		loc_ctr = exp_val;
		loc_start = loc_ctr;
	} else {
		log_error(102,"org expression must be in same section");
	}
}
private void process_push(){
	/*
	 * push print or using level if any
	 */
	init_get_next_parm(bal_parms);
	String parm = get_next_parm();
    while (parm != null){
		if (parm.equals("PRINT")){
			if (print_level < max_push-1){
				print_on[print_level+1] = print_on[print_level];
				print_gen[print_level+1] = print_gen[print_level];
				print_data[print_level+1] = print_data[print_level];
				print_level++;
			} else {
				log_error(126,"maximum push print exceeded");
			}
		} else if (parm.equals("USING")){
			int cur_entries = cur_use_end - cur_use_start;
			if (using_level < max_push-1
					&& cur_use_end + cur_entries <= max_use){
				push_cur_use_start[using_level] = cur_use_start;
				push_cur_use_end[using_level]   = cur_use_end;
				int index = cur_use_start;
				while (index < cur_use_end){
					move_use_entry(index,index+cur_entries);
					index++;
				}
				using_level++;
				cur_use_start = cur_use_start + cur_entries;
				cur_use_end   = cur_use_end   + cur_entries;
			} else {
				log_error(127,"maximum push using exceeded");
			}
		} else {
			log_error(129,"invalid push parm - " + parm);
		}
		parm = get_next_parm();
	}	
}
private void process_pop(){
	/*
	 * pop print or using level if any
	 */
	init_get_next_parm(bal_parms);
	String parm = get_next_parm();
    while (parm != null){
		if (parm.equals("PRINT")){
			if (print_level > 0){
				print_level--;
			}
		} else if (parm.equals("USING")){
			if (using_level > 0){
				using_level--;
				cur_use_start = push_cur_use_start[using_level];
				cur_use_end   = push_cur_use_end[using_level];
			}
		} else {
			log_error(125,"invalid pop parm - " + parm);
		}
		parm = get_next_parm();
	}	
}
private void process_print(){
	/*
	 * process print options
	 */
	init_get_next_parm(bal_parms);
	String parm = get_next_parm();
    while (parm != null){
		if (parm.equals("ON")){
            print_on[print_level] = true;
		} else if (parm.equals("OFF")){
			print_on[print_level] = false;
		} else if (parm.equals("GEN")){
			print_gen[print_level] = true;
		} else if (parm.equals("NOGEN")){
			print_gen[print_level] = false;
		} else if (parm.substring(0,4).equals("DATA")){
			print_data[print_level] = true;
		} else if (parm.equals("NODATA")){
			print_data[print_level] = false;
		} else {
			log_error(128,"undefined print option - " + parm);
		}
		parm = get_next_parm();
	}
}
private void init_get_next_parm(String parms){
	/*
	 * use parm_match to find and return next parm
	 * separated by commas else return null.
	 * 
	 */
	if (parms != null && parms.length() > 0){
		parm_match = parm_pattern.matcher(parms);
	} else {
		parm_match = null;
	}
}
private String get_next_parm(){
	/*
	 * use parm_match to find and return next parm
	 * in upper case else return null.
	 * 
	 */
	if (parm_match != null){
		while (parm_match.find()){
			String parm = parm_match.group().toUpperCase();
			if (parm.charAt(0) <= ' '){
				return null;
			}
			if (parm.charAt(0) != ','){
				return parm;
			}
		}
	}
	return null;
}
private void duplicate_symbol_error(){
	/*
	 * issue error for duplicate symbol definition
	 */
	log_error(72,"duplicate symbol on line" + bal_line_num[bal_line_index] + " and " + bal_line_num[sym_def[cur_sid]]);
}
private String get_lit_bddd(){
	/*
	 * find or add literal for next literal pool
	 * return 
	 * and skip literal starting with = at exp_index
	 */
	String lit_key = "";
	process_dc(2);
	if (!bal_abort){
		if (lit_loc_ref){
			lit_key = "L:" + cur_lit_pool + ":" +bal_line_index + dc_field.substring(dc_lit_index_start,dc_index);
		} else {
			lit_key = "L:" + cur_lit_pool + dc_field.substring(dc_lit_index_start,dc_index);
		}
		cur_lit = tz390.find_key_index(lit_key);
		if (cur_lit != -1){
			add_lit_xref(cur_lit);
			if (lit_loc_ref){
				lit_line_loc[cur_lit] = loc_ctr;
			}
			exp_esd = lit_esd[cur_lit];
			exp_val = lit_loc[cur_lit];
            String test_bddd = get_exp_bddd();
            if (gen_obj_code && test_bddd.charAt(0) == '0'){
            	log_error(140,"literal missing base register");
            }
            return test_bddd;
		}
		if (!gen_obj_code && tot_lit < max_lit){
		    cur_lit = tot_lit;
			if (!tz390.add_key_index(cur_lit)){
			    abort_error(87,"key search table exceeded");
			}
			add_lit_xref(cur_lit);
		    tot_lit++;
			lit_name[cur_lit] = dc_field.substring(dc_lit_index_start,dc_index);
			lit_pool[cur_lit] = cur_lit_pool;
			lit_line[cur_lit] = bal_line_index;
			lit_line_loc[cur_lit] = loc_ctr;
			lit_esd[cur_lit] = cur_esd;
			lit_loc[cur_lit] = -1; // set by gen_lit
			lit_len[cur_lit] = dc_first_len;
			lit_gen[cur_lit] = 0;  // set by gen_lit;
			exp_val = -1;
		} else {
			log_error(57,"literal table size exceeded");
		}
	}
	return "bddd";
}
private void gen_ltorg(){
	/* 
	 * generate ltorg at current location in csect
	 */
	loc_ctr = (loc_ctr + 7)/8*8;
	gen_lit_size(8);
	gen_lit_size(4);
	gen_lit_size(2);
	gen_lit_size(1);
}
private void gen_lit_size(int size){
	/*
	 * generate literal dc's of specified size
	 */
	cur_lit = 0;
	while (cur_lit < tot_lit){
		if (lit_len[cur_lit] == lit_len[cur_lit]/size*size
				&& lit_gen[cur_lit] == 0
				&& lit_pool[cur_lit] == cur_lit_pool
				){
			lit_gen[cur_lit] = 1;
			process_dc(3);
			if (tz390.opt_tracea || (gen_obj_code && tz390.opt_list)){
				if (list_obj_code.length() < 16){
				   list_obj_code = list_obj_code.concat("                ").substring(0,16);
				} 
				list_obj_loc = lit_loc[cur_lit];
		 	    String lit_line = tz390.get_hex(list_obj_loc,6) + " " + list_obj_code.substring(0,16) + " =" + lit_name[cur_lit]; 
	        	put_prn_line(lit_line);
			}
		}
		cur_lit++;
	}
}
private void add_lit_xref(int index){
	/*
	 * add literal xref
	 */
	if (!tz390.opt_xref || !gen_obj_code){  //RPI165
		return;
	}
	if (lit_xref[index] == null){
		lit_xref[index] = new TreeSet<Integer>();
	}
	lit_xref[index].add(bal_line_num[bal_line_index]);
}
private int add_esd(int sid,byte sect_type){
	/*
	 * add new esd chained to sid 
	 * and return index else abort
	 */
	   if (tot_esd < max_esd){
		   tot_esd++;
		   esd_sid[tot_esd] = sid;
		   if (sect_type != sym_ent){
			   sym_esd[sid] = tot_esd;
			   sym_type[sid] = sect_type;
		   }
	   } else {
		   abort_error(96,"maximum esds exceeded");
		   return -1;
	   }
	   return tot_esd;
}
private int add_sym(String name){
	/*
	 * add symbol table entry name and return
	 * index for use in setting remaining fields
	 */
	   if (tot_sym < max_sym - 1){
		   tot_sym++;
		   sym_name[tot_sym] = name;
		   if (!tz390.add_key_index(tot_sym)){
			   return -1;
		   }
		   add_sym_xref(tot_sym);
		   return tot_sym;
	   } else {
		   abort_error(10,"maximum symbol table size exceeded");
		   return -1;
	   }
}
private void add_sym_xref(int index){
	/*
	 * add symbol xref
	 */
	if (!tz390.opt_xref 
		|| !gen_obj_code  //RPI165
		|| (last_xref_index   == index 
			&& last_xref_line == bal_line_index)){ 
		return;
	}
	last_xref_index = index;
	last_xref_line  = bal_line_index;
	if (sym_xref[index] == null){
		sym_xref[index] = new TreeSet<Integer>();
	}
	sym_xref[index].add(bal_line_num[bal_line_index]);
}
private String get_fp_hex(int fp_type,String fp_text){
	/*
	 * return hex for floating point string
	 * in scientific notation 0.314159E1 etc.
	 * format is based on fp type 1-6 (db,dh,eb,eh,lb,lh)
	 *
	 * Notes:
	 *   1.  This is very tricky code!
	 *   2.  Use BigDecimal for all 6 types to 
	 *       insure DH and EH exponents beyond 
	 *       range of DB and EB will be correctly
	 *       handled without error.
	 *   3.  The fp_context is set to significant
	 *       decimal digits plus 3 to insure 
	 *       sufficient significant bits for proper
	 *       rounding occurs.
	 * 
	 * First convert string constant to positive
	 * big_dec1 value with sufficent sig. bits.
	 * Exit with artbitrary format if zero.
	 */
	fp_context = new MathContext(fp_precision[fp_type]);
	fp_big_dec1 = new BigDecimal(fp_text,fp_context);
	if (fp_big_dec1.signum() > 0){
		fp_sign = 0;
	} else if (fp_big_dec1.signum() < 0){
		fp_sign = fp_sign_bit[fp_type];
		fp_big_dec1 = fp_big_dec1.abs();
	} else {
		switch (fp_type){  // gen zero hex for fp_type
		case 0: // fp_db_type s1,e11,m52 with assumed 1
			return "0000000000000000";
		case 1: // fp_dh_type s1,e7,m56 with hex exp
			return "4000000000000000";
		case 2: // fp_eb_type s1,e7,m24 with assumed 1
            return "00000000";
		case 3: // fp_eh_type s1,e7,m24 with hex exp
			return "40000000";
		case 4: // fp_lb_type s1,e15,m112 with assumed 1
			return "00000000000000000000000000000000";
		case 5: // fp_lh_type s1,e7,m112 with split hex	
			return "40000000000000004000000000000000";
		}
	}
	/*******************************************
	 * calc fp_exp and big_dec2 such that:      
	 * big_dec1 = big_dec2 * 2  ** fp_exp      
	 *************************************** 
	 * 
	 * fp_exp = log(big_dec1) / log(2)
	 * 	 *                                           
	 * Since the exponent range of LB exceeds  
	 * native double, the log of big_dec1 is
	 * calculated using equivalent:
	 *   log(X*10**N) = log(X) + N*log(10)
	 * The exponent must then be offset by the number
	 * of bits in the required binary mantissa in 
	 * order to retain significant bits when big_dec2
	 * is converted to big_int format.  The exponent
	 * is also reduced by 1 for assumed bit in binary 
	 * formats plus 1 additional to insure rounding for
	 * irrational values is done by shifting right.
	 * 
	 */ 
	int    work_scale  =  - fp_big_dec1.stripTrailingZeros().scale();
	double work_man    =    fp_big_dec1.multiply(
		BigDecimal.TEN.pow(-work_scale,fp_context),fp_context).doubleValue();
	fp_exp   =  (int)((Math.log(work_man) 
			           + ((double)work_scale 
			                * fp_log10))
			          / fp_log2) 
	         - fp_man_bits[fp_type] 
			 - fp_one_bit_adj[fp_type]; 
	/*
	 * Now calc big_dec2 mantissa truncated integer
	 * fp_exp calculated above.  This calculation
	 * may produce an irrational number with the 
	 * precison specified due to base 10 to base 2
	 * exponent conversion.
     *
	 * big_dec2 = big_dec1 / 2 ** fp_exp/
	 * 
	 */
	try {
	    fp_big_dec2 = fp_big_dec1.multiply(BigDecimal.valueOf(2).pow(-fp_exp,fp_context),fp_context);
	} catch (Exception e){
		log_error(89,"floating point value out of range");
		fp_hex = "FFFF000000000000";
	}
	/*
	 * retrieve fp_big_dec2 mantissa bits as big_int and
	 * adjust fp_exp by mantissa bits
	 */
	fp_big_int1 = fp_big_dec2.toBigInteger();
    fp_exp = fp_exp + fp_man_bits[fp_type];
	/*
	 * adjust mantiss and base 2 exponent to
	 * align for assumed 1 bit for IEEE binary
	 * or IBM base 16 hex exponent and return
	 * hex sign bit, exponent, and mantissa bytes
	 */
	switch (fp_type){  // gen hex for fp type
	case 0: // fp_db_type s1,e11,m52 with assumed 1
		fp_long1 = fp_big_int1.longValue();
		fp_round_bit = 0;
		while (fp_long1 > fp_long_db_one_bits){
			fp_round_bit = (int)(fp_long1 & 1);
			fp_long1 = fp_long1 >>> 1;
			fp_exp++;
			if (fp_long1 <= fp_long_db_one_bits){
				fp_long1 = fp_long1 + fp_round_bit;	
			}
		}
		fp_exp = fp_exp + fp_exp_bias[fp_type];
		if (fp_exp >= 0 && fp_exp <= fp_exp_max[fp_type]){
			fp_hex = get_long_hex( 
			         ((long)(fp_sign | fp_exp) 
			         		<< fp_man_bits[fp_type])
		              | (fp_long1 & fp_long_db_man_bits));
		} else {
			log_error(89,"floating point value out of range");
			fp_hex = "FFFF000000000000";
		}
        break;
	case 1: // fp_dh_type s1,e7,m56 with hex exp
		fp_long1 = fp_big_int1.longValue();
		fp_round_bit = 0;
		while (fp_long1 > fp_long_dh_man_bits
				|| (fp_exp & 0x3) != 0){
			fp_round_bit = (int)(fp_long1 & 1);
			fp_long1 = fp_long1 >>> 1;
			fp_exp++;
			if (fp_long1 <= fp_long_dh_man_bits){
				fp_long1 = fp_long1 + fp_round_bit;	
			}
		}
		fp_exp = (fp_exp >> 2) + fp_exp_bias[fp_type];
		if (fp_exp >= 0 && fp_exp <= fp_exp_max[fp_type]){
			fp_hex = get_long_hex( 
			         ((long)(fp_sign | fp_exp) 
			         		<< fp_man_bits[fp_type])
		              | fp_long1);
		} else {
			log_error(89,"floating point value out of range");
			fp_hex = "FFFF000000000000";
		}
		break;
	case 2: // fp_eb_type s1,e7,m24 with assumed 1
		fp_int1 = fp_big_int1.intValue();
		fp_round_bit = 0;
		while (fp_int1 >= fp_int_eb_one_bits){
			fp_round_bit = fp_int1 & 1;
			fp_int1 = fp_int1 >>> 1;
			fp_exp++;
			if (fp_int1 <= fp_int_eb_one_bits){
				fp_int1 = fp_int1 + fp_round_bit;	
			}
		}
		fp_exp = fp_exp + fp_exp_bias[fp_type];
		if (fp_exp >= 0 && fp_exp <= fp_exp_max[fp_type]){
			fp_hex = tz390.get_hex( 
			          ((fp_sign | fp_exp) 
			          		<< fp_man_bits[fp_type])
		              | (fp_int1 & fp_int_eb_man_bits),8);
		} else {
			log_error(89,"floating point value out of range");
			fp_hex = "FF000000";
		}
		break;
	case 3: // fp_eh_type s1,e7,m24 with hex exp
		fp_int1 = fp_big_int1.intValue();
		fp_round_bit = 0;
		while (fp_int1 > fp_int_eh_man_bits 
				|| (fp_exp & 0x3) != 0){
			fp_round_bit = fp_int1 & 1;
			fp_int1 = fp_int1 >>> 1;
			fp_exp++;
			if (fp_int1 <= fp_int_eh_man_bits){
				fp_int1 = fp_int1 + fp_round_bit;	
			}
		}
		fp_exp = (fp_exp >> 2) + fp_exp_bias[fp_type];
		if (fp_exp >= 0 && fp_exp <= 0x7f){
			fp_hex = tz390.get_hex( 
			          ((fp_sign | fp_exp) << 24)
		              | fp_int1,8);
		} else {
			log_error(89,"floating point value out of range");
			fp_hex = "00000000";
		}
	    break;
	case 4: // fp_lb_type s1,e15,m112 with assumed 1
		fp_round_bit = 0;
		while (fp_big_int1.compareTo(fp_big_int_one_bits) > 0){
			if (fp_big_int1.testBit(0)){
				fp_round_bit = 1;
			} else {
				fp_round_bit = 0;
			}
			fp_big_int1 = fp_big_int1.shiftRight(1);
			fp_exp++;
			if (fp_round_bit == 1 
				&& fp_big_int1.compareTo(fp_big_int_one_bits) <= 0){
				fp_big_int1 = fp_big_int1.add(BigInteger.ONE);
			}
		}
		fp_exp = fp_exp + fp_exp_bias[fp_type];
		if (fp_exp >= 0 && fp_exp <= fp_exp_max[fp_type]){
			fp_big_byte = fp_big_int1.toByteArray();
			int index1 = fp_big_byte.length - 1;
			int index2 = 15;
			while (index2 > 0){
				if (index1 >= 0){
					fp_data_byte[index2] = fp_big_byte[index1];
					index1--;
				} else {
					fp_data_byte[index2] = 0;
				}
				index2--;
			}
			fp_data_buff.putShort(0,(short)(fp_sign | fp_exp));
            fp_hex = bytes_to_hex(fp_data_byte,0,16,0);
		} else {
			log_error(89,"floating point value out of range");
			fp_hex = "FF00000000000000FF00000000000000";
		}
	    break;
	case 5: // fp_lh_type s1,e7,m112 with split hex
        fp_round_bit = 0;
		while (fp_big_int1.compareTo(fp_big_int_man_bits) > 0
				|| (fp_exp & 0x3) != 0){
			if (fp_big_int1.testBit(0)){
				fp_round_bit = 1;
			} else {
				fp_round_bit = 0;
			}
			fp_big_int1 = fp_big_int1.shiftRight(1);
			fp_exp++;
			if (fp_round_bit == 1 
				&& fp_big_int1.compareTo(fp_big_int_man_bits) <= 0){
				fp_big_int1 = fp_big_int1.add(BigInteger.ONE);
			}
		}
		fp_exp = (fp_exp >> 2) + fp_exp_bias[fp_type];
		if (fp_exp >= 0 && fp_exp <= fp_exp_max[fp_type]){
			fp_big_byte = fp_big_int1.toByteArray();
			int index1 = fp_big_byte.length - 1;
			int index2 = 15;
			while (index2 > 0){
				if (index2 == 8){
					index2--;  // skip dup exp byte
			    }
				if (index1 >= 0){
					fp_data_byte[index2] = fp_big_byte[index1];
					index1--;
				} else {
					fp_data_byte[index2] = 0;
				}
				index2--;
			}
			fp_data_buff.put(0,(byte)(fp_sign | fp_exp));
			if ((fp_data_buff.getLong(8) & fp_long_dh_man_bits) == 0){
				fp_data_buff.put(8,(byte)0x40);
			} else {
				fp_data_buff.put(8,(byte)(fp_sign | (fp_exp - 14)));
			}
            fp_hex = bytes_to_hex(fp_data_byte,0,16,0);
		} else {
			log_error(89,"floating point value out of range");
			fp_hex = "FF00000000000000FF00000000000000";
		}
	    break;
	}
	return fp_hex;
}
/*
 *  end of az390 code 
 */
}