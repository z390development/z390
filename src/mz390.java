import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.lang.reflect.Array;
import java.text.SimpleDateFormat;
import java.util.Arrays;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.Random;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.swing.JTextArea;

public  class  mz390 {
	/*****************************************************
	 
	 z390 portable mainframe assembler and emulator.
	 
	 Copyright 2006 Automated Software Tools Corporation
	 
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
	 * 04/06/06 RPI 259 add remaining &SYS variables for compatiblity
	 * 04/07/06 RPI 274 correct exp_pat for C'..' in quotes
	 *          and correct ?' parsing within string.  Also 
	 *          correct precedence for (' operator from 8 to 3
	 *          correct variable substitution on OPSYN
	 *          correct support for macro labels on nested MEND
	 * 04/09/06 RPI 276 execute SYSINIT macro if found at startup
	 * 04/12/06 RPI 280 fix test and increase max_sym to 50,000
	 * 04/17/06 RPI 284 add init_arrays using tz390.opt_max????
	 * 04/18/06 RPI 241 do not substitute &var in comments
	 * 04/25/06 RPI 293 parse EXEC SQL/CICS/DPI into macro form
	 * 04/27/06 RPI 300 expand COPY statements in macros
	 * 04/30/06 RPI 306 update OPSYN for opcodes and macros
	 * 05/05/06 RPI 308 support CICS PROLOG and EPILOG inserts
	 * 05/09/06 RPI 310 issue error for stmt before MACRO
	 * 05/09/06 RPI 312 add pgm name to return code msg
	 * 05/11/06 RPI 313 correct handling of ?' type operators
	 *          appearing in comma delimited contination lines
	 *          and return no pos parms for " , " parm
	 * 05/13/06 RPI 314 add AGOB and AIFB
	 * 05/15/06 RPI 315 add option REFORMAT default false  
	 * 06/04/06 RPI 331 expand macro for cancelled OPSYN 
	 * 06/06/06 RPI 336 correct parsing for D' operator 
	 * 06/08/06 RPI 338 add support for string dup (..)'..' 
	 * 06/08/06 RPI 329 remove file suffix from &SYS.._MEMBER 
	 * 06/09/06 RPI 330 add MNOTE's with level > 0 to ERR log
	 * 06/09/06 RPI 343 support N'&array returning highest store  
	 * 06/16/06 RPI 340 multiple fixes for EQU symbol support
	 * 06/16/06 RPI 349 fix to prevent loop on EQU error during loading
	 * 07/01/06 RPI 351 fix opsyn cancel for previously used opcode
	 * 07/03/06 RPI 353 fix UPPER(...) expression error
	 * 07/08/06 RPI 359 fix support for mixed case macro parms
	 * 07/13/06 RPI 366 support mixed case keyword override values
	 * 07/14/06 RPI 369 skip null 3rd EQU parm and ignore 4th and 5th
	 * 08/07/06 RPI 401 expand array for mult values, fix gbl stats
	 * 08/09/06 RPI 404 add remaining missing operators in HLASM V5
	 *  A2B - convert value to binary string (3 = '11')
	 *  A2C - convert value to character string (240 = '1')
	 *  A2D-  convert value to decimal string (1 = '1')
	 *  A2X - convert value to hex string (240 = 'F0')
	 *  AND - logical and (NC)
	 *  B2A - convert binary string to value (B2A('100') = 4)
	 *  B2C - convert binary string to character string ('11110000' = '1')
	 *  B2D-  convert binary string to decimal string ('100'  = '4')
	 *  B2X - convert binary string to hex string ('11110000' = 'F0')
	 *  C2A - convert 0-4 characters to value (C2A('0') = 240)
	 *  C2B - convert character string to binary string ('1' = '11110000')
	 *  C2D-  convert character string to decimal string ('1'  = '240')
	 *  C2X - convert character string to hex string ('1' = 'F0')
	 *  D2A - convert decimal string to value (D2A('-2') = -2
	 *  D2B - convert decimal string to binary string ('4' = '100')
	 *  D2C-  convert decimal string to character string('240'  = '1')
	 *  D2X - convert decimal string to hex string ('240' = 'F0')
	 *  DCLEN - length of string after reducing double ' and &
	 *  DCVAL - return string with double ' and & reduced
	 *  DCEQUOTE - return string without first and last ' if any
	 *  DOUBLE - double quotes and & in string (NC)
	 *  FIND - return index of any char in string2 found in string1 (NC)
	 *  INDEX - return index of string2 found in string1 else 0 (NC)
	 *  ISBIN - return 1 if valid binary string else 0
	 *  ISDEC - return 1 if valid decimal string else 0 
	 *  ISHEX - return 1 if valid hex string else 0
	 *  ISSYM - return 1 if valid character string for symbol else 0
	 *  LOWER - return lower case string (NC)
	 *  NOT - logical or arithmetic not (NC)
	 *  OR - logical or (NC)
	 *  UPPER - return upper case string (NC)
	 *  SIGNED - return decimal string with minus sign if negative
	 *  SLA - shift left arithmetic (2 SLA 1 = 4)
	 *  SLL - shift left logical (2 SLL 1 = 4)
	 *  SRA - shift right arithmetic (4 SRA 1 = 2)
	 *  SRL - shift right logical (4 SRL 1 = 2)
	 *  SYSATTRA - return assembler attribute for symbol (EQU 4th)
	 *  SYSATTRP - return program attribute for symbol (EQU 5th)
	 *  X2A = convert hex string to value (X2A('F0') = 240)  
	 *  X2B - convert hex string to binary string ('F0' = '11110000')
	 *  X2C-  convert hex string to character string('F0'  = '1')
	 *  X2D - convert hex string to decimal string ('F0' = '240')
	 *  XOR - logical exclusive or (NC) 
	 * 08/09/06 RPI 405 add O'opcode support returning A,E,M,O,S,or U
	 * 08/13/06 RPI 410 fix AREAD to only return "" for eof, add stats
	 * 08/14/06 RPI 414 recognize ERR(nnn) limit override 
	 * 08/15/06 RPI 415 merge mz390 and az390 for MFC
	 * 08/28/06 RPI 411 replace while loops with arraycopy
	 *          and Array.fill functions.
	 * 08/29/06 RPI 421 support string duplication in exp.
	 * 08/30/06 RPI 420 support ID=0-9 for mult AREAD/PUNCH files
	 *          and allow DSN= alais for DSNAME=   
	 * 08/30/06 RPI 422 fix setc compare padded with unequal EBCDIC > 128
	 * 09/01/06 RPI 425 pass (file/line) xref to az390 for errors and
	 *          generate stats and error file xref before END. 
	 * 09/02/06 RPI 427 issue error on sym ref and option NOASM              
	 ********************************************************
	 * Global variables
	 *****************************************************/
	tz390 tz390 = null;
	az390 az390 = null;  // RPI 415
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
	boolean bal_eof = false;
	int tot_mnote_errors  = 0;
	int tot_mnote_warning = 0;
	int max_mnote_level   = 0;
	String bal_text = null;     // curr bal_line text
	int    bal_text_index0 = 0; // end of prev. parm
	int    bal_text_index1 = 0; // start of cur parm
	int    bal_text_index2 = 0; // end   of cur parm
	int tot_mac_ins  = 0;
	int tot_mac_load = 0;
	int tot_mac_call = 0;
	int mlc_line_end = 0;
	File bal_file = null;
	BufferedWriter bal_file_buff = null;
	int tot_aread_io = 0;
	int tot_punch_io = 0;
	int max_ap_files = 10; // max concurrent AREAD and PUNCH files
	int ap_file_index = 0;
	String ap_file_name = null;
	int dat_file_index = 0;
	int pch_file_index = 0;
	File[] dat_file = new File[max_ap_files];
	BufferedReader[] dat_file_buff = new BufferedReader[max_ap_files];
	File[]   pch_file = new File[max_ap_files];
	BufferedWriter[] pch_file_buff = new BufferedWriter[max_ap_files];
	String bal_line = null;
	String bal_label = null;
	String bal_op = null;
	String   save_bal_op = null; // original bal_op
	int      save_opsyn_index = -1; // opsyn index of orig. bal_op
	boolean aif_op = false;
	boolean bal_op_ok = false;
	String bal_parms = null;
	boolean mlc_eof = false;
	boolean end_found = false;
	int actr_limit = 4096;
	int     actr_count = actr_limit;
	SimpleDateFormat sdf_sysclock = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss.SSS000");
	SimpleDateFormat sdf_sysdatc  = new SimpleDateFormat("yyyyMMdd");
	SimpleDateFormat sdf_sysdate  = new SimpleDateFormat("MM/dd/yy");
	SimpleDateFormat sdf_systime = new SimpleDateFormat("HH.mm");
	boolean log_tod = true; 
	JTextArea z390_log_text = null;
	/*
	 * static limits
	 */
	int max_exp_stk = 500;
	String max_substring_len = "100000";
	/*
	 * macro execution global data
	 */
	long    tod_time_limit = 0;
	int     next_time_ins   = 0x1000;
	int     next_time_check = next_time_ins;
	String cur_mac_file_name = null;
	int cur_mac_file = 0;
	File[] mac_file                = null;
	BufferedReader[] mac_file_buff = null;
	int[]        mac_file_cur_file_num = null; 
	int[]        mac_file_cur_line_num = null;
	int[]        mac_file_errors = null;
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
	boolean cics_first_dsa_dsect = false; // cics prolog change to DFHEISTG macro 
	boolean cics_first_csect     = false; // cics prolog change to DFHEIENT macro
	boolean cics_first_end       = false; // cics epilog change to DFHEIEND macro
	/*
	 * macro name table
	 */
	int mac_name_index = 0; //current mlc or macro
	int mac_last_find_index = -1;
	int tot_ins = 0; // count instr/cntl for MFC option
	int tot_mac_name = 0;      // next avail name
	int load_macro_mend_level = 0;
	boolean loading_mac = false;
	boolean load_proto_type = false;
	int load_mac_inline_end = 0;
	int find_mac_name_index = 0;
	int load_mac_name_index = 0;
	byte     load_type       = 0;
	byte     load_mlc_file   = 0; // no MACRO, no prototype, stop on END
	byte     load_mac_file   = 1; // MACRO, prototype, MEND, read file and verify name = prototype
	byte     load_mac_inline = 2; // MACRO, prototype, MEND, read in memory source
	byte     load_mac_exec   = 3; // executing mlc/mac code
	int      load_proto_index = 0; // line index of proto_type statement
	String   load_macro_name = null;
	String   load_file_name = null;
	String[] mac_name            = null;
	int[]    mac_name_line_start = null;
	int[]    mac_name_line_end   = null;
	int[]    mac_name_lab_start  = null;
	int[]    mac_name_lab_end    = null;
	/*
	 * macro bal line tables
	 */
	int old_mac_line_index = 0; //prev    mac_line_index
	int mac_line_index = 0;     //current mac line index
	int bal_xref_index = 0;     // last mac line ref to pass to az390
	int tot_mac_line = 0;       // next avail line
	String[] mac_file_line     = null;
	int[]    mac_file_line_num = null;
	int[]    mac_file_name_num = null;
	int tot_mac_file_name = 0;
	String[] mac_file_name = null;
	/*
	 * macro labels for loaded mlc and macros
	 */
	int tot_mac_lab = 0;
	String[] mac_lab_name  = null; 
	int[]    mac_lab_index = null;
	int[]    mac_lab_num   = null; // RPI 266
	/*
	 * macro call stack variables
	 */
	int tot_expand = 0;
	int expand_inc = 10;         //macro array expansion RPI 401
	int mac_call_level = 0;      //level of macro nesting during loading
	int[]    mac_call_name_index = null;
	int[]    mac_call_return     = null;
	int[]    mac_call_actr       = null;
	int[]    mac_call_pos_start  = null;
	int[]    mac_call_pos_tot    = null;
	int[]    mac_call_kwd_start  = null;
	int[]    mac_call_lcl_name_start = null;
	int[]    mac_call_lcl_seta_start = null;
	int[]    mac_call_lcl_setb_start = null;
	int[]    mac_call_lcl_setc_start = null;
	int[]    mac_call_lcl_key_start  = null;
	int[]    mac_call_lcl_key_root   = null;
	/*
	 * macro positional and key word global variables
	 */
	Pattern var_pattern = null;
	Matcher var_match   = null;
	Pattern proto_pattern = null;
	Matcher proto_match   = null;
	Pattern exp_pattern = null;
	Matcher exp_match   = null;
	Pattern pch_pattern = null;
	Matcher pch_match   = null;
	Pattern label_pattern = null;
	Matcher label_match   = null;
	Pattern symbol_pattern = null; // RPI 404
	Matcher symbol_match = null;
	Pattern exec_pattern = null;
	Matcher exec_match   = null;
	int label_comma_index = 0; // parm into to comma after macro label else -1
	int sublist_index = 0;
	int sublist_count = 0;
	int tot_pos_parm = 0; // cur pos parms on stack
	int cur_pos_parm = 0; // cur pos parm during init (may exceed prototype pos)
	int tot_kwd_parm = 0; // cur kwd parms on stack
	int hwm_pos_parm = 0;      // tot pos parms defined
	int hwm_kwd_parm = 0;      // tot kwd parms defined
	String[] mac_call_pos_name = null; 
	String[] mac_call_pos_parm = null; 
	String[] mac_call_kwd_name = null; 
	String[] mac_call_kwd_parm = null; 
	/*
	 * global and local macro variables
	 */
	boolean var_subscript_calc = false;
	byte var_seta_type = 1;      // loc= lcl or gbl
	byte var_setb_type = 2;      // loc= lcl or gbl
	byte var_setc_type = 3;      // loc= lcl or gbl
	byte var_parm_type = 4;      // loc= pos, kw, sys 
	byte var_subscript_type = 5; // stk_setb=loc, stk_seta=name_index
	byte var_sublist_type = 6;   // stk_setb=loc, stk_seta=sublist_index, stk_setc=cur sublist
	byte lcl_loc = 11;     // lcl_seta, lcl_setb, lcl_setc
	byte gbl_loc = 12;     // gbl_seta, gbl_setb, gbl_setc
	byte pos_loc = 13;     // named positional parm
	byte kw_loc  = 14;     // named keyword parm
	byte syslist_loc = 15; // syslist pos parm ref
	int tot_lcl_name = 0; // cur lcl sets on stack
	int tot_lcl_seta = 0;
	int tot_lcl_setb = 0;
	int tot_lcl_setc = 0;
	int hwm_lcl_name = 0; // high water mark lcl names
	int hwm_lcl_seta = 0;
	int hwm_lcl_setb = 0;
	int hwm_lcl_setc = 0;
	boolean sysinit_done = false;
	int lcl_sysndx = -1;  // macro call counter
	String lcl_sysect = "$$CSECT";
	String lcl_sysloc = lcl_sysect;
	String lcl_sysstyp = "";
	String[] lcl_set_name  = null; 
	byte[]   lcl_set_type  = null;
	int[]    lcl_set_start = null;
	int[]    lcl_set_high  = null;
	int[]    lcl_set_end   = null;
	int[]    lcl_seta      = null;
	byte[]   lcl_setb      = null;
	String[] lcl_setc      = null; 
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
	String store_name = null;
	int    store_name_index = 0;
	byte   store_loc   = lcl_loc;
	byte   store_type = var_seta_type;
	int    store_sub        = 0;
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
	int    gbl_sysclock_index = 0; // YYYY-MM-DD HH:MM:SS.mmmmmm
	int    gbl_sysmac_index = 0;   // macro name at specified level
	String gbl_sysmac  = "";
	int    gbl_syslib_index = 0;  // syslib current macro dsn, mem=+1, vol=+2
	int    gbl_sysm_hsev_index = 0;  // highest mnote severity code      
	int    gbl_sysm_sev_index = 0;   // highest mnote severity in last macro     
	int    gbl_sysstmt_index = 0;    // next MLC statement number
	File sys_file = null; // RPI 259
	String sys_job = null; // set to MLC filename without suffix
	String sys_dsn = null; // full path, file name, and suffix
	String sys_mem = null; // file name and suffix
	String sys_vol = null; // drive letter
	int tot_gbl_name = 0;
	int tot_gbl_seta = 0;
	int tot_gbl_setb = 0;
	int tot_gbl_setc = 0;
	String[] gbl_set_name  = null; 
	byte[]   gbl_set_type  = null;
	int[]    gbl_set_start = null;
	int[]    gbl_set_high  = null; // RPI 342 highest subscript 
	int[]    gbl_set_end   = null;
	int[]    gbl_seta      = null;
	byte[]   gbl_setb      = null;
	String[] gbl_setc      = null;
	/*
	 * macro operation global variables
	 */
	int mac_op_type = 0;
	int max_lcl_key_root = 47;
	int max_lcl_key_tab = 0;
	int tot_lcl_key_tab  = max_lcl_key_root+1;
	int cur_lcl_key_root = 1;
	String lcl_key_text = null;
	int lcl_key_index = 0;
	int lcl_key_index_last = 0;
	Random lcl_key_rand = new Random();
	int lcl_key_hash = 0;
	String[]  lcl_key_tab_key   = null;
	int[]     lcl_key_tab_hash  = null;
	int[]     lcl_key_tab_index = null;
	int[]     lcl_key_tab_low   = null;
	int[]     lcl_key_tab_high  = null;
	
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
	boolean exp_alloc_set_mode = false;  // for lcl/gbl alloc
	boolean exp_parse_set_mode = false;  // for set target and lcl/gbl alloc
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
	int exp_first_sym_index = -1;
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
	byte exp_class_add_sub = 1;
	byte exp_class_mpy_div = 2;
	byte exp_class_open    = 3;
	byte exp_class_cls_sub = 4;
	byte exp_class_str_op  = 5; // 2 operand string ops (".", FIND, INDEX etc.)
	byte exp_class_term    = 6;
	byte exp_class_comp    = 7;
	byte exp_class_str_sub1= 8;
	byte exp_class_str_sub2= 9;
	byte exp_class_oper    = 10; // prefix operators (?',?2?, DOUBLE, LOWER, UPPER, etc.)
	byte exp_class_not     = 11;
	byte exp_class_and     = 12;
	byte exp_class_or      = 13;
	byte exp_class_xor     = 14;
	byte exp_class_create_set = 15;
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
			3, 3, 3, 4, 3, 0, 3, 3, 0, 3, 3, 3, 3, 3, 3, // 3 (   prev open (...) RPI 274
			3, 3, 3,11, 0, 0, 0, 8,11, 0, 3, 3, 3, 3, 0, // 4 )   prev var subscript
			0, 0, 3, 5, 5, 5, 5, 8, 0, 3, 0, 0, 0, 0, 3, // 5 .   prev concat
			3, 3, 3, 6, 3, 6, 3, 8, 3, 3, 3, 3, 3, 3, 3, // 6 ~   prev terminator
			3, 3, 3, 7, 3, 7, 7, 8, 0, 3, 7, 7, 7, 7, 3, // 7 LL  logical compares //RPI144 (was 1,2 now 3,3)
			3, 3, 3, 0, 0, 0, 0, 8, 0, 3, 0, 0, 0, 0, 3, // 8 '   string '....'
			3, 3, 3,10, 0, 0, 0, 0, 9, 3, 0, 0, 0, 0, 3, // 9 ,   substring '...'(e1,e2)
			12,12, 3,12,12,12,12, 3,12,12,12,12,12,12, 3, //10 ?'  prefix operator  //RPI145, RPI196, RPI 353
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
	public void process_mz390(String[] args,JTextArea log_text){
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
		init_lcl_sys();
		if (tz390.opt_trap){
			try {
				process_mac();
			} catch (Exception e){
				abort_error(84,"mz390 internal system exception - " + e.toString());
			}
		} else {
			process_mac();
		}
		exit_mz390();
	}
	private void init_mz390(String[] args, JTextArea log_text){
		/*
		 * 1.  initialize log routing
		 * 2.  set options
		 * 3.  open MLC and BAL buffered I/O files
		 */
		tz390 = new tz390();
		tz390.init_tables();
		tz390.init_options(args,tz390.mlc_type);  
		tz390.open_systerm("MZ390");
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
		if (tz390.opt_asm){
			az390 = new az390(); // RPI 415
			az390.start_az390_thread(args,z390_log_text,tz390.systerm_file);
		}
		open_files();
		compile_patterns();
		tod_time_limit = tz390.max_time_seconds * 1000 + tod_start;
		put_copyright();
		init_arrays();
		mac_name[0] = "OPEN CODE"; // for trace
		init_gbl_sys();
	}
	private void compile_patterns(){
		/*
		 * init regular expression patterns
		 * and issue error if failure
		 * 
		 */
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
					+ "|([dD]['])"                       // D' defined symbol test 0 or 1  RPI 336
					+ "|([kK]['])"                       // K' character length of var
					+ "|([lL]['])"                       // L' length attribute of symbol
					+ "|([nN]['])"                       // N' number of parm subparms (n1,n2,n3)
					+ "|([oO]['])"                       // O' opcode type A,E,M,O,S, or U RPI 405                 
					+ "|([tT]['])"                       // T' type attribute of symbol
					+ "|([bB]['][0|1]+['])"              // B'0110' binary self def. term
					+	"|([cC][aAeE]*[']([^']|(['][']))+['])"    // C'ABCD' ebcdic or ascii self def. term // RPI 270, 274
					+	"|([cC][\"]([^\"]|([\"][\"]))+[\"])"    // C"ABCD" ascii self def. term   RPI73, 274
					+	"|([cC][!]([^!]|([!][!]))+[!])"        // C"ABCD" ebcdic self def. term  RPI84, 274
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
		 * assembler symbol_pattern A-Z09#$%_
		 */
		try {
			symbol_pattern = Pattern.compile(
					"([a-zA-Z$@#_][a-zA-Z0-9$@#_]*)"  // RPI 253         
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
					+ "|([dD]['])"                            // D' defined symbol test 0 or 1  RPI 336
					+ "|([kK]['])"                            // K' character length of var
					+ "|([lL]['])"                            // L' length attribute of symbol
					+ "|([nN]['])"                            // N' number of parm subparms (n1,n2,n3)
					+ "|([oO]['])"                       // O' opcode type A,E,M,O,S, or U RPI 405                
					+ "|([tT]['])"                            // T' type attribute of symbol
					+ "|([bB]['][0|1]+['])"                   // B'0110' binary self def. term
					+	"|([cC][aAeE]*[']([^']|(['][']))+['])"         // C'ABCD' ebcdic or ascii self def. term // RPI 270, 274
					+	"|([cC][\"]([^\"]|([\"][\"]))+[\"])"    // C"ABCD" ascii self def. term   RPI73, 274
					+	"|([cC][!]([^!]|([!][!]))+[!])"         // C"ABCD" ebcdic self def. term  RPI84, 274
					+	"|([xX]['][0-9a-fA-F]+['])"             // X'0F'   hex self defining term
					+ "|([a-zA-Z$@#_][a-zA-Z0-9$@#_]*)"       // symbol or logical operator (AND, OR, XOR, NOT, GT, LT etc.)
					+ "|([^'&]+)"  // RPI 268                            // string text
			);
		} catch (Exception e){
			abort_error(4,"expression pattern errror - " + e.toString());
		}
		/*
		 * macro label_pattern  .lll
		 */
		try {
			exec_pattern = Pattern.compile( // RPI 293
					"([;,])"
					+	"|([^\\s]+)"           
			);
		} catch (Exception e){
			abort_error(147,"exec pattern errror - " + e.toString());
		}
	}
	private void open_files(){
		/*
		 * open BAL file if option BAL
		 */
		if (!tz390.opt_bal){
			return;
		}
		bal_file = new File(tz390.dir_bal + tz390.pgm_name + tz390.bal_type);
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
					put_trace("MACRO CALL END " + mac_name[mac_call_name_index[mac_call_level]]);
				}
				if  (tz390.opt_listcall){
					if (mac_call_level > 0){
						String sysndx = "    " + get_set_string("&SYSNDX",1);
						sysndx = sysndx.substring(sysndx.length() - 4);				 	     
						String sysnest = "  " + mac_call_level;
						sysnest = sysnest.substring(sysnest.length() - 2);
						put_bal_line("*MEXIT #=" + sysndx + " LV=" + sysnest + " " + mac_name[mac_call_name_index[mac_call_level]]);
					}
				}
				mac_call_level--;
				bal_line = null;
				if (mac_call_level >= 0){
					mac_name_index = mac_call_name_index[mac_call_level];
					mac_line_index = mac_call_return[mac_call_level];
					update_sysstmt();
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
					if (tz390.opt_asm){ // RPI 415
						if (tz390.opt_bal && !end_found){
							put_stats(); // RPI 425
						}
    					az390.reset_sym_lock();
                        call_az390_pass_bal_line(bal_line);    
					}
				}
			} else {
				bal_line = mac_file_line[mac_line_index];
				bal_xref_index = mac_line_index;
				parse_bal_line();
				mac_op_type = find_opcode_type(bal_op);
				if (mac_op_type > tz390.max_asm_type){  // RPI 274 -2 for OPSYN cancel
					exec_mac_op();      // execute macro operation
					bal_line = null;    // force macro execution cycle
				} else if (bal_op != null) {
					if (save_opsyn_index == -1 
							|| tz390.opsyn_old_name[save_opsyn_index] == null
							|| !tz390.opt_asm
							|| tz390.find_key_index('O',bal_op) < 0
					    ){
						find_mac_name_index = find_mac_entry(bal_op);
						if (find_mac_name_index == -2  // RPI 351
								&& save_opsyn_index >= 0
								&& tz390.opsyn_old_name[save_opsyn_index] == null){
							find_mac_name_index = -1; // search again for opsyn cancel
						}
					} else { 
						find_mac_name_index = -2; // RPI 331 don't search for opsyn rep.
					}
					if (find_mac_name_index == -1){
						find_and_load_mac_file();
					}
					if (find_mac_name_index >= 0){
						call_mac();      // call a nested macro
						bal_line = null; // force macro exeuction cycle
					}
				}		   
				mac_line_index++;
				update_sysstmt();
			}
			if   (bal_line != null){
				if (tz390.opt_tracem){
					put_trace("BAL OUTPUT - " + bal_line);
				}
				if (tz390.opt_asm){
					if (bal_op != null && bal_op.equals("END")){
						if (tz390.opt_asm && tz390.opt_bal){
							end_found = true;
							put_stats(); // RPI 425
						}
					}
					az390.reset_sym_lock();
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
	private void find_and_load_mac_file(){
		/*
		 * 1.  find macro file on sysmac
		 * 2.  load it if found
		 * 3.  add macro entry or dummy entry
		 */
		cur_mac_file_name = tz390.find_file_name(tz390.dir_mac,bal_op,tz390.mac_type,tz390.dir_cur);
		if (cur_mac_file_name != null){
			load_type = load_mac_file;
			load_file_name = cur_mac_file_name;
			load_mac();
			find_mac_name_index = load_mac_name_index;
		} else { // add dummy mac to prevent search
			tz390.find_key_index('M',bal_op);
			if (!tz390.add_key_index(-2)){
				abort_error(119,"macro index table exceeded");
			}
			find_mac_name_index = -2;
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
		 *   parse EXEC statement with space delimited
		 *   parms and create exec macro parms
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
		 * 8.  Insert MLC copy profile copybook if option 
		 *     PROFILE(copybook) specified.
		 *
		 * Notes:
		 *   1.  At end of MLC load, turn off
		 *       lookahead mode for ordinary symbols.
		 */
		loading_mac = true;
		tot_mac_load++;
		cur_mac_file = 0;
		load_macro_mend_level = 0;
		load_proto_index = 0;
		load_macro_name = "";
		int save_mac_name_index = mac_name_index;
		int save_mac_line_index = mac_line_index;
		switch (load_type){
		case 0: // MLC
			cur_mac_line_num = 0;
			load_open_macro_file();
			load_macro_mend_level = 1; // no macro statement
			mac_line_index = tot_mac_line;
			if (tz390.opt_cics && tz390.opt_prolog){  // RPI 308
				mac_line = " DFHEIGBL";
				mac_file_line[mac_line_index] = mac_line;
				mac_line_index++;
			}
			if (tz390.opt_profile.length() > 0){
				mac_line = " COPY " + tz390.opt_profile;
				mac_file_line[mac_line_index] = mac_line;
				mac_line_index++;
				parse_mac_line();  // open copy
				if (tz390.opt_tracem){
					put_trace("PROFILE " + mac_line);
				}
			}
			load_proto_type = true;
			break;
		case 1: // macro file
			cur_mac_line_num = 0;
			load_open_macro_file();
			load_macro_mend_level = 0; // read macro from file
			mac_line_index = tot_mac_line;
			load_proto_type = false;
			break;
		case 2: // macro inline
			mac_line_index++; // skip macro statement
			update_sysstmt();
			cur_mac_line_num = mac_file_line_num[mac_line_index];
			if (tz390.opt_tracem){
				put_trace("LOADING INLINE MACRO");
			}
			load_macro_mend_level = 1; // macro statment read inline
			load_mac_inline_end = mac_name_line_end[mac_name_index];
			load_proto_type = false;
			break;
		}
		load_get_mac_line();
		while (mac_line != null
				&& mac_line_index < tz390.opt_maxline){
			if (tz390.opt_traceall){
				put_trace("LOADING MAC LINE " + mac_line);
			}
			mac_abort = false;  // RPI 412
			parse_mac_line();
			if (load_type != load_mac_inline
					&& (mac_line.length() < 2
							|| !mac_line.substring(0,2).equals(".*"))
			){
				store_mac_line(); // RPI 273 update now for any cont. error
			}
			if (mac_op != null && mac_op.length() > 0){
				if (mac_op.equals("MACRO")){
					load_macro_mend_level++;
				} else {
					if (load_macro_mend_level == 1){
						if (!load_proto_type){
							load_proto_type();
						} else {
							if (mac_op.equals("EXEC")){
								load_macro_exec();
							}
							load_macro_ago_aif_refs();
							load_macro_label_sym();
						}
					} else if (load_macro_mend_level == 0){
						log_error(148,"macro statment preceeding MACRO " + mac_line);
					}
					if (mac_op.equals("MEND")){
						load_macro_mend_level--;
					}
					if (load_macro_mend_level == 0
							&& load_type != load_mlc_file){
						mac_line = null; // eof at level 0 for macro
					}
				}
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
					update_sysstmt();
				}
				load_get_mac_line();
			}
		}
		if (mac_line_index >= tz390.opt_maxline){
			abort_error(87,"maximum source lines exceeded");
		}
		switch (load_type){
		case 0: // MLC file 
			if (tz390.opt_asm && az390.lookahead){
				az390.lookahead = false;
				az390.cur_esd = 0;
				az390.cur_esd_sid = -1;
			}
			if (load_macro_mend_level != 1){
				log_error(133,"unbalanced macro mend in " + load_macro_name);
			}
			tot_mac_line = mac_line_index;
			mac_name_line_end[mac_name_index] = tot_mac_line;
			mac_line_index = save_mac_line_index;
			update_sysstmt();
			break;
		case 1: // macro file
			if (load_macro_mend_level != 0){
				log_error(134,"unbalanced macro mend in " + load_macro_name);
			}
			tot_mac_line = mac_line_index;
			mac_name_line_end[mac_name_index] = tot_mac_line;
			mac_line_index = save_mac_line_index;
			update_sysstmt();
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
		loading_mac = false;
	}
	private void load_open_macro_file(){
		/*
		 * open file for MLC or macro file
		 * else abort with error
		 */	
		mac_file[cur_mac_file] = new File(load_file_name);
		if (!mac_file[cur_mac_file].isFile()){
			abort_error(39,"file not found - " + load_file_name); //RPI169
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
			put_trace("LOADING FILE " + load_file_name);
		}
	}
	private void load_proto_type(){
		/* 
		 * process proto-type
		 * during loading of MLC or macro
		 */
		load_proto_type = true;
		load_proto_index = mac_line_index;
		if (load_type == load_mac_file){
			mac_name_line_start[mac_name_index] = mac_line_index; // RPI 331 
			if (!mac_op.equals(load_macro_name)){
				log_error(132,"macro proto-type name " + mac_op + " not = file name " + load_macro_name);
			}
		} else {  // define inline macro
			load_macro_name = mac_op;
			load_proto_index = mac_line_index;
			mac_name_index = find_mac_entry(load_macro_name);
			if (mac_name_index < 0){
				if (tot_mac_name < tz390.opt_maxsym){
					mac_name_index = tot_mac_name;
					update_mac_key_index(mac_name_index,mac_op);
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
	private void load_macro_exec(){
		/*
		 * parse space delimited parms for exec
		 * sql cics or dli and replace with comma
		 * delimited parms for EXEC macro processing.
		 */
		exec_match = exec_pattern.matcher(mac_parms);
		String exec_parms = "";
		boolean exec_eof = false;
		while (!exec_eof 
				&& exec_match.find()){
			String exec_parm = exec_match.group();
			switch (exec_parm.charAt(0)){
			case ',': // ignore
				break;
			case ';': // ignore following comments
				exec_eof = true;
				break;
			default: // assume exec statement parm
				exec_parms = exec_parms.concat("," + exec_parm);
			break;
			}
		}
		if (exec_parms.length() > 0){
			exec_parms = exec_parms.substring(1);
		}
		mac_file_line[mac_line_index] = " EXEC " + exec_parms;
	}
	private void load_macro_ago_aif_refs(){
		/*
		 * check ago and aif references during loading
		 */
		int lab_index = 0;
		if (load_macro_mend_level == 1 
				&& (mac_op.equals("AGO") 
						|| mac_op.equals("AGOB"))){
			label_match = label_pattern.matcher(mac_parms);
			if (label_match.find()){
				add_mac_label(mac_name_index
						,label_match.group().toUpperCase()
						,-mac_line_index);
				if (mac_parms.charAt(0) == '('){
					while (label_match.end() < mac_parms.length()
							&& mac_parms.charAt(label_match.end()) == ','){ 
						lab_index = label_match.end()+1;
						if (label_match.find()){
							add_mac_label(mac_name_index
									,label_match.group().toUpperCase()
									,-mac_line_index);
						} else {
							log_error(151," invalid AGO label - " + mac_parms.substring(lab_index));
						}
					}
				}
			} else {
				log_error(112,mac_name[mac_name_index] + " invalid AGO label - " + mac_parms);
			}
		} else if (load_macro_mend_level == 1 
				&& (mac_op.equals("AIF") 
						|| mac_op.equals("AIFB"))){
			int aif_test_index = 0;
			while (aif_test_index >= 0){
				lab_index = mac_parms.substring(aif_test_index).indexOf(").");
				if (lab_index > 0){
					label_match = label_pattern.matcher(mac_parms.substring(aif_test_index+lab_index+1));
					if (label_match.find()){
						add_mac_label(mac_name_index
								,label_match.group().toUpperCase()
								,-mac_line_index);
						aif_test_index = aif_test_index + lab_index + label_match.end()+2;
						if (mac_parms.length() <= aif_test_index
								|| mac_parms.charAt(aif_test_index-1) != ','){
							aif_test_index = -1;
						}
					} else {
						log_error(113,mac_name[mac_name_index] + " invalid AIF label - " + mac_parms);
					}
				} else {
					if (aif_test_index == 0){
						log_error(114,mac_name[mac_name_index] + " invalid AIF label - " + mac_parms);
					}
					aif_test_index = -1;
				}
			}
		}
	}
	private void load_macro_label_sym(){
		/*
		 * During MLC or macro loading:
		 *   1.  Define macro labels .xxx
		 *   2.  If MLC lookahead, define ordinary symbol attribute
		 *       and length if available using az390 DS, DC, and
		 *       EQU processing services.  Note call_az390 surpresses
		 *       sending any BAL trace comments during lookahead.
		 *   3.  remove .* macro comments
		 */
		if (mac_label.length() > 1){
			if (mac_label.charAt(0) == '.'
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
			} else if (tz390.opt_asm 
					&& az390.lookahead
					&& mac_label.charAt(0) != '*'){
				int index = mac_line.indexOf("&");
				if (index == -1){
					set_lookahead_sym_attr_len(mac_label,mac_op,mac_parms);		
				}
			}
		} 
	}
	private void set_lookahead_sym_attr_len(String sym_label,String sym_op,String sym_parms){
		/*
		 * if ASM, set symbol type and length during
		 * MLC macro loading in lookahead mode
		 * 
		 * Notes:
		 *   1.  Called during macro load to define all
		 *       ordinary symbols in open code allowing forward
		 *       reference to type and length if available 
		 *       during macro execution.  The sym_attr and
		 *       sym_len are stored in AZ390 symbol table with
		 *       sym_def = -1 indicating lookahead mode definition.
		 *       Duplicates are ignored as there may be altermate
		 *       macro paths.
         *   2.  During macro execution, AZ390 recalcs
         *       all symbols for use during remained for 
         *       macro expansion.
		 * 
		 * Notes:
		 *   1.  sym_attr = 'U' and sym_len = 1 default
		 *       at first create.
		 *   2.  sym_def = -1 for lookahead and 0 for forward
		 *       ref during macro expansion.  AZ390 source line
		 *       references start at 1 as of RPI 415.
		 *   3.  Note during macro execution,
		 *       macro call label field symbol 
		 *       will be changed to sym_attr 'M'
		 *       if undefined or type 'U' and
		 *       will remain with sym_def = -1 to allow
		 *       redefine as ordinary symbol via BAL expansion.
		 */
		int index = tz390.find_key_index('R',sym_op.toUpperCase());
		if (index >= 0 && tz390.opsyn_old_name[index] != null){
			sym_op = tz390.opsyn_old_name[index];  
		}
		az390.bal_abort = false;
		az390.bal_label = sym_label;
		az390.bal_op    = sym_op;
		az390.bal_parms = sym_parms;
		int op_index = tz390.find_key_index('O',sym_op);
		if (op_index != -1){
			int op_type = tz390.op_type[op_index];
			if (op_type <= tz390.max_op_type_offset){
		        az390.update_label();
				az390.sym_attr[az390.cur_sid] = tz390.ascii_to_ebcdic['I'];
				az390.sym_len[az390.cur_sid] = tz390.op_type_len[op_type];
			} else if (sym_op.equals("CSECT")
					|| sym_op.equals("DSECT")
					|| sym_op.equals("LOCTR")
					|| sym_op.equals("RSECT")
					|| sym_op.equals("COM")					
					){
				az390.update_label();
				az390.sym_attr[az390.cur_sid] = tz390.ascii_to_ebcdic['J'];
				az390.sym_len[az390.cur_sid] = 1;
			} else if (sym_op.equals("DS") || sym_op.equals("DC")){
				az390.process_dc(1);
				az390.update_label();
			} else if (sym_op.equals("EQU")){
				az390.process_equ();
			}
		}	
	}
	private void add_mac(String macro_name){
		/*
		 * add macro file entry and 
		 * set mac_name_index else abort
		 */
		if (tot_mac_name < tz390.opt_maxfile){ // RPI 284
			mac_name_index = tot_mac_name;
			if (tot_mac_name > 0){  // RPI127 skip main pgm to allow macro later
				update_mac_key_index(mac_name_index,macro_name);
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
	private void update_mac_key_index(int index,String name){
		/*
		 * add or update macro key index
		 * 
		 */
		if (tz390.find_key_index('M',name) != -1){ // RPI 351
			tz390.update_key_index(index);
		} else {
			if (!tz390.add_key_index(index)){
				abort_error(170,"key search table exceeded adding " + name);
			}
		}
	}
	private void set_mac_file_num(){
		/*
		 * find/add file name and set cur_mac_file_num
		 */
		String mac_file_key = mac_file[cur_mac_file].getPath(); 
		cur_mac_file_num = tz390.find_key_index(
				'F',mac_file_key);		
		if (cur_mac_file_num == -1){
			if (tot_mac_file_name < tz390.opt_maxfile){
				cur_mac_file_num = tot_mac_file_name;
				tot_mac_file_name++;
				if (!tz390.add_key_index(cur_mac_file_num)){
					abort_error(172,"key search table exceeded adding " + mac_file_key);
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
		 * during macro loading
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
		if (tot_mac_lab < tz390.opt_maxsym){
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
				int old_mac_line_index = mac_line_index;
				mac_line_index = -mac_lab_index[index];
				create_mnote(4,"Macro " + mac_name[mac_index] + " undefined " + mac_lab_name[index] + " at line " + mac_file_line_num[mac_line_index]);
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
				tz390.systerm_io++;
				temp_line = mac_file_buff[cur_mac_file].readLine();
				cur_mac_line_num++;
				store_mac_line(); // RPI 273 update now for any cont. error
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
					temp_line = tz390.trim_trailing_spaces(temp_line);
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
				mac_line = tz390.trim_trailing_spaces(temp_line);  //RPI124
			} else {
				mac_line = tz390.trim_continue(temp_line.substring(0,71),tz390.split_first); // first line
				boolean mac_cont = true;
				while (mac_cont){ //RPI181 //RPI 215
					tz390.systerm_io++;
					temp_line = mac_file_buff[cur_mac_file].readLine();
					cur_mac_line_num++;
					store_mac_line(); // RPI 273 update now for any cont. error
					if (temp_line == null){
						abort_error(139,"missing continuation line " + cur_mac_line_num + " in " + mac_file[cur_mac_file].getPath());
					}
					temp_line = tz390.trim_trailing_spaces(temp_line);
					if (!tz390.verify_ascii_source(temp_line)){
						abort_error(140,"invalid ascii source line " + cur_mac_line_num + " in " + mac_file[cur_mac_file].getPath());
					}
					if (temp_line.length() < 72 || temp_line.charAt(71) <= asc_space_char){ //RPI181
						mac_cont = false;
						temp_line = tz390.trim_trailing_spaces(temp_line); //RPI124
					}
					if  (temp_line.length() >= 16
							&& temp_line.substring(0,15).equals("               ")){ // RPI 167
						temp_line = tz390.trim_continue(temp_line,tz390.split_cont); // RPI 315
						mac_line = mac_line.concat(temp_line.substring(15));         // RPI 315
					} else { 
						log_error(11,"continuation line < 16 characters - " + temp_line);
					}
				} 
			}
		} catch (IOException e){
			abort_error(29,"I/O error on file read " + e.toString());
		}
	}
	private void store_mac_line(){   // RPI 274
		/* 
		 * 1.  save mac_line during input
		 *     for use by log_error
		 * 2.  update &SYSSTMT
		 */  
		mac_file_line[mac_line_index]     = mac_line;
		mac_file_name_num[mac_line_index] = cur_mac_file_num;
		mac_file_line_num[mac_line_index] = cur_mac_line_num;
		bal_xref_index = mac_line_index;
		update_sysstmt();
	}
	private void update_sysstmt(){
		/*
		 * update current macro statement line
		 * system variable &sysstmt after
		 * each line change during loading
		 * and execution.
		 */
		if (mac_line_index >= tz390.opt_maxline){
			abort_error(146,"maximum source lines exceeded - increase maxline(nnn)");
		} else {
			gbl_seta[gbl_sysstmt_index] = mac_file_line_num[mac_line_index];
		}
	}
	private void parse_mac_line(){
		/*
		 * 1.  parse mac line into label, op, parms
		 * 2.  open copybook file if found
		 * 3.  if cics insert prolog and epilog
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
		tz390.split_line(mac_line);
		if (tz390.split_label != null){
			mac_label = tz390.split_label;
		} else {
			mac_label = "";
		}
		if (tz390.split_op != null){
			mac_op    = tz390.split_op.toUpperCase();
		} else {
			mac_op = "";
		}
		if (tz390.split_parms != null){
			mac_parms = tz390.split_parms;
		} else {
			mac_parms = "";
		}
		if (mac_op.equals("COPY")){ // RPI 300
			open_mac_copy_file();
			return;
		}
		if (tz390.opt_cics){
			if (tz390.opt_prolog){
				if (!cics_first_dsa_dsect 
						&& mac_op.equals("DSECT")
						&& mac_label.equals("DFHEISTG")){
					cics_first_dsa_dsect = true;
					mac_line  = " DFHEISTG";
					mac_label = "";
					mac_op    = "DFHEISTG";
					mac_parms = "";
				} else if (!cics_first_csect
						&& (mac_op.equals("CSECT")
						    || mac_op.equals("RSECT"))){
					cics_first_csect = true;
					mac_line = mac_label + " " + "DFHEIENT";
					mac_op = "DFHEIENT";
				}
			}
			if  (tz390.opt_epilog
					&& !cics_first_end
					&& mac_op.equals("END")){
				cics_first_end = true;
				mac_line = " DFHEIEND";
				mac_op = "DFHEIEND";
			}
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
		if (cur_mac_file >= tz390.opt_maxfile){
			cur_mac_file--;
			abort_error(100,"maximum nested copy files exceeded");
			return;
		}
		tz390.split_line(mac_parms); //RPI84
		if (tz390.split_label == null)tz390.split_label = "";
		new_mac_name = tz390.find_file_name(tz390.dir_cpy,tz390.split_label,tz390.cpy_type,tz390.dir_cur);
		if (new_mac_name == null){
			cur_mac_file--;
			if (load_type != load_mlc_file){ // RPI 300 
				log_error(101,"copy file not found - " + mac_parms);
			}
			return;
		}
		switch (load_type){ // RPI 300
		case 0: // load_mlc
		case 1: // load_mac_file
			mac_file[cur_mac_file] = new File(new_mac_name);
			try {
				mac_file_buff[cur_mac_file] = new BufferedReader(new FileReader(mac_file[cur_mac_file]));
				set_sys_dsn_mem_vol(mac_file[cur_mac_file].getCanonicalPath()); // RPI 259
				gbl_setc[gbl_syslib_index] = sys_dsn;
				gbl_setc[gbl_syslib_index+1] = sys_mem;
				gbl_setc[gbl_syslib_index+2] = sys_vol;
				set_mac_file_num();
				mac_file_cur_line_num[cur_mac_file - 1] = cur_mac_line_num;
				cur_mac_line_num = 0;
			} catch (IOException e){
				cur_mac_file--;
				abort_error(26,"I/O error opening file - " + e.toString());
			}
			break;
		case 3: // load_mac_inline
		case 4: // load_mac_exec
			cur_mac_file--;
			break;
		}
	}
	private void put_bal_line(String text_line){
		/*
		 * 1.  strip .mac labels if mfc
		 * 2.  set symbol attr if mfc
		 * 3.  optional reformatting
		 * 4.  optional pass to az390
		 * 5.  optional write to BAL 
		 */
	    if (text_line != null && !bal_eof){
	       tot_bal_line++;	// includes stats after END
	    }
		if (tz390.opt_asm
			&& text_line.length() > 0 
			&& text_line.charAt(0) != '*'){
			tz390.split_line(text_line);
			if (tz390.split_label == null){
				tz390.split_label = "";
			}
			if (tz390.split_op == null){
				tz390.split_op = "";
			}
			if (tz390.split_parms == null){
				tz390.split_parms = "";
			}
	        if (text_line.charAt(0) == '.'){
	        	// remove .mac label and force reformat
                text_line = tz390.left_justify(" ",tz390.split_label.length()) 
                         + text_line.substring(tz390.split_label.length());
	        	tz390.split_label = "";
			}
	        if (tz390.opt_reformat){
	        	text_line = reformat_bal();
	        }
		}
		if (tz390.opt_asm
			&& !bal_eof){
			call_az390_pass_bal_line(text_line); // RPI 415
		}
		if (!tz390.opt_bal){
			return;
		}
		try {
			if  (tz390.opt_text || text_line.length() < 72){ // RPI 264
				tz390.systerm_io++;
				bal_file_buff.write(text_line + "\r\n");
			} else {
				tz390.systerm_io++;
				bal_file_buff.write(text_line.substring(0,71) + "X\r\n");
				String text_left = text_line.substring(71);
				while (text_left.length() > 0){
					if  (text_left.length() > 56){
						String cont_line = "               " 
							+ text_left.substring(0,56) 
							+ "X\r\n";
						tz390.systerm_io++;
						bal_file_buff.write(cont_line);
						text_left = text_left.substring(56);
					} else {
						tz390.systerm_io++;
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
	private void call_az390_pass_bal_line(String text_line){
		/*
		 * pas text_line to az390 and update
		 * the az390 copy of mz390_errors
		 */
		if (az390.lookahead){
			return;
		}
		az390.mz390_errors = mz390_errors; //update mz390 errors for PRN
		az390.pass_bal_line(text_line,mac_file_name_num[bal_xref_index],mac_file_line_num[bal_xref_index]);
		if (az390.pass_bal_eof){
			bal_eof = true;
		}
	}
	private String reformat_bal(){
		/*
		 * reformat text_line from tz390.split
		 * at 10 and operands at 16 if possible
		 */
		String pad_label = "";
		String pad_op = "";
		if  (tz390.split_op.length() > 0
			 && tz390.split_label.length() < 8){
			pad_label = tz390.left_justify(" ",8);   
		}
		if  (tz390.split_op.length() > 0){
			int pad_op_len = 14 - (tz390.split_label.length() + pad_label.length() + tz390.split_op.length());
			if (pad_op_len > 0){
				pad_op = tz390.left_justify(" ",pad_op_len);
			} else {
				pad_op = "";
			}
		}
		return     tz390.split_label 
		         + pad_label + " "
		         + tz390.split_op + pad_op + " " 
		         + tz390.split_parms;
	}
	private void parse_bal_line(){
		/*
		 * 1.  Substitute any macro variables found
		 * 2.  Set bal_label and bal_o
		 */
		if (tz390.opt_tracem){
			put_trace("BAL PARSING  " + bal_line);
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
		int opcode_type = find_opcode_type(bal_op);
		if  (opcode_type <= tz390.max_asm_type){  // RPI 274 OPSYN cancel -2 
			// replace vars on model statements
			// but no conditional macro statements
			bal_line = replace_vars(bal_line,true);
			if (exp_var_replacement_change){
				split_bal_line();
			}
		}
		save_bal_op = bal_op;    
		if (bal_op != null && bal_op.length() > 0){
			String opsyn_key   = bal_op.toUpperCase();
			save_opsyn_index = tz390.find_key_index('R',opsyn_key);
			if (save_opsyn_index >= 0 && tz390.opsyn_old_name[save_opsyn_index] != null){
				bal_op = tz390.opsyn_old_name[save_opsyn_index];  /// RPI 306
			}
		}
	}
	private void split_bal_line(){
		/*
		 * split bal_line into bal_label, bal_op,
		 * and bal_parms
		 */
		tz390.split_line(bal_line);
		if (tz390.split_label != null){
			bal_label = tz390.split_label;
		} else {
			bal_label = "";
		}
		if (tz390.split_op != null){
			bal_op = tz390.split_op;
		} else {
			bal_op = "";
		}
		if (tz390.split_parms != null){
			bal_parms = tz390.split_parms;
		} else {
			bal_parms = "";
		}
	}
	private String replace_vars(String text,boolean bal_source){
		/* 
		 * replace all variables in text
		 * and set var_replacement if changed
		 * if reduce true, replace && with & and '' with '
		 * Notes:
		 *   1.  Per RPI 241 ignore undefined &vars
		 *       and let az390 report error if not in comment
		 */
		exp_var_replacement_mode = false;
		exp_var_replacement_change = false;
		bal_text = text;
		var_match   = var_pattern.matcher(bal_text);
		bal_text_index0 = 0; // end of prev. parm
		String new_text = "";
		String var_save = null;
		boolean comment_var_found = false;
		while (!comment_var_found && var_match.find()){
			bal_text_index1 = var_match.start();
			parm_value = var_match.group();
			var_save = parm_value;
			exp_var_replacement_mode = true;
			exp_var_replacement_change = true;
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
				if (parm_value != null){  // RPI 241 ignore var if error
					bal_text_index2 = bal_text_index1 + exp_next_index;
					if  (bal_text_index2 < bal_text.length()
							&& bal_text.charAt(bal_text_index2) == '.'){
						bal_text_index2++; // skip trailing . in parm substitution
					}
				} else {
					parm_value = var_save;
					bal_text_index2 = bal_text_index1 + parm_value.length();
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
		tz390.parm_match = tz390.parm_pattern.matcher(list.substring(1));
		int index = 1;
		int level = 1;
		String sublist = "";
		while (tz390.parm_match.find()){
			String token = tz390.parm_match.group();
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
				abort_case();
			}
		}
		return null;
	}
	private void abort_case(){
		/*
		 * abort case with invalide index
		 */
		abort_error(68,"internal error - invalid case index");
	}
	private int find_opcode_type(String opcode){
		/*
		 * return opcode type:
		 *   -1 - not found in opcode table
		 *    0 - comment (no opcode)
		 *      1-100 - machine type
		 *    100-200 - assembler type
		 *    200+    - macro operation 
		 * 
		 * Notes:
		 *   1.  tz390 opcode table used to find
		 *       opcode type.
		 *   2.  Return 0 for comment 
		 */
		if (opcode == null || opcode.length() == 0){
			return -1;
		}
		int index = tz390.find_key_index('O',opcode.toUpperCase());
		if (index > 0){
			return tz390.op_type[index];  
		} else {
			return -1;
		}
	}
	private void exec_ago(){
		/*
		 * branch to mac label or computed mac label
		 */
		old_mac_line_index = mac_line_index;
		if (bal_parms != null && bal_parms.length() > 1){
			if (bal_parms.charAt(0) != '('){
				exec_ago_branch(0);
				return;
			} else {
				int ago_index = calc_seta_exp(bal_parms,1);
				if (ago_index >= 1){
					int lab_index = exp_next_index;
					int index = 0;
					while (lab_index < bal_parms.length()){
						index++;
						label_match = label_pattern.matcher(bal_parms.substring(lab_index));
						if (label_match.find()){
							if (index == ago_index){
								exec_ago_branch(lab_index);
								return;
							} else {
								lab_index = lab_index + label_match.end()+1;
								if (lab_index >= bal_parms.length() 
										|| bal_parms.charAt(lab_index-1) != ','){
									// force exit with no branch 
									lab_index = bal_parms.length();
								}
							}
						} else {
							abort_error(150,"AGO invald macro label operand - " + bal_parms.substring(lab_index));
						}
					}
				}
			}
		} else {
			abort_error(149,"AGO missing macro label operand");
		}
	}
	private void exec_ago_branch(int lab_index){
		/*
		 * branch to ago target label
		 */
		actr_count--;
		old_mac_line_index = mac_line_index;
		mac_line_index = get_label_index(bal_parms.substring(lab_index));
		if (mac_line_index < mac_name_line_start[mac_name_index] 
		                                         || mac_line_index >= mac_name_line_end[mac_name_index]){
			mac_line_index = old_mac_line_index;
			abort_error(16,mac_name[mac_name_index] + " undefined " + bal_parms.substring(lab_index));
		}
		update_sysstmt();
	}
	private void exec_aif(){
		/*
		 * execute 1 or more AIF/AIFB tests
		 * and branch if true.
		 */
		int aif_test_index = 0; // start of next aif test (...).lab
		while (aif_test_index >= 0){
			if (calc_setb_exp(bal_parms.substring(aif_test_index),0) != 0){
				actr_count--;
				old_mac_line_index = mac_line_index;
				mac_line_index = get_label_index(bal_parms.substring(aif_test_index+exp_next_index));
				if (mac_line_index < mac_name_line_start[mac_name_index]
				                                         || mac_line_index >= mac_name_line_end[mac_name_index]){
					mac_line_index = old_mac_line_index;
					abort_error(142,"AIF macro label not found - " + bal_parms.substring(aif_test_index+exp_next_index));
				}
				aif_test_index = -1;
				if (tz390.opt_traceall){
					put_trace("AIF BRANCH");
				}
				update_sysstmt();
			} else {
				int label_comma_index = get_label_comma_index(bal_parms.substring(exp_next_index));
				if (label_comma_index != -1){
					aif_test_index = aif_test_index+exp_next_index+label_comma_index+1;
				} else {
					aif_test_index = -1;
					if (tz390.opt_traceall){
						put_trace("AIF  NO BRANCH");
					}
				}
			}
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
		switch (mac_op_type){
		case 201:  // ACTR  
			bal_op_ok = true;
			actr_count = calc_seta_exp(bal_parms,0);
			break;
		case 202:  // AGO 
		case 226:  // AGOB
			bal_op_ok = true;
			exec_ago();
			break;
		case 203:  // AIF 
		case 227:  // AIFB
			bal_op_ok = true;
			aif_op = true;
			exec_aif();
			aif_op = false;
			break;
		case 204:  // AINSERT
			break;
		case 205:  // ANOP 
			bal_op_ok = true;
			break;
		case 206:  // AREAD
			bal_op_ok = true;
			if (get_set_target(var_setc_type)){
				if (store_loc == lcl_loc){
					store_setc_index = lcl_set_start[store_name_index]+store_sub-1;
				} else {
					store_setc_index = gbl_set_start[store_name_index]+store_sub-1;
				}
				String dat_line = get_aread_string();
				put_setc_string(dat_line);
			}
			break;
		case 207:  // GBLA 
			bal_op_ok = true;
			alloc_set(var_seta_type,gbl_loc);
			break;
		case 208:  // GBLB 
			bal_op_ok = true;
			alloc_set(var_setb_type,gbl_loc);
			break;
		case 209:  // GBLC
			bal_op_ok = true;
			alloc_set(var_setc_type,gbl_loc);
			break;
		case 210:  // LCLA
			bal_op_ok = true;
			alloc_set(var_seta_type,lcl_loc);
			break;
		case 211:  // LCLB
			bal_op_ok = true;
			alloc_set(var_setb_type,lcl_loc);
			break;
		case 212:  // LCLC
			bal_op_ok = true;
			alloc_set(var_setc_type,lcl_loc);
			break;
		case 213:  // MHELP 
			break;
		case 214:  // MNOTE  RPI 238
			bal_op_ok = true;
			bal_parms = replace_vars(bal_parms,false);
			int mnote_level = 0;
			if (bal_parms.length() > 0 
					&& bal_parms.charAt(0) != '\''
						&& bal_parms.charAt(0) != ','
							&& bal_parms.charAt(0) != '*'){
				mnote_level = calc_seta_exp(bal_parms,0);
			}
			process_mnote(mnote_level,bal_parms);
			break;
		case 215:  // SETA 
			bal_op_ok = true;
			if (get_set_target(var_seta_type)){
				while (bal_parms != null){
					if (store_loc == lcl_loc){ // RPI 401
						store_seta_index = lcl_set_start[store_name_index]+store_sub-1;
					} else {
						store_seta_index = gbl_set_start[store_name_index]+store_sub-1;
					}
					store_seta_value = calc_seta_exp(bal_parms,0);
					put_seta_value(store_seta_value);
					if (bal_parms.length() >= exp_next_index 
							&& bal_parms.charAt(exp_next_index-1) == ','){
						bal_parms = bal_parms.substring(exp_next_index);
						store_sub++; // RPI 401
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
			if (get_set_target(var_setb_type)){
				while (bal_parms != null){
					if (store_loc == lcl_loc){ // RPI 401
						store_setb_index = lcl_set_start[store_name_index]+store_sub-1;
					} else {
						store_setb_index = gbl_set_start[store_name_index]+store_sub-1;
					}
					store_setb_value = calc_setb_exp(bal_parms,0);
					put_setb_value(store_setb_value);
					if (bal_parms.length() >= exp_next_index 
							&& bal_parms.charAt(exp_next_index-1) == ','){
						bal_parms = bal_parms.substring(exp_next_index);
						store_sub++;  // RPI 401
					} else {
						bal_parms = null;
					}
				}
			}
			break;
		case 218:  // SETC  
			bal_op_ok = true;
			if (get_set_target(var_setc_type)){
				while (!mac_abort && bal_parms != null){ 
					if (store_loc == lcl_loc){ // RPI 401
						store_setc_index = lcl_set_start[store_name_index]+store_sub-1;
					} else {
						store_setc_index = gbl_set_start[store_name_index]+store_sub-1;
					}
					String setc_string = calc_setc_exp(bal_parms,0); 
					put_setc_string(setc_string);
					if  (bal_parms.length() >= exp_next_index
							&& bal_parms.charAt(exp_next_index-1) == ','){
						bal_parms = bal_parms.substring(exp_next_index);
						store_sub++; // RPI 401
					} else {
						bal_parms = null;
					}                
				}
			}
			break;
		case 219:  // SETCF
			break;
		case 220:  // MACRO
			bal_op_ok = true;
			load_type = load_mac_inline;
			load_mac();
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
			put_bal_line("         PUNCH " + bal_parms);  // RPI 410
			break;
		case 224:  // COPY (copy to bal and issue error if not found) RPI 300
			bal_op_ok = true;
			bal_parms = replace_vars(bal_parms,false);
			put_bal_line(bal_line);
			mac_parms = bal_parms;
			load_type = load_mac_exec;
			open_mac_copy_file(); // issue error if not found
			break;
		case 225:  // OPSYN
			bal_op_ok = true;
			bal_line = replace_vars(bal_line,false); // RPI 274
			parse_bal_line();  // RPI 306
			put_bal_line(bal_line);
			tz390.update_opsyn(bal_label,bal_parms);
			break;
		default: 
			abort_case();
		}
		if (!bal_op_ok){
			put_bal_line(bal_line);
			abort_error(47,"macro operation not supported - " + bal_op);
		}
	}
	private void alloc_set(byte alloc_set_type,int alloc_set_loc){
		/*
		 * allocate set scalar,array, or created set
		 * variables on first occurance.
		 * 
		 * alloc_set_loc = lcl_set | gbl_set
		 * alloc_set_type = var_seta_type| var_setb_type | var_setc_type
		 *
		 * Notes:
		 *   1.  Duplicates ignored and expand used to 
		 *       handle any subscript beyond first alloc.
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
					|| (alloc_set_loc == lcl_loc
							&& exp_parse_set_loc == gbl_loc)){  //RPI178
				if (exp_parse_set_name != null){
					if (alloc_set_loc == lcl_loc){
						add_lcl_set(exp_parse_set_name,alloc_set_type,exp_parse_set_sub);
					} else {
						add_gbl_set(exp_parse_set_name,alloc_set_type,exp_parse_set_sub);
					}
				} else {
					log_error(105,"syntax error at " + text.substring(index));
				}
			} else if (exp_parse_set_loc != var_loc){
				log_error(106,"set local/global conflict for - " + text.substring(index));
			} else if (exp_parse_set_type != var_type){  //RPI178
				log_error(107,"set type conflict for - " + text.substring(index));
			} else {
				if (tz390.opt_traceall){
					put_trace(exp_parse_set_name + " already defined");
				}
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
			abort_case();
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
			abort_case();
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
			abort_case();
		}
		return "";
	}
	private boolean get_set_target(byte alloc_set_type){
		/*
		 * set set store info form bal_label
		 * and return true if ok else false
		 */
		store_type = alloc_set_type;
		if (parse_set_var(bal_label,0)){
			if (exp_parse_set_type != store_type){
				log_error(17,"invalid set variable type for - " + bal_label);
				return false;
			}
			store_loc = exp_parse_set_loc;
			store_name_index = exp_parse_set_name_index;
			store_sub  = exp_parse_set_sub;
		} else { 
			if (exp_parse_set_name == null){
				log_error(161,"invalid set variable name - " + bal_label);
				return false;
			}
			store_name = exp_parse_set_name;
			if (exp_parse_set_sub != 1){
				log_error(64,"set array not allocated for " + store_name + "(" + store_sub + ")");
				return false;
			} 
			store_loc = lcl_loc;
			store_sub = 1;
			store_name_index = add_lcl_set(store_name,store_type,store_sub);
		}
		if (store_loc == lcl_loc){
			store_min_index = lcl_set_start[store_name_index];
			store_max_index = lcl_set_end[store_name_index];
		} else {
			store_min_index = gbl_set_start[store_name_index];
			store_max_index = gbl_set_end[store_name_index];
		}
		return true;
	}
	private void put_seta_value(int seta_value){
		/*
		 * store seta string at store loc set by
		 * get_setc_target
		 */
		if  (store_loc == lcl_loc){
			if (store_seta_index < store_min_index){
				log_error(153,"lcla subscript < 1 = " + lcl_set_name[store_name_index] + "(" + (store_seta_index-lcl_set_start[store_name_index]+1) + ")" );
				store_seta_index = store_min_index;
			} else if (store_seta_index >= store_max_index){
				store_seta_index = expand_set(store_name_index,var_seta_type,lcl_loc,store_sub);			
			}
			lcl_seta[store_seta_index] = seta_value;
			if (store_seta_index > lcl_set_high[store_name_index]){
				lcl_set_high[store_name_index] = store_seta_index;
			}
			if (tz390.opt_tracem){
				put_trace("SETA LCLA " + lcl_set_name[store_name_index] + "(" + (store_seta_index - store_min_index + 1) + ")= " + lcl_seta[store_seta_index]);
			}
		} else {
			if (store_seta_index < store_min_index){
				log_error(154,"gbla subscript < 1 = " + gbl_set_name[store_name_index] + "(" + (store_seta_index-gbl_set_start[store_name_index]+1) + ")" );
				store_seta_index = store_min_index;
			} else if (store_seta_index >= store_max_index) {
				store_seta_index = expand_set(store_name_index,var_seta_type,gbl_loc,store_sub);	
			}
			gbl_seta[store_seta_index] = seta_value;
			if (store_seta_index > gbl_set_high[store_name_index]){
				gbl_set_high[store_name_index] = store_seta_index;
			}
			if (tz390.opt_tracem){
				put_log(" TRACE SETA GBLA " + gbl_set_name[store_name_index] + "(" + (store_seta_index - store_min_index + 1) + ")= " + gbl_seta[store_seta_index]);
			}
		}
	}
	private void put_setb_value(int setb_value){
		/*
		 * 
		 */
		if  (store_loc == lcl_loc){
			if (store_setb_index < store_min_index){
				log_error(155,"lclb subscript < 1 = " + lcl_set_name[store_name_index] + "(" + (store_setb_index-lcl_set_start[store_name_index]+1) + ")" );
				store_setb_index = store_min_index;
			} else if (store_setb_index >= store_max_index) {
				store_setb_index = expand_set(store_name_index,var_setb_type,lcl_loc,store_sub);	
			}
			lcl_setb[store_setb_index] = store_setb_value;
			if (store_setb_index > lcl_set_high[store_name_index]){
				lcl_set_high[store_name_index] = store_setb_index;
			}
			if (tz390.opt_tracem){
				put_trace("SETB LCLB " + lcl_set_name[store_name_index] + "(" + (store_setb_index - store_min_index + 1) + ")= " + lcl_setb[store_setb_index]);
			}
		} else {
			if (store_setb_index < store_min_index){
				log_error(156,"gblb subscript < 1 = " + gbl_set_name[store_name_index] + "(" + (store_setb_index-gbl_set_start[store_name_index]+1) + ")" );
				store_setb_index = store_min_index;
			} else if (store_setb_index >= store_max_index) {
				store_setb_index = expand_set(store_name_index,var_setb_type,gbl_loc,store_sub);	
			}
			gbl_setb[store_setb_index] = store_setb_value;
			if (store_setb_index > gbl_set_high[store_name_index]){
				gbl_set_high[store_name_index] = store_setb_index;
			}
			if (tz390.opt_tracem){
				put_trace("SETB GBLB " + gbl_set_name[store_name_index] + "(" + (store_setb_index - store_min_index + 1) + ")= " + gbl_setb[store_setb_index]);
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
			if (store_setc_index < store_min_index){
				log_error(157,"lclc subscript < 1 = " + lcl_set_name[store_name_index] + "(" + (store_setc_index-lcl_set_start[store_name_index]+1) + ")" );
				store_setc_index = store_min_index;
			} else if (store_setc_index >= store_max_index) {
				store_setc_index = expand_set(store_name_index,var_setc_type,lcl_loc,store_sub);	
			}
			lcl_setc[store_setc_index] = setc_string;
			if (store_setc_index > lcl_set_high[store_name_index]){
				lcl_set_high[store_name_index] = store_setc_index;
			}
			if (tz390.opt_tracem){
				put_trace("SETC LCLC " + lcl_set_name[store_name_index] + "(" + (store_setc_index - store_min_index + 1) + ")= " + lcl_setc[store_setc_index]);
			}
		} else {
			if (store_setc_index < store_min_index){
				log_error(158,"gblc subscript < 1 = " + gbl_set_name[store_name_index] + "(" + (store_setc_index-gbl_set_start[store_name_index]+1) + ")" );
				store_setc_index = store_min_index;
			} else if (store_setc_index >= store_max_index) {
				store_setc_index = expand_set(store_name_index,var_setc_type,gbl_loc,store_sub);	
			}
			gbl_setc[store_setc_index] = setc_string;
			if (store_setc_index > gbl_set_high[store_name_index]){
				gbl_set_high[store_name_index] = store_setc_index;
			}
			if (tz390.opt_tracem){
				put_trace("SETC GBLC " + gbl_set_name[store_name_index] + "(" + (store_setc_index - store_min_index + 1) + ")= " + gbl_setc[store_setc_index]);
			}
		}
	}
	private String get_aread_string(){
		/*
		 * read next mlc source line or next record 
		 * from file specified in DDNAME=, DSNAME=, or pgmname.dat 
		 * 1.  DDNAME= is extention to HLL assembler
		 *     where external variable defines file to
		 *     read for AREAD.
		 * 2.  DSNAME= is extention to HLL assembler
		 *     where macro variable defines file
		 *     to read for AREAD.
		 * Notes:
		 *   1.  Only DDNAME or DSNAME can be coded
		 *   2.  empty lines (CR,LF) returned as single space " "
		 *   3.  end of file returns 0 length string "".
		 */
		dat_file_index = 0;
		ap_file_name   = null;
		if (bal_parms.length() > 0 ){
			// read text from aread file 0-9
            set_aread_punch_file_options(bal_parms,tz390.dir_dat,tz390.dat_type);
            dat_file_index = ap_file_index;
			if (dat_file[dat_file_index] == null
				|| (ap_file_name != null 
					&& !ap_file_name.equals(dat_file[dat_file_index].getPath()))
			    ){
				if (dat_file[dat_file_index] != null){
					try {
						dat_file_buff[dat_file_index].close();
					} catch (IOException e){
						abort_error(69,"I/O error on AREAD close - " + e.toString());
						return "";
					}
				}
			}
			if  (dat_file[dat_file_index] == null){
				if (ap_file_name == ""){
					ap_file_name = tz390.dir_dat + tz390.pgm_name + tz390.dat_type;
				}
				try {
					dat_file[dat_file_index] = new File(ap_file_name);
					dat_file_buff[dat_file_index] = new BufferedReader(new FileReader(dat_file[dat_file_index]));
				} catch (IOException e){
					abort_error(70,"I/O error on AREAD open - " + e.toString());
					return "";
				}
			}
			try {
				String text = dat_file_buff[dat_file_index].readLine();
				if (text == null){
					dat_file_buff[dat_file_index].close();
					dat_file[dat_file_index] = null;
					return "";
				} else {
					tz390.systerm_io++;
					tot_aread_io++;
					text = tz390.trim_trailing_spaces(text);
					if (text.length() == 0){ // RPI 410
						text = " "; // return 1 space if all spaces or cr, lf
					}
					if (!tz390.verify_ascii_source(text)){
						abort_error(141,"invalid ascii source line " + cur_mac_line_num + " in " + dat_file[dat_file_index].getPath());
					}
					return text;
				}
			} catch (IOException e){
				abort_error(71,"I/O error on AREAD file read - " + e.toString());
				return "";
			}		
		} else {
			// read aread text from inline source
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
	private void set_aread_punch_file_options(String parms,String file_dir,String file_type){
		/*
		 * set ap_file_index and ap_file_name
		 * from the following AREAD or PUNCH parss:
		 *   1.  DDNAME= environment variable to get file name
		 *   2.  DSNAME= explicit file name string
		 *   3.  DSN=    epxlicit file name string (alias)
		 *   4.  ID=n    file index 0-9 (0 is default)
		 */
		ap_file_index = 0;
		ap_file_name = null;
		parms = replace_vars(parms,false); 
		String parm = null;
		while (parms.length() > 0){
			int index = parms.indexOf(',');
			if (index > 1){
				parm = parms.substring(0,index);
				parms = parms.substring(index+1);
			} else {
				parm = parms;
				parms = "";
			}
			if (parm.length() > 7 && parm.substring(0,7).toUpperCase().equals("DDNAME=")){
				String ddname = parm.substring(7);
				ap_file_name = get_ddname_file_name(ddname);
				ap_file_name = tz390.get_file_name(file_dir,ap_file_name,file_type);
			} else if (parm.length() > 7 && parm.substring(0,7).toUpperCase().equals("DSNAME=")){
				ap_file_name = parm.substring(7);
				ap_file_name = tz390.get_file_name(file_dir,ap_file_name,file_type);
			} else if (parm.length() > 4 && parm.substring(0,4).toUpperCase().equals("DSN=")){
				ap_file_name = parm.substring(4);
				ap_file_name = tz390.get_file_name(file_dir,ap_file_name,file_type);
			} else if (parm.length() > 3 && parm.substring(0,3).toUpperCase().equals("ID=")
					   && parm.substring(3).compareTo("0") >= 0
					   && parm.substring(3).compareTo("9") <= 0
			          ){
				ap_file_index = Integer.valueOf(parm.substring(3));
			} else {
				log_error(185,"invalid parm " + parm);
			}
		}
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
			pch_file_index = 0;
			if (pch_match.end()+2 < pch_parms.length()){
				pch_parms = pch_parms.substring(pch_match.end()+2);
			} else {
				pch_parms = "";
			}
			if (pch_parms.length() > 0){
				set_aread_punch_file_options(pch_parms,tz390.dir_pch,tz390.pch_type);
                pch_file_index = ap_file_index;
				if  (pch_file[pch_file_index] == null
					 || (ap_file_name != null 
						 && !ap_file_name.equals(pch_file[pch_file_index].getPath()))
				    ){
					if  (pch_file[pch_file_index] != null){
						try {							
							pch_file_buff[pch_file_index].close();
							pch_file[pch_file_index] = null;
						} catch (IOException e){
							abort_error(74,"I/O error on PUNCH close - " + e.toString());
							return;
						}
					}
				}
			}
			if (pch_file[pch_file_index] == null){
				if (pch_file_index == 0 
					&& ap_file_name == null){
					ap_file_name = tz390.dir_pch + tz390.pgm_name + tz390.pch_type;
				}
				try {
					pch_file[pch_file_index] = new File(ap_file_name);
					pch_file_buff[pch_file_index] = new BufferedWriter(new FileWriter(pch_file[pch_file_index]));
				} catch (IOException e){
					abort_error(75,"I/O error on PUNCH open - " + e.toString());
				}
			}
			try {
				pch_text = ("X" + pch_text).trim().substring(1); //RPI 195
				tz390.systerm_io++;
				tot_punch_io++;
				pch_file_buff[pch_file_index].write(pch_text + "\r\n");
				if (pch_file[pch_file_index].length() > tz390.max_file_size){
					abort_error(120,"maximum pch file size exceeded");
				}
			} catch (IOException e){
				abort_error(76,"I/O error on PUNCH file write - " + e.toString());
			}
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
		 *       proceeding to next operator, And ignore
		 *       var not found.
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
		exp_first_sym_index = -1;
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
		 *     previous setting of exp_var_last.
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
					exp_start_index = exp_next_index - exp_token.length() + 1;  // RPI 274
					exp_match = exp_pattern.matcher(exp_text.substring(exp_start_index));
					exp_var_last = true;
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
			if (exp_var_pushed){
				exp_next_class = exp_class_mpy_div;
			} else {
				exp_push_sdt("0");  // RPI 340
			}
			break;
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
			} else if (exp_next_op.length() == 3 
					&& exp_next_op.substring(0,2).equals("A2")){ // RPI 404
				exp_next_class = exp_class_oper;
			} else {
				push_sym();
				exp_var_last = true;
			}
			break;
		case 'B':
			if (exp_next_op.length() > 2
					&& exp_next_op.charAt(exp_next_op.length()-1) == '\''){
				exp_push_sdt(exp_token);
			} else if (exp_next_op.length() == 3 
					&& exp_next_op.substring(0,2).equals("B2")){ // RPI 404
				exp_next_class = exp_class_oper;
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
			} else if (exp_next_op.length() == 3 
					&& exp_next_op.substring(0,2).equals("C2")){ // RPI 404
				exp_next_class = exp_class_oper;
			} else {
				push_sym();
				exp_var_last = true;
			}
			break;
		case 'D':
			if (exp_next_op.equals("D'")){ // RPI 336
				exp_next_class = exp_class_oper; 
			} else if (exp_next_op.equals("DOUBLE")){
				exp_next_class = exp_class_oper;
			} else if (exp_next_op.equals("DCLEN")){
				exp_next_class = exp_class_oper;
			} else if (exp_next_op.equals("DCVAL")){
				exp_next_class = exp_class_oper;
			} else if (exp_next_op.equals("DCEQUOTE")){
				exp_next_class = exp_class_oper;
			} else if (exp_next_op.length() == 3 
					&& exp_next_op.substring(0,2).equals("D2")){ // RPI 404
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
			} else if (exp_next_op.length() == 5 
					&& exp_next_op.substring(0,2).equals("IS")){ // RPI 404
				exp_next_class = exp_class_oper;
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
			} else if (exp_next_op.equals("O'")){ // RPI 406
				exp_next_class = exp_class_oper;
			} else {
				push_sym();
				exp_var_last = true;
			}
			break;
		case 'S':
			if (exp_next_op.length() == 6 
					&& exp_next_op.equals("SIGNED")){ // RPI 404
				exp_next_class = exp_class_oper;
			} else if (exp_next_op.length() == 3 
					&& exp_next_op.equals("SLA")){ // RPI 404
				exp_next_class = exp_class_oper;
			} else if (exp_next_op.length() == 3 
					&& exp_next_op.equals("SLL")){ // RPI 404
				exp_next_class = exp_class_oper;
			} else if (exp_next_op.length() == 3 
					&& exp_next_op.equals("SRA")){ // RPI 404
				exp_next_class = exp_class_oper;
			} else if (exp_next_op.length() == 3 
					&& exp_next_op.equals("SRL")){ // RPI 404
				exp_next_class = exp_class_oper;
			} else if (exp_next_op.length() == 8 
						&& exp_next_op.equals("SYSATTRA")){ // RPI 404
				exp_next_class = exp_class_oper;
			} else if (exp_next_op.length() == 8 
					&& exp_next_op.equals("SYSATTRP")){ // RPI 404
				exp_next_class = exp_class_oper;
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
			} else if (exp_next_op.equals("XOR")){
				exp_next_class = exp_class_xor;
			} else if (exp_next_op.length() == 3 
					&& exp_next_op.substring(0,2).equals("X2")){ // RPI 404
				exp_next_class = exp_class_oper;
			} else {
				push_sym();
				exp_var_last = true;
			}
			break;  // RPI 340
		default: // RPI 347
			push_sym();
		exp_var_last = true;
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
			put_trace("EXP OPS=" + tot_exp_stk_op + " VARS=" + tot_exp_stk_var + " PREV OP = " + exp_prev_op +  " NEXT OP = " + exp_token);
		}
		if  (exp_prev_class == 0){
			log_error(162,"invalid expression operator class for - " + exp_token);
		}
		if  (exp_next_class == 0){
			log_error(163,"invalid expression operator class - " + exp_token);
		}
		int action = exp_action[tot_classes*(exp_prev_class-1)+exp_next_class-1];
		if (tz390.opt_traceall){
			put_trace("EXP OPS=" + tot_exp_stk_op + " VARS=" + tot_exp_stk_var + " ACTION = " + action + " PREV CLASS = " + exp_prev_class + " NEXT CLASS = " + exp_next_class);
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
			if (exp_level == 0 && aif_op){  // RPI 314
				exp_set_term_op();
				exp_term(); 
			} else {
				exp_check_prev_op = false;
				if (exp_next_index < exp_text_len
						&& exp_text.charAt(exp_next_index) == '\''){;
						exp_token = "DUP"; // RPI 338
						exp_next_class = exp_class_oper;
						exp_push_op();
				}
			}
			break;
		case  5: // 2 operand string operators 
			exp_pop_op();
			switch (exp_prev_op.charAt(0)){
			case '.': // concatenate
				exp_str_concat();
				break;
			case 'D': // duplicate string
				exp_str_duplicate(); // RPI 421
				break;
			case 'I': // find index in string for
				exp_str_index();
				break;
			case 'F': // find index of substring in string
				exp_str_find(); 
				break;
			default:
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
			exp_perform_prefix_op();
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
	private void exp_perform_prefix_op(){
		/*
		 * perform prefix operator replacing
		 * value on stack with result value
		 */
		exp_pop_op();
		if (tz390.opt_traceall){
			put_trace(" PREFIX OP=" + exp_prev_op + "VARS=" +tot_exp_stk_var);
		}
		if (tot_exp_stk_var < 1 && exp_stk_op[tot_exp_stk_op].charAt(0) != 'U'){
			log_error(175,"missing argument for prefix operator");
			return;
		}
		switch (exp_prev_first){
		case 'A': // A2? convert from value  RPI 404
			if (exp_stk_op[tot_exp_stk_op].equals("A2B")){
				setc_value = Integer.toString(get_seta_stack_value(-1),2);
				tot_exp_stk_var--;
				seta_value = setc_value.length();
				seta_value = seta_value - seta_value/8*8;
				if (seta_value != 0){
					setc_value = "00000000".substring(seta_value) + setc_value;
				}
				put_setc_stack_var();
			} else if (exp_stk_op[tot_exp_stk_op].equals("A2C")){
				seta_value = get_seta_stack_value(-1);
				tot_exp_stk_var--;
				setc_value = ""
					       + (char)tz390.ebcdic_to_ascii[seta_value >>> 24]
					       + (char)tz390.ebcdic_to_ascii[seta_value >>> 16 & 0xff]         
					       + (char)tz390.ebcdic_to_ascii[seta_value >>> 8  & 0xff]
					       + (char)tz390.ebcdic_to_ascii[seta_value        & 0xff]					                               
					       ;
				put_setc_stack_var();
			} else if (exp_stk_op[tot_exp_stk_op].equals("A2D")){
				seta_value = get_seta_stack_value(-1);
				tot_exp_stk_var--;
				if (seta_value < 0){
					seta_value = - seta_value;
				}
				setc_value = Integer.toString(seta_value);
				put_setc_stack_var();
			} else if (exp_stk_op[tot_exp_stk_op].equals("A2X")){
				seta_value = get_seta_stack_value(-1);
				tot_exp_stk_var--;
				setc_value = Integer.toString(seta_value,16).toUpperCase();
				setc_value = ("00000000" + setc_value).substring(setc_value.length());
				put_setc_stack_var();
			} else {
				log_error(176,"invalid prefix operator");
			}
			break;
		case 'B': // B2A - convert '010' to 2 etc.  RPI 404
			if (exp_stk_op[tot_exp_stk_op].equals("B2A")){
				setc_value = get_setc_stack_value();
				exp_push_sdt("B'" + setc_value + "'");
			} else if (exp_stk_op[tot_exp_stk_op].equals("B2C")){
				seta_value = Integer.valueOf(get_setc_stack_value(),2);
				setc_value = ""
					       + (char)tz390.ebcdic_to_ascii[seta_value >>> 24]
					       + (char)tz390.ebcdic_to_ascii[seta_value >>> 16 & 0xff]         
					       + (char)tz390.ebcdic_to_ascii[seta_value >>> 8  & 0xff]
					       + (char)tz390.ebcdic_to_ascii[seta_value        & 0xff]					                               
					       ;
				put_setc_stack_var();
			} else if (exp_stk_op[tot_exp_stk_op].equals("B2D")){
				setc_value = get_setc_stack_value();
				seta_value = Integer.valueOf(setc_value,2);
				if (seta_value < 0){
					seta_value = - seta_value;
				}
				setc_value = Integer.toString(seta_value);
				put_setc_stack_var();
			} else if (exp_stk_op[tot_exp_stk_op].equals("B2X")){
				seta_value = Integer.valueOf(get_setc_stack_value(),2);
				setc_value = Integer.toString(seta_value,16).toUpperCase();
				setc_value = ("00000000" + setc_value).substring(setc_value.length());
				put_setc_stack_var();
			} else {
				log_error(177,"invalid prefix operator");
			}
			break;
		case 'C': // C2A - convert '1' to 241 etc.  RPI 404
			if (exp_stk_op[tot_exp_stk_op].equals("C2A")){
				setc_value = get_setc_stack_value();
				exp_push_sdt("C'" + setc_value + "'");
			} else if (exp_stk_op[tot_exp_stk_op].equals("C2B")){
				setc_value = "C'" + get_setc_stack_value() + "'";
				if (!tz390.get_sdt_char_int(setc_value)){
					log_error(178,"invalid character sdt " + setc_value);
				}
				seta_value = tz390.sdt_char_int; 
				setc_value = Integer.toString(seta_value,2);
				seta_value = setc_value.length();
				seta_value = seta_value - seta_value/8*8;
				if (seta_value != 0){
					setc_value = "00000000".substring(seta_value) + setc_value;
				}
				put_setc_stack_var();
			} else if (exp_stk_op[tot_exp_stk_op].equals("C2D")){
				setc_value = "C'" + get_setc_stack_value() + "'";
				if (!tz390.get_sdt_char_int(setc_value)){
					log_error(179,"invalid character sdt " + setc_value);
				}
				seta_value = tz390.sdt_char_int; 
				if (seta_value < 0){
					seta_value = - seta_value;
				}
				setc_value = Integer.toString(seta_value);
				put_setc_stack_var();
			} else if (exp_stk_op[tot_exp_stk_op].equals("C2X")){
				setc_value = "C'" + get_setc_stack_value() + "'";
				if (!tz390.get_sdt_char_int(setc_value)){
					log_error(180,"invalid character sdt " + setc_value);
				}
				seta_value = tz390.sdt_char_int; 
				setc_value = Integer.toString(seta_value,16).toUpperCase();
				setc_value = ("00000000" + setc_value).substring(setc_value.length());
				put_setc_stack_var();
			} else {
				log_error(181,"invalid prefix operator");
			}
			break;	
		case 'D':
			if (exp_stk_op[tot_exp_stk_op].equals("D2A")){
				seta_value1 = get_seta_stack_value(-1);
				tot_exp_stk_var--;
				put_seta_stack_var();
		    } else if (exp_stk_op[tot_exp_stk_op].equals("D2B")){
				seta_value = get_seta_stack_value(-1);
				tot_exp_stk_var--;
				setc_value = Integer.toString(seta_value,2);
				seta_value = setc_value.length();
				seta_value = seta_value - seta_value/8*8;
				if (seta_value != 0){
					setc_value = "00000000".substring(seta_value) + setc_value;
				}
				put_setc_stack_var();
		    } else if (exp_stk_op[tot_exp_stk_op].equals("D2C")){
				seta_value = get_seta_stack_value(-1);
				tot_exp_stk_var--;
				setc_value = ""
					       + (char)tz390.ebcdic_to_ascii[seta_value >>> 24]
					       + (char)tz390.ebcdic_to_ascii[seta_value >>> 16 & 0xff]         
					       + (char)tz390.ebcdic_to_ascii[seta_value >>> 8  & 0xff]
					       + (char)tz390.ebcdic_to_ascii[seta_value        & 0xff]					                               
					       ;
				put_setc_stack_var();
			} else if (exp_stk_op[tot_exp_stk_op].equals("D2X")){
				seta_value = get_seta_stack_value(-1);
				tot_exp_stk_var--;
				setc_value = Integer.toString(seta_value,16).toUpperCase();
				setc_value = ("00000000" + setc_value).substring(setc_value.length());
				put_setc_stack_var();
			} else if (exp_stk_op[tot_exp_stk_op].equals("D'")){// RPI 336
				if (tot_exp_stk_var > 0){
					setc_value = get_setc_stack_value();
					seta_value1 = get_sym_def(setc_value);
					put_seta_stack_var();
				} else {
					log_error(152,"missing variable for D' operator");
				}
			} else if (exp_stk_op[tot_exp_stk_op].equals("DUP")){// RPI 338
                exp_str_duplicate();	// RPI 421
			} else if (exp_stk_op[tot_exp_stk_op].equals("DOUBLE")){
				setc_value = get_setc_stack_value().replaceAll("\\'","\\'\\'").replaceAll("\\&","\\&\\&"); //RPI195
				put_setc_stack_var();
			} else if (exp_stk_op[tot_exp_stk_op].equals("DCLEN")){
				setc_value = get_setc_stack_value().replaceAll("\\'\\'","\\'").replaceAll("\\&\\&","\\&"); 
				seta_value1 = setc_value.length();
				put_seta_stack_var();
			} else if (exp_stk_op[tot_exp_stk_op].equals("DCVAL")){
				setc_value = get_setc_stack_value().replaceAll("\\'\\'","\\'").replaceAll("\\&\\&","\\&"); 
				put_setc_stack_var();
			} else if (exp_stk_op[tot_exp_stk_op].equals("DCEQUOTE")){
				setc_value = get_setc_stack_value(); 
				if (setc_value.length() > 1
					&& setc_value.charAt(0) == '\'' 
					&& setc_value.charAt(setc_value.length()-1) == '\''){
					setc_value = setc_value.substring(1,setc_value.length()-1);
				}
				put_setc_stack_var();
			} else {
                log_error(182,"undefined prefix operator");
			}
			break;
		case 'I': // ISBIN, ISDEC, ISHEX
			if (exp_stk_op[tot_exp_stk_op].equals("ISBIN")){
				setc_value = get_setc_stack_value();
				try {
					seta_value1 = Integer.valueOf(setc_value,2);
				    setb_value1 = 1;
				} catch (Exception e){
					setb_value1 = 0;
				}
				put_setb_stack_var();
			} else if (exp_stk_op[tot_exp_stk_op].equals("ISDEC")){
				setc_value = get_setc_stack_value();
				try {
					seta_value1 = Integer.valueOf(setc_value);
				    setb_value1 = 1;
				} catch (Exception e){
					setb_value1 = 0;
				}
				put_setb_stack_var();
			} else if (exp_stk_op[tot_exp_stk_op].equals("ISHEX")){
				setc_value = get_setc_stack_value();
				try {
					seta_value1 = Integer.valueOf(setc_value,16);
				    setb_value1 = 1;
				} catch (Exception e){
					setb_value1 = 0;
				}
				put_setb_stack_var();
			} else if (exp_stk_op[tot_exp_stk_op].equals("ISSYM")){
				setc_value = get_setc_stack_value();
				if (setc_value.length() > 0 && setc_value.length() <= 63){
					symbol_match = symbol_pattern.matcher(setc_value); 
					if (symbol_match.find() 
						&& symbol_match.end() == setc_value.length()){
						setb_value1 = 1;
					} else {
						setb_value1 = 0;
					}
				} else {
					setb_value = 0;
				}
				put_setb_stack_var();
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
		case 'N': // N'var returns sublist count or max arrray store subscript
			if (tot_exp_stk_var > 0){
				switch (exp_stk_type[tot_exp_stk_var - 1]){
				case 1: // seta
				case 2: // setb
				case 3: // setc
					if (var_loc == lcl_loc){
						if (lcl_set_end[var_name_index] - lcl_set_start[var_name_index] > 1){
							seta_value1 = lcl_set_high[var_name_index] - lcl_set_start[var_name_index]+1;
						} else {
							seta_value1 = 0;
						}
						tot_exp_stk_var--;
					} else if (var_loc == gbl_loc){
						if (gbl_set_end[var_name_index] - gbl_set_start[var_name_index] > 1
								&& gbl_set_high[var_name_index] > 0){
							seta_value1 = gbl_set_high[var_name_index]-gbl_set_start[var_name_index]+1;
						} else {
							seta_value1 = 0;
						}
						tot_exp_stk_var--;
					} else {
						setc_value = get_setc_stack_value();
						seta_value1 = get_sublist_count(setc_value);
					}
					put_seta_stack_var();
					break;
				case 4: // parm var_parm_type
				case 5: // subscript var_subscript type
				case 6: // sublist var_sublist_type
					if (exp_stk_setb[tot_exp_stk_var - 1] == syslist_loc
							&& exp_stk_seta[tot_exp_stk_var - 1] == -1
							&& mac_call_level > 0){
						seta_value1 = mac_call_pos_tot[mac_call_level];
						tot_exp_stk_var--; // remove syslist var
						put_seta_stack_var();
					} else {
						setc_value = get_setc_stack_value();
						seta_value1 = get_sublist_count(setc_value);
					}
					break;
				default: 
					log_error(159,"invalid argument for N'");
				}
				exp_var_pushed = true; // prevent unary minus
			} else {
				log_error(67,"missing variable for N' operator");
			}
			break;
		case 'O': // O'opcode returns opcode attribute  // RPI 405
			if (tot_exp_stk_var > 0){
				setc_value = get_setc_stack_value().toUpperCase();
				int opcode_type = find_opcode_type(setc_value);
				if (opcode_type >= 0){
					if (opcode_type <= tz390.max_ins_type){
						if (opcode_type == 3
								|| opcode_type == 6
								|| opcode_type == 13){
							setc_value = "E"; // BRX, BCX, BRCX extended mnemonic
						} else {
							setc_value = "O"; // machine opcode
						}
					} else {
						setc_value = "A"; // assembler opcode
					}
				} else if (opcode_type == -1){
					int macro_index = find_mac_entry(setc_value);
					if (macro_index > 0){
						setc_value = "M";
					} else if (macro_index == -1 
							&& tz390.find_file_name(tz390.dir_mac,setc_value,tz390.mac_type,tz390.dir_cur) != null){
						setc_value = "S";
					} else {
						setc_value = "U";
					}
				} else {
					setc_value = "U"; // deleted or not found
				}
				put_setc_stack_var();
			} else {
				log_error(67,"missing variable for L' operator");
			}
			break;
		case 'S': // SIGNED value returns string with sign
		    if (exp_stk_op[tot_exp_stk_op].equals("SIGNED")){
		    	seta_value = get_seta_stack_value(-1);
		    	tot_exp_stk_var--;
		    	setc_value = Integer.toString(seta_value);
		    	put_setc_stack_var();
		    } else if (exp_stk_op[tot_exp_stk_op].equals("SLA")){
		    	seta_value1 = get_seta_stack_value(-2);
		    	seta_value2 = get_seta_stack_value(-1);
		    	tot_exp_stk_var = tot_exp_stk_var - 2;
		    	seta_value = seta_value1 << seta_value2;
		    	if (seta_value1 >= 0){
		    		seta_value1 = seta_value & 0x7fffFfff;
		    	} else {
		    		seta_value1 = seta_value | 0x80000000;
		    	}
		    	put_seta_stack_var();
		    } else if (exp_stk_op[tot_exp_stk_op].equals("SLL")){
		    	seta_value1 = get_seta_stack_value(-2);
		    	seta_value2 = get_seta_stack_value(-1);
		    	tot_exp_stk_var = tot_exp_stk_var - 2;
		    	seta_value1 = seta_value1 << seta_value2;
		    	put_seta_stack_var(); 	
		    } else if (exp_stk_op[tot_exp_stk_op].equals("SRA")){
		    	seta_value1 = get_seta_stack_value(-2);
		    	seta_value2 = get_seta_stack_value(-1);
		    	tot_exp_stk_var = tot_exp_stk_var - 2;
		    	seta_value1 = seta_value1 >> seta_value2;
		    	put_seta_stack_var();
		    } else if (exp_stk_op[tot_exp_stk_op].equals("SRL")){
		    	seta_value1 = get_seta_stack_value(-2);
		    	seta_value2 = get_seta_stack_value(-1);
		    	tot_exp_stk_var = tot_exp_stk_var - 2;
		    	seta_value1 = seta_value1 >>> seta_value2;
		    	put_seta_stack_var();
		    } else if (exp_stk_op[tot_exp_stk_op].equals("SYSATTRA")){
		    	setc_value = get_setc_stack_value();
				int cur_sym = mz390_find_sym(setc_value);
				if (cur_sym >= 0){
					setc_value = az390.sym_attra[cur_sym];
				} else {
					setc_value = "";
				}
		    	put_setc_stack_var();
		    } else if (tz390.opt_asm && exp_stk_op[tot_exp_stk_op].equals("SYSATTRP")){
		    	setc_value = get_setc_stack_value();
		    	int cur_sym = mz390_find_sym(setc_value);
		    	if (cur_sym >= 0){
		    		seta_value = az390.sym_attrp[cur_sym];
		    		setc_value = (""
							       + (char)tz390.ebcdic_to_ascii[seta_value >>> 24]
							       + (char)tz390.ebcdic_to_ascii[seta_value >>> 16 & 0xff]         
							       + (char)tz390.ebcdic_to_ascii[seta_value >>> 8  & 0xff]
							       + (char)tz390.ebcdic_to_ascii[seta_value        & 0xff]					                               
							       ).trim();
		    	} else {
		    		setc_value = "";
		    	}
		    	put_setc_stack_var();
		    } else {
		    	log_error(183,"undefined prefix operator");
		    }
		    break;
		case 'T': // T'sym returns type attribute as setc
			if (tz390.opt_asm && tot_exp_stk_var > 0){
				setc_value = get_setc_stack_value();
				int cur_sym = mz390_find_sym(setc_value);
				if (cur_sym >= 0){
					if (az390.sym_attre[cur_sym] == az390.sym_attre_def){
						setc_value = "" + (char)tz390.ebcdic_to_ascii[az390.sym_attr[cur_sym] & 0xff];
					} else {
						setc_value = "" + (char)tz390.ebcdic_to_ascii[az390.sym_attre[cur_sym] & 0xff];
					}
				} else {
					if (setc_value.length() > 0){
						if  (setc_value.charAt(0) >= '0' 
							&& setc_value.charAt(0) <= '9'){ //RPI180
							setc_value  = "N";
						} else {
							setc_value = "U";
						}
					} else {
						setc_value = "O";;
					}
				}
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
			break;
		case 'X':
			if (exp_stk_op[tot_exp_stk_op].equals("X2A")){
				seta_value1 = Integer.valueOf(get_setc_stack_value(),16);
				put_seta_stack_var();
			} else if (exp_stk_op[tot_exp_stk_op].equals("X2B")){
				seta_value1 = Integer.valueOf(get_setc_stack_value(),16);
				setc_value = Integer.toString(seta_value1,2);
				seta_value = setc_value.length();
				seta_value = seta_value - seta_value/8*8;
				if (seta_value != 0){
					setc_value = "00000000".substring(seta_value) + setc_value;
				}
				put_setc_stack_var();
			} else if (exp_stk_op[tot_exp_stk_op].equals("X2C")){
				seta_value = Integer.valueOf(get_setc_stack_value(),16);
				setc_value = ""
					       + (char)tz390.ebcdic_to_ascii[seta_value >>> 24]
					       + (char)tz390.ebcdic_to_ascii[seta_value >>> 16 & 0xff]         
					       + (char)tz390.ebcdic_to_ascii[seta_value >>> 8  & 0xff]
					       + (char)tz390.ebcdic_to_ascii[seta_value        & 0xff]					                               
					       ;
				put_setc_stack_var();
			} else if (exp_stk_op[tot_exp_stk_op].equals("X2D")){
				setc_value = get_setc_stack_value();
				seta_value = Integer.valueOf(setc_value,16);
				if (seta_value < 0){
					seta_value = - seta_value;
				}
				setc_value = Integer.toString(seta_value);
				put_setc_stack_var();
			} else if (exp_stk_op[tot_exp_stk_op].equals("B2X")){
				seta_value = Integer.valueOf(get_setc_stack_value(),2);
				setc_value = Integer.toString(seta_value,16).toUpperCase();
				setc_value = ("00000000" + setc_value).substring(setc_value.length());
				put_setc_stack_var();
			} else {
				log_error(177,"invalid prefix operator");
			}
            break;
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
		if (tz390.opt_traceall){
			put_trace("ADD " + seta_value1 + " + " + seta_value2);
		}
		seta_value1 = seta_value1 + seta_value2;
		put_seta_stack_var();
	}
	private void exp_sub(){
		/* sub top of stack value from prev. value
		 * and pop the top stack value off
		 */
		get_seta_stack_values();
		if (tz390.opt_traceall){
			put_trace("SUB " + seta_value1 + " - " + seta_value2);
		}
		seta_value1 = seta_value1 - seta_value2;
		put_seta_stack_var();
	}
	private void exp_mpy(){
		/* mpy top of stack value to prev. value
		 * and pop the top stack value off
		 */
		get_seta_stack_values();
		if (tz390.opt_traceall){
			put_trace("MPY " + seta_value1 + " * " + seta_value2);
		}
		seta_value1 = seta_value1 * seta_value2;
		put_seta_stack_var();
	}
	private void exp_div(){
		/* div top of stack value into prev. value
		 * and pop the top stack value off
		 */
		get_seta_stack_values();
		if (tz390.opt_traceall){
			put_trace("DIV " + seta_value1 + " / " + seta_value2);
		}
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
	private void exp_str_duplicate(){ // RPI 421
		/*
		 * duplicate string on top of stack
		 * by value of top-1 count
		 */
		if (tot_exp_stk_var > 1){
			setc_value1 = get_setc_stack_value();
			seta_value1 = get_seta_stack_value(-1);
			tot_exp_stk_var--;
			setc_value = tz390.get_dup_string(setc_value1,seta_value1);
			put_setc_stack_var();
		} else {
			log_error(152,"missing variable for D' operator");
		}
	}
	private void exp_str_index(){
		/*
		 * return seta index of first occurance of 
		 * second string within the first string
		 */
		get_setc_stack_values();
		if (tz390.opt_traceall){
			put_trace("INDEX " + setc_value1 + " IN " + setc_value2);
		}
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
		 * return seta index of first character
		 * in str2 found in str1
		 */
		get_setc_stack_values();
		if (tz390.opt_traceall){
			put_trace("FIND " + setc_value1 + " IN " + setc_value2);
		}
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
				abort_case();
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
				abort_case();
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
				abort_case();
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
				abort_case();
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
				abort_case();
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
				abort_case();
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
			&& setc_value1.charAt(index)
			== setc_value2.charAt(index)){
			index++;
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
				if (  (tz390.ascii_to_ebcdic[setc_value1.charAt(index) & 0xff] & 0xff)
				    > (tz390.ascii_to_ebcdic[setc_value2.charAt(index) & 0xff] & 0xff)
				   ){ // RPI 422
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
		if (tz390.opt_traceall){
			put_trace("NOT");
		}
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
			if (tz390.opt_traceall){
				put_trace("AND '" + seta_value1 + "' AND '" + seta_value2 + "'");
			}
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
			if (tz390.opt_traceall){
				put_trace("OR '" + seta_value1 + "' OR '" + seta_value2 + "'");
			}
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
			if (tz390.opt_traceall){
				put_trace("XOR '" + seta_value1 + "' XOR '" + seta_value2 + "'");
			}
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
			if (tz390.opt_traceall){
				put_trace("SUBSTRING " + exp_stk_setc[tot_exp_stk_var - 1] + "(" + seta_value1 + "," + seta_value2 + ")");
			}
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
						if (tz390.opt_traceall){
							put_trace("STK LCLA " + lcl_set_name[var_name_index] + "(" + (seta_index-lcl_set_start[var_name_index]+1) + ")=" + lcl_seta[seta_index]);
						}
						break;
					case 2: 
						exp_stk_type[tot_exp_stk_var - 1] = var_setb_type;
						exp_stk_setb[tot_exp_stk_var - 1] = lcl_setb[setb_index];
						if (tz390.opt_traceall){
							put_trace("STK SETB = " + lcl_set_name[var_name_index] + "(" + setb_index + ")=" + lcl_setb[setb_index]);
						}
						break;
					case 3: 
						exp_stk_type[tot_exp_stk_var - 1] = var_setc_type;
						exp_stk_setc[tot_exp_stk_var - 1] = lcl_setc[setc_index];
						if (tz390.opt_traceall){
							put_trace("STK SETC = " + lcl_set_name[var_name_index] + "(" + setc_index + ")=" + lcl_setc[setc_index]);
						}
						break;
					default: 
						abort_case();
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
						if (tz390.opt_traceall){
							put_trace("STK SETA =  " + gbl_seta[seta_index]);
						}
						break;
					case 2: 
						exp_stk_type[tot_exp_stk_var - 1] = var_setb_type;
						exp_stk_setb[tot_exp_stk_var - 1] = gbl_setb[setb_index];
						if (tz390.opt_traceall){
							put_trace("STK SETB = " + gbl_setb[setb_index]);
						}
						break;
					case 3: 
						exp_stk_type[tot_exp_stk_var - 1] = var_setc_type;
						exp_stk_setc[tot_exp_stk_var - 1] = gbl_setc[setc_index];
						if (tz390.opt_traceall){
							put_trace("STK SETC = " + gbl_setc[setc_index]);
						}
						break;
					default: 
						abort_case();
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
						if (tz390.opt_traceall){
							if (var_loc == pos_loc){
								put_trace("POS PARM " + mac_call_pos_name[var_name_index] + "=" + setc_value);
							} else {
								put_trace("KEY PARM " + mac_call_kwd_name[var_name_index] + "=" + setc_value);
							}
						}
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
							if (tz390.opt_traceall){
								put_trace("SYSLIST PARM(" + var_name_index + ")=" + setc_value);
							}
						} else {
							log_error(66,"syslist reference only allowed in macro");
						}
					} else {
						setc_value = exp_stk_setc[tot_exp_stk_var - 1]; // get prev syslist(sub) value
						setc_value = get_sublist(setc_value,set_sub);
						if (tz390.opt_traceall){
							put_trace("SUBLIST PARM=" + setc_value);
						}
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
				default:
					abort_case();
				}
			} else if (exp_stk_type[tot_exp_stk_var - 2] == var_sublist_type){
				set_sub = get_seta_stack_value(-1);
				tot_exp_stk_var--;
				setc_value = exp_stk_setc[tot_exp_stk_var - 1]; // get parm value set by find_var
				setc_value = get_sublist(setc_value,set_sub);
				if (tz390.opt_traceall){
					put_trace("SUBLIST PARM=" + setc_value);
				}
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
			abort_case();
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
			if (tz390.opt_tracem){
				put_trace("COMPARE '" + setc_value1 + "' "+ exp_prev_op  + " '" + setc_value2 + "' = " + exp_stk_setb[tot_exp_stk_var -1]);
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
				abort_case();
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
			setc_value1 = "" + seta_value1; // for RPI 274 trace
			setc_value2 = "" + seta_value2; // for RPI 274 trace
		} else {
			log_error(187,"expression missing value error");
			tot_exp_stk_var = 2;
		}
		tot_exp_stk_var = tot_exp_stk_var - 2;
	}
	private int get_seta_stack_value(int offset){
		/*
		 * return seta value of stk + offset
		 * without removing
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
	private void put_setb_stack_var(){
		/*
		 * add setb_value1 to stack 
		 */
		if (inc_tot_exp_stk_var()){
			exp_stk_type[tot_exp_stk_var - 1] = var_setb_type;
			exp_stk_setb[tot_exp_stk_var - 1] = setb_value1;
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
				abort_case();
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
				abort_case();
			}
			setc_value1 = "" + setb_value1; // for RPI 274 trace
			setc_value2 = "" + setb_value2; // for RPI 274 trace
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
		 * and remove it
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
				abort_case();
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
			put_trace("PUSHING OP - " + exp_token + " FROM=" + exp_text.substring(exp_next_index-1));
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
		if (tz390.opt_traceall){
			put_trace("POP OP=" + exp_stk_op[tot_exp_stk_op]);
		}
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
			exp_parse_set_mode = false;
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
					abort_case();
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
					abort_case();
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
					abort_case();
				}
				break;
			}
			exp_end = true;
			exp_ok  = true;
		} else {
			log_error(35,"expression parsing error - total stack values=" + tot_exp_stk_var + "  total ops=" + tot_exp_stk_op); // RPI 260
		}
	}
	private int get_int_from_string(String setc_text,int base){
		/*
		 * return integer from string using specified base
		 * Notes:
		 *   1.  If string has no digits, return 0 default
		 *   2.  If base 10 and string starts with ordinary
		 *       symbol, return the symbol value.
		 */
		int index = mz390_find_sym(setc_text);
		if (index != -1){
			return az390.sym_loc[index];
		}
		try {
			return Integer.valueOf(setc_text,base).intValue();
		} catch (Exception e) {
			if (base == 10){
				index = 0;
				int value = 0;
				while (index < setc_text.length()){
					if (setc_text.charAt(index) >= '0' && setc_text.charAt(index) <= '9'){
						value = value*10 + setc_text.charAt(index) - 0x30;
					} else {
						return value;
					}
					index++;
				}
				return value;
			} else {
				log_error(123,"invalid hex string - " + setc_text);
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
		 * 	Notes:
		 *    1.  If exp_parse_set_mode, set exp_parse_set_name and exit.
		 */
		int index2 = 0;
		if (tz390.opt_traceall){
			index2 = exp_next_index-exp_token.length();
			if (index2 < 0)index2 = 0;
			put_trace("PUSHING VAR - " + exp_token+ " FROM=" + exp_text.substring(index2));
		}
		if (find_var(exp_token)){  // find set or parm var
			if (exp_parse_set_mode && exp_level == 0){  // RPI 345
				exp_parse_set_mode = false;
				exp_end = true;
				exp_ok = true;
				return;
			}
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
						abort_case();
					}
					if (tz390.opt_traceall){
						index2 = exp_next_index-exp_token.length();
						if (index2 < 0)index2 = 0;
						put_trace("STRING CONCAT - " + exp_token + " = " + exp_stk_setc[tot_exp_stk_var-1]+ " FROM=" + exp_text.substring(index2));
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
						abort_case();
					}
					exp_stk_type[tot_exp_stk_var - 1] = var_type;
				}
				if (exp_next_char() == '.'){
					skip_next_token();  // skip trailing .
				}
			}
		} else {
			if (exp_parse_set_mode){  // RPI 345
				exp_end = true;
				return;
			}
			log_error(24,"undefined macro variable - " + exp_token);
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
			put_trace("PUSHING SDT - " + sdt);
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
		if (exp_next_char() == '\''){ // RPI 421
			exp_token = "DUP";
			exp_push_op();
		}
		exp_var_last = true; 
	}
	private void push_sym(){
		/*
		 * push current exp_token symbol  on stack
		 * as setc for use by prefix operators T', L'
		 * else get sym_val else 0.
		 * 
		 */
		if (exp_prev_class == exp_class_oper
			|| (exp_prev_class == exp_class_open
				&& tot_exp_stk_op > 1
				&& exp_stk_op_class[tot_exp_stk_op-2] == exp_class_oper)){
			exp_push_string(exp_token);
		} else {
			if (tz390.opt_asm){
				int index = mz390_find_sym(exp_token);
				if (index >= 0){
					seta_value1 = az390.sym_loc[index];
					put_seta_stack_var();
				} else {
					exp_push_string(exp_token);
				}
			} else {
				exp_push_string(exp_token);
			}
		}
	}
	private int mz390_find_sym(String symbol){
		/*
		 * find ordinary symbol and 
		 * return index else -1
		 * Notes:
		 *  1.  return -1 if not opt_asm
		 *  2.  Force az390 to finish last bal
		 *      and lock az390 until next bal
		 *      while mz390 accesses symbol table.
		 */
		if (symbol == null
			|| symbol.length() == 0
			){
			return -1;
		}
		int index = symbol.indexOf('.');
		if (index > 0){
			// skip labelled using if found
			symbol = symbol.substring(index+1); // RPI 419
		}
		symbol_match = symbol_pattern.matcher(symbol); 
		if (symbol_match.find()){
			symbol = symbol_match.group();
		} else {
			return -1;
		}
		if (!tz390.opt_asm){ // RPI 427
			log_error(190,"ordinary symbol reference requires ASM option - " + symbol);
			return -1;
	    } else if (!az390.az390_running){
			abort_error(189,"mz390 aborting due to az390 abort");
		}
		if (tz390.opt_traceall){
			put_trace(" MZ390 CALLING AZ390 SYM LOCK");
		}
		if (!az390.lookahead){
			az390.set_sym_lock();
		}
        index = az390.az390_find_sym(symbol);
        if (!az390.lookahead){
        	az390.reset_sym_lock();
        }
        return index;
	}
	private void set_sym_macro_attr(String sym_lab){
		/*
		 * set macro call label ordinary symbol type
		 * to 'M' if currently undefined
		 */
		if (!tz390.opt_asm)return;
		if (tz390.opt_traceall){
			put_trace("define type M macro label");
		}
		int index = mz390_find_sym(sym_lab);
		if (index == -1){
			index = az390.add_sym(sym_lab);
		}
		if (index >= 0){ 
			if ( (az390.sym_def[index] == az390.sym_def_lookahead
				  || 
				  az390.sym_def[index] == az390.sym_def_ref
				 )
				&&
				 (az390.sym_attr[index] == tz390.ascii_to_ebcdic['U']
				  || az390.sym_attr[index] == tz390.ascii_to_ebcdic['M']
				 )
				){ // RPI 415 lookahead sym def
				az390.sym_attr[index] = tz390.ascii_to_ebcdic['M'];
				az390.sym_def[index] = az390.sym_def_lookahead;
			}
		} else {
			abort_error(188,"symbol table overflow adding " + sym_lab);
		}
	}
	private int get_sym_len(String symbol){
		/*
		 * return length for ordinary symbol if found
		 * else return 1
		 */
		if (!tz390.opt_asm){
			return 1;
		}
		int cur_sym = mz390_find_sym(symbol);
		if (cur_sym != -1){
			if (az390.sym_attr[cur_sym] == tz390.ascii_to_ebcdic['J']){
				return 1; // RPI 415
			} else {
				return az390.sym_len[cur_sym];
			}
		} else {
			return 1;
		}
	}
	private int get_sym_def(String symbol){
		/*
		 * return 1 if symbol defined else 0
		 */
		if (!tz390.opt_asm){
			return 0;  
		}
		int cur_sym = mz390_find_sym(symbol);
		if (cur_sym != -1 && az390.sym_def[cur_sym] > az390.sym_def_ref){
			return 1;
		} else {
			return 0;
		}
	}
	private void exp_push_string(String value){
		/*
		 * push string on stack as setc
		 */
		if (exp_first_sym_index == -1){
			if (value.length() > 0 && tz390.opt_asm){
				exp_first_sym_index = mz390_find_sym(value);
			}
		}
		if (inc_tot_exp_stk_var()){
			exp_stk_type[tot_exp_stk_var-1] = var_setc_type;
			exp_stk_setc[tot_exp_stk_var-1] = value;
		}
	}
	private int add_lcl_set(String new_name,byte new_type,int new_size){
		/*
		 * add lcl set variable not found by find_set
		 * 
		 */
		if (tot_lcl_name >= tz390.opt_maxsym){
			abort_error(43,"maximum local variables exceeded");
			return -1;
		}
		var_name_index = tot_lcl_name;
		add_lcl_key_index(tot_lcl_name);
		tot_lcl_name++;
		if (tot_lcl_name > hwm_lcl_name){
			hwm_lcl_name = tot_lcl_name;
		}
		var_loc   = lcl_loc;
		var_type  = new_type;
		set_name  = new_name.toUpperCase();
		lcl_set_name[var_name_index] = set_name;
		lcl_set_type[var_name_index] = var_type;
		lcl_set_high[var_name_index] = 0; // RPI 343
		switch (var_type){
		case 1:  // lcl seta
			if (new_size < 1 
					|| tot_lcl_seta + new_size >= tz390.opt_maxlcl){
				abort_error(44,"lcla size out of range " + set_name + "(" + new_size + ")");
				return -1;
			}
			lcl_set_start[var_name_index] = tot_lcl_seta;
			seta_index = tot_lcl_seta;
			tot_lcl_seta = tot_lcl_seta + new_size;
			if (tot_lcl_seta > hwm_lcl_seta){
				hwm_lcl_seta = tot_lcl_seta;
			}
			lcl_set_end[var_name_index]   = tot_lcl_seta;
			seta_value = 0;
			if  (seta_index < tot_lcl_seta){
				Arrays.fill(lcl_seta,seta_index,tot_lcl_seta,seta_value);
			}
			seta_index = lcl_set_start[var_name_index];
			if (tz390.opt_traceall){
				put_trace("LCLA " + lcl_set_name[var_name_index] + "(" + new_size + ")");
			}
			break;
		case 2:  // lcl setb
			if (new_size < 1 
					|| tot_lcl_setb + new_size >= tz390.opt_maxlcl){
				abort_error(45,"lclb size out of range " + set_name + "(" + new_size + ")");
				return -1;
			}
			lcl_set_start[var_name_index] = tot_lcl_setb;
			setb_index = tot_lcl_setb;
			tot_lcl_setb = tot_lcl_setb + new_size;
			if (tot_lcl_setb > hwm_lcl_setb){
				hwm_lcl_setb = tot_lcl_setb;
			}
			lcl_set_end[var_name_index]   = tot_lcl_setb;
			setb_value = 0;
			if  (setb_index < tot_lcl_setb){
				Arrays.fill(lcl_setb,setb_index,tot_lcl_setb,setb_value);
			}
			setb_index = lcl_set_start[var_name_index];
			if (tz390.opt_traceall){
				put_trace("LCLB " + lcl_set_name[var_name_index] + "(" + new_size + ")");
			}
			break;
		case 3:  // lcl setc
			if (new_size < 1 
					|| tot_lcl_setc + new_size >= tz390.opt_maxlcl){
				abort_error(46,"lclc size out of range " + set_name + "(" + new_size + ")");
				return -1;
			}
			lcl_set_start[var_name_index] = tot_lcl_setc;
			setc_index = tot_lcl_setc;
			tot_lcl_setc = tot_lcl_setc + new_size;
			if (tot_lcl_setc > hwm_lcl_setc){
				hwm_lcl_setc = tot_lcl_setc;
			}
			lcl_set_end[var_name_index]   = tot_lcl_setc;
			setc_value = "";
			if  (setc_index < tot_lcl_setc){
				Arrays.fill(lcl_setc,setc_index,tot_lcl_setc,setc_value);
			}
			setc_index = lcl_set_start[var_name_index];
			if (tz390.opt_traceall){
				put_trace("LCLC " + lcl_set_name[var_name_index] + "(" + new_size + ")");
			}
			break;
		default: 
			abort_case();
		return -1;
		}
		return var_name_index;
	}
	private void add_gbl_set(String new_name,byte new_type,int new_size){
		/*
		 * add gbl set variable 
		 */
		if (tot_gbl_name >= tz390.opt_maxsym){
			abort_error(55,"maximum global variables exceeded");
			return;
		}
		var_name_index = tot_gbl_name;
		if (!tz390.add_key_index(var_name_index)){
			abort_error(174,"key search table exceeded adding " + new_name);
		}
		tot_gbl_name++;
		var_loc   = gbl_loc;
		var_type  = new_type;
		set_name  = new_name.toUpperCase();
		gbl_set_name[var_name_index] = set_name;
		gbl_set_type[var_name_index] = var_type;
		gbl_set_high[var_name_index] = 0;
		switch (var_type){
		case 1:  // gbl seta
			if (new_size < 1 
					|| tot_gbl_seta + new_size >= tz390.opt_maxgbl){
				abort_error(56,"gbla size out of range " + set_name + "(" + new_size + ")");
				return;
			}
			gbl_set_start[var_name_index] = tot_gbl_seta;
			seta_index = tot_gbl_seta;
			tot_gbl_seta = tot_gbl_seta + new_size;
			gbl_set_end[var_name_index]   = tot_gbl_seta;
			seta_value = 0;
			if  (seta_index < tot_gbl_seta){
				Arrays.fill(gbl_seta,seta_index,tot_gbl_seta,seta_value);
			}
			seta_index = gbl_set_start[var_name_index];
			if (tz390.opt_traceall){
				put_trace("GBLA " + gbl_set_name[var_name_index] + "(" + new_size + ")");
			}
			break;
		case 2:  // gbl setb
			if (new_size < 1 
					|| tot_gbl_setb + new_size >= tz390.opt_maxgbl){
				abort_error(57,"gblb size out of range " + set_name + "(" + new_size + ")");
				return;
			}
			gbl_set_start[var_name_index] = tot_gbl_setb;
			setb_index = tot_gbl_setb;
			tot_gbl_setb = tot_gbl_setb + new_size;
			gbl_set_end[var_name_index]   = tot_gbl_setb;
			setb_value = 0;
			if  (setb_index < tot_gbl_setb){
				Arrays.fill(gbl_setb,setb_index,tot_gbl_setb,setb_value);
			}
			setb_index = gbl_set_start[var_name_index];
			if (tz390.opt_traceall){
				put_trace("GBLB " + gbl_set_name[var_name_index] + "(" + new_size + ")");
			}
			break;
		case 3:  // gbl setc
			if (new_size < 1 
					|| tot_gbl_setc + new_size >= tz390.opt_maxgbl){
				abort_error(58,"gblc size out of range " + set_name + "(" + new_size + ")");
				return;
			}
			gbl_set_start[var_name_index] = tot_gbl_setc;
			setc_index = tot_gbl_setc;
			tot_gbl_setc = tot_gbl_setc + new_size;
			gbl_set_end[var_name_index]   = tot_gbl_setc;
			setc_value = "";
			if  (setc_index < tot_gbl_setc){
				Arrays.fill(gbl_setc,setc_index,tot_gbl_setc,setc_value);
			}
			setc_index = gbl_set_start[var_name_index];
			if (tz390.opt_traceall){
				put_trace("GBLC " + gbl_set_name[var_name_index] + "(" + new_size + ")");
			}
			break;
		default: 
			abort_case();
		}
	}
	private boolean parse_set_var(String text,int text_index){
		/*
		 * parse scalar, subscripted, or created set
		 * variable with or without subscript using
		 * expression parser in parse_set_var_mode
		 * to set:
		 *  1.  exp_parse_set_name
		 *  2.  exp_parse_set_name_index
		 *  3.  exp_parse_set_type (seta/setb/setc)
		 *  4.  exp_parse_set_loc  (lcl/gbl)
		 *  5.  exp_parse_set_sub 
		 * and return true if it exists or false if not.
		 * Notes:
		 *  1. If var found but exp_parse_set_name
		 *     is null, then issue error for parms
		 * 
		 */
		exp_parse_set_mode = true;
		exp_parse_set_name = null;
		exp_parse_set_name_index = -1;
		exp_parse_set_loc  = 0;
		exp_parse_set_type = 0;
		calc_exp(text,text_index);
		boolean save_exp_ok = exp_ok;
		if (exp_next_index < text.length() 
				&& text.charAt(exp_next_index) == '('){
			exp_parse_set_sub = calc_seta_exp(text,exp_next_index + 1);
			exp_next_index++;  // skip subscript )
		} else {
			exp_parse_set_sub = 1;
		}
		if (save_exp_ok){
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
		}
		if (find_lcl_set(var_name,var_sub)){
			if (exp_parse_set_name_index == -1 && exp_level == 0){ // RPI 345
				exp_parse_set_name_index = var_name_index;
				exp_parse_set_loc = lcl_loc;
				exp_parse_set_type = lcl_set_type[var_name_index];
			}
			return true;
		}
		if (find_gbl_set(var_name,var_sub)){
			if (exp_parse_set_name_index == -1 && exp_level == 0){ // RPI 345
				exp_parse_set_name_index = var_name_index;
				exp_parse_set_loc = gbl_loc;
				exp_parse_set_type = gbl_set_type[var_name_index];
			}
			return true;
		}
		var_name_index = -1;
		return false;
	}
	private boolean find_lcl_set(String var_name,int var_sub){
		/*
		 * find lcl variable or label else false
		 * also set var_name_index = -1 if not found
		 * set following globals if found
		 * 1.  var_loc   = lcl_loc or gbl_loc
		 * 2.  var_type  = var_seta_type|var_setb_type|var_setc_type
		 * 3.  var_name_index = for lcl/gbl seta, setb, setc array 
		 * 4.  set_sub = subscript
		 * 5.  seta_value|setb_value|setc_value
		 * 6.  seta_index|setb_index|setc_index
		 * 
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
		var_name_index = tz390.find_key_index('G',var_name);
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
			if (seta_index >= lcl_set_end[var_name_index]){
				seta_index = expand_set(var_name_index,var_seta_type,lcl_loc,set_sub);
			} else if (seta_index < lcl_set_start[var_name_index]){
				abort_error(164,"lcla subscript < 1 = " + lcl_set_name[var_name_index]
				                                                       + "(" + (seta_index-lcl_set_start[var_name_index]+1) + ")" );
			}
			seta_value = lcl_seta[seta_index];
			break;
		case 2:
			setb_index = lcl_set_start[var_name_index] + set_sub - 1;
			if (setb_index >= lcl_set_end[var_name_index]){
				setb_index = expand_set(var_name_index,var_setb_type,lcl_loc,set_sub);
			} else if (setb_index < lcl_set_start[var_name_index]){
				abort_error(165,"lclb subscript < 1 = " + lcl_set_name[var_name_index]
				                                                       + "(" + (setb_index-lcl_set_start[var_name_index]+1) + ")" );
			}
			setb_value = lcl_setb[setb_index];
			break;
		case 3:
			setc_index = lcl_set_start[var_name_index] + set_sub - 1;
			if (setc_index >= lcl_set_end[var_name_index]){
				setc_index = expand_set(var_name_index,var_setc_type,lcl_loc,set_sub);
			} else if (setc_index < lcl_set_start[var_name_index]){
				abort_error(166,"lclc subscript < 1 = " + lcl_set_name[var_name_index]
				                                                       + "(" + (setc_index-lcl_set_start[var_name_index]+1) + ")" );
			}
			setc_value = lcl_setc[setc_index];
			break;
		default: 
			abort_case();
		}
	}
	private int expand_set(int expand_name_index,byte expand_type,byte expand_loc,int expand_sub){
		/*
		 * expand set array
		 */
		tot_expand++;
		if (expand_loc == lcl_loc){
			switch (expand_type){
			case 1:
				if (tot_lcl_seta + expand_sub + expand_inc > tz390.opt_maxlcl){
					abort_error(48,"lcl seta sub out of range - " + lcl_set_name[expand_name_index] + "(" + expand_sub +")");
					return -1;
				}
				// move existing elements to end if not already there
				if (lcl_set_end[expand_name_index] != tot_lcl_seta){
					int index = lcl_set_start[expand_name_index];
					lcl_set_start[expand_name_index] = tot_lcl_seta;
					if (lcl_set_high[expand_name_index] > 0){
						lcl_set_high[expand_name_index] = 
							lcl_set_high[expand_name_index] 
							             + tot_lcl_seta - index;
					}
					System.arraycopy(lcl_seta,index,lcl_seta,tot_lcl_seta,lcl_set_end[expand_name_index]-index);
					tot_lcl_seta = tot_lcl_seta + (lcl_set_end[expand_name_index] - index); // RPI 415
					lcl_set_end[expand_name_index] = tot_lcl_seta; 
				}
				// expand array to include set_sub + expand_inc
				tot_lcl_seta = lcl_set_start[expand_name_index] + expand_sub + expand_inc;
				adjust_expand_inc(lcl_loc); // grow to reduce repeats
				int index = lcl_set_end[expand_name_index];
				Arrays.fill(lcl_seta,index,tot_lcl_seta,0);
				lcl_set_end[expand_name_index] = tot_lcl_seta;
				return lcl_set_start[expand_name_index] 
				                     + expand_sub - 1;
			case 2:
				if (tot_lcl_setb + expand_sub + expand_inc > tz390.opt_maxlcl){
					abort_error(49,"lcl setb sub out of range - " 
							+ lcl_set_name[expand_name_index] 
							               + "(" + expand_sub +")");
					return -1;
				}
				// move existing elements to end if not already there
				if (lcl_set_end[expand_name_index] != tot_lcl_setb){
					index = lcl_set_start[expand_name_index];
					lcl_set_start[expand_name_index] = tot_lcl_setb;
					if (lcl_set_high[expand_name_index] > 0){
						lcl_set_high[expand_name_index] = 
							lcl_set_high[expand_name_index] 
							             + tot_lcl_setb - index;
					}
					System.arraycopy(lcl_setb,index,lcl_setb,tot_lcl_setb,lcl_set_end[expand_name_index]-index);
					tot_lcl_setb = tot_lcl_setb + (lcl_set_end[expand_name_index] - index); // RPI 415
					lcl_set_end[expand_name_index] = tot_lcl_setb; 
				}
				// expand array to include set_sub + expand_inc
				tot_lcl_setb = lcl_set_start[expand_name_index] + expand_sub + expand_inc;
				adjust_expand_inc(lcl_loc); // grow to reduce repeats
				index = lcl_set_end[expand_name_index];
				Arrays.fill(lcl_setb,index,tot_lcl_setb,(byte)0); // RPI 411
				lcl_set_end[expand_name_index] = tot_lcl_setb;
				return lcl_set_start[expand_name_index] 
				                     + expand_sub - 1;
			case 3:
				if (tot_lcl_setc + expand_sub + expand_inc > tz390.opt_maxlcl){
					abort_error(50,"lcl setc sub out of range - " 
							+ lcl_set_name[expand_name_index] 
							               + "(" + expand_sub +")");
					return -1;
				}
				// move existing elements to end if not already there
				if (lcl_set_end[expand_name_index] != tot_lcl_setc){
					index = lcl_set_start[expand_name_index];
					lcl_set_start[expand_name_index] = tot_lcl_setc;
					if (lcl_set_high[expand_name_index] > 0){
						lcl_set_high[expand_name_index] = 
							lcl_set_high[expand_name_index] 
							             + tot_lcl_setc - index;
					}
					System.arraycopy(lcl_setc,index,lcl_setc,tot_lcl_setc,lcl_set_end[expand_name_index]-index);
					tot_lcl_setc = tot_lcl_setc + (lcl_set_end[expand_name_index] - index); // RPI 415
					lcl_set_end[expand_name_index] = tot_lcl_setc; 
				}
				// expand array to include set_sub + expand_inc
				tot_lcl_setc = lcl_set_start[expand_name_index] + expand_sub + expand_inc;
				adjust_expand_inc(lcl_loc); // grow to reduce repeats
				index = lcl_set_end[expand_name_index];
				Arrays.fill(lcl_setc,index,tot_lcl_setc,"");
				lcl_set_end[expand_name_index] = tot_lcl_setc;
				return lcl_set_start[expand_name_index] 
				                     + expand_sub - 1;
			default:
				abort_case();
			return -1;
			}
		} else {
			switch (expand_type){
			case 1:
				if (tot_gbl_seta + expand_sub + expand_inc > tz390.opt_maxgbl){
					abort_error(59,"gbl seta sub out of range - " + gbl_set_name[expand_name_index] + "(" + expand_sub +")");
					return -1;
				}
				// move existing elements to end if not already there
				if (gbl_set_end[expand_name_index] != tot_gbl_seta){
					int index = gbl_set_start[expand_name_index];
					gbl_set_start[expand_name_index] = tot_gbl_seta;
					if (gbl_set_high[expand_name_index] > 0){
						gbl_set_high[expand_name_index] = 
							gbl_set_high[expand_name_index] 
							             + tot_gbl_seta - index;
					}
					System.arraycopy(gbl_seta,index,gbl_seta,tot_gbl_seta,tot_gbl_seta-index);
					gbl_set_end[expand_name_index] = tot_gbl_seta; 
				}
				// expand array to include set_sub + expand_inc
				tot_gbl_seta = gbl_set_start[expand_name_index] + expand_sub + expand_inc;
				adjust_expand_inc(gbl_loc); // grow to reduce repeats
				int index = gbl_set_end[expand_name_index];
				Arrays.fill(gbl_seta,index,tot_gbl_seta,0);
				gbl_set_end[expand_name_index] = tot_gbl_seta;
				return gbl_set_start[expand_name_index] 
				                     + expand_sub - 1;
			case 2:
				if (tot_gbl_setb + expand_sub + expand_inc > tz390.opt_maxgbl){
					abort_error(61,"gbl setb sub out of range - " 
							+ gbl_set_name[expand_name_index] 
							               + "(" + expand_sub +")");
					return -1;
				}
				// move existing elements to end if not already there
				if (gbl_set_end[expand_name_index] != tot_gbl_setb){
					index = gbl_set_start[expand_name_index];
					gbl_set_start[expand_name_index] = tot_gbl_setb;
					if (gbl_set_high[expand_name_index] > 0){
						gbl_set_high[expand_name_index] = 
							gbl_set_high[expand_name_index] 
							             + tot_gbl_setb - index;
					}
					System.arraycopy(gbl_setb,index,gbl_setb,tot_gbl_setb,tot_gbl_setb-index);
					gbl_set_end[expand_name_index] = tot_gbl_setb; 
				}
				// expand array to include set_sub + expand_inc
				tot_gbl_setb = gbl_set_start[expand_name_index] + expand_sub + expand_inc;
				adjust_expand_inc(gbl_loc); // grow to reduce repeats
				index = gbl_set_end[expand_name_index];
				Arrays.fill(gbl_setb,index,tot_gbl_setb,(byte)0);
				gbl_set_end[expand_name_index] = tot_gbl_setb;
				return gbl_set_start[expand_name_index] 
				                     + expand_sub - 1;
			case 3:
				if (tot_gbl_setc + expand_sub + expand_inc > tz390.opt_maxgbl){
					abort_error(65,"gbl setc sub out of range - " 
							+ gbl_set_name[expand_name_index] 
							               + "(" + expand_sub +")");
					return -1;
				}
				// move existing elements to end if not already there
				if (gbl_set_end[expand_name_index] != tot_gbl_setc){
					index = gbl_set_start[expand_name_index];
					gbl_set_start[expand_name_index] = tot_gbl_setc;
					if (gbl_set_high[expand_name_index] > 0){
						gbl_set_high[expand_name_index] = 
							gbl_set_high[expand_name_index] 
							             + tot_gbl_setb - index;
					}
					System.arraycopy(gbl_setc,index,gbl_setc,tot_gbl_setc,tot_gbl_setc-index);
					gbl_set_end[expand_name_index] = tot_gbl_setc; 
				}
				// expand array to include set_sub + expand_inc
				tot_gbl_setc = gbl_set_start[expand_name_index] + expand_sub + expand_inc;
				adjust_expand_inc(gbl_loc); // grow to reduce repeats
				index = gbl_set_end[expand_name_index];
				Arrays.fill(gbl_setc,index,tot_gbl_setc,""); // RPI 411
				gbl_set_end[expand_name_index] = tot_gbl_setc;
				return gbl_set_start[expand_name_index] 
				                     + expand_sub - 1;
			default:
				abort_case();
			return -1;
			}
		}
	}
	private void adjust_expand_inc(int var_loc){
		/*
		 * increase expansion increment to reduce
		 * overhead of repeated expansions.  This
		 * is a trade-off with running out of memory
		 */
		expand_inc = 10; 
		if (tz390.opt_traceall){
			if (var_loc == lcl_loc){
				put_trace("EXPANSION OF LCL " + lcl_set_name[var_name_index] + "(" + (lcl_set_end[var_name_index]-lcl_set_start[var_name_index]) + ") INC=" + expand_inc);
			} else {
				put_trace("EXPANSION OF GBL " + gbl_set_name[var_name_index] + "(" + (gbl_set_end[var_name_index]-gbl_set_start[var_name_index]) + ") INC=" + expand_inc);
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
			if (seta_index >= gbl_set_end[var_name_index]){
				seta_index = expand_set(var_name_index,var_seta_type,gbl_loc,set_sub);
			} else if (seta_index < gbl_set_start[var_name_index]){
				abort_error(167,"gbla subscript < 1 = " + gbl_set_name[var_name_index]
				                                                       + "(" + (seta_index-gbl_set_start[var_name_index]+1) + ")" );
			}
			seta_value = gbl_seta[seta_index];
			break;
		case 2:
			setb_index = gbl_set_start[var_name_index] + set_sub - 1;
			if (setb_index >= gbl_set_end[var_name_index]){
				setb_index = expand_set(var_name_index,var_setb_type,gbl_loc,set_sub);
			} else if (setb_index < gbl_set_start[var_name_index]){
				abort_error(168,"gblb subscript < 1 = " + gbl_set_name[var_name_index]
				                                                       + "(" + (setb_index-gbl_set_start[var_name_index]+1) + ")" );
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
			if (setc_index >= gbl_set_end[var_name_index]){
				setc_index = expand_set(var_name_index,var_setc_type,gbl_loc,set_sub);
			} else if (setc_index < gbl_set_start[var_name_index]){
				abort_error(169,"gblc subscript < 1 = " + gbl_set_name[var_name_index]
				                                                       + "(" + (setc_index-gbl_set_start[var_name_index]+1) + ")" );
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
			abort_case();
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
			int    label_name_index = find_lcl_key_index("B:" + label_name);
			if (label_name_index != -1){
				return mac_lab_index[label_name_index]-1; // -1 req'd for following ++ cycle
			}
			label_name_index = mac_name_lab_start[mac_name_index];
			while (label_name_index < mac_name_lab_end[mac_name_index]){
				if (mac_lab_name[label_name_index].equals(label_name)){
					if (tz390.opt_tracem){
						put_trace("BRANCH " + label_name);
					}
					add_lcl_key_index(label_name_index);
					return mac_lab_index[label_name_index]-1; // -1 req'd for following ++ cycle
				} else {
					label_name_index++;
				}
			}
		}
		abort_error(25,"macro label not found - " + label_name);
		return -1;
	}
	private int get_label_comma_index(String label_source){
		/*
		 * find and return index to comma after
		 * macro label else return -1
		 */
		label_match = label_pattern.matcher(label_source); 
		if (label_match.find()){
			int index = label_match.end();
			if (index < label_source.length()
					&& label_source.charAt(index) == ','){
				return index;
			}
		}
		return -1;
	}
	private int find_mac_entry(String macro_name){
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
		int index = tz390.find_key_index('M',macro_name.toUpperCase());
		if (index != -1){
			if (index == -2){
				check_sysops();
			}
			return index;
		}
		if (tz390.opt_asm){
			String temp_name = macro_name.toUpperCase();
			if (save_opsyn_index == -1 || tz390.opsyn_old_name[save_opsyn_index] != null){
				index = tz390.find_key_index('O',temp_name);
			} else {
				index = -1; // RPI 331
			}
			if (index != -1){
				if (tz390.find_key_index('M',temp_name) == -1){
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
		 * 4. Set macro label ordinary symbol type
		 *    to 'M' if 'U'.
		 */
		tot_mac_call++;
		gbl_seta[gbl_sysm_sev_index] = 0;
		mac_name_index = find_mac_name_index;
		int save_mac_line_index = mac_line_index;
		mac_call_return[mac_call_level] = mac_line_index + 1;
		mac_call_actr[mac_call_level] = actr_count;
		actr_count = actr_limit;
		if  (mac_call_level < tz390.opt_maxcall-1){ // RPI 284
			mac_call_level++;
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
			Arrays.fill(lcl_key_tab_key,lcl_key_index,tot_lcl_key_tab,null); // RPI 411
			/*
			 * add lcl system variables 
			 */
			init_lcl_sys();
			if  (tz390.opt_listcall){
				String call_label = bal_label;
				String call_parms = bal_parms;
				if (call_label == null){
					call_label = "        ";
				} else {
					if (call_label.length() > 1 && call_label.charAt(0) == '.'){
						call_label = tz390.left_justify(" ",call_label.length());
					}
					call_label = tz390.left_justify(call_label,8);
				}
				String call_op = save_bal_op;
				call_op = tz390.left_justify(call_op,5);
				String sysndx = tz390.right_justify("" + lcl_sysndx,4);
				String sysnest = tz390.right_justify("" + mac_call_level,2);
				if (call_parms == null)call_parms = "";
				String call_line_no = tz390.right_justify("" + mac_file_line_num[save_mac_line_index],6);
				String call_line = null;
				if (tz390.opt_reformat){
					call_line = call_label + " " + call_op + " " + call_parms;
				} else { 
					if (bal_label != null && bal_label.length() > 1 && bal_label.charAt(0) == '.'){
						call_line = tz390.left_justify(" ",bal_label.length()) + bal_line.substring(bal_label.length());
					} else {
						call_line = bal_line;
					}
				}
				put_bal_line("*MCALL #=" + sysndx + " LV=" +  sysnest + " LN=" + call_line_no + " " + call_line);
			}
			/*
			 * 1.  parse proto-type and add initial
			 *     key word parms with values
			 * 2.  parse macro call statement and set
			 *     positional and key word parm values
			 * 
			 */
			init_call_parms();
			set_call_parm_values();
			mac_line_index = mac_name_line_start[mac_name_index];
			update_sysstmt();
		} else {
			abort_error(30,"max level of nested macros exceeded");
		}
	}
	private void init_arrays(){
		/*
		 * init large arrays with optional
		 * overides for size
		 */
		/* 
		 * opt_maxcall - maximum nested call stack
		 */
		mac_call_name_index = (int[])Array.newInstance(int.class,tz390.opt_maxcall);
		mac_call_return = (int[])Array.newInstance(int.class,tz390.opt_maxcall);
		mac_call_actr   = (int[])Array.newInstance(int.class,tz390.opt_maxcall);
		mac_call_pos_start = (int[])Array.newInstance(int.class,tz390.opt_maxcall);
		mac_call_pos_tot   = (int[])Array.newInstance(int.class,tz390.opt_maxcall);
		mac_call_kwd_start = (int[])Array.newInstance(int.class,tz390.opt_maxcall);
		mac_call_lcl_name_start = (int[])Array.newInstance(int.class,tz390.opt_maxcall);
		mac_call_lcl_seta_start = (int[])Array.newInstance(int.class,tz390.opt_maxcall);
		mac_call_lcl_setb_start = (int[])Array.newInstance(int.class,tz390.opt_maxcall);
		mac_call_lcl_setc_start = (int[])Array.newInstance(int.class,tz390.opt_maxcall);
		mac_call_lcl_key_start  = (int[])Array.newInstance(int.class,tz390.opt_maxcall);
		mac_call_lcl_key_root   = (int[])Array.newInstance(int.class,tz390.opt_maxcall);
		/*
		 * opt_maxfile - nested copy file I/O plus 
		 * reference File with file paths for cross reference
		 */
		mac_file_name         = new String[tz390.opt_maxfile];
		mac_file              = (File[])Array.newInstance(File.class,tz390.opt_maxfile);
		mac_file_buff         = (BufferedReader[])Array.newInstance(BufferedReader.class,tz390.opt_maxfile);
		mac_file_cur_file_num = (int[])Array.newInstance(int.class,tz390.opt_maxfile);
		mac_file_cur_line_num = (int[])Array.newInstance(int.class,tz390.opt_maxfile);
		mac_file_errors       = (int[])Array.newInstance(int.class,tz390.opt_maxfile);
		mac_name              = new String[tz390.opt_maxfile];
		mac_name_line_start   =(int[])Array.newInstance(int.class,tz390.opt_maxfile);
		mac_name_line_end   =(int[])Array.newInstance(int.class,tz390.opt_maxfile);
		mac_name_lab_start  = (int[])Array.newInstance(int.class,tz390.opt_maxfile);
		mac_name_lab_end    = (int[])Array.newInstance(int.class,tz390.opt_maxfile);
		/*
		 * opt_maxline - total MLC and MAC/CPY file source loaded
		 */
		mac_file_line   = new String[tz390.opt_maxline];
		mac_file_line_num = (int[])Array.newInstance(int.class,tz390.opt_maxline);
		mac_file_name_num = (int[])Array.newInstance(int.class,tz390.opt_maxline);
		mac_file_line[0] = "";
		/*
		 * opt_maxgbl - global set variable names and cells
		 */
		gbl_set_name  = new String[tz390.opt_maxgbl]; 
		gbl_set_type  = (byte[])Array.newInstance(byte.class,tz390.opt_maxgbl);
		gbl_set_start = (int[])Array.newInstance(int.class,tz390.opt_maxgbl);
		gbl_set_high = (int[])Array.newInstance(int.class,tz390.opt_maxgbl);
		gbl_set_end   = (int[])Array.newInstance(int.class,tz390.opt_maxgbl);
		gbl_seta      = (int[])Array.newInstance(int.class,tz390.opt_maxgbl);
		gbl_setb      = (byte[])Array.newInstance(byte.class,tz390.opt_maxgbl);
		gbl_setc      = new String[tz390.opt_maxgbl]; 
		/*
		 * opt_maxkey - maximum indexed key table
		 */
		max_lcl_key_tab = tz390.opt_maxcall * max_lcl_key_root + tz390.opt_maxlcl + 2 * tz390.opt_maxparm;
		lcl_key_tab_key   = new String[max_lcl_key_tab];
		lcl_key_tab_hash  = (int[])Array.newInstance(int.class,max_lcl_key_tab);
		lcl_key_tab_index = (int[])Array.newInstance(int.class,max_lcl_key_tab);
		lcl_key_tab_low   = (int[])Array.newInstance(int.class,max_lcl_key_tab);
		lcl_key_tab_high  = (int[])Array.newInstance(int.class,max_lcl_key_tab);
		/*
		 * opt_maxlcl - local set variable names and cells
		 */
		lcl_set_name  = new String[tz390.opt_maxlcl]; 
		lcl_set_type  = (byte[])Array.newInstance(byte.class,tz390.opt_maxlcl);
		lcl_set_start = (int[])Array.newInstance(int.class,tz390.opt_maxlcl);
		lcl_set_high  = (int[])Array.newInstance(int.class,tz390.opt_maxlcl);
		lcl_set_end   = (int[])Array.newInstance(int.class,tz390.opt_maxlcl);
		lcl_seta      = (int[])Array.newInstance(int.class,tz390.opt_maxlcl);
		lcl_setb      = (byte[])Array.newInstance(byte.class,tz390.opt_maxlcl);
		lcl_setc      = new String[tz390.opt_maxlcl]; 
		/*
		 * opt_maxparm = positional and keyword parms
		 */
		mac_call_pos_name = new String[tz390.opt_maxparm]; 
		mac_call_pos_parm = new String[tz390.opt_maxparm]; 
		mac_call_kwd_name = new String[tz390.opt_maxparm]; 
		mac_call_kwd_parm = new String[tz390.opt_maxparm];
		/*
		 * opt_maxsym - symbols, macro labels, 
		 */
		mac_lab_name  = new String[tz390.opt_maxsym]; 
		mac_lab_index = (int[])Array.newInstance(int.class,tz390.opt_maxsym);
		mac_lab_num   = (int[])Array.newInstance(int.class,tz390.opt_maxsym); // RPI 266
	}
	private void init_gbl_sys(){
		/*
		 * add global system variables
		 */
		add_gbl_sys("&SYSADATA_DSN",var_setc_type); // full path and file name if any RPI 259
		set_sys_dsn_mem_vol(tz390.dir_mlc + tz390.pgm_name + tz390.ada_type);
		gbl_setc[tot_gbl_setc-1] = sys_dsn;
		add_gbl_sys("&SYSADATA_MEMBER",var_setc_type); // current macro name if any
		gbl_setc[tot_gbl_setc-1] = sys_mem;
		add_gbl_sys("&SYSADATA_VOLUME",var_setc_type); // drive letter if any
		gbl_setc[tot_gbl_setc-1] = sys_vol;
		add_gbl_sys("&SYSASM",var_setc_type);
		gbl_setc[tot_gbl_setc-1] = "z390";
		add_gbl_sys("&SYSCLOCK",var_setc_type);
		gbl_sysclock_index = tot_gbl_setc-1;
		add_gbl_sys("&SYSDATC",var_setc_type);
		gbl_setc[tot_gbl_setc-1] = sdf_sysdatc.format(cur_date);
		add_gbl_sys("&SYSDATE",var_setc_type);
		gbl_setc[tot_gbl_setc-1] = sdf_sysdate.format(cur_date);
		add_gbl_sys("&SYSIN_DSN",var_setc_type); // MLC full path and file name  RPI 259
		set_sys_dsn_mem_vol(tz390.dir_mlc + tz390.pgm_name + tz390.pgm_type);
		gbl_setc[tot_gbl_setc-1] = sys_dsn;
		add_gbl_sys("&SYSIN_MEMBER",var_setc_type); // MLC file name without suffix
		gbl_setc[tot_gbl_setc-1] = sys_mem;
		add_gbl_sys("&SYSIN_VOLUME",var_setc_type); // MLC drive letter
		gbl_setc[tot_gbl_setc-1] = sys_vol;
		add_gbl_sys("&SYSJOB",var_setc_type); // current job = MLC file name without suffix
		sys_job = sys_mem;
		int index = sys_job.indexOf('.');
		if (index > 0){
			sys_job = sys_mem.substring(0,index);
		}
		gbl_setc[tot_gbl_setc-1] = sys_job;
		add_gbl_sys("&SYSLIB_DSN",var_setc_type); // MAC full path and file name if any
		gbl_syslib_index = tot_gbl_setc-1;
		gbl_setc[tot_gbl_setc-1] = sys_dsn;
		add_gbl_sys("&SYSLIB_MEMBER",var_setc_type); // MAC file name without suffix
		gbl_setc[tot_gbl_setc-1] = sys_mem;
		add_gbl_sys("&SYSLIB_VOLUME",var_setc_type); // MAC drive letter if any
		gbl_setc[tot_gbl_setc-1] = sys_vol;
		add_gbl_sys("&SYSLIN_DSN",var_setc_type); // OBJ full path and file name
		set_sys_dsn_mem_vol(tz390.dir_mlc + tz390.pgm_name + tz390.obj_type);
		gbl_setc[tot_gbl_setc-1] = sys_dsn;
		add_gbl_sys("&SYSLIN_MEMBER",var_setc_type); // OBJ file name without suffix
		gbl_setc[tot_gbl_setc-1] = sys_mem;
		add_gbl_sys("&SYSLIN_VOLUME",var_setc_type); //OBJ drive letter
		gbl_setc[tot_gbl_setc-1] = sys_vol;
		add_gbl_sys("&SYSMAC",var_setc_type);
		gbl_sysmac_index = tot_gbl_setc-1;
		add_gbl_sys("&SYSM_HSEV",var_seta_type); // highest MNOTE error level
		gbl_sysm_hsev_index = tot_gbl_seta-1;
		add_gbl_sys("&SYSM_SEV",var_seta_type); // highest MNOTE level in last macro
		gbl_sysm_hsev_index = tot_gbl_seta-1;
		add_gbl_sys("&SYSOPT_DBCS",var_setb_type); // set false indicating no DBCS support
		add_gbl_sys("&SYSOPT_OPTABLE",var_setc_type); // "z390" opcode table name
		gbl_setc[tot_gbl_setc-1] = "z390";
		;	add_gbl_sys("&SYSOPT_RENT",var_setb_type); // set true if RENT option specified
		add_gbl_sys("&SYSOPT_XOBJECT",var_setb_type); //set true if GOFF or XOBJECT
		add_gbl_sys("&SYSPARM",var_setc_type);
		gbl_setc[tot_gbl_setc-1] = tz390.opt_sysparm;
		add_gbl_sys("&SYSPRINT_DSN",var_setc_type); // PRN full path and file name
		set_sys_dsn_mem_vol(tz390.dir_mlc + tz390.pgm_name + tz390.prn_type);
		gbl_setc[tot_gbl_setc-1] = sys_dsn;
		add_gbl_sys("&SYSPRINT_MEMBER",var_setc_type); // PRN file name without type
		gbl_setc[tot_gbl_setc-1] = sys_mem;
		add_gbl_sys("&SYSPRINT_VOLUME",var_setc_type); // PRN drive letter
		gbl_setc[tot_gbl_setc-1] = sys_vol;
		add_gbl_sys("&SYSPUNCH_DSN",var_setc_type); // PCH full path and file name
		set_sys_dsn_mem_vol(tz390.dir_mlc + tz390.pgm_name + tz390.pch_type);
		gbl_setc[tot_gbl_setc-1] = sys_dsn;
		add_gbl_sys("&SYSPUNCH_MEMBER",var_setc_type); //PCH file name without type
		gbl_setc[tot_gbl_setc-1] = sys_mem;
		add_gbl_sys("&SYSPUNCH_VOLUME",var_setc_type); //PCH drive letter
		gbl_setc[tot_gbl_setc-1] = sys_vol;
		add_gbl_sys("&SYSSEQF",var_setc_type); // source sequence field set to 0 length string
		add_gbl_sys("&SYSSTEP",var_setc_type); // current step = MLC file name
		gbl_setc[tot_gbl_setc-1] = sys_job;
		add_gbl_sys("&SYSSTMT",var_seta_type); // next MLC statement number
		gbl_sysstmt_index = tot_gbl_seta-1;
		add_gbl_sys("&SYSTEM_ID",var_setc_type);
		gbl_setc[tot_gbl_setc-1] = 
			System.getProperty("os.name") 
			+ " " + System.getProperty("os.version");
		add_gbl_sys("&SYSTERM_DSN",var_setc_type); // ERR full path and file name
		set_sys_dsn_mem_vol(tz390.dir_mlc + tz390.pgm_name + tz390.err_type);
		gbl_setc[tot_gbl_setc-1] = sys_dsn;
		add_gbl_sys("&SYSTERM_MEMBER",var_setc_type); // ERR file name without type
		gbl_setc[tot_gbl_setc-1] = sys_mem;
		add_gbl_sys("&SYSTERM_VOLUME",var_setc_type); // ERR file drive letter
		gbl_setc[tot_gbl_setc-1] = sys_vol;
		add_gbl_sys("&SYSTEM_JAVA",var_setc_type);
		gbl_setc[tot_gbl_setc-1] = 
			System.getProperty("java.vendor") 
			+ " " + System.getProperty("java.version");
		add_gbl_sys("&SYSTIME",var_setc_type);
		gbl_setc[tot_gbl_setc-1] = sdf_systime.format(cur_date);
		add_gbl_sys("&SYSVER",var_setc_type);
		gbl_setc[tot_gbl_setc-1] = tz390.version;
	}
	private void set_sys_dsn_mem_vol(String file_name){
		/*
		 * set sys_dsn, sys__mem, and sys_vol
		 * from file name
		 */
		sys_dsn = "";
		sys_mem = "";
		sys_vol = "";
		sys_file = new File(file_name.toUpperCase());
		try {
			sys_dsn = sys_file.getCanonicalPath();
			sys_mem = sys_file.getName();
			int index = sys_mem.indexOf(".");
			if (index > 0){ // RPI 329
				sys_mem = sys_mem.substring(0,index);
			}
			sys_vol = sys_dsn.substring(0,1);
		} catch (Exception e){}
	}
	private void add_gbl_sys(String sys_name,byte sys_type){
		/*
		 * add global system variables
		 */
		if (tz390.find_key_index('G',sys_name) == -1){
			add_gbl_set(sys_name,sys_type,1);
		} else {
			abort_error(160,"add global var failed - " + sys_name);
		}
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
		 * add local set variable
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
			if (bal_op.equals("CSECT")
				|| bal_op.equals("RSECT")){
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
		cur_pos_parm = mac_call_pos_start[mac_call_level];  // rpi 313
		String proto_type_line = mac_file_line[mac_name_line_start[mac_name_index]];
		proto_label = null;
		proto_op    = null;
		proto_parms = null;
		tz390.split_line(proto_type_line);
		if (tz390.split_label != null){
			proto_label = tz390.split_label;
		} else {
			proto_label = "";
		}
		if (tz390.split_op != null){
			proto_op = tz390.split_op;
		} else {
			proto_op = "";
		}
		if (tz390.split_parms != null){
			proto_parms = tz390.split_parms;
		} else {
			proto_parms = "";
		}
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
			int first_pos_parm = tot_pos_parm;  // RPI 313
			while (proto_match.find()){
				parm_value = proto_match.group();
				switch (state){
				case 1: // new parm
					if  (parm_value.equals(",")){
						if (tot_pos_parm == first_pos_parm
								&& (proto_parms.length() == 1
										|| proto_parms.charAt(1) <= ' ')){
							state = 4;  // RPI 313
						} else {
							set_pos_parm("");
						}
					} else if (parm_value.charAt(0) <= asc_space_char){ // RPI 239
						state = 4;
					} else {
						if  (parm_value.charAt(parm_value.length()-1) == '='){
							key_name  = parm_value.substring(0,parm_value.length()-1).toUpperCase();  // RPI 359
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
					abort_case();
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
		cur_pos_parm = mac_call_pos_start[mac_call_level]; // rpi 313
		if  (bal_label.length() > 0 && bal_label.charAt(0) != '.'){
			set_sym_macro_attr(bal_label);
			set_pos_parm(bal_label);  // set syslist(0) label
		} else {
			set_pos_parm(""); // syslist(0) null
		}
		if  (bal_parms.length() > 0){
			String key_name = null;
			tz390.parm_match = tz390.parm_pattern.matcher(bal_parms);
			byte state = 1;
			String token = null;
			char   token_first = '?';
			int    token_len   = 0;
			int level = 0;
			int first_pos_parm = tot_pos_parm;
			while (tz390.parm_match.find()){
				token = tz390.parm_match.group();
				token_first = token.charAt(0);
				token_len   = token.length();
				switch (state){
				case 1: // new parm
					parm_value = "";
					if  (token_first == ','){
						if (tot_pos_parm == first_pos_parm
								&& (bal_parms.length() == 1
										|| bal_parms.charAt(1) <= ' ')){
							state = 4;  // RPI 313
						} else {
							set_pos_parm("");
						}
					} else if (token_first <= asc_space_char){ //RPI181
						state = 4;
					} else {
						state = 2; 
						level = 0;
						if  (token_len >=2 && token.charAt(token_len-1) == '='){
							key_name  = token.substring(0,token_len-1).toUpperCase();  // RPI 366 
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
					abort_case();
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
		 * init positional parm
		 */
		pos_parm_name = pos_parm_name.toUpperCase(); // RPI 366
		if (tot_pos_parm +1 > tz390.opt_maxparm){
			abort_error(144,"maximum positional parms exceeded");
		}
		mac_call_pos_name[tot_pos_parm] = pos_parm_name;
		mac_call_pos_parm[tot_pos_parm] = "";
		if (pos_parm_name.length() > 0){
			if (find_lcl_key_index("P:" + pos_parm_name) == -1){
				add_lcl_key_index(tot_pos_parm);
			} else {
				log_error(90,"duplicate positional parm - " + pos_parm_name);
			}
		}
		tot_pos_parm++;
		if (tot_pos_parm > hwm_pos_parm){
			hwm_pos_parm = tot_pos_parm;
		}
	}
	private void init_key_parm(String kwd_parm_name,String kwd_parm_value){
		/*
		 * add key work parm name and default value
		 */
		if (tot_kwd_parm +1 > tz390.opt_maxparm){
			abort_error(145,"maximum key word parms exceeded");
		}
		mac_call_kwd_name[tot_kwd_parm] = kwd_parm_name;
		mac_call_kwd_parm[tot_kwd_parm] = kwd_parm_value;
		if (kwd_parm_name.length() > 0){
			if (find_lcl_key_index("K:" + kwd_parm_name) == -1){
				add_lcl_key_index(tot_kwd_parm);
			} else {
				log_error(91,"duplicate keyword parm - " + kwd_parm_name);
			}
		}
		tot_kwd_parm++;
		if (tot_kwd_parm > hwm_kwd_parm){
			hwm_kwd_parm = tot_kwd_parm;
		}
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
		 * close files and exit to system or caller
		 */
		if (tz390.opt_asm && !tz390.z390_abort){
			if (az390.az390_running){
				call_az390_pass_bal_line(null);
				bal_eof = true;
			}
			while (tz390.opt_asm && az390.az390_running){
				sleep_now(tz390.monitor_wait);
			}
			if (az390.az390_rc > mz390_rc){
				mz390_rc = az390.az390_rc;
			}
		}		
		if  (mz390_errors > 0
	        || max_mnote_level >= 16 // RPI 410
			|| tz390.z390_abort){
			mz390_rc = 16;
		}
		if (!tz390.opt_asm){
			put_stats();
		}
		close_files();
		if (tz390.opt_asm 
			&& az390.az390_rc > mz390_rc){
			mz390_rc = az390.az390_rc;  // RPI 425
		}
		System.exit(mz390_rc);
	}
	private void put_stats(){
		/*
		 * 1.  Display mz390 statistics
		 *     as comments on BAL file prior
		 *     to END statement if option STATS.
		 * 2.  Force BAL display of files
		 *     with errors for cross reference
		 *     if nolistfile and noasm.  
		 * 3.  If asm pass file names and merge file errors
		 *     from mz390 and lookahead phase of az390
		 *     for use in file xref at end of PRN..
		 */
		log_to_bal = false;
		if  (tz390.opt_stats && tz390.opt_bal){
			log_to_bal = true;
			if  (tz390.opt_timing){
				cur_date = new Date();
				put_log("MZ390I " + tz390.version 
						+ " Current Date " +tz390.sdf_mmddyy.format(cur_date)
						+ " Time " + tz390.sdf_hhmmss.format(cur_date));
			} else {
				put_log("MZ390I " + tz390.version);
			}
			put_log("MZ390I program = " + tz390.dir_mlc + tz390.pgm_name + tz390.pgm_type);
			put_log("MZ390I options = " + tz390.cmd_parms);
			put_log("Stats total MLC/MAC loaded  = " + tot_mac_line);
			put_log("Stats total BAL output      = " + tot_bal_line);
			if (tot_aread_io + tot_punch_io > 0){
				put_log("Stats total AREAD input     = " + tot_aread_io);
				put_log("Stats total PUNCH output    = " + tot_punch_io);
			}
			put_log("Stats total instructions    = " + tot_ins);
			put_log("Stats total macros          = " + tot_mac_name);
			put_log("Stats total macro loads     = " + tot_mac_load);
			put_log("Stats total macro calls     = " + tot_mac_call);	
			put_log("Stats total global set names= " + tot_gbl_name);
			put_log("Stats tot global seta cells = " + tot_gbl_seta);
			put_log("Stats tot global setb cells = " + tot_gbl_setb);
			put_log("Stats tot global setc cells = " + tot_gbl_setc);
			put_log("Stats max local pos parms   = " + hwm_pos_parm);
			put_log("Stats max local key parms   = " + hwm_kwd_parm);
			put_log("Stats max local set names   = " + hwm_lcl_name);
			put_log("Stats max local seta cells  = " + hwm_lcl_seta);
			put_log("Stats max local setb cells  = " + hwm_lcl_setb);
			put_log("Stats max local setc cells  = " + hwm_lcl_setc);
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
		}
		int index = 0;
		while (index < tot_mac_file_name){
			if (tz390.opt_listfile && tz390.opt_bal){
				String xref_msg = "MZ390I file=" + tz390.right_justify(""+(index+1),2)
						        + " errors=" + tz390.right_justify(""+mac_file_errors[index],2)
						        + " path=" + mac_file_name[index];
			    put_log(xref_msg);
			    if (!tz390.opt_asm && mac_file_errors[index] > 0){  // RPI 425
			    	tz390.put_systerm(xref_msg);
			    }
			}
			if (tz390.opt_asm){ // RPI 426
				// transfer xref file/line to az390 for
				// combined error xref at end of PRN
				az390.tot_xref_files = tot_mac_file_name;
				az390.xref_file_name[index] = mac_file_name[index];
				az390.xref_file_errors[index] = az390.xref_file_errors[index] + mac_file_errors[index];
			}
			index++;
		}
		if  (tz390.opt_stats && tz390.opt_bal){
			put_log("MZ390I total mnote warnings = " + tot_mnote_warning); // RPI 402
			put_log("MZ390I total mnote errors   = " + tot_mnote_errors
				+ "  max level= " + gbl_seta[gbl_sysm_hsev_index]);
			put_log("MZ390I total errors         = " + mz390_errors);
			put_log("MZ390I return code(" + tz390.left_justify(tz390.pgm_name,8) + ")= " + mz390_rc); // RPI 312
			log_to_bal = false;
		}
	}
	private void close_files(){
		/*
		 * close bal, pch, systerm
		 */
		if (tz390.opt_bal){
			try {
				bal_file_buff.close();
			} catch (IOException e){
				abort_error(34,"I/O error on BAL close - " + e.toString());
			}
		}
		pch_file_index = 0;
		while (pch_file_index < max_ap_files){
			if (pch_file[pch_file_index] != null && pch_file[pch_file_index].isFile()){
				try {
					pch_file_buff[pch_file_index].close();
				} catch (IOException e){
					abort_error(77,"I/O error on PUNCH close - " + e.toString());
				}
			}
			pch_file_index++;
		}
		tz390.close_systerm(mz390_rc);
	}
	private void create_mnote(int level,String text){
		/*
		 * create mnote on BAL and ERR
		 */
		process_mnote(level,"" + level + ",'" + text + "'");
	}
	private void process_mnote(int level,String msg){
		/*
		 * put mnote message on BAL and ERR files
		 */
		if (level > tz390.max_mnote_warning){ 
			tot_mnote_errors++;
			if (!tz390.opt_asm){ // RPI 415 let az390 report mnote in seq on ERR
				tz390.put_systerm("MNOTE " + msg); // RPI 330
			}
		} else if (level > 0){
			tot_mnote_warning++;
		}
		if (level > max_mnote_level){
			max_mnote_level = level;   // RPI 410
		}
		if  (level > gbl_seta[gbl_sysm_hsev_index]){
			gbl_seta[gbl_sysm_hsev_index] = level;
		}
		if (level > gbl_seta[gbl_sysm_sev_index]){
			gbl_seta[gbl_sysm_sev_index] = level;
		}
		put_bal_line("         MNOTE " + msg);
	}
	private void log_error(int error,String msg){
		/*
		 * issue error msg to log with prefix and
		 * inc error total
		 * Notes:
		 *   1.  If exp_replacement mode errror
		 *       ignore and return setc_value = null
		 */
		if (mac_abort)return;
		mac_abort = true;
		exp_end = true;
		if (exp_var_replacement_mode){ // RPI 241
			exp_setc = null;
			return;
		}
		log_to_bal = true;
		int file_index = mac_file_name_num[mac_line_index];
		mac_file_errors[file_index]++;
		String error_msg = "MZ390E error " + error
		+ " file=" + (file_index+1)
		+ " line=" + mac_file_line_num[mac_line_index]
		+ " " + msg;
		put_log(error_msg);
		tz390.put_systerm(error_msg);
		mz390_errors++;
		if (tz390.max_errors != 0 && mz390_errors > tz390.max_errors){
			abort_error(83,"maximum errors exceeded");
		}
	}
	private void abort_error(int error,String msg){
		/*
		 * issue error msg to log with prefix and
		 * inc error total
		 */
		if (tz390.z390_abort){
			msg = "mz390 aborting due to recursive abort for " + msg;
			System.out.println(msg);;
			tz390.put_systerm(msg);
			tz390.close_systerm(16);
			System.exit(16);
		}
		mz390_errors++;
		tz390.z390_abort = true;
		log_to_bal = true;
		String err_line_and_num = "";
		if (mac_line_index < tz390.opt_maxline){
			err_line_and_num =
				" file=" + (mac_file_name_num[mac_line_index]+1)
				+ " line=" + mac_file_line_num[mac_line_index];
		}
		msg = "MZ390E error " + error 
		+ err_line_and_num
		+ " " + msg;
		put_log(msg);
		tz390.put_systerm(msg);
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
					+ " Current Date " +tz390.sdf_mmddyy.format(cur_date)
					+ " Time " + tz390.sdf_hhmmss.format(cur_date));
		} else {
			put_log("MZ390I " + tz390.version);
		}
		if  (z390_log_text == null){
			put_log("Copyright 2006 Automated Software Tools Corporation");
			put_log("z390 is licensed under GNU General Public License");
		}
		put_log("MZ390I program = " + tz390.dir_mlc + tz390.pgm_name + tz390.pgm_type);
		put_log("MZ390I options = " + tz390.cmd_parms);
	}
	private void put_trace(String msg){
		/*
		 * put trace to log with prefix info
		 */
		log_to_bal = true; 
		put_log("TRACEM " + mac_name[mac_call_name_index[mac_call_level]] + " " + msg);
	}
	private synchronized void put_log(String msg) {
		/*
		 * Write message to z390_log_text or console
		 * if running standalone
		 * 
		 */
		if  (tz390.opt_tracem
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
		 *
		 *  lcl key types are:
		 *     K: - key word macro parm
		 *     B: - local macro label
		 *     P: - postional macro parm
		 *     L: = local set variable
		 *
		 *     See tz390 with global find_key_index
		 *     types FGMORSX
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
	private void sleep_now(long mills){
		/*
		 * sleep for 1 monitor wait interval
		 */
		try {
			Thread.sleep(mills);
		} catch (Exception e){
			abort_error(186,"thread sleep error - " + e.toString());
		}
	}
}