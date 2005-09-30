import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.RandomAccessFile;
import java.lang.reflect.Array;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.MathContext;
import java.nio.ByteBuffer;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.Random;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.swing.JTextArea;
import javax.swing.JTextField;

public  class  ez390 {
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

    ez390 is the emulator component of z390 which can be called from
    z390 gui interface or from command line to execute 390 load
    module files.  

    ****************************************************
    * Maintenance
    ****************************************************
    * 04/18/05 copied from lz390.java and modified
    * 06/20/05 start adding and testing common instr.
    * 06/25/05 add opcodes to trace
    * 07/11/05 add floating point instructions
    * 08/20/05 add svc 6 link, fix svc 8 to use r0 pgm name
    * 08/22/05 add SYS390 and SYSLOG dir options
    * 09/04/05 add sequential and random DCB file I/O support
    * 09/16/05 fix DSG, DSGR, DSGF, DSGFR to use R1+1 dividend
    * 09/18/05 add CDE with usage and freemain info for DELETE
    * 09/18/05 add link svc amode support
    * 09/19/05 add TEST option interactive debug
    * 09/27/05 add MEM(MB) option and reduce default to MEM(1)
    * 09/27/05 fix 0C5 at end of mem using work_mem
    ********************************************************
    * Global variables
    *****************************************************/
    /*
     * static limits
     */
	static String version           = "V1.0.00 09/30/05";  //dsh 
	static long   cpu_id            = 0x390;
	static int    max_errors        = 100;        // max errors before abort
    int           max_mem           = 1;          // 1 MB memory default (see mem(mb) override)
    long          max_time_seconds  = 15;         // max elapsed time - override time(sec)
	static int    max_pc_stk        = 50;         // PC, PR, PT stack for psw and regs
    static int    max_tiot_files    = 100;        // max open files
    static int    max_cde_pgms      = 500;        // max loaded 390 pgms
    static int    max_link_stk      = 50;         // max nested links
	static int    max_test_tokens = 4;            // m addr ?? sdt
	static int max_op_type_offset = 31;           // see az390 cases by type
	/* 
	 * shared global variables
	 */
	int ez390_rc = 0;
    int ez390_errors = 0;
    String z390_pgm_parm = null;
    RandomAccessFile z390_file = null;
    File log_file = null;
    BufferedWriter log_file_buff = null;
    String  opt_parms = " ";
    boolean opt_ok = false;
    boolean opt_con      = true;
    boolean opt_list      = true;
    boolean opt_regs     = false;
    boolean opt_stats    = true;
    boolean opt_test     = false;
    boolean opt_time     = true; // abend 422 if out of time TIME(sec)
    boolean opt_timing   = true; // display current date, time, rate
    boolean opt_trace    = false;
    boolean opt_traceall = false;
    boolean opt_tracemem = false;
    boolean opt_trap     = true;
    /*
     * test option interactive debug variables
     */
    String  test_ddname = null;
    String  test_file_name = null;
    boolean test_cmd_abort = false;
    BufferedReader test_cmd_file = null;
    String  test_cmd = null;
    Pattern test_pattern = null;
    Matcher test_match   = null;
    String[] test_token = new String[4];
    int     tot_test_tokens = 0;
    int     test_token_len = 0;
    char    test_opcode = 0;
    int     test_trace_count = 0;
    int test_addr = 0;
    byte test_addr_type = 0;
    static byte test_addr_mem = 0;
    static byte test_addr_reg = 1;
    byte test_compare = 0;
    int test_base_addr = 0;
    int test_mem_loc = 0;
    int test_mem_len = 0;
    int test_reg_loc = 0;
    long test_reg_sdt = 0;
    byte[]  test_mem_sdt  = null;
    /*
     * test reg and mem break on change variables
     */
    boolean test_break_reg_mode = false;
    boolean test_break_mem_mode = false;
    boolean test_break_op_mode = false;
    String  test_break_reg_cmd = null;
    String  test_break_mem_cmd = null;
    String  test_break_op_cmd  = null;
    boolean test_break_reg = false;
    boolean test_break_mem = false;
    int     test_break_reg_loc = 0;
    int     test_break_reg_compare = 0;
    long    test_break_reg_val = 0;
    long    test_break_reg_sdt = 0;
    int     test_break_mem_loc = 0;
    byte    test_break_mem_byte = 0;
    int     test_break_mem_compare = 0;
    int     test_break_mem_equal = 0;
    byte[]  test_break_mem_sdt  = null;
    int     test_break_op1 = 0;
    int     test_break_op2 = 0;
    int     test_break_op2_index = 0;
    int     test_break_op2_mask  = 0;
    /*
     * time and date variables
     */
    SimpleDateFormat cur_date_MMddyy = new SimpleDateFormat("MM/dd/yy");
    SimpleDateFormat cur_tod_hhmmss = new SimpleDateFormat("HH:mm:ss");
    SimpleDateFormat cur_tod_hhmmss00 = new SimpleDateFormat("HHmmss00");
    SimpleDateFormat cur_date_yyyy     = new SimpleDateFormat("yyyy");
    SimpleDateFormat cur_date_MM       = new SimpleDateFormat("MM");
    SimpleDateFormat cur_date_dd       = new SimpleDateFormat("dd");
    SimpleDateFormat cur_date_HH       = new SimpleDateFormat("HH");
    SimpleDateFormat cur_date_mm       = new SimpleDateFormat("mm");
    SimpleDateFormat cur_date_ss       = new SimpleDateFormat("ss");
    SimpleDateFormat cur_date_ms       = new SimpleDateFormat("SSS");
    SimpleDateFormat cur_date_yyddd    = new SimpleDateFormat("yyDDD");
    SimpleDateFormat cur_date_yyyyddd  = new SimpleDateFormat("yyyyDDD");
    SimpleDateFormat cur_date_MMddyyyy = new SimpleDateFormat("MMddyyyy");
    SimpleDateFormat cur_date_ddMMyyyy = new SimpleDateFormat("ddMMyyyy");
    SimpleDateFormat cur_date_yyyyMMdd = new SimpleDateFormat("yyyyMMdd");
    Date cur_date = null;
    int  cur_date_year  = 0;
    int  cur_date_month = 0;
    int  cur_date_day   = 0;
    int  tod_hour  = 0;
    int  tod_min   = 0;
    int  tod_sec   = 0;
    int  tod_msec  = 0;  // 0-999 fraction of sec
    long tod_mil   = 0;  // milli-seconds 
    long tod_mic   = 0;  // micro-seconds
    Calendar cur_date_cal = null;
    long tod_start_day = 0;
    long tod_start_pgm = 0;
    long tod_end_pgm   = 0;
    long tot_sec = 0;
    long tod_time_limit = 0;
    int  next_time_ins   = 0x1000;
    int  next_time_check = next_time_ins;
    boolean log_tod = true; 
    JTextArea z390_log_text = null;
    JTextField  z390_command_text = null;
    boolean ez390_aborted = false;
  /*
   * psw and instruction decode variables
   */  
    int tot_ins    = 0;
    int psw_loc    = 0;
    boolean psw_retry = true;
    static long long_high_bit = ((long)-1) << 63;
    static int  int_high_bit  = 0x80000000;
    static int  int_num_bits  = 0x7fffffff;
    static long long_num_bits = ((long)-1) ^ long_high_bit;
    static long long_low32_bits    = (((long)1) << 32) - 1;
    static long long_high32_bits   = ((long)-1) << 32;
    static long long_sign_bits     = ((long)-1) << 31;
    static int psw_amode24 = 0x00ffffff;
    static int psw_amode31 = 0x7fffffff;
    static int psw_amode24_high_bits = 0xff000000;
    static int psw_amode31_high_bit  = 0x80000000;
    int psw_amode  = psw_amode31;
    int psw_amode_high_bits = (-1) ^ psw_amode;
    static int psw_amode24_bit = 0;
    static int psw_amode31_bit = 0x80000000;
    int psw_amode_bit = psw_amode31_bit;
    /*
     * psw_cc program condition code
     */
    static int psw_cc0 = 8; // EQUAL, ZERO.
    static int psw_cc1 = 4; // LOW, LT
    static int psw_cc2 = 2; // HIGH, GT
    static int psw_cc3 = 1; // OVERFLOW, ONES
    static int[] psw_cc_code = {-1,3,2,-1,1,-1,-1,-1,0};
    static int[] psw_cc_mask = {8,4,2,1}; 
    static int psw_cc_equal = psw_cc0; // mask 8
    static int psw_cc_low   = psw_cc1; // mask 4
    static int psw_cc_high  = psw_cc2; // mask 2
    static int psw_cc_ovf  =  psw_cc3; // mask 1
    int psw_cc     = psw_cc_equal;
    /*
     * psw_pgm_mask fields and pgm interruption fields
     */
    static int psw_pgm_mask_fix = 0x8; // fixed point overflow
    static int psw_pgm_mask_dec = 0x4; // decimal overflow
    static int psw_pgm_mask_exp = 0x2; // fp exponent overflow
    static int psw_pgm_mask_sig = 0x1; // fp significance
    int psw_pgm_mask = 0xe; // enable all but fp sig.
    /*
     * FPC IEEE BFP control registers
     */
    static int fp_fpc_mask_inv = 0x80000000;
    static int fp_fpc_mask_div = 0x40000000;
    static int fp_fpc_mask_ovf = 0x20000000;
    static int fp_fpc_mask_unf = 0x10000000;
    static int fp_fpc_mask_sig = 0x08000000;
    int fp_fpc_reg = 0x70000000; // div+ovf+unf
    static int fp_dxc_dec  = 0x00; // packed decimal data exception
    static int fp_dxc_it   = 0x08; // IEEE inexact and truncated
    static int fp_dxc_ii   = 0x0C; // IEEE inexact and incremented
    static int fp_dxc_ue   = 0x10; // IEEE underflow, exact
    static int fp_dxc_uit  = 0x18; // IEEE underflow, inexact and truncated
    static int fp_dxc_uii  = 0x1C; // IEEE underflow, inexact and incremented
    static int fp_dxc_oe   = 0x20; // IEEE overflow, exact
    static int fp_dxc_oit  = 0x28; // IEEE overflow, inexact and truncated
    static int fp_dxc_oii  = 0x2C; // IEEE overflow, inexact and incremented
    static int fp_dxc_div  = 0x40; // IEEE division by zero
    static int fp_dxc_oper = 0x80; // IEEE invalid operation
    int fp_dxc = 0; // byte 2 of fp_fpc_reg with IEEE exceptions
	
    /*
     * program check and program interruption fields
     */
    boolean psw_check = false;
    boolean fp_signal = false;
    static int psw_pic_exit    = 0; // exit normally
    static int psw_pic_oper    = 0x0c1;
    static int psw_pic_priv    = 0x0c2;
    static int psw_pic_exec    = 0x0c3;
    static int psw_pic_prot    = 0x0c4;
    static int psw_pic_addr    = 0x0c5;
    static int psw_pic_spec    = 0x0c6;
    static int psw_pic_data    = 0x0c7; 
    static int psw_pic_fx_ovf  = 0x0c8;
    static int psw_pic_fx_div  = 0x0c9;
    static int psw_pic_pd_ovf  = 0x0ca;
    static int psw_pic_pd_div  = 0x0cb;
    static int psw_pic_fp_ovf  = 0x0cc;
    static int psw_pic_fp_unf  = 0x0cd;
    static int psw_pic_fp_sig  = 0x0ce;
    static int psw_pic_fp_div  = 0x0cf;
    static int psw_pic_timeout = 0x422;
    static int psw_pic_gm_err  = 0x804; // getmain request invalid
    static int psw_pic_no_mem  = 0x80a; // out of memory
    static int psw_pic_fm_err  = 0x90a; // freemain request invalid
    static int psw_pic_bad_mem = 0xa0a; // memory corruption
    static int psw_pic_stack   = 0xf01;
    static int psw_pic_error   = 0xfff; // internal error
    int psw_pic = 0;
    int[] psw_carry  = {0,1,1,0,0,0,0,0}; // index by psw_cc
    int[] psw_borrow = {0,0,0,0,1,0,0,1}; // index by psw_cc
    int psw_ins_len = 0;
    int dup_opcodes = 0; // count of duplicate opcodes
    int opcode1 = 0;
    int opcode2 = -1;
    int opcode2_offset_e   = 1; // E   PR    oooo
    int opcode2_offset_ri  = 1; // RI  IIHH  ooroiiii
    int opcode2_offset_rie = 5; // RIE BRXLG oorriiii00oo
    int opcode2_offset_ril = 1; // RIL BRCL  oomollllllll
    int opcode2_offset_rrf = 1; // RRF MAER  oooor0rr
    int opcode2_offset_rre = 1; // RRE MSR   oooo00rr
    int opcode2_offset_rsl = 5; // RSL TP    oor0bddd00oo
    int opcode2_offset_rsy = 5; // RSY LMG   oorrbdddhhoo
    int opcode2_offset_rxf = 5; // RXF MAE   oorxbdddr0oo
    int opcode2_offset_rxe = 5; // RXE ADB   oorxbddd00oo
    int opcode2_offset_rxy = 5; // RXY MLG   oorxbdddhhoo
    int opcode2_offset_s   = 1; // S   SSM   oooobddd
    int opcode2_offset_siy = 5; // SIY TMY   ooiibdddhhoo
    int opcode2_offset_sse = 1; // SSE LASP  oooobdddbddd
    int[] op_type_offset = new int[max_op_type_offset];
    int[] op_type_mask   = new int[max_op_type_offset];
    int[] opcode1_type   = new int[256];
    int[] opcode2_offset = new int[256];
    int[] opcode2_mask   = new int[256];
    int svc_code   = 0;
    int espie_exit  = 0; 
    int espie_pie   = 0; // espie psw_pic bit mask
    int estae_exit  = 0;
    int if1        = 0;
    int if2        = 0;
    int sv1        = 0;
    int rflen      = 0;
    int rflen1     = 0;
    int rflen2     = 0;
    int rf1        = 0;
    int rf2        = 0;
    int rf3        = 0;
    int mf1        = 0;
    int mf2        = 0;
    int mf3        = 0;
    int mf4        = 0;
    static int[] mask_bits = {0,1,1,2,1,2,2,3,1,2,2,3,2,3,3,4};
    int rv1        = 0;
    int rv2        = 0;
    int rv3        = 0;
    int rvw        = 0;
    long rlv1        = 0;
    long rlv2        = 0;
    long rlv3        = 0;
    long rlvw        = 0;
    int bf1        = 0;
    int df1        = 0;
    int xf2        = 0;
    int bf2        = 0;
    int df2        = 0;
    int bd1_loc    = 0;
    int bd2_loc    = 0;
    int bd1_start  = 0;
    int bd2_start  = 0;
    int xbd2_loc    = 0;
    int bd1_end    = 0;
    int bd2_end    = 0;
    int data_len   = 0;
    int pad_len    = 0;
    byte fill_char;
    boolean ex_mode = false;
    static int ex_opcode = 0x44;
    byte ex_mod_byte   = 0;  // save targe+1 byte 
    int ex_psw_return  = 0; // return from ex
    byte[]     pd_bytes = (byte[])Array.newInstance(byte.class,16);
    ByteBuffer pd_byte = ByteBuffer.wrap(pd_bytes,0,16);
    String pdf_str = null;
    int    pdf_str_len = 0;
    int    pdf_zeros = 0;
    char   pdf_sign = '+';
    byte   pdf_next_out = 0;
    byte   pdf_next_in  = 0;
    BigInteger big_int = null;
    BigInteger big_int1 = null;
    BigInteger big_int2 = null;
    BigInteger[] big_int_array = null;
    int   pd_cc = 0;
  /*
   * load module header, code, and rld variables
   */  
   /*
    * 16 byte header with 4 fields as follows
    * offset 0 - 4 character format version
    * offset 4 - full word length of code
    * offset 4 - full word entry offset
    * offset 4 - full word count of rlds 
    */ 
    char[] z390_code_ver  = new char[4];
    char[] z390_flags     = new char[4];
    static int z390_flags_amode31 = 0;
    static int z390_flags_rmode31 = 1;
    String z390_pgm_name  = null;
    int    z390_code_load = 0;
    int    z390_code_len  = 0;
    int    z390_code_ent  = 0;
    int    z390_code_rlds = 0;
    /*
     * 16 gpr registers
     */
    byte[]     reg_byte = (byte[])Array.newInstance(byte.class,16*8);
    ByteBuffer reg = ByteBuffer.wrap(reg_byte,0,16*8);  
    byte[]     work_reg_byte = (byte[])Array.newInstance(byte.class,16);
    ByteBuffer work_reg      = ByteBuffer.wrap(work_reg_byte,0,16);
    byte[]     log_reg_byte  = new byte[9];
    ByteBuffer log_reg       = ByteBuffer.wrap(log_reg_byte,0,9);
    static int r0 =  4;
    static int r1 = 12;
    static int r2 = 20;
    static int r3 = 28;
    static int r4 = 36;
    static int r5 = 44;
    static int r6 = 52;
    static int r7 = 60;
    static int r8 = 68;
    static int r9 = 76;
    static int r10= 84;
    static int r11= 92;
    static int r12=100;
    static int r13=108;
    static int r14=116;
    static int r15=124;
    /*
     * 16 fp registers with eb, db, and bd co-regs
     * to avoid conversions when possible fp_ctl
     * defines fp reg state as follows:
     *    0 = fp_ctl_ld no co-reg defined (set by LE,LD)
     *    1 = fp_ctl_eb set for EB float operations
     *    2 = fp_ctl_db set for EH, DH, DB operations
     *    3 = fp_ctl_bd set for LH, LB operations
     * Notes:
     *   1.  LE and LD set fp_reg with fp_ctl_ld 
     *   2.  STE and STD store from fp_reg or co_reg
     *   3.  fp_reg is indexed by reg * 8 byte index
     *   4.  fp_ctl and co-regs are indexed by reg # 
     */
    byte[]     fp_reg_byte = (byte[])Array.newInstance(byte.class,16*8);
    ByteBuffer fp_reg = ByteBuffer.wrap(fp_reg_byte,0,16*8);  
    byte[]     trace_reg_byte = (byte[])Array.newInstance(byte.class,16*8);
    ByteBuffer trace_reg = ByteBuffer.wrap(trace_reg_byte,0,16*8);  

    static byte  fp_ctl_ld = 0;
    static byte  fp_ctl_eb = 1;
    static byte  fp_ctl_db = 2;
    static byte  fp_ctl_bd1 = 3; // first  half
    static byte  fp_ctl_bd2 = 4; // second half
    byte[]       fp_reg_type = (byte[])Array.newInstance(byte.class,16);
    byte[]       fp_reg_ctl  = (byte[])Array.newInstance(byte.class,16);
    float[]      fp_reg_eb   = (float[])Array.newInstance(float.class,16);
    double[]     fp_reg_db   = (double[])Array.newInstance(double.class,16);
    BigDecimal[] fp_reg_bd   = (BigDecimal[])Array.newInstance(BigDecimal.class,16);
    byte[]     work_fp_reg_byte = (byte[])Array.newInstance(byte.class,16);
    ByteBuffer work_fp_reg = ByteBuffer.wrap(work_fp_reg_byte,0,16);
    /*
     * fp work variables
     */
    float fp_rev1 = 0;
    float fp_rev2 = 0;
    double fp_rdv1 = 0;
    double fp_rdv2 = 0;
    double fp_rdv3 = 0;
    double fp_rdv4 = 0;
    BigDecimal fp_rbdv1 = null;
    BigDecimal fp_rbdv2 = null;
    BigDecimal fp_rbdv3 = null;
    BigDecimal fp_bd_half = BigDecimal.ONE.divide(BigDecimal.valueOf(2));
    int fp_bd_sqrt_scale = 0;
    byte[]     work_fp_bi1_bytes = (byte[])Array.newInstance(byte.class,15);
    BigInteger work_fp_bi1 = null;    
    BigDecimal work_fp_bd1 = null;
    long long_dh_zero = (long)(0x40) << 56;
    long long_dh_exp_bits = (long)(0x7f) << 56; 
    long long_dh_man_bits = ((long)(1) << 56) - 1;
    int fp_round = 0;
    long long_work = 0;
    long long_sign = 0;
    long long_exp  = 0;
    long long_man  = 0;
    long long_db_exp_bits = (long)(0x7ff) << 52;
    long long_db_one_bits = ((long)(1) << 53) - 1;
    long long_db_one_bit  = ((long)(1) << 52);
    long long_db_man_bits = ((long)(1) << 52) - 1;
    int  int_eh_exp_bits = 0x7f << 24;
    int  int_eh_zero = 0x40000000;
    int  int_work = 0;
    int  fp_sign = 0;
    int  fp_exp  = 0;
    int  int_man  = 0;
    int  int_eh_man_bits = 0xffffff;
    /*
     * fp conversion from big decimal to LH/LB
     * variables copied from AZ390 routine 
     * developed earlier to convert string to
     * floating point constants
     */
    char   dc_type_sfx = ' ';
    static byte fp_db_type = 0;
    static byte fp_dh_type = 1;
    static byte fp_eb_type = 2;
    static byte fp_eh_type = 3;
    static byte fp_lb_type = 4;
    static byte fp_lh_type = 5;
    byte   fp_type = 0;
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
    MathContext fp_bd_context = new MathContext(fp_precision[fp_lb_type]);  
    MathContext fp_db_context = new MathContext(fp_precision[fp_db_type]);
    MathContext fp_eb_context = new MathContext(fp_precision[fp_eb_type]);
    MathContext fp_e_context = MathContext.DECIMAL32;  
    MathContext fp_d_context = MathContext.DECIMAL64;
    MathContext fp_x_context = MathContext.DECIMAL128;
    int[]  fp_sign_bit = {0x800,0x80,0x100,0x80,0x8000,0x80};
    int[]  fp_one_bit_adj = {2,1,2,1,2,1};
    int[]  fp_exp_bias = {0x3ff,0x40,0x7f,0x40,0x3fff,0x40};
    int[]  fp_exp_max  = {0x7ff,0x7f,0xff,0x7f,0x7fff,0x7f};
	static double fp_log2  = Math.log(2);
	static double fp_log10 = Math.log(10);
    BigDecimal fp_bd = new BigDecimal("0");
    BigDecimal fp_big_dec2 = new BigDecimal("0");
    BigDecimal fp_big_dec3 = new BigDecimal("0");
    byte[] fp_lb_zero = new byte[16];
    byte[] fp_lh_zero = new byte[16];
    BigInteger fp_big_int1 = new BigInteger("0");
    BigInteger fp_big_int2 = new BigInteger("0");
	BigInteger fp_big_int_one_bits = BigInteger.ONE.shiftLeft(113).subtract(BigInteger.ONE);
	BigInteger fp_big_int_man_bits = BigInteger.ONE.shiftLeft(112).subtract(BigInteger.ONE);
	int    fp_int1 = 0;
    static int fp_int_eb_one_bits  = 0xffffff;
    static int fp_int_eb_man_bits  = 0x7fffff;
    static int fp_int_eh_man_bits  = 0xffffff;
    long   fp_long1 = 0;
    static long fp_long_db_one_bits = ((long)(1) << 53) - 1;
    static long fp_long_db_man_bits = ((long)(1) << 52) - 1;
    static long fp_long_dh_man_bits = ((long)(1) << 56) - 1;
    float      fp_eb_min = (float)1.2e-38;  // BFP range ref. pop 19-5 
    float      fp_eb_max = (float)3.4e+38;
    double     fp_db_min = 2.2e-308;
    double     fp_db_max = 1.79e+308; // (1.8 too big?)
    double     fp_eh_min = 5.41e-79; // HFP range ref. pop 18-4
    double     fp_eh_max = 7.2e+75;
    double     fp_dh_min = 5.41e-79;
    double     fp_dh_max = 7.2e+75;
    BigDecimal fp_lh_min = new BigDecimal("5.41e-79",fp_bd_context);
    BigDecimal fp_lh_max = new BigDecimal("7.2e+75",fp_bd_context);
    BigDecimal fp_lb_min = new BigDecimal("3.4e-4932",fp_bd_context);
    BigDecimal fp_lb_max = new BigDecimal("1.2e+4932",fp_bd_context);
    /*
     * PC, PR, PT stack for psw and regs
     */
    int cur_pc_stk = 0;
    int cur_pc_stk_reg = 0;
    byte[]     pc_stk_reg_byte = (byte[])Array.newInstance(byte.class,max_pc_stk*reg_byte.length);
    ByteBuffer pc_stk_reg      = ByteBuffer.wrap(pc_stk_reg_byte,0,max_pc_stk * reg_byte.length);  
    int[]      pc_stk_psw_loc  = (int[])Array.newInstance(int.class,max_pc_stk);
    int[]      pc_stk_psw_cc   = (int[])Array.newInstance(int.class,max_pc_stk);
    /*
     * virtual memory with 24 bit and 31 bit fqes
     * all initialized by init_mem()
     * 
     * Note: current default is 16 MB of each
     */
    static int mem24_start = 4096;
    static int mem24_line  = 0x1000000;
	byte[] mem_byte = null;
	ByteBuffer mem = null;
    byte[]     work_mem_byte = (byte[])Array.newInstance(byte.class,16);
    ByteBuffer work_mem      = ByteBuffer.wrap(work_mem_byte,0,16);
    int dsa24_start = 0;
    int dsa24_end   = 0;
    int dsa31_start = 0;
    int dsa31_end   = 0; 
    int tot_mem     = 0; 
    /*
     * CDE content directory entry variables used
     * by LOAD, LINK, and DELETE to manage 390
     * load modules in memory
     */
    int tot_cde = 0;
    int cur_cde = 0;
    String[] cde_name  = new String[max_cde_pgms];
    int[]    cde_use   = (int[])Array.newInstance(int.class,max_cde_pgms);
    int[]    cde_loc   = (int[])Array.newInstance(int.class,max_cde_pgms);
    int[]    cde_len   = (int[])Array.newInstance(int.class,max_cde_pgms);
    int[]    cde_ent   = (int[])Array.newInstance(int.class,max_cde_pgms);
    /*
     * nested link variables
     */
    int tot_link_stk = 0;
    int cur_link_stk = 0;
    int[]    link_stk_cde = (int[])Array.newInstance(int.class,max_link_stk);
    int[]    link_stk_ret = (int[])Array.newInstance(int.class,max_link_stk);
    /*
     * rld entries in following format
     *   offset 0 full workd rld field offset
     *   offset 4 signed byte rld field len
     * (negative len means subtract the base address
     * versus adding it to rld field.)
     */
     int  rld_loc = 0;
     byte rld_len = 0;
  /*
   * current directory global variables
   */   
     String dir_cur = null;
     String dir_390 = null;
     String dir_log = null;
     String pgm_name = null;
    /*
     * DCB sequential and random file I/O tables
     */
     int    tot_tiot_files = 0;
     int    cur_tiot_index = 0;
     int    cur_decb_addr  = 0;
     int    cur_ecb        = 0;
     static int decb_ecb   = 0;
     static int decb_type  = 4;
     static int decb_dcb   = 8;
     static int decb_area  = 12;
     long   cur_rba        = 0;
     int    cur_dcb_addr   = 0;     // mem offset to dcb from r1
     String cur_dcb_ddnam = null;  // ascii ddname from dcb ebcdic
     String cur_dcb_file_name = null;
     int    cur_dcb_oflgs  = 0;
     int    cur_dcb_synad  = 0;
     int    cur_dcb_eodad  = 0;
     int    cur_dcb_macrf  = 0;
     int    cur_dcb_recfm  = 0;
     int    cur_dcb_area   = 0;
     int    cur_dcb_lrecl  = 0;
     int    cur_dcb_blksi  = 0;
     int    cur_vrec_lrecl  = 0;
     String cur_rec_text  = null;
     int    cur_rec_len   = 0;
     static int dcb_dsorg  = 0x1a;
     static int dcb_dsorg_ps = 0x40; // physical sequential (GM or PM) 
     static int dcb_dsorg_da = 0x20; // direct access(R/W)
     static int dcb_iobad  = 0x1c;      // has tiot_index +1 while open else 0
     static int dcb_eodad  = 0x20;      // end of data exit
     static int dcb_recfm  = 0x24;      // record format
     static int dcb_recfm_f = 0x80;     // fixed
     static int dcb_recfm_v = 0x40;     // variable
     static int dcb_recfm_fb = 0x90;    // fixed blocked
     static int dcb_recfm_vb = 0x50;    // variable blocked
     static int dcb_recfm_ft = 0xa0;    // fixed to/from ascii text
     static int dcb_recfm_vt = 0x60;    // variable to/from ascii text
     static int dcb_ddnam  = 0x28;      // ebcdic ddname
     static int dcb_oflgs  = 0x30;      // open flags
     static int dcb_oflgs_open = 0x80;  // file open
     static int dcb_oflgs_gm   = 0x40;  // read allowed
     static int dcb_oflgs_pm   = 0x20;  // write allowed
     static int dcb_oflgs_rw   = 0x60;  // read and write allowed
     static int dcb_macrf  = 0x32;      // macro access type
     static int dcb_macrf_gm = 0x5000;  // get move
     static int dcb_macrf_pm = 0x0050;  // put move
     static int dcb_macrf_rw = 0x2020;  // read/write random
     static int dcb_synad = 0x38;       // synchronous error exit
     static int dcb_blksi = 0x3e;       // blocksize
     static int dcb_lrecl = 0x52;       // record length
     String[]           tiot_ddnam      = new String[max_tiot_files];
     int[]              tiot_dcb_addr   = (int[])Array.newInstance(int.class,max_tiot_files);
     int[]              tiot_vrec_blksi = (int[])Array.newInstance(int.class,max_tiot_files);
     long[]             tiot_cur_rba    = (long[])Array.newInstance(long.class,max_tiot_files);
     long[]             tiot_eof_rba    = (long[])Array.newInstance(long.class,max_tiot_files);
     RandomAccessFile[] tiot_file       = (RandomAccessFile[])Array.newInstance(RandomAccessFile.class,max_tiot_files);
    /*
     * ASCII and EBCDIC printable character tables
     */
        String ascii_table = 
        "................" + //00
        "................" + //10
        " !" + '"' + "#$%&'()*+,-./" + //20 with "
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
        ".........`:#@'=" + '"' + //70 with "
        ".abcdefghi......" + //80
        ".jklmnopqr......" + //90
        ".~stuvwxyz......" + //A0
        "^.........[]...." + //B0
        "{ABCDEFGHI......" + //C0
        "}JKLMNOPQR......" + //D0
        "\\.STUVWXYZ......" + //E0 with \
        "0123456789......";   //F0
        byte[] ascii_to_ebcdic = new byte[256];
        int ascii_lf = 10;
        int ascii_cr = 13;
        int ascii_period =  (int)'.';
        int ascii_space = (int) ' ';
        int ebcdic_period = 75;
        int ebcdic_space = 64;
  /*
   * communication vector table (ptr at 16)
   */      
      int cvt_start    = 0x200;                  // cvt start
      int cvt_ipl_pgm  = cvt_start+0x08;       // ipl pgm name
      int cvt_fqe24    = cvt_start+0x10;       // amode 24 fqe   
      int cvt_fqe31    = cvt_start+0x14;       // amode 31 fqe
      int cvt_save     = cvt_start+0x18;       // user save  
      int cvt_exit     = cvt_start+0x60;  // svc 3 exit to last link or term
  /*
   * getmain and freemain work areas used by trace
   */
    int req_addr = 0;
    int req_len  = 0;
    int req_opt  = 0;
  	int cur_fqe  = 0;
	int cur_fqe_len = 0;
	int prev_fqe = 0;
	int next_fqe = 0;
	int next_fqe_len = 0;
	static int opt_getmain_amode31 = 0x02;
	static int opt_getmain_cond    = 0x01;
  /*
   * opcode tables for trace
   */
      String[] op_name = {
		       "PR",       // 10 "0101" "PR" "E" 1
		       "UPT",      // 20 "0102" "UPT" "E" 1
		       "SCKPF",    // 30 "0107" "SCKPF" "E" 1
		       "TAM",      // 40 "010B" "TAM" "E" 1
		       "SAM24",    // 50 "010C" "SAM24" "E" 1
		       "SAM31",    // 60 "010D" "SAM31" "E" 1
		       "SAM64",    // 70 "010E" "SAM64" "E" 1
		       "TRAP2",    // 80 "01FF" "TRAP2" "E" 1
		       "SPM",      // 90 "04" "SPM" "RR" 2
		       "BALR",     // 100 "05" "BALR" "RR" 2
		       "BCTR",     // 110 "06" "BCTR" "RR" 2
		       "BCR",      // 120 "07" "BCR" "RR" 2
		       "BR",       // 130 "07F" "BR" "BRX" 3
		       "NOPR",     // 140 "070" "NOPR" "BRX" 3
		       "BHR",      // 150 "072" "BHR" "BRX" 3
		       "BLR",      // 160 "074" "BLR" "BRX" 3
		       "BER",      // 170 "078" "BER" "BRX" 3
		       "BNHR",     // 180 "07D" "BNHR" "BRX" 3
		       "BNLR",     // 190 "07B" "BNLR" "BRX" 3
		       "BNER",     // 200 "077" "BNER" "BRX" 3
		       "BPR",      // 210 "072" "BPR" "BRX" 3
		       "BOR",      // 220 "071" "BOR" "BRX" 3
		       "BMR",      // 230 "074" "BMR" "BRX" 3
		       "BZR",      // 240 "078" "BZR" "BRX" 3
		       "BNPR",     // 250 "07D" "BNPR" "BRX" 3
		       "BNMR",     // 260 "07B" "BNMR" "BRX" 3
		       "BNZR",     // 270 "077" "BNZR" "BRX" 3
		       "BNOR",     // 280 "07E" "BNOR" "BRX" 3
		       "SVC",      // 290 "0A" "SVC" "I" 4
		       "BSM",      // 300 "0B" "BSM" "RR" 2
		       "BASSM",    // 310 "0C" "BASSM" "RR" 2
		       "BASR",     // 320 "0D" "BASR" "RR" 2
		       "MVCL",     // 330 "0E" "MVCL" "RR" 2
		       "CLCL",     // 340 "0F" "CLCL" "RR" 2
		       "LPR",      // 350 "10" "LPR" "RR" 2
		       "LNR",      // 360 "11" "LNR" "RR" 2
		       "LTR",      // 370 "12" "LTR" "RR" 2
		       "LCR",      // 380 "13" "LCR" "RR" 2
		       "NR",       // 390 "14" "NR" "RR" 2
		       "CLR",      // 400 "15" "CLR" "RR" 2
		       "OR",       // 410 "16" "OR" "RR" 2
		       "XR",       // 420 "17" "XR" "RR" 2
		       "LR",       // 430 "18" "LR" "RR" 2
		       "CR",       // 440 "19" "CR" "RR" 2
		       "AR",       // 450 "1A" "AR" "RR" 2
		       "SR",       // 460 "1B" "SR" "RR" 2
		       "MR",       // 470 "1C" "MR" "RR" 2
		       "DR",       // 480 "1D" "DR" "RR" 2
		       "ALR",      // 490 "1E" "ALR" "RR" 2
		       "SLR",      // 500 "1F" "SLR" "RR" 2
		       "LPDR",     // 510 "20" "LPDR" "RR" 2
		       "LNDR",     // 520 "21" "LNDR" "RR" 2
		       "LTDR",     // 530 "22" "LTDR" "RR" 2
		       "LCDR",     // 540 "23" "LCDR" "RR" 2
		       "HDR",      // 550 "24" "HDR" "RR" 2
		       "LDXR",     // 560 "25" "LDXR" "RR" 2
		       "LRDR",     // 570 "25" "LRDR" "RR" 2
		       "MXR",      // 580 "26" "MXR" "RR" 2
		       "MXDR",     // 590 "27" "MXDR" "RR" 2
		       "LDR",      // 600 "28" "LDR" "RR" 2
		       "CDR",      // 610 "29" "CDR" "RR" 2
		       "ADR",      // 620 "2A" "ADR" "RR" 2
		       "SDR",      // 630 "2B" "SDR" "RR" 2
		       "MDR",      // 640 "2C" "MDR" "RR" 2
		       "DDR",      // 650 "2D" "DDR" "RR" 2
		       "AWR",      // 660 "2E" "AWR" "RR" 2
		       "SWR",      // 670 "2F" "SWR" "RR" 2
		       "LPER",     // 680 "30" "LPER" "RR" 2
		       "LNER",     // 690 "31" "LNER" "RR" 2
		       "LTER",     // 700 "32" "LTER" "RR" 2
		       "LCER",     // 710 "33" "LCER" "RR" 2
		       "HER",      // 720 "34" "HER" "RR" 2
		       "LEDR",     // 730 "35" "LEDR" "RR" 2
		       "LRER",     // 740 "35" "LRER" "RR" 2
		       "AXR",      // 750 "36" "AXR" "RR" 2
		       "SXR",      // 760 "37" "SXR" "RR" 2
		       "LER",      // 770 "38" "LER" "RR" 2
		       "CER",      // 780 "39" "CER" "RR" 2
		       "AER",      // 790 "3A" "AER" "RR" 2
		       "SER",      // 800 "3B" "SER" "RR" 2
		       "MDER",     // 810 "3C" "MDER" "RR" 2
		       "MER",      // 820 "3C" "MER" "RR" 2
		       "DER",      // 830 "3D" "DER" "RR" 2
		       "AUR",      // 840 "3E" "AUR" "RR" 2
		       "SUR",      // 850 "3F" "SUR" "RR" 2
		       "STH",      // 860 "40" "STH" "RX" 5
		       "LA",       // 870 "41" "LA" "RX" 5
		       "STC",      // 880 "42" "STC" "RX" 5
		       "IC",       // 890 "43" "IC" "RX" 5
		       "EX",       // 900 "44" "EX" "RX" 5
		       "BAL",      // 910 "45" "BAL" "RX" 5
		       "BCT",      // 920 "46" "BCT" "RX" 5
		       "BC",       // 930 "47" "BC" "RX" 5
		       "B",        // 940 "47F" "B" "BCX" 6
		       "NOP",      // 950 "470" "NOP" "BCX" 6
		       "BH",       // 960 "472" "BH" "BCX" 6
		       "BL",       // 970 "474" "BL" "BCX" 6
		       "BE",       // 980 "478" "BE" "BCX" 6
		       "BNH",      // 990 "47D" "BNH" "BCX" 6
		       "BNL",      // 1000 "47B" "BNL" "BCX" 6
		       "BNE",      // 1010 "477" "BNE" "BCX" 6
		       "BP",       // 1020 "472" "BP" "BCX" 6
		       "BO",       // 1030 "471" "BO" "BCX" 6
		       "BM",       // 1040 "474" "BM" "BCX" 6
		       "BZ",       // 1050 "478" "BZ" "BCX" 6
		       "BNP",      // 1060 "47D" "BNP" "BCX" 6
		       "BNM",      // 1070 "47B" "BNM" "BCX" 6
		       "BNZ",      // 1080 "477" "BNZ" "BCX" 6
		       "BNO",      // 1090 "47E" "BNO" "BCX" 6
		       "LH",       // 1100 "48" "LH" "RX" 5
		       "CH",       // 1110 "49" "CH" "RX" 5
		       "AH",       // 1120 "4A" "AH" "RX" 5
		       "SH",       // 1130 "4B" "SH" "RX" 5
		       "MH",       // 1140 "4C" "MH" "RX" 5
		       "BAS",      // 1150 "4D" "BAS" "RX" 5
		       "CVD",      // 1160 "4E" "CVD" "RX" 5
		       "CVB",      // 1170 "4F" "CVB" "RX" 5
		       "ST",       // 1180 "50" "ST" "RX" 5
		       "LAE",      // 1190 "51" "LAE" "RX" 5
		       "N",        // 1200 "54" "N" "RX" 5
		       "CL",       // 1210 "55" "CL" "RX" 5
		       "O",        // 1220 "56" "O" "RX" 5
		       "X",        // 1230 "57" "X" "RX" 5
		       "L",        // 1240 "58" "L" "RX" 5
		       "C",        // 1250 "59" "C" "RX" 5
		       "A",        // 1260 "5A" "A" "RX" 5
		       "S",        // 1270 "5B" "S" "RX" 5
		       "M",        // 1280 "5C" "M" "RX" 5
		       "D",        // 1290 "5D" "D" "RX" 5
		       "AL",       // 1300 "5E" "AL" "RX" 5
		       "SL",       // 1310 "5F" "SL" "RX" 5
		       "STD",      // 1320 "60" "STD" "RX" 5
		       "MXD",      // 1330 "67" "MXD" "RX" 5
		       "LD",       // 1340 "68" "LD" "RX" 5
		       "CD",       // 1350 "69" "CD" "RX" 5
		       "AD",       // 1360 "6A" "AD" "RX" 5
		       "SD",       // 1370 "6B" "SD" "RX" 5
		       "MD",       // 1380 "6C" "MD" "RX" 5
		       "DD",       // 1390 "6D" "DD" "RX" 5
		       "AW",       // 1400 "6E" "AW" "RX" 5
		       "SW",       // 1410 "6F" "SW" "RX" 5
		       "STE",      // 1420 "70" "STE" "RX" 5
		       "MS",       // 1430 "71" "MS" "RX" 5
		       "LE",       // 1440 "78" "LE" "RX" 5
		       "CE",       // 1450 "79" "CE" "RX" 5
		       "AE",       // 1460 "7A" "AE" "RX" 5
		       "SE",       // 1470 "7B" "SE" "RX" 5
		       "MDE",      // 1480 "7C" "MDE" "RX" 5
		       "ME",       // 1490 "7C" "ME" "RX" 5
		       "DE",       // 1500 "7D" "DE" "RX" 5
		       "AU",       // 1510 "7E" "AU" "RX" 5
		       "SU",       // 1520 "7F" "SU" "RX" 5
		       "SSM",      // 1530 "8000" "SSM" "S" 7
		       "LPSW",     // 1540 "8200" "LPSW" "S" 7
		       "DIAGNOSE", // 1550 "83" "DIAGNOSE" "DM" 8
		       "BRXH",     // 1560 "84" "BRXH" "RSI" 9
		       "JXH",      // 1570 "84" "JXH" "RSI" 9
		       "BRXLE",    // 1580 "85" "BRXLE" "RSI" 9
		       "JXLE",     // 1590 "85" "JXLE" "RSI" 9
		       "BXH",      // 1600 "86" "BXH" "RS" 10
		       "BXLE",     // 1610 "87" "BXLE" "RS" 10
		       "SRL",      // 1620 "88" "SRL" "RS" 10
		       "SLL",      // 1630 "89" "SLL" "RS" 10
		       "SRA",      // 1640 "8A" "SRA" "RS" 10
		       "SLA",      // 1650 "8B" "SLA" "RS" 10
		       "SRDL",     // 1660 "8C" "SRDL" "RS" 10
		       "SLDL",     // 1670 "8D" "SLDL" "RS" 10
		       "SRDA",     // 1680 "8E" "SRDA" "RS" 10
		       "SLDA",     // 1690 "8F" "SLDA" "RS" 10
		       "STM",      // 1700 "90" "STM" "RS" 10
		       "TM",       // 1710 "91" "TM" "SI" 11
		       "MVI",      // 1720 "92" "MVI" "SI" 11
		       "TS",       // 1730 "9300" "TS" "S" 7
		       "NI",       // 1740 "94" "NI" "SI" 11
		       "CLI",      // 1750 "95" "CLI" "SI" 11
		       "OI",       // 1760 "96" "OI" "SI" 11
		       "XI",       // 1770 "97" "XI" "SI" 11
		       "LM",       // 1780 "98" "LM" "RS" 10
		       "TRACE",    // 1790 "99" "TRACE" "RS" 10
		       "LAM",      // 1800 "9A" "LAM" "RS" 10
		       "STAM",     // 1810 "9B" "STAM" "RS" 10
		       "IIHH",     // 1820 "A50" "IIHH" "RI" 12
		       "IIHL",     // 1830 "A51" "IIHL" "RI" 12
		       "IILH",     // 1840 "A52" "IILH" "RI" 12
		       "IILL",     // 1850 "A53" "IILL" "RI" 12
		       "NIHH",     // 1860 "A54" "NIHH" "RI" 12
		       "NIHL",     // 1870 "A55" "NIHL" "RI" 12
		       "NILH",     // 1880 "A56" "NILH" "RI" 12
		       "NILL",     // 1890 "A57" "NILL" "RI" 12
		       "OIHH",     // 1900 "A58" "OIHH" "RI" 12
		       "OIHL",     // 1910 "A59" "OIHL" "RI" 12
		       "OILH",     // 1920 "A5A" "OILH" "RI" 12
		       "OILL",     // 1930 "A5B" "OILL" "RI" 12
		       "LLIHH",    // 1940 "A5C" "LLIHH" "RI" 12
		       "LLIHL",    // 1950 "A5D" "LLIHL" "RI" 12
		       "LLILH",    // 1960 "A5E" "LLILH" "RI" 12
		       "LLILL",    // 1970 "A5F" "LLILL" "RI" 12
		       "TMLH",     // 1980 "A70" "TMLH" "RI" 12
		       "TMH",      // 1990 "A70" "TMH" "RI" 12
		       "TMLL",     // 2000 "A71" "TMLL" "RI" 12
		       "TML",      // 2010 "A71" "TML" "RI" 12
		       "TMHH",     // 2020 "A72" "TMHH" "RI" 12
		       "TMHL",     // 2030 "A73" "TMHL" "RI" 12
		       "BRC",      // 2040 "A74" "BRC" "RI" 12
		       "J",        // 2050 "A74F" "J" "BRC" 13
		       "JNOP",     // 2060 "A740" "JNOP" "BRC" 13
		       "BRU",      // 2070 "A74F" "BRU" "BRC" 13
		       "BRH",      // 2080 "A742" "BRH" "BRC" 13
		       "BRL",      // 2090 "A744" "BRL" "BRC" 13
		       "BRE",      // 2100 "A748" "BRE" "BRC" 13
		       "BRNH",     // 2110 "A74D" "BRNH" "BRC" 13
		       "BRNL",     // 2120 "A74B" "BRNL" "BRC" 13
		       "BRNE",     // 2130 "A747" "BRNE" "BRC" 13
		       "BRP",      // 2140 "A742" "BRP" "BRC" 13
		       "BRM",      // 2150 "A744" "BRM" "BRC" 13
		       "BRZ",      // 2160 "A748" "BRZ" "BRC" 13
		       "BRO",      // 2170 "A741" "BRO" "BRC" 13
		       "BRNP",     // 2180 "A74D" "BRNP" "BRC" 13
		       "BRNM",     // 2190 "A74B" "BRNM" "BRC" 13
		       "BRNZ",     // 2200 "A747" "BRNZ" "BRC" 13
		       "BRNO",     // 2210 "A74E" "BRNO" "BRC" 13
		       "JH",       // 2220 "A742" "JH" "BRC" 13
		       "JL",       // 2230 "A744" "JL" "BRC" 13
		       "JE",       // 2240 "A748" "JE" "BRC" 13
		       "JNH",      // 2250 "A74D" "JNH" "BRC" 13
		       "JNL",      // 2260 "A74B" "JNL" "BRC" 13
		       "JNE",      // 2270 "A747" "JNE" "BRC" 13
		       "JP",       // 2280 "A742" "JP" "BRC" 13
		       "JM",       // 2290 "A744" "JM" "BRC" 13
		       "JZ",       // 2300 "A748" "JZ" "BRC" 13
		       "JO",       // 2310 "A741" "JO" "BRC" 13
		       "JNP",      // 2320 "A74D" "JNP" "BRC" 13
		       "JNM",      // 2330 "A74B" "JNM" "BRC" 13
		       "JNZ",      // 2340 "A747" "JNZ" "BRC" 13
		       "JNO",      // 2350 "A74E" "JNO" "BRC" 13
		       "BRAS",     // 2360 "A75" "BRAS" "RI" 12
		       "JAS",      // 2370 "A75" "JAS" "RI" 12
		       "BRCT",     // 2380 "A76" "BRCT" "RI" 12
		       "JCT",      // 2390 "A76" "JCT" "RI" 12
		       "BRCTG",    // 2400 "A77" "BRCTG" "RI" 12
		       "JCTG",     // 2410 "A77" "JCTG" "RI" 12
		       "LHI",      // 2420 "A78" "LHI" "RI" 12
		       "LGHI",     // 2430 "A79" "LGHI" "RI" 12
		       "AHI",      // 2440 "A7A" "AHI" "RI" 12
		       "AGHI",     // 2450 "A7B" "AGHI" "RI" 12
		       "MHI",      // 2460 "A7C" "MHI" "RI" 12
		       "MGHI",     // 2470 "A7D" "MGHI" "RI" 12
		       "CHI",      // 2480 "A7E" "CHI" "RI" 12
		       "CGHI",     // 2490 "A7F" "CGHI" "RI" 12
		       "MVCLE",    // 2500 "A8" "MVCLE" "RS" 10
		       "CLCLE",    // 2510 "A9" "CLCLE" "RS" 10
		       "STNSM",    // 2520 "AC" "STNSM" "SI" 11
		       "STOSM",    // 2530 "AD" "STOSM" "SI" 11
		       "SIGP",     // 2540 "AE" "SIGP" "RS" 10
		       "MC",       // 2550 "AF" "MC" "SI" 11
		       "LRA",      // 2560 "B1" "LRA" "RX" 5
		       "STIDP",    // 2570 "B202" "STIDP" "S" 7
		       "SCK",      // 2580 "B204" "SCK" "S" 7
		       "STCK",     // 2590 "B205" "STCK" "S" 7
		       "SCKC",     // 2600 "B206" "SCKC" "S" 7
		       "STCKC",    // 2610 "B207" "STCKC" "S" 7
		       "SPT",      // 2620 "B208" "SPT" "S" 7
		       "STPT",     // 2630 "B209" "STPT" "S" 7
		       "SPKA",     // 2640 "B20A" "SPKA" "S" 7
		       "IPK",      // 2650 "B20B" "IPK" "S" 7
		       "PTLB",     // 2660 "B20D" "PTLB" "S" 7
		       "SPX",      // 2670 "B210" "SPX" "S" 7
		       "STPX",     // 2680 "B211" "STPX" "S" 7
		       "STAP",     // 2690 "B212" "STAP" "S" 7
		       "PC",       // 2700 "B218" "PC" "S" 7
		       "SAC",      // 2710 "B219" "SAC" "S" 7
		       "CFC",      // 2720 "B21A" "CFC" "S" 7
		       "IPTE",     // 2730 "B221" "IPTE" "RRE" 14
		       "IPM",      // 2740 "B222" "IPM" "RRE" 14
		       "IVSK",     // 2750 "B223" "IVSK" "RRE" 14
		       "IAC",      // 2760 "B224" "IAC" "RRE" 14
		       "SSAR",     // 2770 "B225" "SSAR" "RRE" 14
		       "EPAR",     // 2780 "B226" "EPAR" "RRE" 14
		       "ESAR",     // 2790 "B227" "ESAR" "RRE" 14
		       "PT",       // 2800 "B228" "PT" "RRE" 14
		       "ISKE",     // 2810 "B229" "ISKE" "RRE" 14
		       "RRBE",     // 2820 "B22A" "RRBE" "RRE" 14
		       "SSKE",     // 2830 "B22B" "SSKE" "RRE" 14
		       "TB",       // 2840 "B22C" "TB" "RRE" 14
		       "DXR",      // 2850 "B22D" "DXR" "RRE" 14
		       "PGIN",     // 2860 "B22E" "PGIN" "RRE" 14
		       "PGOUT",    // 2870 "B22F" "PGOUT" "RRE" 14
		       "CSCH",     // 2880 "B230" "CSCH" "S" 7
		       "HSCH",     // 2890 "B231" "HSCH" "S" 7
		       "MSCH",     // 2900 "B232" "MSCH" "S" 7
		       "SSCH",     // 2910 "B233" "SSCH" "S" 7
		       "STSCH",    // 2920 "B234" "STSCH" "S" 7
		       "TSCH",     // 2930 "B235" "TSCH" "S" 7
		       "TPI",      // 2940 "B236" "TPI" "S" 7
		       "SAL",      // 2950 "B237" "SAL" "S" 7
		       "RSCH",     // 2960 "B238" "RSCH" "S" 7
		       "STCRW",    // 2970 "B239" "STCRW" "S" 7
		       "STCPS",    // 2980 "B23A" "STCPS" "S" 7
		       "RCHP",     // 2990 "B23B" "RCHP" "S" 7
		       "SCHM",     // 3000 "B23C" "SCHM" "S" 7
		       "BAKR",     // 3010 "B240" "BAKR" "RRE" 14
		       "CKSM",     // 3020 "B241" "CKSM" "RRE" 14
		       "SQDR",     // 3030 "B244" "SQDR" "RRE" 14
		       "SQER",     // 3040 "B245" "SQER" "RRE" 14
		       "STURA",    // 3050 "B246" "STURA" "RRE" 14
		       "MSTA",     // 3060 "B247" "MSTA" "RRE" 14
		       "PALB",     // 3070 "B248" "PALB" "RRE" 14
		       "EREG",     // 3080 "B249" "EREG" "RRE" 14
		       "ESTA",     // 3090 "B24A" "ESTA" "RRE" 14
		       "LURA",     // 3100 "B24B" "LURA" "RRE" 14
		       "TAR",      // 3110 "B24C" "TAR" "RRE" 14
		       "CPYA",     // 3120 "B24D" "CPYA" "RRE" 14
		       "SAR",      // 3130 "B24E" "SAR" "RRE" 14
		       "EAR",      // 3140 "B24F" "EAR" "RRE" 14
		       "CSP",      // 3150 "B250" "CSP" "RRE" 14
		       "MSR",      // 3160 "B252" "MSR" "RRE" 14
		       "MVPG",     // 3170 "B254" "MVPG" "RRE" 14
		       "MVST",     // 3180 "B255" "MVST" "RRE" 14
		       "CUSE",     // 3190 "B257" "CUSE" "RRE" 14
		       "BSG",      // 3200 "B258" "BSG" "RRE" 14
		       "BSA",      // 3210 "B25A" "BSA" "RRE" 14
		       "CLST",     // 3220 "B25D" "CLST" "RRE" 14
		       "SRST",     // 3230 "B25E" "SRST" "RRE" 14
		       "CMPSC",    // 3240 "B263" "CMPSC" "RRE" 14
		       "XSCH",     // 3250 "B276" "XSCH" "S" 7
		       "RP",       // 3260 "B277" "RP" "S" 7
		       "STCKE",    // 3270 "B278" "STCKE" "S" 7
		       "SACF",     // 3280 "B279" "SACF" "S" 7
		       "STSI",     // 3290 "B27D" "STSI" "S" 7
		       "SRNM",     // 3300 "B299" "SRNM" "S" 7
		       "STFPC",    // 3310 "B29C" "STFPC" "S" 7
		       "LFPC",     // 3320 "B29D" "LFPC" "S" 7
		       "TRE",      // 3330 "B2A5" "TRE" "RRE" 14
		       "CUUTF",    // 3340 "B2A6" "CUUTF" "RRE" 14
		       "CU21",     // 3350 "B2A6" "CU21" "RRE" 14
		       "CUTFU",    // 3360 "B2A7" "CUTFU" "RRE" 14
		       "CU12",     // 3370 "B2A7" "CU12" "RRE" 14
		       "STFL",     // 3380 "B2B1" "STFL" "S" 7
		       "LPSWE",    // 3390 "B2B2" "LPSWE" "S" 7
		       "TRAP4",    // 3400 "B2FF" "TRAP4" "S" 7
		       "LPEBR",    // 3410 "B300" "LPEBR" "RRE" 14
		       "LNEBR",    // 3420 "B301" "LNEBR" "RRE" 14
		       "LTEBR",    // 3430 "B302" "LTEBR" "RRE" 14
		       "LCEBR",    // 3440 "B303" "LCEBR" "RRE" 14
		       "LDEBR",    // 3450 "B304" "LDEBR" "RRE" 14
		       "LXDBR",    // 3460 "B305" "LXDBR" "RRE" 14
		       "LXEBR",    // 3470 "B306" "LXEBR" "RRE" 14
		       "MXDBR",    // 3480 "B307" "MXDBR" "RRE" 14
		       "KEBR",     // 3490 "B308" "KEBR" "RRE" 14
		       "CEBR",     // 3500 "B309" "CEBR" "RRE" 14
		       "AEBR",     // 3510 "B30A" "AEBR" "RRE" 14
		       "SEBR",     // 3520 "B30B" "SEBR" "RRE" 14
		       "MDEBR",    // 3530 "B30C" "MDEBR" "RRE" 14
		       "DEBR",     // 3540 "B30D" "DEBR" "RRE" 14
		       "MAEBR",    // 3550 "B30E" "MAEBR" "RRF" 15
		       "MSEBR",    // 3560 "B30F" "MSEBR" "RRF" 15
		       "LPDBR",    // 3570 "B310" "LPDBR" "RRE" 14
		       "LNDBR",    // 3580 "B311" "LNDBR" "RRE" 14
		       "LTDBR",    // 3590 "B312" "LTDBR" "RRE" 14
		       "LCDBR",    // 3600 "B313" "LCDBR" "RRE" 14
		       "SQEBR",    // 3610 "B314" "SQEBR" "RRE" 14
		       "SQDBR",    // 3620 "B315" "SQDBR" "RRE" 14
		       "SQXBR",    // 3630 "B316" "SQXBR" "RRE" 14
		       "MEEBR",    // 3640 "B317" "MEEBR" "RRE" 14
		       "KDBR",     // 3650 "B318" "KDBR" "RRE" 14
		       "CDBR",     // 3660 "B319" "CDBR" "RRE" 14
		       "ADBR",     // 3670 "B31A" "ADBR" "RRE" 14
		       "SDBR",     // 3680 "B31B" "SDBR" "RRE" 14
		       "MDBR",     // 3690 "B31C" "MDBR" "RRE" 14
		       "DDBR",     // 3700 "B31D" "DDBR" "RRE" 14
		       "MADBR",    // 3710 "B31E" "MADBR" "RRF" 15
		       "MSDBR",    // 3720 "B31F" "MSDBR" "RRF" 15
		       "LDER",     // 3730 "B324" "LDER" "RRE" 14
		       "LXDR",     // 3740 "B325" "LXDR" "RRE" 14
		       "LXER",     // 3750 "B326" "LXER" "RRE" 14
		       "MAER",     // 3760 "B32E" "MAER" "RRF" 15
		       "MSER",     // 3770 "B32F" "MSER" "RRF" 15
		       "SQXR",     // 3780 "B336" "SQXR" "RRE" 14
		       "MEER",     // 3790 "B337" "MEER" "RRE" 14
		       "MADR",     // 3800 "B33E" "MADR" "RRF" 15
		       "MSDR",     // 3810 "B33F" "MSDR" "RRF" 15
		       "LPXBR",    // 3820 "B340" "LPXBR" "RRE" 14
		       "LNXBR",    // 3830 "B341" "LNXBR" "RRE" 14
		       "LTXBR",    // 3840 "B342" "LTXBR" "RRE" 14
		       "LCXBR",    // 3850 "B343" "LCXBR" "RRE" 14
		       "LEDBR",    // 3860 "B344" "LEDBR" "RRE" 14
		       "LDXBR",    // 3870 "B345" "LDXBR" "RRE" 14
		       "LEXBR",    // 3880 "B346" "LEXBR" "RRE" 14
		       "FIXBR",    // 3890 "B347" "FIXBR" "RRF" 15
		       "KXBR",     // 3900 "B348" "KXBR" "RRE" 14
		       "CXBR",     // 3910 "B349" "CXBR" "RRE" 14
		       "AXBR",     // 3920 "B34A" "AXBR" "RRE" 14
		       "SXBR",     // 3930 "B34B" "SXBR" "RRE" 14
		       "MXBR",     // 3940 "B34C" "MXBR" "RRE" 14
		       "DXBR",     // 3950 "B34D" "DXBR" "RRE" 14
		       "TBEDR",    // 3960 "B350" "TBEDR" "RRF" 15
		       "TBDR",     // 3970 "B351" "TBDR" "RRF" 15
		       "DIEBR",    // 3980 "B353" "DIEBR" "RRF" 15
		       "FIEBR",    // 3990 "B357" "FIEBR" "RRF" 15
		       "THDER",    // 4000 "B358" "THDER" "RRE" 14
		       "THDR",     // 4010 "B359" "THDR" "RRE" 14
		       "DIDBR",    // 4020 "B35B" "DIDBR" "RRF" 15
		       "FIDBR",    // 4030 "B35F" "FIDBR" "RRF" 15
		       "LPXR",     // 4040 "B360" "LPXR" "RRE" 14
		       "LNXR",     // 4050 "B361" "LNXR" "RRE" 14
		       "LTXR",     // 4060 "B362" "LTXR" "RRE" 14
		       "LCXR",     // 4070 "B363" "LCXR" "RRE" 14
		       "LXR",      // 4080 "B365" "LXR" "RRE" 14
		       "LEXR",     // 4090 "B366" "LEXR" "RRE" 14
		       "FIXR",     // 4100 "B367" "FIXR" "RRE" 14
		       "CXR",      // 4110 "B369" "CXR" "RRE" 14
		       "LZER",     // 4120 "B374" "LZER" "RRE" 14
		       "LZDR",     // 4130 "B375" "LZDR" "RRE" 14
		       "LZXR",     // 4140 "B376" "LZXR" "RRE" 14
		       "FIER",     // 4150 "B377" "FIER" "RRE" 14
		       "FIDR",     // 4160 "B37F" "FIDR" "RRE" 14
		       "SFPC",     // 4170 "B384" "SFPC" "RRE" 14
		       "EFPC",     // 4180 "B38C" "EFPC" "RRE" 14
		       "CEFBR",    // 4190 "B394" "CEFBR" "RRE" 14
		       "CDFBR",    // 4200 "B395" "CDFBR" "RRE" 14
		       "CXFBR",    // 4210 "B396" "CXFBR" "RRE" 14
		       "CFEBR",    // 4220 "B398" "CFEBR" "RRF" 15
		       "CFDBR",    // 4230 "B399" "CFDBR" "RRF" 15
		       "CFXBR",    // 4240 "B39A" "CFXBR" "RRF" 15
		       "CEGBR",    // 4250 "B3A4" "CEGBR" "RRE" 14
		       "CDGBR",    // 4260 "B3A5" "CDGBR" "RRE" 14
		       "CXGBR",    // 4270 "B3A6" "CXGBR" "RRE" 14
		       "CGEBR",    // 4280 "B3A8" "CGEBR" "RRF" 15
		       "CGDBR",    // 4290 "B3A9" "CGDBR" "RRF" 15
		       "CGXBR",    // 4300 "B3AA" "CGXBR" "RRF" 15
		       "CEFR",     // 4310 "B3B4" "CEFR" "RRE" 14
		       "CDFR",     // 4320 "B3B5" "CDFR" "RRE" 14
		       "CXFR",     // 4330 "B3B6" "CXFR" "RRE" 14
		       "CFER",     // 4340 "B3B8" "CFER" "RRF" 15
		       "CFDR",     // 4350 "B3B9" "CFDR" "RRF" 15
		       "CFXR",     // 4360 "B3BA" "CFXR" "RRF" 15
		       "CEGR",     // 4370 "B3C4" "CEGR" "RRE" 14
		       "CDGR",     // 4380 "B3C5" "CDGR" "RRE" 14
		       "CXGR",     // 4390 "B3C6" "CXGR" "RRE" 14
		       "CGER",     // 4400 "B3C8" "CGER" "RRF" 15
		       "CGDR",     // 4410 "B3C9" "CGDR" "RRF" 15
		       "CGXR",     // 4420 "B3CA" "CGXR" "RRF" 15
		       "STCTL",    // 4430 "B6" "STCTL" "RS" 10
		       "LCTL",     // 4440 "B7" "LCTL" "RS" 10
		       "LPGR",     // 4450 "B900" "LPGR" "RRE" 14
		       "LNGR",     // 4460 "B901" "LNGR" "RRE" 14
		       "LTGR",     // 4470 "B902" "LTGR" "RRE" 14
		       "LCGR",     // 4480 "B903" "LCGR" "RRE" 14
		       "LGR",      // 4490 "B904" "LGR" "RRE" 14
		       "LURAG",    // 4500 "B905" "LURAG" "RRE" 14
		       "AGR",      // 4510 "B908" "AGR" "RRE" 14
		       "SGR",      // 4520 "B909" "SGR" "RRE" 14
		       "ALGR",     // 4530 "B90A" "ALGR" "RRE" 14
		       "SLGR",     // 4540 "B90B" "SLGR" "RRE" 14
		       "MSGR",     // 4550 "B90C" "MSGR" "RRE" 14
		       "DSGR",     // 4560 "B90D" "DSGR" "RRE" 14
		       "EREGG",    // 4570 "B90E" "EREGG" "RRE" 14
		       "LRVGR",    // 4580 "B90F" "LRVGR" "RRE" 14
		       "LPGFR",    // 4590 "B910" "LPGFR" "RRE" 14
		       "LNGFR",    // 4600 "B911" "LNGFR" "RRE" 14
		       "LTGFR",    // 4610 "B912" "LTGFR" "RRE" 14
		       "LCGFR",    // 4620 "B913" "LCGFR" "RRE" 14
		       "LGFR",     // 4630 "B914" "LGFR" "RRE" 14
		       "LLGFR",    // 4640 "B916" "LLGFR" "RRE" 14
		       "LLGTR",    // 4650 "B917" "LLGTR" "RRE" 14
		       "AGFR",     // 4660 "B918" "AGFR" "RRE" 14
		       "SGFR",     // 4670 "B919" "SGFR" "RRE" 14
		       "ALGFR",    // 4680 "B91A" "ALGFR" "RRE" 14
		       "SLGFR",    // 4690 "B91B" "SLGFR" "RRE" 14
		       "MSGFR",    // 4700 "B91C" "MSGFR" "RRE" 14
		       "DSGFR",    // 4710 "B91D" "DSGFR" "RRE" 14
		       "KMAC",     // 4720 "B91E" "KMAC" "RRE" 14
		       "LRVR",     // 4730 "B91F" "LRVR" "RRE" 14
		       "CGR",      // 4740 "B920" "CGR" "RRE" 14
		       "CLGR",     // 4750 "B921" "CLGR" "RRE" 14
		       "STURG",    // 4760 "B925" "STURG" "RRE" 14
		       "KM",       // 4770 "B92E" "KM" "RRE" 14
		       "KMC",      // 4780 "B92F" "KMC" "RRE" 14
		       "CGFR",     // 4790 "B930" "CGFR" "RRE" 14
		       "CLGFR",    // 4800 "B931" "CLGFR" "RRE" 14
		       "KIMD",     // 4810 "B93E" "KIMD" "RRE" 14
		       "KLMD",     // 4820 "B93F" "KLMD" "RRE" 14
		       "BCTGR",    // 4830 "B946" "BCTGR" "RRE" 14
		       "NGR",      // 4840 "B980" "NGR" "RRE" 14
		       "OGR",      // 4850 "B981" "OGR" "RRE" 14
		       "XGR",      // 4860 "B982" "XGR" "RRE" 14
		       "MLGR",     // 4870 "B986" "MLGR" "RRE" 14
		       "DLGR",     // 4880 "B987" "DLGR" "RRE" 14
		       "ALCGR",    // 4890 "B988" "ALCGR" "RRE" 14
		       "SLBGR",    // 4900 "B989" "SLBGR" "RRE" 14
		       "CSPG",     // 4910 "B98A" "CSPG" "RRE" 14
		       "EPSW",     // 4920 "B98D" "EPSW" "RRE" 14
		       "IDTE",     // 4930 "B98E" "IDTE" "RRF" 15
		       "TRTT",     // 4940 "B990" "TRTT" "RRE" 14
		       "TRTO",     // 4950 "B991" "TRTO" "RRE" 14
		       "TROT",     // 4960 "B992" "TROT" "RRE" 14
		       "TROO",     // 4970 "B993" "TROO" "RRE" 14
		       "MLR",      // 4980 "B996" "MLR" "RRE" 14
		       "DLR",      // 4990 "B997" "DLR" "RRE" 14
		       "ALCR",     // 5000 "B998" "ALCR" "RRE" 14
		       "SLBR",     // 5010 "B999" "SLBR" "RRE" 14
		       "EPAIR",    // 5020 "B99A" "EPAIR" "RRE" 14
		       "ESAIR",    // 5030 "B99B" "ESAIR" "RRE" 14
		       "ESEA",     // 5040 "B99D" "ESEA" "RRE" 14
		       "PTI",      // 5050 "B99E" "PTI" "RRE" 14
		       "SSAIR",    // 5060 "B99F" "SSAIR" "RRE" 14
		       "CU14",     // 5070 "B9B0" "CU14" "RRE" 14
		       "CU24",     // 5080 "B9B1" "CU24" "RRE" 14
		       "CU41",     // 5090 "B9B2" "CU41" "RRE" 14
		       "CU42",     // 5100 "B9B3" "CU42" "RRE" 14
		       "SRSTU",    // 5110 "B9BE" "SRSTU" "RRE" 14
		       "CS",       // 5120 "BA" "CS" "RS" 10
		       "CDS",      // 5130 "BB" "CDS" "RS" 10
		       "CLM",      // 5140 "BD" "CLM" "RS" 10
		       "STCM",     // 5150 "BE" "STCM" "RS" 10
		       "ICM",      // 5160 "BF" "ICM" "RS" 10
		       "LARL",     // 5170 "C00" "LARL" "RIL" 16
		       "BRCL",     // 5180 "C04" "BRCL" "RIL" 16
		       "BRUL",     // 5190 "C0F4" "BRUL" "RIL" 16
		       "BLU",      // 5200 "C0F4" "BLU" "RIL" 16
		       "BRASL",    // 5210 "C05" "BRASL" "RIL" 16
		       "JASL",     // 5220 "C05" "JASL" "RIL" 16
		       "TRTR",     // 5230 "D0" "TRTR" "SS" 17
		       "MVN",      // 5240 "D1" "MVN" "SS" 17
		       "MVC",      // 5250 "D2" "MVC" "SS" 17
		       "MVZ",      // 5260 "D3" "MVZ" "SS" 17
		       "NC",       // 5270 "D4" "NC" "SS" 17
		       "CLC",      // 5280 "D5" "CLC" "SS" 17
		       "OC",       // 5290 "D6" "OC" "SS" 17
		       "XC",       // 5300 "D7" "XC" "SS" 17
		       "MVCK",     // 5310 "D9" "MVCK" "SS" 17
		       "MVCP",     // 5320 "DA" "MVCP" "SS" 17
		       "MVCS",     // 5330 "DB" "MVCS" "SS" 17
		       "TR",       // 5340 "DC" "TR" "SS" 17
		       "TRT",      // 5350 "DD" "TRT" "SS" 17
		       "ED",       // 5360 "DE" "ED" "SS" 17
		       "EDMK",     // 5370 "DF" "EDMK" "SS" 17
		       "PKU",      // 5380 "E1" "PKU" "SS" 17
		       "UNPKU",    // 5390 "E2" "UNPKU" "SS" 17
		       "LRAG",     // 5400 "E303" "LRAG" "RXY" 18
		       "LG",       // 5410 "E304" "LG" "RXY" 18
		       "CVBY",     // 5420 "E306" "CVBY" "RXY" 18
		       "AG",       // 5430 "E308" "AG" "RXY" 18
		       "SG",       // 5440 "E309" "SG" "RXY" 18
		       "ALG",      // 5450 "E30A" "ALG" "RXY" 18
		       "SLG",      // 5460 "E30B" "SLG" "RXY" 18
		       "MSG",      // 5470 "E30C" "MSG" "RXY" 18
		       "DSG",      // 5480 "E30D" "DSG" "RXY" 18
		       "CVBG",     // 5490 "E30E" "CVBG" "RXY" 18
		       "LRVG",     // 5500 "E30F" "LRVG" "RXY" 18
		       "LRAY",     // 5510 "E313" "LRAY" "RXY" 18
		       "LGF",      // 5520 "E314" "LGF" "RXY" 18
		       "LGH",      // 5530 "E315" "LGH" "RXY" 18
		       "LLGF",     // 5540 "E316" "LLGF" "RXY" 18
		       "LLGT",     // 5550 "E317" "LLGT" "RXY" 18
		       "AGF",      // 5560 "E318" "AGF" "RXY" 18
		       "SGF",      // 5570 "E319" "SGF" "RXY" 18
		       "ALGF",     // 5580 "E31A" "ALGF" "RXY" 18
		       "SLGF",     // 5590 "E31B" "SLGF" "RXY" 18
		       "MSGF",     // 5600 "E31C" "MSGF" "RXY" 18
		       "DSGF",     // 5610 "E31D" "DSGF" "RXY" 18
		       "LRV",      // 5620 "E31E" "LRV" "RXY" 18
		       "LRVH",     // 5630 "E31F" "LRVH" "RXY" 18
		       "CG",       // 5640 "E320" "CG" "RXY" 18
		       "CLG",      // 5650 "E321" "CLG" "RXY" 18
		       "STG",      // 5660 "E324" "STG" "RXY" 18
		       "CVDY",     // 5670 "E326" "CVDY" "RXY" 18
		       "CVDG",     // 5680 "E32E" "CVDG" "RXY" 18
		       "STRVG",    // 5690 "E32F" "STRVG" "RXY" 18
		       "CGF",      // 5700 "E330" "CGF" "RXY" 18
		       "CLGF",     // 5710 "E331" "CLGF" "RXY" 18
		       "STRV",     // 5720 "E33E" "STRV" "RXY" 18
		       "STRVH",    // 5730 "E33F" "STRVH" "RXY" 18
		       "BCTG",     // 5740 "E346" "BCTG" "RXY" 18
		       "STY",      // 5750 "E350" "STY" "RXY" 18
		       "MSY",      // 5760 "E351" "MSY" "RXY" 18
		       "NY",       // 5770 "E354" "NY" "RXY" 18
		       "CLY",      // 5780 "E355" "CLY" "RXY" 18
		       "OY",       // 5790 "E356" "OY" "RXY" 18
		       "XY",       // 5800 "E357" "XY" "RXY" 18
		       "LY",       // 5810 "E358" "LY" "RXY" 18
		       "CY",       // 5820 "E359" "CY" "RXY" 18
		       "AY",       // 5830 "E35A" "AY" "RXY" 18
		       "SY",       // 5840 "E35B" "SY" "RXY" 18
		       "ALY",      // 5850 "E35E" "ALY" "RXY" 18
		       "SLY",      // 5860 "E35F" "SLY" "RXY" 18
		       "STHY",     // 5870 "E370" "STHY" "RXY" 18
		       "LAY",      // 5880 "E371" "LAY" "RXY" 18
		       "STCY",     // 5890 "E372" "STCY" "RXY" 18
		       "ICY",      // 5900 "E373" "ICY" "RXY" 18
		       "LB",       // 5910 "E376" "LB" "RXY" 18
		       "LGB",      // 5920 "E377" "LGB" "RXY" 18
		       "LHY",      // 5930 "E378" "LHY" "RXY" 18
		       "CHY",      // 5940 "E379" "CHY" "RXY" 18
		       "AHY",      // 5950 "E37A" "AHY" "RXY" 18
		       "SHY",      // 5960 "E37B" "SHY" "RXY" 18
		       "NG",       // 5970 "E380" "NG" "RXY" 18
		       "OG",       // 5980 "E381" "OG" "RXY" 18
		       "XG",       // 5990 "E382" "XG" "RXY" 18
		       "MLG",      // 6000 "E386" "MLG" "RXY" 18
		       "DLG",      // 6010 "E387" "DLG" "RXY" 18
		       "ALCG",     // 6020 "E388" "ALCG" "RXY" 18
		       "SLBG",     // 6030 "E389" "SLBG" "RXY" 18
		       "STPQ",     // 6040 "E38E" "STPQ" "RXY" 18
		       "LPQ",      // 6050 "E38F" "LPQ" "RXY" 18
		       "LLGC",     // 6060 "E390" "LLGC" "RXY" 18
		       "LLGH",     // 6070 "E391" "LLGH" "RXY" 18
		       "ML",       // 6080 "E396" "ML" "RXY" 18
		       "DL",       // 6090 "E397" "DL" "RXY" 18
		       "ALC",      // 6100 "E398" "ALC" "RXY" 18
		       "SLB",      // 6110 "E399" "SLB" "RXY" 18
		       "LASP",     // 6120 "E500" "LASP" "SSE" 19
		       "TPROT",    // 6130 "E501" "TPROT" "SSE" 19
		       "STRAG",    // 6140 "E502" "STRAG" "SSE" 19
		       "MVCSK",    // 6150 "E50E" "MVCSK" "SSE" 19
		       "MVCDK",    // 6160 "E50F" "MVCDK" "SSE" 19
		       "MVCIN",    // 6170 "E8" "MVCIN" "SS" 17
		       "PKA",      // 6180 "E9" "PKA" "SS" 17
		       "UNPKA",    // 6190 "EA" "UNPKA" "SS" 17
		       "LMG",      // 6200 "EB04" "LMG" "RSY" 20
		       "SRAG",     // 6210 "EB0A" "SRAG" "RSY" 20
		       "SLAG",     // 6220 "EB0B" "SLAG" "RSY" 20
		       "SRLG",     // 6230 "EB0C" "SRLG" "RSY" 20
		       "SLLG",     // 6240 "EB0D" "SLLG" "RSY" 20
		       "TRACG",    // 6250 "EB0F" "TRACG" "RSY" 20
		       "CSY",      // 6260 "EB14" "CSY" "RSY" 20
		       "RLLG",     // 6270 "EB1C" "RLLG" "RSY" 20
		       "RLL",      // 6280 "EB1D" "RLL" "RSY" 20
		       "CLMH",     // 6290 "EB20" "CLMH" "RSY" 20
		       "CLMY",     // 6300 "EB21" "CLMY" "RSY" 20
		       "STMG",     // 6310 "EB24" "STMG" "RSY" 20
		       "STCTG",    // 6320 "EB25" "STCTG" "RSY" 20
		       "STMH",     // 6330 "EB26" "STMH" "RSY" 20
		       "STCMH",    // 6340 "EB2C" "STCMH" "RSY" 20
		       "STCMY",    // 6350 "EB2D" "STCMY" "RSY" 20
		       "LCTLG",    // 6360 "EB2F" "LCTLG" "RSY" 20
		       "CSG",      // 6370 "EB30" "CSG" "RSY" 20
		       "CDSY",     // 6380 "EB31" "CDSY" "RSY" 20
		       "CDSG",     // 6390 "EB3E" "CDSG" "RSY" 20
		       "BXHG",     // 6400 "EB44" "BXHG" "RSY" 20
		       "BXLEG",    // 6410 "EB45" "BXLEG" "RSY" 20
		       "TMY",      // 6420 "EB51" "TMY" "SIY" 21
		       "MVIY",     // 6430 "EB52" "MVIY" "SIY" 21
		       "NIY",      // 6440 "EB54" "NIY" "SIY" 21
		       "CLIY",     // 6450 "EB55" "CLIY" "SIY" 21
		       "OIY",      // 6460 "EB56" "OIY" "SIY" 21
		       "XIY",      // 6470 "EB57" "XIY" "SIY" 21
		       "ICMH",     // 6480 "EB80" "ICMH" "RSY" 20
		       "ICMY",     // 6490 "EB81" "ICMY" "RSY" 20
		       "MVCLU",    // 6500 "EB8E" "MVCLU" "RSY" 20
		       "CLCLU",    // 6510 "EB8F" "CLCLU" "RSY" 20
		       "STMY",     // 6520 "EB90" "STMY" "RSY" 20
		       "LMH",      // 6530 "EB96" "LMH" "RSY" 20
		       "LMY",      // 6540 "EB98" "LMY" "RSY" 20
		       "LAMY",     // 6550 "EB9A" "LAMY" "RSY" 20
		       "STAMY",    // 6560 "EB9B" "STAMY" "RSY" 20
		       "TP",       // 6570 "EBC0" "TP" "RSL" 22
		       "BRXHG",    // 6580 "EC44" "BRXHG" "RIE" 23
		       "JXHG",     // 6590 "EC44" "JXHG" "RIE" 23
		       "BRXLG",    // 6600 "EC45" "BRXLG" "RIE" 23
		       "JXLG",     // 6610 "EC45" "JXLG" "RIE" 23
		       "LDEB",     // 6620 "ED04" "LDEB" "RXE" 24
		       "LXDB",     // 6630 "ED05" "LXDB" "RXE" 24
		       "LXEB",     // 6640 "ED06" "LXEB" "RXE" 24
		       "MXDB",     // 6650 "ED07" "MXDB" "RXE" 24
		       "KEB",      // 6660 "ED08" "KEB" "RXE" 24
		       "CEB",      // 6670 "ED09" "CEB" "RXE" 24
		       "AEB",      // 6680 "ED0A" "AEB" "RXE" 24
		       "SEB",      // 6690 "ED0B" "SEB" "RXE" 24
		       "MDEB",     // 6700 "ED0C" "MDEB" "RXE" 24
		       "DEB",      // 6710 "ED0D" "DEB" "RXE" 24
		       "MAEB",     // 6720 "ED0E" "MAEB" "RXF" 25
		       "MSEB",     // 6730 "ED0F" "MSEB" "RXF" 25
		       "TCEB",     // 6740 "ED10" "TCEB" "RXE" 24
		       "TCDB",     // 6750 "ED11" "TCDB" "RXE" 24
		       "TCXB",     // 6760 "ED12" "TCXB" "RXE" 24
		       "SQEB",     // 6770 "ED14" "SQEB" "RXE" 24
		       "SQDB",     // 6780 "ED15" "SQDB" "RXE" 24
		       "MEEB",     // 6790 "ED17" "MEEB" "RXE" 24
		       "KDB",      // 6800 "ED18" "KDB" "RXE" 24
		       "CDB",      // 6810 "ED19" "CDB" "RXE" 24
		       "ADB",      // 6820 "ED1A" "ADB" "RXE" 24
		       "SDB",      // 6830 "ED1B" "SDB" "RXE" 24
		       "MDB",      // 6840 "ED1C" "MDB" "RXE" 24
		       "DDB",      // 6850 "ED1D" "DDB" "RXE" 24
		       "MADB",     // 6860 "ED1E" "MADB" "RXF" 25
		       "MSDB",     // 6870 "ED1F" "MSDB" "RXF" 25
		       "LDE",      // 6880 "ED24" "LDE" "RXE" 24
		       "LXD",      // 6890 "ED25" "LXD" "RXE" 24
		       "LXE",      // 6900 "ED26" "LXE" "RXE" 24
		       "MAE",      // 6910 "ED2E" "MAE" "RXF" 25
		       "MSE",      // 6920 "ED2F" "MSE" "RXF" 25
		       "SQE",      // 6930 "ED34" "SQE" "RXE" 24
		       "SQD",      // 6940 "ED35" "SQD" "RXE" 24
		       "MEE",      // 6950 "ED37" "MEE" "RXE" 24
		       "MAD",      // 6960 "ED3E" "MAD" "RXF" 25
		       "MSD",      // 6970 "ED3F" "MSD" "RXF" 25
		       "LEY",      // 6980 "ED64" "LEY" "RXY" 18
		       "LDY",      // 6990 "ED65" "LDY" "RXY" 18
		       "STEY",     // 7000 "ED66" "STEY" "RXY" 18
		       "STDY",     // 7010 "ED67" "STDY" "RXY" 18
		       "PLO",      // 7020 "EE" "PLO" "SS3" 27
		       "LMD",      // 7030 "EF" "LMD" "SS4" 28
		       "SRP",      // 7040 "F0" "SRP" "SS5" 29
		       "MVO",      // 7050 "F1" "MVO" "SS2" 26
		       "PACK",     // 7060 "F2" "PACK" "SS2" 26
		       "UNPK",     // 7070 "F3" "UNPK" "SS2" 26
		       "ZAP",      // 7080 "F8" "ZAP" "SS2" 26
		       "CP",       // 7090 "F9" "CP" "SS2" 26
		       "AP",       // 7100 "FA" "AP" "SS2" 26
		       "SP",       // 7110 "FB" "SP" "SS2" 26
		       "MP",       // 7120 "FC" "MP" "SS2" 26
		       "DP",      // 7130 "FD" "DP" "SS2" 26
      };
	int max_op = op_name.length;
  	int[]    op_type  = {
			   0,  // comments
		       1,  // 10 "0101" "PR" "E" 1
		       1,  // 20 "0102" "UPT" "E" 1
		       1,  // 30 "0107" "SCKPF" "E" 1
		       1,  // 40 "010B" "TAM" "E" 1
		       1,  // 50 "010C" "SAM24" "E" 1
		       1,  // 60 "010D" "SAM31" "E" 1
		       1,  // 70 "010E" "SAM64" "E" 1
		       1,  // 80 "01FF" "TRAP2" "E" 1
		       2,  // 90 "04" "SPM" "RR" 2
		       2,  // 100 "05" "BALR" "RR" 2
		       2,  // 110 "06" "BCTR" "RR" 2
		       2,  // 120 "07" "BCR" "RR" 2
		       3,  // 130 "07F" "BR" "BRX" 3
		       3,  // 140 "070" "NOPR" "BRX" 3
		       3,  // 150 "072" "BHR" "BRX" 3
		       3,  // 160 "074" "BLR" "BRX" 3
		       3,  // 170 "078" "BER" "BRX" 3
		       3,  // 180 "07D" "BNHR" "BRX" 3
		       3,  // 190 "07B" "BNLR" "BRX" 3
		       3,  // 200 "077" "BNER" "BRX" 3
		       3,  // 210 "072" "BPR" "BRX" 3
		       3,  // 220 "071" "BOR" "BRX" 3
		       3,  // 230 "074" "BMR" "BRX" 3
		       3,  // 240 "078" "BZR" "BRX" 3
		       3,  // 250 "07D" "BNPR" "BRX" 3
		       3,  // 260 "07B" "BNMR" "BRX" 3
		       3,  // 270 "077" "BNZR" "BRX" 3
		       3,  // 280 "07E" "BNOR" "BRX" 3
		       4,  // 290 "0A" "SVC" "I" 4
		       2,  // 300 "0B" "BSM" "RR" 2
		       2,  // 310 "0C" "BASSM" "RR" 2
		       2,  // 320 "0D" "BASR" "RR" 2
		       2,  // 330 "0E" "MVCL" "RR" 2
		       2,  // 340 "0F" "CLCL" "RR" 2
		       2,  // 350 "10" "LPR" "RR" 2
		       2,  // 360 "11" "LNR" "RR" 2
		       2,  // 370 "12" "LTR" "RR" 2
		       2,  // 380 "13" "LCR" "RR" 2
		       2,  // 390 "14" "NR" "RR" 2
		       2,  // 400 "15" "CLR" "RR" 2
		       2,  // 410 "16" "OR" "RR" 2
		       2,  // 420 "17" "XR" "RR" 2
		       2,  // 430 "18" "LR" "RR" 2
		       2,  // 440 "19" "CR" "RR" 2
		       2,  // 450 "1A" "AR" "RR" 2
		       2,  // 460 "1B" "SR" "RR" 2
		       2,  // 470 "1C" "MR" "RR" 2
		       2,  // 480 "1D" "DR" "RR" 2
		       2,  // 490 "1E" "ALR" "RR" 2
		       2,  // 500 "1F" "SLR" "RR" 2
		       2,  // 510 "20" "LPDR" "RR" 2
		       2,  // 520 "21" "LNDR" "RR" 2
		       2,  // 530 "22" "LTDR" "RR" 2
		       2,  // 540 "23" "LCDR" "RR" 2
		       2,  // 550 "24" "HDR" "RR" 2
		       2,  // 560 "25" "LDXR" "RR" 2
		       2,  // 570 "25" "LRDR" "RR" 2
		       2,  // 580 "26" "MXR" "RR" 2
		       2,  // 590 "27" "MXDR" "RR" 2
		       2,  // 600 "28" "LDR" "RR" 2
		       2,  // 610 "29" "CDR" "RR" 2
		       2,  // 620 "2A" "ADR" "RR" 2
		       2,  // 630 "2B" "SDR" "RR" 2
		       2,  // 640 "2C" "MDR" "RR" 2
		       2,  // 650 "2D" "DDR" "RR" 2
		       2,  // 660 "2E" "AWR" "RR" 2
		       2,  // 670 "2F" "SWR" "RR" 2
		       2,  // 680 "30" "LPER" "RR" 2
		       2,  // 690 "31" "LNER" "RR" 2
		       2,  // 700 "32" "LTER" "RR" 2
		       2,  // 710 "33" "LCER" "RR" 2
		       2,  // 720 "34" "HER" "RR" 2
		       2,  // 730 "35" "LEDR" "RR" 2
		       2,  // 740 "35" "LRER" "RR" 2
		       2,  // 750 "36" "AXR" "RR" 2
		       2,  // 760 "37" "SXR" "RR" 2
		       2,  // 770 "38" "LER" "RR" 2
		       2,  // 780 "39" "CER" "RR" 2
		       2,  // 790 "3A" "AER" "RR" 2
		       2,  // 800 "3B" "SER" "RR" 2
		       2,  // 810 "3C" "MDER" "RR" 2
		       2,  // 820 "3C" "MER" "RR" 2
		       2,  // 830 "3D" "DER" "RR" 2
		       2,  // 840 "3E" "AUR" "RR" 2
		       2,  // 850 "3F" "SUR" "RR" 2
		       5,  // 860 "40" "STH" "RX" 5
		       5,  // 870 "41" "LA" "RX" 5
		       5,  // 880 "42" "STC" "RX" 5
		       5,  // 890 "43" "IC" "RX" 5
		       5,  // 900 "44" "EX" "RX" 5
		       5,  // 910 "45" "BAL" "RX" 5
		       5,  // 920 "46" "BCT" "RX" 5
		       5,  // 930 "47" "BC" "RX" 5
		       6,  // 940 "47F" "B" "BCX" 6
		       6,  // 950 "470" "NOP" "BCX" 6
		       6,  // 960 "472" "BH" "BCX" 6
		       6,  // 970 "474" "BL" "BCX" 6
		       6,  // 980 "478" "BE" "BCX" 6
		       6,  // 990 "47D" "BNH" "BCX" 6
		       6,  // 1000 "47B" "BNL" "BCX" 6
		       6,  // 1010 "477" "BNE" "BCX" 6
		       6,  // 1020 "472" "BP" "BCX" 6
		       6,  // 1030 "471" "BO" "BCX" 6
		       6,  // 1040 "474" "BM" "BCX" 6
		       6,  // 1050 "478" "BZ" "BCX" 6
		       6,  // 1060 "47D" "BNP" "BCX" 6
		       6,  // 1070 "47B" "BNM" "BCX" 6
		       6,  // 1080 "477" "BNZ" "BCX" 6
		       6,  // 1090 "47E" "BNO" "BCX" 6
		       5,  // 1100 "48" "LH" "RX" 5
		       5,  // 1110 "49" "CH" "RX" 5
		       5,  // 1120 "4A" "AH" "RX" 5
		       5,  // 1130 "4B" "SH" "RX" 5
		       5,  // 1140 "4C" "MH" "RX" 5
		       5,  // 1150 "4D" "BAS" "RX" 5
		       5,  // 1160 "4E" "CVD" "RX" 5
		       5,  // 1170 "4F" "CVB" "RX" 5
		       5,  // 1180 "50" "ST" "RX" 5
		       5,  // 1190 "51" "LAE" "RX" 5
		       5,  // 1200 "54" "N" "RX" 5
		       5,  // 1210 "55" "CL" "RX" 5
		       5,  // 1220 "56" "O" "RX" 5
		       5,  // 1230 "57" "X" "RX" 5
		       5,  // 1240 "58" "L" "RX" 5
		       5,  // 1250 "59" "C" "RX" 5
		       5,  // 1260 "5A" "A" "RX" 5
		       5,  // 1270 "5B" "S" "RX" 5
		       5,  // 1280 "5C" "M" "RX" 5
		       5,  // 1290 "5D" "D" "RX" 5
		       5,  // 1300 "5E" "AL" "RX" 5
		       5,  // 1310 "5F" "SL" "RX" 5
		       5,  // 1320 "60" "STD" "RX" 5
		       5,  // 1330 "67" "MXD" "RX" 5
		       5,  // 1340 "68" "LD" "RX" 5
		       5,  // 1350 "69" "CD" "RX" 5
		       5,  // 1360 "6A" "AD" "RX" 5
		       5,  // 1370 "6B" "SD" "RX" 5
		       5,  // 1380 "6C" "MD" "RX" 5
		       5,  // 1390 "6D" "DD" "RX" 5
		       5,  // 1400 "6E" "AW" "RX" 5
		       5,  // 1410 "6F" "SW" "RX" 5
		       5,  // 1420 "70" "STE" "RX" 5
		       5,  // 1430 "71" "MS" "RX" 5
		       5,  // 1440 "78" "LE" "RX" 5
		       5,  // 1450 "79" "CE" "RX" 5
		       5,  // 1460 "7A" "AE" "RX" 5
		       5,  // 1470 "7B" "SE" "RX" 5
		       5,  // 1480 "7C" "MDE" "RX" 5
		       5,  // 1490 "7C" "ME" "RX" 5
		       5,  // 1500 "7D" "DE" "RX" 5
		       5,  // 1510 "7E" "AU" "RX" 5
		       5,  // 1520 "7F" "SU" "RX" 5
		       7,  // 1530 "8000" "SSM" "S" 7
		       7,  // 1540 "8202" "LPSW" "S" 7
		       8,  // 1550 "83" "DIAGNOSE" "DM" 8
		       9,  // 1560 "84" "BRXH" "RSI" 9
		       9,  // 1570 "84" "JXH" "RSI" 9
		       9,  // 1580 "85" "BRXLE" "RSI" 9
		       9,  // 1590 "85" "JXLE" "RSI" 9
		       10,  // 1600 "86" "BXH" "RS" 10
		       10,  // 1610 "87" "BXLE" "RS" 10
		       10,  // 1620 "88" "SRL" "RS" 10
		       10,  // 1630 "89" "SLL" "RS" 10
		       10,  // 1640 "8A" "SRA" "RS" 10
		       10,  // 1650 "8B" "SLA" "RS" 10
		       10,  // 1660 "8C" "SRDL" "RS" 10
		       10,  // 1670 "8D" "SLDL" "RS" 10
		       10,  // 1680 "8E" "SRDA" "RS" 10
		       10,  // 1690 "8F" "SLDA" "RS" 10
		       10,  // 1700 "90" "STM" "RS" 10
		       11,  // 1710 "91" "TM" "SI" 11
		       11,  // 1720 "92" "MVI" "SI" 11
		       7,  // 1730 "93" "TS" "S" 7
		       11,  // 1740 "94" "NI" "SI" 11
		       11,  // 1750 "95" "CLI" "SI" 11
		       11,  // 1760 "96" "OI" "SI" 11
		       11,  // 1770 "97" "XI" "SI" 11
		       10,  // 1780 "98" "LM" "RS" 10
		       10,  // 1790 "99" "TRACE" "RS" 10
		       10,  // 1800 "9A" "LAM" "RS" 10
		       10,  // 1810 "9B" "STAM" "RS" 10
		       12,  // 1820 "A50" "IIHH" "RI" 12
		       12,  // 1830 "A51" "IIHL" "RI" 12
		       12,  // 1840 "A52" "IILH" "RI" 12
		       12,  // 1850 "A53" "IILL" "RI" 12
		       12,  // 1860 "A54" "NIHH" "RI" 12
		       12,  // 1870 "A55" "NIHL" "RI" 12
		       12,  // 1880 "A56" "NILH" "RI" 12
		       12,  // 1890 "A57" "NILL" "RI" 12
		       12,  // 1900 "A58" "OIHH" "RI" 12
		       12,  // 1910 "A59" "OIHL" "RI" 12
		       12,  // 1920 "A5A" "OILH" "RI" 12
		       12,  // 1930 "A5B" "OILL" "RI" 12
		       12,  // 1940 "A5C" "LLIHH" "RI" 12
		       12,  // 1950 "A5D" "LLIHL" "RI" 12
		       12,  // 1960 "A5E" "LLILH" "RI" 12
		       12,  // 1970 "A5F" "LLILL" "RI" 12
		       12,  // 1980 "A70" "TMLH" "RI" 12
		       12,  // 1990 "A70" "TMH" "RI" 12
		       12,  // 2000 "A71" "TMLL" "RI" 12
		       12,  // 2010 "A71" "TML" "RI" 12
		       12,  // 2020 "A72" "TMHH" "RI" 12
		       12,  // 2030 "A73" "TMHL" "RI" 12
		       12,  // 2040 "A74" "BRC" "RI" 12
		       13,  // 2050 "A74F" "J" "BRC" 13
		       13,  // 2060 "A740" "JNOP" "BRC" 13
		       13,  // 2070 "A74F" "BRU" "BRC" 13
		       13,  // 2080 "A742" "BRH" "BRC" 13
		       13,  // 2090 "A744" "BRL" "BRC" 13
		       13,  // 2100 "A748" "BRE" "BRC" 13
		       13,  // 2110 "A74D" "BRNH" "BRC" 13
		       13,  // 2120 "A74B" "BRNL" "BRC" 13
		       13,  // 2130 "A747" "BRNE" "BRC" 13
		       13,  // 2140 "A742" "BRP" "BRC" 13
		       13,  // 2150 "A744" "BRM" "BRC" 13
		       13,  // 2160 "A748" "BRZ" "BRC" 13
		       13,  // 2170 "A741" "BRO" "BRC" 13
		       13,  // 2180 "A74D" "BRNP" "BRC" 13
		       13,  // 2190 "A74B" "BRNM" "BRC" 13
		       13,  // 2200 "A747" "BRNZ" "BRC" 13
		       13,  // 2210 "A74E" "BRNO" "BRC" 13
		       13,  // 2220 "A742" "JH" "BRC" 13
		       13,  // 2230 "A744" "JL" "BRC" 13
		       13,  // 2240 "A748" "JE" "BRC" 13
		       13,  // 2250 "A74D" "JNH" "BRC" 13
		       13,  // 2260 "A74B" "JNL" "BRC" 13
		       13,  // 2270 "A747" "JNE" "BRC" 13
		       13,  // 2280 "A742" "JP" "BRC" 13
		       13,  // 2290 "A744" "JM" "BRC" 13
		       13,  // 2300 "A748" "JZ" "BRC" 13
		       13,  // 2310 "A741" "JO" "BRC" 13
		       13,  // 2320 "A74D" "JNP" "BRC" 13
		       13,  // 2330 "A74B" "JNM" "BRC" 13
		       13,  // 2340 "A747" "JNZ" "BRC" 13
		       13,  // 2350 "A74E" "JNO" "BRC" 13
		       12,  // 2360 "A75" "BRAS" "RI" 12
		       12,  // 2370 "A75" "JAS" "RI" 12
		       12,  // 2380 "A76" "BRCT" "RI" 12
		       12,  // 2390 "A76" "JCT" "RI" 12
		       12,  // 2400 "A77" "BRCTG" "RI" 12
		       12,  // 2410 "A77" "JCTG" "RI" 12
		       12,  // 2420 "A78" "LHI" "RI" 12
		       12,  // 2430 "A79" "LGHI" "RI" 12
		       12,  // 2440 "A7A" "AHI" "RI" 12
		       12,  // 2450 "A7B" "AGHI" "RI" 12
		       12,  // 2460 "A7C" "MHI" "RI" 12
		       12,  // 2470 "A7D" "MGHI" "RI" 12
		       12,  // 2480 "A7E" "CHI" "RI" 12
		       12,  // 2490 "A7F" "CGHI" "RI" 12
		       10,  // 2500 "A8" "MVCLE" "RS" 10
		       10,  // 2510 "A9" "CLCLE" "RS" 10
		       11,  // 2520 "AC" "STNSM" "SI" 11
		       11,  // 2530 "AD" "STOSM" "SI" 11
		       10,  // 2540 "AE" "SIGP" "RS" 10
		       11,  // 2550 "AF" "MC" "SI" 11
		       5,  // 2560 "B1" "LRA" "RX" 5
		       7,  // 2570 "B202" "STIDP" "S" 7
		       7,  // 2580 "B204" "SCK" "S" 7
		       7,  // 2590 "B205" "STCK" "S" 7
		       7,  // 2600 "B206" "SCKC" "S" 7
		       7,  // 2610 "B207" "STCKC" "S" 7
		       7,  // 2620 "B208" "SPT" "S" 7
		       7,  // 2630 "B209" "STPT" "S" 7
		       7,  // 2640 "B20A" "SPKA" "S" 7
		       7,  // 2650 "B20B" "IPK" "S" 7
		       7,  // 2660 "B20D" "PTLB" "S" 7
		       7,  // 2670 "B210" "SPX" "S" 7
		       7,  // 2680 "B211" "STPX" "S" 7
		       7,  // 2690 "B212" "STAP" "S" 7
		       7,  // 2700 "B218" "PC" "S" 7
		       7,  // 2710 "B219" "SAC" "S" 7
		       7,  // 2720 "B21A" "CFC" "S" 7
		       14,  // 2730 "B221" "IPTE" "RRE" 14
		       14,  // 2740 "B222" "IPM" "RRE" 14
		       14,  // 2750 "B223" "IVSK" "RRE" 14
		       14,  // 2760 "B224" "IAC" "RRE" 14
		       14,  // 2770 "B225" "SSAR" "RRE" 14
		       14,  // 2780 "B226" "EPAR" "RRE" 14
		       14,  // 2790 "B227" "ESAR" "RRE" 14
		       14,  // 2800 "B228" "PT" "RRE" 14
		       14,  // 2810 "B229" "ISKE" "RRE" 14
		       14,  // 2820 "B22A" "RRBE" "RRE" 14
		       14,  // 2830 "B22B" "SSKE" "RRE" 14
		       14,  // 2840 "B22C" "TB" "RRE" 14
		       14,  // 2850 "B22D" "DXR" "RRE" 14
		       14,  // 2860 "B22E" "PGIN" "RRE" 14
		       14,  // 2870 "B22F" "PGOUT" "RRE" 14
		       7,  // 2880 "B230" "CSCH" "S" 7
		       7,  // 2890 "B231" "HSCH" "S" 7
		       7,  // 2900 "B232" "MSCH" "S" 7
		       7,  // 2910 "B233" "SSCH" "S" 7
		       7,  // 2920 "B234" "STSCH" "S" 7
		       7,  // 2930 "B235" "TSCH" "S" 7
		       7,  // 2940 "B236" "TPI" "S" 7
		       7,  // 2950 "B237" "SAL" "S" 7
		       7,  // 2960 "B238" "RSCH" "S" 7
		       7,  // 2970 "B239" "STCRW" "S" 7
		       7,  // 2980 "B23A" "STCPS" "S" 7
		       7,  // 2990 "B23B" "RCHP" "S" 7
		       7,  // 3000 "B23C" "SCHM" "S" 7
		       14,  // 3010 "B240" "BAKR" "RRE" 14
		       14,  // 3020 "B241" "CKSM" "RRE" 14
		       14,  // 3030 "B244" "SQDR" "RRE" 14
		       14,  // 3040 "B245" "SQER" "RRE" 14
		       14,  // 3050 "B246" "STURA" "RRE" 14
		       14,  // 3060 "B247" "MSTA" "RRE" 14
		       14,  // 3070 "B248" "PALB" "RRE" 14
		       14,  // 3080 "B249" "EREG" "RRE" 14
		       14,  // 3090 "B24A" "ESTA" "RRE" 14
		       14,  // 3100 "B24B" "LURA" "RRE" 14
		       14,  // 3110 "B24C" "TAR" "RRE" 14
		       14,  // 3120 "B24D" "CPYA" "RRE" 14
		       14,  // 3130 "B24E" "SAR" "RRE" 14
		       14,  // 3140 "B24F" "EAR" "RRE" 14
		       14,  // 3150 "B250" "CSP" "RRE" 14
		       14,  // 3160 "B252" "MSR" "RRE" 14
		       14,  // 3170 "B254" "MVPG" "RRE" 14
		       14,  // 3180 "B255" "MVST" "RRE" 14
		       14,  // 3190 "B257" "CUSE" "RRE" 14
		       14,  // 3200 "B258" "BSG" "RRE" 14
		       14,  // 3210 "B25A" "BSA" "RRE" 14
		       14,  // 3220 "B25D" "CLST" "RRE" 14
		       14,  // 3230 "B25E" "SRST" "RRE" 14
		       14,  // 3240 "B263" "CMPSC" "RRE" 14
		       7,  // 3250 "B276" "XSCH" "S" 7
		       7,  // 3260 "B277" "RP" "S" 7
		       7,  // 3270 "B278" "STCKE" "S" 7
		       7,  // 3280 "B279" "SACF" "S" 7
		       7,  // 3290 "B27D" "STSI" "S" 7
		       7,  // 3300 "B299" "SRNM" "S" 7
		       7,  // 3310 "B29C" "STFPC" "S" 7
		       7,  // 3320 "B29D" "LFPC" "S" 7
		       14,  // 3330 "B2A5" "TRE" "RRE" 14
		       14,  // 3340 "B2A6" "CUUTF" "RRE" 14
		       14,  // 3350 "B2A6" "CU21" "RRE" 14
		       14,  // 3360 "B2A7" "CUTFU" "RRE" 14
		       14,  // 3370 "B2A7" "CU12" "RRE" 14
		       7,  // 3380 "B2B1" "STFL" "S" 7
		       7,  // 3390 "B2B2" "LPSWE" "S" 7
		       7,  // 3400 "B2FF" "TRAP4" "S" 7
		       14,  // 3410 "B300" "LPEBR" "RRE" 14
		       14,  // 3420 "B301" "LNEBR" "RRE" 14
		       14,  // 3430 "B302" "LTEBR" "RRE" 14
		       14,  // 3440 "B303" "LCEBR" "RRE" 14
		       14,  // 3450 "B304" "LDEBR" "RRE" 14
		       14,  // 3460 "B305" "LXDBR" "RRE" 14
		       14,  // 3470 "B306" "LXEBR" "RRE" 14
		       14,  // 3480 "B307" "MXDBR" "RRE" 14
		       14,  // 3490 "B308" "KEBR" "RRE" 14
		       14,  // 3500 "B309" "CEBR" "RRE" 14
		       14,  // 3510 "B30A" "AEBR" "RRE" 14
		       14,  // 3520 "B30B" "SEBR" "RRE" 14
		       14,  // 3530 "B30C" "MDEBR" "RRE" 14
		       14,  // 3540 "B30D" "DEBR" "RRE" 14
		       15,  // 3550 "B30E" "MAEBR" "RRF" 15
		       15,  // 3560 "B30F" "MSEBR" "RRF" 15
		       14,  // 3570 "B310" "LPDBR" "RRE" 14
		       14,  // 3580 "B311" "LNDBR" "RRE" 14
		       14,  // 3590 "B312" "LTDBR" "RRE" 14
		       14,  // 3600 "B313" "LCDBR" "RRE" 14
		       14,  // 3610 "B314" "SQEBR" "RRE" 14
		       14,  // 3620 "B315" "SQDBR" "RRE" 14
		       14,  // 3630 "B316" "SQXBR" "RRE" 14
		       14,  // 3640 "B317" "MEEBR" "RRE" 14
		       14,  // 3650 "B318" "KDBR" "RRE" 14
		       14,  // 3660 "B319" "CDBR" "RRE" 14
		       14,  // 3670 "B31A" "ADBR" "RRE" 14
		       14,  // 3680 "B31B" "SDBR" "RRE" 14
		       14,  // 3690 "B31C" "MDBR" "RRE" 14
		       14,  // 3700 "B31D" "DDBR" "RRE" 14
		       15,  // 3710 "B31E" "MADBR" "RRF" 15
		       15,  // 3720 "B31F" "MSDBR" "RRF" 15
		       14,  // 3730 "B324" "LDER" "RRE" 14
		       14,  // 3740 "B325" "LXDR" "RRE" 14
		       14,  // 3750 "B326" "LXER" "RRE" 14
		       15,  // 3760 "B32E" "MAER" "RRF" 15
		       15,  // 3770 "B32F" "MSER" "RRF" 15
		       14,  // 3780 "B336" "SQXR" "RRE" 14
		       14,  // 3790 "B337" "MEER" "RRE" 14
		       15,  // 3800 "B33E" "MADR" "RRF" 15
		       15,  // 3810 "B33F" "MSDR" "RRF" 15
		       14,  // 3820 "B340" "LPXBR" "RRE" 14
		       14,  // 3830 "B341" "LNXBR" "RRE" 14
		       14,  // 3840 "B342" "LTXBR" "RRE" 14
		       14,  // 3850 "B343" "LCXBR" "RRE" 14
		       14,  // 3860 "B344" "LEDBR" "RRE" 14
		       14,  // 3870 "B345" "LDXBR" "RRE" 14
		       14,  // 3880 "B346" "LEXBR" "RRE" 14
		       15,  // 3890 "B347" "FIXBR" "RRF" 15
		       14,  // 3900 "B348" "KXBR" "RRE" 14
		       14,  // 3910 "B349" "CXBR" "RRE" 14
		       14,  // 3920 "B34A" "AXBR" "RRE" 14
		       14,  // 3930 "B34B" "SXBR" "RRE" 14
		       14,  // 3940 "B34C" "MXBR" "RRE" 14
		       14,  // 3950 "B34D" "DXBR" "RRE" 14
		       15,  // 3960 "B350" "TBEDR" "RRF" 15
		       15,  // 3970 "B351" "TBDR" "RRF" 15
		       30,  // 3980 "B353" "DIEBR" "RR4" 30
		       15,  // 3990 "B357" "FIEBR" "RRF" 15
		       14,  // 4000 "B358" "THDER" "RRE" 14
		       14,  // 4010 "B359" "THDR" "RRE" 14
		       30,  // 4020 "B35B" "DIDBR" "RR4" 30
		       15,  // 4030 "B35F" "FIDBR" "RRF" 15
		       14,  // 4040 "B360" "LPXR" "RRE" 14
		       14,  // 4050 "B361" "LNXR" "RRE" 14
		       14,  // 4060 "B362" "LTXR" "RRE" 14
		       14,  // 4070 "B363" "LCXR" "RRE" 14
		       14,  // 4080 "B365" "LXR" "RRE" 14
		       14,  // 4090 "B366" "LEXR" "RRE" 14
		       14,  // 4100 "B367" "FIXR" "RRE" 14
		       14,  // 4110 "B369" "CXR" "RRE" 14
		       14,  // 4120 "B374" "LZER" "RRE" 14
		       14,  // 4130 "B375" "LZDR" "RRE" 14
		       14,  // 4140 "B376" "LZXR" "RRE" 14
		       14,  // 4150 "B377" "FIER" "RRE" 14
		       14,  // 4160 "B37F" "FIDR" "RRE" 14
		       14,  // 4170 "B384" "SFPC" "RRE" 14
		       14,  // 4180 "B38C" "EFPC" "RRE" 14
		       14,  // 4190 "B394" "CEFBR" "RRE" 14
		       14,  // 4200 "B395" "CDFBR" "RRE" 14
		       14,  // 4210 "B396" "CXFBR" "RRE" 14
		       15,  // 4220 "B398" "CFEBR" "RRF" 15
		       15,  // 4230 "B399" "CFDBR" "RRF" 15
		       15,  // 4240 "B39A" "CFXBR" "RRF" 15
		       14,  // 4250 "B3A4" "CEGBR" "RRE" 14
		       14,  // 4260 "B3A5" "CDGBR" "RRE" 14
		       14,  // 4270 "B3A6" "CXGBR" "RRE" 14
		       15,  // 4280 "B3A8" "CGEBR" "RRF" 15
		       15,  // 4290 "B3A9" "CGDBR" "RRF" 15
		       15,  // 4300 "B3AA" "CGXBR" "RRF" 15
		       14,  // 4310 "B3B4" "CEFR" "RRE" 14
		       14,  // 4320 "B3B5" "CDFR" "RRE" 14
		       14,  // 4330 "B3B6" "CXFR" "RRE" 14
		       15,  // 4340 "B3B8" "CFER" "RRF" 15
		       15,  // 4350 "B3B9" "CFDR" "RRF" 15
		       15,  // 4360 "B3BA" "CFXR" "RRF" 15
		       14,  // 4370 "B3C4" "CEGR" "RRE" 14
		       14,  // 4380 "B3C5" "CDGR" "RRE" 14
		       14,  // 4390 "B3C6" "CXGR" "RRE" 14
		       15,  // 4400 "B3C8" "CGER" "RRF" 15
		       15,  // 4410 "B3C9" "CGDR" "RRF" 15
		       15,  // 4420 "B3CA" "CGXR" "RRF" 15
		       10,  // 4430 "B6" "STCTL" "RS" 10
		       10,  // 4440 "B7" "LCTL" "RS" 10
		       14,  // 4450 "B900" "LPGR" "RRE" 14
		       14,  // 4460 "B901" "LNGR" "RRE" 14
		       14,  // 4470 "B902" "LTGR" "RRE" 14
		       14,  // 4480 "B903" "LCGR" "RRE" 14
		       14,  // 4490 "B904" "LGR" "RRE" 14
		       14,  // 4500 "B905" "LURAG" "RRE" 14
		       14,  // 4510 "B908" "AGR" "RRE" 14
		       14,  // 4520 "B909" "SGR" "RRE" 14
		       14,  // 4530 "B90A" "ALGR" "RRE" 14
		       14,  // 4540 "B90B" "SLGR" "RRE" 14
		       14,  // 4550 "B90C" "MSGR" "RRE" 14
		       14,  // 4560 "B90D" "DSGR" "RRE" 14
		       14,  // 4570 "B90E" "EREGG" "RRE" 14
		       14,  // 4580 "B90F" "LRVGR" "RRE" 14
		       14,  // 4590 "B910" "LPGFR" "RRE" 14
		       14,  // 4600 "B911" "LNGFR" "RRE" 14
		       14,  // 4610 "B912" "LTGFR" "RRE" 14
		       14,  // 4620 "B913" "LCGFR" "RRE" 14
		       14,  // 4630 "B914" "LGFR" "RRE" 14
		       14,  // 4640 "B916" "LLGFR" "RRE" 14
		       14,  // 4650 "B917" "LLGTR" "RRE" 14
		       14,  // 4660 "B918" "AGFR" "RRE" 14
		       14,  // 4670 "B919" "SGFR" "RRE" 14
		       14,  // 4680 "B91A" "ALGFR" "RRE" 14
		       14,  // 4690 "B91B" "SLGFR" "RRE" 14
		       14,  // 4700 "B91C" "MSGFR" "RRE" 14
		       14,  // 4710 "B91D" "DSGFR" "RRE" 14
		       14,  // 4720 "B91E" "KMAC" "RRE" 14
		       14,  // 4730 "B91F" "LRVR" "RRE" 14
		       14,  // 4740 "B920" "CGR" "RRE" 14
		       14,  // 4750 "B921" "CLGR" "RRE" 14
		       14,  // 4760 "B925" "STURG" "RRE" 14
		       14,  // 4770 "B92E" "KM" "RRE" 14
		       14,  // 4780 "B92F" "KMC" "RRE" 14
		       14,  // 4790 "B930" "CGFR" "RRE" 14
		       14,  // 4800 "B931" "CLGFR" "RRE" 14
		       14,  // 4810 "B93E" "KIMD" "RRE" 14
		       14,  // 4820 "B93F" "KLMD" "RRE" 14
		       14,  // 4830 "B946" "BCTGR" "RRE" 14
		       14,  // 4840 "B980" "NGR" "RRE" 14
		       14,  // 4850 "B981" "OGR" "RRE" 14
		       14,  // 4860 "B982" "XGR" "RRE" 14
		       14,  // 4870 "B986" "MLGR" "RRE" 14
		       14,  // 4880 "B987" "DLGR" "RRE" 14
		       14,  // 4890 "B988" "ALCGR" "RRE" 14
		       14,  // 4900 "B989" "SLBGR" "RRE" 14
		       14,  // 4910 "B98A" "CSPG" "RRE" 14
		       14,  // 4920 "B98D" "EPSW" "RRE" 14
		       15,  // 4930 "B98E" "IDTE" "RRF" 15
		       14,  // 4940 "B990" "TRTT" "RRE" 14
		       14,  // 4950 "B991" "TRTO" "RRE" 14
		       14,  // 4960 "B992" "TROT" "RRE" 14
		       14,  // 4970 "B993" "TROO" "RRE" 14
		       14,  // 4980 "B996" "MLR" "RRE" 14
		       14,  // 4990 "B997" "DLR" "RRE" 14
		       14,  // 5000 "B998" "ALCR" "RRE" 14
		       14,  // 5010 "B999" "SLBR" "RRE" 14
		       14,  // 5020 "B99A" "EPAIR" "RRE" 14
		       14,  // 5030 "B99B" "ESAIR" "RRE" 14
		       14,  // 5040 "B99D" "ESEA" "RRE" 14
		       14,  // 5050 "B99E" "PTI" "RRE" 14
		       14,  // 5060 "B99F" "SSAIR" "RRE" 14
		       14,  // 5070 "B9B0" "CU14" "RRE" 14
		       14,  // 5080 "B9B1" "CU24" "RRE" 14
		       14,  // 5090 "B9B2" "CU41" "RRE" 14
		       14,  // 5100 "B9B3" "CU42" "RRE" 14
		       14,  // 5110 "B9BE" "SRSTU" "RRE" 14
		       10,  // 5120 "BA" "CS" "RS" 10
		       10,  // 5130 "BB" "CDS" "RS" 10
		       10,  // 5140 "BD" "CLM" "RS" 10
		       10,  // 5150 "BE" "STCM" "RS" 10
		       10,  // 5160 "BF" "ICM" "RS" 10
		       16,  // 5170 "C00" "LARL" "RIL" 16
		       16,  // 5180 "C04" "BRCL" "RIL" 16
		       16,  // 5190 "C0F4" "BRUL" "RIL" 16
		       16,  // 5200 "C0F4" "BLU" "RIL" 16
		       16,  // 5210 "C05" "BRASL" "RIL" 16
		       16,  // 5220 "C05" "JASL" "RIL" 16
		       17,  // 5230 "D0" "TRTR" "SS" 17
		       17,  // 5240 "D1" "MVN" "SS" 17
		       17,  // 5250 "D2" "MVC" "SS" 17
		       17,  // 5260 "D3" "MVZ" "SS" 17
		       17,  // 5270 "D4" "NC" "SS" 17
		       17,  // 5280 "D5" "CLC" "SS" 17
		       17,  // 5290 "D6" "OC" "SS" 17
		       17,  // 5300 "D7" "XC" "SS" 17
		       17,  // 5310 "D9" "MVCK" "SS" 17
		       17,  // 5320 "DA" "MVCP" "SS" 17
		       17,  // 5330 "DB" "MVCS" "SS" 17
		       17,  // 5340 "DC" "TR" "SS" 17
		       17,  // 5350 "DD" "TRT" "SS" 17
		       17,  // 5360 "DE" "ED" "SS" 17
		       17,  // 5370 "DF" "EDMK" "SS" 17
		       17,  // 5380 "E1" "PKU" "SS" 17
		       17,  // 5390 "E2" "UNPKU" "SS" 17
		       18,  // 5400 "E303" "LRAG" "RXY" 18
		       18,  // 5410 "E304" "LG" "RXY" 18
		       18,  // 5420 "E306" "CVBY" "RXY" 18
		       18,  // 5430 "E308" "AG" "RXY" 18
		       18,  // 5440 "E309" "SG" "RXY" 18
		       18,  // 5450 "E30A" "ALG" "RXY" 18
		       18,  // 5460 "E30B" "SLG" "RXY" 18
		       18,  // 5470 "E30C" "MSG" "RXY" 18
		       18,  // 5480 "E30D" "DSG" "RXY" 18
		       18,  // 5490 "E30E" "CVBG" "RXY" 18
		       18,  // 5500 "E30F" "LRVG" "RXY" 18
		       18,  // 5510 "E313" "LRAY" "RXY" 18
		       18,  // 5520 "E314" "LGF" "RXY" 18
		       18,  // 5530 "E315" "LGH" "RXY" 18
		       18,  // 5540 "E316" "LLGF" "RXY" 18
		       18,  // 5550 "E317" "LLGT" "RXY" 18
		       18,  // 5560 "E318" "AGF" "RXY" 18
		       18,  // 5570 "E319" "SGF" "RXY" 18
		       18,  // 5580 "E31A" "ALGF" "RXY" 18
		       18,  // 5590 "E31B" "SLGF" "RXY" 18
		       18,  // 5600 "E31C" "MSGF" "RXY" 18
		       18,  // 5610 "E31D" "DSGF" "RXY" 18
		       18,  // 5620 "E31E" "LRV" "RXY" 18
		       18,  // 5630 "E31F" "LRVH" "RXY" 18
		       18,  // 5640 "E320" "CG" "RXY" 18
		       18,  // 5650 "E321" "CLG" "RXY" 18
		       18,  // 5660 "E324" "STG" "RXY" 18
		       18,  // 5670 "E326" "CVDY" "RXY" 18
		       18,  // 5680 "E32E" "CVDG" "RXY" 18
		       18,  // 5690 "E32F" "STRVG" "RXY" 18
		       18,  // 5700 "E330" "CGF" "RXY" 18
		       18,  // 5710 "E331" "CLGF" "RXY" 18
		       18,  // 5720 "E33E" "STRV" "RXY" 18
		       18,  // 5730 "E33F" "STRVH" "RXY" 18
		       18,  // 5740 "E346" "BCTG" "RXY" 18
		       18,  // 5750 "E350" "STY" "RXY" 18
		       18,  // 5760 "E351" "MSY" "RXY" 18
		       18,  // 5770 "E354" "NY" "RXY" 18
		       18,  // 5780 "E355" "CLY" "RXY" 18
		       18,  // 5790 "E356" "OY" "RXY" 18
		       18,  // 5800 "E357" "XY" "RXY" 18
		       18,  // 5810 "E358" "LY" "RXY" 18
		       18,  // 5820 "E359" "CY" "RXY" 18
		       18,  // 5830 "E35A" "AY" "RXY" 18
		       18,  // 5840 "E35B" "SY" "RXY" 18
		       18,  // 5850 "E35E" "ALY" "RXY" 18
		       18,  // 5860 "E35F" "SLY" "RXY" 18
		       18,  // 5870 "E370" "STHY" "RXY" 18
		       18,  // 5880 "E371" "LAY" "RXY" 18
		       18,  // 5890 "E372" "STCY" "RXY" 18
		       18,  // 5900 "E373" "ICY" "RXY" 18
		       18,  // 5910 "E376" "LB" "RXY" 18
		       18,  // 5920 "E377" "LGB" "RXY" 18
		       18,  // 5930 "E378" "LHY" "RXY" 18
		       18,  // 5940 "E379" "CHY" "RXY" 18
		       18,  // 5950 "E37A" "AHY" "RXY" 18
		       18,  // 5960 "E37B" "SHY" "RXY" 18
		       18,  // 5970 "E380" "NG" "RXY" 18
		       18,  // 5980 "E381" "OG" "RXY" 18
		       18,  // 5990 "E382" "XG" "RXY" 18
		       18,  // 6000 "E386" "MLG" "RXY" 18
		       18,  // 6010 "E387" "DLG" "RXY" 18
		       18,  // 6020 "E388" "ALCG" "RXY" 18
		       18,  // 6030 "E389" "SLBG" "RXY" 18
		       18,  // 6040 "E38E" "STPQ" "RXY" 18
		       18,  // 6050 "E38F" "LPQ" "RXY" 18
		       18,  // 6060 "E390" "LLGC" "RXY" 18
		       18,  // 6070 "E391" "LLGH" "RXY" 18
		       18,  // 6080 "E396" "ML" "RXY" 18
		       18,  // 6090 "E397" "DL" "RXY" 18
		       18,  // 6100 "E398" "ALC" "RXY" 18
		       18,  // 6110 "E399" "SLB" "RXY" 18
		       19,  // 6120 "E500" "LASP" "SSE" 19
		       19,  // 6130 "E501" "TPROT" "SSE" 19
		       19,  // 6140 "E502" "STRAG" "SSE" 19
		       19,  // 6150 "E50E" "MVCSK" "SSE" 19
		       19,  // 6160 "E50F" "MVCDK" "SSE" 19
		       17,  // 6170 "E8" "MVCIN" "SS" 17
		       17,  // 6180 "E9" "PKA" "SS" 17
		       17,  // 6190 "EA" "UNPKA" "SS" 17
		       20,  // 6200 "EB04" "LMG" "RSY" 20
		       20,  // 6210 "EB0A" "SRAG" "RSY" 20
		       20,  // 6220 "EB0B" "SLAG" "RSY" 20
		       20,  // 6230 "EB0C" "SRLG" "RSY" 20
		       20,  // 6240 "EB0D" "SLLG" "RSY" 20
		       20,  // 6250 "EB0F" "TRACG" "RSY" 20
		       20,  // 6260 "EB14" "CSY" "RSY" 20
		       20,  // 6270 "EB1C" "RLLG" "RSY" 20
		       20,  // 6280 "EB1D" "RLL" "RSY" 20
		       20,  // 6290 "EB20" "CLMH" "RSY" 20
		       20,  // 6300 "EB21" "CLMY" "RSY" 20
		       20,  // 6310 "EB24" "STMG" "RSY" 20
		       20,  // 6320 "EB25" "STCTG" "RSY" 20
		       20,  // 6330 "EB26" "STMH" "RSY" 20
		       20,  // 6340 "EB2C" "STCMH" "RSY" 20
		       20,  // 6350 "EB2D" "STCMY" "RSY" 20
		       20,  // 6360 "EB2F" "LCTLG" "RSY" 20
		       20,  // 6370 "EB30" "CSG" "RSY" 20
		       20,  // 6380 "EB31" "CDSY" "RSY" 20
		       20,  // 6390 "EB3E" "CDSG" "RSY" 20
		       20,  // 6400 "EB44" "BXHG" "RSY" 20
		       20,  // 6410 "EB45" "BXLEG" "RSY" 20
		       21,  // 6420 "EB51" "TMY" "SIY" 21
		       21,  // 6430 "EB52" "MVIY" "SIY" 21
		       21,  // 6440 "EB54" "NIY" "SIY" 21
		       21,  // 6450 "EB55" "CLIY" "SIY" 21
		       21,  // 6460 "EB56" "OIY" "SIY" 21
		       21,  // 6470 "EB57" "XIY" "SIY" 21
		       20,  // 6480 "EB80" "ICMH" "RSY" 20
		       20,  // 6490 "EB81" "ICMY" "RSY" 20
		       20,  // 6500 "EB8E" "MVCLU" "RSY" 20
		       20,  // 6510 "EB8F" "CLCLU" "RSY" 20
		       20,  // 6520 "EB90" "STMY" "RSY" 20
		       20,  // 6530 "EB96" "LMH" "RSY" 20
		       20,  // 6540 "EB98" "LMY" "RSY" 20
		       20,  // 6550 "EB9A" "LAMY" "RSY" 20
		       20,  // 6560 "EB9B" "STAMY" "RSY" 20
		       22,  // 6570 "EBC0" "TP" "RSL" 22
		       23,  // 6580 "EC44" "BRXHG" "RIE" 23
		       23,  // 6590 "EC44" "JXHG" "RIE" 23
		       23,  // 6600 "EC45" "BRXLG" "RIE" 23
		       23,  // 6610 "EC45" "JXLG" "RIE" 23
		       24,  // 6620 "ED04" "LDEB" "RXE" 24
		       24,  // 6630 "ED05" "LXDB" "RXE" 24
		       24,  // 6640 "ED06" "LXEB" "RXE" 24
		       24,  // 6650 "ED07" "MXDB" "RXE" 24
		       24,  // 6660 "ED08" "KEB" "RXE" 24
		       24,  // 6670 "ED09" "CEB" "RXE" 24
		       24,  // 6680 "ED0A" "AEB" "RXE" 24
		       24,  // 6690 "ED0B" "SEB" "RXE" 24
		       24,  // 6700 "ED0C" "MDEB" "RXE" 24
		       24,  // 6710 "ED0D" "DEB" "RXE" 24
		       25,  // 6720 "ED0E" "MAEB" "RXF" 25
		       25,  // 6730 "ED0F" "MSEB" "RXF" 25
		       24,  // 6740 "ED10" "TCEB" "RXE" 24
		       24,  // 6750 "ED11" "TCDB" "RXE" 24
		       24,  // 6760 "ED12" "TCXB" "RXE" 24
		       24,  // 6770 "ED14" "SQEB" "RXE" 24
		       24,  // 6780 "ED15" "SQDB" "RXE" 24
		       24,  // 6790 "ED17" "MEEB" "RXE" 24
		       24,  // 6800 "ED18" "KDB" "RXE" 24
		       24,  // 6810 "ED19" "CDB" "RXE" 24
		       24,  // 6820 "ED1A" "ADB" "RXE" 24
		       24,  // 6830 "ED1B" "SDB" "RXE" 24
		       24,  // 6840 "ED1C" "MDB" "RXE" 24
		       24,  // 6850 "ED1D" "DDB" "RXE" 24
		       25,  // 6860 "ED1E" "MADB" "RXF" 25
		       25,  // 6870 "ED1F" "MSDB" "RXF" 25
		       24,  // 6880 "ED24" "LDE" "RXE" 24
		       24,  // 6890 "ED25" "LXD" "RXE" 24
		       24,  // 6900 "ED26" "LXE" "RXE" 24
		       25,  // 6910 "ED2E" "MAE" "RXF" 25
		       25,  // 6920 "ED2F" "MSE" "RXF" 25
		       24,  // 6930 "ED34" "SQE" "RXE" 24
		       24,  // 6940 "ED35" "SQD" "RXE" 24
		       24,  // 6950 "ED37" "MEE" "RXE" 24
		       25,  // 6960 "ED3E" "MAD" "RXF" 25
		       25,  // 6970 "ED3F" "MSD" "RXF" 25
		       18,  // 6980 "ED64" "LEY" "RXY" 18
		       18,  // 6990 "ED65" "LDY" "RXY" 18
		       18,  // 7000 "ED66" "STEY" "RXY" 18
		       18,  // 7010 "ED67" "STDY" "RXY" 18
		       27,  // 7020 "EE" "PLO" "SS3" 27
		       28,  // 7030 "EF" "LMD" "SS4" 28
		       29,  // 7040 "F0" "SRP" "SS5" 29
		       26,  // 7050 "F1" "MVO" "SS2" 26
		       26,  // 7060 "F2" "PACK" "SS2" 26
		       26,  // 7070 "F3" "UNPK" "SS2" 26
		       26,  // 7080 "F8" "ZAP" "SS2" 26
		       26,  // 7090 "F9" "CP" "SS2" 26
		       26,  // 7100 "FA" "AP" "SS2" 26
		       26,  // 7110 "FB" "SP" "SS2" 26
		       26,  // 7120 "FC" "MP" "SS2" 26
		       26}; // 7130 "FD" "DP" "SS2" 26
  	
      String[]   op_code = {
		       "0101",  // 10 "0101" "PR" "E" 1
		       "0102",  // 20 "0102" "UPT" "E" 1
		       "0107",  // 30 "0107" "SCKPF" "E" 1
		       "010B",  // 40 "010B" "TAM" "E" 1
		       "010C",  // 50 "010C" "SAM24" "E" 1
		       "010D",  // 60 "010D" "SAM31" "E" 1
		       "010E",  // 70 "010E" "SAM64" "E" 1
		       "01FF",  // 80 "01FF" "TRAP2" "E" 1
		       "04",  // 90 "04" "SPM" "RR" 2
		       "05",  // 100 "05" "BALR" "RR" 2
		       "06",  // 110 "06" "BCTR" "RR" 2
		       "07",  // 120 "07" "BCR" "RR" 2
		       "07F",  // 130 "07F" "BR" "BRX" 3
		       "070",  // 140 "070" "NOPR" "BRX" 3
		       "072",  // 150 "072" "BHR" "BRX" 3
		       "074",  // 160 "074" "BLR" "BRX" 3
		       "078",  // 170 "078" "BER" "BRX" 3
		       "07D",  // 180 "07D" "BNHR" "BRX" 3
		       "07B",  // 190 "07B" "BNLR" "BRX" 3
		       "077",  // 200 "077" "BNER" "BRX" 3
		       "072",  // 210 "072" "BPR" "BRX" 3
		       "071",  // 220 "071" "BOR" "BRX" 3
		       "074",  // 230 "074" "BMR" "BRX" 3
		       "078",  // 240 "078" "BZR" "BRX" 3
		       "07D",  // 250 "07D" "BNPR" "BRX" 3
		       "07B",  // 260 "07B" "BNMR" "BRX" 3
		       "077",  // 270 "077" "BNZR" "BRX" 3
		       "07E",  // 280 "07E" "BNOR" "BRX" 3
		       "0A",  // 290 "0A" "SVC" "I" 4
		       "0B",  // 300 "0B" "BSM" "RR" 2
		       "0C",  // 310 "0C" "BASSM" "RR" 2
		       "0D",  // 320 "0D" "BASR" "RR" 2
		       "0E",  // 330 "0E" "MVCL" "RR" 2
		       "0F",  // 340 "0F" "CLCL" "RR" 2
		       "10",  // 350 "10" "LPR" "RR" 2
		       "11",  // 360 "11" "LNR" "RR" 2
		       "12",  // 370 "12" "LTR" "RR" 2
		       "13",  // 380 "13" "LCR" "RR" 2
		       "14",  // 390 "14" "NR" "RR" 2
		       "15",  // 400 "15" "CLR" "RR" 2
		       "16",  // 410 "16" "OR" "RR" 2
		       "17",  // 420 "17" "XR" "RR" 2
		       "18",  // 430 "18" "LR" "RR" 2
		       "19",  // 440 "19" "CR" "RR" 2
		       "1A",  // 450 "1A" "AR" "RR" 2
		       "1B",  // 460 "1B" "SR" "RR" 2
		       "1C",  // 470 "1C" "MR" "RR" 2
		       "1D",  // 480 "1D" "DR" "RR" 2
		       "1E",  // 490 "1E" "ALR" "RR" 2
		       "1F",  // 500 "1F" "SLR" "RR" 2
		       "20",  // 510 "20" "LPDR" "RR" 2
		       "21",  // 520 "21" "LNDR" "RR" 2
		       "22",  // 530 "22" "LTDR" "RR" 2
		       "23",  // 540 "23" "LCDR" "RR" 2
		       "24",  // 550 "24" "HDR" "RR" 2
		       "25",  // 560 "25" "LDXR" "RR" 2
		       "25",  // 570 "25" "LRDR" "RR" 2
		       "26",  // 580 "26" "MXR" "RR" 2
		       "27",  // 590 "27" "MXDR" "RR" 2
		       "28",  // 600 "28" "LDR" "RR" 2
		       "29",  // 610 "29" "CDR" "RR" 2
		       "2A",  // 620 "2A" "ADR" "RR" 2
		       "2B",  // 630 "2B" "SDR" "RR" 2
		       "2C",  // 640 "2C" "MDR" "RR" 2
		       "2D",  // 650 "2D" "DDR" "RR" 2
		       "2E",  // 660 "2E" "AWR" "RR" 2
		       "2F",  // 670 "2F" "SWR" "RR" 2
		       "30",  // 680 "30" "LPER" "RR" 2
		       "31",  // 690 "31" "LNER" "RR" 2
		       "32",  // 700 "32" "LTER" "RR" 2
		       "33",  // 710 "33" "LCER" "RR" 2
		       "34",  // 720 "34" "HER" "RR" 2
		       "35",  // 730 "35" "LEDR" "RR" 2
		       "35",  // 740 "35" "LRER" "RR" 2
		       "36",  // 750 "36" "AXR" "RR" 2
		       "37",  // 760 "37" "SXR" "RR" 2
		       "38",  // 770 "38" "LER" "RR" 2
		       "39",  // 780 "39" "CER" "RR" 2
		       "3A",  // 790 "3A" "AER" "RR" 2
		       "3B",  // 800 "3B" "SER" "RR" 2
		       "3C",  // 810 "3C" "MDER" "RR" 2
		       "3C",  // 820 "3C" "MER" "RR" 2
		       "3D",  // 830 "3D" "DER" "RR" 2
		       "3E",  // 840 "3E" "AUR" "RR" 2
		       "3F",  // 850 "3F" "SUR" "RR" 2
		       "40",  // 860 "40" "STH" "RX" 5
		       "41",  // 870 "41" "LA" "RX" 5
		       "42",  // 880 "42" "STC" "RX" 5
		       "43",  // 890 "43" "IC" "RX" 5
		       "44",  // 900 "44" "EX" "RX" 5
		       "45",  // 910 "45" "BAL" "RX" 5
		       "46",  // 920 "46" "BCT" "RX" 5
		       "47",  // 930 "47" "BC" "RX" 5
		       "47F",  // 940 "47F" "B" "BCX" 6
		       "470",  // 950 "470" "NOP" "BCX" 6
		       "472",  // 960 "472" "BH" "BCX" 6
		       "474",  // 970 "474" "BL" "BCX" 6
		       "478",  // 980 "478" "BE" "BCX" 6
		       "47D",  // 990 "47D" "BNH" "BCX" 6
		       "47B",  // 1000 "47B" "BNL" "BCX" 6
		       "477",  // 1010 "477" "BNE" "BCX" 6
		       "472",  // 1020 "472" "BP" "BCX" 6
		       "471",  // 1030 "471" "BO" "BCX" 6
		       "474",  // 1040 "474" "BM" "BCX" 6
		       "478",  // 1050 "478" "BZ" "BCX" 6
		       "47D",  // 1060 "47D" "BNP" "BCX" 6
		       "47B",  // 1070 "47B" "BNM" "BCX" 6
		       "477",  // 1080 "477" "BNZ" "BCX" 6
		       "47E",  // 1090 "47E" "BNO" "BCX" 6
		       "48",  // 1100 "48" "LH" "RX" 5
		       "49",  // 1110 "49" "CH" "RX" 5
		       "4A",  // 1120 "4A" "AH" "RX" 5
		       "4B",  // 1130 "4B" "SH" "RX" 5
		       "4C",  // 1140 "4C" "MH" "RX" 5
		       "4D",  // 1150 "4D" "BAS" "RX" 5
		       "4E",  // 1160 "4E" "CVD" "RX" 5
		       "4F",  // 1170 "4F" "CVB" "RX" 5
		       "50",  // 1180 "50" "ST" "RX" 5
		       "51",  // 1190 "51" "LAE" "RX" 5
		       "54",  // 1200 "54" "N" "RX" 5
		       "55",  // 1210 "55" "CL" "RX" 5
		       "56",  // 1220 "56" "O" "RX" 5
		       "57",  // 1230 "57" "X" "RX" 5
		       "58",  // 1240 "58" "L" "RX" 5
		       "59",  // 1250 "59" "C" "RX" 5
		       "5A",  // 1260 "5A" "A" "RX" 5
		       "5B",  // 1270 "5B" "S" "RX" 5
		       "5C",  // 1280 "5C" "M" "RX" 5
		       "5D",  // 1290 "5D" "D" "RX" 5
		       "5E",  // 1300 "5E" "AL" "RX" 5
		       "5F",  // 1310 "5F" "SL" "RX" 5
		       "60",  // 1320 "60" "STD" "RX" 5
		       "67",  // 1330 "67" "MXD" "RX" 5
		       "68",  // 1340 "68" "LD" "RX" 5
		       "69",  // 1350 "69" "CD" "RX" 5
		       "6A",  // 1360 "6A" "AD" "RX" 5
		       "6B",  // 1370 "6B" "SD" "RX" 5
		       "6C",  // 1380 "6C" "MD" "RX" 5
		       "6D",  // 1390 "6D" "DD" "RX" 5
		       "6E",  // 1400 "6E" "AW" "RX" 5
		       "6F",  // 1410 "6F" "SW" "RX" 5
		       "70",  // 1420 "70" "STE" "RX" 5
		       "71",  // 1430 "71" "MS" "RX" 5
		       "78",  // 1440 "78" "LE" "RX" 5
		       "79",  // 1450 "79" "CE" "RX" 5
		       "7A",  // 1460 "7A" "AE" "RX" 5
		       "7B",  // 1470 "7B" "SE" "RX" 5
		       "7C",  // 1480 "7C" "MDE" "RX" 5
		       "7C",  // 1490 "7C" "ME" "RX" 5
		       "7D",  // 1500 "7D" "DE" "RX" 5
		       "7E",  // 1510 "7E" "AU" "RX" 5
		       "7F",  // 1520 "7F" "SU" "RX" 5
		       "8000",  // 1530 "8000" "SSM" "S" 7
		       "8200",  // 1540 "8200" "LPSW" "S" 7
		       "83",  // 1550 "83" "DIAGNOSE" "DM" 8
		       "84",  // 1560 "84" "BRXH" "RSI" 9
		       "84",  // 1570 "84" "JXH" "RSI" 9
		       "85",  // 1580 "85" "BRXLE" "RSI" 9
		       "85",  // 1590 "85" "JXLE" "RSI" 9
		       "86",  // 1600 "86" "BXH" "RS" 10
		       "87",  // 1610 "87" "BXLE" "RS" 10
		       "88",  // 1620 "88" "SRL" "RS" 10
		       "89",  // 1630 "89" "SLL" "RS" 10
		       "8A",  // 1640 "8A" "SRA" "RS" 10
		       "8B",  // 1650 "8B" "SLA" "RS" 10
		       "8C",  // 1660 "8C" "SRDL" "RS" 10
		       "8D",  // 1670 "8D" "SLDL" "RS" 10
		       "8E",  // 1680 "8E" "SRDA" "RS" 10
		       "8F",  // 1690 "8F" "SLDA" "RS" 10
		       "90",  // 1700 "90" "STM" "RS" 10
		       "91",  // 1710 "91" "TM" "SI" 11
		       "92",  // 1720 "92" "MVI" "SI" 11
		       "9300",  // 1730 "9300" "TS" "S" 7
		       "94",  // 1740 "94" "NI" "SI" 11
		       "95",  // 1750 "95" "CLI" "SI" 11
		       "96",  // 1760 "96" "OI" "SI" 11
		       "97",  // 1770 "97" "XI" "SI" 11
		       "98",  // 1780 "98" "LM" "RS" 10
		       "99",  // 1790 "99" "TRACE" "RS" 10
		       "9A",  // 1800 "9A" "LAM" "RS" 10
		       "9B",  // 1810 "9B" "STAM" "RS" 10
		       "A50",  // 1820 "A50" "IIHH" "RI" 12
		       "A51",  // 1830 "A51" "IIHL" "RI" 12
		       "A52",  // 1840 "A52" "IILH" "RI" 12
		       "A53",  // 1850 "A53" "IILL" "RI" 12
		       "A54",  // 1860 "A54" "NIHH" "RI" 12
		       "A55",  // 1870 "A55" "NIHL" "RI" 12
		       "A56",  // 1880 "A56" "NILH" "RI" 12
		       "A57",  // 1890 "A57" "NILL" "RI" 12
		       "A58",  // 1900 "A58" "OIHH" "RI" 12
		       "A59",  // 1910 "A59" "OIHL" "RI" 12
		       "A5A",  // 1920 "A5A" "OILH" "RI" 12
		       "A5B",  // 1930 "A5B" "OILL" "RI" 12
		       "A5C",  // 1940 "A5C" "LLIHH" "RI" 12
		       "A5D",  // 1950 "A5D" "LLIHL" "RI" 12
		       "A5E",  // 1960 "A5E" "LLILH" "RI" 12
		       "A5F",  // 1970 "A5F" "LLILL" "RI" 12
		       "A70",  // 1980 "A70" "TMLH" "RI" 12
		       "A70",  // 1990 "A70" "TMH" "RI" 12
		       "A71",  // 2000 "A71" "TMLL" "RI" 12
		       "A71",  // 2010 "A71" "TML" "RI" 12
		       "A72",  // 2020 "A72" "TMHH" "RI" 12
		       "A73",  // 2030 "A73" "TMHL" "RI" 12
		       "A74",  // 2040 "A74" "BRC" "RI" 12
		       "A74F",  // 2050 "A74F" "J" "BRC" 13
		       "A740",  // 2060 "A740" "JNOP" "BRC" 13
		       "A74F",  // 2070 "A74F" "BRU" "BRC" 13
		       "A742",  // 2080 "A742" "BRH" "BRC" 13
		       "A744",  // 2090 "A744" "BRL" "BRC" 13
		       "A748",  // 2100 "A748" "BRE" "BRC" 13
		       "A74D",  // 2110 "A74D" "BRNH" "BRC" 13
		       "A74B",  // 2120 "A74B" "BRNL" "BRC" 13
		       "A747",  // 2130 "A747" "BRNE" "BRC" 13
		       "A742",  // 2140 "A742" "BRP" "BRC" 13
		       "A744",  // 2150 "A744" "BRM" "BRC" 13
		       "A748",  // 2160 "A748" "BRZ" "BRC" 13
		       "A741",  // 2170 "A741" "BRO" "BRC" 13
		       "A74D",  // 2180 "A74D" "BRNP" "BRC" 13
		       "A74B",  // 2190 "A74B" "BRNM" "BRC" 13
		       "A747",  // 2200 "A747" "BRNZ" "BRC" 13
		       "A74E",  // 2210 "A74E" "BRNO" "BRC" 13
		       "A742",  // 2220 "A742" "JH" "BRC" 13
		       "A744",  // 2230 "A744" "JL" "BRC" 13
		       "A748",  // 2240 "A748" "JE" "BRC" 13
		       "A74D",  // 2250 "A74D" "JNH" "BRC" 13
		       "A74B",  // 2260 "A74B" "JNL" "BRC" 13
		       "A747",  // 2270 "A747" "JNE" "BRC" 13
		       "A742",  // 2280 "A742" "JP" "BRC" 13
		       "A744",  // 2290 "A744" "JM" "BRC" 13
		       "A748",  // 2300 "A748" "JZ" "BRC" 13
		       "A741",  // 2310 "A741" "JO" "BRC" 13
		       "A74D",  // 2320 "A74D" "JNP" "BRC" 13
		       "A74B",  // 2330 "A74B" "JNM" "BRC" 13
		       "A747",  // 2340 "A747" "JNZ" "BRC" 13
		       "A74E",  // 2350 "A74E" "JNO" "BRC" 13
		       "A75",  // 2360 "A75" "BRAS" "RI" 12
		       "A75",  // 2370 "A75" "JAS" "RI" 12
		       "A76",  // 2380 "A76" "BRCT" "RI" 12
		       "A76",  // 2390 "A76" "JCT" "RI" 12
		       "A77",  // 2400 "A77" "BRCTG" "RI" 12
		       "A77",  // 2410 "A77" "JCTG" "RI" 12
		       "A78",  // 2420 "A78" "LHI" "RI" 12
		       "A79",  // 2430 "A79" "LGHI" "RI" 12
		       "A7A",  // 2440 "A7A" "AHI" "RI" 12
		       "A7B",  // 2450 "A7B" "AGHI" "RI" 12
		       "A7C",  // 2460 "A7C" "MHI" "RI" 12
		       "A7D",  // 2470 "A7D" "MGHI" "RI" 12
		       "A7E",  // 2480 "A7E" "CHI" "RI" 12
		       "A7F",  // 2490 "A7F" "CGHI" "RI" 12
		       "A8",  // 2500 "A8" "MVCLE" "RS" 10
		       "A9",  // 2510 "A9" "CLCLE" "RS" 10
		       "AC",  // 2520 "AC" "STNSM" "SI" 11
		       "AD",  // 2530 "AD" "STOSM" "SI" 11
		       "AE",  // 2540 "AE" "SIGP" "RS" 10
		       "AF",  // 2550 "AF" "MC" "SI" 11
		       "B1",  // 2560 "B1" "LRA" "RX" 5
		       "B202",  // 2570 "B202" "STIDP" "S" 7
		       "B204",  // 2580 "B204" "SCK" "S" 7
		       "B205",  // 2590 "B205" "STCK" "S" 7
		       "B206",  // 2600 "B206" "SCKC" "S" 7
		       "B207",  // 2610 "B207" "STCKC" "S" 7
		       "B208",  // 2620 "B208" "SPT" "S" 7
		       "B209",  // 2630 "B209" "STPT" "S" 7
		       "B20A",  // 2640 "B20A" "SPKA" "S" 7
		       "B20B",  // 2650 "B20B" "IPK" "S" 7
		       "B20D",  // 2660 "B20D" "PTLB" "S" 7
		       "B210",  // 2670 "B210" "SPX" "S" 7
		       "B211",  // 2680 "B211" "STPX" "S" 7
		       "B212",  // 2690 "B212" "STAP" "S" 7
		       "B218",  // 2700 "B218" "PC" "S" 7
		       "B219",  // 2710 "B219" "SAC" "S" 7
		       "B21A",  // 2720 "B21A" "CFC" "S" 7
		       "B221",  // 2730 "B221" "IPTE" "RRE" 14
		       "B222",  // 2740 "B222" "IPM" "RRE" 14
		       "B223",  // 2750 "B223" "IVSK" "RRE" 14
		       "B224",  // 2760 "B224" "IAC" "RRE" 14
		       "B225",  // 2770 "B225" "SSAR" "RRE" 14
		       "B226",  // 2780 "B226" "EPAR" "RRE" 14
		       "B227",  // 2790 "B227" "ESAR" "RRE" 14
		       "B228",  // 2800 "B228" "PT" "RRE" 14
		       "B229",  // 2810 "B229" "ISKE" "RRE" 14
		       "B22A",  // 2820 "B22A" "RRBE" "RRE" 14
		       "B22B",  // 2830 "B22B" "SSKE" "RRE" 14
		       "B22C",  // 2840 "B22C" "TB" "RRE" 14
		       "B22D",  // 2850 "B22D" "DXR" "RRE" 14
		       "B22E",  // 2860 "B22E" "PGIN" "RRE" 14
		       "B22F",  // 2870 "B22F" "PGOUT" "RRE" 14
		       "B230",  // 2880 "B230" "CSCH" "S" 7
		       "B231",  // 2890 "B231" "HSCH" "S" 7
		       "B232",  // 2900 "B232" "MSCH" "S" 7
		       "B233",  // 2910 "B233" "SSCH" "S" 7
		       "B234",  // 2920 "B234" "STSCH" "S" 7
		       "B235",  // 2930 "B235" "TSCH" "S" 7
		       "B236",  // 2940 "B236" "TPI" "S" 7
		       "B237",  // 2950 "B237" "SAL" "S" 7
		       "B238",  // 2960 "B238" "RSCH" "S" 7
		       "B239",  // 2970 "B239" "STCRW" "S" 7
		       "B23A",  // 2980 "B23A" "STCPS" "S" 7
		       "B23B",  // 2990 "B23B" "RCHP" "S" 7
		       "B23C",  // 3000 "B23C" "SCHM" "S" 7
		       "B240",  // 3010 "B240" "BAKR" "RRE" 14
		       "B241",  // 3020 "B241" "CKSM" "RRE" 14
		       "B244",  // 3030 "B244" "SQDR" "RRE" 14
		       "B245",  // 3040 "B245" "SQER" "RRE" 14
		       "B246",  // 3050 "B246" "STURA" "RRE" 14
		       "B247",  // 3060 "B247" "MSTA" "RRE" 14
		       "B248",  // 3070 "B248" "PALB" "RRE" 14
		       "B249",  // 3080 "B249" "EREG" "RRE" 14
		       "B24A",  // 3090 "B24A" "ESTA" "RRE" 14
		       "B24B",  // 3100 "B24B" "LURA" "RRE" 14
		       "B24C",  // 3110 "B24C" "TAR" "RRE" 14
		       "B24D",  // 3120 "B24D" "CPYA" "RRE" 14
		       "B24E",  // 3130 "B24E" "SAR" "RRE" 14
		       "B24F",  // 3140 "B24F" "EAR" "RRE" 14
		       "B250",  // 3150 "B250" "CSP" "RRE" 14
		       "B252",  // 3160 "B252" "MSR" "RRE" 14
		       "B254",  // 3170 "B254" "MVPG" "RRE" 14
		       "B255",  // 3180 "B255" "MVST" "RRE" 14
		       "B257",  // 3190 "B257" "CUSE" "RRE" 14
		       "B258",  // 3200 "B258" "BSG" "RRE" 14
		       "B25A",  // 3210 "B25A" "BSA" "RRE" 14
		       "B25D",  // 3220 "B25D" "CLST" "RRE" 14
		       "B25E",  // 3230 "B25E" "SRST" "RRE" 14
		       "B263",  // 3240 "B263" "CMPSC" "RRE" 14
		       "B276",  // 3250 "B276" "XSCH" "S" 7
		       "B277",  // 3260 "B277" "RP" "S" 7
		       "B278",  // 3270 "B278" "STCKE" "S" 7
		       "B279",  // 3280 "B279" "SACF" "S" 7
		       "B27D",  // 3290 "B27D" "STSI" "S" 7
		       "B299",  // 3300 "B299" "SRNM" "S" 7
		       "B29C",  // 3310 "B29C" "STFPC" "S" 7
		       "B29D",  // 3320 "B29D" "LFPC" "S" 7
		       "B2A5",  // 3330 "B2A5" "TRE" "RRE" 14
		       "B2A6",  // 3340 "B2A6" "CUUTF" "RRE" 14
		       "B2A6",  // 3350 "B2A6" "CU21" "RRE" 14
		       "B2A7",  // 3360 "B2A7" "CUTFU" "RRE" 14
		       "B2A7",  // 3370 "B2A7" "CU12" "RRE" 14
		       "B2B1",  // 3380 "B2B1" "STFL" "S" 7
		       "B2B2",  // 3390 "B2B2" "LPSWE" "S" 7
		       "B2FF",  // 3400 "B2FF" "TRAP4" "S" 7
		       "B300",  // 3410 "B300" "LPEBR" "RRE" 14
		       "B301",  // 3420 "B301" "LNEBR" "RRE" 14
		       "B302",  // 3430 "B302" "LTEBR" "RRE" 14
		       "B303",  // 3440 "B303" "LCEBR" "RRE" 14
		       "B304",  // 3450 "B304" "LDEBR" "RRE" 14
		       "B305",  // 3460 "B305" "LXDBR" "RRE" 14
		       "B306",  // 3470 "B306" "LXEBR" "RRE" 14
		       "B307",  // 3480 "B307" "MXDBR" "RRE" 14
		       "B308",  // 3490 "B308" "KEBR" "RRE" 14
		       "B309",  // 3500 "B309" "CEBR" "RRE" 14
		       "B30A",  // 3510 "B30A" "AEBR" "RRE" 14
		       "B30B",  // 3520 "B30B" "SEBR" "RRE" 14
		       "B30C",  // 3530 "B30C" "MDEBR" "RRE" 14
		       "B30D",  // 3540 "B30D" "DEBR" "RRE" 14
		       "B30E",  // 3550 "B30E" "MAEBR" "RRF" 15
		       "B30F",  // 3560 "B30F" "MSEBR" "RRF" 15
		       "B310",  // 3570 "B310" "LPDBR" "RRE" 14
		       "B311",  // 3580 "B311" "LNDBR" "RRE" 14
		       "B312",  // 3590 "B312" "LTDBR" "RRE" 14
		       "B313",  // 3600 "B313" "LCDBR" "RRE" 14
		       "B314",  // 3610 "B314" "SQEBR" "RRE" 14
		       "B315",  // 3620 "B315" "SQDBR" "RRE" 14
		       "B316",  // 3630 "B316" "SQXBR" "RRE" 14
		       "B317",  // 3640 "B317" "MEEBR" "RRE" 14
		       "B318",  // 3650 "B318" "KDBR" "RRE" 14
		       "B319",  // 3660 "B319" "CDBR" "RRE" 14
		       "B31A",  // 3670 "B31A" "ADBR" "RRE" 14
		       "B31B",  // 3680 "B31B" "SDBR" "RRE" 14
		       "B31C",  // 3690 "B31C" "MDBR" "RRE" 14
		       "B31D",  // 3700 "B31D" "DDBR" "RRE" 14
		       "B31E",  // 3710 "B31E" "MADBR" "RRF" 15
		       "B31F",  // 3720 "B31F" "MSDBR" "RRF" 15
		       "B324",  // 3730 "B324" "LDER" "RRE" 14
		       "B325",  // 3740 "B325" "LXDR" "RRE" 14
		       "B326",  // 3750 "B326" "LXER" "RRE" 14
		       "B32E",  // 3760 "B32E" "MAER" "RRF" 15
		       "B32F",  // 3770 "B32F" "MSER" "RRF" 15
		       "B336",  // 3780 "B336" "SQXR" "RRE" 14
		       "B337",  // 3790 "B337" "MEER" "RRE" 14
		       "B33E",  // 3800 "B33E" "MADR" "RRF" 15
		       "B33F",  // 3810 "B33F" "MSDR" "RRF" 15
		       "B340",  // 3820 "B340" "LPXBR" "RRE" 14
		       "B341",  // 3830 "B341" "LNXBR" "RRE" 14
		       "B342",  // 3840 "B342" "LTXBR" "RRE" 14
		       "B343",  // 3850 "B343" "LCXBR" "RRE" 14
		       "B344",  // 3860 "B344" "LEDBR" "RRE" 14
		       "B345",  // 3870 "B345" "LDXBR" "RRE" 14
		       "B346",  // 3880 "B346" "LEXBR" "RRE" 14
		       "B347",  // 3890 "B347" "FIXBR" "RRF" 15
		       "B348",  // 3900 "B348" "KXBR" "RRE" 14
		       "B349",  // 3910 "B349" "CXBR" "RRE" 14
		       "B34A",  // 3920 "B34A" "AXBR" "RRE" 14
		       "B34B",  // 3930 "B34B" "SXBR" "RRE" 14
		       "B34C",  // 3940 "B34C" "MXBR" "RRE" 14
		       "B34D",  // 3950 "B34D" "DXBR" "RRE" 14
		       "B350",  // 3960 "B350" "TBEDR" "RRF" 15
		       "B351",  // 3970 "B351" "TBDR" "RRF" 15
		       "B353",  // 3980 "B353" "DIEBR" "RRF" 15
		       "B357",  // 3990 "B357" "FIEBR" "RRF" 15
		       "B358",  // 4000 "B358" "THDER" "RRE" 14
		       "B359",  // 4010 "B359" "THDR" "RRE" 14
		       "B35B",  // 4020 "B35B" "DIDBR" "RRF" 15
		       "B35F",  // 4030 "B35F" "FIDBR" "RRF" 15
		       "B360",  // 4040 "B360" "LPXR" "RRE" 14
		       "B361",  // 4050 "B361" "LNXR" "RRE" 14
		       "B362",  // 4060 "B362" "LTXR" "RRE" 14
		       "B363",  // 4070 "B363" "LCXR" "RRE" 14
		       "B365",  // 4080 "B365" "LXR" "RRE" 14
		       "B366",  // 4090 "B366" "LEXR" "RRE" 14
		       "B367",  // 4100 "B367" "FIXR" "RRE" 14
		       "B369",  // 4110 "B369" "CXR" "RRE" 14
		       "B374",  // 4120 "B374" "LZER" "RRE" 14
		       "B375",  // 4130 "B375" "LZDR" "RRE" 14
		       "B376",  // 4140 "B376" "LZXR" "RRE" 14
		       "B377",  // 4150 "B377" "FIER" "RRE" 14
		       "B37F",  // 4160 "B37F" "FIDR" "RRE" 14
		       "B384",  // 4170 "B384" "SFPC" "RRE" 14
		       "B38C",  // 4180 "B38C" "EFPC" "RRE" 14
		       "B394",  // 4190 "B394" "CEFBR" "RRE" 14
		       "B395",  // 4200 "B395" "CDFBR" "RRE" 14
		       "B396",  // 4210 "B396" "CXFBR" "RRE" 14
		       "B398",  // 4220 "B398" "CFEBR" "RRF" 15
		       "B399",  // 4230 "B399" "CFDBR" "RRF" 15
		       "B39A",  // 4240 "B39A" "CFXBR" "RRF" 15
		       "B3A4",  // 4250 "B3A4" "CEGBR" "RRE" 14
		       "B3A5",  // 4260 "B3A5" "CDGBR" "RRE" 14
		       "B3A6",  // 4270 "B3A6" "CXGBR" "RRE" 14
		       "B3A8",  // 4280 "B3A8" "CGEBR" "RRF" 15
		       "B3A9",  // 4290 "B3A9" "CGDBR" "RRF" 15
		       "B3AA",  // 4300 "B3AA" "CGXBR" "RRF" 15
		       "B3B4",  // 4310 "B3B4" "CEFR" "RRE" 14
		       "B3B5",  // 4320 "B3B5" "CDFR" "RRE" 14
		       "B3B6",  // 4330 "B3B6" "CXFR" "RRE" 14
		       "B3B8",  // 4340 "B3B8" "CFER" "RRF" 15
		       "B3B9",  // 4350 "B3B9" "CFDR" "RRF" 15
		       "B3BA",  // 4360 "B3BA" "CFXR" "RRF" 15
		       "B3C4",  // 4370 "B3C4" "CEGR" "RRE" 14
		       "B3C5",  // 4380 "B3C5" "CDGR" "RRE" 14
		       "B3C6",  // 4390 "B3C6" "CXGR" "RRE" 14
		       "B3C8",  // 4400 "B3C8" "CGER" "RRF" 15
		       "B3C9",  // 4410 "B3C9" "CGDR" "RRF" 15
		       "B3CA",  // 4420 "B3CA" "CGXR" "RRF" 15
		       "B6",  // 4430 "B6" "STCTL" "RS" 10
		       "B7",  // 4440 "B7" "LCTL" "RS" 10
		       "B900",  // 4450 "B900" "LPGR" "RRE" 14
		       "B901",  // 4460 "B901" "LNGR" "RRE" 14
		       "B902",  // 4470 "B902" "LTGR" "RRE" 14
		       "B903",  // 4480 "B903" "LCGR" "RRE" 14
		       "B904",  // 4490 "B904" "LGR" "RRE" 14
		       "B905",  // 4500 "B905" "LURAG" "RRE" 14
		       "B908",  // 4510 "B908" "AGR" "RRE" 14
		       "B909",  // 4520 "B909" "SGR" "RRE" 14
		       "B90A",  // 4530 "B90A" "ALGR" "RRE" 14
		       "B90B",  // 4540 "B90B" "SLGR" "RRE" 14
		       "B90C",  // 4550 "B90C" "MSGR" "RRE" 14
		       "B90D",  // 4560 "B90D" "DSGR" "RRE" 14
		       "B90E",  // 4570 "B90E" "EREGG" "RRE" 14
		       "B90F",  // 4580 "B90F" "LRVGR" "RRE" 14
		       "B910",  // 4590 "B910" "LPGFR" "RRE" 14
		       "B911",  // 4600 "B911" "LNGFR" "RRE" 14
		       "B912",  // 4610 "B912" "LTGFR" "RRE" 14
		       "B913",  // 4620 "B913" "LCGFR" "RRE" 14
		       "B914",  // 4630 "B914" "LGFR" "RRE" 14
		       "B916",  // 4640 "B916" "LLGFR" "RRE" 14
		       "B917",  // 4650 "B917" "LLGTR" "RRE" 14
		       "B918",  // 4660 "B918" "AGFR" "RRE" 14
		       "B919",  // 4670 "B919" "SGFR" "RRE" 14
		       "B91A",  // 4680 "B91A" "ALGFR" "RRE" 14
		       "B91B",  // 4690 "B91B" "SLGFR" "RRE" 14
		       "B91C",  // 4700 "B91C" "MSGFR" "RRE" 14
		       "B91D",  // 4710 "B91D" "DSGFR" "RRE" 14
		       "B91E",  // 4720 "B91E" "KMAC" "RRE" 14
		       "B91F",  // 4730 "B91F" "LRVR" "RRE" 14
		       "B920",  // 4740 "B920" "CGR" "RRE" 14
		       "B921",  // 4750 "B921" "CLGR" "RRE" 14
		       "B925",  // 4760 "B925" "STURG" "RRE" 14
		       "B92E",  // 4770 "B92E" "KM" "RRE" 14
		       "B92F",  // 4780 "B92F" "KMC" "RRE" 14
		       "B930",  // 4790 "B930" "CGFR" "RRE" 14
		       "B931",  // 4800 "B931" "CLGFR" "RRE" 14
		       "B93E",  // 4810 "B93E" "KIMD" "RRE" 14
		       "B93F",  // 4820 "B93F" "KLMD" "RRE" 14
		       "B946",  // 4830 "B946" "BCTGR" "RRE" 14
		       "B980",  // 4840 "B980" "NGR" "RRE" 14
		       "B981",  // 4850 "B981" "OGR" "RRE" 14
		       "B982",  // 4860 "B982" "XGR" "RRE" 14
		       "B986",  // 4870 "B986" "MLGR" "RRE" 14
		       "B987",  // 4880 "B987" "DLGR" "RRE" 14
		       "B988",  // 4890 "B988" "ALCGR" "RRE" 14
		       "B989",  // 4900 "B989" "SLBGR" "RRE" 14
		       "B98A",  // 4910 "B98A" "CSPG" "RRE" 14
		       "B98D",  // 4920 "B98D" "EPSW" "RRE" 14
		       "B98E",  // 4930 "B98E" "IDTE" "RRF" 15
		       "B990",  // 4940 "B990" "TRTT" "RRE" 14
		       "B991",  // 4950 "B991" "TRTO" "RRE" 14
		       "B992",  // 4960 "B992" "TROT" "RRE" 14
		       "B993",  // 4970 "B993" "TROO" "RRE" 14
		       "B996",  // 4980 "B996" "MLR" "RRE" 14
		       "B997",  // 4990 "B997" "DLR" "RRE" 14
		       "B998",  // 5000 "B998" "ALCR" "RRE" 14
		       "B999",  // 5010 "B999" "SLBR" "RRE" 14
		       "B99A",  // 5020 "B99A" "EPAIR" "RRE" 14
		       "B99B",  // 5030 "B99B" "ESAIR" "RRE" 14
		       "B99D",  // 5040 "B99D" "ESEA" "RRE" 14
		       "B99E",  // 5050 "B99E" "PTI" "RRE" 14
		       "B99F",  // 5060 "B99F" "SSAIR" "RRE" 14
		       "B9B0",  // 5070 "B9B0" "CU14" "RRE" 14
		       "B9B1",  // 5080 "B9B1" "CU24" "RRE" 14
		       "B9B2",  // 5090 "B9B2" "CU41" "RRE" 14
		       "B9B3",  // 5100 "B9B3" "CU42" "RRE" 14
		       "B9BE",  // 5110 "B9BE" "SRSTU" "RRE" 14
		       "BA",  // 5120 "BA" "CS" "RS" 10
		       "BB",  // 5130 "BB" "CDS" "RS" 10
		       "BD",  // 5140 "BD" "CLM" "RS" 10
		       "BE",  // 5150 "BE" "STCM" "RS" 10
		       "BF",  // 5160 "BF" "ICM" "RS" 10
		       "C00",  // 5170 "C00" "LARL" "RIL" 16
		       "C04",  // 5180 "C04" "BRCL" "RIL" 16
		       "C0F4",  // 5190 "C0F4" "BRUL" "RIL" 16
		       "C0F4",  // 5200 "C0F4" "BLU" "RIL" 16
		       "C05",  // 5210 "C05" "BRASL" "RIL" 16
		       "C05",  // 5220 "C05" "JASL" "RIL" 16
		       "D0",  // 5230 "D0" "TRTR" "SS" 17
		       "D1",  // 5240 "D1" "MVN" "SS" 17
		       "D2",  // 5250 "D2" "MVC" "SS" 17
		       "D3",  // 5260 "D3" "MVZ" "SS" 17
		       "D4",  // 5270 "D4" "NC" "SS" 17
		       "D5",  // 5280 "D5" "CLC" "SS" 17
		       "D6",  // 5290 "D6" "OC" "SS" 17
		       "D7",  // 5300 "D7" "XC" "SS" 17
		       "D9",  // 5310 "D9" "MVCK" "SS" 17
		       "DA",  // 5320 "DA" "MVCP" "SS" 17
		       "DB",  // 5330 "DB" "MVCS" "SS" 17
		       "DC",  // 5340 "DC" "TR" "SS" 17
		       "DD",  // 5350 "DD" "TRT" "SS" 17
		       "DE",  // 5360 "DE" "ED" "SS" 17
		       "DF",  // 5370 "DF" "EDMK" "SS" 17
		       "E1",  // 5380 "E1" "PKU" "SS" 17
		       "E2",  // 5390 "E2" "UNPKU" "SS" 17
		       "E303",  // 5400 "E303" "LRAG" "RXY" 18
		       "E304",  // 5410 "E304" "LG" "RXY" 18
		       "E306",  // 5420 "E306" "CVBY" "RXY" 18
		       "E308",  // 5430 "E308" "AG" "RXY" 18
		       "E309",  // 5440 "E309" "SG" "RXY" 18
		       "E30A",  // 5450 "E30A" "ALG" "RXY" 18
		       "E30B",  // 5460 "E30B" "SLG" "RXY" 18
		       "E30C",  // 5470 "E30C" "MSG" "RXY" 18
		       "E30D",  // 5480 "E30D" "DSG" "RXY" 18
		       "E30E",  // 5490 "E30E" "CVBG" "RXY" 18
		       "E30F",  // 5500 "E30F" "LRVG" "RXY" 18
		       "E313",  // 5510 "E313" "LRAY" "RXY" 18
		       "E314",  // 5520 "E314" "LGF" "RXY" 18
		       "E315",  // 5530 "E315" "LGH" "RXY" 18
		       "E316",  // 5540 "E316" "LLGF" "RXY" 18
		       "E317",  // 5550 "E317" "LLGT" "RXY" 18
		       "E318",  // 5560 "E318" "AGF" "RXY" 18
		       "E319",  // 5570 "E319" "SGF" "RXY" 18
		       "E31A",  // 5580 "E31A" "ALGF" "RXY" 18
		       "E31B",  // 5590 "E31B" "SLGF" "RXY" 18
		       "E31C",  // 5600 "E31C" "MSGF" "RXY" 18
		       "E31D",  // 5610 "E31D" "DSGF" "RXY" 18
		       "E31E",  // 5620 "E31E" "LRV" "RXY" 18
		       "E31F",  // 5630 "E31F" "LRVH" "RXY" 18
		       "E320",  // 5640 "E320" "CG" "RXY" 18
		       "E321",  // 5650 "E321" "CLG" "RXY" 18
		       "E324",  // 5660 "E324" "STG" "RXY" 18
		       "E326",  // 5670 "E326" "CVDY" "RXY" 18
		       "E32E",  // 5680 "E32E" "CVDG" "RXY" 18
		       "E32F",  // 5690 "E32F" "STRVG" "RXY" 18
		       "E330",  // 5700 "E330" "CGF" "RXY" 18
		       "E331",  // 5710 "E331" "CLGF" "RXY" 18
		       "E33E",  // 5720 "E33E" "STRV" "RXY" 18
		       "E33F",  // 5730 "E33F" "STRVH" "RXY" 18
		       "E346",  // 5740 "E346" "BCTG" "RXY" 18
		       "E350",  // 5750 "E350" "STY" "RXY" 18
		       "E351",  // 5760 "E351" "MSY" "RXY" 18
		       "E354",  // 5770 "E354" "NY" "RXY" 18
		       "E355",  // 5780 "E355" "CLY" "RXY" 18
		       "E356",  // 5790 "E356" "OY" "RXY" 18
		       "E357",  // 5800 "E357" "XY" "RXY" 18
		       "E358",  // 5810 "E358" "LY" "RXY" 18
		       "E359",  // 5820 "E359" "CY" "RXY" 18
		       "E35A",  // 5830 "E35A" "AY" "RXY" 18
		       "E35B",  // 5840 "E35B" "SY" "RXY" 18
		       "E35E",  // 5850 "E35E" "ALY" "RXY" 18
		       "E35F",  // 5860 "E35F" "SLY" "RXY" 18
		       "E370",  // 5870 "E370" "STHY" "RXY" 18
		       "E371",  // 5880 "E371" "LAY" "RXY" 18
		       "E372",  // 5890 "E372" "STCY" "RXY" 18
		       "E373",  // 5900 "E373" "ICY" "RXY" 18
		       "E376",  // 5910 "E376" "LB" "RXY" 18
		       "E377",  // 5920 "E377" "LGB" "RXY" 18
		       "E378",  // 5930 "E378" "LHY" "RXY" 18
		       "E379",  // 5940 "E379" "CHY" "RXY" 18
		       "E37A",  // 5950 "E37A" "AHY" "RXY" 18
		       "E37B",  // 5960 "E37B" "SHY" "RXY" 18
		       "E380",  // 5970 "E380" "NG" "RXY" 18
		       "E381",  // 5980 "E381" "OG" "RXY" 18
		       "E382",  // 5990 "E382" "XG" "RXY" 18
		       "E386",  // 6000 "E386" "MLG" "RXY" 18
		       "E387",  // 6010 "E387" "DLG" "RXY" 18
		       "E388",  // 6020 "E388" "ALCG" "RXY" 18
		       "E389",  // 6030 "E389" "SLBG" "RXY" 18
		       "E38E",  // 6040 "E38E" "STPQ" "RXY" 18
		       "E38F",  // 6050 "E38F" "LPQ" "RXY" 18
		       "E390",  // 6060 "E390" "LLGC" "RXY" 18
		       "E391",  // 6070 "E391" "LLGH" "RXY" 18
		       "E396",  // 6080 "E396" "ML" "RXY" 18
		       "E397",  // 6090 "E397" "DL" "RXY" 18
		       "E398",  // 6100 "E398" "ALC" "RXY" 18
		       "E399",  // 6110 "E399" "SLB" "RXY" 18
		       "E500",  // 6120 "E500" "LASP" "SSE" 19
		       "E501",  // 6130 "E501" "TPROT" "SSE" 19
		       "E502",  // 6140 "E502" "STRAG" "SSE" 19
		       "E50E",  // 6150 "E50E" "MVCSK" "SSE" 19
		       "E50F",  // 6160 "E50F" "MVCDK" "SSE" 19
		       "E8",  // 6170 "E8" "MVCIN" "SS" 17
		       "E9",  // 6180 "E9" "PKA" "SS" 17
		       "EA",  // 6190 "EA" "UNPKA" "SS" 17
		       "EB04",  // 6200 "EB04" "LMG" "RSY" 20
		       "EB0A",  // 6210 "EB0A" "SRAG" "RSY" 20
		       "EB0B",  // 6220 "EB0B" "SLAG" "RSY" 20
		       "EB0C",  // 6230 "EB0C" "SRLG" "RSY" 20
		       "EB0D",  // 6240 "EB0D" "SLLG" "RSY" 20
		       "EB0F",  // 6250 "EB0F" "TRACG" "RSY" 20
		       "EB14",  // 6260 "EB14" "CSY" "RSY" 20
		       "EB1C",  // 6270 "EB1C" "RLLG" "RSY" 20
		       "EB1D",  // 6280 "EB1D" "RLL" "RSY" 20
		       "EB20",  // 6290 "EB20" "CLMH" "RSY" 20
		       "EB21",  // 6300 "EB21" "CLMY" "RSY" 20
		       "EB24",  // 6310 "EB24" "STMG" "RSY" 20
		       "EB25",  // 6320 "EB25" "STCTG" "RSY" 20
		       "EB26",  // 6330 "EB26" "STMH" "RSY" 20
		       "EB2C",  // 6340 "EB2C" "STCMH" "RSY" 20
		       "EB2D",  // 6350 "EB2D" "STCMY" "RSY" 20
		       "EB2F",  // 6360 "EB2F" "LCTLG" "RSY" 20
		       "EB30",  // 6370 "EB30" "CSG" "RSY" 20
		       "EB31",  // 6380 "EB31" "CDSY" "RSY" 20
		       "EB3E",  // 6390 "EB3E" "CDSG" "RSY" 20
		       "EB44",  // 6400 "EB44" "BXHG" "RSY" 20
		       "EB45",  // 6410 "EB45" "BXLEG" "RSY" 20
		       "EB51",  // 6420 "EB51" "TMY" "SIY" 21
		       "EB52",  // 6430 "EB52" "MVIY" "SIY" 21
		       "EB54",  // 6440 "EB54" "NIY" "SIY" 21
		       "EB55",  // 6450 "EB55" "CLIY" "SIY" 21
		       "EB56",  // 6460 "EB56" "OIY" "SIY" 21
		       "EB57",  // 6470 "EB57" "XIY" "SIY" 21
		       "EB80",  // 6480 "EB80" "ICMH" "RSY" 20
		       "EB81",  // 6490 "EB81" "ICMY" "RSY" 20
		       "EB8E",  // 6500 "EB8E" "MVCLU" "RSY" 20
		       "EB8F",  // 6510 "EB8F" "CLCLU" "RSY" 20
		       "EB90",  // 6520 "EB90" "STMY" "RSY" 20
		       "EB96",  // 6530 "EB96" "LMH" "RSY" 20
		       "EB98",  // 6540 "EB98" "LMY" "RSY" 20
		       "EB9A",  // 6550 "EB9A" "LAMY" "RSY" 20
		       "EB9B",  // 6560 "EB9B" "STAMY" "RSY" 20
		       "EBC0",  // 6570 "EBC0" "TP" "RSL" 22
		       "EC44",  // 6580 "EC44" "BRXHG" "RIE" 23
		       "EC44",  // 6590 "EC44" "JXHG" "RIE" 23
		       "EC45",  // 6600 "EC45" "BRXLG" "RIE" 23
		       "EC45",  // 6610 "EC45" "JXLG" "RIE" 23
		       "ED04",  // 6620 "ED04" "LDEB" "RXE" 24
		       "ED05",  // 6630 "ED05" "LXDB" "RXE" 24
		       "ED06",  // 6640 "ED06" "LXEB" "RXE" 24
		       "ED07",  // 6650 "ED07" "MXDB" "RXE" 24
		       "ED08",  // 6660 "ED08" "KEB" "RXE" 24
		       "ED09",  // 6670 "ED09" "CEB" "RXE" 24
		       "ED0A",  // 6680 "ED0A" "AEB" "RXE" 24
		       "ED0B",  // 6690 "ED0B" "SEB" "RXE" 24
		       "ED0C",  // 6700 "ED0C" "MDEB" "RXE" 24
		       "ED0D",  // 6710 "ED0D" "DEB" "RXE" 24
		       "ED0E",  // 6720 "ED0E" "MAEB" "RXF" 25
		       "ED0F",  // 6730 "ED0F" "MSEB" "RXF" 25
		       "ED10",  // 6740 "ED10" "TCEB" "RXE" 24
		       "ED11",  // 6750 "ED11" "TCDB" "RXE" 24
		       "ED12",  // 6760 "ED12" "TCXB" "RXE" 24
		       "ED14",  // 6770 "ED14" "SQEB" "RXE" 24
		       "ED15",  // 6780 "ED15" "SQDB" "RXE" 24
		       "ED17",  // 6790 "ED17" "MEEB" "RXE" 24
		       "ED18",  // 6800 "ED18" "KDB" "RXE" 24
		       "ED19",  // 6810 "ED19" "CDB" "RXE" 24
		       "ED1A",  // 6820 "ED1A" "ADB" "RXE" 24
		       "ED1B",  // 6830 "ED1B" "SDB" "RXE" 24
		       "ED1C",  // 6840 "ED1C" "MDB" "RXE" 24
		       "ED1D",  // 6850 "ED1D" "DDB" "RXE" 24
		       "ED1E",  // 6860 "ED1E" "MADB" "RXF" 25
		       "ED1F",  // 6870 "ED1F" "MSDB" "RXF" 25
		       "ED24",  // 6880 "ED24" "LDE" "RXE" 24
		       "ED25",  // 6890 "ED25" "LXD" "RXE" 24
		       "ED26",  // 6900 "ED26" "LXE" "RXE" 24
		       "ED2E",  // 6910 "ED2E" "MAE" "RXF" 25
		       "ED2F",  // 6920 "ED2F" "MSE" "RXF" 25
		       "ED34",  // 6930 "ED34" "SQE" "RXE" 24
		       "ED35",  // 6940 "ED35" "SQD" "RXE" 24
		       "ED37",  // 6950 "ED37" "MEE" "RXE" 24
		       "ED3E",  // 6960 "ED3E" "MAD" "RXF" 25
		       "ED3F",  // 6970 "ED3F" "MSD" "RXF" 25
		       "ED64",  // 6980 "ED64" "LEY" "RXY" 18
		       "ED65",  // 6990 "ED65" "LDY" "RXY" 18
		       "ED66",  // 7000 "ED66" "STEY" "RXY" 18
		       "ED67",  // 7010 "ED67" "STDY" "RXY" 18
		       "EE",  // 7020 "EE" "PLO" "SS3" 27
		       "EF",  // 7030 "EF" "LMD" "SS4" 28
		       "F0",  // 7040 "F0" "SRP" "SS5" 29
		       "F1",  // 7050 "F1" "MVO" "SS2" 26
		       "F2",  // 7060 "F2" "PACK" "SS2" 26
		       "F3",  // 7070 "F3" "UNPK" "SS2" 26
		       "F8",  // 7080 "F8" "ZAP" "SS2" 26
		       "F9",  // 7090 "F9" "CP" "SS2" 26
		       "FA",  // 7100 "FA" "AP" "SS2" 26
		       "FB",  // 7110 "FB" "SP" "SS2" 26
		       "FC",  // 7120 "FC" "MP" "SS2" 26
		       "FD",  // 7130 "FD" "DP" "SS2" 26
			}; 
      /*
       * key search table data
       */
      static int max_key_root = 1023;
      int max_key_tab = max_key_root + max_op;
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
   * end of global ez390 class data and start of procs
   */
public static void main(String[] args) {
  /*
   * main is entry when executed from command line
   * Create instance of ez390 class and pass
   * parms to ez390 like z390 does.
   */
      ez390 pgm = new ez390();
      pgm.process_ez390(args,null,null);
}
public int process_ez390(String[] args,JTextArea log_text,JTextField command_text){
   /*
    *  execute 390 load module file passed as first arg
    *
    *  Note this may be called directly from z390 GUI or
    *  from main when lz370 run from windows command line.
    *  if called from main, the log_text balect will be null
    *  and local put_log function will route to console instead
    *  of the z390 log window.
    */
	init_ez390(args,log_text,command_text);
	reg.putInt(r13,cvt_save);
	reg.putInt(r14,cvt_exit);
	reg.putInt(r0,cvt_ipl_pgm);  
	svc_link();  
   	if (opt_trap){
   	    psw_retry = true;
   	    while (psw_retry){
    		try {
    	   		exec_390();
    	   		if (psw_pic != psw_pic_exit){
    	   		    psw_pic_handler();
    	   		    psw_retry = true;
    	   		}
    		} catch (Exception e){  
                 psw_pic_handler();
                 psw_retry = true; // retry if no abend
    	    }
    	}
   	} else {
    	exec_390(); // notrap - no exception handling
    }
    if (psw_check && psw_pic != psw_pic_exit){
       	svc_abend(psw_pic);
    }
	exit_ez390();
	if (log_text == null){
	   System.exit(ez390_rc);
	}
	return ez390_rc;
}
private void init_ez390(String[] args, JTextArea log_text, JTextField command_text){
	/*
	 * 1.  initialize log routing
	 * 2.  init ascii to ebcdic table
	 * 3.  init reqular expression paser for test
	 * 4.  set options
	 * 5.  initialize memory
	 * 6.  set runtime hooks for cancel
	 * 
	 */
	    if  (log_text != null){
	    	z390_log_text = log_text;
	    }
	    if (command_text != null){
	    	z390_command_text = command_text;
	    }
        process_args(args);
        set_options();
        init_time();
        init_ascii_ebcdic();
        init_test();
		open_files();
		init_mem();
        put_copyright();
       	init_opcodes();
        fp_lh_zero[0] = (byte)0x40;
        fp_lh_zero[8] = (byte)0x40;
}
private void process_args(String[] args){
	/*
	 * process options
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
	    abort_error(1,"missing 390 file");
    }
}
private void set_dir_and_pgm_name(String file_name){
	/*
	 * set program name and default directory
	 * for all files from first parm.
	 * Notes:
	 *   1.  Use current directory if not path
	 *       specified with program name.
	 *   2.  Options SYS390 and SYSLOG
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
	dir_390 = dir_cur;
	dir_log = dir_cur;
    index = file_name.lastIndexOf("\\.");
    if (index != -1){  // strip extension if any
    	pgm_name = file_name.substring(0,index);
    } else {
    	pgm_name = file_name;
    }
}
private void exit_ez390(){
	/*
	 * display total errors
	 * close files and exit
	 */
	  if    (ez390_rc == 0 && ez390_errors > 0){
    	ez390_rc = 16;
      }
  	  put_stats();
      close_files();
	  if    (ez390_aborted){
	    	System.exit(ez390_rc);
	  }
}
private void put_stats(){
	/*
	 * display statistics as comments at end of bal
	 */
	put_log("Stats total instructions    = " + tot_ins);
	if (opt_trace){
	   put_log("Stats Keys                  = " + tot_key);
	   put_log("Stats Key searches          = " + tot_key_search);
	   if (tot_key_search > 0){
	       avg_key_comp = tot_key_comp/tot_key_search;
	   }
	   put_log("Stats Key avg comps         = " + avg_key_comp);
	   put_log("Stats Key max comps         = " + max_key_comp);
	}
	if (opt_timing){ // dusplay instr rate
	   cur_date = new Date();
	   tod_end_pgm = cur_date.getTime();
	   tot_sec = (tod_end_pgm - tod_start_pgm)/1000;
	   put_log("Stats total seconds         = " + tot_sec);
	   long ins_rate = ((long) tot_ins)*1000/(tod_end_pgm - tod_start_pgm + 1);
	   put_log("Stats instructions/sec      = " + ins_rate);
	}
	put_log("EZ390 total errors          = " + ez390_errors);
	put_log("EZ390 return code           = " + ez390_rc);
}
private void close_files(){
	  if (log_file != null && log_file.isFile()){
	  	  try {
	  	  	  log_file_buff.close();
	  	  } catch (IOException e){
	  	  	  abort_error(3,"I/O error on log file close - " + e.toString());
	  	  }
	  }
	  if  (opt_list){
		  if (log_file != null && log_file.isFile()){
		  	  try {
		  	  	  log_file_buff.close();
		  	  } catch (IOException e){
		  	  	  abort_error(4,"I/O error on log file close - " + e.toString());
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
      put_log("ez390 error " + error + " " + msg);
	  ez390_errors++;
	  if (ez390_errors > max_errors){
	  	 abort_error(5,"max errors exceeded");	 
	  }
}
private void abort_error(int error,String msg){
	/*
	 * issue error msg to log with prefix and
	 * inc error total
	 */
	  ez390_errors++;
	  if (ez390_aborted){
	  	 System.exit(16);
	  }
	  ez390_aborted = true;
	  if (ez390_rc == 0){
	  	 ez390_rc = 16;
	  }
	  put_log("ez390 error " + error + " " + msg);
      exit_ez390();
}
private void put_copyright(){
	   /*
	    * display ez390 version, timestamp,
	    * and copyright if running standalone
	    */
	   	if  (opt_timing){ // display current date/time
			cur_date = new Date();
	   	    put_log("ez390 " + version 
	   			+ " Current Date " + cur_date_MMddyy.format(cur_date)
	   			+ " Time " + cur_tod_hhmmss.format(cur_date));
	   	} else {
	   	    put_log("ez390 " + version);
	   	}
	   	if  (z390_log_text == null){
	   	    put_log("Copyright 2005 Automated Software Tools Corporation");
	   	    put_log("z390 is licensed under GNU General Public License");
	   	}
	   	put_log("EZ390 program = " + pgm_name);
	   	put_log("EZ390 options = " + opt_parms);
	   }
	   private synchronized void put_log(String msg) {
	   	/*
	   	 * Write message to z390_log_text and/or con
	   	 * if running standalone
	   	 * 
	   	 */
   	    	put_log_line(msg);
	        if  (z390_log_text != null){
  	        	z390_log_text.append(msg + "\n");
	        }
   	        if (opt_con || opt_test){
   	    	    System.out.println(msg);
   	        }
	   }
	   private void put_log_line(String msg){
	   /*
	    * put line to listing file
	    */
	   	   if (opt_list){
	   	      try {
	   	          log_file_buff.write(msg + "\r\n");
	   	      } catch (Exception e){
	   	          abort_error(6,"I/O error on log file write");
	   	      }
	   	   }
	   }
private void open_files(){
	/*
	 * open 390 and lst files
	 */
       	if (opt_list){
            log_file = new File(dir_log + pgm_name + ".LOG");
         	try {
       	       log_file_buff = new BufferedWriter(new FileWriter(log_file));
       	    } catch (IOException e){
       		   abort_error(9,"I/O error on log file open - " + e.toString());
       	    }
       	}
}
private void set_options(){
	/*
	 * parse and set options
	 * Notes:
	 *   1.  These use () vs = because bat removes =
	 *        syslog(ddname)
	 *        sys390(ddname)
	 *        test(ddname)
	 *        time(seconds)
	 */
        String[] tokens = opt_parms.toUpperCase().split("\\s+");
        int index1 = 0;
        while (index1 < tokens.length){
            if (tokens[index1].equals("CON")){
            	opt_con = true;
            } else if (tokens[index1].length() > 5
            		&& tokens[index1].substring(0,4).equals("MEM(")){
            	try {
            	    max_mem = Integer.valueOf(tokens[index1].substring(4,tokens[index1].length()-1)).intValue();
            	} catch (Exception e){
            		abort_error(60,"invalid memory option " + tokens[index1]);
            	}
            } else if (tokens[index1].equals("NOLIST")){
            	opt_list = false;
            } else if (tokens[index1].equals("REGS")){
            	opt_regs = true;
            	opt_list  = true;
            } else if (tokens[index1].equals("NOCON")){
            	opt_con = false;
            } else if (tokens[index1].equals("NOSTATS")){
            	opt_stats = false;
            } else if (tokens[index1].equals("NOTIME")){
            	opt_time = false;
            } else if (tokens[index1].equals("NOTIMING")){
            	opt_timing = false;
            	opt_time   = false;
            } else if (tokens[index1].equals("NOTRAP")){
            	opt_trap = false;
            } else if (tokens[index1].length() > 5
            		&& tokens[index1].substring(0,5).equals("PARM(")){
            	z390_pgm_parm = tokens[index1].substring(5,tokens[index1].length()-1);
            } else if (tokens[index1].length() > 7
            		&& tokens[index1].substring(0,7).equals("SYS390(")){
            	dir_390 = tokens[index1].substring(7,tokens[index1].length()-1) + File.separator;	
            } else if (tokens[index1].length() > 7
            		&& tokens[index1].substring(0,7).equals("SYSLOG(")){
            	dir_log = tokens[index1].substring(7,tokens[index1].length()-1) + File.separator;	
            } else if (tokens[index1].length() > 5
            		&& tokens[index1].substring(0,5).equals("TIME(")){
            	max_time_seconds = Long.valueOf(tokens[index1].substring(5,tokens[index1].length()-1)).longValue();
            } else if (tokens[index1].equals("TEST")){
            	opt_test = true;
            } else if (tokens[index1].length() > 5
            		&& tokens[index1].substring(0,5).equals("TEST(")){
            	test_ddname = tokens[index1].substring(5,tokens[index1].length()-1);	
            	opt_test = true;
            } else if (tokens[index1].equals("TRACE")){
            	opt_trace = true;
            	opt_list   = true;
            	opt_con   = false;
            } else if (tokens[index1].equals("TRACEALL")){
            	opt_traceall = true;
            	opt_trace = true;
            	opt_tracemem = true;
            	opt_list   = true;
            	opt_con   = false;
            } else if (tokens[index1].equals("TRACEMEM")){
            	opt_tracemem = true;
            }
            index1++;
        }
   }
private void init_time(){
	/*
	 * init cur_date and calendar with
	 * current time and date or force
	 * fixed time if NOTIMING option set.
	 * Notes:
	 *   1.  This NOTTIMING option is used in
	 *       regression testing timing functions
	 *       for repeatable results.
	 */
	if (opt_timing){ // measure and display timing
	    cur_date = new Date();
	    cur_date_year  = Integer.valueOf(cur_date_yyyy.format(cur_date));
	    cur_date_month = Integer.valueOf(cur_date_MM.format(cur_date));
	    cur_date_day   = Integer.valueOf(cur_date_dd.format(cur_date));
	    tod_hour  = Integer.valueOf(cur_date_HH.format(cur_date));
	    tod_min   = Integer.valueOf(cur_date_mm.format(cur_date));
	    tod_sec   = Integer.valueOf(cur_date_ss.format(cur_date));
	    tod_msec  = Integer.valueOf(cur_date_ms.format(cur_date));
	    cur_date_cal = new GregorianCalendar(cur_date_year,cur_date_month,cur_date_day);
	    tod_start_day = cur_date_cal.getTime().getTime();
	    tod_start_pgm = cur_date.getTime();
        if (opt_time){
        	tod_time_limit = max_time_seconds *1000 + tod_start_pgm;
        }
	} else {
	    cur_date_cal = new GregorianCalendar(2005,0,2,22,33,44);
	    cur_date = new Date(cur_date_cal.getTime().getTime()+567);
	    cur_date_year  = Integer.valueOf(cur_date_yyyy.format(cur_date));
	    cur_date_month = Integer.valueOf(cur_date_MM.format(cur_date));
	    cur_date_day   = Integer.valueOf(cur_date_dd.format(cur_date));
	    tod_hour  = Integer.valueOf(cur_date_HH.format(cur_date));
	    tod_min   = Integer.valueOf(cur_date_mm.format(cur_date));
	    tod_sec   = Integer.valueOf(cur_date_ss.format(cur_date));
	    tod_msec  = Integer.valueOf(cur_date_ms.format(cur_date));
	    cur_date_cal = new GregorianCalendar(cur_date_year,cur_date_month-1,cur_date_day);
	    tod_start_day = cur_date_cal.getTime().getTime();
	    tod_start_pgm = cur_date.getTime();
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
private void init_test(){
	/*
	 * 1. init test regular expression parser
	 * 2. init optional test=ddname file for batch input 
     *
     * expression pattern
     *   1. self defining terms 
     *         B'01'
     *         C'ABC'
     *         F'nnn'
     *         H'nnn'
     *         X'0F' 
     *   2. register
     *         nr or nR
     *   3. memory address
     *         hex. for absolute memory address
     *         dec  for decimal addr (for list len)
     *         +hex or -hex for rel base address
     *         *+hex or *-hex for rel instr. addr
     *         nnr% for 24 bit indirect reg addr
     *         nnR& for 31 bit indirect reg addr
     *   3. break compare operators
     *         =,<,>,!=,>=,<=      
     *   3. test commands (b,g,h,l,m,q,t)
     *   4. test break opcode names
     *   5. set operator =
     */
   	try {
   	    test_pattern = Pattern.compile(
		    "([bB]['][0|1]+['])"            // sdt 
		  +	"|([cC][']([^']|(['][']))*['])" // sdt
   		  +	"|([fF]['][-]*[0-9]+['])"       // sdt
   		  +	"|([hH]['][-]*[0-9]+['])"       // sdt
   		  +	"|([xX]['][0-9a-fA-F]+['])"     // sdt
		  + "|([0-9]+[rR][%?]*)"            // reg addr
		  + "|([0-9a-fA-F]+[\\.])"          // hex. addr
		  + "|([\\*]*[+-][0-9a-fA-F]+)"     // *+-hex addr
		  + "|([0-9]+)"                     // dec
		  + "|([a-zA-Z]+)"                  // cmd or opcode
		  + "|([*])"                        // * addr or comment
		  + "|([=])|([!][=])|([>][=]*)|([<][=]*)"  // set break compare operator
   	    );
   	} catch (Exception e){
   		  abort_error(56,"test error in expression pattern - " + e.toString());
   	}
	if (test_ddname != null){
		test_file_name = get_ddname_file_name(test_ddname);
         try {
        	 test_cmd_file = new BufferedReader(new FileReader(test_file_name));
         } catch (Exception e){
        	 abort_error(58,"test input file for ddname " + test_ddname + " not found - " + test_file_name);
         }
	} else {
		test_cmd_file = new BufferedReader (new InputStreamReader(System.in));
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
private void init_opcodes(){
	/*
	 * add all opcodes to key index table
	 * for use by trace and test options
	 * Notes:
	 *   1.  trace and test do lookup by hex for display
	 *   2.  test break on opcode does lookup by name
	 *       to get hex code for break
	 *   3.  op_type_index is used by test break on
	 *       opcode to get opcode2 offset and mask
	 */
	String hex_key = null;
	if (max_op != op_code.length
			|| max_op != op_type.length-1){ // extra entry for comments
		abort_error(19,"opcode name, type, and hex tables out of sync");
	}
	int index = 0;
	while (index < max_op){
		// add key by name
		if (find_key_index(op_name[index]) == -1){
			add_key_index(index); 
		}
		// add key by hex code 
		hex_key = op_code[index];
		if (hex_key.length() == 3){
			hex_key = hex_key.substring(0,2) + "0" + hex_key.charAt(2);
			if (hex_key.charAt(1) == '7'
				&& (hex_key.charAt(0) == '4'
					|| hex_key.charAt(0) == '0')){
				hex_key = "BC=" + hex_key;
			}
		} else if (hex_key.length() == 4
				   && hex_key.substring(0,3).equals("A74")){
			hex_key = "BR=" + hex_key;
		}
		if (find_key_index(hex_key) == -1){
			add_key_index(index);
		} else if (opt_traceall){
            dup_opcodes++;
		}
		index++;
	}
	// init opcode2_offsets  and masks for each type
	   op_type_offset[1 ] =  1; // E   PR    oooo
	   op_type_offset[7] =   1; // S   SSM   oooobddd
       op_type_offset[12] =  1; // RI  IIHH  ooroiiii
	   op_type_offset[13] =  1; // BRE BRC   oomoiiii
       op_type_offset[14] =  1; // RRE MSR   oooo00rr
	   op_type_offset[15] =  1; // RRF MAER  oooor0rr
	   op_type_offset[16] =  1; // RIL BRCL  oomollllllll
	   op_type_offset[18] =  5; // RXY MLG   oorxbdddhhoo
	   op_type_offset[19] =  1; // SSE LASP  oooobdddbddd
	   op_type_offset[20] =  5; // RSY LMG   oorrbdddhhoo
	   op_type_offset[21] =  5; // SIY TMY   ooiibdddhhoo
	   op_type_offset[22] =  5; // RSL TP    oor0bddd00oo
	   op_type_offset[23] =  5; // RIE BRXLG oorriiii00oo
	   op_type_offset[24] =  5; // RXE ADB   oorxbddd00oo
	   op_type_offset[25] =  5; // RXF MAE   oorxbdddr0oo
	   op_type_offset[30] =  1; // RRF DIER  oooor0rr
	   op_type_mask[1 ] =  0xff; // E   PR    oooo
	   op_type_mask[7] =   0xff; // S   SSM   oooobddd
       op_type_mask[12] =  0x0f; // RI  IIHH  ooroiiii
	   op_type_mask[13] =  0xf0; // BRE BRC   oomoiiii
       op_type_mask[14] =  0xff; // RRE MSR   oooo00rr
	   op_type_mask[15] =  0xff; // RRF MAER  oooor0rr
	   op_type_mask[16] =  0x0f; // RIL BRCL  oomollllllll
	   op_type_mask[18] =  0xff; // RXY MLG   oorxbdddhhoo
	   op_type_mask[19] =  0xff; // SSE LASP  oooobdddbddd
	   op_type_mask[20] =  0xff; // RSY LMG   oorrbdddhhoo
	   op_type_mask[21] =  0xff; // SIY TMY   ooiibdddhhoo
	   op_type_mask[22] =  0xff; // RSL TP    oor0bddd00oo
	   op_type_mask[23] =  0xff; // RIE BRXLG oorriiii00oo
	   op_type_mask[24] =  0xff; // RXE ADB   oorxbddd00oo
	   op_type_mask[25] =  0xff; // RXF MAE   oorxbdddr0oo
	   op_type_mask[30] =  0xff; // RRF DIER  oooor0rr
    // init op2 offset and mask arrays indexed by op1
    index = 0;
    while (index < max_op){
		int op1 = Integer.valueOf(op_code[index].substring(0,2),16).intValue();
		if (opcode1_type[op1] == 0){
		    opcode1_type[op1] = op_type[index];
		}
    	if (op_code[index].length() > 2){
    		opcode2_offset[op1] = op_type_offset[op_type[index]];
   		    opcode2_mask[op1]   = op_type_mask[op_type[index]];
    	}
    	index++;
    }
    tot_key_search = 0;
	tot_key_comp = 0;
	max_key_comp = 0;
}
private void init_mem(){
	/*
	 * allocate memory and initialize as follows:
	 * 1.  Set doubleword at 0 with EBCDIC name
	 *     of initial program to load via svc 8.
	 * 2.  Set mem_fqe_ptr to point to free queue
	 *     with all of available memory for use by 
	 *     svc 4 getmain and svc 5 freemain.
	 * 
	 */
	tot_mem = max_mem << 20; // cvt MB to bytes
	try {
	    mem_byte = new byte[tot_mem]; // alloc mem
   	} catch (Exception e){
   		abort_error(61,"memory allocation failed for " + max_mem + " MB");
   	}
	mem = ByteBuffer.wrap(mem_byte,0,tot_mem);
    /*
     * init 24 bit free memory queue element
     */
	dsa24_start = mem24_start;
	mem.position(dsa24_start);
	mem.putInt(0); // set 24 bit fqe next to 0
	if (tot_mem > mem24_line){
	   dsa24_end = mem24_line;
	   mem.putInt(dsa24_end-dsa24_start); // set 24 but fqe len
	} else {
	   dsa24_end = tot_mem;
	   mem.putInt(dsa24_end - dsa24_start);
	}
	/*
	 * init 31 bit free memory queue element
	 */
	if (tot_mem > mem24_line){
		dsa31_start = mem24_line;
	    mem.position(dsa31_start);
	    mem.putInt(0); // set 31 bit fqe next = 0
	    dsa31_end = tot_mem;
	    mem.putInt(dsa31_end-dsa31_start); // set 31 bit fqe len 
	}
	/*
	 * init cvt pointer at low memory 16
	 */
	mem.position(16);   
	mem.putInt(cvt_start);  
	/*
	 * init cvt pointers to dsa fqe's for use by 
	 * getmain and freemain svc hanlers
	 */
	mem.position(cvt_fqe24); // 24 bit fqe
	mem.putInt(dsa24_start);
	mem.position(cvt_fqe31); // 31 bit fqe
	mem.putInt(dsa31_start);
	/*
	 * init cvt initial program load pgm name field
	 */
	mem.position(cvt_ipl_pgm);
	int index = 0;
	while (index < pgm_name.length()){
		if (pgm_name.charAt(index) != '.'){
		   mem.put(ascii_to_ebcdic[pgm_name.charAt(index)]);
		} else {
			index = pgm_name.length();
		}
		index++;
	}
	while (index < 8){
		mem.put(ascii_to_ebcdic[32]);
		index++;
	}
	/*
	 * init svc 3 for return register r14
	 */
	mem.putShort(cvt_exit,(short)0x0a03); // svc 3
}
/*
 * *********************************************
 * svc support routines
 *********************************************
 */
 private void svc_exit(){
 	/*
 	 * exit to prev link return address or exit ez390
 	 */
 	if (tot_link_stk == 1){
	    set_psw_check(psw_pic_exit);  // exit ez390
 	} else {
 		int link_ret = link_stk_ret[tot_link_stk - 1];
 		cur_cde      = link_stk_cde[tot_link_stk - 1];
 		tot_link_stk--;
 		svc_delete(cde_name[cur_cde]);
 		set_psw_loc(link_ret); 
 		// exit to link caller in prior amode
 		if ((link_ret & psw_amode31_bit) != 0){
 			set_psw_amode(psw_amode31_bit);
 		} else {
 			set_psw_amode(psw_amode24_bit);
 		}
 	}
 }
private void psw_pic_handler(){
	/*
	 * set psw_loc and turn on psw_retry if 
	 * any of the following conditions found:
	 *   1.  If psw_pic has corresponding 
	 *       psw_pgm_mask bit off, continue
	 *       at next instruction.
	 *   2.  If psw_pic has corresponding 
	 *       bit on in ESPIE PIE mask bits,        
	 *       restart at espie_exit if defined.
	 *   3.  If espie_exit defined, then restart
	 *       at estae_exit.
	 *   4.  else issue svc_abend for psw_pic code.
	 */
	 boolean try_espie  = false;
	 boolean ignore_pic = false;
	 switch (psw_pic){
	 case  0x0c1:
 		try_espie = true;
	 	if (!psw_check){
	 		psw_pic = psw_pic_addr; // default 0C5 for ins trap
	 	}
	 	break;
	 case  0x0c7: // decimal data or IEEE BFP exception
 	    switch (fp_dxc){
	        case 0x00: // packed decimal data exception
	        	try_espie = true;
	        	break;
	 	    case 0x08: // IEEE inexact and truncated
			case 0x0C: // IEEE inexact and incremented
				if ((fp_fpc_reg & fp_fpc_mask_sig) != 0){
					try_espie = true;
				} else {
				 	ignore_pic = true;
				}  
				break;
			case 0x10: // IEEE underflow, exact
			case 0x18: // IEEE underflow, inexact and
			case 0x1C: // IEEE underflow, inexact and
				if ((fp_fpc_reg & fp_fpc_mask_unf) != 0){
					try_espie = true;
				} else {
				 	ignore_pic = true;
				}  
				break;
			case 0x20: // IEEE overflow, exact
			case 0x28: // IEEE overflow, inexact and
			case 0x2C: // IEEE overflow, inexact and
				if ((fp_fpc_reg & fp_fpc_mask_ovf) != 0){
					try_espie = true;
				} else {
				 	ignore_pic = true;
				}  
				break;
			case 0x40: // IEEE division by zero
				if ((fp_fpc_reg & fp_fpc_mask_div) != 0){
					try_espie = true;
				} else {
				 	ignore_pic = true;
				}  
				break;
			case 0x80: // IEEE invalid operation
				if ((fp_fpc_reg & fp_fpc_mask_inv) != 0){
					try_espie = true;
				} else {
				 	ignore_pic = true;
				}  
				break;
	 	}
	 	break;
	 case  0x0c8: // fixed point overflow
	 	if ((psw_pgm_mask & psw_pgm_mask_fix) != 0){
	 		try_espie = true; // try spie then stae
	 	} else {
		 	ignore_pic = true;
	 	}
	 	break;
	 case  0x0ca: // decimal overflow
	 	if ((psw_pgm_mask & psw_pgm_mask_dec) != 0){
	 		try_espie = true; // try spie then stae
	 	} else {
		 	ignore_pic = true;
	 	}
	 	break;
	 case  0x0cd: // floating point exponent underflow
	 	if ((psw_pgm_mask & psw_pgm_mask_exp) != 0){
	 		try_espie = true; // try spie then stae
	 	} else {
		 	ignore_pic = true;
	 	}
	 	break;
	 case  0x0ce: // floating point significance
	 	if ((psw_pgm_mask & psw_pgm_mask_sig) != 0){
	 		try_espie = true; // try spie then stae
	 	} else {
		 	ignore_pic = true;
	 	}
	 	break;
	 case  0x0c2: // privalege
	 case  0x0c3: // execute
	 case  0x0c4: // protection
	 case  0x0c5: // addressing (defualt for ins trap above
	 case  0x0c6: // specification
	 case  0x0c9: // fixed point divide
	 case  0x0cb: // decimal divide
	 case  0x0cc: // HFP floating point exp overflow
	 case  0x0cf: // HFP floating point divide
	 	try_espie = true;
	 	break;
	 }
     if (ignore_pic){
     	psw_check = false;
     	psw_retry = true;
     	return;
     }
     if (try_espie & espie_exit != 0){
     	 int index = psw_pic & 0xf;
     	 if ((espie_pie & (int_high_bit >>> index)) != 0){
 	        set_psw_loc(espie_exit);
 	        psw_check = false;
 	        psw_retry = true;
 	        return;
     	 }
     }
     if (estae_exit != 0){
     	set_psw_loc(estae_exit);
     	psw_check = false;
     	psw_retry = true;
     	return;
     } else {
 	    svc_abend(psw_pic);
     }
}
private void svc_load(){
	/*
	 * load 390 load module into virtual memory
	 * 
	 * Input regs
	 *   1. r0  = 8 byte EBCDIC pgm name padded with spaces
	 * Output regs:
	 *   1. r0  = address of loaded 390 program
	 *            above or below line based on
	 *            lz390 rmode option and with 
	 *            high bit indicating amode
	 *            based on lz390 amode option.
	 *   2. r15 = return code 0 ok else not zero
     *
	 * Notes:
	 *   1.  Add CDE entry for new entry else if already
	 *       loaded, increment cde_use and return 
	 *       existing load address.
	 */
	z390_pgm_name = get_ascii_string(reg.getInt(r0),8);
	cur_cde = find_key_index("P:" + z390_pgm_name);
	if (cur_cde != -1 && cde_loc[cur_cde] != 0){
		cde_use[cur_cde]++;
		reg.putInt(r0,cde_loc[cur_cde]);
		reg.putInt(r15,0);
		return;
	}
	try {
        z390_file = new RandomAccessFile(dir_390 + z390_pgm_name + ".390","r");
        z390_file.seek(0);
        z390_code_ver[0] = (char)z390_file.read();
        z390_code_ver[1] = (char)z390_file.read();
        z390_code_ver[2] = (char)z390_file.read();
        z390_code_ver[3] = (char)z390_file.read();
        z390_flags[0]    = (char)z390_file.read();
        z390_flags[1]    = (char)z390_file.read();
        z390_flags[2]    = (char)z390_file.read();
        z390_flags[3]    = (char)z390_file.read();
        z390_code_len    = z390_file.readInt();
        z390_code_ent    = z390_file.readInt();
        z390_code_rlds   = z390_file.readInt();
        if (z390_flags[z390_flags_rmode31] == 'T'){
        	reg.putInt(r0,opt_getmain_amode31); // getmain above line
        }else {
            reg.putInt(r0,0); // getmain below line
        }
        reg.putInt(r1,z390_code_len);
        svc_getmain();
        if (reg.getInt(r15) == 0){
           z390_code_load = reg.getInt(r0);
           z390_file.read(mem_byte,z390_code_load,z390_code_len);
           if (z390_flags[z390_flags_amode31] == 'T'){
               reg.putInt(r0,z390_code_load | psw_amode31_bit);
           } else {
               reg.putInt(r0,z390_code_load);
           }
        } else {
        	abort_error(15,"getmain for svc 8 load failed - PGM=" + pgm_name + "LEN=" + z390_code_len);
        }
        int cur_rld = 0;
        while (cur_rld < z390_code_rlds){
        	rld_loc = z390_file.readInt();
        	rld_len = z390_file.readByte();
        	mem.position(z390_code_load + rld_loc);
        	int rld_field = mem.getInt();
        	if (rld_len == 4){
        		rld_field = rld_field + z390_code_load;
        	}
    		mem.position(z390_code_load + rld_loc);
        	mem.putInt(rld_field);
        	cur_rld++;
        }
        z390_file.close();
        reg.putInt(r15,0);
        add_cde();
	} catch (Exception e){
	 	abort_error(18,"I/O error on 390 load module file - " + e.toString());
	}
}
private void svc_delete(String pgm_name){
	/*
	 * delete 390 load module from irtual memory
	 * if use count 0 after decrement.
	 * 
	 * Output regs:
	 *   1.  r15 = 0 if successfully deleted
	 *   2.  r15 = 4 if not found in memory
	 */
	cur_cde = find_key_index("P:" + pgm_name);
	if (cur_cde != -1 && cde_loc[cur_cde] != 0){
		cde_use[cur_cde]--;
        if  (cde_use[cur_cde] < 1){
        	reg.putInt(r0,cde_loc[cur_cde]);
        	reg.putInt(r1,cde_len[cur_cde]);
        	svc_freemain();
        	cde_loc[cur_cde] = 0;
        }
		reg.putInt(r15,0);
	} else {
		reg.putInt(r15,4);
	}
}
private void add_cde(){
	/*
	 * add new 390 load module to cde entry table
	 * and set usage to 1
	 */
	cur_cde = find_key_index("P:" + z390_pgm_name);
	if (cur_cde == -1){
		if (tot_cde < max_cde_pgms){
			cur_cde = tot_cde;
			tot_cde++;
			add_key_index(cur_cde);
		} else {
			abort_error(55,"CDE maximum 390 load modules exceeded");
            return;
		}
	}
	cde_name[cur_cde] = z390_pgm_name;
	cde_use[cur_cde]  = 1;
	cde_len[cur_cde]  = z390_code_len;
    if (z390_flags[z390_flags_amode31] == 'T'){
        cde_loc[cur_cde] = z390_code_load | psw_amode31_bit;
        cde_ent[cur_cde] = z390_code_load | psw_amode31_bit;
    } else {
        cde_loc[cur_cde] = z390_code_load;
        cde_ent[cur_cde] = z390_code_load;
    }
}
private void svc_link(){
	/*
	 * load and then balr to 390 load module
	 */
	svc_load();
	int link_addr = reg.getInt(r0);
	if  (reg.getInt(r15) == 0){
		if (tot_link_stk < max_link_stk){
			cur_link_stk = tot_link_stk;
			tot_link_stk++;
			link_stk_cde[cur_link_stk] = cur_cde;
			link_stk_ret[cur_link_stk] = psw_loc | psw_amode_bit;
		}
		reg.putInt(r14,cvt_exit);
		reg.putInt(r15,link_addr);
		set_psw_loc(link_addr);
        if ((link_addr & psw_amode31_bit) != 0){
            set_psw_amode(psw_amode31_bit);
        } else {
            set_psw_amode(psw_amode24_bit);
        }
	}
}
private void svc_getmain(){
	/*
	 * Input
	 *   1.  R1 = length to allocate
	 *   2.  R0 = options
	 *         bit 0 allocate memory above the line
	 * Output:    
	 *   1.  set r0 to address of area
	 *   2.  set r15 to 0 of ok, else nz
	 * Notes:
	 *   1.  Use TRACEMEM option to trace FQE's 
	 *   2.  If no 31 bit memory then allocate from
	 *       24 bit memory else abort if requested
	 *       memory type no available.
	 */
	req_len = reg.getInt(r1);	   
	req_len = (req_len + 7)/8*8; // round to 8
	if (req_len <= 0){
		set_psw_check(psw_pic_gm_err);
		return;
	}
	req_opt = reg.getInt(r0);
	if ((req_opt & opt_getmain_amode31) != 0
		&& dsa31_start != 0){
		cur_fqe = mem.getInt(cvt_fqe31);
		prev_fqe = cvt_fqe31;
	} else {
		cur_fqe = mem.getInt(cvt_fqe24);
		prev_fqe = cvt_fqe24;
	}
	while (cur_fqe > 0){
		cur_fqe_len = mem.getInt(cur_fqe+4);
        if (!check_fqe_ok()){
        	return;
        }
		if (cur_fqe_len >= req_len){
			// allocate from end of cur_fqe
			cur_fqe_len = cur_fqe_len - req_len;
			reg.putInt(r0,cur_fqe + cur_fqe_len);
			if (opt_tracemem){
				trace_mem("GETMAIN ",cur_fqe+cur_fqe_len,req_len,0);
			}
			if (cur_fqe_len > 0){
				mem.putInt(cur_fqe+4,cur_fqe_len);
				if (opt_tracemem){
					trace_mem("FQE UPDT",cur_fqe,cur_fqe_len,next_fqe);
				}
			} else {
				if (opt_tracemem){
					trace_mem("FQE DEL ",cur_fqe,req_len,next_fqe);
				}
				mem.putInt(prev_fqe,next_fqe);
			}
			reg.putInt(r15,0);
			return;
		} else {
			prev_fqe = cur_fqe;
			cur_fqe = mem.getInt(cur_fqe);
		}
	}
	if ((req_opt & opt_getmain_cond) != 0){
	   reg.putInt(r15,4);
	} else {
		set_psw_check(psw_pic_no_mem);
	}
}
private void svc_freemain(){
	/*
	 * Input
	 *   1.  r0 = addr to return to free queue
	 *   2.  r1 = length to return
	 *   
	 * Output:    
	 *   1.  set r15 to 0 of ok, else abort
	 */
	req_addr = reg.getInt(r0) & psw_amode31;
	req_len = reg.getInt(r1);	
	req_len = (req_len + 7)/8*8; // round to 8
	if (req_addr < dsa24_end){
		if (req_len <= 0
			|| (req_addr & 0x7) != 0
			|| req_addr < dsa24_start
			|| req_addr + req_len  > dsa24_end
			){
			set_psw_check(psw_pic_fm_err);
			return;
		}
		cur_fqe = mem.getInt(cvt_fqe24);
		prev_fqe = cvt_fqe24;
	} else {
		if (req_len <= 0
			|| (req_addr & 0x7) != 0
			|| req_addr < dsa31_start
			|| req_addr + req_len  > dsa31_end
			){
			set_psw_check(psw_pic_fm_err);
			return;
		}
		cur_fqe = mem.getInt(cvt_fqe31);
		prev_fqe = cvt_fqe31;
	}
	while (cur_fqe > 0){
		next_fqe    = mem.getInt(cur_fqe);
		cur_fqe_len = mem.getInt(cur_fqe+4);
        if (!check_fqe_ok()){
        	return;
        }
		if (req_addr < cur_fqe){
			// insert after prior fqe or cvt
			if (opt_tracemem){
                trace_mem("FREEMAIN",req_addr,req_len,0);
			}
			if (req_addr + req_len == cur_fqe){
				// merge insert with cur_fqe
				mem.putInt(req_addr,next_fqe);
				mem.putInt(req_addr+4,req_len + cur_fqe);
				if (opt_tracemem){
					trace_mem("FQE IMRG",req_addr,req_len+cur_fqe,next_fqe);
				}
			} else {
				mem.putInt(req_addr,cur_fqe);
				mem.putInt(req_addr+4,req_len);
				if (opt_tracemem){
					trace_mem("FQE INST",req_addr,req_len,cur_fqe);
				}
			}
			mem.putInt(prev_fqe,req_addr);			
			reg.putInt(r15,0);
			return;
		} else if (req_addr == cur_fqe + cur_fqe_len){
			// append to current fqe
			if (opt_tracemem){
                trace_mem("FREEMAIN",req_addr,req_len,0);
			}
			cur_fqe_len = cur_fqe_len + req_len;
            if (cur_fqe + cur_fqe_len == next_fqe){
            	// merge cur and next fqe's
            	next_fqe_len = mem.getInt(next_fqe+4);
            	next_fqe = mem.getInt(next_fqe);
            	mem.putInt(cur_fqe,next_fqe);
            	mem.putInt(cur_fqe+4,cur_fqe_len+next_fqe_len);
    			if (opt_tracemem){
                    trace_mem("FQE MRGE",cur_fqe,req_len,next_fqe);
    			}
            } else if (next_fqe > 0 
            		   && cur_fqe + cur_fqe_len > next_fqe){
    			set_psw_check(psw_pic_bad_mem);
    			return;
            } else {
            	mem.putInt(cur_fqe+4,cur_fqe_len);
            }
			reg.putInt(r15,0);
			return;
		} else if (req_addr > cur_fqe + cur_fqe_len){
			prev_fqe = cur_fqe;
			cur_fqe = next_fqe;
		} else {
			// abort due to memory corruption 
			set_psw_check(psw_pic_bad_mem); 
			return;
		}
	}
	// insert after last fqe or cvt
	if (opt_tracemem){
        trace_mem("FREEMAIN",req_addr,req_len,0);
	}
	mem.putInt(req_addr,0);
	mem.putInt(req_addr+4,req_len);
	mem.putInt(prev_fqe,req_addr);
	if (opt_tracemem){
		trace_mem("FQE ADD ",req_addr,req_len,0);
	}
}
private boolean check_fqe_ok(){
	/*
	 * trace fqe if option tracemem on and
	 * verify address and length ok
	 */
	if (opt_tracemem){
		trace_mem("FQE     ",cur_fqe,cur_fqe_len,next_fqe);
	}
	if (cur_fqe <= prev_fqe 
			|| (cur_fqe & 0x7) != 0
			|| cur_fqe_len <= 0
			|| (cur_fqe_len & 0x7) != 0){
		set_psw_check(psw_pic_bad_mem);
		return false;
	}
	return true;
}
private void trace_mem(String mem_type,int mem_addr,int mem_len,int mem_nxt){
	put_log("TRACE MEMORY " + mem_type 
			+ " LOC=" + get_hex(mem_addr,8)
			+ " LEN=" + get_hex(mem_len,8)
			+ " NXT=" + get_hex(mem_nxt,8)
			);
}
private void svc_time(){
	/*
	 * return time and date in requested format
	 * See TIME.MAC for additional information.
	 *   R0 LH = date time
	 *   R0 LL = time type
	 *   R1    = storage address for MIC,STCK,STCKE
	 *   
	 * Notes:
	 *   1.  When option NOTIMING is specified,
	 *       the time and date are fixed at
	 *         TOD  = 22:33:44:567 milliseconds
	 *         DATE = 2005.002 (Jan 2, 2005)
	 *       This is used for regression testing date
	 *       and time functions by comparing expected
	 *       time and date output.
	 */
	int date_type = reg.getShort(r0);
	int time_type = reg.getShort(r0+2);
	int time_addr = reg.getInt(r1) & psw_amode;
	if (opt_timing){
	    cur_date = new Date();
	}
	tod_mil  = cur_date.getTime();
    switch (time_type){
	case 0: // dec
        reg.putInt(r0,Integer.valueOf(cur_tod_hhmmss00.format(cur_date),16)
        		+(Integer.valueOf(cur_date_ms.format(cur_date),16) >> 4));
        reg.putInt(r1,get_ccyydddf());
        reg.putInt(r15,0);
        return;
	case 1: // bin - 0.01 sec = (milliseconds/10)
		reg.putInt(r0,(int)((tod_mil-tod_start_day)/10));
        reg.putInt(r1,get_ccyydddf());
        reg.putInt(r15,0);
        return;
	case 2: // tu - 26.04166 mic (milliseconds * 1000 / 26.04166)
		reg.putInt(r0,(int)((tod_mil-tod_start_day)*1000*100000/2604166));
        reg.putInt(r1,get_ccyydddf());
        reg.putInt(r15,0);
        return;
	case 3: // mic - double word microseconds 
		mem.putLong(time_addr,(tod_mil-tod_start_day)*1000);
        reg.putInt(r1,get_ccyydddf());
        reg.putInt(r15,0);
        return;
	case 4: // stck - double word clock (bit 51 = microseconds)
		mem.putLong(time_addr,((tod_mil-tod_start_day))*1000 << (63-51));
        reg.putInt(r1,get_ccyydddf());
        reg.putInt(r15,0);
        return;
	case 10: // dec
        mem.putInt(time_addr,Integer.valueOf(cur_tod_hhmmss00.format(cur_date),16)
        		+(Integer.valueOf(cur_date_ms.format(cur_date),16) >> 4));
	    break;
	case 11: // bin - 0.01 sec = (mic/10000)
		mem.putInt(time_addr,(int)((tod_mil-tod_start_day)/10));
		break;
	case 13: // mic - double word microseconds 
		mem.putLong(time_addr,(tod_mil-tod_start_day)*1000);
		break;
	case 14: // stck - double word clock (bit 51 = microseconds)
		mem.putLong(time_addr,((tod_mil-tod_start_day)*1000) << (63-51));
		break;
	case 15: // estck - double word clock (bit 51 = microseconds)
		mem.putLong(time_addr,((tod_mil-tod_start_day)*1000) << (63-59));
		mem.putLong(time_addr+8,0);
        reg.putInt(r15,0);
        return;
	default:
		reg.putInt(r15,4);
	    return;
    }
    switch (date_type){
    case 1: // 8(4,R1) = 0YYYYDDD
        mem.putInt(time_addr+8,Integer.valueOf(cur_date_yyyyddd.format(cur_date),16));
        break;
    case 2: // 8(4,R1) = MMDDYYYY
        mem.putInt(time_addr+8,Integer.valueOf(cur_date_MMddyyyy.format(cur_date),16));
        break;
    case 3: // 8(4,R1) = DDMMYYYY
        mem.putInt(time_addr+8,Integer.valueOf(cur_date_ddMMyyyy.format(cur_date),16));
        break;
    case 4: // 8(4,R1) = YYYYMMDD
        mem.putInt(time_addr+8,Integer.valueOf(cur_date_yyyyMMdd.format(cur_date),16));
        break;
    default:
		reg.putInt(r15,4);
        return;
    }
    reg.putInt(r15,0);
}
private int get_ccyydddf(){
	/* 
	 * return ccyydddf for r1 linkage=svc calls
	 */
    int cc = Integer.valueOf(cur_date_yyyy.format(cur_date))/100-19;
    return (cc << 24) 
           | ((Integer.valueOf(cur_date_yyddd.format(cur_date),16)) << 4)  
           | 0xf;
}
private void svc_abend(int pic){
	/*
	 * dump registers and current task and abort
	 */
	dump_regs(-1);
	int dump_loc = psw_loc - psw_ins_len;
	if (pic == 0x0c1 || psw_loc < 0){
		dump_loc = psw_loc;
	}
	abort_error(11,"program abend "
			       + get_hex(pic,3) 
				   + " INS=" + get_hex(dump_loc,8) 
				   + " " + bytes_to_hex(mem_byte,dump_loc,psw_ins_len,0));
}
private void svc_open(){
	/*
	 * open DCB file for sequential or random I/O
	 * Notes:
	 *   1.  R1 = DCB
	 *   2.  R0 = OPEN OPTION
	 *          x'40' - input only  - dcb_oflgs_gm
	 *          x'20' - output only - dcb_oflgs_pm
	 *          x'60' - update      - dcb_oflgs_rw
	 *   3.  DDNAME points to environment variable
	 *       with file path and name.
	 *   4.  See DCBD macro for DCB fields and
	 *       see DCB macro for generation of DCB
	 *   5.  TIOT table with unique entry for
	 *       each DDNAME holds open files.
	 *   6.  Take synad exit if defined else
	 *       issue error message and abort.
	 *  
	 */
	cur_dcb_addr   = reg.getInt(r1) & psw_amode;
	get_cur_tiot_index();
	if (cur_tiot_index != -1){
		cur_dcb_oflgs = mem.get(cur_dcb_addr+dcb_oflgs) & 0xff;
		if ((cur_dcb_oflgs & dcb_oflgs_open) != 0){
			dcb_synad_error(22,"file already open");
			return;
		}
	    cur_dcb_file_name = get_ddname_file_name(tiot_ddnam[cur_tiot_index]);
        cur_dcb_macrf = mem.getShort(cur_dcb_addr + dcb_macrf) & 0xffff;
	    tiot_vrec_blksi[cur_tiot_index] = 0;
		switch (cur_dcb_oflgs){
		case 0x20: // write - dcb_oflgs_pm (note NIO does not support write only)
			try {
	             tiot_file[cur_tiot_index] = new RandomAccessFile(cur_dcb_file_name,"rw");
	             if (cur_dcb_macrf == dcb_macrf_pm){
	            	 tiot_file[cur_tiot_index].setLength(0);
	            	 tiot_eof_rba[cur_tiot_index] = 0;
	             }
	             tiot_eof_rba[cur_tiot_index] = tiot_file[cur_tiot_index].length();
			} catch (Exception e){
				dcb_synad_error(23,"i/o error on open - " + e.toString());
				return;
			}
			break;
	    case 0x40: // read only dcb_oflgs_gm
		    try {
                tiot_file[cur_tiot_index] = new RandomAccessFile(cur_dcb_file_name,"r");
                tiot_eof_rba[cur_tiot_index] = tiot_file[cur_tiot_index].length();
		    } catch (Exception e){
			    dcb_synad_error(23,"i/o error on open - " + e.toString());
			    return;
		    }
		    break;
		case 0x60: // update - dcb_oflgs_rw
			try {
	             tiot_file[cur_tiot_index] = new RandomAccessFile(cur_dcb_file_name,"rw");
	             tiot_eof_rba[cur_tiot_index] = tiot_file[cur_tiot_index].length();
			} catch (Exception e){
				dcb_synad_error(23,"i/o error on open - " + e.toString());
				return;
			}
			break;
	    default:
		    dcb_synad_error(24,"invalid open type - " + get_hex(cur_dcb_oflgs,8));
	        return;
		}
	}
	mem.put(cur_dcb_addr + dcb_oflgs,(byte)(cur_dcb_oflgs | dcb_oflgs_open));
}
private void svc_close(){
	/*
	 * close file if open else synad error
	 */
	cur_dcb_addr   = reg.getInt(r1) & psw_amode;
	get_cur_tiot_index();
	if (cur_tiot_index != -1){
		cur_dcb_oflgs = mem.get(cur_dcb_addr+dcb_oflgs) & 0xff;
		if ((cur_dcb_oflgs & dcb_oflgs_open) == 0){
			dcb_synad_error(25,"file already closed");
			return;
		}
	    try {
            if (tiot_vrec_blksi[cur_tiot_index] > 0){
            	tiot_file[cur_tiot_index].seek(tiot_cur_rba[cur_tiot_index]);
            	tiot_file[cur_tiot_index].writeInt((tiot_vrec_blksi[cur_tiot_index]+4) << 16);
            }
            tiot_file[cur_tiot_index].close();
            tiot_ddnam[cur_tiot_index] = null;
            tiot_dcb_addr[cur_tiot_index] = 0;
	    } catch (Exception e){
		    dcb_synad_error(26,"i/o error on close - " + e.toString());
		    return;
	    }
	} else {
		dcb_synad_error(25,"file already closed");
		return;
	}
}
private void svc_get_move(){
	/*
	 * get next record into area from dcb gm file
	 */
	cur_dcb_addr   = reg.getInt(r1);
	get_cur_tiot_index();
	if (cur_tiot_index != -1){
		if (tiot_cur_rba[cur_tiot_index] >= tiot_eof_rba[cur_tiot_index]){
			dcb_eodad_exit();
			return;
		}
		cur_dcb_recfm = mem.get(cur_dcb_addr+dcb_recfm) & 0xff;
		cur_dcb_area  = reg.getInt(r0) & psw_amode;
		cur_dcb_lrecl = mem.getShort(cur_dcb_addr + dcb_lrecl) & 0xffff;
		cur_dcb_blksi = mem.getShort(cur_dcb_addr + dcb_blksi) & 0xffff;
		switch (cur_dcb_recfm){
		case 0x40: // variable 
			try {
				if (cur_dcb_lrecl == 0){
					cur_dcb_lrecl = cur_dcb_blksi; // use blksi if no lrecl
				}
                cur_vrec_lrecl = tiot_file[cur_tiot_index].readInt();
                mem.putInt(cur_dcb_area,cur_vrec_lrecl);
                cur_vrec_lrecl = cur_vrec_lrecl >>> 16;
                if (cur_vrec_lrecl <= 5 || cur_vrec_lrecl > cur_dcb_lrecl){
                	dcb_synad_error(28,"invalid variable record length - " + cur_vrec_lrecl);
                	return;
            	}
                tiot_file[cur_tiot_index].read(mem_byte,cur_dcb_area + 4,cur_vrec_lrecl - 4);
                tiot_cur_rba[cur_tiot_index] = tiot_file[cur_tiot_index].getFilePointer();
	        } catch (Exception e){
		        dcb_synad_error(29,"i/o error on get move variable - " + e.toString());
		        return;
	        }
	        break;
		case 0x50: // variable blocked 
			try {
				if (cur_dcb_lrecl == 0){
					cur_dcb_lrecl = cur_dcb_blksi - 4; // use blksi if no lrecl
				}
				if (tiot_vrec_blksi[cur_tiot_index] == 0){
					tiot_vrec_blksi[cur_tiot_index] = (tiot_file[cur_tiot_index].readInt() >>> 16)-4;
                    if (tiot_vrec_blksi[cur_tiot_index] < 5 || tiot_vrec_blksi[cur_tiot_index] > cur_dcb_blksi-4){
                    	dcb_synad_error(30,"invalid variable block size - " + tiot_vrec_blksi[cur_tiot_index]);
                    	return;
                    }
				}
                cur_vrec_lrecl = tiot_file[cur_tiot_index].readInt();
                mem.putInt(cur_dcb_area,cur_vrec_lrecl);
                cur_vrec_lrecl = cur_vrec_lrecl >>> 16;
                if (cur_vrec_lrecl <= 5 || cur_vrec_lrecl > cur_dcb_lrecl){
                	dcb_synad_error(28,"invalid variable record length - " + cur_vrec_lrecl);
                	return;
                }
                tiot_file[cur_tiot_index].read(mem_byte,cur_dcb_area + 4,cur_vrec_lrecl - 4);
                tiot_cur_rba[cur_tiot_index] = tiot_file[cur_tiot_index].getFilePointer();
                tiot_vrec_blksi[cur_tiot_index] = tiot_vrec_blksi[cur_tiot_index] - cur_vrec_lrecl;
                if (tiot_vrec_blksi[cur_tiot_index] < 0){
                	dcb_synad_error(31,"invalid variable block at rba " + get_long_hex(tiot_cur_rba[cur_tiot_index]));
                	return;
                }
	        } catch (Exception e){
		        dcb_synad_error(29,"i/o error on get move variable - " + e.toString());
		        return;
	        }
	        break;
		case 0x60: // variable from ascii text
			try {
				if (cur_dcb_lrecl == 0){
					cur_dcb_lrecl = cur_dcb_blksi; // use blksi if no lrecl
				}
                cur_rec_text = tiot_file[cur_tiot_index].readLine();
                cur_rec_len  = cur_rec_text.length();
                tiot_cur_rba[cur_tiot_index] = tiot_file[cur_tiot_index].getFilePointer();
                if (cur_rec_len < 1 || cur_rec_len > cur_dcb_lrecl - 4){
                	dcb_synad_error(46,"variable record too long");
                	return;
                }
                mem.putInt(cur_dcb_area,(cur_rec_len+4) << 16);
                int index = 0;
                while (index < cur_rec_len){
                	mem.put(cur_dcb_area + 4 + index,ascii_to_ebcdic[cur_rec_text.charAt(index)]);
                	index++;
                }
	        } catch (Exception e){
		        dcb_synad_error(47,"i/o error on get move variable from ascii - " + e.toString());
		        return;
	        }
	        break;
		case 0x80: // fixed - read lrecl bytes into area
		case 0x90: // fixed blocked
			try {
				if (cur_dcb_lrecl == 0){
					cur_dcb_lrecl = cur_dcb_blksi; // use blksi if no lrecl
				}
                tiot_file[cur_tiot_index].read(mem_byte,cur_dcb_area,cur_dcb_lrecl);
                tiot_cur_rba[cur_tiot_index] = tiot_cur_rba[cur_tiot_index]+cur_dcb_lrecl;
	        } catch (Exception e){
		        dcb_synad_error(27,"i/o error on get move fixed - " + e.toString());
		        return;
	        }
	        break;
		case 0xa0: // fixed from ascii text
			try {
				if (cur_dcb_lrecl == 0){
					cur_dcb_lrecl = cur_dcb_blksi; // use blksi if no lrecl
				}
                cur_rec_text = tiot_file[cur_tiot_index].readLine();
                cur_rec_len  = cur_rec_text.length();
                tiot_cur_rba[cur_tiot_index] = tiot_file[cur_tiot_index].getFilePointer();
                if (cur_rec_len <= cur_dcb_lrecl){
                	int index = 0;
                	while (index < cur_rec_len){
                		mem.put(cur_dcb_area + index,ascii_to_ebcdic[cur_rec_text.charAt(index)]);
                		index++;
                	}
                	while (index < cur_dcb_lrecl){
                		mem.put(cur_dcb_area + index,(byte)0x40);
                		index++;
                	}
                }
	        } catch (Exception e){
		        dcb_synad_error(44,"i/o error on get move fixed from ascii - " + e.toString());
		        return;
	        }
	        break;
	    default:
	        dcb_synad_error(31,"invalid dcb record format for get move - " + get_hex(cur_dcb_recfm,2));
            return;
		}
	} else {
		dcb_synad_error(30,"file not found");
		return;
	}
}
private void svc_put_move(){
	/*
	 * put next record from area to dcb pm file
	 */
	cur_dcb_addr   = reg.getInt(r1) & psw_amode;
	get_cur_tiot_index();
	if (cur_tiot_index != -1){
		cur_dcb_oflgs = mem.get(cur_dcb_addr + dcb_oflgs) & 0xff;
		if ((cur_dcb_oflgs & dcb_oflgs_pm) == 0){
			dcb_synad_error(33,"file not open for output");
			return;
		}
		cur_dcb_recfm = mem.get(cur_dcb_addr+dcb_recfm) & 0xff;
		cur_dcb_area  = reg.getInt(r0) & psw_amode;
		cur_dcb_lrecl = mem.getShort(cur_dcb_addr + dcb_lrecl) & 0xffff;
		cur_dcb_blksi = mem.getShort(cur_dcb_addr + dcb_blksi) & 0xffff;
		switch (cur_dcb_recfm){
		case 0x80: // fixed - read lrecl bytes into area
		case 0x90: // fixed blocked
			try {
				if (cur_dcb_lrecl == 0){
					cur_dcb_lrecl = cur_dcb_blksi;
				}
                tiot_file[cur_tiot_index].write(mem_byte,cur_dcb_area,cur_dcb_lrecl);
                tiot_cur_rba[cur_tiot_index] = tiot_cur_rba[cur_tiot_index]+cur_dcb_lrecl;
	        } catch (Exception e){
		        dcb_synad_error(34,"i/o error on put move fixed - " + e.toString());
		        return;
	        }
	        break;
		case 0x40: // variable 
			try {
				if (cur_dcb_lrecl == 0){
					cur_dcb_lrecl = cur_dcb_blksi;
				}
                cur_vrec_lrecl = mem.getInt(cur_dcb_area) >>> 16;
                if (cur_vrec_lrecl <= 5 || cur_vrec_lrecl > cur_dcb_lrecl){
    	            dcb_synad_error(35,"invalid variable record length - " + cur_vrec_lrecl);
                    return;
                }
                tiot_file[cur_tiot_index].write(mem_byte,cur_dcb_area,cur_vrec_lrecl);
                tiot_cur_rba[cur_tiot_index] = tiot_cur_rba[cur_tiot_index]+cur_vrec_lrecl;
	        } catch (Exception e){
		        dcb_synad_error(36,"i/o error on put move variable - " + e.toString());
		        return;
	        }
	        break;
		case 0x50: // variable blocked 
			try {
				if (cur_dcb_lrecl == 0){
					cur_dcb_lrecl = cur_dcb_blksi - 4;
				}
				if (tiot_vrec_blksi[cur_tiot_index] == 0){
					tiot_cur_rba[cur_tiot_index] = tiot_file[cur_tiot_index].getFilePointer();
					tiot_file[cur_tiot_index].writeInt(-1); // place holder for vb LLZZ
				}
				cur_vrec_lrecl = mem.getInt(cur_dcb_area) >>> 16;
                if (cur_vrec_lrecl < 5 || cur_vrec_lrecl > cur_dcb_lrecl){
                   	dcb_synad_error(37,"invalid variable record size - " + cur_vrec_lrecl);
                    return;
                }
                tiot_vrec_blksi[cur_tiot_index] = tiot_vrec_blksi[cur_tiot_index] + cur_vrec_lrecl;
                if (tiot_vrec_blksi[cur_tiot_index] > cur_dcb_blksi - 4){
                	tiot_vrec_blksi[cur_tiot_index] = tiot_vrec_blksi[cur_tiot_index] - cur_vrec_lrecl;
                	tiot_file[cur_tiot_index].seek(tiot_cur_rba[cur_tiot_index]);
                	tiot_file[cur_tiot_index].writeInt((tiot_vrec_blksi[cur_tiot_index]+4) << 16);
                	tiot_file[cur_tiot_index].seek(tiot_cur_rba[cur_tiot_index] + tiot_vrec_blksi[cur_tiot_index]+4);
                	tiot_cur_rba[cur_tiot_index] = tiot_file[cur_tiot_index].getFilePointer();
					tiot_file[cur_tiot_index].writeInt(-1); // place holder for vb LLZZ
                	tiot_vrec_blksi[cur_tiot_index] = cur_vrec_lrecl;
                }
                tiot_file[cur_tiot_index].write(mem_byte,cur_dcb_area,cur_vrec_lrecl);
	        } catch (Exception e){
		        dcb_synad_error(40,"i/o error on put move variable - " + e.toString());
		        return;
	        }
	        break;
		case 0x60: // variable to ascii text
			try {
				if (cur_dcb_lrecl == 0){
					cur_dcb_lrecl = cur_dcb_blksi;
				}
                cur_rec_len = (mem.getInt(cur_dcb_area) >> 16)-4;
                if (cur_rec_len < 1 || cur_rec_len > (cur_dcb_lrecl - 4)){
                	dcb_synad_error(48,"variable record too long - " + cur_rec_len);
                	return;
                }
                cur_rec_text = get_ascii_string(cur_dcb_area+4,cur_rec_len);
                tiot_file[cur_tiot_index].writeBytes(cur_rec_text + '\r' + '\n');
                tiot_cur_rba[cur_tiot_index] = tiot_file[cur_tiot_index].getFilePointer();
	        } catch (Exception e){
		        dcb_synad_error(47,"i/o error on get move variable from ascii - " + e.toString());
		        return;
	        }
	        break;
		case 0xa0: // fixed to ascii text
			try {
				if (cur_dcb_lrecl == 0){
					cur_dcb_lrecl = cur_dcb_blksi;
				}
                cur_rec_text = get_ascii_string(cur_dcb_area,cur_dcb_lrecl);
                tiot_file[cur_tiot_index].writeBytes(cur_rec_text + '\r' + '\n');
			} catch (Exception e){
		        dcb_synad_error(45,"i/o error on put move fixed to ascii - " + e.toString());
		        return;
			}
			break;
	    default:
	        dcb_synad_error(41,"invalid dcb record format for put move - " + get_hex(cur_dcb_recfm,2));
            return;
		}
	} else {
		dcb_synad_error(42,"file not found");
		return;
	}
}
private void svc_read(){
	/*
	 * read next record forward or backward
	 * into area from dcb macrf r/rw file
	 */
	cur_decb_addr = reg.getInt(r1) & psw_amode;
	cur_dcb_addr  = mem.getInt(cur_decb_addr + decb_dcb) & psw_amode;
	get_cur_tiot_index();
	if (cur_tiot_index != -1){
		if (tiot_cur_rba[cur_tiot_index] >= tiot_eof_rba[cur_tiot_index]){
            mem.put(cur_decb_addr + decb_ecb,(byte) 0x42); // post ecb for check eof exit
			return;
		}
		cur_dcb_area  = mem.getInt(cur_decb_addr + decb_area) & psw_amode;
		cur_dcb_recfm = mem.get(cur_dcb_addr+dcb_recfm) & 0xff;
		cur_dcb_lrecl = mem.getShort(cur_dcb_addr + dcb_lrecl) & 0xffff;
		cur_dcb_blksi = mem.getShort(cur_dcb_addr + dcb_blksi) & 0xffff;
		try {
	        mem.put(cur_decb_addr + decb_ecb,(byte) 0x80); // post ecb waiting for post
			if (cur_dcb_lrecl == 0){
				cur_dcb_lrecl = cur_dcb_blksi; // use blksi if no lrecl
			}
            tiot_file[cur_tiot_index].read(mem_byte,cur_dcb_area,cur_dcb_lrecl);
            tiot_cur_rba[cur_tiot_index] = tiot_cur_rba[cur_tiot_index]+cur_dcb_lrecl;
        } catch (Exception e){
            mem.put(cur_decb_addr + decb_ecb,(byte) 0x41); // post ecb for check synad exit
	        return;
        }
        mem.put(cur_decb_addr + decb_ecb,(byte) 0x40); // post ecb normal exit
	} else {
		dcb_synad_error(50,"file not found");
		return;
	}
}
private void svc_write(){
	/*
	 * write next record forward or backward
	 * into area from dcb macrf r/rw file
	 */
	cur_decb_addr = reg.getInt(r1) & psw_amode;
	cur_dcb_addr  = mem.getInt(cur_decb_addr + decb_dcb) & psw_amode;
	get_cur_tiot_index();
	if (cur_tiot_index != -1){
		cur_dcb_area  = mem.getInt(cur_decb_addr + decb_area) & psw_amode;
		cur_dcb_recfm = mem.get(cur_dcb_addr+dcb_recfm) & 0xff;
		cur_dcb_lrecl = mem.getShort(cur_dcb_addr + dcb_lrecl) & 0xffff;
		cur_dcb_blksi = mem.getShort(cur_dcb_addr + dcb_blksi) & 0xffff;
        mem.put(cur_decb_addr + decb_ecb,(byte) 0x80); // post ecb waiting for post
		try {
			if (cur_dcb_lrecl == 0){
				cur_dcb_lrecl = cur_dcb_blksi; // use blksi if no lrecl
			}
            tiot_file[cur_tiot_index].write(mem_byte,cur_dcb_area,cur_dcb_lrecl);
            tiot_cur_rba[cur_tiot_index] = tiot_cur_rba[cur_tiot_index]+cur_dcb_lrecl;
        } catch (Exception e){
            mem.put(cur_decb_addr + decb_ecb,(byte) 0x41); // post ecb for check synad exit
	        return;
        }
        mem.put(cur_decb_addr + decb_ecb,(byte) 0x40); // post ecb normal exit
	} else {
		dcb_synad_error(52,"file not found");
		return;
	}
}
private void svc_check(){
	/*
	 * check decb ecb and process as follows:
	 *   ecb = x'40' exit normally
	 *   ecb = x'41' take synad error exit
	 *   ecb = x'42' take eodad exit
	 */
	cur_decb_addr = reg.getInt(r1) & psw_amode;
	cur_ecb = mem.get(cur_decb_addr + decb_ecb) & psw_amode;
	if (cur_ecb == 0x40){
		return;
	} else if (cur_ecb == 0x42){
		dcb_eodad_exit();
		return;
	} else {
		dcb_synad_error(mem.getInt(cur_decb_addr + decb_ecb),"I/O error on read/write");
	}
}
private void svc_point(){
	/*
	 * set dcb file pointer
	 *   r1 = address of dcb
	 *   r0 = 64 bit rba
	 */
	cur_dcb_addr  = reg.getInt(r1) & psw_amode;
	get_cur_tiot_index();
	if (cur_tiot_index != -1){
		try {
			cur_rba = reg.getLong(0);
            tiot_file[cur_tiot_index].seek(cur_rba);
            tiot_cur_rba[cur_tiot_index] = cur_rba;
        } catch (Exception e){
	        dcb_synad_error(53,"i/o error on point - " + e.toString());
	        return;
        }
	} else {
		dcb_synad_error(54,"file not found");
		return;
	}
}
private void dcb_synad_error(int error_num,String error_msg){
	/*
	 * take synad exit if defined else issue
	 * error message and abort
	 */
	cur_dcb_synad = mem.getInt(cur_dcb_addr + dcb_synad) & psw_amode;
	if (cur_dcb_synad == 0){
		String cur_ddnam = get_ascii_string(cur_dcb_addr + dcb_ddnam,8);
		String cur_file  = get_ddname_file_name(cur_ddnam);
		log_error(43,"I/O error for DCB=" + get_hex(cur_dcb_addr,8) 
				             + " DDNAME=" + cur_ddnam
				             + " FILE=" + cur_file);
		abort_error(error_num,error_msg);
	} else {
		mem.putInt(r15,error_num);
		set_psw_loc(cur_dcb_synad);
	}
}
private void dcb_eodad_exit(){
	/*
	 * take eodad exit if defined else issue
	 * error message and abort
	 */
	cur_dcb_eodad = mem.getInt(cur_dcb_addr + dcb_eodad) & psw_amode;
	if (cur_dcb_eodad == 0){
		abort_error(31,"read beyond end of file on " + tiot_ddnam[cur_tiot_index]);
	} else {
		set_psw_loc(cur_dcb_eodad);
	}
}
private void get_cur_tiot_index(){
	/*
	 * 1.  Using cur_dcb addr from R1 or DECB get
	 *     cur_tiot index from DCBIOBAD (x'1C').
	 * 2.  reduce index in DCBIOBAD by 1.
	 * 3.  If index not -1, verify tiot_dcb_addr
	 * 4.  If index = -1, add entry for DCBDDNAM.
	 */
	cur_tiot_index = mem.getInt(cur_dcb_addr + dcb_iobad) - 1;
    if (cur_tiot_index  != -1){
    	if (cur_dcb_addr == tiot_dcb_addr[cur_tiot_index]){
    	    return;
    	} else {
    		abort_error(20,"dcb tiot index invalid DCB=" + get_hex(cur_dcb_addr,8));
    	}
    } else {
    	cur_dcb_ddnam = get_ascii_string(cur_dcb_addr + dcb_ddnam,8);
        cur_tiot_index = 0;
        while (cur_tiot_index < tot_tiot_files){
        	if (tiot_ddnam[cur_tiot_index] == null){
        		mem.putInt(cur_dcb_addr + dcb_iobad,cur_tiot_index + 1);
        		tiot_ddnam[cur_tiot_index] = cur_dcb_ddnam;
        		tiot_dcb_addr[cur_tiot_index] = cur_dcb_addr;
        		return;
        	}
        	cur_tiot_index++;
        }
        if (tot_tiot_files < max_tiot_files){
        	tot_tiot_files++;
    		mem.putInt(cur_dcb_addr + dcb_iobad,cur_tiot_index + 1);
    		tiot_ddnam[cur_tiot_index] = cur_dcb_ddnam;
    		tiot_dcb_addr[cur_tiot_index] = cur_dcb_addr;
    		return;
        } else {
        	abort_error(21,"maximum tiot files open exceeded");
        }
    }
}
private String get_ascii_string(int mem_addr,int mem_len){
	/*
	 * get ascii string with no trailing spaces from
	 * memory address and length
	 * Notes:
	 *   1.  Translates from EBCDIC to ASCII
	 */
	String text = "";
	int index = 0;
	while (index < mem_len){
		text = text + ebcdic_table.charAt(mem.get(mem_addr + index) & 0xff);
		index++;
	}
	return text.trim();
}
private void svc_wto(){
	/*
	 * display write to operator msg on
	 * console and log if active
	 * Input regs:
	 *   r1 = A(WTO FIELD)
	 * WTO FIELD
	 *   0 2 length 
	 *   2 2 mcs flags
	 *   4 (length-4) msg in EBCDIC
	 */
	int wto_fld = reg.getInt(r1) & psw_amode;
	int wto_len = mem.getShort(wto_fld);
	String wto_msg = "";
	int index = 4;
	while (index < wto_len){
		int work_int = mem.get(wto_fld + index) & 0xff;
		wto_msg = wto_msg + ebcdic_table.charAt(work_int);
	    index++;
	}
	put_log("WTO MSG = " + wto_msg);
}
private void svc_espie(){
	/*
	 * set/reset program interruption exit
	 */
	espie_pie  = reg.getInt(r0); // save psw_pic bit mask
	espie_exit = reg.getInt(r1) & psw_amode; // save exit psw addr
}
private void svc_estae(){
	/*
	 * set/reset task abend exit
	 */
	estae_exit = reg.getInt(r1) & psw_amode;
}
private void exec_390(){
	/*
	 * execute 390 code at psw_addr in mem[]
	 */
	psw_check = false;
	while (!psw_check){
		tot_ins++;
	    if (opt_test){
	    	process_test_cmd();
	    }
		opcode1 = mem_byte[psw_loc] & 0xff;
		opcode2 = -1;
		psw_check = true;
		psw_retry = false;
		psw_pic   = psw_pic_oper;
        if  (opt_regs){
    		dump_regs(-1);
        }
		switch (opcode1){
		 case 0x01:  
		     opcode2 = mem_byte[psw_loc+opcode2_offset_e] & 0xff;
		     switch (opcode2){
		     case 0x01:  // 10 "0101" "PR" "E"
		         psw_check = false;
		     	 ins_setup_e();
		     	 pop_pc_stack();
		         break;
		     case 0x02:  // 20 "0102" "UPT" "E"
		         ins_setup_e();
		         break;
		     case 0x07:  // 30 "0107" "SCKPF" "E"
		         ins_setup_e();
		         break;
		     case 0x0B:  // 40 "010B" "TAM" "E"
		         ins_setup_e();
		         break;
		     case 0x0C:  // 50 "010C" "SAM24" "E"
		     	 psw_check = false;
		         ins_setup_e();
		         set_psw_amode(psw_amode24_bit);
		         break;
		     case 0x0D:  // 60 "010D" "SAM31" "E"
		     	 psw_check = false;
		         ins_setup_e();
		         set_psw_amode(psw_amode31_bit);
		         break;
		     case 0x0E:  // 70 "010E" "SAM64" "E"
		         ins_setup_e();
		         break;
		     case 0xFF:  // 80 "01FF" "TRAP2" "E"
		         ins_setup_e();
		         break;
		     }
		     break;
		 case 0x04:  // 90 "04" "SPM" "RR"
	     	 psw_check = false;
	         ins_setup_rr();
	         rv1 = reg.getInt(rf1+4) & 0x3f;
             psw_pgm_mask = rv1 & 0xf;
             psw_cc       = psw_cc_mask[rv1 >> 4];
		     break;
		 case 0x05:  // 100 "05" "BALR" "RR"
			 psw_check = false;
		     ins_setup_rr();
			 reg.putInt(rf1+4,psw_loc | psw_amode_bit);
			 if (rf2 != 0){
			    set_psw_loc(reg.getInt(rf2+4));
			 }
		     break;
		 case 0x06:  // 110 "06" "BCTR" "RR"
		 	 psw_check = false;
		     ins_setup_rr();
		     rv1 = reg.getInt(rf1+4)-1;
			 reg.putInt(rf1+4,rv1);
			 if (rf2 != 0 && rv1 != 0){	
				set_psw_loc(reg.getInt(rf2+4));
			 }
		     break;
		 case 0x07:  // 120 "07" "BCR" "RR"
			 psw_check = false;
		     ins_setup_rr();
			 if ((psw_cc & mf1) > 0
				  && (rf1 != 0)){
				set_psw_loc(reg.getInt(rf2+4));
			 }

		     break;
		 case 0x0A:  // 290 "0A" "SVC" "I"
			 psw_check = false;
		     ins_setup_i();
			 svc(if1);
		     break;
		 case 0x0B:  // 300 "0B" "BSM" "RR"
		 	 psw_check = false;
		     ins_setup_rr();
		     if (rf1 != 0){
		     	if (psw_amode == psw_amode31){
		     	    reg.putInt(rf1+4,reg.getInt(rf1+4) | psw_amode31_bit);
		     	} else {
			     	reg.putInt(rf1+4,reg.getInt(rf1+4) & psw_amode31);
		     	}
		     }
		     if (rf2 != 0){
		     	rv2 = reg.getInt(rf2+4);
		     	if ((rv2 & psw_amode31_bit) != 0){
		            set_psw_amode(psw_amode31_bit);
		     	} else {
		            set_psw_amode(psw_amode24_bit);
		     	}
		     	set_psw_loc(rv2);
		     }	     
		     break;
		 case 0x0C:  // 310 "0C" "BASSM" "RR"
			 psw_check = false;
		     ins_setup_rr();
			 reg.putInt(rf1+4,psw_loc | psw_amode_bit);
		     if (rf2 != 0){
		     	rv2 = reg.getInt(rf2+4);
		     	if ((rv2 & psw_amode31_bit) != 0){
		            set_psw_amode(psw_amode31_bit);
		     	} else {
		            set_psw_amode(psw_amode24_bit);
		     	}
		     	set_psw_loc(rv2);
		     }
		     break;
		 case 0x0D:  // 320 "0D" "BASR" "RR"
			 psw_check = false;
		     ins_setup_rr();
			 reg.putInt(rf1+4,psw_loc | psw_amode_bit);
			 if (rf2 != 0){
			    set_psw_loc(reg.getInt(rf2+4));
			 }
		     break;
		 case 0x0E:  // 330 "0E" "MVCL" "RR"
		 	 psw_check = false;
		     ins_setup_rr();
		     bd1_loc = reg.getInt(rf1+4) & psw_amode;
		     bd2_loc = reg.getInt(rf2+4) & psw_amode;
		     rflen1  = reg.getInt(rf1+12) & psw_amode24;
		     rflen2  = reg.getInt(rf2+12) & psw_amode24;
		     data_len = rflen1;
		     pad_len  = 0;
		     psw_cc = psw_cc0;
		     if (rflen1 < rflen2){
		        psw_cc = psw_cc1;
		     } else if (rflen1 > rflen2){
		     	psw_cc = psw_cc2;
		     	data_len = rflen2;
		     	pad_len  = rflen1 - rflen2;
			    fill_char = reg.get(rf2+12);
		     }
		     if (data_len > 0
		     	&& bd1_loc > bd2_loc
				&& bd1_loc < bd2_loc + data_len){
		     	psw_cc = psw_cc3;
		     	break;
		     }
		     bd1_end = bd1_loc + data_len;
		     while (bd1_loc < bd1_end){
	     		mem.put(bd1_loc,mem.get(bd2_loc));
	     		bd1_loc++;
	     		bd2_loc++;
	     	}
		    bd1_end = bd1_loc + pad_len;
		    while (bd1_loc < bd1_end){ 	
	     		mem.put(bd1_loc,fill_char);
	     		bd1_loc++;
	     	}
		    reg.putInt(rf1+4,(reg.getInt(rf1+4) & psw_amode_high_bits) | bd1_loc);
		    reg.putInt(rf1+12,reg.getInt(rf1+12) & psw_amode24_high_bits);
		    reg.putInt(rf2+4,(reg.getInt(rf2+4) & psw_amode_high_bits) | bd2_loc);
		    reg.putInt(rf2+12,(reg.getInt(rf2+12) & psw_amode24_high_bits) | (rflen2 - data_len));
		    break;
		 case 0x0F:  // 340 "0F" "CLCL" "RR"
		 	 psw_check = false;
		     ins_setup_rr();
		     fill_char = reg.get(rf2+12);
		     bd1_loc = reg.getInt(rf1+4) & psw_amode;
		     bd2_loc = reg.getInt(rf2+4) & psw_amode;
		     bd1_start = bd1_loc;
		     bd2_start = bd2_loc;
		     rflen1  = reg.getInt(rf1+12) & psw_amode24;
		     rflen2  = reg.getInt(rf2+12) & psw_amode24;
		     data_len = rflen1;
		     pad_len  = 0;
		     psw_cc = psw_cc0;
		     if (rflen1 < rflen2){
                data_len = rflen1;
                pad_len  = rflen2 - rflen1;            
		     } else if (rflen1 > rflen2){
		     	data_len = rflen2;
		     	pad_len  = rflen1 - rflen2;
		     }
		     bd1_end = bd1_loc + data_len;
		     while (bd1_loc < bd1_end){
	     		if (mem.get(bd1_loc) != mem.get(bd2_loc)){
             		if ((mem_byte[bd1_loc] &0xff) > (mem_byte[bd2_loc] & 0xff)){
	     				psw_cc = psw_cc_high;
	     			} else {
	     				psw_cc = psw_cc_low;
	     			}
                    bd1_end = bd1_loc;
	     		} else {
	     		    bd1_loc++;
	     		    bd2_loc++;
	     		}
	     	}
		    if (psw_cc == psw_cc_equal & pad_len > 0){
		       if  (rflen1 > rflen2){
			        bd1_end = bd1_loc + pad_len;
		            while (bd1_loc < bd1_end){ 	
	     		        if (mem.get(bd1_loc) != fill_char){
	     			       if ((mem.get(bd1_loc) & 0xff) > (fill_char & 0xff)){
	     				       psw_cc = psw_cc_high;
	     			       } else {
	     				       psw_cc = psw_cc_low;
	     			       }
	     			       bd1_end = bd1_loc;
	     		        } else {
	     		           bd1_loc++;
	     		        }
	     	        }
	           } else {
	           	    bd2_end = bd2_loc + pad_len;
	                while (bd2_loc < bd2_end){ 	
     		           if (mem.get(bd2_loc) != fill_char){
     			           if ((mem.get(bd2_loc) & 0xff) > (fill_char & 0xff)){
     				          psw_cc = psw_cc_high;
     			           } else {
     				          psw_cc = psw_cc_low;
     			           }
     			           bd2_end = bd2_loc;
     		           } else {
     		               bd2_loc++;
     		           }
	                }     	      
	           }
		    }
		    reg.putInt(rf1+4,(reg.getInt(rf1+4) & psw_amode_high_bits) | bd1_loc);
		    reg.putInt(rf1+12,(reg.getInt(rf1+12) & psw_amode24_high_bits) | (rflen1 - (bd1_loc - bd1_start)));
		    reg.putInt(rf2+4,(reg.getInt(rf2+4) & psw_amode_high_bits) | bd2_loc);
		    reg.putInt(rf2+12,(reg.getInt(rf2+12) & psw_amode24_high_bits) | (rflen2 - (bd2_loc - bd2_start)));
		    break;
		 case 0x10:  // 350 "10" "LPR" "RR"
		 	 psw_check = false;
		     ins_setup_rr();
		     rv1 = reg.getInt(rf2+4);
		     if (rv1 < 0){
		     	if  (rv1 == int_high_bit){
		     		psw_cc = psw_cc3;
		     		break;
		     	}
		     	rv1 = - rv1;
		     }
		     reg.putInt(rf1+4,rv1);
		     psw_cc = get_int_comp_cc(rv1,0);
		     break;
		 case 0x11:  // 360 "11" "LNR" "RR"
		 	 psw_check = false;
		     ins_setup_rr();
		     rv1 = reg.getInt(rf2+4);
		     if (rv1 > 0){
		     	rv1 = - rv1;
		     }
		     reg.putInt(rf1+4,rv1);
		     psw_cc = get_int_comp_cc(rv1,0);
		     break;
		 case 0x12:  // 370 "12" "LTR" "RR"
		 	 psw_check = false;
		     ins_setup_rr();
		     rv1 = reg.getInt(rf2+4);
		     reg.putInt(rf1+4,rv1);
		     psw_cc = get_int_comp_cc(rv1,0);
		     break;
		 case 0x13:  // 380 "13" "LCR" "RR"
		 	 psw_check = false;
		     ins_setup_rr();
		     rv1 = reg.getInt(rf2+4);
		     if  (rv1 == int_high_bit){
		     	psw_cc = psw_cc3;
		     	break;
		     }
             rv1 = - rv1;
		     reg.putInt(rf1+4,rv1);
		     psw_cc = get_int_comp_cc(rv1,0);
		     break;
		 case 0x14:  // 390 "14" "NR" "RR"
		 	 psw_check = false;
		     ins_setup_rr();
		     reg.putInt(rf1+4,reg.getInt(rf1+4) & reg.getInt(rf2+4));
		     break;
		 case 0x15:  // 400 "15" "CLR" "RR"
		 	 psw_check = false;
		     ins_setup_rr();
		     psw_cc = get_int_log_comp_cc(
		     		reg.getInt(rf1+4),
					reg.getInt(rf2+4));
		     break;
		 case 0x16:  // 410 "16" "OR" "RR"
		 	 psw_check = false;
		     ins_setup_rr();
		     reg.putInt(rf1+4,reg.getInt(rf1+4) | reg.getInt(rf2+4));
		     break;
		 case 0x17:  // 420 "17" "XR" "RR"
		 	 psw_check = false;
		     ins_setup_rr();
		     reg.putInt(rf1+4,reg.getInt(rf1+4) ^ reg.getInt(rf2+4));
		     break;
		 case 0x18:  // 430 "18" "LR" "RR"
			 psw_check = false;
		     ins_setup_rr();
			 reg.putInt(rf1+4,reg.getInt(rf2+4));
			 break;
		 case 0x19:  // 440 "19" "CR" "RR"
		     psw_check = false;
		     ins_setup_rr();
		     psw_cc = get_int_comp_cc(
		     		reg.getInt(rf1+4),
					reg.getInt(rf2+4));
		     break;
		 case 0x1A:  // 450 "1A" "AR" "RR"
		     psw_check = false;
		     ins_setup_rr();
		     rv1 = reg.getInt(rf1+4);
			 rv2 = reg.getInt(rf2+4);
		     rv3 = rv1 + rv2;
			 reg.putInt(rf1+4,rv3);
			 psw_cc = get_int_add_cc();
		     break;
		 case 0x1B:  // 460 "1B" "SR" "RR"
		     psw_check = false;
		     ins_setup_rr();
			 rv1 = reg.getInt(rf1+4);
			 rv2 = reg.getInt(rf2+4);
			 rv3 = rv1 - rv2;
			 reg.putInt(rf1+4,rv3);
			 psw_cc = get_int_sub_cc();
		     break;
		 case 0x1C:  // 470 "1C" "MR" "RR"
			 psw_check = false;
		     ins_setup_rr();
		     work_reg.putInt(0,reg.getInt(rf1+4));
		     work_reg.putInt(4,reg.getInt(rf1+12));
		     work_reg.putLong(0,work_reg.getLong(0) * reg.getInt(rf2+4));
		     reg.putInt(rf1+4,work_reg.getInt(0));
		     reg.putInt(rf1+12,work_reg.getInt(4));
		     break;
		 case 0x1D:  // 480 "1D" "DR" "RR"
			 psw_check = false;
		     ins_setup_rr();
		     work_reg.putInt(0,reg.getInt(rf1+4));
		     work_reg.putInt(4,reg.getInt(rf1+12));
		     rlv1 = work_reg.getLong(0);
		     rv2 = reg.getInt(rf2+4);
		     if (rv2 != 0){
		     	rv1  = (int) rlv1 / rv2;
		     } else {
		     	set_psw_check(psw_pic_fx_div);
		     	break;
		     }
		     rv1 = (int) rlv1/rv2;
		     rv2 = (int) rlv1 - rv1 * rv2;
		     reg.putInt(rf1+4,rv2);
		     reg.putInt(rf1+12,rv1);
		     break;
		 case 0x1E:  // 490 "1E" "ALR" "RR"
	     	 psw_check = false;
	         ins_setup_rr();
	         rlvw = ((long) reg.getInt(rf1+4) & long_low32_bits)
			      + ((long) reg.getInt(rf2+4) & long_low32_bits);
             rlv1  = rlvw & long_low32_bits;
	         reg.putInt(rf1+4,(int)rlv1);
			 psw_cc = get_log_add_cc(rlv1,rlvw - rlv1);
		     break;
		 case 0x1F:  // 500 "1F" "SLR" "RR"
	     	 psw_check = false;
	         ins_setup_rr();
	         rlvw = ((long) reg.getInt(rf1+4) & long_low32_bits)
			      - ((long) reg.getInt(rf2+4) & long_low32_bits);
             rlv1 = rlvw & long_low32_bits;
	         reg.putInt(rf1+4,(int)rlv1);
			 psw_cc = get_log_sub_cc(rlv1,rlvw - rlv1);
		     break;
		 case 0x20:  // 510 "20" "LPDR" "RR"
		 	 psw_check = false;
		     ins_setup_rr();
		     fp_rdv1 = Math.abs(fp_get_db_from_dh(fp_reg,rf2));
		 	 fp_put_db(rf1,fp_dh_type,fp_rdv1);
		     psw_cc = fp_get_dh_comp_cc(fp_rdv1,0);
		     break;
		 case 0x21:  // 520 "21" "LNDR" "RR"
		 	 psw_check = false;
		     ins_setup_rr();
		     fp_rdv1 = Math.abs(fp_get_db_from_dh(fp_reg,rf2));
		     if (fp_rdv1 != 0){
		     	fp_rdv1 = - fp_rdv1;
		     }
		 	 fp_put_db(rf1,fp_dh_type,fp_rdv1);
		     psw_cc = fp_get_dh_comp_cc(fp_rdv1,0);
		     break;
		 case 0x22:  // 530 "22" "LTDR" "RR"
		 	 psw_check = false;
		     ins_setup_rr();
		     fp_rdv1 = fp_get_db_from_dh(fp_reg,rf2);
		 	 fp_put_db(rf1,fp_dh_type,fp_rdv1);
		     psw_cc = fp_get_dh_comp_cc(fp_rdv1,0);
		     break;
		 case 0x23:  // 540 "23" "LCDR" "RR"
		 	 psw_check = false;
		     ins_setup_rr();
		     fp_rdv1 = fp_get_db_from_dh(fp_reg,rf2);
		     if (fp_rdv1 != 0){
		     	fp_rdv1 = - fp_rdv1;
		     }
		 	 fp_put_db(rf1,fp_dh_type,fp_rdv1);
		     psw_cc = fp_get_dh_comp_cc(fp_rdv1,0);
		     break;
		 case 0x24:  // 550 "24" "HDR" "RR"
		 	 psw_check = false;
		     ins_setup_rr();
		     fp_rdv1 = fp_get_db_from_dh(fp_reg,rf2) 
			         / 2;
		     fp_put_db(rf1,fp_dh_type,fp_rdv1);
		     break;
		 case 0x25:  // 560 "25" "LDXR" "RR"
	     	 psw_check = false;
	         ins_setup_rr();
	         fp_load_reg(fp_dh_type,fp_reg,rf2,fp_lh_type);
		     break;
		 case 0x26:  // 580 "26" "MXR" "RR"
	     	 psw_check = false;
	         ins_setup_rr();
	         fp_rbdv1 = fp_get_bd_from_lh(fp_reg,rf1)
			      .multiply(fp_get_bd_from_lh(fp_reg,rf2),fp_bd_context)
				  .round(fp_x_context);
	         fp_put_bd(rf1,fp_lh_type,fp_rbdv1);
	         check_lh_mpy();
		     break;
		 case 0x27:  // 590 "27" "MXDR" "RR"
	     	 psw_check = false;
	         ins_setup_rr();
	         fp_rbdv1 = fp_get_bd_from_dh(fp_reg,rf1)
			      .multiply(fp_get_bd_from_dh(fp_reg,rf2),fp_db_context)
				  .round(fp_x_context);
	         fp_put_bd(rf1,fp_lh_type,fp_rbdv1);
	         check_lh_mpy();
		     break;
		 case 0x28:  // 600 "28" "LDR" "RR"
		 	 psw_check = false;
		 	 ins_setup_rr();
             fp_load_reg(fp_dh_type,fp_reg,rf2,fp_dh_type);
		     break;
		 case 0x29:  // 610 "29" "CDR" "RR"
	     	 psw_check = false;
	         ins_setup_rr();
		     psw_cc = fp_get_dh_comp_cc(
	     		     fp_get_db_from_dh(fp_reg,rf1),
					 fp_get_db_from_dh(fp_reg,rf2)); 
		     break;
		 case 0x2A:  // 620 "2A" "ADR" "RR"
		 	 psw_check = false;
		     ins_setup_rr();
		     fp_rdv1 = fp_get_db_from_dh(fp_reg,rf1) 
			         + fp_get_db_from_dh(fp_reg,rf2);
		     fp_put_db(rf1,fp_dh_type,fp_rdv1);
		     psw_cc = fp_get_dh_add_sub_cc(); 
		     break;
		 case 0x2B:  // 630 "2B" "SDR" "RR"
		 	 psw_check = false;
		     ins_setup_rr();
		     fp_rdv1 = fp_get_db_from_dh(fp_reg,rf1) 
			         - fp_get_db_from_dh(fp_reg,rf2);
		     fp_put_db(rf1,fp_dh_type,fp_rdv1);
		     psw_cc = fp_get_dh_add_sub_cc();
		     break;
		 case 0x2C:  // 640 "2C" "MDR" "RR"
		 	 psw_check = false;
		     ins_setup_rr();
		     fp_rdv1 = fp_get_db_from_dh(fp_reg,rf1) 
			         * fp_get_db_from_dh(fp_reg,rf2);
		     fp_put_db(rf1,fp_dh_type,fp_rdv1);
	         check_dh_mpy();
		     break;
		 case 0x2D:  // 650 "2D" "DDR" "RR"
		 	 psw_check = false;
		     ins_setup_rr();
		     fp_rdv2 = fp_get_db_from_dh(fp_reg,rf2);
		     fp_rdv1 = fp_get_db_from_dh(fp_reg,rf1) 
			         / fp_rdv2;
		     fp_put_db(rf1,fp_dh_type,fp_rdv1);
	         check_dh_div();
		     break;
		 case 0x2E:  // 660 "2E" "AWR" "RR"
		 	 psw_check = false;
		     ins_setup_rr();
		     fp_rdv1 = fp_get_db_from_dh(fp_reg,rf1) 
			         + fp_get_db_from_dh(fp_reg,rf2);
		     fp_put_db(rf1,fp_dh_type,fp_rdv1);
		     psw_cc = fp_get_dh_add_sub_cc();
		     break;
		 case 0x2F:  // 670 "2F" "SWR" "RR"
		 	 psw_check = false;
		     ins_setup_rr();
		     fp_rdv1 = fp_get_db_from_dh(fp_reg,rf1) 
			         - fp_get_db_from_dh(fp_reg,rf2);
		     fp_put_db(rf1,fp_dh_type,fp_rdv1);
		     psw_cc = fp_get_dh_add_sub_cc();
		     break;
		 case 0x30:  // 680 "30" "LPER" "RR"
		 	 psw_check = false;
		     ins_setup_rr();
		     fp_rdv1 = Math.abs(fp_get_db_from_eh(fp_reg,rf2));
		 	 fp_put_db(rf1,fp_eh_type,fp_rdv1);
		     psw_cc = fp_get_eh_comp_cc(fp_rdv1,0);
		     break;
		 case 0x31:  // 690 "31" "LNER" "RR"
		 	 psw_check = false;
		     ins_setup_rr();
		     fp_rdv1 =  - Math.abs(fp_get_db_from_eh(fp_reg,rf2));
		 	 fp_put_db(rf1,fp_eh_type,fp_rdv1);
		     psw_cc = fp_get_eh_comp_cc(fp_rdv1,0);
		     break;
		 case 0x32:  // 700 "32" "LTER" "RR"
		 	 psw_check = false;
		     ins_setup_rr();
		     fp_rdv1 = fp_get_db_from_eh(fp_reg,rf2);
		 	 fp_put_db(rf1,fp_eh_type,fp_rdv1);
		     psw_cc = fp_get_eh_comp_cc(fp_rdv1,0);
		     break;
		 case 0x33:  // 710 "33" "LCER" "RR"
		 	 psw_check = false;
		     ins_setup_rr();
		     fp_rdv1 = fp_get_db_from_eh(fp_reg,rf2);
		     if (fp_rdv1 != 0){
		     	fp_rdv1 = - fp_rdv1;
		     }
		 	 fp_put_db(rf1,fp_eh_type,fp_rdv1);
		     psw_cc = fp_get_eh_comp_cc(fp_rdv1,0);
		 	 break;
		 case 0x34:  // 720 "34" "HER" "RR"
		 	 psw_check = false;
		     ins_setup_rr();
		     fp_rdv1 = fp_get_db_from_eh(fp_reg,rf2) 
			         / 2;
		     fp_put_db(rf1,fp_eh_type,fp_rdv1);
		     break;
		 case 0x35:  // 730 "35" "LEDR" "RR"
	     	 psw_check = false;
	         ins_setup_rr();
	         fp_load_reg(fp_eh_type,fp_reg,rf2,fp_dh_type);
		     break;
		 case 0x36:  // 750 "36" "AXR" "RR"
	     	 psw_check = false;
	         ins_setup_rr();
	         fp_rbdv1 = fp_get_bd_from_lh(fp_reg,rf1)
			      .add(fp_get_bd_from_lh(fp_reg,rf2),fp_bd_context).round(fp_x_context);
	         fp_put_bd(rf1,fp_lh_type,fp_rbdv1);
		     psw_cc = fp_get_lh_add_sub_cc();
		     break;
		 case 0x37:  // 760 "37" "SXR" "RR"
	     	 psw_check = false;
	         ins_setup_rr();
	         fp_rbdv1 = fp_get_bd_from_lh(fp_reg,rf1)
			      .subtract(fp_get_bd_from_lh(fp_reg,rf2),fp_bd_context).round(fp_x_context);
	         fp_put_bd(rf1,fp_lh_type,fp_rbdv1);
		     psw_cc = fp_get_lh_add_sub_cc();
		     break;
		 case 0x38:  // 770 "38" "LER" "RR"
		 	 psw_check = false;
		 	 ins_setup_rr();
             fp_load_reg(fp_eh_type,fp_reg,rf2,fp_eh_type);
		     break;
		 case 0x39:  // 780 "39" "CER" "RR"
	     	 psw_check = false;
	         ins_setup_rr();
		     psw_cc = fp_get_eh_comp_cc(
	     		     fp_get_db_from_eh(fp_reg,rf1),
					 fp_get_db_from_eh(fp_reg,rf2)); 
		     break;
		 case 0x3A:  // 790 "3A" "AER" "RR"
		 	 psw_check = false;
		     ins_setup_rr();
		     fp_rdv1 = fp_get_db_from_eh(fp_reg,rf1) 
			         + fp_get_db_from_eh(fp_reg,rf2);
		     fp_put_db(rf1,fp_eh_type,fp_rdv1);
		     psw_cc = fp_get_eh_add_sub_cc();
		     break;
		 case 0x3B:  // 800 "3B" "SER" "RR"
		 	 psw_check = false;
		     ins_setup_rr();
		     fp_rdv1 = fp_get_db_from_eh(fp_reg,rf1) 
			         - fp_get_db_from_eh(fp_reg,rf2);
		     fp_put_db(rf1,fp_eh_type,fp_rdv1);
		     psw_cc = fp_get_eh_add_sub_cc();
		     break;
		 case 0x3C:  // 810 "3C" "MDER" "RR"
		 	 psw_check = false;
		     ins_setup_rr();
		     fp_rdv1 = fp_get_db_from_eh(fp_reg,rf1) 
			         * fp_get_db_from_eh(fp_reg,rf2);
		     fp_put_db(rf1,fp_dh_type,fp_rdv1);
	         check_dh_mpy();
		     break;
		 case 0x3D:  // 830 "3D" "DER" "RR"
		 	 psw_check = false;
		     ins_setup_rr();
             fp_rdv2 = fp_get_db_from_eh(fp_reg,rf2);
		     fp_rdv1 = fp_get_db_from_eh(fp_reg,rf1) 
			         / fp_rdv2;
		     fp_put_db(rf1,fp_eh_type,fp_rdv1);
	         check_eh_div();
		     break;
		 case 0x3E:  // 840 "3E" "AUR" "RR"
		 	 psw_check = false;
		     ins_setup_rr();
		     fp_rdv1 = fp_get_db_from_eh(fp_reg,rf1) 
			         + fp_get_db_from_eh(fp_reg,rf2);
		     fp_put_db(rf1,fp_eh_type,fp_rdv1);
		     psw_cc = fp_get_eh_add_sub_cc();
		     break;
		 case 0x3F:  // 850 "3F" "SUR" "RR"
		 	 psw_check = false;
		     ins_setup_rr();
		     fp_rdv1 = fp_get_db_from_eh(fp_reg,rf1) 
			         - fp_get_db_from_eh(fp_reg,rf2);
		     fp_put_db(rf1,fp_eh_type,fp_rdv1);
		     psw_cc = fp_get_eh_add_sub_cc();
		     break;
		 case 0x40:  // 860 "40" "STH" "RX"
		 	 psw_check = false;
		     ins_setup_rx();
		     mem.putShort(xbd2_loc,(short)reg.getInt(rf1+4));
		     break;
		 case 0x41:  // 870 "41" "LA" "RX"
		     psw_check = false;
		     ins_setup_rx();
		     reg.putInt(rf1+4,xbd2_loc & psw_amode);
		     break;
		 case 0x42:  // 880 "42" "STC" "RX"
		 	 psw_check = false;
		     ins_setup_rx();
		     mem.put(xbd2_loc,reg.get(rf1+4+3));
		     break;
		 case 0x43:  // 890 "43" "IC" "RX"
		 	 psw_check = false;
		     ins_setup_rx();
		     reg.put(rf1+7,mem.get(xbd2_loc));
		     break;
		 case 0x44:  // 900 "44" "EX" "RX"
		 	 psw_check = false;
		     ins_setup_rx();
		     if (!ex_mode){
		         ex_psw_return = psw_loc;
		         set_psw_loc(xbd2_loc);
		         ex_mode = true;
		         ex_mod_byte   = mem.get(psw_loc+1);
		         if (rf1 != 0){
		         	mem.put(psw_loc+1,(byte)(ex_mod_byte | reg.get(rf1+4+3)));
		         }
		     } else {
		     	set_psw_check(psw_pic_exec);
		     }
		     break;
		 case 0x45:  // 910 "45" "BAL" "RX"
			 psw_check = false;
		     ins_setup_rx();
		     if (ex_mode){
		     	reg.putInt(rf1+4,ex_psw_return | psw_amode_bit);
		     } else {
			    reg.putInt(rf1+4,psw_loc | psw_amode_bit);
		     }
			 set_psw_loc(xbd2_loc);
		     break;
		 case 0x46:  // 920 "46" "BCT" "RX"
			 psw_check = false;
		     ins_setup_rx();
			 rv1 = reg.getInt(rf1+4)-1;
			 reg.putInt(rf1+4,rv1);
			 if (rv1 != 0){
				set_psw_loc(xbd2_loc);
			 }
		     break;
		 case 0x47:  // 930 "47" "BC" "RX"
			 psw_check = false;
		     ins_setup_rx();
			 if ((psw_cc & mf1) > 0){
				set_psw_loc(xbd2_loc);
			 }
		     break;
		 case 0x48:  // 1100 "48" "LH" "RX"
		 	 psw_check = false;
		     ins_setup_rx();
		     reg.putInt(rf1+4,mem.getShort(xbd2_loc));
		     break;
		 case 0x49:  // 1110 "49" "CH" "RX"
		 	 psw_check = false;
		     ins_setup_rx();
		     psw_cc = get_int_comp_cc(
	     		     reg.getInt(rf1+4),
					 mem.getShort(xbd2_loc));     
		     break;
		 case 0x4A:  // 1120 "4A" "AH" "RX"
		 	 psw_check = false;
		     ins_setup_rx();
		     rv1 = reg.getInt(rf1+4);
			 rv2 = mem.getShort(xbd2_loc);
			 rv3 = rv1 + rv2;
			 reg.putInt(rf1+4,rv3);
			 psw_cc = get_int_add_cc();
		     break;
		 case 0x4B:  // 1130 "4B" "SH" "RX"
		 	 psw_check = false;
		     ins_setup_rx();
		     rv1 = reg.getInt(rf1+4);
			 rv2 = mem.getShort(xbd2_loc);
			 rv3 = rv1 - rv2;
		     reg.putInt(rf1+4,rv3);
			 psw_cc = get_int_sub_cc();
		     break;
		 case 0x4C:  // 1140 "4C" "MH" "RX"
		 	 psw_check = false;
		     ins_setup_rx();
		     rv1 = reg.getInt(rf1+4) * mem.getShort(xbd2_loc);
			 reg.putInt(rf1+4,rv1);
		     break;
		 case 0x4D:  // 1150 "4D" "BAS" "RX"
			 psw_check = false;
		     ins_setup_rx();
		     if (ex_mode){
		     	reg.putInt(rf1+4,ex_psw_return | psw_amode_bit);
		     } else {
			    reg.putInt(rf1+4,psw_loc | psw_amode_bit);
		     }
			 set_psw_loc(xbd2_loc);
		     break;
		 case 0x4E:  // 1160 "4E" "CVD" "RX"
		     psw_check = false;
		     ins_setup_rx();
	         big_int = BigInteger.valueOf(reg.getInt(rf1+4));
		     put_pd_big_int(xbd2_loc,8);
		     break;
		 case 0x4F:  // 1170 "4F" "CVB" "RX"
		 	 psw_check = false;
		     ins_setup_rx();
		     big_int = get_pd_big_int(xbd2_loc,8);
		     reg.putInt(rf1+4,big_int.intValue());
		     break;
		 case 0x50:  // 1180 "50" "ST" "RX"
		     ins_setup_rx();
		     psw_check = false;
		     mem.putInt(xbd2_loc,reg.getInt(rf1+4));
		     break;
		 case 0x51:  // 1190 "51" "LAE" "RX"
		     ins_setup_rx();
		     break;
		 case 0x54:  // 1200 "54" "N" "RX"
		 	 psw_check = false;
		     ins_setup_rx();
		     rv1 = reg.getInt(rf1+4) & mem.getInt(xbd2_loc);
		     reg.putInt(rf1+4,rv1);
		     if (rv1 == 0){
		     	psw_cc = psw_cc0;
		     } else {
		     	psw_cc = psw_cc1;
		     }
		     break;
		 case 0x55:  // 1210 "55" "CL" "RX"
		     psw_check = false;
		     ins_setup_rx();
		     psw_cc = get_int_log_comp_cc(
		     		     reg.getInt(rf1+4),
						 mem.getInt(xbd2_loc));
		     break;
		 case 0x56:  // 1220 "56" "O" "RX"
		 	 psw_check = false;
		     ins_setup_rx();
		     rv1 = reg.getInt(rf1+4) | mem.getInt(xbd2_loc);
		     reg.putInt(rf1+4,rv1);
		     if (rv1 == 0){
		     	psw_cc = psw_cc0;
		     } else {
		     	psw_cc = psw_cc1;
		     }
		     break;
		 case 0x57:  // 1230 "57" "X" "RX"
		 	 psw_check = false;
		     ins_setup_rx();
		     rv1 = reg.getInt(rf1+4) ^ mem.getInt(xbd2_loc);
		     reg.putInt(rf1+4,rv1);
		     if (rv1 == 0){
		     	psw_cc = psw_cc0;
		     } else {
		     	psw_cc = psw_cc1;
		     }
		     break;
		 case 0x58:  // 1240 "58" "L" "RX"
			 psw_check = false;
		     ins_setup_rx();
			 reg.putInt(rf1+4,mem.getInt(xbd2_loc));
		     break;
		 case 0x59:  // 1250 "59" "C" "RX"
		     psw_check = false;
		     ins_setup_rx();
		     psw_cc = get_int_comp_cc(
	     		     reg.getInt(rf1+4),
					 mem.getInt(xbd2_loc));
		     break;
		 case 0x5A:  // 1260 "5A" "A" "RX"
		 	 psw_check = false;
		     ins_setup_rx();
		     rv1 = reg.getInt(rf1+4);
			 rv2 = mem.getInt(xbd2_loc);
			 rv3 = rv1 + rv2;
			 reg.putInt(rf1+4,rv3);
			 psw_cc = get_int_add_cc();
		     break;
		 case 0x5B:  // 1270 "5B" "S" "RX"
		 	 psw_check = false;
		     ins_setup_rx();
		     rv1 = reg.getInt(rf1+4);
			 rv2 = mem.getInt(xbd2_loc);
			 rv3 = rv1 - rv2;
		     reg.putInt(rf1+4,rv3);
			 psw_cc = get_int_sub_cc();
		     break;
		 case 0x5C:  // 1280 "5C" "M" "RX"
		 	 psw_check = false;
		     ins_setup_rx();
		     rlv1 = (long) reg.getInt(rf1+8+4) * (long) mem.getInt(xbd2_loc);
			 work_reg.putLong(0,rlv1);
			 reg.putInt(rf1+4,work_reg.getInt(0));
			 reg.putInt(rf1+8+4,work_reg.getInt(4));
		     break;
		 case 0x5D:  // 1290 "5D" "D" "RX"
		 	 psw_check = false;
		     ins_setup_rx();
		     rlv1 = reg.getLong(rf1);
		     rv2  = mem.getInt(xbd2_loc);
		     if (rv2 != 0){
		     	rv1  = (int) rlv1 / rv2;
		     } else {
		     	set_psw_check(psw_pic_fx_div);
		     	break;
		     }
		     reg.putInt(rf1+4,(int)(rlv1 - rv1 * rv2));
		     reg.putInt(rf1+12,rv1);
		     break;
		 case 0x5E:  // 1300 "5E" "AL" "RX"
	     	 psw_check = false;
	         ins_setup_rx();
	         rlvw = ((long) reg.getInt(rf1+4) & long_low32_bits) 
			      + ((long) mem.getInt(xbd2_loc) & long_low32_bits);
             rlv1  = rlvw & long_low32_bits;
	         reg.putInt(rf1+4,(int)rlv1);
			 psw_cc = get_log_add_cc(rlv1,rlvw - rlv1);
		     break;
		 case 0x5F:  // 1310 "5F" "SL" "RX"
	     	 psw_check = false;
	         ins_setup_rx();
	         rlvw = ((long) reg.getInt(rf1+4) & long_low32_bits)
			      - ((long) mem.getInt(xbd2_loc) & long_low32_bits);
             rlv1 = rlvw & long_low32_bits;
	         reg.putInt(rf1+4,(int)rlv1);
			 psw_cc = get_log_sub_cc(rlv1,rlvw - rlv1);
		     break;
		 case 0x60:  // 1320 "60" "STD" "RX"
		 	 psw_check = false;
		     ins_setup_rx();
		 	 if (fp_reg_ctl[mf1] != fp_ctl_ld){
		 	 	fp_store_reg(fp_reg,rf1);
			 }
			 mem.putLong(xbd2_loc,fp_reg.getLong(rf1));
			 break;
		 case 0x67:  // 1330 "67" "MXD" "RX"
	     	 psw_check = false;
	         ins_setup_rx();
	         fp_rbdv1 = fp_get_bd_from_dh(fp_reg,rf1)
			      .multiply(fp_get_bd_from_dh(mem,xbd2_loc),fp_db_context)
				  .round(fp_x_context);
	         fp_put_bd(rf1,fp_lh_type,fp_rbdv1);
	         check_lh_mpy();
		     break;
		 case 0x68:  // 1340 "68" "LD" "RX"
		 	 psw_check = false;
		 	 ins_setup_rx();
             fp_load_reg(fp_dh_type,mem,xbd2_loc,fp_dh_type);
		     break;
		 case 0x69:  // 1350 "69" "CD" "RX"
		 	 psw_check = false;
		     ins_setup_rx();
		     psw_cc = fp_get_dh_comp_cc(
	     		      fp_get_db_from_dh(fp_reg,rf1),
					  fp_get_db_from_dh(mem,xbd2_loc));  
		     break;
		 case 0x6A:  // 1360 "6A" "AD" "RX"
		 	 psw_check = false;
		     ins_setup_rx();
		     fp_rdv1 = fp_get_db_from_dh(fp_reg,rf1) 
			         + fp_get_db_from_dh(mem,xbd2_loc);
		     fp_put_db(rf1,fp_dh_type,fp_rdv1);
		     psw_cc = fp_get_dh_add_sub_cc(); 
		     break;
		 case 0x6B:  // 1370 "6B" "SD" "RX"
		 	 psw_check = false;
		     ins_setup_rx();
		     fp_rdv1 = fp_get_db_from_dh(fp_reg,rf1) 
			         - fp_get_db_from_dh(mem,xbd2_loc);
		     fp_put_db(rf1,fp_dh_type,fp_rdv1);
		     psw_cc = fp_get_dh_add_sub_cc(); 
		     break;
		 case 0x6C:  // 1380 "6C" "MD" "RX"
		 	 psw_check = false;
		     ins_setup_rx();
		     fp_rdv1 = fp_get_db_from_dh(fp_reg,rf1) 
			         * fp_get_db_from_dh(mem,xbd2_loc);
		     fp_put_db(rf1,fp_dh_type,fp_rdv1);
	         check_dh_mpy();
		     break;
		 case 0x6D:  // 1390 "6D" "DD" "RX"
		 	 psw_check = false;
		 	 psw_check = false;
		     ins_setup_rx();
		     fp_rdv2 = fp_get_db_from_dh(mem,xbd2_loc);
		     fp_rdv1 = fp_get_db_from_dh(fp_reg,rf1) 
			         / fp_rdv2;
		     fp_put_db(rf1,fp_dh_type,fp_rdv1); 
	         check_dh_div();
		     break;
		 case 0x6E:  // 1400 "6E" "AW" "RX"
		 	 psw_check = false;
		     ins_setup_rx();
		     fp_rdv1 = fp_get_db_from_dh(fp_reg,rf1) 
			         + fp_get_db_from_dh(mem,xbd2_loc);
		     fp_put_db(rf1,fp_dh_type,fp_rdv1);
		     psw_cc = fp_get_dh_add_sub_cc(); 
		     break;
		 case 0x6F:  // 1410 "6F" "SW" "RX"
		 	 psw_check = false;
		     ins_setup_rx();
		     fp_rdv1 = fp_get_db_from_dh(fp_reg,rf1) 
			         - fp_get_db_from_dh(mem,xbd2_loc);
		     fp_put_db(rf1,fp_dh_type,fp_rdv1);
		     psw_cc = fp_get_dh_add_sub_cc(); 
		     break;
		 case 0x70:  // 1420 "70" "STE" "RX"
		 	 psw_check = false;
		     ins_setup_rx();
		 	 if (fp_reg_ctl[mf1] != fp_ctl_ld){
		 	 	fp_store_reg(fp_reg,rf1);
			 }
			 mem.putInt(xbd2_loc,fp_reg.getInt(rf1));
			 break;
		 case 0x71:  // 1430 "71" "MS" "RX"
		 	 psw_check = false;
		     ins_setup_rx();
		     reg.putInt(rf1+4,reg.getInt(rf1+4)*mem.getInt(xbd2_loc));
		     break;
		 case 0x78:  // 1440 "78" "LE" "RX"
		 	 psw_check = false;
		 	 ins_setup_rx();
             fp_load_reg(fp_eh_type,mem,xbd2_loc,fp_eh_type);
		     break;
		 case 0x79:  // 1450 "79" "CE" "RX"
		 	 psw_check = false;
		     ins_setup_rx();
		     psw_cc = fp_get_eh_comp_cc(
	     		      fp_get_db_from_eh(fp_reg,rf1),
					  fp_get_db_from_eh(mem,xbd2_loc));
		     break;
		 case 0x7A:  // 1460 "7A" "AE" "RX"
		 	 psw_check = false;
		     ins_setup_rx();
		     fp_rdv1 = fp_get_db_from_eh(fp_reg,rf1) 
      		         + fp_get_db_from_eh(mem,xbd2_loc);
      		 fp_put_db(rf1,fp_dh_type,fp_rdv1);
		     psw_cc = fp_get_eh_add_sub_cc();
		     break;
		 case 0x7B:  // 1470 "7B" "SE" "RX"
		 	 psw_check = false;
		     ins_setup_rx();
		     fp_rdv1 = fp_get_db_from_eh(fp_reg,rf1) 
			         - fp_get_db_from_eh(mem,xbd2_loc);
		     fp_put_db(rf1,fp_eh_type,fp_rdv1);
		     psw_cc = fp_get_eh_add_sub_cc();
		     break;
		 case 0x7C:  // 1480 "7C" "MDE" "RX"
		 	 psw_check = false;
		     ins_setup_rx();
		     fp_rdv1 = fp_get_db_from_eh(fp_reg,rf1) 
			         * fp_get_db_from_eh(mem,xbd2_loc);
		     fp_put_db(rf1,fp_dh_type,fp_rdv1);
	         check_dh_mpy();
		     break;
		 case 0x7D:  // 1500 "7D" "DE" "RX"
		 	 psw_check = false;
		     ins_setup_rx();
		     fp_rdv2 = fp_get_db_from_eh(mem,xbd2_loc);
		     fp_rdv1 = fp_get_db_from_eh(fp_reg,rf1) 
			         / fp_rdv2;
		     fp_put_db(rf1,fp_eh_type,fp_rdv1);
	         check_eh_div();
		     break;
		 case 0x7E:  // 1510 "7E" "AU" "RX"
		 	 psw_check = false;
		     ins_setup_rx();
		     fp_rdv1 = fp_get_db_from_eh(fp_reg,rf1) 
			         + fp_get_db_from_eh(mem,xbd2_loc);
		     fp_put_db(rf1,fp_eh_type,fp_rdv1);
		     psw_cc = fp_get_eh_comp_cc(fp_rdv1,0);
		     break;
		 case 0x7F:  // 1520 "7F" "SU" "RX"
		 	 psw_check = false;
		     ins_setup_rx();
		     fp_rdv1 = fp_get_db_from_eh(fp_reg,rf1) 
			         - fp_get_db_from_eh(mem,xbd2_loc);
		     fp_put_db(rf1,fp_eh_type,fp_rdv1);
		     psw_cc = fp_get_eh_comp_cc(fp_rdv1,0);
		     break;
		 case 0x80:  // 1530 "8000" "SSM" "S"
		     ins_setup_s();
		     break;
		 case 0x82:  // 1540 "8200" "LPSW" "S"
		     ins_setup_s();
		     break;
		 case 0x83:  // 1550 "83" "DIAGNOSE" "DM"
		     ins_setup_dm();
		     break;
		 case 0x84:  // 1560 "84" "BRXH" "RSI"
		     psw_check = false;
	         ins_setup_rsi();
	         rv3 = reg.getInt(rf3+4);
	         rv1 = reg.getInt(rf1+4) + rv3;
	         reg.putInt(rf1+4,rv1);
	         if (rf3 == ((rf3 >> 4) << 4)){
	         	rv3 = reg.getInt(rf3+12);
	         }
	         if (rv1 > rv3){
	             set_psw_loc(psw_loc - 4 + 2*if2);
	         }
		     break;
		 case 0x85:  // 1580 "85" "BRXLE" "RSI"
		     psw_check = false;
	         ins_setup_rsi();
	         rv3 = reg.getInt(rf3+4);
	         rv1 = reg.getInt(rf1+4) + rv3;
	         reg.putInt(rf1+4,rv1);
	         if (rf3 == ((rf3 >> 4) << 4)){
	         	rv3 = reg.getInt(rf3+12);
	         }
	         if (rv1 <= rv3){
	             set_psw_loc(psw_loc - 4 + 2*if2);
	         }
		     break;
		 case 0x86:  // 1600 "86" "BXH" "RS"
		     psw_check = false;
	         ins_setup_rs();
	         rv3 = reg.getInt(rf3+4);
	         rv1 = reg.getInt(rf1+4) + rv3;
	         reg.putInt(rf1+4,rv1);
	         if (rf3 == ((rf3 >> 4) << 4)){
	         	rv3 = reg.getInt(rf3+12);
	         }
	         if (rv1 > rv3){
	             set_psw_loc(bd2_loc);
	         }
		     break;
		 case 0x87:  // 1610 "87" "BXLE" "RS"
		     psw_check = false;
	         ins_setup_rs();
	         rv3 = reg.getInt(rf3+4);
	         rv1 = reg.getInt(rf1+4) + rv3;
	         reg.putInt(rf1+4,rv1);
	         if (rf3 == ((rf3 >> 4) << 4)){
	         	rv3 = reg.getInt(rf3+12);
	         }
	         if (rv1 <= rv3){
	             set_psw_loc(bd2_loc);
	         }
		     break;
		 case 0x88:  // 1620 "88" "SRL" "RS"
		 	 psw_check = false;
		     ins_setup_rs();
		     reg.putInt(rf1+4,reg.getInt(rf1+4) >>> (bd2_loc & 0x3f));
		     break;
		 case 0x89:  // 1630 "89" "SLL" "RS"
		 	 psw_check = false;
		     ins_setup_rs();
		     reg.putInt(rf1+4,reg.getInt(rf1+4) << (bd2_loc & 0x3f));
		     break;
		 case 0x8A:  // 1640 "8A" "SRA" "RS"
		 	 psw_check = false;
		     ins_setup_rs();
		     reg.putInt(rf1+4,get_sra32(reg.getInt(rf1+4),bd2_loc & 0x3f));
		     break;
		 case 0x8B:  // 1650 "8B" "SLA" "RS"
		 	 psw_check = false;
		     ins_setup_rs();
	         reg.putInt(rf1+4,get_sla32(reg.getInt(rf1+4),bd2_loc & 0x3f));
		     break;
		 case 0x8C:  // 1660 "8C" "SRDL" "RS"
		 	 psw_check = false;
		     ins_setup_rs();
		     work_reg.putInt(0,reg.getInt(rf1+4));
		     work_reg.putInt(4,reg.getInt(rf1+12));
		     work_reg.putLong(0,work_reg.getLong(0) >>> (bd2_loc & 0x3f));
		     reg.putInt(rf1+4,work_reg.getInt(0));
		     reg.putInt(rf1+12,work_reg.getInt(4));
		     break;
		 case 0x8D:  // 1670 "8D" "SLDL" "RS"
		 	 psw_check = false;
		     ins_setup_rs();
		     work_reg.putInt(0,reg.getInt(rf1+4));
		     work_reg.putInt(4,reg.getInt(rf1+12));
		     work_reg.putLong(0,work_reg.getLong(0) << (bd2_loc & 0x3f));
		     reg.putInt(rf1+4,work_reg.getInt(0));
		     reg.putInt(rf1+12,work_reg.getInt(4));
		     break;
		 case 0x8E:  // 1680 "8E" "SRDA" "RS"
		 	 psw_check = false;
		     ins_setup_rs();
		     work_reg.putInt(0,reg.getInt(rf1+4));
		     work_reg.putInt(4,reg.getInt(rf1+12));
		     work_reg.putLong(0,get_sra64(work_reg.getLong(0),bd2_loc & 0x3f));
		     reg.putInt(rf1+4,work_reg.getInt(0));
		     reg.putInt(rf1+12,work_reg.getInt(4));
		     break;
		 case 0x8F:  // 1690 "8F" "SLDA" "RS"
		 	 psw_check = false;
		     ins_setup_rs();
		     work_reg.putInt(0,reg.getInt(rf1+4));
		     work_reg.putInt(4,reg.getInt(rf1+12));
		     work_reg.putLong(0,get_sla64(work_reg.getLong(0),bd2_loc & 0x3f));
		     reg.putInt(rf1+4,work_reg.getInt(0));
		     reg.putInt(rf1+12,work_reg.getInt(4));
		     break;
		 case 0x90:  // 1700 "90" "STM" "RS"
		     psw_check = false;
		     ins_setup_rs();
		     if (rf1 > rf3){
		     	while (rf1 < reg_byte.length){
		     		mem.putInt(bd2_loc,reg.getInt(rf1+4));
		     		bd2_loc = bd2_loc+4;
		     		rf1 = rf1 + 8;
		     	}
		     	rf1 = 0;
		     }
		     while (rf1 <= rf3){
		     	mem.putInt(bd2_loc,reg.getInt(rf1+4));
		     	bd2_loc = bd2_loc+4;
		     	rf1 = rf1 + 8;
		     }
		     break;
		 case 0x91:  // 1710 "91" "TM" "SI"
		 	 psw_check = false;
		     ins_setup_si();
             rv1 = mem.get(bd1_loc) & 0xff;
             rvw = rv1 & if2;
             if (rvw == 0){
             	psw_cc = psw_cc0;
             } else if (rvw == rv1){
             	psw_cc = psw_cc3;
             } else {
             	psw_cc = psw_cc1;
             }
		     break;
		 case 0x92:  // 1720 "92" "MVI" "SI"
		     psw_check = false;
		     ins_setup_si();
		     mem_byte[bd1_loc] = (byte) if2;
		     break;
		 case 0x93:  // 1730 "9300" "TS" "S"
		 	 psw_check = false;
		     ins_setup_s();
		     if ((mem.get(bd2_loc) & 0x80) != 0){
		     	psw_cc = psw_cc1;
		     } else {
		     	psw_cc = psw_cc0;
		     }
		     mem.put(bd2_loc,(byte)0xff);
		     break;
		 case 0x94:  // 1740 "94" "NI" "SI"
		 	 psw_check = false;
		     ins_setup_si();
		     sv1 = mem.get(bd1_loc) & if2;
		     mem.put(bd1_loc,(byte)sv1);
		     if (sv1 == 0){
		     	psw_cc = psw_cc0;
		     } else {
		     	psw_cc = psw_cc1;
		     }
		     break;
		 case 0x95:  // 1750 "95" "CLI" "SI"
         	 psw_check = false;
		     ins_setup_si();
		     psw_cc = get_int_comp_cc(
		     		    (mem_byte[bd1_loc] & 0xff),
						if2);
		     break;
		 case 0x96:  // 1760 "96" "OI" "SI"
		 	 psw_check = false;
		     ins_setup_si();
		     sv1 = mem.get(bd1_loc) | if2;
		     mem.put(bd1_loc,(byte)sv1);
		     if (sv1 == 0){
		     	psw_cc = psw_cc0;
		     } else {
		     	psw_cc = psw_cc1;
		     }
		     break;
		 case 0x97:  // 1770 "97" "XI" "SI"
		 	 psw_check = false;
		     ins_setup_si();
		     sv1 = mem.get(bd1_loc) ^ if2;
		     mem.put(bd1_loc,(byte)sv1);
		     if (sv1 == 0){
		     	psw_cc = psw_cc0;
		     } else {
		     	psw_cc = psw_cc1;
		     }
		     break;
		 case 0x98:  // 1780 "98" "LM" "RS"
		     psw_check = false;
		     ins_setup_rs();
		     if (rf1 > rf3){
		     	while (rf1 < reg_byte.length){
		     		reg.putInt(rf1+4,mem.getInt(bd2_loc));
		     		bd2_loc = bd2_loc+4;
		     		rf1 = rf1 + 8;
		     	}
		     	rf1 = 0;
		     }
		     while (rf1 <= rf3){
	     		reg.putInt(rf1+4,mem.getInt(bd2_loc));
		     	bd2_loc = bd2_loc+4;
		     	rf1 = rf1 + 8;
		     }
		     break;
		 case 0x99:  // 1790 "99" "TRACE" "RS"
		     ins_setup_rs();
		     break;
		 case 0x9A:  // 1800 "9A" "LAM" "RS"
		     ins_setup_rs();
		     break;
		 case 0x9B:  // 1810 "9B" "STAM" "RS"
		     ins_setup_rs();
		     break;
		 case 0xA5:  
		     opcode2 = mem_byte[psw_loc+opcode2_offset_ri] & 15;
		     switch (opcode2){
		     case 0x0:  // 1820 "A50" "IIHH" "RI"
		         psw_check = false;
		     	 ins_setup_ri();
		     	 reg.putShort(rf1,(short)if2);
		         break;
		     case 0x1:  // 1830 "A51" "IIHL" "RI"
		     	 psw_check = false;
		         ins_setup_ri();
		     	 reg.putShort(rf1+2,(short)if2);
		         break;
		     case 0x2:  // 1840 "A52" "IILH" "RI"
		     	 psw_check = false;
		         ins_setup_ri();
		     	 reg.putShort(rf1+4,(short)if2);
		         break;
		     case 0x3:  // 1850 "A53" "IILL" "RI"
		     	 psw_check = false;
		         ins_setup_ri();
		     	 reg.putShort(rf1+6,(short)if2);
		         break;
		     case 0x4:  // 1860 "A54" "NIHH" "RI"
		         psw_check = false;
		     	 ins_setup_ri();
		     	 rv1 = reg.getInt(rf1) & ((if2 << 16) | 0xffff);
		     	 reg.putInt(rf1,rv1);
			     if (rv1 == 0){
			     	psw_cc = psw_cc0;
			     } else {
			     	psw_cc = psw_cc1;
			     }
		     	 break;
		     case 0x5:  // 1870 "A55" "NIHL" "RI"
		         psw_check = false;
		     	 ins_setup_ri();		     	 
		     	 rv1 = reg.getInt(rf1) & (0xffff0000 | (if2 & 0xffff));
		     	 reg.putInt(rf1,rv1);
			     if (rv1 == 0){
			     	psw_cc = psw_cc0;
			     } else {
			     	psw_cc = psw_cc1;
			     }
		         break;
		     case 0x6:  // 1880 "A56" "NILH" "RI"
		     	 psw_check = false;
		     	 ins_setup_ri();
		     	 rv1 = reg.getInt(rf1+4) & (0xffff | (if2 << 16));
		     	 reg.putInt(rf1+4,rv1);
			     if (rv1 == 0){
			     	psw_cc = psw_cc0;
			     } else {
			     	psw_cc = psw_cc1;
			     }
		     	 break;
		     case 0x7:  // 1890 "A57" "NILL" "RI"
		         psw_check = false;
		     	 ins_setup_ri();
		     	 rv1 = reg.getInt(rf1+4) & (0xffff0000 | (if2 & 0xffff));
		     	 reg.putInt(rf1+4,rv1);
			     if (rv1 == 0){
			     	psw_cc = psw_cc0;
			     } else {
			     	psw_cc = psw_cc1;
			     }
		     	 break;
		     case 0x8:  // 1900 "A58" "OIHH" "RI"
		     	 psw_check = false;
		     	 ins_setup_ri();
		     	 rv1 = reg.getInt(rf1) | (if2 << 16);
		     	 reg.putInt(rf1,rv1);
			     if (rv1 == 0){
			     	psw_cc = psw_cc0;
			     } else {
			     	psw_cc = psw_cc1;
			     }
		         break;
		     case 0x9:  // 1910 "A59" "OIHL" "RI"
		     	 psw_check = false;
		         ins_setup_ri();
		     	 rv1 = reg.getInt(rf1) | (if2 & 0xffff);
		     	 reg.putInt(rf1,rv1);
		         break;
		     case 0xA:  // 1920 "A5A" "OILH" "RI"
		         psw_check = false;
		     	 ins_setup_ri();
		     	 reg.putInt(rf1+4,reg.getInt(rf1+4) | (if2 << 16));
			     if (rv1 == 0){
			     	psw_cc = psw_cc0;
			     } else {
			     	psw_cc = psw_cc1;
			     }
		     	 break;
		     case 0xB:  // 1930 "A5B" "OILL" "RI"\
		     	 psw_check = false;
		         ins_setup_ri();
		     	 rv1 = reg.getInt(rf1+4) | (if2 & 0xffff);
		     	 reg.putInt(rf1+4,rv1);
			     if (rv1 == 0){
			     	psw_cc = psw_cc0;
			     } else {
			     	psw_cc = psw_cc1;
			     }
		         break;
		     case 0xC:  // 1940 "A5C" "LLIHH" "RI"
		     	 psw_check = false;
		         ins_setup_ri();
		         reg.putShort(rf1,(short)if2);
		         break;
		     case 0xD:  // 1950 "A5D" "LLIHL" "RI"
		     	 psw_check = false;
		         ins_setup_ri();
		         reg.putShort(rf1+2,(short)if2);
		         break;
		     case 0xE:  // 1960 "A5E" "LLILH" "RI"
		     	 psw_check = false;
		         ins_setup_ri();
		         reg.putShort(rf1+4,(short)if2);
		         break;
		     case 0xF:  // 1970 "A5F" "LLILL" "RI"
		     	 psw_check = false;
		         ins_setup_ri();
		         reg.putShort(rf1+6,(short)if2);
		         break;
		     }
		     break;
		 case 0xA7:  
		     opcode2 = mem_byte[psw_loc+opcode2_offset_ri] & 15;
		     switch (opcode2){
		     case 0x0:  // 1980 "A70" "TMLH" "RI"
			 	 psw_check = false;
			     ins_setup_ri();
	             psw_cc = get_tm_reg_cc(reg.getShort(rf1+4) & 0xffff,if2);
		         break;
		     case 0x1:  // 2000 "A71" "TMLL" "RI"
			 	 psw_check = false;
			     ins_setup_ri();
	             psw_cc = get_tm_reg_cc(reg.getShort(rf1+6) & 0xffff,if2);
		         break;
		     case 0x2:  // 2020 "A72" "TMHH" "RI"
			 	 psw_check = false;
			     ins_setup_ri();
	             psw_cc = get_tm_reg_cc(reg.getShort(rf1) & 0xffff,if2);
		         break;
		     case 0x3:  // 2030 "A73" "TMHL" "RI"
			 	 psw_check = false;
			     ins_setup_ri();
	             psw_cc = get_tm_reg_cc(reg.getShort(rf1+2) & 0xffff,if2);
		         break;
		     case 0x4:  // 2040 "A74" "BRC" "RI"
		     	 psw_check = false;
		         ins_setup_ri();
				 if ((psw_cc & mf1) > 0){
					set_psw_loc(psw_loc - 4 + 2 * if2);
				 }
		         break;
		     case 0x5:  // 2360 "A75" "BRAS" "RI"
			     psw_check = false;
		         ins_setup_ri();
		         reg.putInt(rf1+4,psw_loc | psw_amode_bit);
		         set_psw_loc(psw_loc - 4 + 2*if2);
		         break;
		     case 0x6:  // 2380 "A76" "BRCT" "RI"
				 psw_check = false;
			     ins_setup_ri();
				 rv1 = reg.getInt(rf1+4)-1;
				 reg.putInt(rf1+4,rv1);
				 if (rv1 != 0){
					set_psw_loc(psw_loc - 4 + 2*if2);
				 }
		         break;
		     case 0x7:  // 2400 "A77" "BRCTG" "RI"
				 psw_check = false;
			     ins_setup_ri();
				 rlv1 = reg.getLong(rf1)-1;
				 reg.putLong(rf1,rlv1);
				 if (rlv1 != 0){
					set_psw_loc(psw_loc - 4 + 2*if2);
				 }
		         break;
		     case 0x8:  // 2420 "A78" "LHI" "RI"
		         psw_check = false;
		         ins_setup_ri();
		         reg.putInt(rf1+4,if2);
		         break;
		     case 0x9:  // 2430 "A79" "LGHI" "RI"
		     	 psw_check = false;
		         ins_setup_ri();
		     	 reg.putLong(rf1,if2);
		         break;
		     case 0xA:  // 2440 "A7A" "AHI" "RI"
		         psw_check = false;
		         ins_setup_ri();
		         rv1 = reg.getInt(rf1+4);
				 rv2 = if2;
		         rv3 = rv1 + rv2;
		         reg.putInt(rf1+4,rv3);
		         psw_cc = get_int_add_cc();
		         break;
		     case 0xB:  // 2450 "A7B" "AGHI" "RI"
		         psw_check = false;
		         ins_setup_ri();
		         rlv1 = reg.getLong(rf1);
				 rlv2 = if2;
		         rlv3 = rlv1 + rlv2;
		         reg.putLong(rf1,rlv3);
		         psw_cc = get_long_add_cc();
		         break;
		     case 0xC:  // 2460 "A7C" "MHI" "RI"
		     	 psw_check = false;
		         ins_setup_ri();
		         reg.putInt(rf1+4,reg.getInt(rf1+4)*if2);
		         break;
		     case 0xD:  // 2470 "A7D" "MGHI" "RI"
		     	 psw_check = false;
		         ins_setup_ri();
		         reg.putLong(rf1,reg.getLong(rf1)*if2);
		         break;
		     case 0xE:  // 2480 "A7E" "CHI" "RI"
			     psw_check = false;
		         ins_setup_ri();
		         psw_cc = get_int_comp_cc(
			                reg.getInt(rf1+4),
							if2);
		         break;
		     case 0xF:  // 2490 "A7F" "CGHI" "RI"
			     psw_check = false;
		         ins_setup_ri();
		         psw_cc = get_long_comp_cc(
			                reg.getLong(rf1),
							if2);
		         break;
		     }
		     break;
		 case 0xA8:  // 2500 "A8" "MVCLE" "RS"
		 	 psw_check = false;
		     ins_setup_rs();
			 fill_char = (byte) bd2_loc;
		     bd1_loc = reg.getInt(rf1+4) & psw_amode;
		     bd2_loc = reg.getInt(rf3+4) & psw_amode;
		     rflen1  = reg.getInt(rf1+12) & int_num_bits;
		     rflen2  = reg.getInt(rf3+12) & int_num_bits;
		     data_len = rflen1;
		     pad_len  = 0;
		     psw_cc = psw_cc0;
		     if (rflen1 < rflen2){
		        psw_cc = psw_cc1;
		     } else if (rflen1 > rflen2){
		     	psw_cc = psw_cc2;
		     	data_len = rflen2;
		     	pad_len  = rflen1 - rflen2;
		     }
		     bd1_end = bd1_loc + data_len;
		     while (bd1_loc < bd1_end){
	     		mem.put(bd1_loc,mem.get(bd2_loc));
	     		bd1_loc++;
	     		bd2_loc++;
	     	}
		    bd1_end = bd1_loc + pad_len;
		    while (bd1_loc < bd1_end){ 	
	     		mem.put(bd1_loc,fill_char);
	     		bd1_loc++;
	     	}
		    reg.putInt(rf1+4,(reg.getInt(rf1+4) & psw_amode_high_bits) | bd1_loc);
		    reg.putInt(rf1+12,0);
		    reg.putInt(rf3+4,(reg.getInt(rf3+4) & psw_amode_high_bits) | bd2_loc);
		    reg.putInt(rf3+12,rflen2 - data_len);
		     break;
		 case 0xA9:  // 2510 "A9" "CLCLE" "RS"
		 	psw_check = false;
		     ins_setup_rs();
		     fill_char = (byte) bd2_loc;
		     bd1_loc = reg.getInt(rf1+4) & psw_amode;
		     bd2_loc = reg.getInt(rf3+4) & psw_amode;
		     bd1_start = bd1_loc;
		     bd2_start = bd2_loc;
		     rflen1  = reg.getInt(rf1+12) & int_num_bits;
		     rflen2  = reg.getInt(rf3+12) & int_num_bits;
		     data_len = rflen1;
		     pad_len  = 0;
		     psw_cc = psw_cc0;
		     if (rflen1 < rflen2){
               data_len = rflen1;
               pad_len  = rflen2 - rflen1;            
		     } else if (rflen1 > rflen2){
		     	data_len = rflen2;
		     	pad_len  = rflen1 - rflen2;
		     }
		     bd1_end = bd1_loc + data_len;
		     while (bd1_loc < bd1_end){
	     		if (mem.get(bd1_loc) != mem.get(bd2_loc)){
            		if ((mem_byte[bd1_loc] &0xff) > (mem_byte[bd2_loc] & 0xff)){
	     				psw_cc = psw_cc_high;
	     			} else {
	     				psw_cc = psw_cc_low;
	     			}
                   bd1_end = bd1_loc;
	     		} else {
	     		    bd1_loc++;
	     		    bd2_loc++;
	     		}
	     	}
		    if (psw_cc == psw_cc_equal & pad_len > 0){
		       if  (rflen1 > rflen2){
			        bd1_end = bd1_loc + pad_len;
		            while (bd1_loc < bd1_end){ 	
	     		        if (mem.get(bd1_loc) != fill_char){
	     			       if ((mem.get(bd1_loc) & 0xff) > (fill_char & 0xff)){
	     				       psw_cc = psw_cc_high;
	     			       } else {
	     				       psw_cc = psw_cc_low;
	     			       }
	     			       bd1_end = bd1_loc;
	     		        } else {
	     		           bd1_loc++;
	     		        }
	     	        }
	           } else {
	           	    bd2_end = bd2_loc + pad_len;
	                while (bd2_loc < bd2_end){ 	
    		           if (mem.get(bd2_loc) != fill_char){
    			           if ((mem.get(bd2_loc) & 0xff) > (fill_char & 0xff)){
    				          psw_cc = psw_cc_high;
    			           } else {
    				          psw_cc = psw_cc_low;
    			           }
    			           bd2_end = bd2_loc;
    		           } else {
    		               bd2_loc++;
    		           }
	                }     	      
	           }
		    }
		    reg.putInt(rf1+4,(reg.getInt(rf1+4) & psw_amode_high_bits) | bd1_loc);
		    reg.putInt(rf1+12,rflen1 - (bd1_loc - bd1_start));
		    reg.putInt(rf3+4,(reg.getInt(rf3+4) & psw_amode_high_bits) | bd2_loc);
		    reg.putInt(rf3+12,rflen2 - (bd2_loc - bd2_start));
		     break;
		 case 0xAC:  // 2520 "AC" "STNSM" "SI"
		     ins_setup_si();
		     break;
		 case 0xAD:  // 2530 "AD" "STOSM" "SI"
		     ins_setup_si();
		     break;
		 case 0xAE:  // 2540 "AE" "SIGP" "RS"
		     ins_setup_rs();
		     break;
		 case 0xAF:  // 2550 "AF" "MC" "SI"
		     ins_setup_si();
		     break;
		 case 0xB1:  // 2560 "B1" "LRA" "RX"
		     ins_setup_rx();
		     break;
		 case 0xB2:  
		     opcode2 = mem_byte[psw_loc+opcode2_offset_s] & 0xff;
		     switch (opcode2){
		     case 0x02:  // 2570 "B202" "STIDP" "S"
		     	 psw_check = false;
		         ins_setup_s();
		         mem.putLong(bd2_loc,cpu_id);
		         break;
		     case 0x04:  // 2580 "B204" "SCK" "S"
		         ins_setup_s();
		         break;
		     case 0x05:  // 2590 "B205" "STCK" "S"
		     	 psw_check = false;
		         ins_setup_s();
		         /*
		          * NOtes:
		          *   1.  The Java time stored is milliseconds
		          *       since Jan. 1, 1970.
		          *    2. This is not consistent mainframe OS times
		          */
		         mem.putLong(bd2_loc,System.currentTimeMillis());
		         break;
		     case 0x06:  // 2600 "B206" "SCKC" "S"
		         ins_setup_s();
		         break;
		     case 0x07:  // 2610 "B207" "STCKC" "S"
		         ins_setup_s();
		         break;
		     case 0x08:  // 2620 "B208" "SPT" "S"
		         ins_setup_s();
		         break;
		     case 0x09:  // 2630 "B209" "STPT" "S"
		         ins_setup_s();
		         break;
		     case 0x0A:  // 2640 "B20A" "SPKA" "S"
		         ins_setup_s();
		         break;
		     case 0x0B:  // 2650 "B20B" "IPK" "S"
		         ins_setup_s();
		         break;
		     case 0x0D:  // 2660 "B20D" "PTLB" "S"
		         ins_setup_s();
		         break;
		     case 0x10:  // 2670 "B210" "SPX" "S"
		         ins_setup_s();
		         break;
		     case 0x11:  // 2680 "B211" "STPX" "S"
		         ins_setup_s();
		         break;
		     case 0x12:  // 2690 "B212" "STAP" "S"
		         ins_setup_s();
		         break;
		     case 0x18:  // 2700 "B218" "PC" "S"
		         psw_check = false;
		     	 ins_setup_s();
		     	 push_pc_stack(psw_loc);
		     	 set_psw_loc(bd2_loc);
		         break;
		     case 0x19:  // 2710 "B219" "SAC" "S"
		         ins_setup_s();
		         break;
		     case 0x1A:  // 2720 "B21A" "CFC" "S"
		         ins_setup_s();
		         break;
		     case 0x21:  // 2730 "B221" "IPTE" "RRE"
		         ins_setup_rre();
		         break;
		     case 0x22:  // 2740 "B222" "IPM" "RRE"
		     	 psw_check = false;
		         ins_setup_rre();
		         int_work = (psw_cc_code[psw_cc] << 4) | psw_pgm_mask;
		         rv1 = reg.getInt(rf1+4) & ((-1) ^ 0x3f);
		         reg.putInt(rf1+4,rv1 | int_work);
		         break;
		     case 0x23:  // 2750 "B223" "IVSK" "RRE"
		         ins_setup_rre();
		         break;
		     case 0x24:  // 2760 "B224" "IAC" "RRE"
		         ins_setup_rre();
		         break;
		     case 0x25:  // 2770 "B225" "SSAR" "RRE"
		         ins_setup_rre();
		         break;
		     case 0x26:  // 2780 "B226" "EPAR" "RRE"
		         ins_setup_rre();
		         break;
		     case 0x27:  // 2790 "B227" "ESAR" "RRE"
		         ins_setup_rre();
		         break;
		     case 0x28:  // 2800 "B228" "PT" "RRE"
		         ins_setup_rre();
		         break;
		     case 0x29:  // 2810 "B229" "ISKE" "RRE"
		         ins_setup_rre();
		         break;
		     case 0x2A:  // 2820 "B22A" "RRBE" "RRE"
		         ins_setup_rre();
		         break;
		     case 0x2B:  // 2830 "B22B" "SSKE" "RRE"
		         ins_setup_rre();
		         break;
		     case 0x2C:  // 2840 "B22C" "TB" "RRE"
		         ins_setup_rre();
		         break;
		     case 0x2D:  // 2850 "B22D" "DXR" "RRE"
		     	 psw_check = false;
		         ins_setup_rre();
		         fp_rbdv1 = fp_get_bd_from_lh(fp_reg,rf1);
		         fp_rbdv2 = fp_get_bd_from_lh(fp_reg,rf2);
				 if (fp_rbdv2.compareTo(BigDecimal.ZERO) != 0){
				    fp_rbdv1 = fp_rbdv1.divide(fp_rbdv2,fp_bd_context).round(fp_x_context);
				 }
		         fp_put_bd(rf1,fp_lh_type,fp_rbdv1);
		         check_lh_div();
		         break;
		     case 0x2E:  // 2860 "B22E" "PGIN" "RRE"
		         ins_setup_rre();
		         break;
		     case 0x2F:  // 2870 "B22F" "PGOUT" "RRE"
		         ins_setup_rre();
		         break;
		     case 0x30:  // 2880 "B230" "CSCH" "S"
		         ins_setup_s();
		         break;
		     case 0x31:  // 2890 "B231" "HSCH" "S"
		         ins_setup_s();
		         break;
		     case 0x32:  // 2900 "B232" "MSCH" "S"
		         ins_setup_s();
		         break;
		     case 0x33:  // 2910 "B233" "SSCH" "S"
		         ins_setup_s();
		         break;
		     case 0x34:  // 2920 "B234" "STSCH" "S"
		         ins_setup_s();
		         break;
		     case 0x35:  // 2930 "B235" "TSCH" "S"
		         ins_setup_s();
		         break;
		     case 0x36:  // 2940 "B236" "TPI" "S"
		         ins_setup_s();
		         break;
		     case 0x37:  // 2950 "B237" "SAL" "S"
		         ins_setup_s();	         
		         break;
		     case 0x38:  // 2960 "B238" "RSCH" "S"
		         ins_setup_s();
		         break;
		     case 0x39:  // 2970 "B239" "STCRW" "S"
		         ins_setup_s();
		         break;
		     case 0x3A:  // 2980 "B23A" "STCPS" "S"
		         ins_setup_s();
		         break;
		     case 0x3B:  // 2990 "B23B" "RCHP" "S"
		         ins_setup_s();
		         break;
		     case 0x3C:  // 3000 "B23C" "SCHM" "S"
		         ins_setup_s();
		         break;
		     case 0x40:  // 3010 "B240" "BAKR" "RRE"
		     	 psw_check = false;
		         ins_setup_rre();
		         if (rf1 == 0){
		         	push_pc_stack(psw_loc);
		         } else {
		         	push_pc_stack(reg.getInt(rf1+4));
		         }
		         if (rf2 != 0){
		         	set_psw_loc(reg.getInt(rf2+4));
		         }
		         break;
		     case 0x41:  // 3020 "B241" "CKSM" "RRE"
		         ins_setup_rre();
		         break;
		     case 0x44:  // 3030 "B244" "SQDR" "RRE"
			 	 psw_check = false;
			     ins_setup_rre();
			     fp_rdv1 = Math.sqrt(fp_get_db_from_dh(fp_reg,rf2)); 
			     fp_put_db(rf1,fp_dh_type,fp_rdv1);
		         break;
		     case 0x45:  // 3040 "B245" "SQER" "RRE"
			 	 psw_check = false;
			     ins_setup_rre();
			     fp_rdv1 = Math.sqrt(fp_get_db_from_eh(fp_reg,rf2)); 
			     fp_put_db(rf1,fp_eh_type,fp_rdv1);
		         break;
		     case 0x46:  // 3050 "B246" "STURA" "RRE"
		         ins_setup_rre();
		         break;
		     case 0x47:  // 3060 "B247" "MSTA" "RRE"
		         ins_setup_rre();
		         break;
		     case 0x48:  // 3070 "B248" "PALB" "RRE"
		         ins_setup_rre();
		         break;
		     case 0x49:  // 3080 "B249" "EREG" "RRE"
		     	 psw_check = false;
		         ins_setup_rre();
		         if (cur_pc_stk_reg <= 0){  
		         	set_psw_check(psw_pic_stack);
		         	return;
		         }
		         int pc_stk_reg_base = cur_pc_stk_reg - reg_byte.length;
			     if (rf1 > rf3){
			     	while (rf1 < reg_byte.length){
			     		reg.putInt(rf1+4,pc_stk_reg.getInt(pc_stk_reg_base + rf1 + 4));
			     		rf1 = rf1 + 8;
			     	}
			     	rf1 = 0;
			     }
			     while (rf1 <= rf3){
		     		reg.putInt(rf1+4,pc_stk_reg.getInt(pc_stk_reg_base + rf1 + 4));
			     	rf1 = rf1 + 8;
			     }
		         break;
		     case 0x4A:  // 3090 "B24A" "ESTA" "RRE"
		         ins_setup_rre();
		         break;
		     case 0x4B:  // 3100 "B24B" "LURA" "RRE"
		         ins_setup_rre();
		         break;
		     case 0x4C:  // 3110 "B24C" "TAR" "RRE"
		         ins_setup_rre();
		         break;
		     case 0x4D:  // 3120 "B24D" "CPYA" "RRE"
		         ins_setup_rre();
		         break;
		     case 0x4E:  // 3130 "B24E" "SAR" "RRE"
		         ins_setup_rre();
		         break;
		     case 0x4F:  // 3140 "B24F" "EAR" "RRE"
		         ins_setup_rre();
		         break;
		     case 0x50:  // 3150 "B250" "CSP" "RRE"
		         ins_setup_rre();
		         break;
		     case 0x52:  // 3160 "B252" "MSR" "RRE"
		     	 psw_check = false;
		         ins_setup_rre();
		         reg.putInt(rf1+4,reg.getInt(rf1+4) * reg.getInt(rf2+4));
		         break;
		     case 0x54:  // 3170 "B254" "MVPG" "RRE"
		         ins_setup_rre();
		         break;
		     case 0x55:  // 3180 "B255" "MVST" "RRE"
		         ins_setup_rre();
		         break;
		     case 0x57:  // 3190 "B257" "CUSE" "RRE"
		         ins_setup_rre();
		         break;
		     case 0x58:  // 3200 "B258" "BSG" "RRE"
		         ins_setup_rre();
		         break;
		     case 0x5A:  // 3210 "B25A" "BSA" "RRE"
		         ins_setup_rre();
		         break;
		     case 0x5D:  // 3220 "B25D" "CLST" "RRE"
		         ins_setup_rre();
		         break;
		     case 0x5E:  // 3230 "B25E" "SRST" "RRE"
		         ins_setup_rre();
		         break;
		     case 0x63:  // 3240 "B263" "CMPSC" "RRE"
		         ins_setup_rre();
		         break;
		     case 0x76:  // 3250 "B276" "XSCH" "S"
		         ins_setup_s();
		         break;
		     case 0x77:  // 3260 "B277" "RP" "S"
		         ins_setup_s();
		         break;
		     case 0x78:  // 3270 "B278" "STCKE" "S"
		         ins_setup_s();
		         break;
		     case 0x79:  // 3280 "B279" "SACF" "S"
		         ins_setup_s();
		         break;
		     case 0x7D:  // 3290 "B27D" "STSI" "S"
		         ins_setup_s();
		         break;
		     case 0x99:  // 3300 "B299" "SRNM" "S"
		         ins_setup_s();
		         break;
		     case 0x9C:  // 3310 "B29C" "STFPC" "S"
		     	 psw_check = false;
		         ins_setup_s();
		         mem.putInt(bd2_loc,fp_fpc_reg | (fp_dxc << 8));
		         break;
		     case 0x9D:  // 3320 "B29D" "LFPC" "S"
		     	 psw_check = false;
		         ins_setup_s();
		         fp_fpc_reg = mem.getInt(bd2_loc);
		         fp_dxc = (fp_fpc_reg & 0xff00) >>> 8;
		         fp_fpc_reg = fp_fpc_reg & 0xffff00ff;
		         break;
		     case 0xA5:  // 3330 "B2A5" "TRE" "RRE"
		         ins_setup_rre();
		         break;
		     case 0xA6:  // 3340 "B2A6" "CUUTF" "RRE"
		         ins_setup_rre();
		         break;
		     case 0xA7:  // 3360 "B2A7" "CUTFU" "RRE"
		         ins_setup_rre();
		         break;
		     case 0xB1:  // 3380 "B2B1" "STFL" "S"
		         ins_setup_s();
		         break;
		     case 0xB2:  // 3390 "B2B2" "LPSWE" "S"
		         ins_setup_s();
		         break;
		     case 0xFF:  // 3400 "B2FF" "TRAP4" "S"
		         ins_setup_s();
		         break;
		     }
		     break;
		 case 0xB3:  
		     opcode2 = mem_byte[psw_loc+opcode2_offset_rre] & 0xff;
		     switch (opcode2){
		     case 0x00:  // 3410 "B300" "LPEBR" "RRE"
			 	 psw_check = false;
			     ins_setup_rre();
			     fp_rev1 = Math.abs(fp_get_eb_from_eb(fp_reg,rf2));
			 	 fp_put_eb(rf1,fp_eb_type,fp_rev1);
			     psw_cc = fp_get_eb_comp_cc(fp_rev1,0);
		         break;
		     case 0x01:  // 3420 "B301" "LNEBR" "RRE"
			 	 psw_check = false;
			     ins_setup_rre();
			     fp_rev1 =  Math.abs(fp_get_eb_from_eb(fp_reg,rf2));
			 	 if (fp_rev1 != 0){
			 	 	fp_rev1 = - fp_rev1;
			 	 }
			     fp_put_eb(rf1,fp_eb_type,fp_rev1);
			     psw_cc = fp_get_eb_comp_cc(fp_rev1,0);
		         break;
		     case 0x02:  // 3430 "B302" "LTEBR" "RRE"
			 	 psw_check = false;
			     ins_setup_rre();
			     fp_rev1 = fp_get_eb_from_eb(fp_reg,rf2);
			 	 fp_put_eb(rf1,fp_eb_type,fp_rev1);
			     psw_cc = fp_get_eb_comp_cc(fp_rev1,0);
		         break;
		     case 0x03:  // 3440 "B303" "LCEBR" "RRE"
			 	 psw_check = false;
			     ins_setup_rre();
			     fp_rev1 = fp_get_eb_from_eb(fp_reg,rf2);
			     if (fp_rev1 != 0){
			     	fp_rev1 = - fp_rev1;
			     }
			 	 fp_put_eb(rf1,fp_eb_type,fp_rev1);
			     psw_cc = fp_get_eb_comp_cc(fp_rev1,0);
		         break;
		     case 0x04:  // 3450 "B304" "LDEBR" "RRE"
			 	 psw_check = false;
			     ins_setup_rre();
			     fp_rdv1 = fp_get_db_from_eb(fp_reg,rf2);
			 	 fp_put_db(rf1,fp_db_type,fp_rdv1);
		         break;
		     case 0x05:  // 3460 "B305" "LXDBR" "RRE"
			 	 psw_check = false;
			     ins_setup_rre();
			     fp_rbdv1 = fp_get_bd_from_db(fp_reg,rf2);
			 	 fp_put_bd(rf1,fp_lb_type,fp_rbdv1);
		         break;
		     case 0x06:  // 3470 "B306" "LXEBR" "RRE"
			 	 psw_check = false;
			     ins_setup_rre();
			     fp_rbdv1 = fp_get_bd_from_eb(fp_reg,rf2);
			 	 fp_put_bd(rf1,fp_lb_type,fp_rbdv1);
		         break;
		     case 0x07:  // 3480 "B307" "MXDBR" "RRE"
		     	 psw_check = false;
		         ins_setup_rre();
		         fp_rbdv1 = fp_get_bd_from_db(fp_reg,rf1)
				      .multiply(fp_get_bd_from_db(fp_reg,rf2),fp_db_context)
					  .round(fp_x_context);
		         fp_put_bd(rf1,fp_lb_type,fp_rbdv1);
		         check_lb_mpy();
		         break;
		     case 0x08:  // 3490 "B308" "KEBR" "RRE"
		     	 psw_check = false;
		         ins_setup_rre();
		         fp_signal = true;
			     psw_cc = fp_get_eb_comp_cc(
		     		     fp_get_eb_from_eb(fp_reg,rf1),
						 fp_get_eb_from_eb(fp_reg,rf2));	
			     break;
		     case 0x09:  // 3500 "B309" "CEBR" "RRE"
		     	 psw_check = false;
		         ins_setup_rre();
			     psw_cc = fp_get_eb_comp_cc(
		     		     fp_get_eb_from_eb(fp_reg,rf1),
						 fp_get_eb_from_eb(fp_reg,rf2)); 
		         break;
		     case 0x0A:  // 3510 "B30A" "AEBR" "RRE"
			 	 psw_check = false;
			     ins_setup_rre();
			     fp_rev1 = fp_get_eb_from_eb(fp_reg,rf1) 
				         + fp_get_eb_from_eb(fp_reg,rf2);
			     fp_put_eb(rf1,fp_eb_type,fp_rev1);
			     psw_cc = fp_get_eb_add_sub_cc();
		         break;
		     case 0x0B:  // 3520 "B30B" "SEBR" "RRE"
			 	 psw_check = false;
			     ins_setup_rre();
			     fp_rev1 = fp_get_eb_from_eb(fp_reg,rf1) 
				         - fp_get_eb_from_eb(fp_reg,rf2);
			     fp_put_eb(rf1,fp_eb_type,fp_rev1);
			     psw_cc = fp_get_eb_add_sub_cc();
		         break;
		     case 0x0C:  // 3530 "B30C" "MDEBR" "RRE"
			 	 psw_check = false;
			     ins_setup_rre();
			     fp_rdv1 = fp_get_db_from_eb(fp_reg,rf1) 
				         * fp_get_db_from_eb(fp_reg,rf2);
			     fp_put_db(rf1,fp_db_type,fp_rdv1);
		         check_db_mpy();			     
		         break;
		     case 0x0D:  // 3540 "B30D" "DEBR" "RRE"
			 	 psw_check = false;
			     ins_setup_rre();
			     fp_rev1 = fp_get_eb_from_eb(fp_reg,rf1); 
				 fp_rev2 = fp_get_eb_from_eb(fp_reg,rf2);
				 fp_rev1 = fp_rev1 / fp_rev2;
			     fp_put_eb(rf1,fp_eb_type,fp_rev1);
		         check_eb_div();
		         break;
		     case 0x0E:  // 3550 "B30E" "MAEBR" "RRF"
		     	 psw_check = false;
		         ins_setup_rrf();
		         fp_rev1 = fp_get_eb_from_eb(fp_reg,rf1)
				         + fp_get_eb_from_eb(fp_reg,rf2)
						 * fp_get_eb_from_eb(fp_reg,rf3);
		         fp_put_eb(rf1,fp_eb_type,fp_rev1);
		         check_eb_mpy();
		         break;
		     case 0x0F:  // 3560 "B30F" "MSEBR" "RRF"
		     	 psw_check = false;
		         ins_setup_rrf();
		         fp_rev1 = fp_get_eb_from_eb(fp_reg,rf1)
				         - fp_get_eb_from_eb(fp_reg,rf2)
						 * fp_get_eb_from_eb(fp_reg,rf3);
		         fp_put_eb(rf1,fp_eb_type,fp_rev1);
		         check_eb_mpy();
		         break;
		     case 0x10:  // 3570 "B310" "LPDBR" "RRE"
			 	 psw_check = false;
			     ins_setup_rre();
			     fp_rdv1 = Math.abs(fp_get_db_from_db(fp_reg,rf2));
			 	 fp_put_db(rf1,fp_db_type,fp_rdv1);
			     psw_cc = fp_get_db_comp_cc(fp_rdv1,0);
		         break;
		     case 0x11:  // 3580 "B311" "LNDBR" "RRE"
			 	 psw_check = false;
			     ins_setup_rre();
			     fp_rdv1 = - Math.abs(fp_get_db_from_db(fp_reg,rf2));
			 	 fp_put_db(rf1,fp_db_type,fp_rdv1);
			     psw_cc = fp_get_db_comp_cc(fp_rdv1,0);
		         break;
		     case 0x12:  // 3590 "B312" "LTDBR" "RRE"
			 	 psw_check = false;
			     ins_setup_rre();
			     fp_rdv1 = fp_get_db_from_db(fp_reg,rf2);
			 	 fp_put_db(rf1,fp_db_type,fp_rdv1);
			     psw_cc = fp_get_db_comp_cc(fp_rdv1,0);
		         break;
		     case 0x13:  // 3600 "B313" "LCDBR" "RRE"
			 	 psw_check = false;
			     ins_setup_rre();
			     fp_rdv1 =  - fp_get_db_from_db(fp_reg,rf2);
			 	 fp_put_db(rf1,fp_db_type,fp_rdv1);
			     psw_cc = fp_get_db_comp_cc(fp_rdv1,0);
		         break;
		     case 0x14:  // 3610 "B314" "SQEBR" "RRE"
			 	 psw_check = false;
			     ins_setup_rre();
			     fp_rev1 = (float) Math.sqrt(fp_get_db_from_eb(fp_reg,rf2)); 
			     fp_put_eb(rf1,fp_eb_type,fp_rev1);
		         break;
		     case 0x15:  // 3620 "B315" "SQDBR" "RRE"
			 	 psw_check = false;
			     ins_setup_rre();
			     fp_rdv1 = Math.sqrt(fp_get_db_from_db(fp_reg,rf2)); 
			     fp_put_db(rf1,fp_db_type,fp_rdv1);
		         break;
		     case 0x16:  // 3630 "B316" "SQXBR" "RRE"
		     	 psw_check = false;
		         ins_setup_rre();
		         fp_rbdv2 = fp_get_bd_from_lb(fp_reg,rf2);
		         fp_get_bd_sqrt();
		         fp_put_bd(rf1,fp_lb_type,fp_rbdv1);
		         break;
		     case 0x17:  // 3640 "B317" "MEEBR" "RRE"
			 	 psw_check = false;
			     ins_setup_rre();
			     fp_rev1 = fp_get_eb_from_eb(fp_reg,rf1) 
				         * fp_get_eb_from_eb(fp_reg,rf2);
			     fp_put_eb(rf1,fp_eb_type,fp_rev1);
		         check_eh_mpy();
		         break;
		     case 0x18:  // 3650 "B318" "KDBR" "RRE"
		     	 psw_check = false;
		         ins_setup_rre();
		         fp_signal = true;
			     psw_cc = fp_get_db_comp_cc(
		     		     fp_get_db_from_db(fp_reg,rf1),
						 fp_get_db_from_db(fp_reg,rf2));
			     break;
		     case 0x19:  // 3660 "B319" "CDBR" "RRE"
		     	 psw_check = false;
		         ins_setup_rre();
			     psw_cc = fp_get_db_comp_cc(
		     		     fp_get_db_from_db(fp_reg,rf1),
						 fp_get_db_from_db(fp_reg,rf2)); 
		         break;
		     case 0x1A:  // 3670 "B31A" "ADBR" "RRE"
			 	 psw_check = false;
			     ins_setup_rre();
			     fp_rdv1 = fp_get_db_from_db(fp_reg,rf1)
				         + fp_get_db_from_db(fp_reg,rf2);
			     fp_put_db(rf1,fp_db_type,fp_rdv1);
			     psw_cc = fp_get_db_add_sub_cc();
		         break;
		     case 0x1B:  // 3680 "B31B" "SDBR" "RRE"
			 	 psw_check = false;
			     ins_setup_rre();
			     fp_rdv1 = fp_get_db_from_db(fp_reg,rf1)
				         - fp_get_db_from_db(fp_reg,rf2);
			     fp_put_db(rf1,fp_db_type,fp_rdv1);
			     psw_cc = fp_get_db_add_sub_cc();
		         break;
		     case 0x1C:  // 3690 "B31C" "MDBR" "RRE"
			 	 psw_check = false;
			     ins_setup_rre();
			     fp_rdv1 = fp_get_db_from_db(fp_reg,rf1)
				         * fp_get_db_from_db(fp_reg,rf2);
			     fp_put_db(rf1,fp_db_type,fp_rdv1);
		         check_db_mpy();
		         break;
		     case 0x1D:  // 3700 "B31D" "DDBR" "RRE"
			 	 psw_check = false;
			     ins_setup_rre();
			     fp_rdv1 = fp_get_db_from_db(fp_reg,rf1);
				 fp_rdv2 = fp_get_db_from_db(fp_reg,rf2);
				 fp_rdv1 = fp_rdv1 / fp_rdv2;
			     fp_put_db(rf1,fp_db_type,fp_rdv1);
		         check_db_div();
		         break;
		     case 0x1E:  // 3710 "B31E" "MADBR" "RRF"
		     	 psw_check = false;
		         ins_setup_rrf();
		         fp_rdv1 = fp_get_db_from_db(fp_reg,rf1)
				         + fp_get_db_from_db(fp_reg,rf2)
						 * fp_get_db_from_db(fp_reg,rf3);
		         fp_put_db(rf1,fp_db_type,fp_rdv1);
		         check_db_mpy();
		         break;
		     case 0x1F:  // 3720 "B31F" "MSDBR" "RRF"
		     	 psw_check = false;
		         ins_setup_rrf();
		         fp_rdv1 = fp_get_db_from_db(fp_reg,rf1)
				         - fp_get_db_from_db(fp_reg,rf2)
						 * fp_get_db_from_db(fp_reg,rf3);
		         fp_put_db(rf1,fp_db_type,fp_rdv1);
		         check_db_mpy();
		         break;
		     case 0x24:  // 3730 "B324" "LDER" "RRE"
		     	 psw_check = false;
		         ins_setup_rre();
		         fp_load_reg(fp_dh_type,fp_reg,rf2,fp_eh_type);
		         break;
		     case 0x25:  // 3740 "B325" "LXDR" "RRE"
		     	 psw_check = false;
		         ins_setup_rre();
		         fp_load_reg(fp_lh_type,fp_reg,rf2,fp_dh_type);
		         break;
		     case 0x26:  // 3750 "B326" "LXER" "RRE"
		     	 psw_check = false;
		         ins_setup_rre();
		         fp_load_reg(fp_lh_type,fp_reg,rf2,fp_eh_type);
		         break;
		     case 0x2E:  // 3760 "B32E" "MAER" "RRF"
		     	 psw_check = false;
		         ins_setup_rrf();
		         fp_rdv1 = fp_get_db_from_eh(fp_reg,rf1)
				         + fp_get_db_from_eh(fp_reg,rf2)
						 * fp_get_db_from_eh(fp_reg,rf3);
		         fp_put_db(rf1,fp_eh_type,fp_rdv1);
		         check_eh_mpy();
		         break;
		     case 0x2F:  // 3770 "B32F" "MSER" "RRF"
		     	 psw_check = false;
		         ins_setup_rrf();
		         fp_rdv1 = fp_get_db_from_eh(fp_reg,rf1)
				         - fp_get_db_from_eh(fp_reg,rf2)
						 * fp_get_db_from_eh(fp_reg,rf3);
		         fp_put_db(rf1,fp_eh_type,fp_rdv1);
		         check_eh_mpy();
		         break;
		     case 0x36:  // 3780 "B336" "SQXR" "RRE"
		     	 psw_check = false;
		         ins_setup_rre();
		         fp_rbdv2 = fp_get_bd_from_lh(fp_reg,rf2);
		         fp_get_bd_sqrt();
		         fp_put_bd(rf1,fp_lh_type,fp_rbdv1);
		         break;
		     case 0x37:  // 3790 "B337" "MEER" "RRE"
			 	 psw_check = false;
			     ins_setup_rre();
			     fp_rdv1 = fp_get_db_from_eh(fp_reg,rf1) 
				         * fp_get_db_from_eh(fp_reg,rf2);
			     fp_put_db(rf1,fp_eh_type,fp_rdv1);
		         check_eh_mpy();
		         break;
		     case 0x3E:  // 3800 "B33E" "MADR" "RRF"
		     	 psw_check = false;
		         ins_setup_rrf();
		         fp_rdv1 = fp_get_db_from_dh(fp_reg,rf1)
				         + fp_get_db_from_dh(fp_reg,rf2)
						 * fp_get_db_from_dh(fp_reg,rf3);
		         fp_put_db(rf1,fp_dh_type,fp_rdv1);
		         check_dh_mpy();
		         break;
		     case 0x3F:  // 3810 "B33F" "MSDR" "RRF"
		     	 psw_check = false;
		         ins_setup_rrf();
		         fp_rdv1 = fp_get_db_from_dh(fp_reg,rf1)
				         - fp_get_db_from_dh(fp_reg,rf2)
						 * fp_get_db_from_dh(fp_reg,rf3);
		         fp_put_db(rf1,fp_dh_type,fp_rdv1);
		         check_dh_mpy();
		         break;
		     case 0x40:  // 3820 "B340" "LPXBR" "RRE"
			 	 psw_check = false;
			     ins_setup_rre();
			     fp_rbdv1 = fp_get_bd_from_lb(fp_reg,rf2).abs();
			 	 fp_put_bd(rf1,fp_lb_type,fp_rbdv1);
			     psw_cc = fp_get_lb_comp_cc(fp_rbdv1,BigDecimal.ZERO);
		         break;
		     case 0x41:  // 3830 "B341" "LNXBR" "RRE"
			 	 psw_check = false;
			     ins_setup_rre();
			     fp_rbdv1 = fp_get_bd_from_lb(fp_reg,rf2).abs().negate();
			 	 fp_put_bd(rf1,fp_lb_type,fp_rbdv1);
			     psw_cc = fp_get_lb_comp_cc(fp_rbdv1,BigDecimal.ZERO);
		         break;
		     case 0x42:  // 3840 "B342" "LTXBR" "RRE"
			 	 psw_check = false;
			     ins_setup_rre();
			     fp_rbdv1 = fp_get_bd_from_lb(fp_reg,rf2);
			 	 fp_put_bd(rf1,fp_lb_type,fp_rbdv1);
			     psw_cc = fp_get_lb_comp_cc(fp_rbdv1,BigDecimal.ZERO);
		         break;
		     case 0x43:  // 3850 "B343" "LCXBR" "RRE"
			 	 psw_check = false;
			     ins_setup_rre();
			     fp_rbdv1 = fp_get_bd_from_lb(fp_reg,rf2).negate();
			 	 fp_put_bd(rf1,fp_lb_type,fp_rbdv1);
			     psw_cc = fp_get_lb_comp_cc(fp_rbdv1,BigDecimal.ZERO);
		         break;
		     case 0x44:  // 3860 "B344" "LEDBR" "RRE"
		     	 psw_check = false;
		         ins_setup_rre();
		         fp_load_reg(fp_eb_type,fp_reg,rf2,fp_db_type);
		         break;
		     case 0x45:  // 3870 "B345" "LDXBR" "RRE"
		     	 psw_check = false;
		         ins_setup_rre();
		         fp_load_reg(fp_db_type,fp_reg,rf2,fp_lb_type);
		         break;
		     case 0x46:  // 3880 "B346" "LEXBR" "RRE"
		     	 psw_check = false;
		         ins_setup_rre();
		         fp_load_reg(fp_eb_type,fp_reg,rf2,fp_lb_type);
		         break;
		     case 0x47:  // 3890 "B347" "FIXBR" "RRF"
			 	 psw_check = false;
			 	 ins_setup_rrf();
		         if (mf3 != 0){ // only def. rounding for now
		         	set_psw_check(psw_pic_spec); 
		         }
		     	 fp_rbdv1 = fp_get_bd_from_lb(fp_reg,rf2);
		         fp_rbdv1 = fp_rbdv1.subtract(fp_rbdv1.remainder(BigDecimal.ONE,fp_bd_context));
		         fp_put_bd(rf1,fp_lb_type,fp_rbdv1);
		         break;
		     case 0x48:  // 3900 "B348" "KXBR" "RRE"
		     	 psw_check = false;
		         ins_setup_rre();
		         fp_signal = true;
			     psw_cc = fp_get_lb_comp_cc(
		     		     fp_get_bd_from_lb(fp_reg,rf1),
						 fp_get_bd_from_lb(fp_reg,rf2));
				 break;
		     case 0x49:  // 3910 "B349" "CXBR" "RRE"
		     	 psw_check = false;
		         ins_setup_rre();
			     psw_cc = fp_get_lb_comp_cc(
		     		     fp_get_bd_from_lb(fp_reg,rf1),
						 fp_get_bd_from_lb(fp_reg,rf2));
		         break;
		     case 0x4A:  // 3920 "B34A" "AXBR" "RRE"
			 	 psw_check = false;
			     ins_setup_rre();
			     fp_rbdv1 = fp_get_bd_from_lb(fp_reg,rf1);
				 fp_rbdv2 = fp_get_bd_from_lb(fp_reg,rf2);
				 fp_rbdv1 = fp_rbdv1.add(fp_rbdv2,fp_x_context).round(fp_x_context);
			     fp_put_bd(rf1,fp_lb_type,fp_rbdv1);
			     psw_cc = fp_get_lb_add_sub_cc();
		         break;
		     case 0x4B:  // 3930 "B34B" "SXBR" "RRE"
			 	 psw_check = false;
			     ins_setup_rre();
			     fp_rbdv1 = fp_get_bd_from_lb(fp_reg,rf1)
				            .subtract(fp_get_bd_from_lb(fp_reg,rf2),fp_bd_context);
			     fp_put_bd(rf1,fp_lb_type,fp_rbdv1);
			     psw_cc = fp_get_lb_add_sub_cc();
		         break;
		     case 0x4C:  // 3940 "B34C" "MXBR" "RRE"
			 	 psw_check = false;
			     ins_setup_rre();
			     fp_rbdv1 = fp_get_bd_from_lb(fp_reg,rf1)
				            .multiply(fp_get_bd_from_lb(fp_reg,rf2),fp_bd_context)
							.round(fp_x_context);
			     fp_put_bd(rf1,fp_lb_type,fp_rbdv1);
		         check_lb_mpy();
		         break;
		     case 0x4D:  // 3950 "B34D" "DXBR" "RRE"
			 	 psw_check = false;
			     ins_setup_rre();
			     fp_rbdv1 = fp_get_bd_from_lb(fp_reg,rf1);
				 fp_rbdv2 = fp_get_bd_from_lb(fp_reg,rf2);
				 if (fp_rbdv2.compareTo(BigDecimal.ZERO) != 0){
				    fp_rbdv1 = fp_rbdv1.divide(fp_rbdv2,fp_bd_context).round(fp_x_context);
				 }
				 fp_put_bd(rf1,fp_lb_type,fp_rbdv1);
		         check_lb_div();
		         break;
		     case 0x50:  // 3960 "B350" "TBEDR" "RRF"
		     	 psw_check = false;
		         ins_setup_rrf();
		         fp_rev1 = (float)fp_get_db_from_dh(fp_reg,rf2);
		         fp_put_eb(rf1,fp_eb_type,fp_rev1);
		         psw_cc = fp_get_eb_comp_cc(fp_rev1,0);
		         break;
		     case 0x51:  // 3970 "B351" "TBDR" "RRF"
		     	 psw_check = false;
		         ins_setup_rrf();
		         fp_rdv1 = fp_get_db_from_dh(fp_reg,rf2);
		         fp_put_db(rf1,fp_db_type,fp_rdv1);
		         psw_cc = fp_get_db_comp_cc(fp_rdv1,0);
		         break;
		     case 0x53:  // 3980 "B353" "DIEBR" "RR4"
		     	 psw_check = false;
		         ins_setup_rr4();
		         if (mf4 != 0){ // only def. rounding for now
		         	set_psw_check(psw_pic_spec); 
		         }
		         fp_rdv1 = fp_get_db_from_eb(fp_reg,rf1);
		         fp_rdv2 = fp_get_db_from_eb(fp_reg,rf2);
                 fp_rdv3 = (long)(fp_rdv1 / fp_rdv2);
                 fp_rdv4 = fp_rdv1 - fp_rdv3 * fp_rdv2;
                 fp_put_eb(rf3,fp_eb_type,(float)fp_rdv3);
		         fp_put_eb(rf1,fp_eb_type,(float)fp_rdv4); 
		         psw_cc = fp_get_di_cc();
		         break;
		     case 0x57:  // 3990 "B357" "FIEBR" "RRF"
		     	 psw_check = false;
		         ins_setup_rrf();
		         if (mf3 != 0){ // only def. rounding for now
		         	set_psw_check(psw_pic_spec); 
		         }
		         fp_rev1 = (long)fp_get_db_from_eb(fp_reg,rf2);
		         fp_put_eb(rf1,fp_eb_type,fp_rev1);      
		         break;
		     case 0x58:  // 4000 "B358" "THDER" "RRE"
		     	 psw_check = false;
		         ins_setup_rre();
		         fp_rdv1 = fp_get_db_from_eb(fp_reg,rf2);
		         fp_put_db(rf1,fp_dh_type,fp_rdv1);
		         psw_cc = fp_get_db_comp_cc(fp_rdv1,0);
		         break;
		     case 0x59:  // 4010 "B359" "THDR" "RRE"
		     	 psw_check = false;
		         ins_setup_rre();
		         fp_rdv1 = fp_get_db_from_db(fp_reg,rf2);
		         fp_put_db(rf1,fp_dh_type,fp_rdv1);
		         psw_cc = fp_get_db_comp_cc(fp_rdv1,0);
		         break;
		     case 0x5B:  // 4020 "B35B" "DIDBR" "RR4"
		     	 psw_check = false;
		         ins_setup_rr4();
		         if (mf4 != 0){ // only def. rounding for now
		         	set_psw_check(psw_pic_spec); 
		         }
		         fp_rdv1 = fp_get_db_from_db(fp_reg,rf1);
		         fp_rdv2 = fp_get_db_from_db(fp_reg,rf2);
                 fp_rdv3 = (long)(fp_rdv1 / fp_rdv2);
                 fp_rdv4 = fp_rdv1 - fp_rdv3 * fp_rdv2;
                 fp_put_db(rf3,fp_db_type,fp_rdv3);
		         fp_put_db(rf1,fp_db_type,fp_rdv4); 
		         psw_cc = fp_get_di_cc();
		         break;
		     case 0x5F:  // 4030 "B35F" "FIDBR" "RRF"
		     	 psw_check = false;
		         ins_setup_rrf();
		         if (mf3 != 0){ // only def. rounding for now
		         	set_psw_check(psw_pic_spec); 
		         }
		         fp_rdv1 = (long) fp_get_db_from_db(fp_reg,rf2);
		         fp_put_db(rf1,fp_db_type,fp_rdv1); 
		         break;
		     case 0x60:  // 4040 "B360" "LPXR" "RRE"
			 	 psw_check = false;
			     ins_setup_rre();
			     fp_rbdv1 = fp_get_bd_from_lh(fp_reg,rf2).abs();
			 	 fp_put_bd(rf1,fp_lh_type,fp_rbdv1);
			     psw_cc = fp_get_lh_comp_cc(fp_rbdv1,BigDecimal.ZERO);
		         break;
		     case 0x61:  // 4050 "B361" "LNXR" "RRE"
			 	 psw_check = false;
			     ins_setup_rre();
			     fp_rbdv1 = fp_get_bd_from_lh(fp_reg,rf2).abs().negate();
			 	 fp_put_bd(rf1,fp_lh_type,fp_rbdv1);
			     psw_cc = fp_get_lh_comp_cc(fp_rbdv1,BigDecimal.ZERO);
		         break;
		     case 0x62:  // 4060 "B362" "LTXR" "RRE"
			 	 psw_check = false;
			     ins_setup_rre();
			     fp_rbdv1 = fp_get_bd_from_lh(fp_reg,rf2);
			 	 fp_put_bd(rf1,fp_lh_type,fp_rbdv1);
			     psw_cc = fp_get_lh_comp_cc(fp_rbdv1,BigDecimal.ZERO);

		         break;
		     case 0x63:  // 4070 "B363" "LCXR" "RRE"
			 	 psw_check = false;
			     ins_setup_rre();
			     fp_rbdv1 = fp_get_bd_from_lh(fp_reg,rf2).negate();
			 	 fp_put_bd(rf1,fp_lh_type,fp_rbdv1);
			     psw_cc = fp_get_lh_comp_cc(fp_rbdv1,BigDecimal.ZERO);

		         break;
		     case 0x65:  // 4080 "B365" "LXR" "RRE"
			 	 psw_check = false;
			 	 ins_setup_rre();
	             fp_load_reg(fp_lh_type,fp_reg,rf2,fp_lh_type);
		         break;
		     case 0x66:  // 4090 "B366" "LEXR" "RRE"
			 	 psw_check = false;
			 	 ins_setup_rre();
	             fp_load_reg(fp_eh_type,fp_reg,rf2,fp_lh_type);
		         break;
		     case 0x67:  // 4100 "B367" "FIXR" "RRE"
			 	 psw_check = false;
			 	 ins_setup_rre();
		     	 fp_rbdv1 = fp_get_bd_from_lh(fp_reg,rf2);
		         fp_rbdv1 = fp_rbdv1.subtract(fp_rbdv1.remainder(BigDecimal.ONE,fp_bd_context));
		         fp_put_bd(rf1,fp_lh_type,fp_rbdv1);
		         break;
		     case 0x69:  // 4110 "B369" "CXR" "RRE"
		     	 psw_check = false;
		         ins_setup_rre();
			     psw_cc = fp_get_lh_comp_cc(
		     		     fp_get_bd_from_lh(fp_reg,rf1),
						 fp_get_bd_from_lh(fp_reg,rf2)); 
		         break;
		     case 0x74:  // 4120 "B374" "LZER" "RRE"
		     	 psw_check = false;
		         ins_setup_rre();
		         fp_put_db(rf1,fp_eh_type,0);
		         break;
		     case 0x75:  // 4130 "B375" "LZDR" "RRE"
		     	 psw_check = false;
		         ins_setup_rre();
		         fp_put_db(rf1,fp_dh_type,0);
		         break;
		     case 0x76:  // 4140 "B376" "LZXR" "RRE"
		     	 psw_check = false;
		         ins_setup_rre();
		         fp_put_bd(rf1,fp_lh_type,BigDecimal.ZERO);
		         break;
		     case 0x77:  // 4150 "B377" "FIER" "RRE"
		     	 psw_check = false;
		         ins_setup_rre();
		         fp_rdv1 = (long)fp_get_db_from_eh(fp_reg,rf2);
		         fp_put_db(rf1,fp_eh_type,fp_rdv1); 
		         break;
		     case 0x7F:  // 4160 "B37F" "FIDR" "RRE"
		     	 psw_check = false;
		         ins_setup_rre();
		         fp_rdv1 = (long)fp_get_db_from_dh(fp_reg,rf2);
		         fp_put_db(rf1,fp_dh_type,fp_rdv1); 
		         break;
		     case 0x84:  // 4170 "B384" "SFPC" "RRE"
		     	 psw_check = false;
		         ins_setup_rre();
		         fp_fpc_reg = reg.getInt(rf1+4);
		         fp_dxc = (fp_fpc_reg & 0xff00) >>> 8;
		         fp_fpc_reg = fp_fpc_reg & 0xffff00ff;
		         break;
		     case 0x8C:  // 4180 "B38C" "EFPC" "RRE"
		     	 psw_check = false;
		         ins_setup_rre();
		         reg.putInt(rf1+4,fp_fpc_reg | (fp_dxc << 8));
		         break;
		     case 0x94:  // 4190 "B394" "CEFBR" "RRE"
		     	 psw_check = false;
		         ins_setup_rre();
		         fp_put_eb(rf1,fp_eb_type,(float)reg.getInt(rf2+4));
		         break;
		     case 0x95:  // 4200 "B395" "CDFBR" "RRE"
		     	 psw_check = false;
		         ins_setup_rre();
		         fp_put_db(rf1,fp_db_type,(double)reg.getInt(rf2+4));
		         break;
		     case 0x96:  // 4210 "B396" "CXFBR" "RRE"
		     	 psw_check = false;
		         ins_setup_rre();
		         fp_put_bd(rf1,fp_eb_type,BigDecimal.valueOf(reg.getInt(rf2+4)));
		         break;
		     case 0x98:  // 4220 "B398" "CFEBR" "RRF"
		         psw_check = false;
		     	 ins_setup_rrf();
		     	 if (mf3 != 0){
		     	 	set_psw_check(psw_pic_spec); // only def. rounding for now
		     	 }
		     	 rv1 = (int) fp_get_eb_from_eb(fp_reg,rf2);
		     	 reg.putInt(rf1+4,rv1);
		     	 psw_cc = get_int_comp_cc(rv1,0);
		         break;
		     case 0x99:  // 4230 "B399" "CFDBR" "RRF"
		         psw_check = false;
		     	 ins_setup_rrf();
		     	 if (mf3 != 0){
		     	 	set_psw_check(psw_pic_spec); // only def. rounding for now
		     	 }
		     	 rv1 = (int)fp_get_db_from_db(fp_reg,rf2);
		     	 reg.putInt(rf1+4,rv1);
		     	 psw_cc = get_int_comp_cc(rv1,0);
		         break;
		     case 0x9A:  // 4240 "B39A" "CFXBR" "RRF"
		         psw_check = false;
		     	 ins_setup_rrf();
		     	 if (mf3 != 0){
		     	 	set_psw_check(psw_pic_spec); // only def. rounding for now
		     	 }
		     	 rv1 = fp_get_bd_from_lb(fp_reg,rf2).intValue();
		     	 reg.putInt(rf1+4,rv1);
		     	 psw_cc = get_int_comp_cc(rv1,0);
		         break;
		     case 0xA4:  // 4250 "B3A4" "CEGBR" "RRE"
		     	 psw_check = false;
		         ins_setup_rre();
		         fp_put_eb(rf1,fp_eb_type,(float)reg.getLong(rf2));
		         break;
		     case 0xA5:  // 4260 "B3A5" "CDGBR" "RRE"
		     	 psw_check = false;
		         ins_setup_rre();
		         fp_put_db(rf1,fp_db_type,(double)reg.getLong(rf2));
		         break;
		     case 0xA6:  // 4270 "B3A6" "CXGBR" "RRE"
		     	 psw_check = false;
		         ins_setup_rre();
		         fp_put_bd(rf1,fp_lb_type,BigDecimal.valueOf((double)reg.getLong(rf2)));
		         break;
		     case 0xA8:  // 4280 "B3A8" "CGEBR" "RRF"
		         psw_check = false;
		     	 ins_setup_rrf();
		     	 if (mf3 != 0){
		     	 	set_psw_check(psw_pic_spec); // only def. rounding for now
		     	 }
		     	 rlv1 = (long) fp_get_eb_from_eb(fp_reg,rf2);
		     	 reg.putLong(rf1,rlv1);
		     	 psw_cc = get_long_comp_cc(rlv1,0);
		         break;
		     case 0xA9:  // 4290 "B3A9" "CGDBR" "RRF"
		         psw_check = false;
		     	 ins_setup_rrf();
		     	 if (mf3 != 0){
		     	 	set_psw_check(psw_pic_spec); // only def. rounding for now
		     	 }
		     	 rlv1 = (long) fp_get_db_from_db(fp_reg,rf2);
		     	 reg.putLong(rf1,rlv1);
		     	 psw_cc = get_long_comp_cc(rlv1,0);
		         break;
		     case 0xAA:  // 4300 "B3AA" "CGXBR" "RRF"
		         psw_check = false;
		     	 ins_setup_rrf();
		     	 if (mf3 != 0){
		     	 	set_psw_check(psw_pic_spec); // only def. rounding for now
		     	 }
		     	 rlv1 = fp_get_bd_from_lb(fp_reg,rf2).longValue();
		     	 reg.putLong(rf1,rlv1);
		     	 psw_cc = get_long_comp_cc(rlv1,0);
		         break;
		     case 0xB4:  // 4310 "B3B4" "CEFR" "RRE"
		     	 psw_check = false;
		         ins_setup_rre();
		         fp_put_db(rf1,fp_eh_type,(float)reg.getInt(rf2+4));
		         break;
		     case 0xB5:  // 4320 "B3B5" "CDFR" "RRE"
		     	 psw_check = false;
		         ins_setup_rre();
		     	 fp_put_db(rf1,fp_dh_type,(float)reg.getInt(rf2+4));
		         break;
		     case 0xB6:  // 4330 "B3B6" "CXFR" "RRE"
		     	 psw_check = false;
		         ins_setup_rre();
		     	 fp_put_bd(rf1,fp_lh_type,BigDecimal.valueOf(reg.getInt(rf2+4)));
		         break;
		     case 0xB8:  // 4340 "B3B8" "CFER" "RRF"
		         psw_check = false;
		     	 ins_setup_rrf();
		     	 if (mf3 != 0){
		     	 	set_psw_check(psw_pic_spec); // only def. rounding for now
		     	 }
		     	 rv1 = (int) fp_get_db_from_eh(fp_reg,rf2);
		     	 reg.putInt(rf1+4,rv1);
		     	 psw_cc = get_int_comp_cc(rv1,0);
		         break;
		     case 0xB9:  // 4350 "B3B9" "CFDR" "RRF"
		         psw_check = false;
		     	 ins_setup_rrf();
		     	 if (mf3 != 0){
		     	 	set_psw_check(psw_pic_spec); // only def. rounding for now
		     	 }
		     	 rv1 = (int) fp_get_db_from_dh(fp_reg,rf2);
		     	 reg.putInt(rf1+4,rv1);
		     	 psw_cc = get_int_comp_cc(rv1,0);
		         break;
		     case 0xBA:  // 4360 "B3BA" "CFXR" "RRF"
		         psw_check = false;
		     	 ins_setup_rrf();
		     	 if (mf3 != 0){
		     	 	set_psw_check(psw_pic_spec); // only def. rounding for now
		     	 }
		     	 rv1 = fp_get_bd_from_lh(fp_reg,rf2).intValue();
		     	 reg.putInt(rf1+4,rv1);
		     	 psw_cc = get_int_comp_cc(rv1,0);
		         break;
		     case 0xC4:  // 4370 "B3C4" "CEGR" "RRE"
		     	 psw_check = false;
		     	 ins_setup_rre();
		         fp_put_db(rf1,fp_eh_type,(float)reg.getLong(rf2));
		         break;
		     case 0xC5:  // 4380 "B3C5" "CDGR" "RRE"
		     	 psw_check = false;
		     	 ins_setup_rre();
		         fp_put_db(rf1,fp_dh_type,(double)reg.getLong(rf2));
		         break;
		     case 0xC6:  // 4390 "B3C6" "CXGR" "RRE"
		     	 psw_check = false;
		     	 ins_setup_rre();
		         fp_put_bd(rf1,fp_lh_type,BigDecimal.valueOf(reg.getLong(rf2)));
		         break;
		     case 0xC8:  // 4400 "B3C8" "CGER" "RRF"
		         psw_check = false;
		     	 ins_setup_rrf();
		     	 if (mf3 != 0){
		     	 	set_psw_check(psw_pic_spec); // only def. rounding for now
		     	 }
		     	 rlv1 = (long) fp_get_db_from_eh(fp_reg,rf2);
		     	 reg.putLong(rf1,rlv1);
		     	 psw_cc = get_long_comp_cc(rlv1,0);
		         break;
		     case 0xC9:  // 4410 "B3C9" "CGDR" "RRF"
		         psw_check = false;
		     	 ins_setup_rrf();
		     	 if (mf3 != 0){
		     	 	set_psw_check(psw_pic_spec); // only def. rounding for now
		     	 }
		     	 rlv1 = (long) fp_get_db_from_dh(fp_reg,rf2);
		     	 reg.putLong(rf1,rlv1);
		     	 psw_cc = get_long_comp_cc(rlv1,0);
		         break;
		     case 0xCA:  // 4420 "B3CA" "CGXR" "RRF"
		         psw_check = false;
		     	 ins_setup_rrf();
		     	 if (mf3 != 0){
		     	 	set_psw_check(psw_pic_spec); // only def. rounding for now
		     	 }
		     	 rlv1 = fp_get_bd_from_lh(fp_reg,rf2).longValue();
		     	 reg.putLong(rf1,rlv1);
		     	 psw_cc = get_long_comp_cc(rlv1,0);
		         break;
		     }
		     break;
		 case 0xB6:  // 4430 "B6" "STCTL" "RS"
		     ins_setup_rs();
		     break;
		 case 0xB7:  // 4440 "B7" "LCTL" "RS"
		     ins_setup_rs();
		     break;
		 case 0xB9:  
		     opcode2 = mem_byte[psw_loc+opcode2_offset_rre] & 0xff;
		     switch (opcode2){
		     case 0x00:  // 4450 "B900" "LPGR" "RRE"
			 	 psw_check = false;
			     ins_setup_rre();
			     rlv1 = reg.getLong(rf2);
			     if (rlv1 < 0){
			     	if  (rlv1 == long_high_bit){
			     		psw_cc = psw_cc3;
			     		break;
			     	}
			     	rlv1 = - rlv1;
			     }
			     reg.putLong(rf1,rlv1);
			     psw_cc = get_long_comp_cc(rlv1,0);
		         break;
		     case 0x01:  // 4460 "B901" "LNGR" "RRE"
			 	 psw_check = false;
			     ins_setup_rre();
			     rlv1 = reg.getLong(rf2);
			     if (rlv1 > 0){
			     	rlv1 = - rlv1;
			     }
			     reg.putLong(rf1,rlv1);
			     psw_cc = get_long_comp_cc(rlv1,0);
		         break;
		     case 0x02:  // 4470 "B902" "LTGR" "RRE"
			 	 psw_check = false;
			     ins_setup_rre();
			     rlv1 = reg.getLong(rf2);
			     psw_cc = get_long_comp_cc(rlv1,0);
		         break;
		     case 0x03:  // 4480 "B903" "LCGR" "RRE"
			 	 psw_check = false;
			     ins_setup_rre();
			     rlv1 = reg.getLong(rf2);
			     if (rlv1 == long_high_bit){
			     	psw_cc = psw_cc3;
			     	break;
			     }
			     rlv1 = - rlv1;
			     reg.putLong(rf1,rlv1);
			     psw_cc = get_long_comp_cc(rlv1,0);
		         break;
		     case 0x04:  // 4490 "B904" "LGR" "RRE"
				 psw_check = false;
			     ins_setup_rre();
			     reg.putLong(rf1,reg.getLong(rf2));
		         break;
		     case 0x05:  // 4500 "B905" "LURAG" "RRE"
		         ins_setup_rre();
		         break;
		     case 0x08:  // 4510 "B908" "AGR" "RRE"
			     psw_check = false;
			     ins_setup_rre();
				 rlv1 = reg.getLong(rf1);
				 rlv2 = reg.getLong(rf2);
				 rlv3 = rlv1 + rlv2;
				 reg.putLong(rf1,rlv3);
				 psw_cc = get_long_add_cc();
		         break;
		     case 0x09:  // 4520 "B909" "SGR" "RRE"
			     psw_check = false;
			     ins_setup_rre();
				 rlv1 = reg.getLong(rf1);
				 rlv2 = reg.getLong(rf2);
				 rlv3 = rlv1 - rlv2;
				 reg.putLong(rf1,rlv3);
				 psw_cc = get_long_sub_cc();
		         break;
		     case 0x0A:  // 4530 "B90A" "ALGR" "RRE"
		     	 psw_check = false;
		         ins_setup_rre();
		         big_int1 = new BigInteger(get_log_bytes(reg_byte,rf1,8));
		         big_int2 = new BigInteger(get_log_bytes(reg_byte,rf2,8));
		         big_int1 = big_int1.add(big_int2);
                 cvt_big_int_to_work_reg(big_int1,9);
		         rlv1 = work_reg.getLong(1);
                 reg.putLong(rf1,rlv1);
				 psw_cc = get_log_add_cc(rlv1,work_reg_byte[0]);
		         break;
		     case 0x0B:  // 4540 "B90B" "SLGR" "RRE"
		     	 psw_check = false;
		         ins_setup_rre();
		         big_int1 = new BigInteger(get_log_bytes(reg_byte,rf1,8));
		         big_int2 = new BigInteger(get_log_bytes(reg_byte,rf2,8));
		         big_int1 = big_int1.subtract(big_int2);
                 cvt_big_int_to_work_reg(big_int1,9);
		         rlv1 = work_reg.getLong(1);
                 reg.putLong(rf1,rlv1);
				 psw_cc = get_log_sub_cc(rlv1,work_reg_byte[0]);
		         break;
		     case 0x0C:  // 4550 "B90C" "MSGR" "RRE"
		     	 psw_check = false;
		         ins_setup_rre();
		         reg.putLong(rf1,reg.getLong(rf1) * reg.getLong(rf2));
		         break;
		     case 0x0D:  // 4560 "B90D" "DSGR" "RRE"
				 psw_check = false;
			     ins_setup_rre();
			     rlv1 = reg.getLong(rf1+8);
			     rlv2 = reg.getLong(rf2);
			     if (rlv2 != 0){
			     	rlvw  = rlv1 / rlv2;
			     } else {
			     	set_psw_check(psw_pic_fx_div);
			     	break;
			     }
			     rlv1 = rlv1 - rlvw * rlv2;
			     reg.putLong(rf1,rlv1);
			     reg.putLong(rf1+8,rlvw);
		         break;
		     case 0x0E:  // 4570 "B90E" "EREGG" "RRE"
		     	 psw_check = false;
		         ins_setup_rre();
		         if (cur_pc_stk_reg <= 0){  
		         	set_psw_check(psw_pic_stack);
		         	return;
		         }
		         int pc_stk_reg_base = cur_pc_stk_reg - reg_byte.length;
			     if (rf1 > rf3){
			     	while (rf1 < reg_byte.length){
			     		reg.putLong(rf1,pc_stk_reg.getLong(pc_stk_reg_base + rf1));
			     		rf1 = rf1 + 8;
			     	}
			     	rf1 = 0;
			     }
			     while (rf1 <= rf3){
		     		reg.putLong(rf1,pc_stk_reg.getLong(pc_stk_reg_base + rf1));
			     	rf1 = rf1 + 8;
			     }
		         break;
		     case 0x0F:  // 4580 "B90F" "LRVGR" "RRE"
		     	 psw_check = false;
		         ins_setup_rre();
		         rflen = 7;
		         while (rflen >= 0){
		         	reg.put(rf1 + rflen,reg.get(rf2 + 7 - rflen));
		         	rflen--;
		         }
		         break;
		     case 0x10:  // 4590 "B910" "LPGFR" "RRE"
			 	 psw_check = false;
			     ins_setup_rre();
			     rlv1 = reg.getInt(rf2+4);
			     if (rlv1 < 0){
			     	rlv1 = - rlv1;
			     }
			     reg.putLong(rf1,rlv1);
			     psw_cc = get_long_comp_cc(rlv1,0);
		         break;
		     case 0x11:  // 4600 "B911" "LNGFR" "RRE"
			 	 psw_check = false;
			     ins_setup_rre();
			     rlv1 = reg.getInt(rf2+4);
			     if (rlv1 >= 0){
			     	rlv1 = - rlv1;
			     }
			     reg.putLong(rf1,rlv1);
			     psw_cc = get_long_comp_cc(rlv1,0);
		         break;
		     case 0x12:  // 4610 "B912" "LTGFR" "RRE"
			 	 psw_check = false;
			     ins_setup_rre();
			     rlv1 = reg.getInt(rf2+4);
			     reg.putLong(rf1,rlv1);
			     psw_cc = get_long_comp_cc(rlv1,0);
		         break;
		     case 0x13:  // 4620 "B913" "LCGFR" "RRE"
			 	 psw_check = false;
			     ins_setup_rre();
			     rlv1 =  reg.getInt(rf2+4);
		     	 rlv1 = - rlv1;
			     reg.putLong(rf1,rlv1);
			     psw_cc = get_long_comp_cc(rlv1,0);
		         break;
		     case 0x14:  // 4630 "B914" "LGFR" "RRE"
			 	 psw_check = false;
			     ins_setup_rre();
			     reg.putLong(rf1,reg.getInt(rf2+4));
		         break;
		     case 0x16:  // 4640 "B916" "LLGFR" "RRE"
		     	 psw_check = false;
		         ins_setup_rre();
		         reg.putLong(rf1,(long)reg.getInt(rf2+4) & long_low32_bits);
		         break;
		     case 0x17:  // 4650 "B917" "LLGTR" "RRE"
		     	 psw_check = false;
		     	 ins_setup_rre();
		     	 reg.putLong(rf1,reg.getInt(rf2+4) & int_num_bits);
		         break;
		     case 0x18:  // 4660 "B918" "AGFR" "RRE"
			 	 psw_check = false;
			     ins_setup_rre();
			     rlv1 = reg.getLong(rf1);
				 rlv2 = reg.getInt(rf2+4);
				 rlv3 = rlv1 + rlv2;
				 reg.putLong(rf1,rlv3);
				 psw_cc = get_long_add_cc();
		         break;
		     case 0x19:  // 4670 "B919" "SGFR" "RRE"
			 	 psw_check = false;
			     ins_setup_rre();
			     rlv1 = reg.getLong(rf1);
			     rlv2 = reg.getInt(rf2+4);
				 rlv3 = rlv1 - rlv2;
			     reg.putLong(rf1,rlv3);
				 psw_cc = get_long_sub_cc();
		         break;
		     case 0x1A:  // 4680 "B91A" "ALGFR" "RRE"
		     	 psw_check = false;
		         ins_setup_rre();
		         big_int1 = new BigInteger(get_log_bytes(reg_byte,rf1,8));
		         big_int2 = new BigInteger(get_log_bytes(reg_byte,rf2+4,4));
		         big_int1 = big_int1.add(big_int2);
                 cvt_big_int_to_work_reg(big_int1,9);
		         rlv1 = work_reg.getLong(1);
                 reg.putLong(rf1,rlv1);
				 psw_cc = get_log_add_cc(rlv1,work_reg_byte[0]);
		         break;
		     case 0x1B:  // 4690 "B91B" "SLGFR" "RRE"
		     	 psw_check = false;
		         ins_setup_rre();
		         big_int1 = new BigInteger(get_log_bytes(reg_byte,rf1,8));
		         big_int2 = new BigInteger(get_log_bytes(reg_byte,rf2+4,4));
		         big_int1 = big_int1.subtract(big_int2);
                 cvt_big_int_to_work_reg(big_int1,9);
		         rlv1 = work_reg.getLong(1);
                 reg.putLong(rf1,rlv1);
				 psw_cc = get_log_sub_cc(rlv1,work_reg_byte[0]);
		         break;
		     case 0x1C:  // 4700 "B91C" "MSGFR" "RRE"
		     	 psw_check = false;
		         ins_setup_rre();
		         reg.putLong(rf1,reg.getLong(rf1) * reg.getInt(rf2+4));
		         break;
		     case 0x1D:  // 4710 "B91D" "DSGFR" "RRE"
				 psw_check = false;
			     ins_setup_rre();
			     rlv1 = reg.getLong(rf1+8);
			     rlv2 = (long) reg.getInt(rf2+4);
			     if (rlv2 != 0){
			     	rlvw  = rlv1 / rlv2;
			     } else {
			     	set_psw_check(psw_pic_fx_div);
			     	break;
			     }
			     rlv1 = rlv1 - rlvw * rlv2;
			     reg.putLong(rf1,rlv1);
			     reg.putLong(rf1+8,rlvw);
		         break;
		     case 0x1E:  // 4720 "B91E" "KMAC" "RRE"
		         ins_setup_rre();
		         break;
		     case 0x1F:  // 4730 "B91F" "LRVR" "RRE"
		     	 psw_check = false;
		         ins_setup_rre();
		         rflen = 3;
		         while (rflen >= 0){
		         	reg.put(rf1 + 4 + rflen,reg.get(rf2 + 7 - rflen));
		         	rflen--;
		         }
		         break;
		     case 0x20:  // 4740 "B920" "CGR" "RRE"
			     psw_check = false;
			     ins_setup_rre();
			     psw_cc = get_long_comp_cc(
			     		reg.getLong(rf1),
						reg.getLong(rf2));
		         break;
		     case 0x21:  // 4750 "B921" "CLGR" "RRE"
		     	 psw_check = false;
		         ins_setup_rre();
			     psw_cc = get_long_log_comp_cc(
		     		     reg.getLong(rf1),
						 reg.getLong(rf2));
		         break;
		     case 0x25:  // 4760 "B925" "STURG" "RRE"
		         ins_setup_rre();
		         break;
		     case 0x2E:  // 4770 "B92E" "KM" "RRE"
		         ins_setup_rre();
		         break;
		     case 0x2F:  // 4780 "B92F" "KMC" "RRE"
		         ins_setup_rre();
		         break;
		     case 0x30:  // 4790 "B930" "CGFR" "RRE"
			     psw_check = false;
			     ins_setup_rre();
			     psw_cc = get_long_comp_cc(
			     		reg.getLong(rf1),
						(long) reg.getInt(rf2+4));
		         break;
		     case 0x31:  // 4800 "B931" "CLGFR" "RRE"
			     psw_check = false;
			     ins_setup_rre();
			     psw_cc = get_long_log_comp_cc(
			     		reg.getLong(rf1),
						(long) reg.getInt(rf2+4));
		         break;
		     case 0x3E:  // 4810 "B93E" "KIMD" "RRE"
		         ins_setup_rre();
		         break;
		     case 0x3F:  // 4820 "B93F" "KLMD" "RRE"
		         ins_setup_rre();
		         break;
		     case 0x46:  // 4830 "B946" "BCTGR" "RRE"
			 	 psw_check = false;
			     ins_setup_rre();
			     rlv1 = reg.getLong(rf1)-1;
				 reg.putLong(rf1,rlv1);
				 if (rf2 != 0 && rlv1 != 0){	
					set_psw_loc(reg.getInt(rf2+4));
				 }
		         break;
		     case 0x80:  // 4840 "B980" "NGR" "RRE"
		     	 psw_check = false;
		         ins_setup_rre();
		         rlv1 = reg.getLong(rf1) & reg.getLong(rf2);
		         reg.putLong(rf1,rlv1);
		         if (rlv1 == 0){
		         	psw_cc = psw_cc0;
		         } else {
		         	psw_cc = psw_cc1;
		         }
		         break;
		     case 0x81:  // 4850 "B981" "OGR" "RRE"
		     	 psw_check = false;
		         ins_setup_rre();
		         rlv1 = reg.getLong(rf1) | reg.getLong(rf2);
		         reg.putLong(rf1,rlv1);
		         if (rlv1 == 0){
		         	psw_cc = psw_cc0;
		         } else {
		         	psw_cc = psw_cc1;
		         }
		         break;
		     case 0x82:  // 4860 "B982" "XGR" "RRE"
		     	 psw_check = false;
		         ins_setup_rre();
		         rlv1 = reg.getLong(rf1) ^ reg.getLong(rf2);
		         reg.putLong(rf1,rlv1);
		         if (rlv1 == 0){
		         	psw_cc = psw_cc0;
		         } else {
		         	psw_cc = psw_cc1;
		         }
		         break;
		     case 0x86:  // 4870 "B986" "MLGR" "RRE"
		     	 psw_check = false;
		         ins_setup_rre();
		         big_int1 = new BigInteger(get_log_bytes(reg_byte,rf1,8));
		         big_int2 = new BigInteger(get_log_bytes(reg_byte,rf2,8));
		         big_int1 = big_int1.multiply(big_int2);
                 cvt_big_int_to_work_reg(big_int1,16);
		         reg.putLong(rf1,work_reg.getLong(0));
		         reg.putLong(rf1+8,work_reg.getLong(8));
		         break;
		     case 0x87:  // 4880 "B987" "DLGR" "RRE"
		     	 psw_check = false;
		         ins_setup_rre();
		         work_reg.putLong(0,reg.getLong(rf1));
		         work_reg.putLong(8,reg.getLong(rf1+8));
		         big_int1 = new BigInteger(get_log_bytes(work_reg_byte,0,16));
		         big_int2 = new BigInteger(get_log_bytes(reg_byte,rf2,8));
			     if (big_int2.compareTo(BigInteger.ZERO) != 0){
			     	rv1  = (int) rlv1 / rv2;
			     } else {
			     	set_psw_check(psw_pic_fx_div);
			     	break;
			     }
		         big_int_array = big_int1.divideAndRemainder(big_int2);
                 cvt_big_int_to_work_reg(big_int_array[1],8); // get big remainder
		         reg.putLong(rf1,work_reg.getLong(0));
                 cvt_big_int_to_work_reg(big_int_array[0],8); // get big quotent
		         reg.putLong(rf1+8,work_reg.getLong(0));
		         break;
		     case 0x88:  // 4890 "B988" "ALCGR" "RRE"
		     	 psw_check = false;
		         ins_setup_rre();
		         big_int1 = new BigInteger(get_log_bytes(reg_byte,rf1,8));
		         big_int2 = new BigInteger(get_log_bytes(reg_byte,rf2,8));
		         big_int1 = big_int1.add(big_int2);
		         if (psw_carry[psw_cc] == 1){
		         	big_int1 = big_int1.add(BigInteger.ONE);
		         }
                 cvt_big_int_to_work_reg(big_int1,9);
		         rlv1 = work_reg.getLong(1);
                 reg.putLong(rf1,rlv1);
				 psw_cc = get_log_add_cc(rlv1,work_reg_byte[0]);
		         break;
		     case 0x89:  // 4900 "B989" "SLBGR" "RRE"
		     	 psw_check = false;
		         ins_setup_rre();
		         big_int1 = new BigInteger(get_log_bytes(reg_byte,rf1,8));
		         big_int2 = new BigInteger(get_log_bytes(reg_byte,rf2,8));
		         big_int1 = big_int1.subtract(big_int2);
		         if (psw_borrow[psw_cc] == 1){
		         	big_int1 = big_int1.subtract(BigInteger.ONE);
		         }
                 cvt_big_int_to_work_reg(big_int1,9);
		         rlv1 = work_reg.getLong(1);
                 reg.putLong(rf1,rlv1);
				 psw_cc = get_log_sub_cc(rlv1,work_reg_byte[0]);
		         break;
		     case 0x8A:  // 4910 "B98A" "CSPG" "RRE"
		         ins_setup_rre();
		         break;
		     case 0x8D:  // 4920 "B98D" "EPSW" "RRE"
		         ins_setup_rre();
		         break;
		     case 0x8E:  // 4930 "B98E" "IDTE" "RRF"
		         ins_setup_rrf();
		         break;
		     case 0x90:  // 4940 "B990" "TRTT" "RRE"
		         ins_setup_rre();
		         break;
		     case 0x91:  // 4950 "B991" "TRTO" "RRE"
		         ins_setup_rre();
		         break;
		     case 0x92:  // 4960 "B992" "TROT" "RRE"
		         ins_setup_rre();
		         break;
		     case 0x93:  // 4970 "B993" "TROO" "RRE"
		         ins_setup_rre();
		         break;
		     case 0x96:  // 4980 "B996" "MLR" "RRE"
		     	 psw_check = false;
		         ins_setup_rre();
		         big_int1 = new BigInteger(get_log_bytes(reg_byte,rf1+4,4));
		         big_int2 = new BigInteger(get_log_bytes(reg_byte,rf2+4,4));
		         big_int1 = big_int1.multiply(big_int2);
                 cvt_big_int_to_work_reg(big_int1,8);
		         reg.putInt(rf1+4,work_reg.getInt(0));
		         reg.putInt(rf1+12,work_reg.getInt(4));
		         break;
		     case 0x97:  // 4990 "B997" "DLR" "RRE"
		     	 psw_check = false;
		         ins_setup_rre();
		         work_reg.putInt(0,reg.getInt(rf1+4));
		         work_reg.putInt(4,reg.getInt(rf1+12));
		         big_int1 = new BigInteger(get_log_bytes(work_reg_byte,0,8));
		         big_int2 = new BigInteger(get_log_bytes(reg_byte,rf2+4,4));
			     if (big_int2.compareTo(BigInteger.ZERO) != 0){
			     	rv1  = (int) rlv1 / rv2;
			     } else {
			     	set_psw_check(psw_pic_fx_div);
			     	break;
			     }
		         BigInteger[] temp_big = big_int1.divideAndRemainder(big_int2);
                 cvt_big_int_to_work_reg(temp_big[1],4); // get big remainder
		         reg.putInt(rf1+4,work_reg.getInt(0));
                 cvt_big_int_to_work_reg(temp_big[0],4); // get big quotent
		         reg.putInt(rf1+12,work_reg.getInt(0));
		         break;
		     case 0x98:  // 5000 "B998" "ALCR" "RRE"
		     	 psw_check = false;
		         ins_setup_rre();
		         rlvw = ((long) reg.getInt(rf1+4) & long_low32_bits)
				      + ((long) reg.getInt(rf2+4) & long_low32_bits)
					  + psw_carry[psw_cc];
	             rlv1  = rlvw & long_low32_bits;
		         reg.putInt(rf1+4,(int)rlv1);
				 psw_cc = get_log_add_cc(rlv1,rlvw - rlv1);
		         break;
		     case 0x99:  // 5010 "B999" "SLBR" "RRE"
		     	 psw_check = false;
		         ins_setup_rre();
		         rlvw = ((long) reg.getInt(rf1+4) & long_low32_bits)
				      - ((long) reg.getInt(rf2+4) & long_low32_bits)
					  - psw_borrow[psw_cc];
	             rlv1 = rlvw & long_low32_bits;
		         reg.putInt(rf1+4,(int)rlv1);
				 psw_cc = get_log_sub_cc(rlv1,rlvw - rlv1);
		         break;
		     case 0x9A:  // 5020 "B99A" "EPAIR" "RRE"
		         ins_setup_rre();
		         break;
		     case 0x9B:  // 5030 "B99B" "ESAIR" "RRE"
		         ins_setup_rre();
		         break;
		     case 0x9D:  // 5040 "B99D" "ESEA" "RRE"
		         ins_setup_rre();
		         break;
		     case 0x9E:  // 5050 "B99E" "PTI" "RRE"
		         ins_setup_rre();
		         break;
		     case 0x9F:  // 5060 "B99F" "SSAIR" "RRE"
		         ins_setup_rre();
		         break;
		     case 0xB0:  // 5070 "B9B0" "CU14" "RRE"
		         ins_setup_rre();
		         break;
		     case 0xB1:  // 5080 "B9B1" "CU24" "RRE"
		         ins_setup_rre();
		         break;
		     case 0xB2:  // 5090 "B9B2" "CU41" "RRE"
		         ins_setup_rre();
		         break;
		     case 0xB3:  // 5100 "B9B3" "CU42" "RRE"
		         ins_setup_rre();
		         break;
		     case 0xBE:  // 5110 "B9BE" "SRSTU" "RRE"
		         ins_setup_rre();
		         break;
		     }
		     break;
		 case 0xBA:  // 5120 "BA" "CS" "RS"
		 	 psw_check = false;
		     ins_setup_rs();
		     if (reg.getInt(rf1+4) == mem.getInt(bd2_loc)){
		     	psw_cc = psw_cc0;
		     	mem.putInt(bd2_loc,reg.getInt(rf3+4));
		     } else {
		     	psw_cc = psw_cc1;
		     	reg.putInt(rf1+4,mem.getInt(bd2_loc));
		     }		     
		     break;
		 case 0xBB:  // 5130 "BB" "CDS" "RS"
		 	 psw_check = false;
		     ins_setup_rs();
		     if (reg.getInt(rf1+4) == mem.getInt(bd2_loc)
		     	&& reg.getInt(rf1+12) == mem.getInt(bd2_loc+4)){
		     	psw_cc = psw_cc0;
		     	mem.putInt(bd2_loc,reg.getInt(rf3+4));
		     	mem.putInt(bd2_loc+4,reg.getInt(rf3+12));
		     } else {
		     	psw_cc = psw_cc1;
		     	reg.putInt(rf1+4,mem.getInt(bd2_loc));
		     	reg.putInt(rf1+12,mem.getInt(bd2_loc+4));
		     }	
		     break;
		 case 0xBD:  // 5140 "BD" "CLM" "RS"
		 	 psw_check = false;
		     ins_setup_rs();
		     rv1 = reg.getInt(rf1+4);
             exec_clm();
		     break;
		 case 0xBE:  // 5150 "BE" "STCM" "RS"
		 	 psw_check = false;
		     ins_setup_rs();
		     rv1 = reg.getInt(rf1+4);
		     exec_stcm();
		     break;
		 case 0xBF:  // 5160 "BF" "ICM" "RS"
		     psw_check = false;
		 	 ins_setup_rs();
	     	 rv1 = reg.getInt(rf1+4);
	     	 exec_icm();
	     	 reg.putInt(rf1+4,rv1);
		     break;
		 case 0xC0:  
		     opcode2 = mem_byte[psw_loc+opcode2_offset_ril] &0xf;
		     switch (opcode2){
		     case 0x0:  // 5170 "C00" "LARL" "RIL"
		     	 psw_check = false;
		         ins_setup_ril();
		         reg.putInt(rf1+4,(psw_loc - 6 + 2*if2) & psw_amode);
		         break;
		     case 0x4:  // 5180 "C04" "BRCL" "RIL"
		     	 psw_check = false;
		         ins_setup_ril();
		         if ((mf1 & psw_cc) != 0){
		         	set_psw_loc(psw_loc - 6 + 2*if2);
		         }
		         break;
		     case 0x5:  // 5210 "C05" "BRASL" "RIL"
				 psw_check = false;
			     ins_setup_ril();
			     if (ex_mode){
			     	reg.putInt(rf1+4,ex_psw_return | psw_amode_bit);
			     } else {
				    reg.putInt(rf1+4,psw_loc | psw_amode_bit);
			     }
				 set_psw_loc(psw_loc - 6 + 2*if2);
		         break;
		     }
		     break;
		 case 0xD0:  // 5230 "D0" "TRTR" "SS"
		 	 psw_check = false;
		     ins_setup_ss();
		     psw_cc = psw_cc0;
		     bd1_end = bd1_loc - rflen;
		     boolean trt_hit = false;
             while (bd1_loc > bd1_end){
             	if (!trt_hit
             		&& (mem_byte[bd2_loc + (mem_byte[bd1_loc] & 0xff)])
					    != 0){
             		    trt_hit = true;
             		    if (bd1_loc - 1 == bd1_end){
             		    	psw_cc = psw_cc2;
             		    } else {
             		    	psw_cc = psw_cc1;
             		    }
             		    reg.putInt(r1,(reg.getInt(r1+4) & psw_amode_high_bits) | bd1_loc);
             		    reg.put(r2+3,mem_byte[bd2_loc + (mem_byte[bd1_loc] & 0xff)]);
             	}
             	bd1_loc--;
             }
		     break;
		 case 0xD1:  // 5240 "D1" "MVN" "SS"
             psw_check = false;
		     ins_setup_ss();
		     bd1_end = bd1_loc + rflen;
             while (bd1_loc < bd1_end){
            	mem_byte[bd1_loc] = (byte)((mem_byte[bd1_loc] & 0xf0) | (mem_byte[bd2_loc] & 0xf));
            	bd1_loc++;
            	bd2_loc++;
             }
		     break;
		 case 0xD2:  // 5250 "D2" "MVC" "SS"
             psw_check = false;
		     ins_setup_ss();
		     bd1_end = bd1_loc + rflen;
             while (bd1_loc < bd1_end){
             	mem_byte[bd1_loc] = mem_byte[bd2_loc];
             	bd1_loc++;
             	bd2_loc++;
             }
		     break;
		 case 0xD3:  // 5260 "D3" "MVZ" "SS"
	         psw_check = false;
			 ins_setup_ss();
		     bd1_end = bd1_loc + rflen;
             while (bd1_loc < bd1_end){
	           	mem_byte[bd1_loc] = (byte)((mem_byte[bd1_loc] & 0xf) | (mem_byte[bd2_loc] & 0xf0));
                bd1_loc++;
	            bd2_loc++;
	         }
		     break;
		 case 0xD4:  // 5270 "D4" "NC" "SS"
		 	 psw_check = false;
		     ins_setup_ss();
		     psw_cc = psw_cc0;
		     bd1_end = bd1_loc + rflen;
             while (bd1_loc < bd1_end){
             	sv1 = mem_byte[bd1_loc] & mem_byte[bd2_loc];
             	mem_byte[bd1_loc] = (byte) sv1;
             	if (sv1 != 0)psw_cc = psw_cc1;
             	bd1_loc++;
             	bd2_loc++;
             }
		     break;
		 case 0xD5:  // 5280 "D5" "CLC" "SS"
             psw_check = false;
		     ins_setup_ss();
		     psw_cc = psw_cc_equal;
		     bd1_end = bd1_loc + rflen;
             while (bd1_loc < bd1_end){
             	if (mem_byte[bd1_loc] != mem_byte[bd2_loc]){
             		if ((mem_byte[bd1_loc] & 0xff) > (mem_byte[bd2_loc] & 0xff)){
             			psw_cc = psw_cc_high;
             		} else {
             			psw_cc = psw_cc_low;
             		}
             		break;
             	}
             	bd1_loc++;
             	bd2_loc++;
             }
		     break;
		 case 0xD6:  // 5290 "D6" "OC" "SS"
		 	 psw_check = false;
		     ins_setup_ss();
		     psw_cc = psw_cc0;
		     bd1_end = bd1_loc + rflen;
             while (bd1_loc < bd1_end){
             	sv1 = mem_byte[bd1_loc] | mem_byte[bd2_loc];
             	mem_byte[bd1_loc] = (byte) sv1;
             	if (sv1 != 0)psw_cc = psw_cc1;
             	bd1_loc++;
             	bd2_loc++;
             }
		     break;
		 case 0xD7:  // 5300 "D7" "XC" "SS"
		 	 psw_check = false;
		     ins_setup_ss();
		     psw_cc = psw_cc0;
		     bd1_end = bd1_loc + rflen;
             while (bd1_loc < bd1_end){
             	sv1 = mem_byte[bd1_loc] ^ mem_byte[bd2_loc];
             	mem_byte[bd1_loc] = (byte) sv1;
             	if (sv1 != 0)psw_cc = psw_cc1;
             	bd1_loc++;
             	bd2_loc++;
             }
		     break;
		 case 0xD9:  // 5310 "D9" "MVCK" "SS"
		     ins_setup_ss();
		     break;
		 case 0xDA:  // 5320 "DA" "MVCP" "SS"
		     ins_setup_ss();
		     break;
		 case 0xDB:  // 5330 "DB" "MVCS" "SS"
		     ins_setup_ss();
		     break;
		 case 0xDC:  // 5340 "DC" "TR" "SS"
		 	 psw_check = false;
		     ins_setup_ss();
		     bd1_end = bd1_loc + rflen;
             while (bd1_loc < bd1_end){
             	mem_byte[bd1_loc] = mem_byte[bd2_loc + (mem_byte[bd1_loc] & 0xff)];
             	bd1_loc++;
             }
		     break;
		 case 0xDD:  // 5350 "DD" "TRT" "SS"
		 	 psw_check = false;
		     ins_setup_ss();
		     psw_cc = psw_cc0;
		     bd1_end = bd1_loc + rflen;
		     boolean trtr_hit = false;
             while (bd1_loc < bd1_end){
             	if (!trtr_hit
             		&& (mem_byte[bd2_loc + (mem_byte[bd1_loc] & 0xff)])
					    != 0){
             		    trtr_hit = true;
             		    if (bd1_loc + 1 == bd1_end){
             		    	psw_cc = psw_cc2;
             		    } else {
             		    	psw_cc = psw_cc1;
             		    }
             		    reg.putInt(r1,(reg.getInt(r1+4) & psw_amode_high_bits) | bd1_loc);
             		    reg.put(r2+3,mem_byte[bd2_loc + (mem_byte[bd1_loc] & 0xff)]);
             	}
             	bd1_loc++;
             }
		     break;
		 case 0xDE:  // 5360 "DE" "ED" "SS"
		     psw_check = false;
		     ins_setup_ss();
		     exec_ed_edmk(false);
		     break;
		 case 0xDF:  // 5370 "DF" "EDMK" "SS"
		 	 psw_check = false;
		     ins_setup_ss();
		     exec_ed_edmk(true);
		     break;
		 case 0xE1:  // 5380 "E1" "PKU" "SS"
		     ins_setup_ss();
		     break;
		 case 0xE2:  // 5390 "E2" "UNPKU" "SS"
		     ins_setup_ss();
		     break;
		 case 0xE3:  
		     opcode2 = mem_byte[psw_loc+opcode2_offset_rxy] & 0xff;
		     switch (opcode2){
		     case 0x03:  // 5400 "E303" "LRAG" "RXY"
		         ins_setup_rxy();
		         break;
		     case 0x04:  // 5410 "E304" "LG" "RXY"
		     	 psw_check = false;
		         ins_setup_rxy();
		         reg.putLong(rf1,mem.getLong(xbd2_loc));
		         break;
		     case 0x06:  // 5420 "E306" "CVBY" "RXY"
			 	 psw_check = false;
			     ins_setup_rxy();
			     big_int = get_pd_big_int(xbd2_loc,8);
			     reg.putInt(rf1+4,big_int.intValue());
		         break;
		     case 0x08:  // 5430 "E308" "AG" "RXY"
			     psw_check = false;
			     ins_setup_rxy();
				 rlv1 = reg.getLong(rf1);
				 rlv2 = mem.getLong(xbd2_loc);
				 rlv3 = rlv1 + rlv2;
				 reg.putLong(rf1,rlv3);
				 psw_cc = get_long_add_cc();
		         break;
		     case 0x09:  // 5440 "E309" "SG" "RXY"
			     psw_check = false;
			     ins_setup_rxy();
				 rlv1 = reg.getLong(rf1);
				 rlv2 = mem.getLong(xbd2_loc);
				 rlv3 = rlv1 - rlv2;
				 reg.putLong(rf1,rlv3);
				 psw_cc = get_long_sub_cc();
		         break;
		     case 0x0A:  // 5450 "E30A" "ALG" "RXY"
		     	 psw_check = false;
		         ins_setup_rxy();
		         big_int1 = new BigInteger(get_log_bytes(reg_byte,rf1,8));
		         big_int2 = new BigInteger(get_log_bytes(mem_byte,xbd2_loc,8));
		         big_int1 = big_int1.add(big_int2);
                 cvt_big_int_to_work_reg(big_int1,9);
		         rlv1 = work_reg.getLong(1);
                 reg.putLong(rf1,rlv1);
				 psw_cc = get_log_add_cc(rlv1,work_reg_byte[0]);
		         break;
		     case 0x0B:  // 5460 "E30B" "SLG" "RXY"
		     	 psw_check = false;
		         ins_setup_rxy();
		         big_int1 = new BigInteger(get_log_bytes(reg_byte,rf1,8));
		         big_int2 = new BigInteger(get_log_bytes(mem_byte,xbd2_loc,8));
		         big_int1 = big_int1.subtract(big_int2);
                 cvt_big_int_to_work_reg(big_int1,9);
		         rlv1 = work_reg.getLong(1);
                 reg.putLong(rf1,rlv1);
				 psw_cc = get_log_sub_cc(rlv1,work_reg_byte[0]);
		         break;
		     case 0x0C:  // 5470 "E30C" "MSG" "RXY"
		     	 psw_check = false;
		         ins_setup_rxy();
		         reg.putLong(rf1,reg.getLong(rf1) * mem.getLong(xbd2_loc));
		         break;
		     case 0x0D:  // 5480 "E30D" "DSG" "RXY"
				 psw_check = false;
			     ins_setup_rxy();
			     rlv1 = reg.getLong(rf1+8);
			     rlv2 = mem.getLong(xbd2_loc);
			     if (rlv2 != 0){
			     	rlvw  = rlv1 / rlv2;
			     } else {
			     	set_psw_check(psw_pic_fx_div);
			     	break;
			     }
			     rlv1 = rlv1 - rlvw * rlv2;
			     reg.putLong(rf1,rlv1);
			     reg.putLong(rf1+8,rlvw);
		         break;
		     case 0x0E:  // 5490 "E30E" "CVBG" "RXY"
			 	 psw_check = false;
			     ins_setup_rxy();
			     big_int = get_pd_big_int(xbd2_loc,16);
			     rlv1 = big_int.longValue();
			     if (big_int.signum() != -1
			     	 && rlv1 < 0){
			     	 set_psw_check(psw_pic_fx_div);
			     	 break;
			     }
			     reg.putLong(rf1,rlv1);
		         break;
		     case 0x0F:  // 5500 "E30F" "LRVG" "RXY"
		     	 psw_check = false;
		         ins_setup_rxy();
		         rflen = 7;
		         while (rflen >= 0){
		         	reg.put(rf1 + rflen,mem.get(xbd2_loc + 7 - rflen));
		         	rflen--;
		         }
		         break;
		     case 0x13:  // 5510 "E313" "LRAY" "RXY"
		         ins_setup_rxy();
		         break;
		     case 0x14:  // 5520 "E314" "LGF" "RXY"
		     	 psw_check = false;
		         ins_setup_rxy();
		         reg.putLong(rf1,mem.getInt(xbd2_loc));
		         break;
		     case 0x15:  // 5530 "E315" "LGH" "RXY"
		     	 psw_check = false;
		         ins_setup_rxy();
		         reg.putLong(rf1,mem.getShort(xbd2_loc));
		         break;
		     case 0x16:  // 5540 "E316" "LLGF" "RXY"
		     	 psw_check = false;
		         ins_setup_rxy();
		         reg.putLong(rf1,(long)mem.getInt(xbd2_loc) & long_low32_bits);
		         break;
		     case 0x17:  // 5550 "E317" "LLGT" "RXY"
		     	 psw_check = false;
		     	 ins_setup_rxy();
		     	 reg.putLong(rf1,mem.getInt(xbd2_loc+4) & int_num_bits);
		         break;
		     case 0x18:  // 5560 "E318" "AGF" "RXY"
			 	 psw_check = false;
			     ins_setup_rxy();
			     rlv1 = reg.getLong(rf1);
			     rlv2 = mem.getInt(xbd2_loc);
				 rlv3 = rlv1 + rlv2;
			     reg.putLong(rf1,rlv3);
				 psw_cc = get_long_add_cc();
		         break;
		     case 0x19:  // 5570 "E319" "SGF" "RXY"
			 	 psw_check = false;
			     ins_setup_rxy();
			     rlv1 = reg.getLong(rf1);
			     rlv2 = mem.getInt(xbd2_loc);
				 rlv3 = rlv1 - rlv2;
			     reg.putLong(rf1,rlv3);
				 psw_cc = get_long_sub_cc();
		         break;
		     case 0x1A:  // 5580 "E31A" "ALGF" "RXY"
		     	 psw_check = false;
		         ins_setup_rxy();
		         big_int1 = new BigInteger(get_log_bytes(reg_byte,rf1,8));
		         big_int2 = new BigInteger(get_log_bytes(mem_byte,xbd2_loc,4));
		         big_int1 = big_int1.add(big_int2);
                 cvt_big_int_to_work_reg(big_int1,9);
		         rlv1 = work_reg.getLong(1);
                 reg.putLong(rf1,rlv1);
				 psw_cc = get_log_add_cc(rlv1,work_reg_byte[0]);
		         break;
		     case 0x1B:  // 5590 "E31B" "SLGF" "RXY"
		     	 psw_check = false;
		         ins_setup_rxy();
		         big_int1 = new BigInteger(get_log_bytes(reg_byte,rf1,8));
		         big_int2 = new BigInteger(get_log_bytes(mem_byte,xbd2_loc,4));
		         big_int1 = big_int1.subtract(big_int2);
                 cvt_big_int_to_work_reg(big_int1,9);
		         rlv1 = work_reg.getLong(1);
                 reg.putLong(rf1,rlv1);
				 psw_cc = get_log_sub_cc(rlv1,work_reg_byte[0]);
		         break;
		     case 0x1C:  // 5600 "E31C" "MSGF" "RXY"
		     	 psw_check = false;
		         ins_setup_rxy();
		         reg.putLong(rf1,reg.getLong(rf1) * mem.getInt(xbd2_loc));
		         break;
		     case 0x1D:  // 5610 "E31D" "DSGF" "RXY"
				 psw_check = false;
			     ins_setup_rxy();
			     rlv1 = reg.getLong(rf1+8);
			     rlv2 = (long) mem.getInt(xbd2_loc);
			     if (rlv2 != 0){
			     	rlvw  = rlv1 / rlv2;
			     } else {
			     	set_psw_check(psw_pic_fx_div);
			     	break;
			     }
			     rlv1 = rlv1 - rlvw * rlv2;
			     reg.putLong(rf1,rlv1);
			     reg.putLong(rf1+8,rlvw);
		         break;
		     case 0x1E:  // 5620 "E31E" "LRV" "RXY"
		         ins_setup_rxy();
		         break;
		     case 0x1F:  // 5630 "E31F" "LRVH" "RXY"
		     	 psw_check = false;
		         ins_setup_rxy();
		         rflen = 1;
		         while (rflen >= 0){
		         	reg.put(rf1 + 6 + rflen,mem.get(xbd2_loc + 1 - rflen));
		         	rflen--;
		         }
		         break;
		     case 0x20:  // 5640 "E320" "CG" "RXY"
		     	 psw_check = false;
		         ins_setup_rxy();
			     psw_cc = get_long_comp_cc(
		     		     reg.getLong(rf1),
						 mem.getLong(xbd2_loc));
		         break;
		     case 0x21:  // 5650 "E321" "CLG" "RXY"
		     	 psw_check = false;
		         ins_setup_rxy();
			     psw_cc = get_long_log_comp_cc(
		     		     reg.getLong(rf1),
						 mem.getLong(xbd2_loc));
		         break;
		     case 0x24:  // 5660 "E324" "STG" "RXY"
		         psw_check = false;
		     	 ins_setup_rxy();
		     	 mem.putLong(xbd2_loc & psw_amode,reg.getLong(rf1));			 
		         break;
		     case 0x26:  // 5670 "E326" "CVDY" "RXY"
			     psw_check = false;
			     ins_setup_rxy();
		         big_int = BigInteger.valueOf(reg.getInt(rf1+4));
			     put_pd_big_int(xbd2_loc,8);
		         break;
		     case 0x2E:  // 5680 "E32E" "CVDG" "RXY"
		     	 psw_check = false;
		         ins_setup_rxy();
		         big_int = BigInteger.valueOf(reg.getLong(rf1));
			     put_pd_big_int(xbd2_loc,16);
		         break;
		     case 0x2F:  // 5690 "E32F" "STRVG" "RXY"
		     	 psw_check = false;
		         ins_setup_rxy();
		         rflen = 7;
		         while (rflen >= 0){
		         	mem.put(xbd2_loc + rflen,reg.get(rf1 + 7 - rflen));
		         	rflen--;
		         }
		         break;
		     case 0x30:  // 5700 "E330" "CGF" "RXY"
		     	 psw_check = false;
		         ins_setup_rxy();
			     psw_cc = get_long_comp_cc(
		     		     reg.getLong(rf1),
						 (long) mem.getInt(xbd2_loc));
		         break;
		     case 0x31:  // 5710 "E331" "CLGF" "RXY"
		     	 psw_check = false;
		         ins_setup_rxy();
			     psw_cc = get_long_log_comp_cc(
		     		     reg.getLong(rf1),
						 (long)mem.getInt(xbd2_loc) & long_low32_bits);
		         break;
		     case 0x3E:  // 5720 "E33E" "STRV" "RXY"
		     	 psw_check = false;
		         ins_setup_rxy();
		         rflen = 3;
		         while (rflen >= 0){
		         	mem.put(xbd2_loc + rflen,reg.get(rf1 + 7 - rflen));
		         	rflen--;
		         }
		         break;
		     case 0x3F:  // 5730 "E33F" "STRVH" "RXY"
		     	 psw_check = false;
		         ins_setup_rxy();
		         rflen = 1;
		         while (rflen >= 0){
		         	mem.put(xbd2_loc + rflen,reg.get(rf1 + 7 - rflen));
		         	rflen--;
		         }
		         break;
		     case 0x46:  // 5740 "E346" "BCTG" "RXY"
				 psw_check = false;
			     ins_setup_rxy();
				 rlv1 = reg.getLong(rf1)-1;
				 reg.putLong(rf1,rlv1);
				 if (rlv1 != 0){
					set_psw_loc(xbd2_loc);
				 }
		         break;
		     case 0x50:  // 5750 "E350" "STY" "RXY"
			     ins_setup_rxy();
			     psw_check = false;
			     mem.putInt(xbd2_loc,reg.getInt(rf1+4));
		         break;
		     case 0x51:  // 5760 "E351" "MSY" "RXY"
			 	 psw_check = false;
			     ins_setup_rxy();
			     reg.putInt(rf1+4,reg.getInt(rf1+4)*mem.getInt(xbd2_loc));
		         break;
		     case 0x54:  // 5770 "E354" "NY" "RXY"
			 	 psw_check = false;
			     ins_setup_rxy();
			     rv1 = reg.getInt(rf1+4) & mem.getInt(xbd2_loc);
			     reg.putInt(rf1+4,rv1);
			     if (rv1 == 0){
			     	psw_cc = psw_cc0;
			     } else {
			     	psw_cc = psw_cc1;
			     }
		         break;
		     case 0x55:  // 5780 "E355" "CLY" "RXY"
			     psw_check = false;
			     ins_setup_rxy();
			     psw_cc = get_int_log_comp_cc(
			     		     reg.getInt(rf1+4),
							 mem.getInt(xbd2_loc));
		         break;
		     case 0x56:  // 5790 "E356" "OY" "RXY"
			 	 psw_check = false;
			     ins_setup_rxy();
			     rv1 = reg.getInt(rf1+4) | mem.getInt(xbd2_loc);
			     reg.putInt(rf1+4,rv1);
			     if (rv1 == 0){
			     	psw_cc = psw_cc0;
			     } else {
			     	psw_cc = psw_cc1;
			     }
		         break;
		     case 0x57:  // 5800 "E357" "XY" "RXY"
			 	 psw_check = false;
			     ins_setup_rxy();
			     rv1 = reg.getInt(rf1+4) ^ mem.getInt(xbd2_loc);
			     reg.putInt(rf1+4,rv1);
			     if (rv1 == 0){
			     	psw_cc = psw_cc0;
			     } else {
			     	psw_cc = psw_cc1;
			     }
		         break;
		     case 0x58:  // 5810 "E358" "LY" "RXY"
				 psw_check = false;
			     ins_setup_rxy();
				 reg.putInt(rf1+4,mem.getInt(xbd2_loc));
		         break;
		     case 0x59:  // 5820 "E359" "CY" "RXY"
			     psw_check = false;
			     ins_setup_rxy();
			     psw_cc = get_int_comp_cc(
		     		     reg.getInt(rf1+4),
						 mem.getInt(xbd2_loc));
		         break;
		     case 0x5A:  // 5830 "E35A" "AY" "RXY"
			 	 psw_check = false;
			     ins_setup_rxy();
			     rv1 = reg.getInt(rf1+4);
				 rv2 = mem.getInt(xbd2_loc);
			     rv3 = rv1 + rv2;
				 reg.putInt(rf1+4,rv3);
				 psw_cc = get_int_add_cc();
		         break;
		     case 0x5B:  // 5840 "E35B" "SY" "RXY"
			 	 psw_check = false;
			     ins_setup_rxy();
			     rv1 = reg.getInt(rf1+4); 
				 rv2 = mem.getInt(xbd2_loc);
			     rv3 = rv1 - rv2;
				 reg.putInt(rf1+4,rv3);
				 psw_cc = get_int_sub_cc();
		         break;
		     case 0x5E:  // 5850 "E35E" "ALY" "RXY"
		     	 psw_check = false;
		         ins_setup_rxy();
		         rlvw = ((long) reg.getInt(rf1+4) & long_low32_bits)
				      + ((long) mem.getInt(xbd2_loc) & long_low32_bits);
	             rlv1  = rlvw & long_low32_bits;
		         reg.putInt(rf1+4,(int)rlv1);
				 psw_cc = get_log_add_cc(rlv1,rlvw - rlv1);
		         break;
		     case 0x5F:  // 5860 "E35F" "SLY" "RXY"
		     	 psw_check = false;
		         ins_setup_rxy();
		         rlvw = ((long) reg.getInt(rf1+4) & long_low32_bits)
				      - ((long) mem.getInt(xbd2_loc) & long_low32_bits);
	             rlv1 = rlvw & long_low32_bits;
		         reg.putInt(rf1+4,(int)rlv1);
				 psw_cc = get_log_sub_cc(rlv1,rlvw - rlv1);
		         break;
		     case 0x70:  // 5870 "E370" "STHY" "RXY"
			 	 psw_check = false;
			     ins_setup_rxy();
			     mem.putShort(xbd2_loc,(short)reg.getInt(rf1+4));
		         break;
		     case 0x71:  // 5880 "E371" "LAY" "RXY"
			     psw_check = false;
			     ins_setup_rxy();
			     reg.putInt(rf1+4,xbd2_loc & psw_amode);
		         break;
		     case 0x72:  // 5890 "E372" "STCY" "RXY"
			 	 psw_check = false;
			     ins_setup_rxy();
			     mem.put(xbd2_loc,reg.get(rf1+4+3));
		         break;
		     case 0x73:  // 5900 "E373" "ICY" "RXY"
			 	 psw_check = false;
			     ins_setup_rxy();
			     reg.put(rf1+7,mem.get(xbd2_loc));
		         break;
		     case 0x76:  // 5910 "E376" "LB" "RXY"
		     	 psw_check = false;
		         ins_setup_rxy();
		         reg.putInt(rf1+4,mem.get(xbd2_loc));
		         break;
		     case 0x77:  // 5920 "E377" "LGB" "RXY"
		     	 psw_check = false;
		         ins_setup_rxy();
		         reg.putLong(rf1,mem.get(xbd2_loc));
		         break;
		     case 0x78:  // 5930 "E378" "LHY" "RXY"
			 	 psw_check = false;
			     ins_setup_rxy();
			     reg.putInt(rf1+4,mem.getShort(xbd2_loc));
		         break;
		     case 0x79:  // 5940 "E379" "CHY" "RXY"
			 	 psw_check = false;
			     ins_setup_rxy();
			     psw_cc = get_int_comp_cc(
		     		     reg.getInt(rf1+4),
						 mem.getShort(xbd2_loc));
		         break;
		     case 0x7A:  // 5950 "E37A" "AHY" "RXY"
			 	 psw_check = false;
			     ins_setup_rxy();
			     rv1 = reg.getInt(rf1+4);
				 rv2 = mem.getShort(xbd2_loc);
			     rv3 = rv1 + rv2;
				 reg.putInt(rf1+4,rv3);
				 psw_cc = get_int_add_cc();
		         break;
		     case 0x7B:  // 5960 "E37B" "SHY" "RXY"
			 	 psw_check = false;
			     ins_setup_rxy();
			     rv1 = reg.getInt(rf1+4);
				 rv2 = mem.getShort(xbd2_loc);
				 rv3 = rv1 - rv2;
			     reg.putInt(rf1+4,rv3);
				 psw_cc = get_int_sub_cc();
		         break;
		     case 0x80:  // 5970 "E380" "NG" "RXY"
		     	 psw_check = false;
		         ins_setup_rxy();
		         rlv1 = reg.getLong(rf1) & mem.getLong(xbd2_loc);
		         reg.putLong(rf1,rlv1);
		         if (rlv1 == 0){
		         	psw_cc = psw_cc0;
		         } else {
		         	psw_cc = psw_cc1;
		         }
		         break;
		     case 0x81:  // 5980 "E381" "OG" "RXY"
		     	 psw_check = false;
		         ins_setup_rxy();
		         rlv1 = reg.getLong(rf1) | mem.getLong(xbd2_loc);
		         reg.putLong(rf1,rlv1);
		         if (rlv1 == 0){
		         	psw_cc = psw_cc0;
		         } else {
		         	psw_cc = psw_cc1;
		         }
		         break;
		     case 0x82:  // 5990 "E382" "XG" "RXY"
		     	 psw_check = false;
		         ins_setup_rxy();
		         rlv1 = reg.getLong(rf1) ^ mem.getLong(xbd2_loc);
		         reg.putLong(rf1,rlv1);
		         if (rlv1 == 0){
		         	psw_cc = psw_cc0;
		         } else {
		         	psw_cc = psw_cc1;
		         }
		         break;
		     case 0x86:  // 6000 "E386" "MLG" "RXY"
		     	 psw_check = false;
		         ins_setup_rxy();
		         big_int1 = new BigInteger(get_log_bytes(reg_byte,rf1,8));
		         big_int2 = new BigInteger(get_log_bytes(mem_byte,xbd2_loc,8));
		         big_int1 = big_int1.multiply(big_int2);
                 cvt_big_int_to_work_reg(big_int1,16);
		         reg.putLong(rf1,work_reg.getLong(0));
		         reg.putLong(rf1+8,work_reg.getLong(8));
		         break;
		     case 0x87:  // 6010 "E387" "DLG" "RXY"
		     	 psw_check = false;
		         ins_setup_rxy();
		         work_reg.putLong(0,reg.getLong(rf1));
		         work_reg.putLong(8,reg.getLong(rf1+8));
		         big_int1 = new BigInteger(get_log_bytes(work_reg_byte,0,16));
		         big_int2 = new BigInteger(get_log_bytes(mem_byte,xbd2_loc,8));
			     if (big_int2.compareTo(BigInteger.ZERO) != 0){
			     	rv1  = (int) rlv1 / rv2;
			     } else {
			     	set_psw_check(psw_pic_fx_div);
			     	break;
			     }
		         big_int_array = big_int1.divideAndRemainder(big_int2);
                 cvt_big_int_to_work_reg(big_int_array[1],8); // get big remainder
		         reg.putLong(rf1,work_reg.getLong(0));
                 cvt_big_int_to_work_reg(big_int_array[0],8); // get big quotent
		         reg.putLong(rf1+8,work_reg.getLong(0));
		         break;
		     case 0x88:  // 6020 "E388" "ALCG" "RXY"
		     	 psw_check = false;
		         ins_setup_rxy();
		         big_int1 = new BigInteger(get_log_bytes(reg_byte,rf1,8));
		         big_int2 = new BigInteger(get_log_bytes(mem_byte,xbd2_loc,8));
		         big_int1 = big_int1.add(big_int2);
		         if (psw_carry[psw_cc] == 1){
		         	big_int1 = big_int1.add(BigInteger.ONE);
		         }
                 cvt_big_int_to_work_reg(big_int1,9);
		         rlv1 = work_reg.getLong(1);
                 reg.putLong(rf1,rlv1);
				 psw_cc = get_log_add_cc(rlv1,work_reg_byte[0]);
		         break;
		     case 0x89:  // 6030 "E389" "SLBG" "RXY"
		     	 psw_check = false;
		         ins_setup_rxy();
		         big_int1 = new BigInteger(get_log_bytes(reg_byte,rf1,8));
		         big_int2 = new BigInteger(get_log_bytes(mem_byte,xbd2_loc,8));
		         big_int1 = big_int1.subtract(big_int2);
		         if (psw_borrow[psw_cc] == 1){
		         	big_int1 = big_int1.subtract(BigInteger.ONE);
		         }
                 cvt_big_int_to_work_reg(big_int1,9);
		         rlv1 = work_reg.getLong(1);
                 reg.putLong(rf1,rlv1);
				 psw_cc = get_log_sub_cc(rlv1,work_reg_byte[0]);
		         break;
		     case 0x8E:  // 6040 "E38E" "STPQ" "RXY"
		         ins_setup_rxy();
		         break;
		     case 0x8F:  // 6050 "E38F" "LPQ" "RXY"
		         ins_setup_rxy();
		         break;
		     case 0x90:  // 6060 "E390" "LLGC" "RXY"
		     	 psw_check = false;
		         ins_setup_rxy();
		         reg.putLong(rf1,mem.get(xbd2_loc) & 0xff);
		         break;
		     case 0x91:  // 6070 "E391" "LLGH" "RXY"
		     	 psw_check = false;
		         ins_setup_rxy();
		         reg.putLong(rf1,mem.getShort(xbd2_loc) & 0xffff);
		         break;
		     case 0x96:  // 6080 "E396" "ML" "RXY"
		     	 psw_check = false;
		         ins_setup_rxy();
		         big_int1 = new BigInteger(get_log_bytes(reg_byte,rf1+4,4));
		         big_int2 = new BigInteger(get_log_bytes(mem_byte,xbd2_loc,4));
		         big_int1 = big_int1.multiply(big_int2);
                 cvt_big_int_to_work_reg(big_int1,8);
		         reg.putInt(rf1+4,work_reg.getInt(0));
		         reg.putInt(rf1+12,work_reg.getInt(4));
		         break;
		     case 0x97:  // 6090 "E397" "DL" "RXY"
		     	 psw_check = false;
		         ins_setup_rxy();
		         work_reg.putInt(0,reg.getInt(rf1+4));
		         work_reg.putInt(4,reg.getInt(rf1+12));
		         big_int1 = new BigInteger(get_log_bytes(work_reg_byte,0,8));
		         big_int2 = new BigInteger(get_log_bytes(mem_byte,xbd2_loc,4));
			     if (big_int2.compareTo(BigInteger.ZERO) != 0){
			     	rv1  = (int) rlv1 / rv2;
			     } else {
			     	set_psw_check(psw_pic_fx_div);
			     	break;
			     }
		         big_int_array = big_int1.divideAndRemainder(big_int2);
                 cvt_big_int_to_work_reg(big_int_array[1],4); // get big remainder
		         reg.putInt(rf1+4,work_reg.getInt(0));
                 cvt_big_int_to_work_reg(big_int_array[0],4); // get big quotent
		         reg.putInt(rf1+12,work_reg.getInt(0));
		         break;
		     case 0x98:  // 6100 "E398" "ALC" "RXY"
		     	 psw_check = false;
		         ins_setup_rxy();
		         rlvw = ((long) reg.getInt(rf1+4) & long_low32_bits) 
				      + ((long) mem.getInt(xbd2_loc) & long_low32_bits)
					  + psw_carry[psw_cc];
	             rlv1  = rlvw & long_low32_bits;
		         reg.putInt(rf1+4,(int)rlv1);
				 psw_cc = get_log_add_cc(rlv1,rlvw - rlv1);
		         break;
		     case 0x99:  // 6110 "E399" "SLB" "RXY"
		     	 psw_check = false;
		         ins_setup_rxy();
		         rlvw = ((long) reg.getInt(rf1+4) & long_low32_bits)
				      - ((long) mem.getInt(xbd2_loc) & long_low32_bits)
					  - psw_borrow[psw_cc];
	             rlv1 = rlvw & long_low32_bits;
		         reg.putInt(rf1+4,(int)rlv1);
				 psw_cc = get_log_sub_cc(rlv1,rlvw - rlv1);
		         break;
		     }
		     break;
		 case 0xE5:  
		     opcode2 = mem_byte[psw_loc+opcode2_offset_sse] & 0xff;
		     switch (opcode2){
		     case 0x00:  // 6120 "E500" "LASP" "SSE"
		         ins_setup_sse();
		         break;
		     case 0x01:  // 6130 "E501" "TPROT" "SSE"
		         ins_setup_sse();
		         break;
		     case 0x02:  // 6140 "E502" "STRAG" "SSE"
		         ins_setup_sse();
		         break;
		     case 0x0E:  // 6150 "E50E" "MVCSK" "SSE"
		         ins_setup_sse();
		         break;
		     case 0x0F:  // 6160 "E50F" "MVCDK" "SSE"
		         ins_setup_sse();
		         break;
		     }
		     break;
		 case 0xE8:  // 6170 "E8" "MVCIN" "SS"
             psw_check = false;
		     ins_setup_ss();
		     rflen--;
             while (rflen >= 0){
            	mem_byte[bd1_loc] = mem_byte[bd2_loc + rflen];
            	bd1_loc++;
            	rflen--;
             }
		     break;
		 case 0xE9:  // 6180 "E9" "PKA" "SS"
		     ins_setup_ss();
		     break;
		 case 0xEA:  // 6190 "EA" "UNPKA" "SS"
		     ins_setup_ss();
		     break;
		 case 0xEB:  
		     opcode2 = mem_byte[psw_loc+opcode2_offset_rsy] & 0xff;
		     switch (opcode2){
		     case 0x04:  // 6200 "EB04" "LMG" "RSY"
			     psw_check = false;
			     ins_setup_rsy();
			     if (rf1 > rf3){
			     	while (rf1 < reg_byte.length){
			     		reg.putLong(rf1,mem.getLong(bd2_loc));
			     		bd2_loc = bd2_loc+8;
			     		rf1 = rf1 + 8;
			     	}
			     	rf1 = 0;
			     }
			     while (rf1 <= rf3){
		     		reg.putLong(rf1,mem.getLong(bd2_loc));
			     	bd2_loc = bd2_loc+8;
			     	rf1 = rf1 + 8;
			     }
		         break;
		     case 0x0A:  // 6210 "EB0A" "SRAG" "RSY"
		     	 psw_check = false;
		         ins_setup_rsy();
		         reg.putLong(rf1,get_sra64(reg.getLong(rf3),bd2_loc & 0x3f));
		         break;
		     case 0x0B:  // 6220 "EB0B" "SLAG" "RSY"
		     	 psw_check = false;
		         ins_setup_rsy();
		         reg.putLong(rf1,get_sla64(reg.getLong(rf3),bd2_loc & 0x3f));
		         break;
		     case 0x0C:  // 6230 "EB0C" "SRLG" "RSY"
		     	 psw_check = false;
		         ins_setup_rsy();
		         reg.putLong(rf1,reg.getLong(rf3) >>> (bd2_loc & 0x3f));
		         break;
		     case 0x0D:  // 6240 "EB0D" "SLLG" "RSY"
		     	 psw_check = false;
		         ins_setup_rsy();
		         reg.putLong(rf1,reg.getLong(rf3) << (bd2_loc & 0x3f));
		         break;
		     case 0x0F:  // 6250 "EB0F" "TRACG" "RSY"
		         ins_setup_rsy();
		         break;
		     case 0x14:  // 6260 "EB14" "CSY" "RSY"
			 	 psw_check = false;
			     ins_setup_rsy();
			     if (reg.getInt(rf1+4) == mem.getInt(bd2_loc)){
			     	psw_cc = psw_cc0;
			     	mem.putInt(bd2_loc,reg.getInt(rf3+4));
			     } else {
			     	psw_cc = psw_cc1;
			     	reg.putInt(rf1+4,mem.getInt(bd2_loc));
			     }
		         break;
		     case 0x1C:  // 6270 "EB1C" "RLLG" "RSY"
		     	 psw_check = false;
		         ins_setup_rsy();
		         big_int1 = new BigInteger(get_log_bytes(reg_byte,rf3,8));
		         big_int1 = big_int1.multiply(BigInteger.valueOf(2).pow(bd2_loc & 0x3f));
                 cvt_big_int_to_work_reg(big_int1,16);
		         reg.putLong(rf1,work_reg.getLong(8) | work_reg.getLong(0));
		         break;
		     case 0x1D:  // 6280 "EB1D" "RLL" "RSY"
		     	 psw_check = false;
		         ins_setup_rsy();
		         rlv1 = ((long)reg.getInt(rf3+ 4) & long_low32_bits) << (bd2_loc & 0x3f); 
		         reg.putInt(rf1+4,(int)((rlv1 & long_low32_bits) | (rlv1 >>> 32)));
		         break;
		     case 0x20:  // 6290 "EB20" "CLMH" "RSY"
		         psw_check = false;
		     	 ins_setup_rsy();
			     rv1 = reg.getInt(rf1);
	             exec_clm();
		         break;
		     case 0x21:  // 6300 "EB21" "CLMY" "RSY"
		         psw_check = false;
		     	 ins_setup_rsy();
			     rv1 = reg.getInt(rf1+4);
	             exec_clm();
		         break;
		     case 0x24:  // 6310 "EB24" "STMG" "RSY"
			     psw_check = false;
			     ins_setup_rsy();
			     if (rf1 > rf3){
			     	while (rf1 < reg_byte.length){
			     		mem.putLong(bd2_loc,reg.getLong(rf1));
			     		bd2_loc = bd2_loc+8;
			     		rf1 = rf1 + 8;
			     	}
			     	rf1 = 0;
			     }
			     while (rf1 <= rf3){
			     	mem.putLong(bd2_loc,reg.getLong(rf1));
			     	bd2_loc = bd2_loc+8;
			     	rf1 = rf1 + 8;
			     }
		         break;
		     case 0x25:  // 6320 "EB25" "STCTG" "RSY"
		         ins_setup_rsy();
		         break;
		     case 0x26:  // 6330 "EB26" "STMH" "RSY"
			     psw_check = false;
			     ins_setup_rsy();
			     if (rf1 > rf3){
			     	while (rf1 < reg_byte.length){
			     		mem.putInt(bd2_loc,reg.getInt(rf1));
			     		bd2_loc = bd2_loc+4;
			     		rf1 = rf1 + 8;
			     	}
			     	rf1 = 0;
			     }
			     while (rf1 <= rf3){
			     	mem.putInt(bd2_loc,reg.getInt(rf1));
			     	bd2_loc = bd2_loc+4;
			     	rf1 = rf1 + 8;
			     }
		         break;
		     case 0x2C:  // 6340 "EB2C" "STCMH" "RSY"
			 	 psw_check = false;
			     ins_setup_rsy();
			     rv1 = reg.getInt(rf1);
			     exec_stcm();
		         break;
		     case 0x2D:  // 6350 "EB2D" "STCMY" "RSY"
			 	 psw_check = false;
			     ins_setup_rsy();
			     rv1 = reg.getInt(rf1+4);
			     exec_stcm();
		         break;
		     case 0x2F:  // 6360 "EB2F" "LCTLG" "RSY"
		         ins_setup_rsy();
		         break;
		     case 0x30:  // 6370 "EB30" "CSG" "RSY"
			 	 psw_check = false;
			     ins_setup_rsy();
			     if (reg.getLong(rf1) == mem.getLong(bd2_loc)){
			     	psw_cc = psw_cc0;
			     	mem.putLong(bd2_loc,reg.getLong(rf3));
			     } else {
			     	psw_cc = psw_cc1;
			     	reg.putLong(rf1,mem.getLong(bd2_loc));
			     }
		         break;
		     case 0x31:  // 6380 "EB31" "CDSY" "RSY"
			 	 psw_check = false;
			     ins_setup_rsy();
			     if (reg.getInt(rf1+4) == mem.getInt(bd2_loc)
			     	&& reg.getInt(rf1+12) == mem.getInt(bd2_loc+4)){
			     	psw_cc = psw_cc0;
			     	mem.putInt(bd2_loc,reg.getInt(rf3+4));
			     	mem.putInt(bd2_loc+4,reg.getInt(rf3+12));
			     } else {
			     	psw_cc = psw_cc1;
			     	reg.putInt(rf1+4,mem.getInt(bd2_loc));
			     	reg.putInt(rf1+12,mem.getInt(bd2_loc+4));
			     }	
		         break;
		     case 0x3E:  // 6390 "EB3E" "CDSG" "RSY"
			 	 psw_check = false;
			     ins_setup_rsy();
			     if (reg.getLong(rf1) == mem.getLong(bd2_loc)
			     	&& reg.getLong(rf1+8) == mem.getLong(bd2_loc+8)){
			     	psw_cc = psw_cc0;
			     	mem.putLong(bd2_loc,reg.getLong(rf3));
			     	mem.putLong(bd2_loc+8,reg.getLong(rf3+8));
			     } else {
			     	psw_cc = psw_cc1;
			     	reg.putLong(rf1,mem.getLong(bd2_loc));
			     	reg.putLong(rf1+8,mem.getLong(bd2_loc+8));
			     }	
		         break;
		     case 0x44:  // 6400 "EB44" "BXHG" "RSY"
			     psw_check = false;
		         ins_setup_rsy();
		         rlv3 = reg.getLong(rf3);
		         rlv1 = reg.getLong(rf1) + rlv3;
		         reg.putLong(rf1,rlv1);
		         if (rf3 == ((rf3 >> 4) << 4)){
		         	rlv3 = reg.getLong(rf3+8);
		         }
		         if (rlv1 > rlv3){
		             set_psw_loc(bd2_loc);
		         }
		         break;
		     case 0x45:  // 6410 "EB45" "BXLEG" "RSY"
			     psw_check = false;
		         ins_setup_rsy();
		         rlv3 = reg.getLong(rf3);
		         rlv1 = reg.getLong(rf1) + rlv3;
		         reg.putLong(rf1,rlv1);
		         if (rf3 == ((rf3 >> 4) << 4)){
		         	rlv3 = reg.getLong(rf3+8);
		         }
		         if (rlv1 <= rlv3){
		             set_psw_loc(bd2_loc);
		         }
		         break;
		     case 0x51:  // 6420 "EB51" "TMY" "SIY"
			 	 psw_check = false;
			     ins_setup_siy();
	             rv1 = mem.get(bd1_loc) & 0xff;
	             rvw = rv1 & if2;
	             if (rvw == 0){
	             	psw_cc = psw_cc0;
	             } else if (rvw == rv1){
	             	psw_cc = psw_cc3;
	             } else {
	             	psw_cc = psw_cc1;
	             }
		         break;
		     case 0x52:  // 6430 "EB52" "MVIY" "SIY"
			     psw_check = false;
			     ins_setup_siy();
			     mem_byte[bd1_loc] = (byte) if2;
		         break;
		     case 0x54:  // 6440 "EB54" "NIY" "SIY"
			 	 psw_check = false;
			     ins_setup_siy();
			     sv1 = mem.get(bd1_loc) & if2;
			     mem.put(bd1_loc,(byte)sv1);
			     if (sv1 == 0){
			     	psw_cc = psw_cc0;
			     } else {
			     	psw_cc = psw_cc1;
			     }
		         break;
		     case 0x55:  // 6450 "EB55" "CLIY" "SIY"
	         	 psw_check = false;
			     ins_setup_siy();
			     psw_cc = get_int_comp_cc(
			     		    (mem_byte[bd1_loc] & 0xff),
							if2);
		         break;
		     case 0x56:  // 6460 "EB56" "OIY" "SIY"
			 	 psw_check = false;
			     ins_setup_siy();
			     sv1 = mem.get(bd1_loc) | if2;
			     mem.put(bd1_loc,(byte)sv1);
			     if (sv1 == 0){
			     	psw_cc = psw_cc0;
			     } else {
			     	psw_cc = psw_cc1;
			     }
		         break;
		     case 0x57:  // 6470 "EB57" "XIY" "SIY"
			 	 psw_check = false;
			     ins_setup_siy();
			     sv1 = mem.get(bd1_loc) ^ if2;
			     mem.put(bd1_loc,(byte)sv1);
			     if (sv1 == 0){
			     	psw_cc = psw_cc0;
			     } else {
			     	psw_cc = psw_cc1;
			     }
		         break;
		     case 0x80:  // 6480 "EB80" "ICMH" "RSY"
		         psw_check = false;
		     	 ins_setup_rsy();
		     	 rv1 = reg.getInt(rf1);
		     	 exec_icm();
		     	 reg.putInt(rf1,rv1);
		         break;
		     case 0x81:  // 6490 "EB81" "ICMY" "RSY"
		         psw_check = false;
		     	 ins_setup_rsy();
		     	 rv1 = reg.getInt(rf1+4);
		     	 exec_icm();
		     	 reg.putInt(rf1+4,rv1);
		         break;
		     case 0x8E:  // 6500 "EB8E" "MVCLU" "RSY"
		         ins_setup_rsy();
		         break;
		     case 0x8F:  // 6510 "EB8F" "CLCLU" "RSY"
		         ins_setup_rsy();
		         break;
		     case 0x90:  // 6520 "EB90" "STMY" "RSY"
			     psw_check = false;
			     ins_setup_rsy();
			     if (rf1 > rf3){
			     	while (rf1 < reg_byte.length){
			     		mem.putInt(bd2_loc,reg.getInt(rf1+4));
			     		bd2_loc = bd2_loc+4;
			     		rf1 = rf1 + 8;
			     	}
			     	rf1 = 0;
			     }
			     while (rf1 <= rf3){
			     	mem.putInt(bd2_loc,reg.getInt(rf1+4));
			     	bd2_loc = bd2_loc+4;
			     	rf1 = rf1 + 8;
			     }
		         break;
		     case 0x96:  // 6530 "EB96" "LMH" "RSY"
			     psw_check = false;
			     ins_setup_rsy();
			     if (rf1 > rf3){
			     	while (rf1 < reg_byte.length){
			     		reg.putInt(rf1,mem.getInt(bd2_loc));
			     		bd2_loc = bd2_loc+4;
			     		rf1 = rf1 + 8;
			     	}
			     	rf1 = 0;
			     }
			     while (rf1 <= rf3){
		     		reg.putInt(rf1,mem.getInt(bd2_loc));
			     	bd2_loc = bd2_loc+4;
			     	rf1 = rf1 + 8;
			     }
		         break;
		     case 0x98:  // 6540 "EB98" "LMY" "RSY"
			     psw_check = false;
			     ins_setup_rsy();
			     if (rf1 > rf3){
			     	while (rf1 < reg_byte.length){
			     		reg.putInt(rf1+4,mem.getInt(bd2_loc));
			     		bd2_loc = bd2_loc+4;
			     		rf1 = rf1 + 8;
			     	}
			     	rf1 = 0;
			     }
			     while (rf1 <= rf3){
		     		reg.putInt(rf1+4,mem.getInt(bd2_loc));
			     	bd2_loc = bd2_loc+4;
			     	rf1 = rf1 + 8;
			     }
		         break;
		     case 0x9A:  // 6550 "EB9A" "LAMY" "RSY"
		         ins_setup_rsy();
		         break;
		     case 0x9B:  // 6560 "EB9B" "STAMY" "RSY"
		         ins_setup_rsy();
		         break;
		     case 0xC0:  // 6570 "EBC0" "TP" "RSL"
				 psw_check = false;
			     ins_setup_rsl();
			     big_int1 = get_pd_big_int(bd1_loc,rflen1);
		         psw_cc = pd_cc;
			     break;
		     }
		     break;
		 case 0xEC:  
		     opcode2 = mem_byte[psw_loc+opcode2_offset_rie] & 0xff;
		     switch (opcode2){
		     case 0x44:  // 6580 "EC44" "BRXHG" "RIE"
			     psw_check = false;
		         ins_setup_rie();
		         rlv3 = reg.getLong(rf3);
		         rlv1 = reg.getLong(rf1) + rlv3;
		         reg.putLong(rf1,rlv1);
		         if (rf3 == ((rf3 >> 4) << 4)){
		         	rlv3 = reg.getLong(rf3+8);
		         }
		         if (rlv1 > rlv3){
		             set_psw_loc(psw_loc - 6 + 2*if2);
		         }
		         break;
		     case 0x45:  // 6600 "EC45" "BRXLG" "RIE"
			     psw_check = false;
		         ins_setup_rie();
		         rlv3 = reg.getLong(rf3);
		         rlv1 = reg.getLong(rf1) + rlv3;
		         reg.putLong(rf1,rlv1);
		         if (rf3 == ((rf3 >> 4) << 4)){
		         	rlv3 = reg.getLong(rf3+8);
		         }
		         if (rlv1 <= rlv3){
		             set_psw_loc(psw_loc - 6 + 2*if2);
		         }
		         break;
		     }
		     break;
		 case 0xED:  
		     opcode2 = mem_byte[psw_loc+opcode2_offset_rxe] & 0xff;
		     switch (opcode2){
		     case 0x04:  // 6620 "ED04" "LDEB" "RXE"
			 	 psw_check = false;
			     ins_setup_rxe();
			     fp_rdv1 = fp_get_db_from_eb(mem,xbd2_loc);
			 	 fp_put_db(rf1,fp_db_type,fp_rdv1);
		         break;
		     case 0x05:  // 6630 "ED05" "LXDB" "RXE"
			 	 psw_check = false;
			     ins_setup_rxe();
			     fp_rbdv1 = fp_get_bd_from_db(mem,xbd2_loc);
			 	 fp_put_bd(rf1,fp_lb_type,fp_rbdv1);
		         break;
		     case 0x06:  // 6640 "ED06" "LXEB" "RXE"
			 	 psw_check = false;
			     ins_setup_rxe();
			     fp_rbdv1 = fp_get_bd_from_eb(mem,xbd2_loc);
			 	 fp_put_bd(rf1,fp_lb_type,fp_rbdv1);
		         break;
		     case 0x07:  // 6650 "ED07" "MXDB" "RXE"
		     	 psw_check = false;
		         ins_setup_rxe();
		         fp_rbdv1 = fp_get_bd_from_db(fp_reg,rf1)
				      .multiply(fp_get_bd_from_db(mem,xbd2_loc),fp_db_context)
					  .round(fp_x_context);
		         fp_put_bd(rf1,fp_lb_type,fp_rbdv1);
		         check_lb_mpy();
		         break;
		     case 0x08:  // 6660 "ED08" "KEB" "RXE"
			 	 psw_check = false;
			     ins_setup_rxe();
			     fp_signal = true;
			     psw_cc = fp_get_eb_comp_cc(
		     		     fp_get_eb_from_eb(fp_reg,rf1),
						 fp_get_eb_from_eb(mem,xbd2_loc));
			     break;
		     case 0x09:  // 6670 "ED09" "CEB" "RXE"
			 	 psw_check = false;
			     ins_setup_rxe();
			     psw_cc = fp_get_eb_comp_cc(
		     		     fp_get_eb_from_eb(fp_reg,rf1),
						 fp_get_eb_from_eb(mem,xbd2_loc));
		         break;
		     case 0x0A:  // 6680 "ED0A" "AEB" "RXE"
			 	 psw_check = false;
			     ins_setup_rxe();
			     fp_rev1 = fp_get_eb_from_eb(fp_reg,rf1) 
				         + fp_get_eb_from_eb(mem,xbd2_loc);
			     fp_put_eb(rf1,fp_eb_type,fp_rev1);
			     psw_cc = fp_get_eb_add_sub_cc();
		         break;
		     case 0x0B:  // 6690 "ED0B" "SEB" "RXE"
			 	 psw_check = false;
			     ins_setup_rxe();
			     fp_rev1 = fp_get_eb_from_eb(fp_reg,rf1) 
				         - fp_get_eb_from_eb(mem,xbd2_loc);
			     fp_put_eb(rf1,fp_eb_type,fp_rev1);
			     psw_cc = fp_get_eb_add_sub_cc();
		         break;
		     case 0x0C:  // 6700 "ED0C" "MDEB" "RXE"
			 	 psw_check = false;
			     ins_setup_rxe();
			     fp_rdv1 = fp_get_db_from_eb(fp_reg,rf1) 
				         * fp_get_db_from_eb(mem,xbd2_loc);
			     fp_put_db(rf1,fp_db_type,fp_rdv1);
		         check_db_mpy();
		         break;
		     case 0x0D:  // 6710 "ED0D" "DEB" "RXE"
			 	 psw_check = false;
			     ins_setup_rxe();
		         fp_rev2 = fp_get_eb_from_eb(mem,xbd2_loc);
			     fp_rev1 = fp_get_eb_from_eb(fp_reg,rf1) 
				         / fp_rev2;
			     fp_put_eb(rf1,fp_eb_type,fp_rev1);
		         check_eb_div();
		         break;
		     case 0x0E:  // 6720 "ED0E" "MAEB" "RXF"
		     	 psw_check = false;
		         ins_setup_rxf();
		         fp_rev1 = fp_get_eb_from_eb(fp_reg,rf1)
				         + fp_get_eb_from_eb(mem,xbd2_loc)
						 * fp_get_eb_from_eb(fp_reg,rf3);
		         fp_put_eb(rf1,fp_eb_type,fp_rev1);
		         check_eb_mpy();
		         break;
		     case 0x0F:  // 6730 "ED0F" "MSEB" "RXF"
		     	 psw_check = false;
		         ins_setup_rxf();
		         fp_rev1 = fp_get_eb_from_eb(fp_reg,rf1)
				         - fp_get_eb_from_eb(mem,xbd2_loc)
						 * fp_get_eb_from_eb(fp_reg,rf3);
		         fp_put_eb(rf1,fp_eb_type,fp_rev1);
		         check_eb_mpy();
		         break;
		     case 0x10:  // 6740 "ED10" "TCEB" "RXE"
		         ins_setup_rxe();
		         break;
		     case 0x11:  // 6750 "ED11" "TCDB" "RXE"
		         ins_setup_rxe();
		         break;
		     case 0x12:  // 6760 "ED12" "TCXB" "RXE"
		         ins_setup_rxe();
		         break;
		     case 0x14:  // 6770 "ED14" "SQEB" "RXE"
			 	 psw_check = false;
			     ins_setup_rxe();
			     fp_rev1 = (float) Math.sqrt(fp_get_db_from_eb(mem,xbd2_loc)); 
			     fp_put_eb(rf1,fp_eb_type,fp_rev1);
		         break;
		     case 0x15:  // 6780 "ED15" "SQDB" "RXE"
			 	 psw_check = false;
			     ins_setup_rxe();
			     fp_rdv1 = (float) Math.sqrt(fp_get_db_from_db(mem,xbd2_loc)); 
			     fp_put_db(rf1,fp_db_type,fp_rdv1);
		         break;
		     case 0x17:  // 6790 "ED17" "MEEB" "RXE"
			 	 psw_check = false;
			     ins_setup_rxe();
			     fp_rev1 = fp_get_eb_from_eb(fp_reg,rf1) 
				         * fp_get_eb_from_eb(mem,xbd2_loc);
			     fp_put_eb(rf1,fp_eb_type,fp_rev1);
		         check_eh_mpy();
		         break;
		     case 0x18:  // 6800 "ED18" "KDB" "RXE"
		     	psw_check = false;
		     	ins_setup_rxe();
		     	fp_signal = true;
		     	psw_cc = fp_get_db_comp_cc(
		     			 fp_get_db_from_db(fp_reg,rf1),
						 fp_get_db_from_db(mem,xbd2_loc));
		     	break;
		     case 0x19:  // 6810 "ED19" "CDB" "RXE"
			 	 psw_check = false;
			     ins_setup_rxe();
			     psw_cc = fp_get_db_comp_cc(
			     		  fp_get_db_from_db(fp_reg,rf1),
			     		  fp_get_db_from_db(mem,xbd2_loc)); 
		         break;
		     case 0x1A:  // 6820 "ED1A" "ADB" "RXE"
			 	 psw_check = false;
			     ins_setup_rxe();
			     fp_rdv1 = fp_get_db_from_db(fp_reg,rf1)
				         + fp_get_db_from_db(mem,xbd2_loc);
			     fp_put_db(rf1,fp_db_type,fp_rdv1);
			     psw_cc = fp_get_db_add_sub_cc(); 
		         break;
		     case 0x1B:  // 6830 "ED1B" "SDB" "RXE"
			 	 psw_check = false;
			     ins_setup_rxe();
			     fp_rdv1 = fp_get_db_from_db(fp_reg,rf1)
				         - fp_get_db_from_db(mem,xbd2_loc);
			     fp_put_db(rf1,fp_db_type,fp_rdv1);
			     psw_cc = fp_get_db_add_sub_cc(); 
		         break;
		     case 0x1C:  // 6840 "ED1C" "MDB" "RXE"
			 	 psw_check = false;
			     ins_setup_rxe();
			     fp_rdv1 = fp_get_db_from_db(fp_reg,rf1)
				         * fp_get_db_from_db(mem,xbd2_loc);
			     fp_put_db(rf1,fp_db_type,fp_rdv1);
		         check_db_mpy();
		         break;
		     case 0x1D:  // 6850 "ED1D" "DDB" "RXE"
			 	 psw_check = false;
			     ins_setup_rxe();
			     fp_rdv2 = fp_get_db_from_db(mem,xbd2_loc);
			     fp_rdv1 = fp_get_db_from_db(fp_reg,rf1)
				         / fp_rdv2;
			     fp_put_db(rf1,fp_db_type,fp_rdv1);
		         check_db_div();
		         break;
		     case 0x1E:  // 6860 "ED1E" "MADB" "RXF"
		     	 psw_check = false;
		         ins_setup_rxf();
		         fp_rdv1 = fp_get_db_from_db(fp_reg,rf1)
				         + fp_get_db_from_db(mem,xbd2_loc)
						 * fp_get_db_from_db(fp_reg,rf3);
		         fp_put_db(rf1,fp_dh_type,fp_rdv1);
		         check_db_mpy();
		         break;
		     case 0x1F:  // 6870 "ED1F" "MSDB" "RXF"
		     	 psw_check = false;
		         ins_setup_rxf();
		         fp_rdv1 = fp_get_db_from_db(fp_reg,rf1)
				         - fp_get_db_from_db(mem,xbd2_loc)
						 * fp_get_db_from_db(fp_reg,rf3);
		         fp_put_db(rf1,fp_dh_type,fp_rdv1);
		         check_db_mpy();
		         break;
		     case 0x24:  // 6880 "ED24" "LDE" "RXE"
		     	 psw_check = false;
		         ins_setup_rxe();
		         fp_load_reg(fp_dh_type,mem,xbd2_loc,fp_eh_type);
		         break;
		     case 0x25:  // 6890 "ED25" "LXD" "RXE"
		     	 psw_check = false;
		         ins_setup_rxe();
		         fp_load_reg(fp_lh_type,mem,xbd2_loc,fp_dh_type);
		         break;
		     case 0x26:  // 6900 "ED26" "LXE" "RXE"
		     	 psw_check = false;
		         ins_setup_rxe();
		         fp_load_reg(fp_lh_type,mem,xbd2_loc,fp_eh_type);
		         break;
		     case 0x2E:  // 6910 "ED2E" "MAE" "RXF"
		     	 psw_check = false;
		         ins_setup_rxf();
		         fp_rdv1 = fp_get_db_from_eh(fp_reg,rf1)
				         + fp_get_db_from_eh(mem,xbd2_loc)
						 * fp_get_db_from_eh(fp_reg,rf3);
		         fp_put_db(rf1,fp_eh_type,fp_rdv1);
		         check_eh_mpy();
		         break;
		     case 0x2F:  // 6920 "ED2F" "MSE" "RXF"
		     	 psw_check = false;
		         ins_setup_rxf();
		         fp_rdv1 = fp_get_db_from_eh(fp_reg,rf1)
				         - fp_get_db_from_eh(mem,xbd2_loc)
						 * fp_get_db_from_eh(fp_reg,rf3);
		         fp_put_db(rf1,fp_eh_type,fp_rdv1);
		         check_eh_mpy();
		         break;
		     case 0x34:  // 6930 "ED34" "SQE" "RXE"
			 	 psw_check = false;
			     ins_setup_rxe();
			     fp_rdv1 = Math.sqrt(fp_get_db_from_eh(mem,xbd2_loc)); 
			     fp_put_db(rf1,fp_eh_type,fp_rdv1);
		         break;
		     case 0x35:  // 6940 "ED35" "SQD" "RXE"
			 	 psw_check = false;
			     ins_setup_rxe();
			     fp_rdv1 = Math.sqrt(fp_get_db_from_dh(mem,xbd2_loc)); 
			     fp_put_db(rf1,fp_dh_type,fp_rdv1);
		         break;
		     case 0x37:  // 6950 "ED37" "MEE" "RXE"
			 	 psw_check = false;
			     ins_setup_rxe();
			     fp_rdv1 = fp_get_db_from_eh(fp_reg,rf1) 
				         * fp_get_db_from_eh(mem,xbd2_loc);
			     fp_put_db(rf1,fp_eh_type,fp_rdv1);
		         check_eh_mpy();
		         break;
		     case 0x3E:  // 6960 "ED3E" "MAD" "RXF"
		     	 psw_check = false;
		         ins_setup_rxf();
		         fp_rdv1 = fp_get_db_from_dh(fp_reg,rf1)
				         + fp_get_db_from_dh(mem,xbd2_loc)
						 * fp_get_db_from_dh(fp_reg,rf3);
		         fp_put_db(rf1,fp_dh_type,fp_rdv1);
		         check_dh_mpy();
		         break;
		     case 0x3F:  // 6970 "ED3F" "MSD" "RXF"
		     	 psw_check = false;
		         ins_setup_rxf();
		         fp_rdv1 = fp_get_db_from_dh(fp_reg,rf1)
				         - fp_get_db_from_dh(mem,xbd2_loc)
						 * fp_get_db_from_dh(fp_reg,rf3);
		         fp_put_db(rf1,fp_dh_type,fp_rdv1);
		         check_dh_mpy();
		         break;
		     case 0x64:  // 6980 "ED64" "LEY" "RXY"
		     	 psw_check = false;
		         ins_setup_rxy();
		         fp_load_reg(fp_eh_type,mem,xbd2_loc,fp_eh_type);
		         break;
		     case 0x65:  // 6990 "ED65" "LDY" "RXY"
		     	 psw_check = false;
		         ins_setup_rxy();
		         fp_load_reg(fp_dh_type,mem,xbd2_loc,fp_dh_type);
		         break;
		     case 0x66:  // 7000 "ED66" "STEY" "RXY"
			 	 psw_check = false;
			     ins_setup_rxy();
			 	 if (fp_reg_ctl[mf1] != fp_ctl_ld){
			 	 	fp_store_reg(fp_reg,rf1);
				 }
				 mem.putInt(xbd2_loc,fp_reg.getInt(rf1));
		         break;
		     case 0x67:  // 7010 "ED67" "STDY" "RXY"
			 	 psw_check = false;
			     ins_setup_rxy();
			 	 if (fp_reg_ctl[mf1] != fp_ctl_ld){
			 	 	fp_store_reg(fp_reg,rf1);
				 }
				 mem.putLong(xbd2_loc,fp_reg.getLong(rf1));
		         break;
		     }
		     break;
		 case 0xEE:  // 7020 "EE" "PLO" "SS"
		     ins_setup_ss();
		     break;
		 case 0xEF:  // 7030 "EF" "LMD" "SS"
		     psw_check = false;
		     ins_setup_ss();
		     rf1 = ((rflen-1) & 0xf0) >> 1;
		     rf3 = ((rflen-1) & 0xf) << 3;
		     if (rf1 > rf3){
		     	while (rf1 < reg_byte.length){
		     		reg.putInt(rf1,mem.getInt(bd1_loc));
		     		reg.putInt(rf1+4,mem.getInt(bd2_loc));
		     		bd1_loc = bd1_loc+4;
		     		bd2_loc = bd2_loc+4;
		     		rf1 = rf1 + 8;
		     	}
		     	rf1 = 0;
		     }
		     while (rf1 <= rf3){
	     		reg.putInt(rf1,mem.getInt(bd1_loc));
	     		reg.putInt(rf1+4,mem.getInt(bd2_loc));
	     		bd1_loc = bd1_loc+4;
	     		bd2_loc = bd2_loc+4;
		     	rf1 = rf1 + 8;
		     }
		     break;
		 case 0xF0:  // 7040 "F0" "SRP" "SS"
			 psw_check = false;
		     ins_setup_ssp();
		     big_int = get_pd_big_int(bd1_loc,rflen1);
             int round_digit = rflen2-1;
             int shift = bd2_loc & 0x3f;
             if (shift < 0x20){
             	while (shift > 0){
             	    big_int = big_int.multiply(BigInteger.valueOf(10));
             	    shift--;
             	}
             } else {
             	shift = 0x3f - shift + 1;
             	while (shift > 1){
             	    big_int = big_int.divide(BigInteger.valueOf(10));
             	    shift--;
             	}
             	big_int = big_int.add(BigInteger.valueOf(round_digit));
             	big_int = big_int.divide(BigInteger.valueOf(10));
             }
		     put_pd_big_int(bd1_loc,rflen1);
		     psw_cc = pd_cc;
		     break;
		 case 0xF1:  // 7050 "F1" "MVO" "SS"
		 	 psw_check = false;
		     ins_setup_ssp();
		     bd1_end = bd1_loc - 1;
		     bd2_end = bd2_loc - 1;
		     bd1_loc = bd1_loc + rflen1 - 1;
		     bd2_loc = bd2_loc + rflen2 - 1;
		     pdf_next_in = mem.get(bd2_loc);
		     bd2_loc--;
		     pdf_next_out = (byte)(mem.get(bd1_loc) & 0xf);
		     mem.put(bd1_loc,(byte)(pdf_next_out | ((pdf_next_in & 0xf) << 4)));
		     bd1_loc--;
		     while (bd1_loc > bd1_end){
		     	pdf_next_out = (byte)((pdf_next_in & 0xf0) >> 4);
		     	if (bd2_loc > bd2_end){
		     		pdf_next_in = mem.get(bd2_loc);
		     		bd2_loc--;
         		} else {
		    		pdf_next_in = 0;
		     	}
		     	mem.put(bd1_loc,(byte)(pdf_next_out | ((pdf_next_in & 0xf) << 4)));
		     	bd1_loc--;
		     }
		     break;
		 case 0xF2:  // 7060 "F2" "PACK" "SS"
		 	 psw_check = false;
		     ins_setup_ssp();
		     bd1_end = bd1_loc - 1;
		     bd2_end = bd2_loc - 1;
		     bd1_loc = bd1_loc + rflen1 - 1;
		     bd2_loc = bd2_loc + rflen2 - 1;
		     pdf_next_out = (byte)((mem.get(bd2_loc) & 0xf0) >> 4);
		     while (bd1_loc > bd1_end){
		     	if (bd2_loc > bd2_end){
		     		pdf_next_in = mem.get(bd2_loc);
		     		bd2_loc--;
         		} else {
		    		pdf_next_in = 0;
		     	}
		     	mem.put(bd1_loc,(byte)(pdf_next_out | ((pdf_next_in & 0xf) << 4)));
		     	bd1_loc--;
		     	if (bd2_loc > bd2_end){
		     		pdf_next_out = (byte)(mem.get(bd2_loc) & 0xf);
		     		bd2_loc--;
         		} else {
		    		pdf_next_out = 0;
		     	}
		     }
		     break;
		 case 0xF3:  // 7070 "F3" "UNPK" "SS"
		 	 psw_check = false;
		     ins_setup_ssp();
		     bd1_end = bd1_loc - 1;
		     bd2_end = bd2_loc - 1;
		     bd1_loc = bd1_loc + rflen1 - 1;
		     bd2_loc = bd2_loc + rflen2 - 1;
		     mem.put(bd1_loc,(byte)(((mem.get(bd2_loc) & 0xf0) >> 4) | ((mem.get(bd2_loc) & 0xf) << 4)));
		     bd1_loc--;
		     bd2_loc--;
		     boolean next_right = true;
		     while (bd1_loc > bd1_end){
		     	if (next_right){
		     	    if (bd2_loc > bd2_end){
			     		next_right = false;
		     		    pdf_next_in = mem.get(bd2_loc);
		     		    bd2_loc--;
         		    } else {
		    		    pdf_next_in = 0;
		     	    }
	     		    mem.put(bd1_loc,(byte)((pdf_next_in & 0xf) | 0xf0));
	     		    bd1_loc--;
		     	} else {
		     		next_right = true;
	     		    mem.put(bd1_loc,(byte)(((pdf_next_in & 0xf0) >> 4) | 0xf0));
	     		    bd1_loc--;
		     	}
		     }
		     break;
		 case 0xF8:  // 7080 "F8" "ZAP" "SS"
			 psw_check = false;
		     ins_setup_ssp();
		     big_int = get_pd_big_int(bd2_loc,rflen2);
		     put_pd_big_int(bd1_loc,rflen1);
		     psw_cc = pd_cc;
		     break;
		 case 0xF9:  // 7090 "F9" "CP" "SS"
			 psw_check = false;
		     ins_setup_ssp();
		     psw_cc = get_big_int_comp_cc(
		                get_pd_big_int(bd1_loc,rflen1),
						get_pd_big_int(bd2_loc,rflen2));
		     break;
		 case 0xFA:  // 7100 "FA" "AP" "SS"
			 psw_check = false;
		     ins_setup_ssp();
		     big_int1 = get_pd_big_int(bd1_loc,rflen1);
		     big_int2 = get_pd_big_int(bd2_loc,rflen2);
		     big_int = big_int1.add(big_int2);
		     put_pd_big_int(bd1_loc,rflen1);
		     psw_cc = pd_cc;
		     break;
		 case 0xFB:  // 7110 "FB" "SP" "SS"
			 psw_check = false;
		     ins_setup_ssp();
		     big_int1 = get_pd_big_int(bd1_loc,rflen1);
		     big_int2 = get_pd_big_int(bd2_loc,rflen2);
		     big_int = big_int1.subtract(big_int2);
		     put_pd_big_int(bd1_loc,rflen1);
		     psw_cc = pd_cc;
		     break;
		 case 0xFC:  // 7120 "FC" "MP" "SS"
			 psw_check = false;
		     ins_setup_ssp();
		     big_int1 = get_pd_big_int(bd1_loc,rflen1);
		     big_int2 = get_pd_big_int(bd2_loc,rflen2);
		     big_int = big_int1.multiply(big_int2);
		     put_pd_big_int(bd1_loc,rflen1);
		     break;
		 case 0xFD:  // 7130 "FD" "DP" "SS"
			 psw_check = false;
		     ins_setup_ssp();
		     big_int1 = get_pd_big_int(bd1_loc,rflen1);
		     big_int2 = get_pd_big_int(bd2_loc,rflen2);
		     if (big_int2.compareTo(BigInteger.ZERO) == 0){
		     	psw_cc = psw_cc3;
		     	set_psw_check(psw_pic_pd_div);
		     }
		     BigInteger[] big_quo_rem = big_int1.divideAndRemainder(big_int2);
		     big_int = big_quo_rem[0];
		     put_pd_big_int(bd1_loc,rflen1-rflen2);
		     big_int = big_quo_rem[1];
		     put_pd_big_int(bd1_loc+rflen1-rflen2,rflen2);
		     break;
		 default:
			 set_psw_check(psw_pic_oper);
		 }
		 if (ex_mode && (opcode1 != ex_opcode)){
	 	    set_psw_loc(ex_psw_return);
		 }
		 if (opt_time 
		 	&& !psw_check
		 	&& tot_ins > next_time_check){
			next_time_check = tot_ins + next_time_ins;
			cur_date = new Date();
			tod_end_pgm = cur_date.getTime();
			if (tod_end_pgm > tod_time_limit){
				set_psw_check(psw_pic_timeout);  // timeout
			}
		 }
	  }
}
/*
 **********************************************
 * instruction setup routines
 * ********************************************
 */
private void ins_setup_dm(){  // "DM" 1  DIAGNOSE 83000000
	psw_ins_len = 4;
	if  (opt_trace){
        put_ins_trace(" DIAGNOSE");
	}
	if (ex_mode){
		ex_restore();
	}
	psw_loc = psw_loc + 4;
}
private void ins_setup_e(){  // "E" 8 PR oooo
	psw_ins_len = 2;
	if  (opt_trace){
        put_ins_trace("");
	}
	if (ex_mode){
        ex_restore();
	}
	psw_loc = psw_loc + 2;
}
private void ins_setup_i(){  // "I" 1 SVC 00ii
	if1 = mem_byte[psw_loc+1] & 0xff;
	psw_ins_len = 2;
	if  (opt_trace){
        put_ins_trace(
                " I1=" + get_hex(if1,2)
				);
    }
	if (ex_mode){
		ex_restore();
	}
	psw_loc = psw_loc + 2;
}
private void ins_setup_ri(){  // "RI" 37 IIHH  ooroiiii
	/*
	 * fetch rf1 and if2
	 */
	rf2 = mem.get(psw_loc + 1) & 0xff;
	rf1 = (rf2 & 0xf0) >> 1;
	mf1 = rf1 >> 3;
	if2 = mem.getShort(psw_loc+2);
	psw_ins_len = 4;
    if  (opt_trace){
   	    if (opcode1 == 0xa7
   	 	    && opcode2 == 4){
   	 	    rf2 = psw_loc + 2*if2;
            put_ins_trace(
                    " S2(" + get_hex(rf2,8)
				  + ")=" + get_ins_target(rf2)
   		    );
   	    } else if (opcode1 == 0xa7
    	 	 && opcode2 >= 5 && opcode2 <= 7){
    	 	 rf2 = psw_loc + 2*if2;
             put_ins_trace(
    			    " R" + get_hex(mf1,1) + "=" + get_hex(reg.getInt(rf1+4),8)
                  + " S2(" + get_hex(rf2,8)
				  + ")=" + get_ins_target(rf2)
    		 );
    	 } else {
             put_ins_trace(
			    " R" + get_hex(mf1,1) + "=" + get_long_hex(reg.getLong(rf1))
              + " I2=" + get_hex(if2,4)
		     );
    	 }
    }
	if (ex_mode){
		ex_restore();
	}
	psw_loc = psw_loc + 4;
}
private void ins_setup_rie(){  // "RIE" 4  BRXLG  oorriiii00oo
	psw_ins_len = 6;
	rf1 = mem.get(psw_loc + 1) & 0xff;
	mf3 = rf1 & 0xf;
	rf3 = mf3 << 3;
	rf1 = (rf1 & 0xf0) >> 1;
	mf1 = rf1 >> 3;
	if2 = mem.getShort(psw_loc+2);
    if  (opt_trace){
        put_ins_trace(
    			    " R1" + get_hex(mf1,1) + "=" + get_hex(reg.getInt(rf1+4),8)
    			  + " R3" + get_hex(mf3,1) + "=" + get_hex(reg.getInt(rf3+4),8)
		          + " I2=" + get_hex(if2,8)
				  );
    }
	if (ex_mode){
		ex_restore();
	}
	psw_loc = psw_loc + 6;
}
private void ins_setup_ril(){  // "RIL" 6  BRCL  oomollllllll
	psw_ins_len = 6;
	rf1 = (mem.get(psw_loc + 1) & 0xf0) >> 1;
	mf1 = rf1 >> 3;
	if2 = mem.getInt(psw_loc+2);
	bd2_loc = psw_loc + 2 * if2;
    if  (opt_trace){
	     if (opcode2 == 0x0){
	        put_ins_trace(
			  " R" + get_hex(mf1,1) + "=" + get_hex(reg.getInt(rf1+4),8)
			+ " S2(" + get_hex(bd2_loc & psw_amode,8) + ")"
			);
	     } else if (opcode2 == 0x5){
            put_ins_trace(
    			    " R" + get_hex(mf1,1) + "=" + get_hex(reg.getInt(rf1+4),8) 
		          + " S2(" + get_hex(bd2_loc,8)
				  + ")=" + get_ins_target(bd2_loc)
				  );
	     } else {
            put_ins_trace(
    			    " M1=" + get_hex(mf1,1) 
		          + " S2(" + get_hex(bd2_loc,8)
				  + ")=" + get_ins_target(bd2_loc)
				  );
	     }
    }
	if (ex_mode){
		ex_restore();
	}
	psw_loc = psw_loc + 6;
}
private void ins_setup_rr(){  // "RR" 60  LR  oorr
	/*
	 * fetch rf1,rf2 and update psw
	 */
	rf1 = mem.get(psw_loc + 1) & 0xff;
	mf2 = rf1 & 0xf;
	rf2 = mf2 << 3;
	rf1 = (rf1 & 0xf0) >> 1;
	mf1 = rf1 >> 3;
	psw_ins_len = 2;
    if  (opt_trace){
    	if (opcode1 == 0x07){
    		rv2 = reg.getInt(rf2+4);  
     	    put_ins_trace(
                " R2(" + get_hex(rv2,8)
              + ")=" + get_ins_target(rv2 & psw_amode)
     			 );
    	} else if (opcode1 >= 0x20 && opcode1 <= 0x3f){
            put_ins_trace(
  				    " F" + get_hex(mf1,1) + "=" + get_fp_long_hex(rf1)
                  + " F" + get_hex(mf2,1) + "=" + get_fp_long_hex(rf2)
  				);
     	} else {
            put_ins_trace(
				  " R" + get_hex(mf1,1) + "=" + get_hex(reg.getInt(rf1+4),8)
                + " R" + get_hex(mf2,1) + "=" + get_hex(reg.getInt(rf2+4),8)
				);
     	}
    }
	if (ex_mode){
		ex_restore();
	}
	psw_loc = psw_loc + 2;
}
private void ins_setup_rre(){  // "RRE" 185  MSR oooo00rr
	psw_ins_len = 4;
	rf1 = mem.get(psw_loc + 3) & 0xff;
	mf2 = rf1 & 0xf;
	rf2 = mf2 << 3;
	rf1 = (rf1 & 0xf0) >> 1;
	mf1 = rf1 >> 3;
    if  (opt_trace){
    	if (opcode1 == 0xb3
    		|| (opcode1 == 0xb2 && (opcode2 == 0x2d
    				                || opcode2 == 0x44 
    				                || opcode2 == 0x45))){
             put_ins_trace(
				  " F" + get_hex(mf1,1) + "=" + get_fp_long_hex(rf1)
                + " F" + get_hex(mf2,1) + "=" + get_fp_long_hex(rf2)
				);
    	} else {
             put_ins_trace(
  				    " R" + get_hex(mf1,1) + "=" + get_long_hex(reg.getLong(rf1))
                  + " R" + get_hex(mf2,1) + "=" + get_long_hex(reg.getLong(rf2))
  				);
    	}
    }
	if (ex_mode){
		ex_restore();
	}
	psw_loc = psw_loc + 4;
}
private void ins_setup_rrf(){  // "RRF" 28 MAER oooor0rr (r1,r3,r2)
	psw_ins_len = 4;
	rf1 = (mem.get(psw_loc + 2) & 0xf0) >> 1;
	mf1 = rf1 >> 3;
	rf3 = mem.get(psw_loc + 3) & 0xff;
	mf2 = rf3 & 0xf;
	rf2 = mf2 << 3;
	rf3 = (rf3 & 0xf0) >> 1;
	mf3 = rf3 >> 3;
    if  (opt_trace){
        put_ins_trace(
				  " F" + get_hex(mf1,1) + "=" + get_long_hex(fp_reg.getLong(rf1))
                + " F" + get_hex(mf3,1) + "=" + get_long_hex(fp_reg.getLong(rf3))
                + " F" + get_hex(mf2,1) + "=" + get_long_hex(fp_reg.getLong(rf2))
				);
    }
	if (ex_mode){
		ex_restore();
	}
	psw_loc = psw_loc + 4;
}
private void ins_setup_rr4(){  // "RR4" 28 DIEBR oooor0rr (r3,m4,r1,r2 coded r1,r3,r2,m4)
	psw_ins_len = 4;
	rf3 = mem.get(psw_loc + 2) & 0xff;
	mf4 = rf3 & 0xf;
    rf3 = (rf3 & 0xf0) >> 1;
	mf3 = rf3 >> 3;
	rf1 = mem.get(psw_loc + 3) & 0xff;
	mf2 = rf1 & 0xf;
	rf2 = mf2 << 3;
	rf1 = (rf1 & 0xf0) >> 1;
	mf1 = rf1 >> 3;
    if  (opt_trace){
        put_ins_trace(
				  " F" + get_hex(mf1,1) + "=" + get_long_hex(fp_reg.getLong(rf1))
                + " F" + get_hex(mf3,1) + "=" + get_long_hex(fp_reg.getLong(rf3))
                + " F" + get_hex(mf2,1) + "=" + get_long_hex(fp_reg.getLong(rf2))
				+ " M4=" + get_hex(mf4,1)
				);
    }
	if (ex_mode){
		ex_restore();
	}
	psw_loc = psw_loc + 4;
}
private void ins_setup_rs(){  // "RS" 25  oorrbddd
	/*
	 * fetch rf1,rf3,bf2,df2 and update psw 
	 */
	rf1 = mem.get(psw_loc + 1) & 0xff;
	mf3 = rf1 & 0xf;
	rf3 = mf3 << 3;
	rf1 = (rf1 & 0xf0) >> 1;
	mf1 = rf1 >> 3;
	bf2 = mem.getShort(psw_loc + 2) & 0xffff;
	df2 = bf2 & 0xfff;
	bf2 = (bf2 & 0xf000)  >> 9;
	if (bf2 > 0){
		bd2_loc = (reg.getInt(bf2+4) & psw_amode) + df2;
	} else {
		bd2_loc = df2;
	}
	psw_ins_len = 4;
    if  (opt_trace){
    	if (opcode1 == 0xbd     // CLM
    		|| opcode1 == 0xbe   // STCM
    		|| opcode1 == 0xbf){  // ICM
    		put_ins_trace(
          		  " R" + get_hex(mf1,1) + "=" + get_hex(reg.getInt(rf1+4),8)
	                + " M3=" + get_hex(mf3,1)
					+ " S2(" + get_hex(bd2_loc,8)
					+ ")=" + bytes_to_hex(mem_byte,bd2_loc,mask_bits[mf3],0)
					);
    	} else if (opcode1 >= 0x88 && opcode1 <= 0x8f){
            put_ins_trace(
          		  " R" + get_hex(mf1,1) + "=" + get_hex(reg.getInt(rf1+4),8)
  				+ " S2(" + get_hex(bd2_loc,8)
  				+ ")" 
  				);
    	} else {
            put_ins_trace(
        		  " R" + get_hex(mf1,1) + "=" + get_hex(reg.getInt(rf1+4),8)
                + " R" + get_hex(mf3,1) + "=" + get_hex(reg.getInt(rf3+4),8)
				+ " S2(" + get_hex(bd2_loc,8)
				+ ")="   + bytes_to_hex(mem_byte,bd2_loc,4,0)
				);
    	}
    }
	if (ex_mode){
		ex_restore();
	}
	psw_loc = psw_loc + 4;
}
private void ins_setup_rsi(){  // "RSI" 4 BRXH  oorriiii
	psw_ins_len = 4;
	rf1 = mem.get(psw_loc + 1) & 0xff;
	mf3 = rf1 & 0xf;
	rf3 = mf3 << 3;
	rf1 = (rf1 & 0xf0) >> 1;
	mf1 = rf1 >> 3;
	if2 = mem.getShort(psw_loc+2);
    if  (opt_trace){
   	    if (opcode1 >= 0x84
   	   	 	    && opcode1 <= 0x87){
   	   	 	rf2 = psw_loc + 2*if2;
   	        put_ins_trace(
   	      		" R" + get_hex(mf1,1) + "=" + get_hex(reg.getInt(rf1+4),8)
		      + " R" + get_hex(mf3,1) + "=" + get_hex(reg.getInt(rf3+4),8)
   	          + " S2(" + get_hex(rf2,8)
   			  + ")=" + get_ins_target(rf2)
   	   		  );
   	    } else {
            put_ins_trace(
      		    " R" + get_hex(mf1,1) + "=" + get_hex(reg.getInt(rf1+4),8)
              + " R" + get_hex(mf3,1) + "=" + get_hex(reg.getInt(rf3+4),8)
				+ " I2=" + get_hex(if2,8)
				);
   	    }
    }
	if (ex_mode){
		ex_restore();
	}
	psw_loc = psw_loc + 4;
}
private void ins_setup_rsl(){  // "RSL" 1  TP  oor0bddd00oo
	psw_ins_len = 6;
	rflen1 = ((mem.get(psw_loc + 1) & 0xff) >> 4) + 1;
	bf1 = mem.getShort(psw_loc + 2) & 0xffff;
	df1 = bf1 & 0xfff;
	bf1 = (bf1 & 0xf000)  >> 9;
	if (bf1 > 0){
		bd1_loc = (reg.getInt(bf1+4) & psw_amode) + df1;
	} else {
		bd1_loc = df1;
	}
    if  (opt_trace){
        put_ins_trace(
			   " S1(" + get_hex(bd1_loc,8)
             + ")=" + bytes_to_hex(mem_byte,bd1_loc,rflen1,0)
			 );
    }
	if (ex_mode){
		ex_restore();
	}
	psw_loc = psw_loc + 6;
}
private void ins_setup_rsy(){  // "RSY" 31  LMG  oorrbdddhhoo
	psw_ins_len = 6;
	rf1 = mem.get(psw_loc + 1) & 0xff;
	mf3 = rf1 & 0xf;
	rf3 = mf3 << 3;
	rf1 = (rf1 & 0xf0) >> 1;
	mf1 = rf1 >> 3;
	bf2 = mem.getShort(psw_loc + 2) & 0xffff;
	df2 = bf2 & 0xfff;
	bf2 = (bf2 & 0xf000)  >> 9;
	if (bf2 > 0){
		bd2_loc = (reg.getInt(bf2+4) & psw_amode) + df2;
	} else {
		bd2_loc = df2;
	}
    if  (opt_trace){
    	if (opcode2 == 0x20      // CLMH 
    		|| opcode2 == 0x2C  // STCH 
			|| opcode2 == 0x80 // ICMH
			){
            put_ins_trace(
              	  " R" + get_hex(mf1,1) + "=" + get_long_hex(reg.getLong(rf1))
  		        + " M3=" + get_hex(mf3,1)
  				+ " S2(" + get_hex(bd2_loc,8)
				+ ")="   + bytes_to_hex(mem_byte,bd2_loc,mask_bits[mf3],0)
  				);
    	} else if (opcode2 >= 0x21         // CLMY
           			|| opcode2 == 0x2D     // STCY 
       				|| opcode2 >= 0x81){  // ICMY
            put_ins_trace(
            	  " R" + get_hex(mf1,1) + "=" + get_hex(reg.getInt(rf1+4),8)
		        + " M3=" + get_hex(mf3,1)
				+ " S2(" + get_hex(bd2_loc,8)
				+ ")=" + bytes_to_hex(mem_byte,bd2_loc,mask_bits[mf3],0)
				);
    	} else if (opcode2 == 0x1c || opcode2 == 0x1d){
    		put_ins_trace(
  				  " R" + get_hex(mf1,1) + "=" + get_long_hex(reg.getLong(rf1))
                  + " R" + get_hex(mf3,1) + "=" + get_long_hex(reg.getLong(rf3))
  				+ " S2(" + get_hex(bd2_loc,8)
  				+ ")"
  				);
    	} else {
    		put_ins_trace(
				  " R" + get_hex(mf1,1) + "=" + get_hex(reg.getInt(rf1+4),8)
                + " R" + get_hex(mf3,1) + "=" + get_hex(reg.getInt(rf3+4),8)
				+ " S2(" + get_hex(bd2_loc,8)
				+ ")="   + bytes_to_hex(mem_byte,bd2_loc,4,0)
				);
    	}
    }
	if (ex_mode){
		ex_restore();
	}
	psw_loc = psw_loc + 6;
}
private void ins_setup_rx(){  // "RX" 52  L  oorxbddd
	/*
	 * fetch rf1,xf2,bf2,df2 and update psw 
	 */
	rf1 = mem.get(psw_loc + 1) & 0xff;
	xf2 = (rf1 & 0xf) << 3;
	rf1 = (rf1 & 0xf0) >> 1;
	mf1 = rf1 >> 3;
	bf2 = mem.getShort(psw_loc + 2) & 0xffff;
	df2 = bf2 & 0xfff;
	bf2 = (bf2 & 0xf000) >> 9;
	if (xf2 > 0){
		xbd2_loc = (reg.getInt(xf2+4) & psw_amode) + df2;
	} else {
		xbd2_loc = df2;
	}
	if (bf2 > 0){
		xbd2_loc = xbd2_loc + (reg.getInt(bf2+4) & psw_amode);
	} 
	psw_ins_len = 4;
    if  (opt_trace){
    	if (opcode1 == 0x47){
    	   put_ins_trace(
               " S2(" + get_hex(xbd2_loc,8)
             + ")=" + get_ins_target(xbd2_loc)
    			 );
    	} else if (opcode1 == 0x45){
     	   put_ins_trace(
    		    " R" + get_hex(mf1,1) + "=" + get_hex(reg.getInt(rf1+4),8)
              + " S2(" + get_hex(xbd2_loc,8)
              + ")=" + get_ins_target(xbd2_loc)
     			 );
    	} else if (opcode1 == 0x41){
   	        put_ins_trace(
				  " R" + get_hex(mf1,1) + "=" + get_hex(reg.getInt(rf1+4),8)
				+ " S2(" + get_hex(xbd2_loc & psw_amode,8) + ")"
				);
    	} else if (opcode1 >= 0x60 && opcode1 <= 0x7f){
   	        put_ins_trace(
  				  " F" + get_hex(mf1,1) + "=" + get_fp_long_hex(rf1)
  				+ " S2(" + get_hex(xbd2_loc,8)
                  + ")=" + bytes_to_hex(mem_byte,xbd2_loc,4,0)
  				);
    	} else {
   	        put_ins_trace(
				  " R" + get_hex(mf1,1) + "=" + get_hex(reg.getInt(rf1+4),8)
				+ " S2(" + get_hex(xbd2_loc,8)
                + ")=" + bytes_to_hex(mem_byte,xbd2_loc,4,0)
				);
    	}
    }
	if (ex_mode){
		ex_restore();
	}
	psw_loc = psw_loc + 4;
}
private void ins_setup_rxf(){  // "RXF" 8   MAE  oorxbdddr0oo (note r3 before r1)
	psw_ins_len = 6;
	rf3 = mem.get(psw_loc + 1) & 0xff;
	xf2 = (rf3 & 0xf) << 3;
	rf3 = (rf3 & 0xf0) >> 1;
	mf3 = rf3 >> 3;
	rf1 = mem.get(psw_loc + 4) & 0xf0;
	rf1 = rf1 >> 1;
    mf1 = rf1 >> 3;
	bf2 = mem.getShort(psw_loc + 2) & 0xffff;
	df2 = bf2 & 0xfff;
	bf2 = (bf2 & 0xf000) >> 9;
	if (xf2 > 0){
		xbd2_loc = (reg.getInt(xf2+4) & psw_amode) + df2;
	} else {
		xbd2_loc = df2;
	}
	if (bf2 > 0){
		xbd2_loc = xbd2_loc + (reg.getInt(bf2+4) & psw_amode);
	} 
    if  (opt_trace){
        put_ins_trace(
      		      " R" + get_hex(mf1,1) + "=" + get_hex(reg.getInt(rf1+4),8)
                + " R" + get_hex(mf3,1) + "=" + get_hex(reg.getInt(rf3+4),8)
				+ " S2(" + get_hex(bd2_loc,8)
				+ ")="   + bytes_to_hex(mem_byte,bd2_loc,4,0)
				);
    }
	if (ex_mode){
		ex_restore();
	}
	psw_loc = psw_loc + 6;
}
private void ins_setup_rxe(){  // "RXE" 28  ADB oorxbddd00oo
	psw_ins_len = 6;
	rf1 = mem.get(psw_loc + 1) & 0xff;
	xf2 = (rf1 & 0xf) << 3;
	rf1 = (rf1 & 0xf0) >> 1;
    mf1 = rf1 >> 3;
	bf2 = mem.getShort(psw_loc + 2) & 0xffff;
	df2 = bf2 & 0xfff;
	bf2 = (bf2 & 0xf000) >> 9;
	if (xf2 > 0){
		xbd2_loc = (reg.getInt(xf2+4) & psw_amode) + df2;
	} else {
		xbd2_loc = df2;
	}
	if (bf2 > 0){
		xbd2_loc = xbd2_loc + (reg.getInt(bf2+4) & psw_amode);
	} 
    if  (opt_trace){
        put_ins_trace(
				      " F" + get_hex(mf1,1) + "=" + get_fp_long_hex(rf1)
					+ " S2(" + get_hex(xbd2_loc,8)
	                + ")=" + bytes_to_hex(mem_byte,xbd2_loc,8,0)
					);
        		
    }
	if (ex_mode){
		ex_restore();
	}
	psw_loc = psw_loc + 6;
}
private void ins_setup_rxy(){ // "RXY" 76 MLG oorxbdddhhoo
	psw_ins_len = 6;
	rf1 = mem.get(psw_loc + 1) & 0xff;
	xf2 = (rf1 & 0xf) << 3;
	rf1 = (rf1 & 0xf0) >> 1;
	mf1 = rf1 >> 3;
	bf2 = mem.getShort(psw_loc + 2) & 0xffff;
	df2 = bf2 & 0xfff;
	bf2 = (bf2 & 0xf000) >> 9;
	if (xf2 > 0){
		xbd2_loc = (reg.getInt(xf2+4) & psw_amode) + df2;
	} else {
		xbd2_loc = df2;
	}
	if (bf2 > 0){
		xbd2_loc = xbd2_loc + (reg.getInt(bf2+4) & psw_amode);
	} 
    if  (opt_trace){
    	if (opcode1 == 0x47){
    	   put_ins_trace(
                   " S2(" + get_hex(xbd2_loc,8)
                 + ")=" + get_ins_target(xbd2_loc)
    			 );
    	} else {
   	        put_ins_trace(
				  " R" + get_hex(mf1,1) + "=" + get_long_hex(reg.getLong(rf1))
			    + " S2(" + get_hex(xbd2_loc,8)
                + ")=" + bytes_to_hex(mem_byte,xbd2_loc,8,0)
				);
    	}
    }
	if (ex_mode){
		ex_restore();
	}
	psw_loc = psw_loc + 6;
}
private void ins_setup_s(){  // "S" 43 SPM oo00bddd
	bf2 = mem.getShort(psw_loc + 2) & 0xffff;
	df2 = bf2 & 0xfff;
	bf2 = (bf2 & 0xf000) >> 9;
	if (bf2 > 0){
		bd2_loc = (reg.getInt(bf2+4) & psw_amode) + df2;
	} else {
		bd2_loc = df2;
	}
	psw_ins_len = 4;
    if  (opt_trace){
        put_ins_trace(
                  " S2(" + get_hex(bd2_loc,8)
                + ")=" + bytes_to_hex(mem_byte,bd2_loc,8,0)
				);
    }
	if (ex_mode){
		ex_restore();
	}
	psw_loc = psw_loc + 4;
}
private void ins_setup_si(){  // "SI" 9 CLI  ooiibddd
	/*
	 * fetch bd1,if2
	 */
	if2 = mem.get(psw_loc + 1) & 0xff;
	bf1 = mem.getShort(psw_loc + 2) & 0xffff;
	df1 = bf1 & 0xfff;
	bf1 = (bf1 & 0xf000) >> 9;
	if (bf1 > 0){
		bd1_loc = (reg.getInt(bf1+4) & psw_amode) + df1;
	} else {
		bd1_loc = df1;
	}
	psw_ins_len = 4;
    if  (opt_trace){
        put_ins_trace(
                  " S2(" + get_hex(bd1_loc,8)
                + ")=" + bytes_to_hex(mem_byte,bd1_loc,1,0)
				+ " I2=" + get_hex(if2,2)
				);
    }
	if (ex_mode){
		ex_restore();
	}
	psw_loc = psw_loc + 4;
}
private void ins_setup_siy(){  // "SIY" 6  TMY  ooiibdddhhoo
	psw_ins_len = 6;
	if2 = mem.get(psw_loc + 1) & 0xff;
	bf1 = mem.getShort(psw_loc + 2) & 0xffff;
	df1 = bf1 & 0xfff;
	bf1 = (bf1 & 0xf000) >> 9;
	if (bf1 > 0){
		bd1_loc = (reg.getInt(bf1+4) & psw_amode) + df1;
	} else {
		bd1_loc = df1;
	}
    if  (opt_trace){
        put_ins_trace(
                  " S2(" + get_hex(bd1_loc,8)
                + ")=" + bytes_to_hex(mem_byte,bd1_loc,1,0)
				+ " I2=" + get_hex(if2,2)
				);
    }
	if (ex_mode){
		ex_restore();
	}
	psw_loc = psw_loc + 6;
}
private void ins_setup_ssp(){   // AP SS2  oollbdddbddd
	/*
	 * fetch rflen1, rflen2, bd1_loc, and bd2_loc
	 * and update psw
	 */
	    rflen1 = mem_byte[psw_loc+1] & 0xff;
		rflen2 = (rflen1 & 0xf) + 1;
		rflen1 = ((rflen1 >> 4) & 0xf) + 1;
		bf1 = mem.getShort(psw_loc + 2) & 0xffff;
		df1 = bf1 & 0xfff;
		bf1 = (bf1 & 0xf000) >> 9;
		if (bf1 > 0){
			bd1_loc = (reg.getInt(bf1+4) & psw_amode) + df1;
		} else {
			bd1_loc = df1;
		}
	    bf2 = mem.getShort(psw_loc + 4) & 0xffff;
		df2 = bf2 & 0xfff;
		bf2 = (bf2 & 0xf000) >> 9;
		if (bf2 > 0){
			bd2_loc = (reg.getInt(bf2+4) & psw_amode) + df2;
		} else {
			bd2_loc = df2;
		} 
		psw_ins_len = 6;
	    if  (opt_trace){
	    	int maxlen1 = rflen1;
	    	int maxlen2 = rflen2;
	    	if (maxlen1 > 4)maxlen1 =4;
	    	if (maxlen2 > 4)maxlen2 =4;
	        put_ins_trace(
	        		       " S1(" + get_hex(bd1_loc,8)
						+  ")=" + bytes_to_hex(mem_byte,bd1_loc,maxlen1,0)
		        		+  " S2(" + get_hex(bd2_loc,8)
		                +  ")=" + bytes_to_hex(mem_byte,bd2_loc,maxlen2,0)
					);
	    }
		if (ex_mode){
			ex_restore();
		}
	    psw_loc = psw_loc + 6;
}
private void ins_setup_ss(){  // "SS" 32  MVC oollbdddbddd  
	/*
	 * fetch rflen,bd1_loc, bd2_loc, and update psw 
	 */
    rflen = (mem_byte[psw_loc+1] & 0xff) + 1;
	bf1 = mem.getShort(psw_loc + 2) & 0xffff;
	df1 = bf1 & 0xfff;
	bf1 = (bf1 & 0xf000) >> 9;
	if (bf1 > 0){
		bd1_loc = (reg.getInt(bf1+4) & psw_amode) + df1;
	} else {
		bd1_loc = df1;
	}
    bf2 = mem.getShort(psw_loc + 4) & 0xffff;
	df2 = bf2 & 0xfff;
	bf2 = (bf2 & 0xf000) >> 9;
	if (bf2 > 0){
		bd2_loc = (reg.getInt(bf2+4) & psw_amode) + df2;
	} else {
		bd2_loc = df2;
	} 
	psw_ins_len = 6;
    if  (opt_trace){
    	int maxlen = rflen;
    	if (maxlen > 4)maxlen = 4;
        put_ins_trace(
        		   " S1(" + get_hex(bd1_loc,8)
				+  ")=" + bytes_to_hex(mem_byte,bd1_loc,maxlen,0)
        		+  " S2(" + get_hex(bd2_loc,8)
                +  ")=" + bytes_to_hex(mem_byte,bd2_loc,maxlen,0)
				);
    }
	if (ex_mode){
		ex_restore();
	}
	psw_loc = psw_loc + 6;
}
private void ins_setup_sse(){  // "SSE" 5  LASP  oooobdddbddd
	psw_ins_len = 6;
	bf1 = mem.getShort(psw_loc + 2) & 0xffff;
	df1 = bf1 & 0xfff;
	bf1 = (bf1 & 0xf000) >> 9;
	if (bf1 > 0){
		bd1_loc = (reg.getInt(bf1+4) & psw_amode) + df1;
	} else {
		bd1_loc = df1;
	}
    bf2 = mem.getShort(psw_loc + 4) & 0xffff;
	df2 = bf2 & 0xfff;
	bf2 = (bf2 & 0xf000) >> 9;
	if (bf2 > 0){
		bd2_loc = (reg.getInt(bf2+4) & psw_amode) + df2;
	} else {
		bd2_loc = df2;
	} 
    if  (opt_trace){
        put_ins_trace(
     		      " S1(" + get_hex(bd1_loc,8)
			   +  ")=" + bytes_to_hex(mem_byte,bd1_loc,4,0)
       		   +  " S2(" + get_hex(bd2_loc,8)
               +  ")=" + bytes_to_hex(mem_byte,bd2_loc,4,0)
					);
    }
	if (ex_mode){
		ex_restore();
	}
	psw_loc = psw_loc + 6;
}
private void put_ins_trace(String ins_parms){
	/* 
	 * put trace line to log file with ins parms
	 * and process test mode commands if option test
	 */
    put_log(" " + get_hex(psw_loc | psw_amode_bit,8) 
			+ " " + psw_cc_code[psw_cc]
    		+ " " + get_ins_hex(psw_loc)
			+ " " + get_ins_name(psw_loc)
			+ ins_parms
			);
}
private String get_ins_name(int ins_loc){
	/*
	 * return opcode or ????? for given op1 and op2
	 */
	int op1 = mem.get(ins_loc) & 0xff;
	int op2 = -1;
	int op2_offset  = opcode2_offset[op1];
	if (op2_offset > 0){
		switch (opcode2_mask[op1]){
		case 0xff: // full byte
			op2 = mem.get(ins_loc + op2_offset) & 0xff;
			break;
		case 0xf0: // left nibble
			op2 = (mem.get(ins_loc + op2_offset) >> 4) & 0x0f;
			break;
		case 0x0f: // right nibble
		    op2 = mem.get(ins_loc + op2_offset) & 0x0f;
		    break;
	    default:
	    	abort_error(59,"invalid opcode2 mask");
		}
	}
    String ins_name = null;
    String hex_op1 = null;
    String hex_op2 = null;
    String hex_key = null;
	hex_op1 = Integer.toHexString(op1).toUpperCase();
	if (hex_op1.length() == 1){
		hex_op1 = "0" + hex_op1; 
	}
	if (op2 == -1){
		hex_op2 = "";
	} else {
	   hex_op2 = Integer.toHexString(op2).toUpperCase();
	   if (hex_op2.length() == 1){
		   hex_op2 = "0" + hex_op2; 
	   }
	}
	if (op1 == 0x47 || op1 == 0x07){
		hex_key = "BC=" + hex_op1 + "0" + Integer.toHexString(mf1).toUpperCase();
	} else if (op1 == 0xa7 && ((op2 & 0xf) == 4)){
		hex_key = "BR=" + hex_op1 + "4" + Integer.toHexString(mf1).toUpperCase();
	} else {
	    hex_key = hex_op1 + hex_op2;
	}
	int index = find_key_index(hex_key);
	if (index != -1){
		ins_name = op_name[index];
		if (ins_name.length() < 5){
			ins_name = (ins_name + "     ").substring(0,5);
		}
	} else {
		ins_name = "?????";
	}
	return ins_name;
}
private void process_test_cmd(){
	/*
	 * process test option interactive debug commands
	 * 
	 * 1.  Check for reg, memory, or opcode break
	 *     and set count = 0 if hit.
	 * 2.  Decrement go count if positive.
	 * 3.  if go count = 0
	 *        read, parse, and execute test commands
	 *        until go or quit command executed.
	 *      
	 *  Notes:
	 *   1.  Get next command from z390 gui
	 *       cmd line, system command_line, or
	 *       from ddname file specified in test
	 *       option test(ddname)..
	 *  
	 */
	if (test_break_reg_mode){
       check_test_break_reg();
	}
    if (test_break_mem_mode){
       check_test_break_mem();
    }
    if (test_break_op_mode){
    	check_test_break_op();
    }
    if (test_trace_count > 0){
    	test_trace_count--;
    	if (test_trace_count == 0){
    		trace_psw();
    	}
    }
    while (test_trace_count == 0){
    	test_cmd_abort = false;
    	get_test_cmd();
    	if (test_cmd != null){
    		parse_test_cmd();
    		if (tot_test_tokens > 0){
    		    exec_test_cmd();
    		}
    	}   	
    }
}
private void get_test_cmd(){
	/*
	 * get next test command from gui command line
	 * or system command line or file specified
	 * with the test(ddname) option
	 */
	if (z390_command_text != null){
		try {
			z390_command_text.wait();
			test_cmd = z390_command_text.getText();
			z390_command_text.setText("");
		} catch (Exception e){
			put_log("test error i/o on z390 command line");
			opt_test = false; // turn off and zoom
			test_cmd = "Z";
		}
	} else {
		try {
			if (test_ddname == null){
				put_log("test enter command or h for help");
			}
			test_cmd = test_cmd_file.readLine();
			if (test_ddname != null && test_cmd == null){
				test_cmd = "Q";
			}
		} catch (Exception e){
			put_log("test error i/o on command line");
			opt_test = false; // turn off and zoom
			test_cmd = "Z";
		}
	}
}
private void parse_test_cmd(){
	/*
	 * parse test command into array of tokens
	 * using regular expression parser
	 */
	int index = 0;
	if (test_cmd != null && test_cmd.length() > 0){
		put_log("test cmd: " + test_cmd);
	    test_match = test_pattern.matcher(test_cmd);
	    index = 0;
	    while (test_match.find() && index < max_test_tokens){
	    	test_token[index] = test_match.group();
            index++;
	    }
	    tot_test_tokens = index;
	} else {
		put_log("test error on cmd - " + test_cmd);
	}
}
private void check_test_break_reg(){
	/*
	 * check for test mode break on register value
	 */
	test_break_reg_val = reg.getLong(test_break_reg_loc);
	test_break_reg = false;
	switch (test_break_reg_compare){
	case 0: // = 
		if (test_break_reg_val == test_break_reg_sdt){
			test_break_reg = true;
		}
		break;
	case 1: // !=
		if (test_break_reg_val != test_break_reg_sdt){
			test_break_reg = true;
		}
		break;
	case 2: // >
		if (test_break_reg_val > test_break_reg_sdt){
			test_break_reg = true;
		}
		break;
	case 3: // >=
		if (test_break_reg_val >= test_break_reg_sdt){
			test_break_reg = true;
		}
		break;
	case 4: // <
		if (test_break_reg_val < test_break_reg_sdt){
			test_break_reg = true;
		}
		break;
	case 5: // <=
		if (test_break_reg_val <= test_break_reg_sdt){
			test_break_reg = true;
		}
		break;
	default:
		put_log("test error - invalid reg break compare - ignored");
    }
    if (test_break_reg){
    	test_trace_count = 0;
    	put_log("test break on " + test_break_reg_cmd);
    	dump_regs(test_break_reg_loc);
    	trace_psw();
    }
}
private void check_test_break_mem(){
	/*
	 * check for test break on memory value change
	 */
    test_break_mem = false;
    int index = 0;
    test_break_mem_equal = 0; // count equal bytes
    while (index < test_break_mem_sdt.length){
	    test_break_mem_byte = mem.get(test_break_mem_loc + index);
	    switch (test_break_mem_compare){
        case 0: // = 
        	if (test_break_mem_byte == test_break_mem_sdt[index]){
        		test_break_mem_equal++;
        	}
        	break;
        case 1: // !=
        	if (test_break_mem_byte != test_break_mem_sdt[index]){
        		test_break_mem = true;
        		index = test_break_mem_sdt.length;
        	}
        	break;
        case 2: // >
        	if (test_break_mem_byte > test_break_mem_sdt[index]){
        		test_break_mem = true;
        		index = test_break_mem_sdt.length;
        	}
        	break;
        case 3: // >=
        	if (test_break_mem_byte >= test_break_mem_sdt[index]){
        		test_break_mem = true;
        		index = test_break_mem_sdt.length;
        	}
        	break;
        case 4: // <
        	if (test_break_mem_byte < test_break_mem_sdt[index]){
        		test_break_mem = true;
        		index = test_break_mem_sdt.length;
        	}
        	break;
        case 5: // <=
        	if (test_break_mem_byte <= test_break_mem_sdt[index]){
        		test_break_mem = true;
        		index = test_break_mem_sdt.length;
        	}
        	break;
        default:
        	put_log("test error - invalid memory break compare - ignored");
	    }
	    index++;
    }
	if (test_break_mem_compare == 0  // = 
	   	&& test_break_mem_equal == test_break_mem_sdt.length){
	   	test_break_mem = true;
	}
	if (test_break_mem){
        test_trace_count = 0;
    	put_log("test break on " + test_break_mem_cmd);
        dump_mem(test_break_mem_loc,test_break_mem_sdt.length);
        trace_psw();
	}
}

private void check_test_break_op(){
	/*
	 * check for test mode break on opcode
	 * at current psw address
	 */
	if ((mem.get(psw_loc) & 0xff) == test_break_op1){
		if (test_break_op2_index == 0
		    || ((mem.get(psw_loc+test_break_op2_index) & test_break_op2_mask) 
				   == test_break_op2)){
		      test_trace_count = 0;
		      test_break_op_mode = false;
		      put_log("test break on " + test_break_op_cmd);
		      trace_psw();
		}
	}
}
private void exec_test_cmd(){
	/*
	 * parse and execute current test command
	 */
	// assume 1 character operator
	test_opcode = test_token[0].toUpperCase().charAt(0);
	if (test_token[0].length() > 1 // reg|mem=sdt
		|| (test_opcode == '*'
			&& tot_test_tokens > 1 // *=sdt
		    && test_token[1].charAt(0) == '=')
		|| (test_opcode >= '0'     // n=sdt
			&& test_opcode <= '9')
		){
		test_addr = get_test_addr(test_token[0]);
		if (!test_cmd_abort){
			if (test_addr_type == test_addr_mem){
				test_opcode = 'A'; // addr memory change
			} else {
				test_opcode = 'R'; // reg change
			}
		}
	}
	switch (test_opcode){
	case '*': // * with no = following is comment
		break;
	case 'A': // memory set
        test_mem_loc = test_addr;
		test_mem_sdt = get_test_mem_sdt(test_token[2]);
		int index = 0;
		while (index < test_mem_sdt.length){
			mem.put(test_mem_loc+index,test_mem_sdt[index]);
		    index++;
		}
		dump_mem(test_mem_loc,test_mem_sdt.length);
		break;
	case 'B':  // set base for rel addr of memory
		if (tot_test_tokens == 3){
			test_base_addr = get_test_addr(test_token[2]);
		    dump_mem(test_base_addr,16);
		} 
		break;
	case 'G':  // go nn ins or until reg/mem/op break
        go_test();
	    break;
	case 'H':  // help
	    put_log("test command help summary");
	    put_log("  reg=sdt     set regster value (ie 15r=8 changes reg 15 to 8)");
	    put_log("  B=addr      set base for rel addr (ie B=15r% sets base to (r15) 24 bit");
	    put_log("  addr=sdt    set memory value  (ie 1r?=x'80' changes mem at (r1) 31 bit");
	    put_log("  G nn/opcode go exec n instr. or until next opcode/reg/mem break");
	    put_log("  H           list help command summary");
	    put_log("  J addr      jump to new addr and trace instruction");
	    put_log("  L           list all regs and trace current instruction");
	    put_log("  L reg       list contents of register (ie l 1r dumps register 1");
	    put_log("  L addr len  list contents of memory area (ie l 10. 4 dumps cvt addr");
	    put_log("  Q           quit execution now");
	    put_log("  S           clear register and memory breaks");
	    put_log("  S reg??sdt  set break on register change");
	    put_log("  S addr??sdt set break on memory change");
	    put_log("  T nn/opcode trace n instr. or until next opcode/reg/mem break");
	    put_log("  Z nn/opcode zoom n instr. or until next opcode/reg/mem break");
	    put_log("* addr = hex.,+-hex, *+-hex, dec, nnr% (24 bit), nnr? (31 bit)");
	    put_log("* reg  = nnr where nn = 0-15");
	    put_log("* sdt  = self defining term (b'01',c'ab',f'1',h'2',x'ff')");
	    put_log("* ??   = break compare operator (=,!=,<,<=,>,>=)");
	    put_log("for more information visit www.z390.org");
        break;
	case 'J':
		if (tot_test_tokens == 2){
			set_psw_loc(get_test_addr(test_token[1]));
			if (!test_cmd_abort){
				trace_psw();
			}
		}
		break;
	case 'L':  // list reg or memory
		if (tot_test_tokens == 1){
			dump_regs(-1);
			trace_psw();
		} else {
			test_addr = get_test_addr(test_token[1]);
			if (test_addr_type == test_addr_reg){
				test_reg_loc = test_addr * 8;
				dump_regs(test_reg_loc);
			} else if (test_addr_type == test_addr_mem){
				test_mem_loc = test_addr;
				if (tot_test_tokens == 3){
					test_mem_len = get_test_addr(test_token[2]);
				} else {
					test_mem_len = 32;
				}
			    if (!test_cmd_abort
					&& test_mem_len > 0
					&& test_mem_loc + test_mem_len <= tot_mem){
			    	dump_mem(test_mem_loc,test_mem_len);
			    }
			}
		}
        break;
	case 'Q':
		abort_error(57,"test quiting now");
		break;
	case 'R':  // reg set
    	test_reg_loc = test_addr * 8;
		test_reg_sdt = get_test_reg_sdt(test_token[2]);
		reg.putLong(test_reg_loc,test_reg_sdt);
		dump_regs(test_reg_loc);
		break;
	case 'S':  // set break on reg or memory change
		if (tot_test_tokens == 1){
			put_log("test breaks off");
			test_break_reg_mode = false;
			test_break_mem_mode = false;
			test_break_op_mode = false;
		} else if (tot_test_tokens == 4){
			test_addr    = get_test_addr(test_token[1]);
			test_compare = get_test_compare(test_token[2]);
			if (!test_cmd_abort){
				if (test_addr_type == test_addr_reg){
					set_test_break_reg();
					check_test_break_reg();
				} else {
					set_test_break_mem();
					check_test_break_mem();
				}
			}
		}
		break;
	case 'T':  // trace nn ins or until reg/mem/op break
        opt_trace = true;
        go_test();
	    break;
	case 'Z':  // zoom nn ins or until reg/mem/op break
        opt_trace = false;
        go_test();
	    break;
	default:
		put_log("test error - invalid command - " + test_cmd);
	}
}
private void go_test(){
	/*
	 * set count and go execute instructions
	 * until count 0 or break found or exit
	 */
	if (tot_test_tokens == 2){
		try {
			test_trace_count = Integer.valueOf(test_token[1]);
		} catch (Exception e){
			set_test_break_op();
			test_trace_count = -1; // go until break
			check_test_break_op();
		}
	} else {
		test_trace_count = -1; // go until break or exit
	}
}
private void set_test_break_reg(){
	/*
	 * set test break on register change
	 */
	test_break_reg_mode = true;
	test_break_reg_cmd = test_cmd;
	test_break_reg_loc = test_addr * 8;
    test_break_reg_compare = test_compare;
	test_break_reg_sdt = get_test_reg_sdt(test_token[3]);
	dump_regs(test_break_reg_loc);
}
private void set_test_break_mem(){
	/*
	 * set test break on memory change
	 */
	test_break_mem_mode = true;
	test_break_mem_cmd = test_cmd;
	test_break_mem_loc = test_addr;
    test_break_mem_compare = test_compare;
	test_break_mem_sdt = get_test_mem_sdt(test_token[3]);			
    dump_mem(test_break_mem_loc,test_break_mem_sdt.length);
}
private void set_test_break_op(){
	/*
	 * set break on opcode at current psw
	 */
	int index = find_key_index(test_token[1].toUpperCase());
	if (index != -1){
		test_break_op_mode = true;
		test_break_op_cmd  = test_cmd;
		test_break_op1 = Integer.valueOf(op_code[index].substring(0,2),16).byteValue() & 0xff;
		test_break_op2_index = opcode2_offset[test_break_op1];
		if (op_code[index].length() == 4){
			test_break_op2 = Integer.valueOf(op_code[index].substring(2,4),16).byteValue();
			test_break_op2_mask = 0xff;
		} else if (op_code[index].length() == 3){
			if (opcode2_mask[test_break_op1] == 0xf0){
			    test_break_op2 = (Integer.valueOf(op_code[index].substring(2,3),16).intValue() << 4) & 0xff;
			    test_break_op2_mask = 0xf0;
			} else {
				test_break_op2 = Integer.valueOf(op_code[index].substring(2,3),16).intValue() & 0xff;
			    test_break_op2_mask = 0x0f;
			}
		} else {
			test_break_op2_index = 0; // no op2
		}
	} else {
		put_log("test invalid opcode - " + test_token[1]);
	}
}
private int get_test_addr(String text){
	/*
	 * get test address and set type
	 * 
	 * memory type address forms
	 *   dec  = absolute decimal address
	 *   hex. = absolute hex address
	 *   +hex = base + hex offset
	 *   -hex = base - hex offset
	 *   *     = psw_loc
	 *   *+hex = psw_loc + hex offset
	 *   *-hex = psw_loc - hex_offset
	 *   nr% indirect 24 bit 
	 *   nr? indirect 31 bit
	 * register type address forms
	 *    nnr or rnn
	 */
	test_addr_type = test_addr_mem;
	try {
		if (text.length() > 1){
			if (text.toUpperCase().charAt(text.length()-1) == 'R'){
				test_addr_type = test_addr_reg;
				return Integer.valueOf(text.substring(0,text.length()-1)).intValue();
			} else if (text.charAt(text.length()-1) == '.'){
				return Long.valueOf(text.substring(0,text.length()-1),16).intValue() & 0xffffffff;
			} else if (text.charAt(0) == '+'){
				return test_base_addr + Long.valueOf(text.substring(1),16).intValue() & 0xffffffff;
			} else if (text.charAt(0) == '-'){
				return test_base_addr - Long.valueOf(text.substring(1),16).intValue() & 0xffffffff;
			} else if (text.length() > 2 && text.substring(0,2).equals("*+")){
				return psw_loc + Long.valueOf(text.substring(2),16).intValue() & 0xffffffff;
			} else if (text.substring(0,2).equals("*-")){
				return psw_loc - Long.valueOf(text.substring(2),16).intValue() & 0xffffffff;
			} else if (text.toUpperCase().substring(text.length()-2).equals("R%")){
				return (reg.getInt(Integer.valueOf(text.substring(0,text.length()-2)).intValue()*8+4)) & psw_amode24;
			} else if (text.toUpperCase().substring(text.length()-2).equals("R?")){
				return (reg.getInt(Integer.valueOf(text.substring(0,text.length()-2)).intValue()*8+4)) & psw_amode31;
			} else {
				return Integer.valueOf(text).intValue();
			}
		} else if (text.charAt(0) == '*'){
			return psw_loc;
		} else { // assume single digit
			return Integer.valueOf(text).intValue();
		}
	} catch (Exception e){
		put_log("test error invalid addr - " + text);
		test_cmd_abort = true;
	}
	return -1;
}
private byte get_test_compare(String compare){
	/*
	 * set test compare code or issue error
	 *  0 - =
	 *  1 - !=
	 *  2 - >
	 *  3 - >=
	 *  4 - <
	 *  5 - <=
	 */
	if (compare.equals("=")){
		return 0;
	} else if (compare.equals("!=")){
		return 1;
	} else if (compare.equals("!=")){
		return 1;
	} else if (compare.equals(">")){
		return 2;
	} else if (compare.equals(">=")){
		return 3;
	} else if (compare.equals("<")){
		return 4;
	} else if (compare.equals("<=")){
		return 5;
	}
	put_log("test error invalid break compare - " + compare);
	test_cmd_abort = true;
	return -1;
}
private long get_test_reg_sdt(String text){
	/*
	 * return long sdt value for register
	 *    b'...'
	 *    c'...'
	 *    f'...'
	 *    h'...'
	 *    x'...'
	 *    or address hex., dec, nr%, nr?,
	 *       +hex, -hex, *+hex, *-hex
	 */
	if (text.length() <= 1 || text.charAt(1) != '\''){
		return get_test_addr(text) & 0xffffffff;
    } else {   
	  try {
		char type = text.toUpperCase().charAt(0);
		String data = text.substring(2,text.length()-1);
		switch (type){
		case 'B':
			return Long.valueOf(data,2);
		case 'C':
			int index = 0;
			long value = 0;
			int dcc_len = data.length();
			while (index < dcc_len){
				value = value * 0x100 + ascii_to_ebcdic[data.charAt(index)];
				index++;
				if (index < dcc_len 
					&& data.substring(index-1,index+1).equals("''")){
					index++;  // skip 2nd quote
					dcc_len--;
				}
			}
			return value;
		case 'F':
		case 'H':
			return Long.valueOf(data);
		case 'X':
			return Long.valueOf(data,16);
		default:
			put_log("test invalid reg sdt - " + text);
			test_cmd_abort = true;
		}
	} catch (Exception e){
		put_log("test invalid reg sdt - " + text);
		test_cmd_abort = true;
	 }
    }
	return -1;
}
private byte[] get_test_mem_sdt(String text){
	/*
	 * return memory sdt byte array
	 *    b'...'
	 *    c'...'
	 *    f'...'
	 *    h'...'
	 *    x'...'
	 */
	byte[] data_byte = null;
	int  index = 0;
	int  data_len = 0;
	int  data_byte_len = 0;
	long data_val = 0;
	try {
		char type = text.toUpperCase().charAt(0);
		String data_text = text.substring(2,text.length()-1);
		data_len = data_text.length();
		switch (type){
		case 'B':
			data_byte_len = (data_len + 7)/8;
			data_byte = new byte[data_byte_len];
			data_val = Long.valueOf(data_text,2);
			index = data_byte_len-1;
			while (index >= 0){
				data_byte[index] = (byte)(data_val & 0xff);
				data_val = data_val >>> 8;
		        index--;
			}
			return data_byte;
		case 'C':
			data_byte_len = data_len;
			int index1 = 0;
			int index2 = 0;
			while (index1 < data_len){
				index1++;
				index2++;
				if (index1 < data_len 
						&& data_text.substring(index1-1,index1+1).equals("''")){
					index1++; // skip 2nd single quote
				}
			}
			data_byte_len = index2;
			data_byte = new byte[data_byte_len];
			index1 = 0;
			index2 = 0;
			while (index1 < data_len){
				data_byte[index2] = ascii_to_ebcdic[data_text.charAt(index1)];
				index1++;
				index2++;
				if (index1 < data_len 
						&& data_text.substring(index1-1,index1+1).equals("''")){
					index1++; // skip 2nd single quote
				}
			}
			return data_byte;
		case 'F':
			data_byte_len = 4; 
			data_byte = new byte[4];
			data_val = Long.valueOf(data_text);
			index = 3;
			while (index >= 0){
				data_byte[index] = (byte)(data_val & 0xff);
				data_val = data_val >>> 8;
		        index--;
			}
			return data_byte;
		case 'H':
			data_byte_len = 2;
			data_byte = new byte[2];
			data_val = Long.valueOf(data_text);
			index = 1;
			while (index >= 0){
				data_byte[index] = (byte)(data_val & 0xff);
				data_val = data_val >>> 8;
		        index--;
			}
			return data_byte;
		case 'X':
			data_byte_len = (data_len + 1)/2; 
			data_byte = new byte[data_byte_len];
            if (data_text.length() < data_byte_len * 2){
            	data_text = "0" + data_text;
            }
			index = 0;
			while (index < data_byte_len){
				data_byte[index] = Integer.valueOf(data_text.substring(index*2,index*2+2),16).byteValue();
		        index++;
			}
			return data_byte;
		default:
			put_log("test invalid mem sdt - " + text);
			test_cmd_abort = true;
		}
	} catch (Exception e){
		put_log("test invalid mem sdt - " + text);
		test_cmd_abort = true;
	}
	return data_byte;
}
private void ex_restore(){
	/*
	 * restore ex target instruction 2nd byte
	 */
	mem.put(psw_loc+1,ex_mod_byte);
}
/*
 ****************************************
 * instruction support routines
 * **************************************
 */
private void set_psw_check(int pic_type){
	/*
	 * set psw_pic interruption type
	 * if pgm mask type interrupt
	 *    if mask bit 0, set CC3 and continue.
	 *    if espie exit defined, exit to it
	 * else set psw_check for exit to abend handler
	 */
	psw_pic = pic_type;
	if (psw_pic == psw_pic_exit){
		psw_check = true; // normal exit
	} else {
		psw_pic_handler();
	}
}
private void set_psw_amode(int amode_bit){
	/* 
	 * set psw_amode based on amode high bit
	 */
	if ((amode_bit & psw_amode31_bit) != 0){
		psw_amode     = psw_amode31;
		psw_amode_bit = psw_amode31_bit;
	} else {
		psw_amode     = psw_amode24;
		psw_amode_bit = psw_amode24_bit;
	}
    psw_amode_high_bits = (-1) ^ psw_amode;
}
private void set_psw_loc(int addr){
	/*
	 * set psw_loc based on psw_amode
	 * and turn off ex_mode
	 */
	psw_loc = addr & psw_amode;
	ex_mode = false;
}
private int get_tm_reg_cc(int reg,int mask){
	/*
	 * return psw_cc for tm reg
	 */
    int bits = reg & mask;
    if (bits == 0){
    	return psw_cc0;
    } else if (bits == reg){
    	return psw_cc3;
    } else if (bits >= 0x8000){
    	return psw_cc2;
    } else {
    	return psw_cc1;
    }
}
private int get_sla32(int int_reg,int shift_count){
	/*
	 * return int shifted left arith
	 * and set psw_cc
	 */
    long long_result = ((long) int_reg) << shift_count;
    if (int_reg > 0){
    	if ((long_result & long_sign_bits) != 0){
            psw_cc = psw_cc3;
            long_result = long_result & int_num_bits;
    	} else {
    		psw_cc = psw_cc2;
    	}
    } else if (int_reg < 0) {
    	if ((long_result & long_sign_bits) != long_sign_bits){
            psw_cc = psw_cc3;
            long_result = long_result | long_sign_bits;
        } else {
        	psw_cc = psw_cc1;
        }
    } else {
    	psw_cc = psw_cc0;		   
    }
    return (int)long_result;
}
private long get_sla64(long long_reg,int shift_count){
	/*
	 * return long_reg shifted left arith
	 * and set psw_cc
	 */
    long long_result = long_reg << shift_count;
    long shift_out_bits = long_reg >> 63 - shift_count;
    if (long_reg > 0){
    	if (shift_out_bits != 0){
            psw_cc = psw_cc3;
            long_result = long_result & long_num_bits;
    	} else {
    		psw_cc = psw_cc2;
    	}
    } else if (long_reg < 0) {
    	if (shift_out_bits != -1){
            psw_cc = psw_cc3;
            long_result = long_result | long_high_bit;
        } else {
        	psw_cc = psw_cc1;
        }
    } else {
    	psw_cc = psw_cc0;		   
    }
    return long_result;
}
private int get_sra32(int int_reg,int shift_count){
	/*
	 * return int shifted right arith
	 * and set psw_cc
	 */
    int int_result = int_reg >> shift_count;
    psw_cc = get_int_comp_cc(int_result,0);
    return int_result;
}
private long get_sra64(long long_reg,int shift_count){
	/*
	 * return long_reg shifted right arith
	 * and set psw_cc
	 */
    long long_result = long_reg >> shift_count;
    psw_cc = get_long_comp_cc(long_result,0);
    return long_result;
}
private int get_log_add_cc(long rlv,long rlv_carry_bits){
	/* 
	 * set psw_carry and 
	 * return cc for logical add as follows:
	 *   rlv  carry_bits  
	 *    0      0       cc0
	 *   !0      0       cc1
	 *    0     !0       cc2
	 *   !0     !0       cc3  
	 */
	 if (rlv == 0){
		if (rlv_carry_bits == 0){
			return psw_cc0;
		} else {
			return psw_cc2;
		}
	 } else {
		if (rlv_carry_bits == 0){
			return psw_cc1;
		} else {
			return psw_cc3;
		}	
	 }
}
private int get_log_sub_cc(long rlv,long borrow_bits){
	/* 
	 * return cc for logical subtract as follows:
	 *   rlv  borrow  
	 *    0     !0       cc0 (slb only)
	 * 	 !0     !0       cc1  
	 * 	  0      0       cc2
	 *   !0      0       cc3
	 */
	if (rlv == 0){
		if (borrow_bits == 0){
			return psw_cc2;  // Z,NB
		} else {
			return psw_cc0;  // Z,B  
		}
	} else {
		if (borrow_bits == 0){
			return psw_cc3;  // NZ,NB
		} else {
			return psw_cc1;  // NZ,B
		}	
	}
}
private byte[] get_log_bytes(byte[] data_byte,int data_offset,int data_len){
	/*
	 * return byte array with leading 0 byte followed
	 * by data bytes.  This array format is used to 
	 * initialize BigInteger with logical unsigned value.
	 */
	byte[] new_byte = new byte[data_len+1];
	while (data_len > 0){
		data_len--;
		new_byte[data_len+1] = data_byte[data_offset + data_len];
	}
	return new_byte;
}
private void cvt_big_int_to_work_reg(BigInteger big_int,int work_reg_bytes){
	/*
	 * copy big integer bytes to work reg 
	 * with work reg size up to 16
	 */
	byte[] work_byte = big_int.toByteArray();
	byte extend_byte = 0;
	if (work_byte[0] < 0)extend_byte = -1;
	int index1 = work_byte.length - 1;
	int index2 = work_reg_bytes - 1;
	while (index1 >= 0 & index2 >= 0){
		work_reg_byte[index2] = work_byte[index1];
		index1--;
		index2--;
	}
	while (index2 >= 0){
		work_reg_byte[index2] = extend_byte;
		index2--;
	}
}
private void exec_clm(){
	/* 
	 * exec clmh or clm using rv1, db2_loc
	 */
	psw_cc = psw_cc_equal;
    if  ((mf3 & 0x8) != 0){
        rv2 = (rv1 >> 24) & 0xff;
        if2 = mem.get(bd2_loc & psw_amode) & 0xff;
        bd2_loc++;
   	  if (rv2 != if2){
   	     if (rv2 > if2){
   	         psw_cc = psw_cc_high;
   	     } else {
   	         psw_cc = psw_cc_low;
   	     }
   	     return;
   	  }
   }
   if  ((mf3 & 0x4) != 0){
        rv2 = (rv1 >> 16) & 0xff;
        if2 = mem.get(bd2_loc & psw_amode) & 0xff;
        bd2_loc++;
   	    if (rv2 != if2){
   	        if (rv2 > if2){
   	            psw_cc = psw_cc_high;
   	        } else {
   	            psw_cc = psw_cc_low;
   	        }
   	        return;
   	    }
   }
   if  ((mf3 & 0x2) != 0){
        rv2 = (rv1 >> 8) & 0xff;
        if2 = mem.get(bd2_loc & psw_amode) & 0xff;
        bd2_loc++;
   	    if (rv2 != if2){
   	        if (rv2 > if2){
   	            psw_cc = psw_cc_high;
   	        } else {
   	            psw_cc = psw_cc_low;
   	        }
   	        return;
   	    }
   }
   if  ((mf3 & 0x1) != 0){
        rv2 = rv1 & 0xff;
        if2 = mem.get(bd2_loc & psw_amode) & 0xff;
   	    if (rv2 != if2){
   	        if (rv2 > if2){
   	         psw_cc = psw_cc_high;
   	    } else {
   	         psw_cc = psw_cc_low;
   	    }
   	    return;
   	  }
   }
}
private void exec_stcm(){
	/* 
	 * exec stcmh, stcmy, or stcm using rv1, db2_loc
	 */
    if  ((mf3 & 0x8) != 0){
        mem.put(bd2_loc & psw_amode,(byte)(rv1 >> 24));
        bd2_loc++; 
    }
    if  ((mf3 & 0x4) != 0){
        mem.put(bd2_loc & psw_amode,(byte)(rv1 >> 16));
        bd2_loc++; 
    }
    if  ((mf3 & 0x2) != 0){
        mem.put(bd2_loc & psw_amode,(byte)(rv1 >> 8));
        bd2_loc++; 
    }
    if  ((mf3 & 0x1) != 0){
        mem.put(bd2_loc & psw_amode,(byte)rv1);
        bd2_loc++; 
    }
}
private void exec_icm(){
	/* 
	 * exec icmh, icmy, or icm using rv1, db2_loc
	 */
	psw_cc = psw_cc_equal;
    if  ((mf3 & 0x8) != 0){
        if2 = mem.get(bd2_loc & psw_amode) & 0xff;
        if (if2 != 0){
        	if (psw_cc == psw_cc_equal
        		&& if2 >= 0x80){
        		psw_cc = psw_cc_high;
        	} else {
        		psw_cc = psw_cc_low;
        	}
        }
        rv1 = (rv1 & 0x00ffffff) | (if2 << 24); 
        bd2_loc++; 
   }
   if  ((mf3 & 0x4) != 0){
       if2 = mem.get(bd2_loc & psw_amode) & 0xff;
       if (if2 != 0){
    	   if (psw_cc == psw_cc_equal
    		   && if2 >= 0x80){
    		   psw_cc = psw_cc_high;
    	   } else {
    		   psw_cc = psw_cc_low;
    	   }
       }
       rv1 = (rv1 & 0xff00ffff) | (if2 << 16); 
       bd2_loc++; 
   }
   if  ((mf3 & 0x2) != 0){
       if2 = mem.get(bd2_loc & psw_amode) & 0xff;
       if (if2 != 0){
    	   if (psw_cc == psw_cc_equal
    		   && if2 >= 0x80){
    		   psw_cc = psw_cc_high;
    	   } else {
    		   psw_cc = psw_cc_low;
    	   }
       }
       rv1 = (rv1 & 0xffff00ff) | (if2 << 8); 
       bd2_loc++; 
   }
   if  ((mf3 & 0x1) != 0){
        if2 = mem.get(bd2_loc & psw_amode) & 0xff;
        if (if2 != 0){
    	    if (psw_cc == psw_cc_equal
    		    && if2 >= 0x80){
    		    psw_cc = psw_cc_high;
    	    } else {
    		    psw_cc = psw_cc_low;
    	    }
        }
        rv1 = ((rv1 & 0xffffff00) | if2); 
        bd2_loc++; 
   }
}
private void push_pc_stack(int link_addr){
	/*
	 * push psw and regs on PC stack
	 */
	if (cur_pc_stk < max_pc_stk){
		pc_stk_psw_loc[cur_pc_stk] = link_addr;
		pc_stk_psw_cc[cur_pc_stk]  = psw_cc;
		pc_stk_reg.position(cur_pc_stk_reg);
		pc_stk_reg.put(reg_byte,0,reg_byte.length);
		cur_pc_stk++;
		cur_pc_stk_reg = cur_pc_stk_reg + reg_byte.length;
	} else {
		abort_error(13,"PC call stack push error");
		set_psw_check(psw_pic_stack);
	}
}
private void pop_pc_stack(){
	/*
	 * pop psw and regs on PC stack
	 */
	if (cur_pc_stk > 0){
		cur_pc_stk--;
		cur_pc_stk_reg = cur_pc_stk_reg - reg_byte.length;
		set_psw_loc(pc_stk_psw_loc[cur_pc_stk]);
		psw_cc  = pc_stk_psw_cc[cur_pc_stk];
		pc_stk_reg.position(cur_pc_stk_reg);
		pc_stk_reg.get(reg_byte,0,reg_byte.length);
	} else {
		abort_error(14,"PC call stack pop error");
		set_psw_check(psw_pic_stack);
	}
}
private void svc(int svc_id){
	/*
	 * execute supervisor call using
	 * mem and regs for data transfer
	 */
	switch (svc_id){
	case 0x03:  // EXIT  
		svc_exit();
		break;
	case 0x04: // GETMAIN R0_BIT0=AMODE31, R0_BIT1=COND, R1=length
		svc_getmain();
		break;
	case 0x05: // FREEMAIN R0=addr, R1=length
		svc_freemain();
		break;
	case 0x06:  // LINK  R15=PGMID, R1=PARMS
        svc_link();
		break;
	case 0x08:  // LOAD  R15=PGMID
		svc_load();
		break;
	case 0x09: // DELETE R0=A(NAME)
		svc_delete(get_ascii_string(reg.getInt(r0),8));
		break;
	case 0x0b: // TIME R0_LH=DATETYPE, R0_LL=TIMETYPE, R1=ADDR
		svc_time();
		break;
	case 0x0d: // ABEND  R1=abend code 
        set_psw_check(reg.getInt(r1));
	    break;
	case 0x13:  // OPEN DCB R0=(x'40' input,x'20' output) R1=DCB
		svc_open();
		break;
	case 0x14:  // CLOSE DCB R1=DCB
		svc_close();
		break;
	case 0x23:  // WTO R1=ADDR 
		svc_wto();
		break;
	case 0x3c:
	    svc_estae(); // set abend exit R1=addr
	    break;
	case 0x6d:
		svc_espie();  // set program check exit R0=types, R1=addr
		break;
	case 151: // dcb get move R0=REC,R1=DCB
		svc_get_move();
		break;
	case 152: // dcb put move R0=REC, R1=DCB
		svc_put_move();
		break;
	case 153: // dcb read  R1=DECB
		svc_read();
		break;
	case 154: // dcb write R1=DECB
		svc_write();
		break;
	case 155: // decb check  R1=DECB
		svc_check();
		break;
	case 156: // dcb point  R1=DCB
		svc_point();
		break;
	default:
		abort_error(23,"undefined svc - " + svc_id);
		break;
	}
}
private String get_ins_target(int ins_loc){
	/*
	 * return hex ins plus ins name with 1 space
	 */
	return get_ins_hex(ins_loc).trim().trim() + " " + get_ins_name(ins_loc);
}
private String get_ins_hex(int ins_loc){
	/*
	 * return 2, 4, or 6 byte hex instruction
	 */
	String hex;
	int ins_op = mem.get(ins_loc) & 0xff;
	if (ins_op < 0x40){
	    hex = bytes_to_hex(mem_byte,ins_loc,2,0) + "        ";
	} else if (ins_op < 0xc0){
	    hex = bytes_to_hex(mem_byte,ins_loc,4,0) + "    ";
	} else {
	    hex = bytes_to_hex(mem_byte,ins_loc,6,0);
	}
	return hex;
}
private String get_hex(int work_int,int field_length) {
   	/*
   	 * Format int into 1-8 byte hex string
   	 */
   	    String work_hex = Integer.toHexString(work_int);
   	    if (work_hex.length() >= field_length){
   	    	return work_hex.substring(work_hex.length() - field_length).toUpperCase();
   	    } else {
   			return ("00000000" + work_hex).substring(8 - field_length + work_hex.length()).toUpperCase();
   	    }
   }
private String get_fp_long_hex(int reg_index) {
   	/*
   	 * Format fp reg into 16 byte hex string
   	 * after flushing co-reg
   	 * Note:  This trace function slows donw fp
   	 *        by adding extra conversions.
   	 */
	    fp_store_reg(trace_reg,reg_index);
	    String work_hex = Long.toHexString(trace_reg.getLong(reg_index));
	    return ("0000000000000000" + work_hex).substring(work_hex.length()).toUpperCase();
   }
private String get_long_hex(long work_long) {
   	/*
   	 * Format long into 16 byte hex string
   	 */
   	    String work_hex = Long.toHexString(work_long);
		return ("0000000000000000" + work_hex).substring(work_hex.length()).toUpperCase();
   }
   private String bytes_to_hex(byte[] bytes,int byte_start,int byte_length,int chunk){
   	/*
   	 * Format bytes into hex string
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
		    index1++;
            if (chunk > 0){
               hex_bytes++;
               if (hex_bytes >= chunk && index1 < byte_length){
            	  hex.append(" ");
            	  hex_bytes = 0;
               }
            }
        }
        if (hex.length() > 0){
            return hex.toString().toUpperCase();
        } else {
        	return "";
        }
   }
private void dump_regs(int reg_offset){
	/*
	 * dump specified register or all if -1
	 */
	if (reg_offset <= 0 || reg_offset >= r15){
		put_log(" r0-r4 " + bytes_to_hex(reg_byte,0,32,8));
		put_log(" r4-r7 " + bytes_to_hex(reg_byte,32,32,8));
		put_log(" r8-rc " + bytes_to_hex(reg_byte,64,32,8));
		put_log(" rd-rf " + bytes_to_hex(reg_byte,96,32,8));
	} else {
		int reg_num = reg_offset/8;
		put_log(" r" + reg_num + "=" + get_long_hex(reg.getLong(reg_offset)));
	}
}
private void trace_psw(){
	/*
	 * set opcode1 and opcode2 from psw_loc
	 * and then execute setup rotine with trace
	 * on to generate formated instruction trace
	 */
	boolean save_opt_trace = opt_trace;
	int     save_psw_loc   = psw_loc;
	opt_trace = true;
	opcode1 = mem.get(psw_loc) & 0xff;
	int index = opcode2_offset[opcode1];
	if (index > 0){
		opcode2 = mem.get(psw_loc+index) & opcode2_mask[opcode1];
	} else {
		opcode2 = 0;
	}
	index = opcode1_type[opcode1];
    switch (index){
    case 1:	// "E" 8 PR oooo
        ins_setup_e();    
        break;
    case 2: // "RR" 60  LR  oorr  
    	ins_setup_rr();    
    	break;
    case 3:// "BRX" 16  BER oomr
    	ins_setup_rr() ;    
        break;
    case 4:// "I" 1 SVC 00ii
        ins_setup_i() ;    
        break;
    case 5:// "RX" 52  L  oorxbddd
    	ins_setup_rx() ;    
        break;
    case 6:// "BCX" 16 BE  oomxbddd
        ins_setup_rx() ;    
        break;
    case 7:// "S" 43 SPM oo00bddd
    	ins_setup_s();     
        break;
    case 8:// "DM" 1  DIAGNOSE 83000000
    	ins_setup_dm() ;    
        break;
    case 9:// "RSI" 4 BRXH  oorriiii
        ins_setup_rsi() ;    
        break;
    case 10:// "RS" 25  oorrbddd
    	ins_setup_rs() ;    
        break;
    case 11:// "SI" 9 CLI  ooiibddd
        ins_setup_si() ;    
        break;
    case 12:// "RI" 37 IIHH  ooroiiii
        ins_setup_ri();    
        break;
    case 13:// "BRC" 31 BRE  oomoiiii
    	ins_setup_ri();   
        break;
    case 14:// "RRE" 185  MSR oooo00rr
        ins_setup_rre();   
        break;
    case 15:// "RRF" 28 MAER oooor0rr 
    	ins_setup_rrf();    
        break;
    case 16:// "RIL" 6  BRCL  oomollllllll
    	ins_setup_ril();   
        break;
    case 17:// "SS" 32  MVC oollbdddbddd  
        ins_setup_ss() ;    
        break;
    case 18:// "RXY" 76 MLG oorxbdddhhoo
        ins_setup_rxy();  
        break;
    case 19:// "SSE" 5  LASP  oooobdddbddd
    	ins_setup_sse();  
        break;
    case 20:// "RSY" 31  LMG  oorrbdddhhoo
    	ins_setup_rsy();  
        break;
    case 21:// "SIY" 6  TMY  ooiibdddhhoo
    	ins_setup_siy();  
        break;
    case 22:// "RSL" 1  TP  oor0bddd00oo
    	ins_setup_rsl();    
        break;
    case 23:// "RIE" 4  BRXLG  oorriiii00oo
    	ins_setup_rie();    
        break;
    case 24:// "RXE" 28  ADB oorxbddd00oo
    	ins_setup_rxe();  
        break;
    case 25:// "RXF" 8   MAE  oorxbdddr0oo (note r3 before r1)
    	ins_setup_rxf();    
        break;
    case 26:// AP SS2  oollbdddbddd
    	ins_setup_ssp() ;    
        break;
    case 27:// PLO SS3  oorrbdddbddd  r1,s2,r3,s4
        ins_setup_ss() ;   
        break;
    case 28:// LMD SS5  oorrbdddbddd  r1,r3,s2,s4
    	ins_setup_ss() ;    
        break;
    case 29:// SRP SS2  oolibdddbddd s1(l1),s2,i3
        ins_setup_ss() ;  
        break;
    case 30:// "RRF" 30 DIER oooormrr (r3,m4,r1,r2 coded as r1,r3,r2,m4) 
    	ins_setup_rrf();    
        break;
    }
    opt_trace = save_opt_trace;
    psw_loc   = save_psw_loc;
}
private void dump_mem(int mem_addr,int mem_len){
	/*
	 * dump specified area of memory
	 */
	int dump_len = 0;
	if (mem_addr < 0){
		mem_addr = 0;
	}
	while (mem_len > 0 && mem_addr + mem_len <= tot_mem){
		if (mem_len > 16){
			dump_len = 16;
		} else {
			dump_len = mem_len;
		}
		String dump_text = "";
		int index = 0;
		while (index < dump_len){
			dump_text = dump_text + ebcdic_table.charAt(mem.get(mem_addr+index) & 0xff);
			index++;
		}
		put_log(" " +get_hex(mem_addr,8) 
			  + " *"  + bytes_to_hex(mem_byte,mem_addr,dump_len,4)
			  + "* *" + dump_text + "*"
		);
        mem_addr = mem_addr + 16;
        mem_len  = mem_len  - 16;
	}
}
private int get_big_int_comp_cc(BigInteger big_int1,BigInteger big_int2){
	/*
	 * return psw cc for big int comp
	 */
    switch (big_int1.compareTo(big_int2)){
    case -1:
    	return psw_cc_low;
    case 0:
        return psw_cc_equal;
    case 1:
    	return psw_cc_high;
    default:
    	abort_error(12,"invalid big int cc");
        return psw_cc_ovf;
    } 
}
private int get_long_add_cc(){
	/*
	 * check rlv3 = rlv1 + rlv2 for fixed overflow
	 * and set psw_cc or gen fixed exception
	 */
    if (rlv1 >= 0){
     	if (rlv2 > 0){
     		if (rlv3 < 0){
     			set_psw_check(psw_pic_fx_ovf);
     		}
     	}
     } else if (rlv2 < 0){
     	if (rlv3 >= 0){
     		set_psw_check(psw_pic_fx_ovf);
     	}
     }
     if (rlv3 == 0){
     	return psw_cc_equal;
     } else {
     	if (rlv3 > 0){
     	    return psw_cc_high;
        } else {
     	    return psw_cc_low;
        }
     }
}
private int get_long_sub_cc(){
	/*
	 * check rlv3 = rlv1 - rlv2 for fixed overflow
	 * and set psw_cc or gen fixed exception
	 */
    if (rlv1 >= 0){
     	if (rlv2 < 0){
     		if (rlv3 < 0){
     			set_psw_check(psw_pic_fx_ovf);
     		}
     	}
     } else if (rlv2 > 0){
     	if (rlv3 >= 0){
     		set_psw_check(psw_pic_fx_ovf);
     	}
     }
     if (rlv3 == 0){
     	return psw_cc_equal;
     } else {
     	if (rlv3 > 0){
     	    return psw_cc_high;
        } else {
     	    return psw_cc_low;
        }
     }
}
private int get_long_comp_cc(long long1, long long2){
	/*
	 * return psw_cc for long compare
	 */
    if (long1 == long2){
     	return psw_cc_equal;
     } else {
     	if (long1 > long2){
     	    return psw_cc_high;
        } else {
     	    return psw_cc_low;
        }
     }
}
private int get_int_add_cc(){
	/*
	 * check rv3 = rv1 + rv2 for fixed overflow
	 * and set psw_cc or gen fixed exception
	 */
    if (rv1 >= 0){
     	if (rv2 > 0){
     		if (rv3 < 0){
     			set_psw_check(psw_pic_fx_ovf);
     		}
     	}
     } else if (rv2 < 0){
     	if (rv3 >= 0){
     		set_psw_check(psw_pic_fx_ovf);
     	}
     }
     if (rv3 == 0){
     	return psw_cc_equal;
     } else {
     	if (rv3 > 0){
     	    return psw_cc_high;
        } else {
     	    return psw_cc_low;
        }
     }
}
private int get_int_sub_cc(){
	/*
	 * check rv3 = rv1 - rv2 for fixed overflow
	 * and set psw_cc or gen fixed exception
	 */
    if (rv1 >= 0){
     	if (rv2 < 0){
     		if (rv3 < 0){
     			set_psw_check(psw_pic_fx_ovf);
     		}
     	}
     } else if (rv2 > 0){
     	if (rv3 >= 0){
     		set_psw_check(psw_pic_fx_ovf);
     	}
     }
     if (rv3 == 0){
     	return psw_cc_equal;
     } else {
     	if (rv3 > 0){
     	    return psw_cc_high;
        } else {
     	    return psw_cc_low;
        }
     }
}
private int get_int_comp_cc(int int1, int int2){
	/*
	 * return psw_cc for int compare
	 */
    if (int1 == int2){
     	return psw_cc_equal;
     } else {
     	if (int1 > int2){
     	    return psw_cc_high;
        } else {
     	    return psw_cc_low;
        }
     }
}
private void check_eb_mpy(){
	/*
	 * check for EB overflow or underflow
	 */
	    if (Math.abs(fp_rev1) >= fp_eb_max){
		    fp_dxc = fp_dxc_oe;
		    set_psw_check(psw_pic_data);
	    } else if (Math.abs(fp_rev1) <= fp_eb_min){
		    fp_dxc = fp_dxc_ue;
		    set_psw_check(psw_pic_data);
	    }
}
private void check_eh_mpy(){
	/*
	 * check for EH overflow or underflow
	 */
	    if (Math.abs(fp_rdv1) >= fp_eh_max){
		    set_psw_check(psw_pic_fp_ovf);
	    } else if (Math.abs(fp_rdv1) <= fp_eh_min){
		    set_psw_check(psw_pic_fp_unf);
	    }
}
private void check_db_mpy(){
	/*
	 * check for DB overflow or underflow
	 */
	    if (Math.abs(fp_rdv1) >= fp_db_max){
		    fp_dxc = fp_dxc_oe;
		    set_psw_check(psw_pic_data);
	    } else if (Math.abs(fp_rdv1) <= fp_db_min){
		    fp_dxc = fp_dxc_ue;
		    set_psw_check(psw_pic_data);
	    }
}
private void check_dh_mpy(){
	/*
	 * check for DH overflow or underflow
	 */
	    if (Math.abs(fp_rdv1) >= fp_dh_max){
		    set_psw_check(psw_pic_fp_ovf);
	    } else if (Math.abs(fp_rdv1) <= fp_dh_min){
		    set_psw_check(psw_pic_fp_unf);
	    }
}
private void check_lb_mpy(){
	/*
	 * check for LB overflow or underflow
	 */
	    if (fp_rbdv1.abs().compareTo(fp_lh_max) >= 0){
		    fp_dxc = fp_dxc_oe;
		    set_psw_check(psw_pic_data);
	    } else if (fp_rbdv1.abs().compareTo(fp_lh_min) <= 0){
		    fp_dxc = fp_dxc_ue;
		    set_psw_check(psw_pic_data);
	    }
}
private void check_lh_mpy(){
	/*
	 * check for LH overflow or underflow
	 */
	    if (fp_rbdv1.abs().compareTo(fp_lh_max) >= 0){
		    set_psw_check(psw_pic_fp_ovf);
   	    } else if (fp_rbdv1.abs().compareTo(fp_lh_min) <= 0){
		    fp_dxc = fp_dxc_ue;
		    set_psw_check(psw_pic_fp_unf);
	    }
}
private void check_eb_div(){
	/*
	 * check for EB overflow or underflow
	 */
	 if (fp_rev2 == 0){
	    fp_dxc = fp_dxc_div;
	 	set_psw_check(psw_pic_data);
	 } else {
	    if (Math.abs(fp_rev1) >= fp_eb_max){
		    fp_dxc = fp_dxc_oe;
		    set_psw_check(psw_pic_data);
	    } else if (Math.abs(fp_rev1) <= fp_eb_min){
		    fp_dxc = fp_dxc_ue;
		    set_psw_check(psw_pic_data);
	    }
	 }
}
private void check_eh_div(){
	/*
	 * check for EH overflow or underflow
	 */
	 if (fp_rdv2 == 0){
	 	set_psw_check(psw_pic_fp_div);
	 } else {
	    if (Math.abs(fp_rdv1) >= fp_eh_max){
		    set_psw_check(psw_pic_fp_ovf);
	    } else if (Math.abs(fp_rdv1) <= fp_eh_min){
		    set_psw_check(psw_pic_fp_unf);
	    }
	 }
}
private void check_db_div(){
	/*
	 * check for DB overflow or underflow
	 */
	 if (fp_rdv2 == 0){
	    fp_dxc = fp_dxc_div;
	 	set_psw_check(psw_pic_data);
	 } else {
	    if (Math.abs(fp_rdv1) >= fp_db_max){
		    fp_dxc = fp_dxc_oe;
		    set_psw_check(psw_pic_data);
	    } else if (Math.abs(fp_rdv1) <= fp_db_min){
		    fp_dxc = fp_dxc_ue;
		    set_psw_check(psw_pic_data);
	    }
	 }
}
private void check_dh_div(){
	/*
	 * check for DH overflow or underflow
	 */
	 if (fp_rdv2 == 0){
	 	set_psw_check(psw_pic_fp_div);
	 } else {
	    if (Math.abs(fp_rdv1) >= fp_dh_max){
		    set_psw_check(psw_pic_fp_ovf);
	    } else if (Math.abs(fp_rdv1) <= fp_dh_min){
		    set_psw_check(psw_pic_fp_unf);
	    }
	 }
}
private void check_lb_div(){
	/*
	 * check for LB overflow or underflow
	 */
	 if (fp_rbdv2.compareTo(BigDecimal.ZERO) == 0){
	    fp_dxc = fp_dxc_div;
	 	set_psw_check(psw_pic_data);
	 } else {
	    if (fp_rbdv1.abs().compareTo(fp_lh_max) >= 0){
		    fp_dxc = fp_dxc_oe;
		    set_psw_check(psw_pic_data);
	    } else if (fp_rbdv1.abs().compareTo(fp_lh_min) <= 0){
		    fp_dxc = fp_dxc_ue;
		    set_psw_check(psw_pic_data);
	    }
	 }
}
private void check_lh_div(){
	/*
	 * check for LH overflow or underflow
	 */
	 if (fp_rbdv2.compareTo(BigDecimal.ZERO) == 0){
	 	set_psw_check(psw_pic_fp_div);
	 } else {
	    if (fp_rbdv1.abs().compareTo(fp_lh_max) >= 0){
		    set_psw_check(psw_pic_fp_ovf);
   	    } else if (fp_rbdv1.abs().compareTo(fp_lh_min) <= 0){
		    fp_dxc = fp_dxc_ue;
		    set_psw_check(psw_pic_fp_unf);
	    }
	 }
}
private int fp_get_di_cc(){
	/*
	 * return psw_cc for fp divide to integer
	 */
	fp_rdv3 = Math.abs(fp_rdv3);
	fp_rdv4 = Math.abs(fp_rdv4);
	if (fp_rdv3 >= fp_db_max){
		fp_dxc = fp_dxc_oe;
		set_psw_check(psw_pic_data);
		return psw_cc1;
	} else if (fp_rdv3 != 0 && fp_rdv3 <= fp_db_min){
		fp_dxc = fp_dxc_ue;
		set_psw_check(psw_pic_data);
		return psw_cc1;
	} else if (fp_rdv4 >= fp_db_max){
		fp_dxc = fp_dxc_oe;
		set_psw_check(psw_pic_data);
		return psw_cc2;
	} else if (fp_rdv4 != 0 && fp_rdv4 <= fp_db_min){
		fp_dxc = fp_dxc_ue;
		set_psw_check(psw_pic_data);
		return psw_cc2;
	} else {
		return psw_cc0;
	}
}
private int fp_get_eb_comp_cc(float eb1, float eb2){
	/*
	 * return psw_cc for float compare
	 */
	if (fp_signal){
		fp_signal = false;
		if (Math.abs(eb1) >= fp_db_max
			|| Math.abs(eb2) >= fp_eb_max){
		    fp_dxc = fp_dxc_oe;
	 		set_psw_check(psw_pic_data);
		} else if (Math.abs(eb1) <= fp_eb_min
				   || Math.abs(eb2) <= fp_eb_min){
		    fp_dxc = fp_dxc_ue;
	 		set_psw_check(psw_pic_data);
	 	}      
 	}
    if (eb1 == eb2){   
     	return psw_cc_equal;
     } else {
     	if (eb1 > eb2){
     	    return psw_cc_high;
        } else {
     	    return psw_cc_low;
        }
     }
}
private int fp_get_eb_add_sub_cc(){
	/*
	 * return psw_cc for eb add/sub result fp_rev1
	 * and raise BFP sig, ovf, or unf exceptions
	 */
    if (fp_rev1 == 0){
	    fp_dxc = fp_dxc_it;
    	set_psw_check(psw_pic_data);
     	return psw_cc_equal;
     } else {
     	if (fp_rev1 > 0){
     		if (fp_rev1 >= fp_eb_max){
			    fp_dxc = fp_dxc_oe;
     			set_psw_check(psw_pic_data);
     		} else if (fp_rev1 <= fp_eb_min){
			    fp_dxc = fp_dxc_ue;
     			set_psw_check(psw_pic_data);
     		}
     	    return psw_cc_high;
        } else {
     		if (fp_rev1 <= -fp_eb_max){
			    fp_dxc = fp_dxc_oe;
     			set_psw_check(psw_pic_data);
     		} else if (fp_rev1 >= -fp_eh_min){
			    fp_dxc = fp_dxc_ue;
     			set_psw_check(psw_pic_data);
     		}
     	    return psw_cc_low;
        }
     }
}
private int fp_get_db_comp_cc(double db1, double db2){
	/*
	 * return psw_cc for DB double compare
	 */
	if (fp_signal){
		fp_signal = false;
		if (Math.abs(db1) >= fp_db_max
			|| Math.abs(db2) >= fp_db_max){
		    fp_dxc = fp_dxc_oe;
 			set_psw_check(psw_pic_data);
		} else if (Math.abs(db1) <= fp_db_min
				   || Math.abs(db2) <= fp_db_min){
		    fp_dxc = fp_dxc_ue;
 			set_psw_check(psw_pic_data);
 		}
	}
    if (db1 == db2){
     	return psw_cc_equal;
     } else {
     	if (db1 > db2){
     	    return psw_cc_high;
        } else {
     	    return psw_cc_low;
        }
     }
}
private int fp_get_eh_comp_cc(double db1, double db2){
	/*
	 * return psw_cc for EH double compare
	 */
    if (db1 == db2){
     	return psw_cc_equal;
     } else {
     	if (db1 > db2){
     	    return psw_cc_high;
        } else {
     	    return psw_cc_low;
        }
     }
}
private int fp_get_dh_comp_cc(double db1, double db2){
	/*
	 * return psw_cc for DH double compare
	 */
    if (db1 == db2){
     	return psw_cc_equal;
     } else {
     	if (db1 > db2){
     	    return psw_cc_high;
        } else {
     	    return psw_cc_low;
        }
     }
}
private int fp_get_eh_add_sub_cc(){
	/*
	 * return psw_cc for eh add/sub result fp_rdv1
	 * and raise HFP sig, ovf, or unf exceptions
	 */
    if (fp_rdv1 == 0){
    	set_psw_check(psw_pic_fp_sig);
     	return psw_cc_equal;
     } else {
     	if (fp_rdv1 > 0){
     		if (fp_rdv1 >= fp_eh_max){
     			set_psw_check(psw_pic_fp_ovf);
     		} else if (fp_rdv1 <= fp_eh_min){
     			set_psw_check(psw_pic_fp_unf);
     		}
     	    return psw_cc_high;
        } else {
     		if (fp_rdv1 <= -fp_eh_max){
     			set_psw_check(psw_pic_fp_ovf);
     		} else if (fp_rdv1 >= -fp_eh_min){
     			set_psw_check(psw_pic_fp_unf);
     		}
     	    return psw_cc_low;
        }
     }
}
private int fp_get_dh_add_sub_cc(){
	/*
	 * return psw_cc for dh add/sub result fp_rdv1
	 * and raise HFP sig, ovf, or unf exceptions
	 */
    if (fp_rdv1 == 0){
    	set_psw_check(psw_pic_fp_sig);
     	return psw_cc_equal;
     } else {
     	if (fp_rdv1 > 0){
     		if (fp_rdv1 >= fp_dh_max){
     			set_psw_check(psw_pic_fp_ovf);
     		} else if (fp_rdv1 <= fp_dh_min){
     			set_psw_check(psw_pic_fp_unf);
     		}
     	    return psw_cc_high;
        } else {
     		if (fp_rdv1 <= -fp_dh_max){
     			set_psw_check(psw_pic_fp_ovf);
     		} else if (fp_rdv1 >= -fp_dh_min){
     			set_psw_check(psw_pic_fp_unf);
     		}
     	    return psw_cc_low;
        }
     }
}
private int fp_get_db_add_sub_cc(){
	/*
	 * return psw_cc for db add/sub result fp_rdv1
	 * and raise BFP sig, ovf, or unf exceptions
	 */
    if (fp_rdv1 == 0){
	    fp_dxc = fp_dxc_it;
    	set_psw_check(psw_pic_data);
     	return psw_cc_equal;
     } else {
     	if (fp_rdv1 > 0){
     		if (fp_rdv1 >= fp_db_max){
			    fp_dxc = fp_dxc_oe;
     			set_psw_check(psw_pic_data);
     		} else if (fp_rdv1 <= fp_db_min){
			    fp_dxc = fp_dxc_ue;
     			set_psw_check(psw_pic_data);
     		}
     	    return psw_cc_high;
        } else {
     		if (fp_rdv1 <= -fp_db_max){
			    fp_dxc = fp_dxc_oe;
     			set_psw_check(psw_pic_data);
     		} else if (fp_rdv1 >= -fp_db_min){
			    fp_dxc = fp_dxc_ue;
     			set_psw_check(psw_pic_data);
     		}
     	    return psw_cc_low;
        }
     }
}
private int fp_get_lb_comp_cc(BigDecimal bd1, BigDecimal bd2){
	/*
	 * return psw_cc for big decimal compare
	 */
	if (fp_signal){
		fp_signal = false;
 		if (bd1.abs().compareTo(fp_lb_max) >= 0
 			|| bd2.abs().compareTo(fp_lb_max) >= 0){
		    fp_dxc = fp_dxc_oe;
 			set_psw_check(psw_pic_data);
 		} else if (bd1.abs().compareTo(fp_lb_min) <= 0
 				|| bd2.abs().compareTo(fp_lb_min) <= 0){
		    fp_dxc = fp_dxc_ue;
 			set_psw_check(psw_pic_data);
 		}
	}
	int_work = bd1.compareTo(bd2);
    if (int_work == 0){
     	return psw_cc_equal;
     } else {
     	if (int_work > 0){
     	    return psw_cc_high;
        } else {
     	    return psw_cc_low;
        }
     }
}
private int fp_get_lh_comp_cc(BigDecimal bd1, BigDecimal bd2){
	/*
	 * return psw_cc for LH big decimal compare
	 */
	int_work = bd1.compareTo(bd2);
    if (int_work == 0){
     	return psw_cc_equal;
     } else {
     	if (int_work > 0){
     	    return psw_cc_high;
        } else {
     	    return psw_cc_low;
        }
     }
}
private int fp_get_lh_add_sub_cc(){
	/*
	 * return psw_cc for lh add/sub result fp_rbdv1
	 * 	 * and raise HFP sig, ovf, or unf exceptions
	 */
    if (fp_rbdv1.compareTo(BigDecimal.ZERO) == 0){
    	set_psw_check(psw_pic_fp_sig);
     	return psw_cc_equal;
     } else {
     	if (fp_rbdv1.compareTo(BigDecimal.ZERO) > 0){
     		if (fp_rbdv1.compareTo(fp_lh_max) >= 0){
     			set_psw_check(psw_pic_fp_ovf);
     		} else if (fp_rbdv1.compareTo(fp_lh_min) <= 0){
     			set_psw_check(psw_pic_fp_unf);
     		}
     	    return psw_cc_high;
        } else {
     		if (fp_rbdv1.compareTo(fp_lh_max.negate()) <= 0){
     			set_psw_check(psw_pic_fp_ovf);
     		} else if (fp_rbdv1.compareTo(fp_lh_min.negate()) >= 0){
     			set_psw_check(psw_pic_fp_unf);
     		}
     	    return psw_cc_low;
        }
     }
}
private int fp_get_lb_add_sub_cc(){
	/*
	 * return psw_cc for lb add/sub result fp_rbdv1
	 * and raise BFP sig, ovf, or unf exceptions
	 */
	int int_work = fp_rbdv1.compareTo(BigDecimal.ZERO);
    if (int_work == 0){
	    fp_dxc = fp_dxc_it;
    	set_psw_check(psw_pic_data);
     	return psw_cc_equal;
     } else {
     	if (int_work > 0){
     		if (fp_rbdv1.compareTo(fp_lb_max) >= 0){
			    fp_dxc = fp_dxc_oe;
     			set_psw_check(psw_pic_data);
     		} else if (fp_rbdv1.compareTo(fp_lb_min) <= 0){
			    fp_dxc = fp_dxc_ue;
     			set_psw_check(psw_pic_data);
     		}
     	    return psw_cc_high;
        } else {
     		if (fp_rbdv1.compareTo(fp_lb_max.negate()) <= 0){
			    fp_dxc = fp_dxc_oe;
     			set_psw_check(psw_pic_data);
     		} else if (fp_rbdv1.compareTo(fp_lb_min.negate()) >= 0){
			    fp_dxc = fp_dxc_ue;
     			set_psw_check(psw_pic_data);
     		}
     	    return psw_cc_low;
        }
     }
}
private int get_int_log_comp_cc(int int1,int int2){
	/*
	 * return psw_cc for integer logical compare 
	 */
    if (int1 == int2){
     	return psw_cc_equal;
     } else {
     	if (int1 > int2){
     		if ((int1 & 0x80000000) == (int2 & 0x80000000)){
     	        return psw_cc_high;
     		} else {
                return psw_cc_low;
     		}
        } else {
     		if ((int1 & 0x80000000) == (int2 & 0x80000000)){
 			    return psw_cc_low;
     		} else {
     			return psw_cc_high;
     		}
        }
     }
}
private int get_long_log_comp_cc(long long1,long long2){
	/*
	 * return psw_cc for long logical compare 
	 */
    if (long1 == long2){
     	return psw_cc_equal;
     } else {
     	if (long1 > long2){
     		if ((long1 & long_high_bit) == (long2 & long_high_bit)){
     	        return psw_cc_high;
     		} else {
                return psw_cc_low;
     		}
        } else {
     		if ((long1 & long_high_bit) == (long2 & long_high_bit)){
 			    return psw_cc_low;
     		} else {
     			return psw_cc_high;
     		}
        }
     }
}
private float fp_get_eb_from_eb(ByteBuffer fp_buff,int fp_index){
	/*
	 * get float for EB from fp_reg or mem
	 *   1.  If fp_reg then check for co-reg
	 *       to avoid conversion
	 * Notes:
	 *   1.  float is used to support EB only.
	 *   2.  EH is supported using double since
	 *       exponent (4*0x7f) exceeds EB (0xff).
	 */
	 if (fp_buff == fp_reg){
	 	int fp_ctl_index = fp_index >> 3;
		if (fp_reg_ctl[fp_ctl_index] != fp_ctl_eb){
            if (fp_reg_ctl[fp_ctl_index] == fp_ctl_ld){ // set by LE/LD
				fp_reg_type[fp_ctl_index] = fp_eb_type;
				fp_reg_ctl[fp_ctl_index]  = fp_ctl_eb;
				fp_reg_eb[fp_ctl_index]   = fp_buff.getFloat(fp_index);
            } else {
            	set_psw_check(psw_pic_spec);
			}
		}
		return fp_reg_eb[fp_ctl_index];
	} else {
		return fp_buff.getFloat(fp_index);
	}
}

private double fp_get_db_from_eh(ByteBuffer fp_buff,int fp_index){
	/*
	 * get double from EH short hex in fp_reg or mem
	 *   1.  If fp_reg, then check for float co-reg
	 *       to avoid conversion
	 */
	 if (fp_buff == fp_reg){
	 	int fp_ctl_index = fp_index >> 3;
		if (fp_reg_ctl[fp_ctl_index] != fp_ctl_db){
			if (fp_reg_ctl[fp_ctl_index] == fp_ctl_ld){ // set by LE/LD
				fp_reg_type[fp_ctl_index] = fp_eh_type;
				fp_reg_ctl[fp_ctl_index]  = fp_ctl_db;
				fp_reg_db[fp_ctl_index]   = cvt_eh_to_db(fp_buff.getInt(fp_index));
			} else {
				set_psw_check(psw_pic_spec);
			}
		}
		return fp_reg_db[fp_ctl_index];
	} else {
		return cvt_eh_to_db(fp_buff.getInt(fp_index));
	}
}
private float fp_get_db_from_eb(ByteBuffer fp_buff,int fp_index){
	/*
	 * get float from EB short binary fp_reg or mem
	 *   1.  If fp_reg, then check for float co-reg
	 *       to avoid conversion
	 */
	if (fp_buff == fp_reg){
		int fp_ctl_index = fp_index >> 3;
		if (fp_reg_ctl[fp_ctl_index] != fp_ctl_eb){
			if (fp_reg_ctl[fp_ctl_index] == fp_ctl_ld){ // set by LE/LD
				fp_reg_type[fp_ctl_index] = fp_eb_type;
				fp_reg_ctl[fp_ctl_index]  = fp_ctl_eb;
				fp_reg_eb[fp_ctl_index]   = fp_buff.getFloat(fp_index);
			} else {
				set_psw_check(psw_pic_spec);
			}
		}
		return fp_reg_eb[fp_ctl_index];
	} else {
		return fp_buff.getFloat(fp_index);
	}
}
private double fp_get_db_from_dh(ByteBuffer fp_buff,int fp_index){
	/*
	 * get double from DH long hex in fp_reg or mem
	 *   1.  If fp_reg, then check for float co-reg
	 *       to avoid conversion
	 */
	if (fp_buff == fp_reg){
		int fp_ctl_index = fp_index >> 3;
		if (fp_reg_ctl[fp_ctl_index] != fp_ctl_db){
			if (fp_reg_ctl[fp_ctl_index] == fp_ctl_ld){ // set by LE/LD
				fp_reg_type[fp_ctl_index] = fp_dh_type;
				fp_reg_ctl[fp_ctl_index]  = fp_ctl_db;
				fp_reg_db[fp_ctl_index]   = cvt_dh_to_db(fp_buff.getLong(fp_index));
			} else {
				set_psw_check(psw_pic_spec);
			}
		}
		return fp_reg_db[fp_ctl_index];
	} else {
		return cvt_dh_to_db(fp_buff.getLong(fp_index));
	}
}
private double fp_get_db_from_db(ByteBuffer fp_buff,int fp_index){
	/*
	 * get double from DB Long binary in fp_reg or mem
	 *   1.  If fp_reg, then check for float co-reg
	 *       to avoid conversion
	 */
	if (fp_buff == fp_reg){
		int fp_ctl_index = fp_index >> 3;
		if (fp_reg_ctl[fp_ctl_index] != fp_ctl_db){
			if (fp_reg_ctl[fp_ctl_index] == fp_ctl_ld){ // set by LE/LD
				fp_reg_type[fp_ctl_index] = fp_db_type;
				fp_reg_ctl[fp_ctl_index]  = fp_ctl_db;
				fp_reg_db[fp_ctl_index]   = fp_buff.getDouble(fp_index);
			} else {
				set_psw_check(psw_pic_spec);
			}
		}
		return fp_reg_db[fp_ctl_index];
	} else {
		return fp_buff.getDouble(fp_index);
	}
}
private double fp_get_db_from_lh(ByteBuffer fp_buff,int fp_index){
	/*
	 * get double from LH ext hex in fp_reg or mem
	 *   1.  If fp_reg, then check for bd co-reg
	 *       to avoid external conversion
	 */
	if (fp_buff == fp_reg){
		int fp_ctl_index = fp_index >> 3;
		if (fp_reg_ctl[fp_ctl_index] != fp_ctl_bd1){
			if (fp_reg_ctl[fp_ctl_index] == fp_ctl_ld){ // set by LE/LD
				fp_reg_type[fp_ctl_index] = fp_lh_type;
				fp_reg_ctl[fp_ctl_index]  = fp_ctl_bd1;
				fp_reg_ctl[fp_ctl_index+1]= fp_ctl_bd2;
				fp_reg_bd[fp_ctl_index]   = cvt_lh_to_bd(fp_buff,fp_index);	
			} else {
				set_psw_check(psw_pic_spec);
			}
		}
		return fp_reg_bd[fp_ctl_index].doubleValue();
	} else {
		return cvt_lh_to_bd(fp_buff,fp_index).doubleValue();
	}
}
private void fp_get_bd_sqrt(){
	/*
	 * set fp_rbdv1 to square root of fp_rbdv2
	 * 
	 * Notes:
	 *   1.  Return 0 for 0 and issue data
	 *       exception if negative.
	 *   2.  Scale number to within double range 
	 *       to calc estimate to 14 decimal places.
	 *   2.  Use Newton Rapson iteration to 
	 *       reduce error to fp_bd_context limit.
	 */
	/*
	 * First handle 0 and negative values
	 */
	if (fp_rbdv2.signum() <= 0){
		if (fp_rbdv2.signum() < 0){
			set_psw_check(psw_pic_data);
		}
		fp_rbdv1 = BigDecimal.ZERO;
		return;
	}
	/*
	 * First fp_rbdv2 = x * 10 ** N
	 * where N is even so x is within double
	 * range and sqrt = sqrt(x) * 10 ** (N/2)
	 */
	int fp_bd_sqrt_scale = fp_rbdv2.scale();
	if ((fp_bd_sqrt_scale & 1) != 0){
		fp_rbdv2 = fp_rbdv2.setScale(fp_bd_sqrt_scale-1);
		fp_bd_sqrt_scale--;
	}
	fp_rbdv2 = fp_rbdv2.multiply(BigDecimal.TEN.pow(fp_bd_sqrt_scale,fp_bd_context),fp_bd_context);
    /*
     * Now calc initial quess at sqrt of fp_rbdv2
     */
    fp_rbdv1 = BigDecimal.valueOf(Math.sqrt(fp_rbdv2.doubleValue()));
    /*
     * Now iterate using Newton Rapson until error
     * is less than fp_bd_min
     */
    fp_rbdv3 = fp_rbdv1; // save prev
	fp_rbdv1 = fp_rbdv2.divide(fp_rbdv1,fp_bd_context).add(fp_rbdv1,fp_bd_context).multiply(fp_bd_half,fp_bd_context);
    while (fp_rbdv1.compareTo(fp_rbdv3) != 0){
        fp_rbdv3 = fp_rbdv1; // save prev
    	fp_rbdv1 = fp_rbdv2.divide(fp_rbdv1,fp_bd_context).add(fp_rbdv1,fp_bd_context).multiply(fp_bd_half,fp_bd_context);
    }
    fp_rbdv1 = fp_rbdv1.scaleByPowerOfTen(-fp_bd_sqrt_scale / 2).round(fp_x_context);
}
private BigDecimal fp_get_bd_from_lh(ByteBuffer fp_buff,int fp_index){
	/*
	 * get big decimal from LH extended hex in fp_reg or mem
	 *   1.  If fp_reg, then check for big dec co-reg
	 *       to avoid conversion
	 */
	if (fp_buff == fp_reg){
		int fp_ctl_index = fp_index >> 3;
		if (fp_reg_ctl[fp_ctl_index] != fp_ctl_bd1){
			if (fp_reg_ctl[fp_ctl_index] == fp_ctl_ld){ // set by LE/LD
				fp_reg_type[fp_ctl_index] = fp_lh_type;
				fp_reg_ctl[fp_ctl_index]  = fp_ctl_bd1;
				fp_reg_ctl[fp_ctl_index+1]= fp_ctl_bd2;
				fp_reg_bd[fp_ctl_index]   = cvt_lh_to_bd(fp_buff,fp_index);
			} else {
				set_psw_check(psw_pic_spec);
			}
		}
		return fp_reg_bd[fp_ctl_index];
	} else {
		return cvt_lh_to_bd(fp_buff,fp_index);
	}
}
private BigDecimal fp_get_bd_from_lb(ByteBuffer fp_buff,int fp_index){
	/*
	 * get big decimal from LH extended binary in fp_reg or mem
	 *   1.  If fp_reg, then check for big dec co-reg
	 *       to avoid conversion
	 */
	if (fp_buff == fp_reg){
		int fp_ctl_index = fp_index >> 3;
		if (fp_reg_ctl[fp_ctl_index] != fp_ctl_bd1){
			if (fp_reg_ctl[fp_ctl_index] == fp_ctl_ld){ // set by LE/LD
				fp_reg_type[fp_ctl_index] = fp_lb_type;
				fp_reg_ctl[fp_ctl_index]  = fp_ctl_bd1;
				fp_reg_ctl[fp_ctl_index+1]= fp_ctl_bd2;
				fp_reg_bd[fp_ctl_index]   = cvt_lb_to_bd(fp_buff,fp_index);
			} else {
				set_psw_check(psw_pic_spec);
			}
		}
		return fp_reg_bd[fp_ctl_index];
	} else {
		return cvt_lb_to_bd(fp_buff,fp_index);
	}
}
private BigDecimal fp_get_bd_from_dh(ByteBuffer fp_buff,int fp_index){
	/*
	 * get big decimal from DH long hex in fp_reg or mem
	 *   1.  If fp_reg, then check for big dec co-reg
	 *       to avoid conversion
	 */
	if (fp_buff == fp_reg){
		int fp_ctl_index = fp_index >> 3;
		if (fp_reg_ctl[fp_ctl_index] != fp_ctl_bd1){
			if (fp_reg_ctl[fp_ctl_index] == fp_ctl_ld){ // set by LE/LD
				fp_reg_type[fp_ctl_index] = fp_lh_type;
				fp_reg_ctl[fp_ctl_index]  = fp_ctl_bd1;
				fp_reg_ctl[fp_ctl_index+1]= fp_ctl_bd2;
				fp_reg_bd[fp_ctl_index]   = cvt_dh_to_bd(fp_buff,fp_index);
			} else if (fp_reg_ctl[fp_ctl_index] == fp_ctl_db){
				fp_reg_type[fp_ctl_index] = fp_lh_type;
				fp_reg_ctl[fp_ctl_index]  = fp_ctl_bd1;
				fp_reg_ctl[fp_ctl_index+1]= fp_ctl_bd2;
				fp_reg_bd[fp_ctl_index]   = new BigDecimal(fp_reg_db[fp_ctl_index],fp_d_context);
			} else {
				set_psw_check(psw_pic_spec);
			}
		}
		return fp_reg_bd[fp_ctl_index];
	} else {
		return cvt_dh_to_bd(fp_buff,fp_index);
	}
}
private BigDecimal fp_get_bd_from_db(ByteBuffer fp_buff,int fp_index){
	/*
	 * get big decimal from DB long bin in fp_reg or mem
	 *   1.  If fp_reg, then check for big dec co-reg
	 *       to avoid conversion
	 */
	return new BigDecimal(fp_get_db_from_db(fp_buff,fp_index),fp_d_context);
}
private BigDecimal fp_get_bd_from_eb(ByteBuffer fp_buff,int fp_index){
	/*
	 * get big decimal from DB long bin in fp_reg or mem
	 *   1.  If fp_reg, then check for big dec co-reg
	 *       to avoid conversion
	 */
	return new BigDecimal(fp_get_eb_from_eb(fp_buff,fp_index),fp_e_context);
}
private BigDecimal fp_get_bd_from_eh(ByteBuffer fp_buff,int fp_index){
	/*
	 * get big decimal from EH short hex in fp_reg or mem
	 *   1.  If fp_reg, then check for big dec co-reg
	 *       to avoid conversion
	 */
	if (fp_buff == fp_reg){
		int fp_ctl_index = fp_index >> 3;
		if (fp_reg_ctl[fp_ctl_index] != fp_ctl_bd1){
			if (fp_reg_ctl[fp_ctl_index] == fp_ctl_ld){ // set by LE/LD
				fp_reg_type[fp_ctl_index] = fp_lh_type;
				fp_reg_ctl[fp_ctl_index]  = fp_ctl_bd1;
				fp_reg_ctl[fp_ctl_index+1]= fp_ctl_bd2;
				fp_reg_bd[fp_ctl_index]   = cvt_eh_to_bd(fp_buff,fp_index);
			} else if (fp_reg_ctl[fp_ctl_index] == fp_ctl_db){
				fp_reg_type[fp_ctl_index] = fp_lh_type;
				fp_reg_ctl[fp_ctl_index]  = fp_ctl_bd1;
				fp_reg_ctl[fp_ctl_index+1]= fp_ctl_bd2;
				fp_reg_bd[fp_ctl_index]   = new BigDecimal(fp_reg_db[fp_ctl_index],fp_e_context);
			} else {
				set_psw_check(psw_pic_spec);
			}
		}
		return fp_reg_bd[fp_ctl_index];
	} else {
		return cvt_eh_to_bd(fp_buff,fp_index);
	}
}
private void fp_put_eb(int fp_index,byte reg_type,float reg_value){
	/*
	 * store eb co-reg and set type 
	 * Note:
	 *   1.  Only fp_eb_type uses float co-reg due
	 *       to exponent range exceeded for fp_eh_type
	 */
	int fp_ctl_index = fp_index >> 3;
    fp_reset_reg(fp_ctl_index,fp_ctl_eb,reg_type);
	fp_reg_eb[fp_ctl_index]   = reg_value;
}
private void fp_put_db(int fp_index,byte reg_type,double reg_value){
	/*
	 * store db co-reg and set type
	 */
	int fp_ctl_index = fp_index >> 3;
    fp_reset_reg(fp_ctl_index,fp_ctl_db,reg_type);
	fp_reg_db[fp_ctl_index]   = reg_value;
}
private void fp_put_bd(int fp_index,byte reg_type,BigDecimal reg_value){
	/*
	 * store db co-reg and set type for reg pair
	 */
	int fp_ctl_index = fp_index >> 3;
	fp_reset_reg(fp_ctl_index,fp_ctl_bd1,reg_type);
	fp_reg_bd[fp_ctl_index]   = reg_value;
}
private void fp_load_reg(byte reg_type1,ByteBuffer reg_buff2,int reg_buff_index2,byte reg_type2){
	/*
	 * convert reg_buff2,ref_buff_index2,reg_type2
	 * to rf1,reg_type1 using co-reg if possible
	 * else using source conversion routines as required
	 * Notes:
	 *   1.  bd1/bd2 co-reg pair being partially
	 *       replaced is discarded without conversion.
	 */
     fp_reset_reg(mf1,fp_ctl_ld,reg_type1);
	 if (reg_buff2 == fp_reg){
	    int fp_ctl_index2 = reg_buff_index2 >>> 3;
	    switch (fp_reg_ctl[fp_ctl_index2]){
        case 0: // fp_ctl_ld
        	break; // exit and use memory cvt rtns
     	case 1: // fp_ctl_eb
     		switch (reg_type1){
      		case 0: // fp_db_type
    			fp_reg_db[mf1] = fp_reg_eb[fp_ctl_index2];
                fp_reg_ctl[mf1] = fp_ctl_db;
                return;
    		case 1: // fp_dh_type
    			fp_reg_db[mf1] = fp_reg_eb[fp_ctl_index2];
                fp_reg_ctl[mf1] = fp_ctl_db;
                return;
            case 2: // fp_eb_type
    			fp_reg_eb[mf1] = fp_reg_eb[fp_ctl_index2];
                fp_reg_ctl[mf1] = fp_ctl_eb;
                return;
    		case 3: // fp_eh_type
    			fp_reg_db[mf1] = fp_reg_eb[fp_ctl_index2];
                fp_reg_ctl[mf1] = fp_ctl_db;
                return;
     		case 4: // fp_lb_type
    			fp_reg_bd[mf1] = BigDecimal.valueOf(fp_reg_eb[fp_ctl_index2]);
                fp_reg_ctl[mf1] = fp_ctl_bd1;
                return;
     		case 5: // fp_lh_type
    			fp_reg_bd[mf1] = BigDecimal.valueOf(fp_reg_eb[fp_ctl_index2]);
                fp_reg_ctl[mf1] = fp_ctl_bd1;
                return;
    		}
    		break;
    	case 2: // fp_ctl_db
     		switch (reg_type1){
      		case 0: // fp_db_type
    			fp_reg_db[mf1] = fp_reg_db[fp_ctl_index2];
                fp_reg_ctl[mf1] = fp_ctl_db;
                return;
    		case 1: // fp_dh_type
    			fp_reg_db[mf1] = fp_reg_db[fp_ctl_index2];
                fp_reg_ctl[mf1] = fp_ctl_db;
                return;
            case 2: // fp_eb_type
    			fp_reg_eb[mf1] = (float) fp_reg_db[fp_ctl_index2];
                fp_reg_ctl[mf1] = fp_ctl_eb;
                return;
    		case 3: // fp_eh_type
    			fp_reg_db[mf1] = fp_reg_db[fp_ctl_index2];
                fp_reg_ctl[mf1] = fp_ctl_db;
                return;
     		case 4: // fp_lb_type
    			fp_reg_bd[mf1] = BigDecimal.valueOf(fp_reg_db[fp_ctl_index2]);
                fp_reg_ctl[mf1] = fp_ctl_bd1;
                return;
     		case 5: // fp_lh_type
    			fp_reg_bd[mf1] = BigDecimal.valueOf(fp_reg_db[fp_ctl_index2]);
                fp_reg_ctl[mf1] = fp_ctl_bd1;
                return;
    		}
    		break;
    	case 3: // fp_ctl_bd1
     		switch (reg_type1){
      		case 0: // fp_db_type
    			fp_reg_db[mf1] = fp_reg_bd[fp_ctl_index2].doubleValue();
                fp_reg_ctl[mf1] = fp_ctl_db;
                return;
    		case 1: // fp_dh_type
    			fp_reg_db[mf1] = fp_reg_bd[fp_ctl_index2].doubleValue();
                fp_reg_ctl[mf1] = fp_ctl_db;
                return;
            case 2: // fp_eb_type
    			fp_reg_eb[mf1] = fp_reg_bd[fp_ctl_index2].floatValue();
                fp_reg_ctl[mf1] = fp_ctl_eb;
                return;
    		case 3: // fp_eh_type
    			fp_reg_db[mf1] = fp_reg_bd[fp_ctl_index2].floatValue();
                fp_reg_ctl[mf1] = fp_ctl_db;
                return;
     		case 4: // fp_lb_type
    			fp_reg_bd[mf1] = fp_reg_bd[fp_ctl_index2];
                fp_reg_ctl[mf1] = fp_ctl_bd1;
                return;
     		case 5: // fp_lh_type
    			fp_reg_bd[mf1] = fp_reg_bd[fp_ctl_index2];
                fp_reg_ctl[mf1] = fp_ctl_bd1;
                return;
    		}
     		break;
        case 4: // fp_ctl_bd2
            set_psw_check(psw_pic_spec); // invalid LH/LB reg ref.
     		break;
    	}
	 }
	 switch (reg_type2){
	 case 0: // fp_db_type
 		switch (reg_type1){
  		case 0: // fp_db_type
  		 	fp_reg.putLong(rf1,reg_buff2.getLong(reg_buff_index2));
            return;
		case 1: // fp_dh_type
			fp_reg_db[mf1] = fp_get_db_from_db(reg_buff2,reg_buff_index2);
            fp_reg_ctl[mf1] = fp_ctl_db;
            return;
        case 2: // fp_eb_type
			fp_reg_eb[mf1] = (float) fp_get_db_from_db(reg_buff2,reg_buff_index2);
            fp_reg_ctl[mf1] = fp_ctl_eb;
            return;
		case 3: // fp_eh_type
			fp_reg_db[mf1] = fp_get_db_from_db(reg_buff2,reg_buff_index2);
            fp_reg_ctl[mf1] = fp_ctl_db;
            return;
 		case 4: // fp_lb_type
			fp_reg_bd[mf1] = fp_get_bd_from_db(reg_buff2,reg_buff_index2);
            fp_reg_ctl[mf1] = fp_ctl_bd1;
            return;
 		case 5: // fp_lh_type
			fp_reg_bd[mf1] = fp_get_bd_from_db(reg_buff2,reg_buff_index2);
            fp_reg_ctl[mf1] = fp_ctl_bd1;
            return;
		}
 		break;
	 case 1: // fp_dh_type
 		switch (reg_type1){
  		case 0: // fp_db_type
			fp_reg_db[mf1] = fp_get_db_from_dh(reg_buff2,reg_buff_index2);
            fp_reg_ctl[mf1] = fp_ctl_db;
            return;
		case 1: // fp_dh_type
  		 	fp_reg.putLong(rf1,reg_buff2.getLong(reg_buff_index2));
            return;
        case 2: // fp_eb_type
			fp_reg_eb[mf1] = (float) fp_get_db_from_dh(reg_buff2,reg_buff_index2);
            fp_reg_ctl[mf1] = fp_ctl_eb;
            return;
		case 3: // fp_eh_type
			fp_reg_db[mf1] = fp_get_db_from_dh(reg_buff2,reg_buff_index2);
            fp_reg_ctl[mf1] = fp_ctl_db;
            return;
 		case 4: // fp_lb_type
			fp_reg_bd[mf1] = fp_get_bd_from_dh(reg_buff2,reg_buff_index2);
            fp_reg_ctl[mf1] = fp_ctl_bd1;
            return;
 		case 5: // fp_lh_type
			fp_reg_bd[mf1] = fp_get_bd_from_dh(reg_buff2,reg_buff_index2);
            fp_reg_ctl[mf1] = fp_ctl_bd1;
            return;
		}
 		break;
	 case 2: // fp_eb_type
 		switch (reg_type1){
  		case 0: // fp_db_type
			fp_reg_db[mf1] = fp_get_db_from_eb(reg_buff2,reg_buff_index2);
            fp_reg_ctl[mf1] = fp_ctl_db;
            return;
		case 1: // fp_dh_type
			fp_reg_db[mf1] = fp_get_db_from_eb(reg_buff2,reg_buff_index2);
            fp_reg_ctl[mf1] = fp_ctl_db;
            return;
        case 2: // fp_eb_type
    	 	fp_reg.putInt(rf1,reg_buff2.getInt(reg_buff_index2));
            return;
		case 3: // fp_eh_type
			fp_reg_db[mf1] = fp_get_db_from_eb(reg_buff2,reg_buff_index2);
            fp_reg_ctl[mf1] = fp_ctl_db;
            return;
 		case 4: // fp_lb_type
			fp_reg_bd[mf1] = fp_get_bd_from_eb(reg_buff2,reg_buff_index2);
            fp_reg_ctl[mf1] = fp_ctl_bd1;
            return;
 		case 5: // fp_lh_type
			fp_reg_bd[mf1] = fp_get_bd_from_eb(reg_buff2,reg_buff_index2);
            fp_reg_ctl[mf1] = fp_ctl_bd1;
            return;
		}
 		break;
	 case 3: // fp_eh_type
 		switch (reg_type1){
  		case 0: // fp_db_type
			fp_reg_db[mf1] = fp_get_db_from_eh(reg_buff2,reg_buff_index2);
            fp_reg_ctl[mf1] = fp_ctl_db;
            return;
		case 1: // fp_dh_type
			fp_reg_db[mf1] = fp_get_db_from_eh(reg_buff2,reg_buff_index2);
            fp_reg_ctl[mf1] = fp_ctl_db;
            return;
        case 2: // fp_eb_type
			fp_reg_eb[mf1] = (float) fp_get_db_from_eh(reg_buff2,reg_buff_index2);
            fp_reg_ctl[mf1] = fp_ctl_eb;
            return;
		case 3: // fp_eh_type
		 	fp_reg.putInt(rf1,reg_buff2.getInt(reg_buff_index2));
            return;
 		case 4: // fp_lb_type
			fp_reg_bd[mf1] = fp_get_bd_from_eh(reg_buff2,reg_buff_index2);
            fp_reg_ctl[mf1] = fp_ctl_bd1;
            return;
 		case 5: // fp_lh_type
			fp_reg_bd[mf1] = fp_get_bd_from_eh(reg_buff2,reg_buff_index2);
            fp_reg_ctl[mf1] = fp_ctl_bd1;
            return;
		}
 		break;
	 case 4: // fp_lb_type
 		switch (reg_type1){
  		case 0: // fp_db_type
			fp_reg_db[mf1] = fp_get_bd_from_lb(reg_buff2,reg_buff_index2).doubleValue();
            fp_reg_ctl[mf1] = fp_ctl_db;
            return;
		case 1: // fp_dh_type
			fp_reg_db[mf1] = fp_get_bd_from_lb(reg_buff2,reg_buff_index2).doubleValue();
            fp_reg_ctl[mf1] = fp_ctl_db;
            return;
        case 2: // fp_eb_type
			fp_reg_eb[mf1] =  fp_get_bd_from_lb(reg_buff2,reg_buff_index2).floatValue();
            fp_reg_ctl[mf1] = fp_ctl_eb;
            return;
		case 3: // fp_eh_type
			fp_reg_db[mf1] = fp_get_bd_from_lb(reg_buff2,reg_buff_index2).doubleValue();
            fp_reg_ctl[mf1] = fp_ctl_db;
            return;
 		case 4: // fp_lb_type
 		 	fp_reg.putLong(rf1,reg_buff2.getLong(reg_buff_index2));
 		 	fp_reg.putLong(rf1+8,reg_buff2.getLong(reg_buff_index2+8));
            return;
 		case 5: // fp_lh_type
			fp_reg_bd[mf1] = fp_get_bd_from_lh(reg_buff2,reg_buff_index2);
            fp_reg_ctl[mf1] = fp_ctl_bd1;
            return;
		}
 		break;
	 case 5: // fp_lh_type
 		switch (reg_type1){
  		case 0: // fp_db_type
			fp_reg_db[mf1] = fp_get_db_from_lh(reg_buff2,reg_buff_index2);
            fp_reg_ctl[mf1] = fp_ctl_db;
            return;
		case 1: // fp_dh_type
			fp_reg_db[mf1] = fp_get_db_from_lh(reg_buff2,reg_buff_index2);
            fp_reg_ctl[mf1] = fp_ctl_db;
            return;
        case 2: // fp_eb_type
			fp_reg_eb[mf1] = (float) fp_get_db_from_lh(reg_buff2,reg_buff_index2);
            fp_reg_ctl[mf1] = fp_ctl_eb;
            return;
		case 3: // fp_eh_type
			fp_reg_db[mf1] = fp_get_db_from_lh(reg_buff2,reg_buff_index2);
            fp_reg_ctl[mf1] = fp_ctl_db;
            return;
 		case 4: // fp_lb_type
			fp_reg_bd[mf1] = fp_get_bd_from_lh(reg_buff2,reg_buff_index2);
            fp_reg_ctl[mf1] = fp_ctl_bd1;
            return;
 		case 5: // fp_lh_type
 		 	fp_reg.putLong(rf1,reg_buff2.getLong(reg_buff_index2));
 		 	fp_reg.putLong(rf1+8,reg_buff2.getLong(reg_buff_index2+8));
            return;
		}
	 	break;
	 }
	 set_psw_check(psw_pic_spec); // should not occur
}
private void fp_reset_reg(int reg_ctl_index,byte ctl_type,byte reg_type){
	/*
	 * reset mf1 register and co-reg to specified type
	 */
	 if (fp_reg_ctl[reg_ctl_index] == fp_ctl_bd1){
		fp_reg_ctl[reg_ctl_index+1] = fp_ctl_ld; // cancel replaced bd
	 } else if (fp_reg_ctl[reg_ctl_index] == fp_ctl_bd2){
	 	fp_reg_ctl[reg_ctl_index-1] = fp_ctl_ld; // cancel replaced bd
	 }
	 fp_reg_ctl[reg_ctl_index] = ctl_type;
	 fp_reg_type[reg_ctl_index] = reg_type;
	 if (ctl_type == fp_ctl_bd1){
		if ((reg_ctl_index & 1) != 0){
			set_psw_check(psw_pic_spec);
		}
		fp_reg_ctl[reg_ctl_index+1]  = fp_ctl_bd2;
		fp_reg_type[reg_ctl_index+1] = reg_type;
	 }
}
private void fp_store_reg(ByteBuffer reg_buff,int reg_index){
	/* 
	 * convert co-reg back to fp_reg storage format
	 * for use by STE or STD.  If byte buffer is not
	 * fp_reg, copy fp_reg to byte buffer if needed
	 * for use in non destructive trace out of regs.
	 */
	int fp_ctl_index = reg_index >> 3;
	switch (fp_reg_ctl[fp_ctl_index]){
	case 0: // fp_ctl_ld
		if (reg_buff != fp_reg){
			reg_buff.putLong(reg_index,fp_reg.getLong(reg_index));
		}
		break;
	case 1: // fp_ctl_eb
		reg_buff.putFloat(reg_index,fp_reg_eb[fp_ctl_index]);
		break;
	case 2: // fp_ctl_db
		switch (fp_reg_type[fp_ctl_index]){
		case 0: // fp_db_type
			reg_buff.putDouble(reg_index,fp_reg_db[fp_ctl_index]);
			break;
		case 1: // fp_dh_type
			reg_buff.putLong(reg_index,cvt_db_to_dh(fp_reg_db[fp_ctl_index]));
			break;
		case 3: // fp_eh_type
			reg_buff.putInt(reg_index,cvt_db_to_eh(fp_reg_db[fp_ctl_index]));
			break;
		}
		break;
	case 3: // fp_ctl_bd1
 		switch (fp_reg_type[fp_ctl_index]){
 		case 4: // fp_lb_type
 		 	cvt_bd(reg_buff,reg_index,fp_lb_type,fp_reg_bd[fp_ctl_index]);
		    break;
 		case 5: // fp_lh_type
 		 	cvt_bd(reg_buff,reg_index,fp_lh_type,fp_reg_bd[fp_ctl_index]);
 		 	break;
 		}
 		break;
    case 4: // fp_ctl_bd2
 		switch (fp_reg_type[fp_ctl_index-1]){
 		case 4: // fp_lb_type
 		 	cvt_bd(reg_buff,reg_index-8,fp_lb_type,fp_reg_bd[fp_ctl_index-1]);
		    break;
 		case 5: // fp_lh_type
 		 	cvt_bd(reg_buff,reg_index-8,fp_lh_type,fp_reg_bd[fp_ctl_index-1]);
 		 	break;
 		}
 		break;
	}
}
private double cvt_eh_to_db(int eh1){
	/*
	 * convert EH format to double
	 */
	if (eh1 < 0){
		long_sign = long_high_bit;
	} else {
		long_sign = 0;
	}
	long_exp  = (long)((((eh1 & int_eh_exp_bits) >>> 24) 
	            - 0x40) << 2) -4;
	long_man  = (long)(eh1 & int_eh_man_bits) << (52-24+4);
	if (long_man == 0){
		return 0;
	}
	while (long_man > long_db_one_bits){
		long_man = long_man >> 1;
		long_exp++;
	}
	work_fp_reg.putLong(0,
			long_sign
		    | ((long_exp + 0x3ff) << 52)
			| (long_man & long_db_man_bits));
    return work_fp_reg.getDouble(0);
}
private double cvt_dh_to_db(long dh1){
	/*
	 * convert DH long hex format to
	 * DB double format
	 */
	if (dh1 < 0){
		long_sign = long_high_bit;
	} else {
		long_sign = 0;
	}
	long_exp  = (long)((((dh1 & long_dh_exp_bits) >>> 56) 
	            - 0x40) << 2) -4;
	long_man  = dh1 & long_dh_man_bits;
	if (long_man == 0){
		return 0;
	}
	fp_round = 0;
	while (long_man > long_db_one_bits){
		fp_round = (int)(long_man & 1);
		long_man = long_man >> 1;
		long_exp++;
		if (fp_round == 1 
			&& long_man <= long_db_one_bits){
			long_man++;
		}
	}
	work_fp_reg.putLong(0,
			long_sign
		    | ((long_exp + 0x3ff) << 52)
			| (long_man & long_db_man_bits));
    return work_fp_reg.getDouble(0);
}
private int cvt_db_to_eh(double db1){
	/*
	 * convert db float to eh int
	 */
	work_fp_reg.putDouble(0,db1);
	long_work = work_fp_reg.getLong(0);
	if (long_work == 0){
		return int_eh_zero;
	}
	fp_exp = (int)((long)(long_work & long_db_exp_bits) >>> 52)
	         - 0x3ff - 4;
	int_man = (int)(((long_work & long_db_man_bits) | long_db_one_bit) >>> (52-24-4));
	fp_round = 0;
	while (int_man > int_eh_man_bits
			| (fp_exp & 3) != 0){
    	fp_round = int_man & 1;
		int_man = int_man >>> 1;
    	fp_exp++;
        if (fp_round == 1 
        	&& int_man <= int_eh_man_bits){
        	int_man++;
        }
	}
    if (long_work < 0){
    	fp_sign = int_high_bit;
    } else {
    	fp_sign = 0;
    }
    return fp_sign
	       | (((fp_exp >> 2) + 0x40) << 24)
           | int_man;
}
private long cvt_db_to_dh(double db1){
	/*
	 * convert db float to eh long
	 */
	work_fp_reg.putDouble(0,db1);
	long_work = work_fp_reg.getLong(0);
	if (long_work == 0){
		return long_dh_zero;
	}
	fp_exp = (int)((long)(long_work & long_db_exp_bits) >>> 52)
	         - 0x3ff;
	long_man = ((long)(long_work & long_db_man_bits) | long_db_one_bit) << 4;
    fp_round = 0;
	while (long_man > long_dh_man_bits
			|| (fp_exp & 3) != 0){
    	fp_round = (int)(long_man & 1);
		long_man = long_man >>> 1;
    	fp_exp++;
        if (fp_round == 1
        	&& long_man <= long_dh_man_bits){
        	long_man++;
        }
	}
    if (long_work < 0){
    	long_sign = long_high_bit;
    } else {
    	long_sign = 0;
    }
    return long_sign
	       | ((long)((fp_exp >> 2) + 0x40) << 56)
           | long_man;
}
private BigDecimal cvt_eh_to_bd(ByteBuffer eh1_buff, int eh1_index){
	/*
	 * convert EH in fp_reg or mem to big_dec
	 * 
	 */
	return new BigDecimal(cvt_eh_to_db(eh1_buff.getInt(eh1_index)),fp_e_context);
}
private BigDecimal cvt_dh_to_bd(ByteBuffer dh1_buff, int dh1_index){
	/*
	 * convert DH in fp_reg or mem to big_dec
	 * 
	 * first create big dec with 56 mantissa bits
	 */
	return new BigDecimal(cvt_dh_to_db(dh1_buff.getLong(dh1_index)),fp_d_context);
}
private BigDecimal cvt_lh_to_bd(ByteBuffer lh1_buff, int lh1_index){
	/*
	 * convert LH in fp_reg or mem to big_dec
	 * 
	 * first copy 112 bit mantissa into 15 byte
	 * array with leading 0 byte and convert to
	 * bd1 big decimal with 128 bit precision
	 */
	lh1_buff.position(lh1_index+1);
	lh1_buff.get(work_fp_bi1_bytes,1,7);
	lh1_buff.position(lh1_index+9);
	lh1_buff.get(work_fp_bi1_bytes,8,7);
	work_fp_bi1_bytes[0] = 0;
	work_fp_bd1 = new BigDecimal(new BigInteger(1,work_fp_bi1_bytes),fp_bd_context);
	/*
	 * if mantinssa zero exit with big decimal zero
	 */
	if (work_fp_bd1.signum() == 0){
		return work_fp_bd1;
	}
	/*
	 * get sign and exponent as integer
	 */
	int_work = (int)lh1_buff.get(lh1_index);
	fp_exp  = (((int_work & 0x7f) - 0x40) << 2) - 112;
	/*
	 * multiply big decimal by 2 ** exp 
	 */
	work_fp_bd1 = work_fp_bd1.multiply(BigDecimal.valueOf(2).pow(fp_exp,fp_bd_context),fp_bd_context).round(fp_x_context);
	/*
	 * return pos or neg big decimal value 
	 */
	if (int_work >= 0){
		return work_fp_bd1;
	} else {
		return work_fp_bd1.negate();
	}
}
private BigDecimal cvt_lb_to_bd(ByteBuffer lb1_buff, int lb1_index){
	/*
	 * convert LB in fp_reg or mem to big_dec
	 * 
	 * first copy 112 bit mantissa into 15 byte
	 * array with leading 0 byte and convert to
	 * bd1 big decimal with 128 bit precision
	 */
	lb1_buff.position(lb1_index+2);
	lb1_buff.get(work_fp_bi1_bytes,1,14);
	work_fp_bi1_bytes[0] = 1;
	work_fp_bd1 = new BigDecimal(new BigInteger(1,work_fp_bi1_bytes));
	/*
	 * if mantinssa zero exit with big decimal zero
	 */
	if (work_fp_bd1.signum() == 0){
		return work_fp_bd1;
	}
	/*
	 * get sign and exponent as integer
	 */
	int_work = lb1_buff.getShort(lb1_index);
	fp_exp  = (int_work & 0x7fff) - 0x3fff - 112;
	/*
	 * multiply big decimal by 2 ** exp 
	 */
	work_fp_bd1 = work_fp_bd1.multiply(BigDecimal.valueOf(2).pow(fp_exp,fp_bd_context),fp_bd_context).round(fp_x_context);
	if (work_fp_bd1.compareTo(fp_lb_min) < 0){
		return BigDecimal.ZERO;
	}
	/*
	 * return pos or neg big decimal value 
	 */
	if (int_work >= 0){
		return work_fp_bd1;
	} else {
		return work_fp_bd1.negate();
	}
}
private void cvt_bd(ByteBuffer fp_buff,int fp_index,int fp_type,BigDecimal fp_bd){
	/*
	 * store 16 byte LH or LB floating point field
	 * from big decimal
     */
	if (fp_bd.signum() > 0){
		fp_sign = 0;
	} else if (fp_bd.signum() < 0){
		fp_sign = fp_sign_bit[fp_type];
		fp_bd = fp_bd.abs();
	} else {
		switch (fp_type){  // gen zero hex for fp_type
		case 4: // fp_lb_type s1,e15,m112 with assumed 1
			fp_buff.position(fp_index);
			fp_buff.put(fp_lb_zero);
			return;
		case 5: // fp_lh_type s1,e7,m112 with split hex	
			fp_buff.position(fp_index);
			fp_buff.put(fp_lh_zero);
			return;
		}
	}
	/***************************************
	 * calc fp_exp and big_dec2 such that: * 
	 * big_dec1 = big_dec2 * 2  ** fp_exp  *
	 *************************************** 
	 * 
	 * fp_exp = log(big_dec1) / log(2)
	 * 
	 * Since the exponent of LH/LB values can 
	 * exceed the range of double, the log of
	 * big_dec1 is calculated using equivalent
	 * form log(x*10*scale) = log(x_man) + scale * log(10) 
	 * 
	 * fp_exp must then be offset by the number
	 * of bits in the required mantissa in order
	 * to retain significant bits when big_dec2
	 * is converted to big_int format.  The exponent
	 * is also reduced by 1 for ssumed bit in binary 
	 * formats plus 1 additional to insure rounding for
	 * irrational values is done by shifting right.
	 * 
	 */ 
	int    work_scale  =  - fp_bd.stripTrailingZeros().scale();
	double work_man    =    fp_bd.multiply(
		BigDecimal.TEN.pow(-work_scale,fp_bd_context),fp_bd_context).doubleValue();
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
	 * big_dec2 = f[_bd / 2 ** fp_exp/
	 * 
	 */
	fp_big_dec2 = fp_bd.multiply(BigDecimal.valueOf(2).pow(-fp_exp,fp_bd_context),fp_bd_context);
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
	switch (fp_type){  // gen byte array for fp type
	case 4: // fp_lb_type s1,e15,m112 with assumed 1
		fp_round = 0;
		while (fp_big_int1.compareTo(fp_big_int_one_bits) > 0){
			if (fp_big_int1.testBit(0)){
				fp_round = 1;
			} else {
				fp_round = 0;
			}
			fp_big_int1 = fp_big_int1.shiftRight(1);
			fp_exp++;
			if (fp_round == 1
				&& fp_big_int1.compareTo(fp_big_int_one_bits) <= 0 ){
				fp_big_int1 = fp_big_int1.add(BigInteger.ONE);
			}
		}
		fp_exp = fp_exp + fp_exp_bias[fp_type];
		if (fp_exp >= 0 && fp_exp <= fp_exp_max[fp_type]){
			fp_buff.position(fp_index+1);
			fp_buff.put(fp_big_int1.toByteArray());
			fp_buff.putShort(0,(short)(fp_sign | fp_exp));
		} else {
			log_error(89,"floating point value out of range");
			fp_buff.position(fp_index);
			fp_buff.put(fp_lh_zero);
		}
	    break;
	case 5: // fp_lh_type s1,e7,m112 with split hex
        fp_round = 0;
		while (fp_big_int1.compareTo(fp_big_int_man_bits) > 0
				|| (fp_exp & 0x3) != 0){
			if (fp_big_int1.testBit(0)){
				fp_round = 1;
			} else {
				fp_round = 0;
			}
			fp_big_int1 = fp_big_int1.shiftRight(1);
			fp_exp++;
			if (fp_round == 1
					&& fp_big_int1.compareTo(fp_big_int_man_bits) <= 0 ){
					fp_big_int1 = fp_big_int1.add(BigInteger.ONE);
			}
		}
		fp_exp = (fp_exp >> 2) + fp_exp_bias[fp_type];
		if (fp_exp >= 0 && fp_exp <= fp_exp_max[fp_type]){
			fp_buff.put(fp_index,(byte)(fp_sign | fp_exp));
			fp_buff.position(fp_index+2);
			fp_buff.put(fp_big_int1.toByteArray());
            fp_buff.putLong(fp_index+1,fp_buff.getLong(fp_index+2));
            if ((fp_buff.getLong(fp_index+8) & long_dh_man_bits) != 0){
			   fp_buff.put(fp_index+8,(byte)(fp_sign | (fp_exp - 14)));
            } else {
            	fp_buff.put(fp_index+8,(byte)0x40);
            }
		} else {
			log_error(89,"floating point value out of range");
			fp_buff.position(fp_index);
			fp_buff.put(fp_lh_zero);
		}
	    break;
	}
}
private void exec_ed_edmk(boolean edmk_store){
	/*
	 * execute ed or edmk with store into r1
	 */
	psw_cc = psw_cc_equal; // assume last field zero;
    byte target_byte = ' ';
    byte source_byte = ' ';
    int next_digit = ' ';
    int source_sign = 0;
    byte fill_byte = mem_byte[bd1_loc];
    boolean sig_digit = false;
    boolean left_digit = true;
    while (rflen > 0){
    	target_byte = mem_byte[bd1_loc];
    	switch (target_byte){
    	case 0x21: // select significant digit
       	    sig_digit = true;
        case 0x20: // select digit
    		if (left_digit){
    		   source_byte = mem_byte[bd2_loc];
    		   next_digit = (source_byte & 0xf0) >> 4 | 0xf0; 
    		   bd2_loc++;
    		   source_sign = source_byte & 0xf;
    		   if (source_sign <= 9){
    		   	  left_digit = false;
    		   }
    		} else {
    			left_digit = true;
    			next_digit = 0Xf0 | (source_byte & 0x0f);
    		}
    		if (!sig_digit && next_digit == 0xf0){
    			mem_byte[bd1_loc] = fill_byte;
    		} else {
    			mem_byte[bd1_loc] = (byte) next_digit;
    			sig_digit = true;
    			if (edmk_store){
    				edmk_store = false;
    				reg.putInt(r1,bd1_loc);
    			}
    			if (next_digit != 0Xf0){ // assume pos if not zero
    				psw_cc = psw_cc_high;
    			}
    		}
    		if (source_sign > 9){
    			if (source_sign == 0xc || source_sign == 0xf){
  		            sig_digit = false;
    			} else if (psw_cc == psw_cc_high){ 
    			    psw_cc = psw_cc_low;
    			}
  		    }
    	    break;
    	case 0x22: // field separator
    		sig_digit = false;
    		mem_byte[bd1_loc] = fill_byte;
    		psw_cc = psw_cc_equal;
    		break;
    	default:   // any other char in mask
    		if (!sig_digit){
    			mem_byte[bd1_loc] = fill_byte;
    		}
    		break;
    	}
    	bd1_loc++;
    	rflen--;
    }
}
private BigInteger get_pd_big_int(int pdf_loc,int pdf_len){
	/*
	 * convert pd field to big_int and set pd_cc
	 * for use by TP as follows:
	 *   cc0 - sign and digits ok
	 *   cc1 = sign invalid
	 *   cc2 = at least one digit invalid
	 *   cc3 = sign and at least one digit invalid
	 * Notes:
	 *   1.  Raises psw_pic_data if invalid digit
	 *       or sign unless opcode is tp.
	 *   2.  If opcode is tp, sets cc accordingly.
	 */
	BigInteger temp_bd = new BigInteger("0");
	pd_cc = psw_cc0;
	work_mem.position(0);
	work_mem.put(mem_byte,pdf_loc,pdf_len); // avoid 0c5 at end mem
	if (pdf_len <= 4){
		pdf_str = Integer.toHexString(work_mem.getInt(0));
		pdf_zeros = 8 - pdf_str.length();
	} else if (pdf_len <= 8){
		pdf_str = Long.toHexString(work_mem.getLong(0));
		pdf_zeros = 16 - pdf_str.length();
	} else {
		pdf_str = Long.toHexString(work_mem.getLong(0));
		String last_half = Long.toHexString(work_mem.getLong(8));
		pdf_str = pdf_str + ("0000000000000000" + last_half).substring(last_half.length());
		pdf_zeros = 32 - pdf_str.length();
	}
	pdf_str_len = 2*pdf_len-1-pdf_zeros; // assume positive
	pdf_sign = pdf_str.charAt(pdf_str_len);
	if (pdf_str_len == 0){
		pdf_str = "0";
		pdf_str_len = 1;
	}
	switch (pdf_sign){
	case 'a':
	case 'c':
	case 'e':
	case 'f':
		pdf_str = pdf_str.substring(0,pdf_str_len);
	    break;
	case 'b':
	case 'd':
		pdf_str = "-" + pdf_str.substring(0,pdf_str_len);
        break;
    default:
		pdf_str = pdf_str.substring(0,pdf_str_len);
		pd_cc = psw_cc1;
		if (opcode1 != 0xeb || opcode2 != 0xc0){ // not TP
		    fp_dxc = fp_dxc_dec;
			set_psw_check(psw_pic_data);		   
		}
		break;
	}
	try {
	    temp_bd = new BigInteger(pdf_str);
	} catch (Exception e){
		if (pd_cc == psw_cc0){
			pd_cc = psw_cc2; // digit error only
		} else {
			pd_cc = psw_cc3; // digit and sign errors
		}
		if (opcode1 != 0xeb || opcode2 != 0xc0){ // not TP
		    fp_dxc = fp_dxc_dec;
			set_psw_check(psw_pic_data);
		}
	}
	return temp_bd;
}
private void put_pd_big_int(int pdf_loc,int pdf_len){
	/*
	 * store big_int value into pd field
	 * and set pd_cc
	 */
	String pd_str = big_int.toString();
	byte next_byte = 0xc;
	int end_index = 0;
	if (pd_str.charAt(0) == '-'){
		next_byte = 0xd;
		pd_cc = psw_cc_low;
		end_index = 1;
	} else if (pd_str.length() == 1
		       && pd_str.charAt(0) == '0'){
		pd_cc = psw_cc_equal;
	} else {
		pd_cc = psw_cc_high;
	}
	int index = pd_str.length()-1;
	int pdf_index = pdf_loc + pdf_len - 1;
	boolean left_digit = true;
	while (index >= end_index
			&& pdf_index >= pdf_loc){
		if (left_digit){
			next_byte = (byte)(next_byte | ((pd_str.charAt(index) & 0xf) << 4));
			mem.put(pdf_index,next_byte);
			next_byte = 0;
			pdf_index--;
			left_digit = false;
		} else {
			next_byte = (byte)(pd_str.charAt(index) & 0xf);
			left_digit = true;
		}
		index--;
	}
	while (pdf_index >= pdf_loc){
		mem.put(pdf_index,next_byte);
		next_byte = 0;
		pdf_index--;
	}
	if (index >= end_index){
		pd_cc = psw_cc3;
		set_psw_check(psw_pic_pd_ovf);
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
	 *   1.  Usage by ez390 includes the following:
	 *       a.  Lookup opcode by hex opcode - hex_key
	 *       b.  lookup CDE entry by program name - "P" + cde_name
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
			abort_error(21,"key search table exceeded");
		}
		if (key_hash < key_tab_hash[key_index_last]){
		    key_tab_low[key_index_last] = key_index;
	    } else {
		    key_tab_high[key_index_last] = key_index;
	    }
	}
	tot_key++;
	key_tab_key[key_index]   = key_text;
	key_tab_hash[key_index]  = key_hash;
	key_tab_index[key_index] = user_index;
}
/*
 *  end of ez390 code 
 */
}