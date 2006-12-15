import java.lang.reflect.Array;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.MathContext;
import java.math.RoundingMode;
import java.nio.ByteBuffer;
import java.util.Arrays;
import java.util.Date;

public class pz390 {
	/***************************************************************************
	 * 
	 * z390 portable mainframe assembler and emulator.
	 * 
	 * Copyright 2006 Automated Software Tools Corporation
	 * 
	 * z390 is free software; you can redistribute it and/or modify it under the
	 * terms of the GNU General Public License as published by the Free Software
	 * Foundation; either version 2 of the License, or (at your option) any
	 * later version.
	 * 
	 * z390 is distributed in the hope that it will be useful, but WITHOUT ANY
	 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
	 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
	 * details.
	 * 
	 * You should have received a copy of the GNU General Public License along
	 * with z390; if not, write to the Free Software Foundation, Inc., 59 Temple
	 * Place, Suite 330, Boston, MA 02111-1307 USA
	 * 
	 * pz390 is the processor emulator component of z390 which is called from
	 * ez390 to execute 390 code loaded in memory. Both ez390 and pz390 call
	 * sz390 svc component to perform os functions such as memory allocation and
	 * loading.
	 * 
	 * *************************************************** Maintenance
	 * *************************************************** 04/18/05 copied from
	 * lz390.java and modified 06/20/05 start adding and testing common instr.
	 * 06/25/05 add opcodes to trace 07/11/05 add floating point instructions
	 * 08/20/05 add svc 6 link, fix svc 8 to use r0 pgm name 08/22/05 add SYS390
	 *   and SYSLOG dir options 
	 * 09/04/05 add sequential and random DCB file I/O
	 * support 
	 * 09/16/05 fix DSG, DSGR, DSGF, DSGFR to use R1+1 dividend 09/18/05
	 *   add CDE with usage and freemain info for DELETE 09/18/05 add link svc
	 *   amode support 
	 * 09/19/05 add TEST option interactive debug 09/27/05 add
	 * MEM(MB) option and reduce default to MEM(1) 09/27/05 fix 0C5 at end of
	 * mem using work_mem 
	 * 10/04/05 RPI5 - option ASCII use ASCII vs EBCDIC
	 * zcvt_ipl_pgm = ascii name DCB DDNAME field ascii DCB FT RCDS = ascii SVC
	 * LOAD, LINK, DELETE EP/EPLOC ASCII dump text = ascii TEST C'...' SDT =
	 * ASCII ED/EDMK remove high bit on chars SSP type instr. allow 0x3 sign
	 * UNPK gen 0x3 zone 10/04/05 RPI6 - option ERR(nn) limit errors 10/05/05
	 * RPI5 - add test C"..." sdt support 10/12/05 suppress stats if NOSTATS
	 * option on 10/12/05 RPI20 fix error 62 on open of output file 10/14/05
	 * RPI21 0C5 on PSW addr > mem 
	 * 10/14/05 RPI22 turn off time limit if test
	 * 10/14/05 RPI15 req z390 to issue exit at end of test 10/16/05 force
	 * console output of msgs after abort 10/16/05 RPI23 add CLST, CUSE, and
	 * SRST instructions 
	 * 10/18/05 RPI28 change DSNAM field to EBCDIC unless
	 * ASCII mode 
	 * 10/18/05 RPI29 use EZ390E and EZ390I prefixes 10/18/05 RPI31
	 * set r15 to 0 on successful svc 10/19/05 RPI33 only enable PD and exponent
	 * overflow 
	 * 10/19/05 RPI32 add SYS390 dir list support 10/19/05 RPI34 full
	 * ebcdic / ascii translation 
	 * 10/20/05 RPI35 prevent exit loop in test
	 * filling log 
	 * 10/20/05 RPI37 use "I:" and "H:" to separate op/hex keys
	 * 10/20/05 RPI39 set rc=r15 on normal exit 10/22/05 RPI44 fix TM to set CC3
	 * when OR'd byte = mask rather than = to test memory byte 10/23/05 RPI43
	 * correct ED/EDMK sif. 1 byte early 
	 * 10/23/05 RPI42 use full EBCDIC to ASCII
	 * translate for PGMNAME, DDNAM, DSNAM, GET/PUT 10/24/05 RPI46 add svc 34 to
	 * issue OS command 
	 * 10/25/05 RPI47 add svc 93 to support application window
	 * 10/26/05 RPI49 10/27/05 RPI53 set r15 to error # in synad 10/27/05 RPI56
	 * add dump of first pgm memory if dump req 11/01/05 RPI65 fix BALR, BASR,
	 * BASSM when R1=R2 
	 * 11/02/05 RPI66 correct padding spaces in ASCII mode
	 * 11/02/05 RPI69 change ED/EDMK to map X'40'for ASCII 11/03/05 RPI71 add
	 * missing AL3 relocation code 
	 * 11/03/05 RPI63 handle x'1a' eof marker in gm
	 * RT/VT 
	 * 11/03/05 RPI64 issue abend S013 if no synad after io err 11/07/05
	 * RPI73 remove PACK/UNPK mode changes and add PKA/UNPKA 11/08/05 RPI73
	 * change ED/EDMK to require EBCDIC mask and then translate output if ascii
	 * 11/08/05 RPI76 end cmd processes at exit 11/08/05 RPI77 add cmd read and
	 * wait controls 
	 * 11/08/05 RPI79 add mult. cmd task support 11/11/05 RPI73
	 * restore PACK, UNPK ASCII mode support (allow 3/b sign and unpk to f/s or
	 * 3/s zone) 
	 * 11/11/05 RPI75 add SNAP dump support svc 51 11/13/05 RPI88 add
	 * DCB validation 
	 * 11/15/05 RPI92 correct trace format in test 11/15/05 RPI93
	 * correct SNAP svc to use memory range instead of address and length
	 * 11/15/05 RPI94 add svc_timer nano-sec counter 11/18/05 RPI98 ignore cr,lf
	 * TEST commands 
	 * 11/19/05 RPI100 reformat abend dump and include before
	 * continuing in test 
	 * 11/19/05 RPI101 correct open error causing erroneous
	 * eof error after reuse of tiot entry 11/19/05 RPI102 add LOAD extension to
	 * support loading and deleting file in addition to 390's. 11/20/05 RPI106
	 * correct SPM, IPM field position also NR, OR, and XR not setting CC also
	 * PR only restore 2-14 
	 * 11/20/05 RPI82 replace MVC loop with copy & fill
	 * 11/21/05 add LinkedList string type checking 11/21/05 RPI108 speed up BC
	 * by 140 NS when branch not taken by skipping RX address fetch 11/23/05
	 * RPI110 turn off DCBOFLGS open bit at close 11/23/05 RPI108 speed up PD by
	 * removing mem_work and using int vs BigInteger when possible 11/25/05
	 * RPI111 trim spaces from DDNAME and DSNAME file 11/26/05 RPI47 add inital
	 * gz390 GUI window option for WTO and WTOR support 11/27/05 RPI112 correct
	 * error 90 I/O error 
	 * 11/27/05 RPI108 replace byte buffer mem.get, mem.put
	 * reg.get and reg.put with direct byte array. 11/28/05 RPI113 file path
	 * with drive: and no separator 
	 * 11/29/05 RPI47 add svc 1 wait and svc 160
	 * wtor request 
	 * 12/04/05 RPI108 speed up MVCLE, CLC, MVZ, MVN OC, NC, XC.
	 * 12/07/05 RPI123 fix multiple path support 12/08/05 RPI121 abort ez390 on
	 * test q command 
	 * 12/15/05 RPI135 use tz390 shared tables 12/18/05 RPI142
	 * use init_opcode_name_keys in tz390 and document all key codes in tz390.
	 * 12/20/05 RPI103 add multiple +- test addr operands 12/23/05 RPI127 strip
	 * mlc type from file name and use shared set_pgm_name_type 12/23/05 RPI131
	 * limit file output to maxfile(mb) 01/04/06 RPI107 split ez390 into ez390,
	 * pz390 01/06/06 RPI158 clear remainder of register for LL???? 01/08/06
	 * RPI160 correct LLGT loading from loc + 4 01/12/06 RPI 151 add LPSW
	 * support as branch. 
	 * 01/15/06 RPI 173 add LRV, STPQ, LPQ support. 01/26/06
	 * RPI 172 move options to tz390, svc > sz390 02/02/06 RPI 175 rewrite AL???
	 * SL??? using long vs bigint 
	 * 02/02/06 RPI 185 add z9 opcodes AFI, AGFI,
	 * etc. 
	 * 02/04/06 RPI 197 remove test_or_trace interrupts 02/08/06 RPI 185
	 * add STCKF and fix STCK to be unique 02/09/06 RPI 185 add remainder of z9
	 * opcodes 
	 * 02/16/06 RPI 202 correct 2nd opcode mask for z9 instr. 02/18/06
	 * RPI 206 support 3 flavors of RRF format 03/14/06 RPI 228 add CVTDCB OS
	 * flags 
	 * 03/15/06 RPI 229 verify correct even/odd fp pairs 04/05/06 RPI 272
	 * correct MR to only use low 32 bits of r1+1 04/07/06 RPI 275 correct MLR
	 * and ML to only use low 32 bits of r1+1 04/10/06 RPI 276 add zcvt_user_pgm
	 * 04/12/06 RPI 244 support ESPIE/ESTAE PARAM, CT,OV 04/21/06 RPI 279 stimer
	 * real exit support 
	 * 04/23/06 RPI 295 correct ICM, ICMH, ICMY, OIHL, and
	 * OILH CC values. 
	 * 04/26/06 RPI 299 check for 0C5 in setups 04/27/06 RPI 298
	 * add MAYLR MYLR MAYHR MYHR, MAYL MYL MAYH MYH 04/28/06 RPI 301 force 0C7
	 * on PD zero sign was checking was checking first non-zero byte beyond in
	 * error. 
	 * 05/02/06 RPI 305 update ESPIE and ESTAE support and fix get_pd for
	 * pd instr. to avoid recursive program checks 05/02/06 RPI 307 correct MAYL
	 * and MAYLR to remove duplicate setup call corrupting next instruction, and
	 * correct IPM trace format. 06/03/06 RPI 323 allow opcode break on first
	 * stimer exit instr. 
	 * 06/03/06 RPI 325 allow KEB and KDD exact 0 07/03/06
	 * RPI 326 add TCEB, TCDB, and TCXB test data class 07/03/06 RPI 333 add
	 * SRNM and support rounding modes 07/05/06 RPI 335 correct TBEDR and other
	 * users of RRF2 setup to caculate rf3 and mf3 correctly 07/05/06 RPI 348
	 * only show 2 bytes for halfword instr. 07/06/06 RPI 357 impove speed using
	 * short, int, and long buffers 07/17/06 RPI 370 make zcvt conversion rtns
	 * public for svc_cfd 
	 * 07/20/06 RPI 376 CORRECT AL?? CC1 for high bit set
	 * with no carry 
	 * 07/24/06 RPI 383 CORRECT MLG AND MLGR R1+1 * S2/R2 07/26/06
	 * RPI 384 fix HFP true zero 07/30/06 RPI 386 fix MVCL trap when data length =
	 * 0. 
	 * 07/30/06 RPI 387 Fix RXY, RSY, and SIY to support 20 bit signed disp.
	 * 08/06/06 RPI 397 S0C5 on memory violations 08/06/06 RPI 398 fix D and DR
	 * truncated dividend error. 08/27/06 RPI 411 replace while loops with
	 * Arrays.fill and arraycopy 09/06/06 RPI 395 fix IC,STC,SS trace data
	 * lengths 
	 * 09/08/06 RPI 441 add MVST move string and speed up TRT 09/18/06
	 * RPI 453 speedup MVST with byte memory access 09/19/06 RPI 454 add TRE,
	 * TROT, TRTO, TRTT 
	 * 11/04/06 RPI 484 support TRE trace file for TRACE,
	 * TRACEALL 11/10/06 RPI 474 trace invalid opcode if trace on 11/10/06 RPI
	 * 487 speed up MVST using scan and arraycopy 12/06/06 RPI 407 add DFP CSDTR
	 * 12/10/06 RPI 414 add CFD and CTD DFP type conversions
	 * 12/13/06 RPI 407 add DFP initial instruction support
	 * ******************************************************* Global variables
	 * (last RPI)
	 **************************************************************************/
	/*
	 * limits
	 */
	int max_pc_stk = 50; // PC, PR, PT stack for psw and regs /*

	int max_reg_off = 128; // max reg offset 16*8

	/*
	 * shared tables and functions
	 */
	tz390 tz390 = null;

	/*
	 * shared svc handler also accessed by ez390
	 */
	sz390 sz390 = null;

	/*
	 * date variables
	 */
	Date cur_date = null;

	long ibm_mil = 0; // milliseconds from 1900 to 1970 Epoch

	long java_mil = 0; // milliseconds from 1970 to now

	long ibm_ms = 0; // microseconds from 1900 to now

	/*
	 * psw and instruction decode variables
	 */
	int test_trace_count = 0;

	String ins_trace_line = null;

	long cpu_id = 0x390;

	int psw_loc = 0;

	int psw_short = 0;

	short ins_short0 = 0;

	short ins_short1 = 0;

	short ins_short2 = 0;

	long long_high_bit = ((long) -1) << 63;

	int int_high_bit = 0x80000000;

	int max_pos_int = 0x7fffffff;

	int min_neg_int = 0x80000000;

	long max_pos_long = ((long) -1) >>> 1;

	long min_neg_long = ((long) 1) << 63;

	BigInteger bi_max_pos_long = BigInteger.valueOf(max_pos_long);

	BigInteger bi_min_neg_long = BigInteger.valueOf(min_neg_long);

	long long_num_bits = ((long) -1) ^ long_high_bit;

	long long_low32_bits = (((long) 1) << 32) - 1;

	long long_low48_bits = (((long) 1) << 48) - 1;

	long long_high32_bits = ((long) -1) << 32;

	long long_sign_bits = ((long) -1) << 31;

	long long_zone_ones = (((long) 0xf0f0f0f0) << 32)
			| ((long) 0xf0f0f0f0 & long_low32_bits);

	long long_num_ones = (((long) 0x0f0f0f0f) << 32) | (long) 0x0f0f0f0f;

	int psw_amode24 = 0x00ffffff;

	int psw_amode31 = 0x7fffffff;

	int psw_amode24_high_bits = 0xff000000;

	int psw_amode31_high_bit = 0x80000000;

	int psw_amode = psw_amode31;

	int psw_amode_high_bits = (-1) ^ psw_amode;

	int psw_amode24_bit = 0;

	int psw_amode31_bit = 0x80000000;

	int psw_amode_bit = psw_amode31_bit;

	long cur_stck = 0;

	long last_stck = long_high_bit;

	/*
	 * psw_cc program condition code
	 */
	int psw_cc0 = 8; // EQUAL, ZERO.

	int psw_cc1 = 4; // LOW, LT

	int psw_cc2 = 2; // HIGH, GT

	int psw_cc3 = 1; // OVERFLOW, ONES

	// PSW CODE FROM MASK 1 2 4 8
	int[] psw_cc_code = { -1, 3, 2, -1, 1, -1, -1, -1, 0 };

	int[] psw_cc_mask = { 8, 4, 2, 1 };

	int psw_cc_equal = psw_cc0; // mask 8

	int psw_cc_low = psw_cc1; // mask 4

	int psw_cc_high = psw_cc2; // mask 2

	int psw_cc_ovf = psw_cc3; // mask 1

	int psw_cc = psw_cc_equal;

	/*
	 * psw_pgm_mask fields and pgm interruption fields
	 */
	int psw_pgm_mask_fix = 0x8; // fixed point overflow

	int psw_pgm_mask_dec = 0x4; // decimal overflow

	int psw_pgm_mask_exp = 0x2; // fp exponent overflow

	int psw_pgm_mask_sig = 0x1; // fp significance

	int psw_pgm_mask = 0x6; // enable all but fixed and fp sig.//RPI33

	/*
	 * FPC IEEE BFP control registers
	 */
	int fp_fpc_mask_inv = 0x80000000;

	int fp_fpc_mask_div = 0x40000000;

	int fp_fpc_mask_ovf = 0x20000000;

	int fp_fpc_mask_unf = 0x10000000;

	int fp_fpc_mask_sig = 0x08000000;

	int fp_fpc_mask_rnd = 0x00000003; // rounding mode

	int fp_fpc_mask_rnd_not = 0xfffffffc; // not round mode bits

	int fp_fpc_rnd_even = 0x0; // round to nearest (default)

	int fp_fpc_rnd_zero = 0x1; // round toward zero (discard bits to right)

	int fp_fpc_rnd_pi = 0x2; // round toward plus infinity

	int fp_fpc_rnd_ni = 0x3; // round toward negative infinity

	int fp_fpc_rnd = fp_fpc_rnd_even; // default rounding mode

	int fp_fpc_reg = 0x70000000; // div+ovf+unf

	int fp_dxc_dec = 0x00; // packed decimal data exception

	int fp_dxc_it = 0x08; // IEEE inexact and truncated

	int fp_dxc_ii = 0x0C; // IEEE inexact and incremented

	int fp_dxc_ue = 0x10; // IEEE underflow, exact

	int fp_dxc_uit = 0x18; // IEEE underflow, inexact and truncated

	int fp_dxc_uii = 0x1C; // IEEE underflow, inexact and incremented

	int fp_dxc_oe = 0x20; // IEEE overflow, exact

	int fp_dxc_oit = 0x28; // IEEE overflow, inexact and truncated

	int fp_dxc_oii = 0x2C; // IEEE overflow, inexact and incremented

	int fp_dxc_div = 0x40; // IEEE division by zero

	int fp_dxc_oper = 0x80; // IEEE invalid operation

	int fp_dxc = 0; // byte 2 of fp_fpc_reg with IEEE exceptions

	/*
	 * program check and program interruption fields
	 */
	boolean psw_check = false;

	boolean fp_signal = false;

	int psw_pic_exit = 0; // exit normally

	int psw_pic_io = 0x013;

	int psw_pic_oper = 0x0c1;

	int psw_pic_priv = 0x0c2;

	int psw_pic_exec = 0x0c3;

	int psw_pic_prot = 0x0c4;

	int psw_pic_addr = 0x0c5;

	int psw_pic_spec = 0x0c6;

	int psw_pic_data = 0x0c7;

	int psw_pic_fx_ovf = 0x0c8;

	int psw_pic_fx_div = 0x0c9;

	int psw_pic_pd_ovf = 0x0ca;

	int psw_pic_pd_div = 0x0cb;

	int psw_pic_fp_ovf = 0x0cc;

	int psw_pic_fp_unf = 0x0cd;

	int psw_pic_fp_sig = 0x0ce;

	int psw_pic_fp_div = 0x0cf;

	int psw_pic_timeout = 0x422;

	int psw_pic_gm_err = 0x804; // getmain request invalid

	int psw_pic_link_err = 0x806; // link failed

	int psw_pic_no_mem = 0x80a; // out of memory

	int psw_pic_fm_err = 0x90a; // freemain request invalid

	int psw_pic_bad_mem = 0xa0a; // memory corruption

	int psw_pic_stkerr = 0xf01; // pc stack errror

	int psw_pic_operr = 0xf02; // opcode mask error

	int psw_pic_interr = 0xf03; // Bigint error

	int psw_pic_memerr = 0xf04; // memory exceeded

	int psw_pic_waiterr = 0xf05; // wait for pz390 thread error

	int psw_pic_error = 0xfff; // internal error

	int psw_pic = 0;

	// psw_cc? code = 3 2 1 0
	// psw_cc value = 1 2 4 8 //RPI174
	int[] psw_carry = { -1, 1, 1, -1, 0, -1, -1, -1, 0 }; // index by psw_cc

	int[] psw_borrow = { -1, 0, 0, -1, 1, -1, -1, -1, 1 }; // index by psw_cc

	int psw_ins_len = 0;

	int opcode1 = 0;

	int opcode2 = -1;

	int opcode2_offset_e = 1; // E PR oooo

	int opcode2_offset_ri = 1; // RI IIHH ooroiiii

	int opcode2_offset_rie = 5; // RIE BRXLG oorriiii00oo

	int opcode2_offset_ril = 1; // RIL BRCL oomollllllll

	int opcode2_offset_rrf = 1; // RRF MAER oooor0rr

	int opcode2_offset_rre = 1; // RRE MSR oooo00rr

	int opcode2_offset_rsl = 5; // RSL TP oor0bddd00oo

	int opcode2_offset_rsy = 5; // RSY LMG oorrbdddhhoo

	int opcode2_offset_rxf = 5; // RXF MAE oorxbdddr0oo

	int opcode2_offset_rxe = 5; // RXE ADB oorxbddd00oo

	int opcode2_offset_rxy = 5; // RXY MLG oorxbdddhhoo

	int opcode2_offset_s = 1; // S SSM oooobddd

	int opcode2_offset_siy = 5; // SIY TMY ooiibdddhhoo

	int opcode2_offset_sse = 1; // SSE LASP oooobdddbddd

	int opcode2_offset_ssf = 1; // SSF MVCOS oorobdddbddd

	int dup_opcodes = 0; // count of duplicate opcodes

	int max_espie = 50;

	int tot_espie = 0;

	int espie_last_ins = 0;

	boolean espie_exit_running = false;

	int[] espie_pie = (int[]) Array.newInstance(int.class, max_espie); // espie
																		// psw_pic
																		// bit
																		// mask
																		// from
																		// R0

	int[] espie_exit = (int[]) Array.newInstance(int.class, max_espie); // espie
																		// exit
																		// addr
																		// from
																		// R1

	int[] espie_parm = (int[]) Array.newInstance(int.class, max_espie); // parm
																		// address
																		// if
																		// not
																		// zero
																		// from
																		// R15

	int max_estae = 50;

	int tot_estae = 0;

	int estae_last_ins = 0;

	boolean estae_exit_running = false;

	int[] estae_exit = (int[]) Array.newInstance(int.class, max_estae);

	int[] estae_parm = (int[]) Array.newInstance(int.class, max_estae);

	int if1 = 0;

	int if2 = 0;

	int sv1 = 0;

	int rflen = 0;

	int rflen1 = 0;

	int rflen2 = 0;

	int rf1 = 0;

	int rf2 = 0;

	int rf3 = 0;

	int mf1 = 0;

	int mf2 = 0;

	int mf3 = 0;

	int mf4 = 0;

	int[] mask_bits = { 0, 1, 1, 2, 1, 2, 2, 3, 1, 2, 2, 3, 2, 3, 3, 4 };

	int rv1 = 0;

	int rv2 = 0;

	int rv3 = 0;

	int rvw = 0;

	long rlv1 = 0;

	long rlv2 = 0;

	long rlv3 = 0;

	long rlvw = 0;

	int bf1 = 0;

	int df1 = 0;

	int xf2 = 0;

	int bf2 = 0;

	int df2 = 0;

	int bd1_loc = 0;

	int bd2_loc = 0;

	int bd1_start = 0;

	int bd2_start = 0;

	int xbd2_loc = 0;

	int bd1_end = 0;

	byte string_eod = 0;

	byte test_control = 0; // RPI 454

	byte test_byte1 = 0; // RPI 454

	byte test_byte2 = 0; // RPI 454

	boolean string_eod_found = false;

	boolean fields_equal = true;

	int bd2_end = 0;

	int data_len = 0;

	int pad_len = 0;

	byte fill_char;

	boolean ex_mode = false;

	int ex_opcode = 0x44;

	byte ex_mod_byte = 0; // save targe+1 byte

	int ex_psw_return = 0; // return from ex

	byte[] pd_bytes = (byte[]) Array.newInstance(byte.class, 16);

	ByteBuffer pd_byte = ByteBuffer.wrap(pd_bytes, 0, 16);

	String pdf_str = null;

	int pdf_str_len = 0;

	int pdf_zeros = 0;

	char pdf_sign = '+';

	int pdf_zone = 0xf0;

	byte pdf_next_out = 0;

	byte pdf_next_in = 0;

	boolean pdf_next_right = true;

	BigInteger big_int = null;

	BigInteger big_int1 = null;

	BigInteger big_int2 = null;

	boolean pdf_is_big = false;

	BigInteger pdf_big_int = null;

	BigInteger pdf_big_int1 = null;

	BigInteger pdf_big_int2 = null;

	long pdf_long = 0;

	long pdf_long1 = 0;

	long pdf_long2 = 0;

	BigInteger[] big_int_array = null;

	int pd_cc = 0;

	/*
	 * 16 gpr registers
	 */
	byte[] reg_byte = (byte[]) Array.newInstance(byte.class, 16 * 8);

	ByteBuffer reg = ByteBuffer.wrap(reg_byte, 0, 16 * 8);

	int reg_len = 128;

	byte[] work_reg_byte = (byte[]) Array.newInstance(byte.class, 16);

	ByteBuffer work_reg = ByteBuffer.wrap(work_reg_byte, 0, 16);

	byte[] log_reg_byte = new byte[9];

	ByteBuffer log_reg = ByteBuffer.wrap(log_reg_byte, 0, 9);

	int r0 = 4;

	int r1 = 12;

	int r2 = 20;

	int r3 = 28;

	int r4 = 36;

	int r5 = 44;

	int r6 = 52;

	int r7 = 60;

	int r8 = 68;

	int r9 = 76;

	int r10 = 84;

	int r11 = 92;

	int r12 = 100;

	int r13 = 108;

	int r14 = 116;

	int r15 = 124;

	/*
	 * 16 fp registers with eb, db, and bd co-regs to avoid conversions when
	 * possible fp_ctl defines fp reg state as follows: 0 = fp_ctl_ld no co-reg
	 * defined (set by LE,LD) 1 = fp_ctl_eb set for EB float operations 2 =
	 * fp_ctl_db set for EH, DH, DB operations 3 = fp_ctl_bd set for LH, LB
	 * operations Notes: 1. LE and LD set fp_reg with fp_ctl_ld 2. STE and STD
	 * store from fp_reg or co_reg 3. fp_reg is indexed by reg * 8 byte index 4.
	 * fp_ctl and co-regs are indexed by reg #
	 */
	byte[] fp_reg_byte = (byte[]) Array.newInstance(byte.class, 16 * 8);

	ByteBuffer fp_reg = ByteBuffer.wrap(fp_reg_byte, 0, 16 * 8);

	byte[] trace_reg_byte = (byte[]) Array.newInstance(byte.class, 16 * 8);

	ByteBuffer trace_reg = ByteBuffer.wrap(trace_reg_byte, 0, 16 * 8);

	byte fp_ctl_ld = 0;

	byte fp_ctl_eb = 1;  // EB

	byte fp_ctl_db = 2;  // EH, DB

	byte fp_ctl_bd1 = 3; // DH, ED, DD, or first half LB, LH, LD

	byte fp_ctl_bd2 = 4; // second half LB, LH, LD

	byte[] fp_reg_type = (byte[]) Array.newInstance(byte.class, 16);

	byte[] fp_reg_ctl = (byte[]) Array.newInstance(byte.class, 16);

	float[] fp_reg_eb = (float[]) Array.newInstance(float.class, 16);

	double[] fp_reg_db = (double[]) Array.newInstance(double.class, 16);

	BigDecimal[] fp_reg_bd = (BigDecimal[]) Array.newInstance(BigDecimal.class,
			16);

	byte[] work_fp_reg_byte = (byte[]) Array.newInstance(byte.class, 16);

	ByteBuffer work_fp_reg = ByteBuffer.wrap(work_fp_reg_byte, 0, 16);
    boolean[] fp_pair_type  = {
    		false, false, false, // DB, DD, DH
    		false, false, false, // EB, ED, EH
    		true,  true,  true};  // LB, LD, LH
	boolean[] fp_pair_valid = { // RPI 229
	/* 0 1 2 3 */
	        true, true, false, false, // 0 - 3 (0,2) (1,3)
			true, true, false, false, // 4 - 7 (4,6) (5,7)
			true, true, false, false, // 8 - 11 (8,10) (9,11)
			true, true, false, false }; // 12 - 15 (12,14) (13,15)

	/*
	 * fp work variables
	 */
	long lrv1 = 0;

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

	BigDecimal fp_bd_two = BigDecimal.valueOf(2);

	BigDecimal[] fp_bd_int_rem = null; // RPI 333 bd integer and remainder

	BigDecimal fp_bd_inc = null;

	int fp_bd_sqrt_scale = 0;

	byte[] work_fp_bi1_bytes = (byte[]) Array.newInstance(byte.class, 15);

	BigInteger work_fp_bi1 = null;

	BigDecimal work_fp_bd1 = null;

	BigDecimal work_fp_bd2 = null; // RPI 407

	long long_dh_zero = 0; // RPI 384

	long long_dh_exp_bits = (long) (0x7f) << 56;

	long long_dh_man_bits = ((long) (1) << 56) - 1;

	int fp_round = 0;

	long long_work = 0;

	long long_sign = 0;

	long long_exp = 0;

	long long_man = 0;

	long long_db_exp_bits = (long) (0x7ff) << 52;

	long long_db_one_bits = ((long) (1) << 53) - 1;

	long long_db_one_bit = ((long) (1) << 52);

	long long_db_man_bits = ((long) (1) << 52) - 1;

	int int_eh_exp_bits = 0x7f << 24;

	int int_eh_zero = 0x00000000; // RPI 384

	int int_work = 0;

	int int_man = 0;

	int int_eh_man_bits = 0xffffff;

	MathContext fp_bd_context = null;

	MathContext fp_db_context = null;

	MathContext fp_eb_context = null;

	MathContext fp_e_context = MathContext.DECIMAL32; // fp_fpc_rnd default
														// half even

	MathContext fp_d_context = MathContext.DECIMAL64; // fp_fpc_rnd default
														// half even

	MathContext fp_x_context = MathContext.DECIMAL128; // fp_fpc_rnd default
														// half even
    RoundingMode fp_dfp_rnd   = RoundingMode.HALF_UP;
	MathContext fp_ed_context = new MathContext(7,fp_dfp_rnd);  // fp_ed default
	MathContext fp_dd_context = new MathContext(16,fp_dfp_rnd); // fp_dd default
	MathContext fp_ld_context = new MathContext(34,fp_dfp_rnd); // fp_ld default
														// half even

	MathContext fp_e_rhu_context = new MathContext(32, RoundingMode.HALF_UP);

	MathContext fp_e_rhd_context = new MathContext(32, RoundingMode.HALF_DOWN);

	MathContext fp_e_rd_context = new MathContext(32, RoundingMode.DOWN);

	MathContext fp_e_rpi_context = new MathContext(32, RoundingMode.CEILING);

	MathContext fp_e_rni_context = new MathContext(32, RoundingMode.FLOOR);

	MathContext fp_d_rhu_context = new MathContext(64, RoundingMode.HALF_UP);

	MathContext fp_d_rhd_context = new MathContext(64, RoundingMode.HALF_DOWN);

	MathContext fp_d_rd_context = new MathContext(64, RoundingMode.DOWN);

	MathContext fp_d_rpi_context = new MathContext(64, RoundingMode.CEILING);

	MathContext fp_d_rni_context = new MathContext(64, RoundingMode.FLOOR);

	MathContext fp_x_rhu_context = new MathContext(128, RoundingMode.HALF_UP);

	MathContext fp_x_rhd_context = new MathContext(128, RoundingMode.HALF_DOWN);

	MathContext fp_x_rd_context = new MathContext(128, RoundingMode.DOWN);

	MathContext fp_x_rpi_context = new MathContext(128, RoundingMode.CEILING);

	MathContext fp_x_rni_context = new MathContext(128, RoundingMode.FLOOR);

	double fp_log2 = Math.log(2);

	double fp_log10 = Math.log(10);

	BigDecimal fp_bd = new BigDecimal("0");

	BigDecimal fp_big_dec2 = new BigDecimal("0");

	BigDecimal fp_big_dec3 = new BigDecimal("0");

	byte[] fp_lb_zero = new byte[16];

	byte[] fp_lh_zero = { // RPI 384
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };

	byte[] fp_dd_zero = new byte[8];

	byte[] fp_ed_zero = new byte[4];

	byte[] fp_ld_zero = new byte[16];

	BigInteger fp_big_int1 = new BigInteger("0");

	BigInteger fp_big_int2 = new BigInteger("0");

	BigInteger fp_big_int_one_bits = BigInteger.ONE.shiftLeft(113).subtract(
			BigInteger.ONE);

	BigInteger fp_big_int_man_bits = BigInteger.ONE.shiftLeft(112).subtract(
			BigInteger.ONE);

	int fp_int1 = 0;

	int fp_int_eb_one_bits = 0xffffff;

	int fp_int_eb_man_bits = 0x7fffff;

	int fp_int_eh_man_bits = 0xffffff;

	long fp_long1 = 0;

	long fp_long_db_one_bits = ((long) (1) << 53) - 1;

	long fp_long_db_man_bits = ((long) (1) << 52) - 1;

	long fp_long_dh_man_bits = ((long) (1) << 56) - 1;

	float fp_eb_min = (float) 1.2e-38; // BFP range ref. pop 19-5

	float fp_eb_max = (float) 3.4e+38;

	double fp_db_min = 2.2e-308;

	double fp_db_max = 1.79e+308; // (1.8 too big?)

	double fp_eh_min = 5.41e-79; // HFP range ref. pop 18-4

	double fp_eh_max = 7.2e+75;

	double fp_dh_min = 5.41e-79;

	double fp_dh_max = 7.2e+75;

	BigDecimal fp_lh_min = null;

	BigDecimal fp_lh_max = null;

	BigDecimal fp_lb_min = null; // RPI 367 allow (MIN)

	BigDecimal fp_lb_max = null;
	BigDecimal fp_dd_pos_max = null;
	BigDecimal fp_dd_pos_min = null;
	BigDecimal fp_dd_neg_max = null;
	BigDecimal fp_dd_neg_min = null;

	/*
	 * PC, PR, PT stack for psw and regs
	 */
	int cur_pc_stk = 0;

	int cur_pc_stk_reg = 0;

	byte[] pc_stk_reg_byte = (byte[]) Array.newInstance(byte.class, max_pc_stk
			* reg_len);

	ByteBuffer pc_stk_reg = ByteBuffer.wrap(pc_stk_reg_byte, 0, max_pc_stk
			* reg_len);

	int[] pc_stk_psw_loc = (int[]) Array.newInstance(int.class, max_pc_stk);

	int[] pc_stk_psw_cc = (int[]) Array.newInstance(int.class, max_pc_stk);

	/*
	 * virtual memory with 24 bit and 31 bit fqes all initialized by init_mem()
	 */
	int mem24_start = 0x10000; // RPI 276 start at 32k

	int mem24_line = 0x1000000; // start at 1MB if avail.

	byte[] mem_byte = null; // see init_mem

	ByteBuffer mem = null; // see init_mem

	int dsa24_start = 0;

	int dsa24_end = 0;

	int dsa31_start = 0;

	int dsa31_end = 0;

	int tot_mem = 0;

	int tot_mem_alloc = 0;

	int svc_old_psw = 0x20;

	int svc_new_psw = 0x60;

	/*
	 * psa low memory supported fields
	 */
	int psa_cvt = 0x10; // pointer to os cvt

	int psa_cvt2 = 0x4c; // pointer to os cvt

	/*
	 * z390 communication vector table at x'2000';
	 */
	int zcvt_start = 0x2000; // RPI 286 // cvt start

	int zcvt_user_pgm = zcvt_start + 0x00; // user pgm name

	int zcvt_ipl_pgm = zcvt_start + 0x08; // ipl pgm name

	int zcvt_fqe24 = zcvt_start + 0x10; // amode 24 fqe

	int zcvt_fqe31 = zcvt_start + 0x14; // amode 31 fqe

	int zcvt_exit = zcvt_start + 0x18; // svc 3 exit to last link or term

	int zcvt_tget_ecb = zcvt_start + 0x1c; // ecb for tget reply in non gui
											// mode

	int zcvt_save = zcvt_start + 0x100; // user save

	int zcvt_stimer_save = zcvt_start + 0x200;

	int zcvt_exec_parm = zcvt_start + 0x300; // half word length followed by
												// EBCDIC/ASCII value of PARM(.)

	int zcvt_epie = zcvt_start + 0x400; // espie passed in r1

	int zcvt_esta = zcvt_start + 0x500; // estae passed in r1

	/*
	 * OS/MVS compatible CVT with pointer at x'10'
	 */
	int cvt_start = 0x8000;

	int cvt_date = cvt_start + 0x38; // IPL date

	int cvt_dcb = cvt_start + 0x74; // os flags (x'80' 31 bit, x'13' MVS+) RPI
									// 228

	/*
	 * epie fields
	 */
	int epie_id = zcvt_epie; // C'EPIE'

	int epie_parm = zcvt_epie + 4; // ESPIE PARAM addr

	int epie_psw = zcvt_epie + 8; // PSW int,addr

	int epie_gpr = zcvt_epie + 16; // GPR 64 bit regs R0-R15

	/*
	 * epie fields
	 */
	int esta_id = zcvt_esta; // C'EPIE'

	int esta_parm = zcvt_esta + 4; // ESPIE PARAM addr

	int esta_psw = zcvt_esta + 8; // PSW int,addr

	int esta_gpr = zcvt_esta + 16; // GPR 64 bit regs R0-R15

	/*
	 * opcode lookup tables unique to ez390
	 */
	int[] op_type_offset = new int[256];

	int[] op_type_mask = new int[256];

	int[] opcode2_offset = new int[256];

	int[] opcode2_mask = new int[256];

	/*
	 * end of pz390 global variables
	 */
	public void exec_pz390() {
		/*
		 * execute 390 code at psw_addr in mem[]
		 */
		psw_check = false;
		while (!tz390.z390_abort && !psw_check) { // RPI208 run until check or
													// abort
			if (sz390.stimer_exit_request) { // RPI 323 allow opcode break on
												// first stimer exit opcode
				sz390.start_stimer_exit();
			}
			if (tz390.opt_test) {
				sz390.process_test_cmd();
			}
			if (tz390.opt_regs) {
				sz390.dump_gpr(-1);
			}
			tz390.systerm_ins++;
			opcode1 = mem_byte[psw_loc] & 0xff;
			opcode2 = -1;
			psw_check = true;
			psw_pic = psw_pic_oper;
			if (opcode1 < 0x80) {
				if (opcode1 < 0x40) {
					ins_lt_40();
				} else {
					ins_lt_80();
				}
			} else {
				if (opcode1 < 0xc0) {
					ins_lt_c0();
				} else {
					ins_lt_ff();
				}
			}
			if (psw_check && psw_pic != 0) { // RPI 301
				if (psw_pic == psw_pic_oper && tz390.opt_trace) { // RPI 474
					trace_psw();
				}
				set_psw_check(psw_pic_oper);
			}
			if (ex_mode && (opcode1 != ex_opcode)) {
				set_psw_loc(ex_psw_return);
			}
		}
	}

	private void ins_lt_40() {
		/*
		 * exec opcodes < x'40'
		 */
		switch (opcode1) {
		case 0x01:
			opcode2 = mem_byte[psw_loc + opcode2_offset_e] & 0xff;
			switch (opcode2) {
			case 0x01: // 10 "0101" "PR" "E"
				psw_check = false;
				ins_setup_e();
				pop_pc_stack();
				break;
			case 0x02: // 20 "0102" "UPT" "E"
				ins_setup_e();
				break;
			case 0x04: // 30 "0104" "PTFF" "E" Z9-1
				/*
				 * perform timing facility function r0 = function r1 - parm
				 * block address Notes: 1. Map functions 0x00-0x43 to timing
				 * functions 0x80-C3 in svc 11
				 */
				psw_check = false;
				ins_setup_e();
				reg.put(rf1 + 7, (byte) (reg.get(rf1 + 7) | 0x80 - 0x80));
				sz390.svc(11);
				break;
			case 0x07: // 30 "0107" "SCKPF" "E"
				ins_setup_e();
				break;
			case 0x0B: // 40 "010B" "TAM" "E"
				ins_setup_e();
				break;
			case 0x0C: // 50 "010C" "SAM24" "E"
				psw_check = false;
				ins_setup_e();
				set_psw_amode(psw_amode24_bit);
				break;
			case 0x0D: // 60 "010D" "SAM31" "E"
				psw_check = false;
				ins_setup_e();
				set_psw_amode(psw_amode31_bit);
				break;
			case 0x0E: // 70 "010E" "SAM64" "E"
				ins_setup_e();
				break;
			case 0xFF: // 80 "01FF" "TRAP2" "E"
				ins_setup_e();
				break;
			}
			break;
		case 0x04: // 90 "04" "SPM" "RR"
			psw_check = false;
			ins_setup_rr();
			rv1 = (reg.getInt(rf1 + 4) >> 24) & 0x3f;
			psw_pgm_mask = rv1 & 0xf;
			psw_cc = psw_cc_mask[rv1 >> 4];
			break;
		case 0x05: // 100 "05" "BALR" "RR"
			psw_check = false;
			ins_setup_rr();
			if (rf2 != 0) {
				rv2 = reg.getInt(rf2 + 4); // RPI65
			}
			reg.putInt(rf1 + 4, psw_loc | psw_amode_bit);
			if (rf2 != 0) {
				set_psw_loc(rv2); // RPI65
			}
			break;
		case 0x06: // 110 "06" "BCTR" "RR"
			psw_check = false;
			ins_setup_rr();
			rv1 = reg.getInt(rf1 + 4) - 1;
			reg.putInt(rf1 + 4, rv1);
			if (rf2 != 0 && rv1 != 0) {
				set_psw_loc(reg.getInt(rf2 + 4));
			}
			break;
		case 0x07: // 120 "07" "BCR" "RR"
			psw_check = false;
			ins_setup_rr();
			if ((psw_cc & mf1) > 0 && (rf1 != 0)) {
				set_psw_loc(reg.getInt(rf2 + 4));
			}

			break;
		case 0x0A: // 290 "0A" "SVC" "I"
			psw_check = false;
			ins_setup_i();
			if (mem.get(svc_new_psw) == 0) { // native svc call
				sz390.svc(if1);
			} else { // user svc exit
				mem.putShort(svc_old_psw + 2, (short) if1);
				mem.putInt(svc_old_psw + 4, psw_loc);
				set_psw_loc(mem.getInt(svc_new_psw + 4));
			}
			break;
		case 0x0B: // 300 "0B" "BSM" "RR"
			psw_check = false;
			ins_setup_rr();
			if (rf1 != 0) {
				if (psw_amode == psw_amode31) {
					reg.putInt(rf1 + 4, reg.getInt(rf1 + 4) | psw_amode31_bit);
				} else {
					reg.putInt(rf1 + 4, reg.getInt(rf1 + 4) & psw_amode31);
				}
			}
			if (rf2 != 0) {
				rv2 = reg.getInt(rf2 + 4);
				if ((rv2 & psw_amode31_bit) != 0) {
					set_psw_amode(psw_amode31_bit);
				} else {
					set_psw_amode(psw_amode24_bit);
				}
				set_psw_loc(rv2);
			}
			break;
		case 0x0C: // 310 "0C" "BASSM" "RR"
			psw_check = false;
			ins_setup_rr();
			if (rf2 != 0) {
				rv2 = reg.getInt(rf2 + 4); // RPI65
			}
			reg.putInt(rf1 + 4, psw_loc | psw_amode_bit);
			if (rf2 != 0) {
				if ((rv2 & psw_amode31_bit) != 0) {
					set_psw_amode(psw_amode31_bit);
				} else {
					set_psw_amode(psw_amode24_bit);
				}
				set_psw_loc(rv2);
			}
			break;
		case 0x0D: // 320 "0D" "BASR" "RR"
			psw_check = false;
			ins_setup_rr();
			if (rf2 != 0) {
				rv2 = reg.getInt(rf2 + 4); // RPI65
			}
			reg.putInt(rf1 + 4, psw_loc | psw_amode_bit);
			if (rf2 != 0) {
				set_psw_loc(rv2); // RPI65
			}
			break;
		case 0x0E: // 330 "0E" "MVCL" "RR"
			psw_check = false;
			ins_setup_rr();
			bd1_loc = reg.getInt(rf1 + 4) & psw_amode;
			bd2_loc = reg.getInt(rf2 + 4) & psw_amode;
			rflen1 = reg.getInt(rf1 + 12) & psw_amode24;
			rflen2 = reg.getInt(rf2 + 12) & psw_amode24;
			data_len = rflen1;
			pad_len = 0;
			psw_cc = psw_cc0;
			if (rflen1 < rflen2) {
				psw_cc = psw_cc1;
			} else if (rflen1 > rflen2) {
				psw_cc = psw_cc2;
				data_len = rflen2;
				pad_len = rflen1 - rflen2;
				fill_char = reg.get(rf2 + 12);
			}
			if (bd1_loc + data_len + pad_len > tot_mem) {
				set_psw_check(psw_pic_addr); // RPI 397
				break;
			}
			if (data_len > 0 && bd1_loc > bd2_loc
					&& bd1_loc < bd2_loc + data_len) {
				psw_cc = psw_cc3;
				break;
			}
			if (data_len > 0) { // RPI 386
				System
						.arraycopy(mem_byte, bd2_loc, mem_byte, bd1_loc,
								data_len); // RPI 411
				bd1_loc = bd1_loc + data_len;
				bd2_loc = bd2_loc + data_len;
			}
			if (pad_len > 0) {
				Arrays.fill(mem_byte, bd1_loc, bd1_loc + pad_len, fill_char);
				bd1_loc = bd1_loc + pad_len;
			}
			reg.putInt(rf1 + 4, (reg.getInt(rf1 + 4) & psw_amode_high_bits)
					| bd1_loc);
			reg.putInt(rf1 + 12, reg.getInt(rf1 + 12) & psw_amode24_high_bits);
			reg.putInt(rf2 + 4, (reg.getInt(rf2 + 4) & psw_amode_high_bits)
					| bd2_loc);
			reg.putInt(rf2 + 12, (reg.getInt(rf2 + 12) & psw_amode24_high_bits)
					| (rflen2 - data_len));
			break;
		case 0x0F: // 340 "0F" "CLCL" "RR"
			psw_check = false;
			ins_setup_rr();
			fill_char = reg.get(rf2 + 12);
			bd1_loc = reg.getInt(rf1 + 4) & psw_amode;
			bd2_loc = reg.getInt(rf2 + 4) & psw_amode;
			bd1_start = bd1_loc;
			bd2_start = bd2_loc;
			rflen1 = reg.getInt(rf1 + 12) & psw_amode24;
			rflen2 = reg.getInt(rf2 + 12) & psw_amode24;
			data_len = rflen1;
			pad_len = 0;
			psw_cc = psw_cc0;
			if (rflen1 < rflen2) {
				data_len = rflen1;
				pad_len = rflen2 - rflen1;
			} else if (rflen1 > rflen2) {
				data_len = rflen2;
				pad_len = rflen1 - rflen2;
			}
			if (bd1_loc + data_len + pad_len > tot_mem) {
				set_psw_check(psw_pic_addr); // RPI 397
				break;
			}
			bd1_end = bd1_loc + data_len;
			while (bd1_loc < bd1_end) {
				if (mem_byte[bd1_loc] != mem_byte[bd2_loc]) {
					if ((mem_byte[bd1_loc] & 0xff) > (mem_byte[bd2_loc] & 0xff)) {
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
			if (psw_cc == psw_cc_equal & pad_len > 0) {
				if (rflen1 > rflen2) {
					bd1_end = bd1_loc + pad_len;
					while (bd1_loc < bd1_end) {
						if (mem_byte[bd1_loc] != fill_char) {
							if ((mem_byte[bd1_loc] & 0xff) > (fill_char & 0xff)) {
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
					while (bd2_loc < bd2_end) {
						if (mem_byte[bd2_loc] != fill_char) {
							if ((mem_byte[bd2_loc] & 0xff) > (fill_char & 0xff)) {
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
			reg.putInt(rf1 + 4, (reg.getInt(rf1 + 4) & psw_amode_high_bits)
					| bd1_loc);
			reg.putInt(rf1 + 12, (reg.getInt(rf1 + 12) & psw_amode24_high_bits)
					| (rflen1 - (bd1_loc - bd1_start)));
			reg.putInt(rf2 + 4, (reg.getInt(rf2 + 4) & psw_amode_high_bits)
					| bd2_loc);
			reg.putInt(rf2 + 12, (reg.getInt(rf2 + 12) & psw_amode24_high_bits)
					| (rflen2 - (bd2_loc - bd2_start)));
			break;
		case 0x10: // 350 "10" "LPR" "RR"
			psw_check = false;
			ins_setup_rr();
			rv1 = reg.getInt(rf2 + 4);
			if (rv1 < 0) {
				if (rv1 == int_high_bit) {
					psw_cc = psw_cc3;
					break;
				}
				rv1 = -rv1;
			}
			reg.putInt(rf1 + 4, rv1);
			psw_cc = get_int_comp_cc(rv1, 0);
			break;
		case 0x11: // 360 "11" "LNR" "RR"
			psw_check = false;
			ins_setup_rr();
			rv1 = reg.getInt(rf2 + 4);
			if (rv1 > 0) {
				rv1 = -rv1;
			}
			reg.putInt(rf1 + 4, rv1);
			psw_cc = get_int_comp_cc(rv1, 0);
			break;
		case 0x12: // 370 "12" "LTR" "RR"
			psw_check = false;
			ins_setup_rr();
			rv1 = reg.getInt(rf2 + 4);
			reg.putInt(rf1 + 4, rv1);
			psw_cc = get_int_comp_cc(rv1, 0);
			break;
		case 0x13: // 380 "13" "LCR" "RR"
			psw_check = false;
			ins_setup_rr();
			rv1 = reg.getInt(rf2 + 4);
			if (rv1 == int_high_bit) {
				psw_cc = psw_cc3;
				break;
			}
			rv1 = -rv1;
			reg.putInt(rf1 + 4, rv1);
			psw_cc = get_int_comp_cc(rv1, 0);
			break;
		case 0x14: // 390 "14" "NR" "RR"
			psw_check = false;
			ins_setup_rr();
			rv1 = reg.getInt(rf1 + 4) & reg.getInt(rf2 + 4);
			reg.putInt(rf1 + 4, rv1);
			if (rv1 == 0) {
				psw_cc = psw_cc0;
			} else {
				psw_cc = psw_cc1;
			}
			break;
		case 0x15: // 400 "15" "CLR" "RR"
			psw_check = false;
			ins_setup_rr();
			psw_cc = get_int_log_comp_cc(reg.getInt(rf1 + 4), reg
					.getInt(rf2 + 4));
			break;
		case 0x16: // 410 "16" "OR" "RR"
			psw_check = false;
			ins_setup_rr();
			rv1 = reg.getInt(rf1 + 4) | reg.getInt(rf2 + 4);
			reg.putInt(rf1 + 4, rv1);
			if (rv1 == 0) {
				psw_cc = psw_cc0;
			} else {
				psw_cc = psw_cc1;
			}
			break;
		case 0x17: // 420 "17" "XR" "RR"
			psw_check = false;
			ins_setup_rr();
			rv1 = reg.getInt(rf1 + 4) ^ reg.getInt(rf2 + 4);
			reg.putInt(rf1 + 4, rv1);
			if (rv1 == 0) {
				psw_cc = psw_cc0;
			} else {
				psw_cc = psw_cc1;
			}
			break;
		case 0x18: // 430 "18" "LR" "RR"
			psw_check = false;
			ins_setup_rr();
			reg.putInt(rf1 + 4, reg.getInt(rf2 + 4));
			break;
		case 0x19: // 440 "19" "CR" "RR"
			psw_check = false;
			ins_setup_rr();
			psw_cc = get_int_comp_cc(reg.getInt(rf1 + 4), reg.getInt(rf2 + 4));
			break;
		case 0x1A: // 450 "1A" "AR" "RR"
			psw_check = false;
			ins_setup_rr();
			rv1 = reg.getInt(rf1 + 4);
			rv2 = reg.getInt(rf2 + 4);
			rv3 = rv1 + rv2;
			reg.putInt(rf1 + 4, rv3);
			psw_cc = get_int_add_cc();
			break;
		case 0x1B: // 460 "1B" "SR" "RR"
			psw_check = false;
			ins_setup_rr();
			rv1 = reg.getInt(rf1 + 4);
			rv2 = reg.getInt(rf2 + 4);
			rv3 = rv1 - rv2;
			reg.putInt(rf1 + 4, rv3);
			psw_cc = get_int_sub_cc();
			break;
		case 0x1C: // 470 "1C" "MR" "RR"
			psw_check = false;
			ins_setup_rr();
			rlv1 = (long) reg.getInt(rf1 + 12) * (long) reg.getInt(rf2 + 4); // RPI
																				// 272
			reg.putInt(rf1 + 4, (int) (rlv1 >> 32));
			reg.putInt(rf1 + 12, (int) (rlv1 & long_low32_bits));
			break;
		case 0x1D: // 480 "1D" "DR" "RR"
			psw_check = false;
			ins_setup_rr();
			rlv1 = (long) (reg.getInt(rf1 + 4)) << 32
					| ((long) reg.getInt(rf1 + 12) & long_low32_bits); // RPI
																		// 398
			rv2 = reg.getInt(rf2 + 4);
			if (rv2 == 0) {
				set_psw_check(psw_pic_fx_div);
				break;
			}
			rv1 = (int) (rlv1 / rv2); // RPI 398
			rv2 = (int) rlv1 - rv1 * rv2;
			reg.putInt(rf1 + 4, rv2);
			reg.putInt(rf1 + 12, rv1);
			break;
		case 0x1E: // 490 "1E" "ALR" "RR"
			psw_check = false;
			ins_setup_rr();
			rvw = reg.getInt(rf1 + 4);
			rv2 = reg.getInt(rf2 + 4);
			rv1 = rvw + rv2;
			reg.putInt(rf1 + 4, rv1);
			psw_cc = get_int_log_add_cc();
			break;
		case 0x1F: // 500 "1F" "SLR" "RR"
			psw_check = false;
			ins_setup_rr();
			rvw = reg.getInt(rf1 + 4);
			rv2 = reg.getInt(rf2 + 4);
			rv1 = rvw - rv2;
			reg.putInt(rf1 + 4, rv1);
			psw_cc = get_int_log_sub_cc();
			break;
		case 0x20: // 510 "20" "LPDR" "RR"
			psw_check = false;
			ins_setup_rr();
			fp_rdv1 = Math.abs(fp_get_db_from_dh(fp_reg, rf2));
			fp_put_db(rf1, tz390.fp_dh_type, fp_rdv1);
			psw_cc = fp_get_dh_comp_cc(fp_rdv1, 0);
			break;
		case 0x21: // 520 "21" "LNDR" "RR"
			psw_check = false;
			ins_setup_rr();
			fp_rdv1 = Math.abs(fp_get_db_from_dh(fp_reg, rf2));
			if (fp_rdv1 != 0) {
				fp_rdv1 = -fp_rdv1;
			}
			fp_put_db(rf1, tz390.fp_dh_type, fp_rdv1);
			psw_cc = fp_get_dh_comp_cc(fp_rdv1, 0);
			break;
		case 0x22: // 530 "22" "LTDR" "RR"
			psw_check = false;
			ins_setup_rr();
			fp_rdv1 = fp_get_db_from_dh(fp_reg, rf2);
			fp_put_db(rf1, tz390.fp_dh_type, fp_rdv1);
			psw_cc = fp_get_dh_comp_cc(fp_rdv1, 0);
			break;
		case 0x23: // 540 "23" "LCDR" "RR"
			psw_check = false;
			ins_setup_rr();
			fp_rdv1 = fp_get_db_from_dh(fp_reg, rf2);
			if (fp_rdv1 != 0) {
				fp_rdv1 = -fp_rdv1;
			}
			fp_put_db(rf1, tz390.fp_dh_type, fp_rdv1);
			psw_cc = fp_get_dh_comp_cc(fp_rdv1, 0);
			break;
		case 0x24: // 550 "24" "HDR" "RR"
			psw_check = false;
			ins_setup_rr();
			fp_rdv1 = fp_get_db_from_dh(fp_reg, rf2) / 2;
			fp_put_db(rf1, tz390.fp_dh_type, fp_rdv1);
			break;
		case 0x25: // 560 "25" "LDXR" "RR"
			psw_check = false;
			ins_setup_rr();
			fp_load_reg(rf1, tz390.fp_dh_type, fp_reg, rf2, tz390.fp_lh_type);
			break;
		case 0x26: // 580 "26" "MXR" "RR"
			psw_check = false;
			ins_setup_rr();
			fp_rbdv1 = fp_get_bd_from_lh(fp_reg, rf1).multiply(
					fp_get_bd_from_lh(fp_reg, rf2), fp_bd_context).round(
					fp_x_context);
			fp_put_bd(rf1, tz390.fp_lh_type, fp_rbdv1);
			check_lh_mpy();
			break;
		case 0x27: // 590 "27" "MXDR" "RR"
			psw_check = false;
			ins_setup_rr();
			fp_rbdv1 = fp_get_bd_from_dh(fp_reg, rf1).multiply(
					fp_get_bd_from_dh(fp_reg, rf2), fp_db_context).round(
					fp_x_context);
			fp_put_bd(rf1, tz390.fp_lh_type, fp_rbdv1);
			check_lh_mpy();
			break;
		case 0x28: // 600 "28" "LDR" "RR"
			psw_check = false;
			ins_setup_rr();
			fp_load_reg(rf1, tz390.fp_dh_type, fp_reg, rf2, tz390.fp_dh_type);
			break;
		case 0x29: // 610 "29" "CDR" "RR"
			psw_check = false;
			ins_setup_rr();
			psw_cc = fp_get_dh_comp_cc(fp_get_db_from_dh(fp_reg, rf1),
					fp_get_db_from_dh(fp_reg, rf2));
			break;
		case 0x2A: // 620 "2A" "ADR" "RR"
			psw_check = false;
			ins_setup_rr();
			fp_rdv1 = fp_get_db_from_dh(fp_reg, rf1)
					+ fp_get_db_from_dh(fp_reg, rf2);
			fp_put_db(rf1, tz390.fp_dh_type, fp_rdv1);
			psw_cc = fp_get_dh_add_sub_cc();
			break;
		case 0x2B: // 630 "2B" "SDR" "RR"
			psw_check = false;
			ins_setup_rr();
			fp_rdv1 = fp_get_db_from_dh(fp_reg, rf1)
					- fp_get_db_from_dh(fp_reg, rf2);
			fp_put_db(rf1, tz390.fp_dh_type, fp_rdv1);
			psw_cc = fp_get_dh_add_sub_cc();
			break;
		case 0x2C: // 640 "2C" "MDR" "RR"
			psw_check = false;
			ins_setup_rr();
			fp_rdv1 = fp_get_db_from_dh(fp_reg, rf1)
					* fp_get_db_from_dh(fp_reg, rf2);
			fp_put_db(rf1, tz390.fp_dh_type, fp_rdv1);
			check_dh_mpy();
			break;
		case 0x2D: // 650 "2D" "DDR" "RR"
			psw_check = false;
			ins_setup_rr();
			fp_rdv2 = fp_get_db_from_dh(fp_reg, rf2);
			fp_rdv1 = fp_get_db_from_dh(fp_reg, rf1) / fp_rdv2;
			fp_put_db(rf1, tz390.fp_dh_type, fp_rdv1);
			check_dh_div();
			break;
		case 0x2E: // 660 "2E" "AWR" "RR"
			psw_check = false;
			ins_setup_rr();
			fp_rdv1 = fp_get_db_from_dh(fp_reg, rf1)
					+ fp_get_db_from_dh(fp_reg, rf2);
			fp_put_db(rf1, tz390.fp_dh_type, fp_rdv1);
			psw_cc = fp_get_dh_add_sub_cc();
			break;
		case 0x2F: // 670 "2F" "SWR" "RR"
			psw_check = false;
			ins_setup_rr();
			fp_rdv1 = fp_get_db_from_dh(fp_reg, rf1)
					- fp_get_db_from_dh(fp_reg, rf2);
			fp_put_db(rf1, tz390.fp_dh_type, fp_rdv1);
			psw_cc = fp_get_dh_add_sub_cc();
			break;
		case 0x30: // 680 "30" "LPER" "RR"
			psw_check = false;
			ins_setup_rr();
			fp_rdv1 = Math.abs(fp_get_db_from_eh(fp_reg, rf2));
			fp_put_db(rf1, tz390.fp_eh_type, fp_rdv1);
			psw_cc = fp_get_eh_comp_cc(fp_rdv1, 0);
			break;
		case 0x31: // 690 "31" "LNER" "RR"
			psw_check = false;
			ins_setup_rr();
			fp_rdv1 = -Math.abs(fp_get_db_from_eh(fp_reg, rf2));
			fp_put_db(rf1, tz390.fp_eh_type, fp_rdv1);
			psw_cc = fp_get_eh_comp_cc(fp_rdv1, 0);
			break;
		case 0x32: // 700 "32" "LTER" "RR"
			psw_check = false;
			ins_setup_rr();
			fp_rdv1 = fp_get_db_from_eh(fp_reg, rf2);
			fp_put_db(rf1, tz390.fp_eh_type, fp_rdv1);
			psw_cc = fp_get_eh_comp_cc(fp_rdv1, 0);
			break;
		case 0x33: // 710 "33" "LCER" "RR"
			psw_check = false;
			ins_setup_rr();
			fp_rdv1 = fp_get_db_from_eh(fp_reg, rf2);
			if (fp_rdv1 != 0) {
				fp_rdv1 = -fp_rdv1;
			}
			fp_put_db(rf1, tz390.fp_eh_type, fp_rdv1);
			psw_cc = fp_get_eh_comp_cc(fp_rdv1, 0);
			break;
		case 0x34: // 720 "34" "HER" "RR"
			psw_check = false;
			ins_setup_rr();
			fp_rdv1 = fp_get_db_from_eh(fp_reg, rf2) / 2;
			fp_put_db(rf1, tz390.fp_eh_type, fp_rdv1);
			break;
		case 0x35: // 730 "35" "LEDR" "RR"
			psw_check = false;
			ins_setup_rr();
			fp_load_reg(rf1, tz390.fp_eh_type, fp_reg, rf2, tz390.fp_dh_type);
			break;
		case 0x36: // 750 "36" "AXR" "RR"
			psw_check = false;
			ins_setup_rr();
			fp_rbdv1 = fp_get_bd_from_lh(fp_reg, rf1).add(
					fp_get_bd_from_lh(fp_reg, rf2), fp_bd_context).round(
					fp_x_context);
			fp_put_bd(rf1, tz390.fp_lh_type, fp_rbdv1);
			psw_cc = fp_get_lh_add_sub_cc();
			break;
		case 0x37: // 760 "37" "SXR" "RR"
			psw_check = false;
			ins_setup_rr();
			fp_rbdv1 = fp_get_bd_from_lh(fp_reg, rf1).subtract(
					fp_get_bd_from_lh(fp_reg, rf2), fp_bd_context).round(
					fp_x_context);
			fp_put_bd(rf1, tz390.fp_lh_type, fp_rbdv1);
			psw_cc = fp_get_lh_add_sub_cc();
			break;
		case 0x38: // 770 "38" "LER" "RR"
			psw_check = false;
			ins_setup_rr();
			fp_load_reg(rf1, tz390.fp_eh_type, fp_reg, rf2, tz390.fp_eh_type);
			break;
		case 0x39: // 780 "39" "CER" "RR"
			psw_check = false;
			ins_setup_rr();
			psw_cc = fp_get_eh_comp_cc(fp_get_db_from_eh(fp_reg, rf1),
					fp_get_db_from_eh(fp_reg, rf2));
			break;
		case 0x3A: // 790 "3A" "AER" "RR"
			psw_check = false;
			ins_setup_rr();
			fp_rdv1 = fp_get_db_from_eh(fp_reg, rf1)
					+ fp_get_db_from_eh(fp_reg, rf2);
			fp_put_db(rf1, tz390.fp_eh_type, fp_rdv1);
			psw_cc = fp_get_eh_add_sub_cc();
			break;
		case 0x3B: // 800 "3B" "SER" "RR"
			psw_check = false;
			ins_setup_rr();
			fp_rdv1 = fp_get_db_from_eh(fp_reg, rf1)
					- fp_get_db_from_eh(fp_reg, rf2);
			fp_put_db(rf1, tz390.fp_eh_type, fp_rdv1);
			psw_cc = fp_get_eh_add_sub_cc();
			break;
		case 0x3C: // 810 "3C" "MDER" "RR"
			psw_check = false;
			ins_setup_rr();
			fp_rdv1 = fp_get_db_from_eh(fp_reg, rf1)
					* fp_get_db_from_eh(fp_reg, rf2);
			fp_put_db(rf1, tz390.fp_dh_type, fp_rdv1);
			check_dh_mpy();
			break;
		case 0x3D: // 830 "3D" "DER" "RR"
			psw_check = false;
			ins_setup_rr();
			fp_rdv2 = fp_get_db_from_eh(fp_reg, rf2);
			fp_rdv1 = fp_get_db_from_eh(fp_reg, rf1) / fp_rdv2;
			fp_put_db(rf1, tz390.fp_eh_type, fp_rdv1);
			check_eh_div();
			break;
		case 0x3E: // 840 "3E" "AUR" "RR"
			psw_check = false;
			ins_setup_rr();
			fp_rdv1 = fp_get_db_from_eh(fp_reg, rf1)
					+ fp_get_db_from_eh(fp_reg, rf2);
			fp_put_db(rf1, tz390.fp_eh_type, fp_rdv1);
			psw_cc = fp_get_eh_add_sub_cc();
			break;
		case 0x3F: // 850 "3F" "SUR" "RR"
			psw_check = false;
			ins_setup_rr();
			fp_rdv1 = fp_get_db_from_eh(fp_reg, rf1)
					- fp_get_db_from_eh(fp_reg, rf2);
			fp_put_db(rf1, tz390.fp_eh_type, fp_rdv1);
			psw_cc = fp_get_eh_add_sub_cc();
			break;
		}
	}

	private void ins_lt_80() {
		/*
		 * excute instr < x'80'
		 */
		switch (opcode1) {
		case 0x40: // 860 "40" "STH" "RX"
			psw_check = false;
			ins_setup_rx();
			mem.putShort(xbd2_loc, (short) reg.getInt(rf1 + 4));
			break;
		case 0x41: // 870 "41" "LA" "RX"
			psw_check = false;
			ins_setup_rx();
			reg.putInt(rf1 + 4, xbd2_loc & psw_amode);
			break;
		case 0x42: // 880 "42" "STC" "RX"
			psw_check = false;
			ins_setup_rx();
			mem_byte[xbd2_loc] = reg.get(rf1 + 4 + 3);
			break;
		case 0x43: // 890 "43" "IC" "RX"
			psw_check = false;
			ins_setup_rx();
			reg.put(rf1 + 7, mem_byte[xbd2_loc]);
			break;
		case 0x44: // 900 "44" "EX" "RX"
			psw_check = false;
			ins_setup_rx();
			if (!ex_mode) {
				ex_psw_return = psw_loc;
				set_psw_loc(xbd2_loc);
				ex_mode = true;
				ex_mod_byte = mem_byte[psw_loc + 1];
				if (rf1 != 0) {
					mem_byte[psw_loc + 1] = (byte) (ex_mod_byte | reg
							.get(rf1 + 7));
				}
			} else {
				set_psw_check(psw_pic_exec);
			}
			break;
		case 0x45: // 910 "45" "BAL" "RX"
			psw_check = false;
			ins_setup_rx();
			if (ex_mode) {
				reg.putInt(rf1 + 4, ex_psw_return | psw_amode_bit);
			} else {
				reg.putInt(rf1 + 4, psw_loc | psw_amode_bit);
			}
			set_psw_loc(xbd2_loc);
			break;
		case 0x46: // 920 "46" "BCT" "RX"
			psw_check = false;
			ins_setup_rx();
			rv1 = reg.getInt(rf1 + 4) - 1;
			reg.putInt(rf1 + 4, rv1);
			if (rv1 != 0) {
				set_psw_loc(xbd2_loc);
			}
			break;
		case 0x47: // 930 "47" "BC" "RX"
			psw_check = false;
			ins_setup_rx();
			if ((psw_cc & mf1) > 0) {
				set_psw_loc(xbd2_loc);
			}
			break;
		case 0x48: // 1100 "48" "LH" "RX"
			psw_check = false;
			ins_setup_rx();
			reg.putInt(rf1 + 4, mem.getShort(xbd2_loc));
			break;
		case 0x49: // 1110 "49" "CH" "RX"
			psw_check = false;
			ins_setup_rx();
			psw_cc = get_int_comp_cc(reg.getInt(rf1 + 4), mem
					.getShort(xbd2_loc));
			break;
		case 0x4A: // 1120 "4A" "AH" "RX"
			psw_check = false;
			ins_setup_rx();
			rv1 = reg.getInt(rf1 + 4);
			rv2 = mem.getShort(xbd2_loc);
			rv3 = rv1 + rv2;
			reg.putInt(rf1 + 4, rv3);
			psw_cc = get_int_add_cc();
			break;
		case 0x4B: // 1130 "4B" "SH" "RX"
			psw_check = false;
			ins_setup_rx();
			rv1 = reg.getInt(rf1 + 4);
			rv2 = mem.getShort(xbd2_loc);
			rv3 = rv1 - rv2;
			reg.putInt(rf1 + 4, rv3);
			psw_cc = get_int_sub_cc();
			break;
		case 0x4C: // 1140 "4C" "MH" "RX"
			psw_check = false;
			ins_setup_rx();
			rv1 = reg.getInt(rf1 + 4) * mem.getShort(xbd2_loc);
			reg.putInt(rf1 + 4, rv1);
			break;
		case 0x4D: // 1150 "4D" "BAS" "RX"
			psw_check = false;
			ins_setup_rx();
			if (ex_mode) {
				reg.putInt(rf1 + 4, ex_psw_return | psw_amode_bit);
			} else {
				reg.putInt(rf1 + 4, psw_loc | psw_amode_bit);
			}
			set_psw_loc(xbd2_loc);
			break;
		case 0x4E: // 1160 "4E" "CVD" "RX"
			psw_check = false;
			ins_setup_rx();
			pdf_is_big = false;
			pdf_long = reg.getInt(rf1 + 4);
			put_pd(mem_byte, xbd2_loc, 8);
			break;
		case 0x4F: // 1170 "4F" "CVB" "RX"
			psw_check = false;
			ins_setup_rx();
			if (get_pd(xbd2_loc, 8)) { // RPI 305
				if (pdf_is_big) { // RPI 389
					set_psw_check(psw_pic_fx_div);
				} else {
					if (pdf_long <= max_pos_int && pdf_long >= min_neg_int) {
						reg.putInt(rf1 + 4, (int) pdf_long);
					} else {
						set_psw_check(psw_pic_fx_div);
					}
				}
			}
			break;
		case 0x50: // 1180 "50" "ST" "RX"
			ins_setup_rx();
			psw_check = false;
			mem.putInt(xbd2_loc, reg.getInt(rf1 + 4));
			break;
		case 0x51: // 1190 "51" "LAE" "RX"
			ins_setup_rx();
			break;
		case 0x54: // 1200 "54" "N" "RX"
			psw_check = false;
			ins_setup_rx();
			rv1 = reg.getInt(rf1 + 4) & mem.getInt(xbd2_loc);
			reg.putInt(rf1 + 4, rv1);
			if (rv1 == 0) {
				psw_cc = psw_cc0;
			} else {
				psw_cc = psw_cc1;
			}
			break;
		case 0x55: // 1210 "55" "CL" "RX"
			psw_check = false;
			ins_setup_rx();
			psw_cc = get_int_log_comp_cc(reg.getInt(rf1 + 4), mem
					.getInt(xbd2_loc));
			break;
		case 0x56: // 1220 "56" "O" "RX"
			psw_check = false;
			ins_setup_rx();
			rv1 = reg.getInt(rf1 + 4) | mem.getInt(xbd2_loc);
			reg.putInt(rf1 + 4, rv1);
			if (rv1 == 0) {
				psw_cc = psw_cc0;
			} else {
				psw_cc = psw_cc1;
			}
			break;
		case 0x57: // 1230 "57" "X" "RX"
			psw_check = false;
			ins_setup_rx();
			rv1 = reg.getInt(rf1 + 4) ^ mem.getInt(xbd2_loc);
			reg.putInt(rf1 + 4, rv1);
			if (rv1 == 0) {
				psw_cc = psw_cc0;
			} else {
				psw_cc = psw_cc1;
			}
			break;
		case 0x58: // 1240 "58" "L" "RX"
			psw_check = false;
			ins_setup_rx();
			reg.putInt(rf1 + 4, mem.getInt(xbd2_loc));
			break;
		case 0x59: // 1250 "59" "C" "RX"
			psw_check = false;
			ins_setup_rx();
			psw_cc = get_int_comp_cc(reg.getInt(rf1 + 4), mem.getInt(xbd2_loc));
			break;
		case 0x5A: // 1260 "5A" "A" "RX"
			psw_check = false;
			ins_setup_rx();
			rv1 = reg.getInt(rf1 + 4);
			rv2 = mem.getInt(xbd2_loc);
			rv3 = rv1 + rv2;
			reg.putInt(rf1 + 4, rv3);
			psw_cc = get_int_add_cc();
			break;
		case 0x5B: // 1270 "5B" "S" "RX"
			psw_check = false;
			ins_setup_rx();
			rv1 = reg.getInt(rf1 + 4);
			rv2 = mem.getInt(xbd2_loc);
			rv3 = rv1 - rv2;
			reg.putInt(rf1 + 4, rv3);
			psw_cc = get_int_sub_cc();
			break;
		case 0x5C: // 1280 "5C" "M" "RX"
			psw_check = false;
			ins_setup_rx();
			rlv1 = (long) reg.getInt(rf1 + 8 + 4) * (long) mem.getInt(xbd2_loc);
			work_reg.putLong(0, rlv1);
			reg.putInt(rf1 + 4, work_reg.getInt(0));
			reg.putInt(rf1 + 8 + 4, work_reg.getInt(4));
			break;
		case 0x5D: // 1290 "5D" "D" "RX"
			psw_check = false;
			ins_setup_rx();
			rlv1 = (long) (reg.getInt(rf1 + 4)) << 32
					| ((long) reg.getInt(rf1 + 12) & long_low32_bits); // RPI
																		// 398
			rv2 = mem.getInt(xbd2_loc);
			if (rv2 == 0) {
				set_psw_check(psw_pic_fx_div);
				break;
			}
			rv1 = (int) (rlv1 / rv2); // RPI 398
			rv2 = (int) rlv1 - rv1 * rv2;
			reg.putInt(rf1 + 4, rv2);
			reg.putInt(rf1 + 12, rv1);
			break;
		case 0x5E: // 1300 "5E" "AL" "RX"
			psw_check = false;
			ins_setup_rx();
			rvw = reg.getInt(rf1 + 4);
			rv2 = mem.getInt(xbd2_loc);
			rv1 = rvw + rv2;
			reg.putInt(rf1 + 4, rv1);
			psw_cc = get_int_log_add_cc();
			break;
		case 0x5F: // 1310 "5F" "SL" "RX"
			psw_check = false;
			ins_setup_rx();
			rvw = reg.getInt(rf1 + 4);
			rv2 = mem.getInt(xbd2_loc);
			rv1 = rvw - rv2;
			reg.putInt(rf1 + 4, rv1);
			psw_cc = get_int_log_sub_cc();
			break;
		case 0x60: // 1320 "60" "STD" "RX"
			psw_check = false;
			ins_setup_rx();
			if (fp_reg_ctl[mf1] != fp_ctl_ld) {
				fp_store_reg(fp_reg, rf1);
			}
			mem.putLong(xbd2_loc, fp_reg.getLong(rf1));
			break;
		case 0x67: // 1330 "67" "MXD" "RX"
			psw_check = false;
			ins_setup_rx();
			fp_rbdv1 = fp_get_bd_from_dh(fp_reg, rf1).multiply(
					fp_get_bd_from_dh(mem, xbd2_loc), fp_db_context).round(
					fp_x_context);
			fp_put_bd(rf1, tz390.fp_lh_type, fp_rbdv1);
			check_lh_mpy();
			break;
		case 0x68: // 1340 "68" "LD" "RX"
			psw_check = false;
			ins_setup_rx();
			fp_load_reg(rf1, tz390.fp_dh_type, mem, xbd2_loc, tz390.fp_dh_type);
			break;
		case 0x69: // 1350 "69" "CD" "RX"
			psw_check = false;
			ins_setup_rx();
			psw_cc = fp_get_dh_comp_cc(fp_get_db_from_dh(fp_reg, rf1),
					fp_get_db_from_dh(mem, xbd2_loc));
			break;
		case 0x6A: // 1360 "6A" "AD" "RX"
			psw_check = false;
			ins_setup_rx();
			fp_rdv1 = fp_get_db_from_dh(fp_reg, rf1)
					+ fp_get_db_from_dh(mem, xbd2_loc);
			fp_put_db(rf1, tz390.fp_dh_type, fp_rdv1);
			psw_cc = fp_get_dh_add_sub_cc();
			break;
		case 0x6B: // 1370 "6B" "SD" "RX"
			psw_check = false;
			ins_setup_rx();
			fp_rdv1 = fp_get_db_from_dh(fp_reg, rf1)
					- fp_get_db_from_dh(mem, xbd2_loc);
			fp_put_db(rf1, tz390.fp_dh_type, fp_rdv1);
			psw_cc = fp_get_dh_add_sub_cc();
			break;
		case 0x6C: // 1380 "6C" "MD" "RX"
			psw_check = false;
			ins_setup_rx();
			fp_rdv1 = fp_get_db_from_dh(fp_reg, rf1)
					* fp_get_db_from_dh(mem, xbd2_loc);
			fp_put_db(rf1, tz390.fp_dh_type, fp_rdv1);
			check_dh_mpy();
			break;
		case 0x6D: // 1390 "6D" "DD" "RX"
			psw_check = false;
			psw_check = false;
			ins_setup_rx();
			fp_rdv2 = fp_get_db_from_dh(mem, xbd2_loc);
			fp_rdv1 = fp_get_db_from_dh(fp_reg, rf1) / fp_rdv2;
			fp_put_db(rf1, tz390.fp_dh_type, fp_rdv1);
			check_dh_div();
			break;
		case 0x6E: // 1400 "6E" "AW" "RX"
			psw_check = false;
			ins_setup_rx();
			fp_rdv1 = fp_get_db_from_dh(fp_reg, rf1)
					+ fp_get_db_from_dh(mem, xbd2_loc);
			fp_put_db(rf1, tz390.fp_dh_type, fp_rdv1);
			psw_cc = fp_get_dh_add_sub_cc();
			break;
		case 0x6F: // 1410 "6F" "SW" "RX"
			psw_check = false;
			ins_setup_rx();
			fp_rdv1 = fp_get_db_from_dh(fp_reg, rf1)
					- fp_get_db_from_dh(mem, xbd2_loc);
			fp_put_db(rf1, tz390.fp_dh_type, fp_rdv1);
			psw_cc = fp_get_dh_add_sub_cc();
			break;
		case 0x70: // 1420 "70" "STE" "RX"
			psw_check = false;
			ins_setup_rx();
			if (fp_reg_ctl[mf1] != fp_ctl_ld) {
				fp_store_reg(fp_reg, rf1);
			}
			mem.putInt(xbd2_loc, fp_reg.getInt(rf1));
			break;
		case 0x71: // 1430 "71" "MS" "RX"
			psw_check = false;
			ins_setup_rx();
			reg.putInt(rf1 + 4, reg.getInt(rf1 + 4) * mem.getInt(xbd2_loc));
			break;
		case 0x78: // 1440 "78" "LE" "RX"
			psw_check = false;
			ins_setup_rx();
			fp_load_reg(rf1, tz390.fp_eh_type, mem, xbd2_loc, tz390.fp_eh_type);
			break;
		case 0x79: // 1450 "79" "CE" "RX"
			psw_check = false;
			ins_setup_rx();
			psw_cc = fp_get_eh_comp_cc(fp_get_db_from_eh(fp_reg, rf1),
					fp_get_db_from_eh(mem, xbd2_loc));
			break;
		case 0x7A: // 1460 "7A" "AE" "RX"
			psw_check = false;
			ins_setup_rx();
			fp_rdv1 = fp_get_db_from_eh(fp_reg, rf1)
					+ fp_get_db_from_eh(mem, xbd2_loc);
			fp_put_db(rf1, tz390.fp_dh_type, fp_rdv1);
			psw_cc = fp_get_eh_add_sub_cc();
			break;
		case 0x7B: // 1470 "7B" "SE" "RX"
			psw_check = false;
			ins_setup_rx();
			fp_rdv1 = fp_get_db_from_eh(fp_reg, rf1)
					- fp_get_db_from_eh(mem, xbd2_loc);
			fp_put_db(rf1, tz390.fp_eh_type, fp_rdv1);
			psw_cc = fp_get_eh_add_sub_cc();
			break;
		case 0x7C: // 1480 "7C" "MDE" "RX"
			psw_check = false;
			ins_setup_rx();
			fp_rdv1 = fp_get_db_from_eh(fp_reg, rf1)
					* fp_get_db_from_eh(mem, xbd2_loc);
			fp_put_db(rf1, tz390.fp_dh_type, fp_rdv1);
			check_dh_mpy();
			break;
		case 0x7D: // 1500 "7D" "DE" "RX"
			psw_check = false;
			ins_setup_rx();
			fp_rdv2 = fp_get_db_from_eh(mem, xbd2_loc);
			fp_rdv1 = fp_get_db_from_eh(fp_reg, rf1) / fp_rdv2;
			fp_put_db(rf1, tz390.fp_eh_type, fp_rdv1);
			check_eh_div();
			break;
		case 0x7E: // 1510 "7E" "AU" "RX"
			psw_check = false;
			ins_setup_rx();
			fp_rdv1 = fp_get_db_from_eh(fp_reg, rf1)
					+ fp_get_db_from_eh(mem, xbd2_loc);
			fp_put_db(rf1, tz390.fp_eh_type, fp_rdv1);
			psw_cc = fp_get_eh_comp_cc(fp_rdv1, 0);
			break;
		case 0x7F: // 1520 "7F" "SU" "RX"
			psw_check = false;
			ins_setup_rx();
			fp_rdv1 = fp_get_db_from_eh(fp_reg, rf1)
					- fp_get_db_from_eh(mem, xbd2_loc);
			fp_put_db(rf1, tz390.fp_eh_type, fp_rdv1);
			psw_cc = fp_get_eh_comp_cc(fp_rdv1, 0);
			break;
		}
	}

	private void ins_lt_c0() {
		/*
		 * execute opcodes >= x'80'
		 */
		switch (opcode1) {
		case 0x80: // 1530 "8000" "SSM" "S"
			ins_setup_s();
			break;
		case 0x82: // 1540 "8200" "LPSW" "S"
			psw_check = false;
			ins_setup_s();
			set_psw_loc(mem.getInt(bd2_loc + 4));
			break;
		case 0x83: // 1550 "83" "DIAGNOSE" "DM"
			ins_setup_dm();
			break;
		case 0x84: // 1560 "84" "BRXH" "RSI"
			psw_check = false;
			ins_setup_rsi();
			rv3 = reg.getInt(rf3 + 4);
			rv1 = reg.getInt(rf1 + 4) + rv3;
			reg.putInt(rf1 + 4, rv1);
			if (rf3 == ((rf3 >> 4) << 4)) {
				rv3 = reg.getInt(rf3 + 12);
			}
			if (rv1 > rv3) {
				set_psw_loc(psw_loc - 4 + 2 * if2);
			}
			break;
		case 0x85: // 1580 "85" "BRXLE" "RSI"
			psw_check = false;
			ins_setup_rsi();
			rv3 = reg.getInt(rf3 + 4);
			rv1 = reg.getInt(rf1 + 4) + rv3;
			reg.putInt(rf1 + 4, rv1);
			if (rf3 == ((rf3 >> 4) << 4)) {
				rv3 = reg.getInt(rf3 + 12);
			}
			if (rv1 <= rv3) {
				set_psw_loc(psw_loc - 4 + 2 * if2);
			}
			break;
		case 0x86: // 1600 "86" "BXH" "RS"
			psw_check = false;
			ins_setup_rs();
			rv3 = reg.getInt(rf3 + 4);
			rv1 = reg.getInt(rf1 + 4) + rv3;
			reg.putInt(rf1 + 4, rv1);
			if (rf3 == ((rf3 >> 4) << 4)) {
				rv3 = reg.getInt(rf3 + 12);
			}
			if (rv1 > rv3) {
				set_psw_loc(bd2_loc);
			}
			break;
		case 0x87: // 1610 "87" "BXLE" "RS"
			psw_check = false;
			ins_setup_rs();
			rv3 = reg.getInt(rf3 + 4);
			rv1 = reg.getInt(rf1 + 4) + rv3;
			reg.putInt(rf1 + 4, rv1);
			if (rf3 == ((rf3 >> 4) << 4)) {
				rv3 = reg.getInt(rf3 + 12);
			}
			if (rv1 <= rv3) {
				set_psw_loc(bd2_loc);
			}
			break;
		case 0x88: // 1620 "88" "SRL" "RS"
			psw_check = false;
			ins_setup_rs();
			reg.putInt(rf1 + 4, reg.getInt(rf1 + 4) >>> (bd2_loc & 0x3f));
			break;
		case 0x89: // 1630 "89" "SLL" "RS"
			psw_check = false;
			ins_setup_rs();
			reg.putInt(rf1 + 4, reg.getInt(rf1 + 4) << (bd2_loc & 0x3f));
			break;
		case 0x8A: // 1640 "8A" "SRA" "RS"
			psw_check = false;
			ins_setup_rs();
			reg.putInt(rf1 + 4, get_sra32(reg.getInt(rf1 + 4), bd2_loc & 0x3f));
			break;
		case 0x8B: // 1650 "8B" "SLA" "RS"
			psw_check = false;
			ins_setup_rs();
			reg.putInt(rf1 + 4, get_sla32(reg.getInt(rf1 + 4), bd2_loc & 0x3f));
			break;
		case 0x8C: // 1660 "8C" "SRDL" "RS"
			psw_check = false;
			ins_setup_rs();
			rlv1 = ((long) (reg.getInt(rf1 + 4)) << 32 | ((long) reg
					.getInt(rf1 + 12) & long_low32_bits)) >>> (bd2_loc & 0x3f); // RPI
																				// 398
			reg.putInt(rf1 + 4, (int) (rlv1 >>> 32));
			reg.putInt(rf1 + 12, (int) rlv1);
			break;
		case 0x8D: // 1670 "8D" "SLDL" "RS"
			psw_check = false;
			ins_setup_rs();
			rlv1 = ((long) (reg.getInt(rf1 + 4)) << 32 | ((long) reg
					.getInt(rf1 + 12) & long_low32_bits)) << (bd2_loc & 0x3f); // RPI
																				// 398
			reg.putInt(rf1 + 4, (int) (rlv1 >>> 32));
			reg.putInt(rf1 + 12, (int) rlv1);
			break;
		case 0x8E: // 1680 "8E" "SRDA" "RS"
			psw_check = false;
			ins_setup_rs();
			rlv1 = ((long) (reg.getInt(rf1 + 4)) << 32 | ((long) reg
					.getInt(rf1 + 12) & long_low32_bits)) >> (bd2_loc & 0x3f); // RPI
																				// 398
			reg.putInt(rf1 + 4, (int) (rlv1 >>> 32));
			reg.putInt(rf1 + 12, (int) rlv1);
			psw_cc = get_long_comp_cc(rlv1, 0);
			break;
		case 0x8F: // 1690 "8F" "SLDA" "RS"
			psw_check = false;
			ins_setup_rs();
			rlv1 = get_sla64(((long) (reg.getInt(rf1 + 4)) << 32 | ((long) reg
					.getInt(rf1 + 12) & long_low32_bits)), bd2_loc & 0x3f); // RPI
																			// 398
			reg.putInt(rf1 + 4, (int) (rlv1 >>> 32));
			reg.putInt(rf1 + 12, (int) rlv1);
			break;
		case 0x90: // 1700 "90" "STM" "RS"
			psw_check = false;
			ins_setup_rs();
			if (rf1 > rf3) {
				while (rf1 < reg_len) {
					mem.putInt(bd2_loc, reg.getInt(rf1 + 4));
					bd2_loc = bd2_loc + 4;
					rf1 = rf1 + 8;
				}
				rf1 = 0;
			}
			while (rf1 <= rf3) {
				mem.putInt(bd2_loc, reg.getInt(rf1 + 4));
				bd2_loc = bd2_loc + 4;
				rf1 = rf1 + 8;
			}
			break;
		case 0x91: // 1710 "91" "TM" "SI"
			psw_check = false;
			ins_setup_si();
			rv1 = mem_byte[bd1_loc] & 0xff;
			rvw = rv1 & if2;
			if (rvw == 0) {
				psw_cc = psw_cc0;
			} else if (rvw == if2) { // RPI44
				psw_cc = psw_cc3;
			} else {
				psw_cc = psw_cc1;
			}
			break;
		case 0x92: // 1720 "92" "MVI" "SI"
			psw_check = false;
			ins_setup_si();
			mem_byte[bd1_loc] = (byte) if2;
			break;
		case 0x93: // 1730 "9300" "TS" "S"
			psw_check = false;
			ins_setup_s();
			if ((mem_byte[bd2_loc] & 0x80) != 0) {
				psw_cc = psw_cc1;
			} else {
				psw_cc = psw_cc0;
			}
			mem_byte[bd2_loc] = (byte) 0xff;
			break;
		case 0x94: // 1740 "94" "NI" "SI"
			psw_check = false;
			ins_setup_si();
			sv1 = mem_byte[bd1_loc] & if2;
			mem_byte[bd1_loc] = (byte) sv1;
			if (sv1 == 0) {
				psw_cc = psw_cc0;
			} else {
				psw_cc = psw_cc1;
			}
			break;
		case 0x95: // 1750 "95" "CLI" "SI"
			psw_check = false;
			ins_setup_si();
			psw_cc = get_int_comp_cc((mem_byte[bd1_loc] & 0xff), if2);
			break;
		case 0x96: // 1760 "96" "OI" "SI"
			psw_check = false;
			ins_setup_si();
			sv1 = mem_byte[bd1_loc] | if2;
			mem_byte[bd1_loc] = (byte) sv1;
			if (sv1 == 0) {
				psw_cc = psw_cc0;
			} else {
				psw_cc = psw_cc1;
			}
			break;
		case 0x97: // 1770 "97" "XI" "SI"
			psw_check = false;
			ins_setup_si();
			sv1 = mem_byte[bd1_loc] ^ if2;
			mem_byte[bd1_loc] = (byte) sv1;
			if (sv1 == 0) {
				psw_cc = psw_cc0;
			} else {
				psw_cc = psw_cc1;
			}
			break;
		case 0x98: // 1780 "98" "LM" "RS"
			psw_check = false;
			ins_setup_rs();
			if (rf1 > rf3) {
				while (rf1 < reg_len) {
					reg.putInt(rf1 + 4, mem.getInt(bd2_loc));
					bd2_loc = bd2_loc + 4;
					rf1 = rf1 + 8;
				}
				rf1 = 0;
			}
			while (rf1 <= rf3) {
				reg.putInt(rf1 + 4, mem.getInt(bd2_loc));
				bd2_loc = bd2_loc + 4;
				rf1 = rf1 + 8;
			}
			break;
		case 0x99: // 1790 "99" "TRACE" "RS"
			ins_setup_rs();
			break;
		case 0x9A: // 1800 "9A" "LAM" "RS"
			ins_setup_rs();
			break;
		case 0x9B: // 1810 "9B" "STAM" "RS"
			ins_setup_rs();
			break;
		case 0xA5:
			opcode2 = mem_byte[psw_loc + opcode2_offset_ri] & 0x0f;
			switch (opcode2) {
			case 0x0: // 1820 "A50" "IIHH" "RI"
				psw_check = false;
				ins_setup_ri();
				reg.putShort(rf1, (short) if2);
				break;
			case 0x1: // 1830 "A51" "IIHL" "RI"
				psw_check = false;
				ins_setup_ri();
				reg.putShort(rf1 + 2, (short) if2);
				break;
			case 0x2: // 1840 "A52" "IILH" "RI"
				psw_check = false;
				ins_setup_ri();
				reg.putShort(rf1 + 4, (short) if2);
				break;
			case 0x3: // 1850 "A53" "IILL" "RI"
				psw_check = false;
				ins_setup_ri();
				reg.putShort(rf1 + 6, (short) if2);
				break;
			case 0x4: // 1860 "A54" "NIHH" "RI"
				psw_check = false;
				ins_setup_ri();
				rv1 = reg.getInt(rf1) & ((if2 << 16) | 0xffff);
				reg.putInt(rf1, rv1);
				if (rv1 == 0) {
					psw_cc = psw_cc0;
				} else {
					psw_cc = psw_cc1;
				}
				break;
			case 0x5: // 1870 "A55" "NIHL" "RI"
				psw_check = false;
				ins_setup_ri();
				rv1 = reg.getInt(rf1) & (0xffff0000 | (if2 & 0xffff));
				reg.putInt(rf1, rv1);
				if (rv1 == 0) {
					psw_cc = psw_cc0;
				} else {
					psw_cc = psw_cc1;
				}
				break;
			case 0x6: // 1880 "A56" "NILH" "RI"
				psw_check = false;
				ins_setup_ri();
				rv1 = reg.getInt(rf1 + 4) & (0xffff | (if2 << 16));
				reg.putInt(rf1 + 4, rv1);
				if (rv1 == 0) {
					psw_cc = psw_cc0;
				} else {
					psw_cc = psw_cc1;
				}
				break;
			case 0x7: // 1890 "A57" "NILL" "RI"
				psw_check = false;
				ins_setup_ri();
				rv1 = reg.getInt(rf1 + 4) & (0xffff0000 | (if2 & 0xffff));
				reg.putInt(rf1 + 4, rv1);
				if (rv1 == 0) {
					psw_cc = psw_cc0;
				} else {
					psw_cc = psw_cc1;
				}
				break;
			case 0x8: // 1900 "A58" "OIHH" "RI"
				psw_check = false;
				ins_setup_ri();
				rv1 = reg.getInt(rf1) | (if2 << 16);
				reg.putInt(rf1, rv1);
				if (rv1 == 0) {
					psw_cc = psw_cc0;
				} else {
					psw_cc = psw_cc1;
				}
				break;
			case 0x9: // 1910 "A59" "OIHL" "RI"
				psw_check = false;
				ins_setup_ri();
				rv1 = reg.getInt(rf1) | (if2 & 0xffff);
				reg.putInt(rf1, rv1);
				if (rv1 == 0) { // rpi 295
					psw_cc = psw_cc0;
				} else {
					psw_cc = psw_cc1;
				}
				break;
			case 0xA: // 1920 "A5A" "OILH" "RI"
				psw_check = false;
				ins_setup_ri();
				rv1 = reg.getInt(rf1 + 4) | (if2 << 16); // RPI 295
				reg.putInt(rf1 + 4, rv1);
				if (rv1 == 0) {
					psw_cc = psw_cc0;
				} else {
					psw_cc = psw_cc1;
				}
				break;
			case 0xB: // 1930 "A5B" "OILL" "RI"\
				psw_check = false;
				ins_setup_ri();
				rv1 = reg.getInt(rf1 + 4) | (if2 & 0xffff);
				reg.putInt(rf1 + 4, rv1);
				if (rv1 == 0) {
					psw_cc = psw_cc0;
				} else {
					psw_cc = psw_cc1;
				}
				break;
			case 0xC: // 1940 "A5C" "LLIHH" "RI"
				psw_check = false;
				ins_setup_ri();
				reg.putLong(rf1, (long) if2 << 48); // RPI158
				break;
			case 0xD: // 1950 "A5D" "LLIHL" "RI"
				psw_check = false;
				ins_setup_ri();
				reg.putLong(rf1, ((long) if2 << 32) & long_low48_bits); // RPI158
				break;
			case 0xE: // 1960 "A5E" "LLILH" "RI"
				psw_check = false;
				ins_setup_ri();
				reg.putLong(rf1, ((long) if2 << 16) & long_low32_bits); // RPI158
				break;
			case 0xF: // 1970 "A5F" "LLILL" "RI"
				psw_check = false;
				ins_setup_ri();
				reg.putLong(rf1, (long) (if2 & 0xffff)); // RPI158
				break;
			}
			break;
		case 0xA7:
			opcode2 = mem_byte[psw_loc + opcode2_offset_ri] & 0x0f;
			switch (opcode2) {
			case 0x0: // 1980 "A70" "TMLH" "RI"
				psw_check = false;
				ins_setup_ri();
				psw_cc = get_tm_reg_cc(reg.getShort(rf1 + 4) & 0xffff, if2);
				break;
			case 0x1: // 2000 "A71" "TMLL" "RI"
				psw_check = false;
				ins_setup_ri();
				psw_cc = get_tm_reg_cc(reg.getShort(rf1 + 6) & 0xffff, if2);
				break;
			case 0x2: // 2020 "A72" "TMHH" "RI"
				psw_check = false;
				ins_setup_ri();
				psw_cc = get_tm_reg_cc(reg.getShort(rf1) & 0xffff, if2);
				break;
			case 0x3: // 2030 "A73" "TMHL" "RI"
				psw_check = false;
				ins_setup_ri();
				psw_cc = get_tm_reg_cc(reg.getShort(rf1 + 2) & 0xffff, if2);
				break;
			case 0x4: // 2040 "A74" "BRC" "RI"
				psw_check = false;
				ins_setup_ri();
				if ((psw_cc & mf1) > 0) {
					set_psw_loc(psw_loc - 4 + 2 * if2);
				}
				break;
			case 0x5: // 2360 "A75" "BRAS" "RI"
				psw_check = false;
				ins_setup_ri();
				reg.putInt(rf1 + 4, psw_loc | psw_amode_bit);
				set_psw_loc(psw_loc - 4 + 2 * if2);
				break;
			case 0x6: // 2380 "A76" "BRCT" "RI"
				psw_check = false;
				ins_setup_ri();
				rv1 = reg.getInt(rf1 + 4) - 1;
				reg.putInt(rf1 + 4, rv1);
				if (rv1 != 0) {
					set_psw_loc(psw_loc - 4 + if2 + if2); // RPI 357
				}
				break;
			case 0x7: // 2400 "A77" "BRCTG" "RI"
				psw_check = false;
				ins_setup_ri();
				rlv1 = reg.getLong(rf1) - 1;
				reg.putLong(rf1, rlv1);
				if (rlv1 != 0) {
					set_psw_loc(psw_loc - 4 + 2 * if2);
				}
				break;
			case 0x8: // 2420 "A78" "LHI" "RI"
				psw_check = false;
				ins_setup_ri();
				reg.putInt(rf1 + 4, if2);
				break;
			case 0x9: // 2430 "A79" "LGHI" "RI"
				psw_check = false;
				ins_setup_ri();
				reg.putLong(rf1, if2);
				break;
			case 0xA: // 2440 "A7A" "AHI" "RI"
				psw_check = false;
				ins_setup_ri();
				rv1 = reg.getInt(rf1 + 4);
				rv2 = if2;
				rv3 = rv1 + rv2;
				reg.putInt(rf1 + 4, rv3);
				psw_cc = get_int_add_cc();
				break;
			case 0xB: // 2450 "A7B" "AGHI" "RI"
				psw_check = false;
				ins_setup_ri();
				rlv1 = reg.getLong(rf1);
				rlv2 = if2;
				rlv3 = rlv1 + rlv2;
				reg.putLong(rf1, rlv3);
				psw_cc = get_long_add_cc();
				break;
			case 0xC: // 2460 "A7C" "MHI" "RI"
				psw_check = false;
				ins_setup_ri();
				reg.putInt(rf1 + 4, reg.getInt(rf1 + 4) * if2);
				break;
			case 0xD: // 2470 "A7D" "MGHI" "RI"
				psw_check = false;
				ins_setup_ri();
				reg.putLong(rf1, reg.getLong(rf1) * if2);
				break;
			case 0xE: // 2480 "A7E" "CHI" "RI"
				psw_check = false;
				ins_setup_ri();
				psw_cc = get_int_comp_cc(reg.getInt(rf1 + 4), if2);
				break;
			case 0xF: // 2490 "A7F" "CGHI" "RI"
				psw_check = false;
				ins_setup_ri();
				psw_cc = get_long_comp_cc(reg.getLong(rf1), if2);
				break;
			}
			break;
		case 0xA8: // 2500 "A8" "MVCLE" "RS"
			psw_check = false;
			ins_setup_rs();
			fill_char = (byte) bd2_loc;
			bd1_loc = reg.getInt(rf1 + 4) & psw_amode;
			bd2_loc = reg.getInt(rf3 + 4) & psw_amode;
			rflen1 = reg.getInt(rf1 + 12) & max_pos_int;
			rflen2 = reg.getInt(rf3 + 12) & max_pos_int;
			data_len = rflen1;
			pad_len = 0;
			psw_cc = psw_cc0;
			if (rflen1 < rflen2) {
				psw_cc = psw_cc1;
			} else if (rflen1 > rflen2) {
				psw_cc = psw_cc2;
				data_len = rflen2;
				pad_len = rflen1 - rflen2;
			}
			if (bd1_loc + data_len + pad_len > tot_mem) {
				set_psw_check(psw_pic_addr); // RPI 397
				break;
			}
			if (bd1_loc + data_len <= bd2_loc || bd2_loc + data_len <= bd1_loc) {
				System
						.arraycopy(mem_byte, bd2_loc, mem_byte, bd1_loc,
								data_len); // RPI 411
				bd1_loc = bd1_loc + data_len;
				bd2_loc = bd2_loc + data_len;
			} else if (bd2_loc + 1 == bd1_loc) {
				Arrays.fill(mem_byte, bd1_loc, bd1_loc + data_len,
						mem_byte[bd2_loc]);
				bd1_loc = bd1_loc + data_len;
				bd2_loc = bd2_loc + data_len;
			} else {
				bd1_end = bd1_loc + data_len;
				// destructive overlap with gap > 1
				bd1_end = bd1_loc + data_len;
				while (bd1_loc < bd1_end) {
					mem_byte[bd1_loc] = mem_byte[bd2_loc];
					bd1_loc++;
					bd2_loc++;
				}
			}
			bd1_end = bd1_loc + pad_len;
			if (bd1_loc < bd1_end) {
				Arrays.fill(mem_byte, bd1_loc, bd1_loc + pad_len, fill_char);
				bd1_loc = bd1_loc + pad_len;
			}
			reg.putInt(rf1 + 4, (reg.getInt(rf1 + 4) & psw_amode_high_bits)
					| bd1_loc);
			reg.putInt(rf1 + 12, 0);
			reg.putInt(rf3 + 4, (reg.getInt(rf3 + 4) & psw_amode_high_bits)
					| bd2_loc);
			reg.putInt(rf3 + 12, rflen2 - data_len);
			break;
		case 0xA9: // 2510 "A9" "CLCLE" "RS"
			psw_check = false;
			ins_setup_rs();
			fill_char = (byte) bd2_loc;
			bd1_loc = reg.getInt(rf1 + 4) & psw_amode;
			bd2_loc = reg.getInt(rf3 + 4) & psw_amode;
			bd1_start = bd1_loc;
			bd2_start = bd2_loc;
			rflen1 = reg.getInt(rf1 + 12) & max_pos_int;
			rflen2 = reg.getInt(rf3 + 12) & max_pos_int;
			data_len = rflen1;
			pad_len = 0;
			psw_cc = psw_cc0;
			if (rflen1 < rflen2) {
				data_len = rflen1;
				pad_len = rflen2 - rflen1;
			} else if (rflen1 > rflen2) {
				data_len = rflen2;
				pad_len = rflen1 - rflen2;
			}
			if (bd1_loc + data_len + pad_len > tot_mem) {
				set_psw_check(psw_pic_addr); // RPI 397
				break;
			}
			bd1_end = bd1_loc + data_len;
			while (bd1_loc < bd1_end) {
				if (mem_byte[bd1_loc] != mem_byte[bd2_loc]) {
					if ((mem_byte[bd1_loc] & 0xff) > (mem_byte[bd2_loc] & 0xff)) {
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
			if (psw_cc == psw_cc_equal & pad_len > 0) {
				if (rflen1 > rflen2) {
					bd1_end = bd1_loc + pad_len;
					while (bd1_loc < bd1_end) {
						if (mem_byte[bd1_loc] != fill_char) {
							if ((mem_byte[bd1_loc] & 0xff) > (fill_char & 0xff)) {
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
					while (bd2_loc < bd2_end) {
						if (mem_byte[bd2_loc] != fill_char) {
							if ((mem_byte[bd2_loc] & 0xff) > (fill_char & 0xff)) {
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
			reg.putInt(rf1 + 4, (reg.getInt(rf1 + 4) & psw_amode_high_bits)
					| bd1_loc);
			reg.putInt(rf1 + 12, rflen1 - (bd1_loc - bd1_start));
			reg.putInt(rf3 + 4, (reg.getInt(rf3 + 4) & psw_amode_high_bits)
					| bd2_loc);
			reg.putInt(rf3 + 12, rflen2 - (bd2_loc - bd2_start));
			break;
		case 0xAC: // 2520 "AC" "STNSM" "SI"
			ins_setup_si();
			break;
		case 0xAD: // 2530 "AD" "STOSM" "SI"
			ins_setup_si();
			break;
		case 0xAE: // 2540 "AE" "SIGP" "RS"
			ins_setup_rs();
			break;
		case 0xAF: // 2550 "AF" "MC" "SI"
			ins_setup_si();
			break;
		case 0xB1: // 2560 "B1" "LRA" "RX"
			ins_setup_rx();
			break;
		case 0xB2:
			opcode2 = mem_byte[psw_loc + opcode2_offset_s] & 0xff;
			switch (opcode2) {
			case 0x02: // 2570 "B202" "STIDP" "S"
				psw_check = false;
				ins_setup_s();
				mem.putLong(bd2_loc, cpu_id);
				break;
			case 0x04: // 2580 "B204" "SCK" "S"
				ins_setup_s();
				break;
			case 0x05: // 2590 "B205" "STCK" "S"
				psw_check = false;
				ins_setup_s();
				/*
				 * Notes: 1. The Java time stored is milliseconds since Jan. 1,
				 * 1970. 2. For IBM OS compatibility the STCK instruction
				 * converts Java time to base of January 1, 1900 with bit 51
				 * being incremented each microsecond. 3. The low bits are
				 * incremented to prevent duplicate values from being returned
				 * when timing on.
				 */
				if (tz390.opt_timing) {
					java_mil = System.currentTimeMillis();
				} else {
					java_mil = cur_date.getTime();
				}
				ibm_ms = (java_mil + ibm_mil) * 1000;
				cur_stck = ibm_ms << (63 - 51);
				if (cur_stck > last_stck) {
					last_stck = cur_stck;
				} else {
					last_stck++;
				}
				mem.putLong(bd2_loc, last_stck);
				break;
			case 0x06: // 2600 "B206" "SCKC" "S"
				ins_setup_s();
				break;
			case 0x07: // 2610 "B207" "STCKC" "S"
				ins_setup_s();
				break;
			case 0x08: // 2620 "B208" "SPT" "S"
				ins_setup_s();
				break;
			case 0x09: // 2630 "B209" "STPT" "S"
				ins_setup_s();
				break;
			case 0x0A: // 2640 "B20A" "SPKA" "S"
				ins_setup_s();
				break;
			case 0x0B: // 2650 "B20B" "IPK" "S"
				ins_setup_s();
				break;
			case 0x0D: // 2660 "B20D" "PTLB" "S"
				ins_setup_s();
				break;
			case 0x10: // 2670 "B210" "SPX" "S"
				ins_setup_s();
				break;
			case 0x11: // 2680 "B211" "STPX" "S"
				ins_setup_s();
				break;
			case 0x12: // 2690 "B212" "STAP" "S"
				ins_setup_s();
				break;
			case 0x18: // 2700 "B218" "PC" "S"
				psw_check = false;
				ins_setup_s();
				push_pc_stack(psw_loc);
				set_psw_loc(bd2_loc);
				break;
			case 0x19: // 2710 "B219" "SAC" "S"
				ins_setup_s();
				break;
			case 0x1A: // 2720 "B21A" "CFC" "S"
				ins_setup_s();
				break;
			case 0x21: // 2730 "B221" "IPTE" "RRE"
				ins_setup_rre();
				break;
			case 0x22: // 2740 "B222" "IPM" "RRE"
				psw_check = false;
				ins_setup_rre();
				int_work = (psw_cc_code[psw_cc] << 4) | psw_pgm_mask;
				rv1 = reg.getInt(rf1 + 4) & 0xffffff; // RPI106
				reg.putInt(rf1 + 4, rv1 | (int_work << 24)); // RPI106
				break;
			case 0x23: // 2750 "B223" "IVSK" "RRE"
				ins_setup_rre();
				break;
			case 0x24: // 2760 "B224" "IAC" "RRE"
				ins_setup_rre();
				break;
			case 0x25: // 2770 "B225" "SSAR" "RRE"
				ins_setup_rre();
				break;
			case 0x26: // 2780 "B226" "EPAR" "RRE"
				ins_setup_rre();
				break;
			case 0x27: // 2790 "B227" "ESAR" "RRE"
				ins_setup_rre();
				break;
			case 0x28: // 2800 "B228" "PT" "RRE"
				ins_setup_rre();
				break;
			case 0x29: // 2810 "B229" "ISKE" "RRE"
				ins_setup_rre();
				break;
			case 0x2A: // 2820 "B22A" "RRBE" "RRE"
				ins_setup_rre();
				break;
			case 0x2B: // 2830 "B22B" "SSKE" "RRE"
				ins_setup_rre();
				break;
			case 0x2C: // 2840 "B22C" "TB" "RRE"
				ins_setup_rre();
				break;
			case 0x2D: // 2850 "B22D" "DXR" "RRE"
				psw_check = false;
				ins_setup_rre();
				fp_rbdv1 = fp_get_bd_from_lh(fp_reg, rf1);
				fp_rbdv2 = fp_get_bd_from_lh(fp_reg, rf2);
				if (fp_rbdv2.compareTo(BigDecimal.ZERO) != 0) {
					fp_rbdv1 = fp_rbdv1.divide(fp_rbdv2, fp_bd_context).round(
							fp_x_context);
				}
				fp_put_bd(rf1, tz390.fp_lh_type, fp_rbdv1);
				check_lh_div();
				break;
			case 0x2E: // 2860 "B22E" "PGIN" "RRE"
				ins_setup_rre();
				break;
			case 0x2F: // 2870 "B22F" "PGOUT" "RRE"
				ins_setup_rre();
				break;
			case 0x30: // 2880 "B230" "CSCH" "S"
				ins_setup_s();
				break;
			case 0x31: // 2890 "B231" "HSCH" "S"
				ins_setup_s();
				break;
			case 0x32: // 2900 "B232" "MSCH" "S"
				ins_setup_s();
				break;
			case 0x33: // 2910 "B233" "SSCH" "S"
				ins_setup_s();
				break;
			case 0x34: // 2920 "B234" "STSCH" "S"
				ins_setup_s();
				break;
			case 0x35: // 2930 "B235" "TSCH" "S"
				ins_setup_s();
				break;
			case 0x36: // 2940 "B236" "TPI" "S"
				ins_setup_s();
				break;
			case 0x37: // 2950 "B237" "SAL" "S"
				ins_setup_s();
				break;
			case 0x38: // 2960 "B238" "RSCH" "S"
				ins_setup_s();
				break;
			case 0x39: // 2970 "B239" "STCRW" "S"
				ins_setup_s();
				break;
			case 0x3A: // 2980 "B23A" "STCPS" "S"
				ins_setup_s();
				break;
			case 0x3B: // 2990 "B23B" "RCHP" "S"
				ins_setup_s();
				break;
			case 0x3C: // 3000 "B23C" "SCHM" "S"
				ins_setup_s();
				break;
			case 0x40: // 3010 "B240" "BAKR" "RRE"
				psw_check = false;
				ins_setup_rre();
				if (rf1 == 0) {
					push_pc_stack(psw_loc);
				} else {
					push_pc_stack(reg.getInt(rf1 + 4));
				}
				if (rf2 != 0) {
					set_psw_loc(reg.getInt(rf2 + 4));
				}
				break;
			case 0x41: // 3020 "B241" "CKSM" "RRE"
				ins_setup_rre();
				break;
			case 0x44: // 3030 "B244" "SQDR" "RRE"
				psw_check = false;
				ins_setup_rre();
				fp_rdv1 = Math.sqrt(fp_get_db_from_dh(fp_reg, rf2)); // RPI
																		// 364
				fp_put_db(rf1, tz390.fp_dh_type, fp_rdv1);
				break;
			case 0x45: // 3040 "B245" "SQER" "RRE"
				psw_check = false;
				ins_setup_rre();
				fp_rdv1 = Math.sqrt(fp_get_db_from_eh(fp_reg, rf2));
				fp_put_db(rf1, tz390.fp_eh_type, fp_rdv1);
				break;
			case 0x46: // 3050 "B246" "STURA" "RRE"
				ins_setup_rre();
				break;
			case 0x47: // 3060 "B247" "MSTA" "RRE"
				ins_setup_rre();
				break;
			case 0x48: // 3070 "B248" "PALB" "RRE"
				ins_setup_rre();
				break;
			case 0x49: // 3080 "B249" "EREG" "RRE"
				psw_check = false;
				ins_setup_rre();
				if (cur_pc_stk_reg <= 0) {
					set_psw_check(psw_pic_stkerr);
					return;
				}
				int pc_stk_reg_base = cur_pc_stk_reg - reg_len;
				if (rf1 > rf3) {
					while (rf1 < reg_len) {
						reg.putInt(rf1 + 4, pc_stk_reg.getInt(pc_stk_reg_base
								+ rf1 + 4));
						rf1 = rf1 + 8;
					}
					rf1 = 0;
				}
				while (rf1 <= rf3) {
					reg.putInt(rf1 + 4, pc_stk_reg.getInt(pc_stk_reg_base + rf1
							+ 4));
					rf1 = rf1 + 8;
				}
				break;
			case 0x4A: // 3090 "B24A" "ESTA" "RRE"
				ins_setup_rre();
				break;
			case 0x4B: // 3100 "B24B" "LURA" "RRE"
				ins_setup_rre();
				break;
			case 0x4C: // 3110 "B24C" "TAR" "RRE"
				ins_setup_rre();
				break;
			case 0x4D: // 3120 "B24D" "CPYA" "RRE"
				ins_setup_rre();
				break;
			case 0x4E: // 3130 "B24E" "SAR" "RRE"
				ins_setup_rre();
				break;
			case 0x4F: // 3140 "B24F" "EAR" "RRE"
				ins_setup_rre();
				break;
			case 0x50: // 3150 "B250" "CSP" "RRE"
				ins_setup_rre();
				break;
			case 0x52: // 3160 "B252" "MSR" "RRE"
				psw_check = false;
				ins_setup_rre();
				reg.putInt(rf1 + 4, reg.getInt(rf1 + 4) * reg.getInt(rf2 + 4));
				break;
			case 0x54: // 3170 "B254" "MVPG" "RRE"
				ins_setup_rre();
				break;
			case 0x55: // 3180 "B255" "MVST" "RRE"
				psw_check = false; // RPI 441
				ins_setup_rre();
				bd1_loc = reg.getInt(rf1 + 4) & psw_amode;
				bd2_loc = reg.getInt(rf2 + 4) & psw_amode;
				bd2_start = bd2_loc;
				string_eod = reg.get(7);
				while (mem_byte[bd2_loc] != string_eod) { // RPI 453
					bd2_loc++; // RPI 476
				}
				rflen = bd2_loc - bd2_start + 1;
				System.arraycopy(mem_byte, bd2_start, mem_byte, bd1_loc, rflen); // RPI
																					// 411
				reg.putInt(r1, bd1_loc + rflen - 1);
				psw_cc = psw_cc1;
				break;
			case 0x57: // 3190 "B257" "CUSE" "RRE"
				psw_check = false;
				ins_setup_rre();
				exec_cuse();
				break;
			case 0x58: // 3200 "B258" "BSG" "RRE"
				ins_setup_rre();
				break;
			case 0x5A: // 3210 "B25A" "BSA" "RRE"
				ins_setup_rre();
				break;
			case 0x5D: // 3220 "B25D" "CLST" "RRE"
				psw_check = false;
				ins_setup_rre();
				exec_clst();
				break;
			case 0x5E: // 3230 "B25E" "SRST" "RRE"
				psw_check = false;
				ins_setup_rre();
				exec_srst();
				break;
			case 0x63: // 3240 "B263" "CMPSC" "RRE"
				ins_setup_rre();
				break;
			case 0x76: // 3250 "B276" "XSCH" "S"
				ins_setup_s();
				break;
			case 0x77: // 3260 "B277" "RP" "S"
				ins_setup_s();
				break;
			case 0x78: // 3270 "B278" "STCKE" "S"
				psw_check = false;
				ins_setup_s();
				/*
				 * Notes: 1. The Java time stored is milliseconds since Jan. 1,
				 * 1970. 2. For IBM OS compatibility the STCKE instruction
				 * converts Java time to base of January 1, 1900 with bit 59
				 * being incremented each microsecond.
				 */
				if (tz390.opt_timing) {
					java_mil = System.currentTimeMillis();
				} else {
					java_mil = cur_date.getTime();
				}
				ibm_ms = (java_mil + ibm_mil) * 1000;
				mem.putLong(bd2_loc, ibm_ms << (63 - 59));
				mem.putLong(bd2_loc + 8, 0);
				break;
			case 0x79: // 3280 "B279" "SACF" "S"
				ins_setup_s();
				break;
			case 0x7C: // 3300 "B27C" "STCKF" "S" Z9-2
				psw_check = false;
				ins_setup_s();
				/*
				 * Notes: 1. The Java time stored is milliseconds since Jan. 1,
				 * 1970. 2. For IBM OS compatibility the STCK instruction
				 * converts Java time to base of January 1, 1900 with bit 51
				 * being incremented each microsecond.
				 */
				if (tz390.opt_timing) {
					java_mil = System.currentTimeMillis();
				} else {
					java_mil = cur_date.getTime();
				}
				ibm_ms = (java_mil + ibm_mil) * 1000;
				mem.putLong(bd2_loc, ibm_ms << (63 - 51));
				psw_cc = psw_cc0;
				break;
			case 0x7D: // 3290 "B27D" "STSI" "S"
				ins_setup_s();
				break;
			case 0x99: // 3300 "B299" "SRNM" "S"
				psw_check = false;
				ins_setup_s();
				fp_fpc_rnd = (bd2_loc & 0x3); // set rounding mode
				fp_fpc_reg = (fp_fpc_reg & fp_fpc_mask_rnd_not) | fp_fpc_rnd;
				switch (fp_fpc_rnd) {
				case 0: // round half even
					fp_e_context = MathContext.DECIMAL32; // fp_fpc_rnd
															// default half even
					fp_d_context = MathContext.DECIMAL64; // fp_fpc_rnd
															// default half even
					fp_x_context = MathContext.DECIMAL128; // fp_fpc_rnd
															// default half even
					break;
				case 1: // round toward 0
					fp_e_context = new MathContext(32, RoundingMode.DOWN);
					fp_d_context = new MathContext(64, RoundingMode.DOWN);
					fp_x_context = new MathContext(128, RoundingMode.DOWN);
					break;
				case 2: // round toward +infinity
					fp_e_context = new MathContext(32, RoundingMode.CEILING);
					fp_d_context = new MathContext(64, RoundingMode.CEILING);
					fp_x_context = new MathContext(128, RoundingMode.CEILING);
					break;
				case 3: // round toward -infinity;
					fp_e_context = new MathContext(32, RoundingMode.FLOOR);
					fp_d_context = new MathContext(64, RoundingMode.FLOOR);
					fp_x_context = new MathContext(128, RoundingMode.FLOOR);
					break;
				}
				break;
			case 0x9C: // 3310 "B29C" "STFPC" "S"
				psw_check = false;
				ins_setup_s();
				mem.putInt(bd2_loc, fp_fpc_reg | (fp_dxc << 8));
				break;
			case 0x9D: // 3320 "B29D" "LFPC" "S"
				psw_check = false;
				ins_setup_s();
				fp_fpc_reg = mem.getInt(bd2_loc);
				fp_dxc = (fp_fpc_reg & 0xff00) >>> 8;
				fp_fpc_reg = fp_fpc_reg & 0xffff00ff;
				break;
			case 0xA5: // 3330 "B2A5" "TRE" "RRE"
				psw_check = false; // RPI 454
				ins_setup_rre();
				string_eod = reg.get(r0 + 3);
				bd1_loc = reg.getInt(rf1 + 4) & psw_amode;
				bd2_loc = reg.getInt(rf2 + 4) & psw_amode;
				bd1_end = bd1_loc + reg.getInt(rf1 + 12);
				psw_cc = psw_cc3;
				while (psw_cc == psw_cc3) {
					if (mem_byte[bd1_loc] == string_eod) {
						psw_cc = psw_cc1;
					} else {
						mem_byte[bd1_loc] = mem_byte[bd2_loc
								+ (mem_byte[bd1_loc] & 0xff)];
						bd1_loc++;
						if (bd1_loc >= bd1_end) {
							psw_cc = psw_cc0;
						}
					}
				}
				reg.putInt(rf1 + 4, bd1_loc);
				reg.putInt(rf1 + 12, bd1_end - bd1_loc);
				break;
			case 0xA6: // 3340 "B2A6" "CUUTF" "RRE"
				ins_setup_rre();
				break;
			case 0xA7: // 3360 "B2A7" "CUTFU" "RRE"
				ins_setup_rre();
				break;
			case 0xB0: // 3400 "B2B0" "STFLE" "S" Z9-3
				psw_check = false;
				ins_setup_s();
				/*
				 * store feature bit list
				 */
				mem.putLong(bd2_loc, sz390.get_feature_bits());
				reg.put(r0 + 7, (byte) 0); // number of feature doulbe words-1
				psw_cc = psw_cc0;
				break;
			case 0xB1: // 3380 "B2B1" "STFL" "S"
				ins_setup_s();
				break;
			case 0xB2: // 3390 "B2B2" "LPSWE" "S"
				ins_setup_s();
				break;
			case 0xFF: // 3400 "B2FF" "TRAP4" "S"
				ins_setup_s();
				break;
			}
			break;
		case 0xB3:
			opcode2 = mem_byte[psw_loc + opcode2_offset_rre] & 0xff;
			switch (opcode2) {
			case 0x00: // 3410 "B300" "LPEBR" "RRE"
				psw_check = false;
				ins_setup_rre();
				fp_rev1 = Math.abs(fp_get_eb_from_eb(fp_reg, rf2));
				fp_put_eb(rf1, tz390.fp_eb_type, fp_rev1);
				psw_cc = fp_get_eb_comp_cc(fp_rev1, 0);
				break;
			case 0x01: // 3420 "B301" "LNEBR" "RRE"
				psw_check = false;
				ins_setup_rre();
				fp_rev1 = Math.abs(fp_get_eb_from_eb(fp_reg, rf2));
				if (fp_rev1 != 0) {
					fp_rev1 = -fp_rev1;
				}
				fp_put_eb(rf1, tz390.fp_eb_type, fp_rev1);
				psw_cc = fp_get_eb_comp_cc(fp_rev1, 0);
				break;
			case 0x02: // 3430 "B302" "LTEBR" "RRE"
				psw_check = false;
				ins_setup_rre();
				fp_rev1 = fp_get_eb_from_eb(fp_reg, rf2);
				fp_put_eb(rf1, tz390.fp_eb_type, fp_rev1);
				psw_cc = fp_get_eb_comp_cc(fp_rev1, 0);
				break;
			case 0x03: // 3440 "B303" "LCEBR" "RRE"
				psw_check = false;
				ins_setup_rre();
				fp_rev1 = fp_get_eb_from_eb(fp_reg, rf2);
				if (fp_rev1 != 0) {
					fp_rev1 = -fp_rev1;
				}
				fp_put_eb(rf1, tz390.fp_eb_type, fp_rev1);
				psw_cc = fp_get_eb_comp_cc(fp_rev1, 0);
				break;
			case 0x04: // 3450 "B304" "LDEBR" "RRE"
				psw_check = false;
				ins_setup_rre();
				fp_rdv1 = fp_get_db_from_eb(fp_reg, rf2);
				fp_put_db(rf1, tz390.fp_db_type, fp_rdv1);
				break;
			case 0x05: // 3460 "B305" "LXDBR" "RRE"
				psw_check = false;
				ins_setup_rre();
				fp_rbdv1 = fp_get_bd_from_db(fp_reg, rf2);
				fp_put_bd(rf1, tz390.fp_lb_type, fp_rbdv1);
				break;
			case 0x06: // 3470 "B306" "LXEBR" "RRE"
				psw_check = false;
				ins_setup_rre();
				fp_rbdv1 = fp_get_bd_from_eb(fp_reg, rf2);
				fp_put_bd(rf1, tz390.fp_lb_type, fp_rbdv1);
				break;
			case 0x07: // 3480 "B307" "MXDBR" "RRE"
				psw_check = false;
				ins_setup_rre();
				fp_rbdv1 = fp_get_bd_from_db(fp_reg, rf1).multiply(
						fp_get_bd_from_db(fp_reg, rf2), fp_db_context).round(
						fp_x_context);
				fp_put_bd(rf1, tz390.fp_lb_type, fp_rbdv1);
				check_lb_mpy();
				break;
			case 0x08: // 3490 "B308" "KEBR" "RRE"
				psw_check = false;
				ins_setup_rre();
				fp_signal = true;
				psw_cc = fp_get_eb_comp_cc(fp_get_eb_from_eb(fp_reg, rf1),
						fp_get_eb_from_eb(fp_reg, rf2));
				break;
			case 0x09: // 3500 "B309" "CEBR" "RRE"
				psw_check = false;
				ins_setup_rre();
				psw_cc = fp_get_eb_comp_cc(fp_get_eb_from_eb(fp_reg, rf1),
						fp_get_eb_from_eb(fp_reg, rf2));
				break;
			case 0x0A: // 3510 "B30A" "AEBR" "RRE"
				psw_check = false;
				ins_setup_rre();
				fp_rev1 = fp_get_eb_from_eb(fp_reg, rf1)
						+ fp_get_eb_from_eb(fp_reg, rf2);
				fp_put_eb(rf1, tz390.fp_eb_type, fp_rev1);
				psw_cc = fp_get_eb_add_sub_cc();
				break;
			case 0x0B: // 3520 "B30B" "SEBR" "RRE"
				psw_check = false;
				ins_setup_rre();
				fp_rev1 = fp_get_eb_from_eb(fp_reg, rf1)
						- fp_get_eb_from_eb(fp_reg, rf2);
				fp_put_eb(rf1, tz390.fp_eb_type, fp_rev1);
				psw_cc = fp_get_eb_add_sub_cc();
				break;
			case 0x0C: // 3530 "B30C" "MDEBR" "RRE"
				psw_check = false;
				ins_setup_rre();
				fp_rdv1 = fp_get_db_from_eb(fp_reg, rf1)
						* fp_get_db_from_eb(fp_reg, rf2);
				fp_put_db(rf1, tz390.fp_db_type, fp_rdv1);
				check_db_mpy();
				break;
			case 0x0D: // 3540 "B30D" "DEBR" "RRE"
				psw_check = false;
				ins_setup_rre();
				fp_rev1 = fp_get_eb_from_eb(fp_reg, rf1);
				fp_rev2 = fp_get_eb_from_eb(fp_reg, rf2);
				fp_rev1 = fp_rev1 / fp_rev2;
				fp_put_eb(rf1, tz390.fp_eb_type, fp_rev1);
				check_eb_div();
				break;
			case 0x0E: // 3550 "B30E" "MAEBR" "RRF"
				psw_check = false;
				ins_setup_rrf1();
				fp_rev1 = fp_get_eb_from_eb(fp_reg, rf1)
						+ fp_get_eb_from_eb(fp_reg, rf2)
						* fp_get_eb_from_eb(fp_reg, rf3);
				fp_put_eb(rf1, tz390.fp_eb_type, fp_rev1);
				check_eb_mpy();
				break;
			case 0x0F: // 3560 "B30F" "MSEBR" "RRF"
				psw_check = false;
				ins_setup_rrf1();
				fp_rev1 = fp_get_eb_from_eb(fp_reg, rf1)
						- fp_get_eb_from_eb(fp_reg, rf2)
						* fp_get_eb_from_eb(fp_reg, rf3);
				fp_put_eb(rf1, tz390.fp_eb_type, fp_rev1);
				check_eb_mpy();
				break;
			case 0x10: // 3570 "B310" "LPDBR" "RRE"
				psw_check = false;
				ins_setup_rre();
				fp_rdv1 = Math.abs(fp_get_db_from_db(fp_reg, rf2));
				fp_put_db(rf1, tz390.fp_db_type, fp_rdv1);
				psw_cc = fp_get_db_comp_cc(fp_rdv1, 0);
				break;
			case 0x11: // 3580 "B311" "LNDBR" "RRE"
				psw_check = false;
				ins_setup_rre();
				fp_rdv1 = -Math.abs(fp_get_db_from_db(fp_reg, rf2));
				fp_put_db(rf1, tz390.fp_db_type, fp_rdv1);
				psw_cc = fp_get_db_comp_cc(fp_rdv1, 0);
				break;
			case 0x12: // 3590 "B312" "LTDBR" "RRE"
				psw_check = false;
				ins_setup_rre();
				fp_rdv1 = fp_get_db_from_db(fp_reg, rf2);
				fp_put_db(rf1, tz390.fp_db_type, fp_rdv1);
				psw_cc = fp_get_db_comp_cc(fp_rdv1, 0);
				break;
			case 0x13: // 3600 "B313" "LCDBR" "RRE"
				psw_check = false;
				ins_setup_rre();
				fp_rdv1 = -fp_get_db_from_db(fp_reg, rf2);
				fp_put_db(rf1, tz390.fp_db_type, fp_rdv1);
				psw_cc = fp_get_db_comp_cc(fp_rdv1, 0);
				break;
			case 0x14: // 3610 "B314" "SQEBR" "RRE"
				psw_check = false;
				ins_setup_rre();
				fp_rev1 = (float) Math.sqrt(fp_get_db_from_eb(fp_reg, rf2));
				fp_put_eb(rf1, tz390.fp_eb_type, fp_rev1);
				break;
			case 0x15: // 3620 "B315" "SQDBR" "RRE"
				psw_check = false;
				ins_setup_rre();
				fp_rdv1 = Math.sqrt(fp_get_db_from_db(fp_reg, rf2));
				fp_put_db(rf1, tz390.fp_db_type, fp_rdv1);
				break;
			case 0x16: // 3630 "B316" "SQXBR" "RRE"
				psw_check = false;
				ins_setup_rre();
				fp_rbdv2 = fp_get_bd_from_lb(fp_reg, rf2);
				fp_get_bd_sqrt();
				fp_put_bd(rf1, tz390.fp_lb_type, fp_rbdv1);
				break;
			case 0x17: // 3640 "B317" "MEEBR" "RRE"
				psw_check = false;
				ins_setup_rre();
				fp_rev1 = fp_get_eb_from_eb(fp_reg, rf1)
						* fp_get_eb_from_eb(fp_reg, rf2);
				fp_put_eb(rf1, tz390.fp_eb_type, fp_rev1);
				check_eh_mpy();
				break;
			case 0x18: // 3650 "B318" "KDBR" "RRE"
				psw_check = false;
				ins_setup_rre();
				fp_signal = true;
				psw_cc = fp_get_db_comp_cc(fp_get_db_from_db(fp_reg, rf1),
						fp_get_db_from_db(fp_reg, rf2));
				break;
			case 0x19: // 3660 "B319" "CDBR" "RRE"
				psw_check = false;
				ins_setup_rre();
				psw_cc = fp_get_db_comp_cc(fp_get_db_from_db(fp_reg, rf1),
						fp_get_db_from_db(fp_reg, rf2));
				break;
			case 0x1A: // 3670 "B31A" "ADBR" "RRE"
				psw_check = false;
				ins_setup_rre();
				fp_rdv1 = fp_get_db_from_db(fp_reg, rf1)
						+ fp_get_db_from_db(fp_reg, rf2);
				fp_put_db(rf1, tz390.fp_db_type, fp_rdv1);
				psw_cc = fp_get_db_add_sub_cc();
				break;
			case 0x1B: // 3680 "B31B" "SDBR" "RRE"
				psw_check = false;
				ins_setup_rre();
				fp_rdv1 = fp_get_db_from_db(fp_reg, rf1)
						- fp_get_db_from_db(fp_reg, rf2);
				fp_put_db(rf1, tz390.fp_db_type, fp_rdv1);
				psw_cc = fp_get_db_add_sub_cc();
				break;
			case 0x1C: // 3690 "B31C" "MDBR" "RRE"
				psw_check = false;
				ins_setup_rre();
				fp_rdv1 = fp_get_db_from_db(fp_reg, rf1)
						* fp_get_db_from_db(fp_reg, rf2);
				fp_put_db(rf1, tz390.fp_db_type, fp_rdv1);
				check_db_mpy();
				break;
			case 0x1D: // 3700 "B31D" "DDBR" "RRE"
				psw_check = false;
				ins_setup_rre();
				fp_rdv1 = fp_get_db_from_db(fp_reg, rf1);
				fp_rdv2 = fp_get_db_from_db(fp_reg, rf2);
				fp_rdv1 = fp_rdv1 / fp_rdv2;
				fp_put_db(rf1, tz390.fp_db_type, fp_rdv1);
				check_db_div();
				break;
			case 0x1E: // 3710 "B31E" "MADBR" "RRF"
				psw_check = false;
				ins_setup_rrf1();
				fp_rdv1 = fp_get_db_from_db(fp_reg, rf1)
						+ fp_get_db_from_db(fp_reg, rf2)
						* fp_get_db_from_db(fp_reg, rf3);
				fp_put_db(rf1, tz390.fp_db_type, fp_rdv1);
				check_db_mpy();
				break;
			case 0x1F: // 3720 "B31F" "MSDBR" "RRF"
				psw_check = false;
				ins_setup_rrf1();
				fp_rdv1 = fp_get_db_from_db(fp_reg, rf1)
						- fp_get_db_from_db(fp_reg, rf2)
						* fp_get_db_from_db(fp_reg, rf3);
				fp_put_db(rf1, tz390.fp_db_type, fp_rdv1);
				check_db_mpy();
				break;
			case 0x24: // 3730 "B324" "LDER" "RRE"
				psw_check = false;
				ins_setup_rre();
				fp_load_reg(rf1, tz390.fp_dh_type, fp_reg, rf2,
						tz390.fp_eh_type);
				break;
			case 0x25: // 3740 "B325" "LXDR" "RRE"
				psw_check = false;
				ins_setup_rre();
				fp_load_reg(rf1, tz390.fp_lh_type, fp_reg, rf2,
						tz390.fp_dh_type);
				break;
			case 0x26: // 3750 "B326" "LXER" "RRE"
				psw_check = false;
				ins_setup_rre();
				fp_load_reg(rf1, tz390.fp_lh_type, fp_reg, rf2,
						tz390.fp_eh_type);
				break;
			case 0x2E: // 3760 "B32E" "MAER" "RRF"
				psw_check = false;
				ins_setup_rrf1();
				fp_rdv1 = fp_get_db_from_eh(fp_reg, rf1)
						+ fp_get_db_from_eh(fp_reg, rf2)
						* fp_get_db_from_eh(fp_reg, rf3);
				fp_put_db(rf1, tz390.fp_eh_type, fp_rdv1);
				check_eh_mpy();
				break;
			case 0x2F: // 3770 "B32F" "MSER" "RRF"
				psw_check = false;
				ins_setup_rrf1();
				fp_rdv1 = fp_get_db_from_eh(fp_reg, rf1)
						- fp_get_db_from_eh(fp_reg, rf2)
						* fp_get_db_from_eh(fp_reg, rf3);
				fp_put_db(rf1, tz390.fp_eh_type, fp_rdv1);
				check_eh_mpy();
				break;
			case 0x36: // 3780 "B336" "SQXR" "RRE"
				psw_check = false;
				ins_setup_rre();
				fp_rbdv2 = fp_get_bd_from_lh(fp_reg, rf2);
				fp_get_bd_sqrt();
				fp_put_bd(rf1, tz390.fp_lh_type, fp_rbdv1);
				break;
			case 0x37: // 3790 "B337" "MEER" "RRE"
				psw_check = false;
				ins_setup_rre();
				fp_rdv1 = fp_get_db_from_eh(fp_reg, rf1)
						* fp_get_db_from_eh(fp_reg, rf2);
				fp_put_db(rf1, tz390.fp_eh_type, fp_rdv1);
				check_eh_mpy();
				break;
			case 0x38: // 3830 "B338" "MAYLR" "RRF" Z9-4
				psw_check = false; // RPI 298
				ins_setup_rrf1();
				fp_rbdv1 = fp_get_bd_from_dh(fp_reg, rf2).multiply(
						fp_get_bd_from_dh(fp_reg, rf3), fp_db_context).add(
						fp_get_bd_from_dh(fp_reg, rf1)).round(fp_x_context);
				fp_put_bd(rf1, tz390.fp_lh_type, fp_rbdv1);
				check_lh_mpy();
				break;
			case 0x39: // 3840 "B339" "MYLR" "RRF" Z9-5
				psw_check = false; // RPI 298
				ins_setup_rrf1();
				fp_rbdv1 = fp_get_bd_from_dh(fp_reg, rf2).multiply(
						fp_get_bd_from_dh(fp_reg, rf3), fp_db_context).round(
						fp_x_context);
				fp_put_bd(rf1, tz390.fp_lh_type, fp_rbdv1);
				check_lh_mpy();
				break;
			case 0x3A: // 3850 "B33A" "MAYR" "RRF" Z9-6
				psw_check = false;
				ins_setup_rrf1();
				fp_rbdv1 = fp_get_bd_from_dh(fp_reg, rf2).multiply(
						fp_get_bd_from_dh(fp_reg, rf3), fp_db_context).add(
						fp_get_bd_from_dh(fp_reg, rf1)).round(fp_x_context);
				fp_put_bd(rf1, tz390.fp_lh_type, fp_rbdv1);
				check_lh_mpy();
				break;
			case 0x3B: // 3860 "B33B" "MYR" "RRF" Z9-7
				psw_check = false;
				ins_setup_rrf1();
				fp_rbdv1 = fp_get_bd_from_dh(fp_reg, rf2).multiply(
						fp_get_bd_from_dh(fp_reg, rf3), fp_db_context).round(
						fp_x_context);
				fp_put_bd(rf1, tz390.fp_lh_type, fp_rbdv1);
				check_lh_mpy();
				break;
			case 0x3C: // 3870 "B33C" "MAYHR" "RRF" Z9-8
				psw_check = false; // RPI 298
				ins_setup_rrf1();
				fp_rbdv1 = fp_get_bd_from_dh(fp_reg, rf2).multiply(
						fp_get_bd_from_dh(fp_reg, rf3), fp_db_context).add(
						fp_get_bd_from_dh(fp_reg, rf1)).round(fp_x_context);
				fp_put_bd(rf1, tz390.fp_lh_type, fp_rbdv1);
				check_lh_mpy();
				break;
			case 0x3D: // 3880 "B33D" "MYHR" "RRF" Z9-9
				psw_check = false; // RPI 298
				ins_setup_rrf1();
				fp_rbdv1 = fp_get_bd_from_dh(fp_reg, rf2).multiply(
						fp_get_bd_from_dh(fp_reg, rf3), fp_db_context).round(
						fp_x_context);
				fp_put_bd(rf1, tz390.fp_lh_type, fp_rbdv1);
				check_lh_mpy();
				break;
			case 0x3E: // 3800 "B33E" "MADR" "RRF"
				psw_check = false;
				ins_setup_rrf1();
				fp_rdv1 = fp_get_db_from_dh(fp_reg, rf1)
						+ fp_get_db_from_dh(fp_reg, rf2)
						* fp_get_db_from_dh(fp_reg, rf3);
				fp_put_db(rf1, tz390.fp_dh_type, fp_rdv1);
				check_dh_mpy();
				break;
			case 0x3F: // 3810 "B33F" "MSDR" "RRF"
				psw_check = false;
				ins_setup_rrf1();
				fp_rdv1 = fp_get_db_from_dh(fp_reg, rf1)
						- fp_get_db_from_dh(fp_reg, rf2)
						* fp_get_db_from_dh(fp_reg, rf3);
				fp_put_db(rf1, tz390.fp_dh_type, fp_rdv1);
				check_dh_mpy();
				break;
			case 0x40: // 3820 "B340" "LPXBR" "RRE"
				psw_check = false;
				ins_setup_rre();
				fp_rbdv1 = fp_get_bd_from_lb(fp_reg, rf2).abs();
				fp_put_bd(rf1, tz390.fp_lb_type, fp_rbdv1);
				psw_cc = fp_get_lb_comp_cc(fp_rbdv1, BigDecimal.ZERO);
				break;
			case 0x41: // 3830 "B341" "LNXBR" "RRE"
				psw_check = false;
				ins_setup_rre();
				fp_rbdv1 = fp_get_bd_from_lb(fp_reg, rf2).abs().negate();
				fp_put_bd(rf1, tz390.fp_lb_type, fp_rbdv1);
				psw_cc = fp_get_lb_comp_cc(fp_rbdv1, BigDecimal.ZERO);
				break;
			case 0x42: // 3840 "B342" "LTXBR" "RRE"
				psw_check = false;
				ins_setup_rre();
				fp_rbdv1 = fp_get_bd_from_lb(fp_reg, rf2);
				fp_put_bd(rf1, tz390.fp_lb_type, fp_rbdv1);
				psw_cc = fp_get_lb_comp_cc(fp_rbdv1, BigDecimal.ZERO);
				break;
			case 0x43: // 3850 "B343" "LCXBR" "RRE"
				psw_check = false;
				ins_setup_rre();
				fp_rbdv1 = fp_get_bd_from_lb(fp_reg, rf2).negate();
				fp_put_bd(rf1, tz390.fp_lb_type, fp_rbdv1);
				psw_cc = fp_get_lb_comp_cc(fp_rbdv1, BigDecimal.ZERO);
				break;
			case 0x44: // 3860 "B344" "LEDBR" "RRE"
				psw_check = false;
				ins_setup_rre();
				fp_load_reg(rf1, tz390.fp_eb_type, fp_reg, rf2,
						tz390.fp_db_type);
				break;
			case 0x45: // 3870 "B345" "LDXBR" "RRE"
				psw_check = false;
				ins_setup_rre();
				fp_load_reg(rf1, tz390.fp_db_type, fp_reg, rf2,
						tz390.fp_lb_type);
				break;
			case 0x46: // 3880 "B346" "LEXBR" "RRE"
				psw_check = false;
				ins_setup_rre();
				fp_load_reg(rf1, tz390.fp_eb_type, fp_reg, rf2,
						tz390.fp_lb_type);
				break;
			case 0x47: // 3890 "B347" "FIXBR" "RRF"
				psw_check = false;
				ins_setup_rrf2();
				fp_bd_int_rem = fp_get_bd_from_lb(fp_reg, rf2)
						.divideAndRemainder(BigDecimal.ONE);
				fp_put_bd(rf1, tz390.fp_lb_type, fp_get_bd_rnd(mf3));
				break;
			case 0x48: // 3900 "B348" "KXBR" "RRE"
				psw_check = false;
				ins_setup_rre();
				fp_signal = true;
				psw_cc = fp_get_lb_comp_cc(fp_get_bd_from_lb(fp_reg, rf1),
						fp_get_bd_from_lb(fp_reg, rf2));
				break;
			case 0x49: // 3910 "B349" "CXBR" "RRE"
				psw_check = false;
				ins_setup_rre();
				psw_cc = fp_get_lb_comp_cc(fp_get_bd_from_lb(fp_reg, rf1),
						fp_get_bd_from_lb(fp_reg, rf2));
				break;
			case 0x4A: // 3920 "B34A" "AXBR" "RRE"
				psw_check = false;
				ins_setup_rre();
				fp_rbdv1 = fp_get_bd_from_lb(fp_reg, rf1);
				fp_rbdv2 = fp_get_bd_from_lb(fp_reg, rf2);
				fp_rbdv1 = fp_rbdv1.add(fp_rbdv2, fp_x_context).round(
						fp_x_context);
				fp_put_bd(rf1, tz390.fp_lb_type, fp_rbdv1);
				psw_cc = fp_get_lb_add_sub_cc();
				break;
			case 0x4B: // 3930 "B34B" "SXBR" "RRE"
				psw_check = false;
				ins_setup_rre();
				fp_rbdv1 = fp_get_bd_from_lb(fp_reg, rf1).subtract(
						fp_get_bd_from_lb(fp_reg, rf2), fp_bd_context);
				fp_put_bd(rf1, tz390.fp_lb_type, fp_rbdv1);
				psw_cc = fp_get_lb_add_sub_cc();
				break;
			case 0x4C: // 3940 "B34C" "MXBR" "RRE"
				psw_check = false;
				ins_setup_rre();
				fp_rbdv1 = fp_get_bd_from_lb(fp_reg, rf1).multiply(
						fp_get_bd_from_lb(fp_reg, rf2), fp_bd_context).round(
						fp_x_context);
				fp_put_bd(rf1, tz390.fp_lb_type, fp_rbdv1);
				check_lb_mpy();
				break;
			case 0x4D: // 3950 "B34D" "DXBR" "RRE"
				psw_check = false;
				ins_setup_rre();
				fp_rbdv1 = fp_get_bd_from_lb(fp_reg, rf1);
				fp_rbdv2 = fp_get_bd_from_lb(fp_reg, rf2);
				if (fp_rbdv2.compareTo(BigDecimal.ZERO) != 0) {
					fp_rbdv1 = fp_rbdv1.divide(fp_rbdv2, fp_bd_context).round(
							fp_x_context);
				}
				fp_put_bd(rf1, tz390.fp_lb_type, fp_rbdv1);
				check_lb_div();
				break;
			case 0x50: // 3960 "B350" "TBEDR" "RRF"
				psw_check = false;
				ins_setup_rrf2();
				fp_bd_int_rem = fp_get_bd_from_dh(fp_reg, rf2)
						.divideAndRemainder(BigDecimal.ONE);
				fp_rev1 = fp_get_bd_rnd(mf3).floatValue(); // RPI 333
				fp_put_eb(rf1, tz390.fp_eb_type, fp_rev1);
				psw_cc = fp_get_eb_comp_cc(fp_rev1, 0);
				break;
			case 0x51: // 3970 "B351" "TBDR" "RRF"
				psw_check = false;
				ins_setup_rrf2();
				fp_bd_int_rem = fp_get_bd_from_dh(fp_reg, rf2)
						.divideAndRemainder(BigDecimal.ONE);
				fp_rdv1 = fp_get_bd_rnd(mf3).doubleValue(); // RPI 333
				fp_put_db(rf1, tz390.fp_db_type, fp_rdv1);
				psw_cc = fp_get_db_comp_cc(fp_rdv1, 0);
				break;
			case 0x53: // 3980 "B353" "DIEBR" "RR4"
				psw_check = false;
				ins_setup_rrf3();
				if (mf4 != 0) { // only def. rounding for now
					set_psw_check(psw_pic_spec);
				}
				fp_rdv1 = fp_get_db_from_eb(fp_reg, rf1);
				fp_rdv2 = fp_get_db_from_eb(fp_reg, rf2);
				fp_rdv3 = (long) (fp_rdv1 / fp_rdv2);
				fp_rdv4 = fp_rdv1 - fp_rdv3 * fp_rdv2;
				fp_put_eb(rf3, tz390.fp_eb_type, (float) fp_rdv3);
				fp_put_eb(rf1, tz390.fp_eb_type, (float) fp_rdv4);
				psw_cc = fp_get_di_cc();
				break;
			case 0x57: // 3990 "B357" "FIEBR" "RRF"
				psw_check = false;
				ins_setup_rrf2();
				fp_bd_int_rem = fp_get_bd_from_eb(fp_reg, rf2)
						.divideAndRemainder(BigDecimal.ONE);
				fp_put_eb(rf1, tz390.fp_eb_type, fp_get_bd_rnd(mf3)
						.floatValue());
				break;
			case 0x58: // 4000 "B358" "THDER" "RRE"
				psw_check = false;
				ins_setup_rre();
				fp_rdv1 = fp_get_db_from_eb(fp_reg, rf2);
				fp_put_db(rf1, tz390.fp_dh_type, fp_rdv1);
				psw_cc = fp_get_db_comp_cc(fp_rdv1, 0);
				break;
			case 0x59: // 4010 "B359" "THDR" "RRE"
				psw_check = false;
				ins_setup_rre();
				fp_rdv1 = fp_get_db_from_db(fp_reg, rf2);
				fp_put_db(rf1, tz390.fp_dh_type, fp_rdv1);
				psw_cc = fp_get_db_comp_cc(fp_rdv1, 0);
				break;
			case 0x5B: // 4020 "B35B" "DIDBR" "RR4"
				psw_check = false;
				ins_setup_rrf3();
				if (mf4 != 0) { // only def. rounding for now
					set_psw_check(psw_pic_spec);
				}
				fp_rdv1 = fp_get_db_from_db(fp_reg, rf1);
				fp_rdv2 = fp_get_db_from_db(fp_reg, rf2);
				fp_rdv3 = (long) (fp_rdv1 / fp_rdv2);
				fp_rdv4 = fp_rdv1 - fp_rdv3 * fp_rdv2;
				fp_put_db(rf3, tz390.fp_db_type, fp_rdv3);
				fp_put_db(rf1, tz390.fp_db_type, fp_rdv4);
				psw_cc = fp_get_di_cc();
				break;
			case 0x5F: // 4030 "B35F" "FIDBR" "RRF"
				psw_check = false;
				ins_setup_rrf2();
				fp_bd_int_rem = fp_get_bd_from_db(fp_reg, rf2)
						.divideAndRemainder(BigDecimal.ONE);
				fp_put_db(rf1, tz390.fp_db_type, fp_get_bd_rnd(mf3)
						.doubleValue());
				break;
			case 0x60: // 4040 "B360" "LPXR" "RRE"
				psw_check = false;
				ins_setup_rre();
				fp_rbdv1 = fp_get_bd_from_lh(fp_reg, rf2).abs();
				fp_put_bd(rf1, tz390.fp_lh_type, fp_rbdv1);
				psw_cc = fp_get_lh_comp_cc(fp_rbdv1, BigDecimal.ZERO);
				break;
			case 0x61: // 4050 "B361" "LNXR" "RRE"
				psw_check = false;
				ins_setup_rre();
				fp_rbdv1 = fp_get_bd_from_lh(fp_reg, rf2).abs().negate();
				fp_put_bd(rf1, tz390.fp_lh_type, fp_rbdv1);
				psw_cc = fp_get_lh_comp_cc(fp_rbdv1, BigDecimal.ZERO);
				break;
			case 0x62: // 4060 "B362" "LTXR" "RRE"
				psw_check = false;
				ins_setup_rre();
				fp_rbdv1 = fp_get_bd_from_lh(fp_reg, rf2);
				fp_put_bd(rf1, tz390.fp_lh_type, fp_rbdv1);
				psw_cc = fp_get_lh_comp_cc(fp_rbdv1, BigDecimal.ZERO);

				break;
			case 0x63: // 4070 "B363" "LCXR" "RRE"
				psw_check = false;
				ins_setup_rre();
				fp_rbdv1 = fp_get_bd_from_lh(fp_reg, rf2).negate();
				fp_put_bd(rf1, tz390.fp_lh_type, fp_rbdv1);
				psw_cc = fp_get_lh_comp_cc(fp_rbdv1, BigDecimal.ZERO);

				break;
			case 0x65: // 4080 "B365" "LXR" "RRE"
				psw_check = false;
				ins_setup_rre();
				fp_load_reg(rf1, tz390.fp_lh_type, fp_reg, rf2,
						tz390.fp_lh_type);
				break;
			case 0x66: // 4090 "B366" "LEXR" "RRE"
				psw_check = false;
				ins_setup_rre();
				fp_load_reg(rf1, tz390.fp_eh_type, fp_reg, rf2,
						tz390.fp_lh_type);
				break;
			case 0x67: // 4100 "B367" "FIXR" "RRE"
				psw_check = false;
				ins_setup_rre();
				fp_rbdv1 = fp_get_bd_from_lh(fp_reg, rf2);
				fp_rbdv1 = fp_rbdv1.subtract(fp_rbdv1.remainder(BigDecimal.ONE,
						fp_bd_context));
				fp_put_bd(rf1, tz390.fp_lh_type, fp_rbdv1);
				break;
			case 0x69: // 4110 "B369" "CXR" "RRE"
				psw_check = false;
				ins_setup_rre();
				psw_cc = fp_get_lh_comp_cc(fp_get_bd_from_lh(fp_reg, rf1),
						fp_get_bd_from_lh(fp_reg, rf2));
				break;
			case 0x74: // 4120 "B374" "LZER" "RRE"
				psw_check = false;
				ins_setup_rre();
				fp_put_db(rf1, tz390.fp_eh_type, 0);
				break;
			case 0x75: // 4130 "B375" "LZDR" "RRE"
				psw_check = false;
				ins_setup_rre();
				fp_put_db(rf1, tz390.fp_dh_type, 0);
				break;
			case 0x76: // 4140 "B376" "LZXR" "RRE"
				psw_check = false;
				ins_setup_rre();
				fp_put_bd(rf1, tz390.fp_lh_type, BigDecimal.ZERO);
				break;
			case 0x77: // 4150 "B377" "FIER" "RRE"
				psw_check = false;
				ins_setup_rre();
				fp_rdv1 = (long) fp_get_db_from_eh(fp_reg, rf2);
				fp_put_db(rf1, tz390.fp_eh_type, fp_rdv1);
				break;
			case 0x7F: // 4160 "B37F" "FIDR" "RRE"
				psw_check = false;
				ins_setup_rre();
				fp_rdv1 = (long) fp_get_db_from_dh(fp_reg, rf2);
				fp_put_db(rf1, tz390.fp_dh_type, fp_rdv1);
				break;
			case 0x84: // 4170 "B384" "SFPC" "RRE"
				psw_check = false;
				ins_setup_rre();
				fp_fpc_reg = reg.getInt(rf1 + 4);
				fp_dxc = (fp_fpc_reg & 0xff00) >>> 8;
				fp_fpc_reg = fp_fpc_reg & 0xffff00ff;
				break;
			case 0x8C: // 4180 "B38C" "EFPC" "RRE"
				psw_check = false;
				ins_setup_rre();
				reg.putInt(rf1 + 4, fp_fpc_reg | (fp_dxc << 8));
				break;
			case 0x94: // 4190 "B394" "CEFBR" "RRE"
				psw_check = false;
				ins_setup_rre();
				fp_put_eb(rf1, tz390.fp_eb_type, (float) reg.getInt(rf2 + 4));
				break;
			case 0x95: // 4200 "B395" "CDFBR" "RRE"
				psw_check = false;
				ins_setup_rre();
				fp_put_db(rf1, tz390.fp_db_type, (double) reg.getInt(rf2 + 4));
				break;
			case 0x96: // 4210 "B396" "CXFBR" "RRE"
				psw_check = false;
				ins_setup_rre();
				fp_put_bd(rf1, tz390.fp_eb_type, BigDecimal.valueOf(reg
						.getInt(rf2 + 4)));
				break;
			case 0x98: // 4220 "B398" "CFEBR" "RRF"
				psw_check = false;
				ins_setup_rrf2();
				fp_bd_int_rem = fp_get_bd_from_eb(fp_reg, rf2)
						.divideAndRemainder(BigDecimal.ONE);
				rv1 = fp_get_bd_rnd(mf3).intValue(); // RPI 333
				reg.putInt(rf1 + 4, rv1);
				psw_cc = get_int_comp_cc(rv1, 0);
				break;
			case 0x99: // 4230 "B399" "CFDBR" "RRF"
				psw_check = false;
				ins_setup_rrf2();
				fp_bd_int_rem = fp_get_bd_from_db(fp_reg, rf2)
						.divideAndRemainder(BigDecimal.ONE);
				rv1 = fp_get_bd_rnd(mf3).intValue(); // RPI 333
				reg.putInt(rf1 + 4, rv1);
				psw_cc = get_int_comp_cc(rv1, 0);
				break;
			case 0x9A: // 4240 "B39A" "CFXBR" "RRF"
				psw_check = false;
				ins_setup_rrf2();
				fp_bd_int_rem = fp_get_bd_from_lb(fp_reg, rf2)
						.divideAndRemainder(BigDecimal.ONE);
				rv1 = fp_get_bd_rnd(mf3).intValue(); // RPI 333
				reg.putInt(rf1 + 4, rv1);
				psw_cc = get_int_comp_cc(rv1, 0);
				break;
			case 0xA4: // 4250 "B3A4" "CEGBR" "RRE"
				psw_check = false;
				ins_setup_rre();
				fp_put_eb(rf1, tz390.fp_eb_type, (float) reg.getLong(rf2));
				break;
			case 0xA5: // 4260 "B3A5" "CDGBR" "RRE"
				psw_check = false;
				ins_setup_rre();
				fp_put_db(rf1, tz390.fp_db_type, (double) reg.getLong(rf2));
				break;
			case 0xA6: // 4270 "B3A6" "CXGBR" "RRE"
				psw_check = false;
				ins_setup_rre();
				fp_put_bd(rf1, tz390.fp_lb_type, BigDecimal
						.valueOf((double) reg.getLong(rf2)));
				break;
			case 0xA8: // 4280 "B3A8" "CGEBR" "RRF"
				psw_check = false;
				ins_setup_rrf2();
				fp_bd_int_rem = fp_get_bd_from_eb(fp_reg, rf2)
						.divideAndRemainder(BigDecimal.ONE);
				rlv1 = fp_get_bd_rnd(mf3).longValue(); // RPI 333
				reg.putLong(rf1, rlv1);
				psw_cc = get_long_comp_cc(rlv1, 0);
				break;
			case 0xA9: // 4290 "B3A9" "CGDBR" "RRF"
				psw_check = false;
				ins_setup_rrf2();
				fp_bd_int_rem = fp_get_bd_from_db(fp_reg, rf2)
						.divideAndRemainder(BigDecimal.ONE);
				rlv1 = fp_get_bd_rnd(mf3).longValue(); // RPI 333
				reg.putLong(rf1, rlv1);
				psw_cc = get_long_comp_cc(rlv1, 0);
				break;
			case 0xAA: // 4300 "B3AA" "CGXBR" "RRF"
				psw_check = false;
				ins_setup_rrf2();
				fp_bd_int_rem = fp_get_bd_from_lb(fp_reg, rf2)
						.divideAndRemainder(BigDecimal.ONE);
				rlv1 = fp_get_bd_rnd(mf3).longValue(); // RPI 333
				reg.putLong(rf1, rlv1);
				psw_cc = get_long_comp_cc(rlv1, 0);
				break;
			case 0xB4: // 4310 "B3B4" "CEFR" "RRE"
				psw_check = false;
				ins_setup_rre();
				fp_put_db(rf1, tz390.fp_eh_type, (float) reg.getInt(rf2 + 4));
				break;
			case 0xB5: // 4320 "B3B5" "CDFR" "RRE"
				psw_check = false;
				ins_setup_rre();
				fp_put_db(rf1, tz390.fp_dh_type, (float) reg.getInt(rf2 + 4));
				break;
			case 0xB6: // 4330 "B3B6" "CXFR" "RRE"
				psw_check = false;
				ins_setup_rre();
				fp_put_bd(rf1, tz390.fp_lh_type, BigDecimal.valueOf(reg
						.getInt(rf2 + 4)));
				break;
			case 0xB8: // 4340 "B3B8" "CFER" "RRF"
				psw_check = false;
				ins_setup_rrf2();
				fp_bd_int_rem = fp_get_bd_from_eh(fp_reg, rf2)
						.divideAndRemainder(BigDecimal.ONE);
				rv1 = fp_get_bd_rnd(mf3).intValue(); // RPI 333
				reg.putInt(rf1 + 4, rv1);
				psw_cc = get_int_comp_cc(rv1, 0);
				break;
			case 0xB9: // 4350 "B3B9" "CFDR" "RRF"
				psw_check = false;
				ins_setup_rrf2();
				fp_bd_int_rem = fp_get_bd_from_dh(fp_reg, rf2)
						.divideAndRemainder(BigDecimal.ONE);
				rv1 = fp_get_bd_rnd(mf3).intValue(); // RPI 333
				reg.putInt(rf1 + 4, rv1);
				psw_cc = get_int_comp_cc(rv1, 0);
				break;
			case 0xBA: // 4360 "B3BA" "CFXR" "RRF"
				psw_check = false;
				ins_setup_rrf2();
				fp_bd_int_rem = fp_get_bd_from_lh(fp_reg, rf2)
						.divideAndRemainder(BigDecimal.ONE);
				rv1 = fp_get_bd_rnd(mf3).intValue(); // RPI 333
				reg.putInt(rf1 + 4, rv1);
				psw_cc = get_int_comp_cc(rv1, 0);
				break;
			case 0xC4: // 4370 "B3C4" "CEGR" "RRE"
				psw_check = false;
				ins_setup_rre();
				fp_put_db(rf1, tz390.fp_eh_type, (float) reg.getLong(rf2));
				break;
			case 0xC5: // 4380 "B3C5" "CDGR" "RRE"
				psw_check = false;
				ins_setup_rre();
				fp_put_db(rf1, tz390.fp_dh_type, (double) reg.getLong(rf2));
				break;
			case 0xC6: // 4390 "B3C6" "CXGR" "RRE"
				psw_check = false;
				ins_setup_rre();
				fp_put_bd(rf1, tz390.fp_lh_type, BigDecimal.valueOf(reg
						.getLong(rf2)));
				break;
			case 0xC8: // 4400 "B3C8" "CGER" "RRF"
				psw_check = false;
				ins_setup_rrf2();
				fp_bd_int_rem = fp_get_bd_from_eh(fp_reg, rf2)
						.divideAndRemainder(BigDecimal.ONE);
				rlv1 = fp_get_bd_rnd(mf3).longValue(); // RPI 333
				reg.putLong(rf1, rlv1);
				psw_cc = get_long_comp_cc(rlv1, 0);
				break;
			case 0xC9: // 4410 "B3C9" "CGDR" "RRF"
				psw_check = false;
				ins_setup_rrf2();
				fp_bd_int_rem = fp_get_bd_from_dh(fp_reg, rf2)
						.divideAndRemainder(BigDecimal.ONE);
				rlv1 = fp_get_bd_rnd(mf3).longValue(); // RPI 333
				reg.putLong(rf1, rlv1);
				psw_cc = get_long_comp_cc(rlv1, 0);
				break;
			case 0xCA: // 4420 "B3CA" "CGXR" "RRF"
				psw_check = false;
				ins_setup_rrf2();
				fp_bd_int_rem = fp_get_bd_from_lh(fp_reg, rf2)
						.divideAndRemainder(BigDecimal.ONE);
				rlv1 = fp_get_bd_rnd(mf3).longValue(); // RPI 333
				reg.putLong(rf1, rlv1);
				psw_cc = get_long_comp_cc(rlv1, 0);
				break;
			case 0xD0: // "MDTR" "B3D0" "RRR" DFP 1
				psw_check = false;
				ins_setup_rrr();
				fp_rbdv1 = fp_get_bd_from_dd(fp_reg, rf2)
                .multiply(fp_get_bd_from_dd(fp_reg, rf3));
                fp_put_bd(rf1, tz390.fp_dd_type, fp_rbdv1);
				break;
			case 0xD1: // "DDTR" "B3D1" "RRR" DFP 2
				psw_check = false;
				ins_setup_rrr();
				fp_rbdv3 = fp_get_bd_from_dd(fp_reg, rf3);
				if (fp_rbdv3.signum() == 0){
					set_psw_check(psw_pic_fp_div);
					break;
				}
				fp_rbdv1 = fp_get_bd_from_dd(fp_reg, rf2)
                .divide(fp_rbdv3);
                fp_put_bd(rf1, tz390.fp_dd_type, fp_rbdv1);
				break;
			case 0xD2: // "ADTR" "B3D2" "RRR" DFP 3
				psw_check = false;
				ins_setup_rrr();
				fp_rbdv1 = fp_get_bd_from_dd(fp_reg, rf2)
                           .add(fp_get_bd_from_dd(fp_reg, rf3));
				fp_put_bd(rf1, tz390.fp_dd_type, fp_rbdv1);
				psw_cc = fp_get_dfp_add_sub_cc();
				break;
			case 0xD3: // "SDTR" "B3D3" "RRR" DFP 4
				psw_check = false;
				ins_setup_rrr();
				fp_rbdv1 = fp_get_bd_from_dd(fp_reg, rf2)
                         .subtract(fp_get_bd_from_dd(fp_reg, rf3));
				fp_put_bd(rf1, tz390.fp_dd_type, fp_rbdv1);
				psw_cc = fp_get_dfp_add_sub_cc();
				break;
			case 0xD4: // "LDETR" "B3D4" "RRF4" DFP 5
				psw_check = true;
				ins_setup_rrf4();
				break;
			case 0xD5: // "LEDTR" "B3D5" "RRF4" DFP 6
				psw_check = true;
				ins_setup_rrf4();
				break;
			case 0xD6: // "LTDTR" "B3D6" "RRE" DFP 7
				psw_check = true;
				ins_setup_rre();
				break;
			case 0xD7: // "FIDTR" "B3D7" "RRF4" DFP 8
				psw_check = true;
				ins_setup_rrf4();
				break;
			case 0xD8: // "MXTR" "B3D8" "RRR" DFP 9
				psw_check = true;
				ins_setup_rrr();
				break;
			case 0xD9: // "DXTR" "B3D9" "RRR" DFP 10
				psw_check = true;
				ins_setup_rrr();
				break;
			case 0xDA: // "AXTR" "B3DA" "RRR" DFP 11
				psw_check = true;
				ins_setup_rrr();
				break;
			case 0xDB: // "SXTR" "B3DB" "RRR" DFP 12
				psw_check = true;
				ins_setup_rrr();
				break;
			case 0xDC: // "LXDTR" "B3DC" "RRF4" DFP 13
				psw_check = true;
				ins_setup_rrf4();
				break;
			case 0xDD: // "LDXTR" "B3DD" "RRF4" DFP 14
				psw_check = true;
				ins_setup_rrf4();
				break;
			case 0xDE: // "LTXTR" "B3DE" "RRE" DFP 15
				psw_check = true;
				ins_setup_rre();
				break;
			case 0xDF: // "FIXTR" "B3DF" "RRF4" DFP 16
				psw_check = true;
				ins_setup_rrf4();
				break;
			case 0xE0: // "KDTR" "B3E0" "RRE" DFP 17
				psw_check = true;
				ins_setup_rre();
				break;
			case 0xE1: // "CGDTR" "B3E1" "RRF4" DFP 18
				psw_check = true;
				ins_setup_rrf4();
				break;
			case 0xE2: // "CUDTR" "B3E2" "RRE" DFP 19
				psw_check = true;
				ins_setup_rre();
				break;
			case 0xE3: // "CSDTR" "B3E3" "RRF4" DFP 20
				psw_check = false;
				ins_setup_rrf4();
				pdf_big_int = fp_get_bd_from_dd(fp_reg, rf2).unscaledValue();
				pdf_is_big = true;
				put_pd(reg_byte, rf1, 8);
				if (mf4 == 1 && pdf_big_int.signum() >= 0) {
					reg.put(rf1 + 7, (byte) (reg.get(rf1 + 7) | 0xf));
				}
				break;
			case 0xE4: // "CDTR" "B3E4" "RRE" DFP 21
				psw_check = false;
				ins_setup_rre();
				fp_rbdv1 = fp_get_bd_from_dd(fp_reg, rf1)
                .subtract(fp_get_bd_from_dd(fp_reg, rf2));
				psw_cc = fp_get_dfp_add_sub_cc();
				break;
			case 0xE5: // "EEDTR" "B3E5" "RRE" DFP 22
				psw_check = false;
				ins_setup_rre();
				fp_rbdv1 = fp_get_bd_from_dd(fp_reg, rf2);
                reg.putLong(rf1,tz390.fp_exp_bias[tz390.fp_dd_type]
                            - fp_rbdv1.stripTrailingZeros().scale());
                break;
			case 0xE7: // "ESDTR" "B3E7" "RRE" DFP 23
				psw_check = true;
				ins_setup_rre();
				break;
			case 0xE8: // "KXTR" "B3E8" "RRE" DFP 24
				psw_check = true;
				ins_setup_rre();
				break;
			case 0xE9: // "CGXTR" "B3E9" "RRF4" DFP 25
				psw_check = true;
				ins_setup_rrf4();
				break;
			case 0xEA: // "CUXTR" "B3EA" "RRE" DFP 26
				psw_check = true;
				ins_setup_rre();
				break;
			case 0xEB: // "CSXTR" "B3EB" "RRF4" DFP 27
				psw_check = true;
				ins_setup_rrf4();
				break;
			case 0xEC: // "CXTR" "B3EC" "RRE" DFP 28
				psw_check = true;
				ins_setup_rre();
				break;
			case 0xED: // "EEXTR" "B3ED" "RRE" DFP 29
				psw_check = true;
				ins_setup_rre();
				break;
			case 0xEF: // "ESXTR" "B3EF" "RRE" DFP 30
				psw_check = true;
				ins_setup_rre();
				break;
			case 0xF1: // "CDGTR" "B3F1" "RRE" DFP 31
				psw_check = true;
				ins_setup_rre();
				break;
			case 0xF2: // "CDUTR" "B3F2" "RRE" DFP 32
				psw_check = true;
				ins_setup_rre();
				break;
			case 0xF3: // "CDSTR" "B3F3" "RRE" DFP 33
				psw_check = true;
				ins_setup_rre();
				break;
			case 0xF4: // "CEDTR" "B3F4" "RRE" DFP 34
				psw_check = true;
				ins_setup_rre();
				break;
			case 0xF5: // "QADTR" "B3F5" "RRF4" DFP 35
				psw_check = true;
				ins_setup_rrf4();
				break;
			case 0xF6: // "IEDTR" "B3F6" "RRF4" DFP 36
				psw_check = true;
				ins_setup_rrf4();
				break;
			case 0xF7: // "RRDTR" "B3F7" "RRF4" DFP 37
				psw_check = true;
				ins_setup_rrf4();
				break;
			case 0xF9: // "CXGTR" "B3F9" "RRE" DFP 38
				psw_check = true;
				ins_setup_rre();
				break;
			case 0xFA: // "CXUTR" "B3FA" "RRE" DFP 39
				psw_check = true;
				ins_setup_rre();
				break;
			case 0xFB: // "CXSTR" "B3FB" "RRE" DFP 40
				psw_check = true;
				ins_setup_rre();
				break;
			case 0xFC: // "CEXTR" "B3FC" "RRE" DFP 41
				psw_check = true;
				ins_setup_rre();
				break;
			case 0xFD: // "QAXTR" "B3FD" "RRF4" DFP 42
				psw_check = true;
				ins_setup_rrf4();
				break;
			case 0xFE: // "IEXTR" "B3FE" "RRF4" DFP 43
				psw_check = true;
				ins_setup_rrf4();
				break;
			case 0xFF: // "RRXTR" "B3FF" "RRF4" DFP 44
				psw_check = true;
				ins_setup_rrf4();
				break;
			}
			break;
		case 0xB6: // 4430 "B6" "STCTL" "RS"
			ins_setup_rs();
			break;
		case 0xB7: // 4440 "B7" "LCTL" "RS"
			ins_setup_rs();
			break;
		case 0xB9:
			opcode2 = mem_byte[psw_loc + opcode2_offset_rre] & 0xff;
			switch (opcode2) {
			case 0x00: // 4450 "B900" "LPGR" "RRE"
				psw_check = false;
				ins_setup_rre();
				rlv1 = reg.getLong(rf2);
				if (rlv1 < 0) {
					if (rlv1 == long_high_bit) {
						psw_cc = psw_cc3;
						break;
					}
					rlv1 = -rlv1;
				}
				reg.putLong(rf1, rlv1);
				psw_cc = get_long_comp_cc(rlv1, 0);
				break;
			case 0x01: // 4460 "B901" "LNGR" "RRE"
				psw_check = false;
				ins_setup_rre();
				rlv1 = reg.getLong(rf2);
				if (rlv1 > 0) {
					rlv1 = -rlv1;
				}
				reg.putLong(rf1, rlv1);
				psw_cc = get_long_comp_cc(rlv1, 0);
				break;
			case 0x02: // 4470 "B902" "LTGR" "RRE"
				psw_check = false;
				ins_setup_rre();
				rlv1 = reg.getLong(rf2);
				psw_cc = get_long_comp_cc(rlv1, 0);
				break;
			case 0x03: // 4480 "B903" "LCGR" "RRE"
				psw_check = false;
				ins_setup_rre();
				rlv1 = reg.getLong(rf2);
				if (rlv1 == long_high_bit) {
					psw_cc = psw_cc3;
					break;
				}
				rlv1 = -rlv1;
				reg.putLong(rf1, rlv1);
				psw_cc = get_long_comp_cc(rlv1, 0);
				break;
			case 0x04: // 4490 "B904" "LGR" "RRE"
				psw_check = false;
				ins_setup_rre();
				reg.putLong(rf1, reg.getLong(rf2));
				break;
			case 0x05: // 4500 "B905" "LURAG" "RRE"
				ins_setup_rre();
				break;
			case 0x06: // 4600 "B906" "LGBR" "RRE" Z9-10
				psw_check = false;
				ins_setup_rre();
				reg.putLong(rf1, reg.get(rf2 + 7));
				break;
			case 0x07: // 4610 "B907" "LGHR" "RRE" Z9-11
				psw_check = false;
				ins_setup_rre();
				reg.putLong(rf1, reg.getShort(rf2 + 6));
				break;
			case 0x08: // 4510 "B908" "AGR" "RRE"
				psw_check = false;
				ins_setup_rre();
				rlv1 = reg.getLong(rf1);
				rlv2 = reg.getLong(rf2);
				rlv3 = rlv1 + rlv2;
				reg.putLong(rf1, rlv3);
				psw_cc = get_long_add_cc();
				break;
			case 0x09: // 4520 "B909" "SGR" "RRE"
				psw_check = false;
				ins_setup_rre();
				rlv1 = reg.getLong(rf1);
				rlv2 = reg.getLong(rf2);
				rlv3 = rlv1 - rlv2;
				reg.putLong(rf1, rlv3);
				psw_cc = get_long_sub_cc();
				break;
			case 0x0A: // 4530 "B90A" "ALGR" "RRE"
				psw_check = false;
				ins_setup_rre();
				rlvw = reg.getLong(rf1);
				rlv2 = reg.getLong(rf2);
				rlv1 = rlvw + rlv2;
				reg.putLong(rf1, rlv1);
				psw_cc = get_long_log_add_cc();
				break;
			case 0x0B: // 4540 "B90B" "SLGR" "RRE"
				psw_check = false;
				ins_setup_rre();
				rlvw = reg.getLong(rf1);
				rlv2 = reg.getLong(rf2);
				rlv1 = rlvw - rlv2;
				reg.putLong(rf1, rlv1);
				psw_cc = get_long_log_sub_cc();
				break;
			case 0x0C: // 4550 "B90C" "MSGR" "RRE"
				psw_check = false;
				ins_setup_rre();
				reg.putLong(rf1, reg.getLong(rf1) * reg.getLong(rf2));
				break;
			case 0x0D: // 4560 "B90D" "DSGR" "RRE"
				psw_check = false;
				ins_setup_rre();
				rlv1 = reg.getLong(rf1 + 8);
				rlv2 = reg.getLong(rf2);
				if (rlv2 != 0) {
					rlvw = rlv1 / rlv2;
				} else {
					set_psw_check(psw_pic_fx_div);
					break;
				}
				rlv1 = rlv1 - rlvw * rlv2;
				reg.putLong(rf1, rlv1);
				reg.putLong(rf1 + 8, rlvw);
				break;
			case 0x0E: // 4570 "B90E" "EREGG" "RRE"
				psw_check = false;
				ins_setup_rre();
				if (cur_pc_stk_reg <= 0) {
					set_psw_check(psw_pic_stkerr);
					return;
				}
				int pc_stk_reg_base = cur_pc_stk_reg - reg_len;
				if (rf1 > rf3) {
					while (rf1 < reg_len) {
						reg.putLong(rf1, pc_stk_reg.getLong(pc_stk_reg_base
								+ rf1));
						rf1 = rf1 + 8;
					}
					rf1 = 0;
				}
				while (rf1 <= rf3) {
					reg.putLong(rf1, pc_stk_reg.getLong(pc_stk_reg_base + rf1));
					rf1 = rf1 + 8;
				}
				break;
			case 0x0F: // 4580 "B90F" "LRVGR" "RRE"
				psw_check = false;
				ins_setup_rre();
				rflen = 7;
				while (rflen >= 0) {
					reg.put(rf1 + rflen, reg.get(rf2 + 7 - rflen));
					rflen--;
				}
				break;
			case 0x10: // 4590 "B910" "LPGFR" "RRE"
				psw_check = false;
				ins_setup_rre();
				rlv1 = reg.getInt(rf2 + 4);
				if (rlv1 < 0) {
					rlv1 = -rlv1;
				}
				reg.putLong(rf1, rlv1);
				psw_cc = get_long_comp_cc(rlv1, 0);
				break;
			case 0x11: // 4600 "B911" "LNGFR" "RRE"
				psw_check = false;
				ins_setup_rre();
				rlv1 = reg.getInt(rf2 + 4);
				if (rlv1 >= 0) {
					rlv1 = -rlv1;
				}
				reg.putLong(rf1, rlv1);
				psw_cc = get_long_comp_cc(rlv1, 0);
				break;
			case 0x12: // 4610 "B912" "LTGFR" "RRE"
				psw_check = false;
				ins_setup_rre();
				rlv1 = reg.getInt(rf2 + 4);
				reg.putLong(rf1, rlv1);
				psw_cc = get_long_comp_cc(rlv1, 0);
				break;
			case 0x13: // 4620 "B913" "LCGFR" "RRE"
				psw_check = false;
				ins_setup_rre();
				rlv1 = reg.getInt(rf2 + 4);
				rlv1 = -rlv1;
				reg.putLong(rf1, rlv1);
				psw_cc = get_long_comp_cc(rlv1, 0);
				break;
			case 0x14: // 4630 "B914" "LGFR" "RRE"
				psw_check = false;
				ins_setup_rre();
				reg.putLong(rf1, reg.getInt(rf2 + 4));
				break;
			case 0x16: // 4640 "B916" "LLGFR" "RRE"
				psw_check = false;
				ins_setup_rre();
				reg.putLong(rf1, (long) reg.getInt(rf2 + 4) & long_low32_bits);
				break;
			case 0x17: // 4650 "B917" "LLGTR" "RRE"
				psw_check = false;
				ins_setup_rre();
				reg.putLong(rf1, reg.getInt(rf2 + 4) & max_pos_int);
				break;
			case 0x18: // 4660 "B918" "AGFR" "RRE"
				psw_check = false;
				ins_setup_rre();
				rlv1 = reg.getLong(rf1);
				rlv2 = reg.getInt(rf2 + 4);
				rlv3 = rlv1 + rlv2;
				reg.putLong(rf1, rlv3);
				psw_cc = get_long_add_cc();
				break;
			case 0x19: // 4670 "B919" "SGFR" "RRE"
				psw_check = false;
				ins_setup_rre();
				rlv1 = reg.getLong(rf1);
				rlv2 = reg.getInt(rf2 + 4);
				rlv3 = rlv1 - rlv2;
				reg.putLong(rf1, rlv3);
				psw_cc = get_long_sub_cc();
				break;
			case 0x1A: // 4680 "B91A" "ALGFR" "RRE"
				psw_check = false;
				ins_setup_rre();
				rlvw = reg.getLong(rf1);
				rlv2 = ((long) reg.getInt(rf2 + 4) & long_low32_bits);
				rlv1 = rlvw + rlv2;
				reg.putLong(rf1, rlv1);
				psw_cc = get_long_log_add_cc();
				break;
			case 0x1B: // 4690 "B91B" "SLGFR" "RRE"
				psw_check = false;
				ins_setup_rre();
				rlvw = reg.getLong(rf1);
				rlv2 = (long) reg.getInt(rf2 + 4) & long_low32_bits;
				rlv1 = rlvw - rlv2;
				reg.putLong(rf1, rlv1);
				psw_cc = get_long_log_sub_cc();
				break;
			case 0x1C: // 4700 "B91C" "MSGFR" "RRE"
				psw_check = false;
				ins_setup_rre();
				reg.putLong(rf1, reg.getLong(rf1) * reg.getInt(rf2 + 4));
				break;
			case 0x1D: // 4710 "B91D" "DSGFR" "RRE"
				psw_check = false;
				ins_setup_rre();
				rlv1 = reg.getLong(rf1 + 8);
				rlv2 = (long) reg.getInt(rf2 + 4);
				if (rlv2 != 0) {
					rlvw = rlv1 / rlv2;
				} else {
					set_psw_check(psw_pic_fx_div);
					break;
				}
				rlv1 = rlv1 - rlvw * rlv2;
				reg.putLong(rf1, rlv1);
				reg.putLong(rf1 + 8, rlvw);
				break;
			case 0x1E: // 4720 "B91E" "KMAC" "RRE"
				ins_setup_rre();
				break;
			case 0x1F: // 4730 "B91F" "LRVR" "RRE"
				psw_check = false;
				ins_setup_rre();
				rflen = 3;
				while (rflen >= 0) {
					reg.put(rf1 + 4 + rflen, reg.get(rf2 + 7 - rflen));
					rflen--;
				}
				break;
			case 0x20: // 4740 "B920" "CGR" "RRE"
				psw_check = false;
				ins_setup_rre();
				psw_cc = get_long_comp_cc(reg.getLong(rf1), reg.getLong(rf2));
				break;
			case 0x21: // 4750 "B921" "CLGR" "RRE"
				psw_check = false;
				ins_setup_rre();
				psw_cc = get_long_log_comp_cc(reg.getLong(rf1), reg
						.getLong(rf2));
				break;
			case 0x25: // 4760 "B925" "STURG" "RRE"
				ins_setup_rre();
				break;
			case 0x26: // 4880 "B926" "LBR" "RRE" Z9-12
				psw_check = false;
				ins_setup_rre();
				reg.putInt(rf1 + 4, reg.get(rf2 + 7));
				break;
			case 0x27: // 4890 "B927" "LHR" "RRE" Z9-13
				psw_check = false;
				ins_setup_rre();
				reg.putInt(rf1 + 4, reg.getShort(rf2 + 6));
				break;
			case 0x2E: // 4770 "B92E" "KM" "RRE"
				ins_setup_rre();
				break;
			case 0x2F: // 4780 "B92F" "KMC" "RRE"
				ins_setup_rre();
				break;
			case 0x30: // 4790 "B930" "CGFR" "RRE"
				psw_check = false;
				ins_setup_rre();
				psw_cc = get_long_comp_cc(reg.getLong(rf1), (long) reg
						.getInt(rf2 + 4));
				break;
			case 0x31: // 4800 "B931" "CLGFR" "RRE"
				psw_check = false;
				ins_setup_rre();
				psw_cc = get_long_log_comp_cc(reg.getLong(rf1), (long) reg
						.getInt(rf2 + 4));
				break;
			case 0x3E: // 4810 "B93E" "KIMD" "RRE"
				ins_setup_rre();
				break;
			case 0x3F: // 4820 "B93F" "KLMD" "RRE"
				ins_setup_rre();
				break;
			case 0x46: // 4830 "B946" "BCTGR" "RRE"
				psw_check = false;
				ins_setup_rre();
				rlv1 = reg.getLong(rf1) - 1;
				reg.putLong(rf1, rlv1);
				if (rf2 != 0 && rlv1 != 0) {
					set_psw_loc(reg.getInt(rf2 + 4));
				}
				break;
			case 0x80: // 4840 "B980" "NGR" "RRE"
				psw_check = false;
				ins_setup_rre();
				rlv1 = reg.getLong(rf1) & reg.getLong(rf2);
				reg.putLong(rf1, rlv1);
				if (rlv1 == 0) {
					psw_cc = psw_cc0;
				} else {
					psw_cc = psw_cc1;
				}
				break;
			case 0x81: // 4850 "B981" "OGR" "RRE"
				psw_check = false;
				ins_setup_rre();
				rlv1 = reg.getLong(rf1) | reg.getLong(rf2);
				reg.putLong(rf1, rlv1);
				if (rlv1 == 0) {
					psw_cc = psw_cc0;
				} else {
					psw_cc = psw_cc1;
				}
				break;
			case 0x82: // 4860 "B982" "XGR" "RRE"
				psw_check = false;
				ins_setup_rre();
				rlv1 = reg.getLong(rf1) ^ reg.getLong(rf2);
				reg.putLong(rf1, rlv1);
				if (rlv1 == 0) {
					psw_cc = psw_cc0;
				} else {
					psw_cc = psw_cc1;
				}
				break;
			case 0x83: // 5000 "B983" "FLOGR" "RRE" Z9-14
				psw_check = false;
				ins_setup_rre();
				rlv2 = reg.getLong(rf2);
				reg.putLong(rf1, Long.numberOfLeadingZeros(rlv2));
				reg.putLong(rf1 + 8, rlv2 ^ Long.highestOneBit(rlv2));
				if (rlv2 != 0) {
					psw_cc = psw_cc2; // bit found
				} else {
					psw_cc = psw_cc0; // no bits found
				}
				break;
			case 0x84: // 5010 "B984" "LLGCR" "RRE" Z9-15
				psw_check = false;
				ins_setup_rre();
				reg.putLong(rf1, reg.get(rf2 + 7) & 0xff);
				break;
			case 0x85: // 5020 "B985" "LLGHR" "RRE" Z9-16
				psw_check = false;
				ins_setup_rre();
				reg.putLong(rf1, reg.getShort(rf2 + 6) & 0xffff);
				break;
			case 0x86: // 4870 "B986" "MLGR" "RRE"
				psw_check = false;
				ins_setup_rre();
				big_int1 = new BigInteger(get_log_bytes(reg_byte, rf1 + 8, 8)); // RPI
																				// 383
				big_int2 = new BigInteger(get_log_bytes(reg_byte, rf2, 8));
				big_int1 = big_int1.multiply(big_int2);
				zcvt_big_int_to_work_reg(big_int1, 16);
				reg.putLong(rf1, work_reg.getLong(0));
				reg.putLong(rf1 + 8, work_reg.getLong(8));
				break;
			case 0x87: // 4880 "B987" "DLGR" "RRE"
				psw_check = false;
				ins_setup_rre();
				work_reg.putLong(0, reg.getLong(rf1));
				work_reg.putLong(8, reg.getLong(rf1 + 8));
				big_int1 = new BigInteger(get_log_bytes(work_reg_byte, 0, 16));
				big_int2 = new BigInteger(get_log_bytes(reg_byte, rf2, 8));
				if (big_int2.compareTo(BigInteger.ZERO) != 0) {
					rv1 = (int) rlv1 / rv2;
				} else {
					set_psw_check(psw_pic_fx_div);
					break;
				}
				big_int_array = big_int1.divideAndRemainder(big_int2);
				zcvt_big_int_to_work_reg(big_int_array[1], 8); // get big
																// remainder
				reg.putLong(rf1, work_reg.getLong(0));
				zcvt_big_int_to_work_reg(big_int_array[0], 8); // get big
																// quotent
				reg.putLong(rf1 + 8, work_reg.getLong(0));
				break;
			case 0x88: // 4890 "B988" "ALCGR" "RRE"
				psw_check = false;
				ins_setup_rre();
				rlvw = reg.getLong(rf1);
				rlv2 = reg.getLong(rf2);
				rlv1 = rlvw + rlv2 + psw_carry[psw_cc];
				reg.putLong(rf1, rlv1);
				psw_cc = get_long_log_add_cc();
				break;
			case 0x89: // 4900 "B989" "SLBGR" "RRE"
				psw_check = false;
				ins_setup_rre();
				rlvw = reg.getLong(rf1);
				rlv2 = reg.getLong(rf2);
				rlv1 = rlvw - rlv2 - psw_borrow[psw_cc];
				reg.putLong(rf1, rlv1);
				psw_cc = get_long_log_sub_cc();
				break;
			case 0x8A: // 4910 "B98A" "CSPG" "RRE"
				ins_setup_rre();
				break;
			case 0x8D: // 4920 "B98D" "EPSW" "RRE"
				ins_setup_rre();
				break;
			case 0x8E: // 4930 "B98E" "IDTE" "RRF"
				ins_setup_rrf2();
				break;
			case 0x90: // 4940 "B990" "TRTT" "RRE"
				psw_check = false; // RPI 454
				ins_setup_rre();
				test_control = (byte) (mem_byte[psw_loc - 2] & 0x10); // eft2
																		// test
																		// char
																		// control
																		// bit
				test_byte1 = reg.get(r0 + 2);
				test_byte2 = reg.get(r0 + 3);
				xbd2_loc = reg.getInt(r1) & psw_amode;
				bd1_loc = reg.getInt(rf1 + 4) & psw_amode;
				bd2_loc = reg.getInt(rf2 + 4) & psw_amode;
				bd2_end = bd2_loc + reg.getInt(rf1 + 12);
				if (((bd2_end - bd2_loc) & 1) != 0) {
					set_psw_check(psw_pic_spec);
				}
				psw_cc = psw_cc3;
				while (psw_cc == psw_cc3) {
					if (test_control == 0 && mem_byte[bd2_loc] == test_byte1
							&& mem_byte[bd2_loc + 1] == test_byte2) {
						psw_cc = psw_cc1;
					} else {
						int index = xbd2_loc
								+ ((mem.getShort(bd2_loc) & 0xffff) << 1);
						mem_byte[bd1_loc] = mem_byte[index];
						mem_byte[bd1_loc + 1] = mem_byte[index + 1];
						bd1_loc = bd1_loc + 2;
						bd2_loc = bd2_loc + 2;
						if (bd2_loc >= bd2_end) {
							psw_cc = psw_cc0;
						}
					}
				}
				reg.putInt(rf1 + 4, bd1_loc);
				reg.putInt(rf1 + 12, bd2_end - bd2_loc); // bytes not
															// translated
				reg.putInt(rf2 + 4, bd2_loc);
				break;
			case 0x91: // 4950 "B991" "TRTO" "RRE"
				psw_check = false; // RPI 454
				ins_setup_rre();
				test_control = (byte) (mem_byte[psw_loc - 2] & 0x10); // eft2
																		// test
																		// char
																		// control
																		// bit
				test_byte1 = reg.get(r0 + 2);
				test_byte2 = reg.get(r0 + 3);
				xbd2_loc = reg.getInt(r1) & psw_amode;
				bd1_loc = reg.getInt(rf1 + 4) & psw_amode;
				bd2_loc = reg.getInt(rf2 + 4) & psw_amode;
				bd2_end = bd2_loc + reg.getInt(rf1 + 12);
				if (((bd2_end - bd2_loc) & 1) != 0) {
					set_psw_check(psw_pic_spec);
				}
				psw_cc = psw_cc3;
				while (psw_cc == psw_cc3) {
					if (test_control == 0 && mem_byte[bd2_loc] == test_byte1
							&& mem_byte[bd2_loc + 1] == test_byte2) {
						psw_cc = psw_cc1;
					} else {
						int index = xbd2_loc + (mem.getShort(bd2_loc) & 0xffff);
						mem_byte[bd1_loc] = mem_byte[index];
						bd1_loc++;
						bd2_loc = bd2_loc + 2;
						if (bd2_loc >= bd2_end) {
							psw_cc = psw_cc0;
						}
					}
				}
				reg.putInt(rf1 + 4, bd1_loc);
				reg.putInt(rf1 + 12, bd2_end - bd2_loc); // bytes not
															// translated
				reg.putInt(rf2 + 4, bd2_loc);
				break;
			case 0x92: // 4960 "B992" "TROT" "RRE"
				psw_check = false; // RPI 454
				ins_setup_rre();
				test_control = (byte) (mem_byte[psw_loc - 2] & 0x10); // eft2
																		// test
																		// char
																		// control
																		// bit
				test_byte1 = reg.get(r0 + 3);
				xbd2_loc = reg.getInt(r1) & psw_amode;
				bd1_loc = reg.getInt(rf1 + 4) & psw_amode;
				bd2_loc = reg.getInt(rf2 + 4) & psw_amode;
				bd2_end = bd2_loc + reg.getInt(rf1 + 12);
				psw_cc = psw_cc3;
				while (psw_cc == psw_cc3) {
					if (test_control == 0 && test_byte1 == mem_byte[bd2_loc]) {
						psw_cc = psw_cc1;
					} else {
						int index = xbd2_loc
								+ ((mem_byte[bd2_loc] & 0xff) << 1);
						mem_byte[bd1_loc] = mem_byte[index];
						mem_byte[bd1_loc + 1] = mem_byte[index + 1];
						bd1_loc = bd1_loc + 2;
						bd2_loc++;
						if (bd2_loc >= bd2_end) {
							psw_cc = psw_cc0;
						}
					}
				}
				reg.putInt(rf1 + 4, bd1_loc);
				reg.putInt(rf1 + 12, bd2_end - bd2_loc); // bytes not
															// translated
				reg.putInt(rf2 + 4, bd2_loc);
				break;
			case 0x93: // 4970 "B993" "TROO" "RRE"
				psw_check = false; // RPI 454
				ins_setup_rre();
				test_control = (byte) (mem_byte[psw_loc - 2] & 0x10); // eft2
																		// test
																		// char
																		// control
																		// bit
				test_byte1 = reg.get(r0 + 3);
				xbd2_loc = reg.getInt(r1) & psw_amode;
				bd1_loc = reg.getInt(rf1 + 4) & psw_amode;
				bd2_loc = reg.getInt(rf2 + 4) & psw_amode;
				bd2_end = bd2_loc + reg.getInt(rf1 + 12);
				psw_cc = psw_cc3;
				while (psw_cc == psw_cc3) {
					if (test_control == 0 && test_byte1 == mem_byte[bd2_loc]) {
						psw_cc = psw_cc1;
					} else {
						mem_byte[bd1_loc] = mem_byte[xbd2_loc
								+ (mem_byte[bd2_loc] & 0xff)];
						bd1_loc++;
						bd2_loc++;
						if (bd2_loc >= bd2_end) {
							psw_cc = psw_cc0;
						}
					}
				}
				reg.putInt(rf1 + 4, bd1_loc);
				reg.putInt(rf1 + 12, bd2_end - bd2_loc); // bytes not
															// translated
				reg.putInt(rf2 + 4, bd2_loc);
				break;
			case 0x94: // 5140 "B994" "LLCR" "RRE" Z9-17
				psw_check = false;
				ins_setup_rre();
				reg.putInt(rf1 + 4, reg.get(rf2 + 7) & 0xff);
				break;
			case 0x95: // 5150 "B995" "LLHR" "RRE" Z9-18
				psw_check = false;
				ins_setup_rre();
				reg.putInt(rf1 + 4, reg.getShort(rf2 + 6) & 0xffff);
				break;
			case 0x96: // 4980 "B996" "MLR" "RRE"
				psw_check = false;
				ins_setup_rre();
				big_int1 = new BigInteger(get_log_bytes(reg_byte, rf1 + 12, 4)); // RPI
																					// 275
				big_int2 = new BigInteger(get_log_bytes(reg_byte, rf2 + 4, 4));
				big_int1 = big_int1.multiply(big_int2);
				zcvt_big_int_to_work_reg(big_int1, 8);
				reg.putInt(rf1 + 4, work_reg.getInt(0));
				reg.putInt(rf1 + 12, work_reg.getInt(4));
				break;
			case 0x97: // 4990 "B997" "DLR" "RRE"
				psw_check = false;
				ins_setup_rre();
				work_reg.putInt(0, reg.getInt(rf1 + 4));
				work_reg.putInt(4, reg.getInt(rf1 + 12));
				big_int1 = new BigInteger(get_log_bytes(work_reg_byte, 0, 8));
				big_int2 = new BigInteger(get_log_bytes(reg_byte, rf2 + 4, 4));
				if (big_int2.compareTo(BigInteger.ZERO) != 0) {
					rv1 = (int) rlv1 / rv2;
				} else {
					set_psw_check(psw_pic_fx_div);
					break;
				}
				BigInteger[] temp_big = big_int1.divideAndRemainder(big_int2);
				zcvt_big_int_to_work_reg(temp_big[1], 4); // get big remainder
				reg.putInt(rf1 + 4, work_reg.getInt(0));
				zcvt_big_int_to_work_reg(temp_big[0], 4); // get big quotent
				reg.putInt(rf1 + 12, work_reg.getInt(0));
				break;
			case 0x98: // 5000 "B998" "ALCR" "RRE"
				psw_check = false;
				ins_setup_rre();
				rvw = reg.getInt(rf1 + 4);
				rv2 = reg.getInt(rf2 + 4);
				rv1 = rvw + rv2 + psw_carry[psw_cc];
				reg.putInt(rf1 + 4, rv1);
				psw_cc = get_int_log_add_cc();
				break;
			case 0x99: // 5010 "B999" "SLBR" "RRE"
				psw_check = false;
				ins_setup_rre();
				rvw = reg.getInt(rf1 + 4);
				rv2 = reg.getInt(rf2 + 4);
				rv1 = rvw - rv2 - psw_borrow[psw_cc];
				reg.putInt(rf1 + 4, rv1);
				psw_cc = get_int_log_sub_cc();
				break;
			case 0x9A: // 5020 "B99A" "EPAIR" "RRE"
				ins_setup_rre();
				break;
			case 0x9B: // 5030 "B99B" "ESAIR" "RRE"
				ins_setup_rre();
				break;
			case 0x9D: // 5040 "B99D" "ESEA" "RRE"
				ins_setup_rre();
				break;
			case 0x9E: // 5050 "B99E" "PTI" "RRE"
				ins_setup_rre();
				break;
			case 0x9F: // 5060 "B99F" "SSAIR" "RRE"
				ins_setup_rre();
				break;
			case 0xAA: // 5250 "B9AA" "LPTEA" "RRE" Z9-19
				ins_setup_rre();
				break;
			case 0xB0: // 5070 "B9B0" "CU14" "RRE"
				ins_setup_rre();
				break;
			case 0xB1: // 5080 "B9B1" "CU24" "RRE"
				ins_setup_rre();
				break;
			case 0xB2: // 5090 "B9B2" "CU41" "RRE"
				ins_setup_rre();
				break;
			case 0xB3: // 5100 "B9B3" "CU42" "RRE"
				ins_setup_rre();
				break;
			case 0xBE: // 5110 "B9BE" "SRSTU" "RRE"
				ins_setup_rre();
				break;
			}
			break;
		case 0xBA: // 5120 "BA" "CS" "RS"
			psw_check = false;
			ins_setup_rs();
			if (reg.getInt(rf1 + 4) == mem.getInt(bd2_loc)) {
				psw_cc = psw_cc0;
				mem.putInt(bd2_loc, reg.getInt(rf3 + 4));
			} else {
				psw_cc = psw_cc1;
				reg.putInt(rf1 + 4, mem.getInt(bd2_loc));
			}
			break;
		case 0xBB: // 5130 "BB" "CDS" "RS"
			psw_check = false;
			ins_setup_rs();
			if (reg.getInt(rf1 + 4) == mem.getInt(bd2_loc)
					&& reg.getInt(rf1 + 12) == mem.getInt(bd2_loc + 4)) {
				psw_cc = psw_cc0;
				mem.putInt(bd2_loc, reg.getInt(rf3 + 4));
				mem.putInt(bd2_loc + 4, reg.getInt(rf3 + 12));
			} else {
				psw_cc = psw_cc1;
				reg.putInt(rf1 + 4, mem.getInt(bd2_loc));
				reg.putInt(rf1 + 12, mem.getInt(bd2_loc + 4));
			}
			break;
		case 0xBD: // 5140 "BD" "CLM" "RS"
			psw_check = false;
			ins_setup_rs();
			rv1 = reg.getInt(rf1 + 4);
			exec_clm();
			break;
		case 0xBE: // 5150 "BE" "STCM" "RS"
			psw_check = false;
			ins_setup_rs();
			rv1 = reg.getInt(rf1 + 4);
			exec_stcm();
			break;
		case 0xBF: // 5160 "BF" "ICM" "RS"
			psw_check = false;
			ins_setup_rs();
			rv1 = reg.getInt(rf1 + 4);
			exec_icm();
			reg.putInt(rf1 + 4, rv1);
			break;
		}
	}

	private void ins_lt_ff() {
		/*
		 * exec instr. <= x'ff'
		 */
		switch (opcode1) {
		case 0xC0:
			opcode2 = mem_byte[psw_loc + opcode2_offset_ril] & 0x0f;
			switch (opcode2) {
			case 0x0: // 5170 "C00" "LARL" "RIL"
				psw_check = false;
				ins_setup_ril();
				reg.putInt(rf1 + 4, (psw_loc - 6 + 2 * if2) & psw_amode);
				break;
			case 0x1: // 5370 "C01" "LGFI" "RIL" Z9-20
				psw_check = false;
				ins_setup_ril();
				reg.putLong(rf1, if2);
				break;
			case 0x4: // 5180 "C04" "BRCL" "RIL"
				psw_check = false;
				ins_setup_ril();
				if ((mf1 & psw_cc) != 0) {
					set_psw_loc(psw_loc - 6 + 2 * if2);
				}
				break;
			case 0x5: // 5210 "C05" "BRASL" "RIL"
				psw_check = false;
				ins_setup_ril();
				if (ex_mode) {
					reg.putInt(rf1 + 4, ex_psw_return | psw_amode_bit);
				} else {
					reg.putInt(rf1 + 4, psw_loc | psw_amode_bit);
				}
				set_psw_loc(psw_loc - 6 + 2 * if2);
				break;
			case 0x6: // 5430 "C06" "XIHF" "RIL" Z9-21
				psw_check = false;
				ins_setup_ril();
				reg.putInt(rf1, reg.getInt(rf1) ^ if2);
				break;
			case 0x7: // 5440 "C07" "XILF" "RIL" Z9-22
				psw_check = false;
				ins_setup_ril();
				reg.putInt(rf1 + 4, reg.getInt(rf1 + 4) ^ if2);
				break;
			case 0x8: // 5450 "C08" "IIHF" "RIL" Z9-23
				psw_check = false;
				ins_setup_ril();
				reg.putInt(rf1, if2);
				break;
			case 0x9: // 5460 "C09" "IILF" "RIL" Z9-24
				psw_check = false;
				ins_setup_ril();
				reg.putInt(rf1 + 4, if2);
				break;
			case 0xA: // 5470 "C0A" "NIHF" "RIL" Z9-25
				psw_check = false;
				ins_setup_ril();
				reg.putInt(rf1, reg.getInt(rf1) & if2);
				break;
			case 0xB: // 5480 "C0B" "NILF" "RIL" Z9-26
				psw_check = false;
				ins_setup_ril();
				reg.putInt(rf1 + 4, reg.getInt(rf1 + 4) & if2);
				break;
			case 0xC: // 5490 "C0C" "OIHF" "RIL" Z9-27
				psw_check = false;
				ins_setup_ril();
				reg.putInt(rf1, reg.getInt(rf1) | if2);
				break;
			case 0xD: // 5500 "C0D" "OILF" "RIL" Z9-28
				psw_check = false;
				ins_setup_ril();
				reg.putInt(rf1 + 4, reg.getInt(rf1 + 4) | if2);
				break;
			case 0xE: // 5510 "C0E" "LLIHF" "RIL" Z9-29
				psw_check = false;
				ins_setup_ril();
				reg.putLong(rf1, ((long) if2) << 32);
				break;
			case 0xF: // 5520 "C0F" "LLILF" "RIL" Z9-30
				psw_check = false;
				ins_setup_ril();
				reg.putLong(rf1, ((long) if2) & long_low32_bits);
				break;
			}
			break;
		case 0xC2:
			opcode2 = mem_byte[psw_loc + opcode2_offset_ril] & 0x0f; // RPI202
			switch (opcode2) {
			case 0x4: // 5530 "C24" "SLGFI" "RIL" Z9-31
				psw_check = false;
				ins_setup_ril();
				rlvw = reg.getLong(rf1);
				rlv2 = (long) if2 & long_low32_bits;
				rlv1 = rlvw - rlv2;
				reg.putLong(rf1, rlv1);
				psw_cc = get_long_log_sub_cc();
				break;
			case 0x5: // 5540 "C25" "SLFI" "RIL" Z9-32
				psw_check = false;
				ins_setup_ril();
				rvw = reg.getInt(rf1 + 4);
				rv2 = if2;
				rv1 = rvw - rv2;
				reg.putInt(rf1 + 4, rv1);
				psw_cc = get_int_log_sub_cc();
				break;
			case 0x8: // "C28" "AGFI" "RIL" Z9-33
				psw_check = false;
				ins_setup_ril();
				rlv1 = reg.getLong(rf1);
				rlv2 = if2;
				rlv3 = rlv1 + rlv2;
				reg.putLong(rf1, rlv3);
				psw_cc = get_long_add_cc();
				break;
			case 0x9: // "C29" "AFI" "RIL" Z9-34
				psw_check = false;
				ins_setup_ril();
				rv1 = reg.getInt(rf1 + 4);
				rv2 = if2;
				rv3 = rv1 + rv2;
				reg.putInt(rf1 + 4, rv3);
				psw_cc = get_int_add_cc();
				break;
			case 0xA: // "C2A" "ALGFI" "RIL" Z9-35
				psw_check = false;
				ins_setup_ril();
				rlvw = reg.getLong(rf1);
				rlv2 = ((long) if2 & long_low32_bits);
				rlv1 = rlvw + rlv2;
				reg.putLong(rf1, rlv1);
				psw_cc = get_long_log_add_cc();
				break;
			case 0xB: // "C2B" "ALFI" "RIL" Z9-36
				psw_check = false;
				ins_setup_ril();
				rvw = reg.getInt(rf1 + 4);
				rv2 = if2;
				rv1 = rvw + rv2;
				reg.putInt(rf1 + 4, rv1);
				psw_cc = get_int_log_add_cc();
				break;
			case 0xC: // "C2C" "CGFI" "RIL" Z9-37
				psw_check = false;
				ins_setup_ril();
				psw_cc = get_long_comp_cc(reg.getLong(rf1), if2);
				break;
			case 0xD: // "C2D" "CFI" "RIL" Z9-38
				psw_check = false;
				ins_setup_ril();
				psw_cc = get_int_comp_cc(reg.getInt(rf1 + 4), if2);
				break;
			case 0xE: // "C2E" "CLGFI" "RIL" Z9-39
				psw_check = false;
				ins_setup_ril();
				psw_cc = get_long_log_comp_cc(reg.getLong(rf1), (long) if2
						& long_low32_bits);
				break;
			case 0xF: // "C2F" "CLFI" "RIL" Z9-40
				psw_check = false;
				ins_setup_ril();
				psw_cc = get_int_log_comp_cc(reg.getInt(rf1 + 4), if2);
				break;
			}
			break;
		case 0xC8: // 5630 "C80" "MVCOS" "SSF" Z9-41
			opcode2 = mem_byte[psw_loc + opcode2_offset_ssf] + 0x0f; // RPI202
			switch (opcode2) {
			case 0x0: // 5630 "C80" "MVCOS" "SSF" Z9-41
				ins_setup_ss();
				break;
			}
		case 0xD0: // 5230 "D0" "TRTR" "SS"
			psw_check = false;
			ins_setup_ss();
			psw_cc = psw_cc0;
			bd1_end = bd1_loc - rflen;
			boolean trt_hit = false;
			while (bd1_loc > bd1_end) {
				if (!trt_hit
						&& (mem_byte[bd2_loc + (mem_byte[bd1_loc] & 0xff)]) != 0) {
					trt_hit = true;
					if (bd1_loc - 1 == bd1_end) {
						psw_cc = psw_cc2;
					} else {
						psw_cc = psw_cc1;
					}
					reg.putInt(r1, (reg.getInt(r1 + 4) & psw_amode_high_bits)
							| bd1_loc);
					reg.put(r2 + 3, mem_byte[bd2_loc
							+ (mem_byte[bd1_loc] & 0xff)]);
				}
				bd1_loc--;
			}
			break;
		case 0xD1: // 5240 "D1" "MVN" "SS"
			psw_check = false;
			ins_setup_ss();
			while (rflen >= 8) {
				mem.putLong(bd1_loc, (mem.getLong(bd1_loc) & long_zone_ones)
						| (mem.getLong(bd2_loc) & long_num_ones));
				bd1_loc = bd1_loc + 8;
				bd2_loc = bd2_loc + 8;
				rflen = rflen - 8;
			}
			if (rflen >= 4) {
				mem.putInt(bd1_loc, (mem.getInt(bd1_loc) & 0xf0f0f0f0)
						| (mem.getInt(bd2_loc) & 0x0f0f0f0f));
				bd1_loc = bd1_loc + 4;
				bd2_loc = bd2_loc + 4;
				rflen = rflen - 4;
			}
			if (rflen >= 2) {
				mem
						.putShort(
								bd1_loc,
								(short) ((mem.getShort(bd1_loc) & (short) 0xf0f0) | (mem
										.getShort(bd2_loc) & (short) 0x0f0f)));
				bd1_loc = bd1_loc + 2;
				bd2_loc = bd2_loc + 2;
				rflen = rflen - 2;
			}
			if (rflen >= 1) {
				mem_byte[bd1_loc] = (byte) ((mem_byte[bd1_loc] & 0xf0) | (mem_byte[bd2_loc] & 0x0f));
			}
			break;
		case 0xD2: // 5250 "D2" "MVC" "SS"
			psw_check = false;
			ins_setup_ss();
			if (bd1_loc + rflen <= bd2_loc || bd2_loc + rflen <= bd1_loc) {
				System.arraycopy(mem_byte, bd2_loc, mem_byte, bd1_loc, rflen); // RPI
																				// 411
			} else if (bd2_loc + 1 == bd1_loc) {
				Arrays.fill(mem_byte, bd1_loc, bd1_loc + rflen,
						mem_byte[bd2_loc]);
			} else {
				bd1_end = bd1_loc + rflen;
				while (bd1_loc < bd1_end) {
					// destructive overlap with gap > 1
					mem_byte[bd1_loc] = mem_byte[bd2_loc];
					bd1_loc++;
					bd2_loc++;
				}
			}
			break;
		case 0xD3: // 5260 "D3" "MVZ" "SS"
			psw_check = false;
			ins_setup_ss();
			while (rflen >= 8) {
				mem.putLong(bd1_loc, (mem.getLong(bd1_loc) & long_num_ones)
						| (mem.getLong(bd2_loc) & long_zone_ones));
				bd1_loc = bd1_loc + 8;
				bd2_loc = bd2_loc + 8;
				rflen = rflen - 8;
			}
			if (rflen >= 4) {
				mem.putInt(bd1_loc, (mem.getInt(bd1_loc) & 0x0f0f0f0f)
						| (mem.getInt(bd2_loc) & 0xf0f0f0f0));
				bd1_loc = bd1_loc + 4;
				bd2_loc = bd2_loc + 4;
				rflen = rflen - 4;
			}
			if (rflen >= 2) {
				mem.putShort(bd1_loc,
						(short) ((mem.getShort(bd1_loc) & 0x0f0f) | (mem
								.getShort(bd2_loc) & 0xf0f0)));
				bd1_loc = bd1_loc + 2;
				bd2_loc = bd2_loc + 2;
				rflen = rflen - 2;
			}
			if (rflen >= 1) {
				mem_byte[bd1_loc] = (byte) ((mem_byte[bd1_loc] & 0x0f) | (mem_byte[bd2_loc] & 0xf0));
			}
			break;
		case 0xD4: // 5270 "D4" "NC" "SS"
			psw_check = false;
			ins_setup_ss();
			psw_cc = psw_cc0;
			while (rflen >= 8) {
				rlv1 = mem.getLong(bd1_loc) & mem.getLong(bd2_loc);
				mem.putLong(bd1_loc, rlv1);
				if (rlv1 != 0)
					psw_cc = psw_cc1;
				bd1_loc = bd1_loc + 8;
				bd2_loc = bd2_loc + 8;
				rflen = rflen - 8;
			}
			if (rflen >= 4) {
				rv1 = mem.getInt(bd1_loc) & mem.getInt(bd2_loc);
				mem.putInt(bd1_loc, rv1);
				if (rv1 != 0)
					psw_cc = psw_cc1;
				bd1_loc = bd1_loc + 4;
				bd2_loc = bd2_loc + 4;
				rflen = rflen - 4;
			}
			if (rflen >= 2) {
				rv1 = (mem.getShort(bd1_loc) & mem.getShort(bd2_loc)) & 0xffff;
				mem.putShort(bd1_loc, (short) rv1);
				if (rv1 != 0)
					psw_cc = psw_cc1;
				bd1_loc = bd1_loc + 2;
				bd2_loc = bd2_loc + 2;
				rflen = rflen - 2;
			}
			if (rflen >= 1) {
				rv1 = (mem_byte[bd1_loc] & mem_byte[bd2_loc]) & 0xff;
				mem.put(bd1_loc, (byte) rv1);
				if (rv1 != 0)
					psw_cc = psw_cc1;
			}
			break;
		case 0xD5: // 5280 "D5" "CLC" "SS"
			psw_check = false;
			ins_setup_ss();
			psw_cc = psw_cc_equal;
			fields_equal = true;
			while (rflen >= 8 && fields_equal) {
				if (mem.getLong(bd1_loc) == mem.getLong(bd2_loc)) {
					bd1_loc = bd1_loc + 8;
					bd2_loc = bd2_loc + 8;
					rflen = rflen - 8;
				} else {
					fields_equal = false;
				}
			}
			if (rflen >= 4 && mem.getInt(bd1_loc) == mem.getInt(bd2_loc)) {
				bd1_loc = bd1_loc + 4;
				bd2_loc = bd2_loc + 4;
				rflen = rflen - 4;
			}
			if (rflen >= 2 && mem.getShort(bd1_loc) == mem.getShort(bd2_loc)) {
				bd1_loc = bd1_loc + 2;
				bd2_loc = bd2_loc + 2;
				rflen = rflen - 2;
			}
			while (rflen > 0) {
				if (mem.get(bd1_loc) != mem.get(bd2_loc)) {
					if ((mem_byte[bd1_loc] & 0xff) > (mem_byte[bd2_loc] & 0xff)) {
						psw_cc = psw_cc_high;
					} else {
						psw_cc = psw_cc_low;
					}
					break;
				} else {
					bd1_loc++;
					bd2_loc++;
					rflen--;
				}
			}
			break;
		case 0xD6: // 5290 "D6" "OC" "SS"
			psw_check = false;
			ins_setup_ss();
			psw_cc = psw_cc0;
			while (rflen >= 8) {
				rlv1 = mem.getLong(bd1_loc) | mem.getLong(bd2_loc);
				mem.putLong(bd1_loc, rlv1);
				if (rlv1 != 0)
					psw_cc = psw_cc1;
				bd1_loc = bd1_loc + 8;
				bd2_loc = bd2_loc + 8;
				rflen = rflen - 8;
			}
			if (rflen >= 4) {
				rv1 = mem.getInt(bd1_loc) | mem.getInt(bd2_loc);
				mem.putInt(bd1_loc, rv1);
				if (rv1 != 0)
					psw_cc = psw_cc1;
				bd1_loc = bd1_loc + 4;
				bd2_loc = bd2_loc + 4;
				rflen = rflen - 4;
			}
			if (rflen >= 2) {
				rv1 = (mem.getShort(bd1_loc) | mem.getShort(bd2_loc)) & 0xffff;
				mem.putShort(bd1_loc, (short) rv1);
				if (rv1 != 0)
					psw_cc = psw_cc1;
				bd1_loc = bd1_loc + 2;
				bd2_loc = bd2_loc + 2;
				rflen = rflen - 2;
			}
			if (rflen >= 1) {
				rv1 = (mem_byte[bd1_loc] | mem_byte[bd2_loc]) & 0xff;
				mem.put(bd1_loc, (byte) rv1);
				if (rv1 != 0)
					psw_cc = psw_cc1;
			}
			break;
		case 0xD7: // 5300 "D7" "XC" "SS"
			psw_check = false;
			ins_setup_ss();
			psw_cc = psw_cc0;
			while (rflen >= 8) {
				rlv1 = mem.getLong(bd1_loc) ^ mem.getLong(bd2_loc);
				mem.putLong(bd1_loc, rlv1);
				if (rlv1 != 0)
					psw_cc = psw_cc1;
				bd1_loc = bd1_loc + 8;
				bd2_loc = bd2_loc + 8;
				rflen = rflen - 8;
			}
			if (rflen >= 4) {
				rv1 = mem.getInt(bd1_loc) ^ mem.getInt(bd2_loc);
				mem.putInt(bd1_loc, rv1);
				if (rv1 != 0)
					psw_cc = psw_cc1;
				bd1_loc = bd1_loc + 4;
				bd2_loc = bd2_loc + 4;
				rflen = rflen - 4;
			}
			if (rflen >= 2) {
				rv1 = (mem.getShort(bd1_loc) ^ mem.getShort(bd2_loc)) & 0xffff;
				mem.putShort(bd1_loc, (short) rv1);
				if (rv1 != 0)
					psw_cc = psw_cc1;
				bd1_loc = bd1_loc + 2;
				bd2_loc = bd2_loc + 2;
				rflen = rflen - 2;
			}
			if (rflen >= 1) {
				rv1 = (mem_byte[bd1_loc] ^ mem_byte[bd2_loc]) & 0xff;
				mem.put(bd1_loc, (byte) rv1);
				if (rv1 != 0)
					psw_cc = psw_cc1;
			}
			break;
		case 0xD9: // 5310 "D9" "MVCK" "SS"
			ins_setup_ss();
			break;
		case 0xDA: // 5320 "DA" "MVCP" "SS"
			ins_setup_ss();
			break;
		case 0xDB: // 5330 "DB" "MVCS" "SS"
			ins_setup_ss();
			break;
		case 0xDC: // 5340 "DC" "TR" "SS"
			psw_check = false;
			ins_setup_ss();
			bd1_end = bd1_loc + rflen;
			while (bd1_loc < bd1_end) {
				mem_byte[bd1_loc] = mem_byte[bd2_loc
						+ (mem_byte[bd1_loc] & 0xff)];
				bd1_loc++;
			}
			break;
		case 0xDD: // 5350 "DD" "TRT" "SS"
			psw_check = false;
			ins_setup_ss();
			psw_cc = psw_cc0;
			bd1_end = bd1_loc + rflen;
			while (bd1_loc < bd1_end) {
				if ((mem_byte[bd2_loc + (mem_byte[bd1_loc] & 0xff)]) != 0) {
					if (bd1_loc + 1 == bd1_end) {
						psw_cc = psw_cc2;
					} else {
						psw_cc = psw_cc1;
					}
					reg.putInt(r1, (reg.getInt(r1 + 4) & psw_amode_high_bits)
							| bd1_loc);
					reg.put(r2 + 3, mem_byte[bd2_loc
							+ (mem_byte[bd1_loc] & 0xff)]);
					bd1_end = 0; // RPI 441
				}
				bd1_loc++;
			}
			break;
		case 0xDE: // 5360 "DE" "ED" "SS"
			psw_check = false;
			ins_setup_ss();
			exec_ed_edmk(false);
			break;
		case 0xDF: // 5370 "DF" "EDMK" "SS"
			psw_check = false;
			ins_setup_ss();
			exec_ed_edmk(true);
			break;
		case 0xE1: // 5380 "E1" "PKU" "SS"
			ins_setup_ss();
			break;
		case 0xE2: // 5390 "E2" "UNPKU" "SS"
			ins_setup_ss();
			break;
		case 0xE3:
			opcode2 = mem_byte[psw_loc + opcode2_offset_rxy] & 0xff;
			switch (opcode2) {
			case 0x02: // 5810 "E302" "LTG" "RXY" Z9-42
				psw_check = false;
				ins_setup_rxy();
				rlv1 = mem.getLong(xbd2_loc);
				reg.putLong(rf1, rlv1);
				psw_cc = get_long_comp_cc(rlv1, 0);
				break;
			case 0x03: // 5400 "E303" "LRAG" "RXY"
				ins_setup_rxy();
				break;
			case 0x04: // 5410 "E304" "LG" "RXY"
				psw_check = false;
				ins_setup_rxy();
				reg.putLong(rf1, mem.getLong(xbd2_loc));
				break;
			case 0x06: // 5420 "E306" "CVBY" "RXY"
				psw_check = false;
				ins_setup_rxy();
				if (get_pd(xbd2_loc, 8)) { // RPI 305
					if (pdf_is_big) { // RPI 389
						set_psw_check(psw_pic_fx_div);
					} else {
						if (pdf_long <= max_pos_int && pdf_long >= min_neg_int) {
							reg.putInt(rf1 + 4, (int) pdf_long);
						} else {
							set_psw_check(psw_pic_fx_div);
						}
					}
				}
				break;
			case 0x08: // 5430 "E308" "AG" "RXY"
				psw_check = false;
				ins_setup_rxy();
				rlv1 = reg.getLong(rf1);
				rlv2 = mem.getLong(xbd2_loc);
				rlv3 = rlv1 + rlv2;
				reg.putLong(rf1, rlv3);
				psw_cc = get_long_add_cc();
				break;
			case 0x09: // 5440 "E309" "SG" "RXY"
				psw_check = false;
				ins_setup_rxy();
				rlv1 = reg.getLong(rf1);
				rlv2 = mem.getLong(xbd2_loc);
				rlv3 = rlv1 - rlv2;
				reg.putLong(rf1, rlv3);
				psw_cc = get_long_sub_cc();
				break;
			case 0x0A: // 5450 "E30A" "ALG" "RXY"
				psw_check = false;
				ins_setup_rxy();
				rlvw = reg.getLong(rf1);
				rlv2 = mem.getLong(xbd2_loc);
				rlv1 = rlvw + rlv2;
				reg.putLong(rf1, rlv1);
				psw_cc = get_long_log_add_cc();
				break;
			case 0x0B: // 5460 "E30B" "SLG" "RXY"
				psw_check = false;
				ins_setup_rxy();
				rlvw = reg.getLong(rf1);
				rlv2 = mem.getLong(xbd2_loc);
				rlv1 = rlvw - rlv2;
				reg.putLong(rf1, rlv1);
				psw_cc = get_long_log_sub_cc();
				break;
			case 0x0C: // 5470 "E30C" "MSG" "RXY"
				psw_check = false;
				ins_setup_rxy();
				reg.putLong(rf1, reg.getLong(rf1) * mem.getLong(xbd2_loc));
				break;
			case 0x0D: // 5480 "E30D" "DSG" "RXY"
				psw_check = false;
				ins_setup_rxy();
				rlv1 = reg.getLong(rf1 + 8);
				rlv2 = mem.getLong(xbd2_loc);
				if (rlv2 != 0) {
					rlvw = rlv1 / rlv2;
				} else {
					set_psw_check(psw_pic_fx_div);
					break;
				}
				rlv1 = rlv1 - rlvw * rlv2;
				reg.putLong(rf1, rlv1);
				reg.putLong(rf1 + 8, rlvw);
				break;
			case 0x0E: // 5490 "E30E" "CVBG" "RXY"
				psw_check = false;
				ins_setup_rxy();
				if (get_pd(xbd2_loc, 16)) { // RPI 305
					if (pdf_big_int.compareTo(bi_max_pos_long) != 1
							&& pdf_big_int.compareTo(bi_min_neg_long) != -1) {
						reg.putLong(rf1, pdf_big_int.longValue());
					} else {
						set_psw_check(psw_pic_fx_div);
					}
				} else {
					reg.putLong(rf1, pdf_long);
				}
				break;
			case 0x0F: // 5500 "E30F" "LRVG" "RXY"
				psw_check = false;
				ins_setup_rxy();
				rflen = 7;
				while (rflen >= 0) {
					reg.put(rf1 + rflen, mem_byte[xbd2_loc + 7 - rflen]);
					rflen--;
				}
				break;
			case 0x12: // 5930 "E312" "LT" "RXY" Z9-43
				psw_check = false;
				ins_setup_rxy();
				rv1 = mem.getInt(xbd2_loc);
				reg.putInt(rf1 + 4, rv1);
				psw_cc = get_int_comp_cc(rv1, 0);
				break;
			case 0x13: // 5510 "E313" "LRAY" "RXY"
				ins_setup_rxy();
				break;
			case 0x14: // 5520 "E314" "LGF" "RXY"
				psw_check = false;
				ins_setup_rxy();
				reg.putLong(rf1, mem.getInt(xbd2_loc));
				break;
			case 0x15: // 5530 "E315" "LGH" "RXY"
				psw_check = false;
				ins_setup_rxy();
				reg.putLong(rf1, mem.getShort(xbd2_loc));
				break;
			case 0x16: // 5540 "E316" "LLGF" "RXY"
				psw_check = false;
				ins_setup_rxy();
				reg.putLong(rf1, (long) mem.getInt(xbd2_loc) & long_low32_bits);
				break;
			case 0x17: // 5550 "E317" "LLGT" "RXY"
				psw_check = false;
				ins_setup_rxy();
				reg.putLong(rf1, mem.getInt(xbd2_loc) & max_pos_int); // RPI160
																		// was
																		// LOC+4
				break;
			case 0x18: // 5560 "E318" "AGF" "RXY"
				psw_check = false;
				ins_setup_rxy();
				rlv1 = reg.getLong(rf1);
				rlv2 = mem.getInt(xbd2_loc);
				rlv3 = rlv1 + rlv2;
				reg.putLong(rf1, rlv3);
				psw_cc = get_long_add_cc();
				break;
			case 0x19: // 5570 "E319" "SGF" "RXY"
				psw_check = false;
				ins_setup_rxy();
				rlv1 = reg.getLong(rf1);
				rlv2 = mem.getInt(xbd2_loc);
				rlv3 = rlv1 - rlv2;
				reg.putLong(rf1, rlv3);
				psw_cc = get_long_sub_cc();
				break;
			case 0x1A: // 5580 "E31A" "ALGF" "RXY"
				psw_check = false;
				ins_setup_rxy();
				rlvw = reg.getLong(rf1);
				rlv2 = ((long) mem.getInt(xbd2_loc) & long_low32_bits);
				rlv1 = rlvw + rlv2;
				reg.putLong(rf1, rlv1);
				psw_cc = get_long_log_add_cc();
				break;
			case 0x1B: // 5590 "E31B" "SLGF" "RXY"
				psw_check = false;
				ins_setup_rxy();
				rlvw = reg.getLong(rf1);
				rlv2 = (long) mem.getInt(xbd2_loc) & long_low32_bits;
				rlv1 = rlvw - rlv2;
				reg.putLong(rf1, rlv1);
				psw_cc = get_long_log_sub_cc();
				break;
			case 0x1C: // 5600 "E31C" "MSGF" "RXY"
				psw_check = false;
				ins_setup_rxy();
				reg.putLong(rf1, reg.getLong(rf1) * mem.getInt(xbd2_loc));
				break;
			case 0x1D: // 5610 "E31D" "DSGF" "RXY"
				psw_check = false;
				ins_setup_rxy();
				rlv1 = reg.getLong(rf1 + 8);
				rlv2 = (long) mem.getInt(xbd2_loc);
				if (rlv2 != 0) {
					rlvw = rlv1 / rlv2;
				} else {
					set_psw_check(psw_pic_fx_div);
					break;
				}
				rlv1 = rlv1 - rlvw * rlv2;
				reg.putLong(rf1, rlv1);
				reg.putLong(rf1 + 8, rlvw);
				break;
			case 0x1E: // 5620 "E31E" "LRV" "RXY"
				psw_check = false; // RPI173
				ins_setup_rxy();
				rflen = 3;
				while (rflen >= 0) {
					reg.put(rf1 + 4 + rflen, mem_byte[xbd2_loc + 3 - rflen]);
					rflen--;
				}
				break;
			case 0x1F: // 5630 "E31F" "LRVH" "RXY"
				psw_check = false;
				ins_setup_rxy();
				rflen = 1;
				while (rflen >= 0) {
					reg.put(rf1 + 6 + rflen, mem_byte[xbd2_loc + 1 - rflen]);
					rflen--;
				}
				break;
			case 0x20: // 5640 "E320" "CG" "RXY"
				psw_check = false;
				ins_setup_rxy();
				psw_cc = get_long_comp_cc(reg.getLong(rf1), mem
						.getLong(xbd2_loc));
				break;
			case 0x21: // 5650 "E321" "CLG" "RXY"
				psw_check = false;
				ins_setup_rxy();
				psw_cc = get_long_log_comp_cc(reg.getLong(rf1), mem
						.getLong(xbd2_loc));
				break;
			case 0x24: // 5660 "E324" "STG" "RXY"
				psw_check = false;
				ins_setup_rxy();
				mem.putLong(xbd2_loc & psw_amode, reg.getLong(rf1));
				break;
			case 0x26: // 5670 "E326" "CVDY" "RXY"
				psw_check = false;
				ins_setup_rxy();
				big_int = BigInteger.valueOf(reg.getInt(rf1 + 4));
				put_pd(mem_byte, xbd2_loc, 8);
				break;
			case 0x2E: // 5680 "E32E" "CVDG" "RXY"
				psw_check = false;
				ins_setup_rxy();
				pdf_is_big = false; // RPI 389
				pdf_long = reg.getLong(rf1);
				put_pd(mem_byte, xbd2_loc, 16);
				break;
			case 0x2F: // 5690 "E32F" "STRVG" "RXY"
				psw_check = false;
				ins_setup_rxy();
				rflen = 7;
				while (rflen >= 0) {
					mem_byte[xbd2_loc + rflen] = reg.get(rf1 + 7 - rflen);
					rflen--;
				}
				break;
			case 0x30: // 5700 "E330" "CGF" "RXY"
				psw_check = false;
				ins_setup_rxy();
				psw_cc = get_long_comp_cc(reg.getLong(rf1), (long) mem
						.getInt(xbd2_loc));
				break;
			case 0x31: // 5710 "E331" "CLGF" "RXY"
				psw_check = false;
				ins_setup_rxy();
				psw_cc = get_long_log_comp_cc(reg.getLong(rf1), (long) mem
						.getInt(xbd2_loc)
						& long_low32_bits);
				break;
			case 0x3E: // 5720 "E33E" "STRV" "RXY"
				psw_check = false;
				ins_setup_rxy();
				rflen = 3;
				while (rflen >= 0) {
					mem_byte[xbd2_loc + rflen] = reg.get(rf1 + 7 - rflen);
					rflen--;
				}
				break;
			case 0x3F: // 5730 "E33F" "STRVH" "RXY"
				psw_check = false;
				ins_setup_rxy();
				rflen = 1;
				while (rflen >= 0) {
					mem_byte[xbd2_loc + rflen] = reg.get(rf1 + 7 - rflen);
					rflen--;
				}
				break;
			case 0x46: // 5740 "E346" "BCTG" "RXY"
				psw_check = false;
				ins_setup_rxy();
				rlv1 = reg.getLong(rf1) - 1;
				reg.putLong(rf1, rlv1);
				if (rlv1 != 0) {
					set_psw_loc(xbd2_loc);
				}
				break;
			case 0x50: // 5750 "E350" "STY" "RXY"
				ins_setup_rxy();
				psw_check = false;
				mem.putInt(xbd2_loc, reg.getInt(rf1 + 4));
				break;
			case 0x51: // 5760 "E351" "MSY" "RXY"
				psw_check = false;
				ins_setup_rxy();
				reg.putInt(rf1 + 4, reg.getInt(rf1 + 4) * mem.getInt(xbd2_loc));
				break;
			case 0x54: // 5770 "E354" "NY" "RXY"
				psw_check = false;
				ins_setup_rxy();
				rv1 = reg.getInt(rf1 + 4) & mem.getInt(xbd2_loc);
				reg.putInt(rf1 + 4, rv1);
				if (rv1 == 0) {
					psw_cc = psw_cc0;
				} else {
					psw_cc = psw_cc1;
				}
				break;
			case 0x55: // 5780 "E355" "CLY" "RXY"
				psw_check = false;
				ins_setup_rxy();
				psw_cc = get_int_log_comp_cc(reg.getInt(rf1 + 4), mem
						.getInt(xbd2_loc));
				break;
			case 0x56: // 5790 "E356" "OY" "RXY"
				psw_check = false;
				ins_setup_rxy();
				rv1 = reg.getInt(rf1 + 4) | mem.getInt(xbd2_loc);
				reg.putInt(rf1 + 4, rv1);
				if (rv1 == 0) {
					psw_cc = psw_cc0;
				} else {
					psw_cc = psw_cc1;
				}
				break;
			case 0x57: // 5800 "E357" "XY" "RXY"
				psw_check = false;
				ins_setup_rxy();
				rv1 = reg.getInt(rf1 + 4) ^ mem.getInt(xbd2_loc);
				reg.putInt(rf1 + 4, rv1);
				if (rv1 == 0) {
					psw_cc = psw_cc0;
				} else {
					psw_cc = psw_cc1;
				}
				break;
			case 0x58: // 5810 "E358" "LY" "RXY"
				psw_check = false;
				ins_setup_rxy();
				reg.putInt(rf1 + 4, mem.getInt(xbd2_loc));
				break;
			case 0x59: // 5820 "E359" "CY" "RXY"
				psw_check = false;
				ins_setup_rxy();
				psw_cc = get_int_comp_cc(reg.getInt(rf1 + 4), mem
						.getInt(xbd2_loc));
				break;
			case 0x5A: // 5830 "E35A" "AY" "RXY"
				psw_check = false;
				ins_setup_rxy();
				rv1 = reg.getInt(rf1 + 4);
				rv2 = mem.getInt(xbd2_loc);
				rv3 = rv1 + rv2;
				reg.putInt(rf1 + 4, rv3);
				psw_cc = get_int_add_cc();
				break;
			case 0x5B: // 5840 "E35B" "SY" "RXY"
				psw_check = false;
				ins_setup_rxy();
				rv1 = reg.getInt(rf1 + 4);
				rv2 = mem.getInt(xbd2_loc);
				rv3 = rv1 - rv2;
				reg.putInt(rf1 + 4, rv3);
				psw_cc = get_int_sub_cc();
				break;
			case 0x5E: // 5850 "E35E" "ALY" "RXY"
				psw_check = false;
				ins_setup_rxy();
				rvw = reg.getInt(rf1 + 4);
				rv2 = mem.getInt(xbd2_loc);
				rv1 = rvw + rv2;
				reg.putInt(rf1 + 4, rv1);
				psw_cc = get_int_log_add_cc();
				break;
			case 0x5F: // 5860 "E35F" "SLY" "RXY"
				psw_check = false;
				ins_setup_rxy();
				rvw = reg.getInt(rf1 + 4);
				rv2 = mem.getInt(xbd2_loc);
				rv1 = rvw - rv2;
				reg.putInt(rf1 + 4, rv1);
				psw_cc = get_int_log_sub_cc();
				break;
			case 0x70: // 5870 "E370" "STHY" "RXY"
				psw_check = false;
				ins_setup_rxy();
				mem.putShort(xbd2_loc, (short) reg.getInt(rf1 + 4));
				break;
			case 0x71: // 5880 "E371" "LAY" "RXY"
				psw_check = false;
				ins_setup_rxy();
				reg.putInt(rf1 + 4, xbd2_loc & psw_amode);
				break;
			case 0x72: // 5890 "E372" "STCY" "RXY"
				psw_check = false;
				ins_setup_rxy();
				mem_byte[xbd2_loc] = reg.get(rf1 + 4 + 3);
				break;
			case 0x73: // 5900 "E373" "ICY" "RXY"
				psw_check = false;
				ins_setup_rxy();
				reg.put(rf1 + 7, mem_byte[xbd2_loc]);
				break;
			case 0x76: // 5910 "E376" "LB" "RXY"
				psw_check = false;
				ins_setup_rxy();
				reg.putInt(rf1 + 4, mem_byte[xbd2_loc]);
				break;
			case 0x77: // 5920 "E377" "LGB" "RXY"
				psw_check = false;
				ins_setup_rxy();
				reg.putLong(rf1, mem_byte[xbd2_loc]);
				break;
			case 0x78: // 5930 "E378" "LHY" "RXY"
				psw_check = false;
				ins_setup_rxy();
				reg.putInt(rf1 + 4, mem.getShort(xbd2_loc));
				break;
			case 0x79: // 5940 "E379" "CHY" "RXY"
				psw_check = false;
				ins_setup_rxy();
				psw_cc = get_int_comp_cc(reg.getInt(rf1 + 4), mem
						.getShort(xbd2_loc));
				break;
			case 0x7A: // 5950 "E37A" "AHY" "RXY"
				psw_check = false;
				ins_setup_rxy();
				rv1 = reg.getInt(rf1 + 4);
				rv2 = mem.getShort(xbd2_loc);
				rv3 = rv1 + rv2;
				reg.putInt(rf1 + 4, rv3);
				psw_cc = get_int_add_cc();
				break;
			case 0x7B: // 5960 "E37B" "SHY" "RXY"
				psw_check = false;
				ins_setup_rxy();
				rv1 = reg.getInt(rf1 + 4);
				rv2 = mem.getShort(xbd2_loc);
				rv3 = rv1 - rv2;
				reg.putInt(rf1 + 4, rv3);
				psw_cc = get_int_sub_cc();
				break;
			case 0x80: // 5970 "E380" "NG" "RXY"
				psw_check = false;
				ins_setup_rxy();
				rlv1 = reg.getLong(rf1) & mem.getLong(xbd2_loc);
				reg.putLong(rf1, rlv1);
				if (rlv1 == 0) {
					psw_cc = psw_cc0;
				} else {
					psw_cc = psw_cc1;
				}
				break;
			case 0x81: // 5980 "E381" "OG" "RXY"
				psw_check = false;
				ins_setup_rxy();
				rlv1 = reg.getLong(rf1) | mem.getLong(xbd2_loc);
				reg.putLong(rf1, rlv1);
				if (rlv1 == 0) {
					psw_cc = psw_cc0;
				} else {
					psw_cc = psw_cc1;
				}
				break;
			case 0x82: // 5990 "E382" "XG" "RXY"
				psw_check = false;
				ins_setup_rxy();
				rlv1 = reg.getLong(rf1) ^ mem.getLong(xbd2_loc);
				reg.putLong(rf1, rlv1);
				if (rlv1 == 0) {
					psw_cc = psw_cc0;
				} else {
					psw_cc = psw_cc1;
				}
				break;
			case 0x86: // 6000 "E386" "MLG" "RXY"
				psw_check = false;
				ins_setup_rxy();
				big_int1 = new BigInteger(get_log_bytes(reg_byte, rf1 + 8, 8)); // RPI
																				// 383
				big_int2 = new BigInteger(get_log_bytes(mem_byte, xbd2_loc, 8));
				big_int1 = big_int1.multiply(big_int2);
				zcvt_big_int_to_work_reg(big_int1, 16);
				reg.putLong(rf1, work_reg.getLong(0));
				reg.putLong(rf1 + 8, work_reg.getLong(8));
				break;
			case 0x87: // 6010 "E387" "DLG" "RXY"
				psw_check = false;
				ins_setup_rxy();
				work_reg.putLong(0, reg.getLong(rf1));
				work_reg.putLong(8, reg.getLong(rf1 + 8));
				big_int1 = new BigInteger(get_log_bytes(work_reg_byte, 0, 16));
				big_int2 = new BigInteger(get_log_bytes(mem_byte, xbd2_loc, 8));
				if (big_int2.compareTo(BigInteger.ZERO) != 0) {
					rv1 = (int) rlv1 / rv2;
				} else {
					set_psw_check(psw_pic_fx_div);
					break;
				}
				big_int_array = big_int1.divideAndRemainder(big_int2);
				zcvt_big_int_to_work_reg(big_int_array[1], 8); // get big
																// remainder
				reg.putLong(rf1, work_reg.getLong(0));
				zcvt_big_int_to_work_reg(big_int_array[0], 8); // get big
																// quotent
				reg.putLong(rf1 + 8, work_reg.getLong(0));
				break;
			case 0x88: // 6020 "E388" "ALCG" "RXY"
				psw_check = false;
				ins_setup_rxy();
				rlvw = reg.getLong(rf1);
				rlv2 = mem.getLong(xbd2_loc);
				rlv1 = rlvw + rlv2 + psw_carry[psw_cc];
				reg.putLong(rf1, rlv1);
				psw_cc = get_long_log_add_cc();
				break;
			case 0x89: // 6030 "E389" "SLBG" "RXY"
				psw_check = false;
				ins_setup_rxy();
				rlvw = reg.getLong(rf1);
				rlv2 = mem.getLong(xbd2_loc);
				rlv1 = rlvw - rlv2 - psw_borrow[psw_cc];
				reg.putLong(rf1, rlv1);
				psw_cc = get_long_log_sub_cc();
				break;
			case 0x8E: // 6040 "E38E" "STPQ" "RXY"
				psw_check = false;
				ins_setup_rxy();
				mem.putLong(xbd2_loc, reg.getLong(rf1));
				mem.putLong(xbd2_loc + 8, reg.getLong(rf1 + 8));
				break;
			case 0x8F: // 6050 "E38F" "LPQ" "RXY"
				psw_check = false;
				ins_setup_rxy();
				reg.putLong(rf1, mem.getLong(xbd2_loc));
				reg.putLong(rf1 + 8, mem.getLong(xbd2_loc + 8));
				break;
			case 0x90: // 6060 "E390" "LLGC" "RXY"
				psw_check = false;
				ins_setup_rxy();
				reg.putLong(rf1, mem_byte[xbd2_loc] & 0xff);
				break;
			case 0x91: // 6070 "E391" "LLGH" "RXY"
				psw_check = false;
				ins_setup_rxy();
				reg.putLong(rf1, mem.getShort(xbd2_loc) & 0xffff);
				break;
			case 0x94: // 6510 "E394" "LLC" "RXY" Z9-44
				psw_check = false;
				ins_setup_rxy();
				reg.putInt(rf1 + 4, mem.get(xbd2_loc) & 0xff);
				break;
			case 0x95: // 6520 "E395" "LLH" "RXY" Z9-45
				psw_check = false;
				ins_setup_rxy();
				reg.putInt(rf1 + 4, mem.getShort(xbd2_loc) & 0xffff);
				break;
			case 0x96: // 6080 "E396" "ML" "RXY"
				psw_check = false;
				ins_setup_rxy();
				big_int1 = new BigInteger(get_log_bytes(reg_byte, rf1 + 12, 4)); // RPI
																					// 275
				big_int2 = new BigInteger(get_log_bytes(mem_byte, xbd2_loc, 4));
				big_int1 = big_int1.multiply(big_int2);
				zcvt_big_int_to_work_reg(big_int1, 8);
				reg.putInt(rf1 + 4, work_reg.getInt(0));
				reg.putInt(rf1 + 12, work_reg.getInt(4));
				break;
			case 0x97: // 6090 "E397" "DL" "RXY"
				psw_check = false;
				ins_setup_rxy();
				work_reg.putInt(0, reg.getInt(rf1 + 4));
				work_reg.putInt(4, reg.getInt(rf1 + 12));
				big_int1 = new BigInteger(get_log_bytes(work_reg_byte, 0, 8));
				big_int2 = new BigInteger(get_log_bytes(mem_byte, xbd2_loc, 4));
				if (big_int2.compareTo(BigInteger.ZERO) != 0) {
					rv1 = (int) rlv1 / rv2;
				} else {
					set_psw_check(psw_pic_fx_div);
					break;
				}
				big_int_array = big_int1.divideAndRemainder(big_int2);
				zcvt_big_int_to_work_reg(big_int_array[1], 4); // get big
																// remainder
				reg.putInt(rf1 + 4, work_reg.getInt(0));
				zcvt_big_int_to_work_reg(big_int_array[0], 4); // get big
																// quotent
				reg.putInt(rf1 + 12, work_reg.getInt(0));
				break;
			case 0x98: // 6100 "E398" "ALC" "RXY"
				psw_check = false;
				ins_setup_rxy();
				rvw = reg.getInt(rf1 + 4);
				rv2 = mem.getInt(xbd2_loc);
				rv1 = rvw + rv2 + psw_carry[psw_cc];
				reg.putInt(rf1 + 4, rv1);
				psw_cc = get_int_log_add_cc();
				break;
			case 0x99: // 6110 "E399" "SLB" "RXY"
				psw_check = false;
				ins_setup_rxy();
				rvw = reg.getInt(rf1 + 4);
				rv2 = mem.getInt(xbd2_loc);
				rv1 = rvw - rv2 - psw_borrow[psw_cc];
				reg.putInt(rf1 + 4, rv1);
				psw_cc = get_int_log_sub_cc();
				break;
			}
			break;
		case 0xE5:
			opcode2 = mem_byte[psw_loc + opcode2_offset_sse] & 0xff;
			switch (opcode2) {
			case 0x00: // 6120 "E500" "LASP" "SSE"
				ins_setup_sse();
				break;
			case 0x01: // 6130 "E501" "TPROT" "SSE"
				ins_setup_sse();
				break;
			case 0x02: // 6140 "E502" "STRAG" "SSE"
				ins_setup_sse();
				break;
			case 0x0E: // 6150 "E50E" "MVCSK" "SSE"
				ins_setup_sse();
				break;
			case 0x0F: // 6160 "E50F" "MVCDK" "SSE"
				ins_setup_sse();
				break;
			}
			break;
		case 0xE8: // 6170 "E8" "MVCIN" "SS"
			psw_check = false;
			ins_setup_ss();
			rflen--;
			while (rflen >= 0) {
				mem_byte[bd1_loc] = mem_byte[bd2_loc + rflen];
				bd1_loc++;
				rflen--;
			}
			break;
		case 0xE9: // 6180 "E9" "PKA" "SS"
			psw_check = false;
			ins_setup_ss();
			bd1_end = bd1_loc - 1;
			bd2_end = bd2_loc - 1;
			rflen1 = 16;
			if (rflen <= 32) {
				rflen2 = rflen;
			} else {
				set_psw_check(psw_pic_spec);
				break;
			}
			bd1_loc = bd1_loc + rflen1 - 1;
			bd2_loc = bd2_loc + rflen2 - 1;
			pdf_next_out = 0xc;
			while (bd1_loc > bd1_end) {
				if (bd2_loc > bd2_end) {
					pdf_next_in = mem_byte[bd2_loc];
					bd2_loc--;
				} else {
					pdf_next_in = 0;
				}
				mem_byte[bd1_loc] = (byte) (pdf_next_out | ((pdf_next_in & 0xf) << 4));
				bd1_loc--;
				if (bd2_loc > bd2_end) {
					pdf_next_out = (byte) (mem_byte[bd2_loc] & 0xf);
					bd2_loc--;
				} else {
					pdf_next_out = 0;
				}
			}
			break;
		case 0xEA: // 6190 "EA" "UNPKA" "SS"
			psw_check = false;
			ins_setup_ss();
			if (rflen <= 32) {
				rflen1 = rflen;
			} else {
				set_psw_check(psw_pic_spec);
				break;
			}
			rflen2 = 16;
			bd1_end = bd1_loc - 1;
			bd2_end = bd2_loc - 1;
			bd1_loc = bd1_loc + rflen1 - 1;
			bd2_loc = bd2_loc + rflen2 - 1;
			int sign_byte = mem_byte[bd2_loc];
			int sign_nib = sign_byte & 0xf;
			mem_byte[bd1_loc] = (byte) (((sign_byte & 0xf0) >> 4) | 0x30);
			bd1_loc--;
			bd2_loc--;
			pdf_next_right = true;
			while (bd1_loc > bd1_end) {
				if (pdf_next_right) {
					if (bd2_loc > bd2_end) {
						pdf_next_right = false;
						pdf_next_in = mem_byte[bd2_loc];
						bd2_loc--;
					} else {
						pdf_next_in = 0;
					}
					mem_byte[bd1_loc] = (byte) ((pdf_next_in & 0xf) | 0x30);
					bd1_loc--;
				} else {
					pdf_next_right = true;
					mem_byte[bd1_loc] = (byte) (((pdf_next_in & 0xf0) >> 4) | 0x30);
					bd1_loc--;
				}
			}
			switch (sign_nib) {
			case 0xa:
			case 0xc:
			case 0xe:
			case 0xf:
				psw_cc = psw_cc0;
				break;
			case 0xb:
			case 0xd:
				psw_cc = psw_cc1;
				break;
			default:
				psw_cc = psw_cc3;
			}
			break;
		case 0xEB:
			opcode2 = mem_byte[psw_loc + opcode2_offset_rsy] & 0xff;
			switch (opcode2) {
			case 0x04: // 6200 "EB04" "LMG" "RSY"
				psw_check = false;
				ins_setup_rsy();
				if (rf1 > rf3) {
					while (rf1 < reg_len) {
						reg.putLong(rf1, mem.getLong(bd2_loc));
						bd2_loc = bd2_loc + 8;
						rf1 = rf1 + 8;
					}
					rf1 = 0;
				}
				while (rf1 <= rf3) {
					reg.putLong(rf1, mem.getLong(bd2_loc));
					bd2_loc = bd2_loc + 8;
					rf1 = rf1 + 8;
				}
				break;
			case 0x0A: // 6210 "EB0A" "SRAG" "RSY"
				psw_check = false;
				ins_setup_rsy();
				rlv1 = reg.getLong(rf3) >> (bd2_loc & 0x3f);
				reg.putLong(rf1, rlv1); // RPI 398
				psw_cc = get_long_comp_cc(rlv1, 0);
				break;
			case 0x0B: // 6220 "EB0B" "SLAG" "RSY"
				psw_check = false;
				ins_setup_rsy();
				reg.putLong(rf1, get_sla64(reg.getLong(rf3), bd2_loc & 0x3f));
				break;
			case 0x0C: // 6230 "EB0C" "SRLG" "RSY"
				psw_check = false;
				ins_setup_rsy();
				reg.putLong(rf1, reg.getLong(rf3) >>> (bd2_loc & 0x3f));
				break;
			case 0x0D: // 6240 "EB0D" "SLLG" "RSY"
				psw_check = false;
				ins_setup_rsy();
				reg.putLong(rf1, reg.getLong(rf3) << (bd2_loc & 0x3f));
				break;
			case 0x0F: // 6250 "EB0F" "TRACG" "RSY"
				ins_setup_rsy();
				break;
			case 0x14: // 6260 "EB14" "CSY" "RSY"
				psw_check = false;
				ins_setup_rsy();
				if (reg.getInt(rf1 + 4) == mem.getInt(bd2_loc)) {
					psw_cc = psw_cc0;
					mem.putInt(bd2_loc, reg.getInt(rf3 + 4));
				} else {
					psw_cc = psw_cc1;
					reg.putInt(rf1 + 4, mem.getInt(bd2_loc));
				}
				break;
			case 0x1C: // 6270 "EB1C" "RLLG" "RSY"
				psw_check = false;
				ins_setup_rsy();
				big_int1 = new BigInteger(get_log_bytes(reg_byte, rf3, 8));
				big_int1 = big_int1.multiply(BigInteger.valueOf(2).pow(
						bd2_loc & 0x3f));
				zcvt_big_int_to_work_reg(big_int1, 16);
				reg.putLong(rf1, work_reg.getLong(8) | work_reg.getLong(0));
				break;
			case 0x1D: // 6280 "EB1D" "RLL" "RSY"
				psw_check = false;
				ins_setup_rsy();
				rlv1 = ((long) reg.getInt(rf3 + 4) & long_low32_bits) << (bd2_loc & 0x3f);
				reg.putInt(rf1 + 4,
						(int) ((rlv1 & long_low32_bits) | (rlv1 >>> 32)));
				break;
			case 0x20: // 6290 "EB20" "CLMH" "RSY"
				psw_check = false;
				ins_setup_rsy();
				rv1 = reg.getInt(rf1);
				exec_clm();
				break;
			case 0x21: // 6300 "EB21" "CLMY" "RSY"
				psw_check = false;
				ins_setup_rsy();
				rv1 = reg.getInt(rf1 + 4);
				exec_clm();
				break;
			case 0x24: // 6310 "EB24" "STMG" "RSY"
				psw_check = false;
				ins_setup_rsy();
				if (rf1 > rf3) {
					while (rf1 < reg_len) {
						mem.putLong(bd2_loc, reg.getLong(rf1));
						bd2_loc = bd2_loc + 8;
						rf1 = rf1 + 8;
					}
					rf1 = 0;
				}
				while (rf1 <= rf3) {
					mem.putLong(bd2_loc, reg.getLong(rf1));
					bd2_loc = bd2_loc + 8;
					rf1 = rf1 + 8;
				}
				break;
			case 0x25: // 6320 "EB25" "STCTG" "RSY"
				ins_setup_rsy();
				break;
			case 0x26: // 6330 "EB26" "STMH" "RSY"
				psw_check = false;
				ins_setup_rsy();
				if (rf1 > rf3) {
					while (rf1 < reg_len) {
						mem.putInt(bd2_loc, reg.getInt(rf1));
						bd2_loc = bd2_loc + 4;
						rf1 = rf1 + 8;
					}
					rf1 = 0;
				}
				while (rf1 <= rf3) {
					mem.putInt(bd2_loc, reg.getInt(rf1));
					bd2_loc = bd2_loc + 4;
					rf1 = rf1 + 8;
				}
				break;
			case 0x2C: // 6340 "EB2C" "STCMH" "RSY"
				psw_check = false;
				ins_setup_rsy();
				rv1 = reg.getInt(rf1);
				exec_stcm();
				break;
			case 0x2D: // 6350 "EB2D" "STCMY" "RSY"
				psw_check = false;
				ins_setup_rsy();
				rv1 = reg.getInt(rf1 + 4);
				exec_stcm();
				break;
			case 0x2F: // 6360 "EB2F" "LCTLG" "RSY"
				ins_setup_rsy();
				break;
			case 0x30: // 6370 "EB30" "CSG" "RSY"
				psw_check = false;
				ins_setup_rsy();
				if (reg.getLong(rf1) == mem.getLong(bd2_loc)) {
					psw_cc = psw_cc0;
					mem.putLong(bd2_loc, reg.getLong(rf3));
				} else {
					psw_cc = psw_cc1;
					reg.putLong(rf1, mem.getLong(bd2_loc));
				}
				break;
			case 0x31: // 6380 "EB31" "CDSY" "RSY"
				psw_check = false;
				ins_setup_rsy();
				if (reg.getInt(rf1 + 4) == mem.getInt(bd2_loc)
						&& reg.getInt(rf1 + 12) == mem.getInt(bd2_loc + 4)) {
					psw_cc = psw_cc0;
					mem.putInt(bd2_loc, reg.getInt(rf3 + 4));
					mem.putInt(bd2_loc + 4, reg.getInt(rf3 + 12));
				} else {
					psw_cc = psw_cc1;
					reg.putInt(rf1 + 4, mem.getInt(bd2_loc));
					reg.putInt(rf1 + 12, mem.getInt(bd2_loc + 4));
				}
				break;
			case 0x3E: // 6390 "EB3E" "CDSG" "RSY"
				psw_check = false;
				ins_setup_rsy();
				if (reg.getLong(rf1) == mem.getLong(bd2_loc)
						&& reg.getLong(rf1 + 8) == mem.getLong(bd2_loc + 8)) {
					psw_cc = psw_cc0;
					mem.putLong(bd2_loc, reg.getLong(rf3));
					mem.putLong(bd2_loc + 8, reg.getLong(rf3 + 8));
				} else {
					psw_cc = psw_cc1;
					reg.putLong(rf1, mem.getLong(bd2_loc));
					reg.putLong(rf1 + 8, mem.getLong(bd2_loc + 8));
				}
				break;
			case 0x44: // 6400 "EB44" "BXHG" "RSY"
				psw_check = false;
				ins_setup_rsy();
				rlv3 = reg.getLong(rf3);
				rlv1 = reg.getLong(rf1) + rlv3;
				reg.putLong(rf1, rlv1);
				if (rf3 == ((rf3 >> 4) << 4)) {
					rlv3 = reg.getLong(rf3 + 8);
				}
				if (rlv1 > rlv3) {
					set_psw_loc(bd2_loc);
				}
				break;
			case 0x45: // 6410 "EB45" "BXLEG" "RSY"
				psw_check = false;
				ins_setup_rsy();
				rlv3 = reg.getLong(rf3);
				rlv1 = reg.getLong(rf1) + rlv3;
				reg.putLong(rf1, rlv1);
				if (rf3 == ((rf3 >> 4) << 4)) {
					rlv3 = reg.getLong(rf3 + 8);
				}
				if (rlv1 <= rlv3) {
					set_psw_loc(bd2_loc);
				}
				break;
			case 0x51: // 6420 "EB51" "TMY" "SIY"
				psw_check = false;
				ins_setup_siy();
				rv1 = mem_byte[bd1_loc] & 0xff;
				rvw = rv1 & if2;
				if (rvw == 0) {
					psw_cc = psw_cc0;
				} else if (rvw == rv1) {
					psw_cc = psw_cc3;
				} else {
					psw_cc = psw_cc1;
				}
				break;
			case 0x52: // 6430 "EB52" "MVIY" "SIY"
				psw_check = false;
				ins_setup_siy();
				mem_byte[bd1_loc] = (byte) if2;
				break;
			case 0x54: // 6440 "EB54" "NIY" "SIY"
				psw_check = false;
				ins_setup_siy();
				sv1 = mem_byte[bd1_loc] & if2;
				mem_byte[bd1_loc] = (byte) sv1;
				if (sv1 == 0) {
					psw_cc = psw_cc0;
				} else {
					psw_cc = psw_cc1;
				}
				break;
			case 0x55: // 6450 "EB55" "CLIY" "SIY"
				psw_check = false;
				ins_setup_siy();
				psw_cc = get_int_comp_cc((mem_byte[bd1_loc] & 0xff), if2);
				break;
			case 0x56: // 6460 "EB56" "OIY" "SIY"
				psw_check = false;
				ins_setup_siy();
				sv1 = mem_byte[bd1_loc] | if2;
				mem_byte[bd1_loc] = (byte) sv1;
				if (sv1 == 0) {
					psw_cc = psw_cc0;
				} else {
					psw_cc = psw_cc1;
				}
				break;
			case 0x57: // 6470 "EB57" "XIY" "SIY"
				psw_check = false;
				ins_setup_siy();
				sv1 = mem_byte[bd1_loc] ^ if2;
				mem_byte[bd1_loc] = (byte) sv1;
				if (sv1 == 0) {
					psw_cc = psw_cc0;
				} else {
					psw_cc = psw_cc1;
				}
				break;
			case 0x80: // 6480 "EB80" "ICMH" "RSY"
				psw_check = false;
				ins_setup_rsy();
				rv1 = reg.getInt(rf1);
				exec_icm();
				reg.putInt(rf1, rv1);
				break;
			case 0x81: // 6490 "EB81" "ICMY" "RSY"
				psw_check = false;
				ins_setup_rsy();
				rv1 = reg.getInt(rf1 + 4);
				exec_icm();
				reg.putInt(rf1 + 4, rv1);
				break;
			case 0x8E: // 6500 "EB8E" "MVCLU" "RSY"
				ins_setup_rsy();
				break;
			case 0x8F: // 6510 "EB8F" "CLCLU" "RSY"
				ins_setup_rsy();
				break;
			case 0x90: // 6520 "EB90" "STMY" "RSY"
				psw_check = false;
				ins_setup_rsy();
				if (rf1 > rf3) {
					while (rf1 < reg_len) {
						mem.putInt(bd2_loc, reg.getInt(rf1 + 4));
						bd2_loc = bd2_loc + 4;
						rf1 = rf1 + 8;
					}
					rf1 = 0;
				}
				while (rf1 <= rf3) {
					mem.putInt(bd2_loc, reg.getInt(rf1 + 4));
					bd2_loc = bd2_loc + 4;
					rf1 = rf1 + 8;
				}
				break;
			case 0x96: // 6530 "EB96" "LMH" "RSY"
				psw_check = false;
				ins_setup_rsy();
				if (rf1 > rf3) {
					while (rf1 < reg_len) {
						reg.putInt(rf1, mem.getInt(bd2_loc));
						bd2_loc = bd2_loc + 4;
						rf1 = rf1 + 8;
					}
					rf1 = 0;
				}
				while (rf1 <= rf3) {
					reg.putInt(rf1, mem.getInt(bd2_loc));
					bd2_loc = bd2_loc + 4;
					rf1 = rf1 + 8;
				}
				break;
			case 0x98: // 6540 "EB98" "LMY" "RSY"
				psw_check = false;
				ins_setup_rsy();
				if (rf1 > rf3) {
					while (rf1 < reg_len) {
						reg.putInt(rf1 + 4, mem.getInt(bd2_loc));
						bd2_loc = bd2_loc + 4;
						rf1 = rf1 + 8;
					}
					rf1 = 0;
				}
				while (rf1 <= rf3) {
					reg.putInt(rf1 + 4, mem.getInt(bd2_loc));
					bd2_loc = bd2_loc + 4;
					rf1 = rf1 + 8;
				}
				break;
			case 0x9A: // 6550 "EB9A" "LAMY" "RSY"
				ins_setup_rsy();
				break;
			case 0x9B: // 6560 "EB9B" "STAMY" "RSY"
				ins_setup_rsy();
				break;
			case 0xC0: // 6570 "EBC0" "TP" "RSL"
				psw_check = false;
				ins_setup_rsl();
				get_pd(bd1_loc, rflen1);
				psw_cc = pd_cc;
				break;
			}
			break;
		case 0xEC:
			opcode2 = mem_byte[psw_loc + opcode2_offset_rie] & 0xff;
			switch (opcode2) {
			case 0x44: // 6580 "EC44" "BRXHG" "RIE"
				psw_check = false;
				ins_setup_rie();
				rlv3 = reg.getLong(rf3);
				rlv1 = reg.getLong(rf1) + rlv3;
				reg.putLong(rf1, rlv1);
				if (rf3 == ((rf3 >> 4) << 4)) {
					rlv3 = reg.getLong(rf3 + 8);
				}
				if (rlv1 > rlv3) {
					set_psw_loc(psw_loc - 6 + 2 * if2);
				}
				break;
			case 0x45: // 6600 "EC45" "BRXLG" "RIE"
				psw_check = false;
				ins_setup_rie();
				rlv3 = reg.getLong(rf3);
				rlv1 = reg.getLong(rf1) + rlv3;
				reg.putLong(rf1, rlv1);
				if (rf3 == ((rf3 >> 4) << 4)) {
					rlv3 = reg.getLong(rf3 + 8);
				}
				if (rlv1 <= rlv3) {
					set_psw_loc(psw_loc - 6 + 2 * if2);
				}
				break;
			}
			break;
		case 0xED:
			opcode2 = mem_byte[psw_loc + opcode2_offset_rxe] & 0xff;
			switch (opcode2) {
			case 0x04: // 6620 "ED04" "LDEB" "RXE"
				psw_check = false;
				ins_setup_rxe();
				fp_rdv1 = fp_get_db_from_eb(mem, xbd2_loc);
				fp_put_db(rf1, tz390.fp_db_type, fp_rdv1);
				break;
			case 0x05: // 6630 "ED05" "LXDB" "RXE"
				psw_check = false;
				ins_setup_rxe();
				fp_rbdv1 = fp_get_bd_from_db(mem, xbd2_loc);
				fp_put_bd(rf1, tz390.fp_lb_type, fp_rbdv1);
				break;
			case 0x06: // 6640 "ED06" "LXEB" "RXE"
				psw_check = false;
				ins_setup_rxe();
				fp_rbdv1 = fp_get_bd_from_eb(mem, xbd2_loc);
				fp_put_bd(rf1, tz390.fp_lb_type, fp_rbdv1);
				break;
			case 0x07: // 6650 "ED07" "MXDB" "RXE"
				psw_check = false;
				ins_setup_rxe();
				fp_rbdv1 = fp_get_bd_from_db(fp_reg, rf1).multiply(
						fp_get_bd_from_db(mem, xbd2_loc), fp_db_context).round(
						fp_x_context);
				fp_put_bd(rf1, tz390.fp_lb_type, fp_rbdv1);
				check_lb_mpy();
				break;
			case 0x08: // 6660 "ED08" "KEB" "RXE"
				psw_check = false;
				ins_setup_rxe();
				fp_signal = true;
				psw_cc = fp_get_eb_comp_cc(fp_get_eb_from_eb(fp_reg, rf1),
						fp_get_eb_from_eb(mem, xbd2_loc));
				break;
			case 0x09: // 6670 "ED09" "CEB" "RXE"
				psw_check = false;
				ins_setup_rxe();
				psw_cc = fp_get_eb_comp_cc(fp_get_eb_from_eb(fp_reg, rf1),
						fp_get_eb_from_eb(mem, xbd2_loc));
				break;
			case 0x0A: // 6680 "ED0A" "AEB" "RXE"
				psw_check = false;
				ins_setup_rxe();
				fp_rev1 = fp_get_eb_from_eb(fp_reg, rf1)
						+ fp_get_eb_from_eb(mem, xbd2_loc);
				fp_put_eb(rf1, tz390.fp_eb_type, fp_rev1);
				psw_cc = fp_get_eb_add_sub_cc();
				break;
			case 0x0B: // 6690 "ED0B" "SEB" "RXE"
				psw_check = false;
				ins_setup_rxe();
				fp_rev1 = fp_get_eb_from_eb(fp_reg, rf1)
						- fp_get_eb_from_eb(mem, xbd2_loc);
				fp_put_eb(rf1, tz390.fp_eb_type, fp_rev1);
				psw_cc = fp_get_eb_add_sub_cc();
				break;
			case 0x0C: // 6700 "ED0C" "MDEB" "RXE"
				psw_check = false;
				ins_setup_rxe();
				fp_rdv1 = fp_get_db_from_eb(fp_reg, rf1)
						* fp_get_db_from_eb(mem, xbd2_loc);
				fp_put_db(rf1, tz390.fp_db_type, fp_rdv1);
				check_db_mpy();
				break;
			case 0x0D: // 6710 "ED0D" "DEB" "RXE"
				psw_check = false;
				ins_setup_rxe();
				fp_rev2 = fp_get_eb_from_eb(mem, xbd2_loc);
				fp_rev1 = fp_get_eb_from_eb(fp_reg, rf1) / fp_rev2;
				fp_put_eb(rf1, tz390.fp_eb_type, fp_rev1);
				check_eb_div();
				break;
			case 0x0E: // 6720 "ED0E" "MAEB" "RXF"
				psw_check = false;
				ins_setup_rxf();
				fp_rev1 = fp_get_eb_from_eb(fp_reg, rf1)
						+ fp_get_eb_from_eb(mem, xbd2_loc)
						* fp_get_eb_from_eb(fp_reg, rf3);
				fp_put_eb(rf1, tz390.fp_eb_type, fp_rev1);
				check_eb_mpy();
				break;
			case 0x0F: // 6730 "ED0F" "MSEB" "RXF"
				psw_check = false;
				ins_setup_rxf();
				fp_rev1 = fp_get_eb_from_eb(fp_reg, rf1)
						- fp_get_eb_from_eb(mem, xbd2_loc)
						* fp_get_eb_from_eb(fp_reg, rf3);
				fp_put_eb(rf1, tz390.fp_eb_type, fp_rev1);
				check_eb_mpy();
				break;
			case 0x10: // 6740 "ED10" "TCEB" "RXE"
				psw_check = false;
				ins_setup_rxe();
				fp_rev1 = fp_get_eb_from_eb(fp_reg, rf1); // RPI 326
				psw_cc = psw_cc0;
				if ((xbd2_loc & 0x800) != 0 && fp_rev1 == 0) {
					psw_cc = psw_cc1; // +0
				}
				if ((xbd2_loc & 0x400) != 0 && fp_rev1 == 0) {
					psw_cc = psw_cc1; // -0
				}
				if ((xbd2_loc & 0x200) != 0 && fp_rev1 > 0) {
					psw_cc = psw_cc1; // +normalized
				}
				if ((xbd2_loc & 0x100) != 0 && fp_rev1 < 0) {
					psw_cc = psw_cc1; // -normalized
				}
				if ((xbd2_loc & 0x080) != 0) {
					// psw_cc = psw_cc1; //+denormalized (never)
				}
				if ((xbd2_loc & 0x040) != 0) {
					// psw_cc = psw_cc1; //-denormalized (never)
				}
				if ((xbd2_loc & 0x020) != 0) {
					// psw_cc = psw_cc1; //+infinity
				}
				if ((xbd2_loc & 0x010) != 0) {
					// psw_cc = psw_cc1; //-infinity
				}
				if ((xbd2_loc & 0x008) != 0) {
					// psw_cc = psw_cc1; //+quiet nan
				}
				if ((xbd2_loc & 0x004) != 0) {
					// psw_cc = psw_cc1; //-quiet nan
				}
				if ((xbd2_loc & 0x002) != 0) {
					// psw_cc = psw_cc1; //+signaling nan
				}
				if ((xbd2_loc & 0x001) != 0) {
					// psw_cc = psw_cc1; //-signaling nan
				}
				break;
			case 0x11: // 6750 "ED11" "TCDB" "RXE"
				psw_check = false;
				ins_setup_rxe();
				fp_rdv1 = fp_get_db_from_dh(fp_reg, rf1); // RPI 326
				psw_cc = psw_cc0;
				if ((xbd2_loc & 0x800) != 0 && fp_rdv1 == 0) {
					psw_cc = psw_cc1; // +0
				}
				if ((xbd2_loc & 0x400) != 0 && fp_rdv1 == 0) {
					psw_cc = psw_cc1; // -0
				}
				if ((xbd2_loc & 0x200) != 0 && fp_rdv1 > 0) {
					psw_cc = psw_cc1; // +normalized
				}
				if ((xbd2_loc & 0x100) != 0 && fp_rdv1 < 0) {
					psw_cc = psw_cc1; // -normalized
				}
				if ((xbd2_loc & 0x080) != 0) {
					// psw_cc = psw_cc1; //+denormalized (never)
				}
				if ((xbd2_loc & 0x040) != 0) {
					// psw_cc = psw_cc1; //-denormalized (never)
				}
				if ((xbd2_loc & 0x020) != 0) {
					// psw_cc = psw_cc1; //+infinity
				}
				if ((xbd2_loc & 0x010) != 0) {
					// psw_cc = psw_cc1; //-infinity
				}
				if ((xbd2_loc & 0x008) != 0) {
					// psw_cc = psw_cc1; //+quiet nan
				}
				if ((xbd2_loc & 0x004) != 0) {
					// psw_cc = psw_cc1; //-quiet nan
				}
				if ((xbd2_loc & 0x002) != 0) {
					// psw_cc = psw_cc1; //+signaling nan
				}
				if ((xbd2_loc & 0x001) != 0) {
					// psw_cc = psw_cc1; //-signaling nan
				}
				break;
			case 0x12: // 6760 "ED12" "TCXB" "RXE"
				psw_check = false;
				ins_setup_rxe();
				fp_rbdv1 = fp_get_bd_from_lb(fp_reg, rf1); // RPI 326
				psw_cc = psw_cc0;
				if ((xbd2_loc & 0x800) != 0
						&& fp_rbdv1.compareTo(BigDecimal.ZERO) == 0) {
					psw_cc = psw_cc1; // +0
				}
				if ((xbd2_loc & 0x400) != 0
						&& fp_rbdv1.compareTo(BigDecimal.ZERO) == 0) {
					psw_cc = psw_cc1; // -0
				}
				if ((xbd2_loc & 0x200) != 0
						&& fp_rbdv1.compareTo(BigDecimal.ZERO) > 0) {
					psw_cc = psw_cc1; // +normalized
				}
				if ((xbd2_loc & 0x100) != 0
						&& fp_rbdv1.compareTo(BigDecimal.ZERO) < 0) {
					psw_cc = psw_cc1; // -normalized
				}
				if ((xbd2_loc & 0x080) != 0) {
					// psw_cc = psw_cc1; //+denormalized (never)
				}
				if ((xbd2_loc & 0x040) != 0) {
					// psw_cc = psw_cc1; //-denormalized (never)
				}
				if ((xbd2_loc & 0x020) != 0) {
					// psw_cc = psw_cc1; //+infinity
				}
				if ((xbd2_loc & 0x010) != 0) {
					// psw_cc = psw_cc1; //-infinity
				}
				if ((xbd2_loc & 0x008) != 0) {
					// psw_cc = psw_cc1; //+quiet nan
				}
				if ((xbd2_loc & 0x004) != 0) {
					// psw_cc = psw_cc1; //-quiet nan
				}
				if ((xbd2_loc & 0x002) != 0) {
					// psw_cc = psw_cc1; //+signaling nan
				}
				if ((xbd2_loc & 0x001) != 0) {
					// psw_cc = psw_cc1; //-signaling nan
				}
				break;
			case 0x14: // 6770 "ED14" "SQEB" "RXE"
				psw_check = false;
				ins_setup_rxe();
				fp_rev1 = (float) Math.sqrt(fp_get_db_from_eb(mem, xbd2_loc));
				fp_put_eb(rf1, tz390.fp_eb_type, fp_rev1);
				break;
			case 0x15: // 6780 "ED15" "SQDB" "RXE"
				psw_check = false;
				ins_setup_rxe();
				fp_rdv1 = Math.sqrt(fp_get_db_from_db(mem, xbd2_loc));
				fp_put_db(rf1, tz390.fp_db_type, fp_rdv1);
				break;
			case 0x17: // 6790 "ED17" "MEEB" "RXE"
				psw_check = false;
				ins_setup_rxe();
				fp_rev1 = fp_get_eb_from_eb(fp_reg, rf1)
						* fp_get_eb_from_eb(mem, xbd2_loc);
				fp_put_eb(rf1, tz390.fp_eb_type, fp_rev1);
				check_eh_mpy();
				break;
			case 0x18: // 6800 "ED18" "KDB" "RXE"
				psw_check = false;
				ins_setup_rxe();
				fp_signal = true;
				psw_cc = fp_get_db_comp_cc(fp_get_db_from_db(fp_reg, rf1),
						fp_get_db_from_db(mem, xbd2_loc));
				break;
			case 0x19: // 6810 "ED19" "CDB" "RXE"
				psw_check = false;
				ins_setup_rxe();
				psw_cc = fp_get_db_comp_cc(fp_get_db_from_db(fp_reg, rf1),
						fp_get_db_from_db(mem, xbd2_loc));
				break;
			case 0x1A: // 6820 "ED1A" "ADB" "RXE"
				psw_check = false;
				ins_setup_rxe();
				fp_rdv1 = fp_get_db_from_db(fp_reg, rf1)
						+ fp_get_db_from_db(mem, xbd2_loc);
				fp_put_db(rf1, tz390.fp_db_type, fp_rdv1);
				psw_cc = fp_get_db_add_sub_cc();
				break;
			case 0x1B: // 6830 "ED1B" "SDB" "RXE"
				psw_check = false;
				ins_setup_rxe();
				fp_rdv1 = fp_get_db_from_db(fp_reg, rf1)
						- fp_get_db_from_db(mem, xbd2_loc);
				fp_put_db(rf1, tz390.fp_db_type, fp_rdv1);
				psw_cc = fp_get_db_add_sub_cc();
				break;
			case 0x1C: // 6840 "ED1C" "MDB" "RXE"
				psw_check = false;
				ins_setup_rxe();
				fp_rdv1 = fp_get_db_from_db(fp_reg, rf1)
						* fp_get_db_from_db(mem, xbd2_loc);
				fp_put_db(rf1, tz390.fp_db_type, fp_rdv1);
				check_db_mpy();
				break;
			case 0x1D: // 6850 "ED1D" "DDB" "RXE"
				psw_check = false;
				ins_setup_rxe();
				fp_rdv2 = fp_get_db_from_db(mem, xbd2_loc);
				fp_rdv1 = fp_get_db_from_db(fp_reg, rf1) / fp_rdv2;
				fp_put_db(rf1, tz390.fp_db_type, fp_rdv1);
				check_db_div();
				break;
			case 0x1E: // 6860 "ED1E" "MADB" "RXF"
				psw_check = false;
				ins_setup_rxf();
				fp_rdv1 = fp_get_db_from_db(fp_reg, rf1)
						+ fp_get_db_from_db(mem, xbd2_loc)
						* fp_get_db_from_db(fp_reg, rf3);
				fp_put_db(rf1, tz390.fp_dh_type, fp_rdv1);
				check_db_mpy();
				break;
			case 0x1F: // 6870 "ED1F" "MSDB" "RXF"
				psw_check = false;
				ins_setup_rxf();
				fp_rdv1 = fp_get_db_from_db(fp_reg, rf1)
						- fp_get_db_from_db(mem, xbd2_loc)
						* fp_get_db_from_db(fp_reg, rf3);
				fp_put_db(rf1, tz390.fp_dh_type, fp_rdv1);
				check_db_mpy();
				break;
			case 0x24: // 6880 "ED24" "LDE" "RXE"
				psw_check = false;
				ins_setup_rxe();
				fp_load_reg(rf1, tz390.fp_dh_type, mem, xbd2_loc,
						tz390.fp_eh_type);
				break;
			case 0x25: // 6890 "ED25" "LXD" "RXE"
				psw_check = false;
				ins_setup_rxe();
				fp_load_reg(rf1, tz390.fp_lh_type, mem, xbd2_loc,
						tz390.fp_lh_type);
				break;
			case 0x26: // 6900 "ED26" "LXE" "RXE"
				psw_check = false;
				ins_setup_rxe();
				fp_load_reg(rf1, tz390.fp_lh_type, mem, xbd2_loc,
						tz390.fp_eh_type);
				break;
			case 0x2E: // 6910 "ED2E" "MAE" "RXF"
				psw_check = false;
				ins_setup_rxf();
				fp_rdv1 = fp_get_db_from_eh(fp_reg, rf1)
						+ fp_get_db_from_eh(mem, xbd2_loc)
						* fp_get_db_from_eh(fp_reg, rf3);
				fp_put_db(rf1, tz390.fp_eh_type, fp_rdv1);
				check_eh_mpy();
				break;
			case 0x2F: // 6920 "ED2F" "MSE" "RXF"
				psw_check = false;
				ins_setup_rxf();
				fp_rdv1 = fp_get_db_from_eh(fp_reg, rf1)
						- fp_get_db_from_eh(mem, xbd2_loc)
						* fp_get_db_from_eh(fp_reg, rf3);
				fp_put_db(rf1, tz390.fp_eh_type, fp_rdv1);
				check_eh_mpy();
				break;
			case 0x34: // 6930 "ED34" "SQE" "RXE"
				psw_check = false;
				ins_setup_rxe();
				fp_rdv1 = Math.sqrt(fp_get_db_from_eh(mem, xbd2_loc));
				fp_put_db(rf1, tz390.fp_eh_type, fp_rdv1);
				break;
			case 0x35: // 6940 "ED35" "SQD" "RXE"
				psw_check = false;
				ins_setup_rxe();
				fp_rdv1 = Math.sqrt(fp_get_db_from_dh(mem, xbd2_loc));
				fp_put_db(rf1, tz390.fp_dh_type, fp_rdv1);
				break;
			case 0x37: // 6950 "ED37" "MEE" "RXE"
				psw_check = false;
				ins_setup_rxe();
				fp_rdv1 = fp_get_db_from_eh(fp_reg, rf1)
						* fp_get_db_from_eh(mem, xbd2_loc);
				fp_put_db(rf1, tz390.fp_eh_type, fp_rdv1);
				check_eh_mpy();
				break;
			case 0x38: // 7410 "ED38" "MAYL" "RXF" Z9-46
				psw_check = false; // RPI 298
				ins_setup_rxf();
				fp_rbdv1 = fp_get_bd_from_dh(mem, xbd2_loc).multiply(
						fp_get_bd_from_dh(fp_reg, rf3), fp_db_context).add(
						fp_get_bd_from_dh(fp_reg, rf1)).round(fp_x_context);
				fp_put_bd(rf1, tz390.fp_lh_type, fp_rbdv1);
				check_lh_mpy();
				break;
			case 0x39: // 7420 "ED39" "MYL" "RXF" Z9-47
				psw_check = false; // RPI 298
				ins_setup_rxf();
				fp_rbdv1 = fp_get_bd_from_dh(mem, xbd2_loc).multiply(
						fp_get_bd_from_dh(fp_reg, rf3), fp_db_context).round(
						fp_x_context);
				fp_put_bd(rf1, tz390.fp_lh_type, fp_rbdv1);
				check_lh_mpy();
				break;
			case 0x3A: // 7430 "ED3A" "MAY" "RXF" Z9-48
				psw_check = false;
				ins_setup_rxf();
				fp_rbdv1 = fp_get_bd_from_dh(mem, xbd2_loc).multiply(
						fp_get_bd_from_dh(fp_reg, rf3), fp_db_context).add(
						fp_get_bd_from_dh(fp_reg, rf1)).round(fp_x_context);
				fp_put_bd(rf1, tz390.fp_lh_type, fp_rbdv1);
				check_lh_mpy();
				break;
			case 0x3B: // 7440 "ED3B" "MY" "RXF" Z9-49 RPI 298
				psw_check = false;
				ins_setup_rxf();
				fp_rbdv1 = fp_get_bd_from_dh(mem, xbd2_loc).multiply(
						fp_get_bd_from_dh(fp_reg, rf3), fp_db_context).round(
						fp_x_context);
				fp_put_bd(rf1, tz390.fp_lh_type, fp_rbdv1);
				check_lh_mpy();
				break;
			case 0x3C: // 7450 "ED3C" "MAYH" "RXF" Z9-50
				psw_check = false; // RPI 298
				ins_setup_rxf();
				fp_rbdv1 = fp_get_bd_from_dh(mem, xbd2_loc).multiply(
						fp_get_bd_from_dh(fp_reg, rf3), fp_db_context).add(
						fp_get_bd_from_dh(fp_reg, rf1)).round(fp_x_context);
				fp_put_bd(rf1, tz390.fp_lh_type, fp_rbdv1);
				check_lh_mpy();
				break;
			case 0x3D: // 7460 "ED3D" "MYH" "RXF" Z9-51 // RPI 298
				psw_check = false; // RPI 298
				ins_setup_rxf();
				fp_rbdv1 = fp_get_bd_from_dh(mem, xbd2_loc).multiply(
						fp_get_bd_from_dh(fp_reg, rf3), fp_db_context).round(
						fp_x_context);
				fp_put_bd(rf1, tz390.fp_lh_type, fp_rbdv1);
				check_lh_mpy();
				break;
			case 0x3E: // 6960 "ED3E" "MAD" "RXF"
				psw_check = false;
				ins_setup_rxf();
				fp_rdv1 = fp_get_db_from_dh(fp_reg, rf1)
						+ fp_get_db_from_dh(mem, xbd2_loc)
						* fp_get_db_from_dh(fp_reg, rf3);
				fp_put_db(rf1, tz390.fp_dh_type, fp_rdv1);
				check_dh_mpy();
				break;
			case 0x3F: // 6970 "ED3F" "MSD" "RXF"
				psw_check = false;
				ins_setup_rxf();
				fp_rdv1 = fp_get_db_from_dh(fp_reg, rf1)
						- fp_get_db_from_dh(mem, xbd2_loc)
						* fp_get_db_from_dh(fp_reg, rf3);
				fp_put_db(rf1, tz390.fp_dh_type, fp_rdv1);
				check_dh_mpy();
				break;
			case 0x40: // "SLDT" "ED40" "RXF" DFP 45
				psw_check = true;
				ins_setup_rxf();
				break;
			case 0x41: // "SRDT" "ED41" "RXF" DFP 46
				psw_check = true;
				ins_setup_rxf();
				break;
			case 0x48: // "SLXT" "ED48" "RXF" DFP 47
				psw_check = true;
				ins_setup_rxf();
				break;
			case 0x49: // "SRXT" "ED49" "RXF" DFP 48
				psw_check = true;
				ins_setup_rxf();
				break;
			case 0x50: // "TDCET" "ED50" "RXE" DFP 49
				psw_check = true;
				ins_setup_rxe();
				break;
			case 0x51: // "TDGET" "ED51" "RXE" DFP 50
				psw_check = true;
				ins_setup_rxe();
				break;
			case 0x54: // "TDCDT" "ED54" "RXE" DFP 51
				psw_check = true;
				ins_setup_rxe();
				break;
			case 0x55: // "TDGDT" "ED55" "RXE" DFP 52
				psw_check = true;
				ins_setup_rxe();
				break;
			case 0x58: // "TDCXT" "ED58" "RXE" DFP 53
				psw_check = true;
				ins_setup_rxe();
				break;
			case 0x59: // "TDGXT" "ED59" "RXE" DFP 54
				psw_check = true;
				ins_setup_rxe();
				break;
			case 0x64: // 6980 "ED64" "LEY" "RXY"
				psw_check = false;
				ins_setup_rxy();
				fp_load_reg(rf1, tz390.fp_eh_type, mem, xbd2_loc,
						tz390.fp_eh_type);
				break;
			case 0x65: // 6990 "ED65" "LDY" "RXY"
				psw_check = false;
				ins_setup_rxy();
				fp_load_reg(rf1, tz390.fp_dh_type, mem, xbd2_loc,
						tz390.fp_dh_type);
				break;
			case 0x66: // 7000 "ED66" "STEY" "RXY"
				psw_check = false;
				ins_setup_rxy();
				if (fp_reg_ctl[mf1] != fp_ctl_ld) {
					fp_store_reg(fp_reg, rf1);
				}
				mem.putInt(xbd2_loc, fp_reg.getInt(rf1));
				break;
			case 0x67: // 7010 "ED67" "STDY" "RXY"
				psw_check = false;
				ins_setup_rxy();
				if (fp_reg_ctl[mf1] != fp_ctl_ld) {
					fp_store_reg(fp_reg, rf1);
				}
				mem.putLong(xbd2_loc, fp_reg.getLong(rf1));
				break;
			}
			break;
		case 0xEE: // 7020 "EE" "PLO" "SS"
			ins_setup_ss();
			break;
		case 0xEF: // 7030 "EF" "LMD" "SS"
			psw_check = false;
			ins_setup_ss();
			rf1 = ((rflen - 1) & 0xf0) >> 1;
			rf3 = ((rflen - 1) & 0xf) << 3;
			if (rf1 > rf3) {
				while (rf1 < reg_len) {
					reg.putInt(rf1, mem.getInt(bd1_loc));
					reg.putInt(rf1 + 4, mem.getInt(bd2_loc));
					bd1_loc = bd1_loc + 4;
					bd2_loc = bd2_loc + 4;
					rf1 = rf1 + 8;
				}
				rf1 = 0;
			}
			while (rf1 <= rf3) {
				reg.putInt(rf1, mem.getInt(bd1_loc));
				reg.putInt(rf1 + 4, mem.getInt(bd2_loc));
				bd1_loc = bd1_loc + 4;
				bd2_loc = bd2_loc + 4;
				rf1 = rf1 + 8;
			}
			break;
		case 0xF0: // 7040 "F0" "SRP" "SS"
			psw_check = false;
			ins_setup_ssp();
			if (!get_pd(bd1_loc, rflen1)) { // RPI 305
				break;
			}
			int round_digit = rflen2 - 1;
			int shift = bd2_loc & 0x3f;
			if (shift < 0x20) {
				while (shift > 0) {
					if (pdf_is_big) {
						pdf_big_int = pdf_big_int.multiply(BigInteger
								.valueOf(10));
					} else {
						pdf_long = pdf_long * 10;
					}
					shift--;
				}
			} else {
				shift = 0x3f - shift + 1;
				while (shift > 1) {
					if (pdf_is_big) {
						pdf_big_int = pdf_big_int
								.divide(BigInteger.valueOf(10));
					} else {
						pdf_long = pdf_long / 10;
					}
					shift--;
				}
				if (pdf_is_big) {
					pdf_big_int = pdf_big_int.add(BigInteger
							.valueOf(round_digit));
					pdf_big_int = pdf_big_int.divide(BigInteger.valueOf(10));
				} else {
					pdf_long = (pdf_long + round_digit) / 10;
				}
			}
			put_pd(mem_byte, bd1_loc, rflen1);
			psw_cc = pd_cc;
			break;
		case 0xF1: // 7050 "F1" "MVO" "SS"
			psw_check = false;
			ins_setup_ssp();
			bd1_end = bd1_loc - 1;
			bd2_end = bd2_loc - 1;
			bd1_loc = bd1_loc + rflen1 - 1;
			bd2_loc = bd2_loc + rflen2 - 1;
			pdf_next_in = mem_byte[bd2_loc];
			bd2_loc--;
			pdf_next_out = (byte) (mem_byte[bd1_loc] & 0xf);
			mem_byte[bd1_loc] = (byte) (pdf_next_out | ((pdf_next_in & 0xf) << 4));
			bd1_loc--;
			while (bd1_loc > bd1_end) {
				pdf_next_out = (byte) ((pdf_next_in & 0xf0) >> 4);
				if (bd2_loc > bd2_end) {
					pdf_next_in = mem_byte[bd2_loc];
					bd2_loc--;
				} else {
					pdf_next_in = 0;
				}
				mem_byte[bd1_loc] = (byte) (pdf_next_out | ((pdf_next_in & 0xf) << 4));
				bd1_loc--;
			}
			break;
		case 0xF2: // 7060 "F2" "PACK" "SS"
			psw_check = false;
			ins_setup_ssp();
			bd1_end = bd1_loc - 1;
			bd2_end = bd2_loc - 1;
			bd1_loc = bd1_loc + rflen1 - 1;
			bd2_loc = bd2_loc + rflen2 - 1;
			pdf_next_out = (byte) ((mem_byte[bd2_loc] & 0xf0) >> 4);
			while (bd1_loc > bd1_end) {
				if (bd2_loc > bd2_end) {
					pdf_next_in = mem_byte[bd2_loc];
					bd2_loc--;
				} else {
					pdf_next_in = 0;
				}
				mem_byte[bd1_loc] = (byte) (pdf_next_out | ((pdf_next_in & 0xf) << 4));
				bd1_loc--;
				if (bd2_loc > bd2_end) {
					pdf_next_out = (byte) (mem_byte[bd2_loc] & 0xf);
					bd2_loc--;
				} else {
					pdf_next_out = 0;
				}
			}
			break;
		case 0xF3: // 7070 "F3" "UNPK" "SS"
			psw_check = false;
			ins_setup_ssp();
			bd1_end = bd1_loc - 1;
			bd2_end = bd2_loc - 1;
			bd1_loc = bd1_loc + rflen1 - 1;
			bd2_loc = bd2_loc + rflen2 - 1;
			mem_byte[bd1_loc] = (byte) (((mem_byte[bd2_loc] & 0xf0) >> 4) | ((mem_byte[bd2_loc] & 0xf) << 4));
			bd1_loc--;
			bd2_loc--;
			pdf_next_right = true;
			while (bd1_loc > bd1_end) {
				if (pdf_next_right) {
					if (bd2_loc > bd2_end) {
						pdf_next_right = false;
						pdf_next_in = mem_byte[bd2_loc];
						bd2_loc--;
					} else {
						pdf_next_in = 0;
					}
					mem_byte[bd1_loc] = (byte) ((pdf_next_in & 0xf) | pdf_zone);
					bd1_loc--;
				} else {
					pdf_next_right = true;
					mem_byte[bd1_loc] = (byte) (((pdf_next_in & 0xf0) >> 4) | pdf_zone);
					bd1_loc--;
				}
			}
			break;
		case 0xF8: // 7080 "F8" "ZAP" "SS"
			psw_check = false;
			ins_setup_ssp();
			if (get_pd(bd2_loc, rflen2)) {
				put_pd(mem_byte, bd1_loc, rflen1);
				psw_cc = pd_cc;
			}
			break;
		case 0xF9: // 7090 "F9" "CP" "SS"
			psw_check = false;
			ins_setup_ssp();
			get_pdf_ints();
			if (pdf_is_big) {
				psw_cc = get_big_int_comp_cc(pdf_big_int1, pdf_big_int2);
			} else {
				psw_cc = get_long_comp_cc(pdf_long1, pdf_long2);
			}
			break;
		case 0xFA: // 7100 "FA" "AP" "SS"
			psw_check = false;
			ins_setup_ssp();
			get_pdf_ints();
			if (pdf_is_big) {
				pdf_big_int = pdf_big_int1.add(pdf_big_int2);
			} else {
				pdf_long = pdf_long1 + pdf_long2;
			}
			put_pd(mem_byte, bd1_loc, rflen1);
			psw_cc = pd_cc;
			break;
		case 0xFB: // 7110 "FB" "SP" "SS"
			psw_check = false;
			ins_setup_ssp();
			get_pdf_ints();
			if (pdf_is_big) {
				pdf_big_int = pdf_big_int1.subtract(pdf_big_int2);
			} else {
				pdf_long = pdf_long1 - pdf_long2;
			}
			put_pd(mem_byte, bd1_loc, rflen1);
			psw_cc = pd_cc;
			break;
		case 0xFC: // 7120 "FC" "MP" "SS"
			psw_check = false;
			ins_setup_ssp();
			get_pdf_ints();
			if (pdf_is_big) {
				pdf_big_int = pdf_big_int1.multiply(pdf_big_int2);
			} else {
				if (pdf_long1 < max_pos_int && pdf_long1 > min_neg_int
						&& pdf_long2 < max_pos_int && pdf_long2 > min_neg_int) {
					pdf_long = pdf_long1 * pdf_long2;
				} else {
					pdf_is_big = true;
					pdf_big_int1 = BigInteger.valueOf(pdf_long1);
					pdf_big_int2 = BigInteger.valueOf(pdf_long2);
					pdf_big_int = pdf_big_int1.multiply(pdf_big_int2);
				}
			}
			put_pd(mem_byte, bd1_loc, rflen1);
			break;
		case 0xFD: // 7130 "FD" "DP" "SS"
			psw_check = false;
			ins_setup_ssp();
			get_pdf_ints();
			if (pdf_is_big) {
				if (pdf_big_int2.compareTo(BigInteger.ZERO) == 0) {
					psw_cc = psw_cc3;
					set_psw_check(psw_pic_pd_div);
				}
				BigInteger[] big_quo_rem = pdf_big_int1
						.divideAndRemainder(pdf_big_int2);
				pdf_big_int = big_quo_rem[0];
				put_pd(mem_byte, bd1_loc, rflen1 - rflen2);
				pdf_big_int = big_quo_rem[1];
				put_pd(mem_byte, bd1_loc + rflen1 - rflen2, rflen2);
			} else {
				if (pdf_long2 == 0) {
					psw_cc = psw_cc3;
					set_psw_check(psw_pic_pd_div);
					break;
				}
				pdf_long = pdf_long1 / pdf_long2;
				put_pd(mem_byte, bd1_loc, rflen1 - rflen2);
				pdf_long = pdf_long1 - pdf_long * pdf_long2;
				put_pd(mem_byte, bd1_loc + rflen1 - rflen2, rflen2);
			}
			break;
		}
	}

	/*
	 * end of ez390 emulator while switch code
	 */
	/*
	 * ********************************************* instruction setup routines
	 * ********************************************
	 */
	private void ins_setup_dm() { // "DM" 1 DIAGNOSE 83000000
		psw_ins_len = 4;
		if (tz390.opt_trace) {
			put_ins_trace(" DIAGNOSE");
		}
		if (ex_mode) {
			ex_restore();
		}
		psw_loc = psw_loc + 4;
	}

	private void ins_setup_e() { // "E" 8 PR oooo
		psw_ins_len = 2;
		if (tz390.opt_trace) {
			put_ins_trace("");
		}
		if (ex_mode) {
			ex_restore();
		}
		psw_loc = psw_loc + 2;
	}

	private void ins_setup_i() { // "I" 1 SVC 00ii
		if1 = mem_byte[psw_loc + 1] & 0xff;
		psw_ins_len = 2;
		if (tz390.opt_trace) {
			put_ins_trace(" I1=" + tz390.get_hex(if1, 2) + " " + trace_svc() // RPI
																				// 312
			);
		}
		if (ex_mode) {
			ex_restore();
		}
		psw_loc = psw_loc + 2;
	}

	private void ins_setup_ri() { // "RI" 37 IIHH ooroiiii
		/*
		 * fetch rf1 and if2
		 */
		rf1 = (mem_byte[psw_loc + 1] & 0xf0) >> 1;
		mf1 = rf1 >> 3;
		if2 = mem.getShort(psw_loc + 2);
		psw_ins_len = 4;
		if (tz390.opt_trace) {
			if (opcode1 == 0xa7) {
				if (opcode2 == 4) {
					rf2 = psw_loc + 2 * if2;
					put_ins_trace(" S2(" + tz390.get_hex(rf2, 8) + ")="
							+ get_ins_target(rf2));
				} else if (opcode2 >= 5 && opcode2 <= 7) {
					rf2 = psw_loc + 2 * if2;
					put_ins_trace(" R" + tz390.get_hex(mf1, 1) + "="
							+ tz390.get_hex(reg.getInt(rf1 + 4), 8) + " S2("
							+ tz390.get_hex(rf2, 8) + ")="
							+ get_ins_target(rf2));
				} else if (opcode2 == 0x8 || opcode2 == 0xa || opcode2 == 0xc
						|| opcode2 == 0xe) {
					put_ins_trace(" R" + tz390.get_hex(mf1, 1) + "="
							+ tz390.get_hex(reg.getInt(rf1 + 4), 8) + " I2="
							+ tz390.get_hex(if2, 4));
				} else {
					put_ins_trace(" R" + tz390.get_hex(mf1, 1) + "="
							+ get_long_hex(reg.getLong(rf1)) + " I2="
							+ tz390.get_hex(if2, 4));
				}
			} else {
				put_ins_trace(" R" + tz390.get_hex(mf1, 1) + "="
						+ get_long_hex(reg.getLong(rf1)) + " I2="
						+ tz390.get_hex(if2, 4));
			}
		}
		if (ex_mode) {
			ex_restore();
		}
		psw_loc = psw_loc + 4;
	}

	private void ins_setup_rie() { // "RIE" 4 BRXLG oorriiii00oo
		psw_ins_len = 6;
		rf1 = mem_byte[psw_loc + 1] & 0xff;
		mf3 = rf1 & 0xf;
		rf3 = mf3 << 3;
		rf1 = (rf1 & 0xf0) >> 1;
		mf1 = rf1 >> 3;
		if2 = mem.getShort(psw_loc + 2);
		if (tz390.opt_trace) {
			put_ins_trace(" R1" + tz390.get_hex(mf1, 1) + "="
					+ tz390.get_hex(reg.getInt(rf1 + 4), 8) + " R3"
					+ tz390.get_hex(mf3, 1) + "="
					+ tz390.get_hex(reg.getInt(rf3 + 4), 8) + " I2="
					+ tz390.get_hex(if2, 8));
		}
		if (ex_mode) {
			ex_restore();
		}
		psw_loc = psw_loc + 6;
	}

	private void ins_setup_ril() { // "RIL" 6 BRCL oomollllllll
		psw_ins_len = 6;
		rf1 = (mem_byte[psw_loc + 1] & 0xf0) >> 1;
		mf1 = rf1 >> 3;
		if2 = mem.getInt(psw_loc + 2);
		bd2_loc = psw_loc + 2 * if2;
		if (tz390.opt_trace) {
			if (opcode1 == 0xc2 // C2.. SLGFI etc.
					|| opcode2 == 0x1 // C01 LGFI
					|| opcode2 > 0x5) { // C06 XIHF etc. RPI200
				put_ins_trace(" R" + tz390.get_hex(mf1, 1) + "="
						+ get_long_hex(reg.getLong(rf1)) + " I2="
						+ tz390.get_hex(if2, 8));
			} else if (opcode2 == 0x0) { // C00 LARL
				put_ins_trace(" R" + tz390.get_hex(mf1, 1) + "="
						+ tz390.get_hex(reg.getInt(rf1 + 4), 8) + " S2("
						+ tz390.get_hex(bd2_loc & psw_amode, 8) + ")");
			} else if (opcode2 == 0x5) {
				put_ins_trace( // C05 BRASL
				" R" + tz390.get_hex(mf1, 1) + "="
						+ tz390.get_hex(reg.getInt(rf1 + 4), 8) + " S2("
						+ tz390.get_hex(bd2_loc, 8) + ")="
						+ get_ins_target(bd2_loc));
			} else { // C04 BRCL with mnemonics
				put_ins_trace(" S2(" + tz390.get_hex(bd2_loc, 8) + ")="
						+ get_ins_target(bd2_loc));
			}
		}
		if (ex_mode) {
			ex_restore();
		}
		psw_loc = psw_loc + 6;
	}

	private void ins_setup_rr() { // "RR" 60 LR oorr
		/*
		 * fetch rf1,rf2 and update psw
		 */
		rf1 = mem_byte[psw_loc + 1] & 0xff;
		mf2 = rf1 & 0xf;
		rf2 = mf2 << 3;
		rf1 = (rf1 & 0xf0) >> 1;
		mf1 = rf1 >> 3;
		psw_ins_len = 2;
		if (tz390.opt_trace) {
			if (opcode1 == 0x07) {
				rv2 = reg.getInt(rf2 + 4);
				put_ins_trace(" R" + tz390.get_hex(mf2, 1) + "("
						+ tz390.get_hex(rv2, 8) + ")="
						+ get_ins_target(rv2 & psw_amode));
			} else if (opcode1 >= 0x20 && opcode1 <= 0x3f) {
				put_ins_trace(" F" + tz390.get_hex(mf1, 1) + "="
						+ get_fp_long_hex(rf1) + " F" + tz390.get_hex(mf2, 1)
						+ "=" + get_fp_long_hex(rf2));
			} else if (opcode1 == 0x0e // mvcl
					|| opcode1 == 0x0f) { // clcl
				put_ins_trace(" R" + tz390.get_hex(mf1, 1) + "="
						+ tz390.get_hex(reg.getInt(rf1 + 4), 8) + " R"
						+ tz390.get_hex(mf1 + 1, 1) + "="
						+ tz390.get_hex(reg.getInt(rf1 + 12), 8) + " R"
						+ tz390.get_hex(mf2, 1) + "="
						+ tz390.get_hex(reg.getInt(rf2 + 4), 8) + " R"
						+ tz390.get_hex(mf2 + 1, 1) + "="
						+ tz390.get_hex(reg.getInt(rf2 + 12), 8));
			} else if (opcode1 == 0x1c // mr
					|| opcode1 == 0x1d) { // dr
				put_ins_trace(" R" + tz390.get_hex(mf1, 1) + "="
						+ tz390.get_hex(reg.getInt(rf1 + 4), 8) + " R"
						+ tz390.get_hex(mf1 + 1, 1) + "="
						+ tz390.get_hex(reg.getInt(rf1 + 12), 8) + " R"
						+ tz390.get_hex(mf2, 1) + "="
						+ tz390.get_hex(reg.getInt(rf2 + 4), 8));
			} else {
				put_ins_trace(" R" + tz390.get_hex(mf1, 1) + "="
						+ tz390.get_hex(reg.getInt(rf1 + 4), 8) + " R"
						+ tz390.get_hex(mf2, 1) + "="
						+ tz390.get_hex(reg.getInt(rf2 + 4), 8));
			}
		}
		if (ex_mode) {
			ex_restore();
		}
		psw_loc = psw_loc + 2;
	}

	private void ins_setup_rre() { // "RRE" 185 MSR oooo00rr
		psw_ins_len = 4;
		rf1 = mem_byte[psw_loc + 3] & 0xff;
		mf2 = rf1 & 0xf;
		rf2 = mf2 << 3;
		rf1 = (rf1 & 0xf0) >> 1;
		mf1 = rf1 >> 3;
		if (tz390.opt_trace) {
			if (opcode1 == 0xb3
					|| (opcode1 == 0xb2 && (opcode2 == 0x2d || opcode2 == 0x44 || opcode2 == 0x45))) {
				put_ins_trace(" F" + tz390.get_hex(mf1, 1) + "="
						+ get_fp_long_hex(rf1) + " F" + tz390.get_hex(mf2, 1)
						+ "=" + get_fp_long_hex(rf2));
			} else if (opcode1 == 0xb3
					&& (opcode2 == 0xb4 || opcode2 == 0xb5 || opcode2 == 0xb6
							|| opcode2 == 0xc4 || opcode2 == 0xc5 || opcode2 == 0xc6)) {
				put_ins_trace(" F" + tz390.get_hex(mf1, 1) + "="
						+ get_fp_long_hex(rf1) + " R" + tz390.get_hex(mf2, 1)
						+ "=" + get_long_hex(reg.getLong(rf2)));
			} else if (opcode1 == 0xb9) {
				if (opcode2 >= 0x90 && opcode2 <= 0x93) { // RPI 454
					put_ins_trace(" R" + tz390.get_hex(mf1, 1) + "="
							+ tz390.get_hex(reg.getInt(rf1 + 4), 8) + " R"
							+ tz390.get_hex(mf1 + 1, 1) + "="
							+ tz390.get_hex(reg.getInt(rf1 + 12), 8) + " R"
							+ tz390.get_hex(mf2, 1) + "="
							+ tz390.get_hex(reg.getInt(rf2 + 4), 8) + " M="
							+ tz390.get_hex(mem.get(psw_loc + 2) >> 4, 1));
				} else {
					put_ins_trace(" R" + tz390.get_hex(mf1, 1) + "="
							+ get_long_hex(reg.getLong(rf1)) + " R"
							+ tz390.get_hex(mf2, 1) + "="
							+ get_long_hex(reg.getLong(rf2)));
				}
			} else {
				put_ins_trace(" R" + tz390.get_hex(mf1, 1) + "="
						+ tz390.get_hex(reg.getInt(rf1 + 4), 8) + " R"
						+ tz390.get_hex(mf2, 1) + "="
						+ tz390.get_hex(reg.getInt(rf2 + 4), 8));
			}
		}
		if (ex_mode) {
			ex_restore();
		}
		psw_loc = psw_loc + 4;
	}

	private void ins_setup_rrf1() { // RPI 206 "RRF1" 28 MAER oooor0rr (r1,r3,r2
									// maps to oooo1032)
		psw_ins_len = 4;
		rf1 = (mem_byte[psw_loc + 2] & 0xf0) >> 1;
		mf1 = rf1 >> 3;
		rf3 = mem_byte[psw_loc + 3];
		mf2 = rf3 & 0x0f;
		rf2 = mf2 << 3;
		rf3 = (rf3 & 0xf0) >> 1;
		mf3 = rf3 >> 3;
		if (tz390.opt_trace) {
			put_ins_trace(" F" + tz390.get_hex(mf1, 1) + "="
					+ get_fp_long_hex(rf1) + " F" + tz390.get_hex(mf3, 1) + "="
					+ get_fp_long_hex(rf3) + " F" + tz390.get_hex(mf2, 1) + "="
					+ get_fp_long_hex(rf2));
		}
		if (ex_mode) {
			ex_restore();
		}
		psw_loc = psw_loc + 4;
	}

	private void ins_setup_rrf2() { // RPI 206 "RRF2" 28 FIEBR oooo3012
		psw_ins_len = 4;
		rf3 = (mem_byte[psw_loc + 2] & 0xf0) >> 1; // RPI 206 RPI 335
		mf3 = rf3 >> 3; // RPI 335
		rf1 = mem_byte[psw_loc + 3];
		mf2 = rf1 & 0x0f;
		rf2 = mf2 << 3;
		rf1 = (rf1 & 0xf0) >> 1;
		mf1 = rf1 >> 3;
		if (tz390.opt_trace) {
			if ((opcode2 >= 0x98 && opcode2 <= 0x9a)
					|| (opcode2 >= 0xb8 && opcode2 <= 0xba)) {
				put_ins_trace(" R" + tz390.get_hex(mf1, 1) + "="
						+ tz390.get_hex(reg.getInt(rf1 + 4), 8) + " M3="
						+ tz390.get_hex(mf3, 1) + " F" + tz390.get_hex(mf2, 1)
						+ "=" + get_fp_long_hex(rf2));
			} else if ((opcode2 >= 0xa8 && opcode2 <= 0xaa)
					|| (opcode2 >= 0xc8 && opcode2 <= 0xca)) {
				put_ins_trace(" R" + tz390.get_hex(mf1, 1) + "="
						+ get_long_hex(reg.getLong(rf1)) + " M3="
						+ tz390.get_hex(mf3, 1) + " F" + tz390.get_hex(mf2, 1)
						+ "=" + get_fp_long_hex(rf2));
			} else {
				put_ins_trace(" F" + tz390.get_hex(mf1, 1) + "="
						+ get_fp_long_hex(rf1) + " M3=" + tz390.get_hex(mf3, 1)
						+ " F" + tz390.get_hex(mf2, 1) + "="
						+ get_fp_long_hex(rf2));
			}
		}
		if (ex_mode) {
			ex_restore();
		}
		psw_loc = psw_loc + 4;
	}

	private void ins_setup_rrf3() { // RPI 206 "RRF3" 28 DIEBR/DIDBR oooor0rr
									// (r1,r3,r2,m4 maps to oooo3412)
		psw_ins_len = 4;
		rf3 = mem_byte[psw_loc + 2];
		mf4 = rf3 & 0xf;
		rf3 = (rf3 & 0xf0) >> 1;
		mf3 = rf3 >> 3;
		rf1 = mem_byte[psw_loc + 3];
		mf2 = rf1 & 0xf;
		rf2 = mf2 << 3;
		rf1 = (rf1 & 0xf0) >> 1;
		mf1 = rf1 >> 3;
		if (tz390.opt_trace) {
			put_ins_trace(" F" + tz390.get_hex(mf1, 1) + "="
					+ get_fp_long_hex(rf1) + " F" + tz390.get_hex(mf3, 1) + "="
					+ get_fp_long_hex(rf3) + " F" + tz390.get_hex(mf2, 1) + "="
					+ get_fp_long_hex(rf2) + " M4=" + tz390.get_hex(mf4, 1));
		}
		if (ex_mode) {
			ex_restore();
		}
		psw_loc = psw_loc + 4;
	}

	private void ins_setup_rrf4() { // RPI 407 "RRF4" 28 CSDTR oooo0312
		psw_ins_len = 4;
		mf4 = mem_byte[psw_loc + 2] & 0xf;
		rf1 = mem_byte[psw_loc + 3];
		mf2 = rf1 & 0x0f;
		rf2 = mf2 << 3;
		rf1 = (rf1 & 0xf0) >> 1;
		mf1 = rf1 >> 3;
		if (tz390.opt_trace) {
			if ((opcode2 >= 0x98 && opcode2 <= 0x9a)
					|| (opcode2 >= 0xb8 && opcode2 <= 0xba)) {
				put_ins_trace(" R" + tz390.get_hex(mf1, 1) + "="
						+ tz390.get_hex(reg.getInt(rf1 + 4), 8) + " M3="
						+ tz390.get_hex(mf3, 1) + " F" + tz390.get_hex(mf2, 1)
						+ "=" + get_fp_long_hex(rf2));
			} else if ((opcode2 >= 0xa8 && opcode2 <= 0xaa)
					|| (opcode2 >= 0xc8 && opcode2 <= 0xca)) {
				put_ins_trace(" R" + tz390.get_hex(mf1, 1) + "="
						+ get_long_hex(reg.getLong(rf1)) + " M3="
						+ tz390.get_hex(mf3, 1) + " F" + tz390.get_hex(mf2, 1)
						+ "=" + get_fp_long_hex(rf2));
			} else if ((opcode1 == 0xb3 && opcode2 == 0xb3)) {
				put_ins_trace(" R" + tz390.get_hex(mf1, 1) + "="
						+ get_long_hex(reg.getLong(rf1)) + " F"
						+ tz390.get_hex(mf2, 1) + "=" + get_fp_long_hex(rf2)
						+ " M4=" + tz390.get_hex(mf4, 1));
			} else {
				put_ins_trace(" F" + tz390.get_hex(mf1, 1) + "="
						+ get_fp_long_hex(rf1) + " M3=" + tz390.get_hex(mf3, 1)
						+ " F" + tz390.get_hex(mf2, 1) + "="
						+ get_fp_long_hex(rf2));
			}
		}
		if (ex_mode) {
			ex_restore();
		}
		psw_loc = psw_loc + 4;
	}

	private void ins_setup_rrr() { // RPI 407 "ADTR" oooo3012 (r1,r2,r3
		psw_ins_len = 4;
		rf3 = (mem_byte[psw_loc + 2] & 0xf0) >> 1;
		mf3 = rf3 >> 3;
		rf1 = mem_byte[psw_loc + 3];
		mf2 = rf1 & 0x0f;
		rf2 = mf2 << 3;
		rf1 = (rf1 & 0xf0) >> 1;
		mf1 = rf1 >> 3;
		if (tz390.opt_trace) {
			put_ins_trace(" F" + tz390.get_hex(mf1, 1) + "="
					+ get_fp_long_hex(rf1) + " F" + tz390.get_hex(mf2, 1) + "="
					+ get_fp_long_hex(rf2) + " F" + tz390.get_hex(mf3, 1) + "="
					+ get_fp_long_hex(rf3));
		}
		if (ex_mode) {
			ex_restore();
		}
		psw_loc = psw_loc + 4;
	}

	private void ins_setup_rs() { // "RS" 25 oorrbddd
		/*
		 * fetch rf1,rf3,bf2,df2 and update psw
		 */
		rf1 = mem_byte[psw_loc + 1] & 0xff;
		mf3 = rf1 & 0xf;
		rf3 = mf3 << 3;
		rf1 = (rf1 & 0xf0) >> 1;
		mf1 = rf1 >> 3;
		bf2 = mem.getShort(psw_loc + 2) & 0xffff;
		df2 = bf2 & 0xfff;
		bf2 = (bf2 & 0xf000) >> 9;
		if (bf2 > 0) {
			bd2_loc = (reg.getInt(bf2 + 4) & psw_amode) + df2;
		} else {
			bd2_loc = df2;
		}
		psw_ins_len = 4;
		if (tz390.opt_trace) {
			if (opcode1 == 0xbd // CLM
					|| opcode1 == 0xbe // STCM
					|| opcode1 == 0xbf) { // ICM
				put_ins_trace(" R" + tz390.get_hex(mf1, 1) + "="
						+ tz390.get_hex(reg.getInt(rf1 + 4), 8) + " M3="
						+ tz390.get_hex(mf3, 1) + " S2("
						+ tz390.get_hex(bd2_loc, 8) + ")="
						+ bytes_to_hex(mem, bd2_loc, mask_bits[mf3], 0));
			} else if (opcode1 >= 0x88 && opcode1 <= 0x8f) {
				put_ins_trace(" R" + tz390.get_hex(mf1, 1) + "="
						+ tz390.get_hex(reg.getInt(rf1 + 4), 8) + " S2("
						+ tz390.get_hex(bd2_loc, 8) + ")");
			} else {
				put_ins_trace(" R" + tz390.get_hex(mf1, 1) + "="
						+ tz390.get_hex(reg.getInt(rf1 + 4), 8) + " R"
						+ tz390.get_hex(mf3, 1) + "="
						+ tz390.get_hex(reg.getInt(rf3 + 4), 8) + " S2("
						+ tz390.get_hex(bd2_loc, 8) + ")="
						+ bytes_to_hex(mem, bd2_loc, 4, 0));
			}
		}
		if (ex_mode) {
			ex_restore();
		}
		psw_loc = psw_loc + 4;
		if (bd2_loc >= tot_mem) {
			set_psw_check(psw_pic_addr);
		}
	}

	private void ins_setup_rsi() { // "RSI" 4 BRXH oorriiii
		psw_ins_len = 4;
		rf1 = mem_byte[psw_loc + 1] & 0xff;
		mf3 = rf1 & 0xf;
		rf3 = mf3 << 3;
		rf1 = (rf1 & 0xf0) >> 1;
		mf1 = rf1 >> 3;
		if2 = mem.getShort(psw_loc + 2);
		if (tz390.opt_trace) {
			if (opcode1 >= 0x84 && opcode1 <= 0x87) {
				rf2 = psw_loc + 2 * if2;
				put_ins_trace(" R" + tz390.get_hex(mf1, 1) + "="
						+ tz390.get_hex(reg.getInt(rf1 + 4), 8) + " R"
						+ tz390.get_hex(mf3, 1) + "="
						+ tz390.get_hex(reg.getInt(rf3 + 4), 8) + " S2("
						+ tz390.get_hex(rf2, 8) + ")=" + get_ins_target(rf2));
			} else {
				put_ins_trace(" R" + tz390.get_hex(mf1, 1) + "="
						+ tz390.get_hex(reg.getInt(rf1 + 4), 8) + " R"
						+ tz390.get_hex(mf3, 1) + "="
						+ tz390.get_hex(reg.getInt(rf3 + 4), 8) + " I2="
						+ tz390.get_hex(if2, 8));
			}
		}
		if (ex_mode) {
			ex_restore();
		}
		psw_loc = psw_loc + 4;
	}

	private void ins_setup_rsl() { // "RSL" 1 TP oor0bddd00oo
		psw_ins_len = 6;
		rflen1 = ((mem_byte[psw_loc + 1] & 0xff) >> 4) + 1;
		bf1 = mem.getShort(psw_loc + 2) & 0xffff;
		df1 = bf1 & 0xfff;
		bf1 = (bf1 & 0xf000) >> 9;
		if (bf1 > 0) {
			bd1_loc = (reg.getInt(bf1 + 4) & psw_amode) + df1;
		} else {
			bd1_loc = df1;
		}
		if (tz390.opt_trace) {
			put_ins_trace(" S1(" + tz390.get_hex(bd1_loc, 8) + ")="
					+ bytes_to_hex(mem, bd1_loc, rflen1, 0));
		}
		if (ex_mode) {
			ex_restore();
		}
		psw_loc = psw_loc + 6;
	}

	private void ins_setup_rsy() { // "RSY" 31 LMG oorrbdddhhoo
		psw_ins_len = 6;
		rf1 = mem_byte[psw_loc + 1] & 0xff;
		mf3 = rf1 & 0xf;
		rf3 = mf3 << 3;
		rf1 = (rf1 & 0xf0) >> 1;
		mf1 = rf1 >> 3;
		bf2 = mem.getShort(psw_loc + 2);
		df2 = (bf2 & 0xfff) | (mem.get(psw_loc + 4) << 12); // RPI 387
		bf2 = (bf2 & 0xf000) >> 9;
		if (bf2 > 0) {
			bd2_loc = (reg.getInt(bf2 + 4) & psw_amode) + df2;
		} else {
			bd2_loc = df2;
		}
		if (tz390.opt_trace) {
			if (opcode2 == 0x20 // CLMH
					|| opcode2 == 0x2C // STCH
					|| opcode2 == 0x80 // ICMH
			) {
				put_ins_trace(" R" + tz390.get_hex(mf1, 1) + "="
						+ get_long_hex(reg.getLong(rf1)) + " M3="
						+ tz390.get_hex(mf3, 1) + " S2("
						+ tz390.get_hex(bd2_loc, 8) + ")="
						+ bytes_to_hex(mem, bd2_loc, mask_bits[mf3], 0));
			} else if (opcode2 >= 0x21 // CLMY
					|| opcode2 == 0x2D // STCY
					|| opcode2 >= 0x81) { // ICMY
				put_ins_trace(" R" + tz390.get_hex(mf1, 1) + "="
						+ tz390.get_hex(reg.getInt(rf1 + 4), 8) + " M3="
						+ tz390.get_hex(mf3, 1) + " S2("
						+ tz390.get_hex(bd2_loc, 8) + ")="
						+ bytes_to_hex(mem, bd2_loc, mask_bits[mf3], 0));
			} else if (opcode2 == 0x1c || opcode2 == 0x1d) {
				put_ins_trace(" R" + tz390.get_hex(mf1, 1) + "="
						+ get_long_hex(reg.getLong(rf1)) + " R"
						+ tz390.get_hex(mf3, 1) + "="
						+ get_long_hex(reg.getLong(rf3)) + " S2("
						+ tz390.get_hex(bd2_loc, 8) + ")");
			} else {
				put_ins_trace(" R" + tz390.get_hex(mf1, 1) + "="
						+ tz390.get_hex(reg.getInt(rf1 + 4), 8) + " R"
						+ tz390.get_hex(mf3, 1) + "="
						+ tz390.get_hex(reg.getInt(rf3 + 4), 8) + " S2("
						+ tz390.get_hex(bd2_loc, 8) + ")="
						+ bytes_to_hex(mem, bd2_loc, 4, 0));
			}
		}
		if (ex_mode) {
			ex_restore();
		}
		psw_loc = psw_loc + 6;
		if (bd2_loc >= tot_mem) {
			set_psw_check(psw_pic_addr);
		}
	}

	private void ins_setup_rx() { // "RX" 52 L oorxbddd
		/*
		 * fetch rf1,xf2,bf2,df2 and update psw
		 */
		rf1 = mem_byte[psw_loc + 1] & 0xff;
		mf1 = rf1 >> 4;
		xf2 = (rf1 & 0xf) << 3;
		rf1 = (rf1 & 0xf0) >> 1;
		bf2 = mem.getShort(psw_loc + 2) & 0xffff;
		df2 = bf2 & 0xfff;
		bf2 = (bf2 & 0xf000) >> 9;
		if (xf2 > 0) {
			xbd2_loc = (reg.getInt(xf2 + 4) & psw_amode) + df2;
		} else {
			xbd2_loc = df2;
		}
		if (bf2 > 0) {
			xbd2_loc = xbd2_loc + (reg.getInt(bf2 + 4) & psw_amode);
		}
		psw_ins_len = 4;
		if (tz390.opt_trace) {
			if (opcode1 == 0x47) {
				put_ins_trace(" S2(" + tz390.get_hex(xbd2_loc, 8) + ")="
						+ get_ins_target(xbd2_loc));
			} else if (opcode1 == 0x45) {
				put_ins_trace(" R" + tz390.get_hex(mf1, 1) + "="
						+ tz390.get_hex(reg.getInt(rf1 + 4), 8) + " S2("
						+ tz390.get_hex(xbd2_loc, 8) + ")="
						+ get_ins_target(xbd2_loc));
			} else if (opcode1 == 0x41) {
				put_ins_trace(" R" + tz390.get_hex(mf1, 1) + "="
						+ tz390.get_hex(reg.getInt(rf1 + 4), 8) + " S2("
						+ tz390.get_hex(xbd2_loc & psw_amode, 8) + ")");
			} else if (opcode1 == 0x40 // RPI 348
					|| (opcode1 >= 0x48 && opcode1 <= 0x4c)) {
				put_ins_trace(" R" + tz390.get_hex(mf1, 1) + "="
						+ tz390.get_hex(reg.getInt(rf1 + 4), 8) + " S2("
						+ tz390.get_hex(xbd2_loc & psw_amode, 8) + ")="
						+ bytes_to_hex(mem, xbd2_loc, 2, 0));
			} else if (opcode1 >= 0x60 && opcode1 <= 0x7f) {
				put_ins_trace(" F" + tz390.get_hex(mf1, 1) + "="
						+ get_fp_long_hex(rf1) + " S2("
						+ tz390.get_hex(xbd2_loc, 8) + ")="
						+ bytes_to_hex(mem, xbd2_loc, 8, 0));
			} else if (opcode1 == 0x5D // divide
					|| opcode1 == 0x5C) { // mult.
				put_ins_trace(" R" + tz390.get_hex(mf1, 1) + "="
						+ tz390.get_hex(reg.getInt(rf1 + 4), 8) + " R"
						+ tz390.get_hex(mf1 + 1, 1) + "="
						+ tz390.get_hex(reg.getInt(rf1 + 12), 8) + " S2("
						+ tz390.get_hex(xbd2_loc, 8) + ")="
						+ bytes_to_hex(mem, xbd2_loc, 4, 0));
			} else if (opcode1 == 0x42 || opcode1 == 0x43) { // RPI 395
																// IC,STC 1 byte
				put_ins_trace(" R" + tz390.get_hex(mf1, 1) + "="
						+ tz390.get_hex(reg.getInt(rf1 + 4), 8) + " S2("
						+ tz390.get_hex(xbd2_loc & psw_amode, 8) + ")="
						+ bytes_to_hex(mem, xbd2_loc, 1, 0));
			} else {
				put_ins_trace(" R" + tz390.get_hex(mf1, 1) + "="
						+ tz390.get_hex(reg.getInt(rf1 + 4), 8) + " S2("
						+ tz390.get_hex(xbd2_loc, 8) + ")="
						+ bytes_to_hex(mem, xbd2_loc, 4, 0));
			}
		}
		if (ex_mode) {
			ex_restore();
		}
		psw_loc = psw_loc + 4;
		if (xbd2_loc >= tot_mem && opcode1 != 0x41) { // RPI 299
			set_psw_check(psw_pic_addr);
		}
	}

	private void ins_setup_rxf() { // "RXF" 8 MAE oorxbdddr0oo (note r3 before
									// r1)
		psw_ins_len = 6;
		rf3 = mem_byte[psw_loc + 1] & 0xff;
		xf2 = (rf3 & 0xf) << 3;
		rf3 = (rf3 & 0xf0) >> 1;
		mf3 = rf3 >> 3;
		rf1 = mem_byte[psw_loc + 4] & 0xf0;
		rf1 = rf1 >> 1;
		mf1 = rf1 >> 3;
		bf2 = mem.getShort(psw_loc + 2) & 0xffff;
		df2 = bf2 & 0xfff;
		bf2 = (bf2 & 0xf000) >> 9;
		if (xf2 > 0) {
			xbd2_loc = (reg.getInt(xf2 + 4) & psw_amode) + df2;
		} else {
			xbd2_loc = df2;
		}
		if (bf2 > 0) {
			xbd2_loc = xbd2_loc + (reg.getInt(bf2 + 4) & psw_amode);
		}
		if (tz390.opt_trace) {
			put_ins_trace(" F" + tz390.get_hex(mf1, 1) + "="
					+ get_fp_long_hex(rf1) + " F" + tz390.get_hex(mf3, 1) + "="
					+ get_fp_long_hex(rf3) + " S2(" + tz390.get_hex(bd2_loc, 8)
					+ ")=" + bytes_to_hex(mem, bd2_loc, 4, 0));
		}
		if (ex_mode) {
			ex_restore();
		}
		psw_loc = psw_loc + 6;
		if (xbd2_loc >= tot_mem) { // RPI 299
			set_psw_check(psw_pic_addr);
		}
	}

	private void ins_setup_rxe() { // "RXE" 28 ADB oorxbddd00oo
		psw_ins_len = 6;
		rf1 = mem_byte[psw_loc + 1] & 0xff;
		xf2 = (rf1 & 0xf) << 3;
		rf1 = (rf1 & 0xf0) >> 1;
		mf1 = rf1 >> 3;
		bf2 = mem.getShort(psw_loc + 2) & 0xffff;
		df2 = bf2 & 0xfff;
		bf2 = (bf2 & 0xf000) >> 9;
		if (xf2 > 0) {
			xbd2_loc = (reg.getInt(xf2 + 4) & psw_amode) + df2;
		} else {
			xbd2_loc = df2;
		}
		if (bf2 > 0) {
			xbd2_loc = xbd2_loc + (reg.getInt(bf2 + 4) & psw_amode);
		}
		if (tz390.opt_trace) {
			put_ins_trace(" F" + tz390.get_hex(mf1, 1) + "="
					+ get_fp_long_hex(rf1) + " S2("
					+ tz390.get_hex(xbd2_loc, 8) + ")="
					+ bytes_to_hex(mem, xbd2_loc, 8, 0));

		}
		if (ex_mode) {
			ex_restore();
		}
		psw_loc = psw_loc + 6;
		if (xbd2_loc >= tot_mem) { // RPI 299
			set_psw_check(psw_pic_addr);
		}
	}

	private void ins_setup_rxy() { // "RXY" 76 MLG oorxbdddhhoo
		psw_ins_len = 6;
		rf1 = mem_byte[psw_loc + 1] & 0xff;
		xf2 = (rf1 & 0xf) << 3;
		rf1 = (rf1 & 0xf0) >> 1;
		mf1 = rf1 >> 3;
		bf2 = mem.getShort(psw_loc + 2);
		df2 = (bf2 & 0xfff) | (mem.get(psw_loc + 4) << 12); // RPI 387
		bf2 = (bf2 & 0xf000) >> 9;
		if (xf2 > 0) {
			xbd2_loc = (reg.getInt(xf2 + 4) & psw_amode) + df2;
		} else {
			xbd2_loc = df2;
		}
		if (bf2 > 0) {
			xbd2_loc = xbd2_loc + (reg.getInt(bf2 + 4) & psw_amode);
		}
		if (tz390.opt_trace) {
			if (opcode1 == 0x47) {
				put_ins_trace(" S2(" + tz390.get_hex(xbd2_loc, 8) + ")="
						+ get_ins_target(xbd2_loc));
			} else {
				if (opcode1 == 0xb3 // RPI 348
						&& (opcode2 == 0x70
								|| (opcode2 >= 0x78 && opcode2 <= 0x7b)
								|| opcode2 == 0x91 || opcode2 == 0x95)) {
					put_ins_trace(" R" + tz390.get_hex(mf1, 1) + "="
							+ get_long_hex(reg.getLong(rf1)) + " S2("
							+ tz390.get_hex(xbd2_loc, 8) + ")="
							+ bytes_to_hex(mem, xbd2_loc, 4, 0));
				} else if (opcode1 == 0xe3 // RPI 348
						&& (opcode2 >= 0x78 && opcode2 <= 0x7b)) {
					put_ins_trace(" R" + tz390.get_hex(mf1, 1) + "="
							+ tz390.get_hex(reg.getInt(rf1), 8) + " S2("
							+ tz390.get_hex(xbd2_loc, 8) + ")="
							+ bytes_to_hex(mem, xbd2_loc, 2, 0));
				} else {
					put_ins_trace(" R" + tz390.get_hex(mf1, 1) + "="
							+ get_long_hex(reg.getLong(rf1)) + " S2("
							+ tz390.get_hex(xbd2_loc, 8) + ")="
							+ bytes_to_hex(mem, xbd2_loc, 8, 0));
				}
			}
		}
		if (ex_mode) {
			ex_restore();
		}
		psw_loc = psw_loc + 6;
		if (xbd2_loc >= tot_mem) { // RPI 299
			set_psw_check(psw_pic_addr);
		}
	}

	private void ins_setup_s() { // "S" 43 SPM oo00bddd
		bf2 = mem.getShort(psw_loc + 2) & 0xffff;
		df2 = bf2 & 0xfff;
		bf2 = (bf2 & 0xf000) >> 9;
		if (bf2 > 0) {
			bd2_loc = (reg.getInt(bf2 + 4) & psw_amode) + df2;
		} else {
			bd2_loc = df2;
		}
		psw_ins_len = 4;
		if (tz390.opt_trace) {
			put_ins_trace(" S2(" + tz390.get_hex(bd2_loc, 8) + ")="
					+ bytes_to_hex(mem, bd2_loc, 8, 0));
		}
		if (ex_mode) {
			ex_restore();
		}
		psw_loc = psw_loc + 4;
		if (bd2_loc >= tot_mem) { // RPI 299
			set_psw_check(psw_pic_addr);
		}
	}

	private void ins_setup_si() { // "SI" 9 CLI ooiibddd
		/*
		 * fetch bd1,if2
		 */
		if2 = mem_byte[psw_loc + 1] & 0xff;
		bf1 = mem.getShort(psw_loc + 2) & 0xffff;
		df1 = bf1 & 0xfff;
		bf1 = (bf1 & 0xf000) >> 9;
		if (bf1 > 0) {
			bd1_loc = (reg.getInt(bf1 + 4) & psw_amode) + df1;
		} else {
			bd1_loc = df1;
		}
		psw_ins_len = 4;
		if (tz390.opt_trace) {
			put_ins_trace(" S2(" + tz390.get_hex(bd1_loc, 8) + ")="
					+ bytes_to_hex(mem, bd1_loc, 1, 0) + " I2="
					+ tz390.get_hex(if2, 2));
		}
		if (ex_mode) {
			ex_restore();
		}
		psw_loc = psw_loc + 4;
		if (bd1_loc >= tot_mem) { // RPI 299
			set_psw_check(psw_pic_addr);
		}
	}

	private void ins_setup_siy() { // "SIY" 6 TMY ooiibdddhhoo
		psw_ins_len = 6;
		if2 = mem_byte[psw_loc + 1] & 0xff;
		bf1 = mem.getShort(psw_loc + 2);
		df1 = (bf1 & 0xfff) | (mem.get(psw_loc + 4) << 12); // RPI 387
		bf1 = (bf1 & 0xf000) >> 9;
		if (bf1 > 0) {
			bd1_loc = (reg.getInt(bf1 + 4) & psw_amode) + df1;
		} else {
			bd1_loc = df1;
		}
		if (tz390.opt_trace) {
			put_ins_trace(" S2(" + tz390.get_hex(bd1_loc, 8) + ")="
					+ bytes_to_hex(mem, bd1_loc, 1, 0) + " I2="
					+ tz390.get_hex(if2, 2));
		}
		if (ex_mode) {
			ex_restore();
		}
		psw_loc = psw_loc + 6;
		if (bd1_loc >= tot_mem) { // RPI 299
			set_psw_check(psw_pic_addr);
		}
	}

	private void ins_setup_ssp() { // AP SS2 oollbdddbddd
		/*
		 * fetch rflen1, rflen2, bd1_loc, and bd2_loc and update psw
		 */
		rflen1 = mem_byte[psw_loc + 1] & 0xff;
		rflen2 = (rflen1 & 0xf) + 1;
		rflen1 = ((rflen1 >> 4) & 0xf) + 1;
		bf1 = mem.getShort(psw_loc + 2) & 0xffff;
		df1 = bf1 & 0xfff;
		bf1 = (bf1 & 0xf000) >> 9;
		if (bf1 > 0) {
			bd1_loc = (reg.getInt(bf1 + 4) & psw_amode) + df1;
		} else {
			bd1_loc = df1;
		}
		bf2 = mem.getShort(psw_loc + 4) & 0xffff;
		df2 = bf2 & 0xfff;
		bf2 = (bf2 & 0xf000) >> 9;
		if (bf2 > 0) {
			bd2_loc = (reg.getInt(bf2 + 4) & psw_amode) + df2;
		} else {
			bd2_loc = df2;
		}
		psw_ins_len = 6;
		if (tz390.opt_trace) {
			int maxlen1 = rflen1;
			int maxlen2 = rflen2;
			if (maxlen1 > 16)
				maxlen1 = 16; // RPI 395
			if (maxlen2 > 16)
				maxlen2 = 16;
			put_ins_trace(" S1(" + tz390.get_hex(bd1_loc, 8) + ")="
					+ bytes_to_hex(mem, bd1_loc, maxlen1, 0) + " S2("
					+ tz390.get_hex(bd2_loc, 8) + ")="
					+ bytes_to_hex(mem, bd2_loc, maxlen2, 0));
		}
		if (ex_mode) {
			ex_restore();
		}
		psw_loc = psw_loc + 6;
		if (bd1_loc + rflen1 > tot_mem || bd2_loc + rflen2 > tot_mem) { // RPI
																		// 299
			set_psw_check(psw_pic_addr);
		}
	}

	private void ins_setup_ss() { // "SS" 32 MVC oollbdddbddd
		/*
		 * fetch rflen,bd1_loc, bd2_loc, and update psw
		 */
		rflen = (mem_byte[psw_loc + 1] & 0xff) + 1;
		bf1 = mem.getShort(psw_loc + 2) & 0xffff;
		df1 = bf1 & 0xfff;
		bf1 = (bf1 & 0xf000) >> 9;
		if (bf1 > 0) {
			bd1_loc = (reg.getInt(bf1 + 4) & psw_amode) + df1;
		} else {
			bd1_loc = df1;
		}
		bf2 = mem.getShort(psw_loc + 4) & 0xffff;
		df2 = bf2 & 0xfff;
		bf2 = (bf2 & 0xf000) >> 9;
		if (bf2 > 0) {
			bd2_loc = (reg.getInt(bf2 + 4) & psw_amode) + df2;
		} else {
			bd2_loc = df2;
		}
		psw_ins_len = 6;
		if (tz390.opt_trace) {
			int maxlen = rflen;
			if (maxlen > 16)
				maxlen = 16; // RPI 395
			put_ins_trace(" S1(" + tz390.get_hex(bd1_loc, 8) + ")="
					+ bytes_to_hex(mem, bd1_loc, maxlen, 0) + " S2("
					+ tz390.get_hex(bd2_loc, 8) + ")="
					+ bytes_to_hex(mem, bd2_loc, maxlen, 0));
		}
		if (ex_mode) {
			ex_restore();
		}
		psw_loc = psw_loc + 6;
		if (bd2_loc + rflen > tot_mem || bd1_loc + rflen > tot_mem) { // RPI
																		// 299
			set_psw_check(psw_pic_addr);
		}
	}

	private void ins_setup_sse() { // "SSE" 5 LASP oooobdddbddd
		psw_ins_len = 6;
		bf1 = mem.getShort(psw_loc + 2) & 0xffff;
		df1 = bf1 & 0xfff;
		bf1 = (bf1 & 0xf000) >> 9;
		if (bf1 > 0) {
			bd1_loc = (reg.getInt(bf1 + 4) & psw_amode) + df1;
		} else {
			bd1_loc = df1;
		}
		bf2 = mem.getShort(psw_loc + 4) & 0xffff;
		df2 = bf2 & 0xfff;
		bf2 = (bf2 & 0xf000) >> 9;
		if (bf2 > 0) {
			bd2_loc = (reg.getInt(bf2 + 4) & psw_amode) + df2;
		} else {
			bd2_loc = df2;
		}
		if (tz390.opt_trace) {
			put_ins_trace(" S1(" + tz390.get_hex(bd1_loc, 8) + ")="
					+ bytes_to_hex(mem, bd1_loc, 4, 0) + " S2("
					+ tz390.get_hex(bd2_loc, 8) + ")="
					+ bytes_to_hex(mem, bd2_loc, 4, 0));
		}
		if (ex_mode) {
			ex_restore();
		}
		psw_loc = psw_loc + 6;
		if (bd2_loc + rflen > tot_mem || bd1_loc + rflen > tot_mem) { // RPI
																		// 299
			set_psw_check(psw_pic_addr);
		}
	}

	private void put_ins_trace(String ins_parms) {
		/*
		 * put trace line to log file with ins parms and process test mode
		 * commands if option test
		 */
		ins_trace_line = " " + tz390.get_hex(psw_loc | psw_amode_bit, 8) + " "
				+ psw_cc_code[psw_cc] + " " + get_ins_hex(psw_loc) + " "
				+ get_ins_name(psw_loc) + ins_parms;
		if (tz390.opt_test) {
			sz390.put_log(ins_trace_line);
		} else {
			tz390.put_trace(ins_trace_line);
		}
	}

	public String get_ins_name(int ins_loc) {
		/*
		 * return opcode or ????? for given op1 and op2
		 */
		tz390.op_code_index = -1; // assume not found RPI 251
		ins_loc = ins_loc & psw_amode;
		if (ins_loc >= tot_mem) {
			return "?????";
		}
		int op1 = mem_byte[ins_loc] & 0xff;
		int op2 = -1;
		int op2_offset = opcode2_offset[op1];
		if (op2_offset > 0) {
			switch (opcode2_mask[op1]) {
			case 0xff: // full byte
				op2 = mem_byte[ins_loc + op2_offset] & 0xff;
				break;
			case 0xf0: // left nibble
				op2 = (mem_byte[ins_loc + op2_offset] >> 4) & 0x0f;
				break;
			case 0x0f: // right nibble
				op2 = mem_byte[ins_loc + op2_offset] & 0x0f;
				break;
			default:
				set_psw_check(psw_pic_operr);
			}
		}
		String ins_name = null;
		String hex_op1 = null;
		String hex_op2 = null;
		String hex_mf1 = null;
		String hex_key = null;
		hex_op1 = Integer.toHexString(op1).toUpperCase();
		if (hex_op1.length() == 1) {
			hex_op1 = "0" + hex_op1;
		}
		if (op2 == -1) {
			hex_op2 = "";
		} else {
			hex_op2 = Integer.toHexString(op2).toUpperCase();
			if (hex_op2.length() == 1) {
				hex_op2 = "0" + hex_op2;
			}
		}
		hex_mf1 = Integer.toHexString((mem_byte[ins_loc + 1] & 0xff) >> 4)
				.toUpperCase();
		if (op1 == 0x47 || op1 == 0x07) {
			hex_key = "BC=" + hex_op1 + "0" + hex_mf1;
		} else if (op1 == 0xa7 && ((op2 & 0xf) == 4)) {
			hex_key = "BR=" + hex_op1 + "4" + hex_mf1;
		} else if (op1 == 0xC0 && ((op2 & 0xf) == 4)) { // RPI200
			hex_key = "BL=" + hex_op1 + "4" + hex_mf1;
		} else {
			hex_key = hex_op1 + hex_op2;
		}
		tz390.op_code_index = tz390.find_key_index('H', hex_key);
		if (tz390.op_code_index != -1) {
			ins_name = tz390.op_name[tz390.op_code_index];
			if (ins_name.length() < 5) {
				ins_name = (ins_name + "     ").substring(0, 5);
			}
		} else {
			ins_name = "?????";
		}
		return ins_name;
	}

	/*
	 * *************************************** instruction support routines
	 * **************************************
	 */
	public void set_psw_check(int pic_type) {
		/*
		 * set psw_pic interruption type if pgm mask type interrupt if mask bit
		 * 0, set CC3 and continue. if espie exit defined, exit to it else set
		 * psw_check for exit to abend handler
		 */
		psw_pic = pic_type;
		if (psw_pic == psw_pic_exit) {
			psw_check = true; // normal exit
		} else {
			psw_pic_handler();
		}
	}

	public void psw_pic_handler() {
		/*
		 * set psw_loc and turn on psw_retry if any of the following conditions
		 * found: 1. If psw_pic has corresponding psw_pgm_mask bit off, continue
		 * at next instruction. 2. If psw_pic has corresponding bit on in ESPIE
		 * PIE mask bits, restart at espie_exit if defined. 3. If estae_exit
		 * defined, then restart at estae_exit. 4. If option test on 4. else
		 * issue svc_abend for psw_pic code.
		 */
		boolean try_espie = false;
		boolean ignore_pic = false;
		switch (psw_pic) {
		case 0x0c1:
			try_espie = true;
			if (!psw_check) {
				psw_pic = psw_pic_addr; // default 0C5 for ins trap
			}
			break;
		case 0x0c7: // decimal data or IEEE BFP exception
			switch (fp_dxc) {
			case 0x00: // packed decimal data exception
				try_espie = true;
				break;
			case 0x08: // IEEE inexact and truncated
			case 0x0C: // IEEE inexact and incremented
				if ((fp_fpc_reg & fp_fpc_mask_sig) != 0) {
					try_espie = true;
				} else {
					ignore_pic = true;
				}
				break;
			case 0x10: // IEEE underflow, exact
			case 0x18: // IEEE underflow, inexact and
			case 0x1C: // IEEE underflow, inexact and
				if ((fp_fpc_reg & fp_fpc_mask_unf) != 0) {
					try_espie = true;
				} else {
					ignore_pic = true;
				}
				break;
			case 0x20: // IEEE overflow, exact
			case 0x28: // IEEE overflow, inexact and
			case 0x2C: // IEEE overflow, inexact and
				if ((fp_fpc_reg & fp_fpc_mask_ovf) != 0) {
					try_espie = true;
				} else {
					ignore_pic = true;
				}
				break;
			case 0x40: // IEEE division by zero
				if ((fp_fpc_reg & fp_fpc_mask_div) != 0) {
					try_espie = true;
				} else {
					ignore_pic = true;
				}
				break;
			case 0x80: // IEEE invalid operation
				if ((fp_fpc_reg & fp_fpc_mask_inv) != 0) {
					try_espie = true;
				} else {
					ignore_pic = true;
				}
				break;
			}
			break;
		case 0x0c8: // fixed point overflow
			if ((psw_pgm_mask & psw_pgm_mask_fix) != 0) {
				try_espie = true; // try spie then stae
			} else {
				ignore_pic = true;
			}
			break;
		case 0x0ca: // decimal overflow
			if ((psw_pgm_mask & psw_pgm_mask_dec) != 0) {
				try_espie = true; // try spie then stae
			} else {
				ignore_pic = true;
			}
			break;
		case 0x0cd: // floating point exponent underflow
			if ((psw_pgm_mask & psw_pgm_mask_exp) != 0) {
				try_espie = true; // try spie then stae
			} else {
				ignore_pic = true;
			}
			break;
		case 0x0ce: // floating point significance
			if ((psw_pgm_mask & psw_pgm_mask_sig) != 0) {
				try_espie = true; // try spie then stae
			} else {
				ignore_pic = true;
			}
			break;
		case 0x0c2: // privalege
		case 0x0c3: // execute
		case 0x0c4: // protection
		case 0x0c5: // addressing (defualt for ins trap above
		case 0x0c6: // specification
		case 0x0c9: // fixed point divide
		case 0x0cb: // decimal divide
		case 0x0cc: // HFP floating point exp overflow
		case 0x0cf: // HFP floating point divide
			try_espie = true;
			break;
		}
		if (ignore_pic) {
			psw_check = false;
			return;
		}
		if (try_espie && tot_espie > 0 && !espie_exit_running // RPI 305
																// prevent
																// recursion
				&& !estae_exit_running) {
			int psw_int = psw_pic & 0xf;
			if ((espie_pie[tot_espie - 1] & (int_high_bit >>> psw_int)) != 0) {
				setup_espie_exit();
				return;
			}
		}
		if (tot_estae > 0 && !espie_exit_running // RPI 305 prevent recursion
				&& !estae_exit_running) {
			setup_estae_exit();
			return;
		}
		sz390.svc_abend(psw_pic, sz390.system_abend, sz390.tz390.opt_dump);
		test_trace_count = 0;
		psw_check = false;
	}

	private void setup_espie_exit() {
		/*
		 * initialize zcvt_epie and pass addr to espie exit via r1
		 */
		mem.putInt(epie_id, 0xc5d7c9c5); // id=c'EPIE'
		mem.putInt(epie_parm, espie_parm[tot_espie - 1]);
		mem.putInt(epie_psw, psw_pic); // intterrupt
		mem.putInt(epie_psw + 4, psw_loc); // psw addr
		mem.position(epie_gpr); // gpr64 r0-r15
		reg.position(0); // RPI 357
		mem.put(reg);
		reg.putInt(r1, zcvt_epie); // r1 = epie
		reg.putInt(r14, zcvt_exit); // r14 = exit
		reg.putInt(r15, espie_exit[tot_espie - 1]);
		set_psw_loc(espie_exit[tot_espie - 1]);
		espie_exit_running = true;
		psw_check = false;
		if (tz390.opt_trace) {
			tz390.put_trace("ESPIE EXIT STARTING");
		}
	}

	private void setup_estae_exit() {
		/*
		 * initialize zcvt_esta and pass addr to estae exit via r1
		 */
		mem.putInt(esta_id, 0xc5e2e3c1); // id=c'ESTA'
		mem.putInt(esta_parm, estae_parm[tot_estae - 1]);
		mem.putInt(esta_psw, psw_pic); // intterrupt
		mem.putInt(esta_psw + 4, psw_loc); // psw addr
		mem.position(esta_gpr); // gpr64 r0-r15
		reg.position(0); // RPI 357
		mem.put(reg);
		reg.putInt(r1, zcvt_esta); // r1 = epie
		reg.putInt(r14, zcvt_exit); // r14 = exit
		reg.putInt(r15, estae_exit[tot_estae - 1]);
		set_psw_loc(estae_exit[tot_estae - 1]);
		estae_exit_running = true;
		psw_check = false;
		if (tz390.opt_trace) {
			tz390.put_trace("ESTAE EXIT STARTING");
		}
	}

	public void set_psw_amode(int amode_bit) {
		/*
		 * set psw_amode based on amode high bit
		 */
		if ((amode_bit & psw_amode31_bit) != 0) {
			psw_amode = psw_amode31;
			psw_amode_bit = psw_amode31_bit;
		} else {
			psw_amode = psw_amode24;
			psw_amode_bit = psw_amode24_bit;
		}
		psw_amode_high_bits = (-1) ^ psw_amode;
	}

	public void set_psw_loc(int addr) {
		/*
		 * set psw_loc based on psw_amode and turn off ex_mode add add space if
		 * trace on
		 */
		psw_loc = addr & psw_amode;
		if (psw_loc >= tot_mem) {
			set_psw_check(psw_pic_addr);
		}
		if (tz390.opt_trace && !tz390.opt_test) {
			tz390.put_trace(""); // RPI 348
		}
		ex_mode = false;
	}

	private int get_tm_reg_cc(int reg, int mask) {
		/*
		 * return psw_cc for tm reg
		 */
		int bits = reg & mask;
		if (bits == 0) {
			return psw_cc0;
		} else if (bits == reg) {
			return psw_cc3;
		} else if (bits >= 0x8000) {
			return psw_cc2;
		} else {
			return psw_cc1;
		}
	}

	private int get_sla32(int int_reg, int shift_count) {
		/*
		 * return int shifted left arith and set psw_cc
		 */
		long long_result = ((long) int_reg) << shift_count;
		if (int_reg > 0) {
			if ((long_result & long_sign_bits) != 0) {
				psw_cc = psw_cc3;
				long_result = long_result & max_pos_int;
			} else {
				psw_cc = psw_cc2;
			}
		} else if (int_reg < 0) {
			if ((long_result & long_sign_bits) != long_sign_bits) {
				psw_cc = psw_cc3;
				long_result = long_result | long_sign_bits;
			} else {
				psw_cc = psw_cc1;
			}
		} else {
			psw_cc = psw_cc0;
		}
		return (int) long_result;
	}

	private long get_sla64(long long_reg, int shift_count) {
		/*
		 * return long_reg shifted left arith and set psw_cc
		 */
		long long_result = long_reg << shift_count;
		long shift_out_bits = long_reg >> 63 - shift_count;
		if (long_reg > 0) {
			if (shift_out_bits != 0) {
				psw_cc = psw_cc3;
				long_result = long_result & long_num_bits;
			} else {
				psw_cc = psw_cc2;
			}
		} else if (long_reg < 0) {
			if (shift_out_bits != -1) {
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

	private int get_sra32(int int_reg, int shift_count) {
		/*
		 * return int shifted right arith and set psw_cc
		 */
		int int_result = int_reg >> shift_count;
		psw_cc = get_int_comp_cc(int_result, 0);
		return int_result;
	}

	private int get_int_log_add_cc() {
		/*
		 * set psw_carry and return cc for logical add as follows: rv1 carry 0 0
		 * cc0 !0 0 cc1 0 1 cc2 !0 1 cc3 Notes: 1. rvw = r1 input 2. rv2 = r2
		 * input 3. rv1 = result r1+r2
		 */
		boolean rv1_carry = false;
		if (rv1 >= 0) { // RPI 376
			if (rvw < 0 || rv2 < 0) {
				rv1_carry = true;
			}
		} else if (rvw < 0 && rv2 < 0) {
			rv1_carry = true;
		}
		if (rv1 == 0) {
			if (!rv1_carry) {
				return psw_cc0;
			} else {
				return psw_cc2;
			}
		} else {
			if (!rv1_carry) {
				return psw_cc1;
			} else {
				return psw_cc3;
			}
		}
	}

	private int get_long_log_add_cc() {
		/*
		 * set psw_carry and return cc for logical add as follows: rlv1 carry 0
		 * 0 cc0 !0 0 cc1 0 1 cc2 !0 1 cc3
		 */
		boolean rlv1_carry = false;
		if (rlv1 >= 0) { // RPI 376
			if (rlvw < 0 || rlv2 < 0) {
				rlv1_carry = true;
			}
		} else if (rlvw < 0 && rlv2 < 0) {
			rlv1_carry = true;
		}
		if (rlv1 == 0) {
			if (!rlv1_carry) {
				return psw_cc0;
			} else {
				return psw_cc2;
			}
		} else {
			if (!rlv1_carry) {
				return psw_cc1;
			} else {
				return psw_cc3;
			}
		}
	}

	private int get_int_log_sub_cc() {
		/*
		 * return cc for logical subtract as follows: rlv borrow 0 1 cc0 (slb
		 * only) !0 1 cc1 0 0 cc2 !0 0 cc3 Notes: 1. rvw = r1 input 2. rv2 = r2
		 * input 3. rv1 = result
		 */
		boolean rv1_borrow = false;
		if (rvw >= 0) {
			if (rv2 < 0 || rv1 < 0) {
				rv1_borrow = true;
			}
		} else {
			if (rv2 < 0 && rv1 >= 0) {
				rv1_borrow = true;
			}
		}
		if (rv1 == 0) {
			if (!rv1_borrow) {
				return psw_cc2; // Z,NB
			} else {
				return psw_cc0; // Z,B
			}
		} else {
			if (!rv1_borrow) {
				return psw_cc3; // NZ,NB
			} else {
				return psw_cc1; // NZ,B
			}
		}
	}

	private int get_long_log_sub_cc() {
		/*
		 * return cc for logical subtract as follows: rlv borrow 0 1 cc0 (slb
		 * only) !0 1 cc1 0 0 cc2 !0 0 cc3
		 */
		boolean rlv1_borrow = false;
		if (rlvw >= 0) {
			if (rlv2 < 0 || rlv1 < 0) {
				rlv1_borrow = true;
			}
		} else {
			if (rlv2 < 0 && rlv1 >= 0) {
				rlv1_borrow = true;
			}
		}
		if (rlv1 == 0) {
			if (!rlv1_borrow) {
				return psw_cc2; // Z,NB
			} else {
				return psw_cc0; // Z,B
			}
		} else {
			if (!rlv1_borrow) {
				return psw_cc3; // NZ,NB
			} else {
				return psw_cc1; // NZ,B
			}
		}
	}

	private byte[] get_log_bytes(byte[] data_byte, int data_offset, int data_len) {
		/*
		 * return byte array with leading 0 byte followed by data bytes. This
		 * array format is used to initialize BigInteger with logical unsigned
		 * value.
		 */
		byte[] new_byte = new byte[data_len + 1];
		System.arraycopy(data_byte, data_offset, new_byte, 1, data_len); // RPI
																			// 411
		return new_byte;
	}

	private void zcvt_big_int_to_work_reg(BigInteger big_int, int work_reg_bytes) {
		/*
		 * copy big integer bytes to work reg with work reg size up to 16
		 */
		byte[] work_byte = big_int.toByteArray();
		byte extend_byte = 0;
		if (work_byte[0] < 0)
			extend_byte = -1;
		int extend_len = work_reg_bytes - work_byte.length;
		if (extend_len > 0) {
			Arrays.fill(work_reg_byte, 0, extend_len, extend_byte); // RPI 411
			System.arraycopy(work_byte, 0, work_reg_byte, extend_len,
					work_byte.length);
		} else {
			System.arraycopy(work_byte, work_byte.length - work_reg_bytes,
					work_reg_byte, 0, work_reg_bytes);
		}
	}

	private void exec_clm() {
		/*
		 * exec clmh or clm using rv1, db2_loc
		 */
		psw_cc = psw_cc_equal;
		if ((mf3 & 0x8) != 0) {
			rv2 = (rv1 >> 24) & 0xff;
			if2 = mem_byte[bd2_loc & psw_amode] & 0xff;
			bd2_loc++;
			if (rv2 != if2) {
				if (rv2 > if2) {
					psw_cc = psw_cc_high;
				} else {
					psw_cc = psw_cc_low;
				}
				return;
			}
		}
		if ((mf3 & 0x4) != 0) {
			rv2 = (rv1 >> 16) & 0xff;
			if2 = mem_byte[bd2_loc & psw_amode] & 0xff;
			bd2_loc++;
			if (rv2 != if2) {
				if (rv2 > if2) {
					psw_cc = psw_cc_high;
				} else {
					psw_cc = psw_cc_low;
				}
				return;
			}
		}
		if ((mf3 & 0x2) != 0) {
			rv2 = (rv1 >> 8) & 0xff;
			if2 = mem_byte[bd2_loc & psw_amode] & 0xff;
			bd2_loc++;
			if (rv2 != if2) {
				if (rv2 > if2) {
					psw_cc = psw_cc_high;
				} else {
					psw_cc = psw_cc_low;
				}
				return;
			}
		}
		if ((mf3 & 0x1) != 0) {
			rv2 = rv1 & 0xff;
			if2 = mem_byte[bd2_loc & psw_amode] & 0xff;
			if (rv2 != if2) {
				if (rv2 > if2) {
					psw_cc = psw_cc_high;
				} else {
					psw_cc = psw_cc_low;
				}
				return;
			}
		}
	}

	private void exec_clst() {
		/*
		 * compare strings r0 - ending byte rf1,rf2 - string addresses
		 */
		byte str_end_byte = reg.get(r0 + 3);
		int str1_loc = reg.getInt(rf1 + 4) & psw_amode;
		int str2_loc = reg.getInt(rf2 + 4) & psw_amode;
		int index = 0;
		psw_cc = psw_cc3;
		byte str1_byte;
		byte str2_byte;
		boolean str_ended = false;
		psw_cc = psw_cc0;
		while (!str_ended) {
			str1_byte = mem_byte[str1_loc + index];
			str2_byte = mem_byte[str2_loc + index];
			if (str1_byte == str_end_byte || str2_byte == str_end_byte) {
				return;
			} else {
				if (str1_byte == str2_byte) {
					index++;
				} else if (str1_byte > str2_byte) {
					psw_cc = psw_cc2;
					str_ended = true;
				} else {
					psw_cc = psw_cc1;
					str_ended = true;
				}
			}
		}
		reg.putInt(rf1 + 4, str1_loc + index);
		reg.putInt(rf2 + 4, str2_loc + index);
	}

	private void exec_cuse() {
		/*
		 * search for matching string at same offset in two fields: r0 = length
		 * up to 255 r1 = pad char rf1,rf2 = addr and lengh regs cc0 - key found
		 * and rf1, rf2 updated cc1 - ended at longer operand, last byte = cc2 -
		 * ended at longer operand, last byte != cc3 - search incomplete retry
		 */
		int key_len = reg.getInt(r0);
		byte str_pad = reg.get(r1 + 3);
		int str1_loc = reg.getInt(rf1 + 4) & psw_amode;
		int str1_len = reg.getInt(rf1 + 12);
		int str2_loc = reg.getInt(rf2 + 4) & psw_amode;
		int str2_len = reg.getInt(rf2 + 12);
		int max_str_len = str1_len;
		if (str2_len > str1_len) {
			max_str_len = str2_len;
		}
		int index1 = 0;
		int index2 = 0;
		byte str1_byte;
		byte str2_byte;
		psw_cc = psw_cc1;
		while (index1 < max_str_len && psw_cc != psw_cc0) {
			if (index1 < str1_len) {
				str1_byte = mem_byte[str1_loc + index1];
			} else {
				str1_byte = str_pad;
			}
			if (index1 < str2_len) {
				str2_byte = mem_byte[str2_loc + index1];
			} else {
				str2_byte = str_pad;
			}
			if (str1_byte == str2_byte) {
				psw_cc = psw_cc1;
				index2++;
				if (index2 == key_len) {
					psw_cc = psw_cc0;
				}
			} else {
				psw_cc = psw_cc2;
				index2 = 0;
			}
			index1++;
		}
		if (index1 < str1_len) {
			reg.putInt(rf1 + 4, str1_loc + index1 - key_len);
			reg.putInt(rf1 + 12, str1_len - index1 + key_len);
		} else {
			reg.putInt(rf1 + 4, str1_loc + str1_len);
			reg.putInt(rf1 + 12, 0);
		}
		if (index1 < str2_len) {
			reg.putInt(rf2 + 4, str2_loc + index1 - key_len);
			reg.putInt(rf2 + 12, str2_len - index1 + key_len);
		} else {
			reg.putInt(rf2 + 4, str2_loc + str2_len);
			reg.putInt(rf2 + 12, 0);
		}
	}

	private void exec_srst() {
		/*
		 * find char in r2 field which ends at r1 char is in r0 cc 1 = char
		 * found and r1 set to address cc 2 = char not found, no update cc 3 =
		 * instruction incomplete retry
		 */
		byte key_byte = reg.get(r0 + 3);
		int str_loc = reg.getInt(rf2 + 4) & psw_amode;
		int str_end = reg.getInt(rf1 + 4) & psw_amode;
		psw_cc = psw_cc2;
		while (str_loc < str_end) {
			if (mem_byte[str_loc] == key_byte) {
				psw_cc = psw_cc1;
				reg.putInt(rf1 + 4, str_loc);
				str_loc = str_end;
			}
			str_loc++;
		}
	}

	private void exec_stcm() {
		/*
		 * exec stcmh, stcmy, or stcm using rv1, db2_loc
		 */
		if ((mf3 & 0x8) != 0) {
			mem_byte[bd2_loc & psw_amode] = (byte) (rv1 >> 24);
			bd2_loc++;
		}
		if ((mf3 & 0x4) != 0) {
			mem_byte[bd2_loc & psw_amode] = (byte) (rv1 >> 16);
			bd2_loc++;
		}
		if ((mf3 & 0x2) != 0) {
			mem_byte[bd2_loc & psw_amode] = (byte) (rv1 >> 8);
			bd2_loc++;
		}
		if ((mf3 & 0x1) != 0) {
			mem_byte[bd2_loc & psw_amode] = (byte) rv1;
			bd2_loc++;
		}
	}

	private void exec_icm() {
		/*
		 * exec icmh, icmy, or icm using rv1, db2_loc Notes: 1. psw_cc =
		 * allinserted bits 0 or mask 0 2. psw_cc = 1 if first inserted bit 1 3.
		 * psw_cc = 2 if first inserted bit 0 and not all 0
		 */
		psw_cc = psw_cc_equal; // assume all zeros
		boolean first_insert = true;
		if ((mf3 & 0x8) != 0) {
			if2 = mem_byte[bd2_loc & psw_amode] & 0xff;
			rv1 = (rv1 & 0x00ffffff) | (if2 << 24);
			bd2_loc++;
			if (first_insert) { // RPI 303
				first_insert = false;
				if (if2 >= 0x80) {
					psw_cc = psw_cc_low; // RPI 295
				} else if (if2 != 0) {
					psw_cc = psw_cc_high; // RPI 295
				}
			}
		}
		if ((mf3 & 0x4) != 0) {
			if2 = mem_byte[bd2_loc & psw_amode] & 0xff;
			rv1 = (rv1 & 0xff00ffff) | (if2 << 16);
			bd2_loc++;
			if (first_insert) { // RPI 303
				first_insert = false;
				if (if2 >= 0x80) {
					psw_cc = psw_cc_low; // RPI 295
				} else if (if2 != 0) {
					psw_cc = psw_cc_high; // RPI 295
				}
			} else if (if2 != 0 & psw_cc == psw_cc_equal) {
				psw_cc = psw_cc_high;
			}
		}
		if ((mf3 & 0x2) != 0) {
			if2 = mem_byte[bd2_loc & psw_amode] & 0xff;
			rv1 = (rv1 & 0xffff00ff) | (if2 << 8);
			bd2_loc++;
			if (first_insert) { // RPI 303
				first_insert = false;
				if (if2 >= 0x80) {
					psw_cc = psw_cc_low; // RPI 295
				} else if (if2 != 0) {
					psw_cc = psw_cc_high; // RPI 295
				}
			} else if (if2 != 0 & psw_cc == psw_cc_equal) {
				psw_cc = psw_cc_high;
			}
		}
		if ((mf3 & 0x1) != 0) {
			if2 = mem_byte[bd2_loc & psw_amode] & 0xff;
			rv1 = ((rv1 & 0xffffff00) | if2);
			bd2_loc++;
			if (first_insert) { // RPI 303
				first_insert = false;
				if (if2 >= 0x80) {
					psw_cc = psw_cc_low; // RPI 295
				} else if (if2 != 0) {
					psw_cc = psw_cc_high; // RPI 295
				}
			} else if (if2 != 0 & psw_cc == psw_cc_equal) {
				psw_cc = psw_cc_high;
			}
		}
	}

	private void push_pc_stack(int link_addr) {
		/*
		 * push psw and regs on PC stack
		 */
		if (cur_pc_stk < max_pc_stk) {
			pc_stk_psw_loc[cur_pc_stk] = link_addr;
			pc_stk_psw_cc[cur_pc_stk] = psw_cc;
			pc_stk_reg.position(cur_pc_stk_reg);
			reg.position(0); // RPI 357
			pc_stk_reg.put(reg);
			cur_pc_stk++;
			cur_pc_stk_reg = cur_pc_stk_reg + reg_len;
		} else {
			set_psw_check(psw_pic_stkerr);
		}
	}

	private void pop_pc_stack() {
		/*
		 * pop psw and regs on PC stack
		 */
		if (cur_pc_stk > 0) {
			cur_pc_stk--;
			cur_pc_stk_reg = cur_pc_stk_reg - reg_len;
			set_psw_loc(pc_stk_psw_loc[cur_pc_stk]);
			psw_cc = pc_stk_psw_cc[cur_pc_stk];
			reg.position(8);
			reg.put(pc_stk_reg_byte, cur_pc_stk_reg + 8, reg_len - 16);
		} else {
			set_psw_check(psw_pic_stkerr);
		}
	}

	private String get_ins_target(int ins_loc) {
		/*
		 * return hex ins plus ins name with 1 space
		 */
		return get_ins_hex(ins_loc).trim() + " " + get_ins_name(ins_loc);
	}

	public String get_ins_hex(int ins_loc) {
		/*
		 * return 2, 4, or 6 byte hex instruction
		 */
		String hex;
		ins_loc = ins_loc & psw_amode;
		if (ins_loc >= tot_mem) {
			return "????????????";
		}
		int ins_op = mem_byte[ins_loc] & 0xff;
		if (ins_op < 0x40) {
			hex = bytes_to_hex(mem, ins_loc, 2, 0) + "        ";
		} else if (ins_op < 0xc0) {
			hex = bytes_to_hex(mem, ins_loc, 4, 0) + "    ";
		} else {
			hex = bytes_to_hex(mem, ins_loc, 6, 0);
		}
		return hex;
	}

	public String get_fp_long_hex(int reg_index) {
		/*
		 * Format fp reg into 16 byte hex string after flushing co-reg Note:
		 * This trace function slows donw fp by adding extra conversions.
		 */
		fp_store_reg(trace_reg, reg_index);
		String work_hex = Long.toHexString(trace_reg.getLong(reg_index));
		return ("0000000000000000" + work_hex).substring(work_hex.length())
				.toUpperCase();
	}

	public String get_long_hex(long work_long) {
		/*
		 * Format long into 16 byte hex string
		 */
		String work_hex = Long.toHexString(work_long);
		return ("0000000000000000" + work_hex).substring(work_hex.length())
				.toUpperCase();
	}

	public void trace_psw() {
		/*
		 * set opcode1 and opcode2 from psw_loc and then execute setup routine
		 * with trace on to generate formated instruction trace
		 */
		boolean save_opt_trace = tz390.opt_trace;
		int save_psw_loc = psw_loc;
		tz390.opt_trace = true;
		opcode1 = mem_byte[psw_loc] & 0xff;
		opcode2 = opcode2_offset[opcode1];
		if (opcode2 > 0) {
			opcode2 = mem_byte[psw_loc + opcode2] & opcode2_mask[opcode1];
		}
		get_ins_name(psw_loc); // set op_code_index for RPI 251
		if (tz390.op_code_index < 0) {
			put_ins_trace("");
			return;
		}
		switch (tz390.op_type[tz390.op_code_index]) { // RPI 251
		case 1: // "E" 8 PR oooo
			ins_setup_e();
			break;
		case 2: // "RR" 60 LR oorr
			ins_setup_rr();
			break;
		case 3:// "BRX" 16 BER oomr
			ins_setup_rr();
			break;
		case 4:// "I" 1 SVC 00ii
			ins_setup_i();
			break;
		case 5:// "RX" 52 L oorxbddd
			ins_setup_rx();
			break;
		case 6:// "BCX" 16 BE oomxbddd
			ins_setup_rx();
			break;
		case 7:// "S" 43 SPM oo00bddd
			ins_setup_s();
			break;
		case 8:// "DM" 1 DIAGNOSE 83000000
			ins_setup_dm();
			break;
		case 9:// "RSI" 4 BRXH oorriiii
			ins_setup_rsi();
			break;
		case 10:// "RS" 25 oorrbddd
			ins_setup_rs();
			break;
		case 11:// "SI" 9 CLI ooiibddd
			ins_setup_si();
			break;
		case 12:// "RI" 37 IIHH ooroiiii
			ins_setup_ri();
			break;
		case 13:// "BRC" 31 BRE oomoiiii
			ins_setup_ri();
			break;
		case 14:// "RRE" 185 MSR oooo00rr
			ins_setup_rre();
			break;
		case 15:// "RRF" 28 MAER oooor0rr
			ins_setup_rrf1();
			break;
		case 16:// "RIL" 6 BRCL oomollllllll
			ins_setup_ril();
			break;
		case 17:// "SS" 32 MVC oollbdddbddd
			ins_setup_ss();
			break;
		case 18:// "RXY" 76 MLG oorxbdddhhoo
			ins_setup_rxy();
			break;
		case 19:// "SSE" 5 LASP oooobdddbddd
			ins_setup_sse();
			break;
		case 20:// "RSY" 31 LMG oorrbdddhhoo
			ins_setup_rsy();
			break;
		case 21:// "SIY" 6 TMY ooiibdddhhoo
			ins_setup_siy();
			break;
		case 22:// "RSL" 1 TP oor0bddd00oo
			ins_setup_rsl();
			break;
		case 23:// "RIE" 4 BRXLG oorriiii00oo
			ins_setup_rie();
			break;
		case 24:// "RXE" 28 ADB oorxbddd00oo
			ins_setup_rxe();
			break;
		case 25:// "RXF" 8 MAE oorxbdddr0oo (note r3 before r1)
			ins_setup_rxf();
			break;
		case 26:// AP SS2 oollbdddbddd
			ins_setup_ssp();
			break;
		case 27:// PLO SS3 oorrbdddbddd r1,s2,r3,s4
			ins_setup_ss();
			break;
		case 28:// LMD SS5 oorrbdddbddd r1,r3,s2,s4
			ins_setup_ss();
			break;
		case 29:// SRP SS2 oolibdddbddd s1(l1),s2,i3
			ins_setup_ss();
			break;
		case 30:// RPI 206 "RRF3" 30 DIEBR/DIDBR oooormrr (r1,r3,r2,m4 maps to
				// oooo3412)
			ins_setup_rrf3();
			break;
		case 31:// "SS" PKA oollbdddbddd ll from S2
			ins_setup_ss();
			break;
		case 32: // "SSF" MVCOS oor0bdddbddd (s1,s2,r3) z9-41
			ins_setup_ss();
			break;
		case 33: // "BLX" BRCL extended mnemonics
			ins_setup_ril();
			break;
		case 34: // RPI 206 "RRF2" FIEBR (r1,m3,r2 maps to oooo3012)
			ins_setup_rrf2();
			break;
		}
		tz390.opt_trace = save_opt_trace;
		psw_loc = save_psw_loc;
	}

	private String trace_svc() { // RPI 312
		/*
		 * return extended svc trace information including svc function name and
		 * any key parms available such as pgm name or ddname where appropriate
		 */
		switch (if1) {
		case 0x01: // WAIT
			return "WAIT R0=0/COUNT R1=ECB/ECBLIST";
		case 0x02: // POST
			return "POST R1=ECB";
		case 0x03: // EXIT
			return "EXIT";
		case 0x04: // GETMAIN R0_BIT0=AMODE31, R0_BIT1=COND, R1=length
			return "GETMAIN R0 B0=RMODE31, R0B1=CONT, R1=LEN";
		case 0x05: // FREEMAIN R0=LEN, R1=ADDR RPI 244
			return "FREEMAIN R0=LEN, R1=ADDR";
		case 0x06: // LINK R0=PGMID, R1=PARMS
			return "LINK R1=PARMS R0=PGM("
					+ sz390.get_ascii_string(reg.getInt(r0) & psw_amode, 8)
							.toUpperCase() + ")";
		case 0x07: // XCTL R15=PGMID, R1=PARMS
			return "XCTL R1=PARMS R0=PGM("
					+ sz390.get_ascii_string(reg.getInt(r0) & psw_amode, 8)
							.toUpperCase() + ")";
		case 0x08: // LOAD R15=PGMID
			return "LOAD R1=PARMS R0=PGM("
					+ sz390.get_ascii_string(reg.getInt(r0) & psw_amode, 8)
							.toUpperCase() + ")";
		case 0x09: // DELETE R0=A(NAME)
			return "DELETE R1=PARMS R0=PGM("
					+ sz390.get_ascii_string(reg.getInt(r0) & psw_amode, 8)
							.toUpperCase() + ")";
		case 0x0b: // TIME R0_LH=DATETYPE, R0_LL=TIMETYPE, R1=ADDR
			return "TIME R0 LH=DATETYPE, R0 LL=TIMETYPE, R1=ADDR";
		case 0x0d: // ABEND R1=abend code
			return "ABEND R1=ABEND CODE";
		case 0x12: // BLDL FIND PGMS IN SYS390
			return "BLDL R1=BLDL LIST";
		case 0x13: // OPEN DCB R0=(x'40' input,x'20' output) R1=DCB
			return "OPEN R0(X'40'=INPUT.X'20'=OUTPUT), R1=DCB";
		case 0x14: // CLOSE DCB R1=DCB
			return "CLOSE R1=DCB";
		case 0x22: // SVC 34 R1=COMMAND R0+2=ID, R0+3=OP
			return "CMDPROC R0+2=ID, R0+3=OP, R1=COMMAND";
		case 0x23: // WTO R1=ADDR
			return "WTO R1=ADDR(AL2(LEN),AL2(FLAGS),C'MSG')";
		case 0x2e: // TTIMER r0=function, r1=mic addr
			return "TTIMER R0=OP, R1=ADDR";
		case 0x2f: // STIMER r0=flags, r1=rx storage arg.
			return "STIMER R0=OP, R1=ADDR";
		case 0x33: // SNAP r0hh=flags,r0ll=id,r14-15 storage
			return "SNAP R0 HH=FLAGS, R0 LL=ID, R14=START, R15=END";
		case 0x3c:
			return "ESTAE R1=EXIT ADDR"; // set abend exit R1=addr
		case 0x67:
			return "XLATE R0=ADDR(+ASC>ECB,-EBC>ASC), R1=LENGTH"; // translate
																	// ascii/EBCDIC
		case 0x6d:
			return "ESPIE R0=TYPE INT, R1=EXIT ADDR"; // set program check
														// exit R0=types,
														// R1=addr
		case 84: // gui application window I/O
			return "GUAM R0+2=MAJOR, R0+3=MINOR, R1=ADDR";
		case 93: // TGET/TPUT TN3290 data stream for GUI
			int flags = reg.get(r1) & 0xff;
			int len = reg.getShort(r0 + 2) & 0xffff;
			int show = len * 2;
			if (show > 16)
				show = 16;
			int addr = reg.getInt(r1) + psw_amode24;
			if (flags >= 0x80) {
				return "TGET" + " FLAGS=" + tz390.get_hex(flags, 2) + " LEN="
						+ tz390.get_hex(reg.getShort(r0 + 2), 2) + " ADDR="
						+ tz390.get_hex(addr, 6);
			} else {
				return "TPUT" + " FLAGS=" + tz390.get_hex(flags, 2) + " LEN="
						+ tz390.get_hex(reg.getShort(r0 + 2), 2) + " ADDR="
						+ tz390.get_hex(addr, 6);
			}
		case 151: // dcb get move R0=REC,R1=DCB
			return "GETMOVE R0=REC ADDR, R1=DCB";
		case 152: // dcb put move R0=REC, R1=DCB
			return "PUTMOVE R0=REC ADDR, R1=DCB";
		case 153: // dcb read R1=DECB
			return "READ R1=DECB";
		case 154: // dcb write R1=DECB
			return "WRITE R1=DECB";
		case 155: // decb check R1=DECB
			return "CHECK R1=DECB";
		case 156: // dcb point R1=DCB
			return "POINT R1=DCB";
		case 160: // wtor
			return "WTOR R0=REPLY, R1=MSG, R14=LEN, R15=ECB";
		case 170: // CTD
			return "CTD R1=A(TYPE,IN,OUT)";
		case 171: // CFD
			return "CFD R1=A(TYPE,OUT,IN)";
		default:
			return "UNKNOWN";
		}
	}

	private int get_big_int_comp_cc(BigInteger big_int1, BigInteger big_int2) {
		/*
		 * return psw cc for big int comp
		 */
		switch (big_int1.compareTo(big_int2)) {
		case -1:
			return psw_cc_low;
		case 0:
			return psw_cc_equal;
		case 1:
			return psw_cc_high;
		default:
			set_psw_check(psw_pic_interr);
			return psw_cc_ovf;
		}
	}

	private int get_long_add_cc() {
		/*
		 * check rlv3 = rlv1 + rlv2 for fixed overflow and set psw_cc or gen
		 * fixed exception
		 */
		if (rlv1 >= 0) {
			if (rlv2 > 0) {
				if (rlv3 < 0) {
					set_psw_check(psw_pic_fx_ovf);
				}
			}
		} else if (rlv2 < 0) {
			if (rlv3 >= 0) {
				set_psw_check(psw_pic_fx_ovf);
			}
		}
		if (rlv3 == 0) {
			return psw_cc_equal;
		} else {
			if (rlv3 > 0) {
				return psw_cc_high;
			} else {
				return psw_cc_low;
			}
		}
	}

	private int get_long_sub_cc() {
		/*
		 * check rlv3 = rlv1 - rlv2 for fixed overflow and set psw_cc or gen
		 * fixed exception
		 */
		if (rlv1 >= 0) {
			if (rlv2 < 0) {
				if (rlv3 < 0) {
					set_psw_check(psw_pic_fx_ovf);
				}
			}
		} else if (rlv2 > 0) {
			if (rlv3 >= 0) {
				set_psw_check(psw_pic_fx_ovf);
			}
		}
		if (rlv3 == 0) {
			return psw_cc_equal;
		} else {
			if (rlv3 > 0) {
				return psw_cc_high;
			} else {
				return psw_cc_low;
			}
		}
	}

	private int get_long_comp_cc(long long1, long long2) {
		/*
		 * return psw_cc for long compare
		 */
		if (long1 == long2) {
			return psw_cc_equal;
		} else {
			if (long1 > long2) {
				return psw_cc_high;
			} else {
				return psw_cc_low;
			}
		}
	}

	private int get_int_add_cc() {
		/*
		 * check rv3 = rv1 + rv2 for fixed overflow and set psw_cc or gen fixed
		 * exception
		 */
		if (rv1 >= 0) {
			if (rv2 > 0) {
				if (rv3 < 0) {
					set_psw_check(psw_pic_fx_ovf);
				}
			}
		} else if (rv2 < 0) {
			if (rv3 >= 0) {
				set_psw_check(psw_pic_fx_ovf);
			}
		}
		if (rv3 == 0) {
			return psw_cc_equal;
		} else {
			if (rv3 > 0) {
				return psw_cc_high;
			} else {
				return psw_cc_low;
			}
		}
	}

	private int get_int_sub_cc() {
		/*
		 * check rv3 = rv1 - rv2 for fixed overflow and set psw_cc or gen fixed
		 * exception
		 */
		if (rv1 >= 0) {
			if (rv2 < 0) {
				if (rv3 < 0) {
					set_psw_check(psw_pic_fx_ovf);
				}
			}
		} else if (rv2 > 0) {
			if (rv3 >= 0) {
				set_psw_check(psw_pic_fx_ovf);
			}
		}
		if (rv3 == 0) {
			return psw_cc_equal;
		} else {
			if (rv3 > 0) {
				return psw_cc_high;
			} else {
				return psw_cc_low;
			}
		}
	}

	private int get_int_comp_cc(int int1, int int2) {
		/*
		 * return psw_cc for int compare
		 */
		if (int1 == int2) {
			return psw_cc_equal;
		} else {
			if (int1 > int2) {
				return psw_cc_high;
			} else {
				return psw_cc_low;
			}
		}
	}

	private void check_eb_mpy() {
		/*
		 * check for EB overflow or underflow
		 */
		if (Math.abs(fp_rev1) >= fp_eb_max) {
			fp_dxc = fp_dxc_oe;
			set_psw_check(psw_pic_data);
		} else if (Math.abs(fp_rev1) <= fp_eb_min) {
			fp_dxc = fp_dxc_ue;
			set_psw_check(psw_pic_data);
		}
	}

	private void check_eh_mpy() {
		/*
		 * check for EH overflow or underflow
		 */
		if (Math.abs(fp_rdv1) >= fp_eh_max) {
			set_psw_check(psw_pic_fp_ovf);
		} else if (Math.abs(fp_rdv1) <= fp_eh_min) {
			set_psw_check(psw_pic_fp_unf);
		}
	}

	private void check_db_mpy() {
		/*
		 * check for DB overflow or underflow
		 */
		if (Math.abs(fp_rdv1) >= fp_db_max) {
			fp_dxc = fp_dxc_oe;
			set_psw_check(psw_pic_data);
		} else if (Math.abs(fp_rdv1) <= fp_db_min) {
			fp_dxc = fp_dxc_ue;
			set_psw_check(psw_pic_data);
		}
	}

	private void check_dh_mpy() {
		/*
		 * check for DH overflow or underflow
		 */
		if (Math.abs(fp_rdv1) >= fp_dh_max) {
			set_psw_check(psw_pic_fp_ovf);
		} else if (Math.abs(fp_rdv1) <= fp_dh_min) {
			set_psw_check(psw_pic_fp_unf);
		}
	}

	private void check_lb_mpy() {
		/*
		 * check for LB overflow or underflow
		 */
		if (fp_rbdv1.abs().compareTo(fp_lh_max) >= 0) {
			fp_dxc = fp_dxc_oe;
			set_psw_check(psw_pic_data);
		} else if (fp_rbdv1.abs().compareTo(fp_lh_min) <= 0) {
			fp_dxc = fp_dxc_ue;
			set_psw_check(psw_pic_data);
		}
	}

	private void check_lh_mpy() {
		/*
		 * check for LH overflow or underflow
		 */
		if (fp_rbdv1.abs().compareTo(fp_lh_max) >= 0) {
			set_psw_check(psw_pic_fp_ovf);
		} else if (fp_rbdv1.abs().compareTo(fp_lh_min) <= 0) {
			fp_dxc = fp_dxc_ue;
			set_psw_check(psw_pic_fp_unf);
		}
	}

	private void check_eb_div() {
		/*
		 * check for EB overflow or underflow
		 */
		if (fp_rev2 == 0) {
			fp_dxc = fp_dxc_div;
			set_psw_check(psw_pic_data);
		} else {
			if (Math.abs(fp_rev1) >= fp_eb_max) {
				fp_dxc = fp_dxc_oe;
				set_psw_check(psw_pic_data);
			} else if (Math.abs(fp_rev1) <= fp_eb_min) {
				fp_dxc = fp_dxc_ue;
				set_psw_check(psw_pic_data);
			}
		}
	}

	private void check_eh_div() {
		/*
		 * check for EH overflow or underflow
		 */
		if (fp_rdv2 == 0) {
			set_psw_check(psw_pic_fp_div);
		} else {
			if (Math.abs(fp_rdv1) >= fp_eh_max) {
				set_psw_check(psw_pic_fp_ovf);
			} else if (Math.abs(fp_rdv1) <= fp_eh_min) {
				set_psw_check(psw_pic_fp_unf);
			}
		}
	}

	private void check_db_div() {
		/*
		 * check for DB overflow or underflow
		 */
		if (fp_rdv2 == 0) {
			fp_dxc = fp_dxc_div;
			set_psw_check(psw_pic_data);
		} else {
			if (Math.abs(fp_rdv1) >= fp_db_max) {
				fp_dxc = fp_dxc_oe;
				set_psw_check(psw_pic_data);
			} else if (Math.abs(fp_rdv1) <= fp_db_min) {
				fp_dxc = fp_dxc_ue;
				set_psw_check(psw_pic_data);
			}
		}
	}

	private void check_dh_div() {
		/*
		 * check for DH overflow or underflow
		 */
		if (fp_rdv2 == 0) {
			set_psw_check(psw_pic_fp_div);
		} else {
			if (Math.abs(fp_rdv1) >= fp_dh_max) {
				set_psw_check(psw_pic_fp_ovf);
			} else if (Math.abs(fp_rdv1) <= fp_dh_min) {
				set_psw_check(psw_pic_fp_unf);
			}
		}
	}

	private void check_lb_div() {
		/*
		 * check for LB overflow or underflow
		 */
		if (fp_rbdv2.compareTo(BigDecimal.ZERO) == 0) {
			fp_dxc = fp_dxc_div;
			set_psw_check(psw_pic_data);
		} else {
			if (fp_rbdv1.abs().compareTo(fp_lh_max) >= 0) {
				fp_dxc = fp_dxc_oe;
				set_psw_check(psw_pic_data);
			} else if (fp_rbdv1.abs().compareTo(fp_lh_min) <= 0) {
				fp_dxc = fp_dxc_ue;
				set_psw_check(psw_pic_data);
			}
		}
	}

	private void check_lh_div() {
		/*
		 * check for LH overflow or underflow
		 */
		if (fp_rbdv2.compareTo(BigDecimal.ZERO) == 0) {
			set_psw_check(psw_pic_fp_div);
		} else {
			if (fp_rbdv1.abs().compareTo(fp_lh_max) >= 0) {
				set_psw_check(psw_pic_fp_ovf);
			} else if (fp_rbdv1.abs().compareTo(fp_lh_min) <= 0) {
				fp_dxc = fp_dxc_ue;
				set_psw_check(psw_pic_fp_unf);
			}
		}
	}

	private int fp_get_di_cc() {
		/*
		 * return psw_cc for fp divide to integer
		 */
		fp_rdv3 = Math.abs(fp_rdv3);
		fp_rdv4 = Math.abs(fp_rdv4);
		if (fp_rdv3 >= fp_db_max) {
			fp_dxc = fp_dxc_oe;
			set_psw_check(psw_pic_data);
			return psw_cc1;
		} else if (fp_rdv3 != 0 && fp_rdv3 <= fp_db_min) {
			fp_dxc = fp_dxc_ue;
			set_psw_check(psw_pic_data);
			return psw_cc1;
		} else if (fp_rdv4 >= fp_db_max) {
			fp_dxc = fp_dxc_oe;
			set_psw_check(psw_pic_data);
			return psw_cc2;
		} else if (fp_rdv4 != 0 && fp_rdv4 <= fp_db_min) {
			fp_dxc = fp_dxc_ue;
			set_psw_check(psw_pic_data);
			return psw_cc2;
		} else {
			return psw_cc0;
		}
	}

	private int fp_get_eb_comp_cc(float eb1, float eb2) {
		/*
		 * return psw_cc for float compare
		 */
		if (fp_signal) {
			fp_signal = false;
			if (Math.abs(eb1) >= fp_db_max || Math.abs(eb2) >= fp_eb_max) {
				fp_dxc = fp_dxc_oe;
				set_psw_check(psw_pic_data);
			} else if ((Math.abs(eb1) <= fp_eb_min && eb1 != 0.) // RPI 325
					|| (Math.abs(eb2) <= fp_eb_min && eb2 != 0.)) {
				fp_dxc = fp_dxc_ue;
				set_psw_check(psw_pic_data);
			}
		}
		if (eb1 == eb2) {
			return psw_cc_equal;
		} else {
			if (eb1 > eb2) {
				return psw_cc_high;
			} else {
				return psw_cc_low;
			}
		}
	}

	private int fp_get_eb_add_sub_cc() {
		/*
		 * return psw_cc for eb add/sub result fp_rev1 and raise BFP sig, ovf,
		 * or unf exceptions
		 */
		if (fp_rev1 == 0) {
			fp_dxc = fp_dxc_it;
			set_psw_check(psw_pic_data);
			return psw_cc_equal;
		} else {
			if (fp_rev1 > 0) {
				if (fp_rev1 >= fp_eb_max) {
					fp_dxc = fp_dxc_oe;
					set_psw_check(psw_pic_data);
				} else if (fp_rev1 <= fp_eb_min) {
					fp_dxc = fp_dxc_ue;
					set_psw_check(psw_pic_data);
				}
				return psw_cc_high;
			} else {
				if (fp_rev1 <= -fp_eb_max) {
					fp_dxc = fp_dxc_oe;
					set_psw_check(psw_pic_data);
				} else if (fp_rev1 >= -fp_eh_min) {
					fp_dxc = fp_dxc_ue;
					set_psw_check(psw_pic_data);
				}
				return psw_cc_low;
			}
		}
	}

	private int fp_get_db_comp_cc(double db1, double db2) {
		/*
		 * return psw_cc for DB double compare
		 */
		if (fp_signal) {
			fp_signal = false;
			if (Math.abs(db1) >= fp_db_max || Math.abs(db2) >= fp_db_max) {
				fp_dxc = fp_dxc_oe;
				set_psw_check(psw_pic_data);
			} else if ((Math.abs(db1) <= fp_db_min && db1 != 0.) // RPI 325
					|| (Math.abs(db2) <= fp_db_min && db2 != 0.)) {
				fp_dxc = fp_dxc_ue;
				set_psw_check(psw_pic_data);
			}
		}
		if (db1 == db2) {
			return psw_cc_equal;
		} else {
			if (db1 > db2) {
				return psw_cc_high;
			} else {
				return psw_cc_low;
			}
		}
	}
	private int fp_get_eh_comp_cc(double db1, double db2) {
		/*
		 * return psw_cc for EH double compare
		 */
		if (db1 == db2) {
			return psw_cc_equal;
		} else {
			if (db1 > db2) {
				return psw_cc_high;
			} else {
				return psw_cc_low;
			}
		}
	}

	private int fp_get_dh_comp_cc(double db1, double db2) {
		/*
		 * return psw_cc for DH double compare
		 */
		if (db1 == db2) {
			return psw_cc_equal;
		} else {
			if (db1 > db2) {
				return psw_cc_high;
			} else {
				return psw_cc_low;
			}
		}
	}

	private int fp_get_eh_add_sub_cc() {
		/*
		 * return psw_cc for eh add/sub result fp_rdv1 and raise HFP sig, ovf,
		 * or unf exceptions
		 */
		if (fp_rdv1 == 0) {
			set_psw_check(psw_pic_fp_sig);
			return psw_cc_equal;
		} else {
			if (fp_rdv1 > 0) {
				if (fp_rdv1 >= fp_eh_max) {
					set_psw_check(psw_pic_fp_ovf);
				} else if (fp_rdv1 <= fp_eh_min) {
					set_psw_check(psw_pic_fp_unf);
				}
				return psw_cc_high;
			} else {
				if (fp_rdv1 <= -fp_eh_max) {
					set_psw_check(psw_pic_fp_ovf);
				} else if (fp_rdv1 >= -fp_eh_min) {
					set_psw_check(psw_pic_fp_unf);
				}
				return psw_cc_low;
			}
		}
	}

	private int fp_get_dh_add_sub_cc() {
		/*
		 * return psw_cc for dh add/sub result fp_rdv1 and raise HFP sig, ovf,
		 * or unf exceptions
		 */
		if (fp_rdv1 == 0) {
			set_psw_check(psw_pic_fp_sig);
			return psw_cc_equal;
		} else {
			if (fp_rdv1 > 0) {
				if (fp_rdv1 >= fp_dh_max) {
					set_psw_check(psw_pic_fp_ovf);
				} else if (fp_rdv1 <= fp_dh_min) {
					set_psw_check(psw_pic_fp_unf);
				}
				return psw_cc_high;
			} else {
				if (fp_rdv1 <= -fp_dh_max) {
					set_psw_check(psw_pic_fp_ovf);
				} else if (fp_rdv1 >= -fp_dh_min) {
					set_psw_check(psw_pic_fp_unf);
				}
				return psw_cc_low;
			}
		}
	}

	private int fp_get_db_add_sub_cc() {
		/*
		 * return psw_cc for db add/sub result fp_rdv1 and raise BFP sig, ovf,
		 * or unf exceptions
		 */
		if (fp_rdv1 == 0) {
			fp_dxc = fp_dxc_it;
			set_psw_check(psw_pic_data);
			return psw_cc_equal;
		} else {
			if (fp_rdv1 > 0) {
				if (fp_rdv1 >= fp_db_max) {
					fp_dxc = fp_dxc_oe;
					set_psw_check(psw_pic_data);
				} else if (fp_rdv1 <= fp_db_min) {
					fp_dxc = fp_dxc_ue;
					set_psw_check(psw_pic_data);
				}
				return psw_cc_high;
			} else {
				if (fp_rdv1 <= -fp_db_max) {
					fp_dxc = fp_dxc_oe;
					set_psw_check(psw_pic_data);
				} else if (fp_rdv1 >= -fp_db_min) {
					fp_dxc = fp_dxc_ue;
					set_psw_check(psw_pic_data);
				}
				return psw_cc_low;
			}
		}
	}

	private int fp_get_lb_comp_cc(BigDecimal bd1, BigDecimal bd2) {
		/*
		 * return psw_cc for big decimal compare
		 */
		if (fp_signal) {
			fp_signal = false;
			if (bd1.abs().compareTo(fp_lb_max) >= 0
					|| bd2.abs().compareTo(fp_lb_max) >= 0) {
				fp_dxc = fp_dxc_oe;
				set_psw_check(psw_pic_data);
			} else if ((bd1.abs().compareTo(fp_lb_min) <= 0 // RPI 325
					&& !(bd1.abs().compareTo(BigDecimal.ZERO) == 0))
					|| (bd2.abs().compareTo(fp_lb_min) <= 0 && !(bd2.abs()
							.compareTo(BigDecimal.ZERO) == 0))) {
				fp_dxc = fp_dxc_ue;
				set_psw_check(psw_pic_data);
			}
		}
		int_work = bd1.compareTo(bd2);
		if (int_work == 0) {
			return psw_cc_equal;
		} else {
			if (int_work > 0) {
				return psw_cc_high;
			} else {
				return psw_cc_low;
			}
		}
	}

	private int fp_get_lh_comp_cc(BigDecimal bd1, BigDecimal bd2) {
		/*
		 * return psw_cc for LH big decimal compare
		 */
		int_work = bd1.compareTo(bd2);
		if (int_work == 0) {
			return psw_cc_equal;
		} else {
			if (int_work > 0) {
				return psw_cc_high;
			} else {
				return psw_cc_low;
			}
		}
	}

	private int fp_get_lh_add_sub_cc() {
		/*
		 * return psw_cc for lh add/sub result fp_rbdv1 * and raise HFP sig,
		 * ovf, or unf exceptions
		 */
		if (fp_rbdv1.compareTo(BigDecimal.ZERO) == 0) {
			set_psw_check(psw_pic_fp_sig);
			return psw_cc_equal;
		} else {
			if (fp_rbdv1.compareTo(BigDecimal.ZERO) > 0) {
				if (fp_rbdv1.compareTo(fp_lh_max) >= 0) {
					set_psw_check(psw_pic_fp_ovf);
				} else if (fp_rbdv1.compareTo(fp_lh_min) <= 0) {
					set_psw_check(psw_pic_fp_unf);
				}
				return psw_cc_high;
			} else {
				if (fp_rbdv1.compareTo(fp_lh_max.negate()) <= 0) {
					set_psw_check(psw_pic_fp_ovf);
				} else if (fp_rbdv1.compareTo(fp_lh_min.negate()) >= 0) {
					set_psw_check(psw_pic_fp_unf);
				}
				return psw_cc_low;
			}
		}
	}

	private int fp_get_lb_add_sub_cc() {
		/*
		 * return psw_cc for lb add/sub result fp_rbdv1 and raise BFP sig, ovf,
		 * or unf exceptions
		 */
		int int_work = fp_rbdv1.compareTo(BigDecimal.ZERO);
		if (int_work == 0) {
			fp_dxc = fp_dxc_it;
			set_psw_check(psw_pic_data);
			return psw_cc_equal;
		} else {
			if (int_work > 0) {
				if (fp_rbdv1.compareTo(fp_lb_max) >= 0) {
					fp_dxc = fp_dxc_oe;
					set_psw_check(psw_pic_data);
				} else if (fp_rbdv1.compareTo(fp_lb_min) <= 0) {
					fp_dxc = fp_dxc_ue;
					set_psw_check(psw_pic_data);
				}
				return psw_cc_high;
			} else {
				if (fp_rbdv1.compareTo(fp_lb_max.negate()) <= 0) {
					fp_dxc = fp_dxc_oe;
					set_psw_check(psw_pic_data);
				} else if (fp_rbdv1.compareTo(fp_lb_min.negate()) >= 0) {
					fp_dxc = fp_dxc_ue;
					set_psw_check(psw_pic_data);
				}
				return psw_cc_low;
			}
		}
	}
	private int fp_get_dfp_add_sub_cc() {
		/*
		 * return psw_cc for bd add/sub result fp_rdv1 and raise BFP sig, ovf,
		 * or unf exceptions
		 */
		if (fp_rbdv1.signum() == 0) {
			return psw_cc_equal;
		} else {
			if (fp_rbdv1.signum() > 0) {
				if (fp_rbdv1.compareTo(fp_dd_pos_max) > 0){
					fp_dxc = fp_dxc_oe;
					set_psw_check(psw_pic_data);
				} else if (fp_rbdv1.compareTo(fp_dd_pos_min) < 0) {
					fp_dxc = fp_dxc_ue;
					set_psw_check(psw_pic_data);
				}
				return psw_cc_high;
			} else {
				if (fp_rbdv1.compareTo(fp_dd_neg_max) < 0) {
					fp_dxc = fp_dxc_oe;
					set_psw_check(psw_pic_data);
				} else if (fp_rbdv1.compareTo(fp_dd_neg_min) > 0){
					fp_dxc = fp_dxc_ue;
					set_psw_check(psw_pic_data);
				}
				return psw_cc_low;
			}
		}
	}
	private int get_int_log_comp_cc(int int1, int int2) {
		/*
		 * return psw_cc for integer logical compare
		 */
		if (int1 == int2) {
			return psw_cc_equal;
		} else {
			if (int1 > int2) {
				if ((int1 & 0x80000000) == (int2 & 0x80000000)) {
					return psw_cc_high;
				} else {
					return psw_cc_low;
				}
			} else {
				if ((int1 & 0x80000000) == (int2 & 0x80000000)) {
					return psw_cc_low;
				} else {
					return psw_cc_high;
				}
			}
		}
	}

	private int get_long_log_comp_cc(long long1, long long2) {
		/*
		 * return psw_cc for long logical compare
		 */
		if (long1 == long2) {
			return psw_cc_equal;
		} else {
			if (long1 > long2) {
				if ((long1 & long_high_bit) == (long2 & long_high_bit)) {
					return psw_cc_high;
				} else {
					return psw_cc_low;
				}
			} else {
				if ((long1 & long_high_bit) == (long2 & long_high_bit)) {
					return psw_cc_low;
				} else {
					return psw_cc_high;
				}
			}
		}
	}

	public float fp_get_eb_from_eb(ByteBuffer fp_buff, int fp_index) {
		/*
		 * get float for EB from fp_reg or mem 1. If fp_reg then check for
		 * co-reg to avoid conversion Notes: 1. float is used to support EB
		 * only. 2. EH is supported using double since exponent (4*0x7f) exceeds
		 * EB (0xff).
		 */
		int fp_ctl_index = fp_index >> 3;
		if (fp_buff == fp_reg && fp_reg_ctl[fp_ctl_index] != fp_ctl_ld) {
			return fp_ctl_reg_to_eb(fp_ctl_index);
		} else {
			return fp_buff.getFloat(fp_index);
		}
	}

	public double fp_get_db_from_eh(ByteBuffer fp_buff, int fp_index) {
		/*
		 * get double from EH short hex in fp_reg or mem 1. If fp_reg, then
		 * check for float co-reg to avoid conversion
		 */
		int fp_ctl_index = fp_index >> 3;
		if (fp_buff == fp_reg && fp_reg_ctl[fp_ctl_index] != fp_ctl_ld) {
			return fp_ctl_reg_to_db(fp_ctl_index);
		} else {
			return zcvt_eh_to_db(fp_buff.getInt(fp_index));
		}
	}

	public double fp_get_db_from_dh(ByteBuffer fp_buff, int fp_index) {
		/*
		 * get double from DH long hex in fp_reg or mem 1. If fp_reg, then check
		 * for float co-reg to avoid conversion
		 */
		int fp_ctl_index = fp_index >> 3;
		if (fp_buff == fp_reg && fp_reg_ctl[fp_ctl_index] != fp_ctl_ld) {
			return fp_ctl_reg_to_db(fp_ctl_index);
		} else {
			return zcvt_dh_to_db(fp_buff.getLong(fp_index));
		}
	}

	private double fp_get_db_from_eb(ByteBuffer fp_buff, int fp_index) {
		/*
		 * get double from EB short binary in fp_reg or mem 1. If fp_reg, then
		 * check for float co-reg to avoid conversion
		 */
		int fp_ctl_index = fp_index >> 3;
		if (fp_buff == fp_reg && fp_reg_ctl[fp_ctl_index] != fp_ctl_ld) {
			return fp_ctl_reg_to_db(fp_ctl_index);
		} else {
			return fp_buff.getFloat(fp_index);
		}
	}

	public double fp_get_db_from_db(ByteBuffer fp_buff, int fp_index) {
		/*
		 * get double from DB Long binary in fp_reg or mem 1. If fp_reg, then
		 * check for float co-reg to avoid conversion
		 */
		int fp_ctl_index = fp_index >> 3;
		if (fp_buff == fp_reg && fp_reg_ctl[fp_ctl_index] != fp_ctl_ld) {
			return fp_ctl_reg_to_db(fp_ctl_index);
		} else {
			return fp_buff.getDouble(fp_index);
		}
	}

	private double fp_get_db_from_lh(ByteBuffer fp_buff, int fp_index) {
		/*
		 * get double from LH ext hex in fp_reg or mem 1. If fp_reg, then check
		 * for bd co-reg to avoid external conversion
		 */
		int fp_ctl_index = fp_index >> 3;
		if (fp_buff == fp_reg && fp_reg_ctl[fp_ctl_index] != fp_ctl_ld) {
			return fp_ctl_reg_to_db(fp_ctl_index);
		} else {
			return zcvt_lh_to_bd(fp_buff, fp_index).doubleValue();
		}
	}

	private void fp_get_bd_sqrt() {
		/*
		 * set fp_rbdv1 to square root of fp_rbdv2
		 * 
		 * Notes: 1. Return 0 for 0 and issue data exception if negative. 2.
		 * Scale number to within double range to calc estimate to 14 decimal
		 * places. 2. Use Newton Rapson iteration to reduce error to
		 * fp_bd_context limit.
		 */
		/*
		 * First handle 0 and negative values
		 */
		if (fp_rbdv2.signum() <= 0) {
			if (fp_rbdv2.signum() < 0) {
				set_psw_check(psw_pic_data);
			}
			fp_rbdv1 = BigDecimal.ZERO;
			return;
		}
		/*
		 * First fp_rbdv2 = x * 10 ** N where N is even so x is within double
		 * range and sqrt = sqrt(x) * 10 ** (N/2)
		 */
		int fp_bd_sqrt_scale = fp_rbdv2.scale();
		if ((fp_bd_sqrt_scale & 1) != 0) {
			fp_rbdv2 = fp_rbdv2.setScale(fp_bd_sqrt_scale - 1);
			fp_bd_sqrt_scale--;
		}
		fp_rbdv2 = fp_rbdv2.multiply(BigDecimal.TEN.pow(fp_bd_sqrt_scale,
				fp_bd_context), fp_bd_context);
		/*
		 * Now calc initial quess at sqrt of fp_rbdv2
		 */
		fp_rbdv1 = BigDecimal.valueOf(Math.sqrt(fp_rbdv2.doubleValue()));
		/*
		 * Now iterate using Newton Rapson until error is less than fp_bd_min
		 */
		fp_rbdv3 = fp_rbdv1; // save prev
		fp_rbdv1 = fp_rbdv2.divide(fp_rbdv1, fp_bd_context).add(fp_rbdv1,
				fp_bd_context).multiply(fp_bd_half, fp_bd_context);
		while (fp_rbdv1.compareTo(fp_rbdv3) != 0) {
			fp_rbdv3 = fp_rbdv1; // save prev
			fp_rbdv1 = fp_rbdv2.divide(fp_rbdv1, fp_bd_context).add(fp_rbdv1,
					fp_bd_context).multiply(fp_bd_half, fp_bd_context);
		}
		fp_rbdv1 = fp_rbdv1.scaleByPowerOfTen(-fp_bd_sqrt_scale / 2).round(
				fp_x_context);
	}

	public BigDecimal fp_get_bd_from_lh(ByteBuffer fp_buff, int fp_index) {
		/*
		 * get big decimal from LH extended hex in fp_reg or mem 1. If fp_reg,
		 * then check for big dec co-reg to avoid conversion
		 */
		int fp_ctl_index = fp_index >> 3;
		if (fp_buff == fp_reg && fp_reg_ctl[fp_ctl_index] != fp_ctl_ld) {
			return fp_ctl_reg_to_bd(fp_ctl_index);
		} else {
			return zcvt_lh_to_bd(fp_buff, fp_index);
		}
	}

	public BigDecimal fp_get_bd_from_lb(ByteBuffer fp_buff, int fp_index) {
		/*
		 * get big decimal from LH extended binary in fp_reg or mem 1. If
		 * fp_reg, then check for big dec co-reg to avoid conversion
		 */
		int fp_ctl_index = fp_index >> 3;
		if (fp_buff == fp_reg && fp_reg_ctl[fp_ctl_index] != fp_ctl_ld) {
			return fp_ctl_reg_to_bd(fp_ctl_index);
		} else {
			return zcvt_lb_to_bd(fp_buff, fp_index);
		}
	}

	public BigDecimal fp_get_bd_from_ed(ByteBuffer fp_buff, int fp_index) {
		/*
		 * get big decimal from ED dpd short in fp_reg or mem 1. If fp_reg, then
		 * check for big dec co-reg to avoid conversion
		 */
		int fp_ctl_index = fp_index >> 3;
		if (fp_buff == fp_reg && fp_reg_ctl[fp_ctl_index] != fp_ctl_ld) {
			return fp_ctl_reg_to_bd(fp_ctl_index);
		} else {
			return zcvt_ed_to_bd(fp_buff, fp_index);
		}
	}

	public BigDecimal fp_get_bd_from_dd(ByteBuffer fp_buff, int fp_index) {
		/*
		 * get big decimal from ED dpd long in fp_reg or mem 1. If fp_reg, then
		 * check for big dec co-reg to avoid conversion
		 */
		int fp_ctl_index = fp_index >> 3;
		if (fp_buff == fp_reg && fp_reg_ctl[fp_ctl_index] != fp_ctl_ld) {
			return fp_ctl_reg_to_bd(fp_ctl_index);
		} else {
			return zcvt_dd_to_bd(fp_buff, fp_index);
		}
	}

	public BigDecimal fp_get_bd_from_ld(ByteBuffer fp_buff, int fp_index) {
		/*
		 * get big decimal from LD dpd extended in fp_reg or mem 1. If fp_reg,
		 * then check for big dec co-reg to avoid conversion
		 */
		int fp_ctl_index = fp_index >> 3;
		if (fp_buff == fp_reg && fp_reg_ctl[fp_ctl_index] != fp_ctl_ld) {
			return fp_ctl_reg_to_bd(fp_ctl_index);
		} else {
			return zcvt_ld_to_bd(fp_buff, fp_index);
		}
	}

	private Float fp_ctl_reg_to_eb(int fp_ctl_index) {
		/*
		 * return float from fp_reg
		 */
		switch (fp_reg_ctl[fp_ctl_index]) {
		case 1: // fp_ctl_eb
			return fp_reg_eb[fp_ctl_index];
		case 2: // fp_ctl_db
			return (float) fp_reg_db[fp_ctl_index];
		case 3: // fp_ctl_bd
			return fp_reg_bd[fp_ctl_index].floatValue();
		default:
			set_psw_check(psw_pic_spec);
			return (float) 0;
		}
	}

	private Double fp_ctl_reg_to_db(int fp_ctl_index) {
		/*
		 * return double from fp_reg
		 */
		switch (fp_reg_ctl[fp_ctl_index]) {
		case 1: // fp_ctl_eb
			return (double) fp_reg_eb[fp_ctl_index];
		case 2: // fp_ctl_db
			return fp_reg_db[fp_ctl_index];
		case 3: // fp_ctl_bd
			return fp_reg_bd[fp_ctl_index].doubleValue();
		default:
			set_psw_check(psw_pic_spec);
			return (double) 0;
		}
	}

	private BigDecimal fp_ctl_reg_to_bd(int fp_ctl_index) {
		/*
		 * return BigDecimal from fp_reg cache
		 */
		switch (fp_reg_ctl[fp_ctl_index]) {
		case 1: // fp_ctl_eb
			return BigDecimal.valueOf(fp_reg_eb[fp_ctl_index]);
		case 2: // fp_ctl_db
			return BigDecimal.valueOf(fp_reg_db[fp_ctl_index]);
		case 3: // fp_ctl_bd
			return fp_reg_bd[fp_ctl_index];
		default:
			set_psw_check(psw_pic_spec);
			return BigDecimal.ZERO;
		}
	}

	private BigDecimal fp_get_bd_from_dh(ByteBuffer fp_buff, int fp_index) {
		/*
		 * get big decimal from DH long hex in fp_reg or mem 1. If fp_reg, then
		 * check for big dec co-reg to avoid conversion
		 */
		int fp_ctl_index = fp_index >> 3;
		if (fp_buff == fp_reg && fp_reg_ctl[fp_ctl_index] != fp_ctl_ld) {
			return fp_ctl_reg_to_bd(fp_ctl_index);
		} else {
			return zcvt_dh_to_bd(fp_buff, fp_index);
		}
	}

	private BigDecimal fp_get_bd_from_db(ByteBuffer fp_buff, int fp_index) {
		/*
		 * get big decimal from DB long bin in fp_reg or mem 1. If fp_reg, then
		 * check for big dec co-reg to avoid conversion
		 */
		return new BigDecimal(fp_get_db_from_db(fp_buff, fp_index),
				fp_d_context);
	}

	private BigDecimal fp_get_bd_rnd(int mode) {
		/*
		 * get rounded bd from fp_bd_int_rnd
		 */
		if (fp_bd_int_rem[0].signum() >= 0) {
			fp_bd_inc = BigDecimal.ONE;
		} else {
			fp_bd_inc = BigDecimal.ONE.negate();
		}
		int rem_comp = fp_bd_int_rem[1].abs().compareTo(fp_bd_half);
		if (mf3 == 0) {
			mf3 = 4 + (fp_fpc_reg & fp_fpc_mask_rnd);
		}
		switch (mf3) {
		case 1: // bias round to nearest with half up
			if (rem_comp >= 0) {
				return fp_bd_int_rem[0].add(fp_bd_inc);
			} else {
				return fp_bd_int_rem[0];
			}
		case 4: // round to nearest with default half even
			if (rem_comp == 0) {
				if (fp_bd_int_rem[0].remainder(fp_bd_two).compareTo(
						BigDecimal.ZERO) == 0) {
					return fp_bd_int_rem[0];
				} else {
					return fp_bd_int_rem[0].add(fp_bd_inc);
				}
			} else {
				if (rem_comp > 0) {
					return fp_bd_int_rem[0].add(fp_bd_inc);
				} else {
					return fp_bd_int_rem[0];
				}
			}
		case 5: // round toward 0 with down
			return fp_bd_int_rem[0];
		case 6: // round toward + infinity
			if (fp_bd_int_rem[1].compareTo(BigDecimal.ZERO) > 0
					&& fp_bd_int_rem[0].signum() >= 0) {
				return fp_bd_int_rem[0].add(fp_bd_inc);
			} else {
				return fp_bd_int_rem[0];
			}
		case 7: // round toward -infinity
			if (fp_bd_int_rem[1].compareTo(BigDecimal.ZERO) != 0
					&& fp_bd_int_rem[0].signum() < 0) {
				return fp_bd_int_rem[0].add(fp_bd_inc);
			} else {
				return fp_bd_int_rem[0];
			}
		default:
			set_psw_check(psw_pic_spec);
			return fp_bd_int_rem[0];
		}
	}

	private BigDecimal fp_get_bd_from_eb(ByteBuffer fp_buff, int fp_index) {
		/*
		 * get big decimal from DB long bin in fp_reg or mem 1. If fp_reg, then
		 * check for big dec co-reg to avoid conversion
		 */
		return new BigDecimal(fp_get_eb_from_eb(fp_buff, fp_index),
				fp_e_context);
	}

	private BigDecimal fp_get_bd_from_eh(ByteBuffer fp_buff, int fp_index) {
		/*
		 * get big decimal from EH short hex in fp_reg or mem 1. If fp_reg, then
		 * check for big dec co-reg to avoid conversion
		 */
		int fp_ctl_index = fp_index >> 3;
		if (fp_buff == fp_reg && fp_reg_ctl[fp_ctl_index] != fp_ctl_ld) {
			return fp_ctl_reg_to_bd(fp_ctl_index);
		} else {
			return zcvt_eh_to_bd(fp_buff, fp_index);
		}
	}

	private void fp_put_eb(int fp_index, byte reg_type, float reg_value) {
		/*
		 * store float co-reg as EB and set type Note: 1. Only tz390.fp_eb_type
		 * uses float co-reg due to exponent range exceeded for tz390.fp_eh_type
		 */
		int fp_ctl_index = fp_index >> 3;
		fp_reset_reg(fp_ctl_index, fp_ctl_eb, reg_type);
		fp_reg_eb[fp_ctl_index] = reg_value;
	}

	private void fp_put_db(int fp_index, byte reg_type, double reg_value) {
		/*
		 * store db co-reg as EH, DH, or DB and set type (EH used double due to
		 * exponent range of float is too small.
		 */
		int fp_ctl_index = fp_index >> 3;
		fp_reset_reg(fp_ctl_index, fp_ctl_db, reg_type);
		fp_reg_db[fp_ctl_index] = reg_value;
	}

	private void fp_put_bd(int fp_index, byte reg_type, BigDecimal reg_value) {
		/*
		 * store bd co-reg as DD, DH, LD, LH or LB and set type for reg pair
		 */
		int fp_ctl_index = fp_index >> 3;
		fp_reset_reg(fp_ctl_index, fp_ctl_bd1, reg_type);
		fp_reg_bd[fp_ctl_index] = reg_value;
	}

	private void fp_load_reg(int reg_buff_index1, byte reg_type1,
			ByteBuffer reg_buff2, int reg_buff_index2, byte reg_type2) {
		/*
		 * Load fp_ctl from fp_reg or fp_ctl Load fp_reg from memory with no
		 * conversion
		 * 
		 * Notes: 1. bd1/bd2 co-reg pair being partially replaced is discarded
		 * without conversion. 2. This function is called recursively when two
		 * co-registers of different types are involved.
		 */
		int fp_ctl_index1 = reg_buff_index1 >>> 3;
		int fp_ctl_index2 = -1;
		int fp_reg_ctl2 = fp_ctl_ld;
		byte fp_reg_type2 = reg_type2;
		if (reg_buff2 == fp_reg) {
			fp_ctl_index2 = reg_buff_index2 >>> 3;
			if (fp_reg_ctl[fp_ctl_index2] != fp_ctl_ld) {
				// save ctl type before reset incase reg1 = reg2
				fp_reg_ctl2 = fp_reg_ctl[fp_ctl_index2];
				fp_reg_type2 = fp_reg_type[fp_ctl_index2];
			}
		}
		fp_reset_reg(fp_ctl_index1, fp_ctl_ld, reg_type1);
		if (reg_buff2 == fp_reg) {
			// load fp_ctl reg from fp_reg or other fp_ctl reg
			fp_load_ctl(fp_ctl_index1, reg_type1, fp_reg_ctl2, reg_buff_index2,
					fp_reg_type2);
		} else {
			// load fp_reg from memory without conv.
			fp_load_mem(reg_buff_index1, reg_type1, reg_buff_index2, reg_type2);
		}
	}

	private void fp_load_mem(int reg_buff_index1, byte reg_type1,
			int mem_index2, byte mem_type2) {
		/*
		 * load fp_reg memory without conversion
		 */
		fp_reg_ctl[reg_buff_index1 >>> 3] = fp_ctl_ld;
		switch (mem_type2) {
		case 0: // tz390.fp_db_type
		case 1: // tz390.fp_dd_type
		case 2: // tz390.fp_dh_type
			fp_reg.putLong(reg_buff_index1, mem.getLong(mem_index2));
			break;
		case 3: // tz390.fp_eb_type
		case 4: // tz390.fp_ed_type
		case 5: // tz390.fp_eh_type
			fp_reg
					.putLong(reg_buff_index1,
							(long) mem.getInt(mem_index2) << 32);
			break;
		case 6: // tz390.fp_lb_type
		case 7: // tz390.fp_ld_type
		case 8: // tz390.fp_lh_type
			fp_reg.putLong(reg_buff_index1, mem.getLong(mem_index2));
			if (reg_type1 == tz390.fp_lh_type) {
				fp_reg.putLong(reg_buff_index1 + 16, long_dh_zero);
			}
			break;
		default:
			set_psw_check(psw_pic_spec);
		}
	}

	private void fp_load_ctl(int fp_ctl_index1, byte reg_type1,
			int fp_ctl_type2, int fp_buff_index2, byte reg_type2) {
		/*
		 * load fp control register from register memory or other fp control
		 * register
		 */
		int fp_ctl_index2 = fp_buff_index2 >>> 3;
		switch (fp_ctl_type2) {
		case 0: // from fp_ctl_ld
			fp_load_ctl_from_ext_reg(fp_ctl_index1, reg_type1, fp_reg,
					fp_buff_index2, reg_type2);
			return;
		case 1: // fp_ctl_eb
			switch (reg_type1) {
			case 0: // tz390.fp_db_type
				fp_reg_db[fp_ctl_index1] = fp_reg_eb[fp_ctl_index2];
				fp_reg_ctl[fp_ctl_index1] = fp_ctl_db;
				return;
			case 2: // tz390.fp_dh_type
				fp_reg_db[fp_ctl_index1] = fp_reg_eb[fp_ctl_index2];
				fp_reg_ctl[fp_ctl_index1] = fp_ctl_db;
				return;
			case 3: // tz390.fp_eb_type
				fp_reg_eb[fp_ctl_index1] = fp_reg_eb[fp_ctl_index2];
				fp_reg_ctl[fp_ctl_index1] = fp_ctl_eb;
				return;
			case 5: // tz390.fp_eh_type
				fp_reg_db[fp_ctl_index1] = fp_reg_eb[fp_ctl_index2];
				fp_reg_ctl[fp_ctl_index1] = fp_ctl_db;
				return;
			case 6: // tz390.fp_lb_type
				fp_reg_bd[fp_ctl_index1] = BigDecimal
						.valueOf(fp_reg_eb[fp_ctl_index2]);
				fp_reg_ctl[fp_ctl_index1] = fp_ctl_bd1;
				return;
			case 8: // tz390.fp_lh_type
				fp_reg_bd[fp_ctl_index1] = BigDecimal
						.valueOf(fp_reg_eb[fp_ctl_index2]);
				fp_reg_ctl[fp_ctl_index1] = fp_ctl_bd1;
				return;
			}
			break;
		case 2: // fp_ctl_db
			switch (reg_type1) {
			case 0: // tz390.fp_db_type
				fp_reg_db[fp_ctl_index1] = fp_reg_db[fp_ctl_index2];
				fp_reg_ctl[fp_ctl_index1] = fp_ctl_db;
				return;
			case 2: // tz390.fp_dh_type
				fp_reg_db[fp_ctl_index1] = fp_reg_db[fp_ctl_index2];
				fp_reg_ctl[fp_ctl_index1] = fp_ctl_db;
				return;
			case 3: // tz390.fp_eb_type
				fp_reg_eb[fp_ctl_index1] = (float) fp_reg_db[fp_ctl_index2];
				fp_reg_ctl[fp_ctl_index1] = fp_ctl_eb;
				return;
			case 5: // tz390.fp_eh_type
				fp_reg_db[fp_ctl_index1] = fp_reg_db[fp_ctl_index2];
				fp_reg_ctl[fp_ctl_index1] = fp_ctl_db;
				return;
			case 6: // tz390.fp_lb_type
				fp_reg_bd[fp_ctl_index1] = BigDecimal
						.valueOf(fp_reg_db[fp_ctl_index2]);
				fp_reg_ctl[fp_ctl_index1] = fp_ctl_bd1;
				return;
			case 8: // tz390.fp_lh_type
				fp_reg_bd[fp_ctl_index1] = BigDecimal
						.valueOf(fp_reg_db[fp_ctl_index2]);
				fp_reg_ctl[fp_ctl_index1] = fp_ctl_bd1;
				return;
			}
			break;
		case 3: // fp_ctl_bd1
			switch (reg_type1) {
			case 0: // tz390.fp_db_type
				fp_reg_db[fp_ctl_index1] = fp_reg_bd[fp_ctl_index2]
						.doubleValue();
				fp_reg_ctl[fp_ctl_index1] = fp_ctl_db;
				return;
			case 2: // tz390.fp_dh_type
				fp_reg_db[fp_ctl_index1] = fp_reg_bd[fp_ctl_index2]
						.doubleValue();
				fp_reg_ctl[fp_ctl_index1] = fp_ctl_db;
				return;
			case 3: // tz390.fp_eb_type
				fp_reg_eb[fp_ctl_index1] = fp_reg_bd[fp_ctl_index2]
						.floatValue();
				fp_reg_ctl[fp_ctl_index1] = fp_ctl_eb;
				return;
			case 5: // tz390.fp_eh_type
				fp_reg_db[fp_ctl_index1] = fp_reg_bd[fp_ctl_index2]
						.floatValue();
				fp_reg_ctl[fp_ctl_index1] = fp_ctl_db;
				return;
			case 6: // tz390.fp_lb_type
				fp_reg_bd[fp_ctl_index1] = fp_reg_bd[fp_ctl_index2];
				fp_reg_ctl[fp_ctl_index1] = fp_ctl_bd1;
				return;
			case 8: // tz390.fp_lh_type
				fp_reg_bd[fp_ctl_index1] = fp_reg_bd[fp_ctl_index2];
				fp_reg_ctl[fp_ctl_index1] = fp_ctl_bd1;
				return;
			}
			break;
		case 4: // fp_ctl_bd2
			set_psw_check(psw_pic_spec); // invalid LH/LB reg ref.
			return;
		default: // invalid type
			set_psw_check(psw_pic_spec); // invalid LH/LB reg ref.
			return;
		}
	}

	private void fp_load_ctl_from_ext_reg(int fp_ctl_index1, byte reg_type1,
			ByteBuffer reg_buff2, int reg_buff_index2, byte reg_type2) {
		/*
		 * load fp_ctl reg1 from fp_reg2 external format
		 */
		switch (reg_type2) {
		case 0: // from tz390.fp_db_type
			switch (reg_type1) {
			case 0: // to tz390.fp_db_type
				fp_reg_db[fp_ctl_index1] = fp_get_db_from_db(reg_buff2,
						reg_buff_index2);
				fp_reg_ctl[fp_ctl_index1] = fp_ctl_db;
				return;
			case 2: // tz390.fp_dh_type
				fp_reg_db[fp_ctl_index1] = fp_get_db_from_dh(reg_buff2,
						reg_buff_index2);
				fp_reg_ctl[fp_ctl_index1] = fp_ctl_db;
				return;
			case 3: // tz390.fp_eb_type
				fp_reg_eb[fp_ctl_index1] = (float) fp_get_db_from_db(reg_buff2,
						reg_buff_index2);
				fp_reg_ctl[fp_ctl_index1] = fp_ctl_eb;
				return;
			case 5: // tz390.fp_eh_type
				fp_reg_db[fp_ctl_index1] = fp_get_db_from_db(reg_buff2,
						reg_buff_index2);
				fp_reg_ctl[fp_ctl_index1] = fp_ctl_db;
				return;
			case 6: // tz390.fp_lb_type
				fp_reg_bd[fp_ctl_index1] = fp_get_bd_from_db(reg_buff2,
						reg_buff_index2);
				fp_reg_ctl[fp_ctl_index1] = fp_ctl_bd1;
				return;
			case 8: // tz390.fp_lh_type
				fp_reg_bd[fp_ctl_index1] = fp_get_bd_from_db(reg_buff2,
						reg_buff_index2);
				fp_reg_ctl[fp_ctl_index1] = fp_ctl_bd1;
				return;
			}
			break;
		case 2: // tz390.fp_dh_type
			switch (reg_type1) {
			case 0: // tz390.fp_db_type
				fp_reg_db[fp_ctl_index1] = fp_get_db_from_dh(reg_buff2,
						reg_buff_index2);
				fp_reg_ctl[fp_ctl_index1] = fp_ctl_db;
				return;
			case 2: // tz390.fp_dh_type
				fp_reg_db[fp_ctl_index1] = fp_get_db_from_dh(reg_buff2,
						reg_buff_index2);
				fp_reg_ctl[fp_ctl_index1] = fp_ctl_db;
				return;
			case 3: // tz390.fp_eb_type
				fp_reg_eb[fp_ctl_index1] = (float) fp_get_db_from_dh(reg_buff2,
						reg_buff_index2);
				fp_reg_ctl[fp_ctl_index1] = fp_ctl_eb;
				return;
			case 5: // tz390.fp_eh_type
				fp_reg_db[fp_ctl_index1] = fp_get_db_from_dh(reg_buff2,
						reg_buff_index2);
				fp_reg_ctl[fp_ctl_index1] = fp_ctl_db;
				return;
			case 6: // tz390.fp_lb_type
				fp_reg_bd[fp_ctl_index1] = fp_get_bd_from_dh(reg_buff2,
						reg_buff_index2);
				fp_reg_ctl[fp_ctl_index1] = fp_ctl_bd1;
				return;
			case 8: // tz390.fp_lh_type
				fp_reg_bd[fp_ctl_index1] = fp_get_bd_from_dh(reg_buff2,
						reg_buff_index2);
				fp_reg_ctl[fp_ctl_index1] = fp_ctl_bd1;
				return;
			}
			break;
		case 3: // tz390.fp_eb_type
			switch (reg_type1) {
			case 0: // tz390.fp_db_type
				fp_reg_db[fp_ctl_index1] = fp_get_db_from_eb(reg_buff2,
						reg_buff_index2);
				fp_reg_ctl[fp_ctl_index1] = fp_ctl_db;
				return;
			case 2: //
				fp_reg_db[fp_ctl_index1] = fp_get_db_from_eb(reg_buff2,
						reg_buff_index2);
				fp_reg_ctl[fp_ctl_index1] = fp_ctl_db;
				return;
			case 3: // tz390.fp_eb_type
				fp_reg_eb[fp_ctl_index1] = fp_get_eb_from_eb(reg_buff2,
						reg_buff_index2);
				fp_reg_ctl[fp_ctl_index1] = fp_ctl_eb;
				return;
			case 5: // tz390.fp_eh_type
				fp_reg_db[fp_ctl_index1] = fp_get_db_from_eb(reg_buff2,
						reg_buff_index2);
				fp_reg_ctl[fp_ctl_index1] = fp_ctl_db;
				return;
			case 6: // tz390.fp_lb_type
				fp_reg_bd[fp_ctl_index1] = fp_get_bd_from_eb(reg_buff2,
						reg_buff_index2);
				fp_reg_ctl[fp_ctl_index1] = fp_ctl_bd1;
				return;
			case 8: // tz390.fp_lh_type
				fp_reg_bd[fp_ctl_index1] = fp_get_bd_from_eb(reg_buff2,
						reg_buff_index2);
				fp_reg_ctl[fp_ctl_index1] = fp_ctl_bd1;
				return;
			}
			break;
		case 5: // tz390.fp_eh_type
			switch (reg_type1) {
			case 0: // tz390.fp_db_type
				fp_reg_db[fp_ctl_index1] = fp_get_db_from_eh(reg_buff2,
						reg_buff_index2);
				fp_reg_ctl[fp_ctl_index1] = fp_ctl_db;
				return;
			case 2: // tz390.fp_dh_type
				fp_reg_db[fp_ctl_index1] = fp_get_db_from_eh(reg_buff2,
						reg_buff_index2);
				fp_reg_ctl[fp_ctl_index1] = fp_ctl_db;
				return;
			case 3: // tz390.fp_eb_type
				fp_reg_eb[fp_ctl_index1] = (float) fp_get_db_from_eh(reg_buff2,
						reg_buff_index2);
				fp_reg_ctl[fp_ctl_index1] = fp_ctl_eb;
				return;
			case 5: // tz390.fp_eh_type
				fp_reg_db[fp_ctl_index1] = fp_get_db_from_eh(reg_buff2,
						reg_buff_index2);
				fp_reg_ctl[fp_ctl_index1] = fp_ctl_db;
				return;
			case 6: // tz390.fp_lb_type
				fp_reg_bd[fp_ctl_index1] = fp_get_bd_from_eh(reg_buff2,
						reg_buff_index2);
				fp_reg_ctl[fp_ctl_index1] = fp_ctl_bd1;
				return;
			case 8: // tz390.fp_lh_type
				fp_reg_bd[fp_ctl_index1] = fp_get_bd_from_eh(reg_buff2,
						reg_buff_index2);
				fp_reg_ctl[fp_ctl_index1] = fp_ctl_bd1;
				return;
			}
			break;
		case 6: // tz390.fp_lb_type
			switch (reg_type1) {
			case 0: // tz390.fp_db_type
				fp_reg_db[fp_ctl_index1] = fp_get_bd_from_lb(reg_buff2,
						reg_buff_index2).doubleValue();
				fp_reg_ctl[fp_ctl_index1] = fp_ctl_db;
				return;
			case 2: // tz390.fp_dh_type
				fp_reg_db[fp_ctl_index1] = fp_get_bd_from_lb(reg_buff2,
						reg_buff_index2).doubleValue();
				fp_reg_ctl[fp_ctl_index1] = fp_ctl_db;
				return;
			case 3: // tz390.fp_eb_type
				fp_reg_eb[fp_ctl_index1] = fp_get_bd_from_lb(reg_buff2,
						reg_buff_index2).floatValue();
				fp_reg_ctl[fp_ctl_index1] = fp_ctl_eb;
				return;
			case 5: // tz390.fp_eh_type
				fp_reg_db[fp_ctl_index1] = fp_get_bd_from_lb(reg_buff2,
						reg_buff_index2).doubleValue();
				fp_reg_ctl[fp_ctl_index1] = fp_ctl_db;
				return;
			case 6: // tz390.fp_lb_type
				fp_reg_bd[fp_ctl_index1] = fp_get_bd_from_lb(reg_buff2,
						reg_buff_index2);
				fp_reg_ctl[fp_ctl_index1] = fp_ctl_bd1;
				return;
			case 8: // tz390.fp_lh_type
				fp_reg_bd[fp_ctl_index1] = fp_get_bd_from_lh(reg_buff2,
						reg_buff_index2);
				fp_reg_ctl[fp_ctl_index1] = fp_ctl_bd1;
				return;
			}
			break;
		case 8: // tz390.fp_lh_type
			switch (reg_type1) {
			case 0: // tz390.fp_db_type
				fp_reg_db[fp_ctl_index1] = fp_get_db_from_lh(reg_buff2,
						reg_buff_index2);
				fp_reg_ctl[fp_ctl_index1] = fp_ctl_db;
				return;
			case 2: // tz390.fp_dh_type
				fp_reg_db[fp_ctl_index1] = fp_get_db_from_lh(reg_buff2,
						reg_buff_index2);
				fp_reg_ctl[fp_ctl_index1] = fp_ctl_db;
				return;
			case 3: // tz390.fp_eb_type
				fp_reg_eb[fp_ctl_index1] = (float) fp_get_db_from_lh(reg_buff2,
						reg_buff_index2);
				fp_reg_ctl[fp_ctl_index1] = fp_ctl_eb;
				return;
			case 5: // tz390.fp_eh_type
				fp_reg_db[fp_ctl_index1] = fp_get_db_from_lh(reg_buff2,
						reg_buff_index2);
				fp_reg_ctl[fp_ctl_index1] = fp_ctl_db;
				return;
			case 6: // tz390.fp_lb_type
				fp_reg_bd[fp_ctl_index1] = fp_get_bd_from_lh(reg_buff2,
						reg_buff_index2);
				fp_reg_ctl[fp_ctl_index1] = fp_ctl_bd1;
				return;
			case 8: // tz390.fp_lh_type
				fp_reg_bd[fp_ctl_index1] = fp_get_bd_from_lh(reg_buff2,
						reg_buff_index2);
				fp_reg_ctl[fp_ctl_index1] = fp_ctl_bd1;
				return;
			}
			break;
		}
		set_psw_check(psw_pic_spec); // should not occur
	}

	private void fp_reset_reg(int reg_ctl_index, byte ctl_type, byte reg_type) {
		/*
		 * reset register and co-reg to specified type
		 */
		if (fp_reg_ctl[reg_ctl_index] == fp_ctl_bd1
			&& fp_pair_type[fp_reg_type[reg_ctl_index]]) { // RPI 229
			fp_reg_ctl[reg_ctl_index + 2] = fp_ctl_ld; // cancel replaced bd
		} else if (fp_reg_ctl[reg_ctl_index] == fp_ctl_bd2
            && fp_pair_type[fp_reg_type[reg_ctl_index]]) { // RPI 229
			fp_reg_ctl[reg_ctl_index - 2] = fp_ctl_ld; // cancel replaced bd
		}
		fp_reg_ctl[reg_ctl_index] = ctl_type;
		fp_reg_type[reg_ctl_index] = reg_type;
		if (ctl_type == fp_ctl_bd1
			&& fp_pair_type[reg_type]) {
			if (!fp_pair_valid[reg_ctl_index]) { // RPI 229
				set_psw_check(psw_pic_spec);
			}
			fp_reg_ctl[reg_ctl_index + 2] = fp_ctl_bd2; // RPI 229
			fp_reg_type[reg_ctl_index + 2] = reg_type; // RPI 229
		}
	}

	public void fp_store_reg(ByteBuffer reg_buff, int reg_index) {
		/*
		 * convert co-reg back to fp_reg storage format for use by STE or STD.
		 * If byte buffer is not fp_reg, copy fp_reg to byte buffer if needed
		 * for use in non destructive trace out of regs.
		 */
		int fp_ctl_index = reg_index >> 3;
		switch (fp_reg_ctl[fp_ctl_index]) {
		case 0: // fp_ctl_ld
			if (reg_buff != fp_reg) {
				reg_buff.putLong(reg_index, fp_reg.getLong(reg_index));
			}
			break;
		case 1: // fp_ctl_eb
			reg_buff.putFloat(reg_index, fp_reg_eb[fp_ctl_index]);
			break;
		case 2: // fp_ctl_db
			switch (fp_reg_type[fp_ctl_index]) {
			case 0: // tz390.fp_db_type
				reg_buff.putDouble(reg_index, fp_reg_db[fp_ctl_index]);
				break;
			case 2: // tz390.fp_dh_type
				reg_buff.putLong(reg_index,
						zcvt_db_to_dh(fp_reg_db[fp_ctl_index]));
				break;
			case 5: // tz390.fp_eh_type
				reg_buff.putInt(reg_index,
						zcvt_db_to_eh(fp_reg_db[fp_ctl_index]));
				break;
			}
			break;
		case 3: // fp_ctl_bd1
			switch (fp_reg_type[fp_ctl_index]) {
			case 1:
				zcvt_bd(tz390.fp_dd_type, fp_reg_bd[fp_ctl_index]);
				reg_buff.putLong(reg_index, tz390.fp_work_reg.getLong(0));
				break;
			case 4:
				zcvt_bd(tz390.fp_ed_type, fp_reg_bd[fp_ctl_index]);
				reg_buff.putInt(reg_index, tz390.fp_work_reg.getInt(0));
				break;	
			case 6: // tz390.fp_lb_type
				if (fp_reg_ctl[fp_ctl_index + 2] == fp_ctl_bd2) {
					zcvt_bd(tz390.fp_lb_type, fp_reg_bd[fp_ctl_index]);
					reg_buff.putLong(reg_index, tz390.fp_work_reg.getLong(0));
				}
				break;
			case 7: // tz390.fp_ld_type
				if (fp_reg_ctl[fp_ctl_index + 2] == fp_ctl_bd2) {
					zcvt_bd(tz390.fp_ld_type, fp_reg_bd[fp_ctl_index]);
					reg_buff.putLong(reg_index, tz390.fp_work_reg.getLong(0));
				}
				break;
			case 8: // tz390.fp_lh_type
				if (fp_reg_ctl[fp_ctl_index + 2] == fp_ctl_bd2) {
					zcvt_bd(tz390.fp_lh_type, fp_reg_bd[fp_ctl_index]);
					reg_buff.putLong(reg_index, tz390.fp_work_reg.getLong(0));
				}
				break;
			}
			break;
		case 4: // fp_ctl_bd2
			switch (fp_reg_type[fp_ctl_index - 2]) { // RPI 229
			case 6: // tz390.fp_lb_type
				if (fp_reg_ctl[fp_ctl_index - 2] == fp_ctl_bd1) {
					zcvt_bd(tz390.fp_lb_type, fp_reg_bd[fp_ctl_index - 2]); // RPI
																			// 229
					reg_buff.putLong(reg_index, tz390.fp_work_reg.getLong(8));
				}
				break;
			case 7: // tz390.fp_ld_type
				if (fp_reg_ctl[fp_ctl_index - 2] == fp_ctl_bd1) {
					zcvt_bd(tz390.fp_ld_type, fp_reg_bd[fp_ctl_index - 2]); // RPI
																			// 229
					reg_buff.putLong(reg_index, tz390.fp_work_reg.getLong(8));
				}
				break;
			case 8: // tz390.fp_lh_type
				if (fp_reg_ctl[fp_ctl_index - 2] == fp_ctl_bd1) {
					zcvt_bd(tz390.fp_lh_type, fp_reg_bd[fp_ctl_index - 2]); // RPI
																			// 229
					reg_buff.putLong(reg_index, tz390.fp_work_reg.getLong(8));
				}
				break;
			}
			break;
		}
	}

	private BigDecimal zcvt_ed_to_bd(ByteBuffer fp_buff, int fp_index) {
		/*
		 * convert ED format to big decimal
		 */
		int ed1 = fp_buff.getInt(fp_index);
		if (ed1 == 0) {
			return BigDecimal.ZERO;
		}
		int cf5 = (ed1 >> 26) & 0x1f;
		int bxcf6 = (ed1 >> 20) & 0x3f;
		int exp = ((tz390.dfp_cf5_to_exp2[cf5] << 6) + bxcf6)
				- tz390.fp_exp_bias[tz390.fp_ed_type];
		int digits = (int) (tz390.dfp_dpd_to_bcd[ed1 & 0x3ff] + 1000 * (tz390.dfp_dpd_to_bcd[(ed1 >>> 10) & 0x3ff] + 1000 * tz390.dfp_cf5_to_bcd[cf5]));
		if (ed1 < 0) {
			digits = -digits;
		}
		return BigDecimal.valueOf((long) digits).scaleByPowerOfTen(exp);
	}

	private BigDecimal zcvt_dd_to_bd(ByteBuffer fp_buff, int fp_index) {
		/*
		 * convert DD format to big decimal
		 */
		long dd1 = fp_buff.getLong(fp_index);
		if (dd1 == 0) {
			return BigDecimal.ZERO;
		}
		int cf5 = (int) (dd1 >>> 58) & 0x1f;
		int bxcf8 = (int) (dd1 >>> 50) & 0xff;
		int exp = ((tz390.dfp_cf5_to_exp2[cf5] << 8) | bxcf8)
				- tz390.fp_exp_bias[tz390.fp_dd_type]; // adjust exp
		long digits = tz390.dfp_dpd_to_bcd[(int) (dd1 & 0x3ff)]
				+ 1000
				* (tz390.dfp_dpd_to_bcd[(int) ((dd1 >>> 10) & 0x3ff)] + 1000 * (tz390.dfp_dpd_to_bcd[(int) ((dd1 >>> 20) & 0x3ff)] + 1000 * (tz390.dfp_dpd_to_bcd[(int) ((dd1 >>> 30) & 0x3ff)] + 1000 * (tz390.dfp_dpd_to_bcd[(int) ((dd1 >>> 40) & 0x3ff)] + 1000 * (tz390.dfp_cf5_to_bcd[cf5])))));
		if (dd1 < 0) {
			digits = -digits;
		}
		return BigDecimal.valueOf(digits).scaleByPowerOfTen(exp);
	}

	private BigDecimal zcvt_ld_to_bd(ByteBuffer fp_buff, int fp_index) {
		/*
		 * convert LD format to big decimal
		 */
		long ld1 = fp_buff.getLong(fp_index);
		long ld2 = 0;
		if (fp_buff == fp_reg) {
			ld2 = fp_reg.getLong(fp_index + 16);
		} else {
			ld2 = fp_buff.getLong(fp_index + 8);
		}
		if (ld1 == 0 && ld2 == 0) {
			return BigDecimal.ZERO;
		}
		int cf5 = (int) (ld1 >> 58) & 0x1f;
		int bxcf12 = (int) (ld1 >> 46) & 0xfff;
		int exp = ((tz390.dfp_cf5_to_exp2[cf5] << 12) + bxcf12)
				- tz390.fp_exp_bias[tz390.fp_ld_type];
		long digits1 = tz390.dfp_dpd_to_bcd[(int) (((ld1 & 0x3f) << 4) | (ld2 >>> 60))]
				+ 1000
				* (tz390.dfp_dpd_to_bcd[(int) ((ld1 >>> 6) & 0x3ff)] + 1000 * (tz390.dfp_dpd_to_bcd[(int) ((ld1 >>> 16) & 0x3ff)] + 1000 * (tz390.dfp_dpd_to_bcd[(int) ((ld1 >>> 26) & 0x3ff)] + 1000 * (tz390.dfp_dpd_to_bcd[(int) ((ld1 >>> 36) & 0x3ff)]))));
		long digits2 = tz390.dfp_dpd_to_bcd[(int) (ld2 & 0x3ff)]
				+ 1000
				* (tz390.dfp_dpd_to_bcd[(int) ((ld2 >>> 10) & 0x3ff)] + 1000 * (tz390.dfp_dpd_to_bcd[(int) ((ld2 >>> 20) & 0x3ff)] + 1000 * (tz390.dfp_dpd_to_bcd[(int) ((ld2 >>> 30) & 0x3ff)] + 1000 * (tz390.dfp_dpd_to_bcd[(int) ((ld2 >>> 40) & 0x3ff)] + 1000 * (tz390.dfp_dpd_to_bcd[(int) ((ld2 >>> 50) & 0x3ff)])))));
		if (ld1 < 0) {
			return BigDecimal.valueOf(digits1).scaleByPowerOfTen(18).add(
					BigDecimal.valueOf(digits2)).scaleByPowerOfTen(exp)
					.negate();
		} else {
			return BigDecimal.valueOf(digits1).scaleByPowerOfTen(18).add(
					BigDecimal.valueOf(digits2)).scaleByPowerOfTen(exp);
		}
	}

	private double zcvt_eh_to_db(int eh1) {
		/*
		 * convert EH format to double
		 */
		if (eh1 < 0) {
			long_sign = long_high_bit;
		} else {
			long_sign = 0;
		}
		long_exp = (long) ((((eh1 & int_eh_exp_bits) >>> 24) - 0x40) << 2) - 4;
		long_man = (long) (eh1 & int_eh_man_bits) << (52 - 24 + 4);
		if (long_man == 0) {
			return 0;
		}
		while (long_man > long_db_one_bits) {
			long_man = long_man >> 1;
			long_exp++;
		}
		work_fp_reg.putLong(0, long_sign | ((long_exp + 0x3ff) << 52)
				| (long_man & long_db_man_bits));
		return work_fp_reg.getDouble(0);
	}

	private double zcvt_dh_to_db(long dh1) {
		/*
		 * convert DH long hex format to DB double format
		 */
		if (dh1 < 0) {
			long_sign = long_high_bit;
		} else {
			long_sign = 0;
		}
		long_exp = (long) ((((dh1 & long_dh_exp_bits) >>> 56) - 0x40) << 2) - 4;
		long_man = dh1 & long_dh_man_bits;
		if (long_man == 0) {
			return 0;
		}
		fp_round = 0;
		while (long_man > long_db_one_bits) {
			fp_round = (int) (long_man & 1);
			long_man = long_man >> 1;
			long_exp++;
			if (fp_round == 1 && long_man <= long_db_one_bits) {
				long_man++;
			}
		}
		work_fp_reg.putLong(0, long_sign | ((long_exp + 0x3ff) << 52)
				| (long_man & long_db_man_bits));
		return work_fp_reg.getDouble(0);
	}

	public int zcvt_db_to_eh(double db1) {
		/*
		 * convert db float to eh int
		 */
		work_fp_reg.putDouble(0, db1);
		long_work = work_fp_reg.getLong(0);
		if (long_work == 0) {
			return int_eh_zero;
		}
		tz390.fp_exp = (int) ((long) (long_work & long_db_exp_bits) >>> 52) - 0x3ff - 4;
		int_man = (int) (((long_work & long_db_man_bits) | long_db_one_bit) >>> (52 - 24 - 4));
		fp_round = 0;
		while (int_man > int_eh_man_bits | (tz390.fp_exp & 3) != 0) {
			fp_round = int_man & 1;
			int_man = int_man >>> 1;
			tz390.fp_exp++;
			if (fp_round == 1 && int_man <= int_eh_man_bits) {
				int_man++;
			}
		}
		if (long_work < 0) {
			tz390.fp_sign = int_high_bit;
		} else {
			tz390.fp_sign = 0;
		}
		return tz390.fp_sign | (((tz390.fp_exp >> 2) + 0x40) << 24) | int_man;
	}

	public long zcvt_db_to_dh(double db1) {
		/*
		 * convert db float to eh long
		 */
		work_fp_reg.putDouble(0, db1);
		long_work = work_fp_reg.getLong(0);
		if (long_work == 0) {
			return long_dh_zero;
		}
		tz390.fp_exp = (int) ((long) (long_work & long_db_exp_bits) >>> 52) - 0x3ff;
		long_man = ((long) (long_work & long_db_man_bits) | long_db_one_bit) << 4;
		fp_round = 0;
		while (long_man > long_dh_man_bits || (tz390.fp_exp & 3) != 0) {
			fp_round = (int) (long_man & 1);
			long_man = long_man >>> 1;
			tz390.fp_exp++;
			if (fp_round == 1 && long_man <= long_dh_man_bits) {
				long_man++;
			}
		}
		if (long_work < 0) {
			long_sign = long_high_bit;
		} else {
			long_sign = 0;
		}
		return long_sign | ((long) ((tz390.fp_exp >> 2) + 0x40) << 56)
				| long_man;
	}

	private BigDecimal zcvt_eh_to_bd(ByteBuffer eh1_buff, int eh1_index) {
		/*
		 * convert EH in fp_reg or mem to big_dec
		 * 
		 */
		return new BigDecimal(zcvt_eh_to_db(eh1_buff.getInt(eh1_index)),
				fp_e_context);
	}

	private BigDecimal zcvt_dh_to_bd(ByteBuffer dh1_buff, int dh1_index) {
		/*
		 * convert DH in fp_reg or mem to big_dec
		 * 
		 * first create big dec with 56 mantissa bits
		 */
		return new BigDecimal(zcvt_dh_to_db(dh1_buff.getLong(dh1_index)),
				fp_d_context);
	}

	private BigDecimal zcvt_lh_to_bd(ByteBuffer lh1_buff, int lh1_index) {
		/*
		 * convert LH in fp_reg or mem to big_dec
		 * 
		 * first copy 112 bit mantissa into 15 byte array with leading 0 byte
		 * and convert to bd1 big decimal with 128 bit precision
		 */
		if (lh1_buff == fp_reg) { // RPI 229
			lh1_buff.position(lh1_index + 1);
			lh1_buff.get(work_fp_bi1_bytes, 1, 7);
			lh1_buff.position(lh1_index + 17);
			lh1_buff.get(work_fp_bi1_bytes, 8, 7);
		} else {
			lh1_buff.position(lh1_index + 1);
			lh1_buff.get(work_fp_bi1_bytes, 1, 7);
			lh1_buff.position(lh1_index + 9);
			lh1_buff.get(work_fp_bi1_bytes, 8, 7);
		}
		work_fp_bi1_bytes[0] = 0;
		work_fp_bd1 = new BigDecimal(new BigInteger(1, work_fp_bi1_bytes),
				fp_bd_context);
		/*
		 * if mantinssa zero exit with big decimal zero
		 */
		if (work_fp_bd1.signum() == 0) {
			return work_fp_bd1;
		}
		/*
		 * get sign and exponent as integer
		 */
		int_work = (int) lh1_buff.get(lh1_index);
		tz390.fp_exp = (((int_work & 0x7f) - 0x40) << 2) - 112;
		/*
		 * multiply big decimal by 2 ** exp
		 */
		work_fp_bd1 = work_fp_bd1.multiply(
				BigDecimal.valueOf(2).pow(tz390.fp_exp, fp_bd_context),
				fp_bd_context).round(fp_x_context);
		/*
		 * return pos or neg big decimal value
		 */
		if (int_work >= 0) {
			return work_fp_bd1;
		} else {
			return work_fp_bd1.negate();
		}
	}

	private BigDecimal zcvt_lb_to_bd(ByteBuffer lb1_buff, int lb1_index) {
		/*
		 * convert LB in fp_reg or mem to big_dec
		 * 
		 * first copy 112 bit mantissa into 15 byte array with leading 0 byte
		 * and convert to bd1 big decimal with 128 bit precision
		 */
		if (lb1_buff == fp_reg) { // RPI 229
			lb1_buff.position(lb1_index + 2);
			lb1_buff.get(work_fp_bi1_bytes, 1, 6);
			lb1_buff.position(lb1_index + 16);
			lb1_buff.get(work_fp_bi1_bytes, 7, 8);
		} else {
			lb1_buff.position(lb1_index + 2);
			lb1_buff.get(work_fp_bi1_bytes, 1, 14);
		}
		work_fp_bi1_bytes[0] = 1;
		work_fp_bd1 = new BigDecimal(new BigInteger(1, work_fp_bi1_bytes));
		/*
		 * if mantinssa zero exit with big decimal zero
		 */
		if (work_fp_bd1.signum() == 0) {
			return work_fp_bd1;
		}
		/*
		 * get sign and exponent as integer
		 */
		int_work = lb1_buff.getShort(lb1_index);
		tz390.fp_exp = (int_work & 0x7fff) - 0x3fff - 112;
		/*
		 * multiply big decimal by 2 ** exp
		 */
		work_fp_bd1 = work_fp_bd1.multiply(
				BigDecimal.valueOf(2).pow(tz390.fp_exp, fp_bd_context),
				fp_bd_context).round(fp_x_context);
		if (work_fp_bd1.compareTo(fp_lb_min) < 0) {
			return BigDecimal.ZERO;
		}
		/*
		 * return pos or neg big decimal value
		 */
		if (int_work >= 0) {
			return work_fp_bd1;
		} else {
			return work_fp_bd1.negate();
		}
	}

	public void zcvt_bd(int fp_type, BigDecimal fp_bd) {
		/*
		 * store LH, LB, DD, ED, or LD RPI 514 format in 16 byte fp_work_reg
		 * from big decimal
		 */
		if (fp_bd.signum() > 0) {
			tz390.fp_sign = 0;
		} else if (fp_bd.signum() < 0) {
			tz390.fp_sign = tz390.fp_sign_bit[fp_type];
			fp_bd = fp_bd.abs();
		} else {
			switch (fp_type) { // gen zero hex for fp_type
			case 1: // tz390.fp_dd_type s1,cf5,bxcf8,ccf50
				tz390.fp_work_reg.position(0);
				tz390.fp_work_reg.put(fp_dd_zero);
				return;
			case 4: // tz390.fp_ed_type s1,cf5,bxcf6,ccf20
				tz390.fp_work_reg.position(0);
				tz390.fp_work_reg.put(fp_ed_zero);
				return;
			case 6: // tz390.fp_lb_type s1,e15,m112 with assumed 1
				tz390.fp_work_reg.position(0);
				tz390.fp_work_reg.put(fp_lb_zero);
				return;
			case 7: // tz390.fp_ld_type s1,cf5,bxcf12,ccf110
				tz390.fp_work_reg.position(0);
				tz390.fp_work_reg.put(fp_ld_zero);
				return;
			case 8: // tz390.fp_lh_type s1,e7,m112 with split hex
				tz390.fp_work_reg.position(0);
				tz390.fp_work_reg.put(fp_lh_zero);
				return;
			}
		}
		switch (fp_type) {
		case 1: // fp_dd_type
			if (!tz390.get_dfp_bin(tz390.fp_dd_type, fp_bd)) {
				set_psw_check(psw_pic_fp_ovf);
				return;
			}
			break;
		case 4: // fp_ed_type
			if (!tz390.get_dfp_bin(tz390.fp_ed_type, fp_bd)) {
				set_psw_check(psw_pic_fp_ovf);
				return;
			}
			break;
		case 6: // fp_lh_type
			fp_zcvt_xbd(fp_type, fp_bd);
			break;
		case 7: // fp_ld_type
			if (!tz390.get_dfp_bin(tz390.fp_ld_type, fp_bd)) {
				set_psw_check(psw_pic_fp_ovf);
			}
			break;
		case 8: // fp_lh_type
			fp_zcvt_xbd(fp_type, fp_bd);
			break;
		}
	}

	private void fp_zcvt_xbd(int fp_type, BigDecimal fp_bd) {
		/*
		 * convert non-zero lb/lh to work_reg binary format
		 */
		/***********************************************************************
		 * calc tz390.fp_exp and big_dec2 such that: * big_dec1 = big_dec2 * 2 **
		 * tz390.fp_exp * **************************************
		 * 
		 * tz390.fp_exp = log(big_dec1) / log(2)
		 * 
		 * Since the exponent of LH/LB values can exceed the range of double,
		 * the log of big_dec1 is calculated using equivalent form
		 * log(x*10*scale) = log(x_man) + scale * log(10)
		 * 
		 * tz390.fp_exp must then be offset by the number of bits in the
		 * required mantissa in order to retain significant bits when big_dec2
		 * is converted to big_int format. The exponent is also reduced by 1 for
		 * assumed bit in binary formats plus 1 additional to insure rounding
		 * for irrational values is done by shifting right.
		 * 
		 */
		int work_scale = -fp_bd.stripTrailingZeros().scale();
		double work_man = fp_bd.multiply(
				BigDecimal.TEN.pow(-work_scale, fp_bd_context), fp_bd_context)
				.doubleValue();
		tz390.fp_exp = (int) ((Math.log(work_man) + ((double) work_scale * fp_log10)) / fp_log2)
				- tz390.fp_man_bits[fp_type] - tz390.fp_one_bit_adj[fp_type];
		/*
		 * Now calc big_dec2 mantissa truncated integer tz390.fp_exp calculated
		 * above. This calculation may produce an irrational number with the
		 * precison specified due to base 10 to base 2 exponent conversion.
		 * 
		 * big_dec2 = f[_bd / 2 ** tz390.fp_exp/
		 * 
		 */
		fp_big_dec2 = fp_bd.multiply(BigDecimal.valueOf(2).pow(-tz390.fp_exp,
				fp_bd_context), fp_bd_context);
		/*
		 * retrieve fp_big_dec2 mantissa bits as big_int and adjust tz390.fp_exp
		 * by mantissa bits
		 */
		fp_big_int1 = fp_big_dec2.toBigInteger();
		tz390.fp_exp = tz390.fp_exp + tz390.fp_man_bits[fp_type];
		/*
		 * adjust mantiss and base 2 exponent to align for assumed 1 bit for
		 * IEEE binary or IBM base 16 hex exponent and return hex sign bit,
		 * exponent, and mantissa bytes
		 */
		switch (fp_type) { // gen byte array for fp type
		case 6: // tz390.fp_lb_type s1,e15,m112 with assumed 1
			fp_round = 0;
			while (fp_big_int1.compareTo(fp_big_int_one_bits) > 0) {
				if (fp_big_int1.testBit(0)) {
					fp_round = 1;
				} else {
					fp_round = 0;
				}
				fp_big_int1 = fp_big_int1.shiftRight(1);
				tz390.fp_exp++;
				if (fp_round == 1
						&& fp_big_int1.compareTo(fp_big_int_one_bits) <= 0) {
					fp_big_int1 = fp_big_int1.add(BigInteger.ONE);
				}
			}
			tz390.fp_exp = tz390.fp_exp + tz390.fp_exp_bias[fp_type];
			if (tz390.fp_exp >= 0 && tz390.fp_exp <= tz390.fp_exp_max[fp_type]) {
				tz390.fp_work_reg.position(0 + 1); // first byte value 0x01
													// replaced with exp
				tz390.fp_work_reg.put(fp_big_int1.toByteArray());
				tz390.fp_work_reg.putShort(0,
						(short) (tz390.fp_sign | tz390.fp_exp));
			} else {
				set_psw_check(psw_pic_fp_sig);
				tz390.fp_work_reg.position(0);
				tz390.fp_work_reg.put(fp_lh_zero);
			}
			break;
		case 8: // tz390.fp_lh_type s1,e7,m112 with split hex
			fp_round = 0;
			while (fp_big_int1.compareTo(fp_big_int_man_bits) > 0
					|| ((tz390.fp_exp >> 2) << 2) != tz390.fp_exp) {
				if (fp_big_int1.testBit(0)) {
					fp_round = 1;
				} else {
					fp_round = 0;
				}
				fp_big_int1 = fp_big_int1.shiftRight(1);
				tz390.fp_exp++;
				if (fp_round == 1
						&& fp_big_int1.compareTo(fp_big_int_man_bits) <= 0) {
					fp_big_int1 = fp_big_int1.add(BigInteger.ONE);
				}
			}
			tz390.fp_exp = (tz390.fp_exp >> 2) + tz390.fp_exp_bias[fp_type];
			if (tz390.fp_exp >= 0 && tz390.fp_exp <= tz390.fp_exp_max[fp_type]) {
				tz390.fp_work_reg.put(0, (byte) (tz390.fp_sign | tz390.fp_exp));
				tz390.fp_work_reg.position(0 + 2);
				tz390.fp_work_reg.put(fp_big_int1.toByteArray());
				if (tz390.fp_work_reg.get(2) == 0) { // check 0 lead byte
					tz390.fp_work_reg.position(1);
					tz390.fp_work_reg.put(fp_big_int1.toByteArray());
				}
				tz390.fp_work_reg.putLong(0 + 1, tz390.fp_work_reg
						.getLong(0 + 2));
				if (tz390.fp_work_reg.getLong(0) != 0) {
					tz390.fp_work_reg
							.put(
									0 + 8,
									(byte) (tz390.fp_sign | ((tz390.fp_exp - 14) & 0x7f))); // RPI
																							// 384
				} else {
					tz390.fp_work_reg.put(0 + 8, (byte) 0x00); // RPI 384
				}
			} else {
				set_psw_check(psw_pic_fp_sig);
				tz390.fp_work_reg.position(0);
				tz390.fp_work_reg.put(fp_lh_zero);
			}
			break;
		}
	}

	private void exec_ed_edmk(boolean edmk_store) {
		/*
		 * execute ed or edmk with store into r1 Notes: 1. mask must be EBCDIC
		 * in both modes 2. output translated to ASCII if ascii mode
		 */
		psw_cc = psw_cc_equal; // assume last field zero;
		byte target_byte = ' ';
		byte source_byte = ' ';
		int next_digit = 0;
		int source_sign = 0;
		byte fill_byte = (byte) (mem_byte[bd1_loc] & 0xff);
		boolean sig_digit = false;
		boolean left_digit = true;
		while (rflen > 0) {
			target_byte = mem_byte[bd1_loc];
			switch (target_byte) {
			case 0x21: // select significant digit
			case 0x20: // select digit
				if (left_digit) {
					source_byte = mem_byte[bd2_loc];
					next_digit = (source_byte & 0xf0) >> 4;
					bd2_loc++;
					source_sign = source_byte & 0xf;
					if (source_sign <= 9) {
						left_digit = false;
					}
				} else {
					left_digit = true;
					next_digit = source_byte & 0x0f;
				}
				if (!sig_digit && next_digit == 0) {
					mem_byte[bd1_loc] = fill_byte;
					if (target_byte == 0x21) { // RPI43
						sig_digit = true;
					}
				} else {
					mem_byte[bd1_loc] = (byte) (next_digit | 0xf0);
					sig_digit = true;
					if (edmk_store) {
						edmk_store = false;
						reg.putInt(r1, bd1_loc);
					}
					if (next_digit != 0) { // assume pos if not zero
						psw_cc = psw_cc_high;
					}
				}
				if (source_sign > 9) {
					if (source_sign == 0xc || source_sign >= 0xe) {
						sig_digit = false;
					} else if (psw_cc == psw_cc_high) {
						psw_cc = psw_cc_low;
					}
				}
				break;
			case 0x22: // field separator
				sig_digit = false;
				mem_byte[bd1_loc] = fill_byte;
				psw_cc = psw_cc_equal;
				break;
			default: // any other char in mask
				if (!sig_digit) {
					mem_byte[bd1_loc] = fill_byte;
				}
				break;
			}
			if (tz390.opt_ascii) {
				mem_byte[bd1_loc] = tz390.ebcdic_to_ascii[mem_byte[bd1_loc] & 0xff];
			}
			bd1_loc++;
			rflen--;
		}
	}

	private void get_pdf_ints() {
		/*
		 * set pdf_is_big to true or false and set pdf_big_int1 and pdf_big_int2
		 * or pdf_long1 and pdf_long2
		 */
		if (get_pd(bd1_loc, rflen1)) { // RPI 305
			if (pdf_is_big) {
				pdf_big_int1 = pdf_big_int;
				if (get_pd(bd2_loc, rflen2)) { // RPI 305
					if (pdf_is_big) {
						pdf_big_int2 = pdf_big_int;
					} else {
						pdf_is_big = true;
						pdf_big_int2 = BigInteger.valueOf(pdf_long);
					}
				}
			} else {
				pdf_long1 = pdf_long;
				if (get_pd(bd2_loc, rflen2)) { // RPI 305
					if (pdf_is_big) {
						pdf_big_int2 = pdf_big_int;
						pdf_big_int1 = BigInteger.valueOf(pdf_long1);
					} else {
						pdf_long2 = pdf_long;
					}
				}
			}
		}
	}

	private boolean get_pd(int pdf_loc, int pdf_len) {
		/*
		 * convert pd field to pdf_big_int or convert to pdf_long and return
		 * true and set pd_cc for use by TP as follows: cc0 - sign and digits ok
		 * cc1 = sign invalid cc2 = at least one digit invalid cc3 = sign and at
		 * least one digit invalid else return false if error causing 0C7
		 * 
		 * Notes: 1. Raises psw_pic_data if invalid digit or sign unless opcode
		 * is tp. 2. If opcode is tp, sets cc accordingly.
		 */
		pdf_is_big = false; // RPI 389
		pd_cc = psw_cc0;
		if (pdf_len <= 4) {
			pdf_str = Integer.toHexString(mem.getInt(pdf_loc));
			pdf_zeros = 8 - pdf_str.length();
		} else if (pdf_len <= 8) {
			pdf_str = Long.toHexString(mem.getLong(pdf_loc));
			pdf_zeros = 16 - pdf_str.length(); // RPI 389 was 16
		} else {
			pdf_str = Long.toHexString(mem.getLong(pdf_loc));
			String last_half = Long.toHexString(mem.getLong(pdf_loc + 8));
			pdf_str = pdf_str
					+ ("0000000000000000" + last_half).substring(last_half
							.length());
			pdf_zeros = 32 - pdf_str.length();
		}
		pdf_str_len = 2 * pdf_len - 1 - pdf_zeros; // assume positive
		if (pdf_str_len < 0) {
			pdf_str_len = 0; // RPI109 catch 0 sign nibble
			pd_cc = psw_cc1;
			if (opcode1 != 0xeb || opcode2 != 0xc0) { // not TP
				fp_dxc = fp_dxc_dec;
				set_psw_check(psw_pic_data); // RPI 441
				return false; // RPI 305
			}
			return false; // RPI 305
		}
		pdf_sign = pdf_str.charAt(pdf_str_len);
		if (pdf_str_len == 0) {
			pdf_str = "0";
			pdf_str_len = 1;
		}
		switch (pdf_sign) {
		case '3': // ascii plus
		case 'a': // ebcdic plus
		case 'c': // ebcdic plus (default for P'..')
		case 'e': // ebcdic plus
		case 'f': // ebcdic plus (from PACK)
			pdf_str = pdf_str.substring(0, pdf_str_len);
			break;
		case 'b': // acii negative
		case 'd': // ebcdic negative
			pdf_str = "-" + pdf_str.substring(0, pdf_str_len);
			break;
		default:
			pdf_str = pdf_str.substring(0, pdf_str_len);
			pd_cc = psw_cc1;
			if (opcode1 != 0xeb || opcode2 != 0xc0) { // not TP
				fp_dxc = fp_dxc_dec;
				set_psw_check(psw_pic_data);
				return false; // RPI 305
			}
			break;
		}
		try {
			if (pdf_str.length() < 10) { // RPI 389 long can have up to 20
				pdf_long = Long.valueOf(pdf_str);
			} else {
				pdf_big_int = new BigInteger(pdf_str);
				if (pdf_big_int.compareTo(bi_max_pos_long) != 1
						&& pdf_big_int.compareTo(bi_min_neg_long) != -1) {
					pdf_long = pdf_big_int.longValue();
				} else {
					pdf_is_big = true;
				}
			}
		} catch (Exception e) {
			if (pd_cc == psw_cc0) {
				pd_cc = psw_cc2; // digit error only
			} else {
				pd_cc = psw_cc3; // digit and sign errors
			}
			if (opcode1 != 0xeb || opcode2 != 0xc0) { // not TP
				fp_dxc = fp_dxc_dec;
				set_psw_check(psw_pic_data);
				return false; // RPI 305
			}
		}
		return true;
	}

	private void put_pd(byte[] pd_stor, int pdf_loc, int pdf_len) {
		/*
		 * if pdf_is_big store pdf_big_int value into pd field and set pd_cc
		 * else store pdf_long into pdf field and set pd_cc.
		 */
		if (pdf_is_big) {
			pdf_str = pdf_big_int.toString();
		} else {
			pdf_str = Long.valueOf(pdf_long).toString();
		}
		byte next_byte = 0xc;
		int end_index = 0;
		if (pdf_str.charAt(0) == '-') {
			next_byte = 0xd;
			pd_cc = psw_cc_low;
			end_index = 1;
		} else if (pdf_str.length() == 1 && pdf_str.charAt(0) == '0') {
			pd_cc = psw_cc_equal;
		} else {
			pd_cc = psw_cc_high;
		}
		int index = pdf_str.length() - 1;
		int pdf_index = pdf_loc + pdf_len - 1;
		boolean left_digit = true;
		while (index >= end_index && pdf_index >= pdf_loc) {
			if (left_digit) {
				pd_stor[pdf_index] = (byte) (next_byte | ((pdf_str
						.charAt(index) & 0xf) << 4));
				next_byte = 0;
				pdf_index--;
				left_digit = false;
			} else {
				next_byte = (byte) (pdf_str.charAt(index) & 0xf);
				left_digit = true;
			}
			index--;
		}
		while (pdf_index >= pdf_loc) {
			pd_stor[pdf_index] = next_byte;
			next_byte = 0;
			pdf_index--;
		}
		if (index >= end_index) {
			pd_cc = psw_cc3;
			set_psw_check(psw_pic_pd_ovf);
		}
	}

	private void ex_restore() {
		/*
		 * restore ex target instruction 2nd byte
		 */
		mem_byte[psw_loc + 1] = ex_mod_byte;
	}

	public String bytes_to_hex(ByteBuffer bytes, int byte_start,
			int byte_length, int chunk) {
		/*
		 * Format bytes into hex string
		 */
		if (byte_start < 0) {
			return "";
		}
		StringBuffer hex = new StringBuffer(72);
		int index1 = 0;
		int hex_bytes = 0;
		byte_start = byte_start & psw_amode;
		if (byte_start + byte_length > bytes.capacity()) {
			byte_length = bytes.capacity() - byte_start;
		}
		while (index1 < byte_length) {
			int work_int = bytes.get(byte_start + index1) & 0xff;
			String temp_string = Integer.toHexString(work_int);
			if (temp_string.length() == 1) {
				hex.append("0" + temp_string);
			} else {
				hex.append(temp_string);
			}
			index1++;
			if (chunk > 0) {
				hex_bytes++;
				if (hex_bytes >= chunk && index1 < byte_length) {
					hex.append(" ");
					hex_bytes = 0;
				}
			}
		}
		if (hex.length() > 0) {
			return hex.toString().toUpperCase();
		} else {
			return "";
		}
	}

	public void init_pz390(tz390 shared_tz390, sz390 shared_sz390) {
		/*
		 * init tz390
		 */
		tz390 = shared_tz390;
		sz390 = shared_sz390;
		init_fp();
		init_opcode_keys();
		init_mem();
		if (tz390.opt_ascii) {
			pdf_zone = 0x30; // zone for UNPK
		}
	}

	private void init_fp() {
		/*
		 * init fp constants that use tz390 shared variables
		 */
		fp_bd_context = new MathContext(tz390.fp_precision[tz390.fp_lb_type]);
		fp_db_context = new MathContext(tz390.fp_precision[tz390.fp_db_type]);
		fp_eb_context = new MathContext(tz390.fp_precision[tz390.fp_eb_type]);
		fp_lh_min = new BigDecimal("5.41e-79", fp_bd_context);
		fp_lh_max = new BigDecimal("7.2e+75", fp_bd_context);
		fp_lb_min = new BigDecimal("3.3e-4932", fp_bd_context); // RPI 367 allow
																// (MIN)
		fp_lb_max = new BigDecimal("1.2e+4932", fp_bd_context);
		fp_dd_pos_max = new BigDecimal("9999999999999999e+369",fp_dd_context);
		fp_dd_neg_max = new BigDecimal("-9999999999999999e+369",fp_dd_context);
	    fp_dd_pos_min = new BigDecimal("1e-398",fp_dd_context);
	    fp_dd_neg_min = new BigDecimal("-1e-398",fp_dd_context);
	}

	private void init_opcode_keys() {
		/*
		 * add all opcodes to key index table for use by trace and test options
		 * Notes: 1. trace and test do lookup by hex for display 2. test break
		 * on opcode does lookup by name to get hex code for break 3.
		 * op_type_index is used by test break on opcode to get opcode2 offset
		 * and mask
		 */
		if (!tz390.init_opcode_name_keys()) {
			set_psw_check(psw_pic_operr);
		}
		String hex_key = null;
		int index = 1;
		while (index < tz390.op_code.length) {
			// add key by hex code
			hex_key = tz390.op_code[index];
			if (hex_key.length() == 3) {
				hex_key = hex_key.substring(0, 2) + "0" + hex_key.charAt(2);
				if (hex_key.charAt(1) == '7'
						&& (hex_key.charAt(0) == '4' || hex_key.charAt(0) == '0')) {
					hex_key = "BC=" + hex_key;
				}
			} else if (hex_key.length() == 4) {
				if (hex_key.substring(0, 3).equals("A74")) {
					hex_key = "BR=" + hex_key;
				} else if (hex_key.substring(0, 3).equals("C04")) {
					hex_key = "BL=" + hex_key; // RIP200
				}
			}
			if (tz390.find_key_index('H', hex_key) == -1) {
				if (!tz390.add_key_index(index)) {
					set_psw_check(psw_pic_operr);
				}
			} else {
				dup_opcodes++;
			}
			index++;
		}
		// init opcode2_offsets and masks for each type
		op_type_offset[1] = 1; // E PR oooo
		op_type_offset[7] = 1; // S SSM oooobddd
		op_type_offset[12] = 1; // RI IIHH ooroiiii
		op_type_offset[13] = 1; // BRE BRC oomoiiii
		op_type_offset[14] = 1; // RRE MSR oooo00rr
		op_type_offset[15] = 1; // RRF1 MAER oooo1032
		op_type_offset[16] = 1; // RIL BRCL oomollllllll
		op_type_offset[18] = 5; // RXY MLG oorxbdddhhoo
		op_type_offset[19] = 1; // SSE LASP oooobdddbddd
		op_type_offset[20] = 5; // RSY LMG oorrbdddhhoo
		op_type_offset[21] = 5; // SIY TMY ooiibdddhhoo
		op_type_offset[22] = 5; // RSL TP oor0bddd00oo
		op_type_offset[23] = 5; // RIE BRXLG oorriiii00oo
		op_type_offset[24] = 5; // RXE ADB oorxbddd00oo
		op_type_offset[25] = 5; // RXF MAE oorxbdddr0oo
		op_type_offset[30] = 1; // RRF3 DIEBR oooo3412
		op_type_offset[32] = 1; // SSF MVCOS oor0bdddbddd
		op_type_offset[34] = 1; // RRF2 FIEBR ooooM012
		op_type_offset[35] = 1; // RRF4 CSDTR oooo0m12 RPI 407
		op_type_offset[36] = 1; // RRR ADTR oooo3012
		op_type_mask[1] = 0xff; // E PR oooo
		op_type_mask[7] = 0xff; // S SSM oooobddd
		op_type_mask[12] = 0x0f; // RI IIHH ooroiiii
		op_type_mask[13] = 0xf0; // BRE BRC oomoiiii
		op_type_mask[14] = 0xff; // RRE MSR oooo00rr
		op_type_mask[15] = 0xff; // RRF1 MAER oooo1032
		op_type_mask[16] = 0x0f; // RIL BRCL oomollllllll
		op_type_mask[18] = 0xff; // RXY MLG oorxbdddhhoo
		op_type_mask[19] = 0xff; // SSE LASP oooobdddbddd
		op_type_mask[20] = 0xff; // RSY LMG oorrbdddhhoo
		op_type_mask[21] = 0xff; // SIY TMY ooiibdddhhoo
		op_type_mask[22] = 0xff; // RSL TP oor0bddd00oo
		op_type_mask[23] = 0xff; // RIE BRXLG oorriiii00oo
		op_type_mask[24] = 0xff; // RXE ADB oorxbddd00oo
		op_type_mask[25] = 0xff; // RXF MAE oorxbdddr0oo
		op_type_mask[30] = 0xff; // RRF3 DIER oooo3012
		op_type_mask[32] = 0x0f; // SSF MVCOS oor0bdddbddd
		op_type_mask[34] = 0xff; // RRF2 DIEBR oooo3012
		op_type_mask[35] = 0xff; // RRF4 CSDTR oooo0m12 RPI 407
		op_type_mask[36] = 0xff; // RRR ADTR oooo3012 RPI 407
		// init op2 offset and mask arrays indexed by op1
		index = 1; // skip 0 comment code entry
		while (index < tz390.op_code.length) {
			int op1 = Integer.valueOf(tz390.op_code[index].substring(0, 2), 16)
					.intValue();
			if (tz390.op_code[index].length() > 2) {
				opcode2_offset[op1] = op_type_offset[tz390.op_type[index]]; // RPI92//RPI135
				opcode2_mask[op1] = op_type_mask[tz390.op_type[index]]; // RPI92//RPI135
			}
			index++;
		}
		tz390.tot_key_search = 0; // reset after adding opcodes
		tz390.tot_key_comp = 0;
		tz390.max_key_comp = 0;
	}

	private void init_mem() {
		/*
		 * allocate memory and initialize as follows: 1. Set cvt pointer at 16
		 * 2. Set zcvt_ipl_pgm to name of opt_ipl pgm name if any to run via
		 * link svc 6. 3. Set zcvt_user_pgm to name of user initial program to
		 * run via link svc. 4. Set mem_fqe_ptr to point to free queue with all
		 * of available memory for use by svc 4 getmain and svc 5 freemain. a.
		 * MEM(MB) defaults to 1 MB but can be set to any value up to available
		 * memory on machine. See TEST\TESTMEM1 for test of ABOVE/BELOW GETMAIN
		 * using MEM(32) to allocate 16MB below and 16 MB above. b. If less than
		 * 16 MB allocated 31 bit fqe is set to 0 and RMODE31 requests use
		 * memory below the line. 5. Add 8 bytes to physical mem_byte array
		 * allocated to allow get_pd to fetch 8 bytes from last byte in memory
		 * for 1-8 byte packed decimal field. Notes: 1. The default J2RE maximum
		 * memory available is limited by default to small fraction of total
		 * memory to prevent stalling OS or other J2RE applications running. To
		 * override default set -Xmx option at startup of J2RE. For example, add
		 * -Xmx500000000 to z390.bat to allow allocating up to 500 MB but don't
		 * allocate all of it or other J2RE application such as IE may stall or
		 * abort.
		 */
		tot_mem = tz390.max_mem << 20; // cvt MB to bytes
		try {
			mem_byte = new byte[tot_mem + 8];
			mem = ByteBuffer.wrap(mem_byte, 0, tot_mem + 6);
		} catch (Exception e) {
			set_psw_check(psw_pic_memerr);
		}
		/*
		 * init 24 bit free memory queue element
		 */
		dsa24_start = mem24_start;
		mem.position(dsa24_start);
		mem.putInt(0); // set 24 bit fqe next to 0
		if (tot_mem > mem24_line) {
			dsa24_end = mem24_line;
			mem.putInt(dsa24_end - dsa24_start); // set 24 but fqe len
		} else {
			dsa24_end = tot_mem;
			mem.putInt(dsa24_end - dsa24_start);
		}
		/*
		 * init 31 bit free memory queue element
		 */
		if (tot_mem > mem24_line) {
			dsa31_start = mem24_line;
			mem.position(dsa31_start);
			mem.putInt(0); // set 31 bit fqe next = 0
			dsa31_end = tot_mem;
			mem.putInt(dsa31_end - dsa31_start); // set 31 bit fqe len
		}
		/*
		 * init cvt pointer in low memory psacvt x'10' and x'4c'
		 */
		mem.putInt(psa_cvt, cvt_start);
		mem.putInt(psa_cvt2, cvt_start);
		/*
		 * init cvt pointers to dsa fqe's for use by getmain and freemain svc
		 * hanlers
		 */
		mem.position(zcvt_fqe24); // 24 bit fqe
		mem.putInt(dsa24_start);
		mem.position(zcvt_fqe31); // 31 bit fqe
		mem.putInt(dsa31_start);
		/*
		 * init cvt initial program load pgm name field
		 */
		set_pgm_name(zcvt_ipl_pgm, tz390.opt_ipl);
		set_pgm_name(zcvt_user_pgm, tz390.pgm_name);
		/*
		 * init zcvt_exec_parm from PARM(..) option Notes: 1. ez390 sets R1 to
		 * zcvt_exec_parm at start
		 */
		mem.position(zcvt_exec_parm);
		mem.putShort((short) tz390.opt_parm.length());
		int index = 0;
		while (index < tz390.opt_parm.length()) {
			if (tz390.opt_ascii) {
				mem.put((byte) tz390.opt_parm.charAt(index));
			} else {
				mem.put(tz390.ascii_to_ebcdic[tz390.opt_parm.charAt(index)]);
			}
			index++;
		}
		/*
		 * init svc 3 for return register r14
		 */
		mem.putShort(zcvt_exit, (short) 0x0a03); // svc 3
		mem.put(cvt_dcb, (byte) 0x9b); // os flags RPI 228
	}

	private void set_pgm_name(int pgm_name_addr, String pgm_name) {
		/*
		 * store program name in memory in EBCDIC
		 */
		mem.position(pgm_name_addr);
		int index = 0;
		while (index < pgm_name.length()) {
			if (pgm_name.charAt(index) != '.') {
				if (tz390.opt_ascii) {
					mem.put((byte) tz390.pgm_name.charAt(index));
				} else {
					mem.put(tz390.ascii_to_ebcdic[pgm_name.charAt(index)]);
				}
			} else {
				index = pgm_name.length();
			}
			index++;
		}
		while (index < 8) {
			if (tz390.opt_ascii) {
				mem.put((byte) ' ');
			} else {
				mem.put(tz390.ascii_to_ebcdic[32]);
			}
			index++;
		}
	}
}