import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.lang.reflect.Array;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.MathContext;
import java.nio.ByteBuffer;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Random;
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
    * 04/15/05 completed testa2.bal demo support with literal
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
    ********************************************************
    * Global variables
    *****************************************************/
	String version = null;
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
    File obj_file = null;
    BufferedWriter obj_file_buff = null;
    File prn_file = null;
    BufferedWriter prn_file_buff = null;
    String bal_line = null;
    String bal_label = null;
    String bal_op = null;
    boolean bal_op_ok = false;
    String bal_parms = null;
    boolean list_bal_line = false;
	boolean bal_abort = false;
    int bal_op_index = 0;
    boolean bal_eof = false;
    String  opt_parms = " ";
    boolean opt_ok = false;
    boolean opt_con      = true;
    boolean opt_list     = true;
    boolean opt_stats    = true;
    boolean opt_timing   = true;
    boolean opt_time     = true;
    boolean opt_trace    = false;
    boolean opt_traceall = false;
    boolean opt_trap     = true;
	SimpleDateFormat mmddyy = new SimpleDateFormat("MM/dd/yy");
	SimpleDateFormat hhmmss = new SimpleDateFormat("HH:mm:ss");
    boolean log_tod = true; 
    JTextArea z390_log_text = null;
    boolean az390_aborted = false;
    /*
     * static limits
     */
    static int max_errors = 100;
    static int max_pass = 4;
    static int max_bal_line = 200000;
    static int max_sym = 20000;
    static int max_lit = 20000;
    static int max_esd = 1000;
    static int max_use = 500;
    static int max_exp_stk = 500;
    static int max_exp_rld = 500;
    static int max_rld = 10000;
    static int max_text_buff_len = 16;
    long max_time_seconds = 10;     // max elapsed time
     /*
     * bal file global variables
     */
    long    tod_time_limit = 0;
    int     next_time_ins   = 0x1000;
    int     next_time_check = next_time_ins;
    int tot_bal_line = 0;
    String[]  bal_name_line = new String[max_bal_line];
    int[]     bal_name_line_num = (int[])Array.newInstance(int.class,max_bal_line);
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
    Pattern dcc_pattern = null;
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
    int tot_use = 0;
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
     * symbol table global variables
     */
    static byte sym_sdt   = 0;  // dec, b'', c'', h''
    static byte sym_cst   = 1;  // CSECT )alias REL)
    static byte sym_dst   = 2;  // DSECT (alias REL)
    static byte sym_ent   = 3;  // ENTRY (alias REL)
    static byte sym_ext   = 4;  // EXTRN 
    static byte sym_rel   = 5;  // RX (CST.DST,ENT)_
    static byte sym_rld   = 6;  // complex rld exp
    static byte sym_lct   = 7;  // loctr (changed to cst/dst). 
    int tot_sym = 0;
    int tot_sym_find = 0;
    int tot_sym_comp = 0;
    int cur_sid = 0;
    int prev_sect_sid = 0;
    int prev_sect_esd = 0;
    boolean sect_change = false;
    byte prev_sect_type = sym_cst;
    static String[] sym_type_desc = {
    	"ABS","CST","DST","ENT","EXT","REL"};
    String[]  sym_name       = new String[max_sym];
    int[]     sym_def        = (int[])Array.newInstance(int.class,max_sym);
    byte[]    sym_type       = (byte[])Array.newInstance(byte.class,max_sym);
    int[]     sym_esd        = (int[])Array.newInstance(int.class,max_sym);
    int[]     sym_loc        = (int[])Array.newInstance(int.class,max_sym);
    int[]     sym_max_loc    = (int[])Array.newInstance(int.class,max_sym);
    int[]     sym_len        = (int[])Array.newInstance(int.class,max_sym);
    int[]     sym_sect_type  = (int[])Array.newInstance(int.class,max_sym);
    int[]     sym_sect_prev  = (int[])Array.newInstance(int.class,max_sym);
    int[]     sym_sect_next  = (int[])Array.newInstance(int.class,max_sym);
    /*
     * literal table for next pool at LTORG or END
     */
    int tot_lit = 0;
    int cur_lit = 0;
    boolean lit_loc_ref = false;
    int cur_lit_pool = 1;
    String[]  lit_name     = new String[max_sym];
    int[]     lit_pool     = (int[])Array.newInstance(int.class,max_lit);
    int[]     lit_line     = (int[])Array.newInstance(int.class,max_lit);
    int[]     lit_line_loc = (int[])Array.newInstance(int.class,max_lit);
    int[]     lit_esd      = (int[])Array.newInstance(int.class,max_lit);
    int[]     lit_loc      = (int[])Array.newInstance(int.class,max_lit);
    int[]     lit_len      = (int[])Array.newInstance(int.class,max_lit);
    byte[]    lit_gen      = (byte[])Array.newInstance(byte.class,max_lit);
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
	String[] op_name = {
			   "*",        // comments
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
		       "DIEBR",    // 3980 "B353" "DIEBR" "RR4" 30
		       "FIEBR",    // 3990 "B357" "FIEBR" "RRF" 15
		       "THDER",    // 4000 "B358" "THDER" "RRE" 14
		       "THDR",     // 4010 "B359" "THDR" "RRE" 14
		       "DIDBR",    // 4020 "B35B" "DIDBR" "RR4" 30
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
		       "DP",       // 7130 "FD" "DP" "SS2" 26
		       "CCW",      // 7140  "CCW"  101
		       "CCW0",     // 7150  "CCW0"  102
		       "CCW1",     // 7160  "CCW1"  103
		       "DC",       // 7170  "DC"  104
		       "DS",       // 7180  "DS"  105
		       "ALIAS",    // 7190  "ALIAS"  106
		       "AMODE",    // 7200  "AMODE"  107
		       "CATTR",    // 7210  "CATTR"  108
		       "COM",      // 7220  "COM"  109
		       "CSECT",    // 7230  "CSECT"  110
		       "CXD",      // 7240  "CXD"  111
		       "DSECT",    // 7250  "DSECT"  112
		       "DXD",      // 7260  "DXD"  113
		       "ENTRY",    // 7270  "ENTRY"  114
		       "EXTRN",    // 7280  "EXTRN"  115
		       "LOCTR",    // 7290  "LOCTR"  116
		       "RMODE",    // 7300  "RMODE"  117
		       "RSECT",    // 7310  "RSECT"  118
		       "START",    // 7320  "START"  119
		       "WXTRN",    // 7330  "WXTRN"  120
		       "XATTR",    // 7340  "XATTR"  121
		       "",         // 7350  ""  122
		       "DROP",     // 7360  "DROP"  123
		       "USING",    // 7370  "USING"  124
		       "AEJECT",   // 7380  "AEJECT"  125
		       "ASPACE",   // 7390  "ASPACE"  126
		       "CEJECT",   // 7400  "CEJECT"  127
		       "EJECT",    // 7410  "EJECT"  128
		       "PRINT",    // 7420  "PRINT"  129
		       "SPACE",    // 7430  "SPACE"  130
		       "TITLE",    // 7440  "TITLE"  131
		       "ADATA",    // 7450  "ADATA"  132
		       "CNOP",     // 7460  "CNOP"  133
		       "COPY",     // 7470  "COPY"  134
		       "END",      // 7480  "END"  135
		       "EQU",      // 7490  "EQU"  136
		       "EXITCTL",  // 7500  "EXITCTL"  137
		       "ICTL",     // 7510  "ICTL"  138
		       "ISEQ",     // 7520  "ISEQ"  139
		       "LTORG",    // 7530  "LTORG"  140
		       "OPSYN",    // 7540  "OPSYN"  141
		       "ORG",      // 7550  "ORG"  142
		       "POP",      // 7560  "POP"  143
		       "PUNCH",    // 7570  "PUNCH"  144
		       "PUSH",     // 7580  "PUSH"  145
		       "REPRO",    // 7590  "REPRO"  146
		       "ACTR",     // 7600  "ACTR"  147
		       "AGO",      // 7610  "AGO"  148
		       "AIF",      // 7620  "AIF"  149
		       "AINSERT",  // 7630  "AINSERT"  150
		       "ANOP",     // 7640  "ANOP"  151
		       "AREAD",    // 7650  "AREAD"  152
		       "GBLA",     // 7660  "GBLA"  153
		       "GBLB",     // 7670  "GBLB"  154
		       "GBLC",     // 7680  "GBLC"  155
		       "LCLA",     // 7690  "LCLA"  156
		       "LCLB",     // 7700  "LCLB"  157
		       "LCLC",     // 7710  "LCLC"  158
		       "MHELP",    // 7720  "MHELP"  159
		       "MNOTE",    // 7730  "MNOTE"  160
		       "SETA",     // 7740  "SETA"  161
		       "SETAF",    // 7750  "SETAF"  162
		       "SETB",     // 7760  "SETB"  163
		       "SETC",     // 7770  "SETC"  164
		       "SETCF",    // 7780  "SETCF"  165
		       "MACRO",    // 7790  "MACRO"  166
		       "MEND",     // 7800  "MEND"  167
		       "MEXIT",    // 7810  "MEXIT"  168
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
		       26,  // 7130 "FD" "DP" "SS2" 26
		       101,  // 7140  "CCW"  101
		       102,  // 7150  "CCW0"  102
		       103,  // 7160  "CCW1"  103
		       104,  // 7170  "DC"  104
		       105,  // 7180  "DS"  105
		       106,  // 7190  "ALIAS"  106
		       107,  // 7200  "AMODE"  107
		       108,  // 7210  "CATTR"  108
		       109,  // 7220  "COM"  109
		       110,  // 7230  "CSECT"  110
		       111,  // 7240  "CXD"  111
		       112,  // 7250  "DSECT"  112
		       113,  // 7260  "DXD"  113
		       114,  // 7270  "ENTRY"  114
		       115,  // 7280  "EXTRN"  115
		       116,  // 7290  "LOCTR"  116
		       117,  // 7300  "RMODE"  117
		       118,  // 7310  "RSECT"  118
		       119,  // 7320  "START"  119
		       120,  // 7330  "WXTRN"  120
		       121,  // 7340  "XATTR"  121
		       122,  // 7350  ""  122
		       123,  // 7360  "DROP"  123
		       124,  // 7370  "USING"  124
		       125,  // 7380  "AEJECT"  125
		       126,  // 7390  "ASPACE"  126
		       127,  // 7400  "CEJECT"  127
		       128,  // 7410  "EJECT"  128
		       129,  // 7420  "PRINT"  129
		       130,  // 7430  "SPACE"  130
		       131,  // 7440  "TITLE"  131
		       132,  // 7450  "ADATA"  132
		       133,  // 7460  "CNOP"  133
		       134,  // 7470  "COPY"  134
		       135,  // 7480  "END"  135
		       136,  // 7490  "EQU"  136
		       137,  // 7500  "EXITCTL"  137
		       138,  // 7510  "ICTL"  138
		       139,  // 7520  "ISEQ"  139
		       140,  // 7530  "LTORG"  140
		       141,  // 7540  "OPSYN"  141
		       142,  // 7550  "ORG"  142
		       143,  // 7560  "POP"  143
		       144,  // 7570  "PUNCH"  144
		       145,  // 7580  "PUSH"  145
		       146,  // 7590  "REPRO"  146
		       147,  // 7600  "ACTR"  147
		       148,  // 7610  "AGO"  148
		       149,  // 7620  "AIF"  149
		       150,  // 7630  "AINSERT"  150
		       151,  // 7640  "ANOP"  151
		       152,  // 7650  "AREAD"  152
		       153,  // 7660  "GBLA"  153
		       154,  // 7670  "GBLB"  154
		       155,  // 7680  "GBLC"  155
		       156,  // 7690  "LCLA"  156
		       157,  // 7700  "LCLB"  157
		       158,  // 7710  "LCLC"  158
		       159,  // 7720  "MHELP"  159
		       160,  // 7730  "MNOTE"  160
		       161,  // 7740  "SETA"  161
		       162,  // 7750  "SETAF"  162
		       163,  // 7760  "SETB"  163
		       164,  // 7770  "SETC"  164
		       165,  // 7780  "SETCF"  165
		       166,  // 7790  "MACRO"  166
		       167,  // 7800  "MEND"  167
		       168,  // 7810  "MEXIT"  168
			};
	String[]   op_code = {
			   "*",     // comments
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
		       "B353",  // 3980 "B353" "DIEBR" "RR4" 30
		       "B357",  // 3990 "B357" "FIEBR" "RRF" 15
		       "B358",  // 4000 "B358" "THDER" "RRE" 14
		       "B359",  // 4010 "B359" "THDR" "RRE" 14
		       "B35B",  // 4020 "B35B" "DIDBR" "RR4" 30
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
    int max_key_tab = max_key_root + max_sym + max_lit + max_op;
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
   * current directory global variables
   */   
       String dir_cur = null;
       String dir_bal = null;
       String dir_prn = null;
       String dir_obj = null;
       String pgm_name = null;
  /*
   * object code text buffer variables
   */  
      boolean gen_obj_code = false;
  	  String obj_code = "";
  	  String list_obj_code = "";
      String cur_text_buff = null;
      int cur_text_loc = 0;
      int cur_text_len = 0;
      int cur_text_esd = 0;
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
      int    dc_len   = 0;
      int    dc_first_len = 0;
      int    dc_first_loc = 0;
      String dc_first_type = null;
      String dc_hex = null;
      byte[]     dc_data_byte = (byte[])Array.newInstance(byte.class,256);
      ByteBuffer dc_data = ByteBuffer.wrap(dc_data_byte,0,256);
      int dc_type_index = 0;
      static String dc_valid_types = "ABCDEFHLPSVXZ";
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
			1,  // X
			1   // Z
			};
      static int[] dc_type_align = {
      		4,  // A
			0,  // B
			0,  // C
			8,  // D
			4,  // E
			4,  // F
			2,  // H
			8,  // L
			0,  // P
			2,  // S
			4,  // V
			0,  // X
			0   // Z
			};
      static char[] dc_type_delimiter = {
      		'(',  // A
			'\'', // B
			'\'', // C
			'\'', // D
			'\'', // E
			'\'', // F
			'\'', // H
			'\'', // L
			'\'', // P
			'(',  // S
			'(',  // V
			'\'', // X
			'\''  // Z
			};
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
    	if (opt_trap){
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
        version = "V1.0.00 09/30/05";  //dsh
        process_args(args);
        set_options();    
		open_files();
        put_copyright();
        compile_patterns();
        init_ascii_ebcdic(); 
        init_opcodes();
        tod_time_limit = max_time_seconds * 1000 + tod_start;
}
private void process_args(String[] args){
	/*
	 * process bal bal macs options
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
	    abort_error(3,"missing bal file - " + dir_bal + pgm_name + ".BAL");
    }
}
private void set_dir_and_pgm_name(String file_name){
	/*
	 * set program name and default directory
	 * for all files from first parm.
	 * Notes:
	 *   1.  Use current directory if not path
	 *       specified with program name.
	 *   2.  Options SYSBAL, SYSOBJ, and SYSPRN
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
	dir_bal = dir_cur;
	dir_prn = dir_cur;
	dir_obj = dir_cur;
    index = file_name.lastIndexOf("\\.");
    if (index != -1){  // strip extension if any
    	pgm_name = file_name.substring(0,index);
    } else {
    	pgm_name = file_name;
    }
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
        	      +"|([, ])"
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
  		    	    "([b|B]['][0|1]+['])" 
  		    	  +	"|([c|C][']([^']|(['][']))*['])" 
	    		  +	"|([x|X]['][0-9a-fA-F]+['])" 
        		  +	"|([a-zA-Z$@#][a-zA-Z0-9$@#_]*[\\.])" 
        		  +	"|([a-zA-Z$@#][a-zA-Z0-9$@#_]*)" 
				  + "|([0-9]+)"
				  + "|([ ,'\\+\\-\\*\\/\\(\\)])"
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
             exp_op_class['L'] = 5;
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
         		  +	"|([c|C][']([^'|''])*['])" 
         		  + "|([']([^'|''])*['])"   
      			  + "|([^' ,()]+)"           
         	      + "|([' ,()])"
     			  );
         	} catch (Exception e){
         		  abort_error(1,"parm pattern errror - " + e.toString());
         	}
             /*
              * dcc_pattern for quoted string:
              *   1.  '...''...'
              * */
         	try {
         	    dcc_pattern = Pattern.compile(
         			"([^']+)"
         	      + "|([']['])"
				  + "|(['])"             
     			  );
         	} catch (Exception e){
         		  abort_error(1,"dcc pattern errror - " + e.toString());
         	}
}
private void init_opcodes(){
	/*
	 * add all opcodes to key index table
	 */
	if (max_op != op_type.length){ 
		abort_error(85,"opcode name, hex, index tables out of sync");
	}
	int index = 0;
	while (index < max_op){
		if (find_key_index("O:" + op_name[index]) == -1){
			add_key_index(index);
		} else {
			abort_error(86,"init opcode table error at " + op_name[index]);
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
private void open_files(){
	/*
	 * open obj and prn files
	 */
        obj_file = new File(dir_obj + pgm_name + ".OBJ");
       	try {
       	    obj_file_buff = new BufferedWriter(new FileWriter(obj_file));
       	} catch (IOException e){
       		abort_error(4,"I/O error on obj open - " + e.toString());
       	}
       	if (opt_list){
            prn_file = new File(dir_prn + pgm_name + ".PRN");
         	try {
       	       prn_file_buff = new BufferedWriter(new FileWriter(prn_file));
       	    } catch (IOException e){
       		   abort_error(4,"I/O error on prn open - " + e.toString());
       	    }
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
            } else if (tokens[index1].equals("NOCON")){
            	opt_con = false;
            } else if (tokens[index1].equals("NOLIST")){
            	opt_list = false;
            } else if (tokens[index1].equals("NOSTATS")){
            	opt_stats = false;
            } else if (tokens[index1].equals("NOTIME")){
            	opt_time = false;
            } else if (tokens[index1].equals("NOTIMING")){
            	opt_timing = false;
            } else if (tokens[index1].length() > 5
            		&& tokens[index1].substring(0,5).equals("TIME=(")){
            	max_time_seconds = Long.valueOf(tokens[index1].substring(5,tokens[index1].length()-1)).longValue();
            } else if (tokens[index1].length() > 7 && tokens[index1].substring(0,7).equals("SYSBAL(")){
            	dir_bal = tokens[index1].substring(7,tokens[index1].length()-1) + File.separator; 
            } else if (tokens[index1].length() > 7 && tokens[index1].substring(0,7).equals("SYSOBJ(")){
            	dir_obj = tokens[index1].substring(7,tokens[index1].length()-1) + File.separator; 
            } else if (tokens[index1].length() > 7 && tokens[index1].substring(0,7).equals("SYSPRN(")){
            	dir_prn = tokens[index1].substring(7,tokens[index1].length()-1) + File.separator; 
            } else if (tokens[index1].equals("TRACE")){
            	opt_trace = true;
            	opt_list = true;
            } else if (tokens[index1].equals("TRACEALL")){
            	opt_traceall = true;
            	opt_trace = true;
            	opt_list = true;
            } else if (tokens[index1].equals("NOTRAP")){
            	opt_trap = false;
            }
            index1++;
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
	     if (opt_list){
	     	gen_sym_list();
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
             if (opt_trace){
             	put_log("TRACE SYMBOL UPDATE PASS " + cur_pass + " TOTAL ERRORS = " + az390_errors);
             }
         }
    	 az390_errors = 0;
    }
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
	              if (opt_trace){
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
	if (tot_esd > 0 && opt_list){
		put_prn_line("External Symbol Definitions");
	}
	cur_esd = 1;
	while (cur_esd <= tot_esd){
        if (sym_type[esd_sid[cur_esd]] != sym_dst
        	&& sym_sect_prev[esd_sid[cur_esd]] == 0){
    		String esd_code = 
    			" ESD=" + get_hex(sym_esd[esd_sid[cur_esd]],4)
    		  + " LOC=" + get_hex(sym_loc[esd_sid[cur_esd]],8)
    		  + " LEN=" + get_hex(get_sym_len(esd_sid[cur_esd]),8)
    		  + " TYPE=" + get_esd_type()
    		  + " NAME=" + sym_name[esd_sid[cur_esd]]
    		  ;
        	if (opt_list){	
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
	if (opt_trace || (gen_obj_code && opt_list)){
		list_bal_line = true;
	} else {
		list_bal_line = false;
	}
	loc_start = loc_ctr;
	bal_op_ok = false;
	switch (op_type[bal_op_index]){
	case 0:  // * comments
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
 	    put_obj_text();
    	break;
    case 3:  // "BRX" 16  BER oomr
    	bal_op_ok = true;
    	loc_ctr = (loc_ctr+1)/2*2;
    	loc_start = loc_ctr;
    	loc_len = 2;
    	get_hex_op(1,3);  // BCR OP includes mask
    	get_hex_reg();
 	    put_obj_text();
    	break;
    case 4:  // "I" 1 SVC 00ii
    	bal_op_ok = true;
    	loc_ctr = (loc_ctr+1)/2*2;
    	loc_start = loc_ctr;
    	loc_len = 2;
	    get_hex_op(1,2);
    	get_hex_byte();;
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
    	put_obj_text();
    	break;
    case 6:  // "BCX" 16 BE  oomxbddd
    	bal_op_ok = true;
    	loc_ctr = (loc_ctr+1)/2*2;
    	loc_start = loc_ctr;
    	loc_len = 4;
    	get_hex_op(1,3); // BCX op includes mask
    	get_hex_xbddd();
    	put_obj_text();
    	break;
    case 7:  // "S" 43 SPM oo00bddd
    	bal_op_ok = true;
    	loc_ctr = (loc_ctr+1)/2*2;
    	loc_start = loc_ctr;
    	loc_len = 4;
    	get_hex_op(1,4);
    	get_hex_bddd2();
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
    	get_hex_bddd2();
    	put_obj_text();
    	break;
    case 11:  // "SI" 9 CLI  ooiibddd
    	bal_op_ok = true;
    	loc_ctr = (loc_ctr+1)/2*2;
    	loc_start = loc_ctr;
    	loc_len = 4;
    	get_hex_op(1,2); 
    	get_hex_bddd2();
    	skip_comma();
    	get_hex_byte();
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
    	put_obj_text();
    	break;
    case 15:  // "RRF" 28 MAER oooor0rr 
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
    	get_hex_bddd2();
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
    	put_obj_text();
    	break;
    case 19:  // "SSE" 5  LASP  oooobdddbddd
    	bal_op_ok = true;
    	loc_ctr = (loc_ctr+1)/2*2;
    	loc_start = loc_ctr;
    	loc_len = 6;
    	get_hex_op(1,4); 
       	get_hex_bddd2();
       	skip_comma();
    	get_hex_bddd2();
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
    	put_obj_text();
    	break;
    case 28:   // LMD SS5  oorrbdddbddd  r1,r3,s2,s4
    	bal_op_ok = true;
    	loc_ctr = (loc_ctr+1)/2*2;
    	loc_start = loc_ctr;
    	loc_len = 6;
    	get_hex_op(1,2); 
    	get_hex_reg();
    	skip_comma();
    	get_hex_reg();
    	skip_comma();
       	get_hex_bddd2();
       	skip_comma();
    	get_hex_bddd2();
    	put_obj_text();
    	break;
    case 29:   // SRP SS2  oolibdddbddd s1(l1),s2,i3
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
   		hex_bddd2     = hex_bddd;
   		hex_bddd2_loc = hex_bddd_loc;
        skip_comma();
        hex_len2 = get_hex_nib();
    	obj_code = obj_code + hex_len1 + hex_len2 + hex_bddd1 + hex_bddd2;
    	put_obj_text();
    	break;
    case 30:  // "RRF" 30 DIER oooormrr (r3,m4,r1,r2 coded as r1,r3,r2,m4) 
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
    	put_obj_text();
    	break;
    case 101:  // CCW 0 
    case 102:  // CCW0 0 
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
    case 107:  // AMODE 0 
    case 108:  // CATTR 0 
    case 109:  // COM 0 
    	break;
    case 110:  // CSECT 0 
    	bal_op_ok = true;
    	process_sect(sym_cst);
    	if (first_cst_esd == 0)first_cst_esd = cur_esd;
    	break;
    case 111:  // CXD 0 
    	break;
    case 112:  // DSECT 0 
    	bal_op_ok = true;
    	process_sect(sym_dst);
    	break;
    case 113:  // DXD 0 
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
    	process_sect(sym_lct);
    	break;
    case 117:  // RMODE 0 
    case 118:  // RSECT 0 
    case 119:  // START 0 
    case 120:  // WXTRN 0 
    case 121:  // XATTR 0 
    case 123:  // DROP 0
    	bal_op_ok = true;
    	if (gen_obj_code){
            drop_using();
     	}
    	break;
    case 124:  // USING 0 
    	bal_op_ok = true;
    	if (gen_obj_code){
            add_using();
     	}
     	break;
    case 125:  // AEJECT 0 
    case 126:  // ASPACE 0 
    case 127:  // CEJECT 0 
    case 128:  // EJECT 0 
    case 129:  // PRINT 0 
    	break;
    case 130:  // SPACE 0 
    case 131:  // TITLE 0 
    	bal_op_ok = true;
    	break;
    case 132:  // ADATA 0 
    case 133:  // CNOP 0 
    	bal_op_ok = true;
    	process_cnop();
    	break;
    case 134:  // COPY 0 
    	bal_op_ok = true;  // already expanded in mz390
    	break;
    case 135:  // END 0 
    	bal_op_ok = true;
    	if (cur_esd > 0){
      		update_sect_len();
    	}
    	if (list_bal_line){
    		list_bal_line();
    	}
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
    case 138:  // ICTL 0 
    case 139:  // ISEQ 0
    	break;
    case 140:  // LTORG 0
    	bal_op_ok = true;
    	if (list_bal_line){
    		list_bal_line();
    	}
    	if (tot_lit > 0
    		&& cur_esd > 0
    		&& sym_type[esd_sid[cur_esd]] == sym_cst){
     	   gen_ltorg();
     	}
    	cur_lit_pool++;
    	break;
    case 141:  // OPSYN 0 
    case 142:  // ORG 0 
    	bal_op_ok = true;
    	process_org();
    	break;
    case 143:  // POP 0 
    	break;
    case 144:  // PUNCH 0
    	bal_op_ok = true; // pass thru after gen by mz390
    	break;
    case 145:  // PUSH 0 
    case 146:  // REPRO 0 
    case 147:  // ACTR 0 
    case 148:  // AGO 0 
    case 149:  // AIF 0 
    case 150:  // AINSERT 0 
    case 151:  // ANOP 0 
    case 152:  // AREAD 0 
    case 153:  // GBLA 0 
    case 154:  // GBLB 0 
    case 155:  // GBLC 0 
    case 156:  // LCLA 0 
    case 157:  // LCLB 0 
    case 158:  // LCLC 0 
    case 159:  // MHELP 0 
    	break;
    case 160:  // MNOTE 0
    	bal_op_ok = true;  // pass true from mz390
    	break;
    case 161:  // SETA 0 
    case 162:  // SETAF 0 
    case 163:  // SETB 0 
    case 164:  // SETC 0 
    case 165:  // SETCF 0 
    case 166:  // MACRO 0 
    case 167:  // MEND 0 
    case 168:  // MEXIT 0 
        break;
	}
	if (!bal_op_ok){
    	if (bal_line.length() > 0 && bal_line.charAt(0) != '*'){
     	   log_error(62,"undefined operation");
     	}
	}
	if (list_bal_line){
	   list_bal_line();
	}
	if (bal_label != null){
       update_label();
	}
	loc_ctr = loc_ctr + loc_len;
}
private void list_bal_line(){
	/*
	 * list bal line with first 8 bytes of
	 * object code if any and turn off list_bal_code
	 */
	   if (list_obj_code.length() < 16){
		   list_obj_code = list_obj_code.concat("                ");
	   } 
	   list_obj_code = list_obj_code.substring(0,16);
	   put_prn_line(get_hex(loc_start,6) + " " + list_obj_code + " " + hex_bddd1_loc + " " + hex_bddd2_loc + " " + bal_line); 
   	   list_bal_line = false;
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
	if (tot_rld > 0 && opt_list){
		put_prn_line("Relocation Definitions");
	}
	int index = 0;
	while (index < tot_rld){
		String rld_code = 
			" ESD=" + get_hex(rld_fld_esd[index],4)
		  + " LOC=" + get_hex(rld_fld_loc[index],8)
		  + " LEN=" + get_hex(rld_fld_len[index],1)
		  + " SIGN=" + rld_fld_sgn[index]
		  + " XESD=" + get_hex(rld_xrf_esd[index],4)
		  ;
		if (opt_list){	
            put_prn_line(rld_code);
		}
       	put_obj_line(".RLD" + rld_code);
		index++;
	}
}
private void gen_sym_list(){
	/*
	 * list symbols on prn listing file
	 */
	 put_prn_line("Symbol Table Listing");
	 int index = 1;
	 while (index <= tot_sym){
	 	String name = sym_name[index];
	 	if (name.length() < 8){
	 		name = name.concat("       ").substring(0,8);
	 	}
	 	put_prn_line(" SYM=" + name 
	 			   + " TYPE=" + sym_type_desc[sym_type[index]] 
				   + " ESD=" + get_hex(sym_esd[index],4) 
				   + " LOC=" + get_hex(sym_loc[index],8) 
				   + " LEN=" + get_hex(get_sym_len(index),8));
	    index++;
	 }
}
private void load_bal(){
	/*
	 * load bal source
	 * 
	 * 1.  Concatentate any continuations indicated
	 *     by non-blank in position 72.  Each 
	 *     continuation must start at position 16.
	 */
        bal_file = new File(dir_bal + pgm_name + ".BAL");
   	    try {
   	        bal_file_buff = new BufferedReader(new FileReader(bal_file));
   	    } catch (IOException e){
   		    abort_error(6,"I/O error on bal open - " + e.toString());
   	    }
        if (opt_trace){
      	  	 put_log("TRACE LOADING " + dir_bal + pgm_name + ".BAL");
        }
		get_bal_line();
		while (!bal_eof && bal_line != null
				&& tot_bal_line < max_bal_line){
			bal_line_index = tot_bal_line;
			bal_name_line[tot_bal_line] = bal_line;
			bal_name_line_num[tot_bal_line] = cur_line_num;
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
        if (opt_trace){
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
   				   || temp_line.charAt(71) == ' '){
   			bal_line = temp_line;
   		} else {
   		    bal_line = temp_line.substring(0,71);
   		    bal_line = trim_continue(bal_line);
            while (temp_line.length() > 71
            		&& temp_line.charAt(71) != ' '){
            	    temp_line = bal_file_buff.readLine();
            	    cur_line_num++;
            	    if  (temp_line.length() >= 16){
            	    	int temp_end = temp_line.length();
            	    	if (temp_end > 71)temp_end = 71;
            	    	bal_line = bal_line.concat(temp_line.substring(15,temp_end));
               		    bal_line = trim_continue(bal_line);
            	    } else { 
            	    	abort_error(8,"continuation line < 16 characters - " + temp_line);
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
	if (opt_trace){
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
    bal_op    = tokens[1].toUpperCase();
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
       if  (line.charAt(0) != ' '){
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
		index = find_key_index("O:" + bal_op);
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
	 * process EXTRN or ENTRY statement
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
	       case ' ':
	           extrn_eod = true;
	           break;
	       default:
	    	   switch (esd_type){
	    	   case 4: // sym_ext
	    	   	   cur_sid = find_sym(token);
	               if (cur_sid == -1){
	        	      add_extrn(token);
	        	   }
	               break;
	    	   case 3: // sym_ent
	    	   	   cur_sid = find_sym(token);
	               if (cur_sid != -1){
	        	      add_entry(token);
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
	   if (tot_sym < max_sym - 1){
		   tot_sym++;
		   cur_sid = tot_sym;
		   add_key_index(cur_sid);
		   sym_name[cur_sid] = token;
		   sym_type[cur_sid] = sym_ext;
		   if (tot_esd < max_esd){
			   tot_esd++;
			   esd_sid[tot_esd] = cur_sid;
			   sym_esd[cur_sid] = tot_esd;
		   } else {
			   log_error(96,"maximum esds exceeded");
			   cur_sid = -1;
		   }
       } else {
		   log_error(96,"maximum esds exceeded");
		   cur_sid = -1;
       }
}
private void add_entry(String token){
	/*
	 * add ENTRY 
	 */
	   if (sym_type[cur_sid] == sym_rel){
           int index = 1;
           while (index <= tot_esd){
        	   if (esd_sid[index] == cur_sid){
        		   return;
        	   }
        	   index++;
           }
		   if (tot_esd < max_esd){
			   tot_esd++;
			   esd_sid[tot_esd] = cur_sid;
		   } else {
			   log_error(96,"maximum esds exceeded");
			   cur_sid = -1;
		   }
       } else {
		   log_error(97,"invalid entry type symbol - " + sym_name[cur_sid]);
		   cur_sid = -1;
       }
}
private void process_sect(byte sect_type){
	/*
	 * add or update csect, dsect, or loctr
	 * indicated by sym_cst, sym_dst, or sym_lct type parm
	 * Steps:
	 *   1.  Update previous section if any with 
	 *       max length and any loctr pointers
	 *   2.  Add new section if not found
	 *   3.  Reset location counter to end of 
	 *       current section.
	 *   4.  Update prev section type and sid for
	 *       use in processing sym_lct sections.
	 */
	if (cur_esd_sid > 0){
		update_sect_len();
	}
	cur_esd_sid = find_sym(bal_label);
	if (cur_esd_sid < 1){
	   if (sect_type != sym_lct){
		   loc_ctr = (loc_ctr + 7)/8*8;
	   }
	   if (tot_esd < max_esd - 1
	   		&& tot_sym < max_sym - 1){
	   	  tot_esd++;
	   	  cur_esd = tot_esd;
	   	  tot_sym++;
	   	  add_key_index(tot_sym);
		  cur_esd_sid = tot_sym;
	   	  sym_name[cur_esd_sid] = bal_label;
	   	  sym_def[cur_esd_sid]  = bal_line_index;
	   	  sym_type[cur_esd_sid] = sect_type;
	   	  sym_esd[cur_esd_sid]  = tot_esd;
	   	  sym_loc[cur_esd_sid] = loc_ctr;
	   	  sym_len[cur_esd_sid] = 0;
	   	  esd_sid[tot_esd] = cur_esd_sid;
	   	  if (sect_type == sym_lct){
	   	  	 if (prev_sect_type != 0){
	   	  	     sym_sect_prev[cur_esd_sid] = prev_sect_sid;
	   	  	     sym_sect_next[prev_sect_sid] = cur_esd_sid;
	   	  	     sym_type[cur_esd_sid]      = prev_sect_type;
	   	  	     sym_esd[cur_esd_sid] = prev_sect_esd;
	   	  	 } else {
	   	  	 	log_error(90,"LOCTR must follow CSECT or DSECT");
	   	  	    sym_type[cur_esd_sid]      = sym_cst;
	   	  	 }
	   	  } else {
		   	 sym_type[cur_esd_sid] = sect_type;
	   	  }
	   } else {
	   	  abort_error(10,"ESD table size exceeded");
	   }
	} else {
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
	if (opt_time
		&& (tot_sym_find > next_time_check)){
		next_time_check = tot_sym_find + next_time_ins;
		cur_date = new Date();
		tod_end = cur_date.getTime();
	    if (tod_end > tod_time_limit){
           abort_error(80,"time limit exceeded");
		}
	}
	int index = find_key_index("S:" + name);
	if (index != -1 || !dcv_type){
		return index;
    }
    add_extrn(name);
    return cur_sid;
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
	   if (tot_sym < max_sym - 1){
	   	  tot_sym++;
	   	  add_key_index(tot_sym);
	   	  cur_sid = tot_sym;
	   	  sym_name[cur_sid] = bal_label;
	   	  sym_def[cur_sid]  = bal_line_index;
	   	  sym_type[cur_sid] = sym_rel;
	   	  sym_esd[cur_sid]  = cur_esd;
	   	  sym_loc[cur_sid] = loc_start;
	   	  if (loc_len == 0){
	   	  	 sym_len[cur_sid] = dc_first_len;
	   	  } else {
	   	     sym_len[cur_sid] = loc_len;
	   	  }
	   } else {
	   	  abort_error(30,"Symbol table size exceeded");
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
			&& sym_type[cur_sid] != sym_dst) {
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
	       } else if (dc_field.charAt(dc_index) <= ' '){
	       	  dc_index = dc_field.length();  // flush trailing comments
	       }
	       get_dc_field_dup();
	       get_dc_field_type();
	       get_dc_field_len(); // align and set length
	       if  (dc_index < dc_field.length() 
	       		&& dc_field.charAt(dc_index) != ','
	       	    && dc_field.charAt(dc_index) != ' '){
	       	   if (bal_abort || dc_field.charAt(dc_index) 
	       	  		!= dc_type_delimiter[dc_type_index]){
	       	  	  log_error(45,"invalid dc delimiter for type - " + dc_field.substring(0,dc_index+1));
	       	  	  return;
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
  	  	       		 dcv_type = true;
   	  	       	  	 process_dca_data();
   	  	       	  	 dcv_type = false;
   	  	       	  	 break;	 
   	  	       	  case 'X': // 'int,int'
  	  	       	  	 process_dcx_data();
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
	       		|| dc_field.charAt(dc_index) <= ' '
	       		|| dc_field.charAt(dc_index) == dc_type_delimiter[dc_type_index]){ 
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
	   if (exp_index > exp_text.length()){
	   	  return false;
	   }
       exp_match = exp_pattern.matcher(exp_text.substring(exp_index));
       exp_state = 1;
       exp_term = false;
       exp_first_len = false;
       exp_len = 1;
       tot_exp_stk_sym = 0;
       tot_exp_stk_op  = 0;
       tot_exp_rld_add = 0;
       tot_exp_rld_sub = 0;
       exp_sym_last = false;
   	   exp_level = 0;
   	   exp_op = " ";
	   while (!exp_term && !bal_abort){
	   	   if (!exp_op.equals(exp_term_op) && exp_match.find()){
	          exp_token = exp_match.group();
	          exp_index = exp_index + exp_token.length();
	   	   } else {
	   	   	  exp_token = "" + exp_term_op;
	   	   }
		   proc_exp_token();
	   }
	   if (!bal_abort){
   	      exp_index--;  // backup to terminator
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
	        	proc_exp_op();
	            break;
	        case '-':
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
	        case ' ':
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
	        	if (exp_token.length() > 1 && exp_token.charAt(1) == '\''){
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
	        case 'X':
	        	if (exp_token.length() > 1 && exp_token.charAt(1) == '\''){
	        	   proc_exp_sdt();
	        	} else {
	        	   proc_exp_sym();
	        	}
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
	if (tot_exp_stk_sym >= max_exp_stk){
		log_error(22,"maximum stack variables exceeded");
	} else {
	   if (cur_esd > 0){ 
          exp_stk_sym_esd[tot_exp_stk_sym] = cur_esd;
          if (dc_lit_ref || dc_lit_gen){
          	 lit_loc_ref = true;
          	 exp_stk_sym_val[tot_exp_stk_sym] = lit_line_loc[cur_lit];
          } else {
             exp_stk_sym_val[tot_exp_stk_sym] = loc_ctr;
          }
   	      tot_exp_stk_sym++;
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
    case 5: //  L' operators
         exp_sym_len_op();
         break;
    case 6: // terminator space, comma, unmateced )
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
	  if (tot_exp_stk_op >= max_exp_stk){
		  log_error(19,"maximum stack operations exceeded");
	  }
      if (!exp_sym_last){
	     if  (exp_op.equals("-")){  // unary minus
		     push_exp_sdt("0");
		     exp_token = "-";
	     } else if (exp_token.equals("+")){
		     return; // unary plus ignored
	     }
	  }
      exp_stk_op[tot_exp_stk_op] = exp_token;
      tot_exp_stk_op++;
      if (tot_exp_stk_op > max_exp_stk){
      	 log_error(20,"stack size exceeded");
      }
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
	if (tot_exp_stk_sym >= max_exp_stk){
		log_error(22,"maximum stack variables exceeded");
	} else {
	   cur_sid = find_sym(exp_token);
	   if (cur_sid > 0){ 
	   	  if (exp_first_len){
	   	  	 exp_first_len = false;
	   	  	 exp_len = sym_len[cur_sid];
	   	  }
          exp_stk_sym_esd[tot_exp_stk_sym]  = sym_esd[cur_sid];
          exp_stk_sym_val[tot_exp_stk_sym]  = sym_loc[cur_sid];
   	      tot_exp_stk_sym++;
	   } else {
	   	  log_error(98,"symbol not found - " + exp_token);
	   }
    }
}
private void push_exp_sdt(String sdt){
	/*
	 * push self defining abs term on stack
	 */
	   if (tot_exp_stk_sym >= max_exp_stk){
		   log_error(23,"maximum stack variables exceeded");
	   } else {
           exp_stk_sym_esd[tot_exp_stk_sym] = sym_sdt;
           switch (sdt.charAt(0)){
           case 'B': // B'11000001' binary
        	   exp_stk_sym_val[tot_exp_stk_sym] = Integer.valueOf(sdt.substring(2,sdt.length()-1),2).intValue();
           	   break;
           case 'C': // C'A' EBCDIC character
           	   int index = 2;
           	   int sdt_int = 0;
           	   while (index < sdt.length()-1){
           	   	   sdt_int = (sdt_int << 8) + (ascii_to_ebcdic[sdt.charAt(index)] & 0xff);
           	       index++;
           	   }
           	   exp_stk_sym_val[tot_exp_stk_sym] = sdt_int; 
           	   break;
           case 'X': // X'C1' hex
           	   exp_stk_sym_val[tot_exp_stk_sym] = Long.valueOf(sdt.substring(2,sdt.length()-1),16).intValue();
           	   break;
           default:
               exp_stk_sym_val[tot_exp_stk_sym] = Integer.valueOf(sdt).intValue();
               break;
           }
	       tot_exp_stk_sym++;
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
	  if    (az390_aborted){
	    	System.exit(az390_rc);
	  }
}
private void put_stats(){
	/*
	 * display statistics as comments at end of bal
	 */
	if (opt_stats || az390_errors > 0){
	   put_log("Stats BAL lines       = " + tot_bal_line);
	   put_log("Stats symbols         = " + tot_sym);
	   put_log("Stats Literals        = " + tot_lit);
	   put_log("Stats Keys            = " + tot_key);
	   put_log("Stats Key searches    = " + tot_key_search);
	   if (tot_key_search > 0){
	       avg_key_comp = tot_key_comp/tot_key_search;
	   }
	   put_log("Stats Key avg comps   = " + avg_key_comp);
	   put_log("Stats Key max comps   = " + max_key_comp);
	   put_log("Stats ESD symbols     = " + tot_esd);
	   put_log("Stats object bytes    = " + tot_obj_bytes);
	   put_log("Stats object rlds     = " + tot_rld);
	   if (opt_timing){
	      cur_date = new Date();
	      tod_end = cur_date.getTime();
	      tot_sec = (tod_end - tod_start)/1000;
	      put_log("Stats total seconds         = " + tot_sec);
	   }
	}
	put_log("AZ390 total errors          = " + az390_errors);
	put_log("AZ390 return code           = " + az390_rc);
}
private void close_files(){
	  if (obj_file != null && obj_file.isFile()){
	  	  try {
	  	  	  obj_file_buff.close();
	  	  } catch (IOException e){
	  	  	  abort_error(24,"I/O error on obj close - " + e.toString());
	  	  }
	  }
	  if  (opt_list){
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
	  if (gen_obj_code || opt_trace){
    	 if (list_bal_line){
    		list_bal_line();
    	 }
	     put_log("az390 error " + error + " line " + bal_name_line_num[bal_line_index] + "   " + bal_name_line[bal_line_index]);
	     put_log("az390 error " + error + " " + msg);
	  }
	  az390_errors++;
	  if (gen_obj_code && az390_errors > max_errors){
	  	 abort_error(49,"max errors exceeded");	 
	  }
}
private void abort_error(int error,String msg){
	/*
	 * issue error msg to log with prefix and
	 * inc error total
	 */
	  az390_errors++;
	  if (az390_aborted){
	  	 System.exit(16);
	  }
	  az390_aborted = true;
  	  if (list_bal_line){
		list_bal_line();
	  }
	  put_log("az390 error " + error + " on line " + bal_name_line_num[bal_line_index] + " " + bal_name_line[bal_line_index]);
	  put_log("az390 error " + error + " " + msg);
      exit_az390();
}
private void put_copyright(){
	   /*
	    * display az390 version, timestamp,
	    * and copyright if running standalone
	    */
	   	if  (opt_timing){
			cur_date = new Date();
	   	    put_log("az390 " + version 
	   			+ " Current Date " +mmddyy.format(cur_date)
	   			+ " Time " + mmddyy.format(cur_date));
	   	} else {
	   	    put_log("az390 " + version);
	   	}
	   	if  (z390_log_text == null){
	   	    put_log("Copyright 2005 Automated Software Tools Corporation");
	   	    put_log("z390 is licensed under GNU General Public License");
	   	}
	   	put_log("AZ390 program = " + dir_bal + pgm_name + ".BAL");
	   	put_log("AZ390 options = " + opt_parms);
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
   	        	if (opt_con){
   	    	        System.out.println(msg);
   	        	}
   	        }
	   }
	   private void put_prn_line(String msg){
	   /*
	    * put line to listing file
	    */
	   	   if (opt_list || opt_trace){
	   	      try {
	   	          prn_file_buff.write(msg + "\r\n");
	   	      } catch (Exception e){
	   	          abort_error(28,"I/O error on PRN listing file write");
	   	      }
	   	   }
	   }
	   private void put_obj_line(String msg){
		   /*
		    * put object code to obj file
		    */
		   	try {
		   	   obj_file_buff.write(msg + "\r\n");
		   	} catch (Exception e){
		   	    abort_error(28,"I/O error on OBJ file write");
		   	}
		   }
	   private String get_long_hex(long work_long) {
	   	/*
	   	 * Format long into 16 byte hex string
	   	 */
	   	    String work_hex = Long.toHexString(work_long);
			return ("0000000000000000" + work_hex).substring(work_hex.length()).toUpperCase();
	   }
	   private String get_hex(int work_int,int field_length) {
	   	/*
	   	 * Format int into 8 byte hex string
	   	 */
	   	    StringBuffer hex = new StringBuffer(8);
	   	    String work_hex = Integer.toHexString(work_int);
	   	    if (work_hex.length() <= field_length){
                int i;
	   	        for (i=0;i<field_length-work_hex.length();i++){
	   	    	    hex.append("0");
	   	        }
	   	    } else {
	   	    	int i = work_hex.length()-field_length;
	   	        work_hex = work_hex.substring(i,i+field_length);
	   	    }
	   	    hex.append(work_hex);
			return hex.toString().toUpperCase();
	   }
	   private String string_to_hex(String text){
	   	/*
	   	 * Format text string into hex string
	   	 */
            StringBuffer hex = new StringBuffer(2 * text.length());
            int index = 0;
            while (index < text.length()){
            	int work_int = ascii_to_ebcdic[text.charAt(index)] & 0xff;
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
	 if (!gen_obj_code){
	 	return;
	 }
	 if (list_obj_code.length() < 16){
	    list_obj_code = list_obj_code.concat(obj_code);
	 }
	 int obj_code_len = obj_code.length()/2;
	 tot_obj_bytes = tot_obj_bytes + obj_code_len;
	 if (cur_text_len > 0
	 	&& (bal_eof 
	 		|| cur_text_esd != sym_esd[esd_sid[cur_esd]] 
		 	|| cur_text_loc != loc_ctr)){
		cur_text_loc = cur_text_loc - cur_text_len;
		put_obj_line(".TXT ESD=" + get_hex(cur_text_esd,4) + " LOC=" + get_hex(cur_text_loc - sym_loc[esd_sid[cur_text_esd]],8) + " LEN=" + get_hex(cur_text_len,2) + " " + cur_text_buff);
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
	 	put_obj_line(".TXT ESD=" + get_hex(cur_text_esd,4) 
	 			   + " LOC=" + get_hex(cur_text_loc - sym_loc[esd_sid[cur_text_esd]],8) 
	 			   + " LEN=" + get_hex(max_text_buff_len,2) 
	 			   + " " + cur_text_buff.substring(0,2*max_text_buff_len));
		cur_text_loc = cur_text_loc + cur_text_len;
	 	cur_text_buff = cur_text_buff.substring(2*max_text_buff_len);
	 	cur_text_len = cur_text_buff.length()/2;
	}
	obj_code = "";
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
				hex_bddd = get_rel_exp_bddd();
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
	if  (cur_drop_parms == null || cur_drop_parms.length() == 0){
		if (opt_trace){
			int index = 0;
			while (index < tot_use){
				trace_use("DROP",index);
				index++;
			}
		}
		tot_use = 0;  // drop all using
	}
	while (cur_drop_parms.length() > 0){
		int next_comma = cur_drop_parms.indexOf(",");
    	if (next_comma != -1){
			cur_use_lab = cur_drop_parms.substring(0,next_comma);
			cur_drop_parms = cur_drop_parms.substring(next_comma+1);
		} else {
			cur_use_lab = cur_drop_parms;
			cur_drop_parms = "";
		}
		if (find_key_index("U:" + cur_use_lab) != -1){
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
	int index = 0;
	while (index < tot_use){
		if (use_lab[index] != null && use_lab[index].equals(cur_use_lab)){
			if (opt_trace){
				trace_use("DROP",index);
			}
			tot_use--;
			if (index < tot_use){
				move_last_use(index);
			}
		}
		index++;
	}
	
}
private void drop_cur_use_range(){
	/*
	 * remove unlabeld using range if found
	 */
	int index = 0;
	while (index < tot_use){
		if (use_lab[index].length() == 0
				&& use_base_esd[index] == cur_use_base_esd
				&& use_base_loc[index] == cur_use_base_loc){
			if (opt_trace){
				trace_use("DROP",index);
			}
			tot_use--;
			if (index < tot_use){
				move_last_use(index);
			}
		}
		index++;
	}
	
}
private void drop_cur_use_reg(){
	/*
	 * remove using reg entries if found
	 */
	int index = 0;
	while (index < tot_use){
		if (use_reg[index] == cur_use_reg){
			if (opt_trace){
				trace_use("DROP",index);
			}
			tot_use--;
			if (index < tot_use){
				move_last_use(index);
			}
		}
		index++;
	}
}
private void move_last_use(int index){
	/*
	 * move last use entry for deletes
	 */
	use_lab[index] = use_lab[tot_use];
	use_base_esd[index] = use_base_esd[tot_use];
	use_base_loc[index] = use_base_loc[tot_use];
	use_base_len[index] = use_base_len[tot_use];
	use_reg[index] = use_reg[tot_use];
	use_reg_loc[index] = use_reg_loc[tot_use];
}
private void add_use_entry(){
	/*
	 * add use entry
	 */
	if (tot_use < max_use){
		cur_use = tot_use;
		tot_use++;
		use_lab[cur_use] = cur_use_lab;
		if (cur_use_lab.length() > 0 
			&& find_key_index("U:" + cur_use_lab) == -1){
			add_key_index(0); // create key to indicate using label
		}
		use_base_esd[cur_use] = cur_use_base_esd;
		use_base_loc[cur_use] = cur_use_base_loc;
		use_base_len[cur_use] = cur_use_base_len;
		use_reg[cur_use] = cur_use_reg;
		use_reg_loc[cur_use] = cur_use_reg_loc;
		if (opt_trace){
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
			+ " ESD=" + get_hex(use_base_esd[index],4)
			+ " LOC=" + get_hex(use_base_loc[index],8)
			+ " LEN=" + get_hex(use_base_len[index],4)	
			+ " REG=" + get_hex(use_reg[index],2)
			+ " LOC=" + get_hex(use_reg_loc[index],4)
			+ " LAB=" + use_lab[index]
			);
}
private void get_hex_op(int op_offset, int op_len){
	/*
	 * initialize object code with op code
	 * and initialize exp parser with parms
	 * if op_offset = 1
	 */
	hex_op = op_code[bal_op_index].substring(op_offset-1,op_offset - 1 + op_len);
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
		    return get_hex(exp_val,1);
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
		    obj_code = obj_code + get_hex(exp_val,1);
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
		obj_code = obj_code + get_hex(exp_val,2);
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
		hex_bddd_loc = get_hex(exp_val,6);
		exp_val = dc_first_len;
		if (exp_val >= 0 && exp_val <= 0x100){
			if (exp_val > 0)exp_val = exp_val - 1;
			hex_len = get_hex(exp_val,2);
		} else {
			log_error(64,"invalid length - " + exp_val);
		}
	} else if (calc_exp()){
		if  (exp_type == sym_rel){
			hex_bddd = get_rel_exp_bddd();
			hex_bddd_loc = get_hex(exp_val,6);
			if  (exp_text.charAt(exp_index) == '('){
				exp_index++;
				if (calc_abs_exp() && exp_val >= 0 && exp_val <= 0x100){
					if (exp_val > 0)exp_val = exp_val - 1;
					hex_len = get_hex(exp_val,2);
					exp_index++;
				} else {
					log_error(64,"invalid length - " + exp_val);
				}
			} else {
				exp_val = sym_len[cur_sid];
				if (exp_val >= 0 && exp_val <= 0x100){
					if (exp_val > 0)exp_val = exp_val - 1;
					hex_len = get_hex(exp_val,2);
				} else {
					log_error(64,"invalid length - " + exp_val);
				}
			}
		} else {
			if (exp_val >= 0 && exp_val <= 0x1000){
			   hex_ddd = get_hex(exp_val,3);
			} else {
				log_error(56,"invalid displacement - " + exp_val);
			}
			if  (exp_text.length() > exp_index && exp_text.charAt(exp_index) == '('){
				exp_index++;
				if (calc_abs_exp() && exp_val >= 0 && exp_val <= 0x100){
					if (exp_val > 0)exp_val = exp_val -1;
					hex_len = get_hex(exp_val,2);
					if (exp_text.charAt(exp_index) == ','){
						exp_index++;
						if (calc_abs_exp() && exp_val >= 0 && exp_val <= 15){
							hex_b = get_hex(exp_val,1);
						} else {
							log_error(38,"invalid index register expression");
						}
					}
					exp_index++;
				} else {
					log_error(64,"invalid length expression");
				}
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
		hex_bddd2_loc = get_hex(exp_val,6);
		hex_x = "0";
	} else if (calc_exp()){
		if  (exp_type == sym_rel){
			hex_bddd2 = get_rel_exp_bddd();
			hex_bddd2_loc = get_hex(exp_val,6);
			if  (exp_text.length() > exp_index && exp_text.charAt(exp_index) == '('){
				exp_index++;
				if (calc_abs_exp() && exp_val >= 0 && exp_val <= 15){
					hex_x = get_hex(exp_val,1);
					exp_index++;
				} else {
					log_error(40,"invalid index register");
				}
			} else {
				hex_x = "0";
			}
		} else {
			if (exp_val >= 0 && exp_val < 0x1000){
			   hex_ddd = get_hex(exp_val,3);
			} else {
				log_error(56,"invalid displacement - " + exp_val);
			}
			if  (exp_text.length() > exp_index && exp_text.charAt(exp_index) == '('){
				exp_index++;
				if (calc_abs_exp() 
						&& exp_val >= 0 && exp_val <= 15){
					hex_x = get_hex(exp_val,1);
					if (exp_text.length() > exp_index && exp_text.charAt(exp_index) == ','){
						exp_index++;
						if (calc_abs_exp() 
								&& exp_text.length() > exp_index 
								&& exp_text.charAt(exp_index) == ')'
								&& exp_val >= 0 && exp_val <= 15){
							exp_index++;
							hex_b = get_hex(exp_val,1);
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
				} else {
					log_error(39,"invalid index expression");
				}
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
	String hex_x   = "x";
	String hex_b   = "b";
	String hex_dddhh = "dddhh";
	String hex_bdddhh2 = "bdddhh";
	if (exp_text.length() > exp_index && exp_text.charAt(exp_index) == '='){
		hex_bdddhh2 = get_lit_bddd() + "00";
		hex_bddd2_loc = get_hex(exp_val,6);
		hex_x = "0";
	} else if (calc_exp()){
		if  (exp_type == sym_rel){
			hex_bdddhh2 = get_rel_exp_bddd() + "00";
			hex_bddd2_loc = get_hex(exp_val,6);
			if  (exp_text.length() > exp_index && exp_text.charAt(exp_index) == '('){
				exp_index++;
				if (calc_abs_exp() 
					&& exp_text.length() > exp_index 
					&& exp_text.charAt(exp_index) == ')' 
					&& exp_val >= 0 && exp_val <= 15){
					exp_index++;
					hex_x = get_hex(exp_val,1);
				} else {
					log_error(40,"invalid index register");
				}
			} else {
				hex_x = "0";
			}
		} else {
			if (exp_val >= 0 && exp_val < 0x1000){
			   hex_b     = "0";
			   hex_dddhh = get_hex(exp_val,3) + "00";
			} else {
				log_error(56,"invalid displacement - " + exp_val);
			}
			if  (exp_text.length() > exp_index && exp_text.charAt(exp_index) == '('){
				exp_index++;
				if (calc_abs_exp() && exp_val >= 0 && exp_val <= 15){
					hex_x = get_hex(exp_val,1);
					if (exp_text.charAt(exp_index) == ','){
						exp_index++;
						if (calc_abs_exp()
							&& exp_text.length() > exp_index 
							&& exp_text.charAt(exp_index) == ')'
							&& exp_val >= 0 && exp_val <= 15){
							hex_b = get_hex(exp_val,1);
						} else {
							log_error(38,"invalid base register expression");
						}
					} else {
						if (exp_text.length() > exp_index && exp_text.charAt(exp_index) == ')'){
							exp_index++;
						} else {
							log_error(38,"invalid base register expression");
						}
					}
				} else {
					log_error(39,"invalid index expression");
				}
			} else {
				hex_x = "0";
				hex_b = "0";
			}
			hex_bdddhh2 = hex_b + hex_dddhh;
		}
	}
	obj_code = obj_code + hex_x + hex_bdddhh2;
}
private void get_hex_bddd2(){
	/*
	 * append bddd hex object code from next parm
	 */
	String hex_b   = "b";
	String hex_ddd = "ddd";
	hex_bddd2      = "bddd";
	if (exp_text.length() > exp_index && exp_text.charAt(exp_index) == '='){
		hex_bddd2 = get_lit_bddd();
		hex_bddd2_loc = get_hex(exp_val,6);
	} else if (calc_exp()){
		if  (exp_type == sym_rel){
			hex_bddd2 = get_rel_exp_bddd();
			hex_bddd2_loc = get_hex(exp_val,6);
		} else {
			if (exp_val >= 0 && exp_val < 0x1000){
			   hex_b   = "0";
			   hex_ddd = get_hex(exp_val,3);
			} else {
				log_error(56,"invalid displacement - " + exp_val);
			}
			if  (exp_text.length() > exp_index && exp_text.charAt(exp_index) == '('){
				exp_index++;
				if (calc_abs_exp() 
						&& exp_text.length() > exp_index 
						&& exp_text.charAt(exp_index) == ')' 
						&& exp_val >= 0 && exp_val <= 15){
					exp_index++;
					hex_b = get_hex(exp_val,1);
				} else {
					log_error(39,"invalid base expression");
				}
			} else {
				hex_b = "0";
			}
			hex_bddd2 = hex_b + hex_ddd;
		}
	}
	obj_code = obj_code + hex_bddd2;
}
private void get_hex_bdddhh2(){
	/*
	 * append bdddhh hex object code from next parm
	 */
	String hex_b   = "b";
	String hex_dddhh = "dddhh";
	String hex_bdddhh2 = "bdddhh";
	if (exp_text.length() > exp_index && exp_text.charAt(exp_index) == '='){
		hex_bdddhh2 = get_lit_bddd() + "00";
		hex_bddd2_loc = get_hex(exp_val,6);
	} else if (calc_exp()){
		if  (exp_type == sym_rel){
			hex_bdddhh2 = get_rel_exp_bddd() + "00";
			hex_bddd2_loc = get_hex(exp_val,6);
		} else {
			if (exp_val >= 0 && exp_val < 0x1000){
			   hex_b     = "0";
			   hex_dddhh = get_hex(exp_val,3) + "00";
			} else {
				log_error(56,"invalid displacement - " + exp_val);
			}
			if  (exp_text.length() > exp_index && exp_text.charAt(exp_index-1) == '('){
				exp_index++;
				if (calc_abs_exp() 
					&& exp_text.length() > exp_index 
					&& exp_text.charAt(exp_index) == ')' 
					&& exp_val >= 0 && exp_val <= 15){
					exp_index++;
					hex_b = get_hex(exp_val,1);
				} else {
					log_error(39,"invalid base expression");
				}
			}
			hex_bdddhh2 = hex_b + hex_dddhh;
		}
	}
	obj_code = obj_code + hex_bdddhh2;
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
			   hex_iiii = get_hex(exp_val,4);
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
		    hex_llllllll = get_hex(exp_val,8);
		}
	}
	obj_code = obj_code + hex_llllllll;
}
private String get_rel_exp_iiii(){
	/*
	 * return relative signed offset from psw_loc
	 * to symbol in same csect
	 */
	String hex_iiii = "iiii";
	if (exp_esd == cur_esd){
		exp_val = (exp_val - loc_start)/2;
		if (exp_val >= -0x8000 && exp_val <= 0x7fff){
		    hex_iiii = get_hex(exp_val,4);
		} else {
			log_error(74,"relative offset too large - " + exp_val);
		}
	} else {
		log_error(75,"relative offset not in same esd");
	}
	return hex_iiii;
}
private String get_rel_exp_llllllll(){
	/*
	 * return relative signed offset from psw_loc
	 * to symbol in same csect
	 */
	String hex_llllllll= "llllllll";
	if (exp_esd == cur_esd){
		exp_val = (exp_val - loc_start)/2;
	    hex_llllllll = get_hex(exp_val,8);
	} else {
		log_error(76,"relative offset not in same esd");
	}
	return hex_llllllll;
}
private String get_rel_exp_bddd(){
	/*
	 * 1.  Return hex bddd for relative expression
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
	}
	cur_use_reg = -1;  // assume not found
	cur_use_reg_loc = 4096;
	int index = 0;
	int test_offset = 0;
	while (index < tot_use){
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
		return get_hex(cur_use_reg,1) + get_hex(cur_use_reg_loc,3);
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
      dc_type_index = dc_valid_types.indexOf(dc_type);
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
    dc_len = dc_type_len[dc_type_index];
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
	exp_index = dc_index;
	dc_data_start = dc_index; 
	if (!dc_lit_ref && (dc_len == 3 || dc_len == 4)){
		exp_rld_len = (byte) dc_len;
	} else {
		exp_rld_len = 0;
	}
	while (!dc_eod && !bal_abort){
		while (!dc_eod && !bal_abort 
				&& dc_field.charAt(dc_index) != ')'){
		    if (dc_field.charAt(dc_index) == ','){
		    	exp_index++;
		    }
		    if  (calc_exp()){
			    dc_index = exp_index;
			    if (gen_obj_code
			    	&& !dc_lit_ref
                    && sym_type[cur_esd_sid] == sym_cst
					&& dc_dup > 0
					&& dc_op){
			    	if (dc_len <= 4 || exp_val >= 0){
				        obj_code = obj_code + get_hex(exp_val,2*dc_len);
			    	} else {
				        obj_code = obj_code + ("FFFFFFFF").substring(0,2*dc_len-8) + get_hex(exp_val,8);
			    	}
					put_obj_text();
			    } 
			    if (!dc_lit_ref && dc_dup > 0){
				   loc_ctr = loc_ctr + dc_len;
			    }
		    }
		}
	    dc_index++; // skip dca terminator
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
			String dcb_hex = Integer.toHexString(Integer.valueOf(dcb_bin.substring(index,index+8),2).intValue());
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
		} else {
	        dc_index++; // skip dch terminator
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
	exp_rld_len = 0;
}
private void process_dcc_data(){
	/*
	 * allocate or generate dc Cln'...' data
	 * using dc_dup and explicit dc_len if any
	 */
	String dcc_text = "";
	String token = null;
	int dcc_len  = 0;
	int dcc_next = 0;
    dcc_match = dcc_pattern.matcher(dc_field.substring(dc_index + 1));
	while (!dc_eod && !bal_abort
			&& dcc_match.find()){
	       token = dcc_match.group();
	       dcc_next = dcc_match.end();
	       if (token.charAt(0) != '\''){
	       	  dcc_text = dcc_text + token;
	       } else if (token.length() == 2){
	       	  dcc_text = dcc_text + "'";
	       } else {
	       	  dc_eod = true;
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
 	         obj_code = obj_code + string_to_hex(dcc_text);
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
	dc_index++;   // start inside (,,,)
	exp_index = dc_index;
	dc_data_start = dc_index; 
	exp_rld_len = 0;
	while (!dc_eod && !bal_abort){
		while (!dc_eod && !bal_abort 
				&& dc_field.charAt(dc_index) != '\''){
		    if  (calc_abs_exp()){
			    dc_index = exp_index;
			    if (gen_obj_code
			    	&& !dc_lit_ref
                    && sym_type[cur_esd_sid] == sym_cst
					&& dc_dup > 0
					&& dc_op){
			    	if (dc_len <= 4 || exp_val >= 0){
				        obj_code = obj_code + get_hex(exp_val,2*dc_len);
			    	} else {
				        obj_code = obj_code + ("FFFFFFFF").substring(0,2*dc_len-8) + get_hex(exp_val,8);
			    	}
					put_obj_text();
			    }
			    if (!dc_lit_ref && dc_dup > 0){
				   loc_ctr = loc_ctr + dc_len;
			    }
			    if (dc_field.charAt(dc_index) == ','){
			    	exp_index++;
			    } else if (dc_field.charAt(dc_index) == '\''){
			    	dc_eod = true;			    	
			    } else {
				    abort_error(88,"invalid data field expression - " + dc_field);
			    }
		    } else {
			    abort_error(88,"invalid data field expression - " + dc_field);
		    }
		}
	    dc_index++; // skip dcf terminator
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
private void process_dch_data(){
	/*
	 * alloc or gen DS/DC H type parms using prev.
	 * settings for dc_dup and dc_len.  Also save
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
		    if  (calc_abs_exp()){
			    dc_index = exp_index;
			    if (gen_obj_code
			    	&& !dc_lit_ref
                    && sym_type[cur_esd_sid] == sym_cst
					&& dc_dup > 0
					&& dc_op){
			    	if (dc_len <= 4 || exp_val >= 0){
				        obj_code = obj_code + get_hex(exp_val,2*dc_len);
			    	} else {
				        obj_code = obj_code + ("FFFFFFFF").substring(0,2*dc_len-8) + get_hex(exp_val,8);
			    	}
					put_obj_text();
			    }
			    if (!dc_lit_ref && dc_dup > 0){
				   loc_ctr = loc_ctr + dc_len;
			    }
			    if (dc_field.charAt(dc_index) == ','){
			    	exp_index++;
			    } else if (dc_field.charAt(dc_index) == '\''){
			    	dc_eod = true;			    	
			    } else {
				    abort_error(88,"invalid data field expression - " + dc_field);
			    }
		    } else {
			    dc_index = exp_index;
	         	abort_error(88,"invalid data field expression - " + dc_field);
		    }
		}
	    dc_index++; // skip dch terminator
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
			         	abort_error(67,"invalid character in P type data field - " + dc_field);
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
		while (!dc_eod && !bal_abort 
				&& dc_field.charAt(dc_index) != ')'){
		    if (dc_field.charAt(dc_index) == ','){
		    	exp_index++;
		    }
		    if  (calc_exp()){
			    dc_index = exp_index;
			    if (gen_obj_code
			    	&& !dc_lit_ref
                    && sym_type[cur_esd_sid] == sym_cst
					&& dc_dup > 0
					&& dc_op){
			    	if  (dc_len == 2){
		    		    obj_code = obj_code + get_rel_exp_bddd();
			    	} else {
			    		log_error(99,"invalid length for S type");
			    	}
					put_obj_text();
			    } 
			    if (!dc_lit_ref && dc_dup > 0){
				   loc_ctr = loc_ctr + dc_len;
			    }
		    }
		}
	    dc_index++; // skip dca terminator
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
		} else {
	        dc_index++; // skip dch terminator
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
	exp_rld_len = 0;
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
	loc_start = loc_ctr;
	if (bal_label != null){
		cur_sid = find_sym(bal_label);
		if (cur_sid < 1){
			if (tot_sym < max_sym - 1){
			   tot_sym++;
			   add_key_index(tot_sym);
			   cur_sid = tot_sym;
			} else {
				log_error(54,"max symbols exceeded");
			}
		}
		if (cur_sid >= 1){
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
			   	  hex_bddd1_loc = get_hex(exp_val,6);
			 } else {
			   	  log_error(53,"invalid equ expression");
			 }
		}
	}
}
private void process_org(){
	/*
	 * reset current location in same csect
	 */
	loc_start = loc_ctr;
	exp_text = bal_parms;
	exp_index = 0;
	if (cur_esd > 0
		&& calc_rel_exp()
		&& exp_esd == cur_esd){
		loc_ctr = exp_val;
		loc_start = loc_ctr;
	} else {
		log_error(102,"org expression must be in same section");
	}
}
private void duplicate_symbol_error(){
	/*
	 * issue error for duplicate symbol definition
	 */
	log_error(72,"duplicate symbol on line" + bal_name_line_num[bal_line_index] + " and " + bal_name_line_num[sym_def[cur_sid]]);
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
		cur_lit = find_key_index(lit_key);
		if (cur_lit != -1){
			if (lit_loc_ref){
				lit_line_loc[cur_lit] = loc_ctr;
			}
			exp_esd = lit_esd[cur_lit];
			exp_val = lit_loc[cur_lit];
            return get_rel_exp_bddd();
		}
		if (!gen_obj_code && tot_lit < max_lit){
		    cur_lit = tot_lit;
		    add_key_index(cur_lit);
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
			if (opt_trace || (gen_obj_code && opt_list)){
				if (obj_code.length() < 16){
				   list_obj_code = list_obj_code.concat("                ");
				} 
				list_obj_code = list_obj_code.substring(0,16);
		 	    put_prn_line(get_hex(lit_loc[cur_lit],6) + " " + list_obj_code + " =" + lit_name[cur_lit]); 
		    }
		}
		cur_lit++;
	}
}
private int find_key_index(String user_key){
	/*
	 * return user_key_index for user_key else -1
	 * and set following for possible add_key_index:
	 *    1.  key_text = user_key
	 *    2.  key_hash = hash code for key
	 *    3.  key_index_last = last search entry
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
			fp_hex = get_hex( 
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
			fp_hex = get_hex( 
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