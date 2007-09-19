import java.awt.GraphicsEnvironment;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.RandomAccessFile;
import java.lang.management.ManagementFactory;
import java.lang.management.MemoryPoolMXBean;
import java.lang.reflect.Array;
import java.math.BigDecimal;
import java.nio.ByteBuffer;
import java.sql.Timestamp;
import java.text.SimpleDateFormat;
import java.util.Arrays;
import java.util.Date;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;


public  class  tz390 {
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

    tz390 is the shared table component of z390.

    ****************************************************
    * Maintenance
    ****************************************************
    * 12/13/05 copied from ez390.java and modified
    * 12/15/05 RPI135 tz390 shared tables
    * 12/18/05 RPI142 add init_opcode_name_keys for
    *          use by both mz390 and az390
    * 12/23/05 RPI 127 allow file sfx overrides and add
    *          shared set_pgm_dir_name_type method
    * 12/23/05 RPI 131 MAXFILE default 10 MB
    * 12/31/05 add update_key_index for use by mz390
    * 12/31/05 RPI 150 add OPSYN support 
    * 01/14/06 RPI 168 correct null dir_cur in z390 startup
    * 01/26/06 RPI 172 move options to tz390
    * 01/29/06 RPI 192 share get_char_sdt(sdt) with fixes
    * 02/02/06 RPI 185 add z9 op opcodes to tables
    * 02/08/06 RPI198 support PARM(..) and upper/lower
    *          case parms
    * 02/10/06 RPI 199 add BLX extended mnemonics
    * 02/16/06 RPI 201 support "..." parms with special chars
    *          and strip single quotes from "parm('...')"
    * 02/18/06 RPI 206 add RRF3 format type
    * 02/21/06 RPI 208 use z390_abort to sync term.
    * 03/13/06 RPI 226 fix options in double quotes
    * 03/28/06 fix TRACEM not being set
    * 04/02/06 RPI 264 mz390 and az390 use verify_ascii_source()
    *          and option TEXT for free form text I/O in mz390
    * 04/04/06 RPI 270 support CA'ASCII' or CE'EBCDIC' SDT's
    * 04/07/06 RPI 274 correct C'.''..' sdt with double quotes
    *          correct reset of new opsyn's
    * 04/09/06 RPI 276 add mz390 option PROFILE(copybook)
    *          and ez390 option IPL(IPLPGM) options
    * 04/12/06 RPI 279 use correct version of max_time_seconds
    * 04/17/06 RPI 284 and opt_max???? for init_arryas()
    * 04/28/06 RPI 302 allow tabs everywhere
    * 04/28/06 RPI 298 correct opcode for MY and MYH
    * 04/30/06 RPI 306 update shared OPSYN to handle macros
    * 05/03/06 RPI 243 add option SYSTERM(..) support to log
    *          start/stop stats and errors on .ERR mod file
    * 05/05/06 RPI 308 add options CICS, PROLOG, and EPILOG
    * 05/12/06 RPI 313 1)share trim_continue, trim_trailing_
    *          and split_line between mz390 and az390
    *          2) Fix parsing for comma continuation to
    *          support prefix operators L' etc.
    *          3) Improve preformance replacing .split with
    *             find_parm_pattern precompiled rex parser
    * 05/13/06 RPI 314 add AGOB and AIFB    
    * 05/13/06 RPI 315 allow ,space within (...) for
    *          macro ops prior to ,space continuation
    *          add option REFORMAT with default off   
    * 05/24/06 RPI 227 add shared alarm_bell for System.out  
    * 06/04/06 RPI 331 set opsyn_name to null for cancel
    * 06/09/06 RPI 342 correct parsing of parms with exp ?' operators 
    * 07/15/06 RPI 368 add ACONTROL opcode 147
    * 07/20/06 RPI 378 correct to use first SYSOBJ file dir
    * 08/08/06 RPI 397 synchronize system.out
    * 08/10/06 RPI 409 optimize find_key_index using type
    * 08/19/06 RPI 415 replace option MFC with ASM and add BAL
    * 09/04/06 RPI 434 increase MAXLCL to 200000.
    * 09/07/06 RPI 431 add option LISTUSE default to show
    *          current usage at each USING and DROP
    * 09/22/06 RPI 439 add Pseudo Code (PC) add opt_pc and opt_maxpc 
    * 09/25/06 RPI 463 support continued string quote followed by parms 
    * 09/27/06 RPI 467 add TRACEP option for pseudo code 
    * 10/19/06 RPI 484 route all traces to trace files 
    * 11/16/06 RPI 499 merge Linux mods using z390_os_type indicator
    * 11/28/06 RPI 500 use system newline for Win/Linux,
    *          set browser to cmd.exe start or forfire 
    * 12/01/06 RPI 509 use "Monospace" font for Win and Linux 
    * 12/01/06 RPI 510 add z390??? env vars for default progams
    * 12/02/06 RPI 511 add option MCALL to put MCALL and MEXIT on PRN 
    * 12/06/06 RPI 407 add opcode type 35 RRF4 for CSDTR DFP 
    * 12/09/06 RPI 515 remove EZ390I prefix for TEST msgs to console
    * 12/17/06 RPI 518 support leading zeros on DFP values 
    * 12/20/06 RPI 406 correct opcode types and add LFAS, SRNMT
    * 01/16/07 RPI 536 add codes 3,4 for infinity and Nan to dfp_cf5 table
    * 01/19/07 RPI 538 add default option PROTECT to prevent PSA mods
    * 01/30/07 RPI 532 change Linux editor to gedit, fix separators
    *          add option INSTALL(path) to override current dir at startup
    * 02/02/07 RPI 546 fix get_file_name to handle relative path 
    *          overrides using dir_cur override to parm_dir.
    *          and replace \ with / file separator if Linux
    * 02/20/07 RPI 549 return line_id = (FID/FLN)GSN for PRN and traces  
    * 02/20/07 RPI 550 correct FLN after COPY in MLC. 
    * 03/09/07 RPI 569 leave CON on for TEST 
    * 03/12/07 RPI 558 init job_date for use in COMRG init by pz390 
    * 04/27/07 RPI 605 remove dup SYSTERM msgs on trace files
    * 05/14/07 RPI 604 BS2000 compatibility option 
    * 05/16/07 RPI 620 correct get_dup_string for CNOP use 
    * 07/19/07 RPI 662 add TS option for timestamp on trace 
    * 07/21/07 RPI 659 remove redundant trace prefixes 
    * 09/11/07 RPI 694 add option ERRSUM to summarize critical errors
    *           1. List missing COPY and MACRO files.
    *           2. List undefined symbols if #1 = 0
    *           3. Total errror counts to ERR and CON only. 
    * 09/18/07 RPI 697 add TRACEQ for QSAM file I/O trace          
    ********************************************************
    * Shared z390 tables                  (last RPI)
    *****************************************************/
	/*
	 * shared version id 
	 */
	// dsh - change version for every release and ptf
	// dsh - change dcb_id_ver for dcb field changes
    String version    = "V1.3.07e";  //dsh
	String dcb_id_ver = "DCBV1001";  //dsh
	byte   acb_id_ver = (byte)0xa0;  // ACB vs DCB id RPI 644 
	/*
	 * global options 
	 */ 
	byte    z390_os_type  = 0;      // 1=win,2=Linux  RPI 499
	byte    z390_os_win   = 1;
	byte    z390_os_linux = 2;
	String  z390_font    = "Monospaced";  // RPI 509 was Courier
	boolean z390_abort   = false;  // global abort request
    boolean opt_amode24  = false;  // link to run amode24
    boolean opt_amode31  = true;   // link to run amode31
    boolean opt_ascii    = false; // use ascii vs ebcdic
    boolean opt_asm      = true;  // run az390 assembler as mz390 subtask  RPI 415
    boolean opt_bal      = false; // generate bal source output from mz390 RPI 415
    boolean opt_bs2000   = false; // Seimens BS2000 asm compatibility
    boolean opt_cics     = false; // exec cics program honoring prolog,epilog
    boolean opt_con      = true;  // log msgs to console
    boolean opt_dump     = false; // only indicative dump on abend unless on
    boolean opt_epilog   = true;  // if cics, insert DFHEIRET
    boolean opt_errsum   = false; // just list critical errors and summary on ERR file and console 
    boolean opt_guam     = false; // use gz390 GUAM GUI access method interface
    String  opt_ipl      = "";    // program to execute at startup
    String  opt_install_loc = ""; // optional install location for source debugging
    boolean opt_list     = true;  // generate LOG file
    boolean opt_listcall = true;  // list macro calls
    boolean opt_listfile = true;  // list each file path
    boolean opt_listuse  = true;  // list usage at USING and DROP
    boolean opt_mcall    = false; // list MCALL and MEXIT on PRN // RPI 511
    boolean opt_obj      = true;  // generate binary MVS compatible OBJ file RPI 694
    boolean opt_objhex   = false; // generate ascii hex obj records (lz390 accepts bin or hex)
    String  opt_parm     = "";    // user parm string for ez390 (mapped to R1 > cvt_exec_parm)
    boolean opt_pc       = true;  // generate macro pseudo code
    boolean opt_pcopt    = true;  // optimize pc code for speed
    String  opt_profile  = "";    // include PROFILE(COPYBOOK) as first MLC statement
    boolean opt_prolog   = true;  // if cics, insert DFHEIBLK and DFHEIENT
    boolean opt_protect  = true;  // prevent PSA mods by user
    boolean opt_reformat = false;  // reformat BAL statements
    boolean opt_regs     = false; // show registers on trace
    boolean opt_rmode24  = true;  // link to load below line
    boolean opt_rmode31  = false; // link to load above line
    boolean opt_stats    = true;  // show statistics on LOG file
    String  opt_sysparm  = "";    // user parm string for mz390  
    String  opt_systerm  = "";    // mod error file name with explict or SYSERR path
    boolean opt_test     = false; // invoke interactive test cmds
    boolean opt_time     = true;  // abend 422 if out of time TIME (sec)
    boolean opt_timing   = true;  // display current date, time, rate
    boolean opt_trace    = false; // trace pz390 instructions to LOG
    boolean opt_tracea   = false;  // trace az390
    boolean opt_traceall = false; // trace all details
    boolean opt_tracel   = false;  // trace lz390
    boolean opt_tracem   = false; // trace mz390
    boolean opt_tracep   = false; // trace pseudo code
    boolean opt_tracemem = false; // trace memory FQE updates to LOG
    boolean opt_traceq   = false; // trace QSAM file I/O
    boolean opt_tracet   = false; // trace TCPIO and TGET/TPUT data I/O
    boolean opt_tracev   = false; // trace VSAM file I/O
    boolean opt_trap     = true;  // trap exceptions as 0C5
    boolean opt_ts       = false; // time-stamp logs RPI 662
    boolean opt_xref     = true;   // cross reference symbols
    String  cmd_parms = " "; // all options from command
    String  test_ddname = null;
    char    z390_amode31 = 'T';
    char    z390_rmode31 = 'F';
    int opt_maxcall  = 50;
    int opt_maxesd   = 1000;
    int opt_maxfile = 10000;
    int opt_maxgbl  = 100000;   // RPI 284
    int opt_maxlcl  = 100000;   
    int opt_maxline = 200000;
    int opt_maxparm = 10000;
    int opt_maxpc   = 50000;  // RPI 439 pseudo code working set
    int opt_maxrld  = 10000;
    int opt_maxsym  = 50000;
    /*
     * Windows and Linux variables
     */
    String z390_acrobat = null; // RPI 500
    String z390_browser = null; // RPI 500
    String z390_command = null; // RPI 500
    String z390_editor  = null; // RPI 500
    /*
	 * global limits with option overrides
	 */
	int    max_mnote_warning = 4;       // mnote limit for warnings (rc=4 vs rc=16) RPI 415
    int    max_errors        = 100;     // ERR(100) max errors before abort
    int    max_main_width  = 800;
    int    max_main_height = 600;
    int    min_main_width  = 150;
    int    min_main_height = 150;
	int    max_line_len = 80;           // RPI 264
	long   max_file_size = 50 << 20;    // max file output 
	long   max_time_seconds  = 15;      // TIME(15)max elapsed time - override time(sec)
	int    monitor_wait = 300;          // fix interval in milliseconds
    int    max_mem           = 1;       // MEM(1)  MB memory default (see mem(mb) override)
    /*
     * shared date and time formats
     */
	SimpleDateFormat sdf_MMddyy = new SimpleDateFormat("MM/dd/yy");
	SimpleDateFormat sdf_HHmmss = new SimpleDateFormat("HH:mm:ss");
    /*
	 * shared pgm dir, name, type and associated dirs
	 */
	String dir_cur = null; // default current dir
	String pgm_dir  = null; // from first parm else dir_cur
	String job_date = null; // job date mm/dd/yy  RPI 558
	String pgm_name = null; // from first parm else abort
	String pgm_type = null; // from first parm override if mlc else def.
    String ada_type = ".ADA"; // ADATA type (not supported yet)
	String bal_type = ".BAL"; // basic assembler output from mz390, input to az390
	String cpy_type = ".CPY"; // copybook source for mz390
    String dat_type = ".DAT"; // AREAD default input for mz390
	String err_type = ".ERR"; // step error and rc log
    String log_type = ".LOG"; // log for z390, ez390, sz390, pz390
	String mac_type = ".MAC"; // macro source
    String mlc_type = ".MLC"; // macro assembler source program
    String obj_type = ".OBJ"; // relocatable object code for az390 and lz390
    String pch_type = ".PCH"; // punch output from mz390
    String prn_type = ".PRN"; // assembly listing for az390
    String tra_type = ".TRA"; // az390 trace file
    String tre_type = ".TRE"; // ez390 trace file
    String trl_type = ".TRL"; // lz390 trace file
    String trm_type = ".TRM"; // mz390 trace file
    String z390_type = ".390"; // z390 executable load module for lz390 and ez390
    String dir_390 = null; // SYS390() load module
    String dir_bal = null; // SYSBAL() az390 source input
    String dir_cpy = null; // SYSCPY() mz390 copybook lib
    String dir_dat = null; // SYSDAT() mz390 AREAD extended option
    String dir_err = null; // SYSERR() ?z390 systerm error file directory
    String dir_log = null; // SYSLOG() ez390 log // RPI 243
    String dir_lst = null; // SYSLST() lz390 listing 
    String dir_mac = null; // SYSMAC() mz390 macro lib
    String dir_mlc = null; // SYSMLC() mz390 source input
    String dir_pch = null; // SYSPCH() mz390 punch output dir
    String dir_prn = null; // SYSPRN() az390 listing
    String dir_obj = null; // SYSOBJ() lz390 object lib
    String dir_trc = null; // SYSTRC() trace file directory
    int max_opsyn = 1000;
    int tot_opsyn = 0;
    int opsyn_index = -1;
    String[]  opsyn_new_name = new String[max_opsyn];
    String[]  opsyn_old_name = new String[max_opsyn];
    int cur_bal_line_num    = 0; // bal starting line number
    int prev_bal_cont_lines = 0; // bal continue lines for prev bal
    /*
     * shared SYSTERM error file
     */
    long   systerm_start = 0; // start time
    String systerm_sec   = ""; // systerm elapsed seconds
    String systerm_file_name      = null;
    RandomAccessFile systerm_file = null;
	String systerm_time = "";     // hh:mm:ss if opt_timing
    String systerm_prefix = null; // pgm_name plus space
    int    systerm_io     = 0;    // total file io count
    int    systerm_ins    = 0;    // ez390 instruction count
    /*
     * trace file used by mz390, az390, lz390, ez390
     */
    String         trace_file_name = null;
	File           trace_file = null;
	BufferedWriter trace_file_buff = null;
	/*
	 * timestamp data for TS optional trace timestamps
	 * The first 23 characters are standard ODBC SQL Timestamp
	 * to start of previous micro-second.  The last 6 digits
	 * are the nanoseconds from last micro-second to current time.
	 */
    long   ts_nano_start = 0; // RPI 662 nanotime at startup
    long   ts_nano_now = 0;   // RPI 662 nanotime now
    long   ts_mic_start = 0;  // RPI 662 cur time in mics at startup
    long   ts_mic_dif   = 0;  // RPI 662 mics from startup to now
    long   ts_mic_now   = 0;  // RPI 662 cur time in mics   
    String ts_nano_digits;    // RPI 662 last 6 digit nanos within mic
    /*
     * shared parm parsing for comma delimited continue
     * statement parsing to find comma used by 
     * both mz390 and az390.
     */
    Pattern find_parm_pattern = null;
    Matcher find_parm_match = null;
    Pattern parm_pattern = null;
    Matcher parm_match = null;
    boolean split_first = true; // first line of statement
    boolean split_cont  = false; // continuation line of statement
    boolean split_comment = false;
    String  split_label = null;
    String  split_op    = null;
    int     split_op_index = -1; // opcode index else -1
    int     split_op_type  = -1; // opcode type index else -1
    String  split_parms = null;
    int     split_parms_index = -1;  // line index to parms else -1
    int     split_level = 0;
    String  split_quote_text = null;
    boolean split_quote = false;
    boolean split_quote_last = false; // last char of prev continue is quote RPI 463
    /*
     * pad_spaces char table for padding
     * starts at 4096 and expands as required
     */
    int    pad_spaces_len = 0;
    char[] pad_spaces = null;
	/*
	 * dup operator buffer
	 */
	int dup_char_len = 0;
	char[] dup_char = null; 
    /*
     * ASCII and EBCDIC printable character tables
     */
        String newline = System.getProperty("line.separator"); // RPI 500
        char   alarm_bell = 0x07;          // ascii bell char for system.out alarm
        int    sdt_char_int = 0; // RPI 192 shared character sdt
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
        String ascii_to_ebcdic_hex = 
                        "00010203372D2E2F1605250B0C0D0E0F" + //00 ................ 
                        "101112003C3D322618193F2722003500" + //20 ................ 
                        "405A7F7B5B6C507D4D5D5C4E6B604B61" + //20  !"#$%&'()*+,-./ 
                        "F0F1F2F3F4F5F6F7F8F97A5E4C7E6E6F" + //30 0123456789:;<=>? 
                        "7CC1C2C3C4C5C6C7C8C9D1D2D3D4D5D6" + //40 @ABCDEFGHIJKLMNO  
                        "D7D8D9E2E3E4E5E6E7E8E9ADE0BD5F6D" + //50 PQRSTUVWXYZ.\.._  
                        "79818283848586878889919293949596" + //60 `abcdefghijklmno 
                        "979899A2A3A4A5A6A7A8A98B4F9BA107" + //70 pqrstuvwxyz.|.~. 
                        "00010203372D2E2F1605250B0C0D0E0F" + //80 ................  
                        "101112003C3D322618193F2722003500" + //90 ................ 
                        "405A7F7B5B6C507D4D5D5C4E6B604B61" + //A0  !"#$%&'()*+,-./ 
                        "F0F1F2F3F4F5F6F7F8F97A5E4C7E6E6F" + //B0 0123456789:;<=>? 
                        "7CC1C2C3C4C5C6C7C8C9D1D2D3D4D5D6" + //C0 @ABCDEFGHIJKLMNO 
                        "D7D8D9E2E3E4E5E6E7E8E9ADE0BD5F6D" + //D0 PQRSTUVWXYZ.\.._ 
                        "79818283848586878889919293949596" + //E0 `abcdefghijklmno 
                        "979899A2A3A4A5A6A7A8A98B4F9BA107"   //F0 pqrstuvwxyz.|.~. 
        ;
        byte[] ebcdic_to_ascii = new byte[256];
        String ebcdic_to_ascii_hex = 
                        "000102030009007F0000000B0C0D0E0F" + //00 ................ 
                        "10111200000008001819000000000000" + //10 ................ 
                        "00001C00000A171B0000000000050607" + //20 ................ 
                        "00001600001E0004000000001415001A" + //30 ................ 
                        "20000000000000000000002E3C282B7C" + //40  ...........<(+| 
                        "2600000000000000000021242A293B5E" + //50 &.........!$*);^ 
                        "2D2F0000000000000000002C255F3E3F" + //60 -/.........,%_>? 
                        "000000000000000000603A2340273D22" + //70 .........`:#@'=" 
                        "00616263646566676869007B00000000" + //80 .abcdefghi.{.... 
                        "006A6B6C6D6E6F707172007D00000000" + //90 .jklmnopqr.}.... 
                        "007E737475767778797A0000005B0000" + //A0 .~stuvwxyz...[.. 
                        "000000000000000000000000005D0000" + //B0 .............].. 
                        "00414243444546474849000000000000" + //C0 .ABCDEFGHI...... 
                        "004A4B4C4D4E4F505152000000000000" + //D0 .JKLMNOPQR...... 
                        "5C00535455565758595A000000000000" + //E0 \.STUVWXYZ...... 
                        "30313233343536373839000000000000";  //F0 0123456789......  
  /*
   * Floating Point shared types and attributes
   * for use by az390 for constants and pz390 instructions
   */
        /*
         * fp conversion from big decimal to LH/LB
         * variables copied from AZ390 routine 
         * developed earlier to convert string to
         * floating point constants (moved for RPI 407
         */
        byte fp_type    = 0; 
        byte fp_db_type = 0; // BFP long
        byte fp_dd_type = 1; // DPF long
        byte fp_dh_type = 2; // HFP long
        byte fp_eb_type = 3; // BFP short
        byte fp_ed_type = 4; // DFP short
        byte fp_eh_type = 5; // HFP short
        byte fp_lb_type = 6; // BFP extended
        byte fp_ld_type = 7; // DFP extended
        byte fp_lh_type = 8; // HFP extended
        byte fp_db_digits = 15;
        byte fp_dd_digits = 16;
        byte fp_dh_digits = 15;
        byte fp_eb_digits = 7;
        byte fp_ed_digits = 7;
        byte fp_eh_digits = 6;
        byte fp_lb_digits = 34;
        byte fp_ld_digits = 34;
        byte fp_lh_digits = 34;
        byte fp_guard_digits = 3;
        /*
         * follow fp_work_reg used to format
         * edl types to binary storage formats
         */
        byte[]     fp_work_reg_byte = (byte[])Array.newInstance(byte.class,17); // 1 extra guard byte ignored
        ByteBuffer fp_work_reg = ByteBuffer.wrap(fp_work_reg_byte,0,17);  
        /*
         * Note:  The following big decimal precision
         *        array used in both az390 and ez390
         *        should be maintained consistently
         *        as it is used for rounding 
         *        during conversions between types.
         */
        int[]  fp_precision = {
        		fp_db_digits+fp_guard_digits,
        		fp_dd_digits+fp_guard_digits,
        		fp_dh_digits+fp_guard_digits,
        		fp_eb_digits+fp_guard_digits,
        		fp_ed_digits+fp_guard_digits,
        		fp_eh_digits+fp_guard_digits,
        		fp_lb_digits+fp_guard_digits,
        		fp_ld_digits+fp_guard_digits,
        		fp_lh_digits+fp_guard_digits
        		}; 
        int[]  fp_digits_max  = {0,16,0,0,7,0,0,34,0};
        int[]  fp_sign_bit    = {0x800,0x20,0x80,0x100,0x20,0x80,0x8000,0x20,0x80}; // RPI 407
        int[]  fp_one_bit_adj = {2,-1,1,2,-1,1,2,-1,1}; // RPI 407
        int[]  fp_exp_bias    = {0x3ff,398,0x40,0x7f,101,0x40,0x3fff,6176,0x40}; // RPI 407
        int[]  fp_exp_max     = {0x7ff,0x3ff,0x7f,0xff,0xff,0x7f,0x7fff,0x3fff,0x7f}; // RPI 407
        int[]  fp_man_bits = {52,-1,56,23,-1,24,112,-1,112};
  /*
   * DFP Decimal Floating Point shared tables
   */
        int fp_sign = 0;
        int fp_exp   = 0; // scale * log10/log2
        String dfp_digits = null;
        byte[] dfp_work = new byte[16];
   /* 
   * dfp_exp_bcd_to_cf5 returns CF5 5 bit 
   * combination field using index made up of 
   * high 2 bits of bias exponent
   * plus 4 bit BCDnibble for first digit. 
   */
        byte[] dfp_exp_bcd_to_cf5 = { // RPI 407 indexed by high 2 bits of exp + fisrt digit
    			0x00,0x01,0x02,0x03,0x04,0x05,0x06,0x07,0x08,0x09,0,0,0,0,0,0, //0d
    			0x08,0x09,0x0A,0x0B,0x0C,0x0D,0x0E,0x0F,0x1A,0x1B,0,0,0,0,0,0, //1d
    			0x10,0x11,0x12,0x13,0x14,0x15,0x16,0x17,0x1C,0x1D,0,0,0,0,0,0, //2d
    	  };
        /*
         * dfp_bcd_to_dpd returns 10 bit densely
         * packed decimal indexed by 3 digit value 0-999
         */
      	  int[] dfp_bcd_to_dpd = {
      	      0x000,0x001,0x002,0x003,0x004,0x005,0x006,0x007,0x008,0x009,
      	      0x010,0x011,0x012,0x013,0x014,0x015,0x016,0x017,0x018,0x019,
      	      0x020,0x021,0x022,0x023,0x024,0x025,0x026,0x027,0x028,0x029,
      	      0x030,0x031,0x032,0x033,0x034,0x035,0x036,0x037,0x038,0x039,
      	      0x040,0x041,0x042,0x043,0x044,0x045,0x046,0x047,0x048,0x049,
      	      0x050,0x051,0x052,0x053,0x054,0x055,0x056,0x057,0x058,0x059,
      	      0x060,0x061,0x062,0x063,0x064,0x065,0x066,0x067,0x068,0x069,
      	      0x070,0x071,0x072,0x073,0x074,0x075,0x076,0x077,0x078,0x079,
      	      0x00A,0x00B,0x02A,0x02B,0x04A,0x04B,0x06A,0x06B,0x04E,0x04F,
      	      0x01A,0x01B,0x03A,0x03B,0x05A,0x05B,0x07A,0x07B,0x05E,0x05F,
      	      0x080,0x081,0x082,0x083,0x084,0x085,0x086,0x087,0x088,0x089,
      	      0x090,0x091,0x092,0x093,0x094,0x095,0x096,0x097,0x098,0x099,
      	      0x0A0,0x0A1,0x0A2,0x0A3,0x0A4,0x0A5,0x0A6,0x0A7,0x0A8,0x0A9,
      	      0x0B0,0x0B1,0x0B2,0x0B3,0x0B4,0x0B5,0x0B6,0x0B7,0x0B8,0x0B9,
      	      0x0C0,0x0C1,0x0C2,0x0C3,0x0C4,0x0C5,0x0C6,0x0C7,0x0C8,0x0C9,
      	      0x0D0,0x0D1,0x0D2,0x0D3,0x0D4,0x0D5,0x0D6,0x0D7,0x0D8,0x0D9,
      	      0x0E0,0x0E1,0x0E2,0x0E3,0x0E4,0x0E5,0x0E6,0x0E7,0x0E8,0x0E9,
      	      0x0F0,0x0F1,0x0F2,0x0F3,0x0F4,0x0F5,0x0F6,0x0F7,0x0F8,0x0F9,
      	      0x08A,0x08B,0x0AA,0x0AB,0x0CA,0x0CB,0x0EA,0x0EB,0x0CE,0x0CF,
      	      0x09A,0x09B,0x0BA,0x0BB,0x0DA,0x0DB,0x0FA,0x0FB,0x0DE,0x0DF,
      	      0x100,0x101,0x102,0x103,0x104,0x105,0x106,0x107,0x108,0x109,
      	      0x110,0x111,0x112,0x113,0x114,0x115,0x116,0x117,0x118,0x119,
      	      0x120,0x121,0x122,0x123,0x124,0x125,0x126,0x127,0x128,0x129,
      	      0x130,0x131,0x132,0x133,0x134,0x135,0x136,0x137,0x138,0x139,
      	      0x140,0x141,0x142,0x143,0x144,0x145,0x146,0x147,0x148,0x149,
      	      0x150,0x151,0x152,0x153,0x154,0x155,0x156,0x157,0x158,0x159,
      	      0x160,0x161,0x162,0x163,0x164,0x165,0x166,0x167,0x168,0x169,
      	      0x170,0x171,0x172,0x173,0x174,0x175,0x176,0x177,0x178,0x179,
      	      0x10A,0x10B,0x12A,0x12B,0x14A,0x14B,0x16A,0x16B,0x14E,0x14F,
      	      0x11A,0x11B,0x13A,0x13B,0x15A,0x15B,0x17A,0x17B,0x15E,0x15F,
      	      0x180,0x181,0x182,0x183,0x184,0x185,0x186,0x187,0x188,0x189,
      	      0x190,0x191,0x192,0x193,0x194,0x195,0x196,0x197,0x198,0x199,
      	      0x1A0,0x1A1,0x1A2,0x1A3,0x1A4,0x1A5,0x1A6,0x1A7,0x1A8,0x1A9,
      	      0x1B0,0x1B1,0x1B2,0x1B3,0x1B4,0x1B5,0x1B6,0x1B7,0x1B8,0x1B9,
      	      0x1C0,0x1C1,0x1C2,0x1C3,0x1C4,0x1C5,0x1C6,0x1C7,0x1C8,0x1C9,
      	      0x1D0,0x1D1,0x1D2,0x1D3,0x1D4,0x1D5,0x1D6,0x1D7,0x1D8,0x1D9,
      	      0x1E0,0x1E1,0x1E2,0x1E3,0x1E4,0x1E5,0x1E6,0x1E7,0x1E8,0x1E9,
      	      0x1F0,0x1F1,0x1F2,0x1F3,0x1F4,0x1F5,0x1F6,0x1F7,0x1F8,0x1F9,
      	      0x18A,0x18B,0x1AA,0x1AB,0x1CA,0x1CB,0x1EA,0x1EB,0x1CE,0x1CF,
      	      0x19A,0x19B,0x1BA,0x1BB,0x1DA,0x1DB,0x1FA,0x1FB,0x1DE,0x1DF,
      	      0x200,0x201,0x202,0x203,0x204,0x205,0x206,0x207,0x208,0x209,
      	      0x210,0x211,0x212,0x213,0x214,0x215,0x216,0x217,0x218,0x219,
      	      0x220,0x221,0x222,0x223,0x224,0x225,0x226,0x227,0x228,0x229,
      	      0x230,0x231,0x232,0x233,0x234,0x235,0x236,0x237,0x238,0x239,
      	      0x240,0x241,0x242,0x243,0x244,0x245,0x246,0x247,0x248,0x249,
      	      0x250,0x251,0x252,0x253,0x254,0x255,0x256,0x257,0x258,0x259,
      	      0x260,0x261,0x262,0x263,0x264,0x265,0x266,0x267,0x268,0x269,
      	      0x270,0x271,0x272,0x273,0x274,0x275,0x276,0x277,0x278,0x279,
      	      0x20A,0x20B,0x22A,0x22B,0x24A,0x24B,0x26A,0x26B,0x24E,0x24F,
      	      0x21A,0x21B,0x23A,0x23B,0x25A,0x25B,0x27A,0x27B,0x25E,0x25F,
      	      0x280,0x281,0x282,0x283,0x284,0x285,0x286,0x287,0x288,0x289,
      	      0x290,0x291,0x292,0x293,0x294,0x295,0x296,0x297,0x298,0x299,
      	      0x2A0,0x2A1,0x2A2,0x2A3,0x2A4,0x2A5,0x2A6,0x2A7,0x2A8,0x2A9,
      	      0x2B0,0x2B1,0x2B2,0x2B3,0x2B4,0x2B5,0x2B6,0x2B7,0x2B8,0x2B9,
      	      0x2C0,0x2C1,0x2C2,0x2C3,0x2C4,0x2C5,0x2C6,0x2C7,0x2C8,0x2C9,
      	      0x2D0,0x2D1,0x2D2,0x2D3,0x2D4,0x2D5,0x2D6,0x2D7,0x2D8,0x2D9,
      	      0x2E0,0x2E1,0x2E2,0x2E3,0x2E4,0x2E5,0x2E6,0x2E7,0x2E8,0x2E9,
      	      0x2F0,0x2F1,0x2F2,0x2F3,0x2F4,0x2F5,0x2F6,0x2F7,0x2F8,0x2F9,
      	      0x28A,0x28B,0x2AA,0x2AB,0x2CA,0x2CB,0x2EA,0x2EB,0x2CE,0x2CF,
      	      0x29A,0x29B,0x2BA,0x2BB,0x2DA,0x2DB,0x2FA,0x2FB,0x2DE,0x2DF,
      	      0x300,0x301,0x302,0x303,0x304,0x305,0x306,0x307,0x308,0x309,
      	      0x310,0x311,0x312,0x313,0x314,0x315,0x316,0x317,0x318,0x319,
      	      0x320,0x321,0x322,0x323,0x324,0x325,0x326,0x327,0x328,0x329,
      	      0x330,0x331,0x332,0x333,0x334,0x335,0x336,0x337,0x338,0x339,
      	      0x340,0x341,0x342,0x343,0x344,0x345,0x346,0x347,0x348,0x349,
      	      0x350,0x351,0x352,0x353,0x354,0x355,0x356,0x357,0x358,0x359,
      	      0x360,0x361,0x362,0x363,0x364,0x365,0x366,0x367,0x368,0x369,
      	      0x370,0x371,0x372,0x373,0x374,0x375,0x376,0x377,0x378,0x379,
      	      0x30A,0x30B,0x32A,0x32B,0x34A,0x34B,0x36A,0x36B,0x34E,0x34F,
      	      0x31A,0x31B,0x33A,0x33B,0x35A,0x35B,0x37A,0x37B,0x35E,0x35F,
      	      0x380,0x381,0x382,0x383,0x384,0x385,0x386,0x387,0x388,0x389,
      	      0x390,0x391,0x392,0x393,0x394,0x395,0x396,0x397,0x398,0x399,
      	      0x3A0,0x3A1,0x3A2,0x3A3,0x3A4,0x3A5,0x3A6,0x3A7,0x3A8,0x3A9,
      	      0x3B0,0x3B1,0x3B2,0x3B3,0x3B4,0x3B5,0x3B6,0x3B7,0x3B8,0x3B9,
      	      0x3C0,0x3C1,0x3C2,0x3C3,0x3C4,0x3C5,0x3C6,0x3C7,0x3C8,0x3C9,
      	      0x3D0,0x3D1,0x3D2,0x3D3,0x3D4,0x3D5,0x3D6,0x3D7,0x3D8,0x3D9,
      	      0x3E0,0x3E1,0x3E2,0x3E3,0x3E4,0x3E5,0x3E6,0x3E7,0x3E8,0x3E9,
      	      0x3F0,0x3F1,0x3F2,0x3F3,0x3F4,0x3F5,0x3F6,0x3F7,0x3F8,0x3F9,
      	      0x38A,0x38B,0x3AA,0x3AB,0x3CA,0x3CB,0x3EA,0x3EB,0x3CE,0x3CF,
      	      0x39A,0x39B,0x3BA,0x3BB,0x3DA,0x3DB,0x3FA,0x3FB,0x3DE,0x3DF,
      	      0x00C,0x00D,0x10C,0x10D,0x20C,0x20D,0x30C,0x30D,0x02E,0x02F,
      	      0x01C,0x01D,0x11C,0x11D,0x21C,0x21D,0x31C,0x31D,0x03E,0x03F,
      	      0x02C,0x02D,0x12C,0x12D,0x22C,0x22D,0x32C,0x32D,0x12E,0x12F,
      	      0x03C,0x03D,0x13C,0x13D,0x23C,0x23D,0x33C,0x33D,0x13E,0x13F,
      	      0x04C,0x04D,0x14C,0x14D,0x24C,0x24D,0x34C,0x34D,0x22E,0x22F,
      	      0x05C,0x05D,0x15C,0x15D,0x25C,0x25D,0x35C,0x35D,0x23E,0x23F,
      	      0x06C,0x06D,0x16C,0x16D,0x26C,0x26D,0x36C,0x36D,0x32E,0x32F,
      	      0x07C,0x07D,0x17C,0x17D,0x27C,0x27D,0x37C,0x37D,0x33E,0x33F,
      	      0x00E,0x00F,0x10E,0x10F,0x20E,0x20F,0x30E,0x30F,0x06E,0x06F,
      	      0x01E,0x01F,0x11E,0x11F,0x21E,0x21F,0x31E,0x31F,0x07E,0x07F,
      	      0x08C,0x08D,0x18C,0x18D,0x28C,0x28D,0x38C,0x38D,0x0AE,0x0AF,
      	      0x09C,0x09D,0x19C,0x19D,0x29C,0x29D,0x39C,0x39D,0x0BE,0x0BF,
      	      0x0AC,0x0AD,0x1AC,0x1AD,0x2AC,0x2AD,0x3AC,0x3AD,0x1AE,0x1AF,
      	      0x0BC,0x0BD,0x1BC,0x1BD,0x2BC,0x2BD,0x3BC,0x3BD,0x1BE,0x1BF,
      	      0x0CC,0x0CD,0x1CC,0x1CD,0x2CC,0x2CD,0x3CC,0x3CD,0x2AE,0x2AF,
      	      0x0DC,0x0DD,0x1DC,0x1DD,0x2DC,0x2DD,0x3DC,0x3DD,0x2BE,0x2BF,
      	      0x0EC,0x0ED,0x1EC,0x1ED,0x2EC,0x2ED,0x3EC,0x3ED,0x3AE,0x3AF,
      	      0x0FC,0x0FD,0x1FC,0x1FD,0x2FC,0x2FD,0x3FC,0x3FD,0x3BE,0x3BF,
      	      0x08E,0x08F,0x18E,0x18F,0x28E,0x28F,0x38E,0x38F,0x0EE,0x0EF,
      	      0x09E,0x09F,0x19E,0x19F,0x29E,0x29F,0x39E,0x39F,0x0FE,0x0FF,                                                  
      	     };
      	  /*
      	   * dfp_cf5_to_exp2 returns 2 high bits of
      	   * biased exponent indexed by 5 bit combined field
      	   */
          int[] dfp_cf5_to_exp2 = {
        		     0,0,0,0,0,0,0,0,  // 0- 7 = 0
		             1,1,1,1,1,1,1,1,  // 8-15 = 1
		             2,2,2,2,2,2,2,2,  //16-23 = 2
		             0,0,              //24-25 = 0
		             1,1,              //26-27 = 1
		             2,2,             //28-29 = 2
		             3,               //30 infinity  RPI 536
		             4};              //31 NaN      RPI 536
          /*
           * dfp_cf5_to_bcd returns decimal digit 0-9
           * indexed by 5 bit combination field value
           */
             long[] dfp_cf5_to_bcd = { //cf5 value
            		 0,1,2,3,4,5,6,7,  //00-07
            		 0,1,2,3,4,5,6,7,  //08-0F
            		 0,1,2,3,4,5,6,7,  //10-17
            		 8,9,              //18-19
            		 8,9,              //1A-1B
            		 8,9               //1C-1D
             };
      	  /*
      	   * dfp_dpd_to_bcd returns 3 digit decimal
      	   * value 0-999 using 10 bit densely packed
      	   * decimal index value.
      	   * Notes:
      	   *   1. Redundent values in (...)
      	   *   2. Java interprets leading 08 as
      	   *      octal number like 0x is hex so
      	   *      any leading 0's should be removed,
      	   */
          long[] dfp_dpd_to_bcd = {
        		  0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 80, 81,800,801,880,881,    // 00
        		 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 90, 91,810,811,890,891,    // 01
        		 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 82, 83,820,821,808,809,    // 02
        		 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 92, 93,830,831,818,819,    // 03
        		 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 84, 85,840,841, 88, 89,    // 04
        		 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 94, 95,850,851, 98, 99,    // 05
        		 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 86, 87,860,861,888,889,    // 06
        		 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 96, 97,870,871,898,899,    // 07
        		100,101,102,103,104,105,106,107,108,109,180,181,900,901,980,981,    // 08
        		110,111,112,113,114,115,116,117,118,119,190,191,910,911,990,991,    // 09
        		120,121,122,123,124,125,126,127,128,129,182,183,920,921,908,909,    // 0A
        		130,131,132,133,134,135,136,137,138,139,192,193,930,931,918,919,    // 0B
        		140,141,142,143,144,145,146,147,148,149,184,185,940,941,188,189,    // 0C
        		150,151,152,153,154,155,156,157,158,159,194,195,950,951,198,199,    // 0D
        		160,161,162,163,164,165,166,167,168,169,186,187,960,961,988,989,    // 0E
        		170,171,172,173,174,175,176,177,178,179,196,197,970,971,998,999,    // 0F
        		200,201,202,203,204,205,206,207,208,209,280,281,802,803,882,883,    // 10
        		210,211,212,213,214,215,216,217,218,219,290,291,812,813,892,893,    // 11
        		220,221,222,223,224,225,226,227,228,229,282,283,822,823,828,829,    // 12
        		230,231,232,233,234,235,236,237,238,239,292,293,832,833,838,839,    // 13
        		240,241,242,243,244,245,246,247,248,249,284,285,842,843,288,289,    // 14
        		250,251,252,253,254,255,256,257,258,259,294,295,852,853,298,299,    // 15
        		260,261,262,263,264,265,266,267,268,269,286,287,862,863,(888),(889),// 16
        		270,271,272,273,274,275,276,277,278,279,296,297,872,873,(898),(899),// 17
        		300,301,302,303,304,305,306,307,308,309,380,381,902,903,982,983,    // 18
        		310,311,312,313,314,315,316,317,318,319,390,391,912,913,992,993,    // 19
        		320,321,322,323,324,325,326,327,328,329,382,383,922,923,928,929,    // 1A
        		330,331,332,333,334,335,336,337,338,339,392,393,932,933,938,939,    // 1B
        		340,341,342,343,344,345,346,347,348,349,384,385,942,943,388,389,    // 1C
        		350,351,352,353,354,355,356,357,358,359,394,395,952,953,398,399,    // 1D
        		360,361,362,363,364,365,366,367,368,369,386,387,962,963,(988),(989),// 1E
        		370,371,372,373,374,375,376,377,378,379,396,397,972,973,(998),(999),// 1F
        		400,401,402,403,404,405,406,407,408,409,480,481,804,805,884,885,    // 20
        		410,411,412,413,414,415,416,417,418,419,490,491,814,815,894,895,    // 21
        		420,421,422,423,424,425,426,427,428,429,482,483,824,825,848,849,    // 22
        		430,431,432,433,434,435,436,437,438,439,492,493,834,835,858,859,    // 23
        		440,441,442,443,444,445,446,447,448,449,484,485,844,845,488,489,    // 24
        		450,451,452,453,454,455,456,457,458,459,494,495,854,855,498,499,    // 25
        		460,461,462,463,464,465,466,467,468,469,486,487,864,865,(888),(889),// 26
        		470,471,472,473,474,475,476,477,478,479,496,497,874,875,(898),(899),// 27
        		500,501,502,503,504,505,506,507,508,509,580,581,904,905,984,985,    // 28
        		510,511,512,513,514,515,516,517,518,519,590,591,914,915,994,995,    // 29
        		520,521,522,523,524,525,526,527,528,529,582,583,924,925,948,949,    // 2A
        		530,531,532,533,534,535,536,537,538,539,592,593,934,935,958,959,    // 2B
        		540,541,542,543,544,545,546,547,548,549,584,585,944,945,588,589,    // 2C
        		550,551,552,553,554,555,556,557,558,559,594,595,954,955,598,599,    // 2D
        		560,561,562,563,564,565,566,567,568,569,586,587,964,965,(988),(989),// 2E
        		570,571,572,573,574,575,576,577,578,579,596,597,974,975,(998),(999),// 2F
        		600,601,602,603,604,605,606,607,608,609,680,681,806,807,886,887,    // 30
        		610,611,612,613,614,615,616,617,618,619,690,691,816,817,896,897,    // 31
        		620,621,622,623,624,625,626,627,628,629,682,683,826,827,868,869,    // 32
        		630,631,632,633,634,635,636,637,638,639,692,693,836,837,878,879,    // 33
        		640,641,642,643,644,645,646,647,648,649,684,685,846,847,688,689,    // 34
        		650,651,652,653,654,655,656,657,658,659,694,695,856,857,698,699,    // 35
        		660,661,662,663,664,665,666,667,668,669,686,687,866,867,(888),(889),// 36
        		670,671,672,673,674,675,676,677,678,679,696,697,876,877,(898),(899),// 37
        		700,701,702,703,704,705,706,707,708,709,780,781,906,907,986,987,    // 38
        		710,711,712,713,714,715,716,717,718,719,790,791,916,917,996,997,    // 39
        		720,721,722,723,724,725,726,727,728,729,782,783,926,927,968,969,    // 3A
        		730,731,732,733,734,735,736,737,738,739,792,793,936,937,978,979,    // 3B
        		740,741,742,743,744,745,746,747,748,749,784,785,946,947,788,789,    // 3C
        		750,751,752,753,754,755,756,757,758,759,794,795,956,957,798,799,    // 3D
        		760,761,762,763,764,765,766,767,768,769,786,787,966,967,(988),(989),// 3E
        		770,771,772,773,774,775,776,777,778,779,796,797,976,977,(998),(999),// 3F
        		};
  /*
   * opcode tables for trace
   */
      String[] op_name = {
    		   "*",        // 00 comments
		       "PR",       // 10 "0101" "PR" "E" 1
		       "UPT",      // 20 "0102" "UPT" "E" 1
		       "PTFF",     //    "0104" "PTFF" "E" 1 Z9-1
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
		       "J",        // 2050 "A74F" "J" "BRCX" 13
		       "JNOP",     // 2060 "A740" "JNOP" "BRCX" 13
		       "BRU",      // 2070 "A74F" "BRU" "BRCX" 13
		       "BRH",      // 2080 "A742" "BRH" "BRCX" 13
		       "BRL",      // 2090 "A744" "BRL" "BRCX" 13
		       "BRE",      // 2100 "A748" "BRE" "BRCX" 13
		       "BRNH",     // 2110 "A74D" "BRNH" "BRCX" 13
		       "BRNL",     // 2120 "A74B" "BRNL" "BRCX" 13
		       "BRNE",     // 2130 "A747" "BRNE" "BRCX" 13
		       "BRP",      // 2140 "A742" "BRP" "BRCX" 13
		       "BRM",      // 2150 "A744" "BRM" "BRCX" 13
		       "BRZ",      // 2160 "A748" "BRZ" "BRCX" 13
		       "BRO",      // 2170 "A741" "BRO" "BRCX" 13
		       "BRNP",     // 2180 "A74D" "BRNP" "BRCX" 13
		       "BRNM",     // 2190 "A74B" "BRNM" "BRCX" 13
		       "BRNZ",     // 2200 "A747" "BRNZ" "BRCX" 13
		       "BRNO",     // 2210 "A74E" "BRNO" "BRCX" 13
		       "JH",       // 2220 "A742" "JH" "BRCX" 13
		       "JL",       // 2230 "A744" "JL" "BRCX" 13
		       "JE",       // 2240 "A748" "JE" "BRCX" 13
		       "JNH",      // 2250 "A74D" "JNH" "BRCX" 13
		       "JNL",      // 2260 "A74B" "JNL" "BRCX" 13
		       "JNE",      // 2270 "A747" "JNE" "BRCX" 13
		       "JP",       // 2280 "A742" "JP" "BRCX" 13  
		       "JM",       // 2290 "A744" "JM" "BRCX" 13
		       "JZ",       // 2300 "A748" "JZ" "BRCX" 13
		       "JO",       // 2310 "A741" "JO" "BRCX" 13
		       "JNP",      // 2320 "A74D" "JNP" "BRCX" 13
		       "JNM",      // 2330 "A74B" "JNM" "BRCX" 13
		       "JNZ",      // 2340 "A747" "JNZ" "BRCX" 13
		       "JNO",      // 2350 "A74E" "JNO" "BRCX" 13
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
		       "STCKF",    //      "B27C" "STCKF" "S" 7 Z9-2
		       "STSI",     // 3290 "B27D" "STSI" "S" 7
		       "SRNM",     // 3300 "B299" "SRNM" "S" 7
		       "STFPC",    // 3310 "B29C" "STFPC" "S" 7
		       "LFPC",     // 3320 "B29D" "LFPC" "S" 7
		       "TRE",      // 3330 "B2A5" "TRE" "RRE" 14
		       "CUUTF",    // 3340 "B2A6" "CUUTF" "RRE" 14
		       "CU21",     // 3350 "B2A6" "CU21" "RRE" 14
		       "CUTFU",    // 3360 "B2A7" "CUTFU" "RRE" 14
		       "CU12",     // 3370 "B2A7" "CU12" "RRE" 14
		       "STFLE",    //      "B2B0" "STFLE" "S" 7 Z9-3
		       "STFL",     // 3380 "B2B1" "STFL" "S" 7
		       "LPSWE",    // 3390 "B2B2" "LPSWE" "S" 7
		       "SRNMT",    // 3395 "B2B9" "SRNMT" "S" 7 DFP 56
		       "LFAS",     // 3395 "B2BD" "LFAS"  "S" 7 DFP 55
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
		       "MAEBR",    // 3550 "B30E" "MAEBR" "RRF1" 15
		       "MSEBR",    // 3560 "B30F" "MSEBR" "RRF1" 15
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
		       "MADBR",    // 3710 "B31E" "MADBR" "RRF1" 15
		       "MSDBR",    // 3720 "B31F" "MSDBR" "RRF1" 15
		       "LDER",     // 3730 "B324" "LDER" "RRE" 14
		       "LXDR",     // 3740 "B325" "LXDR" "RRE" 14
		       "LXER",     // 3750 "B326" "LXER" "RRE" 14
		       "MAER",     // 3760 "B32E" "MAER" "RRF1" 15
		       "MSER",     // 3770 "B32F" "MSER" "RRF1" 15
		       "SQXR",     // 3780 "B336" "SQXR" "RRE" 14
		       "MEER",     // 3790 "B337" "MEER" "RRE" 14
		       "MAYLR",    //      "B338" "MAYLR" "RRF1" 15 Z9-4
		       "MYLR",     //      "B339" "MYLR" "RRF1" 15 Z9-5
		       "MAYR",     //      "B33A" "MAYR" "RRF1" 15 Z9-6
		       "MYR",      //      "B33B" "MYR" "RRF1" 15 Z9-7
		       "MAYHR",    //      "B33C" "MAYHR" "RRF1" 15 Z9-8
		       "MYHR",     //      "B33D" "MYHR" "RRF1" 15 Z9-9
		       "MADR",     // 3800 "B33E" "MADR" "RRF1" 15
		       "MSDR",     // 3810 "B33F" "MSDR" "RRF1" 15
		       "LPXBR",    // 3820 "B340" "LPXBR" "RRE" 14
		       "LNXBR",    // 3830 "B341" "LNXBR" "RRE" 14
		       "LTXBR",    // 3840 "B342" "LTXBR" "RRE" 14
		       "LCXBR",    // 3850 "B343" "LCXBR" "RRE" 14
		       "LEDBR",    // 3860 "B344" "LEDBR" "RRE" 14
		       "LDXBR",    // 3870 "B345" "LDXBR" "RRE" 14
		       "LEXBR",    // 3880 "B346" "LEXBR" "RRE" 14
		       "FIXBR",    // 3890 "B347" "FIXBR" "RRF2" 34
		       "KXBR",     // 3900 "B348" "KXBR" "RRE" 14
		       "CXBR",     // 3910 "B349" "CXBR" "RRE" 14
		       "AXBR",     // 3920 "B34A" "AXBR" "RRE" 14
		       "SXBR",     // 3930 "B34B" "SXBR" "RRE" 14
		       "MXBR",     // 3940 "B34C" "MXBR" "RRE" 14
		       "DXBR",     // 3950 "B34D" "DXBR" "RRE" 14
		       "TBEDR",    // 3960 "B350" "TBEDR" "RRF2" 34
		       "TBDR",     // 3970 "B351" "TBDR" "RRF2" 34
		       "DIEBR",    // 3980 "B353" "DIEBR" "RRF3" 30
		       "FIEBR",    // 3990 "B357" "FIEBR" "RRF2" 34
		       "THDER",    // 4000 "B358" "THDER" "RRE" 14
		       "THDR",     // 4010 "B359" "THDR" "RRE" 14
		       "DIDBR",    // 4020 "B35B" "DIDBR" "RRF3" 30
		       "FIDBR",    // 4030 "B35F" "FIDBR" "RRF2" 34
		       "LPXR",     // 4040 "B360" "LPXR" "RRE" 14
		       "LNXR",     // 4050 "B361" "LNXR" "RRE" 14
		       "LTXR",     // 4060 "B362" "LTXR" "RRE" 14
		       "LCXR",     // 4070 "B363" "LCXR" "RRE" 14
		       "LXR",      // 4080 "B365" "LXR" "RRE" 14
		       "LEXR",     // 4090 "B366" "LEXR" "RRE" 14
		       "FIXR",     // 4100 "B367" "FIXR" "RRE" 14
		       "CXR",      // 4110 "B369" "CXR" "RRE" 14
		       "LPDFR",    // 4115 "B370" "LPDFR" "RRE"  14 DFP
		       "LNDFR",    // 4115 "B371" "LNDFR" "RRE"  14 DFP
		       "CPSDR",    // 4115 "B372" "CPSDR" "RRF2" 34 DFP
		       "LCDFR",    // 4115 "B373" "LCDFR" "RRE"  14 DFP
		       "LZER",     // 4120 "B374" "LZER" "RRE" 14
		       "LZDR",     // 4130 "B375" "LZDR" "RRE" 14
		       "LZXR",     // 4140 "B376" "LZXR" "RRE" 14
		       "FIER",     // 4150 "B377" "FIER" "RRE" 14
		       "FIDR",     // 4160 "B37F" "FIDR" "RRE" 14
		       "SFPC",     // 4170 "B384" "SFPC" "RRE" 14
		       "SFASR",    // 4175 "B385" "SFASR" "RRE" 14 DFP 57
		       "EFPC",     // 4180 "B38C" "EFPC" "RRE" 14
		       "CEFBR",    // 4190 "B394" "CEFBR" "RRE" 14
		       "CDFBR",    // 4200 "B395" "CDFBR" "RRE" 14
		       "CXFBR",    // 4210 "B396" "CXFBR" "RRE" 14
		       "CFEBR",    // 4220 "B398" "CFEBR" "RRF2" 34
		       "CFDBR",    // 4230 "B399" "CFDBR" "RRF2" 34
		       "CFXBR",    // 4240 "B39A" "CFXBR" "RRF2" 34
		       "CEGBR",    // 4250 "B3A4" "CEGBR" "RRE" 14
		       "CDGBR",    // 4260 "B3A5" "CDGBR" "RRE" 14
		       "CXGBR",    // 4270 "B3A6" "CXGBR" "RRE" 14
		       "CGEBR",    // 4280 "B3A8" "CGEBR" "RRF2" 34
		       "CGDBR",    // 4290 "B3A9" "CGDBR" "RRF2" 34
		       "CGXBR",    // 4300 "B3AA" "CGXBR" "RRF2" 34
		       "CEFR",     // 4310 "B3B4" "CEFR" "RRE" 14
		       "CDFR",     // 4320 "B3B5" "CDFR" "RRE" 14
		       "CXFR",     // 4330 "B3B6" "CXFR" "RRE" 14
		       "CFER",     // 4340 "B3B8" "CFER" "RRF2" 34
		       "CFDR",     // 4350 "B3B9" "CFDR" "RRF2" 34
		       "CFXR",     // 4360 "B3BA" "CFXR" "RRF2" 34
		       "LDGR",     // 4365 "B3C1" "LDGR" "RRE" 14 DFP
		       "CEGR",     // 4370 "B3C4" "CEGR" "RRE" 14
		       "CDGR",     // 4380 "B3C5" "CDGR" "RRE" 14
		       "CXGR",     // 4390 "B3C6" "CXGR" "RRE" 14
		       "CGER",     // 4400 "B3C8" "CGER" "RRF2" 34
		       "CGDR",     // 4410 "B3C9" "CGDR" "RRF2" 34
		       "CGXR",     // 4420 "B3CA" "CGXR" "RRF2" 34
		       "LGDR",     // 4425 "B3CD" "LGDR" "RRE" 14 DFP
		       "MDTR", // "B3D0" "RRR" DFP 1
		       "DDTR", // "B3D1" "RRR" DFP 2
		       "ADTR", // "B3D2" "RRR" DFP 3
		       "SDTR", // "B3D3" "RRR" DFP 4
		       "LDETR", // "B3D4" "RRF4" DFP 5
		       "LEDTR", // "B3D5" "RRF3" DFP 6
		       "LTDTR", // "B3D6" "RRE" DFP 7
		       "FIDTR", // "B3D7" "RRF3" DFP 8
		       "MXTR", // "B3D8" "RRR" DFP 9
		       "DXTR", // "B3D9" "RRR" DFP 10
		       "AXTR", // "B3DA" "RRR" DFP 11
		       "SXTR", // "B3DB" "RRR" DFP 12
		       "LXDTR", // "B3DC" "RRF4" DFP 13
		       "LDXTR", // "B3DD" "RRF3" DFP 14
		       "LTXTR", // "B3DE" "RRE" DFP 15
		       "FIXTR", // "B3DF" "RRF3" DFP 16
		       "KDTR", // "B3E0" "RRE" DFP 17
		       "CGDTR", // "B3E1" "RRF4" DFP 18
		       "CUDTR", // "B3E2" "RRE" DFP 19
		       "CSDTR", // "B3E3" "RRF4" DFP 20
		       "CDTR", // "B3E4" "RRE" DFP 21
		       "EEDTR", // "B3E5" "RRE" DFP 22
		       "ESDTR", // "B3E7" "RRE" DFP 23
		       "KXTR", // "B3E8" "RRE" DFP 24
		       "CGXTR", // "B3E9" "RRF4" DFP 25
		       "CUXTR", // "B3EA" "RRE" DFP 26
		       "CSXTR", // "B3EB" "RRF4" DFP 27
		       "CXTR", // "B3EC" "RRE" DFP 28
		       "EEXTR", // "B3ED" "RRE" DFP 29
		       "ESXTR", // "B3EF" "RRE" DFP 30
		       "CDGTR", // "B3F1" "RRE" DFP 31
		       "CDUTR", // "B3F2" "RRE" DFP 32
		       "CDSTR", // "B3F3" "RRE" DFP 33
		       "CEDTR", // "B3F4" "RRE" DFP 34
		       "QADTR", // "B3F5" "RRF3" DFP 35
		       "IEDTR", // "B3F6" "RRF2" DFP 36
		       "RRDTR", // "B3F7" "RRF3" DFP 37
		       "CXGTR", // "B3F9" "RRE" DFP 38
		       "CXUTR", // "B3FA" "RRE" DFP 39
		       "CXSTR", // "B3FB" "RRE" DFP 40
		       "CEXTR", // "B3FC" "RRE" DFP 41
		       "QAXTR", // "B3FD" "RRF3" DFP 42
		       "IEXTR", // "B3FE" "RRF2" DFP 43
		       "RRXTR", // "B3FF" "RRF3" DFP 44
		       "STCTL",    // 4430 "B6" "STCTL" "RS" 10
		       "LCTL",     // 4440 "B7" "LCTL" "RS" 10
		       "LPGR",     // 4450 "B900" "LPGR" "RRE" 14
		       "LNGR",     // 4460 "B901" "LNGR" "RRE" 14
		       "LTGR",     // 4470 "B902" "LTGR" "RRE" 14
		       "LCGR",     // 4480 "B903" "LCGR" "RRE" 14
		       "LGR",      // 4490 "B904" "LGR" "RRE" 14
		       "LURAG",    // 4500 "B905" "LURAG" "RRE" 14
		       "LGBR",     //      "B906" "LGBR" "RRE" 14 Z9-10
		       "LGHR",     //      "B907" "LGHR" "RRE" 14 Z9-11
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
		       "LBR",      //      "B926" "LBR" "RRE" 14 Z9-12
		       "LHR",      //      "B927" "LHR" "RRE" 14 Z9-13
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
		       "FLOGR",    //      "B983" "FLOGR" "RRE" 14 Z9-14
		       "LLGCR",    //      "B984" "LLGCR" "RRE" 14 Z9-15
		       "LLGHR",    //      "B985" "LLGHR" "RRE" 14 Z9-16
		       "MLGR",     // 4870 "B986" "MLGR" "RRE" 14
		       "DLGR",     // 4880 "B987" "DLGR" "RRE" 14
		       "ALCGR",    // 4890 "B988" "ALCGR" "RRE" 14
		       "SLBGR",    // 4900 "B989" "SLBGR" "RRE" 14
		       "CSPG",     // 4910 "B98A" "CSPG" "RRE" 14
		       "EPSW",     // 4920 "B98D" "EPSW" "RRE" 14
		       "IDTE",     // 4930 "B98E" "IDTE" "RRF2" 34
		       "TRTT",     // 4940 "B990" "TRTT" "RRE" 14
		       "TRTO",     // 4950 "B991" "TRTO" "RRE" 14
		       "TROT",     // 4960 "B992" "TROT" "RRE" 14
		       "TROO",     // 4970 "B993" "TROO" "RRE" 14
		       "LLCR",     //      "B994" "LLCR" "RRE" 14 Z9-17
		       "LLHR",     //      "B995" "LLHR" "RRE" 14 Z9-18
		       "MLR",      // 4980 "B996" "MLR" "RRE" 14
		       "DLR",      // 4990 "B997" "DLR" "RRE" 14
		       "ALCR",     // 5000 "B998" "ALCR" "RRE" 14
		       "SLBR",     // 5010 "B999" "SLBR" "RRE" 14
		       "EPAIR",    // 5020 "B99A" "EPAIR" "RRE" 14
		       "ESAIR",    // 5030 "B99B" "ESAIR" "RRE" 14
		       "ESEA",     // 5040 "B99D" "ESEA" "RRE" 14
		       "PTI",      // 5050 "B99E" "PTI" "RRE" 14
		       "SSAIR",    // 5060 "B99F" "SSAIR" "RRE" 14
		       "LPTEA",    //      "B9AA" "LPTEA" "RRE" 14 Z9-19
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
		       "LGFI",     //      "C01" "LGFI" "RIL" 16 Z9-20
		       "BRCL",     // 5180 "C04" "BRCL" "RIL" 16
		       "JLNOP",    // 5390 "C040" "JLNOP" "BLX" 33
		       "BROL",     // 5400 "C041" "BROL" "BLX" 33
		       "JLO",      // 5410 "C041" "JLO" "BLX" 33
		       "BRHL",     // 5420 "C042" "BRHL" "BLX" 33
		       "BRPL",     // 5430 "C042" "BRPL" "BLX" 33
		       "JLH",      // 5440 "C042" "JLH" "BLX" 33
		       "JLP",      // 5450 "C042" "JLP" "BLX" 33
		       "BRLL",     // 5460 "C044" "BRLL" "BLX" 33
		       "BRML",     // 5470 "C044" "BRML" "BLX" 33
		       "JLL",      // 5480 "C044" "JLL" "BLX" 33
		       "JLM",      // 5490 "C044" "JLM" "BLX" 33
		       "BRNEL",    // 5500 "C047" "BRNEL" "BLX" 33
		       "BRNZL",    // 5510 "C047" "BRNZL" "BLX" 33
		       "JLNE",     // 5520 "C047" "JLNE" "BLX" 33
		       "JLNZ",     // 5530 "C047" "JLNZ" "BLX" 33
		       "BREL",     // 5540 "C048" "BREL" "BLX" 33
		       "BRZL",     // 5550 "C048" "BRZL" "BLX" 33
		       "JLE",      // 5560 "C048" "JLE" "BLX" 33
		       "JLZ",      // 5570 "C048" "JLZ" "BLX" 33
		       "BRNLL",    // 5580 "C04B" "BRNLL" "BLX" 33
		       "BRNML",    // 5590 "C04B" "BRNML" "BLX" 33
		       "JLNL",     // 5600 "C04B" "JLNL" "BLX" 33
		       "JLNM",     // 5610 "C04B" "JLNM" "BLX" 33
		       "BRNHL",    // 5620 "C04D" "BRNHL" "BLX" 33
		       "BRNPL",    // 5630 "C04D" "BRNPL" "BLX" 33
		       "JLNH",     // 5640 "C04D" "JLNH" "BLX" 33
		       "JLNP",     // 5650 "C04D" "JLNP" "BLX" 33
		       "BRNOL",    // 5660 "C04E" "BRNOL" "BLX" 33
		       "JLNO",     // 5670 "C04E" "JLNO" "BLX" 33
		       "BRUL",     // 5680 "C04F" "BRUL" "BLX" 33
		       "JLU",      // 5690 "C04F" "JLU" "BLX" 33
		       "BRASL",    // 5210 "C05" "BRASL" "RIL" 16
		       "JASL",     // 5220 "C05" "JASL" "RIL" 16
		       "XIHF",     //      "C06" "XIHF" "RIL" 16 Z9-21
		       "XILF",     //      "C07" "XILF" "RIL" 16 Z9-22
		       "IIHF",     //      "C08" "IIHF" "RIL" 16 Z9-23
		       "IILF",     //      "C09" "IILF" "RIL" 16 Z9-24
		       "NIHF",     //      "C0A" "NIHF" "RIL" 16 Z9-25
		       "NILF",     //      "C0B" "NILF" "RIL" 16 Z9-26
		       "OIHF",     //      "C0C" "OIHF" "RIL" 16 Z9-27
		       "OILF",     //      "C0D" "OILF" "RIL" 16 Z9-28
		       "LLIHF",    //      "C0E" "LLIHF" "RIL" 16 Z9-29
		       "LLILF",    //      "C0F" "LLILF" "RIL" 16 Z9-30
		       "SLGFI",    //      "C24" "SLGFI" "RIL" 16 Z9-31
		       "SLFI",     //      "C25" "SLFI" "RIL" 16 Z9-32
		       "AGFI",     //      "C28" "AGFI" "RIL" 16 Z9-33
		       "AFI",      //      "C29" "AFI" "RIL" 16 Z9-34
		       "ALGFI",    //      "C2A" "ALGFI" "RIL" 16 Z9-35
		       "ALFI",     //      "C2B" "ALFI" "RIL" 16 Z9-36
		       "CGFI",     //      "C2C" "CGFI" "RIL" 16 Z9-37
		       "CFI",      //      "C2D" "CFI" "RIL" 16 Z9-38
		       "CLGFI",    //      "C2E" "CLGFI" "RIL" 16 Z9-39
		       "CLFI",     //      "C2F" "CLFI" "RIL" 16 Z9-40
		       "MVCOS",    //      "C80" "MVCOS" "SSF" 32 Z9-41 // RPI 606
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
		       "LTG",      //      "E302" "LTG" "RXY" 18 Z9-42
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
		       "LT",       //      "E312" "LT" "RXY" 18 Z9-43
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
		       "LLC",      //      "E394" "LLC" "RXY" 18 Z9-44
		       "LLH",      //      "E395" "LLH" "RXY" 18 Z9-45
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
		       "PKA",      // 6180 "E9" "PKA" "SS" 31
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
		       "JXLEG",    // 6610 "EC45" "JXLEG" "RIE" 23
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
		       "MAYL",     //      "ED38" "MAYL" "RXF" 25 Z9-46
		       "MYL",      //      "ED39" "MYL" "RXF" 25 Z9-47
		       "MAY",      //      "ED3A" "MAY" "RXF" 25 Z9-48
		       "MY",       //      "ED3B" "MY" "RXF" 25 Z9-49 RPI 298
		       "MAYH",     //      "ED3C" "MAYH" "RXF" 25 Z9-50
		       "MYH",      //      "ED3D" "MYH" "RXF" 25 Z9-51 RPI 298
		       "MAD",      // 6960 "ED3E" "MAD" "RXF" 25
		       "MSD",      // 6970 "ED3F" "MSD" "RXF" 25
		       "SLDT", // "ED40" "RXF" DFP 45
		       "SRDT", // "ED41" "RXF" DFP 46
		       "SLXT", // "ED48" "RXF" DFP 47
		       "SRXT", // "ED49" "RXF" DFP 48
		       "TDCET", // "ED50" "RXE" DFP 49
		       "TDGET", // "ED51" "RXE" DFP 50
		       "TDCDT", // "ED54" "RXE" DFP 51
		       "TDGDT", // "ED55" "RXE" DFP 52
		       "TDCXT", // "ED58" "RXE" DFP 53
		       "TDGXT", // "ED59" "RXE" DFP 54

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
		       "COPY",     // 7470  "COPY"  224
		       "END",      // 7480  "END"  135
		       "EQU",      // 7490  "EQU"  136
		       "EXITCTL",  // 7500  "EXITCTL"  137
		       "ICTL",     // 7510  "ICTL"  138
		       "ISEQ",     // 7520  "ISEQ"  139
		       "LTORG",    // 7530  "LTORG"  140
		       "OPSYN",    // 7540  "OPSYN"  225
		       "ORG",      // 7550  "ORG"  142
		       "POP",      // 7560  "POP"  143
		       "PUNCH",    // 7570  "PUNCH"  223
		       "PUSH",     // 7580  "PUSH"  145
		       "REPRO",    // 7590  "REPRO"  146
		       "ACONTROL", // 7595  "ACONTROL" 147 /RPI 368
		       "ACTR",     // 7600  "ACTR"  201
		       "AGO",      // 7610  "AGO"  202
		       "AIF",      // 7620  "AIF"  203
		       "AINSERT",  // 7630  "AINSERT"  204
		       "ANOP",     // 7640  "ANOP"  205
		       "AREAD",    // 7650  "AREAD"  206
		       "GBLA",     // 7660  "GBLA"  207
		       "GBLB",     // 7670  "GBLB"  208
		       "GBLC",     // 7680  "GBLC"  209
		       "LCLA",     // 7690  "LCLA"  210
		       "LCLB",     // 7700  "LCLB"  211
		       "LCLC",     // 7710  "LCLC"  212
		       "MHELP",    // 7720  "MHELP"  213
		       "MNOTE",    // 7730  "MNOTE"  214
		       "SETA",     // 7740  "SETA"  215
		       "SETAF",    // 7750  "SETAF"  216
		       "SETB",     // 7760  "SETB"  217
		       "SETC",     // 7770  "SETC"  218
		       "SETCF",    // 7780  "SETCF"  219
		       "MACRO",    // 7790  "MACRO"  220
		       "MEND",     // 7800  "MEND"  221
		       "MEXIT",   // 7810  "MEXIT"  222
		       "AGOB",    // 7820  "AGOB"   226
		       "AIFB",    // 7830  "AIFB"   227
	       
      };
    int[]    op_type_len = {
    	 0, // 0 comment place holder	
         2,	// 1 "E" 8 PR oooo
         2, // 2 "RR" 60  LR  oorr  
         2, // 3 "BRX" 16  BER oomr
         2, // 4 "I" 1 SVC 00ii
         4, // 5 "RX" 52  L  oorxbddd
         4, // 6 "BCX" 16 BE  oomxbddd
         4, // 7 "S" 43 SPM oo00bddd
         4, // 8 "DM" 1  DIAGNOSE 83000000
         4, // 9 "RSI" 4 BRXH  oorriiii
         4, //10 "RS" 25  oorrbddd
         4, //11 "SI" 9 CLI  ooiibddd
         4, //12 "RI" 37 IIHH  ooroiiii
         4, //13 "BRCX" 31 BRE  oomoiiii
         4, //14 "RRE" 185  MSR oooo00rr
         4, //15 "RRF1" 28 MAER oooor0rr (r1,r3,r2 maps to r1,r3,r2)
         6, //16 "RIL" 6  BRCL  oomollllllll
         6, //17 "SS" 32  MVC oollbdddbddd  
         6, //18 "RXY" 76 MLG oorxbdddhhoo
         6, //19 "SSE" 5  LASP  oooobdddbddd
         6, //20 "RSY" 31  LMG  oorrbdddhhoo
         6, //21 "SIY" 6  TMY  ooiibdddhhoo
         6, //22 "RSL" 1  TP  oor0bddd00oo
         6, //23 "RIE" 4  BRXLG  oorriiii00oo
         6, //24 "RXE" 28  ADB oorxbddd00oo
         6, //25 "RXF" 8   MAE  oorxbdddr0oo (note r3 before r1)
         6, //26 AP SS2  oollbdddbddd
         6, //27 PLO SS3  oorrbdddbddd  r1,s2,r3,s4
         6, //28 LMD SS5  oorrbdddbddd  r1,r3,s2,s4
         6, //29 SRP SS2  oolibdddbddd s1(l1),s2,i3
         4, //30 "RRF3" 30 DIEBR oooormrr (r1,r3,r2,m4 maps to r3,m4,r1,r2) RPI 407 fix (was 6) 
         6, //31 "SS" PKA oollbdddbddd  ll from S2 
         6, //32 "SSF" MVCOS oor0bdddbddd (s1,s2,r3) z9-41
         6, //33 "BLX" BRCL  oomollllllll (label)
         4,  //34 "RRF2" FIXBR oooom0rr (r1,m3,r2 maps to m3,r1,r2) RPI 407 fix was 6
         4,  //35 "FFR4" CSDTR oooo0mrr (r1,r2,m4 maps to m4,r1,r2) RPI 407 add new
         4,  //36 "RRR"  
         };
	int    max_op_type_offset = 36; // see changes required
    int    max_ins_type = 100;    // RPI 315 
    int    max_asm_type = 200;
    int    max_mac_type = 300;
	//  When adding new opcode case: // RPI 407 type 35 for CSDTR etc
	//  1.  Increase the above max.
	//  2.  Change above op_type_len table which must match
    //  3.  Change az390 instruction format cases and
    //  4.  Change pz390 op_type_offset and op_type_mask
    //  5.  Change pz390 trace_psw to add new case
  	int[]    op_type  = {
			   0,  // comments
		       1,  // 10 "0101" "PR" "E" 1
		       1,  // 20 "0102" "UPT" "E" 1		       
		       1,  //    "0104" "PTFF" "E" 1 Z9-1
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
		       13,  // 2050 "A74F" "J" "BRCX" 13
		       13,  // 2060 "A740" "JNOP" "BRCX" 13
		       13,  // 2070 "A74F" "BRU" "BRCX" 13
		       13,  // 2080 "A742" "BRH" "BRCX" 13
		       13,  // 2090 "A744" "BRL" "BRCX" 13
		       13,  // 2100 "A748" "BRE" "BRCX" 13
		       13,  // 2110 "A74D" "BRNH" "BRCX" 13
		       13,  // 2120 "A74B" "BRNL" "BRCX" 13
		       13,  // 2130 "A747" "BRNE" "BRCX" 13
		       13,  // 2140 "A742" "BRP" "BRCX" 13
		       13,  // 2150 "A744" "BRM" "BRCX" 13
		       13,  // 2160 "A748" "BRZ" "BRCX" 13
		       13,  // 2170 "A741" "BRO" "BRCX" 13
		       13,  // 2180 "A74D" "BRNP" "BRCX" 13
		       13,  // 2190 "A74B" "BRNM" "BRCX" 13
		       13,  // 2200 "A747" "BRNZ" "BRCX" 13
		       13,  // 2210 "A74E" "BRNO" "BRCX" 13
		       13,  // 2220 "A742" "JH" "BRCX" 13
		       13,  // 2230 "A744" "JL" "BRCX" 13
		       13,  // 2240 "A748" "JE" "BRCX" 13
		       13,  // 2250 "A74D" "JNH" "BRCX" 13
		       13,  // 2260 "A74B" "JNL" "BRCX" 13
		       13,  // 2270 "A747" "JNE" "BRCX" 13
		       13,  // 2280 "A742" "JP" "BRCX" 13 
		       13,  // 2290 "A744" "JM" "BRCX" 13
		       13,  // 2300 "A748" "JZ" "BRCX" 13
		       13,  // 2310 "A741" "JO" "BRCX" 13
		       13,  // 2320 "A74D" "JNP" "BRCX" 13
		       13,  // 2330 "A74B" "JNM" "BRCX" 13
		       13,  // 2340 "A747" "JNZ" "BRCX" 13
		       13,  // 2350 "A74E" "JNO" "BRCX" 13
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
		       7,  //      "B27C" "STCKF" "S" 7 Z9-2
		       7,  // 3290 "B27D" "STSI" "S" 7
		       7,  // 3300 "B299" "SRNM" "S" 7
		       7,  // 3310 "B29C" "STFPC" "S" 7
		       7,  // 3320 "B29D" "LFPC" "S" 7
		       14,  // 3330 "B2A5" "TRE" "RRE" 14
		       14,  // 3340 "B2A6" "CUUTF" "RRE" 14
		       14,  // 3350 "B2A6" "CU21" "RRE" 14
		       14,  // 3360 "B2A7" "CUTFU" "RRE" 14
		       14,  // 3370 "B2A7" "CU12" "RRE" 14
		       7,  //      "B2B0" "STFLE" "S" 7 Z9-3
		       7,  // 3380 "B2B1" "STFL" "S" 7
		       7,  // 3390 "B2B2" "LPSWE" "S" 7
		       7,  // 3395 "B2B9" "SRNMT" "S" 7 DFP 56
		       7,  // 3395 "B2BD" "LFAS"  "S" 7 DFP 55
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
		       15,  // 3550 "B30E" "MAEBR" "RRF1" 15
		       15,  // 3560 "B30F" "MSEBR" "RRF1" 15
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
		       15,  // 3710 "B31E" "MADBR" "RRF1" 15
		       15,  // 3720 "B31F" "MSDBR" "RRF1" 15
		       14,  // 3730 "B324" "LDER" "RRE" 14
		       14,  // 3740 "B325" "LXDR" "RRE" 14
		       14,  // 3750 "B326" "LXER" "RRE" 14
		       15,  // 3760 "B32E" "MAER" "RRF1" 15
		       15,  // 3770 "B32F" "MSER" "RRF1" 15
		       14,  // 3780 "B336" "SQXR" "RRE" 14
		       14,  // 3790 "B337" "MEER" "RRE" 14
		       15,  //      "B338" "MAYLR" "RRF1" 15 Z9-4
		       15,  //      "B339" "MYLR" "RRF1" 15 Z9-5
		       15,  //      "B33A" "MAYR" "RRF1" 15 Z9-6
		       15,  //      "B33B" "MYR" "RRF1" 15 Z9-7
		       15,  //      "B33C" "MAYHR" "RRF1" 15 Z9-8
		       15,  //      "B33D" "MYHR" "RRF1" 15 Z9-9
		       15,  // 3800 "B33E" "MADR" "RRF1" 15
		       15,  // 3810 "B33F" "MSDR" "RRF1" 15
		       14,  // 3820 "B340" "LPXBR" "RRE" 14
		       14,  // 3830 "B341" "LNXBR" "RRE" 14
		       14,  // 3840 "B342" "LTXBR" "RRE" 14
		       14,  // 3850 "B343" "LCXBR" "RRE" 14
		       14,  // 3860 "B344" "LEDBR" "RRE" 14
		       14,  // 3870 "B345" "LDXBR" "RRE" 14
		       14,  // 3880 "B346" "LEXBR" "RRE" 14
		       34,  // 3890 "B347" "FIXBR" "RRF2" 34
		       14,  // 3900 "B348" "KXBR" "RRE" 14
		       14,  // 3910 "B349" "CXBR" "RRE" 14
		       14,  // 3920 "B34A" "AXBR" "RRE" 14
		       14,  // 3930 "B34B" "SXBR" "RRE" 14
		       14,  // 3940 "B34C" "MXBR" "RRE" 14
		       14,  // 3950 "B34D" "DXBR" "RRE" 14
		       34,  // 3960 "B350" "TBEDR" "RRF2" 34
		       34,  // 3970 "B351" "TBDR" "RRF2" 34
		       30,  // 3980 "B353" "DIEBR" "RRF3" 30
		       34,  // 3990 "B357" "FIEBR" "RRF2" 34
		       14,  // 4000 "B358" "THDER" "RRE" 14
		       14,  // 4010 "B359" "THDR" "RRE" 14
		       30,  // 4020 "B35B" "DIDBR" "RRF3" 30
		       34,  // 4030 "B35F" "FIDBR" "RRF2" 34
		       14,  // 4040 "B360" "LPXR" "RRE" 14
		       14,  // 4050 "B361" "LNXR" "RRE" 14
		       14,  // 4060 "B362" "LTXR" "RRE" 14
		       14,  // 4070 "B363" "LCXR" "RRE" 14
		       14,  // 4080 "B365" "LXR" "RRE" 14
		       14,  // 4090 "B366" "LEXR" "RRE" 14
		       14,  // 4100 "B367" "FIXR" "RRE" 14
		       14,  // 4110 "B369" "CXR" "RRE" 14
		       14,  // 4115 "B370" "LPDFR" "RRE"  14 DFP
		       14,  // 4115 "B371" "LNDFR" "RRE"  14 DFP
		       34,  // 4115 "B372" "CPSDR" "RRF2" 34 DFP
		       14,  // 4115 "B373" "LCDFR" "RRE"  14 DFP
		       14,  // 4120 "B374" "LZER" "RRE" 14
		       14,  // 4130 "B375" "LZDR" "RRE" 14
		       14,  // 4140 "B376" "LZXR" "RRE" 14
		       14,  // 4150 "B377" "FIER" "RRE" 14
		       14,  // 4160 "B37F" "FIDR" "RRE" 14
		       14,  // 4170 "B384" "SFPC" "RRE" 14
		       14,  // 4175 "B385" "SFASR" "RRE" 14 DFP 57
		       14,  // 4180 "B38C" "EFPC" "RRE" 14
		       14,  // 4190 "B394" "CEFBR" "RRE" 14
		       14,  // 4200 "B395" "CDFBR" "RRE" 14
		       14,  // 4210 "B396" "CXFBR" "RRE" 14
		       34,  // 4220 "B398" "CFEBR" "RRF2" 34
		       34,  // 4230 "B399" "CFDBR" "RRF2" 34
		       34,  // 4240 "B39A" "CFXBR" "RRF2" 34
		       14,  // 4250 "B3A4" "CEGBR" "RRE" 14
		       14,  // 4260 "B3A5" "CDGBR" "RRE" 14
		       14,  // 4270 "B3A6" "CXGBR" "RRE" 14
		       34,  // 4280 "B3A8" "CGEBR" "RRF2" 34
		       34,  // 4290 "B3A9" "CGDBR" "RRF2" 34
		       34,  // 4300 "B3AA" "CGXBR" "RRF2" 34
		       14,  // 4310 "B3B4" "CEFR" "RRE" 14
		       14,  // 4320 "B3B5" "CDFR" "RRE" 14
		       14,  // 4330 "B3B6" "CXFR" "RRE" 14
		       34,  // 4340 "B3B8" "CFER" "RRF2" 34
		       34,  // 4350 "B3B9" "CFDR" "RRF2" 34
		       34,  // 4360 "B3BA" "CFXR" "RRF2" 34
		       14,  // 4365 "B3C1" "LDGR" "RRE" 14 DFP
		       14,  // 4370 "B3C4" "CEGR" "RRE" 14
		       14,  // 4380 "B3C5" "CDGR" "RRE" 14
		       14,  // 4390 "B3C6" "CXGR" "RRE" 14
		       34,  // 4400 "B3C8" "CGER" "RRF2" 34
		       34,  // 4410 "B3C9" "CGDR" "RRF2" 34
		       34,  // 4420 "B3CA" "CGXR" "RRF2" 34
		       14,  // 4425 "B3CD" "LGDR" "RRE" 14 DFP
		       36, // "MDTR" "B3D0" "RRR" DFP 1
		       36, // "DDTR" "B3D1" "RRR" DFP 2
		       36, // "ADTR" "B3D2" "RRR" DFP 3
		       36, // "SDTR" "B3D3" "RRR" DFP 4
		       35, // "LDETR" "B3D4" "RRF4" DFP 5
		       30, // "LEDTR" "B3D5" "RRF3" DFP 6
		       14, // "LTDTR" "B3D6" "RRE" DFP 7
		       30, // "FIDTR" "B3D7" "RRF3" DFP 8
		       36, // "MXTR" "B3D8" "RRR" DFP 9
		       36, // "DXTR" "B3D9" "RRR" DFP 10
		       36, // "AXTR" "B3DA" "RRR" DFP 11
		       36, // "SXTR" "B3DB" "RRR" DFP 12
		       35, // "LXDTR" "B3DC" "RRF4" DFP 13
		       30, // "LDXTR" "B3DD" "RRF3" DFP 14
		       14, // "LTXTR" "B3DE" "RRE" DFP 15
		       30, // "FIXTR" "B3DF" "RRF3" DFP 16
		       14, // "KDTR" "B3E0" "RRE" DFP 17
		       34, // "CGDTR" "B3E1" "RRF4" DFP 18
		       14, // "CUDTR" "B3E2" "RRE" DFP 19
		       35, // "CSDTR" "B3E3" "RRF4" DFP 20
		       14, // "CDTR" "B3E4" "RRE" DFP 21
		       14, // "EEDTR" "B3E5" "RRE" DFP 22
		       14, // "ESDTR" "B3E7" "RRE" DFP 23
		       14, // "KXTR" "B3E8" "RRE" DFP 24
		       34, // "CGXTR" "B3E9" "RRF4" DFP 25
		       14, // "CUXTR" "B3EA" "RRE" DFP 26
		       35, // "CSXTR" "B3EB" "RRF4" DFP 27
		       14, // "CXTR" "B3EC" "RRE" DFP 28
		       14, // "EEXTR" "B3ED" "RRE" DFP 29
		       14, // "ESXTR" "B3EF" "RRE" DFP 30
		       14, // "CDGTR" "B3F1" "RRE" DFP 31
		       14, // "CDUTR" "B3F2" "RRE" DFP 32
		       14, // "CDSTR" "B3F3" "RRE" DFP 33
		       14, // "CEDTR" "B3F4" "RRE" DFP 34
		       30, // "QADTR" "B3F5" "RRF3" DFP 35
		       34, // "IEDTR" "B3F6" "RRF2" DFP 36
		       30, // "RRDTR" "B3F7" "RRF3" DFP 37
		       14, // "CXGTR" "B3F9" "RRE" DFP 38
		       14, // "CXUTR" "B3FA" "RRE" DFP 39
		       14, // "CXSTR" "B3FB" "RRE" DFP 40
		       14, // "CEXTR" "B3FC" "RRE" DFP 41
		       30, // "QAXTR" "B3FD" "RRF3" DFP 42
		       34, // "IEXTR" "B3FE" "RRF2" DFP 43
		       30, // "RRXTR" "B3FF" "RRF3" DFP 44
		       10,  // 4430 "B6" "STCTL" "RS" 10
		       10,  // 4440 "B7" "LCTL" "RS" 10
		       14,  // 4450 "B900" "LPGR" "RRE" 14
		       14,  // 4460 "B901" "LNGR" "RRE" 14
		       14,  // 4470 "B902" "LTGR" "RRE" 14
		       14,  // 4480 "B903" "LCGR" "RRE" 14
		       14,  // 4490 "B904" "LGR" "RRE" 14
		       14,  // 4500 "B905" "LURAG" "RRE" 14
		       14,  //      "B906" "LGBR" "RRE" 14 Z9-10
		       14,  //      "B907" "LGHR" "RRE" 14 Z9-11
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
		       14,  //      "B926" "LBR" "RRE" 14 Z9-12
		       14,  //      "B927" "LHR" "RRE" 14 Z9-13
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
		       14,  //      "B983" "FLOGR" "RRE" 14 Z9-14
		       14,  //      "B984" "LLGCR" "RRE" 14 Z9-15
		       14,  //      "B985" "LLGHR" "RRE" 14 Z9-16
		       14,  // 4870 "B986" "MLGR" "RRE" 14
		       14,  // 4880 "B987" "DLGR" "RRE" 14
		       14,  // 4890 "B988" "ALCGR" "RRE" 14
		       14,  // 4900 "B989" "SLBGR" "RRE" 14
		       14,  // 4910 "B98A" "CSPG" "RRE" 14
		       14,  // 4920 "B98D" "EPSW" "RRE" 14
		       34,  // 4930 "B98E" "IDTE" "RRF2" 34
		       14,  // 4940 "B990" "TRTT" "RRE" 14
		       14,  // 4950 "B991" "TRTO" "RRE" 14
		       14,  // 4960 "B992" "TROT" "RRE" 14
		       14,  // 4970 "B993" "TROO" "RRE" 14
		       14,  //      "B994" "LLCR" "RRE" 14 Z9-17
		       14,  //      "B995" "LLHR" "RRE" 14 Z9-18
		       14,  // 4980 "B996" "MLR" "RRE" 14
		       14,  // 4990 "B997" "DLR" "RRE" 14
		       14,  // 5000 "B998" "ALCR" "RRE" 14
		       14,  // 5010 "B999" "SLBR" "RRE" 14
		       14,  // 5020 "B99A" "EPAIR" "RRE" 14
		       14,  // 5030 "B99B" "ESAIR" "RRE" 14
		       14,  // 5040 "B99D" "ESEA" "RRE" 14
		       14,  // 5050 "B99E" "PTI" "RRE" 14
		       14,  // 5060 "B99F" "SSAIR" "RRE" 14
		       14,  //      "B9AA" "LPTEA" "RRE" 14 Z9-19
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
		       16,  //      "C01" "LGFI" "RIL" 16 Z9-20
		       16,  // 5180 "C04" "BRCL" "RIL" 16
		       33,  // 5390 "C040" "JLNOP" "BLX" 33
		       33,  // 5400 "C041" "BROL" "BLX" 33
		       33,  // 5410 "C041" "JLO" "BLX" 33
		       33,  // 5420 "C042" "BRHL" "BLX" 33
		       33,  // 5430 "C042" "BRPL" "BLX" 33
		       33,  // 5440 "C042" "JLH" "BLX" 33
		       33,  // 5450 "C042" "JLP" "BLX" 33
		       33,  // 5460 "C044" "BRLL" "BLX" 33
		       33,  // 5470 "C044" "BRML" "BLX" 33
		       33,  // 5480 "C044" "JLL" "BLX" 33
		       33,  // 5490 "C044" "JLM" "BLX" 33
		       33,  // 5500 "C047" "BRNEL" "BLX" 33
		       33,  // 5510 "C047" "BRNZL" "BLX" 33
		       33,  // 5520 "C047" "JLNE" "BLX" 33
		       33,  // 5530 "C047" "JLNZ" "BLX" 33
		       33,  // 5540 "C048" "BREL" "BLX" 33
		       33,  // 5550 "C048" "BRZL" "BLX" 33
		       33,  // 5560 "C048" "JLE" "BLX" 33
		       33,  // 5570 "C048" "JLZ" "BLX" 33
		       33,  // 5580 "C04B" "BRNLL" "BLX" 33
		       33,  // 5590 "C04B" "BRNML" "BLX" 33
		       33,  // 5600 "C04B" "JLNL" "BLX" 33
		       33,  // 5610 "C04B" "JLNM" "BLX" 33
		       33,  // 5620 "C04D" "BRNHL" "BLX" 33
		       33,  // 5630 "C04D" "BRNPL" "BLX" 33
		       33,  // 5640 "C04D" "JLNH" "BLX" 33
		       33,  // 5650 "C04D" "JLNP" "BLX" 33
		       33,  // 5660 "C04E" "BRNOL" "BLX" 33
		       33,  // 5670 "C04E" "JLNO" "BLX" 33
		       33,  // 5680 "C04F" "BRUL" "BLX" 33
		       33,  // 5690 "C04F" "JLU" "BLX" 33
		       16,  // 5210 "C05" "BRASL" "RIL" 16
		       16,  // 5220 "C05" "JASL" "RIL" 16
		       16,  //      "C06" "XIHF" "RIL" 16 Z9-21
		       16,  //      "C07" "XILF" "RIL" 16 Z9-22
		       16,  //      "C08" "IIHF" "RIL" 16 Z9-23
		       16,  //      "C09" "IILF" "RIL" 16 Z9-24
		       16,  //      "C0A" "NIHF" "RIL" 16 Z9-25
		       16,  //      "C0B" "NILF" "RIL" 16 Z9-26
		       16,  //      "C0C" "OIHF" "RIL" 16 Z9-27
		       16,  //      "C0D" "OILF" "RIL" 16 Z9-28
		       16,  //      "C0E" "LLIHF" "RIL" 16 Z9-29
		       16,  //      "C0F" "LLILF" "RIL" 16 Z9-30
		       16,  //      "C24" "SLGFI" "RIL" 16 Z9-31
		       16,  //      "C25" "SLFI" "RIL" 16 Z9-32
		       16,  //      "C28" "AGFI" "RIL" 16 Z9-33
		       16,  //      "C29" "AFI" "RIL" 16 Z9-34
		       16,  //      "C2A" "ALGFI" "RIL" 16 Z9-35
		       16,  //      "C2B" "ALFI" "RIL" 16 Z9-36
		       16,  //      "C2C" "CGFI" "RIL" 16 Z9-37
		       16,  //      "C2D" "CFI" "RIL" 16 Z9-38
		       16,  //      "C2E" "CLGFI" "RIL" 16 Z9-39
		       16,  //      "C2F" "CLFI" "RIL" 16 Z9-40
		       32,  //      "C80" "MVCOS" "SSF" 32 Z9-41		       
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
		       18,  //      "E302" "LTG" "RXY" 18 Z9-42
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
		       18,  //      "E312" "LT" "RXY" 18 Z9-43
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
		       18,  //      "E394" "LLC" "RXY" 18 Z9-44
		       18,  //      "E395" "LLH" "RXY" 18 Z9-45
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
		       31,  // 6180 "E9" "PKA" "SS" 31
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
		       23,  // 6610 "EC45" "JXLEG" "RIE" 23
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
		       25,  //      "ED38" "MAYL" "RXF" 25 Z9-46
		       25,  //      "ED39" "MYL" "RXF" 25 Z9-47
		       25,  //      "ED3A" "MAY" "RXF" 25 Z9-48
		       25,  //      "ED3B" "MY" "RXF" 25 Z9-49 RPI 298
		       25,  //      "ED3C" "MAYH" "RXF" 25 Z9-50
		       25,  //      "ED3D" "MYH" "RXF" 25 Z9-51 RPI 298
		       25,  // 6960 "ED3E" "MAD" "RXF" 25
		       25,  // 6970 "ED3F" "MSD" "RXF" 25
		       25, // "SLDT" "ED40" "RXF" DFP 45
		       25, // "SRDT" "ED41" "RXF" DFP 46
		       25, // "SLXT" "ED48" "RXF" DFP 47
		       25, // "SRXT" "ED49" "RXF" DFP 48
		       24, // "TDCET" "ED50" "RXE" DFP 49
		       24, // "TDGET" "ED51" "RXE" DFP 50
		       24, // "TDCDT" "ED54" "RXE" DFP 51
		       24, // "TDGDT" "ED55" "RXE" DFP 52
		       24, // "TDCXT" "ED58" "RXE" DFP 53
		       24, // "TDGXT" "ED59" "RXE" DFP 54
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
		       224,  // 7470  "COPY"  224
		       135,  // 7480  "END"  135
		       136,  // 7490  "EQU"  136
		       137,  // 7500  "EXITCTL"  137
		       138,  // 7510  "ICTL"  138
		       139,  // 7520  "ISEQ"  139
		       140,  // 7530  "LTORG"  140
		       225,  // 7540  "OPSYN"  225 //RPI150
		       142,  // 7550  "ORG"  142
		       143,  // 7560  "POP"  143
		       223,  // 7570  "PUNCH"  223
		       145,  // 7580  "PUSH"  145
		       146,  // 7590  "REPRO"  146
		       147,  // 7595  "ACONTROL" 147 // RPI 368
		       201,  // 7600  "ACTR"   
		       202,  // 7610  "AGO"  
		       203,  // 7625  "AIF"  
		       204,  // 7630  "AINSERT"  
		       205,  // 7640  "ANOP"  
		       206,  // 7650  "AREAD" 
		       207,  // 7660  "GBLA"  
		       208,  // 7670  "GBLB"  
		       209,  // 7680  "GBLC"  
		       210,  // 7690  "LCLA"  
		       211,  // 7700  "LCLB"  
		       212,  // 7710  "LCLC"  
		       213,  // 7720  "MHELP"  
		       214,  // 7730  "MNOTE"  
		       215,  // 7740  "SETA"  
		       216,  // 7750  "SETAF"  
		       217,  // 7760  "SETB"  
		       218,  // 7770  "SETC"  
		       219,  // 7780  "SETCF"  
		       220,  // 7790  "MACRO"  
		       221,  // 7800  "MEND"  
		       222,  // 7810  "MEXIT"  	
		       226,  // 7820  "AGOB"  
		       227,  // 7830  "AIFB"  
  	           }; 
  	  int        op_code_index = -1;
      String[]   op_code = {
    		   "??",    // 00 comments
		       "0101",  // 10 "0101" "PR" "E" 1
		       "0102",  // 20 "0102" "UPT" "E" 1
		       "0104",  //    "0104" "PTFF" "E" 1 Z9-1
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
		       "A74F",  // 2050 "A74F" "J" "BRCX" 13
		       "A740",  // 2060 "A740" "JNOP" "BRCX" 13
		       "A74F",  // 2070 "A74F" "BRU" "BRCX" 13
		       "A742",  // 2080 "A742" "BRH" "BRCX" 13
		       "A744",  // 2090 "A744" "BRL" "BRCX" 13
		       "A748",  // 2100 "A748" "BRE" "BRCX" 13
		       "A74D",  // 2110 "A74D" "BRNH" "BRCX" 13
		       "A74B",  // 2120 "A74B" "BRNL" "BRCX" 13
		       "A747",  // 2130 "A747" "BRNE" "BRCX" 13
		       "A742",  // 2140 "A742" "BRP" "BRCX" 13
		       "A744",  // 2150 "A744" "BRM" "BRCX" 13
		       "A748",  // 2160 "A748" "BRZ" "BRCX" 13
		       "A741",  // 2170 "A741" "BRO" "BRCX" 13
		       "A74D",  // 2180 "A74D" "BRNP" "BRCX" 13
		       "A74B",  // 2190 "A74B" "BRNM" "BRCX" 13
		       "A747",  // 2200 "A747" "BRNZ" "BRCX" 13
		       "A74E",  // 2210 "A74E" "BRNO" "BRCX" 13
		       "A742",  // 2220 "A742" "JH" "BRCX" 13
		       "A744",  // 2230 "A744" "JL" "BRCX" 13
		       "A748",  // 2240 "A748" "JE" "BRCX" 13
		       "A74D",  // 2250 "A74D" "JNH" "BRCX" 13
		       "A74B",  // 2260 "A74B" "JNL" "BRCX" 13
		       "A747",  // 2270 "A747" "JNE" "BRCX" 13
		       "A742",  // 2280 "A742" "JP" "BRCX" 13 
		       "A744",  // 2290 "A744" "JM" "BRCX" 13
		       "A748",  // 2300 "A748" "JZ" "BRCX" 13
		       "A741",  // 2310 "A741" "JO" "BRCX" 13
		       "A74D",  // 2320 "A74D" "JNP" "BRCX" 13
		       "A74B",  // 2330 "A74B" "JNM" "BRCX" 13
		       "A747",  // 2340 "A747" "JNZ" "BRCX" 13
		       "A74E",  // 2350 "A74E" "JNO" "BRCX" 13
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
		       "B27C",  //      "B27C" "STCKF" "S" 7 Z9-2
		       "B27D",  // 3290 "B27D" "STSI" "S" 7
		       "B299",  // 3300 "B299" "SRNM" "S" 7
		       "B29C",  // 3310 "B29C" "STFPC" "S" 7
		       "B29D",  // 3320 "B29D" "LFPC" "S" 7
		       "B2A5",  // 3330 "B2A5" "TRE" "RRE" 14
		       "B2A6",  // 3340 "B2A6" "CUUTF" "RRE" 14
		       "B2A6",  // 3350 "B2A6" "CU21" "RRE" 14
		       "B2A7",  // 3360 "B2A7" "CUTFU" "RRE" 14
		       "B2A7",  // 3370 "B2A7" "CU12" "RRE" 14
		       "B2B0",  //      "B2B0" "STFLE" "S" 7 Z9-3
		       "B2B1",  // 3380 "B2B1" "STFL" "S" 7
		       "B2B2",  // 3390 "B2B2" "LPSWE" "S" 7
		       "B2B9",  // 3395 "B2B9" "SRNMT" "S" 7 DFP 56
		       "B2BD",  // 3395 "B2BD" "LFAS"  "S" 7 DFP 55
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
		       "B30E",  // 3550 "B30E" "MAEBR" "RRF1" 15
		       "B30F",  // 3560 "B30F" "MSEBR" "RRF1" 15
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
		       "B31E",  // 3710 "B31E" "MADBR" "RRF1" 15
		       "B31F",  // 3720 "B31F" "MSDBR" "RRF1" 15
		       "B324",  // 3730 "B324" "LDER" "RRE" 14
		       "B325",  // 3740 "B325" "LXDR" "RRE" 14
		       "B326",  // 3750 "B326" "LXER" "RRE" 14
		       "B32E",  // 3760 "B32E" "MAER" "RRF1" 15
		       "B32F",  // 3770 "B32F" "MSER" "RRF1" 15
		       "B336",  // 3780 "B336" "SQXR" "RRE" 14
		       "B337",  // 3790 "B337" "MEER" "RRE" 14
		       "B338",  //      "B338" "MAYLR" "RRF1" 15 Z9-4
		       "B339",  //      "B339" "MYLR" "RRF1" 15 Z9-5
		       "B33A",  //      "B33A" "MAYR" "RRF1" 15 Z9-6
		       "B33B",  //      "B33B" "MYR" "RRF1" 15 Z9-7
		       "B33C",  //      "B33C" "MAYHR" "RRF1" 15 Z9-8
		       "B33D",  //      "B33D" "MYHR" "RRF1" 15 Z9-9
		       "B33E",  // 3800 "B33E" "MADR" "RRF1" 15
		       "B33F",  // 3810 "B33F" "MSDR" "RRF1" 15
		       "B340",  // 3820 "B340" "LPXBR" "RRE" 14
		       "B341",  // 3830 "B341" "LNXBR" "RRE" 14
		       "B342",  // 3840 "B342" "LTXBR" "RRE" 14
		       "B343",  // 3850 "B343" "LCXBR" "RRE" 14
		       "B344",  // 3860 "B344" "LEDBR" "RRE" 14
		       "B345",  // 3870 "B345" "LDXBR" "RRE" 14
		       "B346",  // 3880 "B346" "LEXBR" "RRE" 14
		       "B347",  // 3890 "B347" "FIXBR" "RRF2" 34
		       "B348",  // 3900 "B348" "KXBR" "RRE" 14
		       "B349",  // 3910 "B349" "CXBR" "RRE" 14
		       "B34A",  // 3920 "B34A" "AXBR" "RRE" 14
		       "B34B",  // 3930 "B34B" "SXBR" "RRE" 14
		       "B34C",  // 3940 "B34C" "MXBR" "RRE" 14
		       "B34D",  // 3950 "B34D" "DXBR" "RRE" 14
		       "B350",  // 3960 "B350" "TBEDR" "RRF2" 34
		       "B351",  // 3970 "B351" "TBDR" "RRF2" 34
		       "B353",  // 3980 "B353" "DIEBR" "RRF2" 34
		       "B357",  // 3990 "B357" "FIEBR" "RRF2" 34
		       "B358",  // 4000 "B358" "THDER" "RRE" 14
		       "B359",  // 4010 "B359" "THDR" "RRE" 14
		       "B35B",  // 4020 "B35B" "DIDBR" "RRF2" 34
		       "B35F",  // 4030 "B35F" "FIDBR" "RRF2" 34
		       "B360",  // 4040 "B360" "LPXR" "RRE" 14
		       "B361",  // 4050 "B361" "LNXR" "RRE" 14
		       "B362",  // 4060 "B362" "LTXR" "RRE" 14
		       "B363",  // 4070 "B363" "LCXR" "RRE" 14
		       "B365",  // 4080 "B365" "LXR" "RRE" 14
		       "B366",  // 4090 "B366" "LEXR" "RRE" 14
		       "B367",  // 4100 "B367" "FIXR" "RRE" 14
		       "B369",  // 4110 "B369" "CXR" "RRE" 14
		       "B370",  // 4115 "B370" "LPDFR" "RRE"  14 DFP
		       "B371",  // 4115 "B371" "LNDFR" "RRE"  14 DFP
		       "B372",  // 4115 "B372" "CPSDR" "RRF2" 34 DFP
		       "B373",  // 4115 "B373" "LCDFR" "RRE"  14 DFP
		       "B374",  // 4120 "B374" "LZER" "RRE" 14
		       "B375",  // 4130 "B375" "LZDR" "RRE" 14
		       "B376",  // 4140 "B376" "LZXR" "RRE" 14
		       "B377",  // 4150 "B377" "FIER" "RRE" 14
		       "B37F",  // 4160 "B37F" "FIDR" "RRE" 14
		       "B384",  // 4170 "B384" "SFPC" "RRE" 14
		       "B385",  // 4175 "B385" "SFASR" "RRE" 14 DFP 57
		       "B38C",  // 4180 "B38C" "EFPC" "RRE" 14
		       "B394",  // 4190 "B394" "CEFBR" "RRE" 14
		       "B395",  // 4200 "B395" "CDFBR" "RRE" 14
		       "B396",  // 4210 "B396" "CXFBR" "RRE" 14
		       "B398",  // 4220 "B398" "CFEBR" "RRF2" 34
		       "B399",  // 4230 "B399" "CFDBR" "RRF2" 34
		       "B39A",  // 4240 "B39A" "CFXBR" "RRF2" 34
		       "B3A4",  // 4250 "B3A4" "CEGBR" "RRE" 14
		       "B3A5",  // 4260 "B3A5" "CDGBR" "RRE" 14
		       "B3A6",  // 4270 "B3A6" "CXGBR" "RRE" 14
		       "B3A8",  // 4280 "B3A8" "CGEBR" "RRF2" 34
		       "B3A9",  // 4290 "B3A9" "CGDBR" "RRF2" 34
		       "B3AA",  // 4300 "B3AA" "CGXBR" "RRF2" 34
		       "B3B4",  // 4310 "B3B4" "CEFR" "RRE" 14
		       "B3B5",  // 4320 "B3B5" "CDFR" "RRE" 14
		       "B3B6",  // 4330 "B3B6" "CXFR" "RRE" 14
		       "B3B8",  // 4340 "B3B8" "CFER" "RRF2" 34
		       "B3B9",  // 4350 "B3B9" "CFDR" "RRF2" 34
		       "B3BA",  // 4360 "B3BA" "CFXR" "RRF2" 34
		       "B3C1",  // 4365 "B3C1" "LDGR" "RRE" 14 DFP
		       "B3C4",  // 4370 "B3C4" "CEGR" "RRE" 14
		       "B3C5",  // 4380 "B3C5" "CDGR" "RRE" 14
		       "B3C6",  // 4390 "B3C6" "CXGR" "RRE" 14
		       "B3C8",  // 4400 "B3C8" "CGER" "RRF2" 34
		       "B3C9",  // 4410 "B3C9" "CGDR" "RRF2" 34
		       "B3CA",  // 4420 "B3CA" "CGXR" "RRF2" 34
		       "B3CD",  // 4425 "B3CD" "LGDR" "RRE" 14 DFP
		       "B3D0", // "MDTR" "RRR" DFP 1
		       "B3D1", // "DDTR" "RRR" DFP 2
		       "B3D2", // "ADTR" "RRR" DFP 3
		       "B3D3", // "SDTR" "RRR" DFP 4
		       "B3D4", // "LDETR" "RRF4" DFP 5
		       "B3D5", // "LEDTR" "RRF3" DFP 6
		       "B3D6", // "LTDTR" "RRE" DFP 7
		       "B3D7", // "FIDTR" "RRF3" DFP 8
		       "B3D8", // "MXTR" "RRR" DFP 9
		       "B3D9", // "DXTR" "RRR" DFP 10
		       "B3DA", // "AXTR" "RRR" DFP 11
		       "B3DB", // "SXTR" "RRR" DFP 12
		       "B3DC", // "LXDTR" "RRF4" DFP 13
		       "B3DD", // "LDXTR" "RRF3" DFP 14
		       "B3DE", // "LTXTR" "RRE" DFP 15
		       "B3DF", // "FIXTR" "RRF3" DFP 16
		       "B3E0", // "KDTR" "RRE" DFP 17
		       "B3E1", // "CGDTR" "RRF4" DFP 18
		       "B3E2", // "CUDTR" "RRE" DFP 19
		       "B3E3", // "CSDTR" "RRF4" DFP 20
		       "B3E4", // "CDTR" "RRE" DFP 21
		       "B3E5", // "EEDTR" "RRE" DFP 22
		       "B3E7", // "ESDTR" "RRE" DFP 23
		       "B3E8", // "KXTR" "RRE" DFP 24
		       "B3E9", // "CGXTR" "RRF4" DFP 25
		       "B3EA", // "CUXTR" "RRE" DFP 26
		       "B3EB", // "CSXTR" "RRF4" DFP 27
		       "B3EC", // "CXTR" "RRE" DFP 28
		       "B3ED", // "EEXTR" "RRE" DFP 29
		       "B3EF", // "ESXTR" "RRE" DFP 30
		       "B3F1", // "CDGTR" "RRE" DFP 31
		       "B3F2", // "CDUTR" "RRE" DFP 32
		       "B3F3", // "CDSTR" "RRE" DFP 33
		       "B3F4", // "CEDTR" "RRE" DFP 34
		       "B3F5", // "QADTR" "RRF3" DFP 35
		       "B3F6", // "IEDTR" "RRF2" DFP 36
		       "B3F7", // "RRDTR" "RRF3" DFP 37
		       "B3F9", // "CXGTR" "RRE" DFP 38
		       "B3FA", // "CXUTR" "RRE" DFP 39
		       "B3FB", // "CXSTR" "RRE" DFP 40
		       "B3FC", // "CEXTR" "RRE" DFP 41
		       "B3FD", // "QAXTR" "RRF3" DFP 42
		       "B3FE", // "IEXTR" "RRF2" DFP 43
		       "B3FF", // "RRXTR" "RRF3" DFP 44
		       "B6",  // 4430 "B6" "STCTL" "RS" 10
		       "B7",  // 4440 "B7" "LCTL" "RS" 10
		       "B900",  // 4450 "B900" "LPGR" "RRE" 14
		       "B901",  // 4460 "B901" "LNGR" "RRE" 14
		       "B902",  // 4470 "B902" "LTGR" "RRE" 14
		       "B903",  // 4480 "B903" "LCGR" "RRE" 14
		       "B904",  // 4490 "B904" "LGR" "RRE" 14
		       "B905",  // 4500 "B905" "LURAG" "RRE" 14
		       "B906",  //      "B906" "LGBR" "RRE" 14 Z9-10
		       "B907",  //      "B907" "LGHR" "RRE" 14 Z9-11
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
		       "B926",  //      "B926" "LBR" "RRE" 14 Z9-12
		       "B927",  //      "B927" "LHR" "RRE" 14 Z9-13
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
		       "B983",  //      "B983" "FLOGR" "RRE" 14 Z9-14
		       "B984",  //      "B984" "LLGCR" "RRE" 14 Z9-15
		       "B985",  //      "B985" "LLGHR" "RRE" 14 Z9-16
		       "B986",  // 4870 "B986" "MLGR" "RRE" 14
		       "B987",  // 4880 "B987" "DLGR" "RRE" 14
		       "B988",  // 4890 "B988" "ALCGR" "RRE" 14
		       "B989",  // 4900 "B989" "SLBGR" "RRE" 14
		       "B98A",  // 4910 "B98A" "CSPG" "RRE" 14
		       "B98D",  // 4920 "B98D" "EPSW" "RRE" 14
		       "B98E",  // 4930 "B98E" "IDTE" "RRF2" 34
		       "B990",  // 4940 "B990" "TRTT" "RRE" 14
		       "B991",  // 4950 "B991" "TRTO" "RRE" 14
		       "B992",  // 4960 "B992" "TROT" "RRE" 14
		       "B993",  // 4970 "B993" "TROO" "RRE" 14
		       "B994",  //      "B994" "LLCR" "RRE" 14 Z9-17
		       "B995",  //      "B995" "LLHR" "RRE" 14 Z9-18
		       "B996",  // 4980 "B996" "MLR" "RRE" 14
		       "B997",  // 4990 "B997" "DLR" "RRE" 14
		       "B998",  // 5000 "B998" "ALCR" "RRE" 14
		       "B999",  // 5010 "B999" "SLBR" "RRE" 14
		       "B99A",  // 5020 "B99A" "EPAIR" "RRE" 14
		       "B99B",  // 5030 "B99B" "ESAIR" "RRE" 14
		       "B99D",  // 5040 "B99D" "ESEA" "RRE" 14
		       "B99E",  // 5050 "B99E" "PTI" "RRE" 14
		       "B99F",  // 5060 "B99F" "SSAIR" "RRE" 14
		       "B9AA",  //      "B9AA" "LPTEA" "RRE" 14 Z9-19
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
		       "C01",  //      "C01" "LGFI" "RIL" 16 Z9-20
		       "C04",  // 5180 "C04" "BRCL" "RIL" 16
		       "C040",  // 5390 "C040" "JLNOP" "BLX" 33
		       "C041",  // 5400 "C041" "BROL" "BLX" 33
		       "C041",  // 5410 "C041" "JLO" "BLX" 33
		       "C042",  // 5420 "C042" "BRHL" "BLX" 33
		       "C042",  // 5430 "C042" "BRPL" "BLX" 33
		       "C042",  // 5440 "C042" "JLH" "BLX" 33
		       "C042",  // 5450 "C042" "JLP" "BLX" 33
		       "C044",  // 5460 "C044" "BRLL" "BLX" 33
		       "C044",  // 5470 "C044" "BRML" "BLX" 33
		       "C044",  // 5480 "C044" "JLL" "BLX" 33
		       "C044",  // 5490 "C044" "JLM" "BLX" 33
		       "C047",  // 5500 "C047" "BRNEL" "BLX" 33
		       "C047",  // 5510 "C047" "BRNZL" "BLX" 33
		       "C047",  // 5520 "C047" "JLNE" "BLX" 33
		       "C047",  // 5530 "C047" "JLNZ" "BLX" 33
		       "C048",  // 5540 "C048" "BREL" "BLX" 33
		       "C048",  // 5550 "C048" "BRZL" "BLX" 33
		       "C048",  // 5560 "C048" "JLE" "BLX" 33
		       "C048",  // 5570 "C048" "JLZ" "BLX" 33
		       "C04B",  // 5580 "C04B" "BRNLL" "BLX" 33
		       "C04B",  // 5590 "C04B" "BRNML" "BLX" 33
		       "C04B",  // 5600 "C04B" "JLNL" "BLX" 33
		       "C04B",  // 5610 "C04B" "JLNM" "BLX" 33
		       "C04D",  // 5620 "C04D" "BRNHL" "BLX" 33
		       "C04D",  // 5630 "C04D" "BRNPL" "BLX" 33
		       "C04D",  // 5640 "C04D" "JLNH" "BLX" 33
		       "C04D",  // 5650 "C04D" "JLNP" "BLX" 33
		       "C04E",  // 5660 "C04E" "BRNOL" "BLX" 33
		       "C04E",  // 5670 "C04E" "JLNO" "BLX" 33
		       "C04F",  // 5680 "C04F" "BRUL" "BLX" 33
		       "C04F",  // 5690 "C04F" "JLU" "BLX" 33
		       "C05",  // 5210 "C05" "BRASL" "RIL" 16
		       "C05",  // 5220 "C05" "JASL" "RIL" 16
		       "C06",  //      "C06" "XIHF" "RIL" 16 Z9-21
		       "C07",  //      "C07" "XILF" "RIL" 16 Z9-22
		       "C08",  //      "C08" "IIHF" "RIL" 16 Z9-23
		       "C09",  //      "C09" "IILF" "RIL" 16 Z9-24
		       "C0A",  //      "C0A" "NIHF" "RIL" 16 Z9-25
		       "C0B",  //      "C0B" "NILF" "RIL" 16 Z9-26
		       "C0C",  //      "C0C" "OIHF" "RIL" 16 Z9-27
		       "C0D",  //      "C0D" "OILF" "RIL" 16 Z9-28
		       "C0E",  //      "C0E" "LLIHF" "RIL" 16 Z9-29
		       "C0F",  //      "C0F" "LLILF" "RIL" 16 Z9-30
		       "C24",  //      "C24" "SLGFI" "RIL" 16 Z9-31
		       "C25",  //      "C25" "SLFI" "RIL" 16 Z9-32
		       "C28",  //      "C28" "AGFI" "RIL" 16 Z9-33
		       "C29",  //      "C29" "AFI" "RIL" 16 Z9-34
		       "C2A",  //      "C2A" "ALGFI" "RIL" 16 Z9-35
		       "C2B",  //      "C2B" "ALFI" "RIL" 16 Z9-36
		       "C2C",  //      "C2C" "CGFI" "RIL" 16 Z9-37
		       "C2D",  //      "C2D" "CFI" "RIL" 16 Z9-38
		       "C2E",  //      "C2E" "CLGFI" "RIL" 16 Z9-39
		       "C2F",  //      "C2F" "CLFI" "RIL" 16 Z9-40
		       "C80",  //      "C80" "MVCOS" "SSF" 32 Z9-41
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
		       "E302",  //      "E302" "LTG" "RXY" 18 Z9-42
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
		       "E312",  //      "E312" "LT" "RXY" 18 Z9-43
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
		       "E394",  //      "E394" "LLC" "RXY" 18 Z9-44
		       "E395",  //      "E395" "LLH" "RXY" 18 Z9-45
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
		       "E9",  // 6180 "E9" "PKA" "SS" 31
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
		       "EC45",  // 6610 "EC45" "JXLEG" "RIE" 23
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
		       "ED38",  //      "ED38" "MAYL" "RXF" 25 Z9-46
		       "ED39",  //      "ED39" "MYL" "RXF" 25 Z9-47
		       "ED3A",  //      "ED3A" "MAY" "RXF" 25 Z9-48
		       "ED3B",  //      "ED3B" "MY" "RXF" 25 Z9-49  RPI 298
		       "ED3C",  //      "ED3C" "MAYH" "RXF" 25 Z9-50
		       "ED3D",  //      "ED3D" "MYH" "RXF" 25 Z9-51  RPI 298
		       "ED3E",  // 6960 "ED3E" "MAD" "RXF" 25
		       "ED3F",  // 6970 "ED3F" "MSD" "RXF" 25
		       "ED40", // "SLDT" "RXF" DFP 45
		       "ED41", // "SRDT" "RXF" DFP 46
		       "ED48", // "SLXT" "RXF" DFP 47
		       "ED49", // "SRXT" "RXF" DFP 48
		       "ED50", // "TDCET" "RXE" DFP 49
		       "ED51", // "TDGET" "RXE" DFP 50
		       "ED54", // "TDCDT" "RXE" DFP 51
		       "ED55", // "TDGDT" "RXE" DFP 52
		       "ED58", // "TDCXT" "RXE" DFP 53
		       "ED59", // "TDGXT" "RXE" DFP 54
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
      int last_key_op = 0;
      int key_not_found = 1;
      int key_found = 2;
      int max_key_root = 4000;
      int max_key_tab = 50000;
      int tot_key_tab = max_key_root+1;
      int tot_key = 0;
      char   key_type = '?';
      String key_text = null;
      int key_index = 0;
      int key_index_last = 0;
      int key_hash = 0;
      int tot_key_search = 0;
      int tot_key_comp  = 0;
      int avg_key_comp  = 0;
      int cur_key_comp = 0;
      int max_key_comp = 0;
      char[]    key_tab_type  = (char[])Array.newInstance(char.class,max_key_tab);
      String[]  key_tab_key   = new String[max_key_tab];
      int[]     key_tab_hash  = (int[])Array.newInstance(int.class,max_key_tab);
      int[]     key_tab_index = (int[])Array.newInstance(int.class,max_key_tab);
      int[]     key_tab_low   = (int[])Array.newInstance(int.class,max_key_tab);
      int[]     key_tab_high  = (int[])Array.newInstance(int.class,max_key_tab);
public void init_tables(){
	/*
	 * initialize stared data and tables
	 */
	/*
	 * init starting nanotime for use in timestamps
	 */
	ts_nano_start = System.nanoTime();          // RPI 662
	ts_mic_start  = System.currentTimeMillis(); // RPI 662
	if (opt_install_loc.length() > 0){
		System.setProperty("user.dir",opt_install_loc); // RPI 532
	}
	set_dir_cur();  //RPI168
	init_os();
	init_ascii_ebcdic();
	if (op_name.length != op_type.length){
		abort_error(1,"opcode tables out of sync - aborting");
	}
	int index = 0;
	int max_type = 0;
	int ins_count = 0;
	while (index < op_type.length){
		if (op_type[index] < 100){
			if (op_type[index] > max_type){
				max_type = op_type[index];
			}
			ins_count++;
		}
		index++;
	}
	if (max_type != max_op_type_offset){
		abort_error(2,"opcode max type out of sync - " + max_type + " vs " + max_op_type_offset);
	}
	if (ins_count != op_code.length){
		abort_error(3,"opcode total out of sync - aborting");
	}
    /*
     * find_parm_pattern tokens:
     * skip while space and return next non-white space token
     * */
	try {
	    find_parm_pattern = Pattern.compile(
	    		  "([^\\s]+)"  //RPI 313	    	      
				  );
	} catch (Exception e){
		  abort_error(13,"find parm pattern errror - " + e.toString());
	}
    /*
     * parm_pattern tokens:
     *   1.  ?'     operators such as L', T', etc.
     *   2.  ppp=   parm followed by = for detecting key vs pos
     *   3.  C'xxx' spaces, commas, and '' ok in xxx
     *   4.  'xxx' spaces, commas, and '' ok in xxx
     *   5.  xxx    no spaces or commas in xxx ('s ok)
     *   6.  ,      return to parse sublist and null parms
     *   7.  (      return to parse sublist parm
     *   8.  )      return to parse sublist parm
     *   8.  '      single quotes appended to parm text
     *   9.  space - detect end of parms and comments
     * */
	try {
	    parm_pattern = Pattern.compile(
    		   	    "([a-zA-Z$@#_][a-zA-Z0-9$@#_]*[=])"    // RPI 253
         		  +	"|([cC][aAeE]*[']([^']|(['][']))*['])"  //RPI 270
         		  +	"|([cC][!]([^!]|([!][!]))*[!])" 
         		  + "|([cC][\"]([^\"]|([\"][\"]))*[\"])"
	  	          + "|([']([^']|(['][']))*['])" 
	  	          + "|([diklnstDIKLNST]['])"  // RPI 313 single quote ?' operators
	    		  + "|([^\\s',()+*-/]+)"  //RPI181,342
	    	      + "|([\\s',()+*-/])"    //RPI181,342
				  );
	} catch (Exception e){
		  abort_error(14,"parm pattern errror - " + e.toString());
	}
}
public void init_os(){
	/*
	 * init os dependant variables
	 */
	String os_name = System.getProperty("os.name"); 
	z390_acrobat = System.getenv("Z390ACROBAT");;   // RPI 510
	z390_browser = System.getenv("Z390BROWSER");;   // RPI 510
	z390_command = System.getenv("Z390COMMAND");    // RPI 510
	z390_editor  = System.getenv("Z390EDIT");       // RPI 510
	if  (os_name.substring(0,3).equals("Win")){
		z390_os_type = z390_os_win;        // RPI 499
		if (z390_browser == null
			|| z390_browser.length() == 0){
			z390_browser = "cmd.exe /c Start"; // RPI 500
		}
		if (z390_acrobat == null 
				|| z390_acrobat.length() == 0){
				z390_acrobat = z390_browser;
			}
		if  (z390_command == null 
			|| z390_command.length() == 0){
			if  (os_name.equals("Windows 95")
				|| os_name.equals("Windows 98")){
				z390_command = "command.com" ;
			} else {
				z390_command = "cmd.exe";
			}
		}
		if (z390_editor == null 
		    || z390_editor.length() == 0){
		    z390_editor  = "notepad.exe"; // RPI 500
		}
    } else if (os_name.substring(0,3).equals("Lin")){
    	z390_os_type = z390_os_linux; // RPI 499
		if (z390_browser == null
			|| z390_browser.length() == 0){
			z390_browser = "firefox"; // RPI 500
		}
		if (z390_acrobat == null 
			|| z390_acrobat.length() == 0){
			z390_acrobat = "acroread";
		}
		if  (z390_command == null 
			|| z390_command.length() == 0){
			z390_command = "perl";
		}
		if (z390_editor == null 
		    || z390_editor.length() == 0){
		    z390_editor  = "gedit"; // RPI 500  RPI 532 
		}
    }
}
public void init_options(String[] args,String pgm_type){
	/*
	 * parse and set options
	 * Notes:
	 *   1.  These use () vs = because bat removes =
	 *        syslog(ddname)
	 *        sys390(ddname)
	 *        systerm(filename)
	 *        test(ddname)
	 *        time(seconds)
	 */
    if  (args.length >= 1){
    	if (!set_pgm_dir_name_type(args[0],pgm_type)){
    		abort_error(4,"invalid input file option - " + args[0]);
    	}
    	dir_390 = pgm_dir;
    	dir_bal = pgm_dir;
    	dir_cpy = pgm_dir;
        dir_dat = pgm_dir;
        dir_err = pgm_dir;
        dir_log = pgm_dir;
        dir_lst = pgm_dir;
        dir_mac = pgm_dir;
    	dir_mlc = pgm_dir;
    	dir_obj = pgm_dir;
    	dir_pch = pgm_dir;
    	dir_prn = pgm_dir;
    	dir_trc = pgm_dir;
        if (args.length > 1){
           cmd_parms = args[1];
           int index1 = 2;
           while (index1 < args.length){
            	cmd_parms = cmd_parms.concat(" " + args[index1]);
           	 	index1++;
           }
        }
    } else {
	    abort_error(5,"missing file option");
    }
    String token = null;
    int index1 = 1;
    while (index1 < args.length){
    	token = args[index1];
    	if (token.length() > 2 //RPI201
    		&& token.charAt(0) == '"'
    		&& token.charAt(token.length()-1) == '"'){
    		token = token.substring(1,token.length()-1);
    	}
    	if (token.toUpperCase().equals("AMODE24")){
    		opt_amode24 = true;
    		opt_amode31 = false;
    		z390_amode31 = 'F';
    		z390_rmode31 = 'F';
    	} else if (token.toUpperCase().equals("AMODE31")){
    		opt_amode24 = false;
    		opt_amode31 = true;
    		z390_amode31 = 'T';
    	} else if (token.toUpperCase().equals("ASCII")){
    		opt_ascii = true; 
    	} else if (token.toUpperCase().equals("ASM")){
    		opt_asm = true; 
    	} else if (token.toUpperCase().equals("BAL")){
    		opt_bal = true; 
    	} else if (token.toUpperCase().equals("BS2000")){
    		opt_bs2000 = true;  // RPI 604
    		opt_amode24 = true;
    		opt_amode31 = false;
    		z390_amode31 = 'F';
    	} else if (token.toUpperCase().equals("CICS")){
           	opt_cics = true;
    	} else if (token.toUpperCase().equals("CON")){
           	opt_con = true;
        } else if (token.toUpperCase().equals("DUMP")){
           	opt_dump = true;
        } else if (token.length() > 4
        	&& token.substring(0,4).toUpperCase().equals("ERR(")){
           	try {
           		max_errors = Integer.valueOf(token.substring(4,token.length()-1)).intValue(); 
          	} catch (Exception e){
           		abort_error(6,"invalid error limit - " + token);
           	}
        } else if (token.toUpperCase().equals("ERRSUM")){
           	init_errsum();
        } else if (token.toUpperCase().equals("GUAM")){
           	opt_guam = true;
        } else if (token.length() > 4
         		&& token.substring(0,4).toUpperCase().equals("IPL(")){
        	opt_ipl = token.substring(4,token.length()-1); 
        } else if (token.length() > 8
         		&& token.substring(0,8).toUpperCase().equals("INSTALL(")){
        	opt_install_loc = token.substring(8,token.length()-1); 	
        } else if (token.length() >= 8
          		&& token.substring(0,8).toUpperCase().equals("LISTCALL")){
           	opt_listcall = true;
        } else if (token.length() >= 7
          		&& token.substring(0,7).toUpperCase().equals("LISTUSE")){
           	opt_listuse = true;
        } else if (token.length() > 8
          		&& token.substring(0,8).toUpperCase().equals("MAXCALL(")){
           	opt_maxcall = Integer.valueOf(token.substring(8,token.length()-1)).intValue(); 
        } else if (token.length() > 7
          		&& token.substring(0,7).toUpperCase().equals("MAXESD(")){
           	opt_maxesd = Integer.valueOf(token.substring(7,token.length()-1)).intValue();   	
        } else if (token.length() > 8
        	&& token.substring(0,8).toUpperCase().equals("MAXFILE(")){
           	try {
           		opt_maxfile = Integer.valueOf(token.substring(8,token.length()-1)).intValue();
           	} catch (Exception e){
           		abort_error(7,"invalid maxfile limit (mb) - " + token);
           	}
        } else if (token.length() > 7
          		&& token.substring(0,7).toUpperCase().equals("MAXGBL(")){
           	opt_maxgbl = Integer.valueOf(token.substring(7,token.length()-1)).intValue();
        } else if (token.length() > 7
          		&& token.substring(0,7).toUpperCase().equals("MAXLCL(")){
           	opt_maxlcl = Integer.valueOf(token.substring(7,token.length()-1)).intValue(); 
        } else if (token.length() > 8
          		&& token.substring(0,8).toUpperCase().equals("MAXLINE(")){
           	opt_maxline = Integer.valueOf(token.substring(8,token.length()-1)).intValue(); 
        } else if (token.length() > 8
          		&& token.substring(0,8).toUpperCase().equals("MAXPARM(")){
           	opt_maxparm = Integer.valueOf(token.substring(8,token.length()-1)).intValue(); 
        } else if (token.length() > 6
          		&& token.substring(0,6).toUpperCase().equals("MAXPC(")){ // RPI 439
           	opt_maxpc = Integer.valueOf(token.substring(6,token.length()-1)).intValue();
        } else if (token.length() > 7
          		&& token.substring(0,7).toUpperCase().equals("MAXRLD(")){
           	opt_maxrld = Integer.valueOf(token.substring(7,token.length()-1)).intValue();  
        } else if (token.length() > 8
            	&& token.substring(0,8).toUpperCase().equals("MAXSIZE(")){
               	try {
               		max_file_size = Long.valueOf(token.substring(8,token.length()-1)).longValue() << 20; 
               	} catch (Exception e){
               		abort_error(8,"invalid maxsize limit (mb) - " + token);
               	}
        } else if (token.length() > 7
          		&& token.substring(0,7).toUpperCase().equals("MAXSYM(")){
           	opt_maxsym = Integer.valueOf(token.substring(7,token.length()-1)).intValue(); 
        } else if (token.length() >= 5
          		&& token.substring(0,5).toUpperCase().equals("MCALL")){
           	opt_mcall = true; // RPI 511
           	opt_listcall = true;
        } else if (token.length() > 5
        	&& token.substring(0,4).toUpperCase().equals("MEM(")){
           	try {
           	    max_mem = Integer.valueOf(token.substring(4,token.length()-1)).intValue();
           	} catch (Exception e){
           		abort_error(9,"invalid memory option " + token);
           	}
        } else if (token.toUpperCase().equals("NOASM")){
           	opt_asm = false;
        } else if (token.toUpperCase().equals("NOBAL")){
           	opt_bal = false;    	
        } else if (token.toUpperCase().equals("NOCON")){
           	opt_con = false;
        } else if (token.toUpperCase().equals("NOEPILOG")){
           	opt_epilog = false;   	
        } else if (token.toUpperCase().equals("NOLIST")){
           	opt_list = false;
        } else if (token.toUpperCase().equals("NOLISTCALL")){
           	opt_listcall = false;
        } else if (token.equals("NOLISTFILE")){
           	opt_listfile = false;
        } else if (token.equals("NOLISTUSE")){
           	opt_listuse = false; 
        } else if (token.toUpperCase().equals("NOOBJ")){ // RPI694
           	opt_obj = false;
        } else if (token.toUpperCase().equals("NOPC")){
            opt_pc = false; 
        } else if (token.toUpperCase().equals("NOPCOPT")){
            opt_pcopt = false;
        } else if (token.toUpperCase().equals("NOPROLOG")){
            opt_prolog = false;
        } else if (token.toUpperCase().equals("NOPROTECT")){
            opt_protect = false;
        } else if (token.toUpperCase().equals("NOSTATS")){
           	opt_stats = false;
        } else if (token.toUpperCase().equals("NOTIME")){
        	opt_time = false; // no time limit
        } else if (token.toUpperCase().equals("NOTIMING")){
          	opt_timing = false; // no date/time changes
          	opt_time   = false;
        } else if (token.toUpperCase().equals("NOTRAP")){
           	opt_trap = false;
        } else if (token.toUpperCase().equals("NOXREF")){
           	opt_xref = false;
        } else if (token.toUpperCase().equals("OBJHEX")){
           	opt_objhex = true;
        } else if (token.length() > 5
           		&& token.substring(0,5).toUpperCase().equals("PARM(")){
            	opt_parm = token.substring(5,token.length()-1);
            	if (opt_parm.length() > 2 
            		&& opt_parm.charAt(0) == '\''
            		&& opt_parm.charAt(opt_parm.length()-1) == '\''){
            		opt_parm = opt_parm.substring(1,opt_parm.length()-1); 		
            	}
        } else if (token.toUpperCase().equals("PC")){
            opt_pc = true;
        } else if (token.toUpperCase().equals("PCOPT")){
            opt_pcopt = true;
        } else if (token.length() > 8
          		&& token.substring(0,8).toUpperCase().equals("PROFILE(")){
         	opt_profile = token.substring(8,token.length()-1);
        } else if (token.toUpperCase().equals("REFORMAT")){
            	opt_reformat = true; 
        } else if (token.toUpperCase().equals("REGS")){
           	opt_regs = true;
           	opt_list  = true;
        } else if (token.toUpperCase().equals("RMODE24")){
           	opt_rmode24 = true;
           	opt_rmode31 = false;
           	z390_rmode31 = 'F';
        } else if (token.toUpperCase().equals("RMODE31")){
           	opt_rmode24 = false;
          	opt_rmode31 = true;
           	z390_rmode31 = 'T';
        } else if (token.length() > 7
           		&& token.substring(0,7).toUpperCase().equals("SYS390(")){
           	dir_390 = token.substring(7,token.length()-1) + File.separator;	
        } else if (token.length() > 7 
           		&& token.substring(0,7).toUpperCase().equals("SYSBAL(")){
          	dir_bal = token.substring(7,token.length()-1) + File.separator; 
        } else if (token.length() > 7 
          		&& token.substring(0,7).toUpperCase().equals("SYSCPY(")){
           	dir_cpy = token.substring(7,token.length()-1); 
        } else if (token.length() > 7 
          		&& token.substring(0,7).toUpperCase().equals("SYSDAT(")){
           	dir_dat = token.substring(7,token.length()-1) + File.separator; 
        } else if (token.length() > 7
           		&& token.substring(0,7).toUpperCase().equals("SYSERR(")){
            	dir_err = token.substring(7,token.length()-1) + File.separator; // RPI 243 
        } else if (token.length() > 7
          		&& token.substring(0,7).toUpperCase().equals("SYSLOG(")){
           	dir_log = token.substring(7,token.length()-1) + File.separator;
        } else if (token.length() > 7 
           		&& token.substring(0,7).toUpperCase().equals("SYSMAC(")){
           	dir_mac = token.substring(7,token.length()-1);  
        } else if (token.length() > 7 
           		&& token.substring(0,7).toUpperCase().equals("SYSMLC(")){
          	dir_mlc = get_short_file_name(token.substring(7,token.length()-1) + File.separator); 
        } else if (token.length() > 7 
           		&& token.substring(0,7).toUpperCase().equals("SYSOBJ(")){
           	dir_obj = token.substring(7,token.length()-1) + File.separator; 
        } else if (token.length() > 8
         		&& token.substring(0,8).toUpperCase().equals("SYSPARM(")){
        	opt_sysparm = token.substring(8,token.length()-1); 
        } else if (token.length() > 7 
           		&& token.substring(0,7).toUpperCase().equals("SYSPCH(")){
          	dir_pch = get_short_file_name(token.substring(7,token.length()-1) + File.separator); 
        } else if (token.length() > 7 
          		&& token.substring(0,7).toUpperCase().equals("SYSPRN(")){
          	dir_prn = token.substring(7,token.length()-1) + File.separator; 	
        } else if (token.length() > 8
          		&& token.substring(0,8).toUpperCase().equals("SYSTERM(")){
         	opt_systerm = token.substring(8,token.length()-1);
        } else if (token.length() > 7 
           		&& token.substring(0,7).toUpperCase().equals("SYSTRC(")){
          	dir_trc = token.substring(7,token.length()-1) + File.separator; 
        } else if (token.length() > 5
          		&& token.substring(0,5).toUpperCase().equals("TIME(")){
           	max_time_seconds = Long.valueOf(token.substring(5,token.length()-1)).longValue();
           	if (max_time_seconds > 0){
           		opt_time = true;
           		opt_timing = true;
           	} else {
           		opt_time = false;
           		opt_timing = false;
           	}
        } else if (token.toUpperCase().equals("TEST")){
           	opt_test = true;
           	opt_time = false;
           	opt_con  = true;
        } else if (token.length() > 5
          		&& token.substring(0,5).toUpperCase().equals("TEST(")){
           	test_ddname = token.substring(5,token.length()-1);	
           	opt_test = true;
        } else if (token.toUpperCase().equals("TRACE")){
           	opt_trace = true;
           	opt_tracet = true;
           	opt_list   = true;
           	if (!opt_test){
           		opt_con   = false; // RPI 569 leave on if TEST
           	}
        } else if (token.toUpperCase().equals("TRACEA")){
           	opt_tracea = true;
           	opt_list = true;
           	opt_con   = false;
        } else if (token.toUpperCase().equals("TRACEALL")){
           	opt_traceall = true;
           	opt_trace    = true;
           	opt_tracem   = true;
           	opt_tracep   = true;
           	opt_tracea   = true;
           	opt_tracel   = true;
           	opt_tracet   = true;
           	opt_tracemem = true;
           	opt_list     = true;
           	opt_con   = false;
        } else if (token.toUpperCase().equals("TRACEL")){
           	opt_tracel = true;
           	opt_list = true;
           	opt_con   = false;
        } else if (token.toUpperCase().equals("TRACEM")){
            	opt_tracem = true;
            	opt_list = true;
            	opt_con   = false;
        } else if (token.toUpperCase().equals("TRACEMEM")){
           	opt_tracemem = true;
           	opt_con   = false;
        } else if (token.toUpperCase().equals("TRACEP")){
        	opt_tracep = true;
        	opt_tracem = true;
        	opt_list = true;
        	opt_con   = false;
        } else if (token.toUpperCase().equals("TRACEQ")){
        	opt_traceq = true;
        	opt_con   = false;
        } else if (token.toUpperCase().equals("TRACET")){
        	opt_tracet = true;
        	opt_con   = false;
        } else if (token.toUpperCase().equals("TRACEV")){
        	opt_tracev = true;
        	opt_con   = false;
        } else if (token.toUpperCase().equals("TS")){
        	opt_ts = true; // timestamp traces
        }
        index1++;
    }
    if (opt_systerm.length() == 0){  // RPI 425 RPI 546
    	opt_systerm = pgm_name; // RPI 546
    }
}
public void open_systerm(String z390_pgm){
	/*
	 * open systerm file else set null
	 */
	systerm_prefix = left_justify(pgm_name,9) + " " + z390_pgm + " ";
    if (systerm_file != null)return; // rpi 415
	systerm_file_name = get_file_name(dir_err,opt_systerm,err_type);
    try {
        systerm_file = new RandomAccessFile(systerm_file_name,"rw"); 
        systerm_file.seek(systerm_file.length());
    } catch (Exception e){
    	systerm_file = null;
    	abort_error(10,"systerm file open error " + e.toString());
    }
	if (opt_timing){
		systerm_start = System.currentTimeMillis();
        systerm_time = sdf_HHmmss.format(new Date()) + " ";
        job_date = sdf_MMddyy.format(new Date());
	} else {
		job_date = "MM/DD/YY";
	}
	try {
		systerm_io++;
		systerm_file.writeBytes(systerm_time + systerm_prefix + "STARTED" + newline); // RPI 500
	} catch (Exception e){
        abort_error(11,"I/O error on systerm file " + e.toString());
	}
}
public synchronized void put_systerm(String msg){ // RPI 397
	/*
	 * log error to systerm file
	 */
	if (opt_timing){
           systerm_time = sdf_HHmmss.format(new Date()) + " ";;
	}
	if (systerm_file != null){
		try {
			systerm_io++;
			systerm_file.writeBytes(systerm_time + systerm_prefix + msg + newline); // RPI 500
		} catch (Exception e){
	        abort_error(12,"I/O error on systerm file " + e.toString());
		}
	}
}
public synchronized void close_systerm(int rc){ // RPI 397
	/*
	 * close systerm error file if open
	 * 
	 */
     if (systerm_file != null){
     	 if (opt_timing){
     		systerm_sec  = " SEC=" + right_justify("" + ((System.currentTimeMillis()-systerm_start)/1000),2);
    	    systerm_time = sdf_HHmmss.format(new Date()) + " ";;
    	 }
    	 try {
    		 systerm_io++;
    		 String systerm_ins_text = "";
    		 if (systerm_ins > 0){
    			 systerm_ins_text = " INS=" + systerm_ins;
    		 }
    		 systerm_file.writeBytes(systerm_time + systerm_prefix
    				 + "ENDED   RC=" + right_justify("" + rc,2) 
    				 + systerm_sec 
    				 + " MEM(MB)=" + right_justify("" + get_mem_usage(),3) 
    				 + " IO=" + systerm_io 
    				 + systerm_ins_text + newline); // RPI 500
    	 } catch (Exception e){
    	 }
    	 try {
    		 systerm_file.close();
    	 } catch (Exception e){
    		 System.out.println("TZ390E systerm file close error - " + e.toString());
    	 }
    	 systerm_file = null;  // RPI 622
     }
}
public void close_trace_file(){
	/*
	 * close trace file if open RPI 484
	 */
     if (trace_file_buff != null){
    	 try {
    		 trace_file_buff.close();
    	 } catch (Exception e){
    		 abort_error(15,"trace file close failed " + e.toString());
    	 }
     }
}
private int get_mem_usage(){
	/*
	 * return max memory usage by J2SE in MB
	 */
	long mem_tot = 0;
    List<MemoryPoolMXBean> pools = ManagementFactory.getMemoryPoolMXBeans();
    for (MemoryPoolMXBean p: pools) {
    	 mem_tot = mem_tot + p.getPeakUsage().getUsed();
    }
    return (int)(mem_tot >> 20);
}
private String get_short_file_name(String file_name){
	/*
	 * return shortest file name possible
	 * with quotes if LSN
	 */
	if (file_name.length() > dir_cur.length()
		&& file_name.substring(0,dir_cur.length()).equals(dir_cur)){
		if (file_name.substring(dir_cur.length(),dir_cur.length()+1).equals(File.separator)){
			file_name = file_name.substring(dir_cur.length()+1); // skip dir + sep
		} else {
			file_name = file_name.substring(dir_cur.length()); // skip dir
		}
	}
	int index = file_name.indexOf(" ");
	if (index >=0){
		return "\"" + file_name + "\""; // LSN
	}
	return file_name;
}
public synchronized void abort_error(int error,String msg){ // RPI 397
	/*
	 * display options error on system out
	 * and exit with rc 16.
	 */
	System.out.println("TZ390E abort error " + error + " - " + msg);
    close_systerm(16);
	System.exit(16);
}

private void init_ascii_ebcdic(){
	/*
	 * init ascii/ebcdic conversion tables
	 */	
    int index = 0;
	while (index < 256){
	  ascii_to_ebcdic[index] = (byte) Integer.valueOf(ascii_to_ebcdic_hex.substring(index*2,index*2+2),16).intValue();
	  ebcdic_to_ascii[index] = (byte) Integer.valueOf(ebcdic_to_ascii_hex.substring(index*2,index*2+2),16).intValue();
	  index++;
	}
}
public int find_key_index(char user_key_type,String user_key){
	/*
	 * return user_key_index for user_key else -1
	 * and set following for possible add_key_index:
	 *    1.  key_text = user_key
	 *    2.  key_hash = hash code for key
	 *    3.  key_index_last = last search entry
	 * Notes:
	 *   1.  Usage my mz390
	 *       a.  "A:" - ago gbla table pointer
	 *       a.  "F:" - macro and copybook files
	 *       b.  "G:" - global set variables
	 *       e.  "M:" - loaded macros
	 *       f.  "O:" - opcode table (init_opcode_name_keys)
	 *       h.  "R:" - opcode and macro opsyn
	 *       i.  "S:" - ordinary symbols
	 *       j.  "X:" - executable macro command
	 *   2.  Usage by az390
	 *       a.  "L:" - literals
	 *       b.  "O:" - opcode table (init_opcode_name_keys)
	 *       c.  "R:" - opcode opsyn
	 *       d.  "S:" - ordinary symbols
	 *       e.  "U:" - USING labels
	 *   3.  Usage by lz390
	 *       a.  "G:" - global ESD's
	 *   4.  Usage by ez390
	 *       a.  "H:" - opcodes by hex key
	 *       b.  "H:BR:" - branch opocodes by hex key
	 *       c.  "O:" - opcodes by name (init_opcode_name_keys)
	 *       d.  "P:" - CDE program name lookup
	 *       e.  "R:" - OPSYN opcode/macro substitution
	 *   5.  See find_lcl_key_index in mz390 with
	 *       local key types KBPL
	 *   6.  Optimize by using separate user_key_type char
	 *       to avoid extra string concat and avoid string compare if not 
	 *       desired type.  RPI 409 (all calls changed)
	 */
	tot_key_search++;
	key_type = user_key_type;
	key_text = user_key;
    key_hash  = key_text.hashCode(); // RPI 434 
	key_index = Math.abs(key_hash % max_key_root)+1; 
	if (key_tab_key[key_index] == null){
		key_index_last = key_index;
		last_key_op = key_not_found;
		return -1;
	}
    cur_key_comp = 0;
	while (key_index > 0){ 
		tot_key_comp++;
		cur_key_comp++;
		if (key_hash == key_tab_hash[key_index]
		    && user_key_type == key_tab_type[key_index]                           
		    && user_key.equals(key_tab_key[key_index])){
			if (cur_key_comp > max_key_comp){
				max_key_comp = cur_key_comp;
			}
			last_key_op = key_found;
	    	return key_tab_index[key_index];
	    }
		key_index_last = key_index;
		if (key_hash < key_tab_hash[key_index]){
		    key_index = key_tab_low[key_index];
		} else {
			key_index = key_tab_high[key_index];
		}
	}
	if (cur_key_comp > max_key_comp){
		max_key_comp = cur_key_comp;
	}
	last_key_op = key_not_found;
	return -1;
}
public boolean add_key_index(int user_index){
	/*
	 * add user_index entry based on
	 * key_text, key_hash, and key_index_last
	 * set by prior find_key_index
	 * 
	 */
	if (last_key_op != key_not_found){
		return false;
	}
	if (key_tab_key[key_index_last] == null){
		key_index = key_index_last;
	} else {
		if (tot_key_tab < max_key_tab){
			key_index = tot_key_tab;
			tot_key_tab++;
		} else {
			return false;  // table size exceeded
		}
		if (key_hash < key_tab_hash[key_index_last]){
		    key_tab_low[key_index_last] = key_index;
	    } else {
		    key_tab_high[key_index_last] = key_index;
	    }
	}
	tot_key++;
	key_tab_type[key_index]  = key_type;
	key_tab_key[key_index]   = key_text;
	key_tab_hash[key_index]  = key_hash;
	key_tab_index[key_index] = user_index;
	return true;
}
public boolean update_key_index(int user_key){
	/*
	 * update previously found key index
	 */
	if (last_key_op != key_found){
		return false;
	}
	key_tab_index[key_index] = user_key;
	return true;
}
public String get_file_name(String parm_dir,String parm,String parm_type){
	   /*
	    * 1.  Strip long spacey name quotes if found
	    * 2.  Add directory and type if not specified
	    * 3.  Replace \ with / if Linux
	    */
	        if (z390_os_type == z390_os_linux){ // RPI 532 file separator fix
	        	parm_dir = parm_dir.replace('\\','/');
	        	parm     = parm.replace('\\','/');
	        }
	   	    String file_name = null;
	    	if (parm.charAt(0) == '\"' 
	    		|| parm.charAt(0) == '\''){
	    		file_name = parm.substring(1,parm.length() - 1);
	    	} else {
	    	    file_name = parm;
	    	}
	    	if  (parm_dir.length() > 0 && !parm_dir.substring(parm_dir.length()-1).equals(File.separator)){
	    		parm_dir = parm_dir.concat(File.separator); // RPI 508
	    	}
	    	if  (file_name.length() > 0
	            && !file_name.substring(0,1).equals(File.separator) // RPI 532 
	    		&& file_name.indexOf(':')  == -1
	    		){
	    		if (file_name.indexOf('\\') != -1 // RPI 546
	    			|| file_name.indexOf('/') != -1){
	    			file_name = dir_cur.concat(file_name);
	    		} else {
	    			file_name = parm_dir.concat(file_name);	 // RPI 508
	    		}
	    	}
	    	int index = file_name.indexOf(".");
	    	if (index == -1){
	    		file_name = file_name.trim() + parm_type;
	    	}
	    	return file_name;
}
public String find_file_name(String parm_dir_list, String file_name, String file_type_def, String dir_cur){
	/*
	 * search for existing file in one or more dirs
	 * and return file name or null if not found
	 * Note:
	 *   1.  The separator for multiple files may be ; or +
	 *       (plus sign) verus semi-colon is used in BAT parms 
	 *       to avoid conflict with Windows BAT parsing.
	 *   2.  If file_name has type use it.
	 *       Else if directory path has *.type use
	 *       the type instead of default file_type. 
	 */
	if (file_name == null)return null; // RPI 459
	String file_dir;
	String file_type;
	String temp_file_name;
	File   temp_file;
	int index = file_name.indexOf('.');
	boolean file_type_set = false;
	if (index > 0){
		file_type_set = true;
	}
	index = 0;
	int path_len = 0;  
	while (index <= parm_dir_list.length()){
		file_type = file_type_def;
		int index1 = parm_dir_list.substring(index).indexOf(";");
		if (index1 == -1)index1 = parm_dir_list.substring(index).indexOf("+");
		if (index1 > 0){
			path_len = path_len + index1;   
			file_dir = parm_dir_list.substring(index,path_len); // RPI123
			index = index + index1 + 1;
			path_len = path_len + 1;    
		} else {
			file_dir = parm_dir_list.substring(index);
			index = parm_dir_list.length()+1;
		}
		if (file_dir.equals(".")){
			file_dir = dir_cur;
		}
		int index2 = file_dir.indexOf("*.");
		if (index2 > 0){
			file_type = file_dir.substring(index2+1);
			file_dir  = file_dir.substring(0,index2);
		}
		if (file_dir.length() > 0){
			if (!file_dir.substring(file_dir.length()-1,file_dir.length()).equals(File.separator)){
				file_dir = file_dir + File.separator;
			}
			if (file_type_set){
				temp_file_name = file_dir + file_name;
			} else {
				temp_file_name = file_dir + file_name + file_type;
			}
			temp_file = new File(temp_file_name);
		} else {
			temp_file = new File(file_name + file_type);
		}
		if (temp_file.isFile()){
			return temp_file.getPath(); // RPI 499 drop upper case
		}
	}
	return null;
}
public boolean exec_cmd(String cmd){
     /*
      * exec command as separate task
      */
           try {
  	           Runtime.getRuntime().exec(cmd);
  	           return true;
  	       } catch(Exception e){
  	   	       return false;
  	       }
  	  }
public boolean init_opcode_name_keys(){
	/*
	 * add all opcodes to key index table
	 */
	int index = 0;
	while (index < op_name.length){
		if (find_key_index('O',op_name[index]) == -1){
			if(!add_key_index(index)){ 
				return false;
			}
		} else {
			return false;
		}
		index++;
	}
	return true;
}
public boolean set_pgm_dir_name_type(String file_name,String file_type){
	/*
	 * set pgm_dir, pgm_name, pgm_type from parm 
	 * Notes:
	 *   1.  Only allow file type override for MLC.
	 */
	set_dir_cur(); //RPI168
	if (file_name.charAt(0) == '\"'   // strip lsn quotes
		|| file_name.charAt(0) == '\''){
		file_name = file_name.substring(1,file_name.length() - 1);
	}
    int index = file_name.lastIndexOf(File.separator);
    if (index != -1){  // get dir path if any
    	pgm_dir = file_name.substring(0,index+1);
    	file_name = file_name.substring(index + 1); // RPI 499 drop upper case
    } else if (file_name.length() > 1 && file_name.charAt(1) == ':'){
    	File temp_file = new File(file_name.substring(0,2));
    	try {
    		pgm_dir = temp_file.getCanonicalPath() + File.separator;
    	} catch (Exception e){
    		return false;
    	}
    	file_name = file_name.substring(2); //RPI113
    } else {
    	pgm_dir = dir_cur;
	  	// RPI 499 drop upper case file_name = file_name.toUpperCase();
    }
    index = file_name.lastIndexOf('.');
    if (index != -1){  // strip extension if any
    	pgm_name = file_name.substring(0,index);
    	if (!file_type.equals(mlc_type)){ //RPI169
    		pgm_type=file_type;
    	} else {
    		pgm_type = file_name.substring(index);
    	}
    } else {
     	pgm_name = file_name;
     	pgm_type = file_type;
    }
    return true;
}
private void set_dir_cur(){  //RPI168
	/*
	 * set current directory dir_cur
	 */
	dir_cur = System.getProperty("user.dir") + File.separator; // RPI 499 drop upper case
}
public void reset_opsyn(){
	/*
	 * reset op_code key table indexes changed
	 * by opsyn during previous pass if any.
	 */
	int index = 0;
	while (index < tot_opsyn){
		opsyn_old_name[index] = opsyn_new_name[index]; // RPI 403
		index++;
	}
}
public boolean update_opsyn(String new_name,String old_name){
	/*
	 * Update opsyn table as follows:
	 *   1.  Add new alias name for opcode
	 *   2.  Add null entry to cancel opcode  // RPI 306
	 *   3.  Restore opcode to previous alias
	 *       and remove any cancel entry.  // R{O 404
	 * Notes:
	 *   1.  Indexes pointing to new name entries
	 *       in opsyn table are only added once.
	 *   2,  az390 uses reset_opsyn() to reset old = new
	 *       for multiple passes so opcodes prior to first
	 *       OPSYN statement will map to std. opcode. mz390
	 *       only makes one pass so its not an issue.     
	 */
	int index = -1;
	if (old_name != null){
		index = old_name.indexOf(" ");
		if (index > 0){  // RPI 306 remove comments
			old_name = old_name.substring(0,index).toUpperCase();
		}
		if (old_name.length() == 0 || old_name.charAt(0) == ','){
			old_name = null;
		}
	}
	if (new_name == null || new_name.length() == 0){
		return false;
	}
	new_name = new_name.toUpperCase();
	opsyn_index = find_key_index('R',new_name);
	if (opsyn_index == -1){
		// defining new alias
		if (tot_opsyn < max_opsyn){
			opsyn_index = tot_opsyn;
			tot_opsyn++;
			add_key_index(opsyn_index);
			opsyn_new_name[opsyn_index] = new_name;
		} else {
			return false;
		}
	}
	if (old_name != null){
		index = find_key_index('R',
				old_name);
        if (index != -1){
    		// replace old name with any
        	// previously saved opcode
			old_name = opsyn_old_name[index];
		}
		// save new and old opcodes
		opsyn_old_name[opsyn_index] = old_name;
	} else {
		opsyn_old_name[opsyn_index] = null; // RPI 331
	}
	return true;
}
public String get_hex(int work_int,int req_hex_digits) {
   	/*
   	 * Format int into 1-16 hex digit string
   	 */
   	    String work_hex = Integer.toHexString(work_int);
   	    if (req_hex_digits <= 8 || (work_int >= 0 && req_hex_digits <= 16)){
   			return ("0000000000000000" + work_hex).substring(work_hex.length() + 16 - req_hex_digits).toUpperCase();
   	    } else if (req_hex_digits >= 16 && work_int < 0){
   	    	return ("FFFFFFFFFFFFFFFF" + work_hex).substring(work_hex.length() + 16 - req_hex_digits).toUpperCase();
   	    } else {
   	    	return null; // force error
   	    }
}
public String get_long_hex(long work_long,int req_hex_digits) {
   	/*
   	 * Format long into 1-16 hex digit string
   	 */
   	    String work_hex = Long.toHexString(work_long);
   	    if (req_hex_digits <= 16) {
   			return ("0000000000000000" + work_hex).substring(work_hex.length() + 16 - req_hex_digits).toUpperCase();
   	    } else {
   	    	return null; // force error
   	    }
}
public boolean get_sdt_char_int(String sdt){
	   /*
	    *  set sdt_char_int to
	    *  value of character string else false
	    *  
	    *  C'....' EBCDIC/ASCII (rep ''|&& with'|&)
	    *  C"...." ASCII        (rep ""|''|&& with "|'|&)
	    *  C!....! EBCDIC       (rep !!|''|&& with !|'|&) 
	    *  CA'...' ASCII
	    *  CE'...' EBCDIC
	    */
	   boolean ebcdic = true;
	   int index = 2;
	   sdt_char_int = 0;
	   char sdt_quote = '\'';
	   char char_type = sdt.substring(1,2).toUpperCase().charAt(0); 
	   switch (char_type){
	   case 'A': // ASCII
		   index = 3;
		   ebcdic = false;
		   break;
	   case 'E': // EBCDIC
		   index = 3;
		   break;
	   case '\'': // ASCII or EBCDIC based on opt_ASCII
	       if (opt_ascii){
	    	   ebcdic = false;
	       }
	       break;
	   case '"': // ASCII
		   sdt_quote = '"';
		   ebcdic = false;
		   break;
	   case '!': // EBCDIC
		   sdt_quote = '!';
		   break;
	   }
	   while (index < sdt.length()-1){
		   if (sdt.charAt(index) == sdt_quote
				|| sdt.charAt(index) == '\''
				|| sdt.charAt(index) == '&'){  // RPI 274
			   if (index + 1 < sdt.length()-1
				   && sdt.charAt(index+1) == sdt.charAt(index)){
				   index++;
			   }
		   }
		   if (!ebcdic){ //RPI5  RPI 270
			   sdt_char_int = (sdt_char_int << 8) + (sdt.charAt(index) & 0xff);
		   } else {
			   sdt_char_int = (sdt_char_int << 8) + (ascii_to_ebcdic[sdt.charAt(index)] & 0xff);
		   }
		   if (index+1 == sdt.length()
			   && sdt.charAt(index) != sdt_quote){
			   return false;
		   }
	       index++;
	   }
	   return true;
}
public boolean verify_ascii_source(String temp_line){
	/*
	 * 1.  Verify ascii source code and
	 *     length <= 80 
	 *
	 */
	if (temp_line.length() > max_line_len){ // RPI 437
		return false; 
	}
	int index = 0;
    while (index < temp_line.length()){
    	int next_char = temp_line.charAt(0) & 0xff;
        if (next_char != 9  // RPI 302
        	&& next_char != '.' 
        	&& ascii_table.charAt(next_char) == '.'
        	){
        	return false;
        }
        index++;
    }
    return true;
}
public String left_justify(String text,int padded_len){
	/*
	 * return text left justified in field
	 * if field larger than text
	 */
	if (text == null){
		return "";
	}
	int pad_len = padded_len - text.length();
	if (pad_len > 0){
		if (pad_len > pad_spaces_len){
	        init_pad_spaces(pad_len);
		}
		return text + String.valueOf(pad_spaces,0,pad_len);
	} else {
		return text;
	}
}
public String right_justify(String text,int padded_len){
	/*
	 * return text right justified in field
	 * if field larger than text
	 */
	int pad_len = padded_len - text.length();
	if (pad_len > 0){
		if (pad_len > pad_spaces_len){
           init_pad_spaces(pad_len);
		}
		return String.valueOf(pad_spaces,0,pad_len) + text;
	} else {
		return text;
	}
}
private void init_pad_spaces(int new_pad_len){
	/*
	 * initialize new pad_spaces byte array
	 * used by left and right justify
	 */
	pad_spaces_len = new_pad_len;
	if (pad_spaces_len < 4096){
		pad_spaces_len = 4096;
	}
    pad_spaces = new char[pad_spaces_len];
    Arrays.fill(pad_spaces,0,pad_spaces_len,' ');
}
public String get_dup_string(String text,int dup_count){
	/*
	 * return string with text dupicated
	 * dup_count times
	 */
	if (dup_char_len < dup_count){
		dup_char_len = dup_count;
		if (dup_char_len < 4096){
			dup_char_len = 4096;
		}
		dup_char = new char[dup_char_len];
	}
	int tot_char = text.length() * dup_count;
	if (text.length() == 1){
		Arrays.fill(dup_char,0,dup_count,text.charAt(0));
	} else {
		int rep = 0;
		while (rep < dup_count){  // RPI 620
			System.arraycopy(text.toCharArray(),0,dup_char,rep * text.length(),
					text.length());
			rep++;
		}
	}
	return String.valueOf(dup_char,0,tot_char);
}
public String trim_trailing_spaces(String line,int max_text){ // RPI 437
	/*
	 * remove trailing spaces from non-continued
	 * source line
	 */
	if (max_text > 0 && line.length() > max_text){
	    return ("X" + line.substring(0,max_text)).trim().substring(1);  //RPI124
	} else {
		return ("X" + line).trim().substring(1);
	}
}
public String trim_continue(String line, boolean first_line){
	/*
     * Trim line to comma delimiter or end of line
     * recognizing whether line is continuation of 
     * quoted string or not..
	 * Notes:
	 *   1.  Allows ", " to appear in quotes
	 *       which may be split across lines.
	 *   2.  Allow spaces within (...) on macro
	 *       statements but not opcodes
	 *   3.  Handle quoted string continued on one
	 *       or more continuation lines. RPI 463.
	 *   4.  Remove leading spaces from continuations.    
	 */
	int index;
	int eol_index = line.length();
	if (eol_index >= 72){
		eol_index = 71; // RPI 315
	}
	if (first_line){
		split_level = 0;
		split_quote = false;      // RPI115
		split_quote_text = "";
		split_quote_last = false; // RPI 463
		if (line.charAt(0) == '*'){
			split_comment = true;
			return line.substring(0,eol_index); // RPI 313 don't look in comments
		} else {
			split_comment = false;
		}
		split_line(line);
		if (split_op != null){
			split_op_index = find_key_index('O',split_op.toUpperCase());
		    if (split_op_index >= 0){
		    	split_op_type = op_type[split_op_index];
		    } else {
		    	split_op_type = -1;
		    }
		    if (split_parms_index != -1){
		    	split_quote_text = line.substring(0,split_parms_index);
		    }
		} else {
			split_op_index = -1;
			split_op_type  = -1;
		}
	} else {
		if (split_comment){
			if (line.length() > 16){
				return line.substring(15,eol_index); // RPI 463
			} else {
				return line.substring(0,eol_index);
			}
		}
		if (line.length() >= 16){
			split_parms_index = 15;
			split_quote_text = "";
			if (split_quote){  // RPI 463
				index = line.substring(split_parms_index,eol_index).indexOf('\'');
				while (index != -1 && split_quote){
					if (index == -1){
						return split_quote_text + line.substring(split_parms_index,eol_index);
					} else {
						if (index < eol_index - 1){
							if (line.charAt(index+1) != '\''){
								split_quote = false;
								split_quote_text = split_quote_text + line.substring(split_parms_index,split_parms_index + index+1);
								split_parms_index = split_parms_index + index+1;
							} else {
								split_quote_text = split_quote_text + line.substring(split_parms_index,split_parms_index + index+2);
								split_parms_index = split_parms_index + index+2; // skip double quotes in quoted string
							}
						} else {
							split_quote_last = true;
							split_quote = false;
							return split_quote_text + line.substring(split_parms_index,eol_index);
						}
					}
				}
			}
		} else {
			split_parms_index = -1;		
		}
	}
	if (split_parms_index == -1){
		return line.substring(0,eol_index); // return line if no parms
	}
	parm_match = parm_pattern.matcher(line.substring(split_parms_index));
    index = 0;
	boolean split_parm_end = false;
	while (!split_parm_end && parm_match.find()){
		String parm = parm_match.group();
		index = parm_match.start();
		switch (parm.charAt(0)){
			case ',':
				if ((split_op_type < max_asm_type 
						|| split_level == 0) // RPI 315 allow ,space within (...) for mac ops 
					&& !split_quote 
					&& line.length() > split_parms_index + index+1
					&& line.charAt(split_parms_index + index+1) <= ' '){  //RPI181
					// truncate line to , delimter found
					eol_index = split_parms_index + index +1;
					return split_quote_text + line.substring(split_parms_index,eol_index); // RPI 313
				}
				break;
			case '\'': // single quote found 
				if (parm.length() == 1){  // rpi 463
					if (!split_quote){
						split_quote = true;
						split_parm_end = true;
					} else {
						split_quote = false;
					}
				}
				break;
			case '(': // RPI 315
				if (!split_quote)split_level++;
				break;
			case ')': // RPI 315
				if (!split_quote)split_level--;
				break;
			default: // check for ending white space
				if (parm.charAt(0) <= ' '
					&& !split_quote
					&& (split_op_type < max_asm_type 
						|| split_level == 0) // RPI 315 allow ,space within (...) for mac ops 	
				   ){
					split_parm_end = true; // force end
				}
		}
	}
	return split_quote_text + line.substring(split_parms_index,eol_index); // return line with no comma,space
}
public void split_line(String line){  // RPI 313
	/*
	 * split line into three strings:
	 *   split_label
	 *   split_op
	 *   split_parms 
	 * using precompiled patterm  RPI 313
	 * 
	 * 3 fields are null if none and 
	 * there may be trailing comment on parms
	 */
	find_parm_match = find_parm_pattern.matcher(line);
	if (line.charAt(0) > ' '){
		find_parm_match.find();
		split_label = find_parm_match.group();
	} else {
		split_label = null;
	}
	if (find_parm_match.find()){
		split_op = find_parm_match.group().toUpperCase(); // RPI 532 
		if (find_parm_match.find()){
			split_parms = line.substring(find_parm_match.start());
			split_parms_index = find_parm_match.start();
		} else {
			split_parms = null;
			split_parms_index = -1;
		}
	} else {
		split_op = null;
		split_parms = null;
	}
}
public String get_first_dir(String dirs){
	/*
	 * return first directory in list
	 */
	    String first_dir;
		int index_first = dirs.indexOf("+");
   		if (index_first == -1){
   			index_first = dirs.indexOf(";");   			
   		}
   		if (index_first != -1){ // RPI 378
   			first_dir = dirs.substring(0,index_first);
   		} else {
   			first_dir = dirs;
   		}
   		if (first_dir.charAt(first_dir.length()-1) != File.separator.charAt(0)){
   			first_dir = first_dir + File.separator;
   		}
   		return first_dir;
}
public void put_trace(String text){
	/*
	 * open trace file if trace options on
	 */
	if (text != null 
		&& text.length() > 13
	    && text.substring(0,6).equals(text.substring(7,13))){ // RPI 659
	    text = text.substring(7); // RPI 515 RPI 659
	}
	if (opt_con || opt_test){  // RPI 689
		System.out.println(text);
	}
	if (trace_file == null){
		try {
			trace_file = new File(trace_file_name);
			trace_file_buff = new BufferedWriter(new FileWriter(trace_file));
		} catch (Exception e){
			abort_error(16,"trace file open failed - " + e.toString());
		}
	}
	try {
		if (opt_ts){
			text = get_timestamp() +  text; // RPI 662
		}
		trace_file_buff.write(text + newline); // RPI 500
	} catch (Exception e){
		abort_error(17,"trace file write error " + e.toString());
	}
	if (trace_file.length() > max_file_size){
		abort_error(18,"maximum bal file size exceeded");
	}
}
    public void inc_cur_bal_line_num(String text_line){
	/*
	 * 1.  inc cur_bal_line_num by 1 plus
	 *     previous continuations.
	 * 2.  Set number of continuation lines for next call.
	 */
    	if (text_line == null)return;
	    cur_bal_line_num = cur_bal_line_num + 1 + prev_bal_cont_lines;
	    if (text_line != null && text_line.length() > 71){ // RPI 415 adj for continuations for xref
	        prev_bal_cont_lines = 1 + (text_line.length()-72)/56;	
	    } else {
	    	prev_bal_cont_lines = 0; // RPI 550
	    }
    }
    public String get_cur_bal_line_id(int file_num, int file_line_num, int bal_line_num, int line_level, char line_type){
    	/*
    	 * return unique BAL line id consisting of:  // RPI 549
    	 *   1.  FID file id number (See list of files in stats at end of BAL)
    	 *   2.  FLN file Line number within file
    	 *   3.  GSN Generated statement number for BAL line
    	 *   4.  Type code
    	 *       ' ' main source code
    	 *       '+' generated macro code
    	 *       '=' included copybook code
    	 * Notes:
    	 *   1.  If FLN is 0 only GSN is returned for az standalone mode.
    	 *   2.  If GSN is 0 only (FID/FLN) is returned for mz trace..
    	 */
    	if (file_line_num == 0){
    		return right_justify("" + bal_line_num + line_type,10);
    	}
    	if (bal_line_num == 0){
    		return right_justify("(" + (file_num+1)
    			                 + "/" + file_line_num 
    			                 + ")"
    			                 + line_type,10); // RPI 549
    	}
    	if (line_type == ' ' 
    		&& line_level > 0){
    		line_type = '+'; // RPI 581 inline macro generated code
    	}
    	return right_justify("(" + (file_num+1)
    			                 + "/" + file_line_num 
    			                 + ")" + bal_line_num 
    			                 + line_type,15); // RPI 549
    }
    public String jar_file_dir(){
     	/*
     	 *  Return the directory containing the jar file 
     	 *  (Contributed by Martin Ward)
     	 */
     	StringBuffer path = new StringBuffer(System.getProperty("java.class.path"));
        /* Delete everything from the last directory separator onwards: */
     	path.delete(path.lastIndexOf(File.separator), path.length());
        return path.toString();
    }
    public boolean get_dfp_bin(int dfp_type,BigDecimal dfp_bd){
    	/*
    	 * store binary DD,ED, or LD format
    	 * in fp_work_reg.  Return true if value within range.
    	 */
    	/*
    	 * round to specified precision using default 
    	 */
    	/*
    	 * get digits and power of 10 exponent
    	 */
    	if (dfp_bd.signum() == 0){
    		fp_work_reg.putLong(0,0);
    		fp_work_reg.putLong(8,0);
    		return true; 
    	}
    	dfp_digits = dfp_bd.toString().toUpperCase(); 
    	int  dfp_dec_index = dfp_digits.indexOf('.');
    	int  dfp_exp_index = dfp_digits.indexOf('E');
    	int  dfp_exp = 0;
    	long dfp_scf = 0;
    	if (dfp_exp_index != -1){
    		dfp_exp = Integer.valueOf(dfp_digits.substring(dfp_exp_index+2));
    		if (dfp_digits.charAt(dfp_exp_index+1) == '-'){
    			dfp_exp = - dfp_exp;
    		}
    		if (dfp_dec_index != -1){
    			dfp_exp = dfp_exp - (dfp_exp_index - dfp_dec_index - 1); // adjust exp 
    			dfp_digits = dfp_digits.substring(0,dfp_dec_index) + dfp_digits.substring(dfp_dec_index+1,dfp_exp_index);
    		} else {
    			dfp_digits = dfp_digits.substring(0,dfp_exp_index);
    		}
    	} else {
    		if (dfp_dec_index != -1){
    			dfp_exp = dfp_exp - (dfp_digits.length() - dfp_dec_index - 1); // adjust exp
    			dfp_digits = dfp_digits.substring(0,dfp_dec_index) 
    			           + dfp_digits.substring(dfp_dec_index+1);
    		}
    	}
    	/* strip any leading zero and then
    	 * issue error and return null if out of
    	 * range.
    	 */
    	int index = 0;
    	while (index < dfp_dec_index      
    		   && dfp_digits.charAt(index) == '0'){
    		index++;  // RPI 518
    	}
    	if (index > 0){
    		dfp_digits = dfp_digits.substring(index);
    	}
    	/*
    	 * strip trailing zeros if decimal point
    	 */
    	if (dfp_dec_index != -1){
    		index = dfp_digits.length()-1;
    		while (index > 0 
    				&& dfp_digits.charAt(index) == '0'){
    			index--;  // RPI 518
    			dfp_exp++;
    		}
    		if (index < dfp_digits.length()-1){
    			dfp_digits = dfp_digits.substring(0,index+1);
    		}
    	}
    	if (dfp_digits.length() > fp_digits_max[dfp_type]){
    		return false;
    	}
    	dfp_exp = dfp_exp + fp_exp_bias[dfp_type];
    	if (dfp_exp < 0
    		|| dfp_exp > fp_exp_max[dfp_type]){
    	    return false;
    	}
    	/*
    	 * calc cf, bxcf, and ccf and return hex
    	 */
    	switch (dfp_type){
    	case 1: // fp_dd_type s1,cf5,bxcf6,ccf20
    		dfp_digits = ("0000000000000000" + dfp_digits).substring(dfp_digits.length());
    		dfp_scf = fp_sign | dfp_exp_bcd_to_cf5[(dfp_exp & 0x300) >>> 4 
    		             | (dfp_digits.charAt(0) & 0xf)];
    		fp_work_reg.putLong(0,
    				       (long)dfp_scf << 58 
    				     | (long)(dfp_exp & 0xff) << 50
    				     | get_dfp_ccf_digits(16,1,15));
    		return true;
    	case 4: // fp_ed_type s1,cf5,bxcf8,ccf50
            dfp_digits = ("0000000" + dfp_digits).substring(dfp_digits.length());
    		dfp_scf = fp_sign | dfp_exp_bcd_to_cf5[(dfp_exp & 0xc0) >>> 2 
    		             | (dfp_digits.charAt(0) & 0xf)];
    		fp_work_reg.putInt(0,
    				       (int)(dfp_scf << 26 
                                 | ((dfp_exp & 0x3f) << 20
                                 | (int)get_dfp_ccf_digits(7,1,6)
                                )));
    		return true;
    	case 7: // fp_ld_type s1,cf5,bxdf12,ccf110
            dfp_digits = ("0000000000000000000000000000000000" + dfp_digits).substring(dfp_digits.length());
    		dfp_scf = fp_sign | dfp_exp_bcd_to_cf5[(dfp_exp & 0x3000) >>> 8 | (dfp_digits.charAt(0) & 0xf)]; 
    		long dfp_ccf1 = get_dfp_ccf_digits(34,1,15);
    		fp_work_reg.putLong(0,
    				  (long)dfp_scf << 58 
                    | (long)(dfp_exp & 0xfff) << 46
                    | dfp_ccf1 >>> 4);
    		fp_work_reg.putLong(8,                       
                	  (long)(dfp_ccf1 & 0xf) << 60
                	| get_dfp_ccf_digits(34,16,18));
    		return true;
    	}
    	return false;
    }
    private long get_dfp_ccf_digits(int tot_digits,int digit_offset, int digit_count){
    	/*
    	 * return long with 1 to 6 DPD densly packed deciaml
    	 * truples of 10 bits representing 3 digits.
    	 */
    	long dfp_bits = 0;
    	int index = digit_offset;
    	while (index < digit_offset + digit_count){
            dfp_bits = (dfp_bits << 10) | dfp_bcd_to_dpd[Integer.valueOf(dfp_digits.substring(index,index+3))];
            index = index + 3;
    	}
    	return dfp_bits;
    	
    }
    public String get_timestamp(){  // RPI 662
    	/*
    	 * return current JDBC time stamp string 
    	 * with 9 digit fractional nanosecond forrmat:
    	 * yyyy-mm-dd hh:mm:ss.nnnnnnnnn (29 characters)
    	 * 
    	 * Note only thefirst 3 millisecond digits are
    	 * returned by current JDBC TimeStamp constructor so
    	 * System.nanotime() method is used to add 
    	 * remaining 6 digits of nanosecond fraction.
    	 */
    	ts_nano_now    = System.nanoTime();
    	ts_mic_dif    = (ts_nano_now - ts_nano_start)/1000000;
    	ts_mic_now     = ts_mic_start + ts_mic_dif;
    	ts_nano_digits = "" + (ts_nano_now - (ts_nano_start + ts_mic_dif * 1000000));
    	return (new Timestamp(ts_mic_now).toString() + "000").substring(0,23)
    	            + ("000000" + ts_nano_digits).substring(ts_nano_digits.length())
    	            + " ";
   }
    public void get_window_size(){
    	/*
    	 * set max_main_height and max_main_width
    	 */
        int start_bar_height = 36; //windows start bar
        try {
            max_main_height = GraphicsEnvironment.getLocalGraphicsEnvironment().getScreenDevices()[0].getDisplayMode().getHeight() 
                            - start_bar_height;
            max_main_width = GraphicsEnvironment.getLocalGraphicsEnvironment().getScreenDevices()[0].getDisplayMode().getWidth();
        } catch (Exception e){

        }
    }
    public void init_errsum(){
    	/*
    	 * turn on ERRSUM option either
    	 * by user request or
    	 * if missing COPY or MACRO error
    	 * detected during pass 1 of az390.
    	 * Note:
    	 *   1.  ASM and ERR() != 0 are prereq.
    	 *   2.  Any error limit can prevent finding
    	 *       all the missing copybooks and macros
    	 *       due to pre-mature abort on error limit.
    	 */
    	if (opt_asm && max_errors != 0){
    		opt_errsum = true;
    		max_errors = 0;
    		opt_obj    = false;
    		opt_stats  = false;
    	}
    }
}