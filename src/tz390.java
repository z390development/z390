import java.awt.GraphicsEnvironment;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.RandomAccessFile;
import java.lang.management.ManagementFactory;
import java.lang.management.MemoryPoolMXBean;
import java.lang.reflect.Array;
import java.math.BigDecimal;
import java.nio.ByteBuffer;
import java.nio.charset.Charset;
import java.sql.Timestamp;
import java.text.SimpleDateFormat;
import java.util.Arrays;
import java.util.Date;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.swing.JTextArea;


public  class  tz390 {
   /*****************************************************
	
    z390 portable mainframe assembler and emulator.
	
    Copyright 2011 Automated Software Tools Corporation
	 
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
    * 12/23/05 RPI 131 MAXFILE default 10 MB (see RPI 707)
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
    *             find_non_space_pattern precompiled rex parser
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
    * 09/28/07 RPI 707 change MAXFILE default to 1000 vs 10000  
    * 10/01/07 RPI 700 set dir_390 to pgm_dir + linklib
    * 10/18/07 RPI 713 replace \ with / for Linux   
    * 10/26/07 RPI 728 pass ictl end, cont to trim_continue
    * 10/26/07 RPI 731 add option MAXLOG(mb) to limit visible log size
    * 11/07/07 RPI 733 consolidate TRACE(AEGILMQTV)
    * 11/08/07 RPI 732 add lnk_type for linker commands
    * 11/10/07 RPI 735 change LNK to LKD to avoid conflict
    *          ignore LKD file if explicit .OBJ coded on link file name
    * 11/16/07 RPI 740 add option CHKMAC    
    * 11/25/07 RPI 742 abort if option invalid  
    *          add @file option to read options from file 
    * 11/29/07 RPI 744 validate prinatable ASCII source code  
    * 11/30/07 RPI 742 add option @file support with default
    *          suffix .OPT and search path SYSOPT which defaults
    *          to program path.  Any number of file
    *          and nested files with options * comments.
    *          Change SYSCPY to default to SYSMAC. 
    * 12/04/07 RPI 747 add CHKSRC(0-2) CHKMAC(0-2) checking options 
    * 12/19/07 RPI 756 support explicit path for any file 
    *          and don't add ".." twice  
    * 12/23/07 RPI 767 add option NORM default NONORM  
    * 12/27/07 RPI 755 cleanup msgs to log, sta, tr*, con
    * 12/27/07 RPI 769 check * comments when CHKSRC active 
    * 12/27/07 RPI 770 add AUTOLINK default and NOAUTOLINK 
    * 12/27/07 RPI 773 raise max_opsyn to 10000  
    * 12/28/07 RPI 774 return empty string if dup count <= 0
    * 01/11/08 RPI 786 support DFP preferred exp.
    * 01/14/08 RPI 787 support DFP unnormalized instructions
    * 01/17/08 RPI 790 set DFP exp from explicit decimal else use 0
    *          add fp_normalization for HFP unnormalized instructions
    * 02/28/08 RPI 814 change pgm_dir to dir_pgm for consistency 
    * 02/28/08 RPI 812 add ASSIST option for assembly and emulation of ASSIST 
    * 03/03/08 RPI 817 add 226 z10 instructions  
    * 03/13/08 RPI 820 prevent cf5 array exception due to overflow
    * 03/15/08 RPI 822 add AUTOLINK to STATS file
    * 03/27/08 RPI 828 add option INIT to set regs to x'F4' and mem to x'F5' vs 0's
    * 04/18/08 RPI 833 option ALLOW to permit HLASM extensions
    * 04/23/08 RPI 837 update EZ390 ENDING msg only once and add to log
    * 05/07/08 RPI 849 use shared abort_case for logic errors
    * 05/10/08 RPI 821 switch DH from double to BigDecimal cache
    * 05/28/08 RPI 855 skip line on TRACEM/P AIF/AGO branch
    * 06/12/08 RPI 862 add TRACEC with default NOTRACEC to suppress copybooks
    *          and turn on LISTCALL for TRACEM and TRACEP
    * 06/23/08 RPI 866 add SYSLST and lst_type for use by lz390 
    *          and update get_file_name to allow parm_dir to
    *          override basemane and ext or parm to override path
    *          (SYSPCH and SYSPRN used in DFHALL for BMS map gens)
    * 07/03/08 RPI 874 support all option set/reset regardless of default  
    * 07/26/08 RPI 880 allow trailing * without space on options, show invalid #file spec 
    *          return null for invalid get_file_name path or file
    *          display each invalid option with file/line #  
    *          Support *.sfx override for BAL,ERR,LOG,LST,PCH,PRN  
    * 07/28/08 RPI 882 if TRACES display source lines and errors on console  
    * 07/28/08 RPI 883 add option MOD for LZ390 to generate raw code file with no header or rlds 
    * 08/05/08 RPI 891 use variable mac_gen for setting ID + indicatior  
    * 08/16/08 RPI 900 allow .\ in absolute path and add default suffix
    * 09/02/08 RPI 902 make pad_spaces(n) public for use by ZSTRMAC
    * 09/06/08 RPI 797 suppress versions and mem for NOTIMING 
    * 09/08/08 RPI 903 allow 0 length file type for CD command usage 
    * 09/15/08 RPI 905 remove ", " delimited comments on EXEC stmts  
    * 09/16/08 RPI 908 support path\file overrides for output files and prevent traps
    * 09/25/08 RPI 920 add MAXPASS(2) for nested forward sym refs (see az390 added LOCTR cnt)
    * 10/04/08 RPI 924 remove duplicate STA file entry for TRACET
    * 10/24/08 RPI 935 correct MAXWIDTH and MAXWARN settings (reversed)
    * 10/24/08 RPI 935 prevent recursive abort, correct force_nocon support
    * 11/08/08 RPI 947 add get_ascii_printable_string for use by dump/trace
    * 12/01/08 RPI 970 add key index "C:" to indicate COPY file found
    * 12/11/08 RPI 957 chksrc(3) to chk seq field and > 80
    * 12/11/08 RPI 963 display final options 1 per line
    * 12/19/08 RPI 979 default SYSCPY to pgm dir prior to addition of paths
    * 01/26/09 RPI 986 add zcobol options COMMENT, EXTEND, TRUNC, WARN, R64
    * 03/06/09 RPI 1004 add FLOAT(DECIMAL/BINARY/HEX) for DFP,BFP, or HFP zcobol DEL type
    * 03/09/09 RPI 1013 add PFPO, ECTG, and CSST instructions
    * 04/20/09 RPI 1027 add option EDF and GBLC &SYSEDF for zCICS use
    * 06/13/09 RPI 1053 add check_options for NOASM-CHKMAC-CHKSRC
    * 06/22/09 RPI 1059 require ASM for ERRSUM or abort, remove PRN msg
    * 06/30/09 RPI 1044 add key_inde "V:" for EXTRN
    * 07/14/09 RPI 1062 split out init_pat() for use by options in zc390
    * 08/24/09 RPI 1069 add CODEPAGE(ascii+ebcdic+LIST) option
    * 08/28/09 RPI 1073 add option ALIGN/NOALIGN
    * 09/21/09 RPI 1080 fix \ to / and vice versa for all paths
    *          and compile all replaceall patterns for speed -'Z"?\/
    * 09/25/09 RPI 1080 use init_os_type and init_os_util for linux compat.
    *          fix file_name separators for systerm, fix / to \ and \ to /
    *          set install_loc in options, repackage init_tz390
    *          to run init_pat, os, util, time
    * 01/04/10 RPI 1094 move timeout from pz390 to tz390 for use by gz390
    * 01/07/10 RPI 1097 add support for ..\ and .\ in paths
    * 02/16/10 RPI 1108 add fp_lq_type for LQ quad word
    * 05/25/10 RPI 1118 add MAXDISPLAY(80) max zcobol display line length
    * 05/31/10 RPI 1123 change CDF to EDF and SYSCDF to SYSEDF
    * 06/14/10 RPI 1124 change fp_guard_digits from 3 to 4 to fix lsd error in D'1.23456789'
    * 07/28/10 RPI 1127 add option PRINTALL to suppress PRINT OFF/NOGEN
    * 07/28/10 RPI 865 correct setting of dir_cur for INSTALL(path) option
    * 08/06/10 RPI 1125 add POPCNT per SHARE Pres. 08/04/10
    * 10/08/10 RPI 1125 add LEDBR?, LDXBR?, LEXBR?
    * 10/10/10 RPI 1125 ADD SRNMB
    * 10/14/10 RPI 1131 correct option EDF versus CDF, and MAXDISPLAY stat
    * 10/18/10 RPI 1131 allow * within option but not leading
    * 10/20/10 RPI 1125 add FIEBR?, FIDBR?, FIXBR?, SRNMB
    * 10/21/10 RPI 1125 add B390-B392
    * 11/23/10 RPI 1125 add B394-B39A
    * 11/24/10 RPI 1125 ADD B39C-B3A2 CLFEBR-CXLGBR
    * 11/28/10 RPI 1125 ADD B3A4-B3AE CEGBRA, CGEBRA, CLGEBR
    * 11/30/10 RPI 1125 ADD B3D0-B3DB MDTRA-SXTRA
    * 12/02/10 RPI 1125 ADD B3E1-BEF9 CGDTRA-CXGTRA 
    * 12/03/10 RPI 1125 ADD B928-B92D PCKMO KMOTR 
    * 12/04/10 RPI 1125 ADD B941-B95B CFDTR - CXLFTR, FIX MDTRA DFP/BFP RND
    * 12/06/10 RPI 1125 ADD B9AE-B9CB RRBM-SLHHHR  
    * 12/08/10 RPI 1125 ADD B9CD-B9DF CHHR-CLHHLR
    * 12/09/10 RPI 1125 ADD B9E2-B9FB LOCGR-SLRK 
    * 12/09/10 RPI 1125 ADD C84-C85 LPD-LPDG
    * 12/11/10 RPI 1125 ADD CC6-CCF BRCTH - CLIH 
    * 12/18/10 RPI 1125 ADD E3C0-E3CF LBH - CLHF
    * 12/19/10 RPI 1125 ADD EBDC-EBFA SRAK - LAAL
    * 12/21/10 RPI 1125 ADD EC51-ECDB RISBLG - ALGSIK 
    * 12/23/10 RPI 1142 add option MNOTE(0)
    * 03/23/11 RPI 1156 add options z390\z390.opt options file
    * 03/29/11 RPI 1157 add TRACEI to trace AINSERT
    * 05/08/11 RPI 1149 open TRE trace if put_trace called, add start/end
    * 05/17/11 RPI 1164 add RISBHGZ and RISBLGZ support
    * 07/30/11 RPI 1175 public check_java_version() and
    *          support Sun, Oracle, Apple vers 1.6-9.9
    * 02/15/12 RPI 1186 continuous loc ctr on PRN across CSECT's
    * 02/17/12 RPI 1191 allow periods in file path names
    * 03/30/12 RPI 1199 remove upper limit on Apple release
    * 04/19/12 RPI 1210 allow period in path without corrupting file type
    * 05/02/12 RPI 1211 limit check cf5_index for STE of type 4 ED
    ********************************************************
    * Shared z390 tables                  (last RPI)
    *****************************************************/
	/*
	 * shared version id 
	 */
	// dsh - change version for every release and ptf
	// dsh - change dcb_id_ver for dcb field changes
    String version    = "V1.5.06";  //dsh
	String dcb_id_ver = "DCBV1001";  //dsh
	byte   acb_id_ver = (byte)0xa0;  // ACB vs DCB id RPI 644 
	/*
	 * global options 
	 */ 
	String java_vendor  = System.getProperty("java.vendor");  // RPI 1175
	String java_version = System.getProperty("java.version"); // RPI 1175
	String  os_name = ""; // RPI 1080
	byte    z390_os_type  = 0;      // 1=win,2=Linux  RPI 499
	byte    z390_os_win   = 1;
	byte    z390_os_linux = 2;
	String  z390_font    = "Monospaced";  // RPI 509 was Courier
	boolean timeout = false; // RPI 1094
	boolean z390_abort   = false;  // global abort request
	boolean tz390_recursive_abort = false; // RPI 935
	String  invalid_options = "";  // RPI 742
	boolean opt_align    = true;   // align data fields by type if not explicit RPI 1073
	boolean opt_allow    = false;  // allow extensions such as no quotes for SETC var
    boolean opt_amode24  = false;  // link to run amode24
    boolean opt_amode31  = true;   // link to run amode31
    boolean opt_ascii    = false; // use ascii vs ebcdic
    boolean opt_asm      = true;  // run az390 assembler as mz390 subtask  RPI 415
    boolean opt_assist   = false; // enable assembly and emulation of ASSIST instructions
    boolean opt_autolink = true;  // search SYSOBJ for missing externals
    boolean opt_bal      = false; // generate bal source output from mz390 RPI 415
    boolean opt_bs2000   = false; // Seimens BS2000 asm compatibility
    boolean opt_cics     = false; // exec cics program honoring prolog,epilog
    boolean opt_codepage = false; // use ascii and ebcdic codepages specified CODEPAG(ascii,ebcdic,LIST)
    boolean opt_comment  = true;  // generate source comments for zocobol RPI 986
    boolean opt_con      = true;  // log msgs to console
    boolean force_nocon  = false; // override option con RPI 755
    boolean opt_dump     = false; // only indicative dump on abend unless on
    boolean opt_edf      = true; // option for zCICS RPI 1027 renamed EDF RPI 1123
    boolean opt_epilog   = true;  // if cics, insert DFHEIRET
    boolean opt_errsum   = false; // just list critical errors and summary on ERR file and console 
    boolean opt_extend   = true;  // allow up to 31 digits for P and Z in zocobl RPI 986
    boolean opt_guam     = false; // use gz390 GUAM GUI access method interface
    boolean opt_init     = true;  // init regs to x'F4", mem to x'F5'
    String  codepage     = "CODEPAGE(ISO-8859-1+IBM1047)";  // default z/OS compatible
    String  opt_float    = "DECIMAL"; // zcobol FLOAT-? type D=DFP,B=BFP,H=HFP
    
    String  opt_ipl      = "";    // program to execute at startup
    String  opt_install_loc = ""; // optional install location for source debugging
    boolean opt_list     = true;  // generate LOG file
    boolean opt_listcall = true;  // list macro calls
    boolean opt_listuse  = true;  // list usage at USING and DROP
    boolean opt_loadhigh = true;  // load pgms and alloc storage from top down
    boolean opt_mcall    = false; // list MCALL and MEXIT on PRN // RPI 511
    boolean opt_mod      = false;  // generate raw code output from lz390 with sfx .MOD
    boolean opt_obj      = true;  // generate binary MVS compatible OBJ file RPI 694
    boolean opt_objhex   = false; // generate ascii hex obj records (lz390 accepts bin or hex)
    String  opt_parm     = "";    // user parm string for ez390 (mapped to R1 > cvt_exec_parm)
    boolean opt_pc       = true;  // generate macro pseudo code
    boolean opt_pcopt    = true;  // optimize pc code for speed
    boolean opt_pdsmem8  = false; // check for copy/mac names > 8 // RPI 11
    boolean opt_printall = false; // force default PRINT GEN and ignore PRINT options RPI 1127
    String  opt_profile  = "";    // include PROFILE(COPYBOOK) as first MLC statement
    boolean opt_prolog   = true;  // if cics, insert DFHEIBLK and DFHEIENT
    boolean opt_protect  = true;  // prevent PSA mods by user
    boolean opt_r64      = true;  // allow 64 bit register instructions RPI 986
    boolean opt_reformat = false; // reformat BAL statements 
    boolean opt_regs     = false; // show registers on trace
    boolean opt_rmode24  = true;  // link to load below line
    boolean opt_rmode31  = false; // link to load above line
    boolean opt_stats    = false;  // show statistics on STA file
    String  opt_sysparm  = "";    // user parm string for mz390  
    boolean opt_test     = false; // invoke interactive test cmds
    boolean opt_thread   = true;  // continuous PRN location counter RPI 1186
    boolean opt_time     = true;  // abend 422 if out of time TIME (sec)
    boolean opt_timing   = true;  // display current date, time, rate
    boolean opt_trace    = false; // trace pz390 instructions to LOG
    boolean opt_tracea   = false; // trace az390
    boolean opt_traceall = false; // trace all details
    boolean opt_tracec   = false; // trace copybooks for tracep // RPI 862
    boolean opt_traceg   = false; // trace memory FQE updates to LOG
    boolean opt_tracei   = false; // trace AINSERT RPI 1157
    boolean opt_tracel   = false; // trace lz390
    boolean opt_tracem   = false; // trace mz390
    boolean opt_tracep   = false; // trace pseudo code
    boolean opt_traceq   = false; // trace QSAM file I/O
    boolean opt_traces   = false; // trace MLC source and errors on concole for mz390 // RPI 882    
    boolean opt_tracet   = false; // trace TCPIO and TGET/TPUT data I/O
    boolean opt_tracev   = false; // trace VSAM file I/O
    boolean opt_trap     = true;  // trap exceptions as 0C5
    boolean opt_trunc    = false; // zcobol TRUNC option default NOTRUNC RPI 986
    boolean opt_ts       = false; // time-stamp logs RPI 662
    boolean opt_vcb      = true;  // vsam cache operational
    boolean opt_warn     = true;  // issue zcobol warnings RPI 986
    boolean opt_xref     = true;   // cross reference symbols
    boolean opt_zstrmac  = true;   // allow ZSTRMAC extensions
    boolean max_cmd_queue_exceeded = false;  // RPI 731
    String  cmd_parms = ""; // all options from command
    int     cmd_parms_len = 34; // RPI 755
    int     max_cmd_parms_line = 78; // RPI 755
    String  test_ddname = "";
    char    z390_amode31 = 'T';
    char    z390_rmode31 = 'F';
    int opt_chkmac   = 0; // RPI 747 0-none,1-labels, 2-labels and src after MEND
    int opt_chksrc   = 1; // RPI 747 0-none,1-MLC only,2-all, 3-seq 73-80 and char past 80
    int opt_maxcall  = 50;
    int opt_maxdisplay = 80; // RPI 1118 max display line length for zcobol
    int opt_maxesd   = 1000;
    int opt_maxfile = 1000;     // RPI 707 max concourrent files open
    int opt_maxgbl  = 100000;   // RPI 284
    int opt_maxlcl  = 100000;   
    int opt_maxline = 200000;
    int opt_maxlog  = 1000000; // RPI 731
    int opt_maxparm = 10000;
    int opt_maxpass = 2;       // RPI 920 maximum az390 passes for nested symbol refs
    int opt_maxpc   = 50000;  // RPI 439 pseudo code working set
    int opt_maxque  = 1000;   // RPI 731 max CMD output queue
    int opt_maxrld  = 10000;
    int opt_maxsym  = 50000;
    int opt_mnote   = 0; // RPI 1142 (0 all, 1 az only, 2 mz only)
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
	int    max_rba_size = 0x7fffffff;   // max vsam RBA vs XRBA RPI 706
	long   max_time_seconds  = 15;      // TIME(15)max elapsed time - override time(sec)
	int    monitor_wait = 300;          // fix interval in milliseconds
    int    max_mem           = 1;       // MEM(1)  MB memory default (see mem(mb) override)
    String trace_options = "";
    /*
     * shared date and time formats
     */
	SimpleDateFormat sdf_MMddyy = new SimpleDateFormat("MM/dd/yy");
	SimpleDateFormat sdf_HHmmss = new SimpleDateFormat("HH:mm:ss");
    /*
	 * shared pgm dir, name, type and associated dirs
	 */
	String pgm_name = null; // from first parm else abort
	String pgm_type = null; // from first parm override if mlc else def.
	String file_dir;        // dir for _name RPI 700
	String file_type;       // type for find_file_name
	String ada_type = ".ADA"; // ADATA type (not supported yet)
	String bal_type = ".BAL"; // basic assembler output from mz390, input to az390
	String cpy_type = ".CPY"; // copybook source for mz390
    String dat_type = ".DAT"; // AREAD default input for mz390
	String err_type = ".ERR"; // step error and rc log
    String log_type = ".LOG"; // log for z390, ez390, sz390, pz390
	String lkd_type = ".LKD"; // linker commands INCLUDE, ENTRY, ALIAS, NAME RPI 735
    String lst_type = ".LST"; // linker list file
	Boolean lkd_ignore = false; // RPI 735 ignore LKD if explicit .OBJ
	String mac_type = ".MAC"; // macro source
    String mlc_type = ".MLC"; // macro assembler source program
    String mod_type = ".MOD"; // load module file with no header, trailer,RLDs, and no rounding RPI 883
    String obj_type = ".OBJ"; // relocatable object code for az390 and lz390
    String opt_type = ".OPT"; // @file option file with one option per line plus comments 
    String pch_type = ".PCH"; // punch output from mz390
    String prn_type = ".PRN"; // assembly listing for az390
    String sta_type = ".STA"; // statistics mod file for option stats(filename) RPI 737
    String tra_type = ".TRA"; // az390 trace file
    String tre_type = ".TRE"; // ez390 trace file
    String trl_type = ".TRL"; // lz390 trace file
    String trm_type = ".TRM"; // mz390 trace file
    String z390_type = ".390"; // z390 executable load module for lz390 and ez390
    String dir_390 = null; // SYS390() load module
    String dir_bal = null; // SYSBAL() az390 source input
    String dir_cpy = null; // SYSCPY() mz390 copybook lib defaults to dir_mac RPI 742
	String dir_cur = null; // default current dir
    String dir_dat = null; // SYSDAT() mz390 AREAD extended option
    String dir_err = null; // SYSERR() ?z390 systerm error file directory
    String dir_log = null; // SYSLOG() ez390 log // RPI 243
    String dir_lst = null; // SYSLST() lz390 listing 
    String dir_mac = null; // SYSMAC() mz390 macro lib
    String dir_mlc = null; // SYSMLC() mz390 source input
    String dir_pch = null; // SYSPCH() mz390 punch output dir
	String dir_pgm = null; // from first parm else dir_cur
    String dir_prn = null; // SYSPRN() az390 listing
    String dir_obj = null; // SYSOBJ() lz390 object lib
    String dir_opt = null; // SYSOPT() OPT options @file path defaults to dir_mac RPI 742
    String dir_trc = null; // SYSTRC() trace file directory
    int max_opsyn = 1000; 
    int tot_opsyn = 0;
    int opsyn_index = -1;
    String[]  opsyn_new_name = new String[max_opsyn];
    String[]  opsyn_old_name = new String[max_opsyn];
    int cur_bal_line_num    = 0; // bal starting line number
    int prev_bal_cont_lines = 0; // bal continue lines for prev bal
    int bal_ictl_start =  1; // RPI 728 reformated to std by mz390
    int bal_ictl_end   = 71; // RPI 728
    int bal_ictl_cont  = 16; // RPI 728
    int bal_ictl_cont_tot = 56; // RPI 728
    /*
     * shared SYSTERM error file
     */
    long   systerm_start = 0; // start time
    String systerm_sec   = ""; // systerm elapsed seconds
    String systerm_mem   = " MB"; // RPI 797 MB if 
    String systerm_file_name      = null;
    RandomAccessFile systerm_file = null;
    String systerm_prefix = "";   // pgm_name plus space
    int    systerm_io     = 0;    // total file io count
    long systerm_ins    = 0;    // ez390 instruction count
    String started_msg = "";
    String ended_msg   = "";
    String stats_file_name      = null;
    RandomAccessFile stats_file = null;
    /*
     * log, trace file used by mz390, az390, lz390, ez390
     */
    String         log_file_name = ""; // RPI 755
    String         trace_file_name = null;
	File           trace_file = null;
	BufferedWriter trace_file_buff = null;
    int tot_log_msg  = 0; // RPI 731
    int tot_log_text = 0; // RPI 731
    boolean log_text_added = false; // RPI 731
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
    Pattern find_non_space_pattern = null;
    Pattern find_bslash  = null; // RPI 1080
    Matcher match_bslash = null; // RPI 1080
    Pattern find_slash   = null; // RPI 1080
    Matcher match_slash  = null; // RPI 1080
    Pattern find_dash    = null; // RPI 1080
    Matcher match_dash   = null; // RPI 1080
    Pattern find_squote  = null; // RPI 1080
    Matcher match_squote = null; // RPI 1080
    Pattern find_dsquote  = null; // RPI 1080
    Matcher match_dsquote = null; // RPI 1080
    Pattern find_dquote  = null; // RPI 1080
    Matcher match_dquote = null; // RPI 1080
    Pattern find_ddquote  = null; // RPI 1080
    Matcher match_ddquote = null; // RPI 1080
    Pattern find_amp     = null; // RPI 1080
    Matcher match_amp    = null; // RPI 1080
    Pattern find_damp     = null; // RPI 1080
    Matcher match_damp    = null; // RPI 1080
    Matcher find_parm_match = null; // RPI 1080
    Pattern parm_pattern = null;
    Matcher parm_match = null;
    boolean split_first = true; // first line of statement
    boolean split_cont  = false; // continuation line of statement
    boolean split_comment = false;
    boolean exec_line = false; // RPI 905
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
	    String ascii_min_char =  // RPI 1069
	      " 01234567890" // blank and didgits
	    + "ABCDEFGHIJKLMNOPQRSTUVWXYZ" // uppercase
	    + "abcdefghijklmnopqrstuvwxyz" // lower case
	    + "@#$" // additional leading symbol characters
	    + "&’()*+-./:=_"; // mimimum zscii special char
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
                        "101112003C3D322618193F2722003500" + //10 ................ 
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
   * CODEPAGE(ascii,ebcdic,LIST) option data 
   */
        String default_charset_name = Charset.defaultCharset().name();
        String ascii_charset_name = "";
        String ebcdic_charset_name = "";
        boolean list_charset_map = false;
		String test_ascii;
		String test_ebcdic;
		char   test_char;
		byte[] init_charset_bytes = new byte[256];
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
        byte fp_db_type = 0; // BFP long - double
        byte fp_dd_type = 1; // DFP long - big dec
        byte fp_dh_type = 2; // HFP long - big dec
        byte fp_eb_type = 3; // BFP short - float
        byte fp_ed_type = 4; // DFP short - big dec
        byte fp_eh_type = 5; // HFP short - double
        byte fp_lb_type = 6; // BFP extended - big dec
        byte fp_ld_type = 7; // DFP extended - big dec
        byte fp_lh_type = 8; // HFP extended - big dec
        byte fp_lq_type = 9; // LQ quad word
        byte fp_db_digits = 15;
        byte fp_dd_digits = 16;
        byte fp_dh_digits = 15;   
        byte fp_eb_digits = 7;
        byte fp_ed_digits = 7;
        byte fp_eh_digits = 7;   // RPI 821
        byte fp_lb_digits = 34;
        byte fp_ld_digits = 34;
        byte fp_lh_digits = 32; // RPI 821  
        byte fp_guard_digits = 4; // RPI 1124        
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
        		fp_dd_digits,  // RPI 790 
        		fp_dh_digits+fp_guard_digits,
        		fp_eb_digits+fp_guard_digits,
        		fp_ed_digits,  // RPI 790 
        		fp_eh_digits+fp_guard_digits,
        		fp_lb_digits+fp_guard_digits,
        		fp_ld_digits,  // RPI 790 
        		fp_lh_digits+fp_guard_digits,
        		fp_lh_digits+fp_guard_digits  // rpi 1108 lq 
        		}; 
        int[]  fp_digits_max  = {0,16,0,0,7,0,0,34,0,0};
        int[]  fp_sign_bit    = {0x800,0x20,0x80,0x100,0x20,0x80,0x8000,0x20,0x80,0X80}; // RPI 407
        int[]  fp_one_bit_adj = {2,-1,2,2,-1,1,2,-1,1,1}; // RPI 407 RPI 821 from 1 to 2 
        int[]  fp_exp_bias    = {0x3ff,398,0x40,0x7f,101,0x40,0x3fff,6176,0x40,0X40}; // RPI 407
        int[]  fp_exp_max     = {0x7ff,0x3ff,0x7f,0xff,0xff,0x7f,0x7fff,0x3fff,0x7f,0X7F}; // RPI 407
        int[]  fp_man_bits = {52,-1,56,23,-1,24,112,-1,112,112}; 
  /*
   * DFP Decimal Floating Point shared tables
   */
        int fp_sign = 0;
        int fp_exp   = 0; // scale * log10/log2
        String dfp_digits = null;
    	int    dfp_dec_index = 0;  // RPI 786
    	int    dfp_exp_index = 0;  // RPI 786
    	int    dfp_exp = 0;        // RPI 786
    	int    dfp_scf = 0;        // RPI 786
    	int    dfp_preferred_exp = -2; // RPI 786
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
		       "PFPO",     // 40 "010A" "PFPO" "E" 1   RPI 1013
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
		       "XDECO",    // 1193 "52" "XDECO" "RX" 37 RPI 812
		       "XDECI",    // 1196 "53" "XDECI" "RX" 37 RPI 812	       
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
		       "XHEXI",    // 1323 "61" "XHEXI" "RX" 37 RPI 812
		       "XHEXO",    // 1326 "62" "XHEXO" "RX" 37 RPI 812
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
		       "SRNMB",    // 3392 "B2B8" "SRNMB" "S" 7 RPI 1125
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
		       "LEDBR?",    // 3860 "B344" "LEDBR?" "RRE" 53 RPI 1125
		       "LDXBR?",    // 3870 "B345" "LDXBR?" "RRE" 53 RPI 1125
		       "LEXBR?",    // 3880 "B346" "LEXBR?" "RRE" 53 RPI 1125
		       "FIXBR?",    // 3890 "B347" "FIXBR?" "RRF2" 54 RPI 1125
		       "KXBR",     // 3900 "B348" "KXBR" "RRE" 14
		       "CXBR",     // 3910 "B349" "CXBR" "RRE" 14
		       "AXBR",     // 3920 "B34A" "AXBR" "RRE" 14
		       "SXBR",     // 3930 "B34B" "SXBR" "RRE" 14
		       "MXBR",     // 3940 "B34C" "MXBR" "RRE" 14
		       "DXBR",     // 3950 "B34D" "DXBR" "RRE" 14
		       "TBEDR",    // 3960 "B350" "TBEDR" "RRF2" 34
		       "TBDR",     // 3970 "B351" "TBDR" "RRF2" 34
		       "DIEBR",    // 3980 "B353" "DIEBR" "RRF3" 30
		       "FIEBR?",   // 3990 "B357" "FIEBR?" "RRF2" 54 RPI 1125
		       "THDER",    // 4000 "B358" "THDER" "RRE" 14
		       "THDR",     // 4010 "B359" "THDR" "RRE" 14
		       "DIDBR",    // 4020 "B35B" "DIDBR" "RRF3" 30
		       "FIDBR?",    // 4030 "B35F" "FIDBR?" "RRF2" 54 RPI 1125
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
		       "CELFBR",   //      "B390" "CELFBR" "RRF3" 30 RPI 1125 Z196
               "CDLFBR",   //      "B391" "CDLFBR" "RRF3" 30 RPI 1125 Z196
               "CXLFBR",   //      "B392" "CXLFBR" "RRF3" 30 RPI 5 Z112196
		       "CEFBR?",    // 4190 "B394" "CEFBR?" "RRE" 53 RPI 1125 Z196
		       "CDFBR?",    // 4200 "B395" "CDFBR?" "RRE" 53 RPI 1125 Z196
		       "CXFBR?",    // 4210 "B396" "CXFBR?" "RRE" 53 RPI 1125 Z196
		       "CFEBR?",    // 4220 "B398" "CFEBR?" "RRF2" 34 RPI 1125 Z196
		       "CFDBR?",    // 4230 "B399" "CFDBR?" "RRF2" 34 RPI 1125 Z196
		       "CFXBR?",    // 4240 "B39A" "CFXBR?" "RRF2" 34 RPI 1125 Z196
		       "CLFEBR",    // 'B39C' 'CLFEBR' 'RRF3' 30 RPI 1125 Z196
               "CLFDBR",    // 'B39D' 'CLFDBR' 'RRF3' 30 RPI 1125 Z196
               "CLFXBR",    // 'B39E' 'CLFXBR' 'RRF3' 30 RPI 1125 Z196
               "CELGBR",    // 'B3A0' 'CELGBR' 'RRF3' 30 RPI 1125 Z196
               "CDLGBR",    // 'B3A1' 'CDLGBR' 'RRF3' 30 RPI 1125 Z196
               "CXLGBR",    // 'B3A2' 'CXLGBR' 'RRF3' 30 RPI 1125 Z196
		       "CEGBR?",    // 4250 "B3A4" "CEGBR?" "RRE" 53 RPI 1125 Z196
		       "CDGBR?",    // 4260 "B3A5" "CDGBR?" "RRE" 53 RPI 1125 Z196
		       "CXGBR?",    // 4270 "B3A6" "CXGBR?" "RRE" 53 RPI 1125 Z196
		       "CGEBR?",    // 4280 "B3A8" "CGEBR?" "RRF2" 54 RPI 1125 Z196
		       "CGDBR?",    // 4290 "B3A9" "CGDBR?" "RRF2" 54 RPI 1125 Z196
		       "CGXBR?",    // 4300 "B3AA" "CGXBR?" "RRF2" 54 RPI 1125 Z196
		       "CLGEBR",    //      "B3AC" "CLGEBR" "RRF2" 30  RPI 1125 Z196
               "CLGDBR",    //      "B3AD" "CLGDBR" "RRF2" 30  RPI 1125 Z196
               "CLGXBR",    //      "B3AE" "CLGXBR" "RRF2" 30  RPI 1125 Z196
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
		       "MDTR?",  // "B3D0" "RRR"  DFP  1 RPI 1125
		       "DDTR?",  // "B3D1" "RRR"  DFP  2 RPI 1125
		       "ADTR?",  // "B3D2" "RRR"  DFP  3 RPI 1125
		       "SDTR?",  // "B3D3" "RRR"  DFP  4 RPI 1125
		       "LDETR",  // "B3D4" "RRF4" DFP  5 
		       "LEDTR",  // "B3D5" "RRF3" DFP  6 
		       "LTDTR",  // "B3D6" "RRE"  DFP  7 
		       "FIDTR",  // "B3D7" "RRF3" DFP  8 
		       "MXTR?",  // "B3D8" "RRR"  DFP  9 RPI 1125
		       "DXTR?",  // "B3D9" "RRR"  DFP 10 RPI 1125
		       "AXTR?",  // "B3DA" "RRR"  DFP 11 RPI 1125
		       "SXTR?",  // "B3DB" "RRR"  DFP 12 RPI 1125
		       "LXDTR", // "B3DC" "RRF4" DFP 13
		       "LDXTR", // "B3DD" "RRF3" DFP 14
		       "LTXTR", // "B3DE" "RRE" DFP 15
		       "FIXTR", // "B3DF" "RRF3" DFP 16
		       "KDTR", // "B3E0" "RRE" DFP 17
		       "CGDTR?", // "B3E1" "RRF7" DFP 18 RPI 1125
		       "CUDTR", // "B3E2" "RRE" DFP 19
		       "CSDTR", // "B3E3" "RRF4" DFP 20
		       "CDTR", // "B3E4" "RRE" DFP 21
		       "EEDTR", // "B3E5" "RRE" DFP 22
		       "ESDTR", // "B3E7" "RRE" DFP 23
		       "KXTR", // "B3E8" "RRE" DFP 24
		       "CGXTR?", // "B3E9" "RRF7" DFP 25 RPI 1125
		       "CUXTR", // "B3EA" "RRE" DFP 26
		       "CSXTR", // "B3EB" "RRF4" DFP 27
		       "CXTR", // "B3EC" "RRE" DFP 28
		       "EEXTR", // "B3ED" "RRE" DFP 29
		       "ESXTR", // "B3EF" "RRE" DFP 30
		       "CDGTR?", // "B3F1" "RE3" DFP 31 RPI 1125
		       "CDUTR", // "B3F2" "RRE" DFP 32
		       "CDSTR", // "B3F3" "RRE" DFP 33
		       "CEDTR", // "B3F4" "RRE" DFP 34
		       "QADTR", // "B3F5" "RRF3" DFP 35
		       "IEDTR", // "B3F6" "RRF2" DFP 36
		       "RRDTR", // "B3F7" "RRF3" DFP 37
		       "CXGTR?", // "B3F9" "RE3" DFP 38 RPI 1125
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
		       "PCKMO",    // "B928","PCKMO","RE4"  14 RPI 1125 Z196
               "KMF",      // "B92A","KMF","RRE"    14 RPI 1125 Z196
               "KMO",      // "B92B","KMO","RRE"    14 RPI 1125 Z196
               "PCC",      // "B92C","PCC","RE4"    14 RPI 1125 Z196
               "KMCTR",    // "B92D","KMCTR","RRF2" 34 RPI 1125 Z196
		       "KM",       // 4770 "B92E" "KM" "RRE" 14
		       "KMC",      // 4780 "B92F" "KMC" "RRE" 14
		       "CGFR",     // 4790 "B930" "CGFR" "RRE" 14
		       "CLGFR",    // 4800 "B931" "CLGFR" "RRE" 14
		       "KIMD",     // 4810 "B93E" "KIMD" "RRE" 14
		       "KLMD",     // 4820 "B93F" "KLMD" "RRE" 14
		       "CFDTR",    // "B941","CFDTR","RRF3"  30 RPI 1125 Z196
               "CLGDTR",   // "B942","CLGDTR","RRF3" 30 RPI 1125 Z196
               "CLFDTR",   // "B943","CLFDTR","RRF3" 30 RPI 1125 Z196
		       "BCTGR",    // 4830 "B946" "BCTGR" "RRE" 14
		       "CFXTR",    // "B949","CFXTR","RRF3"   30 RPI 1125 Z196
               "CLGXTR",   // "B94A","CLGXTR","RRF3"  30 RPI 1125 Z196
               "CLFXTR",   // "B94B","CLFXTR","RRF3"  30 RPI 1125 Z196
               "CDFTR",    // "B951","CDFTR","RRF3"   30 RPI 1125 Z196
               "CDLGTR",   // "B952","CDLGTR","RRF3"  30 RPI 1125 Z196
               "CDLFTR",   // "B953","CDLFTR","RRF3"  30 RPI 1125 Z196
               "CXFTR",    // "B959","CXFTR","RRF3"   30 RPI 1125 Z196" 
               "CXLGTR",   // "B95A","CXLGTR","RRF3"  30 RPI 1125 Z196
               "CXLFTR",   // "B95B","CXLFTR","RRF3"  30 RPI 1125 Z196		       
		       "CGRT",     // 10 "B960" "CGRT" "RRF5" 39 RPI 817
		       "CGRTE",    // 20 "B9608" "CGRTE" "RRF6" 40 RPI 817
		       "CGRTH",    // 30 "B9602" "CGRTH" "RRF6" 40 RPI 817
		       "CGRTL",    // 40 "B9604" "CGRTL" "RRF6" 40 RPI 817
		       "CGRTNE",   // 50 "B9606" "CGRTNE" "RRF6" 40 RPI 817
		       "CGRTNH",   // 60 "B960C" "CGRTNH" "RRF6" 40 RPI 817
		       "CGRTNL",   // 70 "B960A" "CGRTNL" "RRF6" 40 RPI 817
		       "CLGRT",    // 10 "B961" "CLGRT" "RRF5" 39 RPI 817
		       "CLGRTE",   // 20 "B9618" "CLGRTE" "RRF6" 40 RPI 817
		       "CLGRTH",   // 30 "B9612" "CLGRTH" "RRF6" 40 RPI 817
		       "CLGRTL",   // 40 "B9614" "CLGRTL" "RRF6" 40 RPI 817
		       "CLGRTNE",  // 50 "B9616" "CLGRTNE" "RRF6" 40 RPI 817
		       "CLGRTNH",  // 60 "B961C" "CLGRTNH" "RRF6" 40 RPI 817
		       "CLGRTNL",  // 70 "B961A" "CLGRTNL" "RRF6" 40 RPI 817
		       "CRT",      // 150 "B972" "CRT" "RRF5" 39 RPI 817
		       "CRTE",     // 160 "B9728" "CRTE" "RRF6" 40 RPI 817
		       "CRTH",     // 170 "B9722" "CRTH" "RRF6" 40 RPI 817
		       "CRTL",     // 180 "B9724" "CRTL" "RRF6" 40 RPI 817
		       "CRTNE",    // 190 "B9726" "CRTNE" "RRF6" 40 RPI 817
		       "CRTNH",    // 200 "B972C" "CRTNH" "RRF6" 40 RPI 817
		       "CRTNL",    // 210 "B972A" "CRTNL" "RRF6" 40 RPI 817
		       "CLRT",     // 80 "B973" "CLRT" "RRF5" 39 RPI 817
		       "CLRTE",    // 90 "B9738" "CLRTE" "RRF6" 40 RPI 817
		       "CLRTH",    // 100 "B9732" "CLRTH" "RRF6" 40 RPI 817
		       "CLRTL",    // 110 "B9734" "CLRTL" "RRF6" 40 RPI 817
		       "CLRTNE",   // 120 "B9736" "CLRTNE" "RRF6" 40 RPI 817
		       "CLRTNH",   // 130 "B973C" "CLRTNH" "RRF6" 40 RPI 817
		       "CLRTNL",   // 140 "B973A" "CLRTNL" "RRF6" 40 RPI 817
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
		       "PTF",      // 10 "B9A2" "PTF" "RRE" 14 RPI 817
		       "LPTEA",    //      "B9AA" "LPTEA" "RRE" 14 Z9-19
		       "RRBM",     // "B9AE","RRBM","RRE"  14 RPI 1125 Z196 
		       "PFMF",     // 20 "B9AF" "PFMF" "RRF5" 39 RPI 817
		       "CU14",     // 5070 "B9B0" "CU14" "RRE" 14
		       "CU24",     // 5080 "B9B1" "CU24" "RRE" 14
		       "CU41",     // 5090 "B9B2" "CU41" "RRE" 14
		       "CU42",     // 5100 "B9B3" "CU42" "RRE" 14
		       "TRTRE",    // 30 "B9BD" "TRTRE" "RRF5" 39 RPI 817
		       "SRSTU",    // 5110 "B9BE" "SRSTU" "RRE" 14     
		       "TRTE",     // 40 "B9BF" "TRTE" "RRF5" 39 RPI 817
		       "AHHHR",    // "B9C8","AHHHR","RRF5"  39 RPI 1125 Z196
		       "SHHHR",    // "B9C9","SHHHR","RRF5"  39 RPI 1125 Z196
		       "ALHHHR",   // "B9CA","ALHHHR","RRF5" 39 RPI 1125 Z196
		       "SLHHHR",   // "B9CB","SLHHHR","RRF5" 39 RPI 1125 Z196
		       "CHHR",     // "B9CD","CHHR","RRE"     14 RPI 1125 Z196
		       "CLHHR",    // "B9CF","CLHHR","RRE"    14 RPI 1125 Z196
		       "AHHLR",    // "B9D8","AHHLR","RRF5 "  39 RPI 1125 Z196
		       "SHHLR",    // "B9D9","SHHLR","RRF5 "  39 RPI 1125 Z196
		       "ALHHLR",   // "B9DA","ALHHLR","RRF5 " 39 RPI 1125 Z196
		       "SLHHLR",   // "B9DB","SLHHLR","RRF5 " 39 RPI 1125 Z196
		       "CHLR",     // "B9DD","CHLR","RRE"     14 RPI 1125 Z196
		       "CLHLR",    // "B9DF","CLHLR","RRE"    14 RPI 1125 Z196
		       "POPCNT",   // 5115 "B9E1" "POPCNT" "RRE" 14 RPI 1125
		       "LOCGR",    // "B9E2","LOCGR","RRF5"   39 RPI 1125 Z196
		       "NGRK",     // "B9E4","NGRK","RRF5"    39 RPI 1125 Z196
		       "OGRK",     // "B9E6","OGRK","RRF5"    39 RPI 1125 Z196
		       "XGRK",     // "B9E7","XGRK","RRF5"    39 RPI 1125 Z196
		       "AGRK",     // "B9E8","AGRK","RRF5"    39 RPI 1125 Z196
		       "SGRK",     // "B9E9","SGRK","RRF5"    39 RPI 1125 Z196
		       "ALGRK",    // "B9EA","ALGRK","RRF5"   39 RPI 1125 Z196
		       "SLGRK",    // "B9EB","SLGRK","RRF5"   39 RPI 1125 Z196
		       "LOCR",     // "B9F2","LOCR","RRF5"    39 RPI 1125 Z196
		       "NRK",      // "B9F4","NRK","RRF5"     39 RPI 1125 Z196
		       "ORK",      // "B9F6","ORK","RRF5"     39 RPI 1125 Z196
		       "XRK",      // "B9F7","XRK","RRF5"     39 RPI 1125 Z196
		       "ARK",      // "B9F8","ARK","RRF5"     39 RPI 1125 Z196
		       "SRK",      // "B9F9","SRK","RRF5"     39 RPI 1125 Z196
		       "ALRK",     // "B9FA","ALRK","RRF5"    39 RPI 1125 Z196
		       "SLRK",     // "B9FB","SLRK","RRF5"    39 RPI 1125 Z196     
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
		       "MSGFI",    // 50 "C20" "MSGFI" "RIL" 16 RPI 817
		       "MSFI",     // 60 "C21" "MSFI" "RIL" 16 RPI 817
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
		       "LLHRL",    // 70 "C42" "LLHRL" "RIL" 16 RPI 817
		       "LGHRL",    // 80 "C44" "LGHRL" "RIL" 16 RPI 817
		       "LHRL",     // 90 "C45" "LHRL" "RIL" 16 RPI 817
		       "LLGHRL",   // 100 "C46" "LLGHRL" "RIL" 16 RPI 817
		       "STHRL",    // 110 "C47" "STHRL" "RIL" 16 RPI 817
		       "LGRL",     // 120 "C48" "LGRL" "RIL" 16 RPI 817
		       "STGRL",    // 130 "C4B" "STGRL" "RIL" 16 RPI 817
		       "LGFRL",    // 140 "C4C" "LGFRL" "RIL" 16 RPI 817
		       "LRL",      // 150 "C4D" "LRL" "RIL" 16 RPI 817
		       "LLGFRL",   // 160 "C4E" "LLGFRL" "RIL" 16 RPI 817
		       "STRL",     // 170 "C4F" "STRL" "RIL" 16 RPI 817
		       "EXRL",     // 180 "C60" "EXRL" "RIL" 16 RPI 817
		       "PFDRL",    // 190 "C62" "PFDRL" "RIL" 16 RPI 817
		       "CGHRL",    // 200 "C64" "CGHRL" "RIL" 16 RPI 817
		       "CHRL",     // 210 "C65" "CHRL" "RIL" 16 RPI 817
		       "CLGHRL",   // 220 "C66" "CLGHRL" "RIL" 16 RPI 817
		       "CLHRL",    // 230 "C67" "CLHRL" "RIL" 16 RPI 817
		       "CGRL",     // 240 "C68" "CGRL" "RIL" 16 RPI 817
		       "CLGRL",    // 250 "C6A" "CLGRL" "RIL" 16 RPI 817
		       "CGFRL",    // 260 "C6C" "CGFRL" "RIL" 16 RPI 817
		       "CRL",      // 270 "C6D" "CRL" "RIL" 16 RPI 817
		       "CLGFRL",   // 280 "C6E" "CLGFRL" "RIL" 16 RPI 817
		       "CLRL",     // 290 "C6F" "CLRL" "RIL" 16 RPI 817
		       "MVCOS",    //      "C80" "MVCOS" "SSF" 32 Z9-41 // RPI 606
		       "ECTG",     //      "C81" "ECTG" "SSF" 32  // RPI 1013
		       "CSST",     //      "C82" "CSST" "SSF" 32  // RPI 1013
		       "LPD",      // "C84","LPD","SSF2"  55 RPI 1125 Z196
		       "LPDG",     // "C85","LPDG","SSF2" 55 RPI 1125 Z196
		       "BRCTH",    // "CC6","BRCTH","RIL"  16 RPI 1125 Z196
		       "AIH",      // "CC8","AIH","RIL"    16  RPI 1125 Z196
		       "ALSIH",    // "CCA","ALSIH","RIL"  16  RPI 1125 Z196
		       "ALSIHN",   // "CCB","ALSIHN","RIL" 16  RPI 1125 Z196
		       "CIH",      // "CCD","CIH","RIL"    16  RPI 1125 Z196
		       "CLIH",     // "CCF","CLIH","RIL"   16  RPI 1125 Z196		       
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
		       "XREAD",    // 5375 "E00" "XREAD" "RXSS" 38 RPI 812
		       "XPRNT",    // 5375 "E02" "XPRNT" "RXSS" 38 RPI 812
		       "XPNCH",    // 5375 "E04" "XPNCH" "RXSS" 38 RPI 812
		       "XDUMP",    // 5375 "E06" "XDUMP" "RXSS" 38 RPI 812
		       "XLIMD",    // 5375 "E08" "XLIMD" "RXSS" 38 RPI 812
		       "XGET",     // 5375 "E0A" "XGET"  "RXSS" 38 RPI 812
		       "XPUT",     // 5375 "E0C" "XPUT"  "RXSS" 38 RPI 812
		       "PKU",      // 5380 "E1" "PKU" "RXSS" 17
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
		       "LTGF",     // 310 "E332" "LTGF" "RXY" 18 RPI 817
		       "CGH",      // 320 "E334" "CGH" "RXY" 18 RPI 817
		       "PFD",      // 330 "E336" "PFD" "RXY" 18 RPI 817
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
		       "MFY",      // 340 "E35C" "MFY" "RXY" 18 RPI 817
		       "ALY",      // 5850 "E35E" "ALY" "RXY" 18
		       "SLY",      // 5860 "E35F" "SLY" "RXY" 18
		       "STHY",     // 5870 "E370" "STHY" "RXY" 18
		       "LAY",      // 5880 "E371" "LAY" "RXY" 18
		       "STCY",     // 5890 "E372" "STCY" "RXY" 18
		       "ICY",      // 5900 "E373" "ICY" "RXY" 18
		       "LAEY",     // 350 "E375" "LAEY" "RXY" 18 RPI 817
		       "LB",       // 5910 "E376" "LB" "RXY" 18
		       "LGB",      // 5920 "E377" "LGB" "RXY" 18
		       "LHY",      // 5930 "E378" "LHY" "RXY" 18
		       "CHY",      // 5940 "E379" "CHY" "RXY" 18
		       "AHY",      // 5950 "E37A" "AHY" "RXY" 18
		       "SHY",      // 5960 "E37B" "SHY" "RXY" 18
		       "MHY",      // 360 "E37C" "MHY" "RXY" 18 RPI 817
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
		       "LBH",  // "E3C0","LBH","RXY"   18  RPI 1125 Z196
		       "LLCH",  // "E3C2","LLCH","RXY"  18  RPI 1125 Z196
		       "STCH",  // "E3C3","STCH","RXY"  18  RPI 1125 Z196
		       "LHH",  // "E3C4","LHH","RXY"   18  RPI 1125 Z196
		       "LLHH",  // "E3C6","LLHH","RXY"  18  RPI 1125 Z196
		       "STHH",  // "E3C7","STHH","RXY"  18  RPI 1125 Z196
		       "LFH",  // "E3CA","LFH","RXY"   18  RPI 1125 Z196
		       "STFH",  // "E3CB","STFH","RXY"  18  RPI 1125 Z196
		       "CHF",  // "E3CD","CHF","RXY"   18  RPI 1125 Z196
		       "CLHF",  // "E3CF","CLHF","RXY"  18  RPI 1125 Z196
		       "LASP",     // 6120 "E500" "LASP" "SSE" 19
		       "TPROT",    // 6130 "E501" "TPROT" "SSE" 19
		       "STRAG",    // 6140 "E502" "STRAG" "SSE" 19
		       "MVCSK",    // 6150 "E50E" "MVCSK" "SSE" 19
		       "MVCDK",    // 6160 "E50F" "MVCDK" "SSE" 19
		       "MVHHI",    // 370 "E544" "MVHHI" "SIL" 51 RPI 817
		       "MVGHI",    // 380 "E548" "MVGHI" "SIL" 51 RPI 817
		       "MVHI",     // 390 "E54C" "MVHI" "SIL" 51 RPI 817
		       "CHHSI",    // 400 "E554" "CHHSI" "SIL" 51 RPI 817
		       "CLHHSI",   // 410 "E555" "CLHHSI" "SIL" 51 RPI 817
		       "CGHSI",    // 420 "E558" "CGHSI" "SIL" 51 RPI 817
		       "CLGHSI",   // 430 "E559" "CLGHSI" "SIL" 51 RPI 817
		       "CHSI",     // 440 "E55C" "CHSI" "SIL" 51 RPI 817
		       "CLFHSI",   // 450 "E55D" "CLFHSI" "SIL" 51 RPI 817
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
		       "ECAG",     // 460 "EB4C" "ECAG" "RSY" 20 RPI 817
		       "TMY",      // 6420 "EB51" "TMY" "SIY" 21
		       "MVIY",     // 6430 "EB52" "MVIY" "SIY" 21
		       "NIY",      // 6440 "EB54" "NIY" "SIY" 21
		       "CLIY",     // 6450 "EB55" "CLIY" "SIY" 21
		       "OIY",      // 6460 "EB56" "OIY" "SIY" 21
		       "XIY",      // 6470 "EB57" "XIY" "SIY" 21
		       "ASI",      // 470 "EB6A" "ASI" "SIY" 21 RPI 817
		       "ALSI",     // 480 "EB6E" "ALSI" "SIY" 21 RPI 817
		       "AGSI",     // 490 "EB7A" "AGSI" "SIY" 21 RPI 817
		       "ALGSI",    // 500 "EB7E" "ALGSI" "SIY" 21 RPI 817
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
		       "SRAK",  //  "EBDC","SRAK","RSY  "  20 RPI 1125 Z196
		       "SLAK",  //  "EBDD","SLAK","RSY  "  20 RPI 1125 Z196
		       "SRLK",  //  "EBDE","SRLK","RSY  "  20 RPI 1125 Z196
		       "SLLK",  //  "EBDF","SLLK","RSY  "  20 RPI 1125 Z196
		       "LOCG",  //  "EBE2","LOCG","RSY2 "  20 RPI 1125 Z196
		       "STOCG", //  "EBE3","STOCG","RSY2 " 20 RPI 1125 Z196
		       "LANG",  //  "EBE4","LANG","RSY  "  20 RPI 1125 Z196
		       "LAOG",  //  "EBE6","LAOG","RSY  "  20 RPI 1125 Z196
		       "LAXG",  //  "EBE7","LAXG","RSY  "  20 RPI 1125 Z196
		       "LAAG",  //  "EBE8","LAAG","RSY  "  20 RPI 1125 Z196
		       "LAALG", //  "EBEA","LAALG","RSY  " 20 RPI 1125 Z196
		       "LOC",   //  "EBF2","LOC","RSY2 "   20 RPI 1125 Z196
		       "STOC",  //  "EBF3","STOC","RSY2 "  20 RPI 1125 Z196
		       "LAN",   //  "EBF4","LAN","RSY  "   20 RPI 1125 Z196
		       "LAO",   //  "EBF6","LAO","RSY  "   20 RPI 1125 Z196
		       "LAX",   //  "EBF7","LAX","RSY  "   20 RPI 1125 Z196
		       "LAA",   //  "EBF8","LAA","RSY  "   20 RPI 1125 Z196
		       "LAAL",  //  "EBFA","LAAL","RSY  "  20 RPI 1125 Z196
		       "BRXHG",    // 6580 "EC44" "BRXHG" "RIE" 23
		       "JXHG",     // 6590 "EC44" "JXHG" "RIE" 23
		       "BRXLG",    // 6600 "EC45" "BRXLG" "RIE" 23
		       "JXLEG",    // 6610 "EC45" "JXLEG" "RIE" 23    
		       "RISBLG",  // "EC51","RISBLG","RIE8"  52 RPI 1125 Z196
		       "LLHFR",  // 'EC51$003132','LOAD (lOW  && HIGH) RISBLGZ','LLHFR','RIE8',52  RPI 1164
		       "LLHLHR",  // 'EC51$163132','LOAD LOG HW (lOW  && HIGH) RISBLGZ','LLHLHR','RIE8',52  RPI 1164
		       "LLCLHR",  // 'EC51$243132','LOAD LOG CH (lOW  && HIGH) RISBLGZ','LLCLHR','RIE8',52  RPI 1164
		       "RISBLGZ",  // "EC51Z","RISBLGZ","RIE8"  52 RPI 1125 Z196  RPI 1164
		       "RNSBG",    // 510 "EC54" "RNSBG" "RIE8" 52 RPI 817
		       "NHHR",     // 'EC54$003100','AND HIGH (HIGH && HIGH) RNSBG','NHHR','RIE8',52  RPI 1164
		       "NHLR",     // 'EC54$003132','AND HIGH (HIGH && LOW ) RNSBG','NHLR','RIE8',52  RPI 1164
		       "NLHR",     // 'EC54$326332','AND HIGH (lOW  && HIGH) RNSBG','NLHR','RIE8',52  RPI 1164
		       "RNSBGT",   // 520 "EC54T" "RNSBGT" "RIE8" 52 RPI 817
		       "RISBG",    // 530 "EC55" "RISBG" "RIE8" 52 RPI 817
		       "RISBGZ",   // 540 "EC55Z" "RISBGZ" "RIE8" 52 RPI 817
		       "ROSBG",    // 550 "EC56" "ROSBG" "RIE8" 52 RPI 817
		       "OHHR",     // 'EC56$003100','OR  HIGH (HIGH && HIGH) ROSBG','OHHR','RIE8',52  RPI 1164
		       "OHLR",     // 'EC56$003132','OR  HIGH (HIGH && LOW ) ROSBG','OHLR','RIE8',52  RPI 1164
		       "OLHR",     // 'EC56$326332','OR  HIGH (lOW  && HIGH) ROSBG','OLHR','RIE8',52  RPI 1164
		       "ROSBGT",   // 560 "EC56T" "ROSBGT" "RIE8" 52 RPI 817
		       "RXSBG",    // 570 "EC57" "RXSBG" "RIE8" 52 RPI 817
		       "XHHR",     // 'EC57$003100','XOR HIGH (HIGH && HIGH) RXSBG','XHHR','RIE8',52  RPI 1164
		       "XHLR",     // 'EC57$003132','XOR HIGH (HIGH && LOW ) RXSBG','XHLR','RIE8',52  RPI 1164
		       "XLHR",     // 'EC57$326332','AOR HIGH (lOW  && HIGH) RXSBG','XLHR','RIE8',52  RPI 1164	
		       "RXSBGT",   // 580 "EC57T" "RXSBGT" "RIE8" 52 RPI 817
		       "RISBHG",  // "EC5D","RISBHG","RIE8"  52 RPI 1125 Z196
		       "LHHR",    // 'EC5D$003100','LOAD (HIGH && HIGH) RISBHGZ','LHHR','RIE8',52  RPI 1164
		       "LHLR",    // 'EC5D$003132','LOAD (HIGH && LOW ) RISBHGZ','LHLR','RIE8',52  RPI 1164
		       "LLHHHR",  // 'EC5D$163100','LOAD LOG HW (HIGH && HIGH) RISBHGZ','LLHHHR','RIE8',52  RPI 1164
		       "LLHHLR",  // 'EC5D$163132','LOAD LOG HW (HIGH && LOW ) RISBHGZ','LLHHLR','RIE8',52  RPI 1164
		       "LLCHHR",  // 'EC5D$243100','LOAD LOG CH (HIGH && HIGH) RISBHGZ','LLCHHR','RIE8',52  RPI 1164
		       "LLCHLR",  // 'EC5D$243132','LOAD LOG CH (HIGH && LOW ) RISBHGZ','LLCHLR','RIE8',52  RPI 1164
		       "RISBHGZ",  // "EC5DZ","RISBHGZ","RIE8"  52 RPI 1125 Z196  RPI 1164
		       "CGRJ",     // 10 "EC64" "CGRJ" "RIE6" 49 RPI 817
		       "CGRJE",    // 20 "EC648" "CGRJE" "RIE7" 50 RPI 817
		       "CGRJH",    // 30 "EC642" "CGRJH" "RIE7" 50 RPI 817
		       "CGRJL",    // 40 "EC644" "CGRJL" "RIE7" 50 RPI 817
		       "CGRJNE",   // 50 "EC646" "CGRJNE" "RIE7" 50 RPI 817
		       "CGRJNH",   // 60 "EC64C" "CGRJNH" "RIE7" 50 RPI 817
		       "CGRJNL",   // 70 "EC64A" "CGRJNL" "RIE7" 50 RPI 817
		       "CLGRJ",    // 80 "EC65" "CLGRJ" "RIE6" 49 RPI 817
		       "CLGRJE",   // 90 "EC658" "CLGRJE" "RIE7" 50 RPI 817
		       "CLGRJH",   // 100 "EC652" "CLGRJH" "RIE7" 50 RPI 817
		       "CLGRJL",   // 110 "EC654" "CLGRJL" "RIE7" 50 RPI 817
		       "CLGRJNE",  // 120 "EC656" "CLGRJNE" "RIE7" 50 RPI 817
		       "CLGRJNH",  // 130 "EC65C" "CLGRJNH" "RIE7" 50 RPI 817
		       "CLGRJNL",  // 140 "EC65A" "CLGRJNL" "RIE7" 50 RPI 817		       
		       "CGIT",     // 1010 "EC70" "CGIT" "RIE2" 41 RPI 817
		       "CGITE",    // 1020 "EC708" "CGITE" "RIE3" 42 RPI 817
		       "CGITH",    // 1030 "EC702" "CGITH" "RIE3" 42 RPI 817
		       "CGITL",    // 1040 "EC704" "CGITL" "RIE3" 42 RPI 817
		       "CGITNE",   // 1050 "EC706" "CGITNE" "RIE3" 42 RPI 817
		       "CGITNH",   // 1060 "EC70C" "CGITNH" "RIE3" 42 RPI 817
		       "CGITNL",   // 1070 "EC70A" "CGITNL" "RIE3" 42 RPI 817
		       "CLGIT",    // 150 "EC71" "CLGIT" "RIE2" 41 RPI 817
		       "CLGITE",   // 160 "EC718" "CLGITE" "RIE3" 42 RPI 817
		       "CLGITH",   // 170 "EC712" "CLGITH" "RIE3" 42 RPI 817
		       "CLGITL",   // 180 "EC714" "CLGITL" "RIE3" 42 RPI 817
		       "CLGITNE",  // 190 "EC716" "CLGITNE" "RIE3" 42 RPI 817
		       "CLGITNH",  // 200 "EC71C" "CLGITNH" "RIE3" 42 RPI 817
		       "CLGITNL",  // 210 "EC71A" "CLGITNL" "RIE3" 42 RPI 817
		       "CIT",      // 1150 "EC72" "CIT" "RIE2" 41 RPI 817
		       "CITE",     // 1160 "EC728" "CITE" "RIE3" 42 RPI 817
		       "CITH",     // 1170 "EC722" "CITH" "RIE3" 42 RPI 817
		       "CITL",     // 1180 "EC724" "CITL" "RIE3" 42 RPI 817
		       "CITNE",    // 1190 "EC726" "CITNE" "RIE3" 42 RPI 817
		       "CITNH",    // 1200 "EC72C" "CITNH" "RIE3" 42 RPI 817
		       "CITNL",    // 1210 "EC72A" "CITNL" "RIE3" 42 RPI 817
		       "CLFIT",    // 220 "EC73" "CLFIT" "RIE2" 41 RPI 817
		       "CLFITE",   // 230 "EC738" "CLFITE" "RIE3" 42 RPI 817
		       "CLFITH",   // 240 "EC732" "CLFITH" "RIE3" 42 RPI 817
		       "CLFITL",   // 250 "EC734" "CLFITL" "RIE3" 42 RPI 817
		       "CLFITNE",  // 260 "EC736" "CLFITNE" "RIE3" 42 RPI 817
		       "CLFITNH",  // 270 "EC73C" "CLFITNH" "RIE3" 42 RPI 817
		       "CLFITNL",  // 280 "EC73A" "CLFITNL" "RIE3" 42 RPI 817
		       "CRJ",      // 150 "EC76" "CRJ" "RIE6" 49 RPI 817
		       "CRJE",     // 160 "EC768" "CRJE" "RIE7" 50 RPI 817
		       "CRJH",     // 170 "EC762" "CRJH" "RIE7" 50 RPI 817
		       "CRJL",     // 180 "EC764" "CRJL" "RIE7" 50 RPI 817
		       "CRJNE",    // 190 "EC766" "CRJNE" "RIE7" 50 RPI 817
		       "CRJNH",    // 200 "EC76C" "CRJNH" "RIE7" 50 RPI 817
		       "CRJNL",    // 210 "EC76A" "CRJNL" "RIE7" 50 RPI 817
		       "CLRJ",     // 220 "EC77" "CLRJ" "RIE6" 49 RPI 817
		       "CLRJE",    // 230 "EC778" "CLRJE" "RIE7" 50 RPI 817
		       "CLRJH",    // 240 "EC772" "CLRJH" "RIE7" 50 RPI 817
		       "CLRJL",    // 250 "EC774" "CLRJL" "RIE7" 50 RPI 817
		       "CLRJNE",   // 260 "EC776" "CLRJNE" "RIE7" 50 RPI 817
		       "CLRJNH",   // 270 "EC77C" "CLRJNH" "RIE7" 50 RPI 817
		       "CLRJNL",   // 280 "EC77A" "CLRJNL" "RIE7" 50 RPI 817
		       "CGIJ",     // 290 "EC7C" "CGIJ" "RIE4" 43 RPI 817
		       "CGIJE",    // 300 "EC7C8" "CGIJE" "RIE5" 44 RPI 817
		       "CGIJH",    // 310 "EC7C2" "CGIJH" "RIE5" 44 RPI 817
		       "CGIJL",    // 320 "EC7C4" "CGIJL" "RIE5" 44 RPI 817
		       "CGIJNE",   // 330 "EC7C6" "CGIJNE" "RIE5" 44 RPI 817
		       "CGIJNH",   // 340 "EC7CC" "CGIJNH" "RIE5" 44 RPI 817
		       "CGIJNL",   // 350 "EC7CA" "CGIJNL" "RIE5" 44 RPI 817
		       "CLGIJ",    // 360 "EC7D" "CLGIJ" "RIE4" 43 RPI 817
		       "CLGIJE",   // 370 "EC7D8" "CLGIJE" "RIE5" 44 RPI 817
		       "CLGIJH",   // 380 "EC7D2" "CLGIJH" "RIE5" 44 RPI 817
		       "CLGIJL",   // 390 "EC7D4" "CLGIJL" "RIE5" 44 RPI 817
		       "CLGIJNE",  // 400 "EC7D6" "CLGIJNE" "RIE5" 44 RPI 817
		       "CLGIJNH",  // 410 "EC7DC" "CLGIJNH" "RIE5" 44 RPI 817
		       "CLGIJNL",  // 420 "EC7DA" "CLGIJNL" "RIE5" 44 RPI 817
		       "CIJ",      // 430 "EC7E" "CIJ" "RIE4" 43 RPI 817
		       "CIJE",     // 440 "EC7E8" "CIJE" "RIE5" 44 RPI 817
		       "CIJH",     // 450 "EC7E2" "CIJH" "RIE5" 44 RPI 817
		       "CIJL",     // 460 "EC7E4" "CIJL" "RIE5" 44 RPI 817
		       "CIJNE",    // 470 "EC7E6" "CIJNE" "RIE5" 44 RPI 817
		       "CIJNH",    // 480 "EC7EC" "CIJNH" "RIE5" 44 RPI 817
		       "CIJNL",    // 490 "EC7EA" "CIJNL" "RIE5" 44 RPI 817
		       "CLIJ",     // 500 "EC7F" "CLIJ" "RIE4" 43 RPI 817
		       "CLIJE",    // 510 "EC7F8" "CLIJE" "RIE5" 44 RPI 817
		       "CLIJH",    // 520 "EC7F2" "CLIJH" "RIE5" 44 RPI 817
		       "CLIJL",    // 530 "EC7F4" "CLIJL" "RIE5" 44 RPI 817
		       "CLIJNE",   // 540 "EC7F6" "CLIJNE" "RIE5" 44 RPI 817
		       "CLIJNH",   // 550 "EC7FC" "CLIJNH" "RIE5" 44 RPI 817
		       "CLIJNL",   // 560 "EC7FA" "CLIJNL" "RIE5" 44 RPI 817
		       "AHIK",  // "ECD8","AHIK","RIE9"    57 RPI 1125 Z196
		       "AGHIK",  // "ECD9","AGHIK","RIE9"   57 RPI 1125 Z196
		       "ALHSIK",  // "ECDA","ALHSIK","RIE9"  57 RPI 1125 Z196
		       "ALGHSIK",  // "ECDB","ALGHSIK","RIE9" 57 RPI 1125 Z196
		       "CGRB",     // 570 "ECE4" "CGRB" "RRS1" 45 RPI 817
		       "CGRBE",    // 580 "ECE48" "CGRBE" "RRS2" 46 RPI 817
		       "CGRBH",    // 590 "ECE42" "CGRBH" "RRS2" 46 RPI 817
		       "CGRBL",    // 600 "ECE44" "CGRBL" "RRS2" 46 RPI 817
		       "CGRBNE",   // 610 "ECE46" "CGRBNE" "RRS2" 46 RPI 817
		       "CGRBNH",   // 620 "ECE4C" "CGRBNH" "RRS2" 46 RPI 817
		       "CGRBNL",   // 630 "ECE4A" "CGRBNL" "RRS2" 46 RPI 817
		       "CLGRB",    // 640 "ECE5" "CLGRB" "RRS1" 45 RPI 817
		       "CLGRBE",   // 650 "ECE58" "CLGRBE" "RRS2" 46 RPI 817
		       "CLGRBH",   // 660 "ECE52" "CLGRBH" "RRS2" 46 RPI 817
		       "CLGRBL",   // 670 "ECE54" "CLGRBL" "RRS2" 46 RPI 817
		       "CLGRBNE",  // 680 "ECE56" "CLGRBNE" "RRS2" 46 RPI 817
		       "CLGRBNH",  // 690 "ECE5C" "CLGRBNH" "RRS2" 46 RPI 817
		       "CLGRBNL",  // 700 "ECE5A" "CLGRBNL" "RRS2" 46 RPI 817
		       "CRB",      // 710 "ECF6" "CRB" "RRS1" 45 RPI 817
		       "CRBE",     // 720 "ECF68" "CRBE" "RRS2" 46 RPI 817
		       "CRBH",     // 730 "ECF62" "CRBH" "RRS2" 46 RPI 817
		       "CRBL",     // 740 "ECF64" "CRBL" "RRS2" 46 RPI 817
		       "CRBNE",    // 750 "ECF66" "CRBNE" "RRS2" 45 RPI 817
		       "CRBNH",    // 760 "ECF6C" "CRBNH" "RRS2" 46 RPI 817
		       "CRBNL",    // 770 "ECF6A" "CRBNL" "RRS2" 46 RPI 817
		       "CLRB",     // 780 "ECF7" "CLRB" "RRS1" 45 RPI 817
		       "CLRBE",    // 790 "ECF78" "CLRBE" "RRS2" 46 RPI 817
		       "CLRBH",    // 800 "ECF72" "CLRBH" "RRS2" 46 RPI 817
		       "CLRBL",    // 810 "ECF74" "CLRBL" "RRS2" 46 RPI 817
		       "CLRBNE",   // 820 "ECF76" "CLRBNE" "RRS2" 46 RPI 817
		       "CLRBNH",   // 830 "ECF7C" "CLRBNH" "RRS2" 46 RPI 817
		       "CLRBNL",   // 840 "ECF7A" "CLRBNL" "RRS2" 46 RPI 817
		       "CGIB",     // 850 "ECFC" "CGIB" "RRS3" 47 RPI 817
		       "CGIBE",    // 860 "ECFC8" "CGIBE" "RRS4" 48 RPI 817
		       "CGIBH",    // 870 "ECFC2" "CGIBH" "RRS4" 48 RPI 817
		       "CGIBL",    // 880 "ECFC4" "CGIBL" "RRS4" 48 RPI 817
		       "CGIBNE",   // 890 "ECFC6" "CGIBNE" "RRS4" 48 RPI 817
		       "CGIBNH",   // 900 "ECFCC" "CGIBNH" "RRS4" 48 RPI 817
		       "CGIBNL",   // 910 "ECFCA" "CGIBNL" "RRS4" 48 RPI 817
		       "CLGIB",    // 920 "ECFD" "CLGIB" "RRS3" 47 RPI 817
		       "CLGIBE",   // 930 "ECFD8" "CLGIBE" "RRS4" 48 RPI 817
		       "CLGIBH",   // 940 "ECFD2" "CLGIBH" "RRS4" 48 RPI 817
		       "CLGIBL",   // 950 "ECFD4" "CLGIBL" "RRS4" 48 RPI 817
		       "CLGIBNE",  // 960 "ECFD6" "CLGIBNE" "RRS4" 48 RPI 817
		       "CLGIBNH",  // 970 "ECFDC" "CLGIBNH" "RRS4" 48 RPI 817
		       "CLGIBNL",  // 980 "ECFDA" "CLGIBNL" "RRS4" 48 RPI 817
		       "CIB",      // 990 "ECFE" "CIB" "RRS3" 47 RPI 817
		       "CIBE",     // 1000 "ECFE8" "CIBE" "RRS4" 48 RPI 817
		       "CIBH",     // 1010 "ECFE2" "CIBH" "RRS4" 48 RPI 817
		       "CIBL",     // 1020 "ECFE4" "CIBL" "RRS4" 48 RPI 817
		       "CIBNE",    // 1030 "ECFE6" "CIBNE" "RRS4" 48 RPI 817
		       "CIBNH",    // 1040 "ECFEC" "CIBNH" "RRS4" 48 RPI 817
		       "CIBNL",    // 1050 "ECFEA" "CIBNL" "RRS4" 48 RPI 817
		       "CLIB",     // 1060 "ECFF" "CLIB" "RRS3" 47 RPI 817
		       "CLIBE",    // 1070 "ECFF8" "CLIBE" "RRS4" 48 RPI 817
		       "CLIBH",    // 1080 "ECFF2" "CLIBH" "RRS4" 48 RPI 817
		       "CLIBL",    // 1090 "ECFF4" "CLIBL" "RRS4" 48 RPI 817
		       "CLIBNE",   // 1100 "ECFF6" "CLIBNE" "RRS4" 48 RPI 817
		       "CLIBNH",   // 1110 "ECFFC" "CLIBNH" "RRS4" 48 RPI 817
		       "CLIBNL",   // 1120 "ECFFA" "CLIBNL" "RRS4" 48 RPI 817		       
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
		       "ICTL",    // 7510  "ICTL"  138 
		       "ISEQ",    // 7520  "ISEQ"  139 
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
         4,  //37 "RXAS" RX    if ASSIST
         6,   //38 "RXSS" RX-SS if ASSIST else PKU x'E1'
         4,   //39 "RRF5" CRT   RPI 817
         4,   //40 "RRF6" CRTE  RPI 817
         6,   //41 "RIE2" CIT   RPI 817
         6,   //42 "RIE3" CITE  RPI 817
         6,   //43 "RIE4" CGIJ  RPI 817
         6,   //44 "RIE5" CGIJE RPI 817
         6,   //45 "RRS1" CGRB  RPI 817
         6,   //46 "RRS2" CGRBE RPI 817
         6,   //47 "RRS3" CGIB  RPI 817
         6,   //48 "RRS4" CGIBE RPI 817
         6,   //49 "RIE6" CGRJ  RPI 817
         6,   //50 "RIE7" CGRJE RPI 817
         6,   //51 "SIL"  MVHHI RPI 817
         6,   //52 "RIE2"
         4,   //53 
         4,   //54
         6,   //55
         6,   //56
         6,   //57
    };
	int    max_op_type_offset = 57; // see changes required RPI 812, RPI 817, RPI 1125
    int    max_ins_type = 100;    // RPI 315 
    int    max_asm_type = 200;
    int    max_mac_type = 300;
	//  When adding new opcode case: // RPI 407 type 35 for CSDTR etc
	//  1.  Increase the above max.
	//  2.  Change above op_type_len table which must match
    //  3.  Change az390 instruction format cases and
    //  4.  Change pz390 op_type_offset and op_type_mask if new primary opcode
    //  5.  Change pz390 trace_psw to add new case formats
  	int[]    op_type  = {
			   0,  // comments
		       1,  // 10 "0101" "PR" "E" 1
		       1,  // 20 "0102" "UPT" "E" 1		       
		       1,  //    "0104" "PTFF" "E" 1 Z9-1
		       1,  // 30 "0107" "SCKPF" "E" 1
		       1,  // 40 "010A" "PFPO" "E" 1   RPI 1013
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
		       37, // 1193 "52" "XDECO" "RX" 37 RPI 812
		       37, // 1196 "53" "XDECI" "RX" 37 RPI 812	
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
		       37, // 1323 "61" "XHEXI" "RX" 37 RPI 812
		       37, // 1326 "62" "XHEXO" "RX" 37 RPI 812
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
		       7,  // 3392 "B2B8" "SRNMB" "S" 7 RPI 1125
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
		       53,  // 3860 "B344" "LEDBR?" "RRE" 53 RPI 1125
		       53,  // 3870 "B345" "LDXBR?" "RRE" 53 RPI 1125
		       53,  // 3880 "B346" "LEXBR?" "RRE" 53 RPI 1125
		       54,  // 3890 "B347" "FIXBR?" "RRF2" 54 RPI 1125
		       14,  // 3900 "B348" "KXBR" "RRE" 14
		       14,  // 3910 "B349" "CXBR" "RRE" 14
		       14,  // 3920 "B34A" "AXBR" "RRE" 14
		       14,  // 3930 "B34B" "SXBR" "RRE" 14
		       14,  // 3940 "B34C" "MXBR" "RRE" 14
		       14,  // 3950 "B34D" "DXBR" "RRE" 14
		       34,  // 3960 "B350" "TBEDR" "RRF2" 34
		       34,  // 3970 "B351" "TBDR" "RRF2" 34
		       30,  // 3980 "B353" "DIEBR" "RRF3" 30
		       54,  // 3990 "B357" "FIEBR?" "RRF2" 54 RPI 1125
		       14,  // 4000 "B358" "THDER" "RRE" 14
		       14,  // 4010 "B359" "THDR" "RRE" 14
		       30,  // 4020 "B35B" "DIDBR" "RRF3" 30
		       54,  // 4030 "B35F" "FIDBR?" "RRF2" 54 RPI 1125
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
		       30,  //      "B390" "CELFBR" "RRF3" 30 RPI 1125 Z196
               30,  //      "B391" "CDLFBR" "RRF3" 30 RPI 1125 Z196
               30,  //      "B392" "CXLFBR" "RRF3" 30 RPI 1125 Z196
		       53,  // 4190 "B394" "CEFBR?" "RRE" 53 RPI 1125 Z196
		       53,  // 4200 "B395" "CDFBR?" "RRE" 53 RPI 1125 Z196
		       53,  // 4210 "B396" "CXFBR?" "RRE" 53 RPI 1125 Z196
		       54,  // 4220 "B398" "CFEBR?" "RRF" 54 RPI 1125 Z196 was RRF2 34
		       54,  // 4230 "B399" "CFDBR?" "RRF" 54 RPI 1125 Z196 was RRF2 34
		       54,  // 4240 "B39A" "CFXBR?" "RRF" 54 RPI 1125 Z196 was RRF2 34
		       30,    // 'B39C' 'CLFEBR' 'RRF3' 30 RPI 1125 Z196
               30,    // 'B39D' 'CLFDBR' 'RRF3' 30 RPI 1125 Z196
               30,    // 'B39E' 'CLFXBR' 'RRF3' 30 RPI 1125 Z196
               30,    // 'B3A0' 'CELGBR' 'RRF3' 30 RPI 1125 Z196
               30,    // 'B3A1' 'CDLGBR' 'RRF3' 30 RPI 1125 Z196
               30,    // 'B3A2' 'CXLGBR' 'RRF3' 30 RPI 1125 Z196
		       53,  // 4250 "B3A4" "CEGBR" "RRE" 53 RPI 1125 Z196
		       53,  // 4260 "B3A5" "CDGBR" "RRE" 53 RPI 1125 Z196
		       53,  // 4270 "B3A6" "CXGBR" "RRE" 53 RPI 1125 Z196
		       54,  // 4280 "B3A8" "CGEBR" "RRF2" 54 RPI 1125 Z196
		       54,  // 4290 "B3A9" "CGDBR" "RRF2" 54 RPI 1125 Z196
		       54,  // 4300 "B3AA" "CGXBR" "RRF2" 54 RPI 1125 Z196
		       30,    //      "B3AC" "CLGEBR" "RRF2" 30  RPI 1125 Z196
               30,    //      "B3AD" "CLGDBR" "RRF2" 30  RPI 1125 Z196
               30,    //      "B3AE" "CLGXBR" "RRF2" 30  RPI 1125 Z196		       
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
		       36, // "MDTR?"  "B3D0" "RRR"  DFP 1  RPI 1125
		       36, // "DDTR?"  "B3D1" "RRR"  DFP 2  RPI 1125
		       36, // "ADTR?"  "B3D2" "RRR"  DFP 3  RPI 1125
		       36, // "SDTR?"  "B3D3" "RRR"  DFP 4  RPI 1125
		       35, // "LDETR"  "B3D4" "RRF4" DFP 5  
		       30, // "LEDTR"  "B3D5" "RRF3" DFP 6  
		       14, // "LTDTR"  "B3D6" "RRE"  DFP 7  
		       30, // "FIDTR"  "B3D7" "RRF3" DFP 8  
		       36, // "MXTR?"  "B3D8" "RRR"  DFP 9  RPI 1125
		       36, // "DXTR?"  "B3D9" "RRR"  DFP 10 RPI 1125
		       36, // "AXTR?"  "B3DA" "RRR"  DFP 11 RPI 1125
		       36, // "SXTR?"  "B3DB" "RRR"  DFP 12 RPI 1125
		       35, // "LXDTR" "B3DC" "RRF4" DFP 13
		       30, // "LDXTR" "B3DD" "RRF3" DFP 14
		       14, // "LTXTR" "B3DE" "RRE"  DFP 15
		       30, // "FIXTR" "B3DF" "RRF3" DFP 16
		       14, // "KDTR" "B3E0" "RRE" DFP 17
		       54, // "CGDTR?" "B3E1" "RRF7" DFP 18 RPI 1125
		       14, // "CUDTR" "B3E2" "RRE" DFP 19
		       35, // "CSDTR" "B3E3" "RRF4" DFP 20
		       14, // "CDTR" "B3E4" "RRE" DFP 21
		       14, // "EEDTR" "B3E5" "RRE" DFP 22
		       14, // "ESDTR" "B3E7" "RRE" DFP 23
		       14, // "KXTR" "B3E8" "RRE" DFP 24
		       54, // "CGXTR?" "B3E9" "RRF7" DFP 25 RPI 1125
		       14, // "CUXTR" "B3EA" "RRE" DFP 26
		       35, // "CSXTR" "B3EB" "RRF4" DFP 27
		       14, // "CXTR" "B3EC" "RRE" DFP 28
		       14, // "EEXTR" "B3ED" "RRE" DFP 29
		       14, // "ESXTR" "B3EF" "RRE" DFP 30
		       53, // "CDGTR?" "B3F1" "RE3" DFP 31 RPI 1125
		       14, // "CDUTR" "B3F2" "RRE" DFP 32
		       14, // "CDSTR" "B3F3" "RRE" DFP 33
		       14, // "CEDTR" "B3F4" "RRE" DFP 34
		       30, // "QADTR" "B3F5" "RRF3" DFP 35
		       34, // "IEDTR" "B3F6" "RRF2" DFP 36
		       30, // "RRDTR" "B3F7" "RRF3" DFP 37
		       53, // "CXGTR?" "B3F9" "RE3" DFP 38 RPI 1125
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
		       14,  // "B928","PCKMO","RE4"  14 RPI 1125 Z196
               14,  // "B92A","KMF","RRE"    14 RPI 1125 Z196
               14,  // "B92B","KMO","RRE"    14 RPI 1125 Z196
               14,  // "B92C","PCC","RE4"    14 RPI 1125 Z196
               34,  // "B92D","KMCTR","RRF2" 34 RPI 1125 Z196
		       14,  // 4770 "B92E" "KM" "RRE" 14
		       14,  // 4780 "B92F" "KMC" "RRE" 14
		       14,  // 4790 "B930" "CGFR" "RRE" 14
		       14,  // 4800 "B931" "CLGFR" "RRE" 14
		       14,  // 4810 "B93E" "KIMD" "RRE" 14
		       14,  // 4820 "B93F" "KLMD" "RRE" 14
		       30,    // "B941","CFDTR","RRF"  30 RPI 1125 Z196
               30,   // "B942","CLGDTR","RRF" 30 RPI 1125 Z196
               30,   // "B943","CLFDTR","RRF" 30 RPI 1125 Z196
		       14,  // 4830 "B946" "BCTGR" "RRE" 14
		       30,    // "B949","CFXTR","RRF3"   30 RPI 1125 Z196
		       30,   // "B94A","CLGXTR","RRF3"  30 RPI 1125 Z196
		       30,   // "B94B","CLFXTR","RRF3"  30 RPI 1125 Z196
		       30,    // "B951","CDFTR","RRF3"   30 RPI 1125 Z196
		       30,   // "B952","CDLGTR","RRF3"  30 RPI 1125 Z196
		       30,   // "B953","CDLFTR","RRF3"  30 RPI 1125 Z196
		       30,    // "B959","CXFTR","RRF3"   30 RPI 1125 Z196" 
		       30,   // "B95A","CXLGTR","RRF3"  30 RPI 1125 Z196
		       30,   // "B95B","CXLFTR","RRF3"  30 RPI 1125 Z196
		       39,  // 10 "B960" "CGRT" "RRF5" 39 RPI 817
		       40,  // 20 "B9608" "CGRTE" "RRF6" 40 RPI 817
		       40,  // 30 "B9602" "CGRTH" "RRF6" 40 RPI 817
		       40,  // 40 "B9604" "CGRTL" "RRF6" 40 RPI 817
		       40,  // 50 "B9606" "CGRTNE" "RRF6" 40 RPI 817
		       40,  // 60 "B960C" "CGRTNH" "RRF6" 40 RPI 817
		       40,  // 70 "B960A" "CGRTNL" "RRF6" 40 RPI 817
		       
		       39,  // 10 "B961" "CLGRT" "RRF5" 39 RPI 817
		       40,  // 20 "B9618" "CLGRTE" "RRF6" 40 RPI 817
		       40,  // 30 "B9612" "CLGRTH" "RRF6" 40 RPI 817
		       40,  // 40 "B9614" "CLGRTL" "RRF6" 40 RPI 817
		       40,  // 50 "B9616" "CLGRTNE" "RRF6" 40 RPI 817
		       40,  // 60 "B961C" "CLGRTNH" "RRF6" 40 RPI 817
		       40,  // 70 "B961A" "CLGRTNL" "RRF6" 40 RPI 817
		       39,  // 150 "B972" "CRT" "RRF5" 39 RPI 817
		       40,  // 160 "B9728" "CRTE" "RRF6" 40 RPI 817
		       40,  // 170 "B9722" "CRTH" "RRF6" 40 RPI 817
		       40,  // 180 "B9724" "CRTL" "RRF6" 40 RPI 817
		       40,  // 190 "B9726" "CRTNE" "RRF6" 40 RPI 817
		       40,  // 200 "B972C" "CRTNH" "RRF6" 40 RPI 817
		       40,  // 210 "B972A" "CRTNL" "RRF6" 40 RPI 817      
		       39,  // 80 "B973" "CLRT" "RRF5" 39 RPI 817
		       40,  // 90 "B9738" "CLRTE" "RRF6" 40 RPI 817
		       40,  // 100 "B9732" "CLRTH" "RRF6" 40 RPI 817
		       40,  // 110 "B9734" "CLRTL" "RRF6" 40 RPI 817
		       40,  // 120 "B9736" "CLRTNE" "RRF6" 40 RPI 817
		       40,  // 130 "B973C" "CLRTNH" "RRF6" 40 RPI 817
		       40,  // 140 "B973A" "CLRTNL" "RRF6" 40 RPI 817
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
		       14,  // 10 "B9A2" "PTF" "RRE" 14  RPI 817
		       14,  //      "B9AA" "LPTEA" "RRE" 14 Z9-196
		       14,  // "B9AE","RRBM","RRE"  14 RPI 1125 Z196
		       39,  // 20 "B9AF" "PFMF" "RRF5" 39  RPI 817
		       14,  // 5070 "B9B0" "CU14" "RRE" 14
		       14,  // 5080 "B9B1" "CU24" "RRE" 14
		       14,  // 5090 "B9B2" "CU41" "RRE" 14
		       14,  // 5100 "B9B3" "CU42" "RRE" 14
		       39,  // 30 "B9BD" "TRTRE" "RRF5" 39  RPI 817
		       14,  // 5110 "B9BE" "SRSTU" "RRE" 14
		       39,  // 40 "B9BF" "TRTE" "RRF5" 39  RPI 817
		       39,    // "B9C8","AHHHR","RRF5"  39 RPI 1125 Z196
		       39,    // "B9C9","SHHHR","RRF5"  39 RPI 1125 Z196
		       39,   // "B9CA","ALHHHR","RRF5" 39 RPI 1125 Z196
		       39,   // "B9CB","SLHHHR","RRF5" 39 RPI 1125 Z196
		       14,     // "B9CD","CHHR","RRE"     14 RPI 1125 Z196
		       14,    // "B9CF","CLHHR","RRE"    14 RPI 1125 Z196
		       39,    // "B9D8","AHHLR","RRF5 "  39 RPI 1125 Z196
		       39,    // "B9D9","SHHLR","RRF5 "  39 RPI 1125 Z196
		       39,    // "B9DA","ALHHLR","RRF5 " 39 RPI 1125 Z196
		       39,    // "B9DB","SLHHLR","RRF5 " 39 RPI 1125 Z196
		       39,    // "B9DD","CHLR","RRE"     14 RPI 1125 Z196
		       14,    // "B9DF","CLHLR","RRE"    14 RPI 1125 Z196     
		       14,  // 5115 "B9E1" "POPCNT" "RRE" 14 RPI 1125
		       39,     // "B9E2","LOCGR","RRF5"   39 RPI 1125 Z196
		       39,     // "B9E4","NGRK","RRF5"    39 RPI 1125 Z196
		       39,     // "B9E6","OGRK","RRF5"    39 RPI 1125 Z196
		       39,     // "B9E7","XGRK","RRF5"    39 RPI 1125 Z196
		       39,     // "B9E8","AGRK","RRF5"    39 RPI 1125 Z196
		       39,     // "B9E9","SGRK","RRF5"    39 RPI 1125 Z196
		       39,     // "B9EA","ALGRK","RRF5"   39 RPI 1125 Z196
		       39,     // "B9EB","SLGRK","RRF5"   39 RPI 1125 Z196
		       39,     // "B9F2","LOCR","RRF5"    39 RPI 1125 Z196
		       39,     // "B9F4","NRK","RRF5"     39 RPI 1125 Z196
		       39,     // "B9F6","ORK","RRF5"     39 RPI 1125 Z196
		       39,     // "B9F7","XRK","RRF5"     39 RPI 1125 Z196
		       39,     // "B9F8","ARK","RRF5"     39 RPI 1125 Z196
		       39,     // "B9F9","SRK","RRF5"     39 RPI 1125 Z196
		       39,     // "B9FA","ALRK","RRF5"    39 RPI 1125 Z196
		       39,     // "B9FB","SLRK","RRF5"    39 RPI 1125 Z196 
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
		       16,  // 50 "C20" "MSGFI" "RIL" 16  RPI 817
		       16,  // 60 "C21" "MSFI" "RIL" 16  RPI 817
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
		       16,  // 70 "C42" "LLHRL" "RIL" 16  RPI 817
		       16,  // 80 "C44" "LGHRL" "RIL" 16  RPI 817
		       16,  // 90 "C45" "LHRL" "RIL" 16  RPI 817
		       16,  // 100 "C46" "LLGHRL" "RIL" 16  RPI 817
		       16,  // 110 "C47" "STHRL" "RIL" 16  RPI 817
		       16,  // 120 "C48" "LGRL" "RIL" 16  RPI 817
		       16,  // 130 "C4B" "STGRL" "RIL" 16  RPI 817
		       16,  // 140 "C4C" "LGFRL" "RIL" 16  RPI 817
		       16,  // 150 "C4D" "LRL" "RIL" 16  RPI 817
		       16,  // 160 "C4E" "LLGFRL" "RIL" 16  RPI 817
		       16,  // 170 "C4F" "STRL" "RIL" 16  RPI 817
		       16,  // 180 "C60" "EXRL" "RIL" 16  RPI 817
		       16,  // 190 "C62" "PFDRL" "RIL" 16  RPI 817
		       16,  // 200 "C64" "CGHRL" "RIL" 16  RPI 817
		       16,  // 210 "C65" "CHRL" "RIL" 16  RPI 817
		       16,  // 220 "C66" "CLGHRL" "RIL" 16  RPI 817
		       16,  // 230 "C67" "CLHRL" "RIL" 16  RPI 817
		       16,  // 240 "C68" "CGRL" "RIL" 16  RPI 817
		       16,  // 250 "C6A" "CLGRL" "RIL" 16  RPI 817
		       16,  // 260 "C6C" "CGFRL" "RIL" 16  RPI 817
		       16,  // 270 "C6D" "CRL" "RIL" 16  RPI 817
		       16,  // 280 "C6E" "CLGFRL" "RIL" 16  RPI 817
		       16,  // 290 "C6F" "CLRL" "RIL" 16  RPI 817
		       32,  //      "C80" "MVCOS" "SSF" 32 Z9-41	
		       32,  //      "C81" "ECTG" "SSF" 32 RPI 1013
		       32,  //      "C82" "CSST" "SSF" 32 RPI 1013		       
		       55,  // "C84","LPD","SSF2"  55 RPI 1125 Z196
		       55,  // "C85","LPDG","SSF2" 55 RPI 1125 Z196
               16, // "CC6","BRCTH","RIL"  16 RPI 1125 Z196
               16, // "CC8","AIH","RIL"    16  RPI 1125 Z196
               16, // "CCA","ALSIH","RIL"  16  RPI 1125 Z196
               16, // "CCB","ALSIHN","RIL" 16  RPI 1125 Z196
               16, // "CCD","CIH","RIL"    16  RPI 1125 Z196
               16, // "CCF","CLIH","RIL"   16  RPI 1125 Z196
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
		       38,  // 5375 "E00" "XREAD" "RXSS" 38 RPI 812
		       38,  // 5375 "E02" "XPRNT" "RXSS" 38 RPI 812
		       38,  // 5375 "E04" "XPNCH" "RXSS" 38 RPI 812
		       38,  // 5375 "E06" "XDUMP" "RXSS" 38 RPI 812
		       38,  // 5375 "E08" "XLIMD" "RXSS" 38 RPI 812
		       38,  // 5375 "E0A" "XGET"  "RXSS" 38 RPI 812
		       38,  // 5375 "E0C" "XPUT"  "RXSS" 38 RPI 812
		       17,  // 5380 "E1" "PKU" "RXSS" 17
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
		       18,  // 310 "E332" "LTGF" "RXY" 18  RPI 817
		       18,  // 320 "E334" "CGH" "RXY" 18  RPI 817
		       18,  // 330 "E336" "PFD" "RXY" 18  RPI 817
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
		       18,  // 340 "E35C" "MFY" "RXY" 18  RPI 817
		       18,  // 5850 "E35E" "ALY" "RXY" 18
		       18,  // 5860 "E35F" "SLY" "RXY" 18
		       18,  // 5870 "E370" "STHY" "RXY" 18
		       18,  // 5880 "E371" "LAY" "RXY" 18
		       18,  // 5890 "E372" "STCY" "RXY" 18
		       18,  // 5900 "E373" "ICY" "RXY" 18
		       18,  // 350 "E375" "LAEY" "RXY" 18  RPI 817
		       18,  // 5910 "E376" "LB" "RXY" 18
		       18,  // 5920 "E377" "LGB" "RXY" 18
		       18,  // 5930 "E378" "LHY" "RXY" 18
		       18,  // 5940 "E379" "CHY" "RXY" 18
		       18,  // 5950 "E37A" "AHY" "RXY" 18
		       18,  // 5960 "E37B" "SHY" "RXY" 18
		       18,  // 360 "E37C" "MHY" "RXY" 18  RPI 817
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
		       18,  // "E3C0","LBH","RXY"   18  RPI 1125 Z196
		       18,  // "E3C2","LLCH","RXY"  18  RPI 1125 Z196
		       18,  // "E3C3","STCH","RXY"  18  RPI 1125 Z196
		       18,  // "E3C4","LHH","RXY"   18  RPI 1125 Z196
		       18,  // "E3C6","LLHH","RXY"  18  RPI 1125 Z196
		       18,  // "E3C7","STHH","RXY"  18  RPI 1125 Z196
		       18,  // "E3CA","LFH","RXY"   18  RPI 1125 Z196
		       18,  // "E3CB","STFH","RXY"  18  RPI 1125 Z196
		       18,  // "E3CD","CHF","RXY"   18  RPI 1125 Z196
		       18,  // "E3CF","CLHF","RXY"  18  RPI 1125 Z196
		       19,  // 6120 "E500" "LASP" "SSE" 19
		       19,  // 6130 "E501" "TPROT" "SSE" 19
		       19,  // 6140 "E502" "STRAG" "SSE" 19
		       19,  // 6150 "E50E" "MVCSK" "SSE" 19
		       19,  // 6160 "E50F" "MVCDK" "SSE" 19
		       51,  // 370 "E544" "MVHHI" "SIL" 51  RPI 817
		       51,  // 380 "E548" "MVGHI" "SIL" 51  RPI 817
		       51,  // 390 "E54C" "MVHI" "SIL" 51  RPI 817
		       51,  // 400 "E554" "CHHSI" "SIL" 51  RPI 817
		       51,  // 410 "E555" "CLHHSI" "SIL" 51  RPI 817
		       51,  // 420 "E558" "CGHSI" "SIL" 51  RPI 817
		       51,  // 430 "E559" "CLGHSI" "SIL" 51  RPI 817
		       51,  // 440 "E55C" "CHSI" "SIL" 51  RPI 817
		       51,  // 450 "E55D" "CLFHSI" "SIL" 51  RPI 817
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
		       20,  // 460 "EB4C" "ECAG" "RSY" 20  RPI 817
		       21,  // 6420 "EB51" "TMY" "SIY" 21
		       21,  // 6430 "EB52" "MVIY" "SIY" 21
		       21,  // 6440 "EB54" "NIY" "SIY" 21
		       21,  // 6450 "EB55" "CLIY" "SIY" 21
		       21,  // 6460 "EB56" "OIY" "SIY" 21
		       21,  // 6470 "EB57" "XIY" "SIY" 21
		       21,  // 470 "EB6A" "ASI" "SIY" 21  RPI 817
		       21,  // 480 "EB6E" "ALSI" "SIY" 21  RPI 817
		       21,  // 490 "EB7A" "AGSI" "SIY" 21  RPI 817
		       21,  // 500 "EB7E" "ALGSI" "SIY" 21  RPI 817
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
		       20,  //  "EBDC","SRAK","RSY  "  20 RPI 1125 Z196
		       20,  //  "EBDD","SLAK","RSY  "  20 RPI 1125 Z196
		       20,  //  "EBDE","SRLK","RSY  "  20 RPI 1125 Z196
		       20,  //  "EBDF","SLLK","RSY  "  20 RPI 1125 Z196
		       56,  //  "EBE2","LOCG","RSY2 "  20 RPI 1125 Z196
		       56,  //  "EBE3","STOCG","RSY2 " 20 RPI 1125 Z196
		       20,  //  "EBE4","LANG","RSY  "  20 RPI 1125 Z196
		       20,  //  "EBE6","LAOG","RSY  "  20 RPI 1125 Z196
		       20,  //  "EBE7","LAXG","RSY  "  20 RPI 1125 Z196
		       20,  //  "EBE8","LAAG","RSY  "  20 RPI 1125 Z196
		       20,  //  "EBEA","LAALG","RSY  " 20 RPI 1125 Z196
		       56,  //  "EBF2","LOC","RSY2 "   20 RPI 1125 Z196
		       56,  //  "EBF3","STOC","RSY2 "  20 RPI 1125 Z196
		       20,  //  "EBF4","LAN","RSY  "   20 RPI 1125 Z196
		       20,  //  "EBF6","LAO","RSY  "   20 RPI 1125 Z196
		       20,  //  "EBF7","LAX","RSY  "   20 RPI 1125 Z196
		       20,  //  "EBF8","LAA","RSY  "   20 RPI 1125 Z196
		       20,  //  "EBFA","LAAL","RSY  "  20 RPI 1125 Z196
		       23,  // 6580 "EC44" "BRXHG" "RIE" 23
		       23,  // 6590 "EC44" "JXHG" "RIE" 23
		       23,  // 6600 "EC45" "BRXLG" "RIE" 23
		       23,  // 6610 "EC45" "JXLEG" "RIE" 23
		       52,  // "EC51","RISBLG#","RIE8"  52 RPI 1125 Z196 RPI 1164
		       52,  // 'EC51$003132','LOAD (lOW  && HIGH) RISBLGZ','LLHFR','RIE8',52  RPI 1164
		       52,  // 'EC51$163132','LOAD LOG HW (lOW  && HIGH) RISBLGZ','LLHLHR','RIE8',52  RPI 1164
		       52,  // 'EC51$243132','LOAD LOG CH (lOW  && HIGH) RISBLGZ','LLCLHR','RIE8',52  RPI 1164
		       52,  // "EC51Z","RISBLGZ","RIE8"  52 RPI 1125 Z196  RPI 1164
		       52,  // 510 "EC54" "RNSBG" "RIE8" 52  RPI 817
		       52,     // 'EC54$003100','AND HIGH (HIGH && HIGH) RNSBG','NHHR','RIE8',52  RPI 1164
		       52,     // 'EC54$003132','AND HIGH (HIGH && LOW ) RNSBG','NHLR','RIE8',52  RPI 1164
		       52,     // 'EC54$326332','AND HIGH (lOW  && HIGH) RNSBG','NLHR','RIE8',52  RPI 1164
		       52,  // 520 "EC54T" "RNSBGT" "RIE8" 52  RPI 817
		       52,  // 530 "EC55" "RISBG" "RIE2" 52  RPI 817
		       52,  // 540 "EC55Z" "RISBGZ" "RIE8" 52  RPI 817
		       52,  // 550 "EC56" "ROSBG" "RIE8" 52  RPI 817
		       52,  // 'EC56$003100','OR  HIGH (HIGH && HIGH) ROSBG','OHHR','RIE8',52  RPI 1164
		       52,  // 'EC56$003132','OR  HIGH (HIGH && LOW ) ROSBG','OHLR','RIE8',52  RPI 1164
		       52,  // 'EC56$326332','OR  HIGH (lOW  && HIGH) ROSBG','OLHR','RIE8',52  RPI 1164
		       52,  // 560 "EC56T" "ROSBGT" "RIE8" 52  RPI 817
		       52,  // 570 "EC57" "RXSBG" "RIE8" 52  RPI 817
		       52,     // 'EC57$003100','XOR HIGH (HIGH && HIGH) RXSBG','XHHR','RIE8',52  RPI 1164
		       52,     // 'EC57$003132','XOR HIGH (HIGH && LOW ) RXSBG','XHLR','RIE8',52  RPI 1164
		       52,     // 'EC57$326332','AOR HIGH (lOW  && HIGH) RXSBG','XLHR','RIE8',52  RPI 1164
		       52,  // 580 "EC57T" "RXSBGT" "RIE8" 52  RPI 817
		       52,  // "EC5D","RISBHG#","RIE8"  52 RPI 1125 Z196 RPI 1164
		       52,  // 'EC5D$003100','LOAD (HIGH && HIGH) RISBHGZ','LHHR','RIE8',52  RPI 1164
		       52,  // 'EC5D$003132','LOAD (HIGH && LOW ) RISBHGZ','LHLR','RIE8',52  RPI 1164
		       52,  // 'EC5D$163100','LOAD LOG HW (HIGH && HIGH) RISBHGZ','LLHHHR','RIE8',52  RPI 1164
		       52,  // 'EC5D$163132','LOAD LOG HW (HIGH && LOW ) RISBHGZ','LLHHLR','RIE8',52  RPI 1164
		       52,  // 'EC5D$243100','LOAD LOG CH (HIGH && HIGH) RISBHGZ','LLCHHR','RIE8',52  RPI 1164
		       52,  // 'EC5D$243132','LOAD LOG CH (HIGH && LOW ) RISBHGZ','LLCHLR','RIE8',52  RPI 1164
		       52,  // "EC5DZ","RISBHGZ","RIE8"  52 RPI 1125 Z196  RPI 1164
		       49,  // 10 "EC64" "CGRJ" "RIE6" 49 RPI 817
		       50,  // 20 "EC648" "CGRJE" "RIE7" 50 RPI 817
		       50,  // 30 "EC642" "CGRJH" "RIE7" 50 RPI 817
		       50,  // 40 "EC644" "CGRJL" "RIE7" 50 RPI 817
		       50,  // 50 "EC646" "CGRJNE" "RIE7" 50 RPI 817
		       50,  // 60 "EC64C" "CGRJNH" "RIE7" 50 RPI 817
		       50,  // 70 "EC64A" "CGRJNL" "RIE7" 50 RPI 817
		       49,  // 80 "EC65" "CLGRJ" "RIE6" 49 RPI 817
		       50,  // 90 "EC658" "CLGRJE" "RIE7" 50 RPI 817
		       50,  // 100 "EC652" "CLGRJH" "RIE7" 50 RPI 817
		       50,  // 110 "EC654" "CLGRJL" "RIE7" 50 RPI 817
		       50,  // 120 "EC656" "CLGRJNE" "RIE7" 50 RPI 817
		       50,  // 130 "EC65C" "CLGRJNH" "RIE7" 50 RPI 817
		       50,  // 140 "EC65A" "CLGRJNL" "RIE7" 50 RPI 817		       
		       41,  // 1010 "EC70" "CGIT" "RIE2" 41 RPI 817
		       42,  // 1020 "EC708" "CGITE" "RIE3" 42 RPI 817
		       42,  // 1030 "EC702" "CGITH" "RIE3" 42 RPI 817
		       42,  // 1040 "EC704" "CGITL" "RIE3" 42 RPI 817
		       42,  // 1050 "EC706" "CGITNE" "RIE3" 42 RPI 817
		       42,  // 1060 "EC70C" "CGITNH" "RIE3" 42 RPI 817
		       42,  // 1070 "EC70A" "CGITNL" "RIE3" 42 RPI 817 
		       41,  // 150 "EC71" "CLGIT" "RIE2" 41 RPI 817
		       42,  // 160 "EC718" "CLGITE" "RIE3" 42 RPI 817
		       42,  // 170 "EC712" "CLGITH" "RIE3" 42 RPI 817
		       42,  // 180 "EC714" "CLGITL" "RIE3" 42 RPI 817
		       42,  // 190 "EC716" "CLGITNE" "RIE3" 42 RPI 817
		       42,  // 200 "EC71C" "CLGITNH" "RIE3" 42 RPI 817
		       42,  // 210 "EC71A" "CLGITNL" "RIE3" 42 RPI 817
		       41,  // 1150 "EC72" "CIT" "RIE2" 41 RPI 817
		       42,  // 1160 "EC728" "CITE" "RIE3" 42 RPI 817
		       42,  // 1170 "EC722" "CITH" "RIE3" 42 RPI 817
		       42,  // 1180 "EC724" "CITL" "RIE3" 42 RPI 817
		       42,  // 1190 "EC726" "CITNE" "RIE3" 42 RPI 817
		       42,  // 1200 "EC72C" "CITNH" "RIE3" 42 RPI 817
		       42,  // 1210 "EC72A" "CITNL" "RIE3" 42 RPI 817
		       41,  // 220 "EC73" "CLFIT" "RIE2" 41 RPI 817
		       42,  // 230 "EC738" "CLFITE" "RIE3" 42 RPI 817
		       42,  // 240 "EC732" "CLFITH" "RIE3" 42 RPI 817
		       42,  // 250 "EC734" "CLFITL" "RIE3" 42 RPI 817
		       42,  // 260 "EC736" "CLFITNE" "RIE3" 42 RPI 817
		       42,  // 270 "EC73C" "CLFITNH" "RIE3" 42 RPI 817
		       42,  // 280 "EC73A" "CLFITNL" "RIE3" 42 RPI 817
		       49,  // 150 "EC76" "CRJ" "RIE6" 49 RPI 817
		       50,  // 160 "EC768" "CRJE" "RIE7" 50 RPI 817
		       50,  // 170 "EC762" "CRJH" "RIE7" 50 RPI 817
		       50,  // 180 "EC764" "CRJL" "RIE7" 50 RPI 817
		       50,  // 190 "EC766" "CRJNE" "RIE7" 50 RPI 817
		       50,  // 200 "EC76C" "CRJNH" "RIE7" 50 RPI 817
		       50,  // 210 "EC76A" "CRJNL" "RIE7" 50 RPI 817
		       49,  // 220 "EC77" "CLRJ" "RIE6" 49 RPI 817
		       50,  // 230 "EC778" "CLRJE" "RIE7" 50 RPI 817
		       50,  // 240 "EC772" "CLRJH" "RIE7" 50 RPI 817
		       50,  // 250 "EC774" "CLRJL" "RIE7" 50 RPI 817
		       50,  // 260 "EC776" "CLRJNE" "RIE7" 50 RPI 817
		       50,  // 270 "EC77C" "CLRJNH" "RIE7" 50 RPI 817
		       50,  // 280 "EC77A" "CLRJNL" "RIE7" 50 RPI 817
		       43,  // 290 "EC7C" "CGIJ" "RIE4" 43 RPI 817
		       44,  // 300 "EC7C8" "CGIJE" "RIE5" 44 RPI 817
		       44,  // 310 "EC7C2" "CGIJH" "RIE5" 44 RPI 817
		       44,  // 320 "EC7C4" "CGIJL" "RIE5" 44 RPI 817
		       44,  // 330 "EC7C6" "CGIJNE" "RIE5" 44 RPI 817
		       44,  // 340 "EC7CC" "CGIJNH" "RIE5" 44 RPI 817
		       44,  // 350 "EC7CA" "CGIJNL" "RIE5" 44 RPI 817
		       43,  // 360 "EC7D" "CLGIJ" "RIE4" 43 RPI 817
		       44,  // 370 "EC7D8" "CLGIJE" "RIE5" 44 RPI 817
		       44,  // 380 "EC7D2" "CLGIJH" "RIE5" 44 RPI 817
		       44,  // 390 "EC7D4" "CLGIJL" "RIE5" 44 RPI 817
		       44,  // 400 "EC7D6" "CLGIJNE" "RIE5" 44 RPI 817
		       44,  // 410 "EC7DC" "CLGIJNH" "RIE5" 44 RPI 817
		       44,  // 420 "EC7DA" "CLGIJNL" "RIE5" 44 RPI 817
		       43,  // 430 "EC7E" "CIJ" "RIE4" 43 RPI 817
		       44,  // 440 "EC7E8" "CIJE" "RIE5" 44 RPI 817
		       44,  // 450 "EC7E2" "CIJH" "RIE5" 44 RPI 817
		       44,  // 460 "EC7E4" "CIJL" "RIE5" 44 RPI 817
		       44,  // 470 "EC7E6" "CIJNE" "RIE5" 44 RPI 817
		       44,  // 480 "EC7EC" "CIJNH" "RIE5" 44 RPI 817
		       44,  // 490 "EC7EA" "CIJNL" "RIE5" 44 RPI 817
		       43,  // 500 "EC7F" "CLIJ" "RIE4" 43 RPI 817
		       44,  // 510 "EC7F8" "CLIJE" "RIE5" 44 RPI 817
		       44,  // 520 "EC7F2" "CLIJH" "RIE5" 44 RPI 817
		       44,  // 530 "EC7F4" "CLIJL" "RIE5" 44 RPI 817
		       44,  // 540 "EC7F6" "CLIJNE" "RIE5" 44 RPI 817
		       44,  // 550 "EC7FC" "CLIJNH" "RIE5" 44 RPI 817
		       44,  // 560 "EC7FA" "CLIJNL" "RIE5" 44 RPI 817
		       57,  // "ECD8","AHIK","RIE9"    57 RPI 1125 Z196
		       57,  // "ECD9","AGHIK","RIE9"   57 RPI 1125 Z196
		       57,  // "ECDA","ALHSIK","RIE9"  57 RPI 1125 Z196
		       57,  // "ECDB","ALGHSIK","RIE9" 57 RPI 1125 Z196
		       45,  // 570 "ECE4" "CGRB" "RRS1" 45 RPI 817
		       46,  // 580 "ECE48" "CGRBE" "RRS2" 446, RPI 817
		       46,  // 590 "ECE42" "CGRBH" "RRS2" 46 RPI 817
		       46,  // 600 "ECE44" "CGRBL" "RRS2" 46 RPI 817
		       46,  // 610 "ECE46" "CGRBNE" "RRS2" 46 RPI 817
		       46,  // 620 "ECE4C" "CGRBNH" "RRS2" 46 RPI 817
		       46,  // 630 "ECE4A" "CGRBNL" "RRS2" 46 RPI 817
		       45,  // 640 "ECE5" "CLGRB" "RRS1" 45 RPI 817
		       46,  // 650 "ECE58" "CLGRBE" "RRS2" 46 RPI 817
		       46,  // 660 "ECE52" "CLGRBH" "RRS2" 46 RPI 817
		       46,  // 670 "ECE54" "CLGRBL" "RRS2" 46 RPI 817
		       46,  // 680 "ECE56" "CLGRBNE" "RRS2" 46 RPI 817
		       46,  // 690 "ECE5C" "CLGRBNH" "RRS2" 46 RPI 817
		       46,  // 700 "ECE5A" "CLGRBNL" "RRS2" 46 RPI 817
		       45,  // 710 "ECF6" "CRB" "RRS1" 45 RPI 817
		       46,  // 720 "ECF68" "CRBE" "RRS2" 46 RPI 817
		       46,  // 730 "ECF62" "CRBH" "RRS2" 46 RPI 817
		       46,  // 740 "ECF64" "CRBL" "RRS2" 46 RPI 817
		       46,  // 750 "ECF66" "CRBNE" "RRS2" 45 RPI 817
		       46,  // 760 "ECF6C" "CRBNH" "RRS2" 46 RPI 817
		       46,  // 770 "ECF6A" "CRBNL" "RRS2" 46 RPI 817
		       45,  // 780 "ECF7" "CLRB" "RRS1" 45 RPI 817
		       46,  // 790 "ECF78" "CLRBE" "RRS2" 46 RPI 817
		       46,  // 800 "ECF72" "CLRBH" "RRS2" 46 RPI 817
		       46,  // 810 "ECF74" "CLRBL" "RRS2" 46 RPI 817
		       46,  // 820 "ECF76" "CLRBNE" "RRS2" 46 RPI 817
		       46,  // 830 "ECF7C" "CLRBNH" "RRS2" 46 RPI 817
		       46,  // 840 "ECF7A" "CLRBNL" "RRS2" 46 RPI 817
		       47,  // 850 "ECFC" "CGIB" "RRS3" 47 RPI 817
		       48,  // 860 "ECFC8" "CGIBE" "RRS4" 48 RPI 817
		       48,  // 870 "ECFC2" "CGIBH" "RRS4" 48 RPI 817
		       48,  // 880 "ECFC4" "CGIBL" "RRS4" 48 RPI 817
		       48,  // 890 "ECFC6" "CGIBNE" "RRS4" 48 RPI 817
		       48,  // 900 "ECFCC" "CGIBNH" "RRS4" 48 RPI 817
		       48,  // 910 "ECFCA" "CGIBNL" "RRS4" 48 RPI 817
		       47,  // 920 "ECFD" "CLGIB" "RRS3" 47 RPI 817
		       48,  // 930 "ECFD8" "CLGIBE" "RRS4" 48 RPI 817
		       48,  // 940 "ECFD2" "CLGIBH" "RRS4" 48 RPI 817
		       48,  // 950 "ECFD4" "CLGIBL" "RRS4" 48 RPI 817
		       48,  // 960 "ECFD6" "CLGIBNE" "RRS4" 48 RPI 817
		       48,  // 970 "ECFDC" "CLGIBNH" "RRS4" 48 RPI 817
		       48,  // 980 "ECFDA" "CLGIBNL" "RRS4" 48 RPI 817
		       47,  // 990 "ECFE" "CIB" "RRS3" 47 RPI 817
		       48,  // 1000 "ECFE8" "CIBE" "RRS4" 48 RPI 817
		       48,  // 1010 "ECFE2" "CIBH" "RRS4" 48 RPI 817
		       48,  // 1020 "ECFE4" "CIBL" "RRS4" 48 RPI 817
		       48,  // 1030 "ECFE6" "CIBNE" "RRS4" 48 RPI 817
		       48,  // 1040 "ECFEC" "CIBNH" "RRS4" 48 RPI 817
		       48,  // 1050 "ECFEA" "CIBNL" "RRS4" 48 RPI 817
		       47,  // 1060 "ECFF" "CLIB" "RRS3" 47 RPI 817
		       48,  // 1070 "ECFF8" "CLIBE" "RRS4" 48 RPI 817
		       48,  // 1080 "ECFF2" "CLIBH" "RRS4" 48 RPI 817
		       48,  // 1090 "ECFF4" "CLIBL" "RRS4" 48 RPI 817
		       48,  // 1100 "ECFF6" "CLIBNE" "RRS4" 48 RPI 817
		       48,  // 1110 "ECFFC" "CLIBNH" "RRS4" 48 RPI 817
		       48,  // 1120 "ECFFA" "CLIBNL" "RRS4" 48 RPI 817		       
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
		       "010A",  // 40 "010A" "PFPO" "E" 1    RPI 1013
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
		       "52",  // 1193 "52" "XDECO" "RX" 37 RPI 812
		       "53",  // 1196 "53" "XDECI" "RX" 37 RPI 812	
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
		       "61",  // 1323 "61" "XHEXI" "RX" 37 RPI 812
		       "62",  // 1326 "62" "XHEXO" "RX" 37 RPI 812
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
		       "B2B8",    // 3392 "B2B8" "SRNMB" "S" 7 RPI 1125
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
		       "B344",  // 3860 "B344" "LEDBR?" "RRE" 53 RPI 1125
		       "B345",  // 3870 "B345" "LDXBR?" "RRE" 53 RPI 1125
		       "B346",  // 3880 "B346" "LEXBR?" "RRE" 53 RPI 1125
		       "B347",  // 3890 "B347" "FIXBR?" "RRF2" 54 RPI 1125
		       "B348",  // 3900 "B348" "KXBR" "RRE" 14
		       "B349",  // 3910 "B349" "CXBR" "RRE" 14
		       "B34A",  // 3920 "B34A" "AXBR" "RRE" 14
		       "B34B",  // 3930 "B34B" "SXBR" "RRE" 14
		       "B34C",  // 3940 "B34C" "MXBR" "RRE" 14
		       "B34D",  // 3950 "B34D" "DXBR" "RRE" 14
		       "B350",  // 3960 "B350" "TBEDR" "RRF2" 34
		       "B351",  // 3970 "B351" "TBDR" "RRF2" 34
		       "B353",  // 3980 "B353" "DIEBR" "RRF2" 34
		       "B357",  // 3990 "B357" "FIEBR?" "RRF2" 54 RPI 1125
		       "B358",  // 4000 "B358" "THDER" "RRE" 14
		       "B359",  // 4010 "B359" "THDR" "RRE" 14
		       "B35B",  // 4020 "B35B" "DIDBR" "RRF2" 34
		       "B35F",  // 4030 "B35F" "FIDBR?" "RRF2" 54 RPI 1125
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
		       "B390",  //      "B390" "CELFBR" "RRF3" 30 RPI 1125 Z196
               "B391",  //      "B391" "CDLFBR" "RRF3" 30 RPI 1125 Z196
               "B392",  //      "B392" "CXLFBR" "RRF3" 30 RPI 1125 Z196
		       "B394",  // 4190 "B394?" "CEFBR" "RRE" 53 RPI 1125 Z196
		       "B395",  // 4200 "B395?" "CDFBR" "RE3" 53 RPI 1125 Z196
		       "B396",  // 4210 "B396?" "CXFBR" "RE3" 53 RPI 1125 Z196
		       "B398",  // 4220 "B398" "CFEBR?" "RRF7" 34 RPI 1125 Z196
		       "B399",  // 4230 "B399" "CFDBR?" "RRF7" 34 RPI 1125 Z196
		       "B39A",  // 4240 "B39A" "CFXBR?" "RRF7" 34 RPI 1125 Z196
		       "B39C",    // 'B39C' 'CLFEBR' 'RRF3' 30 RPI 1125 Z196
               "B39D",    // 'B39D' 'CLFDBR' 'RRF3' 30 RPI 1125 Z196
               "B39E",    // 'B39E' 'CLFXBR' 'RRF3' 30 RPI 1125 Z196
               "B3A0",    // 'B3A0' 'CELGBR' 'RRF3' 30 RPI 1125 Z196
               "B3A1",    // 'B3A1' 'CDLGBR' 'RRF3' 30 RPI 1125 Z196
               "B3A2",    // 'B3A2' 'CXLGBR' 'RRF3' 30 RPI 1125 Z196
		       "B3A4",  // 4250 "B3A4" "CEGBR" "RRE" 53 RPI 1125 Z196
		       "B3A5",  // 4260 "B3A5" "CDGBR" "RRE" 53 RPI 1125 Z196
		       "B3A6",  // 4270 "B3A6" "CXGBR" "RRE" 53 RPI 1125 Z196
		       "B3A8",  // 4280 "B3A8" "CGEBR" "RRF2" 54 RPI 1125 Z196
		       "B3A9",  // 4290 "B3A9" "CGDBR" "RRF2" 54 RPI 1125 Z1964
		       "B3AA",  // 4300 "B3AA" "CGXBR" "RRF2" 54 RPI 1125 Z1964
		       "B3AC",    //      "B3AC" "CLGEBR" "RRF2" 30  RPI 1125 Z196
               "B3AD",    //      "B3AD" "CLGDBR" "RRF2" 30  RPI 1125 Z196
               "B3AE",    //      "B3AE" "CLGXBR" "RRF2" 30  RPI 1125 Z196		       
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
		       "B3D0", // "MDTR?"  "B3D0" "RRR"  DFP 1  RPI 1125
		       "B3D1", // "DDTR?"  "B3D1" "RRR"  DFP 2  RPI 1125
		       "B3D2", // "ADTR?"  "B3D2" "RRR"  DFP 3  RPI 1125
		       "B3D3", // "SDTR?"  "B3D3" "RRR"  DFP 4  RPI 1125
		       "B3D4", // "LDETR"  "B3D4" "RRF4" DFP 5  
		       "B3D5", // "LEDTR"  "B3D5" "RRF3" DFP 6  
		       "B3D6", // "LTDTR"  "B3D6" "RRE"  DFP 7  
		       "B3D7", // "FIDTR"  "B3D7" "RRF3" DFP 8  
		       "B3D8", // "MXTR?"  "B3D8" "RRR"  DFP 9  RPI 1125
		       "B3D9", // "DXTR?"  "B3D9" "RRR"  DFP 10 RPI 1125
		       "B3DA", // "AXTR?"  "B3DA" "RRR"  DFP 11 RPI 1125
		       "B3DB", // "SXTR?"  "B3DB" "RRR"  DFP 12 RPI 1125		       
		       "B3DC", // "LXDTR" "RRF4" DFP 13
		       "B3DD", // "LDXTR" "RRF3" DFP 14
		       "B3DE", // "LTXTR" "RRE" DFP 15
		       "B3DF", // "FIXTR" "RRF3" DFP 16
		       "B3E0", // "KDTR" "RRE" DFP 17
		       "B3E1", // "CGDTR?" "RRF7" DFP 18 RPI 1125
		       "B3E2", // "CUDTR" "RRE" DFP 19
		       "B3E3", // "CSDTR" "RRF4" DFP 20
		       "B3E4", // "CDTR" "RRE" DFP 21
		       "B3E5", // "EEDTR" "RRE" DFP 22
		       "B3E7", // "ESDTR" "RRE" DFP 23
		       "B3E8", // "KXTR" "RRE" DFP 24
		       "B3E9", // "CGXTR?" "RRF7" DFP 25 RPI 1125
		       "B3EA", // "CUXTR" "RRE" DFP 26
		       "B3EB", // "CSXTR" "RRF4" DFP 27
		       "B3EC", // "CXTR" "RRE" DFP 28
		       "B3ED", // "EEXTR" "RRE" DFP 29
		       "B3EF", // "ESXTR" "RRE" DFP 30
		       "B3F1", // "CDGTR?" "RE3" DFP 31 RPI 1125
		       "B3F2", // "CDUTR" "RRE" DFP 32
		       "B3F3", // "CDSTR" "RRE" DFP 33
		       "B3F4", // "CEDTR" "RRE" DFP 34
		       "B3F5", // "QADTR" "RRF3" DFP 35
		       "B3F6", // "IEDTR" "RRF2" DFP 36
		       "B3F7", // "RRDTR" "RRF3" DFP 37
		       "B3F9", // "CXGTR?" "RE3" DFP 38 RPI 1125
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
		       "B928",  // "B928","PCKMO","RE4"  14 RPI 1125 Z196
               "B92A",  // "B92A","KMF","RRE"    14 RPI 1125 Z196
               "B92B",  // "B92B","KMO","RRE"    14 RPI 1125 Z196
               "B92C",  // "B92C","PCC","RE4"    14 RPI 1125 Z196
               "B92D",  // "B92D","KMCTR","RRF2" 34 RPI 1125 Z196
		       "B92E",  // 4770 "B92E" "KM" "RRE" 14
		       "B92F",  // 4780 "B92F" "KMC" "RRE" 14
		       "B930",  // 4790 "B930" "CGFR" "RRE" 14
		       "B931",  // 4800 "B931" "CLGFR" "RRE" 14
		       "B93E",  // 4810 "B93E" "KIMD" "RRE" 14
		       "B93F",  // 4820 "B93F" "KLMD" "RRE" 14
		       "B941",    // "B941","CFDTR","RRF"  30 RPI 1125 Z196
               "B942",   // "B942","CLGDTR","RRF" 30 RPI 1125 Z196
               "B943",   // "B943","CLFDTR","RRF" 30 RPI 1125 Z196		
		       "B946",  // 4830 "B946" "BCTGR" "RRE" 14
		       "B949",    // "B949","CFXTR","RRF3"   30 RPI 1125 Z196
               "B94A",   // "B94A","CLGXTR","RRF3"  30 RPI 1125 Z196
               "B94B",   // "B94B","CLFXTR","RRF3"  30 RPI 1125 Z196
               "B951",    // "B951","CDFTR","RRF3"   30 RPI 1125 Z196
               "B952",   // "B952","CDLGTR","RRF3"  30 RPI 1125 Z196
               "B953",   // "B953","CDLFTR","RRF3"  30 RPI 1125 Z196
               "B959",    // "B959","CXFTR","RRF3"   30 RPI 1125 Z196" 
               "B95A",   // "B95A","CXLGTR","RRF3"  30 RPI 1125 Z196
               "B95B",   // "B95B","CXLFTR","RRF3"  30 RPI 1125 Z196		       
		       "B960",  // 10 "B960" "CGRT" "RRF5" 39 RPI 817
		       "B9608",  // 20 "B9608" "CGRTE" "RRF6" 40 RPI 817
		       "B9602",  // 30 "B9602" "CGRTH" "RRF6" 40 RPI 817
		       "B9604",  // 40 "B9604" "CGRTL" "RRF6" 40 RPI 817
		       "B9606",  // 50 "B9606" "CGRTNE" "RRF6" 40 RPI 817
		       "B960C",  // 60 "B960C" "CGRTNH" "RRF6" 40 RPI 817
		       "B960A",  // 70 "B960A" "CGRTNL" "RRF6" 40 RPI 817
		       "B961",  // 10 "B961" "CLGRT" "RRF5" 39 RPI 817
		       "B9618",  // 20 "B9618" "CLGRTE" "RRF6" 40 RPI 817
		       "B9612",  // 30 "B9612" "CLGRTH" "RRF6" 40 RPI 817
		       "B9614",  // 40 "B9614" "CLGRTL" "RRF6" 40 RPI 817
		       "B9616",  // 50 "B9616" "CLGRTNE" "RRF6" 40 RPI 817
		       "B961C",  // 60 "B961C" "CLGRTNH" "RRF6" 40 RPI 817
		       "B961A",  // 70 "B961A" "CLGRTNL" "RRF6" 40 RPI 817
		       "B972",  // 80 "B972" "CRT" "RRF5" 39 RPI 817
		       "B9728",  // 90 "B9728" "CRTE" "RRF6" 40 RPI 817
		       "B9722",  // 100 "B9722" "CRTH" "RRF6" 40 RPI 817
		       "B9724",  // 110 "B9724" "CRTL" "RRF6" 40 RPI 817
		       "B9726",  // 120 "B9726" "CRTNE" "RRF6" 40 RPI 817
		       "B972C",  // 130 "B972C" "CRTNH" "RRF6" 40 RPI 817
		       "B972A",  // 140 "B972A" "CRTNL" "RRF6" 40 RPI 817
		       "B973",  // 80 "B973" "CLRT" "RRF5" 39 RPI 817
		       "B9738",  // 90 "B9738" "CLRTE" "RRF6" 40 RPI 817
		       "B9732",  // 100 "B9732" "CLRTH" "RRF6" 40 RPI 817
		       "B9734",  // 110 "B9734" "CLRTL" "RRF6" 40 RPI 817
		       "B9736",  // 120 "B9736" "CLRTNE" "RRF6" 40 RPI 817
		       "B973C",  // 130 "B973C" "CLRTNH" "RRF6" 40 RPI 817
		       "B973A",  // 140 "B973A" "CLRTNL" "RRF6" 40 RPI 817
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
		       "B9A2",  // 10 "B9A2" "PTF" "RRE" 14 RPI 817
		       "B9AA",  //      "B9AA" "LPTEA" "RRE" 14 Z9-19
		       "B9AE",  // "B9AE","RRBM","RRE"  14 RPI 1125 Z196		
		       "B9AF",  // 20 "B9AF" "PFMF" "RRF5" 39 RPI 817
		       "B9B0",  // 5070 "B9B0" "CU14" "RRE" 14
		       "B9B1",  // 5080 "B9B1" "CU24" "RRE" 14
		       "B9B2",  // 5090 "B9B2" "CU41" "RRE" 14
		       "B9B3",  // 5100 "B9B3" "CU42" "RRE" 14
		       "B9BD",  // 30 "B9BD" "TRTRE" "RRF5" 39 RPI 817
		       "B9BE",  // 5110 "B9BE" "SRSTU" "RRE" 14
		       "B9BF",  // 40 "B9BF" "TRTE" "RRF5" 39 RPI 817
		       "B9C8",    // "B9C8","AHHHR","RRF5"  39 RPI 1125 Z196
		       "B9C9",    // "B9C9","SHHHR","RRF5"  39 RPI 1125 Z196
		       "B9CA",   // "B9CA","ALHHHR","RRF5" 39 RPI 1125 Z196
		       "B9CB",   // "B9CB","SLHHHR","RRF5" 39 RPI 1125 Z196
		       "B9CD",     // "B9CD","CHHR","RRE"     14 RPI 1125 Z196
		       "B9CF",    // "B9CF","CLHHR","RRE"    14 RPI 1125 Z196
		       "B9D8",    // "B9D8","AHHLR","RRF5 "  39 RPI 1125 Z196
		       "B9D9",    // "B9D9","SHHLR","RRF5 "  39 RPI 1125 Z196
		       "B9DA",    // "B9DA","ALHHLR","RRF5 " 39 RPI 1125 Z196
		       "B9DB",    // "B9DB","SLHHLR","RRF5 " 39 RPI 1125 Z196
		       "B9DD",    // "B9DD","CHLR","RRE"     14 RPI 1125 Z196
		       "B9DF",    // "B9DF","CLHLR","RRE"    14 RPI 1125 Z196     		       
		       "B9E1",  // 5115 "B9E1" "POPCNT" "RRE" 14 RPI 1125
		       "B9E2",    // "B9E2","LOCGR","RRF5"   39 RPI 1125 Z196
		       "B9E4",    // "B9E4","NGRK","RRF5"    39 RPI 1125 Z196
		       "B9E6",    // "B9E6","OGRK","RRF5"    39 RPI 1125 Z196
		       "B9E7",    // "B9E7","XGRK","RRF5"    39 RPI 1125 Z196
		       "B9E8",    // "B9E8","AGRK","RRF5"    39 RPI 1125 Z196
		       "B9E9",    // "B9E9","SGRK","RRF5"    39 RPI 1125 Z196
		       "B9EA",    // "B9EA","ALGRK","RRF5"   39 RPI 1125 Z196
		       "B9EB",    // "B9EB","SLGRK","RRF5"   39 RPI 1125 Z196
		       "B9F2",    // "B9F2","LOCR","RRF5"    39 RPI 1125 Z196
		       "B9F4",    // "B9F4","NRK","RRF5"     39 RPI 1125 Z196
		       "B9F6",    // "B9F6","ORK","RRF5"     39 RPI 1125 Z196
		       "B9F7",    // "B9F7","XRK","RRF5"     39 RPI 1125 Z196
		       "B9F8",    // "B9F8","ARK","RRF5"     39 RPI 1125 Z196
		       "B9F9",    // "B9F9","SRK","RRF5"     39 RPI 1125 Z196
		       "B9FA",    // "B9FA","ALRK","RRF5"    39 RPI 1125 Z196
		       "B9FB",    // "B9FB","SLRK","RRF5"    39 RPI 1125 Z196 	
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
		       "C20",  // 50 "C20" "MSGFI" "RIL" 16 RPI 817
		       "C21",  // 60 "C21" "MSFI" "RIL" 16 RPI 817
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
		       "C42",  // 70 "C42" "LLHRL" "RIL" 16 RPI 817
		       "C44",  // 80 "C44" "LGHRL" "RIL" 16 RPI 817
		       "C45",  // 90 "C45" "LHRL" "RIL" 16 RPI 817
		       "C46",  // 100 "C46" "LLGHRL" "RIL" 16 RPI 817
		       "C47",  // 110 "C47" "STHRL" "RIL" 16 RPI 817
		       "C48",  // 120 "C48" "LGRL" "RIL" 16 RPI 817
		       "C4B",  // 130 "C4B" "STGRL" "RIL" 16 RPI 817
		       "C4C",  // 140 "C4C" "LGFRL" "RIL" 16 RPI 817
		       "C4D",  // 150 "C4D" "LRL" "RIL" 16 RPI 817
		       "C4E",  // 160 "C4E" "LLGFRL" "RIL" 16 RPI 817
		       "C4F",  // 170 "C4F" "STRL" "RIL" 16 RPI 817
		       "C60",  // 180 "C60" "EXRL" "RIL" 16 RPI 817
		       "C62",  // 190 "C62" "PFDRL" "RIL" 16 RPI 817
		       "C64",  // 200 "C64" "CGHRL" "RIL" 16 RPI 817
		       "C65",  // 210 "C65" "CHRL" "RIL" 16 RPI 817
		       "C66",  // 220 "C66" "CLGHRL" "RIL" 16 RPI 817
		       "C67",  // 230 "C67" "CLHRL" "RIL" 16 RPI 817
		       "C68",  // 240 "C68" "CGRL" "RIL" 16 RPI 817
		       "C6A",  // 250 "C6A" "CLGRL" "RIL" 16 RPI 817
		       "C6C",  // 260 "C6C" "CGFRL" "RIL" 16 RPI 817
		       "C6D",  // 270 "C6D" "CRL" "RIL" 16 RPI 817
		       "C6E",  // 280 "C6E" "CLGFRL" "RIL" 16 RPI 817
		       "C6F",  // 290 "C6F" "CLRL" "RIL" 16 RPI 817
		       "C80",  //      "C80" "MVCOS" "SSF" 32 Z9-41
		       "C81",  //      "C81" "ECTG" "SSF" 32 RPI 1013
		       "C82",  //      "C82" "CSST" "SSF" 32 RPI 1013
		       "C84",  // "C84","LPD","SSF2"  55 RPI 1125 Z196
		       "C85",  // "C85","LPDG","SSF2" 55 RPI 1125 Z196
		       "CC6", // "CC6","BRCTH","RIL"  16 RPI 1125 Z196
		       "CC8", // "CC8","AIH","RIL"    16  RPI 1125 Z196
		       "CCA", // "CCA","ALSIH","RIL"  16  RPI 1125 Z196
		       "CCB", // "CCB","ALSIHN","RIL" 16  RPI 1125 Z196
		       "CCD", // "CCD","CIH","RIL"    16  RPI 1125 Z196
		       "CCF", // "CCF","CLIH","RIL"   16  RPI 1125 Z196
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
		       "E00", // 5375 "E00" "XREAD" "RXSS" 38 RPI 812
		       "E02", // 5375 "E02" "XPRNT" "RXSS" 38 RPI 812
		       "E04", // 5375 "E04" "XPNCH" "RXSS" 38 RPI 812
		       "E06", // 5375 "E06" "XDUMP" "RXSS" 38 RPI 812
		       "E08", // 5375 "E08" "XLIMD" "RXSS" 38 RPI 812
		       "E0A", // 5375 "E0A" "XGET"  "RXSS" 38 RPI 812
		       "E0C", // 5375 "E0C" "XPUT"  "RXSS" 38 RPI 812
		       "E1",  // 5380 "E1" "PKU" "RXSS" 17
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
		       "E332",  // 310 "E332" "LTGF" "RXY" 18 RPI 817
		       "E334",  // 320 "E334" "CGH" "RXY" 18 RPI 817
		       "E336",  // 330 "E336" "PFD" "RXY" 18 RPI 817
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
		       "E35C",  // 340 "E35C" "MFY" "RXY" 18 RPI 817
		       "E35E",  // 5850 "E35E" "ALY" "RXY" 18
		       "E35F",  // 5860 "E35F" "SLY" "RXY" 18
		       "E370",  // 5870 "E370" "STHY" "RXY" 18
		       "E371",  // 5880 "E371" "LAY" "RXY" 18
		       "E372",  // 5890 "E372" "STCY" "RXY" 18
		       "E373",  // 5900 "E373" "ICY" "RXY" 18
		       "E375",  // 350 "E375" "LAEY" "RXY" 18 RPI 817
		       "E376",  // 5910 "E376" "LB" "RXY" 18
		       "E377",  // 5920 "E377" "LGB" "RXY" 18
		       "E378",  // 5930 "E378" "LHY" "RXY" 18
		       "E379",  // 5940 "E379" "CHY" "RXY" 18
		       "E37A",  // 5950 "E37A" "AHY" "RXY" 18
		       "E37B",  // 5960 "E37B" "SHY" "RXY" 18
		       "E37C",  // 360 "E37C" "MHY" "RXY" 18 RPI 817
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
		       "E3C0",  // "E3C0","LBH","RXY"   18  RPI 1125 Z196
		       "E3C2",  // "E3C2","LLCH","RXY"  18  RPI 1125 Z196
		       "E3C3",  // "E3C3","STCH","RXY"  18  RPI 1125 Z196
		       "E3C4",  // "E3C4","LHH","RXY"   18  RPI 1125 Z196
		       "E3C6",  // "E3C6","LLHH","RXY"  18  RPI 1125 Z196
		       "E3C7",  // "E3C7","STHH","RXY"  18  RPI 1125 Z196
		       "E3CA",  // "E3CA","LFH","RXY"   18  RPI 1125 Z196
		       "E3CB",  // "E3CB","STFH","RXY"  18  RPI 1125 Z196
		       "E3CD",  // "E3CD","CHF","RXY"   18  RPI 1125 Z196
		       "E3CF",  // "E3CF","CLHF","RXY"  18  RPI 1125 Z196
		       "E500",  // 6120 "E500" "LASP" "SSE" 19
		       "E501",  // 6130 "E501" "TPROT" "SSE" 19
		       "E502",  // 6140 "E502" "STRAG" "SSE" 19
		       "E50E",  // 6150 "E50E" "MVCSK" "SSE" 19
		       "E50F",  // 6160 "E50F" "MVCDK" "SSE" 19
		       "E544",  // 370 "E544" "MVHHI" "SIL" 51 RPI 817
		       "E548",  // 380 "E548" "MVGHI" "SIL" 51 RPI 817
		       "E54C",  // 390 "E54C" "MVHI" "SIL" 51 RPI 817
		       "E554",  // 400 "E554" "CHHSI" "SIL" 51 RPI 817
		       "E555",  // 410 "E555" "CLHHSI" "SIL" 51 RPI 817
		       "E558",  // 420 "E558" "CGHSI" "SIL" 51 RPI 817
		       "E559",  // 430 "E559" "CLGHSI" "SIL" 51 RPI 817
		       "E55C",  // 440 "E55C" "CHSI" "SIL" 51 RPI 817
		       "E55D",  // 450 "E55D" "CLFHSI" "SIL" 51 RPI 817
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
		       "EB4C",  // 460 "EB4C" "ECAG" "RSY" 20 RPI 817
		       "EB51",  // 6420 "EB51" "TMY" "SIY" 21
		       "EB52",  // 6430 "EB52" "MVIY" "SIY" 21
		       "EB54",  // 6440 "EB54" "NIY" "SIY" 21
		       "EB55",  // 6450 "EB55" "CLIY" "SIY" 21
		       "EB56",  // 6460 "EB56" "OIY" "SIY" 21
		       "EB57",  // 6470 "EB57" "XIY" "SIY" 21
		       "EB6A",  // 470 "EB6A" "ASI" "SIY" 21 RPI 817
		       "EB6E",  // 480 "EB6E" "ALSI" "SIY" 21 RPI 817
		       "EB7A",  // 490 "EB7A" "AGSI" "SIY" 21 RPI 817
		       "EB7E",  // 500 "EB7E" "ALGSI" "SIY" 21 RPI 817
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
		       "EBDC",  //  "EBDC","SRAK","RSY  "  20 RPI 1125 Z196
		       "EBDD",  //  "EBDD","SLAK","RSY  "  20 RPI 1125 Z196
		       "EBDE",  //  "EBDE","SRLK","RSY  "  20 RPI 1125 Z196
		       "EBDF",  //  "EBDF","SLLK","RSY  "  20 RPI 1125 Z196
		       "EBE2",  //  "EBE2","LOCG","RSY2 "  20 RPI 1125 Z196
		       "EBE3",  //  "EBE3","STOCG","RSY2 " 20 RPI 1125 Z196
		       "EBE4",  //  "EBE4","LANG","RSY  "  20 RPI 1125 Z196
		       "EBE6",  //  "EBE6","LAOG","RSY  "  20 RPI 1125 Z196
		       "EBE7",  //  "EBE7","LAXG","RSY  "  20 RPI 1125 Z196
		       "EBE8",  //  "EBE8","LAAG","RSY  "  20 RPI 1125 Z196
		       "EBEA",  //  "EBEA","LAALG","RSY  " 20 RPI 1125 Z196
		       "EBF2",  //  "EBF2","LOC","RSY2 "   20 RPI 1125 Z196
		       "EBF3",  //  "EBF3","STOC","RSY2 "  20 RPI 1125 Z196
		       "EBF4",  //  "EBF4","LAN","RSY  "   20 RPI 1125 Z196
		       "EBF6",  //  "EBF6","LAO","RSY  "   20 RPI 1125 Z196
		       "EBF7",  //  "EBF7","LAX","RSY  "   20 RPI 1125 Z196
		       "EBF8",  //  "EBF8","LAA","RSY  "   20 RPI 1125 Z196
		       "EBFA",  //  "EBFA","LAAL","RSY  "  20 RPI 1125 Z196
		       "EC44",  // 6580 "EC44" "BRXHG" "RIE" 23
		       "EC44",  // 6590 "EC44" "JXHG" "RIE" 23
		       "EC45",  // 6600 "EC45" "BRXLG" "RIE" 23
		       "EC45",  // 6610 "EC45" "JXLEG" "RIE" 23
		       "EC51",  // "EC51","RISBLG#","RIE8"  52 RPI 1125 Z196  RPI 1164
		       "EC51$003132",  // 'EC51$003132','LOAD (lOW  && HIGH) RISBLGZ','LLHFR','RIE8',52  RPI 1164
		       "EC51$163132",  // 'EC51$163132','LOAD LOG HW (lOW  && HIGH) RISBLGZ','LLHLHR','RIE8',52  RPI 1164
		       "EC51$243132",  // 'EC51$243132','LOAD LOG CH (lOW  && HIGH) RISBLGZ','LLCLHR','RIE8',52  RPI 1164
		       "EC51Z", // "EC51Z","RISBLGZ","RIE8"  52 RPI 1125 Z196  RPI 1164
		       "EC54",  // 510 "EC54" "RNSBG" "RIE8" 52 RPI 817
		       "EC54$003100",     // 'EC54$003100','AND HIGH (HIGH && HIGH) RNSBG','NHHR','RIE8',52  RPI 1164
		       "EC54$003132",     // 'EC54$003132','AND HIGH (HIGH && LOW ) RNSBG','NHLR','RIE8',52  RPI 1164
		       "EC54$326332",     // 'EC54$326332','AND HIGH (lOW  && HIGH) RNSBG','NLHR','RIE8',52  RPI 1164
		       "EC54T",  // 520 "EC54T" "RNSBGT" "RIE8" 52 RPI 817
		       "EC55",  // 530 "EC55" "RISBG" "RIE8" 52 RPI 817
		       "EC55Z",  // 540 "EC55Z" "RISBGZ" "RIE8" 52 RPI 817
		       "EC56",  // 550 "EC56" "ROSBG" "RIE8" 52 RPI 817
		       "EC56$003100",  // 'EC56$003100','OR  HIGH (HIGH && HIGH) ROSBG','OHHR','RIE8',52  RPI 1164
		       "EC56$003132",  // 'EC56$003132','OR  HIGH (HIGH && LOW ) ROSBG','OHLR','RIE8',52  RPI 1164
		       "EC56$326332",  // 'EC56$326332','OR  HIGH (lOW  && HIGH) ROSBG','OLHR','RIE8',52  RPI 1164
		       "EC56T",  // 560 "EC56T" "ROSBGT" "RIE8" 52 RPI 817
		       "EC57",  // 570 "EC57" "RXSBG" "RIE8" 52 RPI 817
		       "EC57$003100",     // 'EC57$003100','XOR HIGH (HIGH && HIGH) RXSBG','XHHR','RIE8',52  RPI 1164
		       "EC57$003132",     // 'EC57$003132','XOR HIGH (HIGH && LOW ) RXSBG','XHLR','RIE8',52  RPI 1164
		       "EC57$326332",     // 'EC57$326332','AOR HIGH (lOW  && HIGH) RXSBG','XLHR','RIE8',52  RPI 1164
		       "EC57T",  // 580 "EC57T" "RXSBGT" "RIE8" 52 RPI 817
		       "EC5D",  // "EC5D","RISBHG#","RIE8"  52 RPI 1125 Z196 RPI 1164
		       "EC5D$003100",  // 'EC5D$003100','LOAD (HIGH && HIGH) RISBHGZ','LHHR','RIE8',52  RPI 1164
		       "EC5D$003132",  // 'EC5D$003132','LOAD (HIGH && LOW ) RISBHGZ','LHLR','RIE8',52  RPI 1164
		       "EC5D$163100",  // 'EC5D$163100','LOAD LOG HW (HIGH && HIGH) RISBHGZ','LLHHHR','RIE8',52  RPI 1164
		       "EC5D$163132",  // 'EC5D$163132','LOAD LOG HW (HIGH && LOW ) RISBHGZ','LLHHLR','RIE8',52  RPI 1164
		       "EC5D$243100",  // 'EC5D$243100','LOAD LOG CH (HIGH && HIGH) RISBHGZ','LLCHHR','RIE8',52  RPI 1164
		       "EC5D$243132",  // 'EC5D$243132','LOAD LOG CH (HIGH && LOW ) RISBHGZ','LLCHLR','RIE8',52  RPI 1164
		       "EC5DZ", // "EC5DZ","RISBHGZ","RIE8"  52 RPI 1125 Z196  RPI 1164
		       "EC64",  // 10 "EC64" "CGRJ" "RIE6" 49 RPI 817
		       "EC648",  // 20 "EC648" "CGRJE" "RIE7" 50 RPI 817
		       "EC642",  // 30 "EC642" "CGRJH" "RIE7" 50 RPI 817
		       "EC644",  // 40 "EC644" "CGRJL" "RIE7" 50 RPI 817
		       "EC646",  // 50 "EC646" "CGRJNE" "RIE7" 50 RPI 817
		       "EC64C",  // 60 "EC64C" "CGRJNH" "RIE7" 50 RPI 817
		       "EC64A",  // 70 "EC64A" "CGRJNL" "RIE7" 50 RPI 817
		       "EC65",  // 80 "EC65" "CLGRJ" "RIE6" 49 RPI 817
		       "EC658",  // 90 "EC658" "CLGRJE" "RIE7" 50 RPI 817
		       "EC652",  // 100 "EC652" "CLGRJH" "RIE7" 50 RPI 817
		       "EC654",  // 110 "EC654" "CLGRJL" "RIE7" 50 RPI 817
		       "EC656",  // 120 "EC656" "CLGRJNE" "RIE7" 50 RPI 817
		       "EC65C",  // 130 "EC65C" "CLGRJNH" "RIE7" 50 RPI 817
		       "EC65A",  // 140 "EC65A" "CLGRJNL" "RIE7" 50 RPI 817		       
		       "EC70",  // 150 "EC70" "CGIT" "RIE2" 41 RPI 817
		       "EC708",  // 160 "EC708" "CGITE" "RIE3" 42 RPI 817
		       "EC702",  // 170 "EC702" "CGITH" "RIE3" 42 RPI 817
		       "EC704",  // 180 "EC704" "CGITL" "RIE3" 42 RPI 817
		       "EC706",  // 190 "EC706" "CGITNE" "RIE3" 42 RPI 817
		       "EC70C",  // 200 "EC70C" "CGITNH" "RIE3" 42 RPI 817
		       "EC70A",  // 210 "EC70A" "CGITNL" "RIE3" 42 RPI 817
		       "EC71",  // 150 "EC71" "CLGIT" "RIE2" 41 RPI 817
		       "EC718",  // 160 "EC718" "CLGITE" "RIE3" 42 RPI 817
		       "EC712",  // 170 "EC712" "CLGITH" "RIE3" 42 RPI 817
		       "EC714",  // 180 "EC714" "CLGITL" "RIE3" 42 RPI 817
		       "EC716",  // 190 "EC716" "CLGITNE" "RIE3" 42 RPI 817
		       "EC71C",  // 200 "EC71C" "CLGITNH" "RIE3" 42 RPI 817
		       "EC71A",  // 210 "EC71A" "CLGITNL" "RIE3" 42 RPI 817
		       "EC72",  // 220 "EC72" "CIT" "RIE2" 41 RPI 817
		       "EC728",  // 230 "EC728" "CITE" "RIE3" 42 RPI 817
		       "EC722",  // 240 "EC722" "CITH" "RIE3" 42 RPI 817
		       "EC724",  // 250 "EC724" "CITL" "RIE3" 42 RPI 817
		       "EC726",  // 260 "EC726" "CITNE" "RIE3" 42 RPI 817
		       "EC72C",  // 270 "EC72C" "CITNH" "RIE3" 42 RPI 817
		       "EC72A",  // 280 "EC72A" "CITNL" "RIE3" 42 RPI 817
		       "EC73",  // 220 "EC73" "CLFIT" "RIE2" 41 RPI 817
		       "EC738",  // 230 "EC738" "CLFITE" "RIE3" 42 RPI 817
		       "EC732",  // 240 "EC732" "CLFITH" "RIE3" 42 RPI 817
		       "EC734",  // 250 "EC734" "CLFITL" "RIE3" 42 RPI 817
		       "EC736",  // 260 "EC736" "CLFITNE" "RIE3" 42 RPI 817
		       "EC73C",  // 270 "EC73C" "CLFITNH" "RIE3" 42 RPI 817
		       "EC73A",  // 280 "EC73A" "CLFITNL" "RIE3" 42 RPI 817
		       "EC76",  // 150 "EC76" "CRJ" "RIE6" 49 RPI 817
		       "EC768",  // 160 "EC768" "CRJE" "RIE7" 50 RPI 817
		       "EC762",  // 170 "EC762" "CRJH" "RIE7" 50 RPI 817
		       "EC764",  // 180 "EC764" "CRJL" "RIE7" 50 RPI 817
		       "EC766",  // 190 "EC766" "CRJNE" "RIE7" 50 RPI 817
		       "EC76C",  // 200 "EC76C" "CRJNH" "RIE7" 50 RPI 817
		       "EC76A",  // 210 "EC76A" "CRJNL" "RIE7" 50 RPI 817
		       "EC77",  // 220 "EC77" "CLRJ" "RIE6" 49 RPI 817
		       "EC778",  // 230 "EC778" "CLRJE" "RIE7" 50 RPI 817
		       "EC772",  // 240 "EC772" "CLRJH" "RIE7" 50 RPI 817
		       "EC774",  // 250 "EC774" "CLRJL" "RIE7" 50 RPI 817
		       "EC776",  // 260 "EC776" "CLRJNE" "RIE7" 50 RPI 817
		       "EC77C",  // 270 "EC77C" "CLRJNH" "RIE7" 50 RPI 817
		       "EC77A",  // 280 "EC77A" "CLRJNL" "RIE7" 50 RPI 817
		       "EC7C",  // 290 "EC7C" "CGIJ" "RIE4" 43 RPI 817
		       "EC7C8",  // 300 "EC7C8" "CGIJE" "RIE5" 44 RPI 817
		       "EC7C2",  // 310 "EC7C2" "CGIJH" "RIE5" 44 RPI 817
		       "EC7C4",  // 320 "EC7C4" "CGIJL" "RIE5" 44 RPI 817
		       "EC7C6",  // 330 "EC7C6" "CGIJNE" "RIE5" 44 RPI 817
		       "EC7CC",  // 340 "EC7CC" "CGIJNH" "RIE5" 44 RPI 817
		       "EC7CA",  // 350 "EC7CA" "CGIJNL" "RIE5" 44 RPI 817
		       "EC7D",  // 360 "EC7D" "CLGIJ" "RIE4" 43 RPI 817
		       "EC7D8",  // 370 "EC7D8" "CLGIJE" "RIE5" 44 RPI 817
		       "EC7D2",  // 380 "EC7D2" "CLGIJH" "RIE5" 44 RPI 817
		       "EC7D4",  // 390 "EC7D4" "CLGIJL" "RIE5" 44 RPI 817
		       "EC7D6",  // 400 "EC7D6" "CLGIJNE" "RIE5" 44 RPI 817
		       "EC7DC",  // 410 "EC7DC" "CLGIJNH" "RIE5" 44 RPI 817
		       "EC7DA",  // 420 "EC7DA" "CLGIJNL" "RIE5" 44 RPI 817
		       "EC7E",  // 430 "EC7E" "CIJ" "RIE4" 43 RPI 817
		       "EC7E8",  // 440 "EC7E8" "CIJE" "RIE5" 44 RPI 817
		       "EC7E2",  // 450 "EC7E2" "CIJH" "RIE5" 44 RPI 817
		       "EC7E4",  // 460 "EC7E4" "CIJL" "RIE5" 44 RPI 817
		       "EC7E6",  // 470 "EC7E6" "CIJNE" "RIE5" 44 RPI 817
		       "EC7EC",  // 480 "EC7EC" "CIJNH" "RIE5" 44 RPI 817
		       "EC7EA",  // 490 "EC7EA" "CIJNL" "RIE5" 44 RPI 817
		       "EC7F",  // 500 "EC7F" "CLIJ" "RIE4" 43 RPI 817
		       "EC7F8",  // 510 "EC7F8" "CLIJE" "RIE5" 44 RPI 817
		       "EC7F2",  // 520 "EC7F2" "CLIJH" "RIE5" 44 RPI 817
		       "EC7F4",  // 530 "EC7F4" "CLIJL" "RIE5" 44 RPI 817
		       "EC7F6",  // 540 "EC7F6" "CLIJNE" "RIE5" 44 RPI 817
		       "EC7FC",  // 550 "EC7FC" "CLIJNH" "RIE5" 44 RPI 817
		       "EC7FA",  // 560 "EC7FA" "CLIJNL" "RIE5" 44 RPI 817
		       "ECD8",  // "ECD8","AHIK","RIE9"    57 RPI 1125 Z196
		       "ECD9",  // "ECD9","AGHIK","RIE9"   57 RPI 1125 Z196
		       "ECDA",  // "ECDA","ALHSIK","RIE9"  57 RPI 1125 Z196
		       "ECDB",  // "ECDB","ALGHSIK","RIE9" 57 RPI 1125 Z196
		       "ECE4",  // 570 "ECE4" "CGRB" "RRS1" 45 RPI 817
		       "ECE48",  // 580 "ECE48" "CGRBE" "RRS2" 46 RPI 817
		       "ECE42",  // 590 "ECE42" "CGRBH" "RRS2" 46 RPI 817
		       "ECE44",  // 600 "ECE44" "CGRBL" "RRS2" 46 RPI 817
		       "ECE46",  // 610 "ECE46" "CGRBNE" "RRS2" 46 RPI 817
		       "ECE4C",  // 620 "ECE4C" "CGRBNH" "RRS2" 46 RPI 817
		       "ECE4A",  // 630 "ECE4A" "CGRBNL" "RRS2" 46 RPI 817
		       "ECE5",  // 640 "ECE5" "CLGRB" "RRS1" 45 RPI 817
		       "ECE58",  // 650 "ECE58" "CLGRBE" "RRS2" 46 RPI 817
		       "ECE52",  // 660 "ECE52" "CLGRBH" "RRS2" 46 RPI 817
		       "ECE54",  // 670 "ECE54" "CLGRBL" "RRS2" 46 RPI 817
		       "ECE56",  // 680 "ECE56" "CLGRBNE" "RRS2" 46 RPI 817
		       "ECE5C",  // 690 "ECE5C" "CLGRBNH" "RRS2" 46 RPI 817
		       "ECE5A",  // 700 "ECE5A" "CLGRBNL" "RRS2" 46 RPI 817
		       "ECF6",  // 710 "ECF6" "CRB" "RRS1" 45 RPI 817
		       "ECF68",  // 720 "ECF68" "CRBE" "RRS2" 46 RPI 817
		       "ECF62",  // 730 "ECF62" "CRBH" "RRS2" 46 RPI 817
		       "ECF64",  // 740 "ECF64" "CRBL" "RRS2" 46 RPI 817
		       "ECF66",  // 750 "ECF66" "CRBNE" "RRS1" 45 RPI 817
		       "ECF6C",  // 760 "ECF6C" "CRBNH" "RRS2" 46 RPI 817
		       "ECF6A",  // 770 "ECF6A" "CRBNL" "RRS2" 46 RPI 817
		       "ECF7",  // 780 "ECF7" "CLRB" "RRS1" 45 RPI 817
		       "ECF78",  // 790 "ECF78" "CLRBE" "RRS2" 46 RPI 817
		       "ECF72",  // 800 "ECF72" "CLRBH" "RRS2" 46 RPI 817
		       "ECF74",  // 810 "ECF74" "CLRBL" "RRS2" 46 RPI 817
		       "ECF76",  // 820 "ECF76" "CLRBNE" "RRS2" 46 RPI 817
		       "ECF7C",  // 830 "ECF7C" "CLRBNH" "RRS2" 46 RPI 817
		       "ECF7A",  // 840 "ECF7A" "CLRBNL" "RRS2" 46 RPI 817
		       "ECFC",  // 850 "ECFC" "CGIB" "RRS3" 47 RPI 817
		       "ECFC8",  // 860 "ECFC8" "CGIBE" "RRS4" 48 RPI 817
		       "ECFC2",  // 870 "ECFC2" "CGIBH" "RRS4" 48 RPI 817
		       "ECFC4",  // 880 "ECFC4" "CGIBL" "RRS4" 48 RPI 817
		       "ECFC6",  // 890 "ECFC6" "CGIBNE" "RRS4" 48 RPI 817
		       "ECFCC",  // 900 "ECFCC" "CGIBNH" "RRS4" 48 RPI 817
		       "ECFCA",  // 910 "ECFCA" "CGIBNL" "RRS4" 48 RPI 817
		       "ECFD",  // 920 "ECFD" "CLGIB" "RRS3" 47 RPI 817
		       "ECFD8",  // 930 "ECFD8" "CLGIBE" "RRS4" 48 RPI 817
		       "ECFD2",  // 940 "ECFD2" "CLGIBH" "RRS4" 48 RPI 817
		       "ECFD4",  // 950 "ECFD4" "CLGIBL" "RRS4" 48 RPI 817
		       "ECFD6",  // 960 "ECFD6" "CLGIBNE" "RRS4" 48 RPI 817
		       "ECFDC",  // 970 "ECFDC" "CLGIBNH" "RRS4" 48 RPI 817
		       "ECFDA",  // 980 "ECFDA" "CLGIBNL" "RRS4" 48 RPI 817
		       "ECFE",  // 990 "ECFE" "CIB" "RRS3" 47 RPI 817
		       "ECFE8",  // 1000 "ECFE8" "CIBE" "RRS4" 48 RPI 817
		       "ECFE2",  // 1010 "ECFE2" "CIBH" "RRS4" 48 RPI 817
		       "ECFE4",  // 1020 "ECFE4" "CIBL" "RRS4" 48 RPI 817
		       "ECFE6",  // 1030 "ECFE6" "CIBNE" "RRS4" 48 RPI 817
		       "ECFEC",  // 1040 "ECFEC" "CIBNH" "RRS4" 48 RPI 817
		       "ECFEA",  // 1050 "ECFEA" "CIBNL" "RRS4" 48 RPI 817
		       "ECFF",  // 1060 "ECFF" "CLIB" "RRS3" 47 RPI 817
		       "ECFF8",  // 1070 "ECFF8" "CLIBE" "RRS4" 48 RPI 817
		       "ECFF2",  // 1080 "ECFF2" "CLIBH" "RRS4" 48 RPI 817
		       "ECFF4",  // 1090 "ECFF4" "CLIBL" "RRS4" 48 RPI 817
		       "ECFF6",  // 1100 "ECFF6" "CLIBNE" "RRS4" 48 RPI 817
		       "ECFFC",  // 1110 "ECFFC" "CLIBNH" "RRS4" 48 RPI 817
		       "ECFFA",  // 1120 "ECFFA" "CLIBNL" "RRS4" 48 RPI 817		       
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
public void init_tz390(){
	/*
	 * initialize shared data and tables
	 */
	dir_cur = System.getProperty("user.dir") + File.separator; // RPI 499 drop upper case
	ts_nano_start = System.nanoTime();          // RPI 662
	ts_mic_start  = System.currentTimeMillis(); // RPI 662
	init_pat();      // init patterns for matcher
	init_os_type();  // set os type
	init_os_util();  // set os utilities (overides from env var)
    init_opcodes();  // verify opcode tables
}
private void init_opcodes(){
	/*
	 * init opcodes 
	 */
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
}
private void init_pat(){
	/*
	 * init patterns for use by opcode and options routines
	 */
    /*
     * find_non_space_pattern tokens:
     * skip while space and return next non-white space token
     * */
	try {
	    find_non_space_pattern = Pattern.compile(
	    		"([\"][^\"]+[\"])"  
	    	  +	"|([^\\s]+)"  //RPI 313
				  );
	} catch (Exception e){
		  abort_error(13,"find parm pattern errror - " + e.toString());
	}
	/*
     * replace \ with / for Linux
     * */
	try {
	    find_bslash = Pattern.compile(
	    		"[\\\\]"  //RPI 1080
				  );
	} catch (Exception e){
		  abort_error(13,"replace \\ parm pattern errror - " + e.toString());
	}
	/*
     * replace / with \ for Windows
     * */
	try {
	    find_slash = Pattern.compile(
	    		"[/]"  //RPI 1080
				  );
	} catch (Exception e){
		  abort_error(13,"replace / parm pattern errror - " + e.toString());
	}
	/*
     * replace - with _ for Linux
     * */
	try {
	    find_dash = Pattern.compile(
	    		"[-]"  //RPI 1080
				  );
	} catch (Exception e){
		  abort_error(13,"replace - parm pattern errror - " + e.toString());
	}
	/*
     * replace ' with ''
     * */
	try {
	    find_squote = Pattern.compile(
	    		"[']"  //RPI 1080
				  );
	} catch (Exception e){
		  abort_error(13,"replace ' parm pattern errror - " + e.toString());
	}
	/*
     * replace '' with '
     * */
	try {
	    find_dsquote = Pattern.compile(
	    		"['][']"  //RPI 1080
				  );
	} catch (Exception e){
		  abort_error(13,"replace '' parm pattern errror - " + e.toString());
	}
	/*
     * replace " with '
     * */
	try {
	    find_dquote = Pattern.compile(
	    		"[\"]"  //RPI 1080
				  );
	} catch (Exception e){
		  abort_error(13,"replace \" parm pattern errror - " + e.toString());
	}
	/*
     * replace "" with "
     * */
	try {
	    find_ddquote = Pattern.compile(
	    		"[\"][\"]"  //RPI 1080
				  );
	} catch (Exception e){
		  abort_error(13,"replace \"\" parm pattern errror - " + e.toString());
	}
	/*
     * replace & with && for Linux
     * */
	try {
	    find_amp = Pattern.compile(
	    		"[\\&]"  //RPI 1080
				  );
	} catch (Exception e){
		  abort_error(13,"replace & parm pattern errror - " + e.toString());
	}
	/*
     * replace && with & for Linux
     * */
	try {
	    find_damp = Pattern.compile(
	    		"[\\&][\\&]"  //RPI 1080
				  );
	} catch (Exception e){
		  abort_error(13,"replace && parm pattern errror - " + e.toString());
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
private void init_os_type(){
	/*
	 * init os type RPI 1080
	 */
	String os_name = System.getProperty("os.name"); 
	if  (os_name.substring(0,3).equals("Win")){
		z390_os_type = z390_os_win;        // RPI 499

    } else {
    	z390_os_type = z390_os_linux; 
    }
}
private void init_os_util(){
	/*
	 * init os dependant utilities RPI 1080
	 */
	z390_acrobat = System.getenv("Z390ACROBAT");;   // RPI 510
	z390_browser = System.getenv("Z390BROWSER");;   // RPI 510
	z390_command = System.getenv("Z390COMMAND");    // RPI 510
	z390_editor  = System.getenv("Z390EDIT");       // RPI 510
	if  (z390_os_type == z390_os_win){
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
    } else {
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
	 *   2.  Add options check for consistency
	 *       a.  NOASM - requires chkmac(0)   - RPI 1053   
	 */
    if  (args.length >= 1){
    	if (!set_pgm_dir_name_type(args[0],pgm_type)){
    		abort_error(4,"invalid input file option - " + args[0]);
    	}
    	dir_390 = dir_pgm + "+linklib"; // RPI 700
    	dir_bal = dir_pgm;
    	dir_cpy = dir_pgm;
        dir_dat = dir_pgm;
        dir_err = dir_pgm;
        dir_log = dir_pgm;
        dir_lst = dir_pgm;
        dir_mac = dir_pgm;
    	dir_mlc = dir_pgm;
    	dir_obj = dir_pgm;
    	dir_opt = dir_pgm; // RPI 742
    	dir_pch = dir_pgm;
    	dir_prn = dir_pgm;
    	dir_trc = dir_pgm;
    } else {
	    abort_error(5,"missing file option");
    }
    process_options_file(dir_cur + "z390.OPT",false); // RPI 1156 optional opt init
    String token = null;
    int index1 = 1;
    while (index1 < args.length){
    	token = args[index1];
    	process_option("CMD-LINE",index1+1,token);  // RPI 742
    	index1++;
    }
    if (cmd_parms.length() > 0 
    	&& cmd_parms.charAt(cmd_parms.length()-1) == ' '){
    	cmd_parms = cmd_parms.substring(0,cmd_parms.length()-1);
    }
    if (log_file_name.length() > 0){ // RPI 719  RPI 755
    	if (systerm_file_name == null){  // RPI 730
    		systerm_file_name = log_file_name;
    	}
    	if (trace_file_name == null){
    		trace_file_name = log_file_name;
    	}
    }
    if (systerm_file_name == null){  // RPI 425 RPI 546
    	systerm_file_name = pgm_name; // RPI 546
    }
    if (dir_cpy == null){
    	dir_cpy = dir_mac; // RPI 742
    }
    check_options();
}
private void check_options(){
	/*
	 * check options for consistency
	 */
	if (!opt_asm){
		if(opt_chkmac != 0){
			abort_error(26,"NOASM requires CHKMAC(0)"); // RPI 1053
		}
		if (opt_chksrc == 3){
			abort_error(27,"NOASM requires CHKSRC(0-2)"); // RPI 1053
		}
	}
}
private void process_option(String opt_file_name,int opt_file_line,String token){
	/*
	 * process option from command line or
	 * from @file optionsfile line.
	 */
  try {
	if (cmd_parms_len + token.length() + 1 > max_cmd_parms_line){
		String temp_token = token;
		while (temp_token.length() > max_cmd_parms_line - 2){
			cmd_parms = cmd_parms + "\r\n  " + temp_token.substring(0,max_cmd_parms_line - 2);
			temp_token = temp_token.substring(max_cmd_parms_line - 2);
		}
		if (temp_token.length() > 0){
			cmd_parms = cmd_parms + "\r\n  " + temp_token + " ";
			cmd_parms_len = temp_token.length() + 3;
		} else {
			cmd_parms_len = max_cmd_parms_line;
		}
	} else {
		cmd_parms = cmd_parms + token + " ";
		cmd_parms_len = cmd_parms_len + token.length() + 1;
	}
	if (token.length() > 2 //RPI201 RPI 756
    		&& token.charAt(0) == '"'
    		&& token.charAt(token.length()-1) == '"'){
    		token = token.substring(1,token.length()-1);
    }
	if (token.charAt(0) == '@'){
		process_options_file(token.substring(1),true);  // RPI 742 RPI 1156
	} else if (token.toUpperCase().equals("ALIGN")){
		opt_align = true; // RPI 1073
	} else if (token.toUpperCase().equals("NOALIGN")){
       	opt_align = false;  // RPI 1073
	} else if (token.toUpperCase().equals("ALLOW")){
		opt_allow = true; // RPI 833
	} else if (token.toUpperCase().equals("NOALLOW")){
       	opt_allow = false;  // RPI 833
	} else if (token.toUpperCase().equals("AMODE24")){
		opt_amode24 = true;
		opt_amode31 = false;
		z390_amode31 = 'F';
		z390_rmode31 = 'F';
    } else if (token.toUpperCase().equals("NOAMODE24")){
		opt_amode24 = false;
		opt_amode31 = true;
		z390_amode31 = 'T';
	} else if (token.toUpperCase().equals("AMODE31")){
		opt_amode24 = false;
		opt_amode31 = true;
		z390_amode31 = 'T';
	} else if (token.toUpperCase().equals("NOAMODE31")){
		opt_amode24 = true;
		opt_amode31 = false;
		z390_amode31 = 'F';
		z390_rmode31 = 'F';
	} else if (token.toUpperCase().equals("ASCII")){
		opt_ascii = true; 
	} else if (token.toUpperCase().equals("NOASCII")){
		opt_ascii = false; 
	} else if (token.toUpperCase().equals("ASM")){
		opt_asm = true; 
	} else if (token.toUpperCase().equals("NOASM")){
		opt_asm = false; 
	} else if (token.toUpperCase().equals("ASSIST")){
		opt_assist   = true; 
		opt_loadhigh = false; // RPI 819
	} else if (token.toUpperCase().equals("NOASSIST")){
		opt_assist   = false; 
		opt_loadhigh = true; // RPI 819
	 } else if (token.toUpperCase().equals("AUTOLINK")){
	    opt_autolink = true; // RPI 874
	 } else if (token.toUpperCase().equals("NOAUTOLINK")){
	    opt_autolink = false;
	 } else if (token.toUpperCase().equals("BAL")){
		opt_bal = true; 
	 } else if (token.toUpperCase().equals("NOBAL")){
			opt_bal = false; 
	} else if (token.toUpperCase().equals("BS2000")){
		opt_bs2000 = true;  // RPI 604
		opt_amode24 = true;
		opt_amode31 = false;
		z390_amode31 = 'F';
		z390_rmode31 = 'F';
	} else if (token.toUpperCase().equals("NOBS2000")){
		opt_bs2000 = false;  // RPI 604
		opt_amode24 = false;
		opt_amode31 = true;
		z390_amode31 = 'T';
		z390_rmode31 = 'F';
    } else if (token.length() == 9
        	&& token.substring(0,7).toUpperCase().equals("CHKMAC(")){
       	opt_chkmac = token.charAt(7) - '0';
      	if (opt_chkmac < 0 || opt_chkmac > 2){
           		add_invalid_option(opt_file_name,opt_file_line,token);
        }
    } else if (token.length() == 9
        	&& token.substring(0,7).toUpperCase().equals("CHKSRC(")){
           	opt_chksrc = token.charAt(7) - '0';
          	if (opt_chksrc < 0 || opt_chksrc > 3){ // RPI 957
          		add_invalid_option(opt_file_name,opt_file_line,token);
          	}
	} else if (token.toUpperCase().equals("EDF")){ // RPI 1131
       	opt_edf = true;
	} else if (token.toUpperCase().equals("NOEDF")){
       	opt_edf = false;
	} else if (token.toUpperCase().equals("CICS")){
       	opt_cics = true;
	} else if (token.toUpperCase().equals("NOCICS")){
       	opt_cics = false;
	} else if (token.length() > 9 
			   && token.substring(0,9).toUpperCase().equals("CODEPAGE(")){
       	codepage = token;
	} else if (token.toUpperCase().equals("NOCODEPAGE")){
       	codepage = "";
	} else if (token.toUpperCase().equals("COMMENT")){
       	opt_comment = true;
	} else if (token.toUpperCase().equals("NOCOMMENT")){
       	opt_comment = false;
	} else if (token.toUpperCase().equals("CON")){
       	opt_con = true;
	} else if (token.toUpperCase().equals("NOCON")){
       	opt_con = false;
    } else if (token.toUpperCase().equals("DUMP")){
       	opt_dump = true;
    } else if (token.toUpperCase().equals("NODUMP")){
       	opt_dump = false;
    } else if (token.toUpperCase().equals("EPILOG")){
       	opt_epilog = true;
    } else if (token.toUpperCase().equals("NOEPILOG")){
       	opt_epilog = false;
    } else if (token.length() > 4
    	&& token.substring(0,4).toUpperCase().equals("ERR(")){
       	try {
       		max_errors = Integer.valueOf(token.substring(4,token.length()-1)).intValue();
      	} catch (Exception e){
      		add_invalid_option(opt_file_name,opt_file_line,token);
      	}
    } else if (token.toUpperCase().equals("ERRSUM")){
       	init_errsum();
    } else if (token.toUpperCase().equals("NOERRSUM")){
       	opt_errsum = false;
		max_errors = 100;
    } else if (token.toUpperCase().equals("EXTEND")){
       	opt_extend = true;
    } else if (token.toUpperCase().equals("NOEXTEND")){
       	opt_extend = false;
    } else if (token.length() > 6
        	&& token.substring(0,6).toUpperCase().equals("FLOAT(")){
       		opt_float = token.substring(6,token.length()-1).toUpperCase();
            if (!opt_float.equals("DECIMAL") 
            	&& !opt_float.equals("BINARY")
            	&& !opt_float.equals("HEX")){
            	abort_error(25,"option FLOAT must be DECIMAL, BINARY, or HEX");
            }
    } else if (token.toUpperCase().equals("GUAM")){
       	opt_guam = true;
    } else if (token.toUpperCase().equals("NOGUAM")){
       	opt_guam = false;
    } else if (token.toUpperCase().equals("INIT")){
       	opt_init = true;
    } else if (token.toUpperCase().equals("NOINIT")){
       	opt_init = false;
    } else if (token.length() > 4
     		&& token.substring(0,4).toUpperCase().equals("IPL(")){
    	opt_ipl = token.substring(4,token.length()-1); 
    } else if (token.length() > 8
     		&& token.substring(0,8).toUpperCase().equals("INSTALL(")){
    	opt_install_loc = token.substring(8,token.length()-1); 
   		System.setProperty("user.dir",opt_install_loc); // RPI 532 RPI 1080
   		dir_cur = System.getProperty("user.dir") + File.separator; // RPI 499 drop upper case RPI 865 
    } else if (token.toUpperCase().equals("LIST")){
       	opt_list = true;
    } else if (token.toUpperCase().equals("NOLIST")){
       	opt_list = false;
    } else if (token.toUpperCase().equals("LISTCALL")){
       	opt_listcall = true;
    } else if (token.toUpperCase().equals("NOLISTCALL")){
       	opt_listcall = false;
    } else if (token.toUpperCase().equals("LISTUSE")){
       	opt_listuse = true;
    } else if (token.toUpperCase().equals("NOLISTUSE")){
       	opt_listuse = false;
    } else if (token.toUpperCase().equals("LOADHIGH")){
       	opt_loadhigh = true; // RPI 819
    } else if (token.toUpperCase().equals("NOLOADHIGH")){
       	opt_loadhigh = false;
    } else if (token.length() > 4
      		&& token.substring(0,4).toUpperCase().equals("LOG(")){
     	log_file_name = token.substring(4,token.length()-1); // RPI 719
    } else if (token.length() > 8
      		&& token.substring(0,8).toUpperCase().equals("MAXCALL(")){
       	opt_maxcall = Integer.valueOf(token.substring(8,token.length()-1)).intValue(); 
    } else if (token.length() > 11  // RPI 1118 max zcobol display line
      		&& token.substring(0,11).toUpperCase().equals("MAXDISPLAY(")){
       	opt_maxdisplay = Integer.valueOf(token.substring(11,token.length()-1)).intValue(); 
    } else if (token.length() > 7
      		&& token.substring(0,7).toUpperCase().equals("MAXESD(")){
       	opt_maxesd = Integer.valueOf(token.substring(7,token.length()-1)).intValue();   	
    } else if (token.length() > 8
    	&& token.substring(0,8).toUpperCase().equals("MAXFILE(")){
       	try {
       		opt_maxfile = Integer.valueOf(token.substring(8,token.length()-1)).intValue();
       	} catch (Exception e){
       		add_invalid_option(opt_file_name,opt_file_line,token);
       	}   
    } else if (token.length() > 7
      		&& token.substring(0,7).toUpperCase().equals("MAXGBL(")){
       	opt_maxgbl = Integer.valueOf(token.substring(7,token.length()-1)).intValue();
    } else if (token.length() > 10
      		&& token.substring(0,10).toUpperCase().equals("MAXHEIGHT(")){
       	max_main_height = Integer.valueOf(token.substring(10,token.length()-1)).intValue();
    } else if (token.length() > 7
      		&& token.substring(0,7).toUpperCase().equals("MAXLCL(")){
       	opt_maxlcl = Integer.valueOf(token.substring(7,token.length()-1)).intValue(); 
    } else if (token.length() > 8
      		&& token.substring(0,8).toUpperCase().equals("MAXLINE(")){
       	opt_maxline = Integer.valueOf(token.substring(8,token.length()-1)).intValue(); 
    } else if (token.length() > 8
      		&& token.substring(0,7).toUpperCase().equals("MAXLOG(")){
       	opt_maxlog = Integer.valueOf(token.substring(7,token.length()-1)).intValue() << 20; 
    } else if (token.length() > 8
      		&& token.substring(0,8).toUpperCase().equals("MAXPARM(")){
       	opt_maxparm = Integer.valueOf(token.substring(8,token.length()-1)).intValue(); 
    } else if (token.length() > 8
      		&& token.substring(0,8).toUpperCase().equals("MAXPASS(")){ // RPI 920
       	opt_maxpass = Integer.valueOf(token.substring(8,token.length()-1)).intValue(); 
    } else if (token.length() > 6
      		&& token.substring(0,6).toUpperCase().equals("MAXPC(")){ // RPI 439
       	opt_maxpc = Integer.valueOf(token.substring(6,token.length()-1)).intValue();
    } else if (token.length() > 7
      		&& token.substring(0,7).toUpperCase().equals("MAXQUE(")){
       	opt_maxque = Integer.valueOf(token.substring(7,token.length()-1)).intValue(); 
    } else if (token.length() > 7
      		&& token.substring(0,7).toUpperCase().equals("MAXRLD(")){
       	opt_maxrld = Integer.valueOf(token.substring(7,token.length()-1)).intValue();  
    } else if (token.length() > 8
        	&& token.substring(0,8).toUpperCase().equals("MAXSIZE(")){
           	try {
           		max_file_size = Long.valueOf(token.substring(8,token.length()-1)).longValue() << 20; 
           	} catch (Exception e){
           		add_invalid_option(opt_file_name,opt_file_line,token);
           	}
    } else if (token.length() > 7
      		&& token.substring(0,7).toUpperCase().equals("MAXSYM(")){
       	opt_maxsym = Integer.valueOf(token.substring(7,token.length()-1)).intValue(); 
    } else if (token.length() > 7
      		&& token.substring(0,8).toUpperCase().equals("MAXWARN(")){
       	max_mnote_warning = Integer.valueOf(token.substring(8,token.length()-1)).intValue();
    	
    } else if (token.length() > 9
    		   && token.substring(0,9).toUpperCase().equals("MAXWIDTH(")){  // RPI 935
    	max_main_width = Integer.valueOf(token.substring(9,token.length()-1)).intValue();
    } else if (token.toUpperCase().equals("MCALL")){
       	opt_mcall = true; // RPI 511
       	opt_listcall = true;
    } else if (token.toUpperCase().equals("NOMCALL")){
       	opt_mcall = false; 
       	opt_listcall = false;
    } else if (token.length() > 5
    	&& token.substring(0,4).toUpperCase().equals("MEM(")){
       	try {
       	    max_mem = Integer.valueOf(token.substring(4,token.length()-1)).intValue();
       	} catch (Exception e){
       		add_invalid_option(opt_file_name,opt_file_line,token);
       	}
    } else if (token.length() > 10
      		&& token.substring(0,10).toUpperCase().equals("MINHEIGHT(")){
       	min_main_height = Integer.valueOf(token.substring(10,token.length()-1)).intValue();
    } else if (token.length() > 9
      		&& token.substring(0,9).toUpperCase().equals("MINWIDTH(")){
       	min_main_width = Integer.valueOf(token.substring(9,token.length()-1)).intValue();
    } else if (token.length() > 6  // RPI 1142
      		&& token.substring(0,6).toUpperCase().equals("MNOTE(")){
       	opt_mnote = Integer.valueOf(token.substring(6,token.length()-1)).intValue();
    } else if (token.toUpperCase().equals("MOD")){
       	opt_mod = true;
    } else if (token.toUpperCase().equals("NOMOD")){
       	opt_mod = false;
    } else if (token.toUpperCase().equals("OBJ")){
       	opt_obj = true;
    } else if (token.toUpperCase().equals("NOOBJ")){
       	opt_obj = false;
    } else if (token.toUpperCase().equals("OBJHEX")){
       	opt_objhex = true;
    } else if (token.toUpperCase().equals("NOOBJHEX")){
       	opt_objhex = false;
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
    } else if (token.toUpperCase().equals("NOPC")){
        opt_pc = false;
    } else if (token.toUpperCase().equals("PCOPT")){
        opt_pcopt = true;
    } else if (token.toUpperCase().equals("NOPCOPT")){
        opt_pcopt = false;
    } else if (token.toUpperCase().equals("PDSMEM8")){
        opt_pdsmem8 = true;
    } else if (token.toUpperCase().equals("NOPDSMEM8")){
        opt_pdsmem8 = false;
    } else if (token.toUpperCase().equals("PRINTALL")){ // RPI 1127
        opt_printall = true;
    } else if (token.toUpperCase().equals("NOPRINTALL")){
        opt_printall = false;
    } else if (token.length() > 8
      		&& token.substring(0,8).toUpperCase().equals("PROFILE(")){
     	opt_profile = token.substring(8,token.length()-1);
    } else if (token.toUpperCase().equals("PROLOG")){
        opt_prolog = true;
    } else if (token.toUpperCase().equals("NOPROLOG")){
        opt_prolog = false;
    } else if (token.toUpperCase().equals("PROTECT")){
        opt_protect = true;
    } else if (token.toUpperCase().equals("NOPROTECT")){
        opt_protect = false;
    } else if (token.toUpperCase().equals("R64")){ // RPI 986
        opt_r64 = true;
    } else if (token.toUpperCase().equals("NOR64")){
        opt_r64 = false;
    } else if (token.toUpperCase().equals("REFORMAT")){
        opt_reformat = true; 
    } else if (token.toUpperCase().equals("NOREFORMAT")){
        opt_reformat = false; 
    } else if (token.toUpperCase().equals("REGS")){
       	opt_regs = true;
       	opt_list  = true;
    } else if (token.toUpperCase().equals("NOREGS")){
       	opt_regs = false;
    } else if (token.toUpperCase().equals("RMODE24")){
       	opt_rmode24 = true;
       	opt_rmode31 = false;
       	z390_rmode31 = 'F';
    } else if (token.toUpperCase().equals("NORMODE24")){
       	opt_rmode24 = false;
       	opt_rmode31 = true;
       	z390_rmode31 = 'T';
       	z390_amode31 = 'T';
    } else if (token.toUpperCase().equals("RMODE31")){
       	opt_rmode24 = false;
      	opt_rmode31 = true;
       	z390_rmode31 = 'T';
    } else if (token.toUpperCase().equals("NORMODE31")){
       	opt_rmode24 = true;
      	opt_rmode31 = false;
       	z390_rmode31 = 'F';
    } else if (token.toUpperCase().equals("STATS")){
       	opt_stats = true;  // RPI 755
       	stats_file_name = pgm_name;
    } else if (token.toUpperCase().equals("NOSTATS")){
       	opt_stats = false;  // RPI 755
       	stats_file_name = null;
    } else if (token.length() > 6
      		&& token.substring(0,6).toUpperCase().equals("STATS(")){
     	stats_file_name = token.substring(6,token.length()-1); // RPI 736
     	opt_stats = true; // RPI 755
    } else if (token.length() > 7
       		&& token.substring(0,7).toUpperCase().equals("SYS390(")){
       	dir_390 = set_path_option(dir_390,token.substring(7,token.length()-1));	
    } else if (token.length() > 7 
       		&& token.substring(0,7).toUpperCase().equals("SYSBAL(")){
      	dir_bal = set_path_option(dir_bal,token.substring(7,token.length()-1)); 
    } else if (token.length() > 7 
      		&& token.substring(0,7).toUpperCase().equals("SYSCPY(")){
       	if (dir_cpy == null){ // RPI 979
       		dir_cpy = set_path_option(dir_pgm,token.substring(7,token.length()-1)); 
       	} else {
       		dir_cpy = set_path_option(dir_cpy,token.substring(7,token.length()-1)); 
       	}
    } else if (token.length() > 7 
      		&& token.substring(0,7).toUpperCase().equals("SYSDAT(")){
       	dir_dat = set_path_option(dir_dat,token.substring(7,token.length()-1)); 
    } else if (token.length() > 7
       		&& token.substring(0,7).toUpperCase().equals("SYSERR(")){
        dir_err = set_path_option(dir_err,token.substring(7,token.length()-1)); // RPI 243 
    } else if (token.length() > 7
      		&& token.substring(0,7).toUpperCase().equals("SYSLOG(")){
       	dir_log = set_path_option(dir_log,token.substring(7,token.length()-1));
    } else if (token.length() > 7 
      		&& token.substring(0,7).toUpperCase().equals("SYSLST(")){  // RPI 866
      	dir_lst = set_path_option(dir_lst,token.substring(7,token.length()-1)); 
    } else if (token.length() > 7 
       		&& token.substring(0,7).toUpperCase().equals("SYSMAC(")){
       	dir_mac = set_path_option(dir_mac,token.substring(7,token.length()-1));  
    } else if (token.length() > 7 
       		&& token.substring(0,7).toUpperCase().equals("SYSMLC(")){
      	dir_mlc = set_path_option(dir_mlc,get_short_file_name(token.substring(7,token.length()-1))); 
    } else if (token.length() > 7 
       		&& token.substring(0,7).toUpperCase().equals("SYSOBJ(")){
       	dir_obj = set_path_option(dir_obj,token.substring(7,token.length()-1)); 
    } else if (token.length() > 7 
       		&& token.substring(0,7).toUpperCase().equals("SYSOPT(")){
       	dir_opt = set_path_option(dir_opt,token.substring(7,token.length()-1)); // RPI 742
    } else if (token.length() > 8
     		&& token.substring(0,8).toUpperCase().equals("SYSPARM(")){
    	opt_sysparm = token.substring(8,token.length()-1); 
    } else if (token.length() > 7 
       		&& token.substring(0,7).toUpperCase().equals("SYSPCH(")){
      	dir_pch = set_path_option(dir_pch,get_short_file_name(token.substring(7,token.length()-1))); 
    } else if (token.length() > 7 
      		&& token.substring(0,7).toUpperCase().equals("SYSPRN(")){
      	dir_prn = set_path_option(dir_prn,token.substring(7,token.length()-1)); 	
    } else if (token.length() > 8
      		&& token.substring(0,8).toUpperCase().equals("SYSTERM(")){
     	systerm_file_name = token.substring(8,token.length()-1); // RPI 730
    } else if (token.length() > 7 
       		&& token.substring(0,7).toUpperCase().equals("SYSTRC(")){
      	dir_trc = set_path_option(dir_trc,token.substring(7,token.length()-1)); 
    } else if (token.length() > 5
      		&& token.substring(0,5).toUpperCase().equals("TIME(")){
       	max_time_seconds = Long.valueOf(token.substring(5,token.length()-1)).longValue();
       	if (max_time_seconds > 0){
       		opt_time = true;
       	} else {
       		opt_time = false;
       		opt_timing = false;
       	}
    } else if (token.toUpperCase().equals("TIME")){
       	opt_time = true;  
       	max_time_seconds = 15;
    } else if (token.toUpperCase().equals("NOTIME")){
       	opt_time = false;  
    } else if (token.toUpperCase().equals("TIMING")){
       	opt_timing = true;  
    } else if (token.toUpperCase().equals("NOTIMING")){
       	opt_timing = false;
       	opt_time   = false;
    } else if (token.toUpperCase().equals("TEST")){
       	opt_test = true;
       	opt_time = false;
       	opt_con  = true;
    } else if (token.toUpperCase().equals("NOTEST")){
       	opt_test = false;
    } else if (token.toUpperCase().equals("THREAD")){
       	opt_thread = true;  // RPI 1186
    } else if (token.toUpperCase().equals("NOTHREAD")){
       	opt_thread = false; // RPI 1186
    } else if (token.length() > 5
      		&& token.substring(0,5).toUpperCase().equals("TEST(")){
       	test_ddname = token.substring(5,token.length()-1);	
       	opt_test = true;
    } else if (token.toUpperCase().equals("TRACE")){
       	opt_trace = true;
       	opt_list   = true;
       	if (!opt_test){
       		opt_con   = false; // RPI 569 leave on if TEST
       	}
    } else if (token.toUpperCase().equals("NOTRACE")){
       	opt_trace = false;
    } else if (token.length() > 6
      		&& token.substring(0,6).toUpperCase().equals("TRACE(")){
       	trace_options = token.substring(6,token.length()-1).toUpperCase();
       	opt_con = false;
       	set_trace_options(trace_options); // RPI 930       	
    } else if (token.toUpperCase().equals("TRACEA")){
       	opt_tracea = true;
       	opt_list = true;
       	opt_con   = false;
    } else if (token.toUpperCase().equals("NOTRACEA")){
       	opt_tracea = false;
    } else if (token.toUpperCase().equals("TRACEC")){
       	opt_tracec = true;
    } else if (token.toUpperCase().equals("NOTRACEC")){
       	opt_tracec = false;
    } else if (token.toUpperCase().equals("TRACEALL")){
       	opt_trace    = true;
    	opt_traceall = true;
      	opt_tracea   = true;
      	opt_tracec   = true; // RPI 862
       	opt_traceg   = true;
       	opt_tracei   = true; // RPI 1157
       	opt_tracel   = true;
       	opt_tracem   = true;
       	opt_tracep   = true;
       	opt_traceq   = true;
       	opt_traces   = true; // RPI 882
       	opt_tracet   = true;
       	opt_tracev   = true;
       	opt_list     = true;
       	opt_listcall = true; // RPI 862
       	opt_con   = false;
    } else if (token.toUpperCase().equals("NOTRACEALL")){
       	opt_traceall = false;
       	opt_trace    = false;
      	opt_tracea   = false;
      	opt_tracec   = false;
       	opt_traceg   = false;
       	opt_tracei   = false;  // RPI 1157
       	opt_tracel   = false;
      	opt_tracem   = false;
       	opt_tracep   = false;
       	opt_traceq   = false;
       	opt_traces   = false; // RPI 882
       	opt_tracet   = false;
       	opt_tracev   = false;
    } else if (token.toUpperCase().equals("TRACEG")){
       	opt_traceg = true;
       	opt_con   = false;
    } else if (token.toUpperCase().equals("NOTRACEG")){
       	opt_traceg = false;
    } else if (token.toUpperCase().equals("TRACEI")){
       	opt_tracei = true;
       	opt_con   = false;
    } else if (token.toUpperCase().equals("NOTRACEI")){
       	opt_tracei = false;
    } else if (token.toUpperCase().equals("TRACEL")){
       	opt_tracel = true;
       	opt_list = true;
       	opt_con   = false;
    } else if (token.toUpperCase().equals("NOTRACEL")){
       	opt_tracel = false;
    } else if (token.toUpperCase().equals("TRACEM")){
        	opt_tracem = true;
        	opt_list = true;
        	opt_listcall = true; // RPI 862
        	opt_con   = false;
    } else if (token.toUpperCase().equals("NOTRACEM")){
    	opt_tracem = false;
    } else if (token.toUpperCase().equals("TRACEMEM")){
       	opt_traceg = true;
       	opt_con   = false;
    } else if (token.toUpperCase().equals("NOTRACEMEM")){
       	opt_traceg = false;
    } else if (token.toUpperCase().equals("TRACEP")){
    	opt_tracep = true;
    	opt_tracem = true;
    	opt_list = true;
    	opt_listcall = true; // RPI 862
    	opt_con   = false;
    } else if (token.toUpperCase().equals("NOTRACEP")){
    	opt_tracep = false;
    } else if (token.toUpperCase().equals("TRACEQ")){
    	opt_traceq = true;
    	opt_con   = false;
    } else if (token.toUpperCase().equals("NOTRACEQ")){
    	opt_traceq = false;
    } else if (token.toUpperCase().equals("TRACES")){
    	opt_traces = true; // RPI 882
    	opt_con   = false;
    } else if (token.toUpperCase().equals("NOTRACES")){
    	opt_traces = false; // RPI 882
    } else if (token.toUpperCase().equals("TRACET")){
    	opt_tracet = true;
    	opt_con   = false;
    } else if (token.toUpperCase().equals("NOTRACET")){
    	opt_tracet = false;
    } else if (token.toUpperCase().equals("TRACEV")){
    	opt_tracev = true;
    	opt_con   = false;
    } else if (token.toUpperCase().equals("NOTRACEV")){
    	opt_tracev = false;
    } else if (token.toUpperCase().equals("TRAP")){
       	opt_trap = true;
    } else if (token.toUpperCase().equals("NOTRAP")){
       	opt_trap = false;
    } else if (token.toUpperCase().equals("TRUNC")){
       	opt_trunc = true;
    } else if (token.toUpperCase().equals("NOTRUNC")){
       	opt_trunc = false;
    } else if (token.toUpperCase().equals("TS")){
    	opt_ts = true; // timestamp traces
    } else if (token.toUpperCase().equals("NOTS")){
    	opt_ts = false; 
    } else if (token.toUpperCase().equals("VCB")){
    	opt_vcb = true; // VSAM Cache Buffering to reduce I/O
    } else if (token.toUpperCase().equals("NOVCB")){
    	opt_vcb = false;
    } else if (token.toUpperCase().equals("WARN")){
    	opt_warn = true; // VSAM Cache Buffering to reduce I/O
    } else if (token.toUpperCase().equals("NOWARN")){
    	opt_warn = false;
    } else if (token.toUpperCase().equals("XREF")){
       	opt_xref = true;
       	opt_list = true;
    } else if (token.toUpperCase().equals("NOXREF")){
       	opt_xref = false;
    } else if (token.toUpperCase().equals("ZSTRMAC")){
       	opt_zstrmac = true;
    } else if (token.toUpperCase().equals("NOZSTRMAC")){
       	opt_zstrmac = false;
    } else {
        add_invalid_option(opt_file_name,opt_file_line,token);
    }
  } catch (Exception e){
	  add_invalid_option(opt_file_name,opt_file_line,token);
  }
}
public  void set_trace_options(String trace_options){
	/*
	 * set trace options (called by init and 
	 * by mz390 when SYSTRACE is updated.
	 */
	opt_traceall = false;
   	opt_trace    = false;
  	opt_tracea   = false;
  	opt_tracec   = false; 
   	opt_traceg   = false;
   	opt_tracei   = false; // RPI 1157
   	opt_tracel   = false;
   	opt_tracem   = false;
   	opt_tracep   = false;
   	opt_traceq   = false;
   	opt_traces   = false; 
   	opt_tracet   = false;
   	opt_tracev   = false;
	int index = 0;
   	while (index < trace_options.length()){
   		if (trace_options.charAt(index) == '*'){
   			opt_traceall = true;
   	       	opt_trace    = true;
   	      	opt_tracea   = true;
   	      	opt_tracec   = true; 
   	       	opt_traceg   = true;
   	       	opt_tracei   = true; // RPI 1157
   	       	opt_tracel   = true;
   	       	opt_tracem   = true;
   	       	opt_tracep   = true;
   	       	opt_traceq   = true;
   	       	opt_traces   = true; 
   	       	opt_tracet   = true;
   	       	opt_tracev   = true;
   		} else if (trace_options.charAt(index) == 'A'){
   			opt_tracea = true;
   		} else if (trace_options.charAt(index) == 'C'){
       		opt_tracec = true; // RPI 862
   		} else if (trace_options.charAt(index) == 'E'){
   			opt_trace = true;
   		} else if (trace_options.charAt(index) == 'G'){
   			opt_traceg = true;
   		} else if (trace_options.charAt(index) == 'I'){
   			opt_tracei = true;  // RPI 1157
   		} else if (trace_options.charAt(index) == 'L'){
   			opt_tracel = true;
   		} else if (trace_options.charAt(index) == 'M'){
   			opt_tracem = true;
   			opt_listcall = true; // RPI 862
   		} else if (trace_options.charAt(index) == 'P'){
   			opt_tracep = true;	
   			opt_tracem = true;
   			opt_listcall = true; // RPI 862
   		} else if (trace_options.charAt(index) == 'Q'){
   			opt_traceq = true;
   		} else if (trace_options.charAt(index) == 'S'){
   			opt_traces = true; // RPI 882
   		} else if (trace_options.charAt(index) == 'T'){
   			opt_tracet = true;
   		} else if (trace_options.charAt(index) == 'V'){
   			opt_tracev = true;
   		}
   		index++;
   	}
}
private void add_invalid_option(String opt_file_name,int opt_file_line,String option){
	/*
	 * collect invalid options for single error
	 */
	invalid_options = invalid_options + " " + option; // RPI 880
	System.out.println("TZ390E invalid option=" + option + "  (" + opt_file_name + "/" + opt_file_line + ")");
}
private void process_options_file(String file_name,boolean required){ // RPI 1156
	/*
	 * process option file as follows:
	 * 1.  Default suffix .OPT
	 * 2.  Uses SYSOPT path which defaults to program path
	 * 3.  Comments starting with * to end of line
	 * 4.  @file option can be nested.
	 */
    String opt_file_name = find_file_name(dir_opt,file_name,opt_type,dir_cur); // rpi 880
	int    opt_file_line = 0; // rpi 880
    if (opt_file_name != null){
		try {
			File opt_file = new File(opt_file_name);
			BufferedReader opt_file_buff = new BufferedReader(new FileReader(opt_file));       
			String option_line = opt_file_buff.readLine();
			if (!required){
				cmd_parms = cmd_parms.trim() + opt_file_name;
			}
			cmd_parms = cmd_parms.trim() + "=(";
			while (option_line != null){
				opt_file_line++; // rpi 880
				Matcher find_option_match = find_non_space_pattern.matcher(option_line);
				boolean comment_found = false;  // RPI 880
				while (find_option_match.find() 
						&& find_option_match.group().charAt(0) != '*'
						&& !comment_found){  // RPI 880
						String option = find_option_match.group();
						process_option(opt_file_name,opt_file_line,option);
				}
				option_line = opt_file_buff.readLine();
			}
			opt_file_buff.close();
			cmd_parms = cmd_parms.trim() + ") ";
		} catch (Exception e){
			add_invalid_option(opt_file_name,opt_file_line,"@" + file_name); // RPI 880
		}
	} else if (required){
		add_invalid_option(opt_file_name,opt_file_line,"@" + file_name); // RPI 880
	}
}
public void open_systerm(String z390_pgm){
	/*
	 * open systerm file and sta statistics file
	 * positions to add to end of existing files.
	 */
	systerm_prefix = left_justify(pgm_name,9) + " " + z390_pgm + " ";
    if (stats_file == null 
    	&& stats_file_name != null){
    	stats_file_name = get_file_name(dir_err,stats_file_name,sta_type);
    	try {
            stats_file = new RandomAccessFile(stats_file_name,"rw"); 
            stats_file.seek(stats_file.length());
        } catch (Exception e){
        	stats_file = null; 
        	abort_error(20,"stats file open error " + e.toString());
        }
    }
	if (systerm_file != null)return; // rpi 415
	if (systerm_file_name == null
		|| systerm_file_name.length() == 0){
		systerm_file_name = pgm_name; // RPI 880
	}
	systerm_file_name = get_file_name(dir_err,systerm_file_name,err_type);
    try {
        systerm_file = new RandomAccessFile(systerm_file_name,"rw"); 
        systerm_file.seek(systerm_file.length());
        if (invalid_options.length() > 0){
        	abort_error(21,"invalid options - " + invalid_options);
        }
    } catch (Exception e){
    	systerm_file = null; 
    	abort_error(10,"systerm file open error " + e.toString());
    }
    String z390_j2se_versions = "";  // RPI 797
    if (opt_timing){
		systerm_start = System.currentTimeMillis();
	    z390_j2se_versions = 
	    	" USING z390 " + version 
	      + " ON J2SE " + System.getProperty("java.version")
	      + " " + cur_date(); // RPI 797
	}
	try {
		systerm_io++;
		started_msg = cur_time(true) 
		            + systerm_prefix 
		            + "START" + z390_j2se_versions; // RPI 797
		System.out.println(started_msg);
		systerm_file.writeBytes(started_msg + newline); // RPI 500
		if (stats_file != null){
			try { // RPI 935
				stats_file.writeBytes(started_msg + newline); // RPI 755
			} catch (Exception e){
				stats_file = null; 
				abort_error(23,"I/O error on stats file " + e.toString());
			}
		}
	} catch (Exception e){
    	systerm_file = null; 
        abort_error(11,"I/O error on systerm file " + e.toString());
	}
}
public synchronized void put_systerm(String msg){ // RPI 397
	/*
	 * log error to systerm file
	 */
	if (systerm_file != null){ // rpi 935
		try {
			systerm_io++;
			systerm_file.writeBytes(cur_time(true) + systerm_prefix + msg + newline); // RPI 500
		} catch (Exception e){
	        abort_error(12,"I/O error on systerm file " + e.toString());
		}
	} else {
		System.out.println(systerm_prefix + msg); //RPI 1069 codepage prior to setting systerm
	}
}
public synchronized void put_stat_line(String msg){ // RPI 397
	/*
	 * mod stat record on stats.sta file
	 */
	if (stats_file != null){ // RPI 935
		try {
			systerm_io++;
			stats_file.writeBytes(cur_time(true) + systerm_prefix + msg + newline); // RPI 500
		} catch (Exception e){
			stats_file = null; 
	        abort_error(19,"I/O error on stats file " + e.toString());
		}
	}
}
public synchronized void close_systerm(int rc){ // RPI 397
	/*
	 * close systerm error file if open
	 * 
	 */
     if (systerm_file != null){
		 systerm_io++;
     	 set_ended_msg(rc);
    	 try {
    		 System.out.println(ended_msg);
    		 systerm_file.writeBytes(ended_msg + newline); // RPI 500
    	     if (stats_file != null){
    	    	 try {
    	    		 stats_file.writeBytes(ended_msg + newline); // RPI 755
    	    	 } catch (Exception e){
    	    		 stats_file = null;
    	    		 abort_error(24,"I/O error on stats file close " + e.toString());
    	    	 }
    	     }
    	 } catch (Exception e){
    		 systerm_file = null; // RPI 935
    	 }
    	 try {
    		 systerm_file.close();
    	 } catch (Exception e){
    		 System.out.println("TZ390E systerm file close error - " + e.toString());
    	 }
    	 systerm_file = null;  
     }
     if (stats_file != null){
    	 try {
    		 stats_file.close();
    	 } catch (Exception e){
    		 System.out.println("TZ390E stats file close error - " + e.toString());
    	 }
    	 stats_file = null;   
     }
}
public void set_ended_msg(int rc){
	/*
	 * set ended_msg for use by mz390, az390,
	 * lz390, and ez390.
	 */
	if (ended_msg.length() > 0){ // RPI 837
		return;
	}
	if (opt_timing){
 		systerm_sec  = " SEC=" + right_justify("" + ((System.currentTimeMillis()-systerm_start)/1000),2);
	    systerm_mem = right_justify("" + get_mem_usage(),3);
	 }
	 String systerm_ins_text = "";
	 if (systerm_ins > 0){
		 systerm_ins_text = " INS=" + systerm_ins;
	 }
	 ended_msg = cur_time(true) + systerm_prefix
	    + "ENDED   RC=" + right_justify("" + rc,2) 
	    + systerm_sec 
	    + " MEM(MB)=" + systerm_mem 
	    + " IO=" + systerm_io 
	    + systerm_ins_text;
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
public int get_mem_usage(){
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
	if (file_name.charAt(0) != '"' // RPI 756
		&& index >=0){
		return "\"" + file_name + "\""; // LSN
	}
	return file_name;
}
public synchronized void abort_error(int error,String msg){ // RPI 397
	/*
	 * display options error on system out
	 * and exit with rc 16.
	 */
	if (tz390_recursive_abort){ // RPI 935
		System.out.println("TZ390E recurive abort exit");
		System.exit(16);
	}
	tz390_recursive_abort = true;
	msg = "TZ390E abort error " + error + " - " + msg;
	System.out.println(msg);
    System.out.println("z390_abort_request"); // RPI 731 request parent shutdown
	System.out.flush();
	put_systerm(msg);
	z390_abort = true;
    sleep_now(1000);
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
	 *       b.  "C:" - copy file found  RPI 970
	 *       c.  "F:" - macro and copybook files
	 *       d.  "G:" - global set variables
	 *       e.  "M:" - loaded macros
	 *       f.  "O:" - opcode table (init_opcode_name_keys)
	 *       g.  "R:" - opcode and macro opsyn
	 *       h.  "S:" - ordinary symbols
	 *       i.  "X:" - executable macro command
	 *       j.  "Z:" - ZSTRMAC opcodes and apm names RPI 902
	 *   2.  Usage by az390
	 *       a.  "L:" - literals
	 *       b.  "O:" - opcode table (init_opcode_name_keys)
	 *       c.  "R:" - opcode opsyn
	 *       d.  "S:" - ordinary symbols
	 *       e.  "U:" - USING labels
	 *       f.  "V:" - extrn symbol
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
public String get_file_name(String file_dir,String file_name,String file_type){
	   /*
	    * 1.  Strip long spacey name quotes if found from path and file.
	    * 2.  Replace . and ..\ with current directory  RPI 866
	    * 3.  Check for overriding path in filename and ignore default path RPI 866
	    * 4.  Check for overriding filename in path and ignore default filename RPI 866
	    * 2.  Add directory, name, and/or type if not specified  
	    * 3.  Replace \ with / if Linux
	    */
	        if (file_dir == null 
	        	|| file_name == null 
	        	|| file_type == null
	        	|| file_dir.length() == 0
	        	|| file_name.length() == 0){ // RPI 903 allow 0 length type
	        	return null; // RPI 880
	        }
	        int last_path_sep = file_dir.lastIndexOf(File.separatorChar); // RPI 1191
	        file_dir = fix_file_separators(file_dir);  // RPI 1080
	        file_name = fix_file_separators(file_name); // RPI 1080
	        if (file_name.charAt(0) == '\"' 
	    		|| file_name.charAt(0) == '\''){
	    		file_name = file_name.substring(1,file_name.length() - 1);
	    	}
	    	File temp_file;
	    	int index = file_name.indexOf(File.separator); // RPI 866
	    	if (index >= 0
	    		|| (file_name.length() > 2 && file_name.charAt(1) == ':')){
	    		// path found in file_name so ignore file_dir
	    		temp_file = new File(file_name);
	    		file_name = temp_file.getAbsolutePath(); 
	    	} else {
	    		if (file_dir == null 
	    			|| file_dir.length() == 0
	    			|| file_dir.equals(".")){
	    			temp_file = new File(dir_cur);
	    			if (temp_file.isDirectory()){
	    				file_name = temp_file.getAbsolutePath() + File.separator + file_name;
	    			} else {
	    				return null; // rpi 880
	    			}
	    		} else {
	    			temp_file = new File(file_dir + File.separator); // RPI 1210 
	    			index = file_dir.lastIndexOf("."); // RPI 1191 
	    			if (index > last_path_sep){        // RPI 1191 
	    				// file_dir has filename.sfx so ignore file_name
		    			if (file_dir.charAt(index-1) == '*'){  // RPI 908
		    				if (index > 1){
		    					// path\*.sfx
		    				   temp_file = new File(file_dir.substring(0,index-1) + file_name + file_dir.substring(index));
		    				} else {
		    					// *.sfx - replace sfx
		    					temp_file = new File(dir_pgm + File.separator + file_name + file_dir.substring(index));
		    				}
		    			}
    					file_name = temp_file.getAbsolutePath(); // RPI 908 remove file exist chk 
	    			} else {
	    				// concatenate file_dir with file_name
	    				if (temp_file.isDirectory()){
	    					file_name = temp_file.getAbsolutePath() + File.separator + file_name;
	    				} else {
	    					return null; // rpi 880
	    				}
	    			}
	    		}
	    	}
	    	index = file_name.lastIndexOf(".");
	    	int index1 = file_name.lastIndexOf(File.separatorChar); // RPI 1210 
	    	if (index <= index1){ // RPI 1210 
	    		// concat default type if none
	    		file_name = file_name.trim() + file_type;
	    	}
	    	return file_name;
}
public String fix_file_separators(String name){
	/*
	 * 1.  Replace \ with / if Linux else / with |
	 * 2.  Replace ..\ or ../ with parent path
	 * 3.  Remove embedded ./ or .\
	 */
    if (z390_os_type == z390_os_linux){ // RPI 532 file separator fix
    	name = find_bslash.matcher(name).replaceAll("/");  // RPI 1080
    } else {
    	name = find_slash.matcher(name).replaceAll("\\\\");  // RPI 1080
    }
	// proces any ..\..\ relative paths RPI 1097
	File temp_file = new File(System.getProperty("user.dir"));
	boolean parent_path = false;
	while (name.length() >= 3 && name.substring(0,3).equals(".." + File.separator)){
		parent_path = true;
		temp_file = new File(temp_file.getParent());
		name = name.substring(3);
	}
	// remove leading .\ for rel file RPI 1097
	if (name.length() >= 2 && name.substring(0,2).equals("." + File.separator)){
		name = name.substring(2);
	}
	// replace embeeded .\ with \  RPI 1097
	int index = 0;
	while (index < name.length() - 1){
		if (name.charAt(index) == '.'){
			if (name.charAt(index+1) == File.separatorChar){
				name = name.substring(0,index) + name.substring(index+2);
				index--;
			}
		}
		index++;
	}
	// remove trailing \ RPI 1097
	index = name.length()-1;
	while (index >= 0 && name.charAt(index) == File.separatorChar){
		name = name.substring(0,index);
		index--;
	}
	// prefix parent path if any  RPI 1097
	if (parent_path){
		name = temp_file.getPath() + File.separator + name;
	}
	return name;
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
	 *       else if directory path has *.type use
	 *       the type instead of default file_type. 
	 */
	boolean explicit_type = false;
	File    temp_file;
	if (file_name == null)return null; // RPI 459
	if (file_name.charAt(0) == '"'){
		file_name = file_name.substring(1,file_name.length()-1); // RPI 1074 
	}
	file_name  = fix_file_separators(file_name);
	int index  = file_name.lastIndexOf(File.separator); // rpi 1210 
	int index1 = file_name.lastIndexOf('.');
	if (index1 > index){ // rpi 1210
		file_type_def = file_name.substring(index1); // RPI 756 rpi 1210 
		explicit_type = true;
		file_name = file_name.substring(0,index1);   // RPI 756 rpi 1210 
	}
	
	if (index == -1
		&& (file_name.length() > 2 
			&& file_name.charAt(1) == ':')){
		index = 2;
	}	
	if (index >= 0){
		// file_name has explicit path so use it
		temp_file = new File(file_name + file_type_def);
		if (temp_file.isFile()){
			return temp_file.getPath(); // RPI 756
		}
	} else {
		// search directory list for file
		parm_dir_list = fix_file_separators(parm_dir_list); // RPI 1080
		index = 0;
		int path_len = 0;  
		while (index <= parm_dir_list.length()){
			file_type = file_type_def;
			index1 = parm_dir_list.substring(index).indexOf(";");
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
			if (index2 >= 0){  // RPI 756
				if (!explicit_type){
					file_type = file_dir.substring(index2+1);
				}
				file_dir  = file_dir.substring(0,index2);
			}
			if (file_dir.length() > 0){
				if (!file_dir.substring(file_dir.length()-1,file_dir.length()).equals(File.separator)){
					if (file_dir.length() != 2 || file_dir.charAt(1) != ':'){
						file_dir = file_dir + File.separator;
					}
				}
				temp_file = new File(file_dir + file_name + file_type);
			} else {
				temp_file = new File(file_name + file_type);
			}
			if (temp_file.isFile()){
				return temp_file.getPath(); // RPI 499 drop upper case
			}
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
	  if (op_name[index].length() > 4 && op_name[index].substring(op_name[index].length()-1).equals("?")){	
		// add alternate opcodes for ? = blank and A  RPI 1125
		if (find_key_index('O',op_name[index].substring(0,op_name[index].length()-1)) == -1){
			if(!add_key_index(index)){ 
				return false;
			}
		} else {
			return false;
		}
		if (find_key_index('O',op_name[index].substring(0,op_name[index].length()-1).concat("A")) == -1){
			if(!add_key_index(index)){ 
				return false;
			}
		} else {
			return false;
		}
	  } else {
			if (find_key_index('O',op_name[index]) == -1){
				if(!add_key_index(index)){ 
					return false;
				}
			} else {
				return false;
			}
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
	 *   2.  Set lkd_ignore true if explicit .OBJ found RPI 735
	 */
	lkd_ignore = false;
	if (file_name.charAt(0) == '\"'   // strip lsn quotes
		|| file_name.charAt(0) == '\''){
		file_name = file_name.substring(1,file_name.length() - 1);
	}
	file_name = fix_file_separators(file_name); // RPI 1080
    int index = file_name.lastIndexOf(File.separator);
    if (index != -1){  // get dir path if any
    	dir_pgm = file_name.substring(0,index+1);
    	file_name = file_name.substring(index + 1); // RPI 499 drop upper case
    } else if (file_name.length() > 1 && file_name.charAt(1) == ':'){
    	File temp_file = new File(file_name.substring(0,2));
    	try {
    		dir_pgm = temp_file.getCanonicalPath() + File.separator;
    	} catch (Exception e){
    		return false;
    	}
    	file_name = file_name.substring(2); //RPI113
    } else {
    	dir_pgm = dir_cur;
	  	// RPI 499 drop upper case file_name = file_name.toUpperCase();
    }
    index  = file_name.lastIndexOf(File.separator); // rpi 1210 
    int index1 = file_name.lastIndexOf('.');
    if (index1 > index){  // strip extension if any rpi 1210 
    	pgm_name = file_name.substring(0,index1);
    	if (file_name.substring(index1).toUpperCase().equals(".OBJ")){
    		lkd_ignore = true;  // RPI 735 ignore LKD for link with explicit OBJ file
    	}
    	if (!file_type.equals(mlc_type)){ //RPI169
    		pgm_type = file_type;
    	} else {
    		pgm_type = file_name.substring(index1); // RPI 1210
    	}
    } else {
     	pgm_name = file_name;
     	pgm_type = file_type;
    }
    return true;
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
	 *       and remove any cancel entry.  // RPI 404
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
			return false;  // RPI 773
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
	   int bytes = 0; // RPI 1205
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
		   bytes++;
		   if (bytes > 4)return false; // RPI 1205
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
	 *     length <= 80 if not * in col 1.
	 *
	 */
	if (temp_line.length() > max_line_len){ // RPI 437
		return false; 
	}
	int index = 0;
    while (index < temp_line.length()){
    	int next_char = temp_line.charAt(index) & 0xff; // RPI 744
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
		return text + pad_spaces(pad_len); // RPI 902
	} else {
		return text;
	}
}
public String pad_spaces(int n){ // RPI 902
	/*
	 * return n space characters
	 */
	if (n > pad_spaces_len){
        init_pad_spaces(n);  
	}
	return String.valueOf(pad_spaces,0,n);
}
public String right_justify(String text,int padded_len){
	/*
	 * return text right justified in field
	 * if field larger than text
	 */
	int pad_len = padded_len - text.length();
	if (pad_len > 0){
		return pad_spaces(pad_len) + text; // RPI 902
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
	if (dup_count <= 0){
		return ""; // RPI 774
	}
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
public String trim_continue(String line, boolean first_line,int ictl_end,int ictl_cont){
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
	if (eol_index >= ictl_end+1){  // RPI 728
		eol_index = ictl_end; // RPI 315 // RPI 728
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
		    if (split_op.equals("EXEC")){ // RPI 905
		    	exec_line = true;
		    } else {
		    	exec_line = false;
		    }
		} else {
			split_op_index = -1;
			split_op_type  = -1;
		}
	} else {
		// continued line
		if (split_comment){
			if (line.length() > 16){
				return line.substring(15,eol_index); // RPI 463
			} else {
				return line.substring(0,eol_index);
			}
		}
		if (line.length() >= ictl_cont){     // RPI 728
			split_parms_index = ictl_cont-1; // RPI 728
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
				    && !exec_line // rpi 905   
				){
					split_parm_end = true; // force end
				}
		}
	}
	return split_quote_text + line.substring(split_parms_index,eol_index); // return line with no comma,space
}
public void split_line(String line){  // RPI 313
	/*
	 * split line into 4 strings:
	 *   split_label
	 *   split_op
	 *   split_parms 
	 * using precompiled patterm  RPI 313
	 * 
	 * 4 fields are null if none
	 * 
	 */
	split_label = null;
	split_op    = null;
	split_parms = null;
	if (line == null || line.length() == 0){
		return;
	}
	find_parm_match = find_non_space_pattern.matcher(line);
	if (line.charAt(0) > ' '){
		find_parm_match.find();
		split_label = find_parm_match.group();
	}
	if (find_parm_match.find()){
		split_op = find_parm_match.group().toUpperCase(); // RPI 532
		split_op_index = find_parm_match.start();
		if (find_parm_match.find()){
			split_parms_index = find_parm_match.start();
			split_parms = line.substring(split_parms_index);
		} else {
			split_parms_index = -1;
		}
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
	 * open trace file if trace options on for M, A, L, E
	 */
	if (text != null 
		&& text.length() > 13
	    && text.substring(0,6).equals(text.substring(7,13))){ // RPI 659
	    text = text.substring(7); // RPI 515 RPI 659
	}
	if (!force_nocon &&(opt_con || opt_test)){  // RPI 689 RPI 935
		System.out.println(text);
	}
	if (trace_file == null){
		try {
			trace_file = new File(trace_file_name);
			trace_file_buff = new BufferedWriter(new FileWriter(trace_file));
			put_trace(started_msg); // RPI 755 RPI 1149 
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
		abort_error(18,"maximum trace file size exceeded"); // RPI 731
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
    public String get_cur_bal_line_id(int file_num, int file_line_num, int bal_line_num, boolean mac_gen, char line_type){
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
    		&& mac_gen){ // RPI 891 
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
    public boolean fp_get_dfp_bin(int dfp_type,BigDecimal dfp_bd){
    	/*
    	 * store binary DD,ED, or LD format
    	 * in fp_work_reg.  Return true if value within range.
    	 * 
         * Notes:
         *   1.  Set DFP exponent to explicit decimal point
         *       else preferred exponent is 0.
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
    	dfp_dec_index = dfp_digits.indexOf('.');
    	dfp_exp_index = dfp_digits.indexOf('E');
    	dfp_exp = 0;
    	dfp_scf = 0;
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
    	/*
    	 * strip leading zeros
    	 */
    	int index = 0;
    	int limit = dfp_digits.length() - 1;
    	while (index < limit 
    		   && dfp_digits.charAt(index) == '0'){
    		index++;
    	}
    	if (index > 0){
    		dfp_digits = dfp_digits.substring(index);
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
    		int cf5_index = (dfp_exp & 0x300) >>> 4 
                            | (dfp_digits.charAt(0) & 0xf); // RPI 820
            if (cf5_index > dfp_exp_bcd_to_cf5.length){
            	cf5_index = dfp_exp_bcd_to_cf5.length-1; // RPI 820
            }
    		dfp_scf = fp_sign | dfp_exp_bcd_to_cf5[cf5_index]; // RPI 820
    		fp_work_reg.putLong(0,
    				       (long)dfp_scf << 58 
    				     | (long)(dfp_exp & 0xff) << 50
    				     | get_dfp_ccf_digits(16,1,15));
    		return true;
    	case 4: // fp_ed_type s1,cf5,bxcf8,ccf50
            dfp_digits = ("0000000" + dfp_digits).substring(dfp_digits.length());
            cf5_index = (dfp_exp & 0xc0) >>> 2 
		             | (dfp_digits.charAt(0) & 0xf); // RPI 1211 
            if (cf5_index >= dfp_exp_bcd_to_cf5.length){
            	cf5_index = dfp_exp_bcd_to_cf5.length-1; // RPI 820
            }
            dfp_scf = fp_sign | dfp_exp_bcd_to_cf5[cf5_index];
    		fp_work_reg.putInt(0,
    				       (int)(dfp_scf << 26
                                 | ((dfp_exp & 0x3f) << 20
                                 | (int)get_dfp_ccf_digits(7,1,6)
                                )));
    		return true;
    	case 7: // fp_ld_type s1,cf5,bxdf12,ccf110
            dfp_digits = ("0000000000000000000000000000000000" + dfp_digits).substring(dfp_digits.length());
    		cf5_index = (dfp_exp & 0x3000) >>> 8 | (dfp_digits.charAt(0) & 0xf); // RPI 820
            if (cf5_index >= dfp_exp_bcd_to_cf5.length){
            	cf5_index = dfp_exp_bcd_to_cf5.length-1; // RPI 820
            }
            dfp_scf = fp_sign | dfp_exp_bcd_to_cf5[cf5_index]; // RPI 820
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
    	 *   1.  ASM required
    	 *   2.  Any error limit can prevent finding
    	 *       all the missing copybooks and macros
    	 *       due to pre-mature abort on error limit.
    	 *       There may still be additional nesting missing
    	 *       macros and copybooks requiring multiple
    	 *       passes after including missing files listed.
    	 */
    	if (opt_asm){
    		opt_errsum = true;
  			max_errors = 0;
    	} else {
    		abort_error(30,"ERRSUM requires option ASM");
    	}
    }
    public char ascii_printable_char(int mem_byte){
    	/*
    	 * return printable ascii char from byte RPI 947
    	 */
		if (opt_ascii){
			return ascii_table.charAt(mem_byte & 0xff);
		} else {
			return ebcdic_table.charAt(mem_byte & 0xff);
		}
    }
    public String ascii_printable_string(String text){
    	/*
    	 * return printable ascii string from string that
    	 * may have non-printable ascii codes RPI 938
    	 */
    	int index = 0;
    	String ascii_text = "";
		while (index < text.length()){
			ascii_text = ascii_text + ascii_table.charAt((byte)text.charAt(index) & 0xff);
			index++;
		}
		return ascii_text;
    }
    public String get_ascii_printable_string(byte[] byte_array, int addr, int len){
    	/*
    	 * return printable ascii string from byte array RPI 947
    	 */
    	int index = 0;
    	String text = "";
		while (index < len){
			if (opt_ascii){
				text = text + ascii_table.charAt(byte_array[addr+index] & 0xff);
			} else {
				text = text + ebcdic_table.charAt(byte_array[addr+index] & 0xff);
			}
			index++;
		}
		return text;
    }
    public String get_ascii_var_string(byte[] byte_array,int mem_addr,int max_len){
    	/*
    	 * return ascii variable length string 
    	 * delimited by null or double quotes which
    	 * are stripped off along with leading or traling 
    	 * spaces.
    	 */
    	String text = "";
    	int index = 0;
    	while (index < max_len){
    		byte data_byte = byte_array[mem_addr+index];
    		char data_char;
    		if (opt_ascii){
    			data_char = ascii_table.charAt(data_byte & 0xff); // RPI 1069
    		} else {
    			data_char = ascii_table.charAt(ebcdic_to_ascii[data_byte & 0xff] & 0xff); //RPI42 RPI 1069
    		}
    		if (data_byte == 0){
    			break;
    		}
    		if (data_char == '"'){
    			if (index != 0){
    				break;
    			}
    		} else {
    			text = text + data_char;
    		}
    		index++;
    	}
        return text.trim();  //RPI111
    }
    public void log_text_append(JTextArea log_text, String msg){
		   /* 
		    * append msg to visible log textarea
		    * and reduce size by 50% when it exceeds
		    * opt_maxlog byte limit.
		    */
    	   tot_log_msg++;
		   if (tot_log_text > opt_maxlog){
			   log_text.replaceRange(" Z390 visible log truncated at msg #" + tot_log_msg + "\n",0,opt_maxlog/2);
			   tot_log_text = tot_log_text - opt_maxlog/2;
		   }
		   tot_log_text = tot_log_text + msg.length() + 1;
		   log_text.append(msg + "\n");
    	   log_text_added = true;
	   }
    public void sleep_now(long mills){
    	/*
    	 * sleep for 1 monitor wait interval
    	 */
    	try {
    		Thread.sleep(mills);
    	} catch (Exception e){
    		z390_abort = true;
    		System.out.println("TZ390E thread sleep error - " + e.toString());
    	    Thread.yield();
    	}
    }
	private String set_path_option(String old_path,String new_path){
		/*
		 * 1.  if new path starts with +, concat with existing path
		 *     else replace existing path option.
		 */
		if (new_path.charAt(0) == '+'){
			return old_path + new_path;  
		} else {
			return new_path; 
		}
	}
	public void put_stat_final_options(){
		/*
		 * list final value of all changed 
		 * options on stats file
		 */
		/*
		 * option flags
		 */
		cmd_parms_len = 21 + systerm_prefix.length();
	    if (opt_align){ // rpi 1073
	        add_final_opt("ALIGN");
	     } else {
	        add_final_opt("NOALIGN");
	     }
	    if (opt_allow){ // rpi 833 allow HLASM extensions
	        add_final_opt("ALLOW");
	     } else {
	        add_final_opt("NOALLOW");
	     }
		if (opt_amode24 ){ // link to run amode24
	        add_final_opt("AMODE24");
	     } else {
	        add_final_opt("NOAMODE24");
	     }
	     if (opt_amode31 ){ // link to run amode31
	        add_final_opt("AMODE31");
	     } else {
	        add_final_opt("NOAMODE31");
	     }
	     if (opt_ascii   ){ // use ascii vs ebcdic
	        add_final_opt("ASCII");
	     } else {
	        add_final_opt("NOASCII");
	     }
	     if (opt_asm     ){ // run az390 assembler as mz390 subtask  RPI 415
	        add_final_opt("ASM");
	     } else {
	        add_final_opt("NOASM");
	     }
	     if (opt_assist     ){ // assemble and emulate ASSIST instr. RPI 812
		        add_final_opt("ASSIST");
		     } else {
		        add_final_opt("NOASSIST");
		     }
	     if (opt_autolink){ // search for external ref. in linklib RPI 822
		        add_final_opt("AUTOLINK");
		     } else {
		        add_final_opt("NOAUTOLINK");
		     }
	     if (opt_bal     ){ // generate bal source output from mz390 RPI 415
	        add_final_opt("BAL");
	     } else {
	        add_final_opt("NOBAL");
	     }
	     if (opt_bs2000  ){ // Seimens BS2000 asm compatibility
	        add_final_opt("BS2000");
	     } else {
	        add_final_opt("NOBS2000");
	     }
	     if (opt_edf){ // exec cics program honoring prolog,epilog
		        add_final_opt("EDF");
		     } else {
		        add_final_opt("NOEDF");
		     }
	     if (opt_cics    ){ // exec cics program honoring prolog,epilog
	        add_final_opt("CICS");
	     } else {
	        add_final_opt("NOCICS");
	     }
	     if (codepage.length() > 0){ // codepage(ascii,ebcdic,list)
		        add_final_opt(codepage);
		     } else {
		        add_final_opt("NOCODEPAGE");
		     }
	     if (opt_comment){ // zcobol source comments in MLC
	    	 add_final_opt("COMMENT");
		     } else {
		        add_final_opt("NOCOMMENT");
		     }
	     if (opt_con     ){ // log msgs to console
	        add_final_opt("CON");
	     } else {
	        add_final_opt("NOCON");
	     }
	     if (opt_dump    ){ // only indicative dump on abend unless on
	        add_final_opt("DUMP");
	     } else {
	        add_final_opt("NODUMP");
	     }
	     if (opt_epilog  ){ // if cics, insert DFHEIRET
	        add_final_opt("EPILOG");
	     } else {
	        add_final_opt("NOEPILOG");
	     }
	     if (opt_errsum  ){ // just list critical errors and summary on ERR file and console 
	        add_final_opt("ERRSUM");
	     } else {
	        add_final_opt("NOERRSUM");
	     }
	     if (opt_extend){ // allow 31 digit P and Z in zcobol 
		        add_final_opt("EXTEND");
		 } else {
		        add_final_opt("NOEXTEND");
		 }
	     add_final_opt("FLOAT(" + opt_float + ")");
	     if (opt_guam    ){ // use gz390 GUAM GUI access method interface
	        add_final_opt("GUAM");
	     } else {
	        add_final_opt("NOGUAM");
	     }
	     if (opt_init    ){ // init regs x'F4' and mem x'F5' vs 0's
		        add_final_opt("INIT");  // RPI 828
		     } else {
		        add_final_opt("NOINIT");
		     }
	     if (opt_list    ){ // generate LOG file
	        add_final_opt("LIST");
	     } else {
	        add_final_opt("NOLIST");
	     }
	     if (opt_listcall){ // list macro calls
	        add_final_opt("LISTCALL");
	     } else {
	        add_final_opt("NOLISTCALL");
	     }
	     if (opt_listuse ){ // list usage at USING and DROP
	        add_final_opt("LISTUSE");
	     } else {
	        add_final_opt("NOLISTUSE");
	     }
	     if (opt_loadhigh){ // load pgms and alloc storage from top down
		        add_final_opt("LOADHIGH");
		     } else {
		        add_final_opt("NOLOADHIGH");
		     }
	     if (opt_mcall   ){ // list MCALL and MEXIT on PRN // RPI 511
	        add_final_opt("MCALL");
	     } else {
	        add_final_opt("NOMCALL");
	     }
	     if (opt_mod){ // generate code file from lz390 with no header/trailer, no rounding, no RLD's RPI 883
		        add_final_opt("MOD");
		     } else {
		        add_final_opt("NOMOD");
		     }
	     if (opt_obj     ){ // generate binary MVS compatible OBJ file RPI 694
	        add_final_opt("OBJ");
	     } else {
	        add_final_opt("NOOBJ");
	     }
	     if (opt_objhex  ){ // generate ascii hex obj records (lz390 accepts bin or hex)
	        add_final_opt("OBJHEX");
	     } else {
	        add_final_opt("NOOBJHEX");
	     }
	     if (opt_pc      ){ // generate macro pseudo code
	        add_final_opt("PC");
	     } else {
	        add_final_opt("NOPC");
	     }
	     if (opt_pcopt   ){ // optimize pc code for speed
	        add_final_opt("PCOPT");
	     } else {
	        add_final_opt("NOPCOPT");
	     }
	     if (opt_pdsmem8){ // optimize pc code for speed
		        add_final_opt("PDSMEM8");
		     } else {
		        add_final_opt("NOPDSMEM8");
		     }
	     if (opt_printall){ // force printing all source on PRN RPI 1127
		        add_final_opt("PRINTALL");
		     } else {
		        add_final_opt("NOPRINTALL");
		     }
	     if (opt_prolog  ){ // if cics, insert DFHEIBLK and DFHEIENT
	        add_final_opt("PROLOG");
	     } else {
	        add_final_opt("NOPROLOG");
	     }
	     if (opt_protect ){ // prevent PSA mods by user
	        add_final_opt("PROTECT");
	     } else {
	        add_final_opt("NOPROTECT");
	     }
	     if (opt_r64){ // RPI 986 allow 64 bit register usage
		        add_final_opt("R64");
		     } else {
		        add_final_opt("NOR64");
		     }
	     if (opt_reformat){ // reformat BAL statements
	        add_final_opt("REFORMAT");
	     } else {
	        add_final_opt("NOREFORMAT");
	     }
	     if (opt_regs    ){ // show registers on trace
	        add_final_opt("REGS");
	     } else {
	        add_final_opt("NOREGS");
	     }
	     if (opt_rmode24 ){ // link to load below line
	        add_final_opt("RMODE24");
	     } else {
	        add_final_opt("NORMODE24");
	     }
	     if (opt_rmode31 ){ // link to load above line
	        add_final_opt("RMODE31");
	     } else {
	        add_final_opt("NORMODE31");
	     }
	     if (opt_stats   ){ // show statistics on STA file
	        add_final_opt("STATS");
	     } else {
	        add_final_opt("NOSTATS");
	     }
	     if (opt_test    ){ // invoke interactive test cmds
	        add_final_opt("TEST");
	     } else {
	        add_final_opt("NOTEST");
	     }
	     if (opt_thread  ){ // continuous loc ctr RPI 1186
		        add_final_opt("THREAD");
		     } else {
		        add_final_opt("NOTHREAD");
		     }
	     if (opt_time    ){ // abend 422 if out of time TIME (sec)
	        add_final_opt("TIME");
	     } else {
	        add_final_opt("NOTIME");
	     }
	     if (opt_timing  ){ // display current date, time, rate
	        add_final_opt("TIMING");
	     } else {
	        add_final_opt("NOTIMING");
	     }
	     if (opt_trace   ){ // trace(e) trace ez390 to TRE
	        add_final_opt("TRACE");
	     } else {
	        add_final_opt("NOTRACE");
	     }
	     if (opt_tracea  ){ // trace(a) az390 assembler to TRA
	        add_final_opt("TRACEA");
	     } else {
	        add_final_opt("NOTRACEA");
	     }
	     if (opt_traceall){ // trace(aeglmpqtv) trace all TRM,TRA,TRL,TRE
	        add_final_opt("TRACEALL");
	     } else {
	        add_final_opt("NOTRACEALL");
	     }
	     if (opt_tracec){ // trace(aceglmpqtv) trace all TRM,TRA,TRL,TRE
		        add_final_opt("TRACEC"); // RPI 862
		     } else {
		        add_final_opt("NOTRACEC");
		     }
	     if (opt_traceg  ){ // trace(g) memory FQE updates to TRE
	        add_final_opt("TRACEG");
	     } else {
	        add_final_opt("NOTRACEG");
	     }
	     if (opt_tracei  ){ // RPI 1157
		        add_final_opt("TRACEI");
		 } else {
		        add_final_opt("NOTRACEI");
		 }
	     if (opt_tracel  ){ // trace(L) lz390 to TRL
	        add_final_opt("TRACEL");
	     } else {
	        add_final_opt("NOTRACEL");
	     }
	     if (opt_tracem  ){ // trace mz390 to TRM
	        add_final_opt("TRACEM");
	     } else {
	        add_final_opt("NOTRACEM");
	     }
	     if (opt_tracep  ){ // trace mz390 pseudo code to TRM
	        add_final_opt("TRACEP");
	     } else {
	        add_final_opt("NOTRACEP");
	     }
	     if (opt_traceq  ){ // trace(q) QSAM file I/O to TRE
	        add_final_opt("TRACEQ");
	     } else {
	        add_final_opt("NOTRACEQ");
	     }
	     if (opt_traces  ){ // trace(s) show MLC source and errors on console
		        add_final_opt("TRACES");
		     } else {
		        add_final_opt("NOTRACES");
		     }
	     if (opt_tracet  ){ // trace(t) TCPIO and TGET/TPUT I/O to TRE
	        add_final_opt("TRACET");
	     } else {
	        add_final_opt("NOTRACET");
	     }
	     if (opt_tracev  ){ // trace(v) VSAM file I/O to TRE
	        add_final_opt("TRACEV");
	     } else {
	        add_final_opt("NOTRACEV");
	     }
	     if (opt_trap    ){ // trap exceptions as 0C5
	        add_final_opt("TRAP");
	     } else {
	        add_final_opt("NOTRAP");
	     }
	     if (opt_trunc    ){ // truncate COMP to PIC digits
		        add_final_opt("TRUNC");
		     } else {
		        add_final_opt("NOTRUNC");
		     }
	     if (opt_ts      ){ // time-stamp logs RPI 662
	        add_final_opt("TS");
	     } else {
	        add_final_opt("NOTS");
	     }
	     if (opt_vcb     ){ // vsam cache operational
	        add_final_opt("VCB");
	     } else {
	        add_final_opt("NOVCB");
	     }
	     if (opt_warn     ){ // zcobol mnote level 4 warnings
		        add_final_opt("WARN");
		     } else {
		        add_final_opt("NOWARN");
		     }
	     if (opt_xref    ){ // cross reference symbols
	        add_final_opt("XREF");
	     } else {
	        add_final_opt("NOXREF");
	     }
	     if (opt_zstrmac){ // allow ZSTRMAC structured macro extensions
		     add_final_opt("ZSTRMAC");
		 } else {
		     add_final_opt("NOZSTRMAC");
		 }
	     /*
	      * option limits
	      */
	     add_final_opt("CHKMAC=" + opt_chkmac); // RPI 747 0-none,1-labels, 2-labels and src after MEND
	     add_final_opt("CHKSRC=" + opt_chksrc); // RPI 747 0-none,1-MLC only,2-all
	     add_final_opt("ERR=" + max_errors);
	     add_final_opt("MAXCALL=" + opt_maxcall);
	     add_final_opt("MAXDISPLAY=" + opt_maxdisplay); // RPI 1131
	     add_final_opt("MAXESD=" + opt_maxesd);
	     add_final_opt("MAXFILE=" + opt_maxfile);
	     add_final_opt("MAXGBL=" + opt_maxgbl);
	     add_final_opt("MAXHEIGHT=" + max_main_height);
	     add_final_opt("MAXLCL=" + opt_maxlcl);
	     add_final_opt("MAXLEN=" + max_line_len);
	     add_final_opt("MAXLINE=" + opt_maxline);
	     add_final_opt("MAXLOG=" + opt_maxlog); // max gui log before trunc
	     add_final_opt("MAXPARM=" + opt_maxparm);
	     add_final_opt("MAXPASS=" + opt_maxpass); // RPI 920
	     add_final_opt("MAXPC=" + opt_maxpc); // pseudo code cache size
	     add_final_opt("MAXQUE=" + opt_maxque); // max cmd out before trunc
	     add_final_opt("MAXRLD=" + opt_maxrld);
	     add_final_opt("MAXSIZE=" + max_file_size);
	     add_final_opt("MAXSYM=" + opt_maxsym);
	     add_final_opt("MAXWARN=" + max_mnote_warning);
	     add_final_opt("MAXWIDTH=" + max_main_width);
	     add_final_opt("MEM=" + max_mem);
	     add_final_opt("MINHEIGHT=" + min_main_height);
	     add_final_opt("MINWIDTH=" + min_main_width);
	     add_final_opt("MNOTE=" + opt_mnote); // RPI 1142
	     add_final_opt("PARM=" + opt_parm);
	     add_final_opt("PROFILE=" + opt_profile);
	     add_final_opt("STATS=" + stats_file_name);
	     add_final_opt("SYSPARM=" + opt_sysparm);
	     add_final_opt("SYSTERM=" + systerm_file_name);
	     add_final_opt("TESTDD=" + test_ddname);
	     add_final_opt("TIME=" + max_time_seconds);
	     add_final_opt("TRACE=" + trace_options);
	     add_final_opt("Z390ACROBAT=" + z390_acrobat);
	     add_final_opt("Z390BROWSER=" + z390_browser);
	     add_final_opt("Z390COMMAND=" + z390_command);
	     add_final_opt("Z390EDITOR=" + z390_editor);
	     /*
	      * option directories and files
	      */
	     
	     add_final_opt("INSTALL=" + opt_install_loc);
	     add_final_opt("IPL=" + opt_ipl);
	     add_final_opt("LOG=" + log_file_name);
	     add_final_opt("SYS390=" + dir_390); // SYS390() load module
	     add_final_opt("SYSBAL=" + dir_bal); // SYSBAL() az390 source input
	     add_final_opt("SYSCPY=" + dir_cpy); // SYSCPY() mz390 copybook lib defaults to dir_mac RPI 742
	     add_final_opt("SYSDAT=" + dir_dat); // SYSDAT() mz390 AREAD extended option
	     add_final_opt("SYSERR=" + dir_err); // SYSERR() ?z390 systerm error file directory
	     add_final_opt("SYSLOG=" + dir_log); // SYSLOG() ez390 log // RPI 243
	     add_final_opt("SYSLST=" + dir_lst); // SYSLST() lz390 listing 
	     add_final_opt("SYSMAC=" + dir_mac); // SYSMAC() mz390 macro lib
	     add_final_opt("SYSMLC=" + dir_mlc); // SYSMLC() mz390 source input
	     add_final_opt("SYSPCH=" + dir_pch); // SYSPCH() mz390 punch output dir
	     add_final_opt("SYSPRN=" + dir_prn); // SYSPRN() az390 listing
	     add_final_opt("SYSOBJ=" + dir_obj); // SYSOBJ() lz390 object lib
	     add_final_opt("SYSOPT=" + dir_opt); // SYSOPT() OPT options @file path defaults to dir_mac RPI 742
	     add_final_opt("SYSTRC=" + dir_trc); // SYSTRC() trace file directory
	}
	private void add_final_opt(String token){
		/*
		 * add final option value to final_option
		 * formatted string.
		 */
		while (token.length() > max_cmd_parms_line - 2){
			put_stat_line("final options=" + token.substring(0,max_cmd_parms_line - 2));
			token = token.substring(max_cmd_parms_line - 2);
		}
		if (token.length() > 0){
			put_stat_line("final_options=" + token);
		}
	}
	public synchronized void abort_case(){ // RPI 646
		/*
		 * abort case with invalide index
		 * RPI 849 used by pz390, mz390
		 */
		abort_error(22,"internal error - invalid case index");
	}
	public String cur_date(){
		/*
		 * return MM/DD/YY 
		 * or constant if notiming
		 *      
		 */
		if (opt_timing){
			return sdf_MMddyy.format(new Date());
		} else {
			return "MM/DD/YY";
		}
	}
	public String cur_time(boolean space_pad){
		/*
		 * return HH:MM:SS with or without space 
		 * or 0 length string if notiming
		 *      
		 */
		if (opt_timing){
			if (space_pad){
				return sdf_HHmmss.format(new Date()) + " ";
			} else {
				return sdf_HHmmss.format(new Date());
			}
		} else {
			return "";
		}
	}
	public void init_codepage(String codepage_parm){
		/*
		 * initialize ascii and ebcdic translate tables 
		 * using specified Unicode codepages.  If list is on display
		 * mapping between ascii Unicode table and the corresponding
		 * ascii and ebcdic byte values in hex and list available  ascii
		 * and ebcdic charset codepages.  If either of the names are
		 * not valid, a list of the current ascii default and all 
		 * available Charset codepages will be listed.  The two
		 * codepages will be verified to have the required minimum
		 * ebcdic code mapping for z390 assembler A-Z,a-z,0-9,@#$,
		 * blank, and &’()*+-./:=_.  Any characters that have mapping
		 * will attempt to print otherwise they will appear as periods.
		 */
		// init hardcoded ascii/ebcdic tables if no codepage
	    if (codepage_parm.length() == 0){   // RPI 1069
    		init_ascii_ebcdic();
    		if (opt_traceall){
    			put_systerm("TRACEALL default ascii ebcdic codepage");
    			list_hex_ascii_ebcdic();
    		}
    		opt_codepage = false;
	    	return;
	    }
		// initialize 256 byte values
		int index = 0;
		while (index < 256){
			init_charset_bytes[index]=(byte)index;
			index++;
		}
		// extract 3 operands from codepage(ascii+ebcdic+list)
		index = codepage_parm.indexOf('+');
		if (index > 0){
			ascii_charset_name = codepage_parm.substring(9,index);
			codepage_parm = codepage_parm.substring(index+1);
			index = codepage_parm.indexOf('+');
			if (index > 0){
				ebcdic_charset_name = codepage_parm.substring(0,index);
			    if (codepage_parm.substring(index+1,codepage_parm.length()-1).toUpperCase().equals("LIST")){
			    	list_charset_map = true;
			    }
			} else {
				ebcdic_charset_name = codepage_parm.substring(0,codepage_parm.length()-1);
			}
		} else {
			report_codepage_error("invalid CODEPAGE(ascii+ebcdic+list) = " + codepage);
			return;			
		}
		try {			 
		     test_ascii = new String(init_charset_bytes,ascii_charset_name);
		     if (!check_test_ascii()){
		    	 report_codepage_error("ascii codepage validation error");
		    	 return;		    	 
		     }
		} catch (Exception e){
            report_codepage_error("codepage charset load error on " + ascii_charset_name);
            return;
		}
		try {
		     test_ebcdic = new String(init_charset_bytes,ebcdic_charset_name);
		     if (!check_test_ebcdic()){
		    	 report_codepage_error("ebcdic codepage validation error on " + ebcdic_charset_name);
		    	 return;		    	 
		     }
		} catch (Exception e){
			if (!load_ebcdic_charset_hex_file()){
				report_codepage_error("codepage charset load error on " + ebcdic_charset_name);
            	return;
			}
		}
		try {
		     if (list_charset_map){
		    	 put_systerm("CODEPAGE listing for " + codepage);
		    	 put_systerm("Default  ascii  Charset codepage is - " + default_charset_name.trim());
		    	 put_systerm("Selected ascii  Charset codepage is - " + ascii_charset_name);
		    	 put_systerm("Selected ebcdic Charset codepage is - " + ebcdic_charset_name);
		     }
		     init_charset_tables();
		     if (list_charset_map){
		    	 list_ebcdic_ascii_unicode();
		    	 list_available_charsets();
		     }		     
		     opt_codepage = true;
		} catch (Exception e){
             report_codepage_error("codepage charset table initialization error");
             return;
		}
	}
	private void init_charset_tables(){
		/*
		 * 1.  copy test tables to live tables
		 * 2.  initialize translate tables
		 * 3.  initialize printable character table
		 */
		// copy charsets
		ascii_table = test_ascii;
		ebcdic_table = test_ebcdic;
		// init translate tables from Charsets
		int i = 0;
		while (i < 256){
			ascii_to_ebcdic[i]=0;
			ebcdic_to_ascii[i]=0;
			i++;
		}
		i = 0;
		int j;
		while (i < 256){
			j = ebcdic_table.charAt(i) & 0xff;
			if (ascii_to_ebcdic[j] == 0
				&& ebcdic_to_ascii[i] == 0){
			   ascii_to_ebcdic[j]=(byte)i;
			   ebcdic_to_ascii[i]=(byte)j;
			}
			i++;
		}
		// abort if any duplicates in minimum ascii to ebcdic
		i = 0;
		int k;
		int l;
		while (i < 256){
			j = ebcdic_table.charAt(i) & 0xff;
            if ((ascii_to_ebcdic[j] & 0xff) != i
            	|| (ebcdic_to_ascii[i] & 0xff) != j){
            		k = ascii_min_char.indexOf(ebcdic_table.charAt(i));
            		l = ascii_min_char.indexOf((char)i);
            		if (k >= 0 && l >= 0){
            			String msg = "duplicate ascii/ebcdic conversion at - " + Integer.toHexString(i) + "/" + Integer.toHexString(j);
            			report_codepage_error(msg);
            			abort_error(31,msg);
            		}
            	}
			i++;
		}
		if (list_charset_map){
           	list_hex_ascii_ebcdic();
		}
		// replace control characters with period for printing
		int index = 0;
		while (index < 256){
			if ((ascii_table.charAt(index) & 0xff) < 0x20){
				if (index == 0){
					ascii_table = "." + ascii_table.substring(index+1);
				} else if (index == 255){
					ascii_table = ascii_table.substring(0,255)+".";
				} else {
					ascii_table = ascii_table.substring(0,index)+"."+ascii_table.substring(index+1);
				}
			}
			if ((ebcdic_table.charAt(index) & 0xff) < 0x20){
				if (index == 0){
					ebcdic_table = "." + ebcdic_table.substring(index+1);
				} else if (index == 255){
					ebcdic_table = ebcdic_table.substring(0,255)+".";
				} else {
					ebcdic_table = ebcdic_table.substring(0,index)+"."+ebcdic_table.substring(index+1);
				}
			}
			index++;
		}
	}
	private void report_codepage_error(String msg){
		/*
		 * report codepage parm error
		 */		
		put_systerm("CODEPAGE option error - " + msg);
		put_systerm("z390 default ascii/ebcdic tables used");
		put_systerm("Default ascii Charset codepage is - " + default_charset_name);
		list_available_charsets();
		opt_codepage = false;
		init_ascii_ebcdic();
	}
	private void list_ebcdic_ascii_unicode(){
		/* 
		 * list unicode, char, ascii hex, ebcdic hex
		 */
		put_systerm("hex-ebcdic/hex-ascii/print-char/unicode listing");
   	 int index = 0;
   	 while (index < 64){
   		put_systerm(
   				 map_text(index) 
   			   + map_text(index+64)
   			   + map_text(index+128)
   			   + map_text(index+192));
   		 index++;
   	 }
	}
   	 private String map_text(int index){
   		 /*
   		  * return text index,hex-ebcdic,hex-ascii,char,U+hex
   		  */
   		 String codepoint = "000"+Integer.toHexString(test_ascii.codePointAt(index));
   		 codepoint = codepoint.substring(codepoint.length()-4);
   		 char test_char;
   		 if (index >= 32){
   			 test_char = test_ascii.charAt(index);
   	     } else {
   		     test_char = '.';
   	     }
   		 String test_ascii_hex = "0" + Integer.toHexString((byte)test_ascii.charAt(index));
   		 test_ascii_hex = test_ascii_hex.substring(test_ascii_hex.length()-2);
   		 String test_ebcdic_hex = "0" + Integer.toHexString((byte)test_ebcdic.charAt(index));
   		 test_ebcdic_hex = test_ebcdic_hex.substring(test_ebcdic_hex.length()-2);
   		 String text =  
   			                  test_ebcdic_hex
   				     + "/"  + test_ascii_hex
   				     + "/"  + test_char
   				     + "/U" + codepoint + "  ";
   	     return text;
	}
	@SuppressWarnings("unchecked")
	private void list_available_charsets(){
		put_systerm("available ascii and ebcdic charset codepages");
		int tot_charset = 0;
		int tot_ebcdic  = 0;
		int tot_ascii   = 0;
		Map map = Charset.availableCharsets();
		Iterator it = map.keySet().iterator();
		while (it.hasNext()) {
			tot_charset++;
	        // Get charset name
			String charset_name = (String)it.next();
            try {
   		        test_ascii = new String(init_charset_bytes,charset_name);
		        if (check_test_ascii()){
		        	tot_ascii++;
		        	put_systerm("valid ascii  charset - " + charset_name);
		        } else {
	   		        test_ebcdic = new String(init_charset_bytes,charset_name);
			        if (check_test_ebcdic()){
			        	tot_ebcdic++;
			        	put_systerm("valid ebcdic charset - " + charset_name);
			        }
		        }		        
            } catch (Exception e){            	
            }
		}
		put_systerm("total charsets=" + tot_charset + "  total ebcdic=" + tot_ebcdic + "  total ascii=" + tot_ascii);
	}
	private boolean check_test_ascii(){
		/*
		 * return true if test_ascii charset
		 * meets the following  tests:
		 *   1.  Length 256
		 *   2.  hex char
		 *       20  space
		 *       30  zero
		 *       39  nine
		 *       41  A
		 *       5A  Z
		 *       61  a
		 *       7A  z
		 */
		if (test_ascii.length() != 256
				|| test_ascii.charAt(0x30) != '0'
				|| test_ascii.charAt(0x39) != '9'
				|| test_ascii.charAt(0x41) != 'A'
				|| test_ascii.charAt(0x5A) != 'Z'
				|| test_ascii.charAt(0x61) != 'a'
				|| test_ascii.charAt(0x7A) != 'z'
				|| test_ascii.charAt(0x24) != '$'	
				|| test_ascii.charAt(0x23) != '#'
				|| test_ascii.charAt(0x40) != '@'
				|| test_ascii.charAt(0x20) != ' '
				|| test_ascii.charAt(0x26) != '&'
				|| test_ascii.charAt(0x27) != '\''
				|| test_ascii.charAt(0x28) != '('
				|| test_ascii.charAt(0x29) != ')'	
				|| test_ascii.charAt(0x2A) != '*'
				|| test_ascii.charAt(0x2B) != '+'
				|| test_ascii.charAt(0x2C) != ','
				|| test_ascii.charAt(0x2D) != '-'
				|| test_ascii.charAt(0x2E) != '.'	
				|| test_ascii.charAt(0x2F) != '/'
				|| test_ascii.charAt(0x3A) != ':'	
				|| test_ascii.charAt(0x3D) != '='
				|| test_ascii.charAt(0x5F) != '_'
			){
			return false;
		}
		return true;
	}
	private boolean check_test_ebcdic(){
		/*
		 * return true if test_ebcdic charset
		 * meets minimum character requirements
		 */
		if (test_ebcdic.length() != 256
			|| test_ebcdic.charAt(0xF0) != '0'
			|| test_ebcdic.charAt(0xF9) != '9'
			|| test_ebcdic.charAt(0xC1) != 'A'
			|| test_ebcdic.charAt(0xE9) != 'Z'
			|| test_ebcdic.charAt(0x81) != 'a'
			|| test_ebcdic.charAt(0xA9) != 'z'
			|| test_ebcdic.charAt(0x5B) != '$'	
			|| test_ebcdic.charAt(0x7B) != '#'
			|| test_ebcdic.charAt(0x7C) != '@'
			|| test_ebcdic.charAt(0x40) != ' '
			|| test_ebcdic.charAt(0x50) != '&'
			|| test_ebcdic.charAt(0x7D) != '\''
			|| test_ebcdic.charAt(0x4D) != '('
			|| test_ebcdic.charAt(0x5D) != ')'	
			|| test_ebcdic.charAt(0x5C) != '*'
			|| test_ebcdic.charAt(0x4E) != '+'
			|| test_ebcdic.charAt(0x6B) != ','
			|| test_ebcdic.charAt(0x60) != '-'
			|| test_ebcdic.charAt(0x4B) != '.'	
			|| test_ebcdic.charAt(0x61) != '/'
			|| test_ebcdic.charAt(0x7A) != ':'	
			|| test_ebcdic.charAt(0x7E) != '='
			|| test_ebcdic.charAt(0x6D) != '_'	
			){
			return false;
		}
		return true;
	}
	public void list_hex_ascii_ebcdic(){
		/*
		 * list ascii to ebcdic and ebcdic to aascii
		 * conversion tables in hex for debugging
		 * 
		 */
		put_systerm("hex ascii Charset - " + ascii_charset_name);
		int i = 0;
		String hexline = "";
		String hex;
		while (i < 256){
			hex = "0" + Integer.toHexString(ascii_table.charAt(i) & 0xff);
			hexline = hexline + hex.substring(hex.length()-2);
			if (hexline.length() >= 32){
				put_systerm(Integer.toHexString(i/16)+"0 " + hexline);
				hexline = "";
			}
			i++;
		}
		put_systerm("hex ebcdic charset - " + ebcdic_charset_name);
		i = 0;
		hexline = "";
		while (i < 256){
			hex = "0" + Integer.toHexString(ebcdic_table.charAt(i) & 0xff);
			hexline = hexline + hex.substring(hex.length()-2);
			if (hexline.length() >= 32){
				put_systerm(Integer.toHexString(i/16)+"0 " + hexline);
				hexline = "";
			}
			i++;
		}	
		put_systerm("hex ebcdic_to_ascii table");
		i = 0;
		while (i < 256){
			hex = "0" + Integer.toHexString(ebcdic_to_ascii[i] & 0xff);
			hexline = hexline + hex.substring(hex.length()-2);
			if (hexline.length() >= 32){
				put_systerm(Integer.toHexString(i/16)+"0 " + hexline);
				hexline = "";
			}
			i++;
		}
		put_systerm("hex ascii_to_ebcdic table");
		i = 0;
		while (i < 256){
			hex = "0" + Integer.toHexString(ascii_to_ebcdic[i] & 0xff);
			hexline = hexline + hex.substring(hex.length()-2);
			if (hexline.length() >= 32){
				put_systerm(Integer.toHexString(i/16)+"0 " + hexline);
				hexline = "";
			}
			i++;
		}
	}
	private boolean load_ebcdic_charset_hex_file(){
		/*
		 * load ebcdic_charset_name as alternate
		 * source for system defiend ebcdic_charset_name
		 */
		try {
			BufferedReader ebcdic_hex_buff = new BufferedReader(new FileReader(new File(ebcdic_charset_name)));
		    String hex_rec = ebcdic_hex_buff.readLine();
		    int hex_offset = 0;
		    char[]  ebcdic_charset = new char[256];
		    while (hex_rec != null && hex_offset < 256){
		    	if (hex_rec.charAt(0) != '*'){
		    		int ver_offset = Integer.valueOf(hex_rec.substring(0,2),16);
		    		if (hex_offset != ver_offset){
		    			return false;
		    		}
		    		if (hex_offset < 256){
		    			int index = 0;
		    			while (index < 32){
		    				int hex_byte = Integer.valueOf(hex_rec.substring(3+index,5+index),16);
		    				ebcdic_charset[hex_offset] =(char)hex_byte;
		    				hex_offset++;
		    				index = index+2;
		    			}
		    		}
		    	}
		    	hex_rec = ebcdic_hex_buff.readLine();
		    }
		    if (hex_offset == 256){
                test_ebcdic = String.valueOf(ebcdic_charset);
		    } else {
		    	return false;
		    }
		} catch (Exception e){
			return false;
		}
		return true;
	}
public boolean check_java_version(){
	/*
	 * verify version is from known vendor 
	 * and version is 1.6+
	 */
		if (java_vendor.equals("Sun Microsystems Inc.") 
			|| java_vendor.equals("Oracle Corporation") // RPI 1175
			|| java_vendor.equals("Apple Inc.")){       // RPI 1174
			if (java_version.compareTo("1.6") < 0){ // RPI 1199
                return false;
			}
		} else { // RPI 1174
			return false;
		}
		return true;
}
}