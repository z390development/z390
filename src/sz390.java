import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.DataInputStream;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.PrintStream;
import java.io.RandomAccessFile;
import java.lang.reflect.Array;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.net.InetAddress;
import java.net.ServerSocket;
import java.net.Socket;
import java.nio.ByteBuffer;
import java.text.SimpleDateFormat;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.LinkedList;
import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.Timer;
;@SuppressWarnings("unchecked")
public  class  sz390 implements Runnable {
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

    ez390 is the emulator component of z390 which can be called from
    z390 GUI interface or from command line to execute 390 load
    module files.  

    ****************************************************
    * Maintenance
    ****************************************************
    * 01/26/06 copied from ez390.java and modified
    * 02/04/06 RPI 197 correctly set/reset trace T/G/Z
    * 02/17/06 RPI 136 add gz390_abort support to close down
    *          correctly with log support.
    * 02/21/06 RPI 208 set tz390.z390_abort to sync term.
    * 03/02/06 RPI 220 close down normally after tz390.z390_abort set
    * 03/03/06 RPI 209 show current date/time with stats if opt_timing
    * 03/04/06 RPI 221 set R15 return code for TGET/TPUT from tpg_rc
    * 03/13/06 RPI 229 correct flush of FPR's for dump
    * 04/03/06 RPI 271 pad hex and text for dump_mem
    * 04/05/06 RPI 270 support 8 byte RLD's
    * 04/11/06 RPI 244 change FREEMAIN to R0=LEN, R1=ADDR
    *          support ESPIE and ESTAE PARAM, plus CT,OV
    * 04/23/06 RPI 279 add STIMER REAL, TTIMER support
    * 05/01/06 RPI 305 add ESPIE stack for SET/RESET
    *          check for ESTAE recursive abort
    * 05/05/06 RPI 280 add WAIT ECBLIST support
    * 05/05/06 RPI 281 add DCBE support for EODAD and SYNAD
    * 05/06/06 RPI 243 reset errors and rc for TEST z command.
    * 05/07/06 RPI 311 correct BLDL entry length to not include 
    *          length and check entries in ascending order
    * 05/09/06 RPI 312 add pgm name to return code msg and
    *          add svc extended trace information
    * 07/01/06 RPI 352 relax align of DCB to 4 bytes
    * 07/05/06 RPI 347 add test address stop break
    * 07/07/06 RPI 358 prevent trap on invalid S command
    * 07/17/06 RPI 360 and 370 add CFD and CTD conversion svcs
    * 07/20/06 RPI 377 prevent DCB SYNAD recursion on missing file
    * 08/08/06 RPI 397 synchronize system.out
    * 09/02/06 RPI 428 turn off high bit for test break addr
    * 09/06/06 RPI 395 add mult addr stop, mult. indirect, and supp EPA
    *          supress dup. dump lines
    * 09/19/06 RPI 452 return time for TTIMER CANCEL,TU/MIC 
    * 11/04/06 RPI 484 support TRE trace file for TRACE and TRACEALL  
    * 11/10/06 RPI 471 cancel stimer exit at abend   
    * 11/10/06 RPI 477 support ASCII in CTD and CFD  
    * 11/12/06 RPI 490 correct TEST command processor 0r, traps 
    * 11/12/06 RPI 491 correct FREEMAIN merge causing corrupted FQE
    * 11/16/06 RPI 499 fix cde for upper/lower case members on Linux
    * 11/28/06 RPI 500 use system newline for Win/Linux
    *          use perl vs command.com for command processor
    * 11/28/06 RPI 505 prevent duplicate messages to console in TEST mode
    * 11/29/06 RPI 507 support CTD/CFD in/out reg val as well as reg addr
    * 12/02/06 RPI 512 support CFD float for INT128 and set RC=0/8/12
    * 12/10/06 RPI 514 add CFD and CTD DFP support types
    * 12/29/06 RPI 526 trap invalid CFD input and return rc=12
    *          also fix CTD/CFD parm address to match amode
    * 01/06/07 RPI 524 add TCPIO svc x'7C' support
    * 01/16/07 RPI 536 issue RC=4 if CTD for DFP infinity or NAN
    * 01/22/07 RPI 542 change GETMAIN output regs for 
    *          compatibility R0=RND LEN, R1=ADDR 
    * 01/28/06 RPI 545 correct user specified timeout limit on CMDPROC
    * 02/03/07 RPI 547 close std i/o before stopping command process
    * 02/08/07 RPI 532 fix dcb file separator if linux
    * 02/20/07 RPI 551 correct ASCII mode for CMDPROC and WTOR
    * 02/24/07 RPI 560 prevent errouneous recursiave abort error
    * 03/30/07 RPI 566 add TCPIO statistics to log if STATS
    * 04/11/07 RPI 587 support 32 DCBLRECLF and DCBBLKSIF fields
    *          plus return 64 bit file length in R0 for OPEN
    * 04/15/07 RPI 583 dump all storage for abend and snap if PDATA=ALL  
    * 04/15/07 RPI 586 support TEST command E to toggle EBCDIC/ASCII 
    * 04/16/07 RPI 596 correctly pass user parm for XCTL 
    * 04/16/07 RPI 592 use cmd_proc_running to control stopping
    *          of processes without distroying queues etc.   
    * 04/28/07 RPI 598 correct error in RPI 596 saving R1 across XCTL 
    *          and correct bug causing delete of wrong CDE after 
    *          multiple XCTL's          
    ********************************************************
    * Global variables                   (last RPI)
    *****************************************************/
    /*
     * static limits
     */
	int    max_tiot_files    = 100;        // max open files
    int    max_cde_pgms      = 500;        // max loaded 390 pgms
    int    max_link_stk      = 50;         // max nested links
    int max_cmd_out        = 10000;        // cmd output character buffer 
    int max_cmd_proc       = 10;           // max cmd processes ID=0-9
   int max_lsn_spec       = 265;
   int max_dir_list       = 512;
   int max_guam_buff       = 3000;
   int max_ecb_count = 16; // RPI 393
   /* 
    * shared global variables
    */
    tz390 tz390 = null;
    pz390 pz390 = null;
    boolean pz390_running = false;
    String ez390_pgm = null; // saved by link for gz390 title
    int ez390_rc = 0;
    int ez390_errors = 0;
    String load_file_name = null;
    RandomAccessFile z390_file = null;
    File log_file = null;
    BufferedWriter log_file_buff = null;
    String  opt_parms = " ";
    boolean opt_ok = false;
    boolean exit_request = false;
    boolean system_abend = true;
    boolean user_abend   = false;
    boolean svc_abend_type   = system_abend;
    boolean svc_req_dump     = false; // request dump
    long stimer_exit_time = 0;  // set by STIMER for update+monitor to check
    int  stimer_exit_addr = 0;  // exit when STIMER tod reached
    boolean stimer_exit_request = false; // set when time expired
    boolean stimer_exit_running = false; // set while in stimer exit
    int  stimer_save_r13 = 0; // save r13 during exit
    int  stimer_save_r14 = 0; // save r14 during exit
    int  stimer_save_r15 = 0; // save r15 during exit
    int  stimer_save_psw = 0; // save psw at time of exit
    /*
     * gz390 graphical user access method variables
     */
    gz390 gz390 = null;
    int guam_major = 0;  // major function from r0+2
    int guam_minor = 0;  // minor function from r0+3
    int guam_args  = 0;  // arg list addr from r1
    int guam_view_mcs    = 1;
    int guam_view_tn3270 = 2;
    int guam_view_graph  = 3;
    int guam_view  = 0;  // 1=MCS, 2=TN3270, 3=graphics
    int guam_x     = 0;  // x pixels (rows for set screen view)
    int guam_y     = 0;  // y pixels (cols for set screen view)
    int guam_x2    = 0;
    int guam_y2    = 0;
    int guam_color = 0;
    int guam_width = 0;
    int guam_height= 0;
    int guam_row = 0;
    int guam_col = 0;
    int guam_bg_rgb = 0;
    int guam_text_rgb = 0;
    int guam_lfield = 0;
    int guam_cursor_type = 0;
    int guam_font      = 0;
    int guam_abuff     = 0; // addr buff
    int guam_lbuff     = 0; // length buff
    int guam_key_amod  = 0; // addr key mod
    int guam_key_achar = 0; // addr key char
    int guam_key       = 0; // mod and char from keyboard
    int[] guam_mouse   = null; // guam_mouse_read returns x,y,left,right
    int guam_left      = 0;    // GUAM mouse left button addr (1=pressed)
    int guam_right     = 0;    // GUAM_mouse right button addr (1 = pressed)
    boolean guam_wait  = true; // wait for input
    /*
     * global tget and tput data
     */
	int tpg_flags      = 0;    // TGET/TPUT options from high byte R1
	int tpg_op_mask    = 0x80; // 1=TGET, 0=TPUT
	int tpg_op_tget    = 0x80;
	int tpg_op_tput    = 0x00;
	int tpg_wait_mask  = 0x10; // 1=NOWAIT, 0=WAIT
	int tpg_wait       = 0x00;
	int tpg_nowait     = 0x10;
	int tpg_type_mask  = 0x03; // 00=EDIT 01=ASIS 10=CONTROL 11=FULLSCR
    int tpg_type_edit  = 0x00;
    int tpg_type_asis  = 0x01;
    int tpg_type_control = 0x02;
    int tpg_type_fullscr = 0x03;
    byte[] tget_byte = null;
    ByteBuffer tget_buff = null;
    byte[] tput_byte = new byte[max_guam_buff];
    ByteBuffer tput_buff = ByteBuffer.wrap(tput_byte,0,max_guam_buff);
    String guam_text = null;
    /*
     * Monitor variables
     */   
        int     ins_count = 0;  
        int     io_count  = 0;
        Timer   monitor_timer = null;
        long    monitor_cmd_time_total = 0;
	    long    monitor_last_time = 0;
        long    monitor_next_time = 0;
        long    monitor_cur_interval = 0;
        int     monitor_last_ins_count = 0;
        int     monitor_next_ins_count = 0;
        int     monitor_last_io_count = 0;
        int     monitor_next_io_count = 0;
        long    monitor_cur_ins  = 0;
        long    monitor_cur_int  = 0;
        long    monitor_cur_rate = 0;
        boolean monitor_last_cmd_mode = false;
        boolean wtor_reply_pending = false;
        int wto_fld = 0;
        int wto_len = 0;
        String  wtor_reply_string = null;
        int wtor_reply_addr = 0;
        int wtor_reply_len  = 0;
        int wtor_ecb_addr   = -1;
        int ecb_waiting = 0x80000000;
        int ecb_posted  = 0x40000000;
    /*
     *  svc wait variables 
     */
        int wait_count = 0;  // 0 for ECB= else ECBLIST wait count
        int wait_addr  = 0;  // ECB= or ECBLIST= addr
        boolean wait_retry = false; // do not refretch on retry after stimer exit
    /*
     * cmd processor variables
     */
    Process[]           cmd_proc          = (Process[])Array.newInstance(Process.class,max_cmd_proc);
    Thread[]            cmd_proc_thread   = (Thread[])Array.newInstance(Thread.class,max_cmd_proc);
    Thread[]            cmd_error_thread  = (Thread[])Array.newInstance(Thread.class,max_cmd_proc);
    Thread[]            cmd_output_thread = (Thread[])Array.newInstance(Thread.class,max_cmd_proc);
    InputStreamReader[] cmd_error_reader  = (InputStreamReader[])Array.newInstance(InputStreamReader.class,max_cmd_proc);
    InputStreamReader[] cmd_output_reader = (InputStreamReader[])Array.newInstance(InputStreamReader.class,max_cmd_proc);
    OutputStream[]      cmd_input_writer  = (OutputStream[])Array.newInstance(OutputStream.class,max_cmd_proc);
    LinkedList<String>[] cmd_output_queue  = (LinkedList<String>[])Array.newInstance(LinkedList.class,max_cmd_proc);
    boolean[]           cmd_proc_running  = (boolean[])Array.newInstance(boolean.class,max_cmd_proc);
    int[]               cmd_proc_rc       = (int[])Array.newInstance(int.class,max_cmd_proc);
    int[]               cmd_proc_io       = (int[])Array.newInstance(int.class,max_cmd_proc);
    long[]              cmd_proc_start_time = (long[])Array.newInstance(long.class,max_cmd_proc); // RPI 545
    String[]            cmd_error_msg     = new String[max_cmd_proc];
	String[]            cmd_output_msg    = new String[max_cmd_proc];
	int tot_cmd = 0; //max cmd processes started;
	int cmd_io_total = 0;
    /*
     * test option interactive debug variables
     */
    String  test_file_name = null;
    boolean test_cmd_abort = false;
    int     test_loop_count = 0;
    BufferedReader test_cmd_file = null;
    String  test_cmd = null;
    Pattern test_pattern = null;
    Matcher test_match   = null;
    String  test_token = null;
    char    test_bias = ' ';
    int     test_token_len = 0;
    char    test_opcode = 0;
    int test_addr = 0;
    int test_next_addr = 0;
    String test_sdt = null;
    byte test_addr_type = 0;
    byte test_addr_mem = 0;
    byte test_addr_reg = 1;
    byte test_compare = 0;
    int test_base_addr = -1;
    int test_mem_loc = 0;
    int test_mem_len = 0;
    int test_reg_loc = 0;
    long test_reg_sdt = 0;
    byte[]  test_mem_sdt  = null;
    /*
     * test reg and mem break on change variables
     */
    boolean test_break_addr_mode = false;
    boolean test_break_reg_mode  = false;
    boolean test_break_mem_mode  = false;
    boolean test_break_op_mode   = false;
    int     tot_test_break_addr  = 0;
    int     max_break_addr = 100;
    int[]   test_break_addr      = new int[max_break_addr];
    int     test_break_op_ins    = 0;
    String  test_break_addr_cmd  = null;
    String  test_break_reg_cmd   = null;
    String  test_break_mem_cmd   = null;
    String  test_break_op_cmd    = null;
    boolean test_break_reg       = false;
    boolean test_break_mem       = false;
    int     test_break_reg_loc   = 0;
    int     test_break_reg_compare = 0;
    long    test_break_reg_val   = 0;
    long    test_break_reg_sdt   = 0;
    int     test_break_mem_loc   = 0;
    byte    test_break_mem_byte  = 0;
    int     test_break_mem_compare = 0;
    int     test_break_mem_equal = 0;
    byte[]  test_break_mem_sdt   = null;
    int     test_break_op1       = 0;
    int     test_break_op2       = 0;
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
    int  cur_date_year  = 0;
    int  cur_date_month = 0;
    int  cur_date_day   = 0;
    int  tod_hour  = 0;
    int  tod_min   = 0;
    int  tod_sec   = 0;
    int  tod_msec  = 0;  // 0-999 fraction of sec
    long time_mil   = 0;  // milli-seconds 
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
    boolean ez390_startup = true;
   /*
    * 390 load module loader variables
    *
    * 16 byte header with 4 fields as follows
    * offset 0 - 4 character format version
    * offset 4 - full word length of code
    * offset 4 - full word entry offset
    * offset 4 - full word count of rlds 
    */ 
    char[] load_code_ver  = new char[4];
    char[] z390_flags     = new char[4];
    int z390_flags_amode31 = 0;
    int z390_flags_rmode31 = 1;
    String load_dsn       = null;
    int    load_dsn_addr  = 0;
    String load_pgm_dir   = null;
    String load_pgm_name  = null;
    String load_pgm_type  = null;
    int    load_code_load = 0;
    int    load_code_len  = 0;
    int    load_code_ent  = 0;
    int    load_code_rlds = 0;
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
     * convert to display variables
     */
    int        ctd_display_len = 45; // CTD and CFD display field
    float      ctd_e;
    double     ctd_d;
 	byte[]     ctd_byte = new byte[16];
	BigInteger ctd_bi;
	BigDecimal ctd_bd;
	String     ctd_text;
    float      cfd_e;
    double     cfd_d;
 	byte[]     cfd_byte;
	BigInteger cfd_bi;
	BigDecimal cfd_bd;
	String     cfd_text;
    /*
     * tcpio server port global data
     * 
     * Note thread for each open port:
     *   1.  Waits for connection from client
     *   2.  Start connection thread to process
     *       messages and send responses.
     *   3.  Return to wait for next connection
     * 
     */
    final Lock      lock            = new ReentrantLock();
    final Condition lock_condition  = lock.newCondition();
	boolean tcpio_server_running = false;  // true if any tcpio server ports open (required for server threads to run)
	int     tcpio_conn_ready_count   = 0;        // count of conn msgs ready
	int max_tcp_server_port   = 10; // max server ports open
	int cur_tcp_server_index   = 0;  // current port
	int[]          tcp_server_port   = new int[max_tcp_server_port];
	Thread[]       tcp_server_thread = new Thread[max_tcp_server_port];  // RPI 554
	int[]          tcp_server_conn_index = new int[max_tcp_server_port];     // RPI 554 index to allocated conn for next connection
	ServerSocket[] tcp_server_socket = new ServerSocket[max_tcp_server_port];
	String[]       tcp_server_host_text = new String[max_tcp_server_port];
    InetAddress[]  tcp_server_host_ip   = new InetAddress[max_tcp_server_port];
	/*
     * tcp server socket connection data
     * 
     * Note thread for each active connection 
     * started by server port thread processes
     * receive messages and send messages to client.
     */
	int               max_tcp_conn = 10; // max connections to server ports
	int               cur_tcp_conn = 0;  // current thread
	Thread[]          tcp_conn_thread         = new Thread[max_tcp_conn];  // RPI 554
    Socket[]          tcp_conn_socket = new Socket[max_tcp_conn];
    int[]             tcp_conn_server_port  = new int[max_tcp_conn];
    int[]             tcp_conn_server_index = new int[max_tcp_conn];
    boolean[]         tcp_conn_msg_ready    = new boolean[max_tcp_conn]; // 1 or more bytes ready for receiving
    boolean[]         tcp_conn_read         = new boolean[max_tcp_conn];
    byte[]            tcp_conn_byte         = new byte[max_tcp_conn];
    DataInputStream[] tcp_conn_input        = new DataInputStream[max_tcp_conn];
    PrintStream[]     tcp_conn_output       = new PrintStream[max_tcp_conn];
    /*
     * tcpio client global data
     */
    int           max_tcp_client_port   = 10; // max client ports open
    int           cur_tcp_client_index   = 0;
    int[]         tcp_client_port       = new int[max_tcp_client_port];
	Socket[]      tcp_client_socket     = new Socket[max_tcp_client_port];
    String[]      tcp_client_host_text  = new String[max_tcp_client_port];
    InetAddress[] tcp_client_host_ip    = new InetAddress[max_tcp_client_port];
    DataInputStream[] tcp_client_input  = new DataInputStream[max_tcp_client_port];
    PrintStream[]     tcp_client_output = new PrintStream[max_tcp_client_port];
    int    tcpio_op   = 0;       // r0 low = opcode 1-5
    int    tcpio_flags= 0;       // r0 high= bit 15 = NOWAIT for receive
	boolean tcpio_wait = false;
	int    tcpio_conn = 0;       // r2 connection id for send/receive (-1 = any)
	int    tcpio_amsg = 0;       // r14 msg start address
	int    tcpio_lmsg = 0;       // r15 message length (max for receive)
	int    tcpio_lmax = 1000000; // max lmsg set by send/recv
	int    tcpio_lmin = 1;       // min lmsg
	int    tcpio_host_addr   = 0;
	String tcpio_host_text   = null;
	InetAddress tcpio_host_ip = null;
	int    tcpio_port        = 0;
	int    tot_tcpio_oper    = 0;
    int    tot_tcpio_openc   = 0;
    int    tot_tcpio_opens   = 0;
    int    tot_tcpio_closec  = 0;
    int    tot_tcpio_closes  = 0;
    int    tot_tcpio_send    = 0;
    int    tot_tcpio_recv    = 0;
	/*
     * DCB sequential and random file I/O tables
     */
     int    tot_tiot_files = 0;
     int    cur_tiot_index = 0;
     int    cur_decb_addr  = 0;
     int    cur_ecb        = 0;
     int decb_ecb   = 0;
     int decb_type  = 4;
     int decb_dcb   = 8;
     int decb_area  = 12;
     long   cur_rba        = 0;
     int    cur_dcb_addr   = 0;     // mem offset to dcb from r1
     int    cur_dcbe_addr  = 0;     // dcb extension
     String cur_dcb_ddnam = null;  // ascii ddname from dcb ebcdic
     String cur_dcb_file_name = null;
     int    cur_dcb_oflgs  = 0;
     int    cur_dcb_synad  = 0;
     int    cur_dcb_eodad  = 0;
     int    cur_dcb_macrf  = 0;
     int    cur_dcb_recfm  = 0;
     int    cur_dcb_area   = 0;
     int    cur_dcb_lrecl_f  = 0;
     int    cur_dcb_blksi_f  = 0;
     int    cur_vrec_lrecl  = 0;
     String cur_rec_text  = null;
     int    cur_rec_len   = 0;
     int dcb_dsorg  = 0x1a;
     int dcb_dsorg_ps = 0x40; // physical sequential (GM or PM) 
     int dcb_dsorg_da = 0x20; // direct access(R/W)
     int dcb_iobad  = 0x1c;      // has tiot_index +1 while open else 0
     int dcb_eodad  = 0x20;      // end of data exit
     int dcb_recfm  = 0x24;      // record format
     int dcb_recfm_f = 0x80;     // fixed
     int dcb_recfm_v = 0x40;     // variable
     int dcb_recfm_fb = 0x90;    // fixed blocked
     int dcb_recfm_vb = 0x50;    // variable blocked
     int dcb_recfm_ft = 0xa0;    // fixed to/from ascii text
     int dcb_recfm_vt = 0x60;    // variable to/from ascii text
     int dcb_ddnam  = 0x28;      // ddname
     int dcb_id     = 0x00;      // id = EBCDIC or ASCII C'DCB1' RPI88
     int dcb_oflgs  = 0x30;      // open flags
     int dcb_oflgs_open = 0x80;  // file open
     int dcb_oflgs_gm   = 0x40;  // read allowed
     int dcb_oflgs_pm   = 0x20;  // write allowed
     int dcb_oflgs_rw   = 0x60;  // read and write allowed
     int dcb_macrf  = 0x32;      // macro access type
     int dcb_macrf_gm = 0x5000;  // get move
     int dcb_macrf_pm = 0x0050;  // put move
     int dcb_macrf_rw = 0x2020;  // read/write random
     int dcb_synad = 0x38;       // synchronous error exit
     int dcb_blksi_f = 0x3c;       // blocksize RPI 587 32 bits
     int dcb_lrecl_f = 0x50;       // record length RPI 587 32 bits
     int dcb_rec   = 0x58;       // record area
     int dcb_dsnam = 0x5c;       // ascii file spec override ddname
     int dcb_dcbe  = 0x60; // dcbe extention for eodad and synad
     int dcbe_eodad = 0;  
     int dcbe_synad = 4;
     boolean dcb_synad_recur = false; // RPI 377
     boolean[]          tiot_dcb_open   = (boolean[])Array.newInstance(boolean.class,max_tiot_files);
     String[]           tiot_ddnam      = new String[max_tiot_files];
     String[]           tiot_dsn        = new String[max_tiot_files];
     int[]              tiot_dcb_addr   = (int[])Array.newInstance(int.class,max_tiot_files);
     int[]              tiot_vrec_blksi = (int[])Array.newInstance(int.class,max_tiot_files);
     long[]             tiot_cur_rba    = (long[])Array.newInstance(long.class,max_tiot_files);
     long[]             tiot_eof_rba    = (long[])Array.newInstance(long.class,max_tiot_files);
     RandomAccessFile[] tiot_file       = (RandomAccessFile[])Array.newInstance(RandomAccessFile.class,max_tiot_files);

        int ascii_lf = 10;
        int ascii_cr = 13;
        int ascii_period =  (int)'.';
        int ascii_space = (int) ' ';
        int ebcdic_period = 75;
        int ebcdic_space = 64;
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
  int opt_getmain_amode31 = 0x02;
  int opt_getmain_cond    = 0x01;
  /*
   * end of global ez390 class data and start of procs
   */
public void svc(int svc_id){
	/*
	 * execute supervisor call using
	 * mem and regs for data transfer
	 */
	switch (svc_id){
	case 0x01:  // WAIT
		svc_wait();
		break;
	case 0x02:  // POST 
		svc_post();
		break;
	case 0x03:  // EXIT  
		svc_exit();
		break;
	case 0x04: // GETMAIN R0_BIT0=AMODE31, R0_BIT1=COND, R1=length
		svc_getmain();
		break;
	case 0x05: // FREEMAIN R0=LEN, R1=ADDR RPI 244
		svc_freemain();
		break;
	case 0x06:  // LINK  R15=PGMID, R1=PARMS
        svc_link();
		break;
	case 0x07:  // XCTL  R15=PGMID, R1=PARMS
		svc_xctl();
		break;
	case 0x08:  // LOAD  R15=PGMID
		svc_load();
		break;
	case 0x09: // DELETE R0=A(NAME)
		svc_delete();
		break;
	case 0x0b: // TIME R0_LH=DATETYPE, R0_LL=TIMETYPE, R1=ADDR
		svc_time();
		break;
	case 0x0d: // ABEND  R1=abend code 
		pz390.psw_pic = pz390.reg.getInt(pz390.r1);
		if (pz390.psw_pic < 0){
			svc_req_dump = true; // req dump if abend issued
		} else {                 // rather than STAE or SPIE exit
			svc_req_dump = false;
		}
        svc_abend(pz390.psw_pic & 0xfff,user_abend,svc_req_dump);
	    break;
	case 0x12:  // BLDL FIND PGMS IN SYS390 
		svc_bldl();
		break;
	case 0x13:  // OPEN DCB R0=(x'40' input,x'20' output) R1=DCB
		svc_open();
		break;
	case 0x14:  // CLOSE DCB R1=DCB
		svc_close();
		break;
	case 0x22:  // SVC 34 R1=COMMAND
		svc_cmd();
		break;
	case 0x23:  // WTO R1=ADDR 
		/*
		 * WTO FIELD
 	     *   0 2 length 
	     *   2 2 mcs flags
	     *   4 (length-4) msg 
		 */
		wto_fld = pz390.reg.getInt(pz390.r1) & pz390.psw_amode;
        wto_len = pz390.mem.getShort(wto_fld);
		wto_msg("",wto_fld+4,wto_len-4);  //RPI190 remove "WTO  MSG"
		break;
	case 0x2e:  //TTIMER r0=function, r1=mic addr
		svc_ttimer();
		break;
	case 0x2f:  //STIMER r0=flags, r1=rx storage arg.
		svc_stimer();
		break;
	case 0x33:  // SNAP r0hh=flags,r0ll=id,r14-15 storage
        svc_snap();
        break;
	case 0x3c:
	    svc_estae(); // set abend exit R1=addr
	    break;
	case 0x67:
		svc_xlate();  // translate ascii/EBCDIC
		break;
	case 0x6d:
		svc_espie();  // set program check exit R0=types, R1=addr
		break;
	case 84:  // GUAM GUI application window I/O
		svc_guam();
		break;
	case 93:  // TGET/TPUT TN3290 data stream for GUAM GUI
		svc_tget_tput();
		break;
	case 124: // x'7C' TCPIO tcp/ip sockets I/O
		svc_tcpio();
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
	case 160: // wtor 
		svc_wtor();
		break;
	case 170: // ctd - convert to display format r1=a(type,in,out)
		svc_ctd(); // RPI 360
		break;
	case 171: // cfd - convert from display format r1=a(type,in,out)
		svc_cfd(); // RPI 370
		break;
	default:
		abort_error(23,"undefined svc - " + svc_id);
		break;
	}
}
public synchronized void put_log(String msg) {
   	/*
   	 * Write message to z390_log_text and/or con
   	 * if running standalone
   	 * 
   	 */
	put_log_line(msg);
	if  (z390_log_text != null){
		z390_log_text.append(msg + "\n");
   	}
    if (tz390.opt_trace){ // RPI 490
    	tz390.put_trace("EZ390I " + msg);
    } else if (tz390.opt_test // RPI 505
    		|| tz390.opt_con 
    		|| tz390.z390_abort 
    		|| ez390_startup){
		System.out.println(msg);
	}
}
private void put_log_line(String msg){
	   /*
	    * put line to listing file
	    */
	   	   if (tz390.opt_list && log_file_buff != null){
	   	      try {
	   	    	  tz390.systerm_io++;
	   	          log_file_buff.write(msg + tz390.newline); // RPI 500
	                if (log_file.length() > tz390.max_file_size){
	                	abort_error(107,"maximum log file size exceeded");
	                }
	   	      } catch (Exception e){
	   	          abort_error(6,"I/O error on log file write");
	   	      }
	   	   }
	   }
public void log_error(int error,String msg){
	/*
	 * issue error msg to log with prefix and
	 * inc error total
	 * 1.  supress if not gen_obj and not trace
	 */
	  String error_msg = "EZ390E error " + tz390.right_justify("" + error,3) + " " + msg;
      put_log(error_msg);
      tz390.put_systerm(error_msg);
	  ez390_errors++;
	  if (tz390.max_errors != 0 && ez390_errors > tz390.max_errors){
	  	 abort_error(5,"max errors exceeded");	 
	  }
}
public void abort_error(int error,String msg){
	/*
	 * issue error msg to log with prefix and
	 * inc error total
	 */
 	  if (tz390.z390_abort){
		msg = "EZ390E shutdown for " + msg;
		tz390.put_systerm(msg);
		try { // rpi 560
			put_stats();
			close_files();
		} catch (Exception e){
			System.out.println("EZ390E CLOSE FILES FAILED");
		}
		System.exit(16);
	  }
 	  tz390.z390_abort = true;
	  ez390_errors++;
	  if (ez390_rc == 0){
	  	 ez390_rc = 16;
	  }
	  stimer_exit_addr = 0; // RPI 471 cancel stimer exit
	  String error_msg = "EZ390E error " + error + " " + msg;
	  put_log(error_msg);
	  tz390.put_systerm(error_msg);
      exit_ez390();
}
public void exit_ez390(){
	/*
	 * display total errors
	 * close files and exit
	 */
	  int r15_rc = pz390.reg.getInt(pz390.r15); //RPI39
	  if (r15_rc > ez390_rc){
		  ez390_rc = r15_rc;
	  }
	  if  (ez390_errors > 0 || tz390.z390_abort){
		  ez390_rc = 16;
      }
	  try {
		  close_z390_guam();
	  } catch (Exception e){
		  System.out.println("EZ390E GUAM SHUTDOWN FAILED");
	  } 
	  try {
		  close_cmd();  //RPI76
	  } catch (Exception e){
		  System.out.println("EZ390E CMDPROC SHUTDOWN FAILED");
	  }
	  try {
		  tcpio_close_ports();
	  } catch (Exception e){
		  System.out.println("EZ390E TCPIO SHUTDOWN FAILED");
	  }
	  try {
		  put_stats();
		  close_files();
	  } catch (Exception e){
		  System.out.println("EZ390E CLOSE FILES FAILED");
	  }  
      System.exit(ez390_rc); //RPI39
}
private synchronized void close_z390_guam(){  // RPI 397
	/*
	 * if exit request, send shutdown request
	 * to z390 GUI via the sysout queue
	 */
    if (exit_request){
  	  tz390.opt_trace = false;     //RPI35
  	  tz390.z390_abort = true;     //RPI208
  	  // send exit request to z390 GUI process
  	  System.out.println("exit_request");
    }
}
private void put_stats(){
	/*
	 * display statistics as comments at end of bal
	 */
	boolean save_opt_con = tz390.opt_con; // RPI 453
	if (tz390.opt_list){
		tz390.opt_con = false;
	}
	if (tz390.opt_stats){
		put_log("EZ390I Stats total instructions    = " + tz390.systerm_ins);
		if (tot_tcpio_oper > 0){ // RPI 566
			put_log("EZ390I stats TCPIO open client     = " + tot_tcpio_openc);
			put_log("EZ390I stats TCPIO open server     = " + tot_tcpio_opens);
			put_log("EZ390I stats TCPIO close_client    = " + tot_tcpio_closec);
			put_log("EZ390I stats TCPIO close server    = " + tot_tcpio_closes);
			put_log("EZ390I stats TCPIO send message    = " + tot_tcpio_send);
			put_log("EZ390I stats TCPIO receive message = " + tot_tcpio_recv);
		}
		if (tz390.opt_trace){
			tz390.put_trace("EZ390I Stats Keys                  = " + tz390.tot_key);
			tz390.put_trace("EZ390I Stats Key searches          = " + tz390.tot_key_search);
			if (tz390.tot_key_search > 0){
				tz390.avg_key_comp = tz390.tot_key_comp/tz390.tot_key_search;
			}
			tz390.put_trace("EZ390I Stats Key avg comps         = " + tz390.avg_key_comp);
			tz390.put_trace("EZ390I Stats Key max comps         = " + tz390.max_key_comp);
		}
		if (tz390.opt_timing){ // display instr rate
		   	pz390.cur_date = new Date();
		   	put_log("EZ390I Stats current date " + cur_date_MMddyy.format(pz390.cur_date)
		   			+ " time " + cur_tod_hhmmss.format(pz390.cur_date)); // RPI 209
			tod_end_pgm = pz390.cur_date.getTime();
			tot_sec = (tod_end_pgm - tod_start_pgm)/1000;
			put_log("EZ390I Stats total seconds         = " + tot_sec);
			long ins_rate = ((long) tz390.systerm_ins)*1000/(tod_end_pgm - tod_start_pgm + 1);
			put_log("EZ390I Stats instructions/sec      = " + ins_rate);
		}
	}
	tz390.opt_con = save_opt_con; // rpi 453
	put_log("EZ390I total errors         = " + ez390_errors);
	put_log("EZ390I return code(" + tz390.left_justify(tz390.pgm_name,8) + ")= " + ez390_rc); // RPI 312
}
private void close_files(){
	/*
	 * close log, err, tre
	 */
	  if (log_file != null && log_file.isFile()){
	  	  try {
	  	  	  log_file_buff.close();
	  	  } catch (IOException e){
	  	  	  abort_error(3,"I/O error on log file close - " + e.toString());
	  	  }
	  }
	  tz390.close_systerm(ez390_rc);
	  tz390.close_trace_file();
}
private void close_cmd(){
	/*
	 * cancel all active cmd processes
	 */
	int cmd_id = 0;
	while (cmd_id < max_cmd_proc){
		if (cmd_proc_running[cmd_id]){  // RPI 592
			cmd_cancel(cmd_id);
		}
		cmd_id++;
	}
}
public void init_time(){
	/*
	 * init pz390.cur_date and calendar with
	 * current time and date or force
	 * fixed time if NOTIMING option set.
	 * Notes:
	 *   1.  This NOTTIMING option is used in
	 *       regression testing timing functions
	 *       for repeatable results.
	 */
    cur_date_cal = new GregorianCalendar(1900,0,1);
    pz390.ibm_mil = - cur_date_cal.getTime().getTime(); // millisecons from 1900 to 1970
	if (tz390.opt_timing){ // measure and display timing
	    pz390.cur_date = new Date();
	    cur_date_year  = Integer.valueOf(cur_date_yyyy.format(pz390.cur_date));
	    cur_date_month = Integer.valueOf(cur_date_MM.format(pz390.cur_date));
	    cur_date_day   = Integer.valueOf(cur_date_dd.format(pz390.cur_date));
	    tod_hour  = Integer.valueOf(cur_date_HH.format(pz390.cur_date));
	    tod_min   = Integer.valueOf(cur_date_mm.format(pz390.cur_date));
	    tod_sec   = Integer.valueOf(cur_date_ss.format(pz390.cur_date));
	    tod_msec  = Integer.valueOf(cur_date_ms.format(pz390.cur_date));
	    cur_date_cal = new GregorianCalendar(cur_date_year,cur_date_month-1,cur_date_day);
	    tod_start_day = cur_date_cal.getTime().getTime();
	    tod_start_pgm = pz390.cur_date.getTime();
        if (tz390.opt_time){
        	tod_time_limit = tz390.max_time_seconds *1000 + tod_start_pgm;
        }
	} else {
	    cur_date_cal = new GregorianCalendar(2005,0,2,22,33,44);
	    pz390.cur_date = new Date(cur_date_cal.getTime().getTime()+567);
	    cur_date_year  = Integer.valueOf(cur_date_yyyy.format(pz390.cur_date));
	    cur_date_month = Integer.valueOf(cur_date_MM.format(pz390.cur_date));
	    cur_date_day   = Integer.valueOf(cur_date_dd.format(pz390.cur_date));
	    tod_hour  = Integer.valueOf(cur_date_HH.format(pz390.cur_date));
	    tod_min   = Integer.valueOf(cur_date_mm.format(pz390.cur_date));
	    tod_sec   = Integer.valueOf(cur_date_ss.format(pz390.cur_date));
	    tod_msec  = Integer.valueOf(cur_date_ms.format(pz390.cur_date));
	    cur_date_cal = new GregorianCalendar(cur_date_year,cur_date_month-1,cur_date_day);
	    tod_start_day = cur_date_cal.getTime().getTime();
	    tod_start_pgm = pz390.cur_date.getTime();
	}
}
public void init_test(){
	/*
	 * 1. init test regular expression parser
	 * 2. init optional test=ddname file for batch input 
	 *    else init test_cmd_file which is also used
	 *    for wtor replies when not in GUAM GUI mode
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
		  +	"|([cC][']([^']|(['][']))*['])"      // sdt ebcdic
		  +	"|([cC][\"]([^\"]|([\"][\"]))*[\"])" // sdt ascii
   		  +	"|([fF]['][-]*[0-9]+['])"       // sdt
   		  +	"|([hH]['][-]*[0-9]+['])"       // sdt
   		  +	"|([xX]['][0-9a-fA-F]+['])"     // sdt
		  + "|([0-9]+[rR][%?]*)"            // reg addr
		  + "|([0-9a-fA-F]+[\\.])"          // hex. addr
		  + "|([0-9]+)"                     // dec
		  + "|([a-zA-Z]+)"                  // cmd or opcode
		  + "|([*+-])"                        // * addr or comment
		  + "|([=])|([!][=])|([>][=]*)|([<][=]*)"  // set break compare operator
   	    );
   	} catch (Exception e){
   		  abort_error(56,"test error in expression pattern - " + e.toString());
   	}
	if (tz390.test_ddname != null){
		 test_file_name = get_ascii_env_var_string(tz390.test_ddname);
         if (tz390.z390_os_type == tz390.z390_os_linux){ // RPI 532 file separator fix
        	test_file_name = test_file_name.replace('\\','/');
         }
         try {
        	 test_cmd_file = new BufferedReader(new FileReader(test_file_name));
         } catch (Exception e){
        	 abort_error(57,"test input file for ddname " + tz390.test_ddname + " not found - " + test_file_name);
         }
	} else {
		test_cmd_file = new BufferedReader (new InputStreamReader(System.in));
	}
}
public void open_files(){
	/*
	 * 1.  Set trace file for TRACE and TRACEALL
	 * 2.  Open 390 and lst files
	 */
	    tz390.trace_file_name = tz390.dir_trc + tz390.pgm_name + tz390.tre_type;
       	if (tz390.opt_list){
            log_file = new File(tz390.dir_log + tz390.pgm_name + tz390.log_type);
         	try {
       	       log_file_buff = new BufferedWriter(new FileWriter(log_file));
       	    } catch (IOException e){
       		   abort_error(9,"I/O error on log file open - " + e.toString());
       	    }
       	}
}
 private void svc_exit(){
 	/*
 	 * 1.  If stimer_exit_running then restore
 	 *     r13-r15 and exit to saved psw
 	 * 2.  If stae exit running, restore
 	 *     psw and regs from zcvt_stae.
 	 * 3.  If spie exit running, restore
 	 *     psw and regs from zcvt_epie    
 	 * 4.  exit to prev link return address
 	 *     or exit ez390 if none.
 	 */
	if (stimer_exit_running){
		stimer_exit_running = false;
		pz390.reg.putInt(pz390.r13,stimer_save_r13);
		pz390.reg.putInt(pz390.r14,stimer_save_r14);
		pz390.reg.putInt(pz390.r15,stimer_save_r15);
		pz390.set_psw_loc(stimer_save_psw); 
	    if (tz390.opt_trace){
	    	tz390.put_trace("TRACE STIMER EXIT ENDING");
	    }
		return;
	} else if (pz390.estae_exit_running){
		pz390.estae_last_ins = tz390.systerm_ins;
		pz390.estae_exit_running = false;
		pz390.reg.position(0);
		pz390.reg.put(pz390.mem_byte,pz390.esta_gpr,128);
		pz390.set_psw_loc(pz390.mem.getInt(pz390.esta_psw+4));
	    if (tz390.opt_trace){
	    	tz390.put_trace("ESTAE EXIT ENDING");
	    }
		return;
	} else if (pz390.espie_exit_running){
		pz390.espie_last_ins = tz390.systerm_ins;
		pz390.espie_exit_running = false;
		pz390.reg.position(0);
		pz390.reg.put(pz390.mem_byte,pz390.epie_gpr,128);
		pz390.set_psw_loc(pz390.mem.getInt(pz390.epie_psw+4));
	    if (tz390.opt_trace){
	    	tz390.put_trace("ESPIE EXIT ENDING");
	    }
		return;
	}
 	if (tot_link_stk == 1){
	    pz390.set_psw_check(pz390.psw_pic_exit);  // exit ez390
 	} else {
 		int link_ret = link_stk_ret[tot_link_stk - 1];
 		cur_cde      = link_stk_cde[tot_link_stk - 1];
 		tot_link_stk--;
 		delete_cur_cde();
 		pz390.set_psw_loc(link_ret); 
 		// exit to link caller in prior amode
 		if ((link_ret & pz390.psw_amode31_bit) != 0){
 			pz390.set_psw_amode(pz390.psw_amode31_bit);
 		} else {
 			pz390.set_psw_amode(pz390.psw_amode24_bit);
 		}
 	}
 }
private void svc_load(){
	/*
	 * load 390 load module into virtual memory
	 * 
	 * Input regs
	 *   1. r0  = 8 byte pgm name padded with spaces
	 *   2. r15 = 0 use SYS390 default path search
	 *   3. r15 = addr dsname if high bit off
	 *   4. r15 = addr ddname if high bit on
	 * Output regs:
	 *   1. r0  = address of 390 program entry
	 *            above or below line based on
	 *            lz390 rmode option and with 
	 *            high bit indicating amode
	 *            based on lz390 amode option.
	 *            If not 390 file, r0 = load address
	 *   2. r1  = length of 390 file loaded in doubleword 
	 *      count for OS compatiblity.  
	 *      Length in bytes if not 390 file.
	 *   3. r15 = return code 0 ok or 4 if notfound
     *
	 * Notes:
	 *   1.  Add CDE entry for new entry else if already
	 *       loaded, increment cde_use and return 
	 *       existing load address.
	 */
	load_dsn_addr = pz390.reg.getInt(pz390.r15);
	load_pgm_dir =  tz390.dir_390; // RPI 244
	load_pgm_type = tz390.pgm_type;
	if (load_dsn_addr == 0){
		load_pgm_name = get_ascii_string(pz390.reg.getInt(pz390.r0),8);
	} else {
		if (!get_load_dsn()){
			pz390.reg.putInt(pz390.r1,0);  // RPI102
            pz390.reg.putInt(pz390.r15,4);
            return;
		}
	}
	cur_cde = tz390.find_key_index('P',load_pgm_name.toUpperCase() + load_pgm_type); // RPI 499
	if (cur_cde != -1 && cde_loc[cur_cde] != 0){
		cde_use[cur_cde]++;
        svc_load_set_regs();
		return;
	}
	load_file_name = tz390.find_file_name(load_pgm_dir,load_pgm_name,load_pgm_type,tz390.dir_cur); 
	if (load_file_name == null){
		pz390.reg.putInt(pz390.r1,0); // RPI102
        pz390.reg.putInt(pz390.r15,4);
        return;
	}
    if (load_pgm_type.equals(tz390.z390_type)){
       	svc_load_390();
    } else {
       	svc_load_file();
    }
}
private void svc_load_set_regs(){
	/*
	 * set r0, r1, and r15 for load
	 */
	pz390.reg.putInt(pz390.r0,cde_loc[cur_cde]);
	if (cde_ent[cur_cde] != -1){
		pz390.reg.putInt(pz390.r1,cde_len[cur_cde] >>> 3); // RPI 102 390 double words
	} else {
		pz390.reg.putInt(pz390.r1,cde_len[cur_cde]); // RPI 102 non 390 byte count
	}
	pz390.reg.putInt(pz390.r15,0);
}
private void svc_load_390(){
	/*
	 * load 390 file, relocate, and 
	 * set r1 lenght in double words
	 */
  try {
    z390_file = new RandomAccessFile(load_file_name,"r");
    z390_file.seek(0);
    load_code_ver[0] = (char)z390_file.read();
    load_code_ver[1] = (char)z390_file.read();
    load_code_ver[2] = (char)z390_file.read();
    load_code_ver[3] = (char)z390_file.read();
    z390_flags[0]    = (char)z390_file.read();
    z390_flags[1]    = (char)z390_file.read();
    z390_flags[2]    = (char)z390_file.read();
    z390_flags[3]    = (char)z390_file.read();
    load_code_len    = z390_file.readInt();
    load_code_ent    = z390_file.readInt();
    load_code_rlds   = z390_file.readInt();
    tz390.systerm_io = tz390.systerm_io + 11;
    if (z390_flags[z390_flags_rmode31] == 'T'){
    	pz390.reg.putInt(pz390.r0,opt_getmain_amode31); // getmain above line
    }else {
        pz390.reg.putInt(pz390.r0,0); // getmain below line
    }
    pz390.reg.putInt(pz390.r1,load_code_len);
    svc_getmain();
    if (pz390.reg.getInt(pz390.r15) == 0){
       load_code_load = pz390.reg.getInt(pz390.r1); // RPI 542
	   tz390.systerm_io++;
       z390_file.read(pz390.mem_byte,load_code_load,load_code_len);
       load_code_ent = load_code_ent + load_code_load; 
    } else {
    	abort_error(15,"getmain for svc 8 load failed - PGM=" + tz390.pgm_name + "LEN=" + load_code_len);
    }
    svc_load_rlds();
    z390_file.close();
    if (z390_flags[z390_flags_amode31] == 'T'){
 	    load_code_ent  = load_code_ent  | pz390.psw_amode31_bit;
        load_code_load = load_code_load | pz390.psw_amode31_bit;
    }
    add_cde();
    svc_load_set_regs();
  } catch (Exception e){
	abort_error(18,"I/O error on 390 load module file - " + e.toString());
  }
}
private void svc_load_rlds(){
	/*
	 * read and apply rld records at end
	 * of 390 file.
	 */
	int rld_field = 0;
	try {
	    int cur_rld = 0;
	    while (cur_rld < load_code_rlds){
	    	tz390.systerm_io++;
	    	rld_loc = z390_file.readInt();
	    	tz390.systerm_io++;
	    	rld_len = z390_file.readByte();
	    	switch (rld_len){
	    	case 3: //RPI71
		    	pz390.mem.position(load_code_load + rld_loc);
		    	rld_field = pz390.mem.getInt();
	    		rld_field = (rld_field >>> 8) + load_code_load;
	    		pz390.mem.position(load_code_load + rld_loc);
	        	pz390.mem.putShort((short)(rld_field >>> 8));
	        	pz390.mem.put((byte)(rld_field & 0xff));
	        	break;
	    	case 4:
		    	pz390.mem.position(load_code_load + rld_loc);
		    	rld_field = pz390.mem.getInt();
	    		rld_field = rld_field + load_code_load;
	    		pz390.mem.position(load_code_load + rld_loc);
	        	pz390.mem.putInt(rld_field);
	        	break;	
	    	case 8: // RPI 270
	    		rld_loc = rld_loc + 4;
		    	pz390.mem.position(load_code_load + rld_loc);
		    	rld_field = pz390.mem.getInt();
	    		rld_field = rld_field + load_code_load;
	    		pz390.mem.position(load_code_load + rld_loc);
	        	pz390.mem.putInt(rld_field);
	        	break;	
	        }
	    	cur_rld++;
	    }
	} catch (Exception e){
		abort_error(86,"svc 8 I/O error reading RLD");
	}
}
private void svc_load_file(){
	/*
	 * 1. Load non 390 file
	 * 2. Set cde_pgm to file_name
	 * 3. Set cde_loc to load address
	 * 4. Set cde_len to length in bytes
	 * 5. Set cde_ent to -1
	 * 6. Set R0 to load address 
	 * 7. Set r1 to length in bytes
	 */
  try {
    z390_file = new RandomAccessFile(load_file_name,"r");
    if (z390_file.length() > pz390.tot_mem - pz390.tot_mem_alloc){
    	abort_error(85,"file size exceeds available memory ");
    }
    load_code_len = (int) z390_file.length();
    z390_file.seek(0); 
    if (pz390.psw_amode == pz390.psw_amode31){  // load file above line if amode31
    	pz390.reg.putInt(pz390.r0,opt_getmain_amode31); 
    } else {
    	pz390.reg.putInt(pz390.r0,0);  // load below line
    }
    pz390.reg.putInt(pz390.r1,load_code_len);
    svc_getmain();
    if (pz390.reg.getInt(pz390.r15) == 0){
       load_code_load = pz390.reg.getInt(pz390.r1);  // RPI 542
       load_code_ent  = -1; // no entry for files;
       tz390.systerm_io++;
       z390_file.read(pz390.mem_byte,load_code_load,load_code_len);
       if (pz390.psw_amode == pz390.psw_amode31){
           load_code_load = load_code_load | pz390.psw_amode31_bit;
       }
    } else {
    	abort_error(15,"getmain for svc 8 load failed - PGM=" + tz390.pgm_name + "LEN=" + load_code_len);
    }
    z390_file.close();
    add_cde();
    svc_load_set_regs();
  } catch (Exception e){
	abort_error(18,"I/O error on 390 load module file - " + e.toString());
  }
}
private void svc_xctl(){
	/*
	 * delete current module and then 
	 * load and then balr to 390 load module
	 * Input:
	 *   1. r0  = addr pgm name
	 *   2. r1  = user parms
	 * 	 2. r15 = 0 use SYS390 default path search
	 *   3. r15 = addr dsname if high bit off
	 *   4. r15 = addr ddname if high bit on
	 * Output:
	 *   1. r15 = user pgm return code if call ok
	 *   2. abend s106
	 */
	int user_parm = pz390.reg.getInt(pz390.r1); // RPI 596 RPI 598
	svc_load();
	if (pz390.reg.getInt(pz390.r15) != 0){
		if (pz390.tot_estae == 0){
			log_error(102,"xctl program not found - " + load_pgm_name);
		}
		pz390.set_psw_check(pz390.psw_pic_link_err); // link failed
		return;
	}
	int link_addr = pz390.reg.getInt(pz390.r0);
	if  (pz390.reg.getInt(pz390.r15) == 0){
		int save_new_cde = cur_cde;
		cur_cde = link_stk_cde[tot_link_stk-1]; // RPI 598
		delete_cur_cde();
		link_stk_cde[cur_link_stk] = save_new_cde;
		pz390.reg.putInt(pz390.r14,pz390.zcvt_exit);
		pz390.reg.putInt(pz390.r15,link_addr);
		pz390.reg.putInt(pz390.r1,user_parm); // RPI 596
		pz390.set_psw_loc(link_addr);
        if ((link_addr & pz390.psw_amode31_bit) != 0){
            pz390.set_psw_amode(pz390.psw_amode31_bit);
        } else {
            pz390.set_psw_amode(pz390.psw_amode24_bit);
        }
	}
}
private boolean get_load_dsn(){
	/*
	 * 1. Set following from ddname/dsname
	 *    load_pgm_dir (overrides dir_390 default) RPI 244
	 *    load_pgm_name
	 *    load_pgm_type
	 * 2. Return true if ok else false.
	 * Notes:
	 *   1.  If dir_addr high bit on, get user list
	 *       from 8 byte ddname env. var. at dir_addr
	 *       else get user list from dir_addr with
	 *       null delimited or double quote delimiter.
	 */
	load_pgm_type = tz390.z390_type;
	String dsn_source = "dsname";
 	if (load_dsn_addr < 0){
 		String ddname = get_ascii_string(load_dsn_addr & 0x7fffffff,8);
 		dsn_source = "ddname " + ddname;
 		load_dsn = get_ascii_env_var_string(ddname);
 		if (load_dsn == null || load_dsn.length() == 0){
 			log_error(82,"DDNAME=" + ddname + " not defined");
 			return false;
 		}
 	} else {
 		load_dsn = get_ascii_var_string(load_dsn_addr,max_dir_list);
 		if (load_dsn == null || load_dsn.length() == 0){
 			log_error(83,"DSNAME invalid field at " + tz390.get_hex(load_dsn_addr,8));
 			return false;
 		}
 	}
    if (tz390.z390_os_type == tz390.z390_os_linux){ // RPI 532 file separator fix
    	load_dsn = load_dsn.replace('\\','/');
    }
 	int index = load_dsn.indexOf(";");
 	if (index != -1){ // dsn is dir list vs file spec
        load_pgm_dir = load_dsn;
 		if (!get_eploc_pgm_name()){
        	return false;
        }
 	} else { // may be path with or without file name
 		File temp_file = new File(load_dsn);
 		if (temp_file.isFile()){
 			load_pgm_name = temp_file.getName();
 			index = load_pgm_name.indexOf(".");
 			if (index > 0){
 				load_pgm_type  = load_pgm_name.substring(index);
 				load_pgm_name = load_pgm_name.substring(0,index);
 			} else {
 				load_pgm_type = "";
 			}
 			load_pgm_dir = temp_file.getPath();
 			index = load_pgm_dir.lastIndexOf(File.separator);
 			if (index > 0){
 				load_pgm_dir = load_pgm_dir.substring(0,index);
 			}
 			return true;
 		} else if (temp_file.isDirectory()){
 			load_pgm_dir  = temp_file.getPath();
            if (!get_eploc_pgm_name()){
            	return false;
            }
 		} else {
 			log_error(81,"invalid " + dsn_source + "=" + load_dsn);
 			return false;
 		}
 	}
 	return true;
}
private boolean get_eploc_pgm_name(){
	/*
	 * set load_pgm_name from r0 else error
	 */
		int ep_loc = pz390.reg.getInt(pz390.r0) & pz390.psw_amode;
 		if (ep_loc != 0){
 			load_pgm_name = get_ascii_string(ep_loc,8);
 		    return true;
 		} else {
 			log_error(87,"no program or file name found");
 		    return false;
 		}
}
private void svc_delete(){
	/*
	 * delete loaded pgm/dsn from irtual memory
	 * if use count 0 after decrement.
	 * 
	 * Input regs
	 *   1. r0  = 8 byte pgm name padded with spaces
	 *   2. r15 = 0 use SYS390 default path search
	 *   3. r15 = addr dsname if high bit off
	 *   4. r15 = addr ddname if high bit on
	 * Output regs:
	 * Output regs:
	 *   1.  r15 = 0 if successfully deleted
	 *   2.  r15 = 4 if not found in memory
     *
	 * Notes:
	 */
	load_dsn_addr = pz390.reg.getInt(pz390.r15);
	load_pgm_dir = tz390.dir_390;
	load_pgm_type = tz390.z390_type;
	if (load_dsn_addr == 0){
		load_pgm_name = get_ascii_string(pz390.reg.getInt(pz390.r0),8);
	} else {
		if (!get_load_dsn()){
            pz390.reg.putInt(pz390.r15,4);
            return;
		}
	}
	cur_cde = tz390.find_key_index('P',load_pgm_name.toUpperCase() + load_pgm_type); // RPI 499
	if (cur_cde != -1 && delete_cur_cde()){
		pz390.reg.putInt(pz390.r15,0);
	} else {
		pz390.reg.putInt(pz390.r15,4);
	}
}
private boolean delete_cur_cde(){
	/*
	 * decrement use count for cur_cde 
	 * if use count 0
	 *    freemain memory
	 *    set cde_loc to 0 to release cde entry
	 *    return true
	 * else 
	 *    return false
	 */
	if (cur_cde != -1 && cde_loc[cur_cde] != 0){
		cde_use[cur_cde]--;
        if  (cde_use[cur_cde] < 1){
        	pz390.reg.putInt(pz390.r1,cde_loc[cur_cde]); // RPI 244
        	pz390.reg.putInt(pz390.r0,cde_len[cur_cde]); // RPI 244
        	svc_freemain();
        	cde_loc[cur_cde] = 0;
        }
		return true;
	} else {
		abort_error(88,"delete cde system error");
		return false;
	}
}
private void add_cde(){
	/*
	 * add new 390 load module to cde entry table
	 * and set usage to 1
	 */
	cur_cde = tz390.find_key_index('P',load_pgm_name.toUpperCase() + load_pgm_type); // RPI 499
	if (cur_cde == -1){
		if (tot_cde < max_cde_pgms){
			cur_cde = tot_cde;
			tot_cde++;
			if (!tz390.add_key_index(cur_cde)){
				abort_error(21,"key search table exceeded");
			}
		} else {
			abort_error(55,"CDE maximum 390 load modules exceeded");
            return;
		}
	}
	cde_name[cur_cde] = load_pgm_name + load_pgm_type;
	cde_use[cur_cde]  = 1;
	cde_len[cur_cde]  = load_code_len;
    cde_loc[cur_cde] = load_code_load;
    cde_ent[cur_cde] = load_code_ent;
}
public void svc_link(){
	/*
	 * load and then balr to 390 load module
	 * Input:
	 *   1. r0  = addr pgm name
	 *   2. r1  = user parms
	 * 	 2. r15 = 0 use SYS390 default path search
	 *   3. r15 = addr dsname if high bit off
	 *   4. r15 = addr ddname if high bit on
	 * Output:
	 *   1. r15 = user pgm return code if call ok
	 *   2. abend s106
	 *   3. On first load with option GUAM on,
	 *      the gz390 GUAM GUI window will be staretd
	 *      with default title using program name
	 */
	int save_r1 = pz390.reg.getInt(pz390.r1);
	svc_load();
	pz390.reg.putInt(pz390.r1,save_r1);
	if (pz390.reg.getInt(pz390.r15) != 0){
		if (pz390.tot_estae == 0){
			log_error(103,"LINK program not found - " + load_pgm_name);
		}
		pz390.set_psw_check(pz390.psw_pic_link_err); // link failed
		return;
	}
	if (ez390_pgm == null){
		ez390_pgm = load_pgm_name; // save first pgm
        if (tz390.opt_guam){
           	gz390 = new gz390();
        	gz390.start_guam(ez390_pgm,tz390);
			if (tz390.z390_abort){
				abort_error(58,"GUAM GUI startup abort");
			}
        }
	}
	int link_addr = pz390.reg.getInt(pz390.r0);
	if  (pz390.reg.getInt(pz390.r15) == 0){
		if (tot_link_stk < max_link_stk){
			cur_link_stk = tot_link_stk;
			tot_link_stk++;
			link_stk_cde[cur_link_stk] = cur_cde; // set by load
			link_stk_ret[cur_link_stk] = pz390.psw_loc | pz390.psw_amode_bit;
		}
		pz390.reg.putInt(pz390.r14,pz390.zcvt_exit);
		pz390.reg.putInt(pz390.r15,link_addr);
		pz390.set_psw_loc(link_addr);
        if ((link_addr & pz390.psw_amode31_bit) != 0){
            pz390.set_psw_amode(pz390.psw_amode31_bit);
        } else {
            pz390.set_psw_amode(pz390.psw_amode24_bit);
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
	 *   1.  Set r0 to length of allocated area rounded to *8 // RPI 542 (was address)
	 *   2.  set r1 to address of area                        // RPI 542 not set previously                      
	 *   3.  set r15 to 0 of ok, else nz
	 * Notes:
	 *   1.  Use TRACEMEM option to trace FQE's 
	 *   2.  If no 31 bit memory then allocate from
	 *       24 bit memory else abort if requested
	 *       memory type no available.
	 */
	req_len = pz390.reg.getInt(pz390.r1);	   
	req_len = (req_len + 7)/8*8; // round to 8
	if (req_len <= 0){
		pz390.set_psw_check(pz390.psw_pic_gm_err);
		return;
	}
	req_opt = pz390.reg.getInt(pz390.r0);
	if ((req_opt & opt_getmain_amode31) != 0
		&& pz390.dsa31_start != 0){
		cur_fqe = pz390.mem.getInt(pz390.zcvt_fqe31);
		prev_fqe = pz390.zcvt_fqe31;
	} else {
		cur_fqe = pz390.mem.getInt(pz390.zcvt_fqe24);
		prev_fqe = pz390.zcvt_fqe24;
	}
	while (cur_fqe > 0){
		cur_fqe_len = pz390.mem.getInt(cur_fqe+4);
        if (!check_fqe_ok()){
        	return;
        }
		if (cur_fqe_len >= req_len){
			// allocate from end of cur_fqe
			cur_fqe_len = cur_fqe_len - req_len;
			pz390.reg.putInt(pz390.r0,req_len);               // RPI 542
			pz390.reg.putInt(pz390.r1,cur_fqe + cur_fqe_len); // RPI 542
			if (tz390.opt_tracemem){
				trace_mem("GETMAIN ",cur_fqe+cur_fqe_len,req_len,0);
			}
			if (cur_fqe_len > 0){
				pz390.mem.putInt(cur_fqe+4,cur_fqe_len);
				if (tz390.opt_tracemem){
					trace_mem("FQE UPDT",cur_fqe,cur_fqe_len,next_fqe);
				}
			} else {
				if (tz390.opt_tracemem){
					trace_mem("FQE DEL ",cur_fqe,req_len,next_fqe);
				}
				pz390.mem.putInt(prev_fqe,next_fqe);
			}
			pz390.tot_mem_alloc = pz390.tot_mem_alloc + req_len;
			pz390.reg.putInt(pz390.r15,0);
			return;
		} else {
			prev_fqe = cur_fqe;
			cur_fqe = pz390.mem.getInt(cur_fqe);
		}
	}
	pz390.reg.putInt(pz390.r15,4); //RPI191
	if ((req_opt & opt_getmain_cond) == 0){
		pz390.set_psw_check(pz390.psw_pic_no_mem);
	}
}
private void svc_freemain(){
	/*
	 * Input
	 *   1.  r0 = length  to return  RPI 244
	 *   2.  r1 = address to return  RPI 244
	 *   
	 * Output:    
	 *   1.  set r15 to 0 of ok, else abort
	 */
	req_addr = pz390.reg.getInt(pz390.r1) & pz390.psw_amode31; // RPI 244
	req_len = pz390.reg.getInt(pz390.r0);	// RPI 244
	req_len = (req_len + 7)/8*8; // round to 8
	if (req_addr < pz390.dsa24_end){
		if (req_len <= 0
			|| (req_addr & 0x7) != 0
			|| req_addr < pz390.dsa24_start
			|| req_addr + req_len  > pz390.dsa24_end
			){
			pz390.set_psw_check(pz390.psw_pic_fm_err);
			return;
		}
		cur_fqe = pz390.mem.getInt(pz390.zcvt_fqe24);
		prev_fqe = pz390.zcvt_fqe24;
	} else {
		if (req_len <= 0
			|| (req_addr & 0x7) != 0
			|| req_addr < pz390.dsa31_start
			|| req_addr + req_len  > pz390.dsa31_end
			){
			pz390.set_psw_check(pz390.psw_pic_fm_err);
			return;
		}
		cur_fqe = pz390.mem.getInt(pz390.zcvt_fqe31);
		prev_fqe = pz390.zcvt_fqe31;
	}
	while (cur_fqe > 0){
		next_fqe    = pz390.mem.getInt(cur_fqe);
		cur_fqe_len = pz390.mem.getInt(cur_fqe+4);
        if (!check_fqe_ok()){
        	return;
        }
		if (req_addr < cur_fqe){
			// insert after prior fqe or cvt
			if (tz390.opt_tracemem){
                trace_mem("FREEMAIN",req_addr,req_len,0);
			}
			if (req_addr + req_len == cur_fqe){
				// merge insert with cur_fqe
				pz390.mem.putInt(req_addr,next_fqe);
				pz390.mem.putInt(req_addr+4,req_len + cur_fqe_len); // RPI 491 (was + cur_fqe)
				if (tz390.opt_tracemem){
					trace_mem("FQE IMRG",req_addr,req_len+cur_fqe,next_fqe);
				}
			} else {
				pz390.mem.putInt(req_addr,cur_fqe);
				pz390.mem.putInt(req_addr+4,req_len);
				if (tz390.opt_tracemem){
					trace_mem("FQE INST",req_addr,req_len,cur_fqe);
				}
			}
			pz390.mem.putInt(prev_fqe,req_addr);	
			pz390.tot_mem_alloc = pz390.tot_mem_alloc - req_len;
			pz390.reg.putInt(pz390.r15,0);
			return;
		} else if (req_addr == cur_fqe + cur_fqe_len){
			// append to current fqe
			if (tz390.opt_tracemem){
                trace_mem("FREEMAIN",req_addr,req_len,0);
			}
			cur_fqe_len = cur_fqe_len + req_len;
            if (cur_fqe + cur_fqe_len == next_fqe){
            	// merge cur and next fqe's
            	next_fqe_len = pz390.mem.getInt(next_fqe+4);
            	next_fqe = pz390.mem.getInt(next_fqe);
            	pz390.mem.putInt(cur_fqe,next_fqe);
            	pz390.mem.putInt(cur_fqe+4,cur_fqe_len+next_fqe_len);
    			if (tz390.opt_tracemem){
                    trace_mem("FQE MRGE",cur_fqe,req_len,next_fqe);
    			}
            } else if (next_fqe > 0 
            		   && cur_fqe + cur_fqe_len > next_fqe){
    			pz390.set_psw_check(pz390.psw_pic_bad_mem);
    			return;
            } else {
            	pz390.mem.putInt(cur_fqe+4,cur_fqe_len);
            }
            pz390.tot_mem_alloc = pz390.tot_mem_alloc - req_len;
			pz390.reg.putInt(pz390.r15,0);
			return;
		} else if (req_addr > cur_fqe + cur_fqe_len){
			prev_fqe = cur_fqe;
			cur_fqe = next_fqe;
		} else {
			// abort due to memory corruption 
			pz390.set_psw_check(pz390.psw_pic_bad_mem); 
			return;
		}
	}
	// insert after last fqe or cvt
	if (tz390.opt_tracemem){
        trace_mem("FREEMAIN",req_addr,req_len,0);
	}
	pz390.mem.putInt(req_addr,0);
	pz390.mem.putInt(req_addr+4,req_len);
	pz390.mem.putInt(prev_fqe,req_addr);
	if (tz390.opt_tracemem){
		trace_mem("FQE ADD ",req_addr,req_len,0);
	}
	pz390.tot_mem_alloc = pz390.tot_mem_alloc - req_len;
	pz390.reg.putInt(pz390.r15,0);
}
private boolean check_fqe_ok(){
	/*
	 * trace fqe if option tracemem on and
	 * verify address and length ok
	 */
	if (tz390.opt_tracemem){
		trace_mem("FQE     ",cur_fqe,cur_fqe_len,next_fqe);
	}
	if (cur_fqe <= prev_fqe 
			|| (cur_fqe & 0x7) != 0
			|| cur_fqe_len <= 0
			|| (cur_fqe_len & 0x7) != 0){
		pz390.set_psw_check(pz390.psw_pic_bad_mem);
		return false;
	}
	return true;
}
private void trace_mem(String mem_type,int mem_addr,int mem_len,int mem_nxt){
	put_log("TRACE MEMORY " + mem_type 
			+ " LOC=" + tz390.get_hex(mem_addr,8)
			+ " LEN=" + tz390.get_hex(mem_len,8)
			+ " NXT=" + tz390.get_hex(mem_nxt,8)
			);
}
private void svc_time(){
	/*
	 * return time and date in requested format
	 * See TIME.MAC for additional information.
	 *   R0 LH = date type
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
	int date_type = pz390.reg.getShort(pz390.r0);
	int time_type = pz390.reg.getShort(pz390.r0+2);
	int time_addr = pz390.reg.getInt(pz390.r1) & pz390.psw_amode;
	if (tz390.opt_timing){
	    pz390.cur_date = new Date();
	}
	time_mil  = pz390.cur_date.getTime();
    switch (time_type){
	case 0: // dec
        pz390.reg.putInt(pz390.r0,Integer.valueOf(cur_tod_hhmmss00.format(pz390.cur_date),16)
        		+(Integer.valueOf(cur_date_ms.format(pz390.cur_date),16) >> 4));
        pz390.reg.putInt(pz390.r1,get_ccyydddf());
        pz390.reg.putInt(pz390.r15,0);
        return;
	case 1: // bin - 0.01 sec = (milliseconds/10)
		pz390.reg.putInt(pz390.r0,(int)((time_mil-tod_start_day)/10));
        pz390.reg.putInt(pz390.r1,get_ccyydddf());
        pz390.reg.putInt(pz390.r15,0);
        return;
	case 2: // tu - 26.04166 mic (milliseconds * 1000 / 26.04166)
		pz390.reg.putInt(pz390.r0,(int)((time_mil-tod_start_day)*1000*100000/2604166));
        pz390.reg.putInt(pz390.r1,get_ccyydddf());
        pz390.reg.putInt(pz390.r15,0);
        return;
	case 3: // mic - double word microseconds 
		pz390.mem.putLong(time_addr,(time_mil-tod_start_day)*1000);
        pz390.reg.putInt(pz390.r1,get_ccyydddf());
        pz390.reg.putInt(pz390.r15,0);
        return;
	case 4: // stck - double word clock (bit 51 = microseconds)
		pz390.mem.putLong(time_addr,((time_mil-tod_start_day))*1000 << (63-51));
        pz390.reg.putInt(pz390.r1,get_ccyydddf());
        pz390.reg.putInt(pz390.r15,0);
        return;
	case  5: // clock stck  - 8 bytes (bit 51 ms from 1900)
		pz390.ibm_ms = (time_mil+pz390.ibm_mil)*1000;
		pz390.mem.putLong(time_addr,pz390.ibm_ms << (63-51));
		break;
	case  6: // clock stcke - 16 bytes(bit 59 ms from 1900)
		pz390.mem.putLong(time_addr,(((time_mil+pz390.ibm_mil)*1000) << (63-59)));
		pz390.mem.putLong(time_addr+8,0);
		break;
	case  7: // clock java  - 8 bytes (bit 63 mil from 1970)
		pz390.mem.putLong(time_addr,time_mil);
		break;
	case 8:  // nano - 8 byte (bit 63 nano-second counter)
		pz390.mem.putLong(time_addr,System.nanoTime());
		break;
	case 10: // dec
        pz390.mem.putInt(time_addr,Integer.valueOf(cur_tod_hhmmss00.format(pz390.cur_date),16)
        		+(Integer.valueOf(cur_date_ms.format(pz390.cur_date),16) >> 4));
	    break;
	case 11: // bin - 0.01 sec = (mic/10000)
		pz390.mem.putInt(time_addr,(int)((time_mil-tod_start_day)/10));
		break;
	case 13: // mic - double word microseconds 
		pz390.mem.putLong(time_addr,(time_mil-tod_start_day)*1000);
		break;
	case 14: // stck - double word clock (bit 51 = microseconds)
		pz390.mem.putLong(time_addr,((time_mil-tod_start_day)*1000) << (63-51));
		break;
	case 15: // estck - double word clock (bit 51 = microseconds)
		pz390.mem.putLong(time_addr,((time_mil-tod_start_day)*1000) << (63-59));
		pz390.mem.putLong(time_addr+8,0);
        pz390.reg.putInt(pz390.r15,0);
        return;
	case 0x80: // PTFF-QAF 0x00 - query ava8lable functions  
		// 0-3 bits on for functions 0-3 problem state functions avail.
		// 4-127 bits off for supervisor state functions not avail
		pz390.mem.putInt(time_addr,0xf0000000); // functions 0-3 avail
		Arrays.fill(pz390.mem_byte,time_addr+1,time_addr+16,(byte)0x00);
        pz390.reg.putInt(pz390.r15,0);
		return;
	case 0x81: // PTFF-QTO 0x01 - time of day offset
	    // 0-7   physical clock - double word clock (bit 51 = microseconds)
		// 8-15  tod offset from physical clock
		// 16-23 logical tod offset
		// 24-31 tod epoch difference 
		pz390.mem.putLong(time_addr,((time_mil-tod_start_day))*1000 << (63-51));
		Arrays.fill(pz390.mem_byte,time_addr+8,time_addr+32,(byte)0x00);
        pz390.reg.putInt(pz390.r15,0);
        return;
	case 0x82: // PTFF-QSI steering information 
		// 0-7 physical clock
		// 8-56 currently old and new epoch all zeros
		pz390.mem.putLong(time_addr,((time_mil-tod_start_day))*1000 << (63-51));
		Arrays.fill(pz390.mem_byte,time_addr+8,time_addr+56,(byte)0x00);
		pz390.reg.putInt(pz390.r15,0);
        return;
	case 0x83: // PTFF-QPT physical clock
		// 9-7 physical clock
		pz390.mem.putLong(time_addr,((time_mil-tod_start_day))*1000 << (63-51));
		pz390.reg.putInt(pz390.r15,0);
        return;
	default:
		pz390.reg.putInt(pz390.r15,4);
	    return;
    }
    switch (date_type){
    case 1: // 8(4,R1) = 0YYYYDDD
        pz390.mem.putInt(time_addr+8,Integer.valueOf(cur_date_yyyyddd.format(pz390.cur_date),16));
        break;
    case 2: // 8(4,R1) = MMDDYYYY
        pz390.mem.putInt(time_addr+8,Integer.valueOf(cur_date_MMddyyyy.format(pz390.cur_date),16));
        break;
    case 3: // 8(4,R1) = DDMMYYYY
        pz390.mem.putInt(time_addr+8,Integer.valueOf(cur_date_ddMMyyyy.format(pz390.cur_date),16));
        break;
    case 4: // 8(4,R1) = YYYYMMDD
        pz390.mem.putInt(time_addr+8,Integer.valueOf(cur_date_yyyyMMdd.format(pz390.cur_date),16));
        break;
    default:
		pz390.reg.putInt(pz390.r15,4);
        return;
    }
    pz390.reg.putInt(pz390.r15,0);
}
private void svc_ttimer(){
	/*
	 * process TTIMER cancel, tu, or mic request
	 *   R0 BIT 0 = return MIC at R1 addr else TU om R0
	 *   R0 BIT 1 = CANCEL else just return time
	 * Notes:
	 *   1.  RPI 452 return time with/withour cancel
	 *       opcode changed so recompile required.
	 */
	int  opcode = pz390.reg.getInt(pz390.r0);
	long mics_remaining = 1000 * (stimer_exit_time - System.currentTimeMillis()) ;
	if ((opcode & 0x1) == 0){
		// return TU time in R0
		if (mics_remaining <= 0){
			mics_remaining = 0;
			pz390.reg.putInt(pz390.r0,0);
			pz390.reg.putInt(pz390.r15,0);
		} else {
			long tu = (long)(mics_remaining/26.0144);
			if ((tu >> 32) == 0){
				pz390.reg.putInt(pz390.r0,(int)tu);
				pz390.reg.putInt(pz390.r15,0);
			} else {
				pz390.reg.putInt(pz390.r15,4);
			}
		}
	} else {
		// return MIC time at addr in R1
		if (mics_remaining < 0){
            mics_remaining = 0;
		}
		int addr = pz390.reg.getInt(pz390.r1) & pz390.psw_amode;
		pz390.mem.putLong(addr,mics_remaining << 12);
		pz390.reg.putInt(pz390.r15,0);
	}
	if ((opcode & 0x2) == 0x2){
		// cancel stimer
		stimer_exit_addr = 0;
	}
}
private void svc_stimer(){
	/*
	 * process timer interval request
	 */
	byte req_type   = pz390.reg.get(0);
	byte intvl_type = pz390.reg.get(1);
	int  intvl_addr = pz390.reg.getInt(pz390.r1) & pz390.psw_amode;
	long intvl_mics = 0;
	/*
	 * set intvl_mics interval in microseconds
	 */
	switch (intvl_type){
	case 1: // BINTVL=RX FWORD 0.01 SEC
		intvl_mics = (long)pz390.mem.getInt(intvl_addr) * 10000;
;		break;
	case 2: // DINTVL=RX DWORD PL8'HHMMSSTH'
		long hhmmssth = pz390.mem.getLong(intvl_addr) >>> 4;
		int hh = (int)((hhmmssth >> 28)*10+((hhmmssth & 0x0f000000) >> 24));
		int mm = (int)(((hhmmssth & 0x00f00000) >> 20)*10+((hhmmssth & 0x000f0000) >> 16));
		int ss = (int)(((hhmmssth & 0x0000f000) >> 12)*10+((hhmmssth & 0x00000f00) >> 8));
		int th = (int)(((hhmmssth & 0x000000f0) >> 4)*10+(hhmmssth & 0xf));
		intvl_mics = 10000 * (th + 100 * (ss + 60 * (mm + 60 * hh)));
		break;
	case 3: // MICVL=RX DWORD MICS
		intvl_mics = pz390.mem.getLong(intvl_addr);
		break;
	case 4: // TUINTVL=RX DWORD 26.04166 MICS
        intvl_mics = Double.valueOf(pz390.mem.getInt(intvl_addr) * 26.04166).longValue();
		break;
	default:
    	pz390.set_psw_check(pz390.psw_pic_spec);
	    break;
	}
	/*
	 * process request by type
	 */
	switch (req_type){
	case 1: // WAIT
		sleep_now(intvl_mics/1000);
		break;
    case 2: // REAL/TASK
    	stimer_exit_addr  = pz390.reg.getInt(pz390.r0) & pz390.psw_amode;
    	if (stimer_exit_addr == 0){
    		pz390.set_psw_check(pz390.psw_pic_spec);
    	    return;
    	}
    	stimer_exit_time = System.currentTimeMillis() + intvl_mics/1000;
		break;
    default:
    	pz390.set_psw_check(pz390.psw_pic_spec);
    	break;
	}	
}
public void start_stimer_exit(){
	/*
	 * Start stimer exit when stimer_exit_request
	 * found true by instruction loop or wait loop
	 */
	stimer_save_r13 = pz390.reg.getInt(pz390.r13);
	stimer_save_r14 = pz390.reg.getInt(pz390.r14);
	stimer_save_r15 = pz390.reg.getInt(pz390.r15);
	stimer_save_psw = pz390.psw_loc;
	pz390.reg.putInt(pz390.r13,pz390.zcvt_stimer_save);
	pz390.reg.putInt(pz390.r14,pz390.zcvt_exit);
	pz390.reg.putInt(pz390.r15,stimer_exit_addr);
	stimer_exit_request = false; // request stimer exit
	stimer_exit_running = true;  // reset psw
	pz390.psw_loc = stimer_exit_addr;
	stimer_exit_addr = 0;        // turn off stimer
    if (tz390.opt_trace){
    	tz390.put_trace("STIMER EXIT STARTING");
    }
}
private int get_ccyydddf(){
	/* 
	 * return ccyydddf for r1 linkage=svc calls
	 */
    int cc = Integer.valueOf(cur_date_yyyy.format(pz390.cur_date))/100-19;
    return (cc << 24) 
           | ((Integer.valueOf(cur_date_yyddd.format(pz390.cur_date),16)) << 4)  
           | 0xf;
}
private void svc_bldl(){
	/*
	 * search for members specified in BLDL
	 * list passed in R1 using SYS390 direcotory list
	 * and set return code:
	 *   0 - all members found
	 *   4 - one or more not found 
	 *       entry field R (offset  if all found else 4 if not.
	 *   8 - invalid entry count or entry length
	 * The R field in entries found is set to 1
	 * and set to 0 if not found.
	 * If entry length >= 13 then set Z to 1 if found in memory
	 * else 0.
	 */
	int bldl_list_addr = pz390.reg.getInt(pz390.r1) & pz390.psw_amode;
	int bldl_list_count = pz390.mem.getShort(bldl_list_addr);
	if (bldl_list_count < 1){
		pz390.reg.putInt(pz390.r15,8);
		return;
	}
	String bldl_last_name = "";
	int bldl_entry_addr = bldl_list_addr+2;
	int bldl_rc = 0;
	int bldl_entry_len = 0;
	while (bldl_list_count > 0){
		bldl_entry_len = pz390.mem.getShort(bldl_entry_addr);
		if (bldl_entry_len < 12){  // RPI 311
			pz390.reg.putInt(pz390.r15,8);
			return;
		}
		String bldl_member_name = get_ascii_string(bldl_entry_addr+2,8);
		if (bldl_member_name.compareTo(bldl_last_name) <= 0){
			pz390.reg.putInt(pz390.r15,8);
			return;
		}
		bldl_last_name = bldl_member_name;
		cur_cde = tz390.find_key_index('P',bldl_member_name.toUpperCase() + tz390.z390_type); // RPI 499
		if (cur_cde != -1 && cde_loc[cur_cde] != 0){
            pz390.mem.put(bldl_entry_addr+2+10,(byte)1); // R=1 entry found
            if (bldl_entry_len >= 13){  // RPI 311
            	pz390.mem.put(bldl_entry_addr+2+12,(byte)1); // Z=1 entry found in memory
            }
		} else {
			String bldl_member_file_name = tz390.find_file_name(tz390.dir_390,bldl_member_name,tz390.z390_type,tz390.dir_cur);
			if (bldl_member_file_name != null){
				pz390.mem.put(bldl_entry_addr+2+10,(byte)1); // R=1 entry found
				if (bldl_entry_len >= 13){  // RPI 311
					pz390.mem.put(bldl_entry_addr+2+12,(byte)0); // Z=0 entry found in sys390 dir
				}
			} else {
				pz390.mem.put(bldl_entry_addr+2+10,(byte)0); // R=1 entry not found
				bldl_rc = 4;
			}
		}
		bldl_entry_addr = bldl_entry_addr +2 + bldl_entry_len;  // RPI 311
		bldl_list_count--;
	}
	pz390.reg.putInt(pz390.r15,bldl_rc);
}
public void svc_abend(int pic, boolean type, boolean req_dump){
	/*
	 * 1.  Display abend code, psw, instr,and gpr's
	 * 2.  If dump_reguested
	 *        display gpr's
	 *        display fpr'ss
	 *        display tiot dcbs
	 *        dump all storage
	 * 3.  Abort if not in test mode
	 */
	int dump_loc = pz390.psw_loc - pz390.psw_ins_len;
	if (pic == 0x0c1 || pz390.psw_loc < 0){
		dump_loc = pz390.psw_loc;
	}
	String abend_code;
	if (type){  // system or user requested abend
		abend_code = "S" + tz390.get_hex(pic,3);
	} else {
		abend_code = "000" + pic;
		abend_code = "U" + abend_code.substring(abend_code.length()-4);
	}
	log_error(11,"ABEND " + abend_code
	       + " PSW=" + tz390.get_hex(dump_loc,8) 
		   + " INS=" + pz390.get_ins_hex(dump_loc).trim()
		   + " " + pz390.get_ins_name(dump_loc));
	dump_gpr(-1);
	if (req_dump){
		put_log("");
		dump_fpr(-1);
		put_log("");
		dump_tiot();
		put_log("");
		dump_cde_pgms();
		dump_mem(0,pz390.tot_mem);  // RPI 583
	}
	if (!tz390.opt_test){
		ez390_errors--; // don't count abend twice
		abort_error(12,"program aborting due to abend "
		       + abend_code);
	} else {
		pz390.psw_check = false; // reset default
	}
    svc_abend_type = system_abend;
}
public void dump_gpr(int reg_offset){
	/*
	 * dump specified register or all if -1
	 */
	if (reg_offset < 0 || reg_offset > pz390.r15){ // RPI 490
		put_log("R0-R3 " + pz390.bytes_to_hex(pz390.reg,0,32,8));
		put_log("R4-R7 " + pz390.bytes_to_hex(pz390.reg,32,32,8));
		put_log("R8-RB " + pz390.bytes_to_hex(pz390.reg,64,32,8));
		put_log("RC-RF " + pz390.bytes_to_hex(pz390.reg,96,32,8));
	} else {
		int reg_num = reg_offset/8;
		put_log("R" + reg_num + "=" + pz390.get_long_hex(pz390.reg.getLong(reg_offset)));
	}
}
public void dump_fpr(int reg_offset){
	/*
	 * dump specified fp register or all if -1
	 */
	if (reg_offset < 0 || reg_offset > pz390.r15){
		int reg_off = 0;
		while (reg_off < pz390.max_reg_off){  // RPI 229
			pz390.fp_store_reg(pz390.trace_reg,reg_off);
			reg_off = reg_off + 8;
		}
		put_log(" F0-F3 " + pz390.bytes_to_hex(pz390.trace_reg,0,32,8));
		put_log(" F4-F7 " + pz390.bytes_to_hex(pz390.trace_reg,32,32,8));
		put_log(" F8-FB " + pz390.bytes_to_hex(pz390.trace_reg,64,32,8));
		put_log(" FC-FF " + pz390.bytes_to_hex(pz390.trace_reg,96,32,8));
	} else {
		int reg_num = reg_offset/8;
		pz390.fp_store_reg(pz390.trace_reg,reg_offset);
		put_log(" F" + reg_num + "=" + pz390.get_long_hex(pz390.trace_reg.getLong(reg_offset)));
	}
}
private void dump_mem(int mem_addr,int mem_len){
	/*
	 * dump specified area of memory
	 */
	int dump_len = 0;
	if (mem_addr < 0){
		mem_addr = 0;
	}
	boolean last_saved    = false;
	boolean last_dup_line = false;
	String  dump_hex      = null;
	String  last_hex      = null;
	int     last_addr     = 0;
	while (mem_len > 0 && mem_addr + mem_len <= pz390.tot_mem){
		if (mem_len > 16){
			dump_len = 16;
		} else {
			dump_len = mem_len;
		}
		String dump_text = "";
		int index = 0;
		while (index < dump_len){
			if (tz390.opt_ascii){
				dump_text = dump_text + tz390.ascii_table.charAt(pz390.mem_byte[mem_addr+index] & 0xff);
			} else {
				dump_text = dump_text + tz390.ebcdic_table.charAt(pz390.mem_byte[mem_addr+index] & 0xff);
			}
			index++;
		}
		dump_text = tz390.left_justify(dump_text,16); // RPI 411
	    dump_hex = pz390.bytes_to_hex(pz390.mem,mem_addr,dump_len,4); 
		if (!last_saved){
			last_saved = true;
			last_addr  = mem_addr;
			last_dup_line = false;
		} else {
			if (last_hex.equals(dump_hex)){
				last_dup_line = true;
			} else {
				if (last_dup_line){
					put_log(" ........");
				}
				last_dup_line = false;
				last_addr = mem_addr;
			}
		}
		last_hex = dump_hex;
		if  (!last_dup_line){
			dump_hex = tz390.left_justify(dump_hex,35); // RPI 411
			put_log(" " +tz390.get_hex(mem_addr,8) 
				  + " *"  + dump_hex
				  + "* *" + dump_text + "*"
			      );
		}
        mem_addr = mem_addr + 16;
        mem_len  = mem_len  - 16;
	}
	if (last_dup_line){
		if (last_addr + 32 < mem_addr){
			put_log(" ........");
		}
		dump_hex = tz390.left_justify(dump_hex,35); // RPI 411
		put_log(" " +tz390.get_hex(mem_addr-16,8) + " *"  + dump_hex + "*");
	}
}
private void dump_mem_stat(){
	/*
	 * display total allocated and free memory
	 * totals on log
	 */
	put_log(" MEM TOTAL=" + (pz390.tot_mem >> 20) + "MB  ALLOC=" + pz390.tot_mem_alloc 
		  + "  FREE=" + (pz390.tot_mem - pz390.tot_mem_alloc));
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
	 *       with file path and name unless DCBDSNAM
	 *       field has pointer to DSN null or " 
	 *       delimited file spec in EBCDIC unless in
	 *       ASCII mode.
	 *   4.  See DCBD macro for DCB fields and
	 *       see DCB macro for generation of DCB
	 *   5.  TIOT table with unique entry for
	 *       each DDNAME holds open files.
	 *   6.  Take synad exit if defined else
	 *       issue error message and abort.
	 *  
	 *  Output registers:
	 *    R0 - 64 bit file length RPI 587
	 */
	cur_dcb_addr  = pz390.reg.getInt(pz390.r1) & pz390.psw_amode;
	check_dcb_addr();
	get_cur_tiot_index();
	if (cur_tiot_index != -1){
		cur_dcb_oflgs = pz390.mem_byte[cur_dcb_addr+dcb_oflgs] & 0xff;
		if ((cur_dcb_oflgs & dcb_oflgs_open) != 0){
			dcb_synad_error(22,"file already open");
			return;
		}
	    cur_dcb_file_name = get_dcb_file_name();
	    tiot_dsn[cur_tiot_index] = cur_dcb_file_name;
        cur_dcb_macrf = pz390.mem.getShort(cur_dcb_addr + dcb_macrf) & 0xffff;
        tiot_cur_rba[cur_tiot_index] = 0; // RPI101
	    tiot_vrec_blksi[cur_tiot_index] = 0;
		switch (cur_dcb_oflgs){
		case 0x20: // write - dcb_oflgs_pm (note NIO does not support write only)
			try {
	             tiot_file[cur_tiot_index] = new RandomAccessFile(cur_dcb_file_name,"rw");
	             if (cur_dcb_macrf == dcb_macrf_pm){
	            	 tiot_file[cur_tiot_index].setLength(0);
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
		    dcb_synad_error(24,"invalid open type - " + tz390.get_hex(cur_dcb_oflgs,8));
	        return;
		}
	}
	pz390.mem_byte[cur_dcb_addr + dcb_oflgs] = (byte)(cur_dcb_oflgs | dcb_oflgs_open);
	tiot_dcb_open[cur_tiot_index] = true;  //RPI110
	pz390.reg.putLong(0,tiot_eof_rba[cur_tiot_index]);  // RPI 587 return file length
	pz390.reg.putInt(pz390.r15,0);
}
private String get_dcb_file_name(){
	/*
	 * Get file name from DCBDSNAM if not zero
	 * else get file from DCBDDNAM environment 
	 * variable.
	 * NOtes:
	 *   1.  DCBDSNAM is EBCDIC uless in ASCII mode.
	 *   2.  File spec up to 265 long spacey name
	 *       with drive and path. 
	 */
	String file_name = "";
	int dcb_dsn = pz390.mem.getInt(cur_dcb_addr + dcb_dsnam);
	if (dcb_dsn > 0){
        file_name = get_ascii_var_string(dcb_dsn,max_lsn_spec).trim();  
        if (tz390.z390_os_type == tz390.z390_os_linux){ // RPI 532 file separator fix
        	file_name = file_name.replace('\\','/');
        }
        return file_name;
	} else {
		file_name = get_ascii_env_var_string(tiot_ddnam[cur_tiot_index]);
		if (file_name != null && file_name.length() > 0){
	        if (tz390.z390_os_type == tz390.z390_os_linux){ // RPI 532 file separator fix
	        	file_name = file_name.replace('\\','/');
	        }
			return file_name;
		} else {
			dcb_synad_error(62,"ddname=" + tiot_ddnam[cur_tiot_index] + " not found");
		}
	}
	return "";
}
private void svc_close(){
	/*
	 * close file if open else synad error
	 */
	cur_dcb_addr  = pz390.reg.getInt(pz390.r1) & pz390.psw_amode;
	check_dcb_addr();
	get_cur_tiot_index();
	if (cur_tiot_index != -1){
		cur_dcb_oflgs = pz390.mem_byte[cur_dcb_addr+dcb_oflgs] & 0xff;
		if ((cur_dcb_oflgs & dcb_oflgs_open) == 0){
			dcb_synad_error(25,"file already closed");
			return;
		}
	    try {
            if (tiot_vrec_blksi[cur_tiot_index] > 0){
            	tiot_file[cur_tiot_index].seek(tiot_cur_rba[cur_tiot_index]);
            	tz390.systerm_io++;
            	tiot_file[cur_tiot_index].writeInt((tiot_vrec_blksi[cur_tiot_index]+4) << 16);
            }
            tiot_file[cur_tiot_index].close();
            pz390.mem_byte[cur_dcb_addr+dcb_oflgs] = (byte)(pz390.mem_byte[cur_dcb_addr+dcb_oflgs] & (0xff ^ dcb_oflgs_open)); // RPI110
            pz390.mem.putInt(cur_dcb_addr + dcb_iobad,0); //RPI110
            tiot_dcb_open[cur_tiot_index] = false;  //RPI110
	    } catch (Exception e){
		    dcb_synad_error(26,"i/o error on close - " + e.toString());
		    return;
	    }
	} else {
		dcb_synad_error(25,"file already closed");
		return;
	}
	pz390.reg.putInt(pz390.r15,0);
}
private void svc_get_move(){
	/*
	 * get next record into area from dcb gm file
	 * Notes:
	 *   1.  Translate to EBCDIC unless ASCII mode
	 */
	cur_dcb_addr  = pz390.reg.getInt(pz390.r1) & pz390.psw_amode;
	check_dcb_addr();
	get_cur_tiot_index();
	if (cur_tiot_index != -1){
		cur_dcb_recfm = pz390.mem_byte[cur_dcb_addr+dcb_recfm] & 0xff;
		cur_dcb_area  = pz390.reg.getInt(pz390.r0) & pz390.psw_amode;
		cur_dcb_lrecl_f = pz390.mem.getInt(cur_dcb_addr + dcb_lrecl_f); // RPI 587
		cur_dcb_blksi_f = pz390.mem.getInt(cur_dcb_addr + dcb_blksi_f); // RPI 587
		if (tiot_cur_rba[cur_tiot_index] >= tiot_eof_rba[cur_tiot_index]){
			dcb_eodad_exit();
			return;
		} else if ((cur_dcb_recfm == dcb_recfm_ft
			    	 || cur_dcb_recfm == dcb_recfm_vt
			    	)
			    	&& tiot_cur_rba[cur_tiot_index]
			    	   == tiot_eof_rba[cur_tiot_index] -1){
			try {  //RPI63
				tz390.systerm_io++;
                byte eof_byte = tiot_file[cur_tiot_index].readByte();
                if (eof_byte == 0x1a){
        			dcb_eodad_exit();
        			return;
                } else {
                	tiot_file[cur_tiot_index].seek(tiot_cur_rba[cur_tiot_index]);
                }
			} catch (Exception e){
		        dcb_synad_error(78,"i/o error on get move ascii eof byte - " + e.toString());
		        return;
			}
		}
		switch (cur_dcb_recfm){
		case 0x40: // variable 
			try {
				if (cur_dcb_lrecl_f == 0){
					cur_dcb_lrecl_f = cur_dcb_blksi_f; // use blksi if no lrecl
				}
				tz390.systerm_io++;
                cur_vrec_lrecl = tiot_file[cur_tiot_index].readInt();
                pz390.mem.putInt(cur_dcb_area,cur_vrec_lrecl);
                cur_vrec_lrecl = cur_vrec_lrecl >>> 16;
                if (cur_vrec_lrecl <= 5 || cur_vrec_lrecl > cur_dcb_lrecl_f){
                	dcb_synad_error(28,"invalid variable record length - " + cur_vrec_lrecl);
                	return;
            	}
                tz390.systerm_io++;
                tiot_file[cur_tiot_index].read(pz390.mem_byte,cur_dcb_area + 4,cur_vrec_lrecl - 4);
                tiot_cur_rba[cur_tiot_index] = tiot_file[cur_tiot_index].getFilePointer();
	        } catch (Exception e){
		        dcb_synad_error(29,"i/o error on get move variable - " + e.toString());
		        return;
	        }
	        break;
		case 0x50: // variable blocked 
			try {
				if (cur_dcb_lrecl_f == 0){
					cur_dcb_lrecl_f = cur_dcb_blksi_f - 4; // use blksi if no lrecl
				}
				if (tiot_vrec_blksi[cur_tiot_index] == 0){
					tz390.systerm_io++;
					tiot_vrec_blksi[cur_tiot_index] = (tiot_file[cur_tiot_index].readInt() >>> 16)-4;
                    if (tiot_vrec_blksi[cur_tiot_index] < 5 || tiot_vrec_blksi[cur_tiot_index] > cur_dcb_blksi_f-4){
                    	dcb_synad_error(30,"invalid variable block size - " + tiot_vrec_blksi[cur_tiot_index]);
                    	return;
                    }
				}
				tz390.systerm_io++;
                cur_vrec_lrecl = tiot_file[cur_tiot_index].readInt();
                pz390.mem.putInt(cur_dcb_area,cur_vrec_lrecl);
                cur_vrec_lrecl = cur_vrec_lrecl >>> 16;
                if (cur_vrec_lrecl <= 5 || cur_vrec_lrecl > cur_dcb_lrecl_f){
                	dcb_synad_error(28,"invalid variable record length - " + cur_vrec_lrecl);
                	return;
                }
                tz390.systerm_io++;
                tiot_file[cur_tiot_index].read(pz390.mem_byte,cur_dcb_area + 4,cur_vrec_lrecl - 4);
                tiot_cur_rba[cur_tiot_index] = tiot_file[cur_tiot_index].getFilePointer();
                tiot_vrec_blksi[cur_tiot_index] = tiot_vrec_blksi[cur_tiot_index] - cur_vrec_lrecl;
                if (tiot_vrec_blksi[cur_tiot_index] < 0){
                	dcb_synad_error(31,"invalid variable block at rba " + pz390.get_long_hex(tiot_cur_rba[cur_tiot_index]));
                	return;
                }
	        } catch (Exception e){
		        dcb_synad_error(29,"i/o error on get move variable - " + e.toString());
		        return;
	        }
	        break;
		case 0x60: // variable from ascii text
			try {
				if (cur_dcb_lrecl_f == 0){
					cur_dcb_lrecl_f = cur_dcb_blksi_f; // use blksi if no lrecl
				}
				tz390.systerm_io++;
                cur_rec_text = tiot_file[cur_tiot_index].readLine();
                cur_rec_len  = cur_rec_text.length();
                tiot_cur_rba[cur_tiot_index] = tiot_file[cur_tiot_index].getFilePointer();
                if (cur_rec_len < 1 || cur_rec_len > cur_dcb_lrecl_f - 4){
                	dcb_synad_error(46,"variable record too long");
                	return;
                }
                pz390.mem.putInt(cur_dcb_area,(cur_rec_len+4) << 16);
                int index = 0;
                while (index < cur_rec_len){
                	if (tz390.opt_ascii){
                		pz390.mem_byte[cur_dcb_area + 4 + index] = (byte)cur_rec_text.charAt(index);
                	} else {
                		pz390.mem_byte[cur_dcb_area + 4 + index] = tz390.ascii_to_ebcdic[cur_rec_text.charAt(index)];
                	}
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
				if (cur_dcb_lrecl_f == 0){
					cur_dcb_lrecl_f = cur_dcb_blksi_f; // use blksi if no lrecl
				}
				tz390.systerm_io++;
                tiot_file[cur_tiot_index].read(pz390.mem_byte,cur_dcb_area,cur_dcb_lrecl_f);
                tiot_cur_rba[cur_tiot_index] = tiot_cur_rba[cur_tiot_index]+cur_dcb_lrecl_f;
	        } catch (Exception e){
		        dcb_synad_error(27,"i/o error on get move fixed - " + e.toString());
		        return;
	        }
	        break;
		case 0xa0: // fixed from ascii text
			try {
				if (cur_dcb_lrecl_f == 0){
					cur_dcb_lrecl_f = cur_dcb_blksi_f; // use blksi if no lrecl
				}
				tz390.systerm_io++;
                cur_rec_text = tiot_file[cur_tiot_index].readLine();
                cur_rec_len  = cur_rec_text.length();
                tiot_cur_rba[cur_tiot_index] = tiot_file[cur_tiot_index].getFilePointer();
                if (cur_rec_len <= cur_dcb_lrecl_f){
                	int index = 0;
                	while (index < cur_rec_len){
                		if (tz390.opt_ascii){
                			pz390.mem_byte[cur_dcb_area + index] = (byte)cur_rec_text.charAt(index);
                		} else {
                			pz390.mem_byte[cur_dcb_area + index] = tz390.ascii_to_ebcdic[cur_rec_text.charAt(index)];
                		}
                		index++;
                	}
                	while (index < cur_dcb_lrecl_f){
                		if (tz390.opt_ascii){
                			pz390.mem_byte[cur_dcb_area + index] = (byte)0x20; //RPI66
                		} else {
                			pz390.mem_byte[cur_dcb_area + index] = (byte)0x40;
                		}
                		index++;
                	}
                }
	        } catch (Exception e){
		        dcb_synad_error(44,"i/o error on get move fixed from ascii - " + e.toString());
		        return;
	        }
	        break;
	    default:
	        dcb_synad_error(31,"invalid dcb record format for get move - " + tz390.get_hex(cur_dcb_recfm,2));
            return;
		}
	} else {
		dcb_synad_error(30,"file not found");
		return;
	}
	pz390.reg.putInt(pz390.r15,0);
}
private void svc_put_move(){
	/*
	 * put next record from area to dcb pm file
	 */
	cur_dcb_addr  = pz390.reg.getInt(pz390.r1) & pz390.psw_amode;
	check_dcb_addr();
	get_cur_tiot_index();
	if (cur_tiot_index != -1){
		cur_dcb_oflgs = pz390.mem_byte[cur_dcb_addr + dcb_oflgs] & 0xff;
		if ((cur_dcb_oflgs & dcb_oflgs_pm) == 0){
			dcb_synad_error(33,"file not open for output");
			return;
		}
		cur_dcb_recfm = pz390.mem_byte[cur_dcb_addr+dcb_recfm] & 0xff;
		cur_dcb_area  = pz390.reg.getInt(pz390.r0) & pz390.psw_amode;
		cur_dcb_lrecl_f = pz390.mem.getInt(cur_dcb_addr + dcb_lrecl_f); // RPI 587
		cur_dcb_blksi_f = pz390.mem.getInt(cur_dcb_addr + dcb_blksi_f); // RPI 587
		switch (cur_dcb_recfm){
		case 0x80: // fixed - read lrecl bytes into area
		case 0x90: // fixed blocked
			try {
				if (cur_dcb_lrecl_f == 0){
					cur_dcb_lrecl_f = cur_dcb_blksi_f;
				}
				tz390.systerm_io++;
                tiot_file[cur_tiot_index].write(pz390.mem_byte,cur_dcb_area,cur_dcb_lrecl_f);
                tiot_cur_rba[cur_tiot_index] = tiot_cur_rba[cur_tiot_index]+cur_dcb_lrecl_f;
                if (tiot_file[cur_tiot_index].length() > tz390.max_file_size){
                	abort_error(101,"maximum file size exceeded for " + tiot_dsn[cur_tiot_index]);
                }
	        } catch (Exception e){
		        dcb_synad_error(34,"i/o error on put move fixed - " + e.toString());
		        return;
	        }
	        break;
		case 0x40: // variable 
			try {
				if (cur_dcb_lrecl_f == 0){
					cur_dcb_lrecl_f = cur_dcb_blksi_f;
				}
                cur_vrec_lrecl = pz390.mem.getInt(cur_dcb_area) >>> 16;
                if (cur_vrec_lrecl <= 5 || cur_vrec_lrecl > cur_dcb_lrecl_f){
    	            dcb_synad_error(35,"invalid variable record length - " + cur_vrec_lrecl);
                    return;
                }
                tz390.systerm_io++;
                tiot_file[cur_tiot_index].write(pz390.mem_byte,cur_dcb_area,cur_vrec_lrecl);
                tiot_cur_rba[cur_tiot_index] = tiot_cur_rba[cur_tiot_index]+cur_vrec_lrecl;
                if (tiot_file[cur_tiot_index].length() > tz390.max_file_size){
                	abort_error(102,"maximum file size exceeded for " + tiot_dsn[cur_tiot_index]);
                }
	        } catch (Exception e){
		        dcb_synad_error(36,"i/o error on put move variable - " + e.toString());
		        return;
	        }
	        break;
		case 0x50: // variable blocked 
			try {
				if (cur_dcb_lrecl_f == 0){
					cur_dcb_lrecl_f = cur_dcb_blksi_f - 4;
				}
				if (tiot_vrec_blksi[cur_tiot_index] == 0){
					tiot_cur_rba[cur_tiot_index] = tiot_file[cur_tiot_index].getFilePointer();
					tz390.systerm_io++;
					tiot_file[cur_tiot_index].writeInt(-1); // place holder for vb LLZZ
				}
				cur_vrec_lrecl = pz390.mem.getInt(cur_dcb_area) >>> 16;
                if (cur_vrec_lrecl < 5 || cur_vrec_lrecl > cur_dcb_lrecl_f){
                   	dcb_synad_error(37,"invalid variable record size - " + cur_vrec_lrecl);
                    return;
                }
                tiot_vrec_blksi[cur_tiot_index] = tiot_vrec_blksi[cur_tiot_index] + cur_vrec_lrecl;
                if (tiot_vrec_blksi[cur_tiot_index] > cur_dcb_blksi_f - 4){
                	tiot_vrec_blksi[cur_tiot_index] = tiot_vrec_blksi[cur_tiot_index] - cur_vrec_lrecl;
                	tiot_file[cur_tiot_index].seek(tiot_cur_rba[cur_tiot_index]);
                	tz390.systerm_io++;
                	tiot_file[cur_tiot_index].writeInt((tiot_vrec_blksi[cur_tiot_index]+4) << 16);
                	tiot_file[cur_tiot_index].seek(tiot_cur_rba[cur_tiot_index] + tiot_vrec_blksi[cur_tiot_index]+4);
                	tiot_cur_rba[cur_tiot_index] = tiot_file[cur_tiot_index].getFilePointer();
                	tz390.systerm_io++;
                	tiot_file[cur_tiot_index].writeInt(-1); // place holder for vb LLZZ
                	tiot_vrec_blksi[cur_tiot_index] = cur_vrec_lrecl;
                }
                tz390.systerm_io++;
                tiot_file[cur_tiot_index].write(pz390.mem_byte,cur_dcb_area,cur_vrec_lrecl);
                if (tiot_file[cur_tiot_index].length() > tz390.max_file_size){
                	abort_error(103,"maximum file size exceeded for " + tiot_dsn[cur_tiot_index]);
                }
	        } catch (Exception e){
		        dcb_synad_error(40,"i/o error on put move variable - " + e.toString());
		        return;
	        }
	        break;
		case 0x60: // variable to ascii text
			try {
				if (cur_dcb_lrecl_f == 0){
					cur_dcb_lrecl_f = cur_dcb_blksi_f;
				}
                cur_rec_len = (pz390.mem.getInt(cur_dcb_area) >> 16)-4;
                if (cur_rec_len < 1 || cur_rec_len > (cur_dcb_lrecl_f - 4)){
                	dcb_synad_error(48,"variable record too long - " + cur_rec_len);
                	return;
                }
                cur_rec_text = get_ascii_string(cur_dcb_area+4,cur_rec_len);
                tz390.systerm_io++;
                tiot_file[cur_tiot_index].writeBytes(cur_rec_text + tz390.newline); // RPI 500
                tiot_cur_rba[cur_tiot_index] = tiot_file[cur_tiot_index].getFilePointer();
                if (tiot_file[cur_tiot_index].length() > tz390.max_file_size){
                	abort_error(104,"maximum file size exceeded for " + tiot_dsn[cur_tiot_index]);
                }
	        } catch (Exception e){
		        dcb_synad_error(47,"i/o error on get move variable from ascii - " + e.toString());
		        return;
	        }
	        break;
		case 0xa0: // fixed to ascii text
			try {
				if (cur_dcb_lrecl_f == 0){
					cur_dcb_lrecl_f = cur_dcb_blksi_f;
				}
                cur_rec_text = get_ascii_string(cur_dcb_area,cur_dcb_lrecl_f);
                tz390.systerm_io++;
                tiot_file[cur_tiot_index].writeBytes(cur_rec_text + tz390.newline); // RPI 500
                if (tiot_file[cur_tiot_index].length() > tz390.max_file_size){
                	abort_error(105,"maximum file size exceeded for " + tiot_dsn[cur_tiot_index]);
                }
			} catch (Exception e){
		        dcb_synad_error(45,"i/o error on put move fixed to ascii - " + e.toString());
		        return;
			}
			break;
	    default:
	        dcb_synad_error(41,"invalid dcb record format for put move - " + tz390.get_hex(cur_dcb_recfm,2));
            return;
		}
	} else {
		dcb_synad_error(42,"file not found");
		return;
	}
	pz390.reg.putInt(pz390.r15,0);
}
private void svc_read(){
	/*
	 * read next record forward or backward
	 * into area from dcb macrf r/rw file
	 */
	cur_decb_addr  = pz390.reg.getInt(pz390.r1) & pz390.psw_amode;
	cur_dcb_addr  = pz390.mem.getInt(cur_decb_addr + decb_dcb) & pz390.psw_amode;
	check_dcb_addr();
	get_cur_tiot_index();
	if (cur_tiot_index != -1){
		if (tiot_cur_rba[cur_tiot_index] >= tiot_eof_rba[cur_tiot_index]){
            pz390.mem_byte[cur_decb_addr + decb_ecb] = (byte) 0x42; // post ecb for check eof exit
			return;
		}
		cur_dcb_area  = pz390.mem.getInt(cur_decb_addr + decb_area) & pz390.psw_amode;
		cur_dcb_recfm = pz390.mem_byte[cur_dcb_addr+dcb_recfm] & 0xff;
		cur_dcb_lrecl_f = pz390.mem.getInt(cur_dcb_addr + dcb_lrecl_f);  // RPI 587
		cur_dcb_blksi_f = pz390.mem.getInt(cur_dcb_addr + dcb_blksi_f);  // RPI 587
		try {
	        pz390.mem_byte[cur_decb_addr + decb_ecb] = (byte) 0x80; // post ecb waiting for post
			if (cur_dcb_lrecl_f == 0){
				cur_dcb_lrecl_f = cur_dcb_blksi_f; // use blksi if no lrecl
			}
			tz390.systerm_io++;
            tiot_file[cur_tiot_index].read(pz390.mem_byte,cur_dcb_area,cur_dcb_lrecl_f);
            tiot_cur_rba[cur_tiot_index] = tiot_cur_rba[cur_tiot_index]+cur_dcb_lrecl_f;
        } catch (Exception e){
            pz390.mem_byte[cur_decb_addr + decb_ecb] = (byte) 0x41; // post ecb for check synad exit
	        return;
        }
        pz390.mem_byte[cur_decb_addr + decb_ecb] = (byte) 0x40; // post ecb normal exit
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
	cur_decb_addr  = pz390.reg.getInt(pz390.r1) & pz390.psw_amode;
	cur_dcb_addr  = pz390.mem.getInt(cur_decb_addr + decb_dcb) & pz390.psw_amode;
	check_dcb_addr();
	get_cur_tiot_index();
	if (cur_tiot_index != -1){
		cur_dcb_area  = pz390.mem.getInt(cur_decb_addr + decb_area) & pz390.psw_amode;
		cur_dcb_recfm = pz390.mem_byte[cur_dcb_addr+dcb_recfm] & 0xff;
		cur_dcb_lrecl_f = pz390.mem.getInt(cur_dcb_addr + dcb_lrecl_f);  // RPI 587
		cur_dcb_blksi_f = pz390.mem.getInt(cur_dcb_addr + dcb_blksi_f);  // RPI 587
        pz390.mem_byte[cur_decb_addr + decb_ecb] = (byte) 0x80; // post ecb waiting for post
		try {
			if (cur_dcb_lrecl_f == 0){
				cur_dcb_lrecl_f = cur_dcb_blksi_f; // use blksi if no lrecl
			}
			tz390.systerm_io++;
			tiot_file[cur_tiot_index].write(pz390.mem_byte,cur_dcb_area,cur_dcb_lrecl_f);
            tiot_cur_rba[cur_tiot_index] = tiot_cur_rba[cur_tiot_index]+cur_dcb_lrecl_f;
            if (tiot_file[cur_tiot_index].length() > tz390.max_file_size){
            	abort_error(106,"maximum file size exceeded for " + tiot_dsn[cur_tiot_index]);
            }
        } catch (Exception e){
            pz390.mem_byte[cur_decb_addr + decb_ecb] = (byte) 0x41; // post ecb for check synad exit
	        return;
        }
        pz390.mem_byte[cur_decb_addr + decb_ecb] = (byte) 0x40; // post ecb normal exit
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
	cur_decb_addr = pz390.reg.getInt(pz390.r1) & pz390.psw_amode;
	cur_dcb_addr  = pz390.mem.getInt(cur_decb_addr + decb_dcb) & pz390.psw_amode;
	check_dcb_addr();
	cur_ecb = pz390.mem_byte[cur_decb_addr + decb_ecb];
	if (cur_ecb == 0x40){
		return;
	} else if (cur_ecb == 0x42){
		dcb_eodad_exit();
		return;
	} else {
		dcb_synad_error(90,"I/O error on read/write ECB=" + tz390.get_hex(pz390.mem.getInt(cur_decb_addr + decb_ecb),8)); //RPI112
	}
}
private void svc_point(){
	/*
	 * set dcb file pointer
	 *   r1 = address of dcb
	 *   r0 = 64 bit rba
	 */
	cur_dcb_addr  = pz390.reg.getInt(pz390.r1) & pz390.psw_amode;
	check_dcb_addr();
	get_cur_tiot_index();
	if (cur_tiot_index != -1){
		try {
			cur_rba = pz390.reg.getLong(0);
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
	 * error message and issue pgm check
	 */
	if (dcb_synad_recur)return; // RPI 377
	dcb_synad_recur = true;
	pz390.mem.putInt(pz390.r15,error_num);  //RPI53
	cur_dcbe_addr = pz390.mem.getInt(cur_dcb_addr + dcb_dcbe) & pz390.psw_amode;
	if (cur_dcbe_addr != 0){  // RPI 281
		cur_dcb_synad = pz390.mem.getInt(cur_dcbe_addr + dcbe_synad) & pz390.psw_amode;
	} else {
		cur_dcb_synad = pz390.mem.getInt(cur_dcb_addr + dcb_synad) & pz390.psw_amode;
	}
	if (cur_dcb_synad == 0){
		String cur_ddnam = get_ascii_string(cur_dcb_addr + dcb_ddnam,8);
		String cur_file  = get_dcb_file_name();
		log_error(43,"I/O error for DCB=" + tz390.get_hex(cur_dcb_addr,8) 
				             + " DDNAME=" + cur_ddnam
				             + " FILE=" + cur_file);
		log_error(error_num,error_msg);
		pz390.set_psw_check(pz390.psw_pic_io); //RPI64
	} else {
		pz390.set_psw_loc(cur_dcb_synad);
	}
	dcb_synad_recur = false; // RPI 377
}
private void dcb_eodad_exit(){
	/*
	 * take eodad exit if defined else issue
	 * error message and abort
	 */
	cur_dcbe_addr = pz390.mem.getInt(cur_dcb_addr + dcb_dcbe) & pz390.psw_amode;
	if (cur_dcbe_addr != 0){  // RPI 281
		cur_dcb_eodad = pz390.mem.getInt(cur_dcbe_addr + dcbe_eodad) & pz390.psw_amode;
	} else {
		cur_dcb_eodad = pz390.mem.getInt(cur_dcb_addr + dcb_eodad) & pz390.psw_amode;
	}
	if (cur_dcb_eodad == 0){
		abort_error(31,"read at end of file and no EODAD for " + tiot_ddnam[cur_tiot_index]);
	} else {
		pz390.set_psw_loc(cur_dcb_eodad);
	}
}
private void check_dcb_addr(){
	/*
	 * validate that cur_dcb_addr is
	 * on full word bound and that
	 * DCBID = EBCDIC or ASCII C'DCB1'
	 * else abort
	 * Notes:
	 *   1.  Also reset dcb_synad_recur
	 */
	dcb_synad_recur = false; // RPI 377
	if (cur_dcb_addr/4*4 != cur_dcb_addr  // RPI 152
		|| !get_ascii_string(cur_dcb_addr + dcb_id,8).equals(tz390.dcb_id_ver)){
		abort_error(80,"invalid DCB address or ID at DCB=(" 
			+ tz390.get_hex(cur_dcb_addr,8) 
			+ ")=" + pz390.bytes_to_hex(pz390.mem,cur_dcb_addr + dcb_id,8,0));
	}
}
private void get_cur_tiot_index(){
	/*
	 * 1.  Using cur_dcb addr from R1 or DECB get
	 *     cur_tiot index from DCBIOBAD (x'1C').
	 * 2.  reduce index in DCBIOBAD by 1.
	 * 3.  If index not -1, verify tiot_dcb_addr
	 *     and that dcb is open.
	 * 4.  If index = -1 or dcb's don't match
	 *     add new entry for DCBDDNAM.
	 */
	cur_tiot_index = pz390.mem.getInt(cur_dcb_addr + dcb_iobad) - 1;
    if (cur_tiot_index  != -1){
    	if (cur_dcb_addr == tiot_dcb_addr[cur_tiot_index]){
    		return;
    	} else {
    		abort_error(20,"dcb tiot index invalid DCB=" + tz390.get_hex(cur_dcb_addr,8));
    	}
    } else {
    	cur_dcb_ddnam = get_ascii_string(cur_dcb_addr + dcb_ddnam,8);
        cur_tiot_index = 0;
        while (cur_tiot_index < tot_tiot_files){
        	if (!tiot_dcb_open[cur_tiot_index]){
        		pz390.mem.putInt(cur_dcb_addr + dcb_iobad,cur_tiot_index + 1);
        		tiot_ddnam[cur_tiot_index] = cur_dcb_ddnam;
        		tiot_dcb_addr[cur_tiot_index] = cur_dcb_addr;
        		return;
        	}
        	cur_tiot_index++;
        }
        if (tot_tiot_files < max_tiot_files){
        	tot_tiot_files++;
    		pz390.mem.putInt(cur_dcb_addr + dcb_iobad,cur_tiot_index + 1);
    		tiot_ddnam[cur_tiot_index] = cur_dcb_ddnam;
    		tiot_dcb_addr[cur_tiot_index] = cur_dcb_addr;
    		return;
        } else {
        	abort_error(21,"maximum tiot files open exceeded");
        }
    }
}
private String get_ascii_env_var_string(String env_var_name){
	/*
	 * return environment variable string
	 * with leading and trailing spaces removed
	 * or return "" if not found.
	 */
	String text = System.getenv(env_var_name);
	if (text != null){
		return text.trim(); //RPI111
	} else {
		return "";
	}
}
private String get_ascii_var_string(int mem_addr,int max_len){
	/*
	 * return ascii variable length string 
	 * delimited by null or double quotes which
	 * are stripped off along with leading or traling 
	 * spaces.
	 */
	String text = "";
	int index = 0;
	while (index < max_len){
		byte data_byte = pz390.mem_byte[mem_addr+index];
		char data_char;
		if (tz390.opt_ascii){
			data_char = (char) data_byte;
		} else {
			data_char = (char) tz390.ebcdic_to_ascii[data_byte & 0xff]; //RPI42
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
public String get_ascii_string(int mem_addr,int mem_len){
	/*
	 * get ascii string with no trailing spaces from
	 * memory address and length
	 * Notes:
	 *   1.  Translates from EBCDIC to ASCII
	 *       unless in ASCII mode.
	 */
	String text = "";
	int index = 0;
	while (index < mem_len){
		if (tz390.opt_ascii){
			text = text + (char) pz390.mem_byte[mem_addr + index];
		} else {
			text = text + (char) tz390.ebcdic_to_ascii[pz390.mem_byte[mem_addr + index] & 0xff]; //RPI42
		}
		index++;
	}
	return ("x"+text).trim().substring(1);
}
public void put_ascii_string(String text,int mem_addr,int mem_len){
	/*
	 * put ascii string with trailing spaces to
	 * memory address and length
	 * Notes:
	 *   1.  Translates from ASCII to EBCDIC unless
	 *       ASCII mode
	 */
	char text_char;
	int index = 0;
	while (index < mem_len){
		if (index < text.length()){
			text_char = text.charAt(index);
		} else {
			text_char = ' ';
		}
		if (tz390.opt_ascii){
			pz390.mem_byte[mem_addr + index] = (byte)(text_char & 0xff); // RPI 551
		} else {
			pz390.mem_byte[mem_addr + index] = tz390.ascii_to_ebcdic[text_char & 0xff];
		}
		index++;
	}
}
private void svc_cmd(){
	/*
	 * exec OS command process
	 *   r0+2 = cmd process id 0-9
	 *   r0+3 = cmd operation type
	 *   r1   = A(command)
	 *   r2   = command length
	 *   r3   = timeout wait limit in milli-sec
	 */
	String svc_cmd_text = "";
	int cmd_id = pz390.reg.get(pz390.r0+2);
	int cur_cmd_op = pz390.reg.get(pz390.r0+3);
	switch (cur_cmd_op){
	case 0: // start command process at R1
		if (cmd_proc_running[cmd_id]){
			cmd_cancel(cmd_id);
		}
		cmd_startup(cmd_id);
		if (tz390.opt_time){
			pz390.cur_date = new Date();
		}
		pz390.reg.putInt(pz390.r15,0);
		break;
	case 1: // close windows command process
		if (cmd_proc_running[cmd_id]){
			cmd_cancel(cmd_id);
		}
		pz390.reg.putInt(pz390.r15,0);
		break;
	case 2: // send command string at R1, null term
		if (cmd_proc_running[cmd_id]){
			svc_cmd_text = get_ascii_var_string(pz390.reg.getInt(pz390.r1),max_lsn_spec);
			cmd_input(cmd_id,svc_cmd_text);
		    pz390.reg.putInt(pz390.r15,0);
		} else {
			pz390.reg.putInt(pz390.r15,8);
		}
		break;
	case 3: 
		// read next process output line into area 
		// at r1 padded to lenght in r2
		// wait for output up up to r3 millisconds
		// r15 =  0 record returned ok
		// r15 =  4 no record in time allowed
		// r15 =  8 no record and process terminated
		// r15 = 16 i/o error during operation see log
		try {
			int cmd_read_wait = pz390.reg.getInt(pz390.r3);
			String cmd_read_line = cmd_get_queue(cmd_id);
		    cmd_proc_start_time[cmd_id] = System.currentTimeMillis(); // RPI 545
		    while (cmd_read_line == null
					&& cmd_proc_running[cmd_id]
					&& cmd_proc_rc(cmd_id) == -1
					&& System.currentTimeMillis() - cmd_proc_start_time[cmd_id] < cmd_read_wait){
				sleep_now(tz390.monitor_wait);
				cmd_read_line = cmd_get_queue(cmd_id);
			}
			if  (cmd_read_line != null){
				put_ascii_string(cmd_read_line,pz390.reg.getInt(pz390.r1),pz390.reg.getInt(pz390.r2));
				pz390.reg.putInt(pz390.r15,0);
			} else if (cmd_proc_running[cmd_id]
					   && cmd_proc_rc(cmd_id) == -1){
				pz390.reg.putInt(pz390.r15,4);
			} else {
				pz390.reg.putInt(pz390.r15,12);
			}
		} catch (Exception e){
			log_error(76,"cmd read I/O error - " + e.toString());
			pz390.reg.putInt(pz390.r15,16);
		}
		break;
	}
}
private int cmd_startup(int cmd_id){
	/*
	 * start Windows command processer with 
	 * synchronized buffered output.  
     * Notes:
     *   1.  Start process cmd_id
     *   2.  Send input commands via cmd_input(id,msg)
     *   3.  Retrieve output from cmd_output_queue
     *   4.  Cancel via cmd_cancel(cur_id);
	 */
	 int rc;
	 String[] cmd_parms;
	 try {
       	if (tz390.z390_os_type == tz390.z390_os_linux){
           	cmd_parms = new String[2];
       		cmd_parms[0] = tz390.z390_command;
       		cmd_parms[1] = tz390.jar_file_dir() + "/cmd.pl"; // RPI 500
       	} else {
       		cmd_parms = new String[1];
       		cmd_parms[0] = tz390.z390_command;
       	}
       	rc = cmd_proc_start(cmd_id,cmd_parms);
        if  (rc != 0){
        	log_error(67,"CMD task startup error rc = " + rc);
 	 	    cmd_cancel(cmd_id);
	   	}
        return rc;
	 } catch (Exception e) {
	    log_error(66,"execution startup error - " + e.toString());
        cmd_cancel(cmd_id);
	   	return 16;
 	 }
}
public int cmd_proc_start(int cmd_id,String[] exec_cmd){
    /*
     * 1. Terminate any prior cmd process with
     *    error if non zero completion.
     * 2. Start new process running on 
     *    separate thread.  
     * 
     * Note: cmd monitor will issue exec_term
     *       if timeout limit is reached before next
     *       start command does it.  Error will be
     *       issued by exec_term if non zero return code
     *       or if process had to be cancelled.
     */
    int rc;
    if  (cmd_id + 1 > tot_cmd){
    	tot_cmd = cmd_id + 1;
    }
    if  (cmd_proc_running[cmd_id]){
    	rc = cmd_proc_rc(cmd_id);
        if (rc != 0){
        	if (rc == -1){
        		log_error(68,"previous command execution cancelled");
        	    cmd_cancel(cmd_id);
        	} else {
        		log_error(69,"previous command execution ended with rc =" + rc);
        	}
    	}
    }
    try {
        cmd_proc[cmd_id] = Runtime.getRuntime().exec(exec_cmd);
   	    cmd_error_reader[cmd_id] = new InputStreamReader(cmd_proc[cmd_id].getErrorStream());
   	    cmd_output_reader[cmd_id] = new InputStreamReader(cmd_proc[cmd_id].getInputStream());
        cmd_input_writer[cmd_id] = cmd_proc[cmd_id].getOutputStream();
   	    cmd_proc_thread[cmd_id] = new Thread(this);
	    cmd_error_thread[cmd_id] = new Thread(this);
	    cmd_output_thread[cmd_id] = new Thread(this);
	    cmd_output_msg[cmd_id] = "";
	    cmd_error_msg[cmd_id] = "";
	    cmd_output_queue[cmd_id] = new LinkedList<String>();
	    cmd_error_msg[cmd_id] = "";
	    cmd_proc_io[cmd_id] = 0;
	    cmd_proc_running[cmd_id] = true;  // RPI 592
	    cmd_proc_thread[cmd_id].start();
	    cmd_error_thread[cmd_id].start();
	    cmd_output_thread[cmd_id].start();
	    sleep_now(tz390.monitor_wait); // wait for first io
	    int wait_count = 5;
	    while (cmd_proc_io[cmd_id] == 0 && wait_count > 0){
	    	sleep_now(tz390.monitor_wait);
	    	wait_count--;
	    }
	    return 0;
    } catch (Exception e){
        log_error(70,"execution startup error " + e.toString());
        cmd_cancel(cmd_id);
        return -1;
    }
}
private synchronized void cmd_input(int cmd_id,String cmd_line){
    /*
     * send input to exec command in process
     */
    if  (cmd_line == null){
    	cmd_line = tz390.newline; // RPI 500
    } else {
    	cmd_line = cmd_line + tz390.newline; // RPI 500
    }
    try {
    	tz390.systerm_io++;
    	cmd_input_writer[cmd_id].write(cmd_line.getBytes());
    	cmd_io_total++;
    	cmd_proc_io[cmd_id]++;
    	cmd_input_writer[cmd_id].flush();
    } catch (Exception e){
    	log_error(71,"execution input error" + e.toString());
    }
}
public int cmd_proc_rc(int cmd_id){
    /*
     * return ending rc else -1
     * return 0 if no process defined
     */           	
    int rc = -1;
    if  (cmd_proc_running[cmd_id]){
        try {
        	rc = cmd_proc[cmd_id].exitValue(); 
        } catch (Exception e){
        }
    } else {
    	rc = 0;
    }
    return rc;
}
public void cmd_cancel(int cmd_id){
    /*
     * cancel exec process
     * 
     */
    cmd_proc_running[cmd_id] = false;
}
public void run() {
	// wait for comproc threads to end normally
	// or issue error if abnormal termination
	int cmd_id = 0;
	while (cmd_id < tot_cmd){
		if (cmd_proc_running[cmd_id] // RPI 592
            && cmd_proc_thread[cmd_id] == Thread.currentThread()) {
			io_count++;
			cmd_proc_io[cmd_id]++;
			try {
				cmd_proc[cmd_id].waitFor();
			} catch (Exception e){
				abort_error(79,"cmd proc wait error " + e.toString());
			}
			return;
		} else if (cmd_proc_running[cmd_id] // RPI 592
		           && cmd_output_thread[cmd_id] == Thread.currentThread()) {
			copy_cmd_output_to_queue(cmd_id);
			return;
		} else if (cmd_proc_running[cmd_id] // RPI 592
                   && cmd_error_thread[cmd_id] == Thread.currentThread()) {
			copy_cmd_error_to_queue(cmd_id);
			return;
		}
		cmd_id++;
	}
	// wait for next connection on each server
	// port thread, create connection and repeat 
	int port_index = 0;
	while (tcpio_server_running && port_index < max_tcp_server_port){
		if (tcp_server_port[port_index] > 0
			&& tcp_server_thread[port_index] != null
			&& tcp_server_thread[port_index] == Thread.currentThread()){
			while (tcp_server_socket[port_index] != null 
				   && !tcp_server_socket[port_index].isClosed()
				   && tcp_alloc_conn(port_index)){
				int conn_index = tcp_server_conn_index[port_index]; // get allocaed conn 
				try {
					// this conn thread will wait here for next connection
					if (tz390.opt_tracet){
						put_log("TCPIO waiting for connection on port=" + tcp_conn_server_port[conn_index]);
					}
					tcp_conn_socket[conn_index] = tcp_server_socket[port_index].accept();
					tcp_conn_input[conn_index] = new DataInputStream(tcp_conn_socket[conn_index].getInputStream());
					tcp_conn_output[conn_index] = new PrintStream(tcp_conn_socket[conn_index].getOutputStream());
				    tcp_conn_thread[conn_index] = new Thread(this);
				    tcp_conn_thread[conn_index].start();
					if (tz390.opt_tracet){
						put_log("TCPIO new connection on port=" + tcp_conn_server_port[conn_index] + " conn=" + tcp_server_conn_index[port_index]);
					}
				} catch (Exception e){
					if (tcpio_server_running){
						put_log("TCPIO error starting connection for port=" + tcp_server_port[port_index]);
						tcp_free_conn(conn_index);
					} else {
						return; // exit thread during shutdown close
					}
				}
			}
		}
		port_index++;
	}
	int conn_index = 0;  
	while (tcpio_server_running
			&& conn_index < max_tcp_conn){
		// find and run connection thread
		if (tcp_conn_server_port[conn_index] > 0 
			&& tcp_conn_thread[conn_index] != null
			&& tcp_conn_thread[conn_index] == Thread.currentThread()){
			while (tcpio_server_running){
				//
				// this is a live connection thread which
				// runs following loop until server port closed
				//   1   While message ready on connection
				//       signal TCPIO main user thread and
				//       then yield.
				//   2   Read 1 byte from connection
				//   3   if disconnect, release connection
				//       and terminate thread
				//       else set conn msg avail
				//   4.  repeat
				try {
					while (tcpio_server_running 
            			   && tcp_conn_msg_ready[conn_index]
                   	      ){
						lock.lock();
						try { // signal TCPIO main user thread to check for connection messages
							lock_condition.signalAll();
						} catch (Exception e){
							put_log("TCPIO disconnect during post for conn=" + conn_index + " port="  + tcp_conn_server_port[conn_index]);
							tcpio_close_conn(conn_index);
							return; // kill this thread
						} finally {
							lock.unlock();
						}
						// let tcpio main thread run until
						// all current conn msgs are read
						Thread.yield(); 
					}
				} catch(Exception e){
					put_log("TCPIO disconnect during yield for conn=" + conn_index + " port="  + tcp_conn_server_port[conn_index]);
					tcpio_close_conn(conn_index);
					return; // kill this thread
				}
				try {
					// thread waits here for 1 byte read
					// or disconnect interrupt
					tcp_conn_byte[conn_index] = tcp_conn_input[conn_index].readByte();
					tcp_conn_read[conn_index] = true;
					if (tz390.opt_tracet){
						put_log("TCPIO msg received on conn=" + conn_index + " LEN=" +(1+tcp_conn_input[conn_index].available()));
					}
                    tcpio_set_conn_msg_ready(conn_index,true);
				} catch (Exception e) {
					if (tcpio_server_running){
						put_log("TCPIO disconnect during read for conn=" + conn_index + " port="  + tcp_conn_server_port[conn_index]);
						tcpio_close_conn(conn_index);
						return; // kill this task
					}
				}
			}
		}
		conn_index++;
	}
}
public void copy_cmd_output_to_queue(int cmd_id){
	/*
	 * copy cmd output lines to output queue
	 */	
	try {
		tz390.systerm_io++;
		int next_int = cmd_output_reader[cmd_id].read();
		while (cmd_proc_running[cmd_id] // RPI 592
               && next_int != -1){
			if  (next_int == ascii_lf){
				cmd_proc_io[cmd_id]++;
				String msg = cmd_output_msg[cmd_id];
				if (msg.length() > 0){
                    cmd_put_queue(cmd_id,msg);
				}
				cmd_output_msg[cmd_id] = "";
			} else if (next_int != ascii_cr){
				cmd_output_msg[cmd_id] = cmd_output_msg[cmd_id].concat(String.valueOf((char) next_int));
			}
			tz390.systerm_io++;
			next_int = cmd_output_reader[cmd_id].read();
		}
	} catch (Exception e) {
		if (cmd_proc_running[cmd_id]){
			log_error(73,"cmd process output error - " + e.toString());
			cmd_cancel(cmd_id);
		}
	};
}
public void copy_cmd_error_to_queue(int cmd_id){
	/*
	 * copy cmd error lines to output queue
	 */	
	try {
		tz390.systerm_io++;
		int next_int = cmd_error_reader[cmd_id].read();
		while (cmd_proc_running[cmd_id] // RPI 592
               && next_int != -1){
		   if  (next_int == ascii_lf){
		   	   String msg = cmd_error_msg[cmd_id];
			   if (msg.length() > 0){
                   cmd_put_queue(cmd_id,msg);
			   }
		   	   cmd_error_msg[cmd_id] = "";
		   } else if (next_int != ascii_cr){
				cmd_error_msg[cmd_id] = cmd_error_msg[cmd_id].concat(String.valueOf((char) next_int));
		   }
		   tz390.systerm_io++;
		   next_int = cmd_error_reader[cmd_id].read();
		}
	} catch (Exception ex) {
		if (cmd_proc_running[cmd_id]){ // RPI 592
			log_error(74,"exec execution output error");
			cmd_cancel(cmd_id);
		}
	}
}
private synchronized void cmd_put_queue(int cmd_id,String msg){
	/*
	 * add output to linklist queue
	 * synchronized so output and main thread
	 * retrieval via CMDPROC READ are safe.
	 */
	if (cmd_proc_running[cmd_id] // RPI 592
        && !cmd_output_queue[cmd_id].offer(msg)){
		log_error(77,"cmd process output queue io error");
	}
}
private synchronized String cmd_get_queue(int cmd_id){
	/*
	 * retrieve next FIFO line from linklist queue
	 * synchronized so output and main thread
	 * retrieval via CMDPROC READ are safe.
	 * 
	 * If no string ready, return null
	 */
	if (!cmd_proc_running[cmd_id]){ // RPI 592
		return null;
	}
	try {
		String cmd_output_line = (String) cmd_output_queue[cmd_id].remove();
		return cmd_output_line;
	} catch (Exception e){
		return null;
	}
}
private void svc_tget_tput(){
	/*
	 * Read or write to TN3270 terminal
	 * Notes:
	 *   1.  If GUAM GUI Access Method enabled,
	 *       read or write to the GUAM GUI dialog.
	 *   2.  If no GUAM interface and EDIT mode use
	 *       WTO/WTOR to MCS console, else error.
	 */
	tpg_flags = pz390.reg.get(pz390.r1) & 0xff;
	gz390.tpg_flags = tpg_flags;
	gz390.tpg_type  = tpg_flags & tpg_type_mask;
	int buff_len   = pz390.reg.getShort(pz390.r0+2);
	int buff_addr  = pz390.reg.getInt(pz390.r1) & pz390.psw_amode24;
	String wto_msg = null;
	if (tz390.opt_guam){
		if ((tpg_flags & tpg_op_mask) == tpg_op_tput){
            gz390.tput_len = buff_len;
            if (gz390.tput_len > tput_buff.limit()){
            	abort_error(59,"GUAM GUI tput length too long");
            }
			gz390.tput_buff.position(0);
			gz390.tput_buff.put(pz390.mem_byte,buff_addr,buff_len);
			gz390.guam_tput();
			if (tz390.z390_abort){
				abort_error(59,"GUAM GUI tput external abort");
			}
		} else {
			gz390.tget_len = buff_len;
			gz390.guam_tget();
			if (tz390.z390_abort){
				abort_error(60,"GUAM GUI tget abort");
			}
			pz390.mem.position(buff_addr);
            // move tget_len actual and set R1= bytes returned
			pz390.mem.put(gz390.tget_byte,0,gz390.tget_len);
		    pz390.reg.putInt(pz390.r1,gz390.tget_len); 
		}
		pz390.reg.putInt(pz390.r15,gz390.tpg_rc); // RPI 221 set retrun code
	} else {
		switch (tpg_flags & tpg_type_mask){
		case 0x00: // EDIT type
		case 0x01: // ASIS type
			if ((tpg_flags & tpg_op_mask) == tpg_op_tput){ // TPUT
				wto_msg = get_ascii_string(buff_addr,buff_len);
				put_log("TPUT MSG = " + wto_msg);
				pz390.reg.putInt(pz390.r15,0); // RPI 221 set retrun code
			} else { // TGET
				if (!wtor_reply_pending){					
					wtor_reply_addr = buff_addr;
					wtor_reply_len  = buff_len;
					wtor_ecb_addr = pz390.zcvt_tget_ecb;
					wto_msg("TGET ENTER",0,0);
					pz390.mem.putInt(wtor_ecb_addr,ecb_waiting); // ecb waiting for post by montior wtorit
					wtor_reply_string  = null;
					wtor_reply_pending = true;
				}
				while ((tpg_flags & tpg_wait_mask) == tpg_wait
						&& (pz390.mem.getInt(wtor_ecb_addr) & ecb_waiting) == ecb_waiting){
					sleep_now(tz390.monitor_wait);
				}
				if (wtor_reply_string != null){
					pz390.reg.putInt(pz390.r15,0);
				} else {
					pz390.reg.putInt(pz390.r15,4);
				}
			}
			break;
		case 0x10: // CONTROL
			// ignore for now
			break;
		case 0x11: // FULLSCR
			abort_error(108,"tget/tput fullscr type requires GUAM option");
			pz390.reg.putInt(pz390.r15,8); // RPI 221 set retrun code
		}
	}
}
private void svc_guam(){
	/*
	 * GUAN Graphical User Access Method
	 * for user I/O vua GUAM GUI dialog window
	 * with 3 views:
	 *   1.  MCS - view for WTO and WTOR I/O
	 *   2.  SCREEN - view for TPUT and TGET I/0
	 *   3.  GRAPH - view for graphics
	 * r1 = major/minor opcode bytes
	 */
	if (!tz390.opt_guam){
		abort_error(104,"GUAM GUI option not specified - aborting");
	}
	guam_major = pz390.reg.get(pz390.r0+2);
	guam_minor = pz390.reg.get(pz390.r0+3);
	guam_args = pz390.reg.getInt(pz390.r1) & pz390.psw_amode;
	pz390.reg.putInt(pz390.r15,0);
	switch (guam_major){
	case 1: // WINDOW
		switch (guam_minor){
		case 1: // TITLE,"text" 
			guam_text = get_ascii_var_string(pz390.mem.getInt(guam_args),256);
			gz390.guam_window_title(guam_text);
			break;
		case 2: // LOC,x,y
            guam_x = pz390.mem.getInt(guam_args);
            guam_y = pz390.mem.getInt(guam_args+4);
			gz390.guam_window_loc(guam_x,guam_y);
			break;
		case 3: // SIZE,width,height
			guam_width = pz390.mem.getInt(guam_args);
			guam_height = pz390.mem.getInt(guam_args+4);
			gz390.guam_window_size(guam_width,guam_height);
			break;
		case 4: // FONT,size
			guam_font = pz390.mem.getInt(guam_args);
			gz390.guam_window_font(guam_font);
			break;
		case 5: // VIEW,mode,x,y,color
			guam_view = pz390.mem.getInt(guam_args);
			if (guam_view != 1){
				guam_x = pz390.mem.getInt(pz390.mem.getInt(guam_args+4) & pz390.psw_amode);
				guam_y = pz390.mem.getInt(pz390.mem.getInt(guam_args+8) & pz390.psw_amode);
				guam_color = pz390.mem.getInt(pz390.mem.getInt(guam_args+12) & pz390.psw_amode);
			}
			gz390.guam_window_view(guam_view,guam_x,guam_y,guam_color);
			break;
		case 6: // GETVIEW - return current view
			if (tz390.opt_guam){
				guam_view = gz390.guam_window_getview();
			} else {
				guam_view = 0;
			}
			pz390.reg.putInt(pz390.r0,guam_view);
			break;
		default:
			log_error(94,"undefined GUAM GUI Window command - " + guam_minor);
		    pz390.reg.putInt(pz390.r15,8);
		}
		break;
	case 2: // SCREEN
		switch (guam_minor){
		case 1: // READ,buff,lbuff,WAIT/NOWAIT
			guam_abuff  = pz390.mem.getInt(guam_args);
			guam_lbuff = pz390.mem.getInt(guam_args+4);
			guam_wait  = pz390.mem.getInt(guam_args+8) == 1;
			pz390.mem.position(guam_abuff);
			pz390.mem.put(gz390.guam_screen_read(guam_lbuff,guam_wait));
			break;
		case 2: // WRITE,row,col,buff,lbuff,color
			guam_row   = pz390.mem.getInt(guam_args);
			guam_col   = pz390.mem.getInt(guam_args+4);
			guam_abuff  = pz390.mem.getInt(guam_args+8);
			guam_lbuff = pz390.mem.getInt(guam_args+12);
			guam_color = pz390.mem.getInt(guam_args+16);
			tput_buff.position(0);
			tput_buff.put(pz390.mem_byte,guam_abuff,guam_abuff+guam_lbuff);
			gz390.guam_screen_write(guam_row,guam_col,tput_buff,guam_lbuff,guam_color);
			break;
		case 3: // FIELD,row,col,length
			guam_row   = pz390.mem.getInt(guam_args);
			guam_col   = pz390.mem.getInt(guam_args+4);
			guam_lfield  = pz390.mem.getInt(guam_args+8);
			gz390.guam_screen_field(guam_row,guam_col,guam_lfield);
			break;
		case 4: // CURSOR,row,col,type
			guam_row   = pz390.mem.getInt(guam_args);
			guam_col   = pz390.mem.getInt(guam_args+4);
			guam_cursor_type  = pz390.mem.getInt(guam_args+8);
			gz390.guam_screen_cursor(guam_row,guam_col,guam_cursor_type);
			break;
		case 5: // COLOR,background rgb, text rgb
			guam_bg_rgb = pz390.mem.getInt(pz390.mem.getInt(guam_args) & pz390.psw_amode);
			guam_text_rgb = pz390.mem.getInt(pz390.mem.getInt(guam_args+4) & pz390.psw_amode);
			gz390.guam_screen_color(guam_bg_rgb,guam_text_rgb);
		    break;
		default:
			log_error(95,"undefined GUAM GUI Screen command - " + guam_minor);
		    pz390.reg.putInt(pz390.r15,8);
		}
		break;
	case 3: // GRAPH
		switch (guam_minor){
		case 1: // POINT,x,y,color
			guam_x     = pz390.mem.getInt(guam_args);
			guam_y     = pz390.mem.getInt(guam_args+4);
			guam_color = pz390.mem.getInt(guam_args+8);
			gz390.guam_graph_point(guam_x,guam_y,guam_color);
			break;
		case 2: // LINE,x1,y1,x2,y2,color
			guam_x     = pz390.mem.getInt(guam_args);
			guam_y     = pz390.mem.getInt(guam_args+4);
			guam_x2     = pz390.mem.getInt(guam_args+8);
			guam_y2     = pz390.mem.getInt(guam_args+12);
			guam_color = pz390.mem.getInt(guam_args+16);
			gz390.guam_graph_line(guam_x,guam_y,guam_x2,guam_x2,guam_color);
			break;
		case 3: // FILL,x1,y1,x2,y2,color
			guam_x     = pz390.mem.getInt(guam_args);
			guam_y     = pz390.mem.getInt(guam_args+4);
			guam_x2     = pz390.mem.getInt(guam_args+8);
			guam_y2     = pz390.mem.getInt(guam_args+12);
			guam_color = pz390.mem.getInt(guam_args+16);
			gz390.guam_graph_fill(guam_x,guam_y,guam_x2,guam_y2,guam_color);
			break;
		case 4: // TEXT,x,y,buff,lbuff,color
			guam_row   = pz390.mem.getInt(guam_args);
			guam_col   = pz390.mem.getInt(guam_args+4);
			guam_abuff = pz390.mem.getInt(guam_args+8);
			guam_lbuff = pz390.mem.getInt(guam_args+12);
			guam_color = pz390.mem.getInt(guam_args+16);
			tput_buff.position(0);
			tput_buff.put(pz390.mem_byte,guam_abuff,guam_lbuff);
			gz390.guam_screen_write(guam_row,guam_col,tput_buff,guam_lbuff,guam_color);
			break;
		default:
			log_error(96,"undefined GUAM GUI Graph command - " + guam_minor);
		    pz390.reg.putInt(pz390.r15,8);
		}
		break;
	case 4: // KEYBOARD
		switch (guam_minor){
		case 1: // READ,mod,char,WAIT/NOWAIT
			guam_key_amod = pz390.mem.getInt(guam_args);
			guam_key_achar = pz390.mem.getInt(guam_args+4);
			guam_wait = pz390.mem.getInt(guam_args+8) == 1;
			guam_key = gz390.guam_keyboard_read(guam_wait);
			if (guam_key != -1){
				pz390.mem.putInt(guam_key_amod,guam_key >> 8);
				pz390.mem.putInt(guam_key_achar,guam_key & 0xff);
			} else {
				pz390.reg.putInt(pz390.r15,4);
			}
			break;
		default:
			log_error(97,"undefined GUAM GUI Keyboard command - " + guam_minor);
		    pz390.reg.putInt(pz390.r15,8);
		}
		break;
	case 5: // MOUSE
		switch (guam_minor){
		case 1: // READ,x,y,left,right
			guam_x     = pz390.mem.getInt(guam_args);
			guam_y     = pz390.mem.getInt(guam_args+4);
			guam_left  = pz390.mem.getInt(guam_args+8);
			guam_right = pz390.mem.getInt(guam_args+12);
			guam_mouse = gz390.guam_mouse_read();
			pz390.mem.putInt(guam_x,guam_mouse[0]);
			pz390.mem.putInt(guam_x,guam_mouse[1]);
			pz390.mem.putInt(guam_left,guam_mouse[2]);
			pz390.mem.putInt(guam_right,guam_mouse[3]);
			break;
		default:
			log_error(98,"undefined GUAM GUI Mouse command - " + guam_minor);
		    pz390.reg.putInt(pz390.r15,8);
		}
	case 6: // SOUND
		switch (guam_minor){
		case 1: // PLAY,"wav_file"
			guam_abuff = pz390.mem.getInt(guam_args);
			guam_text = get_ascii_var_string(guam_abuff,max_lsn_spec);
			gz390.guam_sound_play(guam_text);
			break;
		default:
			log_error(99,"undefined GUAM GUI Sound command - " + guam_minor);
		    pz390.reg.putInt(pz390.r15,8);
		}
		if (tz390.z390_abort){
			abort_error(61,"GUAM GUI svc abort");
		}
	}
}
private void svc_snap(){
	/*
	 * snap dump control blocks and/or memory
	 *   r0 - flags
	 *     x'8000' - dump storage range (r14,r15)
	 *     x'4000' - dump gpr r0-r15
	 *     x'2000' - dump fpr f0-f15
	 *     x'1000' - dump cde program info
	 *     x'0800' - dump dcb file info
	 *     x'0400' - dump all memory
	 */
	int text_addr = pz390.reg.getInt(pz390.r1);
	if (text_addr > 0){
		put_log("SNAP DUMP ID=" + pz390.reg.getShort(pz390.r0+2)
			  + " TEXT=" + get_ascii_string(text_addr,60)
			  );
	} else {
		put_log("SNAP DUMP ID=" + pz390.reg.getShort(pz390.r0+2));
	}
	int flags = pz390.reg.getShort(pz390.r0);
	if ((flags & 0x4000) != 0){
		dump_gpr(-1);
	}
	if ((flags & 0x2000) != 0){
		dump_fpr(-1);
	}
	if ((flags & 0x1000) != 0){
		dump_cde();
	}
	if ((flags & 0x0800) != 0){
		dump_tiot();
	}
	if ((flags & 0x8000) != 0){
		int dump_addr = pz390.reg.getInt(pz390.r14) & pz390.psw_amode;
		int dump_len  = (pz390.reg.getInt(pz390.r15) & pz390.psw_amode) - dump_addr;
 		dump_mem(dump_addr,dump_len);
	} else if ((flags & 0x0400) != 0){ // RPI 583
		dump_mem(0,pz390.tot_mem);
	}
}
private void dump_cde_pgms(){
	/*
	 * dump cde entries for all loaded pgms and files RPI 583
	 */
	int index = 0;
	boolean first_line = true;
	while (index < tot_cde){
		if (cde_loc[index] != 0){
			if (first_line){
				first_line = false;
			} else {
				put_log("");
			}
			if (cde_ent[index] != -1){
				put_log(" CDE  PGM=" + cde_name[index]
				      + " ENT=" + tz390.get_hex(cde_ent[index],8)
				      + " LOC=" + tz390.get_hex(cde_loc[index],8)
		              + " LEN=" + tz390.get_hex(cde_len[index],8)
		              + " USE=" + tz390.get_hex(cde_use[index],8)
		              + tz390.newline); // RPI 500
			} else {
				put_log(" CDE  DSN=" + cde_name[index] 
				      + " LOC=" + tz390.get_hex(cde_loc[index],8)
				      + " LEN=" + tz390.get_hex(cde_len[index],8)
				      + " USE=" + tz390.get_hex(cde_use[index],8)
				      + tz390.newline); // RPI 500
			}
		}
		index++;
	}
}
private void dump_cde(){
	/*
	 * dump current program cde entries
	 */
	int index = 0;
	while (index < tot_cde){
		if (cde_loc[index] != 0){
			if (cde_ent[index] != -1){
				put_log(" CDE  PGM=" + cde_name[index]
				      + " ENT=" + tz390.get_hex(cde_ent[index],8)
		              + " LOC=" + tz390.get_hex(cde_loc[index],8)
		              + " LEN=" + tz390.get_hex(cde_len[index],8)
		              + " USE=" + tz390.get_hex(cde_use[index],8)
		              );
			} else {
				put_log(" CDE  DSN=" + cde_name[index]
            		  + " LOC=" + tz390.get_hex(cde_loc[index],8)
				      + " LEN=" + tz390.get_hex(cde_len[index],8)
				      + " USE=" + tz390.get_hex(cde_use[index],8)
				     );
			}
		}
		index++;
	}
}
private void dump_tiot(){
	/*
	 * dump content of tiot entries
	 */
	boolean any_found = false;
	int index = 0;
	String dsn;
	while (index < tot_tiot_files){
		if (tiot_dsn[index] != null){
			dsn = tiot_dsn[index];
		} else {
			dsn = "";
		}
		any_found = true;
		put_log(" TIOT  DDNAME=" + tiot_ddnam[index]
		      + " DCB=" + tz390.get_hex(tiot_dcb_addr[index],8)
		      + " DCBOFLGS=" + tz390.get_hex(pz390.mem_byte[tiot_dcb_addr[index] + dcb_oflgs] & 0xff,2)
		      + " DSN=" + dsn
	          );
		index++;
	}
	if (!any_found){
		put_log(" TIOT NO DCB ENTRIES FOUND");
	}
}
private void wto_msg(String wto_pfx,int msg_addr,int msg_len){
	/*
	 * 1.  Log msg on z390 system log
	 * 2.  If QUAM GUI option on, display msg
	 *     on gz390 mcs window view
	 *     
	 */
	String wto_msg = "";
	if (msg_len > 0){
		wto_msg = get_ascii_string(msg_addr,msg_len);
	}
	put_log(wto_pfx + wto_msg);
	if (tz390.opt_guam){
		gz390.guam_put_log(wto_msg);
		if (tz390.z390_abort){
			abort_error(62,"GUAM GUI put_log abort");
		}
	}
	pz390.reg.putInt(pz390.r15,0);  //RPI31
}
private void svc_wait(){
	/*
	 * wait for ecb posting or 
     * stimer exit request.  Retry the
     * wait after stimer exit assuming r1
     * restored to wait ecb parm.
	 * 
	 * Notes:
	 *   1.  WTOR ecb's are posted by 
	 *       gz390 wtor thread at which time
	 *       reply is fetched and stored.
	 *   2.  Don't wait on a user defined ecb
	 *       unless another process will post it
	 *       or an stimer exit will post it.
	 */
	if (!wait_retry){
		wait_count = pz390.reg.getInt(pz390.r0);
		wait_addr    = pz390.reg.getInt(pz390.r1) & pz390.psw_amode;
	}
    while (!check_wait_ecbs()  // RPI 280
    	&& !stimer_exit_request){
		sleep_now(tz390.monitor_wait);
    }
	if (stimer_exit_request){
		wait_retry = true;
		// backup for stimer exit retry
		pz390.psw_loc = pz390.psw_loc - 2;
		tz390.systerm_ins--;
	} else {
		wait_retry = false;
		pz390.reg.putInt(pz390.r15,0);
	    if (wait_count > 0){
	    	reset_wait_list();
	    }
	}
}
private boolean check_wait_ecbs(){
	/*
	 * check wait ecbs and return true if 
	 * required # of ecbs have been posted
	 */
	int ecb_code  = 0;
	if (wait_count == 0){
		ecb_code = pz390.mem.getInt(wait_addr);
		if ((ecb_code & ecb_posted) == 0){
			// if not posted set waiting bit
			pz390.mem.putInt(wait_addr,ecb_code | ecb_waiting);
		    return false;
		} else {
			return true;
		}
	}
	int ecb_count     = wait_count;
	int ecb_list_addr = wait_addr;
	int ecb_addr = pz390.mem.getInt(ecb_list_addr);
	int list_entry_count = 0;
	while (ecb_count > 0){
		list_entry_count++;
		ecb_code = pz390.mem.getInt(ecb_addr & 0x7fffffff);
		if ((ecb_code & ecb_posted) == 0){
			// if not posted set waiting bit
			pz390.mem.putInt(ecb_addr & 0x7fffffff,ecb_code | ecb_waiting);
		} else {
			ecb_count--;
			if (ecb_count == 0){
				return true;
			}
		}
		if (ecb_addr < 0){
			if (list_entry_count < wait_count){ // RPI 393
				pz390.set_psw_check(pz390.psw_pic_waiterr);
			}
			return false;
		}
		ecb_list_addr = ecb_list_addr + 4;
		ecb_addr = pz390.mem.getInt(ecb_list_addr);
	}
    return true;
}
private void reset_wait_list(){
	/*
	 * reset wait bit in ecblist
	 */
	int ecb_list_addr = wait_addr;
	int ecb_addr = pz390.mem.getInt(ecb_list_addr);
	int ecb_list_count = max_ecb_count;
	while (ecb_addr != 0 && ecb_list_count > 0){
		// turn off ecb waiting bit
		int ecb_code = pz390.mem.getInt(ecb_addr & 0x7fffffff);
		pz390.mem.putInt(ecb_addr & 0x7fffffff,ecb_code & 0x7fffffff);
		if (ecb_addr < 0){
			return;
		}
		ecb_list_addr = ecb_list_addr + 4;
		ecb_addr = pz390.mem.getInt(ecb_list_addr);
	    ecb_list_count--;
	}
	if (ecb_list_count == 0){  // RPI 398
		pz390.set_psw_check(pz390.psw_pic_waiterr);
	}
}
private void svc_post(){  // RPI 279
	/*
	 * post ecb complete
	 */
	int ecb_addr = pz390.reg.getInt(pz390.r1) & pz390.psw_amode;
	int ecb_code = pz390.reg.getInt(pz390.r0) & pz390.max_pos_int;
	pz390.mem.putInt(ecb_addr,(ecb_code | ecb_posted));
}
public void sleep_now(long mills){
	/*
	 * sleep for 1 monitor wait interval
	 */
	try {
		Thread.sleep(mills);
	} catch (Exception e){
		log_error(92,"thread sleep error - " + e.toString());
	}
}
private void svc_wtor(){
	/*
	 * request WTOR reply as follows:
	 * 1.  Save r0=reply, r14 length,r15=ecb 
	 * 2.  Issue wtor message
	 * 2.  if QUAM GUI option, check for gz390 cmd reply
	 *     else check for z390 cmd input reply 
	 * 3.  if reply found, post ecb else repeat check
	 *     at every monitor_update interval until
	 *     reply found.
	 */
	wto_fld = pz390.reg.getInt(pz390.r1) & pz390.psw_amode;
	wto_len = pz390.mem.getShort(wto_fld);
	wtor_reply_addr = pz390.reg.getInt(pz390.r0) & pz390.psw_amode;
	wtor_reply_len  = pz390.reg.getInt(pz390.r14) & 0xff;
	wtor_ecb_addr = pz390.reg.getInt(pz390.r15) & pz390.psw_amode;
	wto_msg("",wto_fld+4,wto_len-4);  //RPI190 remove "WTOR MSG"
	pz390.mem.putInt(wtor_ecb_addr,ecb_waiting); // ecb waiting for post by montior wtorit
	if (tz390.opt_guam){
    	gz390.wtor_request_reply(wtor_ecb_addr);
    }
	if (tz390.z390_abort){
		abort_error(63,"guam wtor reply abort");
	}
    wtor_reply_string  = null;
    wtor_reply_pending = true;
}
private void svc_espie(){
	/*
	 * set/reset program interruption exit
	 *  1.  if r0=0 cancel last espie added
	 *      else add new espie exit  RPI 305
	 *  2.  if r0 negative replace exit else add RPI 305
	 *  3.  if r1 not zero, save parm address  RPI 305
	 */
	int espie_pie   = pz390.reg.getInt(pz390.r0); // save pz390.psw_pic bit mask
	int espie_addr  = pz390.reg.getInt(pz390.r1) & pz390.psw_amode; // save exit psw addr
	int espie_param = pz390.reg.getInt(pz390.r15) & pz390.psw_amode; // save exit psw addr
	if (espie_addr == 0){
		if (pz390.tot_espie > 0){
			pz390.tot_espie--;
		}
		pz390.reg.putInt(pz390.r15,0);
	} else if (pz390.tot_espie < pz390.max_espie){
		if (espie_addr < 0){
			if (pz390.tot_espie == 0){
				pz390.tot_espie++;
			}
			espie_addr = espie_addr & 0x7fffffff;
		} else {
			pz390.tot_espie++;
		}
		pz390.espie_pie[pz390.tot_espie-1]  = espie_pie;
		pz390.espie_exit[pz390.tot_espie-1] = espie_addr;
		pz390.espie_parm[pz390.tot_espie-1] = espie_param;
		pz390.reg.putInt(pz390.r15,0);
	} else {
		pz390.set_psw_check(pz390.psw_pic_error);
	}
}
private void svc_estae(){
	/*
	 * set/reset task abend exit
	 *  1.  if r0=0 cancel last estae added
	 *      else add new estae exit  RPI 244
	 *  2.  if r0 negative replace exit else add RPI 244
	 *  3.  if r1 not zero, save parm address  RPI 244
	 */
	int estae_addr = pz390.reg.getInt(pz390.r0) & pz390.psw_amode;
	int estae_param = pz390.reg.getInt(pz390.r1) & pz390.psw_amode;
	if (estae_addr == 0){
		if (pz390.tot_estae > 0){
			pz390.tot_estae--;
		}
		pz390.reg.putInt(pz390.r15,0);
	} else if (pz390.tot_estae < pz390.max_estae){
		if (estae_addr < 0){
			if (pz390.tot_estae == 0){
				pz390.tot_estae++;
			}
			estae_addr = estae_addr & 0x7fffffff;
		} else {
			pz390.tot_estae++;
		}
		pz390.estae_exit[pz390.tot_estae-1] = estae_addr;
		pz390.estae_parm[pz390.tot_estae-1] = estae_param;
		pz390.reg.putInt(pz390.r15,0);
	} else {
		pz390.set_psw_check(pz390.psw_pic_error);
	}
}
private void svc_xlate(){
	/*
	 * translate between ascii/ebcdic
	 *   r0 = area address
	 *        high bit on  for EBCDIC to ASCII
	 *        high bit off for ASCII to EBCDIC
	 *   r1 = length
	 *   
	 */
	int index = pz390.reg.getInt(pz390.r1);
	int addr  = pz390.reg.getInt(pz390.r0);
	if (addr < 0){
		addr = addr & 0x7fffffff;
		while (index > 0){
			pz390.mem_byte[addr] = tz390.ebcdic_to_ascii[pz390.mem_byte[addr] & 0xff];
			addr++;
			index--;
		}
	} else {
		while (index > 0){
			pz390.mem_byte[addr] = tz390.ascii_to_ebcdic[pz390.mem_byte[addr] & 0xff];
			addr++;
			index--;
		}
	}
}
private void svc_ctd(){
	/*
	 * convert to display - r1=a(type,in,out)
	 *   conversion type code:
	 *     1 128 bit integer to 45 byte decimal  display
     *     2 EH short    to 45 byte scientific notation
     *     3 EB short    to 45 byte scientific notation
     *     4 DH long     to 45 byte scientific notation
     *     5 DB long     to 45 byte scientific notation
     *     6 LH extended to 45 byte scientific notation
     *     7 LB extended to 45 byte scientific notation
	 *     8 DD long     to 45 byte scientific notation
	 *     9 ED short    to 45 byte scientific notation
	 *    10 LD extended to 45 byte scientific notation
	 */
	int addr = pz390.reg.getInt(pz390.r1) & pz390.psw_amode;
	byte type = pz390.mem.get(addr+3);
	int addr_in  = pz390.mem.getInt(addr+4) & pz390.psw_amode; // RPI 526
	int addr_out = pz390.mem.getInt(addr+8) & pz390.psw_amode; // RPI 526
	switch (type){
	case 1: // 128 bit int to display
		if (addr_in >= 16){ // RPI 507
			pz390.mem.position(addr_in);
			pz390.mem.get(ctd_byte,0,16);
		} else {
			if ((addr_in & 1) == 0){ // RPI 512, RPI 513
				pz390.reg.position(addr_in * 8);
				pz390.reg.get(ctd_byte,0,16);
			} else {
				pz390.reg.putInt(pz390.r15,8);
			    return;
			}
		}
		ctd_bi = new BigInteger(ctd_byte);
		ctd_text = ctd_bi.toString();
		break;
	case 2: // eh 
		if (addr_in >= 16){ // RPI 507
			ctd_d = pz390.fp_get_db_from_eh(pz390.mem,addr_in); 
		} else {
		 	if (pz390.fp_reg_ctl[addr_in] != pz390.fp_ctl_ld){
			 	pz390.fp_store_reg(pz390.fp_reg,addr_in * 8);
			}
			ctd_d = pz390.fp_get_db_from_eh(pz390.fp_reg,addr_in * 8);
		}
        ctd_text = Double.toString(ctd_d);
        ctd_trunc(tz390.fp_eh_digits); 
        break;
	case 3: // eb
		if (addr_in >= 16){ // RPI 507
			ctd_e = pz390.fp_get_eb_from_eb(pz390.mem,addr_in); 
		} else {
		 	if (pz390.fp_reg_ctl[addr_in] != pz390.fp_ctl_ld){
			 	pz390.fp_store_reg(pz390.fp_reg,addr_in * 8);
			}
			ctd_e = pz390.fp_get_eb_from_eb(pz390.fp_reg,addr_in * 8);
		}
        ctd_text = Float.toString(ctd_e);
        ctd_trunc(tz390.fp_eb_digits); 
		break;
	case 4: // dh 
		if (addr_in >= 16){ // RPI 507
			ctd_d = pz390.fp_get_db_from_dh(pz390.mem,addr_in); 
		} else {
		 	if (pz390.fp_reg_ctl[addr_in] != pz390.fp_ctl_ld){
			 	pz390.fp_store_reg(pz390.fp_reg,addr_in * 8);
			}
			ctd_d = pz390.fp_get_db_from_dh(pz390.fp_reg,addr_in * 8);
		}
        ctd_text = Double.toString(ctd_d);
        ctd_trunc(tz390.fp_dh_digits); 
        break;
	case 5: // db
		if (addr_in >= 16){ // RPI 507
			ctd_d = pz390.fp_get_db_from_db(pz390.mem,addr_in); 
		} else {
		 	if (pz390.fp_reg_ctl[addr_in] != pz390.fp_ctl_ld){
			 	pz390.fp_store_reg(pz390.fp_reg,addr_in * 8);
			}
			ctd_d = pz390.fp_get_db_from_db(pz390.fp_reg,addr_in * 8);
		}
        ctd_text = Double.toString(ctd_d);
        ctd_trunc(tz390.fp_db_digits); 
        break;	
	case 6: // lh 
		if (addr_in >= 16){ // RPI 507
			ctd_bd = pz390.fp_get_bd_from_lh(pz390.mem,addr_in); 
		} else {
			if (!pz390.fp_pair_valid[addr_in]){ // RPI 512
				pz390.reg.putInt(pz390.r15,8);
			    return;
			}
		 	if (pz390.fp_reg_ctl[addr_in] != pz390.fp_ctl_ld){
			 	pz390.fp_store_reg(pz390.fp_reg,addr_in * 8);
			}
			ctd_bd = pz390.fp_get_bd_from_lh(pz390.fp_reg,addr_in * 8);
		}
        ctd_text = ctd_bd.toString();
        ctd_trunc(tz390.fp_lh_digits); 
        break;
	case 7: // lb
		if (addr_in >= 16){ // RPI 507
			ctd_bd = pz390.fp_get_bd_from_lb(pz390.mem,addr_in); 
		} else {
			if (!pz390.fp_pair_valid[addr_in]){ // RPI 512
				pz390.reg.putInt(pz390.r15,8);
			    return;
			}
		 	if (pz390.fp_reg_ctl[addr_in] != pz390.fp_ctl_ld){
			 	pz390.fp_store_reg(pz390.fp_reg,addr_in * 8);
			}
			ctd_bd = pz390.fp_get_bd_from_lb(pz390.fp_reg,addr_in * 8);
		}
        ctd_text = ctd_bd.toString();
        ctd_trunc(tz390.fp_lb_digits); 
        break;	
	case 8: // dd 
		if (addr_in >= 16){ // RPI 507
			if (!check_dfp_finite(pz390.mem_byte,addr_in)){ // RPI 536
				pz390.reg.putInt(pz390.r15,4);
			    return;
			}
			ctd_bd = pz390.fp_get_bd_from_dd(pz390.mem,addr_in); 
		} else {
		 	if (pz390.fp_reg_ctl[addr_in] != pz390.fp_ctl_ld){
			 	pz390.fp_store_reg(pz390.fp_reg,addr_in * 8);
			}
			if (!check_dfp_finite(pz390.fp_reg_byte,addr_in * 8)){ // RPI 536
				pz390.reg.putInt(pz390.r15,4);
			    return;
			}
			ctd_bd = pz390.fp_get_bd_from_dd(pz390.fp_reg,addr_in * 8);
		}
        ctd_text = ctd_bd.toString();
        ctd_trunc(tz390.fp_dd_digits); 
        break;
	case 9: // ed 
		if (addr_in >= 16){ // RPI 507
			ctd_bd = pz390.fp_get_bd_from_ed(pz390.mem,addr_in); 
		} else {
		 	if (pz390.fp_reg_ctl[addr_in] != pz390.fp_ctl_ld){
			 	pz390.fp_store_reg(pz390.fp_reg,addr_in * 8);
			}
			ctd_bd = pz390.fp_get_bd_from_ed(pz390.fp_reg,addr_in * 8);
		}
        ctd_text = ctd_bd.toString();
        ctd_trunc(tz390.fp_ed_digits); 
        break;
	case 10: // ld
		if (addr_in >= 16){ // RPI 507
			ctd_bd = pz390.fp_get_bd_from_ld(pz390.mem,addr_in); 
		} else {
			if (!pz390.fp_pair_valid[addr_in]){ // RPI 512
				pz390.reg.putInt(pz390.r15,8);
			    return;
			}
		 	if (pz390.fp_reg_ctl[addr_in] != pz390.fp_ctl_ld){
			 	pz390.fp_store_reg(pz390.fp_reg,addr_in * 8);
			}
			ctd_bd = pz390.fp_get_bd_from_ld(pz390.fp_reg,addr_in * 8);
		}
        ctd_text = ctd_bd.toString();
        ctd_trunc(tz390.fp_ld_digits); 
        break;	    
	default:
		pz390.reg.putInt(pz390.r15,8);
	    return;
	}
	int index = 0;
	while (index < ctd_text.length()){
		if (tz390.opt_ascii){  // RPI 477
			pz390.mem_byte[addr_out] = (byte)ctd_text.charAt(index);
		} else {
			pz390.mem.put(addr_out,tz390.ascii_to_ebcdic[ctd_text.charAt(index)]);
		}
		index++;
		addr_out++;
	}
	while (index < ctd_display_len){
		if (tz390.opt_ascii){ // RPI 477
			pz390.mem_byte[addr_out] = (byte)' ';
		} else {
			pz390.mem.put(addr_out,(byte)ebcdic_space);
		}
		index++;
		addr_out++;
	}
	pz390.reg.putInt(pz390.r15,0);
}
private void ctd_trunc(byte max_digits){
	/*
	 * trunc to max digits plus exponent
	 * and strip trailing zeros on fraction
	 */
    int d_index = ctd_text.indexOf(".");
    int e_index = ctd_text.indexOf("E");
    String e_text = "";
    if (d_index >= 0){
    	if (e_index > d_index){
        	if (ctd_text.charAt(e_index+1) == '+'){
        		e_text = "E" + ctd_text.substring(e_index+2);
        	} else {
        		e_text = ctd_text.substring(e_index);
        	}
    		d_index = e_index;
            if (d_index > max_digits+1){
               d_index = max_digits;  // last sig. digit offset
            } else {
               d_index = e_index-1;
            }
    	} else {
    		d_index = ctd_text.length()-1;
    	}
    	while (ctd_text.charAt(d_index) == '0'){
    		d_index--;
    	}
    	if (ctd_text.charAt(d_index) == '.'){
    		d_index--;
    	}
   		ctd_text = ctd_text.substring(0,d_index+1) + e_text;
    }
}
private void svc_cfd(){
	/*
	 * convert from display - r1=a(type,out,in)
	 *   conversion type code:
	 *     1 128 bit integer from 45 byte decimal  display
     *     2 EH short    from 45 byte scientific notation
     *     3 EB short    from 45 byte scientific notation
     *     4 DH long     from 45 byte scientific notation
     *     5 DB long     from 45 byte scientific notation
     *     6 LH extended from 45 byte scientific notation
     *     7 LB extended from 45 byte scientific notation
	 *     8 DD long     from 45 byte scientific notation 
	 *     9 ED short    from 45 byte scientific notation
	 *    10 LD extended from 45 byte scientific notation
	 */
	int addr = pz390.reg.getInt(pz390.r1) & pz390.psw_amode;
	byte type = pz390.mem.get(addr+3);
	int addr_out  = pz390.mem.getInt(addr+4) & pz390.psw_amode; // RPI 526
	int addr_in = pz390.mem.getInt(addr+8) & pz390.psw_amode;   // RPI 526
	String cfd_text = get_ascii_var_string(addr_in,ctd_display_len).trim();  
	switch (type){
	case 21: // 128 bit int from display
		try {
			cfd_bi = new BigDecimal(cfd_text).toBigInteger(); // RPI 512
		} catch (Exception e){
			pz390.reg.putInt(pz390.r15,12);
		    return;
		}
		if (addr_out >= 16){ // RPI 507
			pz390.mem.position(addr_out);
		} else {
			if ((addr_out & 1) == 0){ // RPI 512
				pz390.reg.position(addr_out * 8);
			} else { 
				pz390.reg.putInt(pz390.r15,8);
			    return;
			}
		}
		cfd_byte = cfd_bi.toByteArray();
		int index = 16-cfd_byte.length;
        byte fill_byte = 0;
		if (cfd_byte[0] < 0){
			fill_byte = -1;
		}
		while (index > 0){
			if (addr_out >= 16){  // RPI 507
				pz390.mem.put(fill_byte);
			} else {
				pz390.reg.put(fill_byte);
			}
			index--;
		}
		if (addr_out >= 16){
			pz390.mem.put(cfd_byte);
		} else {
			pz390.reg.put(cfd_byte);
		}
		break;
	case 22: // eh
		try {
			cfd_d = Double.valueOf(cfd_text); // RPI 526
		} catch (Exception e){
			pz390.reg.putInt(pz390.r15,12);
		    return;
		}
		
		if (addr_out >= 16){  // RPI 507
			pz390.mem.putInt(addr_out,pz390.zcvt_db_to_eh(cfd_d));
		} else {
			pz390.fp_reg.putInt(addr_out * 8,pz390.zcvt_db_to_eh(cfd_d));
			pz390.fp_reg_ctl[addr_out] = pz390.fp_ctl_ld;
		}
		break;
	case 23: // eb
		try {
			cfd_e = Float.valueOf(cfd_text);  // RPI 526
		} catch (Exception e){
			pz390.reg.putInt(pz390.r15,12);
		    return;
		}
		if (addr_out >= 16){  // RPI 507
			pz390.mem.putFloat(addr_out,cfd_e);
		} else {
			pz390.fp_reg.putFloat(addr_out *8,cfd_e);
			pz390.fp_reg_ctl[addr_out] = pz390.fp_ctl_ld;
		}
		break;
	case 24: // dh 
		try {
			cfd_d = Double.valueOf(cfd_text);  // RPI 526
		} catch (Exception e){
			pz390.reg.putInt(pz390.r15,12);
		    return;
		}		 
		if (addr_out >= 16){  // RPI 507
			pz390.mem.putLong(addr_out,pz390.zcvt_db_to_dh(cfd_d));
		} else {
			pz390.fp_reg.putLong(addr_out * 8,pz390.zcvt_db_to_dh(cfd_d));
			pz390.fp_reg_ctl[addr_out] = pz390.fp_ctl_ld;
		}
		break;
	case 25: // db
		try {
			cfd_d = Double.valueOf(cfd_text);  // RPI 526
		} catch (Exception e){
			pz390.reg.putInt(pz390.r15,12);
		    return;
		}
		if (addr_out >= 16){  // RPI 507
			pz390.mem.putDouble(addr_out,cfd_d);
		} else {
			pz390.fp_reg.putDouble(addr_out * 8,cfd_d);
			pz390.fp_reg_ctl[addr_out] = pz390.fp_ctl_ld;
		}
		break;	
	case 26: // lh 
		try {
			cfd_bd = new BigDecimal(cfd_text,pz390.fp_bdg_context);   // RPI 526
		} catch (Exception e){
			pz390.reg.putInt(pz390.r15,12);
		    return;
		}		
		pz390.zcvt_bd(tz390.fp_lh_type,cfd_bd);
		if (addr_out >= 16){  // RPI 507
			pz390.mem.position(addr_out);
			pz390.mem.put(tz390.fp_work_reg_byte,0,16);
		} else {
			if (pz390.fp_pair_valid[addr_out]){ // RPI 512
				pz390.fp_reg.position(addr_out * 8);
				pz390.fp_reg.put(tz390.fp_work_reg_byte,0,8);
				pz390.fp_reg_ctl[addr_out] = pz390.fp_ctl_ld;
				pz390.fp_reg.position(addr_out * 8 + 16);
				pz390.fp_reg.put(tz390.fp_work_reg_byte,8,8);
				pz390.fp_reg_ctl[addr_out + 2] = pz390.fp_ctl_ld;
			} else {
				pz390.reg.putInt(pz390.r15,8);
			    return;
			}
		}
		break;
	case 27: // lb
		try {
			cfd_bd = new BigDecimal(cfd_text,pz390.fp_bdg_context);   // RPI 526
		} catch (Exception e){
			pz390.reg.putInt(pz390.r15,12);
		    return;
		}
		pz390.zcvt_bd(tz390.fp_lb_type,cfd_bd);
		if (addr_out >= 16){  // RPI 507
			pz390.mem.position(addr_out);
			pz390.mem.put(tz390.fp_work_reg_byte,0,16);
		} else {
			if (pz390.fp_pair_valid[addr_out]){ // RPI 512
				pz390.fp_reg.position(addr_out * 8);
				pz390.fp_reg.put(tz390.fp_work_reg_byte,0,8);
				pz390.fp_reg_ctl[addr_out] = pz390.fp_ctl_ld;
				pz390.fp_reg.position(addr_out * 8 + 16);
				pz390.fp_reg.put(tz390.fp_work_reg_byte,8,8);
				pz390.fp_reg_ctl[addr_out + 2] = pz390.fp_ctl_ld;
			} else {
				pz390.reg.putInt(pz390.r15,8);
			    return;
			}
		}
		break;	
	case 28: // dd   RPI 514
		try {
			cfd_bd = new BigDecimal(cfd_text,pz390.fp_dd_context);   // RPI 526
		} catch (Exception e){
			pz390.reg.putInt(pz390.r15,12);
		    return;
		}
		pz390.zcvt_bd(tz390.fp_dd_type,cfd_bd);
		if (addr_out >= 16){  // RPI 507
			pz390.mem.position(addr_out);
			pz390.mem.put(tz390.fp_work_reg_byte,0,8);
		} else {
			pz390.fp_reg.position(addr_out*8);
			pz390.fp_reg.put(tz390.fp_work_reg_byte,0,8);
			pz390.fp_reg_ctl[addr_out] = pz390.fp_ctl_ld;
		}
		break;
	case 29: // ed  RPI 514
		try {
			cfd_bd = new BigDecimal(cfd_text,pz390.fp_ed_context);   // RPI 526
		} catch (Exception e){
			pz390.reg.putInt(pz390.r15,12);
		    return;
		}
		pz390.zcvt_bd(tz390.fp_ed_type,cfd_bd);
		if (addr_out >= 16){  // RPI 507
			pz390.mem.position(addr_out);
			pz390.mem.put(tz390.fp_work_reg_byte,0,4);
		} else {
			pz390.fp_reg.position(addr_out*8);
			pz390.fp_reg.put(tz390.fp_work_reg_byte,0,4);
			pz390.fp_reg_ctl[addr_out] = pz390.fp_ctl_ld;
		}
		break;	
	case 30: // ld   RPI 514
		try {
			cfd_bd = new BigDecimal(cfd_text,pz390.fp_ld_context);   // RPI 526
		} catch (Exception e){
			pz390.reg.putInt(pz390.r15,12);
		    return;
		}
		pz390.zcvt_bd(tz390.fp_ld_type,cfd_bd);
		if (addr_out >= 16){  // RPI 507
			pz390.mem.position(addr_out);
			pz390.mem.put(tz390.fp_work_reg_byte,0,16);
		} else {
			if (pz390.fp_pair_valid[addr_out]){ // RPI 512
				pz390.fp_reg.position(addr_out * 8);
				pz390.fp_reg.put(tz390.fp_work_reg_byte,0,8);
				pz390.fp_reg_ctl[addr_out] = pz390.fp_ctl_ld;
				pz390.fp_reg.position(addr_out * 8 + 16);
				pz390.fp_reg.put(tz390.fp_work_reg_byte,8,8);
				pz390.fp_reg_ctl[addr_out + 2] = pz390.fp_ctl_ld;
			} else {
				pz390.reg.putInt(pz390.r15,8);
			    return;
			}
		}
		break;	
	default:
		pz390.reg.putInt(pz390.r15,8);
	    return;
	}
	pz390.reg.putInt(pz390.r15,0);
}
public void process_test_cmd(){
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
	 *   1.  Get next command from z390 GUI
	 *       cmd line, system command_line, or
	 *       from ddname file specified in test
	 *       option test(ddname)..
	 *  
	 */
	if (test_break_addr_mode){
		check_test_break_addr();
	}
	if (test_break_reg_mode){
       check_test_break_reg();
	}
    if (test_break_mem_mode){
       check_test_break_mem();
    }
    if (test_break_op_mode && test_break_op_ins < tz390.systerm_ins){
    	check_test_break_op();
    }
    if (pz390.test_trace_count > 0){
    	pz390.test_trace_count--;
    	if (pz390.test_trace_count == 0){
    		pz390.trace_psw();
    	}
    }
    while (!tz390.z390_abort && pz390.test_trace_count == 0){
    	test_cmd_abort = false;
    	get_test_cmd();
    	if (!tz390.z390_abort 
    		&& test_cmd != null
    		&& test_cmd.length() > 0){  // RPI 98
    		test_loop_count = 0;
   		    exec_test_cmd();
    	} else {
    		test_loop_count++;
    		if (test_loop_count > 3){
    			abort_error(65,"test loop detected - aborting");
    		}
    	}
    }
}
private void get_test_cmd(){
	/*
	 * get next test command from Z390 GUI command line
	 * or system command line or file specified
	 * with the test(ddname) option
	 */
	if (!tz390.z390_abort && z390_command_text != null){
		try {
			z390_command_text.wait();
			test_cmd = z390_command_text.getText();
			z390_command_text.setText("");
		} catch (Exception e){
			test_error("i/o on z390 command line");
			test_cmd = "Q";  // quit now
		}
	} else {
		try {
			if (tz390.test_ddname == null
				&& test_loop_count == 0){ // RPI 98
				put_log("test enter command or h for help");
			}
			tz390.systerm_io++;
			test_cmd = test_cmd_file.readLine();
			if (tz390.test_ddname != null && test_cmd == null){
				test_cmd = "Q";
			}
		} catch (Exception e){
			test_error("i/o on command line");
			test_cmd = "Q";  // quit now
		}
	}
}
private void check_test_break_addr(){
	/*
	 * check for psw = break addr
	 */
	int index = 0;
	while (index < tot_test_break_addr){
		if (pz390.psw_loc == test_break_addr[index]){
			pz390.test_trace_count = 0;
			put_log("test break on " + test_break_addr_cmd);
			pz390.trace_psw();
			return;
		}
		index++;
	}
}
private void check_test_break_reg(){
	/*
	 * check for test mode break on register value
	 */
	test_break_reg_val = pz390.reg.getLong(test_break_reg_loc);
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
		test_error("invalid reg break compare - ignored");
    }
    if (test_break_reg){
    	pz390.test_trace_count = 0;
    	put_log("test break on " + test_break_reg_cmd);
    	dump_gpr(test_break_reg_loc);
    	pz390.trace_psw();
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
	    test_break_mem_byte = pz390.mem_byte[test_break_mem_loc + index];
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
        	test_error("invalid memory break compare - ignored");
	    }
	    index++;
    }
	if (test_break_mem_compare == 0  // = 
	   	&& test_break_mem_equal == test_break_mem_sdt.length){
	   	test_break_mem = true;
	}
	if (test_break_mem){
        pz390.test_trace_count = 0;
    	put_log("test break on " + test_break_mem_cmd);
        dump_mem(test_break_mem_loc,test_break_mem_sdt.length);
        pz390.trace_psw();
	}
}

private void check_test_break_op(){
	/*
	 * check for test mode break on opcode
	 * at current psw address
	 */
	if ((pz390.mem_byte[pz390.psw_loc] & 0xff) == test_break_op1){
		if (test_break_op2_index == 0
		    || ((pz390.mem_byte[pz390.psw_loc+test_break_op2_index] & test_break_op2_mask) 
				   == test_break_op2)){
		      pz390.test_trace_count = 0;
		      test_break_op_mode = false;
		      put_log("test break on " + test_break_op_cmd);
		      pz390.trace_psw();
		}
	}
}
private void exec_test_cmd(){
	/*
	 * parse and execute current test command
	 */
	if (test_cmd != null && test_cmd.length() > 0){
		put_log("test cmd: " + test_cmd);
	    test_match = test_pattern.matcher(test_cmd);
	}
	test_token = get_next_test_token();
	if (test_token == null){
		test_error("invalid command");
		return;
	}
	test_opcode = test_token.toUpperCase().charAt(0);
	switch (test_opcode){
	case '+': // relative base replacement +nn=
		test_token = get_next_test_token();
		test_addr = test_base_addr + get_next_test_addr();
		test_opcode = '=';
		break;
	case '-': // relative base replacement -nn=
		test_token = get_next_test_token();
		test_addr = test_base_addr - get_next_test_addr();
		test_opcode = '=';
		break;
	case 'E': // no preprocessing for exit request
		break;
	default:
		test_addr = 0;
		if (test_token.length() > 1
			|| (test_token.charAt(0) >= '0'
				&& test_token.charAt(0) <= '9')
			|| (test_token.charAt(0) == '*'
				&& (test_cmd.length() > 1
					&& test_cmd.indexOf("=") > 0))){
	       test_addr = get_next_test_addr();
	       if (test_token.equals("=")){
	    	   test_opcode = '=';
	       } else {
	    	   test_error("invalid command");
	    	   return;
	       }
		}
	}
	switch (test_opcode){
	case '*': // * with no = following is comment
		break;
	case '=': // addr=sdt or nr=sdt change
		if (!test_cmd_abort){
			test_sdt = get_next_test_token();
			if (test_addr_type == test_addr_mem){ // mem=sdt memory change
		        test_mem_loc = test_addr;
				test_mem_sdt = get_test_mem_sdt(test_sdt);
				int index = 0;
				while (index < test_mem_sdt.length){
					pz390.mem_byte[test_mem_loc+index] = test_mem_sdt[index];
				    index++;
				}
				dump_mem(test_mem_loc,test_mem_sdt.length);
			} else {  // nR=sdt gpr register change
		    	test_reg_loc = test_addr * 8;
				test_reg_sdt = get_test_reg_sdt(test_sdt);
				pz390.reg.putLong(test_reg_loc,test_reg_sdt);
				dump_gpr(test_reg_loc);
			}
		}
		break;
	case 'A':  // set address stop
		test_token = get_next_test_token();
		set_test_break_addr(get_next_test_addr());
		break;
	case 'B':  // set base for rel addr of memory
		test_token = get_next_test_token();
		if (test_token != null && test_token.charAt(0) == '='){
			test_token = get_next_test_token();
			if (test_token != null){
				test_base_addr = get_next_test_addr();
				dump_mem(test_base_addr,16);
				break;
			}
		} 
		test_error("invalide B=addr");
		break;
	case 'D': // dump tiot
    	dump_tiot();
        break;
	case 'E':  // capture exit request from batch and exit when done
	    if (test_token.length() > 1){ // RPI 586
	    	put_log("batch exit request");
	    	exit_request = true;
	    } else {  // RPI 586
	    	if (tz390.opt_ascii){
	    		tz390.opt_ascii = false;
	    		put_log("Setting EBCDIC text mode");
	    	} else {
	    		tz390.opt_ascii = true;
	    		put_log("Setting ASCII text mode");
	    	}
	    }
	    break;
	case 'F': // dump fp regs
		test_token = get_next_test_token();
		if (test_token == null){
			dump_fpr(-1);
		} else {
			dump_fpr(get_test_int(test_token) * 8); // RPI 490
		}
		break;
	case 'G':  // go nn instrs, to hex addr, or until reg/mem/op/addr break
       	tz390.opt_trace = false;
		go_test();
	    break;
	case 'H':  // help
	    put_log("z390 test command help summary (Visit www.z390.org for more information)");
	    put_log("  addr=sdt    set memory value  (ie 1r?=x'80' changes mem at (r1) 31 bit");
	    put_log("  reg=sdt     set register value (ie 15r=8 changes reg 15 to 8)");
	    put_log("  A addr      add/remove address stop (ie A FF348. or A *+4 etc.)");  // RPI 395
	    put_log("  B=addr      set base for rel addr (ie B=15r% sets base to (r15) 24 bit");
	    put_log("  D           display DCB file status, DDNAME, and DSNAME information");
	    put_log("  E           toggle EBCDIC/ASCII mode for dumping storage etc.");
	    put_log("  F nn        display specified floating point registers else all F0-FF");
	    put_log("  G nn/adr/op exec n instr. or to hex addr or until next break without trace");
	    put_log("  J addr      jump to new addr and trace instruction");
	    put_log("  L reg       list contents of register (ie l 1r dumps register 1");
	    put_log("  L addr len  list contents of memory area (ie l 10. 4 dumps cvt addr");
	    put_log("  M           display memory total allocated and free");
	    put_log("  P           display program information from CDE");
	    put_log("  R nn        display specified general purpose register else all R0-RF");
	    put_log("  S           clear all breaks");
	    put_log("  S reg??sdt  set break on register change");
	    put_log("  S addr??sdt set break on memory change");
	    put_log("  T nn/adr/op exec n instr. or to hex addr or until next break with trace");
	    put_log("  Z or Q      Z to zoom to normal end or Q to quit now");
	    put_log("* addr = [hex.|*|dec|nnr%(24)|nnr?(31)][+-addr]");
	    put_log("* reg  = nnr where nn = 0-15");
	    put_log("* sdt  = self defining term (b'01',c'ab',f'1',h'2',x'ff')");
	    put_log("* ??   = break compare operator (=,!=,<,<=,>,>=)");
        break;
	case 'J': // jump to address
		test_token = get_next_test_token();
		if (test_token != null){
			test_addr = get_next_test_addr();
			if (test_addr != -1){
				pz390.set_psw_loc(test_addr);
			} else {
				test_error("invalid jump address");
				return;
			}
			if (!test_cmd_abort){
				pz390.trace_psw();
			}
		} else {
			test_error("missing jump address");
		}
		break;
	case 'L':  // list reg or memory
		test_mem_len = 32;
		test_token = get_next_test_token();
		if (test_token == null){
			dump_gpr(-1);
			pz390.trace_psw();
		} else {
			test_bias = test_token.charAt(0);
			if (test_bias == '+' || test_bias == '-'){
				test_token = get_next_test_token();
				test_addr = get_next_test_addr();
				if (test_addr == -1 || test_base_addr == -1){
					test_error("invalid base or offset");
					return;
				}
				if (test_bias == '+'){
					test_addr = test_base_addr + test_addr;
				} else {
					test_addr = test_base_addr - test_addr;
				}
				if (test_token != null){
					test_mem_len = get_test_addr(test_token);
				}
			} else {
				test_addr = get_next_test_addr();
				if (test_token != null){
					test_mem_len = get_test_addr(test_token);
				}
			}
			if (test_addr == -1){
				test_error("invalid address");
				return;
			}
			if (test_addr_type == test_addr_reg){
				test_reg_loc = test_addr * 8;
				dump_gpr(test_reg_loc);
			} else if (test_addr_type == test_addr_mem){
				test_mem_loc = test_addr;
			    if (!test_cmd_abort){
			    	if (test_mem_len > 0
			    		&& test_mem_loc + test_mem_len <= pz390.tot_mem){
			    		dump_mem(test_mem_loc,test_mem_len);
			    	} else {
			    		test_error("invalid address or length");
			    		return;
			    	}
			    }
			}
		}
        break;
	case 'M': // show getmain/freemain memory stats
		dump_mem_stat();
		break;
	case 'P': // show CDE program info.
		dump_cde();
		break;
	case 'Q': // quit test mode
		abort_error(101,"quitting test mode"); //RPI121
		break;
	case 'R':  // dump gpr regs
		test_token = get_next_test_token();
		if (test_token == null){
			dump_gpr(-1);
		} else {
			dump_gpr(get_test_int(test_token) * 8); // RPI 490
		}
		break;
	case 'S':  // set break on reg or memory change
		test_token = get_next_test_token();
		if (test_token == null){
			put_log("test breaks off");
			test_break_reg_mode = false;
			test_break_mem_mode = false;
			test_break_op_mode = false;
			test_break_addr_mode = false;
		} else {
			test_addr    = get_next_test_addr();
			test_compare = get_test_compare(test_token);
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
       	tz390.opt_trace = true;
       	go_test();
	    break;
	case 'Z':  // zoom to normal end by turning off trace and test
        tz390.opt_trace = false;
        tz390.opt_test  = false;     //RPI186
        pz390.test_trace_count = -1; //RPI186
	    ez390_errors = 0;  // RPI 243
	    ez390_rc = 0;
        break;
	default:
		test_error("undefined test command - " + test_opcode);
	}
}
private int get_test_int(String token){// RPI 490
	/*
	 * return integer value of token
	 * and just issue ivalid int error if error
	 * and return -1.
	 */
	try {
		return Integer.valueOf(token).intValue();
	} catch (Exception e){
		test_error("invalid integer - " + token);
		return -1;
	}
}
private String get_next_test_token(){
	/*
	 * return next test command token or null
	 */
	if (test_match.find()){
	    return test_match.group();
	} else {
		return null;	
	}
}
private int get_next_test_addr(){
	/*
	 * return memory address of [addr][+-addr]
	 * start with current test_token and continue
	 * until next test_token not +,-, or valid address.
	 * If invalid return -1
	 * Notes:
	 *   1.  EPA returns last program load address
	 */
	if (test_token == null){
		return -1;
	}
	int total = 0;
	char test_sign = '+';
	while (test_sign == '+' || test_sign == '-'){	
		test_next_addr = get_test_addr(test_token);
		if (test_next_addr == -1){
			return -1;
		}
		if (test_sign == '+'){
			total = total + test_next_addr;
		} else {
			total = total - test_next_addr;
		}
	    test_token = get_next_test_token();
	    if (test_token == null){
	    	return total;
	    }
	    test_sign = test_token.charAt(0);
	    if (test_sign == '+' || test_sign == '-'){
	    	test_token = get_next_test_token();
	    	if (test_token == null){
	    		return -1;
	    	}
	    }
	}
	return total;
}
private void test_error(String text){
	/*
	 * issue test error message and return to prompt
	 */
	put_log("test error " + text);
}
private void go_test(){
	/*
	 * set count and go execute instructions
	 * until count 0 or break found or exit
	 */
	test_token = get_next_test_token();
	if (test_token != null){
		if (test_cmd.indexOf('*') >= 0
			|| test_cmd.indexOf('+') >= 0
			|| test_cmd.indexOf('-') >= 0
			|| (test_token.length() > 1 
				&& test_token.charAt(test_token.length()-1) == '.')
			){
			set_test_break_addr(get_next_test_addr());
			pz390.test_trace_count = -1; // go until break
		} else {
			try {
                pz390.test_trace_count = Integer.valueOf(test_token);
			} catch (Exception e){
                set_test_break_op();
                pz390.test_trace_count = -1; // go until break
			}
		}
	} else {
		pz390.test_trace_count = -1; // go until break or exit
	}
}
private void set_test_break_addr(int addr){
	/*
	 * set break on specified instruction 
	 * address
	 */
	test_break_addr_mode = true;
	int index = 0;
	while (index < tot_test_break_addr){
		if (test_break_addr[index] == addr){
			put_log("test break addr removed - " + tz390.get_hex(addr,8));
			tot_test_break_addr--;
			if (index < tot_test_break_addr){
				test_break_addr[index] = test_break_addr[tot_test_break_addr]; 
			}
			return;
		}
		index++;
	}
	if (tot_test_break_addr == test_break_addr.length){
		test_error("max addr breaks exceeded - remove one or clear");
	}
	test_break_addr_cmd  = test_cmd;
	test_break_addr[tot_test_break_addr] = addr & 0x7fffffff; // RPI 428
	tot_test_break_addr++;
	dump_mem(test_break_addr[tot_test_break_addr-1],16);
}
private void set_test_break_reg(){
	/*
	 * set test break on register change
	 */
	test_break_reg_mode = true;
	test_break_reg_cmd = test_cmd;
	test_break_reg_loc = test_addr * 8;
    test_break_reg_compare = test_compare;
    test_sdt = get_next_test_token();
	test_break_reg_sdt = get_test_reg_sdt(test_sdt);
	dump_gpr(test_break_reg_loc);
}
private void set_test_break_mem(){
	/*
	 * set test break on memory change
	 */
	test_break_mem_mode = true;
	test_break_mem_cmd = test_cmd;
	test_break_mem_loc = test_addr;
    test_break_mem_compare = test_compare;
    test_sdt = get_next_test_token();
    if (test_sdt != null){
    	test_break_mem_sdt = get_test_mem_sdt(test_sdt);			
    	dump_mem(test_break_mem_loc,test_break_mem_sdt.length);
    } else {
    	test_error("missing sdt for break");
    	test_cmd_abort = true;
    }
}
private void set_test_break_op(){
	/*
	 * set break on opcode at current psw
	 */
	int index = tz390.find_key_index('O',test_token.toUpperCase());
	if (index != -1){
		test_break_op_mode = true;
		test_break_op_ins  = tz390.systerm_ins;
		test_break_op_cmd  = test_cmd;
		try { // RPI 490
			test_break_op1 = Integer.valueOf(tz390.op_code[index].substring(0,2),16).byteValue() & 0xff;
		} catch (Exception e){
			test_error("invalid hex opcode for " + test_token);
			test_break_op1 = 0;
		}
		test_break_op2_index = pz390.opcode2_offset[test_break_op1];
		if (tz390.op_code[index].length() == 4){
			try {
				test_break_op2 = Integer.valueOf(tz390.op_code[index].substring(2,4),16).byteValue() & 0xff;
			} catch (Exception e){
				test_error("invalid hex opcode 2 for " + test_token);
				test_break_op2 = 0;
			}
			test_break_op2_mask = 0xff;
		} else if (tz390.op_code[index].length() == 3){
			if (pz390.opcode2_mask[test_break_op1] == 0xf0){
			    try {
			    	test_break_op2 = (Integer.valueOf(tz390.op_code[index].substring(2,3),16).intValue() << 4) & 0xff;
			    } catch (Exception e){
			    	test_error("invalid hex opcode2 for " + test_token);
			    	test_break_op2 = 0;
			    }
			    test_break_op2_mask = 0xf0;
			} else {
				try {
					test_break_op2 = Integer.valueOf(tz390.op_code[index].substring(2,3),16).intValue() & 0xff;
				} catch (Exception e){
					test_error("invald hex opcode2 for " + test_token);
					test_break_op2 = 0;
				}
			    test_break_op2_mask = 0x0f;
			}
		} else {
			test_break_op2_index = 0; // no op2
		}
	} else {
		test_error("invalid opcode - " + test_token);
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
	 *   *    = pz390.psw_loc
	 *   nr% indirect 24 bit 
	 *   nr? indirect 31 bit
	 *   EPA last load address  // RPI 395
	 * register type address forms
	 *    nnr or rnn
	 */
	int addr = 0;
	test_addr_type = test_addr_mem;
	try {
		if (text.length() > 1){
			if (text.toUpperCase().charAt(text.length()-1) == 'R'){
				test_addr_type = test_addr_reg;
				addr = Integer.valueOf(text.substring(0,text.length()-1)).intValue();
			} else if (text.charAt(text.length()-1) == '.'){
				addr = Long.valueOf(text.substring(0,text.length()-1),16).intValue() & 0xffffffff;
			} else if (text.charAt(0) == '+'){
				addr = test_base_addr + Long.valueOf(text.substring(1),16).intValue() & 0xffffffff;
			} else if (text.charAt(0) == '-'){
				addr = test_base_addr - Long.valueOf(text.substring(1),16).intValue() & 0xffffffff;
			} else if (text.length() > 2 && text.substring(0,2).equals("*+")){
				addr = pz390.psw_loc + Long.valueOf(text.substring(2),16).intValue() & 0xffffffff;
			} else if (text.substring(0,2).equals("*-")){
				addr = pz390.psw_loc - Long.valueOf(text.substring(2),16).intValue() & 0xffffffff;
			} else if (text.toUpperCase().charAt(text.length()-1) == '%'){
				int index_r = text.toUpperCase().indexOf('R');
				int index_p = text.length()-2;
				addr = (pz390.reg.getInt(get_test_int(text.substring(0,index_r))*8+4)) & pz390.psw_amode24;
				while (text.charAt(index_p) == '%'){
				    addr = pz390.mem.getInt(addr) & pz390.psw_amode24;
				    index_p--;
				}
			} else if (text.toUpperCase().charAt(text.length()-1) == '?'){
				int index_r = text.toUpperCase().indexOf('R');
				int index_q = text.length()-2;
				addr = (pz390.reg.getInt(get_test_int(text.substring(0,index_r))*8+4)) & pz390.psw_amode31;
				while (text.charAt(index_q) == '?'){
				    addr = pz390.mem.getInt(addr) & pz390.psw_amode31;
				    index_q--;
				}
			} else if (text.toUpperCase().equals("EPA")){
				addr = load_code_load & pz390.psw_amode;  // RPI 395
			} else {
				addr = get_test_int(text);
			}
		} else if (text.charAt(0) == '*'){
			addr = pz390.psw_loc;
		} else { // assume single digit
			addr = get_test_int(text);
		}
	} catch (Exception e){
		test_error("invalid addr - " + text);
		test_cmd_abort = true;
	}
	if (addr >= 0 && addr < pz390.tot_mem){
		return addr; // RPI 540
	} else {
		return -1;
	}
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
	if (compare != null){
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
	}
	test_error("invalid break compare operator - " + test_cmd);
	test_cmd_abort = true;
	return -1;
}
private long get_test_reg_sdt(String text){
	/*
	 * return long sdt value for register
	 *    b'...'
	 *    c'...'
	 *    c"..."
	 *    f'...'
	 *    h'...'
	 *    x'...'
	 *    or address hex., dec, nr%, nr?,
	 *       +hex, -hex, *+hex, *-hex
	 */
	if (text == null || text.length() == 0){
		test_cmd_abort = true;
		return -1;
	}
	if (text.length() <= 1 
		|| (text.charAt(1) != '\''
			&& text.charAt(1) != '"')){
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
				if (tz390.opt_ascii || text.charAt(1)== '"'){
					value = value * 0x100 + data.charAt(index);
				} else {
					value = value * 0x100 + tz390.ascii_to_ebcdic[data.charAt(index)];
				}
				index++;
				if (index < dcc_len 
					&& data.charAt(index) == text.charAt(1)){
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
	 *    c"..."
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
						&& data_text.charAt(index1) == text.charAt(1)){
					index1++; // skip 2nd single quote
				}
			}
			data_byte_len = index2;
			data_byte = new byte[data_byte_len];
			index1 = 0;
			index2 = 0;
			while (index1 < data_len){
				if (tz390.opt_ascii || text.charAt(1)=='"'){
					data_byte[index2] = (byte)data_text.charAt(index1);
				} else {
					data_byte[index2] = tz390.ascii_to_ebcdic[data_text.charAt(index1)];
				}
				index1++;
				index2++;
				if (index1 < data_len 
						&& data_text.charAt(index1) == text.charAt(1)){
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
				try {
					data_byte[index] = Integer.valueOf(data_text.substring(index*2,index*2+2),16).byteValue();
				} catch (Exception e){
					test_error("invalid hex self defining term " + data_text);
					data_byte[index] = 0;
				}
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
public void init_sz390(tz390 shared_tz390,pz390 shared_pz390){
	/*
	 * init tz390
	 */
	tz390 = shared_tz390;
	pz390 = shared_pz390;
}
public long get_feature_bits(){
	/*
	 * return current os feature bits for use by STFLE instruction
	 * byte  bit
	 *   0   0 - Y zos and 390 instructions avail.
	 *       1 - Y zos mode installed
	 *       2 - Y zos mode active
	 *       7 - Y STFLE facility installed
	 *   2  16 - N extended translation 2 
	 *      18 - N long displacement         
	 *      19 - N long displacement performance
	 *      20 - Y HFP multiply and add/subtract
	 *      21 - Y extended immediate
	 *      22 - N extended translation 3
	 *      23 - Y HFP unnormailized
	 *   3  24 - Y extended timer 2
	 *      25 - Y store clock fast
	 *      28 - N extended TOD steering
	 *      30 - N extended timer 3
	 */
	int bits0_31 = 0xE1000DC0;
	return ((long)bits0_31) << 32;
}
private void svc_tcpio(){
	/*
	 * tcp/ip sockets I/O
	 * Inputs:
	 *   r0 = operation
	 *      1 - open server port
	 *            r1=port
	 *      2 - open client port connection
	 *            r1=port
	 *            r14=host ip addr or 0 (HOST=*)
	 *      3 - close port connection
	 *            r1=port
	 *      4 - send message
	 *            r1=port
	 *            r2=connection id
	 *            r14=msg addr
	 *            r15=msg length
	 *      5 - receive message 
	 *            r1=port
	 *            r2=connection id or -1
	 *            r14=buffer address
	 *            r15=max msg length
     * Output:
     *   r1 = message length for receive
     *   r2 = connection id for receive
     *   r15= return code
     *         0 - ok
     *         4 - no msg and nowait
     *         12- error on last operation
	 */
	tcpio_op    = pz390.reg.getShort(pz390.r0+2);
	tcpio_flags = pz390.reg.getShort(pz390.r0);
	tcpio_wait =  (tcpio_flags & 0x0001) == 0; // set wait true if NOWAIT bit off
	tcpio_port  = pz390.reg.getInt(pz390.r1);
	tcpio_conn  = pz390.reg.getInt(pz390.r2);
	tcpio_amsg  = pz390.reg.getInt(pz390.r14) & pz390.psw_amode;
	tcpio_lmsg  = pz390.reg.getInt(pz390.r15);
	pz390.reg.putInt(pz390.r15,0);
	tot_tcpio_oper++;
	switch (tcpio_op){
	case 1: // open server port
		tot_tcpio_opens++;
		tcpio_server_running = true; // enable tcpio server threads
		if (tz390.opt_tracet){
			put_log("TCPIO open server port " + tcpio_port);
		}
		if (!tcpio_find_server_port() 
			&& cur_tcp_server_index > -1){ 
			tcp_server_port[cur_tcp_server_index] = tcpio_port;
			if (tcp_server_socket[cur_tcp_server_index] != null
				&& !tcp_server_socket[cur_tcp_server_index].isClosed()){				
				put_log("TCPIO server port alrady open");
				break; // ignore if already open
			}
		} else {
			put_log("TCPIO open server failed - no ports available");
           	pz390.reg.putInt(pz390.r15,12);
			break;
		}
       	try {
       		tcpio_host_ip = InetAddress.getLocalHost();
       		tcpio_host_text = tcpio_host_ip.getHostAddress();      		
       	} catch (Exception e){
           	put_log("TCPIO error on open get local host failed");
           	pz390.reg.putInt(pz390.r15,12);
           	break;
       	}
       	tcp_server_host_text[cur_tcp_server_index] = tcpio_host_text;
       	tcp_server_host_ip[cur_tcp_server_index] = tcpio_host_ip;
       	tcp_server_port[cur_tcp_server_index] = tcpio_port;
       	if (tz390.opt_tracet){
    		put_log("TCPIO open server socket" 
    				+ " host=" + tcpio_host_text
    				+ " port=" + tcpio_port);
    	}
    	try {
    		tcp_server_socket[cur_tcp_server_index] = new ServerSocket(tcpio_port);
    		tcp_server_thread[cur_tcp_server_index] = new Thread(this);
    		tcp_server_thread[cur_tcp_server_index].start();
    	} catch (Exception e){
    		put_log("TCPIO error open server socket " + e.toString());
    		pz390.reg.putInt(pz390.r15,12);
    	}
		break;
	case 2: // open client connection to server port
		tot_tcpio_openc++;
		if (tz390.opt_tracet){
			put_log("TCPIO open client port " + tcpio_port);
		}
		if (tcpio_find_client_port()){
			if (tcp_client_socket[cur_tcp_client_index] != null
				&& !tcp_client_socket[cur_tcp_client_index].isClosed()){
				put_log("TCPIO open client port already open");
				break; // ignore if already open
			}
		} else if (cur_tcp_client_index != -1){
            tcp_client_port[cur_tcp_client_index] = tcpio_port;
			tcpio_host_addr = pz390.reg.getInt(pz390.r14) & pz390.psw_amode;
			if (tcpio_host_addr > 0){
				tcpio_host_text = get_ascii_var_string(tcpio_host_addr,265);
				try {
					tcpio_host_ip   = InetAddress.getByName(tcpio_host_text);
				} catch(Exception e) {
					put_log("TCPIO error open client host not found " + tcpio_host_text);
					pz390.reg.putInt(pz390.r15,12);
					break;
				}
			} else {
				try {
					tcpio_host_ip   = InetAddress.getLocalHost();
					tcpio_host_text = tcpio_host_ip.getHostAddress();
				} catch(Exception e) {
					put_log("TCPIO error open client get host failed");
					pz390.reg.putInt(pz390.r15,12);
					break;
				}
			}
			tcp_client_host_text[cur_tcp_client_index] = tcpio_host_text;
			tcp_client_host_ip[cur_tcp_client_index] = tcpio_host_ip;
			if (tz390.opt_tracet){
				put_log("TCPIO open client"
					  + " HOST=" + tcpio_host_text 
					  + " PORT=" + tcpio_port);
			}
			try {
				tcp_client_socket[cur_tcp_client_index] = new Socket(tcpio_host_ip, tcpio_port);
				tcp_client_input[cur_tcp_client_index]  = new DataInputStream(tcp_client_socket[cur_tcp_client_index].getInputStream());
				tcp_client_output[cur_tcp_client_index] = new PrintStream(tcp_client_socket[cur_tcp_client_index].getOutputStream());
			} catch (Exception e){
				put_log("TCPIO error open client socket failed for port " + tcpio_port);
				pz390.reg.putInt(pz390.r15,12);
			}
		} else {
           	put_log("TCPIO error max client ports exceeded");
           	pz390.reg.putInt(pz390.r15,12);	
		}
		break;
	case 3: // close port
		if (tz390.opt_tracet){
			put_log("TCPIO close port" + tcpio_port);
		}
		if (tcpio_find_client_port()){
			tot_tcpio_closec++;
			tcpio_close_client_port();
		} else if (tcpio_find_server_port()){
			tot_tcpio_closes++;
			tcpio_close_server_port();
		}
		break;
	case 4: // send message
		tot_tcpio_send++;
		if (tcpio_lmsg < tcpio_lmin
			|| tcpio_lmsg > tcpio_lmax){
			put_log("TCPIO send error msg length out of range " + tcpio_lmsg);
			pz390.reg.putInt(pz390.r15,12);
			break;
		}
		if (tcpio_find_client_port()){
			if (tcp_client_output[cur_tcp_client_index] != null){
				tcp_client_output[cur_tcp_client_index].write(pz390.mem_byte,tcpio_amsg,tcpio_lmsg);
				if (tz390.opt_tracet){
					put_log("TCPIO send port=" + tcpio_port 
							     + " length=" + tcpio_lmsg);
					dump_mem(tcpio_amsg,tcpio_lmsg);
				}
			} else {
				put_log("TCPIO errpr semd failed for port=" + tcpio_port);
				pz390.reg.putInt(pz390.r15,12);
				break;
	        }
		} else if (tcpio_find_server_port()){
			int conn_index = pz390.reg.getInt(pz390.r2);
			if (conn_index >= 0 
				&& conn_index < max_tcp_conn
				&& tcp_conn_server_port[conn_index] == tcpio_port){
				if (tcp_conn_output[conn_index] != null){
					tcp_conn_output[conn_index].write(pz390.mem_byte,tcpio_amsg,tcpio_lmsg);
					if (tz390.opt_tracet){
						put_log("TCPIO send port=" + tcpio_port 
							  + " conn=" + conn_index	
							  + " length=" + tcpio_lmsg);
						dump_mem(tcpio_amsg,tcpio_lmsg);
					}
				} else {
					put_log("TCPIO error send failed on port=" + tcpio_port);
					pz390.reg.putInt(pz390.r15,12);
					break;
				}
			} else {
				put_log("TCPIO error send port not found " + tcpio_port);
				pz390.reg.putInt(pz390.r15,12);
				break;
			}
		}
		break;
	case 5: // receive message
		tot_tcpio_recv++;
		if (tz390.opt_tracet){
			put_log("TCPIO receive msg for port=" + tcpio_port);
		}
		if (tcpio_lmsg < tcpio_lmin
			|| tcpio_lmsg > tcpio_lmax){
			put_log("TCPIO receive error msg length out of range " + tcpio_lmsg);
			pz390.reg.putInt(pz390.r15,12);
			break;
		}
		if (tcpio_find_client_port()){
			tcpio_receive_client_port();
		} else if (tcpio_find_server_port()){
			tcpio_receive_server_port();
		} else {
			put_log("TCPIO error receive port not found " + tcpio_port);
			pz390.reg.putInt(pz390.r15,12);
			break;
		}		
		break;
    default:
    	put_log("TCPIO error invalid operation " + tcpio_op);
    	pz390.set_psw_check(pz390.psw_pic_spec);			
	}
}
private boolean tcpio_find_server_port(){
	/*
	 * set cur_tcp_server_index to allocated
	 * server port and return true else
	 * set cur_tcp_server_index to first free port
	 * and return true else
	 * set cur_tcp_server_index to -1
	 * and return false indicating no ports avail.
	 * 
	 */
	cur_tcp_server_index = 0;
	int free = -1;
	while (cur_tcp_server_index < max_tcp_server_port){
		if (tcp_server_port[cur_tcp_server_index] == tcpio_port){
			return true;
		} else if (free == -1 && tcp_server_port[cur_tcp_server_index] == 0){
            free = cur_tcp_server_index;			
		}		
		cur_tcp_server_index++;
	}
	cur_tcp_server_index = free;
	return false;
}
private boolean tcpio_find_client_port(){
	/*
	 * set cur_tcp_client_index to allocated
	 * client port and return true else 
	 * set cur_tcp_client_index to first free
	 * client port and return false else 
	 * set cur_tcp_client index to -1 and return false
	 */
	int cur_tcp_client_index = 0;
	int free = -1;
	while (cur_tcp_client_index < max_tcp_client_port){
		if (tcp_client_port[cur_tcp_client_index] == tcpio_port){
			return true;
		} else if (free == -1 && tcp_client_port[cur_tcp_client_index] == 0){
			free = cur_tcp_client_index;
		}
		cur_tcp_client_index++;
	}
	cur_tcp_client_index = free;
	return false;
}
private void tcpio_close_ports(){
	/*
	 * close all client and server ports
	 */
	tcpio_server_running = false; // shut down any server threads
	cur_tcp_client_index = 0;
	while (cur_tcp_client_index < max_tcp_client_port){
		if (tcp_client_port[cur_tcp_client_index] > 0){
			tcpio_close_client_port();
		}
		cur_tcp_client_index++;
	}
	cur_tcp_server_index = 0;
	while (cur_tcp_server_index < max_tcp_server_port){
		if (tcp_server_port[cur_tcp_server_index] > 0){
			tcpio_close_server_port();
		}
		cur_tcp_server_index++;
	}
}
private void tcpio_close_client_port(){
	/*
	 * close all open TCP/IP ports
	 */
	try {
		if (tz390.opt_tracet){
			put_log("TCPIO closing client port" + tcp_client_port[cur_tcp_client_index]);
		}
		tcp_client_output[cur_tcp_client_index].flush();
		tcp_client_output[cur_tcp_client_index].close();
		tcp_client_input[cur_tcp_client_index].close();
		tcp_client_socket[cur_tcp_client_index].close();
		tcp_client_socket[cur_tcp_client_index] = null;
		tcp_client_port[cur_tcp_client_index] = 0;
	} catch (Exception e){
		put_log("TCPIO error closing client port " + tcpio_port);
	}
}
private synchronized void tcpio_close_server_port(){
	/*
	 * close open TCP/IP server port
	 */
	if (tcp_server_socket[cur_tcp_server_index] != null){
		try {
			if (tz390.opt_tracet){
				put_log("TCPIO closing server port=" + tcpio_port);
			}
			int conn_index = 0;
			while (conn_index < max_tcp_conn){
				if (tcp_conn_server_port[conn_index] == tcp_server_port[cur_tcp_server_index]){
                   tcpio_close_conn(conn_index);
				}
				conn_index++;
			}
			tcp_server_socket[cur_tcp_server_index].close();
			tcp_server_socket[cur_tcp_server_index] = null;
		} catch (Exception e){
			put_log("TCPIO error on closing server port=" + tcpio_port);
		}		
	}
}
private synchronized void tcpio_close_conn(int conn_index){
	/*
	 * close connection
	 */
	if (tcp_conn_socket[conn_index] == null){
		return;
	}
	try {
		if (tz390.opt_tracet){
			put_log("TCPIO closing connection " + conn_index);
		}
		tcp_conn_output[conn_index].close();
		tcp_conn_input[conn_index].close();
		tcp_conn_socket[conn_index].close();
		tcp_conn_socket[conn_index] = null;
		tcp_conn_server_port[conn_index] = 0;
	} catch (Exception e){
		put_log("TCPIO close connection failed " + e.toString());
	}
}
private void tcpio_receive_client_port(){
	/* 
	 * receive message from client port
	 * if nowait and message not ready RC=4
	 * else wait for message
	 */
	try {
		int cur_msg_len = tcp_client_input[cur_tcp_client_index].available();
		if (cur_msg_len > 0
			|| tcpio_wait){
			if (tz390.opt_tracet && cur_msg_len == 0){
				put_log("TCPIO waiting for client msg on port=" + tcpio_port);
			}
			cur_msg_len = tcp_client_input[cur_tcp_client_index].read(pz390.mem_byte,tcpio_amsg,tcpio_lmsg);
			if (cur_msg_len <= 0){
				throw new RuntimeException("TCPIO error on client receive port=" + tcpio_port);
			}
			if (tz390.opt_tracet){
				put_log("TCPIO receive client port=" + tcpio_port 
						     + " length=" + cur_msg_len);
				dump_mem(tcpio_amsg,cur_msg_len);
			}
			pz390.reg.putInt(pz390.r1,cur_msg_len);
			return; // return with msg stored in mem
		} else {
			pz390.reg.putInt(pz390.r15,4);
			pz390.reg.putInt(pz390.r1,0);
			return; // return RC=4 for NOWAIT
		}
	} catch (Exception e){
		put_log("TCPIO receive error on client port=" + tcpio_port);
		pz390.reg.putInt(pz390.r15,12);
	}
}
private void tcpio_receive_server_port(){
	/* 
	 * receive message from server
	 * port connection.
	 * If no connection id is specified (-1),
	 * then next message from any conncetion.
	 * if nowait and message not ready RC=4
	 * else wait for next message..
	 * Notes:
	 *   1. Connection # returned in R2
	 */
	if (tz390.opt_traceall){
		put_log("TCPIO receive msg from port=" + tcpio_port);
	}
	int conn_index = tcpio_conn;
	if (tcpio_conn == -1){
		conn_index = 0; // start search at conn 0
	}
	tcpio_conn_ready_count = 0; // count ready connections during scan
	while (tcpio_server_running 
			&& (conn_index < max_tcp_conn 
			    || tcpio_wait)
		  ){
		if (tz390.opt_traceall){
			put_log("TCPIO check receive conn=" + conn_index);
		}		
		try {
			if (tcp_conn_server_port[conn_index] > 0
				&& tcp_conn_msg_ready[conn_index]
				){
				// this connection has msg ready
				// 	so store it and exit 
				tcpio_conn_store_msg(conn_index);
				return;
			}
			if (tcpio_conn == -1){
				// find next message 
				// from any connection
				conn_index++;
				if (conn_index >= max_tcp_conn
					&& tcpio_wait){
					// After checking all connections
					// for any pending messages,
					// wait here for msg ready post
					// from live connection thread
					// and then proceed to check again
                    lock.lock();
                    try {
                    	if (tcpio_conn_ready_count == 0){
                    		lock_condition.await();
                    	}
                    } catch(Exception e){
                    	if (tz390.opt_traceall){
                    		put_log("TCPIO error waiting for server message on any conn");
                    	}
                    } finally {
                    	lock.unlock();
                    }
                    tcpio_conn_ready_count = 0;
					conn_index = 0;
				}
			} else if (tcpio_wait){
				// no msg ready from specific connection
                // so wait for post from connection and try again
                try {
               		lock_condition.await();
                } catch(Exception e){
                	if (tz390.opt_traceall){
                		put_log("TCPIO error waiting for server message on conn=" + tcpio_conn);
                	}
                } finally {
                	lock.unlock();
                }				
			} else {
				// force nowait exit with conn=-1
				conn_index = max_tcp_conn; 
			}
		} catch (Exception e){
    		put_log("TCPIO error checking conn msg available on conn=" + conn_index + " - " + e.toString());
    		tcpio_close_conn(conn_index);     
		}
	}
	pz390.reg.putInt(pz390.r1,0);  // return 0 msg length
	pz390.reg.putInt(pz390.r2,-1); // return -1 conn index (none)
	pz390.reg.putInt(pz390.r15,4); // exit RC=4 NOWAIT and no msg ready
}
private void tcpio_conn_store_msg(int conn_index){
	/*
	 * 1.  store msg from conn input buffer
	 *     up to specified lmsg length
	 *     and return actual length stored in R1.
	 *.2.  Turn off tcp_conn_msg_ready if 0 avail.
	 * Notes:
	 *   1.  First byte may be in conn_byte
	 *       if conn_read = true
	 */
	if (tz390.opt_traceall){
		System.out.println("TCPIO storing msg from conn=" + conn_index);
		sleep_now(tz390.monitor_wait);
	}
	pz390.reg.putInt(pz390.r2,conn_index); // return conn index
    int cur_msg_len = 0;
	int conn_amsg = tcpio_amsg;
	int conn_lmsg = tcpio_lmsg;
	if (tcp_conn_read[conn_index]){
		cur_msg_len = 1;
		pz390.mem.put(conn_amsg,tcp_conn_byte[conn_index]);
	    tcp_conn_read[conn_index] = false;
        conn_amsg++;
        conn_lmsg--;
	}
	if  (conn_lmsg > 1){
        try {
        	int msg_avail = tcp_conn_input[conn_index].available();
        	if (conn_lmsg > msg_avail){
        		conn_lmsg = msg_avail;
        	}
        	if (conn_lmsg > 0 
        		&& tcp_conn_input[conn_index].read(pz390.mem_byte,conn_amsg,conn_lmsg) != conn_lmsg){
        		throw new RuntimeException("TCPIO error on store message from port=" + tcp_conn_server_port[conn_index]);
        	}
        	cur_msg_len = cur_msg_len + conn_lmsg;
    	} catch (Exception e){
    		put_log("TCPIO error storing message from conn=" + conn_index + " - " + e.toString());
    		pz390.reg.putInt(pz390.r15,12);
    	}
	}
	pz390.reg.putInt(pz390.r1,cur_msg_len);
	if (tz390.opt_tracet){
		put_log("TCPIO receive server msg from port=" + tcpio_port 
		      + " conn=" + conn_index
					     + " length=" + cur_msg_len);
		dump_mem(tcpio_amsg,cur_msg_len);
	}
    tcpio_set_conn_msg_ready(conn_index,false);
}
private synchronized boolean tcpio_set_conn_msg_ready(int conn_index,boolean state){
	/*
	 * if state true
	 *    set conn msg ready
	 *    (byte has been read by conn thread)
	 * else if no msg data available 
	 *    reset conn msg ready
	 *    (will force read on conn thread)
	 * else
	 *    leave conn ready set to true
	 *    (allows main user thread to read
	 *    mult msgs without switching back
	 *    to conn thread for a read)
	 */
	if (state){
		tcp_conn_msg_ready[conn_index] = true;
		return true;
	}
	try {
		if (tcp_conn_input[conn_index].available() == 0){
			tcp_conn_msg_ready[conn_index] = false;
			return false;
		} else {
			return true;
		}
	} catch (Exception e){
		put_log("TCPIO disconnect while switching state for conn=" + conn_index);
		tcpio_close_conn(conn_index);
	}
	return false;
}
private synchronized boolean tcp_alloc_conn(int port_index){
    /*
     * allocate next conn for server port use
     */
    int conn_index = 0;
    while (conn_index < max_tcp_conn){
    	if (tcp_conn_server_port[conn_index] == 0){
    		tcp_conn_server_port[conn_index] = tcpio_port;
    		tcp_server_conn_index[port_index] = conn_index;
    		tcp_conn_server_index[conn_index] = port_index;
    		tcp_conn_socket[conn_index] = null;
    		return true;
    	}
    	conn_index++;
    }
    return false;
}
private synchronized void tcp_free_conn(int conn_index){
	/* 
	 * release tcp connecntion for reuse
	 */
	int port_index = tcp_conn_server_index[conn_index];
	if (tcp_conn_server_index[conn_index] == tcp_server_conn_index[port_index]){
		tcp_server_conn_index[port_index] = 0;
		tcp_conn_server_index[conn_index] = 0;
		tcp_conn_server_port[conn_index] = 0;
		tcp_conn_socket[conn_index] = null;
	} else {
		abort_error(23,"TCPIO free conn internal error - abprting");
	}
}
private boolean check_dfp_finite(byte[] dfp_bytes,int dfp_byte_index){
	/*
	 * return true if DFP value finite based
	 * on CF5 field value common to all DFP types
	 */
	if (tz390.dfp_cf5_to_exp2[(dfp_bytes[dfp_byte_index] >>> 2) & 0x1f]<= 2){
		return true;
	} else {
		return false;
	}
}
/*
 *  end of ez390 code 
 */
}