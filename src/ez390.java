import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;

import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.Timer;
;public  class  ez390 implements Runnable {
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
    * 10/04/05 RPI5 - option ASCII use ASCII vs EBCDIC
    *                   pz390.zcvt_ipl_pgm = ascii name
    *                   DCB DDNAME field ascii
    *                   DCB FT RCDS = ascii
    *                   SVC LOAD, LINK, DELETE EP/EPLOC ASCII
    *                   dump text = ascii
    *                   TEST C'...' SDT = ASCII
    *                   ED/EDMK remove high bit on chars
    *                   SSP type instr. allow 0x3 sign 
    *                   UNPK gen 0x3 zone
    * 10/04/05 RPI6 - option ERR(nn) limit errors
    * 10/05/05 RPI5 - add test C"..." sdt support
    * 10/12/05 suppress stats if NOSTATS option on
    * 10/12/05 RPI20 fix error 62 on open of output file
    * 10/14/05 RPI21 0C5 on PSW addr > mem
    * 10/14/05 RPI22 turn off time limit if test
    * 10/14/05 RPI15 req z390 to issue exit at end of test
    * 10/16/05 force console output of msgs after abort
    * 10/16/05 RPI23 add CLST, CUSE, and SRST instructions
    * 10/18/05 RPI28 change DSNAM field to EBCDIC unless ASCII mode
    * 10/18/05 RPI29 use EZ390E and EZ390I prefixes
    * 10/18/05 RPI31 set r15 to 0 on successful svc
    * 10/19/05 RPI33 only enable PD and exponent overflow 
    * 10/19/05 RPI32 add SYS390 dir list support
    * 10/19/05 RPI34 full ebcdic / ascii translation
    * 10/20/05 RPI35 prevent exit loop in test filling log
    * 10/20/05 RPI37 use "I:" and "H:" to separate op/hex keys
    * 10/20/05 RPI39 set rc=r15 on normal exit
    * 10/22/05 RPI44 fix TM to set CC3 when OR'd byte = mask
    *          rather than = to test memory byte
    * 10/23/05 RPI43 correct ED/EDMK sif. 1 byte early
    * 10/23/05 RPI42 use full EBCDIC to ASCII translate for
    *          PGMNAME, DDNAM, DSNAM, GET/PUT
    * 10/24/05 RPI46 add svc 34 to issue OS command
    * 10/25/05 RPI47 add svc 93 to support application window
    * 10/26/05 RPI49
    * 10/27/05 RPI53 set r15 to error # in synad
    * 10/27/05 RPI56 add dump of first pgm memory if dump req
    * 11/01/05 RPI65 fix BALR, BASR, BASSM when R1=R2
    * 11/02/05 RPI66 correct padding spaces in ASCII mode
    * 11/02/05 RPI69 change ED/EDMK to map X'40'for ASCII
    * 11/03/05 RPI71 add missing AL3 relocation code
    * 11/03/05 RPI63 handle x'1a' eof marker in gm RT/VT
    * 11/03/05 RPI64 issue abend S013 if no synad after io err
    * 11/07/05 RPI73 remove PACK/UNPK mode changes
    *          and add PKA/UNPKA
    * 11/08/05 RPI73 change ED/EDMK to require EBCDIC
    *          mask and then translate output if ascii 
    * 11/08/05 RPI76 end cmd processes at exit
    * 11/08/05 RPI77 add cmd read and wait controls
    * 11/08/05 RPI79 add mult. cmd task support
    * 11/11/05 RPI73 restore PACK, UNPK ASCII mode support
    *          (allow 3/b sign and unpk to f/s or 3/s zone) 
    * 11/11/05 RPI75 add SNAP dump support svc 51  
    * 11/13/05 RPI88 add DCB validation   
    * 11/15/05 RPI92 correct trace format in test
    * 11/15/05 RPI93 correct SNAP svc to use memory 
    *          range instead of address and length
    * 11/15/05 RPI94 add svc_timer nano-sec counter  
    * 11/18/05 RPI98 ignore cr,lf TEST commands 
    * 11/19/05 RPI100 reformat abend dump and 
    *          include before continuing in test 
    * 11/19/05 RPI101 correct open error causing
    *          erroneous eof error after reuse of tiot entry
    * 11/19/05 RPI102 add LOAD extension to support 
    *          loading and deleting file in addition to 390's. 
    * 11/20/05 RPI106 correct SPM, IPM field position
    *          also NR, OR, and XR not setting CC
    *          also PR only restore 2-14
    * 11/20/05 RPI82 replace MVC loop with copy & fill   
    * 11/21/05 add LinkedList string type checking  
    * 11/21/05 RPI108 speed up BC by 140 NS when branch
    *          not taken by skipping RX address fetch    
    * 11/23/05 RPI110 turn off DCBOFLGS open bit at close 
    * 11/23/05 RPI108 speed up PD by removing mem_work and
    *          using int vs BigInteger when possible  
    * 11/25/05 RPI111 trim spaces from DDNAME and DSNAME file 
    * 11/26/05 RPI47 add inital gz390 GUI window option
    *          for WTO and WTOR support    
    * 11/27/05 RPI112 correct error 90 I/O error  
    * 11/27/05 RPI108 replace byte buffer pz390.mem.get, pz390.mem.put
    *          pz390.reg.get and pz390.reg.put with direct byte array. 
    * 11/28/05 RPI113 file path with drive: and no separator
    * 11/29/05 RPI47 add svc 1 wait and svc 160 wtor request
    * 12/04/05 RPI108 speed up MVCLE, CLC, MVZ, MVN
    *          OC, NC, XC.   
    * 12/07/05 RPI123 fix multiple path support
    * 12/08/05 RPI121 abort ez390 on test q command
    * 12/15/05 RPI135 use tz390 shared tables
    * 12/18/05 RPI142 use init_opcode_name_keys in tz390
    *          and document all key codes in tz390.
    * 12/20/05 RPI103 add multiple +- test addr operands
    * 12/23/05 RPI127 strip mlc type from file name
    *          and use shared set_pgm_name_type
    * 12/23/05 RPI131 limit file output to maxfile(mb)
    * 01/05/06 RPI107 split ez390 into ez390, pz390, and sz390
    * 01/24/06 RPI186 test cmd g notrace, z notrace & notest
    * 01/24/06 RPI190 remove WTO and WTOR prefixes
    * 01/26/06 RPI191 set R15 for unconditional getmain
    * 02/04/06 RPI197 remove test_or_trace interrupts
    * 02/24/06 RPI218 increase performance using sleep_now
    * 03/02/06 RPI 220 detect tz390.z390_abort and close down
    * 04/10/06 RPI 276 support IPL(pgm) option
    * 04/22/06 RPI 279 request stimer exit when time expires
    * 09/01/06 RPI 423 add runable exception handler to 
    *          shut down thread on interal exceptions
    * 09/09/06 RPI 440 correct reset of ez390_startup in sz390
    * 01/15/07 RPI 535 issue FFF abend on internal trap with dump
    * 04/07/07 RPI 582 set R1 to addr of addr of PARM
    * 04/16/07 RPI 595 echo WTOR reply to console if not GUAM or TEST
    * 05/07/07 RPI 610 correct XCTL link on mult link,xctl,exit 
    * 06/22/07 RPI 644 add init for vz390 VSAM access method
    * 10/18/07 RPI 722 move wtor_reply_buff to sz390
    * 12/25/07 RPI 755 cleanup msgs to log, sta, tr*, con
    * 01/08/08 RPI 782 stop interval timer before exit
    * 03/21/08 RPI 819 zero R15 at startup to get rid of x'F4's
    * 05/23/09 RPI 1040 wait for pz390 to process S422 ABEND
    * 06/13/09 RPI 1054 request S422 ABEND by PZ390 thread
    * 06/15/09 RPI 1050 suppress dup START on CON when TRACE CON
    * 08/24/09 RPI 1069 add CODEPAGE(ascii+ebcdic+LIST) option
    * 09/24/09 RPI 1080 set z390_os_type before init tables 
    * 01/04/10 RPI 1094 move timeout to tz390 to share with gz390
    * 05/10/11 RPI 1149 move started msg to put_trace
    * 07/30/11 RPI 1175 use tz390.check_java_version()
    ********************************************************
    * Global variables                       (last RPI)
    *****************************************************/
	/* 
	 * shared global variables
	 */
	tz390 tz390 = null;
	pz390 pz390 = null;
	sz390 sz390 = null;
	vz390 vz390 = null; // RPI 644
	Thread  pz390_thread = null;
	boolean pz390_running = false;
    String load_file_name = null;
    boolean exit_request = false;
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
        String  cmd_read_line = null;
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
public void process_ez390(String[] args,JTextArea log_text,JTextField command_text){
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
    if (tz390.opt_ipl.length() > 0){
    	run_pgm(pz390.zcvt_ipl_pgm);
    	sz390.ez390_pgm = null;
    	sz390.tot_link_stk = 0;
    }
    run_pgm(pz390.zcvt_user_pgm);
    monitor_timer.stop();  // RPI 782
	sz390.exit_ez390();
}
private void run_pgm(int zcvt_pgm_addr){
	/*
	 * execute IPL pgm and/or application pgm
	 */
	pz390.reg.putInt(pz390.r13,pz390.zcvt_save);
	pz390.reg.putInt(pz390.r14,pz390.zcvt_exit);
	pz390.reg.putInt(pz390.r15,0); // RPI 819 
	pz390.reg.putInt(pz390.r0,zcvt_pgm_addr);
	pz390.reg.putInt(pz390.r1,pz390.zcvt_exec_parma);              // RPI 582
	pz390.mem.putInt(pz390.zcvt_exec_parma,pz390.zcvt_exec_parm);  // RPI 582
	sz390.svc_link();  
   	if (tz390.opt_trap){
   	    pz390.psw_check = false;
   	    while (!tz390.z390_abort && !pz390.psw_check){
    		try {
                exec_pz390();
    		} catch (Exception e){
    			pz390.set_psw_check(pz390.psw_pic_addr);
                pz390.psw_check = false; // retry if no abend
    	    }
    	}
   	} else {
    	exec_pz390(); // notrap - no exception handling
    }
    if (pz390.psw_check && pz390.psw_pic != pz390.psw_pic_exit){
       	sz390.svc_abend(pz390.psw_pic,sz390.svc_abend_type,sz390.svc_req_dump);
    }
}
private void exec_pz390(){
	/* 
	 * 1.  Check for trace msg and log if found
	 * 2.  Execute test commands if test mode
	 * 3.  Dump registers if reqister trace option
	 * 4.  Run pz390 to next interrupt
	 * 5.  Check for trace msg and log if found
	 * 6.  If svc interrupt, exec svc
	 * 7.  else if psw_check 
	 *             if svc_exit exit normally
	 *             else psw_handler for abend
	 *                  or espie/estae restart
	 * 
	 */
    pz390_thread = new Thread(this);
    pz390_running = true;
	pz390_thread.start();
	try {
		if (tz390.opt_guam){
			while (!tz390.z390_abort && pz390_running){
				tz390.sleep_now(tz390.monitor_wait);  // RPI 218
			}
		} else {
			pz390_thread.join(); // faster with no spurious interrupts
		}
	} catch (Exception e){
		sz390.abort_error(202,"pz390 processor error " + e.toString());
	}
}
private void init_ez390(String[] args, JTextArea log_text, JTextField command_text){
	/*
	 * 1.  initialize log routing
	 * 2.  init ascii to ebcdic table
	 * 3.  init reqular expression paser for test
	 * 4.  set options
	 * 5.  initialize memory
	 * 6.  set runtime hooks for cancel
	 * 7.  start monitor for cmd processor and
	 *     timeout, and cpu rate statistics
	 * 
	 * 
	 */
	    if  (log_text != null){
	    	z390_log_text = log_text;
	    }
	    if (command_text != null){
	    	z390_command_text = command_text;
	    }
	    tz390 = new tz390();
	    pz390 = new pz390();
	    sz390 = new sz390();
	    vz390 = new vz390();
	    tz390.init_tz390();  // RPI 1080
    	if (!tz390.check_java_version()){ // RPI 1175
    		sz390.abort_error(204,"unknown java version "
    	    + tz390.java_vendor + " " + tz390.java_version);  
    	}
	    tz390.init_options(args,tz390.z390_type);  
	    tz390.open_systerm("EZ390");
	    tz390.init_codepage(tz390.codepage); // RPI 1069
	    vz390.init_vz390(tz390,pz390,sz390);
	    sz390.init_sz390(tz390,pz390,vz390);
		sz390.open_files(); // RPI 357 RPI 812 moved before init_pz390
	    pz390.init_pz390(tz390,sz390);
		tz390.force_nocon = true;
		sz390.put_log(tz390.started_msg); // RPI 755
		tz390.force_nocon = false; // RPI 1050 moved after trace
		sz390.init_time();
        sz390.init_test();
        put_copyright();
        monitor_startup();
        sz390.ez390_startup = false;
}
private void monitor_startup(){
	/*
	 * start monitor to terminate cmd 
	 * command if timeout limit reached
	 */
    monitor_last_time = System.currentTimeMillis();
    monitor_last_ins_count = ins_count;
    try {
	    ActionListener cmd_listener = new ActionListener() {
	    	public void actionPerformed(ActionEvent evt) {
	            monitor_update();
		    }
		};
		monitor_timer = new Timer(tz390.monitor_wait,cmd_listener);
		monitor_timer.start();
	} catch (Exception e) {
		sz390.log_error(66,"execution startup error " + e.toString());
	}
}
private void monitor_update(){
	/*
	 * 1.  At monitor_wait intervals, update the
	 *     the cpu instruction rate and monitor
	 *     cmd processes if running with timeout
	 *  
	 * 2.  If CMD running and 
	 *     monitor_wait_total > timeout_interval
	 *     then abort cmd process with timeout
	 *  
	 * 3.  If current time beyond timeout
	 *     terminate.
	 * 4.  If WTOR pending, check for reply and post
	 *     ecb.
	 * 5.  If stimer_exit_addr set, check for stimer_Exit_tod
	 *     passed and take exit if tod passed:
	 *     a.  Save r13-r15 and PSW for restore at end of exit
	 *     b.  Set r13 to save, r14 to svc 3 instr, and r15 to exit
	 *     c.  Set stimer_exit_pending for svc 3
	 *     d.  change PSW to r15 exit addr
	 * 6.  If GUAM TN3270 screen active, 
	 *     process pending typed keys    
	 */
	monitor_next_time = System.currentTimeMillis();
	monitor_next_ins_count = ins_count;
	monitor_next_io_count = io_count;
	monitor_cur_interval = monitor_next_time - monitor_last_time;
	if (tz390.opt_timing){
    	pz390.cur_date = new Date();
    	if (tz390.opt_time 
    		&& !pz390.psw_check
            && monitor_next_time > sz390.tod_time_limit){ // RPI 837
    	    while (pz390_running){
    	    	tz390.timeout   = true; // RPI 1054 request pz390 to abend, RPI 1094 moved
    	    	pz390.psw_check = true; // RPI 1054
    	    	if (tz390.opt_guam){ // RPI 1094
    				System.out.println("GUAM GUI window closed due to timeout");
    				System.exit(16);
    	    	}
    	    	Thread.yield(); // RPI 1040 wait to end exec before dump
    	    }
    	}
	}
    int cmd_id = 0;
    while (cmd_id < sz390.tot_cmd){
    	if  ((sz390.cmd_proc_running[cmd_id]) && sz390.cmd_proc_rc(cmd_id) != -1){
    		sz390.put_log("*** " + cur_date_MMddyy.format(pz390.cur_date)
    			 + " " + cur_tod_hhmmss.format(pz390.cur_date)
    			 + " CMD task ended TOT SEC=" + monitor_cmd_time_total
    			 + " TOT LOG IO=" + io_count);
    		sz390.cmd_proc_running[cmd_id] = false;
    	} else if (tz390.opt_time && tz390.max_time_seconds > 0){
    		if (sz390.cmd_proc_running[cmd_id] && sz390.cmd_proc_rc(cmd_id) == -1){
    			if  (monitor_cmd_time_total > tz390.max_time_seconds){
    				sz390.cmd_cancel(cmd_id);
    				sz390.log_error(75,"CMD command timeout error " + monitor_cmd_time_total + " > " + tz390.max_time_seconds); 
    			}
    		}
    	} else {
    		if ((sz390.cmd_proc_cmdlog[cmd_id]     // RPI 731
    		     || tz390.max_cmd_queue_exceeded) // RPI 731
    			&& sz390.cmd_proc_running[cmd_id]
    		    && sz390.cmd_proc_rc(cmd_id) == -1){
    			cmd_read_line = sz390.cmd_get_queue(cmd_id);
    		    while (cmd_read_line != null){
    				sz390.put_log("CMDLOG ID=" + cmd_id + " MSG=" + cmd_read_line);
    				cmd_read_line = sz390.cmd_get_queue(cmd_id);
    			}
    		}
    	}
    	cmd_id++;
	}
	if (sz390.wtor_reply_pending){
		if (tz390.opt_guam){
			sz390.wtor_reply_string = sz390.gz390.get_wtor_reply_string(sz390.wtor_ecb_addr);
		} else if (tz390.opt_test && sz390.test_cmd_file != null){
			try {
				sz390.wtor_reply_string = sz390.test_cmd_file.readLine();
			} catch (Exception e){
				sz390.log_error(93,"wtor reply I/O error - " + e.toString());
			}
		} else {  // RPI 595
			try {
				if (sz390.wtor_reply_buff == null){
					sz390.wtor_reply_buff   = new BufferedReader(new InputStreamReader(System.in));
				}
				if (pz390.mem.getInt(sz390.wtor_ecb_addr) == sz390.ecb_waiting
					|| sz390.wtor_reply_buff.ready()){
					sz390.wtor_reply_string = sz390.wtor_reply_buff.readLine();
				}
			} catch (Exception e){
				sz390.log_error(93,"wtor reply I/O error - " + e.toString());
			}
		}
		if (sz390.wtor_reply_string != null){
			sz390.put_log("" + sz390.wtor_reply_string);  //RPI190 remove "WTOR REPLY MSG"
			sz390.put_ascii_string(sz390.wtor_reply_string,sz390.wtor_reply_addr,sz390.wtor_reply_len,' ');
			sz390.pz390.mem.putInt(sz390.wtor_ecb_addr,sz390.ecb_posted); // post ecb for any wait
			sz390.wtor_reply_pending = false;
		}
	}
	if (!sz390.stimer_exit_running
		&& sz390.stimer_exit_addr != 0
		&& sz390.stimer_exit_time <= monitor_next_time){
        sz390.stimer_exit_request = true; // request exit
	}
	if (tz390.z390_abort){  // RPI 220 shut down due to external request
		sz390.abort_error(203,"EZ390E monitor external shutdown request");
	}
	monitor_last_time = monitor_next_time;
	monitor_last_ins_count  = monitor_next_ins_count;
	monitor_last_io_count   = monitor_next_io_count;
}
public void run() {
	if (pz390_thread == Thread.currentThread()){
		if (tz390.opt_trap){ // RPI 423
			try {
				pz390.exec_pz390();
			} catch (Exception e){
				sz390.svc_abend(pz390.psw_pic_addr,sz390.system_abend,tz390.opt_dump); // RPI 536 // RPI 1054			
			}
		} else {
			pz390.exec_pz390();
		}
		pz390_running = false;
		return;
	}
	int cmd_id = 0;
	while (cmd_id < sz390.tot_cmd){
		if (sz390.cmd_proc_thread[cmd_id] == Thread.currentThread()) {
			io_count++;
			sz390.cmd_proc_io[cmd_id]++;
			try {
				sz390.cmd_proc[cmd_id].waitFor();
			} catch (Exception e){
				sz390.abort_error(201,"cmd proc wait error " + e.toString());
			}
			return;
		} else if (sz390.cmd_output_thread[cmd_id] == Thread.currentThread()) {
			sz390.copy_cmd_output_to_queue(cmd_id);
			return;
		} else if (sz390.cmd_error_thread[cmd_id] == Thread.currentThread()) {
			sz390.copy_cmd_error_to_queue(cmd_id);
			return;
		}
		cmd_id++;
	}
}
private void put_copyright(){
	   /*
	    * display ez390 version, timestamp,
	    * and copyright if running standalone
	    */
	    tz390.force_nocon = true;  // RPI 755
	   	if  (z390_log_text == null){
	   		sz390.put_log("EZ390I Copyright 2011 Automated Software Tools Corporation");
	   		sz390.put_log("EZ390I z390 is licensed under GNU General Public License");
	   	}
	   	sz390.put_log("EZ390I program = " + tz390.dir_mlc + tz390.pgm_name + tz390.pgm_type);
	   	sz390.put_log("EZ390I options = " + tz390.cmd_parms);
		if (tz390.opt_stats){
			tz390.put_stat_line("Copyright 2011 Automated Software Tools Corporation");
			tz390.put_stat_line("z390 is licensed under GNU General Public License");
			tz390.put_stat_line("program = " + tz390.dir_mlc + tz390.pgm_name + tz390.pgm_type);
			tz390.put_stat_line("options = " + tz390.cmd_parms);
		}
	   	tz390.force_nocon = false; // RPI 755
	 }

/*
 *  end of ez390 code 
 */
}