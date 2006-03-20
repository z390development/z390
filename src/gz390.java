import java.awt.Component;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.GraphicsEnvironment;
import java.awt.Toolkit;
import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.StringSelection;
import java.awt.datatransfer.Transferable;
import java.awt.datatransfer.UnsupportedFlavorException;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.AdjustmentEvent;
import java.awt.event.AdjustmentListener;
import java.awt.event.ComponentEvent;
import java.awt.event.ComponentListener;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.io.File;
import java.io.FilePermission;
import java.nio.ByteBuffer;
import java.text.SimpleDateFormat;
import java.util.Arrays;
import java.util.Date;
import java.util.PropertyPermission;
import java.util.StringTokenizer;

import javax.swing.BorderFactory;
import javax.swing.ImageIcon;
import javax.swing.JCheckBoxMenuItem;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.KeyStroke;
import javax.swing.Timer;

public  class  gz390  
    implements MouseListener, KeyListener,
	           ActionListener, 
			   ComponentListener,
			   FocusListener {
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
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


	 ****************************************************
	 * Maintenance
	 * ***************************************************
	 * 11/26/05 RPI47 gz390 copied from z390
	 * 12/14/05 RPI47 add TGET/TPUT EDIT mode support
	 * 12/15/05 RPI135 use tz390 shared tables
	 * 02/21/06 RPI156 add basic TN3270 support
	 * 02/21/06 RPI208 set tz390.z390_abort to abort
	 * 02/23/06 RPI216 replace SHFT-PF?? with ALT-FP??
	 * 02/27/06 RPI219 require WCC for FULLSCR and ASIS,
	 *          require WRITE,WCC for TPUT ASIS,
	 *          correct multiple field support.
	 * 03/02/06 RPI 220 overwrite prior field text and
	 *          set tz390.abort_error and wait for ez390 to close
	 *          add beep support for WCC bit x'04'
	 * 03/04/06 RPI 221 reset keyboard lock for TGET,
	 *          return R15=0 ok, 4 for NOWAIT and no input,
	 *          or 8 for any other error. 
	 * 03/06/06 RPI 222 ignore WCC x'40' reset and 
	 *          fix RA command to translate char and stop after
	 *          updated address matches 
	 * 03/15/06 RPI 224 convert nulls to spaces and do not 
	 *          send as input characters, remove field context,
	 *          add arrow key support       
	 ********************************************************
     * Global variables
     *****************************************************
     * global variables
     */
	/*
	 * global max/min limits
	 */
        int max_errors = 100;
        int max_cmd  = 100;
        int max_keys = 100;
        int max_rows = 24;
        int max_cols = 80;
        int max_addr = max_rows * max_cols;
        int max_fud = 500;
        int max_buff = 3000;
        /*
         * shared z390 library classes
         */
        Toolkit my_toolkit = null;
        tz390 tz390 = null; // shared tables
	    String startup_cmd_file = null;
	    int first_user_parm = 0;
        int hex_base = 16;
	    boolean echo_cmd = true;
	    boolean console_log = false;
        int gz390_errors = 0;
	    boolean cmd_error = false;
        String  main_title = "GZ390";
        boolean check_perms = true;          //reset with /NP
        boolean perm_file_user_dir = false;  //set if user.dir ok
        boolean perm_file_read     = false;  //set if read  ok
        boolean perm_file_write    = false;  //set if write ok
        boolean perm_file_execute  = false;  //set if exec ok
        boolean perm_runtime_thread = false; //set if popup file chooser ok
        boolean perm_file_log      = false;  //set if write log ok
        boolean perm_select        = false;  //set if select dir/file ok
        boolean main_demo  = false; //set if no permissions
        boolean main_lic   = false; //set if permissions ok
        String mode_msg1 = null;
        String mode_msg2 = null;
        Date lic_end_date = null;
        Date cur_date = null;
   	    SimpleDateFormat mmddyy = new SimpleDateFormat("MM/dd/yy");
   	    SimpleDateFormat hhmmss = new SimpleDateFormat("HH:mm:ss");
    /*
     * Monitor variables
     */   
        int     ins_count = 0;  
        int     io_count  = 0;
        int     start_cmd_io_count;
        long    start_cmd_time;
        Timer   monitor_timer = null;
        int     monitor_wait = 100; // RPI 224 
        int     monitor_timeout_limit = 0 * 1000;
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
    /*
     * GUI screen view variables
     */

        static int gui_view_mcs    = 1;
        static int gui_view_screen = 2;
        static int gui_view_graph  = 3;
        int     gui_view = gui_view_mcs;
        int   gui_tot_key  = 0;
        int   gui_next_key = 0;
        int[] gui_key_code_char = new int[max_keys];
        int gui_cur_row = 1;
        int gui_cur_col = 1;
    /*
     *  status interval display variables
     */    
        boolean status_visible = true;
        int     status_interval =  0;
	    long    status_last_time = 0;
        long    status_next_time = 0;
        int     status_last_ins_count = 0;
        int     status_next_ins_count = 0;
        int     status_next_io_count = 0;
        int     status_last_io_count = 0;
        long    status_cur_ins  = 0;
        long    status_cur_int  = 0;
        long    status_cur_rate = 0;
	/*
	 *  WTOR reply thread variables
	 */ 	
    boolean wtor_running = false;
	Thread wtor_thread = null;
	String wtor_reply_string = null;
	int wtor_ecb_addr = -1;
	String cmd_line = null;
    boolean shutdown_exit = false;
    /*
     * global TGET/TPUT TN3290 objects
     */
    int tpg_rc         = 0;    // return code
	int tpg_flags      = 0;    // from high byte R1
	int tpg_op_mask    = 0x80; // 1=TGET, 0=TPUT
	int tpg_op_tget    = 0x80;
	int tpg_op_tput    = 0x00;
	int tpg_wait_mask  = 0x10; // 1=NOWAIT, 0=WAIT
	int tpg_wait       = 0x00; // 0=WAIT
	int tpg_nowait     = 0x10;
	int tpg_type         = 0;
	int tpg_type_mask    = 0x03; // 00=EDIT 01=ASIS 10=CONTROL 11=FULLSCR
    int tpg_type_edit    = 0x00;
    int tpg_type_asis    = 0x01;
    int tpg_type_control = 0x02;
    int tpg_type_fullscr = 0x03;
	byte[] tget_byte = new byte[max_buff];
    ByteBuffer tget_buff = ByteBuffer.wrap(tget_byte);
    int tget_index = 0;
    int tget_len   = 0;
    byte[] tput_byte = new byte[max_buff];
    ByteBuffer tput_buff = ByteBuffer.wrap(tput_byte);
    int tput_index = 0;
    int tput_len = 0;
    int tput_buff_byte = 0;
    /*
     * global tn3270 data
     */
    byte tn3270_tab_code = 0x09;
    byte tn3270_enter_code = 0x7d;
    byte tn3270_sba_cmd = 0x11; // set buffer addr (sba)
    byte tn3270_sf_cmd  = 0x1d; // set field (attr byte)
    byte tn3270_ic_cmd  = 0x13; // insert cursor
    byte tn3270_pt_cmd  = 0x05; // program tab 
    byte tn3270_ra_cmd  = 0x3c; // repeat to addr (sba,char)
    byte tn3270_eua_cmd = 0x12; // erase unprotected to addr (sba)
    byte tn3270_sa_cmd  = 0x28; // eds set attribute
    byte tn3270_sfe_cmd = 0x29; // eds start field
    boolean tn3270_cursor = false;
    boolean tn3270_cursor_alt = false;
    int tn3270_cursor_scn_addr = 0;
    int tn3270_cursor_text_addr = 0;
    int tn3270_cursor_count = 1;
    int tn3270_cursor_wait_int = 2;
    char tn3270_cursor_sym = '_';
    boolean tn3270_full_screen = false;
    byte tn3270_field = 1;
    int tn3270_field_highlight = 0; // eds sfe highlight
    int tn3270_field_color     = 0; // eds sfe color
    int tn3270_protect_mask = 0x20; // mask for protected field attribute bit 
    int tn3270_numeric_mask = 0x10; // numeric field
    int tn3270_mdt_mask     = 0x01; // mask for modified data attribute bit  
    int tn3270_mdt_off      = 0xfe; // turn off mdt
    int tn3270_noaid = 0x60;       // no aid available
    int tn3270_aid = 0x60;         // set from PF key or enter if unlocked
    int tn3270_esc = 0x27;         // tput tso escape followed by write cmd and wcc
    int tn3270_bell = 0x07;        // ascii bell char forsystem.out alarm
    int tn3270_write_cmd    = 0;   // vtam write command after escape
    int tn3270_write = 0xF1;       // tput tso write screen
    int tn3270_erase_write = 0xF5; // tput tso erase write
    int tn3270_write_alt = 0xF1;       // tput tso write screen
    byte tn3270_attr_prot_text = 0x30; // protected unmodified text 
    boolean tn3270_kb_lock = true; 
    boolean tn3270_attn = false;
    int cur_fld_addr = 0;
    int cur_fld_attr = 0;
    int fld_tot;            // protected and unprotected
    int fld_input_tot = 0;  // unprotected fields
    int[]  fld_addr       = new int[max_addr];
    int[]  fld_input_addr = new int[max_addr];
    byte[] scn_fld  = new byte[max_addr];
    int[]  scn_attr = new int[max_addr];
    byte[] scn_byte = new byte[max_addr];
    char[] scn_char = new char[max_addr];
    int scn_addr = 0;
    int[] sba_to_ebc = {
    	0x40,0xC1,0xC2,0xC3,0xC4,0xC5,0xC6,0xC7, //00
    	0xC8,0xC9,0x4A,0x4B,0x4C,0x4D,0x4E,0x4F, //08
        0x50,0xD1,0xD2,0xD3,0xD4,0xD5,0xD6,0xD7, //10
        0xD8,0xD9,0x5A,0x5B,0x5C,0x5D,0x5E,0x5F, //18
        0x60,0x61,0xE2,0xE3,0xE4,0xE5,0xE6,0xE7, //20
        0xE8,0xE9,0x6A,0x6B,0x6C,0x6D,0x6E,0x6F, //28      
        0xF0,0xF1,0xF2,0xF3,0xF4,0xF5,0xF6,0xF7, //30
        0xF8,0xF9,0x7A,0x7B,0x7C,0x7D,0x7E,0x7F  //38
        };
    int[] ebc_to_sba = {
    	0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff, //00
    	0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff, //08
    	0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff, //10
    	0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff, //18
    	0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff, //20
    	0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff, //28
    	0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff, //30
    	0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff, //38
    	0x00,0xff,0xff,0xff,0xff,0xff,0xff,0xff, //40
    	0xff,0xff,0x0a,0x0b,0x0c,0x0d,0x0e,0x0f, //48
    	0x10,0xff,0xff,0xff,0xff,0xff,0xff,0xff, //50
    	0xff,0xff,0x1a,0x1b,0x1c,0x1d,0x1e,0x1f, //58
    	0x20,0x21,0xff,0xff,0xff,0xff,0xff,0xff, //60
    	0xff,0xff,0x2a,0x2b,0x2c,0x2d,0x2e,0x2f, //68
    	0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff, //70
    	0xff,0xff,0x3a,0x3b,0x3c,0x3d,0x3e,0x3f, //78
    	0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff, //80
    	0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff, //88
    	0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff, //90
    	0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff, //98
    	0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff, //A0
    	0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff, //A8
    	0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff, //B0
    	0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff, //B8
    	0xff,0x01,0x02,0x03,0x04,0x05,0x06,0x07, //C0
    	0x08,0x09,0xff,0xff,0xff,0xff,0xff,0xff, //C8
    	0xff,0x11,0x12,0x13,0x14,0x15,0x16,0x17, //D0
    	0x18,0x19,0xff,0xff,0xff,0xff,0xff,0xff, //D8
    	0xff,0xff,0x22,0x23,0x24,0x25,0x26,0x27, //E0
    	0x28,0x29,0xff,0xff,0xff,0xff,0xff,0xff, //E8
    	0x30,0x31,0x32,0x33,0x34,0x35,0x36,0x37, //F0
    	0x38,0x39,0xff,0xff,0xff,0xff,0xff,0xff, //F8
    	};
    /*
     *  Global GUI objects 
     */
    int ascii_lf = 10;
    int ascii_cr = 13;
    int ebcdic_space = 0x40;
    boolean refresh_wait    = false;
    boolean refresh_request = false;
 	boolean main_status  = true;
        boolean main_view_changed = false;
        JFrame main_frame    = null;
        int main_width  = 650;
        int main_height = 550;
        int main_width_max = 800;
        int main_height_max = 600;
        int main_width_min  = 150;
        int main_height_min = 150;
        int main_border = 2;
        int start_bar_height = 36; //windows start bar
        int main_loc_x = 20;
        int main_loc_y = 20;
        int scrollbar_width = 15;
        int font_space = 10;
	int font_size = 12;       //see FONT command
    int title_height = 0;
	int menu_height = 0;
	int font_width = 0;
    int log_char_height = 0; //see FONT command
    int tool_height = 0;     //reset to 0 if hidden;
    int lines_per_page = 0;   //set by update_main_view()
	int log_height = 0;       //set by update_main_view()
	int log_width  = 0;      //set by update_main_view()
	int command_height = 0;
	int command_columns  = 75;
	int status_height  = 0;
	int applet_status_height = 0;
	boolean labels_visible = true;
	int labels_min_width = main_width;
	int labels_max_font  = font_size;
	int label_width    = 0;
        JPanel main_panel    = null;
        JTextArea log_text = null;
        JTextArea scn_text = null;
        JTextArea graph_grid  = null;
        JScrollPane main_view    = null;
        JLabel cmd_label = null;
        JTextField  gz390_cmd_line = null;
        JLabel status_line_label = null;
        JTextField  status_line = null;
        int cur_cmd = 0;
        int last_cmd = 0;
        int view_cmd = -1;
        String[] cmd_history = new String[100];
    /*
     *  Menu items requiring state changes
     */  
        boolean opt_tn3270     = false;  // display TN3270 panel
        boolean opt_graph    = false;  // display graphic panel
        boolean opt_mcs      = true;   // display mcs panel
        JMenuBar menuBar = null;  //RPI81       
        JMenu file_menu = null;            
        JMenu edit_menu = null;                    
        JMenu view_menu = null;            
        JMenu help_menu = null;                    
        JMenuItem file_menu_exit = null;       
        JMenuItem edit_menu_cut = null;        
        JMenuItem edit_menu_copy = null;       
        JMenuItem edit_menu_paste = null;      
        JMenuItem edit_menu_select_all = null; 
        JMenuItem edit_menu_copy_log = null;   
        JMenuItem edit_menu_notepad = null; 
        JCheckBoxMenuItem view_menu_mcs    = null;              
        JCheckBoxMenuItem view_menu_tn3270   = null; 
        JCheckBoxMenuItem view_menu_graph  = null;     
        JMenuItem help_menu_help = null;  
        JMenuItem help_menu_support = null;
        JMenuItem help_menu_about = null;           
    /*
     * Pop-up edit menu variables (right click)
     */    
        JPopupMenu popup_edit_menu = null; 
        Component focus_comp = null;
        /*
         * Dialog frames
         */
           JFrame select_dir_frame  = null;
           JFrame select_file_frame = null; 
           File selected_file = null;
           String selected_file_name = null;
           String selected_dir_name = null;
           String select_cmd = null;
           String select_file_type = null;
           String select_opt  = "";

     /*
      * web site and install location
      */
        String web_site = "http://www.z390.org";
        String install_loc = null;
        String install_doc = null;
      /*
       * macro assembler command global variables
       */
        String sysin_file_name = null;
        String syslib_dir_name = null;
        String sysout_dir_name = null;
        boolean print_option = true;
        boolean anim_option  = true;
       /* 
        * end of global gz390 class data and start of procs
        */

  	public static void main(String[] args) {
  	/*
  	 * Create instance of gz390 class
  	 */
    	    gz390 pgm = new gz390();
            pgm.set_main_mode(args);
            pgm.init_gz390(args);
          }
  	  private int set_main_mode(String[] args){
  		/*
  		 * Set main program execution mode
  		 * Set security permissions
  		 * 
  		 * Notes:
  		 *   1.  called from main or init before
  		 *       gz390 instance started so only
  		 *       set class variables.
  		 */	
  			String java_vendor  = System.getProperty("java.vendor");
  			String java_version = System.getProperty("java.version");
  			if (!java_vendor.equals("Sun Microsystems Inc.")
  				||	java_version.compareTo("1.5") < 0
  				|| java_version.compareTo("9.9" ) > 0){
  				MessageBox box = new MessageBox();
  				box.messageBox("GZ390E error ",
				    "Unsupported Java Version " +                                                        java_vendor + " " + java_version);
				exit_main(16);
  			}
  			main_demo = false;
  			main_lic   = false;
        /*
         * Check for security manager and set permissions
         */
  		 if (check_perms){
            SecurityManager sm = System.getSecurityManager();
            if  (sm != null){
      			FilePermission perm_read = new FilePermission("/*","read");
      			FilePermission perm_write = new FilePermission("/*","write");
                FilePermission perm_execute  = new FilePermission("/*","execute");
      			PropertyPermission perm_user_dir = new PropertyPermission("user.dir","read,write");
                RuntimePermission  perm_thread   = new RuntimePermission("modifyThread");
      			try {
            	    sm.checkPermission(perm_thread);
            	    perm_runtime_thread = true;
            	} catch (SecurityException e){
            		perm_runtime_thread = false;
            	}
                try {
            	    sm.checkPermission(perm_execute);
            	    perm_file_execute = true;
            	} catch (SecurityException e){
            		perm_file_execute = false;
            	}
                try {
            	    sm.checkPermission(perm_user_dir);
            	    perm_file_user_dir = true;
            	} catch (SecurityException e){
            		perm_file_user_dir = false;
            	}
            	try {
            	    sm.checkPermission(perm_read);
            	    perm_file_read = true;
            	} catch (SecurityException e){
            		perm_file_read = false;
            	}
            	try {
            	    sm.checkPermission(perm_write);
            	    perm_file_write = true;
            	} catch (SecurityException e){
            		perm_file_write = false;
            	}
            } else {
            	perm_file_read       = true;
            	perm_file_write      = true;
            	perm_file_user_dir   = true;
            	perm_file_execute    = true;
            	perm_runtime_thread  = true;
            }
  		 } else {
        	perm_file_read       = false;
        	perm_file_write      = false;
        	perm_file_user_dir   = false;
        	perm_file_execute    = false;
        	perm_runtime_thread  = false;
  		 }
            perm_file_log = perm_file_write 
			                && perm_file_user_dir;
            perm_select   = perm_file_read  
			                && perm_file_user_dir 
							&& perm_runtime_thread;
            if (!perm_file_log){
                perm_file_write = false;
            }
            /******************************************
             * Switch to demo mode if no read permission
             */
            if  (!perm_file_read){
            	mode_msg1 = "Permission to read files denied - continuing in demo mode";
            	main_demo = true;
            	main_lic   = false;
            	return 0;
  			}
         	main_lic = true;
            main_demo = false;
            return 0;
  		}
		private void log_error(int error,String msg){
			gz390_errors++;
			cmd_error = true;
			msg = "GZ390E error " + error + " " + msg;
			gui_put_log(msg);
			if  (max_errors != 0 && gz390_errors > max_errors){
		        abort_error(101,"maximum errors exceeded");
            }
		}
		private void abort_error(int error,String msg){
			gz390_errors++;
			tpg_rc = 8;
			status_line.setText(msg);
			msg = "GZ390E " + error + " " + msg;
			gui_put_log(msg);
 		    System.out.println(msg);
 		    if (tz390.z390_abort){
 		    	exit_main(16); // exit now to prevent loop
 		    } else {
 		    	tz390.z390_abort = true; // abort all processes
 		    }
		}
		private void shut_down(int return_code){
		/*
		 * cancel threads and exit with rc
		 * (turn off runtime shutdown exit
		 */
			if  (!tz390.z390_abort){
				abort_error(58,"guam gui window closed");
				int count = 5;
				while (count > 0){
					sleep_now(); // RPI 220 wait for ez390 to term
				    count--;
				}
				return;
			}
			tz390.z390_abort = true; // force ez390/pz390 shutdown
			if  (monitor_timer != null){
			    monitor_timer.stop();
			}
			if  (main_frame != null){
				main_frame.dispose();
			}
		    if  (!shutdown_exit){
					shutdown_exit = true; //disable exit
			        System.exit(return_code);
			}
		}

	   private void process_command(String cmd_line) {
	   	/* 
	   	 * 1.  parse parms and execute 
	   	 *     gz390 command if found.
	   	 *     a.  * in position 1 is a comment
	   	 *     e.  space or null logged as blank line 
	   	 *      
	   	 * 2.  If not a known gz390 command, 
	   	 *     issue CMD Windows command.
	   	 * 
	   	 * Notes:
	   	 * 
	   	 * 1.  gz390_cmd_line event handler
	   	 *     routes input to CMD processor when in
	   	 *     cmd_mode.
	   	 * 
	   	 * 2.  Some commands will issue retry or
	   	 *     cancel error message if command
	   	 *     running on separate thread to avoid
	   	 *     file conflicts or deadlocks.  Other
	   	 *     non destructivecommands will proceed
	   	 *     in parallel which may cause log 
	   	 *     messages to be intermixed.
	   	 * 
	   	 * 3.  Status bar shows progress of command
	   	 *     processes on separate threads.
	   	 * 
	   	 * 4.  Use EXIT or BREAK event to abort CMD
	   	 *     process. CTRL-C works in command mode only.
	   	 */
		 try {
	   	    cmd_error = false;
	   	    if  (cmd_line == null 
	   	    		|| cmd_line.length() == 0
					|| cmd_line.equals(" ")){
	   	    	return;
	   	    }
  	        String cmd_opcode = null;
            String cmd_parm1 = null;
            String cmd_parm2 = null;
	   	    boolean cmd_opcode_ok = false;
            StringTokenizer st = new StringTokenizer(cmd_line," ,\'\"",true);
            String next_token;
            cmd_opcode = get_next_parm(st,true).toUpperCase();
            if (st.hasMoreTokens()) {
                cmd_parm1 = get_next_parm(st,true);
                if (st.hasMoreTokens()) {
                    cmd_parm2 = get_next_parm(st,true);
                    while (st.hasMoreTokens()){
                        next_token = st.nextToken();
                        if (next_token != null
                        	&& !next_token.equals(" ")){
                        	cmd_parm2 = cmd_parm2 + " " + next_token; 
                        }
                    }
                }
                if (cmd_parm1 != null && cmd_parm1.equals(",")){
                   cmd_parm1 = null;
                   cmd_parm2 = null;
                }
            }
         char first_char = cmd_opcode.charAt(0);
         switch (first_char){
         case 'A':  
            break;
         case 'B':           
            break;
         case 'C': 
            break;
         case 'D':
        	 break;
         case 'E':
            break;
         case 'F':            
            break;
         case 'G':             
            break;
         case 'H':          
            break;
         case 'I':            
            break;
          case 'J':
              break;
          case 'L': 
            break;
         case 'M': 
              break;
         case 'N': 
            break;
         case 'O':                      
            break;
         case 'P':
            break;
         case 'R':
            break;
         case 'S':
            break;
         case 'T':
            break;
         case 'U':
            break;
         case 'V':
            break;
         case 'W':
            break;
         case 'X':
            break;
         case '*':         
           	cmd_opcode_ok = true;
            break;
         }
            /*
             * if built in command not found, 
             * try Windows cmd.
             */
            if  (cmd_opcode_ok) {
            	add_cmd_hist();  
            }
		 } catch (Exception e){
			 log_error(7,"command error on -" + cmd_line);
		 }
       }
	   private void add_cmd_hist(){
		   /*
		    * add command cmd_line to rolling history
		    */
		   view_cmd = -1;
		   cur_cmd++;
           if (cur_cmd >= max_cmd){
              cur_cmd = 1;
           } else {
              last_cmd = cur_cmd;
           }
           cmd_history[cur_cmd] = cmd_line;
	   }
	   private void get_prev_cmd(){
		   /*
		    * restore prev cmd to gz390_cmd_line
		    */
   	   	   if  (view_cmd < 0){
   	   	   	   view_cmd = cur_cmd;
   	   	   } else {
   	   		   if (view_cmd > 0){
   	   			   view_cmd--;
   	   	       } else {
   	   	           view_cmd = last_cmd;
   	   	       }
   	   	   }
   	   	   gz390_cmd_line.setText(cmd_history[view_cmd]);
	   }
	   private void get_next_cmd(){
		   /*
		    * restore next cmd to gz390_cmd_line
		    */
  	   	   if  (view_cmd < 0){
  	   	   	   view_cmd = cur_cmd;
  	   	   } else {
  	   		   if (view_cmd < last_cmd) {
  	   			   view_cmd++;
  	   	       } else {
  	  	           view_cmd = 0;
  	   	       }
  	   	   }
 	   	   gz390_cmd_line.setText(cmd_history[view_cmd]);
	   }
	   private String get_next_parm(StringTokenizer st,boolean ignore_spaces){
	   /*
	    * get string with or without single/double
	    * quotes.
	    * 
	    * ignore leading spaces or commas if 
	    * ignore_spaces = true, else return null
	    * if space or comma found next.
	    */
	   	String parm_string = st.nextToken();
	   	String delimiter;
        String next_token;
        while (ignore_spaces && parm_string.equals(" ") && st.hasMoreElements()){
        	parm_string = st.nextToken();
        }
	   	if  (parm_string.equals("\"") || parm_string.equals("\'")) {
            delimiter = parm_string;
	   		next_token = "";
	   		while (st.hasMoreTokens()
	   				&& !next_token.equals(delimiter)){
	   			next_token = st.nextToken();
	   			parm_string = parm_string + next_token;
	   		}
	   	} else {
	   		if  (parm_string.equals(" ")){
	   			parm_string = null;
	   		}
	   	}
	   	return parm_string;
	   }
	   private void about_command(){
		    gui_put_log("\nz390 GUI gz390 Graphical User Access Method "
				  + tz390.version); 
 	   	    gui_put_log("Copyright 2005 Automated Software Tools Corporation");
			gui_put_log("z390 is licensed under GNU General Public License");
	   	    gui_put_log("gz390 supports MCS, TN3270, and Graphic panel views");
	   	    gui_put_log("gz390 J2SE Java source is distributed as part of z390");
	   	    gui_put_log("Visit www.z390.org for additional information and support");
	   }
	   private void font_command(int new_font_size){
	   /* 
	    * reset font size 
	    */
   	    	if (new_font_size < 8 || new_font_size > 72){
   	    		log_error(8,"font outside New Courier fixed width font limits");
   	    	} else {
   	    		font_size = new_font_size;
   	    		set_gui_size();
                set_text_font();
                set_tooltips();
                refresh_request = true;
   	    	}	
	   }
	   private void set_text_font(){
		   /*
		    * reset font size for menu, log, cmd
		    * and status line
		    */
	          menuBar.setFont(new Font("Courier",Font.BOLD,font_size)); //RPI81
   	          file_menu.setFont(new Font("Courier",Font.BOLD,font_size));   
   	          edit_menu.setFont(new Font("Courier",Font.BOLD,font_size));   
   	          view_menu.setFont(new Font("Courier",Font.BOLD,font_size));   
   	          help_menu.setFont(new Font("Courier",Font.BOLD,font_size)); 
   	          file_menu_exit.setFont(new Font("Courier",Font.BOLD,font_size));   
   	          edit_menu_cut.setFont(new Font("Courier",Font.BOLD,font_size));    
   	          edit_menu_copy.setFont(new Font("Courier",Font.BOLD,font_size));   
   	          edit_menu_paste.setFont(new Font("Courier",Font.BOLD,font_size));  
   	          edit_menu_select_all.setFont(new Font("Courier",Font.BOLD,font_size)); 
   	          edit_menu_copy_log.setFont(new Font("Courier",Font.BOLD,font_size));   
   	          edit_menu_notepad.setFont(new Font("Courier",Font.BOLD,font_size));    
   	          view_menu_mcs.setFont(new Font("Courier",Font.BOLD,font_size));      
   	          view_menu_tn3270.setFont(new Font("Courier",Font.BOLD,font_size)); 
   	          view_menu_graph.setFont(new Font("Courier",Font.BOLD,font_size));         
   	          help_menu_help.setFont(new Font("Courier",Font.BOLD,font_size));       
   	          help_menu_support.setFont(new Font("Courier",Font.BOLD,font_size));      
   	          help_menu_about.setFont(new Font("Courier",Font.BOLD,font_size));      
	          log_text.setFont(new Font("Courier",Font.BOLD,font_size));
   	          cmd_label.setFont(new Font("Courier",Font.BOLD,font_size));
   	          gz390_cmd_line.setFont(new Font("Courier",Font.BOLD,font_size));
   	          status_line_label.setFont(new Font("Courier",Font.BOLD,font_size));
   	          status_line.setFont(new Font("Courier",Font.BOLD,font_size));
	   }
	   private void help_command(){
	   	/*
	   	 * log summary list of commands and help reference
	   	 */
	   	gui_put_log("\nz390 GUI z390 Graphical User Access Method Help");
	   	gui_put_log("View menu MCS     - Display WTO and WTOR scrolling window (default)");
	   	gui_put_log("View menu TN3270  - Display TPUT and TGET TN3270 window");
	   	gui_put_log("View menu Graph   - Display graph drawn by gz390 GKS commands");
        gui_put_log("Help menu Support - Link to www.z390.org"); 
	   }
	   private void monitor_startup(){
	   /*
	    * start monitor to terminate cmd 
	    * command if timeout limit reached
	    */
              monitor_last_time = System.currentTimeMillis();
              monitor_last_ins_count = ins_count;
	   	      status_last_time = monitor_last_time;
	   	      status_last_ins_count = ins_count;
              try {
		          ActionListener cmd_listener = new ActionListener() {
		              public void actionPerformed(ActionEvent evt) {
		                  monitor_update();
		              }
		          };
  		          monitor_timer = new Timer(monitor_wait,cmd_listener);
		          monitor_timer.start();
		      } catch (Exception e) {
		       	  log_error(12,"execution startup error " + e.toString());
		      }
	   }
	   private void monitor_update(){
	   /*
	    * 1.  At monitor_wait intervals, update the
	    *     GUI title date and time and the status
	    *     line information.
	    * 
	    * 2.  reset focus to gz390_cmd_line after wto
	    */
		    refresh_wait = false;  // reset main_view wait
	        monitor_next_time = System.currentTimeMillis();
	        monitor_next_ins_count = ins_count;
	        monitor_next_io_count = io_count;
            monitor_cur_interval = monitor_next_time - monitor_last_time;
	   	    monitor_cmd_time_total = (monitor_next_time - start_cmd_time)/1000;
   	    	cur_date = new Date();
    	    title_update();
	   	    check_main_view(); 
	   	    monitor_last_time = monitor_next_time;
	   	    monitor_last_ins_count  = monitor_next_ins_count;
	   	    monitor_last_io_count   = monitor_next_io_count;
	   }
	   /*
	    **************************************************
	    * Command support functions
	    **************************************************  
	    */
   private void init_gz390(String[] args){
       /*
        * Init graphical user interface
        */ 
            main_frame = new JFrame();
            title_update();
            try {
                main_height_max = GraphicsEnvironment.getLocalGraphicsEnvironment().getScreenDevices()[0].getDisplayMode().getHeight() - start_bar_height;
                main_width_max = GraphicsEnvironment.getLocalGraphicsEnvironment().getScreenDevices()[0].getDisplayMode().getWidth();
            } catch (Exception e){

            }
            main_frame.setSize(main_width,main_height);
            main_frame.setLocation(main_loc_x,main_loc_y);
            main_frame.addComponentListener(this);
            build_main_panel();
            monitor_startup();
            main_frame.setVisible(true);
            gz390_cmd_line.requestFocus();
        }
        private void build_main_panel(){ 
        /*
   	     *  Build the main panel with:
   	     *    a.  Scrolling log display
   	     *    b.  command entry field
   	     * 
   	     */
   	        main_panel = new JPanel();
   	        main_panel.setBorder(BorderFactory.createEmptyBorder(0,main_border,main_border,main_border));
            main_panel.setLayout(new FlowLayout(FlowLayout.LEFT)); 
            set_gui_size();
            build_menu_items();
            set_tooltips();
            build_log_view();
            build_gz390_cmd_line();
            build_status_line();
            set_text_font();
            menuBar.add(file_menu);
            menuBar.add(edit_menu);
            menuBar.add(view_menu);
            menuBar.add(help_menu);
            main_frame.setJMenuBar(menuBar);
    	    main_panel.add(main_view);
    	    main_panel.add(cmd_label);
    	    main_panel.add(gz390_cmd_line);
    	    main_panel.add(status_line_label);
            main_panel.add(status_line);
       	    main_frame.getContentPane().add(main_panel);
	        main_frame.addWindowListener(new WindowAdapter() {
	  		       public void windowClosing(WindowEvent e) {
	  		           exit_main(0);
	  		       }
	  		      });
	        refresh_request = true;
        } 
        private void exit_main(int rc){
	        shut_down(rc);
        }
     private void set_gui_size(){
    	 /* 
    	  * calculate gui object sizes based on
    	  * sreen size and font size
    	  */
    	title_height = 56;
  	    menu_height = font_size + font_space;
 	    log_char_height = font_size + font_space;
		log_height = main_height - title_height - menu_height - tool_height - command_height - status_height - applet_status_height;
		log_width  = main_width - scrollbar_width - 4 * main_border;
	    lines_per_page = log_height / log_char_height;
   	    command_height = font_size + font_space 
   	                   + main_border;
   	    status_height  = font_size + font_space
   	                   + main_border;
     }
     private void build_log_view(){
    	 /*
    	  * build scrolling log view based on
    	  * current screen and font size
    	  */
        log_text = new JTextArea();
	    log_text.addMouseListener(this);
        main_view = new JScrollPane(log_text);
        main_view.getVerticalScrollBar().addAdjustmentListener(new AdjustmentListener(){
    	  public void adjustmentValueChanged(AdjustmentEvent e){
    		if (main_view_changed){
                main_view_changed = false;
    			main_view.getVerticalScrollBar().setValue(main_view.getVerticalScrollBar().getMaximum());
    		}       
    	  }});
        main_view.setVerticalScrollBarPolicy(
        JScrollPane.VERTICAL_SCROLLBAR_ALWAYS);
	    main_view.setPreferredSize(   	        		
         	new Dimension(log_width, log_height));
     }
 
     private void build_gz390_cmd_line(){
	/*
     *   Build the command entry field
     */
     	cmd_label = new JLabel("Command: ");
        gz390_cmd_line = new JTextField(command_columns);
        gz390_cmd_line.addActionListener(this);
	    gz390_cmd_line.addMouseListener(this);
        gz390_cmd_line.addKeyListener(this);
        gz390_cmd_line.addFocusListener(this);
     }
     private void build_status_line(){
    	/*
         *   Build the statuts line
         */
     	    status_line_label = new JLabel(" Status: ");
            status_line = new JTextField(command_columns);
            status_line.addActionListener(this);
	        status_line.addMouseListener(this);
            status_line.addKeyListener(this);
            status_line.addFocusListener(this);
         }
     private void build_menu_items(){
    /* 
     *    Build the menu bar
     */
     menuBar = new JMenuBar();
     file_menu = new JMenu("File");
     edit_menu = new JMenu("Edit");
     view_menu = new JMenu("View");
     help_menu = new JMenu("Help");
     file_menu_exit   = new JMenuItem("Exit");
     edit_menu_cut    = new JMenuItem("Cut");
     edit_menu_copy   = new JMenuItem("Copy");
     edit_menu_paste  = new JMenuItem("Paste");
     edit_menu_select_all = new JMenuItem("Select All");
     edit_menu_copy_log   = new JMenuItem("Copy Log");
     edit_menu_notepad = new JMenuItem("Notepad");
     view_menu_mcs    = new JCheckBoxMenuItem("MCS");
     view_menu_mcs.setState(true);
     view_menu_tn3270   = new JCheckBoxMenuItem("TN3270");
     view_menu_graph  = new JCheckBoxMenuItem("Graph");
     help_menu_help       = new JMenuItem("Help");
     help_menu_support    = new JMenuItem("Support");
     help_menu_about      = new JMenuItem("About");
     /*
      * Mnemonic menu bar keys
      */
     file_menu.setMnemonic(KeyEvent.VK_F);
     edit_menu.setMnemonic(KeyEvent.VK_E);
     view_menu.setMnemonic(KeyEvent.VK_V);
     help_menu.setMnemonic(KeyEvent.VK_H);
     file_menu_exit.setMnemonic(KeyEvent.VK_X);
     edit_menu_cut.setMnemonic(KeyEvent.VK_T);
     edit_menu_copy.setMnemonic(KeyEvent.VK_C);
     edit_menu_paste.setMnemonic(KeyEvent.VK_P);
     edit_menu_select_all.setMnemonic(KeyEvent.VK_S);
     edit_menu_copy_log.setMnemonic(KeyEvent.VK_L);
     edit_menu_notepad.setMnemonic(KeyEvent.VK_N);
     view_menu_mcs.setMnemonic(KeyEvent.VK_M);
     view_menu_tn3270.setMnemonic(KeyEvent.VK_3);
     view_menu_graph.setMnemonic(KeyEvent.VK_G);
     help_menu_help.setMnemonic(KeyEvent.VK_H);
     help_menu_support.setMnemonic(KeyEvent.VK_S);
     help_menu_about.setMnemonic(KeyEvent.VK_A);
     /*
      * Set menu bar accelerator keys
      */
     file_menu_exit.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_Z,ActionEvent.CTRL_MASK));
     edit_menu_cut.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_E,ActionEvent.CTRL_MASK));
     edit_menu_copy.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_C,ActionEvent.CTRL_MASK));
     edit_menu_paste.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_V,ActionEvent.CTRL_MASK));
     edit_menu_notepad.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_N,ActionEvent.CTRL_MASK));
     view_menu_mcs.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_M,ActionEvent.CTRL_MASK+ActionEvent.SHIFT_MASK));
     view_menu_tn3270.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_3,ActionEvent.CTRL_MASK+ActionEvent.SHIFT_MASK));
     view_menu_graph.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_G,ActionEvent.CTRL_MASK+ActionEvent.SHIFT_MASK));
     help_menu_help.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_H,ActionEvent.CTRL_MASK+ActionEvent.ALT_MASK));
     help_menu_support.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_S,ActionEvent.CTRL_MASK+ActionEvent.ALT_MASK));
     help_menu_about.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_A,ActionEvent.CTRL_MASK+ActionEvent.ALT_MASK));
     /*
      * add menu action listeners
      */
     file_menu_exit.addActionListener(this);
     edit_menu_cut.addActionListener(this);
     edit_menu_copy.addActionListener(this);
     edit_menu_paste.addActionListener(this);
     edit_menu_select_all.addActionListener(this);
     edit_menu_copy_log.addActionListener(this);
     edit_menu_notepad.addActionListener(this);
     view_menu_mcs.addActionListener(this);
     view_menu_tn3270.addActionListener(this);
     view_menu_graph.addActionListener(this);
     help_menu_help.addActionListener(this);
     help_menu_support.addActionListener(this);
     help_menu_about.addActionListener(this);
     file_menu.add(file_menu_exit); 
     edit_menu.add(edit_menu_cut);
     edit_menu.add(edit_menu_copy);
     edit_menu.add(edit_menu_paste);
     edit_menu.add(edit_menu_select_all);
     edit_menu.add(edit_menu_copy_log);
     edit_menu.add(edit_menu_notepad);
     view_menu.add(view_menu_mcs);
     view_menu.add(view_menu_tn3270);
     view_menu.add(view_menu_graph);
     help_menu.add(help_menu_help);
     help_menu.add(help_menu_support);
     help_menu.add(help_menu_about);
   }
   private void set_tooltips(){
	   /*
	    * set tooltips after font changes
	    */
	     String text_font_pfx = "<html><font size=" + font_size/3 + ">";
	     String text_font_sfx = "</html>";
	     file_menu_exit.setToolTipText(text_font_pfx + "Exit gz390 GUI" + text_font_sfx);
	     edit_menu_cut.setToolTipText(text_font_pfx + "Cut selected text" + text_font_sfx);
	     edit_menu_copy.setToolTipText(text_font_pfx + "Copy selected text to clipboard" + text_font_sfx);
	     edit_menu_paste.setToolTipText(text_font_pfx + "Paste clipboard text (append if log has focus" + text_font_sfx);
	     edit_menu_select_all.setToolTipText(text_font_pfx + "Select all text" + text_font_sfx);
	     edit_menu_copy_log.setToolTipText(text_font_pfx + "Copy current log file to clipboard" + text_font_sfx);
	     edit_menu_notepad.setToolTipText(text_font_pfx + "Launch notepad to edit selected data on clipboard" + text_font_sfx);
	     view_menu_mcs.setToolTipText(text_font_pfx + "MCS WTO and WTOR console" + text_font_sfx);
	     view_menu_tn3270.setToolTipText(text_font_pfx + "TN3270 TPUT and TGET terminal screen" + text_font_sfx);
	     view_menu_graph.setToolTipText(text_font_pfx + "GKS graphic display" + text_font_sfx);
	     help_menu_help.setToolTipText(text_font_pfx + "Display summary of menu selections" + text_font_sfx);
	     help_menu_support.setToolTipText(text_font_pfx + "Link to www.z390.org web site for support information" + text_font_sfx);
	     help_menu_about.setToolTipText(text_font_pfx + "Display information about this version of gz390" + text_font_sfx);
   }
   private void title_update(){
   /*
    * update main frame title with current
    * date and time.
    */	
		 Date cur_date = new Date();
     	 main_frame.setTitle(main_title + "   " + mmddyy.format(cur_date)
     			     + " " + hhmmss.format(cur_date));
   }
   	  public void actionPerformed(ActionEvent event){
   	  /*
   	   * Perform menu and command line requests 
   	   */	
   	  	String event_name = event.getActionCommand().toUpperCase();
   	  	if  (gz390_cmd_line.hasFocus()){
 	  	     cmd_line = gz390_cmd_line.getText();
	  	     reset_gz390_cmd();
   	  	     if  (wtor_running){
  	    		 wtor_reply_string = cmd_line;
  	    		 wtor_running = false;
   	  	     } else {
   	  	          exec_gui_command();
   	  	     }
	         return;
   	  	}
   	  boolean event_ok = false;
   	  char first_char = event_name.charAt(0);
   	  switch (first_char){
   	  case 'A':
   	    if (event_name.equals("ABOUT")){
   	       event_ok = true;
   	       about_command();
	    }
   	    break;
   	  case 'B':
   	    break;
   	  case 'C':	
	  	if (event_name.equals("COPY")){
		   	   event_ok = true;
		   	   if  (log_text == focus_comp){
		   	   	log_text.copy();
		   	   }
		   	   if  (gz390_cmd_line ==  focus_comp){
	                       gz390_cmd_line.copy();
		   	   }
		}
	  	if (event_name.equals("COPY LOG")){
		   	event_ok = true;
    		log_text.requestFocus();
    		log_text.selectAll();
    		log_text.copy();
		}
 	  	if (event_name.equals("CUT")){
		    event_ok = true;
		    if  (gz390_cmd_line ==  focus_comp){
	             gz390_cmd_line.cut();
		    }
		} 	  	
   	  	break;
   	 case 'D':
	  	break;
   	 case 'E':
   	  	if (event_name.equals("EXIT")){
    	  	event_ok = true;
    	  	exit_command();
   	  	} 
   	  	break;
   	  case 'G':
   		if (event_name.equals("GRAPH")){
   			if (!opt_graph){
   				set_view_graph();
   			}
   	    }  
   	    break;
   	 case 'H':
   	    if (event_name.equals("HELP")){
    	    event_ok = true;
       	    help_command();
	    }
   	    break;
   	 case 'J':
        break;
   	 case 'L':
   	    break;	
   	  case 'M':
     	    if (event_name.equals("MCS")){
        	    event_ok = true;
           	    if (!opt_mcs){
           	    	set_view_mcs();
           	    }
    	    }
     	    break;
      case 'N':
	  	if (event_name.equals("NOTEPAD")){
	  		event_ok = true;
	        exec_cmd("NOTEPAD");
		}
	  	break;
      case 'O':
   	  	break;  
	  case 'P':
	  	if (event_name.equals("PASTE")){
	  		   event_ok = true;
	   	       if  (log_text == focus_comp){
                    gui_put_log("PASTE from clipboard starting " + time_stamp());
	   	       	    gui_put_log(getClipboard()); // append to log
	   	            gui_put_log("PASTE from clipboard ending   " + time_stamp());
	   	       }
		   	   if  (gz390_cmd_line ==  focus_comp){
	               gz390_cmd_line.paste();
		   	   }
	     }
	  	 break;
	  case 'R':
	  	 break;
	  case 'S':
	  	 if (event_name.equals("SELECT ALL")){
		   	   event_ok = true;
	   	       if  (log_text == focus_comp){
	   	       	   log_text.selectAll(); 
	   	       }
		   	   if  (gz390_cmd_line ==  focus_comp){
	               gz390_cmd_line.selectAll();
		   	   }
		 }
	   	 if (event_name.equals("SUPPORT")){
	         event_ok = true;
	         exec_cmd("cmd.exe /c Start http://www.z390.org");
	 	 }
         break;
      case 'T':
      	 if (event_name.equals("TN3270")){
      	    if (!opt_tn3270){
        	   	set_view_screen();
            }
      	 } 
      	 break;
      case 'V': 
   	  	break;
   	  }
   	  	/*
   	  	 *  If event_ok not set, exec command
   	  	 */
   	  	if (event_ok == false) {
  			cmd_line = gz390_cmd_line.getText();
  			exec_gui_command();
	  		reset_gz390_cmd();}	        
   	  	}
	   private String time_stamp(){
		   /*
		    * return date and time if opt_timing
		    */
        Date temp_date = new Date(); 
        return mmddyy.format(temp_date)
       + " " + hhmmss.format(temp_date);
	   }
   	  private void exec_gui_command(){
   	  /*
   	   * exec command 
   	   */  	   
  		   cmd_line = gz390_cmd_line.getText();
  		   if  (cmd_line == null || cmd_line.length() == 0){
  		   	   cmd_line = " ";
  		   }
           reset_gz390_cmd();
           process_command(cmd_line); 
   	  } 
   	  private void reset_gz390_cmd(){
   	  /*
   	   * reset gz390_cmd text and set focus
   	   */
  		   gz390_cmd_line.setText("");
           gz390_cmd_line.requestFocus();
   	  }
      private boolean exec_cmd(String cmd){
          /*
           * exec command as separate task
           */
          	   if  (perm_file_execute){
    	           try {
    	  	           Runtime.getRuntime().exec(cmd);
    	  	           return true;
    	  	       } catch(Exception e){
    	  	   	       return false;
    	  	       }
          	   } else {
          	   	   return false;
          	   }
       	  }
      public static String getClipboard() {
      /*
       * Get string text from system clipboard
       */
      	Transferable t = Toolkit.getDefaultToolkit().getSystemClipboard().getContents(null);
          try {
              if (t != null && t.isDataFlavorSupported(DataFlavor.stringFlavor)) {
                  String text = (String)t.getTransferData(DataFlavor.stringFlavor);
                  return text;
              }
          } catch (UnsupportedFlavorException e) {
          } catch (Exception e) {
          }
          return null;
      }
       public static void setClipboard(String str) {
       /*
        * put string to system clipboard
        */
       	StringSelection ss = new StringSelection(str);
          Toolkit.getDefaultToolkit().getSystemClipboard().setContents(ss, null);
      }
       public void keyPressed(KeyEvent e) {
       /*
        * Handle key pressed events
        */
//dsh          displayInfo(e, "KEY PRESSED: ");
           int keyCode = e.getKeyCode();
           int keyMods = e.getModifiers();
           if  (e.isActionKey()){
        	   if (tn3270_kb_lock){
        		   if (keyCode == KeyEvent.VK_UP){
           	   			get_prev_cmd();
           	   			return;
        		   }
        		   if (keyCode == KeyEvent.VK_DOWN){
          	   	   		get_next_cmd();
          	   	   		return;
        		   }
        	   } else {
        		   if (keyCode == KeyEvent.VK_UP){
          	   			scn_addr = scn_addr - max_cols;
          	   			if (scn_addr < 0){
          	   				scn_addr = scn_addr + max_addr;
          	   			}
      	   				tn3270_update_cursor();
          	   			return;
        		   }
        		   if (keyCode == KeyEvent.VK_DOWN){
            		    scn_addr = scn_addr + max_cols;
            		    if (scn_addr >= max_addr){
             	   			scn_addr = scn_addr - max_addr;
             	   		}
         	   			tn3270_update_cursor();
         	   	   		return;
        		   }
        		   if (keyCode == KeyEvent.VK_LEFT){
        			   scn_addr--;
        			   if (scn_addr < 0){
        				   scn_addr = scn_addr + max_addr;
        			   }
    				   tn3270_update_cursor();
        			   return;
        		   }
        		   if (keyCode == KeyEvent.VK_RIGHT){
        			   	scn_addr++;
           		    	if (scn_addr == max_addr){
            	   			scn_addr = 0;
            	   		}
        	   			tn3270_update_cursor();
        	   	   		return;
        		   }
        	   }
          	   if (keyMods == KeyEvent.CTRL_MASK){
              	   if (keyCode == KeyEvent.VK_F1){   // F1 help
               	   	  if (!tn3270_kb_lock){
               	   		  tn3270_aid = 0x6c;  // PA1
               	   		  tn3270_attn = true;
               	   	  }
               	   	  return;
               	   }
              	   if (keyCode == KeyEvent.VK_F2){   // F3 exit
                 	   	  if (!tn3270_kb_lock){
                 	   		  tn3270_aid = 0x6e;  //PA2
                 	   		  tn3270_attn = true;
                 	   	  }
                 	   	  return;
              	   }
               	   if (keyCode == KeyEvent.VK_F3){   // F3 exit
                	   	  if (!tn3270_kb_lock){
                	   		  tn3270_aid = 0x6b;  //PA3
                	   		  tn3270_attn = true;
                	   		  return;
                	   	  }
                	   	  return;
             	   }
               	   return;
          	   }
          	   if (keyMods == (KeyEvent.ALT_MASK | KeyEvent.CTRL_MASK)){ // RPI 216
       		     if (keyCode >= KeyEvent.VK_F1
          			   && keyCode <= KeyEvent.VK_F9){
            	   	  if (!tn3270_kb_lock){
              	   		  tn3270_aid = 0xc1 + keyCode
              	   		                    - KeyEvent.VK_F1;
              	   		  tn3270_attn = true;
               	   	  }
            	   	  return;
          	     }
          	     if (keyCode >= KeyEvent.VK_F10
          			   && keyCode <= KeyEvent.VK_F12){
            	   	  if (!tn3270_kb_lock){
              	   		  tn3270_aid = 0x4a + keyCode 
              	   		                    - KeyEvent.VK_F10;
              	   		  tn3270_attn = true;
               	   	  }
            	   	  return;
          	     }
          	     return;
          	   }
          	   if (keyCode == KeyEvent.VK_F1){   // F1 help
          	   	  if (!tn3270_kb_lock){
          	   		  tn3270_aid = 0xf1;
          	   		  tn3270_attn = true;
          	   	  } else {
          	   		  help_command();
          	   	  }
          	   	  return;
          	   }
         	   if (keyCode == KeyEvent.VK_F2){   // F3 exit
            	   	  if (!tn3270_kb_lock){
            	   		  tn3270_aid = 0xf2;
            	   		  tn3270_attn = true;
            	   	  }
            	   	  return;
         	   }
          	   if (keyCode == KeyEvent.VK_F3){   // F3 exit
           	   	  if (!tn3270_kb_lock){
           	   		  tn3270_aid = 0xf3;
           	   		  tn3270_attn = true;
           	   		  return;
           	   	  } else {
           	   		  process_cancel_key();
           	   	  }
           	   	  return;
        	   }
          	   if (keyCode >= KeyEvent.VK_F4
          			   && keyCode <= KeyEvent.VK_F9){
            	   	  if (!tn3270_kb_lock){
              	   		  tn3270_aid = 0xf4 + keyCode
              	   		                    - KeyEvent.VK_F4;
              	   		  tn3270_attn = true;
               	   	  }
            	   	  return;
          	   }
          	   if (keyCode >= KeyEvent.VK_F10
          			   && keyCode <= KeyEvent.VK_F12){
            	   	  if (!tn3270_kb_lock){
              	   		  tn3270_aid = 0x7a + keyCode 
              	   		                    - KeyEvent.VK_F10;
              	   		  tn3270_attn = true;
               	   	  }
            	   	  return;
          	   }
       		   if (keyCode >= KeyEvent.VK_F13
          			   && keyCode <= KeyEvent.VK_F21){
            	   	  if (!tn3270_kb_lock){
              	   		  tn3270_aid = 0xc1 + keyCode
              	   		                    - KeyEvent.VK_F13;
              	   		  tn3270_attn = true;
               	   	  }
            	   	  return;
          	   }
          	   if (keyCode >= KeyEvent.VK_F22
          			   && keyCode <= KeyEvent.VK_F24){
            	   	  if (!tn3270_kb_lock){
              	   		  tn3270_aid = 0x4a + keyCode 
              	   		                    - KeyEvent.VK_F22;
              	   		  tn3270_attn = true;
               	   	  }
            	   	  return;
          	   }
           } else {  // not action key
          	   if (keyCode == KeyEvent.VK_CANCEL){
          	   	  process_cancel_key();
          	   }
        	   if (keyCode == KeyEvent.VK_ENTER){
        		   if (!tn3270_kb_lock){
        			   tn3270_attn = true;
        			   tn3270_aid = tn3270_enter_code;
        		   }
        	   }
        	   if (keyCode == KeyEvent.VK_BACK_SPACE){
        		   if (!tn3270_kb_lock){
                       if (scn_addr > 0){
                    	   scn_addr--;
                       } else {
                    	   scn_addr = max_addr - 1;
                       }
                       tn3270_update_cursor();
        		   }
        	   }
           }
     	   if (keyCode == KeyEvent.VK_CLEAR
     			   || (keyCode == KeyEvent.VK_C && keyMods == KeyEvent.CTRL_MASK)){
         	  if (!tn3270_kb_lock){
         	  	  tn3270_aid = 0x6d; // clear key
         	  	  tn3270_attn = true;
         	  }
    	   }
  	   }
       private void process_cancel_key(){
       /*
        * cancel cmd, or gui cmd in response to
        * F3 or CTRL-BREAK
        */	
   	      abort_error(102,"Aborting due to external shutdown request");
       }
       public void keyTyped(KeyEvent e) {
       /*
        * Handle key typed events
        */
    //dsh displayInfo(e, "KEY TYPED: "); 
       	  /*
       	   * collect any characters for accept
       	   * which are placed in gz390_cmd_line
       	   * by accept wait loop if not there 
       	   * already.  First accept they are there,
       	   * but following ones not?  Hooky fix!
       	   */
      	   if (e.getKeyChar() == tn3270_tab_code){
      		   if (!tn3270_kb_lock){
      			   tn3270_tab();
      		   }
      		   return;
      	   }
      	   if (!tn3270_kb_lock 
      			   && e.getKeyChar() != KeyEvent.VK_ENTER
      			   && e.getKeyChar() != KeyEvent.VK_BACK_SPACE
      			   && (e.getModifiers() & KeyEvent.CTRL_MASK) == 0){
             if (gui_tot_key < max_keys){        	   
        	   if (tn3270_input_field()){
        		   if ((scn_attr[cur_fld_addr] & tn3270_numeric_mask) == tn3270_numeric_mask
        				&& (e.getKeyChar() < '0'
        						|| e.getKeyChar() > '9')){
        			   scn_addr--;
        			   sound_alarm();
        			   status_line.setText("Alarm - invalid key for numeric field");
        		   } else {
            		   tn3270_modify_field();
            		   scn_byte[scn_addr] = (byte)e.getKeyChar();
            		   scn_char[scn_addr] = e.getKeyChar();
        		   }
        		   tn3270_update_screen(scn_addr);
        		   tn3270_next_input_addr();
        		   tn3270_update_cursor();
        	   } else {
    			   scn_addr--;
    			   sound_alarm();
    			   status_line.setText("Alarm - invalid key for protected field");
        	   }
             }
    	   } else {
    		   gui_key_code_char[gui_tot_key] = e.getKeyCode() << 16 | (int)e.getKeyChar();
    		   gui_tot_key++;
    	   }
       }
       public void keyReleased(KeyEvent e) {
       /* 
        * Handle key released events
        */
  //dsh         displayInfo(e, "KEY RELEASED: ");
       }
       protected void displayInfo(KeyEvent e, String s){
        String keyString, modString, tmpString,
               actionString, locationString;

        //You should only rely on the key char if the event
        //is a key typed event.
        int id = e.getID();
        if (id == KeyEvent.KEY_TYPED) {
            char c = e.getKeyChar();
            keyString = "key character = '" + c + "'";
        } else {
            int keyCode = e.getKeyCode();
            keyString = "key code = " + keyCode
                        + " ("
                        + KeyEvent.getKeyText(keyCode)
                        + ")";
        }

        int modifiers = e.getModifiersEx();
        modString = "modifiers = " + modifiers;
        tmpString = KeyEvent.getModifiersExText(modifiers);
        if (tmpString.length() > 0) {
            modString += " (" + tmpString + ")";
        } else {
            modString += " (no modifiers)";
        }

        actionString = "action key? ";
        if (e.isActionKey()) {
            actionString += "YES";
        } else {
            actionString += "NO";
        }

        locationString = "key location: ";
        int location = e.getKeyLocation();
        if (location == KeyEvent.KEY_LOCATION_STANDARD) {
            locationString += "standard";
        } else if (location == KeyEvent.KEY_LOCATION_LEFT) {
            locationString += "left";
        } else if (location == KeyEvent.KEY_LOCATION_RIGHT) {
            locationString += "right";
        } else if (location == KeyEvent.KEY_LOCATION_NUMPAD) {
            locationString += "numpad";
        } else { // (location == KeyEvent.KEY_LOCATION_UNKNOWN)
            locationString += "unknown";
        }
        String newline = "\n";
        System.out.println(s + newline
                           + "    " + keyString + newline
                           + "    " + modString + newline
                           + "    " + actionString + newline
                           + "    " + locationString + newline);
       
    }
    public void mousePressed(MouseEvent e) {
    /*
     * Popup edit menu on right mouse ck
     */	
    	   check_main_view();
             if (e.getButton() == MouseEvent.BUTTON3){	
              if (popup_edit_menu == null){
                 popup_edit_menu = new JPopupMenu();
                 JMenuItem popup_edit_menu_cut      = new JMenuItem("Cut");
                 JMenuItem popup_edit_menu_copy     = new JMenuItem("Copy");
                 JMenuItem popup_edit_menu_paste  = new JMenuItem("Paste");
                 JMenuItem popup_edit_menu_select_all = new JMenuItem("Select All");
                 JMenuItem popup_edit_menu_copy_log = new JMenuItem("Copy Log");
                 JMenuItem popup_edit_menu_notepad = new JMenuItem("Notepad");
                 popup_edit_menu.add(popup_edit_menu_cut);
                 popup_edit_menu.add(popup_edit_menu_copy);
                 popup_edit_menu.add(popup_edit_menu_paste);
                 popup_edit_menu.add(popup_edit_menu_select_all);
                 popup_edit_menu.add(popup_edit_menu_copy_log);
                 popup_edit_menu.add(popup_edit_menu_notepad);
                 popup_edit_menu_cut.addActionListener(this);
                 popup_edit_menu_copy.addActionListener(this);
                 popup_edit_menu_paste.addActionListener(this);
                 popup_edit_menu_select_all.addActionListener(this);
                 popup_edit_menu_copy_log.addActionListener(this);
                 popup_edit_menu_notepad.addActionListener(this);
              } 
              Component mouse_comp = e.getComponent();
              if  (mouse_comp == log_text){
                   focus_comp = log_text; //force log focus for edit
              }
              popup_edit_menu.show(mouse_comp,e.getX(),e.getY());
           }
    }
    public void mouseReleased(MouseEvent e) {

    }

    public void mouseEntered(MouseEvent e) {

    }

    public void mouseExited(MouseEvent e) {

    }

    public void mouseClicked(MouseEvent e) {

    }
    public void focusLost(FocusEvent e) {
    /*
     * last component to lose focus (ignored for now)
     */	
    }

    public void focusGained(FocusEvent e) {
    /*
     * Save last component to get focus
     */	
     	   Component temp_comp = e.getComponent();

    	   if (temp_comp == log_text
    	   		|| focus_comp == main_view){
    	   	  focus_comp = log_text;
    	   }
    	   if  (temp_comp == gz390_cmd_line){
    	   	   focus_comp = temp_comp;
    	   }    	   
    }
	    private void exit_command(){
	    /*
	     * abort command if runnung and turn off cmd_mode
	     * If no command running and not cmd_mode,
	     * then exit gz390.
	     */
   	  		exit_main(0);
	    }
	    protected static ImageIcon createImageIcon(String path,
	                                               String description) {
	        java.net.URL imgURL = gz390.class.getResource(path);
	        if (imgURL != null) {
	            return new ImageIcon(imgURL, description);
	        } else {
	            System.err.println("Couldn't find file: " + path);
	            return null;
	        }
	    }
	    private synchronized void check_main_view(){
	    /*
	     * if main window size has changed due to
	     * user streching without window event handler
	     * triggering update, do it now.
	     */
	    	char alt_char = ' ';
    		if (tn3270_cursor){ 
    			tn3270_cursor_count--;
    			if (tn3270_cursor_count <= 0){
    				tn3270_cursor_count = tn3270_cursor_wait_int;
    				if (!tn3270_cursor_alt){
    					tn3270_cursor_alt = true;
    					if (scn_char[tn3270_cursor_scn_addr] != tn3270_cursor_sym){
    						alt_char = tn3270_cursor_sym;
    					} else {
    						alt_char = '?'; // blink ? for underline
    					}
    					tn3270_update_screen_char(alt_char,tn3270_cursor_text_addr);
    				} else {
    					tn3270_cursor_alt = false;
    					tn3270_update_screen_char(scn_char[tn3270_cursor_scn_addr],tn3270_cursor_text_addr);
    				}
    				refresh_request = true;
    			}
    		}
	    	if (refresh_request 
	    		|| main_width != main_frame.getSize().getWidth()
	    		|| main_height != main_frame.getSize().getHeight()){
	    		main_width = (int) main_frame.getSize().getWidth();
	    		main_height = (int) main_frame.getSize().getHeight();
	    		update_main_view();
	            gz390_cmd_line.requestFocus();
            	refresh_request = false;
	    	}
	    }
        private void update_main_view(){
        /*
         * update log and command line size 
         * following any of the following changes:
         *   1.  Change in window size
         *   2.  Change in font size
         */	
        	if (refresh_wait){
        		refresh_request = true;
        		return; // wait for next monitor interal
        	}
 	        refresh_wait = true;
       		log_height = main_height - title_height - menu_height - command_height - status_height - applet_status_height;
       		log_width  = main_width - scrollbar_width - 4 * main_border;
            main_panel.setSize(main_width - 4 * main_border,main_height - title_height - menu_height - main_border);
            lines_per_page = log_height / log_char_height;
   	        main_view.setPreferredSize(   	        		
       		new Dimension(log_width, log_height));
   	        rebuild_main_panel();
       		main_frame.setVisible(true);
        }
        private void set_view_mcs(){
        	/*
        	 * display mcs scrolling wto/wtor panel
        	 */
            set_main_view(log_text);
        	gui_view = gui_view_mcs;
   	    	opt_mcs   = true;
   	    	opt_tn3270  = false;
   	    	opt_graph = false;
   	    	view_menu_mcs.setSelected(true);
   	    	view_menu_tn3270.setSelected(false);
   	    	view_menu_graph.setSelected(false);
   			update_main_view();
      		refresh_request = true;
        }
        private void set_view_screen(){
       	 /*
       	  * build screen view based on
       	  * current screen and font size
       	  * Notes:
       	  *   1.  Purge and redefine main_panel with 
       	  *       new main_view
       	  *   2.  Turn off focus subsystem to see tab key
       	  */
       	if (scn_text == null){ // init first time
       		scn_text = new JTextArea();
       		scn_text.setFont(new Font("Courier",Font.BOLD,font_size));
            scn_text.addMouseListener(this);
            tn3270_clear_screen();
            tn3270_update_screen_char(' ',0);
       	}
        set_main_view(scn_text);
       	gui_view = gui_view_screen;
	    opt_mcs    = false;
   	    opt_tn3270   = true;
   	    opt_graph  = false;
   	    view_menu_mcs.setSelected(false);
   	    view_menu_tn3270.setSelected(true);
   	    view_menu_graph.setSelected(false);
		update_main_view();
   	    refresh_request = true;
        }
        private void set_view_graph(){
        	/*
        	 * display graph for QUAM GKS commands
        	 */
           	if (graph_grid == null){ // init first time
           		graph_grid = new JTextArea(
           			"\n  GUI grahics support not done yet"
           	      + "\n  Click on View to see MCS and TN3270 views"
           	      + "\n  Run demogui1 for simple WTO, WTOR, WAIT interface"
           	      + "\n  Run demogui2 for WTO, WTOR,ECB MIP rate calculation"
           	      + "\n  Run demogui3 for TPUT, TGET TN3270 EDIT mode interface"
           	      + "\n  Run demogui4 for TPUT, TGET TN3270 data stream demo (useing edit mode until done)"
           		);
           		graph_grid.addMouseListener(this);
           	}
            set_main_view(graph_grid);
        	gui_view = gui_view_graph;
			opt_mcs   = false;
   			opt_tn3270  = false;
   			opt_graph = true;
   			view_menu_mcs.setSelected(false);
   			view_menu_tn3270.setSelected(false);
   			view_menu_graph.setSelected(true);
   			update_main_view();
   			refresh_request = true;
        }
        private void set_main_view(Component x){
        	/*
        	 * redefine main_view scrolling pane
        	 */
        	main_panel.removeAll();
            main_view = new JScrollPane(x);
            main_view.getVerticalScrollBar().addAdjustmentListener(new AdjustmentListener(){
        	  public void adjustmentValueChanged(AdjustmentEvent e){
        		if (main_view_changed){
                    main_view_changed = false;
        			main_view.getVerticalScrollBar().setValue(main_view.getVerticalScrollBar().getMaximum());
        		}       
        	  }});
            main_view.setVerticalScrollBarPolicy(
            JScrollPane.VERTICAL_SCROLLBAR_ALWAYS);
    	    main_view.setPreferredSize(   	        		
            new Dimension(log_width, log_height));
        	rebuild_main_panel();
        }
        private void rebuild_main_panel(){
        /*
         * rebuild main_panel with current
         * main_view, gz390_cmd and status lines
         * with or without labels to fix current
         * main_panel size.
         */
        	main_panel.removeAll();
        	main_panel.add(main_view);    
        	/* 
        	 * determine if labels will fit
        	 */
            if  (main_width >= labels_min_width
                 && font_size <= labels_max_font){
                 labels_visible = true;
            } else {
            	 labels_visible = false;
            }
            /*
             * add the optional labels and lines
             */
            if (labels_visible){
            	main_panel.add(cmd_label);
            	label_width = cmd_label.getWidth();
            } else {
            	label_width = 0;
            }
    		command_columns = (log_width - label_width)/(gz390_cmd_line.getPreferredSize().width/gz390_cmd_line.getColumns()) - 1;
            gz390_cmd_line.setColumns(command_columns);
            // disable focus subsystem to process tab key
            gz390_cmd_line.setFocusTraversalKeysEnabled(false); 
            main_panel.add(gz390_cmd_line);
            if  (status_visible){
                if  (labels_visible){
            	    main_panel.add(status_line_label);
                }
                status_line.setColumns(command_columns);
                main_panel.add(status_line);
            }
        }
        public void componentHidden(ComponentEvent e) {
    //dsh    	System.out.println("componentHidden event from "
    //dsh    		       + e.getComponent().getClass().getName());
            }

            public void componentMoved(ComponentEvent e) {
     //dsh           Component c = e.getComponent();
     //dsh           System.out.println("componentMoved event from "
     //dsh                          + c.getClass().getName()
     //dsh                          + "; new location: "
     //dsh                          + c.getLocation().x
     //dsh                          + ", "
     //dsh                          + c.getLocation().y);
            }

            public void componentResized(ComponentEvent e) {
     //dsh           Component c = e.getComponent();
     //dsh           System.out.println("componentResized event from "
     //dsh                          + c.getClass().getName()
     //dsh                          + "; new size: "
     //dsh                          + c.getSize().width
     //dsh                          + ", "
     //dsh                          + c.getSize().height);
                               update_main_view();
            }

            public void componentShown(ComponentEvent e) {
   //dsh     	System.out.println("componentShown event from "
   //dsh     		       + e.getComponent().getClass().getName());
            }
     /***********************************************
      * Private screen, graph, keybaord, mouse, sound 
      * functions
      ***********************************************/
private void tput_edit_buffer(byte[] buff, int lbuff){
	/*
	 * update screen from tput ebcdic buffer
	 * using line at at time edit mode
	 */
	String text = get_ascii_string(buff,lbuff);
	int text_start = 0;
	int text_end   = 0;
	gui_cur_col = 1;
	while (text_start < text.length()){
		if (gui_cur_row == 1){
			scn_addr = 0;
		} else {
			scn_addr = (gui_cur_row-1)*(max_cols+2);
		}
		text_end = text_start + max_cols;
        if (text_end > text.length()){
        	text_end = text.length();
        }
        scn_text.replaceRange(text.substring(text_start,text_end),scn_addr,scn_addr+text_end-text_start);
        text_start = text_end;
        scn_next_line();
	}
	update_main_view();
}
private void tn3270_tput_buffer(){
	/*
	 * update screen from tn3270 data stream buffer
	 */
	tput_index = 0;
	switch (tpg_type){
	case 0x01: // asis and noedit  RPI 219
		tn3270_get_tput_byte(); // first byte always write
		if (tput_buff_byte == tn3270_erase_write){
			tn3270_clear_screen();
		}
		tn3270_get_tput_byte(); // second byte always wcc
		tn3270_write_control_char();
		break;
	case 0x03: // fullscr
		tn3270_get_tput_byte(); // first byte esc or wcc
		if (tput_buff_byte == tn3270_esc){
		    tn3270_get_tput_byte(); // wrt,wcc follows esc
			if (tput_buff_byte == tn3270_erase_write){
				tn3270_clear_screen();
			}
			tn3270_get_tput_byte(); // wcc following esc,wrt
		}
		tn3270_write_control_char();
	    break;
	}   
	while (tput_index < tput_len){
		tn3270_get_tput_byte();
		switch (tput_buff_byte){
		case 0x05: // PT  program tab
			tn3270_tab();
			break;
		case 0x11: // SBA set buffer address
			scn_addr = tn3270_get_buff_addr();
			break;
		case 0x12: // EUA erase unprotected to address
            tn3270_eua();
			break;
		case 0x13: // IC insert cursor
			tn3270_cursor = true;
			tn3270_update_cursor();
			break;
		case 0x1d: // SF start new field
			tn3270_start_field();
			break;
		case 0x28: // SA  set extended attribute within field
			tn3270_eds_set_field_attribute();
			break;
		case 0x29: //SFE extended data stream field start
			tn3270_eds_start_field();
			break;
		case 0x3C: // RA repeat to address
            tn3270_ra();
			break;
		default: // write data in next input field postion
			if (scn_fld[scn_addr] == tn3270_field){
				tn3270_drop_field(scn_addr);
			}
			if (tput_buff_byte == 0){
 		    	scn_byte[scn_addr] = 0;
 		    	scn_char[scn_addr] = ' ';
 		    } else {
 		    	scn_byte[scn_addr] = (byte)tz390.ebcdic_to_ascii[tput_buff_byte];
 		        scn_char[scn_addr] = (char)scn_byte[scn_addr];
 		    }
			tn3270_update_screen(scn_addr);
			tn3270_next_field_addr();
		}
	}
}
private void tn3270_get_tput_byte(){
	/*
	 * get next tput buffer byte
	 * in tput_buff_byte else abort
	 */
	if (tput_index < tput_len){
		tput_buff_byte = tput_byte[tput_index] & 0xff;
		tput_index++;
	} else {
		abort_error(105,"tput read past end of buffer");
	}
}
private void tn3270_next_field_addr(){
	/*
	 * incr scn_addr and wrap if at end of screen
	 */
	scn_addr++;
	if (scn_addr >= max_addr){
		scn_addr = 0;
	}
}
private void tn3270_next_input_addr(){
	/*
	 * incr scn_addr to next input field addr
	 */
	scn_addr++;
	if (scn_addr >= max_addr){
		scn_addr = 0;
	}
	if  ((scn_attr[scn_addr] & tn3270_protect_mask) == tn3270_protect_mask){
		tn3270_next_input_field();
	} 
}
private void tn3270_eua(){
	/*
	 * erase unprotected fields from current sba
	 * to ending sba
	 */
	int sba_end = tn3270_get_buff_addr();
	int sba = scn_addr;
	while (sba != sba_end){
		if ((scn_attr[sba] & tn3270_protect_mask) == 0){ 
			// erase and reset mdt in unprotected fields
			scn_byte[sba] = 0;
			scn_char[sba] = ' ';
 		    tn3270_update_screen(sba);
			scn_attr[sba] = scn_attr[sba] & tn3270_mdt_off;
		}
	}
}
private void tn3270_ra(){
	/* 
	 * repeat character to sba address
	 */
	int sba_end = tn3270_get_buff_addr();
	if (sba_end >= max_addr){
		abort_error(103,"tn3270 ra addr error");
	    return;
	}
	byte ra_byte = tput_byte[tput_index];
	tput_index++;
	int sba = scn_addr;
	boolean ra_done = false;
	while (!ra_done){
		if (scn_fld[sba] == tn3270_field){
			tn3270_drop_field(sba);
		}
		if (ra_byte == 0){
			scn_byte[sba] = 0;
		    scn_char[sba] = ' ';
		} else {
			scn_byte[sba] = ra_byte;
			scn_char[sba] = (char)tz390.ebcdic_to_ascii[ra_byte & 0xff];
		}
		tn3270_update_screen(sba);
		sba++;
		if (sba >= max_addr){
			sba = 0;
		}
		if (sba == sba_end)ra_done = true;
	}
}
private void tn3270_tab(){
	/*
	 * tab to next input field from current field
	 */
     tn3270_next_input_field();
     tn3270_update_cursor();
}
private void tn3270_next_input_field(){
	/*
	 * find next input field starting at scn_addr
	 * with wrap and set scn_addr and cursor if on.
	 */
    int index = 0;
    int sba_first = scn_addr;
    int sba_next = max_addr;
    while (index < fld_input_tot){
    	cur_fld_addr = fld_input_addr[index];
    	if (cur_fld_addr > scn_addr
    		&& cur_fld_addr < sba_next){
    		sba_next = cur_fld_addr; 
    	} else if (cur_fld_addr < sba_first){
    		sba_first = cur_fld_addr;
    	}
    	index++;
    }
    if (sba_next != max_addr){
    	scn_addr = sba_next+1;
    	if (scn_addr == max_addr){
    		scn_addr = 0;
    	}
    	tn3270_update_cursor();
    } else if (sba_first != scn_addr){
    	scn_addr = sba_first + 1;
    	if (scn_addr == max_addr){
    		scn_addr = 0;
    	}
    	tn3270_update_cursor();
    }
}
private boolean tn3270_input_field(){
	/*
	 * return true if scn_addr is in unprotected
	 * input field and set fld_addr
	 * Note:
	 *  1. True also returned if no fields
	 *     and fld_addr set to -1 indicating none
	 */
	cur_fld_addr = -1;
	if (fld_tot == 0){
		return true;
	}
	cur_fld_addr = fld_addr[fld_tot-1];
    int index = 0;
	while (index < fld_tot 
			&& fld_addr[index] < scn_addr){
		cur_fld_addr = fld_addr[index];
		index++;
	}
	if ((scn_attr[cur_fld_addr] & tn3270_protect_mask) == 0){
		return true;
	} else {
		return false;
	}
}
private void tn3270_modify_field(){
	/*
	 * update modified field attribute 
	 * bit at fld_addr if any input fields
	 * Note:
	 *   1.  fld_addr must be set by tn3270_input
	 */
	if (fld_input_tot > 0){
		scn_attr[cur_fld_addr] = scn_attr[cur_fld_addr] 
		                     | tn3270_mdt_mask;
	}
}
private void tn3270_update_cursor(){
	/*
	 * update cursor for IC command or change
	 * in focus due to screen input or tab.
	 * 1.  Update scn_addr to next input field
	 *     position or turn off cursor if none
	 *     found.
	 * 2.  Turn on blinking cursor at position found
	 */
	if (!tn3270_cursor){
		return;
	}
	if (scn_addr != tn3270_cursor_scn_addr){ // put char back at prev cursor position
		tn3270_update_screen_char(scn_char[tn3270_cursor_scn_addr],tn3270_cursor_text_addr);
	}
	tn3270_cursor_scn_addr = scn_addr;
	int temp_row = scn_addr/max_cols;
	tn3270_cursor_text_addr = temp_row * (max_cols+2) + (scn_addr - temp_row * max_cols);
	refresh_request = true;
}
private void tn3270_update_screen(int sba){
	/*
	 * update scn_text jtextarea from scn_byte
	 * for tn3270 edit and fullscn modes
	 */
	int row = sba / max_cols;
	int col = sba - row * max_cols;
	int screen_addr = row * (max_cols +2) + col;
	tn3270_update_screen_char(scn_char[sba],screen_addr);
}
private void tn3270_update_screen_char(char screen_char,int screen_addr){
	/*
	 * update jtextarea screen character
	 */
	scn_text.replaceRange("" + screen_char,screen_addr,screen_addr + 1);
}
private void tn3270_get_screen_input(){
	/*
	 * fill tget_byte buffer with the following:
	 *   1, action key  = enter, PF, PA, or clear key)
	 *   2, sba of cursor
	 *   3. sba code x'11', sba addr, modified data bytes
	 *      for each unprotected field else
	 *   4. Modified data bytes for unformated
	 *      screen with no input fields 
	 */
	tget_byte[0] = (byte) tn3270_aid;
	tget_byte[1] = (byte)sba_to_ebc[scn_addr >> 6];
	tget_byte[2] = (byte)sba_to_ebc[scn_addr & 0x3f];
	if (tn3270_aid != tn3270_enter_code){
		tget_len = 3; // return aid with sba for cursor
		return;
	}
	if (fld_input_tot == 0){
		tn3270_unformatted_input();
	} else {
		tn3270_formatted_input();
	}
}
private void tn3270_unformatted_input(){
	/*
	 * return non-null data bytes
	 */
}
private void tn3270_formatted_input(){
	/*
	 * return modified input fields
	 * preceeded with sba code x'11' and sba addr
	 */
	tget_index = 3;             
	int index = 0;
	while (index < fld_input_tot){
		cur_fld_addr = fld_input_addr[index];
	    if ((scn_attr[cur_fld_addr] & tn3270_mdt_mask)
		     == tn3270_mdt_mask){
		    if (tget_index + 3 <= tget_len){
		    	tget_byte[tget_index] = tn3270_sba_cmd;
		    	tget_byte[tget_index+1] = (byte)sba_to_ebc[(cur_fld_addr+1) >> 6];
		    	tget_byte[tget_index+2] = (byte)sba_to_ebc[(cur_fld_addr+1) & 0x3f];
		    	tget_index = tget_index +3;
		    } else {
				abort_error(104,"tget input buffer overrun");
			    return;
		    }
            int sba = cur_fld_addr + 1;
            if (sba == max_addr){
            	sba = 0;
            }
            while (scn_fld[sba] != tn3270_field){
            	if (scn_byte[sba] != 0){
            		if (tget_index < tget_len){
            			if (tz390.opt_ascii){
            				tget_byte[tget_index] = (byte)scn_char[sba];
            			} else {
            				tget_byte[tget_index] = tz390.ascii_to_ebcdic[(byte)scn_char[sba] & 0xff];
            			}
            			tget_index++;
            		} else {
            			abort_error(113,"tget input buffer overrun");
            			return;
            		}
            	}
            	sba++;
            	if (sba == max_addr){
            		sba = 0;
            	}
            }
	    }
	    index++;
	}
	if (tget_index < tget_len){
		tget_len = tget_index; // set actual length
	}
}
private void tn3270_write_control_char(){
	/*
	 * execute wcc from next byte in buffer
     * WWC 0xC3 = clear screen, reset KB and MDT's
     * bit 0   - even bit count 
     * bit 1   - reset screen
     * bit 2-3 - printout format
     * bit 4   - start print
     * bit 5   - sound alarm
     * bit 6   - keyboard restore
     * bit 7   - reset modify data tags (MDT)
	 */
	if ((tput_buff_byte & 0x40) != 0){ // reset screen
        // RPI 222 removed tn3270_clear_screen()
	}
	if  ((tput_buff_byte & 0x04) != 0){ // sound alarm	
		sound_alarm();
		status_line.setText("Alarm message");
	}
	if  ((tput_buff_byte & 0x02) != 0){ // reset keyboard
	    tn3270_kb_lock = false; // allows kb input
	} else {
		tn3270_kb_lock = true;
		status_line.setText("Display only with keyboard locked");
		
	}
	if ((tput_buff_byte & tn3270_mdt_mask) == tn3270_mdt_mask){  // reset mult data tags
		tn3270_reset_mdt();
	}
}
private void sound_alarm(){
	/*
	 * sound alarm by sending ascii bell x'07' 
	 * to System.out
	 * Notes:
	 *   1.  Tried Toolkit.getToolkitDefault().beep();
	 *       and it didn't work
	 *   2.  To get bell to work, I had to go to 
	 *       Windows XP Control Panel, select Sounds,
	 *       and assign the Windows default for "Alert"
	 *       sound to "Windows XP error" sound (it was none)
	 */
	System.out.print((char) tn3270_bell); // RPI 220 use ascii bell x'07'
}
private void tn3270_clear_screen(){
	/*
	 * clear screen and reset fields
	 */
	  	Arrays.fill(scn_char,0,max_addr,' ');
		Arrays.fill(scn_byte,0,max_addr,(byte)0);
		Arrays.fill(scn_fld,0,max_addr,(byte)0);
		Arrays.fill(scn_attr,0,max_addr,0);
		fld_tot = 0;
		fld_input_tot = 0;
	    scn_addr = 0;
	    tn3270_cursor = false;
	    int index = 0;
	    String text = "";
	    while (index < max_rows){
	    	text = text + String.valueOf(scn_char,0,max_cols) + "\r\n";
	    	index++;
	    }
	    scn_text.setText(text);
}
private void tn3270_reset_mdt(){
	/*
	 * reset all mdt bits so only changes will 
	 * be input.
	 */
	int index = 0;
	while (index < fld_input_tot){
	    scn_attr[fld_input_addr[index]] = scn_attr[fld_input_addr[index]] & tn3270_mdt_off;
	    index++;
	}
}
private int tn3270_get_buff_addr(){
	/*
	 * return buffer address from next 2 bytes
	 * Notes:
	 *   1.  Wrap screen if sba > max_addr
	 */
	int sba = -1;
	if (tput_index < tput_len -2){
		int high_bits = ebc_to_sba[tput_byte[tput_index] & 0xff];
		int low_bits = ebc_to_sba[tput_byte[tput_index+1] & 0xff];
		tput_index = tput_index+2;
		sba = (high_bits << 6) | low_bits;
		if (sba >= max_addr){
			return sba - (sba/max_addr)*max_addr;
		}
	} else {
		abort_error(106,"tn3270 command buffer overrun");
	    return -1;
	}
	return sba;
}
private void tn3270_start_field(){
	/* 
	 * start field in tn3270 buffer at current sba.
	 *   1.  Set current tn3270_field_attr.
	 *   2.  Set 1 protected blank at start of field.
	 *   3.  Save unique unprotected fields
	 *       in ascending order until next wcc
	 *       clears them all.
	 * 
	 * field attribute byte
	 * bit  0-1 - set based on remaining bits
	 * bit  2   - protected output
	 * bit  3   - numeric (protected & numeric = skip)
	 * bit  4-5 - display format
	 *              00 - normal intensity, no light pen
	 *              01 - normal intensity, light pen
	 *              10 - high intensity, no light pen
	 *              11 - field not displayed
	 * bit  6   - reserved
	 * bit  7   - modified data tag
	 */
	 scn_fld[scn_addr] = tn3270_field;
	 cur_fld_attr = tput_byte[tput_index] & 0xff;
	 scn_attr[scn_addr] = cur_fld_attr;
	 tput_index++;
	 tn3270_add_field_addr(); 
	 if ((cur_fld_attr & tn3270_protect_mask) == 0){
		 tn3270_add_input_field_addr();
	 }
	 scn_char[scn_addr] = ' ';
	 tn3270_update_screen(scn_addr);
	 tn3270_next_field_addr();
}
private void tn3270_add_field_addr(){
	/*
	 * add scn_addr to fld_addr array if new
	 * and sort after new add.
	 */
	 int index = 0;
	 while (index < fld_tot){
		 if (scn_addr == fld_addr[index]){
			 return;
		 }
		 index++;
	 }
	 fld_addr[fld_tot] = scn_addr;
	 fld_tot++;
	 if (fld_tot > 1){
			// sort all field addresses for use in search
		    Arrays.sort(fld_addr,0,fld_tot);
	 }
}
private void tn3270_add_input_field_addr(){
	/*
	 * add scn_addr to fld_input_addr array
	 * if new and sort after new add.
	 */
	 int index = 0;
	 while (index < fld_input_tot){
		 if (scn_addr == fld_input_addr[index]){
			 return;
		 }
		 index++;
	 }
	 fld_input_addr[fld_input_tot] = scn_addr;
	 fld_input_tot++;
	 if (fld_input_tot > 1){
		// sort input field addresses for use in search
	    Arrays.sort(fld_input_addr,0,fld_input_tot);
	 }
}
private void tn3270_drop_field(int sba){
	/*
	 * remove field definition at sba
	 */
	scn_fld[sba] = 0;
	int index = 0;
	while (index < fld_tot){
		if (fld_addr[index] == sba){
			index++;
			while (index < fld_tot){
				fld_addr[index-1] = fld_addr[index];
				index++;
			}
			fld_tot--;
			if ((scn_attr[sba] & tn3270_protect_mask) == 0){
				tn3270_drop_input_field(sba);
			}
			return;
		}
		index++;
	}
}
private void tn3270_drop_input_field(int sba){
	/*
	 * remove input field
	 */
	int index = 0;
	while (index < fld_input_tot){
		if (fld_input_addr[index] == sba){
			index++;
			while (index < fld_input_tot){
				fld_input_addr[index-1] = fld_input_addr[index];
				index++;
			}
			fld_input_tot--;
			return;
		}
		index++;
	}
}
private void tn3270_eds_start_field(){
	/* 
	 * entended data stream start field
	 * first byte is count of attribute pairs
	 *   type attribute
	 *   C0   basic field attribute
	 *   41   extended highlighting
	 *   42   color
	 *   
	 *   1.  Set current tn3270_field_attr
	 *                   tn3270_field_highlight
	 *                   tn3270_color.
	 *   2.  Set 1 protected blank at start of field.
	 *   3.  Save unique unprotected fields
	 *       in ascending order until next wcc
	 *       clears them all.
	 * 
	 * basic field attribute byte
	 * bit  0-1 - set based on remaining bits
	 * bit  2   - protected output
	 * bit  3   - numeric (protected & numeric = skip)
	 * bit  4-5 - display format
	 *              00 - normal intensity, no light pen
	 *              01 - normal intensity, light pen
	 *              10 - high intensity, no light pen
	 *              11 - field not displayed
	 * bit  6   - reserved
	 * bit  7   - modified data tag
	 * 
	 * highlight attribute byte
	 * 00 - normal
	 * F1 - blink
	 * F2 - reverse video
	 * F4 - underscore
	 * 
	 * color attribute byte
	 * 00 Default 
     * F1 Blue 
     * F2 Red 
     * F3 Pink 
     * F4 Green 
     * F5 Turquoise 
     * F6 Yellow 
     * F7 White 
	 */
	 int count = tput_byte[tput_index] & 0xff;
	 if (count < 1 || count > 3 || tput_index + 2 * count > max_buff){
		 abort_error(111,"invalid tn3270 sfe count " + count);
		 return;
	 }
	 tput_index++;
	 while (count > 0){
         tn3270_eds_set_field_attribute();
		 count--;
	 }	
	 tn3270_update_screen(scn_addr);
	 tn3270_next_field_addr();
}
private void tn3270_eds_set_field_attribute(){
	/*
	 * set single field attribute from next 2 bytes
	 * see sa and sfe commands
	 */
	 int type = tput_byte[tput_index] & 0xff;
	 tput_index++;
	 switch (type){
	 case 0xc0: // basic type attribute
		 tn3270_start_field();
 	     break;
	 case 0x41: // highlighting
		 tn3270_field_highlight = tput_byte[tput_index] & 0xff;
		 tput_index++;
		 break;
	 case 0x42:  // color
		 tn3270_field_color = tput_byte[tput_index] & 0xff;
       	 tput_index++;
       	 break;
     default:
    	 abort_error(112,"invalid tn3270 sfe type code " + tz390.get_hex(type,2));
         return;
	 }
}
private void keyboard_readline(){
	/*
	 * 1.  Read keyboard text into tget_buff until
	 * return or field full and translate to EBCDIC
	 * 2.  Echo characters to screen at row,col.
	 */
	int key = 0;
	int keychar = 0;
	int index = 0;
	while (index < tget_len){
		key = gui_keyboard_read((tpg_flags & tpg_wait_mask) == tpg_wait);
	    keychar = key & 0xffff;
	    if (keychar == KeyEvent.VK_ENTER){
            tget_len = index;
        	return;
	    } else if (keychar == KeyEvent.VK_BACK_SPACE){
	    	if (index > 0){
	    		index--;
	    		if (gui_cur_col > 1){
	    			gui_cur_col--;
	    			scn_write_char(' ');
	    			gui_cur_col--;
	    		}
	    	}
    	} else if (index < tget_len){
            scn_write_char((char)keychar);
    		if (tz390.opt_ascii){
    			tget_byte[index] = (byte)keychar;
    		} else {
    			tget_byte[index] = tz390.ascii_to_ebcdic[keychar];
    		}
    		index++;
    	} else {
    		return;
	    }
	}
}
private void scn_write_char(char key){
	/*
	 * write 1 character at current screen location
	 */
    scn_addr = (gui_cur_row-1)*max_cols + (gui_cur_col-1);
    scn_char[scn_addr] = key;
	tn3270_update_screen(scn_addr);
    gui_cur_col++;
    if (gui_cur_col > max_cols){
		scn_next_line();
	}
}
private void scn_next_line(){
	/*
	 * position to next screen line with
	 * status line prompt before wrapping screen
	 * to position back to line 1
	 */
	gui_cur_col = 1;
	gui_cur_row++;
	if (gui_cur_row > max_rows){
		status_line.setText("Press enter for next screen");
		update_main_view();
		gui_keyboard_read((tpg_flags & tpg_wait_mask) == tpg_wait);
		tn3270_clear_screen();
		status_line.setText("");
		gui_cur_row = 1;
	}
}
private String get_ascii_string(byte[] text_byte,int lbuff){
	/*
	 * return string of ascii characters from
	 * tget_buff up to lbuff long
	 */
	int index = 0;
	String text = "";
	while (index < lbuff){
		byte data_byte = text_byte[index];
		char data_char;
		if (tz390.opt_ascii){
			data_char = (char) data_byte;
		} else {
			data_char = (char) tz390.ebcdic_to_ascii[data_byte & 0xff]; //RPI42
		}
		text = text + data_char;
		index++;
	}
	return text;
}
     /***********************************************
      * Public GUI application interfaces for
      *   WTO/WTOR -  MCS view
      *   TPUT/TGET - Screen view
      *   GUI       - graphic view and window commands
      ***********************************************/
	 public boolean wtor_request_reply(int ecb_addr){
		 /*
		  * return quam_cmd_line and reset
		  */
		 if (!wtor_running){
		    gz390_cmd_line.requestFocus();
			wtor_ecb_addr = ecb_addr;
			wtor_reply_string = null;
       		wtor_running = true; // turn on monitor update of wtor_reply_string
		    return true;
		 } else {
			 abort_error(108,"wtor already running");
			 return false;
		 }
	 }
	 public String get_wtor_reply_string(int ecb_addr){
		 /*
		  * return wtor reply string if ready else null
		  */
		 if (!wtor_running){
			 if (wtor_ecb_addr != -1
				 && wtor_ecb_addr == ecb_addr
				 && wtor_reply_string != null){
				 wtor_ecb_addr = -1;
				 return wtor_reply_string;
			 } else {
				 log_error(23,"wtor reply error");
			 }
		 }
		 return null;
	 }
	 public void start_gui(String title,tz390 shared_tz390){
		 /*
		  * startup gz390 gui window with title
		  * in default mcs mode for wto/wtor
		  */
	     tz390 = shared_tz390;
	     main_title = "GZ390 " + tz390.version;
		 String[] dummy_args = new String[0];
		 set_main_mode(dummy_args);
		 init_gz390(dummy_args);
		 gui_window_title(title);
		 refresh_request = true;
	 }
	 public void gui_window_title(String title){
		 /*
		  * set gz390 gui window title 
		  * 
		  * Notes:
		  *   1.  Called from ez390 with ez390_pgm
		  *       at initization time if option gui.
		  */
		 main_title = title;
	   }
	   public synchronized void gui_put_log(String msg) {
		   	/*
		   	 * Write message to log file and to console
		   	 * if console mode or console option on.
		   	 * 
		   	 * Append any output from CMD still in buffers
		   	 * to front of msg with \n
		   	 */
		   	    io_count++;
  	        	log_text.append(msg + "\n");
   	        	main_view_changed = true;
		   }
       public void gui_window_loc(int x,int y){
           /*
            * set location of main window x, y
            */
           		if  (x < 0){
           			x = 0;
           		} else {
           			if (x + main_width > main_width_max){
           				if  (x + main_width_min > main_width_max){
           					x = main_width_max - main_width_min;
           					main_width = main_width_min;
           				} else {
           				    main_width = main_width_max - x;
           				}
           			}
           		}
           		if  (y < 0){
           			y = 0;
           		} else {
           			if (y + main_height > main_height_max){
                           if  (y + main_height_min > main_height_max){
                           	y = main_height_max - main_height_min;
                           	main_height = main_height_min;
                           } else {
           				    main_height = main_height_max - y;
                           }
           			}
           		}
           		main_loc_x = x;
           		main_loc_y = y;
           		main_frame.setLocation(main_loc_x,main_loc_y);
          		main_frame.setSize(main_width,main_height);
              	refresh_request = true;
           }
   	    public void gui_window_size(int x,int y){
   	    /*
   	     * resize main window
   	     */	
   		    	main_loc_x = (int) main_frame.getLocation().getX();
   		    	main_loc_y = (int) main_frame.getLocation().getY();
   	    		if  (x < main_width_min){
   	    			x = main_width_min;
   	    		} else {
   	    			if (x > main_width_max - main_loc_x){
   	    				x = main_width_max - main_loc_x;
   	    			}
   	    		}
   	    		if  (y < main_height_min){
   	    			y = main_height_min;
   	    		} else {
   	    			if (y > main_height_max - main_loc_y){
   	    				y = main_height_max - main_loc_y;
   	    			}
   	    		}
   	    		main_width  = x;
   	    		main_height = y;
   	    		main_frame.setSize(main_width,main_height);
   	    		refresh_request = true;
    	    }
        public void gui_window_font(int font){
        	/*
        	 * set font size 
        	 */
        	font_command(font);
        }
        public void gui_window_view(int view,int x,int y,int color){
        	/*
        	 * set window view  
        	 *   1 = MCS console view
        	 *   2 = TN3270 screen view
        	 *   3 = graphics view
        	 */
        	switch (view){
        	case 1: // MCS console view
        		set_view_mcs();
        		break;
        	case 2: // TN3270 screen view
        		max_cols = y;
        		max_rows = x;
        		set_view_screen();
        		break;
        	case 3: // graphics view
        		set_view_graph();
        		break;
        	}
        }
        public int gui_window_getview(){
        	/*
        	 * return current window view
        	 *   1 = MCS console view
        	 *   2 = TN3270 screen view
        	 *   3 = graphics view
        	 */
        	return gui_view;
        }
        public void gui_screen_write(int row, int col, ByteBuffer buff,int lbuff, int color){
        	/*
        	 * write text at row,col from buff for lbuff
        	 * using color
        	 */
        }
        public ByteBuffer gui_screen_read(int lbuff,boolean wait){
        	/*
        	 * return ByteBuffer of lenght lbuff
        	 * from TN3270 screen.  If wait = 1 
        	 * wait for input else return r15=4 
        	 * if none ready.
        	 */
        	byte[] temp_byte = new byte[lbuff];
        	ByteBuffer temp_buff = ByteBuffer.wrap(temp_byte,0,lbuff);
        	while (gui_tot_key == 0 && wait){
                sleep_now();
        	}
        	return temp_buff;
        }
        private void sleep_now(){
        	/*
        	 * sleep for interval
        	 */
    		try {
    			Thread.sleep(monitor_wait);
    		} catch (Exception e){
    			abort_error(109,"GUI Screen read wait exception -" + e.toString());
    		}
        }
        public void gui_screen_field(int row, int col, int lfield){
        	/*
        	 * define field for input from screen
        	 */
        }
        public void gui_screen_cursor(int row, int col, int type){
        	/*
        	 * set cursor type at row, col
        	 */
        }
        public void gui_graph_point(int x, int y, int color){
        	/*
        	 * draw point at x,y 
        	 */
        }
        public void gui_graph_line(int x1, int y1, int x2, int y2, int color){
        	/*
        	 * draw point at x,y 
        	 */
        }
        public void gui_graph_fill(int x1, int y1, int x2, int y2, int color){
        	/*
        	 * fill area at x1,y1 to x2,y2 
        	 */
        }
        public void gui_graph_text(int x1, int y1, String text, int color){
        	/*
        	 * draw characters at x,y 
        	 */
        }
        public int gui_keyboard_read(boolean wait){
        	/*
        	 * read next keyboard keycode and keychar
        	 * and return as int (code high 16, char low 16).
        	 * If none ready and no wait return -1
        	 * else wait for next input key
        	 */
        	while (gui_tot_key == 0 && wait){
        		try {
        			Thread.sleep(monitor_wait);
        		} catch (Exception e){
        			abort_error(110,"GUI Keyboard read wait exception -" + e.toString());
        		    return -1;
        		}
        	}
        	if (gui_tot_key > 0){
        		
        		int key = gui_key_code_char[gui_next_key];
        		gui_next_key++;
        		if (gui_next_key >= gui_tot_key){
        			gui_tot_key = 0;
        			gui_next_key = 0;
        		}
                return key;
        	} else {
        		return -1;
        	}
        }
        public int[] gui_mouse_read(){
        	/*
        	 * return int[4] with x,y,left,right
        	 */
        	int[] mouse = new int[4];
        	return mouse;
        }
        public void gui_sound_play(String wav_file){
        	/*
        	 * play wav_file
        	 */

        }
    public void gui_tget(){
    	/*
    	 * 1.  Return last tn3270 data stream input
    	 *     if available following keyboard enter
    	 *     or PF key.  If option edit, remove
    	 *     control characters and translate to
    	 *     EBCDIC unless ASCII mode.
    	 * 2.  If none, and option wait then wait
    	 *     else return R15=4 indicating not avail.
    	 * 3.  Set tget_len to actual bytes returns if
    	 *     less than requested length.
    	 * 4.  Set R1=length of data buffer returned
    	 *     and R15=0 or 4 if none and NOWAIT
    	 */
    	tpg_rc = 0; // assume rc = 0
    	if (gui_view != gui_view_screen){
    		set_view_screen();
    	}
    	if ((tpg_flags & tpg_type_mask) == tpg_type_asis){
        	if (!tn3270_attn){
        		tn3270_kb_lock = false;
        		status_line.setText("Ready for input");
        	}
    		if ((tpg_flags & tpg_wait_mask) == tpg_wait){
    			while (!tz390.z390_abort && !tn3270_attn){
    				sleep_now();
    			}
    		} else if (!tn3270_attn){
    			tpg_rc = 4; // RPI 221 return 4 if NOWAIT and no data 
    			return;
    		}
    	    if (tn3270_attn){
    	    	status_line.setText("Processing keyboard input");
    	    	tn3270_get_screen_input();
    	    	tn3270_attn = false;
    	    	tn3270_aid = tn3270_noaid;
    	    	tn3270_kb_lock = true;
    	    }
     	} else {
            keyboard_readline();
    	}
    }
    public void gui_tput(){
    	/*
    	 * 1.  Display TN3290 data stream buffer on
    	 *     GUI 3270 screen and return true of ok.
    	 */
    	tpg_rc = 0; // RPI 221 assume ok
    	if (gui_view != gui_view_screen){
    		set_view_screen();
    	}
        if ((tpg_flags & tpg_type_mask) == tpg_type_fullscr
        	|| (tpg_flags & tpg_type_mask) == tpg_type_asis){
        	tn3270_full_screen = true;
         	tn3270_tput_buffer();	
     	} else { 
     		tn3270_full_screen = false;
     		tput_edit_buffer(tput_byte,tput_len);
    	}
        gz390_cmd_line.requestFocus();
	}
}
