import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Font;
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
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FilePermission;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.InputStreamReader;
import java.io.PrintStream;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.PropertyPermission;
import java.util.StringTokenizer;

import javax.swing.BorderFactory;
import javax.swing.ImageIcon;
import javax.swing.JApplet;
import javax.swing.JButton;
import javax.swing.JCheckBoxMenuItem;
import javax.swing.JFileChooser;
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
import javax.swing.filechooser.FileFilter;

public  class  z390 extends JApplet 
    implements MouseListener, KeyListener,
	           ActionListener, 
			   ComponentListener,
			   Runnable,
			   FocusListener {
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
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


	 ****************************************************
	 * Maintenance
	 * ***************************************************
	 * 03/05/05 copied from superzap, update menus, cmds
	 * 09/26/05 replace z390 dialog with batch_cmds and options
	 * 10/02/05 RPI   8 fix "compatible java" in about desc. 
	 * 10/03/05 change minimum J2RE release to 1.5
	 * 10/04/05 RPI   5 - option ASCII use ASCII vs EBCDIC
     * 10/04/05 RPI   6 - option ERR nn limit errors
     * 10/04/05 trap any command errors and issue 51
     * 10/09/05 RPI  16 - remove TEST and TRACE dependency bug
     * 10/12/05 RPI  15 & 17 - fix CMD processing and status line
     * 10/18/05 RPI  29 - use Z390E and Z390I prefixes 
     * 10/27/05 RPI  55 - change /SC to execute Z390 GUI commands
     *          which may include batch commands CMD.
     * 11/03/05 RPI  62 - remove extra space added to test commands
     * 11/12/05 RPI  81 - change menu item font_size
     * 11/18/05 RPI  98 - add ASCII and DUMP to options
     *          and add cr,lf around exit
     * 12/15/05 RPI 135 - use tz390 shared file routines
     * 02/21/06 RPI 208 - use tz390.z390_abort to term.
     * 03/19/06 RPI 236 - if install directory read-only, turn off log,
     *          and send msg to console saying use log command
     * 04/03/06 RPI 235 correct CD path display and suport cd.. etc
     * 05/05/06 RPI 309 retain last directory on file select
     * 11/16/06 RPI 499 use z390_os_type to support Windows and Linux
     * 11/28/06 RPI 500 use system newline for Win/Linux
     * 11/29/06 RPI 508 correct error 51 on /SC startup commands
     *          and invalid path after CD command
     * 12/01/06 RPI 509 use Monospace font for Windows and Linux
     * 12/01/06 RPI 510 replace NOTEPAD command with EDIT command
     * 01/20/07 RPI 541 correct Z390 GUI file selection dialog cancel action
     * 01/30/07 RPI 532 correct Doc path and cmd.pl path and separator
     * 04/26/07 RPI 603 correct up/down scrolling
     * 07/06/07 RPI 646 synchronize abort_error to prevent other task abort errors
     * 08/16/07 RPI 630 prevent PF10 causing file menu popup
     *          use get_window_size() for all GUI's
     * 08/23/07 RPI 685 adjust GUI height for status line 
     * 11/12/07 RPI 737 route all commands to CMD process if running 
     * 12/19/07 RPI 756 don't add ".." twice  
     * 12/19/07 RPI 765 force line break to prvent EXIT being split  
     * 01/30/08 RPI 792 remove put error msg before checking for null 
     * 07/30/08 RPI 888 add leading space which may be eaten by PAUSE before EXIT for batch command
     *          also switch cmd_input_writer to do auto flush to prevent sporatic chopped commands  
     * 09/08/08 RPI 872 help menu "Guide" link to www.z390.org or webdoc\index.html 
     * 09/10/08 RPI 904 correct help menu "Guide" to support LSN path 
     * 05/23/09 RPI 1041 replace EDIT SELECTALL with SELECT LOG and SELECT CMD
     * 06/04/09 RPI 1050 suppress blank lines on GUI log  
     * 09/26/09 RPI 1080 replace init_tables with init_tz390
     * 12/08/10 RPI 1141 correct spelling on menu descriptions
     * 07/26/11 RPI 1173 correct LSN path logic to avoid double "" 
     * 07/26/11 RPI 1174 Add "Apple Inc." as valid java vendor with
     *          default to Linux type filenames (see tz390 os_type 
     * 07/30/11 RPI 1175 use shared tz390.check_java_version()              
	 ********************************************************
     * Global variables                  last RPI
     *****************************************************
     * global command mode variables
     */
	    tz390 tz390 = null;
	    String startup_cmd_file = null;
	    int first_user_parm = 0;
        int hex_base = 16;
	    boolean echo_cmd = true;
	    boolean console_log = false;
        int z390_errors = 0;
	    boolean cmd_error = false;
	    int max_errors = 100;
	    boolean main_applet  = false;  // running as browser applet
	    boolean main_gui     = false;     // parm = /g
        boolean main_console = false; // parm = /c
	    boolean main_batch   = false;   // parm = file name
        String  main_title = "z390";
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
     * current directory file
     */
   	    File dir_cur_file = null;
   	/*
     * Global log output file variables
     */
	    String log_file_name = null;
	    boolean log_tod = true; 
        BufferedWriter log_file = null;
        int max_line_length = 80;
    /*
     * Monitor variables
     */   
        int     ins_count = 0;  
        int     io_count  = 0;
        int     start_cmd_io_count;
        long    start_cmd_time;
        Timer   monitor_timer = null;
        int     monitor_wait = 300;
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
	 *  CMD Command execution variables
	 */ 	
    boolean cmd_mode = false;
    boolean cmd_running = false;
    int cmd_io_total = 0;
	Process cmd_exec_process = null;
	BufferedReader     cmd_exec_error_reader = null;  // RPI 731
	BufferedReader     cmd_exec_output_reader = null; // RPI 731
    PrintStream        cmd_exec_input_writer  = null;  // RPI 731
	String cmd_exec_error_msg = "";
	String cmd_exec_output_msg = "";
	Thread cmd_exec_process_thread = null;
	Thread cmd_exec_error_reader_thread = null;
	Thread cmd_exec_output_reader_thread = null;
	int    cmd_exec_rc = 0;
	String cmd_line = "";       // RPI 508 prevent last becoming null
	String last_cmd_line = "x"; // RPI 506
    boolean shutdown_exit = false;
    /*
     *  Global Z390 GUI objects 
     */
    int ascii_lf = 10;
    int ascii_cr = 13;
 	boolean refresh_request = false;
 	boolean main_status  = true;
        JFrame main_frame    = null;
        int main_width  = 625;
        int main_height = 400;
        int main_border = 2;
        int main_loc_x = 50;
        int main_loc_y = 50;
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
   	int command_columns  = 75; // RPI 685
	 int command_height = font_size + font_space 
           + main_border;
     int status_height  = font_size + font_space
           + main_border;
	int applet_status_height = 0;
	boolean labels_visible = true;
	int labels_min_width = main_width;
	int labels_max_font  = font_size;
	int label_width    = 0;
        JPanel main_panel    = null;
        JTextArea log_text = null;
        JScrollPane log_view    = null;
        JLabel cmd_label = null;
        JTextField  z390_cmd_line = null;
        JLabel status_line_label = null;
        JTextField  status_line = null;
        int cur_cmd = 0;  // index of most recent comment entered
        int end_cmd = 0;  // index of highest cmd entered
        int max_cmd  = 100;
        int view_cmd = 0; // index of current cmd in view
        boolean view_restore = false; // RPI 603
        String[] cmd_history = new String[100];
    /*
     *  Menu items requiring state changes
     */  
        JMenuBar menuBar = null;  //RPI81       
        JMenu file_menu = null;            
        JMenu edit_menu = null;            
        JMenu option_menu = null;          
        JMenu view_menu = null;            
        JMenu help_menu = null;             
        JMenuItem file_menu_cd = null;         
        JMenuItem file_menu_edit = null;       
        JMenuItem file_menu_mac = null;        
        JMenuItem file_menu_asm = null;        
        JMenuItem file_menu_asml = null;       
        JMenuItem file_menu_asmlg = null;      
        JMenuItem file_menu_job = null;        
        JMenuItem file_menu_link = null;       
        JMenuItem file_menu_exec = null;       
        JMenuItem file_menu_exit = null;       
        JMenuItem edit_menu_cut = null;        
        JMenuItem edit_menu_copy = null;       
        JMenuItem edit_menu_paste = null;      
        JMenuItem edit_menu_select_log = null; // RPI 1041
        JMenuItem edit_menu_select_cmd = null; // RPI 1041
        JMenuItem edit_menu_copy_log = null;   
        JMenuItem edit_menu_editor = null; 
        JCheckBoxMenuItem option_menu_ascii = null;
        JCheckBoxMenuItem option_menu_con = null;  
        JCheckBoxMenuItem option_menu_dump = null;
        JCheckBoxMenuItem option_menu_guam  = null;
        JCheckBoxMenuItem option_menu_list = null;              
        JCheckBoxMenuItem option_menu_listcall = null;         
        JCheckBoxMenuItem option_menu_stats = null;            
        JCheckBoxMenuItem option_menu_amode31 = null;          
        JCheckBoxMenuItem option_menu_rmode31 = null;          
        JCheckBoxMenuItem option_menu_test = null;              
        JCheckBoxMenuItem option_menu_trace = null;             
        JCheckBoxMenuItem view_menu_status = null;              
        JCheckBoxMenuItem view_menu_cmd = null;              
        JMenuItem help_menu_help = null;            
        JMenuItem help_menu_commands = null;        
        JMenuItem help_menu_guide = null;           
        JMenuItem help_menu_perm = null;            
        JMenuItem help_menu_releases = null;        
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
      * batch command global variables
      */
           String bat_file_name = null;
           String mac_opt = "";
           String asm_opt = "";
           String asml_opt = "";
           String asmlg_opt = "";
           String job_opt = "";
           String link_opt = "";
           String exec_opt = "";
     /*
      * web site and install location
      */
        String web_site = "http://www.z390.org";
        String install_loc = null;
        String install_webdoc = null; // RPI 872
      /*
       * macro assembler command global variables
       */
        String sysin_file_name = null;
        String syslib_dir_name = null;
        String sysout_dir_name = null;
        boolean print_option = true;
        boolean anim_option  = true;
       /* 
        * end of global z390 class data and start of procs
        */
        public void init() {
        /*
         * JApplet execution launched from web broswer
         */	
            ImageIcon start_icon = createImageIcon("z390.jpg","Run z390");
            JButton start_button = new JButton(start_icon);
            start_button.setSize(48,48);
            start_button.setToolTipText("Click on z390 icon to start Z390 GUI");
            start_button.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                    String[] args = new String[1];           
                    args[0] = "/G";
                    main_applet = true;
                    applet_status_height = status_height;
                    if (set_main_mode(args) == 0){
                 	   init_z390(args);
                    }
                }
            });
            getContentPane().add(start_button);
            setSize(200,100); 
        }
  	public static void main(String[] args) {
  	/*
  	 * Create instance of z390 class
  	 */
    	    z390 pgm = new z390();
            pgm.main_applet = false;
            pgm.set_main_mode(args);
            if  (pgm.main_gui){
                pgm.init_z390(args);
            }
          }
  	  private int set_main_mode(String[] args){
  		/*
  		 * Set main program execution mode
  		 * Set security permissions
  		 * 
  		 * Notes:
  		 *   1.  called from main or init before
  		 *       z390 instance started so only
  		 *       set class variables.
  		 */	
  		    tz390 = new tz390();              // RPI 1175
            if (!tz390.check_java_version()){ // RPI 1175
  					MessageBox box = new MessageBox();
  					box.messageBox("SZ390E error ",
  							"Unsupported Java Version " 
  					+ tz390.java_vendor + " " + tz390.java_version);
  					if (!main_applet){
  						exit_main(16);
  					} 
  					return 16;
  			}
  			main_demo = false;
  			main_lic   = false;
  		/*
  		 * Set startup parm options
  		 */
  		     set_startup_parm_options(args);
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
                if  (main_batch){
                	System.out.println("SZ390E error 15 batch log permission denied - aborting");
                	shut_down(16);
                }
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
  		private void set_startup_parm_options(String args[]){
  	    /* 
  	     * process startup parms
  	     *
  	     *    /G   - graphical interface (default)
  	     *    /NP  - no permissions (supress checking permissions
  	     *    /NT  - no unique log file timestamps 
  	     *    /RT  - regress test mode (supress time stamps 
  	     *    /SC  file -  startup cmd mode file (.bat)                
  	     */
            int index1 = 0;
            main_gui = false;
            main_batch = false;
            main_console = false;
  			if (args.length > 0){
  				while (index1 < args.length){
  		            boolean parm_ok = false;
  		            if (args[index1].toUpperCase().equals("/G")){
  		  		        parm_ok = true;
  		            	main_gui = true;
  		  		        main_console = false;
  		  		        main_batch   = false;
  		            }
  					if  (args[index1].toUpperCase().equals("/NP")){
  						parm_ok = true;
  						check_perms = false;
  					}
  					if  (args[index1].toUpperCase().equals("/NT")){
  						parm_ok = true;
  						log_tod = false;
  					}
  					if  (args[index1].toUpperCase().equals("/RT")){
  						parm_ok = true;
  						tz390.opt_timing = false;
  					}  					
  					if  (args[index1].toUpperCase().equals("/SC")){
  						index1++;
  						if (index1 < args.length){
  	  						parm_ok = true;
  							startup_cmd_file = args[index1];
  							if (startup_cmd_file.charAt(0) != '\"'  // RPI 756
  								&& startup_cmd_file.indexOf(' ') != -1){
  								startup_cmd_file = '\"' + startup_cmd_file + '\"';
  							}
  						}
  					}
  					if (!parm_ok){
  						System.out.println("S075 z390 parm error - " + args[index1]);
  					}
  					index1++;
  				}				
  			}
  			if  (!main_gui && !main_batch && !main_console){
  				main_gui = true;
  			}
  		}  		
  		private void log_command(String cmd_parm1,String cmd_parm2){
  		/*
  		 * if OFF specified, turn log off
  		 * If file specified, open new log
  		 * else error.
  		 */	
  			  if  (cmd_parm1 != null
  			  		&& cmd_parm1.toUpperCase().equals("OFF")){
  			  	    close_log_file();
  			  } else if (cmd_parm1 != null){
  			  	    close_log_file();
  			  	    try {
  			  	    	log_file_name = cmd_parm1;
  			            log_file = new BufferedWriter(new FileWriter(log_file_name));
  			            put_copyright();
  			 	   	    put_log("Log file = " + log_file_name);
  			        } catch (Exception e){
  			      	    abort_error(1,"log file open - " + e.toString());
  			        }
  			  } else {
  			  	    log_error(74,"missing log command file or OFF parm");  			  	
  			  }
  		}
		private void open_log_file() {
			/*
			 * Open log file
			 */ 
	      if (perm_file_log){	    	  
	    	  install_loc = System.getProperty("user.dir");
			  File temp_file = new File(install_loc);
			  if (temp_file.isDirectory()){
				  install_loc = temp_file.getPath();
			  } else {
				  abort_error(52,"invalid install directory - " + install_loc);
			  }
			  temp_file = new File(install_loc + File.separator + "webdoc" + File.separator + "index.html"); // RPI 499 correct case for Linux RPI 532, RPI 872, RPI 904 
			  if (temp_file.exists()){  // RPI 872
		   		   try {
		   			   install_webdoc = temp_file.toURI().toURL().toExternalForm();
		   		   } catch (Exception e){
		   			   install_webdoc = null;    // RPI 904
		   		   }
			  } else {
                  install_webdoc = null; // RPI 872
			  }
			  log_file_name = tz390.get_file_name(tz390.dir_cur,"z390",tz390.log_type);
		      try {
		      	  if  (log_tod){
		      	      boolean new_log = false;
		      	      String temp_log_name = "temp";
		              while (!new_log){
			              Date tod = new Date();
			              SimpleDateFormat tod_format = new SimpleDateFormat("_yyyy_MMdd_HHmmss");
				          String log_file_tod = tod_format.format(tod);
				          int index = log_file_name.indexOf('.');
				          if (index == -1){
				        	  temp_log_name = log_file_name + log_file_tod + tz390.log_type;
				          } else {
				        	  temp_log_name = log_file_name.substring(0,index) + log_file_tod + tz390.log_type; // RPI 508
				          }
				          File temp_log_file = new File(temp_log_name);
		                  if  (temp_log_file.exists()){
			                  sleep_now();
		                  } else {
		              	      new_log = true;
		                  }
		              }
		              log_file_name = temp_log_name;
				  } else {
				     log_file_name = log_file_name.concat(tz390.log_type);
		      	  }
		          log_file = new BufferedWriter(new FileWriter(log_file_name));
		          put_copyright();
		 	   	  put_log("Log file = " + log_file_name);
		      } catch (Exception e){
		    	  log_file = null;  // RPI 236
		      	  System.out.println("z390 log file I/O error - use LOG command to open new log " + e.toString());
		      }
	       } else {
	       	  put_copyright();
	       	  log_error(15,"Permission to write log in current directory denied");
	       }
           put_log("Enter command or help");
		}
		private void close_log_file(){
		/*
		 * Close log file
		 */
			if (log_file != null){  // RPI 236
			   try {
	               log_file.close();
			       log_file = null;
			   } catch (Exception e){
			   	   System.out.println("S003 z390 log file close error - " + e.toString());
			   	   shut_down(16);
			   }
			}
		}
		private void close_all_files(){
		/*
		 * display error total on log and close
		 * data and log files
		 */	
            put_log("Z390I total errors = " + z390_errors);
	        close_log_file();
		}
		private void log_error(int error,String msg){
			z390_errors++;
			cmd_error = true;
			msg = "SZ390E error " + error + " " + msg;
			if  (msg.length() > max_line_length){
				put_log(msg.substring(0,max_line_length));
				int index1 = max_line_length;
				while (index1 < msg.length()){
					int text_length = msg.length() - index1;
					if  (text_length > max_line_length){
						text_length = max_line_length - 5;
					}
					put_log("     " + msg.substring(index1,index1 + text_length));
					index1 = index1 + text_length;
				}
			} else {
				put_log(msg);
			}
			if  (max_errors != 0 && z390_errors > max_errors){
		        abort_error(10,"maximum errors exceeded");
            }
		}
		private synchronized void abort_error(int error,String msg){ // RPI 646
			z390_errors++;
			msg = "SZ390E " + error + " " + msg;
			put_log(msg);
 		    System.out.println(msg);
	        exit_main(16);
		}
		private void shut_down(int return_code){
		/*
		 * cancel threads and exit with rc
		 * (turn off runtime shutdown exit
		 */
			if  (monitor_timer != null){
			    monitor_timer.stop();
			}
			if  (cmd_exec_process != null){
				cmd_exec_cancel();
			}
			if  (main_frame != null){
				main_frame.dispose();
			}
			if (main_applet){
				showStatus("Z390I total errors = " + z390_errors);
			} else {
			    if  (!shutdown_exit){
					shutdown_exit = true; //disable exit
			        System.exit(return_code);
			    }
			}
		}
		
	   private void set_runtime_hooks(){
	   	/*
         *   install hook for shutdown when -Xrs VM set
         */
  		     if (main_console && !shutdown_exit){
	   	        Runtime.getRuntime().addShutdownHook(new Thread() {
                    public void run() {
            	        if  (!shutdown_exit){
            	            shutdown_exit = true;
            	            if (tz390.opt_trap){
            	            	try { // RPI 423 catch window shutdown traps too
            	            		abort_error(78,"aborting due to external shutdown request");
            	            	} catch (Exception e){
            	            		System.out.println("z390 internal system exception abort " + e.toString());
            	            	}
            	            } else {
            	            	abort_error(78,"aborting due to external shutdown request");
            	            }
            	        }
                    }
                });
  		     }
	   }
	   private void put_copyright(){
	   /*
	    * display z390 version and copyright
	    */
	   	put_log("Z390I " + tz390.version);
	   	put_log("Copyright 2011 Automated Software Tools Corporation");
	   	put_log("z390 is licensed under GNU General Public License");
	   	if  (mode_msg1 != null){
	   		put_log(mode_msg1);
	   	}
	   	if  (mode_msg2 != null){
	   		put_log(mode_msg2);
	   	}
	   }
	   private synchronized void put_log(String msg) {
	   	/*
	   	 * Write message to log file and to console
	   	 * if console mode or console option on.
	   	 * 
	   	 */
            if (msg.trim().length() == 0){
            	return;  // RPI 1050
            }
	   	    io_count++;
	        if  (main_gui){      
	        	tz390.log_text_append(log_text,msg); // RPI 731
   	        } else {
	   	        if  (main_console || console_log) {
	   	    	    System.out.println(msg);
	   	        }
   	        }
	   	    if (log_file != null){
	   	       try {
	   	    	   log_file.write(msg + tz390.newline); // RPI 500
	   	       } catch (Exception e) {
	   	    	   abort_error(2,"write to log error - " + e.toString());
	   	       }	   	 	
	   	    }
	   }
	   private void process_command(String cmd_text) {  // RPI 508
	   	/* 
	   	 * 1.  parse parms and execute 
	   	 *     z390 command if found.
	   	 *     a.  * in position 1 is a comment
	   	 *     e.  space or null logged as blank line 
	   	 *      
	   	 * 2.  If not a known z390 command, 
	   	 *     issue CMD Windows command.
	   	 * 
	   	 * Notes:
	   	 * 
	   	 * 1.  z390_cmd_line event handler
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
		 cmd_line = cmd_text; // RPI 508
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
	   	    if (cmd_line.length() > 2 && cmd_line.substring(0,2).toUpperCase().equals("CD")){
	   	    	cmd_line = "CD " + cmd_line.substring(2); // RPI 235
	   	    }
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
            if  (cmd_opcode.equals("ABOUT")) {
            	cmd_opcode_ok = true; 
            	about_command(); 
            	break;
            }
        	if  (cmd_opcode.equals("AMODE31")){
             	cmd_opcode_ok = true;
             	tz390.opt_amode31 = options_command(option_menu_amode31, cmd_parm1,cmd_parm2);
             	break;
            }
        	if  (cmd_opcode.equals("ASCII")){
        		cmd_opcode_ok = true;
             	tz390.opt_ascii = options_command(option_menu_ascii, cmd_parm1,cmd_parm2);
        	}
            if  (cmd_opcode.equals("ASM")) {
              	cmd_opcode_ok = true;
              	batch_cmd("ASM",cmd_parm1,"MLC",cmd_parm2);
              	break;
            }
            if  (cmd_opcode.equals("ASML")) {
              	cmd_opcode_ok = true;
              	batch_cmd("ASML",cmd_parm1,"MLC",cmd_parm2);
              	break;
            }
            if  (cmd_opcode.equals("ASMLG")) {
              	cmd_opcode_ok = true;
              	batch_cmd("ASMLG",cmd_parm1,"MLC",cmd_parm2);
              	break;
            }
            break;
         case 'B':           
            break;
         case 'C': 
            if  (cmd_opcode.equals("CD")) {
             	cmd_opcode_ok = true; 
             	cd_command(cmd_parm1); 
             	break;
            }
            if  (cmd_opcode.equals("COMMANDS")) {
            	cmd_opcode_ok = true; 
            	commands_command(cmd_parm1,cmd_parm2); 
            	break;
            }
        	if  (cmd_opcode.equals("CON")){
             	cmd_opcode_ok = true;
             	tz390.opt_con = options_command(option_menu_con, cmd_parm1,cmd_parm2);
             	break;
            }
            if  (cmd_opcode.equals("COPYLOG")){
            	cmd_opcode_ok = true;
            	if  (main_gui){
            		log_text.requestFocus();
            		log_text.selectAll();
            		log_text.copy();
            	} else {
            		log_error(24,"COPYLOG not available in command mode");
            	}
            	break;
            }
            if  (cmd_opcode.equals("CMD")) {
        	    cmd_opcode_ok = true;
        	    if (cmd_parm1 == null){
        	    	if (cmd_mode){  //RPI15
        	    		cmd_exec_cancel();
        	    	} else {
        	    		if (cmd_exec_rc() != -1){
        	    			cmd_startup(null); // start cmd processor
        	    		}
        	    	    cmd_mode = true;
        	    	}
        	    } else {
        	    	int index1 = cmd_line.toUpperCase().indexOf("CMD ") + 4;//RPI62
                    cmd_command(cmd_line.substring(index1));
        	    }
            	break;
            }
            if  (cmd_opcode.equals("CONSOLE")) {
            	cmd_opcode_ok = true; 
            	if  (cmd_parm1 != null){
            	    if (cmd_parm1.toUpperCase().equals("OFF")){
            	       console_log = false;
            	    } else {
            	       console_log = true;
              	    }
            	} else {
            		log_error(50,"missing immediate data parm");
            	}
            	break;
            }
            break;
         case 'D':
             if  (cmd_opcode.equals("DD")) {
               	cmd_opcode_ok = true;
               	batch_cmd("DD",cmd_parm1,"",cmd_parm2);
               	break;
             }
         	if  (cmd_opcode.equals("DUMP")){
             	cmd_opcode_ok = true;
             	tz390.opt_dump = options_command(option_menu_dump, cmd_parm1,cmd_parm2);
             	break;
            }
        	 break;
         case 'E':
            if  (cmd_opcode.equals("ECHO")) {
            	cmd_opcode_ok = true; 
            	if  (cmd_parm1 != null){
            	    if (cmd_parm1.toUpperCase().equals("OFF")){
            	       echo_cmd = false;
            	    } else {
            	       echo_cmd = true;
              	    }
            	} else {
            		log_error(50,"missing immediate data parm");
            	}
            	break;
            }
             if  (cmd_opcode.equals("EDIT")) {
             	cmd_opcode_ok = true;
             	batch_cmd("EDIT",cmd_parm1,"","");
             	break;
             }
             if  (cmd_opcode.equals("ERR")) {
                	cmd_opcode_ok = true;
                	max_errors = Integer.valueOf(cmd_parm1).intValue();
                	put_log("max errors set to - " + max_errors);
                	break;
              }
             if  (cmd_opcode.equals("EXEC")) {
               	cmd_opcode_ok = true;
               	batch_cmd("EXEC",cmd_parm1,"390",cmd_parm2);
               	break;
             }
             if  (cmd_opcode.equals("EXIT")) {
            	cmd_opcode_ok = true;
            	exit_command();
            	break;
            }
            break;
         case 'F':            
            if  (cmd_opcode.equals("FONT")) {
            	cmd_opcode_ok = true; 
                font_command(cmd_parm1,cmd_parm2);
            	break;
            }
            break;
         case 'G': 
          	if  (cmd_opcode.equals("GUAM")){
             	cmd_opcode_ok = true;
             	tz390.opt_guam = options_command(option_menu_guam, cmd_parm1,cmd_parm2);
             	break;
            } 
            if  (cmd_opcode.equals("GUIDE")) {
            	cmd_opcode_ok = true; 
            	if (!main_batch){
            	   guide_command();
         	    } else {
      	   	       log_error(54,"interactive command not supported in batch");
      	        }
            	break;
            }
            break;
         case 'H': 
            if  (cmd_opcode.equals("HELP")) {
            	cmd_opcode_ok = true; 
            	help_command();
            	break;
            }            
            break;
         case 'I':            
            break;
          case 'J':
              if  (cmd_opcode.equals("JOB")) {
                	cmd_opcode_ok = true;
                	batch_cmd("JOB",cmd_parm1,"BAT",cmd_parm2);
                	break;
              }
              break;
          case 'L':
             if  (cmd_opcode.equals("LINK")) {
                	cmd_opcode_ok = true;
                	batch_cmd("LINK",cmd_parm1,"OBJ",cmd_parm2);
                	break;
             }  
             if  (cmd_opcode.equals("LIST")){
             	cmd_opcode_ok = true;
             	tz390.opt_list = options_command(option_menu_list, cmd_parm1,cmd_parm2);
             	break;
            }
         	if  (cmd_opcode.equals("LISTCALL")){
             	cmd_opcode_ok = true;
             	tz390.opt_listcall = options_command(option_menu_listcall, cmd_parm1,cmd_parm2);
             	break;
            }
            if  (cmd_opcode.equals("LOG")) {
            	cmd_opcode_ok = true;
            	log_command(cmd_parm1,cmd_parm2);
            	break;
            }
            if  (cmd_opcode.equals("LOC")) {
            	cmd_opcode_ok = true; 
            	loc_command(cmd_parm1,cmd_parm2);
            	break;
            }
            break;
         case 'M': 
             if  (cmd_opcode.equals("MAC")) {
              	cmd_opcode_ok = true;
              	batch_cmd("MAC",cmd_parm1,"MLC",cmd_parm2);
              	break;
              }
              break;
         case 'O':                      
            break;
         case 'P':
            if  (cmd_opcode.equals("PERM")) {
            	cmd_opcode_ok = true; 
            	perm_command();
            	break;
            }
            if  (cmd_opcode.equals("PASTE")) {
            	cmd_opcode_ok = true;
            	if  (!main_batch){

                    put_log("PASTE from clipboard starting " + time_stamp());
	   	       	    put_log(getClipboard()); // append to log
	   	            put_log("PASTE from clipboard ending   " + time_stamp());
            	} else {
            		log_error(54,"interactive command not supported in batch");
            	}
            	break;
            }
            break;
         case 'R':
            if  (cmd_opcode.equals("REL")) {
            	cmd_opcode_ok = true; 
            	rel_command();
            	break;
            }
        	if  (cmd_opcode.equals("RMODE31")){
             	cmd_opcode_ok = true;
             	tz390.opt_rmode31 = options_command(option_menu_rmode31, cmd_parm1,cmd_parm2);
             	break;
            }
            break;
         case 'S':
        	if  (cmd_opcode.equals("STATS")){
             	cmd_opcode_ok = true;
             	tz390.opt_stats = options_command(option_menu_stats,cmd_parm1,cmd_parm2);
             	break;
            }
            if  (cmd_opcode.equals("STATUS")){
            	cmd_opcode_ok = true;
            	status_command(cmd_parm1,cmd_parm2);
            	break;
            }
            if  (cmd_opcode.equals("SUPPORT")) {
            	cmd_opcode_ok = true; 
            	if (!main_batch){
            	   support_command();
         	    } else {
      	   	       log_error(54,"interactive command not supported in batch");
      	        }
            	break;
            }
            if  (cmd_opcode.equals("SIZE")) {
           	    cmd_opcode_ok = true;
            	size_command(cmd_parm1,cmd_parm2);
            	break;
            }
            break;
         case 'T':
         	if  (cmd_opcode.equals("TEST")){
             	cmd_opcode_ok = true;
             	tz390.opt_test = options_command(option_menu_test, cmd_parm1,cmd_parm2);
             	break;
            }
            if  (cmd_opcode.equals("TIMEOUT")){
            	cmd_opcode_ok = true;
            	timeout_command(cmd_parm1,cmd_parm2);
            	break;
            }
            if  (cmd_opcode.equals("TITLE")){
            	cmd_opcode_ok = true;
            	title_command(cmd_parm1,cmd_parm2);
            	break;
            }
        	if  (cmd_opcode.equals("TRACE")){
             	cmd_opcode_ok = true;
             	tz390.opt_trace = options_command(option_menu_trace, cmd_parm1,cmd_parm2);
             	break;
            }
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
            if  (cmd_opcode_ok != true) {
                process_command("CMD " + cmd_line);
            } else {
            	add_cmd_hist();  
            }
		 } catch (Exception e){
			 log_error(51,"command error on -" + cmd_line);
		 }
       }
	   private void add_cmd_hist(){
		   /*
		    * add command cmd_line to rolling history
		    */
		   if (last_cmd_line.equals(cmd_line)){
			   return;  // RPI 506
		   }
		   last_cmd_line = cmd_line;
		   cur_cmd++;
		   if (cur_cmd < max_cmd && cur_cmd > end_cmd){
			   end_cmd = cur_cmd;
		   }
           if (cur_cmd >= max_cmd){
              cur_cmd = 0;
           }
           cmd_history[cur_cmd] = cmd_line;
           view_restore = true; // RPI 603 force cmd restore on next up/down
           view_cmd = cur_cmd;
	   }
	   private void get_prev_cmd(){
		   /*
		    * restore prev cmd to z390_cmd_line
		    */
		   if (view_restore){
			   view_restore = false;
		   } else {
			   view_cmd--; // RPI 603
   	   	   	   if  (view_cmd < 0){
   	   	   		   view_cmd = end_cmd;
   	   	   	   }
   	   	   }
   	   	   z390_cmd_line.setText(cmd_history[view_cmd]);
	   }
	   private void get_next_cmd(){
		   /*
		    * display next cmd
		    */
		   view_restore = false;
		   view_cmd++;  // RPI 603
  	   	   if  (view_cmd > end_cmd){
  	   	   	   view_cmd = 0;
  	   	   }
 	   	   z390_cmd_line.setText(cmd_history[view_cmd]);
	   }
	   private String time_stamp(){
		   /*
		    * return date and time if tz390.opt_timing
		    */
   	    String temp_date_text = "";
	    if  (tz390.opt_timing){
	        Date temp_date = new Date(); 
            temp_date_text = mmddyy.format(temp_date)
              + " " + hhmmss.format(temp_date);
        }
	    return temp_date_text;
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
	   	  put_copyright();
	   	  put_log("z390 Portable mainframe macro assembler, linker, and emulator tool");
	   	  put_log("  * edit, assemble, link, and execute mainframe assembler code");
	   	  put_log("  * use interactive z390 GUI, command line, or batch interface");
	   	  put_log("  * macro assembler compatible with HLASM");
	   	  put_log("  * linker supports AMODE/RMODE relocatable load modules");
	   	  put_log("  * emulator supports 32/64 bit plus HFP/BFP floating point instructions");
	   	  put_log("  * emulator supports OS basic svcs such as READ/WRITE at macro level");
	   	  put_log("  * emulator includes powerful trace and test debug tools");
          put_log("  * z390 distributed as InstallShield exe for Windows and as zip");
          put_log("  * z390 includes example demos and regression tests");
          put_log("  * z390 written entirely in J2SE 1.5.0 compatible Java");  //rpi8 
          put_log("  * z390 distributed with source under GNU open source license");
          put_log("  * z390 open source project for support and extensions");         
          put_log("Visit www.z390.org for additional information");
	   }
	   private void font_command(String cmd_parm1,String cmd_parm2){
	   /* 
	    * reset font size for log, and command line
	    * and menu pop-ups (RPI 81)
	    */
	   	    int new_font_size;
	   	    if (cmd_parm1 != null){
	   	    	new_font_size = get_dec_int(cmd_parm1);
	   	    	if (new_font_size < 8 || new_font_size > 72){
	   	    		log_error(63,"font outside fixed width font limits");
	   	    	} else {
	   	    		if (main_gui){
		   	    		font_size = new_font_size;
		   	    		set_gui_size();
                        set_text_font();
                        set_tooltips();
	   	                refresh_request = true;
	   	    		}
	   	    	}	
	   	    } else {
	   	    	log_error(63,"font outside fixed width font limits");
	   	    }
	   }
	   private void set_text_font(){
		   /*
		    * reset font size for menu, log, cmd
		    * and status line
		    */
	          menuBar.setFont(new Font(tz390.z390_font,Font.BOLD,font_size)); //RPI81
   	          file_menu.setFont(new Font(tz390.z390_font,Font.BOLD,font_size));   
   	          edit_menu.setFont(new Font(tz390.z390_font,Font.BOLD,font_size));   
   	          option_menu.setFont(new Font(tz390.z390_font,Font.BOLD,font_size)); 
   	          view_menu.setFont(new Font(tz390.z390_font,Font.BOLD,font_size));   
   	          help_menu.setFont(new Font(tz390.z390_font,Font.BOLD,font_size)); 
   	          file_menu_cd.setFont(new Font(tz390.z390_font,Font.BOLD,font_size));     
   	          file_menu_edit.setFont(new Font(tz390.z390_font,Font.BOLD,font_size));   
   	          file_menu_mac.setFont(new Font(tz390.z390_font,Font.BOLD,font_size));    
   	          file_menu_asm.setFont(new Font(tz390.z390_font,Font.BOLD,font_size));    
   	          file_menu_asml.setFont(new Font(tz390.z390_font,Font.BOLD,font_size));   
   	          file_menu_asmlg.setFont(new Font(tz390.z390_font,Font.BOLD,font_size));  
   	          file_menu_job.setFont(new Font(tz390.z390_font,Font.BOLD,font_size));    
   	          file_menu_link.setFont(new Font(tz390.z390_font,Font.BOLD,font_size));   
   	          file_menu_exec.setFont(new Font(tz390.z390_font,Font.BOLD,font_size));   
   	          file_menu_exit.setFont(new Font(tz390.z390_font,Font.BOLD,font_size));   
   	          edit_menu_cut.setFont(new Font(tz390.z390_font,Font.BOLD,font_size));    
   	          edit_menu_copy.setFont(new Font(tz390.z390_font,Font.BOLD,font_size));   
   	          edit_menu_paste.setFont(new Font(tz390.z390_font,Font.BOLD,font_size));  
   	          edit_menu_select_log.setFont(new Font(tz390.z390_font,Font.BOLD,font_size));
   	          edit_menu_select_cmd.setFont(new Font(tz390.z390_font,Font.BOLD,font_size)); 
   	          edit_menu_copy_log.setFont(new Font(tz390.z390_font,Font.BOLD,font_size));   
   	          edit_menu_editor.setFont(new Font(tz390.z390_font,Font.BOLD,font_size));    
   	          option_menu_ascii.setFont(new Font(tz390.z390_font,Font.BOLD,font_size)); 
   	          option_menu_con.setFont(new Font(tz390.z390_font,Font.BOLD,font_size)); 
   	          option_menu_dump.setFont(new Font(tz390.z390_font,Font.BOLD,font_size));   
   	          option_menu_guam.setFont(new Font(tz390.z390_font,Font.BOLD,font_size));
   	          option_menu_list.setFont(new Font(tz390.z390_font,Font.BOLD,font_size));      
   	          option_menu_listcall.setFont(new Font(tz390.z390_font,Font.BOLD,font_size)); 
   	          option_menu_stats.setFont(new Font(tz390.z390_font,Font.BOLD,font_size));    
   	          option_menu_amode31.setFont(new Font(tz390.z390_font,Font.BOLD,font_size));  
   	          option_menu_rmode31.setFont(new Font(tz390.z390_font,Font.BOLD,font_size));  
   	          option_menu_test.setFont(new Font(tz390.z390_font,Font.BOLD,font_size));      
   	          option_menu_trace.setFont(new Font(tz390.z390_font,Font.BOLD,font_size));     
   	          view_menu_status.setFont(new Font(tz390.z390_font,Font.BOLD,font_size));      
   	          view_menu_cmd.setFont(new Font(tz390.z390_font,Font.BOLD,font_size));         
   	          help_menu_help.setFont(new Font(tz390.z390_font,Font.BOLD,font_size));       
   	          help_menu_commands.setFont(new Font(tz390.z390_font,Font.BOLD,font_size));   
   	          help_menu_guide.setFont(new Font(tz390.z390_font,Font.BOLD,font_size));      
   	          help_menu_perm.setFont(new Font(tz390.z390_font,Font.BOLD,font_size));       
   	          help_menu_releases.setFont(new Font(tz390.z390_font,Font.BOLD,font_size));   
   	          help_menu_support.setFont(new Font(tz390.z390_font,Font.BOLD,font_size));    
   	          help_menu_about.setFont(new Font(tz390.z390_font,Font.BOLD,font_size));      
	          log_text.setFont(new Font(tz390.z390_font,Font.BOLD,font_size));
   	          cmd_label.setFont(new Font(tz390.z390_font,Font.BOLD,font_size));
   	          z390_cmd_line.setFont(new Font(tz390.z390_font,Font.BOLD,font_size));
   	          status_line_label.setFont(new Font(tz390.z390_font,Font.BOLD,font_size));
   	          status_line.setFont(new Font(tz390.z390_font,Font.BOLD,font_size));
	   }
	   private int get_dec_int(String cmd_parm){
		   /*
		    * return int from immeidate decimal parm
		    */
		   	     int save_hex_base = hex_base;
		   	     hex_base = 10;
		   	     int work_int = get_int(cmd_parm);
		   	     hex_base = save_hex_base;
		   	     return work_int;
		   }
		   private int get_int(String cmd_parm){
		   	/*
		   	 * return int from immediate hex parm:or reg
		   	 */
		   	       int work_int = 0;
		   	       int index1 = 1;
		   	           try {
		   	       	       if  (cmd_parm.charAt(0) == '\''
		   	       	       	    && cmd_parm.length() <= 6){
		   	       	           while (index1 < cmd_parm.length() - 1){
		   	       	       	       work_int = work_int * 256 + get_text_char((int)cmd_parm.charAt(index1));
		   	       	       	       index1++;
		   	       	           }
		   	       	       } else {
		   	       	       	   if (cmd_parm.length() < 8){
	                              work_int = Integer.valueOf(cmd_parm,hex_base).intValue();
		   	       	       	   } else {
		   	       	       	   	  if (cmd_parm.length() == 8){
		   	       	       	         work_int = Integer.valueOf(cmd_parm.substring(0,4),hex_base).intValue() * 256 * 256
									          + Integer.valueOf(cmd_parm.substring(4),hex_base).intValue();
		   	       	       	   	  } else {
		   		   	       	         log_error(36,"invalid hex value - " + cmd_parm);
		   	       	       	   	  }
		   	       	       	   }
		   	       	       }
	                       return work_int;
		   	           } catch (Exception e){
		   	       	       log_error(36,"invalid hex value - " + cmd_parm);
		   	       	       return 0;
		   	           }
		   }
		   private int  get_text_char(int work_int){
			   /*
			    * return int value of char or 0 if not printable
			    */	
			   	if (work_int < 0){
			   		work_int = work_int +256;
			   	}
		        return work_int;
			   }
	   private void help_command(){
	   	/*
	   	 * log summary list of commands and help reference
	   	 */
	   	put_log("\nz390 help command summary");
        put_log("File menu selections");
	   	put_log("  EDIT  - open source file to edit");
	   	put_log("  MAC   - expand MLC macro source to BAL assembler source");
	   	put_log("  ASM   - assemble MLC to relocatable OBJ file");
	   	put_log("  ASML  - assemble and link MLC to 390 load module file");
	   	put_log("  ASMLG - assemble, link, and execule 390 load module file");
	   	put_log("  JOB   - execute selected batch job");
	   	put_log("  LINK  - link OBJ files into 390 load module file");
	   	put_log("  EXEC  - execute 390 load module file");
	   	put_log("Option menu - toggle default options for above cmds");	  
	   	put_log("View menu - toggle status line and cmd input mode");
        put_log("Type COMMANDS for alphabetical list of all commands");
        put_log("Type GUIDE to view online or local help (if installed)");
        put_log("Type SUPPORT to visit support web site");
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
		       	  log_error(66,"execution startup error " + e.toString());
		      }
	   }
	   private int cmd_startup(String cmd_line){
	   /*
	    * start Windows command processer with 
	    * synchronized buffered output to log.  
	    * 
	    * If cmd_line is null, set cmd_mode and 
	    * start command processor without command.
	    * Future commands in cmd_mode will be passed
	    * to processor via cmd_exec_input_writer.
	    */
	   	    int rc;
	   	    String[] cmd_parms;
	   	    try {
	   	    	if (tz390.z390_os_type == tz390.z390_os_linux) {
	   	    	    if  (cmd_line != null){
	   	    	        cmd_parms = new String[3];
	   	    			cmd_parms[0] = tz390.z390_command;
	   	    			cmd_parms[1] = install_loc + "/cmd.pl"; // RPI 532 
	   	    			cmd_parms[2] = cmd_line;
	   	    		} else {
	   	    			cmd_parms = new String[2];
	   	    			cmd_parms[0] = tz390.z390_command;
	   	    		    cmd_parms[1] = install_loc + "/cmd.pl";// RPI 532 
	   	    		}
	   	    	} else {
	   	    		if  (cmd_line != null){
	   	    			cmd_parms = new String[3];
	   	    			cmd_parms[0] = tz390.z390_command;
	   	    			cmd_parms[1] = "/C";
	   	    			cmd_parms[2] = cmd_line;
	   	    		} else {
	   	    			cmd_parms = new String[1];
	   	    			cmd_parms[0] = tz390.z390_command;
	   	    		}
	   	    	}
            	rc = cmd_exec_start(cmd_parms);
                if  (rc == 0){
   		            monitor_cmd_time_total = 0;
   		            start_cmd_io_count = io_count;
   		            start_cmd_time = cur_date.getTime();
   		            if (!main_gui){
   		            	while (cmd_exec_rc() == -1){
                            sleep_now();
				       }
   		            }
   		            if (tz390.opt_timing){
   		            	cur_date = new Date();
   		            }
                    sync_cmd_dir();
   		            put_log("*** " +  mmddyy.format(cur_date) 
   		            		 + " " + hhmmss.format(cur_date)
   		            		 + " CMD task started");
	   	        } else {
	   	        	log_error(66,"CMD task startup error rc = " + rc);
    	 	        cmd_exec_cancel();
	   	        }
                return rc;
	   	    } catch (Exception e) {
  			    log_error(66,"execution startup error - " + e.toString());
                cmd_exec_cancel();
	   		    return 16;
    	 	}
	   }
	   private void sleep_now(){
		   /*
		    * sleep for monitor interval if not abort
		    */
		   if (tz390.z390_abort){
			   exit_command();
		   }	    
           try {  // wait until done if not gui
               Thread.sleep(monitor_wait);
           } catch (Exception e){
                 abort_error(77,"Wait interrupted " + e.toString() );
           }
	   }
	   private void sync_cmd_dir(){
		   /*
		    * sync the cmd task directory with
		    * current directory.
		    *
		    */
	           if (!tz390.dir_cur.equals(install_loc)){
	        	   if (tz390.z390_os_type == tz390.z390_os_linux){
	        		   cmd_exec_input("CD " + tz390.dir_cur); // RPI 499 change Linux directory and/or drive
	        	   } else {
	        		   if (!tz390.dir_cur.substring(0,2).equals(install_loc.substring(0,2))){
		           			cmd_exec_input(tz390.dir_cur.substring(0,2)); // change windows drive
	        		   }
	        		   cmd_exec_input("CD " + tz390.dir_cur.substring(2)); // change windows directory
	        	   }
		       }
	   }
	   private void monitor_update(){
	   /*
	    * 1.  At monitor_wait intervals, update the
	    *     z390 GUI title date and time and the status
	    *     line information.
	    *  
	    * 2.  If CMD mode and 
	    *     monitor_wait_total > timeout_interval
	    *     then abort CMD.
	    * 
	    * 3.  If monitor_wait_total > status_interval
	    *     then update and log status line in batch
	    *     or command mode.
	    * 
	    * 4.  If current time beyond main_demo timeout
	    *     terminate.
	    * 
	    * 5.  reset focus to z390_cmd_line after update
	    */
		    if (tz390.z390_abort){
		    	exit_command();
		    }
	        monitor_next_time = System.currentTimeMillis();
	        monitor_next_ins_count = ins_count;
	        monitor_next_io_count = io_count;
            monitor_cur_interval = monitor_next_time - monitor_last_time;
	   	    monitor_cmd_time_total = (monitor_next_time - start_cmd_time)/1000;
	   	    if (tz390.opt_timing){
	   	    	cur_date = new Date();
	   	    }
	   	    if  ((cmd_mode || cmd_running) && cmd_exec_rc() != -1){
	   	    	put_log("*** " + mmddyy.format(cur_date)
	   	    			 + " " + hhmmss.format(cur_date)
	   	    			 + " CMD task ended TOT SEC=" + monitor_cmd_time_total
	   	    			 + " TOT LOG IO=" + (io_count-start_cmd_io_count));
	   	    	cmd_mode = false;
	   	    	cmd_running = false;
	  		    if (main_gui){
   	  		       	view_menu_cmd.setSelected(false);
   	  		    }
	   	    }
	   	    if (main_gui){         	
	   	       title_update();
	   	       if  (ins_count > monitor_last_ins_count
	   	       		|| io_count > monitor_last_io_count
					|| monitor_last_cmd_mode != cmd_mode){
	   	       	   monitor_cur_ins = monitor_next_ins_count - monitor_last_ins_count;
	   	       	   monitor_cur_int = monitor_next_time - monitor_last_time;
	   		   	   monitor_cur_rate = monitor_cur_ins *1000 / monitor_cur_int;
	   	       	   status_line.setText(get_status_line());
	   	       }
	   	       check_main_view();
	   	    } 
	   	    if  (status_interval > 0){ 
		         status_next_time = monitor_next_time;
	             status_cur_int = status_next_time - status_last_time;
	   	         if (status_cur_int >= status_interval
	   	         	 && (!cmd_mode && cmd_exec_rc() == -1)){
	   	             status_log_update();
	   	       	 }
	   	    }
	   	    if (monitor_timeout_limit > 0){
   	            if (!cmd_mode && cmd_exec_rc() == -1){
	   	    	   if  (monitor_cmd_time_total > monitor_timeout_limit){
                       cmd_timeout_error(); 
	   	           }
	            }
	   	     }
	   	    monitor_last_cmd_mode = cmd_mode;
	   	    monitor_last_time = monitor_next_time;
	   	    monitor_last_ins_count  = monitor_next_ins_count;
	   	    monitor_last_io_count   = monitor_next_io_count;
	   }
	   private void cmd_timeout_error(){
	   	   cmd_exec_cancel();
 	       status_log_update();
  	       log_error(69,"CMD command timeout error - command aborted");
	   	   reset_z390_cmd();
	   }
	   private void cmd_command(String cmd){
	   /*
	    * exec Windows command as follows:.
        * 1.  If cmd_mode set via prior cmd with no
        *     command, then all commands are routed
        *     to command processor via cmd_exec_input.  Use
        *     BREAK key or EXIT command passed to
        *     command processor to end cmd_mode.
        * 2.  If prior Windows command is still running,
        *     display current status and request 
        *     user hit break or retry later.
        * 3.  If cmd_mode not set then start command
        *     processor via call to cmd_exec_start.
        * 4.  See STATUS command to set interval for
        *     display of status of long running
        *     commands.        * 
	    */	
		     cmd_line = cmd;
	   	     if  (cmd_exec_rc() == -1){
	   	    	 cmd_exec_input(cmd); // route cmd to existing CMD task running
	   	     } else {
	   	    	 if (cmd == null){         //RPI15
	                 cmd_startup(null);
	   	    	 } else {
	   	    		 cmd_startup(null);
	   	    		 cmd_exec_input(cmd);
	   	    		 cmd_running = true;
	   	    	     cmd_exec_input(" exit");  //RPI15, RPI 98, RPI 500, RPI 731 RPI 888 leading space may be eaten by pause
	   	    	 }
  	 	     }
	   }
	   /*
	    **************************************************
	    * Command support functions
	    **************************************************  
	    */

     private String get_status_line(){
     /*
      * format fixed field status line for both
      * z390 gui status line and status log requests
      * 
      *     1.  Time of date
   	  *     2.  INS total
   	  *     3.  I/O total
   	  *     4.  CMD mode
      */
     	String cmd_mode_text = "";
     	if (cmd_exec_rc() == -1){
     	   cmd_mode_text = " CMD";
     	}
     	String status_text = time_stamp()
		  + " LOG IO=" + get_pad(io_count,6)
		  + cmd_mode_text;
     	return status_text;
     }
     private String get_pad(long num,int pad){
     /*
      * format and pad status line number to
      * specified length.  If number 0, return
      * all spaces.  If number > 1000, return K.
      */
     	String padding = "           ";
     	String text = "";
     	if  (num > 0){
     		if  (num > 10000){
     			text = "" + num/1000 + "K";
     		} else {
     		    text = "" + num;
     		}
         	return text+padding.substring(0,pad-text.length());
     	} else {
     	    return padding.substring(0,pad);
     	}
     }
     private void status_log_update(){	
     /*
      * update status interval and write
      * status line to log
      */
     	status_next_time = System.currentTimeMillis();
        status_next_ins_count = ins_count;
        status_next_io_count = io_count;
        status_cur_int = status_next_time - status_last_time;
       	status_cur_ins = status_next_ins_count - status_last_ins_count;
	    if  (status_cur_int > 0){
	    	status_cur_rate = status_cur_ins * 1000/status_cur_int;
	    } else {
	    	status_cur_rate = 0;
	    }
        put_log(get_status_line());
     	status_last_time = status_next_time;
     	status_last_ins_count = status_next_ins_count;
     	status_last_io_count = status_next_io_count;
     }
   private void init_z390(String[] args){
	   /*
	    * load shared tables and file routines
	    */	 	  
	   tz390.init_tz390();    // RPI 1080
	   dir_cur_file = new File(tz390.dir_cur); // RPI 309
       main_title = "Z390 " + tz390.version;
       /*
        * set runtime cancel hooks
        */
   	        set_runtime_hooks();
       /*
        * Invoke graphical user interface
        */ 
            main_frame = new JFrame();
            title_update();
            tz390.get_window_size(); // RPI 630
            main_frame.setSize(main_width,main_height);
            main_frame.setLocation(main_loc_x,main_loc_y);
            main_frame.addComponentListener(this);
            build_main_panel();
            open_log_file();
            monitor_startup();
			if (startup_cmd_file != null){
				try {
					BufferedReader temp_file = new BufferedReader(new FileReader(startup_cmd_file));
					String temp_line = temp_file.readLine();
					while (!tz390.z390_abort && temp_line != null){
					   process_command(temp_line);
					   temp_line = temp_file.readLine();						   
					}
				} catch (Exception e){
					log_error(72,"startup file I/O error - " + e.toString());
				}
			}
	        cmd_history[0] = ""; // RPI 603
            main_frame.setVisible(true);
            z390_cmd_line.requestFocus();
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
            build_z390_cmd_line();
            build_status_line();
            set_text_font();
            menuBar.add(file_menu);
            menuBar.add(edit_menu);
            menuBar.add(option_menu);
            menuBar.add(view_menu);
            menuBar.add(help_menu);
            main_frame.setJMenuBar(menuBar);
    	    main_panel.add(log_view);
    	    main_panel.add(cmd_label);
    	    main_panel.add(z390_cmd_line);
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
        	close_all_files();
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
        log_view = new JScrollPane(log_text);
        log_view.getVerticalScrollBar().addAdjustmentListener(new AdjustmentListener(){
    	  public void adjustmentValueChanged(AdjustmentEvent e){
    		if (tz390.log_text_added){
                tz390.log_text_added = false;
    			log_view.getVerticalScrollBar().setValue(log_view.getVerticalScrollBar().getMaximum());
    		}       
    	  }});
        log_view.setVerticalScrollBarPolicy(
        JScrollPane.VERTICAL_SCROLLBAR_ALWAYS);
	    log_view.setPreferredSize(   	        		
         	new Dimension(log_width, log_height));
     }
     private void build_z390_cmd_line(){
	/*
     *   Build the command entry field
     */
     	cmd_label = new JLabel("Command: ");
        z390_cmd_line = new JTextField(command_columns);
        z390_cmd_line.addActionListener(this);
	    z390_cmd_line.addMouseListener(this);
        z390_cmd_line.addKeyListener(this);
        z390_cmd_line.addFocusListener(this);
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
     option_menu = new JMenu("Options");
     view_menu = new JMenu("View");
     help_menu = new JMenu("Help");
     file_menu_cd     = new JMenuItem("CD..");
     file_menu_edit   = new JMenuItem("EDIT..");
     file_menu_mac    = new JMenuItem("MAC..");
     file_menu_asm    = new JMenuItem("ASM..");
     file_menu_asml   = new JMenuItem("ASML..");
     file_menu_asmlg  = new JMenuItem("ASMLG..");
     file_menu_job    = new JMenuItem("JOB..");
     file_menu_link   = new JMenuItem("LINK..");
     file_menu_exec   = new JMenuItem("EXEC..");
     file_menu_exit   = new JMenuItem("Exit");
     edit_menu_cut    = new JMenuItem("Cut");
     edit_menu_copy   = new JMenuItem("Copy");
     edit_menu_paste  = new JMenuItem("Paste");
     edit_menu_select_log = new JMenuItem("Select Log"); // RPI 1041
     edit_menu_select_cmd = new JMenuItem("Select Cmd"); // RPI 1041
     edit_menu_copy_log   = new JMenuItem("Copy Log");
     edit_menu_editor = new JMenuItem("Editor");
     option_menu_ascii = new JCheckBoxMenuItem("ASCII");
     option_menu_con = new JCheckBoxMenuItem("CON");
     option_menu_con.setSelected(true);
     option_menu_dump = new JCheckBoxMenuItem("DUMP");
     option_menu_guam  = new JCheckBoxMenuItem("GUAM");
     option_menu_list = new JCheckBoxMenuItem("LIST");
     option_menu_list.setSelected(true);
     option_menu_listcall = new JCheckBoxMenuItem("LISTCALL");
     option_menu_listcall.setSelected(true);
     option_menu_stats = new JCheckBoxMenuItem("STATS");
     option_menu_stats.setSelected(true);
     option_menu_amode31 = new JCheckBoxMenuItem("AMODE31");
     option_menu_amode31.setSelected(true);
     option_menu_rmode31 = new JCheckBoxMenuItem("RMODE31");
     option_menu_test = new JCheckBoxMenuItem("TEST");
     option_menu_trace = new JCheckBoxMenuItem("TRACE");
     view_menu_status = new JCheckBoxMenuItem("Status");
     view_menu_status.setSelected(true);
     view_menu_cmd    = new JCheckBoxMenuItem("CMD Mode");
     help_menu_help       = new JMenuItem("Help");
     help_menu_commands   = new JMenuItem("Commands");
     help_menu_guide      = new JMenuItem("Guide");
     help_menu_perm       = new JMenuItem("Permissions");
     help_menu_releases   = new JMenuItem("Releases");
     help_menu_support    = new JMenuItem("Support");
     help_menu_about      = new JMenuItem("About");
     /*
      * Mnemonic menu bar keys
      */
     file_menu.setMnemonic(KeyEvent.VK_F);
     edit_menu.setMnemonic(KeyEvent.VK_E);
     view_menu.setMnemonic(KeyEvent.VK_V);
     help_menu.setMnemonic(KeyEvent.VK_H);
     file_menu_cd.setMnemonic(KeyEvent.VK_C);
     file_menu_edit.setMnemonic(KeyEvent.VK_D);
     file_menu_mac.setMnemonic(KeyEvent.VK_M);
     file_menu_asm.setMnemonic(KeyEvent.VK_A);
     file_menu_asml.setMnemonic(KeyEvent.VK_S);
     file_menu_asmlg.setMnemonic(KeyEvent.VK_G);
     file_menu_job.setMnemonic(KeyEvent.VK_J);
     file_menu_link.setMnemonic(KeyEvent.VK_L);
     file_menu_exec.setMnemonic(KeyEvent.VK_E);
     file_menu_exit.setMnemonic(KeyEvent.VK_X);
     edit_menu_cut.setMnemonic(KeyEvent.VK_T);
     edit_menu_copy.setMnemonic(KeyEvent.VK_C);
     edit_menu_paste.setMnemonic(KeyEvent.VK_P);
     edit_menu_select_log.setMnemonic(KeyEvent.VK_S);
     edit_menu_select_cmd.setMnemonic(KeyEvent.VK_M);
     edit_menu_copy_log.setMnemonic(KeyEvent.VK_L);
     edit_menu_editor.setMnemonic(KeyEvent.VK_N);
     option_menu_ascii.setMnemonic(KeyEvent.VK_I);
     option_menu_con.setMnemonic(KeyEvent.VK_C);
     option_menu_dump.setMnemonic(KeyEvent.VK_D);
     option_menu_guam.setMnemonic(KeyEvent.VK_G);
     option_menu_list.setMnemonic(KeyEvent.VK_L);
     option_menu_listcall.setMnemonic(KeyEvent.VK_I);
     option_menu_stats.setMnemonic(KeyEvent.VK_S);
     option_menu_amode31.setMnemonic(KeyEvent.VK_A);
     option_menu_rmode31.setMnemonic(KeyEvent.VK_R);
     option_menu_trace.setMnemonic(KeyEvent.VK_E);
     option_menu_test.setMnemonic(KeyEvent.VK_T);
     view_menu_status.setMnemonic(KeyEvent.VK_S);
     view_menu_cmd.setMnemonic(KeyEvent.VK_C);
     help_menu_help.setMnemonic(KeyEvent.VK_H);
     help_menu_commands.setMnemonic(KeyEvent.VK_C);
     help_menu_guide.setMnemonic(KeyEvent.VK_G);
     help_menu_perm.setMnemonic(KeyEvent.VK_P);
     help_menu_releases.setMnemonic(KeyEvent.VK_R);
     help_menu_support.setMnemonic(KeyEvent.VK_S);
     help_menu_about.setMnemonic(KeyEvent.VK_A);
     /*
      * Set menu bar accelerator keys
      */
     file_menu_cd.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_D,ActionEvent.CTRL_MASK));
     file_menu_edit.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_I,ActionEvent.CTRL_MASK));
     file_menu_mac.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_M,ActionEvent.CTRL_MASK));
     file_menu_asm.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_A,ActionEvent.CTRL_MASK));
     file_menu_asml.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_S,ActionEvent.CTRL_MASK));
     file_menu_asmlg.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_G,ActionEvent.CTRL_MASK));
     file_menu_job.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_J,ActionEvent.CTRL_MASK));
     file_menu_link.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_L,ActionEvent.CTRL_MASK));
     file_menu_exec.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_X,ActionEvent.CTRL_MASK));
     file_menu_exit.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_Z,ActionEvent.CTRL_MASK));
     edit_menu_cut.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_E,ActionEvent.CTRL_MASK));
     edit_menu_copy.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_C,ActionEvent.CTRL_MASK));
     edit_menu_paste.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_V,ActionEvent.CTRL_MASK));
     edit_menu_editor.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_N,ActionEvent.CTRL_MASK));
     view_menu_status.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_S,ActionEvent.CTRL_MASK+ActionEvent.SHIFT_MASK));
     view_menu_cmd.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_C,ActionEvent.CTRL_MASK+ActionEvent.SHIFT_MASK));
     help_menu_help.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_H,ActionEvent.CTRL_MASK+ActionEvent.ALT_MASK));
     help_menu_commands.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_C,ActionEvent.CTRL_MASK+ActionEvent.ALT_MASK));
     help_menu_guide.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_G,ActionEvent.CTRL_MASK+ActionEvent.ALT_MASK));
     help_menu_perm.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_P,ActionEvent.CTRL_MASK+ActionEvent.ALT_MASK));
     help_menu_releases.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_R,ActionEvent.CTRL_MASK+ActionEvent.ALT_MASK));
     help_menu_support.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_O,ActionEvent.CTRL_MASK+ActionEvent.ALT_MASK));    
     help_menu_about.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_A,ActionEvent.CTRL_MASK+ActionEvent.ALT_MASK));
     /*
      * add menu action listeners
      */
     file_menu_cd.addActionListener(this);
     file_menu_edit.addActionListener(this);
     file_menu_mac.addActionListener(this);
     file_menu_asm.addActionListener(this);
     file_menu_asml.addActionListener(this);
     file_menu_asmlg.addActionListener(this);
     file_menu_job.addActionListener(this);
     file_menu_link.addActionListener(this);
     file_menu_exec.addActionListener(this);
     file_menu_exit.addActionListener(this);
     edit_menu_cut.addActionListener(this);
     edit_menu_copy.addActionListener(this);
     edit_menu_paste.addActionListener(this);
     edit_menu_select_log.addActionListener(this);
     edit_menu_select_cmd.addActionListener(this);
     edit_menu_copy_log.addActionListener(this);
     edit_menu_editor.addActionListener(this);
     option_menu_ascii.addActionListener(this);
     option_menu_con.addActionListener(this);
     option_menu_dump.addActionListener(this);
     option_menu_guam.addActionListener(this);
     option_menu_list.addActionListener(this);
     option_menu_listcall.addActionListener(this);
     option_menu_stats.addActionListener(this);
     option_menu_amode31.addActionListener(this);
     option_menu_rmode31.addActionListener(this);
     option_menu_trace.addActionListener(this);
     option_menu_test.addActionListener(this);
     view_menu_status.addActionListener(this);
     view_menu_cmd.addActionListener(this);
     help_menu_help.addActionListener(this);
     help_menu_commands.addActionListener(this);
     help_menu_guide.addActionListener(this);
     help_menu_perm.addActionListener(this);
     help_menu_releases.addActionListener(this);
     help_menu_support.addActionListener(this);
     help_menu_about.addActionListener(this);
     file_menu.add(file_menu_cd);
     file_menu.add(file_menu_edit);
     file_menu.add(file_menu_mac);
     file_menu.add(file_menu_asm); 
     file_menu.add(file_menu_asml);
     file_menu.add(file_menu_asmlg);
     file_menu.add(file_menu_job);
     file_menu.add(file_menu_link); 
     file_menu.add(file_menu_exec);
     file_menu.add(file_menu_exit); 
     edit_menu.add(edit_menu_cut);
     edit_menu.add(edit_menu_copy);
     edit_menu.add(edit_menu_paste);
     edit_menu.add(edit_menu_select_log);
     edit_menu.add(edit_menu_select_cmd);
     edit_menu.add(edit_menu_copy_log);
     edit_menu.add(edit_menu_editor);
     option_menu.add(option_menu_ascii);
     option_menu.add(option_menu_con);
     option_menu.add(option_menu_dump);
     option_menu.add(option_menu_guam);
     option_menu.add(option_menu_list);
     option_menu.add(option_menu_listcall);
     option_menu.add(option_menu_stats);
     option_menu.add(option_menu_amode31);
     option_menu.add(option_menu_rmode31);
     option_menu.add(option_menu_trace);
     option_menu.add(option_menu_test);
     view_menu.add(view_menu_status);
     view_menu.add(view_menu_cmd);
     help_menu.add(help_menu_help);
     help_menu.add(help_menu_commands);
     help_menu.add(help_menu_guide);
     help_menu.add(help_menu_perm);
     help_menu.add(help_menu_releases);
     help_menu.add(help_menu_support);
     help_menu.add(help_menu_about);
   }
   private void set_tooltips(){
	   /*
	    * set tooltips after font changes
	    */
	     String text_font_pfx = "<html><font size=" + font_size/3 + ">";
	     String text_font_sfx = "</html>";
	     file_menu_cd.setToolTipText(text_font_pfx + "CD change directory" + text_font_sfx);
	     file_menu_edit.setToolTipText(text_font_pfx + "Edit source file" + text_font_sfx);
	     file_menu_mac.setToolTipText(text_font_pfx + "MAC macro expand (MLC > BAL)" + text_font_sfx);
	     file_menu_asm.setToolTipText(text_font_pfx + "ASM macro assemble (MLC > BAL > OBJ)" + text_font_sfx);
	     file_menu_asml.setToolTipText(text_font_pfx + "ASML macro assemble and link (MLC > BAL > OBJ > 390" + text_font_sfx);
	     file_menu_asmlg.setToolTipText(text_font_pfx + "ASMLG macro assemble, link, and exec 390" + text_font_sfx);
	     file_menu_job.setToolTipText(text_font_pfx + "JOB execute selected batch job file BAT" + text_font_sfx);
	     file_menu_link.setToolTipText(text_font_pfx + "LINK link object files into load module (OBJ > 390)" + text_font_sfx);
	     file_menu_exec.setToolTipText(text_font_pfx + "EXEC execute 390 load module" + text_font_sfx);
	     file_menu_exit.setToolTipText(text_font_pfx + "Exit z390 GUI" + text_font_sfx);
	     edit_menu_cut.setToolTipText(text_font_pfx + "Cut selected text" + text_font_sfx);
	     edit_menu_copy.setToolTipText(text_font_pfx + "Copy selected text to clipboard" + text_font_sfx);
	     edit_menu_paste.setToolTipText(text_font_pfx + "Paste clipboard text (append if log has focus" + text_font_sfx);
	     edit_menu_select_log.setToolTipText(text_font_pfx + "Select all log text" + text_font_sfx);
	     edit_menu_select_cmd.setToolTipText(text_font_pfx + "Select all cmd text" + text_font_sfx);
	     edit_menu_copy_log.setToolTipText(text_font_pfx + "Copy current log file to clipboard" + text_font_sfx);
	     edit_menu_editor.setToolTipText(text_font_pfx + "Launch editor to edit selected data on clipboard" + text_font_sfx);
	     option_menu_ascii.setToolTipText(text_font_pfx + "ASCII use ASCII versus EBCDIC for character set" + text_font_sfx);
	     option_menu_con.setToolTipText(text_font_pfx + "CON List statistics and log output on console" + text_font_sfx);
	     option_menu_dump.setToolTipText(text_font_pfx + "DUMP generate full dump on abnormal termination" + text_font_sfx);
	     option_menu_guam.setToolTipText(text_font_pfx + "Open GUAM Graphical User Access Method dialog for MCS, TN3270, and GKS graphics");
	     option_menu_list.setToolTipText(text_font_pfx + "LIST generate PRN, LST, and/or LOG output files" + text_font_sfx);
	     option_menu_listcall.setToolTipText(text_font_pfx + "LISTCALL trace each macro call and exit on BAL" + text_font_sfx);
	     option_menu_stats.setToolTipText(text_font_pfx + "STATS generate statistics comments" + text_font_sfx);
	     option_menu_amode31.setToolTipText(text_font_pfx + "AMODE31 link 390 with 31 bit addressing" + text_font_sfx);
	     option_menu_rmode31.setToolTipText(text_font_pfx + "RMODE31 link 390 to load above 24 bit limit" + text_font_sfx);
	     option_menu_trace.setToolTipText(text_font_pfx + "TRACE generate trace on BAL, PRN, LST, or LOG file" + text_font_sfx);
	     option_menu_test.setToolTipText(text_font_pfx + "TEST prompt for interactive debug commands" + text_font_sfx);
	     view_menu_status.setToolTipText(text_font_pfx + "Status line view or hide to save space" + text_font_sfx);
	     view_menu_cmd.setToolTipText(text_font_pfx + "Windows batch command input Mode" + text_font_sfx);
	     help_menu_help.setToolTipText(text_font_pfx + "Display summary of basic commands" + text_font_sfx);
	     help_menu_commands.setToolTipText(text_font_pfx + "Display alphabetical list of all commands" + text_font_sfx);
	     help_menu_guide.setToolTipText(text_font_pfx + "Link to online or local docs (if installed)" + text_font_sfx);
	     help_menu_perm.setToolTipText(text_font_pfx + "Display Java security manager permissions" + text_font_sfx);
	     help_menu_releases.setToolTipText(text_font_pfx + "Display OS, Java, and z390 verions" + text_font_sfx);
	     help_menu_support.setToolTipText(text_font_pfx + "Link to www.z390.org online support" + text_font_sfx);
	     help_menu_about.setToolTipText(text_font_pfx + "Display information about this version of z390" + text_font_sfx);
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
   	  	if  (z390_cmd_line.hasFocus()){
   	  	    if  (cmd_mode){
   	  		    if  (cmd_exec_rc() == -1){
   	  		    	cmd_line = z390_cmd_line.getText();
  	  		    	add_cmd_hist();  
   	  		    	put_log("CMD input:" + cmd_line);
   	  		    	reset_z390_cmd();
	   	            cmd_exec_input(cmd_line);
   	  		    } else {
   	  		        put_log("CMD mode has ended");
   	  		        cmd_mode = false;
   	  		        if (main_gui){
   	  		        	view_menu_cmd.setSelected(false);
   	  		        }
                }
     	  		reset_z390_cmd();
   	  	    } else {
   	  	        cmd_line = z390_cmd_line.getText();
   	  	        if  (cmd_line != null
   	  		         && cmd_line.length() > 0){
   	  	             exec_gui_command();
   	  	             reset_z390_cmd();
   	  	        }
   	  	    }
	        return;
   	  	}
   	  boolean event_ok = false;
   	  char first_char = event_name.charAt(0);
   	  switch (first_char){
   	  case 'A':
   	    if (event_name.equals("ABOUT")){
	       z390_cmd_line.setText("ABOUT");
	    }
		if (event_name.equals("AMODE31")){
		   	   if (tz390.opt_amode31){
		   	          z390_cmd_line.setText("AMODE31 OFF");
		 	   } else {
		 	  	   	  z390_cmd_line.setText("AMODE31 ON");
		 	   }
		}
 	 	if (event_name.equals("ASCII")){
  	   	   if (tz390.opt_ascii){
  	   	          z390_cmd_line.setText("ASCII OFF");
  	 	   } else {
  	 	  	   	  z390_cmd_line.setText("ASCII ON");
  	 	   }
  	   	}
 	    if (event_name.equals("ASM..")){
            batch_cmd("ASM","","MLC",asm_opt);
            break;
 	    }
 	    if (event_name.equals("ASML..")){
            batch_cmd("ASML","","MLC",asml_opt);
            break;
 	    }
 	    if (event_name.equals("ASMLG..")){
            batch_cmd("ASMLG","","MLC",asmlg_opt);
            break;
 	    }
   	    break;
   	  case 'B':
   	    break;
   	  case 'C':	
   		if (event_name.equals("CD..")){
   			put_log("CD change current directory");
	  		z390_cmd_line.setText("CD");
	  		break;
   		}
 	  	if (event_name.equals("CMD MODE")){
    	  	   if (cmd_mode){
    	  	       event_ok = true;
                   put_log("CMD mode off");
                   cmd_exec_cancel();
    	  	   } else {
    	  		   z390_cmd_line.setText("CMD");
    	  	   }
       	}
 	  	if (event_name.equals("COMMANDS")){
      	        z390_cmd_line.setText("COMMANDS");
      	}
	  	if (event_name.equals("CON")){
	   	   if (tz390.opt_con){
	   	          z390_cmd_line.setText("CON OFF");
	 	   } else {
	 	  	   	  z390_cmd_line.setText("CON ON");
	 	   }
	   	}
	  	if (event_name.equals("COPY")){
		   	   event_ok = true;
		   	   if  (log_text == focus_comp){
		   	   	log_text.copy();
		   	   }
		   	   if  (z390_cmd_line ==  focus_comp){
	                       z390_cmd_line.copy();
		   	   }
		}
	  	if (event_name.equals("COPY LOG")){
	  			event_ok = true;
	            z390_cmd_line.setText("COPYLOG");
		}
 	  	if (event_name.equals("CUT")){
		   	   event_ok = true;
		   	   if  (z390_cmd_line ==  focus_comp){
	                      z390_cmd_line.cut();
		   	   }
		  	}
 	  	
   	  	break;
   	 case 'D':
 	 	if (event_name.equals("DUMP")){
		   if (tz390.opt_dump){
		       z390_cmd_line.setText("DUMP OFF");
		   } else {
		   	   z390_cmd_line.setText("DUMP ON");
		   }
		} 
	  	break;
   	 case 'E':
   	    if (event_name.toUpperCase().equals("EDIT..")){
           batch_cmd("EDIT","","","");
           break;
	    }
   	    if (event_name.toUpperCase().equals("EDITOR")){
   	    	if (perm_file_execute){
                if (!main_batch){
                   if (!tz390.exec_cmd(tz390.z390_editor)){
                          log_error(16,"editor not found - " + tz390.z390_editor);
                   }
                } else {
                       log_error(54,"interactive editor not supported in batch");
                }
             } else {
                     log_error(17,"Permission for file execute denied");
             }
            break;
 	    }
 	    if (event_name.equals("EXEC..")){
            batch_cmd("EXEC","","390",exec_opt);
            break;
 	    }
   	  	if (event_name.equals("EXIT")){
    	  	event_ok = true;
    	  	exit_command();
   	  	} 
   	  	break;
   	  case 'G':
   	 	if (event_name.equals("GUAM")){
 		   if (tz390.opt_guam){
 		       z390_cmd_line.setText("GUAM OFF");
 		   } else {
 		   	   z390_cmd_line.setText("GUAM ON");
 		   }
 		}
   	    if (event_name.equals("GUIDE")){
   	       z390_cmd_line.setText("GUIDE");
   	    }
   	    break;
   	 case 'H':
   	    if (event_name.equals("HELP")){
	        z390_cmd_line.setText("HELP");
	    }
   	    break;
   	 case 'J':
  	    if (event_name.equals("JOB..")){
            batch_cmd("JOB","","BAT",job_opt);
            break;
 	    }
   	 case 'L':
  	    if (event_name.equals("LINK..")){
            batch_cmd("LINK","","OBJ",link_opt);
            break;
 	    }
	  	if (event_name.equals("LIST")){
		   	   if (tz390.opt_list){
		   	          z390_cmd_line.setText("LIST OFF");
		 	   } else {
		 	  	   	  z390_cmd_line.setText("LIST ON");
		 	   }
	  	}
	  	if (event_name.equals("LISTCALL")){
		   	   if (tz390.opt_listcall){
		   	          z390_cmd_line.setText("LISTCALL OFF");
		 	   } else {
		 	  	   	  z390_cmd_line.setText("LISTCALL ON");
		 	   }
		}
   	    break;	
   	  case 'M':
     	    if (event_name.equals("MAC..")){
                batch_cmd("MAC","","MLC",mac_opt);
                break;
     	    }
     	    break;
      case 'O':
   	  	break;  
	  case 'P':
	  	if (event_name.equals("PASTE")){
	  		   event_ok = true;
	   	       if  (log_text == focus_comp){
	   	       	   z390_cmd_line.setText("PASTE");
	   	       }
		   	   if  (z390_cmd_line ==  focus_comp){
	               z390_cmd_line.paste();
		   	   }
	     }
	  	 if (event_name.equals("PERMISSIONS")){
	      	   z390_cmd_line.setText("PERM");
	     }
	  	 break;
	  case 'R':
	  	 if (event_name.equals("RELEASES")){
	      	   z390_cmd_line.setText("REL");
	     }
		 if (event_name.equals("RMODE31")){
			   	   if (tz390.opt_rmode31){
			   	          z390_cmd_line.setText("RMODE31 OFF");
			 	   } else {
			 	  	   	  z390_cmd_line.setText("RMODE31 ON");
			 	   }
		  }
	  	 break;
	  case 'S':
	  	 if (event_name.equals("SELECT LOG")){
		   	   event_ok = true;
		   	   log_text.requestFocus();
   	       	   log_text.selectAll(); 
	  	 }
	  	 if (event_name.equals("SELECT CMD")){
		   	   event_ok = true; 
		   	   z390_cmd_line.requestFocus();
		   	   z390_cmd_line.selectAll();
		 }
		 if (event_name.equals("STATS")){
		   	   if (tz390.opt_stats){
		   	          z390_cmd_line.setText("STATS OFF");
		 	   } else {
		 	  	   	  z390_cmd_line.setText("STATS ON");
		 	   }
		 }
	  	 if (event_name.equals("STATUS")){
	   	  	   if (main_status){
	   	          z390_cmd_line.setText("STATUS OFF");
	 	  	   } else {
	 	  	   	  z390_cmd_line.setText("STATUS ON");
	 	  	   }
	   	 }
	  	 if (event_name.equals("SUPPORT")){
	       	   z390_cmd_line.setText("SUPPORT");
	     }
         break;
      case 'T':
 		 if (event_name.equals("TEST")){
		   	   if (tz390.opt_test){
		   	          z390_cmd_line.setText("TEST OFF");
		 	   } else {
		 	  	   	  z390_cmd_line.setText("TEST ON");
		 	   }
		 }
		 if (event_name.equals("TRACE")){
		   	   if (tz390.opt_trace){
		   	          z390_cmd_line.setText("TRACE OFF");
		 	   } else {
		 	  	   	  z390_cmd_line.setText("TRACE ON");
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
  			cmd_line = z390_cmd_line.getText();
  			exec_gui_command();
	  		reset_z390_cmd();}	        
   	  	}
   	  private void exec_gui_command(){
   	  /*
   	   * exec command 
   	   */  	   
  		   cmd_line = z390_cmd_line.getText();
  		   if  (cmd_line == null || cmd_line.length() == 0){
  		   	   cmd_line = " ";
  		   }
           reset_z390_cmd();
           if (cmd_running){  // RPI 737
        	   process_command("CMD " + cmd_line);
           } else {
        	   process_command(cmd_line);
           }
   	  } 
   	  private void perm_command(){
   	  /*
   	   * display Java security access permissions
   	   */
   	  	if  (check_perms){
   	  	    put_log("Java Security Manager Permissions - see Java PropertyTool Settings");
   	  	} else {
   	  		log_error(40,"Java Permissions denied due to z390 /NP command option");
   	  	}
   	  	if  (perm_file_user_dir){
   	  		put_log("  Property Permissions - user directory access - ok");
   	  	} else {
   	  		put_log("  Property Permissions - user directory access - denied");
   	  	}
   	  	if  (perm_runtime_thread){
   	  		put_log("  Runtime Permissions - thread modify for popups - ok");
   	  	} else {
   	  		put_log("  Runtime Permissions - thread modify for popups - denied");
   	  	}
   	  	if  (perm_file_execute){
   	  		put_log("  File Permissions - execute for browser/edit - ok");
   	  	} else {
   	  		put_log("  File Permissions - execute for browser/edit - denied");
   	  	}
   	  	if  (perm_file_read){
   	  		put_log("  File Permissions - read - ok");
   	  	} else {
   	  		put_log("  File Permissions - read - denied");
   	  	}
   	  	if  (perm_file_write){
   	  		put_log("  File Permissions - write - ok");
   	  	} else {
   	  		put_log("  File Permissions - write - denied");
   	  	}
  	  }
   	  private void rel_command(){
   	  /*
   	   * display Windows, Java Runtime, and z390
   	   * software releases led
   	   */
    	String temp_version;
    	try {
  	        temp_version = System.getProperty("os.name") + " " + System.getProperty("os.version");
            put_log("System version = " + temp_version);
    	} catch (Exception pe){
    	    put_log("System Version Permission denied");
        }
  	    try {
	        temp_version = System.getProperty("java.vendor") + " " + System.getProperty("java.version");
	        put_log("Java Version = " + temp_version);
  	    } catch (Exception pe){
  	        put_log("Java Version Permission denied");
        }
        put_log("Z390I Version = " + tz390.version);
	  }
   	  private void guide_command(){
   	  /*
   	   * link to z390\webdoc\index.html or www.z390.org
   	   * note start parms are /d"path" file 
   	   */
   		  if (install_webdoc == null){
   			  start_doc(web_site); // RPI 872
   		  } else {
   			  start_doc(install_webdoc); // RPI 872
   		  }
      }
   	  private void reset_z390_cmd(){
   	  /*
   	   * reset z390_cmd text and set focus
   	   */
   	  	   if  (main_gui){
   	  		   z390_cmd_line.setText("");
   	           z390_cmd_line.requestFocus();
   	  	   }
   	  }
   	  private void support_command(){
   	  /*
   	   * link to online support www.z390.org
   	   */	
 	  	start_doc(web_site);
      }
   	public boolean start_doc(String url){
   		   if  (tz390.exec_cmd(tz390.z390_browser + " " + url)){  // RPI 904
   			   put_log("Start issued for " + url);
   			   return true;
   		   } else {
   			   log_error(41,"Start error for " + url);
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
           if  (e.isActionKey()){
           	   if (keyCode == KeyEvent.VK_UP){
           	   		get_prev_cmd();
           	   }
          	   if (keyCode == KeyEvent.VK_DOWN){
          	   	   get_next_cmd();
          	   }
          	   if (keyCode == KeyEvent.VK_F1){   // F1 help
          	   	  process_command("HELP");
          	   }
          	   if (keyCode == KeyEvent.VK_F3){   // F3 exit
        	   	  process_cancel_key();
        	   }
          	   if (keyCode == KeyEvent.VK_F10){   // RPI 630 consume to prevent Windows popup of file menu
         	   	  e.consume();
         	   }
           } else {  // not action key
          	   if (keyCode == KeyEvent.VK_CANCEL){
          	   	  process_cancel_key();;
          	   }
           }
       }
       private void process_cancel_key(){
       /*
        * cancel cmd, or gui cmd in response to
        * F3 or CTRL-BREAK
        */	
       	  if  (cmd_exec_rc() == -1){
	   	  	  if (!cmd_mode){
	   	  	     log_error(70,"previous command execution cancelled");
	   	  	     cmd_exec_cancel();
	   	  	  } else {
	   	  	  	 put_log("CMD mode cancelled");
	   	  	  	 cmd_exec_cancel();
	   	  	  }
	   	  } else {
	   	      abort_error(78,"Aborting due to external shutdown request");
	   	      exit_main(16);
	   	  }
       }
       public void keyTyped(KeyEvent e) {
       /*
        * Handle key typed events
        */
    //dsh displayInfo(e, "KEY TYPED: "); 
       	  /*
       	   * collect any characters for accept
       	   * which are placed in z390_cmd_line
       	   * by accept wait loop if not there 
       	   * already.  First accept they are there,
       	   * but following ones not?  Hooky fix!
       	   */
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
                 JMenuItem popup_edit_menu_editor = new JMenuItem("Edit..");
                 popup_edit_menu.add(popup_edit_menu_cut);
                 popup_edit_menu.add(popup_edit_menu_copy);
                 popup_edit_menu.add(popup_edit_menu_paste);
                 popup_edit_menu.add(popup_edit_menu_select_all);
                 popup_edit_menu.add(popup_edit_menu_copy_log);
                 popup_edit_menu.add(popup_edit_menu_editor);
                 popup_edit_menu_cut.addActionListener(this);
                 popup_edit_menu_copy.addActionListener(this);
                 popup_edit_menu_paste.addActionListener(this);
                 popup_edit_menu_select_all.addActionListener(this);
                 popup_edit_menu_copy_log.addActionListener(this);
                 popup_edit_menu_editor.addActionListener(this);
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
    	   if  (temp_comp == z390_cmd_line){
    	   	   focus_comp = temp_comp;
    	   } else {
    		   focus_comp = log_text; // RPI 1041 
    	   }
    }
	    private void exit_command(){
	    /*
	     * abort command if runnung and turn off cmd_mode
	     * If no command running and not cmd_mode,
	     * then exit z390.
	     */
	    	 if (!cmd_mode && cmd_exec_rc() != -1){
	   	  		exit_main(0);
	    	 } else {
	    	 	cmd_exec_cancel();
	    	 }
	    }
        private void loc_command(String cmd_parm1,String cmd_parm2){
        /*
         * set location of main window x, y
         */
        	int x;
        	int y;
        	if (main_gui 
        		&& cmd_parm1 != null
				&& cmd_parm2 != null){
        		x = get_dec_int(cmd_parm1);
        		y = get_dec_int(cmd_parm2);
        		if  (x < 0){
        			x = 0;
        		} else {
        			if (x + main_width > tz390.max_main_width){
        				if  (x + tz390.min_main_width > tz390.max_main_width){
        					x = tz390.max_main_width - tz390.min_main_width;
        					main_width = tz390.min_main_width;
        				} else {
        				    main_width = tz390.max_main_width - x;
        				}
        			}
        		}
        		if  (y < 0){
        			y = 0;
        		} else {
        			if (y + main_height > tz390.max_main_height){
                        if  (y + tz390.min_main_height > tz390.max_main_height){
                        	y = tz390.max_main_height - tz390.min_main_height;
                        	main_height = tz390.min_main_height;
                        } else {
        				    main_height = tz390.max_main_height - y;
                        }
        			}
        		}
        		main_loc_x = x;
        		main_loc_y = y;
        		main_frame.setLocation(main_loc_x,main_loc_y);
       			main_frame.setSize(main_width,main_height);
           		refresh_request = true;
        	} else {
        		log_error(64,"invalid window location");
        	}
        }
	    private void size_command(String cmd_parm1, String cmd_parm2){
	    /*
	     * resize main window
	     */	
	    	int x;
	    	int y;
	    	if (main_gui
	    		&& cmd_parm1 != null
				&& cmd_parm2 != null){
		    	main_loc_x = (int) main_frame.getLocation().getX();
		    	main_loc_y = (int) main_frame.getLocation().getY();
	    		x = get_dec_int(cmd_parm1);
	    		y = get_dec_int(cmd_parm2);
	    		if  (x < tz390.min_main_width){
	    			x = tz390.min_main_width;
	    		} else {
	    			if (x > tz390.max_main_width - main_loc_x){
	    				x = tz390.max_main_width - main_loc_x;
	    			}
	    		}
	    		if  (y < tz390.min_main_height){
	    			y = tz390.min_main_height;
	    		} else {
	    			if (y > tz390.max_main_height - main_loc_y){
	    				y = tz390.max_main_height - main_loc_y;
	    			}
	    		}
	    		main_width  = x;
	    		main_height = y;
	    		main_frame.setSize(main_width,main_height);
	    		refresh_request = true;
	    	} else {
	    		log_error(65,"invalid window size request");
	    	}
	    	refresh_request = true;
	    }
	    private void status_command(String cmd_parm1,String cmd_parm2){
	    /*
	     * set status line display on or off
	     * or set interval for logging status.
	     * If seconds specified as 0 or null, loggging
	     * status is turned off
	     */	
	    	if (cmd_parm1 != null){
		    		if (cmd_parm1.toUpperCase().equals("ON")){
		    		   if (!main_status){
		    		      main_status = true;
		    		      view_menu_status.setSelected(true);
		    		      if (labels_visible){
		    		         main_panel.add(status_line_label);
		    		      }
		    		      main_panel.add(status_line);
		    		      status_height = font_size + font_space + main_border;
		    		   }
		    	    } else if (cmd_parm1.toUpperCase().equals("OFF")){
		    	       if  (main_status){
		    		       main_status = false;
		    		       view_menu_status.setSelected(false);
		    		       if (labels_visible){
		    		          main_panel.remove(status_line_label);
		    		       }
		    		       main_panel.remove(status_line);
		    		       status_height = 0;
		    	       }
		    		} else {
         	           int sec = get_dec_int(cmd_parm1);
         	           if ( sec >= 1){
         	           	  put_log("Status logging interval set to " + sec + " seconds");
         	              status_interval = sec * 1000;
         	           } else {
         	   	          put_log("Status logging turned off");
         	   	          status_interval = 0;
         	           }
		    		}
		    		refresh_request = true;
           	} else {
         	   log_error(50,"missing immediate data parm");	
         	}
	    }
	    private void title_command(String cmd_parm1,String cmd_parm2){
	    /*
	     * set GUI window title 
	     */	
	    	if (main_gui && cmd_parm1 != null
	    		&& cmd_parm1.length() >= 3){
	    		main_title = cmd_parm1.substring(1,cmd_parm1.length()-1);
	    		title_update();
	    	}
	    }
	    private void timeout_command(String cmd_parm1,String cmd_parm2){
	    /*	
	     * set timeout interval in seconds used to timeout
	     * commands when not in command mode.  Default
	     * is 3 seconds.  Issue command with no arugment to
	     * turn off timeout.  Commands can be cancelled via BREAK.
	     */
	    	  int sec = 0;
	    	  if  (cmd_parm1 != null){
	    	  	  sec = get_dec_int(cmd_parm1);
	    	  	  monitor_timeout_limit = sec * 1000;
	    	  }
	    	  if  (monitor_timeout_limit <= 0){
	    	  	  monitor_timeout_limit = 0;
	    	  	  put_log("Timeout monitor for CMD turned off");
	    	  } else {
	    	  	  put_log("Timeout limit for CMD set to " + sec + " seconds");	  
	          }
	    }
	    private boolean options_command(JCheckBoxMenuItem option_men, String cmd_parm1, String cmd_parm2){
	    	/*
	    	 * check or uncheck option menu item 
	    	 * and update option parm lists for commands
	    	 */
	    	boolean option_flag = false;
	    	if (cmd_parm1.toUpperCase().equals("ON")){
	    		option_flag = true;
	    		option_men.setSelected(true);
	    	} else {
	    		option_flag = false;
	    		option_men.setSelected(false);
	    	}
	    	mac_opt = "";
	    	asm_opt = "";
	    	asml_opt = "";
	    	asmlg_opt = "";
	    	job_opt = "";
	    	link_opt = "";
	    	exec_opt = "";
	    	if (option_menu_ascii.isSelected()){  // do last to override trace setting NOCON
	    		mac_opt   = mac_opt   + " ASCII";
	    		asm_opt   = asm_opt   + " ASCII";
	    		asml_opt  = asml_opt  + " ASCII";
	    		asmlg_opt = asmlg_opt + " ASCII";
	    		job_opt   = job_opt   + " ASCII";
	    		link_opt  = link_opt  + " ASCII";
	    		exec_opt  = exec_opt  + " ASCII";
	    	}
	    	if (!option_menu_con.isSelected()){  // do last to override trace setting NOCON
	    		mac_opt   = mac_opt   + " NOCON";
	    		asm_opt   = asm_opt   + " NOCON";
	    		asml_opt  = asml_opt  + " NOCON";
	    		asmlg_opt = asmlg_opt + " NOCON";
	    		job_opt   = job_opt   + " NOCON";
	    		link_opt  = link_opt  + " NOCON";
	    		exec_opt  = exec_opt  + " NOCON";
	    	}
	    	if (option_menu_dump.isSelected()){  // do last to override trace setting NOCON
	    		asmlg_opt = asmlg_opt + " DUMP";
	    		job_opt   = job_opt   + " DUMP";
	    		exec_opt  = exec_opt  + " DUMP";
	    	}
	    	if (option_menu_guam.isSelected()){  // do last to override trace setting NOCON
	    		asmlg_opt = asmlg_opt + " GUAM";
	    		job_opt   = job_opt   + " GUAM";
	    		exec_opt  = exec_opt  + " GUAM";
	    	}
	    	if (!option_menu_amode31.isSelected()){
	    		asml_opt  = asml_opt  + " NOAMODE31";
	    		asmlg_opt = asmlg_opt + " NOAMODE31";
	    		job_opt   = job_opt   + " NOAMODE31";
	    		link_opt  = link_opt  + " NOAMODE31";
	    	}
	    	if (!option_menu_list.isSelected()){
	    		asm_opt   = asm_opt   + " NOLIST";
	    		asml_opt  = asml_opt  + " NOLIST";
	    		asmlg_opt = asmlg_opt + " NOLIST";
	    		job_opt   = job_opt   + " NOLIST";
	    		link_opt  = link_opt  + " NOLIST";
	    		exec_opt  = exec_opt  + " NOLIST";
	    	}
	    	if (!option_menu_listcall.isSelected()){
	    		mac_opt   = mac_opt   + " NOLISTCALL";
	    		asm_opt   = asm_opt   + " NOLISTCALL";
	    		asml_opt  = asml_opt  + " NOLISTCALL";
	    		asmlg_opt = asmlg_opt + " NOLISTCALL";
	    		job_opt   = job_opt   + " NOLISTCALL";
	    	}
	    	if (option_menu_rmode31.isSelected()){
	    		asml_opt  = asml_opt  + " RMODE31";
	    		asmlg_opt = asmlg_opt + " RMODE31";
	    		job_opt   = job_opt   + " RMODE31";
	    		link_opt  = link_opt  + " RMODE31";
	    	}
	    	if (!option_menu_stats.isSelected()){
	    		mac_opt   = mac_opt   + " NOSTATS";
	    		asm_opt   = asm_opt   + " NOSTATS";
	    		asml_opt  = asml_opt  + " NOSTATS";
	    		asmlg_opt = asmlg_opt + " NOSTATS";
	    		job_opt   = job_opt   + " NOSTATS";
	    		link_opt  = link_opt  + " NOSTATS";
	    		exec_opt  = exec_opt  + " NOSTATS";
	    	}
	    	if (option_menu_test.isSelected()){
	    		asmlg_opt = asmlg_opt + " TEST";
	    		exec_opt  = exec_opt  + " TEST";
	    		job_opt   = job_opt   + " TEST";
	    	}
	    	if (option_menu_trace.isSelected()){
	    		mac_opt   = mac_opt   + " TRACE";
	    		asm_opt   = asm_opt   + " TRACE";
	    		asml_opt  = asml_opt  + " TRACE";
	    		link_opt  = link_opt  + " TRACE";
	    		job_opt   = job_opt   + " TRACE";
	    		asmlg_opt = asmlg_opt + " TRACE";
	    		exec_opt  = exec_opt  + " TRACE";
		    	if (option_menu_con.isSelected()){  // override trace setting NOCON
		    		mac_opt   = mac_opt   + " CON";
		    		asm_opt   = asm_opt   + " CON";
		    		asml_opt  = asml_opt  + " CON";
		    		asmlg_opt = asmlg_opt + " CON";
		    		job_opt   = job_opt   + " CON";
		    		link_opt  = link_opt  + " CON";
		    		exec_opt  = exec_opt  + " CON";
		    	}
	    	}
	    	return option_flag;
	    }
		  private void cd_command(String cmd_parm1){
			  /*
			   * set current directory using file chooser
			   * dialog if parm1 null else use path
			   */
		       	if  (perm_file_user_dir){
		    	    if  (cmd_parm1 == null){
		    	    	if (!main_batch){
		    	    	    select_dir();
		    	    	} else {
		    	            log_error(22,"CD missing directory");
		    	    	}
		    	    } else {
		       	        String new_dir = tz390.get_file_name(tz390.dir_cur,cmd_parm1,"");              		
		       	        File new_dir_file = new File(new_dir);
				  	    if  (new_dir_file.isDirectory()){
				  	    	try {
				  	    		new_dir = new_dir_file.getCanonicalPath() + File.separator; // RPI 235 + RPI 508
				  	    	} catch (Exception e){}
				  	    	tz390.dir_cur = new_dir; 
							dir_cur_file = new File(new_dir);
				  	        System.setProperty("user.dir",tz390.dir_cur);
				  	        if (cmd_mode){
				  	            sync_cmd_dir();
				  	        }
				  	        put_log("CD new current directory - " + System.getProperty("user.dir"));
				  	    } else {
				  	       	log_error(37,"directory not found - " + new_dir);
				  	    }
		    	    }
		    	} else {
		    	  	log_error(23,"Permission for CD change directory denied");
		    	}
			  }
		   private void select_dir(){
		   	    /*
		   	     * Invoke file chooser dialog to
		   	     * set dir_cur
		   	     * (Note dialog is kept for non gui mode to avoid
		   	     *  dispose causing gui shutdown on last window)
		   	     */
		   	   if (perm_select){
		   		   final JFileChooser select_dir_chooser 
		               = new JFileChooser();
		   		   // rebuild every time RPI 541
		   		   create_select_dir(select_dir_chooser);
		   	   } else {
		   	   	 	log_error(38,"Permision for directory selection denied");
		   	   }
		   }
		   private void create_select_dir(final JFileChooser select_dir_chooser){
			   /*
			    * create dialog with file chooser to
			    * select current directory 
			    */
	             select_dir_frame = new JFrame(main_title + " Select Current Directory");
	             select_dir_frame.addWindowListener(new WindowAdapter() {
	     		     public void windowClosing(WindowEvent e) {
	                     select_dir_frame.setVisible(false);
	                     if (main_gui){
	                        main_frame.setVisible(true);
	                     }
	     		     }
	     		 });
	             JPanel select_dir_panel = new JPanel();
	             select_dir_panel.setLayout(new BorderLayout());
	        	 if  (dir_cur_file != null){
	       	         select_dir_chooser.setCurrentDirectory(dir_cur_file);
	        	 }
	         	 select_dir_chooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
	        	 select_dir_chooser.addActionListener(new ActionListener() {
	                 public void actionPerformed(ActionEvent e) {
	                 File selected_file = null;
	                 String selected_dir_name = null;
	                 	String state =
	                 (String)e.getActionCommand();
	                 if (state.equals( // RPI 541
	                    JFileChooser.APPROVE_SELECTION)){
		                 selected_file = select_dir_chooser.getSelectedFile();
	                 } else {
	                	select_dir_frame.setVisible(false);
	                    if (main_gui){
	                       main_frame.setVisible(true);
	                    }
	                 }
	                 if (selected_file != null){
	                 	selected_dir_name = selected_file.getPath();
	                    if  (main_gui){
	                        z390_cmd_line.setText("CD " + "\"" + selected_dir_name + "\"");
	                        z390_cmd_line.postActionEvent();
	                    } else {
	                    	cd_command("CD " + "\"" + selected_dir_name + "\"");
	                    }
	                 } else {
	                 	log_error(37,"directory not selected");
	                 }
	                 select_dir_frame.setVisible(false);
	                 if (main_gui){
	                    main_frame.setVisible(true);
	                 }	 
	                 }});
	             select_dir_panel.add(select_dir_chooser);
	             select_dir_frame.getContentPane().add("North", select_dir_panel);
	             select_dir_frame.pack();
	             select_dir_frame.setLocation(100,100);
	   	     	 if  (main_gui){
	                main_frame.setVisible(false);
	  	     	 }
	             select_dir_frame.setVisible(true);
		   }
		   private void select_file(String file_cmd,String file_type,String file_opt){
			    /*
			     * Invoke file chooser dialog to
			     * set selected_file_name within 
			     * select_file_type if any.
			     * 
			     * Notes:
			     *   1.  Note dialog is kept for non gui mode to avoid
			     *       dispose causing gui shutdown on last window)
			     */
			   select_cmd       = file_cmd;
			   select_file_type = file_type;
			   select_opt       = file_opt; 
			   if (perm_select){
				   final JFileChooser select_file_chooser 
				         = new JFileChooser();
				   // rebuild every time RPI 541
				   select_file_chooser.resetChoosableFileFilters();
				   select_file_chooser.setAcceptAllFileFilterUsed(true);
				   if (select_file_type.length() > 0){
					   select_file_chooser.addChoosableFileFilter(new SelectFileType());
					   select_file_chooser.setAcceptAllFileFilterUsed(false);
				   } else {
					   select_file_type = "ALL";
				   }
				   create_select_file(select_file_chooser);
				   if (main_gui){
					   main_frame.setVisible(false);
				   } 
				   select_file_frame.setLocation(100,100);
				   select_file_frame.setVisible(true);
			   	} else {
			   		log_error(39,"Permission for file selection denied");
			   	}
		   } 
		private void create_select_file(final JFileChooser select_file_chooser){
			/*
			 * create select file frame with chooser
			 * on first call.  It is updated after that.
			 */
            select_file_frame = new JFrame(main_title + " Select " + select_file_type + " file for " + select_cmd);
            select_file_frame.addWindowListener(new WindowAdapter() {
		         public void windowClosing(WindowEvent e) {
                	 select_file_frame.setVisible(false);
                    if (main_gui){
                       main_frame.setVisible(true);
                    }
		         }
		     });
            JPanel select_file_panel = new JPanel();
   	        select_file_panel.setLayout(new BorderLayout());
   	        if  (dir_cur_file != null){
  	             select_file_chooser.setCurrentDirectory(dir_cur_file);
   	        }
   	        select_file_chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
   	        select_file_chooser.addActionListener(new ActionListener() {
                public void actionPerformed(ActionEvent e) {
                    File selected_file = null;
                    String selected_file_name = null;
            	     String state =
                       (String)e.getActionCommand();
                    if (state.equals(  // RPI 541
                       JFileChooser.APPROVE_SELECTION)){
                    	selected_file = select_file_chooser.getSelectedFile();
                    } else {
            		    select_file_frame.setVisible(false);
                        if (main_gui){
                          main_frame.setVisible(true);
                        }
                    }
                    if (selected_file != null){
            	        selected_file_name = selected_file.getPath();
            	        if (select_file_type.length() > 0 && !select_cmd.equals("EDIT")){
            	        	int i = selected_file_name.lastIndexOf(".");
            	        	if (i > 0){
            	        		selected_file_name = selected_file_name.substring(0,i);
            	        	}
            	        }
                       if  (main_gui){
                       	 if (select_cmd.equals("EDIT")){
      	     				 if (!tz390.exec_cmd(tz390.z390_editor
      	     						       + " " + selected_file_name)){
   	     				         log_error(19,"start editor failed - " + tz390.z390_editor);
   	     			         }
                       	 }  else if (select_cmd.equals("JOB")){
                       		selected_file_name = get_short_file_name(selected_file_name);
                       		z390_cmd_line.setText(
	                        			  selected_file_name 
	                        			  + select_opt);
                       		z390_cmd_line.postActionEvent();
                       	 } else {
                       		select_cmd = get_short_file_name(install_loc 
            					       + File.separator 
               			               + select_cmd);
                       		selected_file_name = get_short_file_name(selected_file_name);
                       		z390_cmd_line.setText(
                                select_cmd 
                       			+ " " + selected_file_name
                       			+ " " + select_opt);
                       		z390_cmd_line.postActionEvent();
                       	 }
                       } else {
               	         process_command(select_cmd + " " + get_short_file_name(selected_file_name));
                       }
                    } else {
            	        log_error(37,"file not selected");
                    }
            	    select_file_frame.setVisible(false);
                    if (main_gui){
                        main_frame.setVisible(true);
                    }
                }});
            select_file_panel.add(select_file_chooser);
            select_file_frame.getContentPane().add("North", select_file_panel);
            select_file_frame.pack();
		}
		private String get_short_file_name(String file_name){
			/*
			 * return shortest file name possible
			 * with quotes if LSN
			 */
			if (file_name.length() > tz390.dir_cur.length()
				&& file_name.substring(0,tz390.dir_cur.length()).equals(tz390.dir_cur)){
				file_name = file_name.substring(tz390.dir_cur.length()); // skip dir + sep // RPI 499 remove +1 (already skipping sep)
			}
			if (file_name.length() > 0 && file_name.charAt(0) != '"'){ // RPI 1173
				int index = file_name.indexOf(" ");
				if (index >=0){
					return "\"" + file_name + "\""; // LSN
				}
			}
			return file_name;
		}
	    private void commands_command(String cmd_parm1, String cmd_parm2){
	    /*
	     * display alphabetical list of basic and extended commands
	     */ 
	    	put_log("\nz390 alphabetical command list");
	    	put_log(" ");
	    	put_log("ABOUT                    display summary information about z390 tool      ");
	    	put_log("AMODE31  ON/OFF          set amode 24/31 for link cmd");
	    	put_log("ASCII    ON/OFF          set EBCDIC or ASCII mode mode");
	    	put_log("ASM      MLC file        submit assembly of MLC source to OBJ object code file");
	    	put_log("ASML     MLC file        submit assembly and link MLC source to 390 load module file");
	    	put_log("ASMLG    MLC file        submit assembly, link, and execute 390 load module file");
	    	put_log("CD       directory path  change directory");
	    	put_log("CMD      command         set cmd mode and submit batch cmd");
	    	put_log("Copy                     copy selected text to clipboard (GUI right click)    ");
	    	put_log("COMMANDS                 alphabetical list of all commands                    ");
	    	put_log("CON      ON/OFF          set console output for file cmds");
	    	put_log("COPYLOG                  copy the entire log text to clipboard (GUI only)     ");
	    	put_log("Cut                      cut selected text (GUI right click)                  ");
	    	put_log("Dump     ON/OFF          set option for indicative or full dump on abort");
	    	put_log("Edit     any file        edit source file in seprarate window");
	    	put_log("ERR      nnn             change error msg limit from 100 to nnn (0 is off)");
	    	put_log("EXIT                     exit z390 after closing all files (also CTRL-BREAK");
	    	put_log("EXEC     390 file        submit execution of 390 load module");
	    	put_log("FONT     points          change font size");
	    	put_log("GUI      ON/OFF          Open Graphical User Interface for MCS, 3270, and graphics");
	    	put_log("GUIDE                    view PDF user guide in web browser          ");
	    	put_log("HELP                     display help information summary                     ");
	    	put_log("JOB      BAT file        submit batch job");
	    	put_log("LINK     obj file        submit link obj file into 390 load module");
	    	put_log("LIST     ON/OFF          set PRN, LST, and/or LOG output for file cmds");
	    	put_log("LISTCALL ON/OFF          set trace calls for MAC file cmd");
	    	put_log("LOC      x y pixels      set upper left location of window");
	    	put_log("MAC      mlc file        expand mlc macro file to bal source file");
	    	put_log("PASTE                    paste clipboard text (GUI right click)                ");
	    	put_log("PERM                     display current Java security manager permissions     ");
	    	put_log("REL                      display current release level for OS, Java, and z390");
	    	put_log("RMODE31  ON/OFF          set rmode 24/31 for link cmd");
	    	put_log("SIZE     x y pixels      set width and height of window");
	    	put_log("STATS    ON/OFF          set statistics for BAL, PRN, LST, and/or LOG file cmds");
	    	put_log("STATUS   ON/OFF/sec      set status line on or off or set log interval sec  ");
	    	put_log("SUPPORT                  open browser to online support web site               ");
	    	put_log("TEST     ON/OFF          set prompt for interactive test cmds for EXEC cmd");
	    	put_log("TITLE    'text'          set GUI title");
	    	put_log("TIMEOUT  seconds         set command timeout seconds or 0 for no limit (default");
	    	put_log("TRACE    ON/OFF          set trace for BAL, PRN, LST, and/or LOG file cmds");
	    }
	    /** Returns an ImageIcon, or null if the path was invalid. */
	    protected static ImageIcon createImageIcon(String path,
	                                               String description) {
	        java.net.URL imgURL = z390.class.getResource(path);
	        if (imgURL != null) {
	            return new ImageIcon(imgURL, description);
	        } else {
	            System.err.println("Couldn't find file: " + path);
	            return null;
	        }
	    }
	    private void check_main_view(){
	    /*
	     * if main window size has changed due to
	     * user streching without window event handler
	     * triggering update, do it now.
	     */
	    	if (refresh_request
	    		|| main_width != main_frame.getSize().getWidth()
	    		|| main_height != main_frame.getSize().getHeight()){
	    		main_width = (int) main_frame.getSize().getWidth();
	    		main_height = (int) main_frame.getSize().getHeight();
	    		update_main_view();
	            z390_cmd_line.requestFocus();
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
          if (main_gui){
        		log_height = main_height - title_height - menu_height - command_height - status_height - applet_status_height;
        		log_width  = main_width - scrollbar_width - 4 * main_border;
                main_panel.setSize(main_width - 4 * main_border,main_height - title_height - menu_height - main_border);
                lines_per_page = log_height / log_char_height;
       	        log_view.setPreferredSize(   	        		
   	        		new Dimension(log_width, log_height));
       	        rebuild_lines();
        		main_frame.setVisible(true);
        		refresh_request = true;
        	}
        }
        private void rebuild_lines(){
        /*
         * rebuild z390_cmd and status lines
         * with or without labels to fix current
         * main_panel size.
         * 
         * start by removing labels and lines
         */	
        	if  (labels_visible){
        		main_panel.remove(cmd_label);
        		if (status_visible){
        		   main_panel.remove(status_line_label);
        		}
        	} 
        	main_panel.remove(cmd_label);
        	if  (status_visible){
        		main_panel.remove(status_line);
        	}
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
            z390_cmd_line.setColumns(command_columns);
            main_panel.add(z390_cmd_line);
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
            public int cmd_exec_start(String[] exec_cmd){
                /*
                 * 1. Terminate any prior exec process with
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
                	if  (cmd_exec_process != null){
                		rc = cmd_exec_rc();
                	    if (rc != 0){
                	    	if (rc == -1){
                	    		log_error(75,"previous command execution cancelled"); // RPI 792
                	    	    cmd_exec_cancel();
                	    	} else {
                	    		log_error(71,"previous command execution ended with rc =" + rc);
                	    	}
                	    }
                    }
                	try {
                        cmd_exec_process = Runtime.getRuntime().exec(exec_cmd);
               		    cmd_exec_error_reader  = new BufferedReader(new InputStreamReader(cmd_exec_process.getErrorStream()));
               		    cmd_exec_output_reader = new BufferedReader(new InputStreamReader(cmd_exec_process.getInputStream()));
                   	    cmd_exec_input_writer  = new PrintStream(cmd_exec_process.getOutputStream(),true);  // RPI 731 RPI 888 add true for autoflush
               		    cmd_exec_process_thread = new Thread(this);
            		    cmd_exec_error_reader_thread   = new Thread(this);
            		    cmd_exec_output_reader_thread  = new Thread(this);
            		    cmd_exec_output_msg = "";
            		    cmd_exec_error_msg = "";
            		    int last_io_count = io_count;
            		    cmd_exec_process_thread.start();
            		    cmd_exec_error_reader_thread.start();
            		    cmd_exec_output_reader_thread.start();
        		    	sleep_now();
            		    int wait_count = 5;
            		    while (io_count == last_io_count && wait_count > 0){
            		    	sleep_now();
            		    	wait_count--;
            		    }
            		    return 0;
                	} catch (Exception e){
                        log_error(66,"execution startup error " + e.toString());
                        cmd_exec_cancel();
                        return -1;
                	}
                }
            private void cmd_exec_input(String cmd_line){
            /*
             * send input to exec command in process
             */
            	try {
            		if (cmd_line == null){  // RPI 731
            			cmd_exec_input_writer.println("");
            		} else {
            			cmd_exec_input_writer.println(cmd_line);
            		}
            		cmd_io_total++;
            		monitor_cmd_time_total = 0;
            		cmd_io_total = 0;
            	} catch (Exception e){
            		log_error(68,"execution input error" + e.toString());
            	}
            }
            private int cmd_exec_rc(){
                /*
                 * return ending rc else -1
                 * return 0 if no process defined
                 */           	
                	int rc = -1;
                	if  (cmd_exec_process != null){
                	    try {
                	    	rc = cmd_exec_process.exitValue(); 
                	    } catch (Exception e){
                	    	
                	    }
                	} else {
                		rc = 0;
                	}
                	return rc;
                }
            private void cmd_exec_cancel(){
            /*
             * cancel exec process
             * 
             */
            	ins_count++;
            	if  (cmd_exec_process != null){
            	    try {
            	    	cmd_exec_process.destroy();	    	
            	    } catch (Exception e){
                    	cmd_exec_process = null; 
            		}
            	}
            	cmd_mode = false;
	  		    if (main_gui){
   	  		       	view_menu_cmd.setSelected(false);
   	  		    }
            }
        	public void run() {
        	    if (cmd_exec_process_thread == Thread.currentThread()) {
        			try {
        				cmd_exec_process.waitFor();
        			} catch (Exception e) {
        				log_error(66,"exec execution interruption error" + e.toString());
        				cmd_exec_cancel();
        			}
        			io_count++;
        		} else if (cmd_exec_output_reader_thread == Thread.currentThread()) {
        			copy_cmd_output_to_log();
        		} else if (cmd_exec_error_reader_thread == Thread.currentThread()) {
                    copy_cmd_error_to_log();
        		}
        	}
     private void copy_cmd_output_to_log(){
     /*
      * copy cmd output to log a byte at a time
      * to handle cmd output with cr/lf (ie TIME)
      */	
     	try {
            cmd_exec_output_msg = cmd_exec_output_reader.readLine();
			while (cmd_exec_output_msg != null){
				if (cmd_exec_output_msg.equals("exit_request")){
					// if ez390 issues exit request close down gui
					// this is trigged when ez390 exits if 
					// z390 sent "exit_request to input queue
					cmd_exec_input(" exit");  // RPI 98, RPI 500 RPI 731 RPI 765 RPI 888 leading space may be eaten by PAUSE
				} else {
					put_log(cmd_exec_output_msg);
				}
                cmd_exec_output_msg = cmd_exec_output_reader.readLine();
			}     	
		} catch (Exception ex) {
			if (cmd_exec_rc() == -1){  // RPI 731
			    log_error(67,"exec execution output error");
			    cmd_exec_cancel();
			}
		}
     }
     private void copy_cmd_error_to_log(){
        /*
         * copy cmd error to log a line at a time
         */	
        	try {
        		cmd_exec_error_msg = cmd_exec_error_reader.readLine();
   			   	while (cmd_exec_error_msg != null){
		   			put_log(cmd_exec_error_msg);
		   			cmd_exec_error_msg = cmd_exec_error_reader.readLine();
   			   	}
        	} catch (Exception ex) {
        		if (cmd_exec_rc() == -1){  // RPI 731
        			log_error(73,"exec execution output error"); // RPI 792
        			cmd_exec_cancel();
        		}
        	}
        }
		private void batch_cmd(String bat_cmd, String bat_file_name, String bat_file_type, String bat_opt){
	        /*
	         * invoke batch command with specified file
	         * and options.  If file is null, invoke file
	         * selection dialog with specified file type
	         * and then launch batch command when selection
	         * dialog closes.
	         * 
	         * Note:
	         *   1.  select_file_type is set to filter
	         *       files to type for command.
	         *   2.  select_opt is set to any override
	         *       options for command.  The override
	         *       options are updated on any change to
	         *       the options menu.
	         *   3.  CMD mode is started if not already
	         *       running and mult batch commands just
	         *       queue up for single process.
	         *   4.  EDIT command is launched separately in
	         *       parallel to allow multiple edits to run.
	         *       Editor is defined by EDIT environment variable
	         *       else it uses hard coded default.
	         *   5.  JOB launches selected BAT file with options
	         */
	    	 select_cmd = bat_cmd;
	    	 select_opt = bat_opt;
	    	 if (select_opt == null){
	    		 select_opt = "";
	    	 }
	     	 if (perm_file_execute){
	     	   if (bat_file_name == null || bat_file_name.length() == 0){
	     	     	select_file(bat_cmd,bat_file_type, bat_opt);
	     	   } else {
	     			 if (bat_cmd.equals("EDIT")){
	     				 if (!tz390.exec_cmd("\"" + tz390.z390_editor + "\" \"" + bat_file_name + "\"")){
	     				     log_error(19,"start editor failed - " + tz390.z390_editor);
	     			     }
	     			 } else { 
	     			    if (select_cmd.equals("JOB")){
	     				   cmd_command(get_short_file_name(bat_file_name) 
								   + select_opt);
	     			    } else {
	     				   cmd_command(get_short_file_name(install_loc 
							                          + File.separator 
							                          + bat_cmd)
							   + " " + get_short_file_name(bat_file_name) 
							   + " " + select_opt);
	     			    }
	     			 }
	     	   }
	     	} else {
	     		log_error(17,"Permission for file execute denied");
	     	}

	     }
	 private class SelectFileType extends FileFilter{
		 /*
		  * define accept and getdescription methods
		  * for file chooser to filter files to just
		  * select_file_type if any
		  */
		   public boolean accept(File f) {
			    if (f.isDirectory()) {
				return true;
			    }
			    String extension = getExtension(f);
			    if (extension != null) {
			    	if (extension.toUpperCase().equals(select_file_type) ) {
				        return true;
			    	} else {
			    		return false;
			    	}
			    }

			    return false;
			}
		    public String getDescription() {
		        return "Select files of type " + select_file_type;
		    }

		    private String getExtension(File f) {
		        String ext = null;
		        String s = f.getName();
		        int i = s.lastIndexOf('.');

		        if (i > 0 &&  i < s.length() - 1) {
		            ext = s.substring(i+1).toLowerCase();
		        }
		        return ext;
		    }
		}
	}

