import java.awt.BorderLayout;
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
import java.io.BufferedWriter;
import java.io.File;
import java.io.FilePermission;
import java.io.FileWriter;
import java.io.InputStreamReader;
import java.io.OutputStream;
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
	 * 03/05/05 copied from superzap, update menus, cmds
	 * 09/26/05 replace z390 dialog with batch_cmds and options
	 ********************************************************
     * Global variables
     *****************************************************
     * global command mode variables
     */
	    String startup_cmd_file = null;
	    int first_user_parm = 0;
        int hex_base = 16;
	    boolean echo_cmd = true;
	    boolean console_log = false;
        int error_count = 0;
	    boolean cmd_error = false;
	    int max_error_count = 100;
        boolean main_applet  = false;  // running as browser applet
	    boolean main_gui     = false;     // parm = /g
        boolean main_console = false; // parm = /c
	    boolean main_batch   = false;   // parm = file name
        String  version = null;
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
        boolean show_dates = true; // reset by /RT regtest
   	    SimpleDateFormat mmddyy = new SimpleDateFormat("MM/dd/yy");
   	    SimpleDateFormat hhmmss = new SimpleDateFormat("HH:mm:ss");
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
        Timer   monitor_timer = null;
        int     monitor_wait = 300;
        int     monitor_timeout_limit = 0 * 1000; //dsh
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
        int cmd_io_total = 0;
	Process cmd_exec_process = null;
	InputStreamReader     cmd_exec_error_reader = null;
	InputStreamReader  cmd_exec_output_reader = null;
    OutputStream       cmd_exec_input_writer  = null;
	String cmd_exec_error_msg = "";
	String cmd_exec_output_msg = "";
	Thread cmd_exec_process_thread = null;
	Thread cmd_exec_error_reader_thread = null;
	Thread cmd_exec_output_reader_thread = null;
	int    cmd_exec_rc = 0;
	String cmd_line = null;
    boolean shutdown_exit = false;
    /*
     *  Global GUI objects 
     */
    int ascii_lf = 10;
    int ascii_cr = 13;
 	boolean refresh_request = false;
 	boolean main_status  = true;
        boolean log_text_added = false;
        JFrame main_frame    = null;
        int main_width  = 625;
        int main_height = 400;
        int main_width_max = 800;
        int main_height_max = 600;
        int main_width_min  = 150;
        int main_height_min = 150;
        int main_border = 2;
        int menu_height = 64; 
        int start_menu_height = 36; //windows start bar
        int main_loc_x = 50;
        int main_loc_y = 50;
        int scrollbar_width = 15;
        int font_space = 10;
	int font_size = 12;       //see FONT command
	int font_width = font_size;
        int log_char_height = font_size + font_space; //see FONT command
        int tool_height = 36;     //reset to 0 if hidden;
        int lines_per_page = 0;   //set by update_main_view()
	int log_height = 0;       //set by update_main_view()
	int log_width  = 0;      //set by update_main_view()
	int command_height = font_size + font_space + main_border;
	int command_columns  = 75;
	int status_height  = font_size + font_space + main_border;
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
        int cur_cmd = -1;
        int last_cmd = -1;
        int max_cmd  = 100;
        int view_cmd = -1;
        String[] cmd_history = new String[100];
    /*
     *  Menu items requiring state changes
     */  
        boolean opt_con      = true;
        boolean opt_list     = true;
        boolean opt_listcall = true;
        boolean opt_stats    = true;
        boolean opt_amode31  = true;
        boolean opt_rmode31  = false;
        boolean opt_test     = false;
        boolean opt_trace    = false;
        JCheckBoxMenuItem option_menu_con      = null;
        JCheckBoxMenuItem option_menu_list     = null;
        JCheckBoxMenuItem option_menu_listcall = null;
        JCheckBoxMenuItem option_menu_stats    = null; 
        JCheckBoxMenuItem option_menu_amode31  = null;
        JCheckBoxMenuItem option_menu_rmode31  = null;
        JCheckBoxMenuItem option_menu_test     = null;
        JCheckBoxMenuItem option_menu_trace    = null; 
        JCheckBoxMenuItem view_menu_status  = null;
        JCheckBoxMenuItem view_menu_cmd     = null;
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
           String edit_def = "notepad.exe";
           String edit_cmd = null;
     /*
      * web site and install location
      */
        String web_site = "http://www.z390.org";
        String install_loc = "c:\\program files\\Automated Software Tools\\z390";
//dsh   String install_loc = "D:\\work\\z390"; // dshx
        String install_doc = install_loc + "\\doc"; // dshx
     /*
      * current directory global variables
      */   
        File   cur_dir_file = null;
        String cur_dir = null;
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
            start_button.setToolTipText("Click on z390 icon to start GUI");
            start_button.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                    String[] args = new String[1];           
                    args[0] = "/G";
                    main_applet = true;
                    applet_status_height = status_height;
                    if (set_main_mode(args) == 0){
                 	   process_z390_gui(args);
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
                pgm.process_z390_gui(args);
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
  			String java_vendor  = System.getProperty("java.vendor");
  			String java_version = System.getProperty("java.version");
  			if (!java_vendor.equals("Sun Microsystems Inc.")
  				||	java_version.compareTo("1.4") < 0
  				|| java_version.compareTo("9.9" ) > 0){
  				MessageBox box = new MessageBox();
  				box.messageBox("z390 Error",
				    "Unsupported Java Version " +                                                        java_vendor + " " + java_version);
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
  	//dsh	 check_perms = false; //for testing applet perms   
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
            if (perm_file_log){
   		  	   cur_dir = System.getProperty("user.dir");
		  	   cur_dir_file = new File(cur_dir);
            }
            perm_select   = perm_file_read  
			                && perm_file_user_dir 
							&& perm_runtime_thread;
            if (!perm_file_log){
                perm_file_write = false;
                if  (main_batch){
                	System.out.println("z390 Error " + "S015 batch log permission denied - aborting");
                	shut_down(16);
                }
            }
           /*******************************************
            * Set version
            *******************************************/  
            version = "V1.0.00 09/30/05";  //dsh
            main_title = "z390 " + version;
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
  						show_dates = false;
  					}  					
  					if  (args[index1].toUpperCase().equals("/SC")){
  						index1++;
  						if (index1 < args.length){
  	  						parm_ok = true;
  							startup_cmd_file = args[index1];
  							if (startup_cmd_file.indexOf(' ') != -1){
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
 	   private String get_file_name(String parm){
 	   /*
 	    * Strip long spacey name quotes if found
 	    * 
 	    * Add current directory if not specified
 	    */
 	   	    String file_name = null;
 	    	if (parm.charAt(0) == '\"' 
   	    		|| parm.charAt(0) == '\''){
   	    		file_name = parm.substring(1,parm.length() - 1);
   	    	} else {
   	    	    file_name = parm;
   	    	}
 	    	if  (cur_dir == null){
 	    		
 	    	}
 	    	if  (file_name.charAt(0) != '\\'
 	    		 && (file_name.length() < 2 
 	    		     || file_name.charAt(1) != ':')){
 	    		file_name = cur_dir.concat(File.separator + file_name);	
 	    	}
 	    	return file_name;
 	    }
		private void open_log_file() {
			/*
			 * Open log file
			 */ 
	      if (perm_file_log){	
			  log_file_name = get_file_name("z390");
		      try {
		      	  if  (log_tod){
		      	      boolean new_log = false;
		      	      String temp_log_name = "temp";
		              while (!new_log){
			              Date tod = new Date();
			              SimpleDateFormat tod_format = new SimpleDateFormat("_yyyy_MMdd_HHmmss");
				          String log_file_tod = tod_format.format(tod);
			              temp_log_name = log_file_name + log_file_tod + ".log";
				          File temp_log_file = new File(temp_log_name);
		                  if  (temp_log_file.exists()){
			                  Thread.sleep(1000);
		                  } else {
		              	      new_log = true;
		                  }
		              }
		              log_file_name = temp_log_name;
				  } else {
				     log_file_name = log_file_name.concat(".log");
		      	  }
		          log_file = new BufferedWriter(new FileWriter(log_file_name));
		          put_copyright();
		 	   	  put_log("Log file = " + log_file_name);
		      } catch (Exception e){
		      	  abort_error(1,"log file open - " + e.toString());
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
			if (log_file != null){
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
            put_log("z390 total errors = " + error_count);
	        close_log_file();
		}
		private void log_error(int error,String msg){
			error_count++;
			cmd_error = true;
			msg = "z390 Error " + error + " " + msg;
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
			if  (error_count > max_error_count){
		        abort_error(10,"maximum errors exceeded");
            }
		}
		private void abort_error(int error,String msg){
			error_count++;
			msg = "z390 Error " + error + " " + msg;
			if  (main_batch){
				put_log(msg);
				if (!console_log){
				   System.out.println(msg);
				}
		        exit_main(16);
			} else {
				put_log(msg);
			}
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
				showStatus("z390 total errors = " + error_count);
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
                            abort_error(78,"aborting due to external shutdown request");
            	        }
                    }
                });
  		     }
	   }
	   private void put_copyright(){
	   /*
	    * display z390 version and copyright
	    */
	   	put_log("z390 " + version);
	   	put_log("Copyright 2005 Automated Software Tools Corporation");
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
	   	 * Append any output from CMD still in buffers
	   	 * to front of msg with \n
	   	 */
	   	    String work_msg = "";
	   	    if  (cmd_exec_output_msg.length() > 0){
	   	    	work_msg = cmd_exec_output_msg;
	   	    	cmd_exec_output_msg = "";
	   	    }
	   	    if  (cmd_exec_error_msg.length() > 0){
	   	    	if  (work_msg.length() > 0){
	   	    	    work_msg = work_msg + "\n" + cmd_exec_error_msg + "\n";
	   	    	} else {
	   	    		work_msg = cmd_exec_error_msg;
	   	    	}
	   	    	cmd_exec_error_msg = "";
	   	    }
	   	    if  (work_msg.length() > 0){
	   	    	if  (msg.length() > 0){
	   	    		msg = work_msg + "\n" + msg;
	   	    	} else {
	   	    		msg = work_msg;
	   	    	}
	   	    } else if (msg.length() == 0){
	   	    	return;  // false alarm fixed by sync
	   	    }
	   	    io_count++;
	        if  (main_gui){
  	        	log_text.append(msg + "\n");
   	        	log_text_added = true;
   	        } else {
	   	        if  (main_console || console_log) {
	   	    	    System.out.println(msg);
	   	        }
   	        }
	   	    if (log_file != null){
	   	       try {
	   	    	   log_file.write(msg + "\r\n");
	   	       } catch (Exception e) {
	   	    	   abort_error(2,"write to log error - " + e.toString());
	   	       }	   	 	
	   	    }
	   }
	   private String process_command(String cmd_line) {
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
	   	    cmd_error = false;
	   	    if  (cmd_line == null 
	   	    		|| cmd_line.length() == 0
					|| cmd_line.equals(" ")){
	   	    	cmd_line = " ";
	   	    	return cmd_line;
	   	    }
  	        String cmd_opcode = null;
            String cmd_parm1 = null;
            String cmd_parm2 = null;
	   	    boolean cmd_opcode_ok = false;
            StringTokenizer st = new StringTokenizer(cmd_line," ,\'\"",true);
            String next_token;
               cur_cmd++;
               if (cur_cmd >= max_cmd){
                  cur_cmd = 0;
               } else {
                  last_cmd = cur_cmd;
               }
               cmd_history[cur_cmd] = cmd_line;
            view_cmd = -1;
            cmd_opcode = get_next_parm(st,true).toUpperCase();
            if (st.hasMoreTokens()) {
                cmd_parm1 = get_next_parm(st,true);
                if (st.hasMoreTokens()) {
                    cmd_parm2 = get_next_parm(st,true);
                    if  (cmd_parm2 != null 
                    		&& cmd_parm2.equals(",")){
                    	cmd_parm2 = get_next_parm(st,false);
                    }
                    boolean comments = false;
                    while (st.hasMoreTokens()
                    		&& !comments){
                        next_token = st.nextToken();
                        if (next_token != null
                        	&& next_token.equals(",")
							&& st.hasMoreTokens()){   
                           next_token = get_next_parm(st,false);
                           if  (next_token != null 
                           		&& !next_token.equals(" ")){
                        	   cmd_parm2 = cmd_parm2 + "," + next_token;
                           }
                        } else {
						    comments = true;
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
             	opt_amode31 = options_command(option_menu_amode31, cmd_parm1,cmd_parm2);
             	break;
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
             	opt_con = options_command(option_menu_con, cmd_parm1,cmd_parm2);
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
        	    	cmd_command(null);
        	    } else {
        	    	if (!cmd_mode){
        	    		cmd_command(null); // turn on cmd mode for batch
        	    	}
        	    	int index1 = cmd_line.toUpperCase().indexOf("CMD") + 3;
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
             	opt_list = options_command(option_menu_list, cmd_parm1,cmd_parm2);
             	break;
            }
         	if  (cmd_opcode.equals("LISTCALL")){
             	cmd_opcode_ok = true;
             	opt_listcall = options_command(option_menu_listcall, cmd_parm1,cmd_parm2);
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
         case 'N': 
            if  (cmd_opcode.equals("NOTEPAD")){
            	cmd_opcode_ok = true;
            	if (perm_file_execute){
            	   if (!main_batch){
                      if (!exec_cmd("notepad.exe")){
                	     log_error(16,"notepad.exe not found");
                      }
            	   } else {
            	   	  log_error(54,"interactive command not supported in batch");
            	   }
            	} else {
            		log_error(17,"Permission for file execute denied");
            	}
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
             	opt_rmode31 = options_command(option_menu_rmode31, cmd_parm1,cmd_parm2);
             	break;
            }
            break;
         case 'S':
        	if  (cmd_opcode.equals("STATS")){
             	cmd_opcode_ok = true;
             	opt_stats = options_command(option_menu_stats,cmd_parm1,cmd_parm2);
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
             	opt_test = options_command(option_menu_test, cmd_parm1,cmd_parm2);
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
             	opt_trace = options_command(option_menu_trace, cmd_parm1,cmd_parm2);
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
            }
            return cmd_line;
       }
	   private String time_stamp(){
		   /*
		    * return date and time if show_dates
		    */
   	    String temp_date_text = "";
	    if  (show_dates){
	        Date temp_date = new Date(); 
            temp_date_text = "Date = " + mmddyy.format(temp_date)
              + " Time = " + hhmmss.format(temp_date);
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
	   	  put_log("z390 portable mainframe macro assembler, linker, and emulator tool");
	   	  put_log("  * edit, assemble, link, and execute mainframe assembler code");
	   	  put_log("  * use interactive GUI, command line, or batch interface");
	   	  put_log("  * macro assembler compatible with HLASM");
	   	  put_log("  * linker supports AMODE/RMODE relocatable load modules");
	   	  put_log("  * emulator supports 32/64 bit plus HFP/BFP floating point instructions");
	   	  put_log("  * emulator supports OS basic svcs such as READ/WRITE at macro level");
	   	  put_log("  * emulator includes powerful trace and test debug tools");
          put_log("  * z390 distributed as InstallShield exe for Windows and as zip");
          put_log("  * z390 includes example demos and regression tests");
          put_log("  * z390 written entirely in J2SE 1.5.0 comatible Java");
          put_log("  * z390 distributed with source under GNU open source license");
          put_log("  * z390 open source project for support and extensions");         
          put_log("Visit www.z390.org for additional information");
	   }
	   private void font_command(String cmd_parm1,String cmd_parm2){
	   /* 
	    * reset font size for log, and command line
	    */
	   	    int new_font_size;
	   	    if (cmd_parm1 != null){
	   	    	new_font_size = get_dec_int(cmd_parm1);
	   	    	if (new_font_size < 8 || new_font_size > 72){
	   	    		log_error(63,"font outside New Courier fixed width font limits");
	   	    	} else {
	   	    		if (main_gui){
		   	    		font_size = new_font_size;
		   	    		log_char_height = font_size + font_space;
		   	    		command_height = font_size + font_space + main_border;
		   	    		status_height  = font_size + font_space;
	   	                log_text.setFont(new Font("Courier",Font.BOLD,font_size));
	   	                cmd_label.setFont(new Font("Courier",Font.BOLD,font_size));
	   	                z390_cmd_line.setFont(new Font("Courier",Font.BOLD,font_size));
	   	                status_line_label.setFont(new Font("Courier",Font.BOLD,font_size));
	   	                status_line.setFont(new Font("Courier",Font.BOLD,font_size));
	   	                refresh_request = true;
	   	    		}
	   	    	}	
	   	    } else {
	   	    	log_error(63,"font outside New Courier fixed width font limits");
	   	    }
	   }	   private int get_dec_int(String cmd_parm){
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
        put_log("Type GUIDE to view User Guide PDF documentation");
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
	   	    String cmd_pgm;
	   	    String[] cmd_parms;
	   	    try {
	   	    	String os_name = System.getProperty("os.name");
                if(    os_name.equals("Windows 95")
                	|| os_name.equals("Windows 98")){
	                cmd_pgm = "command.com" ;
	            } else {
	            	cmd_pgm = "cmd.exe";
	            }
                if  (cmd_line != null){
                	cmd_mode = false;
   	  		        if (main_gui){
   	  		        	view_menu_cmd.setSelected(false);
   	  		        }
                	cmd_parms = new String[3];
                	cmd_parms[0] = cmd_pgm;
                	cmd_parms[1] = "/C";
                    cmd_parms[2] = cmd_line;
                } else {
                	cmd_mode = true;
   	  		        if (main_gui){
   	  		        	view_menu_cmd.setSelected(true);
   	  		        }
                	cmd_parms = new String[1];
                	cmd_parms[0] = cmd_pgm;
                }
            	rc = cmd_exec_start(cmd_parms);
                if  (rc == 0){
   		            monitor_cmd_time_total = 0;
   		            if (!main_gui){
   		            	while (cmd_exec_rc() == -1){
				               try {  // wait until done if not gui
		                           Thread.sleep(monitor_wait);
		                       } catch (Exception e){
		      	                   abort_error(77,"Wait interrupted " + e.toString() );
		                       }
				           }
   		            }
	   	        } else {
	   	        	log_error(66,"execution startup error rc = " + rc);
    	 	        cmd_exec_cancel();
	   	        }
                return rc;
	   	    } catch (Exception e) {
  			    log_error(66,"execution startup error - " + e.toString());
                cmd_exec_cancel();
	   		    return 16;
    	 	}
	   }
	   private void monitor_update(){
	   /*
	    * 1.  At monitor_wait intervals, update the
	    *     GUI title date and time and the status
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
     	    if (cmd_exec_output_msg.length() + cmd_exec_error_msg.length() > 0){
	   	    	put_log("");
	   	    }
	   	    if  (cmd_mode && cmd_exec_rc() != -1){
	   	    	put_log("CMD mode ended");
	   	    	cmd_mode = false;
	  		    if (main_gui){
   	  		       	view_menu_cmd.setSelected(false);
   	  		    }
	   	    }
	        monitor_next_time = System.currentTimeMillis();
	        monitor_next_ins_count = ins_count;
	        monitor_next_io_count = io_count;
            monitor_cur_interval = monitor_next_time - monitor_last_time;
	   	    monitor_cmd_time_total = monitor_cmd_time_total + monitor_cur_interval;
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
  	       abort_error(69,"CMD command timeout error");
	   	   reset_z390_cmd();
	   }
	   private void cmd_command(String cmd_line){
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
	   	     if  (cmd_exec_rc() == -1){
	   	    	 cmd_exec_input(cmd_line); // route cmd to existing batch processor
	   	     } else {
	             cmd_startup(cmd_line);
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
      * gui status line and status log requests
      * 
      *     1.  Time of date
   	  *     2.  INS total
   	  *     3.  I/O total
   	  *     4.  RBA
   	  *     5.  CMD mode
      */
     	String cmd_mode_text = "";
     	if (cmd_exec_rc() == -1){
     	   cmd_mode_text = " CMD";
     	}
     	String status_text = time_stamp()
		  + " INS=" + get_pad(ins_count,6)
		  + " IO=" + get_pad(io_count,6)
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
   private void process_z390_gui(String[] args){
       /*
        * set runtime cancel hooks
        */
   	        set_runtime_hooks();
       /*
        * Invoke graphical user interface
        */ 
            main_frame = new JFrame();
            title_update();
            try {
                main_height_max = GraphicsEnvironment.getLocalGraphicsEnvironment().getScreenDevices()[0].getDisplayMode().getHeight() - start_menu_height;
                main_width_max = GraphicsEnvironment.getLocalGraphicsEnvironment().getScreenDevices()[0].getDisplayMode().getWidth();
            } catch (Exception e){

            }
            main_frame.setSize(main_width,main_height);
            main_frame.setLocation(main_loc_x,main_loc_y);
            main_frame.addComponentListener(this);
            build_menu_bar();
            build_main_panel();
            open_log_file();
            edit_cmd = System.getenv("EDIT");
            if (edit_cmd == null || edit_cmd.length() == 0){
            	edit_cmd = edit_def;
            }
            monitor_startup();
			if (startup_cmd_file != null){
				cmd_command(null); //switch to cmd mode
				cmd_exec_input(startup_cmd_file);
			}
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
            build_log_view();
            build_z390_cmd_line();
            build_status_line();
       	    main_frame.getContentPane().add(main_panel);
	        main_frame.addWindowListener(new WindowAdapter() {
	  		       public void windowClosing(WindowEvent e) {
	  		           exit_main(0);
	  		       }
	  		      });
        } 
        private void exit_main(int rc){
        	close_all_files();
	        shut_down(rc);
        }
        
     private void build_log_view(){
        log_text = new JTextArea();
        log_text.setFont(new Font("Courier",Font.BOLD,font_size));
	        log_text.addMouseListener(this);
        log_view = new JScrollPane(log_text);
        log_view.getVerticalScrollBar().addAdjustmentListener(new AdjustmentListener(){
    	  public void adjustmentValueChanged(AdjustmentEvent e){
    		if (log_text_added){
                log_text_added = false;
    			log_view.getVerticalScrollBar().setValue(log_view.getVerticalScrollBar().getMaximum());
    		}       
    	  }});
        log_view.setVerticalScrollBarPolicy(
            JScrollPane.VERTICAL_SCROLLBAR_ALWAYS);
		log_height = main_height - menu_height - tool_height - command_height - status_height - applet_status_height;
		log_width  = main_width - scrollbar_width - 4 * main_border;
	    main_panel.add(log_view);
	    lines_per_page = log_height / log_char_height;
	    log_view.setPreferredSize(   	        		
         	new Dimension(log_width, log_height));
     }
     private void build_z390_cmd_line(){
	/*
     *   Build the command entry field
     */
     	cmd_label = new JLabel("Command: ");
        cmd_label.setFont(new Font("Courier",Font.BOLD,font_size));
     	main_panel.add(cmd_label);
        z390_cmd_line = new JTextField(command_columns);
        z390_cmd_line.setFont(new Font("Courier",Font.BOLD,font_size));
        z390_cmd_line.addActionListener(this);
	    z390_cmd_line.addMouseListener(this);
        z390_cmd_line.addKeyListener(this);
        z390_cmd_line.addFocusListener(this);
	    main_panel.add(z390_cmd_line);
     }
     private void build_status_line(){
    	/*
         *   Build the statuts line
         */
     	    status_line_label = new JLabel(" Status: ");
            status_line_label.setFont(new Font("Courier",Font.BOLD,font_size));
     	    main_panel.add(status_line_label);
            status_line = new JTextField(command_columns);
            status_line.setFont(new Font("Courier",Font.BOLD,font_size));
            status_line.addActionListener(this);
	        status_line.addMouseListener(this);
            status_line.addKeyListener(this);
            status_line.addFocusListener(this);
            main_panel.add(status_line);
         }
     private void build_menu_bar(){
    /* 
     *    Build the menu bar
     */
     JMenuBar menuBar = new JMenuBar();
     JMenu file_menu = new JMenu("File");
     JMenu edit_menu = new JMenu("EDIT");
     JMenu option_menu = new JMenu("Options");
     JMenu view_menu = new JMenu("View");
     JMenu help_menu = new JMenu("Help");
     menuBar.add(file_menu);
     menuBar.add(edit_menu);
     menuBar.add(option_menu);
     menuBar.add(view_menu);
     menuBar.add(help_menu);
     JMenuItem file_menu_cd     = new JMenuItem("CD..");
     JMenuItem file_menu_edit   = new JMenuItem("EDIT..");
     JMenuItem file_menu_mac    = new JMenuItem("MAC..");
     JMenuItem file_menu_asm    = new JMenuItem("ASM..");
     JMenuItem file_menu_asml   = new JMenuItem("ASML..");
     JMenuItem file_menu_asmlg  = new JMenuItem("ASMLG..");
     JMenuItem file_menu_job    = new JMenuItem("JOB..");
     JMenuItem file_menu_link   = new JMenuItem("LINK..");
     JMenuItem file_menu_exec   = new JMenuItem("EXEC..");
     JMenuItem file_menu_exit   = new JMenuItem("Exit");
     JMenuItem edit_menu_cut    = new JMenuItem("Cut");
     JMenuItem edit_menu_copy   = new JMenuItem("Copy");
     JMenuItem edit_menu_paste  = new JMenuItem("Paste");
     JMenuItem edit_menu_select_all = new JMenuItem("Select All");
     JMenuItem edit_menu_copy_log   = new JMenuItem("Copy Log");
     JMenuItem edit_menu_notepad = new JMenuItem("Notepad");
     option_menu_con = new JCheckBoxMenuItem("CON");
     option_menu_con.setSelected(true);
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
     JMenuItem help_menu_help       = new JMenuItem("Help");
     JMenuItem help_menu_commands   = new JMenuItem("Commands");
     JMenuItem help_menu_guide      = new JMenuItem("Guide");
     JMenuItem help_menu_perm       = new JMenuItem("Permissions");
     JMenuItem help_menu_releases   = new JMenuItem("Releases");
     JMenuItem help_menu_support    = new JMenuItem("Support");
     JMenuItem help_menu_about      = new JMenuItem("About");
     /*
      * Add tool tips
      */
     file_menu_cd.setToolTipText("CD change directory");
     file_menu_edit.setToolTipText("Edit source file with notepad");
     file_menu_mac.setToolTipText("MAC macro expand (MLC > BAL)");
     file_menu_asm.setToolTipText("ASM macro assemble (MLC > BAL > OBJ)");
     file_menu_asml.setToolTipText("ASML macro assemble and link (MLC > BAL > OBJ > 390");
     file_menu_asmlg.setToolTipText("ASMLG macro assemble, link, and exec 390");
     file_menu_job.setToolTipText("JOB execute selected batch job file BAT");
     file_menu_link.setToolTipText("LINK link object files into load module (OBJ > 390)");
     file_menu_exec.setToolTipText("EXEC execute 390 load module");
     file_menu_exit.setToolTipText("Exit z390 GUI");
     edit_menu_cut.setToolTipText("Cut selected text");
     edit_menu_copy.setToolTipText("Copy selected text to clipboard");
     edit_menu_paste.setToolTipText("Paste clipboard text (append if log has focus");
     edit_menu_select_all.setToolTipText("Select all text");
     edit_menu_copy_log.setToolTipText("Copy current log file to clipboard");
     edit_menu_notepad.setToolTipText("Launch notepad to edit selected data on clipboard");
     option_menu_con.setToolTipText("CON List statistics and log output on console");
     option_menu_list.setToolTipText("LIST generate PRN, LST, and/or LOG output files");
     option_menu_listcall.setToolTipText("LISTCALL trace each macro call and exit on BAL");
     option_menu_stats.setToolTipText("STATS generate statistics comments");
     option_menu_amode31.setToolTipText("AMODE31 link 390 with 31 bit addressing");
     option_menu_rmode31.setToolTipText("RMODE31 link 390 to load above 24 bit limit");
     option_menu_trace.setToolTipText("TRACE generate trace on BAL, PRN, LST, or LOG file");
     option_menu_test.setToolTipText("TEST prompt for interactive debug commands");
     view_menu_status.setToolTipText("Status line view or hide to save space");
     view_menu_cmd.setToolTipText("Windows batch command input Mode");
     help_menu_help.setToolTipText("Display summary of basic commands");
     help_menu_commands.setToolTipText("Display alphabetical list of all commands");
     help_menu_guide.setToolTipText("Link to PDF User Guide");
     help_menu_perm.setToolTipText("Display Java security manager permissions");
     help_menu_releases.setToolTipText("Display OS, Java, and z390 verions");
     help_menu_support.setToolTipText("Link to www.z390.org online support");
     help_menu_about.setToolTipText("Display information about this version of z390");
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
     edit_menu_select_all.setMnemonic(KeyEvent.VK_S);
     edit_menu_copy_log.setMnemonic(KeyEvent.VK_L);
     edit_menu_notepad.setMnemonic(KeyEvent.VK_N);
     option_menu_con.setMnemonic(KeyEvent.VK_C);
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
     edit_menu_notepad.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_N,ActionEvent.CTRL_MASK));
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
     edit_menu_select_all.addActionListener(this);
     edit_menu_copy_log.addActionListener(this);
     edit_menu_notepad.addActionListener(this);
     option_menu_con.addActionListener(this);
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
     edit_menu.add(edit_menu_select_all);
     edit_menu.add(edit_menu_copy_log);
     edit_menu.add(edit_menu_notepad);
     option_menu.add(option_menu_con);
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
     main_frame.setJMenuBar(menuBar);
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
		   	   if (opt_amode31){
		   	          z390_cmd_line.setText("AMODE31 OFF");
		 	   } else {
		 	  	   	  z390_cmd_line.setText("AMODE31 ON");
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
	   	   if (opt_con){
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
	  	break;
   	 case 'E':
   	    if (event_name.equals("EDIT..")){
           batch_cmd("EDIT","","","");
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
		   	   if (opt_list){
		   	          z390_cmd_line.setText("LIST OFF");
		 	   } else {
		 	  	   	  z390_cmd_line.setText("LIST ON");
		 	   }
	  	}
	  	if (event_name.equals("LISTCALL")){
		   	   if (opt_listcall){
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
      case 'N':
	  	if (event_name.equals("NOTEPAD")){
	           z390_cmd_line.setText("NOTEPAD");
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
			   	   if (opt_rmode31){
			   	          z390_cmd_line.setText("RMODE31 OFF");
			 	   } else {
			 	  	   	  z390_cmd_line.setText("RMODE31 ON");
			 	   }
		  }
	  	 break;
	  case 'S':
	  	 if (event_name.equals("SELECT ALL")){
		   	   event_ok = true;
	   	       if  (log_text == focus_comp){
	   	       	   log_text.selectAll(); 
	   	       }
		   	   if  (z390_cmd_line ==  focus_comp){
	               z390_cmd_line.selectAll();
		   	   }
		 }
		 if (event_name.equals("STATS")){
		   	   if (opt_stats){
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
		   	   if (opt_test){
		   	          z390_cmd_line.setText("TEST OFF");
		 	   } else {
		 	  	   	  z390_cmd_line.setText("TRACE ON");
		 	   }
		 }
		 if (event_name.equals("TRACE")){
		   	   if (opt_trace){
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
           process_command(cmd_line); 
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
   	  		put_log("  File Permissions - execute for browser/notepad - ok");
   	  	} else {
   	  		put_log("  File Permissions - execute for browser/notepad - denied");
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
        put_log("z390 Version = " + version);
	  }
   	  private void guide_command(){
   	  /*
   	   * link to PDF User Guide
   	   * note start parms are /d"path" file 
   	   */	
 	  	start_doc("/d" +  "\"" + install_doc + "\" z390_User_Guide.pdf");
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
   	  private boolean start_doc(String file_name){
	       if  (exec_cmd("cmd.exe /c Start " +  file_name)){
   	  	       put_log("Start issued for " + file_name);
	       	   return true;
   	       } else {
	           log_error(41,"Start error for " + file_name);
	       	   return false;
	       }
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
           if  (e.isActionKey()){
           	   if (keyCode == KeyEvent.VK_UP
           	   		&& last_cmd >= 0){ // up arrow  - backup
           	   	   if  (view_cmd < 1){
           	   	   	   view_cmd = cur_cmd;
           	   	   } else {
           	   	       view_cmd--;  
           	   	       if  (view_cmd < 1){
           	   	  	       view_cmd = last_cmd;
           	   	  	       if (view_cmd == cur_cmd){
           	   	  	       	  view_cmd = 1;
           	   	  	       }
           	   	       }
           	   	       if  (view_cmd == cur_cmd){
           	   	   	       view_cmd = cur_cmd + 1;
           	   	       }
           	   	   }
           	   	   z390_cmd_line.setText(cmd_history[view_cmd]);
           	   }
          	   if (keyCode == KeyEvent.VK_DOWN
          	   		&& last_cmd >= 0){ // down arrow
          	   	   if  (view_cmd < 1){
          	   	   	   view_cmd = cur_cmd;
          	   	   } else {
   	   	  	           view_cmd++;
          	   	       if  (view_cmd > last_cmd){
         	   	  	       view_cmd = 1;
         	   	  	       if  (cur_cmd == last_cmd){
         	   	  	       	   view_cmd = last_cmd;
         	   	  	       }
          	   	       }
         	   	  	   if  (view_cmd == cur_cmd + 1){
         	   	  	   	   view_cmd = last_cmd;
         	   	  	   }
          	   	   }
         	   	   z390_cmd_line.setText(cmd_history[view_cmd]);
          	   }
          	   if (keyCode == KeyEvent.VK_F1){   // F1 help
          	   	  process_command("HELP");
          	   }
          	   if (keyCode == KeyEvent.VK_F3){   // F3 exit
        	   	  process_cancel_key();
        	   }
           } else {  // not action key
          	   if (keyCode == KeyEvent.VK_CANCEL){
          	   	  process_cancel_key();
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
    	   		|| focus_comp == log_view){
    	   	  focus_comp = log_text;
    	   }
    	   if  (temp_comp == z390_cmd_line){
    	   	   focus_comp = temp_comp;
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
	    	} else {
	    		log_error(65,"invalid window size request");
	    	}
	    	refresh_request = true;
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
	    private boolean options_command(JCheckBoxMenuItem opt_menu, String cmd_parm1, String cmd_parm2){
	    	/*
	    	 * check or uncheck option menu item 
	    	 * and update option parm lists for commands
	    	 */
	    	boolean opt_flag = false;
	    	if (cmd_parm1.toUpperCase().equals("ON")){
	    		opt_flag = true;
	    		opt_menu.setSelected(true);
	    	} else {
	    		opt_flag = false;
	    		opt_menu.setSelected(false);
	    	}
	    	mac_opt = "";
	    	asm_opt = "";
	    	asml_opt = "";
	    	asmlg_opt = "";
	    	job_opt = "";
	    	link_opt = "";
	    	exec_opt = "";
	    	if (!option_menu_con.isSelected()){  // do last to override trace setting NOCON
	    		mac_opt   = mac_opt   + " NOCON";
	    		asm_opt   = asm_opt   + " NOCON";
	    		asml_opt  = asml_opt  + " NOCON";
	    		asmlg_opt = asmlg_opt + " NOCON";
	    		link_opt  = link_opt  + " NOCON";
	    		exec_opt  = exec_opt  + " NOCON";
	    	}
	    	if (!option_menu_amode31.isSelected()){
	    		asml_opt  = asml_opt  + " NOAMODE31";
	    		asmlg_opt = asmlg_opt + " NOAMODE31";
	    		link_opt  = link_opt  + " NOAMODE31";
	    	}
	    	if (!option_menu_list.isSelected()){
	    		asm_opt   = asm_opt   + " NOLIST";
	    		asml_opt  = asml_opt  + " NOLIST";
	    		asmlg_opt = asmlg_opt + " NOLIST";
	    		link_opt  = link_opt  + " NOLIST";
	    		exec_opt  = exec_opt  + " NOLIST";
	    	}
	    	if (!option_menu_listcall.isSelected()){
	    		mac_opt   = mac_opt   + " NOLISTCALL";
	    		asm_opt   = asm_opt   + " NOLISTCALL";
	    		asml_opt  = asml_opt  + " NOLISTCALL";
	    		asmlg_opt = asmlg_opt + " NOLISTCALL";
	    	}
	    	if (option_menu_rmode31.isSelected()){
	    		asml_opt  = asml_opt  + " RMODE31";
	    		asmlg_opt = asmlg_opt + " RMODE31";
	    		link_opt  = link_opt  + " RMODE31";
	    	}
	    	if (!option_menu_stats.isSelected()){
	    		mac_opt   = mac_opt   + " NOSTATS";
	    		asm_opt   = asm_opt   + " NOSTATS";
	    		asml_opt  = asml_opt  + " NOSTATS";
	    		asmlg_opt = asmlg_opt + " NOSTATS";
	    		link_opt  = link_opt  + " NOSTATS";
	    		exec_opt  = exec_opt  + " NOSTATS";
	    	}
	    	if (option_menu_test.isSelected()){
	    		asmlg_opt = asmlg_opt + " TEST";
	    		exec_opt  = exec_opt  + " TEST";
	    	}
	    	if (option_menu_trace.isSelected()){
	    		mac_opt   = mac_opt   + " TRACE";
	    		asm_opt   = asm_opt   + " TRACE";
	    		asml_opt  = asml_opt  + " TRACE";
	    		link_opt  = link_opt  + " TRACE";
	    		asmlg_opt = asmlg_opt + " TRACE";
	    		exec_opt  = exec_opt  + " TRACE";
		    	if (option_menu_con.isSelected()){  // override trace setting NOCON
		    		mac_opt   = mac_opt   + " CON";
		    		asm_opt   = asm_opt   + " CON";
		    		asml_opt  = asml_opt  + " CON";
		    		asmlg_opt = asmlg_opt + " CON";
		    		link_opt  = link_opt  + " CON";
		    		exec_opt  = exec_opt  + " CON";
		    	}
	    	}
	    	return opt_flag;
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
		       	        String new_dir = get_file_name(cmd_parm1);              		
		       	        File new_dir_file = new File(new_dir);
				  	    if  (new_dir_file.isDirectory()){
				  	    	cur_dir = new_dir;
							cur_dir_file = new File(new_dir);
				  	        System.setProperty("user.dir",cur_dir);
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
		   	     * set cur_dir
		   	     * (Note dialog is kept for non gui mode to avoid
		   	     *  dispose causing gui shutdown on last window)
		   	     */
		   	   if (perm_select){
		         final JFileChooser select_dir_chooser = new
		         JFileChooser();
		         if  (select_dir_frame == null){
                     create_select_dir(select_dir_chooser);
		   	     } else {
		   	     	 if  (main_gui){
		                 main_frame.setVisible(false);
		   	     	 }
		             select_dir_frame.setVisible(true);           
		   	     }
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
	        	 if  (cur_dir_file != null){
	       	         select_dir_chooser.setCurrentDirectory(cur_dir_file);
	        	 }
	         	 select_dir_chooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
	        	 select_dir_chooser.addActionListener(new ActionListener() {
	                 public void actionPerformed(ActionEvent e) {
	                 File selected_file = null;
	                 String selected_dir_name = null;
	                 	String state =
	                 (String)e.getActionCommand();
	                 if (!state.equals(
	                    JFileChooser.APPROVE_SELECTION)){
	                	select_dir_frame.setVisible(false);
	                    if (main_gui){
	                       main_frame.setVisible(true);
	                    }
	                    return;
	                 }
	                 selected_file = select_dir_chooser.getSelectedFile();
	                 if (selected_file != null){
	                 	selected_dir_name = selected_file.getPath();
	                    if  (main_gui){
	                        z390_cmd_line.setText("CD " + "\"" + selected_dir_name + "\"");
	                        z390_cmd_line.postActionEvent();
	                    } else {
	                    	cd_command("CD " + "\"" + selected_dir_name + "\"");
	                    }
	                 } else {
	                 	log_error(37,"directory not found");
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
			       final JFileChooser select_file_chooser = new
		           JFileChooser();
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
   	        if  (cur_dir_file != null){
  	             select_file_chooser.setCurrentDirectory(cur_dir_file);
   	        }
   	        select_file_chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
   	        select_file_chooser.addActionListener(new ActionListener() {
                public void actionPerformed(ActionEvent e) {
                    File selected_file = null;
                    String selected_file_name = null;
            	     String state =
                       (String)e.getActionCommand();
                    if (!state.equals(
                       JFileChooser.APPROVE_SELECTION)){
            		    select_file_frame.setVisible(false);
                        if (main_gui){
                          main_frame.setVisible(true);
                        }
            		    return;
                    }
                    selected_file = select_file_chooser.getSelectedFile();
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
      	     				 if (!exec_cmd("\"" + edit_cmd + "\" \"" 
      	     						 + selected_file_name + "\"")){
   	     				         log_error(19,"start editor failed - " + edit_cmd);
   	     			         }
                       	 }  else if (select_cmd.equals("JOB")){
                       		z390_cmd_line.setText(
	                        			  "\"" + selected_file_name
	                        			+ "\" " + select_opt);
                       		z390_cmd_line.postActionEvent();
                       	 } else {
                       		z390_cmd_line.setText(
                       			"CMD " + "\"" + install_loc + File.separator 
                       			+ select_cmd + "\"" 
                       			+ " \"" + selected_file_name
                       			+ "\" " + select_opt);
                       		z390_cmd_line.postActionEvent();
                       	 }
                       } else {
               	         process_command(select_cmd + "\"" + selected_file_name + "\"");
                       }
                    } else {
            	        log_error(37,"file not found");
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
	    private void commands_command(String cmd_parm1, String cmd_parm2){
	    /*
	     * display alphabetical list of basic and extended commands
	     */ 
	    	put_log("\nz390 alphabetical command list");
	    	put_log(" ");
	    	put_log("ABOUT                    display summary information about z390 tool      ");
	    	put_log("AMODE31  ON/OFF          set amode 24/31 for link cmd");
	    	put_log("ASM      MLC file        assemble MLC source to OBJ object code file");
	    	put_log("ASML     MLC file        assemble and link MLC source to 390 load module file");
	    	put_log("ASMLG    MLC file        assemble, link, and execute 390 load module file");
	    	put_log("CD       directory path  change directory");
	    	put_log("CMD      command         set cmd mode and submit batch cmd");
	    	put_log("Copy                     copy selected text to clipboard (GUI right click)    ");
	    	put_log("COMMANDS                 alphabetical list of all commands                    ");
	    	put_log("CON      ON/OFF          set console output for file cmds");
	    	put_log("COPYLOG                  copy the entire log text to clipboard (GUI only)     ");
	    	put_log("Cut                      cut selected text (GUI right click)                  ");
	    	put_log("Edit     any file        edit source file in seprarate window");
	    	put_log("EXIT                     exit z390 after closing all files (also CTRL-BREAK");
	    	put_log("EXEC     390 file        execute 390 load module");
	    	put_log("FONT     points          change font size");
	    	put_log("GUIDE                    view PDF user guide in web browser          ");
	    	put_log("HELP                     display help information summary                     ");
	    	put_log("LINK     obj file        link obj file into 390 load module");
	    	put_log("LIST     ON/OFF          set PRN, LST, and/or LOG output for file cmds");
	    	put_log("LISTCALL ON/OFF          set trace calls for MAC file cmd");
	    	put_log("LOC      x y pixels      set upper left location of window");
	    	put_log("MAC      mlc file        expand mlc macro file to bal source file");
	    	put_log("NOTEPAD                  start Notepad for use with clipboard data (GUI only)  ");
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
	    		refresh_request = false;
	    	}
	    }
        private void update_main_view(){
        /*
         * update log and command line size 
         * following any of the following changes:
         *   1.  Change in window size
         *   2.  Change in log and command font size
         */	
          if (main_gui){
        		log_height = main_height - menu_height - command_height - status_height - applet_status_height;
        		log_width  = main_width - scrollbar_width - 4 * main_border;
                main_panel.setSize(main_width - 4 * main_border,main_height - menu_height - main_border);
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
    		command_columns = (log_width - label_width)/(z390_cmd_line.getPreferredSize().width/z390_cmd_line.getColumns()) - 1;
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
                	    		log_error(70,"previous command execution cancelled");
                	    	    cmd_exec_cancel();
                	    	} else {
                	    		log_error(71,"previous command execution ended with rc =" + rc);
                	    	}
                	    }
                    }
                	try {
                        cmd_exec_process = Runtime.getRuntime().exec(exec_cmd);
               		    cmd_exec_error_reader  = new InputStreamReader(cmd_exec_process.getErrorStream());
               		    cmd_exec_output_reader = new InputStreamReader(cmd_exec_process.getInputStream());
                   	    cmd_exec_input_writer  = cmd_exec_process.getOutputStream();
               		    cmd_exec_process_thread = new Thread(this);
            		    cmd_exec_error_reader_thread   = new Thread(this);
            		    cmd_exec_output_reader_thread  = new Thread(this);
            		    cmd_exec_output_msg = "";
            		    cmd_exec_error_msg = "";
            		    int last_io_count = io_count;
            		    cmd_exec_process_thread.start();
            		    cmd_exec_error_reader_thread.start();
            		    cmd_exec_output_reader_thread.start();
        		    	put_log("Starting Windows command process");
        		    	Thread.sleep(monitor_wait);
            		    int wait_count = 5;
            		    while (io_count == last_io_count && wait_count > 0){
            		    	put_log("Starting Windows command process");
            		    	Thread.sleep(monitor_wait);
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
            	if  (cmd_line == null){
            		cmd_line = "\r\n";
             	} else {
            		cmd_line = cmd_line + "\r\n";
            	}
            	try {
            		cmd_exec_input_writer.write(cmd_line.getBytes());
            		cmd_io_total++;
            		cmd_exec_input_writer.flush();
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
                	    	cmd_exec_cancel();
                	    } catch (Exception e){}
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
            	    } catch (Exception e){}
                    cmd_exec_process = null;     
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
            int next_int = cmd_exec_output_reader.read();
			while (next_int != -1){
				if  (next_int == ascii_lf){
					String msg = cmd_exec_output_msg;
					cmd_exec_output_msg = "";
				    put_log(msg);
				} else if (next_int != ascii_cr){
                    append_exec_output_msg(next_int);
				}
                next_int = cmd_exec_output_reader.read();
			}
		} catch (Exception ex) {
			log_error(67,"exec execution output error");
			cmd_exec_cancel();
		}
     }
     private void copy_cmd_error_to_log(){
        /*
         * copy cmd error to log a byte at a time
         * to handle cmds such as time with input
         * request and error msgs without CR/LF
         */	
        	try {
               int next_int = cmd_exec_error_reader.read();
   			while (next_int != -1){
   				   if  (next_int == ascii_lf){
   				   	   String msg = cmd_exec_error_msg;
   				   	   cmd_exec_error_msg = "";
   				   	   put_log(msg);
   				   } else if (next_int != ascii_cr){
                       append_exec_error_msg(next_int);
   				   }
                   next_int = cmd_exec_error_reader.read();
   			}
   		} catch (Exception ex) {
   			log_error(67,"exec execution output error");
   			cmd_exec_cancel();
   		}
        }
     private synchronized void append_exec_output_msg(int next_int){
     /*
      * add cmd char to log sync with put_log
      */
		cmd_exec_output_msg = cmd_exec_output_msg.concat(String.valueOf((char) next_int));
     }
     private synchronized void append_exec_error_msg(int next_int){
        /*
         * add cmd char to log sync with put_log
         */
   		cmd_exec_error_msg = cmd_exec_error_msg.concat(String.valueOf((char) next_int));
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
 		     put_log("\n********* starting batch cmd - " + select_cmd + " with options - " + select_opt + "*********");
	     	 if (perm_file_execute){
	     	   if (bat_file_name == null || bat_file_name.length() == 0){
	     	     	select_file(bat_cmd,bat_file_type, bat_opt);
	     	   } else {
	     		   bat_file_name = get_file_name(bat_file_name);
	     		   File temp_file = new File(bat_file_name);
	     		   if (temp_file.isFile()){
	     			 if (bat_cmd.equals("EDIT")){
	     				 if (!exec_cmd("\"" + edit_cmd + "\" \"" + bat_file_name + "\"")){
	     				     log_error(19,"start editor failed - " + edit_cmd);
	     			     }
	     			 } else {
	     			   bat_file_name = temp_file.getPath();
	     			   if (!cmd_mode){
	   				      cmd_command(null); //switch to cmd mode
	     			   }
	     			   if (select_cmd.equals("JOB")){
	     				   cmd_command("\"" + bat_file_name 
								   + "\" " + bat_opt);
	     			   } else {
	     				   cmd_command("\"" + install_loc 
							   + File.separator 
							   + bat_cmd + "\""
							   + " \"" + bat_file_name 
							   + "\" " + bat_opt);
	     			   }
	     			 }
	     		   } else {
	     			   log_error(18,"invalid batch command file name - " + bat_file_name);
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

