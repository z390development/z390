/*
z390 - Mainframe assembler emulator and run-time engine
Copyright (C) 2021 z390 Assembler LLC

This file is part of z390.
z390 is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

z390 is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License along
with this program; if not, see <https://www.gnu.org/licenses/>.
*/

import java.awt.Button;
import java.awt.Dialog;
import java.awt.Frame;
import java.awt.Label;
import java.awt.Panel;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;

/**
  * Class MessageBox is used in the interactive window for z390
  */
public class MessageBox implements 
  ActionListener, WindowListener, KeyListener {
   /*****************************************************
    * Maintenance
    * ***************************************************
    * 2025-10-14 AFK      Add javadoc comments
    *****************************************************
    * Global variables                                   
    *****************************************************/
    /** variable      */ Frame frame = null;



/**
 * Dummy constructor - no initialization needed
 */
public void messageBox()
       {// dummy constructor - no initialization needed.
        }



/**
 * Popup message box with title and message
 * and exit when ok button clicked
 *
 * @param title title for the popup
 * @param message message content
 */
	public void messageBox(String title,String message) {
		frame  = new Frame();
        Dialog dialog = new Dialog(frame, true); // Modal
        dialog.addWindowListener(this);
        dialog.addKeyListener(this);
        frame.setTitle(title);
        Panel messagePanel = new Panel();
        Label messageLabel = new Label(message);
        messagePanel.add(messageLabel);
        dialog.add("Center", messagePanel);
        Button button = new Button("OK");
        button.addKeyListener(this);
        button.addActionListener(this);
        dialog.add("South", button);
        dialog.pack();
        Toolkit.getDefaultToolkit().beep();
        dialog.setLocation(100,100);
        dialog.toFront();
        dialog.setVisible(true);
  }



/**
 * Cancel the message
 */
  private void cancel_message(){
  	  frame.dispose();
  }



/**
 * Handle window activation
 *
 * @param e event
 */
  public void windowActivated(WindowEvent e) {
  }



/**
 * Handle window deactivation
 *
 * @param e event
 */
  public void windowDeactivated(WindowEvent e) {
  	cancel_message();
  }



/**
 * Handle window closed
 *
 * @param e event
 */
  public void windowClosed(WindowEvent e) {
  	cancel_message();
  }



/**
 * Handle window closing
 *
 * @param e event
 */
  public void windowClosing(WindowEvent e) {
  	cancel_message();
  }



/**
 * Handle window iconification
 *
 * @param e event
 */
  public void windowIconified(WindowEvent e) {
  }



/**
 * Handle window de-iconification
 *
 * @param e event
 */
  public void windowDeiconified(WindowEvent e) {
  }



/**
 * Handle window opened
 *
 * @param e event
 */
  public void windowOpened(WindowEvent e) {
  }



/**
 * Handle action performed event
 *
 * @param event action event data
 */
  public void actionPerformed(ActionEvent event){
  	cancel_message();
  }



/**
 * Handle key pressed event
 *
 * @param event keypress data
 */
  public void keyPressed(KeyEvent event){
  	cancel_message();
  }



/**
 * Handle key typed event
 *
 * @param event key typed data
 */
  public void keyTyped(KeyEvent event){
  	cancel_message();
  }



/**
 * Handle key released event
 *
 * @param event key released data
 */
  public void keyReleased(KeyEvent event){
  }
}

