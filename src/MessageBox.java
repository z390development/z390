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

public class MessageBox implements 
  ActionListener, WindowListener, KeyListener {
	Frame frame = null;
	public void messageBox(String title,String message) {
  /*
   * Popup message box with title and message
   * and exit when ok button clicked
	Copyright 2008 Automated Software Tools Corporation
	
    This file is part of z390.
	
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

   */
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
  private void cancel_message(){
  	  frame.dispose();
  }
  public void windowActivated(WindowEvent e) {
  }
  public void windowDeactivated(WindowEvent e) {
  	cancel_message();
  }
  public void windowClosed(WindowEvent e) {
  	cancel_message();
  }
  public void windowClosing(WindowEvent e) {
  	cancel_message();
  }
  public void windowIconified(WindowEvent e) {
  }
  public void windowDeiconified(WindowEvent e) {
  }
  public void windowOpened(WindowEvent e) {
  }
  public void actionPerformed(ActionEvent event){
  	cancel_message();
  }
  public void keyPressed(KeyEvent event){
  	cancel_message();
  }
  public void keyTyped(KeyEvent event){
  	cancel_message();
  }
  public void keyReleased(KeyEvent event){
  }
}

