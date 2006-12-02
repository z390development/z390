import java.awt.Color;
import java.awt.Font;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.awt.font.TextLayout;

import javax.swing.JFrame;
import javax.swing.JScrollPane;

public class test_screen{
	
	public static void main(String argv[]) {
    	/*
    	 * start a test window for gz390_screen
    	 */
        gz390_screen tn_scn = new gz390_screen();
	    int max_rows = 24;
	    int max_cols = 80;
	    int font_size = 12;
	    Color bg_color = Color.BLACK;
	    Color text_color = Color.YELLOW;
	    Font font_ascii = new Font("Monospaced",Font.BOLD,font_size);
	    tn_scn.set_screen(max_rows,max_cols,font_ascii,bg_color,text_color);       
	    JFrame main_frame = new JFrame("Test z390 gz390_screen graphic2d panel class");
        main_frame.addWindowListener(new WindowAdapter() {
            public void windowClosing(WindowEvent e) {System.exit(0);}
        });
        JScrollPane main_view = new JScrollPane(tn_scn);
        main_view.setPreferredSize(tn_scn.scn_size);
        main_view.createVerticalScrollBar();
        main_view.createHorizontalScrollBar();
	    main_frame.getContentPane().add(main_view);
        main_frame.setSize(tn_scn.scn_size);
        main_frame.setVisible(true);
        tn_scn.scn_grid.setColor(Color.yellow);
   	    tn_scn.scn_layout   = new TextLayout("Winner",font_ascii, tn_scn.scn_context);
        tn_scn.scn_grid.setColor(Color.YELLOW);
   	    tn_scn.scn_layout.draw(tn_scn.scn_grid,0,tn_scn.scn_char_height);
        tn_scn.scn_repaint = true;
        tn_scn.scn_layout.draw(tn_scn.scn_grid,tn_scn.scn_char_width,2*tn_scn.scn_char_height);
        tn_scn.scn_repaint = true;
    }
}

