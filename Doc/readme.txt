z390 V1.1.01 Portable Mainframe Assembler
Copyright 2006 Automated Software Tools Corporation

Minimum Requirements for z390:
 * Microsoft Windows 2000 or XP
 * Sun Microsystems Java Runtime J2SE 1.5.0+
 * Adobe Acrobat or later for viewing User Guide


Z390 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License as part of z390 install (see OSI_GPL_License_Agreement.rtf); if not, write to the Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA.

z390 is a portable mainframe assembler, linker, and emulator utility for Windows which includes the following tools:
* z390 - Graphical User Interface written in J2SE Swing Java
* mz390 - macro processor to expand MLC source file to BAL source
     *     CICS, PROLOG, and EPILOG options support EXEC CICS and EXEC SQL compatable 
            statements which are reformatted into standard EXEC macro format for expansion.
* az390 - assembler to translate BAL source file into OBJ file
* lz390 - linker to translate OBJ files into 390 binary load module 
* ez390 - runtime emulator to exec 390 load modules running in 24 or 31 bit mode
* GUAM GUI Access Method dialog option for MCS console, TN3270, and graphical user interfaces which are used by WTO, WTOR, TGET, and TPUT to support EXEC CICS.
* TEST option for interactive debugging with dump, modify, and trace options
* TRACE option for instruction trace with operand register and memory data 

A WTO macro based demo “Hello World” program demo.mlc is included which can be expanded, assembled, linked, and executed with the command “ASMLG DEMO\DEMO” issued from GUI command line or via point and click using the z390 GUI file menu ASMLG selection.

Structured programming macros IF, ELSEIF, ELSE, ENDIF, DO, and ENDDO are included along with demo program DEMO\DEMOSTR1.

A TPUT and TGET macro based demo TN3270 application to update and display name, address, and numeric zip fields on screen can be assembled and executed with the command “ASMLG DEMO\DEMOGUI6 GUAM”.  Use PF1 for help screen and PF3 for exit.  The arrow, backspace, and tab keys can be used to control blinking cursor.  An alarm and status line error is issued if attempt is made to modify protected field or enter alpha data in numeric field.

An EXEC CICS compatible assembler application to update and display name, address, and numeric zip fields on screen can be assembled, linked, and executed.  Use the command RTCICS.BAT to assemble and link all the required EXEC CICS programs.  Use the command CICS PARM(GUI6) to start the z390 EXEC CICS assembler command processor and execute the transaction program TESTGUI6 as the first transaction.  The first GUI window to come up will be the TESTGUI6 application screen.  After exiting the application via PF3, the window will switch from screen view to MCS console view and prompt for the next EXEC CICS transaction or PF3 to exit CICS.

For latest download and additional information about z390 and the open source project visit:

www.z390.org 

z390 Windows installation instructions:

1. Download latest z390_setup.exe file from www.z390.org 
2. Run z390_setup.exe to install it on Windows XP or 2000
3. Download and install Java 1.5.0 runtime update 5+ from: 
    http://java.sun.com/j2se/  (update 7 is now recommended)
4. Double click on the z390 desktop icon to start z390 GUI
    a. Enter the command “ASMLG DEMO” to run demo
    b. Scroll the output log to see output of each step
    c. Use file menu edit selection to view generated ASCII source files:
        1)  DEMO.MLC – source macro assembler demo for WTO ‘hello world’
        2)  DEMO.BAL - expanded basic assembler source code
        3)  DEMO.PRN – assembly listing
        4)  DEMO.OBJ – relocatable object code in ASCII hex format
        5)  DEMO.LST – linker listing
        6)  DEMO.LOG – execution log including WTO display message
    d. To verify entire product enter “RT” to run regression tests
        1)  Each regression test assembles, links, and executes test program
        2)  Generated files are then compared using Windows FC utility
        3)  Last step displays size of comparison files descending by size

5. Click on help menu for links to additional information.

IBM and CICS are registered trademarks of International Business Machines Corporation.


Don Higgins, President
Automated Software Tools Corporation
Email: don@higgins.net 
Voice:  Skype voip id:  dsh33782
