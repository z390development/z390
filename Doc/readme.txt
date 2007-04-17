z390 V1.3.03 Portable Mainframe Assembler
Copyright 2007 Automated Software Tools Corporation

Minimum Requirements for z390:
 * Microsoft Windows 2000, Windows XP, Linux
 * Sun Microsystems Java Runtime J2SE 1.5.0+
 * Minimum of 512 MB memory (default Java user memory allocation set to 100 MB)
 * Adobe Acrobat or later for viewing User Guide


z390 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License as part of z390 install (see OSI_GPL_License_Agreement.rtf); if not, write to the Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA.

z390 is a portable mainframe assembler, linker, and emulator utility for Windows, Linux, and other platforms supporting J2SE.  z390 includes the following tools:
* z390 - Graphical User Interface written in J2SE Swing Java
* mz390 - macro processor to expand MLC source file to BAL source code
o Macro pseudo code generator for 3 times faster macro execution
o Macro pseudo code trace facility to see all macro variables in all expressions
o CICS, PROLOG, and EPILOG options support EXEC CICS and EXEC SQL compatible statements which are reformatted into standard EXEC macro format for expansion.
o Macro libraries with commonly used MVS and VSE compatible macros
* az390 - assembler to translate BAL source code into OBJ file and generate PRN listing
* lz390 - linker to translate OBJ files into 390 binary load module
* ez390 - runtime emulator to exec 390 load modules running in 24/31 bit mode
o All problem state instructions including HFP, BFP, and new DFP floating point
o TEST option for interactive debugging with dump, modify, and trace options
o TRACE option for instruction trace with operand register and memory data
* Z390 Utilities written in z390 assembler with source included:
o SUPERZAP – verify, replace, and dump any Windows or Linux file up to 2 GB.
o UNREF – scan PRN assembly listing and list unreferenced symbols.

A WTO macro based demo “Hello World” program demo.mlc is included which can be expanded, assembled, linked, and executed with the command “ASMLG DEMO\DEMO” issued from GUI command line or via point and click using the z390 GUI file menu ASMLG selection.

Structured programming macros IF, ELSEIF, ELSE, ENDIF, DO, and ENDDO are included along with demo program DEMO\DEMOSTR1.

A Service Oriented Architecture (SOA) client server application generation service including SOA directory with macro library and demo application which uses TCP/IP sockets messaging to enable clients and servers with multiple connections to run on same or different processors on a TCP/IP network including networks connected via VPN links over the Internet.

A TPUT and TGET macro based demo TN3270 application to update and display name, address, and numeric zip fields on screen can be assembled and executed with the command “ASMLG DEMO\DEMOGUI6 GUAM”.  Use PF1 for help screen and PF3 for exit.  The arrow, backspace, and tab keys can be used to control blinking cursor.  An alarm and status line error is issued if attempt is made to modify protected field or enter alpha data in numeric field.

An EXEC CICS compatible assembler application to update and display name, address, and numeric zip fields on screen can be assembled, linked, and executed.  Use the command RTCICS.BAT to assemble and link all the required EXEC CICS programs.  Use the command CICS PARM(GUI6) to start the z390 EXEC CICS assembler command processor and execute the transaction program TESTGUI6 as the first transaction.  The first GUI window to come up will be the TESTGUI6 application screen.  After exiting the application via PF3, the window will switch from screen view to MCS console view and prompt for the next EXEC CICS transaction or PF3 to exit CICS.

For latest Windows and Linux downloads and additional information about z390 and the open source project visit:

www.z390.org 

z390 Windows 2000 and XP installation instructions:

1. Download latest z390 InstallShield setup.exe file from www.z390.org 
2. Run z390 setup.exe to install it on Windows XP or 2000 system
3. Download and install Java 1.5.0 runtime update 10 from: 
    http://java.sun.com/j2se/
4. Remove old versions of J2SE runtime such as 1.4.2 etc.
5. Double click on the z390 desktop icon to start z390 GUI
    a. Enter the command “ASMLG demo\DEMO” to run demo
    b. Scroll the output log to see output of each step
    c. Use file menu edit selection to view generated ASCII source files:
        1)  DEMO.MLC – source macro assembler demo for WTO ‘hello world’
        2)  DEMO.BAL - expanded basic assembler source code
        3)  DEMO.PRN – assembly listing
        4)  DEMO.OBJ – relocatable object code in ASCII hex format
        5)  DEMO.LST – linker listing
        6)  DEMO.LOG – execution log including WTO display message
    d. Optional downloads from www.z390.org for use with z390 install include the following:
        1)   Regression tests for all z390 components - these tests have been successfully run
               on both Windows XP and Ubuntu Linux 6.06 LTS using J2RE 1.5.0_10.           
        2)   Public domain MVS 3.8J macro library with associated demo and test programs

6. Click on help menu for links to additional information.

IBM and CICS are registered trademarks of International Business Machines Corporation.


Don Higgins, President
Automated Software Tools Corporation
Email: don@higgins.net 
Voice:  Skype voip id:  dsh33782
