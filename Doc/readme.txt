z390 V1.0.12 Portable Mainframe Assembler
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
* az390 - assembler to translate BAL source file into OBJ file
* lz390 - linker to translate OBJ files into 390 binary load module 
* ez390 - runtime emulator to exec 390 load modules
* GUAM GUI Access Method dialog option for MCS console, TN3270, and graphical user interfaces.
* TEST option for interactive debugging
* TRACE option for instruction trace with operand register and memory data 

A WTO macro based demo “Hello World” program demo.mlc is included which can be expanded, assembled, linked, and executed with the command “ASMLG DEMO\DEMO” issued from GUI command line or via point and click using the z390 GUI file menu ASMLG selection.

A TPUT and TGET macro based demo to display TN3270 screen and read text from field and display it in another field is included which can be assembled and executed with the command “ASMLG DEMO\DEMOGUI4 GUAM”.

For latest download and additional information about z390 and the open source project visit:

www.z390.org 

z390 Windows installation instructions:

1. Download latest z390_setup.exe file from www.z390.org 
2. Run z390_setup.exe to install it on Windows XP or 2000
3. Download and install Java 1.5.0 runtime update 5+ from: 
    http://java.sun.com/j2se/
4. Double click on the z390 desktop icon to start z390 GUI
    a. Enter the command “ASMLG DEMO” to run demo
    b. Scroll the output log to see output of each step
    c. Use file menu edit selection to view generated ascii source files:
        1)  DEMO.MLC – source macro assembler demo for WTO ‘hello world’
        2)  DEMO.BAL - expanded basic assembler source code
        3)  DEMO.PRN – assembly listing
        4)  DEMO.OBJ – relocatable object code in ASCII hex format
        5) DEMO.LST – linker listing
        6) DEMO.LOG – execution log including WTO display message
    d. To verify entire product enter “RT” to run regression tests
        1)  Each regression test assembles, links, and executes test program
        2)  Generated files are then compared using Windows FC utility
        3)  Last step displays size of comparison files descending by size

5. Click on help menu for links to additional information.

Don Higgins, President
Automated Software Tools Corporation
Email: don@higgins.net 
Voice:  Skype voip id:  dsh33782
