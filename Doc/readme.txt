z390 V1.0.00 09/30/05 Portable Mainframe Assembler
Copyright 2005 Automated Software Tools Corporation

Minimum Requirements for z390 V1.0.00:
 * Microsoft Windows 2000 or XP (only tested on these so far)
 * Sun Microsystems Java Runtime J2SE 1.5.0 update 5+
 * Adobe Acrobat or later for viewing User Guide


Z390 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied       warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License as part of z390 install (see OSI_GPL_License_Agreement.rtf); if not, write to the Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA.

z390 is a portable mainframe assembler, linker, and emulator utility for Windows which includes the following tools:
* z390 - Graphical User Interface written in J2SE Swing Java
* mz390 -  macro processor to expand MLC source file to BAL source
* az390 - assembler to translate BAL source file into OBJ file
* lz390 - linker to translate OBJ files into 390 binary load module 
* ez390 - runtime emulator to exec 390 load modules

A macro based demo “Hello World” program demo.mlc is included which can be expanded, assembled, linked, and executed with the command “ASMLG DEMO” issued from GUI command line or via point and click using the z390 GUI file menu ASMLG selection.

For latest download and additional information about z390 and the open source project visit:

www.z390.net 

z390 Windows installation instructions:

1. Download latest z390.exe file from www.z390.org 
2. Run z390.exe to install it on Windows XP or 2000
3. Download and install Java 1.5.0 runtime update 5+ from: 
    http://java.sun.com/j2se/
4. Double click on the z390 desktop icon to start z390 GUI
    a. Enter the command “ASMLG DEMO” to run demo
    b. Scroll the output log to see output of each step
    c. Use file menu edit selection to view generated source files.
    d. To verify entire product enter “RT” to run regression tests
5. Click on help menu for links to additional information.

Don Higgins, President
Automated Software Tools Corporation
Email: don@higgins.net 
Voice:  Skype voip id:  dsh33782
