z390 V1.4.01 Portable Mainframe Assembler
Copyright 2008 Automated Software Tools Corporation

Minimum Requirements for z390:
 * Microsoft Windows Vista, Windows XP, or Ubuntu Linux 6.06 LTS (others not tested)
 * Sun Microsystems Java Runtime J2SE 6.0+ 
 * Minimum of 512 MB memory (default Java user memory allocation set to 100 MB)
 * Adobe Acrobat or later for viewing User Guide and other documentation


z390 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License as part of z390 install (see OSI_GPL_License_Agreement.rtf); if not, write to the Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA.

z390 is a portable mainframe assembler, linker, and emulator utility for Windows, Linux, and other platforms supporting J2SE.  z390 includes the following tools:
* z390 - Graphical User Interface written in J2SE Swing Java
* mz390 - macro processor to expand MLC source file to BAL source code
o Macro pseudo code generator for 3 times faster macro execution
o Macro pseudo code trace facility to see all macro variables in all expressions
o CICS, PROLOG, and EPILOG options support EXEC CICS and EXEC SQL compatible statements which are reformatted into standard EXEC macro format for expansion.
o Macro libraries with commonly used MVS and VSE compatible macros
o Structured programming macros including IF, ELSE, ENDIF, DO UNTIL, etc.
* az390 - assembler to translate BAL source code into OBJ file and generate PRN listing
* lz390 - linker to translate OBJ files into 390 binary load module
* ez390 - runtime emulator to exec 390 load modules running in 24/31 bit mode
o All problem state instructions including 128 bit HFP, BFP, and DFP floating point and 64 bit register instructions for IBM z9 and z10 mainframe computers.  See the following instruction regression tests:
* Rt\test\TESTINS1 – test the assembly of all z9, z10, and ASSIST instructions totaling over 1000.
* Rt\test\TESTINS2 – test execution of basic problem state instructions
* Rt\test\TESTFP1 – test HFP and BFP 32, 64, and 128 bit floating point
* Rt\test\TESTDFP1 – test DFP 32, 64, and 128 bit decimal floating point
* Rt\test\TESTINS3 – test over 220 new z10 problem state instructions 
o ASSIST extended instructions to simplify I/O for use by students in mainframe assembler computer science classes.
o TEST option for interactive debugging with dump, modify, and trace options
o TRACE option for instruction trace with operand register and memory data
o SVC support for the z390 provided MVS compatible macros including QSAM, VSAM, GETMAIN, FREEMAIN, TIME, etc. plus extensions such as TCPIO for SOA and CICS TCP/IP network messaging.
* z390 EXEC CICS compatible transaction manager V4 contributed by Melvyn Maltz:
o Multiple local and remote CICS TN3270 compatible clients connected to single CICS server over TCP/IP network.
o EXEC CICS commands include SEND, RECEIVE, RETURN, LOAD, LINK, XCTL, HANDLE, PUSH, POP, etc.
o VSAM ESDS, RRDS, and KSDS file browse.
o See the cics directory for additional documentation, demos, and startup commands.
* Z390 Utilities written in z390 assembler with source included:
o REPRO – load or unload VSAM data set from/to sequential file.
o SUPERZAP – verify, replace, and dump any Windows or Linux file up to 2 GB.
o UNREF – scan PRN assembly listing and list unreferenced symbols contributed by Melvyn Maltz.

A WTO macro based demo “Hello World” program demo.mlc is included which can be expanded, assembled, linked, and executed with the command “ASMLG DEMO\DEMO” issued from GUI command line or via point and click using the z390 GUI file menu ASMLG selection.

Structured programming macros IF, ELSEIF, ELSE, ENDIF, DO, and ENDDO are included along with demo programs demo\DEMOSTR1.MLC and util\superzap\SUPERZAP.MLC.

A Service Oriented Architecture (SOA) client server application generation service including SOA directory with macro library and demo application which uses TCP/IP sockets messaging to enable clients and servers with multiple connections to run on same or different processors on a TCP/IP network including networks connected via VPN links over the Internet.  Also see the CICS transaction manager for more client/server examples.

A TPUT and TGET macro based demo TN3270 application to update and display name, address, and numeric zip fields on screen can be assembled and executed with the command “ASMLG DEMO\DEMOGUI6 GUAM”.  Use PF1 for help screen and PF3 for exit.  The arrow, backspace, and tab keys can be used to control blinking cursor.  An alarm and status line error is issued if attempt is made to modify protected field or enter alpha data in numeric field.

Z390 v1.4.00 includes full support for VSAM ESDS and RRDS plus browse support for KSDS files.  Z390 VSAM supports fixed or variable length records up to 2 GB.  Support includes ACB, RPL, GENCB, GET, POINT, PUT, MODCB, SHOWCB, TESTCB, type macros plus DEFINE macro to define loadable VSAM catalogs.  Additional support for KSDS inserts and alternate indexes plus LDS linear support is planned.

For latest Windows and Linux downloads and additional information about z390 and the open source project visit:

www.z390.org 

z390 Windows Vista and XP installation instructions:

1. Uninstall any old z390 releases and delete residual files left in the install directory
    Which could be read-only and prevent proper install of new release.  
2. Download latest z390 InstallShield setup.exe file from www.z390.org 
3. Run z390 setup.exe to install it on Windows Vista or XP system
4. Remove old versions of J2SE runtime such as 1.4.2 etc.
5. Download and install latest J2SE 6.0+ runtime from: 
    http://java.sun.com/j2se/
6. Double click on the z390 desktop icon to start z390 GUI
    a. Enter the command IVP to run installation verification program 
        which should display correct OS, J2SE, and z390 versions.  Verify the z390 install 
        version and the J2SE version.  The RT regression test version and MVS
        macro versions will only match after you download and install these options.
        You can ignore these warnings if you don’t plan to use these optional files.
    b. Enter the command “ASMLG demo\DEMO” to assembler and run demo\DEMO.MLC
    c. Scroll the output log to see output of each step
    d. Use file menu edit selection to view generated ASCII source files:
        1)  DEMO.MLC – source macro assembler demo for WTO ‘hello world’
        2)  DEMO.BAL - expanded basic assembler source code
        3)  DEMO.PRN – assembly listing
        4)  DEMO.OBJ – relocatable object code in ASCII hex format
        5)  DEMO.LST – linker listing
        6)  DEMO.LOG – execution log including WTO display message
    e. Optional downloads from www.z390.org for use with z390 install include the following:
        1)   PTF fix and enhancement zip files with InstallShield install for Windows.  These
               Optional hot fixes and enhancements are published between full releases for users
               that need specific fixes or enhancements.  See the README.TXT file in zip for help.
        2)   Regression tests for all z390 components - these tests have been successfully run
               on Windows Vista, Windows XP, and Ubuntu Linux 6.06 LTS using J2RE 1.6.0_04.           
        3)   Public domain MVS 3.8 macro library with associated WTO demo and test programs.
               Note the WTO demo using the MVS 3.8 macros works because the SVC interface is the
               same for z390.  Most other SVC interfaces are different.  These macros are made
               available for reference only as z390 automatically installs its only version of MVS
               compatible macros with svc’s for its own portable runtime.  The targeted interface
               for z390 MVS compatibility is at the macro source level only.  The z390 macro
               assembler is targeted to support all mainframe compatible macros including all 
               flavors of MVS, VSE, and z/OS.

7. Click on help menu for links to additional information.

IBM, CICS, VSAM, z9, z10, and z/OS are registered trademarks of International Business Machine Corporation.

Don Higgins, President
Automated Software Tools Corporation
Email: don@higgins.net
