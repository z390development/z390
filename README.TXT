z390 v1.5.06rc2 Release Candidate 2 04/17/12
Copyright 2011 Automated Software Tools Corporation

To install this z390 full release candidate, do the following steps:

1.  Verify J2SE 6.0+ is installed and if not download and install latest release from Oracle.com.  This is the only pre-requisite and it is free.

2.  Uninstall all prior releases of z390 PTF's and full release. 

3.  Run the z390 full release candidate setup program for Windows or copy the z390 file directory and sub-directories to the install location for Linux or Apple.  Note that if you are using Windows 7 or Vista and want to leave User Access Control (UAC) Windows option on, you should select an install location other than in the default "C:\Program files (x86)" such as simple "C:" since z390 needs to have write access to the install directories. 

4.  Start z390 GUI and run command IVP to verify z390 installation.  The return code should be 0 or 4 if you do not install optional files such as regression tests..

5.  To run the assembler "Hello World" demo, enter the command:
  ASMLG demo\DEMO

6.  To run the zcobol "Hello World" demo, enter the command:
  ZC390CLG zcobol\demo\HELLO

To see RPI fixes included in this release candidate visit:

http://www.z390.org/z390_Support_Request_Log.htm

Visit www.z390.org for more information on latest z390 updates.
Visit www.zpar.org for more information on ZPAR utilities including source z390/zcobol tracing
Visit www.zcobol.org for more information on zcobol which is included in z390 releases and PTF's.

