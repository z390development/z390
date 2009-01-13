z390 v1.4.04a PTF 01/13/09
Copyright 2009 Automated Software Tools Corporation

To install this cumulative PTF, perform the following steps:

1.  Verify J2SE 6.0+ and z390 base release is correctly installed by starting GUI and enterring command IVP which should then display z390 current version. If you plan to use regression tests or MVS 3.8 macros, be sure these optional downloads are installed before applying PTF to pickup any updates to regression tests or MVS 3.8 macros.

2.  For Windows, download and run the InstallShield setup in the ptf zip file to update base release to current ptf level.

3.  For Linux unzip the ptf zip and copy the z390 sub-directory structure to the corresponding install directory overlaying or adding updated files since the initial install of full release. Rerun Linux command to reset z390 attributes to RWX.

4.  Start z390 GUI and run command IVP to verify z390 installation.  The return code should be 0 or 4 if you do not install optional files.

To see RPI fixes included in this PTF visit:

http://www.z390.org/z390_Download_Archive.htm

Visit www.z390.org for more information on latest z390 updates.

