#!/usr/bin/perl --
# dos.pl perl bat file and dos command processor for use with z390 on Linux
# 
# Installation:
#   1.  Install z390 in /usr/local/lib/z390 directory and assign rwx rights
#   2.  Define link symbol "dos" with following command:
#       sudo  ln -s /usr/local/lib/z390/dos.pl /usr/local/bin/dos
# Usage:
#   1.  Start Linux terminal window from "Applications", ,"Accessories", menu
#   2.  Enter command: dos   (this will  issue cd to z390 directory and prompt
#       for any z390 bat file or supported dos command  such as:
#         ASMLG demo\DEMO  (assemble,  link,  and exec hello world  demo)
#         RT               (run z390 regression tests after installing RT and MVS zips)
#         soa\demo\DEMOSOA (gen SOA client server demo application
#         soa\demo\DEMORUNS (run SOA server in this terminal window)
#         soa\demo\DEMORUNC (run SOA client applicaiton in another terminal window)
#         soa\demo\DEMOSTOP (stop the SOA demo server from anywhere on  network)
#         z390              (start z390 GUI interface from this window)
#         exit              (exit z390 dos command processor)
###########################################################
# Author Don Higgins www.z390.org  don@higgins.net
###########################################################
# Maintenace Log 
# Copyright 2007 Automated Software Tools Corporation
# z390 is licensed under GPL General Public License
###########################################################
# 02/08/07 RPI 532 add dos command processor support
#          (Note Linux requires first line path to end with
#           LF line feed only so do not edit perl scripts
#           with notepad or any Windows editor that adds
#           CR,LF carriage return and line feed.  This only
#           applies to first line of perl scripts as BAT
#           files edited on Windows work fine.)
# 02/09/07 RPI 548 add -- to first line to allow Windows editing
###########################################################
use strict;
use warnings;
use Getopt::Std;
sub run();
if (-f "/usr/local/lib/z390/z390.jar") {
   print "z390 dos command processor started - enter bat file command or exit\n";
} else {
   die "z390 dos error - z390 not installed in /usr/local/lib/z390\n";
}
chdir '/usr/local/lib/z390';
system("cmd.pl");

