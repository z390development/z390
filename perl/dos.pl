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
# 2020/10/12 RPI 2011 Changes for z390 version 1.7.
#              1. Find z390 base directory same way as cmd.pl and z390.pl.
#              2. Perl scripts now in perl subdirectory.
#            Complete source replacement so only one line has RPI 2011.
###########################################################
use strict;
use warnings;

my $HOME = $ENV{'HOME'} || $ENV{'LOGDIR'} ||
		(getpwuid($<))[7] || die "You're homeless!\n";
my $dir; # Base directory for support files
my @dirs = ("$HOME/lib/z390", "/usr/local/lib/z390", "/usr/lib/z390");
unshift(@dirs, $ENV{Z390}) if $ENV{Z390};
foreach my $d (@dirs) {
  if (-f "$d/z390.jar") {                                 
    $dir = $d;
    last;
  }
}
die "Cannot find z390.jar file in any of @dirs!\n" unless $dir;
print "z390 dos command processor started - enter bat file command or exit\n";
chdir "$dir";
system("./perl/cmd.pl"); # RPI 2011
