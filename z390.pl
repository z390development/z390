#!/usr/bin/perl --
# z390 IBM assembler control program
# Usage: z390 [options] file [assembler options]
#
# Options:
# -a assemble
# -l load
# -g go (run)
#
# If no options are given, the default of -alg is used.
# Just give the base name of the file to process.
# Assembler options are things like sysmac(...) syscpy(...) etc.
#
# The support files (z390.jar etc.) are searched for in the following places:
# (1) The directory given in the Z390 environment variable
# (2) $HOME/lib/z390
# (3) /usr/local/lib/z390
# (4) /usr/lib/z390
###########################################################
# Author Martin Ward http://www.cse.dmu.ac.uk/~mward/
###########################################################
# Maintenace Log of changes for z390 by Don Higgins www.z390.org
# Copyright 2007 Automated Software Tools Corporation
# z390 is licensed under GPL General Public License
###########################################################
# 01/28/07 RPI 542 change path to perl for Ubuntu on 1st line
#          (Note Linux requires first line path to end with
#           LF line feed only so do not edit perl scripts
#           with notepad or any Windows editor that adds
#           CR,LF carriage return and line feed.  This only
#           applies to first line of perl scripts as BAT
#           files edited on Windows work fine.)
# 02/09/07 RPI 548 add -- on first line to allow Windows editing
###########################################################

use strict;
use warnings;
use Getopt::Std;

sub run($$$);

our($opt_a, $opt_l, $opt_g);
getopts('alg');

(my $myname = $0) =~ s|(.*/)*||;        # strip path component from name
my $Usage = "Usage: $myname [-alg] ... \n";

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

my $jar = "$dir/z390.jar";  # Location of jar file
my $sysmac = "$dir/mac+.";  # Default macro search path
my $syscpy = "$dir/mac+.";  # Default copybook search path

if (!@ARGV) {
  # Run the GUI:
  chdir $dir or die "Can't chdir to $dir: $!\n"; # RPI 542
  exec "java -jar z390.jar";                     # RPI 542
  exit(1);
}

my $options = "'sysmac($sysmac)' 'syscpy($syscpy)'"; # default options
my %opt = (); # Command line options

my $base = shift(@ARGV);
$base =~ s/\.\w+$//;

$options = " " . join(" ", map { "'$_'" } @ARGV) if @ARGV;

if (!$opt_a && !$opt_l && !$opt_g) {
  # Default is -alg:
  $opt_a = 1;
  $opt_l = 1;
  $opt_g = 1;
}

unlink "$base.BAL" if $opt_a;
unlink "$base.PRN" if $opt_a;
unlink "$base.OBJ" if $opt_a;
unlink "$base.ERR" if $opt_a;
unlink "$base.LST" if $opt_l;
unlink "$base.390" if $opt_l;
unlink "$base.LOG" if $opt_g;

if ($opt_a) {
  # Creates OBJ, PRN, BAL(?) and ERR:
  run("mz390", $base, "BAL or ERR");
}

if ($opt_l) {
  # Creates LST and 390:
  run("lz390", $base, "LST");
}

if ($opt_g) {
  # Creates LOG:
  run("ez390", $base, "LOG");
}

exit(0);

sub run($$$) {
  my ($command, $file, $err) = @_;
  print "\nRunning $command $file...\n\n";
  my $res = system qq[java -classpath $jar -Xrs $command $file $options];
  return if $res == 0;
  if ($? == -1) {
    print "$command $file failed to execute: $!\n";
  } elsif ($? & 127) {
    printf "$command $file child died with signal %d, %s coredump\n",
        ($? & 127),  ($? & 128) ? 'with' : 'without';
  } else {
    printf "child exited with value %d\n", $? >> 8;
  }
  print "See errors in $file.$err\n";
  exit(1);
}

