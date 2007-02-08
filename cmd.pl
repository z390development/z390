#!/usr/bin/perl
# cmd.pl perl script for emulation of cmd.exe
# for handling DOS BAT file command
#
# The support files (z390.jar etc.) are searched for in the following places:
# (1) The directory given in the Z390 environment variable
# (2) $HOME/lib/z390
# (3) /usr/local/lib/z390
# (4) /usr/lib/z390
##################################################################
# Author Martin Ward http://www.cse.dmu.ac.uk/~mward/
##################################################################
# Maintenace Log of changes for z390 by Don Higgins www.z390.org
# Copyright 2007 Automated Software Tools Corporation
##################################################################
# z390 is licensed under GPL General Public License
# 02/06/07 DSH RPI 532 -  changes to support z390 on Ubuntu 6.06 LTS Linux
#   1) Set first line to  point  to Unbuntu perl install
#   2) Support erase command with /q for quiet mode
#   3) Support following upper or lower case commands within BAT files:
#      CALL, COPY, 
#      DIFF dir1 dir2 file3 (compare files in dir1/dir2 and write diff report to file3)
#      DIR, ERASE, EXIT,
#      gedit file1,
#      GOTO label,
#      IF ERRORLEVEL 1 GOTO label,
#      IF ERRORLEVEL 1 PAUSE text,
#      IF EXIST file GOTO label)
#      PAUSE, REM, SET
#      (Any command not recognized such as java will be executed as child program)
#   4) Add dos.pl perl script and alias dos command to support bat file commands
##################################################################
# Notes:
#   1. Paths must be correct case with \ separators
#   2. BAT files must be uppercase and the default added extension  is .BAT
##################################################################
use strict;
use warnings;

sub batch_file($$);
sub set_meta($);
sub split_args($);

$| = 1;
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
my $bat = "$dir";           # Directory to search for batch files.
my $errorlevel = 0;         # Save return code of last command

die "Arguments <" . join(" ", @ARGV) . "> not yet implemented!\n" if @ARGV;

while (<STDIN>) {
  s/\cM//g;
  chomp;

  print "$_\n";

  if ((/^cd (.*)$/) 
   || (/^CD (.*)$/)) {  
    chdir $1 or die "Can't chdir to $1: $!\n";
  } elsif ((/^exit$/) || (/^EXIT$/)) { 
    last;
  } elsif (/^\s*$/) {
    # blank line
  } elsif (s/(^\S+)\s*//) {
    my $cmd = $1;
    # Assume that the command is a batch file
    batch_file($cmd, $_);
  } else {
    die "Unexpected input: $_\n";
  }
}


sub batch_file($$) {
  my ($cmd, $args) = @_;
  $cmd =~ s/\\/\//g;              # replace \ with / 
  $args =~ s/\\/\//g;             # replace \ with / (could change literals vs paths)
  $cmd =~ s/\.bat$//i;
  $cmd =~ s/(\w+)$/\U$1\E/i; # batch file names are all upper case.
  my @args = split_args($args);
  # Add a dummy argument so that the numbering starts from 1:
  unshift(@args, "");
  # Pad out to 9 arguments:
  push(@args, ("") x 9);
  my $file = "";
  if ($cmd =~ /^\//) {
    # $cmd is a full path without suffix
    $file = "$cmd.BAT";
  } else {
    # $cmd is relative path without suffix
    $file = "$bat/$cmd.BAT"; 
  }
# load bat file into @lines array
  my $fh;
  open($fh, $file) or die "Can't read $file: $!\n";
  my @lines = <$fh>;
  close($fh);
# set line number for each :label defined forcinglower case
  my %labline = (); # Map each label to the line it is found on
  my $i = 0;
  foreach (@lines) {
    s/\cM//g;
    chomp;
    s/^\s+//;
    if (/^:(\S+)/) {
      $labline{lc $1} = $i; 
    }
    $i++;
  }

  # Execute the batch file:
  $i = 0; # Current line
  while (1) {
# exec next line within bat file line array
    my $line = $lines[$i];
    $line =~ s/\%\~dps0/$bat\//g;       # replace bat %dps0 with $bat path                         
    $line =~ s/\%([1-9])/$args[$1]/g;   # substitute current bat parms 1-9
    $line =~ s/\s+$//;                  # Trim trailing spaces.
    $line =~ s/\\/\//g;              # replace \ with /
    print "$line\n";

# exec dos command or assume it is native command
    if ($line =~ /^\s*$/) {
      # Blank line

# :label
    } elsif ($line =~ /^:\S/i) {
      # Label line

# call
    } elsif (($line =~ /^call\s+(\S+)(.*)$/) 
          || ($line =~ /^CALL\s+(\S+)(.*)$/)) { 
      # Call another batch file:
      batch_file($1, $2);

# copy one or more files
    } elsif (($line =~ /^copy\s+(\S+)\s+(\S+)$/) 
          || ($line =~ /^COPY\s+(\S+)\s+(\S+)$/)){  
         $errorlevel = system("cp $1 $2");
         if ($errorlevel){
            print "Copy error: $errorlevel\n";
         }

# diff dir1 dir2 file3
    } elsif (($line =~ /^diff\s+(\S+)\s+(\S+)\s+(\S+)$/) 
          || ($line =~ /^DIFF\s+(\S+)\s+(\S+)\s+(\S+)$/)) {
      open IN, "diff $1 $2 |" or die "diff generation failed";
      open OUT,"> $3" or die "diff open output $3 failed";
      print OUT <IN>;
      close IN;
      close OUT; 

# dir
    } elsif (($line =~ /^dir\s+(.*)$/) 
          || ($line =~ /^DIR\s+(.*)$/)) { 
      $errorlevel = system("ls -l $1");
      if ($errorlevel) {
          print "dir errorlevel: $errorlevel";
      }
# erase file
    } elsif (($line =~ /^erase\s+(\S+)$/)
          || ($line =~ /^ERASE\s+(\S+)$/)) { 
      $errorlevel = system("rm $1");
      if ($errorlevel) {
          print "erase errorlevel: $errorlevel";
      };
# erase files /q    
    } elsif (($line =~ /^erase\s+(\S+)\s+(\S+)$/) 
          || ($line =~ /^ERASE\s+(\S+)\s+(\S+)$/)) {
      $errorlevel = system("rm $1");
      if ($errorlevel) {
          print "erase errorlevel: $errorlevel";
      };
# exit
    } elsif (($line =~ /^exit\s$/) 
          || ($line =~ /^EXIT\s$/)) { 
      die "EXIT\n";
# gedit file1
    } elsif (($line =~ /gedit\s+(.*)$/) 
          || ($line =~ /^GEDIT\s+(.*)$/)) { 
      $errorlevel = system("gedit $1");
      if ($errorlevel) {
          print "gedit errorlevel: $errorlevel";
      }
# goto
    } elsif (($line =~ /^goto\s+(\S+)$/) 
          || ($line =~ /^GOTO\s+(\S+)$/)) { 
      $i = $labline{lc $1}; 
      die "Label $1 not found!\n" unless defined($i);
      next;
# if exist file goto label
    } elsif (($line =~ /^if\s+exist\s+(\S+)+\s+goto\s+(\S+)$/i)
          || ($line =~ /^IF\s+EXIST\s+(\S+)+\s+GOTO\s+(\S+)$/i)) {
      if (-f "$1") {
          $i = $labline{lc $2}; 
	  die "Label $1 not found!\n" unless defined($i);
	  next;
      }
# if errorlevel 1 goto label
    } elsif (($line =~ /^if\s+errorlevel\s+\d+\s+goto\s+(\S+)$/i)
          || ($line =~ /^IF\s+ERRORLEVEL\s+\d+\s+GOTO\s+(\S+)$/i)) {
      if ($errorlevel > 0) {
	$i = $labline{lc $1}; 
	die "Label $1 not found!\n" unless defined($i);
	next;
      }
# if errorlevel 1 pause text
    } elsif (($line =~ /^if\s+errorlevel\s+\d+\s+pause\s+(.*)$/)
          || ($line =~ /^IF\s+ERRORLEVEL\s+\d+\s+PAUSE\s+(.*)$/)) {
      if ($errorlevel > 0) {
        die "PAUSE $1\n";  
      }


# pause
    } elsif (($line =~ /^pause\s+(.*)$/) 
          || ($line =~ /^PAUSE\s+(\S+)$/)) { 
      my $REPLY = getc(STDIN);
# rem
    } elsif (($line =~ /^rem/i) || ($line =~ /^REM/i)) {  
      # Comment line
# set
    } elsif (($line =~ /^set\s+(\S+)=+(\S+)$/) 
          || ($line =~ /^SET\s+(\S+)=+(\S+)$/)){  
      $ENV{$1} = $2;
     
# default - assume Linux command and attempt to execute it     

    } else {
      my @args = split_args($line);
      $errorlevel = system(@args);
      if ($errorlevel) {
        if ($? == -1) {
          print "@args failed to execute: $!\n";
        } elsif ($? & 127) {
          printf "@args child died with signal %d, %s coredump\n",
              ($? & 127),  ($? & 128) ? 'with' : 'without';
        } else {
          printf "program exited with value %d\n", $? >> 8;
        }
      }
    }

    # If we are here, then go on to next line:
    $i++;
    last if $i > $#lines;

  } # Next line
}



# return string with meta bits set:
sub set_meta($) {
  my ($a) = @_;
  $a =~ tr/\0-\177/\200-\377/;
  return($a);
}


sub split_args($) {
  my ($args) = @_;
  $args =~ s/^\s+//;
  # Protect quoted strings in $args:
  $args =~ s/\"([^\"]*)\"/"\"".set_meta("$1")."\""/ge;
  $args =~ s/\'([^\"]*)\'/"\'".set_meta("$1")."\'"/ge;
  my @args = split(/\s+/, $args);
  foreach (@args) {
    # Clear the protection:
    tr/\200-\377/\0-\177/;
  }
  return(@args);
}

