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
# 02/15/07 DSH RPI 548
#   1) Correct PAUSE to wait for ENTER key rather than reading stdin
#   2) Use /^.../i for mixed case per Martin Ward suggestion
#   3) Correct if errorlevel  to use specific value vs 1
#   4) Support DIR and CD as dos commands outside BAT file
# 10/20/07 RPI 713 fix SET to  support = in file extended parms (ie [RECFM=FT]
#                  set return code for DIFF command, ignore white space changes
#                  and show brief results in log along with rcexit

#                  pause on unknown BAT command file
# 01/29/08 RPI 792 Support Windows compatible ECHO ON/OFF, IF EXIST file cmd
# 10/07/09 RPI 1080 add support for J2SEOPTIONS 
# 05/16/12 RPI 1216 patch to ignore spaces before "EXIT" command
#              provided by Martin  Ward 01/31/12
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
 my $ECHO = 1;
 my $HOME = $ENV{'HOME'} || $ENV{'LOGDIR'} ||
 		(getpwuid($<))[7] || die "You're homeless!\n";
+my $J2SEOPTIONS = $ENV{'J2SEOPTIONS'} || "";
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
  +s/^\s+//;
  chomp;

  print "$_\n";
# cd command outside bat file
  if (/^cd (.*)$/i) {  
    chdir $1;
    system("ls");
# dir path command outside bat file
  } elsif (/^dir\s+(.*)$/i) { 
      system("ls -l $1");
# dir cur directory command outside bat file
  } elsif (/^dir/i) { 
      system("ls -l");       
# exit dos command processor
  } elsif (/^exit$/i) { 
    last;
# ignore blank lines
  } elsif (/^\s*$/) {
    # blank line
# xxxx assume bat command
  } elsif (s/(^\S+)\s*//) {
    my $cmd = $1;
    # Assume that the command is a batch file
    batch_file($cmd, $_);
  } else {
    printf "Unknown command syntax = $_\n";
  }
}
sub open_bat_file_error(){
  printf "BAT file  not found";
  open(TTY,"/dev/tty");  ## RPI 548
  my $REPLY = getc(TTY);
  close(TTY);
  die "exiting dos processor";
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
  open($fh, $file) or open_bat_file_error(); ## RPI 713
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
+    $line =~ s/\%J2SEOPTIONS\%/$J2SEOPTIONS/g;   # substitute current bat parms 1-9
     $line =~ s/\s+$//;                  # Trim trailing spaces.
     $line =~ s/\\/\//g;              # replace \ with /
     if ($ECHO == 1){
      print "$line\n";
    }

# exec dos command or assume it is native command
    if ($line =~ /^\s*$/) {
      # Blank line

# :label
    } elsif ($line =~ /^:\S/i) {
      # Label line

# call
    } elsif ($line =~ /^call\s+(\S+)(.*)$/i) { 
      # Call another batch file:
      batch_file($1, $2);

# copy one or more files
    } elsif ($line =~ /^copy\s+(\S+)\s+(\S+)$/i){  
         $errorlevel = system("cp $1 $2");
         if ($errorlevel){
            print "Copy error: $errorlevel\n";
         }

# diff dir1 dir2 file3
    } elsif ($line =~ /^diff\s+(\S+)\s+(\S+)\s+(\S+)$/i) {
      open IN, "diff -b $1 $2 |" or die "diff generation failed"; ## RPI 713 ignore white space
      open OUT,"> $3" or die "diff open output $3 failed";
      print OUT <IN>;
      close IN;
      close OUT;
      $errorlevel = system("diff -b -q $1 $2"); ## // RPI 713 set rc, skip white space, brief
      print "diff errorlevel: $errorlevel\n";

# dir
    } elsif ($line =~ /^dir\s+(.*)$/i) { 
      $errorlevel = system("ls -l $1");
      if ($errorlevel) {
          print "dir errorlevel: $errorlevel\n";
      }
# echo on/off
    } elsif ($line =~ /^echo\s+(\S+)$/i) { 
      if ($1 eq "ON"){
         $ECHO = 1;
      } else {
         $ECHO = 0;
      }
# erase file
    } elsif ($line =~ /^erase\s+(\S+)$/i) { 
      $errorlevel = system("rm $1");
      if ($errorlevel) {
          print "erase errorlevel: $errorlevel\n";
      };
# erase files /q    
    } elsif ($line =~ /^erase\s+(\S+)\s+(\S+)$/i) {
      $errorlevel = system("rm $1");
# exit
    } elsif ($line =~ /^exit\s$/i) { 
      die "EXIT\n";
# gedit file1
    } elsif ($line =~ /gedit\s+(.*)$/i) { 
      $errorlevel = system("gedit $1");
      if ($errorlevel) {
          print "gedit errorlevel: $errorlevel\n";
      }
# goto
    } elsif ($line =~ /^goto\s+(\S+)$/i) { 
      $i = $labline{lc $1}; 
      die "Label $1 not found!\n" unless defined($i);
      next;
# if exist file erase file
    } elsif ($line =~ /^if\s+exist\s+(\S+)+\s+erase\s+(\S+)$/i) {
      if (-f "$1") {
          $errorlevel = system("rm $2");
	  next;
      }
# if exist file goto label
    } elsif ($line =~ /^if\s+exist\s+(\S+)+\s+goto\s+(\S+)$/i) {
      if (-f "$1") {
          $i = $labline{lc $2}; 
	  die "Label $1 not found!\n" unless defined($i);
	  next;
      }
# if errorlevel 1 goto label
    } elsif ($line =~ /^if\s+errorlevel\s+(\d+)\s+goto\s+(\S+)$/i) {
      my $RC = $errorlevel/256; ## RPI 548
      if ($RC >= $1) {
	$i = $labline{lc $2}; 
	die "Label $2 not found!\n" unless defined($i);
	next;
      }
# if errorlevel 1 pause text
    } elsif ($line =~ /^if\s+errorlevel\s+(\d+)\s+pause\s+(.*)$/i) {
      my $RC = $errorlevel/256; ## RPI 548
      if ($RC >= $1) {
          open(TTY,"/dev/tty");  ## RPI 548
          my $REPLY = getc(TTY);
          close(TTY);  
      }


# pause
    } elsif ($line =~ /^pause\s+(.*)$/i) { 
      open(TTY,"/dev/tty");  ## RPI 548
      my $REPLY = getc(TTY);
      close(TTY);
# rem
    } elsif (($line =~ /^rem/i) || ($line =~ /^REM/i)) {  
      # Comment line
# set
    } elsif ($line =~ /^set\s+(\w+)=(\S+)$/i){ ## RPI 713
      printf "SET NAME=%s\n", $1;
      printf "SET PARM VALUE=%s\n", $2;
      my $VAL = $2;
      $VAL =~ s/\\=/\>\//g;       # replace = with > to avoid Linux set error
      $ENV{$1} = $VAL;
      printf "SET ENV VALUE=%s\n", $ENV{$1}; ## RPI 713
     
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

