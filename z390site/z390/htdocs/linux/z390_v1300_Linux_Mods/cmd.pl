#!/usr/local/bin/perl
# Emulation of cmd.exe for handling simple batch files.
#
# The support files (z390.jar etc.) are searched for in the following places:
# (1) The directory given in the Z390 environment variable
# (2) $HOME/lib/z390
# (3) /usr/local/lib/z390
# (4) /usr/lib/z390

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

  if (/^CD (.*)$/) {
    chdir $1 or die "Can't chdir to $1: $!\n";
  } elsif (/^exit$/) {
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
  $cmd =~ s/\.bat$//i;
  $cmd =~ s/(\w+)$/\U$1\E/i; # batch file names are all upper case.
  my @args = split_args($args);
  # Add a dummy argument so that the numbering starts from 1:
  unshift(@args, "");
  # Pad out to 9 arguments:
  push(@args, ("") x 9);
  my $file = "";
  if ($cmd =~ /^\//) {
    # $cmd is a full path
    $file = "$cmd.BAT";
  } else {
    # Search for $cmd in $bat dir:
    my $dh;
    opendir($dh, $bat) or die "Can't read $bat directory: $!\n";
    my @files = sort grep { $_ eq "$cmd.BAT" } readdir($dh);
    closedir($dh);
    die "Batchfile $cmd.BAT not found in $bat!\n" unless @files == 1;
    $file = "$bat/$files[0]";
  }
  my $fh;
  open($fh, $file) or die "Can't read $file: $!\n";
  my @lines = <$fh>;
  close($fh);
  my %labline = (); # Map each label to the line it is found on
  my $i = 0;
  foreach (@lines) {
    s/\cM//g;
    chomp;
    s/^\s+//;
    if (/^:(\S+)/) {
      $labline{$1} = $i;
    }
    $i++;
  }

  # Execute the batch file:
  $i = 0; # Current line
  while (1) {
    my $line = $lines[$i];
    $line =~ s/\%\~dps0/$bat\//g; # I don't know where this gets set!
    $line =~ s/\%([1-9])/$args[$1]/g;
    $line =~ s/\s+$//; # Trim trailing spaces.
    print "$line\n";

    if ($line =~ /^\s*$/) {
      # Blank line

    } elsif ($line =~ /^rem/i) {
      # Comment line

    } elsif ($line =~ /^:\S/i) {
      # Label line

    } elsif ($line =~ /^if\s+errorlevel\s+\d+\s+goto\s+(\S+)$/i) {
      if ($errorlevel > 0) {
	$i = $labline{$1};
	die "Label $1 not found!\n" unless defined($i);
	next;
      }

    } elsif ($line =~ /^goto\s+(\S+)$/) {
      $i = $labline{$1};
      die "Label $1 not found!\n" unless defined($i);
      next;

    } elsif ($line =~ /^pause\s+(\S+)$/) {
      print "$1\n";

    } elsif ($line =~ /^erase\s+(\S+)$/) {
      my $pattern = uc($1);
      my @goners = glob($pattern);
      print "Delete file(s): ", join(" ", @goners), "\n";

    } elsif ($line =~ /^copy\s+(\S+)\s+(\S+)$/) {
      my $pattern = $1;
      my $target  = $2;
      # Assume all base filenames are upper case:
      $pattern =~ s/([a-zA-Z0-9]\.[\*\?a-zA-Z0-9])$/\U$1\E/;
      my @list = glob($pattern);
      print "Copy file(s): ", join(" ", @list), " to $target\n";

    } elsif ($line =~ /^call\s+(\S+)(.*)$/) {
      # Call another batch file:
      batch_file($1, $2);

    } elsif ($line =~ /^java\s/) {
      # Call the jar file:
      my @args = split_args($line);
      $errorlevel = system(@args);
      if ($errorlevel) {
        if ($? == -1) {
          print "@args failed to execute: $!\n";
        } elsif ($? & 127) {
          printf "@args child died with signal %d, %s coredump\n",
              ($? & 127),  ($? & 128) ? 'with' : 'without';
        } else {
          printf "child exited with value %d\n", $? >> 8;
        }
      }

    } else {
      die "Unexpected line in BAT file:\n $line\n";
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

