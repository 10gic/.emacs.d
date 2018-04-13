#!/usr/bin/env perl

# Execute program with timeout
# Usage: perl timeout.pl timeout_in_sec prog args...

use strict;
use warnings;

my $timeout_sec=shift;  # first arg
if ($timeout_sec =~ /[0-9]+/ && $timeout_sec > 0) {
  # print STDERR "set timeout to $timeout_sec\n"
} else {
  die "bad usage. usage is `perl timeout.pl timeout_in_sec prog args...`";
}

my $pid;

# Please see: `perldoc -f alarm`
eval {
  local $SIG{ALRM} = sub { die "alarm\n" }; # NB: \n required
  # print STDERR "Start\n";
  alarm $timeout_sec;   # schedule alarm
  $pid = fork;
  if ($pid == 0) {
    exec @ARGV;
  } else {
    wait;
  }
  alarm 0;              # cancel the alarm
  # print STDERR "done with\n";
};

if ($@) {
  if ($@ eq "alarm\n") {
    kill(15, $pid);  # or kill(9, $pid);
    print STDERR "exec timeout, pid $pid killed\n";
    exit 124;
    # exit 124, just like timeout util in linux, see:
    # http://git.savannah.gnu.org/cgit/coreutils.git/tree/src/timeout.c
  } else {
    print STDERR "something else went boom\n"; # propagate unexpected errors
  }
}
