extproc perl -Sw
#!i:/perllib/bin/perl -w

eval 'exec i:/perllib/bin/perl -w -S $0 ${1+"$@"}'
    if 0; # not running under some shell
#
# make_encmap
#
# Copyright 1998 Clark Cooper
# All rights reserved.
#
# This program is free software; you may redistribute it and/or
# modify it under the same terms as Perl itself.
#

my $name = shift;
my $file = shift;

my $except_str = '$@\^`{}~';
my %Exceptions;

foreach (unpack('c*', $except_str)) {
  $Exceptions{$_} = 1;
}

die "Usage is:\n\tmake_encmap name file\n"
  unless (defined($name) and defined($file));

open(MAP, $file) or die "Couldn't open $file";

my @byte1;
my $minpos = 256;

while (<MAP>) {
  next if /^\#/;

  next unless /0x([\da-f]{2,4})\s+0x([\da-f]{4})\s*\#\s*(.*)\s*$/i;

  my ($from, $to, $name) = ($1, $2, $3);

  my $flen = length($from);
  die "Bad line at $., from must be either 2 or 4 digits:\n$_"
    if $flen == 3;

  my $toval = hex($to);
  my $f1 = substr($from, 0, 2);
  my $f1val = hex($f1);

  if ($flen == 2) {
    if ($f1val < 128) {
	next if $f1val == $toval;

	warn "The byte '0x$f1' mapped to 0x$to\n"
	  unless defined($Exceptions{$f1val});
      }
		      
    if (defined($byte1[$f1val])) {
      die "Multiple mappings for 0x$f1val: $to & " . sprintf("0x%x",
							     $byte1[$f1val])
	if ($byte1[$f1val] != $toval);
    }
    else {
      $byte1[$f1val] = $toval;
      $minpos = $f1val
	if $f1val < $minpos;
    }
  }
  else {
    my $b1 = $byte1[$f1val];

    if (defined($b1)) {
      die "The 1st byte of '$from' overlaps a single byte definition."
	unless ref($b1);
    }
    else {
      $b1 = $byte1[$f1val] = [];
      $minpos = $f1val
	if $f1val < $minpos;
    }

    my $f2 = substr($from, 2, 2);
    my $f2val = hex($f2);

    $b1->[$f2val] = $toval;
  }
}

close(MAP);


print "<encmap name='$name'>\n";

die "Minpos never set" unless $minpos < 256;

process_byte(2, $minpos, \@byte1);

print "</encmap>\n";

####
## End main
####

sub emit {
  my ($pre, $start, $lim, $val) = @_;

  my $len = $lim - $start;

  if ($len == 1) {
    printf("$pre<ch byte='x%02x' uni='x%04x'/>\n", $start, $val);
  }
  else {
    printf("$pre<range byte='x%02x' len='%d' uni='x%04x'/>\n",
	   $start, $len, $val);
  }
}  # End emit

sub process_byte {
  my ($lead, $minpos, $aref) = @_;

  my $rngstrt;
  my $rngval;
  my $i;

  my $prefix = ' ' x $lead;

  for ($i = $minpos; $i <= $#{$aref}; $i++) {
    my $v = $ {$aref}[$i];
  
    if (defined($v)) {
      if (ref($v)) {
	emit($prefix, $rngstrt, $i, $rngval)
	  if defined($rngstrt);

	$rngstrt = undef;
	printf "$prefix<prefix byte='x%02x'>\n", $i;
	process_byte($lead + 2, 0, $v);
	print "$prefix</prefix>\n";
      }
      else {
	next if (defined($rngstrt) and ($v - $rngval == $i - $rngstrt));

	emit($prefix, $rngstrt, $i, $rngval)
	  if defined($rngstrt);

	$rngstrt = $i;
	$rngval = $v;
      }
    }
    else {
      emit($prefix, $rngstrt, $i, $rngval)
	if defined($rngstrt);

      $rngstrt = undef;
    }
  }

  emit($prefix, $rngstrt, $i, $rngval)
    if defined($rngstrt);

}  # End process_byte
