extproc perl -Sw
#!i:/perllib/bin/perl -w

eval 'exec i:/perllib/bin/perl -w -S $0 ${1+"$@"}'
    if 0; # not running under some shell

#########################################
# xmlsemdiff -- command-line freindly interface to
#               XML::SemanticDiff
#
########################################

use strict;
use XML::SemanticDiff;
my $diff = XML::SemanticDiff->new(keeplinenums => 1);

my ($file1, $file2) = @ARGV;
usage() unless defined $file1 and defined $file2;

foreach my $change ($diff->compare($file1, $file2)) {
    print "$change->{message} (between lines $change->{startline} and $change->{endline})\n";
}

sub usage {
   die "usage: $0 one.xml two.xml \n";
}

exit;



