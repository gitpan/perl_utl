extproc perl -S
#!i:/perllib/bin/perl

eval 'exec i:/perllib/bin/perl  -S $0 ${1+"$@"}'
    if 0; # not running under some shell

#
# No-brainer to size an image supplied on the command-line. All the real
# work is done in Image::Size
#

=head1 NAME

imgsize - read the dimensions of an image in several popular formats

=head1 SYNOPSIS

 imgsize [ -r | -a | -f fmt ] file

=head1 DESCRIPTION

No-brainer to size an image supplied on the command-line. All the real
work is done in L<Image::Size>

=head1 OPTIONS

By default, the width and height are returned as attributes for an IMG tag
in HTML, essentially "C<WIDTH=40 HEIGHT=30>". The following options may be
used to return alternate formats (all report width first, then height):

=over

=item C<-r>

Return "raw" format data. Just the numbers separated by a single space.

=item C<-a>

Return a Perl-style list of attributes suitable for passing to the C<img()>
method of the CGI module (see L<CGI>).

=item C<-f> B<fmt>

Pass the string specified in I<fmt> to C<sprintf> and thus use it to format
the results to your taste. C<sprintf> will be passed two numbers, so any
other formatting directives will be lost. The numbers are passed as width
first, then height.

=head1 SEE ALSO

L<Image::Size>

=head1 AUTHOR

Randy J. Ray <rjray@blackperl.com>. Copyright (c) 2000. Distributable under
the Artistic License as packaged with Perl version 5.005 and later.

=cut

use strict;
use vars qw($opt_h $opt_r $opt_a $opt_f);

use Image::Size qw(:all);
use Getopt::Std;

my $rtn;

&getopts('hraf:');

#
# Usage reporting: if -h, or no @ARGV, or more than one of the rest...
#
die sprintf("Usage: %s [ -r | -a | -f fmt ] file ...\n", ($0 =~ m|.*/(.*)|o))
    if ($opt_h || (! @ARGV) || (($opt_a && $opt_r) || ($opt_a && $opt_f) ||
                                ($opt_r && $opt_f)));

$rtn = \&html_imgsize;
$opt_a &&
    ($rtn = \&return_attr);
$opt_r &&
    ($rtn = \&return_imgsize);
$opt_f &&
    ($rtn = \&return_fmt);

if (@ARGV > 1)
{
    foreach (@ARGV)
    {
        print STDOUT sprintf("$_: %s\n", &$rtn($_));
    }
}
else
{
    print STDOUT sprintf("%s\n", &$rtn($ARGV[0]));
}

exit;

#
# Note the doubled calls here. This is just a quick, semi-clean attempt at
# functionality. As it happens, the second call will be a cache hit within
# the Image::Size package.
#

sub return_attr
{
    my ($width, $height, $err) = imgsize($_[0]);

    (defined $width) ?
        "(-width => $width, -height => $height)" : "error: $err";
}

sub return_imgsize
{
    my ($width, $height, $err) = imgsize($_[0]);

    (defined $width) ? "$width $height" : "error: $err";
}

sub return_fmt
{
    my ($width, $height, $err) = imgsize($_[0]);

    (defined $width) ? sprintf($opt_f, $width, $height, $err) : "error: $err";
}
