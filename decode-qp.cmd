extproc perl -S
#!i:/perllib/bin/perl

eval 'exec i:/perllib/bin/perl  -S $0 ${1+"$@"}'
    if 0; # not running under some shell

use MIME::QuotedPrint qw(decode_qp);

while (<>) {
    print decode_qp($_);
}

