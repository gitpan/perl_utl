extproc perl -S
#!i:/perllib/bin/perl

eval 'exec i:/perllib/bin/perl  -S $0 ${1+"$@"}'
    if 0; # not running under some shell

use MIME::Base64 qw(decode_base64);

while (<>) {
    print decode_base64($_);
}

