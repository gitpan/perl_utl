extproc perl -S
#!i:/perllib/bin/perl

eval 'exec i:/perllib/bin/perl  -S $0 ${1+"$@"}'
    if 0; # not running under some shell

use MIME::Base64 qw(encode_base64);

my $buf = "";
while (<>) {
    $buf .= $_;
    if (length($buf) >= 57) {
	print encode_base64(substr($buf, 0, int(length($buf) / 57) * 57, ""));
    }
}

print encode_base64($buf);
