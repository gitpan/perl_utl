extproc perl -Sx 
#!f:/perllib/bin/perl
    eval 'exec perl -S $0 "$@"'
	if 0;

use Pod::Text;

if(@ARGV) {
	pod2text($ARGV[0]);
} else {
	pod2text("<&STDIN");
}

