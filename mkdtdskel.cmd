extproc perl -S
#!i:/perllib/bin/perl

eval 'exec i:/perllib/bin/perl  -S $0 ${1+"$@"}'
    if 0; # not running under some shell
use XML::DT;
mkdtdskel (@ARGV);

__END__

=head1 NAME

mkdtskel - DTD generator using XML::DT

=head1 SYNOPSIS

  mkdttskel <xmlfile>

=head1 DESCRIPTION

This command tries to infer the DTD structure for a specific XML file;

=head1 SEE ALSO

XML::DT(1), mkdtskel(1) and perl(1)

=cut
