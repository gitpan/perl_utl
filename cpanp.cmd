extproc perl -S
#!i:/perllib/bin/perl

eval 'exec i:/perllib/bin/perl  -S $0 ${1+"$@"}'
    if 0; # not running under some shell
# $File: //depot/cpanplus/dist/bin/cpanp $
# $Revision: #8 $ $Change: 8345 $ $DateTime: 2003/10/05 17:25:48 $

use strict;
use vars '$VERSION';

use CPANPLUS;
use CPANPLUS::Tools::Term;
use Term::ReadLine;

$VERSION = CPANPLUS->VERSION;

=head1 NAME

cpanp - The CPANPLUS launcher

=head1 SYNOPSIS

B<cpanp>

B<cpanp> S<[ --[B<no>]I<option>... ]> S<[-]B<af>> S<[ I<author>... ]>

B<cpanp> S<[ --[B<no>]I<option>... ]> S<[-]B<moitudlrczb>> S<[ I<module>... ]>

=head1 DESCRIPTION

This script launches the B<CPANPLUS> utility to perform various operations
from the command line. If it's invoked without arguments, an interactive
shell is executed by default.

Optionally, it can take a single-letter switch and one or more argument,
to perform the associated action on each arguments. The commands are:

    a AUTHOR...     # search by author(s)
    m MODULE...     # search by module(s)
    f AUTHOR...     # list all distributions by an author(s)
    i MODULE...     # install module(s)
    t MODULE...     # test module(s)
    u MODULE...     # uninstall module(s)
    d MODULE...     # download module(s) into current directory
    l MODULE...     # display detailed information about module(s)
    r MODULE...     # display README files of module(s)
    c MODULE...     # check for module report(s) from cpan-testers
    z MODULE...     # extract module(s) and open command prompt in it
    o [ MODULE... ] # list installed module(s) that aren't up to date
    b               # make autobundle of currently installed modules

Each command may be prefixed with one or more I<options>.  If preceded by
C<no>, the corresponding option will be set to C<0>, otherwise it's set to
C<1>. The valid options includes C<cpantest>, C<debug>, C<flush>, C<force>,
C<prereqs>, C<storable>, C<verbose>, C<md5> and C<signature>; please consult
C<CPANPLUS::Configure> for an explanation to their meanings.

=cut

my $opt;

my @bool = qw(cpantest debug flush force storable verbose md5 prereqs skiptest signature);

my $cmd = {
    a => "search",
    m => "search",
    f => "distributions",
    o => "uptodate",
    i => "install",
    t => "install",
    u => "uninstall",
    d => "fetch",
    l => "details",
    r => "readme",
    c => "reports",
    z => "shell",
    b => "autobundle",
};

my @cmd_stack;

while ($opt = shift(@ARGV)) {
    push @cmd_stack, "s $2 ".($1 ? 0 : 1) and next
        if ($opt =~ /^--(no)?(\w+)$/);

    $opt =~ s/^-//; $opt = lc(substr($opt, 0, 1));
    last;
};

while (@ARGV) {
    $ARGV[0] =~ /^--(no)?(\w+)$/ or last;
    push @cmd_stack, "s $2 ".($1 ? 0 : 1);
    shift(@ARGV);
}

push @cmd_stack, map { "$opt $_" } @ARGV if ($opt and exists $cmd->{$opt});

# special case: 'o' and 'b' may take zero arguments
@cmd_stack = $opt if !@cmd_stack and $opt and $opt =~ /^[ob]$/;

my $shell_type;
if (@cmd_stack) {
    # initializes configure setup if we've not been through it yet
    no strict 'refs';
    $shell_type = 'Default';

    my $TR = ($^O eq 'MSWin32') ? 'Term::ReadLine::Stub'
                                : $Term::ReadLine::ISA[0];
                                
    # remember the old coderefs in case of a setoption-only invocation
    my $newref  = *{"$TR\::new"}{CODE};
    my $histref = *{"$TR\::addhistory"}{CODE};

    # save the original constructor arguments in the Faked arrayref
    *{"$TR\::new"} = sub {
        # shell only
        goto &{$newref} unless caller(0)->isa('CPANPLUS::Shell')
                            or caller(0) =~ /^CPANPLUS::Shell::[^_]/;

        return bless([@_], 'CPANPLUS::Shell::_Faked');
    };

    *{'CPANPLUS::Shell::_Faked::addhistory'} = sub { };
    *{'CPANPLUS::Shell::_Faked::spawn'}   = sub {
        # there's no immediate actions anyway, and we've run out of 's',
        # so we restore the handlers and regen a genuine object

        *{"$TR\::new"} = $newref;
        *{"$TR\::addhistory"} = $histref;

        # regen self, pass the args to the new object
        return ($_[0] = $newref->(@{$_[0]}));
    };
    *{'CPANPLUS::Shell::_Faked::parse_options'}   = sub {
        return +shift->spawn->parse_options(@_);
    };
    *{'CPANPLUS::Shell::_Faked::get_reply'}   = sub {
        return +shift->spawn->get_reply(@_);
    };
    *{'CPANPLUS::Shell::_Faked::ask_yn'}   = sub {
        return +shift->spawn->ask_yn(@_);
    };
    *{'CPANPLUS::Shell::_Faked::readline'}   = sub {
        return shift @cmd_stack if $opt or @cmd_stack;
        return +shift->spawn->readline(@_);
    };

    *{'CPANPLUS::Shell::_Faked::ReadLine'} = sub { $TR };
    *{'CPANPLUS::Shell::_Faked::parse_options'} = \&CPANPLUS::Tools::Term::parse_options;
}

shell($shell_type);

1;

__END__

=head1 SEE ALSO

L<CPANPLUS>

=head1 AUTHORS

Autrijus Tang E<lt>autrijus@autrijus.orgE<gt>

=head1 COPYRIGHT

Copyright 2001, 2002 by Autrijus Tang E<lt>autrijus@autrijus.orgE<gt>.

This program is free software; you can redistribute it and/or 
modify it under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=cut

# Local variables:
# c-indentation-style: bsd
# c-basic-offset: 4
# indent-tabs-mode: nil
# End:
# vim: expandtab shiftwidth=4:
