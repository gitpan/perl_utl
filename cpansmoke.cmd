extproc perl -S
#!i:/perllib/bin/perl

eval 'exec i:/perllib/bin/perl  -S $0 ${1+"$@"}'
    if 0; # not running under some shell
# $File: //depot/cpanplus/dist/bin/cpansmoke $
# $Revision: #7 $ $Change: 7667 $ $DateTime: 2003/08/23 16:31:15 $

use strict;
use Cwd;
use Config;
use Getopt::Std;
use CPANPLUS::Backend;
use CPANPLUS::I18N;

my $VERSION = '0.06';

=head1 NAME

cpansmoke - CPAN Smoke Tester

=head1 SYNOPSIS

B<cpansmoke> S<[ -B<acdfilpuv> ]> S<[ -B<t> I<timeout> ]> I<packages>...

=head1 DESCRIPTION

This script uses B<CPANPLUS> to test one or more distributions from
CPAN.  It accepts full or partial distribution names (e.g. C<CPANPLUS-0.01>
and C<K/KA/KANE/CPANPLUS-0.01.tar.gz>), or module names (e.g.
C<CPANPLUS::Backend>), in which case the latest distribution containing
the module is tested.

=head1 OPTIONS

 -a    Automatically send reports without prompting or editing
 -c    Always Cc: to the module author (only on FAIL by default)
 -d    Display each package's existing result before testing it
 -f    Force re-fetching cached packages and checksum data
 -i    Don't cc FAIL reports to author if two or more FAILs were reported
 -l    Use user-configured hosts; www.cpan.org is preferred by default
 -p    Fetch, make and test (but not install) prerequisite modules
 -s    Skip modules that have testing results from the same settings
 -u    Skip modules that have more than one testing results as UNKNOWN
 -v    Print verbose proceeding informations
 -t    Sets timeout for each package's install; defaults to 300

=head1 NOTES

If the B<-a> option is set, only failures during C<make test> are reported,
to prevent bogus reports caused by insufficient non-perl requisites.

For automatic testing, C<cpansmoke -aipsu> is preferred. Subscriber of the
C<cpan-testers@perl.org> list may use this B<Mail::Audit> recipe to enable
unattended smoke testing:

    fork || system("cpansmoke -aps $1 >/dev/null 2>&1")
        if $mail->subject =~ /^CPAN Upload: (.*)$/;

Users of C<procmail> should add this line to C<.procmailrc>:

    :0hc
    * ^Subject: CPAN Upload:
    |sh -c "grep Subject|cut -f4 -d' '|xargs cpansmoke -aps >/dev/null 2>&1"

Please consult L<CPANPLUS::TesterGuide> for additional information.

=cut

############################################################################
### Parse Options ##########################################################
############################################################################

my %args;
show_usage() unless @ARGV and getopts('acdfihlpsuvt:', \%args);

my ($auto, $cc, $display, $force, $help, $local, $prereq, $skip, 
    $skip_unknown, $skip_fail, $verbose, $timeout)
    = @args{qw|a c d f h l p s u i v t|};

show_usage() if $help;

############################################################################
### Initialize Environment #################################################
############################################################################

my $cp   = CPANPLUS::Backend->new;              # CPANPLUS Backend object
my $conf = $cp->configure_object;               # Configuration for this session

my $shell = bless({}, 'main');                  # fake shell object
$cp->{_shell} = $shell;

$ENV{VISUAL} = 'echo' if $auto;                 # let cpantest skip editing

my $cpantest_conf = '';
$cpantest_conf .= 'always_cc,'     if $cc;
$cpantest_conf .= 'maketest_only,' if $auto;
$cpantest_conf .= 'dont_cc,'       if $skip_fail;

$conf->set_conf( prereqs  => $prereq  );
$conf->set_conf( force    => $force   );
$conf->set_conf( verbose  => $verbose );
$conf->set_conf( cpantest => $cpantest_conf || 1);

$conf->_set_ftp(urilist => [ {                  # prefers www.cpan.org
    path   => '/',
    scheme => 'http',
    host   => 'www.cpan.org',
}, @{ $conf->_get_ftp('urilist') } ]) unless $local;

$ENV{PERL_EXTUTILS_AUTOINSTALL} .= ' --testonly';
$ENV{PERL_EXTUTILS_AUTOINSTALL} .= ' --defaultdeps' if $auto;

############################################################################
### Start Smoking ##########################################################
############################################################################

foreach my $pkg (@ARGV) {
    $pkg =~ s|.*authors/id||;                   # strip leading paths
    $pkg = "/$pkg" if $pkg =~ m|^[^/].*/|;      # add leading / if needed

    ### Translate module names to package names
    my $dist = $cp->pathname(to => $pkg)
        or (print loc("No such module: %1, skipping.\n", $pkg), next);

    # Check local database
    my $mod =  $dist;
    $mod=~s!^.*/([^/]*)\.tar\.gz$!$1!;

    if ($skip and !$cp->_not_already_sent($mod, 1)) {
        print loc("%1 already tested on this machine; skipping.\n", $dist);
        next;
    }
    next if (_check_existing($dist));

    print loc("Testing: %1\n",($pkg =~ /[^\w:]/ ? $dist : "$pkg ($dist)"));

    eval {
        $timeout ||= 600;

        my $cwd      = Cwd::cwd();
        my $alarm_ok = eval 'alarm ($timeout); 1;';
        my $nohang   = eval 'use POSIX (); POSIX::WNOHANG();' unless $alarm_ok;

        local $SIG{ALRM} = sub { die "\n" } if $alarm_ok;

        if ($nohang and $^O eq 'MSWin32') {
            # chdir to the build dir since IPC::Run and fork don't mix
            my $path = $dist; $path =~ s|.*/||;
            $path =~ s/(?:\.tar\.(?:gz|Z|bz2)|\.t[gb]z|\.zip)$//i;
            chdir $conf->_get_build('base') or die $!;
            chdir $conf->_get_build('moddir') or die $!;
            mkdir $path, 0755 unless -d $path;
            chdir $path or die $!;

            $nohang = 0; # XXX!
        }

        if ($nohang and my $pid = fork()) {
            # waitpid-based alarm loop
            my $time = time;
            while ((time - $time) < $timeout) {
                last if waitpid($pid, $nohang);
                sleep 1;
            }

            kill(1, $pid); sleep 1; kill(9, $pid);
        }
        else {
            $cp->install(                       # perform the install
                modules       => [ $pkg ],      # one module at a time
                target        => 'test',        # but stop after 'make test'
                prereq_target => 'test',        # same for its prerequisites
            );
            exit if $nohang;
        }

        chdir $cwd;
        eval 'alarm 0' if $alarm_ok;
    };

    warn $@ if $@;                              # warns any errors
}

### Fake shell method to confirm sending out report ###
sub _ask_report {
    my $obj   = shift;
    my %args  = @_;
    my $dist  = $args{dist};
    my $grade = $args{grade};

    return 0 if $skip and _check_existing($dist); # yes, we double-check it.
    return 1 if $auto;

    require Term::ReadLine;
    $obj->{_term} ||= Term::ReadLine->new($0);

    return $obj->{_term}->readline(
        "Report ${dist}'s testing result (\U$grade\E)? [y/N]: "
    ) =~ /^[yY]/;
}

### Display usage info ###
sub show_usage {
    print loc("
Usage:
  $0 [ -acflpv ] [ -t timeout ] <module | distribution> ...

  -a    Automatically send reports without prompting or editing
  -c    Always Cc: to the module author (only on FAIL by default)
  -d    Display each package's existing result before testing it
  -f    Force re-fetching cached packages and checksum data
  -i    Don't cc FAIL reports to author if two or more FAILs were reported
  -l    Use user-configured hosts; www.cpan.org is preferred by default
  -p    Fetch, make and test (but not install) prerequisite modules
  -s    Skip modules that have testing results from the same settings
  -u    Skip modules that have more than one testing results as UNKNOWN
  -v    Print verbose proceeding informations
  -t    Sets timeout for each package's install; defaults to 300

");
    exit;
}

### Wrapper to display / check for existing reports on this OS or
### with UNKNOWN status; return 1 if skip ###
sub _check_existing {
    my $dist = shift;

    return unless $display or $skip or $skip_unknown;

    my $report = $cp->reports( modules => [ $dist ] ) || {};
    my $is_there;
    my $is_unknown= 0;

    while ( my ($name, $href) = each (%$report) ) {
        next unless ref($href);
        $href = (values(%$href))[0] or next if UNIVERSAL::isa($href, 'HASH');

        for my $rv (@{$href}) {
            next unless UNIVERSAL::isa($rv, 'HASH');

            if ($rv->{grade} eq 'UNKNOWN') { $is_unknown++; }
            next if (!$skip and !$display);

            printf "%8s %s%s\n", @{$rv}{'grade', 'platform'},
                                 ($rv->{detail} ? ' (*)' : '') if $display;

            #my @tested = split /\s/, $rv->{ 'platform' };
            $is_there = 1 #if $^O eq $tested[0]
#                         and $Config{osvers} =~ /^\Q$tested[1]\E(?:[^-]|$)/
#                         and $Config{archname}=~ /^\Q$tested[2]\E(?:[^-]|$)/;
              if $Config{archname} eq $rv->{ 'platform' };
        }
    }

    if ($skip and $is_there) {
        print loc("%1 already tested on this configuration; skipping.\n", $dist);
        return 1;
    }
    elsif ($skip_unknown and $is_unknown > 1) {
        print loc("%1 already tested and declared as UNKNOWN two times; skipping.\n", $dist);
        return 1;
    }

    return;
}

### PERL5LIB munging so we don't need to install prereqs
BEGIN {
    local $^W; # voluntary namespace munging and redefinition
    no strict 'refs';

    my $oldref = *{'CPANPLUS::Internals::Make::_make'}{CODE};

    *{'CPANPLUS::Internals::Make::_make'} = sub {
        my ($self, %args) = @_;

        $ENV{PERL5LIB} = join(
            $Config{path_sep},
            split($Config{path_sep}, $ENV{PERL5LIB}),
            File::Spec->catdir($args{dir}, 'blib', 'lib'),
            File::Spec->catdir($args{dir}, 'blib', 'arch'),
        );

	goto &$oldref;
    } 
}

__END__

=head1 SEE ALSO

L<CPANPLUS>, L<CPANPLUS::TesterGuide>, L<cpantest>

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
