extproc perl -S
#!i:/perllib/bin/perl

eval 'exec i:/perllib/bin/perl  -S $0 ${1+"$@"}'
    if 0; # not running under some shell
# $File: //member/autrijus/PAR/script/pp $ $Author: autrijus $
# $Revision: #91 $ $Change: 8606 $ $DateTime: 2003/10/28 11:48:53 $ vim: expandtab shiftwidth=4

use 5.006;
use strict;
use warnings;

use Config;
our $PARL;

# bootstrap ourselves on a binary-only install.
unless (eval { require PAR; 1 }) {
    $PARL ||= _can_run("parl$Config{_exe}") or die("Can't find par loader");
    exec($PARL, $0, @ARGV);
}

use Archive::Zip;
use Cwd;
use ExtUtils::MakeMaker; # just for maybe_command()
use File::Basename;
use File::Spec;
use File::Temp qw(tempfile);
use Getopt::Long; 
use Module::ScanDeps 0.10;
use PAR::Filter;

our $VERSION = 0.05;
$| = 1;

$SIG{INT} = sub { exit(); }; # exit gracefully and clean up after ourselves.

sub opt(*); # imal quoting
sub is_win32();
sub vprint($@);

our ($Options);
our (@Input, $Output);
our ($logfh);
our ($par_file);
our (@SharedLibs);

my $dynperl = $Config{useshrplib} && ($Config{useshrplib} ne 'false');

main();

sub main {
    parse_argv();
    check_write($Output);
    generate_code();
    run_code();
    _die("XXX: Not reached?");
}

#######################################################################

sub compile_par { 
    my ($cfh, $lose);
    my $root = '';
    $root = "$Config{archname}/" if opt('m');

    if (opt(S) || opt(p)) {
        # We need to keep it.
        if (opt(e) or !@Input) {
            $par_file = "a.out.par";
        } else {
            $par_file = $Input[0];
            # File off extension if present
            # hold on: plx is executable; also, careful of ordering!
            $par_file =~ s/\.(?:p(?:lx|l|h)|m)\z//i;
            $par_file .= ".par";
        }
        $par_file = $Output if opt(p) && $Output =~ /\.par\z/i;
        check_write($par_file);
    } else {
        # Don't need to keep it, be safe with a tempfile.
        $lose = 1;
        ($cfh, $par_file) = tempfile("ppXXXXX", SUFFIX => ".par"); 
        close $cfh; # See comment just below
    }
    vprint 1, "Writing PAR on $par_file";

    my (@modules, @data, @exclude);
    foreach my $name (@{opt(M) || []}) {
        _name2moddata($name, \@modules, \@data);
    }
    foreach my $name ('PAR', @{opt(X) || []}) {
        _name2moddata($name, \@exclude, \@exclude);
    }

    my %map;
    unshift @INC, @{opt(I) || []};
    unshift @SharedLibs, map _find_shlib($_), @{opt(l) || []};

    Module::ScanDeps::scan_deps(
        rv      => \%map,
        files   => [
            (map Module::ScanDeps::_find_in_inc($_), @modules),
            (@Input ? @Input : ()),
        ],
        recurse => 1,
        skip    => {
            map { (Module::ScanDeps::_find_in_inc($_) => 1) } @exclude
        }
    );
    Module::ScanDeps::add_deps(
        rv      => \%map,
        modules => \@modules,
        skip    => {
            map { (Module::ScanDeps::_find_in_inc($_) => 1) } @exclude
        }
    );

    my %text;
    $text{$_} = ($map{$_}{type} =~ /^(?:module|autoload)$/) for keys %map;
    $map{$_}  = $map{$_}{file} for keys %map;

    my %manifest = map { $_ => 1 } ('MANIFEST', 'META.yml');
    my $size = 0;
    my $zip = Archive::Zip->new;
    my $old_member;
    if (opt('m') and -e $par_file) {
        $zip->read($par_file);
        if ($old_member = $zip->memberNamed( 'MANIFEST' )) {
            $manifest{$_}++ for grep /^\S/, split(/\n/, $old_member->contents);
        }
        else {
            $old_member = 1;
        }
    }
    my %zip_args = (
        'desiredCompressionMethod'
            => Archive::Zip::COMPRESSION_DEFLATED(),
        'desiredCompressionLevel'
            => Archive::Zip::COMPRESSION_LEVEL_BEST_COMPRESSION(),
    );

    $zip->addDirectory('', substr($root, 0, -1)) if $root and %map and $] >= 5.008;
    $zip->addDirectory('', $root.'lib') if %map and $] >= 5.008;

    my $verbatim = ($ENV{PAR_VERBATIM} || 0);
    my $mod_filter = PAR::Filter->new(
        'PatchContent',
        @{ opt(F) || ($verbatim ? [] : ['PodStrip']) },
    );
    foreach my $pfile (sort grep length $map{$_}, keys %map) {
        next if !opt(B) and ($map{$pfile} eq "$Config{privlib}/$pfile"
                          or $map{$pfile} eq "$Config{archlib}/$pfile");

        next unless $zip;
        vprint 2, "... adding $map{$pfile} as ${root}lib/$pfile";

        if ($text{$pfile}) {
            my $content_ref = $mod_filter->apply($map{$pfile}, $pfile);
            $size += length( $$content_ref );
            $zip->addString( $content_ref => $root."lib/$pfile", %zip_args );
        }
        else {
            $zip->addFile($map{$pfile} => $root."lib/$pfile");
            $size += -s $map{$pfile};
        }

        $manifest{$root."lib/$pfile"}++;
    }

    @Input = grep !/\.pm\z/i, @Input;

    $zip->addDirectory('', 'script') if @Input and $] >= 5.008;

    my $script_filter = PAR::Filter->new( @{ opt(f) } ) if opt(f);

    foreach my $input (@Input) {
        my $name = basename($input);
        $size += -s $input;

        if ($script_filter) {
            $zip->addString(
                $script_filter->apply($input, $name) => "script/$name",
                %zip_args,
            );
        }
        else {
            $zip->addFile($input => "script/$name");
        }

        $manifest{"script/$name"}++;
    }

    my $shlib = "shlib/$Config{archname}";
    $zip->addDirectory('', $shlib) if @SharedLibs and $] >= 5.008;
    foreach my $input (@SharedLibs) {
        next unless -e $input;
        $size += -s $input;

        my $name = basename($input);
        vprint 2, "... adding $input as $shlib/$name";
        $zip->addFile($input => "$shlib/$name");
        $manifest{"$shlib/$name"}++;
    }

    foreach my $input (@data) {
	unless (-r $input and !-d $input) {
	    warn "'$input' does not exist or is not readable; skipping\n";
	    next;
	}
        my $name = basename($input);
        $size += -s $input;

        $zip->addFile($input => $name);
        $manifest{$name}++;
    }

    if (@Input) {
        $zip->addString(
            ((@Input == 1)
                ? _main_pl_single("script/" . basename($Input[0]))
                : _main_pl_multi()) => "script/main.pl", %zip_args
        );
        $manifest{"script/main.pl"}++;
    }

    my $dist_name = (opt(p) ? $par_file : $Output);
    my $manifest    = join("\n", '    <!-- accessible as jar:file:///NAME.par!/MANIFEST in compliant browsers -->', (sort keys %manifest), q(    # <html><body onload="var X=document.body.innerHTML.split(/\n/);var Y='<iframe src=&quot;META.yml&quot; style=&quot;float:right;height:40%;width:40%&quot;></iframe><ul>';for(var x in X){if(!X[x].match(/^\s*#/)&&X[x].length)Y+='<li><a href=&quot;'+X[x]+'&quot;>'+X[x]+'</a>'}document.body.innerHTML=Y">));
    my $meta_yaml   = << "YAML";
build_requires: {}
conflicts: {}
dist_name: $dist_name
distribution_type: par
dynamic_config: 0
generated_by: 'Perl Packager version $VERSION'
license: unknown
par:
  cleartemp: 0
  signature: ''
  verbatim: $verbatim
  version: $PAR::VERSION
YAML

    $size += length($_) for ($manifest, $meta_yaml);
    vprint 2, "... making $_" for qw(MANIFEST META.yml);

    $zip->addString($manifest   => 'MANIFEST', %zip_args);
    $zip->addString($meta_yaml  => 'META.yml', %zip_args);
    if ($old_member) {
        $zip->overwrite;
    }
    else {
        $zip->writeToFileNamed($par_file);
    }

    my $newsize = -s $par_file;
    vprint 2, sprintf(
        "*** %s: %d bytes read, %d compressed, %2.2d%% saved.\n",
        $par_file, $size, $newsize, (100 - ($newsize / $size * 100))
    );

    if ( opt('s') ) {
        if (eval {
            require PAR::Dist; require Module::Signature; Module::Signature->VERSION >= 0.25
        }) {
            vprint 0, "Signing $par_file";
            PAR::Dist::sign_par($par_file);
        }
        else {
            vprint -1, "*** Signing requires PAR::Dist with Module::Signature 0.25 or later.  Skipping";
        }
    }

    par_to_exe() unless opt(p);
    
    if ($lose) {
        vprint 2, "Unlinking $par_file";
        unlink $par_file or _die("Can't unlink $par_file: $!"); 
    }
}

sub _name2moddata {
    my ($name, $mod, $dat) = @_;
    if ($name =~ /^[\w:]+$/) {
        $name =~ s/::/\//g;
        push @$mod, "$name.pm";
    }
    elsif ($name =~ /\.(?:pm|ix|al)$/i) {
        push @$mod, $name;
    }
    else {
        push @$dat, $name;
    }
}

sub par_to_exe {
    my $parl = 'parl';
    $parl = 'parldyn' if (opt(d) and $dynperl);
    $parl .= $Config{_exe};
    $parl = 'par.pl' if opt(P);
    $PARL ||= _can_run($parl, opt(P)) or _die("Can't find par loader");

    my $orig_parl = $PARL;
    my $do_unlink;
    if (!opt(p) and opt(i) and $^O eq 'MSWin32' and Win32::IsWinNT() and
        my $replace_icon = _can_run("replaceicon.exe")
    ) {
        my $cfh;
        local $/;
        open _FH, $PARL or die $!;
        binmode(_FH);
        ($cfh, $PARL) = tempfile("parlXXXX", SUFFIX => ".exe"); 
        binmode($cfh);
        print $cfh <_FH>;
        close $cfh;
        vprint 1, "Adding icon to $Output";
        my $cmd = join(
            ' ', map Win32::GetShortPathName($_),
            $replace_icon, $PARL, opt(i)
        );

        `$cmd`;

        my $buf;
        seek _FH, -8, 2;
        read _FH, $buf, 8;
        die unless $buf eq "\nPAR.pm\n";
        seek _FH, -12, 2;
        read _FH, $buf, 4;
        seek _FH, -12 - unpack("N", $buf), 2;
        open $cfh, ">>", $PARL or die $!;
        binmode($cfh);
        print $cfh <_FH>;
        close $cfh;

        $do_unlink = 1;
    }

    my @args = ('-B', "-O$Output", $par_file);
    unshift @args, '-q' unless opt(v);
    if (opt(P)) {
        unshift @args, $PARL;
        $PARL = $^X;
    }
    vprint 0, "Running $PARL @args";
    system($PARL, @args);

    if (opt(g) and $^O eq 'MSWin32') {
        vprint 1, "Fixing $Output to remove its console window";
        strip_console($Output);
        if ($dynperl and !opt(d)) {
            # we have a static.exe that needs taking care of.
            my $buf;
            open _FH, $orig_parl or die $!;
            binmode _FH;
            seek _FH, -8, 2;
            read _FH, $buf, 8;
            die unless $buf eq "\nPAR.pm\n";
            seek _FH, -12, 2;
            read _FH, $buf, 4;
            seek _FH, -12 - unpack("N", $buf) - 4, 2;
            read _FH, $buf, 4;
            strip_console($Output, unpack("N", $buf));
        }
    }

    if ($do_unlink) {
        unlink($PARL);
        unlink("$PARL.bak");
    }
}

sub strip_console {
    my $file = shift;
    my $preoff = shift || 0;
    my ($record, $magic, $signature, $offset, $size);
    open my $exe, "+< $file" or die "Cannot open $file: $!\n";
    binmode $exe;
    seek $exe, $preoff, 0;
    # read IMAGE_DOS_HEADER structure
    read $exe, $record, 64;
    ($magic, $offset) = unpack "Sx58L", $record;

    die "$ARGV[0] is not an MSDOS executable file.\n"
        unless $magic == 0x5a4d; # "MZ"

    # read signature, IMAGE_FILE_HEADER and first WORD of IMAGE_OPTIONAL_HEADER
    seek $exe, $preoff + $offset, 0;
    read $exe, $record, 4+20+2;
    ($signature,$size,$magic) = unpack "Lx16Sx2S", $record;

    die "PE header not found" unless $signature == 0x4550; # "PE\0\0"

    die "Optional header is neither in NT32 nor in NT64 format"
        unless ($size == 224 && $magic == 0x10b) || # IMAGE_NT_OPTIONAL_HDR32_MAGIC
               ($size == 240 && $magic == 0x20b);   # IMAGE_NT_OPTIONAL_HDR64_MAGIC

    # Offset 68 in the IMAGE_OPTIONAL_HEADER(32|64) is the 16 bit subsystem code
    seek $exe, $preoff + $offset+4+20+68, 0;
    print $exe pack "S", 2; # IMAGE_WINDOWS
    close $exe;
}

sub generate_code { 
    vprint 0, "Compiling @Input";
    if (check_par($Input[0])) {
	# invoked as "pp foo.par" - never unlink it
	$par_file = $Input[0];
	$Options->{S} = 1;
        par_to_exe();
    }
    else {
        compile_par();
    }
    exit(0) if (!opt('r'));
}

sub run_code {
    vprint 0, "Running code";
    $Output = File::Spec->catfile(".", $Output);
    system($Output, @ARGV);
    exit(0);
}

sub vprint ($@) {
    my $level = shift;
    my $msg = "@_";
    $msg .= "\n" unless substr($msg, -1) eq "\n";
    if (opt(v) > $level) {
        print        "$0: $msg" if !opt(L);
        print $logfh "$0: $msg" if  opt(L);
    }
}

sub parse_argv {
    Getopt::Long::Configure("no_ignore_case");

    # no difference in exists and defined for %ENV; also, a "0"
    # argument or a "" would not help cc, so skip
    unshift @ARGV, split ' ', $ENV{PP_OPTS} if $ENV{PP_OPTS};

    $Options = {};
    Getopt::Long::GetOptions( $Options,
        'M|add:s@',         # Include modules
        'B|bundle',         # Bundle core modules
        'd|dependent',      # Do not package libperl
        'e|eval:s',         # Packing one-liner
        'X|exclude:s@',     # Exclude modules
        'f|filter:s@',      # Input filters for scripts
        'g|gui',            # No console window
        'h|help',           # Help me
        'i|icon:s',         # Icon file
        'N|info',           # Executable header info
        'I|lib:s@',         # Include directories (for perl)
        'l|link:s@',        # Include additional shared libraries
        'L|log:s',          # Where to log packaging process information
        'F|modfilter:s@',   # Input filter for perl modules
        'm|multiarch',      # Build multiarch PAR file
        'o|output:s',       # Output file
        'p|par',            # Generate PAR only
        'P|perlscript',     # Generate perl script
        'r|run',            # Run the resulting executable
        'S|save',           # Preserve intermediate PAR files
        's|sign',           # Sign PAR files
        'v|verbose:s',      # Verbosity level
        'V|version',        # Show version
    );

    $Options->{p} = 1 if opt('m');
    $Options->{v} = 1 if exists $Options->{v} and $Options->{v} eq '';
    $Options->{B} = 1 unless opt(p) or opt(P);

    helpme() if opt(h); # And exit
    show_version() if opt(V); # And exit

    $Output = opt(o) || (
	'a' . ($Config{_exe} || '.out') . (opt(P) ? '.pl' : '')
    );
    open $logfh, '>>', opt(L) or die ("XXX: Cannot open log: $!") if (opt(L));

    if (opt(e)) {
        warn "$0: using -e 'code' as input file, ignoring @ARGV\n" if @ARGV and !opt(r);
        my ($fh, $fake_input) = tempfile("ppXXXXX", SUFFIX => ".pl", UNLINK => 1); 
        print $fh $Options->{e};
        close $fh;
        @Input = $fake_input;
    }
    else {
        @Input = shift @ARGV if @ARGV;
        _die("$0: No input files specified\n") unless @Input or opt(M);
        push @Input, @ARGV if @ARGV and !opt(r);
        check_read(@Input) if @Input;
        check_perl(@Input) if @Input;
        sanity_check();
    }
}

sub opt(*) {
    my $opt = shift;
    return exists($Options->{$opt}) && ($Options->{$opt} || 0);
} 

sub sanity_check {
    # Check the input and output files make sense, are read/writable.
    if ("@Input" eq $Output) {
        if ("@Input" eq 'a.out') {
            _die("$0: Compiling a.out is probably not what you want to do.\n");
            # You fully deserve what you get now. No you *don't*. typos happen.
        } else {
            warn "$0: Will not write output on top of input file, ",
                "compiling to a.out instead\n";
            $Output = "a.out";
        }
    }
}

sub check_read { 
    foreach my $file (@_) {
        unless (-r $file) {
            _die("$0: Input file $file is a directory, not a file\n") if -d _;
            unless (-e _) {
                _die("$0: Input file $file was not found\n");
            } else {
                _die("$0: Cannot read input file $file: $!\n");
            }
        }
        unless (-f _) {
            # XXX: die?  don't try this on /dev/tty
            warn "$0: WARNING: input $file is not a plain file\n";
        } 
    }
}

sub check_write {
    foreach my $file (@_) {
        if (-d $file) {
            _die("$0: Cannot write on $file, is a directory\n");
        }
        if (-e _) {
            _die("$0: Cannot write on $file: $!\n") unless -w _;
        } 
        unless (-w cwd()) { 
            _die("$0: Cannot write in this directory: $!\n");
        }
    }
}

sub check_perl {
    my $file = shift;
    return if check_par($file);

    unless (-T $file) {
        warn "$0: Binary `$file' sure doesn't smell like perl source!\n";
        if (my $file_checker = _can_run("file")) {
            print "Checking file type... ";
            system($file_checker, $file);
        }
        _die("Please try a perlier file!\n");
    } 

    open(my $handle, "<", $file) or _die("XXX: Can't open $file: $!");
    local $_ = <$handle>;
    if (/^#!/ && !/perl/) {
        _die("$0: $file is a ", /^#!\s*(\S+)/, " script, not perl\n");
    } 

} 

sub check_par {
    my $file = shift or return;
    open(my $handle, "<", $file) or _die("XXX: Can't open $file: $!");
    binmode($handle);
    local $/ = \4;
    return (<$handle> eq "PK\x03\x04");
}

sub helpme {
    print "Perl Packager, version $VERSION (PAR version $PAR::VERSION)\n\n";
    {
        no warnings;
        exec "pod2usage $0";
        exec "perldoc $0";
        exec "pod2text $0";
    }
}

sub show_version {
    print << ".";
Perl Packager, version $VERSION (PAR version $PAR::VERSION)
Copyright 2002, 2003 by Autrijus Tang <autrijus\@autrijus.org>

Neither this program nor the associated "parl" program impose any
licensing restrictions on files generated by their execution, in
accordance with the 8th article of the Artistic License:

    "Aggregation of this Package with a commercial distribution is
    always permitted provided that the use of this Package is embedded;
    that is, when no overt attempt is made to make this Package's
    interfaces visible to the end user of the commercial distribution.
    Such use shall not be construed as a distribution of this Package."

Therefore, you are absolutely free to place any license on the resulting
executable, as long as the packed 3rd-party libraries are also available
under the Artistic License.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.  There is NO warranty; not even for
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

.
    exit;
}

sub pod_strip {
    my ($pl_text, $filename) = @_;

    no warnings 'uninitialized';

    my $data = '';
    $data = $1 if $pl_text =~ s/((?:^__DATA__$).*)//ms;

    my $line = 1;
    if ($pl_text =~ /^=(?:head\d|pod|begin|item|over|for|back|end)\b/) {
        $pl_text = "\n$pl_text";
        $line--;
    }
    $pl_text =~ s{(
	(.*?\n)
	=(?:head\d|pod|begin|item|over|for|back|end)\b
	.*?\n
	(?:=cut[\t ]*[\r\n]*?|\Z)
	(\r?\n)?
    )}{
	my ($pre, $post) = ($2, $3);
        "$pre#line " . (
	    $line += ( () = ( $1 =~ /\n/g ) )
	) . $post;
    }gsex;
    $pl_text = '#line 1 "' . ($filename) . "\"\n" . $pl_text
        if length $filename;
    $pl_text =~ s/^#line 1 (.*\n)(#!.*\n)/$2#line 2 $1/g;

    return $pl_text . $data;
}

sub _die {
    $logfh->print(@_) if opt(L);
    die @_;
}

sub _find_shlib {
    my $file = shift;
    return $file if -e $file;

    for my $dir (
        File::Basename::dirname($0),
        split(/\Q$Config{path_sep}\E/, $ENV{$Config{ldlibpthname}})
    ) {
        my $abs = File::Spec->catfile($dir, $file);
        return $abs if -e $abs;
        $abs = File::Spec->catfile($dir, "$file.$Config{dlext}");
        return $abs if -e $abs;
    }

    # be extra magical and prepend "lib" to the filename
    return _find_shlib("lib$file") unless $file =~ /^lib/;
}

sub _can_run {
    my ($command, $no_exec) = @_;

    for my $dir (
        File::Basename::dirname($0),
        split(/\Q$Config{path_sep}\E/, $ENV{PATH})
    ) {
        my $abs = File::Spec->catfile($dir, $command);
        return $abs if $no_exec or $abs = MM->maybe_command($abs);
    }

    return;
}

sub _main_pl_multi {
    return << '__MAIN__';
my $file = $0;
my $zip = $PAR::LibCache{$0} || Archive::Zip->new(__FILE__);
$file =~ s/^.*[\/\\]//;
$file =~ s/\.[^.]*$//i ;
my $member = eval { $zip->memberNamed($file) }
        || $zip->memberNamed("$file.pl")
        || $zip->memberNamed("script/$file")
        || $zip->memberNamed("script/$file.pl")
    or die qq(Can't open perl script "$file": No such file or directory);
PAR::_run_member($member, 1);

__MAIN__
}

sub _main_pl_single {
    my $file = shift;
    return << "__MAIN__";
my \$zip = \$PAR::LibCache{\$0} || Archive::Zip->new(__FILE__);
my \$member = eval { \$zip->memberNamed('$file') }
    or die qq(Can't open perl script "$file": No such file or directory (\$zip));
PAR::_run_member(\$member, 1);

__MAIN__
}

END {
    unlink $par_file if ($par_file && !opt(S) && !opt(p));
}

__END__

=head1 NAME

pp - Perl Packager

=head1 SYNOPSIS

B<pp> S<[ B<-BILMSVXdeghilmoprsv> ]> S<[ I<parfile> | I<scriptfile> ]>...

=head1 OPTIONS

    % pp hello                  # Pack 'hello' into executable 'a.out'
    % pp -o hello hello.pl      # Pack 'hello.pl' into executable 'hello'

    % pp -o foo foo.pl bar.pl   # Pack 'foo.pl' and 'bar.pl' into 'foo'
    % ./foo                     # Run 'foo.pl' inside 'foo'
    % mv foo bar; ./bar         # Run 'bar.pl' inside 'foo'
    % mv bar baz; ./baz         # Error: Can't open perl script "baz"

    % pp -p file                # Creates a PAR file, 'file.par'
    % pp -o hello file.par      # Pack 'file.par' to executable 'hello'
    % pp -S -o hello file       # Combine the two steps above

    % pp -p -o out.par file     # Creates 'out.par' from 'file'
    % pp -B -p -o out.par file  # same as above, but bundles core modules
    % pp -P -o out.pl file      # Creates 'out.pl' from 'file'
    % pp -B -p -o out.pl file   # same as above, but bundles core modules
                                # (-B is assumed when making executables)

    % pp -e 'print q//'         # Pack a one-liner into 'a.out'
    % pp -p -e 'print q//'      # Creates a PAR file 'a.out.par'

    % pp -I /foo hello          # Extra paths (notice space after -I)
    % pp -M Foo::Bar hello      # Extra modules (notice space after -M)
    % pp -M abbrev.pl hello     # Extra files under @INC
    % pp -X Foo::Bar hello      # Exclude modules (notice space after -X)

    % pp -r hello               # Pack 'hello' into 'a.out', runs 'a.out'
    % pp -r hello a b c         # Pack 'hello' into 'a.out', runs 'a.out'
                                # with arguments 'a b c' 

    % pp hello --log=c          # Pack 'hello' into 'a.out', logs
                                # messages into 'c'

    # Pack 'hello' into a console-less 'out.exe' with icon (Win32 only)
    % pp --gui --icon hello.ico -o out.exe hello

=head1 DESCRIPTION

F<pp> creates standalone executables from Perl programs, using the
compressed packager provided by L<PAR>, and dependency detection
heuristics offered by L<Module::ScanDeps>.  Source files are compressed
verbatim without compilation.

You may think of F<pp> as "F<perlcc> that works without hassle". :-)

A GUI interface is also available as the F<tkpp> command.

It does not provide the compilation-step acceleration provided by
F<perlcc> (however, see B<-f> below for byte-compiled, source-hiding
techniques), but makes up for it with better reliability, smaller
executable size, and full retrieval of original source code.

When a single input program is specified, the resulting executable will
behave identically as that program.  However, when multiple programs
are packaged, the produced executable will run the one that has the
same basename as C<$0> (i.e. the filename used to invoke it).  If
nothing matches, it dies with the error C<Can't open perl script "$0">.

On Microsoft Windows platforms, F<a.exe> is used instead of F<a.out>
as the default executable name.

=head1 OPTIONS

Options are available in a I<short> form and a I<long> form.  For
example, the three lines below are all equivalent:

    % pp -o output.exe input.pl
    % pp --output output.exe input.pl
    % pp --output=output.exe input.pl

=over 4

=item B<-M>, B<--add>=I<MODULE>|I<FILE>

Adds the specified module into the package, along with its dependencies.
Also accepts filenames relative to the C<@INC> path; i.e. C<-M
Module::ScanDeps> means the same thing as C<-M Module/ScanDeps.pm>.

If I<FILE> does not have a C<.pm>/C<.ix>/C<.al> extension, it will not
be scanned for dependencies, and will be placed under C</> instead of
C</lib/> inside the PAR file.

=item B<-B>, B<--bundle>

Bundles core modules in the resulting package.  This option is enabled
by default, except when C<-p> or C<-P> is specified.

=item B<-d>, B<--dependent>

Reduce the executable size by not including a copy of perl interpreter.
Executables built this way will need a separate F<perl5x.dll>
or F<libperl.so> to function correctly.  This option is only available
if perl is built as a shared library.

=item B<-e>, B<--eval>=I<STRING>

Package a one-liner, much the same as C<perl -e '...'>

=item B<-X>, B<--exclude>=I<MODULE>

Excludes the given module from the dependency search patch and from the
package.

=item B<-f>, B<--filter>=I<FILTER>

Filter source script(s) with a L<PAR::Filter> subclass.  You may specify
multiple such filters.

If you wish to hide the source code from casual prying, this will do:

    % pp -f Bleach source.pl

Users with Perl 5.8.1 and above may also try out the experimental
byte-compiling filter, which will strip away all comments and indents:

    % pp -f Bytecode source.pl

=item B<-g>, B<--gui>

Build an executable that does not have a console window. This option is
ignored on non-MSWin32 platforms or when C<-p> is specified.

=item B<-h>, B<--help>

Shows basic usage information.

=item B<-i>, B<--icon>=I<FILE>

Specify an icon file for the executable. This option is ignored on
non-MSWin32 platforms or when C<-p> is specified.

=item B<-N>, B<--info>=I<KEY=VAL>

Add additional information for the packed file, both in C<META.yml>
and in the executable header (if applicable).  The name/value pair is
separated by C<=>.  You may specify C<-N> multiple times.

For Win32 executables, these special C<KEY> names are recognized:

    Comments        CompanyName     FileDescription FileVersion
    InternalName    LegalCopyright  LegalTrademarks OriginalFilename
    ProductName     ProductVersion

This feature is currently unimplemented.

=item B<-I>, B<--lib>=I<DIR>

Adds the given directory to the perl library file search path.

=item B<-l>, B<--link>=I<FILE>|I<LIBRARY>

Adds the given shared library (a.k.a. shared object or DLL) into the
packed file.  Also accepts names under library paths; i.e.
C<-l ncurses> means the same thing as C<-l libncurses.so> or
C<-l /usr/local/lib/libncurses.so> in most Unixes.

=item B<-L>, B<--log>=I<FILE>

Log the output of packaging to a file rather than to stdout.

=item B<-F>, B<--modfilter>=I<FILTER>

Filter included perl module(s) with a L<PAR::Filter> subclass.
You may specify multiple such filters.

=item B<-m>, B<--multiarch>

Build a multi-architecture PAR file.  Implies B<-p>.

=item B<-o>, B<--output>=I<FILE>

Specifies the file name for the final packaged executable.

=item B<-p>, B<--par>

Create PAR archives only; do not package to a standalone binary.

=item B<-P>, B<--perlscript>

Create stand-alone perl script; do not package to a standalone binary.

=item B<-r>, B<--run>

Run the resulting packaged script after packaging it.

=item B<-S>, B<--save>

Do not delete generated PAR file after packaging.

=item B<-s>, B<--sign>

Cryptographically sign the generated PAR or binary file using
L<Module::Signature>.

=item B<-v>, B<--verbose>[=I<NUMBER>]

Increase verbosity of output; I<NUMBER> is an integer from C<0> to C<5>,
C<5> being the most verbose.  Defaults to C<1> if specified without an
argument.

=item B<-V>, B<--version>

Display the version number and copyrights of this program.

=back

=head1 ENVIRONMENT

=over 4

=item PP_OPTS

Command-line options (switches).  Switches in this variable are taken
as if they were on every F<pp> command line.

=back

=head1 NOTES

Here are some recipes showing how to utilize F<pp> to bundle
F<source.pl> with all its dependencies, on target machines with
different expected settings:

=over 4

=item Stand-alone setup

    % pp -o packed.exe source.pl        # makes packed.exe
    # Now, deploy 'packed.exe' to target machine...
    $ packed.exe                        # run it

=item Perl interpreter only, without core modules:

    % pp -B -P -o packed.pl source.pl   # makes packed.exe
    # Now, deploy 'packed.exe' to target machine...
    $ perl packed.pl                    # run it

=item Perl with core module installed:

    % pp -P -o packed.pl source.pl      # makes packed.exe
    # Now, deploy 'packed.pl' to target machine...
    $ perl packed.pl                    # run it

=item Perl with PAR.pm and its dependencies installed:

    % pp -p source.pl                   # makes source.par
    % echo "use PAR 'source.par';" > packed.pl;
    % cat source.pl >> packed.pl;       # makes packed.pl
    # Now, deploy 'source.par' and 'packed.pl' to target machine...
    $ perl packed.pl                    # run it

=back

Note that even if your perl was built with a shared library, the
'Stand-alone setup' above will I<not> need a separate F<perl5x.dll>
or F<libperl.so> to function correctly.  Use C<--dependent> if you
are willing to ship the shared library with the application, which
can significantly reduce the executable size.

=head1 SEE ALSO

L<tkpp>, L<par.pl>, L<parl>, L<perlcc>

L<PAR>, L<Module::ScanDeps>

=head1 ACKNOWLEDGMENTS

Simon Cozens, Tom Christiansen and Edward Peschko for writing
F<perlcc>; this program try to mimic its interface as close
as possible, and copied liberally from their code.

Jan Dubois for writing the F<exetype.pl> utility, which has been
partially adapted into the C<-g> flag.

Mattia Barbon for providing the C<myldr> binary loader code.

Jeff Goff for suggesting the name C<pp>.

=head1 AUTHORS

Autrijus Tang E<lt>autrijus@autrijus.orgE<gt>

L<http://par.perl.org/> is the official PAR website.  You can write
to the mailing list at E<lt>par@perl.orgE<gt>, or send an empty mail to
E<lt>par-subscribe@perl.orgE<gt> to participate in the discussion.

Please submit bug reports to E<lt>bug-par@rt.cpan.orgE<gt>.

=head1 COPYRIGHT

Copyright 2002, 2003 by Autrijus Tang E<lt>autrijus@autrijus.orgE<gt>.

Neither this program nor the associated L<parl> program impose any
licensing restrictions on files generated by their execution, in
accordance with the 8th article of the Artistic License:

    "Aggregation of this Package with a commercial distribution is
    always permitted provided that the use of this Package is embedded;
    that is, when no overt attempt is made to make this Package's
    interfaces visible to the end user of the commercial distribution.
    Such use shall not be construed as a distribution of this Package."

Therefore, you are absolutely free to place any license on the resulting
executable, as long as the packed 3rd-party libraries are also available
under the Artistic License.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=cut
