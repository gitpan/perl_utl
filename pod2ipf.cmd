extproc perl -wS
#!perl -w
use strict qw(refs subs);
use File::Find;
use File::Copy 'copy';
use Cwd;
use Config '%Config';
use Getopt::Long 'GetOptions';
use vars qw{%do_file_hash %do_dirs_hash %bin_hash %faqs_hash %pragmas_hash
	    %mods_hash %pod_hash %add_mods_hash @add_dir @add_files %tree_hash
            $skip_embedded_links};
# use Fatal qw(open close); # would not work: interprets filehandles as barewords.
sub intern_modnamehash;
sub do_libdir;
sub output_file;
sub hash_diff;
sub auto_beautify;
sub create_tree;
sub format_args;
sub output_index;
sub count_index;
sub untabify;
sub untabify_after;
sub strip;
sub find_parent;
sub insert_back;

require 5.004;			# Otherwise pos() in recursive sub cores.

#require 'dumpvar.pl';

# $rcs = ' $Id: pod2ipf.cmd,v 1.34 2003/12/17 06:31:14 vera Exp vera $ ' ;
($VERSION) = (' $Revision: 1.34 $ ' =~ /(\d+(\.\d+)*)/);

# by Marko.Macek@snet.fri.uni-lj.si, mark@hermes.si
# and Ilya Zakharevich ilya@math.ohio-state.edu
#
# TODO:
#   eliminate blank panes that link to next pane when =item XX\n\n=itemYY used
#   rewrite ?<> parsing
#   better index (mostly done)
#   cleaner xref heuristics (mostly done)
#   process embeded pods (done)
#   IPFC doesn't seem to handle tabs - are
#   handle perl/SYNOPSIS properly (tabs, indented lines) -- or is it a bug in doc
#     probably should process as pre but with markup -- done below (ok?)
#   remove =head1 NAME and use it as toplevel heading (done)
#     (also collapse DESCRIPTION if the only section).
#   pod2ipf needs to be split into index generator and translator
#     this should enable separate translation of each .pod
#     and use of .INF concatenation to view the full docs together
#     (with linking if index was used).
#     IPF requires numerical references when concatenation is used, not symbolic :-(
#   improved handling of windows (started to be done)
#   ...
#
#  Changes:
#
#  10.2: parml used instead of ul if needed.
#  	Readability of .ipf improved.
#  	BI partially supported, F<> and C<> are distinct.
#	Some options supported.
#
# 10.3:	C<> works again. 
#	--head_off works (0 and 1).
#
# 10.4:	Auto-beautifies some words, vars and functions, finds links
#	--head_off works (0 and 1 and 2).
#
# 10.5:	--section-name works
#	Bugs with modules in subdirectories corrected.
#	--about works.
#	Better auto-crosslinking.
#
#	2-level indices. (Since we do not know until the second pass
#	whether we put something into an index, there are some false
#	positives.)
#	Handles tabs (checked with Tk).
#	Secondary names used for crosslinking.
# 11:   Additional logic for links like C<-M>.
#	Will process ../INSTALL and ../Porting/pumpkin.pod as well.
#	Additional argument --bin-dir.
#	Support for WWW links via lynx.
#
# Use of uninit value comes from findrefid for pod2ipf it it is not present.
# 1.5:  Add back perltoc - have refs to it.
#	:i[12] tags are shortened to avoid segfaults.
# 1.6:	`pod2ipf myfile.pod' works again;
#	--www added.
#	<-processing could use substr with negative length for C<<=>.
#	Index entries X<> handled (invisible).
#	Will not produce links to "contents" pages from higher level
#	  contents pages (YES!).
# 1.7:	Add implicit links for targets defined by C<>.
#	Uplinks added.
# 1.8:	Will not reference -8.
#	out[12] removed, substituted by 'require 5.004'.

# 1.9:  Better handling of 'Go up' (last item was going to the following
#	section).

# 1.10: Misprint...
#	Supports L<foo|bar>
#	Do not index/create refs for perltoc.
#	Numeration of panes fixed to be not off-by-one.  (Minor bugs may be created for writing TOC sections and "Go up" links.)

# 1.11: Remove (last piece?) of voodoo - in contents() - it did not work anyway
#	Allow links to text containing L<> inside.

# 1.12: Allow for --section-name with --file too;
#	Wrap arguments in "About";
#	--believe-pod-name implemented.
#	--alias implemented.

# 1.13: One more piece of voodoo for backrefs removed.

# 1.14: @add_dir was misspelled

# 1.15: Looking for leading whitespace was done with \s instead of [ \t].
#  Does not help with "By Fermat's little theorem" in PARI tutorial.

# 1.16: Add some more E<name> escapes

# 	$rcs = ' $Revision: 1.34 $ ' ;
# $Log: pod2ipf.cmd,v $
# Revision 1.34  2003/12/17 06:31:14  vera
# Last uninitialized value warning removed.
# Include os2/Changes.
# Update perlos2.pod if needed.
# Work better with layout of "inherited" directories.
#
# Revision 1.33  2003/05/25 08:34:59  vera
# Tries harder to make links for function calls as Func();
# works at least if X<Func> is defined somewhere.
#
# Revision 1.32  2001/05/26 08:06:31  vera
# $Revision was mispelled, so not updated. [wrong case]
#
# Revision 1.31  2001/05/22 19:11:59  vera
# Omited entries in perlfunc, perlvar restored; brush up the docs.
#
# Revision 1.30  2001/05/22 10:33:27  vera
# Warnings 306 gone, but the content of perlfunc became much shorter.
#
# Revision 1.29  2001/05/22 10:06:42  vera
# Add a pod2ipf ref from About; no need to add xsubpp; most "Strip conversion
# warnings" are correct now.
#
# Revision 1.28  2001/05/22 08:57:19  vera
# Find pod2ipf too; Avoid double "Go Up"; allow .pl, .plx
# and no extension at all; do not leave NAME due only to
# long description.
#
# Revision 1.27  2001/05/16 10:04:04  vera
# Remove the undefined reference in $index_seen{$heading}; remove too
# agressive case conversion in auto-linking foo(2), better report of
# auto-linking Carp to carp.
#
# Revision 1.26  2001/05/16 09:15:24  vera
# More aggressive recognition of longjmp(3) as of a crosslink; more
# permissive recognition of links with different whitespacing, case
# conversion, and tags.
#
# Revision 1.25  2001/05/15 21:37:12  vera
# toc=12345 instead of 5; split long lines (Warning 206); debugging
# output to catch undefined $index_seen{$heading}; link inside a link
# was possible (warning 208); leading period was not completely removed
# (warning 208).
#
# Warnings remaining:  Warning 306: Missing panel text in head level tag
# 		     Warning 117: Duplicate text in tag
# 		     Warning 121: Invalid head level [h4]	(?!)
# 		     Warning 110: No ID for this reference [4387]
#
# Revision 1.24  2001/05/15 07:30:53  vera
# Better output of About section; auto-increment $VERSION; would output ::link.
#
# Revision 1.23  2001/05/15 07:12:43  vera
# Instead of treating files with only the NAME section differently,
# added processing of the empty last POD section in the file.
#
# Revision 1.22  2001/05/15 06:23:28  vera
# xsubpp was added too aggressively; Better prefix for last resort
# description; avoid an uninit warning; the URL was removed in
# out($txt,1); no not skip NAME if nothing else is present;
#
# Revision 1.21  2001/05/15 04:25:42  vera
# Add a flexible multi-browser support.
#
# Revision 1.20  2001/05/15 01:27:37  vera
# Revision log added.
#

#revision 1.19   locked by: vera;
#date: 2001/05/13 10:51:13;  author: vera;  state: Exp;  lines: +3 -2
#Remove duplicate Pumpkin entry.
#----------------------------
#revision 1.18
#date: 2001/05/13 06:01:58;  author: vera;  state: Exp;  lines: +3 -3
#Better support for =begin/=end.
#----------------------------
#revision 1.17
#date: 2001/05/13 05:59:09;  author: vera;  state: Exp;  lines: +17 -10
#Added support for =begin, =end, =for (with no tags supported).
#----------------------------
#revision 1.16
#date: 2001/05/13 05:30:21;  author: vera;  state: Exp;  lines: +69 -59
#Works.  Courier size made tolerable on both 72 and 96 dpi.
#Unresolved link C<< (?>pattern) >>...

$font = ''; #':font facename=Helv size=16x8.';

$debug = 0;
$debug_xref = 0;
$dump_xref = 0;
$dump_contents = 0;
$dump_manpages = 1;
$maxtoc = '12345';
$dots = 0;
$multi_win = 1;     # 1 = use alternate window for toc
@do_dirs = ();
@do_file = ();
@bin_dirs = ();
$do_burst = 1;
$do_about = 1;
$do_bin = 1;
$do_mods = 1;
$do_std = 1;
$head_off = 2;
$do_tree = 1;
$do_faqs = 1;
$by_files = $by_dirs = 0;
@add_dir = ();
@add_files = ();
my @args = @ARGV;
my $foundrefs = 0;
my %i1ids;
my %index_seen;
my %index_output;
my $www = '%IPFWWW%||lynx||netscape';
#my $to_C = ':font facename=Courier size=18x10.';
#my $to_C = ':font facename=Courier size=14x8.';
my $to_C = ':font facename=Courier size=11x6.';	# Tolerable on 72/96dpi.
my $from_C = ':font facename=default size=0x0.'; # Documented: default size
@make_bold = qw(EMX RSX WPS Object-REXX HPFS HTML WWW GNU Perl C
		XFree86 OS/2 CRT PM DOS VIO CPAN IBM URL);
@make_code = qw(VCPI DPMI groff awk gawk STDIN STDOUT STDERR Emacs EPM
		CMD 4os2 sh pdksh zip unzip pkunzip man gcc link386 tr
		PATH LIBPATH);
my %in_INC;
@in_INC{@INC} = @INC;

sub by_dirs { by_files();   $by_files = 0;  $by_dirs = 1; }
sub by_files {
  $dump_manpages = 0;
  $do_burst = 0;
  $do_bin = 0;
  $do_mods = 0;
  $do_std = 0;
#  $head_off = 0;
  $do_tree = 0;
  $do_faqs = 0;
  $by_files = 1;
  $by_dirs = 0;
}

%cat_descr = (
	      pod => 'Perl documentation',
	      faqs => 'Frequently asked questions',
	      bin => 'Perl utilities (with POD documentation)',
	      mods => 'Standard Perl modules',
	      add_mods => 'Additional Perl modules',
	      do_file => 'Additional modules',
	      do_dirs => 'Additional directories',
	      tree => 'Hierarchy of documented perl modules',
	      pragmas => 'Pragmata: change Perl\'s behaviour',
	     );

sub add_dir {			# If without args, just finish processing
  my $name = $_[1];
  print STDERR "Starting section `$name'.\n" if @_ and $debug;
  if (@do_dirs) {
    @add_dir = ($cat_descr{do_dirs},[],[]) unless @add_dir;
    push @{$add_dir[-2]}, @do_dirs;
    @do_dirs = ();
  }
  if (@do_file) {
    @add_dir = ($cat_descr{do_dirs},[],[]) unless @add_dir;
    push @{$add_dir[-1]}, @do_file;
    @do_file = ();
  }
  push @add_dir, $name, [], [] if @_;
  push @add_files, $name, [], [] if @_;
}

sub do_alias {
  my $alias = $_[1];
  warn("Ignoring alias $alias: no file name found"), return unless @do_file;
  $alias{$do_file[-1]} = $alias;
}

if (@ARGV >= 1 and $ARGV[0] !~ /^-/) {
    unshift @ARGV, '--by-files';
    unshift @ARGV, '--head-off=0' if @ARGV == 2;
}

GetOptions(
	   "debug!" => \$debug,
	   "burst!" => \$do_burst, # Print Logo page
	   "about!" => \$do_about, # Print About page
	   "mods!" => \$do_mods, # Scan through @INC
	   "std!" => \$do_std,	# Scan through standard Perl PODs in ./
	   "bin!" => \$do_bin,	# Scan through $Config{bin}
	   "tree!" => \$do_tree, # Output tree
	   "faqs!" => \$do_faqs, # Output faqs
	   "file=s@" => \@do_file, # If present, do these files too
	   "dir=s@" => \@do_dirs, # Which addnl directories to scan
	   "dump_xref!" => \$dump_xref,	# Dump them to STDERR
	   "dump-contents!" => \$dump_contents,	# Dump it to STDERR
	   "dump-manpages!" => \$dump_manpages,	# Dump unknown to STDERR
	   "title=s" => \$DocTitle,
	   "head-off=i" => \$head_off,
	   "to-bold=s@" => \@make_bold,
	   "to-code=s@" => \@make_code,
	   "by-files" => \&by_files,
	   "by-dirs" => \&by_dirs,
	   "www" => \$www,	# Browser
	   "to-C" => \$to_C,	# How to switch to CODE sections
	   "section-name=s" => \&add_dir,
	   "alias=s" => \&do_alias,
	   "bin-dir=s@" => \@bin_dirs, # If present, search for bins here too
	   "believe-pod-name!" => \$believe_pod_name, # Believe NAME section
	  );
if ($by_dirs) {
  push @do_dirs, @ARGV;
} elsif ($by_files) {
  push @do_file, @ARGV;
} else {
  warn "Ignoring \@ARGV: `@ARGV'.\n" if @ARGV;
}

$DocTitle = "Perl $Config{version} Manual"
  unless defined $DocTitle or @do_dirs or @do_file;
my $unknownPod_prefix = $do_std ? undef : "File ";

sub unknownPod {
    my $ftitle = shift;
    my $fname = $modnamehash{$ftitle};
    my $pref = $unknownPod_prefix;
    unless (defined $pref) {
	$pref = "Perl module" if $fname =~ /\.pm$/;
	$pref = "Perl script" if $fname =~ /\.(plx?|bat|cmd)$/;
	$pref = "Perl script" if $fname =~ m,[\\/][^\\/.]*$,;
	$pref = "Perl documentation file" if $fname =~ /\.pod$/;
	$pref = "Perl component" unless defined $pref;
    }
    "$pref $ftitle";
}

my @www;

$www =~ s/^\s+//;
$www =~ s/\s+$//;
if ($www =~ /\|\||\s|\@\@\@/) {
  @www = map { /\@\@\@/ ? $_ : $_ . ' "@@@"' } split /\s*\|\|\s*/, $www;
} elsif (! $www =~ /\./) {
  $www .= '.exe'
}

add_dir();
$do_about = 1 if $do_burst;

$make_bold = join '|', @make_bold;
$make_code = join '|', @make_code;

$print_index = 1;		# Do not output index for tables of contents

debug("Module pod/pm discovery");

$curdir = cwd;
my $site_perl_prefix;
my $libdir;

if ((substr $Config{sitelib}, 0, length $Config{privlib}) 
    eq $Config{privlib}) {
  $site_perl_prefix = substr $Config{sitelib}, (length $Config{privlib}) + 1;
  $site_perl_prefix =~ s!\\!/!g ;
}

if (@do_file) {
  foreach $file (@do_file) {
    do_onefile($file);
    # Fake File::Find
#    $File::Find::name = $_ = $file;
#    $libdir = ".";
#    intern_modnamehash();
  }
}
%do_file_hash = %modnamehash;
%old_hash = %modnamehash;

sub do_onefile {
  # Fake File::Find
  local $_ = shift;
  $libdir = shift;
  my $odir;
  if (defined $libdir)  {
    $File::Find::name = "./$_";
    $odir = cwd();
    chdir $libdir or warn "!!! Failed chdir($libdir): $!";
  } else {
    $File::Find::name = $_;
    $libdir = '.';
  }
  intern_modnamehash();
  chdir $odir if defined $odir;
}

{
  no strict 'refs';
  foreach (1 .. @add_dir/3) {
    foreach $libdir (@{$add_dir[3*$_-2]}) {
      do_libdir $libdir;
    }
    foreach $file (@{$add_dir[3*$_-1]}) {
      do_onefile($file);
    }
    print STDERR "Doing section `$_' named `$add_dir[3*$_-3]': dirs `@{$add_dir[3*$_-2]}', files `@{$add_dir[3*$_-1]}'.\n" if $debug;
    %{"do_dirs$ {_}_hash"} = hash_diff(\%old_hash, \%modnamehash);
    $cat_descr{"do_dirs$_"} = $add_dir[3*$_-3];
    %old_hash = %modnamehash;
  }
}

if ($do_mods or $do_faqs) {
  foreach $libdir ( @INC ) {
    do_libdir $libdir;
  }
  %mods_hash = hash_diff(\%old_hash, \%modnamehash);
  %old_hash = %modnamehash;

  my $regex = quotemeta $Config{sitelib};
  foreach $key (keys %mods_hash) {
    next unless $modnamehash{$key} =~ /^$regex/o;
    $add_mods_hash{$key} = delete $mods_hash{$key};
  }
}

foreach $libdir ( $do_bin ? ($Config{bin}, @bin_dirs) : () ) {
  do_libdir $libdir;
}
unless (@args) {
  # my @xsubpp = grep -f "$_/xsubpp", map "$_/ExtUtils", @INC;
  # do_onefile('xsubpp', $xsubpp[0]) if @xsubpp;
  my ($dir,$file) = ($0 =~ m,^(.*)[\\/](.*),);
  do_onefile($file, $dir);
}

%bin_hash = hash_diff(\%old_hash, \%modnamehash);

@modnames = sort keys %modnamehash;

print STDERR "\nFound `@modnames'.\n";

# %modnamehash now maps module name -> file name.
# %moddesc now maps module name -> description.

@files = ();

if ($do_std and -f 'perl.pod') {
  open MPOD, 'perl.pod';
  @files = ();
  while (<MPOD>) {
    last if /sections/;
  }
  while (<MPOD>) {
    last if /^[^\s=]/;
    push @files, [$1, $2] 
      if /^\s+(\S*)\s+(.*)/ and $1 ne 'perltoc' and $1 !~ /^perlfaq/;
  }
  close MPOD;
  open MPOD, 'perltoc.pod';
  while (<MPOD>) {
    last if /^=head1\s+pragma/i;
  }
  while (<MPOD>) {
    last if /^=head1/;
    push @pragmas, $1 if /^=head2\s+(?:L<)?(\S*?)>?\s+-\s/; 
  }
  close MPOD;
  foreach $key (@pragmas) {
    $pragmas_hash{$key} = delete $mods_hash{$key};
  }
  @files = grep $_->[0] ne 'perlos2', @files;
  splice @files, 1, 0,
     [ 'perlos2',      'Perl under OS/2' ],
     [ 'perlos2delta', 'Log of changes to OS/2 port of Perl' ],
     [ 'perltoc',      'Internal table of contents for Perl' ];
  push @files, [ 'perlinstall',  'Installation/compilation of Perl'];
  push @files, ['Pumpkin', 'Notes on handling the Perl Patch Pumpkin']
    unless $] >= 5.006;		# Included between modules
  if (-f '../INSTALL' and not -f 'perlinstall.pod') {
      copy '../INSTALL', 'perlinstall.pod';
  }
  if (-f '../Porting/pumpkin.pod' and not -f 'Pumpkin.pod') {
      copy '../Porting/pumpkin.pod', 'Pumpkin.pod';
  }
  if (-f '../README.os2'
      and ( not -f 'perlos2.pod' or (-M '../README.os2') < -M 'perlos2.pod' )) {
      chmod 0666, 'perlos2.pod';
      unlink 'perlos2.pod';
      copy '../README.os2', 'perlos2.pod';
      chmod 0444, 'perlos2.pod';
  }
  if (-f '../os2/Changes' and
      (not -f 'perlos2delta.pod' or (-M '../os2/Changes') < -M 'perlos2delta.pod')) {
     open my $in, '<', '../os2/Changes' or die;
     chmod 0666, 'perlos2delta.pod';
     open my $out, '>', 'perlos2delta.pod' or die;
     print $out <<EOP;
\=head1 NAME

perlos2delta - a POD copy of $builddir/os2/Changes.

=head1 DESCRIPTION

EOP
     local $_;
     while (<$in>) {
	s/^(?=\S)/ /;
	print $out $_;
     }
     chmod 0444, 'perlos2delta.pod';
  }
  for $file (@files) {
    push @pods, $file->[0];
    $pod_hash{$file->[0]}++;
    $moddesc{$file->[0]} = $file->[1];
  }
}

if ($do_faqs and -f 'perlfaq.pod') {
  opendir DOT, '.';
  while (defined($file = readdir DOT)) {
    next unless $file =~ /(perlfaq.*)[.]pod/i;
    push @faqsfiles, $1;
  }
  closedir DOT;
  # push @faqsfiles, [$1, $2] if /^\s+(\S*)\s+(.*)/ and $1 =~ /^perlfaq/;

  for $file (@faqsfiles) {
    #$faqs_hash{$file}++;
    print STDERR "Doing faq `$file'\n";

    $faqs_hash{$file} = delete $mods_hash{"Pod::$file"} || 
      delete $mods_hash{"pod::$file"} || delete $mods_hash{$file};
    delete $mods_hash{"pod::$file"};
    delete $mods_hash{"$file"};
    delete $mods_hash{"Pod::$file"};
    delete $modnamehash{"pod::$file"};
    delete $modnamehash{"$file"};
    delete $modnamehash{"Pod::$file"};
    # $moddesc{$file->[0]} = $file->[1];
    $add_info{$file} = $1 if $moddesc{$file} =~ s/(\(\$.*\$\))//; # RCS
  }
  unless ($do_mods) {
    %mods_hash = %add_mods_hash = ();
  }
}

#if ($do_tree) {
#  create_tree([keys %modnamehash]);  
#}

my @std_categories = (qw(pod pragmas mods add_mods bin faqs do_dirs),
		      (map "do_dirs$_", 1 .. @add_dir/3),
		      qw(do_file tree));
print STDERR "Categories: `@std_categories'.\n" if $debug;

$tree_hash{'::emit_tree'}++ if $do_tree;

{
  no strict 'refs';
  for $cat (@std_categories) {
    $categories{$cat} = \%{$cat . "_hash"} if %{$cat . "_hash"};
  }
}

for $pod (@files) {
  $doing_pod{$pod->[0] . ".pod"}++;
}

for $pod (qw(perlovl.pod)) {
  $obsolete{$pod}++;
}

for $pod (<*.pod>) {
  $not_doing_pod{$pod}++
    unless $doing_pod{$pod} or $obsolete{$pod} or $pod =~ /perlfaq/;
}

for $pod (keys %not_doing_pod) {
  print STDERR "\n!!! Unknown POD: `$pod'\n" if $do_std;
}

for $name (sort {lc $a cmp lc $b or $a cmp $b} keys %modnamehash) {
  push @files, [$name, ($moddesc{$name} || unknownPod $name)]
}


# in these sections an =item will be treated as an =head[n]
# this is necessary because .IPF/.INF format/compiler have
# some limitations for pane size and linking. :-(

@split_sections =
    (
     'perlfunc/DESCRIPTION/Alphabetical Listing of Perl Functions',
     'perldiag/DESCRIPTION',
     'perlvar/DESCRIPTION/Predefined Names',
    );

@index_sections =		# Put first words in =item into index there
    (
     'perlfunc/DESCRIPTION/Alphabetical Listing of Perl Functions',
     'perlvar/DESCRIPTION/Predefined Names',
    );

$section_head[0] = '';		# To simplify warnings
$section_head[1] = '';		# To simplify warnings

%groups = (
	   links => 1,
	   text => 2,
	   logo => 3,
	   camel => 4,
	   l_camel => 5,
	   r_camel => 6,
	   sublinks => 7,
	   about => 8,
	   tree_nodes => 9,
	  );

%panelwidths = (		# Make a gap.
		links => '29%',
		text => '69%',
		sublinks => '28%',
	       );

# Pickup modules which are not plain words, so it is safer to
# auto-crosslink them (with :: or _, or with mixed capitalization, or
# without vowels with at least 3 letters - avoid B and Tk):
@auto_link = grep /::|_|[a-z][A-Z]|^[^aAeEoOyYiIuU]{3,}$/, keys %alternative_name;
@auto_link{@auto_link} = (1) x @auto_link;


# This is the rest without vowels, will be highlighted only in obvious places:
@auto_link_hard = grep !/::|_|[a-z][A-Z]|^[^aAeEoOyYiIuU]+$/, keys %alternative_name;
@auto_link_hard{@auto_link_hard} = (1) x @auto_link_hard;

sub out;
sub contents;
sub escape;
sub add_ref;
sub findref;
sub winhead;
sub winlink;
sub no_markup_len;
sub insert_nl;

$/ = "";

foreach $sc (@split_sections) { $as_head{$sc} = 1; }

foreach $sc (@index_sections) { $fine_index{$sc} = 1; }

if (not defined $DocTitle) {
  $DocTitle = @files >= 2 ? "Manual" : $files[0][0];
  $DocTitle = escape($DocTitle);
}

$in_item_header = 0;

for ($pass = 1; $pass <= 2; $pass++) {
    if ($pass == 2) {
      $auto_link_hard = join '|', map quotemeta, keys %auto_link_hard;
      $auto_link = join '|',      map quotemeta, keys %auto_link;
      $auto_link_both = join '|', map quotemeta, keys %auto_link, keys %auto_link_hard;
      print STDERR "\nautolink: $auto_link\nautolink_hard: $auto_link_hard\n" 
	if $debug;
    }
    $headno = 0; # make refs hash for this on first pass

    print STDERR "pass: $pass\n";
      print <<EOI if $pass == 2;
:userdoc.
:title.$DocTitle
EOI

    # Insert the Logo page.

    if ($pass == 2 and $do_burst) {
      # We position burst window low, so it does not obsure Contents and/or
      # titles of information windows, and Contents does not obsure us
      # (completely).
      print <<EOI;
:h1 group=$groups{logo} x=7% width=87% y=1% height=90% id=63999 scroll=none.Logo
:i1.Logo
:link reftype=hd refid=63998 auto split group=$groups{l_camel}
       vpx=left vpy=center vpcx=25c vpcy=12c
       scroll=none titlebar=none rules=none.
:link reftype=hd refid=63997 auto split group=$groups{r_camel}
       vpx=right vpy=center vpcx=25c vpcy=12c
       scroll=none titlebar=none rules=none.
:link reftype=hd refid=63996 auto split group=$groups{camel}
       vpx=center vpy=center vpcx=312x vpcy=390p
       scroll=none titlebar=none rules=none.
:h2 hide noprint nosearch id=63998.Dummy
:lines.
:link reftype=hd group=$groups{links} dependent vpx=left vpcx=$panelwidths{links} refid=@{[findrefid('perl')]}.Enter here!:elink.

:link reftype=hd group=$groups{links} dependent vpx=left vpcx=$panelwidths{links} refid=@{[findrefid('perlos2')]}.Perl and OS/2:elink.

:link reftype=hd group=$groups{text} dependent vpx=right vpcx=$panelwidths{text} refid=@{[findrefid('CPAN')]}.Where to get ...:elink.

:link reftype=hd group=$groups{text} dependent vpx=right vpcx=$panelwidths{text} refid=@{[findrefid('ExtUtils::MakeMaker/Default Makefile Behaviour')]}.After you got it:elink.

:link reftype=hd group=$groups{links} dependent vpx=left vpcx=$panelwidths{links} refid=@{[findrefid('perltrap')]}.This should work!:elink.

:link reftype=hd group=$groups{links} dependent vpx=left vpcx=$panelwidths{links} refid=@{[findrefid('perldebug')]}.But it does not!:elink.
:elines.
:h2 hide noprint nosearch id=63997.Dummy
:lines align=right.
:link reftype=hd group=$groups{links} dependent vpx=left vpcx=$panelwidths{links} refid=@{[findrefid('perlbook')]}.The fine book:elink.

:link reftype=hd group=$groups{links} dependent vpx=left vpcx=$panelwidths{links} refid=@{[findrefid('perlmod')]}.Perl extensions:elink.

:link reftype=hd group=$groups{links} dependent vpx=left vpcx=$panelwidths{links} refid=@{[findrefid('perlxs')]}.C extensions:elink.

:link reftype=hd group=$groups{links} dependent vpx=left vpcx=$panelwidths{links} refid=@{[findrefid('perlguts')]}.Inside Camel:elink.

:link reftype=hd group=$groups{links} dependent vpx=left vpcx=$panelwidths{links} refid=@{[findrefid('perlstyle')]}.Ugly code:elink.

:link reftype=hd group=$groups{about} dependent refid=63900.About:elink.
:elines.
:h2 hide noprint nosearch id=63996.Dummy
:artwork align=center name='CamelGrayBig.BMP'.
Do not forget that you can alway click on :hp9.Contents:ehp9., :hp9.Search:ehp9., and :hp9.Index:ehp9. buttons (or use :hp8.Alt-t:ehp8., :hp8.Alt-s:ehp8., :hp8.Alt-i:ehp8. correspondingly).
:font facename=Courier size=7x5. The use of a camel image in conjunction with Perl is a trademark of
O'Reilly &amp. Associates, Inc.:font facename=default size=0x0.

EOI
    }
    if ($pass == 2 and $do_about) {
      print <<EOI;
:h1 toc=1 group=$groups{about} x=center width=100% y=center height=20% id=63900.About
EOI
      print out(<<EOI, 1);
Generated on @{[scalar localtime]}, by L<perl> version $],
L<pod2ipf> version $VERSION @{[format_args]} in directory F<@{[cwd]}>.
EOI
    }
    if ($head_off <= 1 or (keys %categories) <= 1) {
      if ($head_off > 1) {
	print <<EOP if $pass == 2;
:h1 toc=$maxtoc group=$groups{links} x=left width=$panelwidths{links} id=63000.$DocTitle
$DocTitle.

EOP
      }
      for ($fn = 0; $fn <= $#files; $fn++) {
	output_file($files[$fn][0]);
      }
    } else {
      # Separate into categories:
      my @titles;
      for $cat (@std_categories) {
	next unless $categories{$cat};
	insert_back($headno) if $pass == 2;;
	category_emit($cat) if $pass == 2;
	@titles = sort {lc $a cmp lc $b or $a cmp $b} 
		keys %{$categories{$cat}};
	@titles = @pods if $cat eq 'pod'; # Preserve the sorting order
	for $title (@titles) {
	  if ($title eq '::emit_tree') {
	    output_tree(create_tree([keys %modnamehash]), '', 2) if $pass == 2;
	  } else {
	    output_file($title);
	  }
	}
      }
    }
}

print "\n:euserdoc.\n";

if ($dump_xref) {
    foreach (keys %links) {
        print STDERR $_ . "->" . $links{$_} . "\n";
    }
}
if ($dump_contents) {
    for($i = 0; $i <= $#head; $i++) {
        print STDERR "    " x $headlevel[$i], $head[$i], "\n";
    }
}
if ($dump_manpages) {
  my @arr = sort keys %unknown_manpages;
  print STDERR "Unknown manpages: @arr.\n";
}
print STDERR "Found $foundrefs crosslinks.\n";

sub category_emit {
  my $cat = shift;
  $cat_id ||= 63000;
  $cat_id++;
  print <<EOP;
:h1 toc=$maxtoc group=$groups{links} x=left width=$panelwidths{links} id=$cat_id.$cat_descr{$cat}
$cat_descr{$cat}.

EOP
}

sub process_index {
  unless ($print_index) {
    @index = ();
    return;
  }
  my %seen;
  for (@index) {
    $seen{$_}++;
  }
  print "\n" if @index;
  for (keys %seen) {
    print ":i1." . out($_, 0) . "\n";    
  }
  @index = ();
}

sub output_file {
  my $ftitle = shift;
  #warn "<<<doing file for `$ftitle'\n";
  my $fcomment = $moddesc{$ftitle} || unknownPod $ftitle;
  my $begin_depth = 0;

  $fname = $ftitle . '.pod';
  if (not -f "./$fname") {	# Protect 'B::C.pod' from accessing B drive
    $fname = $modnamehash{$ftitle};
  }
  $page = $ftitle;
  $toc = substr $maxtoc, -1, 1;
  @index = ();

  open(IN, $fname) || die "$ftitle: open `$fname': $!";
  print STDERR $fname . ": ";
  print STDERR "\n" if !$dots;

  $section = $ftitle . ' - ' . $fcomment;
  $section_head[1] = $page;
  $path = $section_head[1];
  $headno++;
  #warn "<<<incremented headno to $headno, page='$page', washead='$heading' out='$section'\n";
  if ($pass == 1) {
    addsection($section, $headno, 1);
    add_ref($page, $headno);
    $is_head{$ftitle}++;
  }
  if ($pass == 2) {
    insert_nl;
    my $hlevel = $head_off >= 1 ? $head_off : 1;
    # Done with $processing_endfile now:
    print ":h$hlevel toc=$maxtoc " . winhead($headno)
      . " id=$headno."
	. out($section, 0) . "\n" . $font; # Headers take no fonts.
    output_index($section, $ftitle);
    output_index($ftitle, $ftitle);
    if (exists $add_info{$ftitle}) {
      print out($add_info{$ftitle}, 0), ":p.\n"
    }
    $was_nl = 1;
  } else {
    count_index($section);
    count_index($ftitle);
  }

  @lstack = ();
  # These variables are different when =item is treated as =head
  $emptypane = $emptysection = 1; # Always: TOC for the file
  $inpod = 0;
  my $processing_endfile;

 PARA: while (defined ($line = <IN>) or ++$processing_endfile) {
    chomp $line unless $processing_endfile;
    if ($processing_endfile or $line =~ /^=\w+/) {
      if ($processing_endfile or $line =~ /^=head(\d+)\b\s*/) {
	$inpod = 1;
	$nopara = 0;
	$heading = $';
	unless ($processing_endfile) {
	  $heading =~ s/\s*$//;	# localize $1
	  $heading = untabify $heading;
	}

	if (@lstack) {
	  warn "List not finished (@lstack) in (@section_head[1..$hl]).\n"
	    if $pass == 1;
	  while ($#lstack >= 0) {
	    $t = pop(@lstack);
	    if ($t eq 'ul') {
	      print ":eul.\n" if $pass == 2;
	      $was_nl = 1;
	    } elsif ($t eq 'ol') {
	      print ":eol.\n" if $pass == 2;
	      $was_nl = 1;
	    } elsif ($t eq 'parml') {
	      print ":eparml.\n" if $pass == 2;
	      $was_nl = 1;
	    } elsif ($t eq 'head' or $t eq 'finehead') {
	      $hl--;
	      $path = join('/', @section_head[1..$hl]);
	    }
	  }
	}

	unless ($processing_endfile) {
	  $hl = $1 + 1;
	  $section_head[$hl] = $heading;
	  $path = join('/', @section_head[1..$hl]);
	  $sh_path = join('/', @section_head[1..$hl-1]);
	  if ($skip_sections{$path}) {
	    $inpod = 0;
	    next PARA;
	  }
	}
	# Finish the previous sections:
	contents($headno) if $emptypane;
	insert_back($headno);	# Previous header
	last PARA if $processing_endfile;

	$headno++;
	if ($pass == 1) {
	  addsection($heading, $headno, $hl);
	  # XXXX Is wrong with some escapes:
	  #1 while $heading =~ s/[A-Z]<.*?>/$1/g;
	  add_ref(qq|$page/"$heading"|, $headno);
	  $is_head{$heading}++;
	}
	if ($pass == 2) {
	  insert_nl;
	  print ":h", $hl + $head_off - 1 , " " . winhead($headno)
	    . " id=$headno."
	      . out($heading, 0) . "\n" . $font; # Headers take no fonts
	  output_index($heading, $path);
	} else {
	  count_index($heading);
	}
	print STDERR "." if $dots;
	$emptypane = $emptysection = 1;
      } elsif ($line =~ /^=over\b\s*/) {
	$inpod = 1;
	$step = 5;		# Default
	$step = $& if $' =~ /\d+/;
	$step = int($step * 4/3 + 0.5); # Take into account proportional font
	# look ahead, to see how the list should look like
	if ($pass == 1 and $inpod and $begin_depth == 0) {
	  $auto_link_hard{$1}++,
	    $x_index{$1} = $headno
	      while $line =~ /X<([^<>]+)>/g;
	}
	chomp($line = <IN>);
	if ($pass == 1) {
	  $auto_link_hard{$1}++ while $line =~ /X<([^<>]+)+>/g;
	}
	if ($line =~ /^\=item(\s*$|\s+\*)/) { # item * (or empty)
	  push(@lstack, "ul");
	  insert_nl if $pass == 2;
	  print ":ul.\n" if $pass == 2;
	  $was_nl = 1;
	} elsif ($line =~ /^\=item\s+1\.?/) { # item 1. 
	  push(@lstack, "ol");
	  insert_nl if $pass == 2;
	  print ":ol.\n" if $pass == 2;
	  $was_nl = 1;
	} elsif (defined($as_head{$path})) {
	  # in some cases we use headings instead of lists
	  warn "toc for $page, id=$headno too low" if ! $toc >= $hl + 1;
	  push(@lstack, $fine_index{$path} ? "finehead" : "head");
	  $hl++;
	  $section_head[$hl] = 'list_start';
	  @eitems = ();
	} else {
	  push(@lstack, "parml");
	  insert_nl if $pass == 2;
	  print ":parml break=fit tsize=$step.\n" if $pass == 2;
	  $was_nl = 1;
	}
	$nopara = 0;
	redo PARA;
      } elsif ($line =~ /^=back\b/) {
	$inpod = 1;
	if ($#lstack >= 0) {
	  $t = pop(@lstack);
	  if ($t eq 'ul') {
	    insert_nl if $pass == 2;
	    print ":eul.\n" if $pass == 2;
	    $was_nl = 1;
	  } elsif ($t eq 'ol') {
	    insert_nl if $pass == 2;
	    print ":eol.\n" if $pass == 2;
	    $was_nl = 1;
	  } elsif ($t eq 'parml') {
	    insert_nl if $pass == 2;
	    print ":eparml.\n" if $pass == 2;
	    $was_nl = 1;
	  } elsif ($t eq 'head' or $t eq 'finehead') {
	    $hl--;
	    $path = join('/', @section_head[1..$hl]);
	  }
	} else {
	  warn "stack empty on page=$page, id=$headno";
	  $hl--;
	}
	$nopara = 0;
      } elsif ($line =~ /^=item\b\s*/) {
	$inpod = 1;
	$nopara = 0;
	$heading = $';
	$heading =~ s/\s+$//;
	$heading = untabify($heading);
	$heading =~ s/^\*\s*//;	# Too late to process \* anyway
	#warn "<<<set head='$heading'\n";
	$headx = $heading;
	# Unescape
	$headx =~ s/E<(.*?)>/$HTML_Escapes{$1} or "E<$1>"/ge;
	1 while $headx =~ s/[A-Z]<(.*?)>/$1/g;
	print STDERR "." if $dots;
	if ($#lstack == -1) {
	  push(@lstack, "parml");
	  insert_nl if $pass == 2;
	  print ":parml break=fit tsize=7.\n" if $pass == 2;
	  $was_nl = 1;
	  warn "An =item without =over in (@section_head[1..$hl])\n" 
	}
	if ($lstack[$#lstack] eq 'head'
	    or $lstack[$#lstack] eq 'finehead') {
	  contents($headno - 1) if $emptypane and not @eitems;
	  insert_back($headno - 1)
	    unless $emptysection or @eitems; # Previous uplink

	  # lowest level never empty, IPFC uses next page
	  # by default (but Back button doesn't work :-()
	  # 
	  # However, we treat it specially anyway
	  $emptypane = 0;
	  $emptysection = 1;

	  my ($word1, $word2);
	  $headx =~ /(\^?\w+)/; # $^A
	  $word1 = $1;
	  $headx =~ /(\S+)/;
	  $word2 = $1;
	  $section_head[$hl] = $heading;
	  $path = join('/', @section_head[1..$hl]);
	  $sh_path = join('/', @section_head[1..$hl-1]);
	  insert_nl if $pass == 2;
	  $headno++;
	  #warn "<<<incremented headno to $headno, page='$page', head='$heading'\n";
	  if ($pass == 1) {
	    addsection($heading, $headno, $hl);
	    add_ref(qq|$page/"$headx"|, $headno);
	    add_ref(qq|$page/"$word1"|, $headno) if defined $word1;
	    add_ref(qq|$page/"$word2"|, $headno) if defined $word2;
	  }
	  print ":h", $hl + $head_off - 1, " " . winhead($headno)
	    . " id=$headno."
	      . out($heading, 0) . "\n" . $font 
		if $pass == 2 and not @eitems; # Headers take no fonts
	  output_index($heading, $path);		    
	  $was_nl = 1;
	  if ($#lstack >= 0 
	      and $lstack[$#lstack] eq 'finehead') {
	    output_index($word1, $path)
	      if defined $word1 and $word1 ne $heading;
	    output_index($word2, $path)
	      if defined $word2 and $word2 ne $heading
		and $word2 ne $word1;
	  }
	  $is_head{$heading}++ if $pass == 1; # XXXX Need to strip?

	  if ($pass == 1 and $inpod and $begin_depth == 0) {
	    $auto_link_hard{$1}++,
	      $x_index{$1} = $headno
		while $line =~ /X<([^<>]+)>/g;
	  }

	  # look ahead to see if this =item is empty.
	  # if it is, create a list of the items
	  # on first non-empty pane.
	  chomp($line = <IN>);
	  if ($line =~ /^=item\b/) {
	    push @eitems, $heading;
	    $headno--;
	  } elsif (@eitems) {
	    push @eitems, $heading;
	    if ($pass == 1) {
	      $hl_eitems{$headno} = [@eitems];
	    } else {
	      foreach $l (@eitems) {
		print ":p.:hp2." . out($l, 1) . ":ehp2.";
	      }
	    }
	    @eitems = ();
	  }
	  if ($pass == 1) {
	    $auto_link_hard{$1}++ while /X<([^<>]+)+>/g;
	  }
	  redo PARA;
	} else {		# Different list's items
	  local $in_item_header = 1;
	  $emptypane = $emptysection = 0;
	  add_ref(qq|$page/"$headx"|, $headno, 1);
	  if ($lstack[$#lstack] eq 'ul' && $heading =~ /^\s*\*\s*(.*)$/ or
	      $lstack[$#lstack] eq 'ol' && $heading =~ /^\s*\d+\.?\s*(.*)$/) {		 # Bulleted or numbered item matching list type.
	    print ":li." if $pass == 2;
	    $heading = $1;
	    #warn "<<<set head='$heading'\n";
	    if ($1 ne "") {
	      print out($heading, 1) . "\n" if $pass == 2;
	      output_index($heading, $path)
		unless $is_head{$heading};
	      $was_nl = 1;
	    } else {
	      $nopara = 1;
	      $was_nl = 0;
	    }
	  } elsif ($lstack[$#lstack] eq 'parml') {
	    print ":pt." if $pass == 2;
	    $heading =~ s/^\s*\*?\s+//;
	    $heading =~ s/\s+$//;
	    $heading = '*' if $heading eq '';
	    #warn "<<<set head='$heading'\n";
	    print out($heading, 1) . "\n" if $pass == 2;
	    output_index($heading, $path)
	      unless $is_head{$heading} or $heading eq '*' or $heading eq '';
	    print ":pd." if $pass == 2;
	    $nopara = 1;
	    $was_nl = 0;
	  } else {
	    print ":li." . out($heading, 1) . "\n" if $pass == 2;
	    output_index($heading, $path)
	      unless $heading eq '' or $heading eq '*' or $is_head{$heading};
	    $was_nl = 1;
	    $nopara = 1;
	  }
	}
      } elsif ($line =~ /^=cut/) {
	$inpod = 0;
      } elsif ($line =~ /^=pod/) {
	$inpod = 1;
      } elsif ($line =~ /^=for\b/) { # Ignore
      } elsif ($line =~ /^=begin\b/) {
	$begin_depth++;
      } elsif ($line =~ /^=end\b/) {
	$begin_depth--;
      } else {
	warn "what to do with '$line'?\n";
      }
    } elsif ($inpod == 0 or $begin_depth) {
      # Just ignore this chunk
    } elsif ($line =~ /^[ \t]+\S/) {
      if ($pass == 2) {
	$pre = untabify($line);
	insert_nl;
	1 while $pre =~ s/^(.{250})(?=[^\n])/$1\n/gm; # Warning 206
	print ":xmp.$to_C\n" . escape_with_url($pre)
	  . "\n:exmp.$from_C\n";
	$was_nl = 1;
      }
      $nopara = 0;
      $emptypane = $emptysection = 0;
    } elsif ($line =~ /^[ \t]+\S/m) { # see perl(run)?/SYNOPSIS for this
      if ($pass == 2) {
	$mark = out($line, 1);

	# hack hack ;-)
	# IPFC doesn't handle tabs
	# no_markup_len tries to guess the # of ' ' to next tab,
	# but even when the guess is correct, things don't seem
	# to align when bold,.. is used :-(
	$pre = untabify_after($mark);

	insert_nl;
	# Do not split inside &something.
	1 while $pre =~ s/^(.{240}(?=.{13}))[^\n&]{0,12}(?=[^\n])/$1\n/gm; # Warning 206
	print ":xmp.$to_C\n" . $pre . "\n:exmp.$from_C\n";
	$was_nl = 1;
      }
      $nopara = 0;
      $emptypane = $emptysection = 0;
    } else {
      if ($pass == 2) {
	print ":p.\n" unless $nopara;
	print out(untabify($line), 1);
	process_index();
	$was_nl = 0;
      } else {
	if ($line =~ /^\s+$/) {
	  warn "line with blanks in $page, id=$headno\n";
	}
      }
      $nopara = 0;
      $emptypane = $emptysection = 0;
    }
    if ($pass == 1 and $inpod and $begin_depth == 0) {
      $auto_link_hard{$1}++,
	$x_index{$1} = $headno
	  while $line =~ /X<([^<>]+)>/g;
    }
  }
  close(IN);
  print STDERR "\n" if $dots;
}

sub output_tree {
  my ($tree, $prefix, $level) = @_;
  my ($node, $mod);
  foreach $node (sort keys %$tree) {
    $cat_id++;
    if ($prefix eq '') {
      $mod = substr $node, 2;
    } else {
      $mod = "$prefix$node";      
    }
    if (ref $tree->{$node}) {	# Subtree
      print <<EOP;
:h$level group=$groups{tree_nodes} x=left width=10% y=top height=10% id=$cat_id.@{[escape $mod]}...

EOP
      output_tree($tree->{$node}, $mod, $level + 1);
    } else {
      print <<EOP;
:h$level group=$groups{tree_nodes} x=left width=10% y=top height=10% id=$cat_id.@{[escape $mod]}
:link reftype=hd group=$groups{links} auto vpx=left vpcx=$panelwidths{links} refid=@{[findrefid($mod)]}.
@{[escape $mod]}

EOP
    }
  }
}

sub untabify {
  my @tlines = split(/\n/, shift);
  my $tline;
  foreach $tline (@tlines) {
    1 while $tline =~ s/\t+/' 'x (length($&) * 8 - length($`) % 8)/e;
  }
  join("\n", @tlines);
}

sub untabify_after {		# Some markup is already there.
  my @tlines = split(/\n/, shift);
  my $tline;
  foreach $tline (@tlines) {
    1 while $tline =~ s/\t+/' 'x (length($&) * 8 - &no_markup_len($`) % 8)/e;
  }
  join("\n", @tlines);
}

{
  my $id_c = 0;
  sub i1id {
    return $i1ids{$_[0]} if exists $i1ids{$_[0]};
    $i1ids{$_[0]} = "id_" . ++$id_c;
  }
}

sub trunc_heading {
  my $h = shift;
  return $h unless length $h > 124;
  substr($h, 0, 110) . "...";
}

sub output_index {
  return &count_index if $pass == 1;
  my ($heading, $path) = (shift, shift);
  return if $path =~ m,^perltoc/,;
  $heading = trunc_heading($heading);
  $path = trunc_heading($path);
  warn "Undefined \$index_seen{\$heading}, \$heading='$heading'.\n"
    unless defined $index_seen{$heading};
  if ($index_seen{$heading} > 1) {
    my $id = $i1ids{$heading};
    unless ($id) {
      $id = i1id($heading);
      print ":i1 id=$id." . out($heading, 0) . "\n";
    }
    print ":i2 refid=$id." . out("[$path]", 0) . "\n" 
      unless $index_output{$id}{$headno}++;
  } else {
    print ":i1." . out($heading, 0) . "\n";    
  }
}

sub count_index { $index_seen{trunc_heading shift}++ }

sub maybe_link {
  my $txt = shift;
  return "L<$txt>" if
    exists $links{findref($txt,1)} or $txt =~ m,^((\w+)\([23]\)|POSIX\s*\(3\)/(\w+)|(emx\w+))$,i;

  return $txt;
}

sub strip {
    my $in = shift;

    1 while $in =~ s/X<([^<>]*)>//;
    1 while $in =~ s/[A-Z]<([^<>]*)>/$1/;

    return $in;
}

sub link_url {
  my $url = shift;
  if (@www) {
    my $cmd = join ' || ', map { s/\@\@\@/$url/; $_ } @www;
    ":link reftype=launch object='cmd.exe' data='/c $cmd'."
  } else {
    ":link reftype=launch object='$www' data='$url'."
  }
}

sub try_external_link {
    my ($txt, $outtxt) = shift;
    $outtxt = $txt unless defined $outtxt;
    my $link;

    $foundrefs++, return ":link reftype=hd refid=$x_index{$txt}."
      . out($outtxt) . ":elink."
	if exists $x_index{$txt};

    $txt = $1 if $txt =~ m!^[BCI]<{2,}\s+(.*?)\s+>{2,}$!s;
    $txt = $1 if $txt =~ m!^[BCI]<(.*?)>$!s;
    if ($txt =~ m,^(http|file|ftp|mailto|news|newsrc|gopher)://,) {
	$link = strip($txt);
    } elsif ($txt =~ m,^\"(http|file|ftp|mailto|news|newsrc|gopher)://, 
	     and $txt =~ /\"$/) {
	$link = strip(substr $txt, 1, length($txt) - 2);
    } elsif ($txt =~ m,^((\w+)\([23]\)|POSIX\s*\(3\)/(\w+)|(emx\w+))$,i) {
	return ":link reftype=launch object='view.exe' data='emxbook $+'."
	  . out($outtxt) . ":elink.";
    }
    return link_url($link) . out($outtxt) . ":elink." if $link;
    return undef;
}

sub format_funcall {
  my ($pre, $call, $func) = (@_);
  ( $func =~ /^($auto_link_both)$/o )
    ? "${pre}C<L<$func>()>"
    : "${pre}C<$call>"
}

sub auto_beautify {
    my $para = $_[0];
    # We start with links to make as many of them as we can:
    $para =~ s/(^|[^<:\$@%])\b($auto_link)\b(?=$|[^>:]|:[^:])/$1L<$2>/go
      if $auto_link;
    # perl(1) Tix(n)
    $para =~
      s/ (^|[^<]) \b ( [\w:]+ \b \([\dn]\) ) (?=$|[^>]) /$1 . maybe_link($2)/gxe;
    # words in "SEE ALSO"
    $para =~ s/ (^|[^<:]) \b ( $auto_link_hard ) \b (?=$|[^>:]) /$1L<$2>/gox
	if $hl and $section_head[$hl] eq "SEE ALSO" and $auto_link_hard;
    # Link sections which are highlighted
    $para =~ s/([CBI])<($auto_link_both)>/$1<L<$2>>/go if $auto_link_both;
    $para =~ s/C<-([^\W\d])>/C<L<-$1>>/g;
    $para =~ s/ ^ ( $auto_link_hard ) $  /L<$1>/ox 
      if $in_item_header and $auto_link_hard;
    # URLs inside F<>
    $para =~ s, F< ((http|file|ftp|mailto|news|newsrc|gopher):// [^\s<>]* ) > 
              ,L<$1>,xg ;
    # free-standing URLs.
    $para =~ s% ( \s | ^ ) 
                ( (?:http|file|ftp|mailto|news|newsrc|gopher)
		  :// 
		  [^\s<>]* 
		  [^\s.,:;!?\"\'\)] # Strip trailing punctuation.
		)
                ( [.,:;!?\"\'\)]* ( \s | $ ) | $ )
              %$1L<$2>$3%xg ;
    # <> below is to avoid BOLDing of C in C<>
    $para =~ s/(^|[^<:\$@%])\b($make_bold)\b(?=$|[^<>:]|:[^:])/$1B<$2>/go 
      if $make_bold;
    $para =~ s/(^|[^<:\$@%])\b($make_code)\b(?=$|[^>:]|:[^:])/$1C<$2>/go 
      if $make_code;
    $para =~ s/ (\s+ | ^) ( [\$%\@] [\w:]+ \b) (?=$|[^>]) /$1C<$2>/gx; # $var
#    $para =~ s/([CBI])<($auto_link_both)>/$1<L<$2>>/go if $auto_link_both;
    $para =~ s{ (^|[^<]) \b ( ([\w:]+) \b \(\) ) (?=$|[^>]) }
	      { format_funcall($1, $2, $3) }gex;		# func()
    $para;
}

my @tag_array;
BEGIN { @tag_array = qr{([<>])} }	# ;-) ;-)

sub fill_tag_array {
  my $upto = shift;		# no. of > plus 1
  while (@tag_array <= $upto) {
    my $repeat = @tag_array + 1;
    push @tag_array, qr{(<|\s+>{$repeat})};
  }
}

sub find_matching {
  my $kets = 1 + pop;
  my $off = pop;
   $_[0] = $off
}

sub out {
    return if ($pass == 1);

    my $para = shift;
    my $markup = shift;
    my $beautify = $markup && ! shift;
    my $stop_at_ket = shift;	# Not finished: @stack?
    my $cpos = shift || 0;	# scanned up to
    my @stack = ();
    my @lenstack = $stop_at_ket || 0;
    my $output = "";
    my ($c, $opos);		# opos: output done up to
    my $c_deep = 0;
    my @pos_stack;

    $para = auto_beautify($para) if $beautify;

    pos $para = $opos = $cpos;
    TAG: while ( $para =~ m{$tag_array[$lenstack[-1]]}g ) {
        $cpos = pos $para;
        $c = $1;

        if ($c eq '<' && $cpos < 2) {
            $output .= escape(substr($para, $opos, $cpos - $opos));
        } elsif ($c eq '<') {
            $output .= escape(substr($para, $opos, $cpos - $opos - 2))
	      if $cpos - $opos > 2;

            $c = substr($para, $cpos - 2, 1);
            if ($c !~ /[A-Z]/) {
	      $output .= escape(($cpos - $opos > 1 ? $c : '') . '<');
                pos($para) = $opos = $cpos;
                next TAG;
            }

	    # Look for L<<< X >>>
	    my $extra = 0;
	    pos $para = $cpos;
	    $cpos = pos $para, $extra = length $1 if $para =~ /\G(<+)\s+/g;
	    push @lenstack, $extra;
	    fill_tag_array($extra);

            if ($c eq 'B') {
	      if (grep {$_ eq 'I'} @stack) {
                $output .= ':hp3.' if $markup;
                push (@stack, 'BI');
	      } else {
                $output .= ':hp2.' if $markup;
                push (@stack, $c);
	      }
            } elsif ($c eq 'F') {
                $output .= ':hp6.' if $markup;
                push (@stack, $c);
            } elsif ($c eq 'S') {
                # $output .= ':hp2.' if $markup; # XXXX Should not!
                push (@stack, $c);
            } elsif ($c eq 'I') {
	      if (grep {$_ eq 'B'} @stack) {
                $output .= ':hp3.' if $markup;
                push (@stack, 'BI');
	      } else {
                $output .= ':hp1.' if $markup;
                push (@stack, $c);
	      }
            } elsif ($c eq 'C') {
                $output .= $to_C if $c_deep++ == 0 and $markup;
                push (@stack, $c);
            } elsif ($c eq 'L') {
		# Allow one level of included modifiers:
		push (@stack, $skip_embedded_links ? 'L' : 'LL'); # Treat as ordinary text
		pos($para) = $opos = $cpos;
		unless ($skip_embedded_links) {
		  push @pos_stack, $cpos;
		  push @out_stack, length $output;
		}
		next TAG;
            } elsif ($c eq 'E') {
                pos ($para) = $cpos;
                if ($para =~ m{$tag_array[$lenstack[-1]]}g) {
                    my $esc;
		    my $ocpos = $cpos;
                    $cpos = pos $para;
		    my $tag = substr $para, $ocpos, $cpos - $ocpos - length $1;

		    if ($tag =~ m/^(([A-Za-z]+)|\d+)$/) {
		      if (defined $2) {
			if (exists $HTML_Escapes{$1}) {
			  $escaped = 0;
			  $esc = $HTML_Escapes{$1};
			} else {
			  $esc = exists $HTML_EscapesLit{$1} 
			    ? "&$HTML_EscapesLit{$1}." : "E<$1>";
			  $escaped = 1;
			}
		      } else {
			$esc = chr $1;
			$escaped = 1;
		      }
		      $output .= ($escaped ? $esc : escape($esc));
		    } else {
		      warn "$fname: unknown E<> in `" . (substr $para, $cpos-2, 10) . "'???\n";
		    }
                } else {
                    warn "$fname: unknown E<> in `" . (substr $para, $cpos-2, 10) . "'???\n";
                }
            } elsif ($c eq 'X') {
                pos ($para) = $cpos;
                if ($para =~ m{$tag_array[$lenstack[-1]]}g) {
                    my $esc;
		    my $ocpos = $cpos;
                    $cpos = pos $para;
		    my $tag = substr $para, $ocpos, $cpos - $ocpos - length $1;
                    #$output .= escape($1);
		    push @index, out $tag if $print_index;
                } else {
                    warn "$fname: error parsing X<> ??? `" . (substr $para, $cpos-2, 160) . "'\n";
                }
            } elsif ($c eq 'Z') {
                pos ($para) = $cpos;
                if ($para =~ m/\G>/g) {
                    $cpos = pos $para;
                } else {
                    warn "funny: Z<...> ???\n";
                }
            } else {
                warn "$fname: what to do with $c<> ?\n";
            }
        } elsif ($#stack >= 0) {
	    my $l = length $c;

            $output .= escape(substr($para, $opos, $cpos - $opos - $l));
            $c = pop(@stack);
            if ($c eq 'B') {
                $output .= ':ehp2.' if $markup;
            } elsif ($c eq 'F') {
                $output .= ':ehp6.' if $markup;
            } elsif ($c eq 'S') {
                # $output .= ':ehp2.' if $markup;
            } elsif ($c eq 'I') {
                $output .= ':ehp1.' if $markup;
            } elsif ($c eq 'BI') {
                $output .= ':ehp3.' if $markup;
            } elsif ($c eq 'C') {
                $output .= $from_C if --$c_deep == 0 and $markup;
            } elsif ($c eq 'L') {
                # end "unprocessed" link
            } elsif ($c eq 'LL') {
	      # finish processing of the link

	      # First, ignore what was added
	      substr($output, pop @out_stack) = '';
	      # Avoid warning 208: a link inside a link could be
	      # created during auto_beautify() above.
	      local $skip_embedded_links = 1;

	      # Get the string inside L<< >>
	      my $b = pop @pos_stack;
	      my $link = substr $para, $b, $cpos - $l - $b;
	      my $linktxt = $link;

	      # XXX Use heuristics to find | and /:
	      my $haveliteral = 0;
	      $haveliteral = 1, $linktxt = $1
		if $link =~ s!^(([A-Z]<[^<>]*>|(?>[^>|/]+))+)\|!!;
	      $foundlink = findref($link);
	      if (defined $links{$foundlink}) {
		my $blink = $linktxt;
		unless ($haveliteral) {
		  $blink =~ s|^"(.+)"$|$1|sm or
		    $blink =~ s|^([-\w:]+)/"(.+)"$|$1: $2|sm;
		}
		$output .= ":link reftype=hd refid=$links{$foundlink}."
		  if $markup;
		$output .= out($blink, 1, 1);
		$output .= ":elink." if $markup;
	      } elsif ($foundlink = try_external_link($link,$linktxt)) {
		$output .= $markup ? $foundlink : out($linktxt) ;
	      } else {
		warn "   unresolved link: $link\n";
		$output .= out($linktxt, 1, 1);
	      }
	      pos $para = $opos = $cpos;
            } else {
	      $output .= escape('>'); # XXX Why?
            }
	    pop @lenstack;
        } elsif (defined $stop_at_ket) {
            $output .= escape(substr($para, $opos, $cpos - $opos - length $c));
	    $opos = $cpos;
	    last TAG;
        } else {
            $output .= escape(substr($para, $opos, $cpos - $opos));
        }
        pos($para) = $opos = $cpos;
    }
    $output .= escape(substr($para, $opos, length($para) - $opos));
    if (!$markup) { # for toc/index/...
        $output =~ s/\n\s*/ /g;
        $output = substr($output, 0, 140); # strip too long stuff
    }
    $output =~ s/^\./&per./mg;	# period.  Warning 204
    # Do not split inside "something"
    1 while $output =~
      s/^(.{200}(?=.{50})[^\n \t]{0,49})[ \t]+/$1\n/gm; # Warning 206
    return $output;
}

sub insert_back {		# Args: head_level and num of finished section,
  return unless $pass == 2;
  my $parent = find_parent($_[0]);
  return if $parent == $_[0];
  return if $parent <= 0;
  insert_nl;
  print " :link reftype=hd refid=$parent.:font facename=Courier size=8x6.Go Up:font facename=default size=0x0.:elink.\n";
  $was_nl = 1;
}

# Find the latest node before $i at level < $level
sub find_parent {
    my $level = $headlevel[$_[0]];
    my $i = $_[0] - 1;

    while ($i > 0 && $headlevel[$i] >= $level) { $i--; }

    return $i;
}

sub contents {
    my $no = $_[0];
    my ($i, $cl, $toplevel);
    local $print_index = 0;

    $isempty{$no}++;
    if ($pass == 1) {
        $wingroup[$no] = $groups{links};
        return ;
    }

    #$i = find_parent($level,$no);
    $i = $no;

    $toplevel = $headlevel[$i];
    return if $toplevel <= 0;

    print ":p." . out($head[$i], 1) . "\n";
    $was_nl = 1;
    $i++;
    $cl = $toplevel;
    local $skip_embedded_links = 1;
    for (; $i <= $#head && $headlevel[$i] > $toplevel; $i++) {
        if ($headlevel[$i] > $cl) {
            warn "bad nesting: $toplevel, $headlevel[$i], $cl, $i, `$head[$i]`\n" if $headlevel[$i] != $cl + 1;
            print ":ul compact.\n";
	    $was_nl = 1;
            $cl++;
        } elsif ($cl > $headlevel[$i]) {
            while ($cl > $headlevel[$i]) {
                print ":eul.\n";
		$was_nl = 1;
                $cl--;
            }
        }
	if (exists $isempty{$i}) {
	  print ":li.", out($head[$i], 1, 1), "\n";	
	} elsif (exists $hl_eitems{$i}) {
	  print ":li.:link reftype=hd " . winlink($i)
            . " refid=$i."
	      . out($_, 1, 1) . ":elink.\n" for @{$hl_eitems{$i}};
	} else {
	  print ":li.:link reftype=hd " . winlink($i)
            . " refid=$i."
	      . out($head[$i], 1, 1) . ":elink.\n";
	}
	$was_nl = 1;
    }

    while ($cl > $toplevel) {
        print ":eul.\n";
        $cl--;
	$was_nl = 1;
    }
}

sub findrefid {
  my $in = shift;
  my $out = $links{findref($in)} || $x_index{$in};
  warn "No refid for `$in'\n" if $debug and not defined $out;
  ($out || 0);			# XXXX misplaced link - special panel?
}

sub findref_with_pages { # various heuristics to get a valid prefix for a link
    my $link = shift;
    my $links = shift;

    if (!defined $links->{$link}) {
      if ($link =~ m|(.*?)\.pod/(.*)|s) {
	$link = "$1/$2";	# Remove .pod from page name
      } elsif ($link =~ m|(.*?)\.pod$| and defined $links->{$1}) {
	$link = $1;
      }
    }
    if (!defined $links->{$link}) { # try harder
        if (defined $links->{qq|$page/"$link"|}) {
            $link = qq|$page/"$link"|;
        } elsif ($link =~ /^\"/) {
            $link = "$page/$link";
        } elsif ($link =~ m|^/\"|) {
            $link = "$page$link";
        } elsif ($link =~ m|^/|) {
            $link = qq|$page/"$'"|;
        } elsif ($link =~ m|^([^/ ]+)/([^\"]+)$|) { # A/B to A/"B"
            $link = qq|$1/"$2"|;
        } elsif (exists $alternative_name{$link}
		 and exists $links->{$alternative_name{$link}}) {
            $link = $alternative_name{$link};
	}
        # print STDERR "trans: $link\n" if $debug_xref;
    }
    return $link;
}

sub warn_strip {
  my ($link,$works,$flink) = @_;
  my $b = auto_beautify($flink);
  return if -1 < index $b, $link;	# May be missing perlpod/ or ""
  warn "Strip conversion in link: `$link' vs `$flink'.\n";
}

sub findref_with_variations { # various heuristics to get a valid link
    my $link = shift;
    my $flink = findref_with_pages($link, \%links);
    my $tlink = $link;
    my $t;

    if (!defined $links{$flink}) { # try harder
      ($tlink = $link) =~ s/\s+/ /g;
      $t = findref_with_pages($tlink, \%links_nws);
      $flink = $links_nws{$t} if exists $links_nws{$t};
    }
    if (!defined $links{$flink}) { # try file operators
      if ($link =~ /^-[^\W\d]$/) {
	$tlink = qq|perlfunc/"-X"|;
	$t = findref_with_pages($tlink, \%links_nws);
	$flink = $t if exists $links{$t};
      }
    }
    return $flink if $_[0];
    if (!defined $links{$flink}) { # try harder
      $tlink = lc $tlink;
      $t = findref_with_pages($tlink, \%links_lc);
      if (exists $links_lc{$t}) {
	$flink = $links_lc{$t};
	warn "Case conversion in link: `$link' vs `$flink'.\n";
      }
    }
    if (!defined $links{$flink}) { # try another way
      $tlink = strip $link;
      $t = findref_with_pages($tlink, \%links_strip);
      $flink = $links_strip{$t} if exists $links_strip{$t};
      if (!defined $links{$flink}) { # try harder
	my $b = auto_beautify($tlink);
	$t = findref_with_pages($b, \%links_b);
	if (exists $links_b{$t}) {
	  $flink = $links_b{$t};
	  warn_strip($link,$t,$flink);
	}
      } else {
	  warn_strip($link,$t,$flink);
      }
    }
    if (!defined $links{$flink}) { # try another way
      my $b = auto_beautify($link);
      $t = findref_with_pages($b, \%links_b);
      if (exists $links_b{$t}) {
	$flink = $links_b{$t} if exists $links_b{$t};
	warn "Strip conversion in link: `$link' vs `$flink'.\n";
      }
    }
    return $flink;
}

sub findref { # various heuristics to get a valid link
    my $link = shift;
    my $flink = findref_with_variations($link, @_);

    print STDERR "link: $link\n" if $debug_xref;

    if (!defined $links{$flink}) { # try harder
        if ($link =~ m|\(\)>?$|) {
            my $t = $link;
            $t =~ s/\(\)(>?)$/$1/; # open() -> open, C<open()> => C<open>
            $t = findref_with_variations $t, @_;
            $flink = $t if defined $links{$t};
	} elsif ($link =~ /\([\dn]\)>?$/) { # perl(1)
            my $t = $link;
            $t =~ s/\([\dn]\)(>?)$/$1/; # f(2) -> f, C<f(2)> => C<f>
            $t = findref_with_variations $t, @_;
            if (defined $links{$t}) {
	      $flink = $t;
	    } else {
	      $unknown_manpages{$link}++; # May be converted to view.exe later
	    }
	}
    }
    print STDERR "trans: $link => $flink\n" if $debug_xref;
    $foundrefs++ if defined $links{$flink};
    return $flink;
}

sub add_ref {
    my $page = $_[0];
    my $num = $_[1];
    my $check = $_[2];

    return if $page =~ m,^perltoc/,;
    $page =~ s/\s*$//;
#warn "<<<adding ref $num to \"$page\"\n";

    $links{$page} = $num unless $check and exists $links{$page};
    (my $p = $page) =~ s/\s+/ /g;
    $links_nws{$p} = $page unless $check and exists $links_nws{$p};
    my $lc = lc $p;
    $links_lc{$lc} = $page unless $check and exists $links_lc{$lc};
    my $s = strip $page;
    $links_strip{$s} = $page unless $check and exists $links_strip{$s};
    my $b = auto_beautify($s);
    $links_b{$b} = $page unless $b eq $page or exists $links_b{$b};
    my $bb = auto_beautify($page);
    $links_b{$bb} = $page unless $bb eq $page or $bb eq $b;
}

sub addsection {
    my $section = $_[0];
    my $num = $_[1];
    my $level = $_[2];

    $head[$num] = $section;
    $headlevel[$num] = $level;
}

sub escape {
    my $l = $_[0];

    $l =~ s/\&/\&amp./g;
    $l =~ s/\:/\&colon./g;
    return $l;
}

sub remove_colon {
    my $in = shift;
    $in =~ s/\&colon\./:/g;
    $in;
}

sub escape_with_url {
    my $l = escape(shift);

    $l =~ s% ( \s | ^ )
             (
	      (?:http|file|ftp|mailto|news|newsrc|gopher)
	      \&colon\.//
	      [^\s<>]*
	      [^\s.,:;!?\"\'\)] # Strip trailing punctuation.
	     )
             ( [.,:;!?\"\'\)]* ( \s | $ ) | $ )
           % "$1" . link_url(remove_colon($2)) . "$2:elink.$3"
           %xeg ;

    return $l;
}

BEGIN {
    %HTML_Escapes =		# We provide a _practical_ list.
        (
         'amp'	=>	'&',	#   ampersand
         'lt'	=>	'<',	#   left chevron, less-than
         'gt'	=>	'>',	#   right chevron, greater-than
         'quot'	=>	'"',	#   double quote
         'sol'	=>	'/',	#   slash=solidus
         'verbar' =>	'|',	#   vertical bar
	 39	=>	"'",	#   single quote
	);
    %HTML_EscapesLit =		# We provide a _practical_ list.
        (
	 # Stolen from pod2latex
	 "Aacute"	 =>	"aa", #   capital A, acute accent
	 "aacute"	 =>	"aa", #   small a, acute accent
	 "Acirc"	 =>	"ac", #   capital A, circumflex accent
	 "acirc"	 =>	"ac", #   small a, circumflex accent
	 "AElig"	 =>	'Aelig', #   capital AE diphthong (ligature)
	 "aelig"	 =>	'aelig', #   small ae diphthong (ligature)
	 "Agrave"	 =>	"ag", #   capital A, grave accent
	 "agrave"	 =>	"ag", #   small a, grave accent
	 "Aring"	 =>	'Ao', #   capital A, ring
	 "aring"	 =>	'ao', #   small a, ring
	 #    "Atilde"	 =>	'\\~{A}',	#   capital A, tilde
	 #    "atilde"	 =>	'\\~{a}',	#   small a, tilde
	 "Auml"		 =>	'Ae',	#   capital A, dieresis or umlaut mark
	 "auml"		 =>	'ae',	#   small a, dieresis or umlaut mark
	 #    "Ccedil"	 =>	'\\c{C}',	#   capital C, cedilla
	 #    "ccedil"	 =>	'\\c{c}',	#   small c, cedilla
	 "Eacute"	 =>	"Ea", #   capital E, acute accent
	 "eacute"	 =>	"ea", #   small e, acute accent
	 "Ecirc"	 =>	"ec", #   capital E, circumflex accent
	 "ecirc"	 =>	"ec", #   small e, circumflex accent
	 "Egrave"	 =>	"eg", #   capital E, grave accent
	 "egrave"	 =>	"eg", #   small e, grave accent
	 #    "ETH"	 =>	'\\OE',		#   capital Eth, Icelandic
	 #    "eth"	 =>	'\\oe',		#   small eth, Icelandic
	 #    "Euml"	 =>	'\\"{E}',	#   capital E, dieresis or umlaut mark
	 #    "euml"	 =>	'\\"{e}',	#   small e, dieresis or umlaut mark
	 "Iacute"	 =>	"ia", #   capital I, acute accent
	 "iacute"	 =>	"ia", #   small i, acute accent
	 "Icirc"	 =>	"ic", #   capital I, circumflex accent
	 "icirc"	 =>	"ic", #   small i, circumflex accent
	 "Igrave"	 =>	"ig", #   capital I, grave accent
	 "igrave"	 =>	"ig", #   small i, grave accent
	 "Iuml"		 =>	'Ie',	#   capital I, dieresis or umlaut mark
	 "iuml"		 =>	'ie',	#   small i, dieresis or umlaut mark
	 "Ntilde"	 =>	'Nt', #   capital N, tilde
	 "ntilde"	 =>	'nt', #   small n, tilde
	 "Oacute"	 =>	"oa", #   capital O, acute accent
	 "oacute"	 =>	"oa", #   small o, acute accent
	 "Ocirc"	 =>	"oc", #   capital O, circumflex accent
	 "ocirc"	 =>	"oc", #   small o, circumflex accent
	 "Ograve"	 =>	"og", #   capital O, grave accent
	 "ograve"	 =>	"og", #   small o, grave accent
	 #    "Oslash"	 =>	"\\O",		#   capital O, slash
	 #    "oslash"	 =>	"\\o",		#   small o, slash
	 #    "Otilde"	 =>	"\\~{O}",	#   capital O, tilde
	 #    "otilde"	 =>	"\\~{o}",	#   small o, tilde
	 "Ouml"		 =>	'Oe',	#   capital O, dieresis or umlaut mark
	 "ouml"		 =>	'oe',	#   small o, dieresis or umlaut mark
	 #    "szlig"	 =>	'\\ss{}',	#   small sharp s, German (sz ligature)
	 #    "THORN"	 =>	'\\L',		#   capital THORN, Icelandic
	 #    "thorn"	 =>	'\\l',,		#   small thorn, Icelandic
	 "Uacute"	 =>	"Ua", #   capital U, acute accent
	 "uacute"	 =>	"ua", #   small u, acute accent
	 "Ucirc"	 =>	"uc", #   capital U, circumflex accent
	 "ucirc"	 =>	"uc", #   small u, circumflex accent
	 "Ugrave"	 =>	"ug", #   capital U, grave accent
	 "ugrave"	 =>	"ug", #   small u, grave accent
	 "Uuml"		 =>	'Ue',	#   capital U, dieresis or umlaut mark
	 "uuml"		 =>	'ue',	#   small u, dieresis or umlaut mark
	 #    "Yacute"	 =>	"\\'{Y}",	#   capital Y, acute accent
	 #    "yacute"	 =>	"\\'{y}",	#   small y, acute accent
	 "yuml"		 =>	'ye',	#   small y, dieresis or umlaut mark
        );
}

sub winhead {
    my $no = $_[0];

    if ($multi_win) {
        if (defined $wingroup[$no]) {
            return "group=$groups{links} x=left width=$panelwidths{links}";
        }
    }
    return "";
}

sub winlink {
    my $no = $_[0];
    
    if ($multi_win) {
        if (defined $wingroup[$no]) {
            return "group=$groups{sublinks} vpx=2% vpcx=$panelwidths{sublinks}";
        } else {
            return "group=$groups{text} dependent vpx=right vpcx=$panelwidths{text}"
        }
    } 
    return "";
}

sub no_markup_len { # quick hack
    my $l = $_[0];

    $l =~ s/\:.*?\.//g;
    $l =~ s/\&.*?\./x/g;
    return length $l;
}

sub insert_nl {
  print "\n" if not $was_nl or shift;
  $was_nl = 1;
}

sub do_libdir {
  local $_;
  $libdir = shift;
  chdir $libdir;
  debug("Looking in $libdir:");
  find (\&intern_modnamehash , '.');
  chdir $curdir;
}

sub intern_modnamehash {
# File::Find is pretty screwy.
# I think we can't modify $_ or File::Find can screw up

    my $shortpath;

# this could be a problem - if we search $sitelibdir,
# its usually a subdir of $libdir, in which case we don't want it
# to think 'site_perl' is a class name.

# site_perl and 5.00309 may be seen earlier than needed due to (unconvenient
# for reverse search) ordering of @INC.
  if ( defined $site_perl_prefix and
       $File::Find::name =~ m!/($site_perl_prefix|5\.(\d{3,5}|\d{1,2}\.\d{1,2}))/!o
       and $libdir !~ m!/($site_perl_prefix|5\.(\d{3,5}|\d{1,2}\.\d{1,2}))($|/)!o ) {
    return;
  }

# site_perl/5.9.1 and siteperl may be both in @INC.
  if ( !defined $site_perl_prefix and
       $File::Find::name =~ m!/5\.(\d{3,5}|\d{1,2}\.\d{1,2})/!
       and $libdir !~ m!/5\.(\d{3,5}|\d{1,2}\.\d{1,2})($|/)! ) {
    return;
  }

# XXX - may be doing toplevel modules incorrectly in the above case
# is 'name' just the filename?  thats not good ....
    $shortpath = $_;
    local $_ = $File::Find::name;

# kill leading './'

    s{^[.]/}{};
    my $longname = "$libdir/$_";
    $longname =~ s{^[.]/}{};

# XXX - take the current $libdir (/foo/bar) 
# and see if the file were testing (/foo/bar/site_perl/Plugh/Blah.pm) is
# in any *other*, deeper subdir in @INC
# (/foo/bar/site_perl) - if so, skip this entry, cuz the deeper 
# subdir will catch it properly (Plugh::Blah)

# for other libraries that are proper subdirs of the current libdir
    foreach $otherlibrary (grep /^\Q$libdir\E.+/, @INC) {

# if the other library is part of the current files path, skip it
# because it will be caught when the other library is used

	if ($longname =~ /^\Q$otherlibrary\//) {
	    print STDERR ".";
#	    print "Skipping $_\n";
#	    print "cuz $otherlibrary caught/will catch it\n";
	    return;
	}
    }

# exclude base pods - perlfoo.pod, but not perlfaqs
    /perl(?!faq).*[.]pod/ && $do_std && return;

# for each file entry, kill trailing '.(pod|pm|cmd)'.  Skip other extentions
    (-f $shortpath) and
	s{^(.*)[.](pod|pm|plx?|cmd|bat)$ }{$1}xi
      or $shortpath !~ /[.]/ or return;

# '.pod' files nonhierarchical - keep only last component as module name.
# well, hierarchical in Tk ... keep it hierarchical for now

#    if ($2 eq 'pod') {$_ =~ s{.*/([^/]+)}{$1}; }
    
# translate to module syntax

    s{/}{::}g;

# if its already in the hash, skip it.  We're following @INC order,
# which means if its found in a earlier @INC directory, it will
# be the one thats `use'd.  So rather than overwriting an earlier
# @INC entry with a newer one, we skip the newer one if the earlier
# one exists (or, we could do the foreach on (reverse @INC) instead
# of (@INC)).


    if (defined $seen{lc $_}) {
#	print "already found $_\n";
#	print "in $modnamehash{$_}\n";
	return
    };

# If this is a .pm file, is there actually any documentation in it?

# Under OS/2 perl utilites can have extension .cmd. To be safe, allow
# .bat as well. Since we look into $Config{bin}, we may allow files
# without extension as well, if they are text files.

	if (($longname =~ /[.](pm|plx?|cmd|bat|pod)$/i
	     or $shortpath !~ /[.]/) and -T $longname) {
	    $good = 0;
	    open(MODULE, $shortpath) or die "Cannot open `$shortpath': $!";
	  line: while (defined ($theline = <MODULE>)) {
	      $good = 1, last line if $theline =~ /^=head\d/;
	    }
	    $used_name = $_;
	    if ($good and $theline =~ /^=head\d\s+NAME\b/ ) {
	      my @alternative_names;

	      # Skip whitespace:
	      $theline = "";
	      $theline = <MODULE> while defined $theline and $theline !~ /\S/;
	      # Now have the name, find the description:
	      if ($theline =~ /^((\S+)(,\s+\S+)*)\s*-\s*(.*)/ ) {
		my $desc = $4;
		my $podname = $1;
		my $have_more = length($2) != length($1);

		if (exists $alias{$shortpath}) {
		  $used_name = $alias{$shortpath};
		} elsif ($believe_pod_name) {
		  $used_name = $podname;
		}
		my $skipNAME;	# safe to skip NAME section
		$name_from_pod = $2;
		{ $name_from_pod =~ s/\.pm$//; }
		$name_from_pod = strip($name_from_pod);
#		if ( $name_from_pod =~ /^os2::/i 
#		     and $libdir =~ m!/os2/?$! ) {
#		    (my $l = $libdir) =~ s,/os2/?$,,;
#		    return if $in_INC{$l};	# Will find it later...
#		}
		if ( $used_name =~ /^os2::/i and $libdir !~ m!/os2/?$! ) {
		   if ($seen_from_pod{$name_from_pod}) {
		     if ($in_INC{"$libdir/os2"}) {	# Already processed...
			print STDERR "\n... $_: seen already: `$name_from_pod'\n";
			return;
		     }
		     print STDERR "\n!!! $_: seen but $libdir/os2 not in \@INC: `$name_from_pod'???\n";
		   } else {
			print STDERR "\n!!! $_: bad match for unseen module: `$name_from_pod'\n";
		   }
		   # may have backward-compatibility portable library in @INC,
		   # but not the (assumingly) version specific .../os2
		   my $rest = ($used_name =~ /^os2::(.*)/i);
		   return if lc $rest eq lc $name_from_pod
				and $seen_from_pod{$name_from_pod};
		}
		if (lc($used_name) eq lc($name_from_pod)) {
		  # There is no sense to keep the NAME section due
		  # only to the long description, since the
		  # description is put *both* in the title of contents
		  # window, and at the top of contents window
		  $skipNAME = !$have_more; # && length(strip $theline) < 60;
		  # Prefer the name on the line over the file name:
		  $used_name = $name_from_pod;
		  # dumpValue(\%alternative_name);
		} elsif ($name_from_pod =~ /^OS2::(.*)/ and lc($used_name) eq lc($1)
			 and $libdir =~ m![\\/]os2([/\\])?$!) {
		     # Is not this obsolete?
		  $skipNAME = !$have_more && length(strip $theline) < 60;
		  $used_name = $name_from_pod;
		  # dumpValue(\%alternative_name);
		} else {
		  print STDERR "\n!!! Not matching: `$_' vs. `$name_from_pod'\n"
		    unless /perlfaq/;
		}
		$seen_from_pod{$name_from_pod}++;
		# Now process additional names this manpage may
		# appear under (first from the first line only):
		@alternative_names = ($podname =~ /([\w:.]*[\w:])/g);
		# Second from additional lines
		while (defined ($theline = <MODULE>)
		       and not $theline =~ /^=/) {
		  if ($theline =~ /^((\S+)(,\s+\S+)*)\s*-\s*(.*)/) {
		    push @alternative_names, ($1 =~ /([\w:.]*[\w:])/g);
		    $skipNAME = 0;
		  } elsif ($theline =~ /\S/) {
		    $skipNAME = 0;
		  }
		}
		my $f = $_;
		unshift @alternative_names, $f
		  unless $f eq $used_name or grep $_ eq $f, @alternative_names;
		@alternative_name{@alternative_names} = ($used_name) x @alternative_names;
		print STDERR "Auto-aliases `@alternative_names' for `$used_name'.\n" 
		  if $debug or @alternative_names > 1;
		$moddesc{$used_name} = $desc;
		if ($skipNAME and 0) { # Do not skip if NAME is the only section
		  $theline = <MODULE>
		    while defined ($theline) and not $theline =~ /^=(?!cut)/;
		  $skipNAME = 0 unless defined $theline;
		}
		$skip_sections{"$used_name/NAME"}++ if $skipNAME;
	      } else {
		print STDERR "\n!!! $_: bad NAME: `$theline'\n";
	      }
	    } elsif ($good) {
	      print STDERR "\n!!! $_: no NAME\n";
	    }
	    add_file($used_name, $longname) if $good;
	}

    echopod($_) if $modnamehash{$_};
}

sub add_file {
  my ($used_name, $longname) = @_;
  $seen{lc $used_name}++;
  $modnamehash{$used_name} = $longname;
  $alternative_name{$used_name} = $used_name;
}

sub debug {
  print STDERR "\n", '=' x 79, "\n$_[0]\n", '=' x 79 , "\n";
}

sub echopod {

    $savenew = $_[0];
    $oldpod ||= "";

# if neither has a ::, same line

    if ($oldpod !~ /::/ && $_[0] !~ /::/) {

# if old one has a ::, different lines

    } elsif ($oldpod =~ /::/ && $_[0] !~ /::/) {

	print STDERR "\n";

    } elsif ($oldpod !~ /::/ && $_[0] =~ /::/) {

# if its the new one that has ::, start a header line

	($new) = ($_[0] =~ /^(.+?)::/);
	print STDERR "\n${new} modules: ";
	$_[0] = $';

    } else {

# if both have ::, if stuff before first :: is different, newline
# if stuff before is the same, trim it before printing (same line)

	($old) = ($oldpod =~ /^([^:]+)::/);
	($new) = ($_[0] =~ /^([^:]+)::/);
	if ($old eq $new) {
	    # kill leading stuff
	    $_[0] = $';
	} else {
	    print STDERR "\n${new} modules: ";
	    $_[0] = $';
	}
    } 

    $oldpod = $savenew;
    
    print STDERR $_[0], " ";

}

sub hash_diff {
  my ($old, $new) = @_;
  my @keys = grep {not exists $old->{$_}} keys %$new;
  my %diff;
  @diff{@keys} = $new->{@keys};
  %diff;
}

# Retval: hash: keys: toplevel nodes, values: '' or refs to lower-level-hashes.
# Keys have :: prepended.
sub create_tree {		
  my $in = shift;
  my %branch;
  my (%ret, $leaf, $branch, $subbranch);
  
  # If $leaf is undef, it means ''. The rest has implicit :: prepended.
  foreach $leaf (@$in) {
    $ret{''} = '', next unless  defined $leaf;
    if ($leaf =~ /::/) {
      push @{$branch{$`}}, $';
    } else {
      push @{$branch{$leaf}}, undef; # Cooky to denote a leaf
    }
  }
  if (exists $ret{''} or keys %branch > 1) { # Need this level!
    foreach $branch (keys %branch) {
      $subbranch = create_tree($branch{$branch});
      if (keys %$subbranch > 1) {
	$ret{"::$branch"} = $subbranch;
      } else {
	$ret{"::$branch" . (keys %$subbranch)[0]} = '';
      }
    }
  } elsif (%branch) {		# This level is not needed, just copy sublevel.
    my $key = (keys %branch)[0];
    $subbranch = create_tree($branch{(keys %branch)[0]});
    foreach $leaf (keys %$subbranch) {
      $ret{"::$key$leaf"} = $subbranch->{$leaf};
    }    
  }  
  \%ret;
}

sub format_args {
  return "with no command-line arguments" unless @args;
  'with arguments C<"' . (join qq(">\nC<"), @args) . '">'
}

__END__

=head1 NAME

pod2ipf - translator from POD format to IBM's F<.INF> format.

=head1 SYNOPSYS

  cd \perllib\lib\pod
  pod2ipf > perl.ipf
  ipfc /inf perl.ipf

  pod2ipf my.pod > my.ipf

  pod2ipf my1.pod my2.pod > my.ipf

  pod2ipf --by-files "--title=My first book" \
          chapter1.pod chapter2.pod > mybook.ipf

  pod2ipf --by-dirs "--title=Book for /this/dir" /this/dir > book.ipf

  pod2ipf --by-dirs "--title=Book with chapters" \
          "--section-name=General topics" --dir=gen1 --dir=gen2 \
          "--section-name=Specific topics" --dir=spe1
          --dir=spe2 --dir spe3      >   longbook.ipf

=head1 DESCRIPTION

By default, if no command-line options: processes all the
standard Perl pods in the current directory, as well as all the Perl
libraries and all the Perl utilities it can find using F<Config.pm>,

The result should be converted to .INF via F<ipfc.exe>, with C</inf> switch.

Both steps may produce warnings, mostly because of malformed
C<POD>s.  [Currently, the only false warnings are some of C<Strip conversion>
warnings.]

Recognized command-line switches (with defaults);

  --title		Title of the INF file
  --(no)burst		Print Logo and About pages	(y)
  --(no)about		Print About page		(y)
  --(no)mods		Scan through @INC		(y)
  --(no)std		Scan ./ for standard Perl PODs	(y)
  --(no)bin		Scan through $Config{bin}	(y)
  --(no)tree		Output modules tree		(y)
  --(no)faqs		Output faqs			(y)
  --file		If present, do these files too (multiple OK)
  --dir			Which addnl directories to scan (multiple OK)
  --(no)dump-xref	Dump them to STDERR		(n)
  --(no)dump-contents	Dump it to STDERR		(n)
  --(no)dump-manpages	Dump unknown manpages to STDERR	(y)
  --(no)debug		Print an additional debug info	(n)
  --head-off		Offset of .IPF headings wrt POD	(2|0)
  --to-bold		If present, words to auto-make bold (multiple OK)
			 (EMX RSX WPS Object-REXX HPFS HTML WWW GNU Perl C
			  XFree86 OS/2 CRT PM DOS VIO CPAN IBM URL);
  --to-code		If present, words to auto-make code-like (multiple OK)
			 (VCPI DPMI groff awk gawk STDIN STDOUT STDERR Emacs
			  EPM CMD 4os2 sh pdksh zip unzip pkunzip man gcc
		 	  link386 tr PATH LIBPATH)
  --section-name	Groups next --dir/--file's into a section (multiple OK)
  --bin-dir		If present, search for binaries here too (multiple OK)
  --by-files		Interpret extra args as file names (n if options seen)
  --by-dirs		Interpret extra args as dir names (n)
  --www			Which browser to use. (%IPFWWW%||lynx||netscape)
  --(no)believe-pod-name Take names from NAME sections (n)
  --alias		Assign name to the preceding file (multiple OK)

Depending on the value of C<head_off>, the toplevel sections of the generated
book are formed basing on:

=over 4

=item 0

C<=head1>-entries of the POD document(s);

=item 1

processed POD documents;

=item 2

processed groups of POD documents (sections).

=back

Options C<--by-files> and C<--by-dirs> reset the values to

 --nodump-manpages --noburst --nobin --nomods --nostd --notree --nofaqs

and interpret the unprocessed command-line parameters as names of
files or directories to process.

If the first argument is not an option, an implicit C<--by-files> is
assumed.  If the only argument is not an option, an implicit
C<--head-off=0> is assumed.

=head2 URL

Found URLs are made into links.  Each link starts a browser; the browser
to start is configured at conversion time by the C<--www> option.  Multiple
browsers may be specified, then they are started one-by-one until one of
them succeeds.  Use C<||> to separate browsers.  The first C<@@@> in
the browser command is replaced by the URL.  If no C<@@@> is present,
C<@@@> is assumed at the end.

Since the default uses C<%IPFWWW%> as the first of the browsers, the user of
the generated files may specify his own browser by setting the
environment variable C<IPFWWW>.

=head1 PREREQUISITES

Developer toolkit for OS/2 is required (for C<ifpc>).  It is reported that C<ipfc> is also on DDK which is freely available from IBM site.

=head1 AUTHOR

C<Marko.Macek@snet.fri.uni-lj.si>, C<mark@hermes.si>,
Ilya Zakharevich C<ilya@math.ohio-state.edu>.

=head1 SEE ALSO

L<perlpod>, L<perl>, L<pod2man>, L<perldoc>, L<pod2html>, L<pod2latex>,  L<pod2texi>, L<pod2text>.

=cut

No docs:  L<pod2html>, L<pod2latex>,  L<pod2texi>, L<pod2text>,
