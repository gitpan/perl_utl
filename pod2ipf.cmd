extproc perl -wS
#!perl -w
use strict qw(refs subs);
use File::Find;
use File::Copy 'copy';
use Cwd;
use Config '%Config';
use Getopt::Long 'GetOptions';
use vars qw{%do_file_hash %do_dirs_hash %bin_hash %faqs_hash %pragmas_hash
	    %mods_hash %pod_hash %add_mods_hash @add_dirs %tree_hash};
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

$VERSION = "1.11";

# by Marko.Macek@snet.fri.uni-lj.si, mark@hermes.si
# with additions by Ilya Zakharevich ilya@math.ohio-state.edu
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
#   remove =head1 NAME and use it as toplevel heading
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

$font = ''; #':font facename=Helv size=16x8.';

$debug = 0;
$debug_xref = 0;
$dump_xref = 0;
$dump_contents = 0;
$dump_manpages = 1;
$ref_delta = 1;     # start from 1
$maxtoc = 5;
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
@add_dirs = ();
my @args = @ARGV;
my $foundrefs = 0;
my %i1ids;
my %index_seen;
my %index_output;
my $www = 'lynx.exe';

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
    @add_dir = ($cat_descr{do_dirs},[]) unless @add_dir;
    push @{$add_dir[-1]}, @do_dirs;
    @do_dirs = ();
  }
  push @add_dir, $name, [] if @_;
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
	   "std!" => \$do_std,	# Scan through standard Perl PODs
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
	   "section-name=s" => \&add_dir,
	   "bin-dir=s@" => \@bin_dirs, # If present, search for bins here too
	  );
if ($by_dirs) {
  push @do_dirs, @ARGV;
} elsif ($by_files) {
  push @do_file, @ARGV;
} else {
  warn "Ignoring \@ARGV: `@ARGV'.\n" if @ARGV;
}

add_dir();
$do_about = 1 if $do_burst;

@make_bold = qw(EMX RSX WPS Object-REXX HPFS HTML WWW GNU Perl C
		XFree86 OS/2 CRT PM DOS VIO CPAN IBM URL) unless @make_bold;
@make_code = qw(VCPI DPMI groff awk gawk STDIN STDOUT STDERR Emacs EPM
		CMD 4os2 sh pdksh zip unzip pkunzip man gcc link386 tr
		PATH LIBPATH) unless @make_code;

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
    # Fake File::Find
    $File::Find::name = $_ = $file;
    $libdir = ".";
    intern_modnamehash();
  }
}
%do_file_hash = %modnamehash;
%old_hash = %modnamehash;
{
  no strict 'refs';
  foreach (1 .. @add_dir/2) {
    foreach $libdir (@{$add_dir[2*$_-1]}) {
      do_libdir $libdir;
    }
    print STDERR "Doing section `$_' named `$add_dir[2*$_-2]': `@{$add_dir[2*$_-1]}'.\n" if $debug;
    %{"do_dirs$ {_}_hash"} = hash_diff(\%old_hash, \%modnamehash);
    $cat_descr{"do_dirs$_"} = $add_dir[2*$_-2];
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
%bin_hash = hash_diff(\%old_hash, \%modnamehash);

@modnames = sort keys %modnamehash;
  
print STDERR "Found `@modnames'.\n";
  
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
    last if /^\S/;
    push @files, [$1, $2] if /^\s+(\S*)\s+(.*)/ and $1 !~ /^perlfaq/; 
				# and $1 ne 'perltoc';
  }
  close MPOD;
  open MPOD, 'perltoc.pod';
  while (<MPOD>) {
    last if /^=head1\s+pragma/i;
  }
  while (<MPOD>) {
    last if /^=head1/;
    push @pragmas, $1 if /^=head2\s+(\S*)\s+-\s/; 
  }
  close MPOD;
  foreach $key (@pragmas) {
    $pragmas_hash{$key} = delete $mods_hash{$key};
  }
  splice @files, 1, 0, [ 'perlos2',      'Perl under OS/2' ],
     [ 'perltoc',      'Internal table of contents for Perl' ];
  push @files, [ 'perlinstall',  'Installation/compilation of Perl'],
               ['Pumpkin', 'Notes on handling the Perl Patch Pumpkin'];
  if (-f '../INSTALL' and not -f 'perlinstall.pod') {
      copy '../INSTALL', 'perlinstall.pod';
  }
  if (-f '../Porting/pumpkin.pod' and not -f 'Pumpkin.pod') {
      copy '../Porting/pumpkin.pod', 'Pumpkin.pod';
  }
  if (-f '../README.os2' and not -f 'perlos2.pod') {
      copy '../README.os2', 'perlos2.pod';
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
    if ($moddesc{$file} =~ s/(\(\$.*\$\))//) {
      $add_info{$file} = $1;
    }
  }
  unless ($do_mods) {
    %mods_hash = %add_mods_hash = ();
  }
}

foreach $module (keys %skipNAMEs) {
  $skip_sections{"$module/NAME"}++;
}

#if ($do_tree) {
#  create_tree([keys %modnamehash]);  
#}

my @std_categories = (qw(pod pragmas mods add_mods bin faqs do_dirs),
		      (map "do_dirs$_", 1 .. @add_dir/2),
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
  push @files, [$name, ($moddesc{$name} || "Perl module $name")]
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
@auto_link = grep /::|_|[a-z][A-Z]|^[^aAeEoOyYiIuU]{3,}$/, keys %addref;
@auto_link{@auto_link} = (1) x @auto_link;


# This is the rest without vowels, will be highlighted only in obvious places:
@auto_link_hard = grep !/::|_|[a-z][A-Z]|^[^aAeEoOyYiIuU]+$/, keys %addref;
@auto_link_hard{@auto_link_hard} = (1) x @auto_link_hard;

sub out;
sub contents;
sub escape;
sub addref;
sub findref;
sub winhead;
sub winlink;
sub no_markup_len;
sub insert_nl;

$/ = "";

foreach $sc (@split_sections) { $as_head{$sc} = 1; }

foreach $sc (@index_sections) { $fine_index{$sc} = 1; }

if (not defined $DocTitle) {
  $DocTitle = @files >= 2 ? 'Perl Manual' : $files[0][0];
  $DocTitle = escape($DocTitle);
}

$in_item_header = 0;

for ($pass = 1; $pass <= 2; $pass++) {
    if ($pass == 2) {
      $auto_link_hard = join '|', keys %auto_link_hard;
      $auto_link = join '|', keys %auto_link;
      $auto_link_both = join '|', keys %auto_link, keys %auto_link_hard;
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
Generated on @{[out(scalar localtime, 1)]}, by
:link reftype=hd group=$groups{links} dependent vpx=left vpcx=$panelwidths{links} refid=@{[findrefid('perl')]}.Perl:elink.
version $], 
:link reftype=hd group=$groups{links} dependent vpx=left vpcx=$panelwidths{links} refid=@{[findrefid('pod2ipf')]}.pod2ipf:elink.
version $VERSION @{[format_args]} in directory @{[escape(cwd)]}.

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
        my $fcomment = $moddesc{$ftitle} || "Perl module $ftitle";
	
        $fname = $ftitle . '.pod';
	if (not -f $fname) {
	  $fname = $modnamehash{$ftitle};
	}
        $page = $ftitle;
        $toc = $maxtoc;
	@index = ();
        
        open(IN, $fname) || die "$ftitle: open `$fname': $!";
        print STDERR $fname . ": ";
        print STDERR "\n" if !$dots;

        $section = $ftitle . ' - ' . $fcomment;
        if ($pass == 1) {
            addsection($section, $headno, 1);
            addref($page, $headno);
	    $is_head{$ftitle}++;
        }
        $section_head[1] = $page;
        $path = $section_head[1];
        if ($pass == 2) {
	    insert_nl;
	    my $hlevel = $head_off >= 1 ? $head_off : 1;
	    insert_back($hlevel,$headno - 1);
            print ":h$hlevel toc=$toc " . winhead($headno)
                . " id=" . ($headno + $ref_delta) . "."
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
        $headno++;
        
        @lstack = ();
        $emptypane = $emptysection = 1;
	$inpod = 0;
        
        PARA: while (defined ($line = <IN>)) {
            chomp $line;
	    if ($line =~ /^=\w+/) {
	      if ($line =~ /^=head(\d+)\b\s*/) {
	        $inpod = 1;
                $nopara = 0;
                $heading = $';
		{
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

		$old_hl = $hl;
                $hl = $1 + 1;
                $section_head[$hl] = $heading;
                $path = join('/', @section_head[1..$hl]);
		$sh_path = join('/', @section_head[1..$hl-1]);
		if ($skip_sections{$path}) {
		  $inpod = 0;
		  next PARA;
		}
                contents($hl, $headno) if $emptypane;
		insert_back($old_hl,$headno); # Previous header
                if ($pass == 1) {
                    addsection($heading, $headno, $hl);
		    # XXXX Is wrong with some escapes:
		    #1 while $heading =~ s/[A-Z]<.*?>/$1/g;
                    addref(qq|$page/"$heading"|, $headno);
		    $is_head{$heading}++;
                }
                if ($pass == 2) {
		    insert_nl;
                    print ":h", $hl + $head_off - 1 , " " . winhead($headno)
                        . " id=" . ($headno + $ref_delta) . "."
                        . out($heading, 0) . "\n" . $font; # Headers take no fonts
		    output_index($heading, $path);		    
		} else {
		    count_index($heading);
		}
                $headno++;
                print STDERR "." if $dots;
                $emptypane = $emptysection = 1;
	      } elsif ($line =~ /^=over\b\s*/) {
	        $inpod = 1;
		$step = 5;	# Default
		$step = $& if $' =~ /\d+/;
		$step = int($step * 4/3 + 0.5); # Take into account proportional font
                # look ahead, to see how the list should look like
		if ($pass == 1 and $inpod) {
		  $auto_link_hard{$1}++,
		  $x_index{$1} = $headno
		    while $line =~ /X<([^<>]+)>/g;
		}
                chomp($line = <IN>);
		if ($pass == 1) {
		  $auto_link_hard{$1}++ while /X<([^<>]+)+>/g;
		}
                if ($line =~ /^\=item(\s*$|\s+\*)/) { # item * (or empty)
                    push(@lstack, "ul");
		    insert_nl if $pass == 2;
                    print ":ul.\n" if $pass == 2;
		    $was_nl = 1;
                } elsif ($line =~ /^\=item\s+1\.?/) {  # item 1. 
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
                    $eitems = "";
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
                $headx = $heading;
		$headx =~ s/E<(.*?)>/$HTML_Escapes{$1}/ge; # Primitive: $<digit>
		1 while $headx =~ s/[A-Z]<(.*?)>/$1/g; # Primitive: $<digit>
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
                    contents($hl, $headno) if $emptypane;
		    insert_back($hl,$headno) unless $emptysection; # Previous header

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
                    if ($pass == 1) {
                        addsection($heading, $headno, $hl);
                        addref(qq|$page/"$headx"|, $headno);
                        addref(qq|$page/"$word1"|, $headno) if defined $word1;
                        addref(qq|$page/"$word2"|, $headno) if defined $word2;
                    }
                    $section_head[$hl] = $heading;
                    $path = join('/', @section_head[1..$hl]);
		    $sh_path = join('/', @section_head[1..$hl-1]);
		    insert_nl if $pass == 2;
		    print ":h", $hl + $head_off - 1, " " . winhead($headno)
		      . " id=" . ($headno + $ref_delta) . "."
			. out($heading, 0) . "\n" . $font 
			  if $pass == 2; # Headers take no fonts
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
                    $headno++;

                    # look ahead to see if this =item is empty.
                    # if it is, create a list of empty pages of
                    # on first non-empty.
		    if ($pass == 1 and $inpod) {
		      $auto_link_hard{$1}++,
		      $x_index{$1} = $headno
			while $line =~ /X<([^<>]+)>/g;
		    }
                    chomp($line = <IN>);
                    if ($pass == 2) {
                        if ($line =~ /^=item\b/) {
                            $eitems .= $heading . "\n";
                        } elsif ($eitems ne "") {
                            $eitems .= $heading . "\n";
                            foreach $l (split("\n", $eitems)) {
                                print ":p.:hp2." . out($l, 1) . ":ehp2.";
                            }
                            $eitems = "";
                        }
		    } else {
		      $auto_link_hard{$1}++ while /X<([^<>]+)+>/g;
		    }
                    redo PARA;
                } else {	# Different list's items
		    local $in_item_header = 1;
                    $emptypane = $emptysection = 0;
		    addref(qq|$page/"$headx"|, $headno, 1);
                    if ($lstack[$#lstack] eq 'ul' && $heading =~ /^\s*\*\s*(.*)$/ or
                        $lstack[$#lstack] eq 'ol' && $heading =~ /^\s*\d+\.?\s*(.*)$/)
		      {		# Bulleted or numbered item matching list type.
                            print ":li." if $pass == 2;
                            $heading = $1;
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
	      } else {
                warn "what to do with '$line'?\n";
	      }
            } elsif ($inpod == 0) {
	      # Just ignore this chunk
            } elsif ($line =~ /^\s+\S/) {
                if ($pass == 2) {
                    $pre = untabify($line);
		    insert_nl;
                    print ":xmp.\n" . escape_with_url($pre) . "\n:exmp.\n";
		    $was_nl = 1;
		}
                $nopara = 0;
                $emptypane = $emptysection = 0;
            } elsif ($line =~ /^\s+\S/m) { # see perl(run)?/SYNOPSIS for this
                if ($pass == 2) {
                    $mark = out($line, 1);

                    # hack hack ;-)
                    # IPFC doesn't handle tabs
                    # no_markup_len tries to guess the # of ' ' to next tab,
                    # but even when the guess is correct, things don't seem
                    # to align when bold,.. is used :-(
                    $pre = untabify_after($mark);
                    
		    insert_nl;
                    print ":xmp.\n" . $pre . "\n:exmp.\n";
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
	    if ($pass == 1 and $inpod) {
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

sub output_index {
  return &count_index if $pass == 1;
  my ($heading, $path) = (shift, shift);
  $heading = substr($heading, 0, 110) . "..." if length $heading > 124;
  $path = substr($path, 0, 110) . "..." if length $path > 124;
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

sub count_index { $index_seen{shift()}++ }

sub maybe_link {
  my $txt = shift;
  exists $links{findref($txt)} ? "L<$txt>" : $txt;
}

sub strip {
    my $in = shift;

    1 while $in =~ s/X<([^<>]*)>//;
    1 while $in =~ s/[A-Z]<([^<>]*)>/$1/;
    
    return $in;
}

sub try_external_link {
    my $txt = shift;
    $foundrefs++, return ":link reftype=hd refid=$x_index{$txt}."
      . out($txt) . ":elink."
	if exists $x_index{$txt};
    
    print STDERR "trying `$txt'" if $debug;
    
    if ($txt =~ m,^(http|file|ftp|mailto|news|newsrc|gopher)://,) {
	my $link = strip($txt);
	return 
	  ":link reftype=launch object='$www' data='$link'."
	    . out($txt) . ":elink.";

    } elsif ($txt =~ m,^\"(http|file|ftp|mailto|news|newsrc|gopher)://, 
	     and $txt =~ /\"$/) {
	my $link = strip(substr $txt, 1, length($txt) - 2);
	return 
	  ":link reftype=launch object='$www' data='$link'."
	    . out($txt) . ":elink.";

    } elsif ($txt =~ m,^(\w+)\([23]\)|POSIX\s*\(3\)/(\w+)|(emx\w+)$,i) {
	return 
	  ":link reftype=launch object='view.exe' data='emxbook $+'."
	    . out($txt) . ":elink.";
    }
    return undef;
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
    $para =~ s/ (^|[^<]) \b ( $auto_link_hard ) \b (?=$|[^>]) /$1L<$2>/gox
	if $section_head[$hl] eq "SEE ALSO" and $auto_link_hard;
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
    $para =~ s/ (^|[^<]) \b ( [\w:]+ \b \(\) ) (?=$|[^>]) /$1C<$2>/gx; # func()
    $para;
}

sub out {
    my $para = $_[0];
    my $markup = $_[1];
    my $beautify = $markup && ! $_[2];
    my @stack = ();
    my $output = "";
    my ($c, $cpos, $opos);

    return if ($pass == 1);
    
    $para = auto_beautify($para) if $beautify;
    
    $cpos = 0;
    $opos = 0;
    TAG: while ($para =~ m{([<>])}g) { # ;-) ;-)
        $cpos = pos $para;
        $c = $1;
        
        if ($c eq '<' && $cpos >= 0) {
            $output .= escape(substr($para, $opos, $cpos - $opos - 2))
	      if $cpos - $opos > 2;
            
            $c = substr($para, $cpos - 2, 1);
            if ($c !~ /[A-Z]/) {
	      $output .= escape(($cpos - $opos > 1 ? $c : '') . '<');
                pos($para) = $opos = $cpos;
                next TAG;
            }
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
                $output .= ':font facename=Courier size=18x10.' if $markup;
                push (@stack, $c);
            } elsif ($c eq 'L') {
                my $link;
	        #push (@stack, $c);
                # link
                pos $para = $cpos;
		# Allow one level of included modifiers:
                if ($para =~ m/\G(([A-Z]<[^<>]*>|[^>])+)\>/g) {
                    $cpos = pos $para;
                    $link = $1;
                    $foundlink = findref($link); 
                    if (defined $links{$foundlink}) {
		        my $blink = $link;
			$blink =~ s|^"(.+)"$|$1|sm or
			  $blink =~ s|^([-\w:]+)/"(.+)"$|$1: $2|sm;
                        $output .= ":link reftype=hd refid=" .
                            ($links{$foundlink} + $ref_delta) . '.'
                            if $markup;
                        $output .= out($blink);
                        $output .= ":elink." if $markup;
                    } elsif ($foundlink = try_external_link($link)) {
                        $output .= $foundlink if $markup;
		    } else {
                        warn "   unresolved link: $link\n";
                        $output .= out($link);
                    }
                }
            } elsif ($c eq 'E') {
                pos ($para) = $cpos;
                if ($para =~ m/\G(([A-Za-z]+)|\d+)>/g) {
                    my $esc;
                    $cpos = pos $para;
		    if (defined $2) {
		      $esc = exists $HTML_Escapes{$1} 
		        ? $HTML_Escapes{$1} : "E<$1>";
		    } else {
		      $esc = chr $1;
		    }
                    $output .= escape($esc);
                } else {
                    warn "$fname: E<> ??? `" . (substr $para, $cpos-2, 10) . "'\n";
                }
            } elsif ($c eq 'X') {
                pos ($para) = $cpos;
                if ($para =~ m/\G([^<>]+)>/g) {
                    my $esc;
                    $cpos = pos $para;
                    #$output .= escape($1);
		    push @index, $1 if $print_index;
                } else {
                    warn "$fname: X<> ??? `" . (substr $para, $cpos-2, 160) . "'\n";
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
        } elsif ($c eq '>' && $#stack >= 0) {
            $output .= escape(substr($para, $opos, $cpos - $opos - 1));
            
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
                $output .= ':font facename=default size=0x0.' if $markup;
            } elsif ($c eq 'L') {
                # end link
            } else {
                $output .= escape('>');
            }
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
    $output =~ s/^\./&per./m;	# period
    return $output;
}

sub insert_back {
  return unless $pass == 2;
  my $parent = find_parent(@_) + $ref_delta;
  return if $parent == $_[1];
  insert_nl;
  print " :link reftype=hd refid=$parent.:font facename=Courier size=8x6.Go Up:font facename=default size=0x0.:elink.\n";
  $was_nl = 1;
}

sub find_parent {
    my $level = $_[0];
    my $i = $_[1] - 1;

    while ($i > 0 && $headlevel[$i] >= $level) { $i--; }

    return $i;
}

sub contents {
    my $level = $_[0];
    my $no = $_[1];
    my ($i, $cl, $toplevel);
    local $print_index = 0;

    $isempty{$no-1}++;
    if ($pass == 1) {
        $wingroup[$no - 1] = $groups{links};
        return ;
    }
    
    $i = find_parent($level,$no);

    $toplevel = $headlevel[$i];

    print ":p." . out($head[$i], 1) . "\n";
    $was_nl = 1;
    $i++;
    $cl = $toplevel;
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
	} else {
	  print ":li.:link reftype=hd " . winlink($i)
            . " refid=" . ($i + $ref_delta) . "."
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
  ($out || 0) + $ref_delta;
}

sub findref { # various heuristics to get a valid link
    my $link = $_[0];
    
    $link =~ tr/\n/ /;
    print STDERR "link: $link\n" if $debug_xref;
    if (!defined $links{$link}) {
      if ($link =~ m|\.pod/|) {
	$link = "$`/$'";	# Remove .pod from page name
      } elsif ($link =~ m|\.pod$| and defined $links{$`}) {
	$link = $`;
      }
    }
    if (!defined $links{$link}) { # try harder
        if (defined $links{qq|$page/"$link"|}) {
            $link = qq|$page/"$link"|;
        } elsif ($link =~ /^\"/) {
            $link = "$page/$link";
        } elsif ($link =~ m|^/\"|) {
            $link = "$page$link";
        } elsif ($link =~ m|^/|) {
            $link = qq|$page/"$'"|;
        } elsif ($link =~ m|^([^/ ]+)/([^\"]+)$|) {
            $link = qq|$1/"$2"|;
        } elsif (exists $addref{$link} and exists $links{$addref{$link}}) {
            $link = $addref{$link};
	} elsif ($link =~ /^-[^\W\d]$/) {
	    $link = qq|perlfunc/"-X"|;
	}
        if ($link =~ m|^([^/ ]+)/"([^\"]+)"$| && !defined $links{$link}) {
            my $a = $1;
            my $b = $2;
            my $linka;
            
            if ($b =~ /\(\)$/) { $b = $`; } # open() -> open, ...
            $linka = qq|$a/"$b"|;
            if (defined $links{$linka}) {
                $link = $linka;
            }
	} elsif ($link =~ /\([\dn]\)$/ && defined $links{$`}) {	# perl(1)
	  $link = $`;
	} elsif ($link =~ /\([\dn]\)$/) {
	  $unknown_manpages{$link}++;
	}
        print STDERR "trans: $link\n" if $debug_xref;
    }
    $foundrefs++ if defined $links{$link};
    return $link;
}

sub addref {
    my $page = $_[0];
    my $num = $_[1];
    my $check = $_[2];

    $page =~ s/\s*$//;
    
    $links{$page} = $num unless $check and exists $links{$page};
    if ($page =~ /[A-Z]</) {
      1 while $page =~ s/[A-Z]<(.*?)>/$1/;
      $links{$page} = $num unless $check and exists $links{$page};
    }
    my $b = auto_beautify($page);
    $links{$b} = $num unless $b eq $page;
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
           % "$1:link reftype=launch object='$www' data='" 
	     . remove_colon($2)
	     . "'.$2:elink.$3" 
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
	 39	=>	"'",	#   single quote
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

# site_perl and 5.00309 may be seen earlier than needed due to bad
# ordering of @INC.
  if ( defined $site_perl_prefix and
       $File::Find::name =~ m!/($site_perl_prefix|5\.\d{3,5})/!o
       and $libdir !~ m!/($site_perl_prefix|5\.\d{3,5})($|/)! ) {
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

# for each file entry, kill trailing '.(pod|pm|cmd)'
    (-f $shortpath) &&
	s{^(.*)[.](pod|pm|cmd|bat)$ }{$1}x or return;

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
    
	if ($longname =~ /[.](pm|cmd|bat|pod)$/i
	    or $longname !~ /[.]/ and -T $longname) {
	    $good = 0;
	    open(MODULE, $shortpath) or die "Cannot open `$shortpath': $!";
	  line: while (defined ($theline = <MODULE>)) {
		$theline =~ /^=head\d/ and $good = 1 and last line;
	    }
	    $used_name = $_;
	    if ($good and $theline =~ /^=head\d\s+NAME\b/ ) {
	      my @addrefs;
	      
	      # Skip whitespace:
	      $theline = "";
	      $theline = <MODULE> while defined $theline and $theline !~ /\S/;
	      # Now have the name, find the description:
	      if ($theline =~ /^((\S+)(,\s+\S+)*)\s*-\s*(.*)/ ) {
		my $desc = $4;
		my $skipNAME;	# safe to skip NAME section
		if (lc($used_name) eq lc($2)) {
		  $skipNAME = length($2) == length($1);
		  # dumpValue(\%addref);
		} else {
		  print STDERR "\n!!! Not matching: `$_' vs. `$2'\n"
		    unless /perlfaq/;
		}
		$firstline_name = $2;
		# Now process additional names this manpage may
		# appear under (first from the first line only):
		@addrefs = ($1 =~ /([\w:]+)/g);
		# Second from additional lines
		while (defined ($theline = <MODULE>) 
		       and not $theline =~ /\A=/) {
		  if ($theline =~ /^((\S+)(,\s+\S+)*)\s*-\s*(.*)/) {
		    push @addrefs, ($1 =~ /([\w:]+)/g);
		    $skipNAME = 0;
		  } elsif ($theline =~ /\S/) {
		    $skipNAME = 0;
		  }
		}
		# Prefer the name on the line over the file name:
		if ($skipNAME and $used_name ne $firstline_name) {
		  $used_name = $firstline_name;
		}
		@addref{@addrefs} = ($used_name) x @addrefs;
		print STDERR "Adding `@addrefs' for `$used_name'.\n" if $debug;
		$moddesc{$used_name} = $desc;
		$skipNAMEs{$used_name}++ if $skipNAME;
		#print STDERR "moddesc: `$_' `$oldname' `$3'\n";
	      } else {
		print STDERR "\n!!! $_: bad NAME: `$theline'\n";
	      }
	    } elsif ($good) {
	      print STDERR "\n!!! $_: no NAME\n";
	    }
	    if ($good) {
	      $seen{lc $used_name}++;
	      $modnamehash{$used_name} = $longname;
	      $addref{$used_name} = $used_name;
	    }
	}

    echopod($_) if $modnamehash{$_};
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

	($new) = ($_[0] =~ /^([^:]+)::/);
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
  out('with arguments C<"' . (join '"> C<"', @args) . '">', 1);
}

__END__

=head1 NAME

pod2ipf - translator from POD format to IBM's F<.INF> format.

=head1 SYNOPSYS

  cd \perllib\lib\pod
  pod2ipf > perl.ipf
  ipfc /inf perl.ipf

  pod2ipf my.pod > my.ipf

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

The result should be converted to .INF via F<ipfc.exe>.

Both steps produce a lot of warnings, mostly because of malformed
C<POD>s. Some warnings (n306) from the current design, which intentionally
generates empty pages.

Recognized command-line switches (with defaults);

  --(no)burst		Print Logo and About pages	(y)
  --(no)about		Print About page		(y)
  --(no)mods		Scan through @INC		(y)
  --(no)std		Scan through standard Perl PODs	(y)
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
  --to-bold		If present, words to make bold (multiple OK)
  --to-code		If present, words to make code-like (multiple OK)
  --section-name	Groups following --dir into a section (multiple OK)
  --bin-dir		If present, search for binaries here too (multiple OK)
  --by-files		Interpret extra args as file names (n if options seen)
  --by-dirs		Interpret extra args as dir names (n)
  --www			Which browser to use		(lynx.exe)

Depending on the value of C<head_off>, the toplevel sections of the generated book are formed basing on:

=over 4

=item 0

C<=head1>-entries of the POD document(s);

=item 1

processed POD documents;

=item 2

processed groups of POD documents.

=back 

Options C<--by-files> and C<--by-dirs> reset the values to

 --nodump-manpages --noburst --nobin --nomods --nostd --notree --nofaqs

and interpret the unprocessed command-line parameters as names of
files or directories to process.

=head1 PREREQUISITES

Developer toolkit for OS/2 is required (for C<ifpc>).  It is reported that C<ipfc> is also on DDK which is freely available from IBM site.

=head1 AUTHOR

C<Marko.Macek@snet.fri.uni-lj.si>, C<mark@hermes.si>, reworked
by Ilya Zakharevich C<ilya@math.ohio-state.edu>.

=head1 SEE ALSO

L<perlpod>, L<perl>, L<pod2man>, L<perldoc>, L<pod2html>, L<pod2latex>,  L<pod2texi>, L<pod2text>.

=cut

No docs:  L<pod2html>, L<pod2latex>,  L<pod2texi>, L<pod2text>,
