#!/usr/bin/perl
# From: ilya@math.ohio-state.edu (Ilya Zakharevich)
# So if you do not want to
# descend into directories, just do
# 	rnm '$_ = lc'
# (with 5.004 or close, with perls which are older than a week you need
# 	rnm '$_ = lc $_'
# ).
# 
# I wrote a similarly nice tool which does a similar thing to
# "find". Use it like this:
# 	pfind . '$_ = lc'
# 
# To lowercase only the files which names contain `blah', do
# 	pfind . /blah/ '$_ = lc'
# 
# To lowercase only the files which contain `blah', do
# 	pfind . "=~ /blah/" '$_ = lc'

# 1.5:	correctly open files with leading/trailing spaces in names.
# 1.6:	handle substitutions, -bak;
#	-nosubdir; 
#	-debug exits;
#       -nosubdir bug fixed
#       inserts a stat($_) iff any filter matching /^!?-(M|A|C)/ is used
#       this allows for    -M _ < 2
#       inserts  `if test'  instead of  `unless ! test' if  !test is given
# 1.8:	Preserves attributes of the file if s///
#	Optimizations of -f to -f _ performed
#	Pod added
# 1.11: prt() somehow was not printing the name of the file, but line as well;
#	prt_line() added which prints the line as well;
#	-nosubdir was ignored;
# 1.12: Added du() convenience function
#	Do not stat() the file at all.  Force File::Find to stat() if needed.
#	The above trick did not work.  Backing up to the older way...
#	Doc updates.
# 1.13: $dir =~ /blah/ is considered as a rule too;
#	rename() is more verbose now.
# 1.14+ better error message for non-writable files
#	die on unknown options.
#	New option -alllines.
#	New convenience function skip().
#	New option -utf8
# 1.14.2 better error message for unknown option
# 1.14.3 New patch from Jarausch: make prt_lines default for -alllines, bugfix
# 1.15  Merged patches after 1.14 into the RCS.
# 1.16  NeedLStat patch from Jarausch.
# 1.17  *Very* much redone 'inter' patch from Jarausch.
# 1.18  -vars accepted only 2
# 1.19  Allow merging of content regular expressions by &&& (possibly
#       negated) and negation of match-contents demand.

use strict;
use vars qw($Line_Found $rcs $debug $prune $bak
          $StartDir $binary $haveS $NeedsStat $NeedsLStat $MultiPrint
            $InterActiveProc $Vars);
undef $Line_Found;

$rcs = ' $Id: pfind.pl,v 1.20 2003/11/02 08:41:31 vera Exp vera $ ' ;

sub usage {
  if ($] < 5.00456 or system 'perldoc', '-F', "$0" ) {
    warn "    perldoc -F $0 could not find us!  Short version follows:\n";
    my $version = (split ' ', $rcs)[2];
    
    print <<EOU;
usage: $0 [-Options] [--] startdir rule1 rule2 ...
			>>>> Version: $version. <<<< 
	Options are [-debug] [-nosubdir] [-bin] [-bak=suffix] [-alllines].
	Rules are perl statements to execute. Statements starting with
`-', `/', or `!' are considered filters, a file will be discarded unless
the statement returns true. The rest is executed "as is".
	If only filters are given the default action is `prt' (see below).
	Variables \$_, \$name, \$dir contain the file name, full file name
and the name of the directory. Statements are executed in the directory
of the each processed file.
	If file is not discarded, and \$_ is changed after the perl
statements are executed, the file is renamed to the new value of \$_.
	Convenience function `prt' prints \\n-terminated name of the file.

	If a rule starts with `=~', it is considered as a filter to
match contents of the file. The rest is the regular expression to
match. If regexp contains modifiers `s' or `m', it is matched against
the file as a whole, otherwise it is matched line-by-line. 
	If the regexp-filter fails, file processing stops. Otherwise 
whatever was matched against is available in the variable \$line unless
modifiers `s' or `m' are given.
	If a rule starts with `=~s' it is considered as a filter to
match and MODIFY the contents of the file. Modifiers 's' and 'm' are applicable
as above. To make several modifications at a time, use e.g.
=~ s/abc/ABC/ &&& s/def/DEF/ (modifiers should be the same, though).
In case you give the modifiers 's' or 'm' you should think about using
the modifier 'g', as well.  See POD docs for the convenience function `inter'.

Option -debug just shows and evals the expression - IT DOES NO PROCESSING !
With option -nosubdir does not descend into subdirectories.
Without option -bin binary files are not processed for substrings,
and files are opened in text mode.  -alllines: do for all the matching lines.
EOU
  }
  ;                             # '; # for Emacs
  exit;
}

sub skip { 
  $File::Find::prune=1;
}

sub prt { 
  print "$File::Find::name\n";
}

sub prt_line { 
  print "$File::Find::name";
  print " : $Line_Found"  if  defined($Line_Found);
  print "\n" unless $Line_Found =~ /\n\Z/;
}

my $Total_Size;
my $Num_Files;

sub du {
  $Num_Files++;
  $Total_Size += ($NeedsStat ? -s _ : -s $File::Find::name);
}

sub do_rename {
  my ($from, $to) = @_;
  print STDERR "rename `$File::Find::dir/$from'\t===> `$File::Find::dir/$to'\n";
  rename $from, $to
    or warn("Cannot rename: $!\n"), return 0;
}

sub setup_bak {
  my ($file,$bak) = @_;
  return unless -e "$file$bak";
  my ($prefix,$count,$i) = ("", $bak, 0);
  unless ($bak =~ /^\w+\d+$/) {
    ($prefix,$count) = ($bak, 0);
  }
  $count++, $i++ while $i < 1000 and -e "$file$prefix$count";
  die "Cannot find backup name for '$file'\n" if $i >= 1000;
  die "Exiting...\n" unless do_rename "$file$bak", "$file$prefix$count";
}

my $term;
my $OUT;
my @tc;

sub real_inter ($) {
  my $New = $_[0];
  my $Old = $&;
  my $Ans;
  unless (defined $term) {
    require Term::ReadLine;
    $term = new Term::ReadLine 'pfind-interactive';
    $OUT = $term->OUT || \*STDERR;
    if (@Term::ReadLine::TermCap::rl_term_set) {
      @tc = @Term::ReadLine::TermCap::rl_term_set;
      
      #$message =~ s/B<([^>]+|>)>/$Term::ReadLine::TermCap::rl_term_set[2]$1$Term::ReadLine::TermCap::rl_term_set[3]/g;
      #$message =~ s/I<([^>]+|>)>/$Term::ReadLine::TermCap::rl_term_set[0]$1$Term::ReadLine::TermCap::rl_term_set[1]/g;
    } else {
      @tc = ('', '', '>>>', '<<<');
    }
  }
  while (1) {
    print $OUT "$tc[0]Replacement$tc[1] in $tc[2]$File::Find::name$tc[3]:\n";
    print $OUT "$`$tc[0]$&$tc[1]$'\n$`$tc[0]$New$tc[1]$'\n";
    $Ans = $term->readline('Confirm? (y/n/edit/setvars) ', 'y');
    {				# Localize changes to $` and friends
      if ( $Ans =~ /^\s*setvars\s*$/i ) {
	my $first = 1;
	while (1) {
	  $Ans = $term->readline('Perl expression or "end": ', 
				 ($first ? () : ('end')));
	  $first = 0;
	  if ($Ans =~ /^\s*end\s*$/i) {
	    print $OUT "Changes will be effective on next substitution.\n";
	    last;
	  }
	  print $OUT (eval($Ans),"\n");
	}
      } elsif ( $Ans =~ /^\s*edit\s*$/i) {
	my $escaped;
	($escaped = $New) =~ s/([\\\'])/\\$1/g;
	$Ans = $term->readline('Replacement (to be eval\'ed): ', "'$escaped'");
	return eval($Ans);
      } elsif ($Ans =~ /^\s*y(es)?\s*$/i) {
	print $OUT "replacing...\n";
	return $New;
      } elsif ($Ans =~ /^\s*(no?)?\s*$/i) {
	print $OUT "skipping...\n";
	return $Old;		# take old value
      } else {
	print $OUT "unknown answer '$Ans'\n";
	redo LOOP;		# take old value
      }
    }
  }
}

sub dummy_inter ($) { return $_[0]; }

sub inter ($) { goto $InterActiveProc; }

my $havelabel;

sub wrap_rex_or_sub {
  my ($negated) = shift;
  my ($regexp,$regexp_find) = shift;
  my $SubsIt = $regexp =~ /^\s*s/;
  my ($Flags) = ($regexp =~ /(\w*)\s*$/);	# Does not support m XregexpX
  my $Single = $Flags !~ /[sm]/;
  if ( $SubsIt ) {
    die "do not support negated substitutions" if $negated;
    $haveS++;
    my @SubsList= split /&&&/, $regexp;
    if ( @SubsList > 1 ) { 
      $regexp_find = join '||', @SubsList;
      $regexp = join ',', @SubsList;
    } else {
      $regexp_find = $regexp;
    }
  } else {
    my @parts = split /&&&/, $regexp;
    if (@parts > 1) {
#       (undef, my $flags) = ($parts[0] =~ /^ \s* m? (\S) .* (?:\1|[\]})>])(\w*)\s*\Z/sx;
	die "Perl 5.5.53 required with &&&-splitting" unless $] >= 5.00553;
	for (@parts) {
	  my $how = s/^\s*!// ? '!' : '=';
	  (my $re = $_) =~ s/^\s*m?//;
	  # This is a pessimization for anchored extensions...
	  eval "\$_ = qr$re; \$_ = qr[.*?\$_]s" or die "processing `$re': $@";
	  $_ = "(?$how$_)";
	}
	$regexp = join '', @parts;
	$regexp =~ s,/,\\/,g;
	$regexp = "m/^$regexp/$Flags";
    }
  }
  my $Code;
  if ($binary) {
  $Code = <<'EOS';
return unless -f _ and -r _;
EOS
  } else {
  $Code = <<'EOS';
return unless -f _ and -r _ and -T _;
EOS
  }
  $Code .= <<'EOS' if $SubsIt ;
  (print STDERR "$name is not writable\n"), return unless -w _;
EOS
  $Code .= <<'EOS' unless $MultiPrint;
  {
EOS
  $Code .= <<'EOS';
    my $FileName = $_;
    my $found = 0;
    $FileName =~ s|^(\s)|./$1|; 
    my $openfile = $FileName;
    $openfile =~ s/(\s)$/$1\0/;
    open(FILE, "< $openfile") or die "cannot open '$name': $!";
EOS
  $Code .= <<'EOS' if $binary;
    binmode FILE;
EOS
  if ( $SubsIt ) {		# Doing substitution
    die "-alllines option not supported with substitutions.\n"
      if $MultiPrint;
    my $bmode = '';
    $bmode = <<'EOS' if $binary;
      binmode FILE;
      binmode OUTPUT;
EOS
    my $move_copy = 'rename $FileName, "$FileName$bak"';
    my $op = 'rename';
    my $open_write = 'open(OUTPUT, "> $openfile")';
    my $set_mode = 'chmod $Mode, $FileName';
    if (defined &File::Copy::syscopy) { # There is more than $Mode
      $move_copy = 'File::Copy::syscopy $FileName, "$FileName$bak"';
      $op = 'copy';
      $open_write = 'open(OUTPUT, "+< $openfile") and truncate OUTPUT, 0';
      $set_mode = '';
    }
    if ( $Single ) {		# Single-line substitution
      $Code .= <<EOSS;
    local \$_;
    \$InterActiveProc = \\&dummy_inter;  # no interaction in the following loop
    1 while defined (\$_ = <FILE>) and not (\$found = $regexp_find);
    if ( \$found ) { 
      close FILE or die "cannot close '\$name' for write: \$!"; 
      setup_bak(\$FileName, \$bak);
      $move_copy
	or die "cannot $op '\$name' to '\$name\$bak': \$!";
      open (FILE, "< \$FileName\$bak");
      $open_write
	or die "cannot open '\$name' for write: \$!";
      $bmode
      \$InterActiveProc = \\&real_inter; # now real interaction
      while (<FILE>) { $regexp; print OUTPUT; }
      close OUTPUT or die "cannot close \$name: \$!";
      $set_mode;
      \$FileName .= \$bak;
    }
EOSS
    } else {			# Multi-line substitution
      $Code .= <<EOSB;
    local \$/ = undef;
    local \$_;
    \$_ = <FILE>;
    \$found = $regexp_find;
    if ( \$found ) { 
      close FILE or die "cannot close '\$name' for write: \$!"; 
      setup_bak(\$FileName, \$bak);
      $move_copy
	or die "cannot rename '\$name': \$!";
      open (FILE, "< \$FileName\$bak");
      open(OUTPUT, "> \$openfile") 
	or die "cannot open '\$name' for write: \$!";
$bmode
      \$_ = <FILE>; $regexp; print OUTPUT;
      close OUTPUT or die "cannot close '\$name' for write: \$!";
      $set_mode;
      \$FileName .= \$bak;
    }
EOSB
    }
  } else {			# Doing match, not subst
    die "-alllines option not supported with negated match.\n"
	if $negated and $MultiPrint;
    if ( $Single ) {		# Single line match
      die "-alllines option not supported with multiple matches.\n"
	if $MultiPrint and $havelabel;
      my $cond = $negated ? 'unless' : 'if';
      $Code .= <<'EOSL' if $MultiPrint and not $havelabel++;
  lines:
  {
EOSL
      $Code .= <<EOSS;
    while (defined (\$line = <FILE>)) {
        \$found = 1, last $cond \$line =~ $regexp;
    }
EOSS
    } else {			# Multiline match
      die "-alllines option not supported with multiline match.\n"
	if $MultiPrint;
      $Code .= <<EOSB;
    local \$/ = undef;
    local \$_;
    \$_ = <FILE>;
    \$found = $negated$regexp;
EOSB
    }
  }

  if ( $Single ) {
    $Code .= <<'EOSS';
    $Line_Found = $line if $found;
EOSS
  }

  if ($MultiPrint) {
    $Code .= <<'EOS';
    close FILE or die "cannot close '$name': $!" unless $found;
    return unless $found;
EOS
  } else {
    $Code .= <<'EOS';
    close FILE or die "cannot close '$name': $!";
    return unless $found;
  }
EOS
  }
}

$StartDir = shift;
usage unless @ARGV;
$bak = ".bak";
my $utf;
while ($StartDir =~ /^-/) {
  my $o = $StartDir;
  $StartDir = shift;
  last if $o eq '--';
  $debug = 1,	    next if $o eq '-debug';
  $prune = 1,	    next if $o eq '-nosubdir';
  $binary = 1,	    next if $o eq '-bin';
  $MultiPrint = 1,  next if $o eq '-alllines';
  $utf = 1,  	    next if $o eq '-utf8';
  $bak =  $1,	    next if $o =~ /^-bak=(.*)/;
  $Vars =  $1,	    next if $o =~ /^-vars=(.*)/;
  die "Unknown option `$o'.\n\tStart $0 without arguments for usage info.\n" 
    if $o =~ /^-/;
}

$Vars =~ s/,/ /g if defined $Vars;
$Vars = "use vars qw($Vars);\n\n" if defined $Vars;
$Vars ||= '';

#die "Starting directory `$StartDir' not found.\n" unless -d $StartDir;
while ( -l $StartDir )
{ my $Link = readlink $StartDir;
  $StartDir= ( $StartDir =~ m|(.*)/[^/]+$|s ? $1 : '.');
  $StartDir= ( substr($Link,0,1) eq '/' ? $Link : $StartDir .'/'.$Link );
}
  
die "Starting directory `$StartDir' not found.\n" unless -d $StartDir;

my $NF = 0;  # no of filter arguments
map { $NF++ if  m:^([-/!]):  || m/^=~/ || m/^\$(name|dir)\s*[=!]~/ } @ARGV;
# Convert -f to -f _, -M < 3 to -M _ < 3
@ARGV = map { s/^\s*((!\s*)?-[a-su-zA-Z])\s*([!=<>]|$)/$1 _ $3/; $_ } @ARGV;
$NeedsStat = 0;  $NeedsLStat = 0;
map { $NeedsStat++  if m/^\s*(!\s*)*-[a-km-su-zA-Z]\s+_\b/ } @ARGV; # skip -l -t
map { $NeedsLStat++  if m/^\s*(!\s*)*-l\s+_\b/ } @ARGV;
map { $NeedsStat++  if m/^\s*du\s*$/ } @ARGV; # du
my @rows = map { s:^(([-/]|\$(name|dir)\s*[=!]~).*):return unless\tdo {\t$1\t\t}:s;
		 s:^!(?!\s*=~)(.*):return if\tdo {\t$1\t\t}:s ; $_ } @ARGV;
@rows = map { s/ ^ ((?:!\s*)?)=~ (.*) / $NeedsStat++; wrap_rex_or_sub($1, $2) /xes ; $_ } @rows;

push @rows, "  ;\n". ($MultiPrint ? "  prt_line" :"  prt")  if $NF == @ARGV;
push @rows, '$found = 0', "redo lines", "}" if $havelabel;

my $text = join "  ;\n  ", @rows;

$text = "my (\$dev,\$inode,\$Mode) = CORE::stat(\$_);\n  " . $text  
  if  $haveS;
#$text = "my (\$dev,\$inode,\$Mode) = CORE::stat(_);\n" . $text  
#  if  $haveS;

$text = "CORE::stat(\$_);\n  " . $text  
  if  $NeedsStat and not $haveS;
$text = "CORE::lstat(\$_);\n  " . $text  
  if  $NeedsLStat and not $haveS;

# Disable optimizations of File::Find so that _ is always available
# $File::Find::dont_use_nlink = 1 if $NeedsStat or $haveS;	# does not work

@ARGV = ();

my $setup = <<'EOS';
  my $name = $File::Find::name;
  my $dir  = $File::Find::dir;
  my $was = $_;
  my ($line, $found);
EOS

$setup .= <<EOS if $utf;
  use 5.005_50;
  use utf8;
EOS

my $PRUNE= ( $prune ? 
             '$File::Find::prune=1  unless $File::Find::name eq $StartDir;'
            : '' );

my $finish = <<'EOS';
  do_rename($was,$_) unless $was eq $_;
EOS

my $wanted = <<EOW;
${Vars}sub {
$setup
  $PRUNE
  $text;
$finish
}
EOW

print STDERR "$wanted\n" if  $debug ;

my $sub = eval $wanted;
die $@ if $@;

exit if $debug;

use File::Find;

File::Find::find ( $sub, $StartDir );
print "\n$Total_Size byte", ($Total_Size != 1 ? 's' : ''), 
  " in $Num_Files file", ($Num_Files != 1 ? 's' : ''), "\n"
  if defined($Total_Size);

__END__

=head1 NAME

pfind - find-or-grep-like utility with Perl syntax

=head1 SYNOPSYS

To lowercase only the files names of which contain C<blah>, do

  pfind . /blah/ '$_ = lc'

To lowercase only the files which contain C<blah> inside, do

  pfind . "=~ /blah/" '$_ = lc'

To do recursive C<grep foo bar*> in UTF-8 files, do

  pfind -utf8 -alllines . '/^bar/' '=~ /foo/' prt_line

=head1 DESCRIPTION

usage: 

  pfind [-debug] [-nosubdir] [-bin] [-bak=suffix]	\
      [-alllines] [-utf8] [-vars] [--] startdir        \
        rule1 rule2 ...

=head2 Rules: filters and actions

Rules are perl statements to execute. Statements starting with C<->,
C</>, C<$dir =~>, C<$dir !~>, C<$name =~>, C<$name !~>, or C<!> are
considered filters, a file will be discarded unless the statement
returns true. Statements starting with C<=~> (possibly negated by prepending
C<!>) are filters applied to the contents of the file.
The rest is executed I<as is>.

Rules are executed in the directory of the file.

If only filters are given the default action C<prt> (see below) is added.

One can always emulate a filter C<FILT> by giving a rule 

  return unless FILT;

reversely, one can inhibit interpretation of a rule as a filter by
adding leading whitespace.

=head2 Initialization and Termination

Rules of the forms

  use Blah;

or

  BEGIN {BLOCK}

are executed before the start of tree walk, rules of the form

  END   {BLOCK}

are executed on termination.

=head2 File

In rules variables $_, $name, $dir contain the file name, full file
name and the name of the directory. Statements are executed in the
directory of the each processed file.

=head2 Renaming

If file is not discarded, and $_ is changed after the perl statements
are executed, the file is renamed to the new value of $_.

=head2 C<prt>, C<prt_line>, C<skip> and C<du>

Convenience function C<prt> prints C<\n>-terminated name of the file,

C<prt_line> does the same, but appends the found $line as well.

C<skip> arranges for files inside the given subdirectory to be
skipped.  Ignored for non-directories.

C<du> increments the count of files/bytes-in-files and arranges for a
message of the form

  43952 bytes in 2 files

to be printed on termination.  The count of files is available in
variable $Num_Files, count of bytes-in-files in $Total_Size.

=head2 Contents of the file

If a rule starts with C<=~> (possibly negated by prepending
C<!>), it is considered as a filter to
match contents of the file. The rest is the regular expression to
match.  If regexp contains modifiers C<s> or C<m>, it is matched against
the file as a whole, otherwise it is matched line-by-line.  Several regular
expressions can be joined by C<&&&> (and then possibly negated; e.g.
C<=~ /foo/i &&& !/bar/> matches lines which contain C<foo> but not C<bar>);
currently only the flags on the last expression are taken into account for
choosing between line-by-line or text-as-a-whole match.

If the regexp-filter fails, file processing stops. Otherwise 
whatever was matched against is available in the variable $line, unless
modifiers C<s> or C<m> are given.

=head2 Modifying file

If a rule starts with C<=~ s> it is considered as a filter to match
and MODIFY the contents of the file.  Modifiers C<s> and C<m> are
applicable as above. To make several modifications at a time, use e.g.
C<=~ s/abc/ABC/ &&& s/def/DEF/> (modifiers should be the same, though).
In case you give the modifiers C<s> or C<m> you should think about using
the modifier C<g>, as well.

=head2 Interactive modifications

If you use the 'e' modifier you may invoke an interactive replacement by
using the function C<inter>, e.g. by giving an argument

  =~ s/(\d+)/inter $1+1/eg	# Interactively increment numbers

or in more complicated cases

  =~ s/(\d+)/inter qq[......]/eg # Interactively substitute numbers with dots

When 'inter' is invoked it prints out the file name, the old and the
new values of the line (with the matched part highlighted or
surrounded by C<E<gt>E<gt>E<gt>> C<E<lt>E<lt>E<lt>>).  You have the
following choices

=over

=item C<y> or C<yes>

to accept the proposed change;

=item C<edit>

for being prompted for a replacement value (will be eval'ed);

=item C<setvars>

to get into an eval-print loop which allows to
set/change some global data.  Say, you may change value of some
variables used as arguments to C<inter>.  The initial values of these
variables may be set in a C<BEGIN {...}> rule.  The variables should be 
declared in the L<-vars|C<-vars=$var,%hash,$another>> option.

=item C<n> or C<no>

to keep the old value;

=back

Example (in DOSISH shell syntax with split lines):

  pfind -vars=$incr . "BEGIN { $incr = 2 }" /\.el$/ \
	"=~ s/(\d+)/ inter $1 + $incr /ge"

B<NOTE.> Due to semantic of making substitutions in Perl, if multiple
changes are made on the same line, the previous changes are not
reflected in the message you get from pfind.  Similarly, the changes
to $incr you do during C<setvars> step are active only the I<next>
substitution.

=head1 OPTIONS

=over 8

=item C<-debug> 

just shows the expression which will be given to File::Find.

=item C<-nosubdir> 

do not descend into subdirectories.

=item C<-alllines> 

Do actions following C<=~ /REGEXP/> filter for all the matching lines.

Is supported for one single line match filter only.

=item C<-bin> 

without this binary files are not processed for substrings, and files
are opened in text mode.

=item C<-utf8> 

Interpret code as modified by C<use utf8> directive.

=item C<-bak=SUFFIX> 

Sets suffix to use for backup.

=item C<-vars=$var,%hash,$another> 

Declare the following vars for use the rules.

=back

=head1 VERSION

$Revision: 1.20 $, $Date: 2003/11/02 08:41:31 $.

=head1 AUTHOR

Ilya Zakharevich <ilya@math.ohio-state.edu> with significant additions
by Helmut Jarausch <jarausch@igpm.rwth-aachen.de>

=cut

__END__

Here is the newsgroup announcement:

pfind is a tiny script which allows one to combine the power and
generality of Perl with terseness and convenience of specialized file
and directory utilities.

By learning 4 shortcuts added to standard Perl, 4 command-line options
and 4 convenience functions, one can perform many operations usually
done with diverse toolkits.  pfind command-lines are only slightly
longer than those of a specialized tool.

In addition, since pfind can take *arbitrary* Perl expressions as
arguments, pfind can do much more - while remaining easy to use.  Thus
pfind both simplifies the life of a Perl user, freeing him of learning
syntax of multiple tools, and enhances his abilities as well.
