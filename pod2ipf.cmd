extproc perl -Sx
#!perl -w
use strict qw(refs subs);
use File::Find;
use Cwd;
use Config '%Config';
sub intern_modnamehash;

#require 'dumpvar.pl';

$VERSION = "1.10.1";

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

debug("Module pod/pm discovery");
  
$curdir = cwd;
my $site_perl_prefix;

if ((substr $Config{sitelib}, 0, length $Config{privlib}) 
    eq $Config{privlib}) {
  $site_perl_prefix = substr $Config{sitelib}, (length $Config{privlib}) + 1;
  $site_perl_prefix =~ s!\\!/!g ;
}
  
libdir: foreach $libdir (@INC, $Config{bin}) {
  chdir $libdir;
  debug("Looking in $libdir:");
  find (\&intern_modnamehash , '.');
  chdir $curdir;
}
  
@modnames = sort keys %modnamehash;
  
print STDERR "Found `@modnames'.\n";
  
# %modnamehash now maps module name -> file name.
# %moddesc now maps module name -> description.
  
$DocTitle = 'Perl Manual';

@files =
    ( # from perl.pod
     #  file         section name
     [ 'perl',      'Perl overview' ],
     [ 'perlos2',      'Perl under OS/2' ],
     #[ 'perltoc',   'Perl documentation table of contents' ],
     [ 'perldata',  'Perl data structures' ],
     [ 'perlsyn',   'Perl syntax' ],
     [ 'perlop',    'Perl operators and precedence' ],
     [ 'perlre',    'Perl regular expressions' ],
     [ 'perlrun',   'Perl execution and options' ],
     [ 'perlfunc',  'Perl builtin functions' ],
     [ 'perlvar',   'Perl predefined variables' ],
     [ 'perlsub',   'Perl subroutines' ],
     [ 'perlmod',   'Perl modules' ],
     [ 'perlref',   'Perl references' ],
     [ 'perldsc',   'Perl data structures intro' ],
     [ 'perllol',   'Perl data structures: lists of lists' ],
     [ 'perlobj',   'Perl objects' ],
     [ 'perltie',   'Perl objects hidden behind simple variables' ],
     [ 'perlbot',   'Perl OO tricks and examples' ],
     [ 'perldebug', 'Perl debugging' ],
     [ 'perldiag',  'Perl diagnostic messages' ],
     [ 'perlform',  'Perl formats' ],
     [ 'perlipc',   'Perl interprocess communication' ],
     [ 'perlsec',   'Perl security' ],
     [ 'perltrap',  'Perl traps for the unwary' ],
     [ 'perlstyle', 'Perl style guide' ],
     [ 'perlxs',    'Perl XS application programming interface' ],
     [ 'perlxstut', 'Perl XS tutorial' ],
     [ 'perlguts',  'Perl internal functions for those doing extensions ' ],
     [ 'perlcall',  'Perl calling conventions from C' ],
     [ 'perlembed', 'Perl how to embed perl in your C or C++ app' ],
     [ 'perlpod',   'Perl plain old documentation' ],
     [ 'perlbook',  'Perl book information' ],
    );

for $pod (@files) {
  $doing_pod{$pod->[0] . ".pod"}++;
}

for $pod (qw(perlovl.pod perltoc.pod)) {
  $obsolete{$pod}++;
}

for $pod (<*.pod>) {
  $not_doing_pod{$pod}++ unless $doing_pod{$pod} or $obsolete{$pod};
}

for $pod (keys %not_doing_pod) {
  print STDERR "!!! Unknown POD: `$pod'\n";
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

$font = ''; #':font facename=Helv size=16x8.';

$debug_xref = 0;
$dump_xref = 0;
$dump_contents = 0;
$ref_delta = 1;     # start from 1
$maxtoc = 5;
$dots = 0;
$multi_win = 1;     # 1 = use alternate window for toc
$section_head[0] = '';		# To simplify warnings

%groups = (
	   links => 1,
	   text => 2,
	   logo => 3,
	   camel => 4,
	   l_camel => 5,
	   r_camel => 6,
	   sublinks => 7,
	   about => 8,
	  );

%panelwidths = (		# Make a gap.
		links => '29%',
		text => '69%',
		sublinks => '28%',
	       );

sub out;
sub contents;
sub escape;
sub addref;
sub findref;
sub winhead;
sub winlink;
sub no_markup_len;

$/ = "";

foreach $sc (@split_sections) { $as_head{$sc} = 1; }

foreach $sc (@index_sections) { $fine_index{$sc} = 1; }

# Insert the Logo page.

for ($pass = 1; $pass <= 2; $pass++) {

    $headno = 0; # make refs hash for this on first pass

    print STDERR "pass: $pass\n";
    if ($pass == 2) {
# We position burst window low, so it does not obsure Contents and/or
# titles of information windows, and Contents does not obsure us
# (completely).
      print <<EOI;
:userdoc.
:title.$DocTitle
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

:link reftype=hd group=$groups{text} dependent vpx=right vpcx=$panelwidths{text} refid=@{[findrefid('perlmod/CPAN')]}.Where to get ...:elink.

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
Do not forget that you can alway click on :hp9.Contents:ehp9., :hp9.Search:ehp9., and :hp9.Index:ehp9. buttons, (or use :hp8.Alt-t:ehp8., :hp8.Alt-s:ehp8., :hp8.Alt-i:ehp8. correspondingly).
:h1 toc=1 group=$groups{about} x=center width=100% y=center height=20% id=63900.About
Generated on @{[out(scalar localtime, 1)]}, by
:link reftype=hd group=$groups{links} dependent vpx=left vpcx=$panelwidths{links} refid=@{[findrefid('perl')]}.Perl:elink.
version $], 
:link reftype=hd group=$groups{links} dependent vpx=left vpcx=$panelwidths{links} refid=@{[findrefid('pod2ipf')]}.pod2ipf:elink.
version $VERSION.

EOI
    }
    for ($fn = 0; $fn <= $#files; $fn++) {
        $fname = $files[$fn][0] . '.pod';
	if (not -f $fname) {
	  $fname = $modnamehash{$files[$fn][0]};
	}
        $page = $files[$fn][0];
        $toc = $maxtoc;
        
        open(IN, $fname) || die "$fn: open `$fname': $!";
        print STDERR $fname . ": ";
        print STDERR "\n" if !$dots;

        $section = $files[$fn][0] . ' - ' . $files[$fn][1];
        if ($pass == 1) {
            addsection($section, $headno, 1);
            addref($page, $headno);
	    $is_head{$files[$fn][0]}++;
        }
        $section_head[1] = $page;
        $path = $section_head[1];
        if ($pass == 2) {
            print ":h1 toc=$toc " . winhead($headno)
                . " id=" . ($headno + $ref_delta) . "."
                . out($section, 0) . "\n" . $font;
            print ":i1." . out($section, 0) . "\n";
            print ":i1." . out($files[$fn][0], 0) . "\n";
        }
        $headno++;
        
        @lstack = ();
        $emptypane = 1;
	$inpod = 0;
        
        PARA: while ($line = <IN>) {
            chomp $line;
            if ($line =~ /^=head(\d+)\b\s*/) {
	        $inpod = 1;
                $nopara = 0;
                $heading = $';
		if (@lstack) {
		  warn "List not finished (@lstack) in (@section_head[1..$hl]).\n"
		    if $pass == 1;
		  while ($#lstack >= 0) {
                    $t = pop(@lstack);
                    if ($t eq 'ul') {
		      print ":eul.\n" if $pass == 2;
                    } elsif ($t eq 'ol') {
		      print ":eol.\n" if $pass == 2;
                    } elsif ($t eq 'head' or $t eq 'finehead') {
		      $hl--;
		      $path = join('/', @section_head[1..$hl]);
                    }
		  }	
		}
    
                $hl = $1 + 1;
                contents($hl, $headno) if $emptypane;
                if ($pass == 1) {
                    addsection($heading, $headno, $hl);
		    $plainheading = $heading;
		    # XXXX Is wrong with some escapes:
		    1 while $plainheading =~ s/[A-Z]<([^<>]*)>/$1/g;
                    addref(qq|$page/"$plainheading"|, $headno);
		    $is_head{$plainheading}++;
                }
                $section_head[$hl] = $heading;
                $path = join('/', @section_head[1..$hl]);
                if ($pass == 2) {
                    print ":h$hl " . winhead($headno)
                        . " id=" . ($headno + $ref_delta) . "."
                        . out($heading, 0) . "\n" . $font;
                    print ":i1." . out($heading, 0) . "\n";
                }
                $headno++;
                print STDERR "." if $dots;
                $emptypane = 1;
            } elsif ($line =~ /^=over\b/) {
	        $inpod = 1;
                # look ahead, to see how the list should look like
                chomp($line = <IN>);
                if ($line =~ /^\=item\s+\*/) { # item *
                    push(@lstack, "ul");
                    print ":ul.\n" if $pass == 2;
                } elsif ($line =~ /^\=item\s+1\.?/) {  # item 1. 
                    push(@lstack, "ol");
                    print ":ol.\n" if $pass == 2;
                } elsif (defined($as_head{$path})) {
                    # in some cases we use headings instead of lists
                    warn "toc for $page, id=$headno too low" if ! $toc >= $hl + 1;
                    push(@lstack, $fine_index{$path} ? "finehead" : "head");
                    $hl++;
		    $section_head[$hl] = 'list_start';
                    $eitems = "";
                } else {
                    push(@lstack, "ul");
                    print ":ul.\n" if $pass == 2;
                }
                $nopara = 0;
                redo PARA;
            } elsif ($line =~ /^=back\b/) {
	        $inpod = 1;
                if ($#lstack >= 0) {
                    $t = pop(@lstack);
                    if ($t eq 'ul') {
                        print ":eul.\n" if $pass == 2;
                    } elsif ($t eq 'ol') {
                        print ":eol.\n" if $pass == 2;
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
                $headx = $heading;
                print STDERR "." if $dots;
		if ($#lstack == -1) {
                    push(@lstack, "ul");
                    print ":ul.\n" if $pass == 2;
		    warn "An =item without =over in (@section_head[1..$hl])\n" 
		}
                if ($lstack[$#lstack] eq 'head'
		    or $lstack[$#lstack] eq 'finehead') {
                    contents($hl, $headno) if $emptypane;

                    # lowest level never empty, IPFC uses next page
                    # by default (but Back button doesn't work :-()
                    $emptypane = 0;

		    $headx =~ /(\w+)/;
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
                    if ($pass == 2) {
                        print ":h$hl " . winhead($headno)
                            . " id=" . ($headno + $ref_delta) . "."
                            . out($heading, 0) . "\n" . $font;
                        print ":i1." . out($heading, 0) . "\n";
			$is_head{$heading}++; # XXXX Need to strip?
			if ($#lstack >= 0 
			    and $lstack[$#lstack] eq 'finehead') {
			  print ":i1." . out($word1, 0) . "\n" 
			    if defined $word1 and $word1 ne $heading;
			  print ":i1." . out($word2, 0) . "\n" 
			    if defined $word2 and $word2 ne $heading
			      and $word2 ne $word1;
			}
                    }
                    $headno++;

                    # look ahead to see if this =item is empty.
                    # if it is, create a list of empty pages of
                    # on first non-empty.
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
                    }
                    redo PARA;
                } else {
                    $emptypane = 0;
		    addref(qq|$page/"$headx"|, $headno, 1);
                    if ($lstack[$#lstack] eq 'ul' && $heading =~ /^\s*\*\s*(.*)$/ or
                        $lstack[$#lstack] eq 'ol' && $heading =~ /^\s*\d+\.?\s*(.*)$/)
                    {
                        if ($pass == 2) {
                            print ":li.";
                            $heading = $1;
                            if ($1 ne "") {
                                
                                print out($heading, 1) . "\n";
                                print ":i1." . out($heading, 0) . "\n"
				  unless $is_head{$heading};
                            } else {
                                $nopara = 1;
                            }
                        }
                    } else {
                        if ($pass == 2) {
                            print ":li." . out($heading, 1) . "\n";
                            print ":i1." . out($heading, 0) . "\n"
				  unless $is_head{$heading};
                        }
                    }
                }
            } elsif ($line =~ /^=cut/) {
	        $inpod = 0;
            } elsif ($line =~ /^=pod/) {
	        $inpod = 1;
            } elsif ($line =~ /^=\w+/) {
                warn "what to do with '$line'?\n";
            } elsif ($inpod == 0) {
	      # Just ignore this chunk
            } elsif ($line =~ /^\s+\S/) {
                if ($pass == 2) {
                    @tlines = split(/\n/, $line);
                    foreach $tline (@tlines) {
                        1 while $tline =~ s/\t+/' 'x (length($&) * 8 - length($`) % 8)/e;
                    }
                    $pre = join("\n", @tlines);
                    print "\n:xmp.\n" . escape($pre) . ":exmp.";
                }
                $nopara = 0;
                $emptypane = 0;
            } elsif ($line =~ /^\s+\S/m) { # see perl(run)?/SYNOPSIS for this
                if ($pass == 2) {
                    $mark = out($line, 1);

                    # hack hack ;-)
                    # IPFC doesn't handle tabs
                    # no_markup_len tries to guess the # of ' ' to next tab,
                    # but even when the guess is correct, things don't seem
                    # to align when bold,.. is used :-(
                    @tlines = split(/\n/, $mark);
                    foreach $tline (@tlines) {
                        1 while $tline =~ s/\t+/' 'x (length($&) * 8 - &no_markup_len($`) % 8)/e;
                    }
                    $pre = join("\n", @tlines);
                    
                    print "\n:xmp.\n" . $pre . ":exmp.";
                }
                $nopara = 0;
                $emptypane = 0;
            } else {
                if ($pass == 2) {
                    print ":p." if !$nopara;
                    print out($line, 1);
                } else {
                    if ($line =~ /^\s+$/) {
                        warn "line with blanks in $page, id=$headno\n";
                    }
                }
                $nopara = 0;
                $emptypane = 0;
            }
        }
        close(IN);
        print STDERR "\n" if $dots;
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

sub out {
    my $para = $_[0];
    my $markup = $_[1];
    my @stack = ();
    my $output = "";

    return if ($pass == 1);
    
    $cpos = 0;
    $opos = 0;
    TAG: while ($para =~ m{([<>])}g) { # ;-) ;-)
        $cpos = pos $para;
        $c = $1;
        
        if ($c eq '<' && $cpos >= 0) {
            $output .= escape(substr($para, $opos, $cpos - $opos - 2));
            
            $c = substr($para, $cpos - 2, 1);
            if ($c !~ /[A-Z]/) {
                $output .= escape($c . '<');
                pos($para) = $opos = $cpos;
                next TAG;
            }
            if ($c eq 'B') {
                $output .= ':hp2.' if $markup;
                push (@stack, $c);
            } elsif ($c eq 'F') {
                $output .= ':hp2.' if $markup;
                push (@stack, $c);
            } elsif ($c eq 'S') {
                $output .= ':hp2.' if $markup;
                push (@stack, $c);
            } elsif ($c eq 'I') {
                $output .= ':hp1.' if $markup;
                push (@stack, $c);
            } elsif ($c eq 'C') {
                $output .= ':hp2.' if $markup;
                push (@stack, $c);
            } elsif ($c eq 'L') {
                my $link;
	        #push (@stack, $c);
                # link
                pos $para = $cpos;
                if ($para =~ m/\G([^>]+)\>/g) {
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
                        $output .= escape($blink);
                        $output .= ":elink." if $markup;
                    } else {
                        warn "   unresolved link: $link\n";
                        $output .= escape($link);
                    }
                }
            } elsif ($c eq 'E') {
                pos ($para) = $cpos;
                if ($para =~ m/\G([A-Za-z]+)>/g) {
                    my $esc;
                    $cpos = pos $para;

                    $esc = exists $HTML_Escapes{$1} ? $HTML_Escapes{$1} : "E<$1>";
                    $output .= escape($esc);
                } else {
                    warn "$fname: E<> ???\n";
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
                $output .= ':ehp2.' if $markup;
            } elsif ($c eq 'S') {
                $output .= ':ehp2.' if $markup;
            } elsif ($c eq 'I') {
                $output .= ':ehp1.' if $markup;
            } elsif ($c eq 'C') {
                $output .= ':ehp2.' if $markup;
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
        $output = substr($output, 0, 80); # strip too long stuff
    }
    return $output;
}

sub contents {
    my $level = $_[0];
    my $no = $_[1];
    my ($i, $cl, $toplevel);

    if ($pass == 1) {
        $wingroup[$no - 1] = $groups{links};
        return ;
    }
    
    $i = $no;

    while ($i > 0 && $headlevel[$i] >= $level) { $i--; }

    $toplevel = $headlevel[$i];

    print ":p." . out($head[$i], 0) . "\n";
    $i++;
    $cl = $toplevel;
    for (; $i <= $#head && $headlevel[$i] > $toplevel; $i++) {
        if ($headlevel[$i] > $cl) {
            warn "bad nesting: $toplevel, $headlevel[$i], $cl, $i, `$head[$i]`\n" if $headlevel[$i] != $cl + 1;
            print ":ul compact.\n";
            $cl++;
        } elsif ($cl > $headlevel[$i]) {
            while ($cl > $headlevel[$i]) {
                print ":eul.\n";
                $cl--;
            }
        }
        print ":li.:link reftype=hd " . winlink($i)
            . " refid=" . ($i + $ref_delta) . "."
            . out($head[$i], 1) . ":elink.\n";
    }

    while ($cl > $toplevel) {
        print ":eul.\n";
        $cl--;
    }
}

sub findrefid { $links{findref(shift)} + $ref_delta }

sub findref { # various heuristics to get a valid link
    my $link = $_[0];
    
    $link =~ tr/\n/ /;
    print STDERR "link: $link\n" if $debug_xref;
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
        } else {
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
	} elsif ($link =~ /\([13]\)$/ && defined $links{$`}) {	# perl(1)
	  $link = $`;
	}
        print STDERR "trans: $link\n" if $debug_xref;
    }
    return $link;
}

sub addref {
    my $page = $_[0];
    my $num = $_[1];
    my $check = $_[2];
    
    $links{$page} = $num unless $check and exists $links{$page};
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

BEGIN {
    %HTML_Escapes =
        (
         'amp'	=>	'&',	#   ampersand
         'lt'	=>	'<',	#   left chevron, less-than
         'gt'	=>	'>',	#   right chevron, greater-than
         'quot'	=>	'"',	#   double quote
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
    local $_ = $File::Find::name;
    $shortpath = $_;

# kill leading './'

    s{^[.]/}{};

# XXX - take the current $libdir (/foo/bar) 
# and see if the file were testing (/foo/bar/site_perl/Plugh/Blah.pm) is
# in any *other*, deeper subdir in @INC
# (/foo/bar/site_perl) - if so, skip this entry, cuz the deeper 
# subdir will catch it properly (Plugh::Blah)

# for other libraries that are proper subdirs of the current libdir
    foreach $otherlibrary (grep /^$libdir.+/, @INC) {

# if the other library is part of the current files path, skip it
# because it will be caught when the other library is used

	if ("$libdir/$_" =~ /^$otherlibrary/) {
	    print STDERR ".";
#	    print "Skipping $_\n";
#	    print "cuz $otherlibrary caught/will catch it\n";
	    return;
	}
    }

# exclude base pods - perlfoo.pod
    /perl.*[.]pod/ && return;

# for each file entry, kill trailing '.(pod|pm|cmd)'
    (-f "$libdir/$_") &&
	s{^(.*)[.](pod|pm|cmd)$ }{$1}x or return;

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
    $seen{lc $_}++;

    $modnamehash{$_} = "$libdir/$shortpath";

# If this is a .pm file, is there actually any documentation in it?

# Under OS/2 perl utilites can have extension .cmd. To be safe, allow
# .bat as well. Since we look into $Config{bin}, we may allow files
# without extension as well, if they are text files.
    
	if ($modnamehash{$_} =~ /[.](pm|cmd|bat)$/i
	    or $modnamehash{$_} !~ /[.]/ and -T $modnamehash{$_}) {
	    $good = 0;
	    open(MODULE, "$modnamehash{$_}");
	  line: while ($theline = <MODULE>) {
		$theline =~ /^=head\d/ and $good = 1 and last line;
		eof(MODULE) && delete $modnamehash{$_};
	    }
	    if ($good and $theline =~ /^=head\d\s+NAME\b/ ) {
	      my @addrefs;
	      
	      # Skip whitespace:
	      $theline = "";
	      $theline = <MODULE> while defined $theline and $theline !~ /\S/;
	      # Now have the name, find the description:
	      if ($theline =~ /^(\S+)((,\s+\S+)*)\s*-\s*(.*)/ ) {
		my $desc = $4;
		my $skipNAME;	# safe to skip NAME section
		if (lc($_) eq lc($1)) {
		  $skipNAME = ! defined $2;
		  # Prefer the name on the line over the file name:
		  if ($_ ne $1) {
		    $modnamehash{$1} = $modnamehash{$_};
		    delete $modnamehash{$_};
		    $_ = $1;
		  }
		  # Now process additional names this manpage may appear under.
		  @addrefs = ($2 =~ /([\w:]+)/g);
		  while ($theline = <MODULE> and not $theline =~ /\A=/) {
		    if ($theline =~ /^((\S+)(,\s+\S+)*)\s*-\s*(.*)/) {
		      push @addrefs, ($1 =~ /([\w:]+)/g);
		      $skipNAME = 0;
		    } elsif ($theline =~ /\S/) {
		      $skipNAME = 0;
		    }
		  }
		  @addref{@addrefs} = ($_) x @addrefs;
		  # dumpValue(\%addref);
		} else {
		  print STDERR "!!! Not matching: `$_' vs. `$1'\n";
		}
		$moddesc{$_} = $desc;
		$skipNAMEs{$_} = $skipNAME;
		#print STDERR "moddesc: `$_' `$oldname' `$3'\n";
	      } else {
		print STDERR "!!! $_: bad NAME: `$theline'\n";
	      }
	    } elsif ($good) {
	      print STDERR "!!! $_: no NAME\n";
	    }
	}

    echopod($_) if $modnamehash{$_};
}

sub debug {
  print STDERR "\n", '=' x 79, "\n$_[0]\n", '=' x 79 , "\n";
}

sub echopod {

    $savenew = $_[0];

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
__END__

=head1 NAME

pod2inf - translator from POD format to IBM's F<.INF> format.

=head1 SYNOPSYS

  cd \perllib\lib\pod
  pod2inf > perl.ipf
  ipfc /inf perl.ipf

=head1 DESCRIPTION

Currently takes no command-line options, and processes all the
standard Perl pods in the current directory, as well as all the Perl
libraries and all the Perl utilities it can find using F<Config.pm>,

Both steps produce a lot of warnings, mostly because of malformed C<POD>s.

=head1 PREREQUISITES

Developer toolkit is required (for C<ifpc>).

=head1 AUTHOR

C<Marko.Macek@snet.fri.uni-lj.si>, C<mark@hermes.si>, with additions
by Ilya Zakharevich C<ilya@math.ohio-state.edu>.

=head1 SEE ALSO

L<perlpod>, L<perl>, L<pod2man>, L<perldoc>.

=cut

No docs:  L<pod2html>, L<pod2latex>,  L<pod2texi>, L<pod2text>,
