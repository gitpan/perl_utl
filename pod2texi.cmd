extproc perl -Sx 
#!/usr/build/perl53/bin/perl
#!/usr/local/bin/perl
# changes pod to texinfo
# you still have to insert a few TeXinfo directives to make it look
# right ...
# well, not any more (hopefully).

# parts stolen from pod2html (to get the perlpod listings, et al).
# unfortunately, texinfo 1.1 can't jump to a certain location in a 
# info page, so we can't do the kind of cool exactly-this-spot 
# xrefs that html can ...
# By Krishna Sethuraman (krishna@sgi.com)

# This was a real hack job -- I started this before I fully understood
# anonymous references, so please feel free
# to hack this apart.

# so we can get the locations for the library -> .pm -> module docs

use Config;
use File::Find;
sub intern_modnamehash;
sub debug;

use vars (@envs);
$modnode = 'Module List';
$modinfofile = 'pm.info';

# Im sure not all these are needed, but cant hurt (I hope)

@libdirs = qw(archlibexp installarchlib installprivlib privlibexp archlib
	  installsitearch installsitelib privlib sitearch sitearchexp
	  sitelib sitelibexp);

@libdirs = qw(privlib archlib sitelib sitearch);

# previously, we used $Config{$libdir} in intern_modnamehash and the
# foreach loop ... now we just use $libdir
@libdirs = @INC;

# The beginning of the url for the anchors to the other sections.
$nodeterm = "\c_";
chop($wd=`pwd`);
$type="<A HREF=\"file://localhost".$wd."/";
$debug=0;
$/ = "";
$p=\%p;
#@exclusions=qw(perldebug perlform perlobj perlstyle perltrap perlmod);
$indent=0;
opendir(DIR,".");
@{$p->{"pods"}}=sort grep(/\.pod$/,readdir(DIR)); # sort so perl.pod is first
closedir(DIR);

# put all modules into big hash of module name -> file
# save current dir
chop($curdir = `pwd`);

# for all library directories, go to that directory and find the .pod and .pm
# files
# 

debug("Module pod/pm discovery");
chop($date = `date`);

libdir: foreach $libdir (@libdirs) {
    chop($curdir = `pwd`);
    chdir $libdir;
    debug("Looking in $libdir:");
    find (\&intern_modnamehash , '.');
    chdir $curdir;
}

chdir $curdir;

@modnames = sort keys %modnamehash;

# %modnamehash now maps module name -> file name.

# (1) LEARN the important stuff.

debug("Learning Pods");


foreach $tmpod (@{$p->{"pods"}}, @modnames){

# if its a module, get the full filename

    if (defined($modnamehash{$tmpod})) {
	$podfile = $modnamehash{$tmpod};
    } else {
    $podfile = $tmpod;
    }

    ($pod=$tmpod)=~s/\.(pod|pm)$//;
    $p->{"podnames"}->{$pod}=1;
    next if grep(/$pod/,@exclusions);

# print them out semi-orderedly
    
    &echopod($pod);

# for each podfile

    open(POD,"<$podfile");
    while(<POD>){
# kill bold/italics
	s/B<([^<>]*)>/$1/g;         # bold
	s/I<([^<>]*)>/$1/g;         # bold
# if = cmd
        if (s/^=//) {
	    s/\n$//s;
	    s/\n/ /g;
	    ($cmd, $_) = split(' ', $_, 2);
# if =item cmd
 	    if ($cmd eq  "item") {
		($what,$rest)=split(' ', $_, 2);
# what is now only the (-.) part (dash plus one character)
		$what=~s#(-.).*#$1#;
		$what=~s/\s*$//;

		next if defined $p->{"items"}->{$what};
# put it in items subarray as podname_serialnumber(?)
		$p->{"items"}->{$what} = $pod."_".$i++;
	    }
	    elsif($cmd =~ /^head/){
# if =head cmd
		$_=~s/\s*$//;
		next if defined($p->{"headers"}->{$_});
# put it in headers subarray as podname_serialnumber(?)
# serial numbers, etc., look to be used as tags to indicate a position
# in an html file.  No such luck in texinfo (sigh).
		$p->{"headers"}->{$_} = $pod."_".$i++;
	    }
	}
    }
}

# we can do all the above, just ignore the _ tagging stuff.  Maybe in the next
# version of texinfo, we can ref a char. position in an info file.

# start big top-level files which include everything:

&start_big_files;

debug ("Reading Pods");

	$modulepod = 0;

# (2) READ each pod, write structuring information
foreach $tmpod (@{$p->{"pods"}}, @modnames ){

# I dont think the table of contents is very useful in TeXinfo
# someone may disagree
    next if $tmpod eq 'perltoc.pod';

    if (defined($modnamehash{$tmpod})) {
	# its a module file
	$podfile = $modnamehash{$tmpod};
# this next line gives us an index into @modnames
	$modulepod++;
    } else {
	$podfile = $tmpod;
    }

    open(POD,"<$podfile") || die "cant open $podfile";
    ($pod=$tmpod)=~s/\.(pod|pm)$//;
    open(TEXINFO,">$pod.texi");

    ($curn,$prevn,$nextn,$upn) = ();

# now translate :: to /, for tex/info

    ($curn = $pod) =~ s{::}{/}g;

    if (! $modulepod) {
	
# check if we have the lines array - if so, we can use it to
# generate pod nodes and prev, next, etc. refs
# the lines array comes from perl.pod, giving us the correct ordering
# for the base pods

	if (@linesfornodes) {
	    $i=0;
	  podline: for (@linesfornodes) {
		last podline if $pod eq $_;
		$i++;
	    }
# if we got to $#linesfornodes+1, we didnt find it.
	    unless ($i == $#linesfornodes+1) {
		($prevn, $curn, $nextn) = @linesfornodes[($i?$i-1:0),$i,$i+1];
		$prevn = 'Top' if ($prevn eq $curn);
	    }
	}

# specific to master perl node

    $upn = ($pod eq 'perl')?'(dir)' :'Top';

    $pod eq 'perl' and ($curn,$nextn) = ('Top','perldata');
    $prevn eq 'perl' and $prevn = 'Top';

    $prevn ||= 'Top';
    $nextn ||= 'Top';

} else {
# module pod
# structuring information - yech
# just do a straight giant menu, if we can.  Calculate nodes similar to above.
    
    $idx = $modulepod -1;
    ($prevn,$curn,$nextn) = ($idx?$modnames[$idx-1]:'',
			     @modnames[$idx,$idx+1]);

    $prevn =~ s{::}{/}g;
    $curn =~ s{::}{/}g;
    $nextn =~ s{::}{/}g;

    $upn = $modnode;
# I used to have these - well just have empty previous and next
# to indicate beginning or end of a set of leaf nodes
#    $prevn ||= $modnode;
#    $nextn ||= $modnode;

}

    print STDOUT "for pod $pod, \@node $curn, $nextn, $prevn, $upn\n";
#    print STDOUT "@linesfornodes\n";

    print TEXINFO <<_EOF_;
\@node $curn, $nextn, $prevn, $upn
_EOF_



    $cutting = 1;
    $newenv = '';

    # (3) PROCESS each paragraph
$gotshortdesc = 0;
paragraph:    while (<POD>) {
	if ($cutting) {
	    next unless /^=/;
	    $cutting = 0;
	}
	chop;
	length || (print "\n") && next;

	# Translate verbatim paragraph

# greedy matching here will set $1 to all space before first nonspace
# at beginning of string.  Since its unlikely [Ed: but it happens
# sometimes, however, one common case is code examples with blank
# newlines] anything after that in the same paragraph will be
# outdented farther left than the first line, we can kill that much
# whitespace from the beginning of each line.  we kill whitespace from
# beginning of line for verbatim because example mode adds it back in.

# XXX - perhaps if we find 2 contiguous outdented paragraphs, we should put
# them in the same @example environment
# maybe the last line of the previous paragraph should be outdented
# the same as the first line of the next.

	if (($space) = /^(\s+)/) { 
	    &pre_escapes($_);
	    @lines = split(/\n/);
	    if($lines[0]=~/^\s+(\w*)\t(.*)/){  # maybe a menu
		($key,$rest)=($1,$2);
		if(defined($p->{"podnames"}->{$key})){ # yup, a menu
		    # process menu here. if not a menu, its an example
		    # or Its a menu.  Save it for end of node.
		    print TEXINFO "\n\@menu\n";
		    for (@lines) {
			m/^\s+(\w*)\t(.*)/;
			print TEXINFO "* $1:: $2\n";
		    }

# special case, top perl node
		    if ($key eq 'perl') {
			print TEXINFO <<"EOF";

* Module List:($modinfofile)$modnode. Got your modules, right here
* Function Index:: Perl functions and operators
* Predefined Variable Index:: Perl predefined variables
* Diagnostics Index:: Perl diagnostic messages


EOF
}

		    print TEXINFO "\@end menu\n\n";
# this next bit we will do by hand for now...
#		    for (@lines) {
#			m/^\s+(\w*)\t(.*)/;
#			print TEXINFO "\@include $1.texinfo\n";
#		    }
		    @linesfornodes = @lines;
		    map(s/^\s+(\w*)\t(.*)/$1/,@linesfornodes);
		    # done with menu paragraph, next paragraph
		    next;
		}
		# not a menu, process it as example
	    }
	    s/^$space//mg;
	    print TEXINFO "\@example\n", $_, "\@end example\n\n";
	    next;
	}

	$_ = &all_escapes($_,$pod);

	if (s/^=//) {
	    s/\n$//s;
	    s/\n/ /g;
	    ($cmd, $_) = split(' ', $_, 2);
	    if ($cmd eq 'cut') {
		$cutting = 1;
	    }
	    elsif ($cmd eq 'head1') {

# if NAME, slurp in the next paragraph and use it (instead of 'NAME') as
# a chapter title

# have to run it through the escapes ourselves
		if (/^\s*NAME/) {
		    $gotshortdesc = 1;
		    $podorigshortdesc = <POD>;
		    $podshortdesc = $podorigshortdesc;
		    # trim actual pod name (should we?)
		    $podshortdesc =~ s/^[^---]+-+\s*//;
		    $podshortdesc = &all_escapes($podshortdesc, $pod);
		    # if the name is multiline, change all but the
		    # last to a space
		    $podshortdesc =~ s/\n(.+)/ $1/g;

		    print TEXINFO qq|\@unnumbered $podshortdesc\n|;

		    # include a menu entry for module pods
		    
		    chop($nonewline = $podshortdesc);

		    $modulepod && print MODLISTTEXI
			              qq|* $ {curn}:: $nonewline|;
		}

		# print out the heading info anyway

		&closeenvs;
		print TEXINFO qq|\@unnumberedsec $_\n\n|;
		
		/^\s*NAME/ && ($_ = $podorigshortdesc,redo paragraph);

	    }
	    elsif ($cmd eq 'head2') {

		# if @env has an entry, we had =over or =items
		# but no =back - close it here

		&closeenvs;
		print TEXINFO qq|\@unnumberedsubsec $_\n\n|;
	    }
	    elsif ($cmd eq 'item') {
		($what,$rest)=split(' ', $_, 2);
		$what=~s/\s*$//;

# various cases - single star, star with stuff after it
# number, or something else

# people sometimes forget to put a leading =over.
# if =item at top level (empty array), assume =over

		if ($what =~ /[*]/) { # if a single star, axe it
		# texinfo itemize can put in its own star.
		    $_ = $rest;

		    if (scalar(@envs) == 0 or $newenv) {
			$newenv = 0,
			unshift (@envs, 'itemize'),
			print TEXINFO '@itemize @bullet', "\n";
		     };
		    
# if a single star, its a bulleted list with paragraphs - 
# need a newline before paragraphs if theres anything
# left on that line.  Else, if no star,
# its probably going to be a table - no newlines before paragraph

# sometimes its not a table (but more so than otherwise)
# tables do the right thing, @itemize items run together
# if no newline - no good heuristic if @item foo because
# dont know if table or itemize ahead of time

		    $next_para=1 if $rest;

# if digits, get rid of them ...
		} elsif ($what =~ /^\d+[.]?/) { 

	# texinfo enumerate can put in its own numbers
		    $_ = $rest;

		    if (scalar(@envs) == 0 or $newenv) {
			$newenv = 0,
			unshift (@envs, 'enumerate'),
			print TEXINFO '@enumerate', "\n";
		    }

# if a single star, its a bulleted list with paragraphs - 
# need a newline before paragraphs.  Else, if digits
# its enumerated - no newlines before paragraph

		    $next_para=0;
		} else {

		    if (scalar(@envs) == 0 or $newenv) {
			$newenv = 0,
			unshift (@envs, 'table'),
			print TEXINFO '@table @asis', "\n";
		    }
		}

		# only if we have starred items do we want to really
		# have separate items - else, two items
		# in a row is likely an itemx
		# candidate.  We will see how this goes

# previously we only wanted itemx if they had the first word
# in common (write, write FILEHANDLE, etc.)
#		if($justdid ne $what && $what =~ /[*]/){}

# if theres text on the same line as an item in enumerate,
# emacs texinfo processing will complain

		if(! $justdid || $what =~ /([*])|(\d+[.]?)/){
		    if ($rest && $envs[0] eq 'enumerate') {
			print TEXINFO "\@item \n$_\n";
		    } else {
			print TEXINFO "\@item $_\n";
		    }
		    ($pod =~ /perldiag/) && print TEXINFO "\@dgindex $_\n";
		    ($pod =~ /perlfunc/) && print TEXINFO "\@findex $_\n";
		    ($pod =~ /perlvar/) && print TEXINFO "\@vindex $_\n";
		    $justdid=$what;
		} else {
		    print TEXINFO qq{\@itemx $_\n};
		}
	    }
	    elsif ($cmd eq 'over') {
		# indicate start of a new itemization
		$newenv = 1;
		$justdid = '';
	    }
	    elsif ($cmd eq 'back') {
		# if @env is empty, we had an =over but no =items
		# bad form, but we can silently continue
		
		print TEXINFO '@end ', shift(@envs), "\n" if @envs;
	    }
	    else {
		warn "Unrecognized directive: $cmd\n";
	    }
	}
	else {
# not a perl command, so dont try to compare vs. the last item for itemxing
# upcoming paragraphs
	    $justdid = ''; 
	    
	    length || next;
# argh - in itemize, it sucks the whole thing up to the next line
# in table, it doesn't
# we don't know whether to do table or itemize

	    $next_para && print TEXINFO "\n";
#	    $next_para && (print TEXINFO  qq{<dd>\n});
	    print TEXINFO  "$_\n";
#	    $next_para && (print  TEXINFO qq{</dd>\n<p>\n}) && ($next_para=0);
	    $next_para = 0;
	}
    }

# clean up envs that ran off the end of the document

    &closeenvs;

# write to our big file of include statements
# need 2 newlines cuz of weirdness (bug?) in texinfo processing
    if (! $modulepod) {
	print BIGTEXI "\@include $nextn.texi\n\n" if ($nextn ne 'Top');
    } else {
	print BIGMODTEXI "\@include $modnames[$idx].texi\n\n" 
    }
# if no short description, still create menu entry
    if ($modulepod && ! $gotshortdesc) {
	print MODLISTTEXI "* ${curn}:: MISSING SHORT DESCRIPTION\n";
    }
}

# finish our big files

&finish_big_files;

#########################################################################

sub all_escapes {
    local($_,$pod) = @_;
    &pre_escapes($_);
# bug in texinfo - @@ at beginning of line gets hosed
# only need to fix if not =over paragraph - those work ok
    s/\n@@/\n @@/g;
    $_ = &Do_refs($_,$pod);
	
    s/Z<>//g; #  what to do with this?
#	s/E<lt>/</g;
#	s/E<gt>/>/g;

    s/Less_Than_Special_Sequence/</g;
    s/Greater_Than_Special_Sequence/>/g;

    return $_;

    }

sub pre_escapes {
    local($_) = @_;
    s/E<lt>/Less_Than_Special_Sequence/g;
    s/E<gt>/Greater_Than_Special_Sequence/g;
    s/[\@{}\`\']/\@$&/g;
#    s/C<E<lt>E<lt>>/\@code{<<}/g;
#    s/C<-E<gt>>/\@code{->}/g;
    $_[0] = $_;
}

sub post_escapes{
    local($_) = @_;
#    s/>>/\&gt\;\&gt\;/g;
#    s/([^"AIB])>/$1\&gt\;/g;
    $_[0] = $_;
}

sub Do_refs{
local($para,$pod)=@_;

# quick hack, but may slow things down considerably
# since tags are nestable, we must keep going until we cant see
# any more X< (this means we must make sure E<lt> doesnt get
# translated to a < prior to a call to this function, to be safe).  Done
# in pre_escapes.

# this will *HANG* if tags are mis-nested!!!!

$iter = 0;
pass: while ($para =~ /[LCIBSFZ]</) {
    if ($iter++ == 15) { 
	print  <<EOF;
Too many iterations on this paragraph:

$para

Most likely an unescaped < or > (use E<lt> or E<gt> instead) or one
of those is missing its mate.

EOF
	last pass;
    }
foreach $char (qw(L C I B S F Z)){
    next unless $para =~ /($char<[^<>]*>)/;
# @ar = split paragraph, making array elements out of 
# the current char<foo> as well as regular text
    local(@ar) = split(/($char<[^<>]*>)/,$para);
    local($this,$key,$num,$sec,$also);
# for all @ar elements,
    for($this=0;$this<=$#ar;$this++){
# only handle the current chars char<foo> thingies
        next unless $ar[$this] =~ /$char<([^<>]*)>/;

# if just single foo, $key = foo.  Else if foo/bar, $key = foo, 
# $sec = bar.
	$key=$1;
    ($chkkey,$sec) = ($key =~ m|^([^/]+)(?:/([^/]+))?|);
# XXX if chkkey was '' but there was a slash, use the 'in this node' case
# if the key matches a podname, put in a ref to the pod
	if((defined($p->{"podnames"}->{$chkkey})) && ($char eq "L")){

	    $also = "\@samp{$sec}, " if $sec ;
#	    $ar[$this] = "${also}\@xref{$chkkey,\u$chkkey,,$chkkey.info},";

# specify the other info file, if necessary:

	    $shortname = $chkkey;

	    if (!defined $modnamehash{$pod} &&
		defined $modnamehash{$chkkey}) {
		$chkkey = "($modinfofile)$chkkey";
	    } elsif (defined $modnamehash{$pod} &&
		     !defined $modnamehash{$chkkey}) {
		$chkkey = "(perl.info)$chkkey";
	    }

# as stated above, keys to podnames have ::, which we must translate
# to /
	    $shortname =~ s{::}{/}g;
	    $chkkey =~ s{::}{/}g;

	    $ar[$this] = "${also}\@xref{$chkkey,\u$shortname},";
	    # *note arg2: (arg3) arg1
# otherwise, if char is still "L", then key didnt match a podname
# and therefore is a section on the current manpage
	} 
    elsif ($char eq "L") {
	$ar[$this] = "\@samp{$chkkey} in this node";
	}

# if the key matches an item, put in a ref to the item def.
# ignore this for now
	elsif(defined($p->{"items"}->{$key})){
	    ($pod2,$num)=split(/_/,$p->{"items"}->{$key},2);
		$ar[$this] = (($pod2 eq $pod) && ($para=~/^\=item/)) ?
#		"\n<A NAME=\"".$p->{"items"}->{$key}."\">\n$key</A>\n"
		    $key:$key
#		"\n$type$pod2.html\#".$p->{"items"}->{$key}."\">$key<\/A>\n"
		    ;
        }
# if the key matches a header, put in a ref to the header def.
#ignore this to start with
	elsif(defined($p->{"headers"}->{$key})){
	    ($pod2,$num)=split(/_/,$p->{"headers"}->{$key},2);
		$ar[$this] = (($pod eq $pod2) && ($para=~/^\=head/)) ? 
#		"\n<A NAME=\"".$p->{"headers"}->{$key}."\">\n$key</A>\n"
		$key:$key
#		"\n$type$pod2.html\#".$p->{"headers"}->{$key}."\">$key<\/A>\n"
		    ;

# if its an item or header, formatting will be lost because
# of the else construct here

	}
	else{
	    (warn "No \"=item\" or \"=head\" reference for $ar[$this] in $pod\n") if $debug;
	    if ($char eq "L"){
		$ar[$this]=$key;
	    }
	    elsif($char eq "I"){
		$ar[$this]="\@emph{$key}";
	    }
	    elsif($char eq "B"){
		$ar[$this]="\@strong{$key}";
	    }
	    elsif($char eq "S"){
		$ar[$this]="\@w{$key}";
	    }
	    elsif($char eq "C"){
		$ar[$this]="\@code{$key}";
	    }
	    elsif($char eq "F"){
		$ar[$this]="\@file{$key}";
	    }
	}
    }
    $para=join('',@ar);
}
}
$para;
}

sub intern_modnamehash {

# File::Find is pretty screwy.
# I think we can't modify $_ or File::Find can screw up

    my $shortpath;
    my $thename;
    
# this could be a problem - if we search $sitelibdir,
# its usually a subdir of $libdir, in which case we don't want it
# to think 'site_perl' is a class name.

# XXX - may be doing toplevel modules incorrectly in the above case
# is 'name' just the filename?  thats not good ....
    $thename = $File::Find::name;
    $shortpath = $thename;

# kill leading './'

    $thename =~ s{^[.]/}{};

# XXX - take the current $libdir (/foo/bar) 
# and see if the file were testing (/foo/bar/site_perl/Plugh/Blah.pm) is
# in any *other*, deeper subdir in @INC
# (/foo/bar/site_perl) - if so, skip this entry, cuz the deeper 
# subdir will catch it properly (Plugh::Blah)

# for other libraries that are proper subdirs of the current libdir
    foreach $otherlibrary (grep /^$libdir.+/, @INC) {

# if the other library is part of the current files path, skip it
# because it will be caught when the other library is used

	if ("$libdir/$thename" =~ /^$otherlibrary/) {
	    print ".";
#	    print "Skipping $thename\n";
#	    print "cuz $otherlibrary caught/will catch it\n";
	    return;
	}
    }

# exclude base pods - perlfoo.pod
    ($thename =~ m/perl.*[.]pod/) && return;

# for each file entry, kill trailing '.(pod|pm)'
    (-f "$libdir/$thename") &&
	($thename =~ s{^(.*)[.](pod|pm)$ }{$1}x) or return;

# '.pod' files nonhierarchical - keep only last component as module name.
# well, hierarchical in Tk ... keep it hierarchical for now

#    if ($2 eq 'pod') {$thename =~ s{.*/([^/]+)}{$1}; }
    
# translate to module syntax

    $thename =~ s{/}{::}g;

# if its already in the hash, skip it.  We're following @INC order,
# which means if its found in a earlier @INC directory, it will
# be the one thats `use'd.  So rather than overwriting an earlier
# @INC entry with a newer one, we skip the newer one if the earlier
# one exists (or, we could do the foreach on (reverse @INC) instead
# of (@INC)).

    
    if (defined $modnamehash{$thename}) {
#	print "already found $thename\n";
#	print "in $modnamehash{$thename}\n";
	return
    };

    $modnamehash{$thename} = "$libdir/$shortpath";

# If this is a .pm file, is there actually any documentation in it?    
    
	if ($modnamehash{$thename} =~ /[.]pm$/) {
	    open(MODULE, "$modnamehash{$thename}");
	  line: while ($theline = <MODULE>) {
		$theline =~ /^=head\d/ && last line;
		eof(MODULE) && delete $modnamehash{$thename};
	    }
	}

    echopod($thename) if $modnamehash{$thename};
}

sub wait{1;}

sub echopod {

    $savenew = $_[0];

# if neither has a ::, same line

    if ($oldpod !~ /::/ && $_[0] !~ /::/) {

# if old one has a ::, different lines

    } elsif ($oldpod =~ /::/ && $_[0] !~ /::/) {

	print "\n";

    } elsif ($oldpod !~ /::/ && $_[0] =~ /::/) {

# if its the new one that has ::, start a header line

	($new) = ($_[0] =~ /^([^:]+)::/);
	print "\n${new} modules: ";
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
	    print "\n${new} modules: ";
	    $_[0] = $';
	}
    } 

    $oldpod = $savenew;
    
    print $_[0], " ";

}


sub start_big_files {

open(BIGTEXI,">bigperl.texi"); 
    print BIGTEXI <<'_EOF_' ;
\input texinfo.tex
@comment %**start of header
@setfilename perl.info
settitle perl
@c footnotestyle separate
@c paragraphindent 2
@smallbook
@comment %**end of header
@defindex dg

@include perl.texi

_EOF_

open(BIGMODTEXI,">bigpm.texi"); 
    print BIGMODTEXI <<'_EOF_', "\@setfilename $modinfofile\n", <<'_MOREEOF_';
\input texinfo.tex
@comment %**start of header
_EOF_
@settitle Big Module file
@c footnotestyle separate
@c paragraphindent 2
@smallbook
@comment %**end of header

@comment Giant module file.

@include modlist.texi

_MOREEOF_

open(MODLISTTEXI,">modlist.texi"); 
    print MODLISTTEXI <<"_EOF_" ;
\@node $modnode, (perl.info)Function Index, ,(perl.info)Top
\@unnumbered Module List

This is it!  The list of the installed (documented) modules on
$ENV{'HOST'}, as of $date.

Texinfo source for this file was generated by $ENV{'USER'} running
pod2texi.pl.  This info file itself was created \@today{}.

\@menu

_EOF_

}

sub finish_big_files {
print BIGTEXI << 'EOF';
@include indices.texi

@summarycontents
@contents

@bye
EOF

print BIGMODTEXI << 'EOF';
@summarycontents
@contents

@bye
EOF

print MODLISTTEXI << 'EOF';
@end menu
EOF

# make an index file

open(INDICES,">indices.texi");
print INDICES <<'EOF';

@node Function Index, Predefined Variable Index, , Top
@comment    node-name,         next,       previous, up
@unnumbered Function Index

@printindex fn

@node Predefined Variable Index, Diagnostics Index, Function Index, Top
@comment    node-name,         next,       previous, up
@unnumbered Predefined Variable Index

@printindex vr

@node     Diagnostics Index,  , Predefined Variable Index, Top
@comment      node-name, next,       previous, up
@unnumbered Diagnostics Index

@printindex dg

EOF
}

sub debug {
print "\n", '=' x 79, "\n$_[0]\n", '=' x 79 , "\n";
}

sub closeenvs {
    for (@envs) {
	print TEXINFO "\@end $_\n";
    }
    @envs = ();
}
