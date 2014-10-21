extproc perl -S
#!i:/perllib/bin/perl

eval 'exec i:/perllib/bin/perl  -S $0 ${1+"$@"}'
    if 0; # not running under some shell

use strict;
use XML::Handler::YAWriter;
use XML::Parser::PerlSAX;
use IO::File;

my $options;
   $options->{Output} = new IO::File(">-");

while ($ARGV[0] =~ "^--") {
	my $opt=shift @ARGV;
	$opt =~ s/^--//;
	$options->{Pretty}{$opt}=1;
}

my $file = new IO::File( $#ARGV >=0 ? "<".$ARGV[0] : "<-" );

my $ya = new XML::Handler::YAWriter( $options );
my $perlsax = new XML::Parser::PerlSAX(
	'Handler' => $ya,
	'Source' => { ByteStream => $file }
	);

$perlsax->parse();

0;

=head1 NAME

xmlpretty - XML pretty printer

=head1 SYNOPSIS

  xmlpretty [--options] [filename]

=head1 DESCRIPTION

B<xmlpretty> is the commandline interface to XML::Handler::YAWriter,
acting as a tool to add and remove pretty printing to XML files.

B<xmlpretty> has several methods to add human readablitiy.

If you want to add readablity without adding so-called I<ignorable
whitespace>, use it in the following way :

  $ xmlpretty --AddHiddenNewline \
              --AddHiddenAttrTab \
          --CatchEmptyElement \
          uglyfile.xml > prettyfile.xml

If you do B<not> want to process the file further, but only want it human
readable, add visible whitespace to the file as follows :

  $ xmlpretty --PrettyWhiteNewline \
              --PrettyWhiteIndent \
          --CatchEmptyElement \
          uglyfile.xml > prettyfile.xml

You may use YAWriter to clean whitespace from XML documents.
This may work in 99% of the cases where you want to get rid of
ignorable whitespace caused by the various forms of pretty
printing.

  $ xmlpretty --NoWhiteSpace \
              --NoComments \
              --AddHiddenNewline \
          --AddHiddenAttrTab \
          --CatchEmptyElement \
          prettyfile.xml > cleanfile.xml

=head2 Options

Options are given in a gnu like --option idiom.

=over

=item AddHiddenNewline boolean

Add hidden newline before ">"

=item AddHiddenAttrTab boolean

Add hidden tabulation for attributes

=item CatchEmptyElement boolean

Catch empty Elements, apply "/>" compression

=item CatchWhiteSpace boolean

Catch whitespace with comments

=item IsSGML boolean

This option will cause start_document, processing_instruction and doctype_decl
to appear as SGML. The SGML is still well-formed of course, if your SAX events
are well-formed.

=item NoComments boolean

Supress Comments

=item NoDTD boolean

Supress DTD

=item NoPI boolean

Supress Processing Instructions

=item NoProlog boolean

Supress <?xml ... ?> Prolog

=item NoWhiteSpace boolean

Supress WhiteSpace to clean documents from prior pretty printing.

=item PrettyWhiteIndent boolean

Add visible indent before any eventstring

=item PrettyWhiteNewline boolean

Add visible newlines before any eventstring

=item SAX1 boolean (not yet implemented)

Output only SAX1 compilant eventstrings

=back

=head2 Bugs:

Automatic recoding between 8bit and 16bit does not yet work correctly !

I have Perl-5.6 at home and here I can specify "use utf8;" in the right
places to make recoding work. But I dislike saying "use 5.00555;" because
many systems run 5.00503.

=head1 AUTHOR

Michael Koehne, Kraehe@Copyleft.De

=head1 Thanks

"Derksen, Eduard (Enno), CSCIO" <enno@att.com> helped me with the Escape
hash and gave quite a lot of usefull comments.

=head1 SEE ALSO

L<perl> and L<XML::Parser::PerlSAX>

=cut
