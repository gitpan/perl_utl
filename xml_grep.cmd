extproc perl -Sw
#!i:/perllib/bin/perl -w

eval 'exec i:/perllib/bin/perl -w -S $0 ${1+"$@"}'
    if 0; # not running under some shell
use strict;

use Getopt::Long;
use Pod::Usage;
use XML::Twig;

my $VERSION="0.4";

# options (all used globally in the script)
my( $help, $man, @roots, @paths, $files, $count, $nb_results, $encoding,
    $wrap, $descr, $group, $pretty_print, $version, $text_only);

# used to check if the wrapping tags need to be output
my $results      = 0;
my $file_results = 0;

# first process the case where the user provides only 
# an xpath expression and a list of files
 if( (@ARGV > 1) && ($ARGV[0] !~ m{^-}) )
   { splice( @ARGV, 0, 0, '--group_by_file', 'file', '--pretty_print', 'indented', '--cond'); }

GetOptions( 'help'            => \$help,
            'man'             => \$man,
	          'version'         => \$version,
            'root=s'          => \@roots,
            'cond=s'          => \@paths,
	          'files'           => \$files,
	          'count'           => \$count,
	          'nb_results=i'    => \$nb_results,
	          'encoding=s'      => \$encoding,
	          'wrap:s'          => \$wrap,
	          'descr:s'         => \$descr,
            'group_by_file:s' => \$group,
	          'pretty_print:s'  => \$pretty_print,
			      'text_only'       => \$text_only,
	  ) or pod2usage(2);

pod2usage(1) if $help;
pod2usage(-exitstatus => 0, -verbose => 2) if $man;
if( $version) { warn "$0 version $VERSION\n"; exit; }
	
unless( @roots or @paths or $files)  { pod2usage(1); exit; }
if( ($files or $count) and !@paths)  { pod2usage(1); exit; }
if( ($files or $count) and (@roots or $encoding or defined( $wrap) 
                or defined( $group) or defined( $pretty_print)))
                                     { pod2usage(1); exit; }
if( $files and !@ARGV)               { pod2usage(1); exit; }
if( !$files and !$count and @paths and !@roots) { @roots= @paths; @paths=(); }

# defaults for optional arguments to options
$group        = 'file'     if( defined $group        and !$group);
$pretty_print = 'indented' if( defined $pretty_print and !$pretty_print);
$wrap         = 'xml_grep' if( !defined( $wrap) and (@roots or @paths));
if( !defined( $descr) and (@roots or @paths))
  { my $date= localtime();
    $descr        = qq{version="$VERSION" date="$date"}
  } 

# some globals
my( $current_file, $count_file, $count_total);

# will be used to create the twig
my %options;  

if( $count)
  { my $twig_roots={};
    my $twig_root= sub { $count_file++; $_[0]->purge; };
    foreach my $path (@paths)
      { $twig_roots->{$path}= $twig_root; }

    $options{twig_roots}= $twig_roots;
  }
else
  { if( @roots)
      { my $root_handlers={};
        my $root_handler= sub { my( $t, $root)= @_;
                                if( !@paths or $_->att( '#print'))
                                  { print result_start()        unless( $results); 
                                    print file_result_start()   unless( !$group or $file_results);
                                    if( $text_only)
																		  { print $root->text, "\n"; }
																		else
																		  { $root->print; }
                                    $nb_results--;
                                    unless( $nb_results) { $@= "XMLGREP: NB_RESULT_REACHED"; die; }
                                  }
                                $t->purge; 
                              };
        foreach my $root (@roots)
          { $root_handlers->{$root}= $root_handler; }

        $options{twig_roots}= $root_handlers;
      }

    if( @paths)
      { my $twig_handlers={};
         my $twig_handler;
         if( $files)
           { $twig_handler= sub { $@="XMLGREP: FOUND"; die; }; }
         else
           { $twig_handler= sub { my( $t, $hit)= @_;
                                  foreach my $elt ( $hit->ancestors_or_self) 
                                    { $elt->set_att( '#print' => 1); }
                                };
           }
        foreach my $path (@paths)
          { $twig_handlers->{$path}= $twig_handler; }

        $options{twig_handlers}= $twig_handlers;
      }
  }
    
    


$options{pretty_print}    = $pretty_print if( $pretty_print);
$options{output_encoding} = $encoding     if( $encoding); 
     
my $t= XML::Twig->new( %options);

if( @ARGV)
  { foreach my $file (@ARGV)
      { $current_file= $file;
        my $ok= $t->safe_parsefile( $file);
        if( !$ok)
          { if( $@ =~ m{XMLGREP: FOUND}) 
              { # in files mode
                print $current_file, "\n";
                $nb_results--;
                exit unless( $nb_results);
              }
            elsif( $@ =~ m{^XMLGREP: NB_RESULT_REACHED})
              { print file_result_end() if( $file_results);
                print result_end()      if( $group && $results);
                exit;
              }
            else
              { die /$@/; }
          }
        else
         { if( $count)    { print "$current_file: $count_file\n";
			    $count_total += $count_file;
			    $count_file=0;
                          }
			    
           elsif( @roots) { print file_result_end() if( $file_results); }
	   elsif( $count) { print "$count_total matches\n"; }
         }
      }
    if( $count) { print "total: $count_total\n"; }
    print result_end() if( $results);
  }
else
  { $file_results=0;
    my $ok= $t->safe_parse( \*STDIN);
    die $@ if( !$ok and ( $@ !~ m{^XMLGREP: NB_RESULT_REACHED}));
    if( $count) { print "$count_total matches\n"; }
    else        { print result_end();             }
  }


sub result_start
  { $results=1;
		return if( $text_only);
    my $enc_decl= $encoding ? qq{encoding="$encoding" } : '';
    return   qq{<?xml version="1.0" $enc_decl?>\n}
           . qq{<$wrap $descr>\n};
  }

sub result_end
  { my $result;
		return if( $text_only);
    if( !$group) { $result= "\n"; }
    $result .= qq{</$wrap>\n};
    return $result;
  }

sub file_result_start
  { $file_results=1;
		return if( $text_only);
    my $result;
    $result= qq{<$group filename="$current_file">};
    if( !$pretty_print)
      { $result.= "\n"; }
    return $result;
  }

sub file_result_end
  { $file_results=0;
		return if( $text_only);
    return qq{\n</$group>\n};
  }



__END__

=head1 NAME

xmlgrep - grep XML files looking for specific elements

=head1 SYNOPSYS

xmlgrep [options] [file ...]

 Options:
   -help            brief help message
   -man             full documentation

=head1 OPTIONS

=over 4

=item B<--help>

brief help message

=item B<--man>

full documentation

=item B<--version>

display the tool version

=item B<--root> <cond>

look for and return xml chunks matching <cond>

if neither C<--root> nor C<--file> are used then the element(s)
that trigger the C<--cond> option is (are) used. If C<--cond> is
not used then all elements matching the <cond> are returned

several C<--root> can be provided

=item B<--cond> <cond>

return the chunks (or file names) only if they contain elements matching <cond>

several C<--cond> can be provided (in which case they are OR'ed)

=item B<--files>

return only file names (do not generate an XML output)

usage of this option precludes using any of the options that define the XML output:
C<--roots>, C<--encoding>, C<--wrap>, C<--group_by_file> or C<--pretty_print>

=item B<--count>

return only the number of matches in each file

usage of this option precludes using any of the options that define the XML output:
C<--roots>, C<--encoding>, C<--wrap>, C<--group_by_file> or C<--pretty_print>

=item B<--nb_results> <nb>

output only <nb> results

=item B<--encoding> <enc>

encoding of the xml output (utf-8 by default)

=item B<--wrap> <tag>

wrap the xml result in the provided tag (defaults to 'xml_grep')

=item B<--descr> <string>

attributes of the wrap tag (defaults to C<< version="<VERSION>" date="<date>" >>)

=item B<--file_wrap> <optional_tag>

wrap results for each files into a separate element. By default that element 
is named C<file>. It has an attribute named C<filename> that gives the name of the 
file.

the short version of this option is B<-g>

=item B<--pretty_print> <optional_style>

pretty print the output using XML::Twig styles ('C<indented>', 'C<record>'
or 'C<record_c>' are probably what you are looking for) 

if the option is used but no style is given then 'C<indented>' is used

short form for this arggument is B<-s>

=item B<--text_only>

Displays the text of the results, one by line.

=back

=head2 Condition Syntax

<cond> is an XPath-like expression as allowed by XML::Twig to trigger handlers. 
	 
exemples: 
  'para'
  'para[@compact="compact"]'
  '*[@urgent]'
  '*[@urgent="1"]'
  'para[string()="WARNING"]'

see XML::Twig for a more complete description of the <cond> syntax

options are processedby Getopt::Long so they can start with '-' or '--'
and can be abbreviated (-r instead of --root for example)

=head1 DESCRIPTION

B<xmlgrep> does a grep on XML files. Instead of using regular 
expressions it uses XPath expressions (in fact the subset of 
XPath supported by XML::Twig)

the results can be the names of the files or XML elements 
containing matching elements.

=head1 SEE ALSO

XML::Twig Getopt::Long

=head1 LICENSE

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

=head1 AUTHOR

Michel Rodriguez <mirod@xmltwig.com>


