extproc perl -Ss
#!i:/perllib/bin/perl -s

eval 'exec i:/perllib/bin/perl -s -S $0 ${1+"$@"}'
    if 0; # not running under some shell
use XML::DT;

our ($dtd, $html);

if ($dtd) {

  $/ = ">";
  while(<>) {
    s/\n/ /g;
    s// /g;
    s/^\s*//;
    if (m/<!ELEMENT\s+([A-Za-z_-]+)/) {
      $elem{$1} = [];
    } elsif (m/<!ATTLIST\s+([A-Za-z_-]+)\s+(.*)\s*>/) {
      my $elem = $1;
      my $fields = $2;
      while($fields =~ m!([A-Za-z_-]+)+.*?#(REQUIRED|IMPLIED)\s*!g) {
	push @{$elem{$elem}}, $1;
      }
    } else {
      #print STDERR "Ignoring $_\n";
      #---
    }
  }

  print <<'EOH';
#!/usr/bin/perl -w
use XML::DT;

my $filename = shift;
my %handler = (
EOH

  for (keys %elem) {
    print "\t'$_' => sub {},";
    print " #remember ",join(", ",map {"'$_'"} @{$elem{$_}}) if @{$elem{$_}};
    print "\n";
  }

  print <<'EOH';
);
print dt($filename, %handler);
EOH

} else {
  if ($html) {
    mkdtskel ("-html", @ARGV);
  } else {
    mkdtskel (@ARGV);
  }
}

__END__

=head1 NAME

mkdtskel - Perl code skeleton generator to process XML files with XML::DT

=head1 SYNOPSIS

  mkdtskel <xmlfile>

  mkdtskel -dtd <dtdfile>

  mkdtskel -html <htmlfile>

=head1 DESCRIPTION

Use this command to prepare a skeleton file with basic code needed to
process your XML file with XML::DT; The command checks the element
names and for each one, the attributes. This information is described
on the generated file to remember the programmer.

=head1 SEE ALSO

XML::DT(1), mkdtdskel(1) and perl(1)

=cut
