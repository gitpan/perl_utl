@echo off

rem: To use it, you can provide the name of Perl to check as a command arg,
rem: as in
rem      testperl perl_


set tperl=%1
if "%tperl%" == "" set tperl=perl

echo ====================================
echo Testing %tperl%...  Getting version...
echo ====================================
rem Errorlevel is not set if the program is not found?!
%tperl% -e "exit 1"
if errorlevel 1 goto mayhave
goto noperl

:mayhave
%tperl% -v
if errorlevel 1 goto noperl
goto haveperl

:noperl
echo Cannot start %tperl% !!!  Please check that you have %tperl% on PATH,
echo and (if you do not use perl_.exe) that perl.dll is on LIBPATH.
echo ... Keep in mind that changes to config.sys DO NOT take effect
echo until reboot, and LIBPATH can be changed ONLY from config.sys.
input "Press <ENTER>" %%i
exit 1

:haveperl
perl -S -x testperl.cmd
exit 0

#! perl

sub get_help;

$| = 1;
print <<EOP;
====================================
We checked that Perl can be loaded.  Apparently, your PATH and LIBPATH 
are set to reasonable values.  From now on the tests are coded in Perl.
EOP

print "Press <ENTER>: ";
<>;
print <<EOP;
====================================
The next step is to check that you installed shell in a location perl can 
find, so that you can start external programs.
EOP
open SH, 'echo $SH_VERSION |' or $no_sh++;
unless ($no_sh) {
  chomp ($v = <SH>);
  close SH or warn "Errors closing pipe from shell: \$!='$!', \$?=$?\n";
  print "I could find your shell, its version is '$v'\n" if $v;
  $no_sh++ unless $v;
}
if ($no_sh) {
  print <<EOP;
====================================
I could not find your shell.  You WILL have problems starting external
programs (except the simplest ones).  Please check that you installed
a shell with a Bourne syntax and informed Perl where to find it.
EOP
  if (exists $ENV{PERL_SH_DIR}) {
    print <<EOP;
====================================
You had set an environment variable PERL_SH_DIR. However,
EOP
    if (-d $ENV{PERL_SH_DIR}) {
      print <<EOP;
though there is a directory with name '$ENV{PERL_SH_DIR}',
EOP
      if (-f "$ENV{PERL_SH_DIR}/sh.exe") {
	print <<EOP;
and there is a file with name '$ENV{PERL_SH_DIR}/sh.exe', I could not 
execute it!  This should not happen!  
If this file is executable by hand, please report to 
   ilya\@math.ohio-state.edu.
Thanks.
EOP
      } else {
	print <<EOP;
there is NO FILE with name '$ENV{PERL_SH_DIR}/sh.exe' !
EOP
	get_help '"Starting OS/2"', 'PERL_SH_DIR';
      }
    } else {
      print <<EOP;
there is NO DIRECTORY with name '$ENV{PERL_SH_DIR}' !
EOP
      get_help '"Starting OS/2"', 'PERL_SH_DIR';
    }
  } else {
    print <<EOP;
You do not have PERL_SH_DIR set in your environment.
EOP
    get_help '"Starting OS/2"', 'PERL_SH_DIR';
  }
} else {
  open SH, 'DDD_VERSION=716; echo $DDD_VERSION |' 
    or $no_sh++;
  chomp ($v = <SH>);
  close SH or warn "Errors closing pipe from shell: \$!='$!', \$?=$?\n";
  print <<EOP unless $v == 716;
====================================
  Got '\$v=$v', expected '\$v=716' !
Though a shell can be found, it does not follow Bourne shell conventions.
Expect a lot of problems when starting external programs which expect Bourne
shell semantics.
EOP
}

print "Press <ENTER>: ";
<>;

print <<EOP;
====================================
Ouph, the most frequent problem is behind...  Now testing Perl library search.
EOP

eval 'use Config; 1' or $no_config++;
if ($no_config) {
  print <<EOP;
Cannot find the core of Perl library.  If you have it installed:
EOP
  get_help "PERLLIB_PREFIX";
  print <<EOP;
Skipping further tests now...
EOP
} else {
  print <<EOP;
Found Perl library.  Fine...
EOP
  print "Press <ENTER>: ";
  <>;

  $prefix = 'f:/perllib/';
  if (exists $ENV{PERLLIB_PREFIX}) {
    @parts = split /[ ;]/, $ENV{PERLLIB_PREFIX}, 2;
    print <<EOP if @parts < 2;
====================================
Malformed PERLLIB_PREFIX!  Two parts should be separated by SPACE or SEMICOLON!
EOP
    (substr $prefix, 0, (length $part[0])) = $part[1] 
      if substr $prefix, 0, (length $part[0]) eq $part[0];
  }
  $prefix =~ s|/$||;
  @f_keys = grep { $Config{$_} =~ /^(?!\Q$prefix\E\b\S*$)[a-z]:/i
		     and not /^(emxpath|libemx)$/} keys %Config;
  foreach (@f_keys) {
    next if 
      /^(full_sed|libc|make|rsx|sh|strings|sysman|timeincl|usrinc)$/ 
	and (-e $Config{$_} or -e "$Config{$_}.exe");
    if ($_ eq 'libpth') {
	$baddir = 0;
	foreach $dir (split /\s+/, $Config{libpth}) {
	    $baddir++, last unless -d $dir;
	}
	next unless $baddir;
    }
    print <<EOP unless $have_bad++;
====================================
The following entries in '$INC{'Config.pm'}' 
contain path settings which look suspicious:
EOP
    print " $_ => '$Config{$_}'\n";
  }
  print <<EOP unless $have_bad;
====================================
Path entries in '$INC{'Config.pm'}' look OK: either start 
with reasonable prefix, or mention existing directories/files.
EOP
  print "Press <ENTER>: ";
  <>;

  eval "use Net::Config; 1" or $no_netconfig++;
  if ($no_netconfig) {
    print <<EOP;
====================================
You do not have Net::Config, so cannot have problems with it!  ;-)  OK...
EOP
  } elsif ($NetConfig{inet_domain} ne 'tusik.hip.berkeley.edu') {
    print <<EOP;
====================================
Apparently you edited '$INC{'Net/Config.pm'}', 
so I expect you know what you did.  OK...
EOP
  } else {
    print <<EOP;
====================================
You DID NOT edit '$INC{'Net/Config.pm'}', 
You may have a lot of problems with networking...  Please edit this file.
EOP
  }
}
print "Tests finished. Press <ENTER>: ";
<>;

sub get_help {
  print <<EOP;
Consider getting help via
EOP
  foreach (@_) {
    print <<EOP;
     view perl $_
EOP
  }
  print <<EOP;
or in whatever way you like the Perl documentation accessed, say, via 
  perldoc perlos2
man, acrobat, netscape, lynx, GNU info, TeX, reading REAME.os2, reading
POD files, and so on.
EOP
}

