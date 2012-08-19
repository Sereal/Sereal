use 5.008;

use strict;
use warnings;

use ExtUtils::MakeMaker;
use Config;

my $shared_dir = "../shared";
my $in_source_repo = -d "../.git" and -d $shared_dir;

# This fires from a git source tree only.
# Right now, all devs are on Linux. Feel free to make portable.
eval {
  if (-d "../.git" and -d $shared_dir) {
    # overwrite by default
    opendir my $dh, $shared_dir or die $!;
    foreach my $file (grep -f "$shared_dir/$_", readdir($dh)) {
      if (!-e $file || !-l $file) {
        open my $ifh, "<", "$shared_dir/$file" or die $!;
        open my $ofh, ">", $file or die $!;
        print $ofh "\n# /* WARNING: GENERATED CODE! DO NOT EDIT! See shared/ folder in source repo. */\n\n";
        print $ofh $_ for <$ifh>;
      }
    }
    closedir $dh;
  }
  1
} or warn $@;


# This section deals with extracting constants from the protocol
# definition and including them as Perl constants. Not pretty, I know.
# Regenerate constants if module available.
# TODO move to shared area. Get out of Makefile.PL
if (eval { use ExtUtils::Constant qw(WriteConstants); 1 }) {
  print "Generating constant exports for Perl...\n";
  open my $fh, "<", "srl_protocol.h" or die $!;
  my (@string_const, @int_const);
  while (<$fh>) {
    if (/^#\s*define\s*(SRL_\w+)\s*(.*)$/) {
      my ($name, $value) = ($1, $2);
      next if $name =~ /_H_$/ or $value =~ /"/;
      push @int_const, $name;
    }
  }
  close $fh;
  WriteConstants(
      NAME => 'Sereal::Encoder',
      NAMES => \@int_const,
  );
  open my $ofh, ">", "lib/Sereal/Constants.pm" or die $!;
  print $ofh <<HERE;
# Genereated code! Do not modify! See Makefile.PL.

package Sereal::Constants;
use strict;
use warnings;
use Carp qw(croak);
use Sereal::Encoder; # for XSLoading
our \@ISA = qw(Exporter);
require Exporter;
our \@EXPORT_OK;
BEGIN { \@EXPORT_OK = qw(
HERE
  print $ofh "    $_\n" for (@int_const);
  print $ofh <<'HERE';
  );
  my $code;
  foreach my $constname (@EXPORT_OK) {
    my ($error, $val) = constant($constname);
    if ($error) { croak($error); }
    $code .= "sub $constname () {$val}\n";
  }
  eval "$code\n1;" or do {
    my $err = $@ || 'Zombie error';
    die "Failed to generate constant subs: $err\n Code was:\n$code\n";
  };
}

sub SRL_MAGIC_STRING () {"srl"}
push @EXPORT_OK, "SRL_MAGIC_STRING";
our %EXPORT_TAGS=(all => \@EXPORT_OK);
HERE
  close $ofh;
}
elsif (-d $in_source_repo) {
  warn "Please install ExtUtils::Constant since you appear to be running out of the source repository.\n";
}

# END of constant-related crap

our $OPTIMIZE;

my $defines = join " ", map "-D$_", grep exists $ENV{$_},
              qw(NOINLINE DEBUG MEMDEBUG NDEBUG ENABLE_DANGEROUS_HACKS);

if ($Config{gccversion}) {
    $OPTIMIZE = '-O3 -Wall -W';
} elsif ($Config{osname} eq 'MSWin32') {
    $OPTIMIZE = '-O2 -W4';
} else {
    $OPTIMIZE = $Config{optimize};
}

if ($ENV{DEBUG}) {
  $OPTIMIZE .= ' -g';
}
else {
  $defines .= " -DNDEBUG";
}

# See lib/ExtUtils/MakeMaker.pm for details of how to influence
# the contents of the Makefile that is written.
WriteMakefile1(
    MIN_PERL_VERSION => '5.008',
    META_MERGE => {
        resources => {
            repository => 'git://github.com/tsee/Sereal.git'
        },
    },
    BUILD_REQUIRES => {
        'Test::More' => 0.88,
        #'ExtUtils::Constant' => '0.23',
    },
    NAME              => 'Sereal::Encoder',
    VERSION_FROM      => 'lib/Sereal/Encoder.pm', # finds $VERSION
    PREREQ_PM         => {
        'XSLoader' => 0,
    }, # e.g., Module::Name => 1.1
    LICENSE => 'perl',
    ABSTRACT_FROM => 'lib/Sereal/Encoder.pm',
    AUTHOR => 'Steffen Mueller <smueller@cpan.org>', # FIXME
    LIBS              => [''], # e.g., '-lm'
    DEFINE            => $defines,
    INC               => '-I.', # e.g., '-I. -I/usr/include/other'
    OPTIMIZE          => $OPTIMIZE,
    OBJECT            => '$(O_FILES)',
);
$ENV{OPTIMIZE} = $OPTIMIZE;

sub WriteMakefile1 {  #Written by Alexandr Ciornii, version 0.20. Added by eumm-upgrade.
    my %params=@_;
    my $eumm_version=$ExtUtils::MakeMaker::VERSION;
    $eumm_version=eval $eumm_version;
    die "EXTRA_META is deprecated" if exists $params{EXTRA_META};
    die "License not specified" if not exists $params{LICENSE};
    if ($params{BUILD_REQUIRES} and $eumm_version < 6.5503) {
        #EUMM 6.5502 has problems with BUILD_REQUIRES
        $params{PREREQ_PM}={ %{$params{PREREQ_PM} || {}} , %{$params{BUILD_REQUIRES}} };
        delete $params{BUILD_REQUIRES};
    }
    delete $params{CONFIGURE_REQUIRES} if $eumm_version < 6.52;
    delete $params{MIN_PERL_VERSION} if $eumm_version < 6.48;
    delete $params{META_MERGE} if $eumm_version < 6.46;
    delete $params{META_ADD} if $eumm_version < 6.46;
    delete $params{LICENSE} if $eumm_version < 6.31;
    delete $params{AUTHOR} if $] < 5.005;
    delete $params{ABSTRACT_FROM} if $] < 5.005;
    delete $params{BINARY_LOCATION} if $] < 5.005;

    WriteMakefile(%params);
}
