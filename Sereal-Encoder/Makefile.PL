use 5.008;

use strict;
use warnings;

use ExtUtils::MakeMaker;
use Config;

# This fires from a git source tree only.
# Right now, all devs are on Linux. Feel free to make portable.
eval {
  my $shared_dir = "../shared";
  if (-d "../.git" and -d $shared_dir) {
    # overwrite by default
    opendir my $dh, $shared_dir or die $!;
    foreach my $file (grep -f "$shared_dir/$_", readdir($dh)) {
      open my $ifh, "<", "$shared_dir/$file" or die $!;
      open my $ofh, ">", $file or die $!;
      print $ofh "\n# /* WARNING: GENERATED CODE! DO NOT EDIT! See shared/ folder in source repo. */\n\n";
      print $ofh $_ for <$ifh>;
    }
    closedir $dh;
  }
  1
} or warn $@;

our $OPTIMIZE;

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
    DEFINE            => '', # e.g., '-DHAVE_SOMETHING'
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
