use 5.008;

use strict;
use warnings;

use ExtUtils::MakeMaker;
use Config;

my $shared_dir = "../shared";
my $in_source_repo = -d "../../.git" and -d $shared_dir;

my $module = "Sereal::Decoder";

unshift @INC, '.';
require inc::Sereal::BuildTools;
inc::Sereal::BuildTools::link_files($shared_dir) if $in_source_repo;
inc::Sereal::BuildTools::generate_constant_includes($module) if $in_source_repo;

our $OPTIMIZE;

my $defines = join " ", map "-D$_", grep exists $ENV{$_}, qw(NOINLINE DEBUG MEMDEBUG NDEBUG);
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
            repository => 'git://github.com/Sereal/Sereal.git',
            bugtracker => 'https://github.com/Sereal/Sereal/issues',
        },
    },
    BUILD_REQUIRES => {
        'XSLoader' => 0,
        'Test::More' => 0.88,
        'File::Find' => 0,
        'Scalar::Util' => 0,
        'File::Spec' => 0,
        'File::Path' => 0,
        'ExtUtils::ParseXS' => '2.21',
        'Test::LongString' => '0',
        'Data::Dumper' => '0',
    },
    NAME              => $module,
    VERSION_FROM      => 'lib/Sereal/Decoder.pm', # finds $VERSION
    PREREQ_PM         => {
        'XSLoader' => 0,
    }, # e.g., Module::Name => 1.1
    LICENSE => 'perl',
    ABSTRACT_FROM => 'lib/Sereal/Decoder.pm',
    AUTHOR => 'Steffen Mueller <smueller@cpan.org>, Yves Orton <yves@cpan.org>',
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
    delete $params{OPTIMIZE} if $^O eq 'MSWin32';
    WriteMakefile(%params);
}
