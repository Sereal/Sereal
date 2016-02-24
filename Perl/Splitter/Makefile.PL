use 5.008;

use strict;
use warnings;

use ExtUtils::MakeMaker;
use Config;

# this is boiler plate init code borrowed from the Encoder/Decoder
# we do not use that much of it, except for link_files() below, which
# we use differently from Encoder/Decoder as we do not want the shared
# tests. we could probably manage this stuff better.
my $shared_dir = "../shared";
my $in_source_repo = -d "../../.git" && -d $shared_dir;

my $module = "Sereal::Splitter";

unshift @INC, ".", "./inc";
unshift @INC, $shared_dir, "$shared_dir/inc" if $in_source_repo;
require inc::Sereal::BuildTools;
inc::Sereal::BuildTools::link_files($shared_dir,"without_tests") if $in_source_repo;
inc::Sereal::BuildTools::generate_constant_includes($module) if $in_source_repo;

my $optimize = inc::Sereal::BuildTools::build_optimize({ catch_violations => 0 });

# TODO Configure/optimize for miniz:
#   * Important: For best perf. be sure to customize the below macros for your target platform:
#     #define MINIZ_USE_UNALIGNED_LOADS_AND_STORES 1
#     #define MINIZ_LITTLE_ENDIAN 1
#     #define MINIZ_HAS_64BIT_REGISTERS 1

my $defines = inc::Sereal::BuildTools::build_defines();

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
        'Data::Dumper' => '0',
        'Sereal::Encoder' => '0',
        'Sereal::Decoder' => '0',
        'Data::Dumper' => '0',
    },
    NAME              => $module,
    VERSION_FROM      => 'lib/Sereal/Splitter.pm', # finds $VERSION
    PREREQ_PM         => {
        'XSLoader' => 0,
    }, # e.g., Module::Name => 1.1
    LICENSE => 'perl',
    ABSTRACT_FROM => 'lib/Sereal/Splitter.pm',
    AUTHOR => 'Damien Krotkine <dams@cpan.org>',
    LIBS              => [''], # e.g., '-lm'
    DEFINE            => $defines,
    INC               => '-I.', # e.g., '-I. -I/usr/include/other'
    OPTIMIZE          => $optimize,
    OBJECT            => '$(O_FILES)',
    test              => {
        TESTS => "t/*.t t/*/*/*.t"
    },
);
$ENV{OPTIMIZE} = $optimize;

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
