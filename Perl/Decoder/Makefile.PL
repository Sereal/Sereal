use 5.008;

use strict;
use warnings;

use ExtUtils::MakeMaker;
use Config;

my $shared_dir = "../shared";
my $its_our_repo_file= "../this_is_the_Sereal_repo.txt";
my $in_source_repo =(       -d "../../.git"
                        and -d $shared_dir
                        and -e "../this_is_the_Sereal_repo.txt"
                );


unshift @INC, ".", "./inc";
unshift @INC, $shared_dir, "$shared_dir/inc"
    if $in_source_repo;

my $module = "Sereal::Decoder";
require inc::Sereal::BuildTools;
inc::Sereal::BuildTools::link_files($shared_dir) if $in_source_repo;
inc::Sereal::BuildTools::generate_constant_includes($module) if $in_source_repo;

my $optimize = inc::Sereal::BuildTools::build_optimize();

# TODO Configure/optimize for miniz:
#   * Important: For best perf. be sure to customize the below macros for your target platform:
#     #define MINIZ_USE_UNALIGNED_LOADS_AND_STORES 1
#     #define MINIZ_LITTLE_ENDIAN 1
#     #define MINIZ_HAS_64BIT_REGISTERS 1

my $libs = '';
my $subdirs = [];
my $objects = '$(BASEEXT)$(OBJ_EXT) srl_decoder$(OBJ_EXT)';
my $defines = inc::Sereal::BuildTools::build_defines();

# Prefer external libraries over the bundled one.
inc::Sereal::BuildTools::check_external_libraries(\$libs, \$defines, \$objects, $subdirs);

# See lib/ExtUtils/MakeMaker.pm for details of how to influence
# the contents of the Makefile that is written.
inc::Sereal::BuildTools::WriteMakefile(
    MIN_PERL_VERSION => '5.008',
    META_MERGE => {
        resources => {
            repository => {
              url => 'git://github.com/Sereal/Sereal.git',
            },
            bugtracker => {
              web => 'https://github.com/Sereal/Sereal/issues',
            },
        },
        'meta-spec' => { version => 2 },
    },
    TEST_REQUIRES  => {
        'Test::More' => '0.88',
        'Scalar::Util' => '0',
        'File::Spec' => '0',
        'Test::LongString' => '0',
        'Test::Warn' => '0',
        'Test::Deep' => '0',
        'Test::Differences' => '0',
        'Data::Dumper' => '0',
    },
    BUILD_REQUIRES => {
        'XSLoader' => 0,
        'File::Find' => 0,
        'File::Path' => 0,
        'ExtUtils::ParseXS' => '2.21',
    },
    CONFIGURE_REQUIRES => {
        'ExtUtils::MakeMaker' => '7.0',
    },
    NAME              => $module,
    VERSION_FROM      => 'lib/Sereal/Decoder.pm', # finds $VERSION
    PREREQ_PM         => {
        'XSLoader' => 0,
    }, # e.g., Module::Name => 1.1
    LICENSE => 'perl',
    ABSTRACT_FROM => 'lib/Sereal/Decoder.pm',
    AUTHOR => 'Steffen Mueller <smueller@cpan.org>, Yves Orton <yves@cpan.org>',
    LIBS              => [$libs], # e.g., '-lm'
    DEFINE            => $defines,
    INC               => '-I.', # e.g., '-I. -I/usr/include/other'
    OPTIMIZE          => $optimize,
    OBJECT            => $objects,
    DIR               => $subdirs,
    test              => {
        TESTS => "t/*.t t/*/*/*.t"
    },
);
$ENV{OPTIMIZE} = $optimize;
