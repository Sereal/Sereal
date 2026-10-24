use 5.008;

use strict;
use warnings;

use ExtUtils::MakeMaker;
use Config;

my $shared_dir = "../shared";
my $in_source_repo = -e "../../.git" && -d $shared_dir;

my $module = "Sereal::Splitter";

unshift @INC, ".", "./inc";
unshift @INC, $shared_dir, "$shared_dir/inc" if $in_source_repo;
require inc::Sereal::BuildTools;
inc::Sereal::BuildTools::link_files($shared_dir, "without_tests") if $in_source_repo;
inc::Sereal::BuildTools::generate_constant_includes($module) if $in_source_repo;

my $optimize = inc::Sereal::BuildTools::build_optimize();

my $libs = '';
my $subdirs = [];
my $objects = '$(BASEEXT)$(OBJ_EXT) srl_splitter$(OBJ_EXT)';
my $defines = inc::Sereal::BuildTools::build_defines();

# Prefer external libraries over the bundled one.
inc::Sereal::BuildTools::check_external_libraries(\$libs, \$defines, \$objects, $subdirs);

if ($defines !~ /HAVE_CSNAPPY/) {
    # from Compress::Snappy
    require Devel::CheckLib;
    my $ctz = Devel::CheckLib::check_lib(
        lib      => 'c',
        function => 'return (__builtin_ctzll(0x100000000LL) != 32);'
    ) ? '-DHAVE_BUILTIN_CTZ' : '';
    $defines .= " $ctz" if $ctz;
}

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
    TEST_REQUIRES => {
        'Test::More' => 0.88,
        'Data::Dumper' => 0,
        'Sereal::Encoder' => '6.000_001',
        'Sereal::Decoder' => '6.000_001',
    },
    BUILD_REQUIRES => {
        'File::Find' => 0,
        'File::Path' => 0,
        'ExtUtils::ParseXS' => '2.21',
    },
    CONFIGURE_REQUIRES => {
        'ExtUtils::MakeMaker' => '7.0',
    },
    NAME              => $module,
    VERSION_FROM      => 'lib/Sereal/Splitter.pm', # finds $VERSION
    PREREQ_PM         => {
        'XSLoader' => 0,
    }, # e.g., Module::Name => 1.1
    LICENSE => 'perl',
    ABSTRACT_FROM => 'lib/Sereal/Splitter.pm',
    AUTHOR => 'Damien Krotkine <dams@cpan.org>',
    LIBS              => [$libs], # e.g., '-lm'
    DEFINE            => $defines,
    INC               => '-I.', # e.g., '-I. -I/usr/include/other'
    OPTIMIZE          => $optimize,
    OBJECT            => $objects,
    DIR               => $subdirs,
    test              => {
        TESTS => "t/*.t"
    },
);
$ENV{OPTIMIZE} = $optimize;
