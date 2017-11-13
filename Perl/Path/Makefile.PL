use 5.008;

use strict;
use warnings;

my $shared_dir = "../shared";
my $in_source_repo = -d "../../.git" && -d $shared_dir;

unshift @INC, ".", "./inc";
unshift @INC, $shared_dir, "$shared_dir/inc" if $in_source_repo;

require inc::Sereal::BuildTools;
inc::Sereal::BuildTools::link_files($shared_dir, 'bare_minimum') if $in_source_repo;

my $libs = '';
my $subdirs = [ 'Iterator', 'Tie' ];
my $defines = inc::Sereal::BuildTools::build_defines();
my $objects = '$(BASEEXT)$(OBJ_EXT) srl_path$(OBJ_EXT) Iterator/srl_iterator.o Iterator/srl_decoder.o Iterator/miniz.o Iterator/zstd/libzstd.o';

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
        'Test::Deep' => 0,
        'Test::Exception' => 0.38,
        'Test::Differences' => 0,
        'Scalar::Util' => 0,
        'Data::Dumper' => 0,
        'Sereal::Encoder' => 4.004,
        'Sereal::Decoder' => 4.004,
    },
    BUILD_REQUIRES => {
        'File::Find' => 0,
        'File::Path' => 0,
        'ExtUtils::ParseXS' => '2.21',
    },
    CONFIGURE_REQUIRES => {
        'ExtUtils::MakeMaker' => '7.0',
    },
    NAME              => 'Sereal::Path',
    VERSION_FROM      => 'lib/Sereal/Path.pm', # finds $VERSION
    PREREQ_PM         => { 'XSLoader' => 0 },
    LICENSE           => 'perl',
    ABSTRACT_FROM     => 'lib/Sereal/Path.pm',
    AUTHOR            => 'Ivan Kruglov <ivan.kruglov@yahoo.com>',
    LIBS              => [$libs], # e.g., '-lm'
    DEFINE            => $defines,
    INC               => '-I.', # e.g., '-I. -I/usr/include/other'
    OPTIMIZE          => inc::Sereal::BuildTools::build_optimize(),
    OBJECT            => $objects,
    test              => { TESTS => "t/*.t t/*/*/*.t" },
    DIR               => $subdirs,
);
