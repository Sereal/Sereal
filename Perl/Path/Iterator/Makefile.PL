use 5.008;

use strict;
use warnings;

my $shared_dir = "../../shared";
my $in_source_repo = -d "../../../.git" && -d $shared_dir;
unshift @INC, "..", "../inc";
unshift @INC, $shared_dir, "$shared_dir/inc" if $in_source_repo;

require inc::Sereal::BuildTools;
inc::Sereal::BuildTools::link_files($shared_dir, 'without_tests') if $in_source_repo;

if ($in_source_repo) {
    foreach (qw/srl_decoder.h srl_decoder.c/) {
        -l $_ && unlink($_);
        symlink("../../Decoder/$_", $_) or warn $!;
    }
}

my $libs = '';
my $subdirs = [];
my $objects = '$(BASEEXT)$(OBJ_EXT) srl_iterator$(OBJ_EXT) srl_decoder$(OBJ_EXT)';
my $defines = inc::Sereal::BuildTools::build_defines();
$defines .= ' -DFOLLOW_REFERENCES_IF_NOT_STASHED';

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
    TEST_REQUIRES => {
        'Test::More' => 0.88,
        'Test::Deep' => 0,
        'Test::Exception' => 0.38,
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
    NAME              => 'Sereal::Path::Iterator',
    VERSION_FROM      => 'lib/Sereal/Path/Iterator.pm', # finds $VERSION
    PREREQ_PM         => { 'XSLoader' => 0 },
    LICENSE           => 'perl',
    ABSTRACT_FROM     => 'lib/Sereal/Path/Iterator.pm',
    AUTHOR            => 'Ivan Kruglov <ivan.kruglov@yahoo.com>',
    LIBS              => [$libs], # e.g., '-lm'
    DEFINE            => $defines,
    INC               => '-I.', # e.g., '-I. -I/usr/include/other'
    OPTIMIZE          => inc::Sereal::BuildTools::build_optimize(),
    OBJECT            => $objects,
    test              => { TESTS => "t/*.t t/*/*/*.t" },
    DIR               => $subdirs,
);
