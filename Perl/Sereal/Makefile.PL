use 5.008;
use strict;
use warnings;

use ExtUtils::MakeMaker;
use Cwd;
our $VERSION = '4.004';

my $shared_dir = "../shared";
my $its_our_repo_file= "../this_is_the_Sereal_repo.txt";
my $in_source_repo =(       -d "../../.git"
                        and -d $shared_dir
                        and -e "../this_is_the_Sereal_repo.txt"
                );


unshift @INC, ".", "./inc";
unshift @INC, $shared_dir, "$shared_dir/inc"
    if $in_source_repo;

if ($in_source_repo) {
    eval "use blib '../Encoder/blib'; use blib '../Decoder/blib'; 1"
        or die "While building in Sereal.git, failed to use blib: $@\n",
               "You probably need to run `make` in the Encoder and Decoder directories\n";
}

my $module = "Sereal";
require inc::Sereal::BuildTools;
inc::Sereal::BuildTools::link_files($shared_dir,"tests_only") if $in_source_repo;

# See lib/ExtUtils/MakeMaker.pm for details of how to influence
# the contents of the Makefile that is written.
WriteMakefile1(
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
        'Sereal::Encoder' => $VERSION,
        'Sereal::Decoder' => $VERSION,
        'Scalar::Util' => 0,
        'File::Spec' => 0,
        'Test::LongString' => '0',
        'Test::Warn' => '0',
        'Data::Dumper' => '0',
    },
    NAME              => 'Sereal',
    VERSION_FROM      => 'lib/Sereal.pm', # finds $VERSION
    PREREQ_PM         => {
        'Sereal::Encoder' => $VERSION,
        'Sereal::Decoder' => $VERSION,
    }, # e.g., Module::Name => 1.1
    LICENSE => 'perl',
    ABSTRACT_FROM => 'lib/Sereal.pm',
    AUTHOR => 'Steffen Mueller <smueller@cpan.org>',
    test              => {
        TESTS => "t/*.t t/*/*/*.t",
    },
);

sub WriteMakefile1 {  #Written by Alexandr Ciornii, version 0.20. Added by eumm-upgrade.
    my %params=@_;
    my $eumm_version=$ExtUtils::MakeMaker::VERSION;
    $eumm_version=eval $eumm_version;
    die "EXTRA_META is deprecated" if exists $params{EXTRA_META};
    die "License not specified" if not exists $params{LICENSE};
    if ($params{TEST_REQUIRES} and $eumm_version < 6.6303) {
        $params{BUILD_REQUIRES}={ %{$params{BUILD_REQUIRES} || {}} , %{$params{TEST_REQUIRES}} };
        delete $params{TEST_REQUIRES};
    }
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
