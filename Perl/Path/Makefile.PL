use 5.008;

use strict;
use warnings;

use ExtUtils::MakeMaker;
use Config;

my $shared_dir = "../shared";
my $in_source_repo = -d "../../.git" and -d $shared_dir;

my $module = "Sereal::Path";

unshift @INC, ".", "./inc";
unshift @INC, $shared_dir, "$shared_dir/inc" if $in_source_repo;
require inc::Sereal::BuildTools;
inc::Sereal::BuildTools::link_files($shared_dir, 'without_tests') if $in_source_repo;
inc::Sereal::BuildTools::generate_constant_includes($module) if $in_source_repo;

# TODO Configure/optimize for miniz:
#   * Important: For best perf. be sure to customize the below macros for your target platform:
#     #define MINIZ_USE_UNALIGNED_LOADS_AND_STORES 1
#     #define MINIZ_LITTLE_ENDIAN 1
#     #define MINIZ_HAS_64BIT_REGISTERS 1

our $OPTIMIZE;

my $libs = '';
my $objects = '$(BASEEXT)$(OBJ_EXT) srl_path$(OBJ_EXT) srl_iterator$(OBJ_EXT)';
my $defines = join" ", map { "-D$_" . (defined $ENV{$_} ? "=$ENV{$_}" : '') } grep { exists $ENV{$_} }
              qw(NOINLINE DEBUG MEMDEBUG NDEBUG ENABLE_DANGEROUS_HACKS);

my $moderngccish = 0;
my $clang = 0;
if ($Config{gccversion}) {
    $OPTIMIZE = '-O3 ';
    if ($Config{gccversion} =~ /[Cc]lang/) { # clang.
        $clang = 1;
        $moderngccish = 1;
    } elsif ($Config{gccversion} =~ /^[123]\./) { # Ancient gcc.
        $moderngccish = 0;
    } else { # Modern gcc.
        $moderngccish = 1;
    }
} elsif ($Config{osname} eq 'MSWin32') {
    $OPTIMIZE = '-O2 -W4';
} else {
    $OPTIMIZE = $Config{optimize};
}

# For trapping C++ // comments we would need -std=c89 (aka -ansi)
# but that may be asking too much of different platforms.
if ($moderngccish) {
    $OPTIMIZE .= ' -Werror=declaration-after-statement ';
}

if ($ENV{DEBUG}) {
  $OPTIMIZE .= ' -g';
  if ($ENV{DEBUG} > 0 && $Config{gccversion}) {
      $OPTIMIZE .= ' -Wextra' if $ENV{DEBUG} > 1;
      $OPTIMIZE .= ' -pedantic' if $ENV{DEBUG} > 5; # not pretty
      $OPTIMIZE .= ' -Weverything' if ($ENV{DEBUG} > 6 && $clang); # really not pretty
  }
}
else {
  $defines .= " -DNDEBUG";
}

# TODO: This is identical with Decoder.
if ($Config{osname} eq 'hpux' && not $Config{gccversion}) {
  # HP-UX cc does not support inline.
  # Or rather, it does, but it depends on the compiler flags,
  # assumedly -AC99 instead of -Ae would work.
  # But we cannot change the compiler config too much from
  # the one that was used to compile Perl,
  # so we just fake the inline away.
  $defines .= " -Dinline= ";
}

# Prefer external libraries over the bundled one.
inc::Sereal::BuildTools::check_external_libraries(\$libs, \$defines, \$objects);

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
        'Scalar::Util' => 0,
        'File::Spec' => 0,
        'Test::LongString' => '0',
        'Test::Warn' => '0',
        'Data::Dumper' => '0',
        'Sereal::Encoder' => '3.00',
        'Sereal::Decoder' => '3.00',
        'JSON::Path'      => '0.205',
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
    VERSION_FROM      => 'lib/Sereal/Path.pm', # finds $VERSION
    PREREQ_PM         => {
        'XSLoader' => 0,
    }, # e.g., Module::Name => 1.1
    LICENSE => 'perl',
    ABSTRACT_FROM     => 'lib/Sereal/Path.pm',
    AUTHOR            => 'Ivan Kruglov <ivan.kruglov@yahoo.com>',
    LIBS              => [$libs], # e.g., '-lm'
    DEFINE            => $defines,
    INC               => '-I.', # e.g., '-I. -I/usr/include/other'
    OPTIMIZE          => $OPTIMIZE,
    OBJECT            => $objects,
    test              => {
        TESTS => "t/*.t t/*/*/*.t",
    },
);
$ENV{OPTIMIZE} = $OPTIMIZE;

sub WriteMakefile1 {
    #Original by Alexandr Ciornii, modified by Yves Orton
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
    if ($params{CONFIGURE_REQUIRES} and $eumm_version < 6.52) {
        $params{PREREQ_PM}={ %{$params{PREREQ_PM} || {}}, %{$params{CONFIGURE_REQUIRES}} };
        delete $params{CONFIGURE_REQUIRES};
    }
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
