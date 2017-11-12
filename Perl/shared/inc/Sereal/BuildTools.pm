package #
inc::Sereal::BuildTools;
use strict;
use warnings;

use Config;
use constant OSNAME => $^O;

my %bare_minimum_files= map { $_ => 1 } qw{
    typemap
    ppport.h
    srl_stack.h
    srl_common.h
    srl_inline.h
    srl_taginfo.h
    srl_protocol.h
    srl_reader_error.h
    srl_reader_types.h
    inc/Sereal/BuildTools.pm
    inc/Devel/CheckLib.pm
    inc/Sereal
    inc/Devel
    inc
};

sub link_files {
  my $shared_dir = shift;
  my $mode = shift || "";
  my $exclude_tests= $mode eq "without_tests";
  my $tests_only= $mode eq "tests_only";
  my $bare_minimum= $mode eq "bare_minimum";

  # This fires from a git source tree only.
  # Right now, all devs are on Linux. Feel free to make portable.
  eval {
    # overwrite by default
    require File::Find;
    require File::Path;
    require File::Spec;
    File::Find::find(
      { no_chdir => 1,
        wanted => sub {
          my $f = $_;
          s/^\Q$shared_dir\E\/?// or die $_;
          return unless $_;
          return if $exclude_tests && m#^/?t/#;
          return if $tests_only && !m#^/?t/#;
          return if $bare_minimum && !exists $bare_minimum_files{$_};

          if (-d $f) {
            File::Path::mkpath($_)
          }
          elsif (-f $f) {
            return if $f =~ /(?:\.bak|\.sw[po]|~)$/;
            my @d = File::Spec->splitdir($_);
            my $fname = pop @d;
            my $ref = join "/", ("..") x scalar(@d);
            my $subd = join "/", @d;
            chdir $subd if length($ref);
            my $srcfname = join("/", grep length, $ref, $shared_dir, $subd, $fname);
            if (OSNAME eq 'MSWin32') {
              die "link($srcfname, $fname) failed: $!"
                unless link($srcfname, $fname); #only NTFS implements it
            }
            else {
              symlink($srcfname, $fname);
            }
            chdir($ref) if length($ref);
          }
        },
      }, $shared_dir
    );
    1
  } or warn $@;
}

sub generate_constant_includes {
    # no-op
}

# Prefer external csnappy and miniz libraries over the bundled ones.
sub check_external_libraries {
  my ($libs, $defines, $objects, $subdirs) = @_;
  require Devel::CheckLib;

  if (
    !$ENV{SEREAL_USE_BUNDLED_LIBS} &&
    !$ENV{SEREAL_USE_BUNDLED_CSNAPPY} &&
    Devel::CheckLib::check_lib(
      lib      => 'csnappy',
      header   => 'csnappy.h'
  )) {
    print "Using installed csnappy library\n";
    $$libs .= ' -lcsnappy';
    $$defines .= ' -DHAVE_CSNAPPY';
  } else {
    print "Using bundled csnappy code\n";
  }

  if (
    !$ENV{SEREAL_USE_BUNDLED_LIBS} &&
    !$ENV{SEREAL_USE_BUNDLED_MINIZ} &&
    Devel::CheckLib::check_lib(
      lib      => 'miniz',
      header   => 'miniz.h'
  )) {
    print "Using installed miniz library\n";
    $$libs .= ' -lminiz';
    $$defines .= ' -DHAVE_MINIZ';
  } else {
    print "Using bundled miniz code\n";
    $$objects .= ' miniz$(OBJ_EXT)';
  }

  if (
    !$ENV{SEREAL_USE_BUNDLED_LIBS} &&
    !$ENV{SEREAL_USE_BUNDLED_ZSTD} &&
    Devel::CheckLib::check_lib(
      lib      => 'zstd',
      header   => 'zstd.h'
  )) {
    print "Using installed zstd library\n";
    $$libs .= ' -lzstd';
    $$defines .= ' -DHAVE_ZSTD';
  } else {
    print "Using bundled zstd code\n";
    push @{ $subdirs }, 'zstd';
    $$objects .= ' zstd/libzstd$(OBJ_EXT)';
  }
}

sub build_defines {
    my (@defs) = @_;

    my $defines = join(" ", map { "-D$_" . (defined $ENV{$_} ? "=$ENV{$_}" : '') }
                            grep { exists $ENV{$_} }
                            (qw(NOINLINE DEBUG MEMDEBUG NDEBUG), @defs));

    $defines .= " -DNDEBUG" unless $ENV{DEBUG};
    if ($Config{osname} eq 'hpux' && not $Config{gccversion}) {
      # HP-UX cc does not support inline.
      # Or rather, it does, but it depends on the compiler flags,
      # assumedly -AC99 instead of -Ae would work.
      # But we cannot change the compiler config too much from
      # the one that was used to compile Perl,
      # so we just fake the inline away.
      $defines .= " -Dinline= ";
    }

    return $defines;
}

sub build_optimize {
    my $cc_flags = shift || {};

    my $catch_violations = exists $cc_flags->{catch_violations} ? $cc_flags->{catch_violations} : 1;

    my $OPTIMIZE;

    my $clang = 0;
    if ($Config{gccversion}) {
        $OPTIMIZE = '-O3';
        if ($Config{gccversion} =~ /[Cc]lang/) { # clang.
            $clang = 1;
        }

        my $gccversion = 0;
        if ( $Config{gccversion} =~ /^(\d+\.\d+)/ ) {
            $gccversion = $1;
        }

        if ( $catch_violations && ($clang || $gccversion >= 4.3) ) {
            # -Werror= introduced in GCC 4.3
            # For trapping C++ // comments we would need -std=c89 (aka -ansi)
            # but that may be asking too much of different platforms.
            $OPTIMIZE .= ' -Werror=declaration-after-statement ';
        }

    } elsif ($Config{osname} eq 'MSWin32') {
        $OPTIMIZE = '-O2 -W4';
    } else {
        $OPTIMIZE = $Config{optimize};
    }

    if ($ENV{DEBUG}) {
        $OPTIMIZE .= ' -g';
        if ($ENV{DEBUG} > 0 && $Config{gccversion}) {
            $OPTIMIZE .= ' -Wextra' if $ENV{DEBUG} > 1;
            $OPTIMIZE .= ' -pedantic' if $ENV{DEBUG} > 5; # not pretty
            $OPTIMIZE .= ' -Weverything' if ($ENV{DEBUG} > 6 && $clang); # really not pretty
        }
    }

    return $OPTIMIZE;
}

sub WriteMakefile {
    require ExtUtils::MakeMaker;

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

    ExtUtils::MakeMaker::WriteMakefile(%params);
}

1;
