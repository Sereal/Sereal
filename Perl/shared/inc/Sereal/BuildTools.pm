package inc::Sereal::BuildTools;
use strict;
use warnings;

sub link_files {
  my $shared_dir = shift;
  my $exlude_tests = shift;
  # This fires from a git source tree only.
  # Right now, all devs are on Linux. Feel free to make portable.
  eval {
    if (-d "../../.git" and -d $shared_dir) {
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
            return if $exlude_tests && m#^/?t/#;
            
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
              symlink(join("/", grep length, $ref, $shared_dir, $subd, $fname), $fname);
              chdir($ref) if length($ref);
            }
          },
        }, $shared_dir
      );
    }
    1
  } or warn $@;
}

sub generate_constant_includes {
    # no-op
}

# Prefer external csnappy and miniz libraries over the bundled ones.
sub check_external_libraries {
  my ($libs, $defines, $objects) = @_;
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
}

1;
