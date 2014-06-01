package inc::Sereal::BuildTools;
use strict;
use warnings;

sub link_files {
  my $shared_dir = shift;
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

# This section deals with extracting constants from the protocol
# definition and including them as Perl constants. Not pretty, I know.
# Regenerate constants if module available.
sub generate_constant_includes {
  my $namespace = shift;
  my $constant_namespace = $namespace . "::Constants";
  my $file = $constant_namespace;
  $file =~ s/::/\//g;
  $file = "lib/$file";
  my $dir = $file;
  $file .= '.pm';

  if (eval { use ExtUtils::Constant qw(WriteConstants); 1 }) {
    require File::Path;
    my $fragment = $dir;
    $fragment =~ s/(?:En|De)coder\/?$//;
    File::Path::mkpath($fragment);
    print "Generating constant exports for Perl...\n";
    open my $fh, "<", "srl_protocol.h" or die $!;
    my (@string_const, @int_const);
    while (<$fh>) {
      if (/^#\s*define\s*(SRL_\w+)\s*(.*)$/) {
        my ($name, $value) = ($1, $2);
        next if $name =~ /_H_$/ or $name =~ /SET/ or $value =~ /"/;
        push @int_const, $name;
      }
    }
    close $fh;
    WriteConstants(
        NAME => $constant_namespace,
        NAMES => \@int_const,
    );
    open my $ofh, ">", $file or die $!;
    print $ofh <<HERE;
# Genereated code! Do not modify! See inc/Sereal/BuildTools.pm instead

package $constant_namespace;
use strict;
use warnings;
use Carp qw(croak);
use $namespace; # for XSLoading
our \@ISA = qw(Exporter);
require Exporter;
our \@EXPORT_OK;
BEGIN { \@EXPORT_OK = qw(
HERE
    print $ofh "    $_\n" for (@int_const);
    print $ofh <<'HERE';
  );
  my $code;
  foreach my $constname (@EXPORT_OK) {
    my ($error, $val) = constant($constname);
    if ($error) { croak($error); }
    $code .= "sub $constname () {$val}\n";
  }
  eval "$code\n1;" or do {
    my $err = $@ || 'Zombie error';
    die "Failed to generate constant subs: $err\n Code was:\n$code\n";
  };
}

sub SRL_MAGIC_STRING () {"=srl"}
sub SRL_MAGIC_STRING_HIGHBIT () {"=\xF3rl"}
sub SRL_MAGIC_STRING_HIGHBIT_UTF8 () { "=\xC3\xB3rl" }
push @EXPORT_OK, qw(SRL_MAGIC_STRING SRL_MAGIC_STRING_HIGHBIT SRL_MAGIC_STRING_HIGHBIT_UTF8);
our %EXPORT_TAGS=(all => \@EXPORT_OK);
1;
HERE
    close $ofh;
  }
  else {
    warn "Please install ExtUtils::Constant since you appear to be running out of the source repository.\n";
  }
}


1;

